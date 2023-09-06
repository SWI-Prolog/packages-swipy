/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

#ifdef PYTHON_PACKAGE
#define _REENTRANT 1
#else
#include <config.h>
#endif

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <Python.h>
#include <assert.h>

static atom_t ATOM_none;
static atom_t ATOM_false;
static atom_t ATOM_true;
static atom_t ATOM_pydict;
static atom_t ATOM_tuple;
static atom_t ATOM_curl;
static atom_t ATOM_atom;
static atom_t ATOM_string;
static atom_t ATOM_file;
static atom_t ATOM_eval;
static atom_t ATOM_single;
static atom_t ATOM_builtins;

static functor_t FUNCTOR_python_error3;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_module2;
static functor_t FUNCTOR_eq2;
static functor_t FUNCTOR_hash1;
static functor_t FUNCTOR_comma2;
static functor_t FUNCTOR_curl1;
static functor_t FUNCTOR_key_value2;
static functor_t FUNCTOR_py1;
static functor_t FUNCTOR_py_set1;
static functor_t FUNCTOR_prolog1;
static functor_t FUNCTOR_at1;

static int py_initialize_done = FALSE;

static PyObject *check_error(PyObject *obj);
static int py_unify(term_t t, PyObject *obj, int flags);
static int py_from_prolog(term_t t, PyObject **obj);
static void py_yield(void);
static void py_resume(void);
static PyObject *py_record(term_t t);
static int py_unify_record(term_t t, PyObject *rec);
static int py_unify_constant(term_t t, atom_t c);
static int py_is_record(PyObject *rec);
static PyObject *py_free_record(PyObject *rec);
static int unchain(term_t call, PyObject **py_target);
static PyObject *py_eval(PyObject *obj, term_t func);

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
On Windows,  `python3.dll` provides the  stable C API that  relates to
the concrete  Python version next  to it.  Our  CMakeLists.txt detects
the presence of  `python3.dll` next to the concrete  dll.  When found,
it   replaces   the   dependency  with   `python3.dll`   and   defines
`PYTHON3_COMPAT`.

The `PYTHON3_COMPAT` flag  is used here to use older  API calls rather
than the  more convenient or  even preferred newer API  functions.  In
particular:

  - Slice the partially build argument tuple for a function when
    we encounter keyword arguments rather than resizing it.
  - Use the old (as of 3.11 deprecated) Py_Initialize() to initialize
    the embedded Python system.
  - There is no PyGILState_Check() in the stable API, so we keep
    track ourselves whether we locked the gil.
  - Py_CompileString() is in the stable API, but the header uses a
    macro to redefine it to a newer API function that is not.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


#ifdef PYTHON3_COMPAT
#undef Py_CompileString		/* See above */
#else
#if PY_VERSION_HEX >= 0x03080000
#define HAVE_PYCONFIG 1
#endif
#define HAVE__PYTUPLE_RESIZE 1
#define HAVE_PYGILSTATE_CHECK 1
#endif

#include "hash.c"
#include "mod_swipl.c"

#ifdef _REENTRANT
#ifdef __WINDOWS__
#include <windows.h>
static CRITICAL_SECTION py_mutex;

BOOL WINAPI
DllMain(HINSTANCE hinstDll, DWORD fdwReason, LPVOID lpvReserved)
{ BOOL result = TRUE;

  switch(fdwReason)
  { case DLL_PROCESS_ATTACH:
      InitializeCriticalSection(&py_mutex);
      break;
    case DLL_PROCESS_DETACH:
      DeleteCriticalSection(&py_mutex);
      break;
    case DLL_THREAD_ATTACH:
    case DLL_THREAD_DETACH:
      break;
  }

  return result;
}

#define LOCK() EnterCriticalSection(&py_mutex)
#define UNLOCK() LeaveCriticalSection(&py_mutex)

#ifdef _MSC_VER
/* Only for our purpose */
static inline int
__sync_bool_compare_and_swap(volatile void **addr, void *new, void *old)
{
#if _WIN64
    return (int)_InterlockedCompareExchange64((volatile __int64*)addr, (__int64)new, (__int64)old);
#else
    return (int)_InterlockedCompareExchange64((volatile long*)addr, (long)new, (long)old);
#endif
}

#define _Thread_local __declspec(thread)
#endif

#else /*__WINDOWS__*/
#include <pthread.h>

static pthread_mutex_t py_mutex = PTHREAD_MUTEX_INITIALIZER;
#define LOCK() pthread_mutex_lock(&py_mutex)
#define UNLOCK() pthread_mutex_unlock(&py_mutex)
#endif /*__WINDOWS__*/
#else /*_REENTRANT*/
#define LOCK()
#define UNLOCK()
#endif /*_REENTRANT*/

#define PYU_STRING 0x0001		/* Unify text as Prolog string */
#define PYU_OBJ    0x0002		/* Unify as object */

#ifndef HAVE_PYGILSTATE_CHECK
static _Thread_local int have_gil;
#define PyGILState_Check() (have_gil)
#endif

		 /*******************************
		 *          DEBUGGING           *
		 *******************************/

static int debuglevel = 0;

static foreign_t
py_debug(term_t level)
{ return PL_get_integer_ex(level, &debuglevel);
}

#define DEBUG(l, g) \
	if ( (l) <= debuglevel ) do { g; } while(0)


		 /*******************************
		 *	       BLOB		*
		 *******************************/

typedef struct py_delayed
{ PyObject *obj;
  struct py_delayed *next;
} py_delayed;

static py_delayed *delayed;

static int
delayed_decref(void *ptr)
{ py_delayed *d = delayed;

  if ( __sync_bool_compare_and_swap(&delayed, d, NULL) )
  { py_delayed *n;

    for(; d; d=n)
    { n = d->next;
      Py_DECREF(d->obj);
      free(d);
    }
  }

  return 0;
}

static void
MyPy_DECREF(PyObject *o)
{ if ( PyGILState_Check() )
  { Py_DECREF(o);
  } else
  { py_delayed *d = malloc(sizeof(*d));
    py_delayed *old;

    if ( d )
    { d->obj = o;
      do
      { old = delayed;
	d->next = old;
      } while ( !__sync_bool_compare_and_swap(&delayed, old, d) );
    }
  }
}


		 /*******************************
		 *             BLOB             *
		 *******************************/

static int
release_python_object(atom_t symbol)
{ PyObject *obj = PL_blob_data(symbol, NULL, NULL );

  if ( obj )
    MyPy_DECREF(obj);
  return TRUE;
}

static int
compare_python_object(atom_t a, atom_t b)
{ PyObject *obj1 = PL_blob_data(a, NULL, NULL);
  PyObject *obj2 = PL_blob_data(b, NULL, NULL);
  return ( obj1 > obj2 ?  1 :
	   obj1 < obj2 ? -1 : 0
	 );
}

static int
write_python_object(IOSTREAM *s, atom_t symbol, int flags)
{ PyObject *obj = PL_blob_data(symbol, NULL, NULL);

  if ( obj )
  { PyObject *cls = NULL;
    PyObject *cname = NULL;
    const char *name;

    if ( (cls=PyObject_GetAttrString(obj, "__class__")) &&
	 (cname=PyObject_GetAttrString(cls, "__name__")) )
      name = PyUnicode_AsUTF8AndSize(cname, NULL);
    else
      name = "noclass";

    Sfprintf(s, "<py_%Us>(%p)", name, obj);

    Py_CLEAR(cls);
    Py_CLEAR(cname);
  } else
  { Sfprintf(s, "<py_FREED>(0x0)");
  }

  return TRUE;
}

static void
acquire_python_object(atom_t symbol)
{ PyObject *obj  = PL_blob_data(symbol, NULL, NULL);
  Py_INCREF(obj);
}

static PL_blob_t PY_OBJECT = {
  PL_BLOB_MAGIC,
  PL_BLOB_UNIQUE|PL_BLOB_NOCOPY,
  "PyObject",
  release_python_object,
  compare_python_object,
  write_python_object,
  acquire_python_object
};

static int
unify_py_obj(term_t t, PyObject *obj)
{ return PL_unify_blob(t, obj, sizeof(obj), &PY_OBJECT);
}

static int
get_py_obj(term_t t, PyObject **objp, int error)
{ void *data;
  size_t size;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, &size, &type) && type == &PY_OBJECT )
  { if ( size != 0 )
    { PyObject *obj = data;
      Py_INCREF(obj);
      *objp = obj;
      return TRUE;
    } else
      return PL_existence_error("PyObject", t);
  }

  if ( error )
    PL_type_error("PyObject", t);

  return FALSE;
}

static int
get_py_name(term_t name, PyObject **obj)
{ char *idname;

  if ( PL_get_chars(name, &idname, CVT_ATOM) )
  { PyObject *id = check_error(PyUnicode_FromString(idname));

    if ( id )
    { *obj = id;
      return TRUE;
    }
  }

  return FALSE;
}

static htable *py_module_table = NULL;

static int
get_py_module(term_t name, PyObject **mod)
{ atom_t id;

  if ( PL_get_atom(name, &id) )
  { PyObject *obj;

    if ( id == ATOM_builtins )
    { PyObject *builtins = PyEval_GetBuiltins();
      Py_INCREF(builtins);
      *mod = builtins;
      return TRUE;
    }

    if ( !py_module_table )
      py_module_table = py_new_hashmap();

    if ( (obj=py_lookup_hashmap(py_module_table, id)) )
    { Py_INCREF(obj);
      *mod = obj;
      return TRUE;
    } else
    { PyObject *idobj;

      if ( get_py_name(name, &idobj) )
      { PyObject *m = check_error(PyImport_Import(idobj));

	Py_DECREF(idobj);
	if ( m )
	{ *mod = m;
	  py_add_hashmap(py_module_table, id, m);
	  return TRUE;
	}
      }
    }
  }

  return FALSE;
}


static PyObject *
check_error(PyObject *obj)
{ PyObject *ex = PyErr_Occurred();

  if ( ex )
  { PyObject *type = NULL, *tname = NULL, *value = NULL, *stack = NULL;
    term_t t  = PL_new_term_ref();
    term_t av = PL_new_term_refs(3);

    PyErr_Fetch(&type, &value, &stack);
    if ( (tname=PyObject_GetAttrString(type, "__name__")) &&
         PL_unify_chars(av+0, PL_ATOM|REP_UTF8, (size_t)-1,
			PyUnicode_AsUTF8AndSize(tname, NULL)) &&
	 (value ? py_unify(av+1, value, 0)
		: py_unify_constant(av+1, ATOM_none)) &&
	 (stack ? py_unify(av+2, stack, 0)
		: py_unify_constant(av+2, ATOM_none)) &&
	 PL_cons_functor_v(t, FUNCTOR_python_error3, av) &&
	 PL_put_variable(av+0) &&
	 PL_cons_functor(t, FUNCTOR_error2, t, av+0) )
      PL_raise_exception(t);

    Py_CLEAR(tname);
    return NULL;
  } else
    return obj;
}


static int
py_unify_long(term_t t, PyObject *obj)
{ int overflow;
  long long v = PyLong_AsLongLongAndOverflow(obj, &overflow);

  if ( !overflow )
  { assert(sizeof(long long) == sizeof(int64_t));
    return PL_unify_int64(t, v);
  } else
  { static PyObject *hex = NULL;
    PyObject *av = NULL, *hobj = NULL;
    int rc = FALSE;

    if ( !hex )
    { PyObject *builtins = PyEval_GetBuiltins(); /* borrowed reference */
      hex = PyDict_GetItemString(builtins , "hex");
      if ( !hex )
      { term_t ex;

	return ( (ex=PL_new_term_ref()) &&
		 PL_put_atom_chars(ex, "hex") &&
		 PL_existence_error("python_builtin", ex) );
      }
    }

    av = PyTuple_New(1);
    if ( av )
    { Py_INCREF(obj);
      PyTuple_SetItem(av, 0, obj);
      hobj = check_error(PyObject_CallObject(hex, av));
      Py_CLEAR(av);
      if ( hobj )
      { Py_ssize_t len;
	const char *s = PyUnicode_AsUTF8AndSize(hobj, &len);
	term_t tmp;

	rc = ( (tmp=PL_new_term_ref()) &&
	       PL_put_term_from_chars(tmp, REP_UTF8, len, s) &&
	       PL_unify(t, tmp) );
	Py_CLEAR(hobj);
      }
    }

    return rc;
  }
}

static int
py_unify_unicode(term_t t, PyObject *obj, int flags)
{ ssize_t len;
  const char *s;
  int rc;
  int uflags = REP_UTF8;

  uflags |= (flags&PYU_STRING) ? PL_STRING : PL_ATOM;

  s = PyUnicode_AsUTF8AndSize(obj, &len);
  if ( !check_error((void*)s) )
    return FALSE;
  PL_STRINGS_MARK();
  rc = PL_unify_chars(t, uflags, len, s);
  PL_STRINGS_RELEASE();
  return rc;
}


static int
py_unify_tuple(term_t t, PyObject *obj, int flags)
{ Py_ssize_t arity = PyTuple_GET_SIZE(obj);
  if ( PL_unify_compound(t, PL_new_functor(ATOM_tuple, arity)) )
  { term_t a = PL_new_term_ref();
    for(Py_ssize_t i=0; i<arity; i++)
    { PyObject *py_a = PyTuple_GetItem(obj, i);
      _PL_get_arg(i+1, t, a);
      if ( !py_unify(a, py_a, flags) )
	return FALSE;
    }
    PL_reset_term_refs(a);
    return TRUE;
  }
  return FALSE;
}

static int
py_unify_sequence(term_t t, PyObject *obj, int flags)
{ ssize_t len = PySequence_Size(obj);
  term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();

  for(ssize_t i=0; i<len; i++)
  { PyObject *el = check_error(PySequence_GetItem(obj, i));

    if ( !el )
      return FALSE;
    int rc = ( PL_unify_list(tail, head,tail) &&
	       py_unify(head, el, flags) );
    Py_DECREF(el);
    if ( !rc )
      return FALSE;
  }
  if ( !PL_unify_nil(tail) )
    return FALSE;

  PL_reset_term_refs(tail);
  return TRUE;
}

static int
py_unify_iter(term_t t, PyObject *obj, int flags)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  PyObject *item;

  while((item=check_error(PyIter_Next(obj))))
  { int rc = ( PL_unify_list(tail, head,tail) &&
	       py_unify(head, item, flags) );
    Py_DECREF(item);
    if ( !rc )
      return FALSE;
  }
  if ( PL_exception(0) || !PL_unify_nil(tail) )
    return FALSE;

  PL_reset_term_refs(tail);
  return TRUE;
}

static int
py_unify_set(term_t t, PyObject *obj, int flags)
{ PyObject *iter = check_error(PyObject_GetIter(obj));
  PyObject *item = NULL;
  int rc = FALSE;

  if ( iter )
  { PyObject *item;
    term_t tail = PL_new_term_ref();
    term_t head = PL_new_term_ref();

    if ( !(rc=PL_unify_functor(t, FUNCTOR_py_set1)) )
      goto out;
    _PL_get_arg(1, t, tail);

    while( rc && (item=PyIter_Next(iter)) )
    { rc = ( PL_unify_list(tail, head,tail) &&
	     py_unify(head, item, flags) );
      Py_CLEAR(item);
    }
    rc = rc && PL_unify_nil(tail);

    PL_reset_term_refs(tail);
  }

out:
  Py_CLEAR(iter);
  Py_CLEAR(item);

  return rc;
}

static int
py_unify_dict(term_t t, PyObject *obj, int flags)
{ Py_ssize_t size = PyDict_Size(obj);
  term_t pl_dict = PL_new_term_ref();
  term_t pl_values;
  atom_t fast[25];
  atom_t *pl_keys;
  int rc = FALSE;
  Py_ssize_t i = 0;
  PyObject *py_key, *py_value;

  if ( size < 0 || size > INT_MAX )
    return PL_resource_error("stack");

  pl_values = PL_new_term_refs((int)size);

  if ( size < 25 )
    pl_keys = fast;
  else if ( !(pl_keys = malloc(size*sizeof(atom_t))) )
    return PL_resource_error("memory");
  memset(pl_keys, 0, size*sizeof(atom_t));

  if ( !pl_keys )
  { PL_resource_error("memory");
    goto out;
  }

  for( size_t pli=0; PyDict_Next(obj, &i, &py_key, &py_value); pli++ )
  { if ( PyUnicode_Check(py_key) )
    { ssize_t len;
      wchar_t *s;

      s = PyUnicode_AsWideCharString(py_key, &len);
      if ( !check_error((void*)s) )
	goto out;
      pl_keys[pli] = PL_new_atom_wchars(len, s);
      PyMem_Free(s);
    } else if ( PyLong_Check(py_key) )
    { if ( !(pl_keys[pli]=_PL_cons_small_int(PyLong_AsLongLong(py_key))) )
      { PL_representation_error("py_dict_key");
	goto out;
      }
    } else
    { PL_representation_error("py_dict_key");
      goto out;
    }
    if ( !py_unify(pl_values+pli, py_value, flags) )
      goto out;
  }

  rc = (PL_put_dict(pl_dict, ATOM_pydict, size, pl_keys, pl_values) &&
	PL_unify(t, pl_dict));

out:
  _PL_unregister_keys(size, pl_keys);
  if ( pl_keys != fast )
    free(pl_keys);

  return rc;
}


static int
py_unify_constant(term_t t, atom_t c)
{ return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_at1, PL_ATOM, c);
}


static int
py_unify(term_t t, PyObject *obj, int flags)
{ if ( !obj )
  { check_error(obj);
    return FALSE;
  }

  if ( obj == Py_None )
    return py_unify_constant(t, ATOM_none);
  if ( PyBool_Check(obj) )
    return py_unify_constant(t, PyLong_AsLong(obj) ? ATOM_true : ATOM_false);

  if ( (flags&PYU_OBJ) )
  { if ( PyLong_CheckExact(obj) )
      return py_unify_long(t, obj);
    if ( PyFloat_CheckExact(obj) )
      return PL_unify_float(t, PyFloat_AsDouble(obj));
  } else
  { if ( PyLong_Check(obj) )
      return py_unify_long(t, obj);
    if ( PyFloat_Check(obj) )
      return PL_unify_float(t, PyFloat_AsDouble(obj));
  }

  if ( !(flags&PYU_OBJ) )
  { if ( PyUnicode_Check(obj) )
      return py_unify_unicode(t, obj, flags);
    if ( PyTuple_Check(obj) )
      return py_unify_tuple(t, obj, flags);
    if ( PyDict_Check(obj) )
      return py_unify_dict(t, obj, flags);
    if ( PyIter_Check(obj) )
      return py_unify_iter(t, obj, flags);
    if ( PySequence_Check(obj) )
      return py_unify_sequence(t, obj, flags);
    if ( PySet_Check(obj) )
      return py_unify_set(t, obj, flags);
    if ( py_is_record(obj) )
      return py_unify_record(t, obj);
  }

  return unify_py_obj(t, obj);
}


		 /*******************************
		 *       PROLOG -> PYTHON       *
		 *******************************/

static int
py_add_to_dict(term_t key, term_t value, void *closure)
{ PyObject *py_dict = closure;
  PyObject *py_value;
  char *s;
  int rc;

  if ( !py_from_prolog(value, &py_value) )
    return 1;				/* error */

  if ( PL_get_chars(key, &s, CVT_ATOM) )
  { rc = PyDict_SetItemString(py_dict, s, py_value);
  } else
  { PyObject *py_key;

    if ( !py_from_prolog(key, &py_key) )
      return 1;
    rc = PyDict_SetItem(py_dict, py_key, py_value);
  }

  if ( rc != 0 )
  { check_error(py_value);
    return 1;
  }

  return 0;
}


static int
add_prolog_key_value_to_dict(PyObject *py_dict, term_t tuple,
			     term_t key, term_t value)
{ if ( PL_is_functor(tuple, FUNCTOR_key_value2) )
  { _PL_get_arg(1, tuple, key);
    _PL_get_arg(2, tuple, value);
    PyObject *py_key = NULL, *py_value = NULL;
    int rc=0;

    if ( !py_from_prolog(key, &py_key) ||
	 !py_from_prolog(value, &py_value) ||
	 (rc=PyDict_SetItem(py_dict, py_key, py_value)) != 0 )
    { if ( rc == -1 )
	check_error(NULL);
      Py_CLEAR(py_dict);
      Py_CLEAR(py_key);
      Py_CLEAR(py_value);
      return FALSE;
    } else
      return TRUE;
  } else
  { Py_CLEAR(py_dict);
    return PL_type_error("py_key_value", tuple);
  }
}


static int
py_empty_dict(PyObject **obj)
{ PyObject *py_dict = check_error(PyDict_New());

  if ( py_dict )
  { *obj = py_dict;
    return TRUE;
  } else
    return FALSE;
}

static int
py_from_prolog_curl(term_t t, PyObject **obj)
{ term_t iter  = PL_new_term_ref();
  term_t head  = PL_new_term_ref();
  term_t key   = PL_new_term_ref();
  term_t value = head;
  PyObject *py_dict = PyDict_New();

  _PL_get_arg(1, t, iter);
  while( PL_is_functor(iter, FUNCTOR_comma2) )
  { _PL_get_arg(1, iter, head);
    _PL_get_arg(2, iter, iter);

    if ( !add_prolog_key_value_to_dict(py_dict, head, key, value) )
      return FALSE;
  }
  if ( !add_prolog_key_value_to_dict(py_dict, iter, key, value) )
    return FALSE;

  PL_reset_term_refs(iter);
  *obj = py_dict;
  return TRUE;
}


static int
py_from_prolog_at1(term_t t, PyObject **obj)
{ atom_t a;
  term_t arg = PL_new_term_ref();

  _PL_get_arg(1, t, arg);
  if ( PL_get_atom(arg, &a) )
  { int v = -1;

    PL_reset_term_refs(arg);

    if ( a == ATOM_false )
      v = 0;
    else if ( a == ATOM_true )
      v = 1;

    if ( v >= 0 )
    { *obj = check_error(PyBool_FromLong(v));
      return !!*obj;
    }

    if ( a == ATOM_none )
    { Py_INCREF(Py_None);
      *obj = Py_None;
      return TRUE;
    }
  }

  return PL_domain_error("py_constant", t);
}


static int
py_from_prolog(term_t t, PyObject **obj)
{ size_t len;
  functor_t funct = 0;
  int done = FALSE;

  // #(Term) --> stringify
  if ( PL_is_functor(t, FUNCTOR_hash1) )
  { term_t arg = PL_new_term_ref();
    _PL_get_arg(1, t, arg);

    PL_STRINGS_MARK();
    char *s;
    if ( PL_get_nchars(arg, &len, &s, REP_UTF8|CVT_ALL|CVT_WRITE_CANONICAL) )
    { *obj = PyUnicode_FromStringAndSize(s, len);
      done = TRUE;
    }
    PL_reset_term_refs(arg);
    PL_STRINGS_RELEASE();
    if ( done )
      return !!*obj;
  }

  if ( PL_is_integer(t) )
  { int64_t i;

    if ( PL_get_int64(t, &i) )
    { *obj = PyLong_FromLongLong(i);
      return TRUE;
    } else
    { char *s;
      int rc;

      PL_STRINGS_MARK();
      if ( (rc=PL_get_chars(t, &s, CVT_INTEGER)) ) /* TBD: use hexadecimal exchange */
	*obj = PyLong_FromString(s, NULL, 10);
      PL_STRINGS_RELEASE();

      return rc;
    }

    return FALSE;
  }
  if ( PL_is_float(t) )
  { double f;

    if ( PL_get_float_ex(t, &f) )
    { *obj = PyFloat_FromDouble(f);
      return TRUE;
    }

    return FALSE;
  }

  // Normal text representations.  Note that [] does not qualify
  // in SWI-Prolog as an atom
  PL_STRINGS_MARK();
  char *s;
  if ( PL_get_nchars(t, &len, &s, REP_UTF8|CVT_ATOM|CVT_STRING) )
  { *obj = check_error(PyUnicode_FromStringAndSize(s, len));
    done = TRUE;
  }
  PL_STRINGS_RELEASE();
  if ( done )
    return !!*obj;

  if ( PL_skip_list(t, 0, &len) == PL_LIST )
  { term_t tail = PL_copy_term_ref(t);
    term_t head = PL_new_term_ref();
    PyObject *list = PyList_New(len);
    int rc = TRUE;

    for(Py_ssize_t i=0; PL_get_list(tail, head, tail); i++)
    { PyObject *el = NULL;

      if ( (rc=py_from_prolog(head, &el)) )
      { PyList_SetItem(list, i, el);
      } else
	break;
    }
    if ( rc )
      *obj = list;
    else
      Py_CLEAR(list);
    return rc;
  }

  if ( PL_get_functor(t, &funct) )
  { if ( funct == FUNCTOR_at1 )
      return py_from_prolog_at1(t, obj);

    if ( funct == FUNCTOR_py_set1 )
    { term_t tail = PL_new_term_ref();

      _PL_get_arg(1, t, tail);
      if ( PL_skip_list(tail, 0, NULL) == PL_LIST )
      { term_t head = PL_new_term_ref();
	PyObject *set = check_error(PySet_New(NULL));
	int rc = TRUE;

	if ( !set )
	  return FALSE;

	while(PL_get_list(tail, head, tail))
	{ PyObject *el;

	  if ( (rc=py_from_prolog(head, &el)) )
	  { int r = PySet_Add(set, el);
	    Py_CLEAR(el);
	    if ( r )
	    { check_error(NULL);
	      rc = FALSE;
	      break;
	    }
	  } else
	    break;
	}
	if ( rc )
	  *obj = set;
	else
	  Py_CLEAR(set);

	return rc;
      } else
	return PL_type_error("py_set", t);
    }

    if ( PL_is_dict(t) )
    { PyObject *py_dict = check_error(PyDict_New());

      if ( !py_dict )
	return FALSE;
      if ( PL_for_dict(t, py_add_to_dict, py_dict, 0) != 0 )
      { Py_CLEAR(py_dict);
	return FALSE;
      }
      *obj = py_dict;
      return TRUE;
    }

    if ( funct == FUNCTOR_py1 )
    { term_t a = PL_new_term_ref();
      atom_t c;
      int rc;

      _PL_get_arg(1, t, a);

      if ( PL_is_functor(a, FUNCTOR_curl1) )
      { rc = py_from_prolog_curl(a, obj);
      } else if ( PL_get_atom(a, &c) && c == ATOM_curl )
      { rc = py_empty_dict(obj);
      } else
	goto error;

      PL_reset_term_refs(a);
      return rc;
    }

    if ( funct == FUNCTOR_curl1 )
      return py_from_prolog_curl(t, obj);

    /* prolog(Term) --> record */
    if ( funct == FUNCTOR_prolog1 )
    { term_t a = PL_new_term_ref();
      PyObject *r;
      _PL_get_arg(1, t, a);
      if ( (r=py_record(a)) )
      { PL_reset_term_refs(a);
	*obj = r;
	return TRUE;
      }
      return FALSE;
    }

    /* -(a,b,...) --> Python tuple  */
    if ( PL_functor_name(funct) == ATOM_tuple )
    { size_t arity = PL_functor_arity(funct);
      PyObject *tp = check_error(PyTuple_New(arity));

      if ( tp )
      { term_t arg = PL_new_term_ref();

	for(size_t i=0; i<arity; i++)
	{ PyObject *py_arg;

	  _PL_get_arg(i+1, t, arg);
	  if ( !py_from_prolog(arg, &py_arg) )
	  { Py_CLEAR(tp);
	    return FALSE;
	  }
	  Py_INCREF(py_arg);
	  PyTuple_SetItem(tp, i, py_arg);
	}
	PL_reset_term_refs(arg);
	*obj = tp;
	return TRUE;
      }

      return FALSE;
    }

    if ( funct == FUNCTOR_module2 )
    { term_t call = PL_copy_term_ref(t);
      PyObject *py_target = NULL;

      if ( unchain(call, &py_target) )
      { PyObject *py_res = py_eval(py_target, call);
	Py_XDECREF(py_target);
	if ( py_res )
	{ *obj = py_res;
	  return TRUE;
	}
      }

      Py_CLEAR(py_target);
      return FALSE;
    }
  }

  if ( get_py_obj(t, obj, FALSE) )
    return TRUE;

error:
  return PL_domain_error("py_data", t),FALSE;
}


		 /*******************************
		 *           RECORDS            *
		 *******************************/

static PyObject *
py_term_constructor(void)
{ static PyObject *con = NULL;

  if ( !con )
  { PyObject *janus;

    if ( (janus=mod_janus()) )
      con = PyObject_GetAttrString(janus, "Term");
  }

  return con;
}

static PyObject *
py_record(term_t t)
{ record_t rec = PL_record(t);

  if ( rec )
  { PyObject *r = PyLong_FromLongLong((long long)rec);
    PyObject *argv = NULL;
    PyObject *con;
    PyObject *term = NULL;

    if ( (con=py_term_constructor()) &&
	 (argv=PyTuple_New(1)) )
    { Py_INCREF(r);
      PyTuple_SetItem(argv, 0, r);
      term = PyObject_CallObject(con, argv);
    }

    Py_CLEAR(r);
    Py_CLEAR(argv);
    return term;
  }

  Py_SetPrologError(PL_exception(0));
  return NULL;
}


static int
py_unify_record(term_t t, PyObject *rec)
{ PyObject *r = check_error(PyObject_GetAttrString(rec, "_record"));
  int rc = FALSE;

  if ( r )
  { term_t tmp;
    long long v;

    rc = ( (v=PyLong_AsLongLong(r)) &&
	   (tmp=PL_new_term_ref()) &&
	   PL_recorded((record_t)v, tmp) &&
	   PL_unify(t, tmp) );
    Py_DECREF(r);
  }

  return rc;
}


static PyObject *
py_free_record(PyObject *rec)
{ int rc = FALSE;

  if ( PyLong_Check(rec) )
  { long long v = PyLong_AsLongLong(rec);

    if ( v )
      PL_erase((record_t)v);
    rc = TRUE;
  }

  if ( rc )
    Py_RETURN_NONE;

  return NULL;
}


static int
py_is_record(PyObject *rec)
{ int rc;

  PyObject *cls = PyObject_GetAttrString(rec, "__class__");
  rc = (cls == py_term_constructor());
  Py_CLEAR(cls);

  return rc;
}


		 /*******************************
		 *	   PROLOG BINDING	*
		 *******************************/

#define CVT_TEXT_EX (CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION)

static int
py_init(void)
{ if ( !py_initialize_done )
  { predicate_t pred = PL_predicate("py_initialize", 0, "janus");

    return PL_call_predicate(NULL, PL_Q_NORMAL, pred, 0);
  }

  return TRUE;
}

static foreign_t
py_initialize_(term_t prog, term_t Argv, term_t options)
{ wchar_t *pname;
  size_t argc;
  wchar_t **argv = NULL;
  term_t tail = PL_copy_term_ref(Argv);
  term_t head = PL_new_term_ref();
  int rc = FALSE;

  LOCK();
  if ( py_initialize_done )
  { UNLOCK();
    return FALSE;
  }

  PL_STRINGS_MARK();
  if ( !PL_get_wchars(prog, NULL, &pname, CVT_TEXT_EX) )
    goto failed;
  if ( PL_skip_list(Argv, 0, &argc) != PL_LIST )
  { PL_type_error("list", Argv);
    goto failed;
  }
  if ( !(argv = malloc((argc+1)*sizeof(*argv))) )
  { PL_resource_error("memory");
    goto failed;
  }
  for(size_t i=0; i<argc; i++)
  { if ( !PL_get_list_ex(tail, head, tail) ||
	 !PL_get_wchars(head, NULL, &argv[i], CVT_TEXT_EX) )
      goto failed;
  }

#if !HAVE_PYCONFIG
#ifdef _MSC_VER
#pragma warning(disable : 4996)
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdeprecated-declarations"
#endif
  Py_SetProgramName(pname);
  Py_Initialize();
#ifndef _MSC_VER
#pragma GCC diagnostic pop
#endif
#else
  PyConfig config;

#define PYTRY(g) if ( PyStatus_Exception(g) ) goto py_error;

  PyConfig_InitPythonConfig(&config);
  PYTRY(PyConfig_SetString(&config, &config.program_name, pname));
  PYTRY(PyConfig_SetArgv(&config, argc, argv));
  PYTRY(Py_InitializeFromConfig(&config));

  PyConfig_Clear(&config);
#endif
  py_initialize_done = TRUE;
  rc = TRUE;
  goto succeeded;

#ifdef PYTRY
py_error:
#endif
  check_error(NULL);
  PL_warning("Python initialization failed");
#if HAVE_PYCONFIG
  PyConfig_Clear(&config);
#endif

failed:
  rc = FALSE;

succeeded:
  if ( argv )
    free(argv);
  PL_STRINGS_RELEASE();
  UNLOCK();

  return rc;
}


/* Get the initial target to work on
 */

static int
get_py_initial_target(term_t t, PyObject **py_target, int error)
{ if ( get_py_obj(t, py_target, FALSE) ||
       get_py_module(t, py_target) )
    return TRUE;

  if ( error )
    PL_type_error("py_target", t);

  return FALSE;
}

static PyObject*
builtin_function(PyObject *builtins, atom_t fname)
{ if ( !builtins )
    builtins = PyEval_GetBuiltins(); /* borrowed reference */
  PyObject *py_func = NULL;

  PL_STRINGS_MARK();
  char *fn;
  if ( PL_atom_mbchars(fname, NULL, &fn, REP_UTF8|CVT_EXCEPTION) )
    py_func = PyDict_GetItemString(builtins, fn); /* borrowed reference */
  PL_STRINGS_RELEASE();

  if ( !py_func )
  { term_t fn;

    if ( !PL_exception(0) &&
	 (fn=PL_new_term_ref()) &&
	 PL_put_atom(fn, fname) )
      PL_existence_error("python_builtin", fn);
  } else
    Py_INCREF(py_func);

  return py_func;
}


/* Evaluate func on obj.  If obj = NULL, evaluate a builtin function
 * Return value: New Reference
 */

static PyObject *
py_eval(PyObject *obj, term_t func)
{ char *attr;
  atom_t fname;
  size_t arity;
  PyObject *py_res = NULL;

  if ( !obj && get_py_initial_target(func, &py_res, FALSE) )
    return py_res;

  if ( obj && PL_get_chars(func, &attr, CVT_ATOM) )
  { return check_error(PyObject_GetAttrString(obj, attr));
  } else if ( PL_get_name_arity(func, &fname, &arity) )
  { PyObject *py_func = NULL;
    PyObject *py_argv = NULL;
    PyObject *py_kws  = NULL;

    if ( obj == PyEval_GetBuiltins() )
    { py_func = builtin_function(obj, fname);
    } else if ( obj )
    { PL_STRINGS_MARK();
      char *fn;
      if ( PL_atom_mbchars(fname, NULL, &fn, REP_UTF8|CVT_EXCEPTION) )
	py_func = check_error(PyObject_GetAttrString(obj, fn));
      PL_STRINGS_RELEASE();
    } else
    { py_func = builtin_function(NULL, fname);
    }
    if ( !py_func )
      goto out;

    term_t arg = PL_new_term_ref();

    for(size_t i=0; i<arity; i++)
    { PyObject *py_arg;

      _PL_get_arg(i+1, func, arg);
      if ( PL_is_functor(arg, FUNCTOR_eq2) )
      { py_kws = PyDict_New();
	term_t k = PL_new_term_ref();
	term_t v = PL_new_term_ref();

	if ( py_argv )
	{
#if HAVE__PYTUPLE_RESIZE
	  if ( _PyTuple_Resize(&py_argv, i) == -1 )
	  { check_error(py_argv);
	    goto out;
	  }
#else
	  PyObject *slice = check_error(PyTuple_GetSlice(py_argv, 0, i));
	  if ( !slice )
	    goto out;
	  Py_CLEAR(py_argv);
	  py_argv = slice;
#endif
	} else
	{ if ( !(py_argv=check_error(PyTuple_New(0))) )
	    goto out;
	}

	for(; i<arity; i++)
	{ _PL_get_arg(i+1, func, arg);
	  if ( PL_is_functor(arg, FUNCTOR_eq2) )
	  { _PL_get_arg(1, arg, k);
	    _PL_get_arg(2, arg, v);
	    if ( py_add_to_dict(k, v, py_kws) != 0 )
	      goto out;
	  } else
	  { PL_domain_error("py_keyword_arg", arg);
	    goto out;
	  }
	}
      } else
      { if ( !py_argv && !(py_argv=check_error(PyTuple_New(arity))) )
	  goto out;

	if ( !py_from_prolog(arg, &py_arg) )
	  goto out;
	PyTuple_SetItem(py_argv, i, py_arg);
      }
    }
    PL_reset_term_refs(arg);

    if ( !py_argv && !py_kws )
      py_res = check_error(PyObject_CallObject(py_func, py_argv));
    else
      py_res = check_error(PyObject_Call(py_func, py_argv, py_kws));
  out:
    Py_CLEAR(py_argv);
    Py_CLEAR(py_kws);
    Py_CLEAR(py_func);

    return py_res;
  } else
  { PL_type_error("py_callable", func);
    return NULL;
  }
}


/* _unchain_ a sequence of a:b:c():..., returning a PyObject to
 * operate on and the final function call or attribute in `call`
 * As `call` is written to, it must be a copy of an argument
 * term.
 *
 * Decrements the references to objects we have done with and
 * returns a new reference for *py_target
 */

static int
unchain(term_t call, PyObject **py_target)
{ term_t on = PL_new_term_ref();
  int rc = TRUE;

  while ( PL_is_functor(call, FUNCTOR_module2) )
  { _PL_get_arg(1, call, on);
    _PL_get_arg(2, call, call);

    if ( !*py_target )
    { if ( !(rc=get_py_initial_target(on, py_target, TRUE)) )
	break;
    } else
    { PyObject *next = py_eval(*py_target, on);

      Py_XDECREF(*py_target);
      *py_target = next;
      if ( !next )
      { rc = FALSE;
	break;
      }
    }
  }

  PL_reset_term_refs(on);

  return rc;
}

static int
py_gil_ensure(PyGILState_STATE *state)
{ if ( !py_init() )
    return FALSE;

  py_resume();
  *state = PyGILState_Ensure();
#ifndef HAVE_PYGILSTATE_CHECK
  have_gil = TRUE;
#endif
  if ( delayed )
    delayed_decref(NULL);

  return TRUE;
}


static void
py_gil_release(PyGILState_STATE state)
{ if ( state )
  {
#ifndef HAVE_PYGILSTATE_CHECK
    have_gil = FALSE;
#endif
    PyGILState_Release(state);
    py_yield();
  }
}


static int
atom_domain_error(const char *dom, atom_t a)
{ term_t t;

  return ( (t=PL_new_term_ref()) &&
	   PL_put_atom(t, a) &&
	   PL_domain_error(dom, t) );
}


static PL_option_t pycall_options[] =
{ PL_OPTION("py_string_as",   OPT_ATOM),
  PL_OPTION("py_object",      OPT_BOOL),
  PL_OPTIONS_END
};


static int
get_conversion_options(term_t options, int *flags)
{ if ( options )
  { atom_t string_as = 0;
    int py_object    = -1;

    if ( !PL_scan_options(options, 0, "py_call_options", pycall_options,
			  &string_as, &py_object) )
      return FALSE;
    if ( py_object != -1 )
    { if ( py_object )
	*flags |= PYU_OBJ;
      else
	*flags &= ~PYU_OBJ;
    }
    if ( string_as )
    { if ( string_as == ATOM_atom )
	*flags &= ~PYU_STRING;
      else if ( string_as == ATOM_string )
	*flags |= PYU_STRING;
      else
	return atom_domain_error("py_string_as", string_as);
    }
  }

  return TRUE;
}

static foreign_t
py_call3(term_t Call, term_t result, term_t options)
{ PyObject *py_target = NULL;
  term_t call = PL_copy_term_ref(Call);
  term_t val = 0;
  int rc = TRUE;
  PyGILState_STATE state;
  int uflags = 0;

  if ( !get_conversion_options(options, &uflags) )
    return FALSE;

  if ( PL_is_functor(call, FUNCTOR_eq2) )
  { val = PL_new_term_ref();
    _PL_get_arg(2, call, val);
    _PL_get_arg(1, call, call);
  }

  if ( !py_gil_ensure(&state) )
    return FALSE;

  rc = unchain(call, &py_target);

  if ( rc )
  { if ( val )			/* py_target:attr = val */
    { char *attr;
      if ( py_target )
      { if ( (rc=PL_get_chars(call, &attr, CVT_ATOM|CVT_EXCEPTION)) )
	{ PyObject *py_val = NULL;

	  if ( (rc=py_from_prolog(val, &py_val)) )
	  { if ( PyObject_SetAttrString(py_target, attr, py_val) == -1 )
	      rc = !!check_error(NULL);
	    if ( rc && result )
	      rc = py_unify_constant(result, ATOM_none);
	  }
	  Py_CLEAR(py_val);
	}
      } else
	rc = PL_domain_error("py_attribute", call);
    } else
    { PyObject *py_res = py_eval(py_target, call);

      Py_XDECREF(py_target);
      py_target = py_res;
      rc = !!py_target;
      if ( rc && result )
	rc = py_unify(result, py_target, uflags);
    }
  }

  Py_CLEAR(py_target);
  py_gil_release(state);

  return rc;
}

static foreign_t
py_call2(term_t Call, term_t Ret)
{ return py_call3(Call, Ret, 0);
}

static foreign_t
py_call1(term_t Call)
{ return py_call3(Call, 0, 0);
}


typedef struct
{ PyObject *iterator;
  PyObject *next;
  int uflags;
  int allocated;
} iter_state;

static iter_state *
alloc_iter_state(iter_state *state)
{ if ( !state->allocated )
  { iter_state *copy = malloc(sizeof(*state));
    if ( copy )
    { *copy = *state;
      copy->allocated = TRUE;
    }
    state = copy;
  }

  return state;
}

static void
free_iter_state(iter_state *state)
{ Py_CLEAR(state->iterator);
  Py_CLEAR(state->next);
  if ( state->allocated )
    free(state);
}

static foreign_t
py_iter3(term_t Iterator, term_t Result, term_t options, control_t handle)
{ iter_state iter_buf;
  iter_state *state;
  PyGILState_STATE gil_state;
  int rc = FALSE;

  switch( PL_foreign_control(handle) )
  { case PL_FIRST_CALL:
    { term_t call = PL_copy_term_ref(Iterator);
      PyObject *iter = NULL;

      state = &iter_buf;
      memset(state, 0, sizeof(*state));
      if ( !get_conversion_options(options, &state->uflags) )
	return FALSE;

      if ( !py_gil_ensure(&gil_state) )
	return FALSE;

      if ( !unchain(call, &iter) )
	goto out;
      if ( !(iter = py_eval(iter, call)) )
	goto out;

      state->iterator = check_error(PyObject_GetIter(iter));
      Py_DECREF(iter);
      if ( !state->iterator )
	goto out;
      state->next = PyIter_Next(state->iterator);
      break;
    }
    case PL_REDO:
      state = PL_foreign_context_address(handle);
      if ( !py_gil_ensure(&gil_state) )
	return FALSE;
      break;
    case PL_PRUNED:
      state = PL_foreign_context_address(handle);
      if ( !py_gil_ensure(&gil_state) )
	return FALSE;
      rc = TRUE;
      goto out;
    default:
      assert(0);
      return FALSE;
  }

  fid_t fid = PL_open_foreign_frame();
  if ( fid )
  { while ( state->next )
    { rc = py_unify(Result, state->next, state->uflags);

      Py_CLEAR(state->next);
      state->next = PyIter_Next(state->iterator);

      if ( rc )
      { PL_close_foreign_frame(fid);

	if ( state->next )
	{ py_gil_release(gil_state);
	  PL_retry_address(alloc_iter_state(state)); /* returns */
	}
	rc = !PL_exception(0);
	goto out;
      }

      if ( PL_exception(0) )
      { PL_close_foreign_frame(fid);
	rc = FALSE;
	goto out;
      }
      PL_rewind_foreign_frame(fid);
    }
  }

out:
  free_iter_state(state);
  py_gil_release(gil_state);

  return rc;
}


static foreign_t
py_iter2(term_t Iterator, term_t Result, control_t handle)
{ return py_iter3(Iterator, Result, 0, handle);
}


static foreign_t
py_with_gil(term_t goal)
{ PyGILState_STATE state;

  if ( !py_gil_ensure(&state) )
    return FALSE;
  int rc = PL_call(goal, NULL);
  py_gil_release(state);

  return rc;
}


static PL_option_t pyrun_options[] =
{ PL_OPTION("file_name", OPT_STRING),
  PL_OPTION("start",     OPT_ATOM),
  PL_OPTIONS_END
};



static foreign_t
py_run(term_t Cmd, term_t Globals, term_t Locals, term_t Result, term_t options)
{ char *cmd;

  if ( PL_get_chars(Cmd, &cmd, CVT_ATOM|CVT_STRING|CVT_LIST|CVT_EXCEPTION) )
  { PyObject *locals=NULL, *globals=NULL;
    PyObject *result = NULL;
    PyGILState_STATE state;
    int rc;
    char *file_name = "string";
    atom_t start = ATOM_file;
    int start_token;

    if ( !PL_scan_options(options, 0, "py_run_options", pyrun_options,
			  &file_name, &start) )
      return FALSE;
    if ( start == ATOM_file )
      start_token = Py_file_input;
    else if ( start == ATOM_eval )
      start_token = Py_eval_input;
    else if ( start == ATOM_single )
      start_token = Py_single_input;
    else
      return atom_domain_error("py_run_start", start);

    if ( !py_gil_ensure(&state) )
      return FALSE;

    if ( (rc = (py_from_prolog(Globals, &globals) &&
		py_from_prolog(Locals, &locals))) )
    { PyObject *code = check_error(Py_CompileString(cmd, file_name,
						    start_token));

      if ( code )
	result = check_error(PyEval_EvalCode(code, globals, locals));

      Py_CLEAR(code);

      if ( result )
      { rc = py_unify(Result, result, 0);
      } else
	rc = FALSE;
    }

    Py_CLEAR(result);
    Py_CLEAR(locals);
    Py_CLEAR(globals);
    py_gil_release(state);

    return rc;
  }

  return FALSE;
}


static foreign_t
py_str(term_t t, term_t str)
{ PyObject *obj;
  int rc;
  PyGILState_STATE state;

  if ( !py_gil_ensure(&state) )
    return FALSE;
  if ( (rc=py_from_prolog(t, &obj)) )
  { PyObject *s = check_error(PyObject_Str(obj));
    Py_DECREF(obj);

    if ( s )
    { rc = py_unify(str, s, 0);
      Py_DECREF(s);
    } else
      rc = FALSE;
  }
  py_gil_release(state);

  return rc;
}


static foreign_t
py_free(term_t t)
{ atom_t blob;

  if ( PL_get_atom(t, &blob) )
  { void *data;
    size_t size;
    PL_blob_t *type;

    data = PL_blob_data(blob, &size, &type);
    (void)data;
    if ( type == &PY_OBJECT )
    { if ( size == 0 )
	return PL_existence_error("PyObject", t);
      return PL_free_blob(blob);
    }
  }

  return PL_type_error("py_obj", t);
}


static foreign_t
py_is_object(term_t t)
{ PyObject *obj = NULL;
  int rc = get_py_obj(t, &obj, FALSE);

  Py_CLEAR(obj);

  return rc;
}



		 /*******************************
		 *        GIL MANAGEMENT        *
		 *******************************/

typedef struct
{ PyThreadState *state;
  int		 yielded;
} py_state_t;

static _Thread_local py_state_t py_state;

static void
py_yield(void)
{ if ( !py_state.yielded && PyGILState_GetThisThreadState() )
  { DEBUG(1, Sdprintf("Yielding ..."));
    py_state.state = PyEval_SaveThread();
    DEBUG(1, Sdprintf("ok\n"));
    py_state.yielded = TRUE;
  }
}

static void
py_resume(void)
{ if ( py_state.yielded )
  { DEBUG(1, Sdprintf("Un yielding ..."));
    PyEval_RestoreThread(py_state.state);
    DEBUG(1, Sdprintf("ok\n"));
    py_state.yielded = FALSE;
  }
}


		 /*******************************
		 *	      REGISTER		*
		 *******************************/

#define MKATOM(n) \
	ATOM_ ## n = PL_new_atom(#n)
#define MKFUNCTOR(name, arity) \
	FUNCTOR_ ## name ## arity = PL_new_functor(PL_new_atom(#name), arity)

install_t
install_janus(void)
{ MKATOM(none);
  MKATOM(false);
  MKATOM(true);
  MKATOM(atom);
  MKATOM(string);
  MKATOM(file);
  MKATOM(eval);
  MKATOM(single);
  MKATOM(builtins);
  ATOM_tuple  = PL_new_atom("-");
  ATOM_pydict = PL_new_atom("py");
  ATOM_curl   = PL_new_atom("{}");

  MKFUNCTOR(python_error, 3);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(py, 1);
  FUNCTOR_module2    = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_eq2        = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_hash1      = PL_new_functor(PL_new_atom("#"), 1);
  FUNCTOR_comma2     = PL_new_functor(PL_new_atom(","), 2);
  FUNCTOR_curl1      = PL_new_functor(PL_new_atom("{}"), 1);
  FUNCTOR_py_set1    = PL_new_functor(PL_new_atom("py_set"), 1);
  FUNCTOR_at1        = PL_new_functor(PL_new_atom("@"), 1);
  FUNCTOR_key_value2 = FUNCTOR_module2;
  MKFUNCTOR(prolog, 1);

#define REGISTER(name, arity, func, flags) \
        PL_register_foreign_in_module("janus", name, arity, func, flags)

  REGISTER("py_initialize_",  3, py_initialize_, 0);
  REGISTER("py_call",         1, py_call1,       0);
  REGISTER("py_call",         2, py_call2,       0);
  REGISTER("py_call",         3, py_call3,       0);
  REGISTER("py_iter",         2, py_iter2,       PL_FA_NONDETERMINISTIC);
  REGISTER("py_iter",         3, py_iter3,       PL_FA_NONDETERMINISTIC);
  REGISTER("py_run",          5, py_run,         0);
  REGISTER("py_free",         1, py_free,        0);
  REGISTER("py_is_object",    1, py_is_object,   0);
  REGISTER("py_with_gil",     1, py_with_gil,    PL_FA_TRANSPARENT);
  REGISTER("py_str",          2, py_str,         0);
  REGISTER("py_debug",        1, py_debug,       0);

  if ( PyImport_AppendInittab("_swipl", PyInit__swipl) == -1 )
    Sdprintf("Failed to add module swipl to Python");
}

install_t
uninstall_janus(void)
{ if ( py_module_table )
  { py_free_hashmap(py_module_table);
    py_module_table = NULL;
  }
#if O_DEBUG
  Py_FinalizeEx();
#endif
}
