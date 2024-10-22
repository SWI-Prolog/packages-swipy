/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023-2024, SWI-Prolog Solutions b.v.
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
#if O_PYFRAME
#include <frameobject.h>	/* Limited API */
#endif
#include <assert.h>
#include <stdbool.h>

static atom_t ATOM_none;
static atom_t ATOM_false;
static atom_t ATOM_true;
static atom_t ATOM_pydict;
static atom_t ATOM_tuple;
static atom_t ATOM_curl;
static atom_t ATOM_atom;
static atom_t ATOM_string;
static atom_t ATOM_codes;
static atom_t ATOM_chars;
static atom_t ATOM_file;
static atom_t ATOM_eval;
static atom_t ATOM_single;
static atom_t ATOM_builtins;
static atom_t ATOM_locals;
static atom_t ATOM_globals;
static atom_t ATOM_dict;

static functor_t FUNCTOR_python_error2;
static functor_t FUNCTOR_python_stack1;
static functor_t FUNCTOR_context2;
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
static functor_t FUNCTOR_eval1;
static functor_t FUNCTOR_string1;

static int py_initialize_done = FALSE;
static int py_module_initialize_done = FALSE;
static int py_finalizing = FALSE;
static int py_gil_thread = 0;	/* Prolog thread that owns the GIL */

#ifdef PL_Q_EXCEPT_HALT
static functor_t FUNCTOR_unwind1;
static functor_t FUNCTOR_halt1;
static atom_t ATOM_keyboard_interrupt;
#else
static atom_t ATOM_aborted;
static int exit_requested = INT_MIN;
#endif

typedef struct
{ PyGILState_STATE gil;
  int use_gil;
} py_gil_state;

static PyObject *check_error(PyObject *obj);
static int py_unify(term_t t, PyObject *obj, int flags);
static int py_from_prolog(term_t t, PyObject **obj);
static void py_yield_first(void);
static int py_gil_ensure(py_gil_state *state);
static void py_gil_release(py_gil_state state);
static void py_yield(py_gil_state state);
static void py_resume(py_gil_state *state);
static PyObject *py_record(term_t t);
static int py_unify_record(term_t t, PyObject *rec);
static int py_unify_constant(term_t t, atom_t c);
static int py_is_record(PyObject *rec);
static PyObject *py_free_record(PyObject *rec);
static int unchain(term_t call, PyObject **py_target);
static PyObject *py_eval(PyObject *obj, term_t func);
static foreign_t py_finalize(void);

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
__sync_bool_compare_and_swap(volatile void **addr, void *old, void *new)
{
#if _WIN64
  return _InterlockedCompareExchange64((volatile __int64*)addr, (__int64)new, (__int64)old) == (__int64)old;
#else
  return _InterlockedCompareExchange((volatile long*)addr, (long)new, (long)old) == (long)old;
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

#define PYU_SMASK  0x0003		/* Mask for types */
#define PYU_ATOM   0x0000		/* Unify text as Prolog atom */
#define PYU_STRING 0x0001		/* Unify text as Prolog string */
#define PYU_CODES  0x0002		/* Unify text as Prolog code list */
#define PYU_CHARS  0x0003		/* Unify text as Prolog char list */
#define PYU_OBJ    0x0004		/* Unify as object */
#define PYU_CURL   0x0008		/* Unify dict as {...} */
#define PYU_ERROR  0x0010		/* Inside check_error() */

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

/* Write a Python  object reference as <py_<Class>>(Ref).   To get the
 * class however, we need the  GIL.  Unfortunately, this may deadlock.
 * Assume
 *
 *   - We printing an object ref to stream S.  We hold stream S and
 *     try to get the GIL.
 *   - Some other thread holds the GIL and tries to write to S.
 *
 * To resolve this we'd need a PyGILState_TryEnsure(), but this does
 * not seem to exists.
 */

static int
write_python_object(IOSTREAM *s, atom_t symbol, int flags)
{ PyObject *obj = PL_blob_data(symbol, NULL, NULL);

  if ( obj )
  { PyObject *cls = NULL;
    PyObject *cname = NULL;
    const char *name;
    py_gil_state state;

    if ( py_gil_thread == 0 && py_gil_ensure(&state) )
    { if ( (cls=PyObject_GetAttrString(obj, "__class__")) &&
	   (cname=PyObject_GetAttrString(cls, "__name__")) )
	name = PyUnicode_AsUTF8AndSize(cname, NULL);
      else
	name = "noclass";

      SfprintfX(s, "<py_%Us>(%p)", name, obj);

      Py_CLEAR(cls);
      Py_CLEAR(cname);
      py_gil_release(state);
    } else
    { SfprintfX(s, "<py_obj(no-GIL)>(%p)", obj);
    }
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
      return PL_existence_error("py_object", t);
  }

  if ( error )
    PL_type_error("py_object", t);

  return FALSE;
}

static int
get_py_name(term_t name, PyObject **obj, int flags)
{ char *idname;

  if ( PL_get_chars(name, &idname, CVT_ATOM|flags) )
  { PyObject *id = check_error(PyUnicode_FromString(idname));

    if ( id )
    { *obj = id;
      return TRUE;
    }
  }

  return FALSE;
}

static htable *py_module_table = NULL;
static PL_option_t import_options[] =
{ PL_OPTION("as", OPT_ATOM),
  PL_OPTIONS_END
};


static int
py_register_module(term_t name, term_t options, PyObject **mod, int flags)
{ PyObject *idobj = NULL;
  int rc = FALSE;

  if ( !py_module_table )
    py_module_table = py_new_hashmap();

  if ( get_py_name(name, &idobj, flags) )
  { PyObject *m;
    atom_t as = 0;

    if ( options )
    { if ( !PL_scan_options(options, 0, "py_import_options", import_options,
			    &as) )
	goto out;
      if ( as && py_lookup_hashmap(py_module_table, as) )
      { term_t as_term;
	rc = ((as_term=PL_new_term_ref()) &&
	      PL_put_atom(as_term, as) &&
	      PL_permission_error("import_as", "py_module", as_term));
	goto out;
      }
    }
    if ( !as && !PL_get_atom_ex(name, &as) )
      goto out;

    m = check_error(PyImport_Import(idobj));
    if ( m )
    { PyObject *old = NULL;
      if ( mod )
	*mod = m;
      rc = py_add_hashmap(py_module_table, as, m, &old);
      if ( old )
	Py_DECREF(old);
    } else
      rc = FALSE;
  }

out:
  Py_CLEAR(idobj);
  return rc;
}


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
    { if ( py_register_module(name, 0, mod, 0) )
      { Py_INCREF(*mod);
	return TRUE;
      }
    }
  }

  return FALSE;
}


static foreign_t
py_update_module_cache(term_t name)
{ int rc;
  py_gil_state state;

  if ( !py_gil_ensure(&state) )
    return FALSE;
  rc = py_register_module(name, 0, NULL, CVT_EXCEPTION);
  py_gil_release(state);

  return rc;
}


static foreign_t
py_import(term_t spec, term_t options)
{ py_gil_state state;
  int rc;

  if ( !py_gil_ensure(&state) )
    return FALSE;
  rc = py_register_module(spec, options, NULL, CVT_EXCEPTION);
  py_gil_release(state);

  return rc;
}

static void
propagate_sys_exit(int code)
{
#ifdef PL_Q_EXCEPT_HALT
  PL_halt(code|PL_HALT_WITH_EXCEPTION);
#else
  exit_requested = code;
  PL_action(PL_ACTION_ABORT);
#endif
}

static bool
propagate_keyboard_interrupt(void)
{
#ifdef PL_Q_EXCEPT_HALT
  term_t ex;
  if ( (ex=PL_new_term_ref()) &&
       PL_unify_term(ex, PL_FUNCTOR, FUNCTOR_unwind1,
		     PL_ATOM, ATOM_keyboard_interrupt) )
  { PL_raise_exception(ex);
    return true;
  }
#endif
  return false;
}


static PyObject *
check_error(PyObject *obj)
{ PyObject *ex = PyErr_Occurred();

  if ( ex )
  { PyObject *type = NULL, *tname = NULL, *value = NULL, *stack = NULL;
    const char *error_type = NULL;

    PyErr_Fetch(&type, &value, &stack);
    if ( (tname=PyObject_GetAttrString(type, "__name__")) )
      error_type = PyUnicode_AsUTF8AndSize(tname, NULL);

    PyObject *exit_code;
    long long code;
    if ( error_type )
    { if ( strcmp(error_type, "SystemExit") == 0 &&
	   (exit_code=PyObject_GetAttrString(value, "code")) &&
	   (code=PyLong_AsLongLong(exit_code)) )
      { propagate_sys_exit((int)code);
	Py_CLEAR(tname);
	return NULL;
      }

      if ( strcmp(error_type, "KeyboardInterrupt") == 0 )
      { if ( propagate_keyboard_interrupt() )
	  return NULL;
      }
    }

    term_t t   = PL_new_term_ref();
    term_t av  = PL_new_term_refs(2);
    term_t ctx = PL_new_term_ref();

    if ( stack )
    { if ( !py_unify(ctx, stack, PYU_ERROR) ||
	   !PL_cons_functor_v(ctx, FUNCTOR_python_stack1, ctx) ||
	   !PL_cons_functor(ctx, FUNCTOR_context2, t, ctx) )
	return NULL;
      PL_put_variable(t);
    }

    if ( error_type &&
         PL_unify_chars(av+0, PL_ATOM|REP_UTF8, (size_t)-1, error_type) &&
	 (value ? py_unify(av+1, value, PYU_ERROR)
		: py_unify_constant(av+1, ATOM_none)) &&
	 PL_cons_functor_v(t, FUNCTOR_python_error2, av) &&
	 PL_cons_functor(t, FUNCTOR_error2, t, ctx) )
      PL_raise_exception(t);

    Py_CLEAR(tname);
    return NULL;
  } else
    return obj;
}


static int py_unify_unicode(term_t t, PyObject *obj, int flags);
static PyObject *enum_constructor = NULL;

static int
PyEnum_Check(PyObject *obj)
{ if ( !enum_constructor )
  { PyObject *mod_name = NULL;
    PyObject *mod = NULL;

    if ( (mod_name=check_error(PyUnicode_FromString("enum"))) &&
         (mod=check_error(PyImport_Import(mod_name))) )
      enum_constructor = check_error(PyObject_GetAttrString(mod, "Enum"));

    Py_CLEAR(mod_name);
    Py_CLEAR(mod);
  }

  return PyObject_IsInstance(obj, enum_constructor);
}


static int
py_unify_enum(term_t t, PyObject *obj)
{ PyObject *name = PyObject_GetAttrString(obj, "name");
  return py_unify_unicode(t, name, 0);
}


/* Returns the fractions.Fraction constructor */

static PyObject *
func_Fraction(void)
{ static PyObject *func = NULL;

  if ( !func )
  { PyObject *mod_name = NULL;
    PyObject *mod = NULL;

    if ( (mod_name=check_error(PyUnicode_FromString("fractions"))) &&
         (mod=check_error(PyImport_Import(mod_name))) )
      func = check_error(PyObject_GetAttrString(mod, "Fraction"));

    Py_CLEAR(mod_name);
    Py_CLEAR(mod);
  }

  return func;
}


static int
py_unify_fraction(term_t t, PyObject *obj)
{ PyObject *str = check_error(PyObject_Str(obj));

  if ( str )
  { const char *s;
    ssize_t len;
    int rc;

    if ( (s=PyUnicode_AsUTF8AndSize(str, &len)) )
    { term_t tmp;
      int rc2=TRUE;

      char *sep = strchr(s, '/');
      if ( sep )
	*sep = 'r';
      rc = ((tmp = PL_new_term_ref()) &&
	    PL_put_term_from_chars(tmp, REP_UTF8|CVT_EXCEPTION, len, s) &&
	    (rc2=PL_is_rational(tmp)) &&
	    PL_unify(t, tmp));
      if ( sep )
	*sep = '/';

      if ( tmp )
	PL_reset_term_refs(tmp);
      if ( !rc2 )
	rc = PL_type_error("rational", tmp);

    } else
    { rc = !!check_error((void*)0);
    }

    Py_CLEAR(str);

    return rc;
  }

  return FALSE;
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
  int rc = TRUE;
  int uflags = REP_UTF8;
  static int tflags[] = {PL_ATOM, PL_STRING, PL_CODE_LIST, PL_CHAR_LIST};

  uflags |= tflags[flags&PYU_SMASK];

  s = PyUnicode_AsUTF8AndSize(obj, &len);
  if ( !check_error((void*)s) )
    return FALSE;
  PL_STRINGS_MARK();
  term_t a = 0;
  if ( (flags&PYU_CODES) )	/* codes or chars */
    rc = ((a=PL_new_term_ref()) &&
	  PL_unify_functor(t, FUNCTOR_string1) &&
	  PL_get_arg(1, t, a) &&
	  (t = a));
  rc = rc && PL_unify_chars(t, uflags, len, s);
  if ( a )
    PL_reset_term_refs(a);
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
  { PyObject *el = PySequence_GetItem(obj, i);

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

  return TRUE;
}

static int
py_unify_iter(term_t t, PyObject *obj, int flags)
{ term_t tail = PL_copy_term_ref(t);
  term_t head = PL_new_term_ref();
  PyObject *item;

  while((item=PyIter_Next(obj)))
  { int rc = ( PL_unify_list(tail, head,tail) &&
	       py_unify(head, item, flags) );
    Py_DECREF(item);
    if ( !rc )
      return FALSE;
  }
  if ( PL_exception(0) || !PL_unify_nil(tail) )
    return FALSE;

  return TRUE;
}

static int
py_unify_set(term_t t, PyObject *obj, int flags)
{ PyObject *iter = PyObject_GetIter(obj);
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
py_unify_portable_dict(term_t t, PyObject *obj, int flags)
{ PyObject *py_key, *py_value;
  Py_ssize_t i = 0;

  int rc = PyDict_Next(obj, &i, &py_key, &py_value);
  if ( rc )
  { term_t pl_kv, pl_av, tail, tmp;

    if ( !(pl_kv=PL_new_term_ref()) ||
	 !(tail=PL_new_term_ref()) ||
	 !(pl_av=PL_new_term_refs(2)) )
      return FALSE;
    tmp = pl_av+0;

    if ( !PL_unify_term(t, PL_FUNCTOR, FUNCTOR_curl1, PL_TERM, tail) )
      return FALSE;

    while( rc  )
    { if ( !PL_put_variable(pl_av+0) ||
	   !PL_put_variable(pl_av+1) ||
	   !py_unify(pl_av+0, py_key, flags) ||
	   !py_unify(pl_av+1, py_value, flags) ||
	   !PL_cons_functor_v(pl_kv, FUNCTOR_key_value2, pl_av) )
	return FALSE;		/* pl_kv is now Key:Value */

      rc = PyDict_Next(obj, &i, &py_key, &py_value);

      if ( rc )
      { if ( !PL_put_variable(tmp) ||
	     !PL_unify_term(tail, PL_FUNCTOR, FUNCTOR_comma2,
			    PL_TERM, pl_kv, PL_TERM, tmp) ||
	     !PL_put_term(tail, tmp) )
	  return FALSE;
      } else
      { return PL_unify(tail, pl_kv);
      }
    }
  } else
  { return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_py1, PL_ATOM, ATOM_curl);
  }

  assert(0);
  return FALSE;
}


#define DICT_FAST_KEYS 25

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Unify a Python dict.  If possible we do this as a SWI-Prolog dict.  If
there are  invalid keys we  call py_unify_portable_dict() to  create a
{k:v, ...} list.

(*) It  turns out that PyDict_Next()  may return more elements  in the
dict than PyDict_Size()  told us to be there.  Why?   Could it be that
the functions  we call indirectly  extend the  dict?  In any  case, we
cannot trust size and we must  extend the data structures to deal with
the additional keys.   The test case is

    ?- py_call(sys:modules, M).
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

static int
py_unify_dict(term_t t, PyObject *obj, int flags, fid_t fid)
{ if ( (flags&PYU_CURL) )
    return py_unify_portable_dict(t, obj, flags);

  Py_ssize_t size = PyDict_Size(obj);
  term_t pl_dict = PL_new_term_ref();
  term_t pl_values;
  atom_t fast[DICT_FAST_KEYS];
  atom_t *pl_keys;
  int rc = FALSE;
  Py_ssize_t i = 0;
  Py_ssize_t pli = 0;
  PyObject *py_key, *py_value;
  int invalid_keys = FALSE;

  if ( !(pl_values = PL_new_term_refs((size_t)size)) )
    return FALSE;

  if ( size <= DICT_FAST_KEYS )
  { pl_keys = fast;
  } else if ( !(pl_keys = malloc(size*sizeof(atom_t))) )
  { PL_resource_error("memory");
    goto out;
  }
  memset(pl_keys, 0, size*sizeof(atom_t));

  for( ; PyDict_Next(obj, &i, &py_key, &py_value); pli++ )
  { if ( pli == size )		/* see (*) */
    { size_t ext = size/2;
      if ( ext == 0 )
	ext = 1;

      term_t nv = PL_new_term_refs(ext); /* This should give us the next chunk */
      if ( !nv )			 /* if all py_unify() properly reset */
	goto out;
      assert(nv == pl_values+size);

      if ( size+ext > DICT_FAST_KEYS )
      { if ( pl_keys == fast )
	{ atom_t *nkeys = malloc((size+ext)*sizeof(atom_t));
	  if ( nkeys )
	  { memcpy(nkeys, pl_keys, sizeof(atom_t)*size);
	    pl_keys = nkeys;
	  } else
	  { PL_resource_error("memory");
	    goto out;
	  }
	} else
	{ atom_t *nkeys = realloc(pl_keys, (size+ext)*sizeof(atom_t));
	  if ( nkeys )
	  { pl_keys = nkeys;
	  } else
	  { PL_resource_error("memory");
	    goto out;
	  }
	}
      }
      size += ext;
    }

    if ( PyUnicode_Check(py_key) )
    { ssize_t len;
      wchar_t *s;

      s = PyUnicode_AsWideCharString(py_key, &len);
      if ( !check_error((void*)s) )
	goto out;
      pl_keys[pli] = PL_new_atom_wchars(len, s);
      PyMem_Free(s);
    } else if ( PyLong_Check(py_key) )
    { if ( !(pl_keys[pli]=_PL_cons_small_int(PyLong_AsLongLong(py_key))) )
      { invalid_keys = TRUE;
	goto out;
      }
    } else
    { invalid_keys = TRUE;
      goto out;
    }
    if ( !py_unify(pl_values+pli, py_value, flags) )
      goto out;
  }

  rc = (PL_put_dict(pl_dict, ATOM_pydict, pli, pl_keys, pl_values) &&
	PL_unify(t, pl_dict));

out:
  _PL_unregister_keys(pli, pl_keys);
  if ( pl_keys != fast )
    free(pl_keys);

  if ( invalid_keys )
  { if ( fid )
    { PL_rewind_foreign_frame(fid);
      return py_unify_portable_dict(t, obj, flags);
    } else
    { rc = PL_representation_error("py_dict_key");
    }
  }

  return rc;
}


static int
py_unify_constant(term_t t, atom_t c)
{ return PL_unify_term(t, PL_FUNCTOR, FUNCTOR_at1, PL_ATOM, c);
}


static int
py_unify(term_t t, PyObject *obj, int flags)
{ int rc = -1;
  fid_t fid;

  if ( !obj )
  { check_error(obj);
    return FALSE;
  }

  if ( obj == Py_None )
    return py_unify_constant(t, ATOM_none);
  if ( PyBool_Check(obj) )
    return py_unify_constant(t, PyLong_AsLong(obj) ? ATOM_true : ATOM_false);

  if ( (fid=PL_open_foreign_frame()) )
  { if ( (flags&PYU_OBJ) )	/* py_object(true) effective */
    { if ( PyLong_CheckExact(obj) )
	rc = py_unify_long(t, obj);
      else if ( PyFloat_CheckExact(obj) )
	rc = PL_unify_float(t, PyFloat_AsDouble(obj));
      else if ( PyUnicode_CheckExact(obj) )
	rc = py_unify_unicode(t, obj, flags);
      else if ( PyTuple_CheckExact(obj) )
	rc = py_unify_tuple(t, obj, flags);
    } else
    { if ( PyLong_Check(obj) )
	rc = py_unify_long(t, obj);
      else if ( PyFloat_Check(obj) )
	rc = PL_unify_float(t, PyFloat_AsDouble(obj));
      else if ( PyUnicode_Check(obj) )
	rc = py_unify_unicode(t, obj, flags);
      else if ( PyTuple_Check(obj) )
	rc = py_unify_tuple(t, obj, flags);
      else if ( PyDict_Check(obj) )
	rc = py_unify_dict(t, obj, flags, fid);
      else if ( PyIter_Check(obj) )
	rc = py_unify_iter(t, obj, flags);
      else if ( PySequence_Check(obj) )
	rc = py_unify_sequence(t, obj, flags);
      else if ( PySet_Check(obj) )
	rc = py_unify_set(t, obj, flags);
      else if ( !(flags&PYU_ERROR) )	/* cannot be called with error around */
      { if ( PyObject_IsInstance(obj, func_Fraction()) )
	  rc = py_unify_fraction(t, obj);
	else if ( PyEnum_Check(obj) )
	  rc = py_unify_enum(t, obj);
	else if ( py_is_record(obj) )
	  rc = py_unify_record(t, obj);
      }
    }

    /* conversion failed on Python error: retry as reference */
    if ( rc == FALSE && !PL_exception(0) )
    { PyErr_Clear();
      PL_rewind_foreign_frame(fid);
      rc = -1;
    }

    PL_close_foreign_frame(fid);

    if ( rc == -1 )
      return unify_py_obj(t, obj);

    return rc;
  } else
    return FALSE;
}


		 /*******************************
		 *       PROLOG -> PYTHON       *
		 *******************************/

static int
py_add_to_dict(term_t key, term_t value, void *closure)
{ PyObject *py_dict = closure;
  PyObject *py_value = NULL;
  PyObject *py_key = NULL;
  char *s;
  int rc;

  if ( py_from_prolog(value, &py_value) )
  { if ( PL_get_chars(key, &s, CVT_ATOM) )
    { rc = PyDict_SetItemString(py_dict, s, py_value);
    } else
    { if ( py_from_prolog(key, &py_key) )
      { rc = PyDict_SetItem(py_dict, py_key, py_value);
      } else
      { rc = 1;
	goto out;		/* not a Python error */
      }
    }

    if ( rc != 0 )
      check_error(py_value);
  } else
  { rc = 1;
  }

out:
  Py_CLEAR(py_value);
  Py_CLEAR(py_key);

  return rc;
}


static int
add_prolog_key_value_to_dict(PyObject *py_dict, term_t tuple,
			     term_t key, term_t value)
{ if ( PL_is_functor(tuple, FUNCTOR_key_value2) ) /* Key:Value */
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
fix_ration_sep(char *s)
{ char *sep;

  if ( (sep=strchr(s, 'r')) )
  { *sep = '/';
    return TRUE;
  }

  return FALSE;
}


static int
py_fraction_from_rational(term_t t, PyObject **obj)
{ int rc = FALSE;

  PL_STRINGS_MARK();
  char *s;
  PyObject *constructor;
  PyObject *argv = NULL;
  PyObject *str  = NULL;

  if ( (constructor=func_Fraction()) &&
       PL_get_chars(t, &s, CVT_RATIONAL|CVT_EXCEPTION) &&
       fix_ration_sep(s) &&
       (argv=check_error(PyTuple_New(1))) &&
       (str=check_error(PyUnicode_FromString(s))) )
  { PyObject *f = NULL;

    PyTuple_SetItem(argv, 0, str);
    f = check_error(PyObject_CallObject(constructor, argv));
    if ( f )
    { *obj = f;
      rc = TRUE;
    }
  }
  Py_CLEAR(argv);

  PL_STRINGS_RELEASE();

  return rc;
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

  if ( PL_is_rational(t) )
  { int64_t i;

    if ( !PL_is_integer(t) )
      return py_fraction_from_rational(t, obj);

    if ( PL_get_int64(t, &i) )
    { *obj = PyLong_FromLongLong(i);
      return TRUE;
    } else
    { char *s;
      int rc;
#ifdef CVT_XINTEGER		/* SWI-Prolog 9.1.16 */
#define BASE 16
#else
#define BASE 10
#define CVT_XINTEGER CVT_INTEGER
#endif

      PL_STRINGS_MARK();
      if ( (rc=PL_get_chars(t, &s, CVT_XINTEGER)) ) /* TBD: use hexadecimal exchange */
	*obj = PyLong_FromString(s, NULL, BASE);
      PL_STRINGS_RELEASE();

      return rc;
    }
#undef BASE

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

    if ( funct == FUNCTOR_string1 )
    { term_t a;
      int rc;
      char *s;

      PL_STRINGS_MARK();
      rc = ( (a=PL_new_term_ref()) &&
	     PL_get_arg(1, t, a) &&
	     PL_get_nchars(a, &len, &s,
			   REP_UTF8|CVT_ATOM|CVT_STRING|
			   CVT_LIST|CVT_EXCEPTION) );
      if ( a )
	PL_reset_term_refs(a);
      if ( rc )
	*obj = check_error(PyUnicode_FromStringAndSize(s, len));
      PL_STRINGS_RELEASE();
      return rc && *obj;
    }

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

    if ( funct == FUNCTOR_eval1 )
    { term_t call;
      PyObject *py_target = NULL;

      if ( (call = PL_new_term_ref()) &&
	   PL_get_arg(1, t, call) &&
	   unchain(call, &py_target) )
      { PyObject *py_res = py_eval(py_target, call);
	PL_reset_term_refs(call);
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
  return PL_domain_error("py_term", t),FALSE;
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
  { PyObject *r = PyLong_FromLongLong((uintptr_t)rec);
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
	   PL_recorded((record_t)(uintptr_t)v, tmp) &&
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
      PL_erase((record_t)(uintptr_t)v);
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
static int py_thread = 0;

static int
py_halt(int rc, void *ctx)
{ (void)ctx;
  py_finalize();

  return 0;
}

static void
py_thread_exitted(void *ctx)
{ (void) ctx;
  DEBUG(0, Sdprintf("Thread %d, which created Python has exitted\n",
		    PL_thread_self()));
  py_thread = 0;
}

static int
py_init(void)
{ if ( !py_initialize_done )
  { predicate_t pred = PL_predicate("py_initialize", 0, "janus");

    int rc = PL_call_predicate(NULL, PL_Q_NORMAL, pred, 0);
    if ( rc )
    { py_thread = PL_thread_self();
      PL_thread_at_exit(py_thread_exitted, NULL, FALSE);
    }
    return rc;
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
  PySys_SetArgv(argc, argv);
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
  py_yield_first();
  py_initialize_done = TRUE;
  PL_exit_hook(py_halt, NULL);	/* called just before exit */
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


/* Get the locals()  or globals().  This is pretty much  useless as is
 * as we  get the  locals and  globals of the  running method  that is
 * defined in janus.py.
 *
 * With CPython  3.11 we can  walk up the  contexts and poke  into our
 * calling  context.   Using  `O_PYFRAME`  we  get  very  partial  and
 * untested code trying  to do this.  Notably, we must  find the frame
 * that is _calling_ the Janus functions.
 */

static PyObject *
py_scope(atom_t fname)
{ PyObject *py_res;

#if O_PYFRAME
  PyFrameObject *frame = PyEval_GetFrame();
  int up = 1;

  while(up>0 && frame)
  { PyFrameObject *fr2 = PyFrame_GetBack(frame);
    if ( fr2 )
    { frame = fr2;
      up--;
    } else
      break;
  }

  if ( fname == ATOM_globals )
  { py_res = PyFrame_GetGlobals(frame);

    if ( !py_res )		/* no frame */
    { PyObject *m;

      if ( !(m=check_error(PyImport_AddModule("__main__"))) )
	return NULL;
      py_res = PyModule_GetDict(m);
    }
  } else
  { py_res = check_error(PyFrame_GetLocals(frame));
  }
#else
  if ( fname == ATOM_globals )
  { py_res = PyEval_GetGlobals();

    if ( !py_res )		/* no frame */
    { PyObject *m;

      if ( !(m=check_error(PyImport_AddModule("__main__"))) )
	return NULL;
      py_res = PyModule_GetDict(m);
    }
  } else
  { py_res = check_error(PyEval_GetLocals());
  }
#endif
  if ( py_res )
    Py_INCREF(py_res);

  return py_res;
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

    if ( obj && obj != PyEval_GetBuiltins() )
    { PL_STRINGS_MARK();
      char *fn;
      if ( PL_atom_mbchars(fname, NULL, &fn, REP_UTF8|CVT_EXCEPTION) )
	py_func = check_error(PyObject_GetAttrString(obj, fn));
      PL_STRINGS_RELEASE();
    } else
    { if ( fname == ATOM_globals || fname == ATOM_locals )
	return py_scope(fname);		/* locals() or globals() */

      py_func = builtin_function(obj, fname);
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

    PyObject *next = py_eval(*py_target, on);

    Py_XDECREF(*py_target);
    *py_target = next;
    if ( !next )
    { rc = FALSE;
      break;
    }
  }

  PL_reset_term_refs(on);

  return rc;
}


/* py_gil_ensure()  and py_gil_release()  is  our  wrapper around  the
 * Python GIL.   The first call  lazily initializes Python  if needed.
 * This  thread   than  holds   the  GIL.    This  thread   must  call
 * PyEval_SaveThread() to allow other  threads to interact with Python
 * and call PyEval_RestoreThread() when it wants to call Python again.
 * This,  while other  threads  need to  call PyGILState_Ensure()  and
 * PyGILState_Release() to allow for calling Python.
 *
 * I  am not  sure this  is  correct.  Please  comment if  there is  a
 * simpler way to achieve what we want.
 *
 * If we  have the GIL  and we  have delayed Py_DECREF()  call pending
 * from the atom garbage collector we call delayed_decref();
 */

static int
py_gil_ensure(py_gil_state *state)
{ int self = PL_thread_self();

  if ( !py_init() )
    return FALSE;

  state->use_gil = (self != py_thread);
  if ( state->use_gil )
    state->gil = PyGILState_Ensure();
  else
    py_resume(state);

  py_gil_thread = self;
#ifndef HAVE_PYGILSTATE_CHECK
  have_gil = TRUE;
#endif

  if ( delayed )
    delayed_decref(NULL);

  return TRUE;
}


static void
py_gil_release(py_gil_state state)
{
#ifndef HAVE_PYGILSTATE_CHECK
  have_gil = FALSE;
#endif
  py_gil_thread = 0;
  if ( state.use_gil )
    PyGILState_Release(state.gil);
  else
    py_yield(state);
}


static int
atom_domain_error(const char *dom, atom_t a)
{ term_t t;

  return ( (t=PL_new_term_ref()) &&
	   PL_put_atom(t, a) &&
	   PL_domain_error(dom, t) );
}


static PL_option_t pycall_options[] =
{ PL_OPTION("py_string_as", OPT_ATOM),
  PL_OPTION("py_dict_as",   OPT_ATOM),
  PL_OPTION("py_object",    OPT_BOOL),
  PL_OPTIONS_END
};


static int
get_conversion_options(term_t options, int *flags)
{ if ( options )
  { atom_t string_as = 0;
    atom_t dict_as   = 0;
    int py_object    = -1;

    if ( !PL_scan_options(options, 0, "py_call_options", pycall_options,
			  &string_as, &dict_as, &py_object) )
      return FALSE;
    if ( py_object != -1 )
    { if ( py_object )
	*flags |= PYU_OBJ;
      else
	*flags &= ~PYU_OBJ;
    }
    if ( string_as )
    { int f = *flags & ~PYU_SMASK;
      if ( string_as == ATOM_atom )
	f |= PYU_ATOM;
      else if ( string_as == ATOM_string )
	f |= PYU_STRING;
      else if ( string_as == ATOM_codes )
	f |= PYU_CODES;
      else if ( string_as == ATOM_chars )
	f |= PYU_CHARS;
      else
	return atom_domain_error("py_string_as", string_as);

      *flags = f;
    }
    if ( dict_as )
    { if ( dict_as == ATOM_dict )
	*flags &= ~PYU_CURL;
      else if ( dict_as == ATOM_curl )
	*flags |= PYU_CURL;
      else
	return atom_domain_error("py_dict_as", dict_as);
    }

  }

  return TRUE;
}

static int said_deprecated_setattr = FALSE;

static foreign_t
py_call3(term_t Call, term_t result, term_t options)
{ PyObject *py_target = NULL;
  term_t call = PL_copy_term_ref(Call);
  term_t val = 0;
  int rc = TRUE;
  py_gil_state state;
  int uflags = 0;

  if ( !get_conversion_options(options, &uflags) )
    return FALSE;

  if ( PL_is_functor(call, FUNCTOR_eq2) )
  { val = PL_new_term_ref();
    _PL_get_arg(2, call, val);
    _PL_get_arg(1, call, call);
    if ( !said_deprecated_setattr )
    { said_deprecated_setattr = TRUE;
      Sdprintf("Deprecated: instead of py_call(Obj:Attr = Value), "
	       "use py_setattr(Obj,Attr,Value)");
    }
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

static foreign_t
py_setattr(term_t On, term_t name, term_t value)
{ term_t on = PL_copy_term_ref(On);
  PyObject *py_target = NULL;
  int rc;
  py_gil_state state;

  if ( !py_gil_ensure(&state) )
    return FALSE;

  rc = unchain(on, &py_target);
  PyObject *next = py_eval(py_target, on);
  Py_XDECREF(py_target);
  py_target = next;
  rc = !!py_target;

  if ( rc )
  { char *attr;

    PL_STRINGS_MARK();
    if ( (rc=PL_get_chars(name, &attr, CVT_ATOM|REP_UTF8|CVT_EXCEPTION)) )
    { PyObject *py_val = NULL;

      if ( (rc=py_from_prolog(value, &py_val)) )
      { if ( PyObject_SetAttrString(py_target, attr, py_val) == -1 )
	  rc = !!check_error(NULL);
      }
      Py_CLEAR(py_val);
    }
    PL_STRINGS_RELEASE();
  }

  Py_CLEAR(py_target);
  py_gil_release(state);

  return rc;
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
  py_gil_state gil_state;
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
{ py_gil_state state;

  if ( !py_gil_ensure(&state) )
    return FALSE;
  int rc = PL_call(goal, NULL);
  py_gil_release(state);

  return rc;
}


static foreign_t
py_gil_owner(term_t owner)
{ if ( py_gil_thread )
    return PL_unify_thread_id(owner, py_gil_thread);

  return FALSE;
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
    py_gil_state state;
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
  py_gil_state state;

  if ( !py_gil_ensure(&state) )
    return FALSE;
  if ( (rc=py_from_prolog(t, &obj)) )
  { PyObject *s = check_error(PyObject_Str(obj));
    Py_DECREF(obj);

    if ( s )
    { rc = py_unify(str, s, PYU_STRING);
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
	return PL_existence_error("py_object", t);
      return PL_free_blob(blob);
    }
  }

  return PL_type_error("py_object", t);
}


static foreign_t
py_is_object(term_t t)
{ py_gil_state state;
  PyObject *obj = NULL;

  if ( !py_gil_ensure(&state) )
    return FALSE;

  int rc = get_py_obj(t, &obj, FALSE);
  Py_CLEAR(obj);
  py_gil_release(state);

  return rc;
}


		 /*******************************
		 *        GIL MANAGEMENT        *
		 *******************************/

typedef struct
{ PyThreadState *state;
  int		 nested;
} py_state_t;

static py_state_t py_state;

static void
py_yield_first(void)
{ py_state.state = PyEval_SaveThread();
  py_state.nested = 0;
}

static void
py_yield(py_gil_state state)
{ if ( --py_state.nested == 0 )
  { DEBUG(1, Sdprintf("Yielding ..."));
    py_state.state = PyEval_SaveThread();
    DEBUG(1, Sdprintf("ok\n"));
  } else
  { PyGILState_Release(state.gil);
  }
}

static void
py_resume(py_gil_state *state)
{ if ( py_state.state )
  { DEBUG(1, Sdprintf("Un yielding ..."));
    PyEval_RestoreThread(py_state.state);
    DEBUG(1, Sdprintf("ok\n"));
    py_state.state = NULL;
  } else
  { state->gil = PyGILState_Ensure();
  }
  py_state.nested++;
}

/* Ideally this would shut down Python such that we can restart it, but
   that requires getting rid of all Python object references.   These
   exist both at several places in static variables of this module as
   in Prolog atoms.   So, for now this is just a cleanup after the
   Prolog cleanup.
*/

static foreign_t
py_finalize(void)
{ if ( py_initialize_done && !py_finalizing )
  { py_finalizing = TRUE;
    if ( py_state.state )
    { PyEval_RestoreThread(py_state.state);
      py_state.state = NULL;
    }
    py_state.nested = 0;

    Py_CLEAR(enum_constructor);
    Py_FinalizeEx();
    py_thread = 0;

    py_initialize_done = FALSE;
    if ( py_module_table )
    { py_free_hashmap(py_module_table);
      py_module_table = NULL;
    }
    py_finalizing = FALSE;
  }

  return TRUE;
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
  MKATOM(codes);
  MKATOM(chars);
  MKATOM(dict);
  MKATOM(file);
  MKATOM(eval);
  MKATOM(single);
  MKATOM(builtins);
  MKATOM(locals);
  MKATOM(globals);
  ATOM_tuple   = PL_new_atom("-");
  ATOM_pydict  = PL_new_atom("py");
  ATOM_curl    = PL_new_atom("{}");

#ifdef PL_Q_EXCEPT_HALT
  MKFUNCTOR(unwind, 1);
  MKFUNCTOR(halt, 1);
  MKATOM(keyboard_interrupt);
#else
  ATOM_aborted = PL_new_atom("$aborted");
#endif

  MKFUNCTOR(python_error, 2);
  MKFUNCTOR(python_stack, 1);
  MKFUNCTOR(error, 2);
  MKFUNCTOR(py, 1);
  MKFUNCTOR(context, 2);
  FUNCTOR_module2    = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_eq2        = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_hash1      = PL_new_functor(PL_new_atom("#"), 1);
  FUNCTOR_comma2     = PL_new_functor(PL_new_atom(","), 2);
  FUNCTOR_curl1      = PL_new_functor(PL_new_atom("{}"), 1);
  FUNCTOR_py_set1    = PL_new_functor(PL_new_atom("py_set"), 1);
  FUNCTOR_at1        = PL_new_functor(PL_new_atom("@"), 1);
  FUNCTOR_eval1      = PL_new_functor(PL_new_atom("eval"), 1);
  FUNCTOR_string1    = PL_new_functor(PL_new_atom("string"), 1);
  FUNCTOR_key_value2 = FUNCTOR_module2;
  MKFUNCTOR(prolog, 1);

#define REGISTER(name, arity, func, flags) \
        PL_register_foreign_in_module("janus", name, arity, func, flags)
#define NDET PL_FA_NONDETERMINISTIC

  REGISTER("py_initialize_",	     3,	py_initialize_,		0);
  REGISTER("py_finalize",	     0,	py_finalize,		0);
  REGISTER("py_import_",	     2, py_import,		0);
  REGISTER("py_call",		     1,	py_call1,		0);
  REGISTER("py_call",		     2,	py_call2,		0);
  REGISTER("py_call",		     3,	py_call3,		0);
  REGISTER("py_iter",		     2,	py_iter2,		NDET);
  REGISTER("py_iter",		     3,	py_iter3,		NDET);
  REGISTER("py_setattr",	     3, py_setattr,		0);
  REGISTER("py_run",		     5,	py_run,			0);
  REGISTER("py_free",		     1,	py_free,		0);
  REGISTER("py_is_object",	     1,	py_is_object,		0);
  REGISTER("py_with_gil",	     1,	py_with_gil,		NDET);
  REGISTER("py_gil_owner",	     1,	py_gil_owner,		0);
  REGISTER("py_str",		     2,	py_str,			0);
  REGISTER("py_debug",		     1,	py_debug,		0);
  REGISTER("py_update_module_cache", 1,	py_update_module_cache,	0);

  if ( !py_module_initialize_done &&
       PyImport_AppendInittab("_swipl", PyInit__swipl) == -1 )
    Sdprintf("Failed to add module swipl to Python");
}

install_t
uninstall_janus(void)
{ if ( py_module_table )
  { py_free_hashmap(py_module_table);
    py_module_table = NULL;
  }
#if O_DEBUG
  py_finalize();
#endif
}
