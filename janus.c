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

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <Python.h>
#include <assert.h>

static atom_t ATOM_None;
static atom_t ATOM_false;
static atom_t ATOM_true;
static atom_t ATOM_pydict;
static atom_t ATOM_;

static functor_t FUNCTOR_python_error3;
static functor_t FUNCTOR_error2;
static functor_t FUNCTOR_module2;
static functor_t FUNCTOR_eq2;
static functor_t FUNCTOR_hash1;

static int py_initialize_done = FALSE;

static PyObject *check_error(PyObject *obj);
static int py_unify(term_t t, PyObject *obj);
static int py_from_prolog(term_t t, PyObject **obj);

#define U_AS_TERM	0x1
#define U_AS_OBJECT	0x2

#include "hash.c"
#include "pymod.c"


		 /*******************************
		 *	       BLOB		*
		 *******************************/

typedef struct py_delayed
{ PyObject *obj;
  struct py_delayed *next;
} py_delayed;

static int scheduled = 0;
static py_delayed *delayed;

static int
delayed_decref(void *ptr)
{ py_delayed *d = delayed;

  scheduled = 0;

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

void
MyPy_DECREF(PyObject *o)
{ if ( PyGILState_Check() )
  { Py_DECREF(o);
  } else
  { py_delayed *d = malloc(sizeof(*d));
    py_delayed *old;

    d->obj = o;
    do
    { old = delayed;
      d->next = old;
    } while ( !__sync_bool_compare_and_swap(&delayed, old, d) );

    if ( __sync_bool_compare_and_swap(&scheduled, 0, 1) )
      Py_AddPendingCall(delayed_decref, NULL);
  }
}

		 /*******************************
		 *             BLOB             *
		 *******************************/

static int
release_python_object(atom_t symbol)
{ PyObject *obj = PL_blob_data(symbol, NULL, NULL );

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
{ PyObject *obj= PL_blob_data(symbol, NULL, NULL);
  Sfprintf(s, "<py_obj>(%p)", obj);
  return TRUE;
}

static void
acquire_python_object(atom_t symbol)
{
}

PL_blob_t PY_OBJECT = {
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
get_py_obj(term_t t, PyObject **obj, int error)
{ void *data;
  size_t size;
  PL_blob_t *type;

  if ( PL_get_blob(t, &data, &size, &type) && type == &PY_OBJECT )
  { *(void**)obj = data;
    return TRUE;
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

    if ( !py_module_table )
      py_module_table = py_new_hashmap();

    if ( (obj=py_lookup_hashmap(py_module_table, id)) )
    { *mod = obj;
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
  { PyObject *type, *value, *stack;
    term_t t  = PL_new_term_ref();
    term_t av = PL_new_term_refs(3);

    PyErr_Fetch(&type, &value, &stack);
    int rc = ( py_unify(av+0, type) &&
	       py_unify(av+1, value) &&
	       (stack ? py_unify(av+2, stack)
		      : PL_unify_atom(av+2, ATOM_None)) &&
	       PL_cons_functor_v(t, FUNCTOR_python_error3, av) &&
	       PL_put_variable(av+0) &&
	       PL_cons_functor(t, FUNCTOR_error2, t, av+0) &&
	       PL_raise_exception(t) );
    (void)rc;
    return NULL;
  } else
    return obj;
}

static int
py_unify_int(term_t t, PyObject *obj)
{ if ( !obj )
  { check_error(obj);
    return FALSE;
  }
  if ( obj == Py_None )
    return PL_unify_atom(t, ATOM_None);
  if ( PyBool_Check(obj) )
    return PL_unify_bool(t, PyLong_AsLongLong(obj));
  if ( PyLong_Check(obj) )
    return PL_unify_int64(t, PyLong_AsLongLong(obj));
  if ( PyFloat_Check(obj) )
    return PL_unify_float(t, PyFloat_AsDouble(obj));
  if ( PyUnicode_Check(obj) )
  { ssize_t len;
    wchar_t *s;
    int rc;

    s = PyUnicode_AsWideCharString(obj, &len);
    if ( !check_error((void*)s) )
      return FALSE;
    PL_STRINGS_MARK();
    rc = PL_unify_wchars(t, PL_STRING, len, s);
    PL_STRINGS_RELEASE();
    PyMem_Free(s);
    return rc;
  }
  if ( PyTuple_Check(obj) )
  { Py_ssize_t arity = PyTuple_GET_SIZE(obj);
    if ( PL_unify_functor(t, PL_new_functor(ATOM_, arity)) )
    { term_t a = PL_new_term_ref();
      for(Py_ssize_t i=0; i<arity; i++)
      { PyObject *py_a = PyTuple_GetItem(obj, i);
	_PL_get_arg(i+1, t, a);
	if ( !py_unify_int(a, py_a) )
	  return FALSE;
      }
      PL_reset_term_refs(a);
      return TRUE;
    }
    return FALSE;
  }
  if ( PySequence_Check(obj) )
  { ssize_t len = PySequence_Size(obj);
    term_t tail = PL_copy_term_ref(t);
    term_t head = PL_new_term_ref();

    for(ssize_t i=0; i<len; i++)
    { PyObject *el = check_error(PySequence_GetItem(obj, i));

      if ( !el )
	return FALSE;
      int rc = ( PL_unify_list(tail, head,tail) &&
		 py_unify_int(head, el) );
      Py_DECREF(el);
      if ( !rc )
	return FALSE;
    }
    if ( !PL_unify_nil(tail) )
      return FALSE;

    PL_reset_term_refs(tail);
    return TRUE;
  }
  if ( PyDict_Check(obj) )
  { Py_ssize_t size = PyDict_Size(obj);
    term_t pl_dict = PL_new_term_ref();
    term_t pl_values = PL_new_term_refs(size);
    atom_t fast[25];
    atom_t *pl_keys;
    int rc = FALSE;
    Py_ssize_t i = 0;
    PyObject *py_key, *py_value;

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
      if ( !py_unify(pl_values+pli, py_value) )
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

  return unify_py_obj(t, obj) ? U_AS_OBJECT : FALSE;
}

static int
py_unify(term_t t, PyObject *obj)
{ return !!py_unify_int(t, obj);
}

static int
py_unify_decref(term_t t, PyObject *obj)
{ int rc = py_unify_int(t, obj);
  if ( rc == U_AS_TERM && obj ) Py_DECREF(obj);
  return !!rc;
}

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
py_from_prolog(term_t t, PyObject **obj)
{ wchar_t *s;
  size_t len;
  atom_t a;

  if ( PL_is_integer(t) )
  { int64_t i;

    if ( PL_get_int64_ex(t, &i) )
    { *obj = PyLong_FromLongLong(i);
      return TRUE;
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
  if ( PL_get_atom(t, &a) )
  { int v = -1;

    if ( a == ATOM_false )
      v = 0;
    else if ( a == ATOM_true )
      v = 1;

    if ( v >= 0 )
    { *obj = check_error(PyBool_FromLong(v));
      return !!*obj;
    }

    if ( a == ATOM_None )
    { Py_INCREF(Py_None);
      *obj = Py_None;
      return TRUE;
    }
  }
  if ( PL_get_wchars(t, &len, &s, CVT_ATOM|CVT_STRING) )
  { *obj = PyUnicode_FromWideChar(s, len);
    return TRUE;
  }
  if ( PL_skip_list(t, 0, &len) == PL_LIST )
  { term_t tail = PL_copy_term_ref(t);
    term_t head = PL_new_term_ref();
    PyObject *list = PyList_New(len);

    for(Py_ssize_t i=0; PL_get_list(tail, head, tail); i++)
    { PyObject *el;

      if ( py_from_prolog(head, &el) )
      { Py_INCREF(el);
	PyList_SetItem(list, i, el);
      } else
	return FALSE;			/* TBD: What about the list? */
    }
    *obj = list;
    return TRUE;
  }
  if ( PL_is_dict(t) )
  { PyObject *py_dict = PyDict_New();

    if ( PL_for_dict(t, py_add_to_dict, py_dict, 0) != 0 )
      return FALSE;
    *obj = py_dict;
    return TRUE;
  }
  if ( get_py_obj(t, obj, FALSE) )
    return TRUE;
  if ( PL_is_functor(t, FUNCTOR_hash1) )
  { term_t arg = PL_new_term_ref();
    _PL_get_arg(1, t, arg);

    if ( PL_get_wchars(arg, &len, &s, CVT_ALL|CVT_WRITE_CANONICAL) )
    { PL_reset_term_refs(arg);
      *obj = PyUnicode_FromWideChar(s, len);
      return TRUE;
    }
  }
  /* ''(a,b,...) --> Python tuple  */
  if ( PL_get_name_arity(t, &a, &len) && a == ATOM_ )
  { PyObject *tp = check_error(PyTuple_New(len));

    if ( tp )
    { term_t arg = PL_new_term_ref();

      for(Py_ssize_t i=0; i<len; i++)
      { PyObject *py_arg;

	_PL_get_arg(i+1, t, arg);
	if ( !py_from_prolog(arg, &py_arg) )
	{ Py_DECREF(tp);
	  return FALSE;
	}
	Py_INCREF(py_arg);
	PyTuple_SetItem(tp, i, py_arg);
      }
      PL_reset_term_refs(arg);
      *obj = tp;
      return TRUE;
    }
  }

  return PL_domain_error("py_data", t),FALSE;
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
py_initialize(term_t prog, term_t argv)
{ wchar_t *pname;

  if ( !PL_get_wchars(prog, NULL, &pname, CVT_TEXT_EX) )
    return FALSE;
  Py_SetProgramName(pname);
  Py_Initialize();

  py_initialize_done = TRUE;

  return TRUE;
}


static PyObject *
py_eval(PyObject *obj, term_t func)
{ char *attr;
  atom_t fname;
  size_t arity;
  PyObject *py_res = NULL;

  if ( PL_get_chars(func, &attr, CVT_ATOM) )
  { return PyObject_GetAttrString(obj, attr);
  } else if ( PL_get_name_arity(func, &fname, &arity) )
  { PyObject *py_func = NULL;
    PyObject *py_argv = NULL;
    PyObject *py_kws  = NULL;

    if ( obj )
    { py_func = check_error(PyObject_GetAttrString(obj, PL_atom_chars(fname)));
    } else
    { PyObject *builtins = PyEval_GetBuiltins();
      py_func = PyDict_GetItemString(builtins , PL_atom_chars(fname));
      Py_DECREF(builtins);
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

	if ( _PyTuple_Resize(&py_argv, i) == -1 )
	{ check_error(py_argv);
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
	Py_INCREF(py_arg);
	PyTuple_SetItem(py_argv, i, py_arg);
      }
    }
    PL_reset_term_refs(arg);

    if ( !py_argv && !py_kws )
      py_res = check_error(PyObject_CallObject(py_func, py_argv));
    else
      py_res = check_error(PyObject_Call(py_func, py_argv, py_kws));
  out:
    if ( py_argv ) Py_DECREF(py_argv);
    if ( py_kws  ) Py_DECREF(py_kws);
    if ( py_func ) Py_DECREF(py_func);
    if ( obj     ) Py_DECREF(obj);		/* Py_DECREF() on obj! */

    return py_res;
  } else
  { PL_type_error("py_callable", func);
    return NULL;
  }
}


static foreign_t
py_call(term_t Call, term_t result)
{ PyObject *py_target = NULL;
  term_t call = PL_copy_term_ref(Call);
  term_t on = PL_new_term_ref();

  if ( !py_init() )
    return FALSE;

  while ( PL_is_functor(call, FUNCTOR_module2) )
  { _PL_get_arg(1, call, on);
    _PL_get_arg(2, call, call);

    if ( !py_target )
    { if ( !get_py_obj(on, &py_target, FALSE) &&
	   !get_py_module(on, &py_target) )
	return PL_type_error("py_target", on);
      Py_INCREF(py_target);
    } else
    { if ( !(py_target = py_eval(py_target, on)) )
	return FALSE;
    }
  }

  if ( !(py_target = py_eval(py_target, call)) )
    return FALSE;

  if ( result )
    return py_unify_decref(result, py_target);
  else
    return TRUE;
}

static foreign_t
py_call1(term_t Call)
{ return py_call(Call, 0);
}


static foreign_t
py_str(term_t t, term_t str)
{ PyObject *obj;
  int rc;

  if ( (rc=py_from_prolog(t, &obj)) )
  { PyObject *s = PyObject_Str(obj);

    rc = py_unify_decref(str, s);
  }

  return rc;
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
{ MKATOM(None);
  MKATOM(false);
  MKATOM(true);
  ATOM_ = PL_new_atom("");
  ATOM_pydict = PL_new_atom("py");

  MKFUNCTOR(python_error, 3);
  MKFUNCTOR(error, 2);
  FUNCTOR_module2 = PL_new_functor(PL_new_atom(":"), 2);
  FUNCTOR_eq2 = PL_new_functor(PL_new_atom("="), 2);
  FUNCTOR_hash1 = PL_new_functor(PL_new_atom("#"), 1);

  PL_register_foreign("py_initialize", 2, py_initialize, 0);
  PL_register_foreign("py_call",       2, py_call,       0);
  PL_register_foreign("py_call",       1, py_call1,      0);
  PL_register_foreign("py_str",        2, py_str,        0);

  if ( PyImport_AppendInittab("swipl", PyInit_swipl) == -1 )
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
