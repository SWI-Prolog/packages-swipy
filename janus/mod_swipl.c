/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023-2025, SWI-Prolog Solutions b.v.
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

static PyObject *
mod_janus(void)
{ static PyObject *janus = NULL;

  if ( !janus )
  { PyObject *janus_name = NULL;

    if ( (janus_name=PyUnicode_FromString("janus_swi")) )
      janus = PyImport_Import(janus_name);

    Py_CLEAR(janus_name);
  }

  return janus;
}


static void
Py_SetPrologErrorFromObject(PyObject *obj)
{ PyObject *janus;
  PyObject *constructor = NULL;
  PyObject *argv = NULL;

  if ( (janus=mod_janus()) &&
       (constructor=PyObject_GetAttrString(janus, "PrologError")) &&
       (argv=PyTuple_New(1)) )
  { Py_INCREF(obj);
    PyTuple_SetItem(argv, 0, obj);
    PyObject *ex = PyObject_CallObject(constructor, argv);
    if ( ex )
    { PyErr_SetObject(constructor, ex);
      Py_DECREF(ex);
    }
  }

  Py_CLEAR(constructor);
  Py_CLEAR(argv);
}

#ifdef PL_Q_EXCEPT_HALT
static term_t
is_unwind_exception(term_t ex)
{ term_t a;

  if ( PL_is_functor(ex, FUNCTOR_unwind1) &&
       (a=PL_new_term_ref()) &&
       PL_get_arg(1, ex, a) )
    return a;

  return 0;
}

static bool
is_halt_exception(term_t a, int *code)
{ int i;

  if ( PL_is_functor(a, FUNCTOR_halt1) &&
       PL_get_arg(1, a, a) &&
       PL_get_integer(a, &i) )
  { *code = i;
    return true;
  }

  return false;
}
#endif

static void
Py_SetPrologError(term_t ex)
{ int code = INT_MIN;
  atom_t a;

#ifdef PL_Q_EXCEPT_HALT
  term_t exa;
  if ( (exa=is_unwind_exception(ex)) )
  { if ( PL_get_atom(exa, &a) && a == ATOM_keyboard_interrupt )
    { PyErr_SetObject(PyExc_KeyboardInterrupt, NULL);
      return;
    }
    is_halt_exception(exa, &code);
  }
#else
  if ( exit_requested != INT_MIN &&
       PL_get_atom(ex, &a) && a == ATOM_aborted )
    code = exit_requested;
#endif

  if ( code != INT_MIN )
  { PyObject *exit_code = PyLong_FromLongLong(code);
    PyErr_SetObject(PyExc_SystemExit, exit_code);
  } else
  { PyObject *obj = py_record(ex);
    Py_SetPrologErrorFromObject(obj);
    Py_CLEAR(obj);
  }
}

static void
Py_SetPrologErrorFromChars(const char *s)
{ PyObject *msg = PyUnicode_FromString(s);
  Py_SetPrologErrorFromObject(msg);
  Py_CLEAR(msg);
}


static int
unify_input(term_t t, Py_ssize_t arity, PyObject *args)
{ if ( arity == 1 )		/* no input arguments */
    return PL_put_dict(t, ATOM_pydict, 0, NULL, 0);
  else
    return py_unify(t, PyTuple_GetItem(args, 1), 0);
}

static int
keep_bindings(PyObject *args)
{ PyObject *kp;

  return ( PyTuple_GET_SIZE(args) >= 3 &&
	   (kp=PyTuple_GetItem(args, 2))&&
	   PyBool_Check(kp) &&
	   PyLong_AsLong(kp) );
}

static predicate_t pred = NULL;
static module_t user = 0;

static PyObject *
swipl_call(PyObject *self, PyObject *args)
{ PyObject *out = NULL;
  fid_t fid;
  Py_ssize_t arity = PyTuple_GET_SIZE(args);

  if ( py_finalizing )
    Py_RETURN_NONE;		/* error? */

  if ( arity == 0 || arity > 3 )
  { PyErr_SetString(PyExc_TypeError,
		    "swipl.call(query,bindings,keep) takes 1..3 arguments");
    return NULL;
  }

  if ( PL_thread_attach_engine(NULL) == -1 )
  { Py_SetPrologErrorFromChars("Cannot create thread");
    return NULL;
  }

  if ( !pred || !user )
  { pred = PL_predicate("py_call_string", 3, "janus");
    user = PL_new_module(PL_new_atom("user"));
  }

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av = PL_new_term_refs(3);

    if ( py_unify(av+0, PyTuple_GetItem(args, 0), 0) &&
	 unify_input(av+1, arity, args) )
    { qid_t qid = PL_open_query(user, PL_Q_CATCH_EXCEPTION|PL_Q_EXT_STATUS,
				pred, av);
      int rc;

      Py_BEGIN_ALLOW_THREADS
      rc = PL_next_solution(qid);
      Py_END_ALLOW_THREADS

      switch(rc)
      { case PL_S_TRUE:
	case PL_S_LAST:
	  if ( !py_from_prolog(av+2, &out) )
	  { term_t ex = PL_exception(0);

	    assert(ex);
	    ex = PL_copy_term_ref(ex);
	    PL_clear_exception();
	    Py_SetPrologError(ex);
	  }
	  break;
	case PL_S_FALSE:
	  out = PyBool_FromLong(0);
	  break;
	case PL_S_EXCEPTION:
	  Py_SetPrologError(PL_exception(qid));
	  break;
      }
      PL_cut_query(qid);
    }

    if ( keep_bindings(args) )
      PL_close_foreign_frame(fid);
    else
      PL_discard_foreign_frame(fid);
  }
  PL_thread_destroy_engine();

  return out;
}


static atom_t
py_obj_to_atom(PyObject *obj, const char *ex)
{ if ( PyUnicode_Check(obj) )
  { ssize_t len;
    wchar_t *s;
    atom_t a;

    s = PyUnicode_AsWideCharString(obj, &len);
    if ( !check_error((void*)s) )
      return 0;
    a = PL_new_atom_wchars(len, s);
    PyMem_Free(s);
    return a;
  }

  PyErr_SetString(PyExc_TypeError, ex);
  return 0;
}


static PyObject *
swipl_apply_once(PyObject *self, PyObject *args, PyObject *kwargs)
{ Py_ssize_t argc = PyTuple_GET_SIZE(args);
  PyObject *rc = NULL;
  atom_t mname=0;
  atom_t pname=0;

  if ( argc >= 2 )
  { fid_t fid;
    PyObject *on_fail = NULL;

    if ( kwargs )
    { static const char *kwds[] = {"fail", NULL};
      static PyObject *empty = NULL;

      if ( !empty && !(empty = PyTuple_New(0)) )
	return NULL;
      if ( !PyArg_ParseTupleAndKeywords(empty, kwargs, "|$O",
					(char**)kwds, &on_fail) )
	return NULL;
    }

    if ( !(mname=py_obj_to_atom(PyTuple_GetItem(args, 0),
				"module expected")) )
      goto error;
    if ( !(pname=py_obj_to_atom(PyTuple_GetItem(args, 1),
				"predicate name expected")) )
      goto error;

    if ( (fid=PL_open_foreign_frame()) )
    { term_t av;

      if ( (av=PL_new_term_refs(argc-1)) )
      { for(Py_ssize_t i=2; i < argc; i++)
	{ if ( !py_unify(av+i-2, PyTuple_GetItem(args, i), 0) )
	    goto eunify;

	}
      }
      module_t m = PL_new_module(mname);
      functor_t f = PL_new_functor(pname, argc-1);
      predicate_t pred = PL_pred(f, m);
      qid_t qid;

      if ( (qid=PL_open_query(m, PL_Q_CATCH_EXCEPTION|PL_Q_EXT_STATUS,
			      pred, av)) )
      { int r;

	Py_BEGIN_ALLOW_THREADS
	r = PL_next_solution(qid);
	Py_END_ALLOW_THREADS

	switch(r)
	{ case PL_S_TRUE:
	  case PL_S_LAST:
	    PL_cut_query(qid);
	    if ( !py_from_prolog(av+argc-2, &rc) )
	      Py_SetPrologError(PL_exception(0));
	    break;
	  case PL_S_EXCEPTION:
	    Py_SetPrologError(PL_exception(qid));
	    PL_cut_query(qid);
	    break;
	  case PL_S_FALSE:
	    PL_cut_query(qid);
	    if ( on_fail )
	    { rc = on_fail;
	      Py_INCREF(rc);
	    } else
	      Py_SetPrologErrorFromChars("apply_once(): goal failed");
	    break;
	  default:
	    assert(0);
	}
      }

    eunify:
      PL_discard_foreign_frame(fid);
    }
  } else
  { PyErr_SetString(PyExc_TypeError, "swipl.apply_once(module, predicate, [input ...]) expected");
  }

error:
  if ( mname ) PL_unregister_atom(mname);
  if ( pname ) PL_unregister_atom(pname);

  return rc;
}


static PyObject *
swipl_cmd(PyObject *self, PyObject *args)
{ Py_ssize_t argc = PyTuple_GET_SIZE(args);
  PyObject *rc = NULL;
  atom_t mname=0;
  atom_t pname=0;

  if ( argc >= 2 )
  { fid_t fid;
    size_t arity = argc-2;

    if ( !(mname=py_obj_to_atom(PyTuple_GetItem(args, 0),
				"module expected")) )
      goto error;
    if ( !(pname=py_obj_to_atom(PyTuple_GetItem(args, 1),
				"predicate name expected")) )
      goto error;

    if ( (fid=PL_open_foreign_frame()) )
    { term_t av;

      if ( (av=PL_new_term_refs(arity)) )
      { for(Py_ssize_t i=0; i < (Py_ssize_t)arity; i++)
	{ if ( !py_unify(av+i, PyTuple_GetItem(args, i+2), 0) )
	    goto eunify;

	}
      }
      module_t m = PL_new_module(mname);
      functor_t f = PL_new_functor(pname, arity);
      predicate_t pred = PL_pred(f, m);
      qid_t qid;

      if ( (qid=PL_open_query(m, PL_Q_CATCH_EXCEPTION|PL_Q_EXT_STATUS,
			      pred, av)) )
      { int r;

	Py_BEGIN_ALLOW_THREADS
	r = PL_next_solution(qid);
	Py_END_ALLOW_THREADS

	switch(r)
	{ case PL_S_TRUE:
	  case PL_S_LAST:
	    PL_cut_query(qid);
	    if ( PL_get_delay_list(0) )
	      rc = PyObject_GetAttrString(mod_janus(), "undefined");
	    else
	      rc = Py_True;
	    Py_INCREF(rc);
	    break;
	  case PL_S_EXCEPTION:
	    Py_SetPrologError(PL_exception(qid));
	    PL_cut_query(qid);
	    break;
	  case PL_S_FALSE:
	    PL_cut_query(qid);
	    rc = Py_False;
	    Py_INCREF(rc);
	    break;
	  default:
	    assert(0);
	}
      }

    eunify:
      PL_discard_foreign_frame(fid);
    }
  } else
  { PyErr_SetString(PyExc_TypeError, "swipl.cmd(module, predicate, [arg ...]) expected");
  }

error:
  if ( mname ) PL_unregister_atom(mname);
  if ( pname ) PL_unregister_atom(pname);

  return rc;
}


static void
tuple_set_int(int i, PyObject *tuple, int64_t val)
{ PyObject *v = PyLong_FromLongLong(val);
  Py_INCREF(v);
  PyList_SetItem(tuple, i, v);
}

#define STATE_LIST_LENGTH 4	/* fid,qid,av,keep */

static PyObject *
swipl_open_query(PyObject *self, PyObject *args)
{ PyObject *out = NULL;
  fid_t fid;
  Py_ssize_t arity = PyTuple_GET_SIZE(args);

  if ( arity == 0 || arity > 3 )
  { PyErr_SetString(PyExc_TypeError, "swipl.call(query,bindings,keep) takes 1..3 arguments");
    return NULL;
  }

  if ( PL_thread_attach_engine(NULL) == -1 )
  { Py_SetPrologErrorFromChars("Cannot create thread");
    return NULL;
  }

  if ( !pred || !user )
  { pred = PL_predicate("py_call_string", 3, "janus");
    user = PL_new_module(PL_new_atom("user"));
  }

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av = PL_new_term_refs(3);

    if ( py_unify(av+0, PyTuple_GetItem(args, 0), 0) &&
	 unify_input(av+1, arity, args) )
    { qid_t qid = PL_open_query(user, PL_Q_CATCH_EXCEPTION|PL_Q_EXT_STATUS, pred, av);

      out = PyList_New(STATE_LIST_LENGTH);
      tuple_set_int(0, out, fid);
      tuple_set_int(1, out, (int64_t)(uintptr_t)qid);
      tuple_set_int(2, out, av);
      tuple_set_int(3, out, keep_bindings(args));

      return out;
    }
  }

  PL_thread_destroy_engine();
  Py_SetPrologError(PL_exception(0));
  return NULL;
}


static int
Py_GetInt64Arg(int i, PyObject *tp, int64_t *vp)
{ PyObject *arg = PyList_GetItem(tp, i);
  if ( !PyLong_Check(arg) )
  { PyErr_SetString(PyExc_TypeError, "query type arg must be integer");
    return FALSE;
  }
  *vp = PyLong_AsLongLong(arg);
  return TRUE;
}

static int
query_parms(PyObject *args, PyObject **tpp, fid_t *fid, qid_t *qid, term_t *av, int *keep)
{ if ( PyTuple_GET_SIZE(args) != 1 )
  { PyErr_SetString(PyExc_TypeError, "Method expects a list [fid,qid,av,keep]");
    return FALSE;
  }

  PyObject *tp = PyTuple_GetItem(args, 0);
  if ( !PyList_Check(tp) || PyList_GET_SIZE(tp) != STATE_LIST_LENGTH )
  { PyErr_SetString(PyExc_TypeError, "Method expects a list [fid,qid,av,keep]");
    return FALSE;
  }

  int64_t tav[STATE_LIST_LENGTH];
  *tpp = tp;
  if ( !Py_GetInt64Arg(0, tp, &tav[0]) ||
       !Py_GetInt64Arg(1, tp, &tav[1]) ||
       !Py_GetInt64Arg(2, tp, &tav[2]) ||
       !Py_GetInt64Arg(3, tp, &tav[3]) )
    return FALSE;
  *fid  = (fid_t)tav[0];
  *qid  = (qid_t)(uintptr_t)tav[1];
  *av   = (term_t)tav[2];
  *keep = (int)tav[3];

  return TRUE;
}

static PyObject *
swipl_next_solution(PyObject *self, PyObject *args)
{ fid_t fid;
  qid_t qid;
  term_t av;
  int done = FALSE;
  PyObject *tp;
  int keep;
  int rc;

  if ( !query_parms(args, &tp, &fid, &qid, &av, &keep) )
    return NULL;
  if ( !qid )
    return PyBool_FromLong(0);

  Py_BEGIN_ALLOW_THREADS
  rc = PL_next_solution(qid);
  Py_END_ALLOW_THREADS
  PyObject *out = NULL;

  switch(rc)
  { case PL_S_LAST:
      PL_cut_query(qid);
      done = TRUE;
      /*FALLTHROUGH*/
    case PL_S_TRUE:
      if ( !py_from_prolog(av+2, &out) )
      { term_t ex = PL_exception(0);

	assert(ex);
	ex = PL_copy_term_ref(ex);
	PL_clear_exception();
	Py_SetPrologError(ex);
      }
      break;
    case PL_S_FALSE:
      out = PyBool_FromLong(0);
      PL_cut_query(qid);
      done = TRUE;
      break;
    case PL_S_EXCEPTION:
      Py_SetPrologError(PL_exception(qid));
      PL_cut_query(qid);
      done = TRUE;
      break;
    case PL_S_NOT_INNER:
      Py_SetPrologErrorFromChars("swipl.next_solution(): not inner query");
      return NULL;
      break;
  }
  if ( done )
  { if ( keep )
      PL_close_foreign_frame(fid);
    else
      PL_discard_foreign_frame(fid);

    tuple_set_int(1, tp, 0);   /* set qid to 0 */
    PL_thread_destroy_engine();
  }

  return out;
}

static PyObject *
swipl_close_query(PyObject *self, PyObject *args)
{ fid_t fid;
  qid_t qid;
  term_t av;
  PyObject *tp;
  int keep;

  if ( !query_parms(args, &tp, &fid, &qid, &av, &keep) )
    return NULL;

  if ( qid )
  { if ( PL_cut_query(qid) == PL_S_NOT_INNER )
    { Py_SetPrologErrorFromChars("swipl.next_solution(): not inner query");
      return NULL;
    }
    if ( keep )
      PL_close_foreign_frame(fid);
    else
      PL_discard_foreign_frame(fid);

    PL_thread_destroy_engine();
    tuple_set_int(1, tp, 0);   /* set qid to 0 */
  }

  Py_RETURN_NONE;
}


static PyObject *
swipl_engine(PyObject *self, PyObject *args)
{ int tid = PL_thread_self();

  return PyLong_FromLongLong(tid);
}

static PyObject *
swipl_attach_engine(PyObject *self, PyObject *args)
{ int tid = PL_thread_attach_engine(NULL);
  if ( tid >= 0 )
    return PyLong_FromLongLong(tid);

  Py_SetPrologErrorFromChars("Cannot create thread");
  return NULL;
}

static PyObject *
swipl_detach_engine(PyObject *self, PyObject *args)
{ if ( PL_thread_destroy_engine() )
    Py_RETURN_NONE;

  Py_SetPrologErrorFromChars("No thread to detach");
  return NULL;
}


static PyObject *
swipl_erase(PyObject *self, PyObject *args)
{ PyObject *rec = NULL;
  PyObject *rc = NULL;

  if ( PyTuple_GET_SIZE(args) != 1 )
    goto error;
  rec = PyTuple_GetItem(args, 0); /* borrowed ref */
  rc = py_free_record(rec);
  if ( rc )
    return rc;

error:
  PyErr_SetString(PyExc_TypeError, "swipl.erase(ptr) takes a record");
  return NULL;
}



#ifdef PYTHON_PACKAGE

install_t install_janus(void);

static PyObject *
swipl_initialize(PyObject *self, PyObject *args)
{ Py_ssize_t argc = PyTuple_GET_SIZE(args);
  const char* *argv = malloc((argc+1)*sizeof(*argv));

  memset(argv, 0, (argc+1)*sizeof(*argv));
  for(Py_ssize_t i=0; i<argc; i++)
  { PyObject *a = PyTuple_GetItem(args, i);
    if ( PyUnicode_Check(a) )
    { argv[i] = PyUnicode_AsUTF8AndSize(a, NULL);
    } else
    { assert(0);
    }
  }

  py_initialize_done = TRUE;
  if ( !PL_initialise((int)argc, (char**)argv) )
  { Py_SetPrologErrorFromChars("Failed to initialize SWI-Prolog");
    return NULL;
  }

  install_janus();

  fid_t fid;
  int rc = FALSE;
  predicate_t pred = PL_predicate("use_module", 1, "user");

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av;

    rc = ( (av=PL_new_term_refs(1)) &&
	    PL_unify_term(av+0,
			  PL_FUNCTOR_CHARS, "library", 1,
			    PL_CHARS, "janus") &&
	   PL_call_predicate(NULL, PL_Q_NORMAL, pred, av) );
    PL_discard_foreign_frame(fid);
  }

  if ( rc )
  { term_t t;

    if ( !( (t=PL_new_term_ref()) &&
	    PL_put_term_from_chars(t, 0, (size_t)-1,
				   "py_import('janus_swi.janus', [])") &&
	    PL_call(t, NULL) ) )
    { Py_SetPrologErrorFromChars("import janus_swi as janus");
    }
  } else
  { Py_SetPrologErrorFromChars("Failed to load library(janus) into Prolog");
    return NULL;
  }

  Py_RETURN_TRUE;
}

#endif /*PYTHON_PACKAGE*/

static PyMethodDef swiplMethods[] =
{ {"call", swipl_call, METH_VARARGS,
   "Execute a Prolog query."},
  {"cmd", swipl_cmd, METH_VARARGS,
   "Evaluate predicate as Boolean function.\n\n"
   "Synopsis: cmd(module, predicate, input ...) -> truth"
  },
  {"apply_once", (PyCFunction)swipl_apply_once, METH_VARARGS|METH_KEYWORDS,
   "Evaluate predicate as function.\n\n"
   "Synopsis: apply_once(module, predicate, input ...) -> output"
  },
  {"open_query", swipl_open_query, METH_VARARGS,
   "Open a Prolog query."},
  {"next_solution", swipl_next_solution, METH_VARARGS,
   "Compute the next answer."},
  {"close_query", swipl_close_query, METH_VARARGS,
   "Close an open query."},
  {"engine", swipl_engine, METH_VARARGS,
   "Return the engine id of the attached Prolog engine."},
  {"attach_engine", swipl_attach_engine, METH_VARARGS,
   "Attach a Prolog engine to the current thread."},
  {"detach_engine", swipl_detach_engine, METH_VARARGS,
   "Detach the engine from the current thread."},
  {"erase", swipl_erase, METH_VARARGS,
   "Erase a record."},
#ifdef PYTHON_PACKAGE
  {"initialize", swipl_initialize, METH_VARARGS,
   "Initialize SWI-Prolog."},
#endif
  {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef swipl_module =
{ PyModuleDef_HEAD_INIT,
  "_swipl",  /* name of module */
  "Internal module providing access to SWI-Prolog.",
  -1,        /* size of per-interpreter state of the module,
		or -1 if the module keeps state in global variables. */
  swiplMethods
};

PyMODINIT_FUNC
PyInit__swipl(void)
{ py_module_initialize_done = TRUE;
  return PyModule_Create(&swipl_module);
}
