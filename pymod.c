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

static PyObject *PyExcProlog_store = NULL;
static PyObject *
PyExcProlog(void)
{ if ( !PyExcProlog_store )
    PyExcProlog_store = PyErr_NewException("swipl.Error", NULL, NULL);
  return PyExcProlog_store;
}

static PyObject*
swipl_error_string(term_t ex)
{ predicate_t pred = PL_predicate("message_to_string", 2, "$messages");
  fid_t fid;

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av = PL_new_term_refs(2);
    int rc = FALSE;
    PyObject *obj;

    if ( PL_put_term(av+0, ex) &&
	 PL_call_predicate(NULL, PL_Q_NORMAL|PL_Q_NODEBUG, pred, av) )
      rc = py_from_prolog(av+1, &obj);

    PL_discard_foreign_frame(fid);
    if ( rc )
      return obj;
  }

  Py_RETURN_NONE;
}

static void
Py_SetPrologError(term_t ex)
{ PyObject *msg = swipl_error_string(ex);

  PyErr_SetObject(PyExcProlog(), msg);
  Py_DECREF(msg);
}

static int
unify_input(term_t t, int arity, PyObject *args)
{ if ( arity == 1 )
    return PL_put_dict(t, ATOM_pydict, 0, NULL, 0);
  else
    return py_unify(t, PyTuple_GetItem(args, 1));
}

static PyObject *
swipl_call(PyObject *self, PyObject *args)
{ PyObject *out = NULL;
  static predicate_t pred = NULL;
  static module_t user = 0;
  fid_t fid;
  Py_ssize_t arity = PyTuple_GET_SIZE(args);

  if ( arity == 0 || arity > 2 )
  { PyErr_SetString(PyExc_TypeError, "swipl.call(query,bindings) takes 1 or 2 arguments");
    return NULL;
  }

  if ( !pred )
  { pred = PL_predicate("py_call_string", 3, "janus");
    user = PL_new_module(PL_new_atom("user"));
  }

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av = PL_new_term_refs(3);

    if ( py_unify(av+0, PyTuple_GetItem(args, 0)) &&
	 unify_input(av+1, arity, args) )
    { qid_t qid = PL_open_query(user, PL_Q_CATCH_EXCEPTION|PL_Q_EXT_STATUS, pred, av);
      int rc = PL_next_solution(qid);
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

    PL_close_foreign_frame(fid);
  }

  return out;
}

static void
tuple_set_int(int i, PyObject *tuple, int64_t val)
{ PyObject *v = PyLong_FromLongLong(val);
  Py_INCREF(v);
  PyList_SetItem(tuple, i, v);
}

static PyObject *
swipl_open_query(PyObject *self, PyObject *args)
{ PyObject *out = NULL;
  static predicate_t pred = NULL;
  fid_t fid;
  Py_ssize_t arity = PyTuple_GET_SIZE(args);

  if ( arity == 0 || arity > 2 )
  { PyErr_SetString(PyExc_TypeError, "swipl.call(query,bindings) takes 1 or 2 arguments");
    return NULL;
  }

  if ( !pred )
    pred = PL_predicate("py_call_string", 3, "janus");

  if ( (fid=PL_open_foreign_frame()) )
  { term_t av = PL_new_term_refs(3);

    if ( py_unify(av+0, PyTuple_GetItem(args, 0)) &&
	 unify_input(av+1, arity, args) )
    { qid_t qid = PL_open_query(NULL, PL_Q_CATCH_EXCEPTION|PL_Q_EXT_STATUS, pred, av);

      out = PyList_New(3);
      tuple_set_int(0, out, fid);
      tuple_set_int(1, out, (int64_t)qid);
      tuple_set_int(2, out, av);

      return out;
    }
  }

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
query_parms(PyObject *args, PyObject **tpp, fid_t *fid, qid_t *qid, term_t *av)
{ if ( PyTuple_GET_SIZE(args) != 1 )
  { PyErr_SetString(PyExc_TypeError, "Method expects a list [fid,qid,av]");
    return FALSE;
  }

  PyObject *tp = PyTuple_GetItem(args, 0);
  if ( !PyList_Check(tp) || PyList_GET_SIZE(tp) != 3 )
  { PyErr_SetString(PyExc_TypeError, "Method expects a list [fid,qid,av]");
    return FALSE;
  }

  int64_t tav[3];
  *tpp = tp;
  if ( !Py_GetInt64Arg(0, tp, &tav[0]) ||
       !Py_GetInt64Arg(1, tp, &tav[1]) ||
       !Py_GetInt64Arg(2, tp, &tav[2]) )
    return FALSE;
  *fid = tav[0];
  *qid = (qid_t)tav[1];
  *av  = tav[2];

  return TRUE;
}

static PyObject *
swipl_next_solution(PyObject *self, PyObject *args)
{ fid_t fid;
  qid_t qid;
  term_t av;
  int done = FALSE;
  PyObject *tp;

  if ( !query_parms(args, &tp, &fid, &qid, &av) )
    return NULL;
  if ( !qid )
    return PyBool_FromLong(0);

  int rc = PL_next_solution(qid);
  PyObject *out = NULL;

  switch(rc)
  { case PL_S_LAST:
      PL_cut_query(qid);
      done = TRUE;
      /*FALLTHROUGH*/
    case PL_S_TRUE:
      py_from_prolog(av+2, &out);
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
  }
  if ( done )
  { PL_close_foreign_frame(fid);
    tuple_set_int(1, tp, 0);   /* set qid to 0 */
  }

  return out;
}

static PyObject *
swipl_close_query(PyObject *self, PyObject *args)
{ fid_t fid;
  qid_t qid;
  term_t av;
  PyObject *tp;

  if ( !query_parms(args, &tp, &fid, &qid, &av) )
    return NULL;

  if ( qid )
  { PL_cut_query(qid);
    PL_close_foreign_frame(fid);
    tuple_set_int(1, tp, 0);   /* set qid to 0 */
  }

  Py_RETURN_NONE;
}


static PyMethodDef swiplMethods[] =
{ {"call", swipl_call, METH_VARARGS,
   "Execute a Prolog query."},
  {"open_query", swipl_open_query, METH_VARARGS,
   "Open a Prolog query."},
  {"next_solution", swipl_next_solution, METH_VARARGS,
   "Compute the next answer."},
  {"close_query", swipl_close_query, METH_VARARGS,
   "Close an open query."},
  {NULL, NULL, 0, NULL}        /* Sentinel */
};

static struct PyModuleDef swipl_module =
{ PyModuleDef_HEAD_INIT,
  "swipl",   /* name of module */
  NULL,      /* module documentation, may be NULL */
  -1,        /* size of per-interpreter state of the module,
		or -1 if the module keeps state in global variables. */
  swiplMethods
};

PyMODINIT_FUNC
PyInit_swipl(void)
{ return PyModule_Create(&swipl_module);
}
