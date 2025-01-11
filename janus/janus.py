# Part of SWI-Prolog

# Author:        Jan Wielemaker
# E-mail:        jan@swi-prolog.org
# WWW:           http://www.swi-prolog.org
# Copyright (c)  2023-2024, SWI-Prolog Solutions b.v.
# All rights reserved.

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:

# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in
#    the documentation and/or other materials provided with the
#    distribution.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.

"""Make Prolog available to Python

This module provides  access to Prolog from Python.  It  may be loaded
both  from Prolog  through  `library(janus)` or  Python using  `import
janus`.   The module provides three groups of support:

  - Class janus.query and function janus.query_once() that allow calling
    Prolog at a high level of abstraction.
  - Functions janus.cmp(), janus.qdet() and janus.comp() that
    provide a more low-level interface for calling Prolog that
    is compatible with the original version of Janus for XSB.
  - The classes `Term` and `PrologError` to represent arbitrary
    Prolog terms and Prolog exceptions.
"""

# Import the low-level module.  Note that if we embed Prolog into
# Python, janus is a package and the module is janus.swipl.  When
# Python is embedded into SWI-Prolog `swipl` is just a plain module.
# There must be a cleaner way ...

try:
    import _swipl	        # Loading janus into Prolog
except ModuleNotFoundError:     # Loading janus into Python
    import sys
    if ( hasattr(sys, "getdlopenflags") ):
         import os
         flags = sys.getdlopenflags()
         newflags = (flags & ~os.RTLD_LOCAL|os.RTLD_GLOBAL)
         sys.setdlopenflags(newflags)
         import janus_swi._swipl as _swipl
         sys.setdlopenflags(flags)
    else:
         import janus_swi._swipl as _swipl

if not hasattr(_swipl, 'call'):
    raise RuntimeError(f"Loaded wrong module '_swipl' from {_swipl.__file__}")

################################################################
# Versions

# 10000*major + 100*minor + patch
version_num=10502

def version_str(num=version_num):
    """
    Return Janus version as major.minor.patch
    """
    return f"{num//10000}.{num%10000//100}.{num%100}"

def version():
    """
    Print version information about Janus and the embedded SWI-Prolog system
    """
    global version
    plv = version_str(apply_once("user", "current_prolog_flag", "version"))
    print(f"Janus version {version_str()} embedding SWI-Prolog {plv}")

import enum

################################################################
# Truth representation

class Undefined:
    """
    Class `Undefined` represents undefined answers according to the
    Well Founded Semantics.  Generic undefined answers are represented
    by a unique instance of this class that is available as the property
    `janus.undefined`.

    Instances of this class are created by query_once() and query() and should
    never be created by the user.

    Parameters
    ----------
    term: Term
        Term representing the delay list or residual program.  Defaults
        to `None` for the generic undefined truth.

    """
    def __init__(self, term=None):
        "Create from a Prolog term or `None` for _generic_ undefined"
        self.term = term
    def __str__(self):
        """Either "Undefined" (generic) or __str__() of the `.term`"""
        if self.term == None:
            return "Undefined"
        else:
            return self.term.__str__()
    def __repr__(self):
        """Either "Undefined" (generic) or __repr__() of the `.term`"""
        if self.term == None:
            return "Undefined"
        else:
            return self.term.__repr__()

# Truth value constants

false = False
true = True
undefined = Undefined()

# Ask for specific representations of WFS undefined results.
# Using StrEnum would be more practical, but this is not in
# older Python versions (introduced in 3.11?)

# @enum.global_enum		(not in 3.10)
class TruthVal(enum.Enum):
    """
    Enum constants for asking for the Well Founded Semantics
    details on undefined results.   These are used by query_once()
    and query() and affect the value of the `truth` key in
    results.  Values

      - `NO_TRUTHVALS`
        Undefined results are not reported.  This is quite
        pointless in the current design and this may go.
      - `PLAIN_TRUTHVALS`
        Return undefined results as `janus.undefined`, a
        unique instance of the class `Undefined`.
      - `DELAY_LISTS`
        Return undefined results as an instance of class
        `Undefined` thats holds the delay list in Prolog
        native representation.
      - `RESIDUAL_PROGRAM`
        Return undefined results as an instance of class
        `Undefined` thats holds the _residual program_,
        i.e., a small inconsistent program that forms the
        justification of why the result is undefined.
    """
    NO_TRUTHVALS     = 0
    PLAIN_TRUTHVALS  = 1
    DELAY_LISTS      = 2
    RESIDUAL_PROGRAM = 3

# Make the enum available as  `janus.NO_TRUTHVALS`, etc.  As of Python
# 3.11 this can be done using `@enum.global_enum`

NO_TRUTHVALS     = TruthVal.NO_TRUTHVALS
PLAIN_TRUTHVALS  = TruthVal.PLAIN_TRUTHVALS
DELAY_LISTS      = TruthVal.DELAY_LISTS
RESIDUAL_PROGRAM = TruthVal.RESIDUAL_PROGRAM

################################################################
# Primary high level interface

class query:
    """
    Class `query` implements an _iterator_ over a Prolog goal.

    Attributes
    ----------
    query: str
        A string representing a Prolog goal.
    inputs: dict
        Bind variables of the goal on input with the converted
        Python data from this dict.
    truth_vals : (PLAIN_TRUTHVALS|DELAY_LISTS|RESIDUAL_PROGRAM)=PLAIN_TRUTHVALS
        How to represent Undefined.  Using `PLAIN_TRUTHVALS` undefined
        results use `janus.undefined`.  Using `DELAY_LISTS` an instance
        of `janus.Undefined` is created from the delay condition.

    """
    def __init__(self, query, inputs={}, truth_vals=TruthVal.PLAIN_TRUTHVALS):
        """Create from query and inputs as janus.query_once()"""
        inputs['truth'] = truth_vals
        self.state = _swipl.open_query(query, inputs)
    def __iter__(self):
        """Implement iter protocol"""
        return self
    def __next__(self):
        """Implement iter protocol.  Returns a dict as janus.query_once()"""
        rc = self.next()
        if rc is None:
            raise StopIteration()
        return rc
    def __enter__(self):
        """Implement context manager protocol"""
        return self
    def __exit__(self, type, value, tb):
        """Implement context manager protocol"""
        self.close()
    def __del__(self):
        """Close the Prolog query"""
        self.close()
    def next(self):
        """Allow for explicit enumeration,  Returns None or a dict"""
        rc = _swipl.next_solution(self.state)
        return None if rc == False or rc["truth"] == False else rc
    def close(self):
        """Explicitly close the query."""
        _swipl.close_query(self.state)

def query_once(query, inputs={}, keep=False, truth_vals=TruthVal.PLAIN_TRUTHVALS):
    """
    Call a Prolog predicate as `query_once/1`

    Parameters
    ----------
    query: str
        A string representing a Prolog goal.
    inputs: dict
        Bind variables of the goal on input with the converted
        Python data from this dict.
    keep: bool, optional
        It `True` (default `False`), do not _backtrack_.  May
        be used to preserve changes to global variables using
        `b_setval/2`.
    truth_vals: enum(TruthVal) = TruthVal.PLAIN_TRUTHVALS
        How to deal with _Well Founded Semantics_ undefined results.
    """
    inputs['truth'] = truth_vals
    return _swipl.call(query, inputs, keep)

class Query(query):
    """
    Deprecated.  Renamed to class `query`.
    """
    pass

def once(query, inputs={}, keep=False, truth_vals=TruthVal.PLAIN_TRUTHVALS):
    """
    Deprecated.  Renamed to query_once().
    """
    return query_once(query, inputs, keep, truth)

################################################################
# Functional style interface

# Ideally, we'd define apply_once() and pass on the arguments.  This
# however is considerably slower and in C we can detect the absence
# of `fail=`, which we seems impossible in Python.

apply_once = _swipl.apply_once
# def apply_once(module, predicate, *args, fail='error'):
#     if fail == 'error':
#         return _swipl.apply_once(module, predicate, *args)
#     else:
#         return _swipl.apply_once(module, predicate, *args, fail=fail)


class apply:
    """
    Functional style call on non-deterministic predicate

    Return an _iterator_ that returns the answers for a
    non-deterministic Prolog predicate.   The calling
    conventions are the same as `apply_once()`.

    Examples
    --------
    **Example 1**

    >>> [*apply("user", "between", 1, 6)]
    [1, 2, 3, 4, 5, 6]
    """
    def __init__(self, module, predicate, *args):
        self.state = _swipl.open_query("janus:px_call(In,M,P,Out)",
                                       { "M":module,
                                         "P":predicate,
                                         "In":args
                                        });
    def __iter__(self):
        """Implement iter protocol"""
        return self
    def __next__(self):
        """Implement iter protocol.  Returns a dict as janus.query_once()"""
        rc = _swipl.next_solution(self.state)
        if rc == False:
            raise StopIteration()
        else:
            return rc["Out"]
    def __del__(self):
        """Close the Prolog query"""
        _swipl.close_query(self.state)
    def next(self):
        """Allow for explicit enumeration,  Returns None or a dict"""
        rc = _swipl.next_solution(self.state)
        if rc == False:
            return None
        else:
            return rc["Out"]
    def close(self):
        """Explicitly close the query."""
        _swipl.close_query(self.state)



################################################################
# Misc functions

def engine():
    """Return the engine id if the attached Prolog engine"""
    return _swipl.engine()

def attach_engine():
    """Attach a Prolog engine to the current thread if needed"""
    return _swipl.attach_engine()

def detach_engine():
    """Detach the attached Prolog engine"""
    return _swipl.detach_engine()

def consult(file, data=None, module='user'):
    """
    Consult a Prolog file.

    Parameters
    ----------
    file: str
        Name of the file to consult.
    data: str=None
        If provided, do not read a file, but compile the Prolog text
        from the given string.
    module: str='user'
        Target module.  This is where the code is loaded if the file
        (or data) does not define a module or where the exports of the
        loaded module are imported into.
    """
    query_once("janus:py_consult(File, Data, Module)",
         {"File":file, "Data":data, "Module":module})

def echo(v):
    """
    Echo its argument.

    This utility may be used by py_call/3 to get a Python object
    from a translated term.

        ?- py_call(janus:echo(py{a:1, l:[1,2,3]}), Obj, [py_object]).
        Obj = <py_dict>(0x7f939bd3e800).
    """
    return v

def interact():
    """
    Used by `py_shell/0` to create an interactive Python session.
    Attempts to initialize readline to provide command line
    editing.
    """
    import code
    import sys
    vars = globals()
    vars.update(locals())
    vars.update({"janus":sys.modules[__name__]})
    try:
        import readline
        import rlcompleter
        readline.set_completer(rlcompleter.Completer(vars).complete)
        readline.parse_and_bind("tab: complete")
    except:
        pass
    try:
        code.InteractiveConsole(vars).interact()
    except SystemExit:          # quit() throws SystemExit
        print("now exiting InteractiveConsole...")

def prolog():
    """
    Start and interactive Prolog toplevel.
    """
    query_once("'$toplevel':setup_interactive")
    query_once("prolog")

import sys, importlib.util

# from https://stackoverflow.com/a/53080237/717069
def import_module_from_string(name: str, source: str):
    """
    Import module from source string.
    Example use:
    import_module_from_string("m", "f = lambda: print('hello')")
    m.f()
    """
    spec = importlib.util.spec_from_loader(name, loader=None)
    module = importlib.util.module_from_spec(spec)
    exec(source, module.__dict__)
    sys.modules[name] = module
    globals()[name] = module

################################################################
# Emulated XSB interface

cmd = _swipl.cmd

def comp(module, pred, *args, vars=1, set_collect=False, truth_vals=TruthVal.PLAIN_TRUTHVALS):
    """Call non-deterministic predicate


    Examples
    --------

    >>> comp("user", "between", 1, 6)
    [((1,), 1), ((2,), 1), ((3,), 1), ((4,), 1), ((5,), 1), ((6,), 1)]

    Parameters
    ----------
    vars : int=1
        Number of "output" variables that appear after `args`.
    set_collect : Bool=False
        When True, return a _set_ of answers rather than a _list_.
    truth_vals : (PLAIN_TRUTHVALS|DELAY_LISTS|NO_TRUTHVALS)=PLAIN_TRUTHVALS
        When `NO_TRUTHVALS`, answers are no tuples.  Otherwise each answer
        is a tuple `(Value,Truth)`.  Using `PLAIN_TRUTHVALS`, Truth is
        one of 1 or 2 (undefined).  Using `DELAY_LISTS`, the delay list
        as returned by call_delays/2 is returned.

    Returns
    -------
    answers: list
        Each answer is a either a tuple holding the converted values
        for each of the output arguments or a tuple holding this tuple
        and the truth value.

    """
    d = query_once("janus:px_comp(M,P,Args,Vars,Set,TV,Ret)",
             { "M":module,
               "P":pred,
               "Args":args,
               "Vars":vars,
               "Set":set_collect,
               "TV":truth_vals
              })
    return d["Ret"]

################################################################
# Represent Prolog data

class Term:
    """Represent any Prolog term

    Class `Term` is much like the Python object reference that we have
    in Prolog: it represents an arbitrary Prolog term we cannot represent
    in Python.  Instances are created if data is passed to Python as
    `prolog(Term)`.  Upon transforming the data back to Prolog, the
    interface recovers a copy of the original Prolog terms.

    The user should never create instances of this explicitly.
    """

    def __init__(self, record):
        """Create from a Prolog record pointer. DO NOT USE!"""
        self._record = record;
    def __str__(self):
        """Return the output of print/1 on self"""
        return query_once("with_output_to(string(Str),print(Msg))",
                    {"Msg":self})["Str"]
    def __repr__(self):
        """Return the output of write_canonical/1 on self"""
        return query_once("with_output_to(string(Str),write_canonical(Msg))",
                    {"Msg":self})["Str"]
    def __del__(self):
        """Destroy the represented term"""
        record = self._record;
        self.record = 0
        _swipl.erase(record)

class PrologError(Exception):
    """Represent a Prolog exception

    This class is used when calling Prolog from Python to represent that
    an exception occurred.  If the error comes from Prolog itself, it is
    represented as a Prolog term.  If it originates from illegal use of
    the Python interface functions before Prolog is called, it is
    represented as a string.

    Attributes
    ----------
    term: Term|None
        An instance of class `Term` that represents the exception.
    message: str|None
        Exception from a string
    """
    def __init__(self, msg):
        """Create an instance from a Term or str"""
        if ( isinstance(msg, Term) ):
            self.term = msg
            self.message = None
        else:
            self.message = msg
            self.term = None
    def __str__(self):
        """Return the output of message_to_string/2 on the term"""
        if ( self.term ):
            return query_once("message_to_string(Msg,Str)", {"Msg":self.term})["Str"]
        else:
            return self.message
    def __repr__(self):
        """Return the output of write_canonical/1 on term"""
        if ( self.term ):
            return query_once("with_output_to(string(Str),write_canonical(Msg))",
                         {"Msg":self.term})["Str"]
        else:
            return self.message


################################################################
# Rebind I/O

import io
import sys

# `prompt` is  the current prompt  that we  get using the  audit event
# `builtins.input`.  This is then written  to `stdout`.  We don't want
# that because the Prolog command  line editor writes the prompt.  So,
# the next write to `stdout` that writes the prompt is ignored.
#
# Hopefully there is a cleaner way to achieve this.

prompt=""
ignore=None

class PrologIO(io.TextIOWrapper):
    """Redefine terminal I/O

    Subclass `io.TextIOWrapper`, refining the I/O methods to call Prolog.
    `sys.stdin`, etc., are set to instances of this class such that console
    I/O of Python uses Prolog's console I/O and we can use e.g. py_shell/0
    also from `swipl-win` or other environments that redirect Prolog's
    console to something else that the POSIX file descriptors 0,1 and 2.
    """
    def __init__(self, plstream, *args, **kwargs):
        self.prolog_stream = plstream
        super().__init__:__call__(args, kwargs)
    def write(self, s):
        global ignore
        if ( ignore and ignore == s and self.prolog_stream == "user_output" ):
            ignore = None
        else:
            query_once("janus:py_write(Stream, String)",
                 {'Stream':self.prolog_stream, 'String':s})
    def readline(self, size=-1):
        global prompt
        return query_once("janus:py_readline(Stream, Size, Prompt, Line)",
                    {'Stream':self.prolog_stream, 'Size':size, 'Prompt':prompt})['Line']

def audit(event, args):
    """Intercept the current prompt"""
    if ( event == "builtins.input" ):
        global prompt
        prompt = args[0];
        global ignore
        ignore = args[0];

def connect_io(stdin=True, stdout=True, stderr=True):
    """Handle Python console I/o using Prolog's I/O primitives
    """
    if ( stdin ):
        sys.stdin  = PrologIO("user_input",  io.BytesIO(), line_buffering=True)
        sys.addaudithook(audit)
    if ( stdout ):
        sys.stdout = PrologIO("user_output", io.BytesIO(), line_buffering=True)
    if ( stderr ):
        sys.stderr = PrologIO("user_error",  io.BytesIO(), write_through=True)


################################################################
# Deal with interrupts

def heartbeat_tick():
    i = 1

def heartbeat(n=10000):
    """
    Make Prolog call Python every `N` inferences.  This may be required
    to allow Python processing signals such as a keyboard interrupt while
    Prolog is running.  As Python only processes signals in the main thread,
    it is enough to call this once from the main thread.

    A higher number keeps signals queued for longer, while a lower number
    harms the Prolog performance.  Note that _inferences_ can take really
    long of Prolog is calling foreign code.

    Parameters
    ----------
    n: int
        Number of inferences.  Defaults to 10,000.
    """
    consult("py_heartbeat", """
prolog:heartbeat :-
    py_call(janus_swi:heartbeat_tick()).
""")
    query_once("set_prolog_flag(heartbeat, N)", {"N":n})
