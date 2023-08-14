# Part of SWI-Prolog

# Author:        Jan Wielemaker
# E-mail:        jan@swi-prolog.org
# WWW:           http://www.swi-prolog.org
# Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

  - Class janus.Query and function janus.once() that allow calling
    Prolog at a high level of abstraction.
  - Functions janus.px_cmp(), janus.qdet() and janus.comp() that
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
    import swipl	        # Loading janus into Prolog
except ModuleNotFoundError:     # Loading janus into Python
    import sys
    if ( hasattr(sys, "getdlopenflags") ):
         import os
         flags = sys.getdlopenflags()
         newflags = (flags & ~os.RTLD_LOCAL|os.RTLD_GLOBAL)
         sys.setdlopenflags(newflags)
         import janus_swi.swipl as swipl
         sys.setdlopenflags(flags)
    else:
         import janus_swi.swipl as swipl

# Undefined

Undefined = "Undefined"

################################################################
# Primary high level interface

class Query:
    """
    Class `Query` implements an _iterator_ over a Prolog goal.

    Attributes
    ----------
    query: str
        A string representing a Prolog goal.
    inputs: dict
        Bind variables of the goal on input with the converted
        Python data from this dict.
    """
    def __init__(self, query, inputs={}):
        """Create from query and inputs as janus.once()"""
        self.state = swipl.open_query(query, inputs)
    def __iter__(self):
        """Implement iter protocol"""
        return self
    def __next__(self):
        """Implement iter protocol.  Returns a dict as janus.once()"""
        rc = swipl.next_solution(self.state)
        if rc == False or rc["status"] == False:
            raise StopIteration()
        else:
            return rc
    def __del__(self):
        """Close the Prolog query"""
        swipl.close_query(self.state)
    def next(self):
        """Allow for explicit enumeration,  Returns None or a dict"""
        rc = swipl.next_solution(self.state)
        if rc == False or rc["status"] == False:
            return None
        else:
            return rc
    def close(self):
        """Explicitly close the query."""
        swipl.close_query(self.state)

def once(query, inputs={}, keep=False):
    """
    Call a Prolog predicate as `once/1`

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
    """
    return swipl.call(query, inputs, keep)

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
    once("janus:py_consult(File, Data, Module)",
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
    code.InteractiveConsole(vars).interact();

def prolog():
    """
    Start and interactive Prolog toplevel.
    """
    once("'$toplevel':setup_interactive")
    once("prolog")

################################################################
# Emulated XSB interface

def px_cmd(module, pred, *args):
    """Run module:pred(arg ...)"""
    once("janus:px_cmd(M,P,Args)", {"M":module, "P":pred, "Args":args})

def _xsb_tv(status):
    if status == True:
        return 1
    elif status == False:
        return 0
    else:
        return 2

def px_qdet(module, pred, *args):
    """Run predicate as once/1

    The predicate is called with one more argument than provided in `args`.

    Examples
    --------
    **Example 1**

    >>> px_qdet("user", "current_prolog_flag", "version")
    (90113, 1)

    **Example 2**
    >>> px_qdet("user", "current_prolog_flag", "no_such_flag")
    (None, 0)

    Returns
    -------
    result: (Return,Status)
        A tuple holding the converted value of the last argument of pred
        and the success status.  `0` means failure, `1`, success and `2`
        _undefined_ (success with delays)
    """
    d = once("janus:px_qdet(M,P,Args,Ret)", {"M":module, "P":pred, "Args":args})
    return (d["Ret"], _xsb_tv(d["status"]))

PLAIN_TRUTHVALS="plain"
DELAY_LISTS="delays"
NO_TRUTHVALS="none"

def px_comp(module, pred, *args, vars=1, set_collect=False, truth_vals=PLAIN_TRUTHVALS):
    """Call non-deterministic predicate


    Examples
    --------

    >>> px_comp("user", "between", 1, 6)
    [((1,), 1), ((2,), 1), ((3,), 1), ((4,), 1), ((5,), 1), ((6,), 1)]

    Parameters
    ----------
    vars : int=1
        Number of "output" variables that appear after `args`.
    set_collect : Bool=False
        When True, return a _set_ of answers rather than a _list_.
    truth_vals : (PLAIN_TRUTHVALS|DELAY_LISTS|NO_TRUTHVALS)=PLAIN_TRUTHVALS
        When `NO_TRUTHVALS`, answers are no tuples.  Otherwise each answer
        is a tuple `(Value,Status)`.  Using `PLAIN_TRUTHVALS`, Status is
        one of 1 or 2 (undefined).  Using `DELAY_LISTS`, the delay list
        as returned by call_delays/2 is returned.

    Returns
    -------
    answers: list
        Each answer is a either a tuple holding the converted values
        for each of the output arguments or a tuple holding this tuple
        and the truth value.

    """
    d = once("janus:px_comp(M,P,Args,Vars,Set,TV,Ret)",
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
        return once("with_output_to(string(Str),print(Msg))",
                    {"Msg":self})["Str"]
    def __repr__(self):
        """Return the output of write_canonical/1 on self"""
        return once("with_output_to(string(Str),write_canonical(Msg))",
                    {"Msg":self})["Str"]
    def __del__(self):
        """Destroy the represented term"""
        record = self._record;
        self.record = 0
        swipl.erase(record)

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
            return once("message_to_string(Msg,Str)", {"Msg":self.term})["Str"]
        else:
            return self.message
    def __repr__(self):
        """Return the output of write_canonical/1 on term"""
        if ( self.term ):
            return once("with_output_to(string(Str),write_canonical(Msg))",
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
            once("janus:py_write(Stream, String)",
                 {'Stream':self.prolog_stream, 'String':s})
    def readline(self, size=-1):
        global prompt
        return once("janus:py_readline(Stream, Size, Prompt, Line)",
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
