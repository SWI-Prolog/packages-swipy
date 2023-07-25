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

:- module(janus,
          [ py_call/1,                  % +Call
            py_call/2,                  % +Call, -Return
	    py_free/1,			% +Obj
	    py_with_gil/1,		% :Goal
            py_str/2,                   % +Obj, -String
            py_initialize/2,            % +Program, +Argv
            py_lib_dirs/1,              % -Dirs
            add_py_lib_dir/1,           % +Dir
            add_py_lib_dir/2,           % +Dir,+Where
            py_shell/0
          ]).
:- use_foreign_library(foreign(janus)).
:- meta_predicate py_with_gil(0).

:- public
    py_initialize/0,
    py_call_string/3.

/** <module> Call Python from Prolog

This library implements calling Python from Prolog.
*/

%!  py_call(+Call) is det.
%!  py_call(+Call, -Return) is det.
%
%   Call Python and return the result of   the called function. Call has
%   the shape `[Target:]Action1[:Action2]*`, where `Target`  is either a
%   Python module name or a Python object  reference. If `Target` is not
%   given, `Action1` is called as a builtin function. `Action` is either
%   an atom to get the denoted attribute from   the  `Target` or it is a
%   compound term where the first  argument   is  the function or method
%   name  and  the  arguments  provide  the  parameters  to  the  Python
%   function. On success, the returned Python   object  is translated to
%   Prolog.
%
%   Arguments to Python  functions  use   the  Python  conventions. Both
%   _positional_  and  _keyword_  arguments    are   supported.  Keyword
%   arguments are written as `Name = Value`   and  must appear after the
%   positional arguments.
%
%   Below are some examples.
%
%       % call a built-in
%	?- py_call(print("Hello World!\n")).
%	true.
%	% call function in a module
%	?- py_call(sys:getsizeof([1,2,3]), Size).
%	Size = 80.
%	% call function on an attribute of a module
%       ?- py_call(sys:path:append("/home/bob/janus")).
%       true
%       % get attribute from a module
%       ?- py_call(sys:path, Path)
%       Path = ["dir1", "dir2", ...]
%
%   Given a class in a file `dog.py`  such as the following example from
%   the Python documentation
%
%   ```
%   class Dog:
%       tricks = []
%
%       def __init__(self, name):
%           self.name = name
%
%       def add_trick(self, trick):
%           self.tricks.append(trick)
%   ```
%
%   We can interact with this class as   below. Now that ``$Doc`` in the
%   SWI-Prolog toplevel refers to the  last   toplevel  binding  for the
%   variable `Dog`.
%
%       ?- py_call(dog:'Dog'("Fido"), Dog).
%       Dog = <py_obj>(0x7f095c9d02e0)
%       ?- py_call($Dog:add_trick("roll_over")).
%       true.
%       ?- py_call($Dog:tricks, Tricks).
%       Tricks = ["roll_over"]
%
%   py_call/1 can also be used to set an attribute on a module or object
%   using the syntax py_call(Obj:Attr = Value). For example:
%
%       ?- py_call(dog:'Dog'("Fido"), Dog),
%          py_call(Dog:owner = "Bob"),
%          py_call(Doc:owner, Owner).
%       Dog = <py_obj>(0x7ffff7112170),
%       Owner = "Bob".

%!  py_free(+Obj) is det.
%
%   Immediately free (decrement  the  reference   count)  for  th Python
%   object Obj. Further reference to Obj  using py_call/1,2 or py_free/1
%   raises an `existence_error`. Note that by decrementing the reference
%   count, we make the  reference  invalid   from  Prolog.  This may not
%   actually delete the object because the   object  may have references
%   inside Python.
%
%   Prolog references to Python objects  are   subject  to  atom garbage
%   collection and thus normally do not need to be freed explicitly.

%!  py_with_gil(:Goal) is semidet.
%
%   Run Goal as  once(Goal)  while  holding   the  Phyton  GIL  (_Global
%   Interpreter Lock). Note that py_call/1,2 also   locks  the GIL. This
%   predicate is only required if we  wish   to  make  multiple calls to
%   Python while keeping the GIL. The GIL is a _recursive_ lock and thus
%   calling py_call/1,2 while holding the GIL does not _deadlock_.


		 /*******************************
		 *            INIT		*
		 *******************************/

%!  py_initialize is det.
%!  py_initialize(+Program, +Argv) is det.
%
%   Initialize the embedded Python system.

py_initialize :-
    current_prolog_flag(os_argv, [Program|Argv]),
    py_initialize(Program, Argv),
    absolute_file_name(library('python/janus.py'), Janus,
                       [ access(read) ]),
    file_directory_name(Janus, PythonDir),
    add_py_lib_dir(PythonDir, first).


		 /*******************************
		 *           CALLBACK		*
		 *******************************/

:- meta_predicate py_call_string(:, +, -).

py_call_string(M:String, Input, Dict) :-
    term_string(Goal, String, [variable_names(Map)]),
    exclude(not_in_projection(Input), Map, Map1),
    dict_create(Dict, bindings, Map1),
    call(M:Goal).

not_in_projection(Input, Name=Value) :-
    (   get_dict(Name, Input, Value)
    ->  true
    ;   sub_atom(Name, 0, _, _, '_')
    ).


		 /*******************************
		 *            PATHS		*
		 *******************************/

%!  py_lib_dirs(-Dirs) is det.
%
%   True when Dirs is a list of directories searched for Python modules.
%   The elements of Dirs are in Prolog canonical notation.

py_lib_dirs(Dirs) :-
    py_call(sys:path, Dirs0),
    maplist(prolog_to_os_filename, Dirs, Dirs0).

%!  add_py_lib_dir(+Dir) is det.
%!  add_py_lib_dir(+Dir, +Where) is det.
%
%   Add a directory to the Python  module   search  path.  In the second
%   form, Where is one of `first`   or `last`. add_py_lib_dir/1 adds the
%   directory as last.
%
%   Dir is in Prolog notation. The added   directory  is converted to an
%   absolute path using the OS notation.

add_py_lib_dir(Dir) :-
    add_py_lib_dir(Dir, last).

add_py_lib_dir(Dir, Where) :-
    absolute_file_name(Dir, AbsDir),
    prolog_to_os_filename(AbsDir, OSDir),
    (   Where == last
    ->  py_call(sys:path:append(OSDir), _)
    ;   Where == first
    ->  py_call(sys:path:insert(0, OSDir), _)
    ;   must_be(oneof([first,last]), Where)
    ).


		 /*******************************
		 *             SHELL		*
		 *******************************/

%!  py_shell
%
%   Start an interactive Python REPL  loop   using  the  embedded Python
%   interpreter.

py_shell :-
    py_call(janus:interact(), _).


		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile prolog:error_message//1.

prolog:error_message(python_error(Type, Value, _Stack)) -->
    { py_str(Type, PType),
      py_str(Value, PValue)
    },
    [ 'Python error ~w:'-[PType], nl,
      '  ~w'-[PValue]
    ].
