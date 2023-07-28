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
	    py_iter/2,			% +Call, -Return
            py_run/4,                   % +String, +Globals, +Locals, -Return
            py_free/1,			% +Obj
	    py_with_gil/1,		% :Goal
            py_str/2,                   % +Obj, -String
            py_initialize/3,            % +Program, +Argv, +Options
            py_version/0,
            py_lib_dirs/1,              % -Dirs
            add_py_lib_dir/1,           % +Dir
            add_py_lib_dir/2,           % +Dir,+Where
            py_shell/0
          ]).
:- if(current_prolog_flag(windows, true)).
% just having the Python dir in PATH seems insufficient.  Note that
% this probably does not yet deal with the requirement to have the
% same version of Python installed than was used to build janus.
add_python_dll_dir :-
    absolute_file_name(path('python3.dll'), DLL, [access(read)]),
    file_directory_name(DLL, Dir),
    win_add_dll_directory(Dir).
:- initialization(add_python_dll_dir, now).
:- endif.

:- use_foreign_library(foreign(janus), [visibility(global)]).
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

%!  py_iter(+Iterator, -Value) is nondet.
%
%   True when Value is returned by the Python Iterator. Python iterators
%   may be used to implement   non-deterministic foreign predicates. The
%   implementation uses these steps:
%
%     1. Evaluate Iterator as py_call/2 evaluates its first argument,
%        except the ``Obj:Attr = Value`` construct is not accepted.
%     2. Call ``__iter__`` on the result to get the iterator itself.
%     3. Get the ``__next__`` function of the iterator.
%     4. Loop over the return values of the _next_ function.  If
%        the Python return value unifies with Value, succeed with
%        a choicepoint.  Abort on Python or unification exceptions.
%     5. Re-satisfaction continues at (4).
%
%   The example below uses the built-in iterator range():
%
%       ?- py_iter(range(1,3), X).
%       X = 1 ;
%       X = 2.
%
%   Note that the implementation performs a   _look  ahead_, i.e., after
%   successful unification it calls `__next__()`   again. On failure the
%   Prolog predicate succeeds deterministically. On   success,  the next
%   candidate is stored.

%!  py_run(+String, +Globals, +Locals, -Result) is det.
%
%   Interface to PyRun_String(). Currently   using \const{Py_file_input}
%   as _first token_. Unclear what we can do with this.

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
%
%   Used as a callback from C for lazy initialization of Python.

py_initialize :-
    current_prolog_flag(executable, Program),
    current_prolog_flag(argv, Argv),
    py_initialize(Program, Argv, []).

%!  py_initialize(+Program, +Argv, +Options) is det.
%
%   Initialize  and configure  the  embedded Python  system.  If  this
%   predicate is  not called before any  other call to Python  such as
%   py_call/2, it is called _lazily_, passing the Prolog executable as
%   Program, the  non-Prolog arguments  as Argv  and an  empty Options
%   list.
%
%   Calling this predicate while the  Python is already initialized is
%   a no-op.   This predicate is  thread-safe, where the  first thread
%   initializes Python.
%
%   @arg Options is currently ignored.  It will be used to provide
%   additional configuration options.

py_initialize(Program, Argv, Options) :-
    (   py_initialize_(Program, Argv, Options)
    ->  absolute_file_name(library('python/janus.py'), Janus,
			   [ access(read) ]),
	file_directory_name(Janus, PythonDir),
	add_py_lib_dir(PythonDir, first)
    ;   true
    ).


%!  py_version is det.
%
%   Print version info on the available Python installation

py_version :-
    py_call(sys:version, X),
    print_message(information, py_version(X)).


		 /*******************************
		 *           CALLBACK		*
		 *******************************/

:- meta_predicate py_call_string(:, +, -).

py_call_string(M:String, Input, Dict) :-
    term_string(Goal, String, [variable_names(Map)]),
    exclude(not_in_projection(Input), Map, Map1),
    dict_create(Dict, bindings, [status:Status|Map1]),
    (   call(M:Goal)
    *-> bind_status(Status)
    ;   Status = 'False',
	maplist(bind_none, Map1)
    ).

not_in_projection(Input, Name=Value) :-
    (   get_dict(Name, Input, Value)
    ->  true
    ;   sub_atom(Name, 0, _, _, '_')
    ).

bind_none(_='None').

bind_status(Status) :-
    (   '$tbl_delay_list'([])
    ->  Status = true
    ;   Status = "Undefined"
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
%   interpreter. The interpreter first imports `janus` as below.
%
%       from janus import *
%
%   So, we can do
%
%       ?- py_shell.
%       ...
%       >>> once("writeln(X)", {"X":"Hello world"})
%       Hello world
%       {'status': 'True'}

py_shell :-
    py_run("from janus import *", _{}, _{}, _),
    py_call(janus:interact(), _).


		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile
    prolog:error_message//1,
    prolog:message//1.

prolog:error_message(python_error(Type, Value, _Stack)) -->
    { py_str(Type, PType),
      py_str(Value, PValue)
    },
    [ 'Python error ~w:'-[PType], nl,
      '  ~w'-[PValue]
    ].

prolog:message(py_version(V)) -->
    [ 'Janus embeds Python ~w'-[V] ].
