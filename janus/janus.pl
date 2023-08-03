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
          [ py_version/0,

            py_call/1,                  % +Call
            py_call/2,                  % +Call, -Return
            py_call/3,                  % +Call, -Return, +Options
	    py_iter/2,			% +Call, -Return
	    py_iter/3,			% +Call, -Return, +Options
            py_free/1,			% +Obj
	    py_with_gil/1,		% :Goal

            pyfunc/3,                   % +Module, +Function, -Return
            pyfunc/4,                   % +Module, +Function, +Kwargs, -Return
            pyfunc/5,                   % +Module, +Function, +Kwargs, +Options, -Return
            pydot/4,                    % +Module, +ObjRef, +MethAttr, ?Ret
            pydot/5,                    % +Module, +ObjRef, +MethAttr, +Options, -Ret
            free_python_object/1,       % +ObjRef

            values/3,                   % +Dict, +Path, ?Val
            keys/2,                     % +Dict, ?Keys
            key/2,                      % +Dict, ?Key
            items/2,                    % +Dict, ?Items

            py_shell/0,

	    py_pp/1,                    % +Term
            py_pp/2,                    % +Stream, +Term
            py_pp/3,                    % +Stream, +Term, +Options

            obj_dir/2,                  % +ObjRef,-List
            obj_dict/2,                 % obj_dict(+ObjRef, -Dict)

            py_initialize/3,            % +Program, +Argv, +Options
            py_lib_dirs/1,              % -Dirs
            add_py_lib_dir/1,           % +Dir
            add_py_lib_dir/2,           % +Dir,+Where

            op(50,  fx,  #)             % #Value
          ]).
:- use_module(library(apply_macros), []).
:- autoload(library(lists), [append/3, member/2]).
:- autoload(library(apply), [maplist/2, exclude/3, maplist/3]).
:- autoload(library(error), [must_be/2, domain_error/2]).
:- autoload(library(dicts), [dict_keys/2]).
:- autoload(library(option), [dict_options/2]).

:- if(\+current_predicate(py_call/1)).
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
:- endif.

:- meta_predicate py_with_gil(0).

:- public
    py_initialize/0,
    py_call_string/3.

/** <module> Call Python from Prolog

This library implements calling Python from Prolog.
*/

%!  py_version is det.
%
%   Print version info on the available Python installation

py_version :-
    py_call(sys:version, X),
    print_message(information, py_version(X)).


%!  py_call(+Call) is det.
%!  py_call(+Call, -Return) is det.
%!  py_call(+Call, -Return, +Options) is det.
%
%   Call Python and return the result of   the called function. Call has
%   the shape `[Target][:Action]*`, where `Target`   is  either a Python
%   module name or a Python object reference. Each `Action` is either an
%   atom to get the denoted attribute from   current `Target` or it is a
%   compound term where the first  argument   is  the function or method
%   name  and  the  arguments  provide  the  parameters  to  the  Python
%   function. On success, the returned Python   object  is translated to
%   Prolog.  `Action` without a `Target` denotes a buit-in function.
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
%   We can interact with this class as  below. Note that ``$Doc`` in the
%   SWI-Prolog toplevel refers to the  last   toplevel  binding  for the
%   variable `Dog`.
%
%       ?- py_call(dog:'Dog'("Fido"), Dog).
%       Dog = <py_Dog>(0x7f095c9d02e0).
%       ?- py_call($Dog:add_trick("roll_over")).
%       Dog = <py_Dog>(0x7f095c9d02e0).
%       ?- py_call($Dog:tricks, Tricks).
%       Dog = <py_Dog>(0x7f095c9d02e0),
%       Tricks = ["roll_over"]
%
%   py_call/1 can also be used to set an attribute on a module or object
%   using the syntax py_call(Obj:Attr = Value). For example:
%
%       ?- py_call(dog:'Dog'("Fido"), Dog),
%          py_call(Dog:owner = "Bob"),
%          py_call(Doc:owner, Owner).
%       Dog = <py_Dog>(0x7ffff7112170),
%       Owner = "Bob".
%
%   Options processed:
%
%     - py_string_as(+Type)
%       If Type is `atom` (default), translate a Python String into a
%       Prolog atom.  If Type is `string`, translate into a Prolog string.
%       Note that by using a string we can distinguish the Python ``Bool``
%       ``True`` and ``False`` as well as Python ``None`` from the
%       Python strings ``"true"``, ``"false"`` and ``"None"``.  Strings
%       are also more efficient if they are short lived.
%     - py_object(Boolean)
%       It `true` (default `false`), translate the return as a Python
%       object reference unless it is `None`, a boolean or a number.
%       If the number class is subclassed, we return the object rather
%       than the number.

%!  py_iter(+Iterator, -Value) is nondet.
%!  py_iter(+Iterator, -Value, +Options) is nondet.
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
%
%   Note that a Python _generator_  is   a  Python _iterator. Therefore,
%   given  the  Python  generator   expression    below,   we   can  use
%   py_iter(squares(1,5),X) to generate the squares on backtracking.
%
%   ```
%   def squares(start, stop):
%        for i in range(start, stop):
%            yield i * i
%   ```
%
%   @arg Options is processed as with py_call/3.
%   @bug Iterator may not depend on janus.Query()

%!  py_run(+String, +Globals, +Locals, -Result) is det.
%
%   Interface to PyRun_String(). Currently   using  ``Py_file_input`` as
%   _first token_. Unclear what we can do with this.

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
		 *         COMPATIBILIY		*
		 *******************************/

%!  pyfunc(+Module, +Function, -Return) is det.
%!  pyfunc(+Module, +Function, +Kwargs, -Return) is det.
%!  pyfunc(+Module, +Function, +Kwargs, +Prolog_Opts, -Return) is det.
%
%   XSB  compatible  wrappers  for  py_call/2.  Note  that  the  wrapper
%   supports more call patterns.
%
%   @arg Prolog_Opts is ignored. We may   add that to provide compatible
%   return data.

pyfunc(Module, Function, Return) :-
    py_call(Module:Function, Return).
pyfunc(Module, Function, Kwargs, Return) :-
    pyfunc(Module, Function, Kwargs, _Prolog_Opts, Return).

pyfunc(Module, Function, [], _Prolog_Opts, Return) =>
    py_call(Module:Function, Return).
pyfunc(Module, Function, Kwargs, _Prolog_Opts, Return) =>
    compound_name_arguments(Function, Name, PosArgs),
    append(PosArgs, Kwargs, AllArgs),
    compound_name_arguments(Final, Name, AllArgs),
    py_call(Module:Final, Return).

%!  pydot(+Module, +ObjRef, +MethAttr, -Ret) is det.
%!  pydot(+Module, +ObjRef, +MethAttr, +Prolog_Opts, -Ret) is det.
%
%   XSB compatible wrappers for py_call/2.
%
%   @arg Module is ignored (why do we need that if we have ObjRef?)
%   @arg Prolog_Opts is ignored.  See pyfunc/5.

pydot(_Module, ObjRef, MethAttr, Ret) :-
    py_call(ObjRef:MethAttr, Ret).
pydot(_Module, ObjRef, MethAttr, _Prolog_Opts, Ret) :-
    py_call(ObjRef:MethAttr, Ret).

%!  free_python_object(+ObjRef) is det.
%
%   XSB compatible wrapper for py_free/1. Note that ObjRef is subject to
%   (atom) garbage collection.  Explicitly  freeing   an  object  can be
%   desirable if it is large.

free_python_object(ObjRef) :-
    py_free(ObjRef).


		 /*******************************
		 *   PORTABLE ACCESS TO DICTS	*
		 *******************************/

%!  values(+Dict, +Path, ?Val) is semidet.
%
%   Get the value associated with Dict at  Path. Path is either a single
%   key or a list of keys.

values(Dict, Key, Val), is_dict(Dict), atom(Key) =>
    get_dict(Key, Dict, Val).
values(Dict, Keys, Val), is_dict(Dict), is_list(Keys) =>
    get_dict_path(Keys, Dict, Val).
values(py({CommaDict}), Key, Val) =>
    comma_values(CommaDict, Key, Val).
values({CommaDict}, Key, Val) =>
    comma_values(CommaDict, Key, Val).

get_dict_path([], Val, Val).
get_dict_path([H|T], Dict, Val) :-
    get_dict(H, Dict, Val0),
    get_dict_path(T, Val0, Val).

comma_values(CommaDict, Key, Val), atom(Key) =>
    comma_value(Key, CommaDict, Val).
comma_values(CommaDict, Keys, Val), is_list(Keys) =>
    comma_value_path(Keys, CommaDict, Val).

comma_value(Key, Key:Val0, Val) =>
    Val = Val0.
comma_value(Key, (_,Tail), Val) =>
    comma_value(Key, Tail, Val).

comma_value_path([], Val, Val).
comma_value_path([H|T], Dict, Val) :-
    comma_value(H, Dict, Val0),
    comma_value_path(T, Val0, Val).

%!  keys(+Dict, ?Keys) is det.
%
%   True when Keys is a list of keys that appear in Dict.

keys(Dict, Keys), is_dict(Dict) =>
    dict_keys(Dict, Keys).
keys(py({CommaDict}), Keys) =>
    comma_dict_keys(CommaDict, Keys).
keys({CommaDict}, Keys) =>
    comma_dict_keys(CommaDict, Keys).

comma_dict_keys((Key:_,T), Keys) =>
    Keys = [Key|KT],
    comma_dict_keys(T, KT).
comma_dict_keys(Key:_, Keys) =>
    Keys = [Key].

%!  key(+Dict, ?Key) is nondet.
%
%   True when Key is a key in   Dict.  Backtracking enumerates all known
%   keys.

key(Dict, Key), is_dict(Dict) =>
    dict_pairs(Dict, _Tag, Pairs),
    member(Key-_, Pairs).
key(py({CommaDict}), Keys) =>
    comma_dict_key(CommaDict, Keys).
key({CommaDict}, Keys) =>
    comma_dict_key(CommaDict, Keys).

comma_dict_key((Key:_,_), Key).
comma_dict_key((_,T), Key) :-
    comma_dict_key(T, Key).

%!  items(+Dict, ?Items) is det.
%
%   True when Items is a list of Key:Value that appear in Dict.

items(Dict, Items), is_dict(Dict) =>
    dict_pairs(Dict, _, Pairs),
    maplist(pair_item, Pairs, Items).
items(py({CommaDict}), Keys) =>
    comma_dict_items(CommaDict, Keys).
items({CommaDict}, Keys) =>
    comma_dict_items(CommaDict, Keys).

pair_item(K-V, K:V).

comma_dict_items((Key:Value,T), Keys) =>
    Keys = [Key:Value|KT],
    comma_dict_items(T, KT).
comma_dict_items(Key:Value, Keys) =>
    Keys = [Key:Value].


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
%       {'status': True}
%
%   If possible, we enable command line   editing using the GNU readline
%   library.

py_shell :-
    py_run("from janus import *", _{}, _{}, _),
    py_call(janus:interact(), _).


		 /*******************************
		 *          UTILITIES           *
		 *******************************/

%!  py_pp(+Term) is det.
%!  py_pp(+Term, +Options) is det.
%!  py_pp(+Stream, +Term, +Options) is det.
%
%   Pretty prints the Prolog translation of   a Python data structure in
%   Python syntax. This  exploits  pformat()   from  the  Python  module
%   `pprint` to do the actual  formatting.   Options  is translated into
%   keyword arguments passed to pprint.pformat().  For example:
%
%   ```
%   ?- py_pp(py{a:1, l:[1,2,3], size:1000000}, [underscore_numbers(true)]).
%   {'a': 1, 'l': [1, 2, 3], 'size': 1_000_000}
%   ```

py_pp(Term) :-
    py_pp(current_output, Term, []).

py_pp(Term, Options) :-
    py_pp(current_output, Term, Options).

py_pp(Stream, Term, Options) :-
    opts_kws(Options, Kws),
    PFormat =.. [pformat, Term|Kws],
    py_call(pprint:PFormat, String),
    write(Stream, String).

opts_kws(Options, Kws) :-
    dict_options(Dict, Options),
    dict_pairs(Dict, _, Pairs),
    maplist(pair_kws, Pairs, Kws).

pair_kws(Name-Value, Name=Value).


%!  obj_dir(+ObjRef, -List) is det.
%!  obj_dict(+ObjRef, -Dict) is det.
%
%   Examine attributes of an object. The predicate obj_dir/2 fetches the
%   names of all attributes,  while  obj_dict/2   gets  a  dict with all
%   attributes and their values.

obj_dir(ObjRef, List) :-
    py_call(ObjRef:'__dir__'(), List0),
    maplist(atom_string, List, List0).

obj_dict(ObjRef, Dict) :-
    py_call(ObjRef:'__dict__', Dict).


		 /*******************************
		 *            INIT		*
		 *******************************/

%   py_initialize is det.
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
		 *           CALLBACK		*
		 *******************************/

:- dynamic py_call_cache/7 as volatile.

:- meta_predicate py_call_string(:, +, -).

%   py_call_string(:String, +DictIn, -Dict) is nondet.
%
%   Support janus.once() and janus.Query(). Parses   String  into a goal
%   term. Next, all variables from the goal   term that appear in DictIn
%   are bound to the value from  this   dict.  Dict  is created from the
%   remaining variables, unless they  start   with  an underscore (e.g.,
%   `_Time`) and the key `status. On   success,  the Dict values contain
%   the bindings from the  answer  and   `status`  is  either  `true` or
%   `Undefined`. On failure, the Dict values are bound to `None` and the
%   `status` is `false`.
%
%   Parsing and distributing the variables over the two dicts is cached.

py_call_string(M:String, Input, Dict) :-
    py_call_cache(String, Input, M, Goal, Dict, Status, OutVars),
    !,
    (   call(M:Goal)
    *-> bind_status(Status)
    ;   Status = false,
	maplist(bind_none, OutVars)
    ).
py_call_string(M:String, Input, Dict) :-
    term_string(Goal, String, [variable_names(Map)]),
    unbind_dict(Input, VInput),
    exclude(not_in_projection(VInput), Map, OutBindings),
    dict_create(Dict, bindings, [status=Status|OutBindings]),
    maplist(arg(2), OutBindings, OutVars),
    asserta(py_call_cache(String, VInput, M, Goal, Dict, Status, OutVars)),
    VInput = Input,
    (   call(M:Goal)
    *-> bind_status(Status)
    ;   Status = false,
	maplist(bind_none, OutVars)
    ).

not_in_projection(Input, Name=Value) :-
    (   get_dict(Name, Input, Value)
    ->  true
    ;   sub_atom(Name, 0, _, _, '_')
    ).

bind_none('None').

bind_status(Status) :-
    (   '$tbl_delay_list'([])
    ->  Status = true
    ;   Status = "Undefined"
    ).

unbind_dict(Dict0, Dict) :-
    dict_pairs(Dict0, Tag, Pairs0),
    maplist(unbind, Pairs0, Pairs),
    dict_pairs(Dict, Tag, Pairs).

unbind(Name-_, Name-_).


		 /*******************************
		 *     SUPPORT PYTHON CALLS     *
		 *******************************/

:- public
       px_cmd/3,
       px_qdet/4,
       px_comp/7.

% These predicates are helpers  for the corresponding Python functions
% in janus.py.

px_cmd(M, P, Tuple) :-
    atom_string(Pred, P),
    atom_string(Module, M),
    (   compound(Tuple)
    ->  compound_name_arguments(Tuple, _, Args),
	Goal =.. [Pred|Args]
    ;   Goal = Pred
    ),
    call(Module:Goal).

px_qdet(M, P, Tuple, Ret) :-
    atom_string(Pred, P),
    atom_string(Module, M),
    (   compound(Tuple)
    ->  compound_name_arguments(Tuple, _, Args),
	append(Args, [Ret], GArgs),
	Goal =.. [Pred|GArgs],
	call(Module:Goal)
    ;   call(Module:Pred, Ret)
    ).

px_comp(M, P, Tuple, Vars, Set, TV, Ret) :-
    atom_string(Pred, P),
    atom_string(Module, M),
    length(Out, Vars),
    (   compound(Tuple)
    ->  compound_name_arguments(Tuple, _, Args),
	append(Args, Out, GArgs),
	Goal =.. [Pred|GArgs]
    ;   Goal =.. [Pred|Out]
    ),
    compound_name_arguments(OTempl0, :, Out),
    tv_goal_and_template(TV, Module:Goal, FGoal, OTempl0, OTempl),
    findall(OTempl, FGoal, Ret0),
    (   Set == true
    ->  sort(Ret0, Ret)
    ;   Ret = Ret0
    ).

tv_goal_and_template("plain",  Goal, ucall(Goal, TV), Templ, :(Templ,TV)) :- !.
tv_goal_and_template("delays", Goal, call_delays(Goal, TV), Templ, :(Templ,TV)) :- !.
tv_goal_and_template("none",   Goal, Goal, Templ, Templ) :- !.
tv_goal_and_template(Mode, _, _, _, _) :-
    domain_error("px_comp() truth_vals", Mode).

:- public ucall/2.

ucall(Goal, TV) :-
    call(Goal),
    (   '$tbl_delay_list'([])
    ->  TV = 1
    ;   TV = 2
    ).


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
