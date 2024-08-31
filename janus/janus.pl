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

:- module(janus,
          [ py_version/0,

            py_call/1,                  % +Call
            py_call/2,                  % +Call, -Return
            py_call/3,                  % +Call, -Return, +Options
	    py_iter/2,			% +Call, -Return
	    py_iter/3,			% +Call, -Return, +Options
            py_setattr/3,               % +On, +Name, +Value
            py_free/1,			% +Obj
	    py_is_object/1,		% @Term
	    py_is_dict/1,		% @Term
	    py_with_gil/1,		% :Goal
	    py_gil_owner/1,		% -ThreadID

            py_func/3,                  % +Module, +Func, -Return
            py_func/4,                  % +Module, +Func, -Return, +Options
            py_dot/3,                   % +ObjRef, +Meth, ?Ret
            py_dot/4,                   % +ObjRef, +Meth, -Ret, +Options

            values/3,                   % +Dict, +Path, ?Val
            keys/2,                     % +Dict, ?Keys
            key/2,                      % +Dict, ?Key
            items/2,                    % +Dict, ?Items

            py_shell/0,

	    py_pp/1,                    % +Term
            py_pp/2,                    % +Stream, +Term
            py_pp/3,                    % +Stream, +Term, +Options

            py_object_dir/2,            % +ObjRef, -List
            py_object_dict/2,           % +ObjRef, -Dict
            py_obj_dir/2,               % +ObjRef, -List (deprecated)
            py_obj_dict/2,              % +ObjRef, -Dict (deprecated)
            py_type/2,			% +ObjRef, -Type:atom
            py_isinstance/2,            % +ObjRef, +Type
            py_module_exists/1,         % +Module
            py_hasattr/2,               % +Module, ?Symbol

            py_import/2,                % +Spec, +Options
            py_module/2,                % +Module:atom, +Source:string

            py_initialize/3,            % +Program, +Argv, +Options
            py_lib_dirs/1,              % -Dirs
            py_add_lib_dir/1,           % +Dir
            py_add_lib_dir/2,           % +Dir,+Where

            op(200, fy, @),             % @constant
            op(50,  fx, #)              % #Value
          ]).
:- meta_predicate py_with_gil(0).

:- use_module(library(apply_macros), []).
:- autoload(library(lists), [append/3, member/2, append/2, last/2]).
:- autoload(library(apply),
            [maplist/2, exclude/3, maplist/3, convlist/3, partition/4]).
:- autoload(library(error), [must_be/2, domain_error/2]).
:- autoload(library(dicts), [dict_keys/2]).
:- autoload(library(option), [dict_options/2, select_option/4, option/2]).
:- autoload(library(prolog_code), [comma_list/2]).
:- autoload(library(readutil), [read_line_to_string/2, read_file_to_string/3]).
:- autoload(library(wfs), [call_delays/2, delays_residual_program/2]).
:- autoload(library(dcg/high_order), [sequence//2, sequence//3]).

:- if(\+current_predicate(py_call/1)).
:- if(current_prolog_flag(windows, true)).
:- use_module(library(shlib), [win_add_dll_directory/1]).

% Just having the Python dir in PATH seems insufficient. We also need to
% add the directory to the DLL search path.
add_python_dll_dir :-
    (   current_prolog_flag(msys2, true)
    ->  absolute_file_name(path('libpython3.dll'), DLL, [access(read)])
    ;   absolute_file_name(path('python3.dll'), DLL, [access(read)])
    ),
    file_directory_name(DLL, Dir),
    win_add_dll_directory(Dir).
:- initialization(add_python_dll_dir, now).
:- endif.

:- use_foreign_library(foreign(janus), [visibility(global)]).
:- endif.

:- predicate_options(py_call/3, 3,
                     [ py_object(boolean),
                       py_string_as(oneof([string,atom]))
                     ]).
:- predicate_options(py_func/4, 4,
                     [ pass_to(py_call/3, 3)
                     ]).
:- predicate_options(py_dot/5, 5,
                     [ pass_to(py_call/3, 3)
                     ]).

:- public
    py_initialize/0,
    py_call_string/3,
    py_write/2,
    py_readline/4.

:- create_prolog_flag(py_backtrace,       true, [type(boolean), keep(true)]).
:- create_prolog_flag(py_backtrace_depth, 4,    [type(integer), keep(true)]).
:- create_prolog_flag(py_argv,		  [],   [type(term), keep(true)]).

/** <module> Call Python from Prolog

This library implements calling Python  from   Prolog.  It  is available
directly from Prolog if  the  janus   package  is  bundled.  The library
provides access to an  _embedded_  Python   instance.  If  SWI-Prolog is
embedded into Python  using  the   Python  package  ``janus-swi``,  this
library is provided either from Prolog or from the Python package.

Normally,  the  Prolog  user  can  simply  start  calling  Python  using
py_call/2 or friends. In special cases it   may  be needed to initialize
Python with options using  py_initialize/3   and  optionally  the Python
search path may be extended using py_add_lib_dir/1.
*/

%!  py_version is det.
%
%   Print version  info on the  embedded Python installation  based on
%   Python `sys.version`.  If a Python _virtual environment_ (venv) is
%   active, indicate this with the location of this environment found.

py_version :-
    py_call(sys:version, PythonVersion),
    py_call(janus_swi:version_str(), JanusVersion),
    print_message(information, janus(version(JanusVersion, PythonVersion))),
    (   py_venv(VEnvDir, EnvSiteDir)
    ->  print_message(information, janus(venv(VEnvDir, EnvSiteDir)))
    ;   true
    ).


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
%
%       % call a built-in (alternative)
%	?- py_call(builtins:print("Hello World!\n")).
%	true.
%
%	% call function in a module
%	?- py_call(sys:getsizeof([1,2,3]), Size).
%	Size = 80.
%
%	% call function on an attribute of a module
%       ?- py_call(sys:path:append("/home/bob/janus")).
%       true
%
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
%
%       ?- py_call($Dog:add_trick("roll_over")).
%       Dog = <py_Dog>(0x7f095c9d02e0).
%
%       ?- py_call($Dog:tricks, Tricks).
%       Dog = <py_Dog>(0x7f095c9d02e0),
%       Tricks = ["roll_over"]
%
%   If the principal term of the   first  argument is not `Target:Func`,
%   The argument is evaluated as the initial target, i.e., it must be an
%   object reference or a module.   For example:
%
%       ?- py_call(dog:'Dog'("Fido"), Dog),
%          py_call(Dog, X).
%          Dog = X, X = <py_Dog>(0x7fa8cbd12050).
%       ?- py_call(sys, S).
%          S = <py_module>(0x7fa8cd582390).
%
%   Options processed:
%
%     - py_object(Boolean)
%       If `true` (default `false`), translate the return as a Python
%       object reference. Some objects are _always_ translated to
%       Prolog, regardless of this flag.  These are the Python constants
%       ``None``, ``True`` and ``False`` as well as instances of the
%       Python base classes `int`, `float`, `str` or `tuple`. Instances
%       of sub classes of these base classes are controlled by this
%       option.
%     - py_string_as(+Type)
%       If Type is `atom` (default), translate a Python String into a
%       Prolog atom.  If Type is `string`, translate into a Prolog string.
%	Strings are more efficient if they are short lived.
%     - py_dict_as(+Type)
%       One of `dict` (default) to map a Python dict to a SWI-Prolog
%       dict if all keys can be represented.  If `{}` or not all keys
%       can be represented, Return is unified to a term `{k:v, ...}`
%       or `py({})` if the Python dict is empty.
%
%   @compat  PIP.  The  options  `py_string_as`   and  `py_dict_as`  are
%   SWI-Prolog  specific,  where  SWI-Prolog   Janus  represents  Python
%   strings as atoms as required by  the   PIP  and it represents Python
%   dicts by default  as  SWI-Prolog   dicts.  The  predicates values/3,
%   keys/2, etc. provide portable access to the data in the dict.

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
%   Note that a Python _generator_ is   a  Python _iterator_. Therefore,
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
%   @bug Iterator may not depend on janus.query(), i.e., it is not
%   possible to iterate over a Python iterator that under the hoods
%   relies on a Prolog non-deterministic predicate.
%   @compat PIP.  The same remarks as for py_call/2 apply.

%!  py_setattr(+Target, +Name, +Value) is det.
%
%   Set a Python attribute on an object.  If   Target  is an atom, it is
%   interpreted  as  a  module.  Otherwise  it  is  normally  an  object
%   reference. py_setattr/3 allows for  _chaining_   and  behaves  as if
%   defined as
%
%       py_setattr(Target, Name, Value) :-
%           py_call(Target, Obj, [py_object(true)]),
%           py_call(setattr(Obj, Name, Value)).
%
%   @compat PIP

%!  py_run(+String, +Globals, +Locals, -Result, +Options) is det.
%
%   Interface  to  Py_CompileString()  followed   by  PyEval_EvalCode().
%   Options:
%
%       - file_name(String)
%         Errors are reported against this pseudo file name
%       - start(Token)
%         One of `eval`, `file` (default) or `single`.
%
%   @arg Globals is a dict
%   @arg Locals is a dict

%!  py_is_object(@Term) is semidet.
%
%   True when Term is a Python object reference. Fails silently if Term
%   is any other Prolog term.
%
%   @error existence_error(py_object, Term) is raised of Term is a
%   Python object, but it has been freed using py_free/1.
%
%   @compat PIP. The SWI-Prolog implementation is safe in the sense that
%   an arbitrary term cannot be confused  with   a  Python  object and a
%   reliable error is generated  if  the   references  has  been  freed.
%   Portable applications can not rely on this.

%!  py_is_dict(@Term) is semidet.
%
%   True if Term is a Prolog term that represents a Python dict.
%
%   @compat PIP. The SWI-Prolog version accepts   both a SWI-Prolog dict
%   and the `{k:v,...}`  representation.  See   `py_dict_as`  option  of
%   py_call/2.

py_is_dict(Dict), is_dict(Dict) => true.
py_is_dict(py({})) => true.
py_is_dict(py({KV})) => is_kv(KV).
py_is_dict({KV}) => is_kv(KV).

is_kv((K:V,T)) => ground(K), ground(V), is_kv(T).
is_kv(K:V) => ground(K), ground(V).


%!  py_free(+Obj) is det.
%
%   Immediately free (decrement the  reference   count)  for  the Python
%   object Obj. Further reference  to  Obj   using  e.g.,  py_call/2  or
%   py_free/1 raises an `existence_error`. Note that by decrementing the
%   reference count, we make the reference invalid from Prolog. This may
%   not  actually  delete  the  object  because   the  object  may  have
%   references inside Python.
%
%   Prolog references to Python objects  are   subject  to  atom garbage
%   collection and thus normally do not need to be freed explicitly.
%
%   @compat PIP. The SWI-Prolog  implementation   is  safe  and normally
%   reclaiming Python object can  be  left   to  the  garbage collector.
%   Portable applications may not assume   garbage  collection of Python
%   objects and must ensure to call py_free/1 exactly once on any Python
%   object reference. Not calling  py_free/1   leaks  the Python object.
%   Calling it twice may lead to undefined behavior.

%!  py_with_gil(:Goal) is semidet.
%
%   Run Goal as  once(Goal)  while  holding   the  Phyton  GIL  (_Global
%   Interpreter Lock_). Note that  all   predicates  that  interact with
%   Python lock the GIL. This predicate is   only required if we wish to
%   make multiple calls to Python while keeping   the  GIL. The GIL is a
%   _recursive_ lock and thus calling py_call/1,2  while holding the GIL
%   does not _deadlock_.

%!  py_gil_owner(-Thread) is semidet.
%
%   True when  the Python GIL is  owned by Thread.  Note  that, unless
%   Thread  is the  calling thread,  this merely  samples the  current
%   state and may thus no longer  be true when the predicate succeeds.
%   This predicate is intended to help diagnose _deadlock_ problems.
%
%   Note that  this predicate returns  the Prolog threads  that locked
%   the GIL.  It is however possible that Python releases the GIL, for
%   example if  it performs a  blocking call.  In this  scenario, some
%   other thread or no thread may hold the gil.


		 /*******************************
		 *         COMPATIBILIY		*
		 *******************************/

%!  py_func(+Module, +Function, -Return) is det.
%!  py_func(+Module, +Function, -Return, +Options) is det.
%
%   Call Python Function in  Module.   The  SWI-Prolog implementation is
%   equivalent to py_call(Module:Function, Return).   See  py_call/2 for
%   details.
%
%   @compat  PIP.  See  py_call/2  for  notes.    Note   that,  as  this
%   implementation is based on py_call/2,   Function can use _chaining_,
%   e.g., py_func(sys, path:append(dir), Return)  is   accepted  by this
%   implementation, but not portable.

py_func(Module, Function, Return) :-
    py_call(Module:Function, Return).
py_func(Module, Function, Return, Options) :-
    py_call(Module:Function, Return, Options).

%!  py_dot(+ObjRef, +MethAttr, -Ret) is det.
%!  py_dot(+ObjRef, +MethAttr, -Ret, +Options) is det.
%
%   Call a method or access  an  attribute   on  the  object ObjRef. The
%   SWI-Prolog implementation is equivalent  to py_call(ObjRef:MethAttr,
%   Return). See py_call/2 for details.
%
%   @compat PIP.  See py_func/3 for details.

py_dot(ObjRef, MethAttr, Ret) :-
    py_call(ObjRef:MethAttr, Ret).
py_dot(ObjRef, MethAttr, Ret, Options) :-
    py_call(ObjRef:MethAttr, Ret, Options).


		 /*******************************
		 *   PORTABLE ACCESS TO DICTS	*
		 *******************************/

%!  values(+Dict, +Path, ?Val) is semidet.
%
%   Get the value associated with Dict at  Path. Path is either a single
%   key or a list of keys.
%
%   @compat PIP. Note that this predicate   handle  a SWI-Prolog dict, a
%   {k:v, ...} term as well as py({k:v, ...}.

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
%
%   @compat PIP. Note that this predicate   handle  a SWI-Prolog dict, a
%   {k:v, ...} term as well as py({k:v, ...}.

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
%
%   @compat PIP. Note that this predicate   handle  a SWI-Prolog dict, a
%   {k:v, ...} term as well as py({k:v, ...}.

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
%
%   @compat PIP. Note that this predicate   handle  a SWI-Prolog dict, a
%   {k:v, ...} term as well as py({k:v, ...}.

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
%       >>> query_once("writeln(X)", {"X":"Hello world"})
%       Hello world
%       {'truth': True}
%
%   If possible, we enable command line   editing using the GNU readline
%   library.
%
%   When used in an environment  where  Prolog   does  not  use the file
%   handles 0,1,2 for  the  standard   streams,  e.g.,  in  `swipl-win`,
%   Python's I/O is rebound to use  Prolog's I/O. This includes Prolog's
%   command line editor, resulting in  a   mixed  history  of Prolog and
%   Pythin commands.

py_shell :-
    import_janus,
    py_call(janus_swi:interact(), _).

import_janus :-
    py_call(sys:hexversion, V),
    V >= 0x030A0000,                    % >= 3.10
    !,
    py_run("from janus_swi import *", py{}, py{}, _, []).
import_janus :-
    print_message(warning, janus(py_shell(no_janus))).


		 /*******************************
		 *          UTILITIES           *
		 *******************************/

%!  py_pp(+Term) is det.
%!  py_pp(+Term, +Options) is det.
%!  py_pp(+Stream, +Term, +Options) is det.
%
%   Pretty prints the Prolog translation of a Python data structure in
%   Python  syntax. This  exploits  pformat() from  the Python  module
%   `pprint` to do the actual  formatting.  Options is translated into
%   keyword arguments  passed to  pprint.pformat().  In  addition, the
%   option  nl(Bool)  is processed.   When  `true`  (default), we  use
%   pprint.pp(), which  makes the output  followed by a  newline.  For
%   example:
%
%   ```
%   ?- py_pp(py{a:1, l:[1,2,3], size:1000000},
%            [underscore_numbers(true)]).
%   {'a': 1, 'l': [1, 2, 3], 'size': 1_000_000}
%   ```
%
%   @compat PIP

py_pp(Term) :-
    py_pp(current_output, Term, []).

py_pp(Term, Options) :-
    py_pp(current_output, Term, Options).

py_pp(Stream, Term, Options) :-
    select_option(nl(NL), Options, Options1, true),
    (   NL == true
    ->  Method = pp
    ;   Method = pformat
    ),
    opts_kws(Options1, Kws),
    PFormat =.. [Method, Term|Kws],
    py_call(pprint:PFormat, String),
    write(Stream, String).

opts_kws(Options, Kws) :-
    dict_options(Dict, Options),
    dict_pairs(Dict, _, Pairs),
    maplist(pair_kws, Pairs, Kws).

pair_kws(Name-Value, Name=Value).


%!  py_object_dir(+ObjRef, -List) is det.
%!  py_object_dict(+ObjRef, -Dict) is det.
%
%   Examine attributes of  an  object.   The  predicate  py_object_dir/2
%   fetches the names of all attributes,   while  py_object_dir/2 gets a
%   dict with all attributes and their values.
%
%   @compat PIP

py_object_dir(ObjRef, List) :-
    py_call(ObjRef:'__dir__'(), List).

py_object_dict(ObjRef, Dict) :-
    py_call(ObjRef:'__dict__', Dict).

%!  py_obj_dir(+ObjRef, -List) is det.
%!  py_obj_dict(+ObjRef, -Dict) is det.
%
%   @deprecated Use py_object_dir/2 or py_object_dict/2.

py_obj_dir(ObjRef, List) :-
    py_object_dir(ObjRef, List).

py_obj_dict(ObjRef, Dict) :-
    py_object_dict(ObjRef, Dict).


%!  py_type(+ObjRef, -Type:atom) is det.
%
%   True when Type is the name of the   type of ObjRef. This is the same
%   as ``type(ObjRef).__name__`` in Python.
%
%   @compat PIP

py_type(ObjRef, Type) :-
    py_call(type(ObjRef):'__name__', Type).

%!  py_isinstance(+ObjRef, +Type) is semidet.
%
%   True if ObjRef is an instance of Type   or an instance of one of the
%   sub types of Type. This  is   the  same as ``isinstance(ObjRef)`` in
%   Python.
%
%   @arg Type is either a term `Module:Type` or a plain atom to refer to
%   a built-in type.
%
%   @compat PIP

py_isinstance(Obj, Module:Type) =>
    py_call(isinstance(Obj, eval(Module:Type)), @true).
py_isinstance(Obj, Type) =>
    py_call(isinstance(Obj, eval(sys:modules:'__getitem__'(builtins):Type)), @true).

%!  py_module_exists(+Module) is semidet.
%
%   True if Module is a currently  loaded   Python  module  or it can be
%   loaded.
%
%   @compat PIP

py_module_exists(Module) :-
    must_be(atom, Module),
    py_call(sys:modules:'__contains__'(Module), @true),
    !.
py_module_exists(Module) :-
    py_call(importlib:util:find_spec(Module), R),
    R \== @none,
    py_free(R).

%!  py_hasattr(+ModuleOrObj, ?Name) is nondet.
%
%   True when Name is an attribute of   Module. The name is derived from
%   the Python built-in hasattr(). If Name   is unbound, this enumerates
%   the members of py_object_dir/2.
%
%   @arg ModuleOrObj If this is an atom it refers to a module, otherwise
%   it must be a Python object reference.
%
%   @compat PIP

py_hasattr(ModuleOrObj, Name) :-
    var(Name),
    !,
    py_object_dir(ModuleOrObj, Names),
    member(Name, Names).
py_hasattr(ModuleOrObj, Name) :-
    must_be(atom, Name),
    (   atom(ModuleOrObj)
    ->  py_call(ModuleOrObj:'__name__'), % force loading
        py_call(hasattr(eval(sys:modules:'__getitem__'(ModuleOrObj)), Name), @true)
    ;   py_call(hasattr(ModuleOrObj, Name), @true)
    ).


%!  py_import(+Spec, +Options) is det.
%
%   Import a Python module.  Janus   imports  modules automatically when
%   referred in py_call/2 and  related   predicates.  Importing a module
%   implies  the  module  is  loaded   using  Python's  ``__import__()``
%   built-in and added to a table  that   maps  Prolog atoms to imported
%   modules. This predicate explicitly imports a module and allows it to
%   be associated with a different  name.   This  is  useful for loading
%   _nested modules_, i.e., a specific module   from a Python package as
%   well as for  avoiding  conflicts.  For   example,  with  the  Python
%   `selenium` package installed, we can do in Python:
%
%       >>> from selenium import webdriver
%       >>> browser = webdriver.Chrome()
%
%   Without this predicate, we can do
%
%       ?- py_call('selenium.webdriver':'Chrome'(), Chrome).
%
%   For a single call this is  fine,   but  for making multiple calls it
%   gets cumbersome.  With this predicate we can write this.
%
%       ?- py_import('selenium.webdriver', []).
%       ?- py_call(webdriver:'Chrome'(), Chrome).
%
%   By default, the imported module  is   associated  to an atom created
%   from the last segment of the dotted   name. Below we use an explicit
%   name.
%
%       ?- py_import('selenium.webdriver', [as(browser)]).
%       ?- py_call(browser:'Chrome'(), Chrome).
%
%   @error  permission_error(import_as,  py_module,  As)   if  there  is
%   already a module associated with As.

py_import(Spec, Options) :-
    option(as(_), Options),
    !,
    py_import_(Spec, Options).
py_import(Spec, Options) :-
    split_string(Spec, ".", "", Parts),
    last(Parts, Last),
    atom_string(As, Last),
    py_import_(Spec, [as(As)|Options]).

%!  py_module(+Module:atom, +Source:string) is det.
%
%   Load Source into the Python module Module.   This  is intended to be
%   used together with the `string` _quasi quotation_ that supports long
%   strings in SWI-Prolog.   For example:
%
%   ```
%   :- use_module(library(strings)).
%   :- py_module(hello,
%                {|string||
%                 | def say_hello_to(s):
%                 |     print(f"hello {s}")
%                 |}).
%   ```
%
%   Calling this predicate multiple  times  with   the  same  Module and
%   Source is a no-op. Called with  a   different  source  creates a new
%   Python module that replaces the old in the global namespace.
%
%   @error python_error(Type, Data) is raised if Python raises an error.

:- dynamic py_dyn_module/2 as volatile.

py_module(Module, Source) :-
    variant_sha1(Source, Hash),
    (   py_dyn_module(Module, Hash)
    ->  true
    ;   py_call(janus:import_module_from_string(Module, Source)),
        (   retract(py_dyn_module(Module, _))
        ->  py_update_module_cache(Module)
        ;   true
        ),
        asserta(py_dyn_module(Module, Hash))
    ).


		 /*******************************
		 *            INIT		*
		 *******************************/

:- dynamic py_venv/2 as volatile.
:- dynamic py_is_initialized/0 as volatile.

%   py_initialize is det.
%
%   Used as a callback from C for lazy initialization of Python.

py_initialize :-
    getenv('VIRTUAL_ENV', VEnv),
    prolog_to_os_filename(VEnvDir, VEnv),
    atom_concat(VEnvDir, '/pyvenv.cfg', Cfg),
    venv_config(Cfg, Config),
    !,
    current_prolog_flag(executable, Program),
    current_prolog_flag(py_argv, Argv),
    py_initialize(Program, ['-I'|Argv], []),
    py_setattr(sys, prefix, VEnv),
    venv_update_path(VEnvDir, Config).
py_initialize :-
    current_prolog_flag(executable, Program),
    current_prolog_flag(py_argv, Argv),
    py_initialize(Program, Argv, []).

venv_config(File, Config) :-
    access_file(File, read),
    read_file_to_string(File, String, []),
    split_string(String, "\n", "\n\r", Lines),
    convlist(venv_config_line, Lines, Config).

venv_config_line(Line, Config) :-
    sub_string(Line, B, _, A, "="),
    !,
    sub_string(Line, 0, B, _, NameS),
    split_string(NameS, "", "\t\s", [NameS2]),
    atom_string(Name, NameS2),
    sub_string(Line, _, A, 0, ValueS),
    split_string(ValueS, "", "\t\s", [ValueS2]),
    (   number_string(Value, ValueS2)
    ->  true
    ;   atom_string(Value, ValueS2)
    ),
    Config =.. [Name,Value].

venv_update_path(VEnvDir, Options) :-
    py_call(sys:version_info, Info),    % Tuple
    Info =.. [_,Major,Minor|_],
    format(string(EnvSiteDir),
           '~w/lib/python~w.~w/site-packages',
           [VEnvDir, Major, Minor]),
    prolog_to_os_filename(EnvSiteDir, PyEnvSiteDir),
    (   exists_directory(EnvSiteDir)
    ->  true
    ;   print_message(warning,
                      janus(venv(no_site_package_dir(VEnvDir, EnvSiteDir))))
    ),
    py_call(sys:path, Path0),
    (   option('include-system-site-packages'(true), Options)
    ->  partition(is_site_dir, Path0, PkgPath, SysPath),
        append([SysPath,[PyEnvSiteDir], PkgPath], Path)
    ;   exclude(is_site_dir, Path0, Path1),
        append(Path1, [PyEnvSiteDir], Path)
    ),
    py_setattr(sys, path, Path),
    print_message(silent, janus(venv(VEnvDir, EnvSiteDir))),
    asserta(py_venv(VEnvDir, EnvSiteDir)).

is_site_dir(OsDir) :-
    prolog_to_os_filename(PlDir, OsDir),
    file_base_name(PlDir, Dir0),
    downcase_atom(Dir0, Dir),
    no_env_dir(Dir).

no_env_dir('site-packages').
no_env_dir('dist-packages').

%!  py_initialize(+Program, +Argv, +Options) is det.
%
%   Initialize  and configure  the  embedded Python  system.  If  this
%   predicate is  not called before any  other call to Python  such as
%   py_call/2, it is called _lazily_, passing the Prolog executable as
%   Program, passing Argv from the  Prolog flag `py_argv` and an empty
%   Options list.
%
%   Calling this predicate while the  Python is already initialized is
%   a  no-op.  This  predicate is  thread-safe, where  the first  call
%   initializes Python.
%
%   In addition to initializing the Python system, it
%
%     - Adds the directory holding `janus.py` to the Python module
%       search path.
%     - If Prolog I/O is not connected to the file handles 0,1,2,
%       it rebinds Python I/O to use the Prolog I/O.
%
%   @arg Options is currently ignored.  It will be used to provide
%   additional configuration options.

py_initialize(Program, Argv, Options) :-
    (   py_initialize_(Program, Argv, Options)
    ->  absolute_file_name(library('python/janus.py'), Janus,
			   [ access(read) ]),
	file_directory_name(Janus, PythonDir),
	py_add_lib_dir(PythonDir, first),
	py_connect_io,
        repl_add_cwd,
        asserta(py_is_initialized)
    ;   true
    ).

%!  py_connect_io is det.
%
%   If SWI-Prolog console streams are bound to something non-standard,
%   bind the Python console I/O to our streans.

py_connect_io :-
    maplist(non_file_stream,
	    [0-user_input, 1-user_output, 2-user_error],
	    NonFiles),
    Call =.. [connect_io|NonFiles],
    py_call(janus_swi:Call).

non_file_stream(Expect-Stream, Bool) :-
    (   stream_property(Stream, file_no(Expect))
    ->  Bool = @false
    ;   Bool = @true
    ).

		 /*******************************
		 *            PATHS		*
		 *******************************/

%!  py_lib_dirs(-Dirs) is det.
%
%   True when Dirs is a list of directories searched for Python modules.
%   The elements of Dirs are in Prolog canonical notation.
%
%   @compat PIP

py_lib_dirs(Dirs) :-
    py_call(sys:path, Dirs0),
    maplist(prolog_to_os_filename, Dirs, Dirs0).

%!  py_add_lib_dir(+Dir) is det.
%!  py_add_lib_dir(+Dir, +Where) is det.
%
%   Add a directory to the Python  module   search  path.  In the second
%   form, Where is one of `first`   or `last`. py_add_lib_dir/1 adds the
%   directory as `last`. The property `sys:path`   is not modified if it
%   already contains Dir.
%
%   Dir is in Prolog notation. The added   directory  is converted to an
%   absolute path using the OS notation using prolog_to_os_filename/2.
%
%   If Dir is a _relative_ path, it   is taken relative to Prolog source
%   file when used as a _directive_ and  relative to the process working
%   directory when called as a predicate.
%
%   @compat PIP. Note  that  SWI-Prolog   uses  POSIX  file  conventions
%   internally, mapping to OS  conventions   inside  the predicates that
%   deal with files or explicitly   using prolog_to_os_filename/2. Other
%   systems may use the native file conventions in Prolog.

:- multifile system:term_expansion/2.

system:term_expansion((:- py_add_lib_dir(Dir0)), Directive) :-
    system:term_expansion((:- py_add_lib_dir(Dir0, last)), Directive).
system:term_expansion((:- py_add_lib_dir(Dir0, Where)),
                      (:- initialization(py_add_lib_dir(Dir, Where), now))) :-
    \+ (atomic(Dir0), is_absolute_file_name(Dir0)),
    prolog_load_context(directory, CWD),
    absolute_file_name(Dir0, Dir,
                       [ relative_to(CWD),
                         file_type(directory),
                         access(read)
                       ]).

py_add_lib_dir(Dir) :-
    py_add_lib_dir(Dir, last).

py_add_lib_dir(Dir, Where) :-
    atomic(Dir),
    !,
    absolute_file_name(Dir, AbsDir),
    prolog_to_os_filename(AbsDir, OSDir),
    py_add_lib_dir_(OSDir, Where).
py_add_lib_dir(Alias, Where) :-
    absolute_file_name(Alias, AbsDir,
                       [ file_type(directory),
                         access(read)
                       ]),
    prolog_to_os_filename(AbsDir, OSDir),
    py_add_lib_dir_(OSDir, Where).

py_add_lib_dir_(OSDir, Where) :-
    (   py_call(sys:path, Dirs0),
        memberchk(OSDir, Dirs0)
    ->  true
    ;   Where == last
    ->  py_call(sys:path:append(OSDir), _)
    ;   Where == first
    ->  py_call(sys:path:insert(0, OSDir), _)
    ;   must_be(oneof([first,last]), Where)
    ).

:- det(repl_add_cwd/0).
repl_add_cwd :-
    current_prolog_flag(break_level, Level),
    Level >= 0,
    !,
    (   py_call(sys:path:count(''), N),
        N > 0
    ->  true
    ;   print_message(informational, janus(add_cwd)),
        py_add_lib_dir_('', first)
    ).
repl_add_cwd.

:- multifile
    prolog:repl_loop_hook/2.

prolog:repl_loop_hook(begin, Level) :-
    Level >= 0,
    py_is_initialized,
    repl_add_cwd.


		 /*******************************
		 *           CALLBACK		*
		 *******************************/

:- dynamic py_call_cache/8 as volatile.

:- meta_predicate py_call_string(:, +, -).

%   py_call_string(:String, +DictIn, -Dict) is nondet.
%
%   Support janus.query_once() and janus.query(). Parses   String  into a goal
%   term. Next, all variables from the goal   term that appear in DictIn
%   are bound to the value from  this   dict.  Dict  is created from the
%   remaining variables, unless they  start   with  an underscore (e.g.,
%   `_Time`) and the key `truth. On   success,  the Dict values contain
%   the bindings from the  answer  and   `truth`  is  either  `true` or
%   `Undefined`. On failure, the Dict values are bound to `None` and the
%   `truth` is `false`.
%
%   Parsing and distributing the variables over the two dicts is cached.

py_call_string(M:String, Input, Dict) :-
    py_call_cache(String, Input, TV, M, Goal, Dict, Truth, OutVars),
    !,
    py_call(TV, M:Goal, Truth, OutVars).
py_call_string(M:String, Input, Dict) :-
    term_string(Goal, String, [variable_names(Map)]),
    unbind_dict(Input, VInput),
    exclude(not_in_projection(VInput), Map, OutBindings),
    dict_create(Dict, bindings, [truth=Truth|OutBindings]),
    maplist(arg(2), OutBindings, OutVars),
    TV = Input.get(truth, 'PLAIN_TRUTHVALS'),
    asserta(py_call_cache(String, VInput, TV, M, Goal, Dict, Truth, OutVars)),
    VInput = Input,
    py_call(TV, M:Goal, Truth, OutVars).

py_call('NO_TRUTHVALS', M:Goal, Truth, OutVars) =>
    (   call(M:Goal)
    *-> bind_status_no_no_truthvals(Truth)
    ;   Truth = @false,
	maplist(bind_none, OutVars)
    ).
py_call('PLAIN_TRUTHVALS', M:Goal, Truth, OutVars) =>
    (   call(M:Goal)
    *-> bind_status_plain_truthvals(Truth)
    ;   Truth = @false,
	maplist(bind_none, OutVars)
    ).
py_call('DELAY_LISTS', M:Goal, Truth, OutVars) =>
    (   call_delays(M:Goal, Delays)
    *-> bind_status_delay_lists(Delays, Truth)
    ;   Truth = @false,
	maplist(bind_none, OutVars)
    ).
py_call('RESIDUAL_PROGRAM', M:Goal, Truth, OutVars) =>
    (   call_delays(M:Goal, Delays)
    *-> bind_status_residual_program(Delays, Truth)
    ;   Truth = @false,
	maplist(bind_none, OutVars)
    ).

not_in_projection(Input, Name=Value) :-
    (   get_dict(Name, Input, Value)
    ->  true
    ;   sub_atom(Name, 0, _, _, '_')
    ).

bind_none(@none).

bind_status_no_no_truthvals(@true).

bind_status_plain_truthvals(Truth) =>
    (   '$tbl_delay_list'([])
    ->  Truth = @true
    ;   py_undefined(Truth)
    ).

bind_status_delay_lists(true, Truth) =>
    Truth = @true.
bind_status_delay_lists(Delays, Truth) =>
    py_call(janus:'Undefined'(prolog(Delays)), Truth).

bind_status_residual_program(true, Truth) =>
    Truth = @true.
bind_status_residual_program(Delays, Truth) =>
    delays_residual_program(Delays, Program),
    py_call(janus:'Undefined'(prolog(Program)), Truth).

py_undefined(X) :-
    py_call(janus:undefined, X).

unbind_dict(Dict0, Dict) :-
    dict_pairs(Dict0, Tag, Pairs0),
    maplist(unbind, Pairs0, Pairs),
    dict_pairs(Dict, Tag, Pairs).

unbind(Name-_, Name-_) :-
    sub_atom(Name, 0, 1, _, Char1),
    char_type(Char1, prolog_var_start),
    !.
unbind(NonVar, NonVar).


		 /*******************************
		 *     SUPPORT PYTHON CALLS     *
		 *******************************/

:- public
       px_cmd/3,
       px_call/4,
       px_comp/7.

% These predicates are helpers  for the corresponding Python functions
% in janus.py.


%   px_call(+Input:tuple, +Module, -Pred, -Ret)
%
%   Supports  px_qdet()  and  apply().  Note    that   these  predicates
%   explicitly address predicates  in  a   particular  module.  For meta
%   predicates, this implies they also control  the context module. This
%   leads to ``janus.cmd("consult", "consult", file)`` to consult _file_
%   into the module `consult`, which is not   what we want. Therefore we
%   set the context module to `user`, which is better, but probably also
%   not what we want.

px_call(-(), Module, Pred, Ret) =>
    @(call(Module:Pred, Ret), user).
px_call(-(A1), Module, Pred, Ret) =>
    @(call(Module:Pred, A1, Ret), user).
px_call(-(A1,A2), Module, Pred, Ret) =>
    @(call(Module:Pred, A1, A2, Ret), user).
px_call(-(A1,A2,A3), Module, Pred, Ret) =>
    @(call(Module:Pred, A1, A2, A3, Ret), user).
px_call(-(A1,A2,A3,A4), Module, Pred, Ret) =>
    @(call(Module:Pred, A1, A2, A3, A4, Ret), user).
px_call(Tuple, Module, Pred, Ret) =>
    compound_name_arguments(Tuple, _, Args),
    append(Args, [Ret], GArgs),
    Goal =.. [Pred|GArgs],
    @(Module:Goal, user).

px_cmd(Module, Pred, Tuple) :-
    (   compound(Tuple)
    ->  compound_name_arguments(Tuple, _, Args),
	Goal =.. [Pred|Args]
    ;   Goal = Pred
    ),
    @(Module:Goal, user).

px_comp(Module, Pred, Tuple, Vars, Set, TV, Ret) :-
    length(Out, Vars),
    (   compound(Tuple)
    ->  compound_name_arguments(Tuple, _, Args),
	append(Args, Out, GArgs),
	Goal =.. [Pred|GArgs]
    ;   Goal =.. [Pred|Out]
    ),
    compound_name_arguments(OTempl0, -, Out),
    tv_goal_and_template(TV, @(Module:Goal, user), FGoal, OTempl0, OTempl),
    findall(OTempl, FGoal, Ret0),
    (   Set == @true
    ->  sort(Ret0, Ret)
    ;   Ret = Ret0
    ).

:- meta_predicate
    call_delays_py(0, -).

% 0,1,2: TruthVal(Enum) from janus.py
tv_goal_and_template('NO_TRUTHVALS',
                     Goal, Goal, Templ, Templ) :- !.
tv_goal_and_template('PLAIN_TRUTHVALS',
                     Goal, ucall(Goal, TV), Templ, -(Templ,TV)) :- !.
tv_goal_and_template('DELAY_LISTS',
                     Goal, call_delays_py(Goal, TV), Templ, -(Templ,TV)) :- !.
tv_goal_and_template(Mode, _, _, _, _) :-
    domain_error("px_comp() truth", Mode).

:- public
    ucall/2,
    call_delays_py/2.

ucall(Goal, TV) :-
    call(Goal),
    (   '$tbl_delay_list'([])
    ->  TV = 1
    ;   TV = 2
    ).

call_delays_py(Goal, PyDelays) :-
    call_delays(Goal, Delays),
    (   Delays == true
    ->  PyDelays = []
    ;   comma_list(Delays, Array),
        maplist(term_string, Array, PyDelays)
    ).


		 /*******************************
		 *          PYTHON I/O          *
		 *******************************/

%   py_write(+Stream, -String) is det.
%   py_readline(+Stream, +Size, +Prompt, +Line) is det.
%
%   Called from redefined Python console  I/O   to  write/read using the
%   Prolog streams.

:- '$hide'((py_write/1,
	    py_readline/4)).

py_write(Stream, String) :-
    notrace(format(Stream, '~s', [String])).

py_readline(Stream, Size, Prompt, Line) :-
    notrace(py_readline_(Stream, Size, Prompt, Line)).

py_readline_(Stream, _Size, Prompt, Line) :-
    prompt1(Prompt),
    read_line_to_string(Stream, Read),
    (   Read == end_of_file
    ->  Line = ""
    ;   string_concat(Read, "\n", Line),
	py_add_history(Read)
    ).

py_add_history(Line) :-
    ignore(catch(prolog:history(user_input, add(Line)), _, true)).


		 /*******************************
		 *          COMPILING           *
		 *******************************/

%   py_consult(+File, +Data, +Module) is det.
%
%   Support janus.consult(file, data=None, module='user').

:- public py_consult/3.
py_consult(File, @none, Module) =>
    consult(Module:File).
py_consult(File, Data, Module) =>
    setup_call_cleanup(
	open_string(Data, In),
	load_files(Module:File, [stream(In)]),
	close(In)).


		 /*******************************
		 *           MESSAGES		*
		 *******************************/

:- multifile
    prolog:error_message//1,
    prolog:message_context//1,
    prolog:message//1.

prolog:error_message(python_error(Class, Value)) -->
    { py_str(Value, Message)
    },
    [ 'Python ', ansi(code, "'~w'", [Class]), ':', nl,
      '  ~w'-[Message]
    ].
prolog:error_message(permission_error(import_as, py_module, As)) -->
    [ 'Janus: No permission to import a module as ', ansi(code, '~q', As),
      ': module exists.'
    ].

prolog:message_context(context(_, PythonCtx)) -->
    { nonvar(PythonCtx),
      PythonCtx = python_stack(Stack),
      current_prolog_flag(py_backtrace, true),
      py_is_object(Stack),
      !,
      current_prolog_flag(py_backtrace_depth, Depth),
      py_call(traceback:format_tb(Stack, Depth), Frames)
    },
    [ nl, 'Python stack:', nl ],
    sequence(py_stack_frame, Frames).

py_stack_frame(String) -->
    { split_string(String, "\n", "", Lines)
    },
    sequence(msg_line, [nl], Lines).

msg_line(Line) -->
    [ '~s'-[Line] ].

prolog:message(janus(Msg)) -->
    message(Msg).

message(version(Janus, Python)) -->
    [ 'Janus ~w embeds Python ~w'-[Janus, Python] ].
message(venv(Dir, _EnvSiteDir)) -->
    [ 'Janus: using venv from ~p'-[Dir] ].
message(venv(no_site_package_dir(VEnvDir, Dir))) -->
    [ 'Janus: venv dirrectory ~p does not contain ~p'-[VEnvDir, Dir] ].
message(py_shell(no_janus)) -->
    [ 'Janus: py_shell/0: Importing janus into the Python shell requires Python 3.10 or later.', nl,
      'Run "', ansi(code, 'from janus import *', []), '" in the Python shell to import janus.'
    ].
message(add_cwd) -->
    [ 'Interactive session; added `.` to Python `sys.path`'-[] ].
