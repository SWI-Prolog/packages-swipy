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

:- module(test_janus,
          [ test_janus/0,
            bench_janus/0
          ]).
:- use_module(janus).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply_macros), []).

test_janus :-
    run_tests([ janus,
                janus_gc,
		python_call_prolog
              ]).

:- initialization
    source_file(test_janus, File),
    file_directory_name(File, Dir0),
    directory_file_path(Dir0, tests, Dir),
    writeln(Dir),
    setenv('PYTHONPATH', Dir).

:- begin_tests(janus).

test(noarg, Z == 42) :-
    py_call(demo:int(), Z).
test(multiply, Z == 6) :-
    py_call(demo:multiply(2,3), Z).
test(multiply, Z == 6.8) :-
    py_call(demo:multiply(2,3.4), Z).
test(concat, Z == "aapnoot") :-
    py_call(demo:concat("aap", "noot"), Z).
test(concat, Z == "aapno\u0000ot") :-
    py_call(demo:concat("aap", "no\u0000ot"), Z).
test(concat, Z == [1,2,3]) :-
    py_call(demo:concat([1], [2,3]), Z).
test(dict, Z == py{name:"bob", age:42}) :-
    py_call(demo:echo(py{name:"bob", age:42}), Z).
test(bool, Z == true) :-
    py_call(demo:echo(true), Z).
test(bool, Z == false) :-
    py_call(demo:echo(false), Z).
test(attr, Val = 42) :-
    py_call(demo:test_attr = 42),
    py_call(demo:test_attr, Val).
test(dog, Tricks == ["roll over"]) :-
    py_call(dog:'Dog'('Fido'), Dog),
    py_call(Dog:add_trick("roll over"), _),
    py_call(Dog:tricks, Tricks).
test(arg, R == py{a:1,b:2,c:3}) :-
    py_call(demo:kwd(1,2), R).
test(arg, R == py{a:1,b:2,c:4}) :-
    py_call(demo:kwd(1,2,4), R).
test(arg, R == py{a:1,b:"a",c:3}) :-
    py_call(demo:kwd(1,b="a"), R).
test(arg, R == py{a:1,b:"a",c:"x"}) :-
    py_call(demo:kwd(1,c="x",b="a"), R).
test(dict, R=py{a:1, 2:2}) :-
    py_call(demo:dict1(), R).
test(dict, error(representation_error(py_dict_key))) :-
    py_call(demo:dict2(), _).

:- end_tests(janus).

:- begin_tests(janus_gc).

test(gc) :-
    py_call(demo:gced = 0),
    forall(between(1, 10 000, _),
           py_call(demo:'GCAble'(), _)),
    garbage_collect_atoms,
    py_call(demo:gced, GCed),
    assertion(GCed > 1 000).

:- end_tests(janus_gc).

:- begin_tests(python_call_prolog).

test(iter, Sum == 500500) :-
    py_call(demo:bench_iter(1000), Sum).
test(iter, Sum == 10011) :-
    py_call(demo:abort_iter(1000), Sum).
test(iter, Sum == 10011) :-             % Check we didn't mess up the Prolog stack
    py_call(demo:abort_iter(1000), Sum).

:- end_tests(python_call_prolog).


bench_janus :-
    bench_python(concat_list(100 000)).

bench_python(concat_list(N)) :-
    numlist(1, N, List),
    time(py_call(demo:concat(List, List), Concat)),
    length(Concat, N2),
    assertion(N*2 =:= N2).
