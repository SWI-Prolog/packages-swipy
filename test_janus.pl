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
:- use_module(library(janus)).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply_macros), []).
:- use_module(library(filesex), [directory_file_path/3]).
:- use_module(library(lists), [numlist/3]).
:- use_module(library(statistics), [time/1]).
:- use_module('tests/russel', []).

test_janus :-
    run_tests([ janus_data,
                janus_obj,
                janus_params,
                janus_gc,
		python_call_prolog,
		janus_iter,
		xsb_call
              ]).

:- initialization
    source_file(test_janus, File),
    file_directory_name(File, Dir0),
    directory_file_path(Dir0, tests, Dir),
    add_py_lib_dir(Dir, first).

:- begin_tests(janus_data).

test(noarg, Z == 42) :-
    py_call(demo:int(), Z).
test(multiply, Z == 6) :-
    py_call(demo:multiply(2,3), Z).
test(bigint, Z == Big) :-
    Big is 1<<1000,
    py_call(janus:echo(Big), Z).
test(multiply, Z == 6.8) :-
    py_call(demo:multiply(2,3.4), Z).
test(concat, Z == 'aapnoot') :-
    py_call(demo:concat('aap', 'noot'), Z).
test(concat, Z == 'aapno\u0000ot') :-
    py_call(demo:concat('aap', 'no\u0000ot'), Z).
test(concat, Z == [1,2,3]) :-
    py_call(demo:concat([1], [2,3]), Z).
test(dict, Z == py{name:bob, age:42}) :-
    py_call(janus:echo(py{name:bob, age:42}), Z).
test(dict, Z == py{name:bob, age:42}) :-
    py_call(janus:echo({name:bob, age:42}), Z).
test(dict, Z == py{name:bob, age:42}) :-
    py_call(janus:echo(py({name:bob, age:42})), Z).
test(dict, Z == py{name:"bob", age:42}) :-
    py_call(janus:echo(py{name:bob, age:42}), Z, [py_string_as(string)]).
test(bool, Z == true) :-
    py_call(janus:echo(true), Z).
test(bool, Z == false) :-
    py_call(janus:echo(false), Z).
test(none, Z == 'None') :-
    py_call(janus:echo('None'), Z).
test(bool, Z == true) :-
    py_call(janus:echo(true), Z, [py_string_as(string)]).
test(bool, Z == false) :-
    py_call(janus:echo(false), Z, [py_string_as(string)]).
test(none, Z == 'None') :-
    py_call(janus:echo('None'), Z, [py_string_as(string)]).
test(set, Set == [1, a, false]) :- % True canot be in a Python set??
    py_call(janus:echo(pySet([1,a,false])), pySet(List)),
    sort(List, Set).
test(attr, Val = 42) :-
    py_call(demo:test_attr = 42),
    py_call(demo:test_attr, Val).
test(stringify, R == '6') :-
    py_call(janus:echo(#6), R).
test(stringify, R == '3.14') :-
    py_call(janus:echo(#3.14), R).
test(stringify, R == 'false') :-
    py_call(janus:echo(#false), R).
test(stringify, R == 'None') :-
    py_call(janus:echo(#'None'), R).
test(stringify, R == 'aap noot') :-        % does _not_ quote
    py_call(janus:echo(#'aap noot'), R).
test(stringify_wc, R == '+(1,2)') :-       % write_canonical/1
    py_call(janus:echo(#(1 + 2)), R).
test(stringify_wc, R == 'f(A,_,A)') :-     % numbervars
    py_call(janus:echo(#f(X,_,X)), R).
test(dict, R=py{a:1, 2:2}) :-
    py_call(demo:dict1(), R).
test(dict, error(representation_error(py_dict_key))) :-
    py_call(demo:dict2(), _).
test(iterator, L == [1,2,3,4]) :-          % An iterator checks as a sequence
    py_call(range(1,5), L).

:- end_tests(janus_data).

:- begin_tests(janus_obj).

test(dog, Tricks == ['roll over']) :-
    py_call(dog:'Dog'('Fido'), Dog),
    py_call(Dog:add_trick('roll over'), _),
    py_call(Dog:tricks, Tricks).

:- end_tests(janus_obj).

:- begin_tests(janus_params).

test(arg, R == py{a:1,b:2,c:3}) :-
    py_call(demo:kwd(1,2), R).
test(arg, R == py{a:1,b:2,c:4}) :-
    py_call(demo:kwd(1,2,4), R).
test(arg, R == py{a:1,b:a,c:3}) :-
    py_call(demo:kwd(1,b=a), R).
test(arg, R == py{a:1,b:a,c:x}) :-
    py_call(demo:kwd(1,c=x,b=a), R).

:- end_tests(janus_params).

:- begin_tests(janus_gc).

% This is rather tricky.  If we do not stop the GC thread AGC may
% already be running and our garbage_collect_atoms/0 call is a dummy.
% Therefore we stop this thread and restore the old mode after the
% test.

test(gc) :-
    current_prolog_flag(gc_thread, Old),
    set_prolog_gc_thread(false),
    py_call(demo:gced = 0),
    forall(between(1, 10 000, _),
           py_call(demo:'GCAble'(), _)),
    garbage_collect_atoms,
    set_prolog_gc_thread(Old),
    py_call(demo:gced, GCed),
    assertion(GCed > 1 000).
test(gc, [GCed0,GCed] == [10 000, 10 000]) :-
    current_prolog_flag(gc_thread, Old),
    set_prolog_gc_thread(false),
    garbage_collect_atoms,
    py_call(demo:gced = 0),
    forall(between(1, 10 000, _),
           ( py_call(demo:'GCAble'(), Obj),
	     py_free(Obj) )),
    py_call(demo:gced, GCed0),
    garbage_collect_atoms,
    set_prolog_gc_thread(Old),
    py_call(demo:gced, GCed).
test(free, GCed == 1) :-
    py_call(demo:gced = 0),
    py_call(demo:'GCAble'(), Obj),
    py_free(Obj),
    py_call(demo:gced, GCed).
test(free, error(existence_error('PyObject', Obj))) :-
    py_call(demo:'GCAble'(), Obj),
    py_free(Obj),
    py_free(Obj).
test(free, error(existence_error('PyObject', Obj))) :-
    py_call(demo:'GCAble'(), Obj),
    py_free(Obj),
    py_call(Obj:test = 1).

:- end_tests(janus_gc).

:- begin_tests(python_call_prolog).

test(iter, Sum == 500500) :-
    py_call(demo:bench_iter(1000), Sum).
test(iter, Sum == 10011) :-
    py_call(demo:abort_iter(1000), Sum).
test(iter, Sum == 10011) :-             % Check we didn't mess up the Prolog stack
    py_call(demo:abort_iter(1000), Sum).
test(undef, Result == py{status:'Undefined'}) :-
    py_call(janus:once(undefined), Result).
test(russel, List == [py{'X':barber,'Y':barber,status:'Undefined'},
		      py{'X':barber,'Y':mayor,status:true}]) :-
    py_call(demo:shaves(), List0),
    sort('Y', @=<, List0, List).

:- end_tests(python_call_prolog).

:- begin_tests(janus_iter).

test(range, all(X = [0,1,2,3,4,5])) :-
    py_iter(range(0, 6), X).
test(square, all(X = [1,4,9,16])) :-
    py_iter(demo:squares(1,5), X).

:- end_tests(janus_iter).

:- begin_tests(xsb_call).

test(reverse, X == :([py{a:py{b:c}}, :(mytuple), 3, 2, 1], 1)) :-
    py_call(janus:px_qdet(lists, reverse,
			  [1,2,3,:(mytuple),py{a:py{b:c}}]),
	    X).
:- end_tests(xsb_call).


bench_janus :-
    bench_python(concat_list(100 000)).

bench_python(concat_list(N)) :-
    numlist(1, N, List),
    time(py_call(demo:concat(List, List), Concat)),
    length(Concat, N2),
    assertion(N*2 =:= N2).
