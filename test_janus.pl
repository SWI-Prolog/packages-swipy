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

:- module(test_janus,
          [ test_janus/0,
            bench_janus/0
          ]).
:- use_module(library(janus)).
:- use_module(library(plunit)).
:- use_module(library(debug)).
:- use_module(library(apply_macros), []).
:- use_module(library(lists), [numlist/3]).
:- use_module(library(statistics), [time/1]).
:- use_module('tests/russel', []).
:- use_module(library(error), [must_be/2]).
:- use_module(library(prolog_code), [comma_list/2]).

:- encoding(utf8).

test_janus :-
    run_tests([ janus_data,
                janus_prolog_data,
                janus_eval,
                janus_py_builtin,
                janus_obj,
                janus_py_object,
                janus_params,
                janus_reflect,
                janus_gc,
		python_call_prolog,
		janus_iter,
		janus_errors,
		janus_unicode,
                janus_load,
		xsb_call
              ]).

:- py_add_lib_dir(tests).

:- begin_tests(janus_data).

test(noarg, Z == 42) :-
    py_call(demo:int(), Z).
test(multiply, Z == 6) :-
    py_call(demo:multiply(2,3), Z).
test(bigint, Z == Big) :-
    Big is 1<<1000,
    py_call(janus:echo(Big), Z).
test(rational, Z == 1r3) :-
    py_call(janus:echo(1r3), Z).
test(rational, Z == A) :-
    A is rdiv(random(1<<1000), random(1<<1000)),
    py_call(janus:echo(A), Z).
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
test(nodict, Z == py{}) :-
    py_call(janus:echo(py({})), Z).
test(nodict, Z == {}) :-
    py_call(janus:echo({}), Z).
test(nodict, Z == @true) :-
    py_call(demo:isstr({}), Z).
test(bool, Z == @true) :-
    py_call(janus:echo(@true), Z).
test(bool, Z == @false) :-
    py_call(janus:echo(@false), Z).
test(bool, Z == true) :-
    py_call(janus:echo(true), Z).
test(bool, Z == false) :-
    py_call(janus:echo(false), Z).
test(none, Z == @none) :-
    py_call(janus:echo(@none), Z).
test(none, Z == 'None') :-
    py_call(janus:echo('None'), Z).
test(bool, Z == "true") :-
    py_call(janus:echo(true), Z, [py_string_as(string)]).
test(bool, Z == "false") :-
    py_call(janus:echo(false), Z, [py_string_as(string)]).
test(none, Z == "None") :-
    py_call(janus:echo('None'), Z, [py_string_as(string)]).
test(set, Set == [1, a, false]) :- % True canot be in a Python set??
    py_call(janus:echo(py_set([1,a,false])), py_set(List)),
    sort(List, Set).
test(attr, Val = 42) :-
    py_setattr(demo, test_attr, 42),
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
test(dict) :-
    py_call(demo:dict1(), R, [py_dict_as({})]),
    assertion(py_same_dict(R, {a:1,2:2})).
test(empty_dict, R == py({})) :-
    py_call(demo:dict0(), R, [py_dict_as({})]).
test(dict) :-                              % invalid key: return as {}
    py_call(demo:dict2(), Dict),
    assertion(py_is_dict(Dict)),
    assertion(py_same_dict(Dict, {a:1,2.3:2})).
test(iterator, L == [1,2,3,4]) :-          % An iterator checks as a sequence
    py_call(range(1,5), L).
test(set, Ordered == [1,2,3]) :-
    py_call(janus:echo(py_set([1,2,3])), Obj, [py_object(true)]),
    py_call(Obj, py_set(Set)),
    sort(Set, Ordered),
    py_call(Obj, Set2),
    assertion(Set2 == py_set(Set)).

:- end_tests(janus_data).

py_same_dict({D1}, {D2}) :-
    comma_list(D1, KV1),
    comma_list(D2, KV2),
    KV1 == KV2.

:- begin_tests(janus_prolog_data).

test(echo, Term =@= Copy) :-
    Term = f(X,X,_),
    py_call(janus:echo(prolog(Term)), Copy).
test(echo, Term =@= Copy) :-
    Term = f(X,X,_),
    py_call(janus:echo(prolog(Term)), Ref, [py_object]),
    py_call(Ref:'__str__'(), Str),
    term_string(Copy, Str).

:- end_tests(janus_prolog_data).

:- begin_tests(janus_eval).

test(eval) :-
    py_call(janus:echo(eval(sys:path)), Path),
    must_be(list(atom), Path).

:- end_tests(janus_eval).

:- begin_tests(janus_py_builtin).

test(globals, Class == dict) :-
    py_call(globals():'__class__':'__name__', Class).

:- end_tests(janus_py_builtin).

:- begin_tests(janus_obj).

test(self, Self == Dog) :-
    py_call(dog:'Dog'('Fido'), Dog),
    py_call(Dog:self(), Self).
test(dog, Tricks == ['roll over']) :-
    py_call(dog:'Dog'('Fido'), Dog),
    py_call(Dog:add_trick('roll over'), _),
    py_call(Dog:tricks, Tricks).

:- end_tests(janus_obj).

:- begin_tests(janus_py_object).

test(baseclass, X == @none) :-
    py_call(janus:echo(@none), X, [py_object(true)]).
test(baseclass, X == @true) :-
    py_call(janus:echo(@true), X, [py_object(true)]).
test(baseclass, X == @false) :-
    py_call(janus:echo(@false), X, [py_object(true)]).
test(baseclassdint, X == 42) :-
    py_call(janus:echo(42), X, [py_object(true)]).
test(baseclass, X =:= 3.14) :-
    py_call(janus:echo(3.14), X, [py_object(true)]).
test(baseclass, X == 'a string') :-
    py_call(janus:echo('a string'), X, [py_object(true)]).
test(baseclass, X == a-1) :-
    py_call(janus:echo(a-1), X, [py_object(true)]).

test(list, List = [1,2,3]) :-
    py_call(janus:echo([1,2,3]), X, [py_object(true)]),
    assertion(py_is_object(X)),
    py_call(X, List).

test(subclass, V == 42) :-
    py_call(baseclasses:myint(42), X, [py_object(true)]),
    assertion(py_is_object(X)),
    py_call(X, V).

:- end_tests(janus_py_object).

:- begin_tests(janus_params).

test(arg, R == py{a:1,b:2,c:3}) :-
    py_call(demo:kwd(1,2), R).
test(arg, R == py{a:1,b:2,c:4}) :-
    py_call(demo:kwd(1,2,4), R).
test(arg, R == py{a:1,b:a,c:3}) :-
    py_call(demo:kwd(1,b=a), R).
test(arg, R == py{a:1,b:a,c:x}) :-
    py_call(demo:kwd(1,c=x,b=a), R).
test(arg, R == py{a:1,b:2,c:3}) :-
    py_call(demo:kwd_all(), R).
test(arg, R == py{a:1,b:a,c:3}) :-
    py_call(demo:kwd_all(b=a), R).
test(arg, R == py{a:a,b:b,c:c}) :-
    py_call(demo:kwd_all(c=c,b=b,a=a), R).

:- end_tests(janus_params).

:- begin_tests(janus_reflect).

test(hasattr) :-
    py_hasattr(sys, Attr), Attr == path, !.
test(hasattr) :-
    py_hasattr(sys, path).
test(type) :-
    py_call(baseclasses:myint(42), X, [py_object(true)]),
    py_isinstance(X, int).
test(type) :-
    py_call(dog:'Greyhound'('Speedy'), X),
    py_isinstance(X, dog:'Dog').
test(type, Type == 'Greyhound') :-
    py_call(dog:'Greyhound'('Speedy'), X),
    py_type(X, Type).
test(type, Type == int) :-
    py_type(42, Type).
test(obj_has_attr) :-
    py_call(dog:'Greyhound'('Speedy'), X),
    py_hasattr(X, tricks).

:- end_tests(janus_reflect).


:- begin_tests(janus_gc).

% This is rather tricky.  If we do not stop the GC thread AGC may
% already be running and our garbage_collect_atoms/0 call is a dummy.
% Therefore we stop this thread and restore the old mode after the
% test.

test(gc1) :-
    current_prolog_flag(gc_thread, Old),
    set_prolog_gc_thread(false),
    py_setattr(demo, gced, 0),
    forall(between(1, 10 000, _),
           py_call(demo:'GCAble'(), _)),
    garbage_collect_atoms,
    set_prolog_gc_thread(Old),
    py_call(demo:gced, GCed),
    assertion(GCed > 1 000).
test(gc2, [GCed0,GCed] == [10 000, 10 000]) :-
    current_prolog_flag(gc_thread, Old),
    set_prolog_gc_thread(false),
    garbage_collect_atoms,
    py_setattr(demo, gced, 0),
    forall(between(1, 10 000, _),
           ( py_call(demo:'GCAble'(), Obj),
	     py_free(Obj) )),
    py_call(demo:gced, GCed0),
    garbage_collect_atoms,
    set_prolog_gc_thread(Old),
    py_call(demo:gced, GCed).
test(free, GCed == 1) :-
    py_setattr(demo, gced, 0),
    py_call(demo:'GCAble'(), Obj),
    py_free(Obj),
    py_call(demo:gced, GCed).
test(free, error(existence_error('py_object', Obj))) :-
    py_call(demo:'GCAble'(), Obj),
    py_free(Obj),
    py_free(Obj).
test(free, error(existence_error('py_object', Obj))) :-
    py_call(demo:'GCAble'(), Obj),
    py_free(Obj),
    py_setattr(Obj, test, 1).

:- end_tests(janus_gc).

:- begin_tests(python_call_prolog).

test(iter, Sum == 500500) :-
    py_call(demo:bench_query_iter(1000), Sum).
test(iter, Sum == 10011) :-
    py_call(demo:abort_iter(1000), Sum).
test(iter, Sum == 10011) :-             % Check we didn't mess up the Prolog stack
    py_call(demo:abort_iter(1000), Sum).
test(undef, Result == py{truth:Undef}) :-
    py_call(janus:query_once(undefined), Result),
    py_call(janus:undefined, Undef).
test(russel, List == [py{'X':barber,'Y':barber,truth:Undef},
		      py{'X':barber,'Y':mayor,truth: @true}]) :-
    py_call(demo:shaves(), List0),
    sort('Y', @=<, List0, List),
    py_call(janus:undefined, Undef).
test(py_double, Tuples == [1-1,2-1,3-1,
                           1-2,2-2,3-2,
                           1-3,2-3,3-3,
                           1-4,2-4,3-4]) :-
    py_call(demo:double_iter(3,4), Tuples).
test(invalid_nesting, X == @true) :-
    py_call(demo:test_invalid_nesting(), X).
:- if((py_call(sys:hexversion, V), V >= 0x03080000)).
test(while, X == [1,2,3]) :-
    py_call(while:test_while(), X).
:- endif.
test(apply, X == 3) :-
    py_call(janus:apply_once(user, plus, 1, 2), X).
test(apply, X == 1) :-
    py_call(janus:apply_once(user, between, 1, 2), X).
test(apply, X == 0) :-
    py_call(janus:apply_once(user, between, 3, 2, fail=0), X).
test(apply, error(python_error('PrologError',_))) :-
    py_call(janus:apply_once(user, between, 3, 2), _).
test(apply, error(python_error('PrologError',_))) :-
    py_call(janus:apply_once(janus, no_such_predicate), _).
test(apply, X == 1) :-
    py_call(janus:apply_once(user, =, 1), X).
test(apply, error(python_error('PrologError',_))) :-
    py_call(janus:apply_once(user, =, prolog(f(a))), _).

:- end_tests(python_call_prolog).

:- begin_tests(janus_iter).

test(range, all(X = [0,1,2,3,4,5])) :-
    py_iter(range(0, 6), X).
test(square, all(X = [1,4,9,16])) :-
    py_iter(demo:squares(1,5), X).
test(double, Pairs == [1-1,1-2,2-1,2-2]) :-
    findall(X-Y,
            (   py_iter(range(1,3), X),
                py_iter(range(1,3), Y)
            ), Pairs).

:- end_tests(janus_iter).

:- begin_tests(janus_errors).

test(ex) :-
    py_call(demo:call_except(foo), Ex),
    py_call(Ex:'__str__'(), Str),
    assertion(sub_string(Str,_,_,_,'Unknown procedure: foo/0')).
test(catch) :-
    py_call(demo:try_catch(), Result),
    assertion(Result == @true).         % Otherwise, the exception

:- end_tests(janus_errors).

:- begin_tests(janus_unicode).

test(cyrillic, R == воздуха) :-
    py_call(demo:воздуха(), R).
test(iso_latin_1, R == schloß) :-
    py_call(demo:schloß(), R).

:- end_tests(janus_unicode).

:- begin_tests(janus_load).
:- use_module(library(strings)).

test(load, X == 4) :-
    py_module(load_test,
              {|string||
               | def times_two(n):
               |     return n*2
               |}),
    py_call(load_test:times_two(2), X).
test(reload, [X1,X2] == [6,4]) :-
    py_module(load_test2,
              {|string||
               | def times_two(n):
               |     return n*3
               |}),
    py_call(load_test2:times_two(2), X1),
    py_module(load_test2,
              {|string||
               | def times_two(n):
               |     return n*2
               |}),
    py_call(load_test2:times_two(2), X2).

:- end_tests(janus_load).


:- begin_tests(xsb_call).

test(reverse, X == [py{a:py{b:c}}, -(mytuple), 3, 2, 1]) :-
    py_call(janus:apply_once(lists, reverse,
                             [1,2,3,-(mytuple),py{a:py{b:c}}]),
	    X).
test(comp1, X == [-(1)-1,-(2)-1]) :-
    py_call(janus:comp(user, between, 1, 2), X).
test(comp2, X == [-(1),-(2)]) :-
    py_call(janus:'NO_TRUTHVALS', NoThruthVals),
    py_call(janus:comp(user, between, 1, 2, truth_vals=NoThruthVals), X).
test(cmd, X == @(true)) :-
    py_call(janus:cmd(user, true), X).
test(cmd, X == @(false)) :-
    py_call(janus:cmd(user, fail), X).
test(cmd, X == Undefined) :-
    py_call(janus:undefined, Undefined),
    py_call(janus:cmd(user, undefined), X).
test(cmd, X == @true) :-
    py_call(janus:cmd(test_janus, p, 42), X).
test(cmd, X == @false) :-
    py_call(janus:cmd(test_janus, p, 1), X).
test(cmd, error(python_error('PrologError', _))) :-
    py_call(janus:cmd(test_janus, no_such_pred), _).

test_janus:p(42).

:- end_tests(xsb_call).

bench_janus :-
    bench_python(concat_list(100 000)).

bench_python(concat_list(N)) :-
    numlist(1, N, List),
    time(py_call(demo:concat(List, List), Concat)),
    length(Concat, N2),
    assertion(N*2 =:= N2).
