/*  Copyright 2023 Theresa Swift and Carl Anderson

    Permission is hereby granted, free of charge,  to any person obtaining a
    copy  of  this  software  and    associated   documentation  files  (the
    “Software”), to deal in  the   Software  without  restriction, including
    without limitation the rights to  use,   copy,  modify,  merge, publish,
    distribute, sublicense, and/or sell  copies  of   the  Software,  and to
    permit persons to whom the Software is   furnished  to do so, subject to
    the following conditions:

    The above copyright notice and this  permission notice shall be included
    in all copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT  WARRANTY OF ANY KIND, EXPRESS
    OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
    CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
    TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
    SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

:- module(test_xsb_janus,
          [ test_xsb_janus/0
          ]).

/** <module> Port of XSB test suite

This module is a port of testSuite.P from XSB.  Encountered issues

  - True,False --> 1,0 instead of true,false.
  - Dict       --> {[k:v, ...]} instead of {k:v, ...}
*/

:- use_module(library(janus)).
:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(filesex)).

:- set_prolog_flag(encoding, utf8) .

test_xsb_janus :-
    run_tests([ xsb_janus
              ]).

:-  py_add_lib_dir(xsb_tests).

py_gc :-
    py_call(gc:collect()).

:- begin_tests(xsb_janus,
	       [ cleanup(py_gc)
	       ]).

test(sumlist) :-
    py_func('sumlist3', sumlist3(5,[1,2,3]), [6,7,8]),
    not(py_func('sumlist3', sumlist3(5,[1,2,3]), [4,5,6])),
    not(py_func('sumlist3', sumlist3(5,[1,2,3]),[1,2,3])).
test(data_conversion) :- intConvTest.
test(data_conversion) :- floatConvTest.
test(data_conversion) :- stringConvTest.
test(data_conversion) :- listConvTest.
test(data_conversion) :- setConvTest.
test(data_conversion) :- tupleConvTest.
test(data_conversion) :- dictConvTest.
test(data_conversion) :-
    py_func(returnVal,return_None(),@none).
test(data_conversion) :-
    py_func(returnVal,return_True(),@true).
%   py_func(returnVal,return_True(),1).
test(data_conversion) :-
    py_func(returnVal,return_False(),@false).
%   py_func(returnVal,return_False(),0).
test(json1) :- json_test_1.
test(json2) :- json_test_2.
% Asking for numpy dependency for tests is a bit too much.
%test(pyc, X == 2) :-
%    py_func('numpexamp',go(),X).
test(kwargs, Ret == [foo,-(bar,1),-(baz,2)]) :-
    py_func(kwargs,kwargs_append(foo,bar=1,baz=2),Ret).
test(error) :-
    error_test(no_module,foo(1),
               ['ModuleNotFoundError', 'no_module']).
test(error) :-
    error_test(kwargs,foo(1),
               ['AttributeError', kwargs, foo]).
test(error) :-
    error_test(test_err,raise_err_1(),
               ['Exception', '(\'spam\', \'eggs\')']).
test(method) :-
    meth_tests.
test(variadic) :-
    variadic_tests.
test(gc) :-
    py_func(gc,collect(),Collect),
    assertion(integer(Collect)).


:- end_tests(xsb_janus).

intConvTest :-
    current_prolog_flag(min_tagged_integer, MinValue),
    not(var(MinValue)),
    current_prolog_flag(max_tagged_integer, MaxValue),
    not(var(MaxValue)),
    py_func('returnVal', returnVal(MinValue), MinValue),
    py_func('returnVal', returnVal(MaxValue), MaxValue).

floatConvTest :-
    py_func('returnVal', returnVal(3.54), 3.54),
    py_func('returnVal', returnVal(3.5535252352), 3.5535252352).

stringConvTest :-
    py_func('returnVal', returnVal(helloworld), helloworld),
    py_func('returnVal', returnVal('helloworld'), helloworld),
    py_func('returnVal', returnVal('Санкт-Петербург'),R3),R3 == 'Санкт-Петербург'.

listConvTest:-
    py_func('returnVal', returnVal([a,b,c]), R1),R1 = [a,b,c],
    py_func('returnVal', returnVal([]), R2), R2 == [],
    py_func('returnVal', returnVal([1,[2,3,4],[hello,155]]), R3),
    R3 ==  [1, [2, 3, 4], ['hello', 155]],
    py_func('tupInList', func(), R4), R4 == [1,2,3, -(5, 6), 'hello', [11,17]],
    !.

setConvTest:-
    py_func('returnVal',returnSet( ) ,F ),
    F = ['"foo"','''bar''',py_set(S)],
    length(S,3),
    py_func('returnVal', returnVal(py_set([a,b,c])), R1 ),
    arg(1,R1,A), length(A,3),!.

tupleConvTest:-
    py_func('returnVal', returnVal(-(a,b,c)), R1),R1 = -(a,b,c),
    py_func('tupletest',func(),R2), R2 = -(5,-(),hello,-(5,6,7)),
    !.

dictConvTest:-
    py_func(returnVal,return_dictionary(),Ret),
    Ret = py{'Name':'Geeks', 1:[1,2,3,4]}.
%   Ret = pyDict([''('Name','Geeks'),''(1,[1,2,3,4])]).

json_test_1 :-
    Jstring = '{"name": "Bob", "languages": ["English", "Fench","GERMAN"]}',
    py_func('jintf',prolog_loads(Jstring),F),
    assertion(F ==
              py{languages:['English', 'Fench', 'GERMAN'], name:'Bob'}).
json_test_2 :-
    source_file(json_test_2, This),
    file_directory_name(This, Dir),
    directory_file_path(Dir, 'xsb_tests/sample.json', JsonFile),
    py_func('jintf',prolog_load(JsonFile),F),
    assertion(F ==
              py{glossary:
                 py{'GlossDiv':
                    py{'GlossList':
                       py{'GlossEntry':
                          py{'Abbrev':'ISO 8879:1986',
                             'Acronym':'SGML',
                             'GlossDef':
                             py{ 'GlossSeeAlso':[ 'GML',
                                                  'XML'
                                                ],
                                 para:'A meta-markup language, used to \c
                                       create markup languages such as DocBook.'
                               },
                             'GlossSee':markup,
                             'GlossTerm':'Standard Generalized Markup Language',
                             'ID':'SGML',
                             'SortAs':'SGML'
                            }
                         },
                       title:'S'
                      },
                    title:'example glossary'
                   }
                }).

meth_tests:-
    test_Person(_Obj,Ret1,Ret2,Ret3,Ret4,Ret5,M1,M2),
    Ret1 = 'Hello my name is john',
    Ret2 = 'Hello my name is john and I''m a doofus',
    Ret3 = chocolate,
    Ret4 = 'Hello my name is john and I''m a real doofus',
    Ret5 = 'Hello my name is john and I''m a real big doofus',
    check_error(M1, ['Type error', py_callable, '1']),
    check_error(M2, ['Type error', py_callable, '1']).

test_Person(Obj,Ret1,Ret2,Ret3,Ret4,Ret5,Msg2,Msg3):-
    py_func('Person','Person'(john,35),Obj),
    py_dot(Obj,func0(),Ret1),
    py_dot(Obj,func1(doofus),Ret2),
    py_dot(Obj,favorite_ice_cream,Ret3),
    py_dot(Obj,func2(real,doofus),Ret4),
    py_dot(Obj,func3(real,big,doofus),Ret5),
    catch(py_dot(1,favorite_ice_cream,_Ret4),E2,xsb_error_get_message(E2,Msg2)),
    catch(py_dot(Obj,1,_Ret5),E3,xsb_error_get_message(E3,Msg3)).

variadic_tests:-
    testit(py_func(variadic,variadic_print('a','b','c'),A),A,'a|b|c|'),
    testit(py_func(variadic,variadic_print('a','b','c','d'),B),B,'a|b|c|d|'),
    testit(py_func(variadic,opt_print('a'),C),C,'a|1'),
    testit(py_func(variadic,opt_print('b','c'),D),D,'b|c').

error_test(Mod,Goal,Parts):-
    catch(py_func(Mod,Goal,_X),E,xsb_error_get_message(E, Mess)),
    check_error(Mess,Parts).

		 /*******************************
		 *            SUPPORT		*
		 *******************************/

check_error(Mess, Parts) :-
    (   maplist(appears(Mess), Parts)
    ->  true
    ;   format(user_error, 'Got ~w', [Mess]),
        fail
    ).

appears(Message, String) :-
    sub_atom(Message, _, _, _, String), !.

xsb_error_get_message(E, Msg) :-
    message_to_string(E, Msg).

testit(Call,Var,Answer):-
    call(Call),
    (   Var = Answer
    ->  true
    ;   writeln('!!!wrong_answer'(Call,Var,Answer)),
        fail
    ).
