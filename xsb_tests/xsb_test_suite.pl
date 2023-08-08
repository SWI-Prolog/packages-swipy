:- module(xsb_test_suite,
          [ xsb_test_suite/0
          ]).

/** <module> Port of XSB test suite

This module is a port of testSuite.P from XSB.  Encountered issues

  - True,False --> 1,0 instead of true,false.
  - Dict       --> {[k:v, ...]} instead of {k:v, ...}
*/

:- use_module(library(janus)).
:- use_module(library(plunit)).
:- set_prolog_flag(encoding, utf8) .

xsb_test_suite :-
    run_tests([ xsb_janus
              ]).

:- initialization
    source_file(testSuite, File),
    file_directory_name(File, Dir),
    add_py_lib_dir(Dir, first).

:- begin_tests(xsb_janus).

test(sumlist) :-
    pyfunc('sumlist3', sumlist3(5,[1,2,3]), [6,7,8]),
    not(pyfunc('sumlist3', sumlist3(5,[1,2,3]), [4,5,6])),
    not(pyfunc('sumlist3', sumlist3(5,[1,2,3]),[1,2,3])).
test(data_conversion) :- intConvTest.
test(data_conversion) :- floatConvTest.
test(data_conversion) :- stringConvTest.
test(data_conversion) :- listConvTest.
test(data_conversion) :- setConvTest.
test(data_conversion) :- tupleConvTest.
test(data_conversion) :- dictConvTest.
test(data_conversion) :-
    pyfunc(returnVal,return_None(),'None').
test(data_conversion) :-
    pyfunc(returnVal,return_True(),true).
%   pyfunc(returnVal,return_True(),1).
test(data_conversion) :-
    pyfunc(returnVal,return_False(),false).
%   pyfunc(returnVal,return_False(),0).
test(json1) :- json_test_1.
test(json2) :- json_test_2.
test(pyc, X == 2) :-
    pyfunc('numpexamp',go(),X).
test(kwargs, Ret == [foo,:(bar,1),:(baz,2)]) :-
    pyfunc(kwargs,kwargs_append(foo),[bar=1,baz=2],Ret).
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
    pyfunc(gc,collect(),Collect),
    assertion(integer(Collect)).


:- end_tests(xsb_janus).

intConvTest :-
    current_prolog_flag(min_tagged_integer, MinValue),
    not(var(MinValue)),
    current_prolog_flag(max_tagged_integer, MaxValue),
    not(var(MaxValue)),
    pyfunc('returnVal', returnVal(MinValue), MinValue),
    pyfunc('returnVal', returnVal(MaxValue), MaxValue).

floatConvTest :-
    pyfunc('returnVal', returnVal(3.54), 3.54),
    pyfunc('returnVal', returnVal(3.5535252352), 3.5535252352).

stringConvTest :-
    pyfunc('returnVal', returnVal(helloworld), helloworld),
    pyfunc('returnVal', returnVal('helloworld'), helloworld),
    pyfunc('returnVal', returnVal('Санкт-Петербург'),R3),R3 == 'Санкт-Петербург'.

listConvTest:-
    pyfunc('returnVal', returnVal([a,b,c]), R1),R1 = [a,b,c],
    pyfunc('returnVal', returnVal([]), R2), R2 == [],
    pyfunc('returnVal', returnVal([1,[2,3,4],[hello,155]]), R3),
    R3 ==  [1, [2, 3, 4], ['hello', 155]],
    pyfunc('tupInList', func(), R4), R4 == [1,2,3, :(5, 6), 'hello', [11,17]],
    !.

setConvTest:-
    pyfunc('returnVal',returnSet( ) ,F ),
    F = ['"foo"','''bar''',pySet(S)],
    length(S,3),
    pyfunc('returnVal', returnVal(pySet([a,b,c])), R1 ),
    arg(1,R1,A), length(A,3),!.

tupleConvTest:-
    pyfunc('returnVal', returnVal(:(a,b,c)), R1),R1 = :(a,b,c),
    pyfunc('tupletest',func(),R2), R2 = :(5,:(),hello,:(5,6,7)),
    !.

dictConvTest:-
    pyfunc(returnVal,return_dictionary(),Ret),
    Ret = py{'Name':'Geeks', 1:[1,2,3,4]}.
%   Ret = pyDict([''('Name','Geeks'),''(1,[1,2,3,4])]).

json_test_1 :-
    Jstring = '{"name": "Bob", "languages": ["English", "Fench","GERMAN"]}',
    pyfunc('jintf',prolog_loads(Jstring),F),
    assertion(F ==
              py{languages:['English', 'Fench', 'GERMAN'], name:'Bob'}).
json_test_2 :-
    source_file(json_test_2, This),
    file_directory_name(This, Dir),
    directory_file_path(Dir, 'sample.json', JsonFile),
    pyfunc('jintf',prolog_load(JsonFile),F),
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
    check_error(M1, ['Type error', py_target, '1']),
    check_error(M2, ['Type error', py_callable, '1']).

test_Person(Obj,Ret1,Ret2,Ret3,Ret4,Ret5,Msg2,Msg3):-
    pyfunc('Person','Person'(john,35),Obj),
    pydot('Person',Obj,func0(),Ret1),
    pydot('Person',Obj,func1(doofus),Ret2),
    pydot('Person',Obj,favorite_ice_cream,Ret3),
    pydot('Person',Obj,func2(real,doofus),Ret4),
    pydot('Person',Obj,func3(real,big,doofus),Ret5),
    catch(pydot('Person',1,favorite_ice_cream,_Ret4),E2,xsb_error_get_message(E2,Msg2)),
    catch(pydot('Person',Obj,1,_Ret5),E3,xsb_error_get_message(E3,Msg3)).

variadic_tests:-
    testit(pyfunc(variadic,variadic_print('a','b','c'),A),A,'a|b|c|'),
    testit(pyfunc(variadic,variadic_print('a','b','c','d'),B),B,'a|b|c|d|'),
    testit(pyfunc(variadic,opt_print('a'),C),C,'a|1'),
    testit(pyfunc(variadic,opt_print('b','c'),D),D,'b|c').

error_test(Mod,Goal,Parts):-
    catch(pyfunc(Mod,Goal,_X),E,xsb_error_get_message(E, Mess)),
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

