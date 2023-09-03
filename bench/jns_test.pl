:- module(jns_test,
          [ zero_ary_true/0,instan/1,
            zero_ary_undef/0, win/1, one_ary_undef/1,
            zero_ary_fail/0,
            one_ary_fail/1,p/1,
            throw_an_error/2,
            simple_call/2,
            simple_cmd/1,
            nondet_query/1,
            prolog_makelist/2,
            table_comp/1,test_comp/1,
            test_copy/2,test_copy/3,
            test_copy_1/2,
            tc_rep_max/0,
            prolog_paths/1,
            append_prolog_paths/1,
            table_comp/2,
            test_comp/2,
            test_empty_set/1,
            return_term/1,
            return_tuple/1,
            return_null/1,
            qdet_variadic/4,
            anti_member/2,
            am_3/3,
            det_bind_test/7,
            bind_test/7
          ]).
:- use_module(library(janus)).

here.
:- initialization
    source_file(here, File),
    file_directory_name(File, Dir),
    py_add_lib_dir(Dir, first).


test_copy(X,X).
test_copy(_,X,X).

test_copy_1([X],X).

prolog_paths(Dirs):-
    findall(Dir,usermod:library_directory(Dir),Dirs).

append_prolog_paths(Paths):-
    forall(member(Path, Paths),
           assertz(user:file_search_path(library, Path))).

throw_an_error(Message,_Out):-
    throw(error(janus_error(Message), _)).

instan(a).

zero_ary_true.
%    writeln('     I am a zero-ary predicate called from pt.P').

:- table win/1.
win(X):- tnot(win(X)).

:- table zero_ary_undef/0.
zero_ary_undef :- tnot(zero_ary_undef).

zero_ary_fail :- fail.

one_ary_fail(_):- fail.
one_ary_undef(b):- zero_ary_undef.

p(a).

simple_call(N,N1):- N1 is N + 1.
simple_cmd(N):- _N1 is N + 1.

nondet_query(a).
nondet_query(b).
nondet_query(c).
nondet_query(d).


prolog_makelist(0,[]):-!.
prolog_makelist(N,[N|T]):-
    N1 is N - 1,
    prolog_makelist(N1,T).


tc_rep_max :-
    catch(timed_call(jns_test:loop,
                     [repeating(100,jns_test:myrep_3),max(500,jns_test:mymax_3)]),
          _,
          writeln(finished_tc_rep_max)).

myrep_3:- writeln('tc_rep_max interrupt').

mymax_3:- abort('tc_3 phew!').

loop:- loop.

:- table table_comp/1.
table_comp(a).
table_comp(b).
table_comp(c).
table_comp(d).
table_comp(e):- unk(something).
table_comp(e):- unk(something_else).

test_comp(a).
test_comp(b).
test_comp(c).
test_comp(d).
test_comp(e):- unk(something).
test_comp(e):- unk(something_else).

test_comp(a,1).
test_comp(b,2).
test_comp(c,3).
test_comp(d,4).
test_comp(e,5):- unk(something).
test_comp(e,5):- unk(something_else).

:- table table_comp/2.
table_comp(a,1).
table_comp(b,2).
table_comp(c,3).
table_comp(d,4).
table_comp(e,5):- unk(something).
table_comp(e,5):- unk(something_else).

:- table unk/1.
unk(X):- tnot(unk(X)).

test_empty_set([]).

return_term(p([1],a,f(a),g(b))).

return_tuple('-'([1,a,'-'(a)],'-'(b,c,d),'-'())).

return_null([]).

qdet_variadic(a,b,c,d).

anti_member(List,Elt):- member(Elt,List).

am_3(_Tag,List,Elt):- member(Elt,List).

   det_bind_test(foo,1, 3.14 ,[foo,1,3.14],-(t1,t2),{foo:bar,bar:baz},pySet([1,2,3])).

   bind_test(foo,1, 3.14 ,[foo,1,3.14],-(t1,t2),{foo:bar,bar:baz},pySet([1,2,3])).
   bind_test(pySet([1,2,3]),foo,1, 3.14 ,[foo,1,3.14],-(t1,t2),{foo:bar,bar:baz}).
   bind_test({foo:bar,bar:baz},pySet([1,2,3]),foo,1, 3.14 ,[foo,1,3.14],-(t1,t2)).
   bind_test(-(t1,t2),{foo:bar,bar:baz},pySet([1,2,3]),foo,1, 3.14 ,[foo,1,3.14]).
   bind_test([foo,1,3.14],-(t1,t2),{foo:bar,bar:baz},pySet([1,2,3]),foo,1, 3.14 ).
   bind_test(3.14 ,[foo,1,3.14],-(t1,t2),{foo:bar,bar:baz},pySet([1,2,3]),foo,1).
   bind_test(1,3.14 ,[foo,1,3.14],-(t1,t2),{foo:bar,bar:baz},pySet([1,2,3]),foo).
