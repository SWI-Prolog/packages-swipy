:- use_module(library(janus)).

here.
:- initialization
    source_file(here, File),
    file_directory_name(File, Dir),
    add_py_lib_dir(Dir, first).

bench_py_call(N) :-
    time(forall(between(1,N,_I),
                py_call(sumlist3:sumlist3(5,[1,2,3]), _L))).

bench_iter(N, Sum) :-
    time(py_call(prolog:bench_iter(N), Sum)).

bench_call_prolog(N) :-
    time(py_call(prolog:bench_call(N), _)).

bench_echo_list(N) :-
    numlist(1, N, L),
    time(py_call(prolog:echo(L), _)).

bench_pass_list(N) :-
    numlist(1, N, L),
    time(py_call(prolog:echo(L))).
