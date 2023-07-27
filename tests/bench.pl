:- use_module(library(janus)).

here.
:- initialization
    source_file(here, File),
    file_directory_name(File, Dir),
    add_py_lib_dir(Dir, first).

bench(N) :-
    bench_py_call(N),
    bench_iter(N, _),
    bench_call_prolog(N),
    bench_echo_list(N),
    bench_pass_list(N).

bench_py_call(N) :-
    ansi_format(bold, 'Calling python function ~D times~n', [N]),
    time(forall(between(1,N,_I),
                py_call(demo:int(), _L))).

bench_iter(N, Sum) :-
    ansi_format(bold, 'Iterate over Prolog goal with ~D answers~n', [N]),
    time(py_call(demo:bench_iter(N), Sum)).

bench_call_prolog(N) :-
    ansi_format(bold, 'Call Prolog predicate from Python ~D times~n', [N]),
    time(py_call(demo:bench_call(N), _)).

bench_pass_list(N) :-
    ansi_format(bold, 'Pass list with ~D integers to Python~n', [N]),
    numlist(1, N, L),
    time(py_call(demo:echo(L))).

bench_echo_list(N) :-
    ansi_format(bold, 'Echo list with ~D integers to Python~n', [N]),
    numlist(1, N, L),
    time(py_call(demo:echo(L), _)).

py_thread(Id) :-
    thread_self(Self),
    (   atom(Self)
    ->  Id = Self
    ;   thread_property(Self, id(Id))
    ).
