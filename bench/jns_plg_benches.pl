
:- ensure_loaded(janus).

bench:-
    writeln('--------- func_incr(t00000) ---------'),
    bench_call(func_incr(500000)),
    writeln('--------- pyfunc_list(1000000) ---------'),
    bench_call(pyfunc_list(1000000)),
    writeln('--------- pyiter_list(1000000) ---------'),
    bench_call(py_iter_list(1000000)).

bench_call(Call):-     
    arg(1,Call,N),
    cputime(Start),walltime(_WStart),
    Call,
    cputime(End),walltime(_WEnd),
    CPU is End-Start,
    PerSec is N/CPU,
    write('% Time: '),write_term(CPU,[float_precision(8)]),
    write(' ; '),write_term(PerSec,[float_precision(8)]), writeln(' Per sec.').

func_incr(0):- !.
func_incr(Num):-
    pyfunc(jns_plg_benches,incr(Num),_),
    Num1 is Num-1,
    func_incr(Num1).

pyfunc_list(N):-
    pyfunc(jns_plg_benches,makelist_ret(N),_F).

py_iter_list(N):-
    py_iter(jns_plg_benches,makelist_ret(N),_F),
    fail.
py_iter_list(_N).

