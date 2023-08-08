:- module(test_stress,
          [ test_stress/0,
            testClassList/2
          ]).

here.
:- initialization
    source_file(here, File),
    file_directory_name(File, Dir),
    add_py_lib_dir(Dir, first).

test_stress :-
    N is 1 000 000,
    Max is N-1,
    testClassList(1 000 000, List),
    numlist(1, Max, List2),
    assertion(List == List2).

testClassList(N,Out):-
    pyfunc(stressTest,'StressClass'(),Obj),
    pydot(stressTest,Obj,func0(N),Out).
