
:- ensure_loaded(xsbpy).

testClassList(N,Out):-
    pyfunc(stressTest,'StressClass'(),Obj),
    pymeth(stressTest,Obj,func0(N),Out).
