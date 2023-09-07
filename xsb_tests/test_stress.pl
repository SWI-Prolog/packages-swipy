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

:- module(test_stress,
          [ test_stress/0,
            testClassList/2
          ]).

here.
:- initialization
    source_file(here, File),
    file_directory_name(File, Dir),
    py_add_lib_dir(Dir, first).

test_stress :-
    N is 1 000 000,
    Max is N-1,
    testClassList(1 000 000, List),
    numlist(1, Max, List2),
    assertion(List == List2).

testClassList(N,Out):-
    py_func(stressTest,'StressClass'(),Obj),
    py_dot(stressTest,Obj,func0(N),Out).
