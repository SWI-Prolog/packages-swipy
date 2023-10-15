/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2023, SWI-Prolog Solutions b.v.
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

:- module(bench,
          [ bench/1                     % +Count
          ]).
:- use_module(library(janus)).
:- use_module(library(ansi_term)).
:- use_module(library(lists)).
:- use_module(library(statistics)).
:- use_module(library(apply_macros)).

/** <module> Janus benchmarks
*/

here.
:- initialization
    source_file(here, File),
    file_directory_name(File, Dir),
    py_add_lib_dir(Dir, first).

%!  bench(+Count)
%
%   Run registered benchmarks scaled Count times.

bench(N) :-
    forall(benchmark(N), true).

term_expansion((benchmark(N, Fmt-Args, forall) :- Body), Clause) :-
    term_expansion((benchmark(N, Fmt-Args, call) :- forall(between(1,N,_), Body)), Clause).
term_expansion((benchmark(N, Fmt-Args, call) :- Body),
               (   benchmark(N) :-
                       ansi_format(comment, Fmt, Args),
                       call_time(Body, Time),
                       get_dict(cpu, Time, CPU),
                       format('~t ~3f sec~70|~n', [CPU]))).

benchmark(N,
          'Calling python int() ~D times'-[N],
          forall) :-
    py_call(demo:int(), _).
benchmark(N,
          'Calling python sumlist3(5,[1,2,3]) ~D times'-[N],
          forall) :-
    py_call(demo:sumlist3(5,[1,2,3]), _).
benchmark(N,
          'Iterate over Prolog Query("between(1,M,X)", {"M":~d})'-[N],
          call) :-
    py_call(demo:bench_iter(N), _).
benchmark(N,
          'Pass list [1.. ~D] to Python'-[N],
          call) :-
    numlist(1, N, L),
    py_call(demo:echo(L)).
benchmark(N,
          'Echo list [1.. ~D] to Python and back to Prolog'-[N],
          call) :-
    numlist(1, N, L),
    py_call(demo:echo(L), _).
benchmark(N,
          'Iterating over Python range(0,~d) from Prolog'-[N],
          call) :-
    forall(py_iter(range(0,N), _), true).
benchmark(N,
          'Call once("Y is X+1", {"X":i}) from Python ~D times'-[N],
          call) :-
    py_call(demo:bench_call(N)).
benchmark(N,
          'Call px_cmd("true") ~D times'-[N],
          call) :-
    py_call(demo:bench_px_cmd(N)).
benchmark(N,
          'Call apply1("user", "=", 1) ~D times'-[N],
          call) :-
    py_call(demo:bench_apply1(N)).
benchmark(N,
          'Call apply1("user", "between", 1, 2) ~D times'-[N],
          call) :-
    py_call(demo:bench_apply1a(N)).
benchmark(N,
          'Call apply1("user", "between", 1, 2, fail=0) ~D times'-[N],
          call) :-
    py_call(demo:bench_apply1b(N)).

py_thread(Id) :-
    thread_self(Self),
    (   atom(Self)
    ->  Id = Self
    ;   thread_property(Self, id(Id))
    ).
