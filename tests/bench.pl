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
:- use_module(library(main)).
:- use_module(library(prolog_format)).

/** <module> Janus benchmarks
*/

:- py_add_lib_dir.

:- initialization(main, main).

main(Argv) :-
    argv_options(Argv, Pos, Options),
    option(count(Count), Options, 100 000),
    maplist(atom_number, Pos, Ids),
    (   Ids == []
    ->  bench(Count)
    ;   bench(Count, Ids)
    ).

opt_type(count, count, integer).
opt_type(c,     count, integer).
opt_meta(count, 'TIMES').
opt_help(count, "Number of times to iterate (default 100,000)").
opt_help(help(header),
         "Run Janus benchmarks\n").
opt_help(help(usage),
         " [options] [benchmark ...]").

%!  bench(+Count)
%
%   Run registered benchmarks scaled Count times.

bench(N) :-
    forall(benchmark(_, N), true).

bench(N, Ids) :-
    forall(member(Id, Ids), benchmark(Id, N)).

term_expansion((benchmark(N, Fmt-Args, forall) :- Body), Clause) :-
    term_expansion((benchmark(N, Fmt-Args, call) :- forall(between(1,N,_), Body)),
                   Clause).
term_expansion((benchmark(N, Fmt-Args, call) :- Body),
               (   benchmark(Id, N) :-
                       progress(Id, Fmt, Args),
                       call_time(Body, Time),
                       get_dict(cpu, Time, CPU),
                       PerSec is round(N/CPU),
                       format(' ~`.t ~D/s~78|~n', [PerSec]))) :-
    (   predicate_property(benchmark(_,_), number_of_clauses(NC))
    ->  Id is NC+1
    ;   Id = 1
    ).

benchmark(N,
          'Calling Python `int()` ~D times'-[N],
          forall) :-
    py_call(demo:int(), _).
benchmark(N,
          'Calling Python `sumlist3(5,[1,2,3])` ~D times'-[N],
          forall) :-
    py_call(demo:sumlist3(5,[1,2,3]), _).
benchmark(N,
          'Iterate over Prolog `query("between(1,M,X)", {"M":~d})`'-[N],
          call) :-
    py_call(demo:bench_query_iter(N), _).
benchmark(N,
          'Iterate over Prolog `apply("user","between",1,~d)`'-[N],
          call) :-
    py_call(demo:bench_apply_iter(N), _).
benchmark(N,
          'Iterate over Prolog `apply("user","between",1,2)` ~D times'-[N],
          forall) :-
    py_call(demo:bench_apply_iter(2), _).
benchmark(N,
          'Pass list [1..~D] to Python'-[N],
          call) :-
    numlist(1, N, L),
    py_call(demo:echo(L)).
benchmark(N,
          'Echo list [1..~D] to Python and back to Prolog'-[N],
          call) :-
    numlist(1, N, L),
    py_call(demo:echo(L), _).
benchmark(N,
          'Iterating over Python `range(0,~d)` from Prolog'-[N],
          call) :-
    forall(py_iter(range(0,N), _), true).
benchmark(N,
          'Call `query_once("Y is X+1", {"X":i})` from Python ~D times'-[N],
          call) :-
    py_call(demo:bench_call(N)).
benchmark(N,
          'Call `cmd("true")` ~D times'-[N],
          call) :-
    py_call(demo:bench_cmd(N)).
benchmark(N,
          'Call `apply_once("user", "=", 1)` ~D times'-[N],
          call) :-
    py_call(demo:bench_apply_once(N)).
benchmark(N,
          'Call `apply_once("user", "between", 1, 2)` ~D times'-[N],
          call) :-
    py_call(demo:bench_apply_oncea(N)).
benchmark(N,
          'Call `apply_once("user", "between", 1, 2, fail=0)` ~D times'-[N],
          call) :-
    py_call(demo:bench_apply_onceb(N)).

py_thread(Id) :-
    thread_self(Self),
    (   atom(Self)
    ->  Id = Self
    ;   thread_property(Self, id(Id))
    ).

%!  progress(+Id, +Fmt, +Args) is det.
%
%   As ansi_format/3, handling `code` style.

progress(Id, Fmt, Args) :-
    ansi_format(bold, '~t~w~3| ', [Id]),
    split_string(Fmt, "`", "", Parts),
    progress_(comment, Parts, Args).

progress_(Style, [H|T], Args) =>
    format_types(H, Types),
    same_length(Types, HArgs),
    append(HArgs, RArgs, Args),
    ansi_format(Style, H, HArgs),
    next_style(Style, Style1),
    progress_(Style1, T, RArgs).
progress_(_, [], _) =>
    true.

next_style(comment, code).
next_style(code, comment).
