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

:- module(jns_utils,
          [ pp_py/1,                    % +Term
            pp_py/2,                    % +Stream, +Term
            values/3,                   % +Dict, +Path, ?Val
            keys/2,                     % +Dict, ?Keys
            key/2,                      % +Dict, ?Key
            items/2,                    % +Dict, ?Items
            obj_dir/2,                  % +ObjRef,-List
            obj_dict/2                  % obj_dict(+ObjRef, -Dict)
          ]).
:- autoload(library(apply), [maplist/3]).
:- autoload(library(dicts), [dict_keys/2]).
:- autoload(library(lists), [member/2]).
:- autoload(library(pprint), [print_term/2]).
:- autoload(library(janus), [py_call/2]).

/** <module> Janus utilities

This module provides the Janus utilities  defined   in  a library of the
same name for XSB. Most of these   predicates deal with accessing fields
in the XSB/Janus dict representation.  This   library  provides the same
predicates, both operating on SWI-Prolog dicts  and the current proposal
for XSB Janus dictionaries.
*/

%!  pp_py(+Term) is det.
%!  pp_py(+Stream,+Term) is det.
%
%   Pretty prints the Prolog translation of   a Python data structure in
%   Python-like  syntax.  The   current    implementation   simply  used
%   print_term/2. That is mostly ok, except   that  atoms are not quoted
%   and dicts that are printed as Prolog dicts, including the _tag_.

pp_py(Term) :-
    print_term(Term, [output(current_output)]).
pp_py(Stream, Term) :-
    print_term(Term, [output(Stream)]).

%!  values(+Dict, +Path, ?Val) is semidet.
%
%   Get the value associated with Dict at  Path. Path is either a single
%   key or a list of keys.

values(Dict, Key, Val), is_dict(Dict), atom(Key) =>
    get_dict(Key, Dict, Val).
values(Dict, Keys, Val), is_dict(Dict), is_list(Keys) =>
    get_dict_path(Keys, Dict, Val).
values(py({CommaDict}), Key, Val) =>
    comma_values(CommaDict, Key, Val).
values({CommaDict}, Key, Val) =>
    comma_values(CommaDict, Key, Val).

get_dict_path([], Val, Val).
get_dict_path([H|T], Dict, Val) :-
    get_dict(H, Dict, Val0),
    get_dict_path(T, Val0, Val).

comma_values(CommaDict, Key, Val), atom(Key) =>
    comma_value(Key, CommaDict, Val).
comma_values(CommaDict, Keys, Val), is_list(Keys) =>
    comma_value_path(Keys, CommaDict, Val).

comma_value(Key, Key:Val0, Val) =>
    Val = Val0.
comma_value(Key, (_,Tail), Val) =>
    comma_value(Key, Tail, Val).

comma_value_path([], Val, Val).
comma_value_path([H|T], Dict, Val) :-
    comma_value(H, Dict, Val0),
    comma_value_path(T, Val0, Val).

%!  keys(+Dict, ?Keys) is det.
%
%   True when Keys is a list of keys that appear in Dict.

keys(Dict, Keys), is_dict(Dict) =>
    dict_keys(Dict, Keys).
keys(py({CommaDict}), Keys) =>
    comma_dict_keys(CommaDict, Keys).
keys({CommaDict}, Keys) =>
    comma_dict_keys(CommaDict, Keys).

comma_dict_keys((Key:_,T), Keys) =>
    Keys = [Key|KT],
    comma_dict_keys(T, KT).
comma_dict_keys(Key:_, Keys) =>
    Keys = [Key].

%!  key(+Dict, ?Key) is nondet.
%
%   True when Key is a key in   Dict.  Backtracking enumerates all known
%   keys.

key(Dict, Key), is_dict(Dict) =>
    dict_pairs(Dict, _Tag, Pairs),
    member(Key-_, Pairs).
key(py({CommaDict}), Keys) =>
    comma_dict_key(CommaDict, Keys).
key({CommaDict}, Keys) =>
    comma_dict_key(CommaDict, Keys).

comma_dict_key((Key:_,_), Key).
comma_dict_key((_,T), Key) :-
    comma_dict_key(T, Key).

%!  items(+Dict, ?Items) is det.
%
%   True when Items is a list of Key:Value that appear in Dict.

items(Dict, Items), is_dict(Dict) =>
    dict_pairs(Dict, _, Pairs),
    maplist(pair_item, Pairs, Items).
items(py({CommaDict}), Keys) =>
    comma_dict_items(CommaDict, Keys).
items({CommaDict}, Keys) =>
    comma_dict_items(CommaDict, Keys).

pair_item(K-V, K:V).

comma_dict_items((Key:Value,T), Keys) =>
    Keys = [Key:Value|KT],
    comma_dict_items(T, KT).
comma_dict_items(Key:Value, Keys) =>
    Keys = [Key:Value].

%!  obj_dir(+ObjRef, -List) is det.
%!  obj_dict(+ObjRef, -Dict) is det.
%
%   Examine attributes of an object. The predicate obj_dir/2 fetches the
%   names of all attributes,  while  obj_dict/2   gets  a  dict with all
%   attributes and their values.

obj_dir(ObjRef, List) :-
    py_call(ObjRef:'__dir__'(), List0),
    maplist(atom_string, List, List0).

obj_dict(ObjRef, Dict) :-
    py_call(ObjRef:'__dict__', Dict).

