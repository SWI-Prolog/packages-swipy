% This file is in the public domain
:- module(russel, [shaves/2]).

:- table shaves/2.

shaves(barber,P) :- person(P),  tnot(shaves(P,P)).
person(barber).
person(mayor).
