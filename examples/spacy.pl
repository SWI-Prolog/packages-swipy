% Preparation
%
%    pip install spacy
%    python -m spacy download en

:- use_module(library(janus)).

:- table english/1.

english(NLP) :-
    py_call(spacy:load(en_core_web_sm),NLP).

noun(Sentence, Noun) :-
    english(NLP),
    py_call(NLP:'__call__'(Sentence), Doc, [py_object(true)]),
    py_iter(Doc:noun_chunks, Span, [py_object]),
    py_call(Span:text, Noun, [py_string_as(atom)]).
