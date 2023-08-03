```
?- py_call(spacy:load(en_core_web_sm),NLP).
NLP = <py_English>(0x7f6ede0d76a0).

?- py_call($NLP:'__call__'("This is a sentence"), Doc, [py_object(true)]).
Doc = <py_Doc>(0x7f6eda74a140),
NLP = <py_English>(0x7f6ede0d76a0).

?- py_iter($Doc:noun_chunks, Noun, [py_object]).
Noun = <py_Span>(0x7f6eda640510),
Doc = <py_Doc>(0x7f6eda74a140) ;
Noun = <py_Span>(0x7f6eda6409e0),
Doc = <py_Doc>(0x7f6eda74a140).

?- py_call($Noun:text, X).
```
