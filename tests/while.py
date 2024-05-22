from janus_swi import *

# Tests using :=

def test_while():
    list=[]
    with query("between(1,3,X)") as q:
        while ( s := q.next() ):
            list.append(s['X'])
    return list
