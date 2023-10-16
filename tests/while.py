from janus import *

# Tests using :=

def test_while():
    list=[]
    q = query("between(1,3,X)")
    while ( s := q.next() ):
        list.append(s['X'])
    q.close()
    return list
