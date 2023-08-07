from janus import *

# Tests using :=

def test_while():
    list=[]
    q = Query("between(1,3,X)")
    while ( s := q.next() ):
        list.append(s['X'])
    q.close()
    return list
