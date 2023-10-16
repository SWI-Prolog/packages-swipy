# This example shows that we can run multiple Prolog threads concurrently

import janus_swi as janus
import threading

def one():
    print("Starting one")
    plone()

def plone():    
    x = janus.query_once("""
    thread_self(_Me),
    thread_property(_Me, id(Me)),
    writeln(start(Me)),
    forall(between(1,20,_X),
           (between(1, 1 000 000, _), writeln(_X)))
    """)
    print(f"Completed {x['Me']}")

def concurrent():
    t1 = threading.Thread(target=one)
    t2 = threading.Thread(target=one)
    t1.start()
    t2.start()
    t1.join()
    t2.join()

concurrent()
