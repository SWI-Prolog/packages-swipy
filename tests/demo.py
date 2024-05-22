# This Python module defines some functions that support our tests

from janus_swi import query, query_once, PLAIN_TRUTHVALS, PrologError
import janus_swi as janus

# Simple data exchange

def int():
    return 42;

def hello_world():
    return "Hello World"

def isstr(s):
    """Boolean test whether s is a string"""
    if isinstance(s, str):
        return True
    else:
        return False

def dict0():
    return {}

def dict1():
    return {"a":1, 2:2}

def dict2():
    return {"a":1, 2.3:2}

def echo(d):
    return d

def echo_v(d):
    print(d)
    return d

def tuple():
    return (1,2)

# Unicode support

def воздуха():
    return "воздуха"

def schloß():
    return "schloß"

# Simple function calls

def multiply(a,b):
    return a*b;

def lshift(i, n):
    return i<<n

def concat(a,b):
    return a+b;

def sumlist3(X,Y):
    Z = []
    for element in Y:
        Z.append(X+element)
    return Z

# Handle Python keyword arguments

def kwd(a, b, c=3):
    return {"a":a, "b":b, "c":c}

def kwd_all(a=1, b=2, c=3):
    return {"a":a, "b":b, "c":c}

# Demo for dealing with iterators

def abort_iter(n):
    sum=0
    with query("between(1,M,X)", {"M":n}) as d_q:
        for d in d_q:
            sum += d["X"]
            if sum > 10000:
                break;
    return sum;

def double_iter(w,h):
    tuples=[]
    with query("between(1,M,Y)", {"M":h}) as yd_q:
        for yd in yd_q:
            with query("between(1,M,X)", {"M":w}) as xd_q:
                for xd in xd_q:
                    tuples.append((xd['X'],yd['Y']))
    return tuples

def test_invalid_nesting():
    rc = True
    q1 = query("between(1,3,X)")
    q2 = query("between(1,3,Y)")
    next(q2)
    try:
        rc = next(q1)
    except PrologError:
        q2.close()
        rc = False
    next(q1)
    q1.close()
    return rc

def test_invalid_nesting2():
    with (query("between(1,3,X)") as q1,
          query("between(1,3,Y)") as q2):
        next(q2)
        try:
            return next(q1)
        except Exception as exc:
            if not isinstance(exc, PrologError):
                return "Not instance of PrologError: " + repr(exc)
            return exc
    return False  # Shouldn't get here

def test_invalid_nesting3():
    with (query("between(1,3,X)") as q1,
          query("between(1,3,Y)") as q2):
        next(q2)
        return next(q1)


# Test using generators as custom iterators.

def squares(start, stop):
    for i in range(start, stop):
        yield i * i

# Test for WFS

def shaves(truth_vals=PLAIN_TRUTHVALS):
    return [*query("russel:shaves(X,Y)", truth_vals=truth_vals)]

# Benchmarking support

def bench_query_iter(n):
    sum=0
    for d in query("between(1,M,X)", {"M":n}):
        sum += d["X"]
    return sum;

def bench_apply_iter(n):
    sum=0
    for d in apply("user", "between", 1, n):
        sum += d
    return sum;

def bench_call(n):
    for i in range(1,n):
        query_once("Y is X+1", {"X":i})

def bench_call_v(n):
    for i in range(1,n):
        print((i, query_once("py_thread(T)", {})["T"]))

def bench_apply_once(n):
    sum=0
    for i in range(1,n+1):
        sum = sum + apply_once("user", "=", i)
    return sum

def bench_apply_oncea(n):
    for i in range(1,n+1):
        apply_once("user", "between", 1, 2)

def bench_apply_onceb(n):
    for i in range(1,n+1):
        apply_once("user", "between", 1, 2, fail=0)

def bench_cmd(n):
    """Call `true` `n` times"""
    for i in range(1,n):
        cmd("user", "true")

# Simple counter class

class Counter:
    def __init__(self):
        self.count = 0
    def increment(self):
        self.count += 1

# Tester for Python object garbage collection.

gced = 0

class GCAble:
    def __init__(self):
        self.created = True;
    def __del__(self):
        global gced
        gced += 1

# Test exceptions and terms

def call_except(query, input={}):
    """Call query, return the exception or none"""
    try:
        query_once(query, input)
    except Exception as ex:
        return ex
    return None

def call_try(n, query, input={}):
    """Call query `n` times, eating exceptions"""
    count = 0
    for i in range(0,n):
        try:
            query_once(query, input)
        except Exception as ex:
            count = count+1
    return count

def try_catch():
    try:
        query_once("A is 1 / 0")
    except PrologError as e:
        return True
    except Exception as e:
        return e

# Test locals() and globals()

g1 = 23
g2 = "nice"

def with_gvars(a1):
    global g1
    l1 = 46
    return query_once(
        """py_call(globals(), Globals)
        """)

def with_vars(a1):
    global g1
    l1 = 46
    return query_once(
        """py_call(locals(), Locals),
           py_call(globals(), Globals)
        """)

# Some examples from the documentation

def doc_sqrt():  # returns: {'truth': True, 'Y': 1.4142135623730951}
    return janus.query_once("Y is sqrt(X)", {'X':2})

def doc_user_plus():  # returns: 3
    return janus.apply_once("user", "plus", 1, 2)

def doc_y_is_x_plus_1():  # returns: {'Y': 2, 'truth': True}
    return janus.query_once("Y is X+1", {"X":1})

# TODO: this needs a definition of parent/2:
def doc_findall_parent():  # returns: [ 'Kees', 'Jan' ]
    return janus.query_once(
        "findall(_GP, parent(Me, _P), parent(_P, _GP), GPs)""",
         {'Me':'Jan'})["GPs"]

def doc_has_big_integers():
    return janus.query_once("current_prolog_flag(bounded,false)")['truth']

# doc_printRange() isn't tested; doc_getRange() is similar, returning a list
def doc_printRange(fr, to):
    for d in janus.query("between(F,T,X)", {"F":fr, "T":to}):
        print(d["X"])

def doc_printRange2(fr, to):
    with janus.query("between(F,T,X)", {"F":fr, "T":to}) as d_q:
        for d in d_q:
            print(d["X"])

def doc_getRange(fr, to):
    return [d["X"] for d in janus.query("between(F,T,X)", {"F":fr, "T":to})]

def doc_getRange2(fr, to):
    result = []
    with janus.query("between(F,T,X)", {"F":fr, "T":to}) as d_q:
        for d in d_q:
            result.append(d["X"])
    return result

def doc_double_iter(w,h):  # double_iter(2,3) => [(1, 1), (2, 1), (1, 2), (2, 2), (1, 3), (2, 3)]
                           #          in Prolog: [1-1, 2-1, 1-2, 2-2, 1-3, 2-3)]
    tuples=[]
    for yd in janus.query("between(1,M,Y)", {"M":h}):
        for xd in janus.query("between(1,M,X)", {"M":w}):
            tuples.append((xd['X'],yd['Y']))
    return tuples

def doc_double_iter2(w,h):  # double_iter(2,3) => [(1, 1), (2, 1), (1, 2), (2, 2), (1, 3), (2, 3)]
                            #          in Prolog: [1-1, 2-1, 1-2, 2-2, 1-3, 2-3)]
    tuples=[]
    with janus.query("between(1,M,Y)", {"M":h}) as yd_q:
        for yd in yd_q:
            with janus.query("between(1,M,X)", {"M":w}) as xd_q:
                for xd in xd_q:
                    tuples.append((xd['X'],yd['Y']))
    return tuples

# doc_print_between() isn't tested; doc_get_between() is similar, returning a list
def doc_print_between():
    try:
        q = query("between(1,3,X)")
        while ( s := q.next() ):
            print(s['X'])
    finally:
        q.close()

def doc_get_between():
    result = []
    try:
        q = query("between(1,3,X)")
        while ( s := q.next() ):
            result.append(s['X'])
    finally:
        q.close()
    return result

def doc_apply_between():  # returns: [1, 2, 3, 4, 5, 6]
    return list(janus.apply("user", "between", 1, 6))

def doc_between_1_6():  # returns: [1, 2, 3, 4, 5, 6]
    return [a["D"] for a in query("between(1,6,D)")]
