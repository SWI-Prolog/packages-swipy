# This Python module defines some functions that support our tests

from janus import *

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
    for d in query("between(1,M,X)", {"M":n}):
        sum += d["X"]
        if sum > 10000:
            break;
    return sum;

def double_iter(w,h):
    tuples=[]
    for yd in query("between(1,M,Y)", {"M":h}):
        for xd in query("between(1,M,X)", {"M":w}):
            tuples.append((xd['X'],yd['Y']))
    return tuples

def test_invalid_nesting():
    q1 = query("between(1,3,X)")
    q2 = query("between(1,3,Y)")
    q2.next()
    try:
        q1.next()
    except:
        q2.close()
    q1.next()
    q1.close()
    return True

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

