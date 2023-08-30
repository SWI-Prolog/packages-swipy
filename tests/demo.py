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

def bench_iter(n):
    sum=0
    for d in Query("between(1,M,X)", {"M":n}):
        sum += d["X"]
    return sum;

def abort_iter(n):
    sum=0
    for d in Query("between(1,M,X)", {"M":n}):
        sum += d["X"]
        if sum > 10000:
            break;
    return sum;

def double_iter(w,h):
    tuples=[]
    for yd in Query("between(1,M,Y)", {"M":h}):
        for xd in Query("between(1,M,X)", {"M":w}):
            tuples.append((xd['X'],yd['Y']))
    return tuples

def test_invalid_nesting():
    q1 = Query("between(1,3,X)")
    q2 = Query("between(1,3,Y)")
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

def shaves():
    return [*Query("russel:shaves(X,Y)")]

# Benchmarking support

def bench_call(n):
    for i in range(1,n):
        once("Y is X+1", {"X":i})

def bench_call_v(n):
    for i in range(1,n):
        print((i, once("py_thread(T)", {})["T"]))

def bench_px_cmd(n):
    """Call `true` `n` times"""
    for i in range(1,n):
        px_cmd("user", "true")

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
        once(query, input)
    except Exception as ex:
        return ex
    return None

def call_try(n, query, input={}):
    """Call query `n` times, eating exceptions"""
    count = 0
    for i in range(0,n):
        try:
            once(query, input)
        except Exception as ex:
            count = count+1
    return count
