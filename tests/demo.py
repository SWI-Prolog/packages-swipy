# This Python module defines some functions that support our tests

from janus import *

# Simple data exchange

def int():
    return 42;

def str():
    return "Hello World"

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
