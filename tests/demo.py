# This Python module defines some functions that support our tests

from janus import *

def int():
    return 42;

def str():
    return "Hello World"

def dict1():
    return {"a":1, 2:2}

def dict2():
    return {"a":1, 2.3:2}

def multiply(a,b):
    return a*b;

def concat(a,b):
    return a+b;

def tuple():
    return (1,2)

def kwd(a, b, c=3):
    return {"a":a, "b":b, "c":c}

def sumlist3(X,Y):
    Z = []
    for element in Y:
        Z.append(X+element)
    return Z

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

def bench_call(n):
    for i in range(1,n):
        once("Y is X+1", {"X":i})

def echo(d):
    return d

def echo_v(d):
    print(d)
    return d

class Counter:
    def __init__(self):
        self.count = 0
    def increment(self):
        self.count += 1

gced = 0

class GCAble:
    def __init__(self):
        self.created = True;
    def __del__(self):
        global gced
#       if ( gced == 0 ):
#           print(self)
        gced += 1

