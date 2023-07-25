from janus import *

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
        gced += 1
