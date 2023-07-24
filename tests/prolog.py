import swipl
from janus import Prolog

# Demo for dealing with iterators

def bench_iter(n):
    sum=0
    for d in Prolog("between(1,M,X)", {"M":n}):
        sum += d["X"]
    return sum;

def abort_iter(n):
    sum=0
    for d in Prolog("between(1,M,X)", {"M":n}):
        sum += d["X"]
        if sum > 10000:
            break;
    return sum;

def bench_call(n):
    for i in range(1,n):
        swipl.call("Y is X+1", {"X":i})


def echo(d):
    return d
