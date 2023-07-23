# This Python module defines some functions that support our tests

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

def trivial(val):
    return val;

def trivial_v(val):
    print(val);
    return val;

def tuple():
    return (1,2)

def kwd(a, b, c=3):
    return {"a":a, "b":b, "c":c}
