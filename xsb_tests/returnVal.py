# Copyright 2023 Theresa Swift and Carl Anderson
#
# Permission is hereby granted, free of charge,  to any person obtaining a
# copy  of  this  software  and    associated   documentation  files  (the
# “Software”), to deal in  the   Software  without  restriction, including
# without limitation the rights to  use,   copy,  modify,  merge, publish,
# distribute, sublicense, and/or sell  copies  of   the  Software,  and to
# permit persons to whom the Software is   furnished  to do so, subject to
# the following conditions:
#
# The above copyright notice and this  permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT  WARRANTY OF ANY KIND, EXPRESS
# OR  IMPLIED,  INCLUDING  BUT  NOT   LIMITED    TO   THE   WARRANTIES  OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR   PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS  OR   COPYRIGHT  HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY,  WHETHER   IN  AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM,  OUT  OF   OR  IN  CONNECTION  WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

def returnVal(X):
	return X

def returnSet():
        empty = set()
        X = ['"foo"',"'bar'",{1,"hello",('a','b',7)}]
        return(X)

def easy_returnSet():
        X = {1,"hello",('a','b',7)}
        return(X)


def return_None():
        return(None)

def return_True():
        return(True)

def return_pi():
        return(3.14159)

def print_elt(Elt):
        print(type(Elt))
        print(str(Elt))
        return(Elt)

def return_False():
        return(False)

def return_apostrophe_1():
        return("I'm a doofus")

def return_apostrophe_2():
    return("Hello my name is " + 'John' + " and I'm a " + "doofus")

def return_dictionary():
        return {'Name': 'Geeks', 1: [1, 2, 3, 4]}
