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

import time
import gc

def makelist(N):
    start = time.time()
    retlist = []
    for i in range(1,N):
        retlist.append(i)
    end = time.time()
    print("python_done: " + str(end-start) + " seconds")
    return retlist

def gclen():
    return(len(gc.get_objects()))

def get_garbage():
    return(gc.garbage)

#py_func(gc,get_stats(),F).

class StressClass:
    def __init__(self):
        self.list = []

    def func0(self,N):
        start = time.time()
        for i in range(1,N):
            self.list.append(i)
        end = time.time()
        print("python_done: " + str(end-start) + " seconds")
        return self.list

