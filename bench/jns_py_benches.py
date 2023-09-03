import sys
import time
from janus_swi import *

px_cmd('consult','ensure_loaded','jns_test')

def test_iterations():
    pp_iteration(test_iteration_cmd,200000)
    pp_iteration(test_iteration_qdet,200000)
    pp_iteration(test_iteration_qdet_nd,200000)
    pp_iteration(test_comp_no_tvs,50000)
    pp_iteration(test_comp_default_tvs,50000)
    pp_iteration(test_comp_delay_lists,50000)
    py_to_xsb_list_xfer(1000000)        
    xsb_to_py_list_xfer(1000000)        

    # once() and Query() based versions
    pp_iteration(test_iteration_cmd_once,200000)
    pp_iteration(test_iteration_qdet_once,200000)
    pp_iteration(test_iteration_qdet_nd_once,200000)
    pp_iteration(test_comp_default_tvs_query,50000)


# ============= Iteration Code  =============
def test_iteration_cmd(N):
    for i in range(1,N):
        px_cmd('jns_test','simple_cmd',N)

def test_iteration_cmd_once(N):
    for i in range(1,N):
        once('jns_test:simple_cmd(N)', {'N': N})

# deterministic query        
def test_iteration_qdet(N):
    for i in range(1,N):
        px_qdet('jns_test','simple_call',N)
    
def test_iteration_qdet_once(N):
    for i in range(1,N):
        once('jns_test:simple_call(N, R)',{'N':N})

def test_iteration_qdet_nd(N):
    for i in range(1,N):
        px_qdet('jns_test','nondet_query')

def test_iteration_qdet_nd_once(N):
    for i in range(1,N):
        once('jns_test:nondet_query(R)')

# Passes back 6 solutions of 2 vars w. default tv        
def test_comp_default_tvs(N):
    for i in range(1,N):
        px_comp('jns_test','table_comp',vars=2)

def test_comp_default_tvs_query(N):
    for i in range(1,N):
        [*Query('jns_test:table_comp(X,Y)')]

# Passes back 6 solutions of 2 vars w. no tvs        
def test_comp_no_tvs(N):
    for i in range(1,N):
        px_comp('jns_test','table_comp',vars=2,truth_vals=NO_TRUTHVALS)

# Passes back 6 solutions of 2 vars w. delay_lists
def test_comp_delay_lists(N):
    for i in range(1,N):
        px_comp('jns_test','table_comp',vars=2,truth_vals=DELAY_LISTS)

def pp_iteration(test_func,argument):
    Start = time.time()
    test_func(argument)
    End = time.time()
    print(test_func.__name__+'('+str(argument)+') succeeded')
    PerSec = argument/(End-Start)
    print('# Time: {:.3f} secs; {:_.0f} per sec.'.format(End-Start,PerSec))
    print('')    

def py_to_xsb_list_xfer(N):
    mylist = makelist(N)
#    print('getting the length of List = makelist(100000)')
    start = time.time()
    px_qdet('basics','length',mylist)
    end = time.time()
    PerSec = N/(end-start)
    print('py_to_xsb_list_xfer succeded: '+str(N))
    print('# Time: {:.3f} secs; {:_.0f} per sec.'.format(end-start,PerSec))
    print('')

def xsb_to_py_list_xfer(N):
#    print('calling prolog_makelist(1000000)')
    start = time.time()
    px_qdet('jns_test','prolog_makelist',N)    
    end = time.time()
    PerSec = N/(end-start)
    print('xsb_to_py_list_xfer succeded: '+str(N))
    print('# Time: {:.3f} secs; {:_.0f} per sec.'.format(end-start,PerSec))
    print('')
    
def makelist(N):
    list = []
    for i in range(1,N):
        list.append(i)
    return list


test_iterations()
