import sys
sys.path.insert(0,'../../XSB/packages/janus/janus-py')
from janus import *

jns_cmd('consult','ensure_loaded','jns_test')

def test_iterations():
    pp_iteration(test_iteration_cmd,200000)
    pp_iteration(test_iteration_qdet,200000)
    pp_iteration(test_iteration_qdet_nd,200000)
    pp_iteration(test_comp_no_tvs,50000)
    pp_iteration(test_comp_default_tvs,50000)
    pp_iteration(test_comp_delay_lists,50000)
    py_to_xsb_list_xfer(1000000)        
    xsb_to_py_list_xfer(1000000)        

# ============= Iteration Code  =============
def test_iteration_cmd(N):
    for i in range(1,N):
        jns_cmd('jns_test','simple_cmd',N)

# deterministic query        
def test_iteration_qdet(N):
    for i in range(1,N):
        jns_qdet('jns_test','simple_call',N)
    
def test_iteration_qdet_nd(N):
    for i in range(1,N):
        jns_qdet('jns_test','nondet_query')

# Passes back 6 solutions of 2 vars w. default tv        
def test_comp_default_tvs(N):
    for i in range(1,N):
        jns_comp('jns_test','table_comp',vars=2)

# Passes back 6 solutions of 2 vars w. no tvs        
def test_comp_no_tvs(N):
    for i in range(1,N):
        jns_comp('jns_test','table_comp',vars=2,truth_vals=NO_TRUTHVALS)

# Passes back 6 solutions of 2 vars w. delay_lists
def test_comp_delay_lists(N):
    for i in range(1,N):
        jns_comp('jns_test','table_comp',vars=2,truth_vals=DELAY_LISTS)

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
    jns_qdet('basics','length',mylist)
    end = time.time()
    PerSec = N/(end-start)
    print('py_to_xsb_list_xfer succeded: '+str(N))
    print('# Time: {:.3f} secs; {:_.0f} per sec.'.format(end-start,PerSec))
    print('')

def xsb_to_py_list_xfer(N):
#    print('calling prolog_makelist(1000000)')
    start = time.time()
    jns_qdet('jns_test','prolog_makelist',N)    
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
