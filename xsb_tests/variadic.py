
def variadic_print(*args):
    print('variadic_print',end=': ')
    ret = ''
    for arg in args:
        ret = ret + arg + '|'
    return ret

def opt_print(Arg,OptArg=1):
    return(Arg + '|' + str(OptArg))

#wont work -- Python wont allow
#def breakit(Arg1,OptArg=1,Arg2):
#    print(Arg1)
#    print(OptArg)
#    print(Arg2)
#    return True


