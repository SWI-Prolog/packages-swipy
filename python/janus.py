import swipl

################################################################
# Primary high level interface

class Query:
    """
    Class `Query` implements an _iterator_ over a Prolog goal.

    Attributes
    ----------
    query: str
        A string representing a Prolog goal.
    inputs: dict
        Bind variables of the goal on input with the converted
        Python data from this dict.
    """
    def __init__(self, query, inputs={}):
        self.state = swipl.open_query(query, inputs)
    def __iter__(self):
        return self
    def __next__(self):
        rc = swipl.next_solution(self.state)
        if rc == False or rc["status"] == False:
            raise StopIteration()
        else:
            return rc
    def __del__(self):
        swipl.close_query(self.state)

def once(query, inputs={}, keep=False):
    """
    Call a Prolog predicate as `once/1`

    Parameters
    ----------
    query: str
        A string representing a Prolog goal.
    inputs: dict
        Bind variables of the goal on input with the converted
        Python data from this dict.
    keep: bool, optional
        It `True` (default `False`), do not _backtrack_.  May
        be used to preserve changes to global variables using
        `b_setval/2`.
    """
    return swipl.call(query, inputs, keep)

def consult(file):
    """
    Consult a Prolog file.
    """
    once("consult(File)", {"File":file})

def interact():
    """
    Used by `py_shell/0` to create an interactive Python session.
    Attempts to initialize readline to provide command line
    editing.
    """
    import code
    vars = globals()
    vars.update(locals())
    try:
        import readline
        import rlcompleter
        readline.set_completer(rlcompleter.Completer(vars).complete)
        readline.parse_and_bind("tab: complete")
    except:
        pass
    code.InteractiveConsole(vars).interact();

################################################################
# Emulated XSB interface

def px_cmd(module, pred, *args):
    once("janus:px_cmd(M,P,Args)", {"M":module, "P":pred, "Args":args})

def _xsb_tv(status):
    if status == True:
        return 1
    elif status == False:
        return 0
    else:
        return 2

def px_qdet(module, pred, *args):
    d = once("janus:px_qdet(M,P,Args,Ret)", {"M":module, "P":pred, "Args":args})
    return (d["Ret"], _xsb_tv(d["status"]))

PLAIN_TRUTHVALS="plain"
DELAY_LISTS="delays"
NO_TRUTHVALS="none"

def px_comp(module, pred, *args, vars=1, set_collect=False, truth_vals=PLAIN_TRUTHVALS):
    d = once("janus:px_comp(M,P,Args,Vars,Set,TV,Ret)",
             { "M":module,
               "P":pred,
               "Args":args,
               "Vars":vars,
               "Set":set_collect,
               "TV":truth_vals
              })
    return d["Ret"]
