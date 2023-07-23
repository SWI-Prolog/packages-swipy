import swipl

class Prolog:
    def __init__(self, query, inp):
        self.state = swipl.open_query(query, inp)
    def __iter__(self):
        return self
    def __next__(self):
        rc = swipl.next_solution(self.state)
        if rc == False:
            raise StopIteration()
        else:
            return rc
    def __del__(self):
        swipl.close_query(self.state)

def once(query, inputs={}):
    return swipl.call(query, inputs)

def consult(file):
    once("consult(File)", {"File":file})

def interact():
    import code
    code.InteractiveConsole(locals=globals()).interact();

    
