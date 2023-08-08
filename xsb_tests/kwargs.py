
def kwargs_append(X,**Features):
    List = [X]
    for (key,value) in Features.items():
        List.append((key,value))
    return(List)
    
    
