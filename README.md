# Janus: a bi-directional interface to Python

> This is explorative work.  This work is based on Janus for XSB by
> Theresa Swift and Carl Anderson.  That library is now called xsbpy.
> Nothing in this code base is settled.

This  code  implements  a  ready-to-use  bi-directional  interface  to
Python.  As  motivated by Theresa  Swift, Python opens many  doors for
accessing resources such as graphics, machine learning and many more.

While this code is motivated by Janus, the current interface is rather
different.  In part, that is due to the extended data types that allow
SWI-Prolog to define  a cleaner interface.  In part, this  is based on
experience with e.g., our JavaScript interface.


## Literature

  - https://docs.python.org/3/extending/extending.html
  - https://docs.python.org/2/c-api/exceptions.html
