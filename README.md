# Janus: a bi-directional interface to Python

This  code  implements  a  ready-to-use  bi-directional  interface  to
Python.  As  motivated by Theresa  Swift, Python opens many  doors for
accessing resources such as graphics, machine learning and many more.

The  API defined  in this  interface has  been established  as a  PIP,
_Prolog Improvement Proposal_.  When the PIP is finished and published
we  will  properly  reference  it.  The  main  predicates  and  Python
functions of this interface are compatible with the XSB Python package
`janus_xsb`.   Both `janus_swi`  and `janus_xsb`  implement extensions
upon  the   agreed  interface.   For  example,   `janus_swi`  supports
SWI-Prolog dicts and defines thread synchronization between Prolog and
Python.

## Installing as Prolog library

This  is  normally a  GIT  sub  module  of  the SWI-Prolog  git  repo.
Configuration and installation of `library(janus)` which embeds Python
into Prolog is  handled by the normal  Prolog configuration.  Building
the  interface  requires  the  libraries  and  C  headers  for  Python
embedding to be installed.  On __Debian__ based Linux systems, this is
achieved using

    apt install python3 libpython3-dev

If  you need  to  build  Python, the  following  command is  suggested
(assuming you wish to install  it in `$HOME/.local/bin`). You may also
need the option `--enable-shared`.

    CFLAGS='-fPIC' CCSHARED='-fPIC' ./configure --prefix=$HOME/.local --enable-optimizations
    make -j8   # change "8" to the number of CPUs on your machine
    make install

On __MacOS__,  these files are  included in the Homebrew  and Macports
versions of Python

On  Windows,  these  files  are included  in  the  default  installer.
Configuration requires Python to appear in ``%PATH%``.

After successful installation, running `py_version/0` should result in
printing relevant information on the embedded Python system.

    ?- py_version.
	% Janus embeds Python 3.10.12 (main, Jun 11 2023, 05:26:28) [GCC 11.4.0

### Embedding Prolog into Python

This repo may be  installed as a Python package such  that you can run
e.g.,

    python
	>>> import janus_swi as janus
	>>> janus.query_once("writeln('Hello world!')")
	Hello world!
	{'truth': True}
	>>>

To install  the package you  need `pip` with  a C compiler.   On Linux
this is  probably provided by  default by installing `pip`.   On MacOS
you need  to install Xcode.  On  Windows, it probably depends  how you
installed    CPython.    Assuming    the   binary    installers   from
https://www.python.org/downloads/windows/,   you   need   to   install
Microsoft Visual C++.   If this is not installed, `pip`  will tell you
and give a link from where to download Microsoft Visual C++.

Next, you  need to  make sure  `swipl` can  be found.   The `setup.py`
script first  tries to find  `swipl` (`swipl.exe` on Windows)  on your
application search path.  You can  verify that by running `swipl` from
a terminal.   If `swipl`  cannot be  found or it  is not  the expected
version of  SWI-Prolog, adjust your ``PATH``.   On Windows, `setup.py`
also checks the registry to find  SWI-Prolog as it is installed by the
default installation.  If  this works, SWI-Prolog does not  need to be
in ``%PATH%``.

If this is all in place, you can download this repo and install it, as
in

    git clone https://github.com/SWI-Prolog/packages-swipy.git swipy
	cd swipy
	pip install .

You can also do this using the one-liner below.

    pip install git+https://github.com/SWI-Prolog/packages-swipy.git#egg=janus_swi


## Documentation

See [SWI-Prolog manual](https://www.swi-prolog.org/pldoc/package/janus)


## Alternatives

### MQI (Machine Query Interface)

SWI-Prolog               comes              bundled               with
[MQI](https://www.swi-prolog.org/pldoc/package/mqi).  MQI is initiated
from  Python and  starts SWI-Prolog  as a  server.  It  allows calling
Prolog from Python.  Separated using networking, this approach is easy
to  install and  runs  with any  Python version.   It  does not  allow
calling Python  from Prolog, the  primary reason for the  existence of
this package.  Using networking, the latency is relatively high.

### pyswip

The   [pyswip](https://github.com/yuce/pyswip)   interface  uses   the
[Python    ctypes](https://docs.python.org/3/library/ctypes.html)   to
embed Prolog into Python.  Only relying  on _ctypes_, the package is a
fully portable Python package that supports a wide range of Python and
Prolog versions.

Unlike this  package, embedding  Python into  Prolog is  not possible.
_pyswip_ calls Prolog, similarly than janus, using a string.  However,
where  janus allows  passing input  to the  goal as  a _dict_  that is
transferred using the C API  rather than strings, _pyswip_ also passes
the  input as  a  string.   This is  slower,  sensitive to  _injection
attacks_  and   complicated  because  the  user   is  responsible  for
generating  valid Prolog  syntax.   Calls from  Prolog  to Python  are
possible by defining a Prolog  predicate from Python.  This only seems
to support  deterministic predicates and  it cannot pass data  back to
Prolog.  Janus supports calling  Python functions and methods directly
and  supports  enumerating  Python  _iterators_  and  _generators_  as
non-deterministic goals using py_iter/2.

The  overhead of  Janus is  roughly 5  times less  than _pyswip_.   As
_pyswip_ still sustains over 100K  calls per second this is irrelevant
to many applications.
