# Janus-swi: a bi-directional interface between SWI-Prolog and Python

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


## Bi-directional

This  GIT repository  is a  GIT _submodule_  of the  SWI-Prolog source
repository.  As part of the  SWI-Prolog source distribution it is used
to build `library(janus)`, a Prolog  library that embeds Python.  This
same  module can  be  used  stand-alone to  build  the Python  package
`janus_swi` that embeds Prolog into  Python.  Loaded either way, Janus
is the same and allows for mutually recursive calls between Prolog and
Python.


## Embedding Prolog into Python: the Python janus_swi package

If  this  repository   is  used  to  build  the   Python  pip  package
`janus_swi`, we can  load SWI-Prolog into Python  and call predicates.
For example:

    python
	>>> import janus_swi as janus
	>>> janus.query_once("writeln('Hello world!')")
	Hello world!
	{'truth': True}
	>>>

The    Python    package    is     available    from    __PyPi__    as
[janus-swi](https://pypi.org/project/janus-swi/).      We    currently
provide  a few  _wheels_ for  Windows.   The binaries  in the  Windows
_wheel_ probably supports all Python and Prolog versions that are also
supported by  the source.   The package can  be installed  using `pip`
from source on any system with CPython 3.6 or later, SWI-Prolog 9.1.12
or later and a  C compiler.  For compiling the C  code, GCC, Clang and
VS2022 have been tested.  Thus,  normally the package can be installed
using

    pip install janus-swi

SWI-Prolog is  selected by  finding `swipl`  on the  executable search
path.   If `swipl.exe`  is not  in ``%PATH%``  on Windows  the Windows
registry is examined to find SWI-Prolog.

If  you installed  SWI-Prolog from  source, it  is advices  to install
Janus from the  `packages/swipy` directory in the  Prolog source.  The
package can be installed from within this directory using

    pip install .


## Embedding Python into Prolog: library(janus)

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


## Using Conda

Ongoing work  to get SWI-Prolog  working under  Conda can be  found at
https://github.com/SWI-Prolog/swi-prolog-feedstock.   Eventually, this
work shall be merged with https://anaconda.org/conda-forge/swi-prolog

As  is,  https://github.com/SWI-Prolog/swi-prolog-feedstock  has  been
used  to build  the full  SWI-Prolog  system with  Janus interface  on
Linux, MacOS and Windows.


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
