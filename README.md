# Janus: a bi-directional interface to Python

> This is explorative work.  This work is based on Janus for XSB by
> Theresa Swift and Carl Anderson.

This  code  implements  a  ready-to-use  bi-directional  interface  to
Python.  As  motivated by Theresa  Swift, Python opens many  doors for
accessing resources such as graphics, machine learning and many more.

While this code is motivated by Janus, the current interface is rather
different.  In part, that is due to the extended data types that allow
SWI-Prolog to define  a cleaner interface.  In part, this  is based on
experience  with e.g.,  our JavaScript  interface.  We  are discussing
changes on both ends to make the interfaces as compatible as possible.
Part of this discussion is already reflected in the current state.

## Installing

This  is  normally a  GIT  sub  module  of  the SWI-Prolog  git  repo.
Configuration and installation of `library(janus)` which embeds Python
into Prolog is  handled by the normal  Prolog configuration.  Building
the  interface  requires  the  libraries  and  C  headers  for  Python
embedding to be installed.  On __Debian__ based Linux systems, this is
achieved using

    apt install libpython3-dev

On __MacOS__,  these files are  included in the Homebrew  and Macports
versions of Python

On  Windows,  these  files  are included  in  the  default  installer.
Configuration requires Python to appear in ``%PATH%``.

After successful installation, running `py_version/0` should result in
printing relevant information on the embedded Python system.

    ?- py_version.
	% Janus embeds Python 3.10.12 (main, Jun 11 2023, 05:26:28) [GCC 11.4.0

### Embedding Prolog into Python

This  package provides  `setup.py`.  Given  a SWI-Prolog  version with
this library included accessible as `swipl` and the infra structure to
build C extensions, you can install the package `janus` using

    pip install .

On success, this should work:

    python
	>>> from janus import *
	>>> once("writeln('Hello world!')")
	Hello world!
	{'status': True}
	>>>

## Documentation

See [SWI-Prolog manual](https://www.swi-prolog.org/pldoc/package/janus)
