import os
import sys

if ( sys.platform == "win32" ):
    from janus_swi._find_swipl import swipl_properties
    props = swipl_properties()
    if ( props ):
        if ( int(props["PLVERSION"]) < 90112 ):
             raise RuntimeError("At least SWI-Prolog version 9.1.12 is required")
        libdir = os.path.dirname(props["PLLIBSWIPL"])
        os.add_dll_directory(libdir)
    else:
        raise RuntimeError("Could not find SWI-Prolog in %PATH% or registry")

from janus_swi.janus import *
import janus_swi._swipl

_swipl.initialize("swipl",
                  "-g", "true",
                  "-p", "library="+os.path.dirname(__file__),
                  "--no-signals")
