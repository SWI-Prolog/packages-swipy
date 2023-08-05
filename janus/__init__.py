import os
import sys
import subprocess

def _swipl_libdir():
    config = subprocess.run(["swipl", '--dump-runtime-variables'],
                            stdout=subprocess.PIPE).stdout.decode('utf-8')
    for line in config.splitlines():
        i = line.find("=")      # line is name="value";
        name = line[0:i]
        value = line[i+2:-2]
        if ( name == "PLLIBSWIPL" ):
            return os.path.dirname(value);

if ( sys.platform == "win32" ):
    dir=_swipl_libdir()
#   print(f"Using SWI-Prolog DLLs from {dir}")
    os.add_dll_directory(dir)

from janus.janus import *
import janus.swipl

janus.swipl.initialize("swipl",
                       "-g", "true",
                       "--no-signals")

# Get library(janus) for calling Python from Prolog.  If this library is
# already part of Prolog, use it, else add this directory to the library
# search path.
swipl.call("(exists_source(library(janus))->true;asserta(user:file_search_path(library, Here)))",
           {"Here":os.path.dirname(__file__)})
