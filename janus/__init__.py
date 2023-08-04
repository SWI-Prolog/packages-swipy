from janus.janus import *
import janus.swipl
import os

janus.swipl.initialize("swipl",
                       "-g", "true",
                       "--no-signals")

# Get library(janus) for calling Python from Prolog.  If this library is
# already part of Prolog, use it, else add this directory to the library
# search path.
swipl.call("(exists_source(library(janus))->true;asserta(user:file_search_path(library, Here)))",
           {"Here":os.path.dirname(__file__)})
