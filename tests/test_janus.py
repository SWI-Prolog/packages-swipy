# Test Python loading Janus and running the Prolog test suite.

import pytest
import janus_swi as janus

janus.consult("test_janus.pl")

def test_run_prolog():
    assert janus.query_once("test_janus")["truth"] == True
