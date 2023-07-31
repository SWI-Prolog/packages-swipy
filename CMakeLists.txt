cmake_minimum_required(VERSION 3.18)
project(swipl-swipy)

include("../cmake/PrologPackage.cmake")

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  message("Trying to find debug version of Python")
  set(Python_FIND_ABI "ON" "ANY" "ANY")
endif()

find_package(Python COMPONENTS Interpreter Development)
if(Python_Development.Embed_FOUND)

set(PYTHON_FILES janus.py)
prepend(PYTHON_FILES python/ ${PYTHON_FILES})

swipl_plugin(
    janus
    MODULE janus
    C_SOURCES janus.c
    C_LIBS ${Python_LIBRARIES}
    C_INCLUDE_DIR ${Python_INCLUDE_DIRS}
    PL_LIBS janus.pl jns_utils.pl
    PL_LIB_SUBDIR python
        PL_LIBS ${PYTHON_FILES})

configure_file(config.h.cmake config.h)

test_libs(
    janus
    PACKAGES plunit
    TEST_DIRS tests)

pkg_doc(janus
	SECTION
	    jns_utils.pl
	SOURCE janus.pl libjanus.tex)

endif(Python_Development.Embed_FOUND)
