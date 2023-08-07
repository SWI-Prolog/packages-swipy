cmake_minimum_required(VERSION 3.16)
project(swipl-swipy)

include("../cmake/PrologPackage.cmake")

if(CMAKE_BUILD_TYPE STREQUAL "Debug")
  message("Trying to find debug version of Python")
  set(Python_FIND_ABI "ON" "ANY" "ANY")
endif()

find_package(Python COMPONENTS Interpreter Development)
if(CMAKE_VERSION VERSION_LESS 3.18 AND Python_Development_FOUND)
  set(Python_Development.Embed_FOUND ON)
endif()

if(Python_Development.Embed_FOUND)

set(PYTHON_FILES janus.py)
prepend(PYTHON_FILES janus/ ${PYTHON_FILES})

if(WIN32)
  string(REGEX REPLACE "3[.0-9]+\\." "3." Python3_LIB ${Python_LIBRARIES})
  set(PYTHON3_COMPAT 1)
else()
  set(Python3_LIB ${Python_LIBRARIES})
endif()


swipl_plugin(
    janus
    MODULE janus
    C_SOURCES janus/janus.c
    C_LIBS ${Python3_LIB}
    C_INCLUDE_DIR ${Python_INCLUDE_DIRS}
    PL_LIBS janus/janus.pl
    PL_LIB_SUBDIR python
        PL_LIBS ${PYTHON_FILES})

configure_file(config.h.cmake config.h)

test_libs(
    janus
    PACKAGES plunit
    TEST_DIRS tests)

pkg_doc(janus
	SOURCE janus/janus.pl libjanus.tex)

endif(Python_Development.Embed_FOUND)
