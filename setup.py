# Build janus as a Python package

SWIPL="swipl"

from setuptools import setup, Extension
import subprocess
import sys

PLBASE=None
PLLIBDIR=None
PLVERSION=None

def swiplConfig():
    config = subprocess.run([SWIPL, '--dump-runtime-variables'],
                            stdout=subprocess.PIPE).stdout.decode('utf-8')
    for line in config.splitlines():
        i = line.find("=")      # line is name="value";
        name = line[0:i]
        value = line[i+2:-2]
        if ( name == "PLBASE" ):
            global PLBASE
            PLBASE = value
        elif ( name == "PLLIBDIR"):
            global PLLIBDIR
            PLLIBDIR = value
        elif ( name == "PLVERSION"):
            global PLVERSION
            PLVERSION = int(value)

swiplConfig()

if ( PLBASE == None or PLLIBDIR == None ):
    raise RuntimeError("Failed to find SWI-Prolog components")
if ( PLVERSION < 90112 ):
    raise RuntimeError("At least SWI-Prolog version 9.1.12 is required")

link_args=[]
if ( sys.platform == 'linux' ):
    link_args.append(f'-Wl,-rpath={PLLIBDIR},--enable-new-dtags')
elif ( sys.platform == 'darwin' ):
    link_args.append(f'-Wl,-rpath,{PLLIBDIR}')

setup(name='janus',
      version='0.1.0',
      description="Janus library to call SWI-Prolog",
      author="Jan Wielemaker",
      author_email="jan@swi-prolog.org",
      url="http://github.com/SWI-Prolog/packages-swipy",
      license="BSD-2",
      packages=['janus'],
      package_dir={"janus":"janus"},
      package_data={"janus": ['janus.pl']},
      ext_modules= [
          Extension('janus.swipl',
                    ['janus/janus.c'],
                    depends=[
                        'janus/hash.c',
                        'janus/mod_swipl.c'
                    ],
                    define_macros=[
                        ('PYTHON_PACKAGE', '1')
                    ],
                    include_dirs=[
                        f'{PLBASE}/include'
                    ],
                    extra_link_args=link_args,
                    library_dirs=[PLLIBDIR],
                    libraries=['swipl'])
          ])
