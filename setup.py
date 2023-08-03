# Build janus as a Python package

SWIPL="swipl"

from setuptools import setup, Extension
import subprocess

PLBASE=None
PLLIBDIR=None

def swiplConfig():
    config = subprocess.run([SWIPL, '--dump-runtime-variables'],
                            stdout=subprocess.PIPE).stdout.decode('utf-8')
    for line in config.splitlines():
        i = line.find("=")      # line is name="value";
        name = line[0:i]
        value = line[i+2:-2]
        print(value)
        if ( name == "PLBASE" ):
            global PLBASE
            PLBASE = value
        elif ( name == "PLLIBDIR"):
            global PLLIBDIR
            PLLIBDIR = value

swiplConfig()

if ( PLBASE == None or PLLIBDIR == None ):
    print("Failed to find SWI-Prolog components")
    quit()

setup(name='janus',
      version='0.1',
      description="Janus library to call SWI-Prolog",
      author="Jan Wielemaker",
      author_email="jan@swi-prolog.org",
      url="http://github.com/SWI-Prolog/packages-swipy",
      packages=['janus'],
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
                    extra_link_args=[
                        f'-Wl,-rpath={PLLIBDIR},--enable-new-dtags'
                        ],
                    library_dirs=[PLLIBDIR],
                    libraries=['swipl'])
          ])
