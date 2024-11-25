# Building releases

## Update the version

  - edit VERSION
  - run `./scripts/newversion from the top dir.


## Upload a source wheel

  Run `./scripts/pypi-upload`


## Build and upload a binary for Windows

  - Install Python and SWI-Prolog in their default locations
  - Install VS2022 or later
  - Start the __x64__ version of the VS2020 developer command, normally
    named `x64 Native Tools Command`.
  - Download the source

		git clone https://github.com/SWI-Prolog/packages-swipy swipy

  - Build the package

		py -m pip install .

  - on success, build the distribution

		py -m pip install --upgrade pip
		py -m pip install --upgrade build
		py -m build

  - Upload the wheel created in `dist` using
    [twine](https://pypi.org/project/twine/)
