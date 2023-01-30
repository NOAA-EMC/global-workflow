# global workflow specific tools

Python tools specifically for global applications

## Installation
Simple installation instructions
```sh
$> git clone https://github.com/noaa-emc/global-workflow
$> cd global-workflow/ush/python/pygw
$> pip install .
```

It is not required to install this package.  Instead, 
```sh
$> cd global-workflow/ush/python/pygw
$> export PYTHONPATH=$PWD/src/pygw
```
would put this package in the `PYTHONPATH`

### Note:
These instructions will be updated and the tools are under development.

### Running python tests:
Simple instructions to enable executing pytests manually
```sh
# Create a python virtual environment and step into it
$> cd global-workflow/ush/python/pygw
$> python3 -m venv venv
$> source venv/bin/activate

# Install pygw with the developer requirements
(venv) $> pip install .[dev]

# Run pytests
(venv) $> pytest -v
```
