## Build and Install Instructions
---

### Prerequisites
A supported C and Fortran compiler (see table below).  Other versions may work, in particular if close to the versions listed below.

| Compiler vendor | Supported (tested) versions                                |
|-----------------|------------------------------------------------------------|
| Intel           | 18.0.3.222 and above                                       |
| GNU             | 10.3.0 and above                                           |

A supported MPI library (see table below).  Other versions may work, in particular if close to the versions listed below.

| MPI library     | Supported (tested) versions                                |
|-----------------|------------------------------------------------------------|
| MPICH           | 3.3.1 and above                                            |
| Open MPI        | 3.1.5 and above                                            |
| Intel MPI       | 2018.0.4 and above                                         |

Third-party libraries (TPL) compiled with the same compiler and MPI library (where applicable).

| Library         | Supported (tested) versions                                |
|-----------------|------------------------------------------------------------|
| CMake           | 3.20.1 and above                                           |
| HDF5            | 1.10.4 and above                                           |
| NetCDF-C        | 4.7.3 and above                                            |
| NetCDF-Fortran  | 4.5.2 and above                                            |

NCEP Libraries (NCEPLibs) compiled with the same compiler and MPI library (where applicable).

| Library         | Supported (tested) versions                                |
|-----------------|------------------------------------------------------------|
| BUFR            | 11.4.0 and above                                           |
| CRTM            | 2.3.0 and above                                            |
| IP              | 3.3.3 and above                                            |
| SP              | 2.3.3 and above                                            |
| BACIO           | 2.4.1 and above                                            |
| W3EMC           | 2.9.1 and above                                            |
| SIGIO           | 2.3.2 and above                                            |
| SFCIO           | 1.4.1 and above                                            |
| NEMSIO          | 2.5.2 and above                                            |
| NCIO            | 1.0.0 and above                                            |
| NCDIAG          | 1.0.0 and above                                            |
| WRF_IO          | 1.2.0 and above                                            |

### Building the GSI

`CMake` employs an out-of-source build.  Create a directory for configuring the build and cd into it:

```bash
mkdir -p build && cd build
```

Set the compilers, if needed, to match those being used for compiling the TPL and NCEPLibs listed above: `CC` and `FC` environment variables can be used to point to the desired C and Fortran compilers.

Execute `cmake` from inside your build directory.

```bash
cmake -DCMAKE_INSTALL_PREFIX=<install-prefix> <CMAKE_OPTIONS> /path/to/GSI-source
```

If the dependencies are not located in a path recognized by `cmake` e.g. `/usr/local`, it may be necessary to provide the appropriate environment variables e.g. `<package_ROOT>` or `CMAKE_PREFIX_PATH` so that `cmake` is able to locate these dependencies.

The installation prefix for GSI is provided by the `cmake` command-line argument `-DCMAKE_INSTALL_PREFIX=<install-prefix>`

To build and install:

```
make -j<x>
make install
```

### CMake Options

CMake allows for various options that can be specified on the command line via `-DCMAKE_OPTION=VALUE` or from within the ccmake gui. The list of options currently available is as follows:

| Option              | Description (Default)                                  |
|---------------------|--------------------------------------------------------|
| `OPENMP`              | Enable OpenMP Threading (`OFF`)                      |
| `ENABLE_MKL`          | Use MKL (`ON`), If not found use LAPACK              |
| `BUILD_GSDCLOUD`      | Build GSD Cloud Library (`OFF`)                      |
| `BUILD_MGBF`          | Build MGBF Library (`ON`)                            |
| `BUILD_GSI`           | Build GSI library and executable (`ON`)              |
| `BUILD_ENKF`          | Build EnKF library and executable (`ON`)             |
| `BUILD_REG_TESTING`   | Enable Regression Testing (`ON`)                     |

The flavor of GSI and EnKF can be defined with the use of the following options:

| CMake Option        | Description (Default)        | Valid Options                  |
|---------------------|------------------------------|--------------------------------|
| `GSI_MODE`          | GSI application mode (`GFS`) | `GFS`, `Regional`              |
| `ENKF_MODE`         | EnKF application mode (`GFS`)| `GFS`, `WRF`, `NMMB`, `FV3REG` |

### Regression Testing

When Regression tests are enabled (`-DBUILD_REG_TESTING=ON`), it is necessary to provide
the path to search for the control GSI and EnKF executables.  This can be provided with the option `CONTROLPATH`.  Other search variables are `GSICONTROLPATH` or `ENKFCONTROLPATH` for GSI and EnKF control executables respectively.

If the control executables are not found, the Regression tests will be disabled.

Once the build is complete and control executables have been found, regression testing can be launched from the build directory by executing:
```bash
ctest
```

Refer to the `ctest` manual for more information on `ctest` framework.
