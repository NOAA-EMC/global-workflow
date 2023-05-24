[![Read The Docs Status](https://readthedocs.org/projects/global-workflow/badge/?badge=latest)](http://global-workflow.readthedocs.io/)
[![shellnorms](https://github.com/NOAA-EMC/global-workflow/actions/workflows/linters.yaml/badge.svg)](https://github.com/NOAA-EMC/global-workflow/actions/workflows/linters.yaml)
[![pynorms](https://github.com/NOAA-EMC/global-workflow/actions/workflows/pynorms.yaml/badge.svg)](https://github.com/NOAA-EMC/global-workflow/actions/workflows/pynorms.yaml)
[![pytests](https://github.com/NOAA-EMC/global-workflow/actions/workflows/pytests.yaml/badge.svg)](https://github.com/NOAA-EMC/global-workflow/actions/workflows/pytests.yaml)

CI Workflow Status

[![Orion](https://github.com/TerrenceMcGuinness-NOAA/global-workflow/actions/workflows/orion.yaml/badge.svg)](https://github.com/NOAA-EMC/global-workflow/actions/workflows/orion.yaml)
[![Hera](https://github.com/TerrenceMcGuinness-NOAA/global-workflow/actions/workflows/hera.yaml/badge.svg)](https://github.com/NOAA-EMC/global-workflow/actions/workflows/hera.yaml)

![Hera](https://img.shields.io/endpoint?url=https://gist.github.com/TerrenceMcGuinness-NOAA/13060fc1a0cd9368a4ee94171785fe9d/raw/hera.json)
# global-workflow
Global Workflow currently supporting the Global Forecast System (GFS) with the [UFS-weather-model](https://github.com/ufs-community/ufs-weather-model) and [GSI](https://github.com/NOAA-EMC/GSI)-based Data Assimilation System.

The `global-workflow` depends on the following prerequisities to be available on the system:

* Workflow Engine - [Rocoto](https://github.com/christopherwharrop/rocoto) and [ecFlow](https://github.com/ecmwf/ecflow) (for NWS Operations)
* Compiler - Intel Compiler Suite
* Software - NCEPLIBS (various), ESMF, HDF5, NetCDF, and a host of other softwares (see module files under /modulefiles for additional details)

The `global-workflow` current supports the following tier-1 machines:

* NOAA RDHPCS - Hera
* MSU HPC - Orion
* NOAA's operational HPC - WCOSS2

Additionally, the following tier-2 machine is supported:
* SSEC at Univ. of Wisconsin - S4 (Note that S2S+ experiments are not fully supported)

Documentation (in progress) is available [here](https://global-workflow.readthedocs.io/en/latest/).

# Disclaimer

The United States Department of Commerce (DOC) GitHub project code is provided
on an "as is" basis and the user assumes responsibility for its use. DOC has
relinquished control of the information and no longer has responsibility to
protect the integrity, confidentiality, or availability of the information. Any
claims against the Department of Commerce stemming from the use of its GitHub
project will be governed by all applicable Federal law. Any reference to
specific commercial products, processes, or services by service mark,
trademark, manufacturer, or otherwise, does not constitute or imply their
endorsement, recommendation or favoring by the Department of Commerce. The
Department of Commerce seal and logo, or the seal and logo of a DOC bureau,
shall not be used in any manner to imply endorsement of any commercial product
or activity by DOC or the United States Government.

