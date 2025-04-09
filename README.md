[![Read The Docs Status](https://readthedocs.org/projects/global-workflow/badge/?badge=latest)](http://global-workflow.readthedocs.io/)
[![shellnorms](https://github.com/NOAA-EMC/global-workflow/actions/workflows/linters.yaml/badge.svg)](https://github.com/NOAA-EMC/global-workflow/actions/workflows/linters.yaml)
[![pynorms](https://github.com/NOAA-EMC/global-workflow/actions/workflows/pynorms.yaml/badge.svg)](https://github.com/NOAA-EMC/global-workflow/actions/workflows/pynorms.yaml)

![Custom badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/emcbot/e35aa2904a54deae6bbb1fdc2d960c71/raw/hera.json)
![Custom badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/emcbot/e35aa2904a54deae6bbb1fdc2d960c71/raw/hercules.json)
![Custom badge](https://img.shields.io/endpoint?url=https://gist.githubusercontent.com/emcbot/e35aa2904a54deae6bbb1fdc2d960c71/raw/wcoss2.json)

The Global Workflow supporting the Global Forecast System (GFS), the Global Ensemble Forecasting System (GEFS), and the Seasonal Forecast System (SFS) with the [UFS-weather-model](https://github.com/ufs-community/ufs-weather-model).  Data assimilation, currently only available for the GFS, is provides by both the [GSI](https://github.com/NOAA-EMC/GSI)- and [GDASApp (JEDI)](https://github.com/NOAA-EMC/GDASApp)-based Data Assimilation systems.

In progress [documentation](https://global-workflow.readthedocs.io/en/latest/) is available.

# Prerequisites

The Global Workflow depends on the following prerequisities to be available on the system:

* Workflow Engines - [Rocoto](https://github.com/christopherwharrop/rocoto) and [ecFlow](https://github.com/ecmwf/ecflow) (for NWS Operations)
* Compiler - Intel classic compiler suite version 2021.x
* Software - NCEPLIBS (various), ESMF, HDF5, NetCDF, and a host of other softwares (see module files under /modulefiles for additional details).
    - [Spack-stack](https://github.com/JCSDA/spack-stack) is recommended for the installation of dependent libraries.

## Supported platforms

The Global Workflow currently supports the following machines at the indicated tier.

| HPC                                     | Tier | Notes                                                                      |
| --------------------------------------- |:----:|:--------------------------------------------------------------------------:|
| WCOSS2<br>NCO                           | 1    | GEFS testing is not regularly performed.                                   |
| Hera<br>NOAA RDHPCS                     | 1    |                                                                            |
| Hercules<br>MSU                         | 1    | Currently does not support the TC Tracker.                                 |
| Orion<br>MSU                            | 2    | The GSI runs very slowly on Orion and the TC tracker is not supported.     |
| Gaea C5/C6<br>RDHPCS                    | 3    | Currently non-operational following an OS upgrade.<br>Supported by EPIC.   |
| AWS, GCP, Azure <br>NOAA Parallel Works | 3    | Supported by EPIC.                                                         |
| Jet<br>RDHPCS                           | 3    | Supported by NESDIS.                                                       |
| S4<br>SSEC                              | 3    | Currently non-operational following an OS upgrade.<br>Supported by NESDIS. |

<ins>**Tier Definitions**</ins>

1. Fully supported by the EMC global workflow team.  CI testing is regularly performed on these systems, the majority of the global workflow features are supported, and the team will address any platform-specific features, bugs, upgrades, and requests for data.
2. Supported by the global workflow team on an ad-hoc basis.  CI tests are supported on these systems, but not regularly performed.
3. No official support by the global workflow team, but may be supported by other entities (e.g. EPIC).

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

