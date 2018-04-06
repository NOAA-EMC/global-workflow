Introduction to NEMS {#introduction}
====================

The NOAA Environmental Modeling System (NEMS) is the infrastructure
underlying a **coupled modeling system** that supports predictions of
Earth's environment at a range of time scales. Examples of other
coupled modeling systems are the 
[Community Earth System Model (CEEM)](http://www.cesm.ucar.edu)
and the
[Met Office Unified Model] (http://www.metoffice.gov.uk/research/modelling-systems/unified-model)

**A model component** is a software representation of a physical
domain or process, for example sea ice. It is often developed by a
team of specialists in that domain. Model coupling is a software
representation of feedbacks between physical processes. It involves
modifying the exported fields of a component through grid, unit,
temporal, and other transformations so that they can be used as the
inputs for another component.

NEMS includes infrastructure for coupling model components
representing major Earth system domains and processes.  **External
model components** have a primary repository that is not at the NCEP
Envionmental Modeling Center (EMC). In general, model components are
coupled through the NEMS mediator (in other coupled modeling systems
this is often called the "coupler").  NEMS also includes some
specialized mediators; for example, for space weather. In some cases
in NEMS, the model components are coupled "in-line", meaning that they
are called directly from another model component instead of having
fields sent through the mediator.

NEMS can be assembled into a number of different **modeling
applications** (often shortened to just applications). Modeling
applications are associated with a purpose, like medium-range
forecasting; a set of model components; and a set of parameters that
represent a range of supported options, including grids and
resolutions. Different NEMS modeling applications can have different
types and numbers of model components. Also, the same physical domain
may be represented by different model components in different modeling
applications. For example, in some NEMS modeling applications the
ocean component may be the HYbrid Coordinate Ocean Model (HYCOM) and
in others it may be the Modular Ocean Model (MOM).

[This spreadsheet lists anticipated NEMS modeling applications] (https://docs.google.com/spreadsheets/d/1RS-fTBYnfSIWrJYfalD2lAI-bUOGM0frNPEMIO_ND28/edit#gid=0).
For each modeling application, the spreadsheet includes the set of
model components that is the target for final system delivery, and the
set of components in initial and incremental deliveries. Note that
this spreadsheet does not include all anticipated configurations!
There are many test configurations that are not represented on the
spreadsheet. Documentation about each of the NEMS applications and
components is in general the responsibility of its original
developers. 
[Links to available documentation on NEMS applications and components have been compiled here.](https://docs.google.com/spreadsheets/d/1CLT66uzJrjrsY-um0jB5hU-Gfeh3_VCIJDA4-Ibmu5s/edit#gid=0)


Compilation and Execution
-------------------------

Since there are multiple NEMS modeling applications, each with
multiple model components, and multiple modes for each model component
type (prognostic, prescribed data, etc.), there is a lot of complexity
in the NEMS system. It is important to have a consistent way of
working with the NEMS code base, and especially of coordinating
changes to the NEMS code base.

Modeling applications can be assembled using a tool called the
[NEMSAppBuilder](building).  This tool enables each modeling
application team to fully control its source code.

Development and analysis of modeling applications in NEMS will require
the use of other component modes besides prognostic
components. Prescribed data components, stub components that don't
send any fields, and other options are needed for technical testing
and controlled scientific experimentation. The tool called Component
Sets, or
[Compsets](https://esgf.esrl.noaa.gov/projects/couplednems/compsets),
enables testing of each model application in numerous configurations.
Execution of compsets is done by the
[NEMSCompsetRun](running).

* \ref running
* \ref building

Infrastructure
--------------

[NEMS is built using the Earth System Modeling Framework (ESMF)](https://www.earthsystemcog.org/projects/esmf/)
infrastructure software. ESMF provides utilities like generation of
interpolation weights and utilities for calendar and timee management,
and also wrappers that create a standard component calling
interface. This enables model components developed at different sites
to be coupled more easily.

[The National Unified Operational Prediction Capability (NUOPC) Layer] (https://earthsystemcog.org/projects/nuopc/)
adds additional rules about how ESMF models interact and increases
their interoperability. The NUOPC Layer covers aspects from the level
of build dependencies, to standardization of initialization phases,
all the way to standard names of the exchanged fields. NEMS is an
example of a modeling system built using the NUOPC Layer architecture.

Architecture 
------------

The NEMS architecture is based on an ESMF component hierarchy with the
application driver `MAIN_NEMS` at the top, calling into the
`NEMS_COMP` component which in turn drives the `EARTH_COMP`
component. The `EARTH_COMP` drives the `ATM` component (which calls
into options `GSM, NMMB`, or `FIM`). The architecture allows for
multiple `EARTH_COMP` instances, supporting ensemble applications such
as the Global Ensemble Forecast System (GEFS).

Coupled NEMS includes atmosphere, ocean, ice, wave, land,
aerosol/chemistry, and hydrologic models, with coupling interface and
utilities based on the 
[Earth System Modeling Framework (ESMF)](https://www.earthsystemcog.org/projects/esmf/).
The NEMS applications also utilize intreopereability conventions
introduced by the 
[National Unified Operational Prediction Capability (NUOPC)](https://www.earthsystemcog.org/projects/nuopc/).

* \ref architecture
* \ref structure

Quick Build and Run
-------------------


### 6.1.1   Download

Use the following SVN command to download a specific revision of a NEMS application from its development trunk:
    
    svn co -r <REV> 
    https://svnemc.ncep.noaa.gov/projects/nems/apps/
    <Application>/trunk <Application>

In this command, `<REV>` stands for a revision number, for example, `73964`. In some places on this site, the prefix `DREV` is used to indicate that the revision is on a development trunk. The prefix R indicates that a revision is on an application trunk. These prefixes should not be included in the command, just the revision number.

Where `<Application>` appears, it can have one of the following values:
* HYCOM-Ice
* Regional
* Regional-Nest
* UGCS-Weather
* UGCS-Subseasonal
* UGCS-Seasonal
* WAM
* WAM-IPE

The `SVN` command will download all of the necessary pieces including constituent components from different repositories.  It is possible to access the very latest working revision of the code by omitting the `"-r REV"` part from the `SVN` command when downloading by revision number. This may be convenient during development, however, it is not recommended for validation work where it is critical to keep track of the precise revision information.

There are a few ways to find out more about specific revisions and the features that they contain. The links under the Milestone Revisions header on the left navigation bar describe revisions that are documented and tested for particular [application milestones](https://docs.google.com/spreadsheets/d/1RS-fTBYnfSIWrJYfalD2lAI-bUOGM0frNPEMIO_ND28/edit#gid=0). The development tasks completed for each revision are included in the [task prioritization spreadsheet](https://docs.google.com/spreadsheets/d/1C0k9AfH9DZHmJCW_bSdK2TzfFB9qLjyE8416nqqXjTM/edit#gid=0).

### 6.1.2   Build

Change into the `<Application>` directory created by the SVN command during download. Then execute the `NEMS AppBuilder` by typing:

    ./NEMS/NEMSAppBuilder

A terminal based dialog script will guide you through the build process. The end result of the build process is a NEMS executable `(./NEMS/exe/NEMS.x)` that is configured to run the application.

### 6.1.3   Run

Below are general instructions on how to run a NEMS application. There may be some special or additional directions for running a particular application configuration (for example, for the cold start in UGCS-Seasonal). Please see the links under specific Milestone Revisions for these instructions.

From within the `<Application>` directory execute the `NEMS CompsetRun` tool by typing:

    ./NEMS/NEMSCompsetRun

This command will automatically run all of the compsets listed in the `<Application>.compsetRun` file. This file can be edited to change the compsets that are being run. New `CompsetRun` files can also be created and explicitly specified when calling the `NEMSCompsetRun` tool. Finally, new application specific compsets can be created and added to the repository.
