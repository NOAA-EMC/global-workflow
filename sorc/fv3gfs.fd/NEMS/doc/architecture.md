Architecture {#architecture}
============

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

Key architecture features of the coupled NEMS:

* Data exchanges between major model components go through a central
  (NEMS) mediator component. There may be multiple versions of this
  central mediator (e.g. for global and regional systems), and there
  may be additional specialized mediators (e.g. for the 3D
  interactions associated with space weather coupling).

* The NEMS mediator component is an integral part of the NEMS
  software. The mediator source code is managed alongside the NEMS
  source code, is integrated into the NEMS make system and is built
  when the NEMS executable is built.

* The atmosphere models (currently GSM, FIM, NMMB) currently remain as
  parts of the NEMS software. Their source code is managed alongside
  the NEMS source code, they are integrated into the NEMS make system
  and are built when the NEMS executable is built. This is likely to
  change in the future.

* Ocean, ice, wave and other models are treated by NEMS as external
  dependencies. Their source code is managed outside of NEMS,
  typically in the proximity of the organization maintaining the
  official version of the model. Each model maintains its own separate
  make system with a NUOPC compliant build option. NEMS requires that
  the pre-built models are available when the NEMS executable is being
  built.

* All of the components driven by they EARTH_COMP are NUOPC-compliant
  components.

* All of the components driven by they EARTH_COMP can be configured to
  run on a specific set of PETs (MPI processes), supporting concurrent
  or sequential component execution.

The coupled NEMS version extends the original NEMS version by
implementing the EARTH_COMP level as a NUOPC compliant driver that
accepts a NUOPC compliant mediator component and NUOPC compliant model
components (ATMs, OCNs, ICEs, WAVs). The diagram below shows the
architecture of the target coupled NEMS system.  Although not shown,
recent developments in the mediator are adding separate land and
hydrology components as siblings to ATM, OCN, ICE, and WAV.

The specific OCN, ICE, WAV models are shown within clouds outside of
the NEMS box to indicate that NEMS handles them as external
dependencies. Data exchanges between components are handled by generic
NUOPC_Connector components indicated by green arrows. The generic
connectors perform basic regrid and redist operations as needed to
take field data from one side to the other.

\image html nems-calls.png
