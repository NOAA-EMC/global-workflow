====================================================
Global Chemistry and Aerosol Forecast System (GCAFS)
====================================================

Overview
--------

The Global Chemistry and Aerosol Forecast System (GCAFS) extends the Global Forecast System (GFS)
with interactive aerosol and atmospheric chemistry capabilities. It provides a unified framework
for predicting the evolution of atmospheric composition alongside traditional weather variables.

Key Features
-----------

* Interactive GOCART aerosol module for forecasting dust, sea salt, sulfate, black carbon, and organic carbon
* Optional full atmospheric chemistry with gas-phase and heterogeneous reactions
* Integration with biomass burning emissions sources (QFED, GBBEPx)
* NEXUS emissions preprocessing system for anthropogenic and biogenic sources
* Support for multiple emission inventories (CEDS, HTAP, CAMS)
* Aerosol-radiation-cloud interactions
* Optional aerosol data assimilation

Running GCAFS
------------

GCAFS can be run using the global-workflow framework. To set up a free-forecast GCAFS experiment:

.. code-block:: bash

   ./setup_expt.py gcafs forecast-only --pslot my_gcafs_run --app ATMA \
                --idate 2023010100 --edate 2023010100 \
                --resdetatmos 384 --comroot /path/to/com --expdir /path/to/exp

Configuration is managed through the standard global-workflow configuration files. GCAFS-specific
settings are documented in :doc:`gcafs_config`.

After setting up the experiment, build the workflow XML and launch it:

.. code-block:: bash

   ./setup_workflow.py /path/to/exp/my_gcafs_run
   cd /path/to/exp/my_gcafs_run
   rocotorun -w gcafs.xml -d gcafs.db


One can also run in 'cycled' mode, see other sections of the global-workflow documentation
for details on how to set up a cycled experiment.
For GCAFS, the meteorological analysis in 'cycled' mode is taken from the GDAS (either from
HPSS archive or a location stored on disk). The aerosol analysis is optional and controlled by setting
`USE_AERO_ANL` to be "YES"

GCAFS Workflow
-------------

The GCAFS workflow includes these main tasks:

1. **stage_ic** - Stage initial conditions
2. **prep_emissions** - Prepare emissions data files
3. **aerosol_init** - Initialize aerosol fields
4. **fcst** - Run the UFS model with aerosols/chemistry
5. **atmos_prod** - Post-process atmosphere/aerosol output
6. **arch_vrfy** and **arch_tars** - Archive verification data and create tarballs

The workflow is managed by the Rocoto workflow manager, with tasks defined in the
``workflow/rocoto/gcafs_tasks.py`` file.

Configuration Files
------------------

GCAFS configuration is managed through several key files in the ``parm/config/gcafs/`` directory:

### config.aero.j2

The primary configuration file for aerosol settings, containing:

**Aerosol Model Settings:**

.. code-block:: bash

   export AERO_INPUTS_DIR="/path/to/aerosol/data"    # Base directory for aerosol input data
   export AERO_CONFIG_DIR="${PARMgfs}/ufs/gocart"    # GOCART configuration files
   export fscav_aero="'*:0.3','so2:0.0',..."         # Convective scavenging factors
   export dnats_aero=2                               # Number of diagnostic tracers

**Fire Emissions Settings:**

.. code-block:: bash

   export AERO_EMIS_FIRE="gbbepx"                    # Fire dataset: gbbepx, qfed, none
   export AERO_EMIS_FIRE_VERSION="061"               # Dataset version
   export AERO_EMIS_FIRE_HIST=1                      # Historical (1) vs NRT (0)

**NEXUS Emissions Settings:**

.. code-block:: bash

   export NEXUS_CONFIG="gocart"                      # NEXUS configuration set
   export NEXUS_TSTEP=3600                           # Time step (seconds)
   export NEXUS_DO_CEDS2019=.true.                   # Enable CEDS 2019
   export NEXUS_DO_HTAPv2=.true.                     # Enable HTAP v2
   export NEXUS_DO_CAMS=.false.                      # Enable CAMS

These settings are processed as Jinja2 templates, allowing for experiment-specific customization
through template variables like ``{{ NEXUS_CONFIG | default('gocart') }}``.

Emissions Preprocessing
-----------------------

The ``prep_emissions`` task is a critical component of the GCAFS workflow that prepares all necessary emissions data and configuration files for the model run.

### Overview of prep_emissions

This task performs several important functions:

1. **Configuration Generation**: Creates customized GOCART configuration files from templates
2. **Fire Emissions Processing**: Handles biomass burning emissions from QFED or GBBEPx datasets
3. **NEXUS Preprocessing**: Processes anthropogenic and biogenic emissions through the NEXUS system
4. **Emissions File Preparation**: Generates model-ready emissions data files
5. **Historical Data Handling**: Retrieves historical emissions when needed for testing or spin-up
6. **Template Variable Processing**: Processes all template variables in the configuration files

The task is implemented in ``ush/python/pygfs/task/aero_emissions.py`` as the ``AerosolEmissions`` class.

### Fire Emissions Configuration

GCAFS supports multiple biomass burning emission datasets that can be configured through the ``config.aero`` file:

**Available Fire Emission Datasets:**

* **GBBEPx** (Global Biomass Burning Emissions Product): NOAA/NWS operational fire emissions
* **QFED** (Quick Fire Emission Dataset): NASA fire emissions with near-real-time updates  
* **None**: Disable fire emissions entirely

**Configuration Options:**

.. code-block:: bash

   # Select fire emissions dataset
   export AERO_EMIS_FIRE="gbbepx"           # Options: gbbepx, qfed, none
   export AERO_EMIS_FIRE_VERSION="061"      # Dataset version
   export AERO_EMIS_FIRE_HIST=1             # Use historical (1) or near-real-time (0)
   
   # Directories for emissions data
   export FIRE_EMIS_NRT_DIR=""              # Near-real-time data location
   export FIRE_EMIS_DIR=""                  # Historical data location

### NEXUS Emissions Preprocessing

NEXUS (Next-generation Emissions eXchange Utility System) preprocesses anthropogenic and biogenic emissions from multiple global inventories:

**Supported Emission Inventories:**

* **CEDS** (Community Emissions Data System): Global anthropogenic emissions (2019/2024 versions)
* **HTAP** (Hemispheric Transport of Air Pollution): Regional high-resolution emissions (v2/v3)
* **CAMS** (Copernicus Atmosphere Monitoring Service): European reanalysis emissions
* **MEGAN** (Model of Emissions of Gases and Aerosols from Nature): Biogenic emissions (future)

**NEXUS Configuration:**

.. code-block:: bash

   # NEXUS system configuration
   export NEXUS_CONFIG="gocart"             # Configuration set (gocart, none)
   export NEXUS_TSTEP=3600                  # Time step in seconds
   
   # Grid specification (0.25-degree global)
   export NEXUS_NX=1440                     # Longitude points
   export NEXUS_NY=720                      # Latitude points
   
   # Enable/disable emission inventories
   export NEXUS_DO_CEDS2019=.true.          # CEDS 2019 emissions
   export NEXUS_DO_CEDS2024=.false.         # CEDS 2024 emissions  
   export NEXUS_DO_HTAPv2=.true.            # HTAP v2 emissions
   export NEXUS_DO_CAMS=.false.             # CAMS emissions

### Emission Dataset Details

**Fire Emissions:**

* **GBBEPx (Global Biomass Burning Emissions Product)**: 
  - Operational NOAA/NWS fire emissions based on VIIRS satellite data
  - Near-real-time updates with ~6-hour latency
  - Includes wildfire, agricultural burning, and prescribed burns
  
* **QFED (Quick Fire Emission Dataset)**:
  - NASA fire emissions using MODIS satellite observations
  - Available in near-real-time and historical versions
  - High spatial resolution with detailed speciation

**Anthropogenic/Biogenic Emissions:**

* **CEDS (Community Emissions Data System)**:
  - Global gridded emissions inventory (1750-2019/2024)
  - Anthropogenic sources: energy, industry, transport, residential, agriculture
  - Species: SO2, NOx, CO, NH3, black carbon, organic carbon, PM2.5
  
* **HTAP (Hemispheric Transport of Air Pollution)**:
  - Regional high-resolution emissions for Europe, Asia, North America
  - Focuses on transboundary air pollution
  - Complements CEDS with finer spatial detail
  
* **CAMS (Copernicus Atmosphere Monitoring Service)**:
  - European Centre reanalysis emissions
  - Consistent with meteorological fields
  - Includes temporal disaggregation capabilities

GOCART Configuration Files
--------------------------

The GOCART aerosol module in GCAFS is configured through a set of resource (.rc) files located in
``parm/ufs/gocart/``. These files control the behavior of the aerosol components, emissions,
and diagnostics. The key configuration files include:

Core Configuration
~~~~~~~~~~~~~~~~~~

- **AERO.rc**: Core aerosol module configuration with grid resolution settings
- **AGCM.rc**: Atmospheric model interface configuration
- **CAP.rc**: Component interface specifications defining imports/exports and tracer mappings
- **GOCART2G_GridComp.rc**: Defines active aerosol species instances (DU, SS, SU, CA, NI)

Aerosol Species Configuration
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each aerosol species has its own configuration file with specific parameters:

- **DU2G_instance_DU.rc**: Dust particle properties, emission scheme settings (fengsha/ginoux/k14)
- **SS2G_instance_SS.rc**: Sea salt configuration with size bins and emission methods
- **SU2G_instance_SU.rc**: Sulfate species configuration including volcanic emissions
- **CA2G_instance_CA.bc.rc**: Black carbon aerosol properties and behavior
- **CA2G_instance_CA.oc.rc**: Organic carbon configuration including SOA formation
- **NI2G_instance_NI.rc**: Nitrate aerosol specification (optional species)

Output and Diagnostics
~~~~~~~~~~~~~~~~~~~~~~

- **AERO_HISTORY.rc**: Controls aerosol output diagnostics including:
  - Aerosol concentrations (inst_du_ss, inst_ca, inst_ni, inst_su)
  - Process-specific outputs (emission, deposition)
  - Optical properties (AOD calculations)
  - Output grid configuration and file formats

The frequency parameters for output are specified as variables (e.g., ``@[inst_aod_freq]``) that
are replaced at runtime with values from the workflow configuration.

Emissions Configuration
~~~~~~~~~~~~~~~~~~~~~~~

External data sources for emissions are configured through ExtData resource files:

- **ExtData.gbbepx**: GBBEPx biomass burning emissions configuration
- **ExtData.qfed**: QFED fire emissions configuration  
- **ExtData.nexus**: NEXUS-processed anthropogenic/biogenic emissions
- **ExtData.other**: Additional emission sources (volcanic, lightning, etc.)
- **ExtData.none**: Placeholder configuration when emissions are disabled

The NEXUS system processes emissions through HEMCO (Harmonized Emissions Component) configuration files:

- **NEXUS_Config.rc**: Master configuration orchestrating all emission sources
- **HEMCO_sa_Grid.rc**: Grid definition and interpolation settings
- **HEMCO_sa_Time.rc**: Temporal scaling patterns (diurnal, weekly, seasonal)
- **HEMCO_sa_Spec.rc**: Species mapping between inventories and GOCART tracers
- **HEMCO_sa_Diag.rc**: Diagnostic output configuration

To modify the aerosol configuration, edit these files or create custom versions in your experiment
directory. The file ``gocart_tracer.list`` defines the complete set of aerosol tracers used in the model.

ExtData File Format Details
~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ExtData configuration files specify how external data sources are imported into the model. Each entry follows this format:

.. code-block:: none

   # Import Name | Units | Clim | Regrid | Time Template | Offset | Scale | Var on File | File Template
   OC_BIOMASS NA  N Y %y4-%m2-%d2t12:00:00 none 0.7778 OC ChemInput/FIRE_EMIS.%y4%m2%d2.nc4

Field descriptions:

- **Import Name**: Internal variable name used by GOCART (e.g., OC_BIOMASS, SU_BIOMASS)
- **Units**: Units for the imported field or NA if not applicable
- **Clim**: Climate file flag (Y/N) - Y indicates a static/climatological file
- **Regrid**: Regrid method - Y for bilinear interpolation, N for no regridding, E for conservative regridding
- **Time Template**: Date/time template for file selection using tokens (%y4, %m2, %d2 for year, month, day)
- **Offset Factor**: Value to add to data after reading (or "none")
- **Scale Factor**: Value to multiply data by after reading (or "none")
- **Variable On File**: Name of the variable in the source file
- **File Template**: Path to source file with date tokens for time-varying data

Time tokens include:
- %y4 - 4-digit year
- %m2 - 2-digit month
- %d2 - 2-digit day
- %h2 - 2-digit hour
- %n2 - 2-digit minute

For example, in the QFED configuration:

.. code-block:: none

   SU_BIOMASS NA N Y %y4-%m2-%d2t12:00:00 none 0.7778 biomass ExtData/nexus/QFED/%y4/%m2/qfed2.emis_so2.006.%y4%m2%d2.nc4

This imports SO2 emissions from QFED into the SU_BIOMASS variable, using a scale factor of 0.7778, from files with a date-based naming pattern.

### NEXUS ExtData Configuration

The NEXUS-processed emissions are configured through **ExtData.nexus**, which handles anthropogenic and biogenic emissions from multiple inventories. Example entries:

.. code-block:: none

   # Anthropogenic SO2 from CEDS
   SU_ANTHRO NA N Y %y4-%m2-%d2t12:00:00 none none so2_anthro ExtData/nexus/CEDS/%y4/CEDS.emis_so2.%y4%m2%d2.nc4
   
   # Black carbon from HTAP  
   BC_ANTHRO NA N Y %y4-%m2-%d2t12:00:00 none none bc_anthro ExtData/nexus/HTAP/%y4/HTAP.emis_bc.%y4%m2%d2.nc4

The NEXUS preprocessing system generates these files by:

1. Reading emission inventories (CEDS, HTAP, CAMS) from ``NEXUS_INPUT_DIR``
2. Applying temporal scaling patterns (diurnal, weekly, seasonal)
3. Regridding to the model resolution
4. Outputting model-ready netCDF files with standardized variable names

AERO_HISTORY.rc File Details
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The AERO_HISTORY.rc file controls all diagnostic outputs from the aerosol module. It defines:

1. **Collections**: Groups of variables output together in a single file
2. **Grid configuration**: Output grid specifications (resolution, type)
3. **Output frequency**: How often each collection is written
4. **File formats and templates**: How output files are named and formatted

Basic structure:

.. code-block:: none

   COLLECTIONS: 'inst_aod'
                'inst_du_ss'
                'inst_ca'
                ...

   GRID_LABELS: PC720x361-DC
   ::

   PC720x361-DC.GRID_TYPE: LatLon
   PC720x361-DC.IM_WORLD: 720
   PC720x361-DC.JM_WORLD: 361
   PC720x361-DC.POLE: PC
   PC720x361-DC.DATELINE: DC

For each collection, several parameters are defined:

.. code-block:: none

   inst_aod.format:      'CFIO'
   inst_aod.template:    '%y4%m2%d2_%h2%n2z.nc4'
   inst_aod.mode:        'instantaneous'
   inst_aod.grid_label:  PC720x361-DC
   inst_aod.frequency:   @[inst_aod_freq]
   inst_aod.duration:    010000
   inst_aod.fields:      'DUEXTTAU' , 'GOCART2G' ,
                         'SSEXTTAU' , 'GOCART2G' ,
                         ...

Where:

- **format**: Output file format ('CFIO' for NetCDF)
- **template**: File naming template with time tokens
- **mode**: Output type ('instantaneous' or 'time-averaged')
- **grid_label**: Reference to a grid defined in GRID_LABELS
- **frequency**: How often to output (specified as a variable like @[inst_aod_freq])
- **duration**: How long this collection is active (HHMMSS format)
- **fields**: List of variable name pairs (internal name, component name)

The frequency parameters use variables like ``@[inst_aod_freq]`` that are replaced at runtime with
values from the workflow configuration. These variables are typically set in the emissions
preprocessing step and use the format HHMMSS (hours, minutes, seconds).

Common collections include:

- **inst_aod**: Instantaneous aerosol optical depth
- **inst_du_ss**: Dust and sea salt concentrations
- **inst_ca**: Carbonaceous aerosol (BC/OC) concentrations
- **inst_su**: Sulfate species concentrations
- **inst_3d**: 3D fields for all aerosols
- **inst_2d**: 2D diagnostic fields (column mass, surface concentrations)

To enable specific collections, uncomment them in the COLLECTIONS section and ensure the
corresponding frequency parameters are properly set in your workflow.

Output Products
---------------

GCAFS produces standard meteorological outputs plus comprehensive aerosol fields including:

**Core Aerosol Fields:**
* Aerosol mass concentrations (dust, sea salt, sulfate, black carbon, organic carbon)
* Aerosol optical depth fields at multiple wavelengths
* PM2.5 and PM10 concentrations
* Aerosol extinction coefficients

**Process-Specific Diagnostics:**
* Emission fields from fires and anthropogenic sources (when NEXUS diagnostics enabled)
* Dry and wet deposition fluxes
* Optical properties (single scattering albedo, asymmetry parameter)
* Column-integrated aerosol mass

**Advanced Outputs:**
* 3D aerosol concentrations on model levels
* Aerosol number concentrations
* Full chemical species concentrations when running with chemistry enabled
* NEXUS diagnostic emissions for verification

Output frequency and collections are controlled through the ``AERO_HISTORY.rc`` configuration file,
with standard global-workflow configuration options determining the base output settings.
