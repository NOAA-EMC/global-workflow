======================
View Experiment output
======================

The output from your run will be found in the ``ROTDIR`` you established. This is also where you placed your initial conditions. Within your ``ROTDIR`` your output will be structured in the following hierarchical structure:

.. code::

    ${RUN}.${PDY}/${cyc}
    └ Member (for ensemble)
      └ Data category (conf/analysis/model/product)
        └ Component (atmos/chem/ice/ocean/wave)
          └ Data type
            └ Grid/domain (if any)

An example directory structure may look like (here for the C96C48mx500_S2SW_cyc_gfs case):

.. code::

   C96C48mx500_S2SW_cyc_gfs
   ├── enkfgdas.20211220
   │   ├── 06
   │   │   ├── mem001
   │   │   │   └── model
   │   │   │       └── ocean
   │   │   │           └── restart
   │   │   └── mem002
   │   │       └── model
   │   │           └── ocean
   │   │               └── restart
   │   ├── 12
   │   │   ├── ensstat
   │   │   │   └── model
   │   │   │       └── atmos
   │   │   │           └── history
   │   │   ├── mem001
   │   │   │   ├── analysis
   │   │   │   │   ├── ice
   │   │   │   │   └── ocean
   │   │   │   ├── conf
   │   │   │   └── model
   │   │   │       ├── atmos
   │   │   │       │   ├── history
   │   │   │       │   ├── input
   │   │   │       │   └── restart
   │   │   │       ├── ice
   │   │   │       │   ├── history
   │   │   │       │   └── restart
   │   │   │       ├── med
   │   │   │       │   └── restart
   │   │   │       └── ocean
   │   │   │           ├── history
   │   │   │           └── restart
   │   │   └── mem002
   │   │       ├── analysis
   │   │       │   ├── ice
   │   │       │   └── ocean
   │   │       ├── conf
   │   │       └── model
   │   │           ├── atmos
   │   │           │   ├── history
   │   │           │   ├── input
   │   │           │   └── restart
   │   │           ├── ice
   │   │           │   ├── history
   │   │           │   └── restart
   │   │           ├── med
   │   │           │   └── restart
   │   │           └── ocean
   │   │               ├── history
   │   │               └── restart
   │   └── 18
   │       ├── conf
   │       ├── ensstat
   │       │   └── analysis
   │       │       ├── atmos
   │       │       ├── ice
   │       │       ├── ocean
   │       │       └── snow
   │       ├── mem001
   │       │   └── analysis
   │       │       ├── atmos
   │       │       ├── ice
   │       │       ├── ocean
   │       │       └── snow
   │       └── mem002
   │           └── analysis
   │               ├── atmos
   │               ├── ice
   │               ├── ocean
   │               └── snow
   ├── enkfgfs.20211220
   │   └── 18
   │       ├── conf
   │       ├── ensstat
   │       │   └── analysis
   │       │       ├── atmos
   │       │       ├── ice
   │       │       ├── ocean
   │       │       └── snow
   │       ├── mem001
   │       │   └── analysis
   │       │       ├── atmos
   │       │       ├── ice
   │       │       ├── ocean
   │       │       └── snow
   │       └── mem002
   │           └── analysis
   │               ├── atmos
   │               ├── ice
   │               ├── ocean
   │               └── snow
   ├── fix
   ├── gdas.20211220
   │   ├── 06
   │   │   └── model
   │   │       ├── ocean
   │   │       │   └── restart
   │   │       └── wave
   │   │           └── restart
   │   ├── 12
   │   │   ├── analysis
   │   │   │   ├── atmos
   │   │   │   ├── ice
   │   │   │   └── ocean
   │   │   ├── conf
   │   │   ├── model
   │   │   │   ├── atmos
   │   │   │   │   ├── history
   │   │   │   │   ├── input
   │   │   │   │   ├── master
   │   │   │   │   └── restart
   │   │   │   ├── ice
   │   │   │   │   ├── history
   │   │   │   │   └── restart
   │   │   │   ├── med
   │   │   │   │   └── restart
   │   │   │   ├── ocean
   │   │   │   │   ├── history
   │   │   │   │   └── restart
   │   │   │   └── wave
   │   │   │       ├── history
   │   │   │       ├── prep
   │   │   │       └── restart
   │   │   └── products
   │   │       └── atmos
   │   │           └── grib2
   │   │               ├── 0p25
   │   │               ├── 0p50
   │   │               └── 1p00
   │   └── 18
   │       ├── analysis
   │       │   ├── atmos
   │       │   ├── ice
   │       │   ├── ocean
   │       │   │   ├── bump
   │       │   │   └── diags
   │       │   └── snow
   │       ├── bmatrix
   │       │   ├── ice
   │       │   └── ocean
   │       ├── conf
   │       ├── model
   │       │   ├── atmos
   │       │   │   └── restart
   │       │   └── wave
   │       │       └── prep
   │       └── obs
   ├── gfs.20211220
   │   └── 18
   │       ├── analysis
   │       │   ├── atmos
   │       │   ├── ice
   │       │   ├── ocean
   │       │   │   ├── bump
   │       │   │   └── diags
   │       │   └── snow
   │       ├── bmatrix
   │       │   ├── ice
   │       │   └── ocean
   │       ├── conf
   │       ├── model
   │       │   └── wave
   │       │       └── prep
   │       └── obs
   └── logs
       ├── 2021122012
       └── 2021122018

+-----------+-----------------------------------------------------------+
| Category  | Contents                                                  |
+===========+===========================================================+
| analysis* | analysis files                                            |
+-----------+-----------------------------------------------------------+
| bmatrix*  | background error for analysis                             |
+-----------+-----------------------------------------------------------+
| conf      | select configuration files, mostly forecast namelists     |
+-----------+-----------------------------------------------------------+
| model     | direct input/output from the forecast, including restarts |
+-----------+-----------------------------------------------------------+
| obs*      | observations used for data assimilation                   |
+-----------+-----------------------------------------------------------+
| products  | derived products typically published                      |
+-----------+-----------------------------------------------------------+
| \* Not present for forecast-only cases                                |
+-----------------------------------------------------------------------+
