===============================
Clone and build Global Workflow
===============================

^^^^^^^^^^^^^^^^^^
Quick Instructions
^^^^^^^^^^^^^^^^^^

Quick clone/build/link instructions (more detailed instructions below).

.. note::
   Here we are making the assumption that you are using the workflow to run an experiment and so are working from the authoritative repository. If you are using a development branch then follow the instructions in :doc:`../contributor_guide/development`. Once you do that you can follow the instructions here with the only difference being the repository/fork you are cloning from.

Clone the `global-workflow` and `cd` into the `sorc` directory:

.. code-block:: bash

   git clone --recursive https://github.com/NOAA-EMC/global-workflow
   cd global-workflow/sorc

.. _build_examples:

The `build_all.sh` script can be used to build all required components of the global workflow.
`build_all.sh` allows for optional flags to modify the build behavior:

   - ``-c``: Build on compute nodes.  The default behavior is to build on the head node.
   - ``-A HPC_ACCOUNT``: Specify the HPC account to be used when building on compute nodes.
   - ``-v``: Execute all build scripts with -v option to turn on verbose where supported
   - ``-h``: Print help message and exit

The accepted arguments is a list of systems to be built.  This includes builds for **GFS**, **GEFS**, **GCAFS**, and **SFS** forecast-only experiments, **GSI** and **GDASApp**-based DA for **cycled GFS** experiments.  See :ref:`feature availability <feature_availability>` to see which system(s) are available on each supported system.

.. code-block:: bash

   ./build_all.sh [gfs] [gefs] [sfs] [gcafs] [gsi] [gdas] [all]

For example, to run GFS experiments with GSI DA, execute:

.. code-block:: bash

   ./build_all.sh gfs gsi

This builds the GFS, UFS-utils, GFS-utils, WW3 with PDLIB (structured wave grids), UPP, GSI, GSI-monitor, and GSI-utils executables.

For coupled cycling (using only new UFSDA) execute:

.. code-block:: bash

   ./build_all.sh gfs gdas

This builds all of the same executables, except it builds the GDASApp instead of the GSI.

To build GEFS (forecast-only) execute:

.. code-block:: bash

   ./build_all.sh gefs

This builds the GEFS, UFS-utils, GFS-utils, WW3 *without* PDLIB (unstructure wave grids), and UPP executables.

To run SFS (forecast-only) execute:

.. code-block:: bash

   ./build_all.sh sfs

This builds the same components as GEFS, except the UFS model is built in hydrostatic mode.

To run GCAFS execute:

.. code-block:: bash

   ./build_all.sh gcafs gdas

This builds everything you need to run GCAFS in forecast-only or cycled mode.

.. code-block:: bash

   ./link_workflow.sh


^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Clone workflow and component repositories
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

********
Workflow
********

There are several ways to clone repositories from GitHub. Below we describe how to clone the global-workflow using the `https` method.

.. code-block:: bash

   git clone https://github.com/NOAA-EMC/global-workflow

Check what you just cloned (by default you will have only the `develop` branch):

.. code-block:: bash

   cd global-workflow
   git branch
   * develop

You now have a cloned copy of the global-workflow git repository. To checkout a branch or tag in your clone:

.. code-block:: bash

   git checkout --recurse-submodules BRANCH_NAME

.. note::

   Branch must already exist. If it does not you need to make a new branch using the ``-b`` flag:

.. code-block:: bash

   git checkout -b BRANCH_NAME

The ``checkout`` command will checkout BRANCH_NAME and switch your clone to that branch. Example:

.. code-block:: bash

   git checkout --recurse-submodules my_branch
   git branch
   * my_branch
   develop

Using ``--recurse-submodules`` is important to ensure you are updating the component versions to match the branch.

^^^^^^^^^^^^^^^^
Build components
^^^^^^^^^^^^^^^^

Under the ``/sorc`` folder is a script to build all components called ``build_all.sh``. After checking out the branch you wish to use, run this script to build all components codes:

::

  ./build_all.sh [-c][-A HPC_ACCOUNT][-h][-v] [list of system(s) to build]
  -c:
    Build on compute nodes.  The default behaviour is to build on the head node.
  -A HPC_ACCOUNT:
    Specify the HPC account to be used when building on compute nodes.
  -h:
    Print this help message and exit
  -v:
    Execute all build scripts with -v option to turn on verbose where supported

Lastly, pass to `build_all.sh` a list of systems to build.  This includes `gfs`, `gefs`, `sfs`, `gcafs`, `gsi`, `gdas`, and `all`.

To configure the build with specific flags or options for the various components, you can update the respective build command in the `build_opts.yaml` file.

For examples of how to use this script, see :ref:`build examples <build_examples>`.

.. _build_options:

^^^^^^^^^^^^^
Build Options
^^^^^^^^^^^^^

**Components built for each build option**

+----------------+-------+-------+-------+-------+-------+-------+-------+
| Component      |  gfs  |  gsi  | gdas  | gefs  |  sfs  | gcafs |  all  |
+================+=======+=======+=======+=======+=======+=======+=======+
| gdas           |       |       |   X   |       |       |       |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| gsi_enkf       |       |   X   |       |       |       |       |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| gsi_monitor    |       |   X   |   X   |       |       |       |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| gsi_utils      |       |   X   |   X   |       |       |   X   |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| nexus          |       |       |       |   X   |       |   X   |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| ufs_model      |  gfs  |       |       |  gefs |  sfs  | gcafs |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| verif-global   |       |       |       |       |       |       |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| ww3_prepost    |   X   |       |       |   X   |       |       |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| wxflow         |       |       |       |       |       |       |       |
+----------------+-------+-------+-------+-------+-------+-------+-------+
| gfs_utils, ufs_utils, and upp are always built                         |
+------------------------------------------------------------------------+

+------------------------------------------------------------------------+
|  UFS model configurations (built with FV3/MOM6/CICE6/WW3 unless noted) |
+============+===========================================================+
| GFS model  | Non-hydrostatic with unstructured wave grid (PDLIB=ON)    |
+------------+-----------------------------------------------------------+
| GEFS model | Non-hydrostatic with structured wave grid (PDLIB=OFF)     |
+------------+-----------------------------------------------------------+
| SFS model  | Hydrostatic (built with waves, but not used)              |
+------------+-----------------------------------------------------------+
| GCAFS      | Non-hydrostatic FV3/GOCART                                |
+------------+-----------------------------------------------------------+

^^^^^^^^^^^^^^^
Link components
^^^^^^^^^^^^^^^

At runtime the global-workflow needs all pieces in place within the main superstructure. To establish this a link script is run to create symlinks from the top level folders down to component files checked out in ``/sorc`` folders.

After running the checkout and build scripts run the link script:

.. code-block:: bash

   ./link_workflow.sh [-o]

Where:
   ``-o``: Run in operations (NCO) mode. This creates copies instead of using symlinks and is generally only used by NCO during installation into production. It uses much more space, and can hide local changes when examining with git.
