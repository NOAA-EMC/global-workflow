===============================
Clone and build Global Workflow
===============================

^^^^^^^^^^^^^^^^^^
Quick Instructions
^^^^^^^^^^^^^^^^^^

Quick clone/build/link instructions (more detailed instructions below).

.. note::
   Here we are making the assumption that you are using the workflow to run an experiment and so are working from the authoritative repository. If you are using a development branch then follow the instructions in :doc:`development`. Once you do that you can follow the instructions here with the only difference being the repository/fork you are cloning from.

Clone the `global-workflow` and `cd` into the `sorc` directory:

::

   git clone --recursive https://github.com/NOAA-EMC/global-workflow
   cd global-workflow/sorc

.. _build_examples:

The build_all.sh script can be used to build all required components of the global workflow.  The accepted arguments is a list of systems to be built.  This includes builds for GFS and GEFS forecast-only experiments, GSI and GDASApp-based DA for cycled GFS experiments.  See `feature availability <hpc.html#feature-availability-by-hpc>`__ to see which system(s) are available on each supported system.

::

   ./build_all.sh [gfs] [gefs] [gs] [gdas] [all]

For example, to run GFS experiments with GSI DA, execute:

::

   ./build_all.sh gfs gsi

This builds the GFS, UFS-utils, GFS-utils, WW3 with PDLIB (structured wave grids), UPP, GSI, GSI-monitor, and GSI-utils executables.

For coupled cycling (include new UFSDA) execute:

::

   ./build_all.sh gfs gdas

This builds all of the same executables, except it builds the GDASApp instead of the GSI.

To run GEFS (forecast-only) execute:

::

   ./build_all.sh gefs

This builds the GEFS, UFS-utils, GFS-utils, WW3 *without* PDLIB (unstructure wave grids), and UPP executables.

Once the building is complete, link workflow artifacts such as executables, configuration files, and scripts via

::

   ./link_workflow.sh


^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Clone workflow and component repositories
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

********
Workflow
********

There are several ways to clone repositories from GitHub. Below we describe how to clone the global-workflow using the `https` method.

::

   git clone https://github.com/NOAA-EMC/global-workflow

Check what you just cloned (by default you will have only the `develop` branch):

::

   cd global-workflow
   git branch
   * develop

You now have a cloned copy of the global-workflow git repository. To checkout a branch or tag in your clone:

::

   git checkout --recurse-submodules BRANCH_NAME

.. note::
   Branch must already exist. If it does not you need to make a new branch using the ``-b`` flag:

::

   git checkout -b BRANCH_NAME

The ``checkout`` command will checkout BRANCH_NAME and switch your clone to that branch. Example:

::

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

   ./build_all.sh [-a UFS_app][-k][-h][-v] [list of system(s) to build]
  -a UFS_app:
    Build a specific UFS app instead of the default
  -k:
    Kill all builds immediately if one fails
  -h:
    Print this help message and exit
  -v:
    Execute all build scripts with -v option to turn on verbose where supported

  Lastly, pass to build_all.sh a list of systems to build.  This includes `gfs`, `gefs`, `sfs` (not fully supported), `gsi`, `gdas`, and `all`.

For examples of how to use this script, see :ref:`build examples <build_examples>`.

^^^^^^^^^^^^^^^
Link components
^^^^^^^^^^^^^^^

At runtime the global-workflow needs all pieces in place within the main superstructure. To establish this a link script is run to create symlinks from the top level folders down to component files checked out in ``/sorc`` folders.

After running the checkout and build scripts run the link script:

::

   ./link_workflow.sh [-o]

Where:
   ``-o``: Run in operations (NCO) mode. This creates copies instead of using symlinks and is generally only used by NCO during installation into production.
