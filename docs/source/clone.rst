===============================
Clone and build Global Workflow
===============================

^^^^^^^^^^^^^^^^^^
Quick Instructions
^^^^^^^^^^^^^^^^^^

Quick clone/build/link instructions (more detailed instructions below).

.. note::
   Here we are making the assumption that you are using the workflow to run an experiment and so are working from the authoritative repository. If you are using a development branch then follow the instructions in :doc:`development.rst`. Once you do that you can follow the instructions here with the only difference being the repository/fork you are cloning from.

For forecast-only (coupled or uncoupled):

::

   git clone https://github.com/NOAA-EMC/global-workflow.git
   cd global-workflow/sorc
   ./checkout.sh
   ./build_all.sh
   ./link_workflow.sh

For cycled (w/ data assimilation):

::

   git clone https://github.com/NOAA-EMC/global-workflow.git
   cd global-workflow/sorc
   ./checkout.sh -g
   ./build_all.sh
   ./link_workflow.sh

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Clone workflow and component repositories
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

********
Workflow
********

There are several ways to clone repositories from GitHub. Below we describe how to clone the global-workflow using either the ssh or https methods. **The ssh method is highly preferred and recommended.**

ssh method (using a password protected SSH key):

::

   git clone git@github.com:NOAA-EMC/global-workflow.git

.. note::
   When using ssh methods you need to make sure that your GitHub account is configured for the computer from which you are accessing the repository (See `this link <https://docs.github.com/en/authentication/connecting-to-github-with-ssh/adding-a-new-ssh-key-to-your-github-account>`_)

https method:

::

   git clone https://github.com/NOAA-EMC/global-workflow.git

Check what you just cloned (by default you will have only the develop branch):

::

   cd global-workflow
   git branch
   * develop

You now have a cloned copy of the global-workflow git repository. To checkout a branch or tag in your clone:

::

   git checkout BRANCH_NAME

.. note::
   Branch must already exist. If it does not you need to make a new branch using the ``-b`` flag:

::

   git checkout -b BRANCH_NAME

The ``checkout`` command will checkout BRANCH_NAME and switch your clone to that branch. Example:

::

   git checkout my_branch
   git branch
   * my_branch
   develop

**********
Components
**********

Once you have cloned the workflow repository it's time to checkout/clone its components. The components will be checked out under the ``/sorc`` folder via a script called checkout.sh. Run the script with no arguments for forecast-only:

::

   cd sorc
   ./checkout.sh

Or with the ``-g`` switch to include data assimilation (GSI) for cycling:

::

   cd sorc
   ./checkout.sh -g

Each component cloned via checkout.sh will have a log (``/sorc/logs/checkout-COMPONENT.log``). Check the screen output and logs for clone errors.

^^^^^^^^^^^^^^^^
Build components
^^^^^^^^^^^^^^^^

Under the ``/sorc`` folder is a script to build all components called ``build_all.sh``. After running checkout.sh run this script to build all components codes:

::

   ./build_all.sh [-a UFS_app][-c build_config][-h][-v]
   -a UFS_app:
   Build a specific UFS app instead of the default
   -c build_config:
   Selectively build based on the provided config instead of the default config
   -h:
   Print usage message and exit
   -v:
   Run all scripts in verbose mode

A partial build option is also available via two methods:

  a) modify gfs_build.cfg config file to disable/enable particular builds and then rerun build_all.sh

  b) run individual build scripts also available in ``/sorc`` folder for each component or group of codes

^^^^^^^^^^^^^^^
Link components
^^^^^^^^^^^^^^^

At runtime the global-workflow needs all pieces in place within the main superstructure. To establish this a link script is run to create symlinks from the top level folders down to component files checked out in ``/sorc`` folders.

After running the checkout and build scripts run the link script:

::

   ./link_workflow.sh [-o]

Where:
   ``-o``: Run in operations (NCO) mode. This creates copies instead of using symlinks and is generally only used by NCO during installation into production.

