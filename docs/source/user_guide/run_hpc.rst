########################################
Running Global Workflow on Supported HPC
########################################

This section describes how to run an experiment using the Global Workflow. The GW continues to evolve, and the underlying weather model (UFS-WM) supports multiple configurations. Therefore, this documentation will be updated periodically to reflect current capabilities.

.. note::

   New users are recommended to try the :doc:`../quick_start` first. Then come back here for more detailed explanation.

The workflow can be executed in two modes:

* **forecast-only** mode
* **cycled mode** (Forecast + Data Assimilation)

Because cycled mode requires a number of DA supporting repositories, the setup procedure differ slightly between the two modes for the initial build stage.

For guidance on testing individual workflow tasks using the CTest framework, refer to :doc:`../contributor_guide/testing`.

.. toctree::

   clone.rst
   init.rst
   setup.rst
   configure.rst
   start.rst
   monitor_rocoto.rst
   view.rst
