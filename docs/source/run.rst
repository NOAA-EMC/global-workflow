###################
Run Global Workflow
###################

Here we will show how you can run an experiment using the Global Workflow. The Global workflow is regularly evolving and the underlying UFS-weather-model that it drives can run many different configurations. So this part of the document will be regularly updated. The workflow as it is configured today can be run as forecast only or cycled (forecast+Data Assimilation). Since cycled mode requires a number of Data Assimilation supporting repositories to be checked out, the instructions for the two modes from initial checkout stage will be slightly different. Apart from this there is a third mode that is rarely used in development mode and is primarily for operational use. This mode switches on specialized post processing needed by the avaiation industry. Since the files associated with this mode are restricted, only few users will have need and/or ability to run in this mode. 

.. toctree::
   :hidden:

   clone.rst
   init.rst

