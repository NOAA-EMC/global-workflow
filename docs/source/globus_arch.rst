.. _experiment-setup:

=================================
Setup Globus Connections for HPSS
=================================

 The Global Workflow archives and retrieves data from HPSS.  Some systems, such as Hera and WCOSS2, have direct connections to HPSS, while others like Hercules do not.  To enable HPSS transfers, RDHPCS Mercury offers temporary disk space and HPSS connections.  The high-throughput Globus protocol is used to schedule and transfer data to Mercury where a service (The Doorman) runs jobs to transfer data to HPSS.  To make use of this service, users must initialize their connections to Globus and Mercury.  This guide provides instructions on how to enable these services.

^^^^^^^^^^^^^^^^^
Setting Up Globus
^^^^^^^^^^^^^^^^^

The Globus service offers extremely fast connections between MSU and RDHPCS machines.  To make use of this service, you will first need to establish connections from the client (e.g. Hercules) and the server (i.e. Mercury).  RDHPCS maintains a guide on this procedure, which can be found in their `Globus Guide <https://docs.rdhpcs.noaa.gov/data/globus_online_data_transfer.html>`__.  The simplest way to establish your connection is by running ``globus login`` (after loading the ``globus-cli`` module).  If you have trouble with this or working through the guide, contact RDHPCS for assistance.

Once you are logged in, verify that the Globus connection is active on the client.  First, load the ``globus-cli`` module, then run ``globus session show``.  You should see an entry for your RDHPCS user account.

To test the connection and verify that your session is active, you can attempt a test transfer:

.. code-block:: bash

   echo "Example" > example.file  # Create a test file
   globus endpoint search rdhpcs#mercury  # Get Mercury's Globus ID
   globus endpoint search msuhpc2#Hercules-dtn  # Get Hercules' Globus ID

   # Transfer the file; this will print a transfer ID if successfully initialized
   globus transfer '<Hercules ID (????????-????-????-????-????????????)>:/full/path/to/example.file' '<Mercury ID >:/collab1/data/<your username>/example.file'
   # Wait on the transfer to complete
   globus task wait <transfer ID>

If the above snippet is successful, then you are good to go.  It's possible that you may need to authenticate and/or update permissions on one or both machines.  Globus should give you step-by-step instructions for this.  If it fails, then please reference the guide and/or email RDHPCS for support.


^^^^^^^^^^^^^^^^^^^^
Common Globus Issues
^^^^^^^^^^^^^^^^^^^^

Note that the globus connection stays active for 7 days.  If your experiment fails in a globus* job, then this may be the culprit.  Try running the following from either an MSU or Mercury terminal: ``globus session update --all``.  You will be prompted to enter a link into a browser and respond with the corresponding confirmation code.  Once this is complete, try rebooting the failing job(s).

For some users, the new system, Mercury, occassionally fails to add all necessary permissions necessary to run globus transfers.  If you receive an error about needing to add ``data_access`` in the logs, then login to Mercury and execute

.. code-block::
    module load globus-cli
    globus session update --all
    # Get the host UUID
    globus endpoint search hercules  # Replace Hercules with the system you are running the global workflow on
    # Below, replace <hercules UUID> with the UUID found in the above command
    globus session consent 'urn:globus:auth:scope:transfer.api.globus.org:all[*https://auth.globus.org/scopes/<hercules UUID>/data_access]'
