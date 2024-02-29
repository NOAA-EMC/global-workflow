==============
Start your run
==============

Make sure a rocoto module is loaded: ``module load rocoto``

If needed check for available rocoto modules on machine: ``module avail rocoto`` or ``module spider rocoto``

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Start your run from within your EXPDIR
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

::

   rocotorun -d $PSLOT.db -w $PSLOT.xml

The first jobs of your run should now be queued or already running (depending on machine traffic). How exciting!

You'll now have a "logs" folder in both your ``ROTDIR`` and ``EXPDIR``. The EXPDIR log folder contains workflow log files (e.g. rocoto command results) and the ``ROTDIR`` log folder will contain logs for each job (previously known as dayfiles).

^^^^^^^^^^^^^^^^^^^^^^^^^^^
Set up your experiment cron
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. note::
   Orion and Hercules currently only support cron on Orion-login-1 and Hercules-login-1, respectively. Cron support for other login nodes is coming in the future.

::

   crontab -e

or

::

   crontab $PSLOT.crontab

.. warning::

   The ``crontab $PSLOT.crontab`` command will overwrite existing crontab file on your login node. If running multiple crons recommend editing crontab file with ``crontab -e`` command.

Check your crontab settings::

   crontab -l

Crontab uses following format::

   */5 * * * * /path/to/rocotorun -w /path/to/workflow/definition/file -d /path/to/workflow/database/file
