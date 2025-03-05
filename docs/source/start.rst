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

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Set up your experiment cron or scron
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Most systems allow users to write to their crontabs.  However, some systems, like Gaea, require users the use of scron.  The setup is very similar, with the only differences being the command (crontab or scrontab) and the entry.


.. note::
   Orion and Hercules currently only support cron on Orion-login-1 and Hercules-login-1, respectively. Cron support for other login nodes is coming in the future.

::

   (crontab|scrontab) -e

or

::

   (crontab|scrontab) $PSLOT.crontab

.. warning::

   The ``(crontab|scrontab) $PSLOT.crontab`` command will overwrite the existing crontab/scrontab file on your login node.  If you are running multiple crons, it is recommend editing the crontab/scrontab file with ``(crontab|scrontab) -e`` command.

Check your crontab settings::

   (crontab|scrontab) -l

Crontab uses following format::

   */5 * * * * /path/to/rocotorun -w /path/to/workflow/definition/file -d /path/to/workflow/database/file

Scrontab instead launches a script and requires SCRON directives to launch an sbatch job with the following format::

   #SCRON --partition=<cron partition>
   #SCRON --account=<your account>
   #SCRON --mail-user=<your email (optional)>
   #SCRON --dependency=singleton
   #SCRON --job-name=${PSLOT}_cron
   #SCRON --output=/path/to/EXPDIR/logs/scron.log
   #SCRON --time=00:10:00

   */5 * * * * /path/to/rocoto/launch/script
