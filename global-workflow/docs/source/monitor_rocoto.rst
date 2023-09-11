==================
Monitor ROCOTO Run
==================

Click `here <https://github.com/christopherwharrop/rocoto/wiki/documentation>`__ to view full rocoto documentation on GitHub


^^^^^^^^^^^^^^^^^^
Using command line
^^^^^^^^^^^^^^^^^^

You can use Rocoto commands with arguments to check the status of your experiment. 

Start or continue a run:

::

   rocotorun -d /path/to/workflow/database/file -w /path/to/workflow/xml/file

Check the status of the workflow:

::

   rocotostat -d /path/to/workflow/database/file -w /path/to/workflow/xml/file [-c YYYYMMDDCCmm,[YYYYMMDDCCmm,...]] [-t taskname,[taskname,...]] [-s] [-T]

.. note::
   YYYYMMDDCCmm = YearMonthDayCycleMinute ...where mm/Minute is ’00’ for all cycles currently.

Check the status of a job:

::

   rocotocheck -d /path/to/workflow/database/file -w /path/to/workflow/xml/file -c YYYYMMDDCCmm -t taskname

Force a task to run (ignores dependencies - USE CAREFULLY!):

::

   rocotoboot -d /path/to/workflow/database/file -w /path/to/workflow/xml/file -c YYYYMMDDCCmm -t taskname

Rerun task(s):

::

   rocotorewind -d /path/to/workflow/database/file -w /path/to/workflow/xml/file -c YYYYMMDDCCmm -t taskname

   (If job is currently queued or running rocoto will kill the job. Run rocotorun afterwards to fire off rewound task.)

Set a task to complete (overwrites current state):

::

   rocotocomplete -d /path/to/workflow/database/file -w /path/to/workflow/xml/file -c YYYYMMDDCCmm -t taskname

(Will not kill queued or running job, only update status.)

Several dates and task names may be specified in the same command by adding more -c and -t options. However, lists are not allowed.

^^^^^^^^^^^^^^^^^
Use ROCOTO viewer
^^^^^^^^^^^^^^^^^

An alternative approach is to use a GUI that was designed to assist with monitoring global workflow  experiments that use ROCOTO. It can be found under the ``workflow`` folder in global-workflow.

*****
Usage
*****

::

   ./rocoto_viewer.py -d /path/to/workflow/database/file -w /path/to/workflow/xml/file

.. note::
   Note 1: Terminal/window must be wide enough to display all experiment information columns, viewer will complain if not.

   Note 2: The viewer requires the full path to the database and xml files if you are not in your EXPDIR when you invoke it.

   Note 3: Only ``TERM=xterm`` is supported. You may wish to create a shell function to switch automatically if you are in a different terminal:

   Bash example:

   ::

      function rv {
        oldterm=${TERM};
        export TERM='xterm';
        ${PATH_TO_VIEWER}/rocoto_viewer.py $@;
        export TERM=${oldterm};
      }

*********************
What the viewer shows
*********************

 .. figure:: _static/fv3_rocoto_view.png

  Sample output from Rocoto viewer

The figure above shows a sample output from a Rocoto viewer for a running experiment. Where:

   * First column: cycle (YYYYMMDDCCmm, YYYY=year, MM=month, DD=day, CC=cycle hour, mm=minute)
   * Second column: task name (a "<" symbol indicates a group/meta-task, click "x" when meta-task is selected to expand/collapse)
   * Third column: job ID from scheduler
   * Fourth column: job state (QUEUED, RUNNING, SUCCEEDED, FAILED, or DEAD)
   * Fifth column: exit code (0 if all ended well)
   * Sixth column: number of tries/attempts to run job (0 when not yet run or just rewound, 1 when run once successfully, 2+ for multiple tries up to max try value where job is considered DEAD)
   * Seventh column: job duration in seconds

**************************
How to navigate the viewer
**************************

The rocoto viewer accepts both mouse and keyboard inputs. Click “h” for help menu and more options.

Available viewer commands::

   c = get information on selected job
   r = rewind (rerun) selected job, group, or cycle
   R = run rocotorun
   b = boot (forcibly run) selected job or group
   -> = right arrow key, advance viewer forward to next cycle
   <- = left arrow key, advance viewer backward to previous cycle
   Q = quit/exit viewer

Advanced features:

   * Select multiple tasks at once

      - Click “Enter” on a task to select it, click on other tasks or use the up/down arrows to move to other tasks and click “Enter” to select them as well.
      - When you next choose “r” for rewinding the pop-up window will now ask if you are sure you want to rewind all those selected tasks.

   * Rewind entire group or cycle

      - Group - While group/metatask is collapsed (<) click “r” to rewind whole group/metatask.
      - Cycle - Use up arrow to move selector up past the first task until the entire left column is highlighted. Click “r” and the entire cycle will be rewound.

