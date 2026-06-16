==============================
Common Errors and Known Issues
==============================

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Reserved Variables Causing Workflow Issues
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Several variables are reserved in the workflow and should not be used as environment variables in your shell. Some of the common ones include (but are not limited to):
``HOMEglobal``, ``machine``, ``ROTDIR``, ``COMROT``, ``COMROOT``, ``COMOUT``, ``COMIN``, ``STMP``, ``PTMP``, ``DATAROOT``, ``DATA``, ``ACCOUNT``, ``PDY``, ``cyc``, ``RUN``, etc.
If you are using any of these variables in your shell, you may encounter unexpected behavior in the workflow.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: AttributeError: 'AttrDict' object has no attribute '__frozen' when running workflow setup
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example::

  ./create_experiment.py -y /path/to/gw/dev/ci/cases/pr/C48_ATM.yaml

  ...

  2026-06-16 03:44:30,587 - INFO     - setup_workflow: BEGIN: setup_workflow.main.main: /path/to/gw/dev/workflow/setup_workflow.py
  Traceback (most recent call last):
    File "/path/to/gw/dev/workflow/./create_experiment.py", line 141, in <module>
      setup_workflow.main(setup_workflow_args)
    File "/path/to/gw/sorc/wxflow/src/wxflow/logger.py", line 278, in wrapper
      retval = func(*args, **kwargs)
               ^^^^^^^^^^^^^^^^^^^^^
    File "/path/to/gw/dev/workflow/setup_workflow.py", line 116, in main
      if not check_dir_writable(base[dk]):
                                ~~~~^^^^
    File "/path/to/gw/sorc/wxflow/src/wxflow/attrdict.py", line 84, in __missing__
      raise KeyError(name)
  KeyError: 'PTMP'

**Cause:** Variable name collision with your environment (usually ``PTMP``)

**Solution:** Make sure your environment does not have one of the above reserved variables set

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: "ImportError" message when running setup script
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example::

   $ ./setup_workflow.py /path/to/your/experiment/directory
   /usr/bin/env: python3: No such file or directory

**Cause:** Missing python module in your environment

**Solution:** Load a python module ("module load python") and retry setup script.

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: curses default colors when running viewer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Example::

   $ ./rocoto_viewer.py -d blah.db -w blah.xml
   Traceback (most recent call last):
     File "./rocoto_viewer.py", line 2376, in <module>
       curses.wrapper(main)
     File "/contrib/anaconda/anaconda2/4.4.0/lib/python2.7/curses/wrapper.py", line 43, in wrapper
       return func(stdscr, *args, **kwds)
     File "./rocoto_viewer.py", line 1202, in main
       curses.use_default_colors()
    _curses.error: use_default_colors() returned ERR

**Cause:** wrong TERM setting for curses

**Solution:** set TERM to "xterm" (bash: export TERM=xterm ; csh/tcsh: setenv TERM xterm)

^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Issue: Directory name change for EnKF folder in ROTDIR
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

**Issue:** The EnKF ROTDIR folders were renamed during the GFS v15 development process to remove the period between "enkf" and "gdas": enkf.gdas.$PDY → enkfgdas.$PDY

**Fix:** Older tarballs on HPSS will have the older directory name with the period between 'enkf' and 'gdas'. Make sure to rename folder to 'enkfgdas.$PDY' after obtaining. Only an issue for the initial cycle.
