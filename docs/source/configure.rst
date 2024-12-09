=============
Configure Run
=============

The global-workflow configs contain switches that change how the system runs. Many defaults are set initially. Users wishing to run with different settings should adjust their $EXPDIR configs and then rerun the ``setup_xml.py`` script since some configuration settings/switches change the workflow/xml ("Adjusts XML" column value is "YES").

+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| Switch           | What                             | Default       | Adjusts XML | More Details                                      |
+==================+==================================+===============+=============+===================================================+
| APP              | Model application                | ATM           | YES         | See case block in config.base for options         |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DEBUG_POSTSCRIPT | Debug option for PBS scheduler   | NO            | YES         | Sets debug=true for additional logging            |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DOIAU            | Enable 4DIAU for control         | YES           | NO          | Turned off for cold-start first half cycle        |
|                  | with 3 increments                |               |             |                                                   |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DOHYBVAR         | Run EnKF                         | YES           | YES         | Don't recommend turning off                       |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DONST            | Run NSST                         | YES           | NO          | If YES, turns on NSST in anal/fcst steps, and     |
|                  |                                  |               |             | turn off rtgsst                                   |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_AWIPS         | Run jobs to produce AWIPS        | NO            | YES         | downstream processing, ops only                   |
|                  | products                         |               |             |                                                   |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_BUFRSND       | Run job to produce BUFR          | NO            | YES         | downstream processing                             |
|                  | sounding products                |               |             |                                                   |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_GEMPAK        | Run job to produce GEMPAK        | NO            | YES         | downstream processing, ops only                   |
|                  | products                         |               |             |                                                   |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_FIT2OBS       | Run FIT2OBS job                  | YES           | YES         | Whether to run the FIT2OBS job                    |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_TRACKER       | Run tracker job                  | YES           | YES         | Whether to run the tracker job                    |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_GENESIS       | Run genesis job                  | YES           | YES         | Whether to run the genesis job                    |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_GENESIS_FSU   | Run FSU genesis job              | YES           | YES         | Whether to run the FSU genesis job                |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_VERFOZN       | Run GSI monitor ozone job        | YES           | YES         | Whether to run the GSI monitor ozone job          |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_VERFRAD       | Run GSI monitor radiance job     | YES           | YES         | Whether to run the GSI monitor radiance job       |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_VMINMON       | Run GSI monitor minimization job | YES           | YES         | Whether to run the GSI monitor minimization job   |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| DO_METP          | Run METplus jobs                 | YES           | YES         | One cycle spinup                                  |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| EXP_WARM_START   | Is experiment starting warm      | .false.       | NO          | Impacts IAU settings for initial cycle. Can also  |
|                  | (.true.) or cold (.false)?       |               |             | be set when running ``setup_expt.py`` script with |
|                  |                                  |               |             | the ``--start`` flag (e.g. ``--start warm``)      |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| HPSSARCH         | Archive to HPPS                  | NO            | NO          | Whether to save output to tarballs on HPPS.       |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| LOCALARCH        | Archive to a local directory     | NO            | NO          | Whether to save output to tarballs locally. For   |
|                  |                                  |               |             | HPSSARCH and LOCALARCH, ARCDIR specifies the      |
|                  |                                  |               |             | directory. These options are mutually exclusive.  |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| ARCH_EXPDIR      | Archive the EXPDIR               | NO            | NO          | Whether to create a tarball of the EXPDIR.        |
|                  |                                  |               |             | ARCH_HASHES and ARCH_DIFFS generate text files    |
|                  |                                  |               |             | of git output that are archived with the EXPDIR.  |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| QUILTING         | Use I/O quilting                 | .true.        | NO          | If .true. choose OUTPUT_GRID as cubed_sphere_grid |
|                  |                                  |               |             | in netcdf or gaussian_grid                        |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
| WRITE_DOPOST     | Run inline post                  | .true.        | NO          | If .true. produces master post output in forecast |
|                  |                                  |               |             | job                                               |
+------------------+----------------------------------+---------------+-------------+---------------------------------------------------+
