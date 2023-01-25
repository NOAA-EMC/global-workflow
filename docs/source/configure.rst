=============
Configure Run
=============

The global-workflow configs contain default settings that match operations. Users wishing to run with different settings should adjust their $EXPDIR configs and then rerun the ``setup_workflow[_fcstonly].py`` script since some configuration settings/switches change the workflow/xml ("Adjusts XML" column value is "YES").

+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| Switch         | What                         | Default       | Adjusts XML | More Details                                      |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| EXP_WARM_START | Is experiment starting warm  | .false.       | NO          | Impacts IAU settings for initial cycle. Can also  |
|                | (.true.) or cold (.false)?   |               |             | be set when running setup_expy[_fcstonly].py      |
|                |                              |               |             | script with --start flag (e.g. --start warm)      |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| RETRO          | Use retrospective parallel   | NO            | NO          | Default of NO will tell getic job to pull from    |
|                | for ICs                      |               |             | production tapes.                                 |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| RUN_CCPP       | Run CCPP                     | NO            | NO          | If YES, set desired suite for CCPP_SUITE          | 
|                |                              |               |             | variable [#]_                                     |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| QUILTING       | Use I/O quilting             | .true.        | NO          | If .true. choose OUTPUT_GRID as cubed_sphere_grid |
|                |                              |               |             | in netcdf or gaussian_grid                        |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| OUTPUT_GRID    | Output grid type             | gaussian_grid | NO          | If gaussian_grid, set OUTPUT_FILE for nemsio or   |
|                |                              |               |             | netcdf                                            |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| OUTPUT_FILE    | Output file type             | netcdf        | NO          | GFSv16 uses netcdf, GFSv15 used nemsio, recommend |
|                |                              |               |             | netcdf                                            |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| WRITE_DOPOST   | Run inline post              | .true.        | NO          | If .true. produces master post output in forecast |
|                |                              |               |             | job                                               |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DOIAU          | Enable 4DIAU for control     | YES           | NO          | New for GFSv16                                    |
|                | with 3 increments            |               |             |                                                   | 
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DOHYBVAR       | Run EnKF                     | YES           | YES         | Don't recommend turning off                       |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DONST          | Run NSST                     | YES           | NO          | If YES, turns on NSST in anal/fcst steps, and     |
|                |                              |               |             | turn off rtgsst                                   |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DO_GLDAS       | Run GLDAS to spin up land    | YES           | YES         | Spins up for 84hrs if sflux files not available   |
|                | ICs                          |               |             |                                                   |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DO_WAVE        | Runs wave jobs and produce   | YES           | YES         | One-way wave coupling, new for GFSv16             |
|                | wave forecast output         |               |             |                                                   |  
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DO_BUFRSND     | Run job to produce BUFR      | NO            | YES         | downstream processing                             |
|                | sounding products            |               |             |                                                   |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DO_GEMPAK      | Run job to produce GEMPAK    | NO            | YES         | downstream processing, ops only                   |
|                | products                     |               |             |                                                   |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DO_AWIPS       | Run jobs to produce AWIPS    | NO            | YES         | downstream processing, ops only                   |
|                | products                     |               |             |                                                   |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| WAFSF          | Run jobs to produce WAFS     | NO            | YES         | downstream processing, ops only                   |
|                | products                     |               |             |                                                   |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DO_VRFY        | Run vrfy job                 | NO            | YES         | Whether to include vrfy job (GSI monitoring,      |
|                |                              |               |             | tracker, VSDB, fit2obs)                           |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DO_METP        | Run METplus jobs             | YES           | YES         | One cycle spinup                                  |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| DO_VSDB        | Run VSDB package             | YES           | NO          | Retiring in GFSv17+                               |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+
| LOCALARCH      | Archive to a local directory | NO            | Possibly    | Instead of archiving data to HPSS, archive to a   |
|                |                              |               |             | local directory, specified by ATARDIR. If         |
|                |                              |               |             | LOCALARCH=YES, then HPSSARCH must =NO. Changing   |
|                |                              |               |             | HPSSARCH from YES to NO will adjust the XML.      |
+----------------+------------------------------+---------------+-------------+---------------------------------------------------+

.. [#] CCPP will become default soon, variable may be removed.     
