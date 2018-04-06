NMM Instructions
----------------

### How to use restart:

1. Keep same end time (nhours_fcst: in config file) like in original run.

2. Change restart: argument in config file from false to true
   That's the only change in config file.

3. The only difference from original run script is you don't use
   main_input_filename.  Instead, you use restart_file_XX_nemsio which
   you get from original restart output file
   nmmb_rst_XX_nio_HHHHh_00m_00.00s where XX is the domain ID and HHHH
   is the forecast hour of the restart time.

Limitations:

1. In order to keep bit-identical results, restart must be written
   (used) on full hour

2. Restart cannot be more frequent than history output and must be
   multiplier of history output i.e. if history is written on 3 hours,
   model can be restarted on 3, 6, 9, ... hours (need to be fixed
   later)

TODO:

1. Allow writing restart file at any time in forecast


### How to use time series output in NMMB:

Time series output in NMMB is optional output that is turned on by
providing appropriate namelist file in run directory. The name of that
file must be ts_locations.nml, and the contents of the file is as
follows:

    &TS_LOCATIONS
    NPOINTS=2,
    POINTS_LON=-106.0, -110.0,
    POINTS_LAT=54.0, 50.0
    /

where NPOINTS defines number of locations and POINTS_LON,POINTS_LAT
are arrays of longitudes and latitudes of selected points in degrees
(-180.0 to 180.0).

The output filenames are ts_p01_d01.bin,ts_p02_d01.bin,
ts_p01_d02.bin,ts_p02_d02.bin etc.  The p01 indicates the point number
from 1 to NPOINTS and d01,d02 indicate domain number

The ncarg program tsplot that can be used to plot time series is
located in `/u/wx20du/plot_timeseries`.  It requires a control file as a
command line argument. For example if the control file is named
tsplotsetup_nmm you will need to run:

    $ ./tsplot tsplotsetup_nmm

which will create gmeta file. Sample control file (tsplotsetup_nmm) is
also located in `/u/wx20du/plot_timeseries` directory.


Nesting
-------

The NMM-B has telescoping static and moving nest capability.  All
domains, whether the uppermost parent or any nest, are functionally
equivalent and thus each needs its own configure file.  Both 1-way and
2-way interaction between parent and child domains are available.

### For 1-way nesting:

1. Set 'nest_mode' to '1-way' in all configure files.  The value of
   'generation' is not relevant and can be ignored.
 
2. The uppermost parent's configure file:

    a. The variable 'num_domains_total' must be set in this domain's
       configure file.  This is the total number of domains in the run
       which includes the upper parent plus all nests.  This variable
       does not need to be set in any other configure files (if it is
       set in others it is not read).

    b. Set the value for 'my_domain_id' which must always be 1 for the
       uppermost parent.

    c. Set the value for 'my_parent_id' to -999.

    d. Set 'n_children' to the number of child nests associated with
       the uppermost parent.  This does not include any nests inside
       the first generation of child nests because all interactions
       with nesting involve only a parent and its first generation of
       children.

    e. Set 'my_domain_moves' to false.

3. Static nest configure files:

    a. In each nest's configure file set 'my_domain_id' to a unique
       integer greater than 1.  The user is free to choose these
       integer identifiers in any way desired except that all domain
       IDs must ultimately form a monotonic sequence.  In other words
       if the run contains 2 first generation nests and one of those
       nests contains a nest then the three nests may use any integer
       value between 2 and 4 as their domain ID so that the final IDs
       are 1,2,3, and 4 but never something like 1,2,4,5 which is not
       a monotonic sequence.

    b. Set the value for 'my_parent_id' to the integer ID that was
       given to this nest's parent domain.

    c. Set 'n_children' to the number of child nests inside of this
       nest but not counting any deeper nests inside of those children.

    d. Set 'i_parent_start' and 'j_parent_start' to the I and J
       indices of the H point on the nest's parent's grid that
       coincide with the nest's SW corner H point.  This implies that
       any nest's SW corner must lie directly on a parent grid H
       point.

    e. Set 'parent_child_space_ratio' to the ratio of the size of the
       parent's grid increment to the child's.  Make this an integer.

    f. Set 'input_ready' to true if input data has already been
       produced for this nest.  Set it to false if input data has not
       been produced and the user wants the parent to generate the
       nest's input data.  NPS-generated input data is naturally
       preferable.

    g. Set 'my_domain_moves' to false.

4. Moving nest configure files.  See regrtession test examples: 1tests/nmm_conf/nmm_mnests*conf_*`

    a. Follow all instructions in 3(a)-(f).  

    b. Set 'my_domain_moves' to true.

    c. Set 'ratio_sfc_files' to the ratio of the uppermost parent's
       grid increment to this moving nest's.  Again this should be an
       integer.  The use of moving nests requires the user to generate
       eight different surface-related static datafiles for each
       different resolution of nest in the run.  If there are two
       moving nests with parent_child_space_ratio=3 then a set of the
       following eight files must be pre-generated: ALBASE_ij_3,
       FIS_ij_3, ISLTYP_ij_3, IVGTYP_ij_3, MXSNAL_ij_3, SM_ij_3,
       TG_ij_3, and VEGFRC_ij_3.  These are the base albedo, sfc
       geopotential, soil type, vegetation type, maximum snow albedo,
       sea mask, deep underground temperature, and vegetation
       fraction, respectively, at the 3x nests' resolution but which
       span the entire upper parent domain.

       This data must be present as the nests move across the parent's
       domain.  Then assume one of the 3x moving nests contains a 3x
       moving nest inside it.  In the configure file for the inner
       nest the value of ratio_sfc_files would be 9 and the eight sfc
       datafiles would contain 9x data that spans the entire upper
       parent's domain.  Note that the final integer in these files'
       names must be the value of ratio_sfc_files.

    d. Set the values of 'nrows_p_upd_w', 'nrows_p_upd_e',
       'nrows_p_upd_s', and 'nrows_p_upd_n' to 2.  This is the number
       of rows around the edge of the nest domain that must be updated
       by the parent after the nest moves.  The nest does not use its
       own data in these rows for updating itself because V is not
       present on the north or east sides and some variables needed in
       the integration part of the domain are not computed in these
       rows.

    e. If a moving nest has a child moving nest then for the outer
       nest set the value of 'centers_distance'.  This is the distance
       in units of the outer nest's grid increments that the inner
       nest's center can move from the outer nest's center before the
       outer nest shifts so as to bring their centers very near to
       each other again.

    f. If the uppermost parent domain is global then set the value of
       'latitude_limit'.  If a nest domain (or the outermost nest in a
       telescoping complex of nests) reaches this latitude in either
       hemisphere then the nest stops and never moves again.  Thus the
       nest's delta X cannot become too small due to converging
       meridians which would result in violation of the CFL criterion.

    g. The file called nest.txt must be present in the working
       directory.  The file's 2nd column holds critical specifications
       regarding variables in the Solver internal state when nests
       move.  An explanation is given at the beginning of that file.

5. Task assignment: When 1-way nesting is used then the user assigns
   forecast (compute) tasks and write (quilt) tasks uniquely for each
   domain in that domain's configure file.  The I by J layout of
   forecast tasks are specified with configure variables inpes and
   jnpes, respectively.  Any number of groups of write tasks can be
   assigned with the variable called write_groups.  More than one
   write group should be used if the integration might move from one
   output time to the next before write tasks have finished with the
   earlier output.

   The number of tasks in each write group is assigned with the
   variable called write_tasks_per_group.  The sum of
   `inpes*jnpes+write_groups*write_tasks_per_group` for all domains must
   equal the number of tasks that are assigned to the run in the
   runscript.  This task assignment lets the user fine-tune the
   balance of work being done on all domains to minimize the time that
   any parent or child waits for the other thus leading to all compute
   tasks being busy virtually all the time as all domains integrate
   their forecasts simultaneously.

6. Configure file names: The run script will copy each configure file
   to configure_file_01, configure_file_02, etc. where the final
   integers on the filenames form a monotonic sequence.  The uppermost
   parent's configure file must be used for configure_file_01 but the
   user is not required to make the remaining files' names contain the
   same integer as their corresponding configure files' domain IDs.

### For 2-way nesting

1. Set 'nest_mode' to '2-way' in all configure files.  The integer
   value of each domain's generation must be given to the variable
   called 'generation'.  The generation variable is ignored in 1-way
   mode.

2. The nests.txt file must be present in the working directory.  The
   file's 3rd column specifies which of the Solver's internal state
   variables will be used in the 2-way exchange from child to parent.
   Currently 2-D and 3-D real variables may be selected.  As stated in
   that file's instructions, an H is used to specify that the given
   H-pt variable is to be part of the 2-way exchange while a V
   indicates that the given V-pt variable is to be part of the 2-way
   exchange.

3. The same rules apply for running static or moving nests in 2-way
   nesting as in 1-way nesting described above.
 
4. Task assignments for 2-way interaction cannot be done in the same
   way as they are for 1-way because that would leave too many
   processors idle at any given time as children and parents wait on
   each other to send internal update values and vice versa.
   Therefore the integration in 2-way nesting will continually cycle
   through the generations sequentially but within each generation all
   domains will execute their forecasts concurrently.  To maximize
   overall efficiency the user first decides which generation of
   domains will be the most computationally expensive.  Then ALL
   available compute and forecast tasks in the run are assigned uniquely
   to the domains in that generation where they can be balanced so
   that when this generation executes then all compute tasks will be
   busy.

   As many of the total number of available compute tasks are assigned
   to each of the remaining generations as can be used efficiently,
   i.e., assigning more compute tasks to the generation would not
   decrease runtime or would increase it due to a large amount of halo
   exchanges for task subdomains that are too small.  So that the
   writing of history and restart output remains asynchronous all
   write and quilt tasks must still be assigned uniquely to each
   indiviual domain and cannot be shared among different domains as
   the compute tasks are.  Therefore the sum of `inpes*jnpes` for all
   domains in the most expensive generation plus the sum of
   write_groups times write_tasks_per_group for all domains must equal
   the total number of tasks assigned to the run in the runscript.

### Specifying nest boundary variables

The boundary variables in single-domain regional runs and for the
(regional) upper parent in nested runs are hardwired to PD,T,Q,CW,U,V.
However the user specifies which variables are desired for the nests'
domain boundaries.  This is done through the external nests.txt file.
A column labeled 'BC' in that file is used for this purpose.  The user
places an 'H' or a 'V' in that column for the 2-D, 3-D, or 4-D Solver
internal state H-pt or V-pt variables that will be updated by the
parent(s) on the nest boundaries.  If the desired Solver internal
state variable is not listed in nests.txt then simply add it.  If the
desired variable is not yet in the Solver internal state then see the
section below called 'How to add a new variable'.  The copy of
nests.txt in job and regression_tests currently specifies PD,T,Q,CW,U,V as
the nest boundary variables.

### How to add a new variable:

1. Go to `~/src/atmos/nmm` directory

2. In file module_SOLVER_INTERNAL_STATE.F90 search for similar
   variable; in this case let's use 2D variable ALBEDO as an example.

    a. First command is declaration of the pointer, add your variable
       to that command

    b. Search further and next call is allocating size of your new variable:

            CALL SET_VAR_PTR(int_state%VARS,NV,AF,'ALBEDO',int_state%ALBEDO,(/IMS,JMS/),(/IME,JME/))

    In most cases you'll need just to copy this line and change ALBEDO
       to the name of your variable.

3. Now, your variable is placed in internal state, allocated and given
   initial value to NaN!!!  If you want to initialize a physics
   variable with different number, in same directory open
   module_SOLVER_GRID_COMP.F90, go to subroutine PHYSICS_INITIALIZE,
   search for string "Initialize allocated arrays" and add your
   variable to appropriate place or to the call to the appropriate
   physics scheme's initialization subroutine and initialize with
   desired value.

Same procedure should be done with 3D and 4D arrays.


### Adding variable to the history and/or restart file:

1. If this is a new (non-existing) variable in internal state, go
   through steps 1-3 in previous section.

2. When you have existing variable in internal state, go to
   `~/job/regression_tests` directory, and find the file called
   solver_state.txt.

3. Again let's use ALBEDO as an example, open file solver_state.txt and
   search for ALBEDO.

    a. copy and paste line:

            'ALBEDO'     H       R       O      -      -   T  'Dynamic albedo'

    b. rename ALBEDO to the name of the variable you used in step 2b
      when you added the variable, then give short description in the
      8th column.

4. There are 8 columns in the file: Name History Restart Owned Import
   eXport Time_series Description

    a. If you want your variable in History file, leave letter H in
       second column, if not, just leave dash.

    b. If you want your variable in Restart file, leave letter R in
       third column, if not, just leave dash.

    c. If you want your variable as a part of Time series, leave
       letter T in seventh column, if not, just leave dash.

5. Columns 4, 5 and 6 are part of "ownership suite" and are intended
   to be used for exchange between dynamics and physics without
   necessary (duplicate) allocations.

       - O is for owned
       - X is for export
       - I is for import

   Designate 'O' for most new variables which will tell the code to
   allocate memory for it.  Only if you know the variable will be used
   as an unallocated pointer into another variable that has been
   allocated will you designate a blank ('-').  X/I are used to
   specify which variables must be exported/imported between the
   Solver and the component that handles boundaries and motion of
   nests.  Specify blanks ('-') unless you are certain the new
   variable is required for nests' boundaries and/or motion.
