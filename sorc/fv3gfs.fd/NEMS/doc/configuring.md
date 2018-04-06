Configuring {#configuring}
===========

The coupled NEMS application is highly configurable. During
build-time, i.e. when the NEMS executable is being compiled and
linked, choices are made as to which model and mediator components are
built into the system. The built-in components become accessible
during run-time, allowing the execution of different run
configurations without the need to rebuild NEMS.

Often the build and run configurability of NEMS is hidden from the
user by application or test scripts. However, there are times where it
is useful to understand the technical details on the level of the NEMS
executable.

Build Configuration
-------------------

The NEMS build configuration is accessible through the GNU Make based build system. 

The ocean, sea-ice, ionosphere-plasmasphere model, hydraulic model,
and wave components that are built into the NEMS executable are picked
via the OCN, ICE, IPM, HYD, and WAV variables, respectively. Each of
these variables is a comma separated list of models that can be
specified on the make command line. The currently available options
for each variable are shown below:

*  `ATM = satm, xatm`
*  `LND = slnd, xlnd, lis`
*  `OCN = socn, xocn, mom5, hycom, pom`
*  `ICE = sice, xice, cice`
*  `WAV = swav, xwav, ww3`
*  `IPM = sipm, xipm, ipe`
*  `HYD = shyd, xhyd, wrfhydro`

For each model type the current non-active instance options are listed
below. The definition of these two options is similar to their use in
the 
[Community Earth System Model (CESM)] (http://www2.cesm.ucar.edu/):

+ **Stub components** conform to the
  [NUOPC rules for model components](https://earthsystemcog.org/projects/nuopc/compliance_testing).
  They do not advertise any fields in their `importState` or
  `exportState`. Their primary use is to test control flow between
  components in a driver.

+ **Dead components** conform to the NUOPC rules for model
components. They advertise fields in the `importState` and `exportState`
that are appropriate for the specific model type. Import fields may be
ignored internally. Export fields are filled with data that changes
during time stepping, but has no scientific relevance. Their primary
use is in coupled systems with other dead components to test the data
transfers between components.

All of the variables support the specification of multiple options via
comma separated list on the right-hand side. The default target of the
NEMS makefile (i.e. no target specified) will print out a list of
options with explanation to assist the user with the build
configuration.

Run Configuration
-----------------

During run-time of the NEMS executable, it accesses a file called
nems.configure, which it expects to find in the run directory. This
file specifies the dynamic component selection, and the exact run
sequence to be used. Only models built into the executable at
build-time are accessible during run-time. An error will be triggered
if an unavailable model is selected in nems.configure. The component
selection is based on two variables:

    xxx_model:          abc
    xxx_petlist_bounds: lower upper

Here `xxx` can be `atm`, `ocn`, `ice`, `ipm`, `med`. The `abc`
stands for the actual instance name, e.g. `fv3` or `mom5`. The
lower and upper bounds of the petList specification are integer PET
numbers.

The specification of the run sequence provides the flexibility needed
to cover different coupling scenarios. The format is line based
between special tags:

    runSeq::
        line1
        line2
        ...
        lineN
    ::

There are a number of format options for each line:

* A time loop is introduced by a `@` symbol, followed immediatly by
  the number of seconds of the associated time step.

* A time loop is closed by a single `@` symbol on a line.

* The `RUN` method of model component `xxx` is called by specifying
  just `xxx` on a line. The supported values of `xxx` are the same
  as for the model specification above. A specific RUN phase can be
  provided by adding the phase label to the same line, following the
  model string.

* A connector going from component `xxx` to component `yyy` is
  specified by a line like this: `xxx -> yyy`. An additional
  argument on the same line can be used to specify connection options
  for all of the field handled by the connector. The format and
  supported values of the connection options is documented in the
  NUOPC reference manual.

Here is an example of a run sequence specification with two time scales:

    # Run Sequence #
    runSeq::
        @7200.0
        OCN -> MED
        MED MedPhase_slow
        MED -> OCN
        OCN
        @3600.0
            MED MedPhase_fast_before
            MED -> ATM
            ATM
            ATM -> MED
            MED MedPhase_fast_after
          @
        @
    ::

Anything on a line after the `#` symbol is treated as a comment and
ignored. Indentation in the formatting does not change the meaning of
a line, but is purely used to increase readability.


8.1  Changing the run sequence
-------------------------------

During run-time of the NEMS executable, it accesses a file called nems.configure, which it expects to find in the run directory. This file specifies the dynamic component selection, and the exact run sequence to be used. Only models built into the executable at build-time are accessible during run-time. An error will be triggered if an unavailable model is selected in nems.configure. The component selection is based on two variables:

    xxx_model:     	   abc

    Xxx_petlist_bounds:   lower upper

Here "xxx" can be "atm", "ocn", "ice", "ipm", "med". The "abc" stands for the actual instance name, e.g. "gsm" or "mom5". The lower and upper bounds of the petList specification are integer PET numbers.

The specification of the run sequence provides the flexibility needed to cover different coupling scenarios. The format is line based between special tags:

    runSeq::
    line1
    line2
    ...
    lineN
    ::

There are a number of format options for each line:
* A time loop is introduced by a "@" symbol, followed immediatly by the number of seconds of the associated time step.
* A time loop is closed by a single "@" symbol on a line.
* The RUN method of model component "xxx" is called by specifying just "xxx" on a line. The supported values of "xxx" are the same as for the model specification above. A specific RUN phase can be provided by adding the phase label to the same line, following the model string.
* A connector going from component "xxx" to component "yyy" is specified by a line like this: "xxx -> yyy". An additional argument on the same line can be used to specify connection options for all of the field handled by the connector. The format and supported values of the connection options is documented in the NUOPC reference manual.


    Here is an example of a run sequence specification with two time scales:
    #Run Sequence#
    runSeq::
        @7200.0
            OCN -> MED
            MED MedPhase_slow
            MED -> OCN
            OCN
            @3600.0
            MED MedPhase_fast_before
            MED -> ATM
            ATM
            ATM -> MED
            MED MedPhase_fast_after
            @
        @
    ::

Anything on a line after the "#" symbol is treated as a comment and ignored. Indentation in the formatting does not change the meaning of a line, but is purely used to increase readability.

Adding a model grid with a different resolution
-----------------------------------------------

Changing resolutions in coupled applications is far from an automated process at this point. There are a few reasons for this. One is that each of the components has its own procedure for changing grids, and there is no common interface. There is a constraint that ocean and sea ice grids are the same, which requires a utility to generate ice grids from the ocean grid. Additional utilities are required to generate appropriate initial conditions for all the components on the new grids. Finally, the land masks used by atmosphere, ocean, and sea ice components need to be consistent. This is achieved by having the mediator interpolate the land mask used by the ocean and ice components to the atmosphere grid.

The sequence of steps looks like this:
-# For any component that requires a grid change, use its native procedure to incorporate the new grid. If the ocean resolution changes, generate a new sea ice grid to match the ocean grid.
-# For any component that requires a grid change, generate new initial conditions consistent with the grid.
-# Create a standalone (in NEMS, called side-by-side) compset for the component if one does not exist. Verify correct operation of any component with a new grid and initial conditions running the standalone compset. Create a compset for the target coupled configuration.
-# Run the coupled configuration compset through initialization in order to generate a mask file on the atmosphere grid that is consistent with ocean and ice mask and grids.
-# Use the atmosphere mask file to generate correct initial condition files for the atmosphere using the chgres and orographic utilities.

A run can now be performed with the changed resolution, using the compset for the coupled configuration.

The steps are explained in more detail below.

### Change component grids using their native procedures.

**FV3**	***(Information is needed from FV3 team)***.

**GSM**	***(Information is needed from GSM team)***.

**MOM**

-# Use MOM5 build in ocean grid generation utility “MOM5/src/preprocessing/generate_grids/ocean/ocean_grid_generator.csh” to generate ocean grid specify file (grid_spec.nc) with user’s own resolution setting.
-# Generate ocean initial conditions.
-# Copy grid_spec.nc file and ocean initial conditions to INPUT directory.

**CICE**

-# CICE will use the same grid as the ocean component.
-# CICE requires a grid_file and a kmt_file.
-# The grid file contains the latitudes and longitudes of the corner points as well as the lengths of the North and East faces of the gridcells and the rotation angle from the curvilinear grid to true latitude-longitude grid.
-# The kmt_file contains the land/ocean mask where 1s are ocean and 0s are land.
-# A Fortran code (generateCICEfromFMSgridspec.F90) will take the MOM grid definitions and generate the grid and kmt file for CICE.

### Generate new initial conditions for any components with new grids.

**FV3**	***(Information is needed from FV3 team)***.

**GSM**	***(Information is needed from GSM team)***.

**MOM**

Use “cvtR4p0To5p1” utility to convert NCEP CFSv2 initial conditions (MOM4 based) to   UGCS compatible initial conditions (MOM5 based).

**CICE**

-# Use the NCAR Command Language (NCL) script to take an initial state from CFS and make it CICE compatible.
-# ncl convert_cice5.ncl
-# Expects ice_model.res.nc as input and output is cice5_model.res.nc.

#### 3. Use or create side-by-side (standalone) compsets for any components with new grids and verify correct operation. Create a compset for the target coupled configuration.

[This sheet shows the compsets that already exist](https://docs.google.com/spreadsheets/d/1v9tJb03YuCbwDsXff4M5i6jz4lvBxUrhdImqaGnK_IE/edit#gid=0). (The ones for T574 and T1534 GSM do not)
To create a new compset, see the section called How to Add a New Compset.
To use CompsetRun, see How to Build & Run.

### Generate a land mask for the atmosphere.

Masks from the ocean/ice models are interpolated to the atmosphere grid using a local area conservation scheme in the NEMS mediator. The result is a fractional land mask file, field_med_atm_a_land_mask.nc. The mediator sends this mask to the GSM. Grid cells with values equal to 1 can be considered "all land". Grid cells with values between 0 and 1 will eventually be part land, part ocean/ice.  However, the GSM atmosphere cannot currently handle cells that are part land, part ocean/ice and must create a binary mask representation using a GSM cutoff parameter. This is hardcoded in the routine GSM/phys/do_physics_one_step.f in the following place:

    !  	-> Mask
        fldname='land_mask'
        findex = QueryFieldList(ImportFieldsList,fldname)
        if (importFieldsValid(findex) .and.
    & 	importData(1,1,findex) > -99999.0) then
        do j = 1, lats_node_r
            do i = 1, lonr
                aoi_fld%slimskin(i,j) = 1.0
            if (importData(i,j,findex) \< 0.01) then
                aoi_fld%FICEIN(i,j) = 0.0
                aoi_fld%slimskin(i,j) = 3.0
            endif
            enddo
        enddo

Any cells with a value greater than the cutoff parameter will be land. Currently (for revisions after DREV64441) GSM sets anything greater than 0.01 to land.  In GSM, the points that are associated with ocean and ice following the binary land mask and cutoff parameter receive and use ocean and ice exchange field data from those components.

To generate the field_med_atm_a_land_mask.nc file, you must run the coupled system.  The mediator will generate that file during initialization.  The models must be configured to run on the grids and masks of interest.  The initial conditions and other scientific aspects of the model are not important at this point.  In this case, it's best if no mediator restarts are used to initialize the system.  The goal is to set up a run with the models on the correct grids and masks, to have the coupled system initialize, to have the mediator generate conservative mapping files between grids, for the mediator to interpolate the ocean mask to the atm grid conservatively, and to have that data written to a file.  This will all happen during initialization of the system. The model can abort once initialization is complete. 

In the previous step, a compset that included the coupled target components and grids was created. Use CompsetRun to run this compset through initialization following [How to Build and Run](https://esgf.esrl.noaa.gov/projects/couplednems/quick_build_run). You may need to setup new PET layouts and decompositions for the new grids.  And you need to carefully check the mediator output grid files after this short initial run is complete to verify that the grids in the mediator are consistent with the new grids defined by the models. After the compset runs, you should have the field_med_atm_a_land_mask.nc file in your run directory.

### Run chgres and orographic utilities to get appropriate input files for GSM.

Once the land mask for the atmosphere is available, it is necessary to run the chgres and orography utilities in order to get appropriate input files for GSM. The inputs generated by chgres are siganl and sfcanl, which are initial conditions for the atmospheric model. The siganl file contains the sigma level spherical harmonic spectral coefficients in a binary file of the surface pressure, orography, and remaining model 3D dependent variables and tracers. The sfcanl file contains surface and land surface files in physical space (Gaussian grid) used by the physics component.

In the directory of the files is an ieee file and a GrADS control file that reads and plots the ieee file.  The second record of the ieee file is the sea-land mask made by the code.
The utility chgres uses the sea-land mask (slm) grib file output made by the orography code, if it is present, with the chgres SLMASK exported variable pointing to the Gaussian grib slm file which in turn was made from landmast.txt input to the orography code from the field_med_atm_a_land_mask.nc netcdf file.  In the absence of the export variable SLMASK being set explicitly, the sea-land mask Gaussian file will attempt to be found by chgres in the model fix fields directory and thus be made from the USGS 30" elevations, and the UMD 30" ocean/lake/land mask file.  The same can be expected for the orography file if the export variable OROGRAPHY is not set to a new orography file or the file is not present. The cutoff rule parameter may be changed with recompilation of the orography code.

The sea land mask cutoff parameter (what is the actual parameter name and where is it located?) used to create the set of mask files received on 10/5/15 is believed to be 0, so any cells with value greater than 0 were considered land. This is what has been used to run DREV64441.

After generating the 10/5/15 files, the sea land mask cutoff parameter was set to 0.5, so any cells with a value greater than 0.5 in the fractional netcdf ocean file would be considered land. A new set of mask files was received on 11/17/15 corresponding to the 0.5 cutoff parameter.

Additional files generated by the orography utility and chgres are:

* mtnvar14_126, which is a gwd/mtn blocking file needed for GSM run time fortran number unit 24
* slmgb126, which is the land model file orography code output
* orogb126, which is the orography itself

### File Format:

The headers for the signanl/sfcanl file inputs are of the form:

    sighdr siganl.gfs.2015040100
    jcap
    126
    lonb
    384
    latb
    190
    idate
    2015041500
    stop
    global_sighdr ENDING …
    
and

    sfchdr sfcanl.gfs.2015040100
    lonb
    384
    latb
    190
    idate
    2015041500

###  Potential improvements

*Generating atmosphere land mask offline.* The entire "run the coupled system through initialization" process could be substituted with a preprocessing step that leverages ESMF to generate a conservative mapping file offline to interpolate the ocean mask to the atm grid offline.  That would require the ability to generate ESMF compatible grid files offline for any grids of interest, including corner points in a way that the grid information is identical to the information in the models. Right now, we are using the coupled system to take care of this preprocessing step. 

*Consistency improvement.* It would be preferable if the atmosphere did not choose or determine mask values. The mediator would determine ocean/ice and land grid cells, and send appropriate data. Ideally the mask used by the mediator would be automatically consistent with the mask used for chgres and orography file generation.

*Fractional cells.* The atmospheric model should be able to handle grid cells that are divided into ice/ocean/land portions.
