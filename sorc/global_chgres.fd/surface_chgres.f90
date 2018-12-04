 module surface_chgres
!$$$ module documentation block
!
! module: surface_chgres    interpolate land fields from one
!                           grid to another.
!   prgmmr: gayno           org: w/np2     date: 2005-10-25
!
! abstract: a collection of routines to interpolate land fields
!           from one grid to another.
!
! program history log:
!  2005-10-25  gayno   - initial version
!  2006-04-25  gayno   - use ipolates library to interpolate
!                        continous fields.  modified for use
!                        with gfs and wrf.
!  2011-07-01  Moorthi - Added unfiltered orography for angulation correction
!
! usage: use surface_chgres
!
! attributes:
!   langauge: fortran 90
!
!$$$
!-----------------------------------------------------------------------
! some variable definitions.  
!
! climo_fields_opt       option for determining climo fields on
!                        output grid.  1 ONLY!!
!                        1-interpolate all from input grid
!                        2-interpolate veg, soil, slope type
!                          from input grid.  others from
!                          sfccycle program.
!                        3-all from sfccycle program.
! landice_opt            1-no landice input grid -> landice output grid
!                        2-landice input grid -> landice output grid
!                        3-no landice input grid -> no landice output grid
!                        4-landice input grid -> no landice output grid
!                        5-landice output grid regardless of whether 
!                          input grid has landice or not.
!-----------------------------------------------------------------------

 integer, private                 :: climo_fields_opt  ! only for 1
 integer, private                 :: landice_opt

 integer, allocatable, private    :: iindx_output(:)
 integer, allocatable, private    :: jindx_output(:)

 real, private                    :: mdl_res_input   ! model resol in degrees
 real, private                    :: mdl_res_output  ! model resol in degrees

!-----------------------------------------------------------------------
! these are flag values for veg and soil type at land ice points.
! they depend on the raw data source used.
!
! zobler soil type -> 9
! statsgo soil type -> 16
! usgs veg type -> 24
! sib veg type -> 13
! igbp veg type -> 15
!-----------------------------------------------------------------------

 integer, private                 :: veg_type_ice 
 integer, private                 :: veg_type_ice_input
 integer, private                 :: soil_type_ice

!-----------------------------------------------------------------------
! note: "_input" refers to the input grid, "_output" refers 
! to the output grid, "_output_ext" refers to data on output grid 
! from an external process, such as sfccycle or gayno's wrf si.
!
! smcref_input/output    onset of soil moisture stress
! smcdry_input/output    air dry soil moisture limit
! smcwilt_input/output   plant wilting point
! smclow_input/output    soil moisture scalar multiplier
! smchigh_input/output   soil moisture scalar multiplier
! smcmax_input/output    maximum soil moisture content
! beta_input/output      soil 'b' parameter
! psis_input/output      saturated soil potential
! satdk_input/output     saturated soil hydraulic conductivity
!-----------------------------------------------------------------------

 integer, parameter, private      :: max_soil_types=50
 integer, parameter, private      :: max_veg_types=50
 real, private                    :: salp_output
 real, private                    :: snup_output(max_veg_types)

 real, private                    :: beta_input(max_soil_types)
 real, private                    :: beta_output(max_soil_types)
 real, private                    :: psis_input(max_soil_types)
 real, private                    :: psis_output(max_soil_types)
 real, private                    :: satdk_input(max_soil_types)
 real, private                    :: satdk_output(max_soil_types)
 real, private                    :: smcdry_input(max_soil_types)
 real, private                    :: smcdry_output(max_soil_types)
 real, private                    :: smchigh_input
 real, private                    :: smchigh_output
 real, private                    :: smclow_input
 real, private                    :: smclow_output
 real, private                    :: smcmax_input(max_soil_types)
 real, private                    :: smcmax_output(max_soil_types)
 real, private                    :: smcref_input(max_soil_types)
 real, private                    :: smcref_output(max_soil_types)
 real, private                    :: smcwilt_input(max_soil_types)
 real, private                    :: smcwilt_output(max_soil_types)

!----------------------------------------------------------------------
! note: "_input" refers to the input grid, "_output" refers 
! to the output grid, "_output_ext" refers to data on output grid 
! from an external process, such as sfccycle or gayno's wrf si.
! 
! when the modis data/new radiation treatment (ialb=1) is 
! selected, these arrays hold (gfs):
! ---------------------------------------------------------------------
!
! alnsf            - near ir blacksky albedo 
! alnwf            - near ir whitesky albedo
! alvsf            - visible blacksky albedo
! alvwf            - visible whitesky albedo
!
! ---------------------------------------------------------------------
! when the one degree data/older radiation treatment (ialb=0) is 
! selected, these arrays hold (gfs):
! ---------------------------------------------------------------------
!
! alnsf            - near ir albedo, strong cosz dependence 
! alnwf            - near ir albedo, weak cosz dependence
! alvsf            - vis albedo, strong cosz dependence
! alvwf            - vis albedo, weak cosz dependence
!
! ---------------
! other variables:
! ---------------
!
! albedo           - include effects of snow cover - wrf/nam
! canopy_mc        - canopy moisture content
! facsf            - fraction, strong cosz dependence
! facwf            - fraction, weak cosz dependence
! sea_ice_fract    - sea ice fraction, decimal
! greenfrc         - greenness fraction
! greenfrc_max     - max annual greenness fraction
! greenfrc_min     - min annual greenness fraction
! sea_ice_depth    - sea ice depth
! lsmask           - land/sea mask
! mxsnow_alb       - maximum snow albedo
! orog             - orography
! sea_ice_flag     - yes/no sea ice flag
! skin_temp        - skin temperature, sst over water
! snow_depth       - physical snow depth
! snow_free_albedo - wrf/nam radiation package
! snow_liq_equiv   - liq equivalent snow depth
! soilm_liq        - liquid soil moisture
! soilm_tot        - total soil moisture
! soil_temp        - soil temperature
! slope_type       - soil slope type (category)
! soil_type        - soil type (category)
! substrate_temp   - soil substrate temperature
! veg_type         - vegetation type (category)
! z0               - roughness length
!-----------------------------------------------------------------------

 integer, allocatable, private              :: slope_type_output_ext(:)
 integer, allocatable, private              :: soil_type_output_ext(:)
 integer, allocatable, private              :: veg_type_output_ext(:)

 real, allocatable, private                 :: alnsf_output_ext(:)
 real, allocatable, private                 :: alnwf_output_ext(:)
 real, allocatable, private                 :: alvsf_output_ext(:)
 real, allocatable, private                 :: alvwf_output_ext(:)
 real, allocatable, private                 :: facsf_output_ext(:)
 real, allocatable, private                 :: facwf_output_ext(:)
 real, allocatable, private                 :: greenfrc_output_ext(:)                         
 real, allocatable, private                 :: greenfrc_max_output_ext(:)                         
 real, allocatable, private                 :: greenfrc_min_output_ext(:)                         
 real, allocatable, private                 :: mxsnow_alb_output_ext(:)                         
 real, allocatable, private                 :: snow_free_albedo_output_ext(:)
 real, allocatable, private                 :: substrate_temp_output_ext(:)                         
 real, allocatable, private                 :: z0_output_ext(:)                         

!-----------------------------------------------------------------------
! these structures are to be used by the program that uses this
! module to hold the input and output data.
!-----------------------------------------------------------------------

 type, public :: sfc1d
   real, allocatable    :: albedo(:)
   real, allocatable    :: alnsf(:)
   real, allocatable    :: alnwf(:)
   real, allocatable    :: alvsf(:)
   real, allocatable    :: alvwf(:)
   real, allocatable    :: canopy_mc(:)
   real, allocatable    :: facsf(:)
   real, allocatable    :: facwf(:)
   real, allocatable    :: sea_ice_fract(:)
   real, allocatable    :: greenfrc(:)
   real, allocatable    :: greenfrc_max(:)
   real, allocatable    :: greenfrc_min(:)
   real, allocatable    :: sea_ice_depth(:)
   real, allocatable    :: lats(:)
   real, allocatable    :: lons(:)
   real, allocatable    :: lsmask(:)
   real, allocatable    :: mxsnow_alb(:)
   real, allocatable    :: orog(:)
   real, allocatable    :: sea_ice_temp(:)
   real, allocatable    :: skin_temp(:)
   real, allocatable    :: snow_depth(:)
   real, allocatable    :: snow_free_albedo(:)
   real, allocatable    :: snow_liq_equiv(:)
   real, allocatable    :: soilm_liq(:,:)
   real, allocatable    :: soilm_tot(:,:)
   real, allocatable    :: soil_temp(:,:)
   real, allocatable    :: substrate_temp(:)
   real, allocatable    :: z0(:)
   integer, allocatable :: sea_ice_flag(:)
   integer, allocatable :: slope_type(:)
   integer, allocatable :: soil_type(:)
   integer, allocatable :: veg_type(:)
 end type sfc1d

 type, public ::  sfc2d
   real, allocatable    :: alnsf(:,:)
   real, allocatable    :: alnwf(:,:)
   real, allocatable    :: alvsf(:,:)
   real, allocatable    :: alvwf(:,:)
   real, allocatable    :: canopy_mc(:,:)
   real, allocatable    :: facsf(:,:)
   real, allocatable    :: facwf(:,:)
   real, allocatable    :: sea_ice_fract(:,:)
   real, allocatable    :: greenfrc(:,:)
   real, allocatable    :: greenfrc_max(:,:)
   real, allocatable    :: greenfrc_min(:,:)
   real, allocatable    :: sea_ice_depth(:,:)
   real, allocatable    :: lsmask(:,:)
   real, allocatable    :: mxsnow_alb(:,:)
   real, allocatable    :: orog(:,:)
   real, allocatable    :: sea_ice_temp(:,:)
   real, allocatable    :: skin_temp(:,:)
   real, allocatable    :: snow_depth(:,:)
   real, allocatable    :: snow_liq_equiv(:,:)
   real, allocatable    :: snow_free_albedo(:,:)
   real, allocatable    :: soilm_liq(:,:,:)
   real, allocatable    :: soilm_tot(:,:,:)
   real, allocatable    :: soil_temp(:,:,:)
   real, allocatable    :: substrate_temp(:,:)
   real, allocatable    :: z0(:,:)
   integer, allocatable :: sea_ice_flag(:,:)
   integer, allocatable :: slope_type(:,:)
   integer, allocatable :: soil_type(:,:)
   integer, allocatable :: veg_type(:,:)
 end type sfc2d

 contains

 subroutine surface_chgres_driver(imdl_output, jmdl_output,   &
                                  ijmdl_output, nsoil_output, &
                                  kgds_output, output,        &
                                  imdl_input, jmdl_input,     &
                                  orog_uf,use_ufo, nst_anl,   &
                                  nsoil_input, hour, month,   &
                                  day, year, fhour,           &
                                  kgds_input, input, ialb,    &
                                  isot, ivegsrc, tile_num, merge, iret)
!$$$ subprogram documentation block
!
! subprogram: surface_chgres_driver  driver routine for this module
!   prgmmr: gayno           org: w/np2     date: 2005-10-19
!
! abstract: call some prep routines, then call main interpolation
!           routine.
!
! program history log:
!  2005-10-19  gayno   - initial version
!  2006-04-25  gayno   - created common interface for gfs and wrf.
!
! usage: call surface_chgres_driver(imdl_output, jmdl_output,   &
!                                   ijmdl_output, nsoil_output, &
!                                   lonsperlat_output,          &
!                                   kgds_output, output,        &
!                                   imdl_input, jmdl_input,     &
!                                   nsoil_input, hour, month,   &
!                                   day, year, fhour,           &
!                                   kgds_input, input, ialb,    &
!                                   isot, ivegsrc, merge, iret)
!   input arguments:
!     day                    cycle day
!     fhour                  forecast hour
!     hour                   cycle hour
!     imdl_input             i-dimension, input grid
!     imdl_output            i-dimension, output grid
!     input                  land data on input grid
!     jmdl_input             j-dimension, input grid
!     jmdl_output            j-dimension, output grid
!     kgds_input             grib grid description section of input grid
!     kgds_output            grib grid description section of output grid
!     lonserplat_output      number of i points in each j row, output grid
!     month                  cycle month
!     merge                  used for regional model.  set to false for gfs
!     nsoil_input            number of soil layers, input grid
!     nsoil_output           number of soil layers, output grid
!     year                   cycle year
!     ialb                   when '1', use new bosu albedo.  when '0',
!                            use old albedo
!     isot                   when '1', use new statsgo soil stype.
!                            when '0', use zobler soi type
!     ivegsrc                when '1', use new igbp vegetation type.
!                            when '2', use sib vegetation type
!   outputs:
!     iret                   error status, non-zero if there is a
!                            problem in this module.
!     output                 land data on output grid
!
! subprograms called:
!   setup                - read program namelist, calculate soil parameters
!   get_ext_climo_global - call sfccycle program to get climo
!                          fields on output grid
!   call interp          - interpolate and initialize land states on
!                          output grid
! attributes:
!   langauge: fortran 90
!
 implicit none

 type(sfc2d)                       :: input
 type(sfc1d)                       :: output

 integer, intent(in)               :: hour, month, day, year, ialb, isot, ivegsrc
 integer                           :: i, j, ij, ii, jj
 integer, intent(in)               :: imdl_input
 integer, intent(in)               :: imdl_output
 integer, intent(in)               :: ijmdl_output
 integer, intent(inout)            :: iret
 integer, intent(in)               :: jmdl_input
 integer, intent(in)               :: jmdl_output
 integer, intent(in)               :: kgds_input(200)
 integer, intent(in)               :: kgds_output(200)
 integer, intent(in)               :: nsoil_input
 integer, intent(in)               :: nsoil_output
 integer, intent(in)               :: tile_num

 logical, intent(in)               :: merge

 real, intent(in)                  :: fhour
 real                              :: r
 real, intent(in)                  :: orog_uf(ijmdl_output)
 logical,intent(in)                :: use_ufo, nst_anl

!-----------------------------------------------------------------------
! perform various setup tasks.
!-----------------------------------------------------------------------

 iret = 0  ! becomes non-zero if there is an error in this module.

 print*,"- CALL SETUP ROUTINE"
 call setup (kgds_input, input, imdl_input, jmdl_input, imdl_output, iret)
 if (iret /= 0) return

!-----------------------------------------------------------------------
! the output arrays are 1-d.  keep track of their 2-d indices for
! various diagnostics.
!
! Get any output grid fixed fields (such as greenness) that are not
! to be interpolated from the input grid.  for gfs, we call the
! sfccycle program, for nmm we read them in from grib files.
!-----------------------------------------------------------------------

 allocate (iindx_output(ijmdl_output))
 allocate (jindx_output(ijmdl_output))
 ij = 0
 do j = 1,jmdl_output
   do i = 1, imdl_output
     ij=ij+1
     iindx_output(ij)=i
     jindx_output(ij)=j
   enddo
 enddo

 print*,'- CALL CYCLE TO GET SURFACE STATIC/CLIMO FIELDS ON OUTPUT GRIDS.'
 call get_ext_climo_global(ijmdl_output, output%lsmask, output%orog, &
                           orog_uf,use_ufo,nst_anl,                  &
                           output, hour, month, day, year, fhour,    &
                           ialb, isot, ivegsrc, iindx_output,        &
                           jindx_output, tile_num)

 print*,"- CALL INTERP ROUTINE"

 call interp (imdl_input, jmdl_input, kgds_input, ijmdl_output,  &
              nsoil_input, nsoil_output,  &
              input, output, imdl_output, jmdl_output, & 
              kgds_output, ialb, merge, iret)


 deallocate(iindx_output,jindx_output)

 return

 end subroutine surface_chgres_driver

 subroutine interp (imdl_input, jmdl_input,     &
                    kgds_input, ijmdl_output,   &
                    nsoil_input, nsoil_output,  &
                    input, output, imdl_output, &
                    jmdl_output, kgds_output, ialb, merge, iret)
!$$$ subprogram documentation block
!
! subprogram: interp        interpolate land states
!   prgmmr: gayno           org: w/np2     date: 2005-10-19
!
! abstract: interpolate land states from one grid to another.
!           discreet fields (such as soil/veg type) and fields
!           that are a function of these discreet fields are
!           always interpolated via nearest neighbor.  other
!           continuous fields are interpolated by budget,
!           nearest neighbor or bilinear depending on the
!           resolutions of the input and output grids.  one
!           exception is snow, which is never interpolated
!           using bilinear.
!
! program history log:
!  2005-10-19  gayno   - initial version
!  2006-04-25  gayno   - modified to use ipolates library.
!                        modified to handle both gfs and nmm.
!
! usage: call interp (imdl_input, jmdl_input,     &
!                     kgds_input, ijmdl_output,   &
!                     nsoil_input, nsoil_output,  &
!                     input, output, imdl_output, &
!                     jmdl_output, kgds_output, merge, iret)
!
!   input arguments:
!     ialb                   1-modis albedo data
!     imdl_input             i-dimension, input grid
!     imdl_output            i-dimension, output grid
!     ijmdl_output           number of grid points, output grid
!     input                  land data on input grid
!     jmdl_input             j-dimension, input grid
!     jmdl_output            j-dimension, output grid
!     kgds_input             grib grid description section of input grid
!     kgds_output            grib grid description section of output grid
!     merge                  set to false for gfs
!     nsoil_input            number of soil layers, input grid
!     nsoil_output           number of soil layers, output grid
!   outputs:
!     iret                   error status, non-zero if a problem
!     output                 land data on output grid
!
! subprograms called:
!   gdswzd                 - convert from lat/lon to x/y on a gaussian grid
!   ll2xy_egrid            - convert from lat/lon to x/y on an e-grid grid
!   find_nn_new            - finds nearest neighbor input point for each
!                            output point that is the same type (land,
!                            landice, non-land)
!   adjust_soilt_for_orog  - adjust soil temperature for differences in
!                            orography between input and output grids
!   calc_albedo            - calculate albedo based on snow cover
!   calc_liq_soilm         - calculate liquid soil moisture
!   rescale_soilm          - rescale soil moisture for changes in 
!                            soil type
!               
! attributes:
!   langauge: fortran 90
!
 use ll2xy_utils, only       : ll2xy_egrid

 use interp_utils, only      : find_nn_new

 use soil_utils, only        : rescale_soilm, adjust_soilt_for_orog, &
                               calc_liq_soilm, calc_albedo
 
 use gdswzd_mod

 implicit none

 character*6                       :: grid_type
 real, parameter                   :: frz_ice=271.2, frz_h20=273.16
 integer                           :: count_land_output, count_nonland_output
 integer                           :: count_sea_ice_output
 integer, parameter                :: flag_value = -999
 integer                           :: i,j, ij, ii, jj, n
 integer, allocatable              :: ibi(:), ibo(:)
 integer, intent(in)               :: ijmdl_output
 integer, allocatable              :: ijsav_land_output(:), ijsav_nonland_output(:), &
                                      ijsav_sea_ice_output(:)
 integer, intent(in)               :: ialb, imdl_input, jmdl_input
 integer, intent(in)               :: imdl_output, jmdl_output
 integer                           :: ipopt(20), int_opt, no 
 integer                           :: ipopt_snow(20), int_opt_snow
 integer, allocatable              :: ipts(:,:),jpts(:,:)
 integer, intent(inout)            :: iret
 integer, intent(in)               :: kgds_input(200), kgds_output(200)
 integer                           :: kgds_output_tmp(200), kgdso1 
 integer, allocatable              :: mask_input(:,:)
 integer, allocatable              :: mask_output(:)
 integer                           :: nsoil, nret
 integer, intent(in)               :: nsoil_input
 integer, intent(in)               :: nsoil_output
 integer, allocatable              :: nn_iindx_wrt_input_grid(:)
 integer, allocatable              :: nn_jindx_wrt_input_grid(:)
 integer, allocatable              :: soil_type_sav(:)

 logical, intent(in)               :: merge

 real                              :: center_lat_input, center_lon_input
 real                              :: dx_input, dy_input
 real, allocatable                 :: input_dat(:,:,:)
 real, allocatable                 :: lats_land_output(:), lons_land_output(:)
 real, allocatable                 :: lats_nonland_output(:), lons_nonland_output(:)
 real, allocatable                 :: lats_sea_ice_output(:), lons_sea_ice_output(:)
 real, allocatable                 :: lsmask_output_temp(:)
 real, allocatable                 :: orog_sav(:)
 real, allocatable                 :: output_data_nonland(:), output_data_land(:)
 real, allocatable                 :: output_data_sea_ice(:), output_data_land2(:,:)
 real, allocatable                 :: output_data_sea_ice2(:,:)
 real, allocatable                 :: snow_m(:)
 real, allocatable                 :: soilm_sav(:,:)
 real, allocatable                 :: xindx_wrt_input_grid(:)
 real, allocatable                 :: yindx_wrt_input_grid(:)

 logical*1, allocatable            :: bitmap_land_input(:,:) 
 logical*1, allocatable            :: bitmap_land_output(:)
 logical*1, allocatable            :: bitmap_sea_ice_input(:,:) 
 logical*1, allocatable            :: bitmap_sea_ice_output(:) 
 logical*1, allocatable            :: bitmap_nonland_input(:,:)
 logical*1, allocatable            :: bitmap_nonland_output(:)
 logical*1, allocatable            :: bitmap_land_input2(:,:,:)
 logical*1, allocatable            :: bitmap_land_output2(:,:)
 logical*1, allocatable            :: bitmap_sea_ice_input2(:,:,:)
 logical*1, allocatable            :: bitmap_sea_ice_output2(:,:)
 logical                           :: rescale_soil_moist
 logical                           :: sea_ice_defaults
 logical                           :: veg_from_input

 type(sfc2d)                       :: input
 type(sfc1d)                       :: output

!-----------------------------------------------------------------------
! the following variables are setup for use by the ipolates routines. 
!
! to properly handle coastlines, the ipolates routines are passed
! the land and non-land points separately.
!-----------------------------------------------------------------------

 iret = 0

 count_land_output=0    
 count_nonland_output=0 
 do ij = 1, ijmdl_output
   if (output%lsmask(ij) > 0.0) then
     count_land_output=count_land_output+1
   else
     count_nonland_output=count_nonland_output+1
   endif
 enddo

!-----------------------------------------------------------------------
! note: there are separate options for handling snow (avoid 
!       bilinear method).  Since IPOLATES does recognize the fv3
!       grid, you can't use the budget method.
!-----------------------------------------------------------------------

 ipopt=0

 if (mdl_res_input <= (0.75*mdl_res_output)) then
   print*,"- INTERPOLATE CONTINUOUS DATA FIELDS USING BILINEAR METHOD."
   kgds_output_tmp = kgds_output
   kgdso1 = -1  ! for subsection of model grid.
   int_opt = 0
   ipopt(1)=1
   ipopt(2) = nint(1.0 / mdl_res_input) + 1   ! search box width of 1 deg.
   int_opt_snow = 2    ! use neighbor method instead for snow.
   ipopt_snow = 0
   ipopt_snow(1) = nint(1.0 / mdl_res_input) + 1   ! search box width of 1 deg.
 else 
   print*,"- INTERPOLATE CONTINUOUS DATA FIELDS USING NEIGHBOR METHOD."
   ipopt(1) = nint(1.0 / mdl_res_input) + 1   ! search box width of 1 deg.
   kgdso1 = -1  ! for subsection of model grid.
   int_opt = 2
   int_opt_snow = int_opt
   ipopt_snow = ipopt
 end if

!-----------------------------------------------------------------------
! set up bitmap to tell ipolates where the land and non-land points
! are on the input and output grids.
!-----------------------------------------------------------------------

 allocate(bitmap_land_output(count_land_output))
 bitmap_land_output = .false.
 allocate(output_data_land(count_land_output))
 output_data_land=0.0

 allocate(ijsav_land_output(count_land_output))
 allocate(lats_land_output(count_land_output))
 allocate(lons_land_output(count_land_output))

 count_land_output=0
 do ij = 1, ijmdl_output
   if (output%lsmask(ij) > 0.0) then
     count_land_output=count_land_output+1
     ijsav_land_output(count_land_output)=ij
     lats_land_output(count_land_output)=output%lats(ij)
     lons_land_output(count_land_output)=output%lons(ij)
   endif
 enddo

 allocate(bitmap_land_input(imdl_input,jmdl_input))
 bitmap_land_input=.false.
 where(input%lsmask > 0.0) bitmap_land_input=.true.   

! non-land

 allocate(output_data_nonland(count_nonland_output))
 output_data_nonland=0.0
 allocate(bitmap_nonland_output(count_nonland_output))
 bitmap_nonland_output = .false.

 allocate(ijsav_nonland_output(count_nonland_output))
 allocate(lats_nonland_output(count_nonland_output))
 allocate(lons_nonland_output(count_nonland_output))

 count_nonland_output=0
 do ij = 1, ijmdl_output
   if (output%lsmask(ij) == 0.0) then
     count_nonland_output=count_nonland_output+1
     ijsav_nonland_output(count_nonland_output)=ij
     lats_nonland_output(count_nonland_output)=output%lats(ij)
     lons_nonland_output(count_nonland_output)=output%lons(ij)
   endif
 enddo

 allocate(bitmap_nonland_input(imdl_input,jmdl_input))
 bitmap_nonland_input=.false.
 where(input%lsmask == 0.0) bitmap_nonland_input=.true.   

 allocate(bitmap_sea_ice_input(imdl_input,jmdl_input))  ! sea ice
 bitmap_sea_ice_input=.false.
 where(input%sea_ice_flag == 1) bitmap_sea_ice_input=.true.

!-----------------------------------------------------------------------
! given the grib gds info of the input grid, and the lats and lons of
! output grid, calculate the x/y location of the output grid points
! with respect to the input grid.
!-----------------------------------------------------------------------

 allocate (xindx_wrt_input_grid(ijmdl_output))
 allocate (yindx_wrt_input_grid(ijmdl_output))

 print*,"- DETERMINE CORRESPONDING X/Y ON INPUT GRID" 
 if(kgds_input(1) == 4) then
   call gdswzd(kgds_input, -1, ijmdl_output, -999.9,   &
               xindx_wrt_input_grid,        &
               yindx_wrt_input_grid,        &
               output%lons, output%lats, nret)
   grid_type="global"
 elseif(kgds_input(1) == 203) then
   center_lat_input = float(kgds_input(7)) * 0.001
   center_lon_input = float(kgds_input(8)) * 0.001
   dx_input = -(float(kgds_input(199)) * 0.00001)
   dy_input = float(kgds_input(200)) * 0.00001
! because of the e-grid's stagger, the routine ll2xy_egrid routine
! outputs nearest i/j whereas the gaussian routine (based on ipolates)
! outputs a float value.  the rest of this module expects a float,
! so convert temp arrays i/jpts to a float value after routine call.
   allocate (ipts(imdl_input,jmdl_input))
   allocate (jpts(imdl_input,jmdl_input))
   call ll2xy_egrid(output%lats, output%lons, imdl_input, jmdl_input, &
                    center_lat_input, center_lon_input, dx_input, dy_input, &
                    imdl_output, jmdl_output, ipts, jpts)
   xindx_wrt_input_grid = reshape(float(ipts),(/ijmdl_output/))
   yindx_wrt_input_grid = reshape(float(jpts),(/ijmdl_output/))
   deallocate(ipts,jpts)
   grid_type="egrid"
 end if

!-----------------------------------------------------------------------
! landice points are based on the vegetation type.  so, need to 
! handle this field first.
!-----------------------------------------------------------------------

 veg_from_input=.true.   ! get veg_type from input grid
 if (allocated(veg_type_output_ext)) then
   print*,'- REPLACE VEG TYPE WITH EXTERNAL DATA.'
   output%veg_type = 0
   where (output%lsmask > 0.0) output%veg_type = veg_type_output_ext
   veg_from_input=.false.  ! veg_type from externally prepared process.
                           ! don't get from input grid
   deallocate (veg_type_output_ext)
 end if

!-----------------------------------------------------------------------
! for each point on the output grid, find the nearest neighbor
! point on the input grid.  if output point is land (landice, water), 
! the nearest neighbor will always be land (landice, water).
!-----------------------------------------------------------------------

 allocate (nn_iindx_wrt_input_grid(ijmdl_output))
 allocate (nn_jindx_wrt_input_grid(ijmdl_output))

 allocate (mask_output(ijmdl_output))
 allocate (mask_input(imdl_input,jmdl_input))

 if (landice_opt == 2 .and. .not.(veg_from_input)) then
   do j = 1, jmdl_input
   do i = 1, imdl_input
     if (input%lsmask(i,j) > 0.0) then
       if (input%veg_type(i,j) == veg_type_ice_input) then
         mask_input(i,j) = 2
       else
         mask_input(i,j) = 1
       end if
     else
       mask_input(i,j) = 0
     endif
   enddo
   enddo
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0) then
       if (output%veg_type(ij) == veg_type_ice) then
         mask_output(ij) = 2
       else
         mask_output(ij) = 1
       end if
     else
       mask_output(ij) = 0
     endif
   enddo
 else
   do j = 1, jmdl_input
   do i = 1, imdl_input
     if (input%lsmask(i,j) > 0.0) then
       mask_input(i,j) = 1
     else
       mask_input(i,j) = 0
     endif
   enddo
   enddo
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0) then
       mask_output(ij) = 1
     else
       mask_output(ij) = 0
     endif
   enddo
 end if

 print*,"- CALC NEAREST NEIGHBOR POINTS."
 call find_nn_new(imdl_input, jmdl_input, mask_input,          &
                  ijmdl_output, mask_output,                   &
                  flag_value, grid_type, mdl_res_input, merge, &
                  iindx_output, jindx_output,                  &
                  xindx_wrt_input_grid, yindx_wrt_input_grid,  &
                  nn_iindx_wrt_input_grid,                     &
                  nn_jindx_wrt_input_grid)

 deallocate (mask_output)
 deallocate (mask_input)
 deallocate (xindx_wrt_input_grid)
 deallocate (yindx_wrt_input_grid)

!-----------------------------------------------------------------------
! if user selects, interpolate veg type from input grid.  always
! use nearest neighbor approach on this discreet field.
!-----------------------------------------------------------------------

 if (veg_from_input) then
   print*,"- INTERPOLATE VEG TYPE FROM INPUT GRID"
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) == 0.0) then ! non-land points
         output%veg_type(ij) = 0   
     else
       if ( (nn_iindx_wrt_input_grid(ij) /= flag_value)   .and.  &
            (nn_jindx_wrt_input_grid(ij) /= flag_value) ) then
         ii = nn_iindx_wrt_input_grid(ij)
         jj = nn_jindx_wrt_input_grid(ij)
         output%veg_type(ij) = input%veg_type(ii,jj)
       else  ! no nearest neighbor that is land.  use a default.
         output%veg_type(ij) = 6
       endif
     end if
   enddo
 end if

!-----------------------------------------------------------------------
! sea ice flag...and when using sea ice model - fraction and depth.
! logic is as follows: 
!
! no ice model input -> no ice model output : interpolate ice flag
! as 0 or 100% coverage.
!
! no ice model input -> ice model output : interpolate ice flag
! as 0 or 100% coverage.  set fract and depth to default values.
!
! ice model input -> ice model output : interpolate ice fraction.
! if > 50%, set ice flag to yes.  interpolate ice depth.  ensure
! consistency with ice flag.
!
! ice model input -> no ice model output : interpolate ice fraction.
! if > 50%, set ice flag to yes.
!-----------------------------------------------------------------------

 if (.not. allocated (input%sea_ice_fract)) then  !input grid is pre-seaice model
   print*,"- INTERPOLATE SEA ICE FLAG FROM INPUT GRID."
   bitmap_nonland_output=.false.
   output_data_nonland=0.0
   kgds_output_tmp=kgds_output
   kgds_output_tmp(1) = kgdso1
   no=count_nonland_output
   allocate(ibo(1))
   allocate(input_dat(imdl_input,jmdl_input,1))
   input_dat(:,:,1)=float(input%sea_ice_flag)
   call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
                (imdl_input*jmdl_input), count_nonland_output,    &
                 1, 1, bitmap_nonland_input, input_dat,  &
                 no, lats_nonland_output, lons_nonland_output, ibo, &
                 bitmap_nonland_output, output_data_nonland, iret)
   if (iret /= 0) then
     print*,'- ERROR IN IPOLATES ',iret
     return
   endif
   deallocate(ibo)
   output%sea_ice_flag = 0 ! land
   do ij = 1, count_nonland_output
     if (bitmap_nonland_output(ij)) then
       output%sea_ice_flag(ijsav_nonland_output(ij))=nint(output_data_nonland(ij))  ! 50% or greater 
     else
       if(abs(lats_nonland_output(ij)) > 55.0) then  ! use latitude based default
         output%sea_ice_flag(ijsav_nonland_output(ij))=1  ! search failed, use default
       else
         output%sea_ice_flag(ijsav_nonland_output(ij))=0  ! search failed, use default
       end if
     endif
   enddo
   deallocate(input_dat)
   sea_ice_defaults=.true.
 else  ! input grid used sea ice model,
   print*,"- INTERPOLATE SEA ICE FRACTION FROM INPUT GRID."
   bitmap_nonland_output=.false.
   output_data_nonland=0.0
   kgds_output_tmp=kgds_output
   kgds_output_tmp(1) = kgdso1
   allocate(ibo(1))
   no=count_nonland_output
   call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
                (imdl_input*jmdl_input), count_nonland_output,       &
                 1, 1, bitmap_nonland_input, input%sea_ice_fract,  &
                 no, lats_nonland_output, lons_nonland_output, ibo,  &
                 bitmap_nonland_output, output_data_nonland, iret)
   if (iret /= 0) then
     print*,'- ERROR IN IPOLATES ',iret
     return
   endif
   deallocate(ibo)
   output%sea_ice_flag = 0  ! land
   do ij = 1, count_nonland_output
     if (bitmap_nonland_output(ij)) then
       if (output_data_nonland(ij) >= .15) then  ! make this a variable?
         output%sea_ice_flag(ijsav_nonland_output(ij))=1
       else
         output%sea_ice_flag(ijsav_nonland_output(ij))=0
       endif 
     else  ! use a default value
       if(abs(lats_nonland_output(ij)) > 55.0) then
         output%sea_ice_flag(ijsav_nonland_output(ij))=1
       else
         output%sea_ice_flag(ijsav_nonland_output(ij))=0
       end if
     endif
   enddo
   sea_ice_defaults=.false.
   if (allocated(output%sea_ice_fract)) then  ! output grid to use sea ice model
     output%sea_ice_fract = 0.0 ! land
     do ij = 1, count_nonland_output
       if (bitmap_nonland_output(ij)) then
         if (output_data_nonland(ij) >= .15) then  ! make this a variable?
           output%sea_ice_fract(ijsav_nonland_output(ij))=output_data_nonland(ij)
         else
           output%sea_ice_fract(ijsav_nonland_output(ij))=0.0
         endif 
       else
         if (output%sea_ice_flag(ijsav_nonland_output(ij)) == 1) then
           output%sea_ice_fract(ijsav_nonland_output(ij))=1.0
         end if
       endif
     enddo
   end if
 end if

!-----------------------------------------------------------------------
! now that we know the sea ice on the output grid, set the
! mask for ice for future ipolates calls.
!-----------------------------------------------------------------------

 count_sea_ice_output=0
 do ij = 1, ijmdl_output
   if (output%sea_ice_flag(ij) == 1) then
     count_sea_ice_output=count_sea_ice_output+1
   endif
 enddo

 if (count_sea_ice_output > 0) then

 allocate(output_data_sea_ice(count_sea_ice_output))
 output_data_sea_ice=0.0
 allocate(bitmap_sea_ice_output(count_sea_ice_output))
 bitmap_sea_ice_output = .false.

 allocate(ijsav_sea_ice_output(count_sea_ice_output))
 allocate(lats_sea_ice_output(count_sea_ice_output))
 allocate(lons_sea_ice_output(count_sea_ice_output))

 count_sea_ice_output=0
 do ij = 1, ijmdl_output
   if (output%sea_ice_flag(ij) == 1) then
     count_sea_ice_output=count_sea_ice_output+1
     ijsav_sea_ice_output(count_sea_ice_output)=ij
     lats_sea_ice_output(count_sea_ice_output)=output%lats(ij)
     lons_sea_ice_output(count_sea_ice_output)=output%lons(ij)
   endif
 enddo

 end if

!------------------------------------------------------------------------
! output grid to use sea ice model.  
!
! if sea_ice_defaults logical is true, then the input grid did not
! run with the sea ice model, so need to set fract and depth to
! default values.
!
! if logical is false, then input grid did run with ice model.
! fraction was calculated above, so now interpolate depth.
!------------------------------------------------------------------------
 if (allocated(output%sea_ice_fract) .and. allocated(output%sea_ice_depth)) then
   if (count_sea_ice_output == 0) then
     output%sea_ice_fract = 0.0
     output%sea_ice_depth = 0.0
   elseif (sea_ice_defaults) then
     print*,"- INITIALIZE SEA ICE FRACTION AND DEPTH WITH DEFAULT VALUES"
     do ij = 1, ijmdl_output
       if (output%sea_ice_flag(ij) == 1) then
         output%sea_ice_fract(ij) = 1.0  
         if (output%lats(ij) > 0.0) then
           output%sea_ice_depth(ij) = 3.0  ! in meters
         else
           output%sea_ice_depth(ij) = 1.5  ! in meters
         end if
       else
         output%sea_ice_fract(ij) = 0.0
         output%sea_ice_depth(ij) = 0.0
       endif
     enddo
   else
     print*,"- INTERPOLATE SEA ICE DEPTH FROM INPUT GRID."
     bitmap_sea_ice_output=.false.
     output_data_sea_ice=0.0
     kgds_output_tmp=kgds_output
     kgds_output_tmp(1) = kgdso1
     allocate(ibo(1))
     no=count_sea_ice_output
     call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
                  (imdl_input*jmdl_input), count_sea_ice_output,               &
                   1, 1, bitmap_sea_ice_input, input%sea_ice_depth,  &
                   no, lats_sea_ice_output, lons_sea_ice_output,  &
                   ibo, bitmap_sea_ice_output,     &
                   output_data_sea_ice, iret)
     if (iret /= 0) then
       print*,'- ERROR IN IPOLATES ',iret
       return
     endif
     deallocate(ibo)
     output%sea_ice_depth = 0.0  ! open water/land
     do ij = 1, count_sea_ice_output
       if (bitmap_sea_ice_output(ij)) then 
         output%sea_ice_depth(ijsav_sea_ice_output(ij)) = &
                              output_data_sea_ice(ij)
       else  ! use a default value
         if (output%sea_ice_flag(ijsav_sea_ice_output(ij)) == 1) then
           output%sea_ice_depth(ijsav_sea_ice_output(ij))=1.5
         end if
       endif
     enddo
   endif
 end if

!-----------------------------------------------------------------------
! always use externally generated substrate temps as these are
! tied to the terrain.  when running with land ice options, ensure
! it is below freezing.  note: for wrf grids, this field is read in
! the driver and passed in.  so, only need to do land ice option check.
!-----------------------------------------------------------------------

 if (kgds_output(1) == 4 .or. kgds_output(1) == 0) then ! gaussian/latlon grid
   output%substrate_temp = substrate_temp_output_ext
   deallocate (substrate_temp_output_ext)
 end if

 if (landice_opt == 1 .or. landice_opt == 2) then
   print*,"- ENSURE SUBSTRATE TEMP BELOW FREEZING AT LAND ICE."
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0 .and.  &
         output%veg_type(ij) == veg_type_ice) then
       output%substrate_temp(ij) = min(output%substrate_temp(ij), frz_h20)
     endif
   enddo
 endif

 if (kgds_output(1) == 4 .or. kgds_output(1) == 0) goto 77 ! gaussian/latlon grid

 where (output%lsmask == 0.0) output%substrate_temp = 280.0 ! water flag
 where (output%sea_ice_flag == 1) output%substrate_temp = frz_ice ! sea ice flag

 77 continue

!-----------------------------------------------------------------------
! treat cmc as discreet field because it is a function of veg type.
!-----------------------------------------------------------------------
 print*,"- INTERPOLATE CANOPY MOISTURE CONTENT"
 do ij = 1, ijmdl_output
   if (output%lsmask(ij) == 0.0) then  ! non-land points 
     output%canopy_mc(ij) = 0
   else
     if ( (nn_iindx_wrt_input_grid(ij) /= flag_value)   .and.  &
          (nn_jindx_wrt_input_grid(ij) /= flag_value) ) then
       ii = nn_iindx_wrt_input_grid(ij)
       jj = nn_jindx_wrt_input_grid(ij)
       output%canopy_mc(ij) = input%canopy_mc(ii,jj)
     else  ! no nearest neighbor that is land.  use default.
       output%canopy_mc(ij) = 0
     endif
   endif
 enddo

!-----------------------------------------------------------------------
! treat soil moist as discreet field because it is a function of
! soil type.  note: may want to consider other interpolation 
! methods in the future as long as they use a soil type "mask."
!-----------------------------------------------------------------------
 print*,"- INTERPOLATE TOTAL SOIL MOISTURE"
 do ij = 1, ijmdl_output
   if (output%lsmask(ij) == 0.0) then  ! non-land points 
     output%soilm_tot(ij,:) = 1.0
   else
     if ( (nn_iindx_wrt_input_grid(ij) /= flag_value)   .and.  &
          (nn_jindx_wrt_input_grid(ij) /= flag_value) ) then
       ii = nn_iindx_wrt_input_grid(ij)
       jj = nn_jindx_wrt_input_grid(ij)
       if (nsoil_output == nsoil_input) then
         do n = 1, nsoil_output
           output%soilm_tot(ij,n) = input%soilm_tot(ii,jj,n)
         enddo
       elseif (nsoil_output > nsoil_input) then
         output%soilm_tot(ij,1) = input%soilm_tot(ii,jj,1)
         do n = 2, nsoil_output
           nsoil = min(n,nsoil_input)
           output%soilm_tot(ij,n) = input%soilm_tot(ii,jj,nsoil)
         enddo
       else  ! logic hardwired for 4->2 layers with thicknesses of
             ! 0-.1,.1-.4,.4-1.0,1.0-2.0 and 0-.1,1.0-2.0 meters. 
         output%soilm_tot(ij,1) = input%soilm_tot(ii,jj,1)
         output%soilm_tot(ij,2) =(0.3*input%soilm_tot(ii,jj,2) + &
                                  0.6*input%soilm_tot(ii,jj,3) + &
                                  1.0*input%soilm_tot(ii,jj,4))/1.9
       endif
     else  ! no nearest neighbor that is land.  use default.
           ! will be overwritten later if landice.
       output%soilm_tot(ij,:)  = 0.2 
     endif
   endif
 enddo

!-----------------------------------------------------------------------
! treat roughness as a discreet field as it is tied to vegetation
! type over land. (over water, it is a state variable, so might want
! to consider an approach other than nearest neighbor someday.)
!------------------------------------------------------------------------
 print*,"- INTERPOLATE Z0 FROM INPUT GRID."
 do ij = 1, ijmdl_output
   if ( (nn_iindx_wrt_input_grid(ij) /= flag_value)   .and.  &
        (nn_jindx_wrt_input_grid(ij) /= flag_value) ) then
     ii = nn_iindx_wrt_input_grid(ij)
     jj = nn_jindx_wrt_input_grid(ij)
     output%z0(ij) = input%z0(ii,jj)
   else  ! use a default value
     if (output%lsmask(ij) > 0.0) then  ! points with land
       output%z0(ij)           = 30.0      ! cm
     else   
       if (output%sea_ice_flag(ij) == 1) then
         output%z0(ij)           = 1.0       ! cm
       else       ! open water
         output%z0(ij)           = 0.01      ! cm
       end if
     endif
   endif
 enddo
!-----------------------------------------------------------------------
! replace interpolated z0 with externally generated z0
! on the output grid (if this data was read in).  the externally
! generated data is only valid over land.  
!-----------------------------------------------------------------------
 if (allocated(z0_output_ext)) then
   print*,'- REPLACE Z0 WITH EXTERNAL DATA AT LAND POINTS.'
   where(output%lsmask > 0.0) output%z0 = z0_output_ext
   deallocate (z0_output_ext)
 end if

!-----------------------------------------------------------------------
! skin temperature
!-----------------------------------------------------------------------
 print*,"- INTERPOLATE SKIN TEMPERATURE FROM INPUT GRID."
 bitmap_land_output=.false.
 output_data_land=0.0
 kgds_output_tmp=kgds_output
 kgds_output_tmp(1) = kgdso1
 allocate(ibo(1))
 no=count_land_output
 call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
              (imdl_input*jmdl_input), count_land_output,               &
               1, 1, bitmap_land_input, input%skin_temp,  &
               no, lats_land_output, lons_land_output, ibo,  &
               bitmap_land_output, output_data_land, iret)
 if (iret /= 0) then
   print*,'- ERROR IN IPOLATES ',iret
   return
 endif
 deallocate(ibo)
 output%skin_temp= 0.0 
 do ij = 1, count_land_output
   if (bitmap_land_output(ij)) then
     output%skin_temp(ijsav_land_output(ij))=output_data_land(ij)
   else  ! default value
     output%skin_temp(ijsav_land_output(ij)) = &
                       output%substrate_temp(ijsav_land_output(ij))
   endif
 enddo
! now do over non-land.  note that skint is a mix of ice and open water temp.
 bitmap_nonland_output=.false.
 output_data_nonland=0.0
 kgds_output_tmp=kgds_output
 kgds_output_tmp(1) = kgdso1
 allocate(ibo(1))
 no=count_nonland_output
 call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
              (imdl_input*jmdl_input), count_nonland_output,               &
               1, 1, bitmap_nonland_input, input%skin_temp,  &
               no, lats_nonland_output, lons_nonland_output, ibo,  &
               bitmap_nonland_output, output_data_nonland, iret)
 if (iret /= 0) then
   print*,'- ERROR IN IPOLATES ',iret
   return
 endif
 deallocate(ibo)
 do ij = 1, count_nonland_output
   if (bitmap_nonland_output(ij)) then
     output%skin_temp(ijsav_nonland_output(ij))=output_data_nonland(ij)
   else
     if(abs(lats_nonland_output(ij)) >= 60.0) then
       output%skin_temp(ijsav_nonland_output(ij)) = 273.16
     elseif(abs(lats_nonland_output(ij)) <= 30.0) then
       output%skin_temp(ijsav_nonland_output(ij)) = 300.0
     else
       output%skin_temp(ijsav_nonland_output(ij)) = (-.8947)*(abs(lats_nonland_output(ij))) + 326.84
     endif
   endif
 enddo

! at sea ice points, don't let skin temp go above the freezing point.
! at open water points, don't let sst go below freezing point.

 do ij = 1, ijmdl_output
   if (output%lsmask(ij) == 0.0) then
   if (output%sea_ice_flag(ij) == 1) then
     output%skin_temp(ij) = min(output%skin_temp(ij),frz_h20)
   else
     output%skin_temp(ij) = max(output%skin_temp(ij),(frz_ice+.01))
   endif
   endif
 enddo

!-----------------------------------------------------------------------
! set sea ice temperature.  relationship between skin temp and
! sea ice temp is:
!   skint = icefract*tice + (1-icefract)*271.21
! where 271.21K is the temp of any open water in the grid cell.
!-----------------------------------------------------------------------
 if (allocated(output%sea_ice_temp)) then
   do ij = 1, ijmdl_output
     output%sea_ice_temp(ij) = output%skin_temp(ij)
     if(output%sea_ice_flag(ij) == 1) then
       output%sea_ice_temp(ij) = (output%skin_temp(ij)                     &
         -(frz_ice)*(1.-output%sea_ice_fract(ij)))/output%sea_ice_fract(ij)
     end if
   enddo
 end if

!-----------------------------------------------------------------------
! soil temperature over land.  treat as discreet field.
!-----------------------------------------------------------------------
 allocate (orog_sav(ijmdl_output))
 orog_sav=0.0
 do n=1, nsoil_output
   output%soil_temp(:,n)=output%skin_temp  ! flag value open water
 enddo
 do ij = 1, ijmdl_output
   if (output%lsmask(ij) > 0.0) then
     if ( (nn_iindx_wrt_input_grid(ij) /= flag_value)   .and.  &
          (nn_jindx_wrt_input_grid(ij) /= flag_value) ) then
       ii = nn_iindx_wrt_input_grid(ij)
       jj = nn_jindx_wrt_input_grid(ij)
       if (nsoil_output == nsoil_input) then
         do n = 1, nsoil_output
           output%soil_temp(ij,n) = input%soil_temp(ii,jj,n)
         enddo
       elseif (nsoil_output > nsoil_input) then
         output%soil_temp(ij,1) = input%soil_temp(ii,jj,1)
         do n = 2, nsoil_output
           nsoil = min(n,nsoil_input)
           output%soil_temp(ij,n) = input%soil_temp(ii,jj,nsoil)
         enddo
       else  ! logic hardwired for 4->2 layers with thicknesses of
             ! 0-.1,.1-.4,.4-1.0,1.0-2.0 and 0-.1,1.0-2.0 meters. 
         output%soil_temp(ij,1) = input%soil_temp(ii,jj,1)
         output%soil_temp(ij,2) =(0.3*input%soil_temp(ii,jj,2) +  &
                                  0.6*input%soil_temp(ii,jj,3) +  &
                                  1.0*input%soil_temp(ii,jj,4))/1.9
       endif
       orog_sav(ij) = input%orog(ii,jj)
     else
       print*,'- *WARNING* SETTING TO DEFAULT VALUES AT POINT ', &
                  iindx_output(ij), jindx_output(ij)
       orog_sav(ij)            = output%orog(ij) ! turn off soil t adjustment
       output%soil_temp(ij,:)  = output%substrate_temp(ij)
     endif
   endif
 enddo
!-----------------------------------------------------------------------
! "soil" temperature over sea ice.  treat as continuous field.
! ipolates expects the number of ice levels to be the same.
! hence, the temporary variables for the input and output
! grids are both allocated to nsoil_input.  the vertical
! interpolation to the output grid occurs in the logic
! after the ipolates call.
!-----------------------------------------------------------------------

 if (count_sea_ice_output > 0) then

 print*,"- INTERPOLATE SEA ICE COLUMN TEMPS FROM INPUT GRID."
 allocate(bitmap_sea_ice_output2(count_sea_ice_output,nsoil_input))
 bitmap_sea_ice_output2=.false.
 allocate(bitmap_sea_ice_input2(imdl_input,jmdl_input,nsoil_input))
 do n=1, nsoil_input
   bitmap_sea_ice_input2(:,:,n)=bitmap_sea_ice_input
 enddo
 allocate(output_data_sea_ice2(count_sea_ice_output,nsoil_input))
 output_data_sea_ice2=0.0
 allocate(input_dat(imdl_input,jmdl_input,nsoil_input))
 do n=1, nsoil_input
   input_dat(:,:,n)=input%soil_temp(:,:,n)
 enddo 
 kgds_output_tmp=kgds_output
 kgds_output_tmp(1) = kgdso1
 no=count_sea_ice_output
 allocate(ibo(nsoil_input))
 allocate(ibi(nsoil_input))
 ibi=1
 call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
               (imdl_input*jmdl_input), count_sea_ice_output,               &
                nsoil_input, ibi, bitmap_sea_ice_input2, input_dat,  &
                no, lats_sea_ice_output, lons_sea_ice_output, ibo,  &
                bitmap_sea_ice_output2, output_data_sea_ice2, iret)
 if (iret /= 0) then
   print*,'- ERROR IN IPOLATES ',iret
   return
 endif
 deallocate (ibi, ibo)
 do ij = 1, count_sea_ice_output
   if (bitmap_sea_ice_output2(ij,1)) then
     if (nsoil_output == nsoil_input) then
       do n = 1, nsoil_output
         output%soil_temp(ijsav_sea_ice_output(ij),n) = &
                output_data_sea_ice2(ij,n)
       enddo
     elseif (nsoil_output > nsoil_input) then
       output%soil_temp(ijsav_sea_ice_output(ij),1) =  &
                output_data_sea_ice2(ij,1)
       do n = 2, nsoil_output
         nsoil = min(n,nsoil_input)
         output%soil_temp(ijsav_sea_ice_output(ij),n) =  &
                output_data_sea_ice2(ij,nsoil)
       enddo
     else  ! logic hardwired for 4->2 layers with thicknesses of
           ! 0-.1,.1-.4,.4-1.0,1.0-2.0 and 0-.1,1.0-2.0 meters. 
       output%soil_temp(ijsav_sea_ice_output(ij),1) =  &
                                output_data_sea_ice2(ij,1)
       output%soil_temp(ijsav_sea_ice_output(ij),2) = &
                               (0.3*output_data_sea_ice2(ij,2) +  &
                                0.6*output_data_sea_ice2(ij,3) +  &
                                1.0*output_data_sea_ice2(ij,4))/1.9
     endif
   else  ! use a default value
     output%soil_temp(ijsav_sea_ice_output(ij),:) = frz_ice
   endif
 enddo
 deallocate (bitmap_sea_ice_output2, bitmap_sea_ice_input2,  &
             output_data_sea_ice2, input_dat)

 end if ! no ice

!-----------------------------------------------------------------------
! for snow liq equiv, interpolate snow at land and sea ice points
! separately to prevent large gfs-imposed depths over land from 
! influencing the snow depth at ice.
!-----------------------------------------------------------------------
 print*,"- INTERPOLATE SNOW LIQUID EQUIV FROM INPUT GRID."
 bitmap_land_output=.false.
 output_data_land=0.0
 kgds_output_tmp=kgds_output
 kgds_output_tmp(1) = kgdso1
 no=count_land_output
 allocate(ibo(1))
 call ipolates(int_opt_snow, ipopt_snow, kgds_input, kgds_output_tmp,   &
              (imdl_input*jmdl_input), count_land_output,               &
               1, 1, bitmap_land_input, input%snow_liq_equiv,  &
               no, lats_land_output, lons_land_output, ibo, bitmap_land_output,     &
               output_data_land, iret)
 if (iret /= 0) then
   print*,'- ERROR IN IPOLATES ',iret
   return
 endif
 deallocate(ibo)
 output%snow_liq_equiv= 0.0 ! non-land
! the budget interpolation can spread very shallow amounts of snow over
! somewhat large areas.  eliminate these.  make sure these zeroed
! amounts agree with the snow depth calculated later.
 do ij = 1, count_land_output
   if (bitmap_land_output(ij)) then
     if (int_opt == 3 .and. output_data_land(ij) < 0.2) then
       output%snow_liq_equiv(ijsav_land_output(ij))=0.0
     else
       output%snow_liq_equiv(ijsav_land_output(ij))=output_data_land(ij)
     end if
   else
     if(abs(lats_land_output(ij)) > 55.0) then
       output%snow_liq_equiv(ijsav_land_output(ij))= 2.5  ! search failed, use default (mm)
     end if
    endif
 enddo
! now do snow over sea ice.
 if (count_sea_ice_output > 0) then

 bitmap_sea_ice_output=.false.
 output_data_sea_ice=0.0
 kgds_output_tmp=kgds_output
 kgds_output_tmp(1) = kgdso1
 no=count_sea_ice_output
 allocate(ibo(1))
 call ipolates(int_opt_snow, ipopt_snow, kgds_input, kgds_output_tmp,   &
              (imdl_input*jmdl_input), count_sea_ice_output,               &
               1, 1, bitmap_sea_ice_input, input%snow_liq_equiv,  &
               no, lats_sea_ice_output, lons_sea_ice_output, ibo,  &
               bitmap_sea_ice_output, output_data_sea_ice, iret)
 if (iret /= 0) then
   print*,'- ERROR IN IPOLATES ',iret
   return
 endif
 deallocate(ibo)
 do ij = 1, count_sea_ice_output
   if (bitmap_sea_ice_output(ij)) then
     output%snow_liq_equiv(ijsav_sea_ice_output(ij))=output_data_sea_ice(ij)
   endif
 enddo
 endif  ! no ice

!-----------------------------------------------------------------------
! set state variables when initializing a land ice run.
!-----------------------------------------------------------------------
 if (landice_opt == 1 .or. landice_opt == 2) then
   print*,"- INITIALIZE STATE FIELDS AT POINTS WITH PERMANENT LAND ICE"
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0 .and. output%veg_type(ij) == veg_type_ice) then
       output%canopy_mc(ij)      = 0.0
       output%snow_liq_equiv(ij) = max(output%snow_liq_equiv(ij),100.) ! in mm
       output%soilm_tot(ij,:)    = 1.0
    endif
   enddo
 end if

!-----------------------------------------------------------------------
! physical snow depth.  not used for pre noah lsm runs.
!
! if input data was pre noah lsm, the snow depth array on the input grid
! will not be allocated. in this case, set depth to
! 10 times the snow liquid water equivalent.
!
! when initializing land ice case, ensure depth is at least 1 meter.
!-----------------------------------------------------------------------

 if (allocated(output%snow_depth)) then
   print*,"- PROCESS SNOW DEPTH"
   output%snow_depth = 0.0
   if (.not. allocated (input%snow_depth)) then
     print*,"- INITIALIZE SNOW DEPTH FROM LIQ EQUIV."
     do ij = 1, ijmdl_output
       output%snow_depth(ij) = output%snow_liq_equiv(ij)*10.0
     enddo
   else
     print*,"- INTERPOLATE SNOW DEPTH FROM INPUT GRID - LAND."
     bitmap_land_output=.false.
     output_data_land=0.0
     kgds_output_tmp=kgds_output
     kgds_output_tmp(1) = kgdso1
     allocate(ibo(1))
     no=count_land_output
     call ipolates(int_opt_snow, ipopt_snow, kgds_input, kgds_output_tmp,   &
                  (imdl_input*jmdl_input), count_land_output,               &
                   1, 1, bitmap_land_input, input%snow_depth,  &
                   no, lats_land_output, lons_land_output, ibo, bitmap_land_output,     &
                   output_data_land, iret)
     if (iret /= 0) then
       print*,'- ERROR IN IPOLATES ',iret
       return
     endif
     deallocate(ibo)
! note: very shallow amounts of liquid equivalent are zeroed out
! when the budget interpolation is used.  make sure depth is consistent.
     do ij = 1, count_land_output
       if (bitmap_land_output(ij)) then
         if (output%snow_liq_equiv(ijsav_land_output(ij))==0.0) then
           output%snow_depth(ijsav_land_output(ij))=0.0
         else
           output%snow_depth(ijsav_land_output(ij))=output_data_land(ij)
         end if
       else  ! default value.
         output%snow_depth(ijsav_land_output(ij)) = &
                           output%snow_liq_equiv(ijsav_land_output(ij))*10.0
       endif
     enddo
     if (landice_opt == 1 .or. landice_opt == 2) then
       do ij = 1, ijmdl_output
         if (output%lsmask(ij) > 0.0 .and.  &
             output%veg_type(ij) == veg_type_ice) then
           output%snow_depth(ij) = max(output%snow_depth(ij),1000.) ! in mm
         end if
       enddo
     endif
! now do snow over sea ice.
     if (count_sea_ice_output > 0) then
       print*,"- INTERPOLATE SNOW DEPTH FROM INPUT GRID - NON LAND."
       bitmap_sea_ice_output=.false.
       output_data_sea_ice=0.0
       kgds_output_tmp=kgds_output
       kgds_output_tmp(1) = kgdso1
       no=count_sea_ice_output
       allocate(ibo(1))
       call ipolates(int_opt_snow, ipopt_snow, kgds_input, kgds_output_tmp,   &
                    (imdl_input*jmdl_input), count_sea_ice_output,               &
                     1, 1, bitmap_sea_ice_input, input%snow_depth,  &
                     no, lats_sea_ice_output, lons_sea_ice_output, ibo,  &
                     bitmap_sea_ice_output, output_data_sea_ice, iret)
       if (iret /= 0) then
         print*,'- ERROR IN IPOLATES ',iret
         return
       endif
       deallocate(ibo)
       do ij = 1, count_sea_ice_output
         if (bitmap_sea_ice_output(ij)) then  ! ipolates found data
! ensure interpolated liq equiv is consistent with depth
           if (output%snow_liq_equiv(ijsav_sea_ice_output(ij)) == 0.0) then
             output%snow_depth(ijsav_sea_ice_output(ij)) = 0.0
           else
             output%snow_depth(ijsav_sea_ice_output(ij))=output_data_sea_ice(ij)
           endif
         endif
       enddo
     endif   ! no ice
   endif
  endif

!-----------------------------------------------------------------------
! adjust soil temperatures for new orography
!-----------------------------------------------------------------------

 call adjust_soilt_for_orog(output%soil_temp, orog_sav, output%orog, &
                            output%lsmask, ijmdl_output, nsoil_output)

 deallocate (orog_sav)

!-----------------------------------------------------------------------
! if input grid had landice, and output grid has landice, then
! need to ensure terrain adjustment did not raise sub-surface 
! temperature above freezing.  if initializing landice,
! use the substrate temp, which has already been qc'd for t>freezing.
!-----------------------------------------------------------------------

 if (landice_opt == 2) then
   print*,"- ENSURE COLUMN TEMPERATURES BELOW FREEZING AT LAND ICE"
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0 .and.  &
         output%veg_type(ij) == veg_type_ice) then
       output%skin_temp(ij)   = min(output%skin_temp(ij), frz_h20)
       output%soil_temp(ij,:) = min(output%soil_temp(ij,:), frz_h20)
     endif
   enddo
 endif

 if (landice_opt == 1) then
   print*,"- INITIALIZE COLUMN TEMPERATURES AT POINTS WITH LAND ICE"
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0 .and.   &
         output%veg_type(ij) == veg_type_ice) then
       output%skin_temp(ij)   = output%substrate_temp(ij)
       output%soil_temp(ij,:) = output%substrate_temp(ij)
    endif
   enddo
 end if

!-----------------------------------------------------------------------
! soil type is discreet field, always use nearest neighbor.
!-----------------------------------------------------------------------
 print*,"- INTERPOLATE SOIL TYPE"
 do ij = 1, ijmdl_output
   if (output%lsmask(ij) == 0.0) then  ! non-land points 
     output%soil_type(ij) = 0
   else
     if ( (nn_iindx_wrt_input_grid(ij) /= flag_value)   .and.  &
          (nn_jindx_wrt_input_grid(ij) /= flag_value) ) then
       ii = nn_iindx_wrt_input_grid(ij)
       jj = nn_jindx_wrt_input_grid(ij)
       output%soil_type(ij) = input%soil_type(ii,jj)
     else  ! no nearest neighbor that is land.  use default.
       if (landice_opt == 2 .and. output%veg_type(ij) == veg_type_ice) then
         output%soil_type(ij) = soil_type_ice
       else
         output%soil_type(ij) = 2
       end if
     endif
   endif
 enddo
!-----------------------------------------------------------------------
! the soil moisture rescaling algorithms depend on any changes to
! soil type.  so, save the soil type interpolated from the input grid.
!-----------------------------------------------------------------------
 rescale_soil_moist = .false.

!-----------------------------------------------------------------------
! set soil type flag value at landice when initializing landice runs. 
!-----------------------------------------------------------------------
 if (landice_opt == 1) then
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0 .and.  &
         output%veg_type(ij) == veg_type_ice) then
       output%soil_type(ij) = soil_type_ice
     endif
   enddo
 end if

 allocate(soil_type_sav(ijmdl_output))
 soil_type_sav    = output%soil_type  ! interpolated from input grid.

!-----------------------------------------------------------------------
! replace interpolated soil type with externally generated soil type
! on the output grid (if this data was read in).
!-----------------------------------------------------------------------
 if (allocated(soil_type_output_ext)) then
   print*,'- REPLACE SOIL TYPE WITH EXTERNAL DATA.'
   rescale_soil_moist = .true.
   output%soil_type = 0
   where (output%lsmask > 0.0) output%soil_type = soil_type_output_ext
   deallocate (soil_type_output_ext)
   if (landice_opt == 1 .or. landice_opt == 2) then
     do ij = 1, ijmdl_output
       if (output%lsmask(ij) > 0.0 .and.  &
           output%veg_type(ij) == veg_type_ice) then
           output%soil_type(ij) = soil_type_ice
       endif
     enddo
   end if
 end if

!-----------------------------------------------------------------------
! this option is used when the input grid has land ice, but the
! user does not want land ice on the output grid.  need to set
! a soil moisture at these points.  use the reference value for the
! soil type as a default start value. 
!-----------------------------------------------------------------------
 if (landice_opt == 4) then 
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0 .and.    &
         output%soilm_tot(ij,1) > 0.99) then  ! flag for landice. at these
                                              ! points all layers are set to 1.0
                                              ! so just check the top layer.
       soil_type_sav(ij) = output%soil_type(ij) ! turn off soilm rescaling
       output%soilm_tot(ij,:) = smcref_output(output%soil_type(ij))
     endif
   enddo
 endif

!-----------------------------------------------------------------------
! greenness.  valid only over land.
!-----------------------------------------------------------------------
 if (allocated(greenfrc_output_ext)) then
   print*,'- USE EXTERNAL DATA TO SET GREENNESS.'
   output%greenfrc = 0.
   where(output%lsmask > 0.) output%greenfrc = greenfrc_output_ext
   deallocate (greenfrc_output_ext)
 else ! greenness interpolated from input grid.
   print*,"- INTERPOLATE GREENNESS FROM INPUT GRID."
   bitmap_land_output=.false.
   output_data_land=0.0
   kgds_output_tmp=kgds_output
   kgds_output_tmp(1) = kgdso1
   allocate(ibo(1))
   no=count_land_output
   call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
                (imdl_input*jmdl_input), count_land_output,               &
                 1, 1, bitmap_land_input, input%greenfrc,  &
                 no, lats_land_output, lons_land_output, ibo,  &
                 bitmap_land_output, output_data_land, iret)
   if (iret /= 0) then
     print*,'- ERROR IN IPOLATES ',iret
     return
   endif
   deallocate(ibo)
   output%greenfrc= 0.0 ! non-land
   do ij = 1, count_land_output
     if (bitmap_land_output(ij)) then
       output%greenfrc(ijsav_land_output(ij))=output_data_land(ij)
     else
       output%greenfrc(ijsav_land_output(ij))=0.4  ! search failed, use default
     endif
   enddo
 end if
!-----------------------------------------------------------------------
! once greenness is set, zero output any canopy moisture content
! at points with zero vegetation.  recall, a greenness of 1% actually
! means bare ground.
!-----------------------------------------------------------------------
 do ij = 1, ijmdl_output
   if (output%greenfrc(ij) < 0.011) then
     output%canopy_mc(ij) = 0.0
   endif
 enddo
!-----------------------------------------------------------------------
! greenness is zero at landice points.
!-----------------------------------------------------------------------
 if (landice_opt ==1 .or. landice_opt == 2) then
   print*,"- SET GREENNESS AT LANDICE POINTS."
   do ij = 1, ijmdl_output
     if (output%lsmask(ij) > 0.0 .and.  &
         output%veg_type(ij) == veg_type_ice) then
       output%greenfrc(ij) = 0.0
     endif
   enddo
 end if

!-----------------------------------------------------------------------
! min/max greenness.  only processed when using the noah lsm.
!-----------------------------------------------------------------------

 if (allocated(output%greenfrc_min) .and. allocated(output%greenfrc_max)) then
   if (allocated(greenfrc_min_output_ext) .and. &
       allocated(greenfrc_max_output_ext)) then
     print*,'- USE EXTERNAL DATA FOR MAX/MIN GREEENNESS.'
     output%greenfrc_min = 0.
     where(output%lsmask > 0.) output%greenfrc_min = greenfrc_min_output_ext
     deallocate (greenfrc_min_output_ext)
     output%greenfrc_max = 0.
     where(output%lsmask > 0.) output%greenfrc_max = greenfrc_max_output_ext
     deallocate (greenfrc_max_output_ext)
   elseif (.not. allocated(input%greenfrc_max) .and.  &
           .not. allocated(input%greenfrc_min) ) then
     print*,"- ABORT. INPUT FILE DOES NOT HAVE MAX/MIN GREENNESS."
     iret = 99
     return
   else
     print*,"- INTERPOLATE MAX/MIN GREENNESS FROM INPUT GRID."
     allocate(bitmap_land_output2(count_land_output,2))
     bitmap_land_output2=.false.
     allocate(bitmap_land_input2(imdl_input,jmdl_input,2))
     bitmap_land_input2(:,:,1)=bitmap_land_input
     bitmap_land_input2(:,:,2)=bitmap_land_input
     allocate(output_data_land2(count_land_output,2))
     output_data_land2=0.0
     allocate(input_dat(imdl_input,jmdl_input,2))
     input_dat(:,:,1)=input%greenfrc_min
     input_dat(:,:,2)=input%greenfrc_max
     kgds_output_tmp=kgds_output
     kgds_output_tmp(1) = kgdso1
     allocate(ibo(2))
     no=count_land_output
     call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
                  (imdl_input*jmdl_input), count_land_output,               &
                   2, (/1,1/), bitmap_land_input2, input_dat,  &
                   no, lats_land_output, lons_land_output, ibo,  &
                   bitmap_land_output2, output_data_land2, iret)
     if (iret /= 0) then
       print*,'- ERROR IN IPOLATES ',iret
       return
     endif
     deallocate(ibo)
     output%greenfrc_min= 0.0 ! non-land
     output%greenfrc_max= 0.0 ! non-land
     do ij = 1, count_land_output
       if (bitmap_land_output2(ij,1)) then
         output%greenfrc_min(ijsav_land_output(ij))=output_data_land2(ij,1)
       else
         output%greenfrc_min(ijsav_land_output(ij))=0.2  ! search failed, use default
       endif
     enddo
     do ij = 1, count_land_output
       if (bitmap_land_output2(ij,2)) then
         output%greenfrc_max(ijsav_land_output(ij))=output_data_land2(ij,2)
       else
         output%greenfrc_max(ijsav_land_output(ij))=0.5  ! search failed, use default
       endif
     enddo
     deallocate(bitmap_land_input2, bitmap_land_output2, input_dat, output_data_land2)
   end if
!-----------------------------------------------------------------------
! set greenness to zero at land ice points.
!-----------------------------------------------------------------------
   if (landice_opt ==1 .or. landice_opt == 2) then
     print*,"- SET MAX/MIN GREENNESS TO ZERO AT LAND ICE."
     do ij = 1, ijmdl_output
       if (output%lsmask(ij) > 0.0 .and.  &
           output%veg_type(ij) == veg_type_ice) then
         output%greenfrc_min(ij) = 0.0
         output%greenfrc_max(ij) = 0.0
       endif
     enddo
   end if
 end if

!-----------------------------------------------------------------------
! slope type not used for pre-noah lsm runs.  treat as discreet field.
!-----------------------------------------------------------------------
 if (allocated (output%slope_type)) then
   if (allocated(slope_type_output_ext)) then
     print*,'- USE EXTERNAL DATA FOR SLOPE TYPE.'
     output%slope_type = 0
     where (output%lsmask > 0.0) output%slope_type = slope_type_output_ext
     deallocate (slope_type_output_ext)
   elseif (.not. allocated (input%slope_type)) then
     print*,"- ABORT. INPUT FILE DOES NOT HAVE SLOPE TYPE."
     iret = 99
     return
   else  ! interpolate from input grid
     print*,'- INTERPOLATE SLOPE TYPE FROM INPUT GRID.'
     do ij = 1, ijmdl_output
       if (output%lsmask(ij) == 0.0) then  ! non-land points 
           output%slope_type(ij) = 0
       else
         if ( (nn_iindx_wrt_input_grid(ij) /= flag_value)   .and.  &
              (nn_jindx_wrt_input_grid(ij) /= flag_value) ) then
           ii = nn_iindx_wrt_input_grid(ij)
           jj = nn_jindx_wrt_input_grid(ij)
           output%slope_type(ij) = input%slope_type(ii,jj)
         else  ! no nearest neighbor that is land.  use default.
           output%slope_type(ij) = 2
         endif
       endif
     enddo
   end if
   if (landice_opt == 1 .or. landice_opt == 2) then
     print*,"- INITIALIZE SLOPE TYPE AT LANDICE POINTS."
     do ij = 1, ijmdl_output
       if (output%lsmask(ij) > 0.0 .and.  &
           output%veg_type(ij) == veg_type_ice) then
         output%slope_type(ij) = 9
       endif
     enddo
   end if
 end if

!-----------------------------------------------------------------------
! snow free albedo. note: at non-land points, the gfs calculates
! albedo internally.  for surface files prior to 200501, the 
! albedo was set to a flag value of 0.01, otherwise, it is set to
! 0.06. 
!-----------------------------------------------------------------------

 SNOWFREE_ALB : if (allocated(output%alvsf)) then ! using 4 component albedo

 if (allocated(alvsf_output_ext) .and.  &
     allocated(alnsf_output_ext) .and.  &
     allocated(alnwf_output_ext) .and.  &
     allocated(alvwf_output_ext) .and.  &
     allocated(facsf_output_ext) .and.  &
     allocated(facwf_output_ext)) then
   if (allocated(output%sea_ice_fract) .and. &
       allocated(output%sea_ice_depth) ) then  ! surface file is ivs 200501
     output%alvsf = 0.06   
     output%alnsf = 0.06  
     output%alnwf = 0.06  
     output%alvwf = 0.06
   else    ! surface file prior to ivs 200501
     output%alvsf = 0.01   
     output%alnsf = 0.01 
     output%alnwf = 0.01 
     output%alvwf = 0.01
   endif
   print*,'- SET ALVSF WITH EXTERNAL DATA OVER LAND'
   where (output%lsmask > 0.0) output%alvsf = alvsf_output_ext
   deallocate (alvsf_output_ext)
   print*,'- SET ALNSF WITH EXTERNAL DATA OVER LAND'
   where (output%lsmask > 0.0) output%alnsf = alnsf_output_ext
   deallocate (alnsf_output_ext)
   print*,'- SET ALNWF WITH EXTERNAL DATA OVER LAND'
   where (output%lsmask > 0.0) output%alnwf = alnwf_output_ext
   deallocate (alnwf_output_ext)
   print*,'- SET ANVWF WITH EXTERNAL DATA OVER LAND'
   where (output%lsmask > 0.0) output%alvwf = alvwf_output_ext
   deallocate (alvwf_output_ext)
   print*,'- SET FACSF WITH EXTERNAL DATA OVER LAND'
   output%facsf = 0.0  ! non-land
   where (output%lsmask > 0.0) output%facsf = facsf_output_ext
   deallocate (facsf_output_ext)
   print*,'- SET FACWF WITH EXTERNAL DATA OVER LAND'
   output%facwf = 0.0
   where (output%lsmask > 0.0) output%facwf = facwf_output_ext
   deallocate (facwf_output_ext)
 else ! interp from input grid.
   print*,"- INTERP SNOW-FREE ALBEDO FROM INPUT GRID"
     allocate(bitmap_land_output2(count_land_output,6))
     bitmap_land_output2=.false.
     allocate(bitmap_land_input2(imdl_input,jmdl_input,6))
     bitmap_land_input2(:,:,1)=bitmap_land_input
     bitmap_land_input2(:,:,2)=bitmap_land_input
     bitmap_land_input2(:,:,3)=bitmap_land_input
     bitmap_land_input2(:,:,4)=bitmap_land_input
     bitmap_land_input2(:,:,5)=bitmap_land_input
     bitmap_land_input2(:,:,6)=bitmap_land_input
     allocate(output_data_land2(count_land_output,6))
     output_data_land2=0.0
     allocate(input_dat(imdl_input,jmdl_input,6))
     input_dat(:,:,1)=input%alnsf
     input_dat(:,:,2)=input%alnwf
     input_dat(:,:,3)=input%alvsf
     input_dat(:,:,4)=input%alvwf
     input_dat(:,:,5)=input%facsf
     input_dat(:,:,6)=input%facwf
     kgds_output_tmp=kgds_output
     kgds_output_tmp(1) = kgdso1
     allocate(ibo(6))
     no=count_land_output
     call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
                  (imdl_input*jmdl_input), count_land_output,          &
                   6, (/1,1,1,1,1,1/), bitmap_land_input2, input_dat,  &
                   no, lats_land_output, lons_land_output, ibo,  &
                   bitmap_land_output2, output_data_land2, iret)
     if (iret /= 0) then
       print*,'- ERROR IN IPOLATES ',iret
       return
     endif
     deallocate(ibo)
     output%facsf=0.0  ! non-land
     output%facwf=0.0  ! non-land
     if (allocated(output%sea_ice_fract) .and. & ! non-land points
         allocated(output%sea_ice_depth) ) then  ! surface file is ivs 200501
       output%alnsf=0.06
       output%alnwf=0.06
       output%alvsf=0.06
       output%alvwf=0.06
     else
       output%alnsf=0.01
       output%alnwf=0.01
       output%alvsf=0.01
       output%alvwf=0.01
     endif         
     do ij = 1, count_land_output
       if (bitmap_land_output2(ij,1)) then
         output%alnsf(ijsav_land_output(ij))=output_data_land2(ij,1)
       else
         if (ialb == 1) then  ! modis data
           output%alnsf(ijsav_land_output(ij))=0.30  ! search failed, use default
         else  ! 1 degree gfs data
           output%alnsf(ijsav_land_output(ij))=0.25  ! search failed, use default
         endif
       endif
     enddo
     do ij = 1, count_land_output
       if (bitmap_land_output2(ij,2)) then
         output%alnwf(ijsav_land_output(ij))=output_data_land2(ij,2)
       else
         if (ialb == 1) then  ! modis
           output%alnwf(ijsav_land_output(ij))=0.29  ! search failed, use default
         else ! 1 degree gfs data
           output%alnwf(ijsav_land_output(ij))=0.2  ! search failed, use default
         endif
       endif
     enddo
     do ij = 1, count_land_output
       if (bitmap_land_output2(ij,3)) then
         output%alvsf(ijsav_land_output(ij))=output_data_land2(ij,3)
       else
         if (ialb == 1) then  ! modis
           output%alvsf(ijsav_land_output(ij))=0.15  ! search failed, use default
         else ! 1 degree gfs data
           output%alvsf(ijsav_land_output(ij))=0.15  ! search failed, use default
         endif
       endif
     enddo
     do ij = 1, count_land_output
       if (bitmap_land_output2(ij,4)) then
         output%alvwf(ijsav_land_output(ij))=output_data_land2(ij,4)
       else
         if (ialb == 1) then ! modis
           output%alvwf(ijsav_land_output(ij))=0.14  ! search failed, use default
         else ! 1 degree gfs data
           output%alvwf(ijsav_land_output(ij))=0.1  ! search failed, use default
         end if
       endif
     enddo
     do ij = 1, count_land_output
       if (bitmap_land_output2(ij,5)) then
         output%facsf(ijsav_land_output(ij))=output_data_land2(ij,5)
       else
         output%facsf(ijsav_land_output(ij))=0.5  ! search failed, use default
       endif
     enddo
     do ij = 1, count_land_output
       if (bitmap_land_output2(ij,6)) then
         output%facwf(ijsav_land_output(ij))=output_data_land2(ij,6)
       else
         output%facwf(ijsav_land_output(ij))=0.5  ! search failed, use default
       endif
     enddo
     deallocate(bitmap_land_input2, bitmap_land_output2, input_dat, output_data_land2)
 end if
!-----------------------------------------------------------------------
! single snowfree albedo used by wrf/nmm
!-----------------------------------------------------------------------
 elseif (allocated(output%snow_free_albedo)) then
   if (allocated(snow_free_albedo_output_ext)) then
      print*,"- SET SNOW FREE ALBEDO WITH EXTERNAL DATA"
      output%snow_free_albedo = 0.06  ! open water
      where (output%lsmask > 0.0) output%snow_free_albedo =  &
                                  snow_free_albedo_output_ext  ! land
      where (output%sea_ice_flag == 1) output%snow_free_albedo = 0.65  ! sea ice
   else
     print*,"- INTERPOLATE SNOW-FREE (BASE) ALBEDO FROM INPUT GRID."
     bitmap_land_output=.false.
     output_data_land=0.0
     kgds_output_tmp=kgds_output
     kgds_output_tmp(1) = kgdso1
     no=count_land_output
     allocate(ibo(1))
     call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
                  (imdl_input*jmdl_input), count_land_output,     &
                   1, 1, bitmap_land_input, input%snow_free_albedo,  &
                   no, lats_land_output, lons_land_output, ibo,   &
                   bitmap_land_output, output_data_land, iret)
     if (iret /= 0) then
       print*,'- ERROR IN IPOLATES ',iret
       return
     endif
     deallocate(ibo)
     output%snow_free_albedo = 0.06  ! open water
     do ij = 1, count_land_output
       if (bitmap_land_output(ij)) then
         output%snow_free_albedo(ijsav_land_output(ij))=output_data_land(ij)
       else
         output%snow_free_albedo(ijsav_land_output(ij))=0.2  ! search failed, use default
       endif
     enddo
     where (output%sea_ice_flag == 1) output%snow_free_albedo = 0.65  ! sea ice
   endif
 endif SNOWFREE_ALB

!-----------------------------------------------------------------------
! maximum snow albedo for noah runs.
!-----------------------------------------------------------------------
 if (allocated (output%mxsnow_alb)) then
   if (allocated(mxsnow_alb_output_ext)) then
     print*,'- SET MAX SNOW ALBEDO WITH EXTERNAL DATA.'
     output%mxsnow_alb = 0.0
     where (output%lsmask>0.0) output%mxsnow_alb = mxsnow_alb_output_ext
     deallocate (mxsnow_alb_output_ext)
   elseif (.not. allocated(input%mxsnow_alb)) then
     print*,"- ABORT. INPUT FILE DOES NOT HAVE MAX SNOW ALBEDO."
     iret = 99
     return
   else  ! interpolate from input grid
     print*,"- INTERPOLATE MAX SNOW ALBEDO FROM INPUT GRID."
     bitmap_land_output=.false.
     output_data_land=0.0
     kgds_output_tmp=kgds_output
     kgds_output_tmp(1) = kgdso1
     no=count_land_output
     allocate(ibo(1))
     call ipolates(int_opt, ipopt, kgds_input, kgds_output_tmp,   &
                  (imdl_input*jmdl_input), count_land_output,               &
                   1, 1, bitmap_land_input, input%mxsnow_alb,  &
                   no, lats_land_output, lons_land_output, ibo, bitmap_land_output,     &
                   output_data_land, iret)
     if (iret /= 0) then
       print*,'- ERROR IN IPOLATES ',iret
       return
     endif
     deallocate(ibo)
     output%mxsnow_alb= 0.0 ! non-land
     do ij = 1, count_land_output
       if (bitmap_land_output(ij)) then
         output%mxsnow_alb(ijsav_land_output(ij))=output_data_land(ij)
       else
         output%mxsnow_alb(ijsav_land_output(ij))=0.7  ! search failed, use default
       endif
     enddo
   end if
 end if

!-----------------------------------------------------------------------
! if the soil types have changed, need to rescale
! the soil moisture.
!-----------------------------------------------------------------------
 if (rescale_soil_moist) then
   print*,'- RESCALE SOIL MOISTURE FOR NEW SOIL TYPE.'
   allocate (soilm_sav(ijmdl_output,nsoil_output))
   soilm_sav = output%soilm_tot
   allocate (lsmask_output_temp(ijmdl_output))
   lsmask_output_temp = output%lsmask
!   don't bother doing this at landice points as soil moisture is
!   not used.
   if (landice_opt == 1 .or. landice_opt == 2) then
     do ij = 1, ijmdl_output
       if (output%lsmask(ij) > 0.0 .and.  &
           output%veg_type(ij) == veg_type_ice) then
         lsmask_output_temp(ij) = 0.0
       endif
     enddo
   endif
   call rescale_soilm(soilm_sav, output%soilm_tot,              &
                      soil_type_sav, output%soil_type,          &
                      smcdry_input, smcdry_output,              &
                      smcwilt_input, smcwilt_output,            &
                      smcref_input, smcref_output,              &
                      smcmax_input, smcmax_output,              &
                      lsmask_output_temp, output%greenfrc,      &
                      ijmdl_output,                &
                      nsoil_output, max_soil_types)
   deallocate (soilm_sav)
   deallocate (lsmask_output_temp)
 end if
 deallocate(soil_type_sav)

!-----------------------------------------------------------------------
! now calculate the liquid portion of the total soil moisture.
!-----------------------------------------------------------------------
 if (allocated (output%soilm_liq)) then
   print*,'- CALCULATE LIQUID PORTION OF TOTAL SOIL MOISTURE.'
   output%soilm_liq = 1.0  ! flag value for non-land points
   allocate (lsmask_output_temp(ijmdl_output))
   lsmask_output_temp = output%lsmask
! don't bother doing this at landice points as soil moisture is
! not used.
   if (landice_opt == 1 .or. landice_opt == 2) then
     do ij = 1, ijmdl_output
       if (output%lsmask(ij) > 0.0 .and.  &
           output%veg_type(ij) == veg_type_ice) then
         lsmask_output_temp(ij) = 0.0
       endif
     enddo
   endif
   call calc_liq_soilm(output%soil_type, output%soilm_tot, &
                       output%soil_temp, output%soilm_liq, &
                       lsmask_output_temp, beta_output,         &
                       psis_output, smcmax_output,         &
                       max_soil_types,                     &
                       ijmdl_output, nsoil_output)
   deallocate (lsmask_output_temp)
 end if

!-----------------------------------------------------------------------
! for single albedo, need to calc snow effects.
!-----------------------------------------------------------------------
 if (allocated (output%albedo)) then
  print*,"- CALCULATE SNOW EFFECT ON ALBEDO"
  output%albedo=0.06  ! open water value
  allocate (snow_m(ijmdl_output))
  snow_m = output%snow_liq_equiv * 0.001
  call calc_albedo(output%lsmask, output%veg_type, ijmdl_output, &
                   salp_output, snup_output, max_veg_types, &
                   output%snow_free_albedo, output%mxsnow_alb, &
                   snow_m, output%albedo)
  where(output%sea_ice_flag == 1) output%albedo = 0.65 ! as in eta after may 3, 05
  deallocate (snow_m)
 end if

 where(output%sea_ice_flag == 1) output%lsmask = 2.0

 deallocate (bitmap_land_output, bitmap_land_input)
 deallocate (bitmap_nonland_output, bitmap_nonland_input)
 deallocate (bitmap_sea_ice_input)
 deallocate (ijsav_land_output, ijsav_nonland_output)
 deallocate (lats_land_output, lons_land_output)
 deallocate (lats_nonland_output, lons_nonland_output)
 deallocate (nn_iindx_wrt_input_grid, nn_jindx_wrt_input_grid)
 deallocate (output_data_land, output_data_nonland)
 if (allocated (output_data_sea_ice)) deallocate(output_data_sea_ice)
 if (allocated (bitmap_sea_ice_output)) deallocate(bitmap_sea_ice_output)
 if (allocated (lats_sea_ice_output)) deallocate(lats_sea_ice_output)
 if (allocated (lons_sea_ice_output)) deallocate(lons_sea_ice_output)
 if (allocated (ijsav_sea_ice_output)) deallocate(ijsav_sea_ice_output)

 return

 end subroutine interp
!-----------------------------------------------------------------------
 subroutine setup(kgds_input, input, imdl_input, jmdl_input, & 
                  imo, iret)
!$$$ subprogram documentation block
!
! subprogram: setup    perform various setup tasks
!   prgmmr: gayno           org: w/np2     date: 2005-10-19
!
! abstract: read configuration namelist, calculate model resolution,
!           calculate some soil parameters.
!
! program history log:
!  2005-10-19  gayno   - initial version
!
! usage: 'call setup' with the following arguments
!   input arguments (input/output denotes input/output grid):
!     kgds_input             grib grid description section of input grid
!     input                  surface data on input grid
!     i/jmdl_input           i/j dimensions of input grid
!   output arguments
!     iret                   error status, non-zero means an error occurred
!   other outputs:
!     mdl_res_input/output   resolution (in degrees) of model
!                            input/output grid
!     smcref_input/output    onset of soil moisture stress, input/output
!                            grid values
!     smcdry_input/output    air dry soil moisture limit, input/output
!                            grid values
!     smcwilt_input/output   plant wilting point, input/output grid
!                            values
!     soil_type_ice          soil type for land ice points
!     veg_type_ice           veg type for land ice (output grid)
!     veg_type_ice_input     veg type for land ice (input grid)
!
! namelists:
!   options:
!     climo_fields_opt       option for determining climo fields on
!                            output grid.  1 ONLY!!
!                            1-interpolate all from input grid
!                            2-interpolate veg, soil, slope type
!                              from input grid.  others from
!                              cycle program.
!                            3-all from cycle program.
!     landice_opt            1-no landice input grid -> landice output grid
!                            2-landice input grid -> landice output grid
!                            3-no landice input grid -> no landice output grid
!                            4-landice input grid -> no landice output grid
!                            5-landice on output grid regardless of
!                              whether input grid has landice or not.
!   soil_parameters (input/output grid values):
!     soil_src_input/output  source of soil type database (ex: zobler)
!     smclow_input/output    soil moisture scalar multiplier
!     smchigh_input/output   soil moisture scalar multiplier
!     smcmax_input/output    maximum soil moisture content
!     beta_input/output      soil 'b' parameter
!     psis_input/output      saturated soil potential
!     satdk_input/output     saturated soil hydraulic conductivity
!
!   veg_parameters:
!     veg_src_input/output   source of veg type database (ex: usgs)
!     salp_output            plant factor in albedo calculation
!     snup_output            plant factor in albedo calculation
!
! input files:
!   unit 81    configuration namelists
!
! subprograms called:
!   calc_soil_parms  - calculate soil parameters
!
! attributes:
!   langauge: fortran 90
!
!$$$
 use soil_utils,  only            : calc_soil_parms

 implicit none

 character*10                    :: soil_src_input, soil_src_output, &
                                    veg_src_input, veg_src_output

 integer, intent(in)             :: imdl_input, jmdl_input
 integer                         :: i,j, istat
 integer, intent(in)             :: kgds_input(200), imo
 integer, intent(inout)          :: iret

 type(sfc2d)                     :: input

 namelist /soil_parameters/        soil_src_input,  &
                                   smclow_input,    &
                                   smchigh_input,   &
                                   smcmax_input,    &
                                   beta_input,      &
                                   psis_input,      &
                                   satdk_input,     &
                                   soil_src_output, &
                                   smclow_output,   &
                                   smchigh_output,  &
                                   smcmax_output,   &
                                   beta_output,     &
                                   psis_output,     &
                                   satdk_output

 namelist /veg_parameters/         veg_src_input,   &
                                   veg_src_output,  &
                                   salp_output,     &
                                   snup_output

 namelist /options/                climo_fields_opt, &
                                   landice_opt

 iret=0
 print*,"- READ CONFIGURATION NAMELIST."
 open(81, iostat=istat, err=900)
 read(81, nml=soil_parameters, iostat=istat, err=910)
 read(81, nml=veg_parameters, iostat=istat, err=910)
 read(81, nml=options, iostat=istat, err=910)
 close(81)

 mdl_res_input = 360.0 / float(kgds_input(2))
 print*,"- RESOLUTION OF INPUT GRID IN DEGREES IS: ", mdl_res_input

 mdl_res_output = 360.0 / (float(imo) * 4.0) 
 print*,"- RESOLUTION OF OUTPUT GRID IN DEGREES IS: ", mdl_res_output

!-----------------------------------------------------------------------
! the flag values of soil and veg type for landice depend on
! what raw databases were used.  
!-----------------------------------------------------------------------

 select case (trim(soil_src_output))
   case("zobler")
     soil_type_ice=9
   case("statsgo")
     soil_type_ice=16
   case default
     print*,'- BAD CHOICE OF OUTPUT GRID SOIL SOURCE ',trim(soil_src_output)
     iret=1
     return
 end select

 select case (trim(veg_src_output))
   case("usgs")
     veg_type_ice=24
   case("sib")
     veg_type_ice=13
   case("igbp")
     veg_type_ice=15
   case default
     print*,'- BAD CHOICE OF OUTPUT GRID VEG SOURCE ',trim(veg_src_output)
     iret=2
     return
 end select

 select case (trim(veg_src_input))
   case("usgs")
     veg_type_ice_input=24
   case("sib")
     veg_type_ice_input=13
   case("igbp")
     veg_type_ice_input=15
   case default
     print*,'- BAD CHOICE OF INPUT GRID VEG SOURCE ',trim(veg_src_input)
     iret=3
     return
 end select

!-----------------------------------------------------------------------
! parameters for soil type on input/output grids.
!-----------------------------------------------------------------------

 print*,'- CALCULATE SOIL PARAMETERS'
 call calc_soil_parms(smclow_input, smchigh_input,              &
                      smcmax_input, beta_input,                 &
                      satdk_input, psis_input, max_soil_types,  &
                      smcref_input, smcwilt_input, smcdry_input)

 call calc_soil_parms(smclow_output, smchigh_output,                 &
                      smcmax_output, beta_output,                    &
                      satdk_output, psis_output, max_soil_types,     &
                      smcref_output, smcwilt_output, smcdry_output)

!-----------------------------------------------------------------------
! certain landice options are only valid if the input grid has
! or does not have landice.  check for the soil moisture flag value
! of 1.0 at land ice points.
!-----------------------------------------------------------------------

 if (landice_opt == 1 .or. landice_opt == 3) then 
   do j = 1, jmdl_input
   do i = 1, imdl_input
     if (input%lsmask(i,j) > 0.0 .and. input%soilm_tot(i,j,1) > 0.99) then
       print*,"- LANDICE OPTION OF ", landice_opt, " IS NOT VALID WHEN"
       print*,"- INPUT DATA HAS LANDICE."
       iret=4
       return
     end if
   enddo
   enddo
 elseif (landice_opt == 2 .or. landice_opt == 4) then
   do j = 1, jmdl_input
   do i = 1, imdl_input
     if (input%lsmask(i,j) > 0.0 .and. input%soilm_tot(i,j,1) > 0.99) then
       return
     end if
   enddo
   enddo
   print*,"- LANDICE OPTION OF ", landice_opt, " IS NOT VALID WHEN"
   print*,"- INPUT DATA DOES NOT HAVE LANDICE."
   iret=5
   return
 elseif (landice_opt == 5) then
   print*,"- WILL FORCE LANDICE INITIALIZATION."
   landice_opt = 1    ! for rest of module, 1 and 5 are equivalent.
 end if

 return

900 print*,"- ERROR OPENING CONFIG NAMELIST. ISTAT IS ", istat
    iret=6
    return

910 print*,"- ERROR READING CONFIG NAMELIST. ISTAT IS ", istat
    iret=7
    return

 end subroutine setup
!-----------------------------------------------------------------------
 subroutine get_ext_climo_global(ijmdl_output, lsmask_output, orog_output, &
                                 orog_uf, use_ufo, nst_anl, output,        &
                                 hour, month, day,                         &
                                 year, fhour, ialb, isot, ivegsrc,         &
                                 iindx_output, jindx_output, tile_num)
!$$$ subprogram documentation block
!
! subprogram: get_ext_climo_global    get climo fields on global grid
!   prgmmr: gayno           org: w/np2     date: 2005-10-19
!
! abstract: get climo fields, such as soil type and albedo on the
!           output grid by calling the surface cycle code.  according
!           to the option selected by the user, the climo fields on the
!           output grid will come from either surface cycle or be 
!           interpolated from the input grid.  if a field is to be
!           interpolated from the input grid, the corresponding array
!           from surface cycle will be deallocated.
!           note!! surface cycle is only called to get the climo
!           fields on the grid.  NOT to update the SST, snow or sea ice.
!
! program history log:
!  2005-10-19  gayno   - initial version
!
! usage: call subroutine get_ext_climo_global  &
!                               (ijmdl_output, &
!                                lsmask_output, orog_output, orog_uf, &
!                                use_ufo, nst_anl, output, &
!                                hour, month, day, &
!                                year, fhour, ialb, isot, ivegsrc)
!   input arguments:
!     ijmdl_output           number of grid points, output grid
!     lsmask_output          landmask (0-nonland, 1-land) output grid
!     orog_output            orography of output grid
!     orog_uf                unfiltered orography of output grid
!     use_ufo                when 'true' use unfiltered orography
!     nst_anl                when 'true' use nsst model
!     hour/month/day/year    YYYYMMDDHH of cycle
!     fhour                  forecast hour with respect to cycle time
!     ialb                   when '1', use bosu albedo.  when '0',
!                            use old albedo
!     isot                   when '1', use new statsgo soil type.
!                            when '0', use zobler soil type
!     ivegsrc                when '1', use new igbp vegetation type.
!                            when '2', use sib vegetation type
!   outputs:
!     substrate_temp_output_ext   substrate temperature from sfccycle
!     soil_type_output_ext        soil type from sfccycle
!     veg_type_output_ext         vegetation type from sfccycle
!     slope_type_output_ext       slope type from sfccycle
!     mxsnow_alb_output_ext       max snow albedo from sfccycle
!     z0_output_ext               roughness from sfccycle
!     greenfrc_output_ext         greenness fraction from sfccycle
!     greenfrc_min_output_ext     min greenness fraction from sfccycle
!     greenfrc_max_output_ext     max greenness fraction from sfccycle     
!     facsf_output_ext            fraction, strong cosz dependence from sfccycle
!     facwf_output_ext            fraction, weak cosz dependence from sfccycle
!
!   - old radiation treatment (ialb = 0)
!     alnsf_output_ext            near ir albedo, strong cosz depend. from sfccycle
!     alnwf_output_ext            near ir albedo, weak cosz depend. from sfccycle 
!     alvsf_output_ext            vis albedo, strong cosz depend. from sfccycle
!     alvwf_output_ext            vis albedo, weak cosz depend. from sfccycle
!
!   - new radiation treatment (ialb = 1)
!     alnsf_output_ext            near ir black sky albedo
!     alnwf_output_ext            near ir white sky albedo 
!     alvsf_output_ext            visible black sky albedo
!     alvwf_output_ext            visible white sky albedo
!
! subprograms called:
!   sfccycle  - calculate soil parameters
!
! attributes:
!   langauge: fortran 90
!
!$$$
 use machine, only                 : kind_io8

 implicit none

 integer, parameter                    :: sz_nml = 1

 character(len=4)                      :: input_nml_file(sz_nml)
 character(len=5)                      :: tile_num_ch

 integer, intent(in)                   :: hour, month, day, year, ialb
 integer, intent(in)                   :: ijmdl_output, isot, ivegsrc
 integer, intent(in)                   :: iindx_output(ijmdl_output)
 integer, intent(in)                   :: jindx_output(ijmdl_output)
 integer, intent(in)                   :: tile_num
 integer                               :: lsoil
 integer, parameter                    :: lugb = 51
 integer                               :: nlunit

 real, intent(in)                      :: fhour
 real, intent(in)                      :: lsmask_output(ijmdl_output)
 real, intent(in)                      :: orog_output(ijmdl_output)
 real, intent(in)                      :: orog_uf     (ijmdl_output)
 logical,intent(in)                    :: use_ufo, nst_anl

 real (kind=kind_io8), allocatable :: sig1t(:), &
      slmask(:), orog(:),  sihfcs(:), sicfcs(:), sitfcs(:),&
      swdfcs(:), slcfcs(:,:), vmnfcs(:), vmxfcs(:),   &
      slpfcs(:), absfcs(:),  TSFFCS(:), SNOFCS(:), ZORFCS(:), &
      ALBFCS(:,:), TG3FCS(:), CNPFCS(:), SMCFCS(:,:), STCFCS(:,:),  &
      slifcs(:), AISFCS(:), vegfcs(:), vetfcs(:), &
      sotfcs(:), ALFFCS(:,:), CVFCS(:), CVBFCS(:), CVTFCS(:), &
      lats(:), lons(:)

 real(kind=kind_io8)                   :: deltsfc, fh

 type(sfc1d), intent(in)   :: output

 input_nml_file = "NULL"

 write(tile_num_ch, "(a4,i1)") "tile", tile_num

 lsoil = 4
 deltsfc = 0.0
 fh = fhour

 allocate (sig1t(ijmdl_output),  slmask(ijmdl_output), &
           orog(ijmdl_output),   sihfcs(ijmdl_output), &
           sicfcs(ijmdl_output), swdfcs(ijmdl_output), &
           sitfcs(ijmdl_output),                       &
           slcfcs(ijmdl_output,lsoil), vmnfcs(ijmdl_output), &
           vmxfcs(ijmdl_output), slpfcs(ijmdl_output),  &
           absfcs(ijmdl_output), TSFFCS(ijmdl_output),  &
           SNOFCS(ijmdl_output), zorfcs(ijmdl_output),  &
           ALBFCS(ijmdl_output,4), tg3fcs(ijmdl_output),  &
           CNPFCS(ijmdl_output), SMCFCS(ijmdl_output,lsoil),  &
           STCFCS(ijmdl_output,lsoil), slifcs(ijmdl_output), &
           AISFCS(ijmdl_output),          &
           vegfcs(ijmdl_output), vetfcs(ijmdl_output),       &
           sotfcs(ijmdl_output), ALFFCS(ijmdl_output,2),     &
           CVFCS(ijmdl_output), CVBFCS(ijmdl_output),        &
           CVTFCS(ijmdl_output), lats(ijmdl_output), & 
           lons(ijmdl_output))

 slmask = lsmask_output

 orog = orog_output

!orog = orog_output - orog_uf

 nlunit = 35

 sig1t  = 0.0; sihfcs = 0.0; sicfcs = 0.0; swdfcs = 0.0
 slcfcs = 0.0; vmnfcs = 0.0; vmxfcs = 0.0; slpfcs = 0.0
 absfcs = 0.0; tsffcs = 0.0; snofcs = 0.0; zorfcs = 0.0
 albfcs = 0.0; tg3fcs = 0.0; cnpfcs = 0.0; smcfcs = 0.0
 stcfcs = 0.0; slifcs = 0.0; aisfcs = 0.0; 
 vegfcs = 0.0; vetfcs = 0.0; sotfcs = 0.0; alffcs = 0.0
 cvfcs  = 0.0; cvbfcs = 0.0; cvtfcs = 0.0; sitfcs = 0.0
 lats=output%lats; lons=output%lons
 call SFCCYCLE(LUGB,ijmdl_output,LSOIL,SIG1T,DELTSFC,            &
               year,month,day,hour,FH,                           &
               lats, lons, SLMASK,OROG,orog_uf,use_ufo,nst_anl,  &
               SIHFCS,SICFCS,SITFCS,                             &
               SWDFCS,SLCFCS,                                    &
               VMNFCS, VMXFCS, SLPFCS, ABSFCS,                   &
               TSFFCS,SNOFCS,ZORFCS,ALBFCS,TG3FCS,               &
               CNPFCS,SMCFCS,STCFCS,slifcs,AISFCS,               &
               VEGFCS, VETFCS, SOTFCS, ALFFCS,                   &
               CVFCS,CVBFCS,CVTFCS,0,NLUNIT,                     &
               SZ_NML, INPUT_NML_FILE, IALB, ISOT, IVEGSRC,      &
               TILE_NUM_CH, IINDX_OUTPUT, JINDX_OUTPUT)
!-----------------------------------------------------------------------
! if an array is deallocated, the rest of code knows to interpolate
! that field from the input grid.
!
! always use tbot from surface cycle as it is imitmately tied to the
! output terrain.
!-----------------------------------------------------------------------

 allocate (substrate_temp_output_ext(ijmdl_output))
 substrate_temp_output_ext = tg3fcs

 if (climo_fields_opt == 3) then
   allocate (soil_type_output_ext(ijmdl_output))
   soil_type_output_ext = nint(sotfcs)
   allocate (veg_type_output_ext(ijmdl_output))
   veg_type_output_ext = nint(vetfcs)
   allocate (slope_type_output_ext(ijmdl_output))
   slope_type_output_ext = nint(slpfcs)
 end if

 if (climo_fields_opt == 2 .or. climo_fields_opt == 3) then
   allocate (mxsnow_alb_output_ext(ijmdl_output))
   mxsnow_alb_output_ext = absfcs
   allocate (z0_output_ext(ijmdl_output))
   z0_output_ext = zorfcs
   allocate (greenfrc_output_ext(ijmdl_output))                         
   greenfrc_output_ext = vegfcs
   allocate (greenfrc_min_output_ext(ijmdl_output))                         
   greenfrc_min_output_ext = vmnfcs
   allocate (greenfrc_max_output_ext(ijmdl_output))                         
   greenfrc_max_output_ext = vmxfcs
   allocate (alnsf_output_ext(ijmdl_output))
   alnsf_output_ext = albfcs(:,3)
   allocate (alnwf_output_ext(ijmdl_output))
   alnwf_output_ext = albfcs(:,4)
   allocate (alvsf_output_ext(ijmdl_output))
   alvsf_output_ext = albfcs(:,1)
   allocate (alvwf_output_ext(ijmdl_output))
   alvwf_output_ext = albfcs(:,2)
   allocate (facsf_output_ext(ijmdl_output))
   facsf_output_ext = alffcs(:,1)
   allocate (facwf_output_ext(ijmdl_output))
   facwf_output_ext = alffcs(:,2)
 end if

 deallocate (sig1t, slmask, orog, sihfcs, sicfcs, swdfcs, &
             slcfcs, vmnfcs, vmxfcs, slpfcs, absfcs, sitfcs,   &
             TSFFCS, SNOFCS, zorfcs, ALBFCS, tg3fcs,      &
             CNPFCS, SMCFCS, STCFCS, slifcs, AISFCS, &
             vegfcs, vetfcs, sotfcs, ALFFCS, CVFCS, CVBFCS, CVTFCS, &
             lats, lons)

 return
 end subroutine get_ext_climo_global
!-----------------------------------------------------------------------
 subroutine get_ext_climo_nmm(imdl_output, jmdl_output, curr_hour, &
                              curr_mon, curr_day, curr_year, iret)
!$$$ subprogram documentation block
!
! subprogram: get_ext_climo_nmm    get climo fields on nmm grid
!   prgmmr: gayno           org: w/np2     date: 2006-04-14
!
! abstract: get climo fields, such as soil type and albedo on the
!           output grid by reading them from grib files.  
!           user selects what fields to read in by setting the
!           namelist entries for the path/name of the grib file.
!           if a field is NOT read in from the grib file, it
!           will be interpolated from the input grid. 
!
! program history log:
!  2006-04-14  gayno   - initial version
!
! usage: 'call get_ext_climo_global' with the following arguments
!   input arguments:
!     i/jmdl_output           number of grid points, output grid
!                             in the i/j direction
!     curr_hour/mon/day/year  current time (used to interpolate
!                                           monthly datasets)
!   outputs:
!     soil_type_output_ext        soil type 
!     veg_type_output_ext         vegetation type 
!     slope_type_output_ext       slope type 
!     mxsnow_alb_output_ext       max snow albedo 
!     z0_output_ext               roughness 
!     greenfrc_output_ext         greenness fraction 
!     snow_free_output_ext        snowfree albedo
!
! namelists:
!   optional_output_fields:  (path/name of climo grib files)
!     snow_free_albedo_output_file    
!     greenfrc_output_file          
!     mxsnow_alb_output_file        
!     slope_type_output_file     
!     soil_type_output_file          
!     veg_type_output_file          
!     z0_output_file               
!
! input files:
!   unit 81    configuration namelists
!
! subprograms called:
!   read_grib_data  - read single grib field
!   degrib_climo    - degrib multiple time period grib file
!   time_interp     - time interpolate a field to current date
!   inventory       - inventory multiple time period grib file
!
! attributes:
!   langauge: fortran 90
!
!$$$

 use read_write_utils, only     : read_grib_data,  &
                                  date, &
                                  degrib_climo, &
                                  time_interp, &
                                  inventory

 implicit none

 namelist /optional_output_fields/ snow_free_albedo_output_file,  &
                                   greenfrc_output_file, &
                                   mxsnow_alb_output_file, &
                                   slope_type_output_file,  &
                                   soil_type_output_file, &
                                   veg_type_output_file,  &
                                   z0_output_file

 character*150                  :: soil_type_output_file
 character*150                  :: veg_type_output_file
 character*150                  :: slope_type_output_file
 character*150                  :: mxsnow_alb_output_file
 character*150                  :: greenfrc_output_file
 character*150                  :: snow_free_albedo_output_file
 character*150                  :: z0_output_file

 integer, intent(in)            :: imdl_output, jmdl_output, &
                                   curr_hour, curr_day, &
                                   curr_year, curr_mon

 integer                        :: iret, param_num, tot_num_recs, &
                                   curr_minute, ijmdl_output, istat

 real, allocatable              :: dummy(:)
 real, allocatable              :: data_climo(:,:,:)

 type (date), allocatable       :: dates(:)

 iret = 0

 print*,"- READ CONFIGURATION NAMELIST."

 open(81, iostat=istat, err=900)
 read(81, nml=optional_output_fields, iostat=istat, err=910)
 close(81)

 ijmdl_output = imdl_output * jmdl_output

!-----------------------------------------------------------------------
! if user chooses, read soil type, veg type, slope type and
! max snow albedo.
!-----------------------------------------------------------------------

 if (len_trim(soil_type_output_file) > 0) then
   print*,"- DEGRIB SOIL TYPE ON OUTPUT GRID"
   allocate (dummy(ijmdl_output))
   allocate (soil_type_output_ext(ijmdl_output))
   call read_grib_data(soil_type_output_file, 224, dummy, &
                       ijmdl_output, iret)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID SOIL TYPE'
     return
   end if
   soil_type_output_ext = nint(dummy)
   deallocate (dummy)
 end if

 if (len_trim(veg_type_output_file) > 0) then
   print*,"- DEGRIB VEG TYPE ON OUTPUT GRID"
   allocate (dummy(ijmdl_output))
   allocate (veg_type_output_ext(ijmdl_output))
   call read_grib_data(veg_type_output_file, 225, dummy, &
                       ijmdl_output, iret)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID VEG TYPE'
     return
   end if
   veg_type_output_ext = nint(dummy)
   deallocate (dummy)
 end if

 if (len_trim(slope_type_output_file) > 0) then
   print*,"- DEGRIB SLOPE TYPE ON OUTPUT GRID"
   allocate (dummy(ijmdl_output))
   allocate (slope_type_output_ext(ijmdl_output))
   call read_grib_data(slope_type_output_file, 222, dummy, &
                       ijmdl_output, iret)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID SLOPE TYPE'
     return
   end if
   slope_type_output_ext = nint(dummy)
   deallocate (dummy)
 end if

 if (len_trim(mxsnow_alb_output_file) > 0) then
   print*,"- DEGRIB MAX SNOW ALBEDO ON OUTPUT GRID"
   allocate (mxsnow_alb_output_ext(ijmdl_output))
   call read_grib_data(mxsnow_alb_output_file, 159,     &
                       mxsnow_alb_output_ext, ijmdl_output, iret)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID MAX SNOW ALBEDO'
     return
   end if
! grib standard is %, but model expects decimal.
   mxsnow_alb_output_ext = mxsnow_alb_output_ext * 0.01
 end if

!-----------------------------------------------------------------------
! greenness fraction.
!-----------------------------------------------------------------------

 if (len_trim(greenfrc_output_file) > 0) then
   print*,"- DEGRIB GREENNESS ON OUTPUT GRID"
   param_num = 87
   call inventory(greenfrc_output_file, param_num, tot_num_recs, iret, 0)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID GREENNESS FRACTION.'
     return
   end if
   allocate (dates(tot_num_recs))
   allocate (data_climo(imdl_output,jmdl_output,tot_num_recs))
   call degrib_climo(data_climo, dates, ijmdl_output,           &
                     param_num, greenfrc_output_file,           &
                     tot_num_recs, iret, 0)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID GREENNESS FRACTION.'
     return
   end if
   allocate (greenfrc_output_ext(ijmdl_output))
   greenfrc_output_ext = 0.0
!  need to adjust this for fcst hour????
   curr_minute = 0
   call time_interp(data_climo, dates, tot_num_recs,                &
                    ijmdl_output, curr_year, curr_mon, curr_day,    &
                    curr_hour, curr_minute, greenfrc_output_ext)
!  grib standard is %, but model expects decimal.
   greenfrc_output_ext = greenfrc_output_ext * 0.01
   deallocate (dates)
   deallocate (data_climo)
 end if

!-----------------------------------------------------------------------
! albedo (snow free)
!-----------------------------------------------------------------------

 if (len_trim(snow_free_albedo_output_file) > 0) then
   print*,"- DEGRIB SNOW FREE ALBEDO ON OUTPUT GRID"
   param_num = 170
   call inventory(snow_free_albedo_output_file, param_num,  &
                  tot_num_recs, iret, 0)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID SNOW-FREE ALBEDO'
     return
   end if
   allocate (dates(tot_num_recs))
   allocate (data_climo(imdl_output,jmdl_output,tot_num_recs))
   call degrib_climo(data_climo, dates, ijmdl_output,           &
                     param_num, snow_free_albedo_output_file,    &
                     tot_num_recs, iret, 0)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID SNOW-FREE ALBEDO'
     return
   end if
   allocate (snow_free_albedo_output_ext(ijmdl_output))
   snow_free_albedo_output_ext = 0.0
!  need to adjust this for fcst hour????
   curr_minute = 0
   call time_interp(data_climo, dates, tot_num_recs,                &
                    ijmdl_output, curr_year, curr_mon, curr_day,    &
                    curr_hour, curr_minute, snow_free_albedo_output_ext)
!  grib standard is %, but model uses decimal.
   snow_free_albedo_output_ext = snow_free_albedo_output_ext * 0.01
   deallocate (dates)
   deallocate (data_climo)
 end if

!-----------------------------------------------------------------------
! roughness length.
!-----------------------------------------------------------------------

 if (len_trim(z0_output_file) > 0) then
   print*,"- DEGRIB ROUGHNESS ON OUTPUT GRID"
   param_num = 83
   call inventory(z0_output_file, param_num, tot_num_recs, iret, 0)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID ROUGHNESS LENGTH'
     return
   end if
   allocate (dates(tot_num_recs))
   allocate (data_climo(imdl_output,jmdl_output,tot_num_recs))
   call degrib_climo(data_climo, dates, ijmdl_output,      &
                     param_num, z0_output_file, tot_num_recs, iret, 0)
   if (iret /= 0) then
     print*,'- ERROR DEGRIBBING OUTPUT GRID ROUGHNESS LENGTH'
     return
   end if
   allocate (z0_output_ext(ijmdl_output))
   z0_output_ext = -1.0
!nmm does not use a time varying field.  no need to time interp.
   if (tot_num_recs > 1) then
     curr_minute = 0
     call time_interp(data_climo, dates, tot_num_recs, &
                      ijmdl_output, curr_year, curr_mon, curr_day, &
                      curr_hour, curr_minute, z0_output_ext)
   else
     z0_output_ext = reshape(data_climo(:,:,1),(/ijmdl_output/))
   end if
!-----------------------------------------------------------------------
! interp code expects z0 in cm, raw data in meters.
!-----------------------------------------------------------------------
   z0_output_ext = z0_output_ext * 100.0
   deallocate (data_climo)
   deallocate (dates)
 end if

 return

900 print*,"- ERROR OPENING CONFIG NAMELIST. ISTAT IS ", istat
    iret = 11
    return

910 print*,"- ERROR READING CONFIG NAMELIST. ISTAT IS ", istat
    iret = 12
    return

 end subroutine get_ext_climo_nmm

!-----------------------------------------------------------------------
 subroutine surface_chgres_ax2d(dum)
!$$$ subprogram documentation block
!
! subprogram: surface_chgres_ax2d    free up memory
!   prgmmr: gayno           org: w/np2     date: 2005-10-19
!
! abstract: deallocate a sfc2d data structure
!
! program history log:
!  2005-10-19  gayno   - initial version
!
! usage: call surface_chgres_ax2d(dum)
!   input arguments:
!     dum            data structure containing several surface fields
!
! attributes:
!   langauge: fortran 90
!
!$$$
 type (sfc2d), intent(inout)   :: dum

 if (allocated (dum%alnsf))           deallocate (dum%alnsf)
 if (allocated (dum%alnwf))           deallocate (dum%alnwf)
 if (allocated (dum%alvsf))           deallocate (dum%alvsf)
 if (allocated (dum%alvwf))           deallocate (dum%alvwf)
 if (allocated (dum%canopy_mc))       deallocate (dum%canopy_mc)
 if (allocated (dum%facsf))           deallocate (dum%facsf)
 if (allocated (dum%facwf))           deallocate (dum%facwf)
 if (allocated (dum%sea_ice_fract))   deallocate (dum%sea_ice_fract)
 if (allocated (dum%greenfrc))        deallocate (dum%greenfrc)
 if (allocated (dum%greenfrc_max))    deallocate (dum%greenfrc_max)
 if (allocated (dum%greenfrc_min))    deallocate (dum%greenfrc_min)
 if (allocated (dum%sea_ice_depth))   deallocate (dum%sea_ice_depth)
 if (allocated (dum%lsmask))          deallocate (dum%lsmask)
 if (allocated (dum%mxsnow_alb))      deallocate (dum%mxsnow_alb)
 if (allocated (dum%orog))            deallocate (dum%orog)
 if (allocated (dum%sea_ice_temp))    deallocate (dum%sea_ice_temp)
 if (allocated (dum%skin_temp))       deallocate (dum%skin_temp)
 if (allocated (dum%snow_depth))      deallocate (dum%snow_depth)
 if (allocated (dum%snow_liq_equiv))  deallocate (dum%snow_liq_equiv)
 if (allocated (dum%snow_free_albedo))deallocate (dum%snow_free_albedo)
 if (allocated (dum%soilm_liq))       deallocate (dum%soilm_liq)
 if (allocated (dum%soilm_tot))       deallocate (dum%soilm_tot)
 if (allocated (dum%soil_temp))       deallocate (dum%soil_temp)
 if (allocated (dum%z0))              deallocate (dum%z0)
 if (allocated (dum%sea_ice_flag))    deallocate (dum%sea_ice_flag)
 if (allocated (dum%slope_type))      deallocate (dum%slope_type)
 if (allocated (dum%soil_type))       deallocate (dum%soil_type)
 if (allocated (dum%veg_type))        deallocate (dum%veg_type)

 end subroutine surface_chgres_ax2d
!-----------------------------------------------------------------------
 subroutine surface_chgres_ax1d(dum)
!$$$ subprogram documentation block
!
! subprogram: surface_chgres_ax1d    free up memory
!   prgmmr: gayno           org: w/np2     date: 2005-10-19
!
! abstract: deallocate a sfc1d data structure
!
! program history log:
!  2005-10-19  gayno   - initial version
!
! usage: call surface_chgres_ax1d(dum)
!   input arguments:
!     dum            data structure containing several surface fields
!
! attributes:
!   langauge: fortran 90
!
!$$$
 type (sfc1d), intent(inout)   :: dum

 if (allocated (dum%albedo))          deallocate (dum%albedo)
 if (allocated (dum%alnsf))           deallocate (dum%alnsf)
 if (allocated (dum%alnwf))           deallocate (dum%alnwf)
 if (allocated (dum%alvsf))           deallocate (dum%alvsf)
 if (allocated (dum%alvwf))           deallocate (dum%alvwf)
 if (allocated (dum%canopy_mc))       deallocate (dum%canopy_mc)
 if (allocated (dum%facsf))           deallocate (dum%facsf)
 if (allocated (dum%facwf))           deallocate (dum%facwf)
 if (allocated (dum%sea_ice_fract))   deallocate (dum%sea_ice_fract)
 if (allocated (dum%greenfrc))        deallocate (dum%greenfrc)
 if (allocated (dum%greenfrc_max))    deallocate (dum%greenfrc_max)
 if (allocated (dum%greenfrc_min))    deallocate (dum%greenfrc_min)
 if (allocated (dum%sea_ice_depth))   deallocate (dum%sea_ice_depth)
 if (allocated (dum%lats))            deallocate (dum%lats)
 if (allocated (dum%lons))            deallocate (dum%lons)
 if (allocated (dum%lsmask))          deallocate (dum%lsmask)
 if (allocated (dum%mxsnow_alb))      deallocate (dum%mxsnow_alb)
 if (allocated (dum%orog))            deallocate (dum%orog)
 if (allocated (dum%sea_ice_temp))    deallocate (dum%sea_ice_temp)
 if (allocated (dum%skin_temp))       deallocate (dum%skin_temp)
 if (allocated (dum%snow_depth))      deallocate (dum%snow_depth)
 if (allocated (dum%snow_free_albedo))deallocate (dum%snow_free_albedo) 
 if (allocated (dum%snow_liq_equiv))  deallocate (dum%snow_liq_equiv)
 if (allocated (dum%soilm_liq))       deallocate (dum%soilm_liq)
 if (allocated (dum%soilm_tot))       deallocate (dum%soilm_tot)
 if (allocated (dum%soil_temp))       deallocate (dum%soil_temp)
 if (allocated (dum%substrate_temp))  deallocate (dum%substrate_temp) 
 if (allocated (dum%z0))              deallocate (dum%z0)
 if (allocated (dum%sea_ice_flag))    deallocate (dum%sea_ice_flag)
 if (allocated (dum%slope_type))      deallocate (dum%slope_type)
 if (allocated (dum%soil_type))       deallocate (dum%soil_type)
 if (allocated (dum%veg_type))        deallocate (dum%veg_type)

 end subroutine surface_chgres_ax1d

 end module surface_chgres
