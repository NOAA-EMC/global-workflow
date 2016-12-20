module crtm_interface
!$$$ module documentation block
!           .      .    .                                       
! module:   crtm_interface module for setuprad. Calculates profile and calls crtm
!  prgmmr:
!
! abstract: crtm_interface module for setuprad. Initializes CRTM, Calculates profile and 
!         calls CRTM and destroys initialization
!
! program history log:
!   2010-08-17  Derber - initial creation from intrppx
!   2011-05-06  merkova/todling - add use of q-clear calculation for AIRS
!   2011-04-08  li     - (1) Add nst_gsi, itref,idtw, idtc, itz_tr to apply NSST. 
!                      - (2) Use Tz instead of Ts as water surface temperature when nst_gsi > 1
!                      - (3) add tzbgr as one of the out dummy variable
!                      - (4) Include tz_tr in ts calculation over water
!                      - (5) Change minmum temperature of water surface from 270.0 to 271.0
!   2011-07-04  todling - fixes to run either single or double precision
!   2011-09-20  hclin  - modified for modis_aod
!                        (1) The jacobian of wrfchem/gocart p25 species (not calculated in CRTM)
!                            is derived from dust1 and dust2
!                        (2) skip loading geometry and surface structures for modis_aod
!                        (3) separate jacobian calculation for modis_aod
!   2012-01-17  sienkiewicz - pass date to crtm for SSU cell pressure
!   2013-02-25  zhu - add cold_start option for regional applications
!   2013-10-19  todling - metguess now holds background
!   2013-11-16  todling - merge in latest DTC AOD development;
!                         revisit handling of green-house-gases
!   2014-01-01  li     - change the protection of data_s(itz_tr)
!   2014-02-26  zhu - add non zero jacobian
!   2014-04-27  eliu    - add call crtm_forward to calculate clear-sky Tb under all-sky condition    
!   2016-06-03  Collard - Added changes to allow for historical naming conventions
!
! subroutines included:
!   sub init_crtm
!   sub call_crtm
!   sub destroy_crtm
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds,only: r_kind,i_kind,r_single
use crtm_module, only: crtm_atmosphere_type,crtm_surface_type,crtm_geometry_type, &
    crtm_options_type,crtm_rtsolution_type,crtm_destroy,crtm_options_destroy, &
    crtm_options_create,crtm_options_associated,success,crtm_atmosphere_create, &
    crtm_surface_create,crtm_k_matrix,crtm_forward, &   
    ssu_input_setvalue, &
    crtm_channelinfo_type, &
    crtm_surface_destroy, crtm_surface_associated, crtm_surface_zero, &
    crtm_atmosphere_associated, &
    crtm_atmosphere_destroy,crtm_atmosphere_zero, &
    crtm_rtsolution_type, crtm_rtsolution_create, &
    crtm_rtsolution_destroy, crtm_rtsolution_associated, &
    crtm_irlandcoeff_classification, &
    crtm_kind => fp, &
    crtm_microwave_sensor => microwave_sensor
use gridmod, only: lat2,lon2,nsig,msig,nvege_type,regional,wrf_mass_regional,netcdf,use_gfs_ozone
use mpeu_util, only: die
use crtm_aod_module, only: crtm_aod_k

implicit none

private
public init_crtm            ! Subroutine initializes crtm for specified instrument
public call_crtm            ! Subroutine creates profile for crtm, calls crtm, then adjoint of create
public destroy_crtm         ! Subroutine destroys initialization for crtm
public sensorindex
public surface
public isatid               ! = 1  index of satellite id
public itime                ! = 2  index of analysis relative obs time
public ilon                 ! = 3  index of grid relative obs location (x)
public ilat                 ! = 4  index of grid relative obs location (y)
public ilzen_ang            ! = 5  index of local (satellite) zenith angle (radians)
public ilazi_ang            ! = 6  index of local (satellite) azimuth angle (radians)
public iscan_ang            ! = 7  index of scan (look) angle (radians)
public iscan_pos            ! = 8  index of integer scan position
public iszen_ang            ! = 9  index of solar zenith angle (degrees)
public isazi_ang            ! = 10 index of solar azimuth angle (degrees)
public ifrac_sea            ! = 11 index of ocean percentage
public ifrac_lnd            ! = 12 index of land percentage
public ifrac_ice            ! = 13 index of ice percentage
public ifrac_sno            ! = 14 index of snow percentage
public its_sea              ! = 15 index of ocean temperature
public its_lnd              ! = 16 index of land temperature
public its_ice              ! = 17 index of ice temperature
public its_sno              ! = 18 index of snow temperature
public itsavg               ! = 19 index of average temperature
public ivty                 ! = 20 index of vegetation type
public ivfr                 ! = 21 index of vegetation fraction
public isty                 ! = 22 index of soil type
public istp                 ! = 23 index of soil temperature
public ism                  ! = 24 index of soil moisture
public isn                  ! = 25 index of snow depth
public izz                  ! = 26 index of surface height
public idomsfc              ! = 27 index of dominate surface type
public isfcr                ! = 28 index of surface roughness
public iff10                ! = 29 index of ten meter wind factor
public ilone                ! = 30 index of earth relative longitude (degrees)
public ilate                ! = 31 index of earth relative latitude (degrees)
public iclr_sky             ! = 7  index of clear sky amount (goes_img, seviri)
public isst_navy            ! = 7  index of navy sst retrieval (K) (avhrr_navy)
public idata_type           ! = 32 index of data type (151=day, 152=night, avhrr_navy)
public iclavr               ! = 32 index of clavr cloud flag (avhrr)
public isst_hires           ! = 33 index of interpolated hires sst
public itref                ! = 34/36 index of Tr
public idtw                 ! = 35/37 index of d(Tw)
public idtc                 ! = 36/38 index of d(Tc)
public itz_tr               ! = 37/39 index of d(Tz)/d(Tr)
 
!  Note other module variables are only used within this routine

  character(len=*), parameter :: myname='crtm_interface'
  
  ! Indices for the CRTM NPOESS EmisCoeff file
  integer(i_kind), parameter :: INVALID_LAND = 0
  integer(i_kind), parameter :: COMPACTED_SOIL = 1
  integer(i_kind), parameter :: TILLED_SOIL = 2
  integer(i_kind), parameter :: IRRIGATED_LOW_VEGETATION = 5
  integer(i_kind), parameter :: MEADOW_GRASS = 6
  integer(i_kind), parameter :: SCRUB = 7
  integer(i_kind), parameter :: BROADLEAF_FOREST = 8
  integer(i_kind), parameter :: PINE_FOREST = 9
  integer(i_kind), parameter :: TUNDRA = 10
  integer(i_kind), parameter :: GRASS_SOIL = 11
  integer(i_kind), parameter :: BROADLEAF_PINE_FOREST = 12
  integer(i_kind), parameter :: GRASS_SCRUB = 13
  integer(i_kind), parameter :: URBAN_CONCRETE = 15
  integer(i_kind), parameter :: BROADLEAF_BRUSH = 17
  integer(i_kind), parameter :: WET_SOIL = 18
  integer(i_kind), parameter :: SCRUB_SOIL = 19
  
  character(len=20),save,allocatable,dimension(:)   :: aero_names   ! aerosol names
  real(r_kind)   , save ,allocatable,dimension(:,:) :: aero         ! aerosol (guess) profiles at obs location
  real(r_kind)   , save ,allocatable,dimension(:,:) :: aero_conc    ! aerosol (guess) concentrations at obs location
  real(r_kind)   , save ,allocatable,dimension(:)   :: auxrh        ! temporary array for rh profile as seen by CRTM

  character(len=20),save,allocatable,dimension(:)   :: ghg_names    ! names of green-house gases

  character(len=20),save,allocatable,dimension(:)   :: cloud_names  ! cloud names
  integer(i_kind), save ,allocatable,dimension(:)   :: icloud       ! cloud index for those considered here 
  integer(i_kind), save ,allocatable,dimension(:)   :: jcloud       ! cloud index for those fed to CRTM
  real(r_kind)   , save ,allocatable,dimension(:,:) :: cloud        ! cloud considered here
  real(r_kind)   , save ,allocatable,dimension(:,:) :: cloudefr     ! effective radius of cloud type in CRTM
  real(r_kind)   , save ,allocatable,dimension(:,:) :: cloud_cont   ! cloud content fed into CRTM 
  real(r_kind)   , save ,allocatable,dimension(:,:) :: cloud_efr    ! effective radius of cloud type in CRTM

  real(r_kind)   , save ,allocatable,dimension(:,:,:,:)  :: gesqsat ! qsat to calc rh for aero particle size estimate

  integer(i_kind),save, allocatable,dimension(:) :: map_to_crtm_ir
  integer(i_kind),save, allocatable,dimension(:) :: map_to_crtm_mwave 
  integer(i_kind),save, allocatable,dimension(:) :: icw
  integer(i_kind),save, allocatable,dimension(:) :: iaero_jac
  integer(i_kind),save :: isatid,itime,ilon,ilat,ilzen_ang,ilazi_ang,iscan_ang
  integer(i_kind),save :: iscan_pos,iszen_ang,isazi_ang,ifrac_sea,ifrac_lnd,ifrac_ice
  integer(i_kind),save :: ifrac_sno,its_sea,its_lnd,its_ice,its_sno,itsavg
  integer(i_kind),save :: ivty,ivfr,isty,istp,ism,isn,izz,idomsfc,isfcr,iff10,ilone,ilate
  integer(i_kind),save :: iclr_sky,isst_navy,idata_type,isst_hires,iclavr
  integer(i_kind),save :: itref,idtw,idtc,itz_tr,istype
  integer(i_kind),save :: sensorindex
  integer(i_kind),save :: ico2,ico24crtm
  integer(i_kind),save :: n_aerosols_jac     ! number of aerosols in jocabian
  integer(i_kind),save :: n_aerosols         ! number of aerosols considered
  integer(i_kind),save :: n_aerosols_crtm    ! number of aerosols seen by CRTM
  integer(i_kind),save :: n_clouds_jac       ! number of clouds in jacobian
  integer(i_kind),save :: n_actual_clouds    ! number of clouds considered by this interface
  integer(i_kind),save :: n_clouds           ! number of clouds seen by CRTM
  integer(i_kind),save :: n_ghg              ! number of green-house gases
  integer(i_kind),save :: icf
  integer(i_kind),save :: itv,iqv,ioz,ius,ivs,isst
  integer(i_kind),save :: ip25, indx_p25, indx_dust1, indx_dust2
  logical        ,save :: lcf4crtm
  logical        ,save :: lcw4crtm
  logical        ,save :: lwind
  integer(i_kind), parameter :: min_n_absorbers = 2

  type(crtm_atmosphere_type),save,dimension(1)   :: atmosphere
  type(crtm_surface_type),save,dimension(1)      :: surface
  type(crtm_geometry_type),save,dimension(1)     :: geometryinfo
  type(crtm_options_type),save,dimension(1)      :: options
  type(crtm_channelinfo_type),save,dimension(1)  :: channelinfo


  type(crtm_atmosphere_type),save,allocatable,dimension(:,:):: atmosphere_k
  type(crtm_surface_type),save,allocatable,dimension(:,:):: surface_k
  type(crtm_rtsolution_type),save,allocatable,dimension(:,:):: rtsolution
  type(crtm_rtsolution_type),save,allocatable,dimension(:,:):: rtsolution0              
  type(crtm_rtsolution_type),save,allocatable,dimension(:,:):: rtsolution_k

! Mapping land surface type of GFS to CRTM
!  Notes: index 0 is water, and index 13 is ice. The two indices are not
!         used and just assigned to COMPACTED_SOIL. Also, since there
!         is currently one relevant mapping for the global we apply
!         'crtm' in the naming convention.  
  integer(i_kind), parameter, dimension(0:13) :: gfs_to_crtm=(/COMPACTED_SOIL, &
     BROADLEAF_FOREST, BROADLEAF_FOREST, BROADLEAF_PINE_FOREST, PINE_FOREST, &
     PINE_FOREST, BROADLEAF_BRUSH, SCRUB, SCRUB, SCRUB_SOIL, TUNDRA, &
     COMPACTED_SOIL, TILLED_SOIL, COMPACTED_SOIL/)
! Mapping surface classification to CRTM
  integer(i_kind), parameter :: USGS_N_TYPES = 24
  integer(i_kind), parameter :: IGBP_N_TYPES = 20
  integer(i_kind), parameter :: GFS_N_TYPES = 13
  integer(i_kind), parameter :: SOIL_N_TYPES = 16
  integer(i_kind), parameter :: GFS_SOIL_N_TYPES = 9
  integer(i_kind), parameter :: GFS_VEGETATION_N_TYPES = 13
  integer(i_kind), parameter, dimension(1:USGS_N_TYPES) :: usgs_to_npoess=(/URBAN_CONCRETE, &
     COMPACTED_SOIL, IRRIGATED_LOW_VEGETATION, GRASS_SOIL, MEADOW_GRASS, &
     MEADOW_GRASS, MEADOW_GRASS, SCRUB, GRASS_SCRUB, MEADOW_GRASS, &
     BROADLEAF_FOREST, PINE_FOREST, BROADLEAF_FOREST, PINE_FOREST, &
     BROADLEAF_PINE_FOREST, INVALID_LAND, WET_SOIL, WET_SOIL, &
     IRRIGATED_LOW_VEGETATION, TUNDRA, TUNDRA, TUNDRA, TUNDRA, &
     INVALID_LAND/)
  integer(i_kind), parameter, dimension(1:IGBP_N_TYPES) :: igbp_to_npoess=(/PINE_FOREST, &
    BROADLEAF_FOREST, PINE_FOREST, BROADLEAF_FOREST, BROADLEAF_PINE_FOREST, &
    SCRUB, SCRUB_SOIL, BROADLEAF_BRUSH, BROADLEAF_BRUSH, SCRUB, BROADLEAF_BRUSH, &
    TILLED_SOIL, URBAN_CONCRETE, TILLED_SOIL, INVALID_LAND, COMPACTED_SOIL, &
    INVALID_LAND, TUNDRA, TUNDRA, TUNDRA/)
  integer(i_kind), parameter, dimension(1:USGS_N_TYPES) :: usgs_to_usgs=(/1, &
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, &
    20, 21, 22, 23, 24/)
  integer(i_kind), parameter, dimension(1:IGBP_N_TYPES) :: igbp_to_igbp=(/1, &
    2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, &
    20/)
  integer(i_kind), parameter, dimension(1:IGBP_N_TYPES) :: igbp_to_gfs=(/4, &
    1, 5, 2, 3, 8, 9, 6, 6, 7, 8, 12, 7, 12, 13, 11, 0, 10, 10, 11/)
  integer(i_kind), parameter, dimension(1:USGS_N_TYPES) :: usgs_to_gfs=(/7, &
    12, 12, 12, 12, 12, 7, 9, 8, 6, 2, 5, 1, 4, 3, 0, 8, 8, 11, 10, 10, &
    10, 11, 13/)
 ! Mapping soil types to CRTM
 ! The CRTM soil types for microwave calculations are based on the 
 ! GFS use of the 9 category Zobler dataset. The regional soil types
 ! are based on a 16 category representation of FAO/STATSGO. 
  integer(i_kind), parameter, dimension(1:SOIL_N_TYPES) :: map_soil_to_crtm=(/1, &
    1, 4, 2, 2, 8, 7, 2, 6, 5, 2, 3, 8, 1, 6, 9/)
  
contains
subroutine init_crtm(init_pass,mype_diaghdr,mype,nchanl,isis,obstype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_crtm initializes things for use with call to crtm from setuprad
!
!   prgmmr: derber           org: np2                 date: 2010-08-17
!
! abstract: initialize things for use with call to crtm from setuprad.   
!
! program history log:
!   2010-08-17  derber  
!   2011-02-16  todling - add calculation of rh when aerosols are available
!   2011-05-03  todling - merge with Min-Jeong's MW cloudy radiance; combine w/ metguess
!   2011-05-20  mccarty - add atms wmo_sat_id hack (currently commented out)
!   2011-07-20  zhu     - modified codes for lcw4crtm
!   2012-03-12  yang    - modify to use ch4,n2o,and co
!   2012-12-03  eliu    - add logic for RH total 
!   2014-01-31  mkim    - add flexibility in the variable lcw4crtm for the case when ql and 
!                         qi are separate control variables for all-sky MW radiance DA   
!   2014-04-27  eliu    - add capability to call CRTM forward model to calculate
!                         clear-sky Tb under all-sky condition 
!   2015-09-04  J.Jung  - Added mods for CrIS full spectral resolution (FSR) and
!                         CRTM subset code for CrIS.

!
!   input argument list:
!     init_pass    - state of "setup" processing
!     mype_diaghdr - processor to produce output from crtm
!     mype         - current processor        
!     nchanl       - number of channels    
!     isis         - instrument/sensor character string 
!     obstype      - observation type
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_chemguess_mod, only: gsi_chemguess_bundle   ! for now, a common block
  use gsi_chemguess_mod, only: gsi_chemguess_get
  use gsi_metguess_mod,  only: gsi_metguess_bundle    ! for now, a common block
  use gsi_metguess_mod,  only: gsi_metguess_get
  use crtm_module, only: mass_mixing_ratio_units,co2_id,o3_id,crtm_init, &
      crtm_channelinfo_subset, crtm_channelinfo_n_channels, toa_pressure,max_n_layers, &
      volume_mixing_ratio_units,h2o_id,ch4_id,n2o_id,co_id
  use radinfo, only: crtm_coeffs_path
  use radinfo, only: radjacindxs,radjacnames,jpch_rad,nusis,nuchan
  use aeroinfo, only: aerojacindxs,aerojacnames
  use guess_grids, only: ges_tsen,ges_prsl,nfldsig
  use control_vectors, only: cvars3d
  use mpeu_util, only: getindex
  use constants, only: zero,tiny_r_kind,max_varname_length
  use obsmod, only: dval_use

  implicit none

! argument 
  logical        ,intent(in) :: init_pass
  integer(i_kind),intent(in) :: nchanl,mype_diaghdr,mype
  character(20)  ,intent(in) :: isis
  character(10)  ,intent(in) :: obstype

! local parameters
  character(len=*), parameter :: myname_=myname//'*init_crtm'

! local variables
  integer(i_kind) :: ier,ii,error_status,iderivative
  integer(i_kind) :: k, subset_start, subset_end
  logical :: ice,Load_AerosolCoeff,Load_CloudCoeff
  character(len=20),dimension(1) :: sensorlist
  integer(i_kind) :: icf4crtm,indx,iii,icloud4crtm,icount
! ...all "additional absorber" variables
  integer(i_kind) :: j
  integer(i_kind) :: ig
  integer(i_kind) :: n_absorbers


  isst=-1
  ivs=-1
  ius=-1
  ioz=-1
  iqv=-1
  itv=-1
! Get indexes of variables composing the jacobian
  indx =getindex(radjacnames,'tv')
  if(indx>0) itv=radjacindxs(indx)
  indx =getindex(radjacnames,'q' )
  if(indx>0) iqv=radjacindxs(indx)
  indx =getindex(radjacnames,'oz')
  if(indx>0) ioz=radjacindxs(indx)
  indx =getindex(radjacnames,'u')
  if(indx>0) ius=radjacindxs(indx)
  indx =getindex(radjacnames,'v')
  if(indx>0) ivs=radjacindxs(indx)
  lwind=ius>0.and.ivs>0
  indx=getindex(radjacnames,'sst')
  if(indx>0) isst=radjacindxs(indx)

  call gsi_metguess_get ( 'clouds::3d', n_clouds, ier )
  if (n_clouds>0) then
     allocate(cloud_names(max(n_clouds,1)))
     call gsi_metguess_get ('clouds::3d',cloud_names,ier)
     n_clouds_jac=0
     do ii=1,n_clouds
        indx=getindex(radjacnames,trim(cloud_names(ii)))
        if(indx>0) n_clouds_jac=n_clouds_jac+1
     end do
     allocate(icw(max(n_clouds_jac,1)))
     icw=-1
     n_clouds_jac=0
     do ii=1,n_clouds
        indx=getindex(radjacnames,trim(cloud_names(ii)))
        if(indx>0) then
           n_clouds_jac=n_clouds_jac+1
           icw(n_clouds_jac)=radjacindxs(indx)
        endif
     end do
     deallocate(cloud_names)
  end if

! Get indexes of variables composing the jacobian_aero
  n_aerosols=0
  n_aerosols_jac=0
  call gsi_chemguess_get ( 'aerosols::3d', n_aerosols, ier )
  if (n_aerosols > 0) then
     allocate(aero_names(n_aerosols))
     call gsi_chemguess_get ('aerosols::3d',aero_names,ier)
     indx_p25   = getindex(aero_names,'p25')
     indx_dust1 = getindex(aero_names,'dust1')
     indx_dust2 = getindex(aero_names,'dust2')
     if (indx_p25 > 0) then
        do ii=1,n_aerosols
           indx=getindex(aerojacnames,trim(aero_names(ii)))
           if(indx>0) n_aerosols_jac=n_aerosols_jac+1
        end do
     else
        call gsi_chemguess_get ( 'aerosols_4crtm_jac::3d', n_aerosols_jac, ier )
     endif
     if (n_aerosols_jac >0) then
        allocate(iaero_jac(n_aerosols_jac))
        iaero_jac=-1
        n_aerosols_jac=0
        do ii=1,n_aerosols
           indx=getindex(aerojacnames,trim(aero_names(ii)))
           if(indx>0) then
              n_aerosols_jac=n_aerosols_jac+1
              iaero_jac(n_aerosols_jac)=aerojacindxs(indx)
           endif
        end do
     endif
     deallocate(aero_names)
  endif

! Inquire presence of extra fields in MetGuess
 icf=-1; icf4crtm=-1
 if (size(gsi_metguess_bundle)>0) then ! check to see if bundle's allocated
!   get cloud-fraction for radiation information
    call gsi_bundlegetpointer(gsi_metguess_bundle(1),'cf',icf,ier)
    call gsi_metguess_get ( 'i4crtm::cf', icf4crtm, ier )
 endif
 lcf4crtm = obstype=='airs' .and. icf4crtm==12 .and. icf>0

! When CW is available in MetGuess, defined Cloudy Radiance for MW only
 lcw4crtm=.false.
 if(trim(obstype)=='amsua') then
!   get cloud-condensate information
    call gsi_metguess_get ( 'clouds_4crtm_jac::3d', n_clouds, ier )

    if(n_clouds>0) then
       call gsi_metguess_get ( 'clouds::3d', n_actual_clouds, ier )
       if (getindex(cvars3d,'cw')>0 .or. getindex(cvars3d,'ql')>0 .or. getindex(cvars3d,'qi')>0) lcw4crtm=.true.
                                                                                                                              
       if (mype==0) write(0,*) myname_, " n_clouds, n_actual_clouds: ", n_clouds, n_actual_clouds

       allocate(cloud_cont(msig,n_clouds))
       allocate(cloud_efr(msig,n_clouds))
       allocate(jcloud(n_clouds))
       allocate(cloud(nsig,n_clouds))
       allocate(cloudefr(nsig,n_clouds))
       allocate(icloud(n_actual_clouds))
       allocate(cloud_names(n_actual_clouds))
       cloud_cont=zero
       cloud_efr =zero
       cloud     =zero
       cloudefr  =zero

       call gsi_metguess_get ('clouds::3d',cloud_names,ier)
       call gsi_bundlegetpointer(gsi_metguess_bundle(1),cloud_names,icloud,ier)

       iii=0
       do ii=1,n_actual_clouds
          call gsi_metguess_get ( 'i4crtm::'//trim(cloud_names(ii)), icloud4crtm, ier )
          if (icloud4crtm==12) then
             iii=iii+1
             jcloud(iii)=ii
          endif
       end do
       if(iii/=n_clouds) call die(myname_,'inconsistent cloud count',1)

       Load_CloudCoeff = .true.
    else
       n_actual_clouds = 0
       n_clouds = n_actual_clouds
       Load_CloudCoeff = .false.
    endif
 else
    n_actual_clouds = 0
    n_clouds = n_actual_clouds
    Load_CloudCoeff = .false.
 endif

! Set up index for input satellite data array

 isatid    = 1  ! index of satellite id
 itime     = 2  ! index of analysis relative obs time
 ilon      = 3  ! index of grid relative obs location (x)
 ilat      = 4  ! index of grid relative obs location (y)
 ilzen_ang = 5  ! index of local (satellite) zenith angle (radians)
 ilazi_ang = 6  ! index of local (satellite) azimuth angle (radians)
 iscan_ang = 7  ! index of scan (look) angle (radians)
 iscan_pos = 8  ! index of integer scan position
 iszen_ang = 9  ! index of solar zenith angle (degrees)
 isazi_ang = 10 ! index of solar azimuth angle (degrees)
 ifrac_sea = 11 ! index of ocean percentage
 ifrac_lnd = 12 ! index of land percentage
 ifrac_ice = 13 ! index of ice percentage
 ifrac_sno = 14 ! index of snow percentage
 its_sea   = 15 ! index of ocean temperature
 its_lnd   = 16 ! index of land temperature
 its_ice   = 17 ! index of ice temperature
 its_sno   = 18 ! index of snow temperature
 itsavg    = 19 ! index of average temperature
 ivty      = 20 ! index of vegetation type
 ivfr      = 21 ! index of vegetation fraction
 isty      = 22 ! index of soil type
 istp      = 23 ! index of soil temperature
 ism       = 24 ! index of soil moisture
 isn       = 25 ! index of snow depth
 izz       = 26 ! index of surface height
 idomsfc   = 27 ! index of dominate surface type
 isfcr     = 28 ! index of surface roughness
 iff10     = 29 ! index of ten meter wind factor
 ilone     = 30 ! index of earth relative longitude (degrees)
 ilate     = 31 ! index of earth relative latitude (degrees)
 icount=ilate
 if(dval_use) icount=icount+2
 if ( obstype == 'avhrr_navy' .or. obstype == 'avhrr' ) icount=icount+2 ! when an independent SST analysis is read in
 itref     = icount+1 ! index of foundation temperature: Tr
 idtw      = icount+2 ! index of diurnal warming: d(Tw) at depth zob
 idtc      = icount+3 ! index of sub-layer cooling: d(Tc) at depth zob
 itz_tr    = icount+4 ! index of d(Tz)/d(Tr)

 if (obstype == 'goes_img') then
    iclr_sky      =  7 ! index of clear sky amount
 elseif (obstype == 'avhrr_navy') then
    isst_navy     =  7 ! index of navy sst (K) retrieval
    idata_type    = 32 ! index of data type (151=day, 152=night)
    isst_hires    = 33 ! index of interpolated hires sst (K)
 elseif (obstype == 'avhrr') then
    iclavr        = 32 ! index CLAVR cloud flag with AVHRR data
    isst_hires    = 33 ! index of interpolated hires sst (K)
 elseif (obstype == 'seviri') then
    iclr_sky      =  7 ! index of clear sky amount
 endif


! get the number of trace gases present in the chemguess bundle
 n_ghg=0
 if(size(gsi_chemguess_bundle)>0) then
    call gsi_chemguess_get('ghg',n_ghg,ier)
    if (n_ghg>0) then
       allocate(ghg_names(n_ghg))
       call gsi_chemguess_get('ghg',ghg_names,ier)
    endif
 endif
 n_absorbers = min_n_absorbers + n_ghg


! Are there aerosols to affect CRTM?
 call gsi_chemguess_get ('aerosols_4crtm::3d',n_aerosols_crtm,ier)
 ip25 = -1
 if (n_aerosols_crtm>0) then
    call gsi_bundlegetpointer(gsi_chemguess_bundle(1),'p25',ip25,ier)
    if ( ip25 > 0 ) then
       n_aerosols = n_aerosols_crtm + 1
    else
       n_aerosols = n_aerosols_crtm
    endif
 endif
 if(n_aerosols>0)then
    allocate(aero(nsig,n_aerosols),aero_conc(msig,n_aerosols),auxrh(msig))
    allocate(aero_names(n_aerosols))
    call gsi_chemguess_get ('aerosols::3d',aero_names,ier)

    Load_AerosolCoeff=.true.
 else
    n_aerosols=0
    Load_AerosolCoeff=.false.
 endif


! Initialize radiative transfer

 sensorlist(1)=isis
 if( crtm_coeffs_path /= "" ) then
    if(init_pass .and. mype==mype_diaghdr) write(6,*)myname_,': crtm_init() on path "'//trim(crtm_coeffs_path)//'"'
    error_status = crtm_init(sensorlist,channelinfo,&
       Process_ID=mype,Output_Process_ID=mype_diaghdr, &
       Load_CloudCoeff=Load_CloudCoeff,Load_AerosolCoeff=Load_AerosolCoeff, &
       File_Path = crtm_coeffs_path )
 else
    error_status = crtm_init(sensorlist,channelinfo,&
       Process_ID=mype,Output_Process_ID=mype_diaghdr, &
       Load_CloudCoeff=Load_CloudCoeff,Load_AerosolCoeff=Load_AerosolCoeff)
 endif
 if (error_status /= success) then
    write(6,*)myname_,':  ***ERROR*** crtm_init error_status=',error_status,&
       '   TERMINATE PROGRAM EXECUTION'
    call stop2(71)
 endif

 sensorindex = 0
 if (channelinfo(1)%sensor_id == isis) then
    sensorindex = 1

    if (isis(1:4) == 'iasi' .or. &
        trim(isis) == 'amsua_aqua' .or. &
        isis(1:4) == 'airs' .or. &
        isis(1:4) == 'cris' ) then
       subset_start = 0
       subset_end = 0
       do k=1, jpch_rad
         if (isis == nusis(k)) then
           if (subset_start == 0) subset_start = k
           subset_end = k
         endif
       end do

       error_status = crtm_channelinfo_subset(channelinfo(1), &
           channel_subset = nuchan(subset_start:subset_end))

    endif

!  This is to try to keep the CrIS naming conventions more flexible.  
!  The consistency of CRTM  and BUFR files is checked in read_cris:
else if (channelinfo(1)%sensor_id(1:8) == 'cris-fsr' .AND. isis(1:8) == 'cris-fsr') then
   sensorindex = 1
   subset_start = 0
   subset_end = 0
   do k=1, jpch_rad
     if (isis == nusis(k)) then
       if (subset_start == 0) subset_start = k
       subset_end = k
     endif
   end do

   error_status = crtm_channelinfo_subset(channelinfo(1), &
        channel_subset = nuchan(subset_start:subset_end))

else if (channelinfo(1)%sensor_id(1:4) == 'cris' .AND. isis(1:4) == 'cris') then
   sensorindex = 1
   subset_start = 0
   subset_end = 0
   do k=1, jpch_rad
     if (isis == nusis(k)) then
       if (subset_start == 0) subset_start = k
       subset_end = k
     endif
   end do

   error_status = crtm_channelinfo_subset(channelinfo(1), &
        channel_subset = nuchan(subset_start:subset_end))

else if (channelinfo(1)%sensor_id(1:4) == 'iasi' .AND. isis(1:4) == 'iasi') then
   sensorindex = 1
   subset_start = 0
   subset_end = 0
   do k=1, jpch_rad
     if (isis == nusis(k)) then
       if (subset_start == 0) subset_start = k
       subset_end = k
     endif
   end do

   error_status = crtm_channelinfo_subset(channelinfo(1), &
        channel_subset = nuchan(subset_start:subset_end))

else if (channelinfo(1)%sensor_id(1:4) == 'airs' .AND. isis(1:4) == 'airs') then
   sensorindex = 1
   subset_start = 0
   subset_end = 0
   do k=1, jpch_rad
     if (isis == nusis(k)) then
       if (subset_start == 0) subset_start = k
       subset_end = k
     endif
   end do

   error_status = crtm_channelinfo_subset(channelinfo(1), &
        channel_subset = nuchan(subset_start:subset_end))

endif 

 if (sensorindex == 0 ) then
    write(6,*)myname_,':  ***WARNING*** problem with sensorindex=',isis,&
       ' --> CAN NOT PROCESS isis=',isis,'   TERMINATE PROGRAM EXECUTION found ',&
       channelinfo(1)%sensor_id
    call stop2(71)
 endif

! Check for consistency between user specified number of channels (nchanl)
! and those defined by CRTM channelinfo structure.   Return to calling
! routine if there is a mismatch.

 if (nchanl /= crtm_channelinfo_n_channels(channelinfo(sensorindex))) then
    write(6,*)myname_,':  ***WARNING*** mismatch between nchanl=',&
       nchanl,' and n_channels=',crtm_channelinfo_n_channels(channelinfo(sensorindex)),&
       ' --> CAN NOT PROCESS isis=',isis,'   TERMINATE PROGRAM EXECUTION'
    call stop2(71)
 endif

! Allocate structures for radiative transfer

 if (lcw4crtm) allocate(rtsolution0(channelinfo(sensorindex)%n_channels,1))       

 allocate(&
    rtsolution  (channelinfo(sensorindex)%n_channels,1),&
    rtsolution_k(channelinfo(sensorindex)%n_channels,1),&
    atmosphere_k(channelinfo(sensorindex)%n_channels,1),&
    surface_k   (channelinfo(sensorindex)%n_channels,1))

!  Check to ensure that number of levels requested does not exceed crtm max

 if(msig > max_n_layers)then
    write(6,*) myname_,':  msig > max_n_layers - increase crtm max_n_layers ',&
       msig,max_n_layers
    call stop2(36)
 end if

!  Create structures for radiative transfer

 call crtm_atmosphere_create(atmosphere(1),msig,n_absorbers,n_clouds,n_aerosols_crtm)
!_RTod-NOTE if(r_kind==r_single .and. crtm_kind/=r_kind) then ! take care of case: GSI(single); CRTM(double)
!_RTod-NOTE    call crtm_surface_create(surface(1),channelinfo(sensorindex)%n_channels,tolerance=1.0e-5_crtm_kind)
!_RTod-NOTE else
!_RTod-NOTE: the following will work in single precision but issue lots of msg and remove more obs than needed
 if ( channelinfo(sensorindex)%sensor_type == crtm_microwave_sensor ) then
   call crtm_surface_create(surface(1),channelinfo(sensorindex)%n_channels)
   if (.NOT.(crtm_surface_associated(surface(1)))) then
      write(6,*)myname_,' ***ERROR** creating surface.'
   else
      surface(1)%sensordata%sensor_id        = channelinfo(sensorindex)%sensor_id
      surface(1)%sensordata%wmo_sensor_id    = channelinfo(sensorindex)%wmo_sensor_id
      surface(1)%sensordata%wmo_satellite_id = channelinfo(sensorindex)%wmo_satellite_id
      surface(1)%sensordata%sensor_channel   = channelinfo(sensorindex)%sensor_channel
   end if
 end if
!_RTod-NOTE endif
 if (lcw4crtm) &                                       
 call crtm_rtsolution_create(rtsolution0,msig) 
 call crtm_rtsolution_create(rtsolution,msig)
 call crtm_rtsolution_create(rtsolution_k,msig)
 call crtm_options_create(options,nchanl)

 if (.NOT.(crtm_atmosphere_associated(atmosphere(1)))) &
    write(6,*)myname_,' ***ERROR** creating atmosphere.'
 if (.NOT.(ANY(crtm_rtsolution_associated(rtsolution)))) &
    write(6,*)myname_,' ***ERROR** creating rtsolution.'
 if (lcw4crtm) then                                            
 if (.NOT.(ANY(crtm_rtsolution_associated(rtsolution0)))) &  
    write(6,*)' ***ERROR** creating rtsolution0.'             
 endif                                                        
 if (.NOT.(ANY(crtm_rtsolution_associated(rtsolution_k)))) &
    write(6,*)myname_,' ***ERROR** creating rtsolution_k.'
 if (.NOT.(ANY(crtm_options_associated(options)))) &
    write(6,*)myname_,' ***ERROR** creating options.'

! Turn off antenna correction

 options(1)%use_antenna_correction = .false. 

! Load surface sensor data structure

 surface(1)%sensordata%n_channels = channelinfo(sensorindex)%n_channels

!! REL-1.2 CRTM
!!  surface(1)%sensordata%select_wmo_sensor_id  = channelinfo(1)%wmo_sensor_id
!! RB-1.1.rev1855 CRTM

 atmosphere(1)%n_layers = msig
 atmosphere(1)%absorber_id(1) = H2O_ID
 atmosphere(1)%absorber_id(2) = O3_ID
 atmosphere(1)%absorber_units(1) = MASS_MIXING_RATIO_UNITS
 atmosphere(1)%absorber_units(2) = VOLUME_MIXING_RATIO_UNITS
 atmosphere(1)%level_pressure(0) = TOA_PRESSURE

! Currently all considered trace gases affect CRTM. Load trace gases into CRTM atmosphere
 ico2=-1
 if (n_ghg>0) then
    do ig=1,n_ghg
       j = min_n_absorbers + ig
       select case(trim(ghg_names(ig)))
         case('co2'); atmosphere(1)%absorber_id(j) = CO2_ID
         case('ch4'); atmosphere(1)%absorber_id(j) = CH4_ID
         case('n2o'); atmosphere(1)%absorber_id(j) = N2O_ID
         case('co') ; atmosphere(1)%absorber_id(j) = CO_ID
         case default
           call die(myname_,':  invalid absorber  TERMINATE PROGRAM'//trim(ghg_names(ig)),71) 
       end select
       atmosphere(1)%absorber_units(j) = VOLUME_MIXING_RATIO_UNITS
       if (trim(ghg_names(ig))=='co2') ico2=j
    enddo
 endif
 ico24crtm=-1
 if (ico2>0) call gsi_chemguess_get ( 'i4crtm::co2', ico24crtm, ier )

!  Allocate structure for _k arrays (jacobians)

 do ii=1,nchanl
    atmosphere_k(ii,1) = atmosphere(1)
    surface_k(ii,1)   = surface(1)
 end do

! Mapping land surface type of NMM to CRTM
 if (regional .or. nvege_type==IGBP_N_TYPES) then
    allocate(map_to_crtm_ir(nvege_type))
    allocate(map_to_crtm_mwave(nvege_type))
    if(nvege_type==USGS_N_TYPES)then
       ! Assign mapping for CRTM microwave calculations
       map_to_crtm_mwave=usgs_to_gfs
       ! map usgs to CRTM
       select case ( TRIM(CRTM_IRlandCoeff_Classification()) ) 
         case('NPOESS'); map_to_crtm_ir=usgs_to_npoess
         case('USGS')  ; map_to_crtm_ir=usgs_to_usgs
       end select
    else if(nvege_type==IGBP_N_TYPES)then
       ! Assign mapping for CRTM microwave calculations
       map_to_crtm_mwave=igbp_to_gfs
       ! nmm igbp to CRTM 
       select case ( TRIM(CRTM_IRlandCoeff_Classification()) )
         case('NPOESS'); map_to_crtm_ir=igbp_to_npoess
         case('IGBP')  ; map_to_crtm_ir=igbp_to_igbp
       end select
    else
       write(6,*)myname_,':  ***ERROR*** invalid vegetation types' &
          //' for the CRTM IRland EmisCoeff file used.', &
          ' (only 20 and 24 are setup)  nvege_type=',nvege_type, &
          '  ***STOP IN SETUPRAD***'
       call stop2(71)
    endif ! nvege_type
 endif ! regional or IGBP
    
! Calculate RH when aerosols are present and/or cloud-fraction used
 if (n_aerosols>0 .or. lcf4crtm) then
    allocate(gesqsat(lat2,lon2,nsig,nfldsig))
    ice=.true.
    iderivative=0
    do ii=1,nfldsig
       call genqsat(gesqsat(1,1,1,ii),ges_tsen(1,1,1,ii),ges_prsl(1,1,1,ii),lat2,lon2,nsig,ice,iderivative)
    end do
 endif

 return
end subroutine init_crtm
subroutine destroy_crtm
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_crtm  deallocates crtm arrays
!   prgmmr: parrish          org: np22                date: 2005-01-22
!
! abstract: deallocates crtm arrays
!
! program history log:
!   2010-08-17  derber 
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  implicit none

  character(len=*),parameter::myname_ = myname//'*destroy_crtm'
  integer(i_kind) error_status

  error_status = crtm_destroy(channelinfo)
  if (error_status /= success) &
     write(6,*)myname_,':  ***ERROR*** error_status=',error_status
  if (n_aerosols>0 .or. lcf4crtm) then
     deallocate(gesqsat)
  endif
  call crtm_atmosphere_destroy(atmosphere(1))
  call crtm_surface_destroy(surface(1))
  if (lcw4crtm) &     
  call crtm_rtsolution_destroy(rtsolution0)    
  call crtm_rtsolution_destroy(rtsolution)
  call crtm_rtsolution_destroy(rtsolution_k)
  call crtm_options_destroy(options)
  if (crtm_atmosphere_associated(atmosphere(1))) &
     write(6,*)myname_,' ***ERROR** destroying atmosphere.'
  if (crtm_surface_associated(surface(1))) &
     write(6,*)myname_,' ***ERROR** destroying surface.'
  if (ANY(crtm_rtsolution_associated(rtsolution))) &
     write(6,*)myname_,' ***ERROR** destroying rtsolution.'
  if (lcw4crtm) then            
  if (ANY(crtm_rtsolution_associated(rtsolution0))) &    
     write(6,*)' ***ERROR** destroying rtsolution0.'    
  endif                                                 
  if (ANY(crtm_rtsolution_associated(rtsolution_k))) &
     write(6,*)myname_,' ***ERROR** destroying rtsolution_k.'
  if (ANY(crtm_options_associated(options))) &
     write(6,*)myname_,' ***ERROR** destroying options.'
  deallocate(rtsolution,atmosphere_k,surface_k,rtsolution_k)
  if (lcw4crtm) &         
  deallocate(rtsolution0) 
  if(n_aerosols>0)then
     deallocate(aero_names)
     deallocate(aero,aero_conc,auxrh)
     if(n_aerosols_jac>0) deallocate(iaero_jac)
  endif
  if (n_ghg>0) then
     deallocate(ghg_names)
  endif
  if(allocated(icloud)) deallocate(icloud)
  if(allocated(cloud)) deallocate(cloud)
  if(allocated(cloudefr)) deallocate(cloudefr)
  if(allocated(cloud_names)) deallocate(cloud_names)
  if(allocated(jcloud)) deallocate(jcloud)
  if(allocated(cloud_cont)) deallocate(cloud_cont)
  if(allocated(cloud_efr)) deallocate(cloud_efr)
  if(allocated(icw)) deallocate(icw)
  if(regional .or. nvege_type==IGBP_N_TYPES)deallocate(map_to_crtm_ir)
  if(regional .or. nvege_type==IGBP_N_TYPES)deallocate(map_to_crtm_mwave)

  return
end subroutine destroy_crtm
subroutine call_crtm(obstype,obstime,data_s,nchanl,nreal,ich, &
                   h,q,clw_guess,prsl,prsi, &
                   trop5,tzbgr,dtsavg,sfc_speed,&
                   tsim,emissivity,ptau5,ts, &
                   emissivity_k,temp,wmix,jacobian,error_status,tsim_clr, &
                   layer_od,jacobian_aero)  
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    call_crtm   creates vertical profile of t,q,oz,p,zs,etc., 
!             calls crtm, and does adjoint of creation (where necessary) for setuprad    
!   prgmmr: parrish          org: np22                date: 1990-10-11
!
! abstract: creates vertical profile of t,q,oz,p,zs,etc., 
!             calls crtm, and does adjoint of creation (where necessary) for setuprad
!
! program history log:
!   2010-08-17  derber - modify from intrppx and add threading
!   2011-02-23  todling/da silva - revisit interface to fill in aerosols
!   2011-03-25  yang   - turn off the drop-off of co2 amount when using climatological CO2
!   2011-05-03  todling - merge with Min-Jeong's MW cloudy radiance; combine w/ metguess
!                         (did not include tendencies since they were calc but not used)
!   2011-05-17  auligne/todling - add handling for hydrometeors
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2011-07-05  zhu - add cloud_efr & cloudefr; add cloud_efr & jcloud in the interface of Set_CRTM_Cloud
!   2011-07-05  zhu - rewrite cloud_cont & cwj for cloud control variables (lcw4crtm)
!   2012-03-12  veldelst-- add a internal interpolation function (option)
!   2012-04-25  yang - modify to use trace gas chem_bundle. Trace gas variables are 
!                       invoked by the global_anavinfo.ghg.l64.txt
!   2013-02-25  zhu - add cold_start option for regional applications
!   2014-01-31  mkim-- remove 60.0degree boundary for icmask for all-sky MW radiance DA 
!   2014-02-26  zhu - add non zero jacobian so jacobian will be produced for            
!                     clear-sky background or background with small amount of cloud     
!   2014-04-27  eliu - add option to calculate clear-sky Tb under all-sky condition                
!   2015-02-27  eliu-- wind direction fix for using CRTM FASTEM model 
!   2015-03-23  zaizhong ma - add Himawari-8 ahi
!
!   input argument list:
!     obstype      - type of observations for which to get profile
!     obstime      - time of observations for which to get profile
!     data_s       - array containing input data information
!     nchanl       - number of channels
!     nreal        - number of descriptor information in data_s
!     ich          - channel number array
!
!   output argument list:
!     h            - interpolated temperature
!     q            - interpolated specific humidity (max(qsmall,q))
!     prsl         - interpolated layer pressure (nsig)
!     prsi         - interpolated level pressure (nsig+1)
!     trop5        - interpolated tropopause pressure
!     tzbgr        - water surface temperature used in Tz retrieval
!     dtsavg       - delta average skin temperature over surface types
!     uu5          - interpolated bottom sigma level zonal wind    
!     vv5          - interpolated bottom sigma level meridional wind  
!     tsim         - simulated brightness temperatures
!     emissivity   - surface emissivities
!     ptau5        - level transmittances
!     ts           - skin temperature sensitivities
!     emissivity_k - surface emissivity sensitivities             
!     temp         - temperature sensitivities
!     wmix         - humidity sensitivities
!     jacobian     - nsigradjac level jacobians for use in intrad and stprad
!     error_status - error status from crtm
!     layer_od     - layer optical depth
!     jacobian_aero- nsigaerojac level jacobians for use in intaod
!     tsim_clr     - option to output simulated brightness temperatures for clear sky                  
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  use radinfo, only: ifactq
  use radinfo, only: radjacindxs,nsigradjac
  use gsi_nstcouplermod, only: nst_gsi,nstinfo,fac_dtl,fac_tsl
  use guess_grids, only: ges_tsen,&
      ges_prsl,ges_prsi,tropprs,dsfct,add_rtm_layers, &
      hrdifsig,nfldsig,hrdifsfc,nfldsfc,ntguessfc,isli2,sno2
  use cloud_efr_mod, only: efr_ql,efr_qi,efr_qr,efr_qs,efr_qg,efr_qh
  use ncepgfs_ghg, only: co2vmr_def,ch4vmr_def,n2ovmr_def,covmr_def
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_chemguess_mod, only: gsi_chemguess_bundle   ! for now, a common block
  use gsi_chemguess_mod, only: gsi_chemguess_get
  use gsi_metguess_mod,  only: gsi_metguess_bundle   ! for now, a common block
  use gsi_metguess_mod,  only: gsi_metguess_get
  use gridmod, only: istart,jstart,nlon,nlat,lon1
  use regional_io, only: cold_start
  use constants, only: zero,one,one_tenth,fv,r0_05,r10,r100,r1000,constoz,grav,rad2deg,deg2rad, &
      sqrt_tiny_r_kind,constoz, rd, rd_over_g, two, three, four,five,t0c
  use constants, only: max_varname_length,pi  
  use set_crtm_aerosolmod, only: set_crtm_aerosol
  use set_crtm_cloudmod, only: set_crtm_cloud
  use crtm_module, only: limit_exp,o3_id
  use obsmod, only: iadate
  use aeroinfo, only: nsigaerojac

  implicit none

! Declare passed variables
  real(r_kind)                          ,intent(in   ) :: obstime
  integer(i_kind)                       ,intent(in   ) :: nchanl,nreal
  integer(i_kind),dimension(nchanl)     ,intent(in   ) :: ich
  real(r_kind)                          ,intent(  out) :: trop5,tzbgr
  real(r_kind),dimension(nsig)          ,intent(  out) :: h,q,prsl
  real(r_kind),dimension(nsig+1)        ,intent(  out) :: prsi
  real(r_kind)                          ,intent(  out) :: sfc_speed,dtsavg
  real(r_kind),dimension(nchanl+nreal)  ,intent(in   ) :: data_s
  real(r_kind),dimension(nchanl)        ,intent(  out) :: tsim,emissivity,ts,emissivity_k
  character(10)                         ,intent(in   ) :: obstype
  integer(i_kind)                       ,intent(  out) :: error_status
  real(r_kind),dimension(nsig,nchanl)   ,intent(  out) :: temp,ptau5,wmix
  real(r_kind),dimension(nsigradjac,nchanl),intent(out):: jacobian
  real(r_kind)                          ,intent(  out) :: clw_guess
  real(r_kind),dimension(nchanl)        ,intent(  out), optional  :: tsim_clr      
  real(r_kind),dimension(nsigaerojac,nchanl),intent(out),optional :: jacobian_aero
  real(r_kind),dimension(nsig,nchanl)   ,intent(  out)  ,optional :: layer_od

! Declare local parameters
  character(len=*),parameter::myname_=myname//'*call_crtm'
  real(r_kind),parameter:: minsnow=one_tenth
  real(r_kind),parameter:: qsmall  = 1.e-6_r_kind
  real(r_kind),parameter:: ozsmall = 1.e-10_r_kind
  real(r_kind),parameter:: jac_pert  = 1.0_r_kind
  real(r_kind),parameter:: small_wind = 1.e-3_r_kind
  real(r_kind),parameter:: windscale = 999999.0_r_kind
  real(r_kind),parameter:: windlimit = 0.0001_r_kind
  real(r_kind),parameter:: quadcof  (4, 2  ) =      &
      reshape((/0.0_r_kind, 1.0_r_kind, 1.0_r_kind, 2.0_r_kind, 1.0_r_kind, &
               -1.0_r_kind, 1.0_r_kind, -1.0_r_kind/), (/4, 2/))

! Declare local variables  
  integer(i_kind):: iquadrant  
  integer(i_kind):: ier,ii,kk,kk2,i,itype,leap_day,day_of_year
  integer(i_kind):: ig,istatus
  integer(i_kind):: j,k,m1,ix,ix1,ixp,iy,iy1,iyp,m,iii
  integer(i_kind):: itsig,itsigp,itsfc,itsfcp
  integer(i_kind):: istyp00,istyp01,istyp10,istyp11
  integer(i_kind):: iqs,iozs
  integer(i_kind),dimension(8)::obs_time,anal_time
  integer(i_kind),dimension(msig) :: klevel

! ****************************** 
! Constrained indexing for lai
! CRTM 2.1 implementation change
! ******************************
  integer(i_kind):: lai_type

  real(r_kind):: wind10,wind10_direction,windratio,windangle 
  real(r_kind):: w00,w01,w10,w11,kgkg_kgm2,f10,panglr,dx,dy
! real(r_kind):: w_weights(4)
  real(r_kind):: delx,dely,delx1,dely1,dtsig,dtsigp,dtsfc,dtsfcp
  real(r_kind):: sst00,sst01,sst10,sst11,total_od,term,uu5,vv5, ps
  real(r_kind):: sno00,sno01,sno10,sno11,secant_term
  real(r_kind),dimension(0:3):: wgtavg
  real(r_kind),dimension(nsig,nchanl):: omix
  real(r_kind),dimension(nsig,nchanl,n_aerosols_jac):: jaero
  real(r_kind),dimension(nchanl) :: uwind_k,vwind_k
  real(r_kind),dimension(msig+1) :: prsi_rtm
  real(r_kind),dimension(msig)  :: prsl_rtm
  real(r_kind),dimension(msig)  :: auxq,auxdp
  real(r_kind),dimension(nsig)  :: poz
  real(r_kind),dimension(nsig)  :: rh,qs,qclr
  real(r_kind),dimension(5)     :: tmp_time
  real(r_kind),dimension(0:3)   :: dtskin
  real(r_kind),dimension(msig)  :: c6
  real(r_kind),dimension(nsig)  :: c2,c3,c4,c5
  real(r_kind) cf
  real(r_kind),dimension(nsig) :: ugkg_kgm2,cwj
  real(r_kind),allocatable,dimension(:,:) :: tgas1d
  real(r_kind),pointer,dimension(:,:  )::psges_itsig =>NULL()
  real(r_kind),pointer,dimension(:,:  )::psges_itsigp=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::uges_itsig =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::uges_itsigp=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::vges_itsig =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::vges_itsigp=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::qges_itsig =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::qges_itsigp=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ozges_itsig =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::ozges_itsigp=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::cfges_itsig =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::cfges_itsigp=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::tgasges_itsig =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::tgasges_itsigp=>NULL()
  real(r_kind),pointer,dimension(:,:,:)::aeroges_itsig =>NULL()
  real(r_kind),pointer,dimension(:,:,:)::aeroges_itsigp=>NULL()

  logical :: sea,icmask   

  integer(i_kind),parameter,dimension(12):: mday=(/0,31,59,90,&
       120,151,181,212,243,273,304,334/)
  real(r_kind) ::   lai

  m1=mype+1

  dx  = data_s(ilat)                 ! grid relative latitude
  dy  = data_s(ilon)                 ! grid relative longitude

! Set spatial interpolation indices and weights
  ix1=dx
  ix1=max(1,min(ix1,nlat))
  delx=dx-ix1
  delx=max(zero,min(delx,one))
  ix=ix1-istart(m1)+2
  ixp=ix+1
  if(ix1==nlat) then
     ixp=ix
  end if
  delx1=one-delx

  iy1=dy
  dely=dy-iy1
  iy=iy1-jstart(m1)+2
  if(iy<1) then
     iy1=iy1+nlon
     iy=iy1-jstart(m1)+2
  end if
  if(iy>lon1+1) then
     iy1=iy1-nlon
     iy=iy1-jstart(m1)+2
  end if
  iyp=iy+1
  dely1=one-dely

  w00=delx1*dely1; w10=delx*dely1; w01=delx1*dely; w11=delx*dely
! w_weights = (/w00,w10,w01,w11/)


! Get time interpolation factors for sigma files
  if(obstime > hrdifsig(1) .and. obstime < hrdifsig(nfldsig))then
     do j=1,nfldsig-1
        if(obstime > hrdifsig(j) .and. obstime <= hrdifsig(j+1))then
           itsig=j
           itsigp=j+1
           dtsig=((hrdifsig(j+1)-obstime)/(hrdifsig(j+1)-hrdifsig(j)))
        end if
     end do
  else if(obstime <=hrdifsig(1))then
     itsig=1
     itsigp=1
     dtsig=one
  else
     itsig=nfldsig
     itsigp=nfldsig
     dtsig=one
  end if
  dtsigp=one-dtsig

! Get time interpolation factors for surface files
  if(obstime > hrdifsfc(1) .and. obstime < hrdifsfc(nfldsfc))then
     do j=1,nfldsfc-1
        if(obstime > hrdifsfc(j) .and. obstime <= hrdifsfc(j+1))then
           itsfc=j
           itsfcp=j+1
           dtsfc=((hrdifsfc(j+1)-obstime)/(hrdifsfc(j+1)-hrdifsfc(j)))
        end if
     end do
  else if(obstime <=hrdifsfc(1))then
     itsfc=1
     itsfcp=1
     dtsfc=one
  else
     itsfc=nfldsfc
     itsfcp=nfldsfc
     dtsfc=one
  end if
  dtsfcp=one-dtsfc

  ier=0
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsig ),'ps',psges_itsig ,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'ps',psges_itsigp,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsig ),'u' ,uges_itsig  ,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'u' ,uges_itsigp ,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsig ),'v' ,vges_itsig  ,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'v' ,vges_itsigp ,istatus)
  ier=ier+istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsig ),'oz',ozges_itsig ,iozs)
  iozs=istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'oz',ozges_itsigp,iozs)
  iozs=iozs+istatus

  call gsi_bundlegetpointer(gsi_metguess_bundle(itsig ),'q',qges_itsig ,istatus)
  iqs=istatus
  call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'q',qges_itsigp,istatus)
  iqs=iqs+istatus

  if (lcf4crtm) then
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsig ),'cf',cfges_itsig ,ier)
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'cf',cfges_itsigp,ier)
    if (iqs/=0) call die(myname_,'inconsistent cloud setting, missing q',2)
  endif


! Space-time interpolation of temperature (h) and q fields from sigma files
!$omp parallel do  schedule(dynamic,1) private(k,cf,ii,iii)
  do k=1,nsig
    if(k == 1)then
        jacobian=zero
!    Set surface type flag.  (Same logic as in subroutine deter_sfc)

        istyp00 = isli2(ix ,iy )
        istyp10 = isli2(ixp,iy )
        istyp01 = isli2(ix ,iyp)
        istyp11 = isli2(ixp,iyp)
        sno00= sno2(ix ,iy ,itsfc)*dtsfc+sno2(ix ,iy ,itsfcp)*dtsfcp
        sno01= sno2(ix ,iyp,itsfc)*dtsfc+sno2(ix ,iyp,itsfcp)*dtsfcp
        sno10= sno2(ixp,iy ,itsfc)*dtsfc+sno2(ixp,iy ,itsfcp)*dtsfcp
        sno11= sno2(ixp,iyp,itsfc)*dtsfc+sno2(ixp,iyp,itsfcp)*dtsfcp
        if(istyp00 >= 1 .and. sno00 > minsnow)istyp00 = 3
        if(istyp01 >= 1 .and. sno01 > minsnow)istyp01 = 3
        if(istyp10 >= 1 .and. sno10 > minsnow)istyp10 = 3
        if(istyp11 >= 1 .and. sno11 > minsnow)istyp11 = 3

!        Find delta Surface temperatures for all surface types

        sst00= dsfct(ix ,iy,ntguessfc) ; sst01= dsfct(ix ,iyp,ntguessfc)
        sst10= dsfct(ixp,iy,ntguessfc) ; sst11= dsfct(ixp,iyp,ntguessfc) 
        dtsavg=sst00*w00+sst10*w10+sst01*w01+sst11*w11

        dtskin(0:3)=zero
        wgtavg(0:3)=zero

        if(istyp00 == 1)then
           wgtavg(1) = wgtavg(1) + w00
           dtskin(1)=dtskin(1)+w00*sst00
        else if(istyp00 == 2)then
           wgtavg(2) = wgtavg(2) + w00
           dtskin(2)=dtskin(2)+w00*sst00
        else if(istyp00 == 3)then
           wgtavg(3) = wgtavg(3) + w00
           dtskin(3)=dtskin(3)+w00*sst00
        else
           wgtavg(0) = wgtavg(0) + w00
           dtskin(0)=dtskin(0)+w00*sst00
        end if

        if(istyp01 == 1)then
           wgtavg(1) = wgtavg(1) + w01
           dtskin(1)=dtskin(1)+w01*sst01
        else if(istyp01 == 2)then
           wgtavg(2) = wgtavg(2) + w01
           dtskin(2)=dtskin(2)+w01*sst01
        else if(istyp01 == 3)then
           wgtavg(3) = wgtavg(3) + w01
           dtskin(3)=dtskin(3)+w01*sst01
        else
           wgtavg(0) = wgtavg(0) + w01
           dtskin(0)=dtskin(0)+w01*sst01
        end if

        if(istyp10 == 1)then
           wgtavg(1) = wgtavg(1) + w10
           dtskin(1)=dtskin(1)+w10*sst10
        else if(istyp10 == 2)then
           wgtavg(2) = wgtavg(2) + w10
           dtskin(2)=dtskin(2)+w10*sst10
        else if(istyp10 == 3)then
           wgtavg(3) = wgtavg(3) + w10
           dtskin(3)=dtskin(3)+w10*sst10
        else
           wgtavg(0) = wgtavg(0) + w10
           dtskin(0)=dtskin(0)+w10*sst10
        end if

        if(istyp11 == 1)then
           wgtavg(1) = wgtavg(1) + w11
           dtskin(1)=dtskin(1)+w11*sst11
        else if(istyp11 == 2)then
           wgtavg(2) = wgtavg(2) + w11
           dtskin(2)=dtskin(2)+w11*sst11
        else if(istyp11 == 3)then
           wgtavg(3) = wgtavg(3) + w11
           dtskin(3)=dtskin(3)+w11*sst11
        else
           wgtavg(0) = wgtavg(0) + w11
           dtskin(0)=dtskin(0)+w11*sst11
        end if

        if(wgtavg(0) > zero)then
           dtskin(0) = dtskin(0)/wgtavg(0)
        else
           dtskin(0) = dtsavg
        end if
        if(wgtavg(1) > zero)then
           dtskin(1) = dtskin(1)/wgtavg(1)
        else
           dtskin(1) = dtsavg
        end if
        if(wgtavg(2) > zero)then
           dtskin(2) = dtskin(2)/wgtavg(2)
        else
           dtskin(2) = dtsavg
        end if
        if(wgtavg(3) > zero)then
           dtskin(3) = dtskin(3)/wgtavg(3)
        else
           dtskin(3) = dtsavg
        end if

        if (n_clouds>0) then
            ps=(psges_itsig (ix,iy )*w00+psges_itsig (ixp,iy )*w10+ &
                psges_itsig (ix,iyp)*w01+psges_itsig (ixp,iyp)*w11)*dtsig + &
               (psges_itsigp(ix,iy )*w00+psges_itsigp(ixp,iy )*w10+ &
                psges_itsigp(ix,iyp)*w01+psges_itsigp(ixp,iyp)*w11)*dtsigp
        endif

!       skip loading surface structure if obstype is modis_aod
        if (trim(obstype) /= 'modis_aod') then

!       Load surface structure

! **NOTE:  The model surface type --> CRTM surface type
!          mapping below is specific to the versions NCEP
!          GFS and NNM as of Summer 2016

           itype  = nint(data_s(ivty))
           istype = nint(data_s(isty))
           if (regional .or. nvege_type==IGBP_N_TYPES) then
              itype  = min(max(1,itype),nvege_type)
              istype = min(max(1,istype),SOIL_N_TYPES)
              surface(1)%land_type = max(1,map_to_crtm_ir(itype))
              surface(1)%Vegetation_Type = max(1,map_to_crtm_mwave(itype))
              surface(1)%Soil_Type = map_soil_to_crtm(istype)
              lai_type = map_to_crtm_mwave(itype)
           elseif (nvege_type==GFS_N_TYPES) then
              itype  = min(max(0,itype),GFS_VEGETATION_N_TYPES)
              istype = min(max(1,istype),GFS_SOIL_N_TYPES)
              surface(1)%land_type = gfs_to_crtm(itype)
              surface(1)%Vegetation_Type = max(1,itype)
              surface(1)%Soil_Type = istype
              lai_type = itype
           else
              write(6,*)myname_,':  ***ERROR*** invalid vegetation types' &
                 //' the information does not match any currenctly.', &
                 ' supported surface type maps to the CRTM,', &
                 '  ***STOP IN SETUPRAD***'
                 call stop2(71)
           end if
                                    
           if (lwind) then
!        Interpolate lowest level winds to observation location 

             uu5=(uges_itsig (ix,iy ,1)*w00+uges_itsig (ixp,iy ,1)*w10+ &
                  uges_itsig (ix,iyp,1)*w01+uges_itsig (ixp,iyp,1)*w11)*dtsig + &
                 (uges_itsigp(ix,iy ,1)*w00+uges_itsigp(ixp,iy ,1)*w10+ &
                  uges_itsigp(ix,iyp,1)*w01+uges_itsigp(ixp,iyp,1)*w11)*dtsigp
             vv5=(vges_itsig (ix,iy ,1)*w00+vges_itsig (ixp,iy ,1)*w10+ &
                  vges_itsig (ix,iyp,1)*w01+vges_itsig (ixp,iyp,1)*w11)*dtsig + &
                 (vges_itsigp(ix,iy ,1)*w00+vges_itsigp(ixp,iy ,1)*w10+ &
                  vges_itsigp(ix,iyp,1)*w01+vges_itsigp(ixp,iyp,1)*w11)*dtsigp
             f10=data_s(iff10)
             sfc_speed = f10*sqrt(uu5*uu5+vv5*vv5)
             wind10    = sfc_speed 
             if (uu5*f10 >= 0.0_r_kind .and. vv5*f10 >= 0.0_r_kind) iquadrant = 1 
             if (uu5*f10 >= 0.0_r_kind .and. vv5*f10 <  0.0_r_kind) iquadrant = 2 
             if (uu5*f10 <  0.0_r_kind .and. vv5*f10 >= 0.0_r_kind) iquadrant = 4 
             if (uu5*f10 <  0.0_r_kind .and. vv5*f10 <  0.0_r_kind) iquadrant = 3 
             if (abs(vv5*f10) >= windlimit) then 
                 windratio = (uu5*f10) / (vv5*f10) 
             else 
                 windratio = 0.0_r_kind 
                 if (abs(uu5*f10) > windlimit) then 
                     windratio = windscale * uu5*f10 
                 endif 
             endif 
             windangle        = atan(abs(windratio))   ! wind azimuth is in radians 
             wind10_direction = quadcof(iquadrant, 1) * pi + windangle * quadcof(iquadrant, 2)   
             surface(1)%wind_speed           = sfc_speed
             surface(1)%wind_direction       = rad2deg*wind10_direction
           else !RTodling: not sure the following option makes any sense
             surface(1)%wind_speed           = zero
             surface(1)%wind_direction       = zero
           endif

!       CRTM will reject surface coverages if greater than one and it is possible for
!       these values to be larger due to round off.

           surface(1)%water_coverage        = min(max(zero,data_s(ifrac_sea)),one)
           surface(1)%land_coverage         = min(max(zero,data_s(ifrac_lnd)),one)
           surface(1)%ice_coverage          = min(max(zero,data_s(ifrac_ice)),one)
           surface(1)%snow_coverage         = min(max(zero,data_s(ifrac_sno)),one)
     
!
!       get vegetation lai from summer and winter values.
!

           surface(1)%Lai  = zero
           if (surface(1)%land_coverage>zero) then
              if(lai_type>0)then
                call get_lai(data_s,nchanl,nreal,itime,ilate,lai_type,lai)
                surface(1)%Lai  = lai   ! LAI  
              endif     
     
              ! for Glacial land ice soil type and vegetation type
              if(surface(1)%Soil_Type == 9 .OR. surface(1)%Vegetation_Type == 13) then
                 surface(1)%ice_coverage = min(surface(1)%ice_coverage + surface(1)%land_coverage, one)
                 surface(1)%land_coverage = zero
              endif
           endif

           surface(1)%water_temperature     = max(data_s(its_sea)+dtskin(0),270._r_kind)
           if(nst_gsi>1 .and. surface(1)%water_coverage>zero) then
              surface(1)%water_temperature  = max(data_s(itref)+data_s(idtw)-data_s(idtc)+dtskin(0),271._r_kind)
           endif
           surface(1)%land_temperature      = data_s(its_lnd)+dtskin(1)
           surface(1)%ice_temperature       = min(data_s(its_ice)+dtskin(2),280._r_kind)
           surface(1)%snow_temperature      = min(data_s(its_sno)+dtskin(3),280._r_kind)
           surface(1)%soil_moisture_content = data_s(ism)
           surface(1)%vegetation_fraction   = data_s(ivfr)
           surface(1)%soil_temperature      = data_s(istp)
           surface(1)%snow_depth            = data_s(isn)

           sea = min(max(zero,data_s(ifrac_sea)),one)  >= 0.99_r_kind 
           icmask = sea 

!       assign tzbgr for Tz retrieval when necessary
           tzbgr = surface(1)%water_temperature

        endif ! end of loading surface structure

!       Load geometry structure

!       skip loading geometry structure if obstype is modis_aod
!       iscan_ang,ilzen_ang,ilazi_ang are not available in the modis aod bufr file
!       also, geometryinfo is not needed in crtm aod calculation
        if ( trim(obstype) /= 'modis_aod' ) then
           panglr = data_s(iscan_ang)
           if(obstype == 'goes_img' .or. obstype == 'seviri')panglr = zero
           geometryinfo(1)%sensor_zenith_angle = data_s(ilzen_ang)*rad2deg  ! local zenith angle
           geometryinfo(1)%source_zenith_angle = data_s(iszen_ang)          ! solar zenith angle
           geometryinfo(1)%sensor_azimuth_angle = data_s(ilazi_ang)         ! local zenith angle
           geometryinfo(1)%source_azimuth_angle = data_s(isazi_ang)         ! solar zenith angle
           geometryinfo(1)%sensor_scan_angle   = panglr*rad2deg             ! scan angle
           geometryinfo(1)%ifov                = nint(data_s(iscan_pos))    ! field of view position

!        For some microwave instruments the solar and sensor azimuth angles can be
!        missing  (given a value of 10^11).  Set these to zero to get past CRTM QC.

           if (geometryinfo(1)%source_azimuth_angle > 360.0_r_kind .OR. &
               geometryinfo(1)%source_azimuth_angle < zero ) &
               geometryinfo(1)%source_azimuth_angle = zero
           if (geometryinfo(1)%sensor_azimuth_angle > 360.0_r_kind .OR. &
               geometryinfo(1)%sensor_azimuth_angle < zero ) &
               geometryinfo(1)%sensor_azimuth_angle = zero

        endif ! end of loading geometry structure

!       Special block for SSU cell pressure leakage correction.   Need to compute
!       observation time and load into Time component of geometryinfo structure.
!       geometryinfo%time is only defined in CFSRR CRTM.
        if (obstype == 'ssu') then

!          Compute absolute observation time

           anal_time=0
           obs_time=0
           tmp_time=zero
           tmp_time(2)=obstime
           anal_time(1)=iadate(1)
           anal_time(2)=iadate(2)
           anal_time(3)=iadate(3)
           anal_time(5)=iadate(4)

!external-subroutine w3movdat()

           call w3movdat(tmp_time,anal_time,obs_time)

!          Compute decimal year, for example 1/10/1983
!          d_year = 1983.0 + 10.0/365.0

           leap_day = 0
           if( mod(obs_time(1),4)==0 ) then
              if( (mod(obs_time(1),100)/=0).or.(mod(obs_time(1),400)==0) ) leap_day = 1
           endif
           day_of_year = mday(obs_time(2)) + obs_time(3)
           if(obs_time(2) > 2) day_of_year = day_of_year + leap_day

           call ssu_input_setvalue( options%SSU, &
              Time=float(obs_time(1)) + float(day_of_year)/(365.0_r_kind+leap_day))

        endif

!       Load surface sensor data structure

        do i=1,nchanl


!        Set-up to return Tb jacobians.                                         

           rtsolution_k(i,1)%radiance = zero
           rtsolution_k(i,1)%brightness_temperature = one

           if (trim(obstype) /= 'modis_aod')then

!        Pass CRTM array of tb for surface emissiviy calculations
           if ( channelinfo(1)%sensor_type == crtm_microwave_sensor .and. & 
                crtm_surface_associated(surface(1)) ) & 
                surface(1)%sensordata%tb(i) = data_s(nreal+i) 

!       set up to return layer_optical_depth jacobians
              rtsolution_k(i,1)%layer_optical_depth = one
           endif

        end do

     end if

     h(k)  =(ges_tsen(ix ,iy ,k,itsig )*w00+ &
             ges_tsen(ixp,iy ,k,itsig )*w10+ &
             ges_tsen(ix ,iyp,k,itsig )*w01+ &
             ges_tsen(ixp,iyp,k,itsig )*w11)*dtsig + &
            (ges_tsen(ix ,iy ,k,itsigp)*w00+ &
             ges_tsen(ixp,iy ,k,itsigp)*w10+ &
             ges_tsen(ix ,iyp,k,itsigp)*w01+ &
             ges_tsen(ixp,iyp,k,itsigp)*w11)*dtsigp
! Interpolate layer pressure to observation point
     prsl(k)=(ges_prsl(ix ,iy ,k,itsig )*w00+ &
              ges_prsl(ixp,iy ,k,itsig )*w10+ &
              ges_prsl(ix ,iyp,k,itsig )*w01+ &
              ges_prsl(ixp,iyp,k,itsig )*w11)*dtsig + &
             (ges_prsl(ix ,iy ,k,itsigp)*w00+ &
              ges_prsl(ixp,iy ,k,itsigp)*w10+ &
              ges_prsl(ix ,iyp,k,itsigp)*w01+ &
              ges_prsl(ixp,iyp,k,itsigp)*w11)*dtsigp
! Interpolate level pressure to observation point
     prsi(k)=(ges_prsi(ix ,iy ,k,itsig )*w00+ &
              ges_prsi(ixp,iy ,k,itsig )*w10+ &
              ges_prsi(ix ,iyp,k,itsig )*w01+ &
              ges_prsi(ixp,iyp,k,itsig )*w11)*dtsig + &
             (ges_prsi(ix ,iy ,k,itsigp)*w00+ &
              ges_prsi(ixp,iy ,k,itsigp)*w10+ &
              ges_prsi(ix ,iyp,k,itsigp)*w01+ &
              ges_prsi(ixp,iyp,k,itsigp)*w11)*dtsigp
     if (iqs==0) then
        q(k)  =(qges_itsig (ix ,iy ,k)*w00+ &
                qges_itsig (ixp,iy ,k)*w10+ &
                qges_itsig (ix ,iyp,k)*w01+ &
                qges_itsig (ixp,iyp,k)*w11)*dtsig + &
               (qges_itsigp(ix ,iy ,k)*w00+ &
                qges_itsigp(ixp,iy ,k)*w10+ &
                qges_itsigp(ix ,iyp,k)*w01+ &
                qges_itsigp(ixp,iyp,k)*w11)*dtsigp
!  Ensure q is greater than or equal to qsmall
        q(k)=max(qsmall,q(k))
     else
        q(k)  = qsmall
     endif
     if (lcf4crtm) then
        cf    =(cfges_itsig (ix ,iy ,k)*w00+ &
                cfges_itsig (ixp,iy ,k)*w10+ &
                cfges_itsig (ix ,iyp,k)*w01+ &
                cfges_itsig (ixp,iyp,k)*w11)*dtsig + &
               (cfges_itsigp(ix ,iy ,k)*w00+ &
                cfges_itsigp(ixp,iy ,k)*w10+ &
                cfges_itsigp(ix ,iyp,k)*w01+ &
                cfges_itsigp(ixp,iyp,k)*w11)*dtsigp
        qs(k) =(gesqsat(ix ,iy ,k,itsig )*w00+ &
                gesqsat(ixp,iy ,k,itsig )*w10+ &
                gesqsat(ix ,iyp,k,itsig )*w01+ &
                gesqsat(ixp,iyp,k,itsig )*w11)*dtsig + &
               (gesqsat(ix ,iy ,k,itsigp)*w00+ &
                gesqsat(ixp,iy ,k,itsigp)*w10+ &
                gesqsat(ix ,iyp,k,itsigp)*w01+ &
                gesqsat(ixp,iyp,k,itsigp)*w11)*dtsigp

        if (cf<0.01_r_kind) then
           qclr(k) = q(k)
        else 
           qclr(k) = (q(k) - cf*qs(k))/(one-cf)
           if (qclr(k)<zero) then
              qclr(k)=max(qsmall,qclr(k))
           endif
        endif 

! Create constants for later

        qclr(k)=max(qsmall,qclr(k))
        c2(k)=one/(one+fv*qclr(k))
        c3(k)=one/(one-qclr(k))
     else
        c2(k)=one/(one+fv*q(k))
        c3(k)=one/(one-q(k))
     endif
     c4(k)=fv*h(k)*c2(k)
     c5(k)=r1000*c3(k)*c3(k)
! Space-time interpolation of ozone(poz)
     if (iozs==0) then
         poz(k)=((ozges_itsig (ix ,iy ,k)*w00+ &
                  ozges_itsig (ixp,iy ,k)*w10+ &
                  ozges_itsig (ix ,iyp,k)*w01+ &
                  ozges_itsig (ixp,iyp,k)*w11)*dtsig + &
                 (ozges_itsigp(ix ,iy ,k)*w00+ &
                  ozges_itsigp(ixp,iy ,k)*w10+ &
                  ozges_itsigp(ix ,iyp,k)*w01+ &
                  ozges_itsigp(ixp,iyp,k)*w11)*dtsigp)*constoz

!        Ensure ozone is greater than ozsmall

         poz(k)=max(ozsmall,poz(k))
     endif ! oz
! Quantities required for MW cloudy radiance calculations

     if (n_clouds>0) then
        do ii=1,n_clouds
           iii=jcloud(ii)
           cloud(k,ii) =(gsi_metguess_bundle(itsig )%r3(icloud(iii))%q(ix ,iy ,k)*w00+ &     ! kg/kg
                         gsi_metguess_bundle(itsig )%r3(icloud(iii))%q(ixp,iy ,k)*w10+ &
                         gsi_metguess_bundle(itsig )%r3(icloud(iii))%q(ix ,iyp,k)*w01+ &
                         gsi_metguess_bundle(itsig )%r3(icloud(iii))%q(ixp,iyp,k)*w11)*dtsig + &
                        (gsi_metguess_bundle(itsigp)%r3(icloud(iii))%q(ix ,iy ,k)*w00+ &
                         gsi_metguess_bundle(itsigp)%r3(icloud(iii))%q(ixp,iy ,k)*w10+ &
                         gsi_metguess_bundle(itsigp)%r3(icloud(iii))%q(ix ,iyp,k)*w01+ &
                         gsi_metguess_bundle(itsigp)%r3(icloud(iii))%q(ixp,iyp,k)*w11)*dtsigp
           cloud(k,ii)=max(cloud(k,ii),zero)

           if (regional .and. (.not. wrf_mass_regional)) then
               if (trim(cloud_names(iii))== 'ql' ) then
                 cloudefr(k,ii)=(efr_ql(ix ,iy ,k,itsig)*w00+efr_ql(ixp,iy ,k,itsig)*w10+ &
                                 efr_ql(ix ,iyp,k,itsig)*w01+efr_ql(ixp,iyp,k,itsig)*w11)*dtsig + &
                                (efr_ql(ix ,iy ,k,itsigp)*w00+efr_ql(ixp,iy ,k,itsigp)*w10+ &
                                 efr_ql(ix ,iyp,k,itsigp)*w01+efr_ql(ixp,iyp,k,itsigp)*w11)*dtsigp
               else if (trim(cloud_names(iii))== 'qi' ) then
                 cloudefr(k,ii)=(efr_qi(ix ,iy ,k,itsig)*w00+efr_qi(ixp,iy ,k,itsig)*w10+ &
                                 efr_qi(ix ,iyp,k,itsig)*w01+efr_qi(ixp,iyp,k,itsig)*w11)*dtsig + &
                                (efr_qi(ix ,iy ,k,itsigp)*w00+efr_qi(ixp,iy ,k,itsigp)*w10+ &
                                 efr_qi(ix ,iyp,k,itsigp)*w01+efr_qi(ixp,iyp,k,itsigp)*w11)*dtsigp
               else if (trim(cloud_names(iii))== 'qs' ) then
                 cloudefr(k,ii)=(efr_qs(ix ,iy ,k,itsig)*w00+efr_qs(ixp,iy ,k,itsig)*w10+ &
                                 efr_qs(ix ,iyp,k,itsig)*w01+efr_qs(ixp,iyp,k,itsig)*w11)*dtsig + &
                                (efr_qs(ix ,iy ,k,itsigp)*w00+efr_qs(ixp,iy ,k,itsigp)*w10+ &
                                 efr_qs(ix ,iyp,k,itsigp)*w01+efr_qs(ixp,iyp,k,itsigp)*w11)*dtsigp
               else if (trim(cloud_names(iii))== 'qg' ) then
                 cloudefr(k,ii)=(efr_qg(ix ,iy ,k,itsig)*w00+efr_qg(ixp,iy ,k,itsig)*w10+ &
                                 efr_qg(ix ,iyp,k,itsig)*w01+efr_qg(ixp,iyp,k,itsig)*w11)*dtsig + &
                                (efr_qg(ix ,iy ,k,itsigp)*w00+efr_qg(ixp,iy ,k,itsigp)*w10+ &
                                 efr_qg(ix ,iyp,k,itsigp)*w01+efr_qg(ixp,iyp,k,itsigp)*w11)*dtsigp
               else if (trim(cloud_names(iii))== 'qh' ) then
                 cloudefr(k,ii)=(efr_qh(ix ,iy ,k,itsig)*w00+efr_qh(ixp,iy ,k,itsig)*w10+ &
                                 efr_qh(ix ,iyp,k,itsig)*w01+efr_qh(ixp,iyp,k,itsig)*w11)*dtsig + &
                                (efr_qh(ix ,iy ,k,itsigp)*w00+efr_qh(ixp,iy ,k,itsigp)*w10+ &
                                 efr_qh(ix ,iyp,k,itsigp)*w01+efr_qh(ixp,iyp,k,itsigp)*w11)*dtsigp
               else  if (trim(cloud_names(iii))== 'qr' ) then
                 cloudefr(k,ii)=(efr_qr(ix ,iy ,k,itsig)*w00+efr_qr(ixp,iy ,k,itsig)*w10+ &
                                 efr_qr(ix ,iyp,k,itsig)*w01+efr_qr(ixp,iyp,k,itsig)*w11)*dtsig + &
                                (efr_qr(ix ,iy ,k,itsigp)*w00+efr_qr(ixp,iy ,k,itsigp)*w10+ &
                                 efr_qr(ix ,iyp,k,itsigp)*w01+efr_qr(ixp,iyp,k,itsigp)*w11)*dtsigp
               end if
            end if

        end do  
     endif ! <n_clouds>
  end do
! Interpolate level pressure to observation point for top interface
  prsi(nsig+1)=(ges_prsi(ix ,iy ,nsig+1,itsig )*w00+ &
              ges_prsi(ixp,iy ,nsig+1,itsig )*w10+ &
              ges_prsi(ix ,iyp,nsig+1,itsig )*w01+ &
              ges_prsi(ixp,iyp,nsig+1,itsig )*w11)*dtsig + &
             (ges_prsi(ix ,iy ,nsig+1,itsigp)*w00+ &
              ges_prsi(ixp,iy ,nsig+1,itsigp)*w10+ &
              ges_prsi(ix ,iyp,nsig+1,itsigp)*w01+ &
              ges_prsi(ixp,iyp,nsig+1,itsigp)*w11)*dtsigp

! if(any(prsl<zero)) call die(myname_,': negative pressure found',3)
! if(any(prsi<zero)) call die(myname_,': negative pressure found',4)

! Add additional crtm levels/layers to profile       

  call add_rtm_layers(prsi,prsl,prsi_rtm,prsl_rtm,klevel)
! if(any(prsi_rtm<zero)) call die(myname_,': negative pressure found',5)
! if(any(prsl_rtm<zero)) call die(myname_,': negative pressure found',5)

!       check trace gases
  if (n_ghg>0) then
    allocate (tgas1d(nsig,n_ghg))
    do ig=1,n_ghg
       if(size(gsi_chemguess_bundle)==1) then
          call gsi_bundlegetpointer(gsi_chemguess_bundle(1), ghg_names(ig),tgasges_itsig ,ier)
          do k=1,nsig
! choice:  use the internal interpolation function
!        or just explicitly code, not sure which one is efficient
!            tgas1d(k,ig) = crtm_interface_interp(tgasges_itsig(ix:ixp,iy:iyp,:),&
!                                                 w_weights, &
!                                                 1.0_r_kind)
             tgas1d(k,ig) =(tgasges_itsig(ix ,iy ,k)*w00+ &
                            tgasges_itsig(ixp,iy ,k)*w10+ &
                            tgasges_itsig(ix ,iyp,k)*w01+ &
                            tgasges_itsig(ixp,iyp,k)*w11)
          enddo
       else
          call gsi_bundlegetpointer(gsi_chemguess_bundle(itsig ),ghg_names(ig),tgasges_itsig ,ier)
          call gsi_bundlegetpointer(gsi_chemguess_bundle(itsigp),ghg_names(ig),tgasges_itsigp,ier)
          do k=1,nsig
!            tgas1d(k,ig) = crtm_interface_interp(tgasges_itsig(ix:ixp,iy:iyp,k),&
!                                                 w_weights, &
!                                                 dtsig) + &
!                           crtm_interface_interp(tgasges_itsigp(ix:ixp,iy:iyp,k),&
!                                                 w_weights, &
!                                                 dtsigp)
              

             tgas1d(k,ig) =(tgasges_itsig (ix ,iy ,k)*w00+ &
                           tgasges_itsig (ixp,iy ,k)*w10+ &
                           tgasges_itsig (ix ,iyp,k)*w01+ &
                           tgasges_itsig (ixp,iyp,k)*w11)*dtsig + &
                          (tgasges_itsigp(ix ,iy ,k)*w00+ &
                           tgasges_itsigp(ixp,iy ,k)*w10+ &
                           tgasges_itsigp(ix ,iyp,k)*w01+ &
                           tgasges_itsigp(ixp,iyp,k)*w11)*dtsigp
          enddo
       endif
    enddo
  endif

    
! Space-time interpolation of aerosol fields from sigma files

  if(n_aerosols>0)then
    if(size(gsi_chemguess_bundle)==1) then
       do ii=1,n_aerosols
          call gsi_bundlegetpointer(gsi_chemguess_bundle(1),aero_names(ii),aeroges_itsig ,ier) 
            do k=1,nsig
              aero(k,ii) =(aeroges_itsig(ix ,iy ,k)*w00+ &
                           aeroges_itsig(ixp,iy ,k)*w10+ &
                           aeroges_itsig(ix ,iyp,k)*w01+ &
                           aeroges_itsig(ixp,iyp,k)*w11)
            end do
       enddo
    else
       do ii=1,n_aerosols
          call gsi_bundlegetpointer(gsi_chemguess_bundle(itsig ),aero_names(ii),aeroges_itsig ,ier) 
          call gsi_bundlegetpointer(gsi_chemguess_bundle(itsigp),aero_names(ii),aeroges_itsigp,ier) 
            do k=1,nsig
              aero(k,ii) =(aeroges_itsig (ix ,iy ,k)*w00+ &
                           aeroges_itsig (ixp,iy ,k)*w10+ &
                           aeroges_itsig (ix ,iyp,k)*w01+ &
                           aeroges_itsig (ixp,iyp,k)*w11)*dtsig + &
                          (aeroges_itsigp(ix ,iy ,k)*w00+ &
                           aeroges_itsigp(ixp,iy ,k)*w10+ &
                           aeroges_itsigp(ix ,iyp,k)*w01+ &
                           aeroges_itsigp(ixp,iyp,k)*w11)*dtsigp
             end do
       enddo
    endif
    if(.not.lcf4crtm) then ! otherwise already calculated
       do k=1,nsig
           qs(k) =(gesqsat(ix ,iy ,k,itsig )*w00+ &
                   gesqsat(ixp,iy ,k,itsig )*w10+ &
                   gesqsat(ix ,iyp,k,itsig )*w01+ &
                   gesqsat(ixp,iyp,k,itsig )*w11)*dtsig + &
                  (gesqsat(ix ,iy ,k,itsigp)*w00+ &
                   gesqsat(ixp,iy ,k,itsigp)*w10+ &
                   gesqsat(ix ,iyp,k,itsigp)*w01+ &
                   gesqsat(ixp,iyp,k,itsigp)*w11)*dtsigp
       end do
    endif
    do k=1,nsig
        rh(k) = q(k)/qs(k)
    end do
  endif


! Find tropopause height at observation

  trop5= one_tenth*(tropprs(ix,iy )*w00+tropprs(ixp,iy )*w10+ &
                    tropprs(ix,iyp)*w01+tropprs(ixp,iyp)*w11)

!  Zero atmosphere jacobian structures

  call crtm_atmosphere_zero(atmosphere_k(:,:))
  call crtm_surface_zero(surface_k(:,:))

  clw_guess = zero

  if (n_aerosols>0) then
     do k = 1, nsig
!       Convert mixing-ratio to concentration
        ugkg_kgm2(k)=1.0e-9_r_kind*(prsi(k)-prsi(k+1))*r1000/grav
        aero(k,:)=aero(k,:)*ugkg_kgm2(k)
     enddo
  endif

  sea = min(max(zero,data_s(ifrac_sea)),one)  >= 0.99_r_kind
  icmask = sea

  do k = 1,msig

! Load profiles into extended RTM model layers

     kk = msig - k + 1
     atmosphere(1)%level_pressure(k) = r10*prsi_rtm(kk)
     atmosphere(1)%pressure(k)       = r10*prsl_rtm(kk)

     kk2 = klevel(kk)
     atmosphere(1)%temperature(k)    = h(kk2)
     if(lcf4crtm) then
        atmosphere(1)%absorber(k,1)  = r1000*qclr(kk2)*c3(kk2)
     else
        atmosphere(1)%absorber(k,1)  = r1000*q(kk2)*c3(kk2)
     endif
     if(iozs==0) then
        atmosphere(1)%absorber(k,2)  = poz(kk2)
     else
        atmosphere(1)%absorber(k,2)  = O3_ID
     endif
     if (n_ghg > 0) then
        do ig=1,n_ghg
           j=min_n_absorbers+ ig
           atmosphere(1)%absorber(k,j) = tgas1d(kk2,ig)
        enddo
     endif

     if (n_aerosols>0) then
        aero_conc(k,:)=aero(kk2,:)
        auxrh(k)      =rh(kk2)
     endif

! Include cloud guess profiles in mw radiance computation

     if (n_clouds>0) then
        kgkg_kgm2=(atmosphere(1)%level_pressure(k)-atmosphere(1)%level_pressure(k-1))*r100/grav
        if (lcw4crtm) then
          if (icmask) then 
              c6(k) = kgkg_kgm2
              auxdp(k)=abs(prsi_rtm(kk+1)-prsi_rtm(kk))*r10
              auxq (k)=q(kk2)

              if (regional .and. (.not. wrf_mass_regional) .and. (.not. cold_start)) then
                 do ii=1,n_clouds
                    cloud_cont(k,ii)=cloud(kk2,ii)*c6(k)
                    cloud_efr (k,ii)=cloudefr(kk2,ii)
                 end do
              else
                 do ii=1,n_clouds
                    cloud_cont(k,ii)=cloud(kk2,ii)*c6(k)
                 end do
              end if

              clw_guess = clw_guess +  cloud_cont(k,1)
              do ii=1,n_clouds
                 if (ii==1 .and. atmosphere(1)%temperature(k)-t0c>-20.0_r_kind) &
                    cloud_cont(k,1)=max(1.001_r_kind*1.0E-6_r_kind, cloud_cont(k,1))
                 if (ii==2 .and. atmosphere(1)%temperature(k)<t0c) &
                    cloud_cont(k,2)=max(1.001_r_kind*1.0E-6_r_kind, cloud_cont(k,2))
              end do

          endif   
        else 
           do ii=1,n_clouds
              cloud_cont(k,ii)=cloud(kk2,ii)*kgkg_kgm2
           end do
        endif
     endif

!    Add in a drop-off to absorber amount in the stratosphere to be in more
!    agreement with ECMWF profiles.  The drop-off is removed when climatological CO2 fields
!    are used.
     if(ico24crtm==0)then
        if (atmosphere(1)%level_pressure(k) < 200.0_r_kind) &
            atmosphere(1)%absorber(k,ico2) = atmosphere(1)%absorber(k,ico2) * &
           (0.977_r_kind + 0.000115_r_kind * atmosphere(1)%pressure(k))
     endif
  end do


! Set clouds for CRTM
  if(n_clouds>0) then
     atmosphere(1)%n_clouds = n_clouds  
     call Set_CRTM_Cloud (msig,n_actual_clouds,cloud_names,icmask,n_clouds,cloud_cont,cloud_efr,jcloud,auxdp, &
                          atmosphere(1)%temperature,atmosphere(1)%pressure,auxq,atmosphere(1)%cloud)
  endif

! Set aerosols for CRTM
  if(n_aerosols>0) then
     call Set_CRTM_Aerosol ( msig, n_aerosols, n_aerosols_crtm, aero_names, aero_conc, auxrh, &
                             atmosphere(1)%aerosol )
  endif

! Call CRTM K Matrix model


  do i=1,nchanl
     rtsolution_k(i,1)%layer_optical_depth(:) = jac_pert
  enddo

  error_status = 0
  if ( trim(obstype) /= 'modis_aod' ) then
     error_status = crtm_k_matrix(atmosphere,surface,rtsolution_k,&
        geometryinfo,channelinfo(sensorindex:sensorindex),atmosphere_k,&
        surface_k,rtsolution,options=options)
  else
     error_status = crtm_aod_k(atmosphere,rtsolution_k,&
        channelinfo(sensorindex:sensorindex),rtsolution,atmosphere_k)
  end if

! If the CRTM returns an error flag, do not assimilate any channels for this ob
! and set the QC flag to 10 (done in setuprad).

  if (error_status /=0) then
     write(6,*)myname_,':  ***ERROR*** during crtm_k_matrix call ',&
        error_status
  end if

! Calculate clear-sky Tb for AMSU-A over sea when allsky condition is on
  if (lcw4crtm .and. present(tsim_clr)) then
     if(n_clouds>0) then
        ! Zero out data array in cloud structure: water content, effective
        ! radius and variance

        atmosphere(1)%n_clouds = 0
!       call crtm_cloud_zero(atmosphere(1)%cloud)

        ! call crtm forward model for clear-sky calculation
        error_status = crtm_forward(atmosphere,surface,&
                                    geometryinfo,channelinfo(sensorindex:sensorindex),&
                                    rtsolution0,options=options)
        ! If the CRTM returns an error flag, do not assimilate any channels for this ob
        ! and set the QC flag to 10 (done in setuprad).
        if (error_status /=0) then
           write(6,*)'CRTM_FORWARD  ***ERROR*** during crtm_forward call ',&
           error_status
        end if
     endif 
  endif 

  if (trim(obstype) /= 'modis_aod' ) then
! Secant of satellite zenith angle

    secant_term = one/cos(data_s(ilzen_ang))

!$omp parallel do  schedule(dynamic,1) private(i) &
!$omp private(total_od,k,kk,m,term,ii,cwj)
    do i=1,nchanl
!   Zero jacobian and transmittance arrays
      do k=1,nsig
         omix(k,i)=zero
         temp(k,i)=zero
         ptau5(k,i)=zero
         wmix(k,i)=zero
       end do

!  Simulated brightness temperatures
       tsim(i)=rtsolution(i,1)%brightness_temperature

       if (lcw4crtm .and. present(tsim_clr)) &                          
       tsim_clr(i)=rtsolution0(i,1)%brightness_temperature  

!  Estimated emissivity
       emissivity(i)   = rtsolution(i,1)%surface_emissivity

!  Emissivity sensitivities
       emissivity_k(i) = rtsolution_k(i,1)%surface_emissivity

!  Surface temperature sensitivity
       if(nst_gsi>1 .and. (data_s(itz_tr) > zero .and. data_s(itz_tr) <= one) ) then
          ts(i)   = surface_k(i,1)%water_temperature*data_s(itz_tr) + &
                    surface_k(i,1)%land_temperature + &
                    surface_k(i,1)%ice_temperature + &
                    surface_k(i,1)%snow_temperature
       else
          ts(i)   = surface_k(i,1)%water_temperature + &
                    surface_k(i,1)%land_temperature + &
                    surface_k(i,1)%ice_temperature + &
                    surface_k(i,1)%snow_temperature
       endif
 

       if (abs(ts(i))<sqrt_tiny_r_kind) ts(i) = sign(sqrt_tiny_r_kind,ts(i))

!  Surface wind sensitivities
       if (surface(1)%wind_speed>small_wind) then
          term = surface_k(i,1)%wind_speed * f10*f10 / surface(1)%wind_speed
          uwind_k(i) = term * uu5
          vwind_k(i) = term * vv5
       else
          uwind_k(i)    = zero
          vwind_k(i)    = zero
       endif


       total_od = zero

!   Accumulate values from extended into model layers
!   temp  - temperature sensitivity
!   wmix  - moisture sensitivity
!   omix  - ozone sensitivity
!   ptau5 - layer transmittance
       do k=1,msig
          kk = klevel(msig-k+1)
          temp(kk,i) = temp(kk,i) + atmosphere_k(i,1)%temperature(k)
          wmix(kk,i) = wmix(kk,i) + atmosphere_k(i,1)%absorber(k,1)
          omix(kk,i) = omix(kk,i) + atmosphere_k(i,1)%absorber(k,2)
          total_od   = total_od + rtsolution(i,1)%layer_optical_depth(k)
          ptau5(kk,i) = exp(-min(limit_exp,total_od*secant_term))
       end do

!  Load jacobian array
       do k=1,nsig

!  Small sensitivities for temp
          if (abs(temp(k,i))<sqrt_tiny_r_kind) temp(k,i)=sign(sqrt_tiny_r_kind,temp(k,i))
       end do ! <nsig>

!  Deflate moisture jacobian above the tropopause.
       if (itv>=0) then
          do k=1,nsig
             jacobian(itv+k,i)=temp(k,i)*c2(k)               ! virtual temperature sensitivity
          end do ! <nsig>
       endif
       if (iqv>=0) then
          m=ich(i)
          do k=1,nsig
             jacobian(iqv+k,i)=c5(k)*wmix(k,i)-c4(k)*temp(k,i)        ! moisture sensitivity
             if (prsi(k) < trop5) then
                term = (prsi(k)-trop5)/(trop5-prsi(nsig))
                jacobian(iqv+k,i) = exp(ifactq(m)*term)*jacobian(iqv+k,i)
             endif
          end do ! <nsig>
       endif
       if (ioz>=0) then
!        if (.not. regional .or. use_gfs_ozone)then
          do k=1,nsig
             jacobian(ioz+k,i)=omix(k,i)*constoz       ! ozone sensitivity
          end do ! <nsig>
!        end if
       endif

       if (n_clouds>0) then
          if (icmask) then
             do ii=1,n_clouds_jac
               do k=1,nsig
                  cwj(k)=zero 
               end do
               do k=1,msig
                  kk = klevel(msig-k+1)
                  cwj(kk) = cwj(kk) + atmosphere_k(i,1)%cloud(ii)%water_content(k)*c6(k)
               end do
               do k=1,nsig
                  jacobian(icw(ii)+k,i) = cwj(k)
               end do ! <nsig>
             end do
          else
             do ii=1,n_clouds_jac
              do k=1,nsig
                jacobian(icw(ii)+k,i) = zero
              end do ! <nsig>
             end do
          endif
       endif


       if (ius>=0) then
           jacobian(ius+1,i)=uwind_k(i)         ! surface u wind sensitivity
       endif
       if (ivs>=0) then
           jacobian(ivs+1,i)=vwind_k(i)         ! surface v wind sensitivity
       endif
       if (isst>=0) then
           jacobian(isst+1,i)=ts(i)             ! surface skin temperature sensitivity
       endif
    end do

  else                                    !       obstype == 'modis_aod'
     ! initialize intent(out) variables that are not available with modis_aod
     tzbgr        = zero
     sfc_speed    = zero
     tsim         = zero
     emissivity   = zero
     ts           = zero
     emissivity_k = zero
     ptau5        = zero
     temp         = zero
     wmix         = zero
     jaero        = zero
     if(present(layer_od)) layer_od = zero
     if(present(jacobian_aero)) jacobian_aero = zero
     do i=1,nchanl
        do k=1,msig
           kk = klevel(msig-k+1)
           if(present(layer_od)) then
              layer_od(kk,i) = layer_od(kk,i) + rtsolution(i,1)%layer_optical_depth(k)
           endif
           do ii=1,n_aerosols_jac
              if ( n_aerosols_jac > n_aerosols_crtm .and. ii == indx_p25 ) then
                 jaero(kk,i,ii) = jaero(kk,i,ii) + &
                                  (0.5_r_kind*(0.78_r_kind*atmosphere_k(i,1)%aerosol(indx_dust1)%concentration(k) + &
                                               0.22_r_kind*atmosphere_k(i,1)%aerosol(indx_dust2)%concentration(k)) )
              else
                 jaero(kk,i,ii) = jaero(kk,i,ii) + atmosphere_k(i,1)%aerosol(ii)%concentration(k)
              endif
           enddo
        enddo
        if (present(jacobian_aero)) then
           do k=1,nsig
              do ii=1,n_aerosols_jac
                 jacobian_aero(iaero_jac(ii)+k,i) = jaero(k,i,ii)*ugkg_kgm2(k)
              end do
           enddo
        endif
     enddo
  endif
  if (n_ghg >0) deallocate (tgas1d)
! contains

!   pure function crtm_interface_interp(a,w,dtsig) result(intresult)
!     real(r_kind), intent(in) :: a(:,:)
!     real(r_kind), intent(in) :: w(:,:)
!     real(r_kind), intent(in) :: dtsig
!     real(r_kind) :: intresult
!     integer :: i, j, n
!     n = size(a,dim=1)
!     intresult = 0.0_r_kind
!     do j = 1, n
!       do i = 1, n
!         intresult = intresult + a(i,j)*w(i,j)
!       enddo
!     enddo
!     intresult = intresult * dtsig
!   end function crtm_interface_interp
  end subroutine call_crtm
subroutine get_lai(data_s,nchanl,nreal,itime,ilate,lai_type,lai)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_lai   interpolate vegetation LAI data for call_crtm
!
!   prgmmr:
!
! abstract:
!
! program history log:
!
!   input argument list:
!     data_s       - array containing input data information
!     nchanl       - number of channels
!     nreal        - number of descriptor information in data_s
!     itime        - index of analysis relative obs time
!     ilate        - index of earth relative latitude (degrees)
!
!   output argument list:
!     lai          - interpolated vegetation leaf-area-index for various types (13)
!
!   language: f90
!   machine:  ibm RS/6000 SP
!   
!$$$
!--------
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use obsmod, only: iadate
  implicit none

! Declare passed variables
  integer(i_kind)                       ,intent(in   ) :: nchanl,nreal
  real(r_kind),dimension(nchanl+nreal)  ,intent(in   ) :: data_s
  integer(i_kind)                       ,intent(in   ) :: itime, ilate,lai_type
  real(r_kind)                          ,intent(  out) :: lai

! Declare local variables
  integer(i_kind),dimension(8)::obs_time,anal_time
  real(r_kind),dimension(5)     :: tmp_time
  
  integer(i_kind) jdow, jdoy, jday
  real(r_kind)    rjday
  real(r_kind),dimension(3):: dayhf
  data dayhf/15.5_r_kind, 196.5_r_kind, 380.5_r_kind/
  real(r_kind),dimension(13):: lai_min, lai_max
  data lai_min/3.08_r_kind, 1.85_r_kind, 2.80_r_kind, 5.00_r_kind, 1.00_r_kind, &
               0.50_r_kind, 0.52_r_kind, 0.60_r_kind, 0.50_r_kind, 0.60_r_kind, &
               0.10_r_kind, 1.56_r_kind, 0.01_r_kind            /
  data lai_max/6.48_r_kind, 3.31_r_kind, 5.50_r_kind, 6.40_r_kind, 5.16_r_kind, &
               3.66_r_kind, 2.90_r_kind, 2.60_r_kind, 3.66_r_kind, 2.60_r_kind, &
               0.75_r_kind, 5.68_r_kind, 0.01_r_kind            /
  real(r_kind),dimension(2):: lai_season
  real(r_kind)    wei1s, wei2s
  integer(i_kind) n1, n2, mm, mmm, mmp
!
      anal_time=0
      obs_time=0
      tmp_time=zero
      tmp_time(2)=data_s(itime)
      anal_time(1)=iadate(1)
      anal_time(2)=iadate(2)
      anal_time(3)=iadate(3)
      anal_time(5)=iadate(4)
      call w3movdat(tmp_time,anal_time,obs_time)

      jdow = 0
      jdoy = 0
      jday = 0
      call w3doxdat(obs_time,jdow,jdoy,jday)
      rjday=jdoy+obs_time(5)/24.0_r_kind
      if(rjday.lt.dayhf(1)) rjday=rjday+365.0

      DO MM=1,2
        MMM=MM
        MMP=MM+1
        IF(RJDAY.GE.DAYHF(MMM).AND.RJDAY.LT.DAYHF(MMP)) THEN
            N1=MMM
            N2=MMP
            GO TO 10
        ENDIF
      ENDDO
      PRINT *,'WRONG RJDAY',RJDAY
   10 CONTINUE
      WEI1S = (DAYHF(N2)-RJDAY)/(DAYHF(N2)-DAYHF(N1))
      WEI2S = (RJDAY-DAYHF(N1))/(DAYHF(N2)-DAYHF(N1))
      IF(N2.EQ.3) N2=1

      lai_season(1) = lai_min(lai_type)
      lai_season(2) = lai_max(lai_type)
      if(data_s(ilate) < 0.0_r_kind) then
         lai = wei1s * lai_season(n2) + wei2s * lai_season(n1)
      else
         lai = wei1s * lai_season(n1) + wei2s * lai_season(n2)
      endif

  return
  end subroutine get_lai

  end module crtm_interface
