!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  guess_grids --- Guess-related grid definitions
!
! !INTERFACE:
!

module guess_grids

! !USES:
 
  use kinds, only: r_single,r_kind,i_kind
  use constants, only: max_varname_length

  use gsi_bundlemod, only : gsi_bundlegetpointer

  ! meteorological guess (beyond standard ones)
  use gsi_metguess_mod, only: gsi_metguess_create_grids
  use gsi_metguess_mod, only: gsi_metguess_destroy_grids
  use gsi_metguess_mod, only: gsi_metguess_get
  use gsi_metguess_mod, only: gsi_metguess_bundle

  ! chem trace gases
  use gsi_chemguess_mod, only: gsi_chemguess_create_grids
  use gsi_chemguess_mod, only: gsi_chemguess_destroy_grids
  use gsi_chemguess_mod, only: gsi_chemguess_get

  ! derivatives
  use derivsmod, only: create_ges_derivatives
  use derivsmod, only: destroy_ges_derivatives

  ! tendencies
  use tendsmod, only: create_ges_tendencies
  use tendsmod, only: destroy_ges_tendencies

  use mpeu_util, only: die,tell
  implicit none

! !DESCRIPTION: module containing variables related to the guess fields
!
! !REVISION HISTORY:
!
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2005-06-01  treadon - add routine add_rtm_layers
!   2005-06-03  parrish - add horizontal derivatives of guess fields
!   2005-06-10  devenyi/treadon - initialize nfldsig and nfldsfc
!   2005-08-03  parrish - add array to hold roughness length
!   2005-09-29  kleist - add derivatives of terrain, move prsi allocation
!   2005-11-21  kleist - add tendency arrays
!   2005-11-29  derber - add ozmz remove psfcg
!   2005-11-30  derber - combine create_atm_grids and create_pcp_grids (and destroys)
!   2006-02-02  treadon - prefix prsi,prsl,lnprsl,prslk with "ges_"
!   2006-03-07  treadon - remove ges_prslk (no longer needed)
!   2006-04-14  treadon - add bias_tskin
!   2006-04-17  treadon - add ges_psfcavg and ges_prslavg
!   2006-04-21  kleist - add ges divt and agvt arrays
!   2006-07-28  derber  - clean up add_rtm_layers routine 
!   2006-07-28  derber  - add ges_tsen (sensible temperature) array
!   2006-07-31  kleist - use ges_ps instead of ln(ps)
!   2006-09-20  cucurull - add ln(ges_prsi) array
!   2006-09-29  treadon - add flags to control 10m wind factor recompute
!   2007-05-30  h.liu - remove ozmz
!   2007-06-21  rancic - add pbl (ges_teta)
!   2006-12-01  todling - remove bias stuff; merging GMAO bias correction scheme
!   2006-12-15  todling - add _initialized parameters to control allocations
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes 
!   2008-02-07  eliu    - fixed the unit difference between prsitmp
!                         (kPa) and toa_pressure (hPa).
!   2009-08-19  guo     - added sfc_grids_allocated_, ges_grids_allocated_,
!			  and gesfinfo_created_ to track the state of the data.
!			  for multi-pass observer.
!			- merged destroy_sfc_grids() and destroy_sfct().
!   2008-08-25  hu    - add array definitions for hydrometeor fields
!                     - add subroutine create_cld_grids and destroy_cld_grids
!   2010-04-16  hou   - add array definitions ges_co2 (co2 mixing ratio) and
!                       control variable igfsco2
!   2010-04-22  todling - remove tracers,vtid,pdryini,xncld
!   2010-05-19  todling - add chem init and destroy (revamp Hou's implementation)
!   2010-08-31  cucurull - add logical use_compress
!   2010-09-15  pagowski - add cmaq
!   2010-12-20  cucurull - add integer nsig_ext 
!   2011-01-05  cucurull - add real gpstop
!   2011-02-11  zhu      - add ges_gust,ges_vis,ges_pblh
!   2011-03-13  li      - add for nst FCST file
!   2011-04-29  todling  - some of cloud fields move to wrf_guess_mod; some to met_guess
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2011-11-01  eliu    - modified condition to allocate/deallocate arrays related to 
!                         cloud water tendencies and derivatives 
!   2011-12-27  kleist  - add 4d guess array for saturation specific humidity
!   2012-01-11  Hu      - add GSD PBL height
!   2013-02-22  Carley  - Add NMMB to GSD PBL height calc
!   2013-10-19  todling - metguess now holds background
!                         all tendencies now in a bundle (see tendsmod)
!                         all derivaties now in a bundle (see derivsmod)
!   2015-01-15  Hu      - Add coast_prox to hold coast proximity
!   2017-05-12  Y. Wang and X. Wang - add bottom and top levels of w and rho for
!                                     radar DA later, POC: xuguang.wang@ou.edu
!   2017-10-10  wu      - Add code for fv3_regional 
!   2019-03-21  Wei/Martin - add code for external aerosol file input
!   2019-09-10  martin  - added new fields to save guess tsen/geop_hgt for writing increment
!
! !AUTHOR: 
!   kleist           org: np20                date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------

! set default to private
  private
! set subroutines to public
  public :: create_sfc_grids
  public :: create_ges_grids
  public :: destroy_ges_grids
  public :: destroy_sfc_grids
  public :: create_gesfinfo
  public :: destroy_gesfinfo
  public :: load_prsges
  public :: load_geop_hgt
  public :: load_gsdpbl_hgt
  public :: add_rtm_layers
  public :: load_fact10
  public :: comp_fact10
  public :: guess_grids_print
  public :: guess_grids_stats
  public :: create_metguess_grids
  public :: destroy_metguess_grids
  public :: create_chemges_grids
  public :: destroy_chemges_grids
  public :: get_ref_gesprs
! set passed variables to public
  public :: ntguessig,ges_prsi,ges_psfcavg,ges_prslavg
  public :: isli2,ges_prsl,nfldsig
  public :: ges_teta
  public :: fact_tv,tropprs,sfct
  public :: ntguessfc,ntguesnst,dsfct,ifilesig,veg_frac,soil_type,veg_type
  public :: sno2,ifilesfc,ifilenst,sfc_rough,fact10,sno,isli,soil_temp,soil_moi,coast_prox 
  public :: nfldsfc,nfldnst,hrdifsig,ges_tsen,sfcmod_mm5,sfcmod_gfs,ifact10,hrdifsfc,hrdifnst
  public :: geop_hgti,ges_lnprsi,ges_lnprsl,geop_hgtl,pbl_height,ges_geopi
  public :: geom_hgti,geom_hgti_bg
  public :: wgt_lcbas
  public :: ges_qsat
  public :: use_compress,nsig_ext,gpstop,commgpstop,commgpserrinf
  public :: ges_tsen1,ges_q1
  public :: ntguesaer,ifileaer,nfldaer,hrdifaer ! variables for external aerosol files

  public :: ges_initialized

  public :: nfldsig_all,nfldsig_now,hrdifsig_all
  public :: nfldsfc_all,nfldsfc_now,hrdifsfc_all
  public :: nfldnst_all,nfldnst_now,hrdifnst_all
  public :: nfldaer_all,nfldaer_now,hrdifaer_all ! variables for external aerosol files
  public :: extrap_intime
  public :: ntguessig_ref
  public :: ntguessfc_ref
  public :: ntguesnst_ref
  public :: ntguesaer_ref

  public :: ges_w_btlev
  public :: ges_rho

  logical:: sfcmod_gfs = .false.    ! .true. = recompute 10m wind factor using gfs physics
  logical:: sfcmod_mm5 = .false.    ! .true. = recompute 10m wind factor using mm5 physics

  logical:: use_compress = .false. ! true to turn on compressibility factor in geopotential heights

  logical, save :: ges_initialized = .false.

  integer(i_kind) ntguessig         ! location of actual guess time for sigma fields
  integer(i_kind) ntguessfc         ! location of actual guess time for sfc fields
  integer(i_kind) ntguesnst         ! location of actual guess time for nst FCST fields
  integer(i_kind) ntguesaer         ! location of actual guess time for aer FCST fields

  integer(i_kind), save:: ntguessig_ref	! replace ntguessig as the storage for its original value
  integer(i_kind), save:: ntguessfc_ref	! replace ntguessfc as the storage for its original value
  integer(i_kind), save:: ntguesnst_ref ! replace ntguesnst as the storage for its original value
  integer(i_kind), save:: ntguesaer_ref ! replace ntguesaer as the storage for its original value

  integer(i_kind):: ifact10 = 0     ! 0 = use 10m wind factor from guess
  integer(i_kind):: nsig_ext = 13   ! use 13 layers above model top to compute the bending angle for gpsro

  ! number of guess sigma/surface times are set in GSI_gridComp.rc

  real(r_kind), allocatable, dimension(:), save:: hrdifsig_all  ! a list of all times
  real(r_kind), allocatable, dimension(:), save:: hrdifsfc_all  ! a list of all times
  real(r_kind), allocatable, dimension(:), save:: hrdifnst_all  ! a list of all times
  real(r_kind), allocatable, dimension(:), save:: hrdifaer_all  ! a list of all times

  integer(i_kind), save:: nfldsig_all	! expected total count of time slots
  integer(i_kind), save:: nfldsfc_all
  integer(i_kind), save:: nfldnst_all

  integer(i_kind), save:: nfldsig	! actual count of in-cache time slots
  integer(i_kind), save:: nfldsfc
  integer(i_kind), save:: nfldnst       ! actual count of in-cache time slots for NST file

  integer(i_kind), save:: nfldsig_now	! current count of filled time slots
  integer(i_kind), save:: nfldsfc_now
  integer(i_kind), save:: nfldnst_now

! variables for external aerosol files
  integer(i_kind), save:: nfldaer_all
  integer(i_kind), save:: nfldaer       ! actual count of in-cache time slots for AER file
  integer(i_kind), save:: nfldaer_now

  logical, save:: extrap_intime		! compute o-f interpolate within the time ranges of guess_grids,
  					! or also extrapolate outside the time ranges.

  real(r_kind), allocatable, dimension(:):: hrdifsig  ! times for cached sigma guess_grid
  real(r_kind), allocatable, dimension(:):: hrdifsfc  ! times for cached surface guess_grid
  real(r_kind), allocatable, dimension(:):: hrdifnst  ! times for cached nst guess_grid
  real(r_kind), allocatable, dimension(:):: hrdifaer  ! times for cached aer guess_grid

  integer(i_kind),allocatable, dimension(:)::ifilesfc  ! array used to open the correct surface guess files
  integer(i_kind),allocatable, dimension(:)::ifilesig  ! array used to open the correct sigma guess files
  integer(i_kind),allocatable, dimension(:)::ifilenst  ! array used to open the correct nst guess files
  integer(i_kind),allocatable, dimension(:)::ifileaer  ! array used to open the correct aer guess files

  integer(i_kind),allocatable,dimension(:,:,:):: isli    ! snow/land/ice mask
  integer(i_kind),allocatable,dimension(:,:,:):: isli_g  ! isli on horiz/global grid
  integer(i_kind),allocatable,dimension(:,:):: isli2     ! snow/land/ice mask at analysis time
  real(r_kind),allocatable,dimension(:,:):: coast_prox   ! coast proximity mask

  real(r_kind),allocatable,dimension(:,:,:):: sno2  ! sno depth on subdomain


  real(r_kind):: gpstop=30.0_r_kind   ! maximum gpsro height used in km 
                                      ! geometric height for ref, impact height for bnd
  real(r_kind):: commgpstop=30.0_r_kind
  real(r_kind):: commgpserrinf=1.0_r_kind ! error inflation factor for commercial gnssro
  real(r_kind):: ges_psfcavg                            ! average guess surface pressure 
  real(r_kind),allocatable,dimension(:):: ges_prslavg   ! average guess pressure profile

  real(r_kind),allocatable,dimension(:,:):: tropprs     ! guess tropopause pressure
  real(r_kind),allocatable,dimension(:,:,:):: dsfct     ! delta skin temperature

  real(r_kind),allocatable,dimension(:,:,:):: fact10    ! 10 meter wind factor
  real(r_kind),allocatable,dimension(:,:,:):: sfct      ! guess skin temperature
  real(r_kind),allocatable,dimension(:,:,:):: sno       ! snow-ice mask
  real(r_kind),allocatable,dimension(:,:,:):: veg_type  ! vegetation type
  real(r_kind),allocatable,dimension(:,:,:):: veg_frac  ! vegetation fraction(0-1.0)
  real(r_kind),allocatable,dimension(:,:,:):: sfc_rough ! sfc roughness length
  real(r_kind),allocatable,dimension(:,:,:):: soil_type ! soil type
  real(r_kind),allocatable,dimension(:,:,:):: soil_temp ! soil temperature of first layer
  real(r_kind),allocatable,dimension(:,:,:):: soil_moi  ! soil moisture of first layer
  

  real(r_kind),allocatable,dimension(:,:,:,:):: geop_hgtl ! guess geopotential height at mid-layers
  real(r_kind),allocatable,dimension(:,:,:,:):: geop_hgti ! guess geopotential height at level interfaces

  real(r_kind),allocatable,dimension(:,:,:,:):: geom_hgti ! guess geometricheight at level interfaces
  real(r_kind),allocatable,dimension(:,:,:,:):: geom_hgti_bg ! guess geometricheight at level interface for the background

  real(r_kind),allocatable,dimension(:,:,:,:):: ges_geopi ! input guess geopotential height at level interfaces

  real(r_kind),allocatable,dimension(:,:,:):: pbl_height  !  GSD PBL height in hPa
                                                          ! Guess Fields ...
  real(r_kind),allocatable,dimension(:,:):: wgt_lcbas     ! weight given to base height of lowest cloud seen
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsi  ! interface pressure
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_prsl  ! layer midpoint pressure
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_lnprsl! log(layer midpoint pressure)
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_lnprsi! log(interface pressure)
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_tsen  ! sensible temperature
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_tsen1  ! to save the first guess for increment
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_q1    ! to save the first guess q for increment
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_teta  ! potential temperature

  real(r_kind),allocatable,dimension(:,:,:):: fact_tv      ! 1./(one+fv*ges_q) for virt to sen calc.
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_qsat   ! 4d qsat array

  real(r_kind),allocatable,dimension(:,:,:,:):: ges_w_btlev
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_rho

  interface guess_grids_print
     module procedure print1r8_
     module procedure print2r8_
     module procedure print3r8_
     module procedure print4r8_
  end interface
  interface guess_grids_stats
     module procedure guess_grids_stats3d_
     module procedure guess_grids_stats2d_
  end interface


  logical,save:: sfc_grids_allocated_=.false.
  logical,save:: ges_grids_allocated_=.false.
  logical,save:: gesfinfo_created_=.false.

  character(len=*),parameter::myname='guess_grids'

contains

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: create_sfc_grids --- Allocate memory for surface related grids
!
! !INTERFACE:
!
  subroutine create_sfc_grids

! !USES:

   use gridmod, only: lat2,lon2,nlat,nlon
   use constants, only: zero

   implicit none

! !DESCRIPTION: allocate memory for surface related grids
!
! !REVISION HISTORY:
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-06-03  parrish - allocate and initialize sfct_lat and sfct_lon
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2008-12-5   todling - add time dimension to dsfct
!   2009-01-23  todling - zero out arrays
!   2012-03-06  akella  - add call to initialize arrays for NST analysis
!   2017-08-31  li      - move gsi_nstcoupler_init & gsi_nstcoupler_final to
!                         satthin.F90 and read_obs.F90 respectivaly
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist          org: w/nmc20     date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) :: i,j,it,istatus
    if(sfc_grids_allocated_) call die('create_sfc_grids','alread allocated')
    sfc_grids_allocated_=.true.

    allocate( isli_g(nlat,nlon,nfldsfc),&
         isli2(lat2,lon2),sno2(lat2,lon2,nfldsfc),&
         stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_SFC_GRIDS(1):  allocate error, istatus=',&
         istatus,lat2,lon2,nlat,nlon,nfldsfc

#ifndef HAVE_ESMF
    allocate( isli(lat2,lon2,nfldsfc),fact10(lat2,lon2,nfldsfc),&
         dsfct(lat2,lon2,nfldsfc),sfct(lat2,lon2,nfldsfc),sno(lat2,lon2,nfldsfc),&
         veg_type(lat2,lon2,nfldsfc),veg_frac(lat2,lon2,nfldsfc),&
         sfc_rough(lat2,lon2,nfldsfc),&
         soil_type(lat2,lon2,nfldsfc),soil_temp(lat2,lon2,nfldsfc),&
         soil_moi(lat2,lon2,nfldsfc), coast_prox(lat2,lon2),&
         stat=istatus)
    if (istatus/=0) write(6,*)'CREATE_SFC_GRIDS(2):  allocate error, istatus=',&
         istatus,lat2,lon2,nlat,nlon,nfldsfc
#endif /* HAVE_ESMF */

    do it=1,nfldsfc
       do j=1,nlon
          do i=1,nlat
             isli_g(i,j,it)=0
          end do
       end do
    end do

#ifndef HAVE_ESMF
    do it=1,nfldsfc
       do j=1,lon2
          do i=1,lat2
             isli(i,j,it)=0
             coast_prox(i,j)=zero
             fact10(i,j,it)=zero
             sfct(i,j,it)=zero
             dsfct(i,j,it)=zero
             sno(i,j,it)=zero
             veg_type(i,j,it)=zero
             veg_frac(i,j,it)=zero
             soil_type(i,j,it)=zero
             soil_temp(i,j,it)=zero
             soil_moi(i,j,it)=zero
          end do
       end do
    end do
#endif

    do it=1,nfldsfc
       do j=1,lon2
          do i=1,lat2
             sno2(i,j,it)=zero
          end do
       end do
    end do

    do j=1,lon2
       do i=1,lat2
          isli2(i,j)=0
       end do
    end do

    return
  end subroutine create_sfc_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  create_ges_grids --- Alloc grid for guess and bias
!
! !INTERFACE:
!
  subroutine create_ges_grids(switch_on_derivatives,tendsflag)

! !USES:

    use wrf_vars_mod, only : w_exist
    use constants,only: zero,one
    use gridmod, only: lat2,lon2,nsig
    use gridmod,only: l_reg_update_hydro_delz
    implicit none

! !INPUT PARAMETERS:

    logical,intent(in   ) :: switch_on_derivatives    ! for for horizontal derivatives
    logical,intent(in   ) :: tendsflag                ! for time tendencies


! !OUTPUT PARAMETERS:

! !DESCRIPTION: allocate grids to hold guess and bias correction fields
!
! !REVISION HISTORY:
!   2004-06-03  treadon, original code
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-06-03  parrish - allocate/initialize _lat,_lon derivatives for u,v,cwmr,oz
!   2005-06-08  treadon - pass switch_on_derivatives via argument list
!   2005-07-27  kleist  - modified to include some shared arrays
!   2006-01-10  treadon - remove mype from calling list (not used)
!   2006-07-31  kleist  - use ges_ps arrays instead of ln(ps)
!   2006-12-04  todling - remove bias initialization; rename routine
!   2006-12-15  todling - protection to allow initializing ges/tnd/drv at will
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2011-02-09  zhu     - add ges_gust,ges_vis,ges_pblh
!   2012-05-14  todling - revisit cw check to check also on some hydrometeors
!   2013-10-19  todling - revisit initialization of certain vars wrt ESMF
!   2014-06-09  carley/zhu - add wgt_lcbas
!   2019-03-21  Wei/Martin - add capability to read external aerosol file
!   2019-09-10  martin  - added new fields to save guess tsen/geop_hgt for writing increment
!   2021-01-05  x.zhang/lei  - add code for updating delz analysis in regional da
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20     date: 2004-06-03
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) i,j,k,n,istatus
    if(ges_grids_allocated_) call die('create_ges_grids','already allocated')
    ges_grids_allocated_=.true.

    if ( .not. ges_initialized ) then

#ifndef HAVE_ESMF
       nfldsig_all=nfldsig
       nfldsfc_all=nfldsfc
       nfldnst_all=nfldnst
       nfldsig_now=0 ! _now variables are not used if not for ESMF
       nfldsfc_now=0
       nfldnst_now=0
       nfldaer_all=nfldaer
       nfldaer_now=0
       extrap_intime=.true.
#endif /* HAVE_ESMF */

!      Allocate and zero guess grids
       allocate ( ges_prsi(lat2,lon2,nsig+1,nfldsig),ges_prsl(lat2,lon2,nsig,nfldsig),&
            ges_lnprsl(lat2,lon2,nsig,nfldsig),ges_lnprsi(lat2,lon2,nsig+1,nfldsig),&
            ges_tsen(lat2,lon2,nsig,nfldsig),&
            ges_tsen1(lat2,lon2,nsig,nfldsig),&
            ges_q1(lat2,lon2,nsig,nfldsig),&
            ges_teta(lat2,lon2,nsig,nfldsig),&
            ges_rho(lat2,lon2,nsig,nfldsig), &  
            geop_hgtl(lat2,lon2,nsig,nfldsig), &
            geop_hgti(lat2,lon2,nsig+1,nfldsig),ges_prslavg(nsig),&
            ges_geopi(lat2,lon2,nsig+1,nfldsig),&
            tropprs(lat2,lon2),fact_tv(lat2,lon2,nsig),&
            pbl_height(lat2,lon2,nfldsig),wgt_lcbas(lat2,lon2), &
            ges_qsat(lat2,lon2,nsig,nfldsig),stat=istatus)
         
      if(l_reg_update_hydro_delz) then
         allocate( geom_hgti(lat2,lon2,nsig+1,nfldsig))
         allocate( geom_hgti_bg(lat2,lon2,nsig+1,nfldsig))
       endif

       if(w_exist)then
         allocate(ges_w_btlev(lat2,lon2,2,nfldsig),stat=istatus)
       endif

       if (istatus/=0) write(6,*)'CREATE_GES_GRIDS(ges_prsi,..):  allocate error, istatus=',&
            istatus,lat2,lon2,nsig,nfldsig

       ges_initialized = .true.

!  Default for ges_psfcavg
       ges_psfcavg=zero
       do i=1,nsig
          ges_prslavg(i)=zero
       end do

       do j=1,lon2
          do i=1,lat2
             tropprs(i,j)=zero
          end do
       end do

       do k=1,nsig
          do j=1,lon2
             do i=1,lat2
                fact_tv(i,j,k)=one
             end do
          end do
       end do

       do n=1,nfldsig
          do j=1,lon2
             do i=1,lat2
                pbl_height(i,j,n)=zero
             end do
          end do
       end do
       do n=1,nfldsig
          do k=1,nsig
             do j=1,lon2
                do i=1,lat2
                   ges_prsl(i,j,k,n)=zero
                   ges_lnprsl(i,j,k,n)=zero
                   ges_rho(i,j,k,n)=zero
                   ges_qsat(i,j,k,n)=zero
                   ges_tsen(i,j,k,n)=zero
                   ges_tsen1(i,j,k,n)=zero
                   ges_q1(i,j,k,n)=zero
                   ges_teta(i,j,k,n)=zero
                   geop_hgtl(i,j,k,n)=zero
                end do
             end do
          end do
          do k=1,nsig+1
             do j=1,lon2
                do i=1,lat2
                   ges_prsi(i,j,k,n)=zero
                   ges_lnprsi(i,j,k,n)=zero
                   geop_hgti(i,j,k,n)=zero
                   ges_geopi(i,j,k,n)=zero
                end do
             end do
          end do
       end do

       do j=1,lon2
          do i=1,lat2
             wgt_lcbas(i,j)=0.01_r_kind
          end do
       end do

       if(w_exist) then
         ges_w_btlev=zero
       endif

    end if ! ges_initialized
    
!   If tendencies option on, allocate/initialize _ten arrays to zero
    call create_ges_tendencies(tendsflag)

    call create_ges_derivatives(switch_on_derivatives,nfldsig)

    return
  end subroutine create_ges_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: create_metguess_grids --- initialize meterological guess
!
! !INTERFACE:
!
  subroutine create_metguess_grids(mype,istatus)

! !USES:
  use gridmod, only: lat2,lon2,nsig
  implicit none

! !INPUT PARAMETERS:

  integer(i_kind), intent(in)  :: mype

! !OUTPUT PARAMETERS:

  integer(i_kind), intent(out) :: istatus

! !DESCRIPTION: initialize meteorological background fields beyond 
!               the standard ones - wired-in this module.
!
! !REVISION HISTORY:
!   2011-04-29  todling
!   2013-10-30  todling - update interface
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; Linux Cluster
!
! !AUTHOR: 
!   todling         org: w/nmc20     date: 2011-04-29
!
!EOP
!-------------------------------------------------------------------------
   character(len=*),parameter::myname_=myname//'*create_metguess_grids'
   integer(i_kind) :: nmguess                   ! number of meteorol. fields (namelist)
   character(len=max_varname_length),allocatable:: mguess(:)   ! names of meterol. fields

   istatus=0
  
!  When proper connection to ESMF is complete,
!  the following will not be needed here
!  ------------------------------------------
   call gsi_metguess_get('dim',nmguess,istatus)
   if(istatus/=0) then
      if(mype==0) write(6,*) myname_, ': trouble getting number of met-guess fields'
      return
   endif
   if(nmguess==0) return
   if (nmguess>0) then
       allocate (mguess(nmguess))
       call gsi_metguess_get('gsinames',mguess,istatus)
       if(istatus/=0) then
          if(mype==0) write(6,*) myname_, ': trouble getting name of met-guess fields'
          return
       endif

!      Allocate memory for guess fields
!      --------------------------------
       call gsi_metguess_create_grids(lat2,lon2,nsig,nfldsig,istatus)
       if(istatus/=0) then
          if(mype==0) write(6,*) myname_, ': trouble allocating mem for met-guess'
          return
       endif
   endif

  end subroutine create_metguess_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_metguess_grids --- destroy meterological background
!
! !INTERFACE:
!
  subroutine destroy_metguess_grids(mype,istatus)
! !USES:
  implicit none
! !INPUT PARAMETERS:
  integer(i_kind),intent(in)::mype
! !OUTPUT PARAMETERS:
  integer(i_kind),intent(out)::istatus
! !DESCRIPTION: destroy meterological background
!
! !REVISION HISTORY:
!   2011-04-29  todling
!   2013-10-30  todling - update interface
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; Linux Cluster
!
! !AUTHOR: 
!   todling         org: w/nmc20     date: 2011-04-29
!
!EOP
  character(len=*),parameter::myname_=myname//'destroy_metguess_grids'
  istatus=0
  call gsi_metguess_destroy_grids(istatus)
       if(istatus/=0) then
          if(mype==0) write(6,*) myname_, ': trouble deallocating mem for met-guess'
          return
       endif
  end subroutine destroy_metguess_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: create_chemges_grids --- initialize chem component
!
! !INTERFACE:
!
  subroutine create_chemges_grids(mype,istatus)

! !USES:
  use gridmod, only: lat2,lon2,nsig
  implicit none

! !INPUT PARAMETERS:

  integer(i_kind), intent(in) :: mype

! !OUTPUT PARAMETERS:

  integer(i_kind), intent(out) :: istatus

! !DESCRIPTION: initialize chem background
!
! !REVISION HISTORY:
!   2010-05-19  todling
!   2013-10-30  todling - update interface
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; Linux Cluster
!
! !AUTHOR: 
!   todling         org: w/nmc20     date: 2010-05-19
!
!EOP
!-------------------------------------------------------------------------
  character(len=*),parameter::myname_=myname//'*create_chemges_grids'
   integer(i_kind) :: ntgases                   ! number of tracer gases (namelist)
   character(len=max_varname_length),allocatable:: tgases(:)   ! names of tracer gases

  istatus=0
  
!  When proper connection to ESMF is complete,
!  the following will not be needed here
!  ------------------------------------------
   call gsi_chemguess_get('dim',ntgases,istatus)
   if(istatus/=0) then
      if(mype==0) write(6,*) myname_, ': trouble getting number of chem/gases'
      return
   endif
   if(ntgases==0) return
   if (ntgases>0) then
      allocate (tgases(ntgases))
      call gsi_chemguess_get('gsinames',tgases,istatus)
      if(istatus/=0) then
         if(mype==0) write(6,*) myname_, ': trouble getting name of chem/gases'
         return
      endif

!     Allocate memory for guess files for trace gases
!     ------------------------------------------------
      call gsi_chemguess_create_grids(lat2,lon2,nsig,nfldsig,istatus)
      if(istatus/=0) then
         if(mype==0) write(6,*) myname_, ': trouble allocating mem for chem/gases'
         return
      endif
   endif

  end subroutine create_chemges_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_chemges_grids --- destroy chem background
!
! !INTERFACE:
!
  subroutine destroy_chemges_grids(istatus)
! !USES:
  implicit none
! !INPUT PARAMETERS:
! !OUTPUT PARAMETERS:
  integer(i_kind),intent(out)::istatus
! !DESCRIPTION: destroy chem background
!
! !REVISION HISTORY:
!   2010-05-19  todling
!   2013-10-30  todling - update interface
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; Linux Cluster
!
! !AUTHOR: 
!   todling         org: w/nmc20     date: 2010-05-19
!
!EOP
  istatus=0
  call gsi_chemguess_destroy_grids(istatus)
  end subroutine destroy_chemges_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_ges_grids --- Dealloc guess and bias fields
!
! !INTERFACE:
!
  subroutine destroy_ges_grids

! !USES:
    use wrf_vars_mod, only : w_exist
    use gridmod,only: l_reg_update_hydro_delz

    implicit none

! !INPUT PARAMETERS:
    
! !DESCRIPTION: deallocate guess and bias grids
!
! !REVISION HISTORY:
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2005-06-03  parrish - deallocate _lat,_lon arrays for u,v,cwmr,oz
!   2005-06-08  treadon - check flag to see if need to deallocate derivatives
!   2005-07-27  kleist  - modified to include some shared arrays
!   2006-07-31  kleist  - use ges_ps arrays instead of ln(ps)
!   2006-12-04  todling - remove bias destroy; rename routine
!   2006-12-15  todling - using internal switches to deallc(tnds/drvs)
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2012-05-14  todling - revist cw check to check also on some hyrometeors
!   2019-09-10  martin  - added new fields to save guess tsen/geop_hgt for writing increment
!   2021-01-05  x.zhang/lei  - add code for updating delz analysis in regional da
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   kleist          org: w/nmc20     date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------
    integer(i_kind):: istatus

    call destroy_ges_derivatives

    call destroy_ges_tendencies
!
    deallocate(ges_prsi,ges_prsl,ges_lnprsl,ges_lnprsi,&
         ges_tsen,ges_teta,geop_hgtl,geop_hgti,ges_geopi,ges_prslavg,ges_rho,&
         ges_tsen1,ges_q1,&
         tropprs,fact_tv,pbl_height,wgt_lcbas,ges_qsat,stat=istatus)
    if(l_reg_update_hydro_delz) deallocate( geom_hgti,geom_hgti_bg)
    if(w_exist) deallocate(ges_w_btlev,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_GES_GRIDS(ges_prsi,..):  deallocate error, istatus=',&
         istatus

    return
  end subroutine destroy_ges_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_sfc_grids --- Deallocate surface fields
!
! !INTERFACE:
!
  subroutine destroy_sfc_grids

! !USES:

   implicit none
   
! !DESCRIPTION: deallocate surface related grids
!
! !REVISION HISTORY:
!   2003-12-01  kleist
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2005-06-03  parrish - deallocate sfct_lat and sfct_lon
!   2007-03-15  todling - merged in da Silva/Cruz ESMF changes
!   2008-06-30  derber - remove sfct deallocate to allow earlier call
!   2009-01-17  todling - move isli2,sno2 into destroy_sfct
!   2010-03-15  todling - esmf protection
!   2012-03-06  akella  - add call to destroy NST analysis arrays
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   kleist          org: w/nmc20     date: 2003-12-01
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind):: istatus

    if(.not.sfc_grids_allocated_) call die('destroy_sfc_grids_','not allocated')
    sfc_grids_allocated_=.false.

    deallocate(isli_g,isli2,sno2,stat=istatus)
    if (istatus/=0) &
         write(6,*)'DESTROY_SFC_GRIDS:  deallocate error, istatus=',&
         istatus
#ifndef HAVE_ESMF
    if(allocated(isli))deallocate(isli)
    if(allocated(fact10))deallocate(fact10)
    if(allocated(sfct))deallocate(sfct)
    if(allocated(sno))deallocate(sno)
    if(allocated(veg_type))deallocate(veg_type)
    if(allocated(veg_frac))deallocate(veg_frac)
    if(allocated(soil_type))deallocate(soil_type)
    if(allocated(sfc_rough))deallocate(sfc_rough)
    if(allocated(soil_temp))deallocate(soil_temp)
    if(allocated(soil_moi))deallocate(soil_moi)
    if(allocated(dsfct))deallocate(dsfct)
    if(allocated(coast_prox))deallocate(coast_prox)
#endif

    return
  end subroutine destroy_sfc_grids

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: create_gesfinfo --- Allocate guess-files information arrays
!
! !INTERFACE:
!
  subroutine create_gesfinfo

! !USES:

   implicit none

! !DESCRIPTION: allocate guess-files information arrays
!
! !REVISION HISTORY:
!   2009-01-08  todling
!   2019-03-21  Wei/Martin - added separate aerosol input file
!
! !REMARKS:
!   language: f90
!   machine:ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   todling          org: w/nmc2     date: 2009-01-08
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind):: istatus
    if(gesfinfo_created_) call die('create_gesfinfo','already created')
    gesfinfo_created_=.true.

#ifndef HAVE_ESMF
    nfldsig_all=nfldsig
    nfldsfc_all=nfldsfc
    nfldnst_all=nfldnst
    nfldsig_now=0	! _now variables are not used if not for ESMF
    nfldsfc_now=0
    nfldnst_now=0
    nfldaer_all=nfldaer
    nfldaer_now=0
    extrap_intime=.true.
    if(nfldsig>0) allocate(hrdifsig(nfldsig),ifilesig(nfldsig), &
                           hrdifsig_all(nfldsig_all), &
                           stat=istatus)
    if (istatus/=0) &
         call die('CREATE_GESFINFO', '(hrdifsig,..):  allocate error, istatus=', istatus)
    if(nfldsfc>0) allocate(hrdifsfc(nfldsfc),ifilesfc(nfldsfc), &
                           hrdifsfc_all(nfldsfc_all), &
                           stat=istatus)
    if (istatus/=0) &
         call die('CREATE_GESFINFO', '(hrdifsfc,..):  allocate error, istatus=',&
         istatus)
    if(nfldnst>0) allocate(hrdifnst(nfldnst),ifilenst(nfldnst), &
                           hrdifnst_all(nfldnst_all), &
                           stat=istatus)
    if (istatus/=0) &
         call die('CREATE_GESFINFO', '(hrdifnst,..):  allocate error, istatus=',&
         istatus)
    if(nfldnst>0) allocate(hrdifaer(nfldaer),ifileaer(nfldaer), &
                           hrdifaer_all(nfldaer_all), &
                           stat=istatus)
    if (istatus/=0) &
         call die('CREATE_GESFINFO', '(hrdifaer,..):  allocate error, istatus=',&
         istatus)
#endif /* HAVE_ESMF */

    return
  end subroutine create_gesfinfo

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: destroy_gesfinfo --- Deallocate guess-files information
!
! !INTERFACE:
!
  subroutine destroy_gesfinfo

! !USES:

   implicit none

! !DESCRIPTION: deallocate guess-files information
!
! !REVISION HISTORY:
!   2009-01-08  todling
!   2019-03-21  Wei/Martin - added external aerosol file variables
!
! !REMARKS:
!   language: f90
!   machine:ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR: 
!   todling          org: w/nmc2     date: 2009-01-08
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind):: istatus
    if(.not.gesfinfo_created_) call die('destroy_gesfinfo','not created')
    gesfinfo_created_=.false.

#ifndef HAVE_ESMF
    if(nfldsig>0) deallocate(hrdifsig,ifilesig,hrdifsig_all,stat=istatus)
    if (istatus/=0) &
         call die('DESTROY_GESFINFO', 'deallocate error, istatus=',istatus)
    if(nfldsfc>0) deallocate(hrdifsfc,ifilesfc,hrdifsfc_all,stat=istatus)
    if (istatus/=0) &
         call die('DESTROY_GESFINFO', 'deallocate error, istatus=',istatus)
    if(nfldnst>0) deallocate(hrdifnst,ifilenst,hrdifnst_all,stat=istatus)
    if (istatus/=0) &
         call die('DESTROY_GESFINFO', 'deallocate error, istatus=',istatus)
    if(nfldnst>0) deallocate(hrdifaer,ifileaer,hrdifaer_all,stat=istatus)
    if (istatus/=0) &
         call die('DESTROY_GESFINFO', 'deallocate error, istatus=',istatus)

    nfldsfc_all=0
    nfldnst_all=0
    nfldsig_all=0
    nfldsfc    =0
    nfldnst    =0
    nfldsig    =0
    nfldaer_all=0
    nfldaer    =0
#endif /* HAVE_ESMF */

    return
  end subroutine destroy_gesfinfo


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_prsges --- Populate guess pressure arrays
!
! !INTERFACE:
!
  subroutine load_prsges

! !USES:

    use constants,only: zero,one,rd_over_cp,one_tenth,half,ten,rd,r1000
    use gridmod, only: lat2,lon2,nsig,ak5,bk5,ck5,tref5,idvc5,&
         regional,wrf_nmm_regional,nems_nmmb_regional,wrf_mass_regional,&
         cmaq_regional,pt_ll,aeta2_ll,fv3_regional,&
         aeta1_ll,eta2_ll,pdtop_ll,eta1_ll,twodvar_regional,idsl5
    use obsmod, only: dtype,ndat
    implicit none

! !DESCRIPTION: populate guess pressure arrays
!
! !REVISION HISTORY:
!   2003-10-15  kleist
!   2004-03-22  parrish, regional capability added
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue; added onlys
!   2004-07-28  treadon - remove subroutine call list, pass variables via modules
!   2005-05-24  pondeca - add regional surface analysis option
!   2006-04-14  treadon - unify global calculations to use ak5,bk5
!   2006-04-17  treadon - add ges_psfcavg and ges_prslavg for regional
!   2006-07-31  kleist  - use ges_ps instead of ln(ps)
!   2007-05-08  kleist  - add fully generalized coordinate for pressure calculation
!   2011-07-07  todling - add cap for log(pressure) calculation
!   2017-03-23  Hu      - add code to use hybrid vertical coodinate in WRF MASS
!                         core
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   kleist          org: w/nmc20     date: 2003-10-15
!
!EOP
!-------------------------------------------------------------------------

!   Declare local parameter
    character(len=*),parameter::myname_=myname//'*load_prsges'
    real(r_kind),parameter:: r1013=1013.0_r_kind

!   Declare local variables
    real(r_kind) kap1,kapr,trk
    real(r_kind),dimension(:,:)  ,pointer::ges_ps=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
    real(r_kind) pinc(lat2,lon2)
    integer(i_kind) i,j,k,ii,jj,itv,ips,kp
    logical ihaveprs(nfldsig)

    kap1=rd_over_cp+one
    kapr=one/rd_over_cp

    ihaveprs=.false.
    do jj=1,nfldsig
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps,ips)
       if(ips/=0) call die(myname_,': ps not available in guess, abort',ips)
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv,itv)
       if(idvc5==3) then
          if(itv/=0) call die(myname_,': tv must be present when idvc5=3, abort',itv)
       endif

!!!!!!!!!!!!  load delp to ges_prsi in read_fv3_netcdf_guess !!!!!!!!!!!!!!!!!
    if (fv3_regional ) then
       do j=1,lon2
          do i=1,lat2
             pinc(i,j)=(ges_ps(i,j)-ges_prsi(i,j,1,jj))
          enddo
       enddo
       do k=1,nsig+1
          do j=1,lon2
             do i=1,lat2
                ges_prsi(i,j,k,jj)=ges_prsi(i,j,k,jj)+eta2_ll(k)*pinc(i,j)
             enddo
          enddo
       enddo
    endif

       do k=1,nsig+1
          do j=1,lon2
             do i=1,lat2
                if(regional) then
                   if (wrf_nmm_regional.or.nems_nmmb_regional) &
                      ges_prsi(i,j,k,jj)=one_tenth* &
                             (eta1_ll(k)*pdtop_ll + &
                              eta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &
                              pt_ll)

                   if (twodvar_regional .or. cmaq_regional ) &
                      ges_prsi(i,j,k,jj)=one_tenth*(eta1_ll(k)*(ten*ges_ps(i,j)-pt_ll) + pt_ll)

                   if (wrf_mass_regional) &      
                      ges_prsi(i,j,k,jj)=one_tenth*(eta1_ll(k)*(ten*ges_ps(i,j)-pt_ll) + &
                                                    eta2_ll(k) + pt_ll)
                else
                   if (idvc5==1 .or. idvc5==2) then
                      ges_prsi(i,j,k,jj)=ak5(k)+(bk5(k)*ges_ps(i,j))
                   else if (idvc5==3) then
                      if (k==1) then
                         ges_prsi(i,j,k,jj)=ges_ps(i,j)
                      else if (k==nsig+1) then
                         ges_prsi(i,j,k,jj)=zero
                      else
                         trk=(half*(ges_tv(i,j,k-1)+ges_tv(i,j,k))/tref5(k))**kapr
                         ges_prsi(i,j,k,jj)=ak5(k)+(bk5(k)*ges_ps(i,j))+(ck5(k)*trk)
                      end if
                   end if
                endif
                ges_prsi(i,j,k,jj)=max(ges_prsi(i,j,k,jj),zero)
                ges_lnprsi(i,j,k,jj)=log(max(ges_prsi(i,j,k,jj),0.0001_r_kind))
             end do
          end do
       end do
       ihaveprs(jj)=.true.
    end do

    if(regional) then
       if (wrf_nmm_regional.or.nems_nmmb_regional) then
! load using aeta coefficients
          do jj=1,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps ,ips)
             do k=1,nsig
                do j=1,lon2
                   do i=1,lat2
                      ges_prsl(i,j,k,jj)=one_tenth* &
                                  (aeta1_ll(k)*pdtop_ll + &
                                   aeta2_ll(k)*(ten*ges_ps(i,j)-pdtop_ll-pt_ll) + &
                                   pt_ll)
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))

                   end do
                end do
             end do
          end do
       end if   ! end if wrf_nmm regional block

       if (fv3_regional) then
          do jj=1,nfldsig
             do k=1,nsig
                 kp=k+1
                do j=1,lon2
                   do i=1,lat2
                      ges_prsl(i,j,k,jj)=(ges_prsi(i,j,k,jj)+ges_prsi(i,j,kp,jj))*half
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))

                   end do
                end do
             end do
          end do
       end if   ! end if fv3 regional

       if (twodvar_regional .or. cmaq_regional) then
! load using aeta coefficients
          do jj=1,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps ,ips)
             do k=1,nsig
                do j=1,lon2
                   do i=1,lat2
                      ges_prsl(i,j,k,jj)=one_tenth*(aeta1_ll(k)*(ten*ges_ps(i,j)-pt_ll)+pt_ll)
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
                   end do
                end do
             end do
          end do
       end if   ! end if twodvar_regional, cmaq_regional block
       if (wrf_mass_regional) then
! load using aeta coefficients
          do jj=1,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps ,ips)
             do k=1,nsig
                do j=1,lon2
                   do i=1,lat2
                      ges_prsl(i,j,k,jj)=one_tenth*(aeta1_ll(k)*(ten*ges_ps(i,j)-pt_ll)+&
                                                    aeta2_ll(k) + pt_ll)
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
                   end do
                end do
             end do
          end do
       end if   ! end if wrf_mass regional block

    else

!      load mid-layer pressure by using phillips vertical interpolation
       if (idsl5/=2) then
          do jj=1,nfldsig
             if(.not.ihaveprs(jj)) then
                call tell(myname,'3d pressure has not been calculated somehow',99)
                exit ! won't die ...
             endif
             do j=1,lon2
                do i=1,lat2
                   do k=1,nsig
                      ges_prsl(i,j,k,jj)=((ges_prsi(i,j,k,jj)**kap1-ges_prsi(i,j,k+1,jj)**kap1)/&
                           (kap1*(ges_prsi(i,j,k,jj)-ges_prsi(i,j,k+1,jj))))**kapr
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
                   end do
                end do
             end do
          end do

!      load mid-layer pressure by simple averaging
       else
          do jj=1,nfldsig
             if(.not.ihaveprs(jj)) then
                call tell(myname,'3d pressure has not been calculated somehow',99)
                exit ! won't die ...
             endif
             do j=1,lon2
                do i=1,lat2
                   do k=1,nsig
                      ges_prsl(i,j,k,jj)=(ges_prsi(i,j,k,jj)+ges_prsi(i,j,k+1,jj))*half
                      ges_lnprsl(i,j,k,jj)=log(ges_prsl(i,j,k,jj))
                   end do
                end do
             end do
          end do
       endif

    end if  !  end regional/global block

!   Compute density for dBZ assimilation purposes - multiply by 1000 to convert
!   to Pa
    do ii=1,ndat
       if ( index(dtype(ii), 'dbz') /= 0 )then
          do jj=1,nfldsig
             call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv,itv)
             if(idvc5==3) then
                if(itv/=0) call die(myname_,': tv must be present when idvc5=3,abort',itv)
             endif
             do j=1,lon2
                do i=1,lat2
                   do k=1,nsig
                        ges_rho(i,j,k,jj)=(ges_prsl(i,j,k,jj)/(ges_tv(i,j,k)*rd))*r1000
                   end do
                end do
             end do
          end do
       end if
    end do

! For regional applications only, load variables containing mean
! surface pressure and pressure profile at the layer midpoints
    if (regional) then
       ges_psfcavg = r1013
       if (wrf_nmm_regional.or.nems_nmmb_regional) then
          do k=1,nsig
             ges_prslavg(k)=aeta1_ll(k)*pdtop_ll+aeta2_ll(k)*(r1013-pdtop_ll-pt_ll)+pt_ll
          end do
       elseif (fv3_regional) then
          do k=1,nsig
             ges_prslavg(k)=aeta1_ll(k)*ten+r1013*aeta2_ll(k)
          end do
       elseif (wrf_mass_regional) then
          do k=1,nsig
             ges_prslavg(k)=aeta1_ll(k)*(r1013-pt_ll)+aeta2_ll(k) + pt_ll
          end do
       else
          do k=1,nsig
             ges_prslavg(k)=aeta1_ll(k)*(r1013-pt_ll)+pt_ll
          end do
       endif
    endif


    return
  end subroutine load_prsges

  subroutine get_ref_gesprs(prs)
  use constants, only: zero,one_tenth,r100,r1000
  use gridmod, only: regional,twodvar_regional,cmaq_regional
  use gridmod, only: wrf_nmm_regional,nems_nmmb_regional,wrf_mass_regional,fv3_regional
  use gridmod, only: idvc5,ak5,bk5
  use gridmod, only: eta1_ll
  use gridmod, only: eta2_ll
  use gridmod, only: pdtop_ll
  use gridmod, only: pt_ll
  use gridmod, only: nsig
  implicit none
  real(r_kind), dimension(nsig+1), intent(out) :: prs

  integer(i_kind) k

! get some reference-like pressure levels
  do k=1,nsig+1
     if(regional) then
        if (wrf_nmm_regional.or.nems_nmmb_regional.or.cmaq_regional) &
           prs(k)=one_tenth* &
                  (eta1_ll(k)*pdtop_ll + &
                   eta2_ll(k)*(r1000-pdtop_ll-pt_ll) + &
                   pt_ll)
        if (twodvar_regional) &
           prs(k)=one_tenth*(eta1_ll(k)*(r1000-pt_ll) + pt_ll)
        if (fv3_regional ) &
           prs(k)=eta1_ll(k)+r100*eta2_ll(k)
        if (wrf_mass_regional) &
           prs(k)=one_tenth*(eta1_ll(k)*(r1000-pt_ll) + eta2_ll(k) + pt_ll)
     else
        if (idvc5==1 .or. idvc5==2) then
           prs(k)=ak5(k)+(bk5(k)*r1000)
        else if (idvc5==3) then
           if (k==1) then
              prs(k)=r1000
           else if (k==nsig+1) then
              prs(k)=zero
           else
              prs(k)=ak5(k)+(bk5(k)*r1000)! +(ck5(k)*trk)
           end if
        end if
     endif
  enddo
  end subroutine get_ref_gesprs


!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_geop_hgt --- Populate guess geopotential height
!
! !INTERFACE:
!
  subroutine load_geop_hgt

! !USES:

    use constants, only: one,eps, rd, grav, half, t0c, fv
    use constants, only: cpf_a0, cpf_a1, cpf_a2, cpf_b0, cpf_b1, cpf_c0, cpf_c1, cpf_d, cpf_e
    use constants, only: psv_a, psv_b, psv_c, psv_d
    use constants, only: ef_alpha, ef_beta, ef_gamma
    use constants, only: one,two,grav_equator,flattening,semi_major_axis,grav_ratio,somigliana,eccentricity
    use gridmod, only: lat2, lon2, nsig, twodvar_regional

    use gridmod, only: region_lat,region_lon
    use gridmod,only: istart,jstart
    use gridmod,only: l_reg_update_hydro_delz,nlat,nlon
    use mpimod, only: mype

    implicit none

! !INPUT PARAMETERS:


! !DESCRIPTION: populate guess geopotential height
!
! !REVISION HISTORY:
!   2003-10-15  treadon
!   2004-05-14  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-10-28  treadon - replace "tiny" with "tiny_r_kind"
!   2004-12-15  treadon - replace use of Paul van Delst's Geopotential
!                         function with simple integration of hydrostatic
!                         equation (done to be consistent with Lidia
!                         Cucurull's GPS work)
!   2005-05-24  pondeca - add regional surface analysis option
!   2010-08-27  cucurull - add option to compute and use compressibility factors in geopot heights
!   2021-01-05  x.zhang/lei  - add code for updating delz analysis in regional da
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20      date: 2003-10-15
!
!EOP
!-------------------------------------------------------------------------

    character(len=*),parameter::myname_=myname//'*load_geop_hgt'
    real(r_kind),parameter:: thousand = 1000.0_r_kind

    integer(i_kind) i,j,k,jj,ier,istatus
    real(r_kind) h,dz,rdog
    real(r_kind),dimension(nsig+1):: height
    real(r_kind) cmpr, x_v, rl_hm, fact, pw, tmp_K, tmp_C, prs_sv, prs_a, ehn_fct, prs_v
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_q=>NULL()
    real(r_kind),dimension(:,:  ),pointer::ges_z=>NULL()
 
    real(r_kind) slat,slon
    real(r_kind) sin2,termg,termr,termrg
    integer (i_kind) iglob,jglob,mm1


    if (twodvar_regional) return

    rdog = rd/grav

    if (use_compress) then

!     Compute compressibility factor (Picard et al 2008) and geopotential heights at midpoint 
!     of each layer

       do jj=1,nfldsig
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'z' ,ges_z ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'q' ,ges_q ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
          ier=ier+istatus
          if(ier/=0) exit
          do j=1,lon2
             do i=1,lat2
                k  = 1
                fact    = one + fv * ges_q(i,j,k)
                pw      = eps + ges_q(i,j,k)*( one - eps )
                tmp_K   = ges_tv(i,j,k) / fact
                tmp_C   = tmp_K - t0c
                prs_sv  = exp(psv_a*tmp_K**2 + psv_b*tmp_K + psv_c + psv_d/tmp_K)  ! Pvap sat, eq A1.1 (Pa)
                prs_a   = thousand * exp(half*(log(ges_prsi(i,j,k,jj)) + log(ges_prsl(i,j,k,jj))))     ! (Pa) 
                ehn_fct = ef_alpha + ef_beta*prs_a + ef_gamma*tmp_C**2 ! enhancement factor (eq. A1.2)
                prs_v   = ges_q(i,j,k) * prs_a / pw   ! vapor pressure (Pa)
                rl_hm   = prs_v / prs_sv    ! relative humidity
                x_v     = rl_hm * ehn_fct * prs_sv / prs_a     ! molar fraction of water vapor (eq. A1.3)
 
                ! Compressibility factor (eq A1.4 from Picard et al 2008)
                cmpr = one - (prs_a/tmp_K) * (cpf_a0 + cpf_a1*tmp_C + cpf_a2*tmp_C**2 &
                           + (cpf_b0 + cpf_b1*tmp_C)*x_v + (cpf_c0 + cpf_c1*tmp_C)*x_v**2 ) &
                           + (prs_a**2/tmp_K**2) * (cpf_d + cpf_e*x_v**2)

                h  = rdog * ges_tv(i,j,k)
                dz = h * cmpr * log(ges_prsi(i,j,k,jj)/ges_prsl(i,j,k,jj))
                height(k) = ges_z(i,j) + dz   

                do k=2,nsig
                   fact    = one + fv * half * (ges_q(i,j,k-1)+ges_q(i,j,k))
                   pw      = eps + half * (ges_q(i,j,k-1)+ges_q(i,j,k)) * (one - eps)
                   tmp_K   = half * (ges_tv(i,j,k-1)+ges_tv(i,j,k)) / fact
                   tmp_C   = tmp_K - t0c
                   prs_sv  = exp(psv_a*tmp_K**2 + psv_b*tmp_K + psv_c + psv_d/tmp_K)  ! eq A1.1 (Pa)
                   prs_a   = thousand * exp(half*(log(ges_prsl(i,j,k-1,jj))+log(ges_prsl(i,j,k,jj))))   ! (Pa)
                   ehn_fct = ef_alpha + ef_beta*prs_a + ef_gamma*tmp_C**2 ! enhancement factor (eq. A1.2)
                   prs_v   = half*(ges_q(i,j,k-1)+ges_q(i,j,k) ) * prs_a / pw   ! (Pa)
                   rl_hm   = prs_v / prs_sv    ! relative humidity
                   x_v     = rl_hm * ehn_fct * prs_sv / prs_a     ! molar fraction of water vapor (eq. A1.3)
                   cmpr    = one - (prs_a/tmp_K) * ( cpf_a0 + cpf_a1*tmp_C + cpf_a2*tmp_C**2 &
                             + (cpf_b0 + cpf_b1*tmp_C)*x_v + (cpf_c0 + cpf_c1*tmp_C)*x_v**2 ) &
                             + (prs_a**2/tmp_K**2) * (cpf_d + cpf_e*x_v**2)
                   h       = rdog * half * (ges_tv(i,j,k-1)+ges_tv(i,j,k))
                   dz      = h * cmpr * log(ges_prsl(i,j,k-1,jj)/ges_prsl(i,j,k,jj))
                   height(k) = height(k-1) + dz
                end do

                do k=1,nsig
                   geop_hgtl(i,j,k,jj)=height(k) - ges_z(i,j)
                end do
             enddo
          enddo
       enddo
       if(ier/=0) return

!      Compute compressibility factor (Picard et al 2008) and geopotential heights at interface
!      between layers

       do jj=1,nfldsig
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'z'  ,ges_z ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'q'  ,ges_q ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
          ier=ier+istatus
          if(ier/=0) exit
          do j=1,lon2
             do i=1,lat2
                k=1
                height(k) = ges_z(i,j)

                do k=2,nsig
                   fact    = one + fv * ges_q(i,j,k-1)
                   pw      = eps + ges_q(i,j,k-1)*(one - eps)
                   tmp_K   = ges_tv(i,j,k-1) / fact
                   tmp_C   = tmp_K - t0c
                   prs_sv  = exp(psv_a*tmp_K**2 + psv_b*tmp_K + psv_c + psv_d/tmp_K)  ! eq A1.1 (Pa)
                   prs_a   = thousand * exp(half*(log(ges_prsi(i,j,k-1,jj))+log(ges_prsi(i,j,k,jj)))) 
                   ehn_fct = ef_alpha + ef_beta*prs_a + ef_gamma*tmp_C**2 ! enhancement factor (eq. A1.2)
                   prs_v   = ges_q(i,j,k-1) * prs_a / pw   ! vapor pressure (Pa)
                   rl_hm   = prs_v / prs_sv    ! relative humidity
                   x_v     = rl_hm * ehn_fct * prs_sv / prs_a     ! molar fraction of water vapor (eq. A1.3)
                   cmpr    = one - (prs_a/tmp_K) * ( cpf_a0 + cpf_a1*tmp_C + cpf_a2*tmp_C**2 &
                            + (cpf_b0 + cpf_b1*tmp_C)*x_v + (cpf_c0 + cpf_c1*tmp_C)*x_v**2 ) &
                            + (prs_a**2/tmp_K**2) * (cpf_d + cpf_e*x_v**2)
                   h       = rdog * ges_tv(i,j,k-1)
                   dz      = h * cmpr * log(ges_prsi(i,j,k-1,jj)/ges_prsi(i,j,k,jj))
                   height(k) = height(k-1) + dz
                enddo

                k=nsig+1
                fact    = one + fv* ges_q(i,j,k-1)
                pw      = eps + ges_q(i,j,k-1)*(one - eps)
                tmp_K   = ges_tv(i,j,k-1) / fact
                tmp_C   = tmp_K - t0c
                prs_sv  = exp(psv_a*tmp_K**2 + psv_b*tmp_K + psv_c + psv_d/tmp_K)  ! eq A1.1 (Pa)
                prs_a   = thousand * exp(half*(log(ges_prsi(i,j,k-1,jj))+log(ges_prsl(i,j,k-1,jj))))     ! (Pa)
                ehn_fct = ef_alpha + ef_beta*prs_a + ef_gamma*tmp_C**2 ! enhancement factor (eq. A1.2)
                prs_v   = ges_q(i,j,k-1) * prs_a / pw  
                rl_hm   = prs_v / prs_sv    ! relative humidity
                x_v     = rl_hm * ehn_fct * prs_sv / prs_a     ! molar fraction of water vapor (eq. A1.3)
                cmpr    = one - (prs_a/tmp_K) * ( cpf_a0 + cpf_a1*tmp_C + cpf_a2*tmp_C**2 &
                          + (cpf_b0 + cpf_b1*tmp_C)*x_v + (cpf_c0 + cpf_c1*tmp_C)*x_v**2 ) &
                          + (prs_a**2/tmp_K**2) * (cpf_d + cpf_e*x_v**2)
                h       = rdog * ges_tv(i,j,k-1)
                dz      = h * cmpr * log(ges_prsi(i,j,k-1,jj)/ges_prsl(i,j,k-1,jj))
                height(k) = height(k-1) + dz
 
                do k=1,nsig+1
                   geop_hgti(i,j,k,jj)=height(k) - ges_z(i,j)
                end do
             enddo
          enddo
       enddo
       if(ier/=0) return

    else

!      Compute geopotential height at midpoint of each layer
       do jj=1,nfldsig
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'z'  ,ges_z  ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
          ier=ier+istatus
          if(ier/=0) exit
          do j=1,lon2
             do i=1,lat2
                k  = 1
                h  = rdog * ges_tv(i,j,k)
                dz = h * log(ges_prsi(i,j,k,jj)/ges_prsl(i,j,k,jj))
                height(k) = ges_z(i,j) + dz
 
                do k=2,nsig
                   h  = rdog * half * (ges_tv(i,j,k-1)+ges_tv(i,j,k))
                   dz = h * log(ges_prsl(i,j,k-1,jj)/ges_prsl(i,j,k,jj))
                   height(k) = height(k-1) + dz
                end do

                do k=1,nsig
                   geop_hgtl(i,j,k,jj)=height(k) - ges_z(i,j)
                end do
             end do
          end do
       end do
       if(ier/=0) return

!      Compute geopotential height at interface between layers
       do jj=1,nfldsig
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'z'  ,ges_z  ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
          ier=ier+istatus
          if(ier/=0) call die(myname_,'not all fields available, ier=',ier)
          do j=1,lon2
             do i=1,lat2
                k=1
                height(k) = ges_z(i,j)

                do k=2,nsig
                   h  = rdog * ges_tv(i,j,k-1)
                   dz = h * log(ges_prsi(i,j,k-1,jj)/ges_prsi(i,j,k,jj))
                   height(k) = height(k-1) + dz
                end do

                k=nsig+1
                h = rdog * ges_tv(i,j,k-1)
                dz = h * log(ges_prsi(i,j,k-1,jj)/ges_prsl(i,j,k-1,jj))
                height(k) = height(k-1) + dz

                do k=1,nsig+1
                   geop_hgti(i,j,k,jj)=height(k) - ges_z(i,j)
                end do
             end do
          end do
       end do

    endif
    if (l_reg_update_hydro_delz ) then
!       Convert geopotential height at layer midpoints to geometric height using
!       equations (17, 20, 23) in MJ Mahoney's note "A discussion of various
!       measures of altitude" (2001).  Available on the web at
!       http://mtp.jpl.nasa.gov/notes/altitude/altitude.html
!
!       termg  = equation 17
!       termr  = equation 21
!       termrg = first term in the denominator of equation 23
!       zges   = equation 23
        mm1=mype+1
        do jj=1,nfldsig
          do j=1,lon2
            jglob=max(1,min(j+jstart(mm1)-2,nlon))
            do i=1,lat2
              iglob=max(1,min(i+istart(mm1)-2,nlat))
              slat=region_lat(iglob,jglob)
              slon=region_lon(iglob,jglob)

              sin2  = sin(slat)*sin(slat)
              termg = grav_equator * &
                   ((one+somigliana*sin2)/sqrt(one-eccentricity*eccentricity*sin2))
              termr = semi_major_axis /(one + flattening + grav_ratio -  &
                   two*flattening*sin2)
              termrg = (termg/grav)*termr
              do k=1,nsig+1
                 geom_hgti(i,j,k,jj) = (termr*geop_hgti(i,j,k,jj))/(termrg-geop_hgti(i,j,k,jj))  ! eq (23)
              end do
            enddo
          enddo
        enddo !jj

    endif

   return
  end subroutine load_geop_hgt

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_gsdpbl_hgt --- Populate PBL height
!
! !INTERFACE:
!
  subroutine load_gsdpbl_hgt(mype)

! !USES:

    use constants, only: one,rd_over_cp_mass,r1000,ten,zero,two
    use gridmod, only: lat2, lon2, nsig,wrf_mass_regional, &
         aeta1_ll,aeta2_ll,pdtop_ll,pt_ll,&
         twodvar_regional,nems_nmmb_regional,fv3_regional

    implicit none

! !INPUT PARAMETERS:


! !DESCRIPTION: populate guess geopotential height in millibars
!
!
! !REVISION HISTORY:
!   2011-06-06  Ming Hu
!   2013-02-22  Jacob Carley - Added NMMB
!   2022-01-07  Ming Hu - added fv3_regional
!
! !REMARKS:
!   language: f90
!   machine:  JET
!
! !AUTHOR:
!
!EOP
!-------------------------------------------------------------------------

    character(len=*),parameter::myname_=myname//'*load_gsdpbl_hgt'
    integer(i_kind)              , intent(in   ) :: mype
    integer(i_kind) i,j,k,jj,ier,istatus
    real(r_kind),dimension(nsig):: thetav
    real(r_kind),dimension(nsig):: pbk
    real(r_kind) :: thsfc, d
    real(r_kind),dimension(:,:  ),pointer::ges_ps_01=>NULL()
    real(r_kind),dimension(:,:  ),pointer::ges_ps=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()

    if (twodvar_regional) then
       if(mype==0) write(6,*)'not setup for twodvar_regional in load_gsdpbl_hgt'
       return 
    endif

!   Compute geopotential height at midpoint of each layer
    do jj=1,nfldsig
       ier=0
       call gsi_bundlegetpointer(gsi_metguess_bundle(1) ,'ps' ,ges_ps_01 ,istatus)
       ier=ier+istatus
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'ps' ,ges_ps ,istatus)
       ier=ier+istatus
       call gsi_bundlegetpointer(gsi_metguess_bundle(jj),'tv' ,ges_tv ,istatus)
       ier=ier+istatus
       if(ier/=0) call die(myname_,'not all fields available, ier=',ier)
       do j=1,lon2
          do i=1,lat2

             do k=1,nsig

                if (wrf_mass_regional) then
                   pbk(k) = aeta1_ll(k)*(ges_ps_01(i,j)*ten-pt_ll)+aeta2_ll(k)+pt_ll
                elseif (nems_nmmb_regional) then
                   pbk(k) = aeta1_ll(k)*pdtop_ll + aeta2_ll(k)*(ten*ges_ps(i,j) & 
                            -pdtop_ll-pt_ll) + pt_ll
                elseif (fv3_regional) then
                   pbk(k) = ges_prsl(i,j,k,1) * ten
                else
                   write(*,*) "Error: not an model option in load_gsdpbl_hgt"
                   call stop2(1234)
                end if

                thetav(k)  = ges_tv(i,j,k)*(r1000/pbk(k))**rd_over_cp_mass
             end do

             pbl_height(i,j,jj) = zero
             thsfc = thetav(1)
             k=1
             DO while (abs(pbl_height(i,j,jj)) < 0.0001_r_kind)
               if( thetav(k) > thsfc + 1.0_r_kind ) then
                 pbl_height(i,j,jj) = real(k,r_kind) - (thetav(k) - (thsfc + 1.0_r_kind))/   &
                             max((thetav(k)-thetav(k-1)),0.01_r_kind)
               endif
               k=k+1
             ENDDO
             if(abs(pbl_height(i,j,jj)) < 0.0001_r_kind) pbl_height(i,j,jj)=two
             k=int(pbl_height(i,j,jj))
             if( k < 1 .or. k > nsig-1) then
                write(6,*) ' Error in PBL height calculation ',mype,i,j,pbl_height(i,j,jj)
             endif
             d=pbl_height(i,j,jj) - k
             pbl_height(i,j,jj) = pbk(k) * (one-d) + pbk(k+1) * d

          end do
       end do
    end do

    return
  end subroutine load_gsdpbl_hgt

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: add_rtm_layers --- Add pressure layers for RTM use
!
! !INTERFACE:
!
  subroutine add_rtm_layers(prsitmp,prsltmp,prsitmp_ext,prsltmp_ext,klevel)

! !USES:

    use constants, only: half,ten,one_tenth
    use gridmod, only: nsig,msig,nlayers
    use crtm_module, only: toa_pressure

    implicit none

! !INPUT PARAMETERS:
    integer(i_kind),dimension(msig)  ,intent(  out) :: klevel

    real(r_kind)   ,dimension(nsig+1),intent(in   ) :: prsitmp
    real(r_kind)   ,dimension(nsig)  ,intent(in   ) :: prsltmp

    real(r_kind)   ,dimension(msig+1),intent(  out) :: prsitmp_ext
    real(r_kind)   ,dimension(msig)  ,intent(  out) :: prsltmp_ext


! !DESCRIPTION:  Add pressure layers for use in RTM
!
! !REVISION HISTORY:
!   2005-06-01  treadon
!   2006-05-10  derber modify how levels are added above model top
!   2013-03-27  rancic fix for toa units: crtm(hPa); prsitmp(kPa)
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20      date: 2005-06-01
!
!EOP
!-------------------------------------------------------------------------

!   Declare local variables
    integer(i_kind) k,kk,l
    real(r_kind) dprs,toa_prs_kpa

!   Convert toa_pressure to kPa
!   ---------------------------
    toa_prs_kpa = toa_pressure*one_tenth

!   Check if model top pressure above rtm top pressure, where prsitmp
!   is in kPa and toa_pressure is in hPa.
    if (prsitmp(nsig) < toa_prs_kpa)then
       write(6,*)'ADD_RTM_LAYERS:  model top pressure(hPa)=', &
            ten*prsitmp(nsig),&
            ' above rtm top pressure(hPa)=',toa_pressure
       call stop2(35)
    end if

!   Linear in pressure sub-divsions
    kk=0
    do k = 1,nsig
       if (nlayers(k)<=1) then
          kk = kk + 1
          prsltmp_ext(kk) = prsltmp(k)
          prsitmp_ext(kk) = prsitmp(k)
          klevel(kk) = k
       else
          if (k/=nsig) then
             dprs = (prsitmp(k+1)-prsitmp(k))/nlayers(k)
          else
             dprs = (toa_prs_kpa -prsitmp(k))/nlayers(k)
          end if
          prsitmp_ext(kk+1) = prsitmp(k)
          do l=1,nlayers(k)
             kk=kk + 1
             prsitmp_ext(kk+1) = prsitmp(k) + dprs*l
             prsltmp_ext(kk) = half*(prsitmp_ext(kk+1)+prsitmp_ext(kk))
             klevel(kk) = k
          end do
       endif
    end do

!   Set top of atmosphere pressure
    prsitmp_ext(msig+1) = toa_prs_kpa

  end subroutine add_rtm_layers

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: load_fact10 --- Compute 10m wind factor
!
! !INTERFACE:
!
  subroutine load_fact10

! !USES:

    use gridmod, only: lat2,lon2
    implicit none

! !INPUT PARAMETERS:

! !DESCRIPTION: compute 10m wind factor
!
! !REVISION HISTORY:
!   2006-09-26  treadon
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20      date: 2006-09-26
!
!EOP
!-------------------------------------------------------------------------

!   Declare local variables
    character(len=*),parameter::myname_=myname//'*load_fact10'
    logical iqtflg
    integer(i_kind):: i,j,it,itt,nt,regime,ier,istatus
    integer(i_kind),dimension(nfldsfc):: indx
    real(r_kind):: u10ges,v10ges,t2ges,q2ges
    real(r_kind),dimension(:,:  ),pointer::ges_ps=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_u=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_v=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_q=>NULL()

    nt=0
    indx=1
    do i=1,nfldsfc
       if(abs(hrdifsfc(i)-hrdifsig(i))<0.001_r_kind) then
          nt=nt+1
          indx(nt) = i
       endif
    end do


    if (sfcmod_gfs) then
       do it=1,nt
          itt=indx(it)
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'ps',ges_ps,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'u' ,ges_u ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'v' ,ges_v ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'q' ,ges_q ,istatus)
          ier=ier+istatus
          if(ier/=0) call die(myname_,'not all fields available, ier=',ier)
          do j=1,lon2
             do i=1,lat2
                call compute_fact10(ges_u(i,j,1),ges_v(i,j,1),&
                     ges_tsen(i,j,1,itt),ges_q(i,j,1),&
                     ges_ps(i,j),ges_prsi(i,j,1,itt), &
                     ges_prsi(i,j,2,itt),sfct(i,j,itt), &
                     sfc_rough(i,j,itt),isli(i,j,itt),fact10(i,j,itt))
             end do
          end do
       end do
    endif

    if (sfcmod_mm5) then
       iqtflg=.true.
       do it=1,nt
          itt=indx(it)
          ier=0
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'ps',ges_ps,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'u' ,ges_u ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'v' ,ges_v ,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'tv',ges_tv,istatus)
          ier=ier+istatus
          call gsi_bundlegetpointer(gsi_metguess_bundle(itt),'q' ,ges_q ,istatus)
          ier=ier+istatus
          if(ier/=0) call die(myname_,'not all fields available, ier=',ier)
          do j=1,lon2
             do i=1,lat2
                call SFC_WTQ_FWD (&
                     ges_ps(i,j),&
                     sfct(i,j,itt),&
                     ges_lnprsl(i,j,1,itt),&
                     ges_tv(i,j,1),&
                     ges_q(i,j,1),&
                     ges_u(i,j,1),&
                     ges_v(i,j,1),&
                     ges_lnprsl(i,j,2,itt),&
                     ges_tv(i,j,2),&
                     ges_q(i,j,2),&
                     geop_hgtl(i,j,1,itt),&
                     sfc_rough(i,j,itt),&
                     isli(i,j,itt),&
                     fact10(i,j,itt),&
                     u10ges,v10ges,t2ges,q2ges,regime,iqtflg)
             end do
          end do
       end do
    endif

    return
  end subroutine load_fact10

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE: comp_fact10  ---  compute 10m wind factor
!
! !INTERFACE:
!
  subroutine comp_fact10(dlat,dlon,dtime,skint,sfcrough,islimsk,mype,factw)

! !USES:

    use gridmod, only: nlat,nlon,&
         lon1,istart,jstart
    use constants, only: zero,one
    implicit none

! !INPUT PARAMETERS:

    real(r_kind)   ,intent(in   ) :: dlat,dlon,dtime,skint,sfcrough
    real(r_kind)   ,intent(inout) :: factw
    integer(i_kind),intent(in   ) :: mype,islimsk

! !DESCRIPTION: compute 10m wind factor
!
! !REVISION HISTORY:
!   2006-09-26  treadon
!   2008-12-05  todling - use dsfct(:,:,ntguessfc) for calculation
!   2015-03-10  todling - assign missing ps pointers
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   treadon          org: w/nmc20      date: 2006-09-26
!
!EOP
!-------------------------------------------------------------------------

!   Declare local parameters
    character(len=*),parameter::myname_=myname//'*comp_fact10'

!   Declare local variables
    logical iqtflg
    integer(i_kind) ix,ix1,ixp,iy,iy1,iyp,regime
    integer(i_kind) itsig,itsigp,j,m1,islimsk2,ier,istatus
    real(r_kind) w00,w01,w10,w11
    real(r_kind) delx,dely,delx1,dely1,dtsig,dtsigp
    real(r_kind):: u10ges,v10ges,t2ges,q2ges
    real(r_kind):: pgesin,ugesin,vgesin,qgesin,tgesin,prsigesin1
    real(r_kind):: prsigesin2,lnpgesin1,lnpgesin2,tgesin2,qgesin2,geopgesin,ts
    real(r_kind),dimension(:,:  ),pointer::ges_ps_itsig=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_u_itsig=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_v_itsig=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv_itsig=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_q_itsig=>NULL()
    real(r_kind),dimension(:,:  ),pointer::ges_ps_itsigp=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_u_itsigp=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_v_itsigp=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_tv_itsigp=>NULL()
    real(r_kind),dimension(:,:,:),pointer::ges_q_itsigp=>NULL()

    islimsk2=islimsk
    if(islimsk2 > 2)islimsk2=islimsk2-3
    m1=mype+1
!   Set spatial interpolation indices and weights
    ix1=dlat
    ix1=max(1,min(ix1,nlat))
    delx=dlat-ix1
    delx=max(zero,min(delx,one))
    ix=ix1-istart(m1)+2
    ixp=ix+1
    if(ix1==nlat) then
       ixp=ix
    end if
    delx1=one-delx

    iy1=dlon
    dely=dlon-iy1
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
!   Get time interpolation factors for sigma files
    if(dtime > hrdifsig(1) .and. dtime < hrdifsig(nfldsig))then
       do j=1,nfldsig-1
          if(dtime > hrdifsig(j) .and. dtime <= hrdifsig(j+1))then
             itsig=j
             itsigp=j+1
             dtsig=((hrdifsig(j+1)-dtime)/(hrdifsig(j+1)-hrdifsig(j)))
          end if
       end do
    else if(dtime <=hrdifsig(1))then
       itsig=1
       itsigp=1
       dtsig=one
    else
       itsig=nfldsig
       itsigp=nfldsig
       dtsig=one
    end if
    dtsigp=one-dtsig

    ier=0
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsig) ,'ps' ,ges_ps_itsig ,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'ps' ,ges_ps_itsigp,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsig) ,'u' ,ges_u_itsig ,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'u' ,ges_u_itsigp,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsig) ,'v' ,ges_v_itsig ,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'v' ,ges_v_itsigp,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsig) ,'tv',ges_tv_itsig ,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'tv',ges_tv_itsigp,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsig) ,'q' ,ges_q_itsig ,istatus)
    ier=ier+istatus
    call gsi_bundlegetpointer(gsi_metguess_bundle(itsigp),'q' ,ges_q_itsigp,istatus)
    ier=ier+istatus
    if(ier/=0) return

    ts =(dsfct(ix,iy ,ntguessfc)*w00 + dsfct(ixp,iy ,ntguessfc)*w10 +          &
         dsfct(ix,iyp,ntguessfc)*w01 + dsfct(ixp,iyp,ntguessfc)*w11) + skint

    pgesin=(ges_ps_itsig (ix,iy )*w00+ges_ps_itsig (ixp,iy )*w10+ &
            ges_ps_itsig (ix,iyp)*w01+ges_ps_itsig (ixp,iyp)*w11)*dtsig + &
           (ges_ps_itsigp(ix,iy )*w00+ges_ps_itsigp(ixp,iy )*w10+ &
            ges_ps_itsigp(ix,iyp)*w01+ges_ps_itsigp(ixp,iyp)*w11)*dtsigp
    ugesin=(ges_u_itsig (ix,iy ,1)*w00+ges_u_itsig (ixp,iy ,1)*w10+ &
            ges_u_itsig (ix,iyp,1)*w01+ges_u_itsig (ixp,iyp,1)*w11)*dtsig + &
           (ges_u_itsigp(ix,iy ,1)*w00+ges_u_itsigp(ixp,iy ,1)*w10+ &
            ges_u_itsigp(ix,iyp,1)*w01+ges_u_itsigp(ixp,iyp,1)*w11)*dtsigp
    vgesin=(ges_v_itsig (ix,iy ,1)*w00+ges_v_itsig (ixp,iy ,1)*w10+ &
            ges_v_itsig (ix,iyp,1)*w01+ges_v_itsig (ixp,iyp,1)*w11)*dtsig + &
           (ges_v_itsigp(ix,iy ,1)*w00+ges_v_itsigp(ixp,iy ,1)*w10+ &
            ges_v_itsigp(ix,iyp,1)*w01+ges_v_itsigp(ixp,iyp,1)*w11)*dtsigp
    qgesin=(ges_q_itsig (ix,iy ,1)*w00+ges_q_itsig (ixp,iy ,1)*w10+ &
            ges_q_itsig (ix,iyp,1)*w01+ges_q_itsig (ixp,iyp,1)*w11)*dtsig + &
           (ges_q_itsigp(ix,iy ,1)*w00+ges_q_itsigp(ixp,iy ,1)*w10+ &
            ges_q_itsigp(ix,iyp,1)*w01+ges_q_itsigp(ixp,iyp,1)*w11)*dtsigp


    if (sfcmod_gfs) then
       tgesin    =(ges_tsen(ix ,iy ,1,itsig )*w00+ &
                   ges_tsen(ixp,iy ,1,itsig )*w10+ &
                   ges_tsen(ix ,iyp,1,itsig )*w01+ &
                   ges_tsen(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (ges_tsen(ix ,iy ,1,itsigp)*w00+ &
                   ges_tsen(ixp,iy ,1,itsigp)*w10+ &
                   ges_tsen(ix ,iyp,1,itsigp)*w01+ &
                   ges_tsen(ixp,iyp,1,itsigp)*w11)*dtsigp
       prsigesin1=(ges_prsi(ix ,iy ,1,itsig )*w00+ &
                   ges_prsi(ixp,iy ,1,itsig )*w10+ &
                   ges_prsi(ix ,iyp,1,itsig )*w01+ &
                   ges_prsi(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (ges_prsi(ix ,iy ,1,itsigp)*w00+ &
                   ges_prsi(ixp,iy ,1,itsigp)*w10+ &
                   ges_prsi(ix ,iyp,1,itsigp)*w01+ &
                   ges_prsi(ixp,iyp,1,itsigp)*w11)*dtsigp
       prsigesin2=(ges_prsi(ix ,iy ,2,itsig )*w00+ &
                   ges_prsi(ixp,iy ,2,itsig )*w10+ &
                   ges_prsi(ix ,iyp,2,itsig )*w01+ &
                   ges_prsi(ixp,iyp,2,itsig )*w11)*dtsig + &
                  (ges_prsi(ix ,iy ,2,itsigp)*w00+ &
                   ges_prsi(ixp,iy ,2,itsigp)*w10+ &
                   ges_prsi(ix ,iyp,2,itsigp)*w01+ &
                   ges_prsi(ixp,iyp,2,itsigp)*w11)*dtsigp
       call compute_fact10(ugesin,vgesin,tgesin,qgesin,pgesin, &
            prsigesin1,prsigesin2,ts,sfcrough,islimsk,factw)
    else if (sfcmod_mm5)then
       iqtflg=.true.
       lnpgesin1 =(ges_lnprsl(ix ,iy ,1,itsig )*w00+ &
                   ges_lnprsl(ixp,iy ,1,itsig )*w10+ &
                   ges_lnprsl(ix ,iyp,1,itsig )*w01+ &
                   ges_lnprsl(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (ges_lnprsl(ix ,iy ,1,itsigp)*w00+ &
                   ges_lnprsl(ixp,iy ,1,itsigp)*w10+ &
                   ges_lnprsl(ix ,iyp,1,itsigp)*w01+ &
                   ges_lnprsl(ixp,iyp,1,itsigp)*w11)*dtsigp
       lnpgesin2 =(ges_lnprsl(ix ,iy ,2,itsig )*w00+ &
                   ges_lnprsl(ixp,iy ,2,itsig )*w10+ &
                   ges_lnprsl(ix ,iyp,2,itsig )*w01+ &
                   ges_lnprsl(ixp,iyp,2,itsig )*w11)*dtsig + &
                  (ges_lnprsl(ix ,iy ,2,itsigp)*w00+ &
                   ges_lnprsl(ixp,iy ,2,itsigp)*w10+ &
                   ges_lnprsl(ix ,iyp,2,itsigp)*w01+ &
                   ges_lnprsl(ixp,iyp,2,itsigp)*w11)*dtsigp
       tgesin    =(ges_tv_itsig (ix ,iy ,1)*w00+ &
                   ges_tv_itsig (ixp,iy ,1)*w10+ &
                   ges_tv_itsig (ix ,iyp,1)*w01+ &
                   ges_tv_itsig (ixp,iyp,1)*w11)*dtsig + &
                  (ges_tv_itsigp(ix ,iy ,1)*w00+ &
                   ges_tv_itsigp(ixp,iy ,1)*w10+ &
                   ges_tv_itsigp(ix ,iyp,1)*w01+ &
                   ges_tv_itsigp(ixp,iyp,1)*w11)*dtsigp
       tgesin2   =(ges_tv_itsig (ix ,iy ,2)*w00+ &
                   ges_tv_itsig (ixp,iy ,2)*w10+ &
                   ges_tv_itsig (ix ,iyp,2)*w01+ &
                   ges_tv_itsig (ixp,iyp,2)*w11)*dtsig + &
                  (ges_tv_itsigp(ix ,iy ,2)*w00+ &
                   ges_tv_itsigp(ixp,iy ,2)*w10+ &
                   ges_tv_itsigp(ix ,iyp,2)*w01+ &
                   ges_tv_itsigp(ixp,iyp,2)*w11)*dtsigp
       qgesin2   =(ges_q_itsig (ix ,iy ,2)*w00+ &
                   ges_q_itsig (ixp,iy ,2)*w10+ &
                   ges_q_itsig (ix ,iyp,2)*w01+ &
                   ges_q_itsig (ixp,iyp,2)*w11)*dtsig + &
                  (ges_q_itsigp(ix ,iy ,2)*w00+ &
                   ges_q_itsigp(ixp,iy ,2)*w10+ &
                   ges_q_itsigp(ix ,iyp,2)*w01+ &
                   ges_q_itsigp(ixp,iyp,2)*w11)*dtsigp
       geopgesin =(geop_hgtl(ix ,iy ,1,itsig )*w00+ &
                   geop_hgtl(ixp,iy ,1,itsig )*w10+ &
                   geop_hgtl(ix ,iyp,1,itsig )*w01+ &
                   geop_hgtl(ixp,iyp,1,itsig )*w11)*dtsig + &
                  (geop_hgtl(ix ,iy ,1,itsigp)*w00+ &
                   geop_hgtl(ixp,iy ,1,itsigp)*w10+ &
                   geop_hgtl(ix ,iyp,1,itsigp)*w01+ &
                   geop_hgtl(ixp,iyp,1,itsigp)*w11)*dtsigp
       call SFC_WTQ_FWD (pgesin,ts,lnpgesin1,tgesin,qgesin,ugesin,vgesin, &
                lnpgesin2,tgesin2,qgesin2,geopgesin,sfcrough,islimsk, &
                factw,u10ges,v10ges,t2ges,q2ges,regime,iqtflg)
    endif

    return
  end subroutine comp_fact10


!-------------------------------------------------------------------------
   subroutine guess_grids_stats3d_(name,a,mype)
!-------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    guess_grids_stats3d_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    name
!    a
!    mype     - mpi task id
!
!   output argument log:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   use constants, only: zero
   use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_comm_world
   use gridmod, only: lon1,lat1,nsig

   implicit none

   character(len=*)             , intent(in   ) :: name
   real(r_kind),dimension(:,:,:), intent(in   ) :: a
   integer(i_kind)              , intent(in   ) :: mype


! local variables
   integer(i_kind) :: i,j,k
   real(r_kind),dimension(nsig+1):: work_a,work_a1
   real(r_kind),dimension(nsig):: amz ! global mean profile of a
   real(r_kind) :: rms

! start

!  Calculate global means for a

!  Calculate sums for a to estimate variance.
   work_a = zero
   do k = 1,nsig
      do j = 2,lon1+1
         do i = 2,lat1+1
            work_a(k) = work_a(k) + a(i,j,k)
         end do
      end do
   end do
   work_a(nsig+1)=real(lon1*lat1,r_kind)

   call mpi_allreduce(work_a,work_a1,nsig+1,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)

   amz=zero
   do k=1,nsig
      if (work_a1(nsig+1)>zero) amz(k)=work_a1(k)/work_a1(nsig+1)
      rms=sqrt(amz(k)**2/work_a1(nsig+1))
      if (mype==0) write(*,100) trim(name),k,amz(k),rms
   enddo
100 format(a,': Level, Global mean, RMS = ',i3,1P2E16.8)

   end subroutine guess_grids_stats3d_

!-------------------------------------------------------------------------
   subroutine guess_grids_stats2d_(name,a,mype)
!-------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    guess_grids_stats2d_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    name
!    a
!    mype     - mpi task id
!
!   output argument log:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   use constants, only: zero
   use mpimod, only: ierror,mpi_rtype,mpi_sum,mpi_comm_world
   use gridmod, only: lon1,lat1

   implicit none

   character(len=*)           , intent(in   ) :: name
   real(r_kind),dimension(:,:), intent(in   ) :: a
   integer(i_kind)            , intent(in   ) :: mype


! local variables
   integer(i_kind) :: i,j
   real(r_kind),dimension(2):: work_a,work_a1
   real(r_kind) :: amz, rms

! start

!  Calculate global means for a

!  Calculate sums for a to estimate variance.
   work_a = zero
   do j = 2,lon1+1
      do i = 2,lat1+1
         work_a(1) = work_a(1) + a(i,j)
      end do
   end do
   work_a(2)=real(lon1*lat1,r_kind)

   call mpi_allreduce(work_a,work_a1,2,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)

   amz=zero
   if (work_a1(2)>zero) amz=work_a1(1)/work_a1(2)
   rms=sqrt(amz**2/work_a1(2))      
   if (mype==0) write(*,100) trim(name),amz,rms
100 format(a,': Global mean, RMS = ',1P2E16.8)

   end subroutine guess_grids_stats2d_

!-------------------------------------------------------------------------
   subroutine pstats_(a,amiss,avg,rms)
!-------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pstats_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    amiss
!    a
!
!   output argument log:
!    avg,rms
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
   use constants, only: zero
   implicit none

   real(r_kind),dimension(:,:), intent(in   ) :: a      ! array var
   real(r_kind)               , intent(in   ) :: amiss  ! undef
   real(r_kind)               , intent(  out) :: avg,rms


! local variables
   integer(i_kind) :: i,j
   integer(i_kind) :: allcnt,cnt

! start

   allcnt=0
   cnt=0
   avg=zero
   rms=zero
   do i=1,size(a,1)
      do j=1,size(a,2)
         if(a(i,j)/=amiss) then
            cnt=cnt+1
            avg=avg+a(i,j)
         endif
         allcnt = allcnt+1
      end do
   end do
   avg=avg/max(1,cnt)
   rms=sqrt(avg*avg/max(1,cnt))      

   end subroutine pstats_

!-------------------------------------------------------------------------
   subroutine print1r8_(name,fld,undef)
!-------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    print1r8_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    name
!    fld 
!    undef
!
!   output argument log:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
   implicit none
   character(len=*)         , intent(in   ) :: name
   real(r_kind),dimension(:), intent(in   ) :: fld
   real(r_kind)             , intent(in   ) :: undef
! 
   write(6,100) trim(name),minval(fld),maxval(fld),sum(fld),undef
100 format(a,': range,sum = ',1P3E16.4)
   end subroutine print1r8_

!-------------------------------------------------------------------------
   subroutine print2r8_(name,fld,undef)
!-------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    print2r8_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    name
!    fld
!    undef
!
!   output argument log:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   implicit none
   character(len=*)           , intent(in   ) :: name
   real(r_kind),dimension(:,:), intent(in   ) :: fld
   real(r_kind)               , intent(in   ) :: undef
! 
   real(r_kind) avg,rms
   write(6,100) trim(name),minval(fld),maxval(fld),sum(fld)
   call pstats_(fld,UNDEF,avg,rms)
   write(6,99) trim(name),avg,rms
100 format(a,': range,sum = ',1P3E16.4)
99  format(a,': avg, rms = ',1P2E16.4)
   end subroutine print2r8_

!-------------------------------------------------------------------------
   subroutine print3r8_(name,fld,undef,allk)
!-------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    print3r8_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    name
!    fld
!    undef
!    allk
!
!   output argument log:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
   implicit none
   character(len=*)             , intent(in   ) :: name
   real(r_kind),dimension(:,:,:), intent(in   ) :: fld
   real(r_kind)                 , intent(in   ) :: undef
   logical,optional             , intent(in   ) :: allk
! 
   logical prntlevs
   integer(i_kind) k
   real(r_kind) avg,rms

   if(present(allk)) prntlevs=allk
   if(prntlevs) then
      do k=1,size(fld,3)
         write(6,101) trim(name),k,minval(fld(:,:,k)),maxval(fld(:,:,k)),sum(fld(:,:,k))
         call pstats_(fld(:,:,k),UNDEF,avg,rms)
         write(6,99) trim(name),avg,rms
      end do
   else
      write(6,100) trim(name),minval(fld),maxval(fld),sum(fld)
   end if
101 format(a,': time or lev,range,sum = ',i3,1P3E16.4)
100 format(a,': range,sum = ',1P3E16.4)
99  format(a,': avg, rms = ',1P2E16.4)
   end subroutine print3r8_

!-------------------------------------------------------------------------
   subroutine print4r8_(name,fld,undef,allk)
!-------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    print4r8_
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    name
!    fld
!    undef
!    allk
!
!   output argument log:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   implicit none
   character(len=*)               , intent(in   ) :: name
   real(r_kind),dimension(:,:,:,:), intent(in   ) :: fld
   real(r_kind)                   , intent(in   ) :: undef
   logical,optional               , intent(in   ) :: allk
! 
   logical prntlevs
   integer(i_kind) k,it
   real(r_kind) avg,rms

   if(present(allk)) prntlevs=allk
   if(prntlevs) then
      do it=1,size(fld,4)
         do k=1,size(fld,3)
            write(6,101) trim(name),it,k,minval(fld(:,:,k,it)),maxval(fld(:,:,k,it)),sum(fld(:,:,k,it))
            call pstats_(fld(:,:,k,it),UNDEF,avg,rms)
            write(6,99) trim(name),avg,rms
         end do
      end do
   else
      write(6,100) trim(name),minval(fld),maxval(fld),sum(fld)
   end if
101 format(a,': time,lev,range,sum = ',i3,i3,1P3E16.4)
100 format(a,': range,sum = ',1P3E16.4)
99  format(a,': avg, rms = ',1P2E16.4)
   end subroutine print4r8_
    
end module guess_grids
