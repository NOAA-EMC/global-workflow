module satthin
!$$$ subprogram documenation block
!                .      .    .                                       .
! subprogram:    satthin
!   prgmmr: treadon          org: np20                date: 2002-10-17
!
! abstract:  This module contains code which may be used to selectively
!            thin satellite data.
!
! program history log:
!   2002-10-17 treadon
!   2004-05-28  kleist, subroutine call update
!   2005-03-28  wu, remove unused variable mlath
!   2005-10-06  treadon - add routine destroy_sfc
!   2005-11-29  parrish - add routines getsfc_nmm_binary, getsfc_nmm_netcdf,
!                         getsfc_mass_binary, getsfc_mass_netcdf
!   2005-12-08  treadon - rename global getsfc to getsfc_global, add getsfc
!                         to branch to appropriate getsfc* routine
!   2006-02-01  parrish - remove all getsfc routines, and create new getsfc
!                         which makes full sfc fields from guess_grids.
!                         rename surface fields to sst_full,sno_full, etc.
!                         to eliminate conflict with some fields with same
!                         names in guess_grids.
!                         also add new sfc fields u10_full,v10_full
!   2006-05-23  parrish - add new sfc field zs_full, the model terrain height
!   2006-07-28  derber  - use r1000 from constants
!   2007-05-01  wu      - correct error in subroutine makegvals which incorrectly
!                         defines longitude range on regional grid when domain
!                         includes north pole.
!   2009-04-21  derber  - add ithin to call to makegrids - account for negative ithin
!   2009-08-19  guo     - added assertions of ntguessig and ntguessfc.
!   2009-09-14  guo     - added an experimenting description of the usecase.
!                       - added an an array size assertion on istart_val(:).
!   2011-04-01  li      - add getnst to read nst fields, add destroy_nst
!   2011-05-26  todling - add create_nst
!   2012-01-31  hchuang - add read_nemsnst in sub getnst
!   2012-03-05  akella  - remove create_nst,getnst and destroy_nst; nst fields now handled by gsi_nstcoupler
!   2015-05-01  li      - modify to use single precision for the variables read from sfc files
!   2016-08-18  li      - tic591: when use_readin_anl_sfcmask is true, 
!                                 add read sili_anl from analysis grid/resolution sfc file (sfcf06_anl) 
!                                 modify to use isli_anl
!                                 determine sno2 with interpolate, accordingly 
!                                 use the modified 2d interpolation (sfc_interpolate to intrp22)
!   2018-05-21  j.jin   - add an option for time-thinning. Check time preference (including thin4d) here. 
!
!   2019-07-09  todling - revisit Li''s shuffling of nst init, read and final routines
!   2019-08-08  j.jin   - add a comment block for an example of dtype-wise time-thinning
!                         configuration through an -info file.
!   2019-09-24  martin  - added in option for use_gfs_ncio for getsfc
!
! Subroutines Included:
!   sub makegvals      - set up for superob weighting
!   sub makegrids      - set up thinning grids
!   sub getsfc         - create full horizontal fields of surface arrays
!   sub map2tgrid      - map observation to location on thinning grid
!   sub destroygrids   - deallocate thinning grid arrays
!   sub destroy_sfc    - deallocate full horizontal surface arrays
!   sub indexx         - sort array into ascending order
!   sub tdiff2crit          - get time preference and time cell id in time-thinning
!   sub radthin_time_info   - read information for time-thinning.
!
! Usecase destription:
!     read_obs    -->  read_airs, etc
!   []_radthin_time_info                - read time interval
!   []_makegvals                        - set up for superob weighting
!   []_getsfc                           - create full horizontal fields of surface arrays
!                     []_makegrids      - set up thinning grids
!                     []_tdiff2crit     - get time preference and time cell id in time-thinning 
!                     []_map2tgrid      - map observation to location on thinning grid
!                     []_checkob        - intermediate ob checking to see if it should not be used
!                     []_finalcheck     - the final criterion check for sat obs and increments counters
!                     combine_radobs    - 
!                     []_destroygrids   - deallocate thinning grid arrays
!   []_destroy_sfc                      - deallocate full horizontal fields of surface arrays
!
! Variable Definitions:
!   def mlat           - number of latitudes in thinning grid
!   def mlon           - number of longitudes in thinning grid
!   def superp         - maximum number of data types
!   def maxthin        - maximum number of obs to retain in thinning grid box
!   def itxmax         - total number of points in thinning grid
!   def istart_val     - starting location on thinning grid for superobs
!   def icount         - observation flag  true - no obs in this thinning box previously
!                                          false - previous ob in box
!   def isli_full      - snow/land/ice mask
!   def glat           - latitudes on thinning grid
!   def super_val      - super obs factor across data types
!   def super_val1     - super obs factors summed over all mpi tasks (data types)
!   def glon           - longitudes on thinning grid
!   def hll            - (i,j) index of point on thinning grid
!   def sli_full       - 0=sea/1=land/2=ice mask
!   def sst_full       - skin temperature
!   def sno_full       - snow-ice mask
!   def isli_anl       - snow/land/ice mask mask at analysis grid resolution
!   def sno_anl        - snow-ice mask at analysis grid resolution
!   def zs_full        - model terrain elevation
!   def score_crit     - "best" quality obs score in thinning grid box
!   def use_all        - parameter for turning satellite thinning algorithm off
!
! With new time-thinning mechanism, one can configure time-thinning to be device
! specific through an -info. file, in this form,
!
! > rad_time_thinning_options::
! > ! ptime:      Time interval (hour) for thinning radiance and ozone data.
! > !             It defines the number of time thinning bins (time_window/ptime).
! > !             0, only one time thinning bin, by default.
! > ! ithin_time: Time preference is given 
! > !             1, (default) at the center time when thin4d=false, or 
! > !                observation time is ignored when thin4d=true. ptime must be 0.0;
! > !             2, at the center of time intervals (suppressing thin4d);
! > !             3, at the end of time intervals (suppressing thin4d);
! > !             4, at the beginning, middle, and end of the 1st, 2nd,and 3rd two-hour 
! > !                time interval, respectively. ptime must be 2.0 (suppressing thin4d);
! > !             5, select observations at random time, and ptime must be 0.0
! > !                (Only applicable to seviri data, May 2018).
! > ! ptime=0.0 and ithin_time=1 by default if the observation type is not listed here.
! > !dtype       dplat       dsis                  ptime   ithin_time
! >  seviri      m08         seviri_m08            2.0     4
! >  seviri      m09         seviri_m09            2.0     4
! >  seviri      m10         seviri_m10            2.0     4
! >  seviri      m11         seviri_m11            2.0     4
! > ::
!   
! details through an info file.  
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_quad,r_single
  use mpeu_util, only: die, perr
  use obsmod, only: time_window_max
  use constants, only: deg2rad,rearth_equator,zero,two,pi,half,one,&
       rad2deg,r1000
  use chemmod, only: laeroana_fv3smoke

  implicit none

! set default to private
  private
! set subroutines to public
  public :: makegvals
  public :: makegrids
  public :: getsfc
  public :: get_hsst
  public :: map2tgrid
  public :: destroygrids
  public :: destroy_sfc
  public :: indexx
  public :: radthin_time_info
  public :: tdiff2crit
! set passed variables to public
  public :: rlat_min,rlon_min,dlat_grid,dlon_grid,superp,super_val1,super_val
  public :: veg_type_full,soil_type_full,sfc_rough_full,sno_full,sst_full
  public :: fact10_full,isli_full,soil_moi_full,veg_frac_full,soil_temp_full
  public :: isli_anl,sno_anl
  public :: checkob,score_crit,itxmax,finalcheck,zs_full_gfs,zs_full
  public :: hsst

  integer(i_kind) mlat,superp,maxthin,itxmax
  integer(i_kind) itxmax0
  integer(i_kind), save:: itx_all
  integer(i_kind),dimension(0:51):: istart_val
  
  integer(i_kind),allocatable,dimension(:):: mlon
  logical,allocatable,dimension(:)::icount

  real(r_kind) rlat_min,rlat_max,rlon_min,rlon_max,dlat_grid,dlon_grid

  real(r_kind),   allocatable, dimension(:)     :: glat,score_crit
  real(r_kind),   allocatable, dimension(:)     :: super_val,super_val1
  real(r_kind),   allocatable, dimension(:,:)   :: glon,hll
  real(r_kind),   allocatable, dimension(:,:)   :: zs_full

! declare the dummy variables of routine read_gfssfc
  real(r_single), allocatable, dimension(:,:,:) :: fact10_full,sst_full,sno_full
  real(r_single), allocatable, dimension(:,:)   :: veg_type_full
  real(r_single), allocatable, dimension(:,:,:) :: veg_frac_full
  real(r_single), allocatable, dimension(:,:)   :: soil_type_full
  real(r_single), allocatable, dimension(:,:,:) :: soil_temp_full,soil_moi_full
  integer(i_kind),allocatable, dimension(:,:)   :: isli_full
  real(r_single), allocatable, dimension(:,:,:) :: sfc_rough_full
  real(r_single), allocatable, dimension(:,:)   :: zs_full_gfs
! declare the dummy variables of routine read_gfssfc_anl
  integer(i_kind),allocatable, dimension(:,:)   :: isli_anl
! declare local array sno_anl 
  real(r_single),allocatable, dimension(:,:,:)   :: sno_anl
! declare the dummy variables of routine berror_read_hsst
  real(r_single), allocatable, dimension(:,:) :: hsst

  logical use_all

contains

  subroutine makegvals
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    makegvals                            
!     prgmmr:    derber      org: np23                date: 2002-10-17
!
! abstract:  This routine allocates and initializes arrays
!            used for superobs weighting.
!
! program history log:
!   2002-10-17  derber
!   2004-06-22  treadon - update documentation
!   2004-12-09  treadon - allocate thinning grids consistent with analysis domain
!   2006-07-28  derber  - use r1000 from constants
!   2007-05-01  wu      - correct error which incorrectly defines longitude range 
!                         on regional grid when domain includes north pole.
!   2008-05-23  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2012-10-11  eliu/wu - make sure dlon_e is in the range of 0 and 360
!   2013-01-09  collard - simplify regional dlon_e range check
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: deg2rad,rearth_equator,zero,two,pi,half,one,&
       rad2deg,r1000
    use obsmod, only: dmesh,dthin,ndat
    use obsmod, only: dtype,dplat,dsis
    use gridmod, only: regional,nlat,nlon,txy2ll
    use mpeu_util, only: die
    implicit none

    real(r_kind),parameter:: r90   = 90.0_r_kind
    real(r_kind),parameter:: r360  = 360.0_r_kind
    real(r_kind),parameter:: r999  = 999.0_r_kind

    integer(i_kind) i,ii,j
    integer(i_kind) mlonx,icnt,mlony,mlonj
    real(r_kind) delat,dgv,dx,dy
    real(r_kind) twopi,dlon_g,dlat_g,dlon_e,dlat_e
    real(r_kind) factor,delon
    real(r_kind) rkm2dg,glatm,glatx
    integer(i_kind), allocatable, dimension(:) ::  n_tbin_m1
    integer(i_kind) :: n_tbin0
    real(r_kind)    :: ptime
    integer(i_kind) :: ithin_time

!   Initialize variables, set constants
    maxthin=0
    do i=1,ndat
       maxthin=max(maxthin,abs(dthin(i)))
    end do
!   Check if there are any time-thinning 
    allocate(n_tbin_m1(0:maxthin))
    n_tbin_m1 = 0
    do i=1,ndat
       call radthin_time_info( dtype(i), dplat(i), dsis(i), ptime, ithin_time)
       if( ptime > 0.0_r_kind ) then
         n_tbin0 = nint(2*time_window_max/ptime) - 1
         j=abs(dthin(i))
         n_tbin_m1(j)= max( n_tbin_m1(j), n_tbin0 )
       endif
    end do
    istart_val=0
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*r1000

!   Set lat,lon limits for analysis grid.
    rlat_min = -r90
    rlat_max = r90
    rlon_min = zero
    rlon_max = r360
    if (regional) then
       rlat_min =  r999
       rlat_max = -r999
       rlon_min =  r999
       rlon_max = -r999
       do j=1,nlon
          dlon_g=j
          do i=1,nlat
             dlat_g=i
             call txy2ll(dlon_g,dlat_g,dlon_e,dlat_e)
             dlat_e=dlat_e*rad2deg
             dlon_e=dlon_e*rad2deg
             if (dlon_e < zero) then
                dlon_e = MOD(dlon_e,-r360) + r360
             else if (dlon_e > r360) then
                dlon_e = MOD(dlon_e,r360)
             endif
             rlat_min = min(rlat_min,dlat_e)
             rlat_max = max(rlat_max,dlat_e)
             rlon_min = min(rlon_min,dlon_e)
             rlon_max = max(rlon_max,dlon_e)
          end do
       end do
    endif
    dlat_grid = rlat_max - rlat_min
    dlon_grid = rlon_max - rlon_min

    do ii=1,maxthin

!      Set up dimensions for thinning grids
       istart_val(ii+1)=istart_val(ii)
       if(abs(dmesh(ii)) > one)then
          dx    = dmesh(ii)*rkm2dg
          dy    = dx
          mlat  = dlat_grid/dy + half
          mlonx = dlon_grid/dx + half
          delat = dlat_grid/mlat
          dgv   = delat*half
          mlat=max(2,mlat);   mlonx=max(2,mlonx)

          icnt=0
          do j = 1,mlat
             glatx = rlat_min + (j-1)*delat
             glatx = glatx*deg2rad
             glatm = glatx + dgv*deg2rad
             
             factor = abs(cos(abs(glatm)))
             if (dmesh(ii)>zero) then
                mlonj = nint(mlonx*factor)
                mlony = max(2,mlonj)
             else
                delon = factor*dmesh(ii)
                delon = min(delon,r360)
                mlony = dlon_grid/delon
             endif
             do i = 1,mlony
                icnt=icnt+1
                istart_val(ii+1)=istart_val(ii+1)+1
             enddo

          enddo
          istart_val(ii+1) = istart_val(ii+1)+ & 
                             (istart_val(ii+1)-istart_val(ii))*n_tbin_m1(ii)
       end if
    end do
    superp=istart_val(maxthin+1)
    
!   Allocate and initialize arrays for superobs weighthing
    allocate(super_val(0:superp),super_val1(0:superp))
    do i=0,superp
       super_val(i)=zero
    end do
    deallocate(n_tbin_m1) 
    
    return
  end subroutine makegvals


  subroutine makegrids(rmesh,ithin,n_tbin,itxmax_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    makegrids                            
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine sets up dimensions for and allocates
!            thinning grids.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!   2004-12-09  treadon - allocate thinning grids consistent with analysis domain
!   2008-05-23  safford - rm unused vars
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2015-03-23  zaizhong ma - changed itxmax=1e9 for Himawari-8 ahi read in
!
!   input argument list:
!     rmesh - mesh size (km) of thinning grid.  If (rmesh <= one), 
!             then no thinning of the data will occur.  Instead,
!             all data will be used without thinning.
!     n_tbin - (optional) number of time intervals.
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: rearth_equator,deg2rad,zero,half,one,two,pi

    implicit none

    real(r_kind)   ,intent(in   ) :: rmesh
    integer(i_kind),intent(in   ) :: ithin
    integer(i_kind),intent(in   ), optional :: n_tbin
    integer(i_kind),intent(in   ), optional :: itxmax_in
    real(r_kind),parameter:: r360 = 360.0_r_kind
    integer(i_kind) i,j
    integer(i_kind) mlonx,mlonj
    real(r_kind) dgv,halfpi,dx,dy
    real(r_kind) twopi
    real(r_kind) factor,delon
    real(r_kind) rkm2dg,glatm
    real(r_quad) delat


!   If there is to be no thinning, simply return to calling routine
    use_all=.false.
    itx_all=0
    if(abs(rmesh) <= one .or. ithin <= 0)then
      use_all=.true.
      if (present(itxmax_in)) then
         itxmax = itxmax_in
      else
         itxmax = 1e7
      endif
      allocate(icount(itxmax))
      allocate(score_crit(itxmax))
      do j=1,itxmax
         icount(j) = .true.
         score_crit(j) = 9.99e10_r_kind
      end do
      return
    end if

!   Set constants
    halfpi = half*pi
    twopi  = two*pi
    rkm2dg = r360/(twopi*rearth_equator)*1.e3_r_kind

!   Set up dimensions and allocate arrays for thinning grids
    if (rmesh<zero) rkm2dg=one
    dx    = rmesh*rkm2dg
    dy    = dx
    mlat  = dlat_grid/dy + half
    mlonx = dlon_grid/dx + half
    delat = dlat_grid/mlat
    dgv  = delat*half
    mlat=max(2,mlat);   mlonx=max(2,mlonx)

    allocate(mlon(mlat),glat(mlat),glon(mlonx,mlat),hll(mlonx,mlat))


!   Set up thinning grid lon & lat.  The lon & lat represent the location of the
!   lower left corner of the thinning grid box.
    itxmax=0
    do j = 1,mlat
       glat(j) = rlat_min + (j-1)*delat
       glat(j) = glat(j)*deg2rad
       glatm = glat(j) + dgv*deg2rad

       factor = abs(cos(abs(glatm)))
       if (rmesh>zero) then
          mlonj   = nint(mlonx*factor)
          mlon(j) = max(2,mlonj)
          delon = dlon_grid/mlon(j)
       else
          delon = factor*rmesh
          delon = min(delon,r360)
          mlon(j) = dlon_grid/delon
       endif

       glat(j) = min(max(-halfpi,glat(j)),halfpi)
       do i = 1,mlon(j)
          itxmax=itxmax+1
          hll(i,j)=itxmax
          glon(i,j) = rlon_min + (i-1)*delon
          glon(i,j) = glon(i,j)*deg2rad
          glon(i,j) = min(max(zero,glon(i,j)),twopi)
       enddo

    end do

    if (present(n_tbin)) then
        itxmax0 = itxmax 
        itxmax  = itxmax0 * n_tbin
    endif

!   Allocate  and initialize arrays
    allocate(icount(itxmax))
    allocate(score_crit(itxmax))

    do j=1,itxmax
       icount(j) = .true.
       score_crit(j) = 9.99e10_r_kind
    end do

    return
  end subroutine makegrids

  subroutine get_hsst(mype_io)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_hsst
!     prgmmr:    Xu     org: np23                date: 2020-03-13
!
! abstract:  read hsst from berror_stats
  use gridmod, only:  nlat,nlon
  use ncepnems_io, only: berror_read_hsst
  integer(i_kind), intent(in) :: mype_io

  allocate(hsst(nlat,nlon))
  call berror_read_hsst(mype_io,hsst)

  end subroutine get_hsst

  subroutine getsfc(mype,mype_io,use_sfc,use_sfc_any)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    getsfc
!     prgmmr:    parrish     org: np23                date: 2006-02-02
!
! abstract:  This routine converts subdomain surface fields in
!            guess_grids to full horizontal fields for use in 
!            reading of observations.
!
! program history log:
!   2002-10-17  parrish
!   2008-12-05  todling - add ESMF blocks; update bias_tskin
!   2009-01-28  todling - remove reference to original GMAO interface
!   2010-04-01  treadon - move strip to gridmod
!   2013-10-19  todling - metguess now holds background
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2014-10-05  todling - revisit bkg bias-tskin; rename bkg-bias-related interface
!   2014-12-03  derber  - modify reading of surface fields
!   2015-05-01  li      - modify to handle the single precision sfc fields read from sfc file
!   2017-08-31  li      - modify to read a combined sfc & nst file
!                         (1) move gsi_nstcoupler_init and gsi_nstcoupler_read from read_obs.F90 to getsfc here
!                         (2) use sfcnst_comb from name list
!                         (3) modify subroutine getsfc to read a sfc & nst combined file
!   2019-09-24  martin  - added in option for use_gfs_ncio
!
!   input argument list:
!      mype        - current processor
!      mype_io     - surface IO processor
!      use_sfc     - true if processor uses extra surface fields
!      use_sfc_any - true if any processor uses extra surface fields
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,r_single
    use gridmod, only:  nlat,nlon,lat2,lon2,lat1,lon1,jstart,&
       iglobal,itotsub,ijn,displs_g,regional,istart, &
       rlats,rlons,nlat_sfc,nlon_sfc,rlats_sfc,rlons_sfc,strip,&
       sfcnst_comb,use_gfs_nemsio,use_readin_anl_sfcmask,use_gfs_ncio
    use general_commvars_mod, only: ltosi,ltosj
    use guess_grids, only: ntguessig,isli,sfct,sno,fact10, &
       nfldsfc,ntguessfc,soil_moi,soil_temp,veg_type,soil_type, &
       veg_frac,sfc_rough,nfldsig,isli2,sno2
    use m_gsiBiases, only: bkg_bias_model,bias_hour
    use jfunc, only: bcoption

    use mpimod, only: mpi_comm_world,ierror,mpi_rtype,mpi_rtype4
    use constants, only: zero,half,pi,two,one
    use ncepgfs_io, only: read_gfssfc,read_gfssfc_anl
    use ncepnems_io, only: read_nemssfc,intrp22,read_nemssfc_anl
    use netcdfgfs_io, only: read_gfsncsfc, read_gfsncsfc_anl
    use sfcio_module, only: sfcio_realfill
    use obsmod, only: lobserver
    use gsi_nstcouplermod, only: nst_gsi,gsi_nstcoupler_read
    use gsi_nstcouplermod, only: tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full,&
                                 c_0_full,c_d_full,w_0_full,w_d_full
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    implicit none

    integer(i_kind),intent(in   ) :: mype,mype_io
    logical        ,intent(in   ) :: use_sfc,use_sfc_any

! Local variables
    real(r_kind),dimension(lat1*lon1):: zsm
    real(r_kind),dimension(itotsub):: work1
    real(r_kind),dimension(lat2,lon2):: work2
    real(r_kind),dimension(nlat,nlon):: bias
    real(r_kind),dimension(nlat):: ailoc
    real(r_kind),dimension(nlon):: ajloc
    real(r_kind),allocatable,dimension(:)::wlatx,slatx
    real(r_kind) :: dlon, missing
    real(r_single),allocatable,dimension(:,:)::dum,work
    integer(i_kind) mm1,i,j,k,it,il,jl,jmax,idrt,istatus
    character(24) filename

    real(r_kind),dimension(:,:),pointer:: ges_z =>NULL()

    if( (ntguessig<1.or.ntguessig>nfldsig) .or. &
        (ntguessfc<1.or.ntguessfc>nfldsfc) ) then
        call perr('satthin.getsfc','ntguessig = ',ntguessig)
        call perr('satthin.getsfc','ntguessfc = ',ntguessfc)
        call die('satthin.getsfc')
    endif
    mm1=mype+1

!   if(mype == 0)write(6,*)'GETSFC:  enter with nlat_sfc,nlon_sfc=',nlat_sfc,nlon_sfc,&
!     ' and nlat,nlon=',nlat,nlon
    if(regional)then
       nlat_sfc=nlat
       nlon_sfc=nlon
    end if
!   if(mype == 0)write(6,*)'GETSFC: set nlat_sfc,nlon_sfc=',nlat_sfc,nlon_sfc
    allocate(rlats_sfc(nlat_sfc),rlons_sfc(nlon_sfc))

    allocate(isli_anl(nlat,nlon))
    allocate(sno_anl(nlat,nlon,nfldsfc))

    call create_sfc

    do j=1,lon1*lat1
       zsm(j)=zero
    end do


!  Global read
#ifndef HAVE_ESMF
    if (.not. regional) then
       if (nlon == nlon_sfc .and. nlat == nlat_sfc) then
          rlats_sfc=rlats
          rlons_sfc=rlons
       else
          idrt=4
          jmax=nlat_sfc-2
          allocate(slatx(jmax),wlatx(jmax))
          call splat(idrt,jmax,slatx,wlatx)
          dlon=two*pi/real(nlon_sfc,r_kind)
          do i=1,nlon_sfc
             rlons_sfc(i)=real(i-1,r_kind)*dlon
          end do
          do i=1,(nlat_sfc-1)/2
             rlats_sfc(i+1)=-asin(slatx(i))
             rlats_sfc(nlat_sfc-i)=asin(slatx(i))
          end do
          rlats_sfc(1)=-half*pi
          rlats_sfc(nlat_sfc)=half*pi
          deallocate(slatx,wlatx)
       end if

       if ( use_gfs_nemsio ) then

          if ( sfcnst_comb .and.  nst_gsi > 0  ) then
             call read_nemssfc(mype_io, &
                  sst_full,soil_moi_full,sno_full,soil_temp_full, &
                  veg_frac_full,fact10_full,sfc_rough_full, &
                  veg_type_full,soil_type_full,zs_full_gfs,isli_full,use_sfc_any,&
                  tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full,c_0_full,c_d_full,w_0_full,w_d_full)
          else
             call read_nemssfc(mype_io, &
                   sst_full,soil_moi_full,sno_full,soil_temp_full, &
                   veg_frac_full,fact10_full,sfc_rough_full, &
                   veg_type_full,soil_type_full,zs_full_gfs,isli_full,use_sfc_any)
          endif         ! if (  nst_gsi > 0 ) then

          if ( use_readin_anl_sfcmask ) then
             call read_nemssfc_anl(mype_io,isli_anl)
          endif

       else if ( use_gfs_ncio ) then

          if ( sfcnst_comb .and.  nst_gsi > 0  ) then
             call read_gfsncsfc(mype_io, &
                  sst_full,soil_moi_full,sno_full,soil_temp_full, &
                  veg_frac_full,fact10_full,sfc_rough_full, &
                  veg_type_full,soil_type_full,zs_full_gfs,isli_full,use_sfc_any,&
                  tref_full,dt_cool_full,z_c_full,dt_warm_full,z_w_full,c_0_full,c_d_full,w_0_full,w_d_full)
          else
             call read_gfsncsfc(mype_io, &
                   sst_full,soil_moi_full,sno_full,soil_temp_full, &
                   veg_frac_full,fact10_full,sfc_rough_full, &
                   veg_type_full,soil_type_full,zs_full_gfs,isli_full,use_sfc_any)
          endif         ! if (  nst_gsi > 0 ) then

          if ( use_readin_anl_sfcmask ) then
             call read_gfsncsfc_anl(mype_io,isli_anl)
          endif

       else
          call read_gfssfc(mype_io, &
             sst_full,soil_moi_full,sno_full,soil_temp_full, &
             veg_frac_full,fact10_full,sfc_rough_full, &
             veg_type_full,soil_type_full,zs_full_gfs,isli_full,use_sfc_any)

          if ( use_readin_anl_sfcmask ) then
             call read_gfssfc_anl(mype_io,isli_anl) 
          endif
       endif
!
!      read NSST variables while .not. sfcnst_comb (in sigio or nemsio)
!
       if (nst_gsi > 0 .and. .not. sfcnst_comb) then
          call gsi_nstcoupler_read(mype_io)         ! Read NST fields (each proc needs full NST fields)
       endif

       if(.not. use_sfc)then
          deallocate(soil_moi_full,soil_temp_full)
          deallocate(veg_frac_full,soil_type_full)
          deallocate(veg_type_full)
       end if

       if (bcoption>0) then
          if (mype==0) write(6,*)'GETSFC:   add bias correction to guess field ', trim(filename)
 
!         Correct Tskin over the full grid
          if(nlon == nlon_sfc .and. nlat == nlat_sfc)then
             call bkg_bias_model(work2,'sst',bias_hour,ierror)
             if (ierror==0) then ! successful application of bkg bias model ...
                call strip(work2,zsm)
                call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
                   work1,ijn,displs_g,mpi_rtype,&
                   mpi_comm_world,ierror)
 
                do k=1,iglobal
                   i=ltosi(k) ; j=ltosj(k)
                   bias(i,j)=nint(work1(k))
                end do
                do it=1,nfldsfc
                   do j=1,nlon
                      do i=1,nlat
                         sst_full(i,j,it)=sst_full(i,j,it)+bias(i,j)
                      end do
                   end do
                end do
             end if  ! check for successful application of bkg bias model
          else
             write(6,*)'satthin bias correction - incompatible surface resolution',&
                 nlon,nlon_sfc,nlat,nlat_sfc
             call stop2(82)
          end if
       end if

    else                   ! for regional 
#else /* HAVE_ESMF */
!
!      read NSST variables while .not. sfcnst_comb (in sigio or nemsio)
!
       if (nst_gsi > 0 .and. .not. sfcnst_comb) then
          call gsi_nstcoupler_read(mype_io)         ! Read NST fields (each proc needs full NST fields)
       endif
#endif /* HAVE_ESMF */

       it=ntguessfc
       rlats_sfc=rlats
       rlons_sfc=rlons

! isli_full
       do j=1,lon2
          do i=1,lat2
             work2(i,j)=isli(i,j,it)
          end do
       end do
       call strip(work2,zsm)
       call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
          work1,ijn,displs_g,mpi_rtype,&
          mpi_comm_world,ierror)
       do k=1,iglobal
          i=ltosi(k) ; j=ltosj(k)
          isli_full(i,j)=nint(work1(k))
       end do

! Fields with multiple time levels
       do it=1,nfldsfc

! sst_full
          call strip(sfct(:,:,it),zsm)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             sst_full(i,j,it)=work1(k)
          end do

! fact10_full
          call strip(fact10(:,:,it),zsm)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             fact10_full(i,j,it)=work1(k)
          end do

! sfc_rough_full
          call strip(sfc_rough(:,:,it),zsm)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             sfc_rough_full(i,j,it)=work1(k)
          end do

! sno_full
          call strip(sno(:,:,it),zsm)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             sno_full(i,j,it)=work1(k)
          end do

! veg_frac_full
          call strip(veg_frac(:,:,it),zsm)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          if(use_sfc)then
             do k=1,iglobal
                i=ltosi(k) ; j=ltosj(k)
                veg_frac_full(i,j,it)=work1(k)
             end do
          end if
! soil_temp_full
          call strip(soil_temp(:,:,it),zsm)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          if(use_sfc)then
             do k=1,iglobal
                i=ltosi(k) ; j=ltosj(k)
                soil_temp_full(i,j,it)=work1(k)
             end do
          end if


! soil_moi_full
          call strip(soil_moi(:,:,it),zsm)
          call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
             work1,ijn,displs_g,mpi_rtype,&
             mpi_comm_world,ierror)
          if(use_sfc)then
             do k=1,iglobal
                i=ltosi(k) ; j=ltosj(k)
                soil_moi_full(i,j,it)=work1(k)
             end do
          end if
       end do  !end do ntguessfc



! Now single time level surface fields
       it=ntguessfc
! soil_type_full
       call strip(soil_type(:,:,it),zsm)
       call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
          work1,ijn,displs_g,mpi_rtype,&
          mpi_comm_world,ierror)
       if(use_sfc)then
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             soil_type_full(i,j)=work1(k)
          end do
       end if

! veg_type_full
       call strip(veg_type(:,:,it),zsm)
       call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
          work1,ijn,displs_g,mpi_rtype,&
          mpi_comm_world,ierror)
       if(use_sfc)then
          do k=1,iglobal
             i=ltosi(k) ; j=ltosj(k)
             veg_type_full(i,j)=work1(k)
          end do
       end if

#ifndef HAVE_ESMF
    end if                        ! if (.not. regional) then
#endif /* HAVE_ESMF */

! Now stuff that isn't model dependent
! zs_full
    it=ntguessig
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z',ges_z,istatus)
    if (istatus==0) then
       do j=1,lon1*lat1
          zsm(j)=zero
       end do
       call strip(ges_z,zsm)
       call mpi_allgatherv(zsm,ijn(mm1),mpi_rtype,&
          work1,ijn,displs_g,mpi_rtype,&
          mpi_comm_world,ierror)
       do k=1,iglobal
          i=ltosi(k) ; j=ltosj(k)
          zs_full(i,j)=work1(k)
       end do
    endif

!   old gfs surface files do not have a terrain record.
!   therefore, need to interpolate the terrain from the atm
!   grid to the surface grid, which may not be the same.
    if (.not. regional) then
       missing = sfcio_realfill + one
       if (maxval(zs_full_gfs) < missing) then
          if (nlon == nlon_sfc .and. nlat == nlat_sfc) then
             zs_full_gfs = zs_full
          else
             allocate(dum(nlat_sfc,nlon_sfc))
             allocate(work(nlat,nlon))
             work = zs_full
             call intrp22(work,rlons,rlats,nlon,nlat, &
                          dum, rlons_sfc,rlats_sfc,nlon_sfc,nlat_sfc)
             zs_full_gfs = dum
             deallocate(dum)
             deallocate(work)
          endif
       endif
    endif                 

!   find subdomain for isli2
    if (nlon == nlon_sfc .and. nlat == nlat_sfc) then
       do j=1,lon2
          jl=j+jstart(mm1)-2
          jl=min0(max0(1,jl),nlon)
          do i=1,lat2
             il=i+istart(mm1)-2
             il=min0(max0(1,il),nlat)
             isli2(i,j)=isli_full(il,jl)
             do k=1,nfldsfc
                sno2(i,j,k)=sno_full(il,jl,k)
             end do
          end do
       end do
    else

       if ( use_readin_anl_sfcmask ) then
          do k = 1, nfldsfc
             call intrp22(sno_full(:,:,k),rlons_sfc,rlats_sfc,nlon_sfc,nlat_sfc, &
                          sno_anl (:,:,k),rlons,rlats,nlon,nlat)
          enddo
          do j=1,lon2
             jl=j+jstart(mm1)-2
             jl=min0(max0(1,jl),nlon)
             do i=1,lat2
                il=i+istart(mm1)-2
                il=min0(max0(1,il),nlat)
                isli2(i,j)=isli_anl(il,jl)
                do k=1,nfldsfc
                   sno2(i,j,k)=sno_anl(il,jl,k)
                   if ( isli2(i,j) == 0 ) then
                      sno2(i,j,k) = zero
                   endif
                end do
             end do
          end do

       else

          ailoc=rlats
          ajloc=rlons
          call grdcrd(ailoc,nlat,rlats_sfc,nlat_sfc,1)
          call grdcrd(ajloc,nlon,rlons_sfc,nlon_sfc,1)
          do j=1,lon2
             jl=j+jstart(mm1)-2
             jl=min0(max0(1,jl),nlon)
             jl=nint(ajloc(jl))
             jl=min0(max0(1,jl),nlon_sfc)
             do i=1,lat2
                il=i+istart(mm1)-2
                il=min0(max0(1,il),nlat)
                il=nint(ailoc(il))
                il=min0(max0(1,il),nlat_sfc)
                isli2(i,j)=isli_full(il,jl)
                do k=1,nfldsfc
                   sno2(i,j,k) =sno_full(il,jl,k)
                end do
             end do
          end do
       end if

    end if
    if (.not.lobserver) then
       if(allocated(veg_frac)) deallocate(veg_frac)
!       veg_type will be used in setuppm2_5.f90 for rrfs_sd PM2.5 DA
       if(.not. laeroana_fv3smoke )then
         if(allocated(veg_type)) deallocate(veg_type)
       endif
       if(allocated(soil_type)) deallocate(soil_type)
       if(allocated(soil_moi)) deallocate(soil_moi)
       if(allocated(sfc_rough)) deallocate(sfc_rough)
    endif
    return

  end subroutine getsfc

  subroutine map2tgrid(dlat_earth,dlon_earth,dist1,crit1,itx,ithin,itt,iuse,sis,it_mesh)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    map2tgrid
!     prgmmr:    derber      org: np2                 date: 2006-05-03
!
! abstract:  This routine maps observations to the thinning grid.
!
! program history log:
!   2006-05-03  derber (created from map2grids)
!   2006-09-13  treadon - set itx=1 for the case use_all=.true.
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!
!   input argument list:
!     dlat_earth - earth relative observation latitude (radians)
!     dlon_earth - earth relative observation longitude (radians)
!     crit1      - quality indicator for observation (smaller = better)
!     ithin      - number of obs to retain per thinning grid box
!     sis        - sensor/instrument/satellite
!     it_mesh    - time meth id 
!
!   output argument list:
!     itx   - combined (i,j) index of observation on thinning grid
!     itt   - superobs thinning counter
!     iuse  - .true. if observation should be used
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: one, half
    implicit none

    logical        ,intent(  out) :: iuse
    integer(i_kind),intent(in   ) :: ithin
    integer(i_kind),intent(  out) :: itt,itx
    real(r_kind)   ,intent(in   ) :: dlat_earth,dlon_earth
    real(r_kind)   ,intent(inout) :: crit1
    real(r_kind)   ,intent(  out) :: dist1
    character(20)  ,intent(in   ) :: sis
    integer(i_kind),intent(in   ), optional :: it_mesh

    integer(i_kind) ix,iy
    real(r_kind) dlat1,dlon1,dx,dy,dxx,dyy


!   If using all data (no thinning), simply return to calling routine
    if(use_all .or. ithin <= 0)then
       iuse=.true.
       itt=1
       dist1=one
       if(itx_all < itxmax) then
          itx_all=itx_all+1
       else
          iuse = .false.
          write(6,*)'MAP2TGRID:  ndata > maxobs when reading data for ',sis,itxmax
       end if
       itx=itx_all
       return
    end if

!   Compute (i,j) indices of coarse mesh grid (grid number 1) which 
!   contains the current observation.
    dlat1=dlat_earth
    dlon1=dlon_earth

    call grdcrd1(dlat1,glat,mlat,1)
    iy=int(dlat1)
    dy=dlat1-iy
    iy=max(1,min(iy,mlat))

    call grdcrd1(dlon1,glon(1,iy),mlon(iy),1)
    ix=int(dlon1)
    dx=dlon1-ix
    ix=max(1,min(ix,mlon(iy)))

    dxx=half-min(dx,one-dx)
    dyy=half-min(dy,one-dy)
    dist1=dxx*dxx+dyy*dyy+half
    itx=hll(ix,iy)
!   time mesh
    if( present(it_mesh)  ) then
       itx=itx+it_mesh*itxmax0
    endif
    itt=istart_val(ithin)+itx
    if(ithin == 0) itt=0

!   Increment obs counter on coarse mesh grid.  Also accumulate observation
!   score and distance functions

!   ratio=1.e9
!   if ( dx > zero ) ratio=dy/dx
!   dista=sin(two*atan(ratio))
!   distb=sin(pi*dx)                !dista+distb is max at grid box center
!   dist1=one - quarter*(dista + distb)  !dist1 is min at grid box center and 
                                    !ranges from 1 (at corners)to 
                                    !.5 (at center of box)
    iuse=.true.

    if(dist1*crit1 > score_crit(itx) .and. icount(itx))iuse=.false.

    return
  end subroutine map2tgrid

  subroutine checkob(dist1,crit1,itx,iuse)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    checkob
!     prgmmr:    checkob     org: np23                date: 2002-10-17
!
! abstract:  intermediate ob checking routine to see if we should not use.
!
! program history log:
!   2006-05-03  derber
!
!   input argument list:
!     dist1  - quality indicator for distance (smaller = better)
!     crit1      - quality indicator for observation (smaller = better)
!     itx   - combined (i,j) index of observation on thinning grid
!     iuse  - .true. if observation should be used
!
!   output argument list:
!     iuse  - .true. if observation should be used
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none

    logical        ,intent(inout) :: iuse
    integer(i_kind),intent(in   ) :: itx
    real(r_kind)   ,intent(in   ) :: dist1,crit1

!   If using all data (no thinning), simply return to calling routine
    if(use_all .or. .not. iuse .or. icount(itx))return

    if(crit1*dist1 > score_crit(itx))iuse=.false.

    return
  end subroutine checkob

  subroutine finalcheck(dist1,crit1,itx,iuse)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    finalcheck
!     prgmmr:    derber     org: np23                date: 2002-10-17
!
! abstract:  This routine performs the final criterion check for sat 
!              obs and increments counters
!
! program history log:
!   2006-05-03  derber
!
!   input argument list:
!     dist1  - quality indicator for distance (smaller = better)
!     crit1  - quality indicator for observation (smaller = better)
!     itx    - combined (i,j) index of observation on thinning grid
!
!   output argument list:
!     iuse   - .true. if observation should be used
!     
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none
    logical        ,intent(inout) :: iuse
    integer(i_kind),intent(in   ) :: itx
    real(r_kind)   ,intent(in   ) :: dist1,crit1

    real(r_kind) crit

!   If not using data, simply return to calling routine
    if(.not. iuse)return

    crit=crit1*dist1

!   Increment obs counter

    if(icount(itx) .or. crit < score_crit(itx))then
       score_crit(itx)= crit
       icount(itx)=.false.
    else
       iuse = .false.
    end if

    return
  end subroutine finalcheck


  subroutine destroygrids
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroygrids
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This deallocate arrays used in thinning of satellite data.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    implicit none


    deallocate(icount)
    deallocate(score_crit)

!   If using all data (no thinning), the arrays following the
!   return are never allocated --> therefore nothing to deallocate
    if(use_all) return

    deallocate(mlon,glat,glon,hll)
    return
  end subroutine destroygrids

  subroutine create_sfc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_sfc
!     prgmmr:    todling     org: np23            date: 2019-07-09
!
! abstract:  This deallocate surface arrays
!
! program history log:
!   2019=07-09  todling
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use gridmod, only: nlat,nlon,nlat_sfc,nlon_sfc
    use guess_grids, only: nfldsfc
    use gsi_nstcouplermod, only: nst_gsi,gsi_nstcoupler_init,gsi_nstcoupler_read
    implicit none

#ifndef HAVE_ESMF
    allocate(zs_full_gfs(nlat_sfc,nlon_sfc))
#endif /* HAVE_ESMF */
    allocate(sfc_rough_full(nlat_sfc,nlon_sfc,nfldsfc))
    allocate(zs_full(nlat,nlon))
    allocate(soil_moi_full(nlat_sfc,nlon_sfc,nfldsfc))
    allocate(soil_temp_full(nlat_sfc,nlon_sfc,nfldsfc))
    allocate(veg_frac_full(nlat_sfc,nlon_sfc,nfldsfc))
    allocate(soil_type_full(nlat_sfc,nlon_sfc))
    allocate(veg_type_full(nlat_sfc,nlon_sfc))
    allocate(isli_full(nlat_sfc,nlon_sfc))
    allocate(fact10_full(nlat_sfc,nlon_sfc,nfldsfc))
    allocate(sno_full(nlat_sfc,nlon_sfc,nfldsfc))
    allocate(sst_full(nlat_sfc,nlon_sfc,nfldsfc))

!   Create full horizontal nst arrays
    if (nst_gsi > 0) call gsi_nstcoupler_init()

    return
  end subroutine create_sfc

  subroutine destroy_sfc
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_sfc
!     prgmmr:    treadon     org: np23                date: 2005-10-06
!
! abstract:  This deallocate surface arrays
!
! program history log:
!   2005-10-06  treadon
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use gsi_nstcouplermod, only: nst_gsi,gsi_nstcoupler_final
    implicit none

    if (nst_gsi > 0) call gsi_nstcoupler_final()

    if(allocated(sst_full))deallocate(sst_full)
    if(allocated(sno_full))deallocate(sno_full)
    if(allocated(fact10_full))deallocate(fact10_full)
    if(allocated(isli_full))deallocate(isli_full)
    if(allocated(veg_type_full))deallocate(veg_type_full)
    if(allocated(soil_type_full))deallocate(soil_type_full)
    if(allocated(veg_frac_full))deallocate(veg_frac_full)
    if(allocated(soil_temp_full))deallocate(soil_temp_full)
    if(allocated(soil_moi_full))deallocate(soil_moi_full)
    if(allocated(zs_full))deallocate(zs_full)
    if(allocated(sfc_rough_full))deallocate(sfc_rough_full)
#ifndef HAVE_ESMF
    if(allocated(zs_full_gfs)) deallocate(zs_full_gfs)
#endif /* HAVE_ESMF */

    return
  end subroutine destroy_sfc

  subroutine indexx(n,arr,indx)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    indexx
!     prgmmr:    treadon     org: np23                date: 2002-10-17
!
! abstract:  This routine indexes a sort key array, arr, such that
!            arr(indx(i),i=1,n) is the sort key in ascending order.
!
! program history log:
!   2002-10-17  treadon
!   2004-06-22  treadon - update documentation
!
!   input argument list:
!     n    - size of sort key array
!     arr  - sort key array
!
!   output argument list:
!     indx - index array
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_double
    use constants, only: one
    implicit none

    integer(i_kind) maxblock
    parameter (maxblock=1000)

    integer(i_kind),intent(in   ) :: n
    real(r_kind)   ,intent(in   ) :: arr(n)

    integer(i_kind),intent(  out) :: indx(n)

#ifdef ibm_sp
    real(r_kind),dimension(maxblock)::work
    real(r_double) oned

    oned=1._r_double
    if (digits(one)<digits(oned)) then
       call ssorts(arr,1,n,indx,work,maxblock)
    else
       call dsorts(arr,1,n,indx,work,maxblock)
    endif
#else
    integer(i_kind) m,nstack
    parameter (m=7,nstack=500)
    integer(i_kind) i,indxt,ir,itemp,j,jstack,k,l,istack(nstack)
    real(r_kind) a
    
    do j=1,n
       indx(j)=j
    end do
    jstack=0
    l=1
    ir=n
    
    loop0: do 
    
       if(ir-l<m)then
          loop: do j=l+1,ir
             indxt=indx(j)
             a=arr(indxt)
             do i=j-1,l,-1
                if(arr(indx(i))<=a)then   
                   indx(i+1)=indxt
                   cycle loop
                end if
                indx(i+1)=indx(i)
             end do
             i=l-1
             indx(i+1)=indxt
          end do loop
          if(jstack==0)return
          ir=istack(jstack)
          l=istack(jstack-1)
          jstack=jstack-2
          
       else
          k=(l+ir)/2
          itemp=indx(k)
          indx(k)=indx(l+1)
          indx(l+1)=itemp
          if(arr(indx(l))>arr(indx(ir)))then
             itemp=indx(l)
             indx(l)=indx(ir)
             indx(ir)=itemp
          endif
          if(arr(indx(l+1))>arr(indx(ir)))then
             itemp=indx(l+1)
             indx(l+1)=indx(ir)
             indx(ir)=itemp
          endif
          if(arr(indx(l))>arr(indx(l+1)))then
             itemp=indx(l)
             indx(l)=indx(l+1)
             indx(l+1)=itemp
          endif
          i=l+1
          j=ir
          indxt=indx(l+1)
          a=arr(indxt)
          loop1: do
            i=i+1
            if(arr(indx(i))<a)cycle loop1
          
            loop2: do 
               j=j-1
               if(arr(indx(j))<=a)exit loop2
            end do loop2
            if(j<i)exit loop1
            itemp=indx(i)
            indx(i)=indx(j)
            indx(j)=itemp
          end do loop1
       
          indx(l+1)=indx(j)
          indx(j)=indxt
          jstack=jstack+2
          if(jstack>nstack)then
             write(6,*)'INDEXX:  nstack=',nstack,' too small in indexx'
             call stop2(32)
          endif
          if(ir-i+1>=j-l)then
             istack(jstack)=ir
             istack(jstack-1)=i
             ir=j-1
          else
             istack(jstack)=j-1
             istack(jstack-1)=l
             l=i
          endif
       endif
    end do loop0
#endif
  end subroutine indexx

  subroutine tdiff2crit(tdiff,ptime,ithin_time,timeinflat,crit0,crit1,it_mesh)

!$$$ 
! Abstract:  Get time preference and time cell id in time-thinning.
! Program history log:
!   2018-05-18   j.jin      - initial code.
! 
!$$$ 
!Inputs
!     tdiff         - observational time minus gsttime.
!     ptime         - thinning time interval
!     ltin_time     - id for time preference
!     timeinflat    - a factor to inflat time difference.
!     crit0         - an added value to crit
!Outputs
!     crit1         - thinning crit
!     it_mesh       - time cell id 

    use constants, only: tiny_r_kind
    use gsi_4dvar, only: thin4d 
    implicit none
    integer(i_kind),intent(in   ) :: ithin_time
    real(r_kind)   ,intent(in   ) :: tdiff,ptime,timeinflat
    real(r_kind)   ,intent(in   ) :: crit0
    real(r_kind)   ,intent(out  ) :: crit1
    integer(i_kind),intent(out  ) :: it_mesh

    real(r_kind)     :: crita, critb, ptimeb, crit0_

    if( ptime > 0.0_r_kind) then
       crita=min(tdiff, time_window_max-tiny_r_kind)
       it_mesh=int((crita+time_window_max)/ptime)
       ptimeb=ptime
    else
       it_mesh=0
       ptimeb=2*time_window_max
    endif
    critb=tdiff+time_window_max

    select case (ithin_time)
       case (1)
          if (thin4d) then
             crit1=zero
          else
             crit1=abs(tdiff)   ! .eqv. ithin_time==5
          endif
       case (2)
          crit1=abs(critb-(it_mesh+0.5_r_kind)*ptimeb)
       case (3)
          crit1=abs(critb-(it_mesh+1_r_kind)*ptimeb)
       case (4) 
          crit1=abs(critb-it_mesh*time_window_max)
       case (5)
          crit1=abs(tdiff)      ! .eqv. ithin_time==1 .and. .not.thin4d
    end select

    crit0_=crit0
    crit0_=max(crit0_, 0.01_r_kind)     ! This fixes a problem in some
                                        ! obs-reader code, where a minimum
                                        ! crit0 is set to 0, such that obs
                                        ! thinning is limitted to a first-
                                        ! come-first-serve situation.
    crit1=crit0_+crit1*timeinflat
  end subroutine tdiff2crit

  subroutine radthin_time_info(obstype, platid, sis, ptime, ithin_time)

!$$$ 
! Abstract:  Read time-thinning options for radiance and ozone data.
! Program history log:
!   2018-05-10   j.jin      - initial code.
! 
!$$$ 

! Inputs
!   obstype     - observation type to process
!   platid      - satellite indicator
!   sis         - satellite_instrument/sensor indicator
! Outputs
!   ptime       - time interval 
!   ithin_time  - indicator of time preference  
!
  use kinds, only: r_kind,i_kind
  use file_utility, only: get_lun
  use mpeu_util, only: gettablesize, gettable, die

  character(len=*),intent(in):: obstype,platid,sis
  real(r_kind),intent(out)   :: ptime
  integer(i_kind),intent(out):: ithin_time

  character(len=*),parameter:: rcname='anavinfo' 
  character(len=*),parameter:: tbname='rad_time_thinning_options::'
  integer(i_kind) luin,ii,ntot, nvars
  character(len=256),allocatable,dimension(:):: utable
  character(len=20) :: dtype_info, dplat_info, dsis_info
  real(r_kind)   :: ptime_info
  integer(i_kind):: ithin_time_info


! default outputs
  ptime=0.0_r_kind
  ithin_time=1

! load file
  luin=get_lun()
  open(luin,file=rcname,form='formatted')
! Scan file for desired table first
! and get size of table
  call gettablesize(tbname,luin,ntot,nvars)
  if(nvars<=0) then
     close(luin)
     return
  endif
! Get contents of table
  allocate(utable(nvars))
  call gettable(tbname,luin,ntot,nvars,utable)
! release file unit
  close(luin)

  do ii=1, nvars
     read(utable(ii),*) dtype_info, dplat_info, dsis_info, ptime_info, ithin_time_info
     if( obstype == trim(dtype_info) ) then
        if( platid == trim(dplat_info) ) then
            if( sis == trim(dsis_info) ) then
               ptime = ptime_info
               ithin_time = ithin_time_info
            endif
        endif
     endif
  enddo
  deallocate(utable)

! Check the settings
  if( ithin_time == 1 .or. ithin_time == 5 ) then 
     if( ptime /= 0.0_r_kind ) then
        call die("satthin.F90 (subroutine radthin_time_info)", &
            "ithin_time=1 or 5 requires ptime=0.0"  )
     endif
  else if( ithin_time == 4) then 
     if( ptime /= 2.0_r_kind .or. time_window_max /= 3.0_r_kind ) then
        call die("satthin.F90 (subroutine radthin_time_info)", &
            "ithin_time=4 requires ptime=2.0 and time_window_max=3.0" )
     endif
  endif

  end subroutine radthin_time_info

end module satthin
