module ncepnems_io
!$$$ module documentation block
!           .      .    .                                       .
! module:   ncepnems_io
!   prgmmr: Huang       org: np23                date: 2010-02-22
!
! abstract: This module contains routines which handle input/output
!           operations for NCEP NEMS global atmospheric and surface files.
!
! program history log:
!   2010-02-22 Huang    Initial version.  Based on ncepgfs_io
!   2010-10-18 Huang    Remove subroutine reorder_gfsgrib for no longer been called in GSI
!                       For Now, subroutine sfc_interpolate is kept in ncepgfs_io.f90.
!                       When sigio and gfsio are both retired, i.e., remove ncepgfs_io.f90.
!                       move this routines back to this module
!   2011-03-03 Huang    Changes has been made to adopt to high resolution GSI run (T382 & T574)
!                       both for CPU and memory issues.
!                       Future development of nemsio need to consider a mapping routine be
!                       inserted between read-in forecast field and GSI first guess field,
!                       as well as GSI analysis field and write-out data field for forecast
!                       model. Due to computation resource, GSI may not be able to run at
!                       the same resolution as that of forecast model, e.g., run GSI at T382
!                       w/ T574 forecast model output.
!   2011-10-25 Huang    (1) Add unified error message routine to make the code cleaner
!                       (2) To reduce the memory allocation as low as possible, remove all
!                           reference to sfc_head and re-used the same local arrays.
!                           Remove unneeded nemsio_data & gfsdata.
!                       (3) Add parallel IO code to read_atm_
!   2011-11-01 Huang    (1) add integer nst_gsi to control the mode of NSST
!                       (2) add read_nemsnst to read ncep nst file
!                       (3) add subroutine write_nemssfc_nst to save sfc and nst files
!   2016-01-01 Li       (1) Move tran_gfssfc from ncepgfs_io.f90 to here
!                       (2) Modify write_sfc_nst_ to follows the update done in sfcio
!                       (3) Modify read_sfc_ to follows the update done in sfcio for more effective I/O
!   2016-04-20 Li       Modify to handle the updated nemsio sig file (P, DP & DPDT removed)
!   2016-08-18 li     - tic591: add read_sfc_anl & read_nemssfc_anl to read nemsio sfc file (isli only) with analysis resolution
!                               change/modify sfc_interpolate to be intrp22 to handle more general interpolation (2d to 2d)
!
! Subroutines Included:
!   sub read_nems       - driver to read ncep nems atmospheric and surface
!   sub read_nems_chem
!   sub read_nemsatm    - read ncep nems atmospheric file, scatter
!                         on grid to analysis subdomains
!   sub read_nemssfc    - read ncep nems surface file, scatter on grid to 
!                         analysis subdomains
!   sub read_nemssfc_anl- read ncep EnKF nems surface file, scatter on grid to 
!                         analysis subdomains
!   sub write_nems      - driver to write ncep nems atmospheric and surface
!                         analysis files
!   sub write_nemsatm   - gather on grid, write ncep nems atmospheric analysis file
!   sub write_nemssfc   - gather/write on grid ncep surface analysis file
!   sub read_nemsnst    - read ncep nst file, scatter on grid to analysis subdomains
!   sub write_nems_sfc_nst - gather/write on grid ncep surface & nst analysis file
!   sub intrp22         - interpolate from one grid to another grid (2D)
!
! Variable Definitions:
!   The difference of time Info between operational GFS IO (gfshead%, sfc_head%),
!      analysis time (iadate), and NEMSIO (idate=)
!
!       gfshead & sfc_head            NEMSIO Header           Analysis time (obsmod)    
!       ===================   ============================  ==========================
!         %idate(1)  Hour     idate(1)  Year                iadate(1)  Year
!         %idate(2)  Month    idate(2)  Month               iadate(2)  Month
!         %idate(3)  Day      idate(3)  Day                 iadate(3)  Day
!         %idate(4)  Year     idate(4)  Hour                iadate(4)  Hour
!                             idate(5)  Minute              iadate(5)  Minute
!                             idate(6)  Scaled seconds
!                             idate(7)  Seconds multiplier
!
!   The difference of header forecasting hour Info bewteen operational GFS IO
!      (gfshead%, sfc_head%) and NEMSIO
!
!           gfshead & sfc_head                NEMSIO Header       
!       ==========================     ============================
!       %fhour  FCST Hour (r_kind)     nfhour     FCST Hour (i_kind)
!                                      nfminute   FCST Mins (i_kind)
!                                      nfsecondn  FCST Secs (i_kind) numerator
!                                      nfsecondd  FCST Secs (i_kind) denominator
!
!       %fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
!
!   nframe     - nframe is the number of grids extend outward from the
!                edge of modeling domain.
!
!                NEMSIO provides a more flexible read.  User can get the
!                size of record (1D) to be read from file header. The
!                normal record size should be delx*dely, i.e., total model
!                grid points.  However, some regional models also ouput
!                additional data of grids around the modeling domain 
!                (buffer zone). For this type of output, nframe needs to
!                be know to calculate the size of record, i.e.,
!                   array size = (delx+2*nframe) * (dely+2*nframe)
!
!                However, nframe should always be zero for global model.
!                To simplify the code for reading and writing global model
!                files, we will not factor in the nframe for computing
!                array size or array index shift (by nframe) between 
!                input/output array and internal GSI array.  The normal
!                size of I/O record remains as delx*dely.  Add a checking
!                routine to assure nframe=zero.
!
! attributes:
!   language: f90
!   machine:
!
! NOTE: When global meteorology switched to NEMS/GFS, all routines and 
!       modules of old GFS (sigio) can be deactivated.  To keep the code
!       clean, all "nems" can be replaced by "gfs" for minimal changes 
!       of GSI code structure.  For dual purpose, two distincit routine
!       names are used to accomodiate old and new systems.  It is now 
!       controled by a namelist argument "use_gfs_nemsio"
!
!
!$$$ end documentation block

  use constants, only: zero,one,fv,r60,r3600
  implicit none

  private
  public read_nems
  public read_nems_chem
  public read_nemsatm
  public read_nemssfc
  public read_nemssfc_anl
  public write_nemsatm
  public write_nemssfc
  public read_nemsnst
  public write_nems_sfc_nst
  public intrp22
  public tran_gfssfc
  public error_msg

  interface read_nems
     module procedure read_
  end interface

  interface read_nems_chem
     module procedure read_chem_
  end interface

  interface read_nemsatm
     module procedure read_atm_
  end interface

  interface read_nemssfc
     module procedure read_nemssfc_
  end interface

  interface read_nemssfc_anl
     module procedure read_nemssfc_anl_
  end interface

  interface read_nemsnst
     module procedure read_nemsnst_
  end interface

  interface write_nemsatm
     module procedure write_atm_
  end interface

  interface write_nemssfc
     module procedure write_sfc_
  end interface

  interface write_nems_sfc_nst
     module procedure write_sfc_nst_
  end interface

  interface error_msg
     module procedure error_msg_
  end interface

  character(len=*),parameter::myname='ncepnems_io'

contains

  subroutine read_
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_nems
!
!   prgrmmr: Ho-Chun Huang
!
! abstract:
!
! program history log:
!   2010-03-31  Huang   - create routine based on read_gfs
!   2010-10-19  Huang   - remove spectral part for gridded NEMS/GFS
!   2011-05-01  todling - cwmr no longer in guess-grids; use metguess bundle now
!   2013-10-19  todling - metguess now holds background
!   2016-03-30  todling - update interface to general read (pass bundle)
!   2016-06-23  Li      - Add cloud partitioning, which was missed (based on GFS
!                         ticket #239, comment 18) 
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use gridmod, only: sp_a,grd_a,jcap_b,lat2,lon2,nsig
    use guess_grids, only: ifilesig,nfldsig
    use gsi_metguess_mod, only: gsi_metguess_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_bundlemod, only: gsi_bundlecreate
    use gsi_bundlemod, only: gsi_grid
    use gsi_bundlemod, only: gsi_gridcreate
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundledestroy
    use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info,general_sub2grid_destroy_info
    use mpimod, only: npe,mype
    use cloud_efr_mod, only: cloud_calc_gfs,set_cloud_lower_bound
    implicit none

    character(len=*),parameter::myname_=myname//'*read_'
    character(24) filename
    integer(i_kind):: it, istatus, inner_vars, num_fields
    integer(i_kind):: iret_ql,iret_qi

    real(r_kind),pointer,dimension(:,:  ):: ges_ps_it  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_z_it   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_u_it   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_v_it   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_div_it =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_vor_it =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_tv_it  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_q_it   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_oz_it  =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_cwmr_it=>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ql_it  => NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qi_it  => NULL()

    type(sub2grid_info) :: grd_t
    logical regional
    logical:: l_cld_derived,zflag,inithead

    type(gsi_bundle) :: atm_bundle
    type(gsi_grid)   :: atm_grid
    integer(i_kind),parameter :: n2d=2
    integer(i_kind),parameter :: n3d=8
    character(len=4), parameter :: vars2d(n2d) = (/ 'z   ', 'ps  ' /)
    character(len=4), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
                                                    'vor ', 'div ', &
                                                    'tv  ', 'q   ', &
                                                    'cw  ', 'oz  ' /)
    real(r_kind),pointer,dimension(:,:):: ptr2d   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ptr3d =>NULL()

    regional=.false.
    inner_vars=1
    num_fields=min(8*grd_a%nsig+2,npe)
!  Create temporary communication information fore read routines
    call general_sub2grid_create_info(grd_t,inner_vars,grd_a%nlat,grd_a%nlon, &
          grd_a%nsig,num_fields,regional)

!   Allocate bundle used for reading members
    call gsi_gridcreate(atm_grid,lat2,lon2,nsig)
    call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-read',istatus,names2d=vars2d,names3d=vars3d)
    if(istatus/=0) then
      write(6,*) myname_,': trouble creating atm_bundle'
      call stop2(999)
    endif

    do it=1,nfldsig

       write(filename,'(''sigf'',i2.2)') ifilesig(it)
       
!      Read background fields into bundle
       call general_read_gfsatm_nems(grd_t,sp_a,filename,.true.,.true.,.true.,&
            atm_bundle,.true.,istatus)

       inithead=.false.
       zflag=.false.

!      Set values to actual MetGuess fields
       call set_guess_

       l_cld_derived = associated(ges_cwmr_it).and.&
                       associated(ges_q_it)   .and.&
                       associated(ges_ql_it)  .and.&
                       associated(ges_qi_it)  .and.&
                       associated(ges_tv_it)
!      call set_cloud_lower_bound(ges_cwmr_it)
       if (mype==0) write(6,*)'READ_GFS_NEMS: l_cld_derived = ', l_cld_derived

       if (l_cld_derived) then
          call cloud_calc_gfs(ges_ql_it,ges_qi_it,ges_cwmr_it,ges_q_it,ges_tv_it)
       end if

    end do
    call general_sub2grid_destroy_info(grd_t)
    call gsi_bundledestroy(atm_bundle,istatus)

    contains

    subroutine set_guess_

    call gsi_bundlegetpointer (atm_bundle,'ps',ptr2d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ps',ges_ps_it ,istatus)
       if(istatus==0) ges_ps_it = ptr2d
    endif
    call gsi_bundlegetpointer (atm_bundle,'z',ptr2d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'z' ,ges_z_it ,istatus)
       if(istatus==0) ges_z_it = ptr2d
    endif
    call gsi_bundlegetpointer (atm_bundle,'u',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'u' ,ges_u_it ,istatus)
       if(istatus==0) ges_u_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'v',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'v' ,ges_v_it ,istatus)
       if(istatus==0) ges_v_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'vor',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'vor',ges_vor_it,istatus)
       if(istatus==0) ges_vor_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'div',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'div',ges_div_it,istatus) 
       if(istatus==0) ges_div_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'tv',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'tv',ges_tv_it ,istatus)
       if(istatus==0) ges_tv_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'q',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q' ,ges_q_it ,istatus)
       if(istatus==0) ges_q_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'oz',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'oz',ges_oz_it ,istatus)
       if(istatus==0) ges_oz_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'cw',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cw',ges_cwmr_it,istatus)
       if(istatus==0) ges_cwmr_it = ptr3d
    endif
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql_it,  iret_ql)
    call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi_it,  iret_qi)
    if (iret_ql/=0) then
       if (mype==0) write(6,*)'READ_ NEMSIO: cannot get pointer to ql,iret_ql=',iret_ql
    endif
    if (iret_qi/=0) then
       if (mype==0) write(6,*)'READ_ NEMSIO: cannot get pointer to qi,iret_qi=',iret_qi
    endif

  end subroutine set_guess_

  end subroutine read_

  subroutine read_chem_ ( iyear, month,idd )
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_nems_chem
!
!   prgrmmr: todling
!
! abstract: fills chemguess_bundle with GFS chemistry. 
!
! remarks:
!    1. Right now, only CO2 is done and even this is treated
!        as constant througout the assimialation window.
!    2. iyear and month could come from obsmod, but logically
!       this program should never depend on obsmod
! 
!
! program history log:
!   2010-12-23  Huang   - initial code, based on read_gfs_chem
!   2011-06-29  todling - no explict reference to internal bundle arrays
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

    use kinds,   only: i_kind, r_kind
    use mpimod,  only: mype
    use gridmod, only: lat2,lon2,nsig,nlat,rlats,istart
    use ncepgfs_ghg, only: read_gfsco2
    use guess_grids, only: nfldsig
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gsi_chemguess_mod, only: gsi_chemguess_bundle
    use gsi_chemguess_mod, only: gsi_chemguess_get

    implicit none

!   Declared argument list
    integer(i_kind), intent(in):: iyear
    integer(i_kind), intent(in):: month
    integer(i_kind), intent(in):: idd

!   Declare local variables
    integer(i_kind) :: igfsco2, i, j, n, iret
    real(r_kind),dimension(lat2):: xlats
    real(r_kind),pointer,dimension(:,:,:)::p_co2=>NULL()
    real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()

    if(.not.associated(gsi_chemguess_bundle)) return
    call gsi_bundlegetpointer(gsi_chemguess_bundle(1),'co2',p_co2,iret)
    if(iret /= 0) return

!   Get subdomain latitude array
    j = mype + 1
    do i = 1, lat2
       n = min(max(1, istart(j)+i-2), nlat)
       xlats(i) = rlats(n)
    enddo

!   Read in CO2
    call gsi_chemguess_get ( 'i4crtm::co2', igfsco2, iret )
    call read_gfsco2 ( iyear,month,idd,igfsco2,xlats,&
                       lat2,lon2,nsig,mype, p_co2 )

! Approximation: setting all times co2 values equal to the daily co2 values

    do n = 2, nfldsig
       call gsi_bundlegetpointer(gsi_chemguess_bundle(n),'co2',ptr3d,iret)
       ptr3d = p_co2
    enddo

  end subroutine read_chem_

  subroutine read_atm_ (grd,filename,sp_a,uvflag,vordivflag,zflag, &
       g_z,g_ps,g_vor,g_div,g_u,g_v,&
       g_tv,g_q,g_cwmr,g_oz)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nemsatm    read nems atm and send to all mpi tasks
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract: read ncep nems/gfs atmospheric guess field and 
!           scatter to subdomains
!
! program history log:
!   2010-02-22 Huang    Initial version.  Based on sub read_gfsatm
!   2011-02-28 Huang    Re-arrange the read sequence to be same as model
!                       write sequence.  Alsom allocate and deallocate
!                       temporary working array immediatelt before and after
!                       the processing and scattering first guess field to reduce
!                       maximum resident memory size.  Page fault can happen
!                       when running at high resolution GSI, e.g., T574.
!   2011-09-23 Huang    Add NEMS parallel IO capability
!   2013-10-25 todling  reposition fill_ns,filluv_ns to commvars
!
!   input argument list:
!     grd      - structure variable containing information about grid
!                    (initialized by general_sub2grid_create_info, located in
!                    general_sub2grid_mod.f90)
!     sp_a     - structure variable containing spectral information for analysis
!                    (initialized by general_init_spec_vars, located in
!                    general_specmod.f90)
!     uvflag   - logical to use u,v (.true.) or st,vp (.false.) perturbations
!     vordivflag - logical to determine if routine should output vorticity and
!                  divergence
!     zflag    - logical to determine if surface height field should be output
!
!   output argument list:
!     g_*      - guess fields
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind
    use gridmod, only: ntracer,ncloud,reload,itotsub,jcap_b
    use general_commvars_mod, only: fill_ns,filluv_ns,fill2_ns,filluv2_ns,ltosj_s,ltosi_s
    use general_specmod, only: spec_vars
    use general_sub2grid_mod, only: sub2grid_info
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype,mype
    use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
    use constants, only: two,pi,half,deg2rad
    implicit none
    
!   Declare local parameters
    real(r_kind),parameter:: r0_001 = 0.001_r_kind

!   Declare passed variables
    type(sub2grid_info)                   ,intent(in   ) :: grd
    character(len=24)                     ,intent(in   ) :: filename
    logical                               ,intent(in   ) :: uvflag,vordivflag,zflag
    real(r_kind),dimension(grd%lat2,grd%lon2)     ,intent(  out) :: g_z,g_ps
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig),intent(  out) :: g_u,g_v,&
         g_vor,g_div,g_cwmr,g_q,g_oz,g_tv
    type(spec_vars)                       ,intent(in   ) :: sp_a
    
!   Declare local variables
    character(len=120) :: my_name = 'READ_NEMSATM'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: iret,nlatm2,nflds
    integer(i_kind) :: k,icount,icount_prev,mm1,i,j,kk
    integer(i_kind) :: mype_hs, mype_ps,nord_int
    integer(i_kind) :: latb, lonb, levs, nframe
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 101
    real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
         grid_vor, grid_div, grid_b, grid_b2
    real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
    real(r_kind),allocatable,dimension(:)   :: work, work_vor, work_div, &
         work_v
    real(r_kind),allocatable,dimension(:,:) :: sub, sub_vor, sub_div, &
         sub_v
    real(r_kind),dimension(sp_a%nc):: spec_vor,spec_div
    real(r_kind),allocatable,dimension(:) :: rwork1d0, rwork1d1, rwork1d2
    real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
    real(4),allocatable,dimension(:) :: r4lats,r4lons
    real(r_kind) :: fhour
    type(nemsio_gfile) :: gfile
    logical diff_res,eqspace
    logical,dimension(1) :: vector
    type(egrid2agrid_parm) :: p_high
    
!******************************************************************************  
!   Initialize variables used below
    mm1=mype+1
    mype_hs=min(1,npe-1)
    mype_ps=0
    nlatm2=grd%nlat-2
    nflds=5*grd%nsig+1
    if(zflag) nflds=nflds+1
    if(vordivflag .or. .not. uvflag)nflds=nflds+2*grd%nsig
!   nflds=npe
    nflds=grd%nsig
    levs=grd%nsig

    allocate( work(grd%itotsub),work_v(grd%itotsub) )
    work=zero
    work_v=zero
    allocate( sub(grd%lat2*grd%lon2,max(grd%nsig,npe)),sub_v(grd%lat2*grd%lon2,max(grd%nsig,npe)) )
    allocate( sub_div(grd%lat2*grd%lon2,max(grd%nsig,npe)),sub_vor(grd%lat2*grd%lon2,max(grd%nsig,npe)) )
    if(mype < nflds)then

      call nemsio_init(iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'init',istop,iret)

      call nemsio_open(gfile,filename,'READ',iret=iret)
      if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop+1,iret)

      call nemsio_getfilehead(gfile,iret=iret, nframe=nframe, &
           nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
           idate=idate, dimx=lonb, dimy=latb,dimz=levs)
  
      if( nframe /= 0 ) then
         if ( mype == 0 ) &
         write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
         call stop2(101)
      end if

      fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year
!
!  g_* array already pre-allocate as (lat2,lon2,<nsig>) => 2D and <3D> array
!
      diff_res=.false.
      if(latb /= nlatm2) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
  !      call stop2(101)
      end if
      if(lonb /= grd%nlon) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
  !      call stop2(101)
      end if
      if(levs /= grd%nsig)then
         if ( mype == 0 ) write(6, &
            '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
            trim(my_name),grd%nsig,levs
         call stop2(101)
      end if
!
      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if(diff_res)then
         allocate( grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
         allocate( grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
      end if
      allocate( rwork1d0(latb*lonb) )
      allocate( rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
      allocate(rwork1d1(latb*lonb))
      call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
      call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
      do j=1,latb
        rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
      end do
      do j=1,lonb
        rlons(j)=deg2rad*r4lons(j)
      end do
      deallocate(r4lats,r4lons)
      rlats(1)=-half*pi
      rlats(latb+2)=half*pi
      do j=1,lonb
         clons(j)=cos(rlons(j))
         slons(j)=sin(rlons(j))
      end do

      nord_int=4
      eqspace=.false.
      call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                              latb+2,rlats,lonb,rlons,&
                              nord_int,p_high,.true.,eqspace)
      deallocate(rlats,rlons)
    end if
!
!   Load values into rows for south and north pole before scattering
!
!   Terrain:  scatter to all mpi tasks
!
    if(zflag)then
       if (mype==mype_hs) then
          call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'hgt','read',istop+2,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             call fill_ns(grid,work)
          end if
       endif
       call mpi_scatterv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
          g_z,grd%ijn_s(mm1),mpi_rtype,mype_hs,mpi_comm_world,ierror)
    end if

!   Surface pressure:  same procedure as terrain, but handled by task mype_ps
!
    if (mype==mype_ps) then
       call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d0,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop+3,iret)
       rwork1d1 = r0_001*rwork1d0
       if(diff_res)then
          vector(1)=.false.
          grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
          call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
          call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
          do kk=1,itotsub
            i=ltosi_s(kk)
            j=ltosj_s(kk)
            work(kk)=grid2(i,j,1)
          end do
       else
          grid=reshape(rwork1d1,(/size(grid,1),size(grid,2)/)) ! convert Pa to cb
          call fill_ns(grid,work)
       endif
    endif
    call mpi_scatterv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
       g_ps,grd%ijn_s(mm1),mpi_rtype,mype_ps,mpi_comm_world,ierror)

!   Divergence and voriticity.  Compute u and v from div and vor
    sub_vor=zero
    sub_div=zero
    sub    =zero
    sub_v  =zero
    icount     =0
    icount_prev=1
    allocate( work_vor(grd%itotsub),work_div(grd%itotsub) )
    do k=1,levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          ! Convert grid u,v to div and vor
          call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop+4,iret)
          call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop+5,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             grid_b2=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.true.
             call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
             do j=1,grd%nlon
               do i=2,grd%nlat-1
                 grid(j,grd%nlat-i)=grid2(i,j,1)
               end do
             end do
             call g_egrid2agrid(p_high,grid_c2,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work_v(kk)=grid2(i,j,1)
             end do
             do j=1,grd%nlon
               do i=2,grd%nlat-1
                 grid_v(j,grd%nlat-i)=grid2(i,j,1)
               end do
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             grid_v=reshape(rwork1d1,(/size(grid_v,1),size(grid_v,2)/))
             call filluv_ns(grid,grid_v,work,work_v)
          end if

          if(vordivflag .or. .not. uvflag)then
             
             allocate( grid_vor(grd%nlon,nlatm2), grid_div(grd%nlon,nlatm2) )
             call general_sptez_v(sp_a,spec_div,spec_vor,grid,grid_v,-1)
             call general_sptez_s_b(sp_a,sp_a,spec_div,grid_div,1)
             call general_sptez_s_b(sp_a,sp_a,spec_vor,grid_vor,1)

          ! Load values into rows for south and north pole
             call fill_ns(grid_div,work_div)
             call fill_ns(grid_vor,work_vor)
             deallocate(grid_vor,grid_div)
          end if
       endif
       ! Scatter to sub
       if (mod(icount,npe)==0 .or. icount==levs) then
          if(vordivflag .or. .not. uvflag)then
             call mpi_alltoallv(work_vor,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub_vor(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
             call mpi_alltoallv(work_div,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub_div(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
          end if
          if(uvflag)then
             call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
             call mpi_alltoallv(work_v,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub_v(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
          end if
          icount_prev=icount+1
       endif
    end do
    deallocate(work_vor,work_div)

    ! Transfer vor,div,u,v into real(r_kind) guess arrays
    call reload(sub_vor,g_vor)
    call reload(sub_div,g_div)
    call reload(sub,g_u)
    call reload(sub_v,g_v)
    deallocate(sub_vor,sub_div)

!   Thermodynamic variable and Specific humidity:  communicate to all tasks
!
    sub=zero
    icount=0
    icount_prev=1
    do k=1,levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then

          call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','read',istop+6,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             call fill_ns(grid,work)
          end if

          call nemsio_readrecv(gfile,'tmp','mid layer',k,rwork1d1,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','read',istop+7,iret)
          allocate(rwork1d2(latb*lonb))
          rwork1d2 = rwork1d1*(one+fv*rwork1d0)
          if(diff_res)then
             grid_b=reshape(rwork1d2,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work_v(kk)=grid2(i,j,1)
             end do
          else
             grid_v=reshape(rwork1d2,(/size(grid_v,1),size(grid_v,2)/))
             call fill_ns(grid_v,work_v)
          end if

          deallocate(rwork1d2)
       endif

       if (mod(icount,npe)==0 .or. icount==levs) then
          call mpi_alltoallv(work_v,grd%ijn_s,grd%displs_s,mpi_rtype,&
             sub_v(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
             sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub_v,g_tv)
    call reload(sub,g_q)
    deallocate(sub_v,work_v)

    sub=zero
    icount=0
    icount_prev=1
    do k=1,levs
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          call nemsio_readrecv(gfile,'o3mr','mid layer',k,rwork1d0,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'o3mr','read',istop+8,iret)
          if(diff_res)then
             grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
             call fill_ns(grid,work)
          end if
       endif
       if (mod(icount,npe)==0 .or. icount==levs) then
          call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
             sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
             mpi_comm_world,ierror)
          icount_prev=icount+1
       endif
    end do
    call reload(sub,g_oz)

!   Cloud condensate mixing ratio.

    if (ntracer>2 .or. ncloud>=1) then
       sub=zero
       icount=0
       icount_prev=1
       do k=1,levs
          icount=icount+1
          if (mype==mod(icount-1,npe)) then
             call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork1d0,iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'clwmr','read',istop+9,iret)
             if(diff_res)then
                grid_b=reshape(rwork1d0,(/size(grid_b,1),size(grid_b,2)/))
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,itotsub
                  i=ltosi_s(kk)
                  j=ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
                end do
             else
                grid=reshape(rwork1d0,(/size(grid,1),size(grid,2)/))
                call fill_ns(grid,work)
             end if
          endif
          if (mod(icount,npe)==0 .or. icount==levs) then
             call mpi_alltoallv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
                sub(1,icount_prev),grd%irc_s,grd%ird_s,mpi_rtype,&
                mpi_comm_world,ierror)
             icount_prev=icount+1
          endif
       end do
       call reload(sub,g_cwmr)
    else
       g_cwmr = zero
    endif

    if(mype < nflds)then
       if(diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid2)
       call destroy_egrid2agrid(p_high)
       deallocate(rwork1d1,clons,slons)
       deallocate(rwork1d0)
       deallocate(grid,grid_v)
       call nemsio_close(gfile,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop+9,iret)
    end if
    deallocate(work,sub)

!   Print date/time stamp 
    if ( mype == 0 ) write(6, &
       '(a,'': ges read/scatter,lonb,latb,levs= '',3i6,'',hour= '',f4.1,'',idate= '',4i5)') &
       trim(my_name),lonb,latb,levs,fhour,odate

  end subroutine read_atm_

  subroutine read_sfc_(sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                       veg_type,soil_type,terrain,isli,use_sfc_any)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_sfc_     read nems surface file
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract: read nems surface file
!
! program history log:
!   2010-02-22  Huang    Initial version.  Based on read_gfssfc
!   2011-02-14  Huang    Re-arrange the read sequence to be same as model
!                        write sequence.  Also remove unused array.
!   2016-03-13  xuli     Modify to follow read_sfc in ncepgfs_io for more effective I/O
!  
!   input argument list:
!     use_sfc_any - true if any processor uses extra surface fields
!
!   output argument list:
!     sfct      - surface temperature (skin temp)
!     soil_moi  - soil moisture of first layer
!     sno       - snow depth
!     soil_temp - soil temperature of first layer
!     veg_frac  - vegetation fraction
!     fact10    - 10 meter wind factor
!     sfc_rough - surface roughness
!     veg_type  - vegetation type
!     soil_type - soil type
!     terrain   - terrain height
!     isli      - sea/land/ice mask
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use mpimod, only: mype
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldsfc,ifilesfc
    use constants, only: zero
    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    implicit none

!   Declare passed variables
    logical,                                               intent(in   ) :: use_sfc_any
    real(r_single),  dimension(nlat_sfc,nlon_sfc,nfldsfc), intent(  out) :: sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough
    real(r_single),  dimension(nlat_sfc,nlon_sfc),         intent(  out) :: veg_type,soil_type,terrain
    integer(i_kind), dimension(nlat_sfc,nlon_sfc),         intent(  out) :: isli
!   Declare local parameters
    integer(i_kind),parameter   :: nsfc_all=11
    integer(i_kind),dimension(7):: idate
    integer(i_kind),dimension(4):: odate


!   Declare local variables
    character(len=24)  :: filename
    character(len=120) :: my_name = 'READ_NEMSSFC'
    character(len=1)   :: null = ' '
    integer(i_kind) :: i,j,it,n,nsfc
    integer(i_kind) :: iret, nframe, lonb, latb
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    real(r_single) :: fhour
    integer(i_kind) :: istop = 102
    real(r_single), allocatable, dimension(:)   :: rwork2d
    real(r_single), allocatable, dimension(:,:) :: work,outtmp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Define read variable property   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    type(nemsio_gfile) :: gfile
!-----------------------------------------------------------------------------

    call nemsio_init(iret=iret)
    if (iret /= 0) call error_msg(trim(my_name),null,null,'init',istop,iret)

    do it = 1, nfldsfc
! read a surface file on the task
       write(filename,200)ifilesfc(it)
200    format('sfcf',i2.2)

       call nemsio_open(gfile,filename,'READ',iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop,iret)

       call nemsio_getfilehead(gfile, idate=idate, iret=iret, nframe=nframe,   &
          nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
          dimx=lonb, dimy=latb )

       if( nframe /= 0 ) then
          if ( mype == 0 ) &
          write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
          call stop2(102)
       end if

       fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
       odate(1) = idate(4)  !hour
       odate(2) = idate(2)  !month
       odate(3) = idate(3)  !day
       odate(4) = idate(1)  !year

       if ( (latb /= nlat_sfc-2) .or. (lonb /= nlon_sfc) ) then
          if ( mype == 0 ) write(6, &
             '(a,'': inconsistent spatial dimension '',''nlon,nlatm2 = '',2(i4,tr1),''-vs- sfc file lonb,latb = '',i4)') &
             trim(my_name),nlon_sfc,nlat_sfc-2,lonb,latb
          call stop2(102)
       endif
!
!      Read the surface records (lonb, latb)  and convert to GSI array pattern (nlat_sfc,nlon_sfc)
!      Follow the read order sfcio in ncepgfs_io
!
       allocate(work(lonb,latb))
       allocate(rwork2d(size(work,1)*size(work,2)))
       work    = zero
       rwork2d = zero

       if(it == 1)then
         nsfc=nsfc_all
       else
         nsfc=nsfc_all-4
       end if

       do n = 1, nsfc

          if (n == 1) then                               ! skin temperature

!            Tsea
             call nemsio_readrecv(gfile, 'tmp', 'sfc', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,sfct(1,1,it),lonb,latb)

          elseif(n == 2 .and. use_sfc_any) then          ! soil moisture

!            smc
             call nemsio_readrecv(gfile, 'smc', 'soil layer', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'smc','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,soil_moi(1,1,it),lonb,latb)

          elseif(n == 3) then                            ! snow depth

!            sheleg
             call nemsio_readrecv(gfile, 'weasd','sfc', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'weasd','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,sno(1,1,it),lonb,latb)

          elseif(n == 4 .and. use_sfc_any) then          ! soil temperature

!            stc
             call nemsio_readrecv(gfile, 'stc', 'soil layer', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'stc','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,soil_temp(1,1,it),lonb,latb)

          elseif(n == 5 .and. use_sfc_any) then          ! vegetation cover

!            vfrac
             call nemsio_readrecv(gfile, 'veg',  'sfc', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'veg','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,veg_frac(1,1,it),lonb,latb)

          elseif(n == 6) then                            ! 10m wind factor

!            f10m
             call nemsio_readrecv(gfile, 'f10m', '10 m above gnd', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'f10m','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,fact10(1,1,it),lonb,latb)

          elseif(n == 7) then                            ! suface roughness

!            zorl
             call nemsio_readrecv(gfile, 'sfcr', 'sfc', 1, rwork2d,iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'sfcr','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,sfc_rough(1,1,it),lonb,latb)

          elseif(n == 8 .and. use_sfc_any) then          ! vegetation type

!            vtype
             call nemsio_readrecv(gfile, 'vtype','sfc', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vtype','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,veg_type,lonb,latb)

          elseif(n == 9 .and. use_sfc_any) then          ! soil type

!            stype
             call nemsio_readrecv(gfile, 'sotyp','sfc', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'sotyp','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,soil_type,lonb,latb)

          elseif(n == 10) then                           ! terrain

!            orog
             call nemsio_readrecv(gfile, 'orog', 'sfc', 1, rwork2d,iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'orog','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             call tran_gfssfc(work,terrain,lonb,latb)

          elseif(n == 11) then                           ! sea/land/ice flag

!            slmsk
             call nemsio_readrecv(gfile, 'land', 'sfc', 1, rwork2d, iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'land','read',istop,iret)
             work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
             allocate(outtmp(latb+2,lonb))
             call tran_gfssfc(work,outtmp,lonb,latb)
             do j=1,lonb
                do i=1,latb+2
                   isli(i,j) = nint(outtmp(i,j))
                end do
             end do
             deallocate(outtmp)

          endif

!      End of loop over data records
       enddo

!      Deallocate local work arrays
       deallocate(work,rwork2d)

       call nemsio_close(gfile,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop,iret)
!
!      Print date/time stamp
       if ( mype == 0 ) write(6, &
          '(a,'': sfc read,nlon,nlat= '',2i6,'',hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
!   End of loop over time levels
    end do
  end subroutine read_sfc_

  subroutine read_nemssfc_(iope,sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                           veg_type,soil_type,terrain,isli,use_sfc_any)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nemssfc_     read nems surface file
!   prgmmr: xuli          org: np23                date: 2016-03-13
!
! abstract: read nems surface file
!
! program history log:
!   2003-04-10  treadon
!
!   input argument list:
!     iope        - mpi task handling i/o
!     use_sfc_any - true if any processor uses extra surface fields
!
!   output argument list:
!     sfct      - surface temperature (skin temp)
!     soil_moi  - soil moisture of first layer
!     sno       - snow depth
!     soil_temp - soil temperature of first layer
!     veg_frac  - vegetation fraction
!     fact10    - 10 meter wind factor
!     sfc_rough - surface roughness
!     veg_type  - vegetation type
!     soil_type - soil type
!     terrain   - terrain height
!     isli      - sea/land/ice mask
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldsfc,sfcmod_mm5,sfcmod_gfs
    use mpimod, only: mpi_itype,mpi_rtype4,mpi_comm_world,mype
    use constants, only: zero
    implicit none

!   Declare passed variables
    integer(i_kind),                                       intent(in   ) :: iope
    logical,                                               intent(in   ) :: use_sfc_any
    real(r_single),  dimension(nlat_sfc,nlon_sfc,nfldsfc), intent(  out) :: sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough
    real(r_single),  dimension(nlat_sfc,nlon_sfc),         intent(  out) :: veg_type,soil_type,terrain
    integer(i_kind), dimension(nlat_sfc,nlon_sfc),         intent(  out) :: isli

!   Declare local variables
    integer(i_kind):: iret,npts,nptsall

!-----------------------------------------------------------------------------
!   Read surface file on processor iope
    if(mype == iope)then
       write(*,*) 'read_sfc nemsio'
       call read_sfc_(sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                      veg_type,soil_type,terrain,isli,use_sfc_any)
    end if

!     Load onto all processors

    npts=nlat_sfc*nlon_sfc
    nptsall=npts*nfldsfc
    call mpi_bcast(sfct,      nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(fact10,    nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(sno,       nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    if(sfcmod_mm5 .or. sfcmod_gfs)then
       call mpi_bcast(sfc_rough, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    else
       sfc_rough = zero
    end if
    call mpi_bcast(terrain,   npts,   mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(isli,      npts,   mpi_itype,iope,mpi_comm_world,iret)
    if(use_sfc_any)then
       call mpi_bcast(veg_frac, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_temp,nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_moi, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(veg_type, npts,   mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_type,npts,   mpi_rtype4,iope,mpi_comm_world,iret)
    end if

  end subroutine read_nemssfc_


  subroutine read_sfc_anl_(isli_anl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_sfc_anl_     read nems surface file with analysis resolution
!
!   prgmmr: li            org: np23                date: 2016-08-18
!
! abstract: read nems surface file at analysis grids when nlon /= nlon_sfc or nlat /= nlat_sfc
!
! program history log:
!  
!   input argument list:
!
!   output argument list:
!     isli      - sea/land/ice mask
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use mpimod, only: mype
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat,nlon
    use guess_grids, only: nfldsfc,ifilesfc
    use constants, only: zero
    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    implicit none

!   Declare passed variables
    integer(i_kind), dimension(nlat,nlon),   intent(  out) :: isli_anl

!   Declare local parameters
    integer(i_kind),dimension(7):: idate
    integer(i_kind),dimension(4):: odate


!   Declare local variables
    character(len=24)  :: filename
    character(len=120) :: my_name = 'READ_NEMSSFC_ANL'
    character(len=1)   :: null = ' '
    integer(i_kind) :: i,j
    integer(i_kind) :: iret, nframe, lonb, latb
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    real(r_single) :: fhour
    integer(i_kind) :: istop = 102
    real(r_single), allocatable, dimension(:)   :: rwork2d
    real(r_single), allocatable, dimension(:,:) :: work,outtmp

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Define read variable property   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    type(nemsio_gfile) :: gfile
!-----------------------------------------------------------------------------

    call nemsio_init(iret=iret)
    if (iret /= 0) call error_msg(trim(my_name),null,null,'init',istop,iret)


    filename='sfcf06_anlgrid'
    call nemsio_open(gfile,trim(filename),'READ',iret=iret)
    if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop,iret)

    call nemsio_getfilehead(gfile, idate=idate, iret=iret, nframe=nframe,   &
       nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
       dimx=lonb, dimy=latb )

    if( nframe /= 0 ) then
       if ( mype == 0 ) &
       write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
       call stop2(102)
    end if

    fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
    odate(1) = idate(4)  !hour
    odate(2) = idate(2)  !month
    odate(3) = idate(3)  !day
    odate(4) = idate(1)  !year

    if ( (latb /= nlat-2) .or. (lonb /= nlon) ) then
       if ( mype == 0 ) write(6, &
          '(a,'': inconsistent spatial dimension '',''nlon,nlatm2 = '',2(i4,tr1),''-vs- sfc file lonb,latb = '',i4)') &
          trim(my_name),nlon,nlat-2,lonb,latb
       call stop2(102)
    endif
!
!   Read the surface records (lonb, latb)  and convert to GSI array pattern (nlat,nlon)
!   Follow the read order sfcio in ncepgfs_io
!
    allocate(work(lonb,latb))
    allocate(rwork2d(size(work,1)*size(work,2)))
    work    = zero
    rwork2d = zero

!   slmsk
    call nemsio_readrecv(gfile, 'land', 'sfc', 1, rwork2d, iret=iret)
    if (iret /= 0) call error_msg(trim(my_name),trim(filename),'land','read',istop,iret)
    work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
    allocate(outtmp(latb+2,lonb))
    call tran_gfssfc(work,outtmp,lonb,latb)
    do j=1,lonb
       do i=1,latb+2
          isli_anl(i,j) = nint(outtmp(i,j))
       end do
    end do
    deallocate(outtmp)

!   Deallocate local work arrays
    deallocate(work,rwork2d)

    call nemsio_close(gfile,iret=iret)
    if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop,iret)
!
!   Print date/time stamp
    if ( mype == 0 ) write(6, &
       '(a,'': read_sfc_anl_ ,nlon,nlat= '',2i6,'',hour= '',f4.1,'',idate= '',4i5)') &
       trim(my_name),lonb,latb,fhour,odate
  end subroutine read_sfc_anl_

  subroutine read_nemssfc_anl_(iope,isli_anl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nemssfc_anl     read nems surface guess file with analysis resolution
!
!   prgmmr: xuli          org: np23                date: 2016-08-18
!
! abstract: read nems surface file at analysis grids
!
! program history log:
!
!   input argument list:
!     iope        - mpi task handling i/o
!
!   output argument list:
!     isli      - sea/land/ice mask
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat,nlon
    use mpimod, only: mpi_itype,mpi_comm_world,mype
    implicit none

!   Declare passed variables
    integer(i_kind),                               intent(in   ) :: iope
    integer(i_kind), dimension(nlat,nlon),         intent(  out) :: isli_anl


!   Declare local variables
    integer(i_kind):: iret,npts

!-----------------------------------------------------------------------------
!   Read surface file on processor iope
    if(mype == iope)then
       call read_sfc_anl_(isli_anl)
       write(*,*) 'read_sfc nemsio'
    end if

!   Load onto all processors
    npts=nlat*nlon
    call mpi_bcast(isli_anl,npts,mpi_itype,iope,mpi_comm_world,iret)

  end subroutine read_nemssfc_anl_

  subroutine read_nst_ (tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nst_     read nems nst surface guess file (quadratic 
!                                 Gaussin grids) without scattering to tasks
!   prgmmr: Huang            org: np23                date: 2011-11-01
!
! abstract: read nems surface NST file
!
! program history log:
!   2011-11-01  Huang    Initial version based on sub read_gfsnst
!   2016-03-13  Li       Modify for more effective I/O
!
!   input argument list:
!
!   output argument list:
!   tref     (:,:)                ! oceanic foundation temperature
!   dt_cool  (:,:)                ! sub-layer cooling amount at sub-skin layer
!   z_c      (:,:)                ! depth of sub-layer cooling layer
!   dt_warm  (:,:)                ! diurnal warming amount at sea surface (skin layer)
!   z_w      (:,:)                ! depth of diurnal warming layer
!   c_0      (:,:)                ! coefficient to calculate d(Tz)/d(tr) in dimensionless
!   c_d      (:,:)                ! coefficient to calculate d(Tz)/d(tr) in m^-1
!   w_0      (:,:)                ! coefficient to calculate d(Tz)/d(tr) in dimensionless
!   w_d      (:,:)                ! coefficient to calculate d(Tz)/d(tr) in m^-1
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use mpimod, only: mype
    use gridmod, only: nlat_sfc,nlon_sfc
    use constants, only: zero,two
    use guess_grids, only: nfldnst,ifilenst
    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrecv
    implicit none

!   Declare passed variables
    real(r_single) , dimension(nlat_sfc,nlon_sfc,nfldnst), intent(  out) ::  &
         tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d
!   Declare local parameters
    integer(i_kind),parameter    :: n_nst=9
    integer(i_kind),dimension(7) :: idate
    integer(i_kind),dimension(4) :: odate

!   Declare local variables
    character(len=6)   :: filename
    character(len=120) :: my_name = 'READ_NEMSNST'
    character(len=1)   :: null = ' '
    integer(i_kind) :: it,latb,lonb
    integer(i_kind) :: iret, nframe
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 103
    real(r_single) :: fhour
    real(r_single), dimension(nlat_sfc,nlon_sfc,nfldnst) :: xt
    real(r_single), allocatable, dimension(:)   :: rwork2d
    real(r_single), allocatable, dimension(:,:) :: work

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Define read variable property   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    type(nemsio_gfile) :: gfile
!-----------------------------------------------------------------------------

    call nemsio_init(iret=iret)
    if (iret /= 0) call error_msg(trim(my_name),null,null,'init',istop,iret)


    do it=1,nfldnst
! read a nst file on the task
       write(filename,200)ifilenst(it)
200    format('nstf',i2.2)
       call nemsio_open(gfile,trim(filename),'READ',iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop,iret)

       call nemsio_getfilehead(gfile, idate=idate, iret=iret, nframe=nframe,   &
          nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
          dimx=lonb, dimy=latb )

       if( nframe /= 0 ) then
          if ( mype == 0 ) &
          write(6,*)trim(my_name),': ***ERROR***  nframe /= 0 for global model read, nframe = ', nframe
          call stop2(istop)
       end if

       fhour = float(nfhour) + float(nfminute)/r60 + float(nfsecondn)/float(nfsecondd)/r3600
       odate(1) = idate(4)  !hour
       odate(2) = idate(2)  !month
       odate(3) = idate(3)  !day
       odate(4) = idate(1)  !year

       if ( (latb /= nlat_sfc-2) .or. (lonb /= nlon_sfc) ) then
          if ( mype == 0 ) &
             write(6,'(a,'': inconsistent spatial dimension nlon,nlatm2 = '',2(i4,tr1),''-vs- sfc file lonb,latb = '',i4)') &
             trim(my_name),nlon_sfc,nlat_sfc-2,lonb,latb
          call stop2(80)
       endif
!
!      Load surface fields into local work array
!      Follow NEMS/GFS sfcf read order
!
       allocate(work(lonb,latb))
       allocate(rwork2d(size(work,1)*size(work,2)))
       work    = zero
       rwork2d = zero

!      Tref
       call nemsio_readrecv(gfile, 'tref', 'sfc', 1, rwork2d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tref','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,tref(1,1,it),lonb,latb)

!      dt_cool
       call nemsio_readrecv(gfile, 'dtcool','sfc', 1, rwork2d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'dt_cool','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,dt_cool(1,1,it),lonb,latb)

!      z_c
       call nemsio_readrecv(gfile, 'zc',  'sfc', 1, rwork2d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'z_c','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,z_c(1,1,it),lonb,latb)

!      xt
       call nemsio_readrecv(gfile, 'xt',   'sfc', 1, rwork2d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'xt','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,xt(1,1,it),lonb,latb)

!      xz
       call nemsio_readrecv(gfile, 'xz',   'sfc', 1, rwork2d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'xz','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,z_w(1,1,it),lonb,latb)
!
!      c_0
       call nemsio_readrecv(gfile, 'c0',  'sfc', 1, rwork2d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'c_0','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,c_0(1,1,it),lonb,latb)

!      c_d
       call nemsio_readrecv(gfile, 'cd',  'sfc', 1, rwork2d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'c_d','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,c_d(1,1,it),lonb,latb)

!      w_0
       call nemsio_readrecv(gfile, 'w0',  'sfc', 1, rwork2d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'w_0','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,w_0(1,1,it),lonb,latb)

!      w_d
       call nemsio_readrecv(gfile, 'wd',  'sfc', 1, rwork2d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'w_d','read',istop,iret)
       work(:,:)=reshape(rwork2d(:),(/size(work,1),size(work,2)/))
       call tran_gfssfc(work,w_d(1,1,it),lonb,latb)

!
!      Get diurnal warming amout at z=0
!
       dt_warm(:,:,it) = two*xt(:,:,it)/z_w(:,:,it)

!      Deallocate local work arrays
       deallocate(work,rwork2d)

       call nemsio_close(gfile,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop,iret)
!   End of loop over time levels
    end do
  end subroutine read_nst_


  subroutine read_nemsnst_ (iope,tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nems_nst
!   prgmmr: li          org: np23                date: 2016-03-13
!
! abstract: read nems nst fields from a specific task and then broadcast to others
!
!   input argument list:
!     iope     - mpi task handling i/o
!
!   output argument list:
!   tref     (:,:)                        ! oceanic foundation temperature
!   dt_cool  (:,:)                        ! sub-layer cooling amount at sub-skin layer
!   z_c      (:,:)                        ! depth of sub-layer cooling layer
!   dt_warm  (:,:)                        ! diurnal warming amount at sea surface
!   z_w      (:,:)                        ! depth of diurnal warming layer
!   c_0      (:,:)                        ! coefficient to calculate d(Tz)/d(tr) in dimensionless
!   c_d      (:,:)                        ! coefficient to calculate d(Tz)/d(tr) in m^-1
!   w_0      (:,:)                        ! coefficient to calculate d(Tz)/d(tr) in dimensionless
!   w_d      (:,:)                        ! coefficient to calculate d(Tz)/d(tr) in m^-1
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldnst
    use mpimod, only: mpi_itype,mpi_rtype4,mpi_comm_world
    use mpimod, only: mype
    use constants, only: zero
    implicit none

!   Declare passed variables
    integer(i_kind),                                      intent(in   ) :: iope
    real(r_single), dimension(nlat_sfc,nlon_sfc,nfldnst), intent(  out) :: &
                    tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d

!   Declare local variables
    integer(i_kind):: iret,npts,nptsall

!-----------------------------------------------------------------------------
!   Read nst file on processor iope
    if(mype == iope)then
       write(*,*) 'read_nst nemsio'
       call read_nst_(tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)
    end if

!   Load onto all processors

    npts=nlat_sfc*nlon_sfc
    nptsall=npts*nfldnst

    call mpi_bcast(tref,    nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(dt_cool, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(z_c,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(dt_warm, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(z_w,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(c_0,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(c_d,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(w_0,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(w_d,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)

  end subroutine read_nemsnst_


  subroutine write_atm_ (grd,sp_a,filename,mype_out,gfs_bundle,ibin)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_nemsatm --- Gather, transform, and write out 
!      
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract: This routine gathers fields needed for the GSI analysis
!           file from subdomains and then transforms the fields from
!           analysis grid to model guess grid, then written to an 
!           atmospheric analysis file.
!
! program history log:
!   2010-02-22  Huang    Initial version.  Based on write_gfsatm
!   2011-02-14  Huang    Re-arrange the write sequence to be same as model
!                        read/rite sequence.
!   2013-10-25  todling  reposition load_grid to commvars
!   2016-07-28  mahajan  update with bundling ability
!
!   input argument list:
!     filename  - file to open and write to
!     mype_out  - mpi task to write output file
!    gfs_bundle - bundle containing fields on subdomains
!     ibin      - time bin
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind
    
    use constants, only: r1000,fv,one,zero,qcmin
  
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: npe,mype
    
    use guess_grids, only: ifilesig
    use guess_grids, only: ges_prsl,ges_prsi
    
    use gridmod, only: ntracer
    use gridmod, only: ncloud
    use gridmod, only: strip,itotsub,iglobal,jcap_b
    
    use general_commvars_mod, only: load_grid,fill2_ns,filluv2_ns,ltosj_s,ltosi_s,ltosj,ltosi
    use general_specmod, only: spec_vars

    use obsmod, only: iadate
    
    use nemsio_module, only: nemsio_gfile,nemsio_open,nemsio_init,&
         nemsio_getfilehead,nemsio_close,nemsio_writerecv,nemsio_readrecv
    use gsi_4dvar, only: ibdate,nhr_obsbin
    use general_sub2grid_mod, only: sub2grid_info
    use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
    use constants, only: two,pi,half,deg2rad
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
  
    implicit none

! !INPUT PARAMETERS:

    type(sub2grid_info), intent(in) :: grd
    type(spec_vars),     intent(in) :: sp_a
    character(len=24),   intent(in) :: filename  ! file to open and write to
    integer(i_kind),     intent(in) :: mype_out  ! mpi task to write output file
    type(gsi_bundle),    intent(in) :: gfs_bundle
    integer(i_kind),     intent(in) :: ibin      ! time bin

!-------------------------------------------------------------------------

    real(r_kind),parameter:: r0_001 = 0.001_r_kind
    character(6):: fname_ges
    character(len=120) :: my_name = 'WRITE_NEMSATM'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: k, mm1, nlatm2, nord_int, i, j, kk
    integer(i_kind) :: iret, lonb, latb, levs, istatus
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 104
    integer(i_kind),dimension(5):: mydate
    integer(i_kind),dimension(8) :: ida,jda
    real(r_kind),dimension(5)    :: fha
    real(r_kind)    :: fhour

    real(r_kind),pointer,dimension(:,:) :: sub_ps
    real(r_kind),pointer,dimension(:,:,:) :: sub_u,sub_v,sub_tv
    real(r_kind),pointer,dimension(:,:,:) :: sub_q,sub_oz,sub_cwmr
    
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig) :: sub_prsl
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig+1) :: sub_prsi

    real(r_kind),dimension(grd%lat1*grd%lon1)     :: psm
    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig):: sub_dp
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: tvsm,prslm, usm, vsm
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: dpsm, qsm, ozsm, cwsm
    real(r_kind),dimension(max(grd%iglobal,grd%itotsub))     :: work1,work2
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid
    real(r_kind),allocatable,dimension(:) :: rwork1d,rwork1d1,rlats,rlons,clons,slons
    real(4),allocatable,dimension(:) :: r4lats,r4lons
    real(r_kind),allocatable,dimension(:,:) :: grid_b,grid_b2
    real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid3, grid_c2

    type(nemsio_gfile) :: gfile,gfileo
    logical diff_res,eqspace
    logical,dimension(1) :: vector
    type(egrid2agrid_parm) :: p_low,p_high

!*************************************************************************
!   Initialize local variables
    mm1=mype+1
    nlatm2=grd%nlat-2
    diff_res=.false.

    istatus=0
    call gsi_bundlegetpointer(gfs_bundle,'ps', sub_ps,  iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'u',  sub_u,   iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'v',  sub_v,   iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'tv', sub_tv,  iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'q',  sub_q,   iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'oz', sub_oz,  iret); istatus=istatus+iret
    call gsi_bundlegetpointer(gfs_bundle,'cw', sub_cwmr,iret); istatus=istatus+iret
    if ( istatus /= 0 ) then
       if ( mype == 0 ) then
         write(6,*) 'write_atm_: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
    endif

    if ( sp_a%jcap /= jcap_b ) then
        if ( mype == 0 ) write(6, &
            '('' dual resolution for nems sp_a%jcap,jcap_b = '',2i6)') &
            sp_a%jcap,jcap_b
        diff_res = .true.
    endif


    ! Single task writes analysis data to analysis file
    if ( mype == mype_out ) then
       write(fname_ges,'(''sigf'',i2.2)') ifilesig(ibin)

       ! Read header information from first guess file.
       call nemsio_init(iret)
       if ( iret /= 0 ) call error_msg(trim(my_name),null,null,'init',istop,iret)

       call nemsio_open(gfile,trim(fname_ges),'read',iret)
       if ( iret /= 0 ) call error_msg(trim(my_name),trim(fname_ges),null,'open',istop,iret)

       call nemsio_getfilehead(gfile, iret=iret, nfhour=nfhour, &
            nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd, &
            idate=idate, dimx=lonb, dimy=latb, dimz=levs)
       if ( iret /= 0 ) then
          write(6,*) trim(my_name),': problem with nemsio_getfilehead, Status = ',iret
          call stop2(103)
       endif
       if ( levs /= grd%nsig ) then
          write(6,*) trim(my_name),': problem in data dimension background levs = ',levs,' nsig = ',grd%nsig
          call stop2(103)
       endif

       ! copy input header info to output header info
       gfileo=gfile
 
       ! Update header information (with ibdate) and write it to analysis file (w/ _open statement).
       mydate=ibdate
       fha(:)=zero ; ida=0; jda=0
       fha(2)=real(nhr_obsbin*(ibin-1))  ! relative time interval in hours
       ida(1)=mydate(1) ! year
       ida(2)=mydate(2) ! month
       ida(3)=mydate(3) ! day
       ida(4)=0         ! time zone
       ida(5)=mydate(4) ! hour

       ! Move date-time forward by nhr_assimilation hours
       call w3movdat(fha,ida,jda)

       jdate(1) = jda(1)     ! analysis year
       jdate(2) = jda(2)     ! analysis month
       jdate(3) = jda(3)     ! analysis day
       jdate(4) = jda(5)     ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(6) = 0          ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour   =0       !  new forecast hour, zero at analysis time
       nfminute =0    
       nfsecondn=0     
       nfsecondd=100      ! default for denominator

       fhour = zero
       odate(1) = jdate(4)  !hour
       odate(2) = jdate(2)  !month
       odate(3) = jdate(3)  !day
       odate(4) = jdate(1)  !year

       ! open new output file with new header gfileo with "write" access. 
       ! Use this call to update header as well

       call nemsio_open(gfileo,trim(filename),'write',iret=iret, &
          idate=jdate, nfhour=nfhour, nfminute=nfminute, &
          nfsecondn=nfsecondn, nfsecondd=nfsecondd)
       if ( iret /= 0 ) call error_msg(trim(my_name),trim(filename),null,'open',istop,iret)

       ! Allocate structure arrays to hold data
       allocate(rwork1d(latb*lonb),rwork1d1(latb*lonb))
       if ( diff_res ) then
          allocate( grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid3(grd%nlat,grd%nlon,1))
          allocate( grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
          allocate( rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb),r4lats(lonb*latb),r4lons(lonb*latb))
          call nemsio_getfilehead(gfile,lat=r4lats,iret=iret)
          call nemsio_getfilehead(gfile,lon=r4lons,iret=iret)
          do j=1,latb
            rlats(latb+2-j)=deg2rad*r4lats(lonb/2+(j-1)*lonb)
          enddo
          rlats(1)=-half*pi
          rlats(latb+2)=half*pi
          do j=1,lonb
            rlons(j)=deg2rad*r4lons(j)
          enddo
          do j=1,lonb
             clons(j)=cos(rlons(j))
             slons(j)=sin(rlons(j))
          enddo

          nord_int=4
          eqspace=.false.
          call g_create_egrid2agrid(grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons, &
                                latb+2,rlats,lonb,rlons,&
                                nord_int,p_low,.false.,eqspace=eqspace)
          call g_create_egrid2agrid(latb+2,rlats,lonb,rlons, &
                                grd%nlat,sp_a%rlats,grd%nlon,sp_a%rlons,&
                                nord_int,p_high,.false.,eqspace=eqspace)

          deallocate(rlats,rlons,r4lats,r4lons)
       endif ! if ( diff_res )

       !   Terrain
       !   Write out input file surface height

       call nemsio_readrecv(gfile,'hgt', 'sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'hgt','writeread',istop,iret)
       call nemsio_writerecv(gfileo,'hgt','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'hgt','write',istop,iret)
    endif ! if ( mype == mype_out )

    sub_prsl = ges_prsl(:,:,:,ibin)
    sub_prsi = ges_prsi(:,:,:,ibin)

    do k=1,grd%nsig
       sub_dp(:,:,k) = sub_prsi(:,:,k) - sub_prsi(:,:,k+1)
    end do

    ! Strip off boundary points from subdomains
    call strip(sub_ps  ,psm)
    call strip(sub_tv  ,tvsm  ,grd%nsig)
    call strip(sub_q   ,qsm   ,grd%nsig)
    call strip(sub_oz  ,ozsm  ,grd%nsig)
    call strip(sub_cwmr,cwsm  ,grd%nsig)
    call strip(sub_dp  ,dpsm  ,grd%nsig)
    call strip(sub_prsl,prslm ,grd%nsig)
    call strip(sub_u   ,usm   ,grd%nsig)
    call strip(sub_v   ,vsm   ,grd%nsig)

    ! Thermodynamic variable
    ! The GSI analysis variable is virtual temperature (Tv).   For NEMSIO
    ! output we need the sensible temperature.

    ! Convert Tv to T
    tvsm = tvsm/(one+fv*qsm)

    ! Generate and write analysis fields

    ! Surface pressure.  
    call mpi_gatherv(psm,grd%ijn(mm1),mpi_rtype,&
         work1,grd%ijn,grd%displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       if(diff_res)then
          call nemsio_readrecv(gfile,'pres','sfc',1,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop,iret)
          rwork1d1 = r0_001*rwork1d
          grid_b=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
          vector(1)=.false.
          call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
          call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
          do kk=1,grd%iglobal
             i=grd%ltosi(kk)
             j=grd%ltosj(kk)
             grid3(i,j,1)=work1(kk)-grid3(i,j,1)
          end do

          call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
          do j=1,latb
             do i=1,lonb
                grid_b(i,j)=r1000*(grid_b(i,j)+grid_c(latb-j+2,i,1))
             end do
          end do
          rwork1d = reshape(grid_b,(/size(rwork1d)/))
       else
          call load_grid(work1,grid)
          grid = grid*r1000
          rwork1d = reshape(grid,(/size(rwork1d)/))
       end if
       call nemsio_writerecv(gfileo,'pres','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'psfc','write',istop,iret)
    endif

!   u, v
    do k=1,grd%nsig
       call mpi_gatherv(usm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       call mpi_gatherv(vsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work2,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'ugrd','mid layer',k,rwork1d,iret=iret)
             call nemsio_readrecv(gfile,'vgrd','mid layer',k,rwork1d1,iret=iret)
             grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
             grid_b2=reshape(rwork1d1,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.true.
             call filluv2_ns(grid_b,grid_b2,grid_c(:,:,1),grid_c2(:,:,1),latb+2,lonb,slons,clons)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             call g_egrid2agrid(p_low,grid_c2,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work2(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b2(i,j)=grid_b2(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
             rwork1d1 = reshape(grid_b2,(/size(rwork1d1)/))

          else
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
             call load_grid(work2,grid)
             rwork1d1 = reshape(grid,(/size(rwork1d1)/))
          end if

          ! Zonal wind
          call nemsio_writerecv(gfileo,'ugrd','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','write',istop,iret)
          ! Meridional wind
          call nemsio_writerecv(gfileo,'vgrd','mid layer',k,rwork1d1,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','write',istop,iret)
       endif
    end do

!   Thermodynamic variable
    do k=1,grd%nsig
       call mpi_gatherv(tvsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'tmp','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop,iret)
             grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
          else
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
          end if
          call nemsio_writerecv(gfileo,'tmp','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','write',istop,iret)
       endif
    end do

!   Specific humidity
    do k=1,grd%nsig
       call mpi_gatherv(qsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'spfh','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop,iret)
             grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
          else
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
          end if
          call nemsio_writerecv(gfileo,'spfh','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','write',istop,iret)
       endif
    end do

!   Ozone
    do k=1,grd%nsig
       call mpi_gatherv(ozsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             call nemsio_readrecv(gfile,'o3mr','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop,iret)
             grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-grid3(i,j,1)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             rwork1d = reshape(grid_b,(/size(rwork1d)/))
          else
             call load_grid(work1,grid)
             rwork1d = reshape(grid,(/size(rwork1d)/))
          end if
          call nemsio_writerecv(gfileo,'o3mr','mid layer',k,rwork1d,iret=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'o3mr','write',istop,iret)
       endif
    end do
       
!   Cloud condensate mixing ratio
    if (ntracer>2 .or. ncloud>=1) then
       do k=1,grd%nsig
          call mpi_gatherv(cwsm(1,k),grd%ijn(mm1),mpi_rtype,&
               work1,grd%ijn,grd%displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype == mype_out) then
             if(diff_res)then
                call nemsio_readrecv(gfile,'clwmr','mid layer',k,rwork1d,iret=iret)
                if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop,iret)
                grid_b=reshape(rwork1d,(/size(grid_b,1),size(grid_b,2)/))
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
                do kk=1,grd%iglobal
                   i=grd%ltosi(kk)
                   j=grd%ltosj(kk)
                   grid3(i,j,1)=work1(kk)-max(grid3(i,j,1),qcmin)
                end do
                call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
                do j=1,latb
                   do i=1,lonb
                      grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                   end do
                end do
                rwork1d = reshape(grid_b,(/size(rwork1d)/))
             else
                call load_grid(work1,grid)
                rwork1d = reshape(grid,(/size(rwork1d)/))
             endif
             call nemsio_writerecv(gfileo,'clwmr','mid layer',k,rwork1d,iret=iret)
             if (iret /= 0) call error_msg(trim(my_name),trim(filename),'clwmr','write',istop,iret)
          end if
       end do
    endif
!
! Deallocate local array
!
    if (mype==mype_out) then
       if(diff_res) deallocate(grid_b,grid_b2,grid_c,grid_c2,grid3,clons,slons)
       call nemsio_close(gfile,iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_ges),null,'close',istop,iret)

       call nemsio_close(gfileo,iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop,iret)
!
! Deallocate local array
!
       deallocate(rwork1d,rwork1d1)
!
       write(6,'(a,'': atm anal written for lonb,latb,levs= '',3i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,levs,fhour,odate
    endif

  end subroutine write_atm_

  subroutine write_sfc_ (filename,mype_sfc,dsfct)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_nemssfc --- Write surface analysis to file
!
!   prgmmr: Huang            org: np23                date: 2010-02-22
!
! abstract:     This routine writes the updated surface analysis.  At
!               this point (20101020) the only surface field update by 
!               the gsi is the skin temperature.  The current (20101020)
!               GDAS setup does use the updated surface file.  Rather,
!               the output from surface cycle is used as the surface
!               analysis for subsequent NEMS/GFS runs.
!
!               The routine gathers surface fields from subdomains, 
!               reformats the data records, and then writes each record
!               to the output file.  
!
!               Since the gsi only update the skin temperature, all
!               other surface fields are simply read from the guess
!               surface file and written to the analysis file.
!
! program history log:
!   2010-02-22  Huang    Initial version.  Based on write_gfssfc
!   2011-04-01  Huang    change type of buffer2, grid2 from single to r_kind
!   2013-10-25  todling - reposition ltosi and others to commvars
!
!   input argument list:
!     filename  - file to open and write to
!     dsfct     - delta skin temperature
!     mype_sfc  - mpi task to write output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind,r_single
  
    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype
    
    use gridmod, only: nlat,nlon
    use gridmod, only: lat1,lon1
    use gridmod, only: lat2,lon2
    use gridmod, only: iglobal
    use gridmod, only: ijn
    use gridmod, only: displs_g
    use gridmod, only: itotsub
    use gridmod, only: rlats,rlons,rlats_sfc,rlons_sfc
    
    use general_commvars_mod, only: ltosi,ltosj

    use obsmod, only: iadate
    
    use constants, only: zero
    
    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close,nemsio_readrecv
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead
    use nemsio_module, only:  nemsio_readrec, nemsio_writerec, nemsio_writerecv

    implicit none

! !INPUT PARAMETERS:
    character(24)                    ,intent(in   ) :: filename  ! file to open and write to

    real(r_kind),dimension(lat2,lon2),intent(in   ) :: dsfct   ! delta skin temperature

    integer(i_kind)                  ,intent(in   ) :: mype_sfc ! mpi task to write output file

! !OUTPUT PARAMETERS:

!-------------------------------------------------------------------------

!   Declare local parameters
    character( 6),parameter:: fname_ges='sfcf06'
!   Declare local variables
    character(len=120) :: my_name = 'WRITE_NEMSSFC'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: i, j, ip1, jp1, ilat, ilon, jj, mm1
    integer(i_kind) :: nlatm2, n, nrec, lonb, latb, iret
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 105
    real(r_kind)    :: fhour

    real(r_kind),dimension(lat1,lon1):: sfcsub
    real(r_kind),dimension(nlon,nlat):: grid
    real(r_kind),dimension(max(iglobal,itotsub)):: sfcall
    real(r_kind),allocatable,dimension(:,:) :: tsea
    real(r_kind),allocatable,dimension(:)   :: rwork1d
    real(r_single),dimension(nlon,nlat):: buffer
    real(r_single),allocatable,dimension(:,:) :: buffer2,grid2

    type(nemsio_gfile) :: gfile, gfileo
!*****************************************************************************

!   Initialize local variables
    mm1=mype+1
    nlatm2=nlat-2

!   Gather skin temperature information from all tasks.  
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          sfcsub(i,j)=dsfct(ip1,jp1)
       end do
    end do
    call mpi_gatherv(sfcsub,ijn(mm1),mpi_rtype,&
         sfcall,ijn,displs_g,mpi_rtype,mype_sfc,&
         mpi_comm_world,ierror)

!   Only MPI task mype_sfc writes the surface file.
    if (mype==mype_sfc) then

!      Reorder updated skin temperature to output format
       do i=1,iglobal
          ilon=ltosj(i)
          ilat=ltosi(i)
          grid(ilon,ilat)=sfcall(i)
       end do
       do j=1,nlat
          jj=nlat-j+1
          do i=1,nlon
             buffer(i,j)=grid(i,jj)
          end do
       end do

!      Read surface guess file
       call nemsio_init(iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),null,null,'init',istop,iret)

       call nemsio_open(gfile,fname_ges,'read',iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_ges),null,'open',istop,iret)
!
       call nemsio_getfilehead(gfile, nrec=nrec, idate=idate, dimx=lonb, &
          dimy=latb, nfhour=nfhour, nfminute=nfminute, nfsecondn=nfsecondn, &
          nfsecondd=nfsecondd, iret=iret)
!
!      Replace header record date with analysis time from iadate
!
       jdate(1) = iadate(1)  ! analysis year
       jdate(2) = iadate(2)  ! analysis month
       jdate(3) = iadate(3)  ! analysis day
       jdate(4) = iadate(4)  ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(5) = 0          ! analysis minute
       jdate(6) = 0          ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour=0       !  new forecast hour, zero at analysis time
       nfminute=0
       nfsecondn=0
       nfsecondd=100      ! default for denominator

       fhour    = zero
       odate(1) = jdate(4)  !hour
       odate(2) = jdate(2)  !month
       odate(3) = jdate(3)  !day
       odate(4) = jdate(1)  !year
!
! Start to write output sfc file : filename
!      open new output file with new header gfileo with "write" access. 
!      Use this call to update header as well
!
!
       gfileo=gfile      ! copy input header info to output header info
                         ! need to do this before nemsio_close(gfile)
       call nemsio_open(gfileo,filename,'write',iret=iret, idate=jdate, nfhour=nfhour,&
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd )
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'open',istop,iret)
!
!      First copy entire data from fname_ges to filename, then do selective update
!
       allocate(rwork1d(lonb*latb))
       allocate(buffer2(lonb,latb))
       allocate(grid2(lonb,latb))
       allocate(tsea(lonb,latb))

       do n = 1, nrec
         call nemsio_readrec (gfile, n,rwork1d,iret=iret)
         if ( iret /= 0 ) write(6,*) 'readrec  nrec = ', n, '  Status = ', iret
         call nemsio_writerec(gfileo,n,rwork1d,iret=iret)
         if ( iret /= 0 ) write(6,*) 'writerec nrec = ', n, '  Status = ', iret
       end do
!
! Only sea surface temperature will be updated in the SFC files
!

       call nemsio_readrecv(gfile,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_ges),'tmp','read',istop,iret)
       tsea=reshape(rwork1d,(/size(tsea,1),size(tsea,2)/))

       if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then
          write(6,*)trim(my_name),':  different grid dimensions analysis', &
             ' vs sfc. interpolating sfc temperature nlon,nlat-2=',nlon,  &
             nlatm2,' -vs- sfc file lonb,latb=',lonb,latb
          call intrp22(buffer, rlons,rlats,nlon,nlat, &
                       buffer2,rlons_sfc,rlats_sfc,lonb,latb)
       else
          do j=1,latb
             do i=1,lonb
                buffer2(i,j)=buffer(i,j+1)
             end do
          end do
       endif

       grid2 = tsea + buffer2
       rwork1d = reshape( grid2,(/size(rwork1d)/) )

       deallocate(buffer2)

!      update tsea record
       call nemsio_writerecv(gfileo,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','write',istop,iret)
       deallocate(rwork1d)

       call nemsio_close(gfile, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_ges),null,'close',istop,iret)

       call nemsio_close(gfileo,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop,iret)

       write(6,'(a,'': sfc anal written for lonb,latb= '',2i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
    endif
  end subroutine write_sfc_

  subroutine write_sfc_nst_ (mype_so,dsfct)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_sfc_nst --- Write both sfc and nst surface analysis to file
!
!   prgmmr: Huang            org: np23                date: 2011-11-01
!
! abstract:     This routine writes the sfc & nst analysis files and is nst_gsi dependent.
!               Tr (foundation temperature), instead of skin temperature, is the analysis variable.
!               nst_gsi >  2: Tr analysis is on
!               nst_gsi <= 2: Tr analysis is off
!
!               The routine gathers Tr field from subdomains,
!               reformats the data records, and then writes each record
!               to the output files.
!
!               Since the gsi only update the Tr temperature, all
!               other fields in surface are simply read from the guess
!               files and written to the analysis file.
!
! program history log:
!   2011-11-01  Huang    initial version based on routine write_gfs_sfc_nst
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2016-01-01  li      - update write_sfc_nst_ (nemsio) as for write_gfs_sfc_nst (sfcio)
!
!   input argument list:
!     dsfct     - delta skin temperature
!     mype_so   - mpi task to write output file
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind,r_single
  
    use mpimod, only: mpi_rtype,mpi_itype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype
    
    use gridmod, only: nlat,nlon
    use gridmod, only: lat1,lon1
    use gridmod, only: lat2,lon2
    use gridmod, only: nlat_sfc,nlon_sfc
    use gridmod, only: iglobal
    use gridmod, only: ijn
    use gridmod, only: displs_g
    use gridmod, only: itotsub
    
    use general_commvars_mod, only: ltosi,ltosj

    use obsmod, only: iadate
    
    use constants, only: zero,two,tfrozen,z_w_max
    use constants, only: zero_single
    
    use guess_grids, only: isli2
    use gsi_nstcouplermod, only: nst_gsi,zsea1,zsea2
    use gridmod, only: rlats,rlons,rlats_sfc,rlons_sfc

    use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close,nemsio_readrecv
    use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead
    use nemsio_module, only:  nemsio_readrec, nemsio_writerec, nemsio_writerecv

    implicit none

! !INPUT PARAMETERS:

    real(r_kind),dimension(lat2,lon2),intent(in   ) :: dsfct     ! delta skin temperature
    integer(i_kind)                  ,intent(in   ) :: mype_so   ! mpi task to write output file

! !OUTPUT PARAMETERS:

!-------------------------------------------------------------------------

!   Declare local parameters
    character(6), parameter:: fname_sfcges = 'sfcf06'
    character(6), parameter:: fname_sfcgcy = 'sfcgcy'
    character(6), parameter:: fname_sfctsk = 'sfctsk'
    character(6), parameter:: fname_sfcanl = 'sfcanl'
    character(6), parameter:: fname_nstges = 'nstf06'
    character(6), parameter:: fname_nstanl = 'nstanl'
    character(6), parameter:: fname_dtfanl = 'dtfanl'

!   Declare local variables
    integer(i_kind), parameter:: io_dtfanl = 54
    integer(i_kind), parameter:: nprep=15
    real(r_kind),parameter :: houra = zero_single
    character(len=120) :: my_name = 'WRITE_SFC_NST'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(7):: idate, jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: i, j, ip1, jp1, ilat, ilon, mm1
    integer(i_kind) :: lonb, latb, nlatm2, n, nrec_sfc, nrec_nst, iret
    integer(i_kind) :: lonb_nst, latb_nst
    integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
    integer(i_kind) :: istop = 106
    real(r_kind)    :: fhour
    real(r_single)  :: r_zsea1,r_zsea2

    real(r_kind),    dimension(lat1,lon1):: dsfct_sub
    integer(i_kind), dimension(lat1,lon1):: isli_sub

    real(r_kind),    dimension(max(iglobal,itotsub)):: dsfct_all
    integer(i_kind), dimension(max(iglobal,itotsub)):: isli_all

    real(r_kind),    dimension(nlat,nlon):: dsfct_glb,dsfct_tmp
    integer(i_kind), dimension(nlat,nlon):: isli_glb,isli_tmp

    real(r_kind),    dimension(nlat_sfc,nlon_sfc)  :: dsfct_gsi
    integer(i_kind), dimension(nlat_sfc,nlon_sfc)  :: isli_gsi

    real(r_kind),    dimension(nlon_sfc,nlat_sfc-2):: dsfct_anl
    real(r_single),  dimension(nlon_sfc,nlat_sfc-2):: dtzm
    real(r_single),  dimension(nlat_sfc,nlon_sfc)  :: work

    real(r_single),   allocatable, dimension(:,:) :: tsea,xt,xs,xu,xv,xz,zm,xtts,xzts,dt_cool,z_c, &
                                                     c_0,c_d,w_0,w_d,d_conv,ifd,tref,qrain
    real(r_single),   allocatable, dimension(:,:) :: slmsk_ges,slmsk_anl
    real(r_single),   allocatable, dimension(:)   :: rwork1d

    type(nemsio_gfile) :: gfile_sfcges,gfile_sfcgcy,gfile_nstges,gfile_sfctsk,gfile_sfcanl,gfile_nstanl

!*****************************************************************************

!   Initialize local variables
    mm1=mype+1
    nlatm2=nlat-2
!
!   Extract the analysis increment and surface mask in subdomain without the buffer
!
    do j=1,lon1
       jp1 = j+1
       do i=1,lat1
          ip1 = i+1
          dsfct_sub(i,j) = dsfct(ip1,jp1)
          isli_sub (i,j) = isli2(ip1,jp1)
       end do
    end do
!
!   Gather global analysis increment and surface mask info from subdomains
!
    call mpi_gatherv(dsfct_sub,ijn(mm1),mpi_rtype,&
         dsfct_all,ijn,displs_g,mpi_rtype,mype_so ,&
         mpi_comm_world,ierror)

    call mpi_gatherv(isli_sub,ijn(mm1),mpi_itype,&
         isli_all,ijn,displs_g,mpi_itype,mype_so ,&
         mpi_comm_world,ierror)

!   Only MPI task mype_so  writes the surface file.
    if (mype==mype_so ) then

      write(*,'(a,5(1x,a6))') 'write_nems_sfc_nst:',fname_sfcges,fname_nstges,fname_sfctsk,fname_sfcanl,fname_nstanl
!
!     get Tf analysis increment and surface mask at analysis (lower resolution) grids
!
      do i=1,iglobal
         ilon=ltosj(i)
         ilat=ltosi(i)
         dsfct_glb(ilat,ilon) = dsfct_all(i)
         isli_glb (ilat,ilon) = isli_all (i)
      end do
!
!      write dsfct_anl to a data file for later use (at eupd step at present)
!
       open(io_dtfanl,file=fname_dtfanl,form='unformatted')
       write(io_dtfanl) nlon,nlat
       write(io_dtfanl) dsfct_glb
       write(io_dtfanl) isli_glb

!      Initiate nemsio
       call nemsio_init(iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),null,null,'init',istop,iret)

!      open nsst guess file
       call nemsio_open(gfile_nstges,trim(fname_nstges),'read',iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),null,'open',istop,iret)
!      open surface guess file
       call nemsio_open(gfile_sfcges,trim(fname_sfcges),'read',iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcges),null,'open',istop,iret)
!      open surface gcycle file
       call nemsio_open(gfile_sfcgcy,trim(fname_sfcgcy),'read',iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcgcy),null,'open',istop,iret)

!      read a few surface guess file header records
       call nemsio_getfilehead(gfile_sfcges, nrec=nrec_sfc, idate=idate, &
          dimx=lonb, dimy=latb, nfhour=nfhour, nfminute=nfminute, &
          nfsecondn=nfsecondn, nfsecondd=nfsecondd, iret=iret)

!      read some nsst guess file header records (dimensions)
       call nemsio_getfilehead(gfile_nstges, nrec=nrec_nst, dimx=lonb_nst,dimy=latb_nst,iret=iret)

       write(6,*) 'nrec_sfc, nrec_nst = ',nrec_sfc, nrec_nst

!      check the dimensions consistency in sfc, nst files and the used.
       if ( latb /= latb_nst .or. lonb /= lonb_nst ) then
          write(6,*) 'Inconsistent dimension for sfc & nst files. latb,lonb : ',latb,lonb, &
                     'latb_nst,lonb_nst : ',latb_nst,lonb_nst
          call stop2(80)
       endif

       if ( nlat_sfc /= latb+2 .or. nlon_sfc /= lonb ) then
          write(6,*) 'Inconsistent dimension for used and read. nlat_sfc,nlon_sfc : ',nlat_sfc,nlon_sfc, &
                     'latb+2,lonb :',latb+2,lonb
          call stop2(81)
       endif
!      
       allocate(slmsk_ges(lonb,latb),slmsk_anl(lonb,latb))
       allocate(rwork1d(lonb*latb))

!      read slmsk in fname_sfcges to get slmsk_ges
       call nemsio_readrecv(gfile_sfcges, 'land', 'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcges),'land','read',istop,iret)
       slmsk_ges=reshape(rwork1d,(/size(slmsk_ges,1),size(slmsk_ges,2)/))

!      read slmsk in fname_sfcgcy to get slmsk_anl
       call nemsio_readrecv(gfile_sfcgcy, 'land', 'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcgcy),'land','read',istop,iret)
       slmsk_anl=reshape(rwork1d,(/size(slmsk_anl,1),size(slmsk_anl,2)/))
!
!      Replace header record date with analysis time from iadate
!
       jdate(1) = iadate(1)  ! analysis year
       jdate(2) = iadate(2)  ! analysis month
       jdate(3) = iadate(3)  ! analysis day
       jdate(4) = iadate(4)  ! analysis hour
       jdate(5) = iadate(5)  ! analysis minute
       jdate(5) = 0          ! analysis minute
       jdate(6) = 0          ! analysis scaled seconds
       jdate(7) = idate(7)   ! analysis seconds multiplier

       nfhour=0              !  new forecast hour, zero at analysis time
       nfminute=0
       nfsecondn=0
       nfsecondd=100         ! default for denominator

       fhour    = zero
       odate(1) = jdate(4)   !hour
       odate(2) = jdate(2)   !month
       odate(3) = jdate(3)   !day
       odate(4) = jdate(1)   !year

       if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then
          write(6,*)'WRITE_NEMSIO_SFC_NST:  different grid dimensions analysis vs sfc. interpolating sfc temperature  ',&
               ', nlon,nlat-2=',nlon,nlatm2,' -vs- sfc file lonb,latb=',lonb,latb
          write(6,*) ' WRITE_NEMSIO_SFC_NST, nlon_sfc,nlat_sfc : ',  nlon_sfc,nlat_sfc
!
!         Get the expanded values for a surface type (0 = water now) and the new mask
!
          call int2_msk_glb_prep(dsfct_glb,isli_glb,dsfct_tmp,isli_tmp,nlat,nlon,0,nprep)
!
!         Get updated/analysis surface mask info from sfcgcy file
!
          call tran_gfssfc(slmsk_anl,work,lonb,latb)
          do j=1,lonb
             do i=1,latb+2
                isli_gsi(i,j) = nint(work(i,j))
             end do
          end do
!
!         Interpolate dsfct_tmp(nlat,nlon) to dsfct_gsi(nlat_sfc,nlon_sfc) with surface mask accounted
!
          call int22_msk_glb(dsfct_tmp,isli_tmp,rlats,rlons,nlat,nlon, &
                             dsfct_gsi,isli_gsi,rlats_sfc,rlons_sfc,nlat_sfc,nlon_sfc,0)
!
!         transform the dsfct_gsi(latb+2,lonb) to dsfct_anl(lonb,latb) for sfc file format
!
          do j = 1, latb
             do i = 1, lonb
                dsfct_anl(i,j) = dsfct_gsi(latb+2-j,i)
             end do
          end do

       else
!
!         transform the dsfct_glb(nlat,nlon) to dsfct_anl(lonb,latb) for sfc file
!         format when nlat == latb-2 & nlon = lonb
!
          do j=1,latb
             do i=1,lonb
                dsfct_anl(i,j)=dsfct_glb(latb+1-j,i)
             end do
          end do
       endif                 ! if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then

!
! Start to write output sfc file : fname_sfcanl & fname_nstanl
!      open new output file with new header gfile_sfcanl and gfile_nstanl with "write" access. 
!      Use this call to update header as well
!
!      copy input header info to output header info for sfcanl, need to do this before nemsio_close(gfile)
! 
       gfile_sfcanl=gfile_sfcgcy         
!      open nemsio sfcanl                                        
       call nemsio_open(gfile_sfcanl,trim(fname_sfcanl),'write',iret=iret, idate=jdate, nfhour=nfhour,&
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd )
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcanl),null,'open',istop,iret)

       gfile_sfctsk=gfile_sfcgcy         
!      open nemsio sfctsk                                        
       call nemsio_open(gfile_sfctsk,trim(fname_sfctsk),'write',iret=iret, idate=jdate, nfhour=nfhour,&
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd )
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfctsk),null,'open',istop,iret)
!
!      copy input header info to output header info for nstanl, need to do this before nemsio_close(gfile)
! 
       gfile_nstanl=gfile_nstges       
!      open nemsio nstanl                                        
       call nemsio_open(gfile_nstanl,trim(fname_nstanl),'write',iret=iret, idate=jdate, nfhour=nfhour,&
          nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd )
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),null,'open',istop,iret)
! Allocate work array (rwork1d) and tsea in sfc file
       allocate(tsea(lonb,latb))

! Allocate nsst variables
       allocate(xt(lonb,latb))
       allocate(xs(lonb,latb))
       allocate(xu(lonb,latb))
       allocate(xv(lonb,latb))
       allocate(xz(lonb,latb))
       allocate(zm(lonb,latb))
       allocate(xtts(lonb,latb))
       allocate(xzts(lonb,latb))
       allocate(dt_cool(lonb,latb))
       allocate(z_c(lonb,latb))
       allocate(c_0(lonb,latb))
       allocate(c_d(lonb,latb))
       allocate(w_0(lonb,latb))
       allocate(w_d(lonb,latb))
       allocate(d_conv(lonb,latb))
       allocate(ifd(lonb,latb))
       allocate(tref(lonb,latb))
       allocate(qrain(lonb,latb))
!
!      First copy entire data from sfcgcy to fname_anl, then do selective update
!
!      read the nrec_sfc variables from sfcgcy and then write then to sfcanl
!
       do n = 1, nrec_sfc
          call nemsio_readrec(gfile_sfcgcy,n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'readrec for gfile_sfcgcy,  nrec_sfc = ', n, '  Status = ', iret
          call nemsio_writerec(gfile_sfcanl,n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'writerec for gfile_sfcanl, nrec_sfc = ', n, '  Status = ', iret
          call nemsio_writerec(gfile_sfctsk,n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'writerec for gfile_sfctsk, nrec_sfc = ', n, '  Status = ', iret
       end do

       write(*,*) 'read gfile_sfcgcy, and the write to gfile_sfcanl, gfile_sfctsk' 
!
!      For sfcanl, Only tsea (sea surface temperature) will be updated in the SFC
!                  Need values from nstges for tref update
!      read tsea from sfcges
       call nemsio_readrecv(gfile_sfcges,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcges),'tmp','read',istop,iret)
       tsea=reshape(rwork1d,(/size(tsea,1),size(tsea,2)/))

!      For nstanl, Only tref (foundation temperature) is updated by analysis
!                  others are updated for snow melting case  
!      read 18 nsst variables from nstges
! xt
       call nemsio_readrecv(gfile_nstges, 'xt',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'xt','read',istop,iret)
       xt=reshape(rwork1d,(/size(xt,1),size(xt,2)/))
! xs 
       call nemsio_readrecv(gfile_nstges, 'xs',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'xs','read',istop,iret)
       xs=reshape(rwork1d,(/size(xs,1),size(xs,2)/))
! xu
       call nemsio_readrecv(gfile_nstges, 'xu',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'xu','read',istop,iret)
       xu=reshape(rwork1d,(/size(xu,1),size(xu,2)/))
! xv
       call nemsio_readrecv(gfile_nstges, 'xv',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'xv','read',istop,iret)
       xv=reshape(rwork1d,(/size(xv,1),size(xv,2)/))
! xz
       call nemsio_readrecv(gfile_nstges, 'xz',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'xz','read',istop,iret)
       xz=reshape(rwork1d,(/size(xz,1),size(xz,2)/))
! zm
       call nemsio_readrecv(gfile_nstges, 'zm',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'zm','read',istop,iret)
       zm=reshape(rwork1d,(/size(zm,1),size(zm,2)/))
! xtts
       call nemsio_readrecv(gfile_nstges, 'xtts',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'xtts','read',istop,iret)
       xtts=reshape(rwork1d,(/size(xtts,1),size(xtts,2)/))
! xzts
       call nemsio_readrecv(gfile_nstges, 'xzts',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'xzts','read',istop,iret)
       xzts=reshape(rwork1d,(/size(xzts,1),size(xzts,2)/))
! dt_cool
       call nemsio_readrecv(gfile_nstges, 'dtcool','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'dt_cool','read',istop,iret)
       dt_cool=reshape(rwork1d,(/size(dt_cool,1),size(dt_cool,2)/))
! z_c
       call nemsio_readrecv(gfile_nstges, 'zc','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'zc','read',istop,iret)
       z_c=reshape(rwork1d,(/size(z_c,1),size(z_c,2)/))
! c_0
       call nemsio_readrecv(gfile_nstges, 'c0','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'c0','read',istop,iret)
       c_0=reshape(rwork1d,(/size(c_0,1),size(c_0,2)/))
! c_d
       call nemsio_readrecv(gfile_nstges, 'cd','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'cd','read',istop,iret)
       c_d=reshape(rwork1d,(/size(c_d,1),size(c_d,2)/))
! w_0
       call nemsio_readrecv(gfile_nstges, 'w0','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'w0','read',istop,iret)
       w_0=reshape(rwork1d,(/size(w_0,1),size(w_0,2)/))
! w_d
       call nemsio_readrecv(gfile_nstges, 'wd','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'wd','read',istop,iret)
       w_d=reshape(rwork1d,(/size(w_d,1),size(w_d,2)/))
! tref
       call nemsio_readrecv(gfile_nstges, 'tref',  'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'tref','read',istop,iret)
       tref=reshape(rwork1d,(/size(tref,1),size(tref,2)/))
! d_conv
       call nemsio_readrecv(gfile_nstges, 'dconv',  'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'dconv','read',istop,iret)
       d_conv=reshape(rwork1d,(/size(d_conv,1),size(d_conv,2)/))
! ifd
       call nemsio_readrecv(gfile_nstges, 'ifd',  'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'ifd','read',istop,iret)
       ifd=reshape(rwork1d,(/size(ifd,1),size(ifd,2)/))
! qrain
       call nemsio_readrecv(gfile_nstges, 'qrain',  'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),'qrain','read',istop,iret)
       qrain=reshape(rwork1d,(/size(qrain,1),size(qrain,2)/))
!
!      update tref (in nst file) & tsea (in the surface file) when Tr analysis is on
!      reset NSSTM variables for new open water grids
!
       if ( nst_gsi > 2 ) then
!
!         For the new open water (sea ice just melted) grids, (1) set dsfct_anl = zero; (2) reset the NSSTM variables
!
!         Notes: slmsk_ges is the mask of the background
!                slmsk_anl is the mask of the analysis 
!
          where ( slmsk_anl(:,:) == zero .and. slmsk_ges(:,:) == two )

            dsfct_anl(:,:)        = zero

            xt(:,:)      = zero
            xs(:,:)      = zero
            xu(:,:)      = zero
            xv(:,:)      = zero
            xz(:,:)      = z_w_max
            zm(:,:)      = zero
            xtts(:,:)    = zero
            xzts(:,:)    = zero
            dt_cool(:,:) = zero
            z_c(:,:)     = zero
            c_0(:,:)     = zero
            c_d(:,:)     = zero
            w_0(:,:)     = zero
            w_d(:,:)     = zero
            d_conv(:,:)  = zero
            ifd(:,:)     = zero
            tref(:,:)    = tfrozen
            qrain(:,:)   = zero
          end where
!
!         update analysis variable: Tref (foundation temperature) for nst file
!
          where ( slmsk_anl(:,:) == zero )
             tref(:,:) = max(tref(:,:) + dsfct_anl(:,:),tfrozen)
          elsewhere
             tref(:,:) = tsea(:,:)
          end where
!
!         update SST: tsea for sfc file with NSST profile
!
          r_zsea1 = 0.001_r_single*real(zsea1)
          r_zsea2 = 0.001_r_single*real(zsea2)
          call dtzm_2d(xt,xz,dt_cool,z_c,slmsk_anl,r_zsea1,r_zsea2,lonb,latb,dtzm)

          where ( slmsk_anl(:,:) == zero )
             tsea(:,:) = max(tref(:,:) + dtzm(:,:), tfrozen)
          end where

       else          ! when (nst_gsi <= 2)

          do j=1,latb
             do i=1,lonb
                tref(i,j) = tsea(i,j)  ! keep tref as tsea before analysis
             end do
          end do
!
!         For the new open water (sea ice just melted) grids, reset the NSSTM variables
!
          where ( slmsk_anl(:,:) == zero .and. slmsk_ges(:,:) == two )

            xt(:,:)      = zero
            xs(:,:)      = zero
            xu(:,:)      = zero
            xv(:,:)      = zero
            xz(:,:)      = z_w_max
            zm(:,:)      = zero
            xtts(:,:)    = zero
            xzts(:,:)    = zero
            dt_cool(:,:) = zero
            z_c(:,:)     = zero
            c_0(:,:)     = zero
            c_d(:,:)     = zero
            w_0(:,:)     = zero
            w_d(:,:)     = zero
            d_conv(:,:)  = zero
            ifd(:,:)     = zero
            tref(:,:)    = tfrozen
            qrain(:,:)   = zero
          end where
!
!         update tsea when NO Tf analysis
!
          do j=1,latb
             do i=1,lonb
                tsea(i,j) = max(tsea(i,j) + dsfct_anl(i,j),tfrozen)
             end do
          end do

       endif                   ! if ( nst_gsi > 2 ) then
!
!      update tsea record in sfcanl
!
       rwork1d = reshape(tsea, (/size(rwork1d)/) )
       call nemsio_writerecv(gfile_sfcanl,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcanl),'tmp','write',istop,iret)
       write(6,100) fname_sfcanl,lonb,latb,houra,iadate(1:4),iret
100    format(' WRITE_NEMSIO_SFC_NST:  update tsea in ',a6,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)
!
!      update tsea record in sfctsk
!
       rwork1d = reshape(tsea, (/size(rwork1d)/) )
       call nemsio_writerecv(gfile_sfctsk,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfctsk),'tmp','write',istop,iret)
       write(6,101) fname_sfctsk,lonb,latb,houra,iadate(1:4),iret
101    format(' WRITE_NEMSIO_SFC_NST:  update tsea in ',a6,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)
!
!      update nsst records in nstanl
!
! slmsk
       rwork1d = reshape( slmsk_anl,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'slmsk','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'slmsk','write',istop,iret)
! xt
       rwork1d = reshape( xt,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xt','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'xt','write',istop,iret)
! xs
       rwork1d = reshape( xs,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xs','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'xs','write',istop,iret)
! xu
       rwork1d = reshape( xu,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xu','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'xu','write',istop,iret)
! xv
       rwork1d = reshape( xv,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xv','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'xv','write',istop,iret)
! xz
       rwork1d = reshape( xz,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xz','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'xz','write',istop,iret)
! zm
       rwork1d = reshape( zm,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'zm','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'zm','write',istop,iret)
! xtts
       rwork1d = reshape( xtts,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xtts','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'xtts','write',istop,iret)
! xzts
       rwork1d = reshape( xzts,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xzts','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'xzts','write',istop,iret)
! z_0
       rwork1d = reshape( dt_cool,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'dtcool','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'dtcool','write',istop,iret)
! z_c
       rwork1d = reshape( z_c,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'zc','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'zc','write',istop,iret)
! c_0
       rwork1d = reshape( c_0,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'c0','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'c0','write',istop,iret)
! c_d
       rwork1d = reshape( c_d,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'cd','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'cd','write',istop,iret)
! w_0
       rwork1d = reshape( w_0,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'w0','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'w0','write',istop,iret)
! w_d
       rwork1d = reshape( w_d,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'wd','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'wd','write',istop,iret)
! d_conv
       rwork1d = reshape( d_conv,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'dconv','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'dconv','write',istop,iret)
! ifd
       rwork1d = reshape( ifd,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'ifd','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'ifd','write',istop,iret)
! tref
       rwork1d = reshape( tref,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'tref','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'tref','write',istop,iret)
! qrain
       rwork1d = reshape( qrain,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'qrain','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),'qrain','write',istop,iret)

       write(6,200) fname_nstanl,lonb,latb,houra,iadate(1:4),iret
200    format(' WRITE_NEMSIO_SFC_NST:  update variables in ',a6,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

       deallocate(xt,xs,xu,xv,xz,zm,xtts,xzts,dt_cool,z_c,c_0,c_d,w_0,w_d,d_conv,ifd,tref,qrain)
       deallocate(rwork1d)

       call nemsio_close(gfile_sfcges, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcges),null,'close',istop,iret)

       call nemsio_close(gfile_sfcgcy, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcgcy),null,'close',istop,iret)

       call nemsio_close(gfile_nstges, iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstges),null,'close',istop,iret)

       call nemsio_close(gfile_sfcanl,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfcanl),null,'close',istop,iret)

       call nemsio_close(gfile_nstanl,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_nstanl),null,'close',istop,iret)

       call nemsio_close(gfile_sfctsk,iret=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_sfctsk),null,'close',istop,iret)

       write(6,'(a,'': nemsio sfc_nst anal written for lonb,latb= '',2i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
    endif
  end subroutine write_sfc_nst_

  subroutine error_msg_(sub_name,file_name,var_name,action,stop_code,error_code)
    use mpimod, only: mype
    use kinds, only: i_kind
    implicit none

    character(len=*), intent(in) :: sub_name,file_name,var_name,action
    integer(i_kind),  intent(in) :: stop_code, error_code

    if ( mype == 0 ) then
       select case (trim(action))
       case('init')
          write(6,'(a,'':  PROBLEM with nemsio_init, Status = '', i3)') &
             trim(sub_name), error_code
       case('open')
          write(6,'(a,'':  ***ERROR*** problem opening file '',a,'', Status = '', i3)') &
             trim(sub_name), trim(file_name), error_code
       case('close')
          write(6,'(a,'':  ***ERROR*** problem closing file '',a,'', Status = '', i3)') &
             trim(sub_name), trim(file_name), error_code
       case default
          write(6,'(a,'':  ***ERROR*** '',a,tr1,a,'',variable = '',a,'',Status = '',i3)') &
             trim(sub_name),trim(action),trim(file_name),trim(var_name),error_code
       end select
     end if
     if ( stop_code /= 0 ) call stop2(stop_code)
  end subroutine error_msg_

  subroutine intrp22(a,rlons_a,rlats_a,nlon_a,nlat_a, &
                     b,rlons_b,rlats_b,nlon_b,nlat_b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    intrp22 --- interpolates from one 2-d grid to another 2-d grid 
!                            like analysis to surface grid or vice versa
!   prgrmmr:     li -  initial version; org: np2
!
! abstract:      This routine interpolates a grid to b grid 
!
! program history log:
!
!   input argument list:
!     rlons_a - longitudes of input array
!     rlats_a - latitudes of input array
!     nlon_a  - number of longitude of input array
!     nlat_a  - number of latitude of input array
!     rlons_b - longitudes of output array
!     rlats_b - latitudes of output array
!     nlon_b  - number of longitude of output array
!     nlat_b  - number of latitude of output array
!     a       - input values 
!
!   output argument list:
!     b       - output values
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use kinds, only: r_kind,i_kind,r_single
    use constants, only: zero,one
    
    implicit none

! !INPUT PARAMETERS:
    integer(i_kind)                 ,intent(in   ) :: nlon_a,nlat_a,nlon_b,nlat_b
    real(r_kind), dimension(nlon_a) ,intent(in   ) :: rlons_a
    real(r_kind), dimension(nlat_a) ,intent(in   ) :: rlats_a
    real(r_kind), dimension(nlon_b) ,intent(in   ) :: rlons_b
    real(r_kind), dimension(nlat_b) ,intent(in   ) :: rlats_b

    real(r_single), dimension(nlon_a,nlat_a),intent(in   ) :: a  

! !OUTPUT PARAMETERS:
    real(r_single), dimension(nlon_b,nlat_b),intent(  out) :: b 

!   Declare local variables
    integer(i_kind) i,j,ix,iy,ixp,iyp
    real(r_kind) dx1,dy1,dx,dy,w00,w01,w10,w11,bout,dlat,dlon

!*****************************************************************************

    b=zero
!   Loop over all points to get interpolated value
    do j=1,nlat_b
       dlat=rlats_b(j)
       call grdcrd1(dlat,rlats_a,nlat_a,1)
       iy=int(dlat)
       iy=min(max(1,iy),nlat_a)
       dy  =dlat-iy
       dy1 =one-dy
       iyp=min(nlat_a,iy+1)

       do i=1,nlon_b
          dlon=rlons_b(i)
          call grdcrd1(dlon,rlons_a,nlon_a,1)
          ix=int(dlon)
          dx  =dlon-ix
          dx=max(zero,min(dx,one))
          dx1 =one-dx
          w00=dx1*dy1; w10=dx1*dy; w01=dx*dy1; w11=dx*dy

          ix=min(max(0,ix),nlon_a)
          ixp=ix+1
          if(ix==0) ix=nlon_a
          if(ixp==nlon_a+1) ixp=1
          bout=w00*a(ix,iy)+w01*a(ix,iyp)+w10*a(ixp,iy)+w11*a(ixp,iyp)
          b(i,j)=bout

       end do
    end do

    
!   End of routine
    return
  end subroutine intrp22

  subroutine tran_gfssfc(ain,aout,lonb,latb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tran_gfssfc     transform gfs surface file to analysis grid
!   prgmmr: derber          org: np2                date: 2003-04-10
!
! abstract: transform gfs surface file to analysis grid
!
! program history log:
!   2012-31-38  derber  - initial routine
!
!   input argument list:
!     ain      - input surface record on processor iope
!     lonb     - input number of longitudes
!     latb     - input number of latitudes
!
!   output argument list:
!     aout     - output transposed surface record
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: zero
  use sfcio_module, only: sfcio_realkind
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in ) :: lonb,latb
  real(sfcio_realkind),dimension(lonb,latb),intent(in ) :: ain
  real(r_single),dimension(latb+2,lonb),intent(out) :: aout

! Declare local variables
  integer(i_kind) i,j
  real(r_kind) sumn,sums
! of surface guess array
  sumn = zero
  sums = zero
  do i=1,lonb
     sumn = ain(i,1)    + sumn
     sums = ain(i,latb) + sums
  end do
  sumn = sumn/float(lonb)
  sums = sums/float(lonb)
!  Transfer from local work array to surface guess array
  do j = 1,lonb
     aout(1,j)=sums
     do i=2,latb+1
        aout(i,j) = ain(j,latb+2-i)
     end do
     aout(latb+2,j)=sumn
  end do

  return
  end subroutine tran_gfssfc

end module ncepnems_io

