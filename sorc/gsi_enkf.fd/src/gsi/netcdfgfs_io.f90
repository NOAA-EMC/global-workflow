module netcdfgfs_io
!$$$ module documentation block
!           .      .    .                                       .
! module:   netcdfgfs_io
!   prgmmr: Martin       org: NCEP/EMC                date: 2019-09-24
!
! abstract: This module contains routines which handle input/output
!           operations for NCEP FV3 GFS netCDF global atmospheric and surface files.
!
! program history log:
!   2019-09-24 Martin   Initial version.  Based on ncepnems_io
!
! Subroutines Included:
!   sub read_gfsnc       - driver to read fv3gfs netcdf atmospheric and surface
!   sub read_gfsnc_chem
!   sub read_gfsncatm    - read fv3gfs netcdf atmospheric file, scatter
!                         on grid to analysis subdomains
!   sub read_gfsncsfc    - read fv3gfs netcdf surface file, scatter on grid to
!                         analysis subdomains
!   sub read_gfsncsfc_anl- read ncep EnKF fv3gfs netcdf surface file, scatter on grid to
!                         analysis subdomains
!   sub write_gfsncsfc   - gather/write on grid ncep surface analysis file
!   sub read_gfsncnst    - read ncep nst file, scatter on grid to analysis subdomains
!   sub write_gfsnc_sfc_nst - gather/write on grid ncep surface & nst analysis file
!   sub intrp22         - interpolate from one grid to another grid (2D)
!   sub read_gfsnc_sfcnst  - read sfc hist file, including sfc and nst vars, scatter on grid to analysis subdomains
!
! Variable Definitions:
!
!   language: f90
!   machine:
!
! NOTE: This module adds capability to read netCDF FV3 first guess files
!       and to write netCDF FV3 analysis files using the ncio interface 
!       Using this is controled by a namelist argument "use_gfs_ncio"
!
!
!$$$ end documentation block

  use constants, only: zero,one,fv,r60,r3600
  implicit none

  private
  public read_gfsnc
  public read_gfsnc_chem
  public read_gfsncatm
  public read_gfsncsfc
  public read_gfsncsfc_anl
  public write_gfsncsfc
  public read_gfsncnst
  public write_gfsnc_sfc_nst
  public intrp22
  public tran_gfsncsfc
  public write_gfsncatm

  interface read_gfsnc
     module procedure read_
  end interface

  interface read_gfsnc_chem
     module procedure read_chem_
  end interface

  interface read_gfsncatm
     module procedure read_atm_
  end interface

  interface read_gfsncsfc
     module procedure read_gfsncsfc_
  end interface

  interface read_gfsncsfc_anl
     module procedure read_gfsncsfc_anl_
  end interface

  interface read_gfsncnst
     module procedure read_gfsncnst_
  end interface


  interface write_gfsncsfc
     module procedure write_sfc_
  end interface

  interface write_gfsnc_sfc_nst
     module procedure write_sfc_nst_
  end interface

  interface write_gfsncatm
     module procedure write_atm_
  end interface

  character(len=*),parameter::myname='netcdfgfs_io'

contains

  subroutine read_
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_gfsnc
!
!   prgrmmr: Martin
!
! abstract:
!
! program history log:
!   2019-09-24  Martin   - create routine based on read_nems
!   2022-03-23  Draper   - add option to include T2m and q2m in MetGuess
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!
!$$$ end documentation block

    use kinds, only: i_kind,r_kind
    use constants, only: qcmin
    use gridmod, only: sp_a,grd_a,lat2,lon2,nsig
    use guess_grids, only: ifilesig,nfldsig,ntguessig
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
    use jfunc, only: hofx_2m_sfcfile
    use gridmod, only: fv3_full_hydro

    implicit none

    character(len=*),parameter::myname_=myname//'*read_'
    character(24) filename
    integer(i_kind):: it, istatus, inner_vars, num_fields
    integer(i_kind):: i,j,k


    real(r_kind),pointer,dimension(:,:  ):: ges_ps_it  =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_z_it   =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_t2m_it   =>NULL()
    real(r_kind),pointer,dimension(:,:  ):: ges_q2m_it   =>NULL()
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
    real(r_kind),pointer,dimension(:,:,:):: ges_qr_it  => NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qs_it  => NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_qg_it  => NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_cf_it  => NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_ni_it  => NULL()
    real(r_kind),pointer,dimension(:,:,:):: ges_nr_it  => NULL()
    type(sub2grid_info) :: grd_t
    logical regional
    logical:: l_cld_derived,zflag,inithead
    type(gsi_bundle) :: atm_bundle
    type(gsi_grid)   :: atm_grid
    integer(i_kind),parameter :: n2d=2
    ! integer(i_kind),parameter :: n3d=8
    integer(i_kind),parameter :: n2d_2m=4
    integer(i_kind),parameter :: n3d=16
    character(len=4), parameter :: vars2d(n2d) = (/ 'z   ', 'ps  ' /)
    character(len=4), parameter :: vars2d_with2m(n2d_2m) = (/ 'z   ', 'ps  ','t2m ','q2m ' /)
    ! character(len=4), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
    !                                                 'vor ', 'div ', &
    !                                                 'tv  ', 'q   ', &
    !                                                 'cw  ', 'oz  ' /)
    character(len=4), parameter :: vars3d(n3d) = (/ 'u   ', 'v   ', &
                                                    'vor ', 'div ', &
                                                    'tv  ', 'q   ', &
                                                    'cw  ', 'oz  ', &
                                                    'ql  ', 'qi  ', &
                                                    'qr  ', 'qs  ', &
                                                    'qg  ', 'ni  ', &
                                                    'nr  ', 'cf  ' /)

    real(r_kind),pointer,dimension(:,:):: ptr2d   =>NULL()
    real(r_kind),pointer,dimension(:,:,:):: ptr3d =>NULL()

    regional=.false.
    inner_vars=1

    num_fields=min(n3d*grd_a%nsig+2,npe)
!  Create temporary communication information fore read routines
    call general_sub2grid_create_info(grd_t,inner_vars,grd_a%nlat,grd_a%nlon, &
          grd_a%nsig,num_fields,regional)

!   Allocate bundle used for reading members
    call gsi_gridcreate(atm_grid,lat2,lon2,nsig)
    if (hofx_2m_sfcfile) then
        call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-read',istatus,names2d=vars2d_with2m,names3d=vars3d)
    else
        call gsi_bundlecreate(atm_bundle,atm_grid,'aux-atm-read',istatus,names2d=vars2d,names3d=vars3d)
    endif

    if(istatus/=0) then
      write(6,*) myname_,': trouble creating atm_bundle'
      call stop2(999)
    endif

    do it=1,nfldsig

!      Read background fields into bundle
       if (hofx_2m_sfcfile) then
          if (mype==0) write(*,*) 'calling general_read_gfsatm_nc for 2m data', it
          write(filename,'(''sfcf'',i2.2)') ifilesig(it)
          call general_read_gfsatm_nc(grd_t,sp_a,filename,.true.,.true.,.true.,&
              atm_bundle,.true.,istatus)
          if (mype==0) write(*,*) 'done with general_read_gfsatm_nc for 2m data', it
       end if
       write(filename,'(''sigf'',i2.2)') ifilesig(it)
       if (fv3_full_hydro) then
          if (mype==0) write(*,*) 'calling general_read_gfsatm_allhydro_nc', it
          call general_read_gfsatm_allhydro_nc(grd_t,sp_a,filename,.true.,.true.,.true.,&
              atm_bundle,istatus) ! this loads cloud and precip
          if (mype==0) write(*,*) 'done with general_read_gfsatm_allhydro_nc', it 
       else
          if (mype==0) write(*,*) 'calling general_read_gfsatm_nc'
          call general_read_gfsatm_nc(grd_t,sp_a,filename,.true.,.true.,.true.,&
              atm_bundle,.true.,istatus)
          if (mype==0) write(*,*) 'done with general_read_gfsatm_nc'
       end if

       inithead=.false.
       zflag=.false.

!      Set values to actual MetGuess fields
       call set_guess_

       if (it==ntguessig) then
          if (mype==0) write(6,*)'Print guess field ... after set_guess'
          call prt_guess('guess')
       endif
       if (fv3_full_hydro) then
          do k=1, nsig
             do j=1, lon2
                do i=1, lat2
                   ! set lower bound to hydrometeors
                   if (associated(ges_ql_it)) ges_ql_it(i,j,k) = max(qcmin,ges_ql_it(i,j,k))
                   if (associated(ges_qi_it)) ges_qi_it(i,j,k) = max(qcmin,ges_qi_it(i,j,k))
                   if (associated(ges_qr_it)) ges_qr_it(i,j,k) = max(qcmin,ges_qr_it(i,j,k))
                   if (associated(ges_qs_it)) ges_qs_it(i,j,k) = max(qcmin,ges_qs_it(i,j,k))
                   if (associated(ges_qg_it)) ges_qg_it(i,j,k) = max(qcmin,ges_qg_it(i,j,k))
                   if (associated(ges_ni_it)) ges_ni_it(i,j,k) = max(qcmin,ges_ni_it(i,j,k))
                   if (associated(ges_nr_it)) ges_nr_it(i,j,k) = max(qcmin,ges_nr_it(i,j,k))
                   if (associated(ges_cf_it)) ges_cf_it(i,j,k) = min(max(zero,ges_cf_it(i,j,k)),one)
                enddo
             enddo
          enddo
       else
          l_cld_derived = associated(ges_cwmr_it).and.&
                          associated(ges_q_it)   .and.&
                          associated(ges_ql_it)  .and.&
                          associated(ges_ni_it)  .and.&
                          associated(ges_nr_it)  .and.&
                          associated(ges_qi_it)  .and.& 
                          associated(ges_tv_it)
!         call set_cloud_lower_bound(ges_cwmr_it)
          if (mype==0) write(6,*)'READ_GFS_NETCDF: l_cld_derived = ', l_cld_derived

          if (l_cld_derived) then
             call cloud_calc_gfs(ges_ql_it,ges_qi_it,ges_cwmr_it,ges_q_it,ges_tv_it,.true.)
          end if
       end if
       if (it==ntguessig) then
          if (mype==0) write(6,*)'Print guess field ... after set lower bound for hydrometers'
          call prt_guess('guess')
       endif

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
    call gsi_bundlegetpointer (atm_bundle,'t2m',ptr2d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'t2m' ,ges_t2m_it ,istatus)
       if(istatus==0) ges_t2m_it = ptr2d
    endif
    call gsi_bundlegetpointer (atm_bundle,'q2m',ptr2d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'q2m' ,ges_q2m_it ,istatus)
       if(istatus==0) ges_q2m_it = ptr2d
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
    call gsi_bundlegetpointer (atm_bundle,'ql',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ql',ges_ql_it,istatus)
       if(istatus==0) ges_ql_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'ni',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'ni',ges_ni_it,istatus)
       if(istatus==0) ges_ni_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'nr',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'nr',ges_nr_it,istatus)
       if(istatus==0) ges_nr_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'qi',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qi',ges_qi_it,istatus)
       if(istatus==0) ges_qi_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'qr',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qr',ges_qr_it,istatus)
       if(istatus==0) ges_qr_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'qs',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qs',ges_qs_it,istatus)
       if(istatus==0) ges_qs_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'qg',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'qg',ges_qg_it,istatus)
       if(istatus==0) ges_qg_it = ptr3d
    endif
    call gsi_bundlegetpointer (atm_bundle,'cf',ptr3d,istatus)
    if (istatus==0) then
       call gsi_bundlegetpointer (gsi_metguess_bundle(it),'cf',ges_cf_it,istatus)
       if(istatus==0) ges_cf_it = ptr3d
    endif
  end subroutine set_guess_

  end subroutine read_

  subroutine read_chem_ ( iyear, month,idd )
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    read_gfsnc_chem
!
!   prgrmmr: martin
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
!   2019-09-24  martin   - initial code, based on read_nems_chem
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
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
! subprogram:    read_gfsncatm    read GFS netCDF atm and send to all mpi tasks
!   prgmmr: Martin            org: NCEP/EMC                date: 2019-09-23
!
! abstract: read ncep netcdf/gfs atmospheric guess field and
!           scatter to subdomains
!
! program history log:
!   2019-09-23 Martin    Initial version.  Based on sub read_nemsatm
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
!
!$$$
    use kinds, only: r_kind,i_kind, r_single
    use gridmod, only: ntracer,ncloud,reload,itotsub
    use general_commvars_mod, only: fill_ns,filluv_ns,fill2_ns,filluv2_ns,ltosj_s,ltosi_s
    use general_specmod, only: spec_vars
    use general_sub2grid_mod, only: sub2grid_info
    use mpimod, only: npe,mpi_comm_world,ierror,mpi_rtype,mype
    use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                quantize_data,close_dataset, get_dim, read_vardata, get_idate_from_time_units 
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
    character(len=120) :: my_name = 'READ_GFSNCATM'
    integer(i_kind),dimension(6):: idate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: nlatm2,nflds
    integer(i_kind) :: k,icount,icount_prev,mm1,i,j,kk,kr
    integer(i_kind) :: mype_hs, mype_ps,nord_int
    integer(i_kind) :: latb, lonb, levs
    real(r_kind),allocatable,dimension(:,:) :: grid, grid_v, &
         grid_vor, grid_div, grid_b, grid_b2
    real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid2, grid_c2
    real(r_kind),allocatable,dimension(:)   :: work, work_vor, work_div, &
         work_v
    real(r_kind),allocatable,dimension(:,:) :: sub, sub_vor, sub_div, &
         sub_v
    real(r_kind),dimension(sp_a%nc):: spec_vor,spec_div
    real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
    real(r_kind),allocatable,dimension(:) :: rlats_tmp,rlons_tmp
    real(r_single),allocatable, dimension(:,:) :: rwork2d, rwork2d1
    real(r_single),allocatable, dimension(:,:,:) :: rwork3d, rwork3d1
    real(r_single),allocatable, dimension(:) :: fhour
    logical diff_res,eqspace
    logical,dimension(1) :: vector
    type(egrid2agrid_parm) :: p_high
    type(Dataset) :: atmges
    type(Dimension) :: ncdim

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

      ! open the netCDF file
      atmges = open_dataset(filename)
      ! get dimension sizes
      ncdim = get_dim(atmges, 'grid_xt'); lonb = ncdim%len
      ncdim = get_dim(atmges, 'grid_yt'); latb = ncdim%len
      ncdim = get_dim(atmges, 'pfull'); levs = ncdim%len
      
      ! get time information
      idate = get_idate_from_time_units(atmges)
      odate(1) = idate(4)  !hour
      odate(2) = idate(2)  !month
      odate(3) = idate(3)  !day
      odate(4) = idate(1)  !year
      call read_vardata(atmges, 'time', fhour) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker
!
!  g_* array already pre-allocate as (lat2,lon2,<nsig>) => 2D and <3D> array
!
      diff_res=.false.
      if(latb /= nlatm2) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlatm2 = '',i4,tr1,''latb = '',i4)') &
            trim(my_name),nlatm2,latb
      end if
      if(lonb /= grd%nlon) then
         diff_res=.true.
         if ( mype == 0 ) write(6, &
            '(a,'': different spatial dimension nlon   = '',i4,tr1,''lonb = '',i4)') &
            trim(my_name),grd%nlon,lonb
      end if
      if(levs /= grd%nsig)then
         if ( mype == 0 ) write(6, &
            '(a,'': inconsistent spatial dimension nsig   = '',i4,tr1,''levs = '',i4)') &
            trim(my_name),grd%nsig,levs
         call stop2(101)
      end if

      ! get lat/lon coordinates

      allocate( grid(grd%nlon,nlatm2), grid_v(grd%nlon,nlatm2) )
      if(diff_res)then
         allocate( grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid2(grd%nlat,grd%nlon,1))
         allocate( grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
      end if
      allocate( rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb))
      call read_vardata(atmges, 'grid_xt', rlons_tmp)
      call read_vardata(atmges, 'grid_yt', rlats_tmp)
      do j=1,latb
        rlats(j+1)=deg2rad*rlats_tmp(j)
      end do
      do j=1,lonb
        rlons(j)=deg2rad*rlons_tmp(j)
      end do
      deallocate(rlats_tmp,rlons_tmp)
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
          call read_vardata(atmges, 'hgtsfc', rwork2d)
          if(diff_res)then
             grid_b=rwork2d
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=rwork2d
             call fill_ns(grid,work)
          end if
       endif
       call mpi_scatterv(work,grd%ijn_s,grd%displs_s,mpi_rtype,&
          g_z,grd%ijn_s(mm1),mpi_rtype,mype_hs,mpi_comm_world,ierror)
    end if

!   Surface pressure:  same procedure as terrain, but handled by task mype_ps
!
    if (mype==mype_ps) then
       call read_vardata(atmges, 'pressfc', rwork2d)
       rwork2d = r0_001*rwork2d
       if(diff_res)then
          vector(1)=.false.
          grid_b=rwork2d
          call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
          call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
          do kk=1,itotsub
            i=ltosi_s(kk)
            j=ltosj_s(kk)
            work(kk)=grid2(i,j,1)
          end do
       else
          grid=rwork2d
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
    call read_vardata(atmges, 'ugrd', rwork3d)
    call read_vardata(atmges, 'vgrd', rwork3d1)
! TODO CRM, would above be faster if only done on one PE and then distributed?
    do k=1,levs
       kr = levs+1-k ! netcdf is top to bottom need to flip
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          ! Convert grid u,v to div and vor
          if(diff_res)then
             grid_b=rwork3d(:,:,kr)
             grid_b2=rwork3d1(:,:,kr)
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
             grid=rwork3d(:,:,kr)
             grid_v=rwork3d1(:,:,kr)
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
    call read_vardata(atmges, 'spfh', rwork3d)
    call read_vardata(atmges, 'tmp', rwork3d1)
    allocate(rwork2d1(lonb,latb))
    do k=1,levs
       kr = levs+1-k ! netcdf is top to bottom need to flip
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          if(diff_res)then
             grid_b=rwork3d(:,:,kr)
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=rwork3d(:,:,kr)
             call fill_ns(grid,work)
          end if

          rwork2d1 = rwork3d1(:,:,kr)*(one+fv*rwork3d(:,:,kr))
          if(diff_res)then
             grid_b=rwork2d1
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work_v(kk)=grid2(i,j,1)
             end do
          else
             grid_v=rwork2d1
             call fill_ns(grid_v,work_v)
          end if

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
    deallocate(rwork2d1)
    call reload(sub_v,g_tv)
    call reload(sub,g_q)
    deallocate(sub_v,work_v)

    sub=zero
    icount=0
    icount_prev=1
    call read_vardata(atmges, 'o3mr', rwork3d) ! need k thrown in here somewhere
    do k=1,levs
       kr = levs+1-k ! netcdf is top to bottom need to flip
       icount=icount+1
       if (mype==mod(icount-1,npe)) then
          if(diff_res)then
             grid_b=rwork3d(:,:,kr)
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
             do kk=1,itotsub
               i=ltosi_s(kk)
               j=ltosj_s(kk)
               work(kk)=grid2(i,j,1)
             end do
          else
             grid=rwork3d(:,:,kr)
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
       call read_vardata(atmges, 'clwmr', rwork3d)
       call read_vardata(atmges, 'icmr', rwork3d1)
       do k=1,levs
          kr = levs+1-k ! netcdf is top to bottom need to flip
          icount=icount+1
          if (mype==mod(icount-1,npe)) then
             rwork2d = rwork3d(:,:,kr) + rwork3d1(:,:,kr)
             if(diff_res)then
                grid_b=rwork2d
                vector(1)=.false.
                call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
                call g_egrid2agrid(p_high,grid_c,grid2,1,1,vector)
                do kk=1,itotsub
                  i=ltosi_s(kk)
                  j=ltosj_s(kk)
                  work(kk)=grid2(i,j,1)
                end do
             else
                grid=rwork2d
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
       deallocate(clons,slons)
       deallocate(grid,grid_v)
       call close_dataset(atmges)
    end if
    deallocate(work,sub)

!   Print date/time stamp
    if ( mype == 0 ) write(6, &
       '(a,'': ges read/scatter,lonb,latb,levs= '',3i6,'',hour= '',f4.1,'',idate= '',4i5)') &
       trim(my_name),lonb,latb,levs,fhour,odate

  end subroutine read_atm_

  subroutine read_sfc_(sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                           veg_type,soil_type,terrain,isli,use_sfc_any, &
                           tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d) 
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_sfc_     read netCDF sfc hist file
!   prgmmr: Martin            org: NCEP/EMC                date: 2019-09-23
!
! abstract: read nems sfc & nst combined file
!
! program history log:
!   2019-09-23 Martin    Initial version.
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
!     tref      - optional, oceanic foundation temperature
!     dt_cool   - optional, sub-layer cooling amount at sub-skin layer
!     z_c       - optional, depth of sub-layer cooling layer
!     dt_warm   - optional, diurnal warming amount at sea surface 
!     z_w       - optional, depth of diurnal warming layer
!     c_0       - optional, coefficient to calculate d(Tz)/d(tr) in dimensionless
!     c_d       - optional, coefficient to calculate d(Tz)/d(tr) in m^-1
!     w_0       - optional, coefficient to calculate d(Tz)/d(tr) in dimensionless
!     w_d       - optional, coefficient to calculate d(Tz)/d(tr) in m^-1

!
! attributes:
!   language: f90
!
!$$$
    use mpimod, only: mype
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldsfc,ifilesfc
    use constants, only: zero,two
    use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                           close_dataset, get_dim, read_vardata, get_idate_from_time_units 
    implicit none

!   Declare passed variables
    logical,                                               intent(in)  :: use_sfc_any
    real(r_single),  dimension(nlat_sfc,nlon_sfc,nfldsfc), intent(out) :: sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough
    real(r_single),  dimension(nlat_sfc,nlon_sfc),         intent(out) :: veg_type,soil_type,terrain
    integer(i_kind), dimension(nlat_sfc,nlon_sfc),         intent(out) :: isli
    real(r_single),  optional, dimension(nlat_sfc,nlon_sfc,nfldsfc), intent(out) :: tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d
                                
!   Declare local parameters
    integer(i_kind), parameter :: nsfc_all=11
    integer(i_kind),dimension(6):: idate
    integer(i_kind),dimension(4):: odate
!   Declare local variables
    real(r_single),  dimension(nlat_sfc,nlon_sfc,nfldsfc) :: xt
    character(len=24)  :: filename
    character(len=120) :: my_name = 'READ_SFCNST'
    integer(i_kind) :: i,j,it,n,nsfc,iret
    integer(i_kind) :: lonb, latb
    real(r_single),allocatable, dimension(:)  :: fhour
    real(r_single), allocatable, dimension(:,:) :: work,outtmp
    type(Dataset) :: sfcges
    type(Dimension) :: ncdim
!-----------------------------------------------------------------------------

    do it = 1, nfldsfc
! read a surface history file on the task
       write(filename,200)ifilesfc(it)
200    format('sfcf',i2.2)

       ! open the netCDF file
       sfcges = open_dataset(filename,errcode=iret)
       if (iret/=0) then
          write(6,*) trim(my_name),':  ***ERROR*** ',trim(filename),' NOT AVAILABLE:  PROGRAM STOPS, later'
       endif

       ! get dimension sizes
       ncdim = get_dim(sfcges, 'grid_xt'); lonb = ncdim%len
       ncdim = get_dim(sfcges, 'grid_yt'); latb = ncdim%len

       ! get time information
       idate = get_idate_from_time_units(sfcges)
       odate(1) = idate(4)  !hour
       odate(2) = idate(2)  !month
       odate(3) = idate(3)  !day
       odate(4) = idate(1)  !year
       call read_vardata(sfcges, 'time', fhour) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker

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
       work    = zero

       if(it == 1)then
         nsfc=nsfc_all
       else
         nsfc=nsfc_all-4
       end if

       do n = 1, nsfc

          if (n == 1) then                               ! skin temperature

!            Tsea
             call read_vardata(sfcges, 'tmpsfc', work)
             call tran_gfsncsfc(work,sfct(1,1,it),lonb,latb)

          elseif(n == 2 .and. use_sfc_any) then          ! soil moisture

!            smc/soilw
             call read_vardata(sfcges, 'soilw1', work)
             call tran_gfsncsfc(work,soil_moi(1,1,it),lonb,latb)

          elseif(n == 3) then                            ! snow depth

             call read_vardata(sfcges, 'weasd', work)
             call tran_gfsncsfc(work,sno(1,1,it),lonb,latb)

          elseif(n == 4 .and. use_sfc_any) then          ! soil temperature

!            stc/tmp
             call read_vardata(sfcges, 'soilt1', work)
             call tran_gfsncsfc(work,soil_temp(1,1,it),lonb,latb)

          elseif(n == 5 .and. use_sfc_any) then          ! vegetation cover

!            vfrac
             call read_vardata(sfcges, 'veg', work)
             call tran_gfsncsfc(work,veg_frac(1,1,it),lonb,latb)

          elseif(n == 6) then                            ! 10m wind factor

!            f10m
             call read_vardata(sfcges, 'f10m', work)
             call tran_gfsncsfc(work,fact10(1,1,it),lonb,latb)

          elseif(n == 7) then                            ! suface roughness

!            zorl
             call read_vardata(sfcges, 'sfcr', work)
             call tran_gfsncsfc(work,sfc_rough(1,1,it),lonb,latb)

          elseif(n == 8 .and. use_sfc_any) then          ! vegetation type

!            vtype
             call read_vardata(sfcges, 'vtype', work)
             call tran_gfsncsfc(work,veg_type,lonb,latb)

          elseif(n == 9 .and. use_sfc_any) then          ! soil type

!            stype
             call read_vardata(sfcges, 'sotyp', work)
             call tran_gfsncsfc(work,soil_type,lonb,latb)

          elseif(n == 10) then                           ! terrain

!            orog
             call read_vardata(sfcges, 'orog', work)
             call tran_gfsncsfc(work,terrain,lonb,latb)

          elseif(n == 11) then                           ! sea/land/ice flag

!            slmsk
             call read_vardata(sfcges, 'land', work)
             allocate(outtmp(latb+2,lonb))
             call tran_gfsncsfc(work,outtmp,lonb,latb)
             do j=1,lonb
                do i=1,latb+2
                   isli(i,j) = nint(outtmp(i,j))
                end do
             end do
             deallocate(outtmp)

          endif
!      End of loop over data records
       enddo

       if( present(tref) ) then                         
          if ( mype == 0 ) write(6,*) ' read 9 optional NSST variables '

          call read_vardata(sfcges, 'tref', work)
          call tran_gfsncsfc(work,tref(1,1,it),lonb,latb)

          call read_vardata(sfcges, 'dtcool', work)
          call tran_gfsncsfc(work,dt_cool(1,1,it),lonb,latb)

          call read_vardata(sfcges, 'zc', work)
          call tran_gfsncsfc(work,z_c(1,1,it),lonb,latb)

          call read_vardata(sfcges, 'xt', work)
          call tran_gfsncsfc(work,xt(1,1,it),lonb,latb)

          call read_vardata(sfcges, 'xz', work)
          call tran_gfsncsfc(work,z_w(1,1,it),lonb,latb)
 
          call read_vardata(sfcges, 'c0', work)
          call tran_gfsncsfc(work,c_0(1,1,it),lonb,latb)

          call read_vardata(sfcges, 'cd', work)
          call tran_gfsncsfc(work,c_d(1,1,it),lonb,latb)

          call read_vardata(sfcges, 'w0', work)
          call tran_gfsncsfc(work,w_0(1,1,it),lonb,latb)

          call read_vardata(sfcges, 'wd', work)
          call tran_gfsncsfc(work,w_d(1,1,it),lonb,latb)
!
!         Get diurnal warming amout at z=0
!
          do j = 1,nlon_sfc
             do i = 1,nlat_sfc
                if (z_w(i,j,it) > zero) then
                   dt_warm(i,j,it) = two*xt(i,j,it)/z_w(i,j,it)
                end if
             end do
          end do
       endif
!      Deallocate local work arrays
       deallocate(work)

       call close_dataset(sfcges)
!
!      Print date/time stamp
       if ( mype == 0 ) write(6, &
          '(a,'': sfc read,nlon,nlat= '',2i6,'',hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
!   End of loop over time levels
    end do
  end subroutine read_sfc_

  subroutine read_gfsncsfc_(iope,sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                               veg_type,soil_type,terrain,isli,use_sfc_any, &
                               tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfsncsfc_     read netcdf sfc hist file
!   prgmmr: martin          org: NCEP/EMC                date: 2019-09-23
!
! abstract: read netcdf surface file
!
! program history log:
!   2019-09-23 Martin    Initial version.
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
!     tref      - oceanic foundation temperature
!     dt_cool   - optional, sub-layer cooling amount at sub-skin layer
!     z_c       - optional, depth of sub-layer cooling layer
!     dt_warm   - optional, diurnal warming amount at sea surface 
!     z_w       - optional, depth of diurnal warming layer
!     c_0       - optional, coefficient to calculate d(Tz)/d(tf) 
!     c_d       - optional, coefficient to calculate d(Tz)/d(tf)
!     w_0       - optional, coefficient to calculate d(Tz)/d(tf) 
!     w_d       - optional, coefficient to calculate d(Tz)/d(tf) 
!
! attributes:
!   language: f90
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat_sfc,nlon_sfc
    use guess_grids, only: nfldsfc,sfcmod_mm5,sfcmod_gfs
    use mpimod, only: mpi_itype,mpi_rtype4,mpi_comm_world,mype
    use constants, only: zero
    implicit none

!   Declare passed variables
    integer(i_kind),                                       intent(in)  :: iope
    logical,                                               intent(in)  :: use_sfc_any
    real(r_single),  dimension(nlat_sfc,nlon_sfc,nfldsfc), intent(out) :: sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough
    real(r_single),  dimension(nlat_sfc,nlon_sfc),         intent(out) :: veg_type,soil_type,terrain
    integer(i_kind), dimension(nlat_sfc,nlon_sfc),         intent(out) :: isli
    real(r_single), optional, dimension(nlat_sfc,nlon_sfc,nfldsfc), intent(out) :: tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d

!   Declare local variables
    integer(i_kind):: iret,npts,nptsall

!-----------------------------------------------------------------------------
!   Read surface history file on processor iope
    if(mype == iope)then
       if ( present(tref) ) then
          call read_sfc_(sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                         veg_type,soil_type,terrain,isli,use_sfc_any, &
                         tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)
          write(*,*) 'read_sfc netcdf, with NSST variables'
       else
          call read_sfc_(sfct,soil_moi,sno,soil_temp,veg_frac,fact10,sfc_rough, &
                         veg_type,soil_type,terrain,isli,use_sfc_any)
          write(*,*) 'read_sfc netcdf, without NSST variables'
       endif
    endif

!   Load onto all processors

    npts=nlat_sfc*nlon_sfc
    nptsall=npts*nfldsfc

    call mpi_bcast(sfct,      nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(fact10,    nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(sno,       nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    if(sfcmod_mm5 .or. sfcmod_gfs)then
       call mpi_bcast(sfc_rough, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    else
       sfc_rough = zero
    endif
    call mpi_bcast(terrain, npts,   mpi_rtype4,iope,mpi_comm_world,iret)
    call mpi_bcast(isli,    npts,   mpi_itype, iope,mpi_comm_world,iret)
    if(use_sfc_any)then
       call mpi_bcast(veg_frac, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_temp,nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_moi, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(veg_type, npts,   mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(soil_type,npts,   mpi_rtype4,iope,mpi_comm_world,iret)
    endif
    if ( present(tref) ) then
       call mpi_bcast(tref,    nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(dt_cool, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(z_c,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(dt_warm, nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(z_w,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(c_0,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(c_d,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(w_0,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
       call mpi_bcast(w_d,     nptsall,mpi_rtype4,iope,mpi_comm_world,iret)
    endif

  end subroutine read_gfsncsfc_

  subroutine read_sfc_anl_(isli_anl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_sfc_anl_     read netcdf surface file with analysis resolution
!
!   prgmmr: martin         org: NCEP/EMC            date: 2019-09-23
!
! abstract: read netcdf surface file at analysis grids when nlon /= nlon_sfc or nlat /= nlat_sfc
!
! program history log:
!   2019-09-23 Martin    Initial version.
!
!   input argument list:
!
!   output argument list:
!     isli      - sea/land/ice mask
!
! attributes:
!   language: f90
!
!$$$
    use mpimod, only: mype
    use kinds, only: r_kind,i_kind,r_single
    use gridmod, only: nlat,nlon
    use constants, only: zero
    use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                           close_dataset, get_dim, read_vardata, get_idate_from_time_units 
    implicit none

!   Declare passed variables
    integer(i_kind), dimension(nlat,nlon),   intent(  out) :: isli_anl

!   Declare local parameters
    integer(i_kind),dimension(6):: idate
    integer(i_kind),dimension(4):: odate


!   Declare local variables
    character(len=24)  :: filename
    character(len=120) :: my_name = 'READ_GFSNCSFC_ANL'
    integer(i_kind) :: i,j,iret
    integer(i_kind) :: lonb, latb
    real(r_single),allocatable, dimension(:) :: fhour
    real(r_single), allocatable, dimension(:,:) :: work,outtmp
    type(Dataset) :: sfcges
    type(Dimension) :: ncdim

!-----------------------------------------------------------------------------

    filename='sfcf06_anlgrid'
    ! open the netCDF file
    sfcges = open_dataset(filename,errcode=iret)
    if (iret/=0) then
       write(6,*) trim(my_name),':  ***FATAL ERROR*** ',trim(filename),' NOT AVAILABLE: PROGRAM STOPS'
       call stop2(999)
    endif

    ! get dimension sizes
    ncdim = get_dim(sfcges, 'grid_xt'); lonb = ncdim%len
    ncdim = get_dim(sfcges, 'grid_yt'); latb = ncdim%len

    ! get time information
    idate = get_idate_from_time_units(sfcges)
    odate(1) = idate(4)  !hour
    odate(2) = idate(2)  !month
    odate(3) = idate(3)  !day
    odate(4) = idate(1)  !year
    call read_vardata(sfcges, 'time', fhour) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker

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
    work    = zero

!   slmsk
    call read_vardata(sfcges, 'land', work)
    allocate(outtmp(latb+2,lonb))
    call tran_gfsncsfc(work,outtmp,lonb,latb)
    do j=1,lonb
       do i=1,latb+2
          isli_anl(i,j) = nint(outtmp(i,j))
       end do
    end do
    deallocate(outtmp)

!   Deallocate local work arrays
    deallocate(work)
    call close_dataset(sfcges)

!
!   Print date/time stamp
    if ( mype == 0 ) write(6, &
       '(a,'': read_sfc_anl_ ,nlon,nlat= '',2i6,'',hour= '',f4.1,'',idate= '',4i5)') &
       trim(my_name),lonb,latb,fhour,odate
  end subroutine read_sfc_anl_

  subroutine read_gfsncsfc_anl_(iope,isli_anl)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfsncsfc_anl     read netcdf surface guess file with analysis resolution
!
!   prgmmr: Martin         org: NCEP/EMC              date: 2019-09-23
!
! abstract: read netcdf surface file at analysis grids
!
! program history log:
!   2019-09-23 Martin    Initial version.
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
       write(*,*) 'read_sfc netcdf'
    end if

!   Load onto all processors
    npts=nlat*nlon
    call mpi_bcast(isli_anl,npts,mpi_itype,iope,mpi_comm_world,iret)

  end subroutine read_gfsncsfc_anl_

  subroutine read_nst_ (tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_nst_     read netcdf nst surface guess file (quadratic
!                                 Gaussin grids) without scattering to tasks
!   prgmmr: Martin         org: NCEP/EMC             date: 2019-09-23
!
! abstract: read netcdf surface NST file
!
! program history log:
!   2019-09-23  Martin    Initial version based on sub read_nemsnst
!
!   input argument list:
!
!   output argument list:
!   tref     (:,:)                ! oceanic foundation temperature
!   dt_cool  (:,:)                ! sub-layer cooling amount at sub-skin layer
!   z_c      (:,:)                ! depth of sub-layer cooling layer
!   dt_warm  (:,:)                ! diurnal warming amount at sea surface (skin layer)
!   z_w      (:,:)                ! depth of diurnal warming layer
!   c_0      (:,:)                ! coefficient to calculate d(Tz)/d(tr) 
!   c_d      (:,:)                ! coefficient to calculate d(Tz)/d(tr) 
!   w_0      (:,:)                ! coefficient to calculate d(Tz)/d(tr) 
!   w_d      (:,:)                ! coefficient to calculate d(Tz)/d(tr) 
!
! attributes:
!   language: f90
!
!$$$
    use kinds, only: r_kind,i_kind,r_single
    use mpimod, only: mype
    use gridmod, only: nlat_sfc,nlon_sfc
    use constants, only: zero,two
    use guess_grids, only: nfldnst,ifilenst
    use module_ncio, only: Dataset, Variable, Dimension, open_dataset,&
                           close_dataset, get_dim, read_vardata, get_idate_from_time_units 
    implicit none

!   Declare passed variables
    real(r_single) , dimension(nlat_sfc,nlon_sfc,nfldnst), intent(  out) ::  &
         tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d
!   Declare local parameters
    integer(i_kind),parameter    :: n_nst=9
    integer(i_kind),dimension(6) :: idate
    integer(i_kind),dimension(4) :: odate

!   Declare local variables
    character(len=6)   :: filename
    character(len=120) :: my_name = 'READ_GFSNCNST'
    integer(i_kind) :: i,j,it,latb,lonb
    real(r_single),allocatable, dimension(:) :: fhour
    real(r_single), dimension(nlat_sfc,nlon_sfc,nfldnst) :: xt
    real(r_single), allocatable, dimension(:,:) :: work
    type(Dataset) :: sfcges
    type(Dimension) :: ncdim

!-----------------------------------------------------------------------------

    do it=1,nfldnst
! read a nst file on the task
       write(filename,200)ifilenst(it)
200    format('nstf',i2.2)

       ! open the netCDF file
       sfcges = open_dataset(filename)
       ! get dimension sizes
       ncdim = get_dim(sfcges, 'grid_xt'); lonb = ncdim%len
       ncdim = get_dim(sfcges, 'grid_yt'); latb = ncdim%len

       ! get time information
       idate = get_idate_from_time_units(sfcges)
       odate(1) = idate(4)  !hour
       odate(2) = idate(2)  !month
       odate(3) = idate(3)  !day
       odate(4) = idate(1)  !year
       call read_vardata(sfcges, 'time', fhour) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker

       if ( (latb /= nlat_sfc-2) .or. (lonb /= nlon_sfc) ) then
          if ( mype == 0 ) &
             write(6,'(a,'': inconsistent spatial dimension nlon,nlatm2 = '',2(i4,tr1),''-vs- sfc file lonb,latb = '',i4)') &
             trim(my_name),nlon_sfc,nlat_sfc-2,lonb,latb
          call stop2(80)
       endif
!
!      Load surface fields into local work array
!
       allocate(work(lonb,latb))
       work    = zero

!      Tref
       call read_vardata(sfcges, 'tref', work)
       call tran_gfsncsfc(work,tref(1,1,it),lonb,latb)

!      dt_cool
       call read_vardata(sfcges, 'dtcool', work)
       call tran_gfsncsfc(work,dt_cool(1,1,it),lonb,latb)

!      z_c
       call read_vardata(sfcges, 'zc', work)
       call tran_gfsncsfc(work,z_c(1,1,it),lonb,latb)

!      xt
       call read_vardata(sfcges, 'xt', work)
       call tran_gfsncsfc(work,xt(1,1,it),lonb,latb)

!      xz
       call read_vardata(sfcges, 'xz', work)
       call tran_gfsncsfc(work,z_w(1,1,it),lonb,latb)
!
!      c_0
       call read_vardata(sfcges, 'c0', work)
       call tran_gfsncsfc(work,c_0(1,1,it),lonb,latb)

!      c_d
       call read_vardata(sfcges, 'cd', work)
       call tran_gfsncsfc(work,c_d(1,1,it),lonb,latb)

!      w_0
       call read_vardata(sfcges, 'w0', work)
       call tran_gfsncsfc(work,w_0(1,1,it),lonb,latb)

!      w_d
       call read_vardata(sfcges, 'wd', work)
       call tran_gfsncsfc(work,w_d(1,1,it),lonb,latb)

!
!      Get diurnal warming amout at z=0
!
       do j = 1,nlon_sfc
          do i = 1,nlat_sfc
             if (z_w(i,j,it) > zero) then
                dt_warm(i,j,it) = two*xt(i,j,it)/z_w(i,j,it)
             end if
          end do
       end do

!      Deallocate local work arrays
       deallocate(work)

       call close_dataset(sfcges)
!   End of loop over time levels
    end do
  end subroutine read_nst_


  subroutine read_gfsncnst_ (iope,tref,dt_cool,z_c,dt_warm,z_w,c_0,c_d,w_0,w_d)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_gfsnc_nst
!   prgmmr: Martin          org: NCEP/EMC          date: 2019-09-23
!
! abstract: read netcdf nst fields from a specific task and then broadcast to others
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
       write(*,*) 'read_nst netcdf'
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

  end subroutine read_gfsncnst_

  subroutine write_atm_ (grd,sp_a,filename,mype_out,gfs_bundle,ibin)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_ncatm --- Gather, transform, and write out to netcdf
!
!   prgmmr: whitaker         org: oar                  date: 2019-10-03
!
! abstract: This routine gathers fields needed for the GSI analysis
!           file from subdomains and then transforms the fields from
!           analysis grid to model guess grid, then written to an
!           netcdf atmospheric analysis file.
!
! program history log:
!   2019-10-03  whitaker initial version
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

    use constants, only: r1000,fv,one,zero,qcmin,r0_05,t0c

    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world
    use mpimod, only: ierror
    use mpimod, only: mype

    use guess_grids, only: ifilesig
    use guess_grids, only: load_geop_hgt,geop_hgti,ges_geopi

    use gridmod, only: ntracer
    use gridmod, only: ncloud
    use gridmod, only: strip,jcap_b,bk5

    use general_commvars_mod, only: load_grid,fill2_ns,filluv2_ns
    use general_specmod, only: spec_vars

    use obsmod, only: iadate

    use gsi_4dvar, only: ibdate,nhr_obsbin,lwrite4danl
    use general_sub2grid_mod, only: sub2grid_info
    use egrid2agrid_mod,only: g_egrid2agrid,g_create_egrid2agrid,egrid2agrid_parm,destroy_egrid2agrid
    use constants, only: two,pi,half,deg2rad
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use cloud_efr_mod, only: cloud_calc_gfs

    use netcdf, only: nf90_max_name
    use module_ncio, only: open_dataset, close_dataset, Dimension, Dataset,&
         read_attribute, write_attribute,get_dim, create_dataset, write_vardata, read_vardata,&
         get_idate_from_time_units,quantize_data,get_time_units_from_idate,has_attr,has_var
    use ncepnems_io, only: error_msg

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
    character(len=120) :: my_name = 'WRITE_GFSNCATM'
    character(len=1)   :: null = ' '
    integer(i_kind),dimension(6):: idate,jdate
    integer(i_kind) :: k, mm1, nlatm2, nord_int, i, j, kk, kr, nbits
    integer(i_kind) :: iret, lonb, latb, levs, istatus
    integer(i_kind) :: nfhour
    integer(i_kind) :: istop = 104
    integer(i_kind),dimension(5):: mydate
    integer(i_kind),dimension(8) :: ida,jda
    real(r_kind),dimension(5)    :: fha
    real(r_kind), allocatable, dimension(:)  :: fhour
    real(r_kind),allocatable,dimension(:) :: rlats_tmp,rlons_tmp

    real(r_kind),pointer,dimension(:,:) :: sub_ps
    real(r_kind),pointer,dimension(:,:,:) :: sub_u,sub_v,sub_tv
    real(r_kind),pointer,dimension(:,:,:) :: sub_q,sub_oz,sub_cwmr

    real(r_kind),dimension(grd%lat2,grd%lon2,grd%nsig) :: sub_dzb,sub_dza

    real(r_kind),dimension(grd%lat1*grd%lon1)     :: psm
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: tvsm, usm, vsm
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: qsm, ozsm
    real(r_kind),dimension(grd%lat1*grd%lon1,grd%nsig):: cwsm, dzsm
    real(r_kind),dimension(max(grd%iglobal,grd%itotsub))     :: work1,work2
    real(r_kind),dimension(grd%nlon,grd%nlat-2):: grid
    real(r_kind),allocatable,dimension(:) :: rlats,rlons,clons,slons
    real(r_kind),allocatable,dimension(:,:) :: grid_b,grid_b2
    real(r_kind),allocatable,dimension(:,:,:) :: grid_c, grid3, grid_c2, grid3b
    real(4), allocatable, dimension(:,:) :: values_2d,values_2d_tmp
    real(4), allocatable, dimension(:,:,:) :: values_3d,values_3d_tmp,ug3d,vg3d
    real(4) compress_err
    type(Dataset) :: atmges, atmanl
    type(Dimension) :: ncdim
    character(len=nf90_max_name) :: time_units

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

       ! open the netCDF file
       atmges = open_dataset(fname_ges,errcode=iret)
       if ( iret /= 0 ) call error_msg(trim(my_name),null,null,'open',istop,iret)
       ! get dimension sizes
       ncdim = get_dim(atmges, 'grid_xt'); lonb = ncdim%len
       ncdim = get_dim(atmges, 'grid_yt'); latb = ncdim%len
       ncdim = get_dim(atmges, 'pfull'); levs = ncdim%len

       if ( levs /= grd%nsig ) then
          write(6,*) trim(my_name),': problem in data dimension background levs = ',levs,' nsig = ',grd%nsig
          call stop2(103)
       endif
       
       ! get time information
       idate = get_idate_from_time_units(atmges)
       call read_vardata(atmges, 'time', fhour) ! might need to change this to attribute later
                                               ! depends on model changes from Jeff Whitaker
       nfhour = fhour(1)

       atmanl = create_dataset(filename, atmges, &
                               copy_vardata=.true., errcode=iret)
       if ( iret /= 0 ) call error_msg(trim(my_name),trim(filename),null,'open',istop,iret)

       ! Update time information (with ibdate) and write it to analysis file
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
       jdate(6) = 0          ! analysis second
       fhour = zero

       call write_vardata(atmanl, 'time', fhour)
       time_units = get_time_units_from_idate(jdate)
       call write_attribute(atmanl, 'units', time_units, 'time')

       ! Allocate structure arrays to hold data
       allocate(values_3d_tmp(lonb,latb,levs),values_2d_tmp(lonb,latb))
       allocate(grid3b(grd%nlat,grd%nlon,1))
       allocate( grid_b(lonb,latb),grid_c(latb+2,lonb,1),grid3(grd%nlat,grd%nlon,1))
       allocate( grid_b2(lonb,latb),grid_c2(latb+2,lonb,1))
       allocate( rlats(latb+2),rlons(lonb),clons(lonb),slons(lonb))
       call read_vardata(atmges, 'grid_xt', rlons_tmp, errcode=iret)
       call read_vardata(atmges, 'grid_yt', rlats_tmp, errcode=iret)
       do j=1,latb
         rlats(latb+2-j)=deg2rad*rlats_tmp(j)
       enddo
       rlats(1)=-half*pi
       rlats(latb+2)=half*pi
       do j=1,lonb
         rlons(j)=deg2rad*rlons_tmp(j)
       enddo
       deallocate(rlons_tmp, rlats_tmp)
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

       deallocate(rlats,rlons)

    endif ! if ( mype == mype_out )

    ! compute delz (so that delz < 0 as model expects)
    do k=1,grd%nsig
       sub_dzb(:,:,k) = ges_geopi(:,:,k,ibin) - ges_geopi(:,:,k+1,ibin)
    enddo

    if ((.not. lwrite4danl) .or. ibin == 1) call load_geop_hgt
    do k=1,grd%nsig
       sub_dza(:,:,k) = geop_hgti(:,:,k,ibin) - geop_hgti(:,:,k+1,ibin)
    enddo

    sub_dza = sub_dza - sub_dzb !sub_dza is increment
    
    ! Strip off boundary points from subdomains
    call strip(sub_ps  ,psm)
    call strip(sub_tv  ,tvsm  ,grd%nsig)
    call strip(sub_q   ,qsm   ,grd%nsig)
    call strip(sub_oz  ,ozsm  ,grd%nsig)
    call strip(sub_cwmr,cwsm  ,grd%nsig)
    call strip(sub_u   ,usm   ,grd%nsig)
    call strip(sub_v   ,vsm   ,grd%nsig)
    call strip(sub_dza ,dzsm  ,grd%nsig)

    ! Thermodynamic variable
    ! The GSI analysis variable is virtual temperature (Tv).   For NEMSIO
    ! output we need the sensible temperature.

    ! Convert Tv to T
    tvsm = tvsm/(one+fv*qsm)

    ! Generate and write analysis fields

    ! Surface pressure and delp.
    call mpi_gatherv(psm,grd%ijn(mm1),mpi_rtype,&
         work1,grd%ijn,grd%displs_g,mpi_rtype,&
         mype_out,mpi_comm_world,ierror)
    if (mype==mype_out) then
       call read_vardata(atmges,'pressfc',values_2d,errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pres','read',istop,iret)
       grid_b = r0_001*values_2d
       vector(1)=.false.
       call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
       call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
       do kk=1,grd%iglobal
          i=grd%ltosi(kk)
          j=grd%ltosj(kk)
          grid3(i,j,1)=work1(kk)-grid3(i,j,1)
          work1(kk)=grid3(i,j,1)
       end do
       if (has_var(atmges,'dpres')) then ! skip this if delp not in guess file.
          call read_vardata(atmges,'dpres',values_3d,errcode=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'dpres','read',istop,iret)
          do k=1,grd%nsig
             kr = grd%nsig-k+1
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)*(bk5(k)-bk5(k+1))
             enddo
             call g_egrid2agrid(p_high,grid3,grid_c2,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   values_3d(i,j,kr)=values_3d(i,j,kr)+r1000*(grid_c2(latb-j+2,i,1))
                enddo
             enddo
          enddo
          if (has_attr(atmges, 'nbits', 'dpres')) then
            call read_attribute(atmges, 'nbits', nbits, 'dpres')
            values_3d_tmp = values_3d
            call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
            call write_attribute(atmanl,&
            'max_abs_compression_error',compress_err,'dpres')
          endif
          call write_vardata(atmanl,'dpres',values_3d,errcode=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'dpres','write',istop,iret)
       endif
       do kk=1,grd%iglobal
          i=grd%ltosi(kk)
          j=grd%ltosj(kk)
          grid3(i,j,1)=work1(kk)
       enddo
       call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
       do j=1,latb
          do i=1,lonb
             grid_b(i,j)=r1000*(grid_b(i,j)+grid_c(latb-j+2,i,1))
          end do
       end do
       values_2d = grid_b
       if (has_attr(atmges, 'nbits', 'pressfc')) then
         call read_attribute(atmges, 'nbits', nbits, 'pressfc')
         values_2d_tmp = values_2d
         call quantize_data(values_2d_tmp, values_2d, nbits, compress_err)
         call write_attribute(atmanl,&
         'max_abs_compression_error',compress_err,'pressfc')
       endif
       call write_vardata(atmanl,'pressfc',values_2d,errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'pressfc','write',istop,iret)
    endif

!   u, v
    if (mype==mype_out) then
       if (allocated(values_3d)) deallocate(values_3d)
       call read_vardata(atmges, 'ugrd', ug3d, errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','read',istop,iret)
       call read_vardata(atmges, 'vgrd', vg3d, errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','read',istop,iret)
    endif
    do k=1,grd%nsig
       kr = grd%nsig-k+1
       call mpi_gatherv(usm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       call mpi_gatherv(vsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work2,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype==mype_out) then
          if(diff_res)then
             grid_b = ug3d(:,:,kr)
             grid_b2 = vg3d(:,:,kr)
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
             ug3d(:,:,kr) = grid_b
             vg3d(:,:,kr) = grid_b2
          else
             call load_grid(work1,grid)
             ug3d(:,:,kr) = grid
             call load_grid(work2,grid)
             vg3d(:,:,kr) = grid
          end if
       endif ! mype_out
    end do
    ! Zonal wind
    if (mype==mype_out) then
       if (has_attr(atmges, 'nbits', 'ugrd')) then
         call read_attribute(atmges, 'nbits', nbits, 'ugrd')
         values_3d_tmp = ug3d 
         call quantize_data(values_3d_tmp, ug3d, nbits, compress_err)
         call write_attribute(atmanl,&
         'max_abs_compression_error',compress_err,'ugrd')
       endif
       call write_vardata(atmanl,'ugrd',ug3d,errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'ugrd','write',istop,iret)
       ! Meridional wind
       if (has_attr(atmges, 'nbits', 'vgrd')) then
         call read_attribute(atmges, 'nbits', nbits, 'vgrd')
         values_3d_tmp = vg3d 
         call quantize_data(values_3d_tmp, vg3d, nbits, compress_err)
         call write_attribute(atmanl,&
         'max_abs_compression_error',compress_err,'vgrd')
       endif
       call write_vardata(atmanl,'vgrd',vg3d,errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'vgrd','write',istop,iret)
       deallocate(ug3d, vg3d)
    endif

!   Thermodynamic variable
    if (mype==mype_out) then
       call read_vardata(atmges, 'tmp', values_3d, errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','read',istop,iret)
    endif
    do k=1,grd%nsig
       kr = grd%nsig-k+1
       call mpi_gatherv(tvsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             grid_b=values_3d(:,:,kr)
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
             values_3d(:,:,kr) = grid_b
          else
             call load_grid(work1,grid)
             values_3d(:,:,kr) = grid
          end if
       endif
    end do
    if (mype==mype_out) then
       if (has_attr(atmges, 'nbits', 'tmp')) then
         call read_attribute(atmges, 'nbits', nbits, 'tmp')
         values_3d_tmp = values_3d 
         call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
         call write_attribute(atmanl,&
         'max_abs_compression_error',compress_err,'tmp')
       endif
       call write_vardata(atmanl,'tmp',values_3d,errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'tmp','write',istop,iret)
    endif

!   Specific humidity
    if (mype==mype_out) then
       call read_vardata(atmges, 'spfh', values_3d, errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','read',istop,iret)
    endif
    do k=1,grd%nsig
       kr = grd%nsig-k+1
       call mpi_gatherv(qsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             grid_b=values_3d(:,:,kr)
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
             values_3d(:,:,kr) = grid_b
          else
             call load_grid(work1,grid)
             values_3d(:,:,kr) = grid
          end if
       endif
    end do
    if (mype==mype_out) then
       if (has_attr(atmges, 'nbits', 'spfh')) then
         call read_attribute(atmges, 'nbits', nbits, 'spfh')
         values_3d_tmp = values_3d 
         call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
         call write_attribute(atmanl,&
         'max_abs_compression_error',compress_err,'spfh')
       endif
       call write_vardata(atmanl,'spfh',values_3d,errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'spfh','write',istop,iret)
    endif

!   Ozone
    if (mype==mype_out) then
       call read_vardata(atmges, 'o3mr', values_3d, errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'o3mr','read',istop,iret)
    endif
    do k=1,grd%nsig
       kr = grd%nsig-k+1
       call mpi_gatherv(ozsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if(diff_res)then
             grid_b=values_3d(:,:,kr)
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
             values_3d(:,:,kr) = grid_b
          else
             call load_grid(work1,grid)
             values_3d(:,:,kr) = grid
          end if
       endif
    end do
    if (mype==mype_out) then
       if (has_attr(atmges, 'nbits', 'o3mr')) then
         call read_attribute(atmges, 'nbits', nbits, 'o3mr')
         values_3d_tmp = values_3d 
         call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
         call write_attribute(atmanl,&
         'max_abs_compression_error',compress_err,'o3mr')
       endif
       call write_vardata(atmanl,'o3mr',values_3d,errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),'o3mr','write',istop,iret)
    endif

!   Cloud condensate mixing ratio
    if (ntracer>2 .or. ncloud>=1) then

       if (mype==mype_out) then
          if (allocated(values_3d)) deallocate(values_3d)
          call read_vardata(atmges, 'clwmr', ug3d, errcode=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'clwmr','read',istop,iret)
          call read_vardata(atmges, 'icmr', vg3d, errcode=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'icmr','read',istop,iret)
       endif

       do k=1,grd%nsig
          kr = grd%nsig-k+1
          call mpi_gatherv(cwsm(1,k),grd%ijn(mm1),mpi_rtype,&
               work1,grd%ijn,grd%displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          call mpi_gatherv(tvsm(1,k),grd%ijn(mm1),mpi_rtype,&
               work2,grd%ijn,grd%displs_g,mpi_rtype,&
               mype_out,mpi_comm_world,ierror)
          if (mype == mype_out) then
             grid_b = ug3d(:,:,kr)
             grid_b2=vg3d(:,:,kr)
             grid_b = grid_b + grid_b2
             vector(1)=.false.
             call fill2_ns(grid_b,grid_c(:,:,1),latb+2,lonb)
             call g_egrid2agrid(p_low,grid_c,grid3,1,1,vector)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)-max(grid3(i,j,1),qcmin)
                work2(kk) = -r0_05*(work2(kk) - t0c)
                work2(kk) = max(zero,work2(kk))
                work2(kk) = min(one,work2(kk))
                grid3b(i,j,1)=grid3(i,j,1)
                grid3(i,j,1)=grid3b(i,j,1)*(one - work2(kk))
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             grid_b = grid_b - grid_b2
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             ug3d(:,:,kr) = grid_b
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=grid3b(i,j,1)*work2(kk)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b2(i,j)=grid_b2(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             vg3d(:,:,kr) = grid_b2
          endif !mype == mype_out
       end do
       if (mype==mype_out) then
          if (has_attr(atmges, 'nbits', 'clwmr')) then
            call read_attribute(atmges, 'nbits', nbits, 'clwmr')
            values_3d_tmp = ug3d 
            call quantize_data(values_3d_tmp, ug3d, nbits, compress_err)
            call write_attribute(atmanl,&
            'max_abs_compression_error',compress_err,'clwmr')
          endif
          call write_vardata(atmanl,'clwmr',ug3d,errcode=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'clwmr','write',istop,iret)
          if (has_attr(atmges, 'nbits', 'icmr')) then
            call read_attribute(atmges, 'nbits', nbits, 'icmr')
            values_3d_tmp = vg3d 
            call quantize_data(values_3d_tmp, vg3d, nbits, compress_err)
            call write_attribute(atmanl,&
            'max_abs_compression_error',compress_err,'icmr')
          endif
          call write_vardata(atmanl,'icmr',vg3d,errcode=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'icmr','write',istop,iret)
          deallocate(ug3d, vg3d)
       endif
    endif !ntracer

! Variables needed by the Unified Post Processor (dzdt, delz, delp)
    if (mype==mype_out) then
       if (has_var(atmges,'delz')) then ! if delz in guess file, read it.
          call read_vardata(atmges, 'delz', values_3d, errcode=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'delz','read',istop,iret)
       endif
    endif
    do k=1,grd%nsig
       kr = grd%nsig-k+1
       call mpi_gatherv(dzsm(1,k),grd%ijn(mm1),mpi_rtype,&
            work1,grd%ijn,grd%displs_g,mpi_rtype,&
            mype_out,mpi_comm_world,ierror)
       if (mype == mype_out) then
          if (has_var(atmges,'delz')) then 
          if(diff_res)then
             grid_b=values_3d(:,:,kr)
             do kk=1,grd%iglobal
                i=grd%ltosi(kk)
                j=grd%ltosj(kk)
                grid3(i,j,1)=work1(kk)
             end do
             call g_egrid2agrid(p_high,grid3,grid_c,1,1,vector)
             do j=1,latb
                do i=1,lonb
                   grid_b(i,j)=grid_b(i,j)+grid_c(latb-j+2,i,1)
                end do
             end do
             values_3d(:,:,kr) = grid_b
          else
             call load_grid(work1,grid)
             values_3d(:,:,kr) = values_3d(:,:,kr) + grid
          end if
          end if
       endif
    end do
    if (mype==mype_out) then
       ! if delz in guess file, write to analysis file.
       if (has_var(atmges,'delz')) then 
          if (has_attr(atmges, 'nbits', 'delz')) then
            call read_attribute(atmges, 'nbits', nbits, 'delz')
            values_3d_tmp = values_3d 
            call quantize_data(values_3d_tmp, values_3d, nbits, compress_err)
            call write_attribute(atmanl,&
            'max_abs_compression_error',compress_err,'delz')
          endif
          call write_vardata(atmanl,'delz',values_3d,errcode=iret)
          if (iret /= 0) call error_msg(trim(my_name),trim(filename),'delz','write',istop,iret)
       endif
    endif
    
!
! Deallocate local array
!
    if (mype==mype_out) then
       deallocate(grid_b,grid_b2,grid_c,grid_c2,grid3,clons,slons)
       deallocate(grid3b)

       call close_dataset(atmges, errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(fname_ges),null,'close',istop,iret)

       call close_dataset(atmanl, errcode=iret)
       if (iret /= 0) call error_msg(trim(my_name),trim(filename),null,'close',istop,iret)
!
! Deallocate local array
!
       if (allocated(values_2d)) deallocate(values_2d)
       if (allocated(values_3d)) deallocate(values_3d)
       if (allocated(values_2d_tmp)) deallocate(values_2d_tmp)
       if (allocated(values_3d_tmp)) deallocate(values_3d_tmp)
!
       write(6,'(a,'': atm anal written for lonb,latb,levs= '',3i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,levs,fhour(1),jdate(1:4)
    endif

  end subroutine write_atm_


  subroutine write_sfc_ (filename,mype_sfc,dsfct)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_gfsncsfc --- Write surface analysis to file
!
!   prgmmr: Martin          org: NCEP/EMC            date: 2019-09-24
!
! abstract:     This routine writes the updated surface analysis.
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
!   2019-09-24  Martin    Initial version.  Based on write_nemssfc
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
    use netcdf, only: nf90_max_name
    use module_ncio, only: open_dataset, close_dataset, Dimension, Dataset,&
                           get_dim, create_dataset, write_vardata, read_vardata,&
                           get_time_units_from_idate, write_attribute  


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
    character(len=120) :: my_name = 'WRITE_GFSNCSFC'
    integer(i_kind),dimension(6):: jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: i, j, ip1, jp1, ilat, ilon, jj, mm1
    integer(i_kind) :: nlatm2, lonb, latb
    real(r_kind),allocatable,dimension(:)    :: fhour

    real(r_kind),dimension(lat1,lon1):: sfcsub
    real(r_kind),dimension(nlon,nlat):: grid
    real(r_kind),dimension(max(iglobal,itotsub)):: sfcall
    real(r_kind),allocatable,dimension(:,:) :: tsea
    real(r_single),dimension(nlon,nlat):: buffer
    real(r_single),allocatable,dimension(:,:) :: buffer2,grid2
    type(Dataset) :: sfcges,sfcanl
    type(Dimension) :: ncdim
    character(len=nf90_max_name) :: time_units

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
       ! open the netCDF file
       sfcges = open_dataset(fname_ges)

       ! get dimension sizes
       ncdim = get_dim(sfcges, 'grid_xt'); lonb = ncdim%len
       ncdim = get_dim(sfcges, 'grid_yt'); latb = ncdim%len

!
! Start to write output sfc file : filename
!
!      First copy entire data from fname_ges to filename, then do selective update
!
       sfcanl = create_dataset(filename, sfcges, copy_vardata=.true.) 
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

       allocate(fhour(1))
       fhour    = zero
       odate(1) = jdate(4)  !hour
       odate(2) = jdate(2)  !month
       odate(3) = jdate(3)  !day
       odate(4) = jdate(1)  !year

       call write_vardata(sfcanl, 'time', fhour)
       time_units = get_time_units_from_idate(jdate)
       call write_attribute(sfcanl, 'units', time_units, 'time')

       allocate(buffer2(lonb,latb))
       allocate(grid2(lonb,latb))
       allocate(tsea(lonb,latb))

!
! Only sea surface temperature will be updated in the SFC files
!

       call read_vardata(sfcges, 'tmpsfc', tsea)

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

       deallocate(buffer2)

!      update tsea record
       call write_vardata(sfcanl, 'tmpsfc', grid2)

       call close_dataset(sfcges)
       call close_dataset(sfcanl)

       write(6,'(a,'': sfc anal written for lonb,latb= '',2i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
    endif
  end subroutine write_sfc_

  subroutine write_sfc_nst_ (mype_so,dsfct)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_sfc_nst --- Write both sfc and nst surface analysis to file
!
!   prgmmr: Martin           org: NCEP/EMC            date: 2019-09-24
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
!   2019-09-24  Martin    initial version based on routine write_nems_sfc_nst
!
!   input argument list:
!     dsfct     - delta skin temperature
!     mype_so   - mpi task to write output file
!
!   output argument list:
!
! attributes:
!   language: f90
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

    use module_ncio, only: open_dataset, close_dataset, Dimension, Dataset,&
                           get_dim, create_dataset, write_vardata, read_vardata,&
                           get_time_units_from_idate, write_attribute  
    use netcdf, only: nf90_max_name

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
    integer(i_kind),dimension(7):: jdate
    integer(i_kind),dimension(4):: odate
    integer(i_kind) :: i, j, ip1, jp1, ilat, ilon, mm1
    integer(i_kind) :: lonb, latb, nlatm2
    integer(i_kind) :: lonb_nst, latb_nst
    real(r_kind),allocatable,dimension(:)    :: fhour
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

    type(Dataset) :: sfcges,sfcgcy,nstges,sfctsk,sfcanl,nstanl
    type(Dimension) :: ncdim
    character(len=nf90_max_name) :: time_units

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

      write(*,'(a,5(1x,a6))') 'write_gfsnc_sfc_nst:',fname_sfcges,fname_nstges,fname_sfctsk,fname_sfcanl,fname_nstanl
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

!      open nsst guess file
       nstges = open_dataset(fname_nstges)
!      open surface guess file
       sfcges = open_dataset(fname_sfcges)
!      open surface gcycle file
       sfcgcy = open_dataset(fname_sfcgcy)

!      read a few surface guess file header records
       ! get dimension sizes
       ncdim = get_dim(sfcges, 'grid_xt'); lonb = ncdim%len
       ncdim = get_dim(sfcges, 'grid_yt'); latb = ncdim%len

!      read some nsst guess file header records (dimensions)
       ! get dimension sizes
       ncdim = get_dim(nstges, 'grid_xt'); lonb_nst = ncdim%len
       ncdim = get_dim(nstges, 'grid_yt'); latb_nst = ncdim%len

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

!      read slmsk in fname_sfcges to get slmsk_ges
       call read_vardata(sfcges, 'land', slmsk_ges)

!      read slmsk in fname_sfcgcy to get slmsk_anl
       call read_vardata(sfcgcy, 'land', slmsk_anl)
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

       allocate(fhour(1))
       fhour    = zero
       odate(1) = jdate(4)  !hour
       odate(2) = jdate(2)  !month
       odate(3) = jdate(3)  !day
       odate(4) = jdate(1)  !year

       if ( (latb /= nlatm2) .or. (lonb /= nlon) ) then
          write(6,*)'WRITE_GFSNC_SFC_NST:  different grid dimensions analysis vs sfc. interpolating sfc temperature  ',&
               ', nlon,nlat-2=',nlon,nlatm2,' -vs- sfc file lonb,latb=',lonb,latb
          write(6,*) ' WRITE_GFSNC_SFC_NST, nlon_sfc,nlat_sfc : ',  nlon_sfc,nlat_sfc
!
!         Get the expanded values for a surface type (0 = water now) and the new mask
!
          call int2_msk_glb_prep(dsfct_glb,isli_glb,dsfct_tmp,isli_tmp,nlat,nlon,0,nprep)
!
!         Get updated/analysis surface mask info from sfcgcy file
!
          call tran_gfsncsfc(slmsk_anl,work,lonb,latb)
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
       sfcanl = create_dataset(fname_sfcanl, sfcgcy, copy_vardata=.true.)

       call write_vardata(sfcanl, 'time', fhour)
       time_units = get_time_units_from_idate(jdate)
       call write_attribute(sfcanl, 'units', time_units, 'time')

       sfctsk = create_dataset(fname_sfctsk, sfcgcy, copy_vardata=.true.)

       call write_vardata(sfctsk, 'time', fhour)
       time_units = get_time_units_from_idate(jdate)
       call write_attribute(sfctsk, 'units', time_units, 'time')

!
!      copy input header info to output header info for nstanl, need to do this before nemsio_close(gfile)
!
       nstanl = create_dataset(fname_nstanl, nstges, copy_vardata=.true.)

       call write_vardata(nstanl, 'time', fhour)
       time_units = get_time_units_from_idate(jdate)
       call write_attribute(nstanl, 'units', time_units, 'time')

!
!      For sfcanl, Only tsea (sea surface temperature) will be updated in the SFC
!                  Need values from nstges for tref update
!      read tsea from sfcges
       call read_vardata(sfcges, 'tmpsfc', tsea)

!      For nstanl, Only tref (foundation temperature) is updated by analysis
!                  others are updated for snow melting case
!      read 18 nsst variables from nstges
! xt
       call read_vardata(nstges, 'xt', xt)
! xs
       call read_vardata(nstges, 'xs', xs)
! xu
       call read_vardata(nstges, 'xu', xu)
! xv
       call read_vardata(nstges, 'xv', xv)
! xz
       call read_vardata(nstges, 'xz', xz)
! zm
       call read_vardata(nstges, 'zm', zm)
! xtts
       call read_vardata(nstges, 'xtts', xtts)
! xzts
       call read_vardata(nstges, 'xzts', xzts)
! dt_cool
       call read_vardata(nstges, 'dtcool', dt_cool)
! z_c
       call read_vardata(nstges, 'zc', z_c)
! c_0
       call read_vardata(nstges, 'c0', c_0)
! c_d
       call read_vardata(nstges, 'cd', c_d)
! w_0
       call read_vardata(nstges, 'w0', w_0) 
! w_d
       call read_vardata(nstges, 'wd', w_d)
! tref
       call read_vardata(nstges, 'tref', tref)
! d_conv
       call read_vardata(nstges, 'dconv', d_conv)
! ifd
! CRM - does this exist? what is it's name??
       !call read_vardata(nstges, 'ifd', ifd)
! qrain
       call read_vardata(nstges, 'qrain', qrain)
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
       call write_vardata(sfcanl, 'tmpsfc', tsea) 
       write(6,100) fname_sfcanl,lonb,latb,houra,iadate(1:4)
100    format(' WRITE_GFSNCIO_SFC_NST:  update tsea in ',a6,2i6,1x,f4.1,4(i4,1x))
!
!      update tsea record in sfctsk
!
       call write_vardata(sfctsk, 'tmpsfc', tsea)
       write(6,101) fname_sfctsk,lonb,latb,houra,iadate(1:4)
101    format(' WRITE_GFSNCIO_SFC_NST:  update tsea in ',a6,2i6,1x,f4.1,4(i4,1x))
!
!      update nsst records in nstanl
!
! slmsk
       call write_vardata(nstanl, 'land', slmsk_anl)
! xt
       call write_vardata(nstanl, 'xt', xt)
! xs
       call write_vardata(nstanl, 'xs', xs)
! xu
       call write_vardata(nstanl, 'xu', xu)
! xv
       call write_vardata(nstanl, 'xv', xv)
! xz
       call write_vardata(nstanl, 'xz', xz)
! zm
       call write_vardata(nstanl, 'zm', zm)
! xtts
       call write_vardata(nstanl, 'xtts', xtts)
! xzts
       call write_vardata(nstanl, 'xzts', xzts)
! z_0
       call write_vardata(nstanl, 'dtcool', dt_cool)
! z_c
       call write_vardata(nstanl, 'zc', z_c)
! c_0
       call write_vardata(nstanl, 'c0', c_0)
! c_d
       call write_vardata(nstanl, 'cd', c_d)
! w_0
       call write_vardata(nstanl, 'w0', w_0)
! w_d
       call write_vardata(nstanl, 'wd', w_d)
! d_conv
       call write_vardata(nstanl, 'dconv', d_conv)
! ifd
! CRM See above ifd issue/comment
       !call write_vardata(nstanl, 'ifd', ifd)
! tref
       call write_vardata(nstanl, 'tref', tref)
! qrain
       call write_vardata(nstanl, 'qrain', qrain)

       write(6,200) fname_nstanl,lonb,latb,houra,iadate(1:4)
200    format(' WRITE_GFSNCIO_SFC_NST:  update variables in ',a6,2i6,1x,f4.1,4(i4,1x))

       deallocate(xt,xs,xu,xv,xz,zm,xtts,xzts,dt_cool,z_c,c_0,c_d,w_0,w_d,d_conv,ifd,tref,qrain)

       call close_dataset(sfcges)
       call close_dataset(sfcgcy)
       call close_dataset(nstges)
       call close_dataset(sfcanl)
       call close_dataset(nstanl)
       call close_dataset(sfctsk)

       write(6,'(a,'': gfsncio sfc_nst anal written for lonb,latb= '',2i6,'',valid hour= '',f4.1,'',idate= '',4i5)') &
          trim(my_name),lonb,latb,fhour,odate
    endif
  end subroutine write_sfc_nst_


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

  subroutine tran_gfsncsfc(ain,aout,lonb,latb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tran_gfsncsfc     transform gfs surface file to analysis grid
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
  sumn = sumn/real(lonb,r_kind)
  sums = sums/real(lonb,r_kind)
!  Transfer from local work array to surface guess array
  do j = 1,lonb
     aout(1,j)=sums
     do i=2,latb+1
        aout(i,j) = ain(j,latb+2-i)
     end do
     aout(latb+2,j)=sumn
  end do

  return
  end subroutine tran_gfsncsfc

end module netcdfgfs_io

