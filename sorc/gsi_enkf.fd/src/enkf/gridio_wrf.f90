module gridio

  !========================================================================

  !$$$ Module documentation block
  ! 
  ! This module contains various routines to ingest and update
  ! variables from Weather Research and Forecasting (WRF) model Advanced
  ! Research WRF (ARW) and Non-hydrostatic Mesoscale Model (NMM) dynamical
  ! cores which are required by the Ensemble Kalman Filter (ENKF) currently
  ! designed for operations within the National Centers for Environmental
  ! Prediction (NCEP) Global Forecasting System (GFS)
  !
  ! prgmmr: Winterbottom        org: ESRL/PSD1       date: 2011-11-30
  !
  ! program history log:
  !   
  !   2011-11-30 Winterbottom - Initial version.
  !   2016-02-09 shlyaeva - update to read state and control variables;
  !                         arw control now has Tv, specific humidity and
  !                         surface pressure instead of Tp, mix ratio and
  !                         dry surf pressure. nmm control now has Tv 
  !                         instead of Tsens.
  !   2017-05-12 Y. Wang and X. Wang - add more state variables for radar DA,
  !                                    (Johnson et al. 2015 MWR; Wang and Wang
  !                                    2017 MWR) POC: xuguang.wang@ou.edu
  !   2021-02-08 CAPS(J. Park) 
  !                   - add cliptracer to remove negative hydrometeors
  !                     when updating analysis file.
  !                   - add subroutine 'writewrfvar_native' to minimize
  !                     interpolation error on wind analysis (A-C grid conversion)
  !
  ! attributes:
  !   language:  f95
  !
  !$$$

  !=========================================================================
  ! Define associated modules
  use gridinfo, only: dimensions, npts, cross2dot, dot2cross
  use kinds,    only: r_double, r_kind, r_single, i_kind
  use mpisetup, only: nproc
  use netcdf_io
  use params,   only: nlevs, cliptracers, datapath, arw, nmm, datestring, &
                      pseudo_rh, nmm_restart, l_use_enkf_directZDA
  use mpeu_util, only: getindex

  implicit none

  !-------------------------------------------------------------------------
  ! Define all public subroutines within this module
  private
  public :: readgriddata, readgriddata_pnc
  public :: writegriddata, writegriddata_pnc
  public :: writeincrement, writeincrement_pnc

  !-------------------------------------------------------------------------

contains
  ! Generic WRF read routine, calls ARW-WRF or NMM-WRF
  subroutine readgriddata(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,filesfcprefixes,reducedgrid,vargrid,qsat)
   use constants, only: max_varname_length
   implicit none
   integer, intent(in) :: nanal1,nanal2, n2d, n3d, ndim, ntimes
   character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
   character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
   integer, dimension(0:n3d), intent(in)        :: levels
   character(len=120), dimension(7), intent(in) :: fileprefixes
   character(len=120), dimension(7), intent(in) :: filesfcprefixes
   logical, intent(in) :: reducedgrid

   real(r_single), dimension(npts,ndim,ntimes),  intent(out) :: vargrid
   real(r_double), dimension(npts,nlevs,ntimes), intent(out) :: qsat

   if (arw) then
     call readgriddata_arw(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,filesfcprefixes,vargrid,qsat)
   else if (nmm) then
     call readgriddata_nmm(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,filesfcprefixes,vargrid,qsat)
   endif

  end subroutine readgriddata


  !========================================================================
  ! readgriddata_arw.f90: read WRF-ARW state or control vector
  !-------------------------------------------------------------------------
  subroutine readgriddata_arw(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,filesfcprefixes,vargrid,qsat)
    use constants

    !======================================================================
    ! Define variables passed to subroutine
    integer, intent(in)  :: nanal1, nanal2, n2d, n3d,ndim, ntimes
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, dimension(0:n3d), intent(in) :: levels
    character(len=120), dimension(7), intent(in)  :: fileprefixes
    character(len=120), dimension(7), intent(in)  :: filesfcprefixes

    ! Define variables returned by subroutine
    real(r_single), dimension(npts,ndim,ntimes,nanal2-nanal1+1),  intent(out) :: vargrid
    real(r_double), dimension(npts,nlevs,ntimes,nanal2-nanal1+1), intent(out) :: qsat

    ! Define local variables 
    character(len=500) :: filename
    character(len=500) :: filenamesfc
    character(len=7)   :: charnanal

    logical :: ice
    real(r_single), dimension(:),   allocatable :: znu, znw  ! aeta1 and eta1
    real(r_single), dimension(:),   allocatable :: enkf_mu, enkf_mub
    real(r_single), dimension(:,:), allocatable :: enkf_temp, enkf_virttemp
    real(r_single), dimension(:,:), allocatable :: enkf_pressure
    real(r_single), dimension(:),   allocatable :: enkf_psfc
    real(r_single), dimension(:,:), allocatable :: enkf_mixratio, enkf_spechumd
    real(r_single), dimension(:),   allocatable :: enkf_qintegral
    real(r_single) :: ptop

    ! Define variables required for netcdf variable I/O
    character(len=12) :: varstrname

    ! Define counting variables
    integer :: i, k, nb, ne, nanal
    integer :: u_ind, v_ind, tv_ind, q_ind, oz_ind, ql_ind, qr_ind, qi_ind, qg_ind, &
               qs_ind, qnc_ind, qnr_ind, qni_ind, dbz_ind, w_ind
    integer :: tsen_ind, prse_ind
    integer :: ps_ind, sst_ind

    !======================================================================
    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
    tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
    prse_ind = getindex(vars3d, 'prse') ! pressure
    ql_ind  = getindex(vars3d, 'ql')  ! QL (3D)
    qr_ind  = getindex(vars3d, 'qr')  ! QR (3D)
    qi_ind  = getindex(vars3d, 'qi')  ! QI (3D)
    qg_ind  = getindex(vars3d, 'qg')  ! QG (3D)
    qs_ind  = getindex(vars3d, 'qs')  ! QS (3D)
    qnc_ind  = getindex(vars3d, 'qnc')  ! QNC (3D)
    qnr_ind  = getindex(vars3d, 'qnr')  ! QNR (3D)
    qni_ind  = getindex(vars3d, 'qni')  ! QNI (3D)
    dbz_ind  = getindex(vars3d, 'dbz')  ! DBZ (3D)
    w_ind   = getindex(vars3d, 'w')  ! W (3D)

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
    sst_ind = getindex(vars2d, 'sst') ! SST (2D)

    ! Initialize all constants required by routine
    call init_constants(.true.)

    if (ntimes > 1) then
       write(6,*)'gridio/readgriddata: reading multiple backgrounds not yet supported'
       call stop2(23)
    endif

    ne = 0
    ensmemloop: do nanal=nanal1,nanal2
    ne = ne + 1
    backgroundloop: do nb=1,ntimes

    ! Define character string for ensemble member file
    if (nanal > 0) then
      write(charnanal,'(a3, i3.3)') 'mem', nanal
    else
      charnanal = 'ensmean'
    endif
    filename = trim(adjustl(datapath))//trim(adjustl(fileprefixes(nb)))//trim(charnanal)
    filenamesfc = trim(adjustl(datapath))//trim(adjustl(filesfcprefixes(nb)))//trim(charnanal)

    !----------------------------------------------------------------------
    ! read u-component
    if (u_ind > 0) then
       varstrname = 'U'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(u_ind-1)+1:levels(u_ind),nb,ne),nlevs)
       do k = levels(u_ind-1)+1, levels(u_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: u ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read v-component
    if (v_ind > 0) then
       varstrname = 'V'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(v_ind-1)+1:levels(v_ind),nb,ne),nlevs)
       do k = levels(v_ind-1)+1, levels(v_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: v ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read qcloud
    if ( ql_ind > 0 ) then
       varstrname = 'QCLOUD'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(ql_ind-1)+1:levels(ql_ind),nb,ne),nlevs)
       do k = levels(ql_ind-1)+1, levels(ql_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: ql ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read qrain
    if ( qr_ind > 0 ) then
       varstrname = 'QRAIN'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(qr_ind-1)+1:levels(qr_ind),nb,ne),nlevs)
       do k = levels(qr_ind-1)+1, levels(qr_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: qr ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read qice
    if ( qi_ind > 0 ) then
       varstrname = 'QICE'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(qi_ind-1)+1:levels(qi_ind),nb,ne),nlevs)
       do k = levels(qi_ind-1)+1, levels(qi_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: qi ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read qsnow
    if ( qs_ind > 0 ) then
       varstrname = 'QSNOW'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(qs_ind-1)+1:levels(qs_ind),nb,ne),nlevs)
       do k = levels(qs_ind-1)+1, levels(qs_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: qs ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read qgraup
    if ( qg_ind > 0 ) then
       varstrname = 'QGRAUP'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(qg_ind-1)+1:levels(qg_ind),nb,ne),nlevs)
       do k = levels(qg_ind-1)+1, levels(qg_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: qg ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read qncloud
    if ( qnc_ind > 0 ) then
       varstrname = 'QNCLOUD'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(qnc_ind-1)+1:levels(qnc_ind),nb,ne),nlevs)
       do k = levels(qnc_ind-1)+1, levels(qnc_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: qnc ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read qnice
    if ( qni_ind > 0 ) then
       varstrname = 'QNICE'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(qni_ind-1)+1:levels(qni_ind),nb,ne),nlevs)
       do k = levels(qni_ind-1)+1, levels(qni_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: qni ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read qnrain
    if ( qnr_ind > 0 ) then
       varstrname = 'QNRAIN'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(qnr_ind-1)+1:levels(qnr_ind),nb,ne),nlevs)
       do k = levels(qnr_ind-1)+1, levels(qnr_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: qnr ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read reflectivity
    if ( dbz_ind > 0 ) then
       varstrname = 'REFL_10CM'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(dbz_ind-1)+1:levels(dbz_ind),nb,ne),nlevs)
       do k = levels(dbz_ind-1)+1, levels(dbz_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: dbz ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read w
    if ( w_ind > 0 ) then
       varstrname = 'W'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(w_ind-1)+1:levels(w_ind),nb,ne),nlevs)
       do k = levels(w_ind-1)+1, levels(w_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: w ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! set ozone to zero for now (like in GSI?)
    if (oz_ind > 0) then
       vargrid(:,levels(oz_ind-1)+1:levels(oz_ind),nb,ne) = zero
    endif
    ! set SST to zero for now
    if (sst_ind > 0) then
       vargrid(:,levels(n3d)+sst_ind,nb,ne) = zero
    endif

    ice = .false.

    !----------------------------------------------------------------------
    ! Allocate memory for variables computed within routine
    if(.not. allocated(enkf_temp))      allocate(enkf_temp(npts,nlevs))
    if(.not. allocated(enkf_virttemp))  allocate(enkf_virttemp(npts,nlevs))
    if(.not. allocated(enkf_psfc))      allocate(enkf_psfc(npts))
    if(.not. allocated(enkf_qintegral)) allocate(enkf_qintegral(npts))
    if(.not. allocated(enkf_pressure))  allocate(enkf_pressure(npts,nlevs))
    if(.not. allocated(enkf_mixratio))  allocate(enkf_mixratio(npts,nlevs))
    if(.not. allocated(enkf_spechumd))  allocate(enkf_spechumd(npts,nlevs))

    !----------------------------------------------------------------------
    ! Ingest the perturbation potential temperature from the external file
    varstrname= 'T'
    call readwrfvar(filename, varstrname, enkf_temp, nlevs)

    ! Ingest the water vapor mixing ratio from the external file
    varstrname = 'QVAPOR'
    call readwrfvar(filename, varstrname, enkf_mixratio, nlevs)

    ! read pressure information
    call readpressure_arw(filename, znu, znw, enkf_mu, enkf_mub, ptop)

    ! compute surface pressure
    enkf_qintegral = one
    do i = 1, npts
       do k = 1, nlevs
          enkf_qintegral(i) = enkf_qintegral(i) +                          &
                              (znw(k) - znw(k+1))*enkf_mixratio(i,k)
       enddo
    enddo 

    ! compute dry surface pressure
    enkf_psfc = r0_01 * (enkf_mu + enkf_mub + ptop)
    ! compute full surface pressure
    enkf_psfc = (enkf_psfc - ptop) * enkf_qintegral + ptop

    ! compute specific humidity
    enkf_spechumd = enkf_mixratio / (one + enkf_mixratio)

    ! compute pressure 
    do k = 1, nlevs
      enkf_pressure(:,k) = r0_01 * (znu(k) * (100 * enkf_psfc - ptop) + ptop)
    enddo

    ! compute sensible temperature
    enkf_temp = (enkf_temp + 300.0) * (0.001 * enkf_pressure)**rd_over_cp_mass

    ! compute virtual temperature
    enkf_virttemp = enkf_temp * (1. + fv*enkf_spechumd)

    if (tsen_ind > 0) then
       vargrid(:,levels(tsen_ind-1)+1:levels(tsen_ind),nb,ne) = enkf_temp
       do k = levels(tsen_ind-1)+1, levels(tsen_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: tsen ',                        &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif

    if (q_ind > 0) then
       vargrid(:,levels(q_ind-1)+1:levels(q_ind),nb,ne) = enkf_spechumd
       do k = levels(q_ind-1)+1, levels(q_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: q ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif

    if (tv_ind > 0) then
       vargrid(:,levels(tv_ind-1)+1:levels(tv_ind),nb,ne) = enkf_virttemp
       do k = levels(tv_ind-1)+1, levels(tv_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: tv ',                          &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
 
    if (ps_ind > 0) then
       vargrid(:,levels(n3d)+ps_ind,nb,ne) = enkf_psfc
       k = levels(n3d) + ps_ind
       if (nproc .eq. 0)                                               &
          write(6,*) 'READGRIDDATA_ARW: ps ',                           &
              & minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
    endif

    if (prse_ind > 0) then
       vargrid(:,levels(prse_ind-1)+1:levels(prse_ind)-1, nb,ne) = enkf_pressure
       do k = levels(prse_ind-1)+1, levels(prse_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_ARW: prse ',                        &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif

    !----------------------------------------------------------------------
    ! Compute the saturation specific humidity

    if (pseudo_rh) then
       call genqsat1(enkf_spechumd,qsat(:,:,nb,ne),enkf_pressure,enkf_virttemp,ice,  &
                     npts,nlevs)
    else
       qsat(:,:,nb,ne) = 1._r_double
    endif
          
    !======================================================================
    ! Deallocate memory 
    if(allocated(enkf_mu))             deallocate(enkf_mu)
    if(allocated(enkf_mub))            deallocate(enkf_mub)
    if(allocated(enkf_temp))           deallocate(enkf_temp)
    if(allocated(enkf_psfc))           deallocate(enkf_psfc)
    if(allocated(enkf_qintegral))      deallocate(enkf_qintegral)
    if(allocated(enkf_virttemp))       deallocate(enkf_virttemp)
    if(allocated(enkf_pressure))       deallocate(enkf_pressure)
    if(allocated(enkf_mixratio))       deallocate(enkf_mixratio)
    if(allocated(enkf_spechumd))       deallocate(enkf_spechumd)

    end do backgroundloop ! loop over backgrounds to read in
    end do ensmemloop ! loop over ens members to read in

    return

  end subroutine readgriddata_arw

  !========================================================================
  ! readgriddata_nmm.f90: read WRF-NMM state or control vector
  !-------------------------------------------------------------------------

  subroutine readgriddata_nmm(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,ntimes,fileprefixes,filesfcprefixes,vargrid,qsat)
    use constants
    !======================================================================
    ! Define variables passed to subroutine
    integer, intent(in)  :: nanal1, nanal2, n2d, n3d, ndim, ntimes
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, dimension(0:n3d), intent(in) :: levels
    character(len=120), dimension(7), intent(in)  :: fileprefixes
    character(len=120), dimension(7), intent(in)  :: filesfcprefixes

    ! Define variables returned by subroutine
    real(r_single),  dimension(npts,ndim,ntimes,nanal2-nanal1+1),  intent(out) :: vargrid
    real(r_double),  dimension(npts,nlevs,ntimes,nanal2-nanal1+1), intent(out) :: qsat

    ! Define variables computed within subroutine
    logical :: ice
    real    :: pdtop, pt
    real, dimension(:), allocatable :: aeta1, aeta2
    real(r_single), dimension(:),   allocatable  :: enkf_psfc, enkf_pd
    real(r_single), dimension(:,:), allocatable  :: enkf_temp
    real(r_single), dimension(:,:), allocatable  :: enkf_pressure
    real(r_single), dimension(:,:), allocatable  :: enkf_spechumd

    character(len=12)  :: varstrname
    character(len=500) :: filename
    character(len=500) :: filenamesfc
    character(len=7)   :: charnanal

    ! Define counting variables
    integer(i_kind) :: nb, k, nanal, ne
    integer(i_kind) :: u_ind, v_ind, tv_ind, q_ind, oz_ind
    integer(i_kind) :: cw_ind, tsen_ind, prse_ind
    integer(i_kind) :: ps_ind, sst_ind

    !======================================================================
    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    oz_ind  = getindex(vars3d, 'oz')  ! Oz (3D)
    cw_ind  = getindex(vars3d, 'cw')  ! CW (3D)
    tsen_ind = getindex(vars3d, 'tsen') !sensible T (3D)
    prse_ind = getindex(vars3d, 'prse')

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)
    sst_ind = getindex(vars2d, 'sst')

    ! Initialize all constants required by routine
    call init_constants(.true.)
       
    !----------------------------------------------------------------------
    if (ntimes > 1) then
       write(6,*)'gridio/readgriddata: reading multiple backgrounds not yet supported'
       call stop2(23)
    endif

    ne = 0
    ensmemloop: do nanal=nanal1,nanal2
    ne = ne + 1
    backgroundloop: do nb=1,ntimes

    ! Define character string for ensemble member file
    if (nanal > 0) then
      write(charnanal,'(a3, i3.3)') 'mem', nanal
    else
      charnanal = 'ensmean'
    endif
    filename = trim(adjustl(datapath))//trim(adjustl(fileprefixes(nb)))//trim(charnanal)
    filenamesfc = trim(adjustl(datapath))//trim(adjustl(filesfcprefixes(nb)))//trim(charnanal)

    !----------------------------------------------------------------------
    ! read u-component
    if (u_ind > 0) then
       varstrname = 'U' 
       call readwrfvar(filename, varstrname,                               &
                       vargrid(:,levels(u_ind-1)+1:levels(u_ind),nb,ne), nlevs)
       do k = levels(u_ind-1)+1, levels(u_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: u ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read v-component
    if (v_ind > 0) then
       varstrname = 'V'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(v_ind-1)+1:levels(v_ind),nb,ne), nlevs)
       do k = levels(v_ind-1)+1, levels(v_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: v ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! read cwm
    if (cw_ind > 0) then
       varstrname = 'CWM'
       call readwrfvar(filename, varstrname,                              &
                       vargrid(:,levels(cw_ind-1)+1:levels(cw_ind),nb,ne), nlevs)
       do k = levels(cw_ind-1)+1, levels(cw_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: cw',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif
    ! set ozone to zero for now (like in GSI?)
    if (oz_ind > 0) then
       vargrid(:,levels(oz_ind-1)+1:levels(oz_ind),nb,ne) = zero
    endif
    ! set SST to zero for now
    if (sst_ind > 0) then
       vargrid(:,levels(n3d)+sst_ind,nb,ne) = zero
    endif

    ! Define all constants required by routine
    ice = .false.

    !----------------------------------------------------------------------

    ! Allocate memory for variables computed within routine
    if(.not. allocated(enkf_psfc))     allocate(enkf_psfc(npts))
    if(.not. allocated(enkf_temp))     allocate(enkf_temp(npts,nlevs))
    if(.not. allocated(enkf_pressure)) allocate(enkf_pressure(npts,nlevs))
    if(.not. allocated(enkf_spechumd)) allocate(enkf_spechumd(npts,nlevs))

    !----------------------------------------------------------------------

    ! Ingest the (sensible) temperature from the external file
    varstrname= 'T'
    call readwrfvar(filename, varstrname, enkf_temp, nlevs)
    if (tsen_ind > 0) then
       vargrid(:,levels(tsen_ind-1)+1:levels(tsen_ind),nb,ne) = enkf_temp
       do k = levels(tsen_ind-1)+1, levels(tsen_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: tsen ',                        &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif

    ! Ingest the specific humidity from the external file
    varstrname = 'Q'
    call readwrfvar(filename, varstrname, enkf_spechumd, nlevs)
    if (q_ind > 0) then
       vargrid(:,levels(q_ind-1)+1:levels(q_ind),nb,ne) = enkf_spechumd
       do k = levels(q_ind-1)+1, levels(q_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: q ',                           &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif

    ! calculate virtual temperature
    enkf_temp = enkf_temp * (one + fv*enkf_spechumd)
    if (tv_ind > 0) then
       vargrid(:,levels(tv_ind-1)+1:levels(tv_ind),nb,ne) = enkf_temp
       do k = levels(tv_ind-1)+1, levels(tv_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: tv ',                          &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif

    ! Ingest pressure info from the external file
    call readpressure_nmm(filename, enkf_pd, aeta1, aeta2, pt, pdtop)

    !----------------------------------------------------------------------

    ! calculate surface pressure
    enkf_psfc = r0_01 * (enkf_pd + pdtop + pt)

    if (ps_ind > 0) then
       vargrid(:,levels(n3d)+ps_ind,nb,ne) = enkf_psfc
       k = levels(n3d) + ps_ind
       if (nproc .eq. 0)                                               &
          write(6,*) 'READGRIDDATA_NMM: ps ',                           &
              & minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
    endif

    ! compute the pressure profile 
    do k = 1, nlevs
       enkf_pressure(:,k) = r0_01 * (aeta1(k)*pdtop + aeta2(k)*enkf_pd + pt)
    end do 

    if (prse_ind > 0) then
       vargrid(:,levels(prse_ind-1)+1:levels(prse_ind)-1, nb,ne) = enkf_pressure
       do k = levels(prse_ind-1)+1, levels(prse_ind)
          if (nproc .eq. 0)                                               &
             write(6,*) 'READGRIDDATA_NMM: prse ',                        &
                 & k, minval(vargrid(:,k,nb,ne)), maxval(vargrid(:,k,nb,ne))
       enddo
    endif


    !----------------------------------------------------------------------
    ! Compute the saturation specific humidity
    if (pseudo_rh) then
       call genqsat1(enkf_spechumd,qsat(:,:,nb,ne),enkf_pressure,enkf_temp,ice,  &
                    npts,nlevs)
    else
       qsat(:,:,nb,ne) = 1._r_double
    endif

    ! Deallocate memory for variables computed within routine
    if(allocated(enkf_temp))           deallocate(enkf_temp)
    if(allocated(enkf_psfc))           deallocate(enkf_psfc)
    if(allocated(enkf_pressure))       deallocate(enkf_pressure)
    if(allocated(enkf_spechumd))       deallocate(enkf_spechumd)

    !======================================================================
    end do backgroundloop ! loop over backgrounds to read in
    end do ensmemloop ! loop over ens members to read in

    ! Return calculated values

    return

  end subroutine readgriddata_nmm


  !========================================================================
  ! writegriddata.f90: write WRF-ARW or WRF-NMM analysis
  !-------------------------------------------------------------------------

  subroutine writegriddata(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,vargrid,no_inflate_flag)
    use constants
    use params, only: nbackgrounds, anlfileprefixes, fgfileprefixes
    include 'netcdf.inc'      

    !----------------------------------------------------------------------
    ! Define variables passed to subroutine
    integer, intent(in)  :: nanal1,nanal2, n2d, n3d, ndim
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, dimension(0:n3d), intent(in) :: levels
    real(r_single), dimension(npts,ndim,nbackgrounds,nanal2-nanal1+1), intent(in) :: vargrid
    logical, intent(in) :: no_inflate_flag 
      !Not used here, but added to make writegriddata(...) consistent with gridio_gfs.f90

    !----------------------------------------------------------------------
    ! Define variables computed within subroutine
    character(len=500)  :: filename
    character(len=3)    :: charnanal
    real                :: clip
    integer :: iyear,imonth,iday,ihour,dh1,ierr,iw3jdn

    !----------------------------------------------------------------------
    integer(i_kind) :: u_ind, v_ind, tv_ind, q_ind, ps_ind, ql_ind, qr_ind, qi_ind, qg_ind, &
               qs_ind, qnc_ind, qnr_ind, qni_ind, dbz_ind
    integer(i_kind) :: w_ind, cw_ind, ph_ind

    !----------------------------------------------------------------------
    ! Define variables required by for extracting netcdf variable
    ! fields
    character(len=19)  :: DateStr
    ! Define variables required for netcdf variable I/O
    character(len=12) :: varstrname
    real, dimension(:,:,:), allocatable :: vargrid_native

    !----------------------------------------------------------------------
    ! Define counting variables
    integer :: k, nb, nanal, ne

    real(r_single), dimension(:,:), allocatable :: enkf_t, enkf_q, enkf_field
    real(r_single), dimension(:),   allocatable :: enkf_psfc, pressure
    real(r_single), dimension(:),   allocatable :: enkf_mu, enkf_mub
    real(r_single), dimension(:),   allocatable :: znu, znw
    real(r_single), dimension(:),   allocatable :: qintegral

    real(r_single) :: ptop

    !----------------------------------------------------------------------

    u_ind   = getindex(vars3d, 'u')   !< indices in the state var arrays
    v_ind   = getindex(vars3d, 'v')   ! U and V (3D)
    tv_ind  = getindex(vars3d, 'tv')  ! Tv (3D)
    q_ind   = getindex(vars3d, 'q')   ! Q (3D)
    cw_ind  = getindex(vars3d, 'cw')  ! CWM for WRF-NMM
    w_ind   = getindex(vars3d, 'w')   ! W for WRF-ARW
    ph_ind  = getindex(vars3d, 'ph')  ! PH for WRF-ARW

    ql_ind  = getindex(vars3d, 'ql')  ! QL (3D) for WRF-ARW
    qr_ind  = getindex(vars3d, 'qr')  ! QR (3D) for WRF-ARW
    qi_ind  = getindex(vars3d, 'qi')  ! QI (3D) for WRF-ARW
    qg_ind  = getindex(vars3d, 'qg')  ! QG (3D) for WRF-ARW
    qs_ind  = getindex(vars3d, 'qs')  ! QS (3D) for WRF-ARW
    qnc_ind  = getindex(vars3d, 'qnc')  ! QNC (3D) for WRF-ARW
    qnr_ind  = getindex(vars3d, 'qnr')  ! QNR (3D) for WRF-ARW 
    qni_ind  = getindex(vars3d, 'qni')  ! QNI (3D) for WRF-ARW
    dbz_ind  = getindex(vars3d, 'dbz')  ! DBZ (3D) for WRF-ARW

    ps_ind  = getindex(vars2d, 'ps')  ! Ps (2D)

    ! Initialize constants required by routine
    call init_constants(.true.)

    !----------------------------------------------------------------------
    if (nbackgrounds > 1) then
       write(6,*)'gridio/writegriddata: writing multiple backgrounds not yet supported'
       call stop2(23)
    endif

    ne = 0
    ensmemloop: do nanal=nanal1,nanal2
    ne = ne + 1
    backgroundloop: do nb=1,nbackgrounds

    !----------------------------------------------------------------------
    ! First guess file should be copied to analysis file at scripting
    ! level; only variables updated by EnKF are changed
    write(charnanal,'(i3.3)') nanal
    filename = trim(adjustl(datapath))//trim(adjustl(anlfileprefixes(nb)))//"mem"//charnanal

    !----------------------------------------------------------------------
    ! Update u and v variables (same for NMM and ARW)
    allocate(enkf_field(npts, nlevs))
    if (u_ind > 0) then
       varstrname = 'U'
       if ( l_use_enkf_directZDA ) then ! add wind increment at native grid to reduce interpolation error
          call writewrfvar_native(filename, varstrname, vargrid(:,levels(u_ind-1)+1:levels(u_ind),nb,ne), nlevs)
       else
          call readwrfvar(filename, varstrname, enkf_field, nlevs)
          enkf_field = enkf_field + vargrid(:,levels(u_ind-1)+1:levels(u_ind),nb,ne)
          call writewrfvar(filename, varstrname, enkf_field, nlevs)   
       end if
    endif
    if (v_ind > 0) then
       varstrname = 'V'
       if ( l_use_enkf_directZDA ) then ! add wind increment at native grid to reduce interpolation error
          call writewrfvar_native(filename, varstrname, vargrid(:,levels(v_ind-1)+1:levels(v_ind),nb,ne), nlevs)
       else
          call readwrfvar(filename, varstrname, enkf_field, nlevs)
          enkf_field = enkf_field  + vargrid(:,levels(v_ind-1)+1:levels(v_ind),nb,ne)
          call writewrfvar(filename, varstrname, enkf_field, nlevs)   
       end if
    endif

    ! update CWM for WRF-NMM
    if (nmm .and. cw_ind > 0) then
       varstrname = 'CWM'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(cw_ind-1)+1:levels(cw_ind),nb,ne)
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    ! update reflectivity and hydrometeor mixing ratios for WRF-ARW
    if (arw .and. dbz_ind > 0) then
       varstrname = 'REFL_10CM'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(dbz_ind-1)+1:levels(dbz_ind),nb,ne)
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    if (arw .and. ql_ind > 0) then
       varstrname = 'QCLOUD'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(ql_ind-1)+1:levels(ql_ind),nb,ne)
       if ( l_use_enkf_directZDA .and. cliptracers ) then ! set cliptracers to remove negative hydrometers
           clip = tiny(enkf_field(1,1))
           where (enkf_field < clip) enkf_field = clip
       end if
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    if (arw .and. qr_ind > 0) then
       varstrname = 'QRAIN'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(qr_ind-1)+1:levels(qr_ind),nb,ne)
       if ( l_use_enkf_directZDA .and. cliptracers ) then ! set cliptracers to remove negative hydrometers
           clip = tiny(enkf_field(1,1))
           where (enkf_field < clip) enkf_field = clip
       end if
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    if (arw .and. qi_ind > 0) then
       varstrname = 'QICE'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(qi_ind-1)+1:levels(qi_ind),nb,ne)
       if ( l_use_enkf_directZDA .and. cliptracers ) then ! set cliptracers to remove negative hydrometers
           clip = tiny(enkf_field(1,1))
           where (enkf_field < clip) enkf_field = clip
       end if
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    if (arw .and. qs_ind > 0) then
       varstrname = 'QSNOW'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(qs_ind-1)+1:levels(qs_ind),nb,ne)
       if ( l_use_enkf_directZDA .and. cliptracers ) then ! set cliptracers to remove negative hydrometers
           clip = tiny(enkf_field(1,1))
           where (enkf_field < clip) enkf_field = clip
       end if
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    if (arw .and. qg_ind > 0) then
       varstrname = 'QGRAUP'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(qg_ind-1)+1:levels(qg_ind),nb,ne)
       if ( l_use_enkf_directZDA .and. cliptracers ) then ! set cliptracers to remove negative hydrometers
           clip = tiny(enkf_field(1,1))
           where (enkf_field < clip) enkf_field = clip
       end if
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    if (arw .and. qnc_ind > 0) then
       varstrname = 'QNCLOUD'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(qnc_ind-1)+1:levels(qnc_ind),nb,ne)
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    if (arw .and. qni_ind > 0) then
       varstrname = 'QNICE'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(qni_ind-1)+1:levels(qni_ind),nb,ne)
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    if (arw .and. qnr_ind > 0) then
       varstrname = 'QNRAIN'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(qnr_ind-1)+1:levels(qnr_ind),nb,ne)
       if ( l_use_enkf_directZDA .and. cliptracers ) then ! set cliptracers to remove negative hydrometers
           clip = tiny(enkf_field(1,1))
           where (enkf_field < clip) enkf_field = clip
       end if
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif

    ! update W and PH for WRF-ARW
    if (arw .and. w_ind > 0) then
       varstrname = 'W'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(w_ind-1)+1:levels(w_ind),nb,ne)
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif
    if (arw .and. ph_ind > 0) then
       varstrname = 'PH'
       call readwrfvar(filename, varstrname, enkf_field, nlevs)
       enkf_field = enkf_field + vargrid(:,levels(ph_ind-1)+1:levels(ph_ind),nb,ne)
       call writewrfvar(filename, varstrname, enkf_field, nlevs)
    endif
    deallocate(enkf_field)


    allocate(enkf_t(npts, nlevs), enkf_q(npts,nlevs), enkf_psfc(npts))
    if (nmm) then
       ! Update Tv and Q for NMM files (write out Tsen and Q)
       if (tv_ind > 0 .or. q_ind > 0) then
          ! read background specific humidity and sensible temperature
          varstrname = 'Q'
          call readwrfvar(filename, varstrname, enkf_q, nlevs)
          varstrname = 'T'
          call readwrfvar(filename, varstrname, enkf_t, nlevs)

          ! compute background virtual temperature
          enkf_t = enkf_t * (one + fv*enkf_q)
          
          ! add analysis increment to virtual temperature and specific humidity
          if (tv_ind > 0) then
             enkf_t = enkf_t + vargrid(:,levels(tv_ind-1)+1:levels(tv_ind),nb,ne)
          endif
          if (q_ind > 0) then
             enkf_q = enkf_q + vargrid(:,levels(q_ind-1)+1:levels(q_ind),nb,ne)
          endif

          ! clip Q if needed
          if (cliptracers) then
             clip = tiny(enkf_q(1,1))
             where (enkf_q < clip) enkf_q = clip
          end if 

          ! compute analysis sensible temperature
          enkf_t = enkf_t / (one + fv*enkf_q)

          ! write out analysis sensible temperature and specific humidity
          if (tv_ind > 0) then
             varstrname = 'T'
             call writewrfvar(filename, varstrname, enkf_t, nlevs)
          endif
          if (q_ind > 0) then
             varstrname = 'Q'
             call writewrfvar(filename, varstrname, enkf_q, nlevs)
          endif
       endif
       ! update surface pressure for NMM
       if (ps_ind > 0) then
          varstrname = 'PD'
          call readwrfvar(filename, varstrname, enkf_psfc, 1)

          ! add ps increment (mulitply by 100 since we're updating PD
          enkf_psfc = enkf_psfc + 100.*vargrid(:,levels(n3d)+ps_ind,nb,ne)
          call writewrfvar(filename, varstrname, enkf_psfc, 1)
       endif
    ! for ARW, update Tv and Q, but write out Tp and mix ratio
    elseif (arw) then
       if (tv_ind > 0 .or. q_ind > 0 .or. ps_ind > 0) then
          allocate(qintegral(npts), pressure(npts))

          ! read background potential temperature, mixing ratio
          ! and pressure information
          varstrname = 'QVAPOR'
          call readwrfvar(filename, varstrname, enkf_q, nlevs)
          varstrname = 'T'
          call readwrfvar(filename, varstrname, enkf_t, nlevs)

          call readpressure_arw(filename, znu, znw, enkf_mu, enkf_mub, ptop)

          ! compute background dry surface pressure
          enkf_psfc = r0_01*(enkf_mu + enkf_mub + ptop)
          ! compute background full surface pressure
          qintegral = one
          do k = 1, nlevs
             qintegral(:) = qintegral(:) + (znw(k) - znw(k+1))*enkf_q(:,k)
          enddo
          enkf_psfc = (enkf_psfc - ptop) * qintegral + ptop

          ! compute background specific humidity
          enkf_q = enkf_q / (one + enkf_q)

          ! compute background sensible temperature
          do k = 1, nlevs
             pressure = r0_01 * (znu(k)*(100*enkf_psfc-ptop)+ptop)
             enkf_t(:,k) = (enkf_t(:,k) + 300.0) *  &
                           (0.001 * pressure)**rd_over_cp_mass
          enddo

          ! compute background virtual temperature
          enkf_t = enkf_t * (one + fv*enkf_q)

          ! add analysis increment to virtual temperature, specific humidity
          ! and surface pressure
          if (tv_ind > 0) then
             enkf_t = enkf_t + vargrid(:,levels(tv_ind-1)+1:levels(tv_ind),nb,ne)
          endif
          if (q_ind > 0) then
             enkf_q = enkf_q + vargrid(:,levels(q_ind-1)+1:levels(q_ind),nb,ne)
          endif
          if (ps_ind > 0) then
             enkf_psfc = enkf_psfc + vargrid(:,levels(n3d)+ps_ind,nb,ne)
          endif

          ! clip Q if needed
          if (cliptracers) then
             clip = tiny(enkf_q(1,1))
             where (enkf_q < clip) enkf_q = clip
          end if

          ! compute analysis sensible temperature
          enkf_t = enkf_t / (one + fv*enkf_q)

          ! compute analysis mixing ratio
          enkf_q = enkf_q / (one - enkf_q)

          ! compute analysis potential temperature
          do k = 1, nlevs
             pressure = r0_01 * (znu(k)*(100*enkf_psfc-ptop)+ptop)
             enkf_t(:,k) = enkf_t(:,k) /  &
                           (0.001 * pressure)**rd_over_cp_mass - 300.0
          enddo

          ! compute analysis dry surface pressure
          qintegral = one
          do k = 1, nlevs
             qintegral(:) = qintegral(:) +                            &
                    (znw(k) - znw(k+1))*enkf_q(:,k)
          enddo
          enkf_psfc = (enkf_psfc - ptop) / qintegral + ptop

          ! compute analysis mu
          enkf_psfc = 100.*enkf_psfc - enkf_mub - ptop

          ! write out analysis virtual temperature, specific humidity
          ! and surface pressure
          if (tv_ind > 0) then
             varstrname = 'T'
             call writewrfvar(filename, varstrname, enkf_t, nlevs)
          endif
          if (q_ind > 0) then
             varstrname = 'QVAPOR'
             call writewrfvar(filename, varstrname, enkf_q, nlevs)
          endif
          if (ps_ind > 0) then
             varstrname = 'MU'
             call writewrfvar(filename, varstrname, enkf_psfc, 1)
          endif
       endif
    endif

    !----------------------------------------------------------------------
    ! update NSTART_HOUR in NMM (HWRF) restart file.
    read(datestring(1:4),'(i4)') iyear
    read(datestring(5:6),'(i2)') imonth
    read(datestring(7:8),'(i2)') iday
    read(datestring(9:10),'(i2)') ihour
    if (nmm .and. nmm_restart) then
       varstrname = 'NSTART_HOUR'
       if(.not. allocated(vargrid_native)) allocate(vargrid_native(1,1,1))
       vargrid_native(1,1,1) = ihour
       call writenetcdfdata(filename,vargrid_native,varstrname,1,1,1)
    end if
    !
    !  update START_DATE, SIMULATION_START_DATE, GMT, JULYR, JULDAY 
    !  global attributes.
    !
    write(DateStr,'(i4,"-",i2.2,"-",i2.2,"-",i2.2,"_",i2.2,":",i2.2)') iyear,imonth,iday,ihour,0,0
    ierr = NF_OPEN(trim(filename), NF_WRITE, dh1)
    IF (ierr .NE. NF_NOERR) print *, 'OPEN ',NF_STRERROR(ierr)
    ierr = NF_PUT_ATT_TEXT(dh1,NF_GLOBAL,'START_DATE',len(trim(DateStr)),DateStr)
    IF (ierr .NE. NF_NOERR) print *,'PUT START_DATE', NF_STRERROR(ierr)
    ierr = NF_PUT_ATT_TEXT(dh1,NF_GLOBAL,'SIMULATION_START_DATE',len(trim(DateStr)),DateStr)
    IF (ierr .NE. NF_NOERR) print *,'PUT SIMULATION_START_DATE', NF_STRERROR(ierr)
    ierr = NF_PUT_ATT_REAL(dh1,NF_GLOBAL,'GMT',NF_FLOAT,1,float(ihour))
    IF (ierr .NE. NF_NOERR) print *,'PUT GMT', NF_STRERROR(ierr)
    ierr = NF_PUT_ATT_INT(dh1,NF_GLOBAL,'JULYR',NF_INT,1,iyear)
    IF (ierr .NE. NF_NOERR) print *,'PUT JULYR', NF_STRERROR(ierr)
    ierr=NF_PUT_ATT_INT(dh1,NF_GLOBAL,'JULDAY',NF_INT,1,iw3jdn(iyear,imonth,iday)-iw3jdn(iyear,1,1)+1)
    IF (ierr .NE. NF_NOERR) print *,'PUT JULDAY', NF_STRERROR(ierr)
    ierr = NF_CLOSE(dh1)
    IF (ierr .NE. NF_NOERR) print *, 'CLOSE ',NF_STRERROR(ierr)

    !======================================================================
    end do backgroundloop ! loop over backgrounds to read in
    end do ensmemloop ! loop over ens members to read in

    ! Return calculated values
    return

    !======================================================================

  end subroutine writegriddata


  !======================================================================
  ! readwrfvar.f90: This subroutine reads a varname variable from WRF
  ! ARW or NMM netcdf file and returns the variable interpolated to 
  ! unstaggered grid, in EnKF style (1D array for 2D field); all
  ! checks for grid staggering are contained within this subroutine
  subroutine readwrfvar(filename, varname, grid, nlevs)
    implicit none
    character(len=500), intent(in) :: filename
    character(len=12),  intent(in) :: varname
    integer(i_kind), intent(in) :: nlevs
    real(r_single),  dimension(npts,nlevs),  intent(out) :: grid

    ! Define variables computed within subroutine
    real, dimension(:,:,:), allocatable :: workgrid
    real, dimension(:,:,:), allocatable :: vargrid_native
    integer :: xdim, ydim, zdim
    integer :: xdim_native, ydim_native, zdim_native
    integer :: xdim_local,  ydim_local,  zdim_local

    ! Define variables requiredfor netcdf variable I/O
    character(len=50) :: attstr
    character(len=12) :: varstagger
    character(len=12) :: varmemoryorder

    ! Define counting variables
    integer :: i, j, k
    integer :: counth

    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim

    ! Define staggering attributes for variable grid
    attstr = 'stagger'
    call variableattribute_char(filename,varname,attstr,        &
            & varstagger)

    xdim_native = xdim
    ydim_native = ydim
    zdim_native = zdim
    ! If variable grid is staggered assign array dimensions appropriately
    if(varstagger(1:1) .eq. 'X') then
       xdim_native = xdim + 1
    else if(varstagger(1:1) .eq. 'Y') then
       ydim_native = ydim + 1
    else if(varstagger(1:1) .eq. 'Z') then
       zdim_native = zdim + 1
    end if ! if(varstagger(1:1) .eq. 'X')

    ! Define memory attributes for variable grid
    attstr = 'MemoryOrder'
    call variableattribute_char(filename,varname,attstr,       &
            & varmemoryorder)

    ! If variable is a 2-dimensional field, rescale variables appropriately
    if(varmemoryorder(1:3) .eq. 'XY ') then
       zdim_local = 1
       zdim_native = 1
    else
       zdim_local = zdim
    end if ! if(varmemoryorder(1:3) .eq. 'XY ')

    ! Define local variable dimensions
    xdim_local = xdim
    ydim_local = ydim
    ! Allocate memory for local variable arrays
    if(.not. allocated(workgrid))                                     &
         & allocate(workgrid(xdim_local,ydim_local,zdim_local))
    if(.not. allocated(vargrid_native))                               &
         & allocate(vargrid_native(xdim_native,ydim_native,zdim_native))

    ! Ingest variable from external netcdf formatted file
    call readnetcdfdata(filename,vargrid_native,varname,     &
            & xdim_native,ydim_native,zdim_native)

    ! Interpolate variable from staggered (i.e., E-) grid to
    ! unstaggered (i.e., A-) grid. If variable is staggered in
    ! vertical, intepolate from model layer interfaces
    ! (including surface and top) to model layer midpoints.
    call cross2dot(vargrid_native,xdim_native,ydim_native,            &
         & zdim_native,xdim_local,ydim_local,zdim_local,workgrid)

    !----------------------------------------------------------------------
    ! Loop through vertical coordinate
    do k = 1, zdim_local
       ! Initialize counting variable
       counth = 1
       ! Loop through meridional horizontal coordinate
       do j = 1, ydim_local
          ! Loop through zonal horizontal coordinate
          do i = 1, xdim_local
             ! Assign values to output variable array
             grid(counth,k) = workgrid(i,j,k)

             counth = counth + 1
          end do ! do i = 1, xdim_local
       end do ! do j = 1, ydim_local
    end do ! do k = 1, zdim_local

    !----------------------------------------------------------------------
    ! Deallocate memory for local variables
    if(allocated(vargrid_native)) deallocate(vargrid_native)
    if(allocated(workgrid))       deallocate(workgrid)

  end subroutine readwrfvar


  !======================================================================
  ! writewrfvar: write EnKF-style field in WRF netcdf file; variable is
  ! interpolated to the native variable grid; all checks for
  ! grid staggering are contained within this subroutine
  subroutine writewrfvar(filename, varname, grid, nlevs)
    implicit none
    character(len=500), intent(in) :: filename
    character(len=12),  intent(in) :: varname
    integer(i_kind), intent(in) :: nlevs
    real(r_single),  dimension(npts,nlevs),  intent(in) :: grid

    ! Define variables computed within subroutine
    real, dimension(:,:,:), allocatable :: workgrid
    real, dimension(:,:,:), allocatable :: vargrid_native
    integer :: xdim, ydim, zdim
    integer :: xdim_native, ydim_native, zdim_native
    integer :: xdim_local,  ydim_local,  zdim_local

    ! Define variables requiredfor netcdf variable I/O
    character(len=50) :: attstr
    character(len=12) :: varstagger

    ! Define counting variables
    integer :: i, j, k
    integer :: counth


    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim

    ! Allocate memory for local variable
    allocate(workgrid(xdim,ydim,zdim))

    xdim_native = xdim
    ydim_native = ydim
    zdim_native = zdim

    if (arw) then
       attstr = 'stagger'
       call variableattribute_char(filename,varname,attstr,     &
                  & varstagger)
       !----------------------------------------------------------------------
       ! If variable grid is staggered, assign array dimensions appropriately
       if(varstagger(1:1) .eq. 'X') then
          xdim_native = xdim + 1
       else if(varstagger(1:1) .eq. 'Y') then
          ydim_native = ydim + 1
       else if(varstagger(1:1) .eq. 'Z') then
          zdim_native = zdim + 1
       end if ! if(varstagger(1:1) .eq. 'X')
    endif

    zdim_local = nlevs
    if(nlevs == 1) then
       zdim_native = 1
    end if 

    ! Define local variable dimensions
    xdim_local = xdim
    ydim_local = ydim

    !----------------------------------------------------------------------
    ! Allocate memory local arrays (first check whether they are
    ! already allocated)
    if (allocated(vargrid_native)) deallocate(vargrid_native)
    allocate(vargrid_native(xdim_native,ydim_native,zdim_native))

    !----------------------------------------------------------------------
    ! Loop through vertical coordinate
    do k = 1, zdim_local
       ! Initialize counting variable
       counth = 1

       ! Loop through meridional horizontal coordinate
       do j = 1, ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, xdim
             ! Assign values to local array
             workgrid(i,j,k) = grid(counth,k)

             counth = counth + 1
          end do ! do i = 1, xdim
       end do ! do j = 1, ydim
    end do ! k = 1, zdim_local

    ! Interpolate increments to native grid (i.e., from A-grid to
    ! C-grid; if necessary); on input, workgrid is increments on
    ! unstaggered grid; on output vargrid_native is increments on
    ! model-native (i.e., staggered grid); vargridin_native is
    ! unmodified first guess on native staggered grid
    call dot2cross(xdim_local,ydim_local,zdim_local,xdim_native,    &
            ydim_native,zdim_native,workgrid,vargrid_native)

    !----------------------------------------------------------------------
    ! Write analysis variable.
    call writenetcdfdata(filename,vargrid_native,varname,          &
             xdim_native,ydim_native,zdim_native)

    ! Deallocate memory for local variables
    if(allocated(vargrid_native)) deallocate(vargrid_native)
    if(allocated(workgrid))       deallocate(workgrid)

  end subroutine writewrfvar

  !======================================================================
  ! writewrfvar_native: write EnKF-style field in WRF netcdf file; 
  ! analysis increment is interpolated to the native variable grid
  ! all checks for grid staggering are contained within this subroutine
  ! CAPS(J. Park): modified from 'writewrfvar_native'
  !                increments are added at native grid to minimize interpolation
  !                errors from dot2cross
  subroutine writewrfvar_native(filename, varname, grid, nlevs)
    implicit none
    character(len=500), intent(in) :: filename
    character(len=12),  intent(in) :: varname
    integer(i_kind), intent(in) :: nlevs
    real(r_single),  dimension(npts,nlevs),  intent(in) :: grid    ! analysis increment

    ! Define variables computed within subroutine
    real, dimension(:,:,:), allocatable :: workgrid
    real, dimension(:,:,:), allocatable :: vargrid_native   ! original variables at native grid
    real, dimension(:,:,:), allocatable :: vargridinc_native ! analysis increment at native grid
    integer :: xdim, ydim, zdim
    integer :: xdim_native, ydim_native, zdim_native
    integer :: xdim_local,  ydim_local,  zdim_local

    ! Define variables requiredfor netcdf variable I/O
    character(len=50) :: attstr
    character(len=12) :: varstagger

    ! Define counting variables
    integer :: i, j, k
    integer :: counth


    xdim = dimensions%xdim
    ydim = dimensions%ydim
    zdim = dimensions%zdim

    ! Allocate memory for local variable
    allocate(workgrid(xdim,ydim,zdim))

    xdim_native = xdim
    ydim_native = ydim
    zdim_native = zdim

    if (arw) then
       attstr = 'stagger'
       call variableattribute_char(filename,varname,attstr,     &
                  & varstagger)
       !----------------------------------------------------------------------
       ! If variable grid is staggered, assign array dimensions appropriately
       if(varstagger(1:1) .eq. 'X') then
          xdim_native = xdim + 1
       else if(varstagger(1:1) .eq. 'Y') then
          ydim_native = ydim + 1
       else if(varstagger(1:1) .eq. 'Z') then
          zdim_native = zdim + 1
       end if ! if(varstagger(1:1) .eq. 'X')
    endif

    zdim_local = nlevs
    if(nlevs == 1) then
       zdim_native = 1
    end if

    ! Define local variable dimensions
    xdim_local = xdim
    ydim_local = ydim

    ! read variables at native grid
    if (allocated(vargrid_native)) deallocate(vargrid_native)
    allocate(vargrid_native(xdim_native,ydim_native,zdim_native))

    ! Ingest variable from external netcdf formatted file
    call readnetcdfdata(filename,vargrid_native,varname,     &
            & xdim_native,ydim_native,zdim_native)

    !----------------------------------------------------------------------
    ! Allocate memory local arrays (first check whether they are
    ! already allocated)
    if (allocated(vargridinc_native)) deallocate(vargridinc_native)
    allocate(vargridinc_native(xdim_native,ydim_native,zdim_native))

    !----------------------------------------------------------------------
    ! Loop through vertical coordinate
    do k = 1, zdim_local
       ! Initialize counting variable
       counth = 1

       ! Loop through meridional horizontal coordinate
       do j = 1, ydim
          ! Loop through zonal horizontal coordinate
          do i = 1, xdim
             ! Assign values to local array
             workgrid(i,j,k) = grid(counth,k)

             counth = counth + 1
          end do ! do i = 1, xdim
       end do ! do j = 1, ydim
    end do ! k = 1, zdim_local


    ! Interpolate increments to native grid (i.e., from A-grid to
    ! C-grid; if necessary); on input, workgrid is increments on
    ! unstaggered grid; on output vargrid_native is increments on
    ! model-native (i.e., staggered grid); vargrid_native is
    ! unmodified first guess on native staggered grid
    call dot2cross(xdim_local,ydim_local,zdim_local,xdim_native,    &
            ydim_native,zdim_native,workgrid,vargridinc_native)

    vargrid_native=vargrid_native+vargridinc_native  ! add Increment

    !----------------------------------------------------------------------
    ! Write analysis variable.
    call writenetcdfdata(filename,vargrid_native,varname,          &
             xdim_native,ydim_native,zdim_native)

    ! Deallocate memory for local variables
    if(allocated(vargrid_native)) deallocate(vargrid_native)
    if(allocated(vargridinc_native)) deallocate(vargridinc_native)
    if(allocated(workgrid))       deallocate(workgrid)

  end subroutine writewrfvar_native

  !========================================================================
  ! read pressure information (pd, aeta1, aeta2, pl, pdtop from WRF-NMM file
  ! subroutine allocates space for pd, aeta1 and aeta2
  subroutine readpressure_nmm(filename, pd, aeta1, aeta2, pt, pdtop)
  implicit none
     character(len=500), intent(in) :: filename
     real(r_single), dimension(:), allocatable :: aeta1, aeta2
     real(r_single), dimension(:), allocatable :: pd
     real(r_single) :: pt, pdtop

     real, dimension(:,:,:), allocatable  :: wrfnmm
     character(len=12) :: varstrname

     integer :: zdim

     zdim = dimensions%zdim

     allocate(aeta1(zdim), aeta2(zdim), pd(npts))

     ! Ingest surface pressure from the external file
     varstrname = 'PD'
     call readwrfvar(filename,varstrname,pd,1)

     ! Ingest hybrid vertical coordinate from the external file
     varstrname = 'AETA1'
     allocate(wrfnmm(1,1,zdim))
     call readnetcdfdata(filename,wrfnmm,varstrname,1,1,zdim)
     aeta1 = wrfnmm(1,1,:)

     varstrname = 'AETA2'
     call readnetcdfdata(filename,wrfnmm,varstrname,1,1,zdim)
     aeta2 = wrfnmm(1,1,:)
     deallocate(wrfnmm)

     allocate(wrfnmm(1,1,1))
     ! Ingest pressure at top of domain from the external file
     varstrname = 'PT'
     call readnetcdfdata(filename,wrfnmm,varstrname,1,1,1)
     pt = wrfnmm(1,1,1)

     ! Ingest mass within pressure domain from the external file
     varstrname = 'PDTOP'
     call readnetcdfdata(filename,wrfnmm,varstrname,1,1,1)
     pdtop = wrfnmm(1,1,1)
     deallocate(wrfnmm)

  end subroutine readpressure_nmm

  !========================================================================
  ! read pressure information (aeta1, eta1, mu, mub, ptop from WRF-ARW file
  ! subroutine allocates space for znu, znw, mu, mub
  subroutine readpressure_arw(filename, znu, znw, mu, mub, ptop)
  implicit none
     character(len=500), intent(in) :: filename
     real(r_single), dimension(:), allocatable :: znu, znw  ! aeta1 and eta1
     real(r_single), dimension(:), allocatable :: mu, mub
     real(r_single) :: ptop

     real, dimension(:,:,:), allocatable  :: wrfarw
     character(len=12) :: varstrname

     integer :: zdim

     zdim = dimensions%zdim

     allocate(znu(zdim), znw(zdim + 1), mu(npts), mub(npts))

     ! Ingest the model vertical (eta) levels from the external file
     varstrname = 'ZNU'
     allocate(wrfarw(1,1,zdim))
     call readnetcdfdata(filename,wrfarw,varstrname,1,1,zdim)
     znu = wrfarw(1,1,:)
     deallocate(wrfarw)

     ! Ingest the model vertical (aeta) levels from the external file
     varstrname = 'ZNW'
     allocate(wrfarw(1,1,zdim+1))
     call readnetcdfdata(filename,wrfarw,varstrname,1,1,zdim+1)
     znw = wrfarw(1,1,:)
     deallocate(wrfarw)

     ! Ingest the model top pressure level from the external file
     varstrname = 'P_TOP'
     allocate(wrfarw(1,1,1))
     call readnetcdfdata(filename,wrfarw,varstrname,1,1,1)
     ptop = wrfarw(1,1,1)
     deallocate(wrfarw)

     ! Ingest the model perturbation dry air mass from the external
     ! file
     varstrname = 'MU'
     call readwrfvar(filename,varstrname,mu,1)

     ! Ingest the model base state dry air mass from the external file
     varstrname = 'MUB'
     call readwrfvar(filename,varstrname,mub,1)

  end subroutine readpressure_arw

  subroutine writeincrement(nanal1,nanal2,vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
    use constants, only: max_varname_length
    use params, only: nbackgrounds
    implicit none
    integer, intent(in) :: nanal1,nanal2
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, intent(in) :: n2d,n3d,ndim
    integer, dimension(0:n3d), intent(in) :: levels
    real(r_single), dimension(npts,ndim,nbackgrounds,1), intent(inout) :: grdin
    logical, intent(in) :: no_inflate_flag
  end subroutine writeincrement

  subroutine writeincrement_pnc(vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
    use constants, only: max_varname_length
    use params, only: nbackgrounds
    implicit none
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, intent(in) :: n2d,n3d,ndim
    integer, dimension(0:n3d), intent(in) :: levels
    real(r_single), dimension(npts,ndim,nbackgrounds,1), intent(inout) :: grdin
    logical, intent(in) :: no_inflate_flag
  end subroutine writeincrement_pnc
  
  subroutine readgriddata_pnc(vars3d,vars2d,n3d,n2d,levels,ndim,ntimes, &
                               fileprefixes,filesfcprefixes,reducedgrid,grdin,qsat)
    use constants, only: max_varname_length
    implicit none
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, intent(in) :: n2d, n3d
    integer, dimension(0:n3d), intent(in) :: levels
    integer, intent(in) :: ndim, ntimes
    character(len=120), dimension(7), intent(in)  :: fileprefixes
    character(len=120), dimension(7), intent(in)  :: filesfcprefixes
    logical, intent(in) :: reducedgrid
    real(r_single), dimension(npts,ndim,ntimes,1), intent(out) :: grdin
    real(r_double), dimension(npts,nlevs,ntimes,1), intent(out) :: qsat
  end subroutine readgriddata_pnc

  subroutine writegriddata_pnc(vars3d,vars2d,n3d,n2d,levels,ndim,grdin,no_inflate_flag)
    use constants, only: max_varname_length
    use params, only: nbackgrounds
    implicit none
    character(len=max_varname_length), dimension(n2d), intent(in) :: vars2d
    character(len=max_varname_length), dimension(n3d), intent(in) :: vars3d
    integer, intent(in) :: n2d,n3d,ndim
    integer, dimension(0:n3d), intent(in) :: levels
    real(r_single), dimension(npts,ndim,nbackgrounds,1), intent(inout) :: grdin
    logical, intent(in) :: no_inflate_flag
  end subroutine writegriddata_pnc

end module gridio
