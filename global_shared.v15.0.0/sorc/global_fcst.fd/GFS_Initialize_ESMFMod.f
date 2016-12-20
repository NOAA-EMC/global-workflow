
! !MODULE: GFS_Initialize_ESMFMod --- Initialize module of the ESMF 
!                                     gridded component of the GFS system.
!
! !DESCRIPTION: GFS gridded component initialize module.
!
! !REVISION HISTORY:
!
!  November 2004  Weiyu Yang     Initial code.
!  January 2006  S. Moorthi      Update to the new GFS version
!  August 2006   H. Juang        Add option to run generalized coordinates
!  December 2006 S. Moorthi      GFSIO included
!  February 2008  Weiyu Yang     Modified for the ESMF 3.1.0 version, fixed bug for runDuration.
!  Oct 18   2010  S. Moorthi     Added fscav initialization
!  March 8  2011  H. Juang       Add option to run NDSL, which is the
!                                non-iterating dimensionally-splitted semi-
!                                Lagrangian advection for entire dynamics
!  Dec 11   2012  H. Juang       generalized reduced grid based on Juang 2004
!                                with x number to avoid underflow in reduced
!                                grid for lons per lat
!  Jan.  2013   Y. Hou           Added 4 sw sfc downward flux components and snowfall rate
!                                to the output and added radiation initialization call
!                                "rad_initialize' to set up all radiation related fixed
!                                parameters.
!  May 22, 2013 S. Moorthi       Added "if (.not. adiab) then " loop around radiation initialization
!                                and moved it before zeroing of fluxes and diagnostics
!  Sep 26, 2013 F. Yang          Added restart fields dyn_f3d for dynamics.
!
!
! !INTERFACE:
!
 MODULE GFS_Initialize_ESMFMod

!
!!USES:
!
 USE GFS_GetCf_ESMFMod         ! this module uses GFS_InternalState_ESMFMod that
                               ! includes module machine and most of 'def' modules
 use gfsio_module , only : gfsio_init
 use module_ras , only : nrcmax, fix_ncld_hr

 IMPLICIT none

 CONTAINS

 SUBROUTINE GFS_Initialize(gcGFS, gis, clock, rc)

! This subroutine set up the internal state variables,
! allocate internal state arrays for initializing the GFS system.
!----------------------------------------------------------------

 integer, parameter :: iunit=101

 TYPE(ESMF_VM)                                   :: vm_local   ! ESMF virtual machine
 TYPE(ESMF_GridComp),              INTENT(inout) :: gcGFS
 TYPE(GFS_InternalState), POINTER, INTENT(inout) :: gis
 TYPE(ESMF_Clock),                 INTENT(inout) :: clock
 INTEGER,                          INTENT(out)   :: rc
 INTEGER,             DIMENSION(mpi_status_size) :: status

 TYPE(ESMF_TimeInterval) :: timeStep
 TYPE(ESMF_TimeInterval) :: runDuration
 TYPE(ESMF_Time)         :: startTime
 TYPE(ESMF_Time)         :: stopTime
 TYPE(ESMF_Time)         :: currTime
 INTEGER                 :: timeStep_sec
 INTEGER                 :: runDuration_hour
 INTEGER                 :: ifhmax
 INTEGER                 :: rc1 = ESMF_SUCCESS
 INTEGER                 :: ierr
!INTEGER                 :: yyc, mmc, ddc, hhc, minsc
!
!DHOU 01/11/2008, added these two variables
!INTEGER                 :: lvlw, npe_single_member
 INTEGER                 :: npe_single_member

 logical                 :: file_exists=.false.
 INTEGER :: l, ilat, locl, ikey, nrank_all, nfluxes, latghf, iret
 INTEGER :: i,j,k
 integer num_parthds
 real (kind=kind_io4) blatc4
 real (kind=kind_io4), allocatable :: pl_lat4(:), pl_pres4(:), pl_time4(:)

 real (kind=kind_phys), allocatable :: si_loc(:)
 real (kind=kind_phys), parameter :: typical_pgr=95.0
!integer, parameter :: iflip = 0      ! virtical profile index from toa to sfc
 integer, parameter :: iflip = 1      ! virtical profile index from sfc to toa
 logical :: sas_shal = .true.

! Set up parameters of MPI communications.
! Use ESMF utility to get PE identification and total number of PEs.
!-------------------------------------------------------------------
 rc     = 0                                                             !hmhj
 me     = gis%me
 NODES  = gis%nodes
 nlunit = gis%nam_gfs%nlunit

 npe_single_member = gis%npe_single_member
 if (me == 0) print *,' npe_single_member=',npe_single_member
 CALL COMPNS(gis%DELTIM,gis%IRET,                                            &
!            gis%ntrac,   gis%nxpt, gis%nypt, gis%jintmx, gis%jcap,          &
             gis%ntrac,                                   gis%jcap,          &
             gis%levs,    gis%levr, gis%lonf, gis%lonr,   gis%latg, gis%latr,&
             gis%ntoz,    gis%ntcw, gis%ncld, gis%lsoil,  gis%nmtvr,         &
             gis%num_p3d, gis%num_p2d, gis%num_a3d, gis%num_a2d,             &
             me,    gis%nam_gfs%nlunit, gis%nam_gfs%gfs_namelist)
!
 CALL set_soilveg(me,gis%nam_gfs%nlunit)
 call set_tracer_const(gis%ntrac,me,gis%nam_gfs%nlunit)			! hmhj
!

      ntrac   = gis%ntrac
!     nxpt    = gis%nxpt
!     nypt    = gis%nypt
!     jintmx  = gis%jintmx
      jcap    = gis%jcap
      levs    = gis%levs
      levr    = gis%levr
      lonf    = gis%lonf
      lonr    = gis%lonr
      latg    = gis%latg
      latr    = gis%latr
      ntoz    = gis%ntoz
      ntcw    = gis%ntcw
      ncld    = gis%ncld
      lsoil   = gis%lsoil
      nmtvr   = gis%nmtvr
      num_p3d = gis%num_p3d
      num_p2d = gis%num_p2d
      num_a3d = gis%num_a3d
      num_a2d = gis%num_a2d
      if (gis%nam_gfs%Total_Member <= 1) then
        ens_nam=''
        ens_mem = 0
      else
        ens_mem = gis%nam_gfs%Member_Id
        write(ens_nam,'("_",I2.2)') gis%nam_gfs%Member_Id
      endif
!
!     ivssfc  = 200501
      ivssfc  = 200509
      ivssfc_restart  = 200509
      if (ivssfc > ivssfc_restart) ivssfc_restart = ivssfc
      ivsnst  = 200907
      ivsupa  = 0
      if (levs > 99) ivsupa  = 200509
!
      levh   = ntrac*levs
      gis%levh = levh             ! Added by Weiyu
!     latgd  = latg+ 2*jintmx 
      latgd  = latg
      jcap1  = jcap+1 
      jcap2  = jcap+2 
      latg2  = latg/2 
      latr2  = latr/2 
      levm1  = levs-1 
      levp1  = levs+1 
!jfe  parameter ( lonfx  = lonf+2 )
!     lonfx  = lonf + 1 + 2*nxpt+1 
      lonrx  = lonr+2 
      lnt    = jcap2*jcap1/2 
      lnuv   = jcap2*jcap1 
      lnt2   = 2*lnt 
      lnt22  = 2*lnt+1 
      lnte   = (jcap2/2)*((jcap2/2)+1)-1 
      lnto   = (jcap2/2)*((jcap2/2)+1)-(jcap2/2) 
      lnted  = lnte 
      lntod  = lnto 

!     ngrids_sfcc = 32+LSOIL*3
!     ngrids_sfcc = 29+LSOIL*3   ! No CV, CVB, CVT!
      ngrids_sfcc = 32+LSOIL*3   ! No CV, CVB, CVT! includes T2M, Q2M, TISFC


      if (climate) then
        ngrids_flx  = 66+36+5  ! 4 sw fluxes + frozen precip fraction 
      else
        ngrids_flx  = 66+43+5  ! 4 sw fluxes + frozen precip fraction 
      endif

      if (nst_fcst > 0) then         ! For NST model
!       ngrids_nst = 19              ! oceanic fields (for diurnal warming and sub-layer)
         nr_nst = 10                 ! oceanic fields: for diurnal warming model run
        nf_nst = 9                   ! oceanic fields: for GSI analysis
        ngrids_nst = nr_nst + nf_nst ! oceanic fields (for diurnal warming and sub-layer)
      else
        ngrids_nst = 0
      endif

!*RADFLX*
      nfxr        = 33               ! without total aod field in output
!!    nfxr        = 33+1             ! include total aod field in output
      ngrids_gg   = 2+LEVS*(4+ntrac)

!
      if (ntrac-ncld-1 > 0) then
        allocate ( gis%fscav(ntrac-ncld-1), stat = ierr )
        gis%fscav = 0.0
      endif

      gis%lnt2    = lnt2

      allocate(lat1s_a(0:jcap))
      allocate(lat1s_r(0:jcap))
!     allocate(lon_dims_a(latgd))
!     allocate(lon_dims_ext(latgd))
!my   allocate(lon_dims_r(latgd))
      allocate(lon_dims_r(latr))

      allocate(colrad_a(latg2))
      allocate(wgt_a(latg2))
      allocate(wgtcs_a(latg2))
      allocate(rcs2_a(latg2))
      allocate(sinlat_a(latg2))

      allocate(colrad_r(latr))
      allocate(wgt_r(latr2))
      allocate(wgtcs_r(latr2))
      allocate(rcs2_r(latr2))
      allocate(sinlat_r(latr))
      allocate(coslat_r(latr))

      allocate(am(levs,levs))
      allocate(bm(levs,levs))
      allocate(cm(levs,levs))
      allocate(dm(levs,levs,jcap1))
      allocate(tor(levs))
      allocate(si(levp1))
      allocate(sik(levp1))
      allocate(sl(levs))
      allocate(slk(levs))
      allocate(del(levs))
      allocate(rdel2(levs))
      allocate(ci(levp1))
      allocate(cl(levs))
      allocate(tov(levs))
      allocate(sv(levs))

      allocate(AK5(LEVP1))
      allocate(BK5(LEVP1))
      allocate(CK5(LEVP1))                                            ! hmhj
      allocate(THREF(LEVP1))                                          ! hmhj
      allocate(CK(LEVS))
      allocate(DBK(LEVS))
      allocate(bkl(LEVS))
      allocate(AMHYB(LEVS,LEVS))
      allocate(BMHYB(LEVS,LEVS))
      allocate(SVHYB(LEVS))
      allocate(tor_hyb(LEVS))
      allocate(D_HYB_m(levs,levs,jcap1))
      allocate(dm205_hyb(jcap1,levs,levs))

!sela added for semilag grid computations
      allocate(AM_slg(LEVS,LEVS))
      allocate(BM_slg(LEVS,LEVS))
      allocate(SV_slg(LEVS))
      allocate(tor_slg(LEVS))
      allocate(sv_ecm(LEVS))
      allocate(D_slg_m(levs,levs,jcap1))

      allocate(si_loc(levr+1))

!sela added for semilag grid computations
      allocate(yecm(LEVS,LEVS))
      allocate(tecm(LEVS,LEVS))
      allocate(y_ecm(LEVS,LEVS))
      allocate(t_ecm(LEVS,LEVS))
!sela added for semilag grid computations

      allocate(spdmax(levs))

!     allocate(buf_sig(lnt2,3*levs+2),buff_grid(lonr,latr),
!     allocate(buf_sig(lnt2,3*levs+2),
!    &         buff_mult(lonr,latr,ngrids_sfc))
!     allocate(buf_sig_n(lnt2,levs,ntrac))
      allocate(buff_mult(lonr,latr,ngrids_sfcc+ngrids_nst))
      if (gfsio_out) then
        allocate(buff_multg(lonr*latr,ngrids_gg))
      endif

!     allocate(LBASDZ(4,2,levs),LBASIZ(4,2,LEVS),DETAI(levp1), &
!      DETAM(levs),ETAMID(levs),ETAINT(levp1),                 &
!      SINLAMG(lonf,latg2),COSLAMG(lonf,latg2))
!

      allocate(z(lnt2))
      allocate(z_r(lnt2))
!
      nfluxes = 153
      allocate(fmm(lonr*latr,nfluxes),lbmm(lonr*latr,nfluxes))
      allocate(ibufm(50,nfluxes),rbufm(50,nfluxes))

!
!   Allocate and reading lonsperlat and lonsperlar (if the file is provided)
!
      allocate(gis%LONSPERLAT(latg))
      allocate(gis%lonsperlar(latr))

      inquire (file="lonsperlat.dat", exist=file_exists)
      if ( .not. file_exists ) then
        if ( me == 0 ) then
          print *,'   Requested lonsperlat.dat  data file does not exist'
          print *,'   *** Stopped in subroutine GFS_Init !!'
        endif
        call mpi_quit(1111)
      else
        open (iunit,file='lonsperlat.dat',status='old',form='formatted',      &
                                                       iostat=iret)
        if (iret /= 0) then
          write(0,*)' iret while reading lonsperlat.dat ',iret
          call mpi_quit(1112)
        endif
        rewind iunit
        read (iunit,*,iostat=iret) latghf,(gis%lonsperlat(i),i=1,latghf)
        if (latghf+latghf /= latg) then
           write(0,*)' ltghf=',latghf,' not equal to latg/2=',latg/2
           call mpi_quit(1113)
        endif
        do i=1,latghf
          gis%lonsperlat(latg-i+1) = gis%lonsperlat(i)
        enddo
        close(iunit)
      endif
!
      inquire (file="lonsperlar.dat", exist=file_exists)
      if (file_exists ) then
        open (iunit,file='lonsperlar.dat',status='old',form='formatted',      &
                                                       iostat=iret)
        if (iret == 0) then
          rewind iunit
          read (iunit,*,iostat=iret) latghf,(gis%lonsperlar(i),i=1,latghf)
          do i=1,latghf
            gis%lonsperlar(latg-i+1) = gis%lonsperlar(i)
          enddo
        endif
        close(iunit)
      else             ! lonsperlar not given - assume equal to lonsperlat
        gis%lonsperlar = gis%lonsperlat
      endif
      if (me == 0) then
        write(0,*)' lonsperlat=',gis%lonsperlat
        write(0,*)' lonsperlar=',gis%lonsperlar
      endif

!
!***********************************************************************
!
      if (ras) then
        if (fix_ncld_hr) then
!         nrcm = min(nrcmax, levs-1) * (gis%deltim/1200) + 0.50001
          nrcm = min(nrcmax, levs-1) * (gis%deltim/1200) + 0.10001
!         nrcm = min(nrcmax, levs-1) * min(1.0,gis%deltim/360) + 0.1
        else
          nrcm = min(nrcmax, levs-1)
        endif
!       nrcm = max(nrcmax, nint((nrcmax*gis%deltim)/600.0))
      else
        nrcm = 1
      endif
!
!     if (.not. adiab) then
      if (ntoz <= 0) then      ! Diagnostic ozone
        rewind (kozc)
        read (kozc,end=101) latsozc, levozc, timeozc, blatc4
  101   if (levozc < 10 .or. levozc > 100) then
          rewind (kozc)
          levozc  = 17
          latsozc = 18
          blatc   = -85.0
        else
          blatc   = blatc4
        endif
        latsozp   = 2
        levozp    = 1
        timeoz    = 1
        pl_coeff  = 0
      else                       ! Prognostic Ozone
        rewind (kozpl)
        read (kozpl) pl_coeff, latsozp, levozp, timeoz
        allocate (pl_lat(latsozp), pl_pres(levozp),pl_time(timeoz+1))
        allocate (pl_lat4(latsozp), pl_pres4(levozp),pl_time4(timeoz+1))
        rewind (kozpl)
        read (kozpl) pl_coeff, latsozp, levozp, timeoz, pl_lat4, pl_pres4,  &
                     pl_time4
        pl_pres(:) = pl_pres4(:)
        pl_lat(:)  = pl_lat4(:)
        pl_time(:) = pl_time4(:)
        latsozc = 2
        blatc   = 0.0
      endif
      dphiozc = -(blatc+blatc)/(latsozc-1)
!
      if (me .eq. 0) then
        print *,' latsozp=',latsozp,' levozp=',levozp,' timeoz=',timeoz
        print *,' latsozc=',latsozc,' levozc=',levozc,' timeozc=',        &
                  timeozc, 'dphiozc=',dphiozc
        print *,' pl_lat=',pl_lat
        print *,' pl_pres=',pl_pres
        print *,' pl_time=',pl_time
      endif
!     pl_pres(:) = log(0.1*pl_pres(:))       ! Natural log of pres in cbars
      pl_pres(:) = log(100.0*pl_pres(:))     ! Natural log of pres in Pa
!
      allocate(gis%OZPLIN(LATSOZP,LEVOZP,pl_coeff,timeoz)) !OZONE P-L coeffcients
!     endif
!
!
      P_GZ  = 0*LEVS+0*LEVH+1  !      GZE/O(LNTE/OD,2),
      P_ZEM = 0*LEVS+0*LEVH+2  !     ZEME/O(LNTE/OD,2,LEVS),
      P_DIM = 1*LEVS+0*LEVH+2  !     DIME/O(LNTE/OD,2,LEVS),
      P_TEM = 2*LEVS+0*LEVH+2  !     TEME/O(LNTE/OD,2,LEVS),
      P_QM  = 3*LEVS+0*LEVH+2  !      QME/O(LNTE/OD,2),
      P_ZE  = 3*LEVS+0*LEVH+3  !      ZEE/O(LNTE/OD,2,LEVS),
      P_DI  = 4*LEVS+0*LEVH+3  !      DIE/O(LNTE/OD,2,LEVS),
      P_TE  = 5*LEVS+0*LEVH+3  !      TEE/O(LNTE/OD,2,LEVS),
      P_Q   = 6*LEVS+0*LEVH+3  !       QE/O(LNTE/OD,2),
      P_DLAM= 6*LEVS+0*LEVH+4  !  DPDLAME/O(LNTE/OD,2),
      P_DPHI= 6*LEVS+0*LEVH+5  !  DPDPHIE/O(LNTE/OD,2),
      P_ULN = 6*LEVS+0*LEVH+6  !     ULNE/O(LNTE/OD,2,LEVS),
      P_VLN = 7*LEVS+0*LEVH+6  !     VLNE/O(LNTE/OD,2,LEVS),
      P_W   = 8*LEVS+0*LEVH+6  !       WE/O(LNTE/OD,2,LEVS),
      P_X   = 9*LEVS+0*LEVH+6  !       XE/O(LNTE/OD,2,LEVS),
      P_Y   =10*LEVS+0*LEVH+6  !       YE/O(LNTE/OD,2,LEVS),
      P_ZQ  =11*LEVS+0*LEVH+6  !      ZQE/O(LNTE/OD,2)
      P_RT  =11*LEVS+0*LEVH+7  !      RTE/O(LNTE/OD,2,LEVH),
      P_RM  =11*LEVS+1*LEVH+7  !      RME/O(LNTE/OD,2,LEVH),
      P_RQ  =11*LEVS+2*LEVH+7  !      RQE/O(LNTE/OD,2,LEVH),
!
!**********************Current operational as of May 2009***********
!     P_GZ  = 0*LEVS+0*LEVH+1  !      GZE/O(LNTE/OD,2),
!     P_ZEM = 0*LEVS+0*LEVH+2  !     ZEME/O(LNTE/OD,2,LEVS),
!     P_DIM = 1*LEVS+0*LEVH+2  !     DIME/O(LNTE/OD,2,LEVS),
!     P_TEM = 2*LEVS+0*LEVH+2  !     TEME/O(LNTE/OD,2,LEVS),
!     P_RM  = 3*LEVS+0*LEVH+2  !      RME/O(LNTE/OD,2,LEVH),
!     P_QM  = 3*LEVS+1*LEVH+2  !      QME/O(LNTE/OD,2),
!     P_ZE  = 3*LEVS+1*LEVH+3  !      ZEE/O(LNTE/OD,2,LEVS),
!     P_DI  = 4*LEVS+1*LEVH+3  !      DIE/O(LNTE/OD,2,LEVS),
!     P_TE  = 5*LEVS+1*LEVH+3  !      TEE/O(LNTE/OD,2,LEVS),
!     P_RQ  = 6*LEVS+1*LEVH+3  !      RQE/O(LNTE/OD,2,LEVH),
!     P_Q   = 6*LEVS+2*LEVH+3  !       QE/O(LNTE/OD,2),
!     P_DLAM= 6*LEVS+2*LEVH+4  !  DPDLAME/O(LNTE/OD,2),
!     P_DPHI= 6*LEVS+2*LEVH+5  !  DPDPHIE/O(LNTE/OD,2),
!     P_ULN = 6*LEVS+2*LEVH+6  !     ULNE/O(LNTE/OD,2,LEVS),
!     P_VLN = 7*LEVS+2*LEVH+6  !     VLNE/O(LNTE/OD,2,LEVS),
!     P_W   = 8*LEVS+2*LEVH+6  !       WE/O(LNTE/OD,2,LEVS),
!     P_X   = 9*LEVS+2*LEVH+6  !       XE/O(LNTE/OD,2,LEVS),
!     P_Y   =10*LEVS+2*LEVH+6  !       YE/O(LNTE/OD,2,LEVS),
!     P_RT  =11*LEVS+2*LEVH+6  !      RTE/O(LNTE/OD,2,LEVH),
!     P_ZQ  =11*LEVS+3*LEVH+6  !      ZQE/O(LNTE/OD,2)
!**********************Current operational as of May 2009***********
!C
      LOTS = 5*LEVS+1*LEVH+3 
      LOTD = 6*LEVS+2*LEVH+0 
      LOTA = 3*LEVS+1*LEVH+1 
!
      kwq  = 0*levs+0*levh+1   !   qe/o_ls
      kwte = 0*levs+0*levh+2   !  tee/o_ls
      kwdz = 1*levs+0*levh+2   !  die/o_ls  zee/o_ls
      kwrq = 3*levs+0*levh+2   !  rqe/o_ls

!
      gis%P_GZ  = 0*LEVS+0*LEVH+1  !      GZE/O(LNTE/OD,2),
      gis%P_ZEM = 0*LEVS+0*LEVH+2  !     ZEME/O(LNTE/OD,2,LEVS),
      gis%P_DIM = 1*LEVS+0*LEVH+2  !     DIME/O(LNTE/OD,2,LEVS),
      gis%P_TEM = 2*LEVS+0*LEVH+2  !     TEME/O(LNTE/OD,2,LEVS),
      gis%P_QM  = 3*LEVS+0*LEVH+2  !      QME/O(LNTE/OD,2),
      gis%P_ZE  = 3*LEVS+0*LEVH+3  !      ZEE/O(LNTE/OD,2,LEVS),
      gis%P_DI  = 4*LEVS+0*LEVH+3  !      DIE/O(LNTE/OD,2,LEVS),
      gis%P_TE  = 5*LEVS+0*LEVH+3  !      TEE/O(LNTE/OD,2,LEVS),
      gis%P_Q   = 6*LEVS+0*LEVH+3  !       QE/O(LNTE/OD,2),
      gis%P_DLAM= 6*LEVS+0*LEVH+4  !  DPDLAME/O(LNTE/OD,2),
      gis%P_DPHI= 6*LEVS+0*LEVH+5  !  DPDPHIE/O(LNTE/OD,2),
      gis%P_ULN = 6*LEVS+0*LEVH+6  !     ULNE/O(LNTE/OD,2,LEVS),
      gis%P_VLN = 7*LEVS+0*LEVH+6  !     VLNE/O(LNTE/OD,2,LEVS),
      gis%P_W   = 8*LEVS+0*LEVH+6  !       WE/O(LNTE/OD,2,LEVS),
      gis%P_X   = 9*LEVS+0*LEVH+6  !       XE/O(LNTE/OD,2,LEVS),
      gis%P_Y   =10*LEVS+0*LEVH+6  !       YE/O(LNTE/OD,2,LEVS),
      gis%P_ZQ  =11*LEVS+0*LEVH+6  !      ZQE/O(LNTE/OD,2)
      gis%P_RT  =11*LEVS+0*LEVH+7  !      RTE/O(LNTE/OD,2,LEVH),
      gis%P_RM  =11*LEVS+1*LEVH+7  !      RME/O(LNTE/OD,2,LEVH),
      gis%P_RQ  =11*LEVS+2*LEVH+7  !      RQE/O(LNTE/OD,2,LEVH),

!C
      gis%LOTS = 5*LEVS+1*LEVH+3
      gis%LOTD = 6*LEVS+2*LEVH+0
      gis%LOTA = 3*LEVS+1*LEVH+1
!C
      allocate(gis%TEE1(LEVS))

!     gis%LSLAG=.FALSE.  ! IF FALSE EULERIAN SCHEME =.true. for semilag

!
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!!
!!      Create IO communicator and comp communicator
!!
!sela LIOPE=.FALSE.
!!       LIOPE=.TRUE.
      IF (me == 0) write(*,*) 'IO OPTION ,LIOPE :',LIOPE
!
      CALL ESMF_VMGetCurrent(vm_local, rc = ierr)
      CALL ESMF_VMGet(vm_local, mpiCommunicator = MPI_COMM_ALL,     &
                      peCount = nodes, rc = ierr)
!
      CALL MPI_COMM_DUP(MPI_COMM_ALL, MPI_COMM_ALL_DUP, ierr)
      CALL MPI_Barrier (MPI_COMM_ALL_DUP,               ierr)

      IF (NODES == 1) THEN
         LIOPE=.FALSE.
         write(*,*) 'IO OPTION RESET:,LIOPE :',LIOPE
      ENDIF
      IF (LIOPE) THEN
!       CALL MPI_COMM_SPLIT(MPI_COMM_WORLD,1,1,MPI_COMM_ALL,ierr)
        CALL MPI_COMM_RANK(MPI_COMM_ALL_DUP,nrank_all,ierr)
        icolor     = 1
        ikey       = 1
        nodes_comp = nodes-1
        if (nrank_all == nodes-1) then
!!  IO server
          write(*,*) 'IO server task'
          icolor     = 2
          gis%kcolor = MPI_UNDEFINED
          CALL MPI_COMM_SPLIT(MPI_COMM_ALL_DUP,icolor,ikey,MC_IO,ierr)
          CALL MPI_COMM_SPLIT(MPI_COMM_ALL_DUP,gis%kcolor,ikey,MC_COMP,ierr)
        else
!sela     write(*,*) 'COMPUTE SERVER TASK '
          icolor     = MPI_UNDEFINED
          gis%kcolor = 1
          CALL MPI_COMM_SPLIT(MPI_COMM_ALL_DUP,gis%kcolor,ikey,MC_COMP,ierr)
          CALL MPI_COMM_SPLIT(MPI_COMM_ALL_DUP,icolor,ikey,MC_IO,ierr)
          CALL MPI_COMM_SIZE(MC_COMP,NODES,IERR)
        endif
      ELSE
        icolor     = 2
        MC_COMP    = MPI_COMM_ALL_DUP
        nodes_comp = nodes
      ENDIF
!!
      if(me.eq.0) then
        call w3tagb('gsm     ',0000,0000,0000,'np23   ')
      endif
!!
!
      if (me == 0) then
      PRINT 100, JCAP,LEVS
100   FORMAT (' SMF ',I4,I3,' CREATED AUGUST 2000 EV OD RI ')
      PRINT*,'NUMBER OF THREADS IS ',NUM_PARTHDS()
        if (liope) then
          PRINT*,'NUMBER OF MPI PROCS IS ',NODES
          PRINT*,'NUMBER OF MPI IO PROCS IS 1 (nodes)'
        else
          PRINT*,'NUMBER OF MPI PROCS IS ',NODES
        endif
      endif
!C
      gis%CONS0    =   0.0D0
      gis%CONS0P5  =   0.5D0
      gis%CONS1200 = 1200.D0
      gis%CONS3600 = 3600.D0
!C
      if (liope) then
         if (icolor.eq.2) then
           LS_DIM = JCAP1
         else
           LS_DIM = (JCAP1-1)/NODES+1
         endif
      else
         LS_DIM = (JCAP1-1)/NODES+1
      endif
!!
!C
!CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!C
!C
! For creating the ESMF interface state with the GFS
! internal parallel structure.   Weiyu.
!---------------------------------------------------
      ALLOCATE(gis%TRIE_LS_SIZE      (npe_single_member))
      ALLOCATE(gis%TRIO_LS_SIZE      (npe_single_member))
      ALLOCATE(gis%TRIEO_LS_SIZE     (npe_single_member))
      ALLOCATE(gis%LS_MAX_NODE_GLOBAL(npe_single_member))
      ALLOCATE(gis%LS_NODE_GLOBAL    (LS_DIM*3, npe_single_member))
!---------------------------------------------------

      ALLOCATE (      gis%LS_NODE (LS_DIM*3) )
      ALLOCATE (      gis%LS_NODES(LS_DIM,NODES) )
      ALLOCATE (  gis%MAX_LS_NODES(NODES) )
!C
      ALLOCATE (  gis%LATS_NODES_A(NODES) )
      ALLOCATE ( gis%GLOBAL_LATS_A(LATG) )
!C
      ALLOCATE (  gis%LATS_NODES_R(NODES) )
      ALLOCATE ( gis%GLOBAL_LATS_R(LATR) )
!C
!     ALLOCATE (   gis%LATS_NODES_EXT(NODES) )
!     ALLOCATE ( gis%GLOBAL_LATS_EXT(LATG+2*JINTMX+2*NYPT*(NODES-1)) )
!C
!C
      gis%IPRINT = 0
!     gis%LATS_NODES_EXT = 0

! For creating the ESMF interface state with the GFS
! internal parallel structure.   Weiyu.
!---------------------------------------------------
      gis%LS_NODE_GLOBAL     = 0
      gis%LS_MAX_NODE_GLOBAL = 0
      gis%TRIEO_TOTAL_SIZE   = 0

      DO i = 1, npe_single_member
          CALL GET_LS_NODE(i-1, gis%LS_NODE_GLOBAL(1, i),               &
                            gis%LS_MAX_NODE_GLOBAL(i), gis%IPRINT)
          gis%TRIE_LS_SIZE(i) = 0
          gis%TRIO_LS_SIZE(i) = 0
          DO LOCL = 1, gis%LS_MAX_NODE_GLOBAL(i)
              gis%LS_NODE_GLOBAL(LOCL+  LS_DIM, i)   = gis%TRIE_LS_SIZE(i)
              gis%LS_NODE_GLOBAL(LOCL+  2*LS_DIM, i) = gis%TRIO_LS_SIZE(i)

              L = gis%LS_NODE_GLOBAL(LOCL, i)

              gis%TRIE_LS_SIZE(i) = gis%TRIE_LS_SIZE(i) + (JCAP+3-L)/2
              gis%TRIO_LS_SIZE(i) = gis%TRIO_LS_SIZE(i) + (JCAP+2-L)/2
          END DO
          gis%TRIEO_LS_SIZE(i) = gis%TRIE_LS_SIZE(i)  + gis%TRIO_LS_SIZE(i) + 3
          gis%TRIEO_TOTAL_SIZE = gis%TRIEO_TOTAL_SIZE + gis%TRIEO_LS_SIZE(i)
      END DO

      DO i = 1, 3*LS_DIM
          gis%LS_NODE(i) = gis%LS_NODE_GLOBAL(i, me+1)
      END DO

      LS_MAX_NODE = gis%LS_MAX_NODE_GLOBAL(me+1)
      LEN_TRIE_LS = max(1, gis%TRIE_LS_SIZE(me+1))
      LEN_TRIO_LS = max(1, gis%TRIO_LS_SIZE(me+1))
      IF(LIOPE) THEN
          IF(me == 0) CALL mpi_send(gis%TRIE_LS_SIZE,    &
                                    npe_single_member,   &
                                    mpi_integer,         &
                                    npe_single_member-1, &
                                    900,                 &
                                    MPI_COMM_ALL_DUP,    &
                                    ierr)
          IF(me == npe_single_member-1)                  &
                      CALL mpi_recv(gis%TRIE_LS_SIZE,    &
                                    npe_single_member,   &
                                    mpi_integer,         &
                                    0,                   &
                                    900,                 &
                                    MPI_COMM_ALL_DUP,    &
                                    status,              &
                                    ierr)
          IF(me == 0) CALL mpi_send(gis%TRIO_LS_SIZE,    &
                                    npe_single_member,   &
                                    mpi_integer,         &
                                    npe_single_member-1, &
                                    900,                 &
                                    MPI_COMM_ALL_DUP,    &
                                    ierr)
          IF(me == npe_single_member-1)                  &
                      CALL mpi_recv(gis%TRIO_LS_SIZE,    &
                                    npe_single_member,   &
                                    mpi_integer,         &
                                    0,                   &
                                    900,                 &
                                    MPI_COMM_ALL_DUP,    &
                                    status,              &
                                    ierr)
          IF(me == 0) CALL mpi_send(gis%TRIEO_LS_SIZE,   &
                                    npe_single_member,   &
                                    mpi_integer,         &
                                    npe_single_member-1, &
                                    900,                 &
                                    MPI_COMM_ALL_DUP,    &
                                    ierr)
          IF(me == npe_single_member-1)                  &
                      CALL mpi_recv(gis%TRIEO_LS_SIZE,   &
                                    npe_single_member,   &
                                    mpi_integer,         &
                                    0,                   &
                                    900,                 &
                                    MPI_COMM_ALL_DUP,    &
                                    status,              &
                                    ierr)
          IF(me == 0) CALL mpi_send(gis%TRIEO_TOTAL_SIZE,&
                                    1,                   &
                                    mpi_integer,         &
                                    npe_single_member-1, &
                                    900,                 &
                                    MPI_COMM_ALL_DUP,    &
                                    ierr)
          IF(me == npe_single_member-1)                  &
                      CALL mpi_recv(gis%TRIEO_TOTAL_SIZE,&
                                    1,                   &
                                    mpi_integer,         &
                                    0,                   &
                                    900,                 &
                                    MPI_COMM_ALL_DUP,    &
                                    status,              &
                                    ierr)
     END IF
!-----------------------------------------------------------

!!    CALL GET_LS_NODE( ME, gis%LS_NODE, LS_MAX_NODE, gis%IPRINT )
!C
!C
!!    LEN_TRIE_LS=0
!!    LEN_TRIO_LS=0
!!    DO LOCL=1,LS_MAX_NODE
!!         gis%LS_NODE(LOCL+  LS_DIM)=LEN_TRIE_LS
!!        gis%LS_NODE(LOCL+2*LS_DIM)=LEN_TRIO_LS
!!       L=gis%LS_NODE(LOCL)
!!       LEN_TRIE_LS=LEN_TRIE_LS+(JCAP+3-L)/2
!!       LEN_TRIO_LS=LEN_TRIO_LS+(JCAP+2-L)/2
!!    ENDDO
!C
!C
      ALLOCATE (       gis%EPSE  (LEN_TRIE_LS) )
      ALLOCATE (       gis%EPSO  (LEN_TRIO_LS) )
      ALLOCATE (       gis%EPSEDN(LEN_TRIE_LS) )
      ALLOCATE (       gis%EPSODN(LEN_TRIO_LS) )
!C
      ALLOCATE (      gis%SNNP1EV(LEN_TRIE_LS) )
      ALLOCATE (      gis%SNNP1OD(LEN_TRIO_LS) )
!C
      ALLOCATE (       gis%NDEXEV(LEN_TRIE_LS) )
      ALLOCATE (       gis%NDEXOD(LEN_TRIO_LS) )
!C
      ALLOCATE (      gis%PLNEV_A(LEN_TRIE_LS,LATG2) )
      ALLOCATE (      gis%PLNOD_A(LEN_TRIO_LS,LATG2) )
      ALLOCATE (      gis%PDDEV_A(LEN_TRIE_LS,LATG2) )
      ALLOCATE (      gis%PDDOD_A(LEN_TRIO_LS,LATG2) )
      ALLOCATE (      gis%PLNEW_A(LEN_TRIE_LS,LATG2) )
      ALLOCATE (      gis%PLNOW_A(LEN_TRIO_LS,LATG2) )
!C
      ALLOCATE (      gis%PLNEV_R(LEN_TRIE_LS,LATR2) )
      ALLOCATE (      gis%PLNOD_R(LEN_TRIO_LS,LATR2) )
      ALLOCATE (      gis%PDDEV_R(LEN_TRIE_LS,LATR2) )
      ALLOCATE (      gis%PDDOD_R(LEN_TRIO_LS,LATR2) )
      ALLOCATE (      gis%PLNEW_R(LEN_TRIE_LS,LATR2) )
      ALLOCATE (      gis%PLNOW_R(LEN_TRIO_LS,LATR2) )
!C
      gis%MAXSTP=36

 
      IF(ME.EQ.0) PRINT*,'FROM COMPNS : IRET=',gis%IRET,' NSOUT=',NSOUT, &
       ' NSSWR=',NSSWR,' NSLWR=',NSLWR,' NSZER=',NSZER,' NSRES=',NSRES,  &
       ' NSDFI=',NSDFI,' NSCYC=',NSCYC,' RAS=',RAS
      IF(gis%IRET.NE.0) THEN
        IF(ME.EQ.0) PRINT *,' INCOMPATIBLE NAMELIST - ABORTED IN MAIN'
        CALL MPI_QUIT(13)
      ENDIF
!!
!     IF PREDICTED OZON IS DESIRED SET JO3=2
      JO3 = 2          !USING PREDICTED OZONE IN RADIATION.
!C
!!    gis%LATS_NODES_EXT = 0

      CALL GETCON(gis%NGES,gis%NRADR,gis%NRADF,gis%NNMOD,               &
           gis%N3,gis%N4,gis%NFLPS,gis%NSIGI,gis%NSIGS,gis%NSFCI,       &
           gis%NZNLI,gis%NSFCF,gis%NZNLF,gis%NSFCS,gis%NZNLS,           &
           gis%NDGI,gis%NDGF,gis%NGPKEN,                                &
           gis%MODS,gis%NITER,gis%INI,gis%NSTEP,gis%NFILES,             &
           gis%KSOUT,gis%IFGES,gis%IBRAD,                               &
           gis%LS_NODE,gis%LS_NODES,gis%MAX_LS_NODES,                   &
           gis%LATS_NODES_A,gis%GLOBAL_LATS_A,                          &
           gis%LONSPERLAT,                                              &
           gis%LATS_NODES_R,gis%GLOBAL_LATS_R,                          &
           gis%LONSPERLAR,                                              &
!          gis%LATS_NODES_EXT,gis%GLOBAL_LATS_EXT,                      &
           gis%EPSE,gis%EPSO,gis%EPSEDN,gis%EPSODN,                     &
           gis%SNNP1EV,gis%SNNP1OD,gis%NDEXEV,gis%NDEXOD,               &
           gis%PLNEV_A,gis%PLNOD_A,gis%PDDEV_A,gis%PDDOD_A,             &
           gis%PLNEW_A,gis%PLNOW_A,                                     &
           gis%PLNEV_R,gis%PLNOD_R,gis%PDDEV_R,gis%PDDOD_R,             &
           gis%PLNEW_R,gis%PLNOW_R,gis%colat1)
!!
      call sfcvar_aldata(lonr,lats_node_r,lsoil,gis%sfc_fld,ierr)
      call flxvar_aldata(lonr,lats_node_r,gis%flx_fld,ierr)


!li, added 05/31/2007 (for oceanic component)
      IF (me == 0) write(*,*) ' in "GFS_Initialize_ESMFMod,lonr,lats_node_r,nr_nst,nf_nst : ',lonr,lats_node_r,nr_nst,nf_nst
!    Modified by Moorthi
      call nstvar_aldata(lonr,lats_node_r,gis%nst_fld,ierr)

      ALLOCATE (   gis%XLON(LONR,LATS_NODE_R))
      ALLOCATE (   gis%XLAT(LONR,LATS_NODE_R))
      ALLOCATE (   gis%COSZDG(LONR,LATS_NODE_R))
      ALLOCATE (   gis%SFALB(LONR,LATS_NODE_R))
      ALLOCATE (   gis%HPRIME(LONR,NMTVR,LATS_NODE_R))
      ALLOCATE (   gis%FLUXR(LONR,nfxr,LATS_NODE_R))

!     gis%NBLCK = LONR/NGPTC + 1
      ALLOCATE (   gis%SWH(LONR,LEVS,LATS_NODE_R))
      ALLOCATE (   gis%HLW(LONR,LEVS,LATS_NODE_R))

      ALLOCATE (gis%JINDX1(LATS_NODE_R),gis%JINDX2(LATS_NODE_R))
      ALLOCATE (gis%DDY(LATS_NODE_R))
!
      allocate (gis%phy_f3d(LONR,LEVS,num_p3d,lats_node_r))
      allocate (gis%phy_f2d(lonr,num_p2d,lats_node_r))
      allocate (gis%dyn_f3d(LONF,LEVS,num_a3d,lats_node_a))
      allocate (gis%dyn_f2d(lonf,num_a2d,lats_node_a))
!
      call d3d_init(lonr,lats_node_r,levs,pl_coeff,ldiag3d)

!     if (ldiag3d) then
!       call d3d_init(ngptc,gis%nblck,lonr,lats_node_r,levs,pl_coeff)
!     else
!       call d3d_init(1,gis%nblck,1,lats_node_r,1,pl_coeff) ! Needs allocation
!     endif
      if (gfsio_out .or. gfsio_in) then
        call gfsio_init(ierr)
      endif

      io_task   = icolor == 2 .and. me == nodes-1
      comp_task = icolor /= 2 .or. .not. liope
!     if (icolor /= 2 .or. .not. liope) then
      if (comp_task) then
        if (num_p3d > 0) gis%phy_f3d = 0.0
        if (num_p2d > 0) gis%phy_f2d = 0.0
        if (num_a3d > 0) gis%dyn_f3d = 0.0
        if (num_a2d > 0) gis%dyn_f2d = 0.0
      endif
      if (num_p2d .gt. 0) gis%phy_f2d = 0.0
      if (num_a2d .gt. 0) gis%dyn_f2d = 0.0

!!
! Modified by Weiyu.
!-------------------
!     if (.NOT.LIOPE.or.icolor.ne.2) then
!     if (comp_task) then
!!
      ALLOCATE (   gis%TRIE_LS(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6) )
      ALLOCATE (   gis%TRIO_LS(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6) )

! For Ensemble forecast requirement, add two more arrays to save to
! initial conditions.   Weiyu.
!------------------------------------------------------------------
      ALLOCATE (   gis%TRIE_LS_INI(LEN_TRIE_LS,2,11*LEVS+3*LEVH+6) )
      ALLOCATE (   gis%TRIO_LS_INI(LEN_TRIO_LS,2,11*LEVS+3*LEVH+6) )

!C
!C
!     ALLOCATE (   gis%SYN_GR_A_1(LONFX*gis%LOTS,LATS_DIM_EXT) )
!     ALLOCATE (   gis%SYN_GR_A_2(LONFX*gis%LOTS,LATS_DIM_EXT) )
!     ALLOCATE (   gis%DYN_GR_A_1(LONFX*gis%LOTD,LATS_DIM_EXT) )
!     ALLOCATE (   gis%DYN_GR_A_2(LONFX*gis%LOTD,LATS_DIM_EXT) )
!     ALLOCATE (   gis%ANL_GR_A_1(LONFX*gis%LOTA,LATS_DIM_EXT) )
!     ALLOCATE (   gis%ANL_GR_A_2(LONFX*gis%LOTA,LATS_DIM_EXT) )
!!
!     endif !(.NOT.LIOPE.or.icolor.ne.2)
!!
      if (me == 0) then
        PRINT*, ' LATS_DIM_A=', LATS_DIM_A, ' LATS_NODE_A=', LATS_NODE_A
!       PRINT*, ' LATS_DIM_EXT=', LATS_DIM_EXT,              &
!               ' LATS_NODE_EXT=', LATS_NODE_EXT
        PRINT*, ' LATS_DIM_R=', LATS_DIM_R, ' LATS_NODE_R=', LATS_NODE_R
      endif
!
      ILAT=LATS_NODE_A

!     IF (gis%LSLAG) THEN
!       ILAT=LATS_NODE_EXT
!     ELSE
!       ILAT=LATS_NODE_A
!     ENDIF
!!
!     WRITE(*,*) 'NUMBER OF LATITUDES EXT. :',LATS_NODE_EXT,              &
!                 LATS_DIM_EXT,LATS_NODE_A
!!
!JFE  ALLOCATE (LATLOCAL(LATGD,0:NODES-1))
!JFE  ALLOCATE (LBASIY(4,2,LATS_NODE_EXT))
!JFE  ALLOCATE (PHI(LATS_NODE_EXT))
!JFE  ALLOCATE (DPHI(LATS_NODE_EXT))
!JFE  ALLOCATE (DLAM(LATS_NODE_EXT))
!JFE  ALLOCATE (LAMEXT(LONFX,LATS_NODE_EXT))
!JFE  ALLOCATE (LAM(LONFX,LATS_NODE_A+1))
!JFE  ALLOCATE (LAMMP(LONF,LEVS,LATS_NODE_A))
!JFE  ALLOCATE (SIGMP(LONF,LEVS,LATS_NODE_A))
!JFE  ALLOCATE (PHIMP(LONF,LEVS,LATS_NODE_A))
!!
!JFE  ALLOCATE (LATSINPE(LATS_NODE_A))
!JFE  JPT=0
!JFE  DO NODE=1,NODES
!JFE     IF ( LATS_NODES_A(NODE) .GT. 0 .AND.ME+1.EQ.NODE) THEN
!JFE        DO JCOUNT=1,LATS_NODES_A(NODE)
!JFE           LATSINPE(JCOUNT)=GLOBAL_LATS_A(JPT+JCOUNT)
!JFE        ENDDO
!JFE     ENDIF
!JFE     JPT=JPT+LATS_NODES_A(NODE)
!JFE  ENDDO
!!
!JFE  IF (LSLAG) THEN
!JFE    CALL SULAG(LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,
!JFE &           LBASIY,LAMMP,PHIMP,SIGMP,gis%LONSPERLAT,
!JFE &           IPRINT,LATSINPE)
!JFE  ENDIF
!!
      if (me == 0) then
        print *,' sig_ini=',gis%nam_gfs%sig_ini                        &
     &         ,' sig_ini2=',gis%nam_gfs%sig_ini2                      &
     &         ,' sfc_ini=',gis%nam_gfs%sfc_ini
        print *,' nst_ini=',gis%nam_gfs%nst_ini
      endif
      gis%pdryini = 0.0
      CALL spect_fields(gis%n1, gis%n2,                                &
        gis%PDRYINI, gis%TRIE_LS,  gis%TRIO_LS,                        &
        gis%LS_NODE, gis%LS_NODES, gis%MAX_LS_NODES,                   &
        gis%SNNP1EV, gis%SNNP1OD,  gis%phy_f3d,  gis%phy_f2d,          &
        gis%dyn_f3d,  gis%dyn_f2d,                                     &
        gis%global_lats_r, gis%lonsperlar,                             &
        gis%global_lats_a, gis%lonsperlat,                             &
        gis%epse, gis%epso, gis%plnev_r, gis%plnod_r,                  &
        gis%plnew_r, gis%plnow_r, gis%lats_nodes_r, gis%lats_nodes_a,  &
        gis%nam_gfs%sig_ini, gis%nam_gfs%sig_ini2)
!!
      if(.not.adiab)then
      CALL fix_fields(gis%LONSPERLAR,gis%GLOBAL_LATS_R,                &
        gis%XLON,gis%XLAT,gis%sfc_fld,gis%nst_fld,                     &
        gis%HPRIME,gis%JINDX1,gis%JINDX2,gis%DDY,                      &
        gis%OZPLIN,gis%nam_gfs%sfc_ini,gis%nam_gfs%nst_ini)
      endif


!   if ( me == 72 ) then
!   do j = 1, lats_node_r
!   do i = 1, lonr
!     if ( gis%dt_warm(i,j) > 0.8 ) then
!       write(*,'(a,11F11.2)') 'Initial nstr : ', &
!       gis%ifd(i,j),gis%time_old(i,j),gis%time_ins(i,j),gis%I_Sw(i,j), &
!       gis%I_Q(i,j),gis%I_Qrain(i,j),gis%I_M(i,j),gis%I_Tau(i,j), &
!       gis%I_Sw_Zw(i,j),gis%I_Q_Ts(i,j),gis%I_M_Ts(i,j)
!       write(*,'(a,9F10.5)')  'Initial nstf : ', &
!       gis%Tref(i,j),gis%dt_cool(i,j),gis%z_c(i,j),gis%dt_warm(i,j),gis%z_w(i,j), &
!       gis%c_0(i,j),gis%c_d(i,j),gis%w_0(i,j),gis%w_d(i,j)
!     endif
!   enddo
!   enddo
!   endif


!
!       Apply the diurnal warming & sub-layer cooling (TSEA: foundation/reference temperature)
!

!     if ( .not. tr_analysis ) then
!       gis%nst_fld%Tref(:,:) = gis%sfc_fld%TSEA(:,:)    ! necessary only when Tr analysis unavailable
!     endif

!     if ( nst_fcst > 0 ) then
!       do j = 1, lats_node_r
!         do i = 1, lonr
!           if ( gis%sfc_fld%SLMSK(i,j) == 0.0 ) then
!             gis%sfc_fld%TSEA(i,j) = gis%nst_fld%Tref(i,j)             &
!                + gis%nst_fld%dt_warm(i,j) - gis%nst_fld%dt_cool(i,j)
!           endif
!         enddo
!       enddo
!     endif


!!
      tov = 0.0
!!

! Modified by Weiyu Yang to fix the bug related to the "runDuration".
!--------------------------------------------------------------------
      CALL ESMF_ClockGet(clock, timeStep    = timeStep,    &
                                startTime   = startTime,   &
                                currTime    = currTime,   &
                                rc          = rc1)

      runDuration_hour  = NINT(FHMAX) - NINT(FHINI)
      CALL ESMF_TimeIntervalSet(runDuration, h = runDuration_hour, rc = rc1)

!wy      CALL ESMF_ClockGet(clock, timeStep    = timeStep,    & 
!wy                                runDuration = runDuration, &
!wy                                startTime   = startTime,   &
!wy                                currTime    = currTime,   &
!wy                                rc          = rc1)
 
!
!     currTime = startTime
!
!     CALL ESMF_TimeIntervalGet(timeStep, s = timeStep_sec, rc = rc1)

!  print *,' timestep_sec=',timestep_sec,' rc1=',rc1

!wy CALL ESMF_TimeIntervalGet(runDuration, h = runDuration_hour, rc = rc1)

!  print *,' runduration_hour=',runduration_hour,' rc1=',rc1
!
!Moor ifhmax = NINT(gis%nam_gfs%FHMAX)
      ifhmax = NINT(FHMAX)
      IF(runDuration_hour <= 0    .OR.                  &
          ifhmax /= 0             .AND.                 &
          ifhmax <= gis%kfhour + runDuration_hour) THEN
!Moor     gis%nam_gfs%FHMAX = MAX(gis%nam_gfs%FHMAX, REAL(gis%kfhour))
! ,,      ifhmax            = NINT(gis%nam_gfs%FHMAX)
          ifhmax            = NINT(FHMAX)
! ,,      runDuration_hour  = ifhmax - gis%kfhour
          runDuration_hour  = NINT(FHMAX) - NINT(FHINI)
          CALL ESMF_TimeIntervalSet(runDuration, h = runDuration_hour, rc = rc1)
!  print *,' runduration_hour=',runduration_hour,' rc1=',rc1
      END IF
      if (runDuration_hour < 0) then
        print *,' FHINI=',FHINI, ' > FHMAX=',FHMAX,' JOB ABORTED'
        call mpi_quit(444)
      endif
!     stopTime = startTime + runDuration
      stopTime = currTime  + runDuration

      CALL ESMF_ClockSet(clock, stopTime = stopTime, &
!                               currTime = currTime, &
                                rc       = rc1)
!
      CALL ESMF_TimeIntervalGet(timeStep, s = timeStep_sec, rc = rc1)

      if (me == 0) print *,' timestep_sec=',timestep_sec,' rc1=',rc1
!!
      IF (me.eq.0) THEN
        CALL out_para(REAL(timeStep_sec))
      ENDIF
!!
      IF (me.eq.0) THEN
        PRINT *,' THE GSM WILL FORECAST ',runDuration_hour,' HOURS',      &
                ' FROM HOUR ',gis%kfhour,' TO HOUR ',runDuration_hour+gis%kfhour
      ENDIF
!
!
!CALL ESMF_TimeGet (stopTime, yy = yyc, mm = mmc, dd = ddc, h = hhc, &
!                             m = minsc, rc = rc1)
!PRINT*, ' In Initialize , stopTime=', yyc, mmc, ddc, hhc, minsc
!

!
      if (.not. adiab) then
!  ---  set up parameters for radiation initialization

        sas_shal = sashal .and. (.not. ras)

!  ---  set up sigma levels before radiation initialization

        if ( hybrid .and. (.not.gen_coord_hybrid) ) then

!  ---  following sela: si(k)=(ak5(k)+bk5(k)*Typical_pgr)/Typical_pgr
!       ak(k) and bk(k) go from top to bottom, adjust si_loc direction
!       according model direction flag iflip.

!         if (iflip == 1) then        ! vertical from sfc to top
            si_loc(levr+1) = ak5(1)/typical_pgr+bk5(1)
            do k = 1, levr
              si_loc(levr+1-k) = ak5(levp1-levr+k)/typical_pgr          &
     &                         + bk5(levp1-levr+k)
            enddo
!         else                        ! vertical from top to sfc
!           si_loc(1) = ak5(1)/typical_pgr+bk5(1)
!           do k = 1, levr
!             si_loc(k+1)      = ak5(levp1-levr+k)/typical_pgr          &
!    &                         + bk5(levp1-levr+k)
!           enddo
!         endif
        else

!  ---  using the model's native sigama coordinate, si
!       si(k) goes from bottom to top, adjust si_loc direction
!       according model direction flag iflip.

!         if (iflip == 1) then        ! vertical from sfc to top
            do k = 1, levr
              si_loc(k) = si(k)
            enddo
            si_loc(levr+1) = si(levp1)
!         else                        ! vertical from top to sfc
!           do k = 1, levr
!             si_loc(k+1) = si(levr-k+1)
!           enddo
!           si_loc(1) = si(levp1)
!         endif
        endif    ! end_if_hybrid

        call rad_initialize                                             &
!  ---  inputs:
     &     ( si_loc,levr,ictm,isol,ico2,iaer,iaer_mdl,ialb,iems,ntcw,   &
     &       num_p3d,ntoz,iovr_sw,iovr_lw,isubc_sw,isubc_lw,            &
     &       sas_shal,crick_proof,ccnorm,norad_precip,idate,iflip,me )
!  ---  outputs: ( none )

      endif
!
!     zero fluxes and diagnostics
!
      gis%zhour = fhour
      gis%FLUXR = 0.

      deallocate( si_loc )
!
      call flx_init(gis%flx_fld,ierr)
!
      call d3d_zero(ldiag3d)

!     if (ldiag3d) then
!       call d3d_zero
!     endif
      if (me == 0) print *,' Return from GFS_Initialize'
!
 END SUBROUTINE GFS_Initialize

 END MODULE GFS_Initialize_ESMFMod
