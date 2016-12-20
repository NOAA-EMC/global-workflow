!
! !MODULE: GFS_Run_ESMFMod --- Run module of the ESMF grided
!                              component of the GFS system.
!
! !DESCRIPTION: GFS run module.
!
! !REVISION HISTORY:
!
!  November 2004      Weiyu Yang Initial code.
!  May      2005      Weiyu Yang, updated to the new version of GFS.
!  november 2006      J.Sela updated for semilagrange version
!  May      2009      S.Moorthi merged  SL and GEFS and Parallel GFS versions

!
!
! !INTERFACE:
!
 MODULE GFS_Run_ESMFMod
!
!!USES:
!
 USE GFS_InternalState_ESMFMod

 IMPLICIT none

 CONTAINS

 SUBROUTINE GFS_Run(clock, gis, rc)

 TYPE(ESMF_Clock),                 INTENT(inout) :: clock
 TYPE(GFS_InternalState), POINTER, INTENT(inout) :: gis
 INTEGER, OPTIONAL,                INTENT(out)   :: rc
 INTEGER                                         :: rc1 = ESMF_SUCCESS

 TYPE(ESMF_TimeInterval)                         :: timeStep,  doneTime
 TYPE(ESMF_Time)                                 :: startTime, currTime
!INTEGER(ESMF_KIND_I8)                           :: advanceCount
 INTEGER                                         :: advanceCount4, advanceCounts
 INTEGER                                         :: advanceCount_StartPoint

! For testing the ESMF States.
!-----------------------------
 TYPE(ESMF_Time)                                 :: stopTime
 TYPE(ESMF_TimeInterval)                         :: runDuration
!INTEGER                                         :: advanceCount5
 INTEGER                                         :: dd, hh, yy, mm, mins

 INTEGER                                         :: timeStep_sec
 REAL                                            :: timeStep_sec_r
 REAL                                            :: deltim_loc

 INTEGER                                         :: k
 LOGICAL                                         :: LSOUT

!***********************************************************************
!
!     LSFWD      LOGICAL TRUE DURING A FIRST FORWARD STEP
!     LSSAV      LOGICAL TRUE DURING A STEP FOR WHICH
!                DIAGNOSTICS ARE ACCUMULATED
!     LSCCA      LOGICAL TRUE DURING A STEP FOR WHICH CONVECTIVE CLOUDS
!                ARE CALCULATED FROM CONVECTIVE PRECIPITATION RATES
!     LSSWR      LOGICAL TRUE DURING A STEP FOR WHICH
!                SOLAR RADIATION HEATING RATES ARE COMPUTED
!     LSLWR      LOGICAL TRUE DURING A STEP FOR WHICH
!                LONGWAVE RADIATION HEATING RATES ARE COMPUTED
!     PHOUR      REAL FORECAST HOUR AT THE END OF THE PREVIOUS TIMESTEP
!     FHSWR      REAL FREQUENCY OF SOLAR RADIATION AND CONVECTIVE CLOUD
!                (DEFAULT: 1.)
!     fhrot      if =0 Read One Time level sigmas and te=tem zem=ze ...
!     fhdfi      if =0 no digital filter
!
!     lsout controls freq. of output
!     nsout controls freq. of output in time steps
!     fhout controls freq. of output in hours
!
!     nsres time steps between writing restart files
!     nszer time steps between zeroing fluxes
!
!***********************************************************************
!

 CALL ESMF_ClockGet(clock,           timeStep     = timeStep,     &
                                     startTime    = startTime,    &
                                     currTime     = currTime,     &
                                     rc           = rc1)

 doneTime       = currTime - startTime
 advanceCount4  = nint(doneTime/timeStep)
 advanceCounts  = advanceCount4

 CALL ESMF_TimeIntervalGet(timeStep, s = timeStep_sec, rc=rc1 )
 timeStep_sec_r = REAL(timeStep_sec)

!   print *,' bef ATM advancecount4=',advancecount4,' timestep=',timestep_sec_r

!-> Coupling insertion
    call ATM_ANNOUNCE('to call ATM_TILES_INIT',2)
!   print*,'lats_node_r=',lats_node_r
    call ATM_TILES_INIT(gis%lonr,gis%latr,gis%lonf,gis%latg,lats_node_r,ipt_lats_node_r,gis%global_lats_r,gis%lonsperlar)

    call ATM_ANNOUNCE('to call ATM_SURF_INIT',2)
    CALL ATM_SURF_INIT

    call ATM_ANNOUNCE('to send grid',2)
    call ATM_SENDGRID(gis%XLON,gis%XLAT)

    call ATM_ANNOUNCE('to receive C time step',2)
    call ATM_RECVdtc(gis%DELTIM)
    call ATM_ANNOUNCE('C time step received',2)

    call ATM_ANNOUNCE('to send SLMSK',2)
    call ATM_SENDSLM(gis%sfc_fld%SLMSK)
    call ATM_ANNOUNCE('SLMSK sent',2)
!<- Coupling insertion
!**************************************************
!   print *, ' DHOU GFS_RUN', FHOUR, FHROT, advancecount4
!   print *, ' DHOU ESMF_ClockIsStopTime', ESMF_ClockIsStopTime(clock,       rc = rc1)

    if (me == 0) print *,' advancecount4=',advancecount4,' timestep=',timestep_sec_r, 'SPS=', gis%SPS

!***********************************************************************
! DHOU, 02-20-2008 Added this block to RE-WRITE output/restart files
! DHOU, 05-29-2008 Re-structured this part of flow control
!       the do_tstep_wrt subroutine is adopted from do_tstep
      LSOUT=.false.
!     IF (ESMF_ClockIsStopTime(clock, rc = rc1)) LSOUT=.true.
! DHOU, 09-08-2008 Control is replaced by the HOUTASPS passed in from namelist

      IF ( INT(FHOUR+0.5) .eq. gis%HOUTASPS ) LSOUT=.true.
      IF (gis%SPS.and.FHOUR.gt.1.0) THEN

!     if (me == 0) print *, 'RE-WRITE output files after stochastic at FHOUR', FHOUR

      CALL ensemble_wrt(advanceCount4, gis%PHOUR,                      &
                 gis%TRIE_LS,gis%TRIO_LS,                              &
                 gis%LS_NODE,gis%LS_NODES,gis%MAX_LS_NODES,            &
                 gis%LATS_NODES_R,gis%GLOBAL_LATS_R,gis%LONSPERLAR,    &
                 gis%LATS_NODES_A,gis%GLOBAL_LATS_A,gis%LONSPERLAT,    &
                 gis%EPSEDN,gis%EPSODN,gis%SNNP1EV,gis%SNNP1OD,        &
                 gis%PLNEV_R,gis%PLNOD_R,gis%PDDEV_R,gis%PDDOD_R,      &
                 gis%PLNEW_R,gis%PLNOW_R,                              &
           gis%XLON,gis%XLAT,gis%sfc_fld,gis%flx_fld,gis%nst_fld,      &
           gis%fluxr,gis%pdryini,                                      &
           gis%phy_f3d,  gis%phy_f2d,                                  &
           gis%dyn_f3d,  gis%dyn_f2d,                                  &
           gis%ZHOUR,gis%N1,gis%N4,LSOUT,gis%COLAT1,gis%CFHOUR1)
      ENDIF
!DHOU 05/29/2008 added  the following control
      IF (ESMF_ClockIsStopTime(clock, rc = rc1))  RETURN

!**************************************************

      IF(FHOUR.EQ.FHROT) THEN
       if(fhdfi.ne.0.) then !------------------do digital filter
!***********************************************************************
!
         NSDFI=NINT(FHDFI*3600.0/timeStep_sec_r)
         CALL tldfi(timeStep_sec_r, advanceCount4, gis%PHOUR,            &
                    gis%TRIE_LS,gis%TRIO_LS,                             &
                    gis%LS_NODE,gis%LS_NODES,gis%MAX_LS_NODES,           &
                    gis%LATS_NODES_A,gis%GLOBAL_LATS_A,                  &
                    gis%LONSPERLAT,                                      &
                    gis%LATS_NODES_R,gis%GLOBAL_LATS_R,                  &
                    gis%LONSPERLAR,                                      &
!                   gis%LATS_NODES_EXT,gis%GLOBAL_LATS_EXT,              &
                    gis%EPSE,gis%EPSO,gis%EPSEDN,gis%EPSODN,             &
                    gis%SNNP1EV,gis%SNNP1OD,gis%NDEXEV,gis%NDEXOD,       &
                    gis%PLNEV_A,gis%PLNOD_A,gis%PDDEV_A,gis%PDDOD_A,     &
                    gis%PLNEW_A,gis%PLNOW_A,                             &
                    gis%PLNEV_R,gis%PLNOD_R,gis%PDDEV_R,gis%PDDOD_R,     &
                    gis%PLNEW_R,gis%PLNOW_R,                             &
!                   gis%SYN_GR_A_1,gis%DYN_GR_A_1,gis%ANL_GR_A_1,        &
!                   gis%SYN_GR_A_2,gis%DYN_GR_A_2,gis%ANL_GR_A_2,        &
                    gis%XLON,gis%XLAT,gis%COSZDG,gis%sfc_fld,gis%flx_fld,&
                    gis%nst_fld,                                         &
                    gis%HPRIME,gis%swh,gis%hlw,gis%FLUXR ,               &
                    gis%SFALB,gis%SLAG,gis%SDEC,gis%CDEC,                &
                    gis%OZPLIN,gis%JINDX1,gis%JINDX2,                    &
                    gis%DDY,gis%PDRYINI,                                 &
                    gis%phy_f3d,  gis%phy_f2d,                           &
                    gis%dyn_f3d,  gis%dyn_f2d,                           &
                    gis%ZHOUR,gis%N1,gis%N4,gis%LSOUT,gis%COLAT1,        &
                    gis%CFHOUR1,gis%fscav,gis%SPS,gis%nam_gfs%Total_Member)

         gis%phour=FHOUR
!
!***********************************************************************
 
       endif !  -----------------------------end digital filter
!!
      advanceCount4 = advanceCount4 + 1         ! Check this out !!!! Wei Yu
      FHOUR     = REAL(advanceCount4) * timeStep_sec_r / 3600.0
      lssav     = .true. !always true, except in digital filter
      lsswr     = .true. !ex short wave radaition, used in gloopr(astronomy)
      lslwr     = .true. !ex long  wave radaition, used in gloopr(astronomy)
      lsfwd     = .true. !true only during forward step
      lscca     = .false.!get clouds from precp.(1st step use fixio_R clds)
      gis%lsout = MOD(advanceCount4 ,NSOUT).EQ.0 .OR. gis%PHOUR.EQ.0.
      if (nsout_hf > 0 .and. gis%phour <= fhmax_hf)                          &
        gis%lsout = MOD(advanceCount4 ,NSOUT_hf) == 0 .OR. gis%lsout
!
      if(hybrid)then
        call get_cd_hyb(timeStep_sec_r/2.)
      elseif( gen_coord_hybrid ) then                                  ! hmhj
        call get_cd_hyb_gc(timeStep_sec_r/2.)                          ! hmhj
      endif
!
!       Performing the first time step.
!       *******************************

      if (semilag) then
        deltim_loc = timeStep_sec_r
      else
        deltim_loc = timeStep_sec_r * 0.5
      endif
      if (me == 0) print*,' GFS_RUN -- semilag,deltim_loc = ',semilag,deltim_loc
!
      CALL do_tstep(deltim_loc, advanceCount4, gis%PHOUR,              &
!JFE             LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,       &
!JFE             LBASIY,LATSINPE,LAMMP,PHIMP,SIGMP,                    &
                 gis%TRIE_LS,gis%TRIO_LS,                              &
                 gis%LS_NODE,gis%LS_NODES,gis%MAX_LS_NODES,            &
                 gis%LATS_NODES_A,gis%GLOBAL_LATS_A,                   &
                 gis%LONSPERLAT,                                       &
                 gis%LATS_NODES_R,gis%GLOBAL_LATS_R,                   &
                 gis%LONSPERLAR,                                       &
!                gis%LATS_NODES_EXT,gis%GLOBAL_LATS_EXT,               &
                 gis%EPSE,gis%EPSO,gis%EPSEDN,gis%EPSODN,              &
                 gis%SNNP1EV,gis%SNNP1OD,gis%NDEXEV,gis%NDEXOD,        &
                 gis%PLNEV_A,gis%PLNOD_A,gis%PDDEV_A,gis%PDDOD_A,      &
                 gis%PLNEW_A,gis%PLNOW_A,                              &
                 gis%PLNEV_R,gis%PLNOD_R,gis%PDDEV_R,gis%PDDOD_R,      &
                 gis%PLNEW_R,gis%PLNOW_R,                              &
!                gis%SYN_GR_A_1,gis%DYN_GR_A_1,gis%ANL_GR_A_1,         &
!                gis%SYN_GR_A_2,gis%DYN_GR_A_2,gis%ANL_GR_A_2,         &
                 gis%XLON,gis%XLAT,gis%COSZDG,gis%sfc_fld,gis%flx_fld, &
                 gis%nst_fld,                                          &
                 gis%HPRIME,gis%SWH,gis%HLW,gis%FLUXR ,                &
                 gis%SFALB,gis%SLAG,gis%SDEC,gis%CDEC,                 &
                 gis%OZPLIN,gis%JINDX1,gis%JINDX2,                     &
                 gis%DDY,gis%PDRYINI,                                  &
                 gis%phy_f3d,  gis%phy_f2d,                            &
                 gis%dyn_f3d,  gis%dyn_f2d,                            &
                 gis%ZHOUR,gis%N1,gis%N4,gis%LSOUT,gis%COLAT1,         &
                 gis%CFHOUR1,gis%fscav,gis%SPS,gis%nam_gfs%Total_Member)
 
        gis%PHOUR=FHOUR
! Advance the ESMF clock to the current integration time.
!--------------------------------------------------------
        DO k = 1, advanceCount4-advanceCounts
            CALL ESMF_ClockAdvance(clock, rc = rc1)
        END DO

      ENDIF  ! fin first forward step if needed
!!
!$$$      IF (me.eq.0) THEN
!$$$        write(*,*)'Fin step1'
!$$$        write(*,*)'*********'
!$$$        CALL bar3(trie_ls(1,1,P_ze),trio_ls(1,1,P_ze),'ze ',levs)
!$$$        CALL bar3(trie_ls(1,1,P_di),trio_ls(1,1,P_di),'di ',levs)
!$$$        CALL bar3(trie_ls(1,1,P_te),trio_ls(1,1,P_te),'te ',levs)
!$$$        CALL bar3(trie_ls(1,1,P_rq),trio_ls(1,1,P_rq),'rq ',levs)
!$$$        CALL bar3(trie_ls(1,1,P_rq+levs),trio_ls(1,1,P_rq+levs),
!$$$     &            'oz1 ',levs)
!$$$        CALL bar3(trie_ls(1,1,P_rq+2*levs),trio_ls(1,1,P_rq+2*levs),
!$$$     &            'oz2 ',levs)
!$$$        CALL bar3(trie_ls(1,1,P_q),trio_ls(1,1,P_q),'q  ',1)
!$$$      ENDIF
!
!     Initializations for the semi-implicit solver
!     ********************************************
!!
 if(hybrid)then
     call get_cd_hyb(timeStep_sec_r)
 else if( gen_coord_hybrid ) then                                       ! hmhj
     call get_cd_hyb_gc(timeStep_sec_r)                                 ! hmhj
 endif
!
 lssav = .true. !always true, except in digital filter
 lsfwd = .false. !true only during forward step

!
!***********************************************************************
!     TIME LOOP
!***********************************************************************
!
!!

!For testing the ESMF States.
!-------------------------

 CALL ESMF_ClockGet(clock, stopTime     = stopTime,      &
                           runDuration  = runDuration,   &
                           rc           = rc1)
!
!CALL ESMF_ClockGet(clock, advanceCount = advanceCount,  &
!                          stopTime     = stopTime,      &
!                          runDuration  = runDuration,   &
!                          rc           = rc1)
!advanceCount5 = advanceCount
!IF(advanceCount5 >= 84) THEN
!     CALL ESMF_TimeGet (stopTime,           dd = dd, rc = rc1)
!    CALL ESMF_TimeIntervalGet(runDuration, h  = hh, rc = rc1)
!     dd = dd + 2
!    hh = hh + 48
!     CALL ESMF_TimeSet (stopTIme,           dd = dd, rc = rc1)
!    CALL ESMF_TimeIntervalSet(runDuration, h  = hh, rc = rc1)
!    CALL ESMF_ClockSet(clock, runDuration = runDuration, rc = rc1)
!     CALL ESMF_ClockSet(clock, runDuration = runDuration, stopTime = stopTime, rc = rc1)
!END IF

 CALL ESMF_ClockGet(clock, currTime = currTime, rc = rc1)
 CALL ESMF_TimeGet (currTime, yy = yy, mm = mm, dd = dd, h = hh, m = mins, rc = rc1)
 if (me == 0) PRINT*, ' Before Main Do Loop, currTime=', yy, mm, dd, hh, mins

 advanceCount_StartPoint = advanceCount4

 Main_Time_Loop: DO WHILE(.NOT. (ESMF_ClockIsStopTime(clock,       rc = rc1)))

! Save the initial condition fields for ensemble forecast run.  Weiyu.
!---------------------------------------------------------------------
      IF(advanceCount4 - advanceCount_StartPoint == gis%advanceCount_SetUp) THEN
        IF(gis%ESMF_Sta_List%trieo_export .AND. (.NOT. LIOPE .OR. icolor /= 2)) THEN
          gis%trie_ls_ini = gis%trie_ls
          gis%trio_ls_ini = gis%trio_ls
        ENDIF
      ENDIF

      CALL ESMF_ClockAdvance(clock,                              rc = rc1)
      advanceCount4 = advanceCount4 + 1
      FHOUR     = REAL(advanceCount4) * timeStep_sec_r / 3600.0
      gis%LSOUT = MOD(advanceCount4 ,NSOUT).EQ.0
      if (nsout_hf > 0 .and. gis%phour <= fhmax_hf)                          &
        gis%lsout = MOD(advanceCount4 ,NSOUT_hf) == 0 .OR. gis%lsout

      LSCCA     = MOD(advanceCount4 ,NSSWR).EQ.0
      LSSWR     = MOD(advanceCount4 ,NSSWR).EQ.1
      LSLWR     = MOD(advanceCount4 ,NSLWR).EQ.1

      CALL do_tstep(timeStep_sec_r, advanceCount4, gis%PHOUR,          &
!JFE             LAM,DLAM,LAMEXT,LATLOCAL,PHI,DPHI,DPHIBR,PHIBS,       &
!JFE             LBASIY,LATSINPE,LAMMP,PHIMP,SIGMP,                    &
                 gis%TRIE_LS,gis%TRIO_LS,                              &
                 gis%LS_NODE,gis%LS_NODES,gis%MAX_LS_NODES,            &
                 gis%LATS_NODES_A,gis%GLOBAL_LATS_A,                   &
                 gis%LONSPERLAT,                                       &
                 gis%LATS_NODES_R,gis%GLOBAL_LATS_R,                   &
                 gis%LONSPERLAR,                                       &
!                gis%LATS_NODES_EXT,gis%GLOBAL_LATS_EXT,               &
                 gis%EPSE,gis%EPSO,gis%EPSEDN,gis%EPSODN,              &
                 gis%SNNP1EV,gis%SNNP1OD,gis%NDEXEV,gis%NDEXOD,        &
                 gis%PLNEV_A,gis%PLNOD_A,gis%PDDEV_A,gis%PDDOD_A,      &
                 gis%PLNEW_A,gis%PLNOW_A,                              &
                 gis%PLNEV_R,gis%PLNOD_R,gis%PDDEV_R,gis%PDDOD_R,      &
                 gis%PLNEW_R,gis%PLNOW_R,                              &
!                gis%SYN_GR_A_1,gis%DYN_GR_A_1,gis%ANL_GR_A_1,         &
!                gis%SYN_GR_A_2,gis%DYN_GR_A_2,gis%ANL_GR_A_2,         &
                 gis%XLON,gis%XLAT,gis%COSZDG,gis%sfc_fld,gis%flx_fld, &
                 gis%nst_fld,                                          &
                 gis%HPRIME,gis%SWH,gis%HLW,gis%FLUXR ,                &
                 gis%SFALB,gis%SLAG,gis%SDEC,gis%CDEC,                 &
                 gis%OZPLIN,gis%JINDX1,gis%JINDX2,                     &
                 gis%DDY,gis%PDRYINI,                                  &
                 gis%phy_f3d,  gis%phy_f2d,                            &
                 gis%dyn_f3d,  gis%dyn_f2d,                            &
                 gis%ZHOUR,gis%N1,gis%N4,gis%LSOUT,gis%COLAT1,         &
                 gis%CFHOUR1,gis%fscav,gis%SPS,gis%nam_gfs%Total_Member)
!!
      if (comp_task) then
        do k=1,levs
          if(spdmax(k).gt.0. .and. spdmax(k).lt.1000.) then
            continue
          else
            print *,'UNPHYSICAL MAXIMUM SPEED',spdmax(k),              &
                       ' me=',me
            call mpi_quit(7)
            stop
          endif
        enddo
      endif
!
        gis%PHOUR=FHOUR
 END DO Main_Time_Loop

!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
!
 IF(PRESENT(rc)) THEN
     rc = rc1
 END IF

 END SUBROUTINE GFS_Run

 END MODULE GFS_Run_ESMFMod
