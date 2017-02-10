      SUBROUTINE MDL2P(iostatusD3D)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    MDL2P       VERT INTRP OF MODEL LVLS TO PRESSURE
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-09-23       
!     
! ABSTRACT:
!     FOR MOST APPLICATIONS THIS ROUTINE IS THE WORKHORSE OF THE POST PROCESSOR.
!     IN A NUTSHELL IT INTERPOLATES DATA FROM MODEL TO PRESSURE SURFACES.
!     IT ORIGINATED FROM THE VERTICAL INTERPOLATION CODE IN THE OLD ETA
!     POST PROCESSOR SUBROUTINE OUTMAP AND IS A REVISION OF SUBROUTINE ETA2P.
!     
! PROGRAM HISTORY LOG:
!   99-09-23  T BLACK       - REWRITTEN FROM ETA2P
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-12  MIKE BALDWIN - WRF VERSION
!   02-07-29  H CHUANG - ADD UNDERGROUND FIELDS AND MEMBRANE SLP FOR WRF
!   04-11-24  H CHUANG - ADD FERRIER'S HYDROMETEOR FIELD
!   05-07-07  B ZHOU   - ADD RSM MODEL for SLP  
!   05--8-30  B ZHOU   - ADD AVIATION PRODUCTS: ICING, CAT, LLWS COMPUTATION
!   08-01-01  H CHUANG - ADD GFS D3D FIELDS TO VERTICAL INTERPOLATION
!   10-07-01  SMIRNOVA AND HU - ADD RR CHANGES
!   10-12-30  H CHUANG - ADD HAINES INDEX TO SUPPORT FIRE WEATHER
!   11-02-06  J Wang   - ADD grib2 option TO SUPPORT FIRE WEATHER
!   12-01-11  S LU     - ADD GOCART AEROSOLS
!   13-08-01  S Moorthi - some optimization
!   14-02-26  S Moorthi - threading datapd assignment
!
! USAGE:    CALL MDL2P
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST: 
!     NONE       
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       SCLFLD   - SCALE ARRAY ELEMENTS BY CONSTANT.
!       CALPOT   - COMPUTE POTENTIAL TEMPERATURE.
!       CALRH    - COMPUTE RELATIVE HUMIDITY.
!       CALDWP   - COMPUTE DEWPOINT TEMPERATURE.
!       BOUND    - BOUND ARRAY ELEMENTS BETWEEN LOWER AND UPPER LIMITS.
!       CALMCVG  - COMPUTE MOISTURE CONVERGENCE.
!       CALVOR   - COMPUTE ABSOLUTE VORTICITY.
!       CALSTRM  - COMPUTE GEOSTROPHIC STREAMFUNCTION.
!
!     LIBRARY:
!       COMMON   - CTLBLK
!                  RQSTFLD
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN 90
!     MACHINE : IBM SP
!$$$  
!
!
      use vrbls4d, only: DUST
      use vrbls3d, only: PINT, O3, PMID, T, Q, UH, VH, WH, OMGA, Q2, CWM,      &
                         QQW, QQI, QQR, QQS, QQG, DBZ, F_RIMEF, TTND, CFR,     &
                         ICING_GFIP, RLWTT, RSWTT, VDIFFTT, TCUCN, TCUCNS,     &
                         TRAIN, VDIFFMOIS, DCONVMOIS, SCONVMOIS,NRADTT,        &
                         O3VDIFF, O3PROD, O3TNDY, MWPV, UNKNOWN, VDIFFZACCE,   &
                         ZGDRAG, CNVCTVMMIXING, VDIFFMACCE, MGDRAG,            &
                         CNVCTUMMIXING, NCNVCTCFRAC, CNVCTUMFLX, CNVCTDETMFLX, &
                         CNVCTZGDRAG, CNVCTMGDRAG, ZMID, ZINT, PMIDV,          &
                         CNVCTDMFLX, ICING_GFIS,GTG
      use vrbls2d, only: T500, W_UP_MAX, W_DN_MAX, W_MEAN, PSLP, FIS, Z1000
      use masks,   only: LMH, SM
      use physcons,only: CON_FVIRT, CON_ROG, CON_EPS, CON_EPSM1
      use params_mod, only: H1M12, DBZMIN, H1, PQ0, A2, A3, A4, RHMIN, G,      &
                            RGAMOG, RD, D608, GI, ERAD, PI, SMALL, H100,       &
                            H99999, GAMMA
      use ctlblk_mod, only: MODELNAME, LP1, ME, JSTA, JEND, LM, SPVAL, SPL,    &
                            ALSL, JEND_M, SMFLAG, GRIB, CFLD, FLD_INFO, DATAPD,&
                            TD3D, IFHR, IFMIN, IM, JM, NBIN_DU, JSTA_2L,       &
                            JEND_2U, LSM, d3d_on, gocart_on, ioform
      use rqstfld_mod, only: IGET, LVLS, ID, IAVBLFLD, LVLSXML
      use gridspec_mod, only: GRIDTYPE, MAPTYPE, DXVAL
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
      implicit none
!     
!     INCLUDE MODEL DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
!     GAMMA AND RGAMOG ARE USED IN THE EXTRAPOLATION OF VIRTUAL
!     TEMPERATURES BEYOND THE UPPER OF LOWER LIMITS OF DATA.
!     
!
      real,parameter:: gammam=-1*GAMMA,zshul=75.,tvshul=290.66
!     
!     DECLARE VARIABLES.
!     
      real,PARAMETER :: CAPA=0.28589641,P1000=1000.E2
      LOGICAL IOOMG,IOALL
      real, dimension(im,jm) :: GRID1, GRID2
      real, dimension(im,jsta_2l:jend_2u) :: FSL, TSL, QSL, OSL,  USL, VSL     &
     &,                                      Q2SL,  WSL,   CFRSL, O3SL, TDSL   &
     &,                                      EGRID1,  EGRID2                   &
     &,                                      FSL_OLD, USL_OLD, VSL_OLD         &
     &,                                      OSL_OLD, OSL995                   &
     &,                                      ICINGFSL, ICINGVSL,GTGSL
!     REAL D3DSL(IM,JM,27),DUSTSL(IM,JM,NBIN_DU)
      REAL, allocatable  ::  D3DSL(:,:,:), DUSTSL(:,:,:)
!
      integer,intent(in) :: iostatusD3D
      INTEGER, dimension(im,jsta_2l:jend_2u)  :: NL1X, NL1XF
      real, dimension(IM,JSTA_2L:JEND_2U,LSM) :: TPRS, QPRS, FPRS
!
      INTEGER K, NSMOOTH
!
!--- Definition of the following 2D (horizontal) dummy variables
!
!  C1D   - total condensate
!  QW1   - cloud water mixing ratio
!  QI1   - cloud ice mixing ratio
!  QR1   - rain mixing ratio
!  QS1   - snow mixing ratio
!  QG1   - graupel mixing ratio
!  DBZ1  - radar reflectivity
!
      REAL, dimension(im,jsta_2l:jend_2u) :: C1D, QW1, QI1, QR1, QS1, QG1, DBZ1 &
      ,                                      FRIME, RAD, HAINES

      REAL SDUMMY(IM,2)

!  SAVE RH, U,V, for Icing, CAT, LLWS computation
      REAL SAVRH(IM,jsta:jend)
!jw
      integer I,J,L,LP,LL,LLMH,JJB,JJE,II,JJ,LI,IFINCR,ITD3D,ista,imois,luhi,la
      real fact,ALPSL,PSFC,QBLO,PNL1,TBLO,TVRL,TVRBLO,FAC,PSLPIJ,            &
           ALPTH,AHF,PDV,QL,TVU,TVD,GAMMAS,QSAT,RHL,ZL,TL,PL,ES,part,dum1
      real,external :: fpvsnew
      logical log1
      real dxm, tem, zero
!     
!******************************************************************************
!
!     START MDL2P. 
!
      if (modelname == 'GFS') then
        zero = 0.0
       else
        zero = h1m12
       endif
      if (d3d_on) then
        if (.not. allocated(d3dsl)) allocate(d3dsl(im,jm,27))
!$omp parallel do private(i,j,l)
        do l=1,27
          do j=1,jm
            do i=1,im
              D3DSL(i,j,l)  = SPVAL
            enddo
          enddo
        enddo
      endif
      if (gocart_on) then
        if (.not. allocated(dustsl)) allocate(dustsl(im,jm,nbin_du))
!$omp parallel do private(i,j,l)
        do l=1,nbin_du
          do j=1,jm
            do i=1,im
               DUSTSL(i,j,l)  = SPVAL
            enddo
          enddo
        enddo
      endif
!     
!     SET TOTAL NUMBER OF POINTS ON OUTPUT GRID.
!
!---------------------------------------------------------------
!
!     *** PART I ***
!
!     VERTICAL INTERPOLATION OF EVERYTHING ELSE.  EXECUTE ONLY
!     IF THERE'S SOMETHING WE WANT.
!
      IF((IGET(012) > 0) .OR. (IGET(013) > 0) .OR.      &
         (IGET(014) > 0) .OR. (IGET(015) > 0) .OR.      &
         (IGET(016) > 0) .OR. (IGET(017) > 0) .OR.      &
         (IGET(018) > 0) .OR. (IGET(019) > 0) .OR.      &
         (IGET(020) > 0) .OR. (IGET(030) > 0) .OR.      &
         (IGET(021) > 0) .OR. (IGET(022) > 0) .OR.      &
         (IGET(023) > 0) .OR. (IGET(085) > 0) .OR.      &
         (IGET(086) > 0) .OR. (IGET(284) > 0) .OR.      &
         (IGET(153) > 0) .OR. (IGET(166) > 0) .OR.      &
         (IGET(183) > 0) .OR. (IGET(184) > 0) .OR.      &
         (IGET(198) > 0) .OR. (IGET(251) > 0) .OR.      &
         (IGET(257) > 0) .OR. (IGET(258) > 0) .OR.      &
         (IGET(294) > 0) .OR. (IGET(268) > 0) .OR.      &
         (IGET(331) > 0) .OR. (IGET(326) > 0) .OR.      &
! add D3D fields
         (IGET(354) > 0) .OR. (IGET(355) > 0) .OR.      &
         (IGET(356) > 0) .OR. (IGET(357) > 0) .OR.      &
         (IGET(358) > 0) .OR. (IGET(359) > 0) .OR.      &
         (IGET(360) > 0) .OR. (IGET(361) > 0) .OR.      &
         (IGET(362) > 0) .OR. (IGET(363) > 0) .OR.      &
         (IGET(364) > 0) .OR. (IGET(365) > 0) .OR.      &
         (IGET(366) > 0) .OR. (IGET(367) > 0) .OR.      &
         (IGET(368) > 0) .OR. (IGET(369) > 0) .OR.      &
         (IGET(370) > 0) .OR. (IGET(371) > 0) .OR.      &
         (IGET(372) > 0) .OR. (IGET(373) > 0) .OR.      &
         (IGET(374) > 0) .OR. (IGET(375) > 0) .OR.      &
         (IGET(391) > 0) .OR. (IGET(392) > 0) .OR.      &
         (IGET(393) > 0) .OR. (IGET(394) > 0) .OR.      &
         (IGET(395) > 0) .OR. (IGET(379) > 0) .OR.      &
! ADD DUST FIELDS
         (IGET(438) > 0) .OR. (IGET(439) > 0) .OR.      &
         (IGET(440) > 0) .OR. (IGET(441) > 0) .OR.      &
         (IGET(442) > 0) .OR. (IGET(455) > 0) .OR.      &
! NCAR ICING
         (IGET(450) > 0) .OR. (MODELNAME == 'RAPR') .OR.&
         (IGET(480) > 0) .OR. (MODELNAME == 'RAPR') .OR.&
! NCAR GTG turbulence
         (IGET(464) > 0) .OR.                           &
! LIFTED INDEX needs 500 mb T
         (IGET(030)>0) .OR. (IGET(031)>0) .OR. (IGET(075)>0)) THEN
!
!---------------------------------------------------------------------
!***
!***  BECAUSE SIGMA LAYERS DO NOT GO UNDERGROUND,  DO ALL
!***  INTERPOLATION ABOVE GROUND NOW.
!***
!
!       print*,'LSM= ',lsm

        if(gridtype == 'B' .or. gridtype == 'E')                         &
          call exch(PINT(1:IM,JSTA_2L:JEND_2U,LP1)) 
 
        DO LP=1,LSM

!         if(me == 0) print *,'in LP loop me=',me,'UH=',UH(1:10,JSTA,LP), &
!          'JSTA_2L=',JSTA_2L,'JEND_2U=',JEND_2U,'JSTA=',JSTA,JEND, &
!          'PMID(1,1,L)=',(PMID(1,1,LI),LI=1,LM),'SPL(LP)=',SPL(LP)

!         if(me ==0) print *,'in mdl2p,LP loop o3=',maxval(o3(1:im,jsta:jend,lm))
!
!$omp parallel do private(i,j,l)
          DO J=JSTA_2L,JEND_2U
            DO I=1,IM
              TSL(I,J)      = SPVAL
              QSL(I,J)      = SPVAL
              FSL(I,J)      = SPVAL
              OSL(I,J)      = SPVAL
              USL(I,J)      = SPVAL
              VSL(I,J)      = SPVAL
              Q2SL(I,J)     = SPVAL
              C1D(I,J)      = SPVAL      ! Total condensate
              QW1(I,J)      = SPVAL      ! Cloud water
              QI1(I,J)      = SPVAL      ! Cloud ice
              QR1(I,J)      = SPVAL      ! Rain 
              QS1(I,J)      = SPVAL      ! Snow (precip ice) 
              QG1(I,J)      = SPVAL      ! Graupel
              DBZ1(I,J)     = SPVAL
              FRIME(I,J)    = SPVAL
              RAD(I,J)      = SPVAL
              O3SL(I,J)     = SPVAL
              CFRSL(I,J)    = SPVAL
              ICINGFSL(I,J) = SPVAL
              ICINGVSL(I,J) = SPVAL
              GTGSL(I,J)    = SPVAL
!
!***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER JUST BELOW
!***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
!
              NL1X(I,J) = LP1
              DO L=2,LM
                IF(NL1X(I,J) == LP1 .AND. PMID(I,J,L) > SPL(LP)) THEN
                  NL1X(I,J) = L
                ENDIF
              ENDDO
!
!  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
!  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
!  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
!  WILL EXTRAPOLATE TO THAT POINT
!
              IF(NL1X(I,J) == LP1 .AND. PINT(I,J,LP1) > SPL(LP)) THEN
                NL1X(I,J) = LM
              ENDIF

              NL1XF(I,J) = LP1 + 1
              DO L=2,LP1
                IF(NL1XF(I,J) == (LP1+1) .AND. PINT(I,J,L) > SPL(LP)) THEN
                  NL1XF(I,J) = L
                ENDIF
              ENDDO
!             if(NL1X(I,J) == LMP1)print*,'Debug: NL1X=LMP1 AT ' 1 ,i,j,lp
            ENDDO
          ENDDO             ! end of j loop

!
!mptest      IF(NHOLD == 0)GO TO 310
!
!!$omp  parallel do
!!$omp& private(nn,i,j,ll,fact,qsat,rhl)
!hc      DO 220 NN=1,NHOLD
!hc        I=IHOLD(NN)
!hc         J=JHOLD(NN)
!           DO 220 J=JSTA,JEND

          ii = im/2
          jj = (jsta+jend)/2

!$omp  parallel do private(i,j,k,l,ll,llmh,la,tvd,tvu,fact,fac,ahf,rhl,tl,pl,ql,zl,es,qsat,part,tvrl,tvrblo,tblo,qblo,gammas,pnl1)
          DO J=JSTA,JEND
            DO I=1,IM
!---------------------------------------------------------------------
!***  VERTICAL INTERPOLATION OF GEOPOTENTIAL, TEMPERATURE, SPECIFIC
!***  HUMIDITY, CLOUD WATER/ICE, OMEGA, WINDS, AND TKE.
!---------------------------------------------------------------------
!
               LL   = NL1X(I,J)
               LLMH = NINT(LMH(I,J))

!HC            IF(NL1X(I,J).LE.LM)THEN        

               IF(SPL(LP) < PINT(I,J,2)) THEN ! Above second interface
                 IF(T(I,J,1) < SPVAL)   TSL(I,J) = T(I,J,1)
                 IF(Q(I,J,1) < SPVAL)   QSL(I,J) = Q(I,J,1)

                 IF(gridtype == 'A')THEN
                   USL(I,J) = UH(I,J,1)
                   VSL(I,J) = VH(I,J,1)
                 END IF

!                if ( J == JSTA.and. I == 1.and.me == 0)    &
!                print *,'1 USL=',USL(I,J),UH(I,J,1)
 
                 IF(WH(I,J,1)      < SPVAL) WSL(I,J)   = WH(I,J,1)  
                 IF(OMGA(I,J,1)    < SPVAL) OSL(I,J)   = OMGA(I,J,1)
                 IF(Q2(I,J,1)      < SPVAL) Q2SL(I,J)  = Q2(I,J,1)
                 IF(CWM(I,J,1)     < SPVAL) C1D(I,J)   = CWM(I,J,1)
                 C1D(I,J) = MAX(C1D(I,J),zero)              ! Total condensate
                 IF(QQW(I,J,1)     < SPVAL) QW1(I,J)   = QQW(I,J,1)
                 QW1(I,J) = MAX(QW1(I,J),zero)              ! Cloud water
                 IF(QQI(I,J,1)     < SPVAL) QI1(I,J)   = QQI(I,J,1)
                 QI1(I,J) = MAX(QI1(I,J),zero)              ! Cloud ice
                 IF(QQR(I,J,1)     < SPVAL) QR1(I,J)   = QQR(I,J,1)
                 QR1(I,J) = MAX(QR1(I,J),zero)              ! Rain 
                 IF(QQS(I,J,1)     < SPVAL) QS1(I,J)   = QQS(I,J,1)
                 QS1(I,J) = MAX(QS1(I,J),zero)              ! Snow (precip ice) 
                 IF(QQG(I,J,1)     < SPVAL) QG1(I,J)   = QQG(I,J,1)
                 QG1(I,J) = MAX(QG1(I,J),zero)              ! Graupel (precip ice) 
                 IF(DBZ(I,J,1)     < SPVAL) DBZ1(I,J)  = DBZ(I,J,1)
                 DBZ1(I,J) = MAX(DBZ1(I,J),DBZmin)
                 IF(F_RimeF(I,J,1) < SPVAL) FRIME(I,J) = F_RimeF(I,J,1)
                 FRIME(I,J) = MAX(FRIME(I,J),H1)
                 IF(TTND(I,J,1)    < SPVAL) RAD(I,J)   = TTND(I,J,1)
                 IF(O3(I,J,1)      < SPVAL) O3SL(I,J)  = O3(I,J,1)
                 IF(CFR(I,J,1)     < SPVAL) CFRSL(I,J) = CFR(I,J,1)
!GFIP
                 IF(ICING_GFIP(I,J,1) < SPVAL) ICINGFSL(I,J) = ICING_GFIP(I,J,1) 
                 IF(ICING_GFIS(I,J,1) < SPVAL) ICINGVSL(I,J) = ICING_GFIS(I,J,1)
!GTG
                 IF(GTG(I,J,1) < SPVAL) GTGSL(I,J) = GTG(I,J,1)
! DUST
                 if (gocart_on) then
                   DO K = 1, NBIN_DU
                     IF(DUST(I,J,1,K) < SPVAL) DUSTSL(I,J,K) = DUST(I,J,1,K)
                   ENDDO
                 endif

! only interpolate GFS d3d fields when  reqested
!          if(iostatusD3D ==0 .and. d3d_on)then
                 if (d3d_on) then
                   IF((IGET(354) > 0) .OR. (IGET(355) > 0) .OR.       &
                      (IGET(356) > 0) .OR. (IGET(357) > 0) .OR.       &
                      (IGET(358) > 0) .OR. (IGET(359) > 0) .OR.       &
                      (IGET(360) > 0) .OR. (IGET(361) > 0) .OR.       &
                      (IGET(362) > 0) .OR. (IGET(363) > 0) .OR.       &
                      (IGET(364) > 0) .OR. (IGET(365) > 0) .OR.       &
                      (IGET(366) > 0) .OR. (IGET(367) > 0) .OR.       &
                      (IGET(368) > 0) .OR. (IGET(369) > 0) .OR.       &
                      (IGET(370) > 0) .OR. (IGET(371) > 0) .OR.       &
                      (IGET(372) > 0) .OR. (IGET(373) > 0) .OR.       &
                      (IGET(374) > 0) .OR. (IGET(375) > 0) .OR.       &
                      (IGET(391) > 0) .OR. (IGET(392) > 0) .OR.       &
                      (IGET(393) > 0) .OR. (IGET(394) > 0) .OR.       &
                      (IGET(395) > 0) .OR. (IGET(379) > 0)) THEN
                      D3DSL(i,j,1)  = rlwtt(I,J,1)
                      D3DSL(i,j,2)  = rswtt(I,J,1)
                      D3DSL(i,j,3)  = vdifftt(I,J,1)
                      D3DSL(i,j,4)  = tcucn(I,J,1)
                      D3DSL(i,j,5)  = tcucns(I,J,1)
                      D3DSL(i,j,6)  = train(I,J,1)
                      D3DSL(i,j,7)  = vdiffmois(I,J,1)
                      D3DSL(i,j,8)  = dconvmois(I,J,1)
                      D3DSL(i,j,9)  = sconvmois(I,J,1)
                      D3DSL(i,j,10) = nradtt(I,J,1)
                      D3DSL(i,j,11) = o3vdiff(I,J,1)
                      D3DSL(i,j,12) = o3prod(I,J,1)
                      D3DSL(i,j,13) = o3tndy(I,J,1)
                      D3DSL(i,j,14) = mwpv(I,J,1)
                      D3DSL(i,j,15) = unknown(I,J,1)
                      D3DSL(i,j,16) = vdiffzacce(I,J,1)
                      D3DSL(i,j,17) = zgdrag(I,J,1)
                      D3DSL(i,j,18) = cnvctummixing(I,J,1)
                      D3DSL(i,j,19) = vdiffmacce(I,J,1)
                      D3DSL(i,j,20) = mgdrag(I,J,1)
                      D3DSL(i,j,21) = cnvctvmmixing(I,J,1)
                      D3DSL(i,j,22) = ncnvctcfrac(I,J,1)
                      D3DSL(i,j,23) = cnvctumflx(I,J,1)
                      D3DSL(i,j,24) = cnvctdmflx(I,J,1)
                      D3DSL(i,j,25) = cnvctdetmflx(I,J,1)
                      D3DSL(i,j,26) = cnvctzgdrag(I,J,1)
                      D3DSL(i,j,27) = cnvctmgdrag(I,J,1)
                   end if
                 end if

               ELSE IF(LL <= LLMH)THEN
!
!---------------------------------------------------------------------
!          INTERPOLATE LINEARLY IN LOG(P)
!***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
!***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
!***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
!---------------------------------------------------------------------
!
!KRF: Need ncar and nmm wrf core checks as well?
                 IF (MODELNAME == 'RAPR' .OR. MODELNAME == 'NCAR' .OR. MODELNAME == 'NMM') THEN
                   FACT = (ALSL(LP)-LOG(PMID(I,J,LL)))/                   &
                          max(1.e-6,(LOG(PMID(I,J,LL))-LOG(PMID(I,J,LL-1))))
                   FACT = max(-10.0,min(FACT, 10.0))
                 ELSE
                   FACT = (ALSL(LP)-LOG(PMID(I,J,LL)))/                   &
                          (LOG(PMID(I,J,LL))-LOG(PMID(I,J,LL-1)))
                 ENDIF
                 IF(T(I,J,LL) < SPVAL .AND. T(I,J,LL-1) < SPVAL)          &
                     TSL(I,J) = T(I,J,LL)+(T(I,J,LL)-T(I,J,LL-1))*FACT
                 IF(Q(I,J,LL) < SPVAL .AND. Q(I,J,LL-1) < SPVAL)          &
                     QSL(I,J) = Q(I,J,LL)+(Q(I,J,LL)-Q(I,J,LL-1))*FACT

                 IF(gridtype=='A')THEN
                   IF(UH(I,J,LL) < SPVAL .AND. UH(I,J,LL-1) < SPVAL)       &
                     USL(I,J) = UH(I,J,LL)+(UH(I,J,LL)-UH(I,J,LL-1))*FACT
                   IF(VH(I,J,LL) < SPVAL .AND. VH(I,J,LL-1) < SPVAL)       &
                     VSL(I,J) = VH(I,J,LL)+(VH(I,J,LL)-VH(I,J,LL-1))*FACT
                 END IF 

                 IF(WH(I,J,LL) < SPVAL .AND. WH(I,J,LL-1) < SPVAL)        &
                   WSL(I,J) = WH(I,J,LL)+(WH(I,J,LL)-WH(I,J,LL-1))*FACT
                 IF(OMGA(I,J,LL) < SPVAL .AND. OMGA(I,J,LL-1) < SPVAL)    &
                   OSL(I,J) = OMGA(I,J,LL)+(OMGA(I,J,LL)-OMGA(I,J,LL-1))*FACT
                 IF(Q2(I,J,LL) < SPVAL .AND. Q2(I,J,LL-1) < SPVAL)        &
                   Q2SL(I,J) = Q2(I,J,LL)+(Q2(I,J,LL)-Q2(I,J,LL-1))*FACT

!                IF(ZMID(I,J,LL) < SPVAL .AND. ZMID(I,J,LL-1) < SPVAL)   &
!     &             FSL(I,J) = ZMID(I,J,LL)+(ZMID(I,J,LL)-ZMID(I,J,LL-1))*FACT
!                   FSL(I,J) = FSL(I,J)*G

                 if (modelname == 'GFS') then
                   ES   = min(FPVSNEW(TSL(I,J)), SPL(LP))
                   QSAT = CON_EPS*ES/(SPL(LP)+CON_EPSM1*ES)
                 else
                   QSAT = PQ0/SPL(LP)*EXP(A2*(TSL(I,J)-A3)/(TSL(I,J)-A4))
                 endif
!
                 RHL      = max(RHmin, min(1.0, QSL(I,J)/QSAT))
                 QSL(I,J) = RHL*QSAT

!                if(tsl(i,j) > 330. .or. tsl(i,j) < 100.)print*,             &
!                 'bad isobaric T Q',i,j,lp,tsl(i,j),qsl(i,j)                &
!                 ,T(I,J,LL),T(I,J,LL-1),Q(I,J,LL),Q(I,J,LL-1)

                 IF(Q2SL(I,J) < 0.0) Q2SL(I,J) = 0.0
!	  
!HC ADD FERRIER'S HYDROMETEOR
                 IF(CWM(I,J,LL) < SPVAL .AND. CWM(I,J,LL-1) < SPVAL)         &
                   C1D(I,J) = CWM(I,J,LL) + (CWM(I,J,LL)-CWM(I,J,LL-1))*FACT
                   C1D(I,J) = MAX(C1D(I,J),zero)      ! Total condensate

                 IF(QQW(I,J,LL) < SPVAL .AND. QQW(I,J,LL-1) < SPVAL)         &
                   QW1(I,J) = QQW(I,J,LL) + (QQW(I,J,LL)-QQW(I,J,LL-1))*FACT
                   QW1(I,J) = MAX(QW1(I,J),zero)      ! Cloud water

                 IF(QQI(I,J,LL) < SPVAL .AND. QQI(I,J,LL-1) < SPVAL)         &
                   QI1(I,J) = QQI(I,J,LL) + (QQI(I,J,LL)-QQI(I,J,LL-1))*FACT
                   QI1(I,J) = MAX(QI1(I,J),zero)      ! Cloud ice

                 IF(QQR(I,J,LL) < SPVAL .AND. QQR(I,J,LL-1) < SPVAL)         &
                   QR1(I,J) = QQR(I,J,LL) + (QQR(I,J,LL)-QQR(I,J,LL-1))*FACT
                   QR1(I,J) = MAX(QR1(I,J),zero)      ! Rain 

                 IF(QQS(I,J,LL) < SPVAL .AND. QQS(I,J,LL-1) < SPVAL)         &
                   QS1(I,J) = QQS(I,J,LL) + (QQS(I,J,LL)-QQS(I,J,LL-1))*FACT
                   QS1(I,J) = MAX(QS1(I,J),zero)      ! Snow (precip ice) 

                 IF(QQG(I,J,LL) < SPVAL .AND. QQG(I,J,LL-1) < SPVAL)         &
                   QG1(I,J) = QQG(I,J,LL) + (QQG(I,J,LL)-QQG(I,J,LL-1))*FACT
                   QG1(I,J) = MAX(QG1(I,J),zero)      ! GRAUPEL (precip ice) 

                 IF(DBZ(I,J,LL) < SPVAL .AND. DBZ(I,J,LL-1) < SPVAL)         &
                   DBZ1(I,J) = DBZ(I,J,LL) + (DBZ(I,J,LL)-DBZ(I,J,LL-1))*FACT
                   DBZ1(I,J) = MAX(DBZ1(I,J),DBZmin)

                 IF(F_RimeF(I,J,LL) < SPVAL .AND. F_RimeF(I,J,LL-1) < SPVAL) &
                   FRIME(I,J) = F_RimeF(I,J,LL) + (F_RimeF(I,J,LL) - F_RimeF(I,J,LL-1))*FACT
                   FRIME(I,J)=MAX(FRIME(I,J),H1)

                 IF(TTND(I,J,LL) < SPVAL .AND. TTND(I,J,LL-1) < SPVAL)        &
                   RAD(I,J) = TTND(I,J,LL) + (TTND(I,J,LL)-TTND(I,J,LL-1))*FACT

                 IF(O3(I,J,LL) < SPVAL .AND. O3(I,J,LL-1) < SPVAL)            &
                   O3SL(I,J) = O3(I,J,LL) + (O3(I,J,LL)-O3(I,J,LL-1))*FACT

                 IF(CFR(I,J,LL) < SPVAL .AND. CFR(I,J,LL-1) < SPVAL)          &
                   CFRSL(I,J) = CFR(I,J,LL) + (CFR(I,J,LL)-CFR(I,J,LL-1))*FACT 
!GFIP
                 IF(ICING_GFIP(I,J,LL) < SPVAL .AND. ICING_GFIP(I,J,LL-1) < SPVAL)          &
                   ICINGFSL(I,J) = ICING_GFIP(I,J,LL) + (ICING_GFIP(I,J,LL)-ICING_GFIP(I,J,LL-1))*FACT
                   ICINGFSL(I,J) = max(0.0, ICINGFSL(I,J))
                   ICINGFSL(I,J) = min(1.0, ICINGFSL(I,J))
                 IF(ICING_GFIS(I,J,LL) < SPVAL .AND.  ICING_GFIS(I,J,LL-1) < SPVAL)          &
                   ICINGVSL(I,J) = ICING_GFIS(I,J,LL) + (ICING_GFIS(I,J,LL)-ICING_GFIS(I,J,LL-1))*FACT
!                    Icing severity categories
!                    0 = none (0, 0.08)
!                    4 = trace [0.08, 0.21]
!                    1 = light (0.21, 0.37]
!                    2 = moderate (0.37, 0.67]
!                    3 (no value yet, July 2015)
!                    5 = heavy (0.67, 1]
!                   http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_table4-207.shtml

                   if (ICINGVSL(I,J) < 0.08) then
                      ICINGVSL(I,J) = 0.0
                   elseif (ICINGVSL(I,J) <= 0.21) then
                      ICINGVSL(I,J) = 4.
                   else if(ICINGVSL(I,J) <= 0.37) then
                      ICINGVSL(I,J) = 1.0
                   else if(ICINGVSL(I,J) <= 0.67) then
                      ICINGVSL(I,J) = 2.0
                   else
                      ICINGVSL(I,J) = 5.0
                   endif
                   if(ICINGFSL(I,J)< 0.001) ICINGVSL(I,J) = 0.

! GTG
                 IF(GTG(I,J,LL) < SPVAL .AND. GTG(I,J,LL-1) < SPVAL)          &
                   GTGSL(I,J) = GTG(I,J,LL) + (GTG(I,J,LL)-GTG(I,J,LL-1))*FACT 
! DUST
                 if (gocart_on) then
                   DO K = 1, NBIN_DU
                     IF(DUST(I,J,LL,K) < SPVAL .AND. DUST(I,J,LL-1,K) < SPVAL)   &
                     DUSTSL(I,J,K) = DUST(I,J,LL,K) + (DUST(I,J,LL,K)-DUST(I,J,LL-1,K))*FACT
                   ENDDO
                 endif

! only interpolate GFS d3d fields when  == ested
!          if(iostatusD3D==0)then
                 if (d3d_on) then
                 IF((IGET(354) > 0) .OR. (IGET(355) > 0) .OR.         &
                    (IGET(356) > 0) .OR. (IGET(357) > 0) .OR.         &
                    (IGET(358) > 0) .OR. (IGET(359) > 0) .OR.         &
                    (IGET(360) > 0) .OR. (IGET(361) > 0) .OR.         &
                    (IGET(362) > 0) .OR. (IGET(363) > 0) .OR.         &
                    (IGET(364) > 0) .OR. (IGET(365) > 0) .OR.         &
                    (IGET(366) > 0) .OR. (IGET(367) > 0) .OR.         &
                    (IGET(368) > 0) .OR. (IGET(369) > 0) .OR.         &
                    (IGET(370) > 0) .OR. (IGET(371) > 0) .OR.         &
                    (IGET(372) > 0) .OR. (IGET(373) > 0) .OR.         &
                    (IGET(374) > 0) .OR. (IGET(375) > 0) .OR.         &
                    (IGET(391) > 0) .OR. (IGET(392) > 0) .OR.         &
                    (IGET(393) > 0) .OR. (IGET(394) > 0) .OR.         &
                    (IGET(395) > 0) .OR. (IGET(379) > 0))THEN
                    D3DSL(i,j,1)  = rlwtt(I,J,LL)+(rlwtt(I,J,LL)       &
                                  - rlwtt(I,J,LL-1))*FACT
                    D3DSL(i,j,2)  = rswtt(I,J,LL)+(rswtt(I,J,LL)       &
                                  - rswtt(I,J,LL-1))*FACT
                    D3DSL(i,j,3)  = vdifftt(I,J,LL)+(vdifftt(I,J,LL)   &
                                  - vdifftt(I,J,LL-1))*FACT
                    D3DSL(i,j,4)  = tcucn(I,J,LL)+(tcucn(I,J,LL)       &
                                  - tcucn(I,J,LL-1))*FACT
                    D3DSL(i,j,5)  = tcucns(I,J,LL)+(tcucns(I,J,LL)     &
                                  - tcucns(I,J,LL-1))*FACT
                    D3DSL(i,j,6)  = train(I,J,LL)+(train(I,J,LL)       &
                                  - train(I,J,LL-1))*FACT
                    D3DSL(i,j,7)  = vdiffmois(I,J,LL)+                 &
                                   (vdiffmois(I,J,LL)-vdiffmois(I,J,LL-1))*FACT
                    D3DSL(i,j,8)  = dconvmois(I,J,LL)+                 &
                                   (dconvmois(I,J,LL)-dconvmois(I,J,LL-1))*FACT
                    D3DSL(i,j,9)  = sconvmois(I,J,LL)+                 &
                                   (sconvmois(I,J,LL)-sconvmois(I,J,LL-1))*FACT
                    D3DSL(i,j,10) = nradtt(I,J,LL)+                   &
                                   (nradtt(I,J,LL)-nradtt(I,J,LL-1))*FACT
                    D3DSL(i,j,11) = o3vdiff(I,J,LL)+                  &
                                   (o3vdiff(I,J,LL)-o3vdiff(I,J,LL-1))*FACT
                    D3DSL(i,j,12) = o3prod(I,J,LL)+                   &
                                   (o3prod(I,J,LL)-o3prod(I,J,LL-1))*FACT
                    D3DSL(i,j,13) = o3tndy(I,J,LL)+                   &
                                   (o3tndy(I,J,LL)-o3tndy(I,J,LL-1))*FACT
                    D3DSL(i,j,14) = mwpv(I,J,LL)+                     &
                                   (mwpv(I,J,LL)-mwpv(I,J,LL-1))*FACT
                    D3DSL(i,j,15) = unknown(I,J,LL)+                  &
                                   (unknown(I,J,LL)-unknown(I,J,LL-1))*FACT
                    D3DSL(i,j,16) = vdiffzacce(I,J,LL)+               &
                                   (vdiffzacce(I,J,LL)-vdiffzacce(I,J,LL-1))*FACT
                    D3DSL(i,j,17) = zgdrag(I,J,LL)+                   &
                                   (zgdrag(I,J,LL)-zgdrag(I,J,LL-1))*FACT
                    D3DSL(i,j,18) = cnvctummixing(I,J,LL)+            &
                                   (cnvctummixing(I,J,LL)-cnvctummixing(I,J,LL-1))*FACT
                    D3DSL(i,j,19) = vdiffmacce(I,J,LL)+               &
                                   (vdiffmacce(I,J,LL)-vdiffmacce(I,J,LL-1))*FACT
                    D3DSL(i,j,20) = mgdrag(I,J,LL)+                   &
                                   (mgdrag(I,J,LL)-mgdrag(I,J,LL-1))*FACT
                    D3DSL(i,j,21) = cnvctvmmixing(I,J,LL)+            &
                                   (cnvctvmmixing(I,J,LL)-cnvctvmmixing(I,J,LL-1))*FACT
                    D3DSL(i,j,22) = ncnvctcfrac(I,J,LL)+              &
                                   (ncnvctcfrac(I,J,LL)-ncnvctcfrac(I,J,LL-1))*FACT
                    D3DSL(i,j,23) = cnvctumflx(I,J,LL)+               &
                                   (cnvctumflx(I,J,LL)-cnvctumflx(I,J,LL-1))*FACT    
                    D3DSL(i,j,24) = cnvctdmflx(I,J,LL)+               &
                                   (cnvctdmflx(I,J,LL)-cnvctdmflx(I,J,LL-1))*FACT
                    D3DSL(i,j,25) = cnvctdetmflx(I,J,LL)+             &
                                   (cnvctdetmflx(I,J,LL)-cnvctdetmflx(I,J,LL-1))*FACT
                    D3DSL(i,j,26) = cnvctzgdrag(I,J,LL)+              &
                                   (cnvctzgdrag(I,J,LL)-cnvctzgdrag(I,J,LL-1))*FACT
                    D3DSL(i,j,27) = cnvctmgdrag(I,J,LL)+              &
                                   (cnvctmgdrag(I,J,LL)-cnvctmgdrag(I,J,LL-1))*FACT    
                 end if
                 end if         ! if d3d_on test

! FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
! ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
! LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
! GOUND
               ELSE ! underground
                 IF(MODELNAME == 'GFS')THEN ! GFS deduce T and H using Shuell
                   tvu = T(I,J,LM) * (1.+con_fvirt*Q(I,J,LM))
                   if(ZMID(I,J,LM) > zshul) then
                     tvd = tvu + gamma*ZMID(I,J,LM)
                     if(tvd > tvshul) then
                       if(tvu > tvshul) then
                         tvd = tvshul - 5.e-3*(tvu-tvshul)*(tvu-tvshul)
                       else
                         tvd = tvshul
                       endif
                     endif
                     gammas = (tvu-tvd)/ZMID(I,J,LM)
                   else
                     gammas = 0.
                   endif
                   part     = con_rog*(ALSL(LP)-LOG(PMID(I,J,LM)))
                   FSL(I,J) = ZMID(I,J,LM) - tvu*part/(1.+0.5*gammas*part)
!                  tp(k)    = t(1)+gammas*(hp(k)-h(1))
                   TSL(I,J) = T(I,J,LM) - gamma*(FSL(I,J)-ZMID(I,J,LM))
                   FSL(I,J) = FSL(I,J)*G ! just use NAM G for now since FSL will be divided by GI later
!
! Compute RH at lowest model layer because Iredell and Chuang decided to compute
! underground GFS Q to maintain RH
                   ES   = min(FPVSNEW(T(I,J,LM)), PMID(I,J,LM))
                   QSAT = CON_EPS*ES/(PMID(I,J,LM)+CON_EPSM1*ES)
                   RHL  = Q(I,J,LM)/QSAT
! compute saturation water vapor at isobaric level
                   ES   = min(FPVSNEW(TSL(I,J)), SPL(LP))
                   QSAT = CON_EPS*ES/(SPL(LP)+CON_EPSM1*ES)
! Q at isobaric level is computed by maintaining constant RH	  
                   QSL(I,J) = RHL*QSAT
  
                 ELSE
                   PL = PINT(I,J,LM-1)
                   ZL = ZINT(I,J,LM-1)
                   TL = 0.5*(T(I,J,LM-2)+T(I,J,LM-1))
                   QL = 0.5*(Q(I,J,LM-2)+Q(I,J,LM-1))
!	           TMT0=TL-A0
!                  TMT15=MIN(TMT0,-15.)
!                  AI=0.008855
!                  BI=1.
!                  IF(TMT0 < -20.)THEN
!                    AI=0.007225
!                    BI=0.9674
!                  ENDIF

                   QSAT = PQ0/PL*EXP(A2*(TL-A3)/(TL-A4))
                   RHL  = QL/QSAT
!
                   IF(RHL > 1.)THEN
                     RHL = 1.
                     QL  = RHL*QSAT
                   ENDIF
!
                   IF(RHL < RHmin)THEN
                     RHL = RHmin
                     QL  = RHL*QSAT
                   ENDIF
!
                   TVRL   = TL*(1.+0.608*QL)
                   TVRBLO = TVRL*(SPL(LP)/PL)**RGAMOG
                   TBLO   = TVRBLO/(1.+0.608*QL)
!     
!                  TMT0=TBLO-A3
!                  TMT15=MIN(TMT0,-15.)
!                  AI=0.008855
!                  BI=1.
!                  IF(TMT0 < -20.)THEN
!                    AI=0.007225
!                    BI=0.9674
!                  ENDIF

                   QSAT     = PQ0/SPL(LP)*EXP(A2*(TBLO-A3)/(TBLO-A4))
                   TSL(I,J) = TBLO
                   QBLO     = RHL*QSAT
                   QSL(I,J) = MAX(1.E-12,QBLO)
                 END IF ! endif loop for deducing T and H differently for GFS  

!                if(tsl(i,j) > 330. .or. tsl(i,j) < 100.)print*,            &  
!                  'bad isobaric T Q',i,j,lp,tsl(i,j),qsl(i,j),tl,ql,pl

                 IF(gridtype == 'A')THEN
                   USL(I,J) = UH(I,J,LLMH)
                   VSL(I,J) = VH(I,J,LLMH)
                 END IF 
!         if ( J == JSTA.and. I == 1.and.me == 0)    &
!     &       print *,'3 USL=',USL(I,J),UH(I,J,LLMH),LLMH
                 WSL(I,J)  = WH(I,J,LLMH)
                 OSL(I,J)  = OMGA(I,J,LLMH)
                 Q2SL(I,J) = max(0.0,0.5*(Q2(I,J,LLMH-1)+Q2(I,J,LLMH)))
                 PNL1      = PINT(I,J,ll)
                 FAC       = 0.0
                 AHF       = 0.0

!                 FSL(I,J)=(PNL1-SPL(LP))/(SPL(LP)+PNL1)
!     1           *(TSL(I,J))*(1.+0.608*QSL(I,J))
!     2           *RD*2.+ZINT(I,J,NL1X(I,J))*G

!                 FSL(I,J)=FPRS(I,J,LP-1)-RD*(TPRS(I,J,LP-1)
!     1            *(H1+D608*QPRS(I,J,LP-1))
!     2            +TSL(I,J)*(H1+D608*QSL(I,J)))
!     3            *LOG(SPL(LP)/SPL(LP-1))/2.0

!                 if(abs(SPL(LP)-97500.0) < 0.01)then                               
!                 if(gdlat(i,j) > 35.0.and.gdlat(i,j).le.37.0 .and.           &
!                 gdlon(i,j) > -100.0 .and. gdlon(i,j) < -96.0)print*,        &
!                'Debug:I,J,FPRS(LP-1),TPRS(LP-1),TSL,SPL(LP),SPL(LP-1)='      &
!                ,i,j,FPRS(I,J,LP-1),TPRS(I,J,LP-1),TSL(I,J),SPL(LP)           &
!           ,SPL(LP-1)
!          if(gdlat(i,j) > 35.0.and.gdlat(i,j).le.37.0 .and.
!     1    gdlon(i,j) > -100.0 .and. gdlon(i,j) < -96.0)print*,
!     2    'Debug:I,J,PNL1,TSL,NL1X,ZINT,FSL= ',I,J,PNL1,TSL(I,J)
!     3    ,NL1X(I,J),ZINT(I,J,NL1X(I,J)),FSL(I,J)/G
!          end if
!          if(lp == lsm)print*,'Debug:undergound T,Q,U,V,FSL='
!     1,TSL(I,J),QSL(I,J),USL(I,J),VSL(I,J),FSL(I,J)
!
!--- Set hydrometeor fields to zero below ground
                 C1D(I,J)   = 0.
                 QW1(I,J)   = 0.
                 QI1(I,J)   = 0.
                 QR1(I,J)   = 0.
                 QS1(I,J)   = 0.
                 QG1(I,J)   = 0.
                 DBZ1(I,J)  = DBZmin
                 FRIME(I,J) = 1.
                 RAD(I,J)   = 0.
                 O3SL(I,J)  = O3(I,J,LLMH)
                 CFRSL(I,J) = 0.
               END IF
! Compute heights by interpolating from heights on interface for NAM but
! hydrostaticJ integration for GFS

               IF(MODELNAME == 'GFS') then
                 L=LL
                 IF(SPL(LP) < PMID(I,J,1)) THEN ! above model domain
                   tvd      = T(I,J,1)*(1+con_fvirt*Q(I,J,1))
                   FSL(I,J) = ZMID(I,J,1)-con_rog*tvd *(ALSL(LP)-LOG(PMID(I,J,1)))
                   FSL(I,J) = FSL(I,J)*G
                 ELSE IF(L <= LLMH)THEN 
                   tvd      = T(I,J,L)*(1+con_fvirt*Q(I,J,L))
                   tvu      = TSL(I,J)*(1+con_fvirt*QSL(I,J))
                   FSL(I,J) = ZMID(I,J,L)-con_rog*0.5*(tvd+tvu)               &
                            * (ALSL(LP)-LOG(PMID(I,J,L)))
                   FSL(I,J) = FSL(I,J)*G
                 END IF 
               ELSE
                 LA = NL1XF(I,J)
                 IF(NL1XF(I,J).LE.(LLMH+1)) THEN
                   FACT = (ALSL(LP)-LOG(PINT(I,J,LA)))/                       &
                          (LOG(PINT(I,J,LA))-LOG(PINT(I,J,LA-1)))
                   IF(ZINT(I,J,LA) < SPVAL .AND. ZINT(I,J,LA-1) < SPVAL)      &
                     FSL(I,J) = ZINT(I,J,LA)+(ZINT(I,J,LA)-ZINT(I,J,LA-1))*FACT
                   FSL(I,J) = FSL(I,J)*G 
                 ELSE
                   FSL(I,J) = FPRS(I,J,LP-1)-RD*(TPRS(I,J,LP-1)               &
                            * (H1+D608*QPRS(I,J,LP-1))                        &
                            + TSL(I,J)*(H1+D608*QSL(I,J)))                    &
                            * LOG(SPL(LP)/SPL(LP-1))/2.0 
                 END IF
               END IF  

            enddo        ! End of i loop
          enddo          ! End of J loop
  
!
!***  FILL THE 3-D-IN-PRESSURE ARRAYS FOR THE MEMBRANE SLP REDUCTION
!
!$omp  parallel do private(i,j)
          DO J=JSTA,JEND
            DO I=1,IM
              TPRS(I,J,LP) = TSL(I,J)
              QPRS(I,J,LP) = QSL(I,J)
              FPRS(I,J,LP) = FSL(I,J)
            ENDDO
          ENDDO
!	
! VERTICAL INTERPOLATION FOR WIND FOR E GRID
!
            IF(gridtype == 'E')THEN
              DO J=JSTA,JEND
                DO I=2,IM-MOD(J,2)
!                 IF(i == im/2 .and. j == (jsta+jend)/2)then
!                   do l=1,lm
!                     print*,'PMIDV=',PMIDV(i,j,l)
!                   end do
!                 end if  
!
!***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER FOR V POINT JUST BELOW
!***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
!
                  NL1X(I,J) = LP1
                  DO L=2,LM

!                   IF(J  ==  1 .AND. I  <  IM)THEN   !SOUTHERN BC
!                     PDV=0.5*(PMID(I,J,L)+PMID(I+1,J,L))
!                   ELSE IF(J == JM .AND. I < IM)THEN   !NORTHERN BC
!                     PDV=0.5*(PMID(I,J,L)+PMID(I+1,J,L))
!                   ELSE IF(I  ==  1 .AND. MOD(J,2)  ==  0) THEN !WESTERN EVEN BC
!                     PDV=0.5*(PMID(I,J-1,L)+PMID(I,J+1,L))
!      	            ELSE IF(I  ==  IM .AND. MOD(J,2)  ==  0) THEN !EASTERN EVEN BC
!                     PDV=0.5*(PMID(I,J-1,L)+PMID(I,J+1,L))  
!                   ELSE IF (MOD(J,2)  <  1) THEN
!                     PDV=0.25*(PMID(I,J,L)+PMID(I-1,J,L)
!     &               +PMID(I,J+1,L)+PMID(I,J-1,L))
!                   ELSE
!                     PDV=0.25*(PMID(I,J,L)+PMID(I+1,J,L)
!     &                +PMID(I,J+1,L)+PMID(I,J-1,L))
!                   END IF
!                   JJB=JSTA 
!                   IF(MOD(JSTA,2) == 0)JJB=JSTA+1
!                   JJE=JEND
!                   IF(MOD(JEND,2) == 0)JJE=JEND-1
!                   DO J=JJB,JJE,2 !chc
!                     PDV(IM,J)=PDV(IM-1,J)
!                   END DO

                    IF(NL1X(I,J) == LP1.AND.PMIDV(I,J,L) > SPL(LP))THEN
                      NL1X(I,J) = L
!                      IF(i == im/2 .and. j == jm/2)print*,                   &  
!                          'Wind Debug:LP,NL1X',LP,NL1X(I,J)
                    ENDIF
                  ENDDO
!
!  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
!  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
!  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
!  WILL EXTRAPOLATE TO THAT POINT
!
!                 IF(NL1X(I,J) == LMP1.AND.PINT(I,J,LMP1) > SPL(LP))THEN	
                  IF(NL1X(I,J) == LP1)THEN
                    IF(J  ==  1 .AND. I  <  IM)THEN   !SOUTHERN BC
                      PDV = 0.5*(PINT(I,J,LP1)+PINT(I+1,J,LP1))
                    ELSE IF(J == JM .AND. I < IM)THEN   !NORTHERN BC
                      PDV = 0.5*(PINT(I,J,LP1)+PINT(I+1,J,LP1))
                    ELSE IF(I  ==  1 .AND. MOD(J,2)  ==  0) THEN   !WESTERN EVEN BC
                      PDV = 0.5*(PINT(I,J-1,LP1)+PINT(I,J+1,LP1))
                    ELSE IF(I  ==  IM .AND. MOD(J,2)  ==  0) THEN  !EASTERN EVEN BC
                      PDV = 0.5*(PINT(I,J-1,LP1)+PINT(I,J+1,LP1))  
                    ELSE IF (MOD(J,2)  <  1) THEN
                      PDV = 0.25*(PINT(I,J,LP1)+PINT(I-1,J,LP1)               &
                          +       PINT(I,J+1,LP1)+PINT(I,J-1,LP1))
                    ELSE
                      PDV = 0.25*(PINT(I,J,LP1)+PINT(I+1,J,LP1)               &
                          +       PINT(I,J+1,LP1)+PINT(I,J-1,LP1))
                    END IF
                    IF(PDV  > SPL(LP))THEN
                      NL1X(I,J) = LM
                    END IF 
                  ENDIF
!
                ENDDO
              ENDDO
!
              DO J=JSTA,JEND
                DO I=1,IM-MOD(j,2)
        
                  LL = NL1X(I,J)
!---------------------------------------------------------------------
!***  VERTICAL INTERPOLATION OF WINDS FOR A-E GRID
!---------------------------------------------------------------------
!         
!HC               IF(NL1X(I,J).LE.LM)THEN
                  LLMH = NINT(LMH(I,J))

                  IF(SPL(LP)  <  PINT(I,J,2))THEN ! Above second interface
                    IF(UH(I,J,1) < SPVAL)  USL(I,J) = UH(I,J,1)
                    IF(VH(I,J,1) < SPVAL)  VSL(I,J) = VH(I,J,1)
     
                  ELSE IF(NL1X(I,J).LE.LLMH)THEN
!
!---------------------------------------------------------------------
!          INTERPOLATE LINEARLY IN LOG(P)
!***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
!***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
!***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
!---------------------------------------------------------------------
!

                    FACT = (ALSL(LP)-LOG(PMIDV(I,J,LL)))/                      &
                           (LOG(PMIDV(I,J,LL))-LOG(PMIDV(I,J,LL-1)))
                    IF(UH(I,J,LL) < SPVAL .AND. UH(I,J,LL-1) < SPVAL)          &
                       USL(I,J) = UH(I,J,LL)+(UH(I,J,LL)-UH(I,J,LL-1))*FACT
                    IF(VH(I,J,LL) < SPVAL .AND. VH(I,J,LL-1) < SPVAL)          &
                       VSL(I,J) = VH(I,J,LL)+(VH(I,J,LL)-VH(I,J,LL-1))*FACT
!                    IF(i == im/2 .and. j == jm/2)print*,                       &
!                    'Wind Debug:LP,NL1X,FACT=',LP,NL1X(I,J),FACT
!
! FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
! ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
! LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
! GOUND
                  ELSE
                    IF(UH(I,J,LLMH) < SPVAL) USL(I,J) = UH(I,J,LLMH)
                    IF(VH(I,J,LLMH) < SPVAL) VSL(I,J) = VH(I,J,LLMH)
                  END IF
                ENDDO     ! end of i loop
              ENDDO       ! end of j loop

!        if(me == 0) print *,'after 230 me=',me,'USL=',USL(1:10,JSTA)
              JJB = JSTA 
              IF(MOD(JSTA,2) == 0) JJB = JSTA+1
              JJE = JEND
              IF(MOD(JEND,2) == 0) JJE = JEND-1
              DO J=JJB,JJE,2 !chc
                USL(IM,J) = USL(IM-1,J)
                VSL(IM,J) = VSL(IM-1,J)
              END DO
            ELSE IF(gridtype=='B')THEN ! B grid wind interpolation
              DO J=JSTA,JEND_m
                DO I=1,IM-1
!***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER FOR V POINT JUST BELOW
!***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
!
                  NL1X(I,J)=LP1
                  DO L=2,LM
                    IF(NL1X(I,J) == LP1.AND.PMIDV(I,J,L) > SPL(LP))THEN
                      NL1X(I,J) = L
!                      IF(i == im/2 .and. j == jm/2)print*,                    &  
!                     'Wind Debug for B grid:LP,NL1X',LP,NL1X(I,J)
                    ENDIF
                  ENDDO
!
!  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
!  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
!  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
!  WILL EXTRAPOLATE TO THAT POINT
!
                  IF(NL1X(I,J)==LP1)THEN
                    PDV = 0.25*(PINT(I,J,LP1)+PINT(I+1,J,LP1)                 &
                        + PINT(I,J+1,LP1)+PINT(I+1,J+1,LP1))
                    IF(PDV  > SPL(LP))THEN
                      NL1X(I,J)=LM
                    END IF 
                  ENDIF
!
                ENDDO
              ENDDO
!
              DO J=JSTA,JEND_m
                DO I=1,IM-1
        
                  LL = NL1X(I,J)
!---------------------------------------------------------------------
!***  VERTICAL INTERPOLATION OF WINDS FOR A-E GRID
!---------------------------------------------------------------------
!         
!HC               IF(NL1X(I,J).LE.LM)THEN
                  LLMH = NINT(LMH(I,J))

                  IF(SPL(LP)  <  PINT(I,J,2))THEN ! Above second interface
                    IF(UH(I,J,1) < SPVAL)     USL(I,J) = UH(I,J,1)
                    IF(VH(I,J,1) < SPVAL)     VSL(I,J) = VH(I,J,1)
     
                  ELSE IF(NL1X(I,J).LE.LLMH)THEN
!
!---------------------------------------------------------------------
!          INTERPOLATE LINEARLY IN LOG(P)
!***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
!***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
!***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
!---------------------------------------------------------------------
!

                    FACT = (ALSL(LP)-LOG(PMIDV(I,J,LL)))/                      &
                           (LOG(PMIDV(I,J,LL))-LOG(PMIDV(I,J,LL-1)))
                    IF(UH(I,J,LL) < SPVAL .AND. UH(I,J,LL-1) < SPVAL)          &
                       USL(I,J)=UH(I,J,LL)+(UH(I,J,LL)-UH(I,J,LL-1))*FACT
                    IF(VH(I,J,LL) < SPVAL .AND. VH(I,J,LL-1) < SPVAL)          &
                       VSL(I,J)=VH(I,J,LL)+(VH(I,J,LL)-VH(I,J,LL-1))*FACT
!                    IF(i == im/2 .and. j == jm/2)print*,                         &
!                      'Wind Debug:LP,NL1X,FACT=',LP,NL1X(I,J),FACT
!
! FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
! ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
! LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
! GOUND
                  ELSE
                    IF(UH(I,J,LLMH) < SPVAL)USL(I,J)=UH(I,J,LLMH)
                    IF(VH(I,J,LLMH) < SPVAL)VSL(I,J)=VH(I,J,LLMH)
                  END IF
                enddo
              enddo
            END IF  ! END OF WIND INTERPOLATION FOR NMM
!        if(me == 0) print *,'after 230 if me=',me,'USL=',USL(1:10,JSTA)


!
!---------------------------------------------------------------------
!        LOAD GEOPOTENTIAL AND TEMPERATURE INTO STANDARD LEVEL 
!        ARRAYS FOR THE NEXT PASS.
!---------------------------------------------------------------------
!     
!***     SAVE 500MB TEMPERATURE FOR LIFTED INDEX.
!     
            IF(NINT(SPL(LP)) == 50000)THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  T500(I,J) = TSL(I,J)
                ENDDO
              ENDDO
            ENDIF
!     
!---------------------------------------------------------------------
!***  CALCULATE 1000MB GEOPOTENTIALS CONSISTENT WITH SLP OBTAINED 
!***  FROM THE MESINGER OR NWS SHUELL SLP REDUCTION.
!---------------------------------------------------------------------
!     
!***  FROM MESINGER SLP
!
!HC MOVE THIS PART TO THE END OF THIS SUBROUTINE AFTER PSLP IS COMPUTED
!HC        IF(IGET(023) > 0.AND.NINT(SPL(LP)) == 100000)THEN
!HC          ALPTH=LOG(1.E5)
!HC!$omp  parallel do private(i,j)
!HC          DO J=JSTA,JEND
!HC          DO I=1,IM
!HC           IF(FSL(I,J) < SPVAL) THEN
!HC            PSLPIJ=PSLP(I,J)
!HC            ALPSL=LOG(PSLPIJ)
!HC            PSFC=PINT(I,J,NINT(LMH(I,J))+1)
!HC            IF(ABS(PSLPIJ-PSFC) < 5.E2) THEN
!HC              FSL(I,J)=R*TSL(I,J)*(ALPSL-ALPTH)
!HC            ELSE
!HC              FSL(I,J)=FIS(I,J)/(ALPSL-LOG(PSFC))*
!HC     1                              (ALPSL-ALPTH)
!HC            ENDIF
!HC            Z1000(I,J)=FSL(I,J)*GI
!HC           ELSE
!HC            Z1000(I,J)=SPVAL
!HC           ENDIF
!HC          ENDDO
!HC          ENDDO
!     
!***  FROM NWS SHUELL SLP. NGMSLP2 COMPUTES 1000MB GEOPOTENTIAL.
!
!HC        ELSEIF(IGET(023).LE.0.AND.LP == LSM)THEN
!HC        IF(IGET(023).LE.0.AND.LP == LSM)THEN
!!$omp  parallel do private(i,j)
!HC          DO J=JSTA,JEND
!HC          DO I=1,IM
!HC           IF(Z1000(I,J) < SPVAL) THEN
!HC            FSL(I,J)=Z1000(I,J)*G
!HC           ELSE
!HC            FSL(I,J)=SPVAL
!HC           ENDIF
!HC          ENDDO
!HC          ENDDO
!HC        ENDIF
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!        *** PART II ***
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!        INTERPOLATE/OUTPUT SELECTED FIELDS.
!
!---------------------------------------------------------------------
!
!***  OUTPUT GEOPOTENTIAL (SCALE BY GI)
!
            IF(IGET(012) > 0)THEN
              IF(LVLS(LP,IGET(012)) > 0)THEN
                IF(IGET(023) > 0 .AND. NINT(SPL(LP)) == 100000) THEN
                  GO TO 222
                ELSE
!$omp  parallel do private(i,j)
                  DO J=JSTA,JEND
                    DO I=1,IM
                      IF(FSL(I,J) < SPVAL) THEN
                        GRID1(I,J) = FSL(I,J)*GI
                      ELSE
                        GRID1(I,J) = SPVAL
                      ENDIF
                    ENDDO
                  ENDDO

                  IF (SMFLAG) THEN
!tgs - smoothing of geopotential heights
                    if(MAPTYPE == 6) then
                      dxm = (DXVAL / 360.)*(ERAD*2.*pi)/1000.
                    else
                      dxm = dxval
                    endif
                    if(grib == 'grib2')then
                      dxm=dxm/1000.0
                    endif
                    print *,'dxm=',dxm
                    NSMOOTH = nint(5.*(13500./dxm))
                    call AllGETHERV(GRID1)
                    do k=1,NSMOOTH
                      CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
                    end do
                  ENDIF
                  if(grib == 'grib1')then
                    ID(1:25)=0
                    CALL GRIBIT(IGET(012),LP,GRID1,IM,JM)
                  elseif(grib == 'grib2') then
                    cfld = cfld + 1
                    fld_info(cfld)%ifld=IAVBLFLD(IGET(012))
                    fld_info(cfld)%lvl=LVLSXML(LP,IGET(012))
!$omp parallel do private(i,j,jj)
                    do j=1,jend-jsta+1
                      jj = jsta+j-1
                      do i=1,im
                        datapd(i,j,cfld) = GRID1(i,jj)
                      enddo
                    enddo
                  endif
                END IF
              ENDIF
            ENDIF
 222        CONTINUE
!     
!***  TEMPERATURE
!
            IF(IGET(013) > 0) THEN
              IF(LVLS(LP,IGET(013)) > 0)THEN
!$omp  parallel do private(i,j)
                DO J=JSTA,JEND
                  DO I=1,IM
                    GRID1(I,J) = TSL(I,J)
                  ENDDO
                ENDDO

                IF (SMFLAG) THEN
                  NSMOOTH = nint(3.*(13500./dxm))
                  call AllGETHERV(GRID1)
                  do k=1,NSMOOTH
                    CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
                  end do
                ENDIF

                if(grib == 'grib1')then
                  ID(1:25)=0
                  CALL GRIBIT(IGET(013),LP,GRID1,IM,JM)
                elseif(grib == 'grib2') then
                  cfld = cfld + 1
                  fld_info(cfld)%ifld = IAVBLFLD(IGET(013))
                  fld_info(cfld)%lvl  = LVLSXML(LP,IGET(013))
!$omp parallel do private(i,j,jj)
                  do j=1,jend-jsta+1
                    jj = jsta+j-1
                    do i=1,im
                      datapd(i,j,cfld) = GRID1(i,jj)
                    enddo
                  enddo
                endif
              ENDIF
            ENDIF

!***  virtual TEMPERATURE
!
        IF(IGET(910).GT.0) THEN
          IF(LVLS(LP,IGET(910)).GT.0)THEN
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = TSL(I,J)*(1.+0.608*QSL(I,J))
              ENDDO
            ENDDO

            IF (SMFLAG) THEN
              NSMOOTH = nint(3.*(13500./dxm))
              call AllGETHERV(GRID1)
              do k=1,NSMOOTH
                CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
              end do
            ENDIF

            if(grib=='grib1')then
              ID(1:25)=0
              CALL GRIBIT(IGET(910),LP,GRID1,IM,JM)
            elseif(grib=='grib2') then
              cfld=cfld+1
              fld_info(cfld)%ifld = IAVBLFLD(IGET(910))
              fld_info(cfld)%lvl  = LVLSXML(LP,IGET(910))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF

!     
!***  POTENTIAL TEMPERATURE.
!
        IF(IGET(014) > 0)THEN
          IF(LVLS(LP,IGET(014)) > 0)THEN

            tem = (P1000/spl(lp)) ** capa
!$omp parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                IF(TSL(I,J) < SPVAL) THEN
                  grid1(I,J) = TSL(I,J) * tem
                ELSE
                  grid1(I,J) = SPVAL
                ENDIF
              ENDDO
            ENDDO
!!$omp  parallel do private(i,j)
!           DO J=JSTA,JEND
!             DO I=1,IM
!               EGRID2(I,J) = SPL(LP)
!             ENDDO
!           ENDDO
!
!           CALL CALPOT(EGRID2,TSL,EGRID1)
!!$omp  parallel do private(i,j)
!            DO J=JSTA,JEND
!              DO I=1,IM
!                GRID1(I,J) = EGRID1(I,J)
!              ENDDO
!            ENDDO

            if(grib == 'grib1')then
             ID(1:25)=0
             CALL GRIBIT(IGET(014),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
             cfld = cfld + 1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(014))
             fld_info(cfld)%lvl=LVLSXML(LP,IGET(014))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!***  RELATIVE HUMIDITY.
!
     
        IF(IGET(017) > 0 .OR. IGET(257) > 0)THEN
!         if ( me == 0)  print *,'IGET(17)=',IGET(017),'LP=',LP,IGET(257),  &
!             'LVLS=',LVLS(1,4)
          log1=.false.
          IF(IGET(017) > 0.) then
             if(LVLS(LP,IGET(017)) > 0 ) log1=.true.
          endif
          IF(IGET(257) > 0) then
             if(LVLS(LP,IGET(257)) > 0 ) log1=.true.
          endif
          if ( log1 ) then
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                EGRID2(I,J) = SPL(LP)
              ENDDO
            ENDDO
!
            IF(MODELNAME == 'GFS')THEN
              CALL CALRH_GFS(EGRID2(1,jsta),TSL(1,jsta),QSL(1,jsta),EGRID1(1,jsta))
            ELSEIF (MODELNAME == 'RAPR')THEN 
              CALL CALRH_GSD(EGRID2(1,jsta),TSL(1,jsta),QSL(1,jsta),EGRID1(1,jsta))
            ELSE
              CALL CALRH(EGRID2(1,jsta),TSL(1,jsta),QSL(1,jsta),EGRID1(1,jsta))
            END IF 
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                IF(EGRID1(I,J) < SPVAL) THEN
                  GRID1(I,J) = EGRID1(I,J)*100.
                ELSE
                  GRID1(I,J)  = EGRID1(I,J)
                ENDIF
              ENDDO
            ENDDO

            IF (SMFLAG) THEN
              NSMOOTH=nint(2.*(13500./dxm))
              call AllGETHERV(GRID1)
              do k=1,NSMOOTH
                CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
              end do
            ENDIF
            if(grib == 'grib1')then
              ID(1:25)=0
              CALL GRIBIT(IGET(017),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(017))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(017))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif

!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                SAVRH(I,J) = GRID1(I,J)
              ENDDO
            ENDDO

          ENDIF
        ENDIF
!     
!***  CLOUD FRACTION.
!
        IF(IGET(331) > 0)THEN
          IF(LVLS(LP,IGET(331)) > 0)THEN
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                CFRSL(I,J) = MIN(MAX(0.0,CFRSL(I,J)),1.0)
                IF(abs(CFRSL(I,J)-SPVAL) > SMALL)                   &    
                      GRID1(I,J) = CFRSL(I,J)*H100
              ENDDO 
            ENDDO
            if(grib == 'grib1')then
              ID(1:25) = 0
              CALL GRIBIT(IGET(331),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld = IAVBLFLD(IGET(331))
              fld_info(cfld)%lvl = LVLSXML(LP,IGET(331))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!***  DEWPOINT TEMPERATURE.
!
        IF(IGET(015) > 0)THEN
          IF(LVLS(LP,IGET(015)) > 0)THEN
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                EGRID2(I,J) = SPL(LP)
              ENDDO
            ENDDO
!
            CALL CALDWP(EGRID2(1,jsta),QSL(1,jsta),EGRID1(1,jsta),TSL(1,jsta))
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 IF(TSL(I,J) < SPVAL) THEN
                   GRID1(I,J) = EGRID1(I,J)
                ELSE
                  GRID1(I,J) = SPVAL
                ENDIF
               ENDDO
             ENDDO
            if(grib == 'grib1')then
              ID(1:25)=0
              CALL GRIBIT(IGET(015),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(015))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(015))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!***  SPECIFIC HUMIDITY.
!
        IF(IGET(016) > 0)THEN
          IF(LVLS(LP,IGET(016)) > 0)THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = QSL(I,J)
               ENDDO
             ENDDO
            CALL BOUND(GRID1,zero,H99999)
            if(grib == 'grib1')then
              ID(1:25)=0
              CALL GRIBIT(IGET(016),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(016))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(016))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!***  OMEGA
!
        IF(IGET(020) > 0)THEN
          IF(LVLS(LP,IGET(020)) > 0)THEN
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = OSL(I,J)
              ENDDO
            ENDDO

            IF (SMFLAG .or. ioform == 'binarympiio' ) THEN
              call AllGETHERV(GRID1)
              if (ioform == 'binarympiio') then
!               nsmooth = max(2, min(30,nint(jm/94.0)))
!             do k=1,5
                CALL SMOOTHC(GRID1,SDUMMY,IM,JM,0.5)
                CALL SMOOTHC(GRID1,SDUMMY,IM,JM,-0.5)
!             enddo
              else
                NSMOOTH = nint(3.*(13500./dxm))
!             endif
              do k=1,NSMOOTH
                CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
              end do
              endif
            ENDIF

            if(grib == 'grib1')then
              ID(1:25)=0
!             print *,'me=',me,'OMEGA,OSL=',OSL(1:10,JSTA)
              CALL GRIBIT(IGET(020),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(020))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(020))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!***  W
!
        IF(IGET(284) > 0)THEN
          IF(LVLS(LP,IGET(284)) > 0)THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = WSL(I,J)
               ENDDO
             ENDDO
            if(grib == 'grib1')then
              ID(1:25)=0
              CALL GRIBIT(IGET(284),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(284))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(284))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!***  MOISTURE CONVERGENCE
!
        IF(IGET(085) > 0)THEN
          IF(LVLS(LP,IGET(085)) > 0)THEN
            CALL CALMCVG(QSL(1,jsta_2l),USL(1,jsta_2l),VSL(1,jsta_2l),EGRID1(1,jsta_2l))
!        if(me == 0) print *,'after calmcvgme=',me,'USL=',USL(1:10,JSTA)
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = EGRID1(I,J)
               ENDDO
             ENDDO
!MEB NOT SURE IF I STILL NEED THIS
!     CONVERT TO DIVERGENCE FOR GRIB UNITS
!
!           CALL SCLFLD(GRID1,-1.0,IM,JM)
!MEB NOT SURE IF I STILL NEED THIS
           if(grib == 'grib1')then
             ID(1:25)=0
             CALL GRIBIT(IGET(085),LP,GRID1,IM,JM)
           elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(085))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(085))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
!          if(me==0) print *,'in mdl2p,mconv, lp=',fld_info(cfld)%lvl,'lp=',lp
           endif
          ENDIF
        ENDIF
!     
!***  U AND/OR V WIND
!
        IF(IGET(018) > 0.OR.IGET(019) > 0)THEN
          log1=.false.
          IF(IGET(018) > 0.) then
             if(LVLS(LP,IGET(018)) > 0 ) log1=.true.
          endif
          IF(IGET(019) > 0) then
             if(LVLS(LP,IGET(019)) > 0 ) log1=.true.
          endif
          if ( log1 ) then
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = USL(I,J)
                 GRID2(I,J) = VSL(I,J)
               ENDDO
             ENDDO

            IF (SMFLAG) THEN
              NSMOOTH=nint(5.*(13500./dxm))
              call AllGETHERV(GRID1)
              do k=1,NSMOOTH
                CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
              end do
              NSMOOTH=nint(5.*(13500./dxm))
              call AllGETHERV(GRID2)
              do k=1,NSMOOTH
                CALL SMOOTH(GRID2,SDUMMY,IM,JM,0.5)
              end do
            ENDIF

            if(grib == 'grib1')then
              ID(1:25)=0
              IF(IGET(018) > 0) CALL GRIBIT(IGET(018),LP,GRID1,IM,JM)
              ID(1:25)=0
              IF(IGET(019) > 0) CALL GRIBIT(IGET(019),LP,GRID2,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(018))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(018))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo

              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(019))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(019))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID2(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!***  ABSOLUTE VORTICITY
!
        IF (IGET(021) > 0) THEN
          IF (LVLS(LP,IGET(021)) > 0) THEN
            CALL CALVOR(USL,VSL,EGRID1)
!         print *,'me=',me,'EGRID1=',EGRID1(1:10,JSTA)
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = EGRID1(I,J)
               ENDDO
             ENDDO

            IF (SMFLAG .or. ioform == 'binarympiio' ) THEN
              call AllGETHERV(GRID1)
              if (ioform == 'binarympiio') then
!               nsmooth = max(2, min(30,nint(jm/94.0)))
!             do k=1,5
                CALL SMOOTHC(GRID1,SDUMMY,IM,JM,0.5)
                CALL SMOOTHC(GRID1,SDUMMY,IM,JM,-0.5)
!             enddo
              else
                NSMOOTH = nint(4.*(13500./dxm))
!             endif
              do k=1,NSMOOTH
                CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
              end do
              endif
            ENDIF

            if(grib == 'grib1')then
              ID(1:25)=0
              CALL GRIBIT(IGET(021),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(021))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(021))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF
!     
!        GEOSTROPHIC STREAMFUNCTION.
         IF (IGET(086) > 0) THEN
          IF (LVLS(LP,IGET(086)) > 0) THEN
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                EGRID2(I,J) = FSL(I,J)*GI
              ENDDO
            ENDDO
            CALL CALSTRM(EGRID2(1,jsta),EGRID1(1,jsta))
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 IF(FSL(I,J) < SPVAL) THEN
                   GRID1(I,J) = EGRID1(I,J)
                 ELSE
                   GRID1(I,J) = SPVAL
                 ENDIF
               ENDDO
             ENDDO
            if(grib == 'grib1')then
              ID(1:25)=0
              CALL GRIBIT(IGET(086),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(086))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(086))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
         ENDIF
!     
!***  TURBULENT KINETIC ENERGY
!
         IF (IGET(022) > 0) THEN
          IF (LVLS(LP,IGET(022)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = Q2SL(I,J)
               ENDDO
             ENDDO
            if(grib == 'grib1')then
              ID(1:25)=0
              CALL GRIBIT(IGET(022),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(022))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(022))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
         ENDIF
!     
!***  CLOUD WATER
!
         IF (IGET(153) > 0) THEN
          IF (LVLS(LP,IGET(153)) > 0) THEN
            IF(MODELNAME == 'GFS')then
! GFS does not seperate cloud water from ice, hoping to do that in Feb 08 implementation	     
!$omp  parallel do private(i,j)
               DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J) = QW1(I,J) + QI1(I,J)
                 ENDDO
               ENDDO
             ELSE
!$omp  parallel do private(i,j)
               DO J=JSTA,JEND
                 DO I=1,IM
                   GRID1(I,J) = QW1(I,J)
                 ENDDO
               ENDDO
             END IF 
             if(grib == 'grib1')then
               ID(1:25)=0 
               CALL GRIBIT(IGET(153),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(153))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(153))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
             endif
          ENDIF
         ENDIF
!
!***  CLOUD ICE 
!
         IF (IGET(166) > 0) THEN
          IF (LVLS(LP,IGET(166)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = QI1(I,J)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               CALL GRIBIT(IGET(166),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(166))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(166))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF
!
!---  RAIN
         IF (IGET(183) > 0) THEN
          IF (LVLS(LP,IGET(183)) > 0) THEN 
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = QR1(I,J)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               CALL GRIBIT(IGET(183),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(183))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(183))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF
!
!---  SNOW
         IF (IGET(184) > 0) THEN
           IF (LVLS(LP,IGET(184)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = QS1(I,J)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               CALL GRIBIT(IGET(184),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(184))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(184))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
           ENDIF
         ENDIF
!
!---  GRAUPEL
         IF (IGET(416) > 0) THEN
          IF (LVLS(LP,IGET(416)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = QG1(I,J)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               CALL GRIBIT(IGET(416),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(416))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(416))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF

!
!---  TOTAL CONDENSATE
         IF (IGET(198) > 0) THEN
          IF (LVLS(LP,IGET(198)) > 0) THEN 
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = C1D(I,J)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=129    ! Parameter Table 129
               CALL GRIBIT(IGET(198),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(198))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(198))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF
!
!---  RIME FACTOR
         IF (IGET(263) > 0) THEN
          IF (LVLS(LP,IGET(263)) > 0) THEN 
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = FRIME(I,J)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=129    ! Parameter Table 129
               CALL GRIBIT(IGET(263),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(263))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(263))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF
!
!---  Temperature tendency by all radiation:  == ested by AFWA
         IF (IGET(294) > 0) THEN
          IF (LVLS(LP,IGET(294)) > 0) THEN 
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = RAD(I,J)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               CALL GRIBIT(IGET(294),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(294))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(294))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF
!
!---  Radar Reflectivity
         IF (IGET(251) > 0) THEN
          IF (LVLS(LP,IGET(251)) > 0) THEN
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = DBZ1(I,J)
              ENDDO
            ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=129
               CALL GRIBIT(IGET(251),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(251))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(251))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF
!
!---  IN-FLIGHT ICING CONDITION: ADD BY B. ZHOU
        IF(IGET(257) > 0)THEN
          IF(LVLS(LP,IGET(257)) > 0)THEN
            CALL CALICING(TSL(1,jsta), SAVRH, OSL(1,jsta), EGRID1(1,jsta))
 
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = EGRID1(I,J)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               CALL GRIBIT(IGET(257),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(257))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(257))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
        ENDIF

!
!---  GFIP IN-FLIGHT ICING POTENTIAL: ADDED BY H CHUANG
        IF(IGET(450) > 0)THEN
          IF(LVLS(LP,IGET(450)) > 0)THEN                                  
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = ICINGFSL(I,J)
               ENDDO
             ENDDO
            ID(1:25)=0
            ID(02)=140    ! Parameter Table 140
            if(grib == 'grib1')then
              CALL GRIBIT(IGET(450),LP,GRID1,IM,JM) 
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(450))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(450))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF

!---  GFIP IN-FLIGHT ICING SEVERITY: ADDED BY Y MAO
        IF(IGET(480) >  0) THEN
          IF(LVLS(LP,IGET(480)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = ICINGVSL(I,J)
               ENDDO
             ENDDO
            if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=129       ! Parameter Table 129
               CALL GRIBIT(IGET(480),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
              cfld = cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(480))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(480))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF

!---  GTG EDR turbulence: ADDED BY Y. MAO
        IF(IGET(464) >  0) THEN
          IF(LVLS(LP,IGET(464)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = GTGSL(I,J)
               ENDDO
             ENDDO
            if(grib == 'grib1')then
               ID(1:25)=0
               CALL GRIBIT(IGET(464),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
              cfld = cfld+1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(464))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(464))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
        ENDIF

!---  CLEAR AIR TURBULENCE (CAT): ADD BY B. ZHOU
        IF (LP > 1) THEN
          IF(IGET(258) > 0)THEN
            IF(LVLS(LP,IGET(258)) > 0)THEN
!$omp  parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J)  = FSL(I,J)*GI
                  EGRID1(I,J) = SPVAL
                ENDDO
              ENDDO
              CALL CALCAT(USL(1,jsta_2l),VSL(1,jsta_2l),GRID1(1,jsta_2l) &
                         ,USL_OLD(1,jsta_2l),VSL_OLD(1,jsta_2l)          &
                         ,FSL_OLD(1,jsta_2l),EGRID1(1,jsta_2l))
!$omp  parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = EGRID1(I,J)
!                 IF(GRID1(I,J) > 3. .OR. GRID1(I,J) < 0.)
!     +            print*,'bad CAT',i,j,GRID1(I,J)
                ENDDO
              ENDDO
              if(grib == 'grib1')then
                ID(1:25)=0
                CALL GRIBIT(IGET(258),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(258))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(258))
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            end if
          end if
        end if    
!     
 
!$omp  parallel do private(i,j)
        DO J=JSTA_2L,JEND_2U
          DO I=1,IM
            USL_OLD(I,J) = USL(I,J)
            VSL_OLD(I,J) = VSL(I,J)
            FSL_OLD(I,J) = FSL(I,J)*GI
          ENDDO
        ENDDO
!
!---  OZONE
         IF (IGET(268) > 0) THEN
          IF (LVLS(LP,IGET(268)) > 0) THEN
!$omp  parallel do private(i,j)
            DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = O3SL(I,J)
              ENDDO
            ENDDO
!             print *,'in mdl2p,o3sl=',minval(o3sl(1:im,jsta:jend)), &
!               minval(o3sl(1:im,jsta:jend))
            if(grib == 'grib1')then
              ID(1:25) = 0
              CALL GRIBIT(IGET(268),LP,GRID1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(268))
              fld_info(cfld)%lvl=LVLSXML(LP,IGET(268))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = GRID1(i,jj)
                enddo
              enddo
            endif
          ENDIF
         ENDIF
         if (gocart_on) then
!--- DUST 
         IF (IGET(438) > 0) THEN
          IF (LVLS(LP,IGET(438)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = DUSTSL(I,J,1)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=141             ! Parameter Table 141
               CALL GRIBIT(IGET(438),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(438))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(438))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF

         IF (IGET(439) > 0) THEN
          IF (LVLS(LP,IGET(439)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = DUSTSL(I,J,2)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=141             ! Parameter Table 141
               CALL GRIBIT(IGET(439),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(439))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(439))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF

         IF (IGET(440) > 0) THEN
          IF (LVLS(LP,IGET(440)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = DUSTSL(I,J,3)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=141             ! Parameter Table 141
               CALL GRIBIT(IGET(440),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(440))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(440))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF

         IF (IGET(441) > 0) THEN
          IF (LVLS(LP,IGET(441)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = DUSTSL(I,J,4)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=141             ! Parameter Table 141
               CALL GRIBIT(IGET(441),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(441))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(441))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF

         IF (IGET(442) > 0) THEN
          IF (LVLS(LP,IGET(442)) > 0) THEN
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J) = DUSTSL(I,J,5)
               ENDDO
             ENDDO
             if(grib == 'grib1')then
               ID(1:25)=0
               ID(02)=141             ! Parameter Table 141
               CALL GRIBIT(IGET(442),LP,GRID1,IM,JM)
             elseif(grib == 'grib2') then
               cfld = cfld + 1
               fld_info(cfld)%ifld=IAVBLFLD(IGET(442))
               fld_info(cfld)%lvl=LVLSXML(LP,IGET(442))
!$omp parallel do private(i,j,jj)
               do j=1,jend-jsta+1
                 jj = jsta+j-1
                 do i=1,im
                   datapd(i,j,cfld) = GRID1(i,jj)
                 enddo
               enddo
             endif
          ENDIF
         ENDIF
         endif  ! if gocart_on


         if(iostatusD3D==0 .and. d3d_on) then
!---  longwave tendency
           IF (IGET(355) > 0) THEN
            IF (LVLS(LP,IGET(355)) > 0) THEN
!$omp  parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,1)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(355),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(355))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(355))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(354) > 0) THEN
            IF (LVLS(LP,IGET(354)) > 0) THEN
!$omp  parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,2)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(354),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(354))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(354))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(356) > 0) THEN
            IF (LVLS(LP,IGET(356)) > 0) THEN
!$omp  parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,3)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(356),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(356))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(356))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(357) > 0) THEN
            IF (LVLS(LP,IGET(357)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
              DO I=1,IM
                GRID1(I,J) = D3DSL(i,j,4)
              ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(357),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(357))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(357))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(358) > 0) THEN
            IF (LVLS(LP,IGET(358)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,5)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(358),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(358))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(358))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(359) > 0) THEN
            IF (LVLS(LP,IGET(359)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,6)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(359),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(359))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(359))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(360) > 0) THEN
            IF (LVLS(LP,IGET(360)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,7)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(360),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(360))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(360))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(361) > 0) THEN
            IF (LVLS(LP,IGET(361)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,8)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(361),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(361))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(361))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(362) > 0) THEN
            IF (LVLS(LP,IGET(362)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,9)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(362),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(362))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(362))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(363) > 0) THEN
            IF (LVLS(LP,IGET(363)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,10)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(363),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(363))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(363))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(364) > 0) THEN
            IF (LVLS(LP,IGET(364)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,11)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(364),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(364))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(364))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(365) > 0) THEN
            IF (LVLS(LP,IGET(365)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,12)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(365),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(365))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(365))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(366) > 0) THEN
            IF (LVLS(LP,IGET(366)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,13)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(366),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(366))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(366))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(367) > 0) THEN
            IF (LVLS(LP,IGET(367)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,14)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(367),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(367))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(367))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(368) > 0) THEN
            IF (LVLS(LP,IGET(368)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,15)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(368),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(368))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(368))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(369) > 0) THEN
            IF (LVLS(LP,IGET(369)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,16)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(369),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(369))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(369))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(370) > 0) THEN
            IF (LVLS(LP,IGET(370)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,17)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(370),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(370))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(370))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(371) > 0) THEN
            IF (LVLS(LP,IGET(371)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,18)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(371),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(371))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(371))
                if(ITD3D==0) then
                 fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif 
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(372) > 0) THEN
            IF (LVLS(LP,IGET(372)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,19)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(372),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(372))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(372))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(373) > 0) THEN
            IF (LVLS(LP,IGET(373)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,20)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(373),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(373))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(373))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(374) > 0) THEN
            IF (LVLS(LP,IGET(374)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,21)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1')then
                CALL GRIBIT(IGET(374),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(374))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(374))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  longwave tendency
           IF (IGET(375) > 0) THEN
            IF (LVLS(LP,IGET(375)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,22)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1') then
                CALL GRIBIT(IGET(375),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(375))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(375))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!--- total diabatic heating
           IF (IGET(379) > 0) THEN
            IF (LVLS(LP,IGET(379)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  IF(D3DSL(i,j,1)/=SPVAL)THEN
                    GRID1(I,J) = D3DSL(i,j,1) + D3DSL(i,j,2)         &
                               + D3DSL(i,j,3) + D3DSL(i,j,4)         &
                               + D3DSL(i,j,5) + D3DSL(i,j,6)
                  ELSE
                    GRID1(I,J) = SPVAL
                  END IF
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1') then
              CALL GRIBIT(IGET(379),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(379))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(379))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  convective updraft
           IF (IGET(391) > 0) THEN
            IF (LVLS(LP,IGET(391)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,23)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1') then
                CALL GRIBIT(IGET(391),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(391))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(391))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  convective downdraft
           IF (IGET(392) > 0) THEN
            IF (LVLS(LP,IGET(392)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,24)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1') then
                CALL GRIBIT(IGET(392),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(392))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(392))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  convective detraintment
           IF (IGET(393) > 0) THEN
            IF (LVLS(LP,IGET(393)) > 0) THEN
!$omp parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,25)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1') then
                CALL GRIBIT(IGET(393),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(393))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(393))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  convective gravity drag zonal acce
           IF (IGET(394) > 0) THEN
            IF (LVLS(LP,IGET(394)) > 0) THEN
!$omp  parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,26)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
	      ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
	      ENDIF
              if(grib == 'grib1') then
                CALL GRIBIT(IGET(394),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
              cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(394))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(394))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
!---  convective gravity drag meridional acce
           IF (IGET(395) > 0) THEN
            IF (LVLS(LP,IGET(395)) > 0) THEN
!$omp  parallel do private(i,j)
              DO J=JSTA,JEND
                DO I=1,IM
                  GRID1(I,J) = D3DSL(i,j,27)
                ENDDO
              ENDDO
              ID(1:25)=0
              ITD3D     = NINT(TD3D)
              if (ITD3D .ne. 0) then
                IFINCR     = MOD(IFHR,ITD3D)
                IF(IFMIN .GE. 1)IFINCR= MOD(IFHR*60+IFMIN,ITD3D*60)
              else
                IFINCR     = 0
              endif
              ID(02)=133 ! Table 133
              ID(18)     = 0
              ID(19)     = IFHR
              IF(IFMIN .GE. 1)ID(19)=IFHR*60+IFMIN
              ID(20)     = 3
              IF (IFINCR == 0) THEN
                ID(18) = IFHR-ITD3D
              ELSE
                ID(18) = IFHR-IFINCR
                IF(IFMIN .GE. 1)ID(18)=IFHR*60+IFMIN-IFINCR
              ENDIF
              if(grib == 'grib1') then
                CALL GRIBIT(IGET(395),LP,GRID1,IM,JM)
              elseif(grib == 'grib2') then
                cfld = cfld + 1
                fld_info(cfld)%ifld=IAVBLFLD(IGET(395))
                fld_info(cfld)%lvl=LVLSXML(LP,IGET(395))
                if(ITD3D==0) then
                  fld_info(cfld)%ntrange=0
                else
                  fld_info(cfld)%ntrange=(IFHR-ID(18))/ITD3D
                endif
                fld_info(cfld)%tinvstat=ITD3D
!$omp parallel do private(i,j,jj)
                do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                enddo
              endif
            ENDIF
           ENDIF
         END IF ! end of d3d output

!   CHUANG:   COMPUTE HAINES INDEX 
         IF (IGET(455) > 0) THEN
           ii=im/2+100
           jj=(jsta+jend)/2-100
           IF(ABS(SPL(LP)-50000.)<SMALL) LUHI=LP
           IF(ABS(SPL(LP)-70000.)<SMALL) THEN ! high evevation
!            HAINES=SPVAL
!            print*,'computing dew point for Haine Index at ',SPL(LP)
!$omp  parallel do private(i,j)
             DO J=JSTA,JEND
               DO I=1,IM
                 HAINES(i,j) = SPVAL
                 EGRID2(I,J) = SPL(LP)
               ENDDO
             ENDDO
             CALL CALDWP(EGRID2(1,jsta),QSL(1,jsta),TDSL(1,jsta),TSL(1,jsta))

!$omp  parallel do private(i,j,dum1,ista,imois)
             DO J=JSTA,JEND
               DO I=1,IM
                 IF(SM(I,J) < 1.0 .AND. ZINT(I,J,LM+1) < FSL(I,J)*GI) THEN
                   DUM1 = TSL(I,J)-TPRS(I,J,LUHI)
                   IF(DUM1 <= 17.)THEN
                     ISTA = 1
                   ELSE IF(DUM1 > 17. .AND. DUM1 <= 21.) THEN
                     ISTA = 2
                   ELSE
                     ISTA = 3
                   END IF
                   DUM1 = TSL(I,J)-TDSL(I,J)
                   IF(DUM1 <= 14.) THEN
                     IMOIS = 1
                   ELSE IF(DUM1>14. .AND. DUM1<=20.) THEN
                     IMOIS = 2
                   ELSE
                     IMOIS = 3
                   END IF
                   HAINES(I,J) = ISTA + IMOIS
! 	       if(i==570 .and. j==574)print*,'high hainesindex:',i,j,luhi,tsl(i,j) &
! 	       ,tprs(i,j,luhi),tdsl(i,j),ista,imois,spl(luhi),spl(lp),haines(i,j)
                 END IF 
               END DO
             END DO  

             LUHI = LP
         ENDIF
   
         IF(ABS(SPL(LP)-85000.)<SMALL)THEN ! mid evevation
!          print*,'computing dew point for Haine Index at ',SPL(LP)
!$omp  parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               EGRID2(I,J) = SPL(LP)
             ENDDO
           ENDDO
           CALL CALDWP(EGRID2(1,jsta),QSL(1,jsta),TDSL(1,jsta),TSL(1,jsta))
    
!$omp  parallel do private(i,j,dum1,ista,imois)
           DO J=JSTA,JEND
             DO I=1,IM
               IF(SM(I,J) < 1.0 .AND. ZINT(I,J,LM+1) < FSL(I,J)*GI) THEN
                 DUM1 = TSL(I,J)-TPRS(I,J,LUHI)
                 IF(DUM1 <=5. ) THEN
                   ISTA = 1
                 ELSE IF(DUM1 > 5. .AND. DUM1 <= 10.) THEN
                   ISTA = 2
                 ELSE
                   ISTA = 3
                 END IF
                 DUM1 = TSL(I,J)-TDSL(I,J)
                 IF(DUM1 <= 5.) THEN
                   IMOIS = 1
                 ELSE IF(DUM1 > 5. .AND. DUM1 <= 12.) THEN
                   IMOIS = 2
                 ELSE
                   IMOIS = 3
                 END IF
! 	       if(i==570 .and. j==574)print*,'mid haines index:',i,j,luhi,tsl(i,j) &
! 	       ,tprs(i,j,luhi),tdsl(i,j),ista,imois,spl(luhi),spl(lp),haines(i,j)
                 HAINES(I,J) = ISTA + IMOIS
               END IF 
             END DO
           END DO  

           LUHI = LP
         ENDIF
   
         IF(ABS(SPL(LP)-95000.)<SMALL)THEN ! LOW evevation
!          print*,'computing dew point for Haine Index at ',SPL(LP)
!$omp  parallel do private(i,j)
           DO J=JSTA,JEND
             DO I=1,IM
               EGRID2(I,J)=SPL(LP)
             ENDDO
           ENDDO
           CALL CALDWP(EGRID2(1,jsta),QSL(1,jsta),TDSL(1,jsta),TSL(1,jsta))
    
!$omp  parallel do private(i,j,dum1,ista,imois)
           DO J=JSTA,JEND
             DO I=1,IM
               IF(SM(I,J) < 1.0 .AND. ZINT(I,J,LM+1) < FSL(I,J)*GI) THEN
                 DUM1 = TSL(I,J)-TPRS(I,J,LUHI)
                 IF(DUM1 <= 3.)THEN
                   ISTA = 1
                 ELSE IF(DUM1 > 3. .AND. DUM1 <=7. ) THEN
                   ISTA = 2
                 ELSE
                   ISTA = 3
                 END IF
                 DUM1 = TSL(I,J)-TDSL(I,J)
                 IF(DUM1 <=5. ) THEN
                   IMOIS = 1
                 ELSE IF(DUM1 > 5. .AND. DUM1 <= 9.)THEN
                   IMOIS = 2
                 ELSE
                   IMOIS = 3
                 END IF
! 	       if(i==570 .and. j==574)print*,'low haines index:',i,j,luhi,tsl(i,j) &
! 	       ,tprs(i,j,luhi),tdsl(i,j),ista,imois,spl(luhi),spl(lp),haines(i,j)
                 HAINES(I,J) = ISTA + IMOIS
               END IF 
             END DO
           END DO  
 
           ID(1:25)=0

           ID(02)=129 ! Table 129
!	   ID(10)  =85
!          ID(11)  =95

            if(grib == 'grib1') then 
              do j=jsta,jend
                do i=1,im
                  grid1(i,j) = haines(i,j)
                enddo
              enddo
              CALL GRIBIT(IGET(455),1,grid1,IM,JM)
            elseif(grib == 'grib2') then
              cfld = cfld + 1
              fld_info(cfld)%ifld=IAVBLFLD(IGET(455))
!$omp parallel do private(i,j,jj)
              do j=1,jend-jsta+1
                jj = jsta+j-1
                do i=1,im
                  datapd(i,j,cfld) = HAINES(i,jj)
                enddo
              enddo
            endif

           ENDIF
    
          ENDIF
!     
        ENDDO     !***  END OF MAIN VERTICAL LOOP DO LP=1,LSM
!***  ENDIF FOR IF TEST SEEING IF WE WANT ANY OTHER VARIABLES
      ENDIF
! SRD
!
!        MAX VERTICAL VELOCITY UPDRAFT
!
      IF (IGET(423) > 0) THEN
         ID(1:25) = 0
!         LP=22 ! 400 MB
         LP=46 ! 1000 MB
         ID(02)=129 ! Table 129
         ID(9) = 101
         ID(10)=40
         ID(11)=100
         ID(20) = 2
         ID(19) = IFHR
         IF (IFHR == 0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
!$omp  parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = W_UP_MAX(I,J)
!            print *,' writing w_up_max, i,j, = ', w_up_max(i,j)
           ENDDO
         ENDDO
         if(grib == 'grib1')then
           CALL GRIBIT(IGET(423),LP,GRID1,IM,JM)
         elseif(grib == 'grib2') then
           cfld = cfld + 1
           fld_info(cfld)%ifld = IAVBLFLD(IGET(423))
           fld_info(cfld)%lvl  = LVLSXML(LP,IGET(423))
            if (IFHR .gt. 0) then
               fld_info(cfld)%ntrange=1
               fld_info(cfld)%tinvstat=1
            else
               fld_info(cfld)%ntrange=0
               fld_info(cfld)%tinvstat=1
            endif
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
!
!        MAX VERTICAL VELOCITY DOWNDRAFT
!
      IF (IGET(424) > 0) THEN
         ID(1:25) = 0
         ID(02)   = 129 ! Table 129
         LP       = 46  ! 1000 MB
         ID(9)    = 101
         ID(10)   = 40
         ID(11)   = 100
         ID(20)   = 2
         ID(19)   = IFHR
         IF (IFHR == 0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
!$omp  parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = W_DN_MAX(I,J)
           ENDDO
         ENDDO
         if(grib == 'grib1')then
           CALL GRIBIT(IGET(424),LP,GRID1,IM,JM)
         elseif(grib == 'grib2') then
           cfld = cfld + 1
           fld_info(cfld)%ifld=IAVBLFLD(IGET(424))
           fld_info(cfld)%lvl=LVLSXML(LP,IGET(424))
            if (IFHR .gt. 0) then
               fld_info(cfld)%ntrange=1
               fld_info(cfld)%tinvstat=1
            else
               fld_info(cfld)%ntrange=0
               fld_info(cfld)%tinvstat=1
            endif
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
!
!        MEAN VERTICAL VELOCITY
!
! This hourly mean field is, in fact, based on the column
! mean bounded by sigma levels, but I chose to keep the
! output here with the other diagnostic vertical
! velocity fields
!
      IF (IGET(425) > 0) THEN
         ID(1:25) = 0
         LP       = 46 ! 1000 MB
         ID(9)    = 108
         ID(10)   = 50
         ID(11)   = 80
         ID(20)   = 2
         ID(19)   = IFHR
         IF (IFHR == 0) THEN
           ID(18) = 0
         ELSE
           ID(18) = IFHR - 1
         ENDIF
!$omp  parallel do private(i,j)
         DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J) = W_MEAN(I,J)
           ENDDO
         ENDDO
         if(grib == 'grib1')then
           CALL GRIBIT(IGET(425),LP,GRID1,IM,JM)
         elseif(grib == 'grib2') then
           cfld = cfld + 1
           fld_info(cfld)%ifld = IAVBLFLD(IGET(425))
           fld_info(cfld)%lvl  = LVLSXML(LP,IGET(425))
           if (ifhr == 0) then
              fld_info(cfld)%tinvstat = 0
           else
              fld_info(cfld)%tinvstat = 1
           endif
           fld_info(cfld)%ntrange = 1
!$omp parallel do private(i,j,jj)
           do j=1,jend-jsta+1
             jj = jsta+j-1
             do i=1,im
               datapd(i,j,cfld) = GRID1(i,jj)
             enddo
           enddo
         endif
      ENDIF
! SRD

!
!  CALL MEMBRANE SLP REDUCTION IF  REQESTED IN CONTROL FILE
!
! OUTPUT MEMBRANCE SLP
      IF(IGET(023) > 0)THEN
        IF(gridtype == 'A'.OR. gridtype == 'B') then                  
          if(me==0)PRINT*,'CALLING MEMSLP for A or B grid'
          CALL MEMSLP(TPRS,QPRS,FPRS)
        ELSE IF (gridtype == 'E')THEN
          if(me==0)PRINT*,'CALLING MEMSLP_NMM for E grid'
          CALL MEMSLP_NMM(TPRS,QPRS,FPRS)
        ELSE
          PRINT*,'unknow grid type-> WONT DERIVE MESINGER SLP'
        END IF
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = PSLP(I,J)
          ENDDO
        ENDDO
        ID(1:25) = 0
        if(grib == 'grib1')then
          CALL GRIBIT(IGET(023),LVLS(1,IGET(023)),GRID1,IM,JM)
        elseif(grib == 'grib2') then
          cfld = cfld + 1
          fld_info(cfld)%ifld = IAVBLFLD(IGET(023))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = GRID1(i,jj)
            enddo
          enddo
        endif
      ENDIF

! OUTPUT of MAPS SLP
      IF(IGET(445) > 0)THEN
        if(me==0)PRINT*,'CALLING MAPS SLP'
        CALL MAPSSLP(TPRS)
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            GRID1(I,J) = PSLP(I,J)
          ENDDO
        ENDDO
        ID(1:25) = 0
        if(grib == 'grib1') then
          CALL GRIBIT(IGET(445),LVLS(1,IGET(445)),GRID1,IM,JM)
        elseif(grib == 'grib2') then
          cfld = cfld + 1
          fld_info(cfld)%ifld = IAVBLFLD(IGET(445))
!$omp parallel do private(i,j,jj)
          do j=1,jend-jsta+1
            jj = jsta+j-1
            do i=1,im
              datapd(i,j,cfld) = GRID1(i,jj)
            enddo
          enddo
        endif
      ENDIF
 

! ADJUST 1000 MB HEIGHT TO MEMBEANCE SLP
      IF(IGET(023) > 0.OR.IGET(445) > 0)THEN
        IF(IGET(012) > 0)THEN
          DO LP=LSM,1,-1
           IF(ABS(SPL(LP)-1.0E5) <= 1.0E-5)THEN
             IF(LVLS(LP,IGET(012)) > 0)THEN
               ALPTH = LOG(1.E5)
               IF(MODELNAME == 'GFS')THEN
! GFS does not want to adjust 1000 mb H to membrane SLP
! because MOS can't adjust to the much lower H
!$omp  parallel do private(i,j)
                 DO J=JSTA,JEND
                   DO I=1,IM
                     GRID1(I,J) = FSL(I,J)*GI
                   ENDDO
                 ENDDO    
               ELSE
!$omp  parallel do private(i,j,PSLPIJ,ALPSL,PSFC)
                 DO J=JSTA,JEND
                   DO I=1,IM
                     PSLPIJ = PSLP(I,J)
                     ALPSL  = LOG(PSLPIJ)
                     PSFC   = PINT(I,J,NINT(LMH(I,J))+1)
                     IF(ABS(PSLPIJ-PSFC) < 5.E2) THEN
                       GRID1(I,J) = RD*TPRS(I,J,LP)*(ALPSL-ALPTH)
                     ELSE
                       GRID1(I,J) = FIS(I,J)/(ALPSL-LOG(PSFC))*(ALPSL-ALPTH)
                     ENDIF
                     Z1000(I,J) = GRID1(I,J)*GI
                     GRID1(I,J) = Z1000(I,J)
                   ENDDO
                 ENDDO    
               END IF

               IF (SMFLAG) THEN
!tgs - smoothing of geopotential heights
                 NSMOOTH = nint(5.*(13500./dxm))
                 call AllGETHERV(GRID1)
                 do k=1,NSMOOTH
                   CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
                 end do
               ENDIF

               if(grib == 'grib1') then
                 ID(1:25) = 0
                 CALL GRIBIT(IGET(012),LP,GRID1,IM,JM)
               elseif(grib == 'grib2') then
                 cfld = cfld + 1
                 fld_info(cfld)%ifld = IAVBLFLD(IGET(012))
                 fld_info(cfld)%lvl  = LVLSXML(LP,IGET(012))
!$omp parallel do private(i,j,jj)
                 do j=1,jend-jsta+1
                  jj = jsta+j-1
                  do i=1,im
                    datapd(i,j,cfld) = GRID1(i,jj)
                  enddo
                 enddo
               endif
               exit
             ENDIF
           ENDIF 
          END DO
        ENDIF  
      ENDIF
!
!     END OF ROUTINE.
!
      RETURN
      END
