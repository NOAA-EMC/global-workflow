      SUBROUTINE MDL2SIGMA
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    MDL2P       VERT INTRP OF MODEL LVLS TO PRESSURE
!   PRGRMMR: BLACK           ORG: W/NP22     DATE: 99-09-23       
!     
! ABSTRACT:
!     FOR MOST APPLICATIONS THIS ROUTINE IS THE WORKHORSE
!     OF THE POST PROCESSOR.  IN A NUTSHELL IT INTERPOLATES
!     DATA FROM MODEL TO PRESSURE SURFACES.  IT ORIGINATED
!     FROM THE VERTICAL INTERPOLATION CODE IN THE OLD ETA
!     POST PROCESSOR SUBROUTINE OUTMAP AND IS A REVISION
!     OF SUBROUTINE ETA2P.
!   .     
!     
! PROGRAM HISTORY LOG:
!   99-09-23  T BLACK       - REWRITTEN FROM ETA2P
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-12  MIKE BALDWIN - WRF VERSION
!   02-07-29  H CHUANG - ADD UNDERGROUND FIELDS AND MEMBRANE SLP FOR WRF
!   04-11-24  H CHUANG - ADD FERRIER'S HYDROMETEOR FIELD
!   11-02064  J WANG - ADD GRIB2 option
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
      use vrbls3d,       only: pint, t, q, zint, alpint, pmid, exch_h, uh, &
                               vh, omga, q2, cwm, qqw, qqi, qqr, qqs, cfr, &
                               f_rimef, pmidv
!     use vrbls2d,       only:
      use masks,         only: lmh
      use params_mod,    only: d50 , pq0, a2, a3, a4, h1, d01, d608, rgamog,&
                               h1m12, d00, h2, rd, g, gi, h99999
      use ctlblk_mod,    only: jsta_2l, jend_2u, spval, lp1, jsta, jend, lm, &
                               grib, cfld, datapd, fld_info, me, jend_m, im, &
                               jm, im_jm
      use rqstfld_mod,   only: iget, lvls, id, iavblfld, lvlsxml
      use gridspec_mod,  only :gridtype
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
       implicit none
!     
!     INCLUDE MODEL DIMENSIONS.  SET/DERIVE OTHER PARAMETERS.
!     GAMMA AND RGAMOG ARE USED IN THE EXTRAPOLATION OF VIRTUAL
!     TEMPERATURES BEYOND THE UPPER OF LOWER LIMITS OF DATA.
!     
      integer,PARAMETER :: LSIG=22
      real,PARAMETER :: PTSIGO=1.0E4
!     
!     DECLARE VARIABLES.
!     
      LOGICAL READTHK
      LOGICAL IOOMG,IOALL
      LOGICAL DONEFSL1,TSLDONE
      real, dimension(im,jsta_2l:jend_2u) :: FSL, TSL, QSL, OSL, USL, VSL, Q2SL, &
                                             FSL1, CFRSIG, EGRID1, EGRID2
      REAL GRID1(IM,JM),GRID2(IM,JM)
      REAL SIGO(LSIG+1),DSIGO(LSIG),ASIGO(LSIG)
!
      INTEGER IHOLD(IM_JM),JHOLD(IM_JM),NL1X(IM,JM),NL1XF(IM,JM)
!
!
!--- Definition of the following 2D (horizontal) dummy variables
!
!  C1D   - total condensate
!  QW1   - cloud water mixing ratio
!  QI1   - cloud ice mixing ratio
!  QR1   - rain mixing ratio
!  QS1   - snow mixing ratio
!
      real, dimension(im,jsta_2l:jend_2u) :: C1D, QW1, QI1, QR1, QS1, QG1, AKH
!
      integer I,J,L,LL,LP,LLMH,II,JJ,JJB,JJE,NHOLD
      real PFSIGO,APFSIGO,PSIGO,APSIGO,PNL1,PU,ZU,TU,QU,QSAT,  &
           RHU,TVRU,TVRABV,TABV,QABV,B,AHF,FAC,PL,ZL,TL,QL,    &
           RHL,TMT0,AI,BI,TVRL,TVRBLO,TBLO,QBLO,FACT,          &
           PX,BF,FACF,AHFF,DPSIG,TV,PDV,DENOM,DENOMF,PNL1F,DUM
!
!     
!******************************************************************************
!
!     START MDL2P. 
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
      IF((IGET(205).GT.0).OR.(IGET(206).GT.0).OR.              &
         (IGET(207).GT.0).OR.(IGET(208).GT.0).OR.              &
         (IGET(209).GT.0).OR.(IGET(210).GT.0).OR.              &
         (IGET(216).GT.0).OR.(IGET(217).GT.0).OR.              &
         (IGET(211).GT.0).OR.(IGET(212).GT.0).OR.              &
         (IGET(213).GT.0).OR.(IGET(214).GT.0).OR.              &
         (IGET(215).GT.0).OR.(IGET(222).GT.0).OR.              &
         (IGET(243).GT.0)  ) THEN  !!Air Quality (Plee Oct2003)
!
!---------------------------------------------------------------------
!
!---  VERTICAL INTERPOLATION OF GEOPOTENTIAL, SPECIFIC HUMIDITY, TEMPERATURE, 
!     OMEGA, TKE, & CLOUD FIELDS.  START AT THE UPPERMOST TARGET SIGMA LEVEL.
!
        READTHK=.FALSE.
        IF(READTHK)THEN   ! EITHER READ DSG THICKNESS
	 READ(41)DSIGO  !DSIGO FROM TOP TO BOTTOM
!
         SIGO(1)=0.0
	 DO L=2,LSIG+1
          SIGO(L)=SIGO(L-1)+DSIGO(LSIG-L+2)
	 END DO 
         SIGO(LSIG+1)=1.0
         DO L=1,LSIG
          ASIGO(L)=0.5*(SIGO(L)+SIGO(L+1))
         END DO
        ELSE  ! SPECIFY SIGO
         ASIGO( 1)=   0.0530
         ASIGO( 2)=   0.1580
         ASIGO( 3)=   0.2605
         ASIGO( 4)=   0.3595
         ASIGO( 5)=   0.4550
         ASIGO( 6)=   0.5470
         ASIGO( 7)=   0.6180
         ASIGO( 8)=   0.6690
         ASIGO( 9)=   0.7185
         ASIGO(10)=   0.7585
         ASIGO(11)=   0.7890
         ASIGO(12)=   0.8190
         ASIGO(13)=   0.8480
         ASIGO(14)=   0.8755
         ASIGO(15)=   0.9015
         ASIGO(16)=   0.9260
         ASIGO(17)=   0.9490
         ASIGO(18)=   0.9650
         ASIGO(19)=   0.9745
         ASIGO(20)=   0.9835
         ASIGO(21)=   0.9915
         ASIGO(22)=   0.9975 
!
         SIGO( 1)=   0.0
         SIGO( 2)=   0.1060
         SIGO( 3)=   0.2100
         SIGO( 4)=   0.3110
         SIGO( 5)=   0.4080
         SIGO( 6)=   0.5020
         SIGO( 7)=   0.5920
         SIGO( 8)=   0.6440
         SIGO( 9)=   0.6940
         SIGO(10)=   0.7430
         SIGO(11)=   0.7740
         SIGO(12)=   0.8040
         SIGO(13)=   0.8340
         SIGO(14)=   0.8620
         SIGO(15)=   0.8890
         SIGO(16)=   0.9140
         SIGO(17)=   0.9380
         SIGO(18)=   0.9600
         SIGO(19)=   0.9700
         SIGO(20)=   0.9790
         SIGO(21)=   0.9880
         SIGO(22)=   0.9950 
         SIGO(23)=   1.0
        END IF
! OBTAIN GEOPOTENTIAL AT 1ST LEVEL
       DO J=JSTA_2L,JEND_2U
       DO I=1,IM
	FSL(I,J)=SPVAL
	AKH(I,J)=SPVAL
        NL1XF(I,J)=LP1
        DO L=1,LP1
        IF(NL1XF(I,J).EQ.LP1.AND.PINT(I,J,L).GT.PTSIGO)THEN
          NL1XF(I,J)=L
        ENDIF
        ENDDO
       END DO	
       END DO
       DO 167 J=JSTA,JEND
        DO 167 I=1,IM 
	 DONEFSL1=.FALSE.
         PFSIGO=PTSIGO
         APFSIGO=LOG(PFSIGO)
         PNL1=PINT(I,J,NL1XF(I,J))
	 LL=NL1XF(I,J)
         LLMH = NINT(LMH(I,J))
         IF(NL1XF(I,J).EQ.1 .AND. T(I,J,1).LT.SPVAL           &
      	 .AND. T(I,J,2).LT.SPVAL .AND. Q(I,J,1).LT.SPVAL      &
      	 .AND. Q(I,J,2).LT.SPVAL)THEN
           PU=PINT(I,J,2)
           ZU=ZINT(I,J,2)
           TU=D50*(T(I,J,1)+T(I,J,2))
           QU=D50*(Q(I,J,1)+Q(I,J,2))
           QSAT=PQ0/PU*EXP(A2*(TU-A3)/(TU-A4))
           RHU =QU/QSAT
           IF(RHU.GT.H1)THEN
             RHU=H1
             QU =RHU*QSAT
           ENDIF
           IF(RHU.LT.D01)THEN
             RHU=D01
             QU =RHU*QSAT
           ENDIF
!
           TVRU=TU*(H1+D608*QU)
           TVRABV=TVRU*(PFSIGO/PU)**RGAMOG
           TABV=TVRABV/(H1+D608*QU)
           QSAT=PQ0/PFSIGO*EXP(A2*(TABV-A3)/(TABV-A4))
           QABV =RHU*QSAT
           QABV =MAX(H1M12,QABV)
!==        B    =TABV
           B    =TVRABV       !Marina Tsidulko Dec22, 2003
           AHF  =D00
           FAC  =D00
	   DONEFSL1=.TRUE.
         ELSEIF(NL1XF(I,J).EQ.LP1 .AND. T(I,J,LM-1).LT.SPVAL     &
      	 .AND. T(I,J,LM-2).LT.SPVAL .AND. Q(I,J,LM-1).LT.SPVAL   &
      	 .AND. Q(I,J,LM-2).LT.SPVAL)THEN
           PL=PINT(I,J,LM-1)
           ZL=ZINT(I,J,LM-1)
           TL=D50*(T(I,J,LM-2)+T(I,J,LM-1))
           QL=D50*(Q(I,J,LM-2)+Q(I,J,LM-1))
!
!--- RH w/r/t water for all conditions  (Ferrier, 25 Jan 02)
!
           QSAT=PQ0/PL*EXP(A2*(TL-A3)/(TL-A4))
           RHL=QL/QSAT
           IF(RHL.GT.H1)THEN
            RHL=H1
            QL =RHL*QSAT
           ENDIF
           IF(RHL.LT.D01)THEN
             RHL=D01
             QL =RHL*QSAT
           ENDIF
!
           TVRL  =TL*(H1+D608*QL)
           TVRBLO=TVRL*(PFSIGO/PL)**RGAMOG
           TBLO  =TVRBLO/(H1+D608*QL)
           QSAT=PQ0/PFSIGO*EXP(A2*(TBLO-A3)/(TBLO-A4))
           QBLO =RHL*QSAT
           QBLO =MAX(H1M12,QBLO)
!==        B    =TBLO
           B    =TVRBLO             !Marina Tsidulko Dec22, 2003
           AHF  =D00
           FAC  =D00
	   DONEFSL1=.TRUE.
         ELSEIF(T(I,J,NL1XF(I,J)).LT.SPVAL                 &
     &	 .AND. Q(I,J,NL1XF(I,J)).LT.SPVAL)THEN
!==        B     =T(I,J,NL1XF(I,J)) !Marina Tsidulko Dec22, 2003
           B     =T(I,J,NL1XF(I,J))*(H1+D608*Q(I,J,NL1XF(I,J)))
           DENOM=(ALPINT(I,J,NL1XF(I,J)+1)-ALPINT(I,J,NL1XF(I,J)-1))
!==        AHF  =(B-T(I,J,NL1XF(I,J)-1))/DENOM
           AHF  =(B-T(I,J,NL1XF(I,J)-1)*(H1+D608*Q(I,J,NL1XF(I,J)-1))) &
                   /DENOM            !Marina Tsidulko Dec22, 2003
           FAC =H2*LOG(PMID(I,J,NL1XF(I,J)))
	   DONEFSL1=.TRUE.
         END IF
!
         if(DONEFSL1)FSL1(I,J)=(PNL1-PFSIGO)/(PFSIGO+PNL1)              &
                 *((APFSIGO+ALPINT(I,J,NL1XF(I,J))-FAC)*AHF+B)*RD*H2    &
                 +ZINT(I,J,NL1XF(I,J))*G
! COMPUTE EXCHANGE COEFFICIENT ON FIRST INTERFACTE
         IF(NL1XF(I,J).LE.2 .OR. NL1XF(I,J).GT.(LLMH+1))THEN 
          AKH(I,J)=0.0
         ELSE
          FACT=(APFSIGO-LOG(PINT(I,J,LL)))/                            &
     &         (LOG(PINT(I,J,LL))-LOG(PINT(I,J,LL-1)))
! EXCH_H is on the bottom of model interfaces
          IF(EXCH_H(I,J,LL-2).LT.SPVAL .AND. EXCH_H(I,J,LL-1).LT.SPVAL) &
     &      AKH(I,J)=EXCH_H(I,J,LL-1)+(EXCH_H(I,J,LL-1)                 &
     &	    -EXCH_H(I,J,LL-2))*FACT 
	 END IF    
 167    CONTINUE
! OUTPUT FIRST LAYER GEOPOTENTIAL
!       GEOPOTENTIAL (SCALE BY GI)
        IF (IGET(205).GT.0) THEN
         IF (LVLS(1,IGET(205)).GT.0) THEN
!$omp  parallel do
           DO J=JSTA,JEND
           DO I=1,IM
             IF(FSL1(I,J).LT.SPVAL) THEN
                GRID1(I,J)=FSL1(I,J)*GI
             ELSE
                GRID1(I,J)=SPVAL
             ENDIF
           ENDDO
           ENDDO
          if(grib=='grib1')then
           ID(1:25)=0
	   ID(10)=0
           ID(11)=NINT(SIGO(1)*10000.)
           CALL GRIBIT(IGET(205),1,GRID1,IM,JM)
          elseif(grib=='grib2') then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(205))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
         ENDIF
        ENDIF

! OUTPUT FIRST INTERFACE KH Heat Diffusivity
         IF (IGET(243).GT.0) THEN  !!Air Quality (Plee Oct2003) ^^^^^
          IF (LVLS(1,IGET(243)).GT.0) THEN
!$omp  parallel do
           DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J)=AKH(I,J)
           ENDDO
           ENDDO
	  if(grib=="grib1" )then
           ID(1:25)=0
           ID(02)=129
           ID(10)=0
           ID(11)=NINT(SIGO(1)*10000.)
	   CALL GRIBIT(IGET(243),1,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(243))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif

           if(me.eq.0)print*,'output Heat Diffusivity'
         ENDIF
        ENDIF 	

!***
!***  BECAUSE SIGMA LAYERS DO NOT GO UNDERGROUND,  DO ALL
!***  INTERPOLATION ABOVE GROUND NOW.
!***
!
        DO 310 LP=1,LSIG
        NHOLD=0
!
        DO J=JSTA_2L,JEND_2U
        DO I=1,IM

!
        TSL(I,J)=SPVAL
        QSL(I,J)=SPVAL
        FSL(I,J)=SPVAL
        OSL(I,J)=SPVAL
        USL(I,J)=SPVAL
        VSL(I,J)=SPVAL
        Q2SL(I,J)=SPVAL
        C1D(I,J)=SPVAL      ! Total condensate
        QW1(I,J)=SPVAL      ! Cloud water
        QI1(I,J)=SPVAL      ! Cloud ice
        QR1(I,J)=SPVAL      ! Rain 
        QS1(I,J)=SPVAL      ! Snow (precip ice) 
	QG1(I,J)=SPVAL
        CFRSIG(I,J)=SPVAL
!
!***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER JUST BELOW
!***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
!
        NL1X(I,J)=LP1
        DO L=2,LM
        LLMH = NINT(LMH(I,J))
        PSIGO=PTSIGO+ASIGO(LP)*(PINT(I,J,LLMH+1)-PTSIGO)
        IF(NL1X(I,J).EQ.LP1.AND.PMID(I,J,L).GT.PSIGO)THEN
          NL1X(I,J)=L
        ENDIF
        ENDDO
!
!  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
!  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
!  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
!  WILL EXTRAPOLATE TO THAT POINT
!
        IF(NL1X(I,J).EQ.LP1.AND.PINT(I,J,LLMH+1).GE.PSIGO)THEN
          NL1X(I,J)=LM
        ENDIF
!
!        if(NL1X(I,J).EQ.LP1)print*,'Debug: NL1X=LP1 AT '
!     1 ,i,j,lp
        ENDDO
        ENDDO
!
!mptest        IF(NHOLD.EQ.0)GO TO 310
!
!$omp  parallel do private(i,j,ll,llmh,psigo,apsigo,fact,dum,pl,     &
!$omp &         zl,tl,ql,ai,bi,qsat,rhl,tvrl,tvrblo,tblo,tmt0,       &
!$omp &         qblo,pnl1,fac,ahf)
!hc        DO 220 NN=1,NHOLD
!hc        I=IHOLD(NN)
!hc        J=JHOLD(NN)
        DO 220 J=JSTA,JEND      ! Moorthi on Nov 26 2014
!       DO 220 J=JSTA_2L,JEND_2U
        DO 220 I=1,IM
        LL=NL1X(I,J)
!---------------------------------------------------------------------
!***  VERTICAL INTERPOLATION OF GEOPOTENTIAL, TEMPERATURE, SPECIFIC
!***  HUMIDITY, CLOUD WATER/ICE, OMEGA, WINDS, AND TKE.
!---------------------------------------------------------------------
!
!HC        IF(NL1X(I,J).LE.LM)THEN
        LLMH = NINT(LMH(I,J))
	PSIGO=PTSIGO+ASIGO(LP)*(PINT(I,J,LLMH+1)-PTSIGO) 
	APSIGO=LOG(PSIGO)
        IF(NL1X(I,J).LE.LLMH)THEN
!
!---------------------------------------------------------------------
!          INTERPOLATE LINEARLY IN LOG(P)
!***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
!***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
!***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
!---------------------------------------------------------------------
!

          FACT=(APSIGO-LOG(PMID(I,J,LL)))/                         &
     &         (LOG(PMID(I,J,LL))-LOG(PMID(I,J,LL-1)))
          TSL(I,J)=T(I,J,LL)+(T(I,J,LL)-T(I,J,LL-1))*FACT
          IF(Q(I,J,LL).LT.SPVAL .AND. Q(I,J,LL-1).LT.SPVAL)         &
     &      QSL(I,J)=Q(I,J,LL)+(Q(I,J,LL)-Q(I,J,LL-1))*FACT
	  IF(gridtype=='A')THEN
           IF(UH(I,J,LL).LT.SPVAL .AND. UH(I,J,LL-1).LT.SPVAL)      &
     &       USL(I,J)=UH(I,J,LL)+(UH(I,J,LL)-UH(I,J,LL-1))*FACT           
           IF(VH(I,J,LL).LT.SPVAL .AND. VH(I,J,LL-1).LT.SPVAL)      &
     &       VSL(I,J)=VH(I,J,LL)+(VH(I,J,LL)-VH(I,J,LL-1))*FACT
	  END IF 
          IF(OMGA(I,J,LL).LT.SPVAL .AND. OMGA(I,J,LL-1).LT.SPVAL)   &
     &      OSL(I,J)=OMGA(I,J,LL)+(OMGA(I,J,LL)-OMGA(I,J,LL-1))*FACT
          IF(Q2(I,J,LL).LT.SPVAL .AND. Q2(I,J,LL-1).LT.SPVAL)       &
     &      Q2SL(I,J)=Q2(I,J,LL)+(Q2(I,J,LL)-Q2(I,J,LL-1))*FACT      
!          FSL(I,J)=ZMID(I,J,LL)+(ZMID(I,J,LL)-ZMID(I,J,LL-1))*FACT
!          FSL(I,J)=FSL(I,J)*G
!hc          QSAT=PQ0/PSIGO
!hc     1      *EXP(A2*(TSL(I,J)-A3)/(TSL(I,J)-A4))
!
!hc          RHL=QSL(I,J)/QSAT
!
!hc          IF(RHL.GT.1.) QSL(I,J)=QSAT
!hc          IF(RHL.LT.0.01) QSL(I,J)=0.01*QSAT
          IF(Q2SL(I,J).LT.0.0) Q2SL(I,J)=0.0
!	  
!HC ADD FERRIER'S HYDROMETEOR
          IF(CWM(I,J,LL).LT.SPVAL .AND. CWM(I,J,LL-1).LT.SPVAL)    &
     &       C1D(I,J)=CWM(I,J,LL)+(CWM(I,J,LL)-CWM(I,J,LL-1))*FACT          
          C1D(I,J)=MAX(C1D(I,J),H1M12)      ! Total condensate	  
          IF(QQW(I,J,LL).LT.SPVAL .AND. QQW(I,J,LL-1).LT.SPVAL)    &
     &       QW1(I,J)=QQW(I,J,LL)+(QQW(I,J,LL)-QQW(I,J,LL-1))*FACT
          QW1(I,J)=MAX(QW1(I,J),H1M12)      ! Cloud water
          IF(QQI(I,J,LL).LT.SPVAL .AND. QQI(I,J,LL-1).LT.SPVAL)    &
     &       QI1(I,J)=QQI(I,J,LL)+(QQI(I,J,LL)-QQI(I,J,LL-1))*FACT
          QI1(I,J)=MAX(QI1(I,J),H1M12)      ! Cloud ice
          IF(QQR(I,J,LL).LT.SPVAL .AND. QQR(I,J,LL-1).LT.SPVAL)    &
     &       QR1(I,J)=QQR(I,J,LL)+(QQR(I,J,LL)-QQR(I,J,LL-1))*FACT
          QR1(I,J)=MAX(QR1(I,J),H1M12)      ! Rain 
          IF(QQS(I,J,LL).LT.SPVAL .AND. QQS(I,J,LL-1).LT.SPVAL)    &
     &       QS1(I,J)=QQS(I,J,LL)+(QQS(I,J,LL)-QQS(I,J,LL-1))*FACT
          QS1(I,J)=MAX(QS1(I,J),H1M12)      ! Snow (precip ice) 
          IF(CFR(I,J,LL).LT.SPVAL .AND. CFR(I,J,LL-1).LT.SPVAL)    &
     &       CFRSIG(I,J)=CFR(I,J,LL)+(CFR(I,J,LL)-CFR(I,J,LL-1))*FACT
          CFRSIG(I,J)=MAX(CFRSIG(I,J),H1M12)
          IF(QQS(I,J,LL).LT.SPVAL .AND. QQS(I,J,LL-1).LT.SPVAL)THEN
	   DUM=F_RimeF(I,J,LL)+(F_RimeF(I,J,LL)-F_RimeF(I,J,LL-1))*FACT
           IF(DUM .LE. 5.0)THEN
	    QG1(I,J)=H1M12
	   ELSE
	    QG1(I,J)=QS1(I,J)
	    QS1(I,J)=H1M12
	   END IF 
	  END IF  
! FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
! ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
! LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
! GOUND
        ELSE
!          ii=91
!          jj=13
!          if(i.eq.ii.and.j.eq.jj)print*,'Debug: underg extra at i,j,lp' &
!     &,   i,j,lp
	  PL=PINT(I,J,LM-1)
          ZL=ZINT(I,J,LM-1)
          TL=0.5*(T(I,J,LM-2)+T(I,J,LM-1))
          QL=0.5*(Q(I,J,LM-2)+Q(I,J,LM-1))
          AI=0.008855
          BI=1.
          TMT0=TL-A3
          IF(TMT0.LT.-20.)THEN
            AI=0.007225
            BI=0.9674
          ENDIF
          QSAT=PQ0/PL*EXP(A2*(TL-A3)/(TL-A4))
!
          RHL=QL/QSAT
!
          IF(RHL.GT.1.)THEN
            RHL=1.
            QL =RHL*QSAT
          ENDIF
!
          IF(RHL.LT.0.01)THEN
            RHL=0.01
            QL =RHL*QSAT
          ENDIF
!
          TVRL  =TL*(1.+0.608*QL)
          TVRBLO=TVRL*(PSIGO/PL)**RGAMOG
          TBLO  =TVRBLO/(1.+0.608*QL)
!     
          TMT0=TBLO-A3
          AI=0.008855
          BI=1.
          IF(TMT0.LT.-20.)THEN
            AI=0.007225
            BI=0.9674
          ENDIF
          QSAT=PQ0/PSIGO*EXP(A2*(TBLO-A3)/(TBLO-A4))
!
!          TSL(I,J)=TBLO
          QBLO     = RHL*QSAT
          QSL(I,J) = MAX(1.E-12,QBLO)
          IF(gridtype=='A')THEN
            USL(I,J) = UH(I,J,LLMH)
            VSL(I,J) = VH(I,J,LLMH)
          END IF 
          OSL(I,J)  = OMGA(I,J,LLMH)
          Q2SL(I,J) = max(0.0,0.5*(Q2(I,J,LLMH-1)+Q2(I,J,LLMH)))
          PNL1      = PINT(I,J,NL1X(I,J))
          FAC       = 0.
          AHF       = 0.0
!
!--- Set hydrometeor fields to zero below ground
          C1D(I,J)=0.
          QW1(I,J)=0.
          QI1(I,J)=0.
          QR1(I,J)=0.
          QS1(I,J)=0.
	  QG1(I,J)=0.
          CFRSIG(I,J)=0.
        END IF
  220   CONTINUE
!
! OBTAIN GEOPOTENTIAL AND KH ON INTERFACES 
       DO J=JSTA_2L,JEND_2U
       DO I=1,IM
        FSL(I,J)=SPVAL
	AKH(I,J)=SPVAL
        NL1XF(I,J)=LP1
        LLMH = NINT(LMH(I,J))
        PSIGO=PTSIGO+SIGO(LP+1)*(PINT(I,J,LLMH+1)-PTSIGO)
        DO L=1,LP1
        IF(NL1XF(I,J).EQ.LP1.AND.PINT(I,J,L).GT.PSIGO)THEN
          NL1XF(I,J)=L
        ENDIF
        ENDDO
       END DO
       END DO
!
!      DO J=JSTA_2L,JEND_2U
       DO J=JSTA,JEND          ! Moorthi on 26 Nov 2014
       DO I=1,IM
          DONEFSL1=.FALSE.
	  TSLDONE=.FALSE.
          LLMH = NINT(LMH(I,J))  
          PFSIGO=PTSIGO+SIGO(LP+1)*(PINT(I,J,LLMH+1)-PTSIGO) 
          PSIGO=PTSIGO+ASIGO(LP)*(PINT(I,J,LLMH+1)-PTSIGO)
          APFSIGO=LOG(PFSIGO)
          PNL1F=PINT(I,J,NL1XF(I,J))   
	  LL=NL1XF(I,J)
          IF(NL1XF(I,J).EQ.1 .AND. T(I,J,1).LT.SPVAL        &
     &	 .AND. T(I,J,2).LT.SPVAL .AND. Q(I,J,1).LT.SPVAL    &
     &	 .AND. Q(I,J,2).LT.SPVAL)THEN
              PU=PINT(I,J,2)
              ZU=ZINT(I,J,2)
              TU=D50*(T(I,J,1)+T(I,J,2))
              QU=D50*(Q(I,J,1)+Q(I,J,2))
!
!--- RH w/r/t water for all conditions
!
              QSAT=PQ0/PU*EXP(A2*(TU-A3)/(TU-A4))
              RHU =QU/QSAT
              IF(RHU.GT.H1)THEN
                RHU=H1
                QU =RHU*QSAT
              ENDIF
              IF(RHU.LT.D01)THEN
                RHU=D01
                QU =RHU*QSAT
              ENDIF
!
              TVRU=TU*(H1+D608*QU)
!              TVRABV=TVRU*(SPL(L)/PU)**RGAMOG
!==           TVRABV=TVRU*(PFSIGO/PU)**RGAMOG
              PX=(PFSIGO+PNL1F)*0.5      !Marina Tsidulko Dec22, 2003
              TVRABV=TVRU*(PX/PU)**RGAMOG!Marina Tsidulko Dec22, 2003
              TABV=TVRABV/(H1+D608*QU)
! ADD ADDITIONAL BLOCK FOR INTERPOLATING HEIGHT ONTO INTERFACES
!==           BF   =TABV
              BF   =TVRABV     !Marina Tsidulko Dec22, 2003
              FACF =D00
              AHFF =D00
	      DONEFSL1=.TRUE.
!
!
            ELSEIF(NL1XF(I,J).EQ.LP1 .AND. T(I,J,LM-1).LT.SPVAL    &
     &	 .AND. T(I,J,LM-2).LT.SPVAL .AND. Q(I,J,LM-1).LT.SPVAL     &
     &	 .AND. Q(I,J,LM-2).LT.SPVAL)THEN
!
!           EXTRAPOLATION AT LOWER BOUND.  THE LOWER BOUND IS
!           LM IF OLDRD=.FALSE.  IF OLDRD=.TRUE. THE LOWER
!           BOUND IS THE FIRST ATMOSPHERIC ETA LAYER.
!
              PL=PINT(I,J,LM-1)
              ZL=ZINT(I,J,LM-1)
              TL=D50*(T(I,J,LM-2)+T(I,J,LM-1))
              QL=D50*(Q(I,J,LM-2)+Q(I,J,LM-1)) 
!
!--- RH w/r/t water for all conditions  (Ferrier, 25 Jan 02)
!
              QSAT=PQ0/PL*EXP(A2*(TL-A3)/(TL-A4))
              RHL=QL/QSAT
              IF(RHL.GT.H1)THEN
               RHL=H1
               QL =RHL*QSAT
              ENDIF
              IF(RHL.LT.D01)THEN
                RHL=D01
                QL =RHL*QSAT
              ENDIF
!
              TVRL  =TL*(H1+D608*QL)
!==           TVRBLO=TVRL*(PFSIGO/PL)**RGAMOG
              PX=(PFSIGO+PNL1F)*0.5       !Marina Tsidulko Dec22, 2003
              TVRBLO=TVRL*(PX/PL)**RGAMOG !Marina Tsidulko Dec22, 2003
              TBLO  =TVRBLO/(H1+D608*QL)
! ADD ADDITIONAL BLOCK FOR INTERPOLATING HEIGHT ONTO INTERFACES
!==           BF   =TBLO
              BF   =TVRBLO    !Marina Tsidulko Dec22, 2003
              FACF =D00
              AHFF =D00
	      DONEFSL1=.TRUE.
	      TSLDONE=.TRUE.
!
!
            ELSEIF(T(I,J,NL1XF(I,J)).LT.SPVAL              &
     &	 .AND. Q(I,J,NL1XF(I,J)).LT.SPVAL)THEN
!
!           INTERPOLATION BETWEEN LOWER AND UPPER BOUNDS.
!
! ADD ADDITIONAL BLOCK FOR INTERPOLATING HEIGHT ONTO INTERFACES
!             if(NL1XF(I,J).eq.LMp1)
!    +        print*,'Debug: using bad temp at',i,j
!==           BF   =T(I,J,NL1XF(I,J))  !Marina Tsidulko Dec22, 2003
              BF   =T(I,J,NL1XF(I,J))*(H1+D608*Q(I,J,NL1XF(I,J)))
!HC              FACF =H2*LOG(PT+PDSL(I,J)*AETA(NL1XF(I,J)))
              FACF =H2*LOG(PMID(I,J,NL1XF(I,J)))
              DENOMF=(ALPINT(I,J,NL1XF(I,J)+1)-ALPINT(I,J,NL1XF(I,J)-1))
!==           AHFF=(BF-T(I,J,NL1XF(I,J)-1))/DENOMF
           AHFF=(BF-T(I,J,NL1XF(I,J)-1)*(H1+D608*Q(I,J,NL1XF(I,J)-1)))  &
                   /DENOMF             !Marina Tsidulko Dec22, 2003
!
              DONEFSL1=.TRUE.     
            ENDIF   
!
            IF(DONEFSL1)then
	     FSL(I,J)=(PNL1F-PFSIGO)/(PFSIGO+PNL1F)                   &
              *((APFSIGO+ALPINT(I,J,NL1XF(I,J))-FACF)*AHFF+BF)*RD*H2  &
                 +ZINT(I,J,NL1XF(I,J))*G
!  OBTAIN TEMPERATURE AT MID-SIGMA LAYER BASED ON HYDROSTATIC
             DPSIG=(SIGO(LP+1)-SIGO(LP))*(PINT(I,J,LLMH+1)-PTSIGO)
!           IF(J.eq.jj.and.i.eq.ii)print*,'Debug:L,OLD T= ',
!    +      L,TSL(I,J)
             IF(.NOT.TSLDONE) THEN
               TSL(I,J)=(FSL1(I,J)-FSL(I,J))*PSIGO/(RD*DPSIG)
             ENDIF
             FSL1(I,J)=FSL(I,J)
	     IF(.NOT.TSLDONE) THEN
               TV=TSL(I,J)
               TSL(I,J)=TV/(H1+D608*QSL(I,J))
             ENDIF
             QSAT=PQ0/PSIGO *EXP(A2*(TSL(I,J)-A3)/(TSL(I,J)-A4))
!
             RHL=QSL(I,J)/QSAT
!
             IF(RHL.GT.1.) QSL(I,J)=QSAT
             IF(RHL.LT.0.01) QSL(I,J)=0.01*QSAT
            END IF
!           IF(J.eq.jj.and.i.eq.ii)print*,'Debug:L,T,Q,Q2,FSL=',
!    +      L,TSL(I,J),QSL(I,J),Q2SL(I,J),FSL(I,J)

! COMPUTE EXCHANGE COEFFICIENT ON INTERFACES
           IF(NL1XF(I,J).LE.2 .OR. NL1XF(I,J).GT.(LLMH+1))THEN
             AKH(I,J)=0.0
           ELSE
            FACT=(APFSIGO-LOG(PINT(I,J,LL)))/             &
     &        (LOG(PINT(I,J,LL))-LOG(PINT(I,J,LL-1)))
! EXCH_H is on the bottom of model interfaces
            IF(EXCH_H(I,J,LL-2).LT.SPVAL .AND.             &
     &	    EXCH_H(I,J,LL-1).LT.SPVAL)                     &
     &        AKH(I,J)=EXCH_H(I,J,LL-1)+(EXCH_H(I,J,LL-1)  &
     &         -EXCH_H(I,J,LL-2))*FACT
           END IF

! END OF HYDROSTATIC TEMPERATURE DERIVATION 
       END DO
       END DO
!	
! VERTICAL INTERPOLATION FOR WIND FOR E and B GRIDS
!
        if(gridtype=='B' .or. gridtype=='E') &
	 call exch(PINT(1:IM,JSTA_2L:JEND_2U,LP1))  
        IF(gridtype=='E')THEN
        DO J=JSTA,JEND
        DO I=1,IM-MOD(J,2)
!
!***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER FOR V POINT JUST BELOW
!***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
!
	 LLMH = NINT(LMH(I,J))
         IF(J .EQ. 1 .AND. I .LT. IM)THEN   !SOUTHERN BC
           PDV=0.5*(PINT(I,J,LLMH+1)+PINT(I+1,J,LLMH+1))
         ELSE IF(J.EQ.JM .AND. I.LT.IM)THEN   !NORTHERN BC
           PDV=0.5*(PINT(I,J,LLMH+1)+PINT(I+1,J,LLMH+1))
         ELSE IF(I .EQ. 1 .AND. MOD(J,2) .EQ. 0) THEN   !WESTERN EVEN BC
           PDV=0.5*(PINT(I,J-1,LLMH+1)+PINT(I,J+1,LLMH+1))
         ELSE IF(I .EQ. IM .AND. MOD(J,2) .EQ. 0) THEN   !EASTERN EVEN BC
           PDV=0.5*(PINT(I,J-1,LLMH+1)+PINT(I,J+1,LLMH+1))
         ELSE IF (MOD(J,2) .LT. 1) THEN
           PDV=0.25*(PINT(I,J,LLMH+1)+PINT(I-1,J,LLMH+1)              &
     &       +PINT(I,J+1,LLMH+1)+PINT(I,J-1,LLMH+1))
         ELSE
           PDV=0.25*(PINT(I,J,LLMH+1)+PINT(I+1,J,LLMH+1)              &
     &       +PINT(I,J+1,LLMH+1)+PINT(I,J-1,LLMH+1))
         END IF 
         PSIGO=PTSIGO+ASIGO(LP)*(PDV-PTSIGO)
	 NL1X(I,J)=LP1 
         DO L=2,LM
          IF(NL1X(I,J).EQ.LP1.AND.PMIDV(I,J,L).GT.PSIGO)THEN
           NL1X(I,J)=L
          ENDIF
         ENDDO
!
!  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
!  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
!  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
!  WILL EXTRAPOLATE TO THAT POINT
!
         IF(NL1X(I,J).EQ.LP1.AND. PDV.GT.PSIGO)THEN	
          NL1X(I,J)=LM
         ENDIF
!
        ENDDO
        ENDDO
!
        DO 230 J=JSTA,JEND
        DO 230 I=1,IM-MOD(j,2)
         LLMH = NINT(LMH(I,J))
         IF(J .EQ. 1 .AND. I .LT. IM)THEN   !SOUTHERN BC
           PDV=0.5*(PINT(I,J,LLMH+1)+PINT(I+1,J,LLMH+1))
         ELSE IF(J.EQ.JM .AND. I.LT.IM)THEN   !NORTHERN BC
           PDV=0.5*(PINT(I,J,LLMH+1)+PINT(I+1,J,LLMH+1))
         ELSE IF(I .EQ. 1 .AND. MOD(J,2) .EQ. 0) THEN   !WESTERN EVEN BC
           PDV=0.5*(PINT(I,J-1,LLMH+1)+PINT(I,J+1,LLMH+1))
         ELSE IF(I .EQ. IM .AND. MOD(J,2) .EQ. 0) THEN   !EASTERN EVEN BC
           PDV=0.5*(PINT(I,J-1,LLMH+1)+PINT(I,J+1,LLMH+1))
         ELSE IF (MOD(J,2) .LT. 1) THEN
           PDV=0.25*(PINT(I,J,LLMH+1)+PINT(I-1,J,LLMH+1)        &
     &       +PINT(I,J+1,LLMH+1)+PINT(I,J-1,LLMH+1))
         ELSE
           PDV=0.25*(PINT(I,J,LLMH+1)+PINT(I+1,J,LLMH+1)        &
     &       +PINT(I,J+1,LLMH+1)+PINT(I,J-1,LLMH+1))
         END IF
         PSIGO=PTSIGO+ASIGO(LP)*(PDV-PTSIGO)
	 APSIGO=LOG(PSIGO)  
         LL=NL1X(I,J)
!---------------------------------------------------------------------
!***  VERTICAL INTERPOLATION OF WINDS FOR A-E GRID
!---------------------------------------------------------------------
!         
!HC        IF(NL1X(I,J).LE.LM)THEN
         LLMH = NINT(LMH(I,J))
         IF(NL1X(I,J).LE.LLMH)THEN
!
!---------------------------------------------------------------------
!          INTERPOLATE LINEARLY IN LOG(P)
!***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
!***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
!***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
!---------------------------------------------------------------------
!
	   
          FACT=(APSIGO-LOG(PMIDV(I,J,LL)))/                   &
     &         (LOG(PMIDV(I,J,LL))-LOG(PMIDV(I,J,LL-1)))
          IF(UH(I,J,LL).LT.SPVAL .AND. UH(I,J,LL-1).LT.SPVAL)  &
     &       USL(I,J)=UH(I,J,LL)+(UH(I,J,LL)-UH(I,J,LL-1))*FACT
          IF(VH(I,J,LL).LT.SPVAL .AND. VH(I,J,LL-1).LT.SPVAL)  &
     &       VSL(I,J)=VH(I,J,LL)+(VH(I,J,LL)-VH(I,J,LL-1))*FACT
!
! FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
! ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
! LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
! GOUND
        ELSE
          IF(UH(I,J,LLMH).LT.SPVAL)USL(I,J)=UH(I,J,LLMH)
	  IF(VH(I,J,LLMH).LT.SPVAL)VSL(I,J)=VH(I,J,LLMH)
        END IF
  230   CONTINUE
        JJB=JSTA 
        IF(MOD(JSTA,2).EQ.0)JJB=JSTA+1
        JJE=JEND
        IF(MOD(JEND,2).EQ.0)JJE=JEND-1
        DO J=JJB,JJE,2 !chc
          USL(IM,J)=USL(IM-1,J)
	  VSL(IM,J)=VSL(IM-1,J)
        END DO
	
	ELSE IF (gridtype=='B')THEN
         DO J=JSTA,JEND_M
         DO I=1,IM-1
!
!***  LOCATE VERTICAL INDEX OF MODEL MIDLAYER FOR V POINT JUST BELOW
!***  THE PRESSURE LEVEL TO WHICH WE ARE INTERPOLATING.
!
           PDV=0.25*(PINT(I,J,LP1)+PINT(I+1,J,LP1)                       &
             +PINT(I,J+1,LP1)+PINT(I+1,J+1,LP1))  

           PSIGO=PTSIGO+ASIGO(LP)*(PDV-PTSIGO)
	   NL1X(I,J)=LP1 
           DO L=2,LM
            IF(NL1X(I,J).EQ.LP1.AND.PMIDV(I,J,L).GT.PSIGO)THEN
             NL1X(I,J)=L
            ENDIF
           ENDDO
!
!  IF THE PRESSURE LEVEL IS BELOW THE LOWEST MODEL MIDLAYER
!  BUT STILL ABOVE THE LOWEST MODEL BOTTOM INTERFACE,
!  WE WILL NOT CONSIDER IT UNDERGROUND AND THE INTERPOLATION
!  WILL EXTRAPOLATE TO THAT POINT
!
           IF(NL1X(I,J).EQ.LP1.AND. PDV.GT.PSIGO)THEN	
             NL1X(I,J)=LM
           ENDIF
!
         ENDDO
         ENDDO
!
         DO 231 J=JSTA,JEND_M
         DO 231 I=1,IM-1
	  PDV=0.25*(PINT(I,J,LP1)+PINT(I+1,J,LP1)                       &
             +PINT(I,J+1,LP1)+PINT(I+1,J+1,LP1))
          PSIGO=PTSIGO+ASIGO(LP)*(PDV-PTSIGO)
	  APSIGO=LOG(PSIGO)  
          LL=NL1X(I,J)
!---------------------------------------------------------------------
!***  VERTICAL INTERPOLATION OF WINDS FOR A-E GRID
!---------------------------------------------------------------------
!         
!HC        IF(NL1X(I,J).LE.LM)THEN
          LLMH = NINT(LMH(I,J))
          IF(NL1X(I,J).LE.LLMH)THEN
!
!---------------------------------------------------------------------
!          INTERPOLATE LINEARLY IN LOG(P)
!***  EXTRAPOLATE ABOVE THE TOPMOST MIDLAYER OF THE MODEL
!***  INTERPOLATION BETWEEN NORMAL LOWER AND UPPER BOUNDS
!***  EXTRAPOLATE BELOW LOWEST MODEL MIDLAYER (BUT STILL ABOVE GROUND)
!---------------------------------------------------------------------
!
	   
           FACT=(APSIGO-LOG(PMIDV(I,J,LL)))/                   &
     &         (LOG(PMIDV(I,J,LL))-LOG(PMIDV(I,J,LL-1)))
           IF(UH(I,J,LL).LT.SPVAL .AND. UH(I,J,LL-1).LT.SPVAL)  &
     &        USL(I,J)=UH(I,J,LL)+(UH(I,J,LL)-UH(I,J,LL-1))*FACT
           IF(VH(I,J,LL).LT.SPVAL .AND. VH(I,J,LL-1).LT.SPVAL)  &
     &        VSL(I,J)=VH(I,J,LL)+(VH(I,J,LL)-VH(I,J,LL-1))*FACT
!
! FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
! ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
! LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
! GOUND
          ELSE
           IF(UH(I,J,LLMH).LT.SPVAL)USL(I,J)=UH(I,J,LLMH)
	   IF(VH(I,J,LLMH).LT.SPVAL)VSL(I,J)=VH(I,J,LLMH)
          END IF
  231   CONTINUE
	
	
	
        END IF  ! END OF WIND INTERPOLATION FOR NMM


!
!---------------------------------------------------------------------
!        LOAD GEOPOTENTIAL AND TEMPERATURE INTO STANDARD LEVEL 
!        ARRAYS FOR THE NEXT PASS.
!---------------------------------------------------------------------
!     
!---------------------------------------------------------------------
!***  CALCULATE 1000MB GEOPOTENTIALS CONSISTENT WITH SLP OBTAINED 
!***  FROM THE MESINGER OR NWS SHUELL SLP REDUCTION.
!---------------------------------------------------------------------
!     
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
        IF(IGET(205).GT.0)THEN
          IF(LVLS(LP+1,IGET(205)).GT.0)THEN
!$omp  parallel do
            DO J=JSTA,JEND
            DO I=1,IM
              IF(FSL(I,J).LT.SPVAL) THEN
                GRID1(I,J)=FSL(I,J)*GI
              ELSE
                GRID1(I,J)=SPVAL
              ENDIF
            ENDDO
            ENDDO
	  if(grib=="grib1" )then
            ID(1:25)=0
	    ID(10)=0
            ID(11)=NINT(SIGO(LP+1)*10000.)
            CALL GRIBIT(IGET(205),LP+1,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(205))
            fld_info(cfld)%lvl=LVLSXML(LP+1,IGET(205))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
        ENDIF
!	
!***  OUTPUT EXCHANGE COEEFICIENT
!
! OUTPUT FIRST INTERFACE KH Heat Diffusivity
         IF (IGET(243).GT.0) THEN  !!Air Quality (Plee Oct2003) ^^^^^
          IF (LVLS(LP+1,IGET(243)).GT.0) THEN
!$omp  parallel do
           DO J=JSTA,JEND
           DO I=1,IM
             GRID1(I,J)=AKH(I,J)
	     IF(LP.EQ.(LSIG+1))GRID1(I,J)=0.0  !! NO SLIP ASSUMTION FOR CMAQ
           ENDDO
           ENDDO
	  if(grib=="grib1" )then
           ID(1:25)=0
           ID(02)=129
           ID(10)=0
           ID(11)=NINT(SIGO(LP+1)*10000.)
	   CALL GRIBIT(IGET(243),LP+1,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(243))
            fld_info(cfld)%lvl=LVLSXML(LP+1,IGET(243))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
           if(me.eq.0)print*,'output Heat Diffusivity'
         ENDIF
        ENDIF 
!     
!***  TEMPERATURE
!
        IF(IGET(206).GT.0) THEN
          IF(LVLS(LP,IGET(206)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=TSL(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(206),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(206))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(206))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
        ENDIF
!     
!***  PRESSURE
!
        IF(IGET(216).GT.0)THEN
          IF(LVLS(LP,IGET(216)).GT.0)THEN
!$omp  parallel do
             DO J=JSTA,JEND
             DO I=1,IM
	       LLMH = NINT(LMH(I,J))  
               GRID1(I,J)=PTSIGO+ASIGO(LP)*(PINT(I,J,LLMH+1)-PTSIGO)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(216),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(216))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(216))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
        ENDIF
!
!***  SPECIFIC HUMIDITY.
!
        IF(IGET(207).GT.0)THEN
          IF(LVLS(LP,IGET(207)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QSL(I,J)
             ENDDO
             ENDDO
             CALL BOUND(GRID1,H1M12,H99999)
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(207),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(207))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(207))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
        ENDIF
!     
!***  OMEGA
!
        IF(IGET(210).GT.0)THEN
          IF(LVLS(LP,IGET(210)).GT.0)THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=OSL(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(210),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(210))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(210))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
        ENDIF
!     
!***  U AND/OR V WIND
!
        IF(IGET(208).GT.0.OR.IGET(209).GT.0)THEN
          IF(LVLS(LP,IGET(208)).GT.0.OR.LVLS(LP,IGET(209)).GT.0) then
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=USL(I,J)
               GRID2(I,J)=VSL(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             IF(IGET(208).GT.0) CALL GRIBIT(IGET(208),LP,GRID1,IM,JM)
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             IF(IGET(209).GT.0) CALL GRIBIT(IGET(209),LP,GRID2,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(208))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(208))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(209))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(209))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
        ENDIF
!     
!***  TURBULENT KINETIC ENERGY
!
         IF (IGET(217).GT.0) THEN
          IF (LVLS(LP,IGET(217)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=Q2SL(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
            CALL GRIBIT(IGET(217),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(217))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(217))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
         ENDIF
!     
!***  CLOUD WATER
!
         IF (IGET(211).GT.0) THEN
          IF (LVLS(LP,IGET(211)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QW1(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(211),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(211))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(211))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
         ENDIF
!
!***  CLOUD ICE 
!
         IF (IGET(212).GT.0) THEN
          IF (LVLS(LP,IGET(212)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QI1(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(212),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(212))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(212))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
         ENDIF
!
!---  RAIN
         IF (IGET(213).GT.0) THEN
          IF (LVLS(LP,IGET(213)).GT.0) THEN 
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QR1(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(213),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(213))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(213))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
         ENDIF
!
!---  SNOW
         IF (IGET(214).GT.0) THEN
          IF (LVLS(LP,IGET(214)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QS1(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(214),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(214))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(214))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
         ENDIF
!
!---  GRAUPEL
         IF (IGET(255).GT.0) THEN
          IF (LVLS(LP,IGET(255)).GT.0) THEN
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=QG1(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(255),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(255))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(255))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
         ENDIF
!
!---  TOTAL CONDENSATE
         IF (IGET(215).GT.0) THEN
          IF (LVLS(LP,IGET(215)).GT.0) THEN 
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=C1D(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
             ID(02)=129    ! Parameter Table 129
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(215),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(215))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(215))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
         ENDIF
!
!    TOTAL CLOUD COVER
         IF (IGET(222).GT.0) THEN
          IF (LVLS(LP,IGET(222)).GT.0) THEN 
             DO J=JSTA,JEND
             DO I=1,IM
               GRID1(I,J)=CFRSIG(I,J)
             ENDDO
             ENDDO
	  if(grib=="grib1" )then
             ID(1:25)=0
	     ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(222),LP,GRID1,IM,JM)
          elseif(grib=="grib2" )then
            cfld=cfld+1
            fld_info(cfld)%ifld=IAVBLFLD(IGET(222))
            fld_info(cfld)%lvl=LVLSXML(LP,IGET(222))
            datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
          endif
          ENDIF
         ENDIF
!***  END OF MAIN VERTICAL LOOP
!     
  310   CONTINUE
!***  ENDIF FOR IF TEST SEEING IF WE WANT ANY OTHER VARIABLES
!
      ENDIF
!
!
!     
!     END OF ROUTINE.
!
      RETURN
      END
