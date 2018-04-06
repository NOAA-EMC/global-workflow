       SUBROUTINE CALWXT_REVISED_POST(T,Q,PMID,PINT,HTM,LMH,PREC,ZINT,IWX)
! 
!     FILE: CALWXT.f
!     WRITTEN: 11 NOVEMBER 1993, MICHAEL BALDWIN
!     REVISIONS:
!               30 SEPT 1994-SETUP NEW DECISION TREE (M BALDWIN)
!               12 JUNE 1998-CONVERSION TO 2-D (T BLACK)
!     01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!     02-01-15  MIKE BALDWIN - WRF VERSION
!     05-07-07  BINBIN ZHOU  - ADD PREC FOR RSM
!     05-08-24  GEOFF MANIKIN - MODIFIED THE AREA REQUIREMENTS
!                TO MAKE AN ALTERNATE ALGORITHM 
!                              
!
!     ROUTINE TO COMPUTE PRECIPITATION TYPE USING A DECISION TREE
!     APPROACH THAT USES VARIABLES SUCH AS INTEGRATED WET BULB TEMP
!     BELOW FREEZING AND LOWEST LAYER TEMPERATURE
!
!     SEE BALDWIN AND CONTORNO PREPRINT FROM 13TH WEATHER ANALYSIS
!     AND FORECASTING CONFERENCE FOR MORE DETAILS
!     (OR BALDWIN ET AL, 10TH NWP CONFERENCE PREPRINT)
!
!     SINCE THE ORIGINAL VERSION OF THE ALGORITHM HAS A HIGH BIAS
!      FOR FREEZING RAIN AND SLEET, THE GOAL IS TO BALANCE THAT BIAS
!      WITH A VERSION MORE LIKELY TO PREDICT SNOW
!
     use params_mod, only: h1m12, d00, d608, h1, rog
     use ctlblk_mod, only: jsta, jend, modelname, pthresh, im, jsta_2l, jend_2u, lm,&
              lp1
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!  LIST OF VARIABLES NEEDED
!    PARAMETERS:
!      D608,ROG,H1,D00
!HC       PARAMETER(D608=0.608,ROG=287.04/9.8,H1=1.0,D00=0.0)
!
!    INPUT:
!      T,Q,PMID,HTM,LMH,PREC,ZINT
      REAL,dimension(IM,jsta_2l:jend_2u,LM), intent(in) ::  T,Q,PMID,HTM
      REAL,dimension(IM,jsta_2l:jend_2u,LP1),intent(in) ::  PINT,ZINT 
      REAL,dimension(IM,jsta_2l:jend_2u),    intent(in) ::  LMH
      REAL,dimension(IM,jsta_2l:jend_2u),    intent(in) ::  PREC
!    OUTPUT:
!      IWX - INSTANTANEOUS WEATHER TYPE.
!        ACTS LIKE A 4 BIT BINARY
!          1111 = RAIN/FREEZING RAIN/ICE PELLETS/SNOW
!          WHERE THE ONE'S DIGIT IS FOR SNOW
!                THE TWO'S DIGIT IS FOR ICE PELLETS
!                THE FOUR'S DIGIT IS FOR FREEZING RAIN
!            AND THE EIGHT'S DIGIT IS FOR RAIN
     integer, DIMENSION(IM,jsta:jend),intent(inout) ::  IWX
!    INTERNAL:
!
      REAL, ALLOCATABLE :: TWET(:,:,:)
      integer,DIMENSION(IM,jsta:jend) :: KARR,LICEE
      real,   dimension(IM,jsta:jend) :: TCOLD,TWARM
!
      integer I,J,L,LMHK,LICE,IFREL,IWRML,IFRZL
      real PSFCK,TDCHK,A,TDKL,TDPRE,TLMHK,TWRMK,AREAS8,AREAP4,AREA1,  &
           SURFW,SURFC,DZKL,PINTK1,PINTK2,PM150,QKL,TKL,PKL,AREA0,    &
           AREAP0

!    SUBROUTINES CALLED:
!     WETBULB
!     
!
!     INITIALIZE WEATHER TYPE ARRAY TO ZERO (IE, OFF).
!     WE DO THIS SINCE WE WANT IWX TO REPRESENT THE
!     INSTANTANEOUS WEATHER TYPE ON RETURN.
!     
!
!     ALLOCATE LOCAL STORAGE
!
      ALLOCATE ( TWET(IM,JSTA_2L:JEND_2U,LM) )
!
!$omp  parallel do
      DO J=JSTA,JEND
        DO I=1,IM
          IWX(I,J) = 0
        ENDDO
      ENDDO

!
!!$omp  parallel do
!!$omp& private(a,lmhk,pkl,psfck,qkl,tdchk,tdkl,tdpre,tkl)
      DO 800 J=JSTA,JEND
      DO 800 I=1,IM
      LMHK=NINT(LMH(I,J))
!
!   SKIP THIS POINT IF NO PRECIP THIS TIME STEP 
!
      IF (PREC(I,J).LE.PTHRESH) GOTO 800
!
!   FIND COLDEST AND WARMEST TEMPS IN SATURATED LAYER BETWEEN
!   70 MB ABOVE GROUND AND 500 MB
!   ALSO FIND HIGHEST SATURATED LAYER IN THAT RANGE
!
!meb
      PSFCK=PINT(I,J,LMHK+1)
!meb
      TDCHK=2.0
  760 TCOLD(I,J)=T(I,J,LMHK)
      TWARM(I,J)=T(I,J,LMHK)
      LICEE(I,J)=LMHK
!
      DO 775 L=1,LMHK
      QKL=Q(I,J,L)
      QKL=AMAX1(H1M12,QKL)
      TKL=T(I,J,L)
      PKL=PMID(I,J,L)
!
!   SKIP PAST THIS IF THE LAYER IS NOT BETWEEN 70 MB ABOVE GROUND
!       AND 500 MB
!
      IF (PKL.LT.50000.0.OR.PKL.GT.PSFCK-7000.0) GOTO 775
      A=ALOG(QKL*PKL/(610.78*(0.378*QKL+0.622)))
      TDKL=(237.3*A)/(17.269-A)+273.15
      TDPRE=TKL-TDKL
      IF (TDPRE.LT.TDCHK.AND.TKL.LT.TCOLD(I,J)) TCOLD(I,J)=TKL
      IF (TDPRE.LT.TDCHK.AND.TKL.GT.TWARM(I,J)) TWARM(I,J)=TKL
      IF (TDPRE.LT.TDCHK.AND.L.LT.LICEE(I,J)) LICEE(I,J)=L
  775 CONTINUE
!
!    IF NO SAT LAYER AT DEW POINT DEP=TDCHK, INCREASE TDCHK
!     AND START AGAIN (BUT DON'T MAKE TDCHK > 6)
!
      IF (TCOLD(I,J).EQ.T(I,J,LMHK).AND.TDCHK.LT.6.0) THEN
        TDCHK=TDCHK+2.0
        GOTO 760
      ENDIF
  800 CONTINUE
!
!    LOWEST LAYER T
!
      DO 850 J=JSTA,JEND
      DO 850 I=1,IM
      KARR(I,J)=0
      IF (PREC(I,J).LE.PTHRESH) GOTO 850
      LMHK=NINT(LMH(I,J))
      TLMHK=T(I,J,LMHK)
!
!    DECISION TREE TIME
!
      IF (TCOLD(I,J).GT.269.15) THEN
          IF (TLMHK.LE.273.15) THEN
!             TURN ON THE FLAG FOR
!             FREEZING RAIN = 4
!             IF ITS NOT ON ALREADY
!             IZR=MOD(IWX(I,J),8)/4
!             IF (IZR.LT.1) IWX(I,J)=IWX(I,J)+4
              IWX(I,J)=IWX(I,J)+4
            GOTO 850
          ELSE
!             TURN ON THE FLAG FOR
!             RAIN = 8
!             IF ITS NOT ON ALREADY
!             IRAIN=IWX(I,J)/8
!             IF (IRAIN.LT.1) IWX(I,J)=IWX(I,J)+8
              IWX(I,J)=IWX(I,J)+8
            GOTO 850
          ENDIF
      ENDIF
      KARR(I,J)=1
  850 CONTINUE
!
!   COMPUTE WET BULB ONLY AT POINTS THAT NEED IT
!
      CALL WETBULB(T,Q,PMID,HTM,KARR,TWET)
!
!!$omp  parallel do
!!$omp& private(area1,areap4,areap0,areas8,dzkl,ifrzl,iwrml,lice,
!!$omp&         lmhk,pintk1,pintk2,pm150,psfck,surfc,surfw,
!!$omp&         tlmhk,twrmk)
      DO 1900 J=JSTA,JEND
      DO 1900 I=1,IM
      IF(KARR(I,J).GT.0)THEN
        LMHK=NINT(LMH(I,J))
        LICE=LICEE(I,J)
!meb
        PSFCK=PINT(I,J,LMHK+1)
!meb
        TLMHK=T(I,J,LMHK)
        TWRMK=TWARM(I,J)
!
!    TWET AREA VARIABLES
!     CALCULATE ONLY WHAT IS NEEDED
!      FROM GROUND TO 150 MB ABOVE SURFACE
!      FROM GROUND TO TCOLD LAYER
!      AND FROM GROUND TO 1ST LAYER WHERE WET BULB T < 0.0
!
!     PINTK1 IS THE PRESSURE AT THE BOTTOM OF THE LAYER
!     PINTK2 IS THE PRESSURE AT THE TOP OF THE LAYER
!
!     AREAP4 IS THE AREA OF TWET ABOVE -4 C BELOW HIGHEST SAT LYR 
!     AREAP0 IS THE AREA OF TWET ABOVE 0 C BELOW HIGHEST SAT LYR
!
        AREAS8=D00
        AREAP4=D00
	AREAP0=D00
        SURFW =D00
        SURFC =D00
        
!
        DO 1945 L=LMHK,LICE,-1
        DZKL=ZINT(I,J,L)-ZINT(I,J,L+1)
        AREA1=(TWET(I,J,L)-269.15)*DZKL
        AREA0=(TWET(I,J,L)-273.15)*DZKL
        IF (TWET(I,J,L).GE.269.15) AREAP4=AREAP4+AREA1
        IF (TWET(I,J,L).GE.273.15) AREAP0=AREAP0+AREA0
 1945   CONTINUE
!
!        IF (AREAP4.LT.3000.0) THEN
!             TURN ON THE FLAG FOR
!             SNOW = 1
!             IF ITS NOT ON ALREADY
!             ISNO=MOD(IWX(I,J),2)
!             IF (ISNO.LT.1) IWX(I,J)=IWX(I,J)+1
!          IWX(I,J)=IWX(I,J)+1
!          GO TO 1900
!        ENDIF
        IF (AREAP0.LT.350.0) THEN
!             TURN ON THE FLAG FOR
!             SNOW = 1
              IWX(I,J)=IWX(I,J)+1
            GOTO 1900
       ENDIF
!
!     AREAS8 IS THE NET AREA OF TWET W.R.T. FREEZING IN LOWEST 150MB
!
        PINTK1=PSFCK
        PM150=PSFCK-15000.
!
        DO 1955 L=LMHK,1,-1
        PINTK2=PINT(I,J,L)
        IF(PINTK1.LT.PM150)GO TO 1950
        DZKL=ZINT(I,J,L)-ZINT(I,J,L+1)
!
!    SUM PARTIAL LAYER IF IN 150 MB AGL LAYER
!
        IF(PINTK2.LT.PM150)                                   &
          DZKL=T(I,J,L)*(Q(I,J,L)*D608+H1)*ROG*               &
               ALOG(PINTK1/PM150)
        AREA1=(TWET(I,J,L)-273.15)*DZKL
        AREAS8=AREAS8+AREA1
 1950   PINTK1=PINTK2
 1955   CONTINUE
!
!     SURFW IS THE AREA OF TWET ABOVE FREEZING BETWEEN THE GROUND
!       AND THE FIRST LAYER ABOVE GROUND BELOW FREEZING
!     SURFC IS THE AREA OF TWET BELOW FREEZING BETWEEN THE GROUND
!       AND THE WARMEST SAT LAYER
!
        IFRZL=0
        IWRML=0
!
        DO 2050 L=LMHK,1,-1
        IF (IFRZL.EQ.0.AND.T(I,J,L).LT.273.15) IFRZL=1
        IF (IWRML.EQ.0.AND.T(I,J,L).GE.TWRMK) IWRML=1
!
        IF (IWRML.EQ.0.OR.IFRZL.EQ.0) THEN
          DZKL=ZINT(I,J,L)-ZINT(I,J,L+1)
          AREA1=(TWET(I,J,L)-273.15)*DZKL
          IF(IFRZL.EQ.0.AND.TWET(I,J,L).GE.273.15)SURFW=SURFW+AREA1
          IF(IWRML.EQ.0.AND.TWET(I,J,L).LE.273.15)SURFC=SURFC+AREA1
        ENDIF
 2050   CONTINUE
        IF(SURFC.LT.-3000.0.OR.                                    &
     &    (AREAS8.LT.-3000.0.AND.SURFW.LT.50.0)) THEN
!             TURN ON THE FLAG FOR
!             ICE PELLETS = 2
!             IF ITS NOT ON ALREADY
!             IIP=MOD(IWX(I,J),4)/2
!             IF (IIP.LT.1) IWX(I,J)=IWX(I,J)+2
          IWX(I,J)=IWX(I,J)+2
          GOTO 1900
        ENDIF
!
        IF(TLMHK.LT.273.15) THEN
!             TURN ON THE FLAG FOR
!             FREEZING RAIN = 4
!             IF ITS NOT ON ALREADY
!             IZR=MOD(IWX(K),8)/4
!             IF (IZR.LT.1) IWX(K)=IWX(K)+4
          IWX(I,J)=IWX(I,J)+4
        ELSE
!             TURN ON THE FLAG FOR
!             RAIN = 8
!             IF ITS NOT ON ALREADY
!             IRAIN=IWX(K)/8
!             IF (IRAIN.LT.1) IWX(K)=IWX(K)+8
          IWX(I,J)=IWX(I,J)+8
        ENDIF
      ENDIF
 1900 CONTINUE
!      print *, 'revised check ', IWX(500,800)

!---------------------------------------------------------
      DEALLOCATE (TWET)


      RETURN
      END
