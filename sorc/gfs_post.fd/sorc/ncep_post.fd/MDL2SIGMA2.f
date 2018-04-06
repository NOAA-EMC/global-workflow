      SUBROUTINE MDL2SIGMA2
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
      use vrbls3d, only:  pint, pmid, t, zint, q
!      use vrbls2d, only:
      use masks, only: lmh
      use params_mod, only: pq0, a2, a3, a4, rgamog
      use ctlblk_mod, only: pt, jsta_2l, jend_2u, spval, lp1, lm, jsta, jend,&
                            grib, cfld, datapd, fld_info, im, jm, im_jm
      use rqstfld_mod, only: iget, lvls, id, iavblfld, lvlsxml
!     
      implicit none
!
      integer,PARAMETER :: LSIG=5
!     
!     DECLARE VARIABLES.
!     
      LOGICAL READTHK
!     REAL,dimension(im,jm) :: FSL, TSL, QSL, osl, usl, vsl, q2sl, fsl1,     &
      REAL,dimension(im,jsta_2l:jend_2u) :: TSL
      REAL,dimension(im,jm)              :: grid1
      REAL SIGO(LSIG+1),DSIGO(LSIG),ASIGO(LSIG)
!
!     INTEGER,dimension(im,jm) :: IHOLD,JHOLD,NL1X,NL1XF
      INTEGER,dimension(im,jsta_2l:jend_2u) :: NL1X
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
!     REAL,dimension(im,jm) :: C1D,QW1,QI1,QR1,qs1,qg1,akh 
!
      integer I,J,L,LL,LP,LLMH,NHOLD,II,JJ
      real PTSIGO,PSIGO,APSIGO,FACT,AI,BI,TMT0,QSAT,TVRL,  &
           TVRBLO,TBLO,QL,RHL,ZL,PL,TL
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
      IF((IGET(296).GT.0) ) THEN  !!Air Quality (Plee Oct2003)
!
!---------------------------------------------------------------------
!
!---  VERTICAL INTERPOLATION OF GEOPOTENTIAL, SPECIFIC HUMIDITY, TEMPERATURE, 
!     OMEGA, TKE, & CLOUD FIELDS.  START AT THE UPPERMOST TARGET SIGMA LEVEL.
!
        PTSIGO=PT   
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
          ASIGO( 1)=   0.7000
          ASIGO( 2)=   0.7500
          ASIGO( 3)=   0.8000
          ASIGO( 4)=   0.8500
          ASIGO( 5)=   0.9000
        END IF
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
!!$omp  parallel do
!!$omp& private(nn,i,j,ll,fact,qsat,rhl)
!hc        DO 220 NN=1,NHOLD
!hc        I=IHOLD(NN)
!hc        J=JHOLD(NN)
!         DO 220 J=JSTA,JEND
!         DO 220 J=JSTA_2L,JEND_2U
          DO 220 J=JSTA,JEND           ! Moorthi on Nov 26, 2014
            DO 220 I=1,IM
              LL=NL1X(I,J)
!---------------------------------------------------------------------
!***  VERTICAL INTERPOLATION OF GEOPOTENTIAL, TEMPERATURE, SPECIFIC
!***  HUMIDITY, CLOUD WATER/ICE, OMEGA, WINDS, AND TKE.
!---------------------------------------------------------------------
!
!HC           IF(NL1X(I,J).LE.LM)THEN
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

                FACT=(APSIGO-LOG(PMID(I,J,LL)))/                        &
     &              (LOG(PMID(I,J,LL))-LOG(PMID(I,J,LL-1)))
                TSL(I,J)=T(I,J,LL)+(T(I,J,LL)-T(I,J,LL-1))*FACT
! FOR UNDERGROUND PRESSURE LEVELS, ASSUME TEMPERATURE TO CHANGE 
! ADIABATICLY, RH TO BE THE SAME AS THE AVERAGE OF THE 2ND AND 3RD
! LAYERS FROM THE GOUND, WIND TO BE THE SAME AS THE LOWEST LEVEL ABOVE
! GOUND
              ELSE
                ii=91
                jj=13
!                if(i.eq.ii.and.j.eq.jj)                                 &
!                  print*,'Debug: underg extra at i,j,lp',i,j,lp
                PL = PINT(I,J,LM-1)
                ZL = ZINT(I,J,LM-1)
                TL = 0.5*(T(I,J,LM-2)+T(I,J,LM-1))
                QL = 0.5*(Q(I,J,LM-2)+Q(I,J,LM-1))
                TMT0  = Tl - A3
                AI    = 0.008855
                BI    = 1.
                IF(TMT0.LT.-20.)THEN
                  AI = 0.007225
                  BI = 0.9674
                ENDIF
                QSAT = PQ0/PL*EXP(A2*(TL-A3)/(TL-A4))
!
                RHL = QL/QSAT
!
                IF(RHL.GT.1.)THEN
                  RHL = 1.
                  QL  = RHL*QSAT
                ENDIF
!
                IF(RHL.LT.0.01)THEN
                  RHL = 0.01
                  QL  = RHL*QSAT
                ENDIF
!
!               print *,' tl=',tl,' ql=',ql,' i=',i,' j=',j,' pl=',pl
                TVRL   = TL*(1.+0.608*QL)
                TVRBLO = TVRL*(PSIGO/PL)**RGAMOG
                TBLO   = TVRBLO/(1.+0.608*QL)
!     
                TMT0  = TBLO-A3
                AI    = 0.008855
                BI    = 1.
                IF(TMT0.LT.-20.)THEN
                  AI = 0.007225
                  BI = 0.9674
                ENDIF
                QSAT = PQ0/PSIGO*EXP(A2*(TBLO-A3)/(TBLO-A4))
!
                TSL(I,J) = TBLO
              END IF
  220       CONTINUE

!---------------------------------------------------------------------
!        *** PART II ***
!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!        OUTPUT SELECTED FIELDS.
!     
!***  TEMPERATURE
!
        IF(IGET(296).GT.0) THEN
          IF(LVLS(LP,IGET(296)).GT.0)THEN
             DO J=JSTA,JEND
               DO I=1,IM
                 GRID1(I,J)=TSL(I,J)
               ENDDO
             ENDDO
            if(grib=='grib1')then
             ID(1:25)=0
             ID(10)=0
             ID(11)=NINT(ASIGO(LP)*10000.)
             CALL GRIBIT(IGET(296),LP,GRID1,IM,JM)
            elseif(grib=='grib2') then
             cfld=cfld+1
             fld_info(cfld)%ifld=IAVBLFLD(IGET(296))
             fld_info(cfld)%lvl=LVLSXML(LP,IGET(296))
             datapd(1:im,1:jend-jsta+1,cfld)=GRID1(1:im,jsta:jend)
            endif
          ENDIF
        ENDIF
!     
!***  END OF MAIN VERTICAL LOOP
!     
  310   CONTINUE
!***  ENDIF FOR IF TEST SEEING IF WE WANT ANY OTHER VARIABLES
!
      ENDIF
!
!     END OF ROUTINE.
!
      RETURN
      END
