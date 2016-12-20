!#######################################################################
!-- Lookup tables for the saturation vapor pressure w/r/t water & ice --
!#######################################################################
!
      SUBROUTINE GPVS
!     ******************************************************************
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    GPVS        COMPUTE SATURATION VAPOR PRESSURE TABLE
!   AUTHOR: N PHILLIPS       W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE TABLE AS A FUNCTION OF
!   TEMPERATURE FOR THE TABLE LOOKUP FUNCTION FPVS.
!   EXACT SATURATION VAPOR PRESSURES ARE CALCULATED IN SUBPROGRAM FPVSX.
!   THE CURRENT IMPLEMENTATION COMPUTES A TABLE WITH A LENGTH
!   OF 7501 FOR TEMPERATURES RANGING FROM 180.0 TO 330.0 KELVIN.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL
!   94-12-30  IREDELL             EXPAND TABLE
!   96-02-19  HONG                ICE EFFECT
!
! USAGE:  CALL GPVS
!
! SUBPROGRAMS CALLED:
!   (FPVSX)  - INLINABLE FUNCTION TO COMPUTE SATURATION VAPOR PRESSURE
!
! COMMON BLOCKS:
!   COMPVS   - SCALING PARAMETERS AND TABLE FOR FUNCTION FPVS.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM SP
!
!$$$
!----------------------------------------------------------------------
      use svptbl_mod, only: nx, c1xpvs, c2xpvs, c1xpvs0, c2xpvs0, tbpvs, tbpvs0
!- - - - - - - - - - -- - - -- - - -- - - -- - - - - -- - - -- - - -
      implicit none
!
      real xmin,xmax,xinc,x,t
      integer jx
      real,external :: fpvsx,fpvsx0
!----------------------------------------------------------------------
      XMIN=180.0
      XMAX=330.0
      XINC=(XMAX-XMIN)/(NX-1)
      C1XPVS=1.-XMIN/XINC
      C2XPVS=1./XINC
      C1XPVS0=1.-XMIN/XINC
      C2XPVS0=1./XINC
!
      DO JX=1,NX
        X=XMIN+(JX-1)*XINC
        T=X
        TBPVS(JX)=FPVSX(T)
        TBPVS0(JX)=FPVSX0(T)
      ENDDO
! 
      RETURN
      END
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
                           FUNCTION FPVS(T)
!-----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    FPVS        COMPUTE SATURATION VAPOR PRESSURE
!   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: COMPUTE SATURATION VAPOR PRESSURE FROM THE TEMPERATURE.
!   A LINEAR INTERPOLATION IS DONE BETWEEN VALUES IN A LOOKUP TABLE
!   COMPUTED IN GPVS. SEE DOCUMENTATION FOR FPVSX FOR DETAILS.
!   INPUT VALUES OUTSIDE TABLE RANGE ARE RESET TO TABLE EXTREMA.
!   THE INTERPOLATION ACCURACY IS ALMOST 6 DECIMAL PLACES.
!   ON THE CRAY, FPVS IS ABOUT 4 TIMES FASTER THAN EXACT CALCULATION.
!   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
!   94-12-30  IREDELL             EXPAND TABLE
!   96-02-19  HONG                ICE EFFECT
!
! USAGE:   PVS=FPVS(T)
!
!   INPUT ARGUMENT LIST:
!     T        - REAL TEMPERATURE IN KELVIN
!
!   OUTPUT ARGUMENT LIST:
!     FPVS     - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
!
! COMMON BLOCKS:
!   COMPVS   - SCALING PARAMETERS AND TABLE COMPUTED IN GPVS.
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM SP
!
!$$$
!-----------------------------------------------------------------------
      use svptbl_mod, only : NX,C1XPVS,C2XPVS,TBPVS
!
      implicit none
!
!      integer,parameter::NX=7501
!      real C1XPVS,C2XPVS,TBPVS(NX)

      real T
      real XJ
      integer JX
      real FPVS
!-----------------------------------------------------------------------
      XJ=MIN(MAX(C1XPVS+C2XPVS*T,1.),FLOAT(NX))
      JX=MIN(XJ,NX-1.)
      FPVS=TBPVS(JX)+(XJ-JX)*(TBPVS(JX+1)-TBPVS(JX))
!
      RETURN
      END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                       FUNCTION FPVS0(T,NX,C1XPVS0,C2XPVS0,TBPVS0)
!-----------------------------------------------------------------------
!      use svptbl_mod, only : NX,C1XPVS0,C2XPVS0,TBPVS0
      implicit none
!
      integer NX
      real C1XPVS0,C2XPVS0,TBPVS0(NX)
     
      real T
      real XJ1
      integer JX1
      real FPVS0
!-----------------------------------------------------------------------
      XJ1=MIN(MAX(C1XPVS0+C2XPVS0*T,1.),FLOAT(NX))
      JX1=MIN(XJ1,NX-1.)
      FPVS0=TBPVS0(JX1)+(XJ1-JX1)*(TBPVS0(JX1+1)-TBPVS0(JX1))
!
      RETURN
      END
!-----------------------------------------------------------------------
!***********************************************************************
!-----------------------------------------------------------------------
                         FUNCTION FPVSX(T)
!-----------------------------------------------------------------------
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    FPVSX       COMPUTE SATURATION VAPOR PRESSURE
!   AUTHOR: N PHILLIPS            W/NP2      DATE: 30 DEC 82
!
! ABSTRACT: EXACTLY COMPUTE SATURATION VAPOR PRESSURE FROM TEMPERATURE.
!   THE WATER MODEL ASSUMES A PERFECT GAS, CONSTANT SPECIFIC HEATS
!   FOR GAS AND LIQUID, AND NEGLECTS THE VOLUME OF THE LIQUID.
!   THE MODEL DOES ACCOUNT FOR THE VARIATION OF THE LATENT HEAT
!   OF CONDENSATION WITH TEMPERATURE.  THE ICE OPTION IS NOT INCLUDED.
!   THE CLAUSIUS-CLAPEYRON EQUATION IS INTEGRATED FROM THE TRIPLE POINT
!   TO GET THE FORMULA
!       PVS=PSATK*(TR**XA)*EXP(XB*(1.-TR))
!   WHERE TR IS TTP/T AND OTHER VALUES ARE PHYSICAL CONSTANTS
!   THIS FUNCTION SHOULD BE EXPANDED INLINE IN THE CALLING ROUTINE.
!
! PROGRAM HISTORY LOG:
!   91-05-07  IREDELL             MADE INTO INLINABLE FUNCTION
!   94-12-30  IREDELL             EXACT COMPUTATION
!   96-02-19  HONG                ICE EFFECT 
!
! USAGE:   PVS=FPVSX(T)
! REFERENCE:   EMANUEL(1994),116-117
!
!   INPUT ARGUMENT LIST:
!     T        - REAL TEMPERATURE IN KELVIN
!
!   OUTPUT ARGUMENT LIST:
!     FPVSX    - REAL SATURATION VAPOR PRESSURE IN KILOPASCALS (CB)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM SP
!
!$$$
!-----------------------------------------------------------------------
    implicit none
!
    real,PARAMETER :: CP=1.0046E+3,RD=287.04,RV=4.6150E+2,              &
            TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2,                &
            CLIQ=4.1855E+3,CVAP= 1.8460E+3,CICE=2.1060E+3,HSUB=2.8340E+6
    real,PARAMETER :: PSATK=PSAT*1.E-3
    real,PARAMETER :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
    real,PARAMETER :: DLDTI=CVAP-CICE,XAI=-DLDTI/RV,XBI=XAI+HSUB/(RV*TTP)
    real :: TR, T
    real :: FPVSX
!-----------------------------------------------------------------------
    TR=TTP/T
!
    IF(T.GE.TTP)THEN
      FPVSX=PSATK*(TR**XA)*EXP(XB*(1.-TR))
    ELSE
      FPVSX=PSATK*(TR**XAI)*EXP(XBI*(1.-TR))
    ENDIF
!
    RETURN
    END
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
                        FUNCTION FPVSX0(T)
!-----------------------------------------------------------------------
    implicit none
!
    real,PARAMETER :: CP=1.0046E+3,RD=287.04,RV=4.6150E+2,            &
              TTP=2.7316E+2,HVAP=2.5000E+6,PSAT=6.1078E+2,            &
              CLIQ=4.1855E+3,CVAP=1.8460E+3,CICE=2.1060E+3,           &
              HSUB=2.8340E+6
    real,PARAMETER :: PSATK=PSAT*1.E-3
    real,PARAMETER :: DLDT=CVAP-CLIQ,XA=-DLDT/RV,XB=XA+HVAP/(RV*TTP)
    real,PARAMETER :: DLDTI=CVAP-CICE,XAI=-DLDT/RV,XBI=XA+HSUB/(RV*TTP)
    real TR
    real T,FPVSX0
!-----------------------------------------------------------------------
    TR=TTP/T
    FPVSX0=PSATK*(TR**XA)*EXP(XB*(1.-TR))
!
    RETURN
    END
