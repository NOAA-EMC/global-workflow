 module utils

 private

 public :: calc_kgds
 public :: newps
 public :: newpr1
 public :: vintg
 public :: compute_delz

 contains

 subroutine compute_delz(ijm, levp, ak_in, bk_in, ps, zs, t, sphum, delz)

 implicit none
 integer, intent(in):: levp, ijm
 real,    intent(in), dimension(levp+1):: ak_in, bk_in
 real,    intent(in), dimension(ijm):: ps, zs
 real,    intent(in), dimension(ijm,levp):: t
 real,    intent(in), dimension(ijm,levp):: sphum
 real,    intent(out), dimension(ijm,levp):: delz
! Local:
 real, dimension(ijm,levp+1):: zh
 real, dimension(ijm,levp+1):: pe0, pn0
 real, dimension(levp+1) :: ak, bk
 integer i,k
 real, parameter :: GRAV   = 9.80665
 real, parameter :: RDGAS  = 287.05
 real, parameter :: RVGAS = 461.50
 real  :: zvir
 real:: grd

 print*,"COMPUTE LAYER THICKNESS."

 grd = grav/rdgas
 zvir = rvgas/rdgas - 1.
 ak = ak_in
 bk = bk_in
 ak(levp+1) = max(1.e-9, ak(levp+1))

 do i=1, ijm
   pe0(i,levp+1) = ak(levp+1)
   pn0(i,levp+1) = log(pe0(i,levp+1))
 enddo

 do k=levp,1, -1
   do i=1,ijm
     pe0(i,k) = ak(k) + bk(k)*ps(i)
     pn0(i,k) = log(pe0(i,k))
   enddo
 enddo

 do i = 1, ijm
   zh(i,1) = zs(i)
 enddo

 do k = 2, levp+1
   do i = 1, ijm
     zh(i,k) = zh(i,k-1)+t(i,k-1)*(1.+zvir*sphum(i,k-1))*     &
            (pn0(i,k-1)-pn0(i,k))/grd
   enddo
 enddo

 do k = 1, levp
   do i = 1, ijm
     delz(i,k) = zh(i,k+1) - zh(i,k)
   enddo
 enddo

 end subroutine compute_delz

 subroutine calc_kgds(idim, jdim, kgds)

 use nemsio_module

 implicit none

 integer(nemsio_intkind), intent(in)  :: idim, jdim

 integer, intent(out)                 :: kgds(200)

 kgds     = 0
 kgds(1)  = 4                       ! OCT 6 - TYPE OF GRID (GAUSSIAN)
 kgds(2)  = idim                    ! OCT 7-8 - # PTS ON LATITUDE CIRCLE
 kgds(3)  = jdim                    ! OCT 9-10 - # PTS ON LONGITUDE CIRCLE
 kgds(4)  = 90000                   ! OCT 11-13 - LAT OF ORIGIN
 kgds(5)  = 0                       ! OCT 14-16 - LON OF ORIGIN
 kgds(6)  = 128                     ! OCT 17 - RESOLUTION FLAG
 kgds(7)  = -90000                  ! OCT 18-20 - LAT OF EXTREME POINT
 kgds(8)  = nint(-360000./idim)     ! OCT 21-23 - LON OF EXTREME POINT
 kgds(9)  = nint((360.0 / float(idim))*1000.0)
                                          ! OCT 24-25 - LONGITUDE DIRECTION INCR.
 kgds(10) = jdim/2                  ! OCT 26-27 - NUMBER OF CIRCLES POLE TO EQUATOR
 kgds(12) = 255                     ! OCT 29 - RESERVED
 kgds(20) = 255                     ! OCT 5  - NOT USED, SET TO 255

 end subroutine calc_kgds

 SUBROUTINE NEWPS(ZS,PS,IM,KM,P,T,Q,ZSNEW,PSNEW)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    NEWPS       COMPUTE NEW SURFACE PRESSURE
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: COMPUTES A NEW SURFACE PRESSURE GIVEN A NEW OROGRAPHY.
!   THE NEW PRESSURE IS COMPUTED ASSUMING A HYDROSTATIC BALANCE
!   AND A CONSTANT TEMPERATURE LAPSE RATE.  BELOW GROUND, THE
!   LAPSE RATE IS ASSUMED TO BE -6.5 K/KM.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
!
! USAGE:    CALL NEWPS(ZS,PS,IM,KM,P,T,Q,ZSNEW,PSNEW)
!   INPUT ARGUMENT LIST:
!     IM           INTEGER NUMBER OF POINTS TO COMPUTE
!     ZS           REAL (IM) OLD OROGRAPHY (M)
!     PS           REAL (IM) OLD SURFACE PRESSURE (PA)
!     KM           INTEGER NUMBER OF LEVELS
!     P            REAL (IM,KM) PRESSURES (PA)
!     T            REAL (IM,KM) TEMPERATURES (K)
!     Q            REAL (IM,KM) SPECIFIC HUMIDITIES (KG/KG)
!     ZSNEW        REAL (IM) NEW OROGRAPHY (M)
!   OUTPUT ARGUMENT LIST:
!     PSNEW        REAL (IM) NEW SURFACE PRESSURE (PA)
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!C$$$
      REAL ZS(IM),PS(IM),P(IM,KM),T(IM,KM),Q(IM,KM)
      REAL ZSNEW(IM),PSNEW(IM)
      PARAMETER(BETA=-6.5E-3,EPSILON=1.E-9)
      PARAMETER(G=9.80665,RD=287.05,RV=461.50)
      PARAMETER(GOR=G/RD,FV=RV/RD-1.)
      REAL ZU(IM)
      FTV(AT,AQ)=AT*(1+FV*AQ)
      FGAM(APU,ATVU,APD,ATVD)=-GOR*LOG(ATVD/ATVU)/LOG(APD/APU)
      FZ0(AP,ATV,AZD,APD)=AZD+ATV/GOR*LOG(APD/AP)
      FZ1(AP,ATV,AZD,APD,AGAM)=AZD-ATV/AGAM*((APD/AP)**(-AGAM/GOR)-1)
      FP0(AZ,AZU,APU,ATVU)=APU*EXP(-GOR/ATVU*(AZ-AZU))
      FP1(AZ,AZU,APU,ATVU,AGAM)=APU*(1+AGAM/ATVU*(AZ-AZU))**(-GOR/AGAM)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE SURFACE PRESSURE BELOW THE ORIGINAL GROUND
      LS=0
      K=1
      GAMMA=BETA
      DO I=1,IM
          PU=P(I,K)
          TVU=FTV(T(I,K),Q(I,K))
          ZU(I)=FZ1(PU,TVU,ZS(I),PS(I),GAMMA)
          IF(ZSNEW(I).LE.ZU(I)) THEN
            PU=P(I,K)
            TVU=FTV(T(I,K),Q(I,K))
            IF(ABS(GAMMA).GT.EPSILON) THEN
              PSNEW(I)=FP1(ZSNEW(I),ZU(I),PU,TVU,GAMMA)
            ELSE
              PSNEW(I)=FP0(ZSNEW(I),ZU(I),PU,TVU)
            ENDIF
          ELSE
            PSNEW(I)=0
            LS=LS+1
          ENDIF
!       endif
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE SURFACE PRESSURE ABOVE THE ORIGINAL GROUND
      DO K=2,KM
        IF(LS.GT.0) THEN
          DO I=1,IM
            IF(PSNEW(I).EQ.0) THEN
              PU=P(I,K)
              TVU=FTV(T(I,K),Q(I,K))
              PD=P(I,K-1)
              TVD=FTV(T(I,K-1),Q(I,K-1))
              GAMMA=FGAM(PU,TVU,PD,TVD)
              IF(ABS(GAMMA).GT.EPSILON) THEN
                ZU(I)=FZ1(PU,TVU,ZU(I),PD,GAMMA)
              ELSE
                ZU(I)=FZ0(PU,TVU,ZU(I),PD)
              ENDIF
              IF(ZSNEW(I).LE.ZU(I)) THEN
                IF(ABS(GAMMA).GT.EPSILON) THEN
                  PSNEW(I)=FP1(ZSNEW(I),ZU(I),PU,TVU,GAMMA)
                ELSE
                  PSNEW(I)=FP0(ZSNEW(I),ZU(I),PU,TVU)
                ENDIF
                LS=LS-1
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE SURFACE PRESSURE OVER THE TOP
      IF(LS.GT.0) THEN
        K=KM
        GAMMA=0
        DO I=1,IM
          IF(PSNEW(I).EQ.0) THEN
            PU=P(I,K)
            TVU=FTV(T(I,K),Q(I,K))
            PSNEW(I)=FP0(ZSNEW(I),ZU(I),PU,TVU)
          ENDIF
        ENDDO
      ENDIF
 END SUBROUTINE NEWPS

 SUBROUTINE NEWPR1(IM,KM,IDVC,IDSL,NVCOORD,VCOORD,     &
                   PS,PM,DP)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    NEWPR1      COMPUTE MODEL PRESSURES                    
!   PRGMMR: JUANG          ORG: W/NMC23     DATE: 2005-04-11            
!   PRGMMR: Fanglin Yang   ORG: W/NMC23     DATE: 2006-11-28            
!   PRGMMR: S. Moorthi     ORG: NCEP/EMC    DATE: 2006-12-12            
!   PRGMMR: S. Moorthi     ORG: NCEP/EMC    DATE: 2007-01-02            
!                                                                       
! ABSTRACT: COMPUTE MODEL PRESSURES.                                    
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-04-11  HANN_MING HENRY JUANG    hybrid sigma, sigma-p, and sigma-
!                                                                       
! USAGE:    CALL NEWPR1(IM,IX,KM,KMP,IDVC,IDSL,NVCOORD,VCOORD,PP,TP,QP,P
!   INPUT ARGUMENT LIST:                                                
!     IM           INTEGER NUMBER OF POINTS TO COMPUTE                  
!     KM           INTEGER NUMBER OF LEVELS                             
!     IDVC         INTEGER VERTICAL COORDINATE ID                       
!                  (1 FOR SIGMA AND 2 FOR HYBRID)                       
!     IDSL         INTEGER TYPE OF SIGMA STRUCTURE                      
!                  (1 FOR PHILLIPS OR 2 FOR MEAN)                       
!     NVCOORD      INTEGER NUMBER OF VERTICAL COORDINATES               
!     VCOORD       REAL (KM+1,NVCOORD) VERTICAL COORDINATE VALUES       
!                  FOR IDVC=1, NVCOORD=1: SIGMA INTERFACE               
!                  FOR IDVC=2, NVCOORD=2: HYBRID INTERFACE A AND B      
!                  FOR IDVC=3, NVCOORD=3: JUANG GENERAL HYBRID INTERFACE
!                     AK  REAL (KM+1) HYBRID INTERFACE A                
!                     BK  REAL (KM+1) HYBRID INTERFACE B                
!     PS           REAL (IX) SURFACE PRESSURE (PA)                      
!   OUTPUT ARGUMENT LIST:                                               
!     PM           REAL (IX,KM) MID-LAYER PRESSURE (PA)                 
!     DP           REAL (IX,KM) LAYER DELTA PRESSURE (PA)               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
 IMPLICIT NONE 

 INTEGER, INTENT(IN)     :: IM, KM, NVCOORD, IDVC, IDSL

 REAL, INTENT(IN)        :: VCOORD(KM+1,NVCOORD)
 REAL, INTENT(IN)        :: PS(IM)

 REAL, INTENT(OUT)       :: PM(IM,KM)
 REAL, OPTIONAL, INTENT(OUT) :: DP(IM,KM)

 REAL, PARAMETER :: RD=287.05,  RV=461.50,    CP=1004.6,           &
                    ROCP=RD/CP, ROCP1=ROCP+1, ROCPR=1/ROCP,        &
                   FV=RV/RD-1.                                    

 INTEGER                 :: I, K

 REAL                    :: AK(KM+1), BK(KM+1), PI(IM,KM+1)
                
 IF(IDVC.EQ.2) THEN 
   DO K=1,KM+1 
     AK(K)      = VCOORD(K,1) 
     BK(K)      = VCOORD(K,2) 
     PI(1:IM,K) = AK(K) + BK(K)*PS(1:IM) 
   ENDDO 
 ELSE 
   print*,'routine only works for idvc 2'
   stop
 ENDIF

 IF(IDSL.EQ.2) THEN 
   DO K=1,KM 
     PM(1:IM,K) = (PI(1:IM,K)+PI(1:IM,K+1))/2 
   ENDDO 
 ELSE 
   DO K=1,KM 
     PM(1:IM,K) = ((PI(1:IM,K)**ROCP1-PI(1:IM,K+1)**ROCP1)/        &
                   (ROCP1*(PI(1:IM,K)-PI(1:IM,K+1))))**ROCPR        
   ENDDO 
 ENDIF 

 IF(PRESENT(DP))THEN
   DO K=1,KM 
     DO I=1,IM 
       DP(I,K) = PI(I,K) - PI(I,K+1) 
     ENDDO 
   ENDDO 
 ENDIF

 END SUBROUTINE NEWPR1 

 SUBROUTINE TERP3(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,             &
                  KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2,J2)      
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    TERP3       CUBICALLY INTERPOLATE IN ONE DIMENSION     
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01            
!                                                                       
! ABSTRACT: INTERPOLATE FIELD(S) IN ONE DIMENSION ALONG THE COLUMN(S).  
!   THE INTERPOLATION IS CUBIC LAGRANGIAN WITH A MONOTONIC CONSTRAINT   
!   IN THE CENTER OF THE DOMAIN.  IN THE OUTER INTERVALS IT IS LINEAR.  
!   OUTSIDE THE DOMAIN, FIELDS ARE HELD CONSTANT.                       
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   98-05-01  MARK IREDELL                                              
! 1999-01-04  IREDELL  USE ESSL SEARCH                                  
!                                                                       
! USAGE:    CALL TERP3(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,             
!    &                 KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2,J2)      
!   INPUT ARGUMENT LIST:                                                
!     IM           INTEGER NUMBER OF COLUMNS                            
!     IXZ1         INTEGER COLUMN SKIP NUMBER FOR Z1                    
!     IXQ1         INTEGER COLUMN SKIP NUMBER FOR Q1                    
!     IXZ2         INTEGER COLUMN SKIP NUMBER FOR Z2                    
!     IXQ2         INTEGER COLUMN SKIP NUMBER FOR Q2                    
!     NM           INTEGER NUMBER OF FIELDS PER COLUMN                  
!     NXQ1         INTEGER FIELD SKIP NUMBER FOR Q1                     
!     NXQ2         INTEGER FIELD SKIP NUMBER FOR Q2                     
!     KM1          INTEGER NUMBER OF INPUT POINTS                       
!     KXZ1         INTEGER POINT SKIP NUMBER FOR Z1                     
!     KXQ1         INTEGER POINT SKIP NUMBER FOR Q1                     
!     Z1           REAL (1+(IM-1)*IXZ1+(KM1-1)*KXZ1)                    
!                  INPUT COORDINATE VALUES IN WHICH TO INTERPOLATE      
!                  (Z1 MUST BE STRICTLY MONOTONIC IN EITHER DIRECTION)  
!     Q1           REAL (1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1)        
!                  INPUT FIELDS TO INTERPOLATE                          
!     KM2          INTEGER NUMBER OF OUTPUT POINTS                      
!     KXZ2         INTEGER POINT SKIP NUMBER FOR Z2                     
!     KXQ2         INTEGER POINT SKIP NUMBER FOR Q2                     
!     Z2           REAL (1+(IM-1)*IXZ2+(KM2-1)*KXZ2)                    
!                  OUTPUT COORDINATE VALUES TO WHICH TO INTERPOLATE     
!                  (Z2 NEED NOT BE MONOTONIC)                           
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     Q2           REAL (1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)        
!                  OUTPUT INTERPOLATED FIELDS                           
!     J2           REAL (1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2)        
!                  OUTPUT INTERPOLATED FIELDS CHANGE WRT Z2             
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   RSEARCH      SEARCH FOR A SURROUNDING REAL INTERVAL                 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      INTEGER IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2 
      INTEGER KM1,KXZ1,KXQ1,KM2,KXZ2,KXQ2 
      INTEGER I,K1,K2,N 
      REAL Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1) 
      REAL Q1(1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1) 
      REAL Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2) 
      REAL Q2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2) 
      REAL J2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2) 
      REAL FFA(IM),FFB(IM),FFC(IM),FFD(IM) 
      REAL GGA(IM),GGB(IM),GGC(IM),GGD(IM) 
      INTEGER K1S(IM,KM2) 
      REAL Z1A,Z1B,Z1C,Z1D,Q1A,Q1B,Q1C,Q1D,Z2S,Q2S,J2S 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.           
      CALL RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,1,IM,K1S) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  GENERALLY INTERPOLATE CUBICALLY WITH MONOTONIC CONSTRAINT            
!  FROM TWO NEAREST INPUT POINTS ON EITHER SIDE OF THE OUTPUT POINT,    
!  BUT WITHIN THE TWO EDGE INTERVALS INTERPOLATE LINEARLY.              
!  KEEP THE OUTPUT FIELDS CONSTANT OUTSIDE THE INPUT DOMAIN.            
                                                                        
!!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(IM,IXZ1,IXQ1,IXZ2), &
!!$OMP& SHARED(IXQ2,NM,NXQ1,NXQ2,KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2), &
!!$OMP& SHARED(KXQ2,Z2,Q2,J2,K1S)
                                                                        
      DO K2=1,KM2 
        DO I=1,IM 
          K1=K1S(I,K2) 
          IF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN 
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2) 
            Z1A=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1) 
            Z1B=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1) 
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B) 
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A) 
            GGA(I)=1/(Z1A-Z1B) 
            GGB(I)=1/(Z1B-Z1A) 
          ELSEIF(K1.GT.1.AND.K1.LT.KM1-1) THEN 
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2) 
            Z1A=Z1(1+(I-1)*IXZ1+(K1-2)*KXZ1) 
            Z1B=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1) 
            Z1C=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1) 
            Z1D=Z1(1+(I-1)*IXZ1+(K1+1)*KXZ1) 
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B)*                                 &
                   (Z2S-Z1C)/(Z1A-Z1C)*                                 &
                   (Z2S-Z1D)/(Z1A-Z1D)                                  
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A)*                                 &
                   (Z2S-Z1C)/(Z1B-Z1C)*                                 &
                   (Z2S-Z1D)/(Z1B-Z1D)                                  
            FFC(I)=(Z2S-Z1A)/(Z1C-Z1A)*                                 &
                   (Z2S-Z1B)/(Z1C-Z1B)*                                 &
                   (Z2S-Z1D)/(Z1C-Z1D)                                  
            FFD(I)=(Z2S-Z1A)/(Z1D-Z1A)*                                 &
                   (Z2S-Z1B)/(Z1D-Z1B)*                                 &
                   (Z2S-Z1C)/(Z1D-Z1C)                                  
            GGA(I)=        1/(Z1A-Z1B)*                                 &
                   (Z2S-Z1C)/(Z1A-Z1C)*                                 &
                   (Z2S-Z1D)/(Z1A-Z1D)+                                 &
                   (Z2S-Z1B)/(Z1A-Z1B)*                                 &
                           1/(Z1A-Z1C)*                                 &
                   (Z2S-Z1D)/(Z1A-Z1D)+                                 &
                   (Z2S-Z1B)/(Z1A-Z1B)*                                 &
                   (Z2S-Z1C)/(Z1A-Z1C)*                                 &
                           1/(Z1A-Z1D)                                  
            GGB(I)=        1/(Z1B-Z1A)*                                 &
                   (Z2S-Z1C)/(Z1B-Z1C)*                                 &
                   (Z2S-Z1D)/(Z1B-Z1D)+                                 &
                   (Z2S-Z1A)/(Z1B-Z1A)*                                 &
                           1/(Z1B-Z1C)*                                 &
                   (Z2S-Z1D)/(Z1B-Z1D)+                                 &
                   (Z2S-Z1A)/(Z1B-Z1A)*                                 &
                   (Z2S-Z1C)/(Z1B-Z1C)*                                 &
                           1/(Z1B-Z1D)                                  
            GGC(I)=        1/(Z1C-Z1A)*                                 &
                   (Z2S-Z1B)/(Z1C-Z1B)*                                 &
                   (Z2S-Z1D)/(Z1C-Z1D)+                                 &
                   (Z2S-Z1A)/(Z1C-Z1A)*                                 &
                           1/(Z1C-Z1B)*                                 &
                   (Z2S-Z1D)/(Z1C-Z1D)+                                 &
                   (Z2S-Z1A)/(Z1C-Z1A)*                                 &
                   (Z2S-Z1B)/(Z1C-Z1B)*                                 &
                           1/(Z1C-Z1D)                                  
            GGD(I)=        1/(Z1D-Z1A)*                                 &
                   (Z2S-Z1B)/(Z1D-Z1B)*                                 &
                   (Z2S-Z1C)/(Z1D-Z1C)+                                 &
                   (Z2S-Z1A)/(Z1D-Z1A)*                                 &
                           1/(Z1D-Z1B)*                                 &
                   (Z2S-Z1C)/(Z1D-Z1C)+                                 &
                   (Z2S-Z1A)/(Z1D-Z1A)*                                 &
                   (Z2S-Z1B)/(Z1D-Z1B)*                                 &
                           1/(Z1D-Z1C)                                  
          ENDIF 
        ENDDO 
!  INTERPOLATE.                                                         
        DO N=1,NM 
          DO I=1,IM 
            K1=K1S(I,K2) 
            IF(K1.EQ.0) THEN 
              Q2S=Q1(1+(I-1)*IXQ1+(N-1)*NXQ1) 
              J2S=0 
            ELSEIF(K1.EQ.KM1) THEN 
              Q2S=Q1(1+(I-1)*IXQ1+(KM1-1)*KXQ1+(N-1)*NXQ1) 
              J2S=0 
            ELSEIF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN 
              Q1A=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1) 
              Q1B=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1) 
              Q2S=FFA(I)*Q1A+FFB(I)*Q1B 
              J2S=GGA(I)*Q1A+GGB(I)*Q1B 
            ELSE 
              Q1A=Q1(1+(I-1)*IXQ1+(K1-2)*KXQ1+(N-1)*NXQ1) 
              Q1B=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1) 
              Q1C=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1) 
              Q1D=Q1(1+(I-1)*IXQ1+(K1+1)*KXQ1+(N-1)*NXQ1) 
              Q2S=FFA(I)*Q1A+FFB(I)*Q1B+FFC(I)*Q1C+FFD(I)*Q1D 
              J2S=GGA(I)*Q1A+GGB(I)*Q1B+GGC(I)*Q1C+GGD(I)*Q1D 
              IF(Q2S.LT.MIN(Q1B,Q1C)) THEN 
                Q2S=MIN(Q1B,Q1C) 
                J2S=0 
              ELSEIF(Q2S.GT.MAX(Q1B,Q1C)) THEN 
                Q2S=MAX(Q1B,Q1C) 
                J2S=0 
              ENDIF 
            ENDIF 
            Q2(1+(I-1)*IXQ2+(K2-1)*KXQ2+(N-1)*NXQ2)=Q2S 
            J2(1+(I-1)*IXQ2+(K2-1)*KXQ2+(N-1)*NXQ2)=J2S 
          ENDDO 
        ENDDO 
      ENDDO 
!!$OMP END PARALLEL DO                                                   

 END SUBROUTINE TERP3 

 SUBROUTINE RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,IXL2,KXL2,&
                         L2)                                            
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    RSEARCH     SEARCH FOR A SURROUNDING REAL INTERVAL     
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-05-01            
!                                                                       
! ABSTRACT: THIS SUBPROGRAM SEARCHES MONOTONIC SEQUENCES OF REAL NUMBERS
!   FOR INTERVALS THAT SURROUND A GIVEN SEARCH SET OF REAL NUMBERS.     
!   THE SEQUENCES MAY BE MONOTONIC IN EITHER DIRECTION; THE REAL NUMBERS
!   MAY BE SINGLE OR DOUBLE PRECISION; THE INPUT SEQUENCES AND SETS     
!   AND THE OUTPUT LOCATIONS MAY BE ARBITRARILY DIMENSIONED.            
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 1999-01-05  MARK IREDELL                                              
!                                                                       
! USAGE:    CALL RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,IXL2,KXL2,
!    &                   L2)                                            
!   INPUT ARGUMENT LIST:                                                
!     IM           INTEGER NUMBER OF SEQUENCES TO SEARCH                
!     KM1          INTEGER NUMBER OF POINTS IN EACH SEQUENCE            
!     IXZ1         INTEGER SEQUENCE SKIP NUMBER FOR Z1                  
!     KXZ1         INTEGER POINT SKIP NUMBER FOR Z1                     
!     Z1           REAL (1+(IM-1)*IXZ1+(KM1-1)*KXZ1)                    
!                  SEQUENCE VALUES TO SEARCH                            
!                  (Z1 MUST BE MONOTONIC IN EITHER DIRECTION)           
!     KM2          INTEGER NUMBER OF POINTS TO SEARCH FOR               
!                  IN EACH RESPECTIVE SEQUENCE                          
!     IXZ2         INTEGER SEQUENCE SKIP NUMBER FOR Z2                  
!     KXZ2         INTEGER POINT SKIP NUMBER FOR Z2                     
!     Z2           REAL (1+(IM-1)*IXZ2+(KM2-1)*KXZ2)                    
!                  SET OF VALUES TO SEARCH FOR                          
!                  (Z2 NEED NOT BE MONOTONIC)                           
!     IXL2         INTEGER SEQUENCE SKIP NUMBER FOR L2                  
!     KXL2         INTEGER POINT SKIP NUMBER FOR L2                     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     L2           INTEGER (1+(IM-1)*IXL2+(KM2-1)*KXL2)                 
!                  INTERVAL LOCATIONS HAVING VALUES FROM 0 TO KM1       
!                  (Z2 WILL BE BETWEEN Z1(L2) AND Z1(L2+1))             
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   SBSRCH       ESSL BINARY SEARCH                                     
!   DBSRCH       ESSL BINARY SEARCH                                     
!                                                                       
! REMARKS:                                                              
!   IF THE ARRAY Z1 IS DIMENSIONED (IM,KM1), THEN THE SKIP NUMBERS ARE  
!   IXZ1=1 AND KXZ1=IM; IF IT IS DIMENSIONED (KM1,IM), THEN THE SKIP    
!   NUMBERS ARE IXZ1=KM1 AND KXZ1=1; IF IT IS DIMENSIONED (IM,JM,KM1),  
!   THEN THE SKIP NUMBERS ARE IXZ1=1 AND KXZ1=IM*JM; ETCETERA.          
!   SIMILAR EXAMPLES APPLY TO THE SKIP NUMBERS FOR Z2 AND L2.           
!                                                                       
!   RETURNED VALUES OF 0 OR KM1 INDICATE THAT THE GIVEN SEARCH VALUE    
!   IS OUTSIDE THE RANGE OF THE SEQUENCE.                               
!                                                                       
!   IF A SEARCH VALUE IS IDENTICAL TO ONE OF THE SEQUENCE VALUES        
!   THEN THE LOCATION RETURNED POINTS TO THE IDENTICAL VALUE.           
!   IF THE SEQUENCE IS NOT STRICTLY MONOTONIC AND A SEARCH VALUE IS     
!   IDENTICAL TO MORE THAN ONE OF THE SEQUENCE VALUES, THEN THE         
!   LOCATION RETURNED MAY POINT TO ANY OF THE IDENTICAL VALUES.         
!                                                                       
!   TO BE EXACT, FOR EACH I FROM 1 TO IM AND FOR EACH K FROM 1 TO KM2,  
!   Z=Z2(1+(I-1)*IXZ2+(K-1)*KXZ2) IS THE SEARCH VALUE AND               
!   L=L2(1+(I-1)*IXL2+(K-1)*KXL2) IS THE LOCATION RETURNED.             
!   IF L=0, THEN Z IS LESS THAN THE START POINT Z1(1+(I-1)*IXZ1)        
!   FOR ASCENDING SEQUENCES (OR GREATER THAN FOR DESCENDING SEQUENCES). 
!   IF L=KM1, THEN Z IS GREATER THAN OR EQUAL TO THE END POINT          
!   Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1) FOR ASCENDING SEQUENCES               
!   (OR LESS THAN OR EQUAL TO FOR DESCENDING SEQUENCES).                
!   OTHERWISE Z IS BETWEEN THE VALUES Z1(1+(I-1)*IXZ1+(L-1)*KXZ1) AND   
!   Z1(1+(I-1)*IXZ1+(L-0)*KXZ1) AND MAY EQUAL THE FORMER.               
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
!     IMPLICIT NONE                                                     
!     INTEGER,INTENT(IN):: IM,KM1,IXZ1,KXZ1,KM2,IXZ2,KXZ2,IXL2,KXL2     
!     REAL,INTENT(IN):: Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1)                  
!     REAL,INTENT(IN):: Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2)                  
!     INTEGER,INTENT(OUT):: L2(1+(IM-1)*IXL2+(KM2-1)*KXL2)              
!     INTEGER(4) INCX,N,INCY,M,INDX(KM2),RC(KM2),IOPT                   
!     INTEGER I,K2                                                      
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.           
!     DO I=1,IM                                                         
!       IF(Z1(1+(I-1)*IXZ1).LE.Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1)) THEN      
!  INPUT COORDINATE IS MONOTONICALLY ASCENDING.                         
!         INCX=KXZ2                                                     
!         N=KM2                                                         
!         INCY=KXZ1                                                     
!         M=KM1                                                         
!         IOPT=1                                                        
!         IF(DIGITS(1.).LT.DIGITS(1._8)) THEN                           
!           CALL SBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,                        
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)           
!         ELSE                                                          
!           CALL DBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,                        
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)           
!         ENDIF                                                         
!         DO K2=1,KM2                                                   
!           L2(1+(I-1)*IXL2+(K2-1)*KXL2)=INDX(K2)-RC(K2)                
!         ENDDO                                                         
!       ELSE                                                            
!  INPUT COORDINATE IS MONOTONICALLY DESCENDING.                        
!         INCX=KXZ2                                                     
!         N=KM2                                                         
!         INCY=-KXZ1                                                    
!         M=KM1                                                         
!         IOPT=0                                                        
!         IF(DIGITS(1.).LT.DIGITS(1._8)) THEN                           
!           CALL SBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,                        
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)           
!         ELSE                                                          
!           CALL DBSRCH(Z2(1+(I-1)*IXZ2),INCX,N,                        
!    &                  Z1(1+(I-1)*IXZ1),INCY,M,INDX,RC,IOPT)           
!         ENDIF                                                         
!         DO K2=1,KM2                                                   
!           L2(1+(I-1)*IXL2+(K2-1)*KXL2)=KM1+1-INDX(K2)                 
!         ENDDO                                                         
!       ENDIF                                                           
!     ENDDO                                                             
!                                                                       
      IMPLICIT NONE 
      INTEGER,INTENT(IN):: IM,KM1,IXZ1,KXZ1,KM2,IXZ2,KXZ2,IXL2,KXL2 
      REAL,INTENT(IN):: Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1) 
      REAL,INTENT(IN):: Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2) 
      INTEGER,INTENT(OUT):: L2(1+(IM-1)*IXL2+(KM2-1)*KXL2) 
      INTEGER I,K2,L 
      REAL Z 
!C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!C  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.          
      DO I=1,IM 
        IF(Z1(1+(I-1)*IXZ1).LE.Z1(1+(I-1)*IXZ1+(KM1-1)*KXZ1)) THEN 
!C  INPUT COORDINATE IS MONOTONICALLY ASCENDING.                        
          DO K2=1,KM2 
            Z=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2) 
            L=0 
            DO 
              IF(Z.LT.Z1(1+(I-1)*IXZ1+L*KXZ1)) EXIT 
              L=L+1 
              IF(L.EQ.KM1) EXIT 
            ENDDO 
            L2(1+(I-1)*IXL2+(K2-1)*KXL2)=L 
          ENDDO 
        ELSE 
!C  INPUT COORDINATE IS MONOTONICALLY DESCENDING.                       
          DO K2=1,KM2 
            Z=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2) 
            L=0 
            DO 
              IF(Z.GT.Z1(1+(I-1)*IXZ1+L*KXZ1)) EXIT 
              L=L+1 
              IF(L.EQ.KM1) EXIT 
            ENDDO 
            L2(1+(I-1)*IXL2+(K2-1)*KXL2)=L 
          ENDDO 
        ENDIF 
      ENDDO 
                                                                        
 END SUBROUTINE RSEARCH 

 SUBROUTINE VINTG(IM,KM1,KM2,NT,P1,U1,V1,T1,Q1,W1,P2, &
                  U2,V2,T2,Q2,W2)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!
! SUBPROGRAM:    VINTG       VERTICALLY INTERPOLATE UPPER-AIR FIELDS
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31
!
! ABSTRACT: VERTICALLY INTERPOLATE UPPER-AIR FIELDS.
!   WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS ARE INTERPOLATED.
!   THE INTERPOLATION IS CUBIC LAGRANGIAN IN LOG PRESSURE
!   WITH A MONOTONIC CONSTRAINT IN THE CENTER OF THE DOMAIN.
!   IN THE OUTER INTERVALS IT IS LINEAR IN LOG PRESSURE.
!   OUTSIDE THE DOMAIN, FIELDS ARE GENERALLY HELD CONSTANT,
!   EXCEPT FOR TEMPERATURE AND HUMIDITY BELOW THE INPUT DOMAIN,
!   WHERE THE TEMPERATURE LAPSE RATE IS HELD FIXED AT -6.5 K/KM AND
!   THE RELATIVE HUMIDITY IS HELD CONSTANT.
!
! PROGRAM HISTORY LOG:
!   91-10-31  MARK IREDELL
!
! USAGE:    CALL VINTG(IM,KM1,KM2,NT,P1,U1,V1,T1,Q1,P2,
!    &                 U2,V2,T2,Q2)
!   INPUT ARGUMENT LIST:
!     IM           INTEGER NUMBER OF POINTS TO COMPUTE
!     KM1          INTEGER NUMBER OF INPUT LEVELS
!     KM2          INTEGER NUMBER OF OUTPUT LEVELS
!     NT           INTEGER NUMBER OF TRACERS
!     P1           REAL (IM,KM1) INPUT PRESSURES
!                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE
!     U1           REAL (IM,KM1) INPUT ZONAL WIND
!     V1           REAL (IM,KM1) INPUT MERIDIONAL WIND
!     T1           REAL (IM,KM1) INPUT TEMPERATURE (K)
!     Q1           REAL (IM,KM1,NT) INPUT TRACERS (HUMIDITY FIRST)
!     P2           REAL (IM,KM2) OUTPUT PRESSURES
!   OUTPUT ARGUMENT LIST:
!     U2           REAL (IM,KM2) OUTPUT ZONAL WIND
!     V2           REAL (IM,KM2) OUTPUT MERIDIONAL WIND
!     T2           REAL (IM,KM2) OUTPUT TEMPERATURE (K)
!     Q2           REAL (IM,KM2,NT) OUTPUT TRACERS (HUMIDITY FIRST)
!
! SUBPROGRAMS CALLED:
!   TERP3        CUBICALLY INTERPOLATE IN ONE DIMENSION
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN
!
!C$$$
      IMPLICIT NONE

      INTEGER, INTENT(IN)      :: IM, KM1, KM2, NT

      REAL,    INTENT(IN)      :: P1(IM,KM1),U1(IM,KM1),V1(IM,KM1)
      REAL,    INTENT(IN)      :: T1(IM,KM1),Q1(IM,KM1,NT)
      REAL,    INTENT(IN)      :: W1(IM,KM1),P2(IM,KM2)
      REAL,    INTENT(OUT)     :: U2(IM,KM2),V2(IM,KM2)
      REAL,    INTENT(OUT)     :: T2(IM,KM2),Q2(IM,KM2,NT)
      REAL,    INTENT(OUT)     :: W2(IM,KM2)

      REAL, PARAMETER          :: DLTDZ=-6.5E-3*287.05/9.80665
      REAL, PARAMETER          :: DLPVDRT=-2.5E6/461.50

      INTEGER                  :: I, K, N

      REAL                     :: DZ
      REAL,ALLOCATABLE         :: Z1(:,:),Z2(:,:)
      REAL,ALLOCATABLE         :: C1(:,:,:),C2(:,:,:),J2(:,:,:)
 
      ALLOCATE (Z1(IM+1,KM1),Z2(IM+1,KM2))
      ALLOCATE (C1(IM+1,KM1,4+NT),C2(IM+1,KM2,4+NT),J2(IM+1,KM2,4+NT))
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE
!  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
!$OMP PARALLEL DO PRIVATE(K,I)
      DO K=1,KM1
        DO I=1,IM
          Z1(I,K)   = -LOG(P1(I,K))
          C1(I,K,1) =  U1(I,K)
          C1(I,K,2) =  V1(I,K)
          C1(I,K,3) =  W1(I,K)
          C1(I,K,4) =  T1(I,K)
          C1(I,K,5) =  Q1(I,K,1)
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
      DO N=2,NT
        DO K=1,KM1
          DO I=1,IM
            C1(I,K,4+N) = Q1(I,K,N)
          ENDDO
        ENDDO
      ENDDO
!$OMP PARALLEL DO PRIVATE(K,I)
      DO K=1,KM2
        DO I=1,IM
          Z2(I,K) = -LOG(P2(I,K))
        ENDDO
      ENDDO
!$OMP END PARALLEL DO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  PERFORM LAGRANGIAN ONE-DIMENSIONAL INTERPOLATION
!  THAT IS 4TH-ORDER IN INTERIOR, 2ND-ORDER IN OUTSIDE INTERVALS
!  AND 1ST-ORDER FOR EXTRAPOLATION.
      CALL TERP3(IM,1,1,1,1,4+NT,(IM+1)*KM1,(IM+1)*KM2, &
                 KM1,IM+1,IM+1,Z1,C1,KM2,IM+1,IM+1,Z2,C2,J2)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  COPY OUTPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS
!  EXCEPT BELOW THE INPUT DOMAIN, LET TEMPERATURE INCREASE WITH A FIXED
!  LAPSE RATE AND LET THE RELATIVE HUMIDITY REMAIN CONSTANT.
      DO K=1,KM2
        DO I=1,IM
          U2(I,K)=C2(I,K,1)
          V2(I,K)=C2(I,K,2)
          W2(I,K)=C2(I,K,3)
          DZ=Z2(I,K)-Z1(I,1)
          IF(DZ.GE.0) THEN
            T2(I,K)=C2(I,K,4)
            Q2(I,K,1)=C2(I,K,5)
          ELSE
            T2(I,K)=T1(I,1)*EXP(DLTDZ*DZ)
            Q2(I,K,1)=Q1(I,1,1)*EXP(DLPVDRT*(1/T2(I,K)-1/T1(I,1))-DZ)
          ENDIF
        ENDDO
      ENDDO
      DO N=2,NT
        DO K=1,KM2
          DO I=1,IM
            Q2(I,K,N)=C2(I,K,4+N)
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE (Z1,Z2,C1,C2,J2)
 END SUBROUTINE VINTG
 end module utils
