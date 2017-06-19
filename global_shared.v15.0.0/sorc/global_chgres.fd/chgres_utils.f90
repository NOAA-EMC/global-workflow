      SUBROUTINE GL2ANY(IP,KM,G1,IM1,JM1,G2,IM2,JM2,RLON,RLAT) 
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    GL2ANY      INTERPOLATE GAUSSIAN GRID TO ANY GRID      
!   PRGMMR: EMC              ORG: W/NMC23     DATE: JAN-18-2017
!                                                                       
! ABSTRACT: LINEARLY INTERPOLATES GAUSSIAN GRID TO ANY GRID.            
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   2017-JAN-18  EMC      INITIAL VERSION
!                                                                       
! USAGE:    CALL GL2ANY(IP,KM,G1,IM1,JM1,G2,IM2,JM2,RLON,RLAT)          
!   INPUT ARGUMENT LIST:                                                
!     IP           INTEGER INTERPOLATION TYPE                           
!     KM           INTEGER NUMBER OF LEVELS                             
!     G1           REAL (IM1,JM1,KM) INPUT GAUSSIAN FIELD               
!     IM1          INTEGER NUMBER OF INPUT LONGITUDES                   
!     JM1          INTEGER NUMBER OF INPUT LATITUDES                    
!     IM2          INTEGER NUMBER OF OUTPUT LONGITUDES                  
!     JM2          INTEGER NUMBER OF OUTPUT LATITUDES                   
!     RLON         REAL (IM2,JM2) OUTPUT GRID LONGITUDES                
!     RLAT         REAL (IM2,JM2) OUTPUT GRID LATITUDES                 
!   OUTPUT ARGUMENT LIST:                                               
!     G2           REAL (IM2,JM2,KM) OUTPUT FIELD              
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   IPOLATES     IREDELL'S POLATE FOR SCALAR FIELDS                     
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      INTEGER, INTENT(IN)     :: IP, KM, IM1, JM1, IM2, JM2 
      REAL, INTENT(IN)        :: G1(IM1,JM1,KM) 
      REAL, INTENT(IN)        :: RLAT(IM2,JM2),RLON(IM2,JM2) 
      REAL, INTENT(OUT)       :: G2(IM2,JM2,KM) 
      LOGICAL*1               :: L1(IM1,JM1,KM),L2(IM2,JM2,KM) 
      INTEGER                 :: IB1(KM),IB2(KM) 
      INTEGER                 :: KGDS1(200),KGDS2(200) 
      INTEGER                 :: IPOPT(20), IRET, NO 
      DATA KGDS1/4,0,0,90000,0,0,-90000,193*0/ 
      DATA KGDS2/200*0/ 
      DATA IPOPT/20*0/ 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      L1=.TRUE. 
      KGDS2(1) = -1 
      NO = IM2*JM2 
      IB1=0 
      KGDS1(2)=IM1 
      KGDS1(3)=JM1 
      KGDS1(8)=NINT(-360000./IM1) 
      KGDS1(10)=JM1/2 
      CALL IPOLATES(IP,IPOPT,KGDS1,KGDS2,IM1*JM1,IM2*JM2,KM,IB1,L1,G1,  &
                    NO,RLAT,RLON,IB2,L2,G2,IRET)                        
      IF(IRET/=0)THEN 
        PRINT*,'FATAL ERROR IN ROUTINE GL2ANY, IRET: ', IRET 
        CALL ERREXIT(23) 
      ENDIF 
      END SUBROUTINE GL2ANY 
                                                                        
!-----------------------------------------------------------------------
      SUBROUTINE GL2ANYV(IP,KM,G1U,G1V,IM1,JM1,G2U,G2V,IM2,JM2,RLON,RLAT) 
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    GL2ANYV     INTERPOLATE GAUSSIAN GRID TO ANY GRID      
!   PRGMMR: EMC              ORG: W/NMC23     DATE: JAN-23-2017
!                                                                       
! ABSTRACT: LINEARLY INTERPOLATES VECTOR FIELD FROM GAUSSIAN GRID TO
!           ANY GRID.  OUTPUT WINDS ARE EARTH RELATIVE.
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   2017-JAN-23  ESRL/EMC    INITIAL VERSION
!                                                                       
! USAGE:    CALL GL2ANY(IP,KM,G1U,G1V,IM1,JM1,G2U,G2V,IM2,JM2,RLON,RLAT)
!   INPUT ARGUMENT LIST:                                                
!     IP           INTEGER INTERPOLATION TYPE                           
!     KM           INTEGER NUMBER OF LEVELS                             
!     G1U          REAL (IM1,JM1,KM) INPUT GAUSSIAN U-COMPONENT FIELD               
!     G1V          REAL (IM1,JM1,KM) INPUT GAUSSIAN V-COMPONENT FIELD               
!     IM1          INTEGER NUMBER OF INPUT LONGITUDES                   
!     JM1          INTEGER NUMBER OF INPUT LATITUDES                    
!     IM2          INTEGER NUMBER OF OUTPUT LONGITUDES                  
!     JM2          INTEGER NUMBER OF OUTPUT LATITUDES                   
!     RLON         REAL (IM2,JM2) OUTPUT GRID LONGITUDES                
!     RLAT         REAL (IM2,JM2) OUTPUT GRID LATITUDES                 
!   OUTPUT ARGUMENT LIST:                                               
!     G2U          REAL (IM2,JM2,KM) OUTPUT U-COMPONENT FIELD
!     G2V          REAL (IM2,JM2,KM) OUTPUT V-COMPONENT FIELD
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   IPOLATEV     IREDELL'S POLATE FOR VECTOR FIELDS                     
!                                                                       
! ATTRIBUTES:                                                          
!   LANGUAGE: FORTRAN 90
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      INTEGER, INTENT(IN)     :: IP, KM, IM1, JM1, IM2, JM2 
      REAL, INTENT(IN)        :: G1U(IM1,JM1,KM), G1V(IM1,JM1,KM)
      REAL, INTENT(IN)        :: RLAT(IM2,JM2),RLON(IM2,JM2) 
      REAL, INTENT(OUT)       :: G2U(IM2,JM2,KM), G2V(IM2,JM2,KM)
      LOGICAL*1               :: L1(IM1,JM1,KM),L2(IM2,JM2,KM) 
      INTEGER                 :: IB1(KM),IB2(KM) 
      INTEGER                 :: KGDS1(200),KGDS2(200) 
      INTEGER                 :: IPOPT(20), IRET, NO 
      REAL                    :: CROT(IM2,JM2),SROT(IM2,JM2) 
      DATA KGDS1/4,0,0,90000,0,0,-90000,193*0/ 
      DATA KGDS2/200*0/ 
      DATA IPOPT/20*0/ 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      L1=.TRUE. 
      KGDS2(1) = -1  ! THE OUTPUT GRID IS A SERIES OF POINTS
      NO = IM2*JM2 
      IB1=0 
      KGDS1(2)=IM1 
      KGDS1(3)=JM1 
      KGDS1(8)=NINT(-360000./IM1) 
      KGDS1(10)=JM1/2 
      CROT = 1.0    ! DONT ROTATE WINDS TO THE OUTPUT GRID.
      SROT = 0.0    ! FV3 EXPECTS EARTH RELATIVE WINDS.
      CALL IPOLATEV(IP,IPOPT,KGDS1,KGDS2,IM1*JM1,IM2*JM2,KM,IB1,L1,G1U,  &
                    G1V,NO,RLAT,RLON,CROT,SROT,IB2,L2,G2U,G2V,IRET)                        
      IF(IRET/=0)THEN 
        PRINT*,'FATAL ERROR IN ROUTINE GL2ANYV, IRET: ', IRET 
        CALL ERREXIT(23) 
      ENDIF 
      END SUBROUTINE GL2ANYV

!-----------------------------------------------------------------------
      SUBROUTINE NEWSIG(NSIL,IDVC,LEVS,NVCOORD,VCOORD,IRET) 
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM: NEWSIG         GET NEW SIGMA STRUCTURE                    
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 98-04-03            
!                                                                       
! ABSTRACT: READ IN INTERFACE SIGMA VALUES (OR USE OLD VALUES)          
!   AND COMPUTE FULL SIGMA VALUES.                                      
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   98-04-03  IREDELL                                                   
!                                                                       
! USAGE:    CALL NEWSIG(NSIL,IDVC,LEVS,NVCOORD,VCOORD,IRET)             
!   INPUT ARGUMENTS:                                                    
!     NSIL         INTEGER UNIT NUMBER OF NEW SIGMA INTERFACE VALUES    
!     IDVC         INTEGER VERTICAL COORDINATE ID                       
!     LEVS         INTEGER NEW NUMBER OF LEVELS                         
!     NVCOORD      INTEGER NEW NUMBER OF VERTICAL COORDINATES           
!   OUTPUT ARGUMENTS:                                                   
!     VCOORD       REAL (LEVS+1,NVCOORD) NEW VERTICAL COORDINATES       
!     IRET         INTEGER RETURN CODE                                  
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
!                                                                       
      INTEGER :: NSIL,IDVC,LEVS,NVCOORD,IRET 
      REAL    :: VCOORD(LEVS+1,NVCOORD) 
!                                                                       
      INTEGER :: IDVCI,LEVSI,NVCOORDI,K,N 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  READ VERTICAL COORDINATES                                            
      PRINT*,'' 
      PRINT*,"READ VERTICAL COORDS FROM UNIT ",NSIL 
      READ(NSIL,*,IOSTAT=IRET) IDVCI,LEVSI,NVCOORDI 
      write(*,*)'IDVCI=',IDVCI,' LEVSI=',LEVSI,' NVCOORDI=',NVCOORDI 
      IF(IRET == 0) THEN 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                 ! Added by Moorthi for gaea            
        if (nvcoordi == 0) then 
          nvcoordi = nvcoord 
          backspace nsil 
          if (idvci > 5) then 
            levsi    = idvci 
            idvci    = 0 
            idvc     = idvci 
            nvcoordi = 1 
            nvcoord  = nvcoordi 
                                                                        
            backspace nsil 
            READ(NSIL,*,IOSTAT=IRET) (VCOORD(K,1),K=2,LEVS) 
            VCOORD(1,1)      = 1. 
            VCOORD(LEVS+1,1) = 0. 
          else 
            READ(NSIL,*,IOSTAT=IRET)                                    &
                        ((VCOORD(K,N),N=1,NVCOORD),K=1,LEVS+1)          
          endif 
        elseif (nvcoordi <= 3) then 
          READ(NSIL,*,IOSTAT=IRET)                                      &
                        ((VCOORD(K,N),N=1,NVCOORD),K=1,LEVS+1)          
        else 
          write(0,*)'FATAL ERROR: nvcoordi=',nvcoordi,' not available-abort chgres' 
          call errexit(55)
        endif 
        IF(IRET    .NE. 0) RETURN 
        IF(IDVCI   .NE. IDVC.OR.LEVSI .NE. LEVS) IRET = 28 
        IF(NVCOORDI.NE. NVCOORD)                 IRET = 28 
        IF(IRET .NE. 0) RETURN 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  READ INTERFACE HYBRID VALUES                                         
      ELSE 
        REWIND NSIL 
        READ(NSIL,*,IOSTAT=IRET) IDVCI 
        REWIND NSIL 
        IF(IRET == 0 .AND. (IDVCI == 2 .OR. IDVCI == 3)) THEN 
          READ(NSIL,*,IOSTAT=IRET) IDVCI, LEVSI 
          READ(NSIL,*,IOSTAT=IRET) (VCOORD(K,1),VCOORD(K,2),K=1,LEVS+1) 
          IF(IRET.NE.0) RETURN 
          IF(IDVCI.NE.IDVC.OR.LEVSI.NE.LEVS) IRET = 28 
          IF(IRET.NE.0) RETURN 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  READ INTERFACE SIGMA VALUES                                          
        ELSE 
          VCOORD(1,1)      = 1. 
          VCOORD(LEVS+1,1) = 0. 
          READ(NSIL,*,IOSTAT=IRET) LEVSI 
          READ(NSIL,*,IOSTAT=IRET) (VCOORD(K,1),K=2,LEVS) 
          IF(IRET.NE.0) RETURN 
          IF(LEVSI.NE.LEVS) IRET = 28 
          IF(IRET.NE.0) RETURN 
        ENDIF 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      ENDIF 
      IRET=0 
      END SUBROUTINE NEWSIG 
!-----------------------------------------------------------------------
      SUBROUTINE TRSSC(JCAP,NC,KM,NTRAC,IDVM,                           &
                       IDRT,LONB,LATB,IJN,J1,J2,JC,LONSPERLAT,          &
                       SZS,SPS,ST,SD,SZ,SQ,ZS,PS,T,U,V,Q)               
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    TRSSC       TRANSFORM SIGMA SPECTRAL FIELDS TO GRID    
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31            
!                                                                       
! ABSTRACT: TRANSFORMS SIGMA SPECTRAL FIELDS TO GRID AND CONVERTS       
!   LOG SURFACE PRESSURE TO SURFACE PRESSURE AND VIRTUAL TEMPERATURE    
!   TO TEMPERATURE.                                                     
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-10-31  MARK IREDELL                                              
!                                                                       
! USAGE:    CALL TRSSC(JCAP,NC,KM,NTRAC,IDVM,                           
!    &                 IDRT,LONB,LATB,IJN,J1,J2,JC,LONSPERLAT,          
!    &                 SZS,SPS,ST,SD,SZ,SQ,ZS,PS,T,U,V,Q)               
!   INPUT ARGUMENT LIST:                                                
!     JCAP         INTEGER SPECTRAL TRUNCATION                          
!     NC           INTEGER FIRST DIMENSION (NC>=(JCAP+1)*(JCAP+2))      
!     KM           INTEGER NUMBER OF LEVELS                             
!     NTRAC        INTEGER NUMBER OF TRACERS                            
!     IDVM         INTEGER MASS VARIABLE ID                             
!     IDRT         INTEGER DATA REPRESENTATION TYPE                     
!     LONB         INTEGER NUMBER OF LONGITUDES                         
!     LATB         INTEGER NUMBER OF LATITUDES                          
!     IJN          INTEGER HORIZONTAL DIMENSION                         
!     J1           INTEGER FIRST LATITUDE                               
!     J2           INTEGER LAST LATITUDE                                
!     JC           INTEGER NUMBER OF CPUS                               
!     LONSPERLAT   INTEGER (J1:J2) NUMBER OF LONGITUDES PER LATITUDE    
!     SZS          REAL (NC) OROGRAPHY                                  
!     SPS          REAL (NC) LOG SURFACE PRESSURE                       
!     ST           REAL (NC,LEVS) VIRTUAL TEMPERATURE                   
!     SD           REAL (NC,LEVS) DIVERGENCE                            
!     SZ           REAL (NC,LEVS) VORTICITY                             
!     SQ           REAL (NC,LEVS*NTRAC) TRACERS                         
!   OUTPUT ARGUMENT LIST:                                               
!     ZS           REAL (IJN) OROGRAPHY                                 
!     PS           REAL (IJN) SURFACE PRESSURE                          
!     T            REAL (IJN,KM) TEMPERATURE                            
!     U            REAL (IJN,KM) ZONAL WIND                             
!     V            REAL (IJN,KM) MERIDIONAL WIND                        
!     Q            REAL (IJN,KM*NTRAC) TRACERS                          
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   SPTRAN       PERFORM A SCALAR SPHERICAL TRANSFORM                   
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      INTEGER LONSPERLAT(J1:J2) 
      REAL SZS(NC),SPS(NC),ST(NC,KM),SD(NC,KM),SZ(NC,KM),SQ(NC,KM*NTRAC) 
      REAL ZS(IJN),PS(IJN),T(IJN,KM),U(IJN,KM),V(IJN,KM),Q(IJN,KM*NTRAC) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  SPECTRAL TRANSFORMS                                                  
      LONB2=LONB*2 
      IJ=LONB2*(J2-J1+1) 
      IN=1 
      IS=1+LONB 
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,1,1,1,LONB2,LONB2,NC,IJN,       &
                  J1,J2,JC,SZS,ZS(IN),ZS(IS),1)                         
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,1,1,1,LONB2,LONB2,NC,IJN,       &
                  J1,J2,JC,SPS,PS(IN),PS(IS),1)                         
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,KM,1,1,LONB2,LONB2,NC,IJN,      &
                  J1,J2,JC,ST,T(IN,1),T(IS,1),1)                        
      CALL SPTRANV(0,JCAP,IDRT,LONB,LATB,KM,1,1,LONB2,LONB2,NC,IJN,     &
                   J1,J2,JC,SD,SZ,U(IN,1),U(IS,1),V(IN,1),V(IS,1),1)    
      CALL SPTRAN(0,JCAP,IDRT,LONB,LATB,KM*NTRAC,1,1,LONB2,LONB2,NC,IJN,&
                  J1,J2,JC,SQ,Q(IN,1),Q(IS,1),1)                        
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  TRANSFORM TO REDUCED GRID INSTEAD                                    
      DO J=J1,J2 
        JN=LONB2*(J-J1)+IN 
        JS=LONB2*(J-J1)+IS 
        CALL SPTRRJ(LONB,LONSPERLAT(J),ZS(JN),ZS(JN),1) 
        CALL SPTRRJ(LONB,LONSPERLAT(J),ZS(JS),ZS(JS),1) 
        CALL SPTRRJ(LONB,LONSPERLAT(J),PS(JN),PS(JN),1) 
        CALL SPTRRJ(LONB,LONSPERLAT(J),PS(JS),PS(JS),1) 
        DO K=1,KM 
          CALL SPTRRJ(LONB,LONSPERLAT(J),T(JN,K),T(JN,K),1) 
          CALL SPTRRJ(LONB,LONSPERLAT(J),T(JS,K),T(JS,K),1) 
          CALL SPTRRJ(LONB,LONSPERLAT(J),U(JN,K),U(JN,K),1) 
          CALL SPTRRJ(LONB,LONSPERLAT(J),U(JS,K),U(JS,K),1) 
          CALL SPTRRJ(LONB,LONSPERLAT(J),V(JN,K),V(JN,K),1) 
          CALL SPTRRJ(LONB,LONSPERLAT(J),V(JS,K),V(JS,K),1) 
        ENDDO 
        DO K=1,KM*NTRAC 
          CALL SPTRRJ(LONB,LONSPERLAT(J),Q(JN,K),Q(JN,K),1) 
          CALL SPTRRJ(LONB,LONSPERLAT(J),Q(JS,K),Q(JS,K),1) 
        ENDDO 
      ENDDO 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  CONVERT TO SURFACE PRESSURE AND TEMPERATURE                          
!     SELECT CASE(MOD(IDVM,10))                                         
!     CASE(0,1)                                                         
!       DO I=1,IJ                                                       
!         PS(I)=1.E3*EXP(PS(I))                                         
!       ENDDO                                                           
!     CASE(2)                                                           
!       DO I=1,IJ                                                       
!         PS(I)=1.E3*PS(I)                                              
!       ENDDO                                                           
!     CASE DEFAULT                                                      
!       DO I=1,IJ                                                       
!         PS(I)=1.E3*EXP(PS(I))                                         
!       ENDDO                                                           
!     END SELECT                                                        
!     SELECT CASE(MOD(IDVM/10,10))                                      
!     CASE(0,1)                                                         
!       DO K=1,KM                                                       
!         DO I=1,IJ                                                     
!           T(I,K)=T(I,K)/(1.+(461.50/287.05-1)*Q(I,K))                 
!     if (t(i,k) .lt. 10) print *,' t=',t(i,k),' i=',i                  
!         ENDDO                                                         
!     print *,' T=',t(ij,k),' q=',Q(IJ,K),' k=',k                       
!       ENDDO                                                           
!     CASE DEFAULT                                                      
!       DO K=1,KM                                                       
!         DO I=1,IJ                                                     
!           T(I,K)=T(I,K)/(1.+(461.50/287.05-1)*Q(I,K))                 
!         ENDDO                                                         
!       ENDDO                                                           
!     END SELECT                                                        
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      END SUBROUTINE TRSSC 
!-----------------------------------------------------------------------
      SUBROUTINE NEWPS(IM,ZS,PS,IMX,KM,P,T,Q,ZSNEW,PSNEW) 
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
! USAGE:    CALL NEWPS(IM,ZS,PS,IMX,KM,P,T,Q,ZSNEW,PSNEW)               
!   INPUT ARGUMENT LIST:                                                
!     IM           INTEGER NUMBER OF POINTS TO COMPUTE                  
!     ZS           REAL (IM) OLD OROGRAPHY (M)                          
!     PS           REAL (IM) OLD SURFACE PRESSURE (PA)                  
!     IMX          INTEGER FIRST DIMENSION                              
!     KM           INTEGER NUMBER OF LEVELS                             
!     P            REAL (IMX,KM) PRESSURES (PA)                         
!     T            REAL (IMX,KM) TEMPERATURES (K)                       
!     Q            REAL (IMX,KM) SPECIFIC HUMIDITIES (KG/KG)            
!     ZSNEW        REAL (IM) NEW OROGRAPHY (M)                          
!   OUTPUT ARGUMENT LIST:                                               
!     PSNEW        REAL (IM) NEW SURFACE PRESSURE (PA)                  
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      INTEGER,INTENT(IN)::IM,IMX,KM 
      REAL,INTENT(IN):: ZS(IM),PS(IM),P(IMX,KM) 
      REAL,INTENT(IN):: T(IMX,KM),Q(IMX,KM) 
      REAL,INTENT(IN):: ZSNEW(IM) 
      REAL,INTENT(OUT):: PSNEW(IM) 
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
!       if (zsnew(i) == zs(i)) then                                     
!         psnew(i) = ps(i)                                              
!       else                                                            
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
!-----------------------------------------------------------------------
      SUBROUTINE VINTG(IM,IX,KM1,KM2,NT,P1,U1,V1,T1,Q1,W1,P2,           &
                       U2,V2,T2,Q2,DTDP2,W2)                            
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
! USAGE:    CALL VINTG(IM,IX,KM1,KM2,NT,P1,U1,V1,T1,Q1,P2,              
!    &                 U2,V2,T2,Q2)                                     
!   INPUT ARGUMENT LIST:                                                
!     IM           INTEGER NUMBER OF POINTS TO COMPUTE                  
!     IX           INTEGER FIRST DIMENSION                              
!     KM1          INTEGER NUMBER OF INPUT LEVELS                       
!     KM2          INTEGER NUMBER OF OUTPUT LEVELS                      
!     NT           INTEGER NUMBER OF TRACERS                            
!     P1           REAL (IX,KM1) INPUT PRESSURES                        
!                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE             
!     U1           REAL (IX,KM1) INPUT ZONAL WIND                       
!     V1           REAL (IX,KM1) INPUT MERIDIONAL WIND                  
!     T1           REAL (IX,KM1) INPUT TEMPERATURE (K)                  
!     Q1           REAL (IX,KM1,NT) INPUT TRACERS (HUMIDITY FIRST)      
!     P2           REAL (IX,KM2) OUTPUT PRESSURES                       
!   OUTPUT ARGUMENT LIST:                                               
!     U2           REAL (IX,KM2) OUTPUT ZONAL WIND                      
!     V2           REAL (IX,KM2) OUTPUT MERIDIONAL WIND                 
!     T2           REAL (IX,KM2) OUTPUT TEMPERATURE (K)                 
!     Q2           REAL (IX,KM2,NT) OUTPUT TRACERS (HUMIDITY FIRST)     
!     DTDP2        REAL (IX,KM2) OUTPUT DTDP                            
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   TERP3        CUBICALLY INTERPOLATE IN ONE DIMENSION                 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      REAL P1(IX,KM1),U1(IX,KM1),V1(IX,KM1),T1(IX,KM1),Q1(IX,KM1,NT)    &
           ,W1(IX,KM1)                                                  
      REAL P2(IX,KM2),U2(IX,KM2),V2(IX,KM2),T2(IX,KM2),Q2(IX,KM2,NT)    &
           ,W2(IX,KM2)                                                  
      REAL,optional :: DTDP2(IX,KM2) 
      PARAMETER(DLTDZ=-6.5E-3*287.05/9.80665) 
      PARAMETER(DLPVDRT=-2.5E6/461.50) 
                                                                        
      REAL,allocatable :: Z1(:,:),Z2(:,:) 
      REAL,allocatable :: C1(:,:,:),C2(:,:,:),J2(:,:,:) 
                                                                        
      allocate (Z1(IM+1,KM1),Z2(IM+1,KM2)) 
      allocate (C1(IM+1,KM1,4+NT),C2(IM+1,KM2,4+NT),J2(IM+1,KM2,4+NT)) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE                        
!  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS         
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(K,I)                                                     
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
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(K,I)                                                     
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
      CALL TERP3(IM,1,1,1,1,4+NT,(IM+1)*KM1,(IM+1)*KM2,                 &
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
!jaa        DTDP2(I,K)=-J2(I,K,4)/P2(I,K)                               
          ELSE 
            T2(I,K)=T1(I,1)*EXP(DLTDZ*DZ) 
            Q2(I,K,1)=Q1(I,1,1)*EXP(DLPVDRT*(1/T2(I,K)-1/T1(I,1))-DZ) 
!jaa        DTDP2(I,K)=-T2(I,K)*DLTDZ/P2(I,K)                           
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
!-----------------------------------------------------------------------
      SUBROUTINE TERP3(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,             &
     &                 KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2,J2)      
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
                                                                        
!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(IM,IXZ1,IXQ1,IXZ2)            
!$OMP+ SHARED(IXQ2,NM,NXQ1,NXQ2,KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2)           
!$OMP+ SHARED(KXQ2,Z2,Q2,J2,K1S)                                        
                                                                        
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
!$OMP END PARALLEL DO                                                   
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      END SUBROUTINE TERP3 
!-----------------------------------------------------------------------
      SUBROUTINE RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,IXL2,KXL2,&
     &                   L2)                                            
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
!-----------------------------------------------------------------------
      SUBROUTINE SPPAD(I1,M1,Q1,I2,M2,Q2) 
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    SPPAD       PAD OR TRUNCATE A SPECTRAL FIELD           
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31            
!                                                                       
! ABSTRACT: PAD OR TRUNCATE A SPECTRAL FIELD                            
!                                                                       
! PROGRAM HISTORY LOG:                                                  
!   91-10-31  MARK IREDELL                                              
!                                                                       
! USAGE:    CALL SPPAD(I1,M1,Q1,I2,M2,Q2)                               
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     I1       - INTEGER INPUT SPECTRAL DOMAIN SHAPE                    
!                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)                   
!     M1       - INTEGER INPUT SPECTRAL TRUNCATION                      
!     Q1       - REAL ((M+1)*((I+1)*M+2)) INPUT FIELD                   
!     I2       - INTEGER OUTPUT SPECTRAL DOMAIN SHAPE                   
!                (0 FOR TRIANGULAR, 1 FOR RHOMBOIDAL)                   
!     M2       - INTEGER OUTPUT SPECTRAL TRUNCATION                     
!                                                                       
!   OUTPUT ARGUMENT LIST:                                               
!     Q2       - REAL ((M+1)*((I+1)*M+2)) OUTPUT FIELD                  
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      REAL Q1((M1+1)*((I1+1)*M1+2)) 
      REAL Q2((M2+1)*((I2+1)*M2+2)) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      DO L=0,M2 
        DO N=L,I2*L+M2 
          KS2=L*(2*M2+(I2-1)*(L-1))+2*N 
          IF(L.LE.M1.AND.N.LE.I1*L+M1) THEN 
            KS1=L*(2*M1+(I1-1)*(L-1))+2*N 
            Q2(KS2+1)=Q1(KS1+1) 
            Q2(KS2+2)=Q1(KS1+2) 
          ELSE 
            Q2(KS2+1)=0 
            Q2(KS2+2)=0 
          ENDIF 
        ENDDO 
      ENDDO 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      RETURN 
      END SUBROUTINE SPPAD 
      SUBROUTINE SPECSETS(H,D,IDRT) 
!-----------------------------------------------------------------------
!$$$  Subprogram documentation block                                    
!                                                                       
! Subprogram: specsets       Create special tracer sets                 
!   Prgmmr: Iredell          Org: W/NP23      Date: 2004-09-24          
!                                                                       
! Abstract: This subprogram optionally augments the tracers             
!   in the global model initial conditions if special tracer set        
!   are requested, based on the value of the tracer variable ID.        
!                                                                       
! Program history log:                                                  
!   2004-09-24   Iredell                                                
!                                                                       
! Modules used:                                                         
!   sigio_module   global model sigma file types and I/O                
!                                                                       
! Usage:    call specsets(h,d,idrt)                                     
!   Input arguments:                                                    
!     h            type(sigio_head) sigma file header                   
!       ak                                                              
!       bk                                                              
!       idsl                                                            
!       idvc                                                            
!       idvt                                                            
!       jcap                                                            
!       latb                                                            
!       levs                                                            
!       lonb                                                            
!       ntrac                                                           
!       si                                                              
!     d            type(sigio_data) sigma file data                     
!       hs                                                              
!       ps                                                              
!       t                                                               
!       d                                                               
!       z                                                               
!       q                                                               
!                                                                       
!   Output arguments:                                                   
!     d            type(sigio_data) sigma file data                     
!       q                                                               
!                                                                       
! Attributes:                                                           
!   Language: Fortran90                                                 
!                                                                       
! Remarks:                                                              
!   Pertinent values of h%idvt and h%ntrac                              
!   idvt  ntrac                                                         
!    100     20    set 1: vapor,ozone,cloud, and initial values of      
!                  clat*clon,clat*slon,slat,                            
!                  v*slon-u*slat*clon,-v*clon-u*slat*slon,u*clat        
!                  one,k,sigma,ps,pres,temp,entropy,moist entropy       
!                  vapor,ozone,cloud                                    
!                                                                       
!C$$$                                                                   
        use sigio_module 
        implicit none 
        type(sigio_head),intent(in):: h 
        type(sigio_dbta),intent(inout):: d 
        integer, intent(in)   :: idrt 
        real,dimension(h%latb):: slat,wlat 
        real,dimension(h%lonb,h%latb):: hs,ps 
        real,dimension(h%lonb,h%latb,h%levs):: pm,pd 
        real,dimension(h%lonb,h%latb,h%levs):: t,u,v 
        real,dimension(h%lonb,h%latb,h%levs,h%ntrac):: q 
        real clat,rlon 
        real, allocatable :: vcoord(:,:) 
                                                                        
        integer i,j,k,iret 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        call splat(idrt,h%latb,slat,wlat) 
        call sptez(0,h%jcap,idrt,h%lonb,h%latb,d%hs,hs,+1) 
        call sptez(0,h%jcap,idrt,h%lonb,h%latb,d%ps,ps,+1) 
        call sptezm(0,h%jcap,idrt,h%lonb,h%latb,h%levs,d%t,t,+1) 
        call sptezmv(0,h%jcap,idrt,h%lonb,h%latb,h%levs,d%d,d%z,u,v,+1) 
        call sptezm(0,h%jcap,idrt,h%lonb,h%latb,h%levs*3,d%q,q,+1) 
        ps=1.e3*exp(ps) 
        t=t/(1.+(461.50/287.05-1)*q(:,:,:,1)) 
        allocate(vcoord(h%levs+1,h%nvcoord)) 
        vcoord = h%vcoord 
        call sigio_modprd(h%lonb*h%latb,h%lonb*h%latb,h%levs,h%nvcoord, &
                          h%idvc,h%idsl,vcoord,iret,                    &
                          ps=ps,t=t,pm=pm,pd=pd)                        
       deallocate (vcoord) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        if(h%idvt==100.and.h%ntrac==20) then 
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(i,j,clat,rlon,k)                                         
          do j=1,h%latb 
            clat=sqrt(1-slat(j)**2) 
            do i=1,h%lonb 
              rlon=2*acos(-1.)*(i-1)/h%lonb 
              q(i,j,:,4)=clat*cos(rlon) 
              q(i,j,:,5)=clat*sin(rlon) 
              q(i,j,:,6)=slat(j) 
              q(i,j,:,7)=v(i,j,:)*sin(rlon)-u(i,j,:)*slat(j)*cos(rlon) 
              q(i,j,:,8)=-v(i,j,:)*cos(rlon)-u(i,j,:)*slat(j)*sin(rlon) 
              q(i,j,:,9)=u(i,j,:)*clat 
              q(i,j,:,10)=1 
              q(i,j,:,11)=(/(k,k=1,h%levs)/) 
              q(i,j,:,12)=pm(i,j,:)/ps(i,j) 
              q(i,j,:,13)=ps(i,j) 
              q(i,j,:,14)=pm(i,j,:) 
              q(i,j,:,15)=t(i,j,:) 
              call dothe(1,1,h%levs,pm(i,j,:),t(i,j,:),q(i,j,:,1),      &
                         q(i,j,:,16),q(i,j,:,17))                       
              q(i,j,:,16)=1004.6*log(q(i,j,:,16)/273.15) 
              q(i,j,:,17)=1004.6*log(q(i,j,:,17)/273.15) 
              q(i,j,:,18)=q(i,j,:,1) 
              q(i,j,:,19)=q(i,j,:,2) 
              q(i,j,:,20)=q(i,j,:,3) 
            enddo 
          enddo 
!$OMP END PARALLEL DO                                                   
          call sptezm(0,h%jcap,idrt,h%lonb,h%latb,h%levs*(h%ntrac-3),   &
                      d%q(1,1,4),q(1,1,1,4),-1)                         
        endif 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      end subroutine specsets 
!-----------------------------------------------------------------------
      subroutine dothe(im,ix,km,p,t,q,th,the) 
      use physcons 
      use funcphys 
      implicit none 
      integer,intent(in):: im,ix,km 
      real,intent(in):: p(ix,km),t(ix,km),q(ix,km) 
      real,intent(out):: th(ix,km),the(ix,km) 
      integer i,k 
      real(krealfp) pr,tr,qr 
      real(krealfp) qminr,elr,pvr,tdpdr,tlclr,pklclr 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  potential temperature                                                
      do k=1,km 
        do i=1,im 
          pr=p(i,k) 
          tr=t(i,k) 
          th(i,k)=tr/fpkapx(pr) 
        enddo 
      enddo 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  equivalent potential temperature                                     
      qminr=1.e-6 
      do k=1,km 
        do i=1,im 
          pr=p(i,k) 
          tr=t(i,k) 
          qr=q(i,k) 
          if(qr.lt.qminr) then 
            elr=con_hvap+con_dldt*(tr-con_ttp) 
            elr=elr*exp(-con_dldt/con_cp*(qminr-qr)) 
            tr=(elr-con_hvap)/con_dldt+con_ttp 
            qr=qminr 
          endif 
          pvr=pr*qr/(con_eps-con_epsm1*qr) 
          tdpdr=tr-ftdpx(pvr) 
          tlclr=ftlclx(tr,tdpdr) 
          pklclr=fpkapx(pr)*tlclr/tr 
          the(i,k)=fthex(tlclr,pklclr) 
        enddo 
      enddo 
      end subroutine dothe 
!-----------------------------------------------------------------------
      SUBROUTINE NEWPR1(IM,IX,KM,KMP,IDVC,IDVM,IDSL,NVCOORD,VCOORD,     &
     &                  RI, CPI, NTRACM,PP,TP,QP,PS,PM,DP)
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
!     IX           INTEGER FIRST DIMENSION                              
!     KM           INTEGER NUMBER OF LEVELS                             
!     KMP          INTEGER NUMBER OF OLD LEVELS                         
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
!                     CK  REAL (KM+1) HYBRID INTERFACE C                
!     PP           REAL (IX,KM) OLD PRESSURE                            
!     TP           REAL (IX,KM) OLD TEMPERATURE                         
!     QP           REAL (IX,KM) OLD SPECIFIC HUMIDITY                   
!     PS           REAL (IX) SURFACE PRESSURE (PA)                      
!   OUTPUT ARGUMENT LIST:                                               
!     PM           REAL (IX,KM) MID-LAYER PRESSURE (PA)                 
!     DP           REAL (IX,KM) LAYER DELTA PRESSURE (PA)               
!   TEMPORARY                                                           
!     PI           REAL (IX,KM+1) INTERFACE PRESSURE (PA)               
!     SI           REAL (KM+1) SIGMA INTERFACE VALUES (IDVC=1)          
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      REAL, PARAMETER :: RD=287.05,  RV=461.50,    CP=1004.6,           &
                         ROCP=RD/CP, ROCP1=ROCP+1, ROCPR=1/ROCP,        &
                         FV=RV/RD-1.                                    
      integer im, ix, km, kmp, idvc, idvm, idsl, nvcoord, ntracm 
      real ri(0:ntracm), cpi(0:ntracm) 
      REAL SI(KM+1),AK(KM+1),BK(KM+1),CK(KM+1) 
      REAL VCOORD(KM+1,NVCOORD) 
      REAL PS(IX),PI(IX,KM+1),PM(IX,KM) 
      REAL DP(IX,KM) 
      REAL PP(IX,KMP),TP(IX,KMP),QP(IX,KMP,NTRACM) 
      REAL PO(KMP),TO(KMP),QO(KMP,ntracm) 
      REAL PN(KM ),TN(KM ),QN(KM,ntracm ), AKBKPS(KM) 
      REAL TOV(KM),TRK,PIO(KM+1) 
!                                                                       
      real xcp, sumq, xcp2, sumq2, temu, temd, converg, dpmin,          &
           dpminall, tvu, tvd, tem, tem1, cp0i, qnk                     
      integer sfcpress_id, thermodyn_id, i, k, n, nit 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                                                                        
      sfcpress_id  = mod(IDVM,1) 
      thermodyn_id = mod(IDVM/10,10) 
                          ! hmhj for s-t                                
      IF(IDVC.EQ.3) THEN 
        DO K=1,KM 
          AK(K)  = VCOORD(K,1) 
          BK(K)  = VCOORD(K,2) 
          CK(K)  = VCOORD(K,3) 
          TOV(K) = 300.0 
        ENDDO 
        PI(1:IM,1)    = PS(1:IM) 
        PI(1:IM,KM+1) = 0.0 
!                                                                       
! first guess : assume KMP=KM                                           
!                                                                       
        if (thermodyn_id <= 1) then 
!!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(KM,kmp,IM)                   
!!$OMP+ SHARED(qn,qp,TOV,PI,AK,BK,PS,CK)                                
!$omp  parallel do shared(km,kmp,im,qp,tp,tov,pi,ak,bk,ck)              
!$omp1 private(i,k,tem,qnk,trk)                                         
          DO K=2,KM 
            tem = float(k-1) / float(kmp-1) 
            DO I=1,IM 
              qnk     = qp(i,1,1)  + (qp(i,kmp,1)-qp(i,1,1))*TEM 
              TRK     = (TP(I,K)*(1.0+FV*QNK)/TOV(K)) ** ROCPR 
                                                        ! p at interface
              PI(I,K) = AK(K) + BK(K)*PS(I) + CK(K)*TRK	 
            ENDDO 
          ENDDO 
!!$OMP END PARALLEL DO                                                  
        elseif (thermodyn_id == 3) then 
          cp0i = 1.0 / cpi(0) 
          DO K=2,KM 
            tem = float(k-1) / float(kmp-1) 
            DO I=1,IM 
              xcp  = 0.0 
              sumq = 0.0 
              do n=1,NTRACM 
                qn(k,n) = qp(i,1,n)  + (qp(i,kmp,n)-qp(i,1,n))*TEM 
                if( cpi(n).ne.0.0 ) then 
                  xcp  = xcp  + cpi(n)*qn(k,n) 
                  sumq = sumq + qn(k,n) 
                endif 
              enddo 
              xcp  = (1.-sumq) + xcp * cp0i 
              tem1 = tp(i,1) + (tp(i,kmp)-tp(i,1))*tem 
              trk  = (tem1*xcp/tov(k)) ** ROCPR 
                                                        ! p at interface
              PI(I,K) = AK(K) + BK(K)*PS(I) + CK(K)*TRK	 
            enddo 
          enddo 
        endif 
                                                                        
        DPMINALL=1000.0 
!$omp parallel do                                                       
!$omp1 shared(im,km,kmp,ntracm,thermodyn_id,pp,tp,qp,cpi,cp0i)          
!$omp1 shared(ak,bk,ck,pi)                                              
!$omp1 private(i,k,nit,converg,dpmin,tvu,tvd,trk)                       
!$omp1 private(pio,po,to,qo,pn,tn,qn,akbkps)                            
!$omp1 private(xcp,xcp2,sumq,sumq2,temu,temd)                           
!                                                                       
        DO I=1,IM 
          DO K=1,KMP 
            PO(K)   = PP(I,K) 
            TO(K)   = TP(I,K) 
            QO(K,:) = QP(I,K,:) 
          ENDDO 
          do k=2,km 
            akbkps(k) = ak(k) + bk(k)*ps(i) 
          enddo 
! iteration                                                             
                      ! default number of iterations                    
         DO Nit=1,400	 
           CONVERG = 0.0 
           DPMIN   = 1000.0 
           DO K=1,KM+1 
             PIO(K) = PI(I,K) 
           ENDDO 
           DO K=1,KM 
             PN(K) = 0.5*(PIO(K)+PIO(K+1)) 
           ENDDO 
! do interpolation by the intrinsic method to get TN and QN             
           if (thermodyn_id <= 1) then 
             CALL VINTTQ(KMP,KM,PO,TO,QO(1,1),PN,TN,QN(1,1)) 
             DO K=2,KM 
               TVU = TN(K  )*(1.0+FV*QN(K,1)) 
               TVD = TN(K-1)*(1.0+FV*QN(K-1,1)) 
               TRK = ((TVD+TVU)/(TOV(K-1)+TOV(K))) ** ROCPR 
               PI(I,K) = AKBKPS(K) + CK(K)*TRK 
               CONVERG = MAX(CONVERG,ABS(PI(I,K)-PIO(K))                &
                                       /(PI(I,K)+PIO(K)))               
                                              ! make it converged faster
               PI(I,K) = 0.5*(PI(I,K)+PIO(K))	 
               DPMIN   = MIN(DPMIN,PI(I,K-1)-PI(I,K)) 
             ENDDO 
           elseif (thermodyn_id == 3) then 
             CALL VINTTR(1,1,KMP,KM,NTRACM,PO,TO,QO,PN,TN,QN) 
             DO K=2,KM 
               xcp   = 0.0 
               xcp2  = 0.0 
               sumq  = 0.0 
               sumq2 = 0.0 
               do n=1,NTRACM 
                 if( cpi(n).ne.0.0 ) then 
                   xcp   = xcp   + cpi(n)*qn(k,n) 
                   sumq  = sumq  + qn(k,n) 
                   xcp2  = xcp2  + cpi(n)*qn(k-1,n) 
                   sumq2 = sumq2 + qn(k-1,n) 
                 endif 
               enddo 
               temu    = (1.-sumq)  + xcp*cp0i 
               temd    = (1.-sumq2) + xcp2*cp0i 
               trk     = ((tn(k)*temu + tn(k-1)*temd)                   &
                       /  (TOV(K)     + TOV(K-1))) ** ROCPR             
               PI(I,K) = AKBKPS(K) + CK(K)*TRK 
               CONVERG = MAX(CONVERG,ABS(PI(I,K)-PIO(K))                &
                                       /(PI(I,K)+PIO(K)))               
                                                   ! make it converged f
               PI(I,K) = 0.5*(PI(I,K)+PIO(K)) 
               DPMIN   = MIN(DPMIN,PI(I,K-1)-PI(I,K)) 
             ENDDO 
!     if (i .eq. 1) print *,' converg=',converg,' nit=',nit             
           endif 
           IF( CONVERG.LE.1.E-6 ) GOTO 100 
         ENDDO 
  100    CONTINUE 
!        PRINT *,'I=',I,' CONVERGED AT',Nit,' ITERATIONS',' DPMIN='     
!    &,          DPMIN                                                  
         DPMINALL = MIN(DPMINALL,DPMIN) 
        ENDDO 
!!$OMP END PARALLEL DO                                                  
!       PRINT *,' ---- THE MINIMUM DP FOR A GROUP IS ',DPMINALL         
      ELSE IF(IDVC.EQ.2) THEN 
        DO K=1,KM+1 
          AK(K)      = VCOORD(K,1) 
          BK(K)      = VCOORD(K,2) 
          PI(1:IM,K) = AK(K) + BK(K)*PS(1:IM) 
        ENDDO 
      ELSE 
        DO K=1,KM+1 
          SI(K)      = VCOORD(K,1) 
          PI(1:IM,K) = SI(K)*PS(1:IM) 
        ENDDO 
      ENDIF 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      IF(IDSL.EQ.2) THEN 
        DO K=1,KM 
          PM(1:IM,K) = (PI(1:IM,K)+PI(1:IM,K+1))/2 
        ENDDO 
      ELSE 
!$OMP PARALLEL DO DEFAULT(PRIVATE) SHARED(KM,PM,IM,PI)                  
        DO K=1,KM 
          PM(1:IM,K) = ((PI(1:IM,K)**ROCP1-PI(1:IM,K+1)**ROCP1)/        &
                       (ROCP1*(PI(1:IM,K)-PI(1:IM,K+1))))**ROCPR        
        ENDDO 
!$OMP END PARALLEL DO                                                   
      ENDIF 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      DO K=1,KM 
        DO I=1,IM 
          DP(I,K) = PI(I,K) - PI(I,K+1) 
        ENDDO 
      ENDDO 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      END SUBROUTINE NEWPR1 
!-----------------------------------------------------------------------
      SUBROUTINE CHECKDP(IM,IX,KM,AK,BK,CK,PS,TP,QP) 
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    CHECKDP       COMPUTE MODEL PRESSURES                  
!   PRGMMR: JUANG          ORG: W/NMC23     DATE: 2005-04-11            
!                                                                       
! ABSTRACT: CHECK THICKNESS FOR SIGMA-THETA COORDINATE                  
!                                                                       
! PROGRAM HISTORY LOG:                                                  
! 2005-04-11  HANN_MING HENRY JUANG    hybrid sigma, sigma-p, and sigma-
!                                                                       
!   INPUT ARGUMENT LIST:                                                
!     IM           INTEGER NUMBER OF POINTS TO COMPUTE                  
!     IX           INTEGER FIRST DIMENSION                              
!     KM           INTEGER NUMBER OF LEVELS                             
!     AK           REAL (KM+1) HYBRID INTERFACE A                       
!     BK           REAL (KM+1) HYBRID INTERFACE B                       
!     CK           REAL (KM+1) HYBRID INTERFACE C                       
!     TP           REAL (IX,KM) OLD TEMPERATURE                         
!     QP           REAL (IX,KM) OLD SPECIFIC HUMIDITY                   
!     PS           REAL (IX) SURFACE PRESSURE (PA)                      
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      INTEGER IM,IX,KM 
      REAL,PARAMETER  :: RD=287.05,RV=461.50,CP=1004.6 
      REAL,PARAMETER  :: ROCP=RD/CP,ROCP1=ROCP+1,ROCPR=1./ROCP,         &
                         FV=RV/RD-1.                                    
      REAL AK(KM+1),BK(KM+1),CK(KM+1),PS(IX) 
      REAL TP(IX,KM),QP(IX,KM),PI(IM,KM+1) 
      REAL TOV(KM),TRK,TVU,TVD 
      INTEGER K,I,KMIN 
      REAL DPMIN,FTV,AT,AQ 
!                                                                       
      FTV(AT,AQ)=AT*(1+FV*AQ) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        DO K=1,KM 
          TOV(K) = 300.0 
        ENDDO 
        PI(1:IM,1)=PS(1:IM) 
        PI(1:IM,KM+1)=0.0 
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(K,I,TVU,TVD,TRK)                                         
        DO K=2,KM 
          DO I=1,IM 
            TVU=FTV(TP(I,K  ),QP(I,K  )) 
            TVD=FTV(TP(I,K-1),QP(I,K-1)) 
            TRK = (TVD+TVU)/(TOV(K-1)+TOV(K)) 
            TRK = TRK ** ROCPR 
            PI(I,K)=AK(K)+BK(K)*PS(I)+CK(K)*TRK 
          ENDDO 
        ENDDO 
!$OMP END PARALLEL DO                                                   
                                                                        
        DO I=1,IM 
          DPMIN=1000. 
          DO K=1,KM 
            IF( PI(I,K)-PI(I,K+1) .LT. DPMIN ) THEN 
              KMIN=K 
              DPMIN=PI(I,K)-PI(I,K+1) 
            ENDIF 
          ENDDO 
          IF( DPMIN.LT.0.0 )PRINT *,' I KMIN DPMIN ',I,KMIN,DPMIN 
        ENDDO 
      RETURN 
      END SUBROUTINE CHECKDP 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      SUBROUTINE VINTTQ(KM1,KM2,P1,T1,Q1,P2,T2,Q2) 
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    VINTTQ   VERTICALLY INTERPOLATE UPPER-AIR T AND Q      
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
!   1991-10-31  MARK IREDELL                                            
!   2005-08-31  Henry JUANG MODIFIED IT TO DO T AND Q FROM VINTG        
!                                                                       
! USAGE:    CALL VINTTQ(IM,IX,KM1,KM2,P1,T1,Q1,P2,T2,Q2)                
!   INPUT ARGUMENT LIST:                                                
!     KM1          INTEGER NUMBER OF INPUT LEVELS                       
!     KM2          INTEGER NUMBER OF OUTPUT LEVELS                      
!     P1           REAL (IX,KM1) INPUT PRESSURES                        
!                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE             
!     T1           REAL (IX,KM1) INPUT TEMPERATURE (K)                  
!     Q1           REAL (IX,KM1,NT) INPUT TRACERS (HUMIDITY FIRST)      
!     P2           REAL (IX,KM2) OUTPUT PRESSURES                       
!   OUTPUT ARGUMENT LIST:                                               
!     T2           REAL (IX,KM2) OUTPUT TEMPERATURE (K)                 
!     Q2           REAL (IX,KM2,NT) OUTPUT TRACERS (HUMIDITY FIRST)     
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   TERP3_HJ     CUBICALLY INTERPOLATE IN ONE DIMENSION                 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      INTEGER KM1,KM2 
      REAL P1(KM1),T1(KM1),Q1(KM1) 
      REAL P2(KM2),T2(KM2),Q2(KM2) 
      REAL,PARAMETER :: DLTDZ=-6.5E-3*287.05/9.80665 
      REAL,PARAMETER :: DLPVDRT=-2.5E6/461.50 
      REAL Z1(2,KM1),Z2(2,KM2) 
      REAL C1(2,KM1,2),C2(2,KM2,2) 
      INTEGER K 
      REAL DZ 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE                        
!  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS         
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(K)                                                       
      DO K=1,KM1 
        Z1(1,K)=-LOG(P1(K)) 
        C1(1,K,1)=T1(K) 
        C1(1,K,2)=Q1(K) 
      ENDDO 
!$OMP END PARALLEL DO                                                   
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(K)                                                       
      DO K=1,KM2 
        Z2(1,K)=-LOG(P2(K)) 
      ENDDO 
!$OMP END PARALLEL DO                                                   
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  PERFORM LAGRANGIAN ONE-DIMENSIONAL INTERPOLATION                     
!  THAT IS 4TH-ORDER IN INTERIOR, 2ND-ORDER IN OUTSIDE INTERVALS        
!  AND 1ST-ORDER FOR EXTRAPOLATION.                                     
      CALL TERP3_HJ(1,1,1,1,1,2,2*KM1,2*KM2,                            &
                 KM1,2,2,Z1,C1,KM2,2,2,Z2,C2)                           
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  COPY OUTPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS            
!  EXCEPT BELOW THE INPUT DOMAIN, LET TEMPERATURE INCREASE WITH A FIXED 
!  LAPSE RATE AND LET THE RELATIVE HUMIDITY REMAIN CONSTANT.            
      DO K=1,KM2 
        DZ=Z2(1,K)-Z1(1,1) 
        IF(DZ.GE.0) THEN 
          T2(K)=C2(1,K,1) 
          Q2(K)=C2(1,K,2) 
        ELSE 
          T2(K)=T1(1)*EXP(DLTDZ*DZ) 
          Q2(K)=Q1(1)*EXP(DLPVDRT*(1/T2(K)-1/T1(1))-DZ) 
        ENDIF 
      ENDDO 
      END SUBROUTINE VINTTQ 
!-----------------------------------------------------------------------
      SUBROUTINE TERP3_HJ(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,          &
                       KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2)         
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    TERP3_HJ    CUBICALLY INTERPOLATE IN ONE DIMENSION     
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
! 2006-11-10  SIMPLIFIED VERSION OF TERP3                               
!                                                                       
! USAGE:    CALL TERP3_HJ(IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,          
!    &                 KM1,KXZ1,KXQ1,Z1,Q1,KM2,KXZ2,KXQ2,Z2,Q2)         
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
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   RSEARCH      SEARCH FOR A SURROUNDING REAL INTERVAL                 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      INTEGER IM,IXZ1,IXQ1,IXZ2,IXQ2,NM,NXQ1,NXQ2,KM1,KXZ1,KXQ1,        &
              KM2,KXZ2,KXQ2                                             
      REAL Z1(1+(IM-1)*IXZ1+(KM1-1)*KXZ1) 
      REAL Q1(1+(IM-1)*IXQ1+(KM1-1)*KXQ1+(NM-1)*NXQ1) 
      REAL Z2(1+(IM-1)*IXZ2+(KM2-1)*KXZ2) 
      REAL Q2(1+(IM-1)*IXQ2+(KM2-1)*KXQ2+(NM-1)*NXQ2) 
      REAL FFA(IM),FFB(IM),FFC(IM),FFD(IM) 
      INTEGER K1S(IM,KM2) 
      REAL Q2S,Q1A,Q1B,Q1C,Q1D,Z2S,Z1A,Z1B,Z1C,Z1D 
      INTEGER I,K1,K2,N 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  FIND THE SURROUNDING INPUT INTERVAL FOR EACH OUTPUT POINT.           
      CALL RSEARCH(IM,KM1,IXZ1,KXZ1,Z1,KM2,IXZ2,KXZ2,Z2,1,IM,K1S) 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  GENERALLY INTERPOLATE CUBICALLY WITH MONOTONIC CONSTRAINT            
!  FROM TWO NEAREST INPUT POINTS ON EITHER SIDE OF THE OUTPUT POINT,    
!  BUT WITHIN THE TWO EDGE INTERVALS INTERPOLATE LINEARLY.              
!  KEEP THE OUTPUT FIELDS CONSTANT OUTSIDE THE INPUT DOMAIN.            
      DO K2=1,KM2 
        DO I=1,IM 
          K1=K1S(I,K2) 
          IF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN 
            Z2S=Z2(1+(I-1)*IXZ2+(K2-1)*KXZ2) 
            Z1A=Z1(1+(I-1)*IXZ1+(K1-1)*KXZ1) 
            Z1B=Z1(1+(I-1)*IXZ1+(K1+0)*KXZ1) 
            FFA(I)=(Z2S-Z1B)/(Z1A-Z1B) 
            FFB(I)=(Z2S-Z1A)/(Z1B-Z1A) 
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
          ENDIF 
        ENDDO 
!  INTERPOLATE.                                                         
        DO N=1,NM 
          DO I=1,IM 
            K1=K1S(I,K2) 
            IF(K1.EQ.0) THEN 
              Q2S=Q1(1+(I-1)*IXQ1+(N-1)*NXQ1) 
            ELSEIF(K1.EQ.KM1) THEN 
              Q2S=Q1(1+(I-1)*IXQ1+(KM1-1)*KXQ1+(N-1)*NXQ1) 
            ELSEIF(K1.EQ.1.OR.K1.EQ.KM1-1) THEN 
              Q1A=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1) 
              Q1B=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1) 
              Q2S=FFA(I)*Q1A+FFB(I)*Q1B 
            ELSE 
              Q1A=Q1(1+(I-1)*IXQ1+(K1-2)*KXQ1+(N-1)*NXQ1) 
              Q1B=Q1(1+(I-1)*IXQ1+(K1-1)*KXQ1+(N-1)*NXQ1) 
              Q1C=Q1(1+(I-1)*IXQ1+(K1+0)*KXQ1+(N-1)*NXQ1) 
              Q1D=Q1(1+(I-1)*IXQ1+(K1+1)*KXQ1+(N-1)*NXQ1) 
              Q2S=MIN(MAX(                                              &
                  FFA(I)*Q1A+FFB(I)*Q1B+FFC(I)*Q1C+FFD(I)*Q1D,          &
                  MIN(Q1B,Q1C)),MAX(Q1B,Q1C))                           
            ENDIF 
            Q2(1+(I-1)*IXQ2+(K2-1)*KXQ2+(N-1)*NXQ2)=Q2S 
          ENDDO 
        ENDDO 
      ENDDO 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      END SUBROUTINE TERP3_HJ 
!-----------------------------------------------------------------------
      SUBROUTINE VINTTR(IM,IX,KM1,KM2,NT,P1,T1,Q1,P2,T2,Q2) 
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! SUBPROGRAM:    VINTG       VERTICALLY INTERPOLATE UPPER-AIR FIELDS    
!   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 92-10-31            
!   PRGMMR: S. MOORTHI       ORG: NCEP/EMC    DATE: 2006-12-12          
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
! USAGE:    CALL VINTTR(IM,IX,KM1,KM2,NT,P1,T1,Q1,P2,T2,Q2)             
!   INPUT ARGUMENT LIST:                                                
!     IM           INTEGER NUMBER OF POINTS TO COMPUTE                  
!     IX           INTEGER FIRST DIMENSION                              
!     KM1          INTEGER NUMBER OF INPUT LEVELS                       
!     KM2          INTEGER NUMBER OF OUTPUT LEVELS                      
!     NT           INTEGER NUMBER OF TRACERS                            
!     P1           REAL (IX,KM1) INPUT PRESSURES                        
!                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE             
!     T1           REAL (IX,KM1) INPUT TEMPERATURE (K)                  
!     Q1           REAL (IX,KM1,NT) INPUT TRACERS (HUMIDITY FIRST)      
!     P2           REAL (IX,KM2) OUTPUT PRESSURES                       
!   OUTPUT ARGUMENT LIST:                                               
!     T2           REAL (IX,KM2) OUTPUT TEMPERATURE (K)                 
!     Q2           REAL (IX,KM2,NT) OUTPUT TRACERS (HUMIDITY FIRST)     
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   TERP3        CUBICALLY INTERPOLATE IN ONE DIMENSION                 
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      INTEGER IM,IX,KM1,KM2,NT 
      REAL P1(IX,KM1),T1(IX,KM1),Q1(IX,KM1,NT) 
      REAL P2(IX,KM2),T2(IX,KM2),Q2(IX,KM2,NT) 
      REAL,PARAMETER :: DLTDZ=-6.5E-3*287.05/9.80665 
      REAL,PARAMETER :: DLPVDRT=-2.5E6/461.50 
      REAL Z1(IM+1,KM1),Z2(IM+1,KM2) 
      REAL C1(IM+1,KM1,1+NT),C2(IM+1,KM2,1+NT),J2(IM+1,KM2,1+NT) 
      INTEGER I,N,K 
      REAL DZ 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  COMPUTE LOG PRESSURE INTERPOLATING COORDINATE                        
!  AND COPY INPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS         
                                                                        
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(K,I)                                                     
      DO K=1,KM1 
        DO I=1,IM 
          Z1(I,K)=-LOG(P1(I,K)) 
          C1(I,K,1)=T1(I,K) 
          C1(I,K,2)=Q1(I,K,1) 
        ENDDO 
      ENDDO 
!$OMP END PARALLEL DO                                                   
      DO N=2,NT 
        DO K=1,KM1 
          DO I=1,IM 
            C1(I,K,1+N)=Q1(I,K,N) 
          ENDDO 
        ENDDO 
      ENDDO 
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(K,I)                                                     
      DO K=1,KM2 
        DO I=1,IM 
          Z2(I,K)=-LOG(P2(I,K)) 
        ENDDO 
      ENDDO 
!$OMP END PARALLEL DO                                                   
                                                                        
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  PERFORM LAGRANGIAN ONE-DIMENSIONAL INTERPOLATION                     
!  THAT IS 4TH-ORDER IN INTERIOR, 2ND-ORDER IN OUTSIDE INTERVALS        
!  AND 1ST-ORDER FOR EXTRAPOLATION.                                     
      CALL TERP3(IM,1,1,1,1,1+NT,(IM+1)*KM1,(IM+1)*KM2,                 &
                 KM1,IM+1,IM+1,Z1,C1,KM2,IM+1,IM+1,Z2,C2,J2)            
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!  COPY OUTPUT WIND, TEMPERATURE, HUMIDITY AND OTHER TRACERS            
!  EXCEPT BELOW THE INPUT DOMAIN, LET TEMPERATURE INCREASE WITH A FIXED 
!  LAPSE RATE AND LET THE RELATIVE HUMIDITY REMAIN CONSTANT.            
      DO K=1,KM2 
!       print *,' p2=',p2(1,k),' k=',k                                  
!       print *,' J2=',j2(1,k,3),' k=',k                                
        DO I=1,IM 
          DZ=Z2(I,K)-Z1(I,1) 
          IF(DZ.GE.0) THEN 
            T2(I,K)=C2(I,K,1) 
            Q2(I,K,1)=C2(I,K,2) 
          ELSE 
            T2(I,K)=T1(I,1)*EXP(DLTDZ*DZ) 
            Q2(I,K,1)=Q1(I,1,1)*EXP(DLPVDRT*(1/T2(I,K)-1/T1(I,1))-DZ) 
          ENDIF 
        ENDDO 
      ENDDO 
      DO N=2,NT 
        DO K=1,KM2 
          DO I=1,IM 
            Q2(I,K,N)=C2(I,K,1+N) 
          ENDDO 
        ENDDO 
      ENDDO 
      END SUBROUTINE VINTTR 
                                                                        
!-----------------------------------------------------------------------
       subroutine getomega(jcap,nc,km,idvc,idvm,idrt,idsl,nvcoord,      &
            vcoord,lonb,latb,ijl,ijn,j1,j2,jc,sd,sps,psi,ti,ui,vi,wi)
       use sigio_module, only : sigio_modprd 
       implicit none 
!                                                                       
       integer,intent(in):: jcap,nc,km,idvc,idvm,idrt,idsl,nvcoord 
       integer,intent(in):: lonb,latb,ijl,j1,j2,jc,ijn 
       real,intent(in):: vcoord(km+1,nvcoord) 
       real,intent(in):: sd(nc,km),sps(nc) 
       real,intent(in):: psi(ijn),ti(ijn,km),ui(ijn,km),vi(ijn,km) 
       real,intent(out):: wi(ijn,km) 
       real :: pd(ijn,km),pi(ijn,km+1),pm(ijn,km) 
       real :: os 
       real dpmdps(ijn,km),dpddps(ijn,km),dpidps(ijn,km+1),vgradp,psmean 
       real di(ijn,km),psx(ijn),psy(ijn) 
       integer k,i,ij,lonb2,in,is,iret 
!----1. spectral transform                                              
      lonb2=lonb*2 
      ij=lonb2*(j2-j1+1) 
      in=1 
      is=1+lonb 
      call sptrand(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijn,      &
           j1,j2,jc,sps,psmean,                                         &
           psx(in),psx(is),psy(in),psy(is),1)                           
      SELECT CASE(MOD(IDVM,10)) 
      CASE(0,1) 
          continue 
      CASE(2) 
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(i)                                                       
          do i=1,ijn 
           psx(i)=psx(i)/(psi(i)*1.0E-3) 
           psy(i)=psy(i)/(psi(i)*1.0E-3) 
          enddo 
!$OMP END PARALLEL DO                                                   
      CASE DEFAULT 
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(i)                                                       
          do i=1,ijn 
           psx(i)=psx(i)/psi(i) 
           psy(i)=psy(i)/psi(i) 
          enddo 
!$OMP END PARALLEL DO                                                   
      END SELECT 
                                                                        
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(K)                                                       
      do K=1,km 
        call sptran(0,jcap,idrt,lonb,latb,1,1,1,lonb2,lonb2,nc,ijn,     &
           j1,j2,jc,sd(1,k),di(in,k),di(is,k),1)                        
      enddo 
!$OMP END PARALLEL DO                                                   
                                                                        
       call sigio_modprd(ijl,ijn,km,nvcoord,idvc,idsl,vcoord,iret,      &
                   ps=psi,t=ti,pm=pm,pd=pd,dpmdps=dpmdps,dpddps=dpddps) 
                                                                        
!----3.omeda from modstuff                                              
!$OMP PARALLEL DO DEFAULT(SHARED)                                       
!$OMP+ PRIVATE(i)                                                       
      do i=1,ijl 
       pi(i,1)=psi(i) 
       dpidps(i,1)=1. 
       do k=1,km 
         pi(i,k+1)=pi(i,k)-pd(i,k) 
         dpidps(i,k+1)=dpidps(i,k)-dpddps(i,k) 
       enddo 
       os=0. 
       do k=km,1,-1 
        vgradp=ui(i,k)*psx(i)+vi(i,k)*psy(i) 
        os=os-vgradp*psi(i)*(dpmdps(i,k)-dpidps(i,k+1))-                &
           di(i,k)*(pm(i,k)-pi(i,k+1))                                  
        wi(i,k)=vgradp*psi(i)*dpmdps(i,k)+os 
        os=os-vgradp*psi(i)*(dpidps(i,k)-dpmdps(i,k))-                  &
           di(i,k)*(pi(i,k)-pm(i,k))                                    
       enddo 
!                                                                       
      enddo 
!$OMP END PARALLEL DO                                                   
       return 
       end subroutine getomega 
      subroutine sptrrj(imax,lonsperlat,grid,gred,idir) 
        implicit none 
        integer,intent(in):: imax,lonsperlat,idir 
        real,intent(inout):: grid(imax),gred(imax) 
        real four(imax+2),gour(lonsperlat) 
        integer i,iour 
        real rred 
                                                                        
        rred = lonsperlat/real(imax) 
        four = 0 
!! take transformed to full 'grid' and make it like transformed to reduc
        if(idir > 0) then 
          call spfft1(imax,imax/2+1,imax,1,four,grid,-idir) 
          call spfft1(lonsperlat,imax/2+1,imax,1,four,gour,idir) 
          do i=1,imax 
            iour = nint((i-1)*rred)+1 
            if(iour == lonsperlat+1) iour  =1 
            gred(i) = gour(iour) 
          enddo 
!! take transformed to reduced and interpolated 'gred' and make it like 
        elseif(idir < 0) then 
          do iour=1,lonsperlat 
            i = nint((iour-1)/rred)+1 
            if(i == imax+1) i = 1 
            gour(iour) = gred(i) 
          enddo 
          call spfft1(lonsperlat,imax/2+1,imax,1,four,gour,idir) 
          call spfft1(imax,imax/2+1,imax,1,four,grid,-idir) 
        endif 
      end subroutine sptrrj 
      subroutine compute_zh(im, jm, levp, ak_in, bk_in, ps, zs, t, sphum, zh) 
       implicit none 
       integer, intent(in):: levp, im,jm 
       real,    intent(in), dimension(levp+1):: ak_in, bk_in 
       real,    intent(in), dimension(im,jm):: ps, zs 
       real,    intent(in), dimension(im,jm,levp):: t 
       real,    intent(in), dimension(im,jm,levp):: sphum 
       real,    intent(out), dimension(im,jm,levp+1):: zh 
       ! Local:                                                         
       real, dimension(im,levp+1):: pe0, pn0 
       real, dimension(levp+1) :: ak, bk 
       integer i,j,k 
       real, parameter :: GRAV   = 9.80665 
       real, parameter :: RDGAS  = 287.05 
       real, parameter :: RVGAS = 461.50 
       real, parameter :: e0 = 610.71 
       real, parameter :: hlv = 2.501e6 
       real, parameter :: tfreeze = 273.15 
       real  :: zvir 
       real:: grd 
       grd = grav/rdgas 
       zvir = rvgas/rdgas - 1. 
       ak = ak_in 
       bk = bk_in 
       ak(levp+1) = max(1.e-9, ak(levp+1)) 
                                                                        
       do j = 1, jm 
         do i=1, im 
           pe0(i,levp+1) = ak(levp+1) 
           pn0(i,levp+1) = log(pe0(i,levp+1)) 
         enddo 
                                                                        
         do k=levp,1, -1 
            do i=1,im 
              pe0(i,k) = ak(k) + bk(k)*ps(i,j) 
              pn0(i,k) = log(pe0(i,k)) 
            enddo 
         enddo 
                                                                        
         zh(1:im,j,1) = zs(1:im,j) 
         do k = 2, levp+1 
           do i = 1, im 
       zh(i,j,k) = zh(i,j,k-1)+t(i,j,k-1)*(1.+zvir*sphum(i,j,k-1))*     &
              (pn0(i,k-1)-pn0(i,k))/grd                                 
           enddo 
         enddo 
                                                                        
       enddo 
                                                                        
      end subroutine compute_zh 
      SUBROUTINE GET_TRACERS(IDVT, NTRACO, I_CLD, I_OZN, TRAC_NAME)
                                                                        
      IMPLICIT NONE 
                                                                        
      INTEGER, INTENT(IN)            :: IDVT, NTRACO 
                                                                        
      CHARACTER(LEN=16), INTENT(OUT) :: TRAC_NAME(NTRACO) 
                                                                        
      INTEGER, INTENT(OUT)           :: I_CLD, I_OZN 
                                                                        
      TRAC_NAME = ' ' 
                                                                        
      TRAC_NAME(1) = 'spfh' 
      IF (NTRACO == 2) THEN 
        IF(IDVT  == 1) THEN 
          TRAC_NAME(2) = 'o3mr' 
          I_OZN = 2 
          I_CLD = 0 
        ELSEIF(IDVT == 2) THEN 
          TRAC_NAME(2) = 'clwmr' 
          I_OZN = 0 
          I_CLD = 2 
        ENDIF 
      ELSEIF (NTRACO == 3) THEN 
        IF(IDVT == 0) THEN 
          TRAC_NAME(2) = 'o3mr' 
          TRAC_NAME(3) = 'clwmr' 
          I_OZN = 2 
          I_CLD = 0 
        ELSEIF(IDVT == 21) THEN 
          TRAC_NAME(2) = 'o3mr' 
          TRAC_NAME(3) = 'clwmr' 
          I_OZN = 2 
          I_CLD = 3 
        ELSEIF(IDVT == 12) THEN 
          TRAC_NAME(2) = 'clwmr' 
          TRAC_NAME(3) = 'o3mr' 
          I_OZN = 3 
          I_CLD = 2 
        ENDIF 
      ELSEIF (NTRACO == 4) THEN 
        TRAC_NAME(4) = 'tke' 
        IF(IDVT == 0) THEN 
          TRAC_NAME(2) = 'o3mr' 
          TRAC_NAME(3) = 'clwmr' 
          I_OZN = 2 
          I_CLD = 0 
        ELSEIF(IDVT == 21) THEN 
          TRAC_NAME(2) = 'o3mr' 
          TRAC_NAME(3) = 'clwmr' 
          I_OZN = 2 
          I_CLD = 3 
        ELSEIF(IDVT == 12) THEN 
          TRAC_NAME(2) = 'clwmr' 
          TRAC_NAME(3) = 'o3mr' 
          I_OZN = 3 
          I_CLD = 2 
        ENDIF 
      ELSEIF(IDVT == 100) THEN 
        TRAC_NAME(2) = 'clwmr' 
        TRAC_NAME(3) = 'o3mr' 
        I_OZN = 2 
        I_CLD = 3 
                                   ! for WAM                            
      ELSEIF(IDVT == 200) THEN 
        TRAC_NAME(2) = 'clwmr' 
        TRAC_NAME(3) = 'o3mr' 
        TRAC_NAME(4) = 'o' 
        TRAC_NAME(5) = 'o2' 
        I_OZN = 2 
        I_CLD = 3 
      ENDIF 
                                                                        
      END SUBROUTINE GET_TRACERS 
      SUBROUTINE VINTG_IDEA(IMO,LATCH,KM2,NT,P2,RLAT,JMO,J1,J2,IDAY,    &
      U2,V2,T2,Q2)                                                      
!$$$  SUBPROGRAM DOCUMENTATION BLOCK                                    
!                                                                       
! ABSTRACT: MAKE UPPER-AIR FIELDS MORE REAL                             
!   WIND, TEMPERATURE, HUMIDITY AND OTHER TRACER                        
!                                                                       
! USAGE:    CALL VINTG_IDEA(IMO,LATCH,KM2,NT,P2,RLAT,JMO,J1,J2,IDAY,    
!    &U2,V2,T2,Q2)                                                      
!   INPUT ARGUMENT LIST:                                                
!     IMO          INTEGER NUMBER OF LOGITUDE                           
!     LATCH        INTEGER MAX NUMBER OF LAT TO PROCCESS                
!     KM2          INTEGER NUMBER OF OUTPUT LEVELS                      
!     NT           INTEGER NUMBER OF TRACERS INPUT                      
!     JMO          INTEGER NUMDER OF LATITUDE                           
!     J1           INTEGER FIRST LATITUDE INDEX,(NORTH TO SOUTH)        
!     J2           INTEGER LAST LATITUDE INDEX,                         
!     IDAY         INTEGER (4) HOUR MONTH DAY YEAR                      
!     RLAT         REAL (JMO) LATITUDE IN DEGREE                        
!     P2           REAL (IMO,2*LATCH,KM2) PRESSURES                     
!                  ORDERED FROM BOTTOM TO TOP OF ATMOSPHERE             
!   OUTPUT AND INPUT  ARGUMENT LIST:                                    
!     U2           REAL (IMO,2*LATCH,KM2)  ZONAL WIND                   
!     V2           REAL (IMO,2*LATCH,KM2)  MERIDIONAL WIND              
!     T2           REAL (IMO,2*LATCH,KM2)  TEMPERATURE (K)              
!     Q2           REAL (IMO,2*LATCH,KM2,NT+2)  TRACERS (HUMIDITY FIRST)
!                                                                       
! SUBPROGRAMS CALLED:                                                   
!   GETTEMP        Calculate temperature                                
!                                                                       
! ATTRIBUTES:                                                           
!   LANGUAGE: FORTRAN                                                   
!                                                                       
!C$$$                                                                   
      IMPLICIT NONE 
      INTEGER, INTENT(IN)    :: imo,latch,km2,nt,jmo,j1,j2,iday(4) 
      REAL   , INTENT(IN)    :: rlat(jmo),p2(imo,2*latch,km2) 
      REAL   , INTENT(INOUT) :: u2(imo,2*latch,km2),                    &
                                v2(imo,2*latch,km2),                    &
                                t2(imo,2*latch,km2),                    &
                                q2(imo,2*latch,km2,nt)                  
      REAL, parameter:: top=64.25 
                                     ! molecular wght of O (g/mol)      
      REAL, parameter:: amo=15.9994 
                                     ! molecular wght of O2 (g/mol)     
      REAL, parameter:: amo2=31.999 
                                     ! molecular wght of N2 (g/mol)     
      REAL, parameter:: amn2=28.013 
      REAL  temps(km2),tempn(km2),zmprn(km2),zmprs(km2),wfun(10) 
      REAL  n_os(km2),n_on(km2),n_o2s(km2),n_o2n(km2) 
      REAL  n_n2s(km2),n_n2n(km2) 
      REAL  sumn,sums,rlats,coe,hold 
      INTEGER i, j, k, kref, jjn, jjs, ciday, ik,idat(8),jdow,jday 
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! get weight function at joints                                         
      do i=1,10 
      wfun(i)=(i-1)/9. 
      enddo 
! get calendar day                                                      
!     call getcday(iday,ciday)                                          
!     ciday=iday(3)+30*(iday(2)-1)                                      
      idat(1)=iday(4) 
      idat(2)=iday(2) 
      idat(3)=iday(3) 
      idat(5)=iday(1) 
      call w3doxdat(idat,jdow,ciday,jday) 
!     print*,idat                                                       
      print*,iday 
      print*,'ciday',ciday 
!                                                                       
      do i=1,imo 
      do j=1,16 
      do k=1,km2 
      if(p2(i,j,k).le.0.) print*,i,j,k 
      enddo 
      enddo 
      enddo 
!  For EACH LATITUDE couple                                             
      do j=j1,j2 
! second index of data array for latitude couple                        
      jjn=2*(j-j1)+1 
      jjs=2*(j-j1)+2 
! Get zonal meam pressure                                               
        do k=1,km2 
        sumn=0. 
        sums=0. 
          do i=1,imo 
          sumn=sumn+p2(i,jjn,k) 
          sums=sums+p2(i,jjs,k) 
          enddo 
        zmprn(k)=sumn/float(imo)*.01 
        zmprs(k)=sums/float(imo)*.01 
        enddo 
! GET TEMP PROFILE                                                      
        call gettemp(ciday,1,rlat(j),1,zmprn,km2,tempn,n_on,n_o2n,      &
         n_n2n)                                                         
        rlats=-1.*rlat(j) 
        call gettemp(ciday,1,rlats,1,zmprs,km2,temps,n_os,n_o2s,        &
         n_n2s)                                                         
! JIONT WITH EACH LONGITUDE north                                       
        do i=1,imo 
           do k=1,km2 
             hold = 1./(n_on(k)*amo+amo2*n_o2n(k)+amn2*n_n2n(k)) 
!            q2(i,jjn,k,nt+1)=(amo*n_on(k))*hold                        
!            q2(i,jjn,k,nt+2)=(amo2*n_o2n(k))*hold                      
             q2(i,jjn,k,nt-1) = (amo*n_on(k))   * hold 
             q2(i,jjn,k,nt)   = (amo2*n_o2n(k)) * hold 
           enddo 
! find joint location (orig data top)                                   
           do k=1,km2 
             if(p2(i,jjn,k).le.top) then 
               kref=k 
               go to 10 
             endif 
           enddo 
   10 continue 
! temperature joint                                                     
           do k=kref,km2 
             t2(i,jjn,k) = tempn(k) 
           enddo 
           do k=kref-10,kref-1 
             t2(i,jjn,k) =    wfun(k-kref+11)  * tempn(k)+              &
                          (1.-wfun(k-kref+11)) * t2(i,jjn,k)            
           enddo 
! others : u v q                                                        
           do k=kref,km2 
             coe = p2(i,jjn,k)/p2(i,jjn,kref) 
!            coe = log(p2(i,jjn,kref))/log(p2(i,jjn,k))                 
             u2(i,jjn,k) = coe*u2(i,jjn,kref) 
             v2(i,jjn,k) = coe*v2(i,jjn,kref) 
           enddo 
        enddo 
! JIONT WITH EACH LONGITUDE south                                       
        do i=1,imo 
           do k=1,km2 
             hold = 1./(n_os(k)*amo+amo2*n_o2s(k)+amn2*n_n2s(k)) 
             q2(i,jjs,k,nt-1) = (amo*n_os(k))   * hold 
             q2(i,jjs,k,nt)   = (amo2*n_o2s(k)) * hold 
           enddo 
! find joint location (orig data top)                                   
           do k=1,km2 
             if(p2(i,jjs,k).le.top) then 
               kref=k 
               go to 11 
             endif 
           enddo 
   11      continue 
! temperature joint                                                     
           do k=kref,km2 
             t2(i,jjs,k) = temps(k) 
           enddo 
           do k=kref-10,kref-1 
             t2(i,jjs,k) = wfun(k-kref+11)  * temps(k)+                 &
                      (1.- wfun(k-kref+11)) * t2(i,jjs,k)               
           enddo 
! others : u v q ...........                                            
           do k=kref,km2 
             coe = p2(i,jjs,k)/p2(i,jjs,kref) 
!            coe = log(p2(i,jjs,kref))/log(p2(i,jjs,k))                 
             u2(i,jjs,k) = coe*u2(i,jjs,kref) 
             v2(i,jjs,k) = coe*v2(i,jjs,kref) 
           enddo 
              !logitude                                                 
        enddo 
      enddo 
!     print*,'www1'                                                     
!     print'(12f6.1)',(q2(1,i,km2,4),i=1,2*latch)                       
      end subroutine vintg_idea 
!-----------------------------------------------------------------------
      subroutine gettemp(iday,nday,xlat,nlat,pr,np,temp,n_o,n_o2,n_n2) 
!  calculate temperature at each grid point useing nrlmsise00_sub       
      implicit none 
                                                  !number of days       
      integer, intent(in) :: nday 
                                                  !number of latitudes  
      integer, intent(in) :: nlat 
                                                  !number of pressure le
      integer, intent(in) :: np 
                                                  ! pressure in mb      
      real,    intent(in) :: pr(np) 
                                                  !latitude in degree   
      real,    intent(in) :: xlat(nlat) 
                                                  !calender day         
      integer, intent(in) :: iday(nday) 
                                                  ! temperature         
      real,   intent(out) :: temp(np,nlat,nday) 
                                                 ! number density of o (
      real,   intent(out) :: n_o(np,nlat,nday) 
                                                  ! number density of o2
      real,   intent(out) :: n_o2(np,nlat,nday) 
                                                  ! number density of N2
      real,   intent(out) :: n_n2(np,nlat,nday) 
                                                  ! altitude in km      
      real                :: alt(np,nlat,nday) 
      real                :: D(9),T(2),SW(25),AP(7),ut,xlong,xlst,f107, &
                             f107a                                      
      integer             :: k,il,ip 
! set magnetic index average value                                      
      DATA AP/7*9./ 
! set swich 7,8,10,14  zero to avoid diurnal changes in output temperatu
! swich #7 is for diurnal,#8 is for semidiurnal,# 10 is for all UT/longi
! effect,#14 is for terdiurnal                                          
      data sw/1.,1.,1.,1.,1.,1.,0.,0.,1.,0.,1.,1.,1.,0.,1.,1.,1.,1.,1., &
      1.,1.,1.,1.,1.,1./                                                
! set 10.7cm flux be average value                                      
      f107=150. 
      f107a=150. 
! turn on swich                                                         
      CALL TSELEC(SW) 
! set longitude, UT, local time , It should not make difference to outpu
      ut=0. 
      xlong=0. 
      xlst=ut/3600.+xlong/15. 
! calculate temperature for each lat,pres level,day                     
      do k=1,nday 
      do il=1,nlat 
      do ip=1,np 
      CALL GHP7(IDAY(k),UT,ALT(ip,il,k),XLAT(il),XLONG,XLST,F107A,F107, &
      AP,D,T,pr(ip))                                                    
      temp(ip,il,k)=t(2) 
      n_o(ip,il,k)=D(2) 
      n_o2(ip,il,k)=D(4) 
      n_n2(ip,il,k)=D(3) 
      enddo 
      enddo 
      enddo 
      end subroutine gettemp 
