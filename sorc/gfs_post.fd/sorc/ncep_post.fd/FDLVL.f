!      SUBROUTINE FDLVL(NFD,ITYPE,HTFD,TFD,QFD,UFD,VFD,PFD)
!      SUBROUTINE FDLVL(ITYPE,TFD,QFD,UFD,VFD,PFD,ICINGFD)
      SUBROUTINE FDLVL(ITYPE,TFD,QFD,UFD,VFD,PFD,ICINGFD,AERFD)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    FDLVL       COMPUTES FD LEVEL T, Q, U, V
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES TEMPERATURE, SPEC. HUM, U WIND COMPONENT,
!     AND V WIND COMPONENT ON THE NFD=6 FD LEVELS.  THE
!     HEIGHT OF THESE LEVELS (IN METERS) IS GIVEN IN THE 
!     DATA STATEMENT BELOW.  THE ALGORITHM PROCEEDS AS 
!     FOLLOWS. (AGL IN PARENTHESES)
!     
!     AT EACH MASS POINT MOVE UP VERTICALLY FROM THE LM-TH (LOWEST
!     ATMOSPHERIC) ETA LAYER.  FIND THE ETA LAYERS WHOSE 
!     HEIGHT (ABOVE GROUND) BOUNDS THE TARGET FD LEVEL HEIGHT.
!     VERTICALLY INTERPOLATE TO GET TEMPERATURE AT THIS FD
!     LEVEL.  AVERAGE THE FOUR SURROUNDING WINDS
!     TO GET A MASS POINT WIND.  VERTICALLY INTERPOLATE THESE
!     MASS POINT WINDS TO THE TARGET FD LEVEL.  CONTINUE THIS
!     PROCESS UNTIL ALL NFD=6 FD LEVELS HAVE BEEN PROCESSED.
!     MOVE ON TO THE NEXT MASS POINT.  
!     
!     AVERAGING THE FOUR ABOVE GROUND WINDS TO THE MASS POINT
!     WAS FOUND TO SMOOTH THE FIELD AND REDUCE THE OCCURRENCE
!     OF POINT PEAK WINDS FAR IN EXCESS OF THE WINDS AT 
!     ADJACENT POINTS.  MASS POINT VALUES ARE RETURNED.
!   .     
!     
! PROGRAM HISTORY LOG:
!   92-12-22  RUSS TREADON
!   93-11-23  RUSS TREADON - CORRECTED ROUTINE TO COMPUTE
!             FD LEVELS WITH REPECT TO MEAN SEA LEVEL.
!   94-01-04  MICHAEL BALDWIN - INCLUDE OPTIONS FOR COMPUTING
!                               EITHER AGL OR MSL
!   98-06-15  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION            
!   02-01-15  MIKE BALDWIN - WRF VERSION
!   11-12-14  SARAH LU - ADD GOCART AEROSOL AERFD
!     
! USAGE:    CALL FDLVL(ITYPE,TFD,QFD,UFD,VFD)
!   INPUT ARGUMENT LIST:
!     ITYPE    - FLAG THAT DETERMINES WHETHER MSL (1) OR AGL (2)
!                   LEVELS ARE USED.
!
!   OUTPUT ARGUMENT LIST: 
!     TFD      - TEMPERATURE (K) ON FD LEVELS.
!     QFD      - SPEC HUM ON FD LEVELS.
!     UFD      - U WIND (M/S) ON FD LEVELS.
!     VFD      - V WIND (M/S) ON FD LEVELS.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!
!     LIBRARY:
!       COMMON   - 
!                  LOOPS
!                  MASKS
!                  OPTIONS
!                  INDX
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!
      use vrbls4d,    only: DUST
      use vrbls3d,    only: ZMID, T, Q, PMID, ICING_GFIP, UH, VH
      use vrbls2d,    only: FIS
      use masks,      only: LMH
      use params_mod, only: GI, G
      use ctlblk_mod, only: JSTA, JEND, SPVAL, JSTA_2L, JEND_2U, LM, JSTA_M, &
                            JEND_M, HTFD, NFD, IM, JM, NBIN_DU, gocart_on,   &
                            MODELNAME
      use gridspec_mod, only: GRIDTYPE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!     SET NUMBER OF FD LEVELS.
!jw      integer,intent(in) :: NFD ! coming from calling subroutine
!     
!     DECLARE VARIABLES
!     
      integer,intent(in) ::  ITYPE(NFD)
!jw      real,intent(in) :: HTFD(NFD)
      real,dimension(IM,JSTA:JEND,NFD),intent(out) :: TFD,QFD,UFD,VFD,PFD,ICINGFD
      real,dimension(IM,JSTA:JEND,NFD,NBIN_DU),intent(out) :: AERFD
!
      INTEGER LVL(NFD),LHL(NFD)
      INTEGER IVE(JM),IVW(JM)
      REAL DZABV(NFD), DZABH(NFD)
      LOGICAL DONEH, DONEV
!jw
      integer I,J,JVS,JVN,IE,IW,JN,JS,JNT,L,LLMH,IFD,N
      integer ISTART,ISTOP,JSTART,JSTOP
      real htt,htsfc,httuv,dz,rdz,delt,delq,delu,delv,z1,z2,htabv,htabh,htsfcv
!
!     SET FD LEVEL HEIGHTS IN METERS.
!      DATA HTFD  / 30.E0,50.E0,80.E0,100.E0,305.E0,457.E0,610.E0,914.E0,1524.E0,  &
!          1829.E0,2134.E0,2743.E0,3658.E0,4572.E0,6000.E0/
!     
!****************************************************************
!     START FDLVL HERE
!     
!     INITIALIZE ARRAYS.
!     
!$omp  parallel do
      DO IFD = 1,NFD
        DO J=JSTA,JEND
          DO I=1,IM
            TFD(I,J,IFD)     = SPVAL
            QFD(I,J,IFD)     = SPVAL
            UFD(I,J,IFD)     = SPVAL
            VFD(I,J,IFD)     = SPVAL
            PFD(I,J,IFD)     = SPVAL
            ICINGFD(I,J,IFD) = SPVAL
          ENDDO
        ENDDO
      ENDDO
      if (gocart_on) then
        DO N = 1, NBIN_DU
          DO IFD = 1,NFD
            DO J=JSTA,JEND
              DO I=1,IM
                AERFD(I,J,IFD,N) = SPVAL
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      endif

      IF(gridtype == 'E') THEN
        JVN =  1
        JVS = -1
        do J=JSTA,JEND
          IVE(J) = MOD(J,2)
          IVW(J) = IVE(J)-1
        enddo
      END IF

      IF(gridtype /= 'A')THEN
        CALL EXCH(FIS(1:IM,JSTA_2L:JEND_2U))
        DO L=1,LM
          CALL EXCH(ZMID(1:IM,JSTA_2L:JEND_2U,L))
        END DO
        ISTART = 2
        ISTOP  = IM-1
        JSTART = JSTA_M
        JSTOP  = JEND_M
      ELSE
        ISTART = 1
        ISTOP  = IM
        JSTART = JSTA
        JSTOP  = JEND
      END IF
      DO IFD = 1, NFD
!
!     MSL FD LEVELS
!
        IF (ITYPE(IFD).EQ.1) THEN
!	write(6,*) 'computing above MSL'
!     
!       LOOP OVER HORIZONTAL GRID.
!	  
          DO J=JSTART,JSTOP
            DO I=ISTART,ISTOP
              HTSFC = FIS(I,J)*GI
              LLMH  = NINT(LMH(I,J))
!             IFD = 1
!     
!        LOCATE VERTICAL INDICES OF T,Q,U,V, LEVEL JUST 
!        ABOVE EACH FD LEVEL.
!
!        DO 22 IFD = 1, NFD
              DONEH=.FALSE.
              DONEV=.FALSE.
              DO L = LM,1,-1
                HTT = ZMID(I,J,L)
                IF(gridtype == 'E') THEN
                  IE = I+IVE(J)
                  IW = I+IVW(J)
                  JN = J+JVN
                  JS = J+JVS
                  HTTUV = 0.25*(ZMID(IW,J,L)                          &
                        + ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(I,JS,L))
                ELSE IF(gridtype=='B')THEN
                  IE = I+1
                  IW = I
                  JN = J+1
                  JS = J
                  HTTUV = 0.25*(ZMID(IW,J,L)                          &
                        + ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(IE,JN,L)) 
                ELSE
                  HTTUV = HTT
                END IF
    
                IF (.NOT. DONEH .AND. HTT.GT.HTFD(IFD)) THEN
                  LHL(IFD)   = L
                  DZABH(IFD) = HTT-HTFD(IFD)
                  DONEH = .TRUE.
! THIS SHOULD SET BELOW GROUND VALUES TO SPVAL
                  IF(HTSFC > HTFD(IFD)) THEN
!mp
                     LHL(IFD) = LM+1  ! CHUANG: changed to lm+1
!mp
                  ENDIF
! THIS SHOULD SET BELOW GROUND VALUES TO SPVAL
!               IFD        = IFD + 1
!               IF (IFD.GT.NFD) GOTO 30
                END IF   
     
                IF (.NOT. DONEV .AND. HTTUV.GT.HTFD(IFD)) THEN
                  LVL(IFD)   = L
                  DZABV(IFD) = HTTUV-HTFD(IFD)
                  DONEV=.TRUE.
! THIS SHOULD SET BELOW GROUND VALUES TO SPVAL
                  IF(HTSFC.GT.HTFD(IFD)) THEN
!mp
                    LVL(IFD)=LM+1  ! CHUANG: changed to lm+1
!mp
                  ENDIF
! THIS SHOULD SET BELOW GROUND VALUES TO SPVAL
!               IFD        = IFD + 1
!               IF (IFD.GT.NFD) GOTO 30
                ENDIF
    
                IF(DONEH .AND. DONEV) exit
              enddo           ! end of l loop
! 22     CONTINUE   	
!     
!        COMPUTE T, Q, U, AND V AT FD LEVELS.
!
!         DO 40 IFD = 1,NFD
 
              L = LHL(IFD)
              IF (L < LM) THEN
                DZ   = ZMID(I,J,L)-ZMID(I,J,L+1)
                RDZ  = 1./DZ
                DELT = T(I,J,L)-T(I,J,L+1)
                DELQ = Q(I,J,L)-Q(I,J,L+1)
                TFD(I,J,IFD) = T(I,J,L) - DELT*RDZ*DZABH(IFD)
                QFD(I,J,IFD) = Q(I,J,L) - DELQ*RDZ*DZABH(IFD)
                PFD(I,J,IFD) = PMID(I,J,L) - (PMID(I,J,L)-PMID(I,J,L+1))*RDZ*DZABH(IFD)
                ICINGFD(I,J,IFD) = ICING_GFIP(I,J,L) - &
                 (ICING_GFIP(I,J,L)-ICING_GFIP(I,J,L+1))*RDZ*DZABH(IFD)
                if (gocart_on) then
                  DO N = 1, NBIN_DU
                    AERFD(I,J,IFD,N) = DUST(I,J,L,N) - &
                        (DUST(I,J,L,N)-DUST(I,J,L+1,N))*RDZ*DZABH(IFD)
                  ENDDO
                endif
              ELSEIF (L == LM) THEN
                TFD(I,J,IFD) = T(I,J,L)
                QFD(I,J,IFD) = Q(I,J,L)
                PFD(I,J,IFD) = PMID(I,J,L)
                ICINGFD(I,J,IFD) = ICING_GFIP(I,J,L)
                if (gocart_on) then
                  DO N = 1, NBIN_DU
                    AERFD(I,J,IFD,N) = DUST(I,J,L,N)
                  ENDDO
                endif
              ENDIF
    
              L = LVL(IFD)
              IF (L < LM) THEN
                IF(gridtype == 'E')THEN
                  IE = I+IVE(J)
                  IW = I+IVW(J)
                  JN = J+JVN
                  JS = J+JVS
                  Z1 = 0.25*(ZMID(IW,J,L)                              &
                     + ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(I,JS,L))
                  Z2 = 0.25*(ZMID(IW,J,L+1)                            &
                     + ZMID(IE,J,L+1)+ZMID(I,JN,L+1)+ZMID(I,JS,L+1))
                  DZ = Z1-Z2
       
                ELSE IF(gridtype=='B')THEN
                  IE =I+1
                  IW = I
                  JN = J+1
                  JS = J
                  Z1 = 0.25*(ZMID(IW,J,L)                              &
                     + ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(IE,JN,L))
                  Z2 = 0.25*(ZMID(IW,J,L+1)                            &
                     + ZMID(IE,J,L+1)+ZMID(I,JN,L+1)+ZMID(IE,JN,L+1))
                  DZ = Z1-Z2
                ELSE 
                  DZ   = ZMID(I,J,L)-ZMID(I,J,L+1)
                END IF 
                RDZ  = 1./DZ
                DELU = UH(I,J,L) - UH(I,J,L+1)
                DELV = VH(I,J,L) - VH(I,J,L+1)
                UFD(I,J,IFD) = UH(I,J,L) - DELU*RDZ*DZABV(IFD)
                VFD(I,J,IFD) = VH(I,J,L) - DELV*RDZ*DZABV(IFD)
              ELSEIF (L.EQ.LM) THEN
                UFD(I,J,IFD)=UH(I,J,L)
                VFD(I,J,IFD)=VH(I,J,L)
              ENDIF
! 40      CONTINUE
!     
!     COMPUTE FD LEVEL T, Q, U, AND V AT NEXT K.
!
            enddo        ! end of i loop
          enddo          ! end of j loop
!     END OF MSL FD LEVELS
        ELSE
!          write(6,*) 'computing above AGL'
!
!     AGL FD LEVELS 
!
!     
!     LOOP OVER HORIZONTAL GRID.
!     
          DO J=JSTART,JSTOP
            DO I=ISTART,ISTOP
              HTSFC = FIS(I,J)*GI
              IF(gridtype == 'E') THEN
                IE = I+IVE(J)
                IW = I+IVW(J)
                JN = J+JVN
                JS = J+JVS
                HTSFCV = (FIS(IW,J)+FIS(IE,J)+FIS(I,JN)+FIS(I,JS))*(0.25/G)
              ELSE IF(gridtype == 'B')THEN
                IE = I+1
                IW = I
                JN = J+1
                JS = J
                HTSFCV = (FIS(IW,J)+FIS(IE,J)+FIS(I,JN)+FIS(IE,JN))*(0.25/G)
              END IF
              LLMH  = NINT(LMH(I,J))
!             IFD   = 1
!     
!        LOCATE VERTICAL INDICES OF T,U,V, LEVEL JUST 
!        ABOVE EACH FD LEVEL.
!
!             DO 222 IFD = 1, NFD
              DONEH=.FALSE.
              DONEV=.FALSE.
              DO L = LLMH,1,-1
                HTABH = ZMID(I,J,L)-HTSFC
!                if(i==245.and.j==813)print*,'Debug FDL HTABH= ',htabh,zmid(i,j,l),htsfc
                IF(gridtype=='E')THEN
                  HTABV = 0.25*(ZMID(IW,J,L)                        &
                        + ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(I,JS,L))-HTSFCV
                ELSE IF(gridtype=='B')THEN
                  HTABV = 0.25*(ZMID(IW,J,L)                        &
                        + ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(IE,JN,L))-HTSFCV
                ELSE
                  HTABV = HTABH
                END IF
    
                IF (.NOT. DONEH .AND. HTABH.GT.HTFD(IFD)) THEN
                  LHL(IFD)   = L
                  DZABH(IFD) = HTABH-HTFD(IFD)
                  DONEH=.TRUE.
!                 IFD        = IFD + 1
!                 IF (IFD.GT.NFD) GOTO 230
                ENDIF

                IF (.NOT. DONEV .AND. HTABV.GT.HTFD(IFD)) THEN
                  LVL(IFD)   = L
                  DZABV(IFD) = HTABV-HTFD(IFD)
                  DONEV = .TRUE.
!                 IFD        = IFD + 1
!                 IF (IFD.GT.NFD) GOTO 230
                ENDIF
                IF(DONEH .AND. DONEV) exit
              enddo        ! end of l loop
!     
!        COMPUTE T, Q, U, AND V AT FD LEVELS.
!
! 222     CONTINUE
!
!             DO 240 IFD = 1,NFD
               L = LHL(IFD)
               IF (L.LT.LM) THEN
                 DZ   = ZMID(I,J,L)-ZMID(I,J,L+1)
                 RDZ  = 1./DZ
                 DELT = T(I,J,L)-T(I,J,L+1)
                 DELQ = Q(I,J,L)-Q(I,J,L+1)
                 TFD(I,J,IFD) = T(I,J,L) - DELT*RDZ*DZABH(IFD)
                 QFD(I,J,IFD) = Q(I,J,L) - DELQ*RDZ*DZABH(IFD)
                 PFD(I,J,IFD) = PMID(I,J,L) - (PMID(I,J,L)-PMID(I,J,L+1))*RDZ*DZABH(IFD)
                 ICINGFD(I,J,IFD) = ICING_GFIP(I,J,L) - &
                   (ICING_GFIP(I,J,L)-ICING_GFIP(I,J,L+1))*RDZ*DZABH(IFD)
                 if (gocart_on) then
                   DO N = 1, NBIN_DU
                     AERFD(I,J,IFD,N) = DUST(I,J,L,N) - &
                    (DUST(I,J,L,N)-DUST(I,J,L+1,N))*RDZ*DZABH(IFD)
                   ENDDO
                 endif
               ELSE
                 TFD(I,J,IFD) = T(I,J,L)
                 QFD(I,J,IFD) = Q(I,J,L)
                 PFD(I,J,IFD) = PMID(I,J,L)
                 ICINGFD(I,J,IFD) = ICING_GFIP(I,J,L)
                 if (gocart_on) then
                   DO N = 1, NBIN_DU
                     AERFD(I,J,IFD,N) = DUST(I,J,L,N)
                   ENDDO
                 endif
               ENDIF

               L = LVL(IFD)
               IF (L < LM) THEN
                 IF(gridtype == 'E')THEN
                   IE = I+IVE(J)
                   IW = I+IVW(J)
                   JN = J+JVN
                   JS = J+JVS
                   Z1 = 0.25*(ZMID(IW,J,L)                          &
                      + ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(I,JS,L))
                   Z2 = 0.25*(ZMID(IW,J,L+1)                        &
                      + ZMID(IE,J,L+1)+ZMID(I,JN,L+1)+ZMID(I,JS,L+1))
                   DZ = Z1-Z2
                 ELSE IF(gridtype=='B')THEN
                   IE = I+1
                   IW = I
                   JN = J+1
                   JS = J
                   Z1 = 0.25*(ZMID(IW,J,L)                          &
                      + ZMID(IE,J,L)+ZMID(I,JN,L)+ZMID(IE,JN,L))
                   Z2 = 0.25*(ZMID(IW,J,L+1)                        &
                      + ZMID(IE,J,L+1)+ZMID(I,JN,L+1)+ZMID(IE,JN,L+1))
                   DZ = Z1-Z2
                 ELSE
                   DZ   = ZMID(I,J,L)-ZMID(I,J,L+1)
                 END IF
                 RDZ  = 1./DZ
                 DELU = UH(I,J,L)-UH(I,J,L+1)
                 DELV = VH(I,J,L)-VH(I,J,L+1)
                 UFD(I,J,IFD) = UH(I,J,L) - DELU*RDZ*DZABV(IFD)
                 VFD(I,J,IFD) = VH(I,J,L) - DELV*RDZ*DZABV(IFD)
               ELSE
                 UFD(I,J,IFD) = UH(I,J,L)
                 VFD(I,J,IFD) = VH(I,J,L)
              ENDIF
! 240     CONTINUE
!     
!     COMPUTE FD LEVEL T, U, AND V AT NEXT K.
!
            enddo      ! end of i loop
          enddo        ! end of j loop
!     END OF AGL FD LEVELS
        ENDIF
      enddo          ! end of IFD loop

!  safety check to avoid tiny QFD values
     IF(MODELNAME == 'RAPR') THEN
       DO 420 IFD = 1,NFD
         DO J=JSTA,JEND
         DO I=1,IM
            if(QFD(I,J,IFD) < 1.0e-8) QFD(I,J,IFD)=0.0
         ENDDO
         ENDDO
420    CONTINUE
     endif
!
!     END OF ROUTINE.
!
      RETURN
      END

      SUBROUTINE FDLVL_MASS(ITYPE,NFD,HTFD,QIN,QFD)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    FDLVL_MASS       COMPUTES FD LEVEL FOR MASS-RELATED VARIABLE
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-22       
!     
! ABSTRACT:
!     THIS ROUTINE COMPUTES MASS-RELATED VARIABLE (TEMPERATURE, SPEC. HUM...)
!     ON THE NFD(=6 by default) FD LEVELS.  THE
!     HEIGHT OF THESE LEVELS (IN METERS) IS GIVEN IN THE 
!     DATA STATEMENT BELOW.  THE ALGORITHM PROCEEDS AS 
!     FOLLOWS. (AGL IN PARENTHESES)
!     
!     AT EACH MASS POINT MOVE UP VERTICALLY FROM THE LM-TH (LOWEST
!     ATMOSPHERIC) ETA LAYER.  FIND THE ETA LAYERS WHOSE 
!     HEIGHT (ABOVE GROUND) BOUNDS THE TARGET FD LEVEL HEIGHT.
!     VERTICALLY INTERPOLATE TO GET TEMPERATURE AT THIS FD
!     LEVEL.  AVERAGE THE FOUR SURROUNDING WINDS
!     TO GET A MASS POINT WIND.  VERTICALLY INTERPOLATE THESE
!     MASS POINT WINDS TO THE TARGET FD LEVEL.  CONTINUE THIS
!     PROCESS UNTIL ALL NFD=6 FD LEVELS HAVE BEEN PROCESSED.
!     MOVE ON TO THE NEXT MASS POINT.  
!     
!     AVERAGING THE FOUR ABOVE GROUND WINDS TO THE MASS POINT
!     WAS FOUND TO SMOOTH THE FIELD AND REDUCE THE OCCURRENCE
!     OF POINT PEAK WINDS FAR IN EXCESS OF THE WINDS AT 
!     ADJACENT POINTS.  MASS POINT VALUES ARE RETURNED.
!   .     
!     
! PROGRAM HISTORY LOG:
!   92-12-22  RUSS TREADON
!   93-11-23  RUSS TREADON - CORRECTED ROUTINE TO COMPUTE
!             FD LEVELS WITH REPECT TO MEAN SEA LEVEL.
!   94-01-04  MICHAEL BALDWIN - INCLUDE OPTIONS FOR COMPUTING
!                               EITHER AGL OR MSL
!   98-06-15  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION            
!   02-01-15  MIKE BALDWIN - WRF VERSION
!   11-12-14  SARAH LU - ADD GOCART AEROSOL AERFD
!   17-06-01  Y Mao - ADD FD levels for GTG(EDPARM CATEDR MWTURB) and allow 
!                     levels input from control file
!     
! USAGE:    CALL FDLVL_MASS(ITYPE,NFD,HTFD,QIN,QFD)
!   INPUT ARGUMENT LIST:
!     ITYPE    - FLAG THAT DETERMINES WHETHER MSL (1) OR AGL (2)
!                   LEVELS ARE USED.
!     NFD      - NUMBER OF FD LEVELS
!     HTFD     - FD LEVELS
!     QIN      - MASS POINT VALUE ON MODEL LEVELS
!
!   OUTPUT ARGUMENT LIST: 
!     QFD      - MASS POINT VALUE ON FD LEVELS.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!
!     LIBRARY:
!       COMMON   - 
!                  LOOPS
!                  MASKS
!                  OPTIONS
!                  INDX
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!

      use vrbls3d,    only: ZMID
      use vrbls2d,    only: FIS
      use masks,      only: LMH
      use params_mod, only: GI, G
      use ctlblk_mod, only: JSTA, JEND, SPVAL, JSTA_2L, JEND_2U, LM, JSTA_M, &
                            JEND_M, IM, JM,global
      use gridspec_mod, only: GRIDTYPE
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
!     SET NUMBER OF FD LEVELS.
!     
!     DECLARE VARIABLES
!     
      integer,intent(in) ::  ITYPE(NFD)
      integer,intent(in) :: NFD ! coming from calling subroutine
      real,intent(in) :: HTFD(NFD)
      real,intent(in) :: QIN(IM,JSTA_2L:JEND_2U,LM)
      real,dimension(IM,JSTA:JEND,NFD),intent(out) :: QFD

!
      INTEGER LHL(NFD)
      REAL DZABH(NFD)
      LOGICAL DONEH
!jw
      integer I,J,L,LLMH,IFD
      real htt,htsfc,dz,rdz,delq,htabh
!
!      Default FD LEVEL HEIGHTS IN METERS.
!      DATA HTFD  / 30.E0,50.E0,80.E0,100.E0,305.E0,457.E0,610.E0,914.E0,1524.E0,  &
!          1829.E0,2134.E0,2743.E0,3658.E0,4572.E0,6000.E0/
!     
!****************************************************************
!     START FDLVL_MASS HERE
!     
!     INITIALIZE ARRAYS.
!     
!$omp  parallel do
      DO IFD = 1,NFD
        DO J=JSTA,JEND
          DO I=1,IM
            QFD(I,J,IFD)     = SPVAL
          ENDDO
        ENDDO
      ENDDO

      DO IFD = 1, NFD
!
!     MSL FD LEVELS
!
        IF (ITYPE(IFD) == 1) THEN
!	write(6,*) 'computing above MSL'
!     
!       LOOP OVER HORIZONTAL GRID.
!	  
          DO J=JSTA,JEND
            DO I=1,IM
              HTSFC = FIS(I,J)*GI
              LLMH  = LM
!             IFD = 1
!     
!        LOCATE VERTICAL INDICES OF Q, LEVEL JUST 
!        ABOVE EACH FD LEVEL.
!
!        DO 22 IFD = 1, NFD
              DONEH=.FALSE.
              DO L = LLMH,1,-1
                HTT = ZMID(I,J,L)
    
                IF (.NOT. DONEH .AND. HTT > HTFD(IFD)) THEN
                  LHL(IFD)   = L
                  DZABH(IFD) = HTT-HTFD(IFD)
                  DONEH = .TRUE.
! THIS SHOULD SET BELOW GROUND VALUES TO SPVAL
                  IF(HTSFC > HTFD(IFD)) THEN
!mp
                     LHL(IFD) = LM+1  ! CHUANG: changed to lm+1
!mp
                  ENDIF
! THIS SHOULD SET BELOW GROUND VALUES TO SPVAL
!               IFD        = IFD + 1
!               IF (IFD.GT.NFD) GOTO 30
                END IF   

                IF(DONEH) exit
              enddo           ! end of l loop
! 22     CONTINUE   	
!     
!        COMPUTE Q AT FD LEVELS.
!
!         DO 40 IFD = 1,NFD
 
              L = LHL(IFD)
              IF (L < LM) THEN
                DZ   = ZMID(I,J,L)-ZMID(I,J,L+1)
                RDZ  = 1./DZ
                DELQ = QIN(I,J,L)-QIN(I,J,L+1)
                if(QIN(I,J,L)<SPVAL) then
                   QFD(I,J,IFD)=QIN(I,J,L+1)
                elseif(QIN(I,J,L+1)<SPVAL) then
                   QFD(I,J,IFD)=QIN(I,J,L)
                else
                   QFD(I,J,IFD) = QIN(I,J,L) - DELQ*RDZ*DZABH(IFD)
                endif
              ELSEIF (L == LM) THEN
                QFD(I,J,IFD) = QIN(I,J,L)
              ENDIF
    
! 40      CONTINUE
!     
!     COMPUTE FD LEVEL Q AT NEXT K.
!
            enddo        ! end of i loop
          enddo          ! end of j loop
!     END OF MSL FD LEVELS
        ELSE
!          write(6,*) 'computing above AGL'
!
!     AGL FD LEVELS 
!
!     
!     LOOP OVER HORIZONTAL GRID.
!     
          DO J=JSTA,JEND
            DO I=1,IM
              HTSFC = FIS(I,J)*GI
              LLMH  = NINT(LMH(I,J))
!             IFD   = 1
!     
!        LOCATE VERTICAL INDICES OF Q, LEVEL JUST 
!        ABOVE EACH FD LEVEL.
!
!             DO 222 IFD = 1, NFD
              DONEH=.FALSE.
              DO L = LLMH,1,-1
                HTABH = ZMID(I,J,L)-HTSFC

                if(i==245.and.j==813)print*,'Debug FDL HTABH= ',htabh,zmid(i,j,l),htsfc
    
                IF (.NOT. DONEH .AND. HTABH.GT.HTFD(IFD)) THEN
                  LHL(IFD)   = L
                  DZABH(IFD) = HTABH-HTFD(IFD)
                  DONEH=.TRUE.
!                 IFD        = IFD + 1
!                 IF (IFD.GT.NFD) GOTO 230
                ENDIF

                IF(DONEH) exit
              enddo        ! end of l loop
!     
!        COMPUTE Q AT FD LEVELS.
!
! 222     CONTINUE
!
!             DO 240 IFD = 1,NFD
               L = LHL(IFD)
               IF (L.LT.LM) THEN
                 DZ   = ZMID(I,J,L)-ZMID(I,J,L+1)
                 RDZ  = 1./DZ
                 DELQ = QIN(I,J,L)-QIN(I,J,L+1)
                 if(QIN(I,J,L)<SPVAL) then
                    QFD(I,J,IFD)=QIN(I,J,L+1)
                 elseif(QIN(I,J,L+1)<SPVAL) then
                    QFD(I,J,IFD)=QIN(I,J,L)
                 else
                    QFD(I,J,IFD) = QIN(I,J,L) - DELQ*RDZ*DZABH(IFD)
                 endif
               ELSE
                 QFD(I,J,IFD) = QIN(I,J,L)
               ENDIF

! 240     CONTINUE
!     
!     COMPUTE FD LEVEL Q AT NEXT K.
!
            enddo      ! end of i loop
          enddo        ! end of j loop
!     END OF AGL FD LEVELS
        ENDIF
      enddo          ! end of IFD loop

!
!     END OF ROUTINE.
!
      RETURN
      END
