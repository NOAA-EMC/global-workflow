      SUBROUTINE CALTAU(TAUX,TAUY)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALTAU      COMPUTE U AND V WIND STRESSES
!   PRGRMMR: TREADON         ORG: W/NP2      DATE: 93-09-01
!     
! ABSTRACT:  THIS ROUTINE COMPUTES SURFACE LAYER U AND V
!   WIND COMPONENT STRESSES USING K THEORY AS PRESENTED
!   IN SECTION 8.4 OF "NUMBERICAL PREDICTION AND DYNAMIC
!   METEOROLOGY" BY HALTINER AND WILLIAMS (1980, JOHN WILEY
!   & SONS).
!   .     
!     
! PROGRAM HISTORY LOG:
!   93-09-01  RUSS TREADON
!   98-06-11  T BLACK - CONVERSION FROM 1-D TO 2-D
!   00-01-04  JIM TUCCILLO - MPI VERSION
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID OUTPUT
!   02-01-15  MIKE BALDWIN - WRF VERSION, OUTPUT IS ON MASS-POINTS
!   05-02-23  H CHUANG - COMPUTE STRESS FOR NMM ON WIND POINTS
!   05-07-07  BINBIN ZHOU - ADD RSM STRESS for A GRID     
! USAGE:    CALL CALTAU(TAUX,TAUY)
!   INPUT ARGUMENT LIST:
!     NONE     
!
!   OUTPUT ARGUMENT LIST: 
!     TAUX     - SUFACE LAYER U COMPONENT WIND STRESS.
!     TAUY     - SUFACE LAYER V COMPONENT WIND STRESS.
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       CLMAX
!       MIXLEN
!
!     LIBRARY:
!       COMMON   - 
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : CRAY C-90
!$$$  
!     
!
      use vrbls3d,      only: zint, pmid, q, t, uh, vh, el_pbl, zmid
      use vrbls2d,      only: z0, uz0, vz0
      use masks,        only: lmh
      use params_mod,   only:  d00, d50, h1, d608, rd, d25
      use ctlblk_mod,   only: jsta_2l, jend_2u, lm, jsta, jend, spval, jsta_m,&
                            jm, im, jend_m
      use gridspec_mod, only: gridtype

      implicit none
!
!     DECLARE VARIABLES.
      INTEGER, dimension(4)  :: KK(4)
      INTEGER, dimension(jm) :: ive, ivw
      REAL, dimension(im,jsta:jend), intent(inout) :: TAUX, TAUY
      REAL, ALLOCATABLE :: EL(:,:,:)
      REAL, dimension(im,jsta:jend) ::  EGRIDU,EGRIDV,EGRID4,EGRID5, EL0
      REAL UZ0V,VZ0V
      CHARACTER*1 AGRID
      integer I,J,LMHK,IE,IW,ii,jj
      real DZ,RDZ,RSFC,TV,RHO,ULMH,VLMH,DELUDZ,DELVDZ,ELSQR,ZINT1,    &
           ZINT2,Z0V,PSFC,TVV,QVV,ELV,ELV1,ELV2
!     
!********************************************************************
!     START CALTAU HERE.
!    
      ALLOCATE (EL(IM,JSTA_2L:JEND_2U,LM))
!
!     COMPUTE MASTER LENGTH SCALE.
!
!      CALL CLMAX(EL0,EGRIDU,EGRIDV,EGRID4,EGRID5)
!      CALL MIXLEN(EL0,EL)
!     
!     INITIALIZE OUTPUT AND WORK ARRAY TO ZERO.
!     
      DO J=JSTA,JEND
      DO I=1,IM
        EGRIDU(I,J) = D00
        EGRIDV(I,J) = D00
        TAUX(I,J)   = SPVAL
        TAUY(I,J)   = SPVAL
      ENDDO
      ENDDO
!     
!     COMPUTE SURFACE LAYER U AND V WIND STRESSES.
!
!     ASSUME THAT U AND V HAVE UPDATED HALOS
!
      IF(GRIDTYPE == 'A')THEN
       CALL CLMAX(EL0,EGRIDU,EGRIDV,EGRID4,EGRID5)
       CALL MIXLEN(EL0,EL)

       DO J=JSTA,JEND
       DO I=1,IM
!
        LMHK = NINT(LMH(I,J))
!
!       COMPUTE THICKNESS OF LAYER AT MASS POINT.
!
        DZ  = D50*(ZINT(I,J,LMHK)-ZINT(I,J,LMHK+1))
        DZ  = DZ-Z0(I,J)
        RDZ = 1./DZ
!
!        COMPUTE REPRESENTATIVE AIR DENSITY.
!
        PSFC = PMID(I,J,LMHK)
        TV   = (H1+D608*Q(I,J,LMHK))*T(I,J,LMHK)
        RHO  = PSFC/(RD*TV)
!     
!        COMPUTE A MEAN MASS POINT WIND IN THE 
!        FIRST ATMOSPHERIC ETA LAYER.
!
        ULMH = UH(I,J,LMHK)
        VLMH = VH(I,J,LMHK)
!
!       COMPUTE WIND SHEAR COMPONENTS ACROSS LAYER.
!
        DELUDZ = (ULMH-UZ0(I,J))*RDZ
        DELVDZ = (VLMH-VZ0(I,J))*RDZ
!     
!       COMPUTE U (EGRIDU) AND V (EGRIDV) WIND STRESSES.
!
        ELSQR     = EL(I,J,LMHK)*EL(I,J,LMHK)
        TAUX(I,J) = RHO*ELSQR*DELUDZ*DELUDZ
        TAUY(I,J) = RHO*ELSQR*DELVDZ*DELVDZ

!
       END DO
       END DO
      ELSE IF(GRIDTYPE == 'E')THEN
       call exch(ZINT(1,jsta_2l,LM))
       call exch(ZINT(1,jsta_2l,LM+1))
       call exch(Z0(1,jsta_2l))
       call exch(PMID(1,jsta_2l,LM))
       call exch(T(1,jsta_2l,LM))
       call exch(Q(1,jsta_2l,LM))
       call exch(EL_PBL(1,jsta_2l,LM))
       call exch(EL_PBL(1,jsta_2l,LM-1))

       DO J=JSTA_M,JEND_M
        IVE(J)=MOD(J,2)
        IVW(J)=IVE(J)-1
       ENDDO
 
       DO J=JSTA_M,JEND_M
       DO I=2,IM-1
!
        LMHK = NINT(LMH(I,J)) 
        IE=I+IVE(J)
        IW=I+IVW(J)
        ZINT1=(ZINT(IW,J,LMHK)+ZINT(IE,J,LMHK)                  &  
         +ZINT(I,J+1,LMHK)+ZINT(I,J-1,LMHK))*D25
        ZINT2=(ZINT(IW,J,LMHK+1)+ZINT(IE,J,LMHK+1)              &
         +ZINT(I,J+1,LMHK+1)+ZINT(I,J-1,LMHK+1))*D25
        DZ  = D50*(ZINT1-ZINT2)       
        Z0V=(Z0(IW,J)+Z0(IE,J)+Z0(I,J+1)+Z0(I,J-1))*D25
        DZ  = DZ-Z0V
        RDZ = 1./DZ
!
!        COMPUTE REPRESENTATIVE AIR DENSITY.
!
        PSFC = (PMID(IW,J,LMHK)+PMID(IE,J,LMHK)                &
         +PMID(I,J+1,LMHK)+PMID(I,J-1,LMHK))*D25 
        TVV = (T(IW,J,LMHK)+T(IE,J,LMHK)                       &
         +T(I,J+1,LMHK)+T(I,J-1,LMHK))*D25
        QVV = (Q(IW,J,LMHK)+Q(IE,J,LMHK)                       &
         +Q(I,J+1,LMHK)+Q(I,J-1,LMHK))*D25
        TV   = (H1+D608*QVV)*TVV
        RHO  = PSFC/(RD*TV) 

!       COMPUTE WIND SHEAR COMPONENTS ACROSS LAYER.
!
        DELUDZ = (UH(I,J,LMHK)-UZ0(I,J))*RDZ
        DELVDZ = (VH(I,J,LMHK)-VZ0(I,J))*RDZ 

!       COMPUTE U (EGRIDU) AND V (EGRIDV) WIND STRESSES.
!                                       
        ELV1=(EL_PBL(IW,J,LMHK)+EL_PBL(IE,J,LMHK)              &
         +EL_PBL(I,J+1,LMHK)+EL_PBL(I,J-1,LMHK))*D25
        ELV2=(EL_PBL(IW,J,LMHK-1)+EL_PBL(IE,J,LMHK-1)          &
         +EL_PBL(I,J+1,LMHK-1)+EL_PBL(I,J-1,LMHK-1))*D25
        ELV=(ELV1+ELV2)/2.0  ! EL is defined at the bottom of layer
        ELSQR       =ELV*ELV
        TAUX(I,J)=RHO*ELSQR*DELUDZ*DELUDZ 
        TAUY(I,J)=RHO*ELSQR*DELVDZ*DELVDZ
!	ii=im/2
!	jj=(jsta+jend)/2
!        if(i.eq.ii.and.j.eq.jj)print*,'sample tau'
!     &	,RHO,ELSQR,DELUDZ,DELVDZ  
       END DO
       END DO
      ELSE IF(GRIDTYPE == 'B')THEN
! PUT TAUX AND TAUY ON MASS POINTS      
       call exch(VH(1,jsta_2l,LM))
       DO J=JSTA_M,JEND_M
       DO I=2,IM-1
!
        LMHK = NINT(LMH(I,J))
!
!       COMPUTE THICKNESS OF LAYER AT MASS POINT.
!
!        DZ  = D50*(ZINT(I,J,LMHK)-ZINT(I,J,LMHK+1))
!        DZ  = ZMID(I,J,LMHK)-Z0(I,J)
        DZ=ZMID(I,J,LMHK)-(Z0(I,J)+ZINT(I,J,LMHK+1))
        if(DZ.eq.0.0)DZ=0.2
        RDZ = 1./DZ
!
!        COMPUTE REPRESENTATIVE AIR DENSITY.
!
        PSFC = PMID(I,J,LMHK)
        TV   = (H1+D608*Q(I,J,LMHK))*T(I,J,LMHK)
        RHO  = PSFC/(RD*TV)
!     
!        PUT U AND V ONTO MASS POINTS
!
        ULMH = 0.5*(UH(I-1,J,LMHK)+UH(I,J,LMHK))
        VLMH = 0.5*(VH(I,J-1,LMHK)+VH(I,J,LMHK))
!
!       COMPUTE WIND SHEAR COMPONENTS ACROSS LAYER.
!
        DELUDZ = (ULMH-UZ0(I,J))*RDZ
        DELVDZ = (VLMH-VZ0(I,J))*RDZ
!     
!       COMPUTE U (EGRIDU) AND V (EGRIDV) WIND STRESSES.
!
        ELV=0.5*(EL_PBL(I,J,LMHK)+EL_PBL(I,J,LMHK-1))
        ELSQR     = ELV*ELV
        TAUX(I,J) = RHO*ELSQR*DELUDZ*DELUDZ
!        if(TAUX(I,J)>1.0e2)print*,'Debug TAUX= ',i,j, &
!       ELV,ULMH,UZ0(I,J),ZMID(I,J,LMHK),Z0(I,J),RDZ,TAUX(I,J),zint(i,j,lm+1)
        TAUY(I,J) = RHO*ELSQR*DELVDZ*DELVDZ
	
       END DO
       END DO
      END IF
!     
      DEALLOCATE(EL)
!     END OF ROUTINE.
!     
      RETURN
      END
