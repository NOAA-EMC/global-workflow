!----------------------------------------------------------------------
      subroutine vadjust(VALIDPT,U,V,HTOPO,DX,DY,IM,JM,gdin)
!----------------------------------------------------------------------

! --- FROM CALMET   Version: 5.8        Level: 050328                 ADJUST

!      THIS ROUTINE ADJUSTS SURFACE WINDS FOR TERRAIN EFFECTS
!     INPUTS:  U (R ARRAY)     - GRIDDED X-DIRECTION WIND COMPONENTS
!              V (R ARRAY)     - GRIDDED Y-DIRECTION WIND COMPONENTS
!              HTOPO (R ARRAY) - GRIDDED TERRAIN HEIGHTS
!              HBAR (R ARRAY)  - MODEL LEVELS HEIGHTS
!              UB (R ARRAY)    - U-COMPONENT BOUNDARY VALUES
!              VB (R ARRAY)    - V-COMPONENT BOUNDARY VALUES
!       Parameters: MXNX, MXNY, MXNZ, MXNZP1

!     OUTPUTS:  U (R ARRAY) - X-DIRECTION WIND COMPONENTS WITH
!                             ADJUSTED SURFACE LAYER WINDS
!               V (R ARRAY) - Y-DIRECTION WIND COMPONENTS WITH
!                             ADJUSTED SURFACE LAYER WINDS

    use constants
    use grddef
    use aset2d
    use aset3d

    LOGICAL, INTENT(IN) :: VALIDPT(:,:)
    REAL, INTENT(INOUT) :: U(:,:),V(:,:)
    REAL, INTENT(IN) :: HTOPO(:,:),DX,DY
    TYPE (GINFO)        :: GDIN
    REAL, ALLOCATABLE   :: PHI(:,:,:)
    real HBAR,DXI,DYI,FX,FY,HTOIM1,HTOJM1,HTOIP1,HTOJP1,DHDX,DHDY, &
         DXSQ,DYSQ,DSQ,FACT,ERROR,ERR,EPSI,OVREL,XX,YY
    integer itmax,ii,jj,kk,idir,it

!     ITERATION CRITERIA
      DATA ITMAX,EPSI,OVREL/75,0.02,1.5/

    INTERFACE
    SUBROUTINE setphibnd(validpt,nx,ny,phi)
!==========================================================
!     Set PHI at validpt boundaries
!==========================================================
      LOGICAL, INTENT(IN) :: VALIDPT(:,:)
      REAL, INTENT(INOUT) :: PHI(:,:,:)
      INTEGER, INTENT(IN) :: NX,NY
     END SUBROUTINE setphibnd
    END INTERFACE

      KK = 1
      NX=IM;NY=JM
      ALLOCATE (PHI(NX,NY,2),STAT=kret)
      print *,'VADJUST:  DX  DY  NX NY', DX,DY,NX,NY

!     COMPUTE TERRAIN GRADIENTS AND INITIAL POTENTIAL
      PHI=0.1
      DXI=0.5/DX
      DYI=0.5/DY
      do j=2,ny-1
      do i=2,nx-1
       if(validpt(i,j)) then
         HBAR=HGHT(I,J,1)
         FX=DXI/(HBAR)
         FY=DYI/(HBAR)
         HTOIM1=HTOPO(I,J)
         HTOJM1=HTOPO(I,J)
         HTOIP1=HTOPO(I,J)
         HTOJP1=HTOPO(I,J)
         IF(validpt(i-1,j)) HTOIM1=HTOPO(I-1,J)
         IF(validpt(i,j-1)) HTOJM1=HTOPO(I,J-1)
         IF(validpt(i+1,j)) HTOIP1=HTOPO(I+1,J)
         IF(validpt(i,j+1)) HTOJP1=HTOPO(I,J+1)

         DHDX=(HTOIP1-HTOIM1)*FX
         DHDY=(HTOJP1-HTOJM1)*FY
         PHI(I,J,2)=(U(I,J)*DHDX+V(I,J)*DHDY)

!     CALCULATE THE VERTICAL VELOCITY DUE TO TOPOGRAPHIC EFFECTS (JTM)
!          WTOPO=U(I,J)*DHDX+V(I,J)*DHDY
!          PHI(I,J,1)=WTOPO
!          TEST assume terrain following winds --no topo effects
         PHI(I,J,1)=0.1
       else
         PHI(I,J,1)=0.1
         PHI(I,J,2)=0.1
       endif
      enddo
      enddo

!     SET BOUNDARY VALUES FOR PHI
      call setphibnd(validpt,nx,ny,phi)

!     SOLVE POISSON EQUATION BY GAUSS-SEIDEL METHOD FOR
!     VELOCITY POTENTIAL

      DXSQ=DX*DX
      DYSQ=DY*DY
      DSQ=DXSQ*DYSQ
      FACT=1.0/(2.0*(DXSQ+DYSQ))
      DO 100 IT=1,ITMAX
        DO 90 IDIR=1,4
          ERROR=-1.0E+09
          do jj=2,ny-1
          do ii=2,nx-1
            SELECT CASE (IDIR)
             CASE (1)
              I=II
              J=JJ
             CASE(2)  
              I=NX-II+1
              J=JJ
             CASE (3) 
              I=II
              J=NY-JJ+1
             CASE(4)
              I=NX-II+1
              J=NY-JJ+1
           END SELECT

           if(validpt(i,j)) then
             XOLD=PHI(I,J,KK)
             PHIIM1=PHI(I,J,KK)
             PHIJM1=PHI(I,J,KK)
             PHIIP1=PHI(I,J,KK)
             PHIJP1=PHI(I,J,KK)
             IF(validpt(i-1,j)) PHIIM1=PHI(I-1,J,KK)
             IF(validpt(i,j-1)) PHIJM1=PHI(I,J-1,KK)
             IF(validpt(i+1,j)) PHIIP1=PHI(I+1,J,KK)
             IF(validpt(i,j+1)) PHIJP1=PHI(I,J+1,KK)

             XX=DYSQ*(PHIIP1+PHIIM1)
             YY=DXSQ*(PHIJP1+PHIJM1)
!==================================================================================
             PHI(I,J,KK) = (1.-OVREL)*PHI(I,J,KK)+ OVREL*FACT*(XX+YY-DSQ*PHI(I,J,2))
!==================================================================================
           else
             PHI(I,J,KK)=0.1;XOLD=0.1;PHIIM1=0.1;PHIJM1=0.1;PHIIP1=.1;PHIJP1=0.1
           endif
           IF(ABS(XOLD).GE.1.0E-10) THEN     
             ERR=ABS((PHI(I,J,KK)-XOLD)/XOLD)
             ERROR=AMAX1(ERR,ERROR)
           ENDIF
         enddo
         enddo 
   90   CONTINUE
        print *,'VADJUST :  ERROR',IT, IDIR, ERROR,' EPSI',EPSI,' XOLD',PHIOLD
        IF (ERROR.LE.EPSI) exit
  100 CONTINUE

! Set PHI at validpt boundaries
      call setphibnd(validpt,nx,ny,phi)

      print *,'VADJUST PHI 1 :POIS ', MINVAL(PHI(:,:,1)),MAXVAL(PHI(:,:,1))
      print *,'VADJUST PHI 2 :', MINVAL(PHI(:,:,2)),MAXVAL(PHI(:,:,2))

!     COMPUTE WIND COMPONENTS FROM VELOCITY POTENTIAL
      do j=2,ny-1
      do i=2,nx-1
        if (validpt(i,j)) then
          PHIIM1=PHI(I,J,KK)
          PHIJM1=PHI(I,J,KK)
          PHIIP1=PHI(I,J,KK)
          PHIJP1=PHI(I,J,KK)
          IF(validpt(i-1,j)) PHIIM1=PHI(I-1,J,KK)
          IF(validpt(i,j-1)) PHIJM1=PHI(I,J-1,KK)
          IF(validpt(i+1,j)) PHIIP1=PHI(I+1,J,KK)
          IF(validpt(i,j+1)) PHIJP1=PHI(I,J+1,KK)
          UOLD=U(I,J)
          VOLD=V(I,J)
          U(I,J)=(PHIIP1-PHIIM1)*DXI+U(I,J)
          V(I,J)=(PHIJP1-PHIJM1)*DYI+V(I,J)
          diffi=UOLD-U(i,j)
          diffj=VOLD-V(i,j)
          if (diffi.gt.10. .or. diffi.lt.-10.) then
            if(diffi.gt.10) diffi=10
            if(diffi.lt.-10) diffi=-10
            U(I,J)=UOLD+diffi
          endif
          if (diffj.gt.10. .or. diffj.lt.-10.)  then
            if(diffj.gt.10) diffj=10
            if(diffj.lt.-10) diffj=-10
            V(I,J)=VOLD+diffj
          endif
        endif
      enddo
      enddo
      print *,'VADJUST UWND  :',  MINVAL(U(:,:)),MAXVAL(U(:,:))
      print *,'VADJUST VWND  :',  MINVAL(V(:,:)),MAXVAL(V(:,:))

      RETURN
      END

      SUBROUTINE setphibnd(validpt,nx,ny,phi)
!==========================================================
!     Set PHI at validpt boundaries
!==========================================================
      LOGICAL, INTENT(IN) :: VALIDPT(:,:)
      REAL, INTENT(INOUT) :: PHI(:,:,:)
      INTEGER, INTENT(IN) :: NX,NY

      PHIAVG=SUM(PHI(:,:,1))/(NX*NY)
      do j=2,ny-1
      do i=2,nx-1
       if (.not.validpt(i,j))then
          if (validpt(i+1,j)) then
           PHI(I,J,1)=PHI(I+1,J,1)
          elseif (validpt(i,j+1)) then
           PHI(I,J,1)=PHI(I,J+1,1)
          elseif (validpt(i+1,j+1)) then
           PHI(I,J,1)=PHI(I+1,J+1,1)
          elseif (validpt(i-1,j)) then
           PHI(I,J,1)=PHI(I-1,J,1)
          elseif (validpt(i,j-1)) then
           PHI(I,J,1)=PHI(I,J-1,1)
          elseif (validpt(i-1,j-1)) then
           PHI(I,J,1)=PHI(I-1,J-1,1)
          elseif (validpt(i-1,j+1)) then
           PHI(I,J,1)=PHI(I-1,J+1,1)
          elseif (validpt(i+1,j-1)) then
           PHI(I,J,1)=PHI(I+1,J-1,1)
          else
           PHI(I,J,1)=PHIAVG
          endif
        endif
      enddo
      enddo

!     Set PHI at domain edge boundaries
      PHIAVG=SUM(PHI(:,:,1))/(NX*NY)
      print *, 'PHIAVG ',PHIAVG
      do j=1,ny
        if(validpt(2,j)) then
           PHI(1,J,1)=PHI(2,J,1)
        else
           PHI(1,J,1)=PHIAVG
        endif
        if(validpt(NX-1,j)) then
           PHI(NX,j,1)=PHI(NX-1,j,1)
        else
           PHI(NX,J,1)=PHIAVG
        endif
      enddo
      do i=1,nx
        if(validpt(i,1)) then
           PHI(i,1,1)=PHI(2,1,1)
        else
           PHI(i,1,1)=PHIAVG
        endif
        if(validpt(i,ny-1)) then
           PHI(i,ny,1)=PHI(i,ny-1,1)
        else
           PHI(i,ny,1)=PHIAVG
        endif
      enddo

      return
      end

