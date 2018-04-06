      SUBROUTINE CALPBLREGIME(PBLREGIME)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .     
! SUBPROGRAM:    CALPBL COMPUTES PBL HEIGHT BASED ON BULK RCH NUMBER
!     
! ABSTRACT:  
!   THIS ROUTINE COMPUTES THE BULK RICHARDSON NUMBER BASED ON ALGORITHMS
!   FROM WRF SURFACE LAYER AND THEN DERIVE PBL REGIME AS FOLLOWS:
!        1. BR .GE. 0.2;
!               REPRESENTS NIGHTTIME STABLE CONDITIONS (REGIME=1),
!
!        2. BR .LT. 0.2 .AND. BR .GT. 0.0;
!               REPRESENTS DAMPED MECHANICAL TURBULENT CONDITIONS
!               (REGIME=2),
!
!        3. BR .EQ. 0.0
!               REPRESENTS FORCED CONVECTION CONDITIONS (REGIME=3),
!
!        4. BR .LT. 0.0
!               REPRESENTS FREE CONVECTION CONDITIONS (REGIME=4).    
!   .     
!     
! PROGRAM HISTORY LOG:
!   07-04-27  H CHUANG 
!   
! USAGE:    CALL CALPBLREGIME(PBLREGIME)
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST: 
!     PBLRI  - PBL HEIGHT ABOVE GROUND
!     
!   OUTPUT FILES:
!     NONE
!     
!   SUBPROGRAMS CALLED:
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON   - 
!                  CTLBLK
!     
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : 
!$$$  
!
      use vrbls3d,      only: uh, vh, pmid, t, q, pint, zmid, zint
      use vrbls2d,      only: ths, qs, smstav, twbs, qwbs, pblh
      use masks,        only: dx
      use params_mod,   only: p1000, capa, d608, h1, g, rd, cp
      use ctlblk_mod,   only: jsta, jend, spval, lm, jsta_m, jend_m, im,    &
                              jsta_2l, jend_2u
      use gridspec_mod, only: gridtype
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!     
!     INCLUDE,DERIVE,SET PARAMETERS.
!     
      REAL    , PARAMETER ::  VCONVC=1.
!     
!     DECLARE VARIABLES.
!     
      REAL,dimension(IM,jsta_2l:jend_2u),intent(inout) ::  PBLREGIME
!
      integer I,J,IE,IW,ii,jj
      real APE,THV,THVX,GOVRTH,UMASS,VMASS,WSPD,TSKV,DTHV,RHOX,fluxc,tsfc,  &
           VCONV,VSGD,BR,THX
     
!
!     
!*************************************************************************
!     
!     INITIALIZE ARRAYS.
!
!$omp  parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            PBLREGIME(I,J) = SPVAL
          ENDDO
        ENDDO
!
!     COMPUTE BULK RICHARDSON NUMBER AS CODED IN WRF module_sf_sfclay
!
!!$omp  parallel do
!!$omp& private(uhkl,ulkl,vhkl,vlkl,rib,ubot,utop,vbot,vtop,
!!$omp&         betta,ricr,ustarr,wmin,tvhtop,ztop,
!!$omp&         wndsl,wndslp,betta,ricr,ustarr,wmin 
!!$omp&       ,IFRSTLEV
!!$omp&       ,ICALPBL
!!$omp&       ,LVLP
!!$omp&       ,RIF
!!$omp&       ,RIBP
!!$omp&       ,UBOT1
!!$omp&       ,VBOT1
!!$omp&       ,ZBOT1
!!$omp&       ,THVBOT1)
!
      IF(GRIDTYPE /= 'A')THEN
         call exch(UH(1,jsta_2l,LM))
         call exch(VH(1,jsta_2l,LM))
      END IF
             
      DO J=JSTA_M,JEND_M
        DO I=2,IM-1
!
          APE  = (P1000/PMID(I,J,LM))**CAPA
          THX  = T(I,J,LM)*APE
          THVX = (Q(I,J,LM)*D608+H1)*THX
          GOVRTH = G/THX
          IF(GRIDTYPE == 'E')THEN
            IE=I+MOD(J+1,2) 
            IW=I+MOD(J+1,2)-1
            UMASS = (UH(I,J-1,LM)+UH(IW,J,LM)+UH(IE,J,LM)              &   
                  +  UH(I,J+1,LM))/4.0
            VMASS = (VH(I,J-1,LM)+VH(IW,J,LM)+VH(IE,J,LM)              &
                  +  VH(I,J+1,LM))/4.0
            WSPD= SQRT(UMASS*UMASS+VMASS*VMASS)
          ELSE IF(GRIDTYPE == 'B')THEN
            IE = I
            IW = I-1
            UMASS = (UH(IW,J-1,LM)+UH(IW,J,LM)+UH(IE,J-1,LM)              &   
                  +  UH(I,J,LM))/4.0
            VMASS = (VH(IW,J-1,LM)+VH(IW,J,LM)+VH(IE,J-1,LM)              &
                  +  VH(I,J,LM))/4.0
            WSPD= SQRT(UMASS*UMASS+VMASS*VMASS)  
          ELSE
            WSPD = SQRT(UH(I,J,LM)*UH(I,J,LM)+VH(I,J,LM)*VH(I,J,LM))
          END IF
                                                                                 
          TSKV = THS(I,J)*(1.+D608*QS(I,J)*SMSTAV(I,J))
          DTHV = (THVX-TSKV)
!  Convective velocity scale Vc and subgrid-scale velocity Vsg
!  following Beljaars (1995, QJRMS) and Mahrt and Sun (1995, MWR)
!                                ... HONG Aug. 2001
!
          rhox  = PINT(I,J,LM+1)/RD/(T(I,J,LM)*(Q(I,J,LM)*D608+H1)) !density
          fluxc = max(-twbs(i,j)/rhox/cp - d608*tskv*QWBS(i,j)/rhox,0.)
          tsfc  = THS(I,J)*(PINT(I,J,LM+1)/P1000)**CAPA
          VCONV = vconvc*(g/tsfc*pblh(i,j)*fluxc)**.33
! VCONV comes from Beljaars only
          VSGD  = 0.32 * (max(dx(i,j)/5000.-1.,0.))**.33
          WSPD  = SQRT(WSPD*WSPD+VCONV*VCONV+vsgd*vsgd)
          WSPD  = MAX(WSPD,0.1)
          BR    = GOVRTH*(ZMID(I,J,LM)-ZINT(I,J,LM+1))*DTHV/(WSPD*WSPD)
     
          IF(BR < 0.0) THEN
            PBLREGIME(I,J) = 4.0
          ELSE IF(BR == 0.0) THEN
            PBLREGIME(I,J) = 3.0
          ELSE IF(BR < 0.2) THEN
            PBLREGIME(I,J) = 2.0
          ELSE
            PBLREGIME(I,J) = 1.0 
          END IF

!         ii=im/2
!         jj=(jsta+jend)/2
!         if(i==ii.and.j==jj)print*,'Debug: CALPBLREGIME ',i,j,br,     &  
!         PBLREGIME(I,J)
   
        ENDDO
      ENDDO
!      
!     END OF ROUTINE.
!     
      RETURN
      END

