SUBROUTINE calc_pbl_height(mype,nlat,nlon,nsig,q_bk,t_bk,p_bk,pblh)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  pbl_height  to calculate PBL height or level
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2011-04-06
!
! ABSTRACT: 
!  This subroutine calculate PBL height
!
! PROGRAM HISTORY LOG:
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     q_bk        - 3D moisture
!     t_bk        - 3D background potential temperature (K)
!     p_bk        - 3D background pressure  (hPa)
!
!   output argument list:
!     pblh        - 2D PBL height (level number) 
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use kinds, only: r_single,i_kind, r_kind

  implicit none

  integer(i_kind),intent(in):: mype
  integer(i_kind),intent(in):: nlat,nlon,nsig
!
!  background
!
  real(r_single),intent(in)    :: t_bk(nlon,nlat,nsig)   ! potential temperature (K)
  real(r_single),intent(in)    :: q_bk(nlon,nlat,nsig)   ! mixing ratio (kg/kg)
  real(r_single),intent(in)    :: p_bk(nlon,nlat,nsig)   ! pressure  (hpa)
!
!  Variables for cloud analysis
!
  real (r_single),intent(out) :: pblh(nlon,nlat) 
!
!-----------------------------------------------------------
!
! temp.
!
  INTEGER(i_kind) :: i,j,k
  real(r_single) :: thetav(nsig)
  real(r_single) :: thsfc,qsp

!====================================================================
!  Begin
!
!
  DO j = 1,nlat
    DO i = 1,nlon
   
      DO k = 1,nsig
        qsp=q_bk(i,j,k)/(1.0+q_bk(i,j,k))          ! q_bk = water vapor mixing ratio
        thetav(k) = t_bk(i,j,k)*(1.0 + 0.61 * qsp) ! qsp  = spcific humidity
! if(mype==10.and.i==10.and.j==10) then
!     write(*,*) 'cal PBL=',k,thetav(k),t_bk(i,j,k),q_bk(i,j,k)
! endif
      ENDDO
      
      pblh(i,j) = 0.0_r_single
      thsfc = thetav(1)
      k=1
      DO while (abs(pblh(i,j)) < 0.0001_r_single)
        if( thetav(k) > thsfc + 1.0_r_single ) then
          pblh(i,j) = float(k) - (thetav(k) - (thsfc + 1.0_r_single))/   &
                             max((thetav(k)-thetav(k-1)),0.01_r_single)
        endif
        k=k+1
      ENDDO
      if(abs(pblh(i,j)) < 0.0001) pblh(i,j)=2.0_r_single

! if(mype==10.and.i==10.and.j==10) then
!     write(*,*) 'cal PBL=',pblh(i,j),k
! endif


    enddo   ! i
  enddo     ! j

END SUBROUTINE calc_pbl_height

