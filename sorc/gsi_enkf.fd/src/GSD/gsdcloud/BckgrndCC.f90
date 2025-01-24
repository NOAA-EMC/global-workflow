SUBROUTINE  BckgrndCC(nlon,nlat,nsig,tbk,pbk,q,hbk,zh,   &
                      cv_bk,t_k,z_lcl)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  BckgrndCC  generate background field for
!           fractional cloud cover based on RH
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-27
!
! ABSTRACT: 
!  This subroutine calculate cloud field based on background fields
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     tbk         - 3D background potentional temperature (K)
!     pbk         - 3D background pressure  (hPa)
!     q           - 3D moisture  (kg/kg)
!     hbk         - 3D height
!     zh          - terrain
!
!   output argument list:
!     cv_bk       - 3D background cloud cover
!     t_k         - 3D temperature in K
!     z_lcl       - lifting condensation level
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
  use gsd_kinds, only: r_single,i_kind,r_kind
  use constants, only: h1000, rd_over_cp, g_over_rd

  implicit none

  integer(i_kind),intent(in):: nlon,nlat,nsig
! background
!
! read in from WRF
!
  real(r_single),intent(in) :: tbk(nlon,nlat,nsig)   ! potential temperature
  real(r_single),intent(in) :: zh(nlon,nlat)         ! terrain elevation
  real(r_single),intent(in) :: q(nlon,nlat,nsig)     ! moisture
  real(r_single),intent(in) :: hbk(nlon,nlat,nsig)   ! height
  real(r_single),intent(in) :: pbk(nlon,nlat,nsig)   ! pressure

  real(r_single),intent(out) :: t_k(nlon,nlat,nsig)  ! temperature in K
  real(r_single),intent(out) :: z_lcl(nlon,nlat)     ! lifting condensation level
  real(r_single),intent(out) :: cv_bk(nlon,nlat,nsig)!  cloud cover

!  CONSTANTS:
  real(r_single) :: gamma_d   ! dry adiabatic lapse rate (K/m)
  real(r_single) :: z_ref_lcl
  PARAMETER(z_ref_lcl = 180.0_r_single)

!  misc.
!
  real(r_single) :: rhbk(nlon,nlat,nsig)   ! rh

  INTEGER :: i,j,k


  REAL(r_kind) :: f_qvsat
  REAL(r_kind) :: qvsat
  REAL(r_kind) :: rh_to_cldcv

  REAL(r_kind) :: z_ref,x
  REAL(r_kind) :: arg,arg2, t_ref_c, td_ref_c
  REAL(r_kind) :: frac_z, t_ref_k,rh_ref

!
!================================================================
!
  gamma_d = g_over_rd/rd_over_cp
!
! get the RH
!
  do k=1,nsig
    do j=2,nlat-1
      do i=2,nlon-1
        t_k(i,j,k)=tbk(i,j,k)*(pbk(i,j,k)/h1000)**rd_over_cp
        qvsat=f_qvsat(pbk(i,j,k)*100.0_r_kind,t_k(i,j,k))   
                    ! Saturation water vapor specific humidity 
        qvsat = qvsat/(1.0 - qvsat)  ! convert to saturation mixing ratio (kg/kg)
        rhbk(i,j,k)=100._r_kind*MIN(1._r_kind,MAX(0._r_kind,(q(i,j,k)/qvsat)))
                                     ! q is mixing ration kg/kg
      enddo
    enddo
  enddo
!
!  Find the lifting condensation level
!
  z_lcl = -99999.0_r_kind
  do j=2,nlat-1
    do i=2,nlon-1
      z_ref = z_ref_lcl + zh(i,j)
      IF (z_ref <= hbk(i,j,2) .OR. z_ref > hbk(i,j,nsig-1)) THEN
        write(6,*) 'Error, ref.level is out of bounds at pt:' &
                    ,i,j,z_ref,hbk(i,j,2),hbk(i,j,nsig-1)
        call STOP2(114)
      END IF

      DO k = 3,nsig-1
        IF ( z_ref < hbk(i,j,k) .and. z_ref >= hbk(i,j,k-1)) THEN
          frac_z = (z_ref-hbk(i,j,k-1))/(hbk(i,j,k)-hbk(i,j,k-1))
          t_ref_k = t_k(i,j,k-1)+ frac_z*(t_k(i,j,k)-t_k(i,j,k-1))
          t_ref_c = t_ref_k - 273.15_r_kind
!
          rh_ref = rhbk(i,j,k-1)+ frac_z*(rhbk(i,j,k)-rhbk(i,j,k-1))
!   compute dew point depression.
!          td_ref_c = dwpt(t_ref_c,rh_ref)
          x = 1._r_kind-0.01_r_kind*rh_ref
          td_ref_c =t_ref_c-(14.55_r_kind+0.114_r_kind*t_ref_c)*x+      &
                ((2.5_r_kind+0.007_r_kind*t_ref_c)*x)**3+      &
                (15.9_r_kind+0.117_r_kind*t_ref_c)*x**14

        END IF
      END DO  ! k = 2,nz-1
!
      z_lcl(i,j) = z_ref + (t_ref_c - td_ref_c)/gamma_d
      z_lcl(i,j) = min(hbk(i,j,nsig-1),max(z_lcl(i,j),hbk(i,j,2)))
    enddo
  enddo
!
!  get background cloud cover
!
  cv_bk=0.0_r_kind
  do k=1,nsig
    do j=2,nlat-1
      do i=2,nlon-1
        IF (hbk(i,j,k) >= z_lcl(i,j)) THEN
           arg = hbk(i,j,k) - zh(i,j)
           arg2=rhbk(i,j,k)*0.01_r_kind
           cv_bk(i,j,k) = rh_to_cldcv(arg2,arg)
        ENDIF
      enddo
    enddo
  enddo
!

END SUBROUTINE BckgrndCC
