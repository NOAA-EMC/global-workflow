SUBROUTINE cloudCover_radar(mype,nlat,nlon,nsig,h_bk,grid_ref, &
                            cld_cover_3d,wthr_type)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  cloudCover_radar  cloud cover analysis using radar reflectivity
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-10
!
! ABSTRACT: 
!  This subroutine find cloud cover using radar reflectivity
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     nsig        - no. of levels
!     h_bk        - 3D background height  
!     grid_ref    - radar reflectivity in analysis grid
!
!   output argument list:
!     cld_cover_3d- 3D cloud cover
!     wthr_type   - 3D weather type
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

  use constants, only: rd_over_cp, h1000
  use constants, only: deg2rad, rad2deg, pi
  use gsd_kinds, only: r_single,i_kind,r_kind

  implicit none

  integer(i_kind),intent(in) :: mype
  integer(i_kind),intent(in) :: nlat,nlon,nsig
!
!  background
!
  real(r_single), intent(in) :: h_bk(nlon,nlat,nsig+1)   ! height
!
! Observation
!
  real(r_kind),   intent(in) :: grid_ref(nlon,nlat,nsig)
!
! Variables for cloud analysis
!
  real (r_single),intent(inout) :: cld_cover_3d(nlon,nlat,nsig)
  integer(i_kind),intent(inout) :: wthr_type(nlon,nlat)
!
  REAL(r_kind) :: ref_base         ! "significant" radar echo at upper levels
!
  REAL(r_kind) :: cloud_base
!
!-----------------------------------------------------------
!
! threshold
!

  REAL(r_kind) :: radar_cover
  PARAMETER(radar_cover=1.02)
  REAL(r_kind) :: thresh_cvr   ! lower radar echo threshold for cloud filling
  PARAMETER (thresh_cvr = 0.9)
!
! temp.
!
  INTEGER(i_kind) :: i,j,k
  REAL(r_kind)    :: zs_1d(nsig)

!
!====================================================================
!  Begin
!
   ref_base = 10.0
!
!-----------------------------------------------------------------------
!
!  Essentially, this go downward to detect radar tops in time
!  to search for a new cloud base
!
!-----------------------------------------------------------------------
!

  DO i = 2,nlon-1
    DO j = 2,nlat-1

      DO k=1,nsig
        zs_1d(k) = h_bk(i,j,k)
      END DO

      cloud_base = 200000._r_kind
!
      DO k = nsig-1,1,-1
        IF( (cld_cover_3d(i,j,k) < thresh_cvr) .and.   &
            (cld_cover_3d(i,j,k+1) >= thresh_cvr .and. &
             cld_cover_3d(i,j,k+1) < 2.0_r_kind) ) THEN
               cloud_base = 0.5_r_kind * (zs_1d(k) + zs_1d(k+1))
        END IF
      END DO ! k


      DO k = 2, nsig-1
        if(grid_ref(i,j,k) > ref_base ) then
           if( zs_1d(k) > cloud_base .and. cld_cover_3d(i,j,k) < thresh_cvr ) then
             cld_cover_3d(i,j,k)=radar_cover
           endif
        endif
      ENDDO  ! k

    ENDDO  ! i
  ENDDO    ! j
!

END SUBROUTINE cloudCover_radar

