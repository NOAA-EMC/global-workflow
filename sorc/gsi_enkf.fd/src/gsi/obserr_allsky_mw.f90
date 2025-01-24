subroutine obserr_allsky_mw(error0,tnoise,tnoise_cld,clwp_obs,clwp_guess)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   obserr_allsky_mw 
!   prgmmr: mkim          org: np23                date: 2014-01-31
!
! abstract: define observation error for all-sky microwave radiance data 
!
! program history log:
!   2014-01-31  mkim
!
!   input argument list:
!     error0    - input observation error before applying changes for cloudy sky condition
!
!   output argument list:
!     error0      - observation error 
!
! remarks: see modules used
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half
  implicit none

  real(r_kind),intent(inout) :: error0 
  real(r_kind),intent(in   ) :: tnoise, tnoise_cld, clwp_obs,clwp_guess

! Declare local variables 
  real(r_kind) :: clwp_avg

  clwp_avg=half*(clwp_obs+clwp_guess)
  if(clwp_avg < 0.05_r_kind) then 
      error0 = tnoise
  else
      error0 = tnoise_cld
  endif

end subroutine obserr_allsky_mw 

