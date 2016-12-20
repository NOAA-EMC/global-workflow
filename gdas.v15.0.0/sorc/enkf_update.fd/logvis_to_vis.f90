subroutine logvis_to_vis(logvis,vis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    logvis_to_vis  tlm for logvis to vis
!   prgmmr: zhu               org: np20                date: 2011-06-30
!
! abstract: get visibility vis from logarithm of vis
!
! program history log:
!   2011-06-30  zhu
!
!   input argument list:
!      logvis
!
!   output argument list:
!      vis
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use derivsmod, only: dvisdlog

  implicit none
  real(r_kind),intent(in   ) :: logvis(lat2,lon2)
  real(r_kind),intent(  out) :: vis(lat2,lon2)

  integer(i_kind) i,j

  do j=1,lon2
     do i=1,lat2
        vis(i,j)=dvisdlog(i,j)*logvis(i,j)
     end do
  end do
end subroutine logvis_to_vis


subroutine logvis_to_vis_ad(logvis,vis)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    logvis_to_vis_ad  adjoint of logvis_to_vis
!   prgmmr: zhu               org: np20                date: 2011-06-30
!
! abstract: adjoint of logvis_to_vis
!
! program history log:
!   2011-06-30  zhu
!
!   input argument list:
!      logvis
!      vis
!
!   output argument list:
!      logvis
!      vis
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2
  use derivsmod, only: dvisdlog

  implicit none
  real(r_kind),intent(inout) :: logvis(lat2,lon2)
  real(r_kind),intent(inout) :: vis(lat2,lon2)

  integer(i_kind) i,j

  do j=1,lon2
     do i=1,lat2
        logvis(i,j)=logvis(i,j)+dvisdlog(i,j)*vis(i,j)
        vis(i,j)=zero
     end do
  end do
end subroutine logvis_to_vis_ad
