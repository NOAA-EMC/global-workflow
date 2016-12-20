subroutine logcldch_to_cldch(logcldch,cldch)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    logcldch_to_cldch  tlm for logcldch to cldch
!   prgmmr: pondeca               org: np20            date: 2015-07-10
!
! abstract: get cloud ceiling height cldch  from logarithm of cldch
!
! program history log:
!   2015-07-10  pondeca
!
!   input argument list:
!      logcldch
!
!   output argument list:
!      cldch
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use derivsmod, only: dcldchdlog

  implicit none
  real(r_kind),intent(in   ) :: logcldch(lat2,lon2)
  real(r_kind),intent(  out) :: cldch(lat2,lon2)

  integer(i_kind) i,j

  do j=1,lon2
     do i=1,lat2
        cldch(i,j)=dcldchdlog(i,j)*logcldch(i,j)
     end do
  end do
end subroutine logcldch_to_cldch


subroutine logcldch_to_cldch_ad(logcldch,cldch)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    logcldch_to_cldch_ad  adjoint of logcldch_to_cldch
!   prgmmr: zhu               org: np20                date: 2011-06-30
!
! abstract: adjoint of logcldch_to_cldch
!
! program history log:
!   2015-07-10  pondeca
!
!   input argument list:
!      logcldch
!      cldch
!
!   output argument list:
!      logcldch
!      cldch
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2
  use derivsmod, only: dcldchdlog

  implicit none
  real(r_kind),intent(inout) :: logcldch(lat2,lon2)
  real(r_kind),intent(inout) :: cldch(lat2,lon2)

  integer(i_kind) i,j

  do j=1,lon2
     do i=1,lat2
        logcldch(i,j)=logcldch(i,j)+dcldchdlog(i,j)*cldch(i,j)
        cldch(i,j)=zero
     end do
  end do
end subroutine logcldch_to_cldch_ad
