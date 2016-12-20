subroutine loglcbas_to_lcbas(loglcbas,lcbas)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    loglcbas_to_lcbas  tlm for loglcbas to lcbas
!   prgmmr: zhu               org: np20                date: 2011-06-30
!
! abstract: get lcbas from logarithm of lcbas
!
! program history log:
!   2011-06-30  zhu
!
!   input argument list:
!      loglcbas
!
!   output argument list:
!      lcbas
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2
  use derivsmod, only: dlcbasdlog

  implicit none
  real(r_kind),intent(in   ) :: loglcbas(lat2,lon2)
  real(r_kind),intent(  out) :: lcbas(lat2,lon2)

  integer(i_kind) i,j

  do j=1,lon2
     do i=1,lat2
        lcbas(i,j)=dlcbasdlog(i,j)*loglcbas(i,j)
     end do
  end do
end subroutine loglcbas_to_lcbas


subroutine loglcbas_to_lcbas_ad(loglcbas,lcbas)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    loglcbas_to_lcbas_ad  adjoint of loglcbas_to_lcbas
!   prgmmr: zhu               org: np20                date: 2011-06-30
!
! abstract: adjoint of loglcbas_to_lcbas
!
! program history log:
!   2011-06-30  zhu
!
!   input argument list:
!      loglcbas
!      lcbas
!
!   output argument list:
!      loglcbas
!      lcbas
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  use gridmod, only: lat2,lon2
  use derivsmod, only: dlcbasdlog

  implicit none
  real(r_kind),intent(inout) :: loglcbas(lat2,lon2)
  real(r_kind),intent(inout) :: lcbas(lat2,lon2)

  integer(i_kind) i,j

  do j=1,lon2
     do i=1,lat2
        loglcbas(i,j)=loglcbas(i,j)+dlcbasdlog(i,j)*lcbas(i,j)
        lcbas(i,j)=zero
     end do
  end do
end subroutine loglcbas_to_lcbas_ad
