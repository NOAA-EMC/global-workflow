subroutine normal_rh_to_q(rhnorm,t,p,q)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    normal_rh_to_q  tlm for normalized RH to q
!   prgmmr: wu               org: np20                date: 2005-03-06
!
! abstract: get specific humidity q from normalized RH
!
! program history log:
!   2005-03-06  wu
!   2005-03-30  treadon - reformat code (cosmetic change only)
!   2005-11-21  kleist - use 3d pressure increment for coupling
!   2005-11-21  derber modify to make qoption =1 work same as =2
!   2006-01-09  derber move sigsum calculation to compute_derived and clean up
!   2006-07-31  kleist - analysis variable changed from ln(ps) to ps
!   2008-05-28  safford - rm unused uses
!   2015-10-27  mahajan - code clean-up
!
!   input argument list:
!      rhnorm - normalized RH
!      t      - virtual temperature
!      p      - psfc
!
!   output argument list:
!      q      - specific humidity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use derivsmod, only: dqdrh,dqdp,dqdt
  use jfunc, only: qoption
  use gridmod, only: lat2,lon2,nsig

  implicit none

  real(r_kind),intent(in   ) :: rhnorm(lat2,lon2,nsig)
  real(r_kind),intent(in   ) :: t(lat2,lon2,nsig)
  real(r_kind),intent(in   ) :: p(lat2,lon2,nsig+1)  
  real(r_kind),intent(  out) :: q(lat2,lon2,nsig)
  
  integer(i_kind) i,j,k

! Convert normalized rh to q
   do k=1,nsig
      do j=1,lon2
         do i=1,lat2
            q(i,j,k) = dqdrh(i,j,k)*rhnorm(i,j,k)
            if ( qoption == 2 ) then
               q(i,j,k) = q(i,j,k) + &
                          dqdt(i,j,k)*t(i,j,k) - &
                          dqdp(i,j,k)*(p(i,j,k) + p(i,j,k+1))
            endif
         enddo
      enddo
   enddo

   return

end subroutine normal_rh_to_q

subroutine normal_rh_to_q_ad(rhnorm,t,p,q)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    normal_rh_to_q_ad  adjoint of normal_rh_to_q
!   prgmmr: wu               org: np20                date: 2005-03-06
!
! abstract: adjoint of normal_rh_to_q
!
! program history log:
!   2005-03-06  wu
!   2005-03-30  treadon - reformat code (cosmetic change only)
!   2005-11-21  kleist - use 3d pressure increment for coupling
!   2005-11-21  derber modify to make qoption =1 work same as =2
!   2006-01-09  derber move sigsum calculation to compute_derived and clean up
!   2006-07-31  kleist - analysis variable changed from ln(ps) to ps
!   2006-08-16  parrish - correct adjoint error, which only has impact when
!                         using strong balance constraint.
!   2008-05-28  safford - rm unused uses
!   2015-10-27  mahajan - code clean-up
!
!   input argument list:
!      rhnorm - normalized RH
!      t      - virtual temperature
!      p      - psfc
!
!   output argument list:
!      q      - specific humidity
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use derivsmod, only: dqdrh,dqdp,dqdt
  use jfunc, only: qoption
  use gridmod, only: lat2,lon2,nsig
  use constants, only: zero
  implicit none

  real(r_kind),intent(inout) :: rhnorm(lat2,lon2,nsig)
  real(r_kind),intent(inout) :: t(lat2,lon2,nsig)
  real(r_kind),intent(inout) :: p(lat2,lon2,nsig+1)
  real(r_kind),intent(inout) :: q(lat2,lon2,nsig)
  
! local variables:
  integer(i_kind) i,j,k
  
! Adjoint of convert normalized rh to q
   do k=1,nsig
      do j=1,lon2
         do i=1,lat2
            rhnorm(i,j,k) = rhnorm(i,j,k) + dqdrh(i,j,k)*q(i,j,k)
            if ( qoption == 2 ) then
               t(i,j,k  ) = t(i,j,k  ) + dqdt(i,j,k)*q(i,j,k)
               p(i,j,k  ) = p(i,j,k  ) - dqdp(i,j,k)*q(i,j,k)
               p(i,j,k+1) = p(i,j,k+1) - dqdp(i,j,k)*q(i,j,k)
            endif
            q(i,j,k) = zero
         enddo
      enddo
   enddo

   return
 
end subroutine normal_rh_to_q_ad
