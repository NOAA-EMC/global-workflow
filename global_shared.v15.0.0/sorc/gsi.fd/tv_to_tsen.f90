subroutine tv_to_tsen(tv,q,tsen)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tv_to_tsen  tlm for normalized virtual to sensible temp
!   prgmmr: wu               org: np20                date: 2005-03-06
!
! abstract: get sensible temperature from virtual temperature
!
! program history log:
!   2006-07-17  derber
!   2008-03-31  safford - rm unused uses
!   2008-10-14  derber - modify to use fact_tv
!   2008-11-03  sato - change for twodvar_regional
!   2008-11-28  todling - no longer does tendencies (need to call twice)
!
!   input argument list:
!      tv     - virtual temperature
!      q      - specific humidity
!
!   output argument list:
!      tsen   - sensible temperature
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: lat2,lon2,nsig,twodvar_regional
  use constants, only: fv
  use guess_grids, only: ges_tsen,fact_tv,ntguessig
  use jfunc, only: tsensible

  implicit none

  real(r_kind),intent(in   ) :: tv(lat2,lon2,nsig)
  real(r_kind),intent(in   ) ::  q(lat2,lon2,nsig)

  real(r_kind),intent(  out) :: tsen(lat2,lon2,nsig)

! local arrays
  integer(i_kind) i,j,k

! Convert normalized tv to tsen
  if (twodvar_regional .and. tsensible) then
     tsen=tv
  else
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              tsen(i,j,k)=(tv(i,j,k)-fv*ges_tsen(i,j,k,ntguessig)*q(i,j,k))*fact_tv(i,j,k)

           end do
        end do
     end do
  end if

end subroutine tv_to_tsen

subroutine tv_to_tsen_ad(tv,q,tsen)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tv_to_tsen_ad  adjoint of tv_to_tsen
!   prgmmr: wu               org: np20                date: 2005-03-06
!
! abstract: adjoint of tv_to_tsen
!
! program history log:
!   2005-03-06  wu
!   2005-03-30  treadon - reformat code (cosmetic change only)
!   2005-11-21  kleist - use 3d pressure increment for coupling
!   2005-11-21  derber modify to make qoption =1 work same as =2
!   2006-01-09  derber move sigsum calculation to compute_derived and clean up
!   2008-03-31  safford - rm unused uses
!   2008-10-14  derber - modify to use fact_tv
!   2008-11-03  sato - change for twodvar_regional
!   2008-11-28  todling - no longer does tendencies (need to call twice)
!   2009-11-25  todling - zero out tsen
!
!   input argument list:
!      tv     - virtual temperature
!      q      - specific humidity
!      tsen   - sensible temperature
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
  use gridmod, only: lat2,lon2,nsig,twodvar_regional
  use constants, only: fv,zero
  use guess_grids, only: ges_tsen,fact_tv,ntguessig
  use jfunc, only: tsensible

  implicit none

  real(r_kind),intent(inout) :: tv(lat2,lon2,nsig)
  real(r_kind),intent(inout) :: q (lat2,lon2,nsig)
  real(r_kind),intent(inout) :: tsen(lat2,lon2,nsig)

! local variables:
  integer(i_kind) i,j,k

! Adjoint of convert tv to t sensible
  if (twodvar_regional .and. tsensible) then
     tv=tv+tsen
  else
     do k=1,nsig
        do j=1,lon2
           do i=1,lat2
              tv(i,j,k)=tv(i,j,k)+tsen(i,j,k)*fact_tv(i,j,k)
              q(i,j,k)=q(i,j,k)-tsen(i,j,k)*fv*ges_tsen(i,j,k,ntguessig)*fact_tv(i,j,k)
              tsen(i,j,k)=zero
           end do
        end do
     end do
  end if

end subroutine tv_to_tsen_ad
