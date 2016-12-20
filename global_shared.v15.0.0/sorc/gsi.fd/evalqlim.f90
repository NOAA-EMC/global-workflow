subroutine evalqlim(sval,pbc,rval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    evalqlim
!   prgmmr: tremolet
!
! abstract: Computes Jq component
!
! program history log:
!   2007-03-01  tremolet
!   2008-12-8   todling - updated to GSI-May08
!   2009-01-15  todling - carry summation in quadruple precision
!   2009-08-14  lueken  - update documentation
!   2010-03-23  derber - made consistent with stplimq and intlimq (but not checked)
!   2010-05-05  derber - omp commands removed
!   2010-05-13  todling - udpate to use gsi_bundle; interface change
!
!   input argument list:
!    sq
!    rq
!    pbc
!
!   output argument list:
!    rq
!    pbc
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: zero,one,zero_quad
  use gridmod, only: lat1,lon1,lat2,lon2,nsig
  use jfunc, only: factqmin,factqmax
  use derivsmod, only: qgues,qsatg
  use mpl_allreducemod, only: mpl_allreduce
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval
  real(r_quad)      ,intent(inout) :: pbc

! Declare local variables
  integer(i_kind) i,j,k,ier,istatus
  real(r_quad) :: zbc(2)
  real(r_kind) :: q,term
  real(r_kind),pointer,dimension(:,:,:) :: sq
  real(r_kind),pointer,dimension(:,:,:) :: rq
  
  pbc=zero_quad
  if (factqmin==zero .and. factqmax==zero) return
  
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'q',sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',rq,istatus);ier=istatus+ier
  if(ier/=0)return

  zbc=zero_quad
! Loop over interior of subdomain          
  do k = 1,nsig
     do j = 2,lon1+1
        do i = 2,lat1+1
!          Value for q
           q = qgues(i,j,k) + sq(i,j,k)
!          Compute penalty for neg q
           if (q<zero) then
              term = factqmin*q/(qsatg(i,j,k)*qsatg(i,j,k))
              zbc(1) = zbc(1) + term*q
!             Adjoint
              rq(i,j,k) = rq(i,j,k) + term
           endif
!          Compute penalty for excess q
           if (q>qsatg(i,j,k)) then
              term=factqmax*(q-qsatg(i,j,k))/(qsatg(i,j,k)*qsatg(i,j,k))
              zbc(2) = zbc(2) + term*(q-qsatg(i,j,k))
!             Adjoint
              rq(i,j,k) = rq(i,j,k) + term
           endif
        end do
     end do
  end do

! Reduce on all procs
  call mpl_allreduce(2,qpvals=zbc)
  pbc=pbc+zbc(1)+zbc(2)

  return
end subroutine evalqlim
