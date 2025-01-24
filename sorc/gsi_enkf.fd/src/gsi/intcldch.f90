module intcldchmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intcldchmod    module for intcldch and its tangent linear intcldch_tl
!   prgmmr:
!
! abstract: module for intcldch and its tangent linear intcldch_tl
!
! program history log:
!   2015-07-10  Manuel Pondeca
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intcldch
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode  , only: obsNode
use m_cldchNode, only: cldchNode
use m_cldchNode, only: cldchNode_typecast
use m_cldchNode, only: cldchNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intcldch

contains

subroutine intcldch(cldchhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intcldch      apply nonlin qc obs operator for conv. cldch
!   prgmmr: pondeca           org: np23                date: 2015-07-10
!
! abstract: apply observation operator and adjoint for conventional cldch
!           observations with nonlinear qc operator
!
! program history log:
!
!   2015-07-10  Manuel Pondeca
!
!   input argument list:
!     cldchhead
!     scldch    - increment in grid space
!     rcldch
!
!   output argument list:
!     rcldch    - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term
  use obsmod, only: lsaveobsens, l_do_adjoint, luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode),pointer,intent(in) :: cldchhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_cldch,p0,grad,wnotgross,wgross,pg_cldch
  real(r_kind),pointer,dimension(:) :: scldch
  real(r_kind),pointer,dimension(:) :: rcldch
  type(cldchNode), pointer :: cldchptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'cldch',scldch,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'cldch',rcldch,istatus);ier=istatus+ier
  if(ier/=0)return

  !cldchptr => cldchhead
  cldchptr => cldchNode_typecast(cldchhead)
  do while (associated(cldchptr))
     j1=cldchptr%ij(1)
     j2=cldchptr%ij(2)
     j3=cldchptr%ij(3)
     j4=cldchptr%ij(4)
     w1=cldchptr%wij(1)
     w2=cldchptr%wij(2)
     w3=cldchptr%wij(3)
     w4=cldchptr%wij(4)

!    Forward model
     val=w1*scldch(j1)+w2*scldch(j2)&
        +w3*scldch(j3)+w4*scldch(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*cldchptr%raterr2*cldchptr%err2
           !-- cldchptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(cldchptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (cldchptr%luse) cldchptr%diags%tldepart(jiter)=val
           if (cldchptr%luse) call obsdiagNode_set(cldchptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not. ladtest_obs)  val=val-cldchptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. cldchptr%pg > tiny_r_kind .and. &
                                cldchptr%b  > tiny_r_kind) then
              pg_cldch=cldchptr%pg*varqc_iter
              cg_cldch=cg_term/cldchptr%b
              wnotgross= one-pg_cldch
              wgross = pg_cldch*cg_cldch/wnotgross
              p0   = wgross/(wgross+exp(-half*cldchptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*cldchptr%raterr2*cldchptr%err2
           end if
        endif

!       Adjoint
        rcldch(j1)=rcldch(j1)+w1*grad
        rcldch(j2)=rcldch(j2)+w2*grad
        rcldch(j3)=rcldch(j3)+w3*grad
        rcldch(j4)=rcldch(j4)+w4*grad
     endif

     !cldchptr => cldchptr%llpoint
     cldchptr => cldchNode_nextcast(cldchptr)

  end do

  return
end subroutine intcldch

end module intcldchmod
