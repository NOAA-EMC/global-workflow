module intlcbasmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intlcbasmod    module for intlcbas 
!   prgmmr:
!
! abstract: module for intlcbas 
!
! program history log:
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intlcbas
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode  , only: obsNode
use m_lcbasNode, only: lcbasNode
use m_lcbasNode, only: lcbasNode_typecast
use m_lcbasNode, only: lcbasNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intlcbas

contains

subroutine intlcbas(lcbashead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlcbas      apply nonlin qc obs operator for conv. lcbas
!   prgmmr: zhu           org: np23                date: 2012-01-20
!
! abstract: apply observation operator and adjoint for conventional lcbas
!           observations with nonlinear qc operator
!
! program history log:
!
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     lcbashead
!     slcbas    - increment in grid space
!     rlcbas
!
!   output argument list:
!     rlcbas    - results from observation operator (0 for no data)
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
  implicit none

! Declare passed variables
  class(obsNode),pointer,intent(in) :: lcbashead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_lcbas,p0,grad,wnotgross,wgross,pg_lcbas
  real(r_kind),pointer,dimension(:) :: slcbas
  real(r_kind),pointer,dimension(:) :: rlcbas
  type(lcbasNode), pointer :: lcbasptr

! If no lcbas data return
  if(.not. associated(lcbashead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'lcbas',slcbas,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'lcbas',rlcbas,istatus);ier=istatus+ier
  if(ier/=0)return

  !lcbasptr => lcbashead
  lcbasptr => lcbasNode_typecast(lcbashead)
  do while (associated(lcbasptr))
     j1=lcbasptr%ij(1)
     j2=lcbasptr%ij(2)
     j3=lcbasptr%ij(3)
     j4=lcbasptr%ij(4)
     w1=lcbasptr%wij(1)
     w2=lcbasptr%wij(2)
     w3=lcbasptr%wij(3)
     w4=lcbasptr%wij(4)

!    Forward model
     val=w1*slcbas(j1)+w2*slcbas(j2)&
        +w3*slcbas(j3)+w4*slcbas(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*lcbasptr%raterr2*lcbasptr%err2
           !-- lcbasptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(lcbasptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (lcbasptr%luse) lcbasptr%diags%tldepart(jiter)=val
           if (lcbasptr%luse) call obsdiagNode_set(lcbasptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           val=val-lcbasptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. lcbasptr%pg > tiny_r_kind .and. &
                                lcbasptr%b  > tiny_r_kind) then
              pg_lcbas=lcbasptr%pg*varqc_iter
              cg_lcbas=cg_term/lcbasptr%b
              wnotgross= one-pg_lcbas
              wgross = pg_lcbas*cg_lcbas/wnotgross
              p0   = wgross/(wgross+exp(-half*lcbasptr%err2*val**2))
              val = val*(one-p0)
           endif

           grad = val*lcbasptr%raterr2*lcbasptr%err2
        endif

!       Adjoint
        rlcbas(j1)=rlcbas(j1)+w1*grad
        rlcbas(j2)=rlcbas(j2)+w2*grad
        rlcbas(j3)=rlcbas(j3)+w3*grad
        rlcbas(j4)=rlcbas(j4)+w4*grad
     endif

     !lcbasptr => lcbasptr%llpoint
     lcbasptr => lcbasNode_nextcast(lcbasptr)

  end do

  return
end subroutine intlcbas

end module intlcbasmod
