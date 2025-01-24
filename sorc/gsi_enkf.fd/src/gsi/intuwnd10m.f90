module intuwnd10mmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intuwnd10mmod    module for intuwnd10m and its tangent linear intuwnd10m_tl
!   prgmmr:
!
! abstract: module for intuwnd10m and its tangent linear intuwnd10m_tl
!
! program history log:
!   2016-05-03 -  pondeca
!   2017-03-19 -  yang     - replaced ob_type with polymorphic obsNode through
!   type casting (follow Guo's code)
!
! subroutines included:
!   sub intuwnd10m
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode    , only: obsNode
use m_uwnd10mNode, only: uwnd10mNode
use m_uwnd10mNode, only: uwnd10mNode_typecast
use m_uwnd10mNode, only: uwnd10mNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set

implicit none

PRIVATE
PUBLIC intuwnd10m

contains

subroutine intuwnd10m(uwnd10mhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intuwnd10m      apply nonlin qc obs operator for conv. uwnd10m
!   prgmmr: pondeca        org: np23                date: 2014-03-19
!
! abstract: apply observation operator and adjoint for conventional uwnd10m
!           observations with nonlinear qc operator
!
! program history log:
!
!   2014-03-19 -  pondeca
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     uwnd10mhead
!     suwnd10m    - increment in grid space
!     ruwnd10m
!
!   output argument list:
!     ruwnd10m    - results from observation operator (0 for no data)
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
  class(obsNode), pointer,intent(in   ) :: uwnd10mhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_uwnd10m,p0,grad,wnotgross,wgross,pg_uwnd10m
  real(r_kind),pointer,dimension(:) :: suwnd10m
  real(r_kind),pointer,dimension(:) :: ruwnd10m
  type(uwnd10mNode), pointer :: uwnd10mptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'uwnd10m',suwnd10m,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'uwnd10m',ruwnd10m,istatus);ier=istatus+ier
  if(ier/=0)return

!  uwnd10mptr => uwnd10mhead
  uwnd10mptr => uwnd10mNode_typecast(uwnd10mhead)

  do while (associated(uwnd10mptr))
     j1=uwnd10mptr%ij(1)
     j2=uwnd10mptr%ij(2)
     j3=uwnd10mptr%ij(3)
     j4=uwnd10mptr%ij(4)
     w1=uwnd10mptr%wij(1)
     w2=uwnd10mptr%wij(2)
     w3=uwnd10mptr%wij(3)
     w4=uwnd10mptr%wij(4)

!    Forward model
     val=w1*suwnd10m(j1)+w2*suwnd10m(j2)&
        +w3*suwnd10m(j3)+w4*suwnd10m(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*uwnd10mptr%raterr2*uwnd10mptr%err2
           !-- uwnd10mptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(uwnd10mptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (uwnd10mptr%luse) uwnd10mptr%diags%tldepart(jiter)=val
           if (uwnd10mptr%luse) call obsdiagNode_set(uwnd10mptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-uwnd10mptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. uwnd10mptr%pg > tiny_r_kind .and. &
                                uwnd10mptr%b  > tiny_r_kind) then
              pg_uwnd10m=uwnd10mptr%pg*varqc_iter
              cg_uwnd10m=cg_term/uwnd10mptr%b
              wnotgross= one-pg_uwnd10m
              wgross = pg_uwnd10m*cg_uwnd10m/wnotgross
              p0   = wgross/(wgross+exp(-half*uwnd10mptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*uwnd10mptr%raterr2*uwnd10mptr%err2
           end if
        endif

!       Adjoint
        ruwnd10m(j1)=ruwnd10m(j1)+w1*grad
        ruwnd10m(j2)=ruwnd10m(j2)+w2*grad
        ruwnd10m(j3)=ruwnd10m(j3)+w3*grad
        ruwnd10m(j4)=ruwnd10m(j4)+w4*grad
     endif

!    uwnd10mptr => uwnd10mptr%llpoint
     uwnd10mptr => uwnd10mNode_nextcast(uwnd10mptr)


  end do

  return
end subroutine intuwnd10m

end module intuwnd10mmod
