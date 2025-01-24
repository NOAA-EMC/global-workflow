module intvwnd10mmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intvwnd10mmod    module for intvwnd10m and its tangent linear intvwnd10m_tl
!   prgmmr:
!
! abstract: module for intvwnd10m and its tangent linear intvwnd10m_tl
!
! program history log:
!   2016-05-03 -  pondeca
!   2017-03-19 -  yang     - replaced ob_type with polymorphic obsNode through
!   type casting (follow Guo's code)
!
! subroutines included:
!   sub intvwnd10m
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use m_obsNode    , only: obsNode
use m_vwnd10mNode, only: vwnd10mNode
use m_vwnd10mNode, only: vwnd10mNode_typecast
use m_vwnd10mNode, only: vwnd10mNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set

implicit none

PRIVATE
PUBLIC intvwnd10m

contains

subroutine intvwnd10m(vwnd10mhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intvwnd10m      apply nonlin qc obs operator for conv. vwnd10m
!   prgmmr: pondeca        org: np23                date: 2014-03-19
!
! abstract: apply observation operator and adjoint for conventional vwnd10m
!           observations with nonlinear qc operator
!
! program history log:
!
!   2014-03-19 -  pondeca
!   2015-03-11 -  pondeca  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     vwnd10mhead
!     svwnd10m    - increment in grid space
!     rvwnd10m
!
!   output argument list:
!     rvwnd10m    - results from observation operator (0 for no data)
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
  class(obsNode), pointer,intent(in   ) :: vwnd10mhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
! real(r_kind) penalty
  real(r_kind) w1,w2,w3,w4
  real(r_kind) val
  real(r_kind) cg_vwnd10m,p0,grad,wnotgross,wgross,pg_vwnd10m
  real(r_kind),pointer,dimension(:) :: svwnd10m
  real(r_kind),pointer,dimension(:) :: rvwnd10m
  type(vwnd10mNode), pointer :: vwnd10mptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'vwnd10m',svwnd10m,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'vwnd10m',rvwnd10m,istatus);ier=istatus+ier
  if(ier/=0)return

!  vwnd10mptr => vwnd10mhead
  vwnd10mptr => vwnd10mNode_typecast(vwnd10mhead)

  do while (associated(vwnd10mptr))
     j1=vwnd10mptr%ij(1)
     j2=vwnd10mptr%ij(2)
     j3=vwnd10mptr%ij(3)
     j4=vwnd10mptr%ij(4)
     w1=vwnd10mptr%wij(1)
     w2=vwnd10mptr%wij(2)
     w3=vwnd10mptr%wij(3)
     w4=vwnd10mptr%wij(4)

!    Forward model
     val=w1*svwnd10m(j1)+w2*svwnd10m(j2)&
        +w3*svwnd10m(j3)+w4*svwnd10m(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*vwnd10mptr%raterr2*vwnd10mptr%err2
           !-- vwnd10mptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(vwnd10mptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (vwnd10mptr%luse) vwnd10mptr%diags%tldepart(jiter)=val
           if (vwnd10mptr%luse) call obsdiagNode_set(vwnd10mptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if(.not.ladtest_obs)  val=val-vwnd10mptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. vwnd10mptr%pg > tiny_r_kind .and. &
                                vwnd10mptr%b  > tiny_r_kind) then
              pg_vwnd10m=vwnd10mptr%pg*varqc_iter
              cg_vwnd10m=cg_term/vwnd10mptr%b
              wnotgross= one-pg_vwnd10m
              wgross = pg_vwnd10m*cg_vwnd10m/wnotgross
              p0   = wgross/(wgross+exp(-half*vwnd10mptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*vwnd10mptr%raterr2*vwnd10mptr%err2
           end if
        endif

!       Adjoint
        rvwnd10m(j1)=rvwnd10m(j1)+w1*grad
        rvwnd10m(j2)=rvwnd10m(j2)+w2*grad
        rvwnd10m(j3)=rvwnd10m(j3)+w3*grad
        rvwnd10m(j4)=rvwnd10m(j4)+w4*grad
     endif

!    vwnd10mptr => vwnd10mptr%llpoint
     vwnd10mptr => vwnd10mNode_nextcast(vwnd10mptr) 

  end do

  return
end subroutine intvwnd10m

end module intvwnd10mmod
