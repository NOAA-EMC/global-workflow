module intdwmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intdwmod    module for intdw and its tangent linear intdw_tl
!   prgmmr:
!
! abstract: module for intdw and its tangent linear intdw_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intdw and its tangent linear intdw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intdw_tl
!   2009-08-13  lueken - updated documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intdw_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_dwNode , only: dwNode
use m_dwNode , only: dwNode_typecast
use m_dwNode , only: dwNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intdw

interface intdw; module procedure &
          intdw_
end interface

contains

subroutine intdw_(dwhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intw        apply nonlin qc operator for lidar winds
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for lidar winds
!             with nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intdw and intdw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-06-02  safford - rm unused vars
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!   2010-05-13  todling  - update to use gsi_bundle; update interface
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
! usage: call intdw(ru,rv,su,sv)
!   input argument list:
!     dwhead   - obs type pointer to obs structure
!     su       - u increment in grid space
!     sv       - v increment in grid space
!
!   output argument list:
!     dwhead   - obs type pointer to obs structure
!     ru       - output u adjoint operator results 
!     rv       - output v adjoint operator results 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode),pointer,intent(in   ) :: dwhead
  type(gsi_bundle),        intent(in   ) :: sval
  type(gsi_bundle),        intent(inout) :: rval

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,ier,istatus
! real(r_kind) penalty
  real(r_kind) val,valu,valv,w1,w2,w3,w4,w5,w6,w7,w8,pg_dw
  real(r_kind) cg_dw,p0,grad,wnotgross,wgross
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(dwNode), pointer :: dwptr

!  If no dw observations return
  if(.not. associated(dwhead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(ier/=0)return

  !dwptr => dwhead
  dwptr => dwNode_typecast(dwhead)
  do while (associated(dwptr))
     j1=dwptr%ij(1)
     j2=dwptr%ij(2)
     j3=dwptr%ij(3)
     j4=dwptr%ij(4)
     j5=dwptr%ij(5)
     j6=dwptr%ij(6)
     j7=dwptr%ij(7)
     j8=dwptr%ij(8)
     w1=dwptr%wij(1)
     w2=dwptr%wij(2)
     w3=dwptr%wij(3)
     w4=dwptr%wij(4)
     w5=dwptr%wij(5)
     w6=dwptr%wij(6)
     w7=dwptr%wij(7)
     w8=dwptr%wij(8)
     

!    Forward model
     val=(w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)+                   &
          w5*su(j5)+w6*su(j6)+w7*su(j7)+w8*su(j8))*dwptr%sinazm+     &
         (w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)+                   &
          w5*sv(j5)+w6*sv(j6)+w7*sv(j7)+w8*sv(j8))*dwptr%cosazm

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val * dwptr%raterr2 * dwptr%err2
           !-- dwptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(dwptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (dwptr%luse) dwptr%diags%tldepart(jiter)=val
           if (dwptr%luse) call obsdiagNode_set(dwptr%diags,jiter=jiter,tldepart=val)
        endif
     endif

!    Do Adjoint
     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs)   val=val-dwptr%res
 
!          gradient of nonlinear operator
           if (nlnqc_iter .and. dwptr%pg > tiny_r_kind .and. &
                                dwptr%b  > tiny_r_kind) then
              pg_dw=varqc_iter*dwptr%pg
              cg_dw=cg_term/dwptr%b
              wnotgross= one-pg_dw
              wgross = pg_dw*cg_dw/wnotgross
              p0   = wgross/(wgross+exp(-half*dwptr%err2*val**2))
              val = val*(one-p0)
           endif

           if( ladtest_obs)  then 
              grad = val 
           else
              grad = val * dwptr%raterr2 * dwptr%err2
           end if
        endif

!       Adjoint
        valu=dwptr%sinazm * grad
        valv=dwptr%cosazm * grad
        ru(j1)=ru(j1)+w1*valu
        ru(j2)=ru(j2)+w2*valu
        ru(j3)=ru(j3)+w3*valu
        ru(j4)=ru(j4)+w4*valu
        ru(j5)=ru(j5)+w5*valu
        ru(j6)=ru(j6)+w6*valu
        ru(j7)=ru(j7)+w7*valu
        ru(j8)=ru(j8)+w8*valu
        rv(j1)=rv(j1)+w1*valv
        rv(j2)=rv(j2)+w2*valv
        rv(j3)=rv(j3)+w3*valv
        rv(j4)=rv(j4)+w4*valv
        rv(j5)=rv(j5)+w5*valv
        rv(j6)=rv(j6)+w6*valv
        rv(j7)=rv(j7)+w7*valv
        rv(j8)=rv(j8)+w8*valv
     endif

     !dwptr => dwptr%llpoint
     dwptr => dwNode_nextcast(dwptr)

  end do

  return
end subroutine intdw_

end module intdwmod
