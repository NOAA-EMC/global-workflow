module intpwmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intpwmod    module for intpw and its tangent linear intpw_tl
!   prgmmr:
!
! abstract: module for intpw and its tangent linear intpw_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intpw and its tangent linear intpw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intpw_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intpw_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_pwNode, only: pwNode
use m_pwNode, only: pwNode_typecast
use m_pwNode, only: pwNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intpw

interface intpw; module procedure &
          intpw_
end interface

contains

subroutine intpw_(pwhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intpw       apply nonlin qc obs operator for p.w.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for precip. water
!           with addition of nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1993-08-15  wu
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2003-12-23  d.kleist - routine generalized to use interpolated delta(pressure)
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intpw and intpw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-03-30  wu - add vertical index k to i1,i2,i3,i4 in adjoint (bug fix)
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
!   input argument list:
!     pwhead   - obs type pointer to obs structure
!     sq       - q increment in grid space
!     rq
!
!   output argument list:
!     rq       - results from q observation operator 
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use gridmod, only: latlon11,nsig
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,tpwcon,half,one,tiny_r_kind,cg_term,r3600
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode), pointer, intent(in   ) :: pwhead
  type(gsi_bundle)        ,intent(in   ) :: sval
  type(gsi_bundle)        ,intent(inout) :: rval

! Declare local variables
  integer(i_kind) k,ier,istatus
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) val,pwcon1,w1,w2,w3,w4,time_pw
  real(r_kind) cg_pw,grad,p0,wnotgross,wgross,pg_pw
  real(r_kind),pointer,dimension(:) :: sq
  real(r_kind),pointer,dimension(:) :: rq
  type(pwNode), pointer :: pwptr

!  If no pw data return
  if(.not. associated(pwhead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'q',sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',rq,istatus);ier=istatus+ier

  if(ier/=0)return
   
  time_pw = zero

  !pwptr => pwhead
  pwptr => pwNode_typecast(pwhead)
  do while (associated(pwptr))
     w1=pwptr%wij(1)
     w2=pwptr%wij(2)
     w3=pwptr%wij(3)
     w4=pwptr%wij(4)
     i1(1)=pwptr%ij(1)
     i2(1)=pwptr%ij(2)
     i3(1)=pwptr%ij(3)
     i4(1)=pwptr%ij(4)
     do k=2,nsig
        i1(k)=i1(k-1)+latlon11
        i2(k)=i2(k-1)+latlon11
        i3(k)=i3(k-1)+latlon11
        i4(k)=i4(k-1)+latlon11
     end do
     
     val=zero
!    Forward model
     do k=1,nsig
        val=val+(w1* sq(i1(k))+w2* sq(i2(k))            &
               + w3* sq(i3(k))+w4* sq(i4(k)))*          &
                 tpwcon*pwptr%dp(k)
     end do

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*pwptr%raterr2*pwptr%err2
           !-- pwptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(pwptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (pwptr%luse) pwptr%diags%tldepart(jiter)=val
           if (pwptr%luse) call obsdiagNode_set(pwptr%diags,jiter=jiter,tldepart=val)
        endif
     end if

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
!          Difference from observation
           if( .not. ladtest_obs)   val=val-pwptr%res
!          needed for gradient of nonlinear qc operator
           if (nlnqc_iter .and. pwptr%pg > tiny_r_kind .and.  &
                                pwptr%b  > tiny_r_kind) then
              pg_pw=pwptr%pg*varqc_iter
              cg_pw=cg_term/pwptr%b
              wnotgross= one-pg_pw
              wgross = pg_pw*cg_pw/wnotgross
              p0   = wgross/(wgross+exp(-half*pwptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*pwptr%raterr2*pwptr%err2
           end if
        endif

!       Adjoint
        do k=1,nsig
           pwcon1=tpwcon*pwptr%dp(k)*grad
           rq(i1(k))   =   rq(i1(k))+w1*pwcon1
           rq(i2(k))   =   rq(i2(k))+w2*pwcon1
           rq(i3(k))   =   rq(i3(k))+w3*pwcon1
           rq(i4(k))   =   rq(i4(k))+w4*pwcon1
        end do
     endif

     !pwptr => pwptr%llpoint
     pwptr => pwNode_nextcast(pwptr)

  end do

  return
end subroutine intpw_

end module intpwmod
