module intpsmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intpsmod    module for intps and its tangent linear intps_tl
!   prgmmr:
!
! abstract: module for intps and its tangent linear intps_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intps and its tangent linear intps_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intps_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2013-10-28  todling - rename p3d to prse
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2015-02-26       Su - add njqc as an option to choose Purser's varqc
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intps_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_psNode , only: psNode
use m_psNode , only: psNode_typecast
use m_psNode , only: psNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intps

interface intps; module procedure &
          intps_
end interface

contains

subroutine intps_(pshead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intps       apply nonlin qc obs operator for ps
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for ps observations
!           with nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intps and intps_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-06-02  safford - rm unused vars
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!   2010-05-13  todling  - update to use gsi_bundlemod; update interface
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!   2019-09-20  Su      - remove current VQC part and add VQC subroutine call
!
!   input argument list:
!     pshead  - obs type pointer to obs structure
!     sp      - ps increment in grid space
!     rp
!
!   output argument list:
!     rp      - ps results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600,two,zero
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter,njqc,vqc,nvqc
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode), pointer, intent(in   ) :: pshead
  type(gsi_bundle),        intent(in   ) :: sval
  type(gsi_bundle),        intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4,ibb,ikk
! real(r_kind) penalty
  real(r_kind) cg_t,val,grad,t_pg,var_jb,error2,rat_error2
  real(r_kind) w1,w2,w3,w4
  real(r_kind),pointer,dimension(:) :: sp
  real(r_kind),pointer,dimension(:) :: rp
  type(psNode), pointer :: psptr

!  If no ps data return
  if(.not. associated(pshead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'prse',sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
  if(ier/=0)return

  !psptr => pshead
  psptr => psNode_typecast(pshead)
  do while (associated(psptr))
     j1=psptr%ij(1)
     j2=psptr%ij(2)
     j3=psptr%ij(3)
     j4=psptr%ij(4)
     w1=psptr%wij(1)
     w2=psptr%wij(2)
     w3=psptr%wij(3)
     w4=psptr%wij(4)
     

!    Forward model
     val=w1* sp(j1)+w2* sp(j2)+w3* sp(j3)+w4* sp(j4)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*psptr%raterr2*psptr%err2
           !-- psptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(psptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (psptr%luse) psptr%diags%tldepart(jiter)=val
           if (psptr%luse) call obsdiagNode_set(psptr%diags,jiter=jiter,tldepart=val)
        endif
     endif
  
     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs)   val=val-psptr%res
!          gradient of nonlinear operator
           rat_error2=psptr%raterr2
           error2=psptr%err2
         
           if (vqc .and. nlnqc_iter .and. psptr%pg > tiny_r_kind .and.  &
                                psptr%b  > tiny_r_kind) then
              t_pg=psptr%pg*varqc_iter
              cg_t=cg_term/psptr%b                           ! b is d in Enderson
           else
              t_pg=zero
              cg_t=zero
           endif
           if (njqc .and. psptr%jb  > tiny_r_kind .and. psptr%jb <10.0_r_kind) then
              var_jb=psptr%jb
           else
              var_jb=zero 
           endif
           if(nvqc .and. psptr%ib >0) then
              ibb=psptr%ib
              ikk=psptr%ik
           else
              ibb=0
              ikk=0
           endif
           call vqc_int(error2,rat_error2,t_pg,cg_t,var_jb,ibb,ikk,val,grad) 

           if( ladtest_obs) then
              grad = val
           endif
        endif

!       Adjoint
        rp(j1)=rp(j1)+w1*grad
        rp(j2)=rp(j2)+w2*grad
        rp(j3)=rp(j3)+w3*grad
        rp(j4)=rp(j4)+w4*grad

     endif

     !psptr => psptr%llpoint
     psptr => psNode_nextcast(psptr)
  end do
  return
end subroutine intps_

end module intpsmod
