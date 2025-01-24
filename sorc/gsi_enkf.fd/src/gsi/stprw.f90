module stprwmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stprwmod    module for stprw and its tangent linear stprw_tl
!  prgmmr:
!
! abstract: module for stprw and its tangent linear stprw_tl
!
! program history log:
!   2005-05-19  Yanqiu zhu - wrap stprw and its tangent linear stprw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stprw_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stprw
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stprw

contains

subroutine stprw(rwhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stprw       calculate penalty and contribution to
!                            stepsize with nonlinear qc added.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from radar winds
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stprw and stprw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2007-03-19  tremolet - binning of observations
!   2007-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic - add foto
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - update to use gsi_bundle
!   2017-05-12  Y. Wang and X. Wang - include w into adjoint of rw operator,
!                                     POC: xuguang.wang@ou.edu
!   2016-06-23  lippi   - add terms for vertical velocity, uses include_w, and
!                         now multiplying by costilt here instead of being
!                         factored into the wij term.
!
!   input argument list:
!     rwhead
!     ru       - search direction for u
!     rv       - search direction for v
!     rw       - search direction for w
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sw       - analysis increment for w
!     sges     - step size estimates (nstep)
!     nstep    - number of step sizes (== 0 means use outer iteration value)
!
!   output argument list     - output for step size calculation
!     out(1:nstep)   - penalty from radar winds sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: if_use_w_vr
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,r3600
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_rwNode , only: rwNode
  use m_rwNode , only: rwNode_typecast
  use m_rwNode , only: rwNode_nextcast

  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: rwhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  logical include_w
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
  real(r_kind) valrw,facrw,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_rw,r,wgross,wnotgross
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind) pg_rw
  real(r_kind),pointer,dimension(:) :: su,sv,sw
  real(r_kind),pointer,dimension(:) :: ru,rv,rw
  type(rwNode), pointer :: rwptr

  out=zero_quad

!  If no rw data return
  if(.not. associated(rwhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  include_w=.false.
  call gsi_bundlegetpointer(sval,'w',sw,istatus)
  if (if_use_w_vr.and.istatus==0) then
     call gsi_bundlegetpointer(rval,'w',rw,istatus)
     if(istatus == 0)include_w=.true.
  end if

  if(ier/=0)return

  rwptr => rwNode_typecast(rwhead)
  do while (associated(rwptr))
     if(rwptr%luse)then
        if(nstep > 0)then
           j1=rwptr%ij(1)
           j2=rwptr%ij(2)
           j3=rwptr%ij(3)
           j4=rwptr%ij(4)
           j5=rwptr%ij(5)
           j6=rwptr%ij(6)
           j7=rwptr%ij(7)
           j8=rwptr%ij(8)
           w1=rwptr%wij(1)
           w2=rwptr%wij(2)
           w3=rwptr%wij(3)
           w4=rwptr%wij(4)
           w5=rwptr%wij(5)
           w6=rwptr%wij(6)
           w7=rwptr%wij(7)
           w8=rwptr%wij(8)

!          Gradient
           valrw=(w1*ru(j1)+ w2*ru(j2)+ w3*ru(j3)+ &
                  w4*ru(j4)+ w5*ru(j5)+ w6*ru(j6)+ &
                  w7*ru(j7)+ w8*ru(j8))*rwptr%cosazm_costilt+ &
                 (w1*rv(j1)+ w2*rv(j2)+ w3*rv(j3)+ &
                  w4*rv(j4)+ w5*rv(j5)+ w6*rv(j6)+ &
                  w7*rv(j7)+ w8*rv(j8))*rwptr%sinazm_costilt
           if(include_w) then
              valrw=valrw+(w1*rw(j1)+ w2*rw(j2)+ w3*rw(j3)+ &
                           w4*rw(j4)+ w5*rw(j5)+ w6*rw(j6)+ &
                           w7*rw(j7)+w8*rw(j8))*rwptr%sintilt
           end if

!          Gradient - residual
            facrw=(w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+w5* su(j5)+  &
                   w6* su(j6)+w7* su(j7)+w8* su(j8))*rwptr%cosazm_costilt+  &
                  (w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+w5* sv(j5)+  &
                   w6* sv(j6)+w7* sv(j7)+w8* sv(j8))*rwptr%sinazm_costilt
           if(include_w) then
              facrw=facrw+(w1*sw(j1)+ w2*sw(j2)+ w3*sw(j3)+ w4*sw(j4)+w5*sw(j5)+&
                           w6*sw(j6)+ w7*sw(j7)+ w8*sw(j8))*rwptr%sintilt
           end if
           facrw=facrw-rwptr%res

           do kk=1,nstep
              r=facrw+sges(kk)*valrw
              pen(kk)=r*r*rwptr%err2
           end do
        else
           pen(1)=rwptr%res*rwptr%res*rwptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. rwptr%pg > tiny_r_kind .and.  &
                             rwptr%b  > tiny_r_kind) then
           pg_rw=rwptr%pg*varqc_iter
           cg_rw=cg_term/rwptr%b
           wnotgross= one-pg_rw
           wgross = pg_rw*cg_rw/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*rwptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*rwptr%raterr2
        end do
     end if

     rwptr => rwNode_nextcast(rwptr)

  end do
  return
end subroutine stprw

end module stprwmod
