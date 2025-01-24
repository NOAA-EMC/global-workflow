module stpwmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpwmod    module for stpw and its tangent linear stpw_tl
!  prgmmr:
!
! abstract: module for stpw and its tangent linear stpw_tl
!
! program history log:
!   2005-05-20  Yanqiu zhu - wrap stpw and its tangent linear stpw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-01  Todling - remove stpw_tl; add interface back
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2015-02-26       su - add njqc as an option to chose new non linear qc
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-05-31  Su      - remove current VQC part and add VQC subroutine call
!
! subroutines included:
!   sub stpw
!
! attributes:
!   langauge: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpw

contains

subroutine stpw(whead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpw        calculate penalty and contribution to stepsize
!                            from winds, using nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from winds,
!              using nonlinear qc.
!
! program history log:
!   1991-02-26  derber
!   1998-02-03  derber
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpw and stpw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-19  tremolet - binning of observations
!   2006-07-28  derber  - modify output for b1 and b3
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2010-05-13  todling - update to use gsi_bundle
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!
!   input argument list:
!     whead
!     ru       - search direction for u
!     rv       - search direction for v
!     su       - analysis increment for u
!     sv       - analysis increment for v
!     sges     - step size estimates  (nstep)
!     nstep    - number of step sizes ( if == 0 use outer iteration value)
!
!   output argument list  
!     out(1:nstep)   - current penalty using sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter,njqc,vqc,nvqc
  use constants, only: one,half,two,tiny_r_kind,cg_term,zero_quad,r3600,zero
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_wNode  , only: wNode
  use m_wNode  , only: wNode_typecast
  use m_wNode  , only: wNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in):: whead
  integer(i_kind)                     ,intent(in):: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout):: out
  type(gsi_bundle)                    ,intent(in):: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in):: sges

! Declare local variables
  integer(i_kind) ier,istatus,ibb,ikk
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
  real(r_kind) valu,facu,valv,facv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_t,t_pg,var_jb 
  real(r_kind) uu,vv
  real(r_kind),dimension(max(1,nstep))::pen,penu,penv
  real(r_kind),pointer,dimension(:):: ru,rv,su,sv
  type(wNode), pointer :: wptr

  out=zero_quad

!  If no w data return
  if(.not. associated(whead))return

  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(ier/=0) return

  wptr => wNode_typecast(whead)
  do while (associated(wptr))
     if(wptr%luse)then
        if(nstep > 0)then
           j1=wptr%ij(1)
           j2=wptr%ij(2)
           j3=wptr%ij(3)
           j4=wptr%ij(4)
           j5=wptr%ij(5)
           j6=wptr%ij(6)
           j7=wptr%ij(7)
           j8=wptr%ij(8)
           w1=wptr%wij(1)
           w2=wptr%wij(2)
           w3=wptr%wij(3)
           w4=wptr%wij(4)
           w5=wptr%wij(5)
           w6=wptr%wij(6)
           w7=wptr%wij(7)
           w8=wptr%wij(8)

           valu=w1* ru(j1)+w2* ru(j2)+w3* ru(j3)+w4* ru(j4) &
               +w5* ru(j5)+w6* ru(j6)+w7* ru(j7)+w8* ru(j8)  

           facu=w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4) &
               +w5* su(j5)+w6* su(j6)+w7* su(j7)+w8* su(j8) - wptr%ures
 
           valv=w1* rv(j1)+w2* rv(j2)+w3* rv(j3)+w4* rv(j4) &
               +w5* rv(j5)+w6* rv(j6)+w7* rv(j7)+w8* rv(j8)  
 
           facv=w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4) &
               +w5* sv(j5)+w6* sv(j6)+w7* sv(j7)+w8* sv(j8) - wptr%vres
     
           do kk=1,nstep
              uu=facu+sges(kk)*valu
              vv=facv+sges(kk)*valv
              penu(kk)= (uu*uu)*wptr%err2
              penv(kk)= (vv*vv)*wptr%err2
           end do
        else
           penu(1)= (wptr%ures*wptr%ures)*wptr%err2
           penv(1)= (wptr%vres*wptr%vres)*wptr%err2
        end if

!  Modify penalty term if nonlinear QC

        t_pg=zero
        cg_t=zero
        var_jb=zero
        ibb=0
        ikk=0
        if (vqc  .and. nlnqc_iter .and. wptr%pg > tiny_r_kind .and. &
                             wptr%b  > tiny_r_kind) then
           t_pg=wptr%pg*varqc_iter
           cg_t=cg_term/wptr%b
        else if(njqc  .and. wptr%jb > tiny_r_kind .and. wptr%jb <10.0_r_kind) then
!   for Dr. Jim purser' non liear quality control
           var_jb =wptr%jb
        else if(nvqc .and. wptr%ib >0) then
!  mix model VQC
           ibb=wptr%ib
           ikk=wptr%ik
        endif
        
        call vqc_stp(penu,nstep,t_pg,cg_t,var_jb,ibb,ikk)
        call vqc_stp(penv,nstep,t_pg,cg_t,var_jb,ibb,ikk)
        do kk=1,nstep
          pen(kk)=penu(kk)+penv(kk)
        end do
        out(1) = out(1)+pen(1)*wptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*wptr%raterr2
        end do

     end if

     wptr => wNode_nextcast(wptr)

  end do
  return
end subroutine stpw

end module stpwmod
