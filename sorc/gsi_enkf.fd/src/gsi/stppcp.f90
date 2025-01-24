module stppcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stppcpmod    module for stppcp and its tangent linear stppcp_tl
!  prgmmr:
!
! abstract: module for stppcp and its tangent linear stppcp_tl
!
! program history log:
!   2005-05-17  Yanqiu zhu - wrap stppcp and its tangent linear stppcp_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stppcp_tl
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stppcp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documnetation block

implicit none

PRIVATE 
PUBLIC  stppcp

contains

subroutine stppcp(pcphead,dval,xval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppcp     compute contribution to penalty and
!                           stepsize from pcp, with nonlinear qc
!   prgmmr: treadon          org:  np23               date: 2003-09-13
!
! abstract: compute contribution to penalty and stepsize from precipitation
!           observations
!
! program history log:
!   2003-12-18 treadon - initial routine
!   2004-06-15 treadon - update documentation
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07 parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stppcp and stppcp_qc into single routine
!   2005-09-28  derber  - modify var qc and change location and weight arrays
!   2006-07-28  derber   - modify to use new inner loop obs data structure
!                        - unify NL qc
!   2006-09-18  derber   - modify output b1 and b3 
!   2007-01-19  derber   - limit pcp_ges* > zero
!   2007-03-19  tremolet - binning of observations
!   2007-05-10  tremolet - add opt to run as linear procedure
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2008-06-02  safford - rm unused var and uses
!   2008-12-03  todling - changed handling of ptr%time
!   2009-01-26  todling - re-implement Tremolet's linearization for q1fy10
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-03-25  zhu     - use state_vector in the interface;
!                       - add handlings of cw case; add pointer_state
!   2010-05-13 todling  - update to use gsi_bundle
!                       - on-the-spot handling of non-essential vars
!   2010-09-25 todling  - fix linearization
!   2011-11-01 eliu     - add handling for ql and qi increments 
!
!   input argument list:
!     pcphead
!     rt       - search direction for temperature
!     rq       - search direction for moisture 
!     ru       - search direction for zonal wind
!     rv       - search direction for meridional wind
!     rql      - search direction for cloud liquid water mixing ratio
!     rqi      - search direction for cloud ice water mixing ratio
!     st       - input temperature correction field
!     sq       - input  q correction field
!     su       - input  u correction field
!     sv       - input  v correction field
!     sql      - input ql correction field
!     sqi      - input qi correction field
!     sges     - step size estimates (nstep)
!     nstep    - number of stepsizes  (==0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty from precipitation rate - sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use pcpinfo, only: b_pcp,pg_pcp,tinym1_obs
  use constants, only: zero,one,half,two,tiny_r_kind,cg_term,zero_quad,r3600
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon11,nsig
  use gsi_4dvar, only: ltlint
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_pcpNode, only: pcpNode
  use m_pcpNode, only: pcpNode_typecast
  use m_pcpNode, only: pcpNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: pcphead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle),intent(in) :: dval
  type(gsi_bundle),intent(in) :: xval

! Declare local variables
  logical:: lcld 
  integer(i_kind) n,ncwm,nq,nt,nu,nv,kx,ier,istatus,icw,iql,iqi
  integer(i_kind) j1,j2,j3,j4,kk
  real(r_kind) dt,dt0,w1,w2,w3,w4
  real(r_kind) dq,dq0
  real(r_kind) du,du0
  real(r_kind) dv,dv0
  real(r_kind) dcwm,dcwm0
  real(r_kind) pcp_gest,pcp_ges0,pcp_ges,obsges,termges,termgtl,obsgtl
  real(r_kind),dimension(max(1,nstep)):: pen
  real(r_kind) cg_pcp,wgross,wnotgross,pentl,pencur
  type(pcpNode), pointer :: pcpptr
  real(r_kind),pointer,dimension(:):: rt,st,rq,sq,ru,su,rv,sv,rcwm,scwm
  real(r_kind),pointer,dimension(:):: rql,rqi,sql,sqi

! Initialize penalty, b1, and b3 to zero  
  out=zero_quad

! If no  pcp data return
  if(.not. associated(pcphead))return

! Retrieve pointers
  ier=0; icw=0; iql=0; iqi=0
  call gsi_bundlegetpointer(xval,'u',    su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'v',    sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'tsen' ,st,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'q',    sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(xval,'cw', scwm,istatus);icw=istatus+icw
  call gsi_bundlegetpointer(xval,'ql',  sql,istatus);iql=istatus+iql
  call gsi_bundlegetpointer(xval,'qi',  sqi,istatus);iqi=istatus+iqi
  if(ier/=0)return

  call gsi_bundlegetpointer(dval,'u',    ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'v',    rv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'tsen' ,rt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'q',    rq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(dval,'cw', rcwm,istatus);icw=istatus+icw
  call gsi_bundlegetpointer(dval,'ql',  rql,istatus);iql=istatus+iql
  call gsi_bundlegetpointer(dval,'qi',  rqi,istatus);iqi=istatus+iqi
  if(ier/=0)return

  lcld = (icw==0 .or. (iql+iqi)==0)

! Loop over number of observations.
  pcpptr => pcpNode_typecast(pcphead)
  do while(associated(pcpptr))
     if(pcpptr%luse)then
        pcp_ges0 = pcpptr%ges
        if(nstep > 0)then
           j1=pcpptr%ij(1)
           j2=pcpptr%ij(2)
           j3=pcpptr%ij(3)
           j4=pcpptr%ij(4)
           w1=pcpptr%wij(1)
           w2=pcpptr%wij(2)
           w3=pcpptr%wij(3)
           w4=pcpptr%wij(4)
           pcp_gest = zero


!          Compute updates to simulated precipitation
           do n=1,nsig
              dt  =w1*   st(j1)+w2*   st(j2)+ w3*   st(j3)+w4*   st(j4)
              dq  =w1*   sq(j1)+w2*   sq(j2)+ w3*   sq(j3)+w4*   sq(j4)
              du  =w1*   su(j1)+w2*   su(j2)+ w3*   su(j3)+w4*   su(j4)
              dv  =w1*   sv(j1)+w2*   sv(j2)+ w3*   sv(j3)+w4*   sv(j4)
              if (lcld) then
                 if (icw==0) then
                    dcwm=w1* scwm(j1)+w2* scwm(j2)+ w3* scwm(j3)+w4* scwm(j4)
                 else
                    dcwm=w1* (sql(j1)+sqi(j1))+ &
                         w2* (sql(j2)+sqi(j2))+ &
                         w3* (sql(j3)+sqi(j3))+ &
                         w4* (sql(j4)+sqi(j4))
                 end if
              else
                 dcwm=zero
              endif

              dt0  =w1*   rt(j1)+w2*   rt(j2)+ w3*   rt(j3)+w4*   rt(j4)
              dq0  =w1*   rq(j1)+w2*   rq(j2)+ w3*   rq(j3)+w4*   rq(j4)
              du0  =w1*   ru(j1)+w2*   ru(j2)+ w3*   ru(j3)+w4*   ru(j4)
              dv0  =w1*   rv(j1)+w2*   rv(j2)+ w3*   rv(j3)+w4*   rv(j4)

              if (lcld) then
                 if (icw==0) then
                    dcwm0=w1* rcwm(j1)+w2* rcwm(j2)+ w3* rcwm(j3)+w4* rcwm(j4)
                 else
                    dcwm0=w1* (rql(j1)+rqi(j1))+ &
                          w2* (rql(j2)+rqi(j2))+ &
                          w3* (rql(j3)+rqi(j3))+ &
                          w4* (rql(j4)+rqi(j4))
                 end if
	      else
                 dcwm0=zero
              endif

              nt=n; nq=nt+nsig; nu=nq+nsig; nv=nu+nsig; ncwm=nv+nsig
              pcp_ges0 = pcp_ges0 +  pcpptr%dpcp_dvar(nt)  *dt + &
                                     pcpptr%dpcp_dvar(nq)  *dq + &
                                     pcpptr%dpcp_dvar(nu)  *du + &
                                     pcpptr%dpcp_dvar(nv)  *dv + &
                                     pcpptr%dpcp_dvar(ncwm)*dcwm
              pcp_gest = pcp_gest +  pcpptr%dpcp_dvar(nt)  *dt0+ &
                                     pcpptr%dpcp_dvar(nq)  *dq0+ &
                                     pcpptr%dpcp_dvar(nu)  *du0+ &
                                     pcpptr%dpcp_dvar(nv)  *dv0+ &
                                     pcpptr%dpcp_dvar(ncwm)*dcwm0

              j1=j1+latlon11
              j2=j2+latlon11
              j3=j3+latlon11
              j4=j4+latlon11

           end do

           if (ltlint) then
              pcp_ges = pcp_ges0
!             Logrithmic formulation.  Ensure pcp_ges > zero
              pcp_ges = max(pcp_ges,zero)
              termges = log(one+pcp_ges)
              obsges= pcpptr%obs - termges
              pencur = pcpptr%err2*obsges*obsges
              do kk=1,nstep
                 if (pcp_ges>tinym1_obs) then
                    termgtl = pcp_gest/(one+pcp_ges)
                 else
                    termgtl = zero
                 endif
                 obsgtl= - termgtl
                 pentl   = two*pcpptr%err2*obsges*obsgtl
                 pen(kk) = pencur+sges(kk)*pentl
              enddo
           else
              do kk=1,nstep
                 pcp_ges = pcp_ges0 + sges(kk)*pcp_gest
!                Logrithmic formulation.  Ensure pcp_ges > zero
                 pcp_ges = max(pcp_ges,zero)
                 termges = log(one+pcp_ges)
                 obsges= pcpptr%obs - termges
                 pen(kk) = pcpptr%err2*obsges*obsges
              enddo
           end if

        else
           pen(1)=pcpptr%err2*pcp_ges0*pcp_ges0
        end if

        kx=pcpptr%icxp
!       Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. pg_pcp(kx) > tiny_r_kind .and.  &
                             b_pcp(kx)  > tiny_r_kind) then
           cg_pcp=cg_term/b_pcp(kx)
           wnotgross= one-pg_pcp(kx)*varqc_iter
           wgross = varqc_iter*pg_pcp(kx)*cg_pcp/wnotgross
           do kk=1,max(1,nstep)
              pen(kk)= -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

!       Accumulate stepsize terms
        out(1) = out(1)+ pen(1) * pcpptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+ (pen(kk)-pen(1)) * pcpptr%raterr2
        end do

     end if ! <luse>
     
     pcpptr => pcpNode_nextcast(pcpptr)
  end do
 
  return
end subroutine stppcp

end module stppcpmod
