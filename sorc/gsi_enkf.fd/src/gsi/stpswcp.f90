module stpswcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpswcpmod    module for stpswcp and its tangent linear stpswcp_tl
!  prgmmr:
!
! abstract: module for stpswcp and its tangent linear stpswcp_tl
!
! program history log:
!
! subroutines included:
!   sub stpswcp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC stpswcp

contains

subroutine stpswcp(swcphead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpswcp       calculate penalty and contribution to stepsize
!                              for swcp using nonlinear qc
!   prgmmr: Ting-Chi Wu        org: CIRA/CSU                date: 2017-06-28 
!
! abstract: calculate penalty and contribution to stepsize from swcp
!           using nonlinear qc.
!
! program history log:
!   2017-06-28  Ting-Chi Wu - mimic the structure in stppw.f90 and stpgps.f90 
!                           - stpswcp.f90 includes 2 stp options
!                             1) when l_wcp_cwm = .false.: 
!                                operator = f(T,P,q)
!                             2) when l_wcp_cwm = .true. and CWM partition6: 
!                                 operator = f(qi,qs,qg,qh) partition6
!
!   input argument list:
!     swcphead
!     rt       - search direction for t
!     rp       - search direction for p
!     rq       - search direction for q
!     rqi      - search direction for qi
!     rqs      - search direction for qs
!     rqg      - search direction for qg
!     rqh      - search direction for qh
!     st       - analysis increment for t
!     sp       - analysis increment for p 
!     sq       - analysis increment for q
!     sqi      - analysis increment for qi
!     sqs      - analysis increment for qs
!     sqg      - analysis increment for qg
!     sqh      - analysis increment for qh
!     sges     - stepsize estimates(4)
!     nstep    - number of stepsizes ( == 0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution to penalty for precip. water sges(1:nstep)
!
!   comments:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,two,tiny_r_kind,cg_term,zero_quad,&
       r3600
  use gridmod, only: nsig
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_swcpNode , only: swcpNode
  use m_swcpNode , only: swcpNode_typecast
  use m_swcpNode , only: swcpNode_nextcast
  use obsmod, only: l_wcp_cwm
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: swcphead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables  
  integer(i_kind) j,kk,ier,istatus
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
  real(r_kind) val,val2,w1,w2,w3,w4,pg_swcp
  real(r_kind) cg_swcp,wgross,wnotgross,swcpx
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind),pointer,dimension(:) :: st, sp, sq
  real(r_kind),pointer,dimension(:) :: sqi, sqs, sqg, sqh
  real(r_kind),pointer,dimension(:) :: rt, rp, rq
  real(r_kind),pointer,dimension(:) :: rqi, rqs, rqg, rqh
  real(r_kind) :: t_TL,p_TL,q_TL
  real(r_kind) :: rt_TL,rp_TL,rq_TL
  real(r_kind) :: qi_TL,qs_TL,qg_TL,qh_TL
  real(r_kind) :: rqi_TL,rqs_TL,rqg_TL,rqh_TL

  type(swcpNode), pointer :: swcpptr


  out=zero_quad

!  If no swcp data return
  if(.not. associated(swcphead))return

! Retrieve pointers
  ier=0

  if (.not.l_wcp_cwm) then

    call gsi_bundlegetpointer(sval,'tsen',st,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'prse',sp,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'q'   ,sq,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'tsen',rt,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'q'   ,rq,istatus);ier=istatus+ier
    !if (ier==0) write(6,*) 'STPSWCP (l_wcp_cwm = F)'

  else

    call gsi_bundlegetpointer(sval,'qi',sqi,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'qs',sqs,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'qg',sqg,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'qh',sqh,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qi',rqi,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qs',rqs,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qg',rqg,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qh',rqh,istatus);ier=istatus+ier
    !if (ier==0) write(6,*) 'STPSWCP (l_wcp_cwm = T)'

  endif ! l_wcp_cwm

  if(ier/=0)return

  swcpptr => swcpNode_typecast(swcphead)
  do while (associated(swcpptr))
     if(swcpptr%luse)then

        val2=-swcpptr%res
        if(nstep > 0)then

           do j=1,nsig
             i1(j)=swcpptr%ij(1,j)
             i2(j)=swcpptr%ij(2,j)
             i3(j)=swcpptr%ij(3,j)
             i4(j)=swcpptr%ij(4,j)
           enddo
           w1 = swcpptr%wij(1)
           w2 = swcpptr%wij(2)
           w3 = swcpptr%wij(3)
           w4 = swcpptr%wij(4)
             
           val=zero

!      Calculate solid-water content path increment and delta swcp increment

           if (.not.l_wcp_cwm) then
             do j=1,nsig
                 t_TL =w1* st(i1(j))+w2* st(i2(j))+w3* st(i3(j))+w4* st(i4(j))
                rt_TL =w1* rt(i1(j))+w2* rt(i2(j))+w3* rt(i3(j))+w4* rt(i4(j))
                 p_TL =w1* sp(i1(j))+w2* sp(i2(j))+w3* sp(i3(j))+w4* sp(i4(j))
                rp_TL =w1* rp(i1(j))+w2* rp(i2(j))+w3* rp(i3(j))+w4* rp(i4(j))
                 q_TL =w1* sq(i1(j))+w2* sq(i2(j))+w3* sq(i3(j))+w4* sq(i4(j))
                rq_TL =w1* rq(i1(j))+w2* rq(i2(j))+w3* rq(i3(j))+w4* rq(i4(j))
                val2 = val2 +  t_tl*swcpptr%jac_t(j)+ p_tl*swcpptr%jac_p(j)+ q_tl*swcpptr%jac_q(j)
                val  = val  + rt_tl*swcpptr%jac_t(j)+rp_tl*swcpptr%jac_p(j)+rq_tl*swcpptr%jac_q(j)
             enddo
           else
             do j=1,nsig
                qi_TL =w1* sqi(i1(j))+w2* sqi(i2(j))+w3* sqi(i3(j))+w4* sqi(i4(j))
               rqi_TL =w1* rqi(i1(j))+w2* rqi(i2(j))+w3* rqi(i3(j))+w4* rqi(i4(j))
                qs_TL =w1* sqs(i1(j))+w2* sqs(i2(j))+w3* sqs(i3(j))+w4* sqs(i4(j))
               rqs_TL =w1* rqs(i1(j))+w2* rqs(i2(j))+w3* rqs(i3(j))+w4* rqs(i4(j))
                qg_TL =w1* sqg(i1(j))+w2* sqg(i2(j))+w3* sqg(i3(j))+w4* sqg(i4(j))
               rqg_TL =w1* rqg(i1(j))+w2* rqg(i2(j))+w3* rqg(i3(j))+w4* rqg(i4(j))
                qh_TL =w1* sqh(i1(j))+w2* sqh(i2(j))+w3* sqh(i3(j))+w4* sqh(i4(j))
               rqh_TL =w1* rqh(i1(j))+w2* rqh(i2(j))+w3* rqh(i3(j))+w4* rqh(i4(j))
               val2 = val2 +  qi_tl*swcpptr%jac_qi(j)+  qs_tl*swcpptr%jac_qs(j) &
                           +  qg_tl*swcpptr%jac_qg(j)+  qh_tl*swcpptr%jac_qh(j)
               val  = val  + rqi_tl*swcpptr%jac_qi(j)+ rqs_tl*swcpptr%jac_qs(j) &
                           + rqg_tl*swcpptr%jac_qg(j)+ rqh_tl*swcpptr%jac_qh(j)
             enddo        
           endif ! l_wcp_cwm

           do kk=1,nstep
              swcpx=val2+sges(kk)*val
              pen(kk)=swcpx*swcpx*swcpptr%err2
           end do

        else
           pen(1)=val2*val2*swcpptr%err2
        end if

!  Modify penalty term if nonlinear QC
        if (nlnqc_iter .and. swcpptr%pg > tiny_r_kind .and. &
                             swcpptr%b  > tiny_r_kind) then
           pg_swcp=swcpptr%pg*varqc_iter
           cg_swcp=cg_term/swcpptr%b
           wnotgross= one-pg_swcp
           wgross = pg_swcp*cg_swcp/wnotgross
           do kk=1,max(1,nstep)
              pen(kk) = -two*log((exp(-half*pen(kk)) + wgross)/(one+wgross))
           end do
        endif

        out(1) = out(1)+pen(1)*swcpptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*swcpptr%raterr2
        end do
     end if

     swcpptr => swcpNode_nextcast(swcpptr)

  end do
  return
end subroutine stpswcp

end module stpswcpmod
