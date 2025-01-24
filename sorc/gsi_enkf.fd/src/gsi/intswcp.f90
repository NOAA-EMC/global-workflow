module intswcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intswcpmod    module for intswcp and its tangent linear intswcp_tl
!   prgmmr:
!
! abstract: module for intswcp and its tangent linear intswcp_tl
!
! program history log:
!
! subroutines included:
!   sub intswcp_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_swcpNode, only: swcpNode
use m_swcpNode, only: swcpNode_typecast
use m_swcpNode, only: swcpNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intswcp

interface intswcp; module procedure &
          intswcp_
end interface

contains

subroutine intswcp_(swcphead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intswcp       apply nonlin qc obs operator for swcp
!   prgmmr: Ting-Chi Wu        org: CIRA/CSU                date: 2017-06-28 
!
! abstract: apply observation operator and adjoint for solid-water content path
!           with addition of nonlinear qc.
!
! program history log:
!   2017-06-28  Ting-Chi Wu - mimic the structure in intpw.f90 and intgps.f90 
!                           - intswcp.f90 includes 2 TL/ADJ options
!                             1) when l_wcp_cwm = .false.: 
!                                operator = f(T,P,q)
!                             2) when l_wcp_cwm = .true. and CWM partition6: 
!                                 operator = f(qi,qs,qg,qh) partition6
!
!   input argument list:
!     swcphead   - obs type pointer to obs structure
!     st       - t increment in grid space
!     sp       - p increment in grid space
!     sq       - q increment in grid space
!     sqi      - qi increment in grid space
!     sqs      - qs increment in grid space
!     sqg      - qg increment in grid space
!     sqh      - qh increment in grid space
!
!   output argument list:
!     rt       - results of t from swcp observation operator 
!     rp       - results of p from swcp observation operator 
!     rq       - results of q from swcp observation operator 
!     rqi      - results of qi from swcp observation operator 
!     rqs      - results of qs from swcp observation operator 
!     rqg      - results of qg from swcp observation operator 
!     rqh      - results of qh from swcp observation operator 
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
  use obsmod, only: l_wcp_cwm
  use gridmod, only: nsig
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero,half,one,tiny_r_kind,cg_term,r3600
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode), pointer, intent(in   ) :: swcphead
  type(gsi_bundle)        ,intent(in   ) :: sval
  type(gsi_bundle)        ,intent(inout) :: rval

! Declare local variables
  integer(i_kind) k,ier,istatus
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) :: t_TL,p_TL,q_TL
  real(r_kind) :: t_AD,p_AD,q_AD
  real(r_kind) :: qi_TL,qs_TL,qg_TL,qh_TL
  real(r_kind) :: qi_AD,qs_AD,qg_AD,qh_AD
  real(r_kind) val,w1,w2,w3,w4
  real(r_kind) cg_swcp,grad,p0,wnotgross,wgross,pg_swcp
  real(r_kind),pointer,dimension(:) :: st, sp, sq
  real(r_kind),pointer,dimension(:) :: sqi, sqs, sqg, sqh
  real(r_kind),pointer,dimension(:) :: rt, rp, rq
  real(r_kind),pointer,dimension(:) :: rqi, rqs, rqg, rqh
  type(swcpNode), pointer :: swcpptr

!  If no swcp data return
  if(.not. associated(swcphead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0

  if (.not.l_wcp_cwm) then

    call gsi_bundlegetpointer(sval,'tsen',st,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'prse',sp,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'q'   ,sq,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'tsen',rt,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'prse',rp,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'q'   ,rq,istatus);ier=istatus+ier

  else

    call gsi_bundlegetpointer(sval,'qi',sqi,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'qs',sqs,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'qg',sqg,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'qh',sqh,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qi',rqi,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qs',rqs,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qg',rqg,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qh',rqh,istatus);ier=istatus+ier

  endif ! l_wcp_cwm

  if(ier/=0)return

  !swcpptr => swcphead
  swcpptr => swcpNode_typecast(swcphead)
  do while (associated(swcpptr))

    do k=1,nsig
      i1(k)=swcpptr%ij(1,k)
      i2(k)=swcpptr%ij(2,k)
      i3(k)=swcpptr%ij(3,k)
      i4(k)=swcpptr%ij(4,k)
    enddo
    w1=swcpptr%wij(1)
    w2=swcpptr%wij(2)
    w3=swcpptr%wij(3)
    w4=swcpptr%wij(4)
     
    val=zero

!    Forward model (linear operator)

    if (.not.l_wcp_cwm) then
      do k=1,nsig
        t_TL=w1* st(i1(k))+w2* st(i2(k))+w3* st(i3(k))+w4* st(i4(k))
        p_TL=w1* sp(i1(k))+w2* sp(i2(k))+w3* sp(i3(k))+w4* sp(i4(k))
        q_TL=w1* sq(i1(k))+w2* sq(i2(k))+w3* sq(i3(k))+w4* sq(i4(k))
        val = val + ( t_TL*swcpptr%jac_t(k) + &
                      p_TL*swcpptr%jac_p(k) + & 
                      q_TL*swcpptr%jac_q(k) ) ! tpwcon*r10*(piges(k)-piges(k+1)) already did in setupswcp.f90
      end do
    else
      do k=1,nsig
        qi_TL=w1* sqi(i1(k))+w2* sqi(i2(k))+w3* sqi(i3(k))+w4* sqi(i4(k))
        qs_TL=w1* sqs(i1(k))+w2* sqs(i2(k))+w3* sqs(i3(k))+w4* sqs(i4(k))
        qg_TL=w1* sqg(i1(k))+w2* sqg(i2(k))+w3* sqg(i3(k))+w4* sqg(i4(k))
        qh_TL=w1* sqh(i1(k))+w2* sqh(i2(k))+w3* sqh(i3(k))+w4* sqh(i4(k))
        val = val + ( qi_TL*swcpptr%jac_qi(k) + & 
                      qs_TL*swcpptr%jac_qs(k) + &
                      qg_TL*swcpptr%jac_qg(k) + & 
                      qh_TL*swcpptr%jac_qh(k) ) ! tpwcon*r10*(piges(k)-piges(k+1)) already did in setupswcp.f90
      end do
    endif ! l_wcp_cwm

    if(luse_obsdiag)then
       if (lsaveobsens) then
          grad = val*swcpptr%raterr2*swcpptr%err2
          !-- swcpptr%diags%obssen(jiter) = grad
          call obsdiagNode_set(swcpptr%diags,jiter=jiter,obssen=grad)
       else
          !-- if (swcpptr%luse) swcpptr%diags%tldepart(jiter)=val
          if (swcpptr%luse) call obsdiagNode_set(swcpptr%diags,jiter=jiter,tldepart=val)
       endif
    end if

    if (l_do_adjoint) then
       if (.not. lsaveobsens) then
!         Difference from observation
          if( .not. ladtest_obs)   val=val-swcpptr%res
!         needed for gradient of nonlinear qc operator
          if (nlnqc_iter .and. swcpptr%pg > tiny_r_kind .and.  &
                               swcpptr%b  > tiny_r_kind) then
             pg_swcp=swcpptr%pg*varqc_iter
             cg_swcp=cg_term/swcpptr%b
             wnotgross= one-pg_swcp
             wgross = pg_swcp*cg_swcp/wnotgross
             p0   = wgross/(wgross+exp(-half*swcpptr%err2*val**2))
             val = val*(one-p0)
          endif
          if( ladtest_obs) then
             grad = val
          else
             grad = val*swcpptr%raterr2*swcpptr%err2
          end if
       endif

!      Adjoint
       if (.not.l_wcp_cwm) then
         do k=1,nsig
           t_AD = grad*swcpptr%jac_t(k)
           rt(i1(k))=rt(i1(k))+w1*t_AD
           rt(i2(k))=rt(i2(k))+w2*t_AD
           rt(i3(k))=rt(i3(k))+w3*t_AD
           rt(i4(k))=rt(i4(k))+w4*t_AD
           p_AD = grad*swcpptr%jac_p(k)
           rp(i1(k))=rp(i1(k))+w1*p_AD
           rp(i2(k))=rp(i2(k))+w2*p_AD
           rp(i3(k))=rp(i3(k))+w3*p_AD
           rp(i4(k))=rp(i4(k))+w4*p_AD
           q_AD = grad*swcpptr%jac_q(k)
           rq(i1(k))=rq(i1(k))+w1*q_AD
           rq(i2(k))=rq(i2(k))+w2*q_AD
           rq(i3(k))=rq(i3(k))+w3*q_AD
           rq(i4(k))=rq(i4(k))+w4*q_AD
         enddo
       else
         do k=1,nsig
           qi_AD = grad*swcpptr%jac_qi(k)
           rqi(i1(k))=rqi(i1(k))+w1*qi_AD
           rqi(i2(k))=rqi(i2(k))+w2*qi_AD
           rqi(i3(k))=rqi(i3(k))+w3*qi_AD
           rqi(i4(k))=rqi(i4(k))+w4*qi_AD
           qs_AD = grad*swcpptr%jac_qs(k)
           rqs(i1(k))=rqs(i1(k))+w1*qs_AD
           rqs(i2(k))=rqs(i2(k))+w2*qs_AD
           rqs(i3(k))=rqs(i3(k))+w3*qs_AD
           rqs(i4(k))=rqs(i4(k))+w4*qs_AD
           qg_AD = grad*swcpptr%jac_qg(k)
           rqg(i1(k))=rqg(i1(k))+w1*qg_AD
           rqg(i2(k))=rqg(i2(k))+w2*qg_AD
           rqg(i3(k))=rqg(i3(k))+w3*qg_AD
           rqg(i4(k))=rqg(i4(k))+w4*qg_AD
           qh_AD = grad*swcpptr%jac_qh(k)
           rqh(i1(k))=rqh(i1(k))+w1*qh_AD
           rqh(i2(k))=rqh(i2(k))+w2*qh_AD
           rqh(i3(k))=rqh(i3(k))+w3*qh_AD
           rqh(i4(k))=rqh(i4(k))+w4*qh_AD
         enddo
       endif ! l_wcp_cwm
     endif ! l_do_adjoint


     !swcpptr => swcpptr%llpoint
     swcpptr => swcpNode_nextcast(swcpptr)

  end do

  return

end subroutine intswcp_

end module intswcpmod
