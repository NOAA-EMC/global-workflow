module intlwcpmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intlwcpmod    module for intlwcp and its tangent linear intlwcp_tl
!   prgmmr:
!
! abstract: module for intlwcp and its tangent linear intlwcp_tl
!
! program history log:
!
! subroutines included:
!   sub intlwcp_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_lwcpNode, only: lwcpNode
use m_lwcpNode, only: lwcpNode_typecast
use m_lwcpNode, only: lwcpNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intlwcp

interface intlwcp; module procedure &
          intlwcp_
end interface

contains

subroutine intlwcp_(lwcphead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intlwcp       apply nonlin qc obs operator for lwcp
!   prgmmr: Ting-Chi Wu        org: CIRA/CSU                date: 2017-06-28 
!
! abstract: apply observation operator and adjoint for solid-water content path
!           with addition of nonlinear qc.
!
! program history log:
!   2017-06-28  Ting-Chi Wu - mimic the structure in intpw.f90 and intgps.f90 
!                           - intlwcp.f90 includes 2 TL/ADJ options
!                             1) when l_wcp_cwm = .false.: 
!                                operator = f(T,P,q)
!                             2) when l_wcp_cwm = .true. and CWM partition6: 
!                                 operator = f(ql,qr) partition6
!
!   input argument list:
!     lwcphead   - obs type pointer to obs structure
!     st       - t increment in grid space
!     sp       - p increment in grid space
!     sq       - q increment in grid space
!     sql      - ql increment in grid space
!     sqr      - qr increment in grid space
!
!   output argument list:
!     rt       - results of t from lwcp observation operator 
!     rp       - results of p from lwcp observation operator 
!     rq       - results of q from lwcp observation operator 
!     rql      - results of ql from lwcp observation operator 
!     rqr      - results of qr from lwcp observation operator 
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
  class(obsNode), pointer, intent(in   ) :: lwcphead
  type(gsi_bundle)        ,intent(in   ) :: sval
  type(gsi_bundle)        ,intent(inout) :: rval

! Declare local variables
  integer(i_kind) k,ier,istatus
  integer(i_kind),dimension(nsig):: i1,i2,i3,i4
! real(r_kind) penalty
  real(r_kind) :: t_TL,p_TL,q_TL
  real(r_kind) :: t_AD,p_AD,q_AD
  real(r_kind) :: ql_TL,qr_TL
  real(r_kind) :: ql_AD,qr_AD
  real(r_kind) val,w1,w2,w3,w4
  real(r_kind) cg_lwcp,grad,p0,wnotgross,wgross,pg_lwcp
  real(r_kind),pointer,dimension(:) :: st, sp, sq
  real(r_kind),pointer,dimension(:) :: sql, sqr
  real(r_kind),pointer,dimension(:) :: rt, rp, rq
  real(r_kind),pointer,dimension(:) :: rql, rqr
  type(lwcpNode), pointer :: lwcpptr

!  If no lwcp data return
  if(.not. associated(lwcphead))return
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

    call gsi_bundlegetpointer(sval,'ql',sql,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(sval,'qr',sqr,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'ql',rql,istatus);ier=istatus+ier
    call gsi_bundlegetpointer(rval,'qr',rqr,istatus);ier=istatus+ier

  endif ! l_wcp_cwm

  if(ier/=0)return

  !lwcpptr => lwcphead
  lwcpptr => lwcpNode_typecast(lwcphead)
  do while (associated(lwcpptr))

     w1=lwcpptr%wij(1)
     w2=lwcpptr%wij(2)
     w3=lwcpptr%wij(3)
     w4=lwcpptr%wij(4)
    do k=1,nsig
      i1(k)=lwcpptr%ij(1,k)
      i2(k)=lwcpptr%ij(2,k)
      i3(k)=lwcpptr%ij(3,k)
      i4(k)=lwcpptr%ij(4,k)
    enddo
     
     val=zero

!    Forward model

     if (.not.l_wcp_cwm) then
       do k=1,nsig
         t_TL=w1* st(i1(k))+w2* st(i2(k))+w3* st(i3(k))+w4* st(i4(k))
         p_TL=w1* sp(i1(k))+w2* sp(i2(k))+w3* sp(i3(k))+w4* sp(i4(k))
         q_TL=w1* sq(i1(k))+w2* sq(i2(k))+w3* sq(i3(k))+w4* sq(i4(k))
         val = val + ( t_TL*lwcpptr%jac_t(k) + &
                       p_TL*lwcpptr%jac_p(k) + & 
                       q_TL*lwcpptr%jac_q(k) ) ! tpwcon*r10*(piges(k)-piges(k+1)) already did in setuplwcp.f90
       end do
     else
       do k=1,nsig
         ql_TL=w1* sql(i1(k))+w2* sql(i2(k))+w3* sql(i3(k))+w4* sql(i4(k))
         qr_TL=w1* sqr(i1(k))+w2* sqr(i2(k))+w3* sqr(i3(k))+w4* sqr(i4(k))
         val = val + ( ql_TL*lwcpptr%jac_ql(k) + & 
                       qr_TL*lwcpptr%jac_qr(k) ) ! tpwcon*r10*(piges(k)-piges(k+1)) already did in setuplwcp.f90
       end do
     endif ! l_wcp_cwm

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*lwcpptr%raterr2*lwcpptr%err2
           !-- lwcpptr%diags%obssen(jiter) = grad
           call obsdiagNode_set(lwcpptr%diags,jiter=jiter,obssen=grad)
        else
           !-- if (lwcpptr%luse) lwcpptr%diags%tldepart(jiter)=val
           if (lwcpptr%luse) call obsdiagNode_set(lwcpptr%diags,jiter=jiter,tldepart=val)
        endif
     end if

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
!          Difference from observation
           if( .not. ladtest_obs)   val=val-lwcpptr%res
!          needed for gradient of nonlinear qc operator
           if (nlnqc_iter .and. lwcpptr%pg > tiny_r_kind .and.  &
                                lwcpptr%b  > tiny_r_kind) then
              pg_lwcp=lwcpptr%pg*varqc_iter
              cg_lwcp=cg_term/lwcpptr%b
              wnotgross= one-pg_lwcp
              wgross = pg_lwcp*cg_lwcp/wnotgross
              p0   = wgross/(wgross+exp(-half*lwcpptr%err2*val**2))
              val = val*(one-p0)
           endif
           if( ladtest_obs) then
              grad = val
           else
              grad = val*lwcpptr%raterr2*lwcpptr%err2
           end if
        endif

!       Adjoint

        if (.not.l_wcp_cwm) then
          do k=1,nsig
            t_AD = grad*lwcpptr%jac_t(k)
            rt(i1(k))=rt(i1(k))+w1*t_AD
            rt(i2(k))=rt(i2(k))+w2*t_AD
            rt(i3(k))=rt(i3(k))+w3*t_AD
            rt(i4(k))=rt(i4(k))+w4*t_AD
            p_AD = grad*lwcpptr%jac_p(k)
            rp(i1(k))=rp(i1(k))+w1*p_AD
            rp(i2(k))=rp(i2(k))+w2*p_AD
            rp(i3(k))=rp(i3(k))+w3*p_AD
            rp(i4(k))=rp(i4(k))+w4*p_AD
            q_AD = grad*lwcpptr%jac_q(k)
            rq(i1(k))=rq(i1(k))+w1*q_AD
            rq(i2(k))=rq(i2(k))+w2*q_AD
            rq(i3(k))=rq(i3(k))+w3*q_AD
            rq(i4(k))=rq(i4(k))+w4*q_AD
          enddo
        else
          do k=1,nsig
            ql_AD = grad*lwcpptr%jac_ql(k)
            rql(i1(k))=rql(i1(k))+w1*ql_AD
            rql(i2(k))=rql(i2(k))+w2*ql_AD
            rql(i3(k))=rql(i3(k))+w3*ql_AD
            rql(i4(k))=rql(i4(k))+w4*ql_AD
            qr_AD = grad*lwcpptr%jac_qr(k)
            rqr(i1(k))=rqr(i1(k))+w1*qr_AD
            rqr(i2(k))=rqr(i2(k))+w2*qr_AD
            rqr(i3(k))=rqr(i3(k))+w3*qr_AD
            rqr(i4(k))=rqr(i4(k))+w4*qr_AD
          enddo
        endif ! l_wcp_cwm

     endif ! l_do_adjoint

     !lwcpptr => lwcpptr%llpoint
     lwcpptr => lwcpNode_nextcast(lwcpptr)

  end do

  return
end subroutine intlwcp_

end module intlwcpmod
