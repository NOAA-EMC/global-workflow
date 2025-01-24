module intwmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intwmod    module for intw and its tangent linear intw_tl
!   prgmmr:
!
! abstract: module for intw and its tangent linear intw_tl
!
! program history log:
!   2005-05-16  Yanqiu zhu - wrap intw and its tangent linear intw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intw_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2015-02-26       su - add njqc as an option to chose new non linear qc
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub intw_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_wNode, only: wNode
use m_wNode, only: wNode_typecast
use m_wNode, only: wNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intw

interface intw; module procedure &
          intw_
end interface

contains

subroutine intw_(whead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intw        apply nonlin qc obs operator for winds
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for winds with
!             nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-09  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intw and intw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su      - modify for variational qc
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!   2010-03-13  todling  - update to use gsi_bundle; update interface
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-04-12       su - add non linear qc from Purser's scheme
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!   2019-09-20  Su      - add new variational scheme
!
!   input argument list:
!     whead    - obs type pointer to obs structure
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     ru
!     rv
!
!   output argument list:
!     ru       - u results from observation operator 
!     rv       - v results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: half,one,tiny_r_kind,cg_term,r3600,two
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter,njqc,vqc,nvqc,hub_norm
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs 
  use pvqc, only: vqch,vqcs
  implicit none

! Declare passed variables
  class(obsNode), pointer,intent(in   ) :: whead
  type(gsi_bundle),       intent(in   ) :: sval
  type(gsi_bundle),       intent(inout) :: rval

! Declare local variables
  integer(i_kind) i1,i2,i3,i4,i5,i6,i7,i8,ier,istatus
  integer(i_kind) ib,ik
! real(r_kind) penalty
  real(r_kind) valu,valv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) gu,gv,wu,wv,ww,con
  real(r_kind) cg_w,p0,gradu,gradv,wnotgross,wgross,term,w_pg
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(wNode), pointer :: wptr

!  If no w data return
  if(.not. associated(whead))return

  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(ier/=0)return

  !wptr => whead
  wptr => wNode_typecast(whead)
  do while(associated(wptr))
     i1=wptr%ij(1)
     i2=wptr%ij(2)
     i3=wptr%ij(3)
     i4=wptr%ij(4)
     i5=wptr%ij(5)
     i6=wptr%ij(6)
     i7=wptr%ij(7)
     i8=wptr%ij(8)
     w1=wptr%wij(1)
     w2=wptr%wij(2)
     w3=wptr%wij(3)
     w4=wptr%wij(4)
     w5=wptr%wij(5)
     w6=wptr%wij(6)
     w7=wptr%wij(7)
     w8=wptr%wij(8)
  
!    Forward model
     valu=w1* su(i1)+w2* su(i2)+w3* su(i3)+w4* su(i4)+&
          w5* su(i5)+w6* su(i6)+w7* su(i7)+w8* su(i8)
     valv=w1* sv(i1)+w2* sv(i2)+w3* sv(i3)+w4* sv(i4)+&
          w5* sv(i5)+w6* sv(i6)+w7* sv(i7)+w8* sv(i8)

     if(luse_obsdiag)then
        if (lsaveobsens) then
           gradu = valu*wptr%raterr2*wptr%err2
           gradv = valv*wptr%raterr2*wptr%err2
           !-- wptr%diagu%obssen(jiter) = gradu
           !-- wptr%diagv%obssen(jiter) = gradv
           call obsdiagNode_set(wptr%diagu,jiter=jiter,obssen=gradu)
           call obsdiagNode_set(wptr%diagv,jiter=jiter,obssen=gradv)
        else
           if (wptr%luse) then
              !-- wptr%diagu%tldepart(jiter)=valu
              !-- wptr%diagv%tldepart(jiter)=valv
              call obsdiagNode_set(wptr%diagu,jiter=jiter,tldepart=valu)
              call obsdiagNode_set(wptr%diagv,jiter=jiter,tldepart=valv)
           endif
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs) then
              valu=valu-wptr%ures
              valv=valv-wptr%vres
           end if

!          gradient of nonlinear operator

 
           if (vqc .and. nlnqc_iter .and. wptr%pg > tiny_r_kind .and.  &
                                wptr%b  > tiny_r_kind) then
              w_pg=wptr%pg*varqc_iter
              cg_w=cg_term/wptr%b
              wnotgross= one-w_pg
              wgross =w_pg*cg_w/wnotgross                ! wgross is gama in Enderson
              p0=wgross/(wgross+                      &  ! p0 is P in Enderson
              exp(-half*wptr%err2*(valu**2+valv**2))) 
              term=one-p0                                !  term is Wqc in Enderson
              valu = valu*term
              valv = valv*term
              gradu = valu*wptr%raterr2*wptr%err2
              gradv = valv*wptr%raterr2*wptr%err2
            else if (njqc .and. wptr%jb  > tiny_r_kind .and. wptr%jb <10.0_r_kind) then
              con=sqrt(wptr%err2)
              valu=sqrt(two*wptr%jb)*tanh(con*valu/sqrt(two*wptr%jb))
              valv=sqrt(two*wptr%jb)*tanh(con*valv/sqrt(two*wptr%jb))
              gradu = valu*wptr%raterr2*con
              gradv = valv*wptr%raterr2*con
           else if (nvqc .and. wptr%ib >0) then
              ib=wptr%ib
              ik=wptr%ik
              con=sqrt(wptr%err2)
              ww=valu*con
              if(hub_norm) then
                 call vqch(ib,ik,ww,gu,wu)
              else
                 call vqcs(ib,ik,ww,gu,wu)
              endif
              gradu =wu*ww*con*wptr%raterr2
              ww=valv*con
              if(hub_norm) then
                 call vqch(ib,ik,ww,gv,wv)
              else 
                 call vqcs(ib,ik,ww,gv,wv)
              endif
              gradv =wv*ww*con*wptr%raterr2
           else
              gradu = valu*wptr%raterr2*wptr%err2
              gradv = valv*wptr%raterr2*wptr%err2
           endif


           if( ladtest_obs) then
              gradu = valu
              gradv = valv
           end if
        endif

!       Adjoint
        ru(i1)=ru(i1)+w1*gradu
        ru(i2)=ru(i2)+w2*gradu
        ru(i3)=ru(i3)+w3*gradu
        ru(i4)=ru(i4)+w4*gradu
        ru(i5)=ru(i5)+w5*gradu
        ru(i6)=ru(i6)+w6*gradu
        ru(i7)=ru(i7)+w7*gradu
        ru(i8)=ru(i8)+w8*gradu

        rv(i1)=rv(i1)+w1*gradv
        rv(i2)=rv(i2)+w2*gradv
        rv(i3)=rv(i3)+w3*gradv
        rv(i4)=rv(i4)+w4*gradv
        rv(i5)=rv(i5)+w5*gradv
        rv(i6)=rv(i6)+w6*gradv
        rv(i7)=rv(i7)+w7*gradv
        rv(i8)=rv(i8)+w8*gradv
     
     endif

     !wptr => wptr%llpoint
     wptr => wNode_nextcast(wptr)

  end do
  return
end subroutine intw_

end module intwmod
