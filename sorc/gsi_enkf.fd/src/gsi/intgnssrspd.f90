module intgnssrspdmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intgnssrspdmod    module for intgnssrspd and its tangent linear i ntgnssrspd_tl
!           (Based on intspd.f90)
!   prgmmr: Apodaca, K. - Add CYGNSS TL and AD modules
!   email: Karina.Apodaca@Spire.com
!
! abstract: module for intgnssrspd and its tangent linear intgnssrspd_tl
!
! program history log:
!   2023-04-21  k apodaca - initial version
!
! subroutines included:
!   sub intgnssrspd_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use m_obsNode, only: obsNode
use m_gnssrspdNode, only: gnssrspdNode
use m_gnssrspdNode, only: gnssrspdNode_typecast
use m_gnssrspdNode, only: gnssrspdNode_nextcast
use m_obsdiagNode, only: obsdiagNode_set
implicit none

PRIVATE
PUBLIC intgnssrspd

interface intgnssrspd; module procedure &
          intgnssrspd_
end interface

contains

subroutine intgnssrspd_(gnssrspdhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intgnssrspd      apply nonlin qc obs operator for wind speed
!   prgmmr: K. Apodaca       org: Spire Global, Inc. date: 2023-03-15
!   email: Karina.Apodaca@Spire.com
!   Based on intspd.f90 by  prgmmr: derber     org: np23   date: 1991-02-26
!
! abstract: apply nonlinear observation operator and adjoint for winds
!
! program history log:
!   2023-04-21  k apodaca - initial version`
!
!   input argument list:
!     gnssrspdhead  - obs type pointer to obs structure
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     ru
!     rv
!
!   output argument list:
!     gnssrspdhead  - obs type pointer to obs structure
!     ru       - u results from observation operator 
!     rv       - v results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero, half, one, tiny_r_kind,cg_term,r3600
  use gsi_4dvar, only: ltlint
  use jfunc, only: jiter
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  class(obsNode),  pointer, intent(in   ) :: gnssrspdhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4,term
! real(r_kind) penalty
  real(r_kind) uanl,vanl,gnssrspdanl,gnssrspd,valv,valu
  real(r_kind) uatl,vatl,gnssrspdatl,gnssrspdtra,grad
  real(r_kind) cg_gnssrspd,p0,wnotgross,wgross,pg_gnssrspd
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(gnssrspdNode), pointer :: gnssrspdptr
  logical :: ltlint_tmp

!  If no gnssrspd data return
  if(.not. associated(gnssrspdhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(ier/=0)return

  if( ladtest_obs) then
     ltlint_tmp = ltlint   
     ltlint = .true.
  end if
  !gnssrspdptr => gnssrspdhead
  gnssrspdptr => gnssrspdNode_typecast(gnssrspdhead)
  do while (associated(gnssrspdptr))

     j1 = gnssrspdptr%ij(1)
     j2 = gnssrspdptr%ij(2)
     j3 = gnssrspdptr%ij(3)
     j4 = gnssrspdptr%ij(4)
     w1 = gnssrspdptr%wij(1)
     w2 = gnssrspdptr%wij(2)
     w3 = gnssrspdptr%wij(3)
     w4 = gnssrspdptr%wij(4)


     valu=zero
     valv=zero
     gnssrspdtra=sqrt(gnssrspdptr%uges*gnssrspdptr%uges+gnssrspdptr%vges*gnssrspdptr%vges)

     if (ltlint) then

        if (gnssrspdtra>EPSILON(gnssrspdtra)) then
!          Forward model
           uatl=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)
           vatl=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)
           gnssrspdatl=gnssrspdptr%uges*uatl+gnssrspdptr%vges*vatl
           gnssrspdatl=gnssrspdatl/gnssrspdtra

           if(luse_obsdiag)then
              if (lsaveobsens) then
                 grad=gnssrspdptr%raterr2*gnssrspdptr%err2*gnssrspdatl
                 !-- gnssrspdptr%diags%obssen(jiter)=grad
                 call obsdiagNode_set(gnssrspdptr%diags,jiter=jiter,obssen=grad)
              else
                 !-- if (gnssrspdptr%luse) gnssrspdptr%diags%tldepart(jiter)=gnssrspdatl
                 if (gnssrspdptr%luse) call obsdiagNode_set(gnssrspdptr%diags,jiter=jiter,tldepart=gnssrspdatl)
              endif
           endif

           if (l_do_adjoint) then
              if (.not. lsaveobsens) then
                 if( ladtest_obs ) then
                    grad=gnssrspdatl
                 else
                    gnssrspd=gnssrspdatl-gnssrspdptr%res+sqrt(gnssrspdptr%uges*gnssrspdptr%uges+gnssrspdptr%vges*gnssrspdptr%vges)
                    grad=gnssrspdptr%raterr2*gnssrspdptr%err2*gnssrspd
                 end if
              endif

!             Adjoint
              valu=grad*gnssrspdptr%uges/gnssrspdtra
              valv=grad*gnssrspdptr%vges/gnssrspdtra
           endif
        else
           if(luse_obsdiag)then
              !-- if (gnssrspdptr%luse) gnssrspdptr%diags%tldepart(jiter)=zero
              if (gnssrspdptr%luse) call obsdiagNode_set(gnssrspdptr%diags,jiter=jiter,tldepart=zero)
              !-- if (lsaveobsens) gnssrspdptr%diags%obssen(jiter)=zero
              if (lsaveobsens) call obsdiagNode_set(gnssrspdptr%diags,jiter=jiter,obssen=zero)
           end if
        endif


     else ! < ltlint >

!       Forward model
        uanl=gnssrspdptr%uges+w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)
        vanl=gnssrspdptr%vges+w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)
        gnssrspdanl=sqrt(uanl*uanl+vanl*vanl)
        !-- if (luse_obsdiag .and. gnssrspdptr%luse) gnssrspdptr%diags%tldepart(jiter)=gnssrspdanl-gnssrspdtra
        if (luse_obsdiag .and. gnssrspdptr%luse) call obsdiagNode_set(gnssrspdptr%diags,jiter=jiter,tldepart=gnssrspdanl-gnssrspdtra)

        if (l_do_adjoint) then
           valu=zero
           valv=zero
           gnssrspd=gnssrspdanl-gnssrspdptr%res
           grad=gnssrspdptr%raterr2*gnssrspdptr%err2*gnssrspd

!          Adjoint
!          if(gnssrspdanl > tiny_r_kind*100._r_kind) then
           if (gnssrspdanl>EPSILON(gnssrspdanl)) then
              !-- if (luse_obsdiag .and. lsaveobsens) gnssrspdptr%diags%obssen(jiter)=grad
              if (luse_obsdiag .and. lsaveobsens) call obsdiagNode_set(gnssrspdptr%diags,jiter=jiter,obssen=grad)
              valu=uanl/gnssrspdanl
              valv=vanl/gnssrspdanl
              if (nlnqc_iter .and. gnssrspdptr%pg > tiny_r_kind .and.  &
                                   gnssrspdptr%b  > tiny_r_kind) then
                 pg_gnssrspd=gnssrspdptr%pg*varqc_iter
                 cg_gnssrspd=cg_term/gnssrspdptr%b
                 wnotgross= one-pg_gnssrspd
                 wgross = pg_gnssrspd*cg_gnssrspd/wnotgross
                 p0 = wgross/(wgross+exp(-half*gnssrspdptr%err2*gnssrspd**2))
                 term = (one-p0)
                 grad = grad*term
              endif
           end if
        endif ! < l_do_adjoint >

        valu=valu*grad
        valv=valv*grad

     endif ! < ltlint >


     if (l_do_adjoint) then
        ru(j1)=ru(j1)+w1*valu
        ru(j2)=ru(j2)+w2*valu
        ru(j3)=ru(j3)+w3*valu
        ru(j4)=ru(j4)+w4*valu
        rv(j1)=rv(j1)+w1*valv
        rv(j2)=rv(j2)+w2*valv
        rv(j3)=rv(j3)+w3*valv
        rv(j4)=rv(j4)+w4*valv

     endif

     !gnssrspdptr => gnssrspdptr%llpoint
     gnssrspdptr => gnssrspdNode_nextcast(gnssrspdptr)

  end do
  if( ladtest_obs) ltlint = ltlint_tmp
  return
end subroutine intgnssrspd_

end module intgnssrspdmod
