module intspdmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intspdmod    module for intspd and its tangent linear intspd_tl
!   prgmmr:
!
! abstract: module for intspd and its tangent linear intspd_tl
!
! program history log:
!   2005-05-11  Yanqiu zhu - wrap intspd and its tangent linear intspd_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intspd_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub intspd_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intspd

interface intspd; module procedure &
          intspd_
end interface

contains

subroutine intspd_(spdhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intspd      apply nonlin qc obs operator for wind speed
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply nonlinear observation operator and adjoint for winds
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08 parrish  - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intspd and intspd_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed handling of ptr%time
!   2010-01-29  zhang,b  - fix adjoint of linearization
!   2010-02-26  todling  - fix for observation sensitivity
!   2010-05-13  todling  - update to use gsi_bundle; udpate interface
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!
!   input argument list:
!     spdhead  - obs type pointer to obs structure
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     ru
!     rv
!
!   output argument list:
!     spdhead  - obs type pointer to obs structure
!     ru       - u results from observation operator 
!     rv       - v results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use obsmod, only: spd_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use constants, only: zero, half, one, tiny_r_kind,cg_term,r3600
  use gridmod, only: latlon1n
  use gsi_4dvar, only: ltlint
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(spd_ob_type),pointer,intent(in   ) :: spdhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4
  real(r_kind) w1,w2,w3,w4,term,time_spd
! real(r_kind) penalty
  real(r_kind) uanl,vanl,spdanl,spd,valv,valu
  real(r_kind) uatl,vatl,spdatl,spdtra,grad
  real(r_kind) cg_spd,p0,wnotgross,wgross,pg_spd
  real(r_kind),pointer,dimension(:) :: xhat_dt_u,xhat_dt_v
  real(r_kind),pointer,dimension(:) :: dhat_dt_u,dhat_dt_v
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(spd_ob_type), pointer :: spdptr
  logical :: ltlint_tmp

!  If no spd data return
  if(.not. associated(spdhead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0
  call gsi_bundlegetpointer(sval,'u',su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',rv,istatus);ier=istatus+ier
  if(l_foto) then
     call gsi_bundlegetpointer(xhat_dt,'u',xhat_dt_u,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'v',xhat_dt_v,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'u',dhat_dt_u,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'v',dhat_dt_v,istatus);ier=istatus+ier
  endif
  if(ier/=0)return

  if( ladtest_obs) then
     ltlint_tmp = ltlint   
     ltlint = .true.
  end if
  spdptr => spdhead
  do while (associated(spdptr))

     j1 = spdptr%ij(1)
     j2 = spdptr%ij(2)
     j3 = spdptr%ij(3)
     j4 = spdptr%ij(4)
     w1 = spdptr%wij(1)
     w2 = spdptr%wij(2)
     w3 = spdptr%wij(3)
     w4 = spdptr%wij(4)


     valu=zero
     valv=zero
     spdtra=sqrt(spdptr%uges*spdptr%uges+spdptr%vges*spdptr%vges)

     if (ltlint) then

        if (spdtra>EPSILON(spdtra)) then
!          Forward model
           uatl=w1*su(j1)+w2*su(j2)+w3*su(j3)+w4*su(j4)
           vatl=w1*sv(j1)+w2*sv(j2)+w3*sv(j3)+w4*sv(j4)
           spdatl=spdptr%uges*uatl+spdptr%vges*vatl
           spdatl=spdatl/spdtra

           if(luse_obsdiag)then
              if (lsaveobsens) then
                 grad=spdptr%raterr2*spdptr%err2*spdatl
                 spdptr%diags%obssen(jiter)=grad
              else
                 if (spdptr%luse) spdptr%diags%tldepart(jiter)=spdatl
              endif
           endif

           if (l_do_adjoint) then
              if (.not. lsaveobsens) then
                 if( ladtest_obs ) then
                    grad=spdatl
                 else
                    spd=spdatl-spdptr%res+sqrt(spdptr%uges*spdptr%uges+spdptr%vges*spdptr%vges)
                    grad=spdptr%raterr2*spdptr%err2*spd
                 end if
              endif

!             Adjoint
              valu=grad*spdptr%uges/spdtra
              valv=grad*spdptr%vges/spdtra
           endif
        else
           if(luse_obsdiag)then
              if (spdptr%luse) spdptr%diags%tldepart(jiter)=zero
              if (lsaveobsens) spdptr%diags%obssen(jiter)=zero
           end if
        endif


     else ! < ltlint >

!       Forward model
        uanl=spdptr%uges+w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)
        vanl=spdptr%vges+w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)
        if ( l_foto ) then
           time_spd=spdptr%time*r3600
           uanl=uanl+&
                time_spd*(w1*xhat_dt_u(j1)+w2*xhat_dt_u(j2)+ &
                          w3*xhat_dt_u(j3)+w4*xhat_dt_u(j4))
           vanl=vanl+&
                time_spd*(w1*xhat_dt_v(j1)+w2*xhat_dt_v(j2)+ &
                          w3*xhat_dt_v(j3)+w4*xhat_dt_v(j4))
        endif
        spdanl=sqrt(uanl*uanl+vanl*vanl)
        if (luse_obsdiag .and. spdptr%luse) spdptr%diags%tldepart(jiter)=spdanl-spdtra

        if (l_do_adjoint) then
           valu=zero
           valv=zero
           spd=spdanl-spdptr%res
           grad=spdptr%raterr2*spdptr%err2*spd

!          Adjoint
!          if(spdanl > tiny_r_kind*100._r_kind) then
           if (spdanl>EPSILON(spdanl)) then
              if (luse_obsdiag .and. lsaveobsens) spdptr%diags%obssen(jiter)=grad
              valu=uanl/spdanl
              valv=vanl/spdanl
              if (nlnqc_iter .and. spdptr%pg > tiny_r_kind .and.  &
                                   spdptr%b  > tiny_r_kind) then
                 pg_spd=spdptr%pg*varqc_iter
                 cg_spd=cg_term/spdptr%b
                 wnotgross= one-pg_spd
                 wgross = pg_spd*cg_spd/wnotgross
                 p0 = wgross/(wgross+exp(-half*spdptr%err2*spd**2))
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

        if (l_foto) then
           valu=valu*time_spd
           valv=valv*time_spd
           dhat_dt_u(j1)=dhat_dt_u(j1)+w1*valu
           dhat_dt_u(j2)=dhat_dt_u(j2)+w2*valu
           dhat_dt_u(j3)=dhat_dt_u(j3)+w3*valu
           dhat_dt_u(j4)=dhat_dt_u(j4)+w4*valu
           dhat_dt_v(j1)=dhat_dt_v(j1)+w1*valv
           dhat_dt_v(j2)=dhat_dt_v(j2)+w2*valv
           dhat_dt_v(j3)=dhat_dt_v(j3)+w3*valv
           dhat_dt_v(j4)=dhat_dt_v(j4)+w4*valv
        endif
     endif

     spdptr => spdptr%llpoint

  end do
  if( ladtest_obs) ltlint = ltlint_tmp
  return
end subroutine intspd_

end module intspdmod
