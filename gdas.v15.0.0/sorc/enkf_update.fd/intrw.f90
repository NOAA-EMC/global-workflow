module intrwmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   intrwmod    module for intrw and its tangent linear intrw_tl
!   prgmmr:
!
! abstract: module for intrw and its tangent linear intrw_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intrw and its tangent linear intrw_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intrw_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub intrw_
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
PUBLIC intrw

interface intrw; module procedure &
          intrw_
end interface

contains

subroutine intrw_(rwhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intrw       apply nonlin qc operator for radar winds
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator for radar winds
!             with nonlinear qc operator
!
! program history log:
!   1991-02-26  derber
!   1999-11-22  yang
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intrw and intrw_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic  - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!   2010-05-13  todlng   - update to use gsi_bundle; update interface
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
! usage: call intw(ru,rv,su,sv)
!   input argument list:
!     rwhead   - obs type pointer to obs structure     
!     su       - current u solution increment 
!     sv       - current v solution increment 
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
  use constants, only: half,one,tiny_r_kind,cg_term,r3600
  use obsmod, only: rw_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter
  use gridmod, only: latlon1n
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(rw_ob_type),pointer,intent(in   ) :: rwhead
  type(gsi_bundle),        intent(in   ) :: sval
  type(gsi_bundle),        intent(inout) :: rval

! Declare local varibles
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,ier,istatus
! real(r_kind) penalty
  real(r_kind),pointer,dimension(:) :: xhat_dt_u,xhat_dt_v
  real(r_kind),pointer,dimension(:) :: dhat_dt_u,dhat_dt_v
  real(r_kind) val,valu,valv,w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind) cg_rw,p0,grad,wnotgross,wgross,time_rw,pg_rw
  real(r_kind),pointer,dimension(:) :: su,sv
  real(r_kind),pointer,dimension(:) :: ru,rv
  type(rw_ob_type), pointer :: rwptr

!  If no rw data return
  if(.not. associated(rwhead))return

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


  rwptr => rwhead
  do while (associated(rwptr))
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


!    Forward mode l
     val=(w1* su(j1)+w2* su(j2)+w3* su(j3)+w4* su(j4)+                       &
          w5* su(j5)+w6* su(j6)+w7* su(j7)+w8* su(j8))*rwptr%cosazm+         &
         (w1* sv(j1)+w2* sv(j2)+w3* sv(j3)+w4* sv(j4)+                       &
          w5* sv(j5)+w6* sv(j6)+w7* sv(j7)+w8* sv(j8))*rwptr%sinazm
     if ( l_foto ) then
        time_rw=rwptr%time*r3600
        val=val+                                                    &
         ((w1*xhat_dt_u(j1)+w2*xhat_dt_u(j2)+                       &
           w3*xhat_dt_u(j3)+w4*xhat_dt_u(j4)+                       &
           w5*xhat_dt_u(j5)+w6*xhat_dt_u(j6)+                       &
           w7*xhat_dt_u(j7)+w8*xhat_dt_u(j8))*rwptr%cosazm+         &
          (w1*xhat_dt_v(j1)+w2*xhat_dt_v(j2)+                       &
           w3*xhat_dt_v(j3)+w4*xhat_dt_v(j4)+                       &
           w5*xhat_dt_v(j5)+w6*xhat_dt_v(j6)+                       &
           w7*xhat_dt_v(j7)+w8*xhat_dt_v(j8))*rwptr%sinazm)*time_rw
     endif

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*rwptr%raterr2*rwptr%err2
           rwptr%diags%obssen(jiter) = grad
        else
           if (rwptr%luse) rwptr%diags%tldepart(jiter)=val
        endif
     endif

     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs ) val=val-rwptr%res

!          gradient of nonlinear operator
           if (nlnqc_iter .and. rwptr%pg > tiny_r_kind .and. &
                                rwptr%b  > tiny_r_kind) then
              pg_rw=rwptr%pg*varqc_iter
              cg_rw=cg_term/rwptr%b
              wnotgross= one-pg_rw
              wgross = pg_rw*cg_rw/wnotgross
              p0   = wgross/(wgross+exp(-half*rwptr%err2*val**2))
              val = val*(one-p0)
           endif

           if( ladtest_obs)  then
              grad = val
           else
              grad = val*rwptr%raterr2*rwptr%err2
           end if

        endif

!       Adjoint
        valu=rwptr%cosazm*grad
        valv=rwptr%sinazm*grad
        ru(j1)=ru(j1)+w1*valu
        ru(j2)=ru(j2)+w2*valu
        ru(j3)=ru(j3)+w3*valu
        ru(j4)=ru(j4)+w4*valu
        ru(j5)=ru(j5)+w5*valu
        ru(j6)=ru(j6)+w6*valu
        ru(j7)=ru(j7)+w7*valu
        ru(j8)=ru(j8)+w8*valu
        rv(j1)=rv(j1)+w1*valv
        rv(j2)=rv(j2)+w2*valv
        rv(j3)=rv(j3)+w3*valv
        rv(j4)=rv(j4)+w4*valv
        rv(j5)=rv(j5)+w5*valv
        rv(j6)=rv(j6)+w6*valv
        rv(j7)=rv(j7)+w7*valv
        rv(j8)=rv(j8)+w8*valv
 
        if ( l_foto ) then
           valu=valu*time_rw
           valv=valv*time_rw
           dhat_dt_u(j1)=dhat_dt_u(j1)+w1*valu
           dhat_dt_u(j2)=dhat_dt_u(j2)+w2*valu
           dhat_dt_u(j3)=dhat_dt_u(j3)+w3*valu
           dhat_dt_u(j4)=dhat_dt_u(j4)+w4*valu
           dhat_dt_u(j5)=dhat_dt_u(j5)+w5*valu
           dhat_dt_u(j6)=dhat_dt_u(j6)+w6*valu
           dhat_dt_u(j7)=dhat_dt_u(j7)+w7*valu
           dhat_dt_u(j8)=dhat_dt_u(j8)+w8*valu
           dhat_dt_v(j1)=dhat_dt_v(j1)+w1*valv
           dhat_dt_v(j2)=dhat_dt_v(j2)+w2*valv
           dhat_dt_v(j3)=dhat_dt_v(j3)+w3*valv
           dhat_dt_v(j4)=dhat_dt_v(j4)+w4*valv
           dhat_dt_v(j5)=dhat_dt_v(j5)+w5*valv
           dhat_dt_v(j6)=dhat_dt_v(j6)+w6*valv
           dhat_dt_v(j7)=dhat_dt_v(j7)+w7*valv
           dhat_dt_v(j8)=dhat_dt_v(j8)+w8*valv
        endif
     endif

     rwptr => rwptr%llpoint
  end do
  return
end subroutine intrw_

end module intrwmod
