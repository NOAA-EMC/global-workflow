module inttmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   inttmod    module for intt and its tangent linear intt_tl
!   prgmmr:
!
! abstract: module for intt and its tangent linear intt_tl
!
! program history log:
!   2005-05-12  Yanqiu zhu - wrap intt and its tangent linear intt_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intt_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!   2013-10-28  todling - rename p3d to prse
!   2014-04-09      Su  - add non linear qc from Purser's scheme
!   2015-02-26      Su  - add njqc as an option to choose Purser varqc
!
! subroutines included:
!   sub intt_
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
PUBLIC intt

interface intt; module procedure &
          intt_
end interface

contains

subroutine intt_(thead,rval,sval,rpred,spred)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intt        apply nonlin qc observation operator for temps
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: apply observation operator and adjoint for temperatures with
!             nonlinear qc operator
!
!
! program history log:
!   1991-02-26  derber
!   1997-12-12  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-05  parrish - add non-linear qc option
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intt and intt_qc into single routine
!   2005-08-02  derber  - modify for variational qc parameters for each ob
!   2005-09-28  derber  - consolidate location and weight arrays
!   2005-10-21  su  - modify for variational qc
!   2005-12-20  parrish - add option for boundary layer tlm
!   2006-03-30  park - correct indexing error for surface temp adjoint interpolation
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-20  derber - add sensible temperature for conventional temperatures
!   2006-10-20  rancic - add foto
!   2007-03-19  tremolet - binning of observations
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-11-26  Todling - turned FOTO optional; changed ptr%time handle
!   2010-03-25  zhu  - use state_vector in the interface; 
!                    - add nrf2_sst case; add pointer_state
!   2010-05-13  todling  - update to use gsi_bundle
!                        - on-the-spot handling of non-essential vars
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2013-05-26  zhu  - add aircraft temperature bias correction contribution
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!   2015-12-21  yang    - Parrish's correction to the previous code in new varqc.
!
!   input argument list:
!     thead    - obs type pointer to obs structure
!     st       - sensible temperature increment in grid space
!     stv      - virtual temperature increment in grid space
!     sq       - moisture increment in grid space
!     su       - u increment in grid space
!     sv       - v increment in grid space
!     sp       - surface pressure increment in grid space
!     ssst     - sst increment in grid space
!     rt
!     rtv
!     rq
!     ru
!     rv
!     rp
!     rsst
!
!   output argument list:
!     rt       - sensible temperature results from observation operator
!     rtv      - virtual temperature results from observation operator
!     rq       - moisture results from observation operator
!     ru       - u results from observation operator
!     rv       - v results from observation operator
!     rp       - surface pressure results from observation operator
!     rsst     - sst results from observation operator
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: half,one,zero,tiny_r_kind,cg_term,r3600,two
  use obsmod, only: t_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
  use qcmod, only: nlnqc_iter,varqc_iter,njqc,vqc
  use gridmod, only: latlon1n,latlon11,latlon1n1
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: gsi_bundleprint
  use gsi_4dvar, only: ladtest_obs 
  use aircraftinfo, only: npredt,ntail,aircraft_t_bc_pof,aircraft_t_bc
  implicit none
  

! Declare passed variables
  type(t_ob_type),pointer,intent(in   ) :: thead
  type(gsi_bundle)       ,intent(in   ) :: sval
  type(gsi_bundle)       ,intent(inout) :: rval
  real(r_kind),optional,dimension(npredt*ntail),intent(in   ) :: spred
  real(r_quad),optional,dimension(npredt*ntail),intent(inout) :: rpred

  real(r_kind),dimension(:),pointer :: st,stv,sq,su,sv
  real(r_kind),dimension(:),pointer :: ssst
  real(r_kind),dimension(:),pointer :: sp
  real(r_kind),dimension(:),pointer :: rt,rtv,rq,ru,rv
  real(r_kind),dimension(:),pointer :: rsst
  real(r_kind),dimension(:),pointer :: rp
  real(r_kind),dimension(:),pointer :: xhat_dt_tsen,xhat_dt_t,xhat_dt_q,xhat_dt_u,xhat_dt_v,xhat_dt_prse
  real(r_kind),dimension(:),pointer :: dhat_dt_tsen,dhat_dt_t,dhat_dt_q,dhat_dt_u,dhat_dt_v,dhat_dt_prse

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,ier,istatus,isst,ix,n
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,time_t
! real(r_kind) penalty
  real(r_kind) cg_t,val,p0,grad,wnotgross,wgross,t_pg
  real(r_kind) psfc_grad,tg_grad
  real(r_kind) ts_grad,us_grad,vs_grad,qs_grad
  real(r_kind) qs_prime0,tg_prime0,ts_prime0,psfc_prime0
  real(r_kind) us_prime0,vs_prime0
  type(t_ob_type), pointer :: tptr

!  If no t data return
  if(.not. associated(thead))return

! Retrieve pointers
! Simply return if any pointer not found
  ier=0; isst=0
  call gsi_bundlegetpointer(sval,'tsen', st,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'tv',  stv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'q',    sq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'u',    su,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'v',    sv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'prse', sp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(sval,'sst',ssst,istatus);isst=istatus+isst
  if(ier/=0) return

  call gsi_bundlegetpointer(rval,'tsen', rt,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'tv',  rtv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'q',    rq,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'u',    ru,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'v',    rv,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'prse', rp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'sst',rsst,istatus);isst=istatus+isst
  if(ier/=0) return

  if (l_foto) then
     call gsi_bundlegetpointer(xhat_dt,'tsen',xhat_dt_tsen,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'tv',     xhat_dt_t,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'q',      xhat_dt_q,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'u',      xhat_dt_u,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'v',      xhat_dt_v,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(xhat_dt,'prse',xhat_dt_prse,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'tsen',dhat_dt_tsen,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'tv',     dhat_dt_t,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'q',      dhat_dt_q,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'u',      dhat_dt_u,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'v',      dhat_dt_v,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'prse',dhat_dt_prse,istatus);ier=istatus+ier
     if(ier/=0)return
  endif

  time_t=zero
  tptr => thead
  do while (associated(tptr))

     j1=tptr%ij(1)
     j2=tptr%ij(2)
     j3=tptr%ij(3)
     j4=tptr%ij(4)
     j5=tptr%ij(5)
     j6=tptr%ij(6)
     j7=tptr%ij(7)
     j8=tptr%ij(8)
     w1=tptr%wij(1)
     w2=tptr%wij(2)
     w3=tptr%wij(3)
     w4=tptr%wij(4)
     w5=tptr%wij(5)
     w6=tptr%wij(6)
     w7=tptr%wij(7)
     w8=tptr%wij(8)

     if(l_foto) time_t=tptr%time*r3600
     if(tptr%use_sfc_model) then

!----------use surface model----------------------

        if(tptr%tv_ob)then
           ts_prime0=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)
        else
           ts_prime0=w1*st(j1)+w2*st(j2)+w3*st(j3)+w4*st(j4)
        end if 
        if (isst==0) then 
           tg_prime0=w1* ssst(j1)+w2*ssst(j2)+w3*ssst(j3)+w4*ssst(j4)
        else 
           tg_prime0=zero
        end if
        qs_prime0=w1*   sq(j1)+w2*  sq(j2)+w3*  sq(j3)+w4*  sq(j4)
        us_prime0=w1*   su(j1)+w2*  su(j2)+w3*  su(j3)+w4*  su(j4)
        vs_prime0=w1*   sv(j1)+w2*  sv(j2)+w3*  sv(j3)+w4*  sv(j4)
        psfc_prime0=w1* sp(j1)+w2*  sp(j2)+w3*  sp(j3)+w4*  sp(j4)
        if ( l_foto ) then
           ts_prime0=ts_prime0+ &
                    (w1*xhat_dt_tsen(j1)+w2*xhat_dt_tsen(j2)+ &
                     w3*xhat_dt_tsen(j3)+w4*xhat_dt_tsen(j4))*time_t
           qs_prime0=qs_prime0+ &
                    (w1*xhat_dt_q(j1)+w2*xhat_dt_q(j2)+ &
                     w3*xhat_dt_q(j3)+w4*xhat_dt_q(j4))*time_t
           us_prime0=us_prime0+&
                    (w1*xhat_dt_u(j1)+w2*xhat_dt_u(j2)+ &
                     w3*xhat_dt_u(j3)+w4*xhat_dt_u(j4))*time_t
           vs_prime0=vs_prime0+&
                    (w1*xhat_dt_v(j1)+w2*xhat_dt_v(j2)+ &
                     w3*xhat_dt_v(j3)+w4*xhat_dt_v(j4))*time_t
           psfc_prime0=psfc_prime0+ &
                    (w1*xhat_dt_prse(j1)+w2*xhat_dt_prse(j2)+ &
                     w3*xhat_dt_prse(j3)+w4*xhat_dt_prse(j4))*time_t
        endif

        val=psfc_prime0*tptr%tlm_tsfc(1) + tg_prime0*tptr%tlm_tsfc(2) + &
            ts_prime0  *tptr%tlm_tsfc(3) + qs_prime0*tptr%tlm_tsfc(4) + &
            us_prime0  *tptr%tlm_tsfc(5) + vs_prime0*tptr%tlm_tsfc(6)
 
     else

!       Forward model (for interpolation)
        if(tptr%tv_ob)then
           val=w1*stv(j1)+w2*stv(j2)+w3*stv(j3)+w4*stv(j4)&
              +w5*stv(j5)+w6*stv(j6)+w7*stv(j7)+w8*stv(j8)
           if ( l_foto ) then
              val=val&
              +(w1*xhat_dt_t(j1)+w2*xhat_dt_t(j2)+ &
                w3*xhat_dt_t(j3)+w4*xhat_dt_t(j4)+ &
                w5*xhat_dt_t(j5)+w6*xhat_dt_t(j6)+ &
                w7*xhat_dt_t(j7)+w8*xhat_dt_t(j8))*time_t
           endif
        else
           val=w1*    st(j1)+w2*    st(j2)+w3*    st(j3)+w4*    st(j4)&
              +w5*    st(j5)+w6*    st(j6)+w7*    st(j7)+w8*    st(j8)
           if ( l_foto ) then
              val=val&
              +(w1*xhat_dt_tsen(j1)+w2*xhat_dt_tsen(j2)+ &
                w3*xhat_dt_tsen(j3)+w4*xhat_dt_tsen(j4)+ &
                w5*xhat_dt_tsen(j5)+w6*xhat_dt_tsen(j6)+ &
                w7*xhat_dt_tsen(j7)+w8*xhat_dt_tsen(j8))*time_t
           endif
        end if

     end if

!    Include contributions from bias correction terms
     if (.not. ladtest_obs .and. (aircraft_t_bc_pof .or. aircraft_t_bc) .and. tptr%idx>0) then
        ix=(tptr%idx-1)*npredt
        do n=1,npredt
           val=val+spred(ix+n)*tptr%pred(n)
        end do
     end if

     if(luse_obsdiag)then
        if (lsaveobsens) then
           grad = val*tptr%raterr2*tptr%err2
           tptr%diags%obssen(jiter) = grad
        else
           if (tptr%luse) tptr%diags%tldepart(jiter)=val
        endif
     endif

!    Do adjoint
     if (l_do_adjoint) then
        if (.not. lsaveobsens) then
           if( .not. ladtest_obs)   val=val-tptr%res
 
!          gradient of nonlinear operator

           if (vqc .and. nlnqc_iter .and. tptr%pg > tiny_r_kind .and.  &
                                tptr%b  > tiny_r_kind) then
              t_pg=tptr%pg*varqc_iter
              cg_t=cg_term/tptr%b
              wnotgross= one-t_pg
              wgross =t_pg*cg_t/wnotgross
              p0=wgross/(wgross+exp(-half*tptr%err2*val**2))
              val=val*(one-p0)                  
           endif
           if (njqc .and. tptr%jb > tiny_r_kind .and. tptr%jb <10.0_r_kind) then
              val=sqrt(two*tptr%jb)*tanh(sqrt(tptr%err2)*val/sqrt(two*tptr%jb))
              grad = val*tptr%raterr2*sqrt(tptr%err2)
           else
              grad = val*tptr%raterr2*tptr%err2
           endif
           if(ladtest_obs) then
              grad = val
           endif
        endif
!       Adjoint of interpolation
!       Extract contributions from bias correction terms
        if (.not. ladtest_obs .and. (aircraft_t_bc_pof .or. aircraft_t_bc) .and. tptr%idx>0) then
           if (tptr%luse) then 
              do n=1,npredt
                 rpred(ix+n)=rpred(ix+n)+tptr%pred(n)*grad
              end do
           end if
        end if

        if(tptr%use_sfc_model) then

!          Surface model
 
           psfc_grad=tptr%tlm_tsfc(1)*grad
           rp(j1)=rp(j1)+w1*psfc_grad
           rp(j2)=rp(j2)+w2*psfc_grad
           rp(j3)=rp(j3)+w3*psfc_grad
           rp(j4)=rp(j4)+w4*psfc_grad
           vs_grad  =tptr%tlm_tsfc(6)*grad
           rv(j1)=rv(j1)+w1*vs_grad
           rv(j2)=rv(j2)+w2*vs_grad
           rv(j3)=rv(j3)+w3*vs_grad
           rv(j4)=rv(j4)+w4*vs_grad
           us_grad  =tptr%tlm_tsfc(5)*grad
           ru(j1)=ru(j1)+w1*us_grad
           ru(j2)=ru(j2)+w2*us_grad
           ru(j3)=ru(j3)+w3*us_grad
           ru(j4)=ru(j4)+w4*us_grad
           qs_grad  =tptr%tlm_tsfc(4)*grad
           rq(j1)=rq(j1)+w1*qs_grad
           rq(j2)=rq(j2)+w2*qs_grad
           rq(j3)=rq(j3)+w3*qs_grad
           rq(j4)=rq(j4)+w4*qs_grad
           if (isst==0) then
              tg_grad  =tptr%tlm_tsfc(2)*grad
              rsst(j1)=rsst(j1)+w1*tg_grad
              rsst(j2)=rsst(j2)+w2*tg_grad
              rsst(j3)=rsst(j3)+w3*tg_grad
              rsst(j4)=rsst(j4)+w4*tg_grad
           end if
           if (l_foto) then
              dhat_dt_prse(j1)=dhat_dt_prse(j1)+w1*psfc_grad*time_t
              dhat_dt_prse(j2)=dhat_dt_prse(j2)+w2*psfc_grad*time_t
              dhat_dt_prse(j3)=dhat_dt_prse(j3)+w3*psfc_grad*time_t
              dhat_dt_prse(j4)=dhat_dt_prse(j4)+w4*psfc_grad*time_t
              dhat_dt_v(j1)=dhat_dt_v(j1)+w1*vs_grad*time_t
              dhat_dt_v(j2)=dhat_dt_v(j2)+w2*vs_grad*time_t
              dhat_dt_v(j3)=dhat_dt_v(j3)+w3*vs_grad*time_t
              dhat_dt_v(j4)=dhat_dt_v(j4)+w4*vs_grad*time_t
              dhat_dt_u(j1)=dhat_dt_u(j1)+w1*us_grad*time_t
              dhat_dt_u(j2)=dhat_dt_u(j2)+w2*us_grad*time_t
              dhat_dt_u(j3)=dhat_dt_u(j3)+w3*us_grad*time_t
              dhat_dt_u(j4)=dhat_dt_u(j4)+w4*us_grad*time_t
              dhat_dt_q(j1)=dhat_dt_q(j1)+w1*qs_grad*time_t
              dhat_dt_q(j2)=dhat_dt_q(j2)+w2*qs_grad*time_t
              dhat_dt_q(j3)=dhat_dt_q(j3)+w3*qs_grad*time_t
              dhat_dt_q(j4)=dhat_dt_q(j4)+w4*qs_grad*time_t
           endif


           ts_grad  =tptr%tlm_tsfc(3)*grad
           if(tptr%tv_ob)then
              rtv(j1)=rtv(j1)+w1*ts_grad
              rtv(j2)=rtv(j2)+w2*ts_grad
              rtv(j3)=rtv(j3)+w3*ts_grad
              rtv(j4)=rtv(j4)+w4*ts_grad

              if (l_foto) then
                 dhat_dt_t(j1)=dhat_dt_t(j1)+w1*ts_grad*time_t
                 dhat_dt_t(j2)=dhat_dt_t(j2)+w2*ts_grad*time_t
                 dhat_dt_t(j3)=dhat_dt_t(j3)+w3*ts_grad*time_t
                 dhat_dt_t(j4)=dhat_dt_t(j4)+w4*ts_grad*time_t
              endif

           else
              rt(j1)=rt(j1)+w1*ts_grad
              rt(j2)=rt(j2)+w2*ts_grad
              rt(j3)=rt(j3)+w3*ts_grad
              rt(j4)=rt(j4)+w4*ts_grad
 
              if (l_foto) then
                 dhat_dt_tsen(j1)=dhat_dt_tsen(j1)+w1*ts_grad*time_t
                 dhat_dt_tsen(j2)=dhat_dt_tsen(j2)+w2*ts_grad*time_t
                 dhat_dt_tsen(j3)=dhat_dt_tsen(j3)+w3*ts_grad*time_t
                 dhat_dt_tsen(j4)=dhat_dt_tsen(j4)+w4*ts_grad*time_t
              endif

           end if

        else

!------bypass surface model--------------------------

           if(tptr%tv_ob)then
              rtv(j1)=rtv(j1)+w1*grad
              rtv(j2)=rtv(j2)+w2*grad
              rtv(j3)=rtv(j3)+w3*grad
              rtv(j4)=rtv(j4)+w4*grad
              rtv(j5)=rtv(j5)+w5*grad
              rtv(j6)=rtv(j6)+w6*grad
              rtv(j7)=rtv(j7)+w7*grad
              rtv(j8)=rtv(j8)+w8*grad
 
              if ( l_foto ) then
                 dhat_dt_t(j1)=dhat_dt_t(j1)+w1*grad*time_t
                 dhat_dt_t(j2)=dhat_dt_t(j2)+w2*grad*time_t
                 dhat_dt_t(j3)=dhat_dt_t(j3)+w3*grad*time_t
                 dhat_dt_t(j4)=dhat_dt_t(j4)+w4*grad*time_t
                 dhat_dt_t(j5)=dhat_dt_t(j5)+w5*grad*time_t
                 dhat_dt_t(j6)=dhat_dt_t(j6)+w6*grad*time_t
                 dhat_dt_t(j7)=dhat_dt_t(j7)+w7*grad*time_t
                 dhat_dt_t(j8)=dhat_dt_t(j8)+w8*grad*time_t
              endif

           else
              rt(j1)=rt(j1)+w1*grad
              rt(j2)=rt(j2)+w2*grad
              rt(j3)=rt(j3)+w3*grad
              rt(j4)=rt(j4)+w4*grad
              rt(j5)=rt(j5)+w5*grad
              rt(j6)=rt(j6)+w6*grad
              rt(j7)=rt(j7)+w7*grad
              rt(j8)=rt(j8)+w8*grad
 
              if ( l_foto ) then
                 dhat_dt_tsen(j1)=dhat_dt_tsen(j1)+w1*grad*time_t
                 dhat_dt_tsen(j2)=dhat_dt_tsen(j2)+w2*grad*time_t
                 dhat_dt_tsen(j3)=dhat_dt_tsen(j3)+w3*grad*time_t
                 dhat_dt_tsen(j4)=dhat_dt_tsen(j4)+w4*grad*time_t
                 dhat_dt_tsen(j5)=dhat_dt_tsen(j5)+w5*grad*time_t
                 dhat_dt_tsen(j6)=dhat_dt_tsen(j6)+w6*grad*time_t
                 dhat_dt_tsen(j7)=dhat_dt_tsen(j7)+w7*grad*time_t
                 dhat_dt_tsen(j8)=dhat_dt_tsen(j8)+w8*grad*time_t
              endif

           end if

        end if

     end if

     tptr => tptr%llpoint
  end do
  return
end subroutine intt_

end module inttmod
