subroutine ensctl2model_ad(eval,mval,grad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensctl2model
!   prgmmr: kleist
!
! abstract:  Contribution from state space to ensemble control vector
!
! program history log:
!   2011-11-17  kleist - initial code
!   2013-10-28  todling - rename p3d to prse
!   Todling: this routine is simply a PLACE HOLDER for now - not working yet
!
!   input argument list:
!     eval - Ensemble state variable variable
!     grad - Control variable
!
!   output argument list:
!     grad - Control variable
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use control_vectors, only: control_vector,cvars3d
use gsi_4dvar, only: ibin_anl
use hybrid_ensemble_parameters, only: uv_hyb_ens,dual_res,nval_lenz_en,ntlevs_ens,n_ens,q_hyb_ens
use hybrid_ensemble_isotropic, only: ensemble_forward_model_ad
use hybrid_ensemble_isotropic, only: ckgcov_a_en_new_factorization_ad
use hybrid_ensemble_isotropic, only: ensemble_forward_model_ad_dual_res
use hybrid_ensemble_isotropic, only: sqrt_beta_s_mult,sqrt_beta_e_mult
use balmod, only: strong_bk_ad
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: assignment(=)
use gsi_bundlemod, only : self_add
use constants, only: zero,max_varname_length
use mpeu_util, only: getindex
use gsi_metguess_mod, only: gsi_metguess_get
use mod_strong, only: tlnmc_option
use timermod, only: timer_ini,timer_fnl
use hybrid_ensemble_parameters,only: naensgrp
implicit none

! Declare passed variables
type(control_vector), intent(inout) :: grad
type(gsi_bundle)    , intent(inout) :: mval
type(gsi_bundle)    , intent(in   ) :: eval(ntlevs_ens)

! Declare local variables
character(len=*),parameter::myname='ensctl2model_ad'
character(len=max_varname_length),allocatable,dimension(:) :: clouds
integer(i_kind) :: ii,jj,ic,id,istatus,nclouds,nn

integer(i_kind), parameter :: ncvars = 5
integer(i_kind) :: icps(ncvars)
type(gsi_bundle):: wbundle_c ! work bundle
type(gsi_bundle),allocatable :: ebundle(:,:)
real(r_kind) :: grade(nval_lenz_en)
character(len=3), parameter :: mycvars(ncvars) = (/  &  ! vars from CV needed here
                               'sf ', 'vp ', 'ps ', 't  ',    &
                               'q  '/)
logical :: lc_sf,lc_vp,lc_ps,lc_t,lc_rh
real(r_kind),pointer,dimension(:,:)   :: cv_ps
real(r_kind),pointer,dimension(:,:,:) :: cv_sf,cv_vp,cv_rh,cv_tv
! Declare required local state variables
integer(i_kind), parameter :: nsvars = 5
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
                               'u   ', 'v   ', 'prse', 'q   ', 'tsen' /)
logical :: ls_u,ls_v,ls_prse,ls_q,ls_tsen
real(r_kind),pointer,dimension(:,:)   :: rv_ps,rv_sst
real(r_kind),pointer,dimension(:,:,:) :: rv_u,rv_v,rv_prse,rv_q,rv_tsen,rv_tv,rv_oz
real(r_kind),pointer,dimension(:,:,:) :: rv_rank3

logical :: do_getuv,do_tv_to_tsen_ad,do_normal_rh_to_q_ad,do_getprs_ad
logical :: do_tlnmc,lstrong_bk_vars,do_q_copy
integer(i_kind) :: ig
!****************************************************************************

! Initialize timer
call timer_ini(trim(myname))

! Inquire about chemistry
call gsi_metguess_get('clouds::3d',nclouds,istatus)
if (nclouds>0) then
    allocate(clouds(nclouds))
    call gsi_metguess_get('clouds::3d',clouds,istatus)
endif

! Since each internal vector of grad has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (grad%step(1),mycvars,icps,istatus)
lc_sf =icps(1)>0; lc_vp =icps(2)>0; lc_ps =icps(3)>0
lc_t  =icps(4)>0; lc_rh =icps(5)>0

! Since each internal vector of grad has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (eval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0

! Define what to do depending on what's in CV and SV
lstrong_bk_vars     =lc_sf.and.lc_vp.and.lc_ps .and.lc_t
do_getuv            =lc_sf.and.lc_vp.and.ls_u  .and.ls_v
do_tv_to_tsen_ad    =lc_t .and.ls_q .and.ls_tsen
do_normal_rh_to_q_ad=lc_t .and.lc_rh.and.ls_prse.and.ls_q.and.(.not.q_hyb_ens)
do_q_copy=.false.
if(.not. do_normal_rh_to_q_ad) then
  do_q_copy = lc_rh.and.lc_t .and.ls_prse.and.ls_q.and.q_hyb_ens
end if
do_getprs_ad        =lc_t .and.lc_ps.and.ls_prse

! Initialize
mval%values=zero

do jj=1,ntlevs_ens

   do_tlnmc = lstrong_bk_vars .and. ( (tlnmc_option==3) .or. &
         (jj==ibin_anl .and. tlnmc_option==2) )

   !allocate(grade(nval_lenz_en))
   allocate(ebundle(naensgrp,n_ens))
   do ig=1,naensgrp
      do nn=1,n_ens
         call gsi_bundlecreate (ebundle(ig,nn),grad%aens(1,1,1),'m2c ensemble work',istatus)
         if(istatus/=0) then
            write(6,*) trim(myname), ': trouble creating work ens-bundle'
            call stop2(999)
         endif
      enddo
   enddo

!  Create a temporary bundle similar to grad, and copy contents of grad into it
   call gsi_bundlecreate ( wbundle_c, grad%step(1), 'stat2ensctl work', istatus )
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble creating work bundle'
      call stop2(999)
   endif
   wbundle_c%values=zero

   call gsi_bundlegetpointer (wbundle_c,'sf' ,cv_sf ,istatus)
   call gsi_bundlegetpointer (wbundle_c,'vp' ,cv_vp ,istatus)
   call gsi_bundlegetpointer (wbundle_c,'q'  ,cv_rh ,istatus)
   call gsi_bundlegetpointer (wbundle_c,'t'  ,cv_tv, istatus)
   call gsi_bundlegetpointer (wbundle_c,'ps' ,cv_ps ,istatus)

! Get sv pointers here
!  Get pointers to required state variables
   call gsi_bundlegetpointer (eval(jj),'u'   ,rv_u,   istatus)
   call gsi_bundlegetpointer (eval(jj),'v'   ,rv_v,   istatus)
   call gsi_bundlegetpointer (eval(jj),'ps'  ,rv_ps,  istatus)
   call gsi_bundlegetpointer (eval(jj),'prse',rv_prse,istatus)
   call gsi_bundlegetpointer (eval(jj),'tv'  ,rv_tv,  istatus)
   call gsi_bundlegetpointer (eval(jj),'tsen',rv_tsen,istatus)
   call gsi_bundlegetpointer (eval(jj),'q'   ,rv_q ,  istatus)
   call gsi_bundlegetpointer (eval(jj),'oz'  ,rv_oz , istatus)
   call gsi_bundlegetpointer (eval(jj),'sst' ,rv_sst, istatus)

!  Adjoint of consistency for sensible temperature, calculate sensible temperature
   if(do_tv_to_tsen_ad) call tv_to_tsen_ad(rv_tv,rv_q,rv_tsen)

!  If calling TLNMC, already have u,v (so set last argument to true)
   if(do_tlnmc) then

!     Adjoint to convert ps to 3-d pressure
      if(do_getprs_ad) call getprs_ad(rv_ps,rv_tv,rv_prse)
      rv_prse=zero

!     Adjoint of strong_bk
      call strong_bk_ad(rv_u,rv_v,rv_ps,rv_tv,.true.)
   end if

   call self_add(mval,eval(jj))

!  Adjoint of control to initial state
   call gsi_bundleputvar ( wbundle_c, 't' ,  rv_tv,  istatus )
   call gsi_bundleputvar ( wbundle_c, 'ps',  rv_ps,  istatus )
   call gsi_bundleputvar ( wbundle_c, 'oz',  rv_oz,  istatus )
   call gsi_bundleputvar ( wbundle_c, 'sst', rv_sst, istatus )

!  Since cloud-vars map one-to-one, take care of them together
   do ic=1,nclouds
      id=getindex(cvars3d,clouds(ic))
      if (id>0) then
          call gsi_bundlegetpointer (eval(jj),clouds(ic),rv_rank3,istatus)
          call gsi_bundleputvar     (wbundle_c, clouds(ic),rv_rank3,istatus)
      endif
   enddo

!  Convert RHS calculations for u,v to st/vp
   if (do_getuv) then
      if(uv_hyb_ens) then
         call gsi_bundleputvar ( wbundle_c, 'sf', rv_u, istatus )
         call gsi_bundleputvar ( wbundle_c, 'vp', rv_v, istatus )
      else
         call getuv(rv_u,rv_v,cv_sf,cv_vp,1)
      end if
   end if

   if(do_q_copy) then
      call gsi_bundleputvar (wbundle_c, 'q', rv_q, istatus )
   else

!     Adjoint of convert input normalized RH to q to add contribution of moisture
!     to t, p , and normalized rh
      if(do_normal_rh_to_q_ad) call normal_rh_to_q_ad(cv_rh,cv_tv,rv_prse,rv_q)

!     Adjoint to convert ps to 3-d pressure
      if(do_getprs_ad) call getprs_ad(cv_ps,cv_tv,rv_prse)
   end if

   do ig=1,naensgrp
      do nn=1,n_ens
         ebundle(ig,nn)%values=grad%aens(jj,ig,nn)%values
      enddo
   enddo
   if(dual_res) then
      call ensemble_forward_model_ad_dual_res(wbundle_c,ebundle,jj)
   else
      call ensemble_forward_model_ad(wbundle_c,ebundle,jj)
   end if
   call sqrt_beta_s_mult(wbundle_c)

!  Apply square-root of ensemble error covariance
   call sqrt_beta_e_mult(ebundle)
   do ig=1,naensgrp
      call ckgcov_a_en_new_factorization_ad(ig,grade,ebundle(ig,:))

      do ii=1,n_ens
         grad%aens(jj,ig,ii)%values=grad%aens(jj,ig,ii)%values + grade
      enddo
   enddo

   do ig=1,naensgrp
      do nn=n_ens,1,-1 ! first in; last out
         call gsi_bundledestroy(ebundle(ig,nn),istatus)
         if(istatus/=0) then
            write(6,*) trim(myname), ': trouble destroying work ens bundle', ig, nn, istatus
            call stop2(999)
         endif
      enddo
   enddo

   call gsi_bundledestroy(wbundle_c,istatus)
   if (istatus/=0) then
      write(6,*) trim(myname),': trouble destroying work bundle'
      call stop2(999)
   endif

   deallocate(ebundle)
!  deallocate(grade)

end do

if (nclouds>0) deallocate(clouds)

! Finalize timer
call timer_fnl(trim(myname))

return 
end subroutine ensctl2model_ad
