subroutine ensctl2model(xhat,mval,eval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensctl2model
!   prgmmr: kleist
!
! abstract:  Converts ensemble control variable to model space
!
! program history log:
!   2011-11-17  tremolet - initial code
!   2013-10-28  todling - rename p3d to prse 
!   Todling: this routine is simply a PLACE HOLDER for now - not working yet
!
!   input argument list:
!     xhat - Control variable
!     mval - contribution from static B component
!     eval - Ensemble contribution to state vector
!
!   output argument list:
!     eval - Ensemble contribution to state vector
!
!$$$ end documentation block

use constants, only:  zero,max_varname_length
use kinds, only: r_kind,i_kind
use control_vectors, only: control_vector,cvars3d
use gsi_4dvar, only: ibin_anl
use hybrid_ensemble_parameters, only: uv_hyb_ens,dual_res,ntlevs_ens
use hybrid_ensemble_parameters, only: n_ens,q_hyb_ens
use hybrid_ensemble_isotropic, only: ensemble_forward_model,ensemble_forward_model_dual_res
use hybrid_ensemble_isotropic, only: sqrt_beta_s_mult,sqrt_beta_e_mult, &
        ckgcov_a_en_new_factorization
use balmod, only: strong_bk
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: self_add
use gsi_bundlemod, only: assignment(=)
use mpeu_util, only: getindex
use gsi_metguess_mod, only: gsi_metguess_get
use mod_strong, only: tlnmc_option
use timermod, only: timer_ini,timer_fnl
use hybrid_ensemble_parameters,only: naensgrp
implicit none

! Declare passed variables
type(control_vector), intent(in   ) :: xhat
type(gsi_bundle)    , intent(in   ) :: mval
type(gsi_bundle)    , intent(inout) :: eval(ntlevs_ens)

! Declare local variables
character(len=*),parameter::myname='ensctl2model'
character(len=max_varname_length),allocatable,dimension(:) :: clouds
integer(i_kind) :: jj,ic,id,istatus,nclouds,nn

integer(i_kind), parameter :: ncvars = 5
integer(i_kind) :: icps(ncvars)
type(gsi_bundle):: wbundle_c ! work bundle
type(gsi_bundle),allocatable :: ebundle(:,:)
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
real(r_kind),pointer,dimension(:,:)   :: sv_ps,sv_sst
real(r_kind),pointer,dimension(:,:,:) :: sv_u,sv_v,sv_prse,sv_q,sv_tsen,sv_tv,sv_oz
real(r_kind),pointer,dimension(:,:,:) :: sv_rank3

logical :: do_getprs_tl,do_normal_rh_to_q,do_tv_to_tsen,do_getuv,lstrong_bk_vars
logical :: do_tlnmc,do_q_copy
integer(i_kind) :: ig
! ****************************************************************************

! Initialize timer
call timer_ini(trim(myname))

! Inquire about cloud-vars
call gsi_metguess_get('clouds::3d',nclouds,istatus)
if (nclouds>0) then
    allocate(clouds(nclouds))
    call gsi_metguess_get('clouds::3d',clouds,istatus)
endif

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (xhat%step(1),mycvars,icps,istatus)
lc_sf =icps(1)>0; lc_vp =icps(2)>0; lc_ps =icps(3)>0
lc_t  =icps(4)>0; lc_rh =icps(5)>0

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (eval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0

! Define what to do depending on what's in CV and SV
lstrong_bk_vars  =lc_ps.and.lc_sf.and.lc_vp.and.lc_t
do_getprs_tl     =lc_ps.and.lc_t .and.ls_prse
do_normal_rh_to_q=lc_rh.and.lc_t .and.ls_prse.and.ls_q.and.(.not. q_hyb_ens)
do_q_copy=.false.
if(.not. do_normal_rh_to_q) then
  do_q_copy = lc_rh.and.lc_t .and.ls_prse.and.ls_q.and.q_hyb_ens
end if
do_tv_to_tsen    =lc_t .and.ls_q .and.ls_tsen
do_getuv         =lc_sf.and.lc_vp.and.ls_u.and.ls_v

do jj=1,ntlevs_ens 

   do_tlnmc = lstrong_bk_vars .and. ( (tlnmc_option==3) .or. &
         (jj==ibin_anl .and. tlnmc_option==2) )

   allocate(ebundle(naensgrp,n_ens))
   do ig=1,naensgrp
      do nn=1,n_ens
         call gsi_bundlecreate (ebundle(ig,nn),xhat%aens(1,1,1),'c2m ensemble work',istatus)
         if(istatus/=0) then
            write(6,*) trim(myname), ': trouble creating work ens-bundle'
            call stop2(999)
         endif
      enddo
   enddo

!  Apply square-root of ensemble error covariance
   do ig=1,naensgrp
      call ckgcov_a_en_new_factorization(ig,xhat%aens(jj,ig,1)%values(:),ebundle(ig,:))
   enddo
   call sqrt_beta_e_mult(ebundle)

! Initialize ensemble contribution to zero
   eval(jj)%values=zero

!  Create a temporary bundle similar to xhat, and copy contents of xhat into it
   call gsi_bundlecreate ( wbundle_c, xhat%step(1), 'ensctl2model work', istatus )
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble creating work bundle'
      call stop2(999)
   endif

!  Initialize work bundle to first component 
!  For 4densvar, this is the "3D/Time-invariant contribution from static B"
   wbundle_c%values=zero

   call gsi_bundlegetpointer (wbundle_c,'sf' ,cv_sf ,istatus)
   call gsi_bundlegetpointer (wbundle_c,'vp' ,cv_vp ,istatus)
   call gsi_bundlegetpointer (wbundle_c,'q'  ,cv_rh ,istatus)
   call gsi_bundlegetpointer (wbundle_c,'t'  ,cv_tv, istatus)
   call gsi_bundlegetpointer (wbundle_c,'ps' ,cv_ps ,istatus)

! Get sv pointers here
!  Get pointers to required state variables
   call gsi_bundlegetpointer (eval(jj),'u'   ,sv_u,   istatus)
   call gsi_bundlegetpointer (eval(jj),'v'   ,sv_v,   istatus)
   call gsi_bundlegetpointer (eval(jj),'ps'  ,sv_ps,  istatus)
   call gsi_bundlegetpointer (eval(jj),'prse',sv_prse,istatus)
   call gsi_bundlegetpointer (eval(jj),'tv'  ,sv_tv,  istatus)
   call gsi_bundlegetpointer (eval(jj),'tsen',sv_tsen,istatus)
   call gsi_bundlegetpointer (eval(jj),'q'   ,sv_q ,  istatus)
   call gsi_bundlegetpointer (eval(jj),'oz'  ,sv_oz , istatus)
   call gsi_bundlegetpointer (eval(jj),'sst' ,sv_sst, istatus)

   call sqrt_beta_s_mult(wbundle_c)
   if(dual_res) then
      call ensemble_forward_model_dual_res(wbundle_c,ebundle,jj)
   else
      call ensemble_forward_model(wbundle_c,ebundle,jj)
   end if

   sv_q=zero
   if(do_q_copy) then
      call gsi_bundlegetvar ( wbundle_c, 'q', sv_q, istatus )
   else
!     Get 3d pressure
      if(do_getprs_tl) call getprs_tl(cv_ps,cv_tv,sv_prse)

!     Convert input normalized RH to q
      if(do_normal_rh_to_q) then
         call normal_rh_to_q(cv_rh,cv_tv,sv_prse,sv_q)
      end if
   end if

!  Convert streamfunction and velocity potential to u,v
   if(do_getuv) then
      if(uv_hyb_ens) then
         call gsi_bundlegetvar ( wbundle_c, 'sf', sv_u, istatus )
         call gsi_bundlegetvar ( wbundle_c, 'vp', sv_v, istatus )
      else
         call getuv(sv_u,sv_v,cv_sf,cv_vp,0)
      end if
   end if

!  Copy variables
   call gsi_bundlegetvar ( wbundle_c, 't'  , sv_tv,  istatus )
   call gsi_bundlegetvar ( wbundle_c, 'oz' , sv_oz,  istatus )
   call gsi_bundlegetvar ( wbundle_c, 'ps' , sv_ps,  istatus )
   call gsi_bundlegetvar ( wbundle_c, 'sst', sv_sst, istatus )

!  Since cloud-vars map one-to-one, take care of them together
   do ic=1,nclouds
      id=getindex(cvars3d,clouds(ic))
      if (id>0) then
          call gsi_bundlegetpointer (eval(jj),clouds(ic),sv_rank3,istatus)
          call gsi_bundlegetvar     (wbundle_c, clouds(ic),sv_rank3,istatus)
      endif
   enddo

!  Add contribution from static B, if necessary
   call self_add(eval(jj),mval)

!  Call strong constraint if necessary
   if(do_tlnmc) then

      call strong_bk(sv_u,sv_v,sv_ps,sv_tv,.true.)

!     Need to update 3d pressure and sensible temperature again for consistency
!     Get 3d pressure
      if(do_getprs_tl) call getprs_tl(sv_ps,sv_tv,sv_prse)
  
   end if

!  Calculate sensible temperature 
   if(do_tv_to_tsen) call tv_to_tsen(sv_tv,sv_q,sv_tsen)

   call gsi_bundledestroy(wbundle_c,istatus)
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble destroying work bundle'
      call stop2(999)
   endif

   do ig=1,naensgrp
      do nn=n_ens,1,-1 ! first in; last out
         call gsi_bundledestroy(ebundle(ig,nn),istatus)
         if(istatus/=0) then
            write(6,*) trim(myname), ': trouble destroying work ens bundle, ', istatus
            call stop2(999)
         endif
      enddo
   enddo
   deallocate(ebundle)

end do  ! ntlevs

if (nclouds>0) deallocate(clouds)

! Finalize timer
call timer_fnl(trim(myname))

return 
end subroutine ensctl2model
