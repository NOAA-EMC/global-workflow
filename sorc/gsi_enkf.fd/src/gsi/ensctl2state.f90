!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  ensctl2state_mod --- ensctl2state_mod variables and routines
!
! !INTERFACE:
!
module ensctl2state_mod

! !USES:


! !DESCRIPTION: module ensctl2state routines and variables


use constants, only:  zero,max_varname_length
use kinds, only: r_kind,i_kind
use control_vectors, only: control_vector,cvars3d,e2sset_flg
use gsi_4dvar, only: ibin_anl
use hybrid_ensemble_parameters, only: uv_hyb_ens,dual_res,ntlevs_ens,q_hyb_ens
use hybrid_ensemble_isotropic, only: ensemble_forward_model,ensemble_forward_model_dual_res
use hybrid_ensemble_isotropic, only: ensemble_forward_model_ad,ensemble_forward_model_ad_dual_res
use balmod, only: strong_bk,strong_bk_ad
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
use cwhydromod, only: cw2hydro_tl,cw2hydro_ad
use cwhydromod, only: cw2hydro_tl_hwrf,cw2hydro_ad_hwrf
use timermod, only: timer_ini,timer_fnl
use gridmod, only: nems_nmmb_regional

implicit none

private
public :: ensctl2state,ensctl2state_ad

logical :: ls_u,ls_v,ls_prse,ls_q,ls_tsen,ls_ql,ls_qi
logical :: ls_qr,ls_qs,ls_qg,ls_qh
logical :: ls_w,ls_dw

logical :: lc_sf,lc_vp,lc_ps,lc_t,lc_rh,lc_cw
logical :: lc_w,lc_dw

logical :: do_getuv,do_tv_to_tsen,do_normal_rh_to_q,do_getprs,lstrong_bk_vars
logical :: do_q_copy
logical :: do_cw_to_hydro
logical :: do_cw_to_hydro_hwrf

integer(i_kind) :: nclouds,idozone,istatus


contains
   subroutine ensctl2state(xhat,mval,eval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensctl2state
!   prgmmr: kleist
!   2013-11-22  kleist - add option for q perturbations
!
! abstract:  Converts ensemble control variable to state vector space
!
! program history log:
!   2011-11-17  tremolet - initial code
!   2013-10-28  todling - rename p3d to prse 
!   2013-11-22  kleist - add option for q perturbations
!   2014-12-03  derber   - introduce parallel regions for optimization
!   2017-05-12  Y. Wang and X. Wang - add w as state variable for rw DA, POC: xuguang.wang@ou.edu
!   2019-07-11  Todling - check on w and dw on the fly
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

implicit none

! Declare passed variables
type(control_vector), intent(in   ) :: xhat
type(gsi_bundle)    , intent(in   ) :: mval
type(gsi_bundle)    , intent(inout) :: eval(ntlevs_ens)

! Declare local variables
character(len=*),parameter::myname='ensctl2state'
character(len=max_varname_length),allocatable,dimension(:) :: clouds
integer(i_kind) :: jj,ic,id
logical :: do_tlnmc

type(gsi_bundle):: wbundle_c ! work bundle
real(r_kind),pointer,dimension(:,:,:) :: cv_sf=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vp=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_rh=>NULL()
! Declare required local state variables
real(r_kind),pointer,dimension(:,:)   :: sv_ps=>NULL()
real(r_kind),pointer,dimension(:,:)   :: sv_sst=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_u=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_v=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_prse=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_q=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_tsen=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_tv=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_oz=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_rank3=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_w=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_dw=>NULL()

! ****************************************************************************

! Initialize timer
! call timer_ini(trim(myname))

if(e2sset_flg)call ensctl2state_set(xhat,eval)

! Inquire about cloud-vars
if (nclouds>0) then
    allocate(clouds(nclouds))
    call gsi_metguess_get('clouds::3d',clouds,istatus)
endif


!  Create a temporary bundle similar to xhat, and copy contents of xhat into it
call gsi_bundlecreate ( wbundle_c, xhat%step(1), 'ensctl2state work', istatus )
if(istatus/=0) then
   write(6,*) trim(myname), ': trouble creating work bundle'
   call stop2(999)
endif


! Initialize ensemble contribution to zero
!$omp parallel do schedule(dynamic,1) private(jj)
do jj=1,ntlevs_ens 
   eval(jj)%values=zero
end do

! wbundle_c%values=zero

do jj=1,ntlevs_ens 

   do_tlnmc = lstrong_bk_vars .and. ( (tlnmc_option==3) .or. &
         (jj==ibin_anl .and. tlnmc_option==2) )

!  Initialize work bundle to first component 
!  For 4densvar, this is the "3D/Time-invariant contribution from static B"

   if(dual_res) then
      call ensemble_forward_model_dual_res(wbundle_c,xhat%aens(1,:,:),jj)
   else
      call ensemble_forward_model(wbundle_c,xhat%aens(1,:,:),jj)
   end if

!  Get pointers to required state variables
   call gsi_bundlegetpointer (eval(jj),'ps'  ,sv_ps,  istatus)
   call gsi_bundlegetpointer (eval(jj),'tv'  ,sv_tv,  istatus)
   call gsi_bundlegetpointer (eval(jj),'q'   ,sv_q ,  istatus)
   call gsi_bundlegetpointer (eval(jj),'prse',sv_prse,istatus)
   call gsi_bundlegetpointer (wbundle_c,'q'  ,cv_rh ,istatus)
   call gsi_bundlegetpointer (eval(jj),'u'   ,sv_u,   istatus)
   call gsi_bundlegetpointer (eval(jj),'v'   ,sv_v,   istatus)
   call gsi_bundlegetpointer (eval(jj),'tsen',sv_tsen,istatus)
!$omp parallel sections private(ic,id,istatus)

!$omp section

!  Get pointers to required state variables
!  Convert streamfunction and velocity potential to u,v
   if(do_getuv) then
      if(uv_hyb_ens) then
         call gsi_bundlegetvar ( wbundle_c, 'sf', sv_u, istatus )
         call gsi_bundlegetvar ( wbundle_c, 'vp', sv_v, istatus )
      else
         call gsi_bundlegetpointer (wbundle_c,'sf' ,cv_sf ,istatus)
         call gsi_bundlegetpointer (wbundle_c,'vp' ,cv_vp ,istatus)
         call getuv(sv_u,sv_v,cv_sf,cv_vp,0)
      end if
   end if

!$omp section

!  Copy variables
   call gsi_bundlegetvar ( wbundle_c, 't'  , sv_tv,  istatus )
   call gsi_bundlegetvar ( wbundle_c, 'ps' , sv_ps,  istatus )
!  Get 3d pressure
   if(do_q_copy) then
      call gsi_bundlegetvar ( wbundle_c, 'q', sv_q, istatus )
   else
      if(do_getprs) call getprs_tl(sv_ps,sv_tv,sv_prse)

!  Convert RH to Q
      if(do_normal_rh_to_q) then
         call normal_rh_to_q(cv_rh,sv_tv,sv_prse,sv_q)
      end if

   end if

   if (do_cw_to_hydro .and. .not.do_cw_to_hydro_hwrf) then
!     Case when cloud-vars do not map one-to-one (cv-to-sv)
!     e.g. cw-to-ql&qi
      call cw2hydro_tl(eval(jj),wbundle_c,clouds,nclouds)
   elseif (do_cw_to_hydro_hwrf) then
!     Case when cloud-vars do not map one-to-one (cv-to-sv)
!     e.g. cw-to-ql&qi&qr&qs&qg&qh
      if (.not.do_tv_to_tsen) then
        call tv_to_tsen(sv_tv,sv_q,sv_tsen)
      endif
      call cw2hydro_tl_hwrf(eval(jj),wbundle_c,sv_tsen)
   else
!  Since cloud-vars map one-to-one, take care of them together
      do ic=1,nclouds
         id=getindex(cvars3d,clouds(ic))
         if (id>0) then
            call gsi_bundlegetpointer (eval(jj),clouds(ic),sv_rank3,istatus)
            call gsi_bundlegetvar     (wbundle_c, clouds(ic),sv_rank3,istatus)
         endif
      enddo
   endif


!$omp section

!  Get pointers to required state variables and copy
   call gsi_bundlegetpointer (eval(jj),'sst' ,sv_sst, istatus)
   call gsi_bundlegetvar ( wbundle_c, 'sst', sv_sst, istatus )
   if(ls_w .and. lc_w)then
     call gsi_bundlegetpointer (eval(jj),'w' ,sv_w, istatus)
     call gsi_bundlegetvar ( wbundle_c, 'w' , sv_w,  istatus )
     if(ls_dw .and. lc_dw)then
        call gsi_bundlegetpointer (eval(jj),'dw' ,sv_dw, istatus)
        call gsi_bundlegetvar ( wbundle_c, 'dw' , sv_dw,  istatus )
     end if
   end if

!  Get the ozone vector if it is defined
   if(idozone > 0) then
      call gsi_bundlegetpointer (eval(jj),'oz'  ,sv_oz , istatus)
      call gsi_bundlegetvar ( wbundle_c, 'oz' , sv_oz,  istatus )
   endif

!$omp end parallel sections

! Add contribution from static B, if necessary
   call self_add(eval(jj),mval)

! Call strong constraint if necessary
   if(do_tlnmc) then

      call strong_bk(sv_u,sv_v,sv_ps,sv_tv,.true.)

!  Need to update 3d pressure and sensible temperature again for consistency
!  Get 3d pressure
      if(do_getprs) call getprs_tl(sv_ps,sv_tv,sv_prse)
  
   end if

!  Calculate sensible temperature 
   if(do_tv_to_tsen) call tv_to_tsen(sv_tv,sv_q,sv_tsen)

end do  ! ntlevs

call gsi_bundledestroy(wbundle_c,istatus)
if(istatus/=0) then
   write(6,*) trim(myname), ': trouble destroying work bundle'
   call stop2(999)
endif

if (nclouds>0) deallocate(clouds)

! Finalize timer
! call timer_fnl(trim(myname))

return 
end subroutine ensctl2state

subroutine ensctl2state_set(xhat,eval)
!                .      .    .                                       .
! subprogram:    ensctl2state_set
!   prgmmr: derber
!
! abstract:  Sets flags for ensctl2state and ensctl2state_ad
!
! program history log:
!   2022-08-30  derber   - initial code from control2state

!   input argument list:
!     xhat - Control variable
!     sval - State variable
!
!$$$ end documentation block

implicit none

type(control_vector), intent(in) :: xhat
type(gsi_bundle)    , intent(in) :: eval(ntlevs_ens)

integer(i_kind), parameter :: nsvars = 13
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
             'u   ', 'v   ', 'prse', 'q   ', 'tsen', 'ql  ','qi  ', &
             'qr  ', 'qs  ', 'qg  ', 'qh  ', 'w   ', 'dw  ' /)
integer(i_kind), parameter :: ncvars = 8
integer(i_kind) :: icps(ncvars)
character(len=3), parameter :: mycvars(ncvars) = (/  &  ! vars from CV needed here
                               'sf ', 'vp ', 'ps ', 't  ',    &
                               'q  ', 'cw ', 'w  ', 'dw '/)
! Inquire about cloud-vars
call gsi_metguess_get('clouds::3d',nclouds,istatus)

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (xhat%step(1),mycvars,icps,istatus)
lc_sf =icps(1)>0; lc_vp =icps(2)>0; lc_ps =icps(3)>0
lc_t  =icps(4)>0; lc_rh =icps(5)>0; lc_cw =icps(6)>0
lc_w  =icps(7)>0; lc_dw =icps(8)>0

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (eval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_ql =isps(6)>0; ls_qi =isps(7)>0
ls_qr  =isps(8)>0; ls_qs  =isps(9)>0
ls_qg  =isps(10)>0; ls_qh =isps(11)>0
ls_w   =isps(12)>0; ls_dw =isps(13)>0.and.nems_nmmb_regional

! Define what to do depending on what's in CV and SV
lstrong_bk_vars  =lc_ps.and.lc_sf.and.lc_vp.and.lc_t
do_getprs     =lc_ps.and.lc_t .and.ls_prse
do_normal_rh_to_q=(.not.q_hyb_ens).and.&
                  lc_rh.and.lc_t .and.ls_prse.and.ls_q
if(.not. do_normal_rh_to_q) then
  do_q_copy = lc_rh.and.lc_t .and.ls_prse.and.ls_q.and.q_hyb_ens
else
  do_q_copy=.false.
end if
do_tv_to_tsen    =lc_t .and.ls_q .and.ls_tsen
do_getuv         =lc_sf.and.lc_vp.and.ls_u.and.ls_v

do_cw_to_hydro = lc_cw .and. ls_ql .and. ls_qi
do_cw_to_hydro_hwrf = lc_cw.and.ls_ql.and.ls_qi.and.ls_qr.and.ls_qs.and.ls_qg.and.ls_qh


idozone=getindex(cvars3d,"oz")

e2sset_flg=.false.  ! set to true in setup.  set to false after first (only) call to ensctl2state_set


return
end subroutine ensctl2state_set
subroutine ensctl2state_ad(eval,mval,grad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    ensctl2state_ad
!   prgmmr: kleist
!
! abstract:  Contribution from state space to ensemble control vector
!
! program history log:
!   2011-11-17  kleist - initial code
!   2013-10-28  todling - rename p3d to prse
!   2013-11-22  kleist - add option for q perturbations
!   2014-12-03  derber   - introduce parallel regions for optimization
!   2017-05-12  Y. Wang and X. Wang - add w as state variable for rw DA, POC: xuguang.wang@ou.edu
!   2019-07-11  Todling - there should be no need to check on the existence of w and dw
!
!   input argument list:
!     eval - Ensemble state variable variable
!     grad - Control variable
!
!   output argument list:
!     grad - Control variable
!
!$$$ end documentation block

implicit none

! Declare passed variables
type(control_vector), intent(inout) :: grad
type(gsi_bundle)    , intent(inout) :: mval
type(gsi_bundle)    , intent(in   ) :: eval(ntlevs_ens)

! Declare local variables
character(len=*),parameter::myname='ensctl2state_ad'
integer(i_kind) :: jj,ic,id
logical :: do_tlnmc

character(len=max_varname_length),allocatable,dimension(:) :: clouds
type(gsi_bundle):: wbundle_c ! work bundle
real(r_kind),pointer,dimension(:,:,:) :: cv_sf=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vp=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_rh=>NULL()
! Declare required local state variables
real(r_kind),pointer,dimension(:,:)   :: rv_ps=>NULL()
real(r_kind),pointer,dimension(:,:)   :: rv_sst=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_u=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_v=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_prse=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_q=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_tsen=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_tv=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_oz=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_rank3=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_w=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_dw=>NULL()

!****************************************************************************

! Initialize timer
!call timer_ini(trim(myname))

! Inquire about chemistry
if(e2sset_flg)call ensctl2state_set(grad,eval)
if (nclouds>0) then
    allocate(clouds(nclouds))
    call gsi_metguess_get('clouds::3d',clouds,istatus)
endif

! Initialize
mval%values=zero
!  Create a temporary bundle similar to grad, and copy contents of grad into it
call gsi_bundlecreate ( wbundle_c, grad%step(1), 'ensctl2state_ad work', istatus )
if(istatus/=0) then
   write(6,*) trim(myname), ': trouble creating work bundle'
   call stop2(999)
endif

do jj=1,ntlevs_ens

! If calling TLNMC, already have u,v (so set last argument to true)
   do_tlnmc = lstrong_bk_vars .and. ( (tlnmc_option==3) .or. &
            (jj==ibin_anl .and. tlnmc_option==2))  

   wbundle_c%values=zero

! Get sv pointers here
!  Get pointers to required state variables
   call gsi_bundlegetpointer (eval(jj),'u'   ,rv_u,   istatus)
   call gsi_bundlegetpointer (eval(jj),'v'   ,rv_v,   istatus)
   call gsi_bundlegetpointer (eval(jj),'ps'  ,rv_ps,  istatus)
   call gsi_bundlegetpointer (eval(jj),'prse',rv_prse,istatus)
   call gsi_bundlegetpointer (eval(jj),'tv'  ,rv_tv,  istatus)
   call gsi_bundlegetpointer (eval(jj),'tsen',rv_tsen,istatus)
   call gsi_bundlegetpointer (eval(jj),'q'   ,rv_q ,  istatus)
   call gsi_bundlegetpointer (wbundle_c,'q'  ,cv_rh ,istatus)

!  Adjoint of consistency for sensible temperature, calculate sensible temperature
   if(do_tv_to_tsen) call tv_to_tsen_ad(rv_tv,rv_q,rv_tsen)

   if(do_tlnmc) then

      ! Adjoint to convert ps to 3-d pressure
      if(do_getprs) call getprs_ad(rv_ps,rv_tv,rv_prse)
      rv_prse=zero

      ! Adjoint of strong_bk
      call strong_bk_ad(rv_u,rv_v,rv_ps,rv_tv,.true.)

   end if

   call self_add(mval,eval(jj))

!$omp parallel sections private(ic,id,istatus)

!$omp section

!  Convert RHS calculations for u,v to st/vp
   if (do_getuv) then
      if(uv_hyb_ens) then
         call gsi_bundleputvar ( wbundle_c, 'sf', rv_u, istatus )
         call gsi_bundleputvar ( wbundle_c, 'vp', rv_v, istatus )
      else
         call gsi_bundlegetpointer (wbundle_c,'sf' ,cv_sf ,istatus)
         call gsi_bundlegetpointer (wbundle_c,'vp' ,cv_vp ,istatus)
         call getuv(rv_u,rv_v,cv_sf,cv_vp,1)
      end if
   end if

!$omp section

   call gsi_bundlegetpointer (eval(jj),'sst' ,rv_sst, istatus)
   call gsi_bundleputvar ( wbundle_c, 'sst', rv_sst, istatus )
   if(lc_w .and. ls_w)then
     call gsi_bundlegetpointer (eval(jj),'w' ,rv_w, istatus)
     call gsi_bundleputvar ( wbundle_c, 'w', rv_w, istatus )
     if(ls_dw .and. lc_dw)then
       call gsi_bundlegetpointer (eval(jj),'dw' ,rv_dw, istatus)
       call gsi_bundleputvar ( wbundle_c, 'dw', rv_dw, istatus )
     end if
   end if

!  Get the ozone vector if it is defined
   if(idozone > 0) then
      call gsi_bundlegetpointer (eval(jj),'oz'  ,rv_oz , istatus)
      call gsi_bundleputvar ( wbundle_c, 'oz',  rv_oz,  istatus )
   endif

!$omp section

   if (do_cw_to_hydro .and. .not.do_cw_to_hydro_hwrf) then
!     Case when cloud-vars do not map one-to-one
!     e.g. cw-to-ql&qi
      call cw2hydro_ad(eval(jj),wbundle_c,clouds,nclouds)
   elseif (do_cw_to_hydro_hwrf) then
!!     Case when cloud-vars do not map one-to-one
!!     e.g. cw-to-ql&qi&qr&qs&qg&qh
      call cw2hydro_ad_hwrf(eval(jj),wbundle_c,rv_tsen)
   else
!  Since cloud-vars map one-to-one, take care of them together
      do ic=1,nclouds
         id=getindex(cvars3d,clouds(ic))
         if (id>0) then
            call gsi_bundlegetpointer (eval(jj),clouds(ic),rv_rank3,istatus)
            call gsi_bundleputvar     (wbundle_c, clouds(ic),rv_rank3,istatus)
         endif
      enddo
   endif

!  Calculate sensible temperature
   if(do_q_copy) then
      call gsi_bundleputvar (wbundle_c, 'q', rv_q, istatus )
   else

!     Adjoint of convert input normalized RH to q to add contribution of moisture
!     to t, p , and normalized rh
      if(do_normal_rh_to_q) call normal_rh_to_q_ad(cv_rh,rv_tv,rv_prse,rv_q)

!     Adjoint to convert ps to 3-d pressure
      if(do_getprs) call getprs_ad(rv_ps,rv_tv,rv_prse)
   end if

!  Adjoint of control to initial state
   call gsi_bundleputvar ( wbundle_c, 't' ,  rv_tv,  istatus )
   call gsi_bundleputvar ( wbundle_c, 'ps',  rv_ps,  istatus )
!  call gsi_bundleputvar ( wbundle_c, 'q' ,  zero,   istatus )  !mjk                    
!$omp end parallel sections

   if(dual_res) then
      call ensemble_forward_model_ad_dual_res(wbundle_c,grad%aens(1,:,:),jj)
   else
      call ensemble_forward_model_ad(wbundle_c,grad%aens(1,:,:),jj)
   end if

end do

call gsi_bundledestroy(wbundle_c,istatus)
if (istatus/=0) then
   write(6,*) trim(myname),': trouble destroying work bundle'
   call stop2(999)
endif

if (nclouds>0) deallocate(clouds)

! Finalize timer
!call timer_fnl(trim(myname))

return 
end subroutine ensctl2state_ad
end module ensctl2state_mod
