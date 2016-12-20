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
use gsi_4dvar, only: l4dvar,l4densvar,nobs_bins,ibin_anl
use hybrid_ensemble_parameters, only: uv_hyb_ens,dual_res,ntlevs_ens,q_hyb_ens
use hybrid_ensemble_isotropic, only: ensemble_forward_model,ensemble_forward_model_dual_res
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
use gridmod, only: regional,lat2,lon2,nsig
use cwhydromod, only: cw2hydro_tl
use timermod, only: timer_ini,timer_fnl
implicit none

! Declare passed variables
type(control_vector), intent(in   ) :: xhat
type(gsi_bundle)    , intent(in   ) :: mval
type(gsi_bundle)    , intent(inout) :: eval(ntlevs_ens)

! Declare local variables
character(len=*),parameter::myname='ensctl2state'
character(len=max_varname_length),allocatable,dimension(:) :: clouds
integer(i_kind) :: jj,ic,id,istatus,nclouds

integer(i_kind), parameter :: ncvars = 6
integer(i_kind) :: icps(ncvars)
type(gsi_bundle):: wbundle_c ! work bundle
character(len=3), parameter :: mycvars(ncvars) = (/  &  ! vars from CV needed here
                               'sf ', 'vp ', 'ps ', 't  ',    &
                               'q  ', 'cw '/)
logical :: lc_sf,lc_vp,lc_ps,lc_t,lc_rh,lc_cw
real(r_kind),pointer,dimension(:,:,:) :: cv_sf,cv_vp,cv_rh
! Declare required local state variables
integer(i_kind), parameter :: nsvars = 7
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
             'u   ', 'v   ', 'prse', 'q   ', 'tsen', 'ql  ','qi  ' /)
logical :: ls_u,ls_v,ls_prse,ls_q,ls_tsen,ls_ql,ls_qi
real(r_kind),pointer,dimension(:,:)   :: sv_ps,sv_sst
real(r_kind),pointer,dimension(:,:,:) :: sv_u,sv_v,sv_prse,sv_q,sv_tsen,sv_tv,sv_oz
real(r_kind),pointer,dimension(:,:,:) :: sv_rank3

logical :: do_getprs_tl,do_normal_rh_to_q,do_tv_to_tsen,do_getuv,lstrong_bk_vars
logical :: do_tlnmc,do_q_copy
logical :: do_cw_to_hydro

! ****************************************************************************

! Initialize timer
! call timer_ini(trim(myname))

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
lc_t  =icps(4)>0; lc_rh =icps(5)>0; lc_cw =icps(6)>0

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (eval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_ql =isps(6)>0; ls_qi =isps(7)>0

! Define what to do depending on what's in CV and SV
lstrong_bk_vars  =lc_ps.and.lc_sf.and.lc_vp.and.lc_t
do_getprs_tl     =lc_ps.and.lc_t .and.ls_prse
do_normal_rh_to_q=(.not.q_hyb_ens).and.&
                  lc_rh.and.lc_t .and.ls_prse.and.ls_q
do_q_copy=.false.
if(.not. do_normal_rh_to_q) then
  do_q_copy = lc_rh.and.lc_t .and.ls_prse.and.ls_q.and.q_hyb_ens
end if
do_tv_to_tsen    =lc_t .and.ls_q .and.ls_tsen
do_getuv         =lc_sf.and.lc_vp.and.ls_u.and.ls_v
!  Create a temporary bundle similar to xhat, and copy contents of xhat into it
call gsi_bundlecreate ( wbundle_c, xhat%step(1), 'ensctl2state work', istatus )
if(istatus/=0) then
   write(6,*) trim(myname), ': trouble creating work bundle'
   call stop2(999)
endif

do_cw_to_hydro = .false.
do_cw_to_hydro = lc_cw .and. ls_ql .and. ls_qi

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
      call ensemble_forward_model_dual_res(wbundle_c,xhat%aens(1,:),jj)
   else
      call ensemble_forward_model(wbundle_c,xhat%aens(1,:),jj)
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
      if(do_getprs_tl) call getprs_tl(sv_ps,sv_tv,sv_prse)

!  Convert RH to Q
      if(do_normal_rh_to_q) then
         call normal_rh_to_q(cv_rh,sv_tv,sv_prse,sv_q)
      end if

   end if

   if (do_cw_to_hydro) then
!     Case when cloud-vars do not map one-to-one (cv-to-sv)
!     e.g. cw-to-ql&qi
      call cw2hydro_tl(eval(jj),wbundle_c,clouds,nclouds)
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

!  Get pointers to required state variables
   call gsi_bundlegetpointer (eval(jj),'oz'  ,sv_oz , istatus)
   call gsi_bundlegetpointer (eval(jj),'sst' ,sv_sst, istatus)
!  Copy variables
   call gsi_bundlegetvar ( wbundle_c, 'oz' , sv_oz,  istatus )
   call gsi_bundlegetvar ( wbundle_c, 'sst', sv_sst, istatus )

!$omp end parallel sections

! Add contribution from static B, if necessary
   call self_add(eval(jj),mval)

! Call strong constraint if necessary
   if(do_tlnmc) then

      call strong_bk(sv_u,sv_v,sv_ps,sv_tv,.true.)

!  Need to update 3d pressure and sensible temperature again for consistency
!  Get 3d pressure
      if(do_getprs_tl) call getprs_tl(sv_ps,sv_tv,sv_prse)
  
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
