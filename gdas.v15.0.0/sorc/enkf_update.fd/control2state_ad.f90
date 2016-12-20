subroutine control2state_ad(rval,bval,grad)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    control2state_ad
!   prgmmr: tremolet
!
! abstract:  Converts variables from physical space to control space
!            This is also the adjoint of control2state
!
! program history log:
!   2007-04-16  tremolet - initial code
!   2008-11-28  todling  - update to GSI May 2008: add tsen and p3d
!   2009-01-15  todling  - handle predictors in quad precision
!   2009-04-21  derber   - modify call to getstvp to call to getuv
!   2009-06-15  parrish  - add call to strong_bk_ad when l_hyb_ens=.true. (hybrid ensemble run)
!   2009-08-12  lueken   - update documentation
!   2009-11-27  parrish  - for uv_hyb_ens=.true., then ensemble perturbations contain u,v instead of st,vp
!                            so introduce extra code to handle this case.
!   2010-02-20  parrish  - introduce modifications to allow dual resolution capability when running
!                            in hybrid ensemble mode.
!   2010-03-24  zhu      - use cstate for generalizing control variable
!   2010-04-29  todling  - update to use gsi_bundle; rename cstate to wbundle
!   2010-05-31  todling  - better consistency checks; add co/co2
!                        - ready to bypass analysis of (any) meteorological fields
!   2010-06-15  todling  - generalized handling of chemistry
!   2011-02-22  zhu      - add gust,vis,pblh
!   2011-05-15  auligne/todling - generalized cloud handling
!   2011-07-12  zhu      - add do_cw_to_hydro_ad and cw2hydro_ad
!   2011-11-01  eliu     - generalize the use of do_cw_to_hydro_ad
!   2012-02-08  kleist   - remove strong_bk_ad and ensemble_forward_model_ad and related parameters
!   2013-05-23  zhu      - add ntclen and predt for aircraft temperature bias correction
!   2013-10-25  todling  - nullify work pointers
!   2013-10-28  todling  - rename p3d to prse
!   2014-01-31  mkim     - add support for when ql and qi are CVs for all-sky mw radiance DA
!   2014-03-19  pondeca  - add wspd10m
!   2014-04-10  pondeca  - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca  - add howv
!   2014-06-16  carley/zhu - add tcamt and lcbas
!   2014-12-03  derber   - introduce parallel regions for optimization
!   2015-07-10  pondeca  - add cloud ceiling height (cldch)
!
!   input argument list:
!     rval - State variable
!     bval
!   output argument list:
!     grad - Control variable
!
!$$$
use kinds, only: i_kind,r_kind
use control_vectors, only: control_vector
use control_vectors, only: cvars3d,cvars2d
use bias_predictors, only: predictors
use gsi_4dvar, only: nsubwin, lsqrtb
use gridmod, only: latlon1n,latlon11,regional,lat2,lon2,nsig,twodvar_regional
use jfunc, only: nsclen,npclen,ntclen
use cwhydromod, only: cw2hydro_ad
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_chemguess_mod, only: gsi_chemguess_get
use gsi_metguess_mod, only: gsi_metguess_get
use mpeu_util, only: getindex
use constants, only: max_varname_length,zero

implicit none

! Declare passed variables
type(gsi_bundle)    , intent(inout) :: rval(nsubwin)
type(predictors)    , intent(in   ) :: bval
type(control_vector), intent(inout) :: grad

! Declare local variables
character(len=*),parameter::myname='control2state_ad'
character(len=max_varname_length),allocatable,dimension(:) :: gases
character(len=max_varname_length),allocatable,dimension(:) :: clouds
integer(i_kind) :: ii,jj,ic,id,ngases,nclouds,istatus,istatus_oz 
type(gsi_bundle) :: wbundle ! work bundle

! Note: The following does not aim to get all variables in
!       the state and control vectors, but rather the ones
!       this routines knows how to handle.
integer(i_kind), parameter :: ncvars = 8
integer(i_kind) :: icps(ncvars)
integer(i_kind) :: icpblh,icgust,icvis,icoz,icwspd10m
integer(i_kind) :: ictd2m,icmxtm,icmitm,icpmsl,ichowv
integer(i_kind) :: ictcamt,iclcbas,icsfwter,icvpwter
integer(i_kind) :: iccldch
character(len=3), parameter :: mycvars(ncvars) = (/  &
                               'sf ', 'vp ', 'ps ', 't  ', 'q  ','cw ', 'ql ', 'qi '/)
logical :: lc_sf,lc_vp,lc_ps,lc_t,lc_rh,lc_cw,lc_ql,lc_qi
real(r_kind),pointer,dimension(:,:)   :: cv_ps=>NULL()
real(r_kind),pointer,dimension(:,:)   :: cv_vis=>NULL()
real(r_kind),pointer,dimension(:,:)   :: cv_lcbas=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_sf=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vp=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_t=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_rh=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_sfwter=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vpwter=>NULL()
real(r_kind),pointer,dimension(:,:)   :: cv_cldch=>NULL()

! Declare required local state variables
integer(i_kind), parameter :: nsvars = 7
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
                               'u   ', 'v   ', 'prse', 'q   ', 'tsen', 'ql  ', 'qi  ' /)
logical :: ls_u,ls_v,ls_prse,ls_q,ls_tsen,ls_ql,ls_qi
real(r_kind),pointer,dimension(:,:)   :: rv_ps,rv_sst
real(r_kind),pointer,dimension(:,:)   :: rv_gust,rv_vis,rv_pblh,rv_wspd10m,rv_tcamt,rv_lcbas
real(r_kind),pointer,dimension(:,:)   :: rv_td2m,rv_mxtm,rv_mitm,rv_pmsl,rv_howv,rv_cldch
real(r_kind),pointer,dimension(:,:,:) :: rv_u,rv_v,rv_prse,rv_q,rv_tsen,rv_tv,rv_oz
real(r_kind),pointer,dimension(:,:,:) :: rv_rank3
real(r_kind),pointer,dimension(:,:)   :: rv_rank2

real(r_kind),allocatable,dimension(:,:,:):: uland,vland,uwter,vwter

logical :: do_getuv,do_tv_to_tsen_ad,do_normal_rh_to_q_ad,do_getprs_ad,do_cw_to_hydro_ad

!******************************************************************************

if (lsqrtb) then
   write(6,*)trim(myname),': not for sqrt(B)'
   call stop2(311)
end if

! Inquire about clouds
call gsi_metguess_get ('clouds::3d',nclouds,istatus)
if (nclouds>0) then
   allocate(clouds(nclouds))
   call gsi_metguess_get ('clouds::3d',clouds,istatus)
endif

! Inquire about chemistry
call gsi_chemguess_get('dim',ngases,istatus)
if (ngases>0) then
    allocate(gases(ngases))
    call gsi_chemguess_get('gsinames',gases,istatus)
endif

! Since each internal vector [step(jj)] of grad has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (grad%step(1),mycvars,icps,istatus)
lc_sf =icps(1)>0;lc_vp =icps(2)>0;lc_ps=icps(3)>0;lc_t  =icps(4)>0
lc_rh =icps(5)>0;lc_cw =icps(6)>0;lc_ql=icps(7)>0;lc_qi =icps(8)>0

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (rval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_ql =isps(6)>0; ls_qi =isps(7)>0

! Define what to do depending on what's in CV and SV
do_getuv            =lc_sf.and.lc_vp.and.ls_u  .and.ls_v
do_tv_to_tsen_ad    =lc_t .and.ls_q .and.ls_tsen
do_normal_rh_to_q_ad=lc_t .and.lc_rh.and.ls_prse.and.ls_q
do_getprs_ad        =lc_t .and.lc_ps.and.ls_prse

do_cw_to_hydro_ad=.false.
if (regional) then
   do_cw_to_hydro_ad=lc_cw.and.ls_ql.and.ls_qi
else
   do_cw_to_hydro_ad=lc_cw.and.ls_tsen.and.ls_ql.and.ls_qi.and.(.not.lc_ql) !ncep global
endif

call gsi_bundlegetpointer (grad%step(1),'oz',icoz,istatus)
call gsi_bundlegetpointer (grad%step(1),'gust',icgust,istatus)
call gsi_bundlegetpointer (grad%step(1),'vis',icvis,istatus)
call gsi_bundlegetpointer (grad%step(1),'pblh',icpblh,istatus)
call gsi_bundlegetpointer (grad%step(1),'wspd10m',icwspd10m,istatus)
call gsi_bundlegetpointer (grad%step(1),'td2m',ictd2m,istatus)
call gsi_bundlegetpointer (grad%step(1),'mxtm',icmxtm,istatus)
call gsi_bundlegetpointer (grad%step(1),'mitm',icmitm,istatus)
call gsi_bundlegetpointer (grad%step(1),'pmsl',icpmsl,istatus)
call gsi_bundlegetpointer (grad%step(1),'howv',ichowv,istatus)
call gsi_bundlegetpointer (grad%step(1),'sfwter',icsfwter,istatus)
call gsi_bundlegetpointer (grad%step(1),'vpwter',icvpwter,istatus)
call gsi_bundlegetpointer (grad%step(1),'tcamt',ictcamt,istatus)
call gsi_bundlegetpointer (grad%step(1),'lcbas',iclcbas,istatus)
call gsi_bundlegetpointer (grad%step(1),'cldch',iccldch,istatus)

! Loop over control steps
do jj=1,nsubwin

!  Create a work bundle similar to grad control vector's bundle
   call gsi_bundlecreate ( wbundle, grad%step(jj), 'control2state_ad work', istatus )
   if (istatus/=0) then
      write(6,*) trim(myname),': trouble creating work bundle'
      call stop2(999)
   endif

!$omp parallel sections private(istatus,ii,ic,id,istatus_oz)

!$omp section

   call gsi_bundlegetpointer (wbundle,'sf' ,cv_sf ,istatus)
   call gsi_bundlegetpointer (wbundle,'vp' ,cv_vp ,istatus)
   call gsi_bundlegetpointer (rval(jj),'u'   ,rv_u,   istatus)
   call gsi_bundlegetpointer (rval(jj),'v'   ,rv_v,   istatus)
   call gsi_bundleputvar ( wbundle, 'sf',  zero,   istatus )
   call gsi_bundleputvar ( wbundle, 'vp',  zero,   istatus )
!  Convert RHS calculations for u,v to st/vp for application of
!  background error
   if (do_getuv) then
       if (twodvar_regional .and. icsfwter>0 .and. icvpwter>0) then
           call gsi_bundlegetpointer (wbundle,'sfwter', cv_sfwter,istatus)
           call gsi_bundlegetpointer (wbundle,'vpwter', cv_vpwter,istatus)
           allocate(uland(lat2,lon2,nsig),vland(lat2,lon2,nsig), &
                    uwter(lat2,lon2,nsig),vwter(lat2,lon2,nsig))

           uland=zero ; uwter=zero
           vland=zero ; vwter=zero

           call landlake_uvmerge(rv_u,rv_v,uland,vland,uwter,vwter,0)

           call getuv(uwter,vwter,cv_sfwter,cv_vpwter,1)
           call getuv(uland,vland,cv_sf,cv_vp,1)
           deallocate(uland,vland,uwter,vwter)
         else
           call getuv(rv_u,rv_v,cv_sf,cv_vp,1)
       endif
   endif

   if(jj == 1)then
      do ii=1,nsclen
        grad%predr(ii)=bval%predr(ii)
      enddo
      do ii=1,npclen
        grad%predp(ii)=bval%predp(ii)
      enddo
      if (ntclen>0) then 
         do ii=1,ntclen
           grad%predt(ii)=bval%predt(ii)
         enddo
      end if
   end if

!$omp section

!  Get pointers to required control variables
   call gsi_bundlegetpointer (wbundle,'ps' ,cv_ps ,istatus)
   call gsi_bundlegetpointer (wbundle,'t'  ,cv_t,  istatus)
   call gsi_bundlegetpointer (wbundle,'q'  ,cv_rh ,istatus)

!  Get pointers to this subwin require state variables
   call gsi_bundlegetpointer (rval(jj),'ps'  ,rv_ps,  istatus)
   call gsi_bundlegetpointer (rval(jj),'prse',rv_prse,istatus)
   call gsi_bundlegetpointer (rval(jj),'tv'  ,rv_tv,  istatus)
   call gsi_bundlegetpointer (rval(jj),'tsen',rv_tsen,istatus)
   call gsi_bundlegetpointer (rval(jj),'q'   ,rv_q ,  istatus)

!  Adjoint of control to initial state
   call gsi_bundleputvar ( wbundle, 't' ,  rv_tv,  istatus )
   call gsi_bundleputvar ( wbundle, 'q' ,  zero,   istatus )
   call gsi_bundleputvar ( wbundle, 'ps',  rv_ps,  istatus )
   if (do_cw_to_hydro_ad) then
!     Case when cloud-vars do not map one-to-one
!     e.g. cw-to-ql&qi
      call cw2hydro_ad(rval(jj),wbundle,clouds,nclouds)
   else
!     Case when cloud-vars map one-to-one, take care of them together
!     e.g. cw-to-cw
      do ic=1,nclouds
         id=getindex(cvars3d,clouds(ic))
         if (id>0) then
            call gsi_bundlegetpointer (rval(jj),clouds(ic),rv_rank3,istatus)
            call gsi_bundleputvar     (wbundle, clouds(ic),rv_rank3,istatus)
         endif
      enddo
   end if
!  Calculate sensible temperature
   if(do_tv_to_tsen_ad) call tv_to_tsen_ad(cv_t,rv_q,rv_tsen)

!  Adjoint of convert input normalized RH to q to add contribution of moisture
!  to t, p , and normalized rh
   if(do_normal_rh_to_q_ad) call normal_rh_to_q_ad(cv_rh,cv_t,rv_prse,rv_q)

!  Adjoint to convert ps to 3-d pressure
   if(do_getprs_ad) call getprs_ad(cv_ps,cv_t,rv_prse)


!$omp section

   call gsi_bundlegetpointer (rval(jj),'sst' ,rv_sst, istatus)
   call gsi_bundleputvar ( wbundle, 'sst', rv_sst, istatus )

!  call gsi_bundlegetpointer (rval(jj),'oz'  ,rv_oz , istatus)     
   call gsi_bundlegetpointer (rval(jj),'oz'  ,rv_oz , istatus_oz) 

   if (icoz>0) then
      call gsi_bundleputvar ( wbundle, 'oz',  rv_oz,  istatus )
   else
      if(istatus_oz==0) rv_oz=zero 
   end if

!  Same one-to-one map for chemistry-vars; take care of them together
   do ic=1,ngases
      id=getindex(cvars3d,gases(ic))
      if (id>0) then
          call gsi_bundlegetpointer (rval(jj),gases(ic),rv_rank3,istatus)
          call gsi_bundleputvar     (wbundle, gases(ic),rv_rank3,istatus)
      endif
      id=getindex(cvars2d,gases(ic))
      if (id>0) then
          call gsi_bundlegetpointer (rval(jj),gases(ic),rv_rank2,istatus)
          call gsi_bundleputvar     (wbundle, gases(ic),rv_rank2,istatus)
      endif
   enddo

   if (icgust>0) then
      call gsi_bundlegetpointer (rval(jj),'gust' ,rv_gust, istatus)
      call gsi_bundleputvar ( wbundle, 'gust', rv_gust, istatus )
   end if
   if (icvis >0) then
      call gsi_bundlegetpointer (wbundle,'vis'  ,cv_vis ,istatus)
      call gsi_bundlegetpointer (rval(jj),'vis'  ,rv_vis , istatus)
      call gsi_bundleputvar ( wbundle, 'vis' , zero   , istatus )
      !  Adjoint of convert logvis to vis
      call logvis_to_vis_ad(cv_vis,rv_vis)
   end if
   if (icpblh>0)then
      call gsi_bundlegetpointer (rval(jj),'pblh' ,rv_pblh, istatus)
      call gsi_bundleputvar ( wbundle, 'pblh', rv_pblh, istatus )
   end if
   if (icwspd10m>0) then
      call gsi_bundlegetpointer (rval(jj),'wspd10m' ,rv_wspd10m, istatus)
      call gsi_bundleputvar ( wbundle, 'wspd10m', rv_wspd10m, istatus )
   end if
   if (ictd2m>0) then
      call gsi_bundlegetpointer (rval(jj),'td2m' ,rv_td2m, istatus)
      call gsi_bundleputvar ( wbundle, 'td2m', rv_td2m, istatus )
   end if
   if (icmxtm>0) then
      call gsi_bundlegetpointer (rval(jj),'mxtm' ,rv_mxtm, istatus)
      call gsi_bundleputvar ( wbundle, 'mxtm', rv_mxtm, istatus )
   end if
   if (icmitm>0) then
      call gsi_bundlegetpointer (rval(jj),'mitm' ,rv_mitm, istatus)
      call gsi_bundleputvar ( wbundle, 'mitm', rv_mitm, istatus )
   end if
   if (icpmsl>0) then
      call gsi_bundlegetpointer (rval(jj),'pmsl' ,rv_pmsl, istatus)
      call gsi_bundleputvar ( wbundle, 'pmsl', rv_pmsl, istatus )
   end if
   if (ichowv>0) then
      call gsi_bundlegetpointer (rval(jj),'howv' ,rv_howv, istatus)
      call gsi_bundleputvar ( wbundle, 'howv', rv_howv, istatus )
   end if
   if (ictcamt>0) then
      call gsi_bundlegetpointer (rval(jj),'tcamt',rv_tcamt, istatus)
      call gsi_bundleputvar ( wbundle, 'tcamt', rv_tcamt, istatus )
   end if
   if (iclcbas>0) then
      call gsi_bundlegetpointer (wbundle,'lcbas',cv_lcbas,istatus)
      call gsi_bundlegetpointer (rval(jj),'lcbas',rv_lcbas, istatus)
      call gsi_bundleputvar ( wbundle, 'lcbas', zero, istatus )
      !  Adjoint of convert loglcbas to lcbas
      call loglcbas_to_lcbas_ad(cv_lcbas,rv_lcbas)
   end if
   if (iccldch >0) then
      call gsi_bundlegetpointer (wbundle,'cldch'  ,cv_cldch ,istatus)
      call gsi_bundlegetpointer (rval(jj),'cldch' ,rv_cldch , istatus)
      call gsi_bundleputvar ( wbundle, 'cldch' , zero   , istatus )
      !  Adjoint of convert logcldch to cldch
      call logcldch_to_cldch_ad(cv_cldch,rv_cldch)
   end if

!$omp end parallel sections

!  Adjoint of transfer variables

   do ii=1,wbundle%ndim
      grad%step(jj)%values(ii)=wbundle%values(ii)+grad%step(jj)%values(ii)
   enddo
   call gsi_bundledestroy(wbundle,istatus)
   if (istatus/=0) then
      write(6,*) trim(myname),': trouble destroying work bundle'
      call stop2(999)
   endif

end do

! Clean up
if (ngases>0) deallocate(gases)

if (nclouds>0) deallocate(clouds)

return
end subroutine control2state_ad
