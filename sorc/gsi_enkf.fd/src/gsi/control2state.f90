!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  control2state_mod --- control2state_mod variables and routines
!
! !INTERFACE:
!
module control2state_mod

! !USES:


! !DESCRIPTION: module control2state routines and variables

use kinds, only: r_kind,i_kind
use constants, only : max_varname_length, zero
use control_vectors, only: control_vector,c2sset_flg
use control_vectors, only: cvars3d,cvars2d
use bias_predictors, only: predictors
use jfunc, only: nsclen,npclen,ntclen
use gsi_4dvar, only: nsubwin, l4dvar, lsqrtb,ladtest_obs
use gsi_chemguess_mod, only: gsi_chemguess_get
use gsi_metguess_mod, only: gsi_metguess_get
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_bundlemod, only: assignment(=)
use gridmod, only: nems_nmmb_regional
use gridmod, only: regional, twodvar_regional            
use gridmod, only: lat2,lon2,nsig,nlat,nlon            
use chemmod, only: laeroana_fv3cmaq, naero_cmaq_fv3,aeronames_cmaq_fv3,imodes_cmaq_fv3,icvt_cmaq_fv3
use mpeu_util, only: getindex

implicit none

private
public :: control2state
public :: control2state_ad

logical :: do_getprs,do_normal_rh_to_q,do_tv_to_tsen,do_getuv,do_cw_to_hydro
logical :: do_cw_to_hydro_hwrf

integer(i_kind) :: icpblh,icgust,icvis,icoz,icwspd10m,icw
integer(i_kind) :: ictd2m,icmxtm,icmitm,icpmsl,ichowv
integer(i_kind) :: icsfwter,icvpwter,ictcamt,iclcbas
integer(i_kind) :: iccldch,icuwnd10m,icvwnd10m

integer :: ngases,nclouds

contains
subroutine control2state(xhat,sval,bval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    control2state
!   prgmmr: tremolet
!
! abstract:  Converts control variable to physical space
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2008-11-28  todling  - add calc of 3dp; upd rh_to_q (Cucurull 2007-07-26)
!   2009-04-21  derber   - modify call to getuv to getuv(*,0)
!   2009-06-16  parrish  - for l_hyb_ens=.true., add calls to ensemble_forward_model and strong_bk
!   2009-08-14  lueken   - update documentation
!   2009-11-27  parrish  - for uv_hyb_ens=.true., then ensemble perturbations contain u,v instead of st,vp
!                            so introduce extra code to handle this case.
!   2010-02-21  parrish  - introduce changes to allow dual resolution, with ensemble computation on
!                            lower resolution grid compared to analysis grid.
!                            new parameter dual_res=.true. if ensemble grid is different from analysis grid.
!   2010-03-23  zhu      - use cstate for generalizing control variable
!   2010-04-29  todling  - update to use gsi_bundle; some changes toward bypassing standard atmos analysis
!   2010-05-12  todling  - rename cstate to wbundle; state_vector now a bundle
!   2010-05-31  todling  - better consistency checks; add co/co2
!                        - ready to bypass analysis of (any) meteorological fields
!   2010-06-04  parrish  - bug fix: u,v copy to wbundle after getuv for hyb ensemble
!   2010-06-15  todling  - generalized handling of chemistry
!   2011-02-20  zhu      - add gust,vis,pblh
!   2011-05-15  auligne/todling - generalized cloud handling
!   2011-07-12   zhu     - add do_cw_to_hydro and cwhydromod for cloudy radiance assimilation
!   2011-11-01  eliu     - generalize the use of do_cw_to_hydro
!   2012-02-08  kleist   - remove call to strong_bk, ensemble_forward_model, 
!                             ensemble_forward_model_dual_res, and related parameters
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - updated for obs adjoint test and added ladtest_obs  
!   2013-10-24  todling  - nullify work pointers
!   2013-10-28  todling  - rename p3d to prse
!   2013-05-23   zhu     - add ntclen and predt for aircraft temperature bias correction
!   2014-01-31  mkim     - add support for when ql and qi are CVs for all-sky mw radiance DA
!   2014-03-19  pondeca  - add wspd10m
!   2014-04-10  pondeca  - add td2m,mxtm,mitm,pmsl
!   2014-05-07  pondeca - add howv
!   2014-06-16  carley/zhu - add tcamt and lcbas
!   2014-12-03  derber   - introduce parallel regions for optimization
!   2015-07-10  pondeca  - add cldch
!   2016-05-03  pondeca  - add uwnd10m and vwnd10m
!   2017-05-12  Y. Wang and X. Wang - add w as state variable for rw DA, POC: xuguang.wang@ou.edu
!   2016-08-12  lippi    - add vertical velocity (w) to mycvars and mysvars.
!   2022-05-24  H.Wang   - add amass2aero_tl for regional FV3-CMAQ DA when using
!                          total mass as control variable. 
!
!   input argument list:
!     xhat - Control variable
!     sval - State variable
!     bval - Bias predictors
!
!   output argument list:
!     sval - State variable
!     bval - Bias predictors
!
!$$$ end documentation block
use amassaeromod, only: amass2aero_tl
use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
use general_commvars_mod, only: s2g_cv
use cwhydromod, only: cw2hydro_tl
use cwhydromod, only: cw2hydro_tl_hwrf
implicit none
  
! Declare passed variables  
type(control_vector), intent(in   ) :: xhat
type(gsi_bundle)    , intent(inout) :: sval(nsubwin)
type(predictors)    , intent(inout) :: bval

! Declare local variables  	
character(len=*),parameter::myname='control2state'
character(len=max_varname_length),allocatable,dimension(:) :: gases
character(len=max_varname_length),allocatable,dimension(:) :: clouds
real(r_kind),dimension(nlat*nlon*s2g_cv%nlevs_alloc)      :: hwork
integer(i_kind) :: ii,jj,ic,id,istatus,istatus_oz 
type(gsi_bundle):: wbundle ! work bundle

real(r_kind),pointer,dimension(:,:)   :: cv_ps=>NULL()
real(r_kind),pointer,dimension(:,:)   :: cv_lcbas=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_sf=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vp=>NULL()
!real(r_kind),pointer,dimension(:,:,:) :: cv_w=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_t=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_rh=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_sfwter=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vpwter=>NULL()

real(r_kind),pointer,dimension(:,:)   :: sv_ps=>NULL(),sv_sst=>NULL()
real(r_kind),pointer,dimension(:,:)   :: sv_gust=>NULL(),sv_vis=>NULL(),sv_pblh=>NULL()
real(r_kind),pointer,dimension(:,:)   :: sv_wspd10m=>NULL(),sv_tcamt=>NULL(),sv_lcbas=>NULL()
real(r_kind),pointer,dimension(:,:)   :: sv_td2m=>NULL(),sv_mxtm=>NULL(),sv_mitm=>NULL()
real(r_kind),pointer,dimension(:,:)   :: sv_pmsl=>NULL(),sv_howv=>NULL(),sv_cldch=>NULL()
real(r_kind),pointer,dimension(:,:)   :: sv_uwnd10m=>NULL(),sv_vwnd10m=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_u=>NULL(),sv_v=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_w=>NULL(),sv_dw=>NULL(),sv_prse=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_q=>NULL(),sv_tsen=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_tv=>NULL(),sv_oz=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: sv_rank3=>NULL()
real(r_kind),pointer,dimension(:,:)   :: sv_rank2=>NULL()

real(r_kind),allocatable,dimension(:,:,:):: uland,vland,uwter,vwter

if (c2sset_flg)call c2sset(xhat,sval)
if (nclouds>0) then
   allocate(clouds(nclouds))
   call gsi_metguess_get('clouds::3d',clouds,istatus)
end if

if (ngases>0) then
    allocate(gases(ngases))
    call gsi_chemguess_get('gsinames',gases,istatus)
endif


! Loop over control steps
do jj=1,nsubwin

!  Create a temporary bundle similar to xhat, and copy contents of xhat into it
   call gsi_bundlecreate ( wbundle, xhat%step(jj), 'control2state work', istatus )
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble creating work bundle'
      call stop2(999)
   endif
   wbundle=xhat%step(jj)

!  Get pointers to required control variables

   if(ladtest_obs) then
! Convert from subdomain to full horizontal field distributed among processors
      call general_sub2grid(s2g_cv,wbundle%values,hwork)
! Put back onto subdomains
      call general_grid2sub(s2g_cv,hwork,wbundle%values)
   end if

!$omp parallel sections private(istatus,ii,ic,id,uland,vland,uwter,vwter) 

!$omp section

   call gsi_bundlegetpointer (wbundle,'sf' ,cv_sf ,istatus)
   call gsi_bundlegetpointer (wbundle,'vp' ,cv_vp ,istatus)
   call gsi_bundlegetpointer (sval(jj),'u'   ,sv_u,   istatus)
   call gsi_bundlegetpointer (sval(jj),'v'   ,sv_v,   istatus)
!  Convert streamfunction and velocity potential to u,v
   if(do_getuv) then
      if (twodvar_regional .and. icsfwter>0 .and. icvpwter>0) then
         call gsi_bundlegetpointer (wbundle,'sfwter',cv_sfwter,istatus)
         call gsi_bundlegetpointer (wbundle,'vpwter',cv_vpwter,istatus)
         allocate(uland(lat2,lon2,nsig),vland(lat2,lon2,nsig), &
                  uwter(lat2,lon2,nsig),vwter(lat2,lon2,nsig))
         call getuv(uland,vland,cv_sf,cv_vp,0)
         call getuv(uwter,vwter,cv_sfwter,cv_vpwter,0)

         call landlake_uvmerge(sv_u,sv_v,uland,vland,uwter,vwter,1)
         deallocate(uland,vland,uwter,vwter)
      else
         call getuv(sv_u,sv_v,cv_sf,cv_vp,0)
      end if
   end if

!$omp section
!  Get pointers to required state variables
   call gsi_bundlegetpointer (sval(jj),'prse',sv_prse,istatus)
   call gsi_bundlegetpointer (sval(jj),'tv'  ,sv_tv,  istatus)
   call gsi_bundlegetpointer (sval(jj),'tsen',sv_tsen,istatus)
   call gsi_bundlegetpointer (sval(jj),'q'   ,sv_q ,  istatus)
   call gsi_bundlegetpointer (wbundle,'ps' ,cv_ps ,istatus)
   call gsi_bundlegetpointer (wbundle,'t'  ,cv_t,  istatus)
   call gsi_bundlegetpointer (wbundle,'q'  ,cv_rh ,istatus)

!  Copy other variables
   call gsi_bundlegetvar ( wbundle, 't'  , sv_tv,  istatus )  
!  Get 3d pressure
   if(do_getprs) call getprs_tl(cv_ps,cv_t,sv_prse)

!  Convert input normalized RH to q
   if(do_normal_rh_to_q) call normal_rh_to_q(cv_rh,cv_t,sv_prse,sv_q)

!  Calculate sensible temperature
   if(do_tv_to_tsen) call tv_to_tsen(cv_t,sv_q,sv_tsen) 


   if (do_cw_to_hydro .and. .not.do_cw_to_hydro_hwrf) then
!     Case when cloud-vars do not map one-to-one (cv-to-sv)
!     e.g. cw-to-ql&qi
      call cw2hydro_tl(sval(jj),wbundle,clouds,nclouds)
   elseif (do_cw_to_hydro_hwrf) then
!     Case when cloud-vars do not map one-to-one (cv-to-sv)
!     e.g. cw-to-ql&qi&qr&qs&qg&qh
      if (.not.do_tv_to_tsen) then
        call tv_to_tsen(cv_t,sv_q,sv_tsen)
      endif
      call cw2hydro_tl_hwrf(sval(jj),wbundle,sv_tsen)
   else
!     Case when cloud-vars map one-to-one (cv-to-sv), take care of them together
!     e.g. cw-to-cw
      do ic=1,nclouds
         id=getindex(cvars3d,clouds(ic))
         if (id>0) then
             call gsi_bundlegetpointer (sval(jj),clouds(ic),sv_rank3,istatus)
             call gsi_bundlegetvar     (wbundle, clouds(ic),sv_rank3,istatus)
         endif
      enddo
   end if
   call gsi_bundlegetpointer (sval(jj),'ps'  ,sv_ps,  istatus)
   call gsi_bundlegetvar ( wbundle, 'ps' , sv_ps,  istatus )

!$omp section
   call gsi_bundlegetpointer (sval(jj),'oz'  ,sv_oz , istatus_oz)  
   if (icoz>0) then
      call gsi_bundlegetvar ( wbundle, 'oz' , sv_oz,  istatus )
   else
      if(istatus_oz==0) sv_oz=zero   
   end if

!  Same one-to-one map for chemistry-vars; take care of them together 
   if (.not.laeroana_fv3cmaq .and. icvt_cmaq_fv3 == 2) then
         write(6,*) ' icvt_cmaq_fv3 == 2 but laeroana_fv3cmaq=false stop!!!'
         call stop2(999)
   endif
   if (icvt_cmaq_fv3 == 2) then
      call amass2aero_tl(sval(jj),wbundle,aeronames_cmaq_fv3,naero_cmaq_fv3)
   else
     do ic=1,ngases
       ! take care gases and aero variables if one to one mapping
       id=getindex(cvars3d,gases(ic))
       if (id>0) then
          call gsi_bundlegetpointer (sval(jj),gases(ic),sv_rank3,istatus)
          call gsi_bundlegetvar     (wbundle, gases(ic),sv_rank3,istatus)
       endif
       id=getindex(cvars2d,gases(ic))
       if (id>0) then
          call gsi_bundlegetpointer (sval(jj),gases(ic),sv_rank2,istatus)
          call gsi_bundlegetvar     (wbundle, gases(ic),sv_rank2,istatus)
       endif
     enddo
   end if 

!$omp section
   if(jj == 1)then
!    Biases
      do ii=1,nsclen
         bval%predr(ii)=xhat%predr(ii)
      enddo

      do ii=1,npclen
         bval%predp(ii)=xhat%predp(ii)
      enddo

      if (ntclen>0) then
         do ii=1,ntclen
            bval%predt(ii)=xhat%predt(ii)
         enddo
      end if
   end if

   call gsi_bundlegetpointer (sval(jj),'sst' ,sv_sst, istatus)
   call gsi_bundlegetvar ( wbundle, 'sst', sv_sst, istatus )

   if (icgust>0) then
      call gsi_bundlegetpointer (sval(jj),'gust' ,sv_gust, istatus)
      call gsi_bundlegetvar ( wbundle, 'gust', sv_gust, istatus )
   end if
   if (icpblh>0) then
      call gsi_bundlegetpointer (sval(jj),'pblh' ,sv_pblh, istatus)
      call gsi_bundlegetvar ( wbundle, 'pblh', sv_pblh, istatus )
   end if
   if (icvis >0) then
      call gsi_bundlegetpointer (sval(jj),'vis'  ,sv_vis , istatus)
      call gsi_bundlegetvar  (wbundle,'vis',sv_vis,istatus)
   end if
   if (icwspd10m>0) then
      call gsi_bundlegetpointer (sval(jj),'wspd10m' ,sv_wspd10m, istatus)
      call gsi_bundlegetvar ( wbundle, 'wspd10m', sv_wspd10m, istatus )      
   end if
   if (ictd2m>0) then
      call gsi_bundlegetpointer (sval(jj),'td2m' ,sv_td2m, istatus)
      call gsi_bundlegetvar ( wbundle, 'td2m', sv_td2m, istatus )
   end if
   if (icmxtm>0) then
      call gsi_bundlegetpointer (sval(jj),'mxtm' ,sv_mxtm, istatus)
      call gsi_bundlegetvar ( wbundle, 'mxtm', sv_mxtm, istatus )
   end if
   if (icmitm>0) then 
      call gsi_bundlegetpointer (sval(jj),'mitm' ,sv_mitm, istatus)
      call gsi_bundlegetvar ( wbundle, 'mitm', sv_mitm, istatus )
   end if
   if (icpmsl>0) then 
      call gsi_bundlegetpointer (sval(jj),'pmsl' ,sv_pmsl, istatus)
      call gsi_bundlegetvar ( wbundle, 'pmsl', sv_pmsl, istatus )
   end if
   if (ichowv>0) then
      call gsi_bundlegetpointer (sval(jj),'howv' ,sv_howv, istatus)
      call gsi_bundlegetvar ( wbundle, 'howv', sv_howv, istatus )
   end if
   if (icw>0) then 
      call gsi_bundlegetpointer (sval(jj),'w' ,sv_w, istatus)
      call gsi_bundlegetvar ( wbundle, 'w', sv_w, istatus )
      if(nems_nmmb_regional)then
         call gsi_bundlegetpointer (sval(jj),'dw'  ,sv_dw,  istatus)
         call gsi_bundlegetvar ( wbundle, 'dw' , sv_dw,  istatus )
      end if
   end if
   if (ictcamt>0) then 
      call gsi_bundlegetpointer (sval(jj),'tcamt' ,sv_tcamt, istatus)
      call gsi_bundlegetvar ( wbundle, 'tcamt', sv_tcamt, istatus )
   end if
   if (iclcbas >0) then 
      call gsi_bundlegetpointer (wbundle,'lcbas',cv_lcbas,istatus)
      call gsi_bundlegetpointer (sval(jj),'lcbas' ,sv_lcbas, istatus)
      ! Convert log(lcbas) to lcbas
      call loglcbas_to_lcbas(cv_lcbas,sv_lcbas)
   end if
   if (iccldch >0) then
      call gsi_bundlegetpointer (sval(jj),'cldch'  ,sv_cldch , istatus)
      call gsi_bundlegetvar (wbundle,'cldch',sv_cldch,istatus)
   end if
   if (icuwnd10m>0) then
      call gsi_bundlegetpointer (sval(jj),'uwnd10m' ,sv_uwnd10m, istatus)
      call gsi_bundlegetvar ( wbundle, 'uwnd10m', sv_uwnd10m, istatus )
   end if
   if (icvwnd10m>0) then
      call gsi_bundlegetpointer (sval(jj),'vwnd10m' ,sv_vwnd10m, istatus)
      call gsi_bundlegetvar ( wbundle, 'vwnd10m', sv_vwnd10m, istatus )
   end if


!$omp end parallel sections

! Clean up
   call gsi_bundledestroy(wbundle,istatus)
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble destroying work bundle'
      call stop2(999)
   endif

end do

if (ngases>0)  deallocate(gases)

if (nclouds>0) deallocate(clouds)

return
end subroutine control2state
subroutine c2sset(xhat,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    c2sset
!   prgmmr: derber
!
! abstract:  Sets flags for control2state and control2state_ad
!
! program history log:
!   2022-08-30  derber   - initial code from control2state

!   input argument list:
!     xhat - Control variable
!     sval - State variable
!
!$$$ end documentation block
implicit none

! Declare passed variables
type(control_vector), intent(in) :: xhat
type(gsi_bundle)    , intent(in) :: sval(nsubwin)

! Declare local variables
character(len=*),parameter::myname='c2sset'
integer(i_kind) :: istatus

! Note: The following does not aim to get all variables in
!       the state and control vectors, but rather the ones
!       this routines knows how to handle.
! Declare required local control variables
integer(i_kind), parameter :: ncvars = 9
integer(i_kind) :: icps(ncvars)
character(len=3), parameter :: mycvars(ncvars) = (/  &  ! vars from CV needed here
                'sf ', 'vp ', 'ps ', 't  ', 'q  ', 'cw ', 'ql ', 'qi ', 'w  ' /)
logical :: lc_sf,lc_vp,lc_w,lc_ps,lc_t,lc_rh,lc_cw,lc_ql,lc_qi

! Declare required local state variables
integer(i_kind), parameter :: nsvars = 12
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
                'u   ', 'v   ', 'prse', 'q   ', 'tsen', 'ql  ', 'qi  ', 'w   ', &
                'qr  ', 'qs  ', 'qg  ', 'qh  ' /)
logical :: ls_u,ls_v,ls_w,ls_prse,ls_q,ls_tsen,ls_ql,ls_qi
logical :: ls_qr,ls_qs,ls_qg,ls_qh



if (lsqrtb) then
   write(6,*)trim(myname),': not for sqrt(B)'
   call stop2(106)
end if
if (nsubwin/=1 .and. .not.l4dvar) then
   write(6,*)trim(myname),': error 3dvar',nsubwin,l4dvar
   call stop2(107)
end if

! Inquire about cloud-vars
call gsi_metguess_get('clouds::3d',nclouds,istatus)

! Inquire about chemistry
call gsi_chemguess_get('dim',ngases,istatus)

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (xhat%step(1),mycvars,icps,istatus)
lc_sf =icps(1)>0; lc_vp =icps(2)>0; lc_ps =icps(3)>0
lc_t  =icps(4)>0; lc_rh =icps(5)>0; lc_cw =icps(6)>0
lc_ql =icps(7)>0; lc_qi =icps(8)>0; lc_w  =icps(9)>0

! Since each internal vector of sval has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (sval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_ql =isps(6)>0
ls_qi =isps(7)>0; ls_w   =isps(8)>0
ls_qr =isps(9)>0; ls_qs  =isps(10)>0
ls_qg =isps(11)>0; ls_qh =isps(12)>0

! Define what to do depending on what's in CV and SV
do_getprs        =lc_ps.and.lc_t .and.ls_prse
do_normal_rh_to_q=lc_rh.and.lc_t .and.ls_prse.and.ls_q
do_tv_to_tsen    =lc_t .and.ls_q .and.ls_tsen
do_getuv         =lc_sf.and.lc_vp.and.ls_u.and.ls_v

do_cw_to_hydro=.false.
do_cw_to_hydro_hwrf=.false.
if (regional) then
   do_cw_to_hydro=lc_cw.and.ls_ql.and.ls_qi
   do_cw_to_hydro_hwrf=lc_cw.and.ls_ql.and.ls_qi.and.ls_qr.and.ls_qs.and.ls_qg.and.ls_qh
else
   do_cw_to_hydro=lc_cw.and.ls_tsen.and.ls_ql.and.ls_qi.and.(.not.lc_ql) !ncep global 
endif

call gsi_bundlegetpointer (xhat%step(1),'oz',icoz,istatus)
call gsi_bundlegetpointer (xhat%step(1),'gust',icgust,istatus)
call gsi_bundlegetpointer (xhat%step(1),'vis',icvis,istatus)
call gsi_bundlegetpointer (xhat%step(1),'pblh',icpblh,istatus)
call gsi_bundlegetpointer (xhat%step(1),'wspd10m',icwspd10m,istatus)
call gsi_bundlegetpointer (xhat%step(1),'td2m',ictd2m,istatus)
call gsi_bundlegetpointer (xhat%step(1),'mxtm',icmxtm,istatus)
call gsi_bundlegetpointer (xhat%step(1),'mitm',icmitm,istatus)
call gsi_bundlegetpointer (xhat%step(1),'pmsl',icpmsl,istatus)
call gsi_bundlegetpointer (xhat%step(1),'howv',ichowv,istatus)
call gsi_bundlegetpointer (xhat%step(1),'sfwter',icsfwter,istatus)
call gsi_bundlegetpointer (xhat%step(1),'vpwter',icvpwter,istatus)
call gsi_bundlegetpointer (xhat%step(1),'w',icw,istatus)
call gsi_bundlegetpointer (xhat%step(1),'tcamt',ictcamt,istatus)
call gsi_bundlegetpointer (xhat%step(1),'lcbas',iclcbas,istatus)
call gsi_bundlegetpointer (xhat%step(1),'cldch',iccldch,istatus)
call gsi_bundlegetpointer (xhat%step(1),'uwnd10m',icuwnd10m,istatus)
call gsi_bundlegetpointer (xhat%step(1),'vwnd10m',icvwnd10m,istatus)

c2sset_flg=.false.  ! set to true in setup.  set to false after first (only) call to c2sset
return
end subroutine c2sset
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
!   2016-05-03  pondeca  - add uwnd10m, and vwnd10m
!   2017-05-12  Y. Wang and X. Wang - add w as state variable for rw DA, POC: xuguang.wang@ou.edu
!   2016-08-12  lippi    - add vertical velocity (w) to mycvars and mysvars.
!   2016-05-03  pondeca  - add uwnd10m, and vwnd10m
!   2022-05-24  H.Wang   - add amass2aero_ad for regional FV3-CMAQ DA when using
!                          total mass as control variable. 
!
!   input argument list:
!     rval - State variable
!     bval
!   output argument list:
!     grad - Control variable
!
!$$$
use amassaeromod, only: amass2aero_ad 
use cwhydromod, only: cw2hydro_ad
use cwhydromod, only: cw2hydro_ad_hwrf

implicit none

! Declare passed variables
type(gsi_bundle)    , intent(inout) :: rval(nsubwin)
type(predictors)    , intent(in   ) :: bval
type(control_vector), intent(inout) :: grad

! Declare local variables
character(len=*),parameter::myname='control2state_ad'
character(len=max_varname_length),allocatable,dimension(:) :: gases
character(len=max_varname_length),allocatable,dimension(:) :: clouds
integer(i_kind) :: ii,jj,ic,id,istatus,istatus_oz 
type(gsi_bundle) :: wbundle ! work bundle

real(r_kind),pointer,dimension(:,:)   :: cv_ps=>NULL()
real(r_kind),pointer,dimension(:,:)   :: cv_lcbas=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_sf=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vp=>NULL()
!real(r_kind),pointer,dimension(:,:,:) :: cv_w=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_t=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_rh=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_sfwter=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: cv_vpwter=>NULL()

! Declare required local state variables
real(r_kind),pointer,dimension(:,:)   :: rv_ps=>NULL(),rv_sst=>NULL()
real(r_kind),pointer,dimension(:,:)   :: rv_gust=>NULL(),rv_vis=>NULL(),rv_pblh=>NULL()
real(r_kind),pointer,dimension(:,:)   :: rv_wspd10m=>NULL(),rv_tcamt=>NULL(),rv_lcbas=>NULL()
real(r_kind),pointer,dimension(:,:)   :: rv_td2m=>NULL(),rv_mxtm=>NULL(),rv_mitm=>NULL()
real(r_kind),pointer,dimension(:,:)   :: rv_pmsl=>NULL(),rv_howv=>NULL(),rv_cldch=>NULL()
real(r_kind),pointer,dimension(:,:)   :: rv_uwnd10m=>NULL(),rv_vwnd10m=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_u=>NULL(),rv_v=>NULL(),rv_w=>NULL(),rv_dw=>NULL(),rv_prse=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_q=>NULL(),rv_tsen=>NULL(),rv_tv=>NULL(),rv_oz=>NULL()
real(r_kind),pointer,dimension(:,:,:) :: rv_rank3=>NULL()
real(r_kind),pointer,dimension(:,:)   :: rv_rank2=>NULL()

real(r_kind),allocatable,dimension(:,:,:):: uland,vland,uwter,vwter



!******************************************************************************

if (c2sset_flg)call c2sset(grad,rval)
if (lsqrtb) then
   write(6,*)trim(myname),': not for sqrt(B)'
   call stop2(311)
end if

! Inquire about clouds
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


! Loop over control steps
do jj=1,nsubwin

!  Create a work bundle similar to grad control vector's bundle
   call gsi_bundlecreate ( wbundle, grad%step(jj), 'control2state_ad work', istatus )
   if (istatus/=0) then
      write(6,*) trim(myname),': trouble creating work bundle'
      call stop2(999)
   endif

!$omp parallel sections private(istatus,ii,ic,id,istatus_oz,rv_u,rv_v,rv_prse,rv_q,rv_tsen,uland,vland,uwter,vwter)

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

   if (do_cw_to_hydro .and. .not.do_cw_to_hydro_hwrf) then
!     Case when cloud-vars do not map one-to-one
!     e.g. cw-to-ql&qi
      call cw2hydro_ad(rval(jj),wbundle,clouds,nclouds)
   elseif (do_cw_to_hydro_hwrf) then
!     Case when cloud-vars do not map one-to-one
!     e.g. cw-to-ql&qi&qr&qs&qg&qh
      call cw2hydro_ad_hwrf(rval(jj),wbundle,rv_tsen)
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
   if(do_tv_to_tsen) call tv_to_tsen_ad(cv_t,rv_q,rv_tsen)

!  Adjoint of convert input normalized RH to q to add contribution of moisture
!  to t, p , and normalized rh
   if(do_normal_rh_to_q) call normal_rh_to_q_ad(cv_rh,cv_t,rv_prse,rv_q)

!  Adjoint to convert ps to 3-d pressure
   if(do_getprs) call getprs_ad(cv_ps,cv_t,rv_prse)


!$omp section


!  call gsi_bundlegetpointer (rval(jj),'oz'  ,rv_oz , istatus)     
   call gsi_bundlegetpointer (rval(jj),'oz'  ,rv_oz , istatus_oz) 

   if (icoz>0) then
      call gsi_bundleputvar ( wbundle, 'oz',  rv_oz,  istatus )
   else
      if(istatus_oz==0) rv_oz=zero 
   end if

!  Same one-to-one map for chemistry-vars; take care of them together
   if (.not.laeroana_fv3cmaq .and. icvt_cmaq_fv3 == 2) then
         write(6,*) ' icvt_cmaq_fv3 == 2 but laeroana_fv3cmaq=false stop!!!'
         call stop2(999)
   endif

   if (icvt_cmaq_fv3 == 2) then
      call amass2aero_ad(rval(jj),wbundle,aeronames_cmaq_fv3,naero_cmaq_fv3)
   else
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
   end if
!$omp section
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

   call gsi_bundlegetpointer (rval(jj),'sst' ,rv_sst, istatus)
   call gsi_bundleputvar ( wbundle, 'sst', rv_sst, istatus )

   if (icgust>0) then
      call gsi_bundlegetpointer (rval(jj),'gust' ,rv_gust, istatus)
      call gsi_bundleputvar ( wbundle, 'gust', rv_gust, istatus )
   end if
   if (icvis >0) then
      call gsi_bundlegetpointer (rval(jj),'vis'  ,rv_vis , istatus)
      call gsi_bundleputvar ( wbundle, 'vis' , rv_vis   , istatus )
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
   if (icw>0) then
      call gsi_bundlegetpointer (rval(jj),'w' ,rv_w, istatus)
      call gsi_bundleputvar ( wbundle, 'w', rv_w, istatus )
      if(nems_nmmb_regional)then
         call gsi_bundlegetpointer (rval(jj),'dw' ,rv_dw, istatus)
         call gsi_bundleputvar ( wbundle, 'dw', rv_dw, istatus )
       end if
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
      call gsi_bundlegetpointer (rval(jj),'cldch' ,rv_cldch , istatus)
      call gsi_bundleputvar ( wbundle, 'cldch' , rv_cldch  , istatus )
   end if
   if (icuwnd10m>0) then
      call gsi_bundlegetpointer (rval(jj),'uwnd10m' ,rv_uwnd10m, istatus)
      call gsi_bundleputvar ( wbundle, 'uwnd10m', rv_uwnd10m, istatus )
   end if
   if (icvwnd10m>0) then
      call gsi_bundlegetpointer (rval(jj),'vwnd10m' ,rv_vwnd10m, istatus)
      call gsi_bundleputvar ( wbundle, 'vwnd10m', rv_vwnd10m, istatus )
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
end module control2state_mod
