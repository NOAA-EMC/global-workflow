subroutine control2model_ad(rval,bval,grad)
!$$$  subprogram documentation block
!
! abstract:  Converts variables from physical space to control space
!            This is also the adjoint of control2model
!
! program history log:
!   2007-04-16  tremolet - initial code
!   2007-04-27  tremolet - multiply by sqrt(B)^T (from ckerror_ad D. Parrish)
!   2008-12-04  todling  - update interface to ckgcov_ad; add tsen and p3d
!   2008-12-29  todling  - add call to strong balance contraint
!   2009-04-21  derber   - modify call to getstvp to getuv(*,1)
!   2009-11-10  todling  - remove preditors call to mpl_addreduce
!   2010-03-15  zhu      - make changes to ckgcov_ad, use cstate
!   2010-04-29  todling  - update to use gsi_bundle
!   2010-05-31  todling  - better consistency checks; add co/co2
!                        - ready to bypass analysis of (any) meteorological fields
!   2010-06-15  todling  - generalized handling of chemistry
!   2011-05-15  auligne/todling - generalized cloud handling
!   2011-07-12  zhu      - add cw_to_hydro_ad and cw2hydro_ad
!   2011-12-14  mkim     - changed clouds4crtm to clouds in metguess
!   2012-06-12  parrish  - modify bundle wbundle so motley variables are included if available.  The
!                          subroutine ckgcov_ad now looks for motley variables as part of the input bundle.
!   2012-06-12  parrish  - remove nnnn1o--no longer needed.
!   2012-10-09  wgu      - update interface to tbalance (fut2ps)
!   2013-10-25  todling  - nullify work pointers
!   2013-10-28  todling  - rename p3d to prse
!   2013-05-23  zhu      - add ntclen and predt for aircraft temperature bias correction
!
!   input argument list:
!     rval - State variable
!   output argument list:
!     grad - Control variable
!
!$$$
use kinds, only: r_kind,i_kind
use constants, only: zero,max_varname_length
use control_vectors, only: control_vector
use control_vectors, only: cvars3d,cvars2d,cvarsmd
use control_vectors, only: nc2d,nc3d,mvars
use bias_predictors, only: predictors
use gsi_4dvar, only: nsubwin, lsqrtb
use gridmod, only: lat2,lon2,nsig
use berror, only: varprd,fpsproj,fut2ps
use balmod, only: tbalance
use jfunc, only: nsclen,npclen,ntclen,nrclen,nval_lenz
use cwhydromod, only: cw2hydro_ad
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_gridcreate
use gsi_bundlemod, only: gsi_grid
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundleputvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_chemguess_mod, only: gsi_chemguess_get
use gsi_metguess_mod, only: gsi_metguess_get
use mpeu_util, only: getindex
implicit none

! Declare passed variables
type(gsi_bundle)    ,intent(inout) :: rval(nsubwin)
type(predictors)    ,intent(in   ) :: bval
type(control_vector),intent(inout) :: grad

! Declare local variables
character(len=*),parameter::myname='control2model_ad'
character(len=10),allocatable,dimension(:) :: gases
character(len=max_varname_length),allocatable,dimension(:) :: clouds
real(r_kind),dimension(lat2,lon2,nsig) :: workst,workvp,workrh
integer(i_kind) :: ii,jj,i,ic,id,ngases,nclouds,istatus
real(r_kind) :: gradz(nval_lenz)
type(gsi_bundle) :: wbundle
type(gsi_grid)   :: grid

! Note: The following does not aim to get all variables in
!       the state and control vectors, but rather the ones
!       explicitly needed by this routine.
! Declare required local state variables
logical :: ls_u,ls_v,ls_prse,ls_q,ls_tsen,ls_tv,ls_ps,ls_ql,ls_qi
integer(i_kind), parameter :: nsvars = 9
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &
                               'u   ', 'v   ', 'prse', 'q   ', 'tsen',   &
                               'tv  ', 'ps  ','ql  ', 'qi  ' /)
character(len=max_varname_length),allocatable,dimension(:) :: cvars2dpm  ! names of 2d fields including
                                                                         !  motley vars (if any)

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
real(r_kind),pointer,dimension(:,:)   :: rv_rank2=>NULL()

logical :: do_getuv,do_tv_to_tsen_ad,do_normal_rh_to_q_ad,do_getprs_ad,do_tbalance,cw_to_hydro_ad

!******************************************************************************

if (.not.lsqrtb) then
   write(6,*)trim(myname),': assumes lsqrtb'
   call stop2(146)
end if

!  create extended list of 2d variable names to include motley variables.
!     NOTE: if mvars=0, there are no motley variables so cvars2dpm=cvars2d.
   allocate(cvars2dpm(nc2d+mvars))
   do i=1,nc2d
      cvars2dpm(i)=cvars2d(i)
   end do
   do i=1,mvars
      cvars2dpm(nc2d+i)=cvarsmd(i)
   end do
   call gsi_gridcreate(grid,lat2,lon2,nsig)

! Inquire about chemistry
call gsi_chemguess_get('dim',ngases,istatus)
if (ngases>0) then
    allocate(gases(ngases))
    call gsi_chemguess_get('gsinames',gases,istatus)
endif

! Since each internal vector of xhat has the same structure, pointers are
! the same independent of the subwindow jj
call gsi_bundlegetpointer (rval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_tv =isps(6)>0
ls_ps =isps(7)>0; ls_ql  =isps(8)>0; ls_qi =isps(9)>0

! Define what to do depending on what's in SV and
! what's explictly needed in this routine
do_getuv            =ls_u .and.ls_v
do_tv_to_tsen_ad    =ls_tv.and.ls_q  .and.ls_tsen
do_normal_rh_to_q_ad=ls_tv.and.ls_prse.and.ls_q
do_getprs_ad        =ls_ps.and.ls_tv .and.ls_prse
do_tbalance         =ls_tv.and.ls_ps

! Inquire about clouds-variables
cw_to_hydro_ad=.false.
call gsi_metguess_get ('clouds::3d',nclouds,istatus)
if (nclouds>0) then
   allocate(clouds(nclouds))
   call gsi_metguess_get ('clouds::3d',clouds,istatus)
   if (getindex(cvars3d,'cw')>0 .and. ls_ql .and. ls_qi) cw_to_hydro_ad=.true.
end if

! Loop over control steps
do jj=1,nsubwin

   workst(:,:,:)=zero
   workvp(:,:,:)=zero
   workrh(:,:,:)=zero

!  Get pointers to require state variables
   call gsi_bundlegetpointer (rval(jj),'u'   ,rv_u,   istatus)
   call gsi_bundlegetpointer (rval(jj),'v'   ,rv_v,   istatus)
   call gsi_bundlegetpointer (rval(jj),'ps'  ,rv_ps,  istatus)
   call gsi_bundlegetpointer (rval(jj),'prse',rv_prse,istatus)
   call gsi_bundlegetpointer (rval(jj),'tv'  ,rv_tv,  istatus)
   call gsi_bundlegetpointer (rval(jj),'tsen',rv_tsen,istatus)
   call gsi_bundlegetpointer (rval(jj),'q'   ,rv_q ,  istatus)
   call gsi_bundlegetpointer (rval(jj),'oz'  ,rv_oz , istatus)
   call gsi_bundlegetpointer (rval(jj),'sst' ,rv_sst, istatus)

!  Convert RHS calculations for u,v to st/vp for application of
!  background error
   if(do_getuv) call getuv(rv_u,rv_v,workst,workvp,1)

!  Calculate sensible temperature
   if(do_tv_to_tsen_ad) call tv_to_tsen_ad(rv_tv,rv_q,rv_tsen)

!  Adjoint of convert input normalized RH to q to add contribution of moisture
!  to t, p , and normalized rh
   if(do_normal_rh_to_q_ad) call normal_rh_to_q_ad(workrh,rv_tv,rv_prse,rv_q)

!  Adjoint to convert ps to 3-d pressure
   if(do_getprs_ad) call getprs_ad(rv_ps,rv_tv,rv_prse)

!  Multiply by sqrt of background error adjoint (ckerror_ad)
!  -----------------------------------------------------------------------------

!  Transpose of balance equation
   if(do_tbalance) call tbalance(rv_tv,rv_ps,workst,workvp,fpsproj,fut2ps)

!  Apply variances, as well as vertical & horizontal parts of background error
   gradz(:)=zero

!  create an internal structure w/ the same vars as those in the control vector, including motley vars
   call gsi_bundlecreate (wbundle,grid,'control2model_ad work',istatus,names2d=cvars2dpm,names3d=cvars3d)
   if (istatus/=0) then
      write(6,*)trim(myname),': trouble creating work bundle'
      call stop2(999)
   endif

!  Adjoint of control to initial state
   call gsi_bundleputvar ( wbundle, 'sf', workst, istatus )
   call gsi_bundleputvar ( wbundle, 'vp', workvp, istatus )
   call gsi_bundleputvar ( wbundle, 't' , rv_tv,  istatus )
   call gsi_bundleputvar ( wbundle, 'q' , workrh, istatus )
   call gsi_bundleputvar ( wbundle, 'ps', rv_ps,  istatus )
   call gsi_bundleputvar ( wbundle, 'oz', rv_oz,  istatus )
   call gsi_bundleputvar ( wbundle, 'sst',rv_sst, istatus )

   if (nclouds>0) then
      if (cw_to_hydro_ad) then
!        Case when cw is generated from hydrometeors
         call cw2hydro_ad(rval(jj),wbundle,clouds,nclouds)
      else
!        Case when cloud-vars map one-to-one, take care of them together
         do ic=1,nclouds
            id=getindex(cvars3d,clouds(ic))
            if (id>0) then
               call gsi_bundlegetpointer (rval(jj),clouds(ic),rv_rank3,istatus)
               call gsi_bundleputvar     (wbundle, clouds(ic),rv_rank3,istatus)
            endif
         enddo
      end if
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

!  Apply adjoint of sqrt-B
   call ckgcov_ad(gradz,wbundle,nval_lenz)

!  Clean up
   call gsi_bundledestroy(wbundle,istatus)
   if (istatus/=0) then
      write(6,*)trim(myname),': trouble destroying work bundle'
      call stop2(999)
   endif

   do ii=1,nval_lenz
      grad%step(jj)%values(ii)=grad%step(jj)%values(ii)+gradz(ii)
   enddo

! -----------------------------------------------------------------------------

end do

! Bias predictors are duplicated
do ii=1,nsclen
   grad%predr(ii)=grad%predr(ii)+bval%predr(ii)*sqrt(varprd(ii))
enddo
do ii=1,npclen
   grad%predp(ii)=grad%predp(ii)+bval%predp(ii)*sqrt(varprd(nsclen+ii))
enddo
if (ntclen>0) then 
   do ii=1,ntclen
      grad%predt(ii)=grad%predt(ii)+bval%predt(ii)*sqrt(varprd(nsclen+npclen+ii))
   enddo
end if

! Clean up
deallocate(cvars2dpm)
if (ngases>0) then
    deallocate(gases)
endif

if (nclouds>0) deallocate(clouds)

return
end subroutine control2model_ad
