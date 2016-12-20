subroutine control2model(xhat,sval,bval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    control2model
!   prgmmr: tremolet
!
! abstract:  Converts control variable to physical space
!
! program history log:
!   2007-04-13  tremolet - initial code
!   2007-04-27  tremolet - multiply by sqrt(B) (from ckerror D. Parrish)
!   2008-12-04  todling  - update interface to ckgcov; add tsen/p3d
!   2008-12-29  todling  - add call to strong balance contraint
!   2009-04-21  derber   - modify call to getuv to getuv(*,0)
!   2009-08-14  lueken   - update documentation
!   2010-03-15  zhu      - make changes to ckbcov, add assign_cs2array
!   2010-04-28  todling  - update to use gsi_bundle
!   2010-05-31  todling  - better consistency checks; add co/co2
!                        - ready to bypass analysis of (any) meteorological fields
!   2010-06-15  todling  - generalized handling of chemistry
!   2011-05-15  auligne/todling - generalized cloud handling
!   2011-07-12  zhu      - add cw_to_hydro and cwhydromod
!   2011-12-14  mkim     - changed clouds4crtm to clouds in metguess 
!   2012-06-25  parrish  - modify wbundle by adding motley variables to control vector
!                          so will be in form expected by new version of ckgcov which uses general_grid2sub.
!   2012-10-09  wgu      - update interface to tbalance (fut2ps)
!   2013-10-25  todling  - nullify work pointers
!   2013-10-28  todling  - rename p3d to prse
!   2013-05-23   zhu     - add ntclen and predt for aircraft temperature bias correction
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
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
use kinds, only: r_kind,i_kind
use control_vectors, only: control_vector
use control_vectors, only: cvars3d,cvars2d,cvarsmd
use control_vectors, only: nc2d,nc3d,mvars
use bias_predictors, only: predictors
use gsi_4dvar, only: nsubwin, l4dvar, lsqrtb
use gridmod, only: lat2,lon2,nsig,nnnn1o
use jfunc, only: nsclen,npclen,ntclen
use berror, only: varprd,fpsproj,fut2ps
use balmod, only: balance
use cwhydromod, only: cw2hydro_tl
use gsi_bundlemod, only: gsi_bundlecreate
use gsi_bundlemod, only: gsi_gridcreate
use gsi_bundlemod, only: gsi_grid
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: gsi_bundlegetpointer
use gsi_bundlemod, only: gsi_bundlegetvar
use gsi_bundlemod, only: gsi_bundledestroy
use gsi_chemguess_mod, only: gsi_chemguess_get
use gsi_metguess_mod, only: gsi_metguess_get
use mpeu_util, only: getindex
use constants, only: max_varname_length
implicit none
  
! Declare passed variables  
type(control_vector), intent(in   ) :: xhat
type(gsi_bundle)    , intent(inout) :: sval(nsubwin)
type(predictors)    , intent(inout) :: bval

! Declare local variables  	
character(len=*),parameter:: myname ='control2model'
real(r_kind),dimension(lat2,lon2,nsig) :: workst,workvp,workrh
type(gsi_bundle) :: wbundle
type(gsi_grid)   :: grid
integer(i_kind) :: ii,jj,i,ic,id,ngases,nclouds,istatus
character(len=10),allocatable,dimension(:) :: gases
character(len=max_varname_length),allocatable,dimension(:) :: clouds

! Note: The following does not aim to get all variables in
!       the state and control vectors, but rather the ones
!       explicitly needed by this routine.
! Declare required local state variables
logical :: ls_u,ls_v,ls_prse,ls_q,ls_tsen,ls_tv,ls_ps,ls_ql,ls_qi
integer(i_kind), parameter :: nsvars = 9
integer(i_kind) :: isps(nsvars)
character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from SV needed here
        'u   ', 'v   ', 'prse', 'q   ', 'tsen',  'tv  ', 'ps  ','ql  ', 'qi  ' /)
character(len=max_varname_length),allocatable,dimension(:) :: cvars2dpm  ! names of 2d fields including
                                                                         !  motley vars (if any)
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
real(r_kind),pointer,dimension(:,:)   :: sv_rank2=>NULL()

logical :: do_balance,do_getprs_tl,do_normal_rh_to_q,do_tv_to_tsen,do_getuv,cw_to_hydro

!******************************************************************************

if (.not.lsqrtb) then
   write(6,*)trim(myname),': assumes lsqrtb'
   call stop2(104)
end if
if (nsubwin/=1 .and. .not.l4dvar) then
   write(6,*)trim(myname),': error 3dvar',nsubwin
   call stop2(105)
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
call gsi_bundlegetpointer (sval(1),mysvars,isps,istatus)
ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
ls_q  =isps(4)>0; ls_tsen=isps(5)>0; ls_tv =isps(6)>0
ls_ps =isps(7)>0; ls_ql  =isps(8)>0; ls_qi =isps(9)>0

! Define what to do depending on what's in SV and 
! what's explictly needed in this routine
do_balance       =ls_tv.and.ls_ps
do_getprs_tl     =ls_ps.and.ls_tv .and.ls_prse
do_normal_rh_to_q=ls_tv.and.ls_prse.and.ls_q
do_tv_to_tsen    =ls_tv.and.ls_q  .and.ls_tsen
do_getuv         =ls_u .and.ls_v

! Inquire about cloud-variables
cw_to_hydro=.false.
call gsi_metguess_get ('clouds::3d',nclouds,istatus)
if (nclouds>0) then
   allocate(clouds(nclouds))
   call gsi_metguess_get ('clouds::3d',clouds,istatus)
   if (getindex(cvars3d,'cw')>0 .and. ls_ql .and. ls_qi) cw_to_hydro=.true.
end if

! Loop over control steps
do jj=1,nsubwin

!  create an internal structure w/ the same vars as those in the control vector, including motley vars
   call gsi_bundlecreate (wbundle,grid,'control2model work',istatus,names2d=cvars2dpm,names3d=cvars3d)
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble creating work bundle'
      call stop2(999)
   endif

!  Multiply by sqrt of background error (ckerror)
!  -----------------------------------------------------------------------------
!  Apply sqrt of variance, as well as vertical & horizontal parts of background
!  error

   call ckgcov(xhat%step(jj)%values(:),wbundle,size(xhat%step(jj)%values(:)))

!  Get pointers to required state variables
   call gsi_bundlegetpointer (sval(jj),'u'   ,sv_u,   istatus)
   call gsi_bundlegetpointer (sval(jj),'v'   ,sv_v,   istatus)
   call gsi_bundlegetpointer (sval(jj),'ps'  ,sv_ps,  istatus)
   call gsi_bundlegetpointer (sval(jj),'prse',sv_prse,istatus)
   call gsi_bundlegetpointer (sval(jj),'tv'  ,sv_tv,  istatus)
   call gsi_bundlegetpointer (sval(jj),'tsen',sv_tsen,istatus)
   call gsi_bundlegetpointer (sval(jj),'q'   ,sv_q ,  istatus)
   call gsi_bundlegetpointer (sval(jj),'oz'  ,sv_oz , istatus)
   call gsi_bundlegetpointer (sval(jj),'sst' ,sv_sst, istatus)

!  Copy variables from CV to SV
   call gsi_bundlegetvar ( wbundle, 'sf' , workst, istatus )
   call gsi_bundlegetvar ( wbundle, 'vp' , workvp, istatus )
   call gsi_bundlegetvar ( wbundle, 'q'  , workrh, istatus )
   call gsi_bundlegetvar ( wbundle, 't'  , sv_tv,  istatus )
   call gsi_bundlegetvar ( wbundle, 'oz' , sv_oz,  istatus )
   call gsi_bundlegetvar ( wbundle, 'ps' , sv_ps,  istatus )
   call gsi_bundlegetvar ( wbundle, 'sst', sv_sst, istatus )

!  Same one-to-one map for chemistry-vars; take care of them together
   do ic=1,ngases
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

!  Balance equation
   if(do_balance) call balance(sv_tv,sv_ps,workst,workvp,fpsproj,fut2ps)

!  -----------------------------------------------------------------------------

!  Get 3d pressure
   if(do_getprs_tl) call getprs_tl(sv_ps,sv_tv,sv_prse)

!  Convert input normalized RH to q
   if(do_normal_rh_to_q) call normal_rh_to_q(workrh,sv_tv,sv_prse,sv_q)

!  Calculate sensible temperature
   if(do_tv_to_tsen) call tv_to_tsen(sv_tv,sv_q,sv_tsen)

!  Convert streamfunction and velocity potential to u,v
   if(do_getuv) call getuv(sv_u,sv_v,workst,workvp,0)

   if (nclouds>0) then
      if (cw_to_hydro) then
!        Case when cw is generated from hydrometeors
         call cw2hydro_tl(sval(jj),wbundle,clouds,nclouds)
      else
!        Case when cloud-vars map one-to-one, take care of them together
         do ic=1,nclouds
            id=getindex(cvars3d,clouds(ic))
            if (id>0) then
                call gsi_bundlegetpointer (sval(jj),clouds(ic),sv_rank3,istatus)
                call gsi_bundlegetvar     (wbundle, clouds(ic),sv_rank3,istatus)
            endif
         enddo
      end if
   end if

!  destroy temporary bundle
   call gsi_bundledestroy(wbundle,istatus)
   if(istatus/=0) then
      write(6,*) trim(myname), ': trouble destroying work bundle'
      call stop2(999)
   endif

end do

! Bias correction terms
do ii=1,nsclen
   bval%predr(ii)=xhat%predr(ii)*sqrt(varprd(ii))
enddo

do ii=1,npclen
   bval%predp(ii)=xhat%predp(ii)*sqrt(varprd(nsclen+ii))
enddo

if (ntclen>0) then
   do ii=1,ntclen
      bval%predt(ii)=xhat%predt(ii)*sqrt(varprd(nsclen+npclen+ii))
   enddo
end if

! Clean up
if (ngases>0) then
    deallocate(gases)
endif

if (nclouds>0) deallocate(clouds)

return
end subroutine control2model
