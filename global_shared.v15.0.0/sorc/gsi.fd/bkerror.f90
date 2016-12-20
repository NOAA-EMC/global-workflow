subroutine bkerror(gradx,grady)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkerror  apply background error covariance            
!   prgmmr: wu               org: np22                date: 1999-12-07
!
! abstract: grid transform, apply recursive filters on conformal/Cartesian 
!            grids back to Gaussian grid.
!
! program history log:
!   2003-12-18  derber, j. bring hoper and htoper into background error calcs.
!   2004-05-15  treadon - move slndt,sst,sicet=0 up from htoper to this routine
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-08-27  kleist - background error covariance placed in single routine
!   2004-10-26  kleist - u,v removed from vector
!   2005-01-22  parrish - add module balmod to access balance and tbalance
!   2005-03-30  treadon - add more comments to periodic block
!   2006-11-30  todling - add fpsproj as arg to (t)balance routine(s)
!   2007-04-13  tremolet - use control vectors
!   2007-10-01  todling  - add timer
!   2008-12-29  todling - update interface to strong_bk/bk_ad
!   2009-04-13  derber - move strong_bk into balance
!   2010-03-01  zhu    - change bkgcov interface for generalized control variables
!                      - make changes with iterfaces of sub2grid and grid2sub
!   2010-04-28  todling - update to use gsi_bundle
!   2010-05-31  todling - revisit check on pointers
!   2010-08-19  lueken  - add only to module use
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2011-09-05  todling - made sure ckgcov still reproduces bkgcov
!   2012-06-25  parrish - replace sub2grid, grid2sub with general_sub2grid, general_grid2sub.
!                         To accomplish this, slndt, sicet removed, and motley variables are
!                         now used.  This requires the use of gsi_bundlemerge to merge together
!                         bundles grady%step(ii) and grady%motley(ii) to new temporary bundle
!                         mbundle.  If there are no motley variables (mvars<=0), then gsi_bundledup
!                         is used in place of gsi_bundlemerge.
!   2013-04-23 Pondecca - bug fix in calling gsi_bundledup
!   2012-10-09  Gu - add fut2ps to project unbalanced temp to surface pressure in static B modeling
!   2013-05-23  zhu     - add ntclen and predt for aircraft temperature bias correction
!
!   input argument list:
!     gradx    - input field  
!
!   output
!     grady    - background structure * gradx 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use berror, only: varprd,fpsproj,fut2ps
  use balmod, only: balance,tbalance
  use gsi_4dvar, only: nsubwin, lsqrtb
  use gridmod, only: lat2,lon2,nlat,nlon,periodic,latlon11
  use jfunc, only: nsclen,npclen,ntclen
  use jfunc, only: set_sqrt_2dsize
  use constants, only:  zero
  use control_vectors, only: control_vector,assignment(=)
  use control_vectors, only: mvars,nrf,nrf_var,nrf_3d,cvarsmd
  use timermod, only: timer_ini,timer_fnl
  use gsi_bundlemod, only: gsi_bundlegetpointer,gsi_bundlemerge,gsi_bundle,gsi_bundledup,gsi_bundledestroy
  use general_sub2grid_mod, only: general_sub2grid,general_grid2sub
  use general_commvars_mod, only: s2g_raf,s2g_cv
  implicit none

! Declare passed variables
  type(control_vector),intent(inout) :: gradx
  type(control_vector),intent(inout) :: grady

! Declare local variables
  integer(i_kind) i,ii
  integer(i_kind) i_t,i_p,i_st,i_vp
  integer(i_kind) ipnts(4),istatus
! integer(i_kind) nval_lenz,ndim2d
  real(r_kind),dimension(nlat*nlon*s2g_cv%nlevs_alloc)::workcv
  real(r_kind),pointer,dimension(:,:,:):: p_t  =>NULL()
  real(r_kind),pointer,dimension(:,:,:):: p_st =>NULL()
  real(r_kind),pointer,dimension(:,:,:):: p_vp =>NULL()
  real(r_kind),pointer,dimension(:,:)  :: p_ps =>NULL()
  real(r_kind),pointer::rank2a(:,:)  =>NULL()
  real(r_kind),pointer::rank2b(:,:)  =>NULL()
  real(r_kind),pointer::rank3a(:,:,:)=>NULL()
  real(r_kind),pointer::rank3b(:,:,:)=>NULL()
  logical dobal
! real(r_kind),allocatable,dimension(:):: gradz
  type(gsi_bundle) :: mbundle

  if (lsqrtb) then
     write(6,*)'bkerror: not for use with lsqrtb'
     call stop2(317)
  end if

! Initialize timer
  call timer_ini('bkerror')

! If dealing with periodic (sub)domain, gather full domain grids,
! account for periodicity, and redistribute to subdomains.  This
! only needs to be done when running with a single mpi task and
! then only for array gradx.
  if (periodic) then
     do ii=1,nsubwin
        call general_sub2grid(s2g_cv,gradx%step(ii)%values,workcv)
        call general_grid2sub(s2g_cv,workcv,gradx%step(ii)%values)
     end do
  endif

! Put things in grady first since operations change input variables
  grady=gradx

! Only need to get pointer for ii=1 - all other are the same
  call gsi_bundlegetpointer ( grady%step(1), (/'t ','sf','vp','ps'/), &
                              ipnts, istatus )
  i_t  = ipnts(1)
  i_st = ipnts(2)
  i_vp = ipnts(3)
  i_p  = ipnts(4)
  dobal = i_t>0.and.i_p>0.and.i_st>0.and.i_vp>0

! Loop on control steps
  do ii=1,nsubwin

!    Create temporary bundle which merges grady%step(ii) with grady%motley(ii)
     if(mvars>0) then
        call gsi_bundlemerge(mbundle,grady%step(ii),grady%motley(ii),' add motley to step',istatus)
     else
        call gsi_bundledup(grady%step(ii),mbundle,' copy of step ',istatus) 
     end if

!    Transpose of balance equation
     if(dobal) then
        call gsi_bundlegetpointer ( mbundle,'t' ,p_t ,istatus )
        call gsi_bundlegetpointer ( mbundle,'sf',p_st,istatus )
        call gsi_bundlegetpointer ( mbundle,'vp',p_vp,istatus )
        call gsi_bundlegetpointer ( mbundle,'ps',p_ps,istatus )
        call tbalance(p_t,p_ps,p_st,p_vp,fpsproj,fut2ps)
     endif

!    Apply variances, as well as vertical & horizontal parts of background error
     call bkgcov(mbundle)

!    The following lines test that indeed proper application of cgkcov
!    reproduces results of bkgcov - left as comments (please do not remove
!    as this tends to break from time to time).
!    Last tested Jun 9, 2012: correct to within roundoff
!
!    call set_sqrt_2dsize(ndim2d)
!    nval_lenz=ndim2d*s2g_raf%nlevs_alloc
!    allocate(gradz(nval_lenz))
!    gradz=zero
!    call ckgcov_ad(gradz,mbundle,nval_lenz)
!    call ckgcov   (gradz,mbundle,nval_lenz)
!    deallocate(gradz)

!    Balance equation
     if(dobal) call balance(p_t,p_ps,p_st,p_vp,fpsproj,fut2ps)

!    Transfer step part of mbundle back to grady%step(ii)
     do i=1,nrf
        if(nrf_3d(i)) then
           call gsi_bundlegetpointer(mbundle,trim(nrf_var(i)),rank3a,istatus)
           call gsi_bundlegetpointer(grady%step(ii),trim(nrf_var(i)),rank3b,istatus)
           rank3b=rank3a
        else
           call gsi_bundlegetpointer(mbundle,trim(nrf_var(i)),rank2a,istatus)
           call gsi_bundlegetpointer(grady%step(ii),trim(nrf_var(i)),rank2b,istatus)
           rank2b=rank2a
        end if
     end do

  end do

! clean work space
  call gsi_bundledestroy(mbundle,istatus)
  if(istatus/=0) then
     write(6,*) ' in bkerror: trouble destroying work mbundle'
     call stop2(999)
  endif

! Take care of background error for bias correction terms
  do i=1,nsclen
     grady%predr(i)=grady%predr(i)*varprd(i)
  end do
  do i=1,npclen
     grady%predp(i)=grady%predp(i)*varprd(nsclen+i)
  end do
  if (ntclen>0) then
     do i=1,ntclen
        grady%predt(i)=grady%predt(i)*varprd(nsclen+npclen+i)
     end do
  end if

! Finalize timer
  call timer_fnl('bkerror')

  return
end subroutine bkerror
