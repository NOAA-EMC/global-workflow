subroutine bkgvar(cvec,iflg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkgvar      apply background error variances
!   prgmmr: parrish          org: np22                date: 1990-10-06
!
! abstract: apply latitudinal background error variances & manipulate
!           skin temp <--> sst,sfc temp, and ice temp fields
!
! program history log:
!   1990-10-06  parrish
!   2004-08-24  kleist - hoper & htoper replaced
!   2004-11-16  treadon - add longitude dimension to variance array dssv
!   2004-11-22  derber - modify for openMP
!   2005-01-22  parrish - add "use balmod"
!   2005-07-14  wu - add max bound to l2
!   2007-03-13  derber - modify to allow use qvar3d array
!   2007-07-03  kleist - add full 2d error array for surface pressure (global only)
!   2007-11-26  s.liu - correct bug in water point skin temperature variances
!   2010-03-01  zhu   - replace explicit use of each control variable by one array
!                       'cstate' and use nrf* for generalized control variable
!                     - merge global and regional cases
!   2010-05-06  todling - use gsi_bundle
!   2010-06-03  todling - protection for mvars<2
!   2010-07-07  todling - rename cstate to cvec for clarity
!   2011-06-29  todling - no explict reference to internal bundle arrays
!   2012-06-25  parrish - remove sst,slndt,sict as input/output arrays.  They are now contained
!                         in the input bundle cvec, using motley variables.
!   2012-06-25  parrish - remove integer constants izero,ione.
!   2013-10-28  todling - rename p3d to prse
!
!   input argument list:
!     t        - t grid values
!     p        - p surface grid values
!     q        - q grid values
!     oz       - ozone grid values
!     skint    - skin temperature grid values
!     cwmr     - cloud water mixing ratio grid values
!     st       - streamfunction grid values
!     vp       - velocity potential grid values
!     sst      - sst grid values
!     slndt    - land surface temperature grid values
!     sicet    - snow/ice covered surface temperature grid values
!     iflg     - flag for skin temperature manipulation
!                0: skint --> sst,slndt,sicet
!                1: sst,slndt,sicet --> skint
!
!   output argument list:
!     t        - t grid values
!     p        - p surface grid values
!     q        - q grid values
!     oz       - ozone grid values
!     skint    - skin temperature grid values
!     cwmr     - cloud water mixing ratio grid values
!     st       - streamfunction grid values
!     vp       - velocity potential grid values
!     sst      - sst grid values
!     slndt    - land surface temperature grid values
!     sicet    - snow/ice covered surface temperature grid values
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  use berror, only: dssv,dssvs
  use gridmod, only: nsig,lat2,lon2
  use guess_grids, only: isli2
  use gsi_bundlemod, only : gsi_bundle
  use gsi_bundlemod, only : gsi_bundlegetpointer
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: iflg
  type(gsi_bundle),intent(inout) :: cvec

! Declare local variables
  integer(i_kind) i,j,k,n,i_sst,i_stl,i_sti,istatus
  real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()
  real(r_kind),pointer,dimension(:,:)  ::ptr2d=>NULL()
  real(r_kind),pointer,dimension(:,:)  ::ptrsst=>NULL()
  real(r_kind),pointer,dimension(:,:)  ::ptrstl=>NULL()
  real(r_kind),pointer,dimension(:,:)  ::ptrsti=>NULL()
  real(r_kind),dimension(lat2,lon2) :: sst

! Multipy by variances
!$omp parallel do  schedule(dynamic,1) private(n,k,i,j,ptr3d,istatus)
  do n=1,cvec%n3d   ! _RT: must map dssv to this (assumes same order)
     call gsi_bundlegetpointer ( cvec,cvec%r3(n)%shortname,ptr3d,istatus )
     do k=1,nsig
        do i=1,lon2
           do j=1,lat2
              ptr3d(j,i,k)  =ptr3d(j,i,k)*dssv(j,i,k,n)
           end do
        enddo
     enddo
  end do

! Get pointer for SST
  call gsi_bundlegetpointer(cvec,'sst',i_sst,istatus)
  call gsi_bundlegetpointer(cvec,'stl',i_stl,istatus)
  call gsi_bundlegetpointer(cvec,'sti',i_sti,istatus)
  call gsi_bundlegetpointer(cvec,'sst',ptrsst,istatus)
  call gsi_bundlegetpointer(cvec,'stl',ptrstl,istatus)
  call gsi_bundlegetpointer(cvec,'sti',ptrsti,istatus)
  if(iflg == 0)then
     if(i_sst > 0)sst=zero
     if(i_stl > 0)ptrstl=zero
     if(i_sti > 0)ptrsti=zero
  end if

! Surface fields
!     !$omp parallel do  schedule(dynamic,1) private(n,i,j,ptr2d,istatus)
  do n=1,cvec%n2d
     if(n/=i_sst.and.n/=i_stl.and.n/=i_sti) then
        call gsi_bundlegetpointer(cvec,cvec%r2(n)%shortname,ptr2d,istatus)
        do i=1,lon2
           do j=1,lat2
              ptr2d(j,i)=ptr2d(j,i)*dssvs(j,i,n)
           end do
        end do
     elseif(i_sst>0) then
        if(iflg==0) then
           if(n==i_sst) then
              do i=1,lon2
                 do j=1,lat2
                    if(isli2(j,i)/=1.and.isli2(j,i)/=2) sst(j,i)=ptrsst(j,i)*dssvs(j,i,n)
                 end do
              end do
           elseif(n==i_stl) then
              do i=1,lon2
                 do j=1,lat2
                    if(isli2(j,i) == 1) ptrstl(j,i)=ptrsst(j,i)*dssvs(j,i,n)
                 end do
              end do
           elseif(n==i_sti) then
              do i=1,lon2
                 do j=1,lat2
                    if(isli2(j,i) == 2) ptrsti(j,i)=ptrsst(j,i)*dssvs(j,i,n)
                 end do
              end do
           end if
        else
           if(n==i_sst) then
              do i=1,lon2
                 do j=1,lat2
                    if(isli2(j,i)/=1.and.isli2(j,i)/=2) ptrsst(j,i)=ptrsst(j,i)*dssvs(j,i,n)
                 end do
              end do
           elseif(n==i_stl) then
              do i=1,lon2
                 do j=1,lat2
                    if(isli2(j,i) == 1) ptrsst(j,i)=ptrstl(j,i)*dssvs(j,i,n)
                 end do
              end do
           elseif(n==i_sti) then
              do i=1,lon2
                 do j=1,lat2
                    if(isli2(j,i) == 2) ptrsst(j,i)=ptrsti(j,i)*dssvs(j,i,n)
                 end do
              end do
           end if
        end if
     end if
     
  end do
  if(iflg == 0 .and. i_sst>0) ptrsst=sst

  
  return
end subroutine bkgvar

subroutine bkg_stddev(cvec,svec)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bkg_variances      apply background error variances
!   prgmmr: el akkraoui          org: gmao              date: 2010-06-05
!
! abstract: retrieve background error standard deviations including 
!           flow dependent part
!
! program history log:
!   2010-06-05  el akkraoui
!   2010-07-08  todling - revisit original code
!   2013-01-12  parrish - remove sst,slndt,sicet -- no longer needed when calling bkgvar.
!                             also now an argument conflict only caught with more sophisticated
!                             debug tools turned on, since bkgvar not in a module.
!
!   input argument list:
!     cvec - allocated bundle in control space
!     svec - allocated bundle in state   space
!
!   output argument list:
!     cvec - bundle holding standard deviations in control space
!     svec - bundle holding standard deviations in state   space
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use mpimod, only : mype
  use constants, only: one,zero
  use berror, only: bkgv_flowdep
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_bundlemod, only: assignment(=)

  implicit none
  
! Declare passed variables  
  type(gsi_bundle), intent(inout) :: cvec
  type(gsi_bundle), intent(inout) :: svec

! Declare local variables  	
  integer(i_kind) :: istatus
  real(r_kind),pointer,dimension(:,:)   :: cv_ps
  real(r_kind),pointer,dimension(:,:,:) :: cv_t,cv_sf,cv_vp,cv_rh
  real(r_kind),pointer,dimension(:,:,:) :: sv_tsen,sv_u,sv_v,sv_q,sv_prse
  logical do_flow_dep,do_getprs_tl,do_normal_rh_to_q,do_tv_to_tsen,do_getuv

! Declare required local control variables
  integer(i_kind), parameter :: ncvars = 5
  integer(i_kind) :: icps(ncvars)
  character(len=3), parameter :: mycvars(ncvars) = (/  &  ! vars from CV needed here
                               'sf ', 'vp ', 'ps ', 't  ', 'q  '/)
  logical lc_sf,lc_vp,lc_t,lc_ps,lc_rh

! Declare required local state variables
  integer(i_kind), parameter :: nsvars = 5
  integer(i_kind)            :: isps(nsvars)
  character(len=4), parameter :: mysvars(nsvars) = (/  &  ! vars from ST needed here
                               'u   ', 'v   ', 'prse', 'q   ', 'tsen' /)
  logical ls_u,ls_v,ls_tsen,ls_prse,ls_q

! Check presence of fields in control bundle
  call gsi_bundlegetpointer (cvec,mycvars,icps,istatus)
  lc_sf =icps(1)>0; lc_vp =icps(2)>0; lc_ps =icps(3)>0
  lc_t  =icps(4)>0; lc_rh =icps(5)>0

! Check presence of fields in state bundle
  call gsi_bundlegetpointer (svec,mysvars,isps,istatus)
  ls_u  =isps(1)>0; ls_v   =isps(2)>0; ls_prse=isps(3)>0
  ls_q  =isps(4)>0; ls_tsen=isps(5)>0

! Determine what to do given what's available
  do_flow_dep      =lc_sf.and.lc_vp.and.lc_t.and.lc_ps
  do_getprs_tl     =lc_ps.and.lc_t .and.ls_prse
  do_normal_rh_to_q=lc_rh.and.lc_t .and.ls_prse.and.ls_q
  do_tv_to_tsen    =lc_t .and.ls_q .and.ls_tsen
  do_getuv         =lc_sf.and.lc_vp.and.ls_u.and.ls_v

  cvec =one


! Get standard deviations (why is this called bkgvar?) in control space
!  bkgvar only applies sqrt of variance each time it is called.
!  Normally, in bkgcov, it is called once before correlations are applied, and
!  again after.  Perhaps it should be called bkgstd, but this subroutine is
!  called bkg_stddev.
  call bkgvar(cvec,1)

  call gsi_bundlegetpointer (cvec,'sf',cv_sf,istatus)
  call gsi_bundlegetpointer (cvec,'vp',cv_vp,istatus)
  call gsi_bundlegetpointer (cvec,'t' ,cv_t ,istatus)
  call gsi_bundlegetpointer (cvec,'ps',cv_ps,istatus)
  call gsi_bundlegetpointer (cvec,'q' ,cv_rh,istatus)

  call gsi_bundlegetpointer (svec,'u'   ,sv_u    ,istatus)
  call gsi_bundlegetpointer (svec,'v'   ,sv_v    ,istatus)
  call gsi_bundlegetpointer (svec,'tsen',sv_tsen ,istatus)
  call gsi_bundlegetpointer (svec,'prse',sv_prse ,istatus)
  call gsi_bundlegetpointer (svec,'q'   ,sv_q    ,istatus)

! Add flow dependent part to standard deviations
  if(bkgv_flowdep) then
      if(do_flow_dep) call bkgvar_rewgt(cv_sf,cv_vp,cv_t,cv_ps,mype)
  endif

!  Get 3d pressure
   if(do_getprs_tl) call getprs_tl(cv_ps,cv_t,sv_prse)

!  Convert input normalized RH to q
   if(do_normal_rh_to_q) call normal_rh_to_q(cv_rh,cv_t,sv_prse,sv_q)
   
!  Calculate sensible temperature
   if(do_tv_to_tsen) call tv_to_tsen(cv_t,sv_q,sv_tsen)

!  Convert streamfunction and velocity potential to u and v
   if(do_getuv) then
      call getuv(sv_u,sv_v,cv_sf,cv_vp,0)
   end if

!  TO BE DONE: handle the rest of CV and SV fields

end subroutine bkg_stddev
