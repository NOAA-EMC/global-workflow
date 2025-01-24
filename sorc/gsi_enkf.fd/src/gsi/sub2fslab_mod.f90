module sub2fslab_mod
!$$$ module documentation block
!           .      .    .                                       .
! module:   sub2fslab_mod
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: contains stuffs to convert the subdomain data
!           into filter-space slab data.
!
! program history log:
!   2008-01-23  sato
!   2010-03-25  zhu   - change work_* from arrays to a data structure work;
!                     - change interface of sub2grid 
!   2012-06-09  parrish - change so can use general_sub2grid
!
! subroutines included:
!   sub setup_sub2fslab
!   sub destroy_sub2fslab
!   sub sub2fslab
!   sub sub2fslab_glb
!   sub sub2fslabdz
!   sub sub2fslabdz_glb
!   sub sub2slab2d
!   sub sub2fslab2d
!   sub sub2fslab2d_glb
!
! Variable Definitions:
!   var work_*  - work array for sub2grid
!   var hfine   - work array for agrid2fgrid/grd2patch input
!   var hfilter - work array for agrid2fgrid output (for regional)
!   var hflt?   - work array for grd2patch   output (for global)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind,r_single
  use gridmod, only: nlon, nlat, & ! for slab mode
                     nsig  , lat2, lon2, & ! for subdomain
                     regional, twodvar_regional
  use anberror, only: prm0 => pf2aP1, &
                      prm2 => pf2aP2,  &
                      prm3 => pf2aP3
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlecreate
  use gsi_bundlemod, only: gsi_bundledestroy
  use gsi_bundlemod, only: gsi_grid
  use gsi_bundlemod, only: gsi_gridcreate
  use gsi_bundlemod, only: gsi_bundlemerge
  use control_vectors, only: cvars2d,cvars3d,cvarsmd,mvars
  use general_sub2grid_mod, only: general_sub2grid
  use general_commvars_mod, only: s2g_raf

  implicit none

! set default to private
  private
! set subroutines to public
  public :: setup_sub2fslab
  public :: destroy_sub2fslab
  public :: sub2fslab
  public :: sub2fslab_glb
  public :: sub2fslabdz
  public :: sub2fslabdz_glb
  public :: sub2slab2d
  public :: sub2fslab2d
  public :: sub2fslab2d_glb

  character(len=*),parameter::myname='sub2fslab_mod'

  type(gsi_bundle),save :: work

  real(r_kind)  ,allocatable,dimension(:,:,:,:)  :: hfine
  real(r_kind)  ,allocatable,dimension(:,:)    :: hfilter
  real(r_kind)  ,allocatable,dimension(:,:)    :: hflt0,hflt2,hflt3

contains
!=======================================================================
subroutine setup_sub2fslab
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_sub2fslab
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: set up work arrays
!
! program history log:
!   2008-01-23  sato
!   2010-04-28  todling - alloc/deall should always be: first in; last out
!                       - update to use gsi_bundle
!   2011-07-04  todling  - fixes to run either single or double precision
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none
  character(len=*),parameter::myname_=trim(myname)//'*setup_sub2fslab'
  integer(i_kind) :: istatus,istatall
  type(gsi_grid):: grid
  type(gsi_bundle):: cwork,mwork

! create an internal structure w/ the same vars as those in the control vector
  call gsi_gridcreate(grid,lat2,lon2,nsig)
  if(mvars>0) then
     istatall=0
     call gsi_bundlecreate (cwork,grid,'cwork',istatus, &
                         names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
     istatall=istatall+istatus
     call gsi_bundlecreate (mwork,grid,'mwork',istatus, &
                         names2d=cvarsmd,bundle_kind=r_kind)
     istatall=istatall+istatus
     call gsi_bundlemerge(work,cwork,mwork,'merge cwork and mwork',istatus)
     istatall=istatall+istatus
     if (istatall/=0) then
        write(6,*) trim(myname_),': trouble creating merged work bundle'
        call stop2(999)
     endif
     istatall=0
     call gsi_bundledestroy (cwork,istatus)
     istatall=istatall+istatus
     call gsi_bundledestroy (mwork,istatus)
     istatall=istatall+istatus
     if (istatall/=0) then
        write(6,*) trim(myname_),': trouble destroying cwork and mwork bundles'
        call stop2(999)
     endif

  else
     call gsi_bundlecreate (work,grid,'work',istatus, &
                         names2d=cvars2d,names3d=cvars3d,bundle_kind=r_kind)
     if (istatus/=0) then
        write(6,*) trim(myname_),': trouble creating work bundle--no motley variable version'
        call stop2(999)
     endif
  end if

  allocate(hfine(s2g_raf%inner_vars,nlat,nlon,s2g_raf%nlevs_alloc))

  if( regional ) then
  !  for regional mode
     allocate(hfilter(prm0%nlatf,prm0%nlonf))
  else
  !  for global mode
     allocate(hflt0(prm0%nlatf,prm0%nlonf))
     allocate(hflt2(prm2%nlatf,prm2%nlonf))
     allocate(hflt3(prm3%nlatf,prm3%nlonf))
  end if

end subroutine setup_sub2fslab
!=======================================================================
!=======================================================================
subroutine destroy_sub2fslab
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_sub2fslab
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: destroy work arrays
!
! program history log:
!   2008-01-23  sato
!   2010-04-28  todling - alloc/deall should always be: first in; last out
!                       - update to use gsi_bundle
!
!   input argument list:
!
!   output argument list:
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  implicit none
  character(len=*),parameter::myname_=trim(myname)//'*destroy_sub2fslab'
  integer(i_kind) :: istatus

  if( regional ) then
  !  for regional mode
     deallocate(hfilter)
  else
  !  for global mode
     deallocate(hflt0)
     deallocate(hflt2)
     deallocate(hflt3)
  end if

  call gsi_bundledestroy (work,istatus)
   if (istatus/=0) then
      write(6,*) trim(myname_),': trouble destroying work bundle'
      call stop2(999)
   endif


  deallocate(hfine)

end subroutine destroy_sub2fslab
!=======================================================================
!=======================================================================
subroutine sub2fslab(fsub,fslab)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2fslab
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert subdomain data into filter-space slab data
!
! program history log:
!   2008-01-23  sato
!   2010-03-25  zhu   - change work_* from arrays to a data structure work;
!                     - change interface of sub2grid 
!   2010-04-28  todling - update to use bundle
!   2011-06-29  todling - no explict reference to internal bundle arrays
!
!   input argument list:
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block\
  use fgrid2agrid_mod, only: agrid2fgrid
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  real(r_kind)  ,intent(in   ) :: fsub(lat2,lon2,nsig)
  real(r_single),intent(  out) :: fslab(prm0%nlatf,prm0%nlonf,s2g_raf%nlevs_alloc)

! Declare local variables
  integer(i_kind):: k,i,j,n,n2d,n3d,istatus
  real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ptr2d=>NULL()

  n2d=work%n2d
  n3d=work%n3d
  do n=1,n3d
     call gsi_bundlegetpointer(work,work%r3(n)%shortname,ptr3d,istatus)
     do k=1,nsig
        do i=1,lon2
           do j=1,lat2
              ptr3d(j,i,k) =fsub(j,i,k)
           end do
        end do
     end do
  end do
  do n=1,n2d
     call gsi_bundlegetpointer(work,work%r2(n)%shortname,ptr2d,istatus)
     do i=1,lon2
        do j=1,lat2
           ptr2d(j,i) =fsub(j,i,1)
        end do
     end do
  end do

  call general_sub2grid(s2g_raf,work%values,hfine)

  do k=1,s2g_raf%nlevs_loc
     call agrid2fgrid(prm0,hfine(1,1,1,k),hfilter) !analysis to filter grid
     fslab(:,:,k)=real(hfilter(:,:),r_single)
  end do

  return
end subroutine sub2fslab
!=======================================================================
!=======================================================================
subroutine sub2fslab_glb(fsub,fslb0,fslb2,fslb3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2fslab
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert subdomain data into filter-space slab data
!           for global mode
!
! program history log:
!   2008-01-23  sato
!   2010-03-25  zhu   - change work_* from arrays to a data structure work;
!                     - change interface of sub2grid 
!   2010-04-28  todling - update to use bundle
!   2011-06-29  todling - no explict reference to internal bundle arrays
!
!   input argument list:
!    fsub                 - subdomain data array
!
!   output argument list:
!    fslb0,fslb2,fslb3    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use patch2grid_mod, only: grid2patch
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  real(r_kind)  ,intent(in   ) :: fsub(lat2,lon2,nsig)
  real(r_single),intent(  out) :: fslb0(prm0%nlatf,prm0%nlonf,s2g_raf%nlevs_alloc)
  real(r_single),intent(  out) :: fslb2(prm2%nlatf,prm2%nlonf,s2g_raf%nlevs_alloc)
  real(r_single),intent(  out) :: fslb3(prm3%nlatf,prm3%nlonf,s2g_raf%nlevs_alloc)

! Declare local variables
  integer(i_kind):: k,i,j,n,n2d,n3d,istatus
  real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ptr2d=>NULL()

  n2d=work%n2d
  n3d=work%n3d
  do n=1,n3d
     call gsi_bundlegetpointer(work,work%r3(n)%shortname,ptr3d,istatus)
     do k=1,nsig
        do i=1,lon2
           do j=1,lat2
              ptr3d(j,i,k) =fsub(j,i,k)
           end do
        end do
     end do
  end do
  do n=1,n2d
     call gsi_bundlegetpointer(work,work%r2(n)%shortname,ptr2d,istatus)
     do i=1,lon2
        do j=1,lat2
           ptr2d(j,i) =fsub(j,i,1)
        end do
     end do
  end do

  call general_sub2grid(s2g_raf,work%values,hfine)

  do k=1,s2g_raf%nlevs_loc
     call grid2patch(hfine(1,1,1,k),hflt0,hflt2,hflt3) !analysis to filter grid
     fslb0(:,:,k)=real(hflt0(:,:),r_single)
     fslb2(:,:,k)=real(hflt2(:,:),r_single)
     fslb3(:,:,k)=real(hflt3(:,:),r_single)
  end do

  return
end subroutine sub2fslab_glb
!=======================================================================
!=======================================================================
subroutine sub2fslabdz(fsub,fslab)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2fslabdz
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert subdomain data into filter-space slab data with
!           vertical derivative
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: zero, one
  implicit none

! Declare passed variables
  real(r_kind)  ,intent(in   ) :: fsub(lat2,lon2,nsig)
  real(r_single),intent(  out) :: fslab(prm0%nlatf,prm0%nlonf,s2g_raf%nlevs_alloc)

! Declare local variables
  real(r_kind):: fsubdz(lat2,lon2,nsig),dzi
  integer(i_kind):: k,kp,km

  do k=1,nsig
     km=max(1,k-1)
     kp=min(nsig,k+1)
     if (twodvar_regional) then; dzi=zero
     else;                       dzi=one/real(kp-km,r_kind)
     end if
     fsubdz(:,:,k)=dzi*(fsub(:,:,kp)-fsub(:,:,km))
  end do

  call sub2fslab(fsubdz,fslab)

  return
end subroutine sub2fslabdz
!=======================================================================
!=======================================================================
subroutine sub2fslabdz_glb(fsub,fslb0,fslb2,fslb3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   sub2fslabdz
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert subdomain data into filter-space slab data with
!           vertical derivative for global mode
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    fsub                 - subdomain data array
!
!   output argument list:
!    fslb0,fslb2,fslb3    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use constants, only: one
  implicit none

! Declare passed variables
  real(r_kind)  ,intent(in   ) :: fsub(lat2,lon2,nsig)
  real(r_single),intent(  out) :: fslb0(prm0%nlatf,prm0%nlonf,s2g_raf%nlevs_alloc)
  real(r_single),intent(  out) :: fslb2(prm2%nlatf,prm2%nlonf,s2g_raf%nlevs_alloc)
  real(r_single),intent(  out) :: fslb3(prm3%nlatf,prm3%nlonf,s2g_raf%nlevs_alloc)

! Declare local variables
  real(r_kind):: fsubdz(lat2,lon2,nsig),dzi
  integer(i_kind):: k,kp,km

  do k=1,nsig
     km=max(1,k-1)
     kp=min(nsig,k+1)
     dzi=one/real(kp-km,r_kind)
     fsubdz(:,:,k)=dzi*(fsub(:,:,kp)-fsub(:,:,km))
  end do

  call sub2fslab_glb(fsubdz,fslb0,fslb2,fslb3)

  return
end subroutine sub2fslabdz_glb
!=======================================================================
!=======================================================================
subroutine sub2slab2d(fsub,slab)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2slab2d
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert 2d subdomain data into anlaysis-space slab data
!
! program history log:
!   2008-01-23  sato
!   2010-03-25  zhu   - change work_* from arrays to a data structure work;
!                     - change interface of sub2grid 
!   2010-04-28  todling - update to use bundle
!   2011-06-29  todling - no explict reference to internal bundle arrays
!
!   input argument list:
!    fsub     - subdomain data array
!
!   output argument list:
!    slab     - anlaysis-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use gsi_bundlemod, only: gsi_bundlegetpointer
  implicit none

! Declare passed variables
  real(r_kind),intent(in   ) :: fsub(lat2,lon2)
  real(r_kind),intent(  out) :: slab(s2g_raf%inner_vars,nlat,nlon,s2g_raf%nlevs_alloc)
  
! Declare local variables
  integer(i_kind):: k,i,j,n,n2d,n3d,istatus
  real(r_kind),pointer,dimension(:,:,:)::ptr3d=>NULL()
  real(r_kind),pointer,dimension(:,:  )::ptr2d=>NULL()

  n2d=work%n2d
  n3d=work%n3d
  do n=1,n3d
     call gsi_bundlegetpointer(work,work%r3(n)%shortname,ptr3d,istatus)
     do k=1,nsig
        do i=1,lon2
           do j=1,lat2
              ptr3d(j,i,k) =fsub(j,i)
           end do
        end do
     end do
  end do
  do n=1,n2d
     call gsi_bundlegetpointer(work,work%r2(n)%shortname,ptr2d,istatus)
     do i=1,lon2
        do j=1,lat2
           ptr2d(j,i) =fsub(j,i)
        end do
     end do
  end do

  call general_sub2grid(s2g_raf,work%values,slab)

  return
end subroutine sub2slab2d
!=======================================================================
!=======================================================================
subroutine sub2fslab2d(fsub,fslab)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2fslab2d
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert 2d subdomain data into filter-space slab data
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    fsub     - subdomain data array
!
!   output argument list:
!    fslab    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use fgrid2agrid_mod, only: agrid2fgrid
  implicit none

! Declare passed variables
  real(r_kind)  ,intent(in   ) :: fsub(lat2,lon2)
  real(r_single),intent(  out) :: fslab(prm0%nlatf,prm0%nlonf)


! Declare local variables

  call sub2slab2d(fsub,hfine)

  call agrid2fgrid(prm0,hfine,hfilter) !analysis to filter grid
  fslab(:,:)=real(hfilter(:,:),r_single)

  return
end subroutine sub2fslab2d
!=======================================================================
!=======================================================================
subroutine sub2fslab2d_glb(fsub,fslb0,fslb2,fslb3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    sub2fslab2d
!   prgmmr: sato             org: np23                date: 2008-01-23
!
! abstract: convert 2d subdomain data into filter-space slab data
!           for global mode.
!
! program history log:
!   2008-01-23  sato
!
!   input argument list:
!    fsub                 - subdomain data array
!
!   output argument list:
!    fslb0,fslb2,fslb3    - filter-space data array
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use patch2grid_mod, only: grid2patch
  implicit none

! Declare passed variables
  real(r_kind)  ,intent(in   ) :: fsub(lat2,lon2)
  real(r_single),intent(  out) :: fslb0(prm0%nlatf,prm0%nlonf)
  real(r_single),intent(  out) :: fslb2(prm2%nlatf,prm2%nlonf)
  real(r_single),intent(  out) :: fslb3(prm3%nlatf,prm3%nlonf)

! Declare local variables

  call sub2slab2d(fsub,hfine)

  call grid2patch(hfine,hflt0,hflt2,hflt3) !analysis to filter grid
  fslb0(:,:)=real(hflt0(:,:),r_single)
  fslb2(:,:)=real(hflt2(:,:),r_single)
  fslb3(:,:)=real(hflt3(:,:),r_single)

  return
end subroutine sub2fslab2d_glb
!=======================================================================
end module sub2fslab_mod
