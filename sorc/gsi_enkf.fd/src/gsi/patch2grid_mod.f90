module patch2grid_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    patch2grid_mod
!   prgmmr: sato             org: np23                date: 2007-09-20
!
! abstract: contains the stuff to convert the full latlon grid data to
!           three filter patch spaces.
!           The three patches are north/south polar, and zonal patch.
!
! program history log:
!   2007-09-20  sato
!
! subroutines included:
!   sub setup_patch2grid
!   sub setup_blend
!   sub destroy_patch2grid
!   sub grid2patch
!   sub patch2grid
!   sub tpatch2grid
!   sub vpatch2grid
!
! Variable Definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants,only: zero,one
  use anberror, only: pf2aP1,pf2aP2,pf2aP3,&
                      nx,ny,mr,nr,nf
  use gridmod,  only: nlat,nlon

  implicit none

! set default to private
  private
! set subroutines to public
  public :: setup_patch2grid
  public :: setup_blend
  public :: destroy_patch2grid
  public :: grid2patch
  public :: patch2grid
  public :: tpatch2grid
  public :: vpatch2grid

  integer(i_kind):: ndx,ndy,ndx2,nmix,nrmxb,nmixp,nymx,nlatxb

  real(r_kind),allocatable,dimension(:,:):: p1all
  real(r_kind),allocatable,dimension(:,:):: p2all,p3all
  real(r_kind),allocatable,dimension(:,:):: p2pol,p3pol

  real(r_kind),allocatable,dimension(:):: bl,bl2

  contains

subroutine setup_patch2grid
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_patch2grid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  integer(i_kind):: ier

  ndx=(nx-nlon)/2
  ndy=(nlat-ny)/2
  ndx2=2*ndx
  nmix=nr+1+(ny-nlat)/2
  nrmxb=ndy-1
  nmixp=nmix+1
  nymx=ny-nmix
  nlatxb=nlat-nrmxb

  allocate(p1all(ny,nx))
  allocate(p2all(nlon+1,mr:nr))
  allocate(p3all(nlon+1,mr:nr))
  allocate(p2pol(nf*2+1,nf*2+1))
  allocate(p3pol(nf*2+1,nf*2+1),stat=ier)

  if( ier /= 0 ) then
     write(6,*) 'setup_patch2grid: could not allocate memories'
     call stop2(99)
  end if

!   initialize blend array (bl,bl2)
  allocate( bl(nx-nlon), bl2(nr+1+(ny-nlat)/2) )
  call setup_blend

end subroutine

subroutine setup_blend
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_blend
!   prgmmr:
!
! abstract: setup blend array bl & bl2
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use blendmod, only: blend
  implicit none

  integer(i_kind),dimension(0:40):: iblend
  integer(i_kind):: mm,nolp,nbuf,nmixbl,ndxbl
  real(r_kind):: dxx,x,y

  integer(i_kind):: i,j,k

! Setup blending
  mm=4
  call blend(mm,iblend)

  nolp=nr+1+(ny-nlat)/2
  nbuf=0
  nmixbl=nolp-nbuf*2
  dxx=one/(nmixbl+1)
  bl2=zero
  k=0
  do i=1,nmixbl
     k=k+1
     x=i*dxx
     y=iblend(mm)
     do j=mm-1,0,-1
        y=x*y+iblend(j)
     end do
     y=y*x**(mm+1)
     bl2(k)=one-y
  end do
  do k=1,nmixbl
     bl2(k)=sqrt(bl2(k))
  end do

  nmixbl=(nx-nlon)
  dxx=one/(nmixbl+1)
  ndxbl=(nx-nlon)
  bl=zero
  k=ndxbl-nmixbl
  do i=1,nmixbl
     k=k+1
     x=i*dxx
     y=iblend(mm)
     do j=mm-1,0,-1
        y=x*y+iblend(j)
     end do
     y=y*x**(mm+1)
     bl(k)=one-y
  enddo
  do k=1,nmixbl
     bl(k)=sqrt(bl(k))
  end do

end subroutine setup_blend

subroutine destroy_patch2grid
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_patch2grid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none

  deallocate(p1all)
  deallocate(p2all,p3all)
  deallocate(p2pol,p3pol)
  deallocate(bl,bl2)

end subroutine

!=======================================================================
!=======================================================================
subroutine grid2patch(grid_wrk,hflt_all,hflt_p2,hflt_p3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    grid2patch
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    grid_wrk
!
!   output argument list:
!    hflt_all
!    hflt_p2
!    hflt_p3
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use smooth_polcarf, only: smooth_caspol
  use fgrid2agrid_mod, only: agrid2fgrid

  implicit none

  real(r_kind),dimension(nlat,nlon),intent(in   ) :: grid_wrk

  real(r_kind)                     ,intent(  out) :: hflt_all(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_kind)                     ,intent(  out) :: hflt_p2 (pf2aP2%nlatf ,pf2aP2%nlonf )
  real(r_kind)                     ,intent(  out) :: hflt_p3 (pf2aP3%nlatf ,pf2aP3%nlonf )

  integer(i_kind):: i,i1,i2,j,j1

! Extract central patch (band) from full grid_wrk (work --> p1)
! Blending zones
  p1all=zero
  do i=1,ndx
     i1=i-ndx+nlon
     i2=nx-ndx+i
     do j=1,ny
        j1=j+ndy
        p1all(j,i) =grid_wrk(j1,i1)      ! left (west) blending zone
        p1all(j,i2)=grid_wrk(j1,i)       ! right (east) blending zone
     end do
  end do

! Middle zone (no blending)
  do i=ndx+1,nx-ndx
     i1=i-ndx
     do j=1,ny
        p1all(j,i)=grid_wrk(j+ndy,i1)
     end do
  end do

! Handle polar patches
  do i=1,nlon
     do j=mr,nrmxb+nmix
        p2all(i,j)=grid_wrk(nlat-j,i)
        p3all(i,j)=grid_wrk(j+1,i)
     end do
  end do

  call agrid2fgrid(pf2aP1,p1all,hflt_all) !analysis to filter grid_wrk

  call smooth_caspol(p2pol,p2all)
  call agrid2fgrid(pf2aP2 ,p2pol,hflt_p2)  !analysis to filter grid_wrk

  call smooth_caspol(p3pol,p3all)
  call agrid2fgrid(pf2aP3 ,p3pol,hflt_p3)  !analysis to filter grid_wrk

end subroutine grid2patch
!=======================================================================
!=======================================================================
subroutine tpatch2grid(grid_wrk,hflt_all,hflt_p2,hflt_p3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    tpatch2grid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    grid_wrk
!
!   output argument list:
!    hflt_all
!    hflt_p2
!    hflt_p3
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use smooth_polcarf, only: smooth_polcasa
  use fgrid2agrid_mod, only: tfgrid2agrid

  implicit none

  real(r_kind),dimension(nlat,nlon),intent(in   ) :: grid_wrk

  real(r_kind)                     ,intent(  out) :: hflt_all(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_kind)                     ,intent(  out) :: hflt_p2 (pf2aP2%nlatf ,pf2aP2%nlonf )
  real(r_kind)                     ,intent(  out) :: hflt_p3 (pf2aP3%nlatf ,pf2aP3%nlonf )

  integer(i_kind):: i,i1,i2,j,j1

! Extract central patch (band) from full grid_wrk (work --> p1)
! Blending zones
  p1all=zero

  do i=1,ndx
     i1=i-ndx+nlon
     i2=nx-ndx+i
     do j=1,ny
        j1=j+ndy
        p1all(j,i) =grid_wrk(j1,i1)      ! left (west) blending zone
        p1all(j,i2)=grid_wrk(j1,i)       ! right (east) blending zone
     end do
  end do

! Middle zone (no blending)
  do i=ndx+1,nx-ndx
     i1=i-ndx
     do j=1,ny
        p1all(j,i)=grid_wrk(j+ndy,i1)
     end do
  end do

! Apply blending coefficients to central patch
  do i=1,ndx2
     i1=ndx2+1-i
     i2=nx-ndx2+i
     do j=1,ny
        p1all(j,i) =p1all(j,i) *bl(i1)  ! left (west) blending zone
        p1all(j,i2)=p1all(j,i2)*bl(i)   ! right (east) blending zone
     end do
  end do

! bl2 of p1
  do i=1,nx
     do j=1,nmix
        p1all(j,i)=p1all(j,i)*bl2(nmixp-j)
     end do
     do j=nymx+1,ny
       p1all(j,i)=p1all(j,i)*bl2(j-nymx)
     end do
  end do

! Handle polar patches
  p2all=zero
  p3all=zero

  do j=mr,nrmxb+nmix
     do i=1,nlon
        p2all(i,j)=grid_wrk(nlat-j,i)
        p3all(i,j)=grid_wrk(j+1,i)
     end do
  end do

! Apply blending coefficients
  do j=nrmxb+1,nrmxb+nmix
     j1=j-nrmxb
     do i=1,nlon
        p2all(i,j)=p2all(i,j)*bl2(j1)
        p3all(i,j)=p3all(i,j)*bl2(j1)
     end do
  end do

  call tfgrid2agrid(pf2aP1,p1all,hflt_all) !analysis to filter grid_wrk

  call smooth_polcasa(p2pol,p2all)
  call tfgrid2agrid(pf2aP2,p2pol,hflt_p2) !analysis to filter grid_wrk

  call smooth_polcasa(p3pol,p3all)
  call tfgrid2agrid(pf2aP3,p3pol,hflt_p3) !analysis to filter grid_wrk

end subroutine tpatch2grid
!=======================================================================
!=======================================================================
subroutine patch2grid(grid_wrk,hflt_all,hflt_p2,hflt_p3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    patch2grid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    hflt_all
!    hflt_p2
!    hflt_p3
!
!   output argument list:
!    grid_wrk
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use smooth_polcarf, only: smooth_polcas
  use fgrid2agrid_mod, only: fgrid2agrid

  implicit none

  real(r_kind),dimension(nlat,nlon),intent(  out) :: grid_wrk

  real(r_kind)                     ,intent(in   ) :: hflt_all(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_kind)                     ,intent(in   ) :: hflt_p2 (pf2aP2%nlatf ,pf2aP2%nlonf )
  real(r_kind)                     ,intent(in   ) :: hflt_p3 (pf2aP3%nlatf ,pf2aP3%nlonf )

  integer(i_kind):: i,i1,i2,j,j1

  call fgrid2agrid(pf2aP1,hflt_all,p1all) !analysis to filter grid_wrk

  call fgrid2agrid(pf2aP2,hflt_p2,p2pol) !analysis to filter grid_wrk
  call smooth_polcas(p2pol,p2all)

  call fgrid2agrid(pf2aP3,hflt_p3,p3pol) !analysis to filter grid_wrk
  call smooth_polcas(p3pol,p3all)

! zero output array
  grid_wrk=zero

! Equatorial patch
! Adjoint of central patch blending on left/right sides of patch
  do i=1,ndx2
     i1=ndx2+1-i
     i2=nx-ndx2+i
     do j=1,ny
        p1all(j,i) =p1all(j,i) *bl(i1)   ! left (west) blending zone
        p1all(j,i2)=p1all(j,i2)*bl(i)    ! right (east) blending zone
     end do
  end do

! bl2 of p1
  do i=1,nx
     do j=1,nmix
        p1all(j,i)=p1all(j,i)*bl2(nmixp-j)
     end do
     do j=nymx+1,ny
        p1all(j,i)=p1all(j,i)*bl2(j-nymx)
     end do
  end do

! Adjoint of transfer between central band and full grid (p1 --> work)
  do i=1,ndx
     i1=i-ndx+nlon
     i2=nx-ndx+i
     do j=1,ny
        j1=j+ndy
        grid_wrk(j1,i1)=grid_wrk(j1,i1)+p1all(j,i)  ! left (west) blending zone
        grid_wrk(j1,i )=grid_wrk(j1,i )+p1all(j,i2) ! right (east) blending zone
     end do
  end do

! Middle zone (no blending)
  do i=ndx+1,nx-ndx
     i1=i-ndx
     do j=1,ny
        j1=j+ndy
        grid_wrk(j1,i1)=grid_wrk(j1,i1)+p1all(j,i)
     end do
  end do

! Adjoint of North pole patch(p2) -- blending and transfer to grid
! Adjoint of South pole patch(p3) -- blending and transfer to grid

  do j=nlatxb-nmix,nlatxb-1
! Adjoint of blending
     do i=1,nlon
        p2all(i,nlat-j)=p2all(i,nlat-j)*bl2(nlatxb-j)
     end do
  end do

  do j=nrmxb+1,nrmxb+nmix
!    Adjoint of blending
     do i=1,nlon
        p3all(i,j)=p3all(i,j)*bl2(j-nrmxb)
     end do
  end do

  do i=1,nlon
!    Adjoint of transfer
     do j=mr,nrmxb+nmix
        grid_wrk(j+1,i)=grid_wrk(j+1,i)+p3all(i,j)
     end do
     do j=nlatxb-nmix,nlat-mr
        grid_wrk(j  ,i)=grid_wrk(j  ,i)+p2all(i,nlat-j)
     end do
  end do

end subroutine patch2grid

!=======================================================================
!=======================================================================
subroutine vpatch2grid(grid_wrk,hflt_all,hflt_p2,hflt_p3)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    vpatch2grid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-09-22  lueken - added subprogram doc block
!
!   input argument list:
!    hflt_all
!    hflt_p2
!    hflt_p3
!
!   output argument list:
!    grid_wrk
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use smooth_polcarf, only: smooth_polcasv
  use fgrid2agrid_mod, only: fgrid2agrid

  implicit none

  real(r_kind),dimension(nlat,nlon),intent(  out) :: grid_wrk

  real(r_kind)                     ,intent(in   ) :: hflt_all(pf2aP1%nlatf,pf2aP1%nlonf)
  real(r_kind)                     ,intent(in   ) :: hflt_p2 (pf2aP2%nlatf ,pf2aP2%nlonf )
  real(r_kind)                     ,intent(in   ) :: hflt_p3 (pf2aP3%nlatf ,pf2aP3%nlonf )

  real(r_kind),allocatable,dimension(:,:) :: wgt_wrk
  real(r_kind),allocatable,dimension(:,:) :: p1wgt
  real(r_kind),allocatable,dimension(:,:) :: p2wgt,p3wgt

  integer(i_kind):: i,i1,i2,j,j1

  allocate(wgt_wrk(nlat,nlon))

  call fgrid2agrid(pf2aP1,hflt_all,p1all) !analysis to filter grid_wrk

  call fgrid2agrid(pf2aP2,hflt_p2,p2pol) !analysis to filter grid_wrk
  call smooth_polcasv(p2pol,p2all)

  call fgrid2agrid(pf2aP3,hflt_p3,p3pol) !analysis to filter grid_wrk
  call smooth_polcasv(p3pol,p3all)

  wgt_wrk=zero

! Equatorial patch
! Adjoint of central patch blending on left/right sides of patch
  allocate(p1wgt(ny,nx)); p1wgt=one

  do i=1,ndx2
     i1=ndx2+1-i
     i2=nx-ndx2+i
     do j=1,ny
        p1all(j,i) =p1all(j,i) *bl(i1)   ! left (west) blending zone
        p1all(j,i2)=p1all(j,i2)*bl(i)    ! right (east) blending zone
        p1wgt(j,i) =p1wgt(j,i) *bl(i1)
        p1wgt(j,i2)=p1wgt(j,i2)*bl(i)
     end do
  end do

! bl2 of p1
  do i=1,nx
     do j=1,nmix
        p1all(j,i)=p1all(j,i)*bl2(nmixp-j)
        p1wgt(j,i)=p1wgt(j,i)*bl2(nmixp-j)
     end do
     do j=nymx+1,ny
        p1all(j,i)=p1all(j,i)*bl2(j-nymx)
        p1wgt(j,i)=p1wgt(j,i)*bl2(j-nymx)
     end do
  end do

! zero output array
  grid_wrk=zero
  wgt_wrk=zero

! Adjoint of transfer between central band and full grid (p1 --> work)
  do i=1,ndx
     i1=i-ndx+nlon
     i2=nx-ndx+i
     do j=1,ny
        j1=j+ndy
        grid_wrk(j1,i1)=grid_wrk(j1,i1)+p1all(j,i)  ! left  (west) blending zone
        grid_wrk(j1,i )=grid_wrk(j1,i )+p1all(j,i2) ! right (east) blending zone
        wgt_wrk (j1,i1)=wgt_wrk (j1,i1)+p1wgt(j,i)  ! left  (west) blending zone
        wgt_wrk (j1,i )=wgt_wrk (j1,i )+p1wgt(j,i2) ! right (east) blending zone
     end do
  end do

! Middle zone (no blending)
  do i=ndx+1,nx-ndx
     i1=i-ndx
     do j=1,ny
        j1=j+ndy
        grid_wrk(j1,i1)=grid_wrk(j1,i1)+p1all(j,i)
        wgt_wrk (j1,i1)=wgt_wrk (j1,i1)+p1wgt(j,i)
     end do
  end do

  deallocate(p1wgt)

! Adjoint of North pole patch(p2) -- blending and transfer to grid
! Adjoint of South pole patch(p3) -- blending and transfer to grid
  allocate(p2wgt(nlon+1,mr:nr)); p2wgt=one
  allocate(p3wgt(nlon+1,mr:nr)); p3wgt=one

  do j=nlatxb-nmix,nlatxb-1
! Adjoint of blending
     do i=1,nlon
        p2all(i,nlat-j)=p2all(i,nlat-j)*bl2(nlatxb-j)
        p2wgt(i,nlat-j)=p2wgt(i,nlat-j)*bl2(nlatxb-j)
     end do
  end do

  do j=nrmxb+1,nrmxb+nmix
!    Adjoint of blending
     do i=1,nlon
        p3all(i,j)=p3all(i,j)*bl2(j-nrmxb)
        p3wgt(i,j)=p3wgt(i,j)*bl2(j-nrmxb)
     end do
  end do

  do i=1,nlon
!    Adjoint of transfer
     do j=mr,nrmxb+nmix
        grid_wrk(j+1,i)=grid_wrk(j+1,i)+p3all(i,j)
        wgt_wrk (j+1,i)=wgt_wrk (j+1,i)+p3wgt(i,j)
     end do
     do j=nlatxb-nmix,nlat-mr
        grid_wrk(j  ,i)=grid_wrk(j  ,i)+p2all(i,nlat-j)
        wgt_wrk (j  ,i)=wgt_wrk (j  ,i)+p2wgt(i,nlat-j)
     end do
  end do

  deallocate(p2wgt,p3wgt)

  where(wgt_wrk>zero) grid_wrk=grid_wrk/wgt_wrk

  deallocate(wgt_wrk)

end subroutine vpatch2grid

end module patch2grid_mod
