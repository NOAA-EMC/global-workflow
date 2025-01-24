module mod_fv3_lola
!$$$ module documentation block
!           .      .    .                                       .
! module:   mod_fv3_lola
!   prgmmr: parrish
!
! abstract:  This module contains routines to interpolate from a single
!             fv3 D grid tile to a rotated lat-lon analysis grid which completely
!             covers the fv3 tile.  Points beyond the fv3 tile are
!             filled with nearest fv3 edge values, but have no actual
!             impact on the analysis.
!
! program history log:
!   2017-02-24  parrish--initial documentation (patterned after
!   mod_fv3_to_a.f90)
!   2017-10-10  wu w - setup interpolation and trnsform coeff in generate_anl_grid
!                      add routines earthuv2fv3, fv3uv2earth, fv3_h_to_ll
!                        fv3_ll_to_h
!   2019-11-01  wu   - add checks in generate_anl_grid to present the mean
!                      longitude correctly to fix problem near lon=0
!   2022-03-01  X.Lu & X.Wang - add functions for HAFS dual ens capability. POC:
!   xuguang.wang@ou.edu
!   
! subroutines included:
!   sub generate_anl_grid
!   sub definecoef_regular_grids
!   sub earthuv2fv3
!   sub fv3uv2earth
!   sub fv3uv2earthens
!   sub fv3_h_to_ll
!   sub fv3_h_to_ll_ens
!   sub fv3_ll_to_h
!   sub rotate2deg 
!   sub unrotate2deg 
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

!       DIAGRAM:  D-Grid layout:
!
!   1                  nx
!   .                   .   (U,H)
!
! 1                     nx +1
! .                       .    (V)

!   U   U   U   U   U   U        + ny +1 (for U)
! V H V H V H V H V H V H V      + ny    (for V,H)
!   U   U   U   U   U   U                            xh(i) = i            dx=1
! V H V H V H V H V H V H V                          xu(i) = i
!   U   U   U   U   U   U                            xv(i) = i-0.5
! V H V H V H V H V H V H V
!   U   U   U   U   U   U                            yh(j) = j            dy=1
! V H V H V H V H V H V H V                          yu(j) = j-0.5
!   U   U   U   U   U   U                            yv(j) = j
! V H V H V H V H V H V H V
!   U   U   U   U   U   U
! V H V H V H V H V H V H V      + 1     (for V,H)
!   U   U   U   U   U   U        + 1     (for U)

! U(nx ,ny +1),V(nx +1,ny ),H(nx ,ny )

  use kinds, only: r_kind,i_kind
  implicit none
!
  private
  public :: generate_anl_grid,fv3_h_to_ll,fv3_ll_to_h,fv3uv2earth,earthuv2fv3
  public :: fv3dx,fv3dx1,fv3dy,fv3dy1,fv3ix,fv3ixp,fv3jy,fv3jyp,a3dx,a3dx1,a3dy,a3dy1,a3ix,a3ixp,a3jy,a3jyp
  public :: nxa,nya,cangu,sangu,cangv,sangv,nx,ny,bilinear
  public :: definecoef_regular_grids,fv3_h_to_ll_ens,fv3uv2earthens
  public :: fv3dxens,fv3dx1ens,fv3dyens,fv3dy1ens,fv3ixens,fv3ixpens,fv3jyens,fv3jypens,a3dxens,a3dx1ens,a3dyens,a3dy1ens,a3ixens,a3ixpens,a3jyens,a3jypens
  public :: nxe,nye,canguens,sanguens,cangvens,sangvens

  logical bilinear
  integer(i_kind) nxa,nya,nx,ny
  real(r_kind) ,allocatable,dimension(:,:):: fv3dx,fv3dx1,fv3dy,fv3dy1
  integer(i_kind),allocatable,dimension(:,:)::  fv3ix,fv3ixp,fv3jy,fv3jyp
  real(r_kind) ,allocatable,dimension(:,:):: a3dx,a3dx1,a3dy,a3dy1
  real(r_kind) ,allocatable,dimension(:,:):: cangu,sangu,cangv,sangv
  integer(i_kind),allocatable,dimension(:,:)::  a3ix,a3ixp,a3jy,a3jyp
  integer(i_kind) nxe,nye
  real(r_kind) ,allocatable,dimension(:,:):: fv3dxens,fv3dx1ens,fv3dyens,fv3dy1ens
  integer(i_kind),allocatable,dimension(:,:)::  fv3ixens,fv3ixpens,fv3jyens,fv3jypens
  real(r_kind) ,allocatable,dimension(:,:):: a3dxens,a3dx1ens,a3dyens,a3dy1ens
  real(r_kind) ,allocatable,dimension(:,:):: canguens,sanguens,cangvens,sangvens
  integer(i_kind),allocatable,dimension(:,:)::  a3ixens,a3ixpens,a3jyens,a3jypens
  

contains

subroutine generate_anl_grid(nx,ny,grid_lon,grid_lont,grid_lat,grid_latt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    generate_anl_grid
!   prgmmr: parrish
!
! abstract:  define rotated lat-lon analysis grid which is centered on fv3 tile 
!             and oriented to completely cover the tile.
!
! program history log:
!   2017-05-02  parrish
!   2017-10-10  wu   - 1. setup analysis A-grid, 
!                      2. compute/setup FV3 to A grid interpolation parameters
!                      3. compute/setup A to FV3 grid interpolation parameters         
!                      4. setup weightings for wind conversion from FV3 to earth
!   2019-11-01  wu   - add checks to present the mean longitude correctly to fix
!                       problem near lon=0
!
!   2021-08-11   lei - a fix for an upper bound of the dimnsion of  a3jyp 
!   input argument list:
!    nx, ny               - number of cells = nx*ny 
!    grid_lon ,grid_lat   - longitudes and latitudes of fv3 grid cell corners
!    grid_lont,grid_latt  - longitudes and latitudes of fv3 grid cell centers
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: quarter,one,two,half,zero,deg2rad,rearth,rad2deg
  use gridmod,  only:grid_ratio_fv3_regional, region_lat,region_lon,nlat,nlon
  use gridmod,  only: region_dy,region_dx,region_dyi,region_dxi,coeffy,coeffx
  use gridmod,  only:init_general_transform,region_dy,region_dx 
  use mpimod, only: mype
  use egrid2agrid_mod, only: egrid2agrid_parm
  implicit none

  real(r_kind),allocatable,dimension(:)::xbh_a,xa_a,xa_b
  real(r_kind),allocatable,dimension(:)::ybh_a,ya_a,ya_b,yy
  real(r_kind),allocatable,dimension(:,:)::xbh_b,ybh_b
  real(r_kind) dlat,dlon,dyy,dxx,dyyi,dxxi
  real(r_kind) dyyh,dxxh


  integer(i_kind), intent(in   ) :: nx,ny                 ! fv3 tile x- and y-dimensions
  real(r_kind)   , intent(inout) :: grid_lon(nx+1,ny+1)   ! fv3 cell corner longitudes
  real(r_kind)   , intent(inout) :: grid_lont(nx,ny)      ! fv3 cell center longitudes
  real(r_kind)   , intent(inout) :: grid_lat(nx+1,ny+1)   ! fv3 cell corner latitudes
  real(r_kind)   , intent(inout) :: grid_latt(nx,ny)      ! fv3 cell center latitudes

  integer(i_kind) i,j,ir,jr,n
  real(r_kind),allocatable,dimension(:,:) :: xc,yc,zc,gclat,gclon,gcrlat,gcrlon,rlon_in,rlat_in
  real(r_kind),allocatable,dimension(:,:) :: glon_an,glat_an
  real(r_kind) xcent,ycent,zcent,rnorm,centlat,centlon
  real(r_kind) adlon,adlat,alon,clat,clon
  integer(i_kind) nlonh,nlath,nxh,nyh
  integer(i_kind) ib1,ib2,jb1,jb2,jj

  integer(i_kind) nord_e2a
  real(r_kind)gxa,gya

  real(r_kind) x(nx+1,ny+1),y(nx+1,ny+1),z(nx+1,ny+1), xr,yr,zr,xu,yu,zu,rlat,rlon
  real(r_kind) xv,yv,zv,vval
  real(r_kind) cx,cy
  real(r_kind) uval,ewval,nsval
  real(r_kind) diff,sq180
  real(r_kind) d(4),ds
  integer(i_kind) kk,k


  nord_e2a=4
  bilinear=.false.


!   create xc,yc,zc for the cell centers.
  allocate(xc(nx,ny))
  allocate(yc(nx,ny))
  allocate(zc(nx,ny))
  allocate(gclat(nx,ny))
  allocate(gclon(nx,ny))
  allocate(gcrlat(nx,ny))
  allocate(gcrlon(nx,ny))
  do j=1,ny
     do i=1,nx
        xc(i,j)=cos(grid_latt(i,j)*deg2rad)*cos(grid_lont(i,j)*deg2rad)
        yc(i,j)=cos(grid_latt(i,j)*deg2rad)*sin(grid_lont(i,j)*deg2rad)
        zc(i,j)=sin(grid_latt(i,j)*deg2rad)
     enddo
  enddo

!  compute center as average x,y,z coordinates of corners of domain --

  xcent=quarter*(xc(1,1)+xc(1,ny)+xc(nx,1)+xc(nx,ny))
  ycent=quarter*(yc(1,1)+yc(1,ny)+yc(nx,1)+yc(nx,ny))
  zcent=quarter*(zc(1,1)+zc(1,ny)+zc(nx,1)+zc(nx,ny))

  rnorm=one/sqrt(xcent**2+ycent**2+zcent**2)
  xcent=rnorm*xcent
  ycent=rnorm*ycent
  zcent=rnorm*zcent
  centlat=asin(zcent)*rad2deg
  centlon=atan2(ycent,xcent)*rad2deg

!!  compute new lats, lons
  call rotate2deg(grid_lont,grid_latt,gcrlon,gcrlat, &
                  centlon,centlat,nx,ny)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  compute analysis A-grid  lats, lons
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!--------------------------obtain analysis grid dimensions nxa,nya
  nxa=1+nint((nx-one)/grid_ratio_fv3_regional)
  nya=1+nint((ny-one)/grid_ratio_fv3_regional)
  nlat=nya
  nlon=nxa
  if(mype==0) print *,'nlat,nlon=nya,nxa= ',nlat,nlon

!--------------------------obtain analysis grid spacing
  dlat=(maxval(gcrlat)-minval(gcrlat))/(ny-1)
  dlon=(maxval(gcrlon)-minval(gcrlon))/(nx-1)
  adlat=dlat*grid_ratio_fv3_regional
  adlon=dlon*grid_ratio_fv3_regional

!-------setup analysis A-grid; find center of the domain
  nlonh=nlon/2
  nlath=nlat/2

  if(nlonh*2==nlon)then
     clon=adlon/two
     cx=half
  else
     clon=adlon
     cx=one
  endif

  if(nlath*2==nlat)then
     clat=adlat/two
     cy=half
  else
     clat=adlat
     cy=one
  endif

!
!-----setup analysis A-grid from center of the domain
!
  allocate(rlat_in(nlat,nlon),rlon_in(nlat,nlon))
  do j=1,nlon
     alon=(j-nlonh)*adlon-clon
     do i=1,nlat
        rlon_in(i,j)=alon
     enddo
  enddo


  do j=1,nlon
     do i=1,nlat
        rlat_in(i,j)=(i-nlath)*adlat-clat
     enddo
  enddo

  if (allocated(region_dx )) deallocate(region_dx )
  if (allocated(region_dy )) deallocate(region_dy )
  allocate(region_dx(nlat,nlon),region_dy(nlat,nlon))
  allocate(region_dxi(nlat,nlon),region_dyi(nlat,nlon))
  allocate(coeffx(nlat,nlon),coeffy(nlat,nlon))
  dyy=rearth*adlat*deg2rad
  dyyi=one/dyy
  dyyh=half/dyy
  do j=1,nlon
     do i=1,nlat
        region_dy(i,j)=dyy
        region_dyi(i,j)=dyyi
        coeffy(i,j)=dyyh
     enddo
  enddo

  do i=1,nlat
     dxx=rearth*cos(rlat_in(i,1)*deg2rad)*adlon*deg2rad
     dxxi=one/dxx
     dxxh=half/dxx
     do j=1,nlon
        region_dx(i,j)=dxx
        region_dxi(i,j)=dxxi
        coeffx(i,j)=dxxh
     enddo
  enddo

!
!----------  setup  region_lat,region_lon in earth coord
!
  if (allocated(region_lat)) deallocate(region_lat)
  if (allocated(region_lon)) deallocate(region_lon)
  allocate(region_lat(nlat,nlon),region_lon(nlat,nlon))
  allocate(glat_an(nlon,nlat),glon_an(nlon,nlat))

  call unrotate2deg(region_lon,region_lat,rlon_in,rlat_in, &
                    centlon,centlat,nlat,nlon)

  region_lat=region_lat*deg2rad
  region_lon=region_lon*deg2rad

  do j=1,nlat
     do i=1,nlon
        glat_an(i,j)=region_lat(j,i)
        glon_an(i,j)=region_lon(j,i)
     enddo
  enddo

  call init_general_transform(glat_an,glon_an)
 
  deallocate(glat_an,glon_an)

!--------------------compute all combinations of relative coordinates

  allocate(xbh_a(nx),xbh_b(nx,ny),xa_a(nxa),xa_b(nxa))
  allocate(ybh_a(ny),ybh_b(nx,ny),ya_a(nya),ya_b(nya))

  nxh=nx/2
  nyh=ny/2

!!!!!! fv3 rotated grid; not equal spacing, non_orthogonal !!!!!!
  do j=1,ny
     jr=ny+1-j
     do i=1,nx
        ir=nx+1-i
        xbh_b(ir,jr)=gcrlon(i,j)/dlon
     end do
  end do
  do j=1,ny
     jr=ny+1-j
     do i=1,nx
       ir=nx+1-i
       ybh_b(ir,jr)=gcrlat(i,j)/dlat
     end do
  end do

!!!!  define analysis A grid  !!!!!!!!!!!!!
  do j=1,nxa
     xa_a(j)=(real(j-nlonh,r_kind)-cx)*grid_ratio_fv3_regional
  end do
  do i=1,nya
     ya_a(i)=(real(i-nlath,r_kind)-cy)*grid_ratio_fv3_regional
  end do

!!!!!compute fv3 to A grid interpolation parameters !!!!!!!!!
  allocate  (   fv3dx(nxa,nya),fv3dx1(nxa,nya),fv3dy(nxa,nya),fv3dy1(nxa,nya) )
  allocate  (   fv3ix(nxa,nya),fv3ixp(nxa,nya),fv3jy(nxa,nya),fv3jyp(nxa,nya) )
  allocate(yy(ny))

! iteration to find the fv3 grid cell
  jb1=1
  ib1=1
  do j=1,nya
     do i=1,nxa
      do n=1,3 
         gxa=xa_a(i)
         if(gxa < xbh_b(1,jb1))then
            gxa= 1
         else if(gxa > xbh_b(nx,jb1))then
            gxa= nx
         else
            call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
         endif
         ib2=ib1
         ib1=gxa
         do jj=1,ny
            yy(jj)=ybh_b(ib1,jj)
         enddo
         gya=ya_a(j)
         if(gya < yy(1))then
            gya= 1
         else if(gya > yy(ny))then
            gya= ny
         else
            call grdcrd1(gya,yy,ny,1)
         endif
         jb2=jb1
         jb1=gya
         if(ib1+1 > nx)then  !this block( 6 lines)  is copied from GSL gsi repository 
            ib1=ib1-1
         endif
         if(jb1+1 > ny)then
            jb1=jb1-1
         endif


         if((ib1 == ib2) .and. (jb1 == jb2)) exit
         if(n==3 ) then     
!!!!!!!   if not converge, find the nearest corner point
            d(1)=(xa_a(i)-xbh_b(ib1,jb1))**2+(ya_a(j)-ybh_b(ib1,jb1))**2
            d(2)=(xa_a(i)-xbh_b(ib1+1,jb1))**2+(ya_a(j)-ybh_b(ib1+1,jb1))**2
            d(3)=(xa_a(i)-xbh_b(ib1,jb1+1))**2+(ya_a(j)-ybh_b(ib1,jb1+1))**2
            d(4)=(xa_a(i)-xbh_b(ib1+1,jb1+1))**2+(ya_a(j)-ybh_b(ib1+1,jb1+1))**2
            kk=1 
            do k=2,4
               if(d(k)<d(kk))kk=k
            enddo
!!!!!!!!!!! Find the cell for interpolation
            gxa=xa_a(i)
            gya=ya_a(j)
            if(kk==1)then
               call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==2)then
               call grdcrd1(gxa,xbh_b(1,jb1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1+1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==3)then
               call grdcrd1(gxa,xbh_b(1,jb1+1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            else if(kk==4)then
               call grdcrd1(gxa,xbh_b(1,jb1+1),nx,1)
               do jj=1,ny
                  yy(jj)=ybh_b(ib1+1,jj)
               enddo
               call grdcrd1(gya,yy,ny,1)
            endif
            exit
         endif  !n=3   
      enddo  ! n

      fv3ix(i,j)=int(gxa)
      fv3ix(i,j)=min(max(1,fv3ix(i,j)),nx)
      fv3ixp(i,j)=min(nx,fv3ix(i,j)+1)
      fv3jy(i,j)=int(gya)
      fv3jy(i,j)=min(max(1,fv3jy(i,j)),ny)
      fv3jyp(i,j)=min(ny,fv3jy(i,j)+1)

      if(bilinear)then
         fv3dy(i,j)=max(zero,min(one,gya-fv3jy(i,j)))
         fv3dy1(i,j)=one-fv3dy(i,j)
         fv3dx(i,j)=max(zero,min(one,gxa-fv3ix(i,j)))
         fv3dx1(i,j)=one-fv3dx(i,j)
      else ! inverse-distance weighting average 
         ib1=fv3ix(i,j)
         ib2=fv3ixp(i,j)
         jb1=fv3jy(i,j)
         jb2=fv3jyp(i,j)
         if(xa_a(i)==xbh_b(ib1,jb1) .and. ya_a(j)==ybh_b(ib1,jb1))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=zero
            fv3dx(i,j)=one
            fv3dx1(i,j)=zero
         else if(xa_a(i)==xbh_b(ib2,jb1) .and. ya_a(j)==ybh_b(ib2,jb1))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=zero
            fv3dx(i,j)=zero
            fv3dx1(i,j)=one
         else if(xa_a(i)==xbh_b(ib1,jb2) .and. ya_a(j)==ybh_b(ib1,jb2))then
            fv3dy(i,j)=one 
            fv3dy1(i,j)=zero
            fv3dx(i,j)=zero
            fv3dx1(i,j)=zero
         else if(xa_a(i)==xbh_b(ib2,jb2) .and. ya_a(j)==ybh_b(ib2,jb2))then
            fv3dy(i,j)=zero
            fv3dy1(i,j)=one 
            fv3dx(i,j)=zero
            fv3dx1(i,j)=zero
         else 
            d(1)=one/((xa_a(i)-xbh_b(ib1,jb1))**2+(ya_a(j)-ybh_b(ib1,jb1))**2)
            d(2)=one/((xa_a(i)-xbh_b(ib2,jb1))**2+(ya_a(j)-ybh_b(ib2,jb1))**2)
            d(3)=one/((xa_a(i)-xbh_b(ib1,jb2))**2+(ya_a(j)-ybh_b(ib1,jb2))**2)
            d(4)=one/((xa_a(i)-xbh_b(ib2,jb2))**2+(ya_a(j)-ybh_b(ib2,jb2))**2)
            ds=one/(d(1)+d(2)+d(3)+d(4))
            fv3dy(i,j)=d(3)*ds
            fv3dy1(i,j)=d(4)*ds
            fv3dx(i,j)=d(1)*ds
            fv3dx1(i,j)=d(2)*ds
         endif
      endif !bilinear

     end do ! i
  end do ! j

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
!!!!!compute A to fv3 grid interpolation parameters !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
  allocate  (   a3dx(ny,nx),a3dx1(ny,nx),a3dy(ny,nx),a3dy1(ny,nx) )
  allocate  (   a3ix(ny,nx),a3ixp(ny,nx),a3jy(ny,nx),a3jyp(ny,nx) )
  do i=1,nx
     do j=1,ny
        gxa=xbh_b(i,j)
        if(gxa < xa_a(1))then
           gxa= 1
        else if(gxa > xa_a(nxa))then
           gxa= nxa
        else
           call grdcrd1(gxa,xa_a,nxa,1)
        endif
        a3ix(j,i)=int(gxa)
        a3ix(j,i)=min(max(1,a3ix(j,i)),nxa)
        a3dx(j,i)=max(zero,min(one,gxa-a3ix(j,i)))
        a3dx1(j,i)=one-a3dx(j,i)
        a3ixp(j,i)=min(nxa,a3ix(j,i)+1)
     end do
  end do

  do i=1,nx
    do j=1,ny
        gya=ybh_b(i,j)
        if(gya < ya_a(1))then
           gya= 1
        else if(gya > ya_a(nya))then
           gya= nya
        else
           call grdcrd1(gya,ya_a,nya,1)
        endif
        a3jy(j,i)=int(gya)
        a3jy(j,i)=min(max(1,a3jy(j,i)),nya)
        a3dy(j,i)=max(zero,min(one,gya-a3jy(j,i)))
        a3dy1(j,i)=one-a3dy(j,i)
        a3jyp(j,i)=min(nya,a3jy(j,i)+1)
     end do
  end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! find coefficients for wind conversion btw FV3 & earth
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  allocate  (   cangu(nx,ny+1),sangu(nx,ny+1),cangv(nx+1,ny),sangv(nx+1,ny) )

!   1.  compute x,y,z at cell cornor from grid_lon, grid_lat

  do j=1,ny+1
     do i=1,nx+1
        x(i,j)=cos(grid_lat(i,j)*deg2rad)*cos(grid_lon(i,j)*deg2rad)
        y(i,j)=cos(grid_lat(i,j)*deg2rad)*sin(grid_lon(i,j)*deg2rad)
        z(i,j)=sin(grid_lat(i,j)*deg2rad)
     enddo
  enddo

!  2   find angles to E-W and N-S for U edges
  sq180=180._r_kind**2 
  do j=1,ny+1
     do i=1,nx
!      center lat/lon of the edge 
        rlat=half*(grid_lat(i,j)+grid_lat(i+1,j))
        diff=(grid_lon(i,j)-grid_lon(i+1,j))**2
        if(diff < sq180)then
           rlon=half*(grid_lon(i,j)+grid_lon(i+1,j))
        else
           rlon=half*(grid_lon(i,j)+grid_lon(i+1,j)-360._r_kind)
        endif
!    vector to center of the edge
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
!     vector of the edge
        xu= x(i+1,j)-x(i,j)
        yu= y(i+1,j)-y(i,j)
        zu= z(i+1,j)-z(i,j)
!    find angle with cross product
        uval=sqrt((xu**2+yu**2+zu**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangu(i,j)=(-yr*xu+xr*yu)/ewval/uval
        sangu(i,j)=(-xr*zr*xu-zr*yr*yu+(xr*xr+yr*yr)*zu) / nsval/uval
     enddo
  enddo
 
!  3   find angles to E-W and N-S for V edges
  do j=1,ny
     do i=1,nx+1
        rlat=half*(grid_lat(i,j)+grid_lat(i,j+1))
        diff=(grid_lon(i,j)-grid_lon(i,j+1))**2
        if(diff < sq180)then
           rlon=half*(grid_lon(i,j)+grid_lon(i,j+1))
        else
           rlon=half*(grid_lon(i,j)+grid_lon(i,j+1)-360._r_kind)
        endif
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
        xv= x(i,j+1)-x(i,j)
        yv= y(i,j+1)-y(i,j)
        zv= z(i,j+1)-z(i,j)
        vval=sqrt((xv**2+yv**2+zv**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangv(i,j)=(-yr*xv+xr*yv)/ewval/vval
        sangv(i,j)=(-xr*zr*xv-zr*yr*yv+(xr*xr+yr*yr)*zv) / nsval/vval
     enddo
  enddo
  deallocate( xc,yc,zc,gclat,gclon,gcrlat,gcrlon)
  deallocate(rlat_in,rlon_in)
end subroutine generate_anl_grid

subroutine definecoef_regular_grids(nxen,nyen,grid_lon,grid_lont,grid_lat,grid_latt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    generate_??ens_grid
!clt modified from generate_regular_grid
!   prgmmr: parrish
!
! abstract:  define rotated lat-lon analysis grid which is centered on fv3 tile 
!             and oriented to completely cover the tile.
!
! program history log:
!   2017-05-02  parrish
!   2017-10-10  wu   - 1. setup analysis A-grid, 
!                      2. compute/setup FV3 to A grid interpolation parameters
!                      3. compute/setup A to FV3 grid interpolation parameters         
!                      4. setup weightings for wind conversion from FV3 to earth
!   2021-02-01 Lu & Wang - modify variable intent for HAFS dual ens. POC:
!   xuguang.wang@ou.edu
!
!   input argument list:
!    nxen, nyen               - number of cells = nxen*nyen 
!    grid_lon ,grid_lat   - longitudes and latitudes of fv3 grid cell corners
!    grid_lont,grid_latt  - longitudes and latitudes of fv3 grid cell centers
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use constants, only: quarter,one,two,half,zero,deg2rad,rearth,rad2deg
  use gridmod,  only:grid_ratio_fv3_regional
  use mpimod, only: mype
  use hybrid_ensemble_parameters, only: nlon_ens,nlat_ens,region_lon_ens,region_lat_ens
  implicit none
  real(r_kind),allocatable,dimension(:)::xbh_a,xa_a,xa_b
  real(r_kind),allocatable,dimension(:)::ybh_a,ya_a,ya_b,yy
  real(r_kind),allocatable,dimension(:,:)::xbh_b,ybh_b
  real(r_kind) dlat,dlon

  real(r_kind),allocatable:: region_lat_tmp(:,:),region_lon_tmp(:,:)
  integer(i_kind), intent(in   ) :: nxen,nyen                 ! fv3 tile x- and y-dimensions
  real(r_kind)   , intent(inout) :: grid_lon(nxen+1,nyen+1)   ! fv3 cell corner longitudes
  real(r_kind)   , intent(inout) :: grid_lont(nxen,nyen)      ! fv3 cell center longitudes
  real(r_kind)   , intent(inout) :: grid_lat(nxen+1,nyen+1)   ! fv3 cell corner latitudes
  real(r_kind)   , intent(inout) :: grid_latt(nxen,nyen)      ! fv3 cell center latitudes
  integer(i_kind) i,j,ir,jr,n
  real(r_kind),allocatable,dimension(:,:) :: xc,yc,zc,gclat,gclon,gcrlat,gcrlon,rlon_in,rlat_in
  real(r_kind) xcent,ycent,zcent,rnorm,centlat,centlon
  integer(i_kind) nxh,nyh
  integer(i_kind) ib1,ib2,jb1,jb2,jj
  integer (i_kind):: index0
  integer(i_kind) nord_e2a
  real(r_kind)gxa,gya

  real(r_kind) x(nxen+1,nyen+1),y(nxen+1,nyen+1),z(nxen+1,nyen+1),xr,yr,zr,xu,yu,zu,rlat,rlon
  real(r_kind) xv,yv,zv,vval
  real(r_kind) uval,ewval,nsval

  real(r_kind) d(4),ds
  integer(i_kind) kk,k
  real(r_kind) diff,sq180

  nord_e2a=4
  bilinear=.false.

!   create xc,yc,zc for the cell centers.
  allocate(xc(nxen,nyen))
  allocate(yc(nxen,nyen))
  allocate(zc(nxen,nyen))
  allocate(gclat(nxen,nyen))
  allocate(gclon(nxen,nyen))
  allocate(gcrlat(nxen,nyen))
  allocate(gcrlon(nxen,nyen))
  do j=1,nyen
     do i=1,nxen
        xc(i,j)=cos(grid_latt(i,j)*deg2rad)*cos(grid_lont(i,j)*deg2rad)
        yc(i,j)=cos(grid_latt(i,j)*deg2rad)*sin(grid_lont(i,j)*deg2rad)
        zc(i,j)=sin(grid_latt(i,j)*deg2rad)
     enddo
  enddo

!  compute center as average x,y,z coordinates of corners of domain --

  xcent=quarter*(xc(1,1)+xc(1,nyen)+xc(nxen,1)+xc(nxen,nyen))
  ycent=quarter*(yc(1,1)+yc(1,nyen)+yc(nxen,1)+yc(nxen,nyen))
  zcent=quarter*(zc(1,1)+zc(1,nyen)+zc(nxen,1)+zc(nxen,nyen))

  rnorm=one/sqrt(xcent**2+ycent**2+zcent**2)
  xcent=rnorm*xcent
  ycent=rnorm*ycent
  zcent=rnorm*zcent
  centlat=asin(zcent)*rad2deg
  centlon=atan2(ycent,xcent)*rad2deg

!!  compute new lats, lons
  call rotate2deg(grid_lont,grid_latt,gcrlon,gcrlat, &
                  centlon,centlat,nxen,nyen)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!  compute analysis A-grid  lats, lons
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!--------------------------obtain analysis grid dimensions nxe,nye
  nxe=nlon_ens
  nye=nlat_ens
  if(mype==0) print *,'nlat,nlon=nye,nxe= ',nlat_ens,nlon_ens

  allocate(rlat_in(nlat_ens,nlon_ens),rlon_in(nlat_ens,nlon_ens))
  allocate(region_lon_tmp(nlat_ens,nlon_ens),region_lat_tmp(nlat_ens,nlon_ens))
  region_lon_tmp=region_lon_ens*rad2deg
  region_lat_tmp=region_lat_ens*rad2deg
  call rotate2deg(region_lon_tmp,region_lat_tmp,rlon_in,rlat_in, &
                    centlon,centlat,nlat_ens,nlon_ens)

!--------------------------obtain analysis grid spacing
  dlat=(maxval(gcrlat)-minval(gcrlat))/(nyen-1)
  dlon=(maxval(gcrlon)-minval(gcrlon))/(nxen-1)


!-----setup analysis A-grid from center of the domain
!--------------------compute all combinations of relative coordinates

  allocate(xbh_a(nxen),xbh_b(nxen,nyen),xa_a(nxe),xa_b(nxe))
  allocate(ybh_a(nyen),ybh_b(nxen,nyen),ya_a(nye),ya_b(nye))

  nxh=nxen/2
  nyh=nyen/2


!!!!!! fv3 rotated grid; not equal spacing, non_orthogonal !!!!!!
  do j=1,nyen
     jr=nyen+1-j
     do i=1,nxen
        ir=nxen+1-i
        xbh_b(ir,jr)=gcrlon(i,j)/dlon
     end do
  end do
  do j=1,nyen
     jr=nyen+1-j
     do i=1,nxen
       ir=nxen+1-i
       ybh_b(ir,jr)=gcrlat(i,j)/dlat
     end do
  end do

!!!!  define analysis A grid  !!!!!!!!!!!!!

  index0=1
  do j=1,nxe
     xa_a(j)= rlon_in(index0,j)/dlon
  end do
  do i=1,nye
     ya_a(i)= rlat_in(i,index0)/dlat
  end do

!!!!!compute fv3 to A grid interpolation parameters !!!!!!!!!
  allocate (fv3dxens(nxe,nye),fv3dx1ens(nxe,nye),fv3dyens(nxe,nye),fv3dy1ens(nxe,nye))
  allocate (fv3ixens(nxe,nye),fv3ixpens(nxe,nye),fv3jyens(nxe,nye),fv3jypens(nxe,nye))
  allocate(yy(nyen))

! iteration to find the fv3 grid cell
  jb1=1
  ib1=1
  do j=1,nye
     do i=1,nxe
      do n=1,3
         gxa=xa_a(i)
         if(gxa < xbh_b(1,jb1))then
            gxa= 1
         else if(gxa > xbh_b(nxen,jb1))then
            gxa= nxen
         else
            call grdcrd1(gxa,xbh_b(1,jb1),nxen,1)
         endif
         ib2=ib1
         ib1=gxa
         do jj=1,nyen
            yy(jj)=ybh_b(ib1,jj)
         enddo
         gya=ya_a(j)
         if(gya < yy(1))then
            gya= 1
         else if(gya > yy(nyen))then
            gya= nyen
         else
            call grdcrd1(gya,yy,nyen,1)
         endif
         jb2=jb1
         jb1=gya
         if(ib1+1 > nxen)then  !this block( 6 lines)  is copied from GSL gsi repository
            ib1=ib1-1
         endif
         if(jb1+1 > nyen)then
            jb1=jb1-1
         endif

         if((ib1 == ib2) .and. (jb1 == jb2)) exit
         if(n==3 ) then
!!!!!!!   if not converge, find the nearest corner point
            d(1)=(xa_a(i)-xbh_b(ib1,jb1))**2+(ya_a(j)-ybh_b(ib1,jb1))**2
            d(2)=(xa_a(i)-xbh_b(ib1+1,jb1))**2+(ya_a(j)-ybh_b(ib1+1,jb1))**2
            d(3)=(xa_a(i)-xbh_b(ib1,jb1+1))**2+(ya_a(j)-ybh_b(ib1,jb1+1))**2
            d(4)=(xa_a(i)-xbh_b(ib1+1,jb1+1))**2+(ya_a(j)-ybh_b(ib1+1,jb1+1))**2
            kk=1
            do k=2,4
               if(d(k)<d(kk))kk=k
            enddo
!!!!!!!!!!! Find the cell for interpolation
            gxa=xa_a(i)
            gya=ya_a(j)
            if(kk==1)then
               call grdcrd1(gxa,xbh_b(1,jb1),nxen,1)
               do jj=1,nyen
                  yy(jj)=ybh_b(ib1,jj)
               enddo
               call grdcrd1(gya,yy,nyen,1)
            else if(kk==2)then
               call grdcrd1(gxa,xbh_b(1,jb1),nxen,1)
               do jj=1,nyen
                  yy(jj)=ybh_b(ib1+1,jj)
               enddo
               call grdcrd1(gya,yy,nyen,1)
            else if(kk==3)then
               call grdcrd1(gxa,xbh_b(1,jb1+1),nxen,1)
               do jj=1,nyen
                  yy(jj)=ybh_b(ib1,jj)
               enddo
               call grdcrd1(gya,yy,nyen,1)
            else if(kk==4)then
               call grdcrd1(gxa,xbh_b(1,jb1+1),nxen,1)
               do jj=1,nyen
                  yy(jj)=ybh_b(ib1+1,jj)
               enddo
               call grdcrd1(gya,yy,nyen,1)
            endif
            exit
         endif  !n=3   
      enddo  ! n

      fv3ixens(i,j)=int(gxa)
      fv3ixens(i,j)=min(max(1,fv3ixens(i,j)),nxen)
      fv3ixpens(i,j)=min(nxen,fv3ixens(i,j)+1)
      fv3jyens(i,j)=int(gya)
      fv3jyens(i,j)=min(max(1,fv3jyens(i,j)),nyen)
      fv3jypens(i,j)=min(nyen,fv3jyens(i,j)+1)

      if(bilinear)then
         fv3dyens(i,j)=max(zero,min(one,gya-fv3jyens(i,j)))
         fv3dy1ens(i,j)=one-fv3dyens(i,j)
         fv3dxens(i,j)=max(zero,min(one,gxa-fv3ixens(i,j)))
         fv3dx1ens(i,j)=one-fv3dxens(i,j)
      else ! inverse-distance weighting average 
         ib1=fv3ixens(i,j)
         ib2=fv3ixpens(i,j)
         jb1=fv3jyens(i,j)
         jb2=fv3jypens(i,j)
         if(xa_a(i)==xbh_b(ib1,jb1) .and. ya_a(j)==ybh_b(ib1,jb1))then
            fv3dyens(i,j)=zero
            fv3dy1ens(i,j)=zero
            fv3dxens(i,j)=one
            fv3dx1ens(i,j)=zero
         else if(xa_a(i)==xbh_b(ib2,jb1) .and. ya_a(j)==ybh_b(ib2,jb1))then
            fv3dyens(i,j)=zero
            fv3dy1ens(i,j)=zero
            fv3dxens(i,j)=zero
            fv3dx1ens(i,j)=one
         else if(xa_a(i)==xbh_b(ib1,jb2) .and. ya_a(j)==ybh_b(ib1,jb2))then
            fv3dyens(i,j)=one
            fv3dy1ens(i,j)=zero
            fv3dxens(i,j)=zero
            fv3dx1ens(i,j)=zero
         else if(xa_a(i)==xbh_b(ib2,jb2) .and. ya_a(j)==ybh_b(ib2,jb2))then
            fv3dyens(i,j)=zero
            fv3dy1ens(i,j)=one
            fv3dxens(i,j)=zero
            fv3dx1ens(i,j)=zero
         else
            d(1)=one/((xa_a(i)-xbh_b(ib1,jb1))**2+(ya_a(j)-ybh_b(ib1,jb1))**2)
            d(2)=one/((xa_a(i)-xbh_b(ib2,jb1))**2+(ya_a(j)-ybh_b(ib2,jb1))**2)
            d(3)=one/((xa_a(i)-xbh_b(ib1,jb2))**2+(ya_a(j)-ybh_b(ib1,jb2))**2)
            d(4)=one/((xa_a(i)-xbh_b(ib2,jb2))**2+(ya_a(j)-ybh_b(ib2,jb2))**2)
            ds=one/(d(1)+d(2)+d(3)+d(4))
            fv3dyens(i,j)=d(3)*ds
            fv3dy1ens(i,j)=d(4)*ds
            fv3dxens(i,j)=d(1)*ds
            fv3dx1ens(i,j)=d(2)*ds
         endif
      endif !bilinear

     end do ! i
  end do ! j

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
!!!!!compute A to fv3 grid interpolation parameters
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
  allocate (a3dxens(nyen,nxen),a3dx1ens(nyen,nxen),a3dyens(nyen,nxen),a3dy1ens(nyen,nxen))
  allocate (a3ixens(nyen,nxen),a3ixpens(nyen,nxen),a3jyens(nyen,nxen),a3jypens(nyen,nxen))

  do i=1,nxen
     do j=1,nyen
        gxa=xbh_b(i,j)
        if(gxa < xa_a(1))then
           gxa= 1
        else if(gxa > xa_a(nxe))then
           gxa= nxe
        else
           call grdcrd1(gxa,xa_a,nxe,1)
        endif
        a3ixens(j,i)=int(gxa)
        a3ixens(j,i)=min(max(1,a3ixens(j,i)),nxe)
        a3dxens(j,i)=max(zero,min(one,gxa-a3ixens(j,i)))
        a3dx1ens(j,i)=one-a3dxens(j,i)
        a3ixpens(j,i)=min(nxe,a3ixens(j,i)+1)
     end do
  end do

  do i=1,nxen
    do j=1,nyen
        gya=ybh_b(i,j)
        if(gya < ya_a(1))then
           gya= 1
        else if(gya > ya_a(nye))then
           gya= nye
        else
           call grdcrd1(gya,ya_a,nye,1)
        endif
        a3jyens(j,i)=int(gya)
        a3jyens(j,i)=min(max(1,a3jyens(j,i)),nye)
        a3dyens(j,i)=max(zero,min(one,gya-a3jyens(j,i)))
        a3dy1ens(j,i)=one-a3dyens(j,i)
        a3jypens(j,i)=min(nye,a3jyens(j,i)+1)
     end do
  end do

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!! find coefficients for wind conversion btw FV3 & earth
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  allocate (canguens(nxen,nyen+1),sanguens(nxen,nyen+1),cangvens(nxen+1,nyen),sangvens(nxen+1,nyen))

!   1.  compute x,y,z at cell cornor from grid_lon, grid_lat

  do j=1,nyen+1
     do i=1,nxen+1
        x(i,j)=cos(grid_lat(i,j)*deg2rad)*cos(grid_lon(i,j)*deg2rad)
        y(i,j)=cos(grid_lat(i,j)*deg2rad)*sin(grid_lon(i,j)*deg2rad)
        z(i,j)=sin(grid_lat(i,j)*deg2rad)
     enddo
  enddo

!  2   find angles to E-W and N-S for U edges

  sq180=180._r_kind**2
  do j=1,nyen+1
     do i=1,nxen
!      center lat/lon of the edge 
        rlat=half*(grid_lat(i,j)+grid_lat(i+1,j))
        diff=(grid_lon(i,j)-grid_lon(i+1,j))**2
        if(diff < sq180)then
           rlon=half*(grid_lon(i,j)+grid_lon(i+1,j))
        else
           rlon=half*(grid_lon(i,j)+grid_lon(i+1,j)-360._r_kind)
        endif
!    vector to center of the edge
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
!     vector of the edge
        xu= x(i+1,j)-x(i,j)
        yu= y(i+1,j)-y(i,j)
        zu= z(i+1,j)-z(i,j)
!    find angle with cross product
        uval=sqrt((xu**2+yu**2+zu**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        canguens(i,j)=(-yr*xu+xr*yu)/ewval/uval
        sanguens(i,j)=(-xr*zr*xu-zr*yr*yu+(xr*xr+yr*yr)*zu) / nsval/uval
     enddo
  enddo

!  3   find angles to E-W and N-S for V edges
  do j=1,nyen
     do i=1,nxen+1
        rlat=half*(grid_lat(i,j)+grid_lat(i,j+1))
        diff=(grid_lon(i,j)-grid_lon(i,j+1))**2
        if(diff < sq180)then
           rlon=half*(grid_lon(i,j)+grid_lon(i,j+1))
        else
           rlon=half*(grid_lon(i,j)+grid_lon(i,j+1)-360._r_kind)
        endif
        xr=cos(rlat*deg2rad)*cos(rlon*deg2rad)
        yr=cos(rlat*deg2rad)*sin(rlon*deg2rad)
        zr=sin(rlat*deg2rad)
        xv= x(i,j+1)-x(i,j)
        yv= y(i,j+1)-y(i,j)
        zv= z(i,j+1)-z(i,j)
        vval=sqrt((xv**2+yv**2+zv**2))
        ewval=sqrt((xr**2+yr**2))
        nsval=sqrt((xr*zr)**2+(zr*yr)**2+(xr*xr+yr*yr)**2)
        cangvens(i,j)=(-yr*xv+xr*yv)/ewval/vval
        sangvens(i,j)=(-xr*zr*xv-zr*yr*yv+(xr*xr+yr*yr)*zv) / nsval/vval
     enddo
  enddo
  deallocate( xc,yc,zc,gclat,gclon,gcrlat,gcrlon)
  deallocate(rlat_in,rlon_in)
end subroutine definecoef_regular_grids

subroutine earthuv2fv3(u,v,nx,ny,u_out,v_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    earthuv2fv3
!   prgmmr: wu                      2017-06-15
!
! abstract: project earth UV to fv3 UV and interpolate to edge of the cell
!
! program history log:
!   
!
!   input argument list:
!    u,v -  earth wind components at center of the cell
!    nx,ny - dimensions
!
!   output argument list:
!    u_out,v_out - output fv3 winds on the cell boundaries
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use kinds, only: r_kind,i_kind
  use constants, only: half
  implicit none

  integer(i_kind), intent(in   ) :: nx,ny                 ! fv3 tile x- and y-dimensions
  real(r_kind),intent(in   ) :: u(nx,ny),v(nx,ny)
  real(r_kind),intent(  out) :: u_out(nx,ny+1),v_out(nx+1,ny)
  integer(i_kind) i,j


!!!!!!! earth u/v to covariant u/v
  j=1
  do i=1,nx
     u_out(i,j)= u(i,j)*cangu(i,j)+v(i,j)*sangu(i,j)
  end do

  do j=2,ny
     do i=1,nx
        u_out(i,j)=half   *( (u(i,j)+u(i,j-1))*cangu(i,j)+(v(i,j)+v(i,j-1))*sangu(i,j) )
     end do
  end do
  j=ny
  do i=1,nx
     u_out(i,j+1)= u(i,j)*cangu(i,j+1)+v(i,j)*sangu(i,j+1)
  end do

  do j=1,ny
     v_out(1,j)=u(1,j)*cangv(1,j)+v(1,j)*sangv(1,j)
     do i=2,nx
        v_out(i,j)=half   *( (u(i,j)+u(i-1,j))*cangv(i,j)+(v(i,j)+v(i-1,j))*sangv(i,j) )
     end do
     v_out(nx+1,j)=u(nx,j)*cangv(nx+1,j)+v(nx,j)*sangv(nx+1,j)
  end do
end subroutine earthuv2fv3

subroutine fv3uv2earth(u,v,nx,ny,u_out,v_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3uv2earth
!   prgmmr: wu                      2017-06-15
!
! abstract: project fv3 UV to earth UV and interpolate to the center of the cells
!
! program history log:
!   
!
!   input argument list:
!    u,v - fv3 winds on the cell boundaries
!    nx,ny - dimensions
!
!   output argument list:
!    u_out,v_out - output earth wind components at center of the cell
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use kinds, only: r_kind,i_kind
  use constants, only: half
  implicit none

  integer(i_kind), intent(in   ) :: nx,ny                 ! fv3 tile x- and y-dimensions
  real(r_kind),intent(in   ) :: u(nx,ny+1),v(nx+1,ny)
  real(r_kind),intent(  out) :: u_out(nx,ny),v_out(nx,ny)
  integer(i_kind) i,j

  do j=1,ny
     do i=1,nx
        u_out(i,j)=half *( (u(i,j)*sangv(i,j)-v(i,j)*sangu(i,j))/(cangu(i,j)*sangv(i,j)-sangu(i,j)*cangv(i,j)) &
                       +(u(i,j+1)*sangv(i+1,j)-v(i+1,j)*sangu(i,j+1))/(cangu(i,j+1)*sangv(i+1,j)-sangu(i,j+1)*cangv(i+1,j)))
        v_out(i,j)=half *( (u(i,j)*cangv(i,j)-v(i,j)*cangu(i,j))/(sangu(i,j)*cangv(i,j)-cangu(i,j)*sangv(i,j)) &
                       +(u(i,j+1)*cangv(i+1,j)-v(i+1,j)*cangu(i,j+1))/(sangu(i,j+1)*cangv(i+1,j)-cangu(i,j+1)*sangv(i+1,j)))
     end do
  end do
  return
end subroutine fv3uv2earth

subroutine fv3uv2earthens(u,v,nxen,nyen,u_out,v_out)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3uv2earthens
!   prgmmr: wu                      2017-06-15
!
! abstract: project fv3 UV to earth UV and interpolate to the center of the
! cells
!
! program history log:
!   
!
!   input argument list:
!    u,v - fv3 winds on the cell boundaries
!    nx,ny - dimensions
!
!   output argument list:
!    u_out,v_out - output earth wind components at center of the cell
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use kinds, only: r_kind,i_kind
  use constants, only: half
  implicit none

  integer(i_kind), intent(in   ) :: nxen,nyen                 ! fv3 tile x- and y-dimensions
  real(r_kind),intent(in   ) :: u(nxen,nyen+1),v(nxen+1,nyen)
  real(r_kind),intent(  out) :: u_out(nxen,nyen),v_out(nxen,nyen)
  integer(i_kind) i,j

  do j=1,nyen
     do i=1,nxen
        u_out(i,j)=half *((u(i,j)*sangvens(i,j)-v(i,j)*sanguens(i,j))/(canguens(i,j)*sangvens(i,j)-sanguens(i,j)*cangvens(i,j)) &
                       +(u(i,j+1)*sangvens(i+1,j)-v(i+1,j)*sanguens(i,j+1))/(canguens(i,j+1)*sangvens(i+1,j)-sanguens(i,j+1)*cangvens(i+1,j)))
        v_out(i,j)=half *((u(i,j)*cangvens(i,j)-v(i,j)*canguens(i,j))/(sanguens(i,j)*cangvens(i,j)-canguens(i,j)*sangvens(i,j)) &
                       +(u(i,j+1)*cangvens(i+1,j)-v(i+1,j)*canguens(i,j+1))/(sanguens(i,j+1)*cangvens(i+1,j)-canguens(i,j+1)*sangvens(i+1,j)))
     end do
  end do
  return
end subroutine fv3uv2earthens

subroutine fv3_h_to_ll(b_in,a,nb,mb,na,ma,rev_flg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3_h_to_ll
!   prgmmr: wu                      2017-05-30
!
! abstract: interpolate from rotated fv3 grid to A grid. 
!     Interpolation choices 1)bilinear both ways 
!                           2)inverse-distance weighting average
!     reverse E-W and N-S directions & reverse i,j for output array a(nlat,nlon)
!
! program history log:
!   
!
!   input argument list:
!    mb,nb - fv3 dimensions
!    ma,na - a dimensions
!    b     - input variable b
!    xb,yb - b array x and y coordinates
!    xa,ya - a array coordinates (xa in xb units, ya in yb units)
!
!   output argument list:
!    a     - output interpolated array
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use constants, only: zero,one
  implicit none

  integer(i_kind),intent(in   ) :: mb,nb,ma,na
  real(r_kind)   ,intent(in   ) :: b_in(nb,mb)
  logical        ,intent(in   ) :: rev_flg
  real(r_kind)   ,intent(  out) :: a(ma,na)

  integer(i_kind) i,j,ir,jr,mbp,nbp
  real(r_kind)    b(nb,mb)

  mbp=mb+1
  nbp=nb+1
  if(rev_flg) then
!!!!!!!!! reverse E-W and N-S
     do j=1,mb
        jr=mbp-j
        do i=1,nb
           ir=nbp-i
           b(ir,jr)=b_in(i,j)
        end do
     end do
  else
     b(:,:)=b_in(:,:)
  endif
!!!!!!!!! interpolate to A grid & reverse ij for array a(lat,lon)
  if(bilinear)then ! bilinear interpolation
     do j=1,ma
        do i=1,na
           a(j,i)=fv3dx1(i,j)*(fv3dy1(i,j)*b(fv3ix (i,j),fv3jy(i,j))+fv3dy(i,j)*b(fv3ix (i,j),fv3jyp(i,j))) &
              +fv3dx (i,j)*(fv3dy1(i,j)*b(fv3ixp(i,j),fv3jy(i,j))+fv3dy(i,j)*b(fv3ixp(i,j),fv3jyp(i,j)))
        end do
     end do
  else  ! inverse-distance weighting average 
     do j=1,ma
        do i=1,na
           a(j,i)=fv3dx(i,j)*b(fv3ix (i,j),fv3jy(i,j))+fv3dy(i,j)*b(fv3ix (i,j),fv3jyp(i,j)) &
              +fv3dx1(i,j)*b(fv3ixp(i,j),fv3jy(i,j))+fv3dy1(i,j)*b(fv3ixp(i,j),fv3jyp(i,j))
        end do
     end do
  endif
  return
end subroutine fv3_h_to_ll

subroutine fv3_h_to_ll_ens(b_in,a,nb,mb,na,ma,rev_flg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3_h_to_ll
!   prgmmr: wu                      2017-05-30
!
! abstract: interpolate from rotated fv3 grid to A grid. 
!     Interpolation choices 1)bilinear both ways 
!                           2)inverse-distance weighting average
!     reverse E-W and N-S directions & reverse i,j for output array a(nlat,nlon)
!
! program history log:
!   
!
!   input argument list:
!    mb,nb - fv3 dimensions
!    ma,na - a dimensions
!    b     - input variable b
!    xb,yb - b array x and y coordinates
!    xa,ya - a array coordinates (xa in xb units, ya in yb units)
!
!   output argument list:
!    a     - output interpolated array
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  
  use constants, only: zero,one
  implicit none

  integer(i_kind),intent(in   ) :: mb,nb,ma,na
  real(r_kind)   ,intent(in   ) :: b_in(nb,mb)
  logical        ,intent(in   ) :: rev_flg
  real(r_kind)   ,intent(  out) :: a(ma,na)

  integer(i_kind) i,j,ir,jr,mbp,nbp
  real(r_kind)    b(nb,mb)

  mbp=mb+1
  nbp=nb+1
  bilinear=.false.
  if(rev_flg) then
!!!!!!!!! reverse E-W and N-S
     do j=1,mb
        jr=mbp-j
        do i=1,nb
           ir=nbp-i
           b(ir,jr)=b_in(i,j)
        end do
     end do
  else
     b(:,:)=b_in(:,:)
  endif
!!!!!!!!! interpolate to A grid & reverse ij for array a(lat,lon)
  if(bilinear)then ! bilinear interpolation
     do j=1,ma
        do i=1,na
           a(j,i)=fv3dx1ens(i,j)*(fv3dy1ens(i,j)*b(fv3ixens(i,j),fv3jyens(i,j)) &
           +fv3dyens(i,j)*b(fv3ixens(i,j),fv3jypens(i,j))) &
           +fv3dxens(i,j)*(fv3dy1ens(i,j)*b(fv3ixpens(i,j),fv3jyens(i,j)) &
           +fv3dyens(i,j)*b(fv3ixpens(i,j),fv3jypens(i,j)))
        end do
     end do
  else  ! inverse-distance weighting average 
     do j=1,ma
        do i=1,na
           a(j,i)=fv3dxens(i,j)*b(fv3ixens(i,j),fv3jyens(i,j)) &
              +fv3dyens(i,j)*b(fv3ixens(i,j),fv3jypens(i,j)) &
              +fv3dx1ens(i,j)*b(fv3ixpens(i,j),fv3jyens(i,j)) &
              +fv3dy1ens(i,j)*b(fv3ixpens(i,j),fv3jypens(i,j))
        end do
     end do
  endif

  return
end subroutine fv3_h_to_ll_ens

subroutine fv3_ll_to_h(a,b,nxa,nya,nxb,nyb,rev_flg)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    fv3_ll_to_h
!   prgmmr: wu                      2017-05-30
!
! abstract: interpolate from analysis A grid to rotated fv3 grid. 
!    Interpolation is bilinear both ways.  Reverse E-W and N-S and
!     reverse i,j for output array b(nxb,nyb)
!
! program history log:
!   
!
!   input argument list:
!    nxa,nya - a array dimensions
!    nxb,nyb - b array dimensions
!
!    b     - input variable b
!    rev_flg - flag for reverse i,j order
!   output argument list:
!    a     - output interpolated array
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block  

  use kinds, only: r_kind,i_kind
  use constants, only: zero,one
  implicit none

  integer(i_kind),intent(in   ) :: nyb,nxb,nya,nxa
  real(r_kind)   ,intent(in   ) :: a(nya,nxa)
  logical        ,intent(in   ) :: rev_flg 
  real(r_kind)   ,intent(  out) :: b(nxb*nyb)

  integer(i_kind) i,j,ir,jr,nybp,nxbp,ijr

  if(rev_flg)then
!!!!!!!!!! output in reverse E-W, N-S and reversed i,j !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
     nybp=nyb+1
     nxbp=nxb+1
     do i=1,nyb
        ir=nybp-i
        ijr=(ir-1)*nxb
        do j=1,nxb
           jr=nxbp-j
           b(jr+ijr)=a3dy1(i,j)*(a3dx1(i,j)*a(a3jy (i,j),a3ix(i,j))+a3dx(i,j)*a(a3jy (i,j),a3ixp(i,j))) &
             +a3dy (i,j)*(a3dx1(i,j)*a(a3jyp(i,j),a3ix(i,j))+a3dx(i,j)*a(a3jyp(i,j),a3ixp(i,j)))
        end do
     end do
  else
!!!!!!!!!! output order as input W-E S-N and (i:lat,j:lon) !!!!!!!!!!!
     do i=1,nyb
        ijr=(i-1)*nxb
        do j=1,nxb
           b(j+ijr)=a3dy1(i,j)*(a3dx1(i,j)*a(a3jy (i,j),a3ix(i,j))+a3dx(i,j)*a(a3jy (i,j),a3ixp(i,j))) &
             +a3dy (i,j)*(a3dx1(i,j)*a(a3jyp(i,j),a3ix(i,j))+a3dx(i,j)*a(a3jyp(i,j),a3ixp(i,j)))
        end do
     end do
  endif
end subroutine fv3_ll_to_h

end module mod_fv3_lola

subroutine rotate2deg(rlon_in,rlat_in,rlon_out,rlat_out,rlon0,rlat0,nx,ny)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    rotate2deg
!
!   prgmmr: parrish
!
!   Rotate right-handed spherical coordinate to new right-handed spherical
!   coordinate.  The coordinates are latitude (-90 to 90) and longitude.
!   Output for longitude is principle range of atan2d function ( -180 < rlon_out <= 180 )
!
! program history log:
!   2017-05-02  parrish
!
!  Method is as follows:
!  1.  define x,y,z coordinate system with origin at center of sphere,
!      x intersecting sphere at 0 deg N,  0 deg E,
!      y intersecting sphere at 0 deg N, 90 deg E,
!      z intersecting sphere at 90 deg N  (north pole).

!   4 steps:

!   1.  compute x,y,z from rlon_in, rlat_in

!   2.  rotate (x,y,z) about z axis by amount rlon0 -- (x,y,z) --> (xt,yt,zt)

!   3.  rotate (xt,yt,zt) about yt axis by amount rlat0 --- (xt,yt,zt) --> (xtt,ytt,ztt)

!   4.  compute rlon_out, rlat_out from xtt,ytt,ztt

!   This is the desired new orientation, where (0N, 0E) maps to point
!         (rlon0,rlat0) in original coordinate and the new equator is tangent to
!          the original latitude circle rlat0 at original longitude rlon0.
! attributes:
!   langauge: f90
!   machine:
!
!$$$ end documentation block


  use kinds, only: r_kind,i_kind
  use constants, only: deg2rad,rad2deg
  implicit none

  integer(i_kind), intent(in   ) :: nx,ny                 ! fv3 tile x- and y-dimensions
  real(r_kind),intent(in   ) :: rlon_in(nx,ny),rlat_in(nx,ny),rlon0,rlat0
  real(r_kind),intent(  out) :: rlon_out(nx,ny),rlat_out(nx,ny)

  real(r_kind) x,y,z, xt,yt,zt, xtt,ytt,ztt
  integer(i_kind) i,j

  do j=1,ny
     do i=1,nx
!   1.  compute x,y,z from rlon_in, rlat_in

        x=cos(rlat_in(i,j)*deg2rad)*cos(rlon_in(i,j)*deg2rad)
        y=cos(rlat_in(i,j)*deg2rad)*sin(rlon_in(i,j)*deg2rad)
        z=sin(rlat_in(i,j)*deg2rad)

!   2.  rotate (x,y,z) about z axis by amount rlon0 -- (x,y,z) --> (xt,yt,zt)

        xt= x*cos(rlon0*deg2rad)+y*sin(rlon0*deg2rad)
        yt=-x*sin(rlon0*deg2rad)+y*cos(rlon0*deg2rad)
        zt=z

!   3.  rotate (xt,yt,zt) about yt axis by amount rlat0 --- (xt,yt,zt) --> (xtt,ytt,ztt)

        xtt= xt*cos(rlat0*deg2rad)+zt*sin(rlat0*deg2rad)
        ytt= yt
        ztt=-xt*sin(rlat0*deg2rad)+zt*cos(rlat0*deg2rad)

!   4.  compute rlon_out, rlat_out from xtt,ytt,ztt

        rlat_out(i,j)=asin(ztt)*rad2deg
        rlon_out(i,j)=atan2(ytt,xtt)*rad2deg
     enddo
  enddo
end subroutine rotate2deg

subroutine unrotate2deg(rlon_in,rlat_in,rlon_out,rlat_out,rlon0,rlat0,nx,ny)
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    unrotate2deg
!
!   prgmmr: parrish
!
! abstract:  inverse of rotate2deg.
!
! program history log:
!   2017-05-02  parrish

! attributes:
!   langauge: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: deg2rad,rad2deg
  implicit none

  real(r_kind),intent(in   ) :: rlon_out(nx,ny),rlat_out(nx,ny),rlon0,rlat0
  integer(i_kind),intent(in   ) :: nx,ny
  real(r_kind),intent(  out) :: rlon_in(nx,ny),rlat_in(nx,ny)

  real(r_kind) x,y,z, xt,yt,zt, xtt,ytt,ztt
  integer(i_kind) i,j
  do j=1,ny
     do i=1,nx
        xtt=cos(rlat_out(i,j)*deg2rad)*cos(rlon_out(i,j)*deg2rad)
        ytt=cos(rlat_out(i,j)*deg2rad)*sin(rlon_out(i,j)*deg2rad)
        ztt=sin(rlat_out(i,j)*deg2rad)

        xt= xtt*cos(rlat0*deg2rad)-ztt*sin(rlat0*deg2rad)
        yt= ytt
        zt= xtt*sin(rlat0*deg2rad)+ztt*cos(rlat0*deg2rad)

        x= xt*cos(rlon0*deg2rad)-yt*sin(rlon0*deg2rad)
        y= xt*sin(rlon0*deg2rad)+yt*cos(rlon0*deg2rad)
        z= zt

        rlat_in(i,j)=asin(z)*rad2deg
        rlon_in(i,j)=atan2(y,x)*rad2deg
     enddo
  enddo

end subroutine unrotate2deg
