module general_tll2xy_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:    generic_tll2xy_mod  generalized conversion from latlon to xy
!   prgmmr: parrish          org: np22                date: 2010-10-28
!
! abstract: This module contains generalized tll2xy and related routines to convert
!             earth lat lon coordinates to grid coordinates for an orthogonal
!             non-staggered grid for which the earth lat and lon is known for each grid point.
!            NOTE: all routines tested against same routines in gridmod, using region_lat, region_lon first,
!                then random x,y,ug,vg.  get exact match between gridmod routines and the corresponding
!                  general_ routines here.  See subroutine test1_general_ll2xy at end of this module.
!
! program history log:
!   2010-10-28  parrish - initial documentation
!   2013-01-23  parrish - modify so calls to ll2rpolar and rpolar2ll avoid type mismatch error
!                           when using WCOSS intel debug compile.
!
! subroutines included:
!   sub general_create_llxy_transform - initialize type(llxy_cons) for desired grid
!   sub general_tll2xy                - convert from lat-lon to grid coordinates for desired grid
!   sub general_txy2ll                - convert from grid coordinates to lat-lon for desired grid
!   sub general_rotate_wind_ll2xy     - rotate earth wind to grid wind for desired grid
!   sub general_rotate_wind_xy2ll     - rotate grid wind to earth wind for desired grid
!
! Variable Definitions:
!   def llxy_cons:                    - type variable which is used to make parameter set previously
!                                         hardwired in gridmod.f90 portable for arbitrary grids.
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind,i_byte

   implicit none

! set default to private
   private
! set subroutines to public
   public :: general_create_llxy_transform
   public :: general_tll2xy
   public :: general_txy2ll
   public :: general_rotate_wind_ll2xy
   public :: general_rotate_wind_xy2ll
! set passed variables to public
   public :: llxy_cons

   type llxy_cons
! The following is for the generalized transform
      real(r_kind) rlon_min_dd,rlon_max_dd,rlat_min_dd,rlat_max_dd
      real(r_kind) pihalf,sign_pole,rlambda0
      real(r_kind) atilde_x,btilde_x,atilde_y,btilde_y
      real(r_kind) btilde_xinv,btilde_yinv
      integer(i_kind) nlon,nlat,nxtilde,nytilde
      integer(i_kind),pointer::i0_tilde(:,:) => NULL()
      integer(i_kind),pointer::j0_tilde(:,:) => NULL()
      integer(i_byte),pointer::ip_tilde(:,:) => NULL()
      integer(i_byte),pointer::jp_tilde(:,:) => NULL()
      real(r_kind),pointer::xtilde0(:,:) => NULL()
      real(r_kind),pointer::ytilde0(:,:) => NULL()
      real(r_kind),pointer::cos_beta_ref(:,:) => NULL()
      real(r_kind),pointer::sin_beta_ref(:,:) => NULL()
      real(r_kind),pointer::region_lat(:,:) => NULL()
      real(r_kind),pointer::region_lon(:,:) => NULL()
      logical:: lallocated = .false.

   end type llxy_cons

!  other declarations  ...

   contains

 subroutine general_create_llxy_transform(region_lat,region_lon,nlat,nlon,gt)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_create_llxy_transform
!   prgmmr:  parrish
!
! abstract:  copy routines from gridmod.f90 to make a general purpose module which allows
!     conversion of earth lat lons to grid coordinates for any non-staggered rectangular 
!     orthogonal grid with known earth lat and lon of each grid point.
!     set up constants to allow conversion between earth lat lon and analysis grid units.
!     There is no need to specify details of the analysis grid projection.  All that is required
!     is the earth latitude and longitude in radians of each analysis grid point.
!
! program history log:
!   2010-10-28  parrish - initial documentation
!   2012-11-28  tong - added gt%lallocated=.true. after arrays of gt are allocated and 
!                      removed the duplicate allocation of gt%region_lat,gt%region_lon
!                      at the begining
!
!   input argument list:
!    glats       - earth latitudes of each grid point for desired grid.
!    glons       - earth longitudes of each grid point for desired grid.
!
!   output argument list:
!    gt          - variable of type llxy_cons, which contains all information needed for 
!                   conversion between earth lat lon and this grid grid units.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use constants, only: zero,one,half,pi
  implicit none

  integer(i_kind),intent(in   ) :: nlat,nlon
  real(r_kind)   ,intent(in   ) :: region_lat(nlat,nlon),region_lon(nlat,nlon)
  type(llxy_cons),intent(inout) :: gt

  real(r_kind),parameter:: rbig =1.0e30_r_kind
  real(r_kind) xbar_min,xbar_max,ybar_min,ybar_max
  real(r_kind) clon,slon,r_of_lat,xbar,ybar
  integer(i_kind) i,j,istart0,iend,iinc,itemp,ilast,jlast
  real(r_kind) glats(nlon,nlat),glons(nlon,nlat)
  real(r_kind),allocatable:: clata(:,:),slata(:,:),clona(:,:),slona(:,:)
  real(r_kind) clat0,slat0,clon0,slon0
  real(r_kind) clat_m1,slat_m1,clon_m1,slon_m1
  real(r_kind) clat_p1,slat_p1,clon_p1,slon_p1
  real(r_kind) x,y,z,xt,yt,zt,xb,yb,zb
  real(r_kind) rlonb_m1,clonb_m1,slonb_m1
  real(r_kind) rlonb_p1,clonb_p1,slonb_p1
  real(r_kind) crot,srot

  do j=1,nlon
     do i=1,nlat
        glats(j,i)=region_lat(i,j)
        glons(j,i)=region_lon(i,j)
     end do
  end do
  gt%pihalf=half*pi
  gt%nlon=nlon
  gt%nlat=nlat
  gt%rlon_min_dd=one
  gt%rlat_min_dd=one
  gt%rlon_max_dd=nlon
  gt%rlat_max_dd=nlat

!  define xtilde, ytilde grid, transform

!      glons,glats are lons, lats of input grid points of dimension nlon,nlat
  call general_get_xytilde_domain(gt,gt%nlon,gt%nlat,glons,glats,gt%nxtilde,gt%nytilde, &
                   xbar_min,xbar_max,ybar_min,ybar_max)
  if(gt%lallocated) then
     deallocate(gt%i0_tilde,gt%j0_tilde,gt%ip_tilde,gt%jp_tilde,gt%xtilde0,gt%ytilde0)
     deallocate(gt%cos_beta_ref,gt%sin_beta_ref,gt%region_lat,gt%region_lon)
     gt%lallocated=.false.
  end if
  allocate(gt%i0_tilde(gt%nxtilde,gt%nytilde),gt%j0_tilde(gt%nxtilde,gt%nytilde))
  allocate(gt%ip_tilde(gt%nxtilde,gt%nytilde),gt%jp_tilde(gt%nxtilde,gt%nytilde))
  allocate(gt%xtilde0(gt%nlon,gt%nlat),gt%ytilde0(gt%nlon,gt%nlat))
  allocate(gt%cos_beta_ref(gt%nlon,gt%nlat),gt%sin_beta_ref(gt%nlon,gt%nlat))
  allocate(gt%region_lat(gt%nlat,gt%nlon),gt%region_lon(gt%nlat,gt%nlon))

  gt%lallocated=.true.

  do j=1,nlon
     do i=1,nlat
        gt%region_lat(i,j)=region_lat(i,j)
        gt%region_lon(i,j)=region_lon(i,j)
     end do
  end do

! define atilde_x, btilde_x, atilde_y, btilde_y

  gt%btilde_x   =(gt%nxtilde -one     )/(xbar_max-xbar_min)
  gt%btilde_xinv=(xbar_max-xbar_min)/(gt%nxtilde -one     )
  gt%atilde_x   =one-gt%btilde_x*xbar_min
  gt%btilde_y   =(gt%nytilde -one     )/(ybar_max-ybar_min)
  gt%btilde_yinv=(ybar_max-ybar_min)/(gt%nytilde -one     )
  gt%atilde_y   =one-gt%btilde_y*ybar_min

! define xtilde0,ytilde0
  do j=1,gt%nlat
     do i=1,gt%nlon
        r_of_lat=gt%pihalf+gt%sign_pole*glats(i,j)
        clon=cos(glons(i,j)+gt%rlambda0)
        slon=sin(glons(i,j)+gt%rlambda0)
        xbar=r_of_lat*clon
        ybar=r_of_lat*slon
        gt%xtilde0(i,j)=gt%atilde_x+gt%btilde_x*xbar
        gt%ytilde0(i,j)=gt%atilde_y+gt%btilde_y*ybar
     end do
  end do

!  now get i0_tilde, j0_tilde, ip_tilde,jp_tilde
  ilast=1 ; jlast=1
  istart0=gt%nxtilde
  iend=1
  iinc=-1
  do j=1,gt%nytilde
     itemp=istart0
     istart0=iend
     iend=itemp
     iinc=-iinc
     ybar=j
     do i=istart0,iend,iinc
        xbar=i
        call general_nearest_3(ilast,jlast,gt%i0_tilde(i,j),gt%j0_tilde(i,j), &
                       gt%ip_tilde(i,j),gt%jp_tilde(i,j),xbar,ybar,gt%nlon,gt%nlat,gt%xtilde0,gt%ytilde0)
     end do
  end do

!   new, more accurate and robust computation of cos_beta_ref and sin_beta_ref which is independent
!     of sign_pole and works for any orientation of grid on sphere (only restriction for now is that
!     x-y coordinate of analysis grid is right handed).
  allocate(clata(gt%nlon,gt%nlat),slata(gt%nlon,gt%nlat),clona(gt%nlon,gt%nlat),slona(gt%nlon,gt%nlat))
  do j=1,gt%nlat
     do i=1,gt%nlon
        clata(i,j)=cos(glats(i,j))
        slata(i,j)=sin(glats(i,j))
        clona(i,j)=cos(glons(i,j))
        slona(i,j)=sin(glons(i,j))
     end do
  end do
  do j=1,gt%nlat
     do i=2,gt%nlon-1

!     do all interior lon points to 2nd order accuracy

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)

!    now obtain new coordinates for m1 and p1 points.

        clat_m1=clata(i-1,j) ; slat_m1=slata(i-1,j) ; clon_m1=clona(i-1,j) ; slon_m1=slona(i-1,j)
        clat_p1=clata(i+1,j) ; slat_p1=slata(i+1,j) ; clon_p1=clona(i+1,j) ; slon_p1=slona(i+1,j)

        x=clat_m1*clon_m1 ; y=clat_m1*slon_m1 ; z=slat_m1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0

        rlonb_m1=atan2(-yb,-xb)   !  the minus signs here are so line for m1 is directed same
        clonb_m1=cos(rlonb_m1)
        slonb_m1=sin(rlonb_m1)

        x=clat_p1*clon_p1 ; y=clat_p1*slon_p1 ; z=slat_p1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0
        rlonb_p1=atan2(yb,xb)
        clonb_p1=cos(rlonb_p1)
        slonb_p1=sin(rlonb_p1)
        crot=half*(clonb_m1+clonb_p1)
        srot=half*(slonb_m1+slonb_p1)
        gt%cos_beta_ref(i,j)=crot*clon0-srot*slon0
        gt%sin_beta_ref(i,j)=srot*clon0+crot*slon0
     end do
!               now do i=1 and i=gt%nlon at 1st order accuracy
     i=1

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)
!    now obtain new coordinates for m1 and p1 points.

        clat_p1=clata(i+1,j) ; slat_p1=slata(i+1,j) ; clon_p1=clona(i+1,j) ; slon_p1=slona(i+1,j)

        x=clat_p1*clon_p1 ; y=clat_p1*slon_p1 ; z=slat_p1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0
        rlonb_p1=atan2(yb,xb)
        clonb_p1=cos(rlonb_p1)
        slonb_p1=sin(rlonb_p1)
        crot=clonb_p1
        srot=slonb_p1
        gt%cos_beta_ref(i,j)=crot*clon0-srot*slon0
        gt%sin_beta_ref(i,j)=srot*clon0+crot*slon0

     i=gt%nlon

!   transform so pole is at rlat0,rlon0 and 0 meridian is tangent to earth latitude at rlat0,rlon0.

        clat0=clata(i,j) ; slat0=slata(i,j) ; clon0=clona(i,j) ; slon0=slona(i,j)

!    now obtain new coordinates for m1 and p1 points.

        clat_m1=clata(i-1,j) ; slat_m1=slata(i-1,j) ; clon_m1=clona(i-1,j) ; slon_m1=slona(i-1,j)

        x=clat_m1*clon_m1 ; y=clat_m1*slon_m1 ; z=slat_m1
        xt=x*clon0+y*slon0 ; yt=-x*slon0+y*clon0 ; zt=z
        yb=zt*clat0-xt*slat0
        xb=yt
        zb=xt*clat0+zt*slat0

        rlonb_m1=atan2(-yb,-xb)   !  the minus signs here are so line for m1 is directed same
        clonb_m1=cos(rlonb_m1)
        slonb_m1=sin(rlonb_m1)

        crot=clonb_m1
        srot=slonb_m1
        gt%cos_beta_ref(i,j)=crot*clon0-srot*slon0
        gt%sin_beta_ref(i,j)=srot*clon0+crot*slon0
  end do
  deallocate(clata,slata,clona,slona)

end subroutine general_create_llxy_transform

subroutine general_tll2xy(gt,rlon,rlat,x,y,outside)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_tll2xy
!   prgmmr:  parrish
!
! abstract:  copy routines from gridmod.f90 to make a general purpose module which allows
!     conversion of earth lat lons to grid coordinates for any non-staggered rectangular 
!     orthogonal grid with known earth lat and lon of each grid point.
!     general_tll2xy converts earth lon-lat to x-y grid units of a
!           general regional rectangular domain.  Also, decides if
!           point is inside this domain.  As a result, there is
!           no restriction on type of horizontal coordinate for
!           a regional run, other than that it not have periodicity
!           or polar singularities.
!           This is done by first converting rlon, rlat to an
!           intermediate coordinate xtilde,ytilde, which has
!           precomputed pointers and constants for final conversion
!           to the desired x,y via 3 point inverse interpolation.
!           All of the information needed is derived from arrays
!           specifying earth latitude and longitude of every point
!           on the input grid.  Currently, the input x-y grid that
!           this is based on must be non-staggered.  This restriction
!           will eventually be lifted so we can run directly from
!           model grids that are staggered without first resorting
!           to interpolation of the guess to a non-staggered grid.
!
! program history log:
!   2010-10-28  parrish - initial documentation
!
!   input argument list:
!    gt          - type(llxy_cons) contains transform information to convert lat,lon to grid units x,y
!    rlon,rlat   - earth longitude and latitude (radians)
!
!   output argument list:
!    x,y         - grid coordinates in grid units.
!    outside     - if .false., then point is inside grid domain
!                    otherwise point is outside domain.
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use kinds, only: r_kind,i_kind
    use constants, only: one
    implicit none

    type(llxy_cons),intent(in) :: gt
    real(r_kind),intent(in   ) :: rlon
    real(r_kind),intent(in   ) :: rlat
    real(r_kind),intent(  out) :: x
    real(r_kind),intent(  out) :: y
    logical     ,intent(  out) :: outside

    real(r_kind) clon,slon,r_of_lat,xtilde,ytilde
    real(r_kind) dtilde,etilde
    real(r_kind) d1tilde,d2tilde,e1tilde,e2tilde,detinv
    integer(i_kind) itilde,jtilde
    integer(i_kind) i0,j0,ip,jp

!   first compute xtilde, ytilde

    clon=cos(rlon+gt%rlambda0)
    slon=sin(rlon+gt%rlambda0)
    r_of_lat=gt%pihalf+gt%sign_pole*rlat

    xtilde=gt%atilde_x+gt%btilde_x*r_of_lat*clon
    ytilde=gt%atilde_y+gt%btilde_y*r_of_lat*slon

!  next get interpolation information

    itilde=max(1,min(nint(xtilde),gt%nxtilde))
    jtilde=max(1,min(nint(ytilde),gt%nytilde))

    i0     =   gt%i0_tilde(itilde,jtilde)
    j0     =   gt%j0_tilde(itilde,jtilde)
    ip     =i0+gt%ip_tilde(itilde,jtilde)
    jp     =j0+gt%jp_tilde(itilde,jtilde)
    dtilde =xtilde-gt%xtilde0(i0,j0)
    etilde =ytilde-gt%ytilde0(i0,j0)
    d1tilde=(gt%xtilde0(ip,j0)-gt%xtilde0(i0,j0))*(ip-i0)
    d2tilde=(gt%xtilde0(i0,jp)-gt%xtilde0(i0,j0))*(jp-j0)
    e1tilde=(gt%ytilde0(ip,j0)-gt%ytilde0(i0,j0))*(ip-i0)
    e2tilde=(gt%ytilde0(i0,jp)-gt%ytilde0(i0,j0))*(jp-j0)
    detinv =one/(d1tilde*e2tilde-d2tilde*e1tilde)
    x = i0+detinv*(e2tilde*dtilde-d2tilde*etilde)
    y = j0+detinv*(d1tilde*etilde-e1tilde*dtilde)

    outside=x < gt%rlon_min_dd .or. x > gt%rlon_max_dd .or. &
            y < gt%rlat_min_dd .or. y > gt%rlat_max_dd

end subroutine general_tll2xy

subroutine general_txy2ll(gt,x,y,rlon,rlat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    general_txy2ll
!   prgmmr:  parrish
!
! abstract: convert x-y grid units to earth lat-lon coordinates
!
! program history log:
!   2010-10-28 parrish - initial documentation
!
!   input argument list:
!    gt          - type(llxy_cons) contains transform information to convert x,y to lat,lon
!    x,y         - grid coordinates in grid units.
!
!   output argument list:
!    rlon,rlat   - earth longitude and latitude (radians)
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use kinds, only: r_kind,i_kind
    use constants, only: one
    implicit none

    type(llxy_cons),intent(in) :: gt
    real(r_kind),intent(in   ) :: x
    real(r_kind),intent(in   ) :: y
    real(r_kind),intent(  out) :: rlon
    real(r_kind),intent(  out) :: rlat

    real(r_kind) r_of_lat,xtilde,ytilde
    real(r_kind) dtilde,etilde,xbar,ybar
    real(r_kind) d1tilde,d2tilde,e1tilde,e2tilde
    integer(i_kind) i0,j0,ip,jp

    i0=nint(x)
    j0=nint(y)
    i0=max(1,min(i0,gt%nlon))
    j0=max(1,min(j0,gt%nlat))
    ip=i0+nint(sign(one,x-i0))
    jp=j0+nint(sign(one,y-j0))
    if(ip<1) then
       i0=2
       ip=1
    end if
    if(jp<1) then
       j0=2
       jp=1
    end if
    if(ip>gt%nlon) then
       i0=gt%nlon-1
       ip=gt%nlon
    end if
    if(jp>gt%nlat) then
       j0=gt%nlat-1
       jp=gt%nlat
    end if
    d1tilde=(gt%xtilde0(ip,j0)-gt%xtilde0(i0,j0))*(ip-i0)
    d2tilde=(gt%xtilde0(i0,jp)-gt%xtilde0(i0,j0))*(jp-j0)
    e1tilde=(gt%ytilde0(ip,j0)-gt%ytilde0(i0,j0))*(ip-i0)
    e2tilde=(gt%ytilde0(i0,jp)-gt%ytilde0(i0,j0))*(jp-j0)
    dtilde =d1tilde*(x-i0) +d2tilde*(y-j0)
    etilde =e1tilde*(x-i0) +e2tilde*(y-j0)
    xtilde =dtilde         +gt%xtilde0(i0,j0)
    ytilde =etilde         +gt%ytilde0(i0,j0)

    xbar=(xtilde-gt%atilde_x)*gt%btilde_xinv
    ybar=(ytilde-gt%atilde_y)*gt%btilde_yinv
    r_of_lat=sqrt(xbar**2+ybar**2)
    rlat=(r_of_lat-gt%pihalf)*gt%sign_pole
    rlon=atan2(ybar,xbar)-gt%rlambda0

end subroutine general_txy2ll

subroutine general_nearest_3(ilast,jlast,i0,j0,ip,jp,x,y,nx0,ny0,x0,y0)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nearest_3
!   prgmmr:
!
! abstract: find closest 3 points to (x,y) on grid defined by x0,y0
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    ilast,jlast
!    nx0,ny0
!    x,y
!    x0,y0
!
!   output argument list:
!    ilast,jlast
!    i0,j0
!    ip,jp
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,i_byte
  implicit none

  integer(i_kind),intent(inout) :: ilast,jlast
  integer(i_kind),intent(  out) :: i0,j0
  integer(i_byte),intent(  out) :: ip,jp
  integer(i_kind),intent(in   ) :: nx0,ny0
  real(r_kind)   ,intent(in   ) :: x,y
  real(r_kind)   ,intent(in   ) :: x0(nx0,ny0),y0(nx0,ny0)
 
  real(r_kind) dista,distb,dist2,dist2min
  integer(i_kind) i,inext,j,jnext

  do
     i0=ilast
     j0=jlast
     dist2min=huge(dist2min)
     inext=0
     jnext=0
     do j=max(j0-1,1),min(j0+1,ny0)
        do i=max(i0-1,1),min(i0+1,nx0)
           dist2=(x-x0(i,j))**2+(y-y0(i,j))**2
           if(dist2<dist2min) then
              dist2min=dist2
              inext=i
              jnext=j
           end if
        end do
     end do
     if(inext==i0.and.jnext==j0) exit
     ilast=inext
     jlast=jnext
  end do

!  now find which way to go in x for second point

  ip=0
  if(i0==nx0)  ip=-1
  if(i0==1) ip=1
  if(ip==0) then
     dista=(x-x0(i0-1,j0))**2+(y-y0(i0-1,j0))**2
     distb=(x-x0(i0+1,j0))**2+(y-y0(i0+1,j0))**2
     if(distb<dista) then
        ip=1
     else
        ip=-1
     end if
  end if

!  repeat for y for 3rd point

  jp=0
  if(j0==ny0  ) jp=-1
  if(j0==1 ) jp=1
  if(jp==0) then
     dista=(x-x0(i0,j0-1))**2+(y-y0(i0,j0-1))**2
     distb=(x-x0(i0,j0+1))**2+(y-y0(i0,j0+1))**2
     if(distb<dista) then
        jp=1
     else
        jp=-1
     end if
  end if

  ilast=i0
  jlast=j0
    
end subroutine general_nearest_3

subroutine general_get_xytilde_domain(gt,nx0,ny0,rlons0,rlats0, &
                                  nx,ny,xminout,xmaxout,yminout,ymaxout)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_xytilde_domain
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    nx0,ny0
!    rlons0,rlats0
!
!   output argument list:
!    nx,ny
!    xminout,xmaxout,yminout,ymaxout
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

   use kinds, only: r_kind,i_kind
   use constants, only: one, deg2rad,half,zero,r10
!  define parameters for xy domain which optimally overlays input grid

  implicit none

  type(llxy_cons),intent(inout) :: gt
  integer(i_kind),intent(in   ) :: nx0,ny0
  real(r_kind)   ,intent(in   ) :: rlons0(nx0,ny0),rlats0(nx0,ny0)

  integer(i_kind),intent(  out) :: nx,ny
  real(r_kind)   ,intent(  out) :: xminout,xmaxout,yminout,ymaxout

  real(r_kind),parameter:: r37=37.0_r_kind

  real(r_kind) area,areamax,areamin,extra,rlats0max,rlats0min,testlambda
  real(r_kind) xthis,ythis
  integer(i_kind) i,ip1,j,jp1,m

  real(r_kind) coslon0(nx0,ny0),sinlon0(nx0,ny0)
  real(r_kind) coslat0(nx0,ny0),sinlat0(nx0,ny0)
  real(r_kind) count,delbar
  real(r_kind) dx,dy,disti,distj,distmin,distmax
  real(r_kind) xmin,xmax,ymin,ymax

!  get range of lats for input grid

  rlats0max=maxval(rlats0) ; rlats0min=minval(rlats0)

!   assign hemisphere ( parameter sign_pole )

  if(rlats0min>-r37*deg2rad) gt%sign_pole=-one   !  northern hemisphere xy domain
  if(rlats0max< r37*deg2rad) gt%sign_pole= one   !  southern hemisphere xy domain


!   get optimum rotation angle rlambda0

  areamin= huge(areamin)
  areamax=-huge(areamax)
  do m=0,359
     testlambda=m*deg2rad
     xmax=-huge(xmax)
     xmin= huge(xmin)
     ymax=-huge(ymax)
     ymin= huge(ymin)
     do j=1,ny0,ny0-1
        do i=1,nx0
           xthis=(gt%pihalf+gt%sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(gt%pihalf+gt%sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     do j=1,ny0
        do i=1,nx0,nx0-1
           xthis=(gt%pihalf+gt%sign_pole*rlats0(i,j))*cos(rlons0(i,j)+testlambda)
           ythis=(gt%pihalf+gt%sign_pole*rlats0(i,j))*sin(rlons0(i,j)+testlambda)
           xmax=max(xmax,xthis)
           ymax=max(ymax,ythis)
           xmin=min(xmin,xthis)
           ymin=min(ymin,ythis)
        end do
     end do
     area=(xmax-xmin)*(ymax-ymin)
     areamax=max(area,areamax)
     if(area<areamin) then
        areamin =area
        gt%rlambda0=testlambda
        xmaxout =xmax
        xminout =xmin
        ymaxout =ymax
        yminout =ymin
     end if
  end do


!   now determine resolution of input grid and choose nx,ny of xy grid accordingly
!                 (currently hard-wired at 1/2 the average input grid increment)

  do j=1,ny0
     do i=1,nx0
        coslon0(i,j)=cos(one*rlons0(i,j)) ; sinlon0(i,j)=sin(one*rlons0(i,j))
        coslat0(i,j)=cos(one*rlats0(i,j)) ; sinlat0(i,j)=sin(one*rlats0(i,j))
     end do
  end do

  delbar=zero
  count =zero
  do j=1,ny0-1
     jp1=j+1
     do i=1,nx0-1
        ip1=i+1
        disti=acos(sinlat0(i,j)*sinlat0(ip1,j)+coslat0(i,j)*coslat0(ip1,j)* &
                  (sinlon0(i,j)*sinlon0(ip1,j)+coslon0(i,j)*coslon0(ip1,j)))
        distj=acos(sinlat0(i,j)*sinlat0(i,jp1)+coslat0(i,j)*coslat0(i,jp1)* &
                  (sinlon0(i,j)*sinlon0(i,jp1)+coslon0(i,j)*coslon0(i,jp1)))
        distmax=max(disti,distj)
        distmin=min(disti,distj)
        delbar=delbar+distmax
        count=count+one
     end do
  end do
  delbar=delbar/count
  dx=half*delbar
  dy=dx

!   add extra space to computational grid to push any boundary problems away from
!     area of interest

  extra=r10*dx
  xmaxout=xmaxout+extra
  xminout=xminout-extra
  ymaxout=ymaxout+extra
  yminout=yminout-extra
  nx=1+(xmaxout-xminout)/dx
  ny=1+(ymaxout-yminout)/dy
 
end subroutine general_get_xytilde_domain

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  general_rotate_wind_ll2xy ---  Rotate earth vector wind
!
! !INTERFACE:
!
  subroutine general_rotate_wind_ll2xy(gt,u0,v0,u,v,rlon0,x,y)

! !USES:

    use kinds, only: r_kind,i_kind
    use constants, only: one,two,one_tenth
    implicit none

! !INPUT PARAMETERS:

    type(llxy_cons),intent(in) :: gt
    real(r_kind),intent(in   ) :: u0,v0        ! earth wind component
    real(r_kind),intent(in   ) :: rlon0        ! earth   lon (radians)
    real(r_kind),intent(in   ) :: x,y          ! local x,y coordinate (grid units)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: u,v          ! rotated coordinate of winds

! !DESCRIPTION: to convert earth vector wind components to corresponding
!           local x,y coordinate
!
! !REVISION HISTORY:
!   2003-09-30  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2010-09-08  parrish, remove sign_pole variable--no longer needed, due to more accurate and
!                 robust computation of reference wind rotation angle defined by
!                 cos_beta_ref, sin_beta_ref.
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------

  real(r_kind) beta,delx,delxp,dely,delyp
  real(r_kind) sin_beta,cos_beta
  integer(i_kind) ix,iy

!  interpolate departure from longitude part of angle between earth positive east and local positive x

  ix=x
  iy=y
  ix=max(1,min(ix,gt%nlon-1))
  iy=max(1,min(iy,gt%nlat-1))
  delx=x-ix
  dely=y-iy
  delxp=one-delx
  delyp=one-dely
  cos_beta=gt%cos_beta_ref(ix  ,iy  )*delxp*delyp+gt%cos_beta_ref(ix+1,iy  )*delx *delyp+ &
           gt%cos_beta_ref(ix  ,iy+1)*delxp*dely +gt%cos_beta_ref(ix+1,iy+1)*delx *dely
  sin_beta=gt%sin_beta_ref(ix  ,iy  )*delxp*delyp+gt%sin_beta_ref(ix+1,iy  )*delx *delyp+ &
           gt%sin_beta_ref(ix  ,iy+1)*delxp*dely +gt%sin_beta_ref(ix+1,iy+1)*delx *dely
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u= u0*cos(beta-rlon0)+v0*sin(beta-rlon0)
  v=-u0*sin(beta-rlon0)+v0*cos(beta-rlon0)

end subroutine general_rotate_wind_ll2xy

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  rotate_wind_xy2ll ---  Unrotate earth vector wind
!
! !INTERFACE:
!
  subroutine general_rotate_wind_xy2ll(gt,u,v,u0,v0,rlon0,x,y)

! !USES:

    use kinds, only: r_kind,i_kind
    use constants, only: one
    implicit none

! !INPUT PARAMETERS:

    type(llxy_cons),intent(in) :: gt
    real(r_kind),intent(in   ) :: u,v         ! rotated coordinate winds
    real(r_kind),intent(in   ) :: rlon0       ! earth   lon     (radians)
    real(r_kind),intent(in   ) :: x,y         ! rotated lon/lat (radians)

! !OUTPUT PARAMETERS:

    real(r_kind),intent(  out) :: u0,v0       ! earth winds

! !DESCRIPTION: rotate u,v in local x,y coordinate to u0,v0 in earth 
!           lat, lon coordinate
!
! !REVISION HISTORY:
!   2003-09-30  parrish
!   2004-05-13  kleist, documentation
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-20  todling, fixed description
!   2010-09-08  parrish, remove sign_pole variable--no longer needed, due to more accurate and
!                 robust computation of reference wind rotation angle defined by
!                 cos_beta_ref, sin_beta_ref.
!
! !REMARKS:
!   language: f90
!   machine:  ibm rs/6000 sp; SGI Origin 2000; Compaq/HP
!
! !AUTHOR:
!   parrish          org: np22                date: 2003-09-30
!
!EOP
!-------------------------------------------------------------------------
  real(r_kind) beta,delx,delxp,dely,delyp
  real(r_kind) sin_beta,cos_beta
  integer(i_kind) ix,iy

!  interpolate departure from longitude part of angle between earth 
!  positive east and local positive x

  ix=x
  iy=y
  ix=max(1,min(ix,gt%nlon-1))
  iy=max(1,min(iy,gt%nlat-1))
  delx=x-ix
  dely=y-iy
  delxp=one-delx
  delyp=one-dely
  cos_beta=gt%cos_beta_ref(ix  ,iy  )*delxp*delyp+gt%cos_beta_ref(ix+1,iy  )*delx *delyp+ &
           gt%cos_beta_ref(ix  ,iy+1)*delxp*dely +gt%cos_beta_ref(ix+1,iy+1)*delx *dely
  sin_beta=gt%sin_beta_ref(ix  ,iy  )*delxp*delyp+gt%sin_beta_ref(ix+1,iy  )*delx *delyp+ &
           gt%sin_beta_ref(ix  ,iy+1)*delxp*dely +gt%sin_beta_ref(ix+1,iy+1)*delx *dely
  beta=atan2(sin_beta,cos_beta)

!  now rotate;

  u0= u*cos(beta-rlon0)-v*sin(beta-rlon0)
  v0= u*sin(beta-rlon0)+v*cos(beta-rlon0)

end subroutine general_rotate_wind_xy2ll

end module general_tll2xy_mod

subroutine merge_grid_e_to_grid_a_initialize(region_lat_e,region_lon_e,region_lat_a,region_lon_a, &
                                   nlat_e,nlon_e,nlat_a,nlon_a,nord_e2a,nord_blend,nmix,gt_e,gt_a,p_e2a)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    merge_grid_e_to_grid_a_initialize
!   prgmmr:  parrish
!
! abstract:  initialize parameters needed for routines merge_grid_e_to_grid_a and merge_vgrid_e_to_vgrid_a.
!
! program history log:
!   2010-10-28  parrish - initial documentation
!   2012-03-01  tong - modified the way to call create_egrid2points_slow. If nmix <= 0, the orginal 
!               way to call the subroutine will cause segmentation fault
!
!   input argument list:
!    region_lat_e - earth lats for e grid (radians) 
!    region_lon_e - earth lons for e grid (radians)
!    region_lat_a - earth lats for a grid (radians)
!    region_lon_a - earth lons for a grid (radians)
!    nlat_e       - "y" dimension of e grid
!    nlon_e       - "x" dimension of e grid
!    nlat_a       - "y" dimension of a grid
!    nlon_a       - "x" dimension of a grid
!    nord_e2a     - order of accuracy of interpolation from grid e to grid a (1=bilinear, 2=biquadratic, etc)
!    nord_blend   - order of continuity of blend function (1=continuous 1st derivative, etc)
!    nmix         - width of blend zone on edge of e grid in e grid units.
!
!   output argument list:
!    gt_e         - navigation structure variable for grid e
!    gt_a         - navigation structure variable for grid a
!    p_e2a        - structure variable containing interpolation info for interpolation from e grid to a grid
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use general_tll2xy_mod, only: llxy_cons,general_create_llxy_transform,general_tll2xy
  use egrid2agrid_mod, only: egrid2agrid_parm,create_egrid2points_slow
  use constants, only: zero,one
  implicit none

  type(llxy_cons),intent(inout):: gt_e,gt_a
  type(egrid2agrid_parm),intent(inout) :: p_e2a
  integer(i_kind),intent(in):: nlat_e,nlon_e,nlat_a,nlon_a,nord_e2a,nord_blend,nmix
  real(r_kind),intent(in)::region_lat_e(nlat_e,nlon_e),region_lon_e(nlat_e,nlon_e)
  real(r_kind),intent(in)::region_lat_a(nlat_a,nlon_a),region_lon_a(nlat_a,nlon_a)

  integer(i_kind) i,j,np
  real(r_kind),allocatable::xa_e(:,:),ya_e(:,:),xe(:),ye(:)
  logical outside
  real(r_kind) diffmax,range_lat,range_lon

! define navigation for e grid and a grid

  if(nlat_e == nlat_a .and. nlon_e == nlon_a) then
     range_lat=max(abs(region_lat_a(nlat_a,1)-region_lat_a(1,1)),abs(region_lat_a(nlat_a,nlon_a)-region_lat_a(1,nlon_a)))
     range_lat=max(range_lat,abs(region_lat_a(nlat_a,nlon_a/2)-region_lat_a(1,nlon_a/2)))
     range_lat=max(range_lat,abs(region_lat_e(nlat_e,1)-region_lat_e(1,1)))
     range_lat=max(range_lat,abs(region_lat_e(nlat_e,nlon_e)-region_lat_e(1,nlon_e)))
     range_lat=max(range_lat,abs(region_lat_e(nlat_e,nlon_e/2)-region_lat_e(1,nlon_e/2)))
     if(nlat_a == 1) range_lat=one
     range_lon=max(abs(region_lon_a(1,nlon_a)-region_lon_a(1,1)),abs(region_lon_a(nlat_a,nlon_a)-region_lon_a(nlat_a,1)))
     range_lon=max(range_lon,abs(region_lon_a(nlat_a/2,nlon_a)-region_lon_a(nlat_a/2,1)))
     range_lon=max(range_lon,abs(region_lon_e(1,nlon_e)-region_lon_e(1,1)))
     range_lon=max(range_lon,abs(region_lon_e(nlat_e,nlon_e)-region_lon_e(nlat_e,1)))
     range_lon=max(range_lon,abs(region_lon_e(nlat_e/2,nlon_e)-region_lon_e(nlat_e/2,1)))
     if(nlon_a == 1) range_lon=one
     diffmax=zero
     do j=1,nlon_a
        do i=1,nlat_a
           diffmax=max(diffmax,abs(region_lat_a(i,j)-region_lat_e(i,j))/range_lat)
        end do
     end do
     do j=1,nlon_a
        do i=1,nlat_a
           diffmax=max(diffmax,abs(region_lon_a(i,j)-region_lon_e(i,j))/range_lon)
        end do
     end do
     if(diffmax < .0000001_r_kind)then
        p_e2a%identity=.true.
        return
     end if
  end if

  call general_create_llxy_transform(region_lat_e,region_lon_e,nlat_e,nlon_e,gt_e)
  call general_create_llxy_transform(region_lat_a,region_lon_a,nlat_a,nlon_a,gt_a)

!    obtain xa_e, ya_e, the coordinates of the a grid points in e-grid measure.

  allocate(xa_e(nlat_a,nlon_a),ya_e(nlat_a,nlon_a))
  do j=1,nlon_a
     do i=1,nlat_a
        call general_tll2xy(gt_e,region_lon_a(i,j),region_lat_a(i,j),xa_e(i,j),ya_e(i,j),outside)
     end do
  end do

  allocate(xe(nlon_e),ye(nlat_e))
  do j=1,nlon_e
     xe(j)=j
  end do
  do i=1,nlat_e
     ye(i)=i
  end do
  np=nlat_a*nlon_a
  if(nord_blend > 0 .and. nmix > 0)then
     call create_egrid2points_slow(np,ya_e,xa_e,nlat_e,ye,nlon_e,xe,nord_e2a,p_e2a,nord_blend,nmix)
  else
     call create_egrid2points_slow(np,ya_e,xa_e,nlat_e,ye,nlon_e,xe,nord_e2a,p_e2a)
  end if

  deallocate(xe,ye,xa_e,ya_e)

end subroutine merge_grid_e_to_grid_a_initialize

subroutine merge_grid_e_to_grid_a(e_in,a_in,a_out,gt_e,gt_a,p_e2a)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    merge_grid_e_to_grid_a  merge scalar field from one grid to another
!   prgmmr: parrish          org: np22                date: 2010-11-02
!
! abstract: interpolate input scalar field e_in from e grid to a grid, blend with existing
!             field a_in, and return result in a_out.
!
! program history log:
!   2010-11-02  parrish, initial documentation
!
!   input argument list:
!      e_in:             input field on e-grid
!      a_in:             input field on a-grid
!      gt_e:             structure variable containing navigation info and dimensions for e_in
!      gt_a:             same for a_in, a_out
!      p_e2a:            structure variable containing info for interpolating from e-grid to a-grid.
!
!   output argument list:
!      a_out:            blended field with interpolated e-grid field smoothly replacing
!                           region of a-grid covered by e-grid.
!              
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use general_tll2xy_mod, only: llxy_cons
  use egrid2agrid_mod, only: egrid2agrid_parm,egrid2points
  use constants, only: one
  implicit none

  type(llxy_cons),intent(in):: gt_e,gt_a
  type(egrid2agrid_parm),intent(in):: p_e2a
  real(r_kind),intent(in),dimension(gt_e%nlat,gt_e%nlon):: e_in
  real(r_kind),intent(in),dimension(gt_a%nlat,gt_a%nlon):: a_in
  real(r_kind),intent(out),dimension(gt_a%nlat,gt_a%nlon):: a_out

  integer(i_kind) i,ii,j

  call egrid2points(p_e2a,e_in,a_out)
  ii=0
  do j=1,gt_a%nlon
     do i=1,gt_a%nlat
        ii=ii+1
!   blend smoothly with a_in
        a_out(i,j)=p_e2a%blend(ii)*a_out(i,j)+(one-p_e2a%blend(ii))*a_in(i,j)
!        a_out(i,j)=blend(i,j)*a_out(i,j)+(one-blend(i,j))*a_in(i,j)
     end do
  end do

end subroutine merge_grid_e_to_grid_a

subroutine merge_vgrid_e_to_vgrid_a(eu_in,ev_in,au_in,av_in,au_out,av_out,gt_e,gt_a,p_e2a)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    merge_vgrid_e_to_vgrid_a  merge vector field from one grid to another
!   prgmmr: parrish          org: np22                date: 2010-11-02
!
! abstract: interpolate input vector field eu_in,ev_in from e grid to a grid, blend with existing
!             vector field au_in, av_in, and return result in au_out,av_out.
!
! program history log:
!   2010-11-02  parrish, initial documentation
!
!   input argument list:
!      eu_in:            input u component on e-grid
!      ev_in:            input v component on e-grid
!      au_in:            input u component on a-grid
!      av_in:            input v component on a-grid
!      gt_e:             structure variable containing navigation info and dimensions for eu_in, ev_in
!      gt_a:             same for au_in, av_in, au_out,av_out
!      p_e2a:            structure variable containing info for interpolating from e-grid to a-grid.
!
!   output argument list:
!      au_out:           blended u component with interpolated e-grid u component smoothly replacing
!                           region of a-grid covered by e-grid.
!      av_out:           same for v component
!              
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: r_kind,i_kind
  use general_tll2xy_mod, only: llxy_cons,general_rotate_wind_ll2xy,general_rotate_wind_xy2ll
  use egrid2agrid_mod, only: egrid2agrid_parm,egrid2points
  use constants, only: one
  implicit none

  type(llxy_cons),intent(in):: gt_e,gt_a
  type(egrid2agrid_parm),intent(in):: p_e2a
  real(r_kind),intent(in),dimension(gt_e%nlat,gt_e%nlon):: eu_in,ev_in
  real(r_kind),intent(in),dimension(gt_a%nlat,gt_a%nlon):: au_in,av_in
  real(r_kind),intent(out),dimension(gt_a%nlat,gt_a%nlon):: au_out,av_out

  integer(i_kind) i,ii,j
  real(r_kind) xa,ya,au_earth,av_earth

  call egrid2points(p_e2a,eu_in,au_out)
  call egrid2points(p_e2a,ev_in,av_out)
  ii=0
  do j=1,gt_a%nlon
     xa=j
     do i=1,gt_a%nlat
        ya=i
        ii=ii+1
!    rotate vector from e grid orientation to earth lat-lon orientation
        call general_rotate_wind_xy2ll(gt_e,au_out(i,j),av_out(i,j),au_earth,av_earth, &
                                       gt_a%region_lon(i,j),p_e2a%xa_e(ii),p_e2a%ya_e(ii))
!    rotate vector from earth lat-lon orientation to a grid orientation
        call general_rotate_wind_ll2xy(gt_a,au_earth,av_earth,au_out(i,j),av_out(i,j), &
                                       gt_a%region_lon(i,j),xa,ya)
!   blend smoothly with au_in, av_in
        au_out(i,j)=p_e2a%blend(ii)*au_out(i,j)+(one-p_e2a%blend(ii))*au_in(i,j)
        av_out(i,j)=p_e2a%blend(ii)*av_out(i,j)+(one-p_e2a%blend(ii))*av_in(i,j)
!        au_out(i,j)=blend(i,j)*au_out(i,j)+(one-blend(i,j))*au_in(i,j)
!        av_out(i,j)=blend(i,j)*av_out(i,j)+(one-blend(i,j))*av_in(i,j)
     end do
  end do

end subroutine merge_vgrid_e_to_vgrid_a

subroutine test1_general_ll2xy

!  compare subroutines tll2xy with general_tll2xy, etc. also for txy2ll, rotate_wind_xy2ll, rotate_wind_ll2xy
!   using analysis grid defined by region_lon,region_lat, nlon, nlat from gridmod.f90

  use gridmod, only: region_lat,region_lon,nlat,nlon,tll2xy,txy2ll,rotate_wind_xy2ll,rotate_wind_ll2xy
  use general_tll2xy_mod, only: llxy_cons,general_create_llxy_transform,general_tll2xy,general_txy2ll
  use general_tll2xy_mod, only: general_rotate_wind_ll2xy,general_rotate_wind_xy2ll
  use mpimod, only: mype
  use kinds, only: r_kind,i_kind
  use constants, only: zero
  implicit none

  type(llxy_cons) gt_a
  integer(i_kind) i,j
  real(r_kind) grid_lats(nlat,nlon),grid_lons(nlat,nlon)
  real(r_kind) grid_lats_test(nlat,nlon),grid_lons_test(nlat,nlon)
  real(r_kind) rlat,rlon,y,x,rlat_test,rlon_test,errmax_lat,errmax_lon
  real(r_kind) u,v,ug,vg,utest,vtest,ugtest,vgtest,errmax_w,errmax_wg
  real(r_kind) xmax,ymax,wgmax
  logical outside

  call general_create_llxy_transform(region_lat,region_lon,nlat,nlon,gt_a)

!             first convert region_lat,region_lon to grid units for both tll2xy and general_tll2xy
  do j=1,nlon
     do i=1,nlat
        call general_tll2xy(gt_a,region_lon(i,j),region_lat(i,j), &
                            grid_lons_test(i,j),grid_lats_test(i,j),outside)
        call tll2xy(region_lon(i,j),region_lat(i,j),grid_lons(i,j),grid_lats(i,j),outside)
     end do
  end do
!              compare grid_lons_test, grid_lats_test to grid_lons,grid_lats--should be identical
  if(mype==0) then
      write(0,*)' min,max grid_lons=',minval(grid_lons),maxval(grid_lons)
      write(0,*)' min,max grid_lons_test=',minval(grid_lons_test),maxval(grid_lons_test)
      write(0,*)' min,max grid_lats=',minval(grid_lats),maxval(grid_lats)
      write(0,*)' min,max grid_lats_test=',minval(grid_lats_test),maxval(grid_lats_test)
      write(0,*)' max err grid_lons_test=',maxval(abs(grid_lons-grid_lons_test))
      write(0,*)' max err grid_lats_test=',maxval(abs(grid_lats-grid_lats_test))
  end if
  errmax_lat=zero
  errmax_lon=zero
  errmax_w=zero
  errmax_wg=zero
  xmax=zero
  ymax=zero
  wgmax=zero
  
  
!             repeat for random x,y and random ug,vg to also test rotate_wind_xy2ll, rotate_wind_ll2xy
!                  against general_rotate_wind_xy2ll, general_rotate_wind_ll2xy
  do i=1,nlon*nlat
     call random_number(x)
     call random_number(y)
     call random_number(ug)
     call random_number(vg)
     wgmax=max(sqrt(ug**2+vg**2),wgmax)
     x=-4._r_kind+(nlon+8._r_kind)*x
     y=-4._r_kind+(nlat+8._r_kind)*y
     xmax=max(xmax,x)
     ymax=max(ymax,y)
     call general_txy2ll(gt_a,x,y,rlon_test,rlat_test)
     call txy2ll(x,y,rlon,rlat)
     errmax_lat=max(abs(rlat_test-rlat),errmax_lat)
     errmax_lon=max(abs(rlon_test-rlon),errmax_lon)
     call general_rotate_wind_xy2ll(gt_a,ug,vg,utest,vtest,rlon_test,x,y)
     call rotate_wind_xy2ll(ug,vg,u,v,rlon,x,y)
     errmax_w=max(sqrt((u-utest)**2+(v-vtest)**2),errmax_w)
     call general_rotate_wind_ll2xy(gt_a,utest,vtest,ugtest,vgtest,rlon_test,x,y)
     call rotate_wind_ll2xy(u,v,ug,vg,rlon,x,y)
     errmax_wg=max(sqrt((ug-ugtest)**2+(vg-vgtest)**2),errmax_wg)
  end do
!     check differences--should be zero
  if(mype==0) then
     write(0,*)' for txy2ll, errmax_lon,lat=',errmax_lon,errmax_lat
     write(0,*)' for rotate_wind, errmax_w,wg=',errmax_w,errmax_wg
     write(0,*)' for rotate_wind, xmax,ymax,wgmax=',xmax,ymax,wgmax
  end if
     
end subroutine test1_general_ll2xy

subroutine test3_egrid2points

!    test new subroutine egrid2points, which interpolates from e-grid to a-grid, where e-grid can be
!      different projection than a-grid, and need not completely cover a-grid.

!  (there seems to be a problem with interpolating vector fields that is related to how they are rotated
!    after interpolation--I am looking into this).

  use gridmod, only: region_lat,region_lon,nlat,nlon,tll2xy,txy2ll,rotate_wind_xy2ll,rotate_wind_ll2xy
  use general_tll2xy_mod, only: llxy_cons,general_create_llxy_transform,general_tll2xy,general_txy2ll
  use general_tll2xy_mod, only: general_rotate_wind_ll2xy,general_rotate_wind_xy2ll
  use mpimod, only: mype
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: zero,one,two,pi,rad2deg
  use egrid2agrid_mod, only: egrid2agrid_parm,create_egrid2points_slow,egrid2points
  implicit none

  type(llxy_cons) gt_e,gt_a
  type(egrid2agrid_parm) p_e2a
  integer(i_kind) i,j,nye,nxe,nord_e2a
  real(r_kind) y(1),x(1),errmax,fmax
  real(r_kind) region_lat1(1),region_lon1(1)
  real(r_kind) rotate3,xmin,xmax,ymin,ymax
  real(r_kind),allocatable,dimension(:,:)::region_lat_e,region_lon_e
  real(r_kind),allocatable,dimension(:,:)::testlona,testlata,test_stream_e,test_stream_a
  real(r_kind),allocatable,dimension(:,:)::exact_stream_a
  real(r_single),allocatable:: out1(:,:),out2(:,:)
  real(r_kind),allocatable,dimension(:,:)::ue,ve,ua,va,uearth_a,vearth_a,uearth_e,vearth_e,ua_exact,va_exact
  real(r_kind) uearth_test,vearth_test,xe,ye,xa,ya
  integer(i_kind) nmix,nord_blend
  integer(i_kind) ii

  nmix=10
  nord_blend=4
  

!  1.  define subgrid which is rotated with respect to analysis grid
!       the way perhaps to do this is to define a rotated polar-stereo grid 
!        then get lats and lons of this grid using rpolar2ll.

                       if(mype==0) write(0,*)' at 1 in test3'
  rotate3=pi/4._r_kind
  xmin=huge(xmin)
  xmax=-huge(xmax)
  ymin=huge(ymin)
  ymax=-huge(ymax)
  do j=nlon/3,2*nlon/3
     do i=nlat/3,2*nlat/3
        region_lat1(1)=region_lat(i,j)
        region_lon1(1)=region_lon(i,j)
        call ll2rpolar(region_lat1,region_lon1,1,x,y, &
                       region_lat(nlat/2,nlon/2),region_lon(nlat/2,nlon/2),rotate3)
        xmin=min(x(1),xmin)
        xmax=max(x(1),xmax)
        ymin=min(y(1),ymin)
        ymax=max(y(1),ymax)
     end do
  end do
                       if(mype==0) write(0,*)' min,max(region_lat)=',minval(region_lat),maxval(region_lat)
  xmin=.8_r_kind*xmin
  xmax=.8_r_kind*xmax
  ymin=.8_r_kind*ymin
  ymax=.8_r_kind*ymax
                 if(mype==0) write(0,*)' xmin,max,ymin,max=',xmin,xmax,ymin,ymax
  nxe=nlon/3
  nye=nlat/3
  allocate(region_lat_e(nye,nxe),region_lon_e(nye,nxe))
  do j=1,nxe
     x(1)=xmin+(xmax-xmin)*(j-one)/(nxe-one)
     do i=1,nye
        y(1)=ymin+(ymax-ymin)*(i-one)/(nye-one)
        call rpolar2ll(x,y,1,region_lat1,region_lon1, &
                       region_lat(nlat/2,nlon/2),region_lon(nlat/2,nlon/2),rotate3)
        region_lat_e(i,j)=region_lat1(1)
        region_lon_e(i,j)=region_lon1(1)
     end do
  end do
  allocate(out1(nxe,nye))
  do i=1,nye
     do j=1,nxe
        out1(j,i)=region_lat_e(i,j)*rad2deg
     end do
  end do
  call outgrads1(out1,nxe,nye,'region_lat_e')
  do i=1,nye
     do j=1,nxe
        out1(j,i)=region_lon_e(i,j)*rad2deg
     end do
  end do
  call outgrads1(out1,nxe,nye,'region_lon_e')
                       if(mype==0) write(0,*)' min,max(region_lat_e)=',minval(region_lat_e),maxval(region_lat_e)

!  initialize merge_grid_e_to_grid_a

  nord_e2a=4
        if(mype==0) write(0,*)' at 2 in test3'
        if(mype==0) write(0,*)' min,max(region_lat_e)=',minval(region_lat_e),maxval(region_lat_e)
        if(mype==0) write(0,*)' min,max(region_lon_e)=',minval(region_lon_e),maxval(region_lon_e)
        if(mype==0) write(0,*)' min,max(region_lat)=',minval(region_lat),maxval(region_lat)
        if(mype==0) write(0,*)' min,max(region_lon)=',minval(region_lon),maxval(region_lon)
        if(mype==0) write(0,*)' nye,nxe,nlat,nlon,nord_e2a,nord_blend,nmix=',&
                                nye,nxe,nlat,nlon,nord_e2a,nord_blend,nmix
  call merge_grid_e_to_grid_a_initialize(region_lat_e,region_lon_e,region_lat,region_lon, &
                  nye,nxe,nlat,nlon,nord_e2a,nord_blend,nmix,gt_e,gt_a,p_e2a)

!  2.  interpolate earth lat field and lon field from subgrid to analysis grid.  make plots of
!             result.
  allocate(testlata(nlat,nlon),testlona(nlat,nlon),test_stream_e(nye,nxe),test_stream_a(nlat,nlon))
  allocate(exact_stream_a(nlat,nlon))
  do j=1,nxe
     do i=1,nye
        test_stream_e(i,j)=cos(region_lat_e(i,j))*sin(region_lat_e(i,j))*sin(region_lon_e(i,j))
     end do
  end do
  do j=1,nlon
     do i=1,nlat
        exact_stream_a(i,j)=cos(region_lat(i,j))*sin(region_lat(i,j))*sin(region_lon(i,j))
     end do
  end do
        if(mype==0) write(0,*)' min,max(test_stream_e)=',minval(test_stream_e),maxval(test_stream_e)
        if(mype==0) write(0,*)' min,max(exact_stream_a)=',minval(exact_stream_a),maxval(exact_stream_a)
        if(mype==0) write(0,*)' at 3 in test3'
  call merge_grid_e_to_grid_a(test_stream_e,exact_stream_a,test_stream_a,gt_e,gt_a,p_e2a)
        if(mype==0) write(0,*)' at 4 in test3'
        if(mype==0) write(0,*)' min,max(test_stream_a)=',minval(test_stream_a),maxval(test_stream_a)
  call merge_grid_e_to_grid_a(region_lat_e,region_lat,testlata,gt_e,gt_a,p_e2a)
  call merge_grid_e_to_grid_a(region_lon_e,region_lon,testlona,gt_e,gt_a,p_e2a)

  deallocate(out1)
  allocate(out1(nlon,nlat),out2(nlon,nlat))
  do i=1,nlat
     do j=1,nlon
        out1(j,i)=testlata(i,j)*rad2deg
     end do
  end do
  call outgrads1(out1,nlon,nlat,'testlata')
  do i=1,nlat
     do j=1,nlon
        out1(j,i)=testlona(i,j)*rad2deg
     end do
  end do
  call outgrads1(out1,nlon,nlat,'testlona')
  do i=1,nlat
     do j=1,nlon
        out1(j,i)=test_stream_a(i,j)
     end do
  end do
  call outgrads1(out1,nlon,nlat,'test_stream_a')
  do i=1,nlat
     do j=1,nlon
        out1(j,i)=exact_stream_a(i,j)
     end do
  end do
  call outgrads1(out1,nlon,nlat,'exact_stream_a')
  ii=0
  do j=1,nlon
     do i=1,nlat
        ii=ii+1
        out1(j,i)=p_e2a%blend(ii)
     end do
  end do
  call outgrads1(out1,nlon,nlat,'blend')
                       if(mype==0) write(0,*)' at 8 in test3'
  errmax=zero
  fmax=zero
  do j=1,nlon
     do i=1,nlat
        errmax=max(abs(testlata(i,j)-region_lat(i,j)),errmax)
        fmax=max(abs(region_lat(i,j)),fmax)
     end do
  end do
  if(mype==0) write(0,*)' for interpolated lat from e to a, errmax,fmax=',rad2deg*errmax,rad2deg*fmax
  errmax=zero
  fmax=zero
  do j=1,nlon
     do i=1,nlat
        errmax=max(abs(testlona(i,j)-region_lon(i,j)),errmax)
        fmax=max(abs(region_lon(i,j)),fmax)
     end do
  end do
  if(mype==0) write(0,*)' for interpolated lon from e to a, errmax,fmax=',rad2deg*errmax,rad2deg*fmax
!<><><><><><

!  3.  test with wind from large scale analytic stream function.
!
!       psi = cos(lat)*sin(lat)*sin(lon)
!
!       u = -dpsi/dlat = -sin(lon)*cos(2*lat)
!
!       v = (dpsi/dlon)/(1/cos(lat)) = sin(lat)*cos(lon)
!
  allocate(ue(nye,nxe),ve(nye,nxe))
  allocate(uearth_e(nye,nxe),vearth_e(nye,nxe))
  allocate(ua(nlat,nlon),va(nlat,nlon))
  allocate(ua_exact(nlat,nlon),va_exact(nlat,nlon))
  allocate(uearth_a(nlat,nlon),vearth_a(nlat,nlon))
                       if(mype==0) write(0,*)' at 9 in test3'
  do j=1,nxe
     xe=j
     do i=1,nye
        ye=i
!                      if(mype==0) write(0,*)' at 9.1 in test3,i,j,nye,nxe=',i,j,nye,nxe
        vearth_test=sin(region_lat_e(i,j))*cos(region_lon_e(i,j))
!               if(mype==0) write(0,*)' at 9.2 in test3,i,j,nye,nxe,vearth_test=',i,j,nye,nxe,vearth_test
        uearth_test=-sin(region_lon_e(i,j))*cos(two*region_lat_e(i,j))
!               if(mype==0) write(0,*)' at 9.3 in test3,i,j,nye,nxe,vearth_test=',i,j,nye,nxe,uearth_test
!           reorient from earth coordinates to e-grid coordinates
        call general_rotate_wind_ll2xy(gt_e,uearth_test,vearth_test,ue(i,j),ve(i,j), &
                                       region_lon_e(i,j),xe,ye)
!               if(mype==0) write(0,*)' at 9.4 in test3,i,j,nye,nxe,ue,ve,region_lon_e,xe,ye=',&
!                                         i,j,nye,nxe,ue(i,j),ve(i,j),region_lon_e(i,j)*rad2deg,xe,ye
     end do
  end do
                       if(mype==0) write(0,*)' at 10 in test3'
   do j=1,nlon
      xa=j
      do i=1,nlat
         ya=i
         vearth_test=sin(region_lat(i,j))*cos(region_lon(i,j))
         uearth_test=-sin(region_lon(i,j))*cos(two*region_lat(i,j))
!           reorient from earth coordinates to a-grid coordinates
         call general_rotate_wind_ll2xy(gt_a,uearth_test,vearth_test,ua_exact(i,j),va_exact(i,j), &
                                        region_lon(i,j),xa,ya)
      end do
   end do
                       if(mype==0) write(0,*)' at 11 in test3'
  call merge_vgrid_e_to_vgrid_a(ue,ve,ua_exact,va_exact,ua,va,gt_e,gt_a,p_e2a)
                       if(mype==0) write(0,*)' at 12 in test3'
   do i=1,nlat
      do j=1,nlon
         out1(j,i)=ua(i,j)
      end do
   end do
                       if(mype==0) write(0,*)' at 13 in test3'
   do i=1,nlat
      do j=1,nlon
         out2(j,i)=va(i,j)
      end do
   end do
                       if(mype==0) write(0,*)' at 14 in test3'
   call outgrads1(out1,nlon,nlat,'u1')
   call outgrads1(out2,nlon,nlat,'v1')
                       if(mype==0) write(0,*)' at 15 in test3'
   do i=1,nlat
      do j=1,nlon
         out1(j,i)=ua_exact(i,j)
      end do
   end do
   do i=1,nlat
      do j=1,nlon
         out2(j,i)=va_exact(i,j)
      end do
   end do
                       if(mype==0) write(0,*)' at 16 in test3'
   call outgrads1(out1,nlon,nlat,'u1exact')
   call outgrads1(out2,nlon,nlat,'v1exact')
                       if(mype==0) write(0,*)' at 17 in test3'
   ii=0
   do j=1,nlon
      do i=1,nlat
         ii=ii+1
         out1(j,i)=p_e2a%xa_e(ii)
      end do
   end do
   ii=0
   do j=1,nlon
      do i=1,nlat
         ii=ii+1
         out2(j,i)=p_e2a%ya_e(ii)
      end do
   end do
                       if(mype==0) write(0,*)' at 18 in test3'
   call outgrads1(out1,nlon,nlat,'xa_e')
   call outgrads1(out2,nlon,nlat,'ya_e')
                       if(mype==0) write(0,*)' at 19 in test3'

  deallocate(region_lat_e,region_lon_e)
  deallocate(out1,out2)
  deallocate(testlata,testlona,test_stream_e,test_stream_a,exact_stream_a)
  deallocate(ue,ve)
  deallocate(uearth_e,vearth_e,ua,va,ua_exact,va_exact,uearth_a,vearth_a)

end subroutine test3_egrid2points
