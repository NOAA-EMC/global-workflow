module mod_nmmb_to_a
!$$$ module documentation block
!           .      .    .                                       .
! module:   mod_nmmb_to_a
!   prgmmr: parrish
!
! abstract:  This module contains routines to interpolate from the nmmb b grid to analysis a grid
!             which exactly covers either the H or V component of the nmmb grid, but optionally
!             at a coarser resolution.
!            The resolution of the a grid is controlled by the variable grid_ratio_nmmb, which is 
!             an input variable to subroutine init_nmmb_to_a in this module.
!            Because the B grid is actually two A grids staggered with respect to each other, one
!             for mass variables, the other for wind variables, there is an input character variable
!             nmmb_reference_grid, which if ="H", then uses H grid as reference for the analysis grid
!             and similarly for the V grid.
!
! program history log:
!   2009-08-06  lueken - added module doc block
!   2010-09-10  parrish, add more extensive description, and add routine nmmb_h_to_a8
!   2012-02-08  parrish - add public variables ratio_x, ratio_y, for use in gridmod.F90 for bug fix
!                            in definition of region_dx, region_dy.
!
! subroutines included:
!   sub init_nmmb_to_a
!   sub nmmb_h_to_a
!   sub nmmb_h_to_a8
!   sub nmmb_v_to_a
!   sub nmmb_a_to_h
!   sub nmmb_a_to_v
!   sub b_to_a_interpolate
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

  private
  public init_nmmb_to_a,nmmb_h_to_a,nmmb_h_to_a8,nmmb_v_to_a,nmmb_a_to_h,nmmb_a_to_v
  public nxa,nya,ratio_x,ratio_y

  integer(i_kind) nxa,nya,nxb,nyb
  real(r_kind),allocatable,dimension(:)::xbh_a,xbh_b,xbv_a,xbv_b,xa_a,xa_b
  real(r_kind),allocatable,dimension(:)::ybh_a,ybh_b,ybv_a,ybv_b,ya_a,ya_b
  real(r_kind) ratio_x,ratio_y

contains

subroutine init_nmmb_to_a(nmmb_reference_grid,grid_ratio_nmmb,nxb_in,nyb_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_nmmb_to_a
!   prgmmr: parrish
!
! abstract:  define analysis grid and set up interpolation constants required to
!             interpolate back and forth between nmmb grid and analysis grid.
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish, add more extensive description
!
!   input argument list:
!    nmmb_reference_grid - character variable, ="H" for H grid as reference, ="V" for V grid as reference
!    grid_ratio_nmmb     - analysis grid increment in nmmb grid units
!    nxb_in,nyb_in       - x and y dimensions of nmmb grid
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

!   initialize constants required to interpolate back and forth between nmmb grid and analysis grid

  use constants, only: half,one,two
  implicit none

  character(1)   , intent(in   ) :: nmmb_reference_grid   ! ='H': use nmmb H grid as reference for analysis grid
                                                          ! ='V': use nmmb V grid as reference for analysis grid
  real(r_kind)   , intent(in   ) :: grid_ratio_nmmb       ! analysis grid increment in nmmb grid units
  integer(i_kind), intent(in   ) :: nxb_in,nyb_in         ! x and y dimensions of nmmb grid.

  integer(i_kind) i,j

  nxb=nxb_in
  nyb=nyb_in

!--------------------------obtain analysis grid dimensions nxa,nxb

  if(nmmb_reference_grid=='H') then
     nxa=1+nint((nxb-one)/grid_ratio_nmmb)
     nya=1+nint((nyb-one)/grid_ratio_nmmb)
  else if(nmmb_reference_grid=='V') then
     nxa=1+nint((nxb-two)/grid_ratio_nmmb)
     nya=1+nint((nyb-two)/grid_ratio_nmmb)
  end if

!--------------------compute all combinations of relative coordinates

  allocate(xbh_a(nxb),xbh_b(nxb),xbv_a(nxb),xbv_b(nxb),xa_a(nxa),xa_b(nxa))
  allocate(ybh_a(nyb),ybh_b(nyb),ybv_a(nyb),ybv_b(nyb),ya_a(nya),ya_b(nya))

  do j=1,nxb
     xbh_b(j)=j
     xbv_b(j)=j+half
  end do
  do j=1,nxa
     xa_a(j)=j
  end do
  do i=1,nyb
     ybh_b(i)=i
     ybv_b(i)=i+half
  end do
  do i=1,nya
     ya_a(i)=i
  end do
  if(nmmb_reference_grid=='H') then
     ratio_x=(nxb-one)/(nxa-one)
     do j=1,nxa
        xa_b(j)=one+(j-one)*ratio_x
     end do
     do j=1,nxb
        xbh_a(j)=one+(j-one)/ratio_x
        xbv_a(j)=one+(j-half)/ratio_x
     end do
     ratio_y=(nyb-one)/(nya-one)
     do i=1,nya
        ya_b(i)=one+(i-one)*ratio_y
     end do
     do i=1,nyb
        ybh_a(i)=one+(i-one)/ratio_y
        ybv_a(i)=one+(i-half)/ratio_y
     end do
  else if(nmmb_reference_grid=='V') then
     ratio_x=(nxb-two)/(nxa-one)
     do j=1,nxa
        xa_b(j)=one+half+(j-one)*ratio_x
     end do
     do j=1,nxb
        xbh_a(j)=one+(j-one-half)/ratio_x
        xbv_a(j)=one+(j-one)/ratio_x
     end do
     ratio_y=(nyb-two)/(nya-one)
     do i=1,nya
        ya_b(i)=one+half+(i-one)*ratio_y
     end do
     do i=1,nyb
        ybh_a(i)=one+(i-one-half)/ratio_y
        ybv_a(i)=one+(i-one)/ratio_y
     end do
  end if

end subroutine init_nmmb_to_a

subroutine nmmb_h_to_a(hb,ha)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nmmb_h_to_a
!   prgmmr: parrish
!
! abstract: interpolate from nmmb H grid to analysis grid
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish, add documentation
!
!   input argument list:
!    hb - input nmmb H grid variable
!
!   output argument list:
!    ha - output interpolated variable on analysis grid
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single
  implicit none

  real(r_single),intent(in   ) :: hb(nxb,nyb)
  real(r_kind)  ,intent(  out) :: ha(nya,nxa)

  integer(i_kind) i,j
  real(r_kind) bh(nyb,nxb)

  do j=1,nxb
     do i=1,nyb
        bh(i,j)=hb(j,i)
     end do
  end do
  call b_to_a_interpolate(bh,ha,nxb,nyb,nxa,nya,xbh_b,ybh_b,xa_b,ya_b)

end subroutine nmmb_h_to_a

subroutine nmmb_h_to_a8(hb,ha)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nmmb_h_to_a
!   prgmmr:
!
! abstract:  copy of nmmb_h_to_a for input variable hb real(r_kind)
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish, add documentation
!
!   input argument list:
!    hb - input nmmb H grid variable
!
!   output argument list:
!    ha - output interpolated variable on analysis grid
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  real(r_kind),intent(in   ) :: hb(nxb,nyb)
  real(r_kind)  ,intent(  out) :: ha(nya,nxa)

  integer(i_kind) i,j
  real(r_kind) bh(nyb,nxb)

  do j=1,nxb
     do i=1,nyb
        bh(i,j)=hb(j,i)
     end do
  end do
  call b_to_a_interpolate(bh,ha,nxb,nyb,nxa,nya,xbh_b,ybh_b,xa_b,ya_b)

end subroutine nmmb_h_to_a8

subroutine nmmb_v_to_a(vb,va)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nmmb_v_to_a
!   prgmmr: parrish
!
! abstract: interpolate from nmmb V grid to analysis grid
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish, add documentation
!
!   input argument list:
!    vb - variable on nmmb V grid
!
!   output argument list:
!    va - interolated variable on analysis grid
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single
  implicit none

  real(r_single),intent(in   ) :: vb(nxb,nyb)
  real(r_kind)  ,intent(  out) :: va(nya,nxa)

  integer(i_kind) i,ii,j,jj
  real(r_kind) bv(nyb,nxb)

!                  input variable on v grid is zero on north and east boundaries,
!                  so replace with values of adjacent interior points.
  do j=1,nxb
     jj=min(j,nxb-1)
     do i=1,nyb
        ii=min(i,nyb-1)
        bv(i,j)=vb(jj,ii)
     end do
  end do
  call b_to_a_interpolate(bv,va,nxb,nyb,nxa,nya,xbv_b,ybv_b,xa_b,ya_b)

end subroutine nmmb_v_to_a

subroutine nmmb_a_to_h(ha,hb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nmmb_a_to_h
!   prgmmr: parrish
!
! abstract: interpolate from analysis grid to nmmb H grid
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish - add documentation
!
!   input argument list:
!    ha - variable on analysis grid
!
!   output argument list:
!    hb - interpolated variable on nmmb H grid
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single
  implicit none

  real(r_kind)  ,intent(in   ) :: ha(nya,nxa)
  real(r_single),intent(  out) :: hb(nxb,nyb)

  integer(i_kind) i,j
  real(r_kind) bh(nyb,nxb)

  call b_to_a_interpolate(ha,bh,nxa,nya,nxb,nyb,xa_a,ya_a,xbh_a,ybh_a)
  do j=1,nxb
     do i=1,nyb
        hb(j,i)=bh(i,j)
     end do
  end do

end subroutine nmmb_a_to_h

subroutine nmmb_a_to_v(va,vb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    nmmb_a_to_v
!   prgmmr: parrish
!
! abstract: interpolate from analysis grid to nmmb V grid
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish - add documentation
!
!   input argument list:
!    va - analysis variable
!
!   output argument list:
!    vb - interpolated nmmb V grid variable
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single
  use constants, only: zero
  implicit none

  real(r_kind)  ,intent(in   ) :: va(nya,nxa)
  real(r_single),intent(  out) :: vb(nxb,nyb)

  integer(i_kind) i,j
  real(r_kind) bv(nyb,nxb)

  call b_to_a_interpolate(va,bv,nxa,nya,nxb,nyb,xa_a,ya_a,xbv_a,ybv_a)
  do j=1,nxb
     do i=1,nyb
        vb(j,i)=bv(i,j)
     end do
  end do

!             set north and east boundaries of output array to zero for variable on v grid

  do i=1,nyb
     vb(nxb,i)=zero
  end do
  do j=1,nxb-1
     vb(j,nyb)=zero
  end do

end subroutine nmmb_a_to_v

subroutine b_to_a_interpolate(b,a,mb,nb,ma,na,xb,yb,xa,ya)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    b_to_a_interpolate
!   prgmmr: parrish
!
! abstract: interpolate from variable b to variable a.  This routine is
!    used for interpolating both ways, nmmb H/V grid to analysis and back.
!    Direction is controlled by input arguments.  Interpolation is bilinear
!    both ways.
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2013-01-23  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!
!   input argument list:
!    mb,nb - b dimensions
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

!  interpolate from b-grid to a-grid

!   NOTE:  xa is in xb units, ya is in yb units

  use constants, only: zero,one
  implicit none

  integer(i_kind),intent(in   ) :: mb,nb,ma,na
  real(r_kind)   ,intent(in   ) :: b(nb,mb),xb(mb),yb(nb),xa(ma),ya(na)
  real(r_kind)   ,intent(  out) :: a(na,ma)

  integer(i_kind) i,j
  real(r_kind) gxa,gya
  real(r_kind) dx(ma),dx1(ma),dy(na),dy1(na)
  integer(i_kind) jxa(ma),jxap(ma),iya(na),iyap(na)

  do j=1,ma
     gxa=xa(j)
     call grdcrd1(gxa,xb,mb,1)
     jxa(j)=int(gxa)
     jxa(j)=min(max(1,jxa(j)),mb)
     dx(j)=max(zero,min(one,gxa-jxa(j)))
     dx1(j)=one-dx(j)
     jxap(j)=min(mb,jxa(j)+1)
  end do
  do i=1,na
     gya=ya(i)
     call grdcrd1(gya,yb,nb,1)
     iya(i)=int(gya)
     iya(i)=min(max(1,iya(i)),nb)
     dy(i)=max(zero,min(one,gya-iya(i)))
     dy1(i)=one-dy(i)
     iyap(i)=min(nb,iya(i)+1)
  end do

  do j=1,ma
     do i=1,na
        a(i,j)=dx1(j)*(dy1(i)*b(iya(i),jxa (j))+dy(i)*b(iyap(i),jxa (j))) &
              +dx (j)*(dy1(i)*b(iya(i),jxap(j))+dy(i)*b(iyap(i),jxap(j)))
     end do
  end do

end subroutine b_to_a_interpolate

end module mod_nmmb_to_a
