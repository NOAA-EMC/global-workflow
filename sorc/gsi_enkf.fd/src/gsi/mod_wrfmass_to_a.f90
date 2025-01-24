module mod_wrfmass_to_a
!$$$ module documentation block
!           .      .    .                                       .
! module:   mod_wrfmass_to_a
!   prgmmr: parrish
!
! abstract:  This module contains routines to interpolate from the wrf mass c grid to analysis a grid
!             which exactly covers either the H or V component of the wrf mass grid, but optionally
!             at a coarser resolution.
!            The resolution of the a grid is controlled by the variable grid_ratio_wrfmass, which is 
!             an input variable to subroutine init_wrfmass_to_a in this module.
!
! program history log:
!   2013-03-25  Hu     - start from mod_wrfmass_to_a module
!
! subroutines included:
!   sub init_wrfmass_to_a
!   sub wrfmass_h_to_a
!   sub wrfmass_h_to_a8
!   sub wrfmass_a_to_h
!   sub wrfmass_h_to_a4
!   sub wrfmass_obs_to_a8
!   sub wrfmass_a_to_h4
!   sub b_to_a_interpolate
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use mpimod, only: mype
  implicit none

  private
  public init_wrfmass_to_a,wrfmass_h_to_a,wrfmass_h_to_a8
  public wrfmass_h_to_a4,wrfmass_a_to_h4
  public wrfmass_a_to_h,wrfmass_obs_to_a8,wrfmass_map_a_to_h4
  public nxa_wrfmass,nya_wrfmass,ratio_x_wrfmass,ratio_y_wrfmass

  integer(i_kind) nxa_wrfmass,nya_wrfmass
  integer(i_kind) nxa,nya,nxb,nyb
  real(r_kind),allocatable,dimension(:)::xbh_a,xbh_b,xbv_a,xbv_b,xa_a,xa_b
  real(r_kind),allocatable,dimension(:)::ybh_a,ybh_b,ybv_a,ybv_b,ya_a,ya_b
  real(r_kind) ratio_x_wrfmass,ratio_y_wrfmass

contains

subroutine init_wrfmass_to_a(grid_ratio_wrfmass,nxb_in,nyb_in)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_wrfmass_to_a
!   prgmmr: parrish
!
! abstract:  define analysis grid and set up interpolation constants required to
!             interpolate back and forth between wrfmass grid and analysis grid.
!
! program history log:
!   2013-03-25  Hu     - start from init_nmmb_to_a
!
!   input argument list:
!    grid_ratio_wrfmass     - analysis grid increment in wrfmass grid units
!    nxb_in,nyb_in       - x and y dimensions of wrfmass grid
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

!   initialize constants required to interpolate back and forth between wrfmass grid and analysis grid

  use constants, only: half,one,two
  implicit none

  real(r_kind)   , intent(in   ) :: grid_ratio_wrfmass       ! analysis grid increment in wrfmass grid units
  integer(i_kind), intent(in   ) :: nxb_in,nyb_in         ! x and y dimensions of wrfmass grid.

  integer(i_kind) i,j

  nxb=nxb_in
  nyb=nyb_in

!--------------------------obtain analysis grid dimensions nxa,nxb

  nxa=1+nint((nxb-one)/grid_ratio_wrfmass)
  nya=1+nint((nyb-one)/grid_ratio_wrfmass)
  nxa_wrfmass=nxa
  nya_wrfmass=nya

!--------------------compute all combinations of relative coordinates

  allocate(xbh_a(nxb),xbh_b(nxb),xbv_a(nxb),xbv_b(nxb),xa_a(nxa),xa_b(nxa))
  allocate(ybh_a(nyb),ybh_b(nyb),ybv_a(nyb),ybv_b(nyb),ya_a(nya),ya_b(nya))

  do j=1,nxb
     xbh_b(j)=j
  end do
  do j=1,nxa
     xa_a(j)=j
  end do
  do i=1,nyb
     ybh_b(i)=i
  end do
  do i=1,nya
     ya_a(i)=i
  end do

  ratio_x_wrfmass=(nxb-one)/(nxa-one)
  do j=1,nxa
        xa_b(j)=one+(j-one)*ratio_x_wrfmass
  end do
  do j=1,nxb
        xbh_a(j)=one+(j-one)/ratio_x_wrfmass
  end do
  ratio_y_wrfmass=(nyb-one)/(nya-one)
  do i=1,nya
        ya_b(i)=one+(i-one)*ratio_y_wrfmass
  end do
  do i=1,nyb
        ybh_a(i)=one+(i-one)/ratio_y_wrfmass
  end do

end subroutine init_wrfmass_to_a

subroutine wrfmass_h_to_a(hb,ha)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfmass_h_to_a
!   prgmmr: parrish
!
! abstract: interpolate from wrf mass H grid to analysis grid
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish, add documentation
!
!   input argument list:
!    hb - input wrfmass H grid variable
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

end subroutine wrfmass_h_to_a

subroutine wrfmass_h_to_a4(hb,ha)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfmass_h_to_a4
!   prgmmr: parrish
!
! abstract: interpolate from wrf mass H grid to analysis grid
!
! program history log:
!   2013-03-26  Hu     , start based on nmmb_h_to_a
!
!   input argument list:
!    hb - input wrfmass H grid variable
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
  real(r_single),intent(  out) :: ha(nxa,nya)

  integer(i_kind) i,j
  real(r_kind)  :: bh(nyb,nxb)
  real(r_kind)  :: ha8(nya,nxa)

  do j=1,nxb
     do i=1,nyb
        bh(i,j)=hb(j,i)
     end do
  end do
  call b_to_a_interpolate(bh,ha8,nxb,nyb,nxa,nya,xbh_b,ybh_b,xa_b,ya_b)
  do j=1,nxa
     do i=1,nya
        ha(j,i)=ha8(i,j)
     end do
  end do

end subroutine wrfmass_h_to_a4

subroutine wrfmass_h_to_a8(hb,ha)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfmass_h_to_a
!   prgmmr:
!
! abstract:  copy of wrfmass_h_to_a for input variable hb real(r_kind)
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish, add documentation
!
!   input argument list:
!    hb - input wrfmass H grid variable
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

end subroutine wrfmass_h_to_a8

subroutine wrfmass_obs_to_a8(obsba,nreal,maxobs,ilat,ilon,numobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfmass_obs_to_a
!   prgmmr:
!
! abstract:  get obs to analysis grid
!
! program history log:
!   2013-03-27  Hu     , start
!
!   input argument list:
!    obsb - input observation in wrf mass H grid
!    nreal,maxobs - elements and number of obs in wrf mass H grid
!    ilat,ilon - j and i in wrf mass H grid
!
!   output argument list:
!    obsa - output mapped variable on analysis grid
!    numobs - obs number on analysis grid
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  real(r_kind),intent(inout)    :: obsba(nreal,maxobs)
  integer(i_kind),intent(in   ) :: nreal,maxobs,ilat,ilon
  integer(i_kind),intent(out)  :: numobs

  integer(i_kind) i,j,n,k
  real(r_kind) :: ria(maxobs),rja(maxobs)
  integer(i_kind) :: ija(nxa,nya)
  real(r_kind)    :: dist(nxa,nya)
  real(r_kind)    :: obsa(nreal,maxobs)
  real(r_kind)    :: adist

  obsa=obsba
  numobs=maxobs

  do n=1,maxobs
     i=int(obsba(ilon,n))
     j=int(obsba(ilat,n))

     ria(n)=xbh_a(i)
     rja(n)=ybh_a(j)
  end do

  dist=99999.9_r_kind
  ija=0
  do n=1,maxobs
     i=int(ria(n)+0.5_r_kind)
     j=int(rja(n)+0.5_r_kind)

     adist=(ria(n)-real(i,r_kind))**2+(rja(n)-real(j,r_kind))**2
     if(adist < dist(i,j)) then
        dist(i,j)=adist
        ija(i,j)=n
     endif
  enddo

  n=0
  do j=1,nya
     do i=1,nxa
        if(ija(i,j) > 0) then
           n=n+1
           obsba(ilon,n)=real(i,r_kind)
           obsba(ilat,n)=real(j,r_kind)
           do k=3,nreal
              obsba(k,n)=obsa(k,ija(i,j))
           enddo
        endif
     enddo
  enddo
  numobs=n
  write(*,*) 'wrfmass_obs_to_a8: map obs from ',maxobs,' to ',numobs

end subroutine wrfmass_obs_to_a8

subroutine wrfmass_a_to_h(ha,hb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfmass_a_to_h
!   prgmmr: parrish
!
! abstract: interpolate from analysis grid to wrfmass H grid
!
! program history log:
!   2009-08-06  lueken - added subprogram doc block
!   2010-09-10  parrish - add documentation
!
!   input argument list:
!    ha - variable on analysis grid
!
!   output argument list:
!    hb - interpolated variable on wrfmass H grid
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

end subroutine wrfmass_a_to_h

subroutine wrfmass_a_to_h4(ha,hb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfmass_a_to_h4
!   prgmmr: parrish
!
! abstract: interpolate from analysis grid to wrfmass H grid
!
! program history log:
!   2013-03-26  hu      - start based on nmmb_a_to_h
!
!   input argument list:
!    ha - variable on analysis grid
!
!   output argument list:
!    hb - interpolated variable on wrfmass H grid
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single
  implicit none

  real(r_single),intent(in   ) :: ha(nxa,nya)
  real(r_single),intent(  out) :: hb(nxb,nyb)

  integer(i_kind) i,j
  real(r_kind) :: bh(nyb,nxb)
  real(r_kind) :: ha8(nya,nxa)

  do j=1,nxa
     do i=1,nya
        ha8(i,j)=ha(j,i)
     end do
  end do
  call b_to_a_interpolate(ha8,bh,nxa,nya,nxb,nyb,xa_a,ya_a,xbh_a,ybh_a)
  do j=1,nxb
     do i=1,nyb
        hb(j,i)=bh(i,j)
     end do
  end do

end subroutine wrfmass_a_to_h4

subroutine wrfmass_map_a_to_h4(ha,hb)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    wrfmass_a_to_h4
!   prgmmr: parrish
!
! abstract: interpolate from analysis grid to wrfmass H grid
!
! program history log:
!   2013-03-26  hu      - start based on nmmb_a_to_h
!
!   input argument list:
!    ha - variable on analysis grid
!
!   output argument list:
!    hb - interpolated variable on wrfmass H grid
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: r_single
  implicit none

  real(r_single),intent(in   ) :: ha(nxa,nya)
  real(r_single),intent(  out) :: hb(nxb,nyb)

  integer(i_kind) i,j
  real(r_kind) :: bh(nyb,nxb)
  real(r_kind) :: ha8(nya,nxa)

  do j=1,nxa
     do i=1,nya
        ha8(i,j)=ha(j,i)
     end do
  end do
  call b_to_a_map(ha8,bh,nxa,nya,nxb,nyb,xa_a,ya_a,xbh_a,ybh_a)
  do j=1,nxb
     do i=1,nyb
        hb(j,i)=bh(i,j)
     end do
  end do

end subroutine wrfmass_map_a_to_h4

subroutine b_to_a_interpolate(b,a,mb,nb,ma,na,xb,yb,xa,ya)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    b_to_a_interpolate
!   prgmmr: parrish
!
! abstract: interpolate from variable b to variable a.  This routine is
!    used for interpolating both ways, wrfmass H/V grid to analysis and back.
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

subroutine b_to_a_map(b,a,mb,nb,ma,na,xb,yb,xa,ya)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    b_to_a_interpolate
!   prgmmr: parrish
!
! abstract: map from variable b to variable a.  This routine is
!    used for map both ways, wrfmass H/V grid to analysis and back.
!    Direction is controlled by input arguments.
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
        if(dx1(j) > dx (j)) then
           if(dy1(i) > dy(i)) then
              a(i,j)=b(iya(i),jxa (j))
           else
              a(i,j)=b(iyap(i),jxa (j))
           endif
        else
           if(dy1(i) > dy(i)) then
              a(i,j)=b(iya(i),jxap(j))
           else
              a(i,j)=b(iyap(i),jxap(j))
           endif
        endif
     end do
  end do

end subroutine b_to_a_map

end module mod_wrfmass_to_a
