module smooth_polcarf
!$$$   module documentation block
!                .      .    .                                       .
! module:    smooth_polcarf  smooth polar cascade interpolation stuff
!   prgmmr: parrish          org: np23                date: 2005-04-28
!
! abstract: contains everything needed to implement polar smoothing
!             spline interpolation, as an alternative to the
!             existing polcas polar cascade routines which have
!             noise problems near the pole due to magnification
!             of normally small errors in the longitude direction
!             very close to the pole.
!             B-splines are used in smoothing form, not as a direct
!             fit.
!
! program history log:
!   2005-04-28  parrish
!   2010-10-08  derber - optimize and clean up
!
! subroutines included:
!   sub init_smooth_polcas
!   sub destroy_smooth_polcas
!   sub setup_smooth_polcas
!   sub smooth_polcas   - interpolate from x-y to lat-lon using smooth cascade interpolation.
!   sub smooth_polcasa
!   sub smooth_caspol
!   sub smooth_polcasv
!   sub bspline
!
! Variable Definitions:
!   def norsp      - order of smooth cascade interpolation
!                      if =0, then old cascade interpolation is used
!   def nlon_m89   - 1st longitude index after leaving east side of exclusion zone around y < 0
!   def nlon_p89   - last longitude index before entering west side of exclusion zone around y > 0
!   def nlon_p91   - 1st longitude index after leaving east side of exclusion zone around y > 0
!   def nlon_p269  - last longitude index before entering west side of exclusion zone around y < 0
!   def nlon_p1    - 1st longitude index after leaving east side of exclusion zone around x > 0
!   def nlon_p179  - last longitude index before entering west side of exclusion zone around x < 0
!   def nlon_m179  - 1st longitude index after leaving east side of exclusion zone around x < 0
!   def nlon_m1    - last longitude index before entering west side of exclusion zone around x > 0
!   def nlon_m89b  - lower bound on nlon_m89
!   def nlon_p89b  - upper bound on nlon_p89
!   def nlon_p91b  - lower bound on nlon_p91
!   def nlon_p269b - upper bound on nlon_p269
!   def nlon_p1b   - lower bound on nlon_p1b
!   def nlon_p179b - upper bound on nlon_p179
!   def nlon_m179b - lower bound on nlon_m179
!   def nlon_m1b   - upper bound on nlon_m1
!
!   def wxp        - weights for interpolating from y to longitude in right half plane
!   def iwxp       - indices for wxp
!   def norwxp     - interpolation order for wxp
!   def wxm        - weights for interpolating from y to longitude in left half plane
!   def iwxm       - indices for wxm
!   def norwxm     - interpolation order for wxm
!   def wyp        - weights for interpolating from x to longitude in upper half plane
!   def iwyp       - indices for wyp
!   def norwyp     - interpolation order for wyp
!   def wym        - weights for interpolating from x to longitude in lower half plane
!   def iwym       - indices for wym
!   def norwym     - interpolation order for wym
!   def wx2lat     - weights for interpolating to latitude along longitude lines for y axis excluded.
!   def iwx2lat    - indices for wx2lat
!   def nwx2lat    - interpolation order for wx2lat
!   def wy2lat     - weights for interpolating to latitude along longitude lines for x axis excluded.
!   def iwy2lat    - indices for wy2lat
!   def nwy2lat    - interpolation order for wy2lat
!   def blendxp    - blend weights for right half plane
!   def blendxm    - blend weights for left half plane
!   def blendyp    - blend weights for upper half plane
!   def blendym    - blend weights for lower half plane
!   def nlon_p90   - =0 unless nlon is divisible by 4, in which case nlon_p90=nlon/4
!   def nlon_p270  - =0 unless nlon is divisible by 4, in which case nlon_p270=3*nlon/4
!
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_smooth_polcas
  public :: destroy_smooth_polcas
  public :: setup_smooth_polcas
  public :: smooth_polcas
  public :: smooth_polcasa
  public :: smooth_caspol
  public :: smooth_polcasv
  public :: bspline
! set passed variables to public
  public :: norsp

  integer(i_kind) norsp


  real(r_kind),allocatable,dimension(:,:,:):: xwtxys,ywtxys
  integer(i_kind),allocatable,dimension(:,:,:):: ixwtxys,iywtxys
  integer(i_kind),allocatable,dimension(:,:):: nxwtxys,nywtxys

contains

  subroutine init_smooth_polcas
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_smooth_polcas  
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-11  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    implicit none

    norsp=0

  end subroutine init_smooth_polcas

  subroutine destroy_smooth_polcas
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_smooth_polcas
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-11  lueken - added subprogram doc block
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
    implicit none

    deallocate(xwtxys,ywtxys,ixwtxys,iywtxys,nxwtxys,nywtxys)

  end subroutine destroy_smooth_polcas

  subroutine setup_smooth_polcas
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    setup_smooth_polcas  create info for smooth_polcas
!   prgmmr: parrish          org: np23                date: 2005-04-28
!
! abstract: create various information needed for using smooth_polcas.
!
! program history log:
!   2008-04-11  safford - complete subroutine documentation block, rm unused vars
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  use gridmod, only: nlon,nlat,rlats
  use constants, only: zero,half,one,two,pi
  use berror, only: nf,nr
  implicit none

  integer(i_kind) i,j,jj,nxgrid,nygrid
  real(r_kind) dlon,df,pi2
  real(r_kind) xgrid(-nf:nf),ygrid(-nf:nf),rs(0:nr)
  integer(i_kind) nor1,iwgt1(0:norsp),nor2,iwgt2(0:norsp)
  real(r_kind) wgt1(0:norsp),wgt2(0:norsp)
  integer(i_kind) ieven1,ieven2,iodd1,iodd2,nord_evenmax1,nord_oddmax1
  integer(i_kind) ii,iord1,iord2,nord_evenmax2,nord_oddmax2
  real(r_kind) tin,xin,yin
  real(r_kind) slon(nlon),clon(nlon)

!              define lat-lon grid lats in polar stereographic units (distance from pole)
  do i=1,nr
     rs(i)=cos(rlats(nlat-i))/(one+sin(rlats(nlat-i)))
  end do
  rs(0)=zero
  pi2=two*pi
!              define polar stereographic grid increment (same in x and y directions)
  df=tan(pi2/nlon*half)

  dlon=two*pi/nlon


!   compute interpolation weights:

!  first define xgrid, ygrid, and various other things

  nxgrid=2*nf+1
  nygrid=2*nf+1
  do i=-nf,nf
     xgrid(i)=df*i
     ygrid(i)=xgrid(i)
  end do

  allocate(xwtxys(0:norsp,nlon,0:nr),ywtxys(0:norsp,nlon,0:nr))
  allocate(ixwtxys(0:norsp,nlon,0:nr),iywtxys(0:norsp,nlon,0:nr))
  allocate(nxwtxys(nlon,0:nr),nywtxys(nlon,0:nr))
  do j=1,nlon
     clon(j)=cos((j-1)*dlon)
     slon(j)=sin((j-1)*dlon)
  end do
  do i=0,nr
     do j=1,nlon
!----------------------------x coordinate first
        xin=rs(i)*clon(j)
        ieven1=nint(xin/df)
        iodd1=int(xin/df)
        if(xin<zero) iodd1=iodd1-1
        nord_oddmax1=min(2*(iodd1+nf)+1,2*(nf-iodd1-1)+1)
        nord_evenmax1=2*min(ieven1+nf,nf-ieven1)
        iord1=min(norsp,nord_oddmax1,nord_evenmax1)
        if(iord1<=0) then
           nor1=0 ; wgt1(0)=one ; iwgt1(0)=min(nf,max(-nf,nint(xin/df)))
        end if
        if(iord1>0.and.mod(iord1,2)==0) then
           tin=half+(xin-xgrid(ieven1))/df
           call bspline(tin,iord1+1,wgt1)
           nor1=iord1
           do ii=-iord1/2,iord1/2
              iwgt1(ii+iord1/2)=ieven1+ii
           end do
        end if
        if(iord1>0.and.mod(iord1,2)/=0) then
           tin=(xin-xgrid(iodd1))/df
           call bspline(tin,iord1+1,wgt1)
           nor1=iord1
           do ii=-(iord1-1)/2,1+(iord1-1)/2
              iwgt1(ii+(iord1-1)/2)=iodd1+ii
           end do
        end if

!----------------------------y coordinate next
        yin=rs(i)*slon(j)
        ieven2=nint(yin/df)
        iodd2=int(yin/df)
        if(yin<zero) iodd2=iodd2-1
        nord_oddmax2=min(2*(iodd2+nf)+1,2*(nf-iodd2-1)+1)
        nord_evenmax2=2*min(ieven2+nf,nf-ieven2)
        iord2=min(norsp,nord_oddmax2,nord_evenmax2)
        if(iord2<=0) then
           nor2=0 ; wgt2(0)=one ; iwgt2(0)=min(nf,max(-nf,nint(yin/df)))
        end if
        if(iord2>0.and.mod(iord2,2)==0) then
           tin=half+(yin-ygrid(ieven2))/df
           call bspline(tin,iord2+1,wgt2)
           nor2=iord2
           do ii=-iord2/2,iord2/2
              iwgt2(ii+iord2/2)=ieven2+ii
           end do
        end if
        if(iord2>0.and.mod(iord2,2)/=0) then
           tin=(yin-ygrid(iodd2))/df
           call bspline(tin,iord2+1,wgt2)
           nor2=iord2
           do ii=-(iord2-1)/2,1+(iord2-1)/2
              iwgt2(ii+(iord2-1)/2)=iodd2+ii
           end do
        end if
!---------------------------now consolidate and get final weights, addresses for this point

        nxwtxys(j,i)=nor1
        nywtxys(j,i)=nor2
        do jj=0,nor2
           iywtxys(jj,j,i)=iwgt2(jj)
           ywtxys(jj,j,i)=wgt2(jj)
        end do
        do ii=0,nor1
           ixwtxys(ii,j,i)=iwgt1(ii)
           xwtxys(ii,j,i)=wgt1(ii)
        end do
     end do
  end do

  end subroutine setup_smooth_polcas

  subroutine smooth_polcas(fxy,hlatlon)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smooth_polcas 2d smoothing spline interpolation
!   prgmmr: parrish          org: np22                date: 2005-04-22
!
! abstract: Interpolate polar stereo field fxy(-nf:nf,-nf:nf) to
!            corresponding lat-lon field hlatlon(0:nr,nlon), where
!            fxy(0,0) is pole point, and hlatlon(0,:) is also nlon copies of
!            pole point, using smoothing splines.
!
! program history log:
!   2005-05-14  parrish
!   2008-04-11  safford - rm unused uses
!
!   input argument list:
!     fxy     - input data on cartesian grid, dimensions [-nf:nf,-nf:nf].
!
!   output argument list:
!     hlatlon - output data on polar grid, dimensions [0:nlon,0:nr]
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$ end documentation block

  use constants, only: zero
  use berror, only: nf,nr
  use gridmod, only: nlon
  implicit none

  real(r_kind),dimension(-nf:nf,-nf:nf) ,intent(in   ) :: fxy
  real(r_kind),dimension(nlon+1,0:nr),intent(  out) :: hlatlon

! Declare local arrays variables:
  integer(i_kind) i,ii,j,jj,jjj
  real(r_kind) sum,ywgt,xywgt

  do i=0,nr
     hlatlon(nlon+1,i)=zero
     do j=1,nlon
        sum=zero
        do jj=0,nywtxys(j,i)
           ywgt=ywtxys(jj,j,i)
           jjj=iywtxys(jj,j,i)
           do ii=0,nxwtxys(j,i)
              xywgt=ywgt*xwtxys(ii,j,i)
              sum=sum+xywgt*fxy(ixwtxys(ii,j,i),jjj)
           end do
        end do
        hlatlon(j,i)=sum
     end do
  end do
  end subroutine smooth_polcas

  subroutine smooth_polcasa(fxy,hlatlon)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smooth_polcasa adjoint of smooth_polcas
!   prgmmr: parrish          org: np22                date: 2005-04-22
!
! abstract: adjoint of smooth_polcas
!
! program history log:
!   2005-05-14  parrish
!   2008-04-12  safford - rm unused uses
!
!   input argument list:
!     hlatlon - input data on polar grid, dimensions [0:nlon,0:nr]
!
!   output argument list:
!     fxy     - output data on cartesian grid, dimensions [-nf:nf,-nf:nf].
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use constants, only: zero
  use berror, only: nf,nr
  use gridmod, only: nlon
  implicit none

  real(r_kind),dimension(-nf:nf,-nf:nf) ,intent(  out) :: fxy
  real(r_kind),dimension(nlon+1,0:nr),intent(in   ) :: hlatlon

! Declare local arrays variables:
  integer(i_kind) i,ii,j,jj,jjj
  real(r_kind) sum,ywgt,xywgt


  fxy=zero
  do i=0,nr
     do j=1,nlon
        sum=hlatlon(j,i)
        do jj=0,nywtxys(j,i)
           ywgt=ywtxys(jj,j,i)
           jjj=iywtxys(jj,j,i)
           do ii=0,nxwtxys(j,i)
              xywgt=ywgt*xwtxys(ii,j,i)
              fxy(ixwtxys(ii,j,i),jjj)=fxy(ixwtxys(ii,j,i),jjj)+sum*xywgt
           end do
        end do
     end do
  end do
  end subroutine smooth_polcasa


subroutine smooth_caspol(fxy,hlatlon)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smooth_caspol
!   prgmmr: sato             org: np23                date: 2008-11-03
!
! abstract:  reverse conversion of smooth_polcas()
!
! program history log:
!   2008-11-03  sato
!
!   input argument list:
!     hlatlon - output data on polar grid, dimensions [0:nlon,0:nr]
!
!   output argument list:
!     fxy    - input data on cartesian grid, dimensions [-nf:nf,-nf:nf].
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$
  use constants, only: zero
  use berror, only: nf,nr
  use gridmod, only: nlon
  implicit none

  real(r_kind),dimension(-nf:nf,-nf:nf) ,intent(  out) :: fxy
  real(r_kind),dimension(nlon+1,0:nr),intent(in   ) :: hlatlon

! Declare local arrays variables:
  integer(i_kind) i,ii,j,jj,i0,j0
  real(r_kind),dimension(-nf:nf,-nf:nf):: rwgt

  rwgt=zero

  do i=0,nr
     do j=1,nlon
        do jj=0,nywtxys(j,i)
           j0=iywtxys(jj,j,i)
           do ii=0,nxwtxys(j,i)
              i0=ixwtxys(ii,j,i)
              rwgt(i0,j0)=rwgt(i0,j0)+ywtxys(jj,j,i)*xwtxys(ii,j,i)
           end do
        end do
     end do
  end do
  call smooth_polcasa(fxy,hlatlon)
  where(rwgt>zero) fxy=fxy/rwgt

end subroutine smooth_caspol


  subroutine smooth_polcasv(fxy,hlatlon)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    smooth_polcas 2d smoothing spline interpolation
!   prgmmr: sato             org: np23                date: 2008-11-03
!
! abstract: almost same as smooth_polcas, but output is not sum but average
!
! program history log:
!   2008-11-03 sato
!
!   input argument list:
!     fxy    - input data on cartesian grid, dimensions [-nf:nf,-nf:nf].
!
!   output argument list:
!     hlatlon_out - output data on polar grid, dimensions [0:nlon,0:nr]
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!$$$

  use constants, only: zero
  use berror, only: nf,nr
  use gridmod, only: nlon
  implicit none

  real(r_kind),dimension(-nf:nf,-nf:nf) ,intent(in   ) :: fxy
  real(r_kind),dimension(nlon+1,0:nr),intent(  out) :: hlatlon

! Declare local arrays variables:
  integer(i_kind) i,ii,j,jj,jjj
  real(r_kind) sum,ywgt,xywgt,sumwgt

  hlatlon=zero
  do i=0,nr
     do j=1,nlon
        sumwgt=zero
        sum=zero
        do jj=0,nywtxys(j,i)
           ywgt=ywtxys(jj,j,i)
           jjj=iywtxys(jj,j,i)
           do ii=0,nxwtxys(j,i)
              xywgt=ywgt*xwtxys(ii,j,i)
              sum=sum+xywgt*fxy(ixwtxys(ii,j,i),jjj)
              sumwgt=sumwgt+xywgt
           end do
        end do
        if(sumwgt>zero) then
           hlatlon(j,i)=sum/sumwgt
        else
           hlatlon(j,i)=sum
        end if
     end do
  end do
  end subroutine smooth_polcasv


subroutine bspline(tin,k,wout)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    bspline
!   prgmmr:
!
! abstract: compute weights for a bspline of order k (degree k-1, continuous k-2)
!               k >= 1
!         for unit spaced grid.
!            0 <= tin <= 1
!
! program history log:
!   2008-04-12  safford - add documentation block
!
!   input argument list:
!     tin   -
!     k     -
!
!   output argument list:
!     wout  -
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block

  use constants, only: zero,one
  implicit none

  integer(i_kind),intent(in   ) :: k
  real(r_kind)   ,intent(in   ) :: tin

  real(r_kind)   ,intent(  out) :: wout(0:k-1)

  integer(i_kind) i,m
  real(r_kind) t,w(0:k),rmi(k)

  do m=1,k
     rmi(m)=one/m
  end do
  t=tin+k-1
  w=zero
  w(k-1)=one
  if(k>1) then
     do m=2,k
        do i=0,k-1
           w(i)=w(i)*(t-i)*rmi(m-1) + w(i+1)*(i+m-t)*rmi(m-1)
        end do
     end do
  end if

  do i=0,k-1
     wout(i)=w(i)
  end do

end subroutine bspline

end module smooth_polcarf
