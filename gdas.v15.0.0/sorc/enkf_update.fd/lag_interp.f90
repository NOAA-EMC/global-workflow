module lag_interp
!$$$ module documentation block
!           .      .    .                                       .
! module:   lag_interp
!   prgmmr: meunier          org:                     date: 2009-03-11
!
! abstract:  This module contains 3D interpolation function for the
!            wind field. This version differ from the original
!            version of GSI because :
!            - it operates on global grids (not on subdomains like other
!            routines)
!            - in the tangent linear and adjoint, an increment on the horizontal
!            location may be given (not on the vertical position)
!            - a constant pressure grid is used for the vertical interpolation,
!            thus these routines are only reliable at high levels where delp is
!            constant. This constant pressure grid must be initialised prior to
!            use.
!
! module history log:
!   2009-03-11  meunier
!   2011-08-01  lueken  - removed double &
!
! subroutines included:
!   sub lag_inipgrid
!   sub lag_delpgrid
!   sub lag_gridrel_ijk
!   sub lag_retr_3d
!   sub lag_int3d_ad
!
! functions included:
!   lag_index_h
!   lag_index_3d
!   lag_int3d_nl
!   lag_int3d_tl
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:  
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind
  use gridmod, only: nlon,nlat,nsig,rlons,rlats
  use constants, only: zero,one

  implicit none

  private
  public:: lag_accur
  public:: lag_inipgrid,lag_delpgrid,lag_logcte_p
  public:: lag_gridrel_ijk,lag_index_h
  public:: lag_int3d_nl,lag_int3d_tl,lag_int3d_ad

  ! Accuracy of localisation to determine wether or not a point is on the grid
  real(r_kind)::lag_accur = 1.0e-6_r_kind

  ! to store the constant pressure grid
  real(r_kind),dimension(:),allocatable::lag_logcte_p

  contains


  ! ------------------------------------------------------------------------
  ! Set the constant pressure grid for interpolatin
  ! ------------------------------------------------------------------------
  subroutine lag_inipgrid(newgrid)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_inipgrid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    newgrid
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),dimension(:),intent(in   ) :: newgrid

    call lag_delpgrid()
    allocate(lag_logcte_p(size(newgrid)))
    lag_logcte_p=newgrid

  end subroutine lag_inipgrid


  ! ------------------------------------------------------------------------
  ! deallocate the constant pressure grid
  ! ------------------------------------------------------------------------
  subroutine lag_delpgrid()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_delpgrid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
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

    if (allocated(lag_logcte_p)) &
       deallocate(lag_logcte_p)

  end subroutine lag_delpgrid

  
  ! ------------------------------------------------------------------------
  ! Give grid relative coordinates for lon,lat and level (use the GSI routine)
  ! ------------------------------------------------------------------------
  subroutine lag_gridrel_ijk(lon,lat,p,i,j,k)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_gridrel_ijk
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!   2013-01-23  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!
!   input argument list:
!    lat,lon,p
!
!   output argument list:
!    i,j,k
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    real(r_kind),intent(in   ) :: lon,lat,p
    real(r_kind),intent(  out) :: i,j,k

    ! Use the function already implemented
    i=lon
    call grdcrd1(i,rlons,nlon,1)
    j=lat
    call grdcrd1(j,rlats,nlat,1)
    k=log(p)
    call grdcrd1(k,lag_logcte_p,nsig,-1)
  end subroutine lag_gridrel_ijk

  
  ! ------------------------------------------------------------------------
  ! Give the global array index number for a grid point on horizontal dimension
  ! ------------------------------------------------------------------------
  function lag_index_h(lon,lat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_index_h
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lat,lon
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    integer(i_kind),intent(in   ) :: lon,lat

    integer(i_kind)::lag_index_h

    lag_index_h=lat+(lon-1)*nlat
  end function lag_index_h
  ! ------------------------------------------------------------------------
  ! Give the global array index number for a grid point on 3D grid
  ! ------------------------------------------------------------------------
  function lag_index_3d(lonlat,sig)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_index_3d
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lonlat,sig
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    integer(i_kind),intent(in   ) :: lonlat,sig

    integer(i_kind)::lag_index_3d

    lag_index_3d=lonlat+(sig-1)*nlat*nlon
  end function lag_index_3d
  ! ------------------------------------------------------------------------
  ! Retrieve horizontal index and sigma level from a 3D index number
  ! ------------------------------------------------------------------------
  subroutine lag_retr_3d(i3d,lonlat,sig)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_retr_3d
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    i3d
!
!   output argument list:
!    lonlat,sig
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    integer(i_kind),intent(in   ) :: i3d
    integer(i_kind),intent(  out) :: lonlat,sig
 
    sig   =(i3d/(nlat*nlon))+1
    lonlat=mod(i3d,nlat*nlon)
  end subroutine lag_retr_3d

  
  ! ------------------------------------------------------------------------
  ! 3D interpolation : non linear version
  ! ------------------------------------------------------------------------
  function lag_int3d_nl(field,lon,lat,p,lspec_i,lspec_r)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_int3d_nl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    field
!    lat,lon,p
!
!   output argument list:
!    lspec_i
!    lspec_r
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    use constants, only: half
    implicit none

    ! field = Field from which to interpolate : two dimensions
    !  1st : horizontal location (see lag_index_h above)
    !  2nd : vertical location
    real(r_kind)   ,dimension(:,:)        ,intent(in   ) :: field
    ! lon, lat (radians) and p (hPa) where the interpolation is requested
    real(r_kind)                          ,intent(in   ) :: lat,lon,p
    ! specification for the tangent-linear and adjoint (optional)
    integer(i_kind),dimension(8) ,optional,intent(  out) :: lspec_i
    real(r_kind)   ,dimension(10),optional,intent(  out) :: lspec_r
    ! Interpolated value
    real(r_kind)::lag_int3d_nl
  
    logical::lv_spec

    ! Location of points used in the interpolation
    integer(i_kind),dimension(3)::i111,i211,i121,i221,i112,i212,i122,i222
    ! Location of points used in the determination of the TL parameters
    integer(i_kind),dimension(3)::im111,im112,im121,im122
    integer(i_kind),dimension(3)::i1m11,i1m12,i2m11,i2m12
    ! Relative distance to the origin (in the grid box)
    real(r_kind)::rdx,rdy,rdz
    ! Coefficients in lon and lat for the TL
    real(r_kind)::rcx,rcy
    ! Grid relative coordinates
    real(r_kind)::rlong,rlatg,rpg
    ! Weight coefficients for the interpolation
    real(r_kind)   ,dimension(8)::rcoeff
    ! Horizontal location of points for the interpolation
    integer(i_kind),dimension(8)::ihoriz
    ! Vertical location of points for the interpolation
    integer(i_kind),dimension(8)::isigma
    
    integer(i_kind)::i

    lv_spec=present(lspec_i) .and. present(lspec_r)

    ! Calculate grid relative coordinates
    call lag_gridrel_ijk(lon,lat,p,rlong,rlatg,rpg)
    
    ! Position of the grid points for the upper level
    i111(1)=floor(rlong); i111(2)=floor(rlatg); i111(3)=floor(rpg)
    i211=i111; i211(1)=i111(1)+1; if (i211(1)>nlon) i211(1)=1
    i121=i111; i121(2)=i111(2)+1; if (i121(2)>nlat) i121(2)=1
    i221(1)=i211(1); i221(2)=i121(2); i221(3)=i111(3)
    
    ! Position of the grid points for the lower level
    i112=i111; i112(3)=i111(3)+1; if (i112(3)>nsig) i112(3)=nsig
    i212=i211; i212(3)=i112(3)
    i122=i121; i122(3)=i112(3)
    i222=i221; i222(3)=i112(3)
    
    ! Distance of the point to the origin in grid relative coordinates 
    rdx=rlong-real(i111(1),r_kind)
    rdy=rlatg-real(i111(2),r_kind)
    rdz=rpg  -real(i111(3),r_kind)
    
    ! Calculate coefficients for each of the 8 points use to interpolate
    rcoeff(1)=(one-rdz)*(one-rdx-rdy+rdx*rdy)
    ihoriz(1)=lag_index_h(i111(1),i111(2))
    isigma(1)=i111(3)
    rcoeff(2)=(one-rdz)*(rdx-rdx*rdy)
    ihoriz(2)=lag_index_h(i211(1),i211(2))
    isigma(2)=i211(3)
    rcoeff(3)=(one-rdz)*(rdy-rdx*rdy)
    ihoriz(3)=lag_index_h(i121(1),i121(2))
    isigma(3)=i121(3)
    rcoeff(4)=(one-rdz)*rdx*rdy
    ihoriz(4)=lag_index_h(i221(1),i221(2))
    isigma(4)=i221(3)

    rcoeff(5)=rdz*(one-rdx-rdy+rdx*rdy)
    ihoriz(5)=lag_index_h(i112(1),i112(2))
    isigma(5)=i112(3)
    rcoeff(6)=rdz*(rdx-rdx*rdy)
    ihoriz(6)=lag_index_h(i212(1),i212(2))
    isigma(6)=i212(3)
    rcoeff(7)=rdz*(rdy-rdx*rdy)
    ihoriz(7)=lag_index_h(i122(1),i122(2))
    isigma(7)=i122(3)
    rcoeff(8)=rdz*rdx*rdy
    ihoriz(8)=lag_index_h(i222(1),i222(2))
    isigma(8)=i222(3)

    !interpolation
    lag_int3d_nl = zero
    do i=1,8
       lag_int3d_nl = lag_int3d_nl + rcoeff(i)*field(ihoriz(i),isigma(i))
    end do

    ! Have the parameters for the tangent linear to be retrieved ?
    if (lv_spec) then

       ! lon and lat coefficients first
      
       ! On a grid point ?
       if (abs(rdx)<lag_accur .and. abs(rdy)<lag_accur) then
          im111=i111; im111(1)=i111(1)-1; if (im111(1)<1) im111(1)=nlon
          i1m11=i111; i1m11(2)=i111(2)-1; if (i1m11(2)<1) i1m11(2)=nlat
          im112=i112; im112(1)=i112(1)-1; if (im112(1)<1) im112(1)=nlon
          i1m12=i112; i1m12(2)=i112(2)-1; if (i1m12(2)<1) i1m12(2)=nlat
          rcx=half*( &
            (one-rdz)*(field(lag_index_h(i211(1),i211(2)),i211(3))- &
              field(lag_index_h(im111(1),im111(2)),im111(3))) + &
            rdz*(field(lag_index_h(i212(1),i212(2)),i212(3))- &
              field(lag_index_h(im112(1),im112(2)),im112(3))) )
          rcy=half*( &
            (one-rdz)*(field(lag_index_h(i121(1),i121(2)),i121(3))- &
              field(lag_index_h(i1m11(1),i1m11(2)),i1m11(3))) + &
            rdz*(field(lag_index_h(i122(1),i122(2)),i122(3))- &
              field(lag_index_h(i1m12(1),i1m12(2)),i1m12(3))) )

       ! On a longitude line ?
       elseif (abs(rdx)<lag_accur .and. abs(rdy)>lag_accur) then
          im111=i111; im111(1)=i111(1)-1; if (im111(1)<1) im111(1)=nlon
          im112=i112; im112(1)=i112(1)-1; if (im112(1)<1) im112(1)=nlon
          im121=i121; im121(1)=i121(1)-1; if (im121(1)<1) im121(1)=nlon
          im122=i122; im122(1)=i122(1)-1; if (im122(1)<1) im122(1)=nlon
          rcx=half*( &
            (one-rdz)*(field(lag_index_h(i211(1),i211(2)),i211(3))-&
                     field(lag_index_h(im111(1),im111(2)),im111(3))+&
                     rdy*(field(lag_index_h(im111(1),im111(2)),im111(3)) -&
                          field(lag_index_h(i211(1),i211(2)),i211(3)) -&
                          field(lag_index_h(im121(1),im121(2)),im121(3)) +&
                          field(lag_index_h(i221(1) ,i221(2)) ,i221(3))) )   +&
            (    rdz)*(field(lag_index_h(i212(1),i212(2)),i212(3))-&
                     field(lag_index_h(im112(1),im112(2)),im112(3))+&
                     rdy*(field(lag_index_h(im112(1),im112(2)),im112(3)) -&
                          field(lag_index_h(i212(1),i212(2)),i212(3)) -&
                          field(lag_index_h(im122(1),im122(2)),im122(3)) +&
                          field(lag_index_h(i222(1) ,i222(2)) ,i222(3))) ) )
          rcy=(one-rdz)*(field(lag_index_h(i121(1),i121(2)),i121(3))-&
                       field(lag_index_h(i111(1),i111(2)),i111(3))) +&
              (    rdz)*(field(lag_index_h(i122(1),i122(2)),i122(3))-&
                       field(lag_index_h(i112(1),i112(2)),i112(3)))
                     
       ! On a lattitude line ?
       elseif (abs(rdx)>lag_accur .and. abs(rdy)<lag_accur) then
          i1m11=i111; i1m11(2)=i111(2)-1; if (i1m11(2)<1) i1m11(2)=nlat
          i1m12=i112; i1m12(2)=i112(2)-1; if (i1m12(2)<1) i1m12(2)=nlat
          i2m11=i211; i2m11(2)=i211(2)-1; if (i2m11(2)<1) i2m11(2)=nlat
          i2m12=i212; i2m12(2)=i212(2)-1; if (i2m12(2)<1) i2m12(2)=nlat
          rcx=(one-rdz)*(field(lag_index_h(i211(1),i211(2)),i211(3))-&
                       field(lag_index_h(i111(1),i111(2)),i111(3))) +&
              (    rdz)*(field(lag_index_h(i212(1),i212(2)),i212(3))-&
                       field(lag_index_h(i112(1),i112(2)),i112(3)))
          rcy=half*( &
            (one-rdz)*(field(lag_index_h(i121(1),i121(2)),i121(3))-&
                     field(lag_index_h(i1m11(1),i1m11(2)),i1m11(3))+&
                     rdx*(field(lag_index_h(i1m11(1),i1m11(2)),i1m11(3)) -&
                          field(lag_index_h(i121(1),i121(2)),i121(3)) -&
                          field(lag_index_h(i2m11(1),i2m11(2)),i2m11(3)) +&
                          field(lag_index_h(i221(1) ,i221(2)) ,i221(3))) )   +&
            (    rdz)*(field(lag_index_h(i122(1),i122(2)),i122(3))-&
                     field(lag_index_h(i1m12(1),i1m12(2)),i1m12(3))+&
                     rdx*(field(lag_index_h(i1m12(1),i1m12(2)),i1m12(3)) -&
                          field(lag_index_h(i122(1),i122(2)),i122(3)) -&
                          field(lag_index_h(i2m12(1),i2m12(2)),i2m12(3)) +&
                          field(lag_index_h(i222(1) ,i222(2)) ,i222(3))) ) )

       ! "Normal" case
       else 
          rcx=(field(lag_index_h(i211(1),i211(2)),i211(3))-&
                field(lag_index_h(i111(1),i111(2)),i111(3))+&
                rdy*(field(lag_index_h(i111(1),i111(2)),i111(3))-&
                     field(lag_index_h(i211(1),i211(2)),i211(3))-&
                     field(lag_index_h(i121(1),i121(2)),i121(3))+&
                     field(lag_index_h(i221(1),i221(2)),i221(3)))&  
              )*(one-rdz)+&
              (field(lag_index_h(i212(1),i212(2)),i212(3))-&
                field(lag_index_h(i112(1),i112(2)),i112(3))+&
                rdy*(field(lag_index_h(i112(1),i112(2)),i112(3))-&
                     field(lag_index_h(i212(1),i212(2)),i212(3))-&
                     field(lag_index_h(i122(1),i122(2)),i122(3))+&
                     field(lag_index_h(i222(1),i222(2)),i222(3)))&  
              )*rdz
          rcy=(field(lag_index_h(i121(1),i121(2)),i121(3))-&
                field(lag_index_h(i111(1),i111(2)),i111(3))+&
                rdx*(field(lag_index_h(i111(1),i111(2)),i111(3))-&
                     field(lag_index_h(i211(1),i211(2)),i211(3))-&
                     field(lag_index_h(i121(1),i121(2)),i121(3))+&
                     field(lag_index_h(i221(1),i221(2)),i221(3)))&  
              )*(one-rdz)+&
              (field(lag_index_h(i122(1),i122(2)),i122(3))-&
                field(lag_index_h(i112(1),i112(2)),i112(3))+&
                rdx*(field(lag_index_h(i112(1),i112(2)),i112(3))-&
                     field(lag_index_h(i212(1),i212(2)),i212(3))-&
                     field(lag_index_h(i122(1),i122(2)),i122(3))+&
                     field(lag_index_h(i222(1),i222(2)),i222(3)))&  
              )*rdz
       end if
       rcx=rcx/(rlons(i211(1))-rlons(i111(1)))
       rcy=rcy/(rlats(i121(2))-rlats(i111(2)))

       ! save weights and locations for the interpolation
       do i=1,8
          lspec_i(i)=lag_index_3d(ihoriz(i),isigma(i))
          lspec_r(i)=rcoeff(i)
       end do
       lspec_r(9:10)=(/ rcx,rcy /)
 
    end if

  end function lag_int3d_nl

  ! ------------------------------------------------------------------------
  ! 3D interpolation : linearised
  ! ------------------------------------------------------------------------
  function lag_int3d_tl(lspec_i,lspec_r,dlon,dlat,dfield)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_int3d_tl
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_i
!    lspec_r
!    dlat,dlon
!    dfield
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    integer(i_kind),dimension(8)  ,intent(in   ) :: lspec_i
    real(r_kind)   ,dimension(10) ,intent(in   ) :: lspec_r
    real(r_kind)                  ,intent(in   ) :: dlat,dlon
    real(r_kind)   ,dimension(:,:),intent(in   ) :: dfield

    real(r_kind)::lag_int3d_tl
    integer(i_kind)::i,horiz,sigma

    lag_int3d_tl=zero

    do i=1,8
       call lag_retr_3d(lspec_i(i),horiz,sigma)
       lag_int3d_tl=lag_int3d_tl+dfield(horiz,sigma)*lspec_r(i)
    end do

    lag_int3d_tl=lag_int3d_tl+&
       lspec_r(9)*dlon + lspec_r(10)*dlat
    
  end function lag_int3d_tl

  ! ------------------------------------------------------------------------
  ! 3D interpolation : adjoint
  ! ------------------------------------------------------------------------
  subroutine lag_int3d_ad(lspec_i,lspec_r,adint3d,adlon,adlat,adfield)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lag_inipgrid
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-05  lueken - added subprogram doc block
!
!   input argument list:
!    lspec_i
!    lspec_r
!    adint3d
!    adlat,adlon
!    adfield
!
!   output argument list:
!    adlat,adlon
!    adfield
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
    implicit none

    integer(i_kind),dimension(8)  ,intent(in   ) :: lspec_i
    real(r_kind)   ,dimension(10) ,intent(in   ) :: lspec_r
    real(r_kind)                  ,intent(in   ) :: adint3d
    real(r_kind)                  ,intent(inout) :: adlat,adlon
    real(r_kind)   ,dimension(:,:),intent(inout) :: adfield

    integer(i_kind)::i,horiz,sigma

    do i=1,8
       call lag_retr_3d(lspec_i(i),horiz,sigma)
       adfield(horiz,sigma)=adfield(horiz,sigma)+lspec_r(i)*adint3d
    end do
    
    adlon=adlon+lspec_r(9) *adint3d
    adlat=adlat+lspec_r(10)*adint3d
    
  end subroutine lag_int3d_ad


end module lag_interp
