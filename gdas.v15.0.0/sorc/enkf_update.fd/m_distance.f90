module m_distance

!$$$ module documentation block
!           .      .    .                                       .
! module:   m_distance   calculates great circle distance between two points 
!   prgmmr: eliu 
!
! abstract: module to calculate the great circle distance between two points 
! assimilation
!
! program history log:
!   1996-10-01  Joiner/Karki - initial coding from NASA/GMAO 
!   2012-02-15  eliu         - reformat to use in GSI 
!
! subroutines included:
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use constants, only: deg2rad,half,two
  use kinds,     only: i_kind, r_kind

  interface distance
    module procedure distance1
    module procedure distance2
  end interface

  contains

  function distance1( alat, alon, blat, blon ) result (dist)
  implicit NONE


  !INPUT PARAMETERS:
  real(r_kind), dimension(:), intent(in) :: alat  ! latitude of the 1st point
  real(r_kind), dimension(:), intent(in) :: alon  ! longitude of the 1st point                                   
  real(r_kind), dimension(:), intent(in) :: blat  ! latitude of the 2nd point            
  real(r_kind), dimension(:), intent(in) :: blon  ! lontigude of the 2nd point                           

  !OUTPUT PARAMETERS:
  real(r_kind), dimension(size(alat)) :: dist ! function output, great circle distance         

  integer(i_kind), parameter      :: maxlat=3000
  integer(i_kind)                 :: nlat
  real(r_kind), dimension(maxlat) :: work0
  real(r_kind), dimension(maxlat) :: work1
  real(r_kind), dimension(maxlat) :: work2

  !Initialize
  !----------
  dist=0.

  !Compute distances
  !=================
  !>>orig 
  !nlat=size(alat)
  !work0(1:nlat)=sin((alat-blat)*deg2rad*0.5 )**2
  !work1(1:nlat)=sin((alon-blon)*deg2rad*0.5 )**2
  !work2(1:nlat)=cos((alat+blat)*deg2rad*0.5 )**2
  !dist=(work2(1:nlat)-work0(1:nlat))*work1(1:nlat)+work0(1:nlat)
  !dist=asin(sqrt(dist))/deg2rad
  !<<orig

  !>>new    
  nlat=size(alat)
  work0(1:nlat)=sin((alat-blat)*deg2rad*half)**two
  work1(1:nlat)=sin((alon-blon)*deg2rad*half)**two
  work2(1:nlat)=cos((alat+blat)*deg2rad*half)**two
  dist=(work2(1:nlat)-work0(1:nlat))*work1(1:nlat)+work0(1:nlat)
  dist=asin(sqrt(dist))*two
  dist= 6372.8_r_kind*dist 
  !<<new

  !>>test 
  !nlat=size(alat)
  !work0(1:nlat)=sin((alat-blat)*deg2rad*0.5_r_kind )**2.0_r_kind
  !work1(1:nlat)=sin((alon-blon)*deg2rad*0.5_r_kind )**2.0_r_kind
  !work2(1:nlat)=cos(alat*deg2rad)*cos(blat*deg2rad)
  !dist = work0(1:nlat)+work2(1:nlat)*work1(1:nlat)
  !dist = sqrt(dist) 
  !dist = 2.0_r_kind*asin(min(1.0_r_kind,minval(dist)))
  !dist = 6372.8_r_kind*dist 
  !<<test

  end function distance1

  function distance2( alat, alon, blat, blon ) result (dist)

  real(r_kind), intent(in)   :: alat
  real(r_kind), intent(in)   :: alon
  real(r_kind), intent(in)   :: blat
  real(r_kind), intent(in)   :: blon
  real(r_kind)               :: dist

  real(r_kind), dimension(1) :: alat2
  real(r_kind), dimension(1) :: alon2
  real(r_kind), dimension(1) :: blat2
  real(r_kind), dimension(1) :: blon2
  real(r_kind), dimension(1) :: dist2

  alat2(1) = alat
  alon2(1) = alon
  blat2(1) = blat
  blon2(1) = blon
  dist2    = distance1( alat2, alon2, blat2, blon2 )
  dist     = dist2(1)

  end function distance2

end module m_distance

