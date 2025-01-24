SUBROUTINE read_NESDIS(mype,lunin,numobs,istart,jstart,nlon,nlat,  &
                       sat_ctp,sat_tem,w_frac,npts_rad,ioption)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_NESDIS     read in NESDIS cloud products and map them into analysis grid
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-30
!
! ABSTRACT: 
!  This subroutine read in NESDIS cloud products and map them into analysis grid
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     lunin       - unit in which data are read in
!     numobs      - number of observation
!     jstart      - start lon of the whole array on each pe
!     istart      - start lat of the whole array on each pe
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!
!   output argument list:
!     sat_ctp     - GOES cloud top pressure in analysis grid
!     sat_tem     - GOES cloud top temperature in analysis grid
!     w_frac      - GOES cloud coverage in analysis grid
!
!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!
!_____________________________________________________________________
!

  use gsd_kinds, only: r_single,i_kind,r_kind

  implicit none

  integer(i_kind),intent(in) :: mype
  integer(i_kind),intent(in) :: lunin
  INTEGER(i_kind),intent(in) :: numobs
  INTEGER(i_kind),intent(in) :: nlon,nlat
  integer(i_kind),intent(in) :: istart
  integer(i_kind),intent(in) :: jstart
  INTEGER(i_kind),intent(in) :: npts_rad
  INTEGER(i_kind),intent(in) :: ioption
  
  real(r_single), intent(out):: sat_ctp(nlon,nlat)         ! cloud top pressure
  real(r_single), intent(out):: sat_tem(nlon,nlat)         ! cloud top temperature
  real(r_single), intent(out):: w_frac(nlon,nlat)          ! cloud fraction
!
  INTEGER(i_kind) :: nn_obs
  real(r_kind),allocatable,dimension(:,:):: data_s
  logical,allocatable,dimension(:):: luse
!
! misc.
!
  character(10)   :: obstype
  integer(i_kind) :: mm1
  integer(i_kind) :: nreal,nchanl
  character(20)   :: isis

  INTEGER(i_kind) :: i, j
  INTEGER(i_kind) :: ib, jb
!        
! ===============================================================
!

  mm1=mype+1

  read(lunin) obstype,isis,nreal,nchanl
  nn_obs = nreal + nchanl
  allocate(luse(numobs),data_s(nn_obs,numobs))
  read(lunin) data_s, luse 
!
  ib=jstart   ! begin i point of this domain
  jb=istart   ! begin j point of this domain
  call map_ctp (ib,jb,nlon,nlat,nn_obs,numobs,data_s,sat_ctp,sat_tem,w_frac,npts_rad,ioption)
!!
!  filling boundarys
!
  DO i=2,nlon-1
    sat_ctp(i,1)   =sat_ctp(i,2)
    sat_tem(i,1)   =sat_tem(i,2)
    w_frac(i,1)    =w_frac(i,2)
    sat_ctp(i,nlat)=sat_ctp(i,nlat-1)
    sat_tem(i,nlat)=sat_tem(i,nlat-1)
    w_frac(i,nlat) =w_frac(i,nlat-1)
  enddo
  DO j=2,nlat-1
    sat_ctp(1,j)   =sat_ctp(2,j)
    sat_tem(1,j)   =sat_tem(2,j)
    w_frac(1,j)    =w_frac(2,j)
    sat_ctp(nlon,j)=sat_ctp(nlon-1,j)
    sat_tem(nlon,j)=sat_tem(nlon-1,j)
    w_frac(nlon,j) =w_frac(nlon-1,j)
  enddo
  sat_ctp(1,1)      =sat_ctp(2,2)
  sat_tem(1,1)      =sat_tem(2,2)
  w_frac(1,1)       =w_frac(2,2)
  sat_ctp(1,nlat)   =sat_ctp(2,nlat-1)
  sat_tem(1,nlat)   =sat_tem(2,nlat-1)
  w_frac(1,nlat)    =w_frac(2,nlat-1)
  sat_ctp(nlon,1)   =sat_ctp(nlon-1,2)
  sat_tem(nlon,1)   =sat_tem(nlon-1,2)
  w_frac(nlon,1)    =w_frac(nlon-1,2)
  sat_ctp(nlon,nlat)=sat_ctp(nlon-1,nlat-1)
  sat_tem(nlon,nlat)=sat_tem(nlon-1,nlat-1)
  w_frac(nlon,nlat) =w_frac(nlon-1,nlat-1)

END SUBROUTINE read_NESDIS
