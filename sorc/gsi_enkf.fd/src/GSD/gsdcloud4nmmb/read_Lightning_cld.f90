SUBROUTINE read_Lightning2cld(mype,lunin,regional_time,istart,jstart,  &
                              nlon,nlat,numlight,lightning)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_NESDIS     read in lightning flash rate  
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2008-11-30
!
! ABSTRACT: 
!  This subroutine read in lightning flash rate
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     lunin       - unit in which data are read in
!     regional_time - analysis time
!     jstart      - start lon of the whole array on each pe
!     istart      - start lat of the whole array on each pe
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!     numlight    - number of observation
!
!   output argument list:
!     lightning   - lightning flash rate in analysis grid
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

  use kinds, only: r_kind,i_kind, r_single
  implicit none

  integer(i_kind),intent(in) :: lunin
  integer(i_kind),intent(in) :: mype
  INTEGER(i_kind),intent(in) :: nlon,nlat
  integer(i_kind),intent(in) :: regional_time(6)
  integer(i_kind),intent(in) :: istart
  integer(i_kind),intent(in) :: jstart
  INTEGER(i_kind),intent(in) :: numlight 

  real(r_single), intent(out):: lightning(nlon,nlat)
!
!  local
!
  real(r_kind),allocatable :: light_in(:,:)

  character(10) :: obstype
  integer(i_kind):: nreal,nchanl,ilat1s,ilon1s
  character(20) :: isis

  INTEGER(i_kind) :: i,j, ii,jj,k2, k
  INTEGER(i_kind) :: ib,jb

!
  ib=jstart   ! begin i point of this domain
  jb=istart   ! begin j point of this domain

  ilon1s=1
  ilat1s=2

  read(lunin) obstype,isis,nreal,nchanl

  allocate( light_in(nreal,numlight) )
  light_in=-9999.0_r_kind

  read(lunin)  light_in
  DO i=1,numlight
    ii=int(light_in(ilon1s,i)+0.001_r_kind) - ib + 2
    jj=int(light_in(ilat1s,i)+0.001_r_kind) - jb + 2
    if( ii < 1 .or. ii > nlon ) write(6,*) 'read_Lightning_cld: ', &
                                'Error in read in lightning ii:',mype,ii,jj,i,ib,jb
    if( jj < 1 .or. jj > nlat ) write(6,*) 'read_Lightning_cld:', &
                                'Error in read in lightning jj:',mype,ii,jj,i,ib,jb
    lightning(ii,jj)=light_in(3,i)
  ENDDO
  deallocate(light_in)

END SUBROUTINE read_Lightning2cld
