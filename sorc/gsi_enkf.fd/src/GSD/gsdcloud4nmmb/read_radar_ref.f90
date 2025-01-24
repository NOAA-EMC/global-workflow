SUBROUTINE read_radar_ref(mype,lunin,regional_time,istart,jstart,   &
                         nlon,nlat,Nmsclvl,numref,ref_mosaic31)
!
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_NESDIS     read in radar reflectivity    
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-11-30
!
! ABSTRACT: 
!  This subroutine read in radar reflectivity
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
!     numref      - number of observation
!
!   output argument list:
!     Nmsclvl     - vertical level of radar observation ref_mosaic31
!     ref_mosaic31- radar reflectivity horizontally in analysis grid and 
!                       vertically in mosaic grid (height)
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
  use kinds, only: r_kind,i_kind
  implicit none

  INTEGER(i_kind),intent(in) :: mype
  INTEGER(i_kind),intent(in) :: nlon,nlat
  integer(i_kind),intent(in) :: lunin
  integer(i_kind),intent(in) :: regional_time(6)
  integer(i_kind),intent(in) :: istart
  integer(i_kind),intent(in) :: jstart
  INTEGER(i_kind),intent(in) :: numref

  INTEGER(i_kind),intent(out):: Nmsclvl
  real(r_kind),   intent(out):: ref_mosaic31(nlon,nlat,31)
!
!  local 
!
  real(r_kind),allocatable :: ref_in(:,:)

  character(10) :: obstype
  integer(i_kind):: nreal,nchanl,ilat1s,ilon1s
  character(20) :: isis

  INTEGER(i_kind) :: i,j, ii,jj,k2, k
  INTEGER(i_kind) :: ib,jb

!
  ib=jstart   ! begin i point of this domain
  jb=istart   ! begin j point of this domain

  read(lunin) obstype,isis,nreal,nchanl

  ilon1s=1
  ilat1s=2
  Nmsclvl = nreal - 2
  IF( Nmsclvl .ne. 21 .and. Nmsclvl .ne.31) then
     write(6,*) ' read_radar_ref: ',      &
                'vertical dimesion inconsistent when read in reflectivty mosaic'
     write(6,*) 'read in:',Nmsclvl
     write(6,*) 'need:', 21, 'or', 31
     call stop2(114)
  ENDIF
  allocate( ref_in(nreal,numref) )
  ref_mosaic31=-9999.0_r_kind

  read(lunin)  ref_in
  DO i=1,numref
    ii=int(ref_in(ilon1s,i)+0.001_r_kind) - ib + 2
    jj=int(ref_in(ilat1s,i)+0.001_r_kind) - jb + 2
    if( ii < 1 .or. ii > nlon ) write(6,*) 'read_radar_ref: ', &
                                'Error in read in ref ii:',mype,ii,jj,i,ib,jb
    if( jj < 1 .or. jj > nlat ) write(6,*) 'read_radar_ref: ', &
                                'Error in read in ref jj:',mype,ii,jj,i,ib,jb
    DO k=1,Nmsclvl
      ref_mosaic31(ii,jj,k)=ref_in(2+k,i)
    ENDDO
  ENDDO
  deallocate(ref_in)

END SUBROUTINE read_radar_ref
