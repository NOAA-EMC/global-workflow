SUBROUTINE read_NASALaRC(mype,lunin,numLaRC,istart,jstart,  &
                         nlon,nlat,nasalarc)
!
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_NASALaRC        read in nasalarc cloud 
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2009-09-21
!
! ABSTRACT: 
!  This subroutine reads in nasalarc cloud products that are already mapped to
!  analysis grid.
!
! PROGRAM HISTORY LOG:
!    2009-01-20  Hu  Add NCO document block
!
!
!   input argument list:
!     mype        - processor ID
!     lunin       - unit in which data are read in
!     numLaRC    - number of observation
!     jstart      - start lon of the whole array on each pe
!     istart      - start lat of the whole array on each pe
!     nlon        - no. of lons on subdomain (buffer points on ends)
!     nlat        - no. of lats on subdomain (buffer points on ends)
!
!   output argument list:
!     nasalarc   - nasalarc cloud  in analysis grid
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

  use gsd_kinds, only: r_kind,i_kind, r_single
  implicit none

  integer(i_kind),intent(in) :: lunin
  integer(i_kind),intent(in) :: mype
  INTEGER(i_kind),intent(in) :: numLaRC 
  INTEGER(i_kind),intent(in) :: nlon,nlat
  integer(i_kind),intent(in) :: istart
  integer(i_kind),intent(in) :: jstart

  real(r_single), intent(out) :: nasalarc(nlon,nlat,5)
!
!  local
!
  real(r_kind),allocatable :: nasalarc_in(:,:)

  character(10)  :: obstype
  integer(i_kind):: nreal,nchanl,ilat1s,ilon1s
  character(20)  :: isis

  INTEGER(i_kind) :: i,j, ii,jj, k
  INTEGER(i_kind) :: ib,jb

  REAL(r_kind)    :: miss_obs_real
  PARAMETER ( miss_obs_real = -99999.0_r_kind )

!
  ib=jstart   ! begin i point of this domain
  jb=istart   ! begin j point of this domain

  ilon1s=1
  ilat1s=2

  read(lunin) obstype,isis,nreal,nchanl

  allocate( nasalarc_in(nreal,numLaRC) )
  nasalarc_in=miss_obs_real

  read(lunin)  nasalarc_in
  DO i=1,numLaRC
    ii=int(nasalarc_in(ilon1s,i)+0.001_r_kind) - ib + 2
    jj=int(nasalarc_in(ilat1s,i)+0.001_r_kind) - jb + 2
    if( ii < 1 .or. ii > nlon ) write(6,*) 'read_nasalarc_cld: ', &
                                'Error in read in nasa ii:',mype,ii,jj,i,ib,jb
    if( jj < 1 .or. jj > nlat ) write(6,*) 'read_nasalarc_cld: ', &
                                'Error in read in nasa jj:',mype,ii,jj,i,ib,jb
    DO k=1,2
      if(nasalarc_in(k+2,i) > 8888.0_r_kind ) then
        nasalarc(ii,jj,k)=miss_obs_real
      else
        nasalarc(ii,jj,k)=nasalarc_in(k+2,i)   ! k=1 w_pcld, 2=w_tcld
      endif
    enddo  ! k

    if(nasalarc_in(5,i) > 8888.0_r_kind ) then
        nasalarc(ii,jj,3)=miss_obs_real
    else
        nasalarc(ii,jj,3)=nasalarc_in(5,i)/100.0_r_kind   ! w_frac
    endif

    if(nasalarc_in(6,i) > 8888.0_r_kind) then
        nasalarc(ii,jj,4)=miss_obs_real
    else
        nasalarc(ii,jj,4)=nasalarc_in(6,i)/1000.0_r_kind  ! w_lwp
    endif

    if(nasalarc_in(7,i) > 8888.0_r_kind ) then
        nasalarc(ii,jj,5)=miss_obs_real
    else
        nasalarc(ii,jj,5)=nasalarc_in(7,i)   ! nlv_cld
    endif
  ENDDO
  deallocate(nasalarc_in)
!
!  filling boundarys
!
  DO k=1,5
    DO i=2,nlon-1
      nasalarc(i,1,k)=nasalarc(i,2,k)
      nasalarc(i,nlat,k)=nasalarc(i,nlat-1,k)
    enddo
    DO j=2,nlat-1
      nasalarc(1,j,k)=nasalarc(2,j,k)
      nasalarc(nlon,j,k)=nasalarc(nlon-1,j,k)
    enddo
    nasalarc(1,1,k)=nasalarc(2,2,k)
    nasalarc(1,nlat,k)=nasalarc(2,nlat-1,k)
    nasalarc(nlon,1,k)=nasalarc(nlon-1,2,k)
    nasalarc(nlon,nlat,k)=nasalarc(nlon-1,nlat-1,k)
  ENDDO


END SUBROUTINE read_NASALaRC

SUBROUTINE read_map_nasalarc(mype,lunin,numobs,istart,jstart,nlon,nlat,  &
                       nasalarc,ioption)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_map_nasalarc     read in NASA LaRC cloud products and map them into analysis grid
!
!   PRGMMR: Ming Hu & Terra Ladwig         ORG: GSD/EMB        DATE: 2015-04-30
!
! ABSTRACT: 
!  This subroutine reads in global NASA LaRC cloud products and map them into analysis grid.
!
! PROGRAM HISTORY LOG:
!    2015-04-20  Hu  This code is based on read_NESDIS 
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
!     nasalarc   - nasalarc cloud  in analysis grid
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
  integer(i_kind),intent(in) :: ioption
  
  real(r_single):: sat_ctp(nlon,nlat)         ! cloud top pressure
  real(r_single):: sat_tem(nlon,nlat)         ! cloud top temperature
  real(r_single):: w_frac(nlon,nlat)          ! cloud fraction
  real(r_single):: w_lwp(nlon,nlat)          ! cloud fraction
  integer(i_kind):: nlev_cld(nlon,nlat)          ! cloud fraction
  real(r_single):: nasalarc(nlon,nlat,5)
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

! do i=1,numobs
! write(6,*)'sliu larcclddata::',mype,data_s(2,i),data_s(3,i)
! end do

! write(6,*)'read_map_nasalarc::',mype, maxval(data_s(7,:)),numobs


  ib=jstart   ! begin i point of this domain
  jb=istart   ! begin j point of this domain
  call map_ctp_lar(mype,ib,jb,nlon,nlat,nn_obs,numobs,data_s,sat_ctp,sat_tem,w_frac,w_lwp,nlev_cld,ioption)
!!
!  filling boundarys
!
  DO i=2,nlon-1
    sat_ctp(i,1)   =sat_ctp(i,2)
    sat_tem(i,1)   =sat_tem(i,2)
    w_frac(i,1)    =w_frac(i,2)
    w_lwp(i,1)    =w_lwp(i,2)
    nlev_cld(i,1)    =nlev_cld(i,2)
    sat_ctp(i,nlat)=sat_ctp(i,nlat-1)
    sat_tem(i,nlat)=sat_tem(i,nlat-1)
    w_frac(i,nlat) =w_frac(i,nlat-1)
    w_lwp(i,nlat) =w_lwp(i,nlat-1)
    nlev_cld(i,nlat) =nlev_cld(i,nlat-1)
  enddo
  DO j=2,nlat-1
    sat_ctp(1,j)   =sat_ctp(2,j)
    sat_tem(1,j)   =sat_tem(2,j)
    w_frac(1,j)    =w_lwp(2,j)
    w_lwp(1,j)    =w_lwp(2,j)
    nlev_cld(1,j)    =nlev_cld(2,j)
    sat_ctp(nlon,j)=sat_ctp(nlon-1,j)
    sat_tem(nlon,j)=sat_tem(nlon-1,j)
    w_frac(nlon,j) =w_frac(nlon-1,j)
    w_lwp(nlon,j) =w_lwp(nlon-1,j)
    nlev_cld(nlon,j) =nlev_cld(nlon-1,j)
  enddo
  sat_ctp(1,1)      =sat_ctp(2,2)
  sat_tem(1,1)      =sat_tem(2,2)
  w_frac(1,1)       =w_frac(2,2)
  w_lwp(1,1)       =w_lwp(2,2)
  nlev_cld(1,1)       =nlev_cld(2,2)

  sat_ctp(1,nlat)   =sat_ctp(2,nlat-1)
  sat_tem(1,nlat)   =sat_tem(2,nlat-1)
  w_frac(1,nlat)    =w_frac(2,nlat-1)
  w_lwp(1,nlat)    =w_lwp(2,nlat-1)
  nlev_cld(1,nlat)    =nlev_cld(2,nlat-1)

  sat_ctp(nlon,1)   =sat_ctp(nlon-1,2)
  sat_tem(nlon,1)   =sat_tem(nlon-1,2)
  w_frac(nlon,1)    =w_frac(nlon-1,2)
  w_lwp(nlon,1)    =w_lwp(nlon-1,2)
  nlev_cld(nlon,1)    =nlev_cld(nlon-1,2)

  sat_ctp(nlon,nlat)=sat_ctp(nlon-1,nlat-1)
  sat_tem(nlon,nlat)=sat_tem(nlon-1,nlat-1)
  w_frac(nlon,nlat) =w_frac(nlon-1,nlat-1)

  do i=1,nlon
  do j=1,nlat
    nasalarc(i,j,1)=sat_ctp(i,j)
    nasalarc(i,j,2)=sat_tem(i,j)
    nasalarc(i,j,3)=w_frac(i,j) !/100.0
    nasalarc(i,j,4)=w_lwp(i,j)  !/100.0
    nasalarc(i,j,5)=nlev_cld(i,j)
!   if(abs(sat_tem(i,j))>0.and.abs(sat_tem(i,j))<400) then
!    write(6,*)'sat_tem2 in read_cloud::',sat_ctp(i,j),sat_tem(i,j),nasalarc(i,j,1)
!   end if
  end do
  end do
  

END SUBROUTINE read_map_nasalarc
