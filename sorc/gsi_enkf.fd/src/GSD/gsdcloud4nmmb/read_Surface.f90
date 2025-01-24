SUBROUTINE read_Surface(mype,lunin,regional_time,istart,jstart,nlon,nlat,& 
                  numsao,NVARCLD_P,OI,OJ,OCLD,OWX,Oelvtn,Odist,cstation, &
                  OIstation,OJstation)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_Surface   read in cloud observations in surface observation
!
!   PRGMMR: Ming Hu          ORG: GSD/AMB        DATE: 2006-10-30
!
! ABSTRACT: 
!  This subroutine read in cloud observations in surface observation
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
!     numsao      - maximum observation number (observation number)
!     NVARCLD_P   - first dimension of OLCD
!
!   output argument list:
!
!     OI          -  observation x location
!     OJ          -  observation y location
!     OLCD        -  cloud amount, cloud height, visibility
!     OWX         -  weather observation
!     Oelvtn      -  observation elevation
!     Odist       -  distance from the nearest station
!     cstation    -  station name

!
! USAGE:
!   INPUT FILES: 
!
!   OUTPUT FILES:
!
!
! REMARKS:
!
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90 
!   MACHINE:  Linux cluster (WJET)
!
!$$$
!

  use kinds, only: r_single,i_kind,r_kind,r_double

  implicit none

  integer(i_kind), intent(in) :: mype
  integer(i_kind), intent(in) :: lunin
  integer(i_kind), intent(in) :: regional_time(6)
  integer(i_kind), intent(in) :: istart
  integer(i_kind), intent(in) :: jstart
  INTEGER(i_kind), intent(in) :: nlon,nlat
  INTEGER(i_kind), intent(in) :: numsao
  INTEGER(i_kind), intent(in) :: NVARCLD_P

  real(r_single),  intent(out) :: OI(numsao)              ! x location, grid
  real(r_single),  intent(out) :: OJ(numsao)              ! y location, grid
  INTEGER(i_kind), intent(out) :: OCLD(NVARCLD_P,numsao)  ! cloud amount, cloud height,
                                                          ! visibility
  CHARACTER*10,    intent(out) :: OWX(numsao)             ! weather
  real(r_single),  intent(out) :: Oelvtn(numsao)          ! elevation
  real(r_single),  intent(out) :: Odist(numsao)           ! distance from the nearest station
  character(8),    intent(out) :: cstation(numsao)        ! station name
  real(r_single),  intent(out) :: OIstation(numsao)       ! x location, station
  real(r_single),  intent(out) :: OJstation(numsao)       ! y location, station

!
! temp.
!
  character*12   :: adate
  character*9    :: STANAM  ! stattion name
  real(r_single) :: LAT     ! latitude
  real(r_single) :: LON     ! longitude

  real(r_single) :: VIS     ! horizontal visibility
  real(r_single) :: CLD(3)  ! cloud base height
  character*10   :: WX      ! weather
  character*8    :: sky(3)  ! cloud cover or amount

!
!  misc.
!
  real(r_kind),allocatable,dimension(:,:):: data_s
  logical,allocatable,dimension(:):: luse
  character(10)  :: obstype
  integer(i_kind):: nreal,nchanl
  character(20)  :: isis

  INTEGER(i_kind) :: nn_obs
  real(r_kind)    ::  cldamt,awx,cldhgt
  character*3     :: msky,mwx
  INTEGER(i_kind) :: i,j,k,k2,ic,jb,ib
  integer(i_kind) :: start, end

  real(r_kind)    ::     spval_p
  parameter (spval_p = 99999.)

  real(r_double) rstation_id
  character(8) :: cstation1,cc,ci
  equivalence(cstation1,rstation_id)


!====================================================================
!  Begin
  OWX=''
  OCLD=-99999
 
  ib=jstart   ! begin i point of this domain
  jb=istart   ! begin j point of this domain

!
  read(lunin) obstype,isis,nreal,nchanl

  nn_obs = nreal + nchanl
  allocate(luse(numsao),data_s(nn_obs,numsao))
  read(lunin) data_s, luse 
!
! read in ruface observations:
! station name, x location, y location, longitude, latitude, elevation
! visibility, cloud amount, cloud height, weather
!
    DO i=1,numsao
       rstation_id=data_s(1,i)
       cstation(i)=cstation1
       OI(i) = data_s(2,i) - ib + 2  ! covert it to the local grid   
       OJ(i) = data_s(3,i) - jb + 2  ! covert it to the local grid
       if( OI(i) < 1 .or. OI(i) > nlon ) write(6,*) 'read_Surface: Error in reading ii:',mype,OI(i),ib,jb
       if( OJ(i) < 1 .or. OJ(i) > nlat ) write(6,*) 'read_Surface: Error in reading jj:',mype,OJ(i),ib,jb
       Oelvtn(i)  = data_s(4,i)
       Odist(i)   = data_s(23,i)
       OIstation(i) = data_s(24,i)
       OJstation(i) = data_s(25,i)
       if(data_s(22,i) > 50 ) cycle   ! do not use this data
       VIS   = data_s(5,i)
! cloud amonut and base height
! C 020011
!     0 0 oktas (0/10)
!     1 1 okta or less, but not zero (1/10 or less, but not zero)
!     2 2 oktas (2/10 - 3/10)
!     3 3 oktas (4/10)
!     4 4 oktas (5/10)
!     5 5 oktas (6/10)
!     6 6 oktas (7/10 - 8/10)
!     7 7 oktas or more, but not 8 oktas (9/10 or more, but not 10/10)
!     8 8 oktas (10/10)
!     9 Sky obscured by fog and/or other meteorological phenomena
!    10 Sky partially obscured by fog and/or other meteorological phenomena
!    11 Scattered
!    12 Broken
!    13 Few
!    14 Reserved
!    15 Cloud cover is indiscernible for reasons other than
!                 fog or other meteorological phenomena, or observation is not made

       DO j=1,3
          cldamt =  data_s(5+j,i)         ! cloud amount
          cldhgt =  int(data_s(11+j,i))   ! cloud bottom height
          if(cldamt < spval_p .and. cldhgt < spval_p) then
            if(abs(cldamt-0._r_kind) < 0.0001_r_kind) then 
              OCLD(j,i)=0                 !msky='CLR'
              cldhgt=spval_p              
            elseif(abs(cldamt-13._r_kind) < 0.0001_r_kind) then
              OCLD(j,i)=1                 !msky='FEW'
            elseif(abs(cldamt-11._r_kind) < 0.0001_r_kind) then 
              OCLD(j,i)=2                 !msky='SCT'
            elseif(abs(cldamt-12._r_kind) < 0.0001_r_kind) then
              OCLD(j,i)=3                 !msky='BKN'
            elseif((abs(cldamt-8._r_kind) < 0.0001_r_kind) .or. & 
                   (abs(cldamt-9._r_kind) < 0.0001_r_kind)) then
              OCLD(j,i)=4                 !   msky='OVC'   msky='VV '
            elseif(abs(cldamt-1._r_kind) < 0.0001_r_kind) then 
              OCLD(j,i)=1                
            elseif(abs(cldamt-2._r_kind) < 0.0001_r_kind .or.   &
                   abs(cldamt-3._r_kind) < 0.0001_r_kind  ) then 
              OCLD(j,i)=2                
            elseif(cldamt > 3.5_r_kind .and. cldamt < 6.5_r_kind  ) then 
              OCLD(j,i)=3                
            elseif(abs(cldamt-7._r_kind) < 0.0001_r_kind ) then 
              OCLD(j,i)=4                
            else
              OCLD(j,i) = spval_p         ! wrong cloud observation type
              cldhgt = spval_p
            endif
            if(cldhgt > 0.0_r_kind ) then
              OCLD(6+j,i) = cldhgt
            else
              OCLD(j,i) = spval_p
              OCLD(6+j,i) = spval_p
            endif
          else
              OCLD(j,i) = 99
              OCLD(6+j,i) = spval_p
          endif
       enddo   ! j
! weather
       DO j=1,3
          awx    =  data_s(17+j,i)        ! weather
          mwx='   '
          if(awx>=10._r_kind .and.awx<=12._r_kind ) mwx='BR '
          if(awx>=110._r_kind.and.awx<=112._r_kind) mwx='BR '
          if(awx==5._r_kind  .or. awx==105._r_kind) mwx='HZ '
          if(awx>=40._r_kind .and.awx<=49._r_kind ) mwx='FG '
          if(awx>=130._r_kind.and.awx<=135._r_kind) mwx='FG '
          if(awx>=50._r_kind .and.awx<=59._r_kind ) mwx='DZ '
          if(awx>=150._r_kind.and.awx<=159._r_kind) mwx='DZ '
          if(awx>=60._r_kind .and.awx<=69._r_kind ) mwx='RA '
          if(awx>=160._r_kind.and.awx<=169._r_kind) mwx='RA '
          if(awx>=70._r_kind .and.awx<=78._r_kind ) mwx='SN '
          if(awx>=170._r_kind.and.awx<=178._r_kind) mwx='SN '
          if(awx==79._r_kind .or. awx==179._r_kind) mwx='PE '

          if(awx>=80._r_kind .and.awx<=90._r_kind ) mwx='SH '
          if(awx>=180._r_kind.and.awx<=187._r_kind) mwx='SH '
          if(awx>=91._r_kind .and.awx<=99._r_kind ) mwx='TH '
          if(awx>=190._r_kind.and.awx<=196._r_kind) mwx='TH '

          if (j==1) start=1
          if (j==2) start=4
          if (j==3) start=7
          end=start+2
          OWX(i)(start:end)=mwx
       enddo
! visiblity
       IF(VIS > spval_P) then
          OCLD(13,i)=spval_P
       else
          IF(VIS > 100.0_r_kind ) then
            OCLD(13,i)=int(VIS)
          elseif(VIS <=100.0_r_kind .and. VIS > 0.0_r_kind ) then
            OCLD(13,i)=100
!           write(6,*) 'read_Surface, Warning: change visibility to 100 m !!!'
          ENDIF
       endif

    ENDDO    ! i = numsao
!

END SUBROUTINE read_Surface

