module tcv_mod
!$$$   module documentation block
!                .      .    .                                       .
! module:  tcv_mod
!
! prgmmr:  kleist              org: np23               date: 2009-02-02
!
! abstract: This module contains variables and subroutines to read the
!           TC Vitals file correctly
!
! program history log:
!   2009-02-02  kleist
!   2010-09-08  treadon - add centerid and destroy_tcv_card; code cleanup
!
! Subroutines Included:
!   sub get_storminfo       - loads storm data structure from tc vitals info
!   sub read_tcv_card       - read data structure from tc vitals ascii file
!   sub destroy_tcv_card    - deallocate arrays containing storm information
!   sub init_tcps_errvals   - initialize values for tcps ob error
!
! Variable Definitions:
!   def numstorms    - number of storms in tc vitals file
!   def stormswitch  - integer switch to turn on reading of individual storms
!   def stormid      - storm character identifier
!   def stormlat     - storm latitude
!   def stormlon     - storm longitude
!   def stormpsmin   - storm sea level pressure minimum
!   def stormdattim  - storm dat/time 
!   def centerid     - organization (center) id
!   def stormid      - storm id with basin identifier
!   def tcp_refps    - reference pressure for tcps oberr calculation (mb)
!   def tcp_width    - parameter for tcps oberr inflation (width, mb)
!   def tcp_ermin    - parameter for tcps oberr inflation (minimum oberr, mb)
!   def tcp_ermax    - parameter for tcps oberr inflation (maximum oberr, mb)
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
  public :: get_storminfo
  public :: read_tcv_card
  public :: destroy_tcv_card
  public :: init_tcps_errvals
! set passed variables to public
  public :: stormpsmin,stormdattim,stormlon,numstorms,stormlat,centerid,stormid
  public :: tcvcard
  public :: tcp_refps,tcp_width,tcp_ermin,tcp_ermax

  integer(i_kind) numstorms
  integer(i_kind),dimension(:),allocatable:: stormswitch
  character(len=3),dimension(:),allocatable:: stormid
  character(len=4),dimension(:),allocatable:: centerid
  real(r_kind),dimension(:),allocatable:: stormlat,stormlon,stormpsmin
  integer(i_kind),dimension(:),allocatable:: stormdattim
  real(r_kind) tcp_refps,tcp_width,tcp_ermin,tcp_ermax

  type:: tcvcard ! Define a new type for a TC Vitals card
     character*4    :: tcv_center      ! Hurricane Center Acronym
     character*3    :: tcv_storm_id    ! Storm Identifier (03L, etc)
     character*9    :: tcv_storm_name  ! Storm name
     integer(i_kind):: tcv_century     ! 2-digit century id (19 or 20)
     integer(i_kind):: tcv_yymmdd      ! Date of observation
     integer(i_kind):: tcv_hhmm        ! Time of observation (UTC)
     integer(i_kind):: tcv_lat         ! Storm Lat (*10), always >0
     character*1    :: tcv_latns       ! 'N' or 'S'
     integer(i_kind):: tcv_lon         ! Storm Lon (*10), always >0
     character*1    :: tcv_lonew       ! 'E' or 'W'
     integer(i_kind):: tcv_stdir       ! Storm motion vector (in degr)
     integer(i_kind):: tcv_stspd       ! Spd of storm movement (m/s*10)
     integer(i_kind):: tcv_pcen        ! Min central pressure (mb)
     integer(i_kind):: tcv_penv        ! val outrmost closed isobar(mb)
     integer(i_kind):: tcv_penvrad     ! rad outrmost closed isobar(km)
     integer(i_kind):: tcv_vmax        ! max sfc wind speed (m/s)
     integer(i_kind):: tcv_vmaxrad     ! rad of max sfc wind spd (km)
     integer(i_kind):: tcv_r15ne       ! NE rad of 15 m/s winds (km)
     integer(i_kind):: tcv_r15se       ! SE rad of 15 m/s winds (km)
     integer(i_kind):: tcv_r15sw       ! SW rad of 15 m/s winds (km)
     integer(i_kind):: tcv_r15nw       ! NW rad of 15 m/s winds (km)
     character*1    :: tcv_depth       ! Storm depth (S,M,D) X=missing
  end type tcvcard
  
contains 

  subroutine get_storminfo(lunin)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_storminfo       load tc storm information arrays
!
!   prgmmr: kleist            org: np23                date: 2009-02-02
!
! abstract: loads the tropical storm arrays necessary for the assim.
!           of synthetic tc-mslp observations
!
! program history log:
!   2009-02-02  kleist
!
!   input argument list:
!     lunin    - integer unit from which to read tc-vitals ascii data
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none

    integer(i_kind),intent(in   ) :: lunin

    integer(i_kind) iret,lucard,ii
    type(tcvcard) stormtmp
    type(tcvcard),dimension(:),allocatable:: storminfo

    lucard=lunin

! Find number of storms in tcvitals file
    rewind(lucard)
    ii=0
    do while (.true.)
       read (lucard,21,END=801,ERR=891) stormtmp
       ii = ii + 1
    enddo
 801 continue
!
 21 format (a4,1x,a3,1x,a9,1x,i2,i6,1x,i4,1x,i3,a1,1x,i4,a1,1x,i3,1x, &
            i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)

    numstorms=ii

! Allocate arrays
    allocate(stormswitch(numstorms),stormid(numstorms),centerid(numstorms))
    allocate(stormlat(numstorms),stormlon(numstorms),stormpsmin(numstorms))
    allocate(storminfo(numstorms))
    allocate(stormdattim(numstorms))

    stormswitch=1
    call read_tcv_card(numstorms,storminfo,lucard,stormswitch,stormlon,stormlat,&
         centerid,stormid,stormpsmin,stormdattim,iret)
    deallocate(storminfo)

    if (numstorms>0) then
       iret = 0
       return
    else
       write(6,*)'GET_STORMINFO:  ***ERROR*** num storms to be processed <= 0'
       write(6,*)'GET_STORMINFO:     Check file assigned to unit lucard=',lucard
       iret = 99
       return
    endif

 891 write(6,*)'GET_STORMINFO:  ***ERROR*** in reading unit luncard=',lucard
    iret = 98

    return
  end subroutine get_storminfo

  subroutine read_tcv_card(nums,storm,lucard,stswitch,slonfg,slatfg,centerid,stid,stpsmin,stdattim,iret)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_storminfo       load tc storm information arrays
!
!   prgmmr: kleist            org: np23                date: 2009-02-02
!
! abstract: Reads the tcvitals file for current time and loads necessary
!           storm arrays.
!
! program history log:
!   2009-02-02  kleist
!   2010-03-30  treadon - loop tcvitals read from 1 to nums
!
!   input argument list:
!     nums     - integer number of storms to read
!     stswitch - integer switch to determine whether or not to read in
!     lucard   - integer identifying input file to read from
!
!   output argument list:
!     storm    - array containing data structure with tc vitals info
!     slonfg   - storm longitudes
!     slatfg   - storm latitudes
!     centerid - organization (center) id
!     stid     - storm id
!     stpsmin  - storm minimum sea level pressure (mb)
!     stdattim - storm date and time
!     iret     - integer return flag
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use constants, only: zero,one
    implicit none

    integer(i_kind)                 ,intent(in   ) :: nums,lucard
    integer(i_kind) ,dimension(nums),intent(in   ) :: stswitch

    type(tcvcard)   ,dimension(nums),intent(  out) :: storm
    character(len=4),dimension(nums),intent(  out) :: centerid
    character(len=3),dimension(nums),intent(  out) :: stid
    real(r_kind)    ,dimension(nums),intent(  out) :: slonfg,slatfg,stpsmin
    integer(i_kind) ,dimension(nums),intent(  out) :: stdattim
    integer(i_kind)                 ,intent(  out) :: iret

    integer(i_kind) ict,i,ii

    slonfg = zero; slatfg = zero
!
    rewind(lucard)
    do ii=1,nums
       read (lucard,21,END=901,ERR=991) storm(ii)
    enddo 
 901 continue
!
 21 format (a4,1x,a3,1x,a9,1x,i2,i6,1x,i4,1x,i3,a1,1x,i4,a1,1x,i3,1x, &
            i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)
!
    write(6,*)'READ_TCV_CARD:  Following are the storms to be processed: '
    ict=0
    do i=1,nums
       if (stswitch(i)==1) then
          ict = ict + 1
          write (6,31) storm(i)
 
          if (storm(i)%tcv_lonew == 'W') then
             slonfg(i) =  360._r_kind - float(storm(i)%tcv_lon)/10.0_r_kind
          else
             slonfg(i) = float(storm(i)%tcv_lon)/10.0_r_kind
          endif
          if (storm(i)%tcv_latns == 'S') then
             slatfg(i) = -one * float(storm(i)%tcv_lat)/10.0_r_kind
          else
             slatfg(i) = float(storm(i)%tcv_lat)/10.0_r_kind
          endif
        
          centerid(i) = storm(i)%tcv_center
          stid(i) = storm(i)%tcv_storm_id
          stpsmin(i) = storm(i)%tcv_pcen

          stdattim(i) = 100000000*storm(i)%tcv_century + 100*storm(i)%tcv_yymmdd + storm(i)%tcv_hhmm/100

       endif
       write(6,*)'READ_TCV_CARD:  STORM #, STID,LAT, LON, MINSLP = ',i,stid(i),slatfg(i),slonfg(i),stpsmin(i)
       write(6,*)'READ_TCV_CARD:  STORM DATTIM = ',stdattim(i)
    enddo
 31 format (1x,a4,1x,a3,1x,a9,1x,i2,i6.6,1x,i4.4,1x,i3,a1,1x,i4,a1,1x,i3, &
            1x,i3,3(1x,i4),1x,i2,1x,i3,1x,4(i4,1x),a1)

    if (ict>0) then
       iret = 0
       return
    else
       write(6,*)'READ_TCV_CARD:  ***ERROR*** num storms to be processed <=0 '
       write(6,*)'READ_TCV_CARD:     Check file assigned to unit lucard=',lucard
       iret = 99
       return
    endif
!
  991 write(6,*)'READ_TCV_CARD:  ***ERROR*** in read_tcv_card reading unit lucard=',lucard
    iret = 98
!
    write(6,*) 'END OF READ_TCV_CARD: number of storms to process = ',nums

    return
  end subroutine read_tcv_card

  subroutine destroy_tcv_card
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_tcv_card       deallocate storm information arrays
!
!   prgmmr: treadon           org: np23                date: 2010-09-08
!
! abstract: Deallocate storm information arrays.
!
! program history log:
!   2010-09-08  treadon
!
!   input argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none

    deallocate(stormswitch,stormid,centerid)
    deallocate(stormlat,stormlon,stormpsmin)
    deallocate(stormdattim)

  end subroutine destroy_tcv_card

  subroutine init_tcps_errvals
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_tcps_errvals       initialize parm values
!
!   prgmmr: kleist             org: np23                date: 2010-09-14
!
! abstract: Initialize parameter values for specification of tcps ob error
!
! program history log:
!   2010-09-14  kleist
!
!   input argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    implicit none

!   note:  all values in mb
    tcp_refps=1000.0_r_kind
    tcp_width=50.0_r_kind
    tcp_ermin=0.75_r_kind  
    tcp_ermax=5.0_r_kind

  end subroutine init_tcps_errvals

end module tcv_mod
