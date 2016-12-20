module aircraftobsqc
!$$$ module documentation block
!           .      .    .                                       .
! module:   aircraftobsqc
!   prgmmr: Ming Hu
!
! abstract: contains subroutines for the qc of aircraft obs based
!           on (i) rejectlists for temperature, wind and RH   
!           the code inquires for the 
!           existence of thsese lists in the gsi working directorty 
!           and applies them if present.
!           
!
! program history log:
!   2010-10-28  Hu
!
! subroutines included:
!   sub init_aircraft_rjlists
!   sub get_aircraft_usagerj
!   sub destroy_aircraft_rjlists
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds, only: i_kind,r_kind

  implicit none

  private

  character(8),allocatable,dimension(:,:)::w_aircraft_rjlist
  character(8),allocatable,dimension(:,:)::t_aircraft_rjlist
  character(8),allocatable,dimension(:,:)::q_aircraft_rjlist

  integer(i_kind) nwrjs_aircraft,ntrjs_aircraft,nqrjs_aircraft

  logical listexist_aircraft

  public init_aircraft_rjlists
  public get_aircraft_usagerj
  public destroy_aircraft_rjlists

contains

subroutine init_aircraft_rjlists
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_aircraft_rjlists
!   prgmmr:
!
! abstract: initialize qc lists 
!
! program history log:
!   2010-10-28  Hu
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

! Declare passed variables

! Declare local variables
  integer(i_kind) aircraft_unit,m
  character(80) cstring

  integer(i_kind), parameter::nmax=500

  data aircraft_unit / 20 /
!**************************************************************************
  nwrjs_aircraft=0
  ntrjs_aircraft=0
  nqrjs_aircraft=0

  allocate(w_aircraft_rjlist(nmax,2))
  allocate(t_aircraft_rjlist(nmax,2))
  allocate(q_aircraft_rjlist(nmax,2))
!
!==> Read in station names from the reject list for 
!    wind,temperature, and humidity if it exists

 inquire(file='current_bad_aircraft',exist=listexist_aircraft)
 if(listexist_aircraft) then
    open (aircraft_unit,file='current_bad_aircraft',form='formatted')
    do m=1,16
       read(aircraft_unit,*,end=141)
    enddo
140 continue
    read(aircraft_unit,'(a30)',end=141) cstring
    if(cstring(11:11) == 'T') then
       ntrjs_aircraft=ntrjs_aircraft+1
       t_aircraft_rjlist(ntrjs_aircraft,1)=cstring(1:8)
       t_aircraft_rjlist(ntrjs_aircraft,2)=cstring(22:29)
    endif
    if(cstring(13:13) == 'W') then
       nwrjs_aircraft=nwrjs_aircraft+1
       w_aircraft_rjlist(nwrjs_aircraft,1)=cstring(1:8)
       w_aircraft_rjlist(nwrjs_aircraft,2)=cstring(22:29)
    endif
    if(cstring(15:15) == 'R') then
       nqrjs_aircraft=nqrjs_aircraft+1
       q_aircraft_rjlist(nqrjs_aircraft,1)=cstring(1:8)
       q_aircraft_rjlist(nqrjs_aircraft,2)=cstring(22:29)
    endif
    goto 140
141 continue
    print*,'aircraft_rejectlist: T, W, R=', ntrjs_aircraft,nwrjs_aircraft,nqrjs_aircraft
 endif
 close(aircraft_unit)
!
end subroutine init_aircraft_rjlists

subroutine get_aircraft_usagerj(kx,obstype,c_station_id,usage_rj)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_aircraft_usagerj
!   prgmmr:
!
! abstract: determine the usage value of read_prepbufr for aircraft obs. the following
!           is done: (i) if incoming usage value is >=100. then do nothing, since
!           read_prepbufr has already flagged this ob and assigned a specific usage 
!           value to it. (ii) use usage=500. for temperature, moisture, or surface pressure
!           obs which are found in the rejectlist. (iii) 
!
! program history log:
!   2010-10-28  Hu
!
!   input argument list:
!    kx
!    obstype
!    c_station_id
!
!   output argument list:
!    usage_rj
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none

  integer(i_kind),intent(in   ) :: kx
  character(10)  ,intent(in   ) :: obstype
  character(8)   ,intent(in   ) :: c_station_id
  real(r_kind)   ,intent(inout) :: usage_rj

! Declare local variables
  integer(i_kind) m,nlen
  character(8)  ch8,ch8MDCRS
  real(r_kind) usage_rj0

! Declare local parameters
  real(r_kind),parameter:: r6    = 6.0_r_kind
  real(r_kind),parameter:: r450  = 450._r_kind

  if (usage_rj >= r6) return

  usage_rj0=usage_rj

  if (kx<190) then  !<==mass obs

     if(obstype=='t' .and. (ntrjs_aircraft > 0) ) then
        do m=1,ntrjs_aircraft
           ch8=t_aircraft_rjlist(m,1)
           ch8MDCRS=t_aircraft_rjlist(m,2)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)).or.(trim(c_station_id) == trim(ch8MDCRS))) then
              usage_rj=r450
              exit
           endif
        enddo
     elseif(obstype=='q' .and. (nqrjs_aircraft > 0) ) then
        do m=1,nqrjs_aircraft
           ch8=q_aircraft_rjlist(m,1)
           ch8MDCRS=q_aircraft_rjlist(m,2)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)).or.(trim(c_station_id) == trim(ch8MDCRS))) then
              usage_rj=r450
              exit
           endif
        enddo
     end if

  elseif (kx>=190) then !<==wind obs

     if(obstype=='uv' .and. (nwrjs_aircraft > 0) ) then
        do m=1,nwrjs_aircraft
           ch8=w_aircraft_rjlist(m,1)
           ch8MDCRS=w_aircraft_rjlist(m,2)
           nlen=len_trim(ch8)
           if ((trim(c_station_id) == trim(ch8)).or.(trim(c_station_id) == trim(ch8MDCRS))) then
              usage_rj=r450
              exit
           endif
        enddo
     endif

  end if
end subroutine get_aircraft_usagerj

subroutine destroy_aircraft_rjlists
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroy_aircraft_rjlists
!   prgmmr:
!
! abstract:
!
! program history log:
!   2010-10-28  Hu
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

  deallocate(w_aircraft_rjlist)
  deallocate(t_aircraft_rjlist)
  deallocate(q_aircraft_rjlist)

end subroutine destroy_aircraft_rjlists

end module aircraftobsqc
