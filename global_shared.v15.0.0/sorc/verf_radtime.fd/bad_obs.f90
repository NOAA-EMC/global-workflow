!$$$ subprogram documentation block
!               .      .    .
! subprogram:	bad_obs				build bad_obs.txt file
!     prgmmr:   safford			date:  2009-11
!
! abstract:	This module contains code to build the bad_obs.txt file.
!		The bad_obs.txt file reports the satellite and channel for
!		which an unexpected number of observations were found in 
!               assimilated radiance data.  
!
! program history log:
!	2009-11-23 safford - initial coding
!
! contains:
!
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

module bad_obs

  implicit none
  
  private

  public :: open_bad_obs_file
  public :: write_bad_obs
  public :: close_bad_obs_file

  integer, parameter       :: funit = 10

  contains


    !-------------------------------------------------------------
    !  create the bad_obs file
    !-------------------------------------------------------------

    subroutine open_bad_obs_file( date, cycle, fios )

      !--- interface
      character(8), intent( in )	:: date
      character(8), intent( in )	:: cycle
      integer, intent( out )		:: fios     
    
      !--- variables
      logical 				:: lexist = .FALSE. 
      character(60)                     :: fname


      write(*,*) '--> open_bad_obs_file, date, cycle = ', date, cycle
      !--- build the file name
      fname = 'bad_obs.' // trim(date) // trim(cycle)

      !--- open file and write the header
      inquire(file=fname, exist=lexist)
      if( lexist .eqv. .FALSE. ) then
         write(*,*) ' opening new bad_obs file'
         open( UNIT=funit, FILE=fname, STATUS='NEW', IOSTAT=fios )
      else
         write(*,*) ' opening existing bad_obs.txt file'
         open( UNIT=funit, FILE=fname, STATUS='OLD', POSITION='APPEND', IOSTAT=fios )
      end if
    
    end subroutine open_bad_obs_file


    subroutine write_bad_obs( satname, channel, region, num_obs )

      !--- interface
      character(20), intent( in )       :: satname
      integer, intent( in )		:: channel
      integer, intent( in )             :: region
      real, intent( in )		:: num_obs
      
      !--- variables
      real                              :: count

      !--- 
!      if( num_obs  < 0.0 ) then
!         count = 0.0
!      else
          count = num_obs
!       end if

      write(funit,*) satname, 'channel= ', channel, '	region= ', region, '	num_obs= ', count

    end subroutine write_bad_obs


    subroutine close_bad_obs_file( )
      write(*,*) '--> close_bad_obs_file'
      close( funit ) 
    end subroutine close_bad_obs_file

end module bad_obs
