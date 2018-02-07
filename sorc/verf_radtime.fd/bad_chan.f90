!$$$ subprogram documentation block
!               .      .    .
! subprogram:	bad_chan	build bad_chan.txt file
!     prgmmr:   safford			date:  2009-11
!
! abstract:	This module contains code to build the bad_chan.txt file.
!		The bad_chan.txt file reports the satellite and channel for
!		which zero obs were found on a channel that is set for use
!               and normally has data.
!
! program history log:
!	2009-12-22 safford - initial coding
!
! contains:
!
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

module bad_chan

  implicit none
  
  private

  public :: open_bad_chan_file
  public :: write_bad_chan
  public :: close_bad_chan_file

  integer, parameter       :: funit = 14

  contains


    !-------------------------------------------------------------
    !  create the bad_chan file
    !-------------------------------------------------------------

    subroutine open_bad_chan_file( date, cycle, fios )

      !--- interface
      character(8), intent( in )	:: date
      character(8), intent( in )	:: cycle
      integer, intent( out )		:: fios     
    
      !--- variables
      logical 				:: lexist = .FALSE. 
      character(60)                     :: fname


!      write(*,*) '--> open_bad_chan_file, date, cycle = ', date, cycle
      !--- build the file name
      fname = 'bad_chan.' // trim(date) // trim(cycle)

      write(*,*) '   fname = ', fname
      
      !--- open file and write the header
      inquire(file=fname, exist=lexist)
      if( lexist .eqv. .FALSE. ) then
         write(6,*) ' opening new bad_chan file'
         open( UNIT=funit, FILE=fname, STATUS='NEW', IOSTAT=fios )
      else
         write(6,*) ' opening existing bad_chan.txt file'
         open( UNIT=funit, FILE=fname, STATUS='OLD', POSITION='APPEND', IOSTAT=fios )
      end if
    
    end subroutine open_bad_chan_file


    subroutine write_bad_chan( satname, channel )

      !--- interface
      character(20), intent( in )       :: satname
      integer, intent( in )		:: channel
      
      !--- variables
      real                              :: count

      write(6,*) 'write_bad_chan, satname, channel', satname, channel

      write(funit,*) satname, 'channel= ', channel

    end subroutine write_bad_chan


    subroutine close_bad_chan_file( )
!      write(6,*) '--> close_bad_chan_file'
      close( funit ) 
    end subroutine close_bad_chan_file

end module bad_chan
