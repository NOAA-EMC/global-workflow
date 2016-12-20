!$$$ subprogram documentation block
!               .      .    .
! subprogram:	penalty			build penalty.txt file
!     prgmmr:   safford			date:  2009-11
!
! abstract:	This module contains code to build the penalty.txt file.
!		The penalty.txt file reports the satellite and channel for
!		which a penalty value is likely out of range.
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

module bad_penalty

  implicit none
  
  private

  public :: open_bad_penalty_file
  public :: write_bad_penalty
  public :: close_bad_penalty_file

  integer, parameter       :: funit = 11

  contains


    !-------------------------------------------------------------
    !  open the bad_pen file
    !-------------------------------------------------------------

    subroutine open_bad_penalty_file( date, cycle, fios )

      !--- interface
      character(8), intent( in )	:: date
      character(8), intent( in )	:: cycle
      integer, intent( out )		:: fios     
     
      !--- variables
      logical                           :: lexist = .FALSE.
      character(60)                     :: fname
      
!      write(*,*) '--> open_bad_penalty_file, date, cycle = ', date, cycle

      !--- build the file name
      fname= 'bad_pen.' // trim(date) // trim(cycle)  
      write(*,*) '   fname = ', fname

      !--- open file and write the header
      inquire(file=fname, exist=lexist)
      if( lexist .eqv. .FALSE. ) then
         write(*,*) ' opening new bad_pen file'
         open( UNIT=funit, FILE=fname, STATUS='NEW', IOSTAT=fios )
      else
         write(*,*) ' opening existing bad_penalty.txt file'
         open( UNIT=funit, FILE=fname, STATUS='OLD', POSITION='APPEND', IOSTAT=fios )
      end if

    end subroutine open_bad_penalty_file


    !-------------------------------------------------------------
    !  add an entry to the bad_penalty.txt file
    !-------------------------------------------------------------
    subroutine write_bad_penalty( satname, channel, region, penalty, bound )

      !--- interface
      character(20), intent( in )       :: satname
      integer, intent( in )		:: channel
      integer, intent( in )		:: region
      real, intent( in )		:: penalty
      real, intent( in )                :: bound
      character(60)                     :: myformat

      myformat = "(A20,A10,I5,A9,I1,A10,ES15.7E2,A8,ES15.7E2)"

      
      write(funit,myformat) satname, ' channel= ',channel, ' region= ', region, ' penalty= ', penalty, ' bound= ', bound

    end subroutine write_bad_penalty


    !-------------------------------------------------------------
    !  close the bad_penalty.txt file
    !-------------------------------------------------------------
    subroutine close_bad_penalty_file( )
!      write(*,*) '--> close_bad_penalty_file'
      close( funit ) 
    end subroutine close_bad_penalty_file

end module bad_penalty
