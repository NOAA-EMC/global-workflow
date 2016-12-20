!$$$ module documentation block
!           .      .    .                                       .
! module:   stub_timermod
!  prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: module providing interface to timing procedures
!
! program history log:
!   2007-10-01  todling
!   2009-02-26  todling - if-def from GMAO_FVGSI to GEOS_PERT
!   2009-08-13  lueken - update documentation
!   2010-06-16  guo - separated stub implementation with implicit interfaces
!		      from module implementation with explicit interfaces.
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic)
!
! subroutines included:
!   sub timer_init_
!   sub timer_final_
!   sub timer_pri_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

subroutine timer_init_ (str)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    timer_init_       initialize procedure timing
!
!   prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: initializes timer
!
! program history log:
!   2007-10-01  todling
!
!   input argument list:
!     str - string designation for process to be timed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
character(len=*),intent(in   ) :: str
end subroutine timer_init_

subroutine timer_final_ (str)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    timer_final_       finalize procedure timing
!
!   prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: finalize timer
!
! program history log:
!   2007-10-01  todling
!
!   input argument list:
!     str - string designation for process timed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none
character(len=*),intent(in   ) :: str
end subroutine timer_final_

subroutine timer_pri_ (lu)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    timer_pri_       summarizes timing results
!
!   prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: summary of timing results
!
! program history log:
!   2007-10-01  todling
!
!   input argument list:
!     str - string designation for process timed
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only : i_kind
implicit none
integer(i_kind),intent(in   ) :: lu
end subroutine timer_pri_
