module timermod

!$$$ module documentation block
!           .      .    .                                       .
! module:   timermod
!  prgmmr: todling          org: gmao                date: 2007-10-01
!
! abstract: module providing interface to timing procedures
!
! program history log:
!   2007-10-01  todling
!   2009-02-26  todling - if-def from GMAO_FVGSI to GEOS_PERT
!   2009-08-13  lueken  - update documentation
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic)
!
! subroutines included:
!   sub timer_ini
!   sub timer_fnl
!   sub timer_pri
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

private

public timer_ini
public timer_fnl
public timer_pri

interface timer_ini
   subroutine timer_init_ (str)
   implicit none
   character(len=*),intent(in   ) :: str
   end subroutine timer_init_
end interface

interface timer_fnl
   subroutine timer_final_ (str)
   implicit none
   character(len=*),intent(in   ) :: str
   end subroutine timer_final_
end interface

interface timer_pri
   subroutine timer_pri_ (lu)
   use kinds, only : i_kind
   implicit none
   integer(i_kind),intent(in   ) :: lu
   end subroutine timer_pri_
end interface

end module timermod
