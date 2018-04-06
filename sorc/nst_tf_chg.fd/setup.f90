module setup
!$$$   module documentation block
!                .      .    .                                       .
! module:  setup
! prgmmr:  Xu Li                                       date: 2017-03-13
!
! abstract: This module contains variables to run nst_tf_chg
!
! program history log:
! 
! Subroutines Included:
!   sub init_grdmod   - initialize grided related variables to default values
!
! Variable Definitions:
!   def nsmth       - the number of 9-point smooth
!   def missing_value - missing_value
!
!$$$ end documentation block

  integer :: nsmth,istyp
  
contains

  subroutine init_setup
!
! abstract:  set defaults for observation related variables
!
    implicit none

!   Initialize arrays used in namelist obs_input 
    nsmth = 0
    istyp = 0
  end subroutine init_setup
  
end module setup


