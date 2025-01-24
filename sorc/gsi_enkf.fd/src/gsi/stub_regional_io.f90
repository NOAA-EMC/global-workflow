!$$$   module documentation block
!                .      .    .                                       .
! module:  stub_regional_io
! prgmmr:  mahajan           org: np23                date: 2016-06-14
!
! abstract: This contains stub routines that handle the input/output
!           of regional gsi guess(analysis) grids
!
! program history log:
!   2016-06-14  mahajan - initial version
!   
! Subroutines Included:
!   sub init_regional_io        - initialize regional_io needs
!   sub convert_regional_guess  - convert regional guess to internal format
!   sub write_regional_analysis - write regional analysis
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
module regional_io_mod
use abstract_regional_io_mod
  type, extends(abstract_regional_io_class) :: regional_io_class
  contains
      procedure, pass(this) :: init_regional_io => init_regional_io_dummy 
      procedure, pass(this) :: write_regional_analysis => write_regional_analysis_dummy 
      procedure, pass(this) :: convert_regional_guess => convert_regional_guess_dummy
  end type regional_io_class

logical,parameter:: VERBOSE=.false.
!logical,parameter:: VERBOSE=.true.
contains
  subroutine init_regional_io_dummy(this)
    implicit none
    class(regional_io_class), intent(inout) :: this
    if(VERBOSE) write(6,*) 'DUMMY CALL to init_regional_io'
    return
  end subroutine init_regional_io_dummy

  subroutine write_regional_analysis_dummy(this,mype)
    use kinds, only: i_kind
    implicit none
    class(regional_io_class), intent(inout) :: this
    integer(i_kind),intent(in):: mype
    if(VERBOSE) write(6,*) 'DUMMY CALL to write_regional_analysis'
    return
  end subroutine write_regional_analysis_dummy

  subroutine convert_regional_guess_dummy(this,mype,ctph0,stph0,tlm0)
    use kinds, only: i_kind,r_kind
    implicit none
    class(regional_io_class), intent(inout) :: this
    integer(i_kind),intent(in   ) :: mype
    real(r_kind)   ,intent(  out) :: ctph0,stph0,tlm0
    ctph0 = 0.0_r_kind
    stph0 = 0.0_r_kind
    tlm0 = 0.0_r_kind
    if(VERBOSE) write(6,*) 'DUMMY CALL to convert_regional_guess'
    return
  end subroutine convert_regional_guess_dummy

end module regional_io_mod
