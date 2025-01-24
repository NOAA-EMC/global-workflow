module abstract_regional_io_mod 
  type, abstract :: abstract_regional_io_class
    integer, allocatable :: dummy(:)
    contains
      procedure(init_regional_io), deferred, pass(this) :: init_regional_io
      procedure(write_regional_analysis), deferred, pass(this) :: write_regional_analysis
      procedure(convert_regional_guess), deferred, pass(this) :: convert_regional_guess
  end type abstract_regional_io_class

  abstract interface
  subroutine init_regional_io(this)
    import abstract_regional_io_class
    implicit none
    class(abstract_regional_io_class), intent(inout) :: this
  end subroutine init_regional_io
  end interface

  abstract interface
  subroutine write_regional_analysis(this,mype)
    use kinds, only: i_kind
    import abstract_regional_io_class
    implicit none
    class(abstract_regional_io_class), intent(inout) :: this
    integer(i_kind),intent(in):: mype
  end subroutine write_regional_analysis
  end interface

  abstract interface
  subroutine convert_regional_guess(this, mype,ctph0,stph0,tlm0)
    use kinds, only: i_kind,r_kind
    import abstract_regional_io_class
    implicit none
    class(abstract_regional_io_class), intent(inout) :: this
    integer(i_kind),intent(in   ) :: mype
    real(r_kind)   ,intent(  out) :: ctph0,stph0,tlm0
  end subroutine convert_regional_guess
  end interface


end module abstract_regional_io_mod


