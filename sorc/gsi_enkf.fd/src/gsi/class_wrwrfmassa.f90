module abstract_wrwrfmassa_mod
  type, abstract :: abstract_wrwrfmassa_class
  contains
    procedure(wrwrfmassa_binary), deferred, pass(this) :: wrwrfmassa_binary  
    procedure(wrwrfmassa_netcdf), deferred, pass(this) :: wrwrfmassa_netcdf
  end type abstract_wrwrfmassa_class

  abstract interface
  subroutine wrwrfmassa_binary(this,mype)
    use kinds, only: i_kind
    import abstract_wrwrfmassa_class
    implicit none
    class(abstract_wrwrfmassa_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  end subroutine wrwrfmassa_binary
  end interface

  abstract interface
  subroutine wrwrfmassa_netcdf(this,mype)
    use kinds, only: i_kind
    import abstract_wrwrfmassa_class
    implicit none
    class(abstract_wrwrfmassa_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  end subroutine wrwrfmassa_netcdf
  end interface
end module abstract_wrwrfmassa_mod
