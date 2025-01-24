module abstract_wrwrfnmma_mod
  type, abstract :: abstract_wrwrfnmma_class
  contains
    procedure(wrwrfnmma_binary), deferred, pass(this) :: wrwrfnmma_binary  
    procedure(wrwrfnmma_netcdf), deferred, pass(this) :: wrwrfnmma_netcdf
  end type abstract_wrwrfnmma_class

  abstract interface
  subroutine wrwrfnmma_binary(this,mype)
    use kinds, only: i_kind
    import abstract_wrwrfnmma_class
    implicit none
    class(abstract_wrwrfnmma_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  end subroutine wrwrfnmma_binary
  end interface

  abstract interface
  subroutine wrwrfnmma_netcdf(this,mype)
    use kinds, only: i_kind
    import abstract_wrwrfnmma_class
    implicit none
    class(abstract_wrwrfnmma_class), intent(inout) :: this 
    integer(i_kind),intent(in   ) :: mype
  end subroutine wrwrfnmma_netcdf
  end interface
end module abstract_wrwrfnmma_mod
