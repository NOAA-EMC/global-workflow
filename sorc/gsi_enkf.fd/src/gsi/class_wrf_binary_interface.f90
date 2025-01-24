module abstract_get_wrf_binary_interface_mod
  type, abstract :: abstract_get_wrf_binary_interface_class
  contains
    procedure(convert_binary_mass), deferred, pass(this) :: convert_binary_mass
    procedure(convert_binary_nmm), deferred, pass(this) :: convert_binary_nmm
    procedure(convert_nems_nmmb), deferred, pass(this) :: convert_nems_nmmb
  end type abstract_get_wrf_binary_interface_class

  abstract interface
  subroutine convert_binary_mass(this)
    import abstract_get_wrf_binary_interface_class
    implicit none
    class(abstract_get_wrf_binary_interface_class), intent(inout) :: this
  end subroutine convert_binary_mass
  end interface

  abstract interface
  subroutine convert_binary_nmm(this,update_pint,ctph0,stph0,tlm0)
    use kinds, only: r_kind
    import abstract_get_wrf_binary_interface_class
    implicit none
    class(abstract_get_wrf_binary_interface_class), intent(inout) :: this
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0

  end subroutine convert_binary_nmm
  end interface

  abstract interface
  subroutine convert_nems_nmmb(this,update_pint,ctph0,stph0,tlm0)
    use kinds, only: r_kind
    import abstract_get_wrf_binary_interface_class
    implicit none
    class(abstract_get_wrf_binary_interface_class), intent(inout) :: this
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
  end subroutine convert_nems_nmmb
  end interface

end module abstract_get_wrf_binary_interface_mod
