module abstract_convert_netcdf_mod 
  type, abstract :: abstract_convert_netcdf_class
    contains
      procedure(convert_netcdf_mass), deferred, pass(this) :: convert_netcdf_mass 
      procedure(convert_netcdf_nmm), deferred, pass(this) :: convert_netcdf_nmm  
      procedure(update_netcdf_mass), deferred, pass(this) :: update_netcdf_mass 
      procedure(update_netcdf_nmm), deferred, pass(this) :: update_netcdf_nmm  
  end type abstract_convert_netcdf_class

  abstract interface
  subroutine convert_netcdf_mass(this)
    import abstract_convert_netcdf_class 
    implicit none
    class(abstract_convert_netcdf_class), intent(inout) :: this
  end subroutine convert_netcdf_mass
  end interface

  abstract interface
  subroutine update_netcdf_nmm(this)
    import abstract_convert_netcdf_class 
    implicit none
    class(abstract_convert_netcdf_class), intent(inout) :: this
  
  end subroutine update_netcdf_nmm
  end interface

  abstract interface
  subroutine update_netcdf_mass(this)
    import abstract_convert_netcdf_class 
    implicit none
    class(abstract_convert_netcdf_class), intent(inout) :: this
  
  end subroutine update_netcdf_mass
  end interface

  abstract interface
  subroutine convert_netcdf_nmm(this,update_pint,ctph0,stph0,tlm0,guess)
    use kinds, only: r_single,i_kind,r_kind
    import abstract_convert_netcdf_class 
    implicit none
    class(abstract_convert_netcdf_class), intent(inout) :: this
    logical     ,intent(in   ) :: guess
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
  
  end subroutine convert_netcdf_nmm
  end interface
end module abstract_convert_netcdf_mod


