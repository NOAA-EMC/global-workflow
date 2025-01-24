module abstract_read_wrf_mass_guess_mod
  type, abstract :: abstract_read_wrf_mass_guess_class
  contains
    procedure(read_wrf_mass_binary_guess), deferred, pass(this) :: read_wrf_mass_binary_guess
    procedure(read_wrf_mass_netcdf_guess), deferred, pass(this) :: read_wrf_mass_netcdf_guess
  end type abstract_read_wrf_mass_guess_class

  abstract interface
  subroutine read_wrf_mass_binary_guess(this,mype)
    use kinds, only: i_kind
    import abstract_read_wrf_mass_guess_class
    implicit none
    class(abstract_read_wrf_mass_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
  end subroutine read_wrf_mass_binary_guess
  end interface
  abstract interface
  subroutine read_wrf_mass_netcdf_guess(this,mype)
    use kinds, only: i_kind
    import abstract_read_wrf_mass_guess_class
    implicit none
    class(abstract_read_wrf_mass_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
  end subroutine read_wrf_mass_netcdf_guess
  end interface

end module abstract_read_wrf_mass_guess_mod
