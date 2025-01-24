module read_wrf_mass_guess_mod
use abstract_read_wrf_mass_guess_mod
  type, extends(abstract_read_wrf_mass_guess_class) :: read_wrf_mass_guess_class 
  contains
    procedure, pass(this) :: read_wrf_mass_binary_guess => read_wrf_mass_binary_guess_dummy
    procedure, pass(this) :: read_wrf_mass_netcdf_guess => read_wrf_mass_netcdf_guess_dummy
  end type read_wrf_mass_guess_class 
contains
  subroutine read_wrf_mass_binary_guess_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_mass_binary_guess
  !   prgmmr:
  !
  ! abstract:
  !
  ! program history log:
  !   2009-12-07  lueken - added subprogram doc block and implicit none
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    use kinds,only: i_kind
    implicit none
    class(read_wrf_mass_guess_class),intent(inout) :: this
    integer(i_kind),intent(in)::mype
    write(6,*)'READ_WRF_MASS_BINARY_GUESS:  dummy routine, does nothing!'
  end subroutine read_wrf_mass_binary_guess_dummy
  
  subroutine read_wrf_mass_netcdf_guess_dummy(this,mype)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    read_wrf_mass_netcdf_guess
  !   prgmmr:
  !
  ! abstract:
  !
  ! program history log:
  !   2009-12-07  lueken - added subprogram doc block and implicit none
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm RS/6000 SP
  !
  !$$$ end documentation block
    use kinds,only: i_kind
    implicit none
    class(read_wrf_mass_guess_class),intent(inout) :: this
    integer(i_kind),intent(in)::mype
    write(6,*)'READ_WRF_MASS_NETCDF_GUESS:  dummy routine, does nothing!'
  end subroutine read_wrf_mass_netcdf_guess_dummy
end module read_wrf_mass_guess_mod
