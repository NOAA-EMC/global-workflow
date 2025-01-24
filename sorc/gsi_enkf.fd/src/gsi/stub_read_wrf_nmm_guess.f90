module read_wrf_nmm_guess_mod
use abstract_read_wrf_nmm_guess_mod
  type, extends(abstract_read_wrf_nmm_guess_class) :: read_wrf_nmm_guess_class 
  contains
    procedure, pass(this) :: read_wrf_nmm_binary_guess => read_wrf_nmm_binary_guess_dummy
    procedure, pass(this) :: read_wrf_nmm_netcdf_guess => read_wrf_nmm_netcdf_guess_dummy
    procedure, pass(this) :: read_nems_nmmb_guess => read_nems_nmmb_guess_dummy
  end type read_wrf_nmm_guess_class 
contains
  subroutine read_wrf_nmm_binary_guess_dummy(this,mype)
    use kinds, only: i_kind
    implicit none
  
  ! Declare passed variables here
    class(read_wrf_nmm_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
    write(6,*)'READ_WRF_NMM_BINARY_GUESS:  dummy routine, does nothing!'
  end subroutine read_wrf_nmm_binary_guess_dummy
  
  subroutine read_wrf_nmm_netcdf_guess_dummy(this,mype)
    use kinds, only: i_kind
    implicit none
  
  ! Declare passed variables here
    class(read_wrf_nmm_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
    write(6,*)'READ_WRF_NMM_NETCDF_GUESS:  dummy routine, does nothing!'
  end subroutine read_wrf_nmm_netcdf_guess_dummy
  subroutine read_nems_nmmb_guess_dummy(this,mype)
    use kinds, only: i_kind
    implicit none
  
  ! Declare passed variables here
    class(read_wrf_nmm_guess_class),intent(inout) :: this
    integer(i_kind),intent(in):: mype
    write(6,*)'READ_NEMS_NMMB_GUESS:  dummy routine, does nothing!'
  end subroutine read_nems_nmmb_guess_dummy

end module read_wrf_nmm_guess_mod
