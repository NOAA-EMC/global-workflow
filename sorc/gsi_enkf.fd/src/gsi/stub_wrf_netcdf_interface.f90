!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  wrf_netcdf_interface
!
! !DESCRIPTION: This stub provides the default interfaces to the 
!               WRF netcdf capability in GSI.
!
! !REVISION HISTORY:
!
!  08Jun2016 Mahajan - Initial code
!
!EOP
!-------------------------------------------------------------------------
module convert_netcdf_mod
use abstract_convert_netcdf_mod 
  type, extends(abstract_convert_netcdf_class) :: convert_netcdf_class
  contains
    procedure, pass(this) :: convert_netcdf_mass => convert_netcdf_mass_dummy
    procedure, pass(this) :: convert_netcdf_nmm  => convert_netcdf_nmm_dummy
    procedure, pass(this) :: update_netcdf_mass  => update_netcdf_mass_dummy
    procedure, pass(this) :: update_netcdf_nmm   => update_netcdf_nmm_dummy
  end type convert_netcdf_class
contains
  subroutine convert_netcdf_mass_dummy(this)
    
    implicit none
    class(convert_netcdf_class) ,intent(inout) :: this
  end subroutine convert_netcdf_mass_dummy
  
  subroutine convert_netcdf_nmm_dummy(this,update_pint,ctph0,stph0,tlm0,guess)
    use kinds, only: r_single,i_kind,r_kind
    use constants, only: zero 
    implicit none
    class(convert_netcdf_class) ,intent(inout) :: this
    logical     ,intent(in   ) :: guess
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
    ctph0 = zero 
    stph0 = zero 
    tlm0 = zero 
  end subroutine convert_netcdf_nmm_dummy
  
  subroutine update_netcdf_mass_dummy(this)
  
    implicit none
    class(convert_netcdf_class) ,intent(inout) :: this
  
  end subroutine update_netcdf_mass_dummy
  
  subroutine update_netcdf_nmm_dummy(this)
  
    implicit none
    class(convert_netcdf_class) ,intent(inout) :: this
  
  end subroutine update_netcdf_nmm_dummy
end module convert_netcdf_mod
