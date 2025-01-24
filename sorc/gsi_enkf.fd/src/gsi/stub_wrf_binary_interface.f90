!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  wrf_binary_interface
!
! !DESCRIPTION: This stub provides the default interfaces to the 
!               WRF binary capability in GSI.
!
! !REVISION HISTORY:
!
!  08Jun2016 Mahajan - Initial code
!
!EOP
!-------------------------------------------------------------------------
module get_wrf_binary_interface_mod
use abstract_get_wrf_binary_interface_mod
  type, extends(abstract_get_wrf_binary_interface_class) :: get_wrf_binary_interface_class
  contains
    procedure, pass(this) :: convert_binary_mass => convert_binary_mass_dummy
    procedure, pass(this) :: convert_binary_nmm => convert_binary_nmm_dummy
    procedure, pass(this) :: convert_nems_nmmb => convert_nems_nmmb_dummy
  end type get_wrf_binary_interface_class
contains

  subroutine convert_binary_mass_dummy(this)
    implicit none
    class(get_wrf_binary_interface_class), intent(inout) :: this
  end subroutine convert_binary_mass_dummy

  subroutine convert_binary_nmm_dummy(this,update_pint,ctph0,stph0,tlm0)
    use kinds, only: r_kind
    use constants, only: zero 
    implicit none
    class(get_wrf_binary_interface_class), intent(inout) :: this
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
    ctph0 = zero
    stph0 = zero
    tlm0 = zero
  end subroutine convert_binary_nmm_dummy

  subroutine convert_nems_nmmb_dummy(this,update_pint,ctph0,stph0,tlm0)
    use kinds, only: r_kind
    use constants, only: zero 
    implicit none
    class(get_wrf_binary_interface_class), intent(inout) :: this
    logical     ,intent(inout) :: update_pint
    real(r_kind),intent(  out) :: ctph0,stph0,tlm0
    ctph0 = zero
    stph0 = zero
    tlm0 = zero
  end subroutine convert_nems_nmmb_dummy

end module get_wrf_binary_interface_mod
