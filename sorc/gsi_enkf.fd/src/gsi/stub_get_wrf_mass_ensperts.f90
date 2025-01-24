!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_wrf_mass_ensperts  read wrf arw model ensemble members
!   prgmmr: mtong           org: np22                date: 2010-06-28
!
! abstract: dummy for read ensemble members from the wrf arw model in both
! binary and netcdf
!             format, for use with hybrid ensemble option. ensemble spread is
!             also
!             written out as a byproduct for diagnostic purposes.
!
!
! program history log:
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

module get_wrf_mass_ensperts_mod
use abstract_get_wrf_mass_ensperts_mod
  type, extends(abstract_get_wrf_mass_ensperts_class) :: get_wrf_mass_ensperts_class
  contains
    procedure, pass(this) :: get_wrf_mass_ensperts => get_wrf_mass_ensperts_dummy
    procedure, pass(this) :: ens_spread_dualres_regional => ens_spread_dualres_regional_dummy
!   procedure, pass(this) :: get_wrf_mass_ensperts_netcdf_dummy
  end type get_wrf_mass_ensperts_class

contains
  subroutine ens_spread_dualres_regional_dummy(this,mype,en_perts,nelen,en_bar)
    use kinds, only: i_kind
    use gsi_bundlemod, only: gsi_bundle
    implicit none
    class(get_wrf_mass_ensperts_class), intent(inout) :: this
    integer(i_kind),intent(in):: mype
    type(gsi_bundle),allocatable, intent(in   ) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
    type(gsi_bundle),OPTIONAL,intent(in):: en_bar
    write(6,*)'get_wrf_mass_ensperts:  ***WARNING*** dummy call ... does nothing!'
  return
  end subroutine ens_spread_dualres_regional_dummy

  subroutine get_wrf_mass_ensperts_dummy(this,en_perts,nelen,ps_bar)
    use kinds, only: i_kind,r_single
    use gsi_bundlemod, only: gsi_bundle
    implicit none
    class(get_wrf_mass_ensperts_class), intent(inout) :: this
    type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
    real(r_single),dimension(:,:,:),allocatable:: ps_bar
    write(6,*)'get_wrf_mass_ensperts:  ***WARNING*** dummy call ... does nothing!'
  return
  end subroutine get_wrf_mass_ensperts_dummy
end module get_wrf_mass_ensperts_mod
