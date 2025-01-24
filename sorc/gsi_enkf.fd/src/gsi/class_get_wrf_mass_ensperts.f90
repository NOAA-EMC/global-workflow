module abstract_get_wrf_mass_ensperts_mod
  type, abstract :: abstract_get_wrf_mass_ensperts_class
  contains
    procedure(get_wrf_mass_ensperts), deferred, pass(this) :: get_wrf_mass_ensperts
    procedure(ens_spread_dualres_regional), deferred, pass(this) :: ens_spread_dualres_regional
  end type abstract_get_wrf_mass_ensperts_class
 
  abstract interface
  subroutine get_wrf_mass_ensperts(this,en_perts,nelen,ps_bar)
    use gsi_bundlemod, only: gsi_bundle
    use kinds, only: i_kind,r_single
    import abstract_get_wrf_mass_ensperts_class
    implicit none
    class(abstract_get_wrf_mass_ensperts_class), intent(inout) :: this
    type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
    real(r_single),dimension(:,:,:),allocatable:: ps_bar
  end subroutine get_wrf_mass_ensperts
  end interface
  abstract interface
  subroutine ens_spread_dualres_regional(this,mype,en_perts,nelen,en_bar)
    use kinds, only: r_single,r_kind,i_kind
    use gsi_bundlemod, only: gsi_bundle
    import abstract_get_wrf_mass_ensperts_class
    implicit none
    class(abstract_get_wrf_mass_ensperts_class), intent(inout) :: this
    integer(i_kind),intent(in):: mype
    type(gsi_bundle),allocatable, intent(in   ) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
    type(gsi_bundle),OPTIONAL,intent(in):: en_bar
  end subroutine ens_spread_dualres_regional
  end interface


end module abstract_get_wrf_mass_ensperts_mod
