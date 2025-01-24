module abstract_get_wrf_nmm_ensperts_mod
  type, abstract :: abstract_get_wrf_nmm_ensperts_class
  contains
    procedure(get_wrf_nmm_ensperts), deferred, pass(this) :: get_wrf_nmm_ensperts
    procedure(convert_binary_nmm_ens), deferred, pass(this) :: convert_binary_nmm_ens
  end type abstract_get_wrf_nmm_ensperts_class

  abstract interface
  subroutine get_wrf_nmm_ensperts(this,en_perts,nelen,region_lat_ens,region_lon_ens,ps_bar)
    use gsi_bundlemod, only: gsi_bundle
    use kinds, only: r_kind,i_kind,r_single
    
    import abstract_get_wrf_nmm_ensperts_class
    implicit none
    class(abstract_get_wrf_nmm_ensperts_class),intent(inout) :: this
    type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
    real(r_kind),allocatable, intent(inout):: region_lat_ens(:,:),region_lon_ens(:,:)
    real(r_single),dimension(:,:,:),allocatable, intent(inout):: ps_bar

  end subroutine get_wrf_nmm_ensperts
  end interface
  abstract interface

  subroutine convert_binary_nmm_ens(this)
    import abstract_get_wrf_nmm_ensperts_class
    implicit none
    class(abstract_get_wrf_nmm_ensperts_class),intent(inout) :: this
  end subroutine convert_binary_nmm_ens
  end interface

end module abstract_get_wrf_nmm_ensperts_mod
