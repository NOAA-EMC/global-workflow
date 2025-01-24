module abstract_get_pseudo_ensperts_mod
  type, abstract :: abstract_get_pseudo_ensperts_class
  integer, allocatable :: dummy(:)
  contains
    procedure(get_pseudo_ensperts), deferred, pass(this) :: get_pseudo_ensperts
  end type abstract_get_pseudo_ensperts_class
 
  abstract interface
  subroutine get_pseudo_ensperts(this,en_perts,nelen)
    use gsi_bundlemod, only: gsi_bundle
    use kinds, only: i_kind
    import abstract_get_pseudo_ensperts_class
    implicit none
    class(abstract_get_pseudo_ensperts_class), intent(inout) :: this
    type(gsi_bundle),allocatable, intent(in   ) :: en_perts(:,:,:)
    integer(i_kind),                   intent(in   ) :: nelen
  end subroutine get_pseudo_ensperts
  end interface

end module abstract_get_pseudo_ensperts_mod
