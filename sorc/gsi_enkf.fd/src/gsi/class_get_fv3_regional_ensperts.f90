module abstract_get_fv3_regional_ensperts_mod
!$$$   module documentation block
!             .      .    .                                       .
! module:   abstract_get_fv3_regional_ensperts_mod 
!           first copied from class_get_wrf_nmm_ensperts.f90
!   prgmmr: Ting  , EMC/NCEP
!
! abstract: IO routines for regional FV3
!
! program history log:
!
! subroutines included:
!
! variable definitions:
!
! attributes:
!   langauge: f90
!    machine:
!
!$$$ end documentation block
  type, abstract :: abstract_get_fv3_regional_ensperts_class
  contains
    procedure(get_fv3_regional_ensperts), deferred, pass(this) :: get_fv3_regional_ensperts
  end type abstract_get_fv3_regional_ensperts_class

  abstract interface
  subroutine get_fv3_regional_ensperts(this,en_perts,nelen,ps_bar)
    use gsi_bundlemod, only: gsi_bundle
    use kinds, only: r_kind,i_kind,r_single
    
    import abstract_get_fv3_regional_ensperts_class
    implicit none
    class(abstract_get_fv3_regional_ensperts_class),intent(inout) :: this
    type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
    real(r_single),dimension(:,:,:),allocatable, intent(inout):: ps_bar

  end subroutine get_fv3_regional_ensperts
  end interface
  abstract interface

  subroutine convert_binary_fv3_regional_ens(this)
    import abstract_get_fv3_regional_ensperts_class
    implicit none
    class(abstract_get_fv3_regional_ensperts_class),intent(inout) :: this
  end subroutine convert_binary_fv3_regional_ens
  end interface

end module abstract_get_fv3_regional_ensperts_mod
