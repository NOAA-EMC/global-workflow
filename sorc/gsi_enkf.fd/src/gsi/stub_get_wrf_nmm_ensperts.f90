module get_wrf_nmm_ensperts_mod
use abstract_get_wrf_nmm_ensperts_mod
  type, extends(abstract_get_wrf_nmm_ensperts_class) :: get_wrf_nmm_ensperts_class
  contains
    procedure, pass(this) :: get_wrf_nmm_ensperts => get_wrf_nmm_ensperts_dummy
    procedure, pass(this) :: convert_binary_nmm_ens => convert_binary_nmm_ens_dummy
  end type get_wrf_nmm_ensperts_class
contains
  subroutine get_wrf_nmm_ensperts_dummy(this,en_perts,nelen,region_lat_ens,region_lon_ens,ps_bar)

    use gsi_bundlemod, only: gsi_bundle
    use kinds, only: r_kind,i_kind,r_single
    
    implicit none
    class(get_wrf_nmm_ensperts_class), intent(inout) :: this
    type(gsi_bundle),allocatable, intent(inout) :: en_perts(:,:,:)
    integer(i_kind), intent(in   ):: nelen
    real(r_kind),allocatable, intent(inout):: region_lat_ens(:,:),region_lon_ens(:,:)
    real(r_single),dimension(:,:,:),allocatable, intent(inout):: ps_bar
  
    write(6,*)'GET_WRF_NMM_ENSPERTS:  ***WARNING*** dummy call ... does nothing!'
  return
  end subroutine get_wrf_nmm_ensperts_dummy
  
  subroutine convert_binary_nmm_ens_dummy(this)
  !$$$  subprogram documentation block
  !                .      .    .                                       .
  ! subprogram:    convert_binary_nmm_ens
  !   pgrmmr:
  !
  ! abstract: dummy call... does nothing
  !
  ! program history log:
  !   2012-02-27  parrish - added subprogram doc block
  !
  !   input argument list:
  !
  !   output argument list:
  !
  ! attributes:
  !   language: f90
  !   machine:
  !
  !$$$ end documentation block
    implicit none
    class(get_wrf_nmm_ensperts_class), intent(inout) :: this
  
    write(6,*)'CONVERT_BINARY_NMM_ENS:  ***WARNING*** dummy call ... does nothing!'
    return
  end subroutine convert_binary_nmm_ens_dummy
end module get_wrf_nmm_ensperts_mod
