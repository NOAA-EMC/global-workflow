!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_pseudo_ensperts
!   prgmmr: mtong           org: np22                date: 2011-09-06
!
! abstract: read pseudo ensemble members from TC library, and interpolate to analysis
!           grid. Calculate pseudo ensemble perturbations and replace global ensemble
!           perturbations in vortex region (300 km from storm center)   
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
module get_pseudo_ensperts_mod
use abstract_get_pseudo_ensperts_mod
  type, extends(abstract_get_pseudo_ensperts_class) :: get_pseudo_ensperts_class
  contains
    procedure, pass(this) :: get_pseudo_ensperts => get_pseudo_ensperts_dummy
  end type get_pseudo_ensperts_class

contains

  subroutine get_pseudo_ensperts_dummy(this,en_perts,nelen)
    use gsi_bundlemod, only: gsi_bundle
    use kinds, only: i_kind
    implicit none
    class(get_pseudo_ensperts_class), intent(inout) :: this
    type(gsi_bundle),allocatable, intent(in   ) :: en_perts(:,:,:)
    integer(i_kind),                   intent(in   ) :: nelen
  
    write(6,*)'get_pseudo_ensperts:  ***WARNING*** dummy call ... does nothing!'
  
    return
  
  end subroutine get_pseudo_ensperts_dummy
end module get_pseudo_ensperts_mod
