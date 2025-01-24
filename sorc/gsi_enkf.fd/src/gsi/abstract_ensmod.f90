module abstract_ensmod
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    abstract_ensmod    handles abstract ensemble 
!   prgmmr: mpotts          org: emc/ncep            date: 2016-06-30
!
! abstract: Handle abstract ensemble (full fields and perturbations)
!
! program history log:
!   2016-07-20  mpotts   - introduced as class_gfs_ensmod.f90
!   2019-06-30  todling  - revised as abstract layer - no GFS referencing
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  implicit none
  private
  public :: abstractEnsemble
  public :: abstractEnsemble_typename

  interface abstractEnsemble_typename; module procedure typename_; end interface

  type, abstract :: abstractEnsemble
    private
    contains
      procedure(mytype               ), nopass, deferred :: mytype
      procedure(create_sub2grid_info ), nopass, deferred :: create_sub2grid_info
      procedure(destroy_sub2grid_info), nopass, deferred :: destroy_sub2grid_info
      procedure(get_user_ens         ),         deferred :: get_user_ens
      procedure(get_user_Nens        ),         deferred :: get_user_Nens
      procedure(put_user_ens         ),         deferred :: put_user_ens
      procedure(non_gaussian_ens_grid),         deferred :: non_gaussian_ens_grid
  end type abstractEnsemble

  abstract interface
    function mytype() result(type_)
      implicit none
      character(:),allocatable:: type_
    end function mytype
  end interface

  abstract interface
    subroutine create_sub2grid_info(s2gi,nsig,npe,s2gi_ref)
      use kinds, only: i_kind
      use general_sub2grid_mod, only: sub2grid_info
      import abstractEnsemble
      implicit none
      type(sub2grid_info), intent(out  ) :: s2gi
      integer(i_kind    ), intent(in   ) :: nsig
      integer(i_kind    ), intent(in   ) :: npe
      type(sub2grid_info), intent(in   ) :: s2gi_ref
    end subroutine create_sub2grid_info
  end interface

  abstract interface
    subroutine destroy_sub2grid_info(s2gi)
      use general_sub2grid_mod, only: sub2grid_info
      import abstractEnsemble
      implicit none
      type(sub2grid_info), intent(inout) :: s2gi
    end subroutine destroy_sub2grid_info
  end interface

  abstract interface
    subroutine get_user_ens(this,grd,member,ntindex,atm_bundle,iret)
      use kinds, only: i_kind
      use general_sub2grid_mod, only: sub2grid_info
      use gsi_bundlemod, only: gsi_bundle
      import abstractEnsemble
      implicit none
      class(abstractEnsemble), intent(inout) :: this
      type(sub2grid_info), intent(in   ) :: grd
      integer(i_kind),     intent(in   ) :: member
      integer(i_kind),     intent(in   ) :: ntindex
      type(gsi_bundle),    intent(inout) :: atm_bundle
      integer(i_kind),     intent(  out) :: iret
    end subroutine get_user_ens
  end interface

  abstract interface
    subroutine get_user_Nens(this,grd,members,ntindex,atm_bundle,iret)
      use kinds, only: i_kind
      use general_sub2grid_mod, only: sub2grid_info
      use gsi_bundlemod, only: gsi_bundle
      import abstractEnsemble
      implicit none
      class(abstractEnsemble), intent(inout) :: this
      type(sub2grid_info), intent(in   ) :: grd
      integer(i_kind),     intent(in   ) :: members
      integer(i_kind),     intent(in   ) :: ntindex
      type(gsi_bundle),    intent(inout) :: atm_bundle(:)
      integer(i_kind),     intent(  out) :: iret
    end subroutine get_user_Nens
  end interface

  abstract interface
    subroutine put_user_ens(this,grd,member,ntindex,pert,iret)
      use kinds, only: i_kind
      use gsi_bundlemod, only: gsi_bundle
      use general_sub2grid_mod, only: sub2grid_info
      import abstractEnsemble
      implicit none
      class(abstractEnsemble), intent(inout) :: this
      type(sub2grid_info), intent(in   ) :: grd
      integer(i_kind),     intent(in   ) :: member
      integer(i_kind),     intent(in   ) :: ntindex
      type(gsi_bundle),    intent(inout) :: pert
      integer(i_kind),     intent(  out) :: iret
    end subroutine put_user_ens
  end interface

  abstract interface
    subroutine non_gaussian_ens_grid(this,elats,elons)
      use kinds, only: r_kind
      import abstractEnsemble
      implicit none
      class(abstractEnsemble), intent(inout) :: this
      real(r_kind), intent(out) :: elats(:),elons(:)
    end subroutine non_gaussian_ens_grid
  end interface

contains

function typename_() result(typename)
!-- Return the type name.
  implicit none
  character(len=:),allocatable:: typename
  typename="[abstractEnsemble]"
end function typename_

end module abstract_ensmod
