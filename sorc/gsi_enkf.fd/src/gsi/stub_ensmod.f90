module stub_ensmod
!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_EnsCouplerMod ---
!
! !DESCRIPTION: This stub provides the default interfaces to read an 
!               ensemble in GSI.
!
! !REVISION HISTORY:
!
!  19Sep2011 Todling - Initial code
!  01Dec2011 Todling - Add put_gsi_ens to allow write out of internal members
!  30Nov2014 Todling - Update interface to get (bundle passed in)
!  30Jun2019 Todling - Revamp in light of abstract layer
!
! !REMARKS:
!    1. Unlike the previous version of this stub, the correct version should
!       always be loaded as part of the library - there is not need to remove
!       this from the library.
!
!EOP
!-------------------------------------------------------------------------

    use abstract_ensmod, only: abstractEnsemble
    implicit none
    private
    public :: ensemble
    public :: ensemble_typemold

    type, extends(abstractEnsemble) :: ensemble
      private
      contains
      procedure,nopass:: mytype
      procedure :: get_user_ens
      procedure :: get_user_Nens
      procedure,nopass:: create_sub2grid_info
      procedure,nopass:: destroy_sub2grid_info
      procedure :: put_user_ens
      procedure :: non_gaussian_ens_grid
    end type ensemble

    character(len=*),parameter:: myname  ="stub_ensmod"
    type(ensemble),target:: mold_

contains

  function ensemble_typemold() result(typemold)
!-- return a mold for this application
    implicit none
    type(ensemble),pointer:: typemold
    typemold => mold_
  end function ensemble_typemold

  function mytype()
    implicit none
    character(len=:), allocatable:: mytype
    mytype="["//myname//"::ensemble]"
  end function mytype

  subroutine get_user_ens(this,grd,member,ntindex,atm_bundle,iret)

     use kinds, only: i_kind
     use general_sub2grid_mod, only: sub2grid_info
     use gsi_bundlemod, only: gsi_bundle

     implicit none

     ! Declare passed variables
     class(ensemble),     intent(inout) :: this
     type(sub2grid_info), intent(in   ) :: grd
     integer(i_kind),     intent(in   ) :: member
     integer(i_kind),     intent(in   ) :: ntindex
     type(gsi_bundle),    intent(inout) :: atm_bundle
     integer(i_kind),     intent(  out) :: iret
!    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
!    end associate
     iret = 0
 
     return

  end subroutine get_user_ens
  
  subroutine get_user_Nens(this,grd,members,ntindex,atm_bundle,iret)
 
     use kinds, only: i_kind
     use general_sub2grid_mod, only: sub2grid_info
     use gsi_bundlemod, only: gsi_bundle
 
     implicit none
 
     ! Declare passed variables
     class(ensemble),     intent(inout) :: this
     type(sub2grid_info), intent(in   ) :: grd
     integer(i_kind),     intent(in   ) :: members
     integer(i_kind),     intent(in   ) :: ntindex
     type(gsi_bundle),    intent(inout) :: atm_bundle(:)
     integer(i_kind),     intent(  out) :: iret
!!   associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
!!   end associate
     iret = 0
 
     return
 
  end subroutine get_user_Nens

  subroutine create_sub2grid_info(s2gi,nsig,npe,s2gi_ref)
     use kinds, only: i_kind
     use general_sub2grid_mod, only: sub2grid_info
     implicit none
 
     ! Declare passed variables
     type(sub2grid_info), intent(out  ) :: s2gi
     integer(i_kind),     intent(in   ) :: nsig
     integer(i_kind),     intent(in   ) :: npe
     type(sub2grid_info), intent(in   ) :: s2gi_ref

     ! This is a simple copy
     s2gi = s2gi_ref
 
     return
  end subroutine create_sub2grid_info

  subroutine destroy_sub2grid_info(s2gi)
     use general_sub2grid_mod, only: sub2grid_info
     implicit none
 
     ! Declare passed variables
     type(sub2grid_info), intent(inout) :: s2gi

     ! Reset the variable to a different memory location, so any original target
     ! won't be accessed anymore.
 
     s2gi = sub2grid_info()
     return
  end subroutine destroy_sub2grid_info

  subroutine put_user_ens(this,grd,member,ntindex,pert,iret)

     use kinds, only: i_kind
     use general_sub2grid_mod, only: sub2grid_info
     use gsi_bundlemod, only: gsi_bundle

     implicit none
     ! Declare passed variables
     class(ensemble),     intent(inout) :: this
     type(sub2grid_info), intent(in   ) :: grd
     integer(i_kind),     intent(in   ) :: member
     integer(i_kind),     intent(in   ) :: ntindex
     type(gsi_bundle),    intent(inout) :: pert
     integer(i_kind),     intent(  out) :: iret

!    associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
!    end associate
     iret = 0
     return
  end subroutine put_user_ens

  subroutine non_gaussian_ens_grid(this,elats,elons)
 
     use kinds, only: r_kind
     use hybrid_ensemble_parameters, only: sp_ens
     implicit none
     ! Declare passed variables
     class(ensemble), intent(inout) :: this
     real(r_kind), intent(out) :: elats(:),elons(:)
!    real(r_kind), intent(out) :: elats(size(sp_ens%rlats)),elons(size(sp_ens%rlons))
 
     elats=sp_ens%rlats
     elons=sp_ens%rlons
!!   associate( this => this ) ! eliminates warning for unused dummy argument needed for binding
!!   end associate
 
     return
  end subroutine non_gaussian_ens_grid

end module stub_ensmod
