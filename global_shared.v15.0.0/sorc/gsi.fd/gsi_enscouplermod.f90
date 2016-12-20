!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_EnsCouplerMod ---
!
! !INTERFACE:

module GSI_EnsCouplerMod

! !USES:

use gsi_bundlemod, only: gsi_bundle
implicit none
private

!
! !PUBLIC MEMBER FUNCTIONS:
!
public GSI_EnsCoupler_localization_grid
public GSI_EnsCoupler_get_user_ens
public GSI_EnsCoupler_put_gsi_ens


interface gsi_enscoupler_localization_grid
   subroutine non_gaussian_ens_grid_ (elats,elons)
   use kinds, only: i_kind,r_kind
   use gridmod, only: rlats,rlons
   implicit none
   !  Declare passed variables
   real(r_kind),intent(out) :: elats(size(rlats)),elons(size(rlons)) ! worst hack ever
   end subroutine non_gaussian_ens_grid_
end interface

interface gsi_enscoupler_get_user_ens
   subroutine get_user_ens_(grd,member,ntindex,en_read,iret)
   use kinds, only: i_kind,r_kind
   use gsi_bundlemod, only: gsi_bundle
   use general_sub2grid_mod, only: sub2grid_info
   implicit none
   !  Declare passed variables
   type(sub2grid_info),intent(in   ) :: grd
   integer(i_kind)    ,intent(in   ) :: member  ! member index
   integer(i_kind)    ,intent(in   ) :: ntindex ! time index
   type(gsi_bundle)   ,intent(inout) :: en_read
   integer(i_kind)    ,intent(  out) :: iret
   end subroutine get_user_ens_
end interface

interface gsi_enscoupler_put_gsi_ens
   subroutine put_gsi_ens_(grd,member,ntindex,en_write,iret)
   use kinds, only: i_kind,r_kind
   use general_sub2grid_mod, only: sub2grid_info
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   !  Declare passed variables
   type(sub2grid_info),intent(in   ) :: grd
   integer(i_kind),    intent(in   ) :: member
   integer(i_kind),    intent(in   ) :: ntindex
   type(gsi_bundle),   intent(inout) :: en_write
   integer(i_kind),    intent(  out) :: iret
   end subroutine put_gsi_ens_
end interface



! !DESCRIPTION: This module provides general interface for
!               ensemble capability
!
! !REVISION HISTORY:
!
!  19Sep2011 Todling - Initial code
!  30Nov2014 Todling - Update interface to get (bundle passed in)
!
!EOP
!-------------------------------------------------------------------------
end module GSI_EnsCouplerMod
