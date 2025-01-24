!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_EnsCouplerMod ---
!
! !INTERFACE:

module GSI_EnsCouplerMod

! !USES:

use abstract_ensmod, only: abstractEnsemble
use gsi_bundlemod, only: gsi_bundle
use mpeu_util, only: tell,warn
implicit none
private

! !PUBLIC MEMBER FUNCTIONS:

  public GSI_EnsCoupler_localization_grid
  public GSI_EnsCoupler_get_user_ens
  public GSI_EnsCoupler_get_user_Nens
  public GSI_EnsCoupler_put_gsi_ens
  public GSI_EnsCoupler_registry
  public GSI_EnsCoupler_name
  public GSI_EnsCoupler_create_sub2grid_info
  public GSI_EnsCoupler_destroy_sub2grid_info

! !INTERFACES:
  interface GSI_EnsCoupler_localization_grid;  module procedure non_gaussian_ens_grid_;  end interface
  interface GSI_EnsCoupler_get_user_ens;       module procedure get_user_ens_;           end interface
  interface GSI_EnsCoupler_get_user_Nens;      module procedure get_user_Nens_;          end interface
  interface GSI_EnsCoupler_put_gsi_ens;        module procedure put_user_ens_;           end interface

  interface GSI_EnsCoupler_registry ; module procedure typedef_ ; end interface
  interface GSI_EnsCoupler_name;      module procedure typename_; end interface
  interface GSI_EnsCoupler_create_sub2grid_info ; module procedure  create_s2gi; end interface
  interface GSI_EnsCoupler_destroy_sub2grid_info; module procedure destroy_s2gi; end interface

! !CLASSES:

  class(abstractEnsemble),allocatable,target,save:: typemold_
  class(abstractEnsemble),allocatable,target,save:: this_ensemble_

! This flag controls internal debugging messages.
   logical,parameter:: verbose=.false.
  !logical,parameter:: verbose=.true.

  character(len=*),parameter:: myname='GSI_EnsCouplerMod'
contains

subroutine typedef_(mold)
!-- A high-level interface type-define the concrete multi-ensemble to use.

  use stub_ensmod, only: stub_ensemble => ensemble
  implicit none
  class(abstractEnsemble),optional,target,intent(in):: mold

  character(len=*),parameter:: myname_=myname//'::typedef_'
  class(abstractEnsemble),pointer:: pmold_

        ! argument checking
  pmold_ => null()
  if(present(mold)) then
    pmold_ => mold
    if(.not.associated(pmold_)) &               ! is argument _mold_ a null-object?
      call warn(myname_,'a null argument (mold) is given.  Will typedef to default')
  endif

        ! reset current typemold
  if(allocated(typemold_)) then
    if(verbose) call tell(myname_,'deallocating, typemold_%mytype() = '//typemold_%mytype())
    deallocate(typemold_)
  endif

        ! (re)allocate the new typemold_
  if(associated(pmold_)) then
    allocate(typemold_,mold=pmold_)
    pmold_ => null()
  else
    allocate(stub_ensemble::typemold_)
  endif
  if(verbose) call tell(myname_,'allocated, typemold_%mytype() = '//typemold_%mytype())
end subroutine typedef_

function typename_() result(name)
!-- Return the name of the current concrete multi-ensemble type.

  use abstract_ensmod, only: abstractEnsemble_typename
  implicit none
  character(len=:),allocatable:: name   ! return the type name
  name=abstractEnsemble_typename()
  if(allocated(typemold_)) name=typemold_%mytype()
        ! Note the use of typemold_, instead of this_ensemble_.
end function typename_

   subroutine get_user_ens_(grd,member,ntindex,atm_bundle,iret)
   use kinds, only: i_kind,r_kind
   use gsi_bundlemod, only: gsi_bundle
   use general_sub2grid_mod, only: sub2grid_info
   implicit none
!  Declare passed variables
      type(sub2grid_info)                   ,intent(in   ) :: grd
      integer(i_kind)                       ,intent(in   ) :: member
      integer(i_kind)                       ,intent(in   ) :: ntindex
      type(gsi_bundle)                      ,intent(inout) :: atm_bundle
      integer(i_kind)                       ,intent(  out) :: iret
      call ifn_alloc_()     ! to ensure an allocated(this_ensemble_)
      call this_ensemble_%get_user_ens(grd,member,ntindex,atm_bundle,iret)
   end subroutine get_user_ens_

   subroutine get_user_Nens_(grd,members,ntindex,atm_bundle,iret)
   use kinds, only: i_kind,r_kind
   use gsi_bundlemod, only: gsi_bundle
   use general_sub2grid_mod, only: sub2grid_info
   implicit none
!  Declare passed variables
      type(sub2grid_info)                   ,intent(in   ) :: grd
      integer(i_kind)                       ,intent(in   ) :: members
      integer(i_kind)                       ,intent(in   ) :: ntindex
      type(gsi_bundle)                      ,intent(inout) :: atm_bundle(:)
      integer(i_kind)                       ,intent(  out) :: iret
      call ifn_alloc_()     ! to ensure an allocated(this_ensemble_)
      call this_ensemble_%get_user_Nens(grd,members,ntindex,atm_bundle,iret)
   end subroutine get_user_Nens_

   subroutine put_user_ens_(grd,member,ntindex,pert,iret)
   use kinds, only: i_kind,r_kind
   use general_sub2grid_mod, only: sub2grid_info
   use gsi_bundlemod, only: gsi_bundle
   implicit none
!  Declare passed variables
      type(sub2grid_info),intent(in   ) :: grd
      integer(i_kind),    intent(in   ) :: member
      integer(i_kind),    intent(in   ) :: ntindex
      type(gsi_bundle),   intent(inout) :: pert
      integer(i_kind),    intent(  out) :: iret
      call ifn_alloc_()     ! to ensure an allocated(this_ensemble_)
      call this_ensemble_%put_user_ens(grd,member,ntindex,pert,iret)
   end subroutine put_user_ens_

   subroutine non_gaussian_ens_grid_ (elats,elons)
   use kinds, only: i_kind,r_kind
   implicit none
   real(r_kind),intent(out) :: elats(:),elons(:)
   call ifn_alloc_()     ! to ensure an allocated(this_ensemble_)
   call this_ensemble_%non_gaussian_ens_grid(elats,elons)
   end subroutine non_gaussian_ens_grid_

   subroutine ifn_alloc_()
!-- If-not-properly-allocated(this_ensemble_), do something
   implicit none
   class(abstractEnsemble),pointer:: pmold_

    ! First, check to make sure typemold_ is type-defined, at least to a
    ! default multi-ensemble type.
   pmold_ => typemold_
   if(.not.associated(pmold_)) call typedef_()
   pmold_ => null()

    ! Then, check and possibly instantiate this_ensemble_, which is must be
    ! typed the same as typemold_

   if(allocated(this_ensemble_)) then
     if(same_type_as(typemold_,this_ensemble_)) return      ! Everything seems good.
 
       ! Otherwise, this_ensemble_ must be re-intentiated with a different type.
 
     deallocate(this_ensemble_)
   endif
   allocate(this_ensemble_,mold=typemold_)
  end subroutine ifn_alloc_

! !DESCRIPTION: This module provides general interface for
!               ensemble capability
!
! !REVISION HISTORY:
!
!  19Sep2011 Todling - Initial code
!  30Nov2014 Todling - Update interface to get (bundle passed in)
!  28Jun2018 Todling - Revamp in light of truly abstract ensemble interface
!
!EOP
!-------------------------------------------------------------------------

subroutine create_s2gi(s2gi, nsig, npe, s2gi_ref)
   use kinds, only: i_kind
   use general_sub2grid_mod, only: sub2grid_info
   implicit none
!  Declare passed variables
   type(sub2grid_info),intent(  out) :: s2gi
   integer(i_kind),    intent(in   ) :: nsig
   integer(i_kind),    intent(in   ) :: npe
   type(sub2grid_info),intent(in   ) :: s2gi_ref

   call ifn_alloc_()            ! to ensure an allocated(this_ensemble_)
   call this_ensemble_%create_sub2grid_info(s2gi, nsig,npe, s2gi_ref)
return
end subroutine create_s2gi
subroutine destroy_s2gi(s2gi)
   use general_sub2grid_mod, only: sub2grid_info
   implicit none
!  Declare passed variables
   type(sub2grid_info),intent(inout) :: s2gi

   call ifn_alloc_()            ! to ensure an allocated(this_ensemble_)
   call this_ensemble_%destroy_sub2grid_info(s2gi)
return
end subroutine destroy_s2gi

end module GSI_EnsCouplerMod
