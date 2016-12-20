!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  GSI_4dCouplerMod ---
!
! !INTERFACE:

module GSI_4dCouplerMod

! !USES:

use gsi_bundlemod, only: gsi_bundle
implicit none
private

!
! !PUBLIC MEMBER FUNCTIONS:
!
public GSI_4dCoupler_parallel_init
public GSI_4dCoupler_setServices
public GSI_4dCoupler_init_traj
public GSI_4dCoupler_init_model_tl
public GSI_4dCoupler_model_tl
public GSI_4dCoupler_final_model_tl
public GSI_4dCoupler_init_model_ad
public GSI_4dCoupler_model_ad
public GSI_4dCoupler_final_model_ad
public GSI_4dCoupler_grtests
public GSI_4dCoupler_getpert
public GSI_4dCoupler_putpert
public GSI_4dCoupler_final_traj

!
! !PUBLIC DATA: (for stub implementation only)
!

public:: idmodel_
logical,save:: idmodel_ = .true.

! !METHOD OVERLOADING:

interface GSI_4dCoupler_parallel_init
   subroutine parallel_init_ ()
   implicit none
   end subroutine parallel_init_
end interface

interface GSI_4dCoupler_setServices
   subroutine pertmod_setServices_ (rc)
   use kinds, only: i_kind
   implicit none
   integer(i_kind),optional,intent(out):: rc     ! return status code
   end subroutine pertmod_setServices_
end interface

interface GSI_4dCoupler_init_traj
   subroutine pertmod_initialize_ (idmodel,rc)
   use kinds, only: i_kind
   implicit none
   logical,optional,intent(in):: idmodel
   integer(i_kind),optional,intent(out):: rc	! return status code
   end subroutine pertmod_initialize_
end interface

interface GSI_4dCoupler_final_traj  
   subroutine pertmod_finalize_(rc)
   use kinds, only: i_kind
   implicit none
   integer(i_kind),optional,intent(out):: rc	! return status code
   end subroutine pertmod_finalize_
end interface

interface GSI_4dCoupler_init_model_tl
   subroutine pertmod_TLinit_(xini,xobs,iymd,ihms,ndtsec,rc)
   use kinds     , only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(in ):: xini	! a known state as a template
   type(gsi_bundle),intent(out):: xobs	! a state container to be defined as xini
   integer(i_kind ),intent(in ):: iymd	! initial date (YYYYMMDD) of the perturbation state
   integer(i_kind ),intent(in ):: ihms	! initial time (HHMMSS) of the perturbation state
   integer(i_kind ),intent(out):: ndtsec	! TL model time step in seconds
   integer(i_kind ),optional,intent(out):: rc	! return status code
   end subroutine pertmod_TLinit_
end interface

interface GSI_4dCoupler_model_tl
   subroutine pertmod_TLrun_(xini,xobs,iymd,ihms,ndt,rc)
   use kinds, only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),      pointer:: xini	! input: increment perturbation propagated by TLM
   type(gsi_bundle),intent(inout):: xobs	! inout: TL perturbation state
   integer(i_kind ),intent(in ):: iymd	! staring date (YYYYMMDD) of the perturbation state
   integer(i_kind ),intent(in ):: ihms	! staring time (HHMMSS) of the perturbation state
   integer(i_kind ),intent(in ):: ndt	! Number of time steps to integrate TLM for
   integer(i_kind ),optional,intent(out):: rc	! return status code
   end subroutine pertmod_TLrun_
end interface

interface GSI_4dCoupler_final_model_tl
   subroutine pertmod_TLfin_(xini,xobs,iymd,ihms,rc)
   use kinds, only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(in   ):: xini	! untouched perturbation increment
   type(gsi_bundle),intent(inout):: xobs	! destroyed perturbation state
   integer(i_kind ),intent(in   ):: iymd	! final date (YYYYMMDD) of the perturbation state
   integer(i_kind ),intent(in   ):: ihms	! final time (HHMMSS) of the perturbation state
   integer(i_kind ),optional,intent(out):: rc	! return status code
   end subroutine pertmod_TLfin_
end interface

interface GSI_4dCoupler_init_model_ad
   subroutine pertmod_ADinit_(xini,xobs,iymd,ihms,ndtsec,rc)
   use kinds     , only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(out):: xini	! a state container to be defined as xobs
   type(gsi_bundle),intent(in ):: xobs	! a known state as a template
   integer(i_kind ),intent(in ):: iymd	! initial date (YYYYMMDD) of the adjoint perturbation state
   integer(i_kind ),intent(in ):: ihms	! initial time (HHMMSS) of the adjoint perturbation state
   integer(i_kind ),intent(out):: ndtsec	! AD model time step in seconds
   integer(i_kind ),optional,intent(out):: rc	! return status code
   end subroutine pertmod_ADinit_
end interface

interface GSI_4dCoupler_model_ad
   subroutine pertmod_ADrun_(xini,xobs,iymd,ihms,ndt,rc)
   use kinds, only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(inout):: xini	! inout: adjoint increment perturbation
   type(gsi_bundle),      pointer:: xobs ! input: adjoint perturbation state
   integer(i_kind ),intent(in   ):: iymd	! starting date (YYYYMMDD) of the adjoint perturbation state
   integer(i_kind ),intent(in   ):: ihms	! starting time (HHMMSS) of the adjoint perturbation state
   integer(i_kind ),intent(in   ):: ndt	! Number of time steps to integrate TLM for
   integer(i_kind ),optional,intent(out):: rc	! return status code
   end subroutine pertmod_ADrun_
end interface

interface GSI_4dCoupler_final_model_ad
   subroutine pertmod_ADfin_(xini,xobs,iymd,ihms,rc)
   use kinds, only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(inout):: xini	! destroyed perturbation state
   type(gsi_bundle),intent(in   ):: xobs	! untouched perturbation increment
   integer(i_kind ),intent(in   ):: iymd	! final date (YYYYMMDD) of the adjoint perturbation state
   integer(i_kind ),intent(in   ):: ihms	! final time (HHMMSS) of the adjoint perturbation state
   integer(i_kind ),optional,intent(out):: rc	! return status code
   end subroutine pertmod_ADfin_
end interface

interface GSI_4dCoupler_grtests
   subroutine grtests_ (mval,sval,nsubwin,nobs_bins)
   use kinds,only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   integer(i_kind),intent(in) :: nsubwin,nobs_bins
   type(gsi_bundle),intent(inout) :: mval(nsubwin)
   type(gsi_bundle),intent(inout) :: sval(nobs_bins)
   ! user-specific gradient tests related to TL and AD models
   end subroutine grtests_
end interface

interface GSI_4dCoupler_getpert
   subroutine get_1pert_ (xx,what,filename)
   ! get perturbation from user's model and convert it to relevant gsi bundle
   use constants, only: zero
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(inout) :: xx
   character(len=*),intent(in) :: what       ! indicates whether tl or ad type perturbation
   character(len=*),intent(in) :: filename   ! filename containing pert - set to NULL when not applicable
   end subroutine get_1pert_
   !-------------------------!
   subroutine get_Npert_ (xx,n,what,filename)
   ! get perturbation from user's model and convert it to relevant gsi bundle
   use kinds,only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(inout) :: xx(n)
   integer(i_kind) ,intent(in) :: n
   character(len=*),intent(in) :: what        ! indicates whether tl or ad type perturbation
   character(len=*),intent(in) :: filename(n) ! n-pert filenames
   end subroutine get_Npert_
end interface

interface GSI_4dCoupler_putpert
   subroutine put_1pert_ (xx,nymd,nhms,what,label)
   ! convert xx to the user's model perturbation and write it out
   use kinds, only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(inout) :: xx     ! gsi perturbation (bundle) vector
   integer(i_kind), intent(in)    :: nymd   ! date to write out field, as in, YYYYMMDD
   integer(i_kind), intent(in)    :: nhms   ! time to write out field, as in, HHMMSS
   character(len=*),intent(in)    :: what   ! indicates whether tl or ad type perturbation
   character(len=*),intent(in)    :: label  ! label used to identify output filename
   end subroutine put_1pert_
   !-------------------------!
   subroutine put_Npert_ (xx,n,what)
   ! convert xx to the user's model perturbation and write it out
   use kinds,only: i_kind
   use gsi_bundlemod, only: gsi_bundle
   implicit none
   type(gsi_bundle),intent(inout) :: xx(n)     ! gsi perturbation (bundle) vector
   integer(i_kind) ,intent(in) :: n
   character(len=*),intent(in) :: what      ! indicates whether tl or ad type perturbation
   end subroutine put_Npert_
end interface

! !DESCRIPTION: This module intents to provide a general interface to the
!               necessary ingredients for 4dvar.
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling - Initial code
!  16Jun2010 Guo     - Separated interface module from its stub implementaion
!  31Aug2010 Guo     - Changed interfaces:
!			[]_init_model_tl(), []_model_tl()
!			[]_init_model_ad(), []_model_ad()
!		     - Added stub_internal_state module variables.
!  01Aug2011 Lueken  - Replaced F90 with f90 (no machine logic)
!
!EOP
!-------------------------------------------------------------------------
end module GSI_4dCouplerMod
