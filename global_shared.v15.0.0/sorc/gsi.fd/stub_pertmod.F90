!----------------------------------------------------------------------------
!BOP
!  
! !MODULE:  stub_pertMod ---
!
! !DESCRIPTION: This module is an implicit interface implementation of
!		module gsi_4dCouplerMod.  Operations implemented here
!		support an identity perturbation model.
!
! !REVISION HISTORY:
!
!  27Apr2010 Todling - Initial code
!  27Apr2010 Guo     - Separated stub implementation from interface module.
!  31Aug2010 Guo     - Changed interfaces:
!			init_pertmod_tl(), pertmod_tl()
!			init_pertmod_ad(), pertmod_ad()
!		     - Use stub_internal_state module variables.
!		     - changed algorithms of pertmod_tl() and pertmod_ad()
!
!EOP
!-------------------------------------------------------------------------
#define MYNAME	"stub"
!#define VERBOSE
#include "mytrace.H"

subroutine parallel_init_()
!! Need an explaination why this call has to be made this way instead of
!! in aother explicit approach.
use kinds, only: i_kind
use mpeu_util, only: die
implicit none
integer(i_kind):: ierror
logical:: already_init_mpi
character(len=*),parameter:: myname_=MYNAME//'::parallel_init_'
  ierror=0
  call mpi_initialized(already_init_mpi,ierror)
  if(ierror/=0) call die(myname_,'mpi_initialized(), ierror =',ierror)
  if(.not.already_init_mpi) then
     call mpi_init(ierror)
     if(ierror/=0) call die(myname_,'mpi_init(), ierror =',ierror)
  endif
end subroutine parallel_init_

!------------------------------------------------------------------------------------
subroutine pertmod_setServices_(rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_setServices_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-11-05
!
! abstract: stub for pertmod component configuration
!
! program history log:
!   2010-11-05  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  implicit none
  integer(i_kind),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_setServices_'

_ENTRY_(myname_)
  if(present(rc)) rc=0
_EXIT_(myname_)
end subroutine pertmod_setServices_

!------------------------------------------------------------------------------------
subroutine pertmod_initialize_(idmodel,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_initialize_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: stub default identity pertmod initialization
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use mpeu_util, only: tell,perr,die
  use gsi_4dCouplerMod, only: idmodel_
  implicit none
  logical,optional,intent(in):: idmodel
  integer(i_kind),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_initialize_'

_ENTRY_(myname_)
  if(present(rc)) rc=0
  idmodel_=.true.
  if(present(idmodel)) idmodel_=idmodel
  if(.not.idmodel_) then
    call perr(myname_,'unexpected setting, idmodel =',idmodel_)
    if(.not.present(rc)) call die(myname_)
    rc=-huge(rc)
_EXIT_(myname_)
    return
  endif

_EXIT_(myname_)
end subroutine pertmod_initialize_

!------------------------------------------------------------------------------------
subroutine pertmod_finalize_(rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_finalize_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: stub default identity pertmod finalization
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use mpeu_util, only: tell,perr,die
  use gsi_4dCouplerMod, only: idmodel_
  implicit none
  integer(i_kind),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_finalize_'

_ENTRY_(myname_)
  if(present(rc)) rc=0
  if(.not.idmodel_) then
    call perr(myname_,'unexpected setting, idmodel =',idmodel_)
    if(.not.present(rc)) call die(myname_)
    rc=-huge(rc)
_EXIT_(myname_)
    return
  endif

  idmodel_=.true.
_EXIT_(myname_)
end subroutine pertmod_finalize_

!------------------------------------------------------------------------------------
subroutine pertmod_TLinit_(xini,xobs,iymd,ihms,ndtsec,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_TLinit_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: initialize a TLM integration process
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds     , only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundleCreate
  use gsi_bundlemod, only: assignment(=)
  use constants , only: ZERO
  use constants , only: R3600
  use gsi_4dCouplerMod, only: idmodel_
  use mpeu_util, only: tell,perr,die
  implicit none
  type(gsi_bundle),intent(in ):: xini	! a known state as a template
  type(gsi_bundle),intent(out):: xobs	! a state container to be defined as xini
  integer(i_kind ),intent(in ):: iymd	! initial date (YYYYMMDD) of the perturbation state
  integer(i_kind ),intent(in ):: ihms	! initial time (HHMMSS) of the perturbation state
  integer(i_kind ),intent(out):: ndtsec	! TL model time step in seconds
  integer(i_kind ),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_TLinit_'
  character(len=*),parameter :: xoname_='TLpertState'
  integer(i_kind):: ier

_ENTRY_(myname_)
#ifdef VERBOSE
  call tell(myname_,'at (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0
  if(.not.idmodel_) then
    call perr(myname_,'unexpected setting, idmodel =',idmodel_)
    if(.not.present(rc)) call die(myname_)
    rc=-huge(rc)
_EXIT_(myname_)
    return
  endif

  ndtsec=R3600
  call gsi_bundleCreate(xobs,xini,xoname_,ier)	! make xobs as xini
  	if(ier/=0) then
	  call perr(myname_,'gsi_bundleCreate("'//xoname_//'"), istatus =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
_EXIT_(myname_)
	  return
	endif
  xobs=ZERO
_EXIT_(myname_)
end subroutine pertmod_TLinit_

!------------------------------------------------------------------------------------
subroutine pertmod_TLrun_(xini,xobs,iymd,ihms,ndt,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_TLrun_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: One TL model integration step
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use gsi_4dCouplerMod, only: idmodel_
  use mpeu_util, only: tell,perr,die
  implicit none

  type(gsi_bundle),      pointer:: xini	! input: increment perturbation propagated by TLM
  type(gsi_bundle),intent(inout):: xobs	! inout: TL perturbation state
  integer(i_kind ),intent(in ):: iymd	! staring date (YYYYMMDD) of the perturbation state
  integer(i_kind ),intent(in ):: ihms	! staring time (HHMMSS) of the perturbation state
  integer(i_kind ),intent(in ):: ndt	! Number of time steps to integrate TLM for
  integer(i_kind ),optional,intent(out):: rc	! return status code

  	!! t := (nymdi,nhmsi); n:=ndt; xi:=xini; yo:=xobs
  	!! e(t) = A(t)*xi(t)
	!! z(t+n) = M(t+n,t)*[z(t)+e(t)]
	!! yo(t+n) = G(t+n)*z(t)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_TLrun_'
  integer(i_kind):: ier

_ENTRY_(myname_)
#ifdef VERBOSE
  call tell(myname_,'with (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0
  if(.not.idmodel_) then
    call perr(myname_,'unexpected setting, idmodel =',idmodel_)
    if(.not.present(rc)) call die(myname_)
    rc=-huge(rc)
_EXIT_(myname_)
    return
  endif
!! Nothing to be done here, is equivalent to an identity model.
_EXIT_(myname_)
end subroutine pertmod_TLrun_

!------------------------------------------------------------------------------------
subroutine pertmod_TLfin_(xini,xobs,iymd,ihms,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_TLfin_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: end of TL model process
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundleDestroy
  use gsi_4dCouplerMod, only: idmodel_
  use mpeu_util, only: tell,perr,die
  implicit none

  type(gsi_bundle),intent(in   ):: xini	! untouched perturbation increment
  type(gsi_bundle),intent(inout):: xobs	! destroyed perturbation state
  integer(i_kind ),intent(in   ):: iymd	! final date (YYYYMMDD) of the perturbation state
  integer(i_kind ),intent(in   ):: ihms	! final time (HHMMSS) of the perturbation state
  integer(i_kind ),optional,intent(out):: rc	! return status code
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter:: myname_=MYNAME//"::pertmod_TLfin_"
  integer(i_kind):: ier

_ENTRY_(myname_)
#ifdef VERBOSE
  call tell(myname_,'with (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0
  if(.not.idmodel_) then
    call perr(myname_,'unexpected setting, idmodel =',idmodel_)
    if(.not.present(rc)) call die(myname_)
    rc=-huge(rc)
_EXIT_(myname_)
    return
  endif

  call gsi_bundleDestroy(xobs,ier)
  	if(ier/=0) then
	  call perr(myname_,'gsi_bundleDestroy(), istatus =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=-huge(rc)
_EXIT_(myname_)
	  return
	endif

_EXIT_(myname_)
end subroutine pertmod_TLfin_

!------------------------------------------------------------------------------------
subroutine pertmod_ADinit_(xini,xobs,iymd,ihms,ndtsec,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_ADinit_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: initialize a TLM integration process
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds     , only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundleCreate
  use gsi_bundlemod, only: assignment(=)
  use constants , only: ZERO
  use constants , only: R3600
  use gsi_4dCouplerMod, only: idmodel_
  use mpeu_util, only: tell,perr,die
  implicit none
  type(gsi_bundle),intent(out):: xini	! a state container to be defined as xobs
  type(gsi_bundle),intent(in ):: xobs	! a known state as a template
  integer(i_kind ),intent(in ):: iymd	! initial date (YYYYMMDD) of the adjoint perturbation state
  integer(i_kind ),intent(in ):: ihms	! initial time (HHMMSS) of the adjoint perturbation state
  integer(i_kind ),intent(out):: ndtsec	! AD model time step in seconds
  integer(i_kind ),optional,intent(out):: rc	! return status code

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_ADinit_'
  character(len=*),parameter :: xoname_='ADpertState'
  integer(i_kind):: ier


_ENTRY_(myname_)
#ifdef VERBOSE
  call tell(myname_,'at (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0
  if(.not.idmodel_) then
    call perr(myname_,'unexpected setting, idmodel =',idmodel_)
    if(.not.present(rc)) call die(myname_)
    rc=-huge(rc)
_EXIT_(myname_)
    return
  endif

  ndtsec=R3600
  call gsi_bundleCreate(xini,xobs,xoname_,ier)
  	if(ier/=0) then
	  call perr(myname_,'gsi_bundleCreate("'//xoname_//'"), istatus =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=ier
_EXIT_(myname_)
	  return
	endif
  xini=ZERO
_EXIT_(myname_)
end subroutine pertmod_ADinit_

!------------------------------------------------------------------------------------
subroutine pertmod_ADrun_(xini,xobs,iymd,ihms,ndt,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_ADrun_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: One TL model integration step
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use gsi_4dCouplerMod, only: idmodel_
  use mpeu_util, only: tell,perr,die
  implicit none

  type(gsi_bundle),intent(inout):: xini	! inout: adjoint increment perturbation
  type(gsi_bundle),      pointer:: xobs ! input: adjoint perturbation state
  integer(i_kind ),intent(in   ):: iymd	! starting date (YYYYMMDD) of the adjoint perturbation state
  integer(i_kind ),intent(in   ):: ihms	! starting time (HHMMSS) of the adjoint perturbation state
  integer(i_kind ),intent(in   ):: ndt	! Number of time steps to integrate TLM for
  integer(i_kind ),optional,intent(out):: rc	! return status code

  	!! t := (nymdi,nhmsi); n:=ndt; xo:=xini; yi:=xobs
  	!! z(t+n) = G'(t+n)*yi(t+n)
	!! e(t) = M'(t+n,t)*[z(t+n)+e(t+n)]
	!! xo(t) = A'(t)*e(t)
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname_=MYNAME//'::pertmod_ADrun_'
  integer(i_kind):: ier

_ENTRY_(myname_)
#ifdef VERBOSE
  call tell(myname_,'with (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0
  if(.not.idmodel_) then
    call perr(myname_,'unexpected setting, idmodel =',idmodel_)
    if(.not.present(rc)) call die(myname_)
    rc=-huge(rc)
_EXIT_(myname_)
    return
  endif
!! Nothing to be done here, is equivalent to an identity model.
_EXIT_(myname_)
end subroutine pertmod_ADrun_

!------------------------------------------------------------------------------------
subroutine pertmod_ADfin_(xini,xobs,iymd,ihms,rc)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 subroutine pertmod_ADfin_
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 900.3
!     date:	 2010-10-28
!
! abstract: end of TL model process
!
! program history log:
!   2010-10-28  j guo   - added this document block
!
!   input argument list: see Fortran 90 style document below
!
!   output argument list: see Fortran 90 style document below
!
! attributes:
!   language: Fortran 90 and/or above
!   machine:
!
!$$$  end subprogram documentation block

! subroutine interface:

  use kinds, only: i_kind
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundleDestroy
  use gsi_4dCouplerMod, only: idmodel_
  use mpeu_util, only: tell,perr,die
  implicit none

  type(gsi_bundle),intent(inout):: xini	! destroyed perturbation state
  type(gsi_bundle),intent(in   ):: xobs	! untouched perturbation increment
  integer(i_kind ),intent(in   ):: iymd	! final date (YYYYMMDD) of the adjoint perturbation state
  integer(i_kind ),intent(in   ):: ihms	! final time (HHMMSS) of the adjoint perturbation state
  integer(i_kind ),optional,intent(out):: rc	! return status code
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter:: myname_=MYNAME//"::pertmod_ADfin_"
  integer(i_kind):: ier

_ENTRY_(myname_)
#ifdef VERBOSE
  call tell(myname_,'with (iymd,ihms) =',(/iymd,ihms/))
#endif

  if(present(rc)) rc=0
  if(.not.idmodel_) then
    call perr(myname_,'unexpected setting, idmodel =',idmodel_)
    if(.not.present(rc)) call die(myname_)
    rc=-huge(rc)
_EXIT_(myname_)
    return
  endif

  call gsi_bundleDestroy(xini,ier)
  	if(ier/=0) then
	  call perr(myname_,'gsi_bundleDestroy(), istatus =',ier)
	  if(.not.present(rc)) call die(myname_)
	  rc=-huge(rc)
_EXIT_(myname_)
	  return
	endif

_EXIT_(myname_)
end subroutine pertmod_ADfin_

!------------------------------------------------------------------------------------
subroutine grtests_ (mval,sval,nsubwin,nobs_bins)
use kinds,only: i_kind
use gsi_bundlemod, only: gsi_bundle
implicit none
integer(i_kind),intent(in) :: nsubwin,nobs_bins
type(gsi_bundle),intent(inout):: mval(nsubwin)
type(gsi_bundle),intent(inout):: sval(nobs_bins)
! user-specific gradient tests related to TL and AD models
end subroutine grtests_
!------------------------------------------------------------------------------------
subroutine get_1pert_ (xx,what,filename)
! get perturbation from user's model and convert it to relevant gsi bundle
use constants, only: zero
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
implicit none
type(gsi_bundle),intent(inout) :: xx
character(len=*),intent(in) :: what     ! indicates whether tl or ad type perturbation
character(len=*),intent(in) :: filename ! filename containing pert - set to NULL when n/a
xx=zero
end subroutine get_1pert_
!------------------------------------------------------------------------------------
subroutine put_1pert_ (xx,nymd,nhms,what,label)
! convert xx to the user's model perturbation and write it out
use kinds, only: i_kind
use gsi_bundlemod, only: gsi_bundle
implicit none
type(gsi_bundle),intent(inout) :: xx     ! gsi perturbation (bundle) vector
character(len=*),intent(in)    :: what   ! indicates whether tl or ad type perturbation
character(len=*),intent(in)    :: label  ! label used to identify output filename
integer(i_kind), intent(in)    :: nymd   ! date to write out field, as in, YYYYMMDD
integer(i_kind), intent(in)    :: nhms   ! time to write out field, as in, HHMMSS
end subroutine put_1pert_
!------------------------------------------------------------------------------------
subroutine get_Npert_ (xx,n,what,filename)
! get perturbation from user's model and convert it to relevant gsi bundle
use kinds,only: i_kind
use constants, only: zero
use gsi_bundlemod, only: gsi_bundle
use gsi_bundlemod, only: assignment(=)
implicit none
integer(i_kind) ,intent(in) :: n
type(gsi_bundle),intent(inout) :: xx(n)
character(len=*),intent(in) :: what         ! indicates whether tl or ad type perturbation
character(len=*),intent(in) :: filename(n)  ! arrays w/ filename for pert (NULL when n/a)
integer(i_kind) ii
do ii=1,n
   xx(ii)=zero
enddo
end subroutine get_Npert_
!------------------------------------------------------------------------------------
subroutine put_Npert_ (xx,n,what)
! convert xx to the user's model perturbation and write it out
use kinds,only: i_kind
use gsi_bundlemod, only: gsi_bundle
implicit none
integer(i_kind),intent(in) :: n
type(gsi_bundle),intent(in) :: xx(n)     ! gsi perturbation (bundle) vector
character(len=*),intent(in) :: what      ! indicates whether tl or ad type perturbation
end subroutine put_Npert_
!------------------------------------------------------------------------------------

