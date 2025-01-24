module gsi_fixture
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module gsi_fixture_REGIONAL (but named as gsi_fixture)
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2019-08-04
!
! abstract: - configure GSI extensions for a REGIONAL fixture.
!
! program history log:
!   2019-08-04  j guo   - initial code
!                       . a generic module name "gsi_fixture" is used to let
!                         the code compilable with a simple switch through
!                         CMakeLists.txt file selection.
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

! module interface:

  implicit none
  private	! except
  public:: fixture_config

        ! fixture_config() is the interface to all configuration extension
        ! details.  It is not implemented as a generic interface, to emphasize
        ! its exclusiveness.

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='gsi_fixture_REGIONAL'

contains
subroutine fixture_config()
!> In a REGIONAL fixture as it is,
!>      - use GSI stub_timer
!>      - if (use_gfs_ens) then
!>          use GSI get_gfs_ensmod_mod from cplr_gfs_ensmod.f90
!>        else
!>          use GSI stub_ensmod from stub_ensmod.f90
!>        endif

!> singleton timermod and gsi_enscouplemod, which manage the actual timer and
!> gfs_ensenble extentions.

  use timermod         , only: timer_typedef
  use gsi_enscouplermod, only: ensemble_typedef => gsi_enscoupler_registry

!> Define the actual extensions (timermod and gfs_ensemble) to be used.
  use hybrid_ensemble_parameters, only: use_gfs_ens
  use m_stubTimer       , only:      my_timer_mold =>    timer_typemold
  use stub_ensmod       , only: stub_ensemble_mold => ensemble_typemold
  use get_gfs_ensmod_mod, only:  gfs_ensemble_mold => ensemble_typemold

  implicit none

!> Fix up the extensions used by corresponding GSI singleton modules.

  call    timer_typedef(my_timer_mold())

  if(use_gfs_ens) then
    call ensemble_typedef( gfs_ensemble_mold())
  else
    call ensemble_typedef(stub_ensemble_mold())
  endif

end subroutine fixture_config
end module gsi_fixture
