module jcmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    jcmod           contains stuff for Jc penalty
!
!   prgmmr: kleist           org: np20                date: 2005-07-01
!
! abstract:  contains routines and variables for dynamic constraint 
!            term
!
! program history log:
!   2005-07-01
!   2005-09-29  kleist, expand to include variables for more terms
!   2005-11-21  kleist, remove tendency arrays
!   2006-04-06  kleist, expand and redefine Jc term for two formulations
!   2007-10-18  tremolet - added Jc DFI option
!   2012-02-08  kleist - add parameter ljc4tlevs 
!   2013-05-15  todling - add knobs to control constraint on total water
!
! subroutines included:
!   sub init_jcvars          - initialize Jc related variables
!
! Variable Definitions:
!   def ljcdfi               - if true, use digital filter Jc
!   def alphajc              - multiplying factor for Jc DFI
!   def ljc4tlevs            - if true, apply jc (limq, pdry) over all time levels if in 4D mode
!
!   The z_* arrays are used to accumulate information from previous outer loops regarding
!      congributions to the Jc term
!      Their definitions depend on formulation:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind
  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_jcvars
! set passed variables to public
  public :: ljcdfi,alphajc,wgtdfi,bamp_jcpdry,ljcpdry,eps_eer
  public :: ljc4tlevs

  logical ljcdfi,ljcpdry,ljc4tlevs
  real(r_kind) alphajc,bamp_jcpdry,eps_eer
  real(r_kind),allocatable :: wgtdfi(:)

contains

  subroutine init_jcvars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_jcvars     initial Jc related variables
!   prgmmr: kleist          org: np20                date: 2005-07-01
!
! abstract: initialize dynamic constraint term variables
!
! program history log:
!   2005-07-01  kleist
!   2005-09-29  kleist, expanded for new terms
!   2006-04-06  kleist, include both formulations
!   2010-09-06  todling, add Errico-Ehrendorfer parameter for E-norm
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
    use constants, only: zero,one
    implicit none

! load defaults for non-allocatable arrays
    ljcdfi=.false.
    ljcpdry=.false.
    ljc4tlevs=.false.
    alphajc=10.0_r_kind
    bamp_jcpdry=zero
    eps_eer=-one  ! default: don't use q-term in norm

    return
  end subroutine init_jcvars


end module jcmod
