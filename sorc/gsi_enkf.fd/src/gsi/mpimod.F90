!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !MODULE:  mpimod --- GSI Module containing mpi related variables
!
! !INTERFACE:
!

module mpimod

! !USES:

  use kinds, only: i_kind

#ifdef ibm_sp
! Include standard mpi includes file.
  use mpi
#else
  use mpeu_mpif, only : mpi_rtype4 => mpi_real4
#ifdef _REAL4_
  use mpeu_mpif, only : mpi_rtype => mpi_real4
#else
  use mpeu_mpif, only : mpi_rtype => mpi_real8
#endif
  use mpeu_mpif, only : mpi_itype => mpi_integer4
  use mpeu_mpif, only : mpi_real8
  use mpeu_mpif, only : mpi_real16
  use mpeu_mpif, only : mpi_status_size
  use mpeu_mpif, only : mpi_sum
  use mpeu_mpif, only : mpi_integer
  use mpeu_mpif, only : mpi_integer1
  use mpeu_mpif, only : mpi_integer2
  use mpeu_mpif, only : mpi_integer4
  use mpeu_mpif, only : mpi_integer8
  use mpeu_mpif, only : mpi_real4
  use mpeu_mpif, only : mpi_max
  use mpeu_mpif, only : mpi_min
  use mpeu_mpif, only : mpi_character
  use mpeu_mpif, only : mpi_offset_kind
  use mpeu_mpif, only : mpi_info_null
  use mpeu_mpif, only : mpi_mode_rdonly
  use mpeu_mpif, only : mpi_mode_create
  use mpeu_mpif, only : mpi_mode_wronly
  use mpeu_mpif, only : mpi_mode_rdwr
  use mpeu_mpif, only : mpi_byte
#ifndef HAVE_ESMF
  use mpeu_mpif, only : mpi_comm_world
#endif /* HAVE_ESMF */
#endif

  implicit none

!
! !DESCRIPTION: module containing mpi related variables
!
! !REVISION HISTORY:
!
!   2003-09-30  kleist
!   2004-05-18  kleist, new variables and documentation
!   2004-06-10  todling, explicitly declated var from m_mpif
!   2004-07-15  todling, protex-compliant prologue
!   2004-07-23  treadon - add routine strip_periodic
!   2005-01-24  kleist - fix bug in array initialization
!   2005-02-15  todling - add use m_mpif, only for mpi_integer4,
!                         mpi_offset_kind, ... (only applies
!                         to non IBM SP machines)    
!   2005-07-25  todling - add a couple more exports from m_mpif
!                         (only applies to non IBM SP machines)
!   2006-04-06  middlecoff - remove mpi_request_null since not used
!   2006-06-20  treadon - add mpi_itype
!   2006-06-28  da Silva - Added 2 integers represing a layout: nxPE and nyPE.
!   2009-02-19  jing guo - replaced m_mpif of GMAO_mpeu with gmaogsi_mpif.
!   2009-04-21  derber - add communications for strong balance constraint (bal)
!                        and unified uv (vec) transformation
!   2010-04-01  treadon - remove routines reorder, reorder2, strip_single, strip,
!                         vectosub, reload, and strip_periodic from mpimod - these
!                         routines are now found in gridmod
!   2010-05-23  todling - nvarbal_id no longer wired to 1,2,3,4, rather linked
!                         to where fields are in control vector
!   2011-07-04  todling - allow proper setting of REAL*4 or REAL*8
!   2012-06-12  parrish - remove all communication variables, except for levs_id, nvar_id, and nvar_pe.
!                         Remove subroutines init_mpi_vars and destroy_mpi_vars.
!                         All communication variables that used to be here are now created in
!                         general_commvars_mod.f90 using calls to general_sub2grid_create_info.
!   2012-06-12  parrish - remove use of integer constants izero, ione.
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm RS/6000 SP, SGI Origin 2000; Compaq HP
!
! !AUTHOR: 
!    kleist           org: np20                date: 2003-09-30 
!
!EOP
!-------------------------------------------------------------------------

! set default to private
  private
! set subroutines to public
  public :: setcomm
! set passed variables to public
  public :: ierror,mpi_comm_world,npe,mpi_rtype,mpi_sum,mype,mpi_max,mpi_itype
  public :: mpi_real4,mpi_integer4,levs_id,mpi_min,mpi_real8,mpi_real16,mpi_integer8
  public :: mpi_integer,mpi_integer1,mpi_integer2,nvar_id
  public :: mpi_status_size,mpi_rtype4,nvar_pe,nype,nxpe
  public :: mpi_mode_rdonly,mpi_info_null,mpi_offset_kind
  public :: mpi_mode_rdwr,mpi_byte
  public :: mpi_mode_create
  public :: mpi_mode_wronly
  public :: mpi_character

#ifdef HAVE_ESMF
  integer(i_kind) :: mpi_comm_world
#endif

#ifdef ibm_sp
! Define size for mpi_real
  integer(i_kind), parameter :: mpi_rtype=mpi_real8
  integer(i_kind), parameter :: mpi_rtype4=mpi_real4
! integer(i_kind), parameter :: mpi_rtype=mpi_real4
  integer(i_kind), parameter :: mpi_itype=mpi_integer4
#endif

  integer(i_kind) ierror
  integer(i_kind) :: npe         ! total num of MPI tasks
  integer(i_kind) :: mype        ! number of MPI task

! Optional ESMF-like layout information: nxPE is the number of
! processors used to decompose the longitudinal dimensional, while nyPE 
! the number of processors used to decompose the latitudinal dimension.
! By construction, nPE = nxPE * nyPE.
! 
  integer(i_kind) :: nxpe=-1     ! optional layout information
  integer(i_kind) :: nype=-1     ! optional layout information


! communication arrays...set up in init_mpi_vars  (almost none left)

  integer(i_kind),allocatable,dimension(:):: levs_id ! vert lev id for each level 
                                             !  of the nsig1o slabs (zero if
                                             !  empty, else can vary between 1-->nsig)


  integer(i_kind),allocatable,dimension(:):: nvar_id ! variable id for each level 
                                             !   of the nsig1o slabs:
                                             !    1: streamfunction
                                             !    2: velocity potential
                                             !    3: surface pressure
                                             !    4: temperature
                                             !    5: q
                                             !    6: ozone
                                             !    7: sea surface temperature
                                             !    8: cloud water
                                             !    9: land skin temperature
                                             !   10: sfc ice temperature
  integer(i_kind),allocatable,dimension(:,:):: nvar_pe ! pe where each var is kept

contains

!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI        !
!-------------------------------------------------------------------------
!BOP
!
! !IROUTINE:  setcomm ---  set mpi communicator
!
! !INTERFACE:
!
  subroutine setcomm(iworld,iworld_group,nsize,members,ncomma,ierr)

! !USES:
    implicit none

! ! INPUT PARAMETERS:

    integer(i_kind)                 ,intent(inout) :: iworld_group
    integer(i_kind)                 ,intent(in   ) :: nsize
    integer(i_kind),dimension(nsize),intent(in   ) :: members

! ! OUTPUT PARAMETERS:

! ! INPUT/OUTPUT PARAMETERS:

    integer(i_kind)                 ,intent(inout) :: iworld,ncomma
    integer(i_kind)                 ,intent(inout) :: ierr

! !DESCRIPTION: set mpi communicator
!
! !REVISION HISTORY:
!
!   2004-07-23  treadon
!   2004-08-04  treadon - protex-compliant prologue
!
! !REMARKS:
!
!   language: f90
!   machine:  ibm rs/6000 sp; sgi origin 2000; compaq/hp
!
! !AUTHOR:
!    treadon           org: np20                date: 2004-07-23
!
!EOP
!-------------------------------------------------------------------------

    integer(i_kind) :: ncommva_group

    ncomma=mpi_comm_world
    iworld=mpi_comm_world
    call mpi_comm_group(iworld,iworld_group,ierr)

    call mpi_group_incl(iworld_group,nsize,members,ncommva_group,ierr)
    call mpi_comm_create(iworld,ncommva_group,ncomma,ierr)
    call mpi_group_free(ncommva_group,ierr)
    return
  end subroutine setcomm

end module mpimod


