module timermod

!$$$ module documentation block
!           .      .    .                                       .
! module:   timermod
! prgmmr:   todling          org: gmao                date: 2007-10-01
!
! abstract: a module interfacing to user provided multi-timer extensions.
!
! program history log:
!   2007-10-01  todling
!   2009-02-26  todling - if-def from GMAO_FVGSI to GEOS_PERT
!   2009-08-13  lueken  - update documentation
!   2011-08-01  lueken  - replaced F90 with f90 (no machine logic)
!   2017-06-28  guo     - Extended to a polymorphic portal to multi-timer extensions.
!
!  subroutines included: see Fortran style document below.
!
!   input argument list: see Fortran style document below
!
!  output argument list: see Fortran style document below
!
! attributes:
!   language: Fortran 2003/2008 and/or above
!    machine:
!
!$$$ end module documentation block

! module interface:

  use m_abstractTimer, only: abstractTimer

  use mpeu_util, only: tell,warn
  use kinds    , only: i_kind
  implicit none
  private              ! except

!-------------------------------------------------------------------------------
! (1) This -timermod- is an implemention GSI multi-timer interfaces supporting
!     customized timers.  While the original implementation was just a
!     collection of declared implicit interfaces in FORTRAN, it is now a portal
!     of abstract multi-timer management to access other multi-timer extensions,
!     and a facade to multi-timer's type-bound-procedures.

!   .....................
!   (1a) For backward compatibility, all original interfaces are supported
!       unchanged as they were.

  public:: timer_ini    ! call timer_ini("proc-A")
  public:: timer_fnl    ! call timer_fnl("proc-A")
  public:: timer_pri    ! call timer_pri(6)

        interface timer_ini; module procedure tmon_ ; end interface
        interface timer_fnl; module procedure tmoff_; end interface
        interface timer_pri; module procedure flush_; end interface

!   .....................
!   Use-Case 1: three-point profiling -- the original use-case of "ini-fnl-pri"
!
!   | use timermod, only: timer_pri
!   | >>>
!   |     use timermod, only: timer_ini, timer_fnl
!   |     call timer_ini('setuprhsall')           ! set timer:"setuprhsall" on
!   |     >>>
!   |         use timermod, only: timer_ini, timer_fnl
!   |         call timer_ini('setupoz')           ! set timer:"setupoz" on
!   |         call timer_fnl('setupoz')           ! set timer:"setupoz" off
!   |     <<<
!   |     call timer_fnl('setuprhsall')           ! set timer:"setuprhsall" off
!   | <<<
!   | if(myPE==0) call timer_pri(6)               ! summarize all timers

!    .....................
!    (1b) A new set of interfaces are also provided, which is similar but with
!       different use-case in mind, which requires additional interfaces of
!       extension management and supports some refined use-case steps for user's
!       convinience.

  public:: timer_typedef        ! call timer_typedef([my_timer_mold])
                                ! - (re)types module variable -typemold_-.
  public:: timer_typename       ! timer_name=timer_typename()
                                ! - returns an allocatable char(*) for the
                                ! concrete type name of the user specified typemold_.

        interface timer_typedef ; module procedure typedef_ ; end interface
        interface timer_typename; module procedure typename_; end interface

  public:: timer_on             ! call timer_on ("proc-A")
  public:: timer_off            ! call timer_off("proc-A")
  public:: timer_reset          ! call timer_reset()
  public:: timer_flush          ! call timer_flush(lu=6)
  public:: timer_allflush       ! call timer_allflush(lu=6,root=0,comm=my_comm_world)

        interface timer_on      ; module procedure tmon_    ; end interface
        interface timer_off     ; module procedure tmoff_   ; end interface
        interface timer_reset   ; module procedure reset_   ; end interface
        interface timer_flush   ; module procedure flush_   ; end interface
        interface timer_allflush; module procedure allflush_; end interface

!   .....................
!   Use-Case 2: reduced parallel profiling, with distributed timers
!
!   | use timermod , only: timer_typedef, timer_typename
!   | use timermod , only: timer_flush, timer_allflush, timer_reset
!   | use m_myTimer, only: myTimer_typemold
!   |
!   | call timer_typedef(myTimer_typemold())      ! use myTimer as the multi-timer
!   | if(myPE==0) print*,'customized timer_typename =',timer_typename()
!   | >>>
!   |     use timermod, only: timer_on, timer_off
!   |     call timer_on ('setuprhsall')           ! set timer:"setuprhsall" on
!   |     >>>
!   |         use timermod, only: timer_on, timer_off
!   |         call timer_on ('setupoz')           ! set timer:"setupoz" on
!   |         call timer_off('setupoz')           ! set timer:"setupoz" off
!   |     <<<
!   |     call timer_off('setuprhsall')           ! set timer:"setuprhsall" off
!   | <<<
!   | if(myPE==0) call timer_flush(6)                     ! summarize timers on PE=0
!   | call timer_allflush(6,comm=MPI_comm_world,root=0)   ! reduce-summarize distributed timers
!   | call timer_reset()                                  ! reset all timers
!
!   Note the use of extension management through timer_typedef().


!-------------------------------------------------------------------------------
! (2) -timermod- manages a typemold_ variable to hold the user specified type
!     definition through []_typedef(), as well as a concrete multi-timer
!     variable this_timer_.  Variable thie_timer_ is allocated based on
!     typemold_, and initialized at the first timer_on() call, or at a
!     timer_reset() call. 

  class(abstractTimer),allocatable,target,save:: typemold_
  class(abstractTimer),allocatable,target,save:: this_timer_

!     Given the single instance nature of -timermod- as a common service, there
!     is no need to support an explicitly accessible multi-timer object to
!     high-level users.

!-------------------------------------------------------------------------------
  character(len=*),parameter:: myname="timermod"

  ! This flag controls internal debugging messages.
  logical,parameter:: verbose=.false.
  !logical,parameter:: verbose=.true.

contains
!-------------------------------------------------------------------------------
! Extension management routines

subroutine typedef_(mold)
!-- A high-level interface type-define the concrete multi-timer to use.

  use m_stubTimer, only: stubTimer => timer
  implicit none
  class(abstractTimer),optional,target,intent(in):: mold

  character(len=*),parameter:: myname_=myname//'::typedef_'
  class(abstractTimer),pointer:: pmold_

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
    call typemold_%reset()
    deallocate(typemold_)
  endif

        ! (re)allocate the new typemold_
  if(associated(pmold_)) then
    allocate(typemold_,mold=pmold_)
    pmold_ => null()

  else
    allocate(stubTimer::typemold_)
  endif
  if(verbose) call tell(myname_,'allocated, typemold_%mytype() = '//typemold_%mytype())
end subroutine typedef_

function typename_() result(name)
!-- Return the name of the current concrete multi-timer type.

  use m_abstractTimer, only: abstractTimer_typename
  implicit none
  character(len=:),allocatable:: name   ! return the type name
  name=abstractTimer_typename()
  if(allocated(typemold_)) name=typemold_%mytype()
        ! Note the use of typemold_, instead of this_timer_.
end function typename_

!-------------------------------------------------------------------------------
! Routines supporting timing activities
subroutine tmon_(name)
!-- Set a single named timer on
implicit none
  character(len=*),intent(in):: name    ! a timer name

  call ifn_alloc_()     ! to ensure an allocated(this_timer_)
  call this_timer_%on(name)
end subroutine tmon_

subroutine tmoff_(name)
!-- set a single named timer off
  implicit none
  character(len=*),intent(in):: name    ! a timer name

  call ifn_alloc_()     ! to ensure an allocated(this_timer_)
  call this_timer_%off(name)
end subroutine tmoff_

subroutine ifn_alloc_()
!-- If-not-properly-allocated(this_timer_), do something
  implicit none
  class(abstractTimer),pointer:: pmold_

    ! First, check to make sure typemold_ is type-defined, at least to a
    ! default multi-timer type.
  pmold_ => typemold_
  if(.not.associated(pmold_)) call typedef_()
  pmold_ => null()

    ! Then, check and possibly instantiate this_timer_, which is must be
    ! typed the same as typemold_

  if(allocated(this_timer_)) then
    if(same_type_as(typemold_,this_timer_)) return      ! Everything seems good.

      ! Otherwise, this_timer_ must be re-intentiated with a different type.

    call this_timer_%reset()  ! before deallocate -this_timer-, empty it.
    deallocate(this_timer_)
  endif
  allocate(this_timer_,mold=typemold_)
end subroutine ifn_alloc_

subroutine reset_()
!-- Reset this_timer_ to its initialized state.
  implicit none
  call ifn_alloc_()     ! to ensure an allocated(this_timer_)
  call this_timer_%reset()
end subroutine reset_

subroutine flush_(lu)
!-- Make a summarized dump of the local multi-timer to unit=lu
  implicit none
  integer(kind=i_kind),intent(in):: lu         ! output unit

  call ifn_alloc_()     ! to ensure an allocated(this_timer_)
  call this_timer_%flush(lu)
end subroutine flush_

subroutine allflush_(lu,comm,root)
!-- Make a reduced summary of distributed multi-timers to unit=lu on root PE.
  use mpeu_mpif, only: my_comm => MPI_COMM_WORLD
  implicit none
  integer(kind=i_kind),         intent(in):: lu         ! output unit
  integer(kind=i_kind),optional,intent(in):: comm       ! communicator (MPI)
  integer(kind=i_kind),optional,intent(in):: root       ! root processor ID (MPI)

  integer(kind=i_kind):: comm_,root_

  comm_=my_comm; if(present(comm)) comm_=comm           ! default to MPI_COMM_WORLD
  root_=0      ; if(present(root)) root_=root           ! default to ROOT=0

  call ifn_alloc_()     ! to ensure an allocated(this_timer_)
  call this_timer_%allflush(lu,comm=comm_,root=root_)
end subroutine allflush_

end module timermod
