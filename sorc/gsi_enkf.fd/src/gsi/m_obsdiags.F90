module m_obsdiags
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_obsdiags
!   prgmmr:      j guo <jguo@nasa.gov>
!      org:      NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:      2015-02-04
!
! abstract: a bundle of GSI multiple obstypes and the obsdiags linked-lists
!
! program history log:
!   2015-02-04  j guo   - Re-implemented read_obsdiags() and write_obsdiags(),
!                         to support reconfigurable observation operators.  This
!                         implemenstation uses an obsLList template to support,
!                         in ceterian degree, static polymoprhism for different
!                         observation types.
!   2015-10-09  j guo   - By using Fortran 2003 dynamic polymorphism, this
!                         version has removed many ugly type dispatching SELECT
!                         CASE constructs, by using an obsLList, a linked-list
!                         of dynamic polymorphic observation type (obsNode),
!                         which replaced the earlier obsLList template.
!   2016-06-22  j guo   - Added latlonRange for selected file readings, to let
!                         []_mread() to skip unnecessary read() of some files
!                         containing no relevant observations.
!                       . Added obsdiags_alwaysLocal, as a user controlable
!                         switch to allow or to bypass selected file readings.
!                       . Added CHECK_SIZES_ outputs to allow size checkings.
!                       . Added #define SHOW_LLRANGE, for text-dumping of latlonRanges.
!                       . Added #define DEBUG_obsdiags, for text-dumping
!                         specific sections of obsdiags(:,:).
!                       . Locally renamed MPI_comm_world to gsi_comm_world.
!   2018-01-23  k apodaca - Add a new observation type i.e. lightning (light) 
!                           suitable for the GOES/GLM instrument
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

  use kinds, only: i_kind, r_kind
  use mpeu_util, only: tell,warn,perr,die
  use mpeu_util, only: assert_
  use mpeu_util, only: stdout_open,stdout_close,stdout
  use mpeu_mpif, only: gsi_comm_world => MPI_comm_world

  use gsi_obOper, only: obOper

  use m_obsLList, only: obsLList
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags

  use gsi_obOperTypeManager, only: nobs_type => obOper_count
  use gsi_4dvar            , only: nobs_bins

  !use obsmod, only: obsdiags     ! (nobs_type,nobs_bins)
  implicit none
  private       ! except

  public:: obOpers_config
        interface obOpers_config; module procedure config_; end interface

        ! obOper instance creater with initialization to objects corresponding
        ! linked-list data instances.
  public:: obOper_create
  public:: obOper_headNode
  public:: obOper_destroy
        interface obOper_create; module procedure &
          createbydtype_, &
          createbyindex_, &
          createbyvmold_
        end interface
        interface obOper_headNode; module procedure headnode_; end interface
        interface obOper_destroy ; module procedure destroy_ ; end interface

  public:: obsdiags_reset
  public:: obsdiags_write
  public:: obsdiags_read
  public:: obsdiags_sort

        interface obsdiags_reset; module procedure reset_; end interface
        interface obsdiags_write; module procedure write_; end interface
        interface obsdiags_read ; module procedure mread_; end interface
        interface obsdiags_sort ; module procedure lsort_; end interface

  public:: obsdiags_create
  public:: obsdiags_destroy
  public:: obsdiags_inquire
        interface obsdiags_create ; module procedure  create_obsmod_vars; end interface
        interface obsdiags_destroy; module procedure destroy_obsmod_vars; end interface
        interface obsdiags_inquire; module procedure inquire_obsdiags   ; end interface

  public:: obsdiags_summary

        interface obsdiags_summary ; module procedure summary_ ; end interface

  public:: obsdiags_alwaysLocal
        logical,save:: obsdiags_alwaysLocal = .false.

! Note: User configurables
!
!   (1) obsdiags_mread(..,mPEs,..) via /SETUP/:mPEs_observer:
!
!       mPEs==0, for reading "my own data";
!       mPEs=>0, reading "all data", from PE 0 to mPEs-1, but only up to the
!                highest count of the actually accessible files.
!
!       This value is configured through gsimod namelist/SETUP/:mPEs_observer,
!       with the default value set to 0, to behave as it was ("my own data").
!       Otherwise, a simple usage is to let mPEs_observer=1024, or other large
!       enough value, such that the solver-mode will try to determine how many
!       files created by the observer-mode are actually there to read.
!
!   (2) obsdiags_alwaysLocal via /SETUP/:alwaysLocal:
!       
!       obsdiags_alwaysLocal sets an alternative default value of the optional
!       argument of obsdiags_mread(..,alwaysLocal).
!
!       obsdiags_alwaysLocal==.false., its default value.
!               It let obsdiags_mread() to check the locality of a file first,
!               using latlonRange_islocal(iPE), to avoid unnecessary opening+
!               reading some files.
!       obsdiags_alwaysLocal==.true., override latlonRange_islocal(iPE).
!               It let obsdiags_mread() to always open+read all file.

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='m_obsdiags'
  logical,save:: lobsdiags_allocated_ = .false.
  logical,save:: lobstypes_allocated_ = .false.

  logical,parameter:: All_PEs =.false.  ! report status on all PEs or root only
  !logical,parameter:: All_PEs =.true.  ! report status on all PEs or root only
  logical,parameter:: DO_SUMMARY =.false.  ! report status on all PEs or root only
  !logical,parameter:: DO_SUMMARY =.true.  ! report status on all PEs or root only

        ! SYNCH_MESSAGES is a flag to invoke MPI_Barrier() calls before some
        ! status messages.  These calls are otherwise not necessary for the
        ! functionalities, but used here to ensure those messages mean what they
        ! intent to mean, in case that only the root PE is used to report some
        ! all PE status.

  !logical,parameter:: SYNCH_MESSAGES = .true.          ! turn synch on
  !logical,parameter:: SYNCH_MESSAGES = .not. All_PEs   ! conditionally turn on
  logical,parameter:: SYNCH_MESSAGES = .false.          ! turn synch off

  public :: obsdiags
  public :: obsLLists

  type(obsLList ),save,dimension(:,:),pointer :: obsLLists => null()
  type(obs_diags),save,dimension(:,:),pointer :: obsdiags  => null()  ! (nobs_type,nobs_bins)
  
  integer(i_kind),save:: jfunc__jiter = -1
  integer(i_kind),save:: jfunc__miter = -1
  integer(i_kind),save:: jfunc__jiterstart = -1

  integer(i_kind),save:: gsi_4dvar__nobs_bins  = -1
  integer(i_kind),save:: gsi_4dvar__min_offset = -1
  real   (r_kind),save:: gsi_4dvar__hr_obsbin  = -999._r_kind

!#define DEBUG_TRACE
!#define DEBUG_VERBOSE
#include "mytrace.H"
#include "myassert.H"

#define _TIMER_ON_
#ifdef  _TIMER_ON_
#undef  _TIMER_ON_
#undef  _TIMER_OFF_
#undef  _TIMER_USE_
#define _TIMER_ON_(id)  call timer_ini(id)
#define _TIMER_OFF_(id) call timer_fnl(id)
#define _TIMER_USE_     use timermod, only: timer_ini,timer_fnl
#else
#define _TIMER_ON_(id)
#define _TIMER_OFF_(id)
#define _TIMER_USE_
#endif

  logical,parameter:: CHECK_SIZES_=.false.
  !logical,parameter:: CHECK_SIZES_=.true.

  !-- if(CHECK_SIZES_) then 
  !--   these size counters,

  integer(i_kind),allocatable,dimension(:),save:: lsize_type    !  luse counts of ob_type
  integer(i_kind),allocatable,dimension(:),save:: nsize_type    ! total counts of ob_type
  integer(i_kind),allocatable,dimension(:),save:: lsize_diag    !  luse counts of obs_diags
  integer(i_kind),allocatable,dimension(:),save:: msize_diag    !  muse counts of obs_diags
  integer(i_kind),allocatable,dimension(:),save:: nsize_diag    ! total counts of obs_diags

  !--   will be used to generate extra log-information, reporting different
  !--   size-counts of linked-lists, of all j-type, i-bin, on all PEs.  Search
  !--   "CHECK_SIZES_" here for details.
  !-- endif

contains

subroutine config_()
!> Coupling external configurations (through external modules) to obOpers' own
!> module configurations
  implicit none

!> For all obOpers, import external configurations
  call     jfunc__import_()
  call gsi_4dvar__import_()

!> For specific obOpers, import specific configurations
  call  lwcpOper__config_()

return
contains
subroutine jfunc__import_()
  !> jfunc parameters imported
    use jfunc, only: jiter
    use jfunc, only: miter
    use jfunc, only: jiterstart
    implicit none
    jfunc__jiter = jiter
    jfunc__miter = miter
    jfunc__jiterstart = jiterstart
  return
  end subroutine jfunc__import_
subroutine gsi_4dvar__import_()
  !> gsi4dvar parameters imported
    use gsi_4dvar, only: nobs_bins
    use gsi_4dvar, only: min_offset
    use gsi_4dvar, only: hr_obsbin
    implicit none
    gsi_4dvar__nobs_bins  = nobs_bins
    gsi_4dvar__min_offset = min_offset
    gsi_4dvar__hr_obsbin  = hr_obsbin
  return
  end subroutine gsi_4dvar__import_
subroutine lwcpOper__config_()
  !> gsi_lwcpOper parameters for configuration
  !> gfs_stratosphere imports
    use gfs_stratosphere, only: use_gfs_stratosphere
    use gfs_stratosphere, only: nsig_save
  !> lwcpOper
    use gsi_lwcpOper    , only: lwcpOper_config
    implicit none
  
    call lwcpOper_config()   ! reset to default
  !> From gfs_stratosphere to gsi_lwcpOper, and expected to be refactored into an attribute of profile-vectors objects)
    if(use_gfs_stratosphere) call lwcpOper_config(nsig_save=nsig_save)
  return
  end subroutine lwcpOper__config_
end subroutine config_

function createbydtype_(dtype) result(self)
!>> create an obOper to its components instanciated in this data module, with
!>> a given obOper registered dtype
  use gsi_obOperTypeManager, only: obOper_typeMold     ! (dtype)
  implicit none
  class(obOper),pointer:: self
  character(len=*),intent(in):: dtype
  character(len=*),parameter:: myname_=myname//"::createbydtype_"

  self => createbyvmold_(obOper_typeMold(dtype))

#ifdef DEBUG_VERBOSE
! show status of the object for debugging
  call tell(myname_,'--- argument dtype =',trim(dtype))
  call tell(myname_,'associated(return) =',associated(self))
  !if(associated(self)) call obOper_show_(myname_,self)
#endif
end function createbydtype_

function createbyindex_(ioper) result(self)
!>> create an obOper to its components instanciated in this data module, with
!>> a given obOper registered index.
  use gsi_obOperTypeManager, only: obOper_typeMold     ! (ioper)
  use gsi_obOperTypeManager, only: obOper_lbound
  use gsi_obOperTypeManager, only: obOper_ubound
  implicit none
  class(obOper),pointer:: self
  integer(kind=i_kind),intent(in):: ioper

  character(len=*),parameter:: myname_=myname//"::createbyindex_"
  class(obOper),pointer:: mold_

  mold_ => obOper_typeMold(ioper)
  if(associated(mold_)) then
    allocate(self,mold=mold_)

        if(ioper<lbound(obsLLists,1) .or. ioper>ubound(obsLLists,1)) then
          call perr(myname_,'unexpected value, ioper =',ioper)
          call perr(myname_,'    lbound(obsLLists,1) =',lbound(obsLLists,1))
          call perr(myname_,'    ubound(obsLLists,1) =',ubound(obsLLists,1))
          call perr(myname_,'              %mytype() =',self%mytype())
          call perr(myname_,'      %mytype(nodetype) =',self%mytype(nodetype=.true.))
          call  die(myname_)
        endif
        if(ioper<lbound( obsdiags,1) .or. ioper>ubound( obsdiags,1)) then
          call perr(myname_,'unexpected value, ioper =',ioper)
          call perr(myname_,'    lbound( obsdiags,1) =',lbound( obsdiags,1))
          call perr(myname_,'    ubound( obsdiags,1) =',ubound( obsdiags,1))
          call perr(myname_,'              %mytype() =',self%mytype())
          call perr(myname_,'      %mytype(nodetype) =',self%mytype(nodetype=.true.))
          call  die(myname_)
        endif

    call self%init(obsLLists(ioper,:), &
                    obsdiags(ioper,:)  )
    mold_ => null()

  else
        call perr(myname_,'.not.associated, ioper =',ioper)
        call  die(myname_)
  endif

#ifdef DEBUG_VERBOSE
!>> show status of the object for debugging
  call tell(myname_,'--- argument ioper =',ioper)
  call tell(myname_,'associated(return) =',associated(self))
  !if(associated(self)) call obOper_show_(myname_,self)
#endif
end function createbyindex_

function createbyvmold_(mold) result(self)
!>> initialize an obOper to its components (linked-lists)
  use gsi_obOperTypeManager, only: obOper_typeIndex      ! to type-index
  use gsi_obOperTypeManager, only: obOper_typeIndex      ! to type-index
  implicit none
  class(obOper),pointer:: self
  class(obOper),target,intent(in):: mold

  character(len=*),parameter:: myname_=myname//"::createbyvmold_"
  integer(kind=i_kind):: itype  ! for a registered obsNode type index

  self => mold
  if(associated(self)) then
    allocate(self,mold=mold)

    ! Get its corresponding obsNode type name, then convert to its type-index
    itype=obOper_typeIndex(self)

        if(itype<lbound(obsLLists,1) .or. itype>ubound(obsLLists,1)) then
          call perr(myname_,'unexpected value, itype =',itype)
          call perr(myname_,'    lbound(obsLLists,1) =',lbound(obsLLists,1))
          call perr(myname_,'    ubound(obsLLists,1) =',ubound(obsLLists,1))
          call perr(myname_,'              %mytype() =',self%mytype())
          call perr(myname_,'      %mytype(nodetype) =',self%mytype(nodetype=.true.))
          call  die(myname_)
        endif
        if(itype<lbound( obsdiags,1) .or. itype>ubound( obsdiags,1)) then
          call perr(myname_,'unexpected value, itype =',itype)
          call perr(myname_,'    lbound( obsdiags,1) =',lbound( obsdiags,1))
          call perr(myname_,'    ubound( obsdiags,1) =',ubound( obsdiags,1))
          call perr(myname_,'              %mytype() =',self%mytype())
          call perr(myname_,'      %mytype(nodetype) =',self%mytype(nodetype=.true.))
          call  die(myname_)
        endif

    call self%init(obsLLists(itype,:), &
                    obsdiags(itype,:)  )
  endif

#ifdef DEBUG_VERBOSE
! show status of the object for debugging
  call tell(myname_,'--- argument mold%mytype() =',mold%mytype())
  call tell(myname_,'     mold%mytype(nodetype) =',mold%mytype(nodetype=.true.))
  call tell(myname_,'        associated(return) =',associated(self))
  if(associated(self)) call obOper_show_(myname_,self)
#endif
end function createbyvmold_

subroutine oboper_show_(mname,self)
  use gsi_obOper, only: obOper
  use gsi_obOperTypeManager, only: obOper_typeIndex
  use gsi_obOperTypeManager, only: obOper_typeInfo
  use m_obsNodeTypeManager , only: obsNode_typeIndex     ! to type-index
  use mpeu_util, only: tell
  implicit none
  character(len=*),intent(in):: mname
  class(obOper),target,intent(in):: self

  call tell(mname,' obOper_typeIndex(%) =',obOper_typeIndex(self))
  call tell(mname,'  obOper_typeInfo(%) =',obOper_typeInfo(self))
  call tell(mname,'  associated(%obsLL) =',associated(self%obsLL))
  call tell(mname,'associated(%odiagLL) =',associated(self%odiagLL))
  call tell(mname,'     self%nodetype() =', self%mytype(nodetype=.true.))
end subroutine obOper_show_

subroutine destroy_(self)
  implicit none
  class(obOper),pointer,intent(inout):: self
  if(associated(self)) then
    call self%clean()
    deallocate(self)
  endif
end subroutine destroy_

function headNode_(iobOper,ibin) result(anode)
!>> Example: -- get the head node of an obOper%obsLL(ibin)
!>>   psptr => psNode_typecast(headNode(iobOper_ps))
  use m_obsNode , only: obsNode
  use m_obsLList, only: obsLList_headNode
  use gsi_obOper, only: obOper
  implicit none
  integer(kind=i_kind),intent(in):: iobOper
  integer(kind=i_kind),intent(in):: ibin
  class(obsNode),pointer:: anode

  character(len=*),parameter:: myname_=myname//"::headNode_"
  class(obOper),pointer:: obOper_

  obOper_ => createbyindex_(iobOper)
        if(.not.associated(obOper_)) then
          call perr(myname_,'createbuindex_(), associated(obOper_) =',associated(obOper_))
          call perr(myname_,'                                ioper =',iobOper)
          call perr(myname_,'                                 ibin =',ibin)
          call  die(myname_)
        endif

  anode => obsLList_headNode(obOper_%obsLL(ibin))
  call destroy_(obOper_)
end function headNode_

subroutine lobsdiags_statusCheck_(who,allocated)
!-- check the allocation status of basic obsdiags components.
  use obsmod, only: luse_obsdiag
  implicit none
  character(len=*),intent(in):: who
  logical,intent(in):: allocated

  if(.not.luse_obsdiag) return
  if(allocated) then
    if( .not.lobsdiags_allocated_ .or. &
        .not.lobstypes_allocated_ ) then
      if(.not.lobsdiags_allocated_) call perr(who,'.not.lobsdiags_allocated_')
      if(.not.lobstypes_allocated_) call perr(who,'.not.lobstypes_allocated_')
      call die(who)
    endif

  else
    if( lobsdiags_allocated_ .or. &
        lobstypes_allocated_ ) then
      if(lobsdiags_allocated_) call perr(who,'lobsdiags_allocated_ already')
      if(lobstypes_allocated_) call perr(who,'lobstypes_allocated_ already')
      call die(who)
    endif
  endif
end subroutine lobsdiags_statusCheck_

subroutine mread_(cdfile,mPEs,force,jiter_expected,alwaysLocal)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    m_obdiags::mread_
!   prgmmr:      tremolet
!
! abstract: Read obsdiags data structure from file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-08-04  todling  - using get_lun to determine file unit number
!   2007-10-03  todling  - expanded to account for full observer 
!   2009-01-08  todling  - remove reference to ozohead
!   2009-01-23  todling  - add read_gpshead
!   2009-04-02  meunier  - add read_laghead
!   2010-04-27  tangborn - addded read_colvkhead
!   2010-05-26  treadon  - add read_tcphead
!   2011-05-18  todling  - aero, aerol, and pm2_5
!   2011-09-20  hclin    - 1d wij for aero
!   2015-02-04  j guo   - Re-implemented to support re-configurable observation
!                         operators.  read_() is split to read_() for a single
!                         file, and mread_() for one file only or all files for
!                         redistribution
!   2015-10-09  j guo   - Now it uses Fortran 2003 dynamic polymorphism.
!
!   input argument list:
!     cdfile - filename to read data from
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use mpeu_util, only: tell,perr,die,stdout_open,stdout_close,stdout
  _TIMER_USE_
  use kinds, only: r_kind,i_kind

  use obsmod, only: lobserver
  use mpimod, only: myPE
  use m_latlonRange, only: latlonRange
  use m_latlonRange, only: latlonRange_reset
  use m_latlonRange, only: latlonRange_islocal
  use m_latlonRange, only: latlonRange_readBcast
  use m_latlonRange, only: latlonRange_allDump

  use m_obsdiagNode, only: obsdiagLList_dump
  implicit none
  character(len=*), intent(in) :: cdfile          ! prefix, "obsdiags.<miter>"
  integer(i_kind),optional,intent(in):: mPEs      ! number of files, from 0 to mPEs-1
  logical        ,optional,intent(in):: force     ! force to read ob_types, regardless l4dvar etc.
  integer(i_kind),optional,intent(in):: jiter_expected  ! expected input jiter
  logical        ,optional,intent(in):: alwaysLocal ! read all files

! ----------------------------------------------------------
  character(len=*),parameter:: myname_=myname//"::mread_"
  logical:: redistr,exist_
  integer(i_kind):: lPE,uPE,iPE,ier
  integer(i_kind):: jtyp,jread
  logical:: force_read
  logical:: alwaysLocal_
  logical:: fileislocal
  type(latlonRange),allocatable,dimension(:):: allRanges
_ENTRY_(myname_)
_TIMER_ON_(myname_)
!call stdout_open("obsdiags_mread")
  force_read=.false.
  if(present(force)) force_read=force
  alwaysLocal_=obsdiags_alwaysLocal
  if(present(alwaysLocal)) alwaysLocal_=alwaysLocal

  call lobsdiags_statusCheck_(myname_,allocated=.true.)

        ! Determine the configuration, either read-my-own-data-only, or
        ! try-to-read-all-data-available.

  lPE=myPE
  uPE=lPE
  redistr=.false.
  if(present(mPEs)) then
    if(mPEs>0) then
      redistr=.true.
      lPE=0
      uPE=-1
      do iPE=lPE,mPEs-1
        inquire(file=trim(filename_(cdfile,iPE)), exist=exist_)
        if(exist_) uPE=iPE
      enddo
    endif
  endif

        ! Reset components of obsdiags, for their re-construction from files
  call reset_()

  if(CHECK_SIZES_) then
    allocate(lsize_type(nobs_type))
    allocate(nsize_type(nobs_type))
    allocate(lsize_diag(nobs_type))
    allocate(nsize_diag(nobs_type))
    allocate(msize_diag(nobs_type))

    lsize_type(:)=0
    nsize_type(:)=0
    lsize_diag(:)=0
    nsize_diag(:)=0
    msize_diag(:)=0
  endif

        ! MPI_Barrier() calls are not necessary.  They are used here to ensure
        ! the log-messages mean what they really mean, if only the root is used to
        ! report the all-PE status.

  if(SYNCH_MESSAGES) call MPI_Barrier(gsi_comm_world,ier)

  if(redistr) then
    if(mype==0) then
      call tell(myname_,'Reading obsdiags files for redistribution, nPEs =',uPE-lPE+1)
      call tell(myname_,'                    prefix of the files, cdfile =',trim(cdfile))
      call tell(myname_,'                                            lPE =',lPE)
      call tell(myname_,'                                            uPE =',uPE)
      call tell(myname_,'                                    alwaysLocal =',alwaysLocal_)
    endif

    allocate(allRanges(0:uPE))
    call latlonRange_reset(allRanges)
    call latlonRange_readBcast(hdfilename_(cdfile),allRanges,root=0,comm=gsi_comm_world)

!#define SHOW_LLRANGE
#ifdef SHOW_LLRANGE
    call latlonRange_alldump(allRanges,"obsLLRange")
#endif


    jread=-1    ! checker of the input jiter values
    do iPE=lPE,uPE
      fileislocal=latlonRange_islocal(allRanges(iPE))
      if(alwaysLocal_.or.fileislocal) then
        call read_(cdfile,iPE,redistr,fileislocal=fileislocal, &
                force=force, &
                jiter_expected=jiter_expected, &
                verbose=.not.alwaysLocal_.or.myPE==0, &
                jread=jread)
      endif
    enddo

!#define DEBUG_obsdiags
#ifdef DEBUG_obsdiags
        ! This is an example of dumping information for debugging, on selected
        ! PEs, for specific jtyp and ibin.
        !
        ! This example is on PE #1, for (jtype==3 .and. ibin==3).

    if(myPE==1) then
      call tell(myname_)
      call tell(myname_,'dumping obsdiags(), jtyp =',3)
      call tell(myname_,'                    ibin =',3)
      call tell(myname_,'                   jread =',jread)
      call obsdiagLList_dump(obsdiags(3,3),jiter=jread)
    endif
#endif

        ! Sort to ensure the ordering is unique.
    call lsort_()

    call latlonRange_reset(allRanges)
    deallocate(allRanges)

  else  ! of if(redistr)
    call read_(cdfile,myPE,redistr,fileislocal=.true., &
        force=force, &
        jiter_expected=jiter_expected, &
        verbose=.true.)

  endif ! of if(redistr)

  if(myPE==0) then
    call tell(myname_,'Finished reading of all obsdiags files, nPEs =',uPE-lPE+1)
  endif
  
  if(CHECK_SIZES_) then
    do jtyp=lbound(lsize_type,1),ubound(lsize_type,1)
      if( msize_diag(jtyp)>0.or.lsize_diag(jtyp)>0.or.nsize_diag(jtyp)>0 .or. &
                                lsize_type(jtyp)>0.or.nsize_type(jtyp)>0 ) then
        write(stdout,'(i5.3,i5,7x,5i8,2x,l1)')   myPE,jtyp ,lsize_type(jtyp),nsize_type(jtyp), &
                                           msize_diag(jtyp),lsize_diag(jtyp),nsize_diag(jtyp)
      endif
    enddo

    call iMPI_reduceSUM_(lsize_type,root=0,comm=gsi_comm_world)
    call iMPI_reduceSUM_(nsize_type,root=0,comm=gsi_comm_world)
    call iMPI_reduceSUM_(lsize_diag,root=0,comm=gsi_comm_world)
    call iMPI_reduceSUM_(nsize_diag,root=0,comm=gsi_comm_world)
    call iMPI_reduceSUM_(msize_diag,root=0,comm=gsi_comm_world)

    if(myPE==0) then
      do jtyp=lbound(lsize_type,1),ubound(lsize_type,1)
        if( msize_diag(jtyp)>0.or.lsize_diag(jtyp)>0.or.nsize_diag(jtyp)>0 .or. &
                                  lsize_type(jtyp)>0.or.nsize_type(jtyp)>0 ) then
          write(stdout,'(2x,a,i5,7x,5i8,2x,l1)')    '***',jtyp ,lsize_type(jtyp),nsize_type(jtyp), &
                                               msize_diag(jtyp),lsize_diag(jtyp),nsize_diag(jtyp)
        endif
      enddo
    endif

    deallocate(lsize_type)
    deallocate(nsize_type)
    deallocate(lsize_diag)
    deallocate(nsize_diag)
    deallocate(msize_diag)
  endif

  if(SYNCH_MESSAGES) call MPI_Barrier(gsi_comm_world,ier)
  if(DO_SUMMARY) call summary_(myname_)

  if(lobserver) then
    if(.not.force_read) then
      !call destroyobs(   skipit=.true.)
      call reset_(obsdiags_keep=.true.)
    endif
  endif
!call stdout_close()
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine mread_

subroutine reset_(obsdiags_keep)
  !use obsmod, only: obsdiags
  use obsmod, only: luse_obsdiag
  use obsmod, only: lobsdiag_allocated

  use m_obsdiagNode, only: obsdiagLList_reset
  use m_obsdiagNode, only: obsdiagLList_rewind
  use m_obsLList, only: obsLList_reset
  use m_obsNode , only: obsNode
  use gsi_obOperTypeManager, only: obOper_typeMold
  use gsi_obOperTypeManager, only: obOper_lbound
  use gsi_obOperTypeManager, only: obOper_ubound
  use m_obsNodeTypeManager , only: obsNode_typeMold

  _TIMER_USE_
  implicit none
  logical,optional,intent(in):: obsdiags_keep
  character(len=*),parameter:: myname_=myname//'::reset_'
  integer(i_kind):: ii,jj
  logical:: obsdiags_keep_
  integer(i_kind):: ier
  class(obsNode),pointer:: mNode_
  class(obOper ),pointer:: mOper_
_ENTRY_(myname_)
_TIMER_ON_(myname_)

_TRACEV_(myname_,'lobsdiag_allocated   =',lobsdiag_allocated)
_TRACEV_(myname_,'lobsdiags_allocated_ =',lobsdiags_allocated_)

  ASSERT(nobs_type>0)
  ASSERT(nobs_bins>0)

  ! Both objects, obsdiags and obsLLists are checked for their associated sizes
  ! and allocated shapes, regardless luse_obsdiag or not.  This is to simplify
  ! the algorithm logic.  The enforcements of (luse_obsdiag) are done on lower
  ! levels only.

  if(.not.lobstypes_allocated_) then
    lobstypes_allocated_=.true.
    if(.not.associated(obsLLists)) call die(myname_,'unexpectedly, .not.associated(obsLLists)')
  endif

  if(.not.lobsdiags_allocated_) then
    lobsdiags_allocated_=.true.
    if(.not.associated(obsdiags )) call die(myname_,'unexpectedly, .not.associated(obsdiags)')
  endif

  ASSERT(all(shape(obsdiags  )==shape(obsLLists  )))
  ASSERT(     size(obsdiags,1)== size(obsLLists,1) )
  ASSERT(     size(obsdiags,2)== size(obsLLists,2) )

  obsdiags_keep_=.false.
  if(present(obsdiags_keep)) obsdiags_keep_=obsdiags_keep

  do ii=1,size(obsLLists,2)      ! nobs_bins
    do jj=1,size(obsLLists,1)    ! nobs_type
      if(luse_obsdiag) then
        if(.not.obsdiags_keep_) then
          call obsdiagLList_reset(obsdiags(jj,ii))
          lobsdiag_allocated=.false.

        else
          call obsdiagLList_rewind(obsdiags(jj,ii))

          ! In cases of rewinding without resetting, an obsdiagLList can
          ! be either initialized (lobsdiag_allocated), or not initialized
          ! (.not.lobsdiag_allocated).  So the code here should not try
          ! to alter the value of lobsdiag_allocated.
        endif
      endif

!++++
      mOper_ => obOper_typeMold(jj)
        if(.not.associated(mOper_)) then
          call perr(myname_,'obOper_typeMold(j) not associated, j =',jj)
          call perr(myname_,'                       obOper_lbound =',obOper_lbound)
          call perr(myname_,'                       obOper_ubound =',obOper_ubound)
          call  die(myname_)
        endif

      mNode_ => mOper_%nodeMold()
        if(.not.associated(mNode_)) then
          call perr(myname_,'mOper_%nodeMold() not associated, j =',jj)
          call perr(myname_,'                    mOper_%mytype() =',mOper_%mytype())
          call  die(myname_)
        endif
      
      mOper_ => null()

!++++

      call obsLList_reset(obsLLists(jj,ii),mold=mNode_, stat=ier)
        if(ier/=0) then
          call perr(myname_,'call obsLList_reset(), stat =',ier)
          call perr(myname_,'                       ibin =',ii)
          call perr(myname_,'                      jtype =',jj)
          call perr(myname_,'              mold%mytype() =',mNode_%mytype())
          call  die(myname_)
        endif

      mNode_ => null()
    enddo
  enddo
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine reset_

subroutine lsort_()
!$$$  subprogram documentation block
!
! abstract: sort entries of obsdiags(:,:) and obsLLists(:,:)
!
! program history log:
!
!   input argument list:
!
!$$$

  use gsi_unformatted, only: unformatted_open
  use obsmod, only: luse_obsdiag

  use m_obsLList, only: obsLList_lsort
  use m_obsdiagNode, only: obsdiagLList_lsize
  use m_obsdiagNode, only: obsdiagLList_lsort

  _TIMER_USE_
  implicit none

  character(len=*), parameter :: myname_=myname//"::lsort_"

  integer(i_kind) :: ii,jj !,iobs,lobs,ierr
_ENTRY_(myname_)
_TIMER_ON_(myname_)
! ----------------------------------------------------------
  call lobsdiags_statusCheck_(myname_,allocated=.true.)

  if (luse_obsdiag) then

     ASSERT(all(shape(obsdiags)==shape(obsLLists)))
     ASSERT(size(obsdiags,1)==size(obsLLists,1))
     ASSERT(size(obsdiags,2)==size(obsLLists,2))

  endif

  do jj=1,size(obsdiags,1)
     do ii=1,size(obsdiags,2)
        call obsdiagLList_lsort(obsdiags(jj,ii),itype=jj,ibin=ii)
     enddo
  enddo

  do jj=1,size(obsLLists,1)
     do ii=1,size(obsLLists,2)
       call obsLList_lsort(obsLLists(jj,ii),itype=jj,ibin=ii)
     enddo
  enddo

! ----------------------------------------------------------
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine lsort_

subroutine write_(cdfile,luseonly,force)
!$$$  subprogram documentation block
!
! abstract: Write obsdiags data structure to file.
!
! program history log:
!   2007-07-05  tremolet
!   2007-10-03  todling - expanded to account for full observer
!   2007-10-24  todling - add parameter nchnperobs to obsdiag
!   2009-01-08  todling - remove reference to ozohead
!   2009-01-27  todling - add gps write
!   2010-05-26  treadon - add write_tcphead
!   2010-06-03  todling - add write_colvkhead
!   2011-05-18  todling - aero, aerol, and pm2_5
!   2015-02-04  j guo   - Re-implemented to support re-configurable observation
!                         operators.
!   2015-10-09  j guo   - Now it uses Fortran 2003 dynamic polymorphism.
!
!   input argument list:
!     cdfile - filename to write data
!
!$$$

use mpeu_util, only: tell,die,perr,stdout_open,stdout_close
_TIMER_USE_

  use gsi_unformatted, only: unformatted_open
  use mpimod, only: mype
  use gsi_4dvar, only: l4dvar
  use  jfunc, only: jiter, miter

  use m_obsLList, only: obsLList_write
  use m_obsdiagNode, only: obsdiagLList_lsize
  use m_obsdiagNode, only: obsdiagLList_write

  use m_latlonRange, only: latlonRange
  use m_latlonRange, only: latlonRange_reset
  use m_latlonRange, only: latlonRange_gatherWrite
  use m_latlonRange, only: latlonRange_gatherDump

  implicit none
  character(len=*), intent(in) :: cdfile        ! := "obsdiags.<miter>"
  logical,optional, intent(in) :: luseonly      ! output only if(%luse)
  logical,optional, intent(in) :: force         ! write all out regardlessly

  character(len=*), parameter :: myname_=myname//"::write_"

integer(i_kind) :: iunit,istat
integer(i_kind) :: ii,jj,ier
logical :: luseonly_
logical :: force_write
type(latlonRange):: luseRange
! ----------------------------------------------------------
_ENTRY_(myname_)
_TIMER_ON_(myname_)
!call stdout_open("obsdiags_write")
  force_write=.false.
  if(present(force)) force_write=force
  call lobsdiags_statusCheck_(myname_,allocated=.true.)

        ASSERT(all(shape(obsdiags)==shape(obsLLists)))
        ASSERT(size(obsdiags,1)==size(obsLLists,1))
        ASSERT(size(obsdiags,2)==size(obsLLists,2))

  luseonly_=.false.
  if(present(luseonly)) luseonly_=luseonly
 
  call unformatted_open( unit=iunit, &
        file=trim(filename_(cdfile,myPE)), &
        class='.obsdiags.', &
        action='write', &
        status='unknown', &
        newunit=.true., &       ! with newunit=.true., unit returns a value assigned by Fortran.
        iostat=istat,silent=.true.)
                if(istat/=0) then
                  call perr(myname_,'unformatted_open(), file =',filename_(cdfile,myPE))
                  call perr(myname_,'                 newunit =',iunit)
                  call perr(myname_,'                  iostat =',istat)
                  call  die(myname_)
                endif

  if(DO_SUMMARY) call summary_(myname_)

  do ii=1,size(obsdiags,2)
    do jj=1,size(obsdiags,1)
      call obsdiagLList_write(obsdiags(jj,ii),iunit,luseonly_,jiter,miter,jj,ii,luseRange=luseRange)

      if (force_write .or. l4dvar) then
        call obsLList_write(obsLLists(jj,ii),iunit,luseonly_,jj,luseRange=luseRange)
      endif

      write(iunit)ii,jj   ! a jj_obstype-block trailer
    enddo
  enddo

  close(iunit)

        ! latlonRange_gatherWrite() implies a mpi_barrier() action.
  call latlonRange_gatherWrite(luseRange,hdfilename_(cdfile),root=0,comm=gsi_comm_world)

#ifdef SHOW_LLRANGE
        ! Text-dump to diagnose the values
  call latlonRange_gatherDump(          "cvgLLRange",root=0,comm=gsi_comm_world)
  call latlonRange_gatherDump(luseRange,"obsLLRange",root=0,comm=gsi_comm_world)
#endif

  call latlonRange_reset(luseRange)

  if(SYNCH_MESSAGES) call MPI_Barrier(gsi_comm_world,ier)
  if (mype==0) call tell(myname_,'Finish writing obsdiags to file ',filename_(cdfile,myPE))

! ----------------------------------------------------------
!call stdout_close()
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine write_

subroutine read_(cdfile,iPE,redistr,fileislocal,force,jiter_expected,verbose,jread)
  use mpeu_util, only: tell,perr,die
  use mpeu_util, only: stdout
  use mpimod, only: mype
  use gsi_4dvar, only: l4dvar
  use gsi_unformatted, only: unformatted_open
  use  jfunc, only: jiter,miter
  _TIMER_USE_

  use obsmod, only: lobserver

  use m_obsLList, only: obsLList_read
  use m_obsLList, only: obsLList_lsize
  use m_obsLList, only: obsLList_lcount

  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obsdiagLList_read
  use m_obsdiagNode, only: obsdiagLList_lsize
  use m_obsdiagNode, only: obsdiagLList_lcount
  use m_obsdiagNode, only: obsdiagLookup_build
  use m_obsdiagNode, only: obsdiagLookup_clean

  implicit none
  character(len=*), intent(in ):: cdfile        ! prefix of the input file
  integer(i_kind ), intent(in ):: iPE           ! iPE of the input file
  logical         , intent(in ):: redistr       ! data redistribution is expected
  logical         , intent(in ):: fileislocal   ! the file to read, is known local

  logical,optional, intent(in ):: force         ! (force to read ob_type data
  integer(i_kind ), optional, intent(in   ):: jiter_expected    ! expecte input jiter
  logical,optional, intent(in ):: verbose       ! report each reading
  integer(i_kind ), optional, intent(inout):: jread     ! jiter read from the input

  character(len=*),parameter:: myname_=myname//'::read_'
  character(len=*),parameter:: diag_timer_=myname_//'.obsdiagLList_read'
  character(len=*),parameter:: list_timer_=myname_//'.obsLList_read'
  integer(i_kind):: ii,jj
  integer(i_kind):: ki,kj
  integer(i_kind):: iunit,istat
  integer(i_kind):: jread_
  integer(i_kind):: lsize_type_,nsize_type_
  integer(i_kind):: lsize_diag_,nsize_diag_,msize_diag_
  type(obs_diag),pointer:: leadNode => NULL()
  logical:: force_read
  logical:: verbose_
_ENTRY_(myname_)
_TIMER_ON_(myname_)

  call lobsdiags_statusCheck_(myname_,allocated=.true.)
  force_read=.false.
  if(present(force)) force_read=force

  verbose_=.false.
  if(present(verbose)) verbose_=verbose

        ASSERT(all(shape(obsdiags)==shape(obsLLists)))
        ASSERT(size(obsdiags,1)==size(obsLLists,1))
        ASSERT(size(obsdiags,2)==size(obsLLists,2))
        if(CHECK_SIZES_) then
          ASSERT(size(obsdiags,1)==size(lsize_type ))
          ASSERT(size(obsdiags,1)==size(nsize_type ))
          ASSERT(size(obsdiags,1)==size(lsize_diag ))
          ASSERT(size(obsdiags,1)==size(nsize_diag ))
        endif

  call unformatted_open( unit=iunit, &
        file=trim(filename_(cdfile,iPE)), &
        class='.obsdiags.', &
        action='read', &
        status='old', &
        newunit=.true., &       ! with newunit=.true., unit returns a value assigned by Fortran.
        iostat=istat,silent=.true.)
                if(istat/=0) then
                  call perr(myname_,'unformatted_open(), file =',trim(filename_(cdfile,iPE)))
                  call perr(myname_,'                    myPE =',myPE)
                  call perr(myname_,'                     iPE =',iPE)
                  call perr(myname_,'                   miter =',miter)
                  call perr(myname_,'                 redistr =',redistr)
                  call perr(myname_,'                 newunit =',iunit)
                  call perr(myname_,'                  iostat =',istat)
                  call  die(myname_)
                endif

  if(verbose_) call tell(myname_,'Reading obsdiags, file =',trim(filename_(cdfile,iPE)))

  leadNode => null()
  do ii=1,size(obsdiags,2)
    do jj=1,size(obsdiags,1)
      if(CHECK_SIZES_) then
        lsize_type_= obsLList_lcount(obsLLists(jj,ii),luseonly=.true.,recount=.true.)
        nsize_type_= obsLList_lsize (obsLLists(jj,ii)                )

        lsize_diag_= obsdiagLList_lcount(obsdiags(jj,ii),luseonly=.true.,recount=.true.)
        !msize_diag_= obsdiagLList_lcount(obsdiags(jj,ii),museonly=.true.)
        nsize_diag_= obsdiagLList_lsize (obsdiags(jj,ii)                )
      endif

      call obsdiagLList_read(obsdiags(jj,ii),iunit,redistr,jiter,miter,jj,ii,jread_,leadNode=leadNode, &
        jiter_expected=jiter_expected)
      if(present(jread)) then
        if(jread/=jread_) then
          if(jread>0) then
            call perr(myname_,'not the same iteration, jiter =',jiter)
            call perr(myname_,'                  saved jread =',jread)
            call perr(myname_,'                current jread =',jread_)
            call  die(myname_)
          endif
          jread=jread_
        endif
      endif

      call obsdiagLookup_build(obsdiags(jj,ii),leadNode=leadNode,jiter=jread)
          leadNode => null()     ! nullified after its use, to avoid leadNode dangling arround.

      if (force_read .or. l4dvar.and..not.(lobserver.and.jiter==1)) then
        call obsLList_read(obsLLists(jj,ii),iunit,redistr,obsdiags(jj,ii),jj)
      endif

      call obsdiagLookup_clean(obsdiags(jj,ii))

      read(iunit)ki,kj
      if(ki/=ii .or. kj/=jj) then
        call perr(myname_,'mismatched block id, file =',filename_(cdfile,iPE))
        if(kj/=jj) then
        call perr(myname_,'               reading kj =',kj)
        call perr(myname_,'             expecting jj =',jj)
        endif
        if(ki/=ii) then
        call perr(myname_,'               reading ki =',ki)
        call perr(myname_,'             expecting ii =',ii)
        endif
        call perr(myname_,'                     file =',filename_(cdfile,iPE))
        call perr(myname_,'                   cdfile =',cdfile)
        call perr(myname_,'                     myPE =',myPE)
        call perr(myname_,'                      iPE =',iPE)
        call perr(myname_,'                    miter =',miter)
        call perr(myname_,'                  redistr =',redistr)
        call perr(myname_,'                  newunit =',iunit)
        call perr(myname_,'                   iostat =',istat)
        call  die(myname_)
      endif

      ASSERT(1<=jj.and.jj<=nobs_type)

      if(CHECK_SIZES_) then
        lsize_type_= obsLList_lcount(obsLLists(jj,ii),luseonly=.true.)-lsize_type_
        nsize_type_= obsLList_lsize (obsLLists(jj,ii)                )-nsize_type_

        lsize_diag_= obsdiagLList_lcount(obsdiags(jj,ii),luseonly=.true.)-lsize_diag_
        !msize_diag_= obsdiagLList_lcount(obsdiags(jj,ii),museonly=.true.)-msize_diag_
        nsize_diag_= obsdiagLList_lsize (obsdiags(jj,ii)                )-nsize_diag_

        if( fileislocal  .or. lsize_type_>0.or.nsize_type_>0 .or. &
            msize_diag_>0.or. lsize_diag_>0.or.nsize_diag_>0 ) then
          write(stdout,'(i5.3,2i5,2x,5i6,2x,l1)') iPE,jj,ii,lsize_type_,nsize_type_, &
                                                msize_diag_,lsize_diag_,nsize_diag_,fileislocal
        endif
      
        lsize_type(jj)= lsize_type(jj) +lsize_type_
        nsize_type(jj)= nsize_type(jj) +nsize_type_

        lsize_diag(jj)= lsize_diag(jj) +lsize_diag_
        !msize_diag(jj)= msize_diag(jj) +msize_diag_
        nsize_diag(jj)= nsize_diag(jj) +nsize_diag_
      endif

    enddo       ! jj=1,size(obsdiags,1)
  enddo         ! ii=1,size(obsdiags,2)

  close(iunit)
! ----------------------------------------------------------
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine read_

function filename_(prefix,iPE)
!>> name of partitioned (obsdiags,obsLLists) files
  implicit none
  character(len=:),allocatable:: filename_
  character(len=*)    , intent(in ):: prefix
  integer(kind=i_kind), intent(in ):: iPE

  character(len=4):: chPE
  write(chPE,'(i4.4)') iPE
  filename_=trim(adjustl(prefix))//'.'//trim(chPE)
end function filename_

function hdfilename_(prefix)
!>> name of the header file
  use kinds, only: i_kind
  implicit none
  character(len=:),allocatable:: hdfilename_
  character(len=*)    , intent(in ):: prefix
  hdfilename_=trim(adjustl(prefix))//'.headers'
end function hdfilename_

subroutine summary_(title)
!-- get a summary of obsdiags(:,:) and obsLLists(:,:)
use obsmod, only: luse_obsdiag
use mpeu_util, only: tell,die,perr,stdout_open,stdout_close
_TIMER_USE_

  use gsi_unformatted, only: unformatted_open
  use gsi_4dvar, only: nobs_bins

  use m_obsLList, only: obsLList_lsize => obsLList_lcount
  use m_obsdiagNode, only: obsdiagLList_lsize => obsdiagLList_lcount

  implicit none
  character(len=*), intent(in) :: title

  character(len=*), parameter :: myname_=myname//"::summary_"

  integer(i_kind) :: ii,jj
  integer(i_kind),dimension(nobs_type,nobs_bins):: ldiag,ndiag
  integer(i_kind),dimension(nobs_type,nobs_bins):: lobss,nobss
_ENTRY_(myname_)
_TIMER_ON_(myname_)
! ----------------------------------------------------------

  call lobsdiags_statusCheck_(myname_,allocated=.true.)

  if (luse_obsdiag) then
        ASSERT(all(shape(obsdiags)==shape(obsLLists)))
        ASSERT(size(obsdiags,1)==size(obsLLists,1))
        ASSERT(size(obsdiags,2)==size(obsLLists,2))
  endif

  do ii=1,size(obsdiags,2)
    do jj=1,size(obsdiags,1)
      ldiag(jj,ii) = obsdiagLList_lsize(obsdiags(jj,ii),luseonly=.true. ,recount=.true.)
      ndiag(jj,ii) = obsdiagLList_lsize(obsdiags(jj,ii),luseonly=.false.,recount=.true.)
    enddo
  enddo

  do ii=1,size(obsLLists,2)
    do jj=1,size(obsLLists,1)
      lobss(jj,ii) = obsLList_lsize(obsLLists(jj,ii),luseonly=.true. ,recount=.true.)
      nobss(jj,ii) = obsLList_lsize(obsLLists(jj,ii),luseonly=.false.,recount=.true.)
    enddo
  enddo

  call gather_write_(title,lobss,ldiag,nobss,ndiag,root=0,comm=gsi_comm_world)

! ----------------------------------------------------------
_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine summary_

subroutine gather_write_(title,lobss,ldiag,nobss,ndiag,root,comm)
  use mpimod   , only: mype,nPE
  use kinds    , only: i_kind
  use mpeu_mpif, only: MPI_ikind
  _TIMER_USE_
  implicit none
  character(len=*),intent(in):: title
  integer(kind=i_kind),dimension(:,:),intent(in):: lobss,ldiag
  integer(kind=i_kind),dimension(:,:),intent(in):: nobss,ndiag
  integer(kind=MPI_ikind),intent(in):: root
  integer(kind=MPI_ikind),intent(in):: comm

  character(len=*),parameter:: myname_=myname//'::gather_write_'
  integer(kind=i_kind):: jj,ii,iPE
  integer(kind=i_kind) :: mtyp,mbin,mPEs
  integer(kind=i_kind),allocatable,dimension(:,:,:):: ldiagm,ndiagm
  integer(kind=i_kind),allocatable,dimension(:,:,:):: lobssm,nobssm

_ENTRY_(myname_)
_TIMER_ON_(myname_)
  mtyp=size(lobss,1)
  mbin=size(lobss,2)
        ASSERT(mtyp==size(nobss,1))
        ASSERT(mbin==size(nobss,2))
        ASSERT(mtyp==size(ldiag,1))
        ASSERT(mbin==size(ldiag,2))
        ASSERT(mtyp==size(ndiag,1))
        ASSERT(mbin==size(ndiag,2))

  mPEs=0        ! its value is significant only on root
  if(myPE==root) mPEs=nPE

  allocate(lobssm(mtyp,mbin,0:mPEs-1))
  allocate(ldiagm(mtyp,mbin,0:mPEs-1))
  allocate(nobssm(mtyp,mbin,0:mPEs-1))
  allocate(ndiagm(mtyp,mbin,0:mPEs-1))

  call iMPI_gather_(lobss,lobssm,root,comm)
  call iMPI_gather_(nobss,nobssm,root,comm)
  call iMPI_gather_(ldiag,ldiagm,root,comm)
  call iMPI_gather_(ndiag,ndiagm,root,comm)

  if(myPE==root) then
    do iPE=0,nPE-1
      write(stdout,'(2a,i6)'     ) title,'(): local obs/diag counts, iPE =',iPE
      write(stdout,'(2a,9(1x,a))') title,'(): typ', ('|  -----lo -----ld  -----no -----nd',ii=1,mbin)
      do jj=1,mtyp
        write(stdout,'(2a,i3,9(1x,a,2(1x,2i8)))') &
                                   title,'(): ',jj , &
          ("|",lobssm(jj,ii,iPE),ldiagm(jj,ii,iPE), &
               nobssm(jj,ii,iPE),ndiagm(jj,ii,iPE), ii=1,mbin)
      enddo
    enddo
  endif

  deallocate(lobssm)
  deallocate(ldiagm)
  deallocate(nobssm)
  deallocate(ndiagm)
_TIMER_OFF_(myname_)
_EXIT_(myname_)
end subroutine gather_write_

subroutine iMPI_barrier_(comm)
  use mpeu_mpif, only: MPI_ikind
  use mpeu_util, only: die
  implicit none
  integer(kind=MPI_ikind),intent(in):: comm

  character(len=*),parameter:: myname_=myname//"::iMPI_barrier_"
  integer(kind=MPI_ikind):: ier

  call MPI_barrier(comm,ier)
        if(ier/=0) call die(myname_,'MPI_barrier() error, ierror =',ier)
end subroutine iMPI_barrier_

subroutine iMPI_gather_(isend,irecv,root,comm)
  use mpeu_mpif,only: MPI_ikind,MPI_type
  use mpeu_util, only: die
  use kinds, only: i_kind
  implicit none
  integer(kind=i_kind),dimension(:,:  ),intent(in ):: isend
  integer(kind=i_kind),dimension(:,:,:),intent(out):: irecv
  integer(kind=MPI_ikind),intent(in):: root
  integer(kind=MPI_ikind),intent(in):: comm

  character(len=*),parameter:: myname_=myname//"::iMPI_gather_"
  integer(kind=MPI_ikind):: itype,isize,ierr

  isize=size(isend)
  itype=MPI_type(isend)
  call MPI_gather(isend,isize,itype, &
                  irecv,isize,itype, root,comm,ierr)
        if(ierr/=0) call die(myname_,'MPI_gather() error, ierror =',ierr)
end subroutine iMPI_gather_

subroutine iMPI_reduceSUM_(iredu,root,comm)
  use mpeu_mpif,only: MPI_ikind,MPI_type,MPI_SUM
  use mpeu_util, only: die
  use kinds, only: i_kind
  implicit none
  integer(kind=i_kind),dimension(:),intent(inout):: iredu
  integer(kind=MPI_ikind),intent(in):: root
  integer(kind=MPI_ikind),intent(in):: comm

  character(len=*),parameter:: myname_=myname//"::iMPI_reduceSUM_"
  integer(kind=MPI_ikind):: itype,isize,ierr
  !integer(kind=kind(iredu)),dimension(size(iredu)):: irecv

  isize=size(iredu)
  itype=MPI_type(iredu)
  call MPI_reduce((iredu),iredu,isize,itype, MPI_SUM, root,comm,ierr)
        if(ierr/=0) call die(myname_,'MPI_reduce(MPI_SUM) error, ierror =',ierr)
  !iredu(:)=irecv(:)
end subroutine iMPI_reduceSUM_

subroutine create_obsmod_vars()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_obsmod_vars
!     prgmmr:    derber            org: np23           date: 2003-09-25
!
! abstract:  allocate arrays to hold observation related information
!
! program history log:
!   2003-09-25  derber
!   2004-05-13  treadon, documentation
!   2015-10-09  j guo   - moved here from MODULE OBSMOD with modifcations
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
    use gsi_4dvar, only: nobs_bins
    implicit none
    lobstypes_allocated_=.true.
    lobsdiags_allocated_=.true.
    allocate(obsllists(nobs_type,nobs_bins))
    allocate(obsdiags (nobs_type,nobs_bins))
    return
end subroutine create_obsmod_vars

subroutine destroy_obsmod_vars()
!-- Created to pair with create_obsmod_vars().
  implicit none
  deallocate(obsllists)
  deallocate(obsdiags )
  lobstypes_allocated_=.false.
  lobsdiags_allocated_=.false.
  return
end subroutine destroy_obsmod_vars

! ----------------------------------------------------------------------
subroutine inquire_obsdiags(kiter)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    inquire_obsdiags
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-07  lueken - added  subprogram doc block
!
!   input argument list:
!    kiter
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use constants, only:  one,two,three,four,five
use mpimod, only: mpi_max,mpi_comm_world,ierror,mype
use mpeu_mpif, only: mpi_type, MPI_IKIND
implicit none

integer(i_kind), intent(in   ) :: kiter

real(r_kind) :: sizei, sizer, sizel, sizep, ziter, zsize, ztot
integer(i_kind) :: ii,jj,iobsa(2),iobsb(2)
type(obs_diag), pointer :: obsptr => null()

! Any better way to determine size or i_kind, r_kind, etc... ?
sizei=four
sizer=8.0_r_kind
sizel=one
sizep=four

iobsa(:)=0
do ii=1,size(obsdiags,2)
   do jj=1,size(obsdiags,1)
      obsptr => obsdiags(jj,ii)%head
      do while (associated(obsptr))
         iobsa(1)=iobsa(1)+1
         if (ANY(obsptr%muse(:))) iobsa(2)=iobsa(2)+1
         obsptr => obsptr%next
      enddo
   enddo
enddo

call mpi_reduce(iobsa,iobsb,2_MPI_IKIND,mpi_type(iobsa),mpi_max,0_MPI_IKIND,mpi_comm_world,ierror)

if (mype==0) then
   ziter=real(kiter,r_kind)
   zsize = sizer*(three*ziter+two) + sizei + sizel*(ziter+one) + sizep*five
   ztot=real(iobsb(1),r_kind)*zsize
   ztot=ztot/(1024.0_r_kind*1024.0_r_kind)
 
   write(6,*)'obsdiags: Bytes per element=',NINT(zsize)
   write(6,*)'obsdiags: length total, used=',iobsb(1),iobsb(2)
   write(6,'(A,F8.1,A)')'obsdiags: Estimated memory usage= ',ztot,' Mb'
endif

end subroutine inquire_obsdiags

end module m_obsdiags
