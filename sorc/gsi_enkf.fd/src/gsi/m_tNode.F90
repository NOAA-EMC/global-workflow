module m_tNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_tNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of obs-type tNode ((virtual) temperature)
!
! program history log:
!   2016-05-18  j guo   - added this document block for the initial polymorphic
!                         implementation.
!   2019-09-20  X.Su    - add new variational QC parameters
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
  use m_obsdiagNode, only: obs_diag
  use m_obsdiagNode, only: obs_diags
  use kinds , only: i_kind,r_kind
  use mpeu_util, only: assert_,die,perr,warn,tell
  use m_obsNode, only: obsNode
  implicit none
  private

  public:: tNode

  type,extends(obsNode):: tNode
     !type(t_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL() 
     real(r_kind)    :: res           !  temperature residual
     real(r_kind)    :: err2          !  temperature error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     !real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: jb            !  variational quality control parameter
     integer(i_kind) :: ib            !  new variational quality control parameter
     integer(i_kind) :: ik            !  new variational quality control parameter
     real(r_kind)    :: tlm_tsfc(6)   !  sensitivity vector for sfc temp 
                                      !  forward model
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: tpertb        !  random number adding to the obs
     !logical         :: luse          !  flag indicating if ob is used in pen.
     logical         :: use_sfc_model !  logical flag for using boundary model
     logical         :: tv_ob         !  logical flag for virtual temperature or
     integer(i_kind) :: idx           !  index of tail number
     real(r_kind),dimension(:),pointer :: pred => NULL() 
                                      !  predictor for aircraft temperature bias 
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     integer(i_kind) :: ij(8)         !  horizontal locations

     !integer(i_kind) :: idv,iob	      ! device id and obs index for sorting
     !real   (r_kind) :: elat, elon      ! earth lat-lon for redistribution
     !real   (r_kind) :: dlat, dlon      ! earth lat-lon for redistribution
     real   (r_kind) :: dlev            ! reference to the vertical grid

     integer(i_kind) :: ich0=0  ! ich code to mark derived data.  See
                                ! tNode_ich0 and tNode_ich0_PBL_Pseudo below
  contains
    procedure,nopass::  mytype
    procedure::  setHop => obsNode_setHop_
    procedure::   xread => obsNode_xread_
    procedure::  xwrite => obsNode_xwrite_
    procedure:: isvalid => obsNode_isvalid_
    procedure::  gettlddp => gettlddp_

    ! procedure, nopass:: headerRead  => obsHeader_read_
    ! procedure, nopass:: headerWrite => obsHeader_write_
    procedure:: init  => obsNode_init_
    procedure:: clean => obsNode_clean_
  end type tNode

  public:: tNode_typecast
  public:: tNode_nextcast
        interface tNode_typecast; module procedure typecast_ ; end interface
        interface tNode_nextcast; module procedure nextcast_ ; end interface

  public:: tNode_appendto
        interface tNode_appendto; module procedure appendto_ ; end interface

  public:: tNode_ich0
  public:: tNode_ich0_pbl_pseudo
        integer(i_kind),parameter:: tNode_ich0            = 0
        integer(i_kind),parameter:: tNode_ich0_pbl_pseudo = tNode_ich0+1

  character(len=*),parameter:: MYNAME="m_tNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(tNode)
  use m_obsNode, only: obsNode
  implicit none
  type(tNode   ),pointer:: ptr_
  class(obsNode),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(tNode)
    ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(tNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type(tNode   ),pointer:: ptr_
  class(obsNode),target ,intent(in):: aNode

  class(obsNode),pointer:: inode_
  inode_ => obsNode_next(aNode)
  ptr_ => typecast_(inode_)
return
end function nextcast_

subroutine appendto_(aNode,oll)
!-- append aNode to linked-list oLL
  use m_obsNode , only: obsNode
  use m_obsLList, only: obsLList,obsLList_appendNode
  implicit none
  type(tNode),pointer,intent(in):: aNode
  type(obsLList),intent(inout):: oLL

  class(obsNode),pointer:: inode_
  inode_ => aNode
  call obsLList_appendNode(oLL,inode_)
  inode_ => null()
end subroutine appendto_

! obsNode implementations

function mytype()
  implicit none
  character(len=:),allocatable:: mytype
  mytype="[tNode]"
end function mytype

subroutine obsNode_init_(aNode)
  use aircraftinfo, only: npredt,aircraft_t_bc,aircraft_t_bc_pof
  implicit none
  class(tNode),intent(out):: aNode

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_init_'
_ENTRY_(myname_)

  !aNode = _obsNode_()
  aNode%llpoint => null()
  aNode%luse = .false.
  aNode%elat = 0._r_kind
  aNode%elon = 0._r_kind
  aNode%time = 0._r_kind
  aNode%idv  =-1
  aNode%iob  =-1
  !-aNode%dlev = 0._r_kind
  !-aNode%ich  =-1._i_kind

  if(aircraft_t_bc_pof .or. aircraft_t_bc) then
    allocate(aNode%pred(npredt))
  else
    allocate(aNode%pred(0))
  endif
_EXIT_(myname_)
return
end subroutine obsNode_init_

subroutine obsNode_clean_(aNode)
  implicit none
  class(tNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_clean_'
_ENTRY_(myname_)
!_TRACEV_(myname_,'%mytype() =',aNode%mytype())
    if(associated(aNode%pred)) deallocate(aNode%pred)
_EXIT_(myname_)
return
end subroutine obsNode_clean_

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use m_obsdiagNode, only: obsdiagLookup_locate
  use aircraftinfo, only: aircraft_t_bc,aircraft_t_bc_pof
  implicit none
  class(tNode),intent(inout):: aNode
  integer(i_kind),intent(in   ):: iunit
  integer(i_kind),intent(  out):: istat
  type(obs_diags),intent(in   ):: diagLookup
  logical,optional,intent(in   ):: skip

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_xread_'
  logical:: skip_
_ENTRY_(myname_)
  skip_=.false.
  if(present(skip)) skip_=skip

  istat=0
  if(skip_) then
    read(iunit,iostat=istat)
                if(istat/=0) then
                  call perr(myname_,'skipping read(%(res,err2,...)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                endif

  else
    if (.not. (aircraft_t_bc_pof .or. aircraft_t_bc)) then
      read(iunit,iostat=istat)  aNode%res       , &
                                aNode%err2      , &
                                aNode%raterr2   , &
                                aNode%b         , &
                                aNode%pg        , &
                                aNode%jb        , &
                                aNode%ib        , &
                                aNode%ik        , &
                                aNode%use_sfc_model, &
                                aNode%tlm_tsfc  , &
                                aNode%tpertb    , &
                                aNode%tv_ob     , &
                                aNode%k1        , &
                                aNode%kx        , &
                                aNode%dlev      , &
                                aNode%ich0      , &
                                aNode%wij       , &
                                aNode%ij
                if(istat/=0) then
                  call perr(myname_,'read(%(res,err2,...), iostat =',istat)
                  call perr(myname_,'     .not.(aircraft_t_bc_pof =',aircraft_t_bc_pof)
                  call perr(myname_,'          .or.aircraft_t_bc) =',aircraft_t_bc)
                  _EXIT_(myname_)
                  return
                endif
    else
      read(iunit,iostat=istat)  aNode%res       , &
                                aNode%err2      , &
                                aNode%raterr2   , &
                                aNode%b         , &
                                aNode%pg        , &
                                aNode%jb        , &
                                aNode%ib        , &
                                aNode%ik        , &
                                aNode%use_sfc_model, &
                                aNode%tlm_tsfc  , &
                                aNode%tpertb    , &
                                aNode%tv_ob     , &
                                aNode%idx       , &     ! 
                                aNode%pred(:)   , &     ! (1:npred)
                                aNode%k1        , &
                                aNode%kx        , &
                                aNode%dlev      , &
                                aNode%ich0      , &
                                aNode%wij       , &
                                aNode%ij
                if(istat/=0) then
                  call perr(myname_,'read(%res,err2,...), iostat =',istat)
                  call perr(myname_,'          aircraft_t_bc_pof =',aircraft_t_bc_pof)
                  call perr(myname_,'          .or.aircraft_t_bc =',aircraft_t_bc)
                  _EXIT_(myname_)
                  return
                endif
    end if

    aNode%diags => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,aNode%ich0+1_i_kind)
                if(.not.associated(aNode%diags)) then
                  call perr(myname_,'obsdiagLookup_locate(), %idv =',aNode%idv)
                  call perr(myname_,'                        %iob =',aNode%iob)
                  call perr(myname_,'                       %ich0 =',aNode%ich0)
                  call  die(myname_)
                endif
  endif
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  use aircraftinfo, only: aircraft_t_bc,aircraft_t_bc_pof
  implicit none
  class(tNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_xwrite_'
_ENTRY_(myname_)

  jstat=0
  if (.not. (aircraft_t_bc_pof .or. aircraft_t_bc)) then
    write(junit,iostat=jstat)   aNode%res       , &
                                aNode%err2      , &
                                aNode%raterr2   , &
                                aNode%b         , &
                                aNode%pg        , &
                                aNode%jb        , &
                                aNode%ib        , &
                                aNode%ik        , &
                                aNode%use_sfc_model, &
                                aNode%tlm_tsfc  , &
                                aNode%tpertb    , &
                                aNode%tv_ob     , &
                                aNode%k1        , &
                                aNode%kx        , &
                                aNode%dlev      , &
                                aNode%ich0      , &
                                aNode%wij       , &
                                aNode%ij
                if(jstat/=0) then
                  call perr(myname_,'write(%(res,err2,...), iostat =',jstat)
                  call perr(myname_,'      .not.(aircraft_t_bc_pof =',aircraft_t_bc_pof)
                  call perr(myname_,'           .or.aircraft_t_bc) =',aircraft_t_bc)
                  _EXIT_(myname_)
                  return
                endif
  else
    write(junit,iostat=jstat)   aNode%res       , &
                                aNode%err2      , &
                                aNode%raterr2   , &
                                aNode%b         , &
                                aNode%pg        , &
                                aNode%jb        , &
                                aNode%ib        , &
                                aNode%ik        , &
                                aNode%use_sfc_model, &
                                aNode%tlm_tsfc  , &
                                aNode%tpertb    , &
                                aNode%tv_ob     , &
                                aNode%idx       , &     ! 
                                aNode%pred(:)   , &     ! (1:npredt)
                                aNode%k1        , &
                                aNode%kx        , &
                                aNode%dlev      , &
                                aNode%ich0      , &
                                aNode%wij       , &
                                aNode%ij
                if(jstat/=0) then
                  call perr(myname_,'write(%res,err2,...), iostat =',jstat)
                  call perr(myname_,'           aircraft_t_bc_pof =',aircraft_t_bc_pof)
                  call perr(myname_,'           .or.aircraft_t_bc =',aircraft_t_bc)
                  _EXIT_(myname_)
                  return
                endif
  end if
_EXIT_(myname_)
return
end subroutine obsNode_xwrite_

subroutine obsNode_setHop_(aNode)
  use m_cvgridLookup, only: cvgridLookup_getiw
  implicit none
  class(tNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
_ENTRY_(myname_)
  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%dlev,aNode%ij,aNode%wij)
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(tNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
_ENTRY_(myname_)
  isvalid_=associated(aNode%diags)
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine gettlddp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(tNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  tlddp = tlddp + aNode%diags%tldepart(jiter)*aNode%diags%tldepart(jiter)
  if(present(nob)) nob=nob+1
return
end subroutine gettlddp_

end module m_tNode
