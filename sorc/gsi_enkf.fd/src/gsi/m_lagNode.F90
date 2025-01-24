module m_lagNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_lagNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of obs-type lagNode (lagrangian data)
!
! program history log:
!   2016-05-18  j guo   - added this document block for the initial polymorphic
!                         implementation.
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

  public:: lagNode

  type,extends(obsNode):: lagNode
     !type(lag_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diag_lon => NULL()
     type(obs_diag), pointer :: diag_lat => NULL()
     real(r_kind)    :: res_lon       ! residual
     real(r_kind)    :: res_lat       ! residual
     real(r_kind)    :: err2_lon      ! error squared
     real(r_kind)    :: err2_lat      ! error squared
     real(r_kind)    :: raterr2       ! square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: obslon        ! observed longitude (rad)
     real(r_kind)    :: obslat        ! observed latitude  (rad)
     real(r_kind)    :: geslon        ! guessed longitude (rad)
     real(r_kind)    :: geslat        ! guessed latitude  (rad)
     real(r_kind)   ,dimension(:),allocatable :: specr  ! TL parameter
     !real(r_kind)    :: time          ! observation time in sec     
     real(r_kind)    :: b             ! variational quality control parameter
     real(r_kind)    :: pg            ! variational quality control parameter
     integer(i_kind),dimension(:),allocatable :: speci  ! TL parameter
     integer(i_kind) :: intnum        ! internal number of balloon
     !logical         :: luse          ! flag indicating if ob is used in pen.

     !integer(i_kind) :: idv,iob	      ! device id and obs index for sorting
     !real   (r_kind) :: elat, elon      ! earth lat-lon for redistribution
     !real   (r_kind) :: dlat, dlon      ! earth lat-lon for redistribution
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
  end type lagNode

  public:: lagNode_typecast
  public:: lagNode_nextcast
        interface lagNode_typecast; module procedure typecast_ ; end interface
        interface lagNode_nextcast; module procedure nextcast_ ; end interface

  public:: lagNode_appendto
        interface lagNode_appendto; module procedure appendto_ ; end interface

  character(len=*),parameter:: MYNAME="m_lagNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(lagNode)
  use m_obsNode, only: obsNode
  implicit none
  type(lagNode ),pointer:: ptr_
  class(obsNode),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(lagNode)
    ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(lagNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type(lagNode ),pointer:: ptr_
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
  type(lagNode),pointer,intent(in):: aNode
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
  mytype="[lagNode]"
end function mytype

subroutine obsNode_init_(aNode)
  use lag_traj, only: lag_rk2itenpara_i,lag_rk2itenpara_r
  implicit none
  class(lagNode),intent(out):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_init_'
_ENTRY_(myname_)
  allocate(aNode%speci(lag_rk2itenpara_i), &
           aNode%specr(lag_rk2itenpara_r)  )
return
end subroutine obsNode_init_

subroutine obsNode_clean_(aNode)
  implicit none
  class(lagNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_clean_'
_ENTRY_(myname_)
!_TRACEV_(myname_,'%mytype() =',aNode%mytype())
  if(allocated(aNode%speci)) deallocate(aNode%speci)
  if(allocated(aNode%specr)) deallocate(aNode%specr)
_EXIT_(myname_)
return
end subroutine obsNode_clean_

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use m_obsdiagNode, only: obsdiagLookup_locate
  implicit none
  class(lagNode) , intent(inout):: aNode
  integer(i_kind) , intent(in   ):: iunit
  integer(i_kind) , intent(  out):: istat
  type(obs_diags) , intent(in   ):: diagLookup
  logical,optional, intent(in   ):: skip

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xread_'
  logical:: skip_
_ENTRY_(myname_)
  skip_=.false.
  if(present(skip)) skip_=skip

  istat=0
  if(skip_) then
    read(iunit,iostat=istat)
                if(istat/=0) then
                  call perr(myname_,'skipping read(%(res,err2,...)), istat =',istat)
                  _EXIT_(myname_)
                  return
                endif

  else
    read(iunit,iostat=istat)    aNode%res_lon , &
                                aNode%res_lat , &
                                aNode%err2_lon, &
                                aNode%err2_lat, &
                                aNode%raterr2 , &
                                aNode%b       , &
                                aNode%pg      , &
                                aNode%obslon  , &
                                aNode%obslat  , &
                                aNode%geslon  , &
                                aNode%geslat  , &
                                aNode%intnum  , &
                                aNode%speci   , & !(lag_rk2itenpara_i)
                                aNode%specr       !(lag_rk2itenpara_r)
                if (istat/=0) then
                  call perr(myname_,'read(%(res,err2,...)), istat =',istat)
                  _EXIT_(myname_)
                  return
                end if

    aNode%diag_lon => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,1_i_kind)
    aNode%diag_lat => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,2_i_kind)

                if(.not.( associated(aNode%diag_lon).and. &
                          associated(aNode%diag_lat)      ) ) then
                  call perr(myname_,'obsdiagLookup_locate(lon,lat), %idv =',aNode%idv)
                  call perr(myname_,'                               %iob =',aNode%iob)
                  if(.not.associated(aNode%diag_lon)) &
                  call perr(myname_,'       can not locate diag_lon, ich =',1_i_kind)
                  if(.not.associated(aNode%diag_lat)) &
                  call perr(myname_,'       can not locate diag_lat, ich =',2_i_kind)
                  call  die(myname_)
                endif
  endif
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  implicit none
  class(lagNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xwrite_'
_ENTRY_(myname_)

  jstat=0
  write(junit,iostat=jstat)     aNode%res_lon , &
                                aNode%res_lat , &
                                aNode%err2_lon, &
                                aNode%err2_lat, &
                                aNode%raterr2 , &
                                aNode%b       , &
                                aNode%pg      , &
                                aNode%obslon  , &
                                aNode%obslat  , &
                                aNode%geslon  , &
                                aNode%geslat  , &
                                aNode%intnum  , &
                                aNode%speci   , & !(lag_rk2itenpara_i)
                                aNode%specr       !(lag_rk2itenpara_r)
                if (jstat/=0) then
                  call perr(myname_,'write(%(res,err2,...)), jstat =',jstat)
                  _EXIT_(myname_)
                  return
                end if
_EXIT_(myname_)
return
end subroutine obsNode_xwrite_

subroutine obsNode_setHop_(aNode)
  use m_cvgridLookup, only: cvgridLookup_getiw
  implicit none
  class(lagNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
  real(r_kind) :: dum
_ENTRY_(myname_)
  !-- yet to be defined
  call perr(myname_,'nothing about setHop has been defined')
  call  die(myname_)
  !-- following is here to satisfy var-usage requirement
  dum=aNode%elat
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(lagNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
_ENTRY_(myname_)
  isvalid_=associated(aNode%diag_lat) .and. &
           associated(aNode%diag_lat)
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine gettlddp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(lagNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  tlddp = tlddp + aNode%diag_lat%tldepart(jiter)*aNode%diag_lat%tldepart(jiter)
  tlddp = tlddp + aNode%diag_lon%tldepart(jiter)*aNode%diag_lon%tldepart(jiter)
  if(present(nob)) nob=nob+2
return
end subroutine gettlddp_

end module m_lagNode
