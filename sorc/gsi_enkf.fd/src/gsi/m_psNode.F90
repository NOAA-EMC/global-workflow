module m_psNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_psNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of obs-type psNode (surface pressure)
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
  use kinds , only: i_kind,r_kind
  use mpeu_util, only: assert_,die,perr,warn,tell
  use m_obsdiagNode, only: obs_diag
  use m_obsNode, only: obsNode
  implicit none
  private

  public:: psNode

  type,extends(obsNode):: psNode
     !private
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res    =0._r_kind    !  surface pressure residual
     real(r_kind)    :: err2   =0._r_kind    !  surface pressure error squared
                                             !  in reciprocal
     real(r_kind)    :: raterr2=0._r_kind    !  square of ratio of final obs error 
                                             !  to original obs error
     real(r_kind)    :: b      =0._r_kind    !  variational quality control parameter
     real(r_kind)    :: pg     =0._r_kind    !  variational quality control parameter
     real(r_kind)    :: jb     =0._r_kind    !  variational quality control parameter
     integer(i_kind) :: ib     =0_i_kind     !  new variational quality control parameter
     integer(i_kind) :: ik     =0_i_kind     !  new variational quality control parameter
     real(r_kind)    :: wij(4) =0._r_kind    !  horizontal interpolation weights
     real(r_kind)    :: ppertb =0._r_kind    !  random number adding to the obs
     integer(i_kind) :: ij(4)  =0_i_kind     !  horizontal locations
     integer(i_kind) :: kx     =0_i_kind     !  ob type

     !type(ps_ob_type),pointer :: llpoint => NULL()
     !real(r_kind)    :: time          !  observation time in sec     
     !logical         :: luse          !  flag indicating if ob is used in pen.
     !integer(i_kind) :: idv,iob              ! device id and obs index for sorting
     !real   (r_kind) :: elat, elon      ! earth lat-lon for redistribution
     !real   (r_kind) :: dlat, dlon      ! earth lat-lon for redistribution
  contains
    procedure,nopass::  mytype
    procedure::  setHop => obsNode_setHop_
    procedure::   xread => obsNode_xread_
    procedure::  xwrite => obsNode_xwrite_
    procedure:: isvalid => obsNode_isvalid_
    procedure::gettlddp => gettlddp_

    ! procedure, nopass:: headerRead  => obsHeader_read_
    ! procedure, nopass:: headerWrite => obsHeader_write_
    ! procedure:: init  => obsNode_init_
    ! procedure:: clean => obsNode_clean_
  end type psNode

  public:: psNode_typecast
  public:: psNode_nextcast
        interface psNode_typecast; module procedure typecast_ ; end interface
        interface psNode_nextcast; module procedure nextcast_ ; end interface

  public:: psNode_appendto
        interface psNode_appendto; module procedure appendto_ ; end interface

  character(len=*),parameter:: MYNAME="m_psNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(psNode)
  use m_obsNode, only: obsNode
  implicit none
  type (psNode ),pointer:: ptr_
  class(obsNode),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(psNode)
    ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(psNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type (psNode ),pointer:: ptr_
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
  type(psNode),pointer,intent(in):: aNode
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
  mytype="[psNode]"
end function mytype

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use m_obsdiagNode, only: obsdiagLookup_locate
  use m_obsdiagNode, only: obs_diags
  implicit none
  class(psNode), intent(inout):: aNode
  integer(i_kind), intent(in   ):: iunit
  integer(i_kind), intent(  out):: istat
  type(obs_diags), intent(in   ):: diagLookup
  logical,optional,intent(in   ):: skip

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xread_'
  logical:: skip_
_ENTRY_(myname_)

  skip_=.false.
  if(present(skip)) skip_=skip

  if(skip_) then
    read(iunit,iostat=istat)
                if(istat/=0) then
                  call perr(myname_,'skipping read(%(res,...)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                endif

  else
    read(iunit,iostat=istat)    aNode%res    , &
                                aNode%err2   , &
                                aNode%raterr2, &
                                aNode%b      , &
                                aNode%pg     , &
                                aNode%jb     , &
                                aNode%ib     , &
                                aNode%ik     , &
                                aNode%ppertb , &
                                aNode%kx     , &
                                aNode%wij    , &
                                aNode%ij
                if (istat/=0) then
                  call perr(myname_,'read(%(res,...)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if

    aNode%diags => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,1_i_kind)
                if(.not.  associated(aNode%diags)) then
                  call perr(myname_,'obsdiagLookup_locate(), %idv =',aNode%idv)
                  call perr(myname_,'                        %iob =',aNode%iob)
                  call  die(myname_)
                endif
  endif ! if(skip_); else
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  implicit none
  class(psNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xwrite_'
_ENTRY_(myname_)

  write(junit,iostat=jstat)     aNode%res    , &
                                aNode%err2   , &
                                aNode%raterr2, &
                                aNode%b      , &
                                aNode%pg     , &
                                aNode%jb     , &
                                aNode%ib     , &
                                aNode%ik     , &
                                aNode%ppertb , &
                                aNode%kx     , &
                                aNode%wij    , &
                                aNode%ij
                if (jstat/=0) then
                  call perr(myname_,'write(%(res,...)), iostat =',jstat)
                  _EXIT_(myname_)
                  return
                end if
_EXIT_(myname_)
return
end subroutine obsNode_xwrite_

subroutine obsNode_setHop_(aNode)
  use m_cvgridLookup, only: cvgridLookup_getiw
  implicit none
  class(psNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
_ENTRY_(myname_)
  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%ij,aNode%wij)
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(psNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
_ENTRY_(myname_)
  isvalid_= associated(aNode%diags)
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine gettlddp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(psNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  tlddp = tlddp + aNode%diags%tldepart(jiter)*aNode%diags%tldepart(jiter)
  if(present(nob)) nob=nob+1
return
end subroutine gettlddp_

end module m_psNode
