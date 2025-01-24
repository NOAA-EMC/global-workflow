module m_fedNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_fedNode
!   prgmmr:      YPW <yaping.wang@ou.ed>
!      org:      CIMMS 
!     date:      2019-09-24
!
! abstract: class-module of obs-type fedNode (GLM flash extent density)
! Modified based on m_tdNode.f90
!
! program history log:
!   2019-09-24  YPW   - added this document block for the initial polymorphic
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

  public:: fedNode

  type,extends(obsNode):: fedNode
     type(obs_diag), pointer :: diags => NULL() 
     real(r_kind)    :: res           !  flash extent density residual
     real(r_kind)    :: err2          !  flash extent density error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
                                      !  to original obs error
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: jb            !  variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: fedpertb        !  random number adding to the obs
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     integer(i_kind) :: ij(8)         !  horizontal locations

     real   (r_kind) :: dlev            ! reference to the vertical grid
  contains
    procedure,nopass::  mytype
    procedure::  setHop => obsNode_setHop_
    procedure::   xread => obsNode_xread_
    procedure::  xwrite => obsNode_xwrite_
    procedure:: isvalid => obsNode_isvalid_
    procedure::  gettlddp => gettlddp_

  end type fedNode

  public:: fedNode_typecast
  public:: fedNode_nextcast
        interface fedNode_typecast; module procedure typecast_ ; end interface
        interface fedNode_nextcast; module procedure nextcast_ ; end interface

  public:: fedNode_appendto
        interface fedNode_appendto; module procedure appendto_ ; end interface

  character(len=*),parameter:: MYNAME="m_fedNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(tdNode)
  use m_obsNode, only: obsNode
  implicit none
  type(fedNode),pointer:: ptr_
  class(obsNode),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
  select type(aNode)
    type is(fedNode)
      ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(fedNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type(fedNode),pointer:: ptr_
  class(obsNode),target,intent(in):: aNode

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
  type(fedNode),pointer,intent(in):: aNode
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
  mytype="[fedNode]"
end function mytype


subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use m_obsdiagNode, only: obsdiagLookup_locate
  implicit none
  class(fedNode),intent(inout):: aNode
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
      read(iunit,iostat=istat)  aNode%res       , &
                                aNode%err2      , &
                                aNode%raterr2   , &
                                aNode%b         , &
                                aNode%pg        , &
                                aNode%jb        , &
                                aNode%k1        , &
                                aNode%kx        , &
                                aNode%dlev      , &
                                aNode%wij       , &
                                aNode%ij
                if(istat/=0) then
                  call perr(myname_,'read(%(res,err2,...), iostat =',istat)
                  _EXIT_(myname_)
                  return
                endif

    aNode%diags => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,1_i_kind)
                if(.not.associated(aNode%diags)) then
                  call perr(myname_,'obsdiagLookup_locate(), %idv =',aNode%idv)
                  call perr(myname_,'                        %iob =',aNode%iob)
                  call  die(myname_)
                endif
  endif
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  implicit none
  class(fedNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_xwrite_'
_ENTRY_(myname_)

  jstat=0
  write(junit,iostat=jstat)   aNode%res       , &
                                aNode%err2      , &
                                aNode%raterr2   , &
                                aNode%b         , &
                                aNode%pg        , &
                                aNode%jb        , &
                                aNode%k1        , &
                                aNode%kx        , &
                                aNode%dlev      , &
                                aNode%wij       , &
                                aNode%ij
                if(jstat/=0) then
                  call perr(myname_,'write(%res,err2,...), iostat =',jstat)
                  _EXIT_(myname_)
                  return
                endif
_EXIT_(myname_)
return
end subroutine obsNode_xwrite_

subroutine obsNode_setHop_(aNode)
  use m_cvgridLookup, only: cvgridLookup_getiw
  implicit none
  class(fedNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
_ENTRY_(myname_)
  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%dlev,aNode%ij,aNode%wij)
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(fedNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
_ENTRY_(myname_)
  isvalid_=associated(aNode%diags)
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine gettlddp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(fedNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  tlddp = tlddp + aNode%diags%tldepart(jiter)*aNode%diags%tldepart(jiter)
  if(present(nob)) nob=nob+1
return
end subroutine gettlddp_

end module m_fedNode
