module m_wNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_wNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of obs-type wNode (wind components)
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

  public:: wNode

  type,extends(obsNode):: wNode
     !type(w_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diagu => NULL()
     type(obs_diag), pointer :: diagv => NULL()
     real(r_kind)    :: ures          !  u component residual
     real(r_kind)    :: vres          !  v component residual
     real(r_kind)    :: err2          !  surface pressure error squared
     real(r_kind)    :: raterr2       !  square of ratio of final obs error 
     !real(r_kind)    :: time          !  observation time in sec     
     real(r_kind)    :: b             !  variational quality control parameter
     real(r_kind)    :: pg            !  variational quality control parameter
     real(r_kind)    :: jb            !  variational quality control parameter
     integer(i_kind) :: ib            !  new variational quality control parameter
     integer(i_kind) :: ik            !  new variational quality control parameter
     real(r_kind)    :: wij(8)        !  horizontal interpolation weights
     real(r_kind)    :: upertb        !  random number adding to the obs
     real(r_kind)    :: vpertb        !  random number adding to the obs
     integer(i_kind) :: ij(8)         !  horizontal locations
     integer(i_kind) :: k1            !  level of errtable 1-33
     integer(i_kind) :: kx            !  ob type
     !logical         :: luse          !  flag indicating if ob is used in pen.

     !integer(i_kind) :: idv,iob	      ! device id and obs index for sorting
     !real   (r_kind) :: elat, elon      ! earth lat-lon for redistribution
     !real   (r_kind) :: dlat, dlon      ! earth lat-lon for redistribution
     real   (r_kind) :: dlev            ! reference to the vertical grid
     real   (r_kind) :: factw           ! factor of 10m wind

     integer(i_kind) :: ich0=0  ! ich code to mark derived data.  See
                                ! wNode_ich0 and wNode_ich0_PBL_Pseudo below
  contains
    procedure,nopass::  mytype
    procedure::  setHop => obsNode_setHop_
    procedure::   xread => obsNode_xread_
    procedure::  xwrite => obsNode_xwrite_
    procedure:: isvalid => obsNode_isvalid_
    procedure::  gettlddp => gettlddp_

    ! procedure, nopass:: headerRead  => obsHeader_read_
    ! procedure, nopass:: headerWrite => obsHeader_write_
    ! procedure:: init  => obsNode_init_
    ! procedure:: clean => obsNode_clean_
  end type wNode

  public:: wNode_typecast
  public:: wNode_nextcast
        interface wNode_typecast; module procedure typecast_ ; end interface
        interface wNode_nextcast; module procedure nextcast_ ; end interface

  public:: wNode_appendto
        interface wNode_appendto; module procedure appendto_ ; end interface

  ! Because there are two components in wNode for an ordinary wind obs,
  ! ich values are set to (1,2).  Therefore, ich values for PBL_pseudo_surfobsUV
  ! are set to (3,4), and wNode_ich0_pbl_pseudo is set to 2.

  public:: wNode_ich0
  public:: wNode_ich0_PBL_pseudo
        integer(i_kind),parameter :: wNode_ich0            = 0            ! (1,2)
        integer(i_kind),parameter :: wNode_ich0_PBL_pseudo = wNode_ich0+2 ! (3,4)

  character(len=*),parameter:: MYNAME="m_wNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(wNode)
  use m_obsNode, only: obsNode
  implicit none
  type(wNode   ),pointer:: ptr_
  class(obsNode),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(wNode)
    ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(wNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type(wNode   ),pointer:: ptr_
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
  type(wNode),pointer,intent(in):: aNode
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
  mytype="[wNode]"
end function mytype

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use m_obsdiagNode, only: obsdiagLookup_locate
  implicit none
  class(wNode),intent(inout):: aNode
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
    read(iunit,iostat=istat)    aNode%ures   , &
                                aNode%vres   , &
                                aNode%err2   , &
                                aNode%raterr2, &
                                aNode%b      , &
                                aNode%pg     , &
                                aNode%jb     , &
                                aNode%ib     , &
                                aNode%ik     , &
                                aNode%upertb , &
                                aNode%vpertb , &
                                aNode%k1     , &
                                aNode%kx     , &
                                aNode%dlev   , &
                                aNode%factw  , &
                                aNode%ich0   , &
                                aNode%wij    , &
                                aNode%ij
                if (istat/=0) then
                  call perr(myname_,'read(%(res,err2,...)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if

    aNode%diagu => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,aNode%ich0+1_i_kind)
    aNode%diagv => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,aNode%ich0+2_i_kind)

                if(.not. (associated(aNode%diagu) .and. &
                          associated(aNode%diagv) )     ) then
                  call perr(myname_,'obsdiagLookup_locate(u,v), %idv =',aNode%idv)
                  call perr(myname_,'                           %iob =',aNode%iob)
                  call perr(myname_,'                          %ich0 =',aNode%ich0)
                  if(.not.associated(aNode%diagu)) &
                  call perr(myname_,'   .not.associated(%diagu), ich =',aNode%ich0+1_i_kind)
                  if(.not.associated(aNode%diagv)) &
                  call perr(myname_,'   .not.associated(%diagv), ich =',aNode%ich0+2_i_kind)
                  call  die(myname_)
                endif
  endif
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  implicit none
  class(wNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_xwrite_'
_ENTRY_(myname_)

  jstat=0
  write(junit,iostat=jstat)     aNode%ures   , &
                                aNode%vres   , &
                                aNode%err2   , &
                                aNode%raterr2, &
                                aNode%b      , &
                                aNode%pg     , &
                                aNode%jb     , &
                                aNode%ib     , &
                                aNode%ik     , &
                                aNode%upertb , &
                                aNode%vpertb , &
                                aNode%k1     , &
                                aNode%kx     , &
                                aNode%dlev   , &
                                aNode%factw  , &
                                aNode%ich0   , &
                                aNode%wij    , &
                                aNode%ij
                if (jstat/=0) then
                  call perr(myname_,'write(%(res,err2,...)), iostat =',jstat)
                  _EXIT_(myname_)
                  return
                end if
_EXIT_(myname_)
return
end subroutine obsNode_xwrite_

subroutine obsNode_setHop_(aNode)
  use m_cvgridLookup, only: cvgridLookup_getiw
  implicit none
  class(wNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
_ENTRY_(myname_)
  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%dlev,aNode%ij,aNode%wij)
  aNode%wij(1:8) = aNode%wij(1:8)*aNode%factw
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(wNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
_ENTRY_(myname_)
  isvalid_=associated(aNode%diagu) .and. &
           associated(aNode%diagv)
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine gettlddp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(wNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  tlddp = tlddp + aNode%diagu%tldepart(jiter)*aNode%diagu%tldepart(jiter)
  tlddp = tlddp + aNode%diagv%tldepart(jiter)*aNode%diagv%tldepart(jiter)
  if(present(nob)) nob=nob+2
return
end subroutine gettlddp_

end module m_wNode
