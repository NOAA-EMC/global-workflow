module m_aeroNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_aeroNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of obs-type aeroNode (aerosal AOD)
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
  use m_obsdiagNode, only: obs_diag,aofp_obs_diag => fptr_obsdiagNode
  use m_obsdiagNode, only: obs_diags

  use kinds , only: i_kind,r_kind
  use mpeu_util, only: assert_,die,perr,warn,tell
  use m_obsNode, only: obsNode
  implicit none
  private

  public:: aeroNode

  type,extends(obsNode):: aeroNode
     !type(aero_ob_type),pointer :: llpoint => NULL()
     type(aofp_obs_diag), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer    :: res  => NULL()    !  aerosol property residual
     real(r_kind),dimension(:),pointer    :: err2 => NULL()    !  aerosol property error squared
     real(r_kind),dimension(:),pointer    :: raterr2 => NULL() !  square of ratio of final obs error
                                                               !  to original obs error
     !real(r_kind)                         :: time             !  observation time in sec
     real(r_kind)    :: wij(4)  =0._r_kind                     !  horizontal interpolation weights
     real(r_kind),dimension(:,:),pointer :: daod_dvar => NULL()! jacobians_aero (nsig*n_aerosols,nchan)
     real(r_kind),dimension(:),pointer    :: prs => NULL()     !  pressure levels
     integer(i_kind),dimension(:),pointer :: ipos  => NULL()
     integer(i_kind),dimension(:),pointer :: icx  => NULL()
     integer(i_kind) :: ij(4)   =0                             !  horizontal locations
     integer(i_kind) :: nlaero  =0                             !  number of channels
     !logical         :: luse                                   !  flag indicating if ob is used in pen.
     !integer(i_kind) :: idv,iob                                !  device id and obs index for sorting
     !real   (r_kind) :: elat, elon      ! earth lat-lon for redistribution
     !real   (r_kind) :: dlat, dlon      ! earth lat-lon for redistribution
     integer(i_kind),dimension(:),pointer :: ich => NULL()
  contains
    procedure,nopass::  mytype
    procedure::  setHop => obsNode_setHop_
    procedure::   xread => obsNode_xread_
    procedure::  xwrite => obsNode_xwrite_
    procedure:: isvalid => obsNode_isvalid_
    procedure:: getTLDdp => getTLDdp_

    ! procedure, nopass:: headerRead  => obsHeader_read_
    ! procedure, nopass:: headerWrite => obsHeader_write_
    ! procedure:: init  => obsNode_init_
    procedure:: clean => obsNode_clean_
  end type aeroNode

  public:: aeroNode_typecast
  public:: aeroNode_nextcast
        interface aeroNode_typecast; module procedure typecast_ ; end interface
        interface aeroNode_nextcast; module procedure nextcast_ ; end interface

  public:: aeroNode_appendto
        interface aeroNode_appendto; module procedure appendto_ ; end interface

  character(len=*),parameter:: MYNAME="m_aeroNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(aeroNode)
  use m_obsNode, only: obsNode
  implicit none
  type(aeroNode),pointer:: ptr_
  class(obsNode),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(aeroNode)
    ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(aeroNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type(aeroNode),pointer:: ptr_
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
  type(aeroNode),pointer,intent(in):: aNode
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
  mytype="[aeroNode]"
end function mytype

subroutine obsNode_clean_(aNode)
  implicit none
  class(aeroNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_clean_'
_ENTRY_(myname_)
!_TRACEV_(myname_,'%mytype() =',aNode%mytype())
        if(associated(aNode%daod_dvar)) deallocate(aNode%daod_dvar)
        if(associated(aNode%diags  )) deallocate(aNode%diags  )
        if(associated(aNode%res    )) deallocate(aNode%res    )
        if(associated(aNode%err2   )) deallocate(aNode%err2   )
        if(associated(aNode%raterr2)) deallocate(aNode%raterr2)
        if(associated(aNode%prs    )) deallocate(aNode%prs    )
        if(associated(aNode%ipos   )) deallocate(aNode%ipos   )
        if(associated(aNode%icx    )) deallocate(aNode%icx    )
_EXIT_(myname_)
return
end subroutine obsNode_clean_

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use aeroinfo, only: nsigaerojac
  use m_obsdiagNode, only: obsdiagLookup_locate
  implicit none
  class(aeroNode), intent(inout):: aNode
  integer(i_kind), intent(in   ):: iunit
  integer(i_kind), intent(  out):: istat
  type(obs_diags), intent(in   ):: diagLookup
  logical,optional,intent(in   ):: skip

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xread_'
  integer(i_kind),allocatable,dimension(:):: ich
  integer(i_kind):: nlaero,nlevp,k
  logical:: skip_
_ENTRY_(myname_)

  skip_=.false.
  if(present(skip)) skip_=skip

  if(skip_) then
    read(iunit,iostat=istat)
                if(istat/=0) then
                  call perr(myname_,'skipping read(%nlaero), iostat =',istat)
                  _EXIT_(myname_)
                  return
                endif

    read(iunit,iostat=istat)
                if(istat/=0) then
                  call perr(myname_,'skipping read(%(ich,...)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                endif

  else
    read(iunit,iostat=istat)    aNode%nlaero
                if (istat/=0) then
                  call perr(myname_,'read(%nlaero), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if

        if(associated(aNode%daod_dvar)) deallocate(aNode%daod_dvar)
        if(associated(aNode%diags  )) deallocate(aNode%diags  )
        if(associated(aNode%res    )) deallocate(aNode%res    )
        if(associated(aNode%err2   )) deallocate(aNode%err2   )
        if(associated(aNode%raterr2)) deallocate(aNode%raterr2)
        if(associated(aNode%prs    )) deallocate(aNode%prs    )
        if(associated(aNode%ipos   )) deallocate(aNode%ipos   )
        if(associated(aNode%icx    )) deallocate(aNode%icx    )

    nlaero=aNode%nlaero
    nlevp = max(nlaero,1)

        allocate( anode%daod_dvar(nsigaerojac,nlaero), &
                  aNode%diags(nlaero), &
                  aNode%res  (nlaero), &
                  aNode%err2 (nlaero), &
                  aNode%raterr2(nlaero), &
                  aNode%prs  (nlevp ), &
                  aNode%ipos (nlaero), &
                  aNode%icx  (nlaero)  )

        allocate(       ich  (nlaero)  )

    read(iunit,iostat=istat)          ich    , &
                                aNode%res    , &
                                aNode%err2   , &
                                aNode%raterr2, &
                                aNode%prs    , &
                                aNode%ipos   , &
                                aNode%icx    , &
                                aNode%daod_dvar, &
                                aNode%wij    , &
                                aNode%ij
                if (istat/=0) then
                  call perr(myname_,'read(ich,%(...)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if

    do k=1,aNode%nlaero
      aNode%diags(k)%ptr => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,ich(k))
                if(.not. associated(aNode%diags(k)%ptr)) then
                  call perr(myname_,'obsdiagLookup_locate(k), k =',k)
                  call perr(myname_,'                      %idv =',aNode%idv)
                  call perr(myname_,'                      %iob =',aNode%iob)
                  call perr(myname_,'                       ich =',ich(k))
                  call  die(myname_)
                endif
    enddo
        deallocate(ich)
    
  endif ! if(skip_); else
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  implicit none
  class(aeroNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xwrite_'
  integer(i_kind):: k
_ENTRY_(myname_)

  write(junit,iostat=jstat) aNode%nlaero
                if(jstat/=0) then
                  call perr(myname_,'write(%nlaero), iostat =',jstat)
                  _EXIT_(myname_)
                  return
                endif

  write(junit,iostat=jstat)  (/(aNode%diags(k)%ptr%ich,k=1,aNode%nlaero)/), & !(nlaero)
                                aNode%res    , & !(nlaero)
                                aNode%err2   , & !(nlaero)
                                aNode%raterr2, & !(nlaero)
                                aNode%prs    , & !(nlevp )
                                aNode%ipos   , & !(nlaero)
                                aNode%icx    , & !(nlaero)
                                aNode%daod_dvar, & !(nsigaerojac,nlaero)
                                aNode%wij    , &
                                aNode%ij
                if (jstat/=0) then
                  call perr(myname_,'write(ich,%(...)), iostat =',jstat)
                  _EXIT_(myname_)
                  return
                end if
_EXIT_(myname_)
return
end subroutine obsNode_xwrite_

subroutine obsNode_setHop_(aNode)
  use m_cvgridLookup, only: cvgridLookup_getiw
  implicit none
  class(aeroNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
_ENTRY_(myname_)
  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%ij,aNode%wij)
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(aeroNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
  integer(i_kind):: k
_ENTRY_(myname_)
  isvalid_= all ( (/ (associated(aNode%diags(k)%ptr), k=1,aNode%nlaero) /) )
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine getTLDdp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(aeroNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  integer(kind=i_kind):: k
  do k=1,aNode%nlaero
    tlddp = tlddp + aNode%diags(k)%ptr%tldepart(jiter)*aNode%diags(k)%ptr%tldepart(jiter)
  enddo
  if(present(nob)) nob=nob+aNode%nlaero
return
end subroutine getTLDdp_

end module m_aeroNode
