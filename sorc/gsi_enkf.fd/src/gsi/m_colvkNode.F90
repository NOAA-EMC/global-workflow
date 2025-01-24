module m_colvkNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_colvkNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of obs-type colvkNode (sbuv co)
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

  public:: colvkNode

  type,extends(obsNode):: colvkNode
     !type(colvk_ob_type),pointer :: llpoint => NULL()
     type(aofp_obs_diag), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  co residual
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  co error squared
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  square of ratio of final obs error
                                      !  to original obs error
     !real(r_kind)    :: time          !  observation time in sec
     real(r_kind),dimension(  :),pointer :: wk  => NULL()
     real(r_kind),dimension(:,:),pointer :: wij => NULL()
                                      !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: prs => NULL()
                                      !  pressure levels
     integer(i_kind),dimension(:),pointer :: ipos  => NULL()
     real(r_kind),dimension(:,:),pointer :: ak  => NULL()   
                                      ! MOPITT vertical averaging kernel
     real(r_kind),dimension(:),pointer :: ap  => NULL()   
                                      ! MOPITT a priori
     !real(r_kind),dimension(:),pointer   :: wkk1 => NULL()
     !real(r_kind),dimension(:),pointer   :: wkk2 => NULL()
                                      ! vertical intropolation weights for MOPITT

     integer(i_kind) :: nlco =0       ! number of levels for this profile
     integer(i_kind) :: ij(4)=0       !  horizontal locations
     !logical         :: luse          !  flag indicating if ob is used in pen.

     !integer(i_kind) :: idv,iob         ! device id and obs index for sorting
     !real   (r_kind) :: elat, elon      ! earth lat-lon for redistribution
     !real   (r_kind) :: dlat, dlon      ! earth lat-lon for redistribution
  contains
    procedure,nopass::  mytype
    procedure::  setHop => obsNode_setHop_
    procedure::   xread => obsNode_xread_
    procedure::  xwrite => obsNode_xwrite_
    procedure:: isvalid => obsNode_isvalid_
    procedure::  getTLDdp => getTLDdp_

    ! procedure, nopass:: headerRead  => obsHeader_read_
    ! procedure, nopass:: headerWrite => obsHeader_write_
    ! procedure:: init  => obsNode_init_
    procedure:: clean => obsNode_clean_

  end type colvkNode

  public:: colvkNode_typecast
  public:: colvkNode_nextcast
        interface colvkNode_typecast; module procedure typecast_ ; end interface
        interface colvkNode_nextcast; module procedure nextcast_ ; end interface

  public:: colvkNode_appendto
        interface colvkNode_appendto; module procedure appendto_ ; end interface

  character(len=*),parameter:: MYNAME="m_colvkNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(colvkNode)
  use m_obsNode, only: obsNode
  implicit none
  type(colvkNode),pointer:: ptr_
  class(obsNode ),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(colvkNode)
    ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(colvkNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type(colvkNode),pointer:: ptr_
  class(obsNode ),target ,intent(in):: aNode

  class(obsNode),pointer:: inode_
  inode_ => obsNode_next(aNode)
  ptr_ => typecast_(inode_)
return
end function nextcast_

subroutine appendto_(aNode,oll)
  use m_obsNode , only: obsNode
  use m_obsLList, only: obsLList,obsLList_appendNode
  implicit none
  type(colvkNode),pointer,intent(in):: aNode
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
  mytype="[colvkNode]"
end function mytype

subroutine obsNode_clean_(aNode)
  implicit none
  class(colvkNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_clean_'
_ENTRY_(myname_)
!_TRACEV_(myname_,'%mytype() =',aNode%mytype())
    if(associated(aNode%res    )) deallocate(aNode%res    )
    if(associated(aNode%diags  )) deallocate(aNode%diags  )
    if(associated(aNode%err2   )) deallocate(aNode%err2   )
    if(associated(aNode%raterr2)) deallocate(aNode%raterr2)
    if(associated(aNode%prs    )) deallocate(aNode%prs    )
    if(associated(aNode%ipos   )) deallocate(aNode%ipos   )
    if(associated(aNode%ak     )) deallocate(aNode%ak     )
    if(associated(aNode%ap     )) deallocate(aNode%ap     )
    if(associated(aNode%wk     )) deallocate(aNode%wk     )
    if(associated(aNode%wij    )) deallocate(aNode%wij    )

_EXIT_(myname_)
return
end subroutine obsNode_clean_

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use gridmod, only: nsig
  use m_obsdiagNode, only: obsdiagLookup_locate
  implicit none
  class(colvkNode) , intent(inout):: aNode
  integer(i_kind) , intent(in   ):: iunit
  integer(i_kind) , intent(  out):: istat
  type(obs_diags) , intent(in   ):: diagLookup
  logical,optional, intent(in   ):: skip

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xread_'
  integer(i_kind),allocatable,dimension(:):: ich
  integer(i_kind):: k,nlco,nlevp
  logical:: skip_
_ENTRY_(myname_)
  skip_=.false.
  if(present(skip)) skip_=skip

  istat=0
  if(skip_) then
    read(iunit,iostat=istat)
                if (istat/=0) then
                  call perr(myname_,'skipping read(%nlco), istat =',istat)
                  _EXIT_(myname_)
                  return
                end if
    read(iunit,iostat=istat)
                if(istat/=0) then
                  call perr(myname_,'skipping read(%(res,err2,...)), istat =',istat)
                  _EXIT_(myname_)
                  return
                endif

  else
    read(iunit,iostat=istat)    aNode%nlco
                if (istat/=0) then
                  call perr(myname_,'read(%nlco), istat =',istat)
                  _EXIT_(myname_)
                  return
                end if

        call die(myname_,'unfinished implementation here')
        ! The size of %wij(8,:) seems in a mess.  It is nlev, nlevs, or nsig?
        ! See setupco() for additional details.

                ! re-allocation is needed, because a node may have been read in
                ! but excluded later.
        if(associated(aNode%res    )) deallocate(aNode%res    )
        if(associated(aNode%diags  )) deallocate(aNode%diags  )
        if(associated(aNode%err2   )) deallocate(aNode%err2   )
        if(associated(aNode%raterr2)) deallocate(aNode%raterr2)
        if(associated(aNode%prs    )) deallocate(aNode%prs    )
        if(associated(aNode%ipos   )) deallocate(aNode%ipos   )
        if(associated(aNode%ak     )) deallocate(aNode%ak     )
        if(associated(aNode%ap     )) deallocate(aNode%ap     )
        if(associated(aNode%wk     )) deallocate(aNode%wk     )
        if(associated(aNode%wij    )) deallocate(aNode%wij    )

    nlco = aNode%nlco
    nlevp= max(nlco,1)

        allocate( aNode%res    (nlco) , &
                  aNode%diags  (nlco) , &
                  aNode%err2   (nlco) , &
                  aNode%raterr2(nlco) , &
                  aNode%prs    (nlevp), &
                  aNode%ipos   (nlco) , & 
                  aNode%ak(nlco,nlco) , & 
                  aNode%ap     (nlco) , & 
                  aNode%wk     (nsig) , &
                  aNode%wij  (8,nsig)   )

        allocate(       ich    (nlco)   )

    read(iunit,iostat=istat)          ich    , & !(nlco)
                                aNode%res    , & !(nlco)
                                aNode%err2   , & !(nlco)
                                aNode%raterr2, & !(nlco)
                                aNode%prs    , & !(nlevp)
                                aNode%ipos   , & !(nlco)
                                aNode%ak     , & !(nlco,nlco)
                                aNode%ap     , & !(nlco)
                                aNode%wk     , & !(  nsig)
                                aNode%wij    , & !(8,nsig)
                                aNode%ij         !(4)
                if (istat/=0) then
                  call perr(myname_,'read(ich,%(...)), istat =',istat)
                  _EXIT_(myname_)
                  return
                end if

    do k=1,nlco
      ! Not sure if ich=k or ich=ich(k) for now.  More verification is needed.
      aNode%diags(k)%ptr => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,ich(k))
                if(.not.associated(aNode%diags(k)%ptr)) then
                  call perr(myname_,'obsdiagLookup_locate(k), k =',aNode%idv)
                  call perr(myname_,'                      %idv =',aNode%idv)
                  call perr(myname_,'                      %iob =',aNode%iob)
                  call perr(myname_,'                       ich =',ich(k))
                  call  die(myname_)
                  istat=-1
                  _EXIT_(myname_)
                  return
                endif
    enddo
    deallocate(ich)
  endif
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  implicit none
  class(colvkNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xwrite_'
  integer(i_kind):: k
_ENTRY_(myname_)

  jstat=0
  write(junit,iostat=jstat) aNode%nlco
                if (jstat/=0) then
                  call perr(myname_,'write(%nlco), jstat =',jstat)
                  _EXIT_(myname_)
                  return
                end if

  write(junit,iostat=jstat) (/ (aNode%diags(k)%ptr%ich, k=1,aNode%nlco) /), & !(nlco)
                                aNode%res    , & !(nlco)
                                aNode%err2   , & !(nlco)
                                aNode%raterr2, & !(nlco)
                                aNode%prs    , & !(nlevp)
                                aNode%ipos   , & !(nlco)
                                aNode%ak     , & !(nlco,nlco)
                                aNode%ap     , & !(nlco)
                                aNode%wk     , & !(  nsig)
                                aNode%wij    , & !(8,nsig)
                                aNode%ij         !(4)
                if (jstat/=0) then
                  call perr(myname_,'write(%(res,err2,...)), istat =',jstat)
                  _EXIT_(myname_)
                  return
                end if
_EXIT_(myname_)
return
end subroutine obsNode_xwrite_

subroutine obsNode_setHop_(aNode)
  !use m_obsNode, only: obstype_getHop => obsNode_getHop
  use m_cvgridLookup, only: cvgridLookup_getiw
  use gridmod, only: nsig
  implicit none
  class(colvkNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
  real(r_kind),dimension(4):: zwij
  integer(i_kind):: k
_ENTRY_(myname_)
  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%ij,zwij)
  do k=1,nsig
    aNode%wij(1:4,k) = zwij(1:4)*aNode%wk(k)
    aNode%wij(5:8,k) = zwij(1:4)*(1._r_kind-aNode%wk(k))
  enddo
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(colvkNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
  integer(i_kind):: k
_ENTRY_(myname_)
  isvalid_ = all( (/ (associated(aNode%diags(k)%ptr), k=1,aNode%nlco) /) )
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine getTLDdp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(colvkNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  integer(kind=i_kind):: k
  do k=1,aNode%nlco
    tlddp = tlddp + aNode%diags(k)%ptr%tldepart(jiter)*aNode%diags(k)%ptr%tldepart(jiter)
  enddo
  if(present(nob)) nob=nob+aNode%nlco
return
end subroutine getTLDdp_

end module m_colvkNode
