module m_ozNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_ozNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of obs-type ozNode (ozone layer amounts and total column)
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

  public:: ozNode

  type,extends(obsNode):: ozNode
     !type(oz_ob_type),pointer :: llpoint => NULL()
     type(aofp_obs_diag), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  ozone residual
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  ozone error squared
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  square of ratio of final obs error 
                                      !  to original obs error
     !real(r_kind)    :: time          !  observation time in sec     
     real(r_kind),dimension(:,:),pointer :: wij => NULL()
                                      !  horizontal interpolation weights
     real(r_kind),dimension(:),pointer :: prs => NULL()
                                      !  pressure levels
     integer(i_kind),dimension(:),pointer :: ipos  => NULL()
     integer(i_kind) :: nloz          ! number of levels for this profile
     integer(i_kind) :: ij(4)         !  horizontal locations
     !logical         :: luse          !  flag indicating if ob is used in pen.

     !integer(i_kind) :: idv,iob	      ! device id and obs index for sorting
     !real   (r_kind) :: elat, elon      ! earth lat-lon for redistribution
     !real   (r_kind) :: dlat, dlon      ! earth lat-lon for redistribution
     real   (r_kind) :: rozcon          ! see setupozlay() for about rozcon
     real   (r_kind),dimension(:),pointer :: dprsi           ! see setupozlay() for about prsitmp(k)-prsitmp(k+1)

     logical:: has_eff
     real(r_kind),dimension(:),pointer :: apriori    ! OMI retrieval first guess
     real(r_kind),dimension(:),pointer :: efficiency ! OMI efficiency factor
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
    procedure:: clean => obsNode_clean_
  end type ozNode

  public:: ozNode_typecast
  public:: ozNode_nextcast
        interface ozNode_typecast; module procedure typecast_ ; end interface
        interface ozNode_nextcast; module procedure nextcast_ ; end interface

  public:: ozNode_appendto
        interface ozNode_appendto; module procedure appendto_ ; end interface

  character(len=*),parameter:: MYNAME="m_ozNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(ozNode)
  use m_obsNode, only: obsNode
  implicit none
  type (ozNode ),pointer:: ptr_
  class(obsNode),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(ozNode)
    ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(ozNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type (ozNode ),pointer:: ptr_
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
  type(ozNode),pointer,intent(in):: aNode
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
  mytype="[ozNode]"
end function mytype

subroutine obsNode_clean_(aNode)
  implicit none
  class(ozNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_clean_'
_ENTRY_(myname_)
!_TRACEV_(myname_,'%mytype() =',aNode%mytype())
        if(associated(aNode%diags  )) deallocate(aNode%diags  )
        if(associated(aNode%res    )) deallocate(aNode%res    )
        if(associated(aNode%err2   )) deallocate(aNode%err2   )
        if(associated(aNode%raterr2)) deallocate(aNode%raterr2)
        if(associated(aNode%prs    )) deallocate(aNode%prs    )
        if(associated(aNode%ipos   )) deallocate(aNode%ipos   )
        if(associated(aNode%apriori)) deallocate(aNode%apriori)
        if(associated(aNode%efficiency)) deallocate(aNode%efficiency)
        if(associated(aNode%dprsi  )) deallocate(aNode%dprsi  )
        if(associated(aNode%wij    )) deallocate(aNode%wij    )
_EXIT_(myname_)
return
end subroutine obsNode_clean_

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use m_obsdiagNode, only: obsdiagLookup_locate
  use gridmod, only: nsig
  implicit none
  class(ozNode) , intent(inout):: aNode
  integer(i_kind) , intent(in   ):: iunit
  integer(i_kind) , intent(  out):: istat
  type(obs_diags) , intent(in   ):: diagLookup
  logical,optional, intent(in   ):: skip

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_xread_'
  integer(i_kind):: k,mloz,mlev,mlevp,meff,msig
  integer(i_kind),allocatable,dimension(:):: ich
  logical:: skip_
_ENTRY_(myname_)

  skip_=.false.
  if(present(skip)) skip_=skip

  istat=0
  if(skip_) then
    read(iunit,iostat=istat)
                if (istat/=0) then
                  call perr(myname_,'skipping read(%nloz), istat =',istat)
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
    read(iunit,iostat=istat)    aNode%nloz,mlev,mlevp,meff,msig
                if (istat/=0) then
                  call perr(myname_,'read(%nloz), istat =',istat)
                  _EXIT_(myname_)
                  return
                end if

        ASSERT(mlev==aNode%nloz+1)
        ASSERT(msig==nsig)

        if(associated(aNode%diags  )) deallocate(aNode%diags  )
        if(associated(aNode%res    )) deallocate(aNode%res    )
        if(associated(aNode%err2   )) deallocate(aNode%err2   )
        if(associated(aNode%raterr2)) deallocate(aNode%raterr2)
        if(associated(aNode%prs    )) deallocate(aNode%prs    )
        if(associated(aNode%ipos   )) deallocate(aNode%ipos   )
        if(associated(aNode%apriori)) deallocate(aNode%apriori)
        if(associated(aNode%efficiency)) deallocate(aNode%efficiency)
        if(associated(aNode%dprsi  )) deallocate(aNode%dprsi  )
        if(associated(aNode%wij    )) deallocate(aNode%wij    )

        allocate( aNode%diags  (mlev ), &
                  aNode%res    (mlev ), &
                  aNode%err2   (mlev ), &
                  aNode%raterr2(mlev ), &
                  aNode%prs    (mlevp), &
                  aNode%ipos   (mlev ), &
                  aNode%apriori(meff ), &
                  aNode%efficiency(meff), &
                  aNode%wij  (4,msig ), &
                  aNode%dprsi(  msig )  )

        allocate(ich(mlev))

    read(iunit,iostat=istat)          ich    , &
                                aNode%res    , &
                                aNode%err2   , &
                                aNode%raterr2, &
                                aNode%prs    , &
                                aNode%ipos   , &
                                aNode%apriori, &
                                aNode%efficiency, &
                                aNode%rozcon , &
                                aNode%dprsi  , &
                                aNode%wij    , &
                                aNode%ij
                if (istat/=0) then
                  call perr(myname_,'read(%(res,err2,...)), istat =',istat)
                  call perr(myname_,'                        meff =',meff)
                  call perr(myname_,'                        mloz =',mloz)
                  call perr(myname_,'                        mlev =',mlev)
                  call perr(myname_,'                       mlevp =',mlevp)
                  call perr(myname_,'                        msig =',msig)
                  _EXIT_(myname_)
                  return
                end if

    do k=1,mlev
      aNode%diags(k)%ptr => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,ich(k))
                if(.not.associated(aNode%diags(k)%ptr)) then
                  call perr(myname_,'obsdiagLookup_locate(k), k =',k)
                  call perr(myname_,'                      %idv =',aNode%idv)
                  call perr(myname_,'                      %iob =',aNode%iob)
                  call perr(myname_,'                       ich =',ich(k))
                  call  die(myname_)
                endif
    enddo
    deallocate(ich)
  endif
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  implicit none
  class(ozNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_xwrite_'
  integer(i_kind):: k,mlev,mlevp,meff,msig
_ENTRY_(myname_)

  mlev =size(aNode%diags)
  mlevp=size(aNode%prs)
        if(mlev/=aNode%nloz+1) then
          call perr(myname_,'mlev/=aNode%nloz+1, mlev =',mlev)
          call perr(myname_,'                   mlevp =',mlevp)
          call perr(myname_,'                   %nloz =',aNode%nloz)
          call  die(myname_)
        endif
        ASSERT(mlev==aNode%nloz+1)

  meff =size(aNode%apriori)
  msig =size(aNode%dprsi)

  write(junit,iostat=jstat) aNode%nloz,mlev,mlevp,meff,msig
                if(jstat/=0) then
                  call perr(myname_,'write(%nloz,...)), jstat =',jstat)
                  _EXIT_(myname_)
                  return
                endif

  write(junit,iostat=jstat) (/ (aNode%diags(k)%ptr%ich,k=1,mlev)/), &
                                aNode%res    , & ! nlev
                                aNode%err2   , & ! nlev
                                aNode%raterr2, & ! nlev
                                aNode%prs    , & ! nlevp
                                aNode%ipos   , & !
                                aNode%apriori, &
                                aNode%efficiency, &
                                aNode%rozcon , &
                                aNode%dprsi  , &
                                aNode%wij    , &
                                aNode%ij
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
  use gridmod, only: nsig
  implicit none
  class(ozNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
  real(r_kind),dimension(4):: twij
  integer(i_kind):: k
_ENTRY_(myname_)
  ASSERT(size(aNode%wij,2)==nsig)
  ASSERT(nsig>0)

  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%ij,twij(:))
  do k=1,nsig
    aNode%wij(:,k)=twij(:)*aNode%rozcon*aNode%dprsi(k)
  enddo
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(ozNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
  integer(i_kind):: k
_ENTRY_(myname_)
  isvalid_= all ( (/ (associated(aNode%diags(k)%ptr), k=1,aNode%nloz) /) )
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine gettlddp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(ozNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  integer(kind=i_kind):: k
  do k=1,aNode%nloz
    tlddp = tlddp + aNode%diags(k)%ptr%tldepart(jiter)*aNode%diags(k)%ptr%tldepart(jiter)
  enddo
  if(present(nob)) nob=nob+aNode%nloz
return
end subroutine gettlddp_

end module m_ozNode
