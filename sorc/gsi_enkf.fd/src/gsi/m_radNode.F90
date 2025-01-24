module m_radNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_radNode
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of obs-type radNode (radiances)
!
! program history log:
!   2016-05-18  j guo   - added this document block for the initial polymorphic
!                         implementation.
!   2016-07-19  kbathmann - add rsqrtinv and use_corr_obs to rad_ob_type
!   2019-04-22  kbathmann - change rsqrtinv to Rpred
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

  public:: radNode

  type,extends(obsNode):: radNode
     type(aofp_obs_diag), dimension(:), pointer :: diags => NULL()
     real(r_kind),dimension(:),pointer :: res => NULL()
                                      !  obs-guess residual (nchan)
     real(r_kind),dimension(:),pointer :: err2 => NULL()
                                      !  error variances squared (nchan)
     real(r_kind),dimension(:),pointer :: raterr2 => NULL()
                                      !  ratio of error variances squared (nchan)
     real(r_kind)    :: wij(4)        !  horizontal interpolation weights
     real(r_kind),dimension(:,:),pointer :: pred => NULL()
                                      !  predictors (npred,nchan)
     real(r_kind),dimension(:,:),pointer :: dtb_dvar => NULL()
                                      !  radiance jacobian (nsigradjac,nchan)

     real(r_kind),dimension(:,:),pointer :: Rpred => NULL()
                                      !  square root of inverse of R, multiplied
                                      !  by bias predictor jacobian
                                      !  only used if using correlated obs
     real(r_kind),dimension(:  ),pointer :: rsqrtinv => NULL()
                                      !  square root of inverse of R, only used
                                      !  if using correlated obs

     integer(i_kind),dimension(:),pointer :: iccerr => NULL()
     integer(i_kind),dimension(:),pointer :: icx => NULL()
     integer(i_kind),dimension(:),pointer :: ich => NULL()
     integer(i_kind) :: nchan         !  number of channels for this profile
     integer(i_kind) :: ij(4)         !  horizontal locations
     logical         :: use_corr_obs  = .false. !  to indicate if correlated obs is implemented
     integer(i_kind) :: iuse_PredOper_type = 0 !  indicate which type of correlated predictor operator is implemented
                                        ! 0 uses none (diagonal)
                                        ! 1 uses rsqrtinv
                                        ! 2 uses Rpred

!!! Is %isis or %isfctype ever being assigned somewhere in the code?
!!! They are used in intrad().
!!!
!!! Now, they are not written to an obsdiags file, nor read from one.

     character(20) :: isis            ! sensor/instrument/satellite id, e.g. amsua_n15
     !integer(i_kind) :: isfctype      ! surf mask: ocean=0,land=1,ice=2,snow=3,mixed=4
     character(80) :: covtype      ! surf mask: ocean=0,land=1,ice=2,snow=3,mixed=4
  contains
    procedure,nopass::  mytype
    procedure::  setHop => obsNode_setHop_
    procedure::   xread => obsNode_xread_
    procedure::  xwrite => obsNode_xwrite_
    procedure:: isvalid => obsNode_isvalid_
    procedure::  gettlddp => gettlddp_

    procedure, nopass:: headerRead  => obsHeader_read_
    procedure, nopass:: headerWrite => obsHeader_write_
    ! procedure:: init  => obsNode_init_
    procedure:: clean => obsNode_clean_
  end type radNode

  public:: radNode_typecast
  public:: radNode_nextcast
        interface radNode_typecast; module procedure typecast_ ; end interface
        interface radNode_nextcast; module procedure nextcast_ ; end interface

  public:: radNode_appendto
        interface radNode_appendto; module procedure appendto_ ; end interface

  character(len=*),parameter:: MYNAME="m_radNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(radNode)
  use m_obsNode, only: obsNode
  implicit none
  type(radNode ),pointer:: ptr_
  class(obsNode),pointer,intent(in):: aNode
  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(radNode)
    ptr_ => aNode
  end select
return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(radNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type(radNode ),pointer:: ptr_
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
  type(radNode),pointer,intent(in):: aNode
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
  mytype="[radNode]"
end function mytype

subroutine obsHeader_read_(iunit,mobs,jread,istat)
  use radinfo, only: npred,nsigradjac
  implicit none
  integer(i_kind),intent(in ):: iunit
  integer(i_kind),intent(out):: mobs
  integer(i_kind),intent(out):: jread
  integer(i_kind),intent(out):: istat
  
  character(len=*),parameter:: myname_=myname//'.obsHeader_read_'
  integer(i_kind):: mpred,msigradjac
_ENTRY_(myname_)
  
  read(iunit,iostat=istat) mobs,jread, mpred,msigradjac
  if(istat==0 .and. (npred/=mpred .or. nsigradjac/=msigradjac)) then
    call perr(myname_,'unmatched dimension information, npred or nsigradjac')
    if(npred/=mpred) then
      call perr(myname_,'     expecting npred =',npred)
      call perr(myname_,'      but read mpred =',mpred)
    endif
    if(nsigradjac/=msigradjac) then
      call perr(myname_,'expecting nsigradjac =',nsigradjac)
      call perr(myname_,' but read msigradjac =',msigradjac)
    endif
    call die(myname_)
  endif
_EXIT_(myname_)
return
end subroutine obsHeader_read_

subroutine obsHeader_write_(junit,mobs,jwrite,jstat)
  use radinfo, only: npred,nsigradjac
  implicit none
  integer(i_kind),intent(in ):: junit
  integer(i_kind),intent(in ):: mobs
  integer(i_kind),intent(in ):: jwrite
  integer(i_kind),intent(out):: jstat
  
  character(len=*),parameter:: myname_=myname//'.obsHeader_write_'
_ENTRY_(myname_)
  write(junit,iostat=jstat) mobs,jwrite, npred,nsigradjac
_EXIT_(myname_)
return
end subroutine obsHeader_write_

subroutine obsNode_clean_(aNode)
  implicit none
  class(radNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_clean_'
_ENTRY_(myname_)
!_TRACEV_(myname_,'%mytype() =',aNode%mytype())
  if(associated(aNode%diags   )) deallocate(aNode%diags   )
  if(associated(aNode%ich     )) deallocate(aNode%ich     )
  if(associated(aNode%res     )) deallocate(aNode%res     )
  if(associated(aNode%err2    )) deallocate(aNode%err2    )
  if(associated(aNode%raterr2 )) deallocate(aNode%raterr2 )
  if(associated(aNode%pred    )) deallocate(aNode%pred    )
  if(associated(aNode%dtb_dvar)) deallocate(aNode%dtb_dvar)
  if(associated(aNode%Rpred   )) deallocate(aNode%Rpred   )
  if(associated(aNode%rsqrtinv)) deallocate(aNode%rsqrtinv)
  if(associated(aNode%icx     )) deallocate(aNode%icx     )
  if(associated(aNode%iccerr  )) deallocate(aNode%iccerr   )
_EXIT_(myname_)
return
end subroutine obsNode_clean_

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use m_obsdiagNode, only: obsdiagLookup_locate
  use radinfo, only: npred,nsigradjac
  implicit none
  class(radNode),intent(inout):: aNode
  integer(i_kind),intent(in   ):: iunit
  integer(i_kind),intent(  out):: istat
  type(obs_diags),intent(in   ):: diagLookup
  logical,optional,intent(in   ):: skip

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_xread_'
  integer(i_kind):: k,nchan
  logical:: skip_
_ENTRY_(myname_)
  skip_=.false.
  if(present(skip)) skip_=skip

  istat=0
  if(skip_) then
    read(iunit,iostat=istat)
                if (istat/=0) then
                  call perr(myname_,'skipping read(%(nchan,iuse_PredOper_type)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if

    read(iunit,iostat=istat)
                if(istat/=0) then
                  call perr(myname_,'skipping read(%(res,err2,...)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                endif

    read(iunit,iostat=istat)
                if(istat/=0) then
                  call perr(myname_,'skipping read(%(Rpred||rsqrtinv)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                endif

  else
    read(iunit,iostat=istat) aNode%nchan,aNode%use_corr_obs,aNode%iuse_PredOper_type
                if (istat/=0) then
                  call perr(myname_,'read(%(nchan,use_corr_obs,iuse_PredOper_type)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if

        if(associated(aNode%diags   )) deallocate(aNode%diags   )
        if(associated(aNode%ich     )) deallocate(aNode%ich     )
        if(associated(aNode%res     )) deallocate(aNode%res     )
        if(associated(aNode%err2    )) deallocate(aNode%err2    )
        if(associated(aNode%raterr2 )) deallocate(aNode%raterr2 )
        if(associated(aNode%pred    )) deallocate(aNode%pred    )
        if(associated(aNode%dtb_dvar)) deallocate(aNode%dtb_dvar)
        if(associated(aNode%Rpred   )) deallocate(aNode%Rpred)
        if(associated(aNode%rsqrtinv)) deallocate(aNode%rsqrtinv)
        if(associated(aNode%icx     )) deallocate(aNode%icx     )
        if(associated(aNode%iccerr  )) deallocate(aNode%iccerr  )

        nchan=aNode%nchan
        allocate( aNode%diags(nchan), &
                  aNode%res  (nchan), &
                  aNode%err2 (nchan), &
                  aNode%raterr2            (nchan), &
                  aNode%pred         (npred,nchan), &
                  aNode%dtb_dvar(nsigradjac,nchan), &
                  aNode%ich  (nchan), &
                  aNode%icx  (nchan), aNode%iccerr(nchan)  )

        read(iunit,iostat=istat)    aNode%ich     , &
                                    aNode%res     , &
                                    aNode%err2    , &
                                    aNode%raterr2 , &
                                    aNode%pred    , &
                                    aNode%icx     , &
                                    aNode%iccerr  , &
                                    aNode%dtb_dvar, &
                                    aNode%wij     , &
                                    aNode%ij
                if (istat/=0) then
                  call perr(myname_,'read(%(res,err2,...)), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if

        if(.not.aNode%use_corr_obs) aNode%iuse_PredOper_type=0
        select case(aNode%iuse_PredOper_type)
        case(1)
            allocate(aNode%rsqrtinv(((nchan+1)*nchan)/2))
            read(iunit,iostat=istat) aNode%rsqrtinv
                if (istat/=0) then
                  call perr(myname_,'read(%rsqrtinv), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if
        case(2)
            allocate(aNode%Rpred(((nchan+1)*nchan)/2,npred))
            read(iunit,iostat=istat) aNode%Rpred
                if (istat/=0) then
                  call perr(myname_,'read(%Rpred), iostat =',istat)
                  _EXIT_(myname_)
                  return
                end if

        case default
            read(iunit,iostat=istat)
        end select

    do k=1,nchan
      aNode%diags(k)%ptr => obsdiagLookup_locate(diagLookup,aNode%idv,aNode%iob,aNode%ich(k))
                if(.not.associated(aNode%diags(k)%ptr)) then
                  call perr(myname_,'obsdiagLookup_locate(k), k =',k)
                  call perr(myname_,'                      %idv =',aNode%idv)
                  call perr(myname_,'                      %iob =',aNode%iob)
                  call perr(myname_,'                   %ich(k) =',aNode%ich(k))
                  call  die(myname_)
                endif
    enddo
  endif
_EXIT_(myname_)
return
end subroutine obsNode_xread_

subroutine obsNode_xwrite_(aNode,junit,jstat)
  use radinfo, only: npred
  implicit none
  class(radNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'.obsNode_xwrite_'
  integer(i_kind):: k
  integer(i_kind):: iuse_PredOper_type
_ENTRY_(myname_)

  jstat=0
  iuse_PredOper_type=0
  if(aNode%use_corr_obs) iuse_PredOper_type=aNode%iuse_PredOper_type
  write(junit,iostat=jstat) aNode%nchan,aNode%use_corr_obs,iuse_PredOper_type
                if (jstat/=0) then
                  call perr(myname_,'write(%(nchan,use_corr_obs, etc.)), iostat =',jstat)
                  _EXIT_(myname_)
                  return
                end if

  write(junit,iostat=jstat) (/ (aNode%ich(k),k=1,aNode%nchan) /), &
                                aNode%res     , &
                                aNode%err2    , &
                                aNode%raterr2 , &
                                aNode%pred    , &
                                aNode%icx     , &
                                aNode%iccerr  , &
                                aNode%dtb_dvar, &
                                aNode%wij     , &
                                aNode%ij
                if (jstat/=0) then
                  call perr(myname_,'write(%(ich,res,err2,...)), iostat =',jstat)
                  _EXIT_(myname_)
                  return
                end if

  select case(iuse_PredOper_type)
  case(1)
      ASSERT(size(aNode%rsqrtinv)==((aNode%nchan+1)*aNode%nchan)/2)
      write(junit,iostat=jstat) aNode%rsqrtinv
           if (jstat/=0) then
               call perr(myname_,'write(%rsqrtinv), iostat =',jstat)
               _EXIT_(myname_)
               return
           end if
  case(2)
      ASSERT(size(aNode%Rpred,1)==((aNode%nchan+1)*aNode%nchan)/2)
      ASSERT(size(aNode%Rpred,2)==npred)
      write(junit,iostat=jstat) aNode%Rpred
           if (jstat/=0) then
               call perr(myname_,'write(%Rpred), iostat =',jstat)
               _EXIT_(myname_)
               return
           end if

  case default
      write(junit,iostat=jstat)
           if (jstat/=0) then
               call perr(myname_,'write as skip record, iostat =',jstat)
               _EXIT_(myname_)
               return
           end if
  end select

_EXIT_(myname_)
return
end subroutine obsNode_xwrite_

subroutine obsNode_setHop_(aNode)
  use m_cvgridLookup, only: cvgridLookup_getiw
  implicit none
  class(radNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
_ENTRY_(myname_)
  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%ij,aNode%wij)
_EXIT_(myname_)
return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(radNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
  integer(i_kind):: k
_ENTRY_(myname_)
  isvalid_=all( (/ (associated(aNode%diags(k)%ptr),k=1,aNode%nchan) /) )
_EXIT_(myname_)
return
end function obsNode_isvalid_

pure subroutine gettlddp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(radNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  integer(kind=i_kind):: k
  do k=1,aNode%nchan
    tlddp = tlddp + aNode%diags(k)%ptr%tldepart(jiter)*aNode%diags(k)%ptr%tldepart(jiter)
  enddo
  if(present(nob)) nob=nob+aNode%nchan
return
end subroutine gettlddp_

end module m_radNode
