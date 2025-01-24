module m_obsLList
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_obsLList
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2016-05-18
!
! abstract: class-module of linked-list of polymorphic obsNode.
!
! program history log:
!   2016-05-18  j guo   - added this document block for the initial polymorphic
!                         implementation.
!   2016-06-24  j.guo   - added support of using m_latlonRange to find a cluster
!                         latlonRange from (elat,elon) values of observations.
!   2016-07-25  j.guo   - added getTLDdotprod, to accumulate obsNode TLD-dot_produst
!   2016-09-19  j.guo   - added function lincr_() to extend []_lsize().
!   2017-08-26  G.Ge    - change allocate(headLL%mold,mold=mold)
!                             to allocate(headLL%mold,source=mold)
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
  use kinds , only: i_kind
  use mpeu_util, only: assert_,die,perr,warn,tell
  use m_obsNode, only: obsNode
  implicit none
  private

  public:: obsLList

  type obsLList
     private
     integer(i_kind):: n_alloc    =0

     integer(i_kind):: my_obsType =0
     class(obsNode),pointer:: mold => null()    ! a mold for the nodes

     class(obsNode),pointer:: head => null()    ! 
     class(obsNode),pointer:: tail => null()

     integer(i_kind):: l_alloc    =0            ! previous n_alloc, see showincr
  end type obsLList

  public:: obsLList_mold        ! get the mold of the obsLList
        interface obsLList_mold; module procedure lmold_; end interface

  public:: obsLList_reset       ! reset or finalize obsLList to its empty state.
  public:: obsLList_appendNode  ! append a node to obsLList

        interface obsLList_reset     ; module procedure lreset_     ; end interface
        interface obsLList_appendNode; module procedure lappendNode_; end interface

  public:: obsLList_rewind      ! rewind obsLList
  public:: obsLList_nextNode    ! move obsLList to its next node

        interface obsLList_rewind    ; module procedure lrewind_    ; end interface
        interface obsLList_nextNode  ; module procedure lnextNode_  ; end interface

  public:: obsLList_headNode    ! locate the head node of obsLList
  public:: obsLList_tailNode    ! locate the tail node of obsLList

        interface obsLList_headNode  ; module procedure lheadNode_  ; end interface
        interface obsLList_tailNode  ; module procedure ltailNode_  ; end interface

  public:: obsLList_lsize        ! get the size of a LList
  public:: obsLList_lcount       ! get the size of a LList
  public:: obsLList_lsort        ! sort nodes according to their keys
  public:: obsLList_write        ! output a LList to a file unit
  public:: obsLList_read         ! input from a file created by _write()
  public:: obsLList_checksum     ! size consistency checking
  public:: obsLList_summary      ! show some information about the LList

        interface obsLList_lsize  ; module procedure lsize_, &
                                                     lincr_   ; end interface
        interface obsLList_lcount ; module procedure lcount_  ; end interface
        interface obsLList_lsort  ; module procedure lsort_   ; end interface
        interface obsLList_write  ; module procedure lwrite_  ; end interface
        interface obsLList_read   ; module procedure lread_   ; end interface
        interface obsLList_checksum; module procedure &
          lchecksum_, &
          lchecksum1_ ; end interface
        interface obsLList_summary; module procedure lsummary_; end interface

  public:: obsLList_getTLDdotprod     ! get "LHS" (dot-product of (:)%diags%tldepar, plus count)
        interface obsLList_getTLDdotprod ; module procedure lTLDdotprod_ ; end interface

  character(len=*),parameter:: MYNAME="m_obsLList"

#include "myassert.H"
#include "mytrace.H"
contains

subroutine lTLDdotprod_(headLL,jiter,tlddp,nnode,nob)
!-- get "LHS" of the given linked-list
  use kinds, only: i_kind,r_kind
  use m_obsNode, only: obsNode_next, obsNode_isluse
  implicit none
  type(obsLList),target, intent(in):: headLL    ! a linked-list 
  integer(kind=i_kind) , intent(in):: jiter     ! for this iteration
  real   (kind=r_kind) , intent(inout):: tlddp  ! dot_product((:)%tld)
  integer(kind=i_kind) , optional, intent(inout):: nnode ! node count
  integer(kind=i_kind) , optional, intent(inout):: nob   ! obs. count

  class(obsNode),pointer:: iNode
  iNode => lheadNode_(headLL)
  do while(associated(iNode))
    if(obsNode_isluse(iNode)) then
      call iNode%getTLDdp(jiter,tlddp,nob=nob)
      if(present(nnode)) nnode=nnode+1
    endif
    iNode => obsNode_next(iNode)
  enddo
end subroutine lTLDdotprod_

function lmold_(headLL) result(ptr_)
  implicit none
  class(obsNode),pointer:: ptr_
  type(obsLList),target,intent(in):: headLL
  ptr_ => null()
  if(associated(headLL%mold)) ptr_ => headLL%mold
end function lmold_

!--------------------------- will go to m_obsLList ----------------------
subroutine lrewind_(headLL)
  implicit none
  type(obsLList),target,intent(inout):: headLL
  headLL%tail => null()
end subroutine lrewind_

function lnextNode_(headLL) result(here_)
  use m_obsNode, only: obsNode_next
  implicit none
  class(obsNode),pointer:: here_
  type(obsLList),target,intent(inout):: headLL

  if(associated(headLL%tail)) then
        ! when not the first time lnextNode_(), after call lrewind_()
    headLL%tail => obsNode_next(headLL%tail)
  else
        ! When the first time lnextNode_(), after call lrewind_()
    headLL%tail => lheadNode_(headLL)
  endif
  here_ => headLL%tail
end function lnextNode_

function lheadNode_(headLL) result(here_)
  implicit none
  class(obsNode),pointer:: here_
  type(obsLList),target,intent(in):: headLL
  here_ => headLL%head
end function lheadNode_

function ltailNode_(headLL) result(here_)
  implicit none
  class(obsNode),pointer:: here_
  type(obsLList),target,intent(in):: headLL
  here_ => headLL%tail
end function ltailNode_

function lsize_(headLL)
  implicit none
  integer(i_kind):: lsize_
  type(obsLList),intent(in):: headLL
  lsize_=headLL%n_alloc
end function lsize_
function lincr_(headLL,incr)
  implicit none
  integer(i_kind):: lincr_
  type(obsLList),intent(inout):: headLL
  logical,intent(in):: incr
  lincr_=headLL%n_alloc
  if(incr) then
    lincr_=lincr_-headLL%l_alloc
    headLL%l_alloc=headLL%n_alloc
  endif
end function lincr_

subroutine lreset_(headLL,mold,stat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lreset_
!   prgmmr:      J. Guo
!
! abstract: reset a linked-list to empty.
!
! program history log:
!   2015-01-12  guo     - reset headLL for a generic obsNode
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
  use m_obsNode, only: obsNode_next
  use m_obsNode, only: obsNode_clean
  use m_obsNode, only: obsNode_type => obsNode_mytype
  implicit none
  type(obsLList), intent(inout):: headLL
  class(obsNode), intent(in   ):: mold
  integer(i_kind),optional,intent(out):: stat

  character(len=*),parameter:: myname_=MYNAME//"::lreset_"
  character(len=:),allocatable:: mymold_
  integer(i_kind):: n
  integer(i_kind):: ier
_ENTRY_(myname_)

  if(present(stat)) stat=0

  call obsNode_clean(headLL%head,deep=.true.,depth=n,stat=ier)
        if(ier/=0.or.n/=0) then
          call perr(myname_,'obsNode_clean(.deep.), stat =',ier)
          call perr(myname_,'                      depth =',n)
          call perr(myname_,'              lsize(headLL) =',lsize_(headLL))
          call perr(myname_,'       headLL%head%mytype() =',obsNode_type(headLL%head))
          call perr(myname_,'       headLL%mold%mytype() =',obsNode_type(headLL%mold))
          if(.not.present(stat)) call die(myname_)
          stat=ier
          _EXIT_(myname_)
          return
        endif

  call nodeDestroy_(headLL%head)

  headLL%n_alloc = 0
  headLL%l_alloc = 0
  headLL%head    => null()
  headLL%tail    => null()

  if(associated(headLL%mold)) then
    mymold_ = obsNode_type(headLL%mold)
    deallocate(headLL%mold,stat=ier)
        if(ier/=0) then
          call perr(myname_,'deallocate(headLL%mold), stat =',ier)
          call perr(myname_,'    obsNode_type(headLL%mold) =',mymold_)
          if(.not.present(stat)) call die(myname_)
          stat=ier
          _EXIT_(myname_)
          return
        endif
  endif

  allocate(headLL%mold, mold=mold)
_EXIT_(myname_)
return
end subroutine lreset_

subroutine lappendNode_(headLL,targetNode)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lappendNode_
!   prgmmr:      J. Guo
!
! abstract: append a node to the given linked-list
!
! program history log:
!   2015-01-12  guo     - constructed for generic _obsNode_
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
        ! Link the next node of the list to the given targetNode.  The return
        ! result is a pointer associated to the same targetNode.
  use m_obsNode, only: obsNode_append
  implicit none
  type(obsLList), intent(inout):: headLL
  !class(obsNode), target, intent(in):: targetNode
  class(obsNode), pointer, intent(in):: targetNode

  character(len=*),parameter:: myname_=MYNAME//'::lappendNode_'
!_ENTRY_(myname_)
        ASSERT(associated(targetNode))

  if(.not.associated(headLL%head)) then
                ! this is a fresh starting -node- for this linked-list ...
    call obsNode_append(headLL%head,targetNode)
    headLL%tail => headLL%head
    headLL%n_alloc = 1

  else
        ASSERT(associated(headLL%tail))
        ASSERT(.not.associated(headLL%tail,targetNode))

    call obsNode_append(headLL%tail,targetNode)
    headLL%n_alloc = headLL%n_alloc + 1

  endif

!_EXIT_(myname_)
return
end subroutine lappendNode_

!--------------------------- will go to m_obsLListIO ----------------------
subroutine lread_(headLL,iunit,redistr,diagLookup,jtype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lread_
!   prgmmr:      todling
!   prgmmr:      J. Guo
!
! abstract: Read obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling - (original read_obsdiags::read_${OBSTYPE}head_()
!   2008-12-08  todling - update to May08 version
!   2015-01-12  guo     - restructured for generic _obsNode_, with redistributions
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    !use obsmod, only: obs_diags
    use m_obsdiagNode, only: obs_diags
    use m_obsNode, only: obsNode_read
    use m_obsNode, only: obsNode_setluse
    implicit none
    type(obsLList), intent(inout):: headLL
    integer(i_kind), intent(in  ):: iunit
    logical        , intent(in  ):: redistr
    type(obs_diags), intent(in  ):: diagLookup
    integer(i_kind), intent(in  ):: jtype

    character(len=*),parameter:: myname_=MYNAME//"::lread_"
    class(obsNode),pointer :: aNode => NULL()
    integer(i_kind) :: kk,istat,mobs,jread
_ENTRY_(myname_)

!   Read in an obs-specific header of the next block
!   >>>>>----------------------------
!   obsHeader is the information about an obs-block, where an obs-block
!   a collection of nodes of the same _obsNode_ type,
!   !-- not about the corresponding linked-list.

        ASSERT(associated(headLL%mold))

    call obsHeader_read_(headLL%mold,iunit,mobs,jread,istat)

        if(istat/=0) then
          call perr(myname_,'%obsHeader_read_(mobs,jread), istat =',istat)
          call perr(myname_,'                              iunit =',iunit)
          call  die(myname_)
        endif

        if(jtype/=jread) then
          call perr(myname_,'unexpected record type, jread =',jread)
          call perr(myname_,'              expecting jtype =',jtype)
          call perr(myname_,'                         mobs =',mobs)
          call perr(myname_,'                        iunit =',iunit)
          call  die(myname_)
        end if
!   ----------------------------<<<<<

    if(mobs==0) then
        ! No more record to read
      _EXIT_(myname_)
      return
    endif

    !-- construct an aNode
    aNode => alloc_nodeCreate_(mold=headLL%mold)
    do kk=1,mobs
      !-- initialize aNode from a file (iunit)
      call obsNode_read(aNode,iunit,istat,redistr=redistr,diagLookup=diagLookup)
                if(istat<0) then
                  call perr(myname_,'obsNode_read(), istat =',istat)
                  call perr(myname_,'                iunit =',iunit)
                  call perr(myname_,'                   kk =',kk)
                  call perr(myname_,'                 mobs =',mobs)
                  call perr(myname_,'              redistr =',redistr)
                  call perr(myname_,'                jtype =',jtype)
                  call  die(myname_)
                endif

      if(istat==0) cycle

      !-- If this aNode is to be kept ...
      if(redistr) then
        ! recompute its %luse and %Hop for the redistributed grid partition,

        call obsNode_setluse(aNode)     ! reset %luse for subdomain ownership
        call aNode%setHop()             ! recompute %Hop for the new grid
      endif

                !-- keep this obsNode in its linked-list, obsLList := obsdiags(jtype,ibin)
      call lappendNode_(headLL,targetNode=aNode)

                !-- Drop the earlier object, contruct a new aNode.
                !-- No deep deallocation is needed for aNode, since its
                !-- associated target has been passed to headLL
      aNode => null()
      aNode => alloc_nodeCreate_(mold=headLL%mold)

    enddo  ! < mobs >

    call nodeDestroy_(aNode)  ! Clean up the working-space an_onsNode
    
_EXIT_(myname_)
return
end subroutine lread_

subroutine lwrite_(headLL,iunit,luseonly,jtype,luseRange)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lwrite_
!   prgmmr:      todling
!   prgmmr:      J. Guo
!
! abstract: Write obs-specific data structure from file.
!
! program history log:
!   2007-10-03  todling - (original write_obsdiags::write_${OBSTYPE}head_()
!   2008-12-08  todling - update to May08 version
!   2015-01-12  guo     - restructured for generic _obsNode_, with redistributions
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

    use m_obsNode, only: obsNode_next
    use m_obsNode, only: obsNode_isluse
    use m_obsNode, only: obsNode_write
    use m_latlonRange, only: latlonRange
    use m_latlonRange, only: latlonRange_enclose
    implicit none
    type(obsLList), intent(in):: headLL
    integer(i_kind ), intent(in):: iunit       ! unit for output
    logical         , intent(in):: luseonly
    integer(i_kind ), intent(in):: jtype
    type(latlonRange),optional,intent(inout):: luseRange

    character(len=*),parameter:: myname_=MYNAME//"::lwrite_"
    class(obsNode), pointer :: iNode => NULL()
    integer(i_kind) :: istat
    integer(i_kind) :: mobs,lobs,iobs,kobs
    logical:: isluse_
_ENTRY_(myname_)

!        if(jtype/=iobsType) then
!          call perr(myname_,'unexpected record type, jtype =',jtype)
!          call perr(myname_,'           expecting iobsType =',iobsType)
!          call perr(myname_,'                        iunit =',iunit)
!          call  die(myname_)
!        end if

!   Read in an obs-specific header of the next block
!   >>>>>----------------------------
!   !-- A header is about a collection of nodes of the same obsNode type,
!   !-- not about the corresponding linked-list.

        ASSERT(associated(headLL%mold))

    lobs = lcount_(headLL,luseonly=luseonly)      ! actual count of write
    mobs = lobs
    if(.not.luseonly) mobs = lsize_(headLL)       ! actual count of nodes

    call obsHeader_write_(headLL%mold,iunit,lobs,jtype,istat)

        if(istat/=0) then
          call perr(myname_,'obsHeader_write_(), istat =',istat)
          call perr(myname_,'                    iunit =',iunit)
          call perr(myname_,'                    jtype =',jtype)
          call perr(myname_,'        no. node of write =',lobs)
          call perr(myname_,'         no. node of data =',mobs)
          call perr(myname_,'                 luseonly =',luseonly)
          call  die(myname_)
        endif
!   ----------------------------<<<<<

    if(lobs==0) then
        ! No more record to write
      _EXIT_(myname_)
      return
    endif

!-- looping over the linked-list for every obsNode,

    iNode => lheadNode_(headLL)
    iobs=0
    kobs=0
    do while(associated(iNode))
      iobs=iobs+1
      isluse_=obsNode_isluse(iNode)
      if(isluse_ .or. .not.luseonly) then
        if(isluse_.and.present(luseRange)) &
                call latlonRange_enclose(luseRange,iNode%elat,iNode%elon)
        kobs=kobs+1
        call obsNode_write(iNode,iunit,istat)
                if(istat/=0) then
                  call perr(myname_,' obsNode_write(), istat =',istat)
                  call perr(myname_,'                  iunit =',iunit)
                  call perr(myname_,'                  jtype =',jtype)
                  call perr(myname_,'current-luse-node, kobs =',kobs)
                  call perr(myname_,' current-all-node, iobs =',iobs)
                  call perr(myname_,'  total-luse-node-count =',lobs)
                  call perr(myname_,'   total-all-node-count =',mobs)
                  call perr(myname_,'               luseonly =',luseonly)
                  call  die(myname_)
                endif
      endif
      iNode => obsNode_next(iNode)
    enddo

        ASSERT(iobs==mobs)
        ASSERT(kobs==lobs)
_EXIT_(myname_)
return
end subroutine lwrite_

subroutine lchecksum_(headLL,itype,ibin,leadNode,sorted)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lchecksum_
!   prgmmr:      J. Guo
!
! abstract: check the size values against a known counts.
!
! program history log:
!   2015-06-26  guo     - 
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
  use mpeu_util, only: stdout,stdout_lead
  implicit none
  type(obsLList), intent(in):: headLL
  integer(kind=i_kind),optional,intent(in ):: itype,ibin
  class(obsNode),optional,pointer, intent(in):: leadNode
  logical             ,optional,intent(out):: sorted

  character(len=*),parameter:: myname_=MYNAME//"::lchecksum_"
  integer(kind=i_kind):: lrecount
  integer(kind=i_kind):: jtype,jbin
  integer(kind=i_kind):: nuse,nooo,ndup,ksum(2)
_ENTRY_(myname_)
  lrecount=lcount_(headLL,recount=.true.,nuse=nuse,nooo=nooo,ndup=ndup,ksum=ksum,leadNode=leadNode)
  if(present(sorted)) sorted = nooo==0.and.ndup==0

  jtype=itype
  jbin =ibin
_EXIT_(myname_)
return
end subroutine lchecksum_
subroutine lchecksum1_(headLL,itype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lchecksum_
!   prgmmr:      J. Guo
!
! abstract: check the size values against a known counts.
!
! program history log:
!   2015-06-26  guo     - 
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
  implicit none
  type(obsLList), dimension(:),intent(in):: headLL
  integer(kind=i_kind),optional ,intent(in):: itype

  character(len=*),parameter:: myname_=MYNAME//"::lchecksum1_"
  integer(kind=i_kind):: i
_ENTRY_(myname_)
  do i=1,size(headLL)
    call lchecksum_(headLL(i),itype=itype,ibin=i)
  enddo
_EXIT_(myname_)
return
end subroutine lchecksum1_

subroutine lsummary_(headLL,verbose)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lsummary_
!   prgmmr:      J. Guo
!
! abstract: summarize for the contents of a linked-list.
!
! program history log:
!   2015-01-12  guo     - constructed for generic _obsNode_
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
  use m_obsNode, only: obsNode_next
  use m_obsNode, only: obsNode_show
  implicit none
  type(obsLList), intent(in):: headLL
  logical,optional, intent(in):: verbose

  character(len=*),parameter:: myname_=MYNAME//"::lsummary_"
  class(obsNode), pointer:: iNode
  integer(i_kind):: iobs_
  logical:: verbose_
  verbose_=.false.
  if(present(verbose)) verbose_=verbose
_ENTRY_(myname_)
  !call tell(myname_,' headLList%n_alloc =',headLL%n_alloc)

  if(verbose_) then
    iobs_ = 0
    iNode => lheadNode_(headLL)
    do while(associated(iNode))
      iobs_=iobs_+1
      call obsNode_show(iNode,iobs_)
      iNode => obsNode_next(iNode)
    enddo
  endif
_EXIT_(myname_)
return
end subroutine lsummary_

function lcount_(headLL,luseonly,recount,nuse,nooo,ndup,ksum,leadNode) result(lobs_)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    lcount_
!   prgmmr:      J. Guo
!
! abstract: inquire for the size information about the linked-list
!
! program history log:
!   2015-01-12  guo     - constructed for generic _obsNode_
!
!   input argument list: (see Fortran declarations below)
!
!   output argument list: (see Fortran declarations below)
!
! attributes:
!   language: f90/f95/f2003/f2008
!   machine:
!
!$$$ end documentation block
  use m_obsNode, only: obsNode_next
  use m_obsNode, only: obsNode_isluse
  implicit none
  integer(kind=i_kind):: lobs_
  type(obsLList), target, intent(in):: headLL
  logical,optional,intent(in):: luseonly        ! count only luse data
  logical,optional,intent(in):: recount
  integer(kind=i_kind),optional,intent(out):: nuse      ! no. luse
  integer(kind=i_kind),optional,intent(out):: nooo      ! no. out-of-orders
  integer(kind=i_kind),optional,intent(out):: ndup      ! no. duplicates
  integer(kind=i_kind),optional,dimension(:),intent(out):: ksum ! key value sum
  class(obsNode), pointer, optional, intent(in):: leadNode

  character(len=*),parameter:: myname_=MYNAME//"::lcount_"
  class(obsNode), pointer:: iNode
  integer(i_kind):: nuse_
  integer(kind=i_kind),dimension(2) :: kprev
  logical:: luseonly_,recount_,checksum_
_ENTRY_(myname_)

  luseonly_=.false.
  if(present(luseonly)) luseonly_=luseonly
  recount_ =.false.
  if(present(recount )) recount_ =recount
  if(present(leadNode)) recount_ =.true.

  checksum_= present(nuse).or.present(nooo).or.present(ndup).or.present(ksum)
  if(.not.recount_) recount_ = checksum_

  if(present(ksum)) then
    ALWAYS_ASSERT(size(ksum)==size(kprev))
  endif

  if(.not.(luseonly_.or.recount_)) then
    lobs_=headLL%n_alloc

  else
    lobs_ = 0
    nuse_ = 0

    if(checksum_) call checksum_init_(kprev,nooo=nooo,ndup=ndup,ksum=ksum)

    iNode => lheadNode_(headLL)
    do while(associated(iNode))
      if(obsNode_isluse(iNode)) nuse_=nuse_+1
      if(.not.luseonly_ .or. obsNode_isluse(iNode)) lobs_=lobs_+1

      if(checksum_) call checksum_add_(kprev, &
        knext=(/iNode%idv,iNode%iob/),nooo=nooo,ndup=ndup,ksum=ksum)

      iNode => obsNode_next(iNode)
    enddo
    if(present(nuse)) nuse=nuse_
  endif

_EXIT_(myname_)
return
end function lcount_

subroutine checksum_init_(kprev,nooo,ndup,ksum)
  implicit none
  integer(kind=i_kind),dimension(:),intent(out):: kprev
  integer(kind=i_kind),optional,intent(out):: nooo
  integer(kind=i_kind),optional,intent(out):: ndup
  integer(kind=i_kind),optional,dimension(:),intent(out):: ksum

  kprev(:)= 0
  if(present(nooo)) nooo=0
  if(present(ndup)) ndup=0
  if(present(ksum)) ksum(:)=0
end subroutine checksum_init_

subroutine checksum_add_(kprev,knext,nooo,ndup,ksum)
  implicit none
  integer(kind=i_kind),dimension(:),intent(inout):: kprev
  integer(kind=i_kind),dimension(:),intent(in   ):: knext
  integer(kind=i_kind),optional,intent(inout):: nooo
  integer(kind=i_kind),optional,intent(inout):: ndup
  integer(kind=i_kind),optional,dimension(:),intent(inout):: ksum

  integer(kind=i_kind):: k
  k=compare_(kprev,knext)
  if(present(nooo).and.k> 0) nooo=nooo+1
  if(present(ndup).and.k==0) ndup=ndup+1
  if(present(ksum)) ksum(:)=ksum(:)+knext(:)
  kprev(:)=knext(:)
end subroutine checksum_add_

function compare_(key1,key2) result (m)
  implicit none
  integer(kind=i_kind):: m
  integer(kind=i_kind),dimension(:),intent(in):: key1,key2

  integer(kind=i_kind):: n,i
  m=0
  n=min(size(key1),size(key2))
  do i=1,n
    if    (key1(i)<key2(i)) then
      m=-1; exit
    elseif(key1(i)>key2(i)) then
      m=+1; exit
    endif
  enddo
end function compare_

subroutine lsort_(headLL,itype,ibin)
!       lsort_: node-sort diagLL, to line-up nodes according to their keys
!_TIMER_USE_
!  use timermod , only: timer_ini,timer_fnl
  use mpeu_util, only: IndexSet
  use mpeu_util, only: IndexSort
  use m_obsNode, only: obsNode_next
  !use mpeu_util, only: die
  implicit none
  type(obsLList), intent(inout):: headLL
  integer(kind=i_kind),optional,intent(in):: itype,ibin

  character(len=*),parameter:: myname_=myname//'::lsort_'
  class(obsNode),pointer:: pNode
  integer(kind=i_kind),allocatable,dimension(:):: indx,idv_,iob_
  integer(kind=i_kind):: i,n
  logical:: sorted

  type fptr_of_obsnode
    class(obsNode),pointer:: ptr
  end type fptr_of_obsnode
  type(fptr_of_obsnode),allocatable,dimension(:):: lookup
_ENTRY_(myname_)
!_TIMER_ON_(myname_)
!  call timer_ini(myname_)

  call lchecksum_(headLL,itype=itype,ibin=ibin,sorted=sorted)
        if(sorted) then
          _EXIT_(myname_)
          return
        endif

  n=lsize_(headLL)

  allocate(lookup(n))
  allocate(indx(n),idv_(n),iob_(n))

        ! Loop over the linked-list, to get keys.
  i=0
  pNode => lheadNode_(headLL)
  do while(associated(pNode))
    i=i+1
    if(i<=n) then
      lookup(i)%ptr => pNode
        idv_(i)     =  pNode%idv
        iob_(i)     =  pNode%iob
    endif
    pNode => obsNode_next(pNode)
  enddo
 
  ASSERT(i==n)

        ! sort %lookup(1:n), by its (idv,iob) values
  call IndexSet (indx)
  call IndexSort(indx,iob_)
  call IndexSort(indx,idv_)
  lookup(1:n) = lookup(indx(1:n))

  deallocate(indx,idv_,iob_)

        ! Rebuild the linked-list from lookup(1:n)%ptr
  headLL%n_alloc = 0
  headLL%head    => null()
  headLL%tail    => null()

        ! rebuild the list according to the sorted table
  do i=1,n
    call lappendNode_(headLL,lookup(i)%ptr)
  enddo
        ASSERT(n==headLL%n_alloc)
        if(associated(headLL%tail)) then
          ASSERT(.not.associated(headLL%tail%llpoint))
        endif

        ! discard the table
  deallocate(lookup)

  call lchecksum_(headLL,itype=itype,ibin=ibin,sorted=sorted)
        if(.not.sorted) then
          call perr(myname_,'failed post-sorting lchecksum_(), sorted =',sorted)
          if(present(itype)) &
          call perr(myname_,'                                   itype =',itype)
          if(present(ibin )) &
          call perr(myname_,'                                    ibin =',ibin)
          call  die(myname_)
        endif

!  call timer_fnl(myname_)
!_TIMER_OFF_(myname_)
_EXIT_(myname_)
return
end subroutine lsort_

function alloc_nodeCreate_(mold) result(ptr_)
!-- allocate() + init()
  implicit none
  class(obsNode),pointer:: ptr_
  class(obsNode),target,intent(in):: mold
  allocate(ptr_,mold=mold)
  call ptr_%init()
return
end function alloc_nodeCreate_

subroutine nodeDestroy_(node)
!-- clean() + deallocate()
  use m_obsNode, only: obsNode_type => obsNode_mytype
  implicit none
  class(obsNode),pointer,intent(inout):: node
  character(len=*),parameter:: myname_=myname//'::nodeDestroy_'
  integer(i_kind):: ier
  if(associated(node)) then
    call node%clean()
    deallocate(node,stat=ier)
    if(ier/=0) then
      call perr(myname_,'can not deallocate(node), stat =',ier)
      call perr(myname_,'            obsNode_type(node) =',obsNode_type(node))
      call die(myname_)
    endif
  endif
return
end subroutine nodeDestroy_

subroutine obsHeader_read_(aNode,iunit,iobs,itype,istat)
!-- read header of some type
  use m_obsNode, only: obsNode
  implicit none
  class(obsNode) ,intent(in ):: aNode
  integer(i_kind),intent(in ):: iunit
  integer(i_kind),intent(out):: iobs,itype
  integer(i_kind),intent(out):: istat
  call aNode%headerRead(iunit,iobs,itype,istat)
end subroutine obsHeader_read_

subroutine obsHeader_write_(aNode,junit,mobs,mtype,istat)
!-- write header of some type
  use m_obsNode, only: obsNode
  implicit none
  class(obsNode) ,intent(in ):: aNode
  integer(i_kind),intent(in ):: junit
  integer(i_kind),intent(in ):: mobs,mtype
  integer(i_kind),intent(out):: istat
  call aNode%headerWrite(junit,mobs,mtype,istat)
end subroutine obsHeader_write_
end module m_obsLList
