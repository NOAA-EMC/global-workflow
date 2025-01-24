module m_lightNode
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    module m_lightNode
!   prgmmr:      k apodaca <karina.apodaca@colostate.edu>
!      org:      CSU/CIRA, Data Assimilation Group
!     date:      2018-01-18
!
! abstract: class-module of obs-type lightNode (Lightning)
!
! program history log:
!   2018-01-18  k apodaca   - add this document block for the implementation of
!                             variational lightning data assimilation.
!   2018-08-26  k apodaca   - add coefficients relaed to a second observaion operator
!                             for lighning flash rate, suitable for non-hydrostatic, 
!                             cloud-resolving models.
!                             

!                .      .    .                                       .

! In the case of lightning observations (e.g. GOES/GLM), the schematic shown below is
! used for the interpolation of background fields to the location of an observation (+)
! and for the finite-difference derivation method used in the calculation of the TL of
! the observation operator for lightning flash rate. Calculations are done at each
! quadrant (i.e. central, north, south, east, and west).
!
!         i6-------i8
!          |       |
!          |       |
! i10-----i2-------i4-----i12
!  |       |       |       |
!  |       |     + |       |
! i9------i1-------i3-----i11
!          |       |
!          |       |
!         i5-------i7
!

!                .      .    .                                       .

!   2019-03-01  j guo   - Merged in some cleaning up changes as in other
!                         obsNode types:
!                       . Added a type specific subroutine appendto_(), to avoid
!                         unnecessary type generalization between a generic
!                         append() and user code.
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

  public:: lightNode

  type,extends(obsNode):: lightNode
     !type(light_ob_type),pointer :: llpoint => NULL()
     type(obs_diag), pointer :: diags => NULL()
     real(r_kind)    :: res    =0._r_kind    !  light residual
     real(r_kind)    :: err2   =0._r_kind    !  light error squared
     real(r_kind)    :: raterr2=0._r_kind    !  square of ratio of final obs error
                                      !  to original obs error
     !real(r_kind)    :: time          !  observation time in sec
     real(r_kind)    :: b      =0._r_kind    !  variational quality control parameter
     real(r_kind)    :: pg     =0._r_kind    !  variational quality control parameter
     real(r_kind)    :: wij(4) =0._r_kind    !  horizontal interpolation weights

! Central quadrant
     real(r_kind),pointer               :: jac_z0i1  => NULL() ! surface z at i1
     real(r_kind),pointer               :: jac_z0i2  => NULL() ! surface z at i2
     real(r_kind),pointer               :: jac_z0i3  => NULL() ! surface z at i3
     real(r_kind),pointer               :: jac_z0i4  => NULL() ! surface z at i4
     real(r_kind),dimension(:),pointer  :: jac_vertqi1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zdxi1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zdxi2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zdxi3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zdxi4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zdyi1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zdyi2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zdyi3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zdyi4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_udxi1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_udxi2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_udxi3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_udxi4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vdyi1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vdyi2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vdyi3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vdyi4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vert => NULL()
     real(r_kind),dimension(:),pointer  :: jac_sigdoti1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_sigdoti2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_sigdoti3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_sigdoti4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qi1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qi2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qi3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qi4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_ti1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_ti2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_ti3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_ti4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qgmai1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qgmai2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qgmai3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qgmai4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qgmbi1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qgmbi2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qgmbi3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_qgmbi4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_icei1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_icei2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_icei3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_icei4 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zicei1 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zicei2 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zicei3 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_zicei4 => NULL()
     real(r_kind),pointer               :: kboti1 => NULL()
     real(r_kind),pointer               :: kboti2 => NULL()
     real(r_kind),pointer               :: kboti3 => NULL()
     real(r_kind),pointer               :: kboti4 => NULL()
     real(r_kind),pointer               :: jac_kverti1 => NULL()
     real(r_kind),pointer               :: jac_kverti2 => NULL()
     real(r_kind),pointer               :: jac_kverti3 => NULL()
     real(r_kind),pointer               :: jac_kverti4 => NULL()
     real(r_kind),pointer               :: jac_fratei1 => NULL()
     real(r_kind),pointer               :: jac_fratei2 => NULL()
     real(r_kind),pointer               :: jac_fratei3 => NULL()
     real(r_kind),pointer               :: jac_fratei4 => NULL()
     logical,pointer                    :: jac_wmaxflagi1 => NULL() ! wmax flag at i1
     logical,pointer                    :: jac_wmaxflagi2 => NULL() ! wmax flag at i2
     logical,pointer                    :: jac_wmaxflagi3 => NULL() ! wmax flag at i3
     logical,pointer                    :: jac_wmaxflagi4 => NULL() ! wmax flag at i4

! South quadrant
     real(r_kind),pointer               :: jac_z0i5 => NULL()
     real(r_kind),pointer               :: jac_z0i7 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi5 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi7 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti5 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti7 => NULL()

! North quadrant
     real(r_kind),pointer               :: jac_z0i6 => NULL()
     real(r_kind),pointer               :: jac_z0i8 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi6 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi8 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti6 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti8 => NULL()

! West quadrant
     real(r_kind),pointer               :: jac_z0i9  => NULL() ! surface z at i9
     real(r_kind),pointer               :: jac_z0i10 => NULL() ! surface z at i10
     real(r_kind),dimension(:),pointer  :: jac_vertqi9  => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi10 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti9  => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti10 => NULL()

! East quadrant
     real(r_kind),pointer               :: jac_z0i11 => NULL() ! surface z at i11
     real(r_kind),pointer               :: jac_z0i12 => NULL() ! surface z at i12
     real(r_kind),dimension(:),pointer  :: jac_vertqi11 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertqi12 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti11 => NULL()
     real(r_kind),dimension(:),pointer  :: jac_vertti12 => NULL()

     integer(i_kind),dimension(:,:),pointer :: ij  => NULL()
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
    procedure::  gettlddp => gettlddp_

    procedure, nopass:: headerRead  => obsHeader_read_
    procedure, nopass:: headerWrite => obsHeader_write_
    procedure:: init  => obsNode_init_
    procedure:: clean => obsNode_clean_
  end type lightNode

  public:: lightNode_typecast
  public:: lightNode_nextcast
        interface lightNode_typecast; module procedure typecast_ ; end interface
        interface lightNode_nextcast; module procedure nextcast_ ; end interface

  public:: lightNode_appendto
        interface lightNode_appendto; module procedure appendto_ ; end interface

  character(len=*),parameter:: MYNAME="m_lightNode"

#include "myassert.H"
#include "mytrace.H"
contains
function typecast_(aNode) result(ptr_)
!-- cast a class(obsNode) to a type(lightNode)
  use m_obsNode, only: obsNode
  implicit none
  type(lightNode),pointer:: ptr_
  class(obsNode ),pointer,intent(in):: aNode

  ptr_ => null()
  if(.not.associated(aNode)) return
        ! logically, typecast of a null-reference is a null pointer.
  select type(aNode)
  type is(lightNode)
    ptr_ => aNode
  end select
  return
end function typecast_

function nextcast_(aNode) result(ptr_)
!-- cast an obsNode_next(obsNode) to a type(lightNode)
  use m_obsNode, only: obsNode,obsNode_next
  implicit none
  type(lightNode),pointer:: ptr_
  class(obsNode ),target ,intent(in):: aNode

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
  type(lightNode),pointer,intent(in):: aNode
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
  mytype="[lightNode]"
end function mytype

subroutine obsHeader_read_(iunit,mobs,jread,istat)
  use gridmod, only: nsig
  implicit none
  integer(i_kind),intent(in ):: iunit
  integer(i_kind),intent(out):: mobs
  integer(i_kind),intent(out):: jread
  integer(i_kind),intent(out):: istat

  character(len=*),parameter:: myname_=myname//"::obsHeader_read"
  integer(i_kind):: msig
_ENTRY_(myname_)

  msig=-1
  read(iunit,iostat=istat) mobs,jread, msig
  if(istat==0 .and. msig/=nsig) then
    call perr(myname_,'unmatched dimension information, expecting nsig =',nsig)
    call perr(myname_,'                                  but read msig =',msig)
    call  die(myname_)
  endif
_EXIT_(myname_)
  return
end subroutine obsHeader_read_

subroutine obsHeader_write_(junit,mobs,jwrite,jstat)
  use gridmod, only: nsig
  implicit none
  integer(i_kind),intent(in ):: junit
  integer(i_kind),intent(in ):: mobs
  integer(i_kind),intent(in ):: jwrite
  integer(i_kind),intent(out):: jstat

  character(len=*),parameter:: myname_=myname//"::obsHeader_write"
_ENTRY_(myname_)
  write(junit,iostat=jstat) mobs,jwrite, nsig
_EXIT_(myname_)
  return
end subroutine obsHeader_write_

subroutine obsNode_init_(aNode)
  use gridmod, only: nsig
  implicit none
  class(lightNode),intent(out):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_init_'
_ENTRY_(myname_)
  allocate(aNode%jac_z0i1,          aNode%jac_z0i2,           & 
           aNode%jac_z0i3,          aNode%jac_z0i4,           &
           aNode%jac_z0i5,          aNode%jac_z0i6,           &
           aNode%jac_z0i7,          aNode%jac_z0i8,           &
           aNode%jac_z0i9,          aNode%jac_z0i10,          &
           aNode%jac_z0i11,         aNode%jac_z0i12           )

  allocate(aNode%jac_vertqi1(nsig), aNode%jac_vertqi2(nsig),  &
           aNode%jac_vertqi3(nsig), aNode%jac_vertqi4(nsig),  &
           aNode%jac_vertqi5(nsig), aNode%jac_vertqi6(nsig),  &
           aNode%jac_vertqi7(nsig), aNode%jac_vertqi8(nsig),  &
           aNode%jac_vertqi9(nsig), aNode%jac_vertqi10(nsig), &
           aNode%jac_vertqi11(nsig),aNode%jac_vertqi12(nsig)  )


  allocate(aNode%jac_vertti1(nsig), aNode%jac_vertti2(nsig),  &
           aNode%jac_vertti3(nsig), aNode%jac_vertti4(nsig),  &
           aNode%jac_vertti5(nsig), aNode%jac_vertti6(nsig),  &
           aNode%jac_vertti7(nsig), aNode%jac_vertti8(nsig),  &
           aNode%jac_vertti9(nsig), aNode%jac_vertti10(nsig), &
           aNode%jac_vertti11(nsig),aNode%jac_vertti12(nsig)  )

  allocate(aNode%jac_zdxi1(nsig),   aNode%jac_zdxi2(nsig),    &
           aNode%jac_zdxi3(nsig),   aNode%jac_zdxi4(nsig)     )

  allocate(aNode%jac_zdyi1(nsig),   aNode%jac_zdyi2(nsig),    &
           aNode%jac_zdyi3(nsig),   aNode%jac_zdyi4(nsig)     )

  allocate(aNode%jac_udxi1(nsig),   aNode%jac_udxi2(nsig),    &
           aNode%jac_udxi3(nsig),   aNode%jac_udxi4(nsig)     )

  allocate(aNode%jac_vdyi1(nsig),   aNode%jac_vdyi2(nsig),    &
           aNode%jac_vdyi3(nsig),   aNode%jac_vdyi4(nsig)     )

  allocate(aNode%jac_vert(nsig)                               ) 

  allocate(aNode%jac_sigdoti1(nsig),aNode%jac_sigdoti2(nsig), &
           aNode%jac_sigdoti3(nsig),aNode%jac_sigdoti4(nsig)  )

  allocate(aNode%jac_qi1(nsig),     aNode%jac_qi2(nsig),      &
           aNode%jac_qi3(nsig),     aNode%jac_qi4(nsig)       )

  allocate(aNode%jac_ti1(nsig),     aNode%jac_ti2(nsig),      &
           aNode%jac_ti3(nsig),     aNode%jac_ti4(nsig)       ) 

  allocate(aNode%jac_qgmai1(nsig),  aNode%jac_qgmai2(nsig),   &
           aNode%jac_qgmai3(nsig),  aNode%jac_qgmai4(nsig)    )

  allocate(aNode%jac_qgmbi1(nsig),  aNode%jac_qgmbi2(nsig),   &
           aNode%jac_qgmbi3(nsig),  aNode%jac_qgmbi4(nsig)    )

  allocate(aNode%jac_icei1(nsig),   aNode%jac_icei2(nsig),    &
           aNode%jac_icei3(nsig),   aNode%jac_icei4(nsig)     )

  allocate(aNode%jac_zicei1(nsig),  aNode%jac_zicei2(nsig),   &
           aNode%jac_zicei3(nsig),  aNode%jac_zicei4(nsig)    )

  allocate(aNode%kboti1,            aNode%kboti2,             &
           aNode%kboti3,            aNode%kboti4              )

  allocate(aNode%jac_kverti1,       aNode%jac_kverti2,        &
           aNode%jac_kverti3,       aNode%jac_kverti4         )

  allocate(aNode%jac_fratei1,       aNode%jac_fratei2,        &
           aNode%jac_fratei3,       aNode%jac_fratei4         )

  allocate(aNode%jac_wmaxflagi1,    aNode%jac_wmaxflagi2,     &
           aNode%jac_wmaxflagi3,    aNode%jac_wmaxflagi4,     &
           aNode%ij(12,nsig)                                  )
_EXIT_(myname_)
  return
end subroutine obsNode_init_

subroutine obsNode_clean_(aNode)
  implicit none
  class(lightNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_clean_'
_ENTRY_(myname_)
!_TRACEV_(myname_,'%mytype() =',aNode%mytype())
     if(associated(aNode%jac_z0i1 ))  deallocate(aNode%jac_z0i1 )
     if(associated(aNode%jac_z0i2 ))  deallocate(aNode%jac_z0i2 )
     if(associated(aNode%jac_z0i3 ))  deallocate(aNode%jac_z0i3 )
     if(associated(aNode%jac_z0i4 ))  deallocate(aNode%jac_z0i4 )
     if(associated(aNode%jac_z0i5 ))  deallocate(aNode%jac_z0i5 )
     if(associated(aNode%jac_z0i6 ))  deallocate(aNode%jac_z0i6 )
     if(associated(aNode%jac_z0i7 ))  deallocate(aNode%jac_z0i7 )
     if(associated(aNode%jac_z0i8 ))  deallocate(aNode%jac_z0i8 )
     if(associated(aNode%jac_z0i9 ))  deallocate(aNode%jac_z0i9 )
     if(associated(aNode%jac_z0i10))  deallocate(aNode%jac_z0i10)
     if(associated(aNode%jac_z0i11))  deallocate(aNode%jac_z0i11)
     if(associated(aNode%jac_z0i12))  deallocate(aNode%jac_z0i12)

     if(associated(aNode%jac_vertqi1 )) deallocate(aNode%jac_vertqi1 )
     if(associated(aNode%jac_vertqi2 )) deallocate(aNode%jac_vertqi2 )
     if(associated(aNode%jac_vertqi3 )) deallocate(aNode%jac_vertqi3 )
     if(associated(aNode%jac_vertqi4 )) deallocate(aNode%jac_vertqi4 )
     if(associated(aNode%jac_vertqi5 )) deallocate(aNode%jac_vertqi5 )
     if(associated(aNode%jac_vertqi6 )) deallocate(aNode%jac_vertqi6 )
     if(associated(aNode%jac_vertqi7 )) deallocate(aNode%jac_vertqi7 )
     if(associated(aNode%jac_vertqi8 )) deallocate(aNode%jac_vertqi8 )
     if(associated(aNode%jac_vertqi9 )) deallocate(aNode%jac_vertqi9 )
     if(associated(aNode%jac_vertqi10)) deallocate(aNode%jac_vertqi10)
     if(associated(aNode%jac_vertqi11)) deallocate(aNode%jac_vertqi11)
     if(associated(aNode%jac_vertqi12)) deallocate(aNode%jac_vertqi12)

     if(associated(aNode%jac_vertti1 )) deallocate(aNode%jac_vertti1 )
     if(associated(aNode%jac_vertti2 )) deallocate(aNode%jac_vertti2 )
     if(associated(aNode%jac_vertti3 )) deallocate(aNode%jac_vertti3 )
     if(associated(aNode%jac_vertti4 )) deallocate(aNode%jac_vertti4 )
     if(associated(aNode%jac_vertti5 )) deallocate(aNode%jac_vertti5 )
     if(associated(aNode%jac_vertti6 )) deallocate(aNode%jac_vertti6 )
     if(associated(aNode%jac_vertti7 )) deallocate(aNode%jac_vertti7 )
     if(associated(aNode%jac_vertti8 )) deallocate(aNode%jac_vertti8 )
     if(associated(aNode%jac_vertti9 )) deallocate(aNode%jac_vertti9 )
     if(associated(aNode%jac_vertti10)) deallocate(aNode%jac_vertti10)
     if(associated(aNode%jac_vertti11)) deallocate(aNode%jac_vertti11)
     if(associated(aNode%jac_vertti12)) deallocate(aNode%jac_vertti12)

     if(associated(aNode%jac_zdxi1)) deallocate(aNode%jac_zdxi1)
     if(associated(aNode%jac_zdxi2)) deallocate(aNode%jac_zdxi2)
     if(associated(aNode%jac_zdxi3)) deallocate(aNode%jac_zdxi3)
     if(associated(aNode%jac_zdxi4)) deallocate(aNode%jac_zdxi4)

     if(associated(aNode%jac_zdyi1)) deallocate(aNode%jac_zdyi1)
     if(associated(aNode%jac_zdyi2)) deallocate(aNode%jac_zdyi2)
     if(associated(aNode%jac_zdyi3)) deallocate(aNode%jac_zdyi3)
     if(associated(aNode%jac_zdyi4)) deallocate(aNode%jac_zdyi4)

     if(associated(aNode%jac_udxi1)) deallocate(aNode%jac_udxi1)
     if(associated(aNode%jac_udxi2)) deallocate(aNode%jac_udxi2)
     if(associated(aNode%jac_udxi3)) deallocate(aNode%jac_udxi3)
     if(associated(aNode%jac_udxi4)) deallocate(aNode%jac_udxi4)

     if(associated(aNode%jac_vdyi1)) deallocate(aNode%jac_vdyi1)
     if(associated(aNode%jac_vdyi2)) deallocate(aNode%jac_vdyi2)
     if(associated(aNode%jac_vdyi3)) deallocate(aNode%jac_vdyi3)
     if(associated(aNode%jac_vdyi4)) deallocate(aNode%jac_vdyi4)

     if(associated(aNode%jac_vert)) deallocate(aNode%jac_vert)

     if(associated(aNode%jac_sigdoti1)) deallocate(aNode%jac_sigdoti1)
     if(associated(aNode%jac_sigdoti2)) deallocate(aNode%jac_sigdoti2)
     if(associated(aNode%jac_sigdoti3)) deallocate(aNode%jac_sigdoti3)
     if(associated(aNode%jac_sigdoti1)) deallocate(aNode%jac_sigdoti4)

     if(associated(aNode%jac_qi1 )) deallocate(aNode%jac_qi1 )
     if(associated(aNode%jac_qi2 )) deallocate(aNode%jac_qi2 )
     if(associated(aNode%jac_qi3 )) deallocate(aNode%jac_qi3 )
     if(associated(aNode%jac_qi4 )) deallocate(aNode%jac_qi4 )

     if(associated(aNode%jac_ti1 )) deallocate(aNode%jac_ti1 )
     if(associated(aNode%jac_ti2 )) deallocate(aNode%jac_ti2 )
     if(associated(aNode%jac_ti3 )) deallocate(aNode%jac_ti3 )
     if(associated(aNode%jac_ti4 )) deallocate(aNode%jac_ti4 )

     if(associated(aNode%jac_qgmai1)) deallocate(aNode%jac_qgmai1)
     if(associated(aNode%jac_qgmai2)) deallocate(aNode%jac_qgmai2)
     if(associated(aNode%jac_qgmai3)) deallocate(aNode%jac_qgmai3)
     if(associated(aNode%jac_qgmai4)) deallocate(aNode%jac_qgmai4)

     if(associated(aNode%jac_qgmbi1)) deallocate(aNode%jac_qgmbi1)
     if(associated(aNode%jac_qgmbi2)) deallocate(aNode%jac_qgmbi2)
     if(associated(aNode%jac_qgmbi3)) deallocate(aNode%jac_qgmbi3)
     if(associated(aNode%jac_qgmbi4)) deallocate(aNode%jac_qgmbi4)

     if(associated(aNode%jac_icei1)) deallocate(aNode%jac_icei1)
     if(associated(aNode%jac_icei2)) deallocate(aNode%jac_icei2)
     if(associated(aNode%jac_icei3)) deallocate(aNode%jac_icei3)
     if(associated(aNode%jac_icei4)) deallocate(aNode%jac_icei4)
 
     if(associated(aNode%jac_zicei1)) deallocate(aNode%jac_zicei1)
     if(associated(aNode%jac_zicei2)) deallocate(aNode%jac_zicei2)
     if(associated(aNode%jac_zicei3)) deallocate(aNode%jac_zicei3)
     if(associated(aNode%jac_zicei4)) deallocate(aNode%jac_zicei4)

     if(associated(aNode%kboti1)) deallocate(aNode%kboti1)
     if(associated(aNode%kboti2)) deallocate(aNode%kboti2)
     if(associated(aNode%kboti3)) deallocate(aNode%kboti3)
     if(associated(aNode%kboti4)) deallocate(aNode%kboti4)

     if(associated(aNode%jac_kverti1)) deallocate(aNode%jac_kverti1)
     if(associated(aNode%jac_kverti2)) deallocate(aNode%jac_kverti2)
     if(associated(aNode%jac_kverti3)) deallocate(aNode%jac_kverti3)
     if(associated(aNode%jac_kverti4)) deallocate(aNode%jac_kverti4)

     if(associated(aNode%jac_fratei1)) deallocate(aNode%jac_fratei1)
     if(associated(aNode%jac_fratei2)) deallocate(aNode%jac_fratei2)
     if(associated(aNode%jac_fratei3)) deallocate(aNode%jac_fratei3)
     if(associated(aNode%jac_fratei4)) deallocate(aNode%jac_fratei4)

     if(associated(aNode%jac_wmaxflagi1)) deallocate(aNode%jac_wmaxflagi1)
     if(associated(aNode%jac_wmaxflagi2)) deallocate(aNode%jac_wmaxflagi2)
     if(associated(aNode%jac_wmaxflagi3)) deallocate(aNode%jac_wmaxflagi3)
     if(associated(aNode%jac_wmaxflagi4)) deallocate(aNode%jac_wmaxflagi4)
     if(associated(aNode%ij            )) deallocate(aNode%ij            )    
_EXIT_(myname_)
  return
end subroutine obsNode_clean_

subroutine obsNode_xread_(aNode,iunit,istat,diagLookup,skip)
  use m_obsdiagNode, only: obsdiagLookup_locate
  implicit none
  class(lightNode) , intent(inout):: aNode
  integer(i_kind)  , intent(in   ):: iunit
  integer(i_kind)  , intent(  out):: istat
  type(obs_diags)  , intent(in   ):: diagLookup
  logical,optional , intent(in   ):: skip

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xread_'
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
    read(iunit,iostat=istat)    aNode%res           , &
                                aNode%err2          , &
                                aNode%raterr2       , &
                                aNode%b             , &
                                aNode%pg            , & 
                                aNode%jac_z0i1      , &
                                aNode%jac_z0i2      , &
                                aNode%jac_z0i3      , &
                                aNode%jac_z0i4      , &
                                aNode%jac_z0i5      , &
                                aNode%jac_z0i6      , &
                                aNode%jac_z0i7      , &
                                aNode%jac_z0i8      , &
                                aNode%jac_z0i9      , &
                                aNode%jac_z0i10     , &
                                aNode%jac_z0i11     , &
                                aNode%jac_z0i12     , & 
                                aNode%jac_vertqi1   , & !(   nsig)
                                aNode%jac_vertqi2   , & !(   nsig)
                                aNode%jac_vertqi3   , & !(   nsig)
                                aNode%jac_vertqi4   , & !(   nsig)
                                aNode%jac_vertqi5   , & !(   nsig)
                                aNode%jac_vertqi6   , & !(   nsig)
                                aNode%jac_vertqi7   , & !(   nsig)
                                aNode%jac_vertqi8   , & !(   nsig)
                                aNode%jac_vertqi9   , & !(   nsig)
                                aNode%jac_vertqi10  , & !(   nsig)
                                aNode%jac_vertqi11  , & !(   nsig)
                                aNode%jac_vertqi12  , & !(   nsig)
                                aNode%jac_vertti1   , & !(   nsig)
                                aNode%jac_vertti2   , & !(   nsig)
                                aNode%jac_vertti3   , & !(   nsig)
                                aNode%jac_vertti4   , & !(   nsig)
                                aNode%jac_vertti5   , & !(   nsig)
                                aNode%jac_vertti6   , & !(   nsig)
                                aNode%jac_vertti7   , & !(   nsig)
                                aNode%jac_vertti8   , & !(   nsig)
                                aNode%jac_vertti9   , & !(   nsig)
                                aNode%jac_vertti10  , & !(   nsig)
                                aNode%jac_vertti11  , & !(   nsig)
                                aNode%jac_vertti12  , & !(   nsig)
                                aNode%jac_zdxi1     , & !(   nsig)
                                aNode%jac_zdxi2     , & !(   nsig)
                                aNode%jac_zdxi3     , & !(   nsig)
                                aNode%jac_zdxi4     , & !(   nsig)
                                aNode%jac_zdyi1     , & !(   nsig)
                                aNode%jac_zdyi2     , & !(   nsig)
                                aNode%jac_zdyi3     , & !(   nsig)
                                aNode%jac_zdyi4     , & !(   nsig)
                                aNode%jac_udxi1     , & !(   nsig)
                                aNode%jac_udxi2     , & !(   nsig)
                                aNode%jac_udxi3     , & !(   nsig)
                                aNode%jac_udxi4     , & !(   nsig)
                                aNode%jac_vdyi1     , & !(   nsig)
                                aNode%jac_vdyi2     , & !(   nsig)
                                aNode%jac_vdyi3     , & !(   nsig)
                                aNode%jac_vdyi4     , & !(   nsig)
                                aNode%jac_vert      , & !(   nsig)
                                aNode%jac_sigdoti1  , & !(   nsig)
                                aNode%jac_sigdoti2  , & !(   nsig)
                                aNode%jac_sigdoti3  , & !(   nsig)
                                aNode%jac_sigdoti4  , & !(   nsig)
                                aNode%jac_qi1       , & !(   nsig)
                                aNode%jac_qi2       , & !(   nsig)
                                aNode%jac_qi3       , & !(   nsig)
                                aNode%jac_qi4       , & !(   nsig)
                                aNode%jac_ti1       , & !(   nsig)
                                aNode%jac_ti2       , & !(   nsig)
                                aNode%jac_ti3       , & !(   nsig)
                                aNode%jac_ti4       , & !(   nsig)
                                aNode%jac_qgmai1    , & !(   nsig)
                                aNode%jac_qgmai2    , & !(   nsig)
                                aNode%jac_qgmai3    , & !(   nsig)
                                aNode%jac_qgmai4    , & !(   nsig)
                                aNode%jac_qgmbi1    , & !(   nsig)
                                aNode%jac_qgmbi2    , & !(   nsig)
                                aNode%jac_qgmbi3    , & !(   nsig)
                                aNode%jac_qgmbi4    , & !(   nsig)
                                aNode%jac_icei1     , & !(   nsig)
                                aNode%jac_icei2     , & !(   nsig)
                                aNode%jac_icei3     , & !(   nsig)
                                aNode%jac_icei4     , & !(   nsig)
                                aNode%jac_zicei1    , & !(   nsig)
                                aNode%jac_zicei2    , & !(   nsig)
                                aNode%jac_zicei3    , & !(   nsig)
                                aNode%jac_zicei4    , & !(   nsig)
                                aNode%kboti1        , & 
                                aNode%kboti2        , & 
                                aNode%kboti3        , &
                                aNode%kboti4        , & 
                                aNode%jac_kverti1   , &
                                aNode%jac_kverti2   , &
                                aNode%jac_kverti3   , &
                                aNode%jac_kverti4   , &
                                aNode%jac_fratei1   , &
                                aNode%jac_fratei2   , &
                                aNode%jac_fratei3   , &
                                aNode%jac_fratei4   , &
                                aNode%jac_wmaxflagi1, &
                                aNode%jac_wmaxflagi2, &
                                aNode%jac_wmaxflagi3, &
                                aNode%jac_wmaxflagi4, &
                                aNode%wij           , & !(4)
                                aNode%ij                !(12,nsig)
    if (istat/=0) then
       call perr(myname_,'read(%(res,err2,...)), iostat =',istat)
       _EXIT_(myname_)
       return
    end if

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
  class(lightNode),intent(in):: aNode
  integer(i_kind),intent(in   ):: junit
  integer(i_kind),intent(  out):: jstat

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_xwrite_'
_ENTRY_(myname_)

  jstat=0
  write(junit,iostat=jstat)     aNode%res           , &
                                aNode%err2          , &
                                aNode%raterr2       , &
                                aNode%b             , &
                                aNode%pg            , &
                                aNode%jac_z0i1      , &
                                aNode%jac_z0i2      , &
                                aNode%jac_z0i3      , &
                                aNode%jac_z0i4      , &
                                aNode%jac_z0i5      , &
                                aNode%jac_z0i6      , &
                                aNode%jac_z0i7      , &
                                aNode%jac_z0i8      , &
                                aNode%jac_z0i9      , &
                                aNode%jac_z0i10     , &
                                aNode%jac_z0i11     , &
                                aNode%jac_z0i12     , &
                                aNode%jac_vertqi1   , & !(   nsig)
                                aNode%jac_vertqi2   , & !(   nsig)
                                aNode%jac_vertqi3   , & !(   nsig)
                                aNode%jac_vertqi4   , & !(   nsig)
                                aNode%jac_vertqi5   , & !(   nsig)
                                aNode%jac_vertqi6   , & !(   nsig)
                                aNode%jac_vertqi7   , & !(   nsig)
                                aNode%jac_vertqi8   , & !(   nsig)
                                aNode%jac_vertqi9   , & !(   nsig)
                                aNode%jac_vertqi10  , & !(   nsig)
                                aNode%jac_vertqi11  , & !(   nsig)
                                aNode%jac_vertqi12  , & !(   nsig)
                                aNode%jac_vertti1   , & !(   nsig)
                                aNode%jac_vertti2   , & !(   nsig)
                                aNode%jac_vertti3   , & !(   nsig)
                                aNode%jac_vertti4   , & !(   nsig)
                                aNode%jac_vertti5   , & !(   nsig)
                                aNode%jac_vertti6   , & !(   nsig)
                                aNode%jac_vertti7   , & !(   nsig)
                                aNode%jac_vertti8   , & !(   nsig)
                                aNode%jac_vertti9   , & !(   nsig)
                                aNode%jac_vertti10  , & !(   nsig)
                                aNode%jac_vertti11  , & !(   nsig)
                                aNode%jac_vertti12  , & !(   nsig)
                                aNode%jac_zdxi1     , & !(   nsig)
                                aNode%jac_zdxi2     , & !(   nsig)
                                aNode%jac_zdxi3     , & !(   nsig)
                                aNode%jac_zdxi4     , & !(   nsig)
                                aNode%jac_zdyi1     , & !(   nsig)
                                aNode%jac_zdyi2     , & !(   nsig)
                                aNode%jac_zdyi3     , & !(   nsig)
                                aNode%jac_zdyi4     , & !(   nsig)
                                aNode%jac_udxi1     , & !(   nsig)
                                aNode%jac_udxi2     , & !(   nsig)
                                aNode%jac_udxi3     , & !(   nsig)
                                aNode%jac_udxi4     , & !(   nsig)
                                aNode%jac_vdyi1     , & !(   nsig)
                                aNode%jac_vdyi2     , & !(   nsig)
                                aNode%jac_vdyi3     , & !(   nsig)
                                aNode%jac_vdyi4     , & !(   nsig)
                                aNode%jac_vert      , & !(   nsig)
                                aNode%jac_sigdoti1  , & !(   nsig)
                                aNode%jac_sigdoti2  , & !(   nsig)
                                aNode%jac_sigdoti3  , & !(   nsig)
                                aNode%jac_sigdoti4  , & !(   nsig)
                                aNode%jac_qi1       , & !(   nsig)
                                aNode%jac_qi2       , & !(   nsig)
                                aNode%jac_qi3       , & !(   nsig)
                                aNode%jac_qi4       , & !(   nsig)
                                aNode%jac_ti1       , & !(   nsig)
                                aNode%jac_ti2       , & !(   nsig)
                                aNode%jac_ti3       , & !(   nsig)
                                aNode%jac_ti4       , & !(   nsig)
                                aNode%jac_qgmai1    , & !(   nsig)
                                aNode%jac_qgmai2    , & !(   nsig)
                                aNode%jac_qgmai3    , & !(   nsig)
                                aNode%jac_qgmai4    , & !(   nsig)
                                aNode%jac_qgmbi1    , & !(   nsig)
                                aNode%jac_qgmbi2    , & !(   nsig)
                                aNode%jac_qgmbi3    , & !(   nsig)
                                aNode%jac_qgmbi4    , & !(   nsig)
                                aNode%jac_icei1     , & !(   nsig)
                                aNode%jac_icei2     , & !(   nsig)
                                aNode%jac_icei3     , & !(   nsig)
                                aNode%jac_icei4     , & !(   nsig)
                                aNode%jac_zicei1    , & !(   nsig)
                                aNode%jac_zicei2    , & !(   nsig)
                                aNode%jac_zicei3    , & !(   nsig)
                                aNode%jac_zicei4    , & !(   nsig)
                                aNode%kboti1        , & 
                                aNode%kboti2        , & 
                                aNode%kboti3        , & 
                                aNode%kboti4        , & 
                                aNode%jac_kverti1   , &
                                aNode%jac_kverti2   , &
                                aNode%jac_kverti3   , &
                                aNode%jac_kverti4   , &
                                aNode%jac_fratei1   , &
                                aNode%jac_fratei2   , &
                                aNode%jac_fratei3   , &
                                aNode%jac_fratei4   , &
                                aNode%jac_wmaxflagi1, &
                                aNode%jac_wmaxflagi2, &
                                aNode%jac_wmaxflagi3, &
                                aNode%jac_wmaxflagi4, &
                                aNode%wij           , & !(4)
                                aNode%ij                !(12,nsig)
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
  use gridmod, only: nsig,latlon11
  implicit none
  class(lightNode),intent(inout):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_setHop_'
  integer(i_kind):: k
_ENTRY_(myname_)

  ASSERT(size(aNode%ij,2)==nsig)
  ASSERT(nsig>0)

  call cvgridLookup_getiw(aNode%elat,aNode%elon,aNode%ij(:,1),aNode%wij)
  do k=2,nsig
    aNode%ij(:,k) = aNode%ij(:,1)+(k-1)*latlon11
  enddo
_EXIT_(myname_)
  return
end subroutine obsNode_setHop_

function obsNode_isvalid_(aNode) result(isvalid_)
  implicit none
  logical:: isvalid_
  class(lightNode),intent(in):: aNode

  character(len=*),parameter:: myname_=MYNAME//'::obsNode_isvalid_'
_ENTRY_(myname_)
  isvalid_=associated(aNode%diags)
_EXIT_(myname_)
end function obsNode_isvalid_

pure subroutine gettlddp_(aNode,jiter,tlddp,nob)
  use kinds, only: r_kind
  implicit none
  class(lightNode), intent(in):: aNode
  integer(kind=i_kind),intent(in):: jiter
  real(kind=r_kind),intent(inout):: tlddp
  integer(kind=i_kind),optional,intent(inout):: nob

  tlddp = tlddp + aNode%diags%tldepart(jiter)*aNode%diags%tldepart(jiter)
  if(present(nob)) nob=nob+1
  return
end subroutine gettlddp_

end module m_lightNode
