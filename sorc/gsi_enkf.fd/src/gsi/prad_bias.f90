module m_prad
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:	 module m_prad
!   prgmmr:	 j guo <jguo@nasa.gov>
!      org:	 NASA/GSFC, Global Modeling and Assimilation Office, 610.3
!     date:	 2015-08-28
!
! abstract: a modular version of prad_bias and related data
!
! program history log:
!   2015-08-28  j guo    - created this module on top of prad_bias();
!                        . completed with data components from obsmod;
!                        . changed code where this module needs to be used;
!                        . added this document block;
!                        . for earlier history log, see the history log section
!                          inside ::prad_bias() below.
!   2015-09-03  j guo   - removed all older interface names.
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
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

  use m_obsNode , only: obsNode
  use m_radNode , only: radNode
  use m_radNode , only: radNode_typecast
  use m_radNode , only: radNode_nextcast
  use m_obsLList, only: obsLList
  use m_obsLList, only: obsLList_headNode
  use m_obsLList, only: obsLList_reset
  implicit none
  
        ! Data objects:

  public:: radheadm

        ! interfaces:

  public:: prad_create          !, create_passive_obsmod_vars
  public:: prad_destroy         !, destroyobs_passive

        interface prad_create ; module procedure  create_passive_obsmod_vars; end interface
        interface prad_destroy; module procedure          destroyobs_passive; end interface

  public:: prad_updatePredx     !, prad_bias
        interface prad_updatePredx ; module procedure prad_bias; end interface

        ! Synopsis:
        !       - []_create: allocated in gsimod::gsimain_initialize(), through
        !         ::create_passive_obsmod_vars();
        !
        !       - externally built, node-by-node, in setuprad();
        !
        !       - []_updatePredx: used to update radinfo::predx, in glbsoi(), through
        !         ::prad_bias();
        !
        !       - []_destroy: deallocated in gsimod::gsimain_finalize(), through
        !         ::create_passive_obsmod_vars().

!   def radheadm     - radiance linked list head for monitored radiance data

  type(obsLList),dimension(:),pointer :: radheadm => null()

  type(radNode),target,save:: radNode_mold

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  character(len=*),parameter :: myname='prad_bias'

contains
! ----------------------------------------------------------------------
  subroutine create_passive_obsmod_vars
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    create_passive_obsmod_vars
!     prgmmr:    zhu            org: np23           date: 2010-05-12
!
! abstract:  allocate arrays to hold observation related information
!
! program history log:
!   2010-05-12 zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$ end documentation block
    use gsi_4dvar, only: nobs_bins
    implicit none

    ALLOCATE(radheadm(nobs_bins))
    call lreset_(radheadm(:))
    return
  end subroutine create_passive_obsmod_vars

  subroutine lreset_(llists)
    use kinds, only: i_kind
    implicit none
    type(obsLList),dimension(:),intent(inout) :: llists
    integer(kind=i_kind):: ib
    do ib=lbound(llists,1),ubound(llists,1)
      call obsLList_reset(llists(ib),mold=radNode_mold)
    enddo
  end subroutine lreset_

! ----------------------------------------------------------------------
  subroutine destroyobs_passive
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    destroyobs_passive
!     prgmmr:    zhu            org: np23           date: 2010-05-12
!
! abstract:  deallocate arrays that hold observation information for
!            use in outer and inner loops
!
! program history log:
!   2010-05-12  zhu
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$  end documentation block
    implicit none

    if(associated(radheadm)) then
      call lreset_(radheadm(:))
      deallocate(radheadm)
    endif
    return
  end subroutine destroyobs_passive

! ----------------------------------------------------------------------
  subroutine prad_bias()
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    prad_bias    compute bias coefficients for passive radiances
!   prgmmr: zhu           org: np23                date: 2010-05-15
!
! abstract: compute bias coefficients for passive radiances 
!
! program history log:
!   2010-05-15  zhu
!   2013-10-27  todling - move destroy to module where init reside (gsimod)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$

  use kinds, only: r_kind,i_kind,r_quad
  use mpimod, only: mype
  use radinfo, only: npred,jpch_rad,iuse_rad,inew_rad   ! intent(in)
  use radinfo, only: predx      ! intent(inout)
  use gsi_4dvar, only: nobs_bins
  use berror, only: varprd      ! intent(in)
  use mpl_allreducemod, only: mpl_allreduce
  use timermod, only: timer_ini,timer_fnl
  use constants, only : zero,one,zero_quad
  implicit none

  integer(i_kind),parameter :: nthreshold=100
  real(r_kind),parameter :: atiny=1.0e-10_r_kind
  integer(i_kind) i,n,j,ii,jj,jpassive,ibin,ic,mp,mm,kpred
  integer(i_kind),dimension(jpch_rad) :: icp
  integer(i_kind),dimension(npred)    :: iorder
  real(r_kind) varc
  real(r_quad),dimension(npred,npred) :: Atmp

  real(r_kind),allocatable,dimension(:) :: iobs
  real(r_quad),allocatable,dimension(:,:,:) :: A
  real(r_quad),allocatable,dimension(:,:) :: b
  real(r_kind),allocatable,dimension(:,:) :: AA
  real(r_kind),allocatable,dimension(:) :: be

  class(obsNode),pointer:: obsptrm
  type(radNode),pointer:: radptrm

! Initialize timer
  call timer_ini('prad_bias')

! Allocate arrays
  icp      = 0
  jpassive = 0
  do j=1,jpch_rad
     if (iuse_rad(j)==-1 .or. iuse_rad(j)==0) then 
        jpassive=jpassive+1
        icp(j) = jpassive
     end if
  end do
  if (mype==0) write(6,*) 'prad_bias: number of passive channels=', jpassive 
  if (jpassive<1) return

! Allocate arrays and initialize
  allocate(A(npred,npred,jpassive),b(npred,jpassive))
  allocate(iobs(jpassive))
  do n=1,jpassive
     iobs(n)=zero
     do j=1,npred
        b(j,n)=zero_quad
        do i=1,npred
           A(i,j,n)=zero_quad
        end do
     end do
  end do

! Big loop for observations
  do ibin=1,nobs_bins
     !radptrm => obsLList_headNode(radheadm(ibin))
     obsptrm => obsLList_headNode(radheadm(ibin))
     radptrm => radNode_typecast(obsptrm)

     do while (associated(radptrm))

        if (radptrm%luse) then

!          begin channel specific calculations
           do n=1,radptrm%nchan
              ic=radptrm%icx(n)
              if (inew_rad(ic) .and. all(predx(:,ic)==zero)) cycle 

              mp=icp(ic)

!             Assign arrays Ax=b              
              varc=radptrm%err2(n)*radptrm%raterr2(n)
              if (sqrt(varc)<atiny) cycle
              iobs(mp)=iobs(mp)+one
              do i=1,npred
                 do j=1,npred
                    A(i,j,mp)=A(i,j,mp)+radptrm%pred(i,n)*radptrm%pred(j,n)*varc
                 end do
              end do
              do i=1,npred
                 b(i,mp)=b(i,mp)+radptrm%pred(i,n)*radptrm%res(n)*varc
              end do

           end do  ! <n channel loop>
        end if  ! <luse>

        ! radptrm => radptrm%llpoint
        radptrm => radNode_nextcast(radptrm)
     end do
  end do  ! <ibin loop>


! Collect data from all processors
  call mpl_allreduce(jpassive,rpvals=iobs)
  call mpl_allreduce(npred,jpassive,b)

  do n = 1,jpassive
     if (iobs(n)<nthreshold) cycle

     do i=1,jpch_rad
        if (icp(i)==n) then
           mm=i
           exit
        end if
     end do

!    Collect data from all processors for each channel
     Atmp(:,:) = A(:,:,n)
     call mpl_allreduce(npred,npred,Atmp)

!    Solve linear system
     iorder=0
     kpred=0
     do i=1,npred
        use: do j=1,npred
           if(Atmp(i,j) /= zero_quad)then
             kpred = kpred+1
             iorder(kpred) = i
             exit use
           end if
        end do use
     end do

     if (kpred==0) cycle

     allocate(AA(kpred,kpred),be(kpred))
     do i = 1,kpred
        ii=iorder(i)
        be(i) = b(ii,n)

        do j = 1,kpred
           jj=iorder(j)
           AA(i,j) = Atmp(ii,jj)
        end do
     end do


     do i = 1,kpred
        ii=iorder(i)
        jj=(mm-1)*npred+ii
        AA(i,i) = AA(i,i)+one/varprd(jj)
     end do
     if (all(abs(AA)<atiny)) cycle
     if (all(abs(be)<atiny)) cycle
     call linmm(AA,be,kpred,1,kpred,kpred)


!    Update bias coefficients for passive channels
     do i=1,kpred
        ii=iorder(i)
        predx(ii,mm)=predx(ii,mm)+be(i)
     end do

     deallocate(AA,be)
  end do  ! end of jpassive loop

  deallocate(A,b,iobs)

! Finalize timer
  call timer_fnl('prad_bias')

! End of routine
  return
  end subroutine prad_bias
end module m_prad
