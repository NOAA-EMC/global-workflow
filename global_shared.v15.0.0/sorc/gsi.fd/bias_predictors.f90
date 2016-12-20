module bias_predictors
!$$$ module documentation block
!           .      .    .                                       .
! module:   bias_predictors
!  prgmmr: tremolet
!
! abstract: define predictors and basic operators
!
! program history log:
!   2007-04-16  tremolet - initial code
!   2009-08-14  lueken - update documentation
!   2012-07-13  todling - add read and write
!   2013-05-21  zhu    - add aircraft temperature bias correction coefficients
!   2014-02-07  todling - move bias preds update inside this module
!
! subroutines included:
!   sub setup_predictors
!   sub allocate_preds
!   sub deallocate_preds
!   sub assign_scalar2preds
!   sub assign_preds2preds
!   sub update_bias_preds
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use kinds, only: r_kind,i_kind
use constants, only : zero
use file_utility, only : get_lun

implicit none
save
private
public predictors, allocate_preds, deallocate_preds, &
     assignment(=), setup_predictors, read_preds, write_preds, &
     update_bias_preds

type predictors
   real(r_kind), pointer :: values(:) => NULL()

   real(r_kind), pointer :: predr(:) => NULL()
   real(r_kind), pointer :: predp(:) => NULL()
   real(r_kind), pointer :: predt(:) => NULL()

   logical :: lallocated = .false.
end type predictors

integer(i_kind) :: nrclen,nsclen,npclen,ntclen

logical :: llinit = .false.

! ----------------------------------------------------------------------
INTERFACE ASSIGNMENT (=)
MODULE PROCEDURE assign_scalar2preds, assign_preds2preds
END INTERFACE
! ----------------------------------------------------------------------
contains
! ----------------------------------------------------------------------
subroutine setup_predictors(krclen,ksclen,kpclen,ktclen)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    setup_predictors
!   prgmmr:                  org:                    date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    krclen
!    ksclen
!    kpclen
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  implicit none
  integer(i_kind), intent(in   ) :: krclen,ksclen,kpclen,ktclen

  nrclen=krclen
  nsclen=ksclen
  npclen=kpclen
  ntclen=ktclen

  llinit = .true.

  return
end subroutine setup_predictors
! ----------------------------------------------------------------------
subroutine allocate_preds(yst)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    allocate_preds
!   prgmmr:                  org:                    date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    yst
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(predictors), intent(inout) :: yst
  integer(i_kind) :: ii

  if (yst%lallocated) then
     write(6,*) ' allocate_preds: vector already allocated'
     call stop2(102)
  end if

  ALLOCATE(yst%values(nrclen))
  yst%values = zero

  ii=0
  yst%predr => yst%values(ii+1:ii+nsclen)
  ii=ii+nsclen
  yst%predp => yst%values(ii+1:ii+npclen)
  ii=ii+npclen
  if (ntclen>0) then
     yst%predt => yst%values(ii+1:ii+ntclen)
     ii=ii+ntclen
  end if

  if (ii/=nrclen) then
     write(6,*)' allocate_preds: error length',ii,nrclen
     call stop2(103)
  end if
  yst%lallocated = .true.

  return
end subroutine allocate_preds
! ----------------------------------------------------------------------
subroutine deallocate_preds(yst)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    deallocate_preds
!   prgmmr:                  org:                    date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    yst
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(predictors), intent(inout) :: yst

  if (yst%lallocated) then 
     NULLIFY(yst%predr)
     NULLIFY(yst%predp)
     NULLIFY(yst%predt)
     DEALLOCATE(yst%values)
     yst%lallocated = .false.
  else
     write(6,*) 'deallocate_preds warning: trying to dealloc() vector not allocated'
  endif

  return
end subroutine deallocate_preds
! ----------------------------------------------------------------------
subroutine assign_scalar2preds(yst,pval)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    assign_scalar2preds
!   prgmmr:                  org:                    date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    yst
!    pval
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(predictors), intent(inout) :: yst
  real(r_kind)    , intent(in   ) :: pval
  integer(i_kind) :: ii

  DO ii=1,nrclen
     yst%values(ii)=pval
  ENDDO

  return
end subroutine assign_scalar2preds
! ----------------------------------------------------------------------
subroutine assign_preds2preds(yst,xst)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    assign_preds2preds
!   prgmmr:                  org:                    date:
!
! abstract:
!
! program history log:
!   2009-08-04  lueken - added subprogram doc block
!
!   input argument list:
!    yst
!    xst
!
!   output argument list:
!    yst
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(predictors), intent(inout) :: yst
  type(predictors), intent(in   ) :: xst
  integer(i_kind) :: ii

  DO ii=1,nrclen
     yst%values(ii)=xst%values(ii)
  ENDDO

  return
end subroutine assign_preds2preds
! ----------------------------------------------------------------------
subroutine read_preds (yst,filename)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    read_preds
!   prgmmr: todling          org:                    date:
!
! abstract:
!
! program history log:
!   2012-07-13  todling - initial code
!   2014-01-27  todling - add support for aircraft bias
!
!   input argument list:
!    yst
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(predictors), intent(inout) :: yst
  character(len=*), intent(in)    :: filename

  integer(i_kind) :: ii,iunit,nsclen_in,npclen_in,ntclen_in
  real(r_kind),allocatable,dimension(:)::preds,predp,predt
  logical :: allwell
  
  allwell=.true.
  allocate(preds(nsclen),predp(npclen),predt(ntclen))

  iunit=get_lun()
  open(iunit,file=trim(filename),form='unformatted')
  read(iunit)nsclen_in,npclen_in,ntclen_in
  if(nsclen_in/=nsclen .or. npclen_in/=npclen) then
     allwell=.false.
  else
     if (ntclen_in>0) then
        read(iunit)preds,predp,predt
     else
        read(iunit)preds,predp
     endif
  endif
  close(iunit)

  if (.not.allwell) then
     write(6,*) ' read_preds: vector already allocated'
     call stop2(102)
  endif

  ii=0
  yst%values(ii+1:ii+nsclen) = preds
  yst%values(ii+1:ii+npclen) = predp
  if(ntclen_in>0) then
    yst%values(ii+1:ii+ntclen) = predt
  endif
  deallocate(preds,predp,predt)
  return
end subroutine read_preds
subroutine write_preds (yst,filename,mype)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    write_preds
!   prgmmr: todling         org:                    date:
!
! abstract:
!
! program history log:
!   2012-07-13  todling - initial code
!   2014-01-27  todling - add support for aircraft bias
!
!   input argument list:
!    yst
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  implicit none
  type(predictors), intent(in) :: yst
  character(len=*), intent(in) :: filename
  integer(i_kind),  intent(in) :: mype

  integer(i_kind) :: ii,iunit
  real(r_kind),allocatable,dimension(:)::preds,predp,predt
  
  allocate(preds(nsclen),predp(npclen),predt(ntclen))
  ii=0
  preds = yst%values(ii+1:ii+nsclen)
  predp = yst%values(ii+1:ii+npclen)
  if(ntclen>0) then
    predt = yst%values(ii+1:ii+ntclen)
  endif

  iunit=get_lun()
  if (mype==0) then
     open(iunit,file=trim(filename),form='unformatted')
     write(iunit)nsclen,npclen,ntclen
     if(ntclen>0) then
        write(iunit)preds,predp,predt
     else
        write(iunit)preds,predp
     endif
     close(iunit)
  endif

  deallocate(preds,predp,predt)
  return
end subroutine write_preds
subroutine update_bias_preds(regional2d,sbias)
!$$$  subprogram documentation block
!                .      .    .                                      .
! subprogram:    update_bias_preds
!   prgmmr: todling          org:                    date: 2014-03-08
!
! abstract:
!
! program history log:
!   2014-03-08  todling - added to update bias
!
!   input argument list:
!    regional2d - exception for 2dvar regional
!    sbias      - input bias parameters
!
!   output argument list:
!    sbias      - updated bias
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use aircraftinfo, only: aircraft_t_bc_pof,aircraft_t_bc,npredt,predt,ntail
  use radinfo, only: npred,jpch_rad,predx
  use pcpinfo, only: npredp,npcptype,predxp
  implicit none
  logical, intent(in) :: regional2d
  type(predictors), intent(inout) :: sbias

! local variables
  integer(i_kind) i,j,ij

! Update bias correction coefficients.
! Not necessary if running in 2dvar mode.

  if (.not.regional2d) then

!    Satellite radiance biases
     ij=0
     do j=1,jpch_rad
        do i=1,npred
           ij=ij+1
           predx(i,j)=predx(i,j)+sbias%predr(ij)
        end do
     end do

!    Precipitation biases
     ij=0
     do j=1,npcptype
        do i=1,npredp
           ij=ij+1
           predxp(i,j)=predxp(i,j)+sbias%predp(ij)
        end do
     end do

!    Aircraft temperature bias 
     if (aircraft_t_bc_pof .or. aircraft_t_bc) then 
        ij=0
        do j=1,ntail
           do i=1,npredt
              ij=ij+1
              predt(i,j)=predt(i,j)+sbias%predt(ij)
           end do
        end do
     end if
  endif
end subroutine update_bias_preds
end module bias_predictors
