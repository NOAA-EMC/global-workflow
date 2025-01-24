submodule(mg_intstate) mg_entrymod
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_entrymod
!   prgmmr: rancic           org: NCEP/EMC            date: 2020
!
! abstract:  Initialize and finialize multigrid Beta filter
!            for modeling of background error covariance
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   mg_initialize -
!   mg_finalize -
!
! Functions Included:
!
! remarks:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpi
use kinds, only: r_kind,i_kind

contains

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine mg_initialize(this,inputfilename,obj_parameter)
implicit none
!**********************************************************************!
!                                                                      !
!   Initialization subroutine                                          !
!                                                     M. Rancic (2020) !
!***********************************************************************
class (mg_intstate_type):: this
character*(*),optional,intent(in) :: inputfilename
class(mg_parameter_type),optional,intent(in)::obj_parameter

!---------------------------------------------------------------------------
!
!               Firs set of subroutines is called only once and serves to 
!               initialte the MGBF run                                 
! 
!---------------------------------------------------------------------------

!****
!**** Initialize run multigrid Beta filter parameters
!****
if (present(inputfilename)) then  
   call this%init_mg_parameter(inputfilename)
elseif (present(obj_parameter)) then
   this%mg_parameter_type=obj_parameter
endif

!****
!**** Initialize MPI
!****
if(this%nxm*this%nym>1) call this%init_mg_MPI

!***
!*** Initialize integration domain
!***
call this%init_mg_domain
if(this%l_loc) then
   call this%init_domain_loc
endif

!---------------------------------------------------------------------------
!
!               All others are function of km2,km3,km,nm,mm,im,jm
!               and needs to be called separately for each application
! 
!---------------------------------------------------------------------------
!***
!*** Define km and WORKA array based on input from mg_parameters and
!*** depending on specific application
!***

!***
!*** Allocate variables, define weights, prepare mapping 
!*** between analysis and filter grid
!***

call this%allocate_mg_intstate

call this%def_offset_coef

call this%def_mg_weights

if(this%mgbf_line) then
   call this%init_mg_line
endif

call this%lsqr_mg_coef 

call this%lwq_vertical_coef(this%lm_a,this%lm,this%cvf1,this%cvf2,this%cvf3,this%cvf4,this%lref)

!***
!*** Just for testing of standalone version. In GSI WORKA will be given
!*** through a separate subroutine 
!***

!call input_3d(WORKA(     1:  lm,:,:),1,1,     1,mm,nm,  lm,mm0,4,3)
!call input_3d(WORKA(  lm+1:2*lm,:,:),1,1,  lm+1,mm,nm,2*lm,mm0,6,5)
!call input_3d(WORKA(2*lm+1:3*lm,:,:),1,1,2*lm+1,mm,nm,3*lm,mm0,2,1)
!call input_3d(WORKA(3*lm+1:4*lm,:,:),1,1,3*lm+1,mm,nm,4*lm,mm0,3,2)
!call input_3d(WORKA(4*lm+1:5*lm,:,:),1,1,4*lm+1,mm,nm,5*lm,mm0,7,3)
!call input_3d(WORKA(5*lm+1:6*lm,:,:),1,1,5*lm+1,mm,nm,6*lm,mm0,4,5)

!call input_3d(WORKA(6*lm+1:6*lm+1,:,:),1,1,6*lm+1,mm,nm,6*lm+1,mm0,2,1)
!call input_3d(WORKA(6*lm+2:6*lm+2,:,:),1,1,6*lm+2,mm,nm,6*lm+2,mm0,4,1)
!call input_3d(WORKA(6*lm+3:6*lm+3,:,:),1,1,6*lm+3,mm,nm,6*lm+3,mm0,5,1)
!call input_3d(WORKA(6*lm+4:6*lm+4,:,:),1,1,6*lm+4,mm,nm,6*lm+4,mm0,7,1)

!-----------------------------------------------------------------------
endsubroutine mg_initialize

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine mg_finalize(this)
!**********************************************************************!
!                                                                      !
!   Finalize multigrid Beta Function                                   !
!                                                     M. Rancic (2020) !
!***********************************************************************
implicit none
class (mg_intstate_type)::this

real(r_kind), allocatable, dimension(:,:):: PA, VA
integer(i_kind):: n,m,L
integer:: nm,mm,lm
!-----------------------------------------------------------------------

if(this%ldelta) then
   !
   ! Horizontal cross-section
   !
   nm=this%nm
   mm=this%mm
   lm=this%lm
endif

if(this%nxm*this%nym>1) call this%barrierMPI

call this%deallocate_mg_intstate          

!-----------------------------------------------------------------------
endsubroutine mg_finalize
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end submodule mg_entrymod
