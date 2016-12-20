!
! !MODULE: Nst_Var_ESMFMod  ---                Definition of the Nst_Var model
!                                           fields in the ESMF internal state.
!
! !DESCRIPTION: Nst_Var_ESMFMod ---            Define the Nst_Var model  variables
!                                            in the ESMF internal state.
!---------------------------------------------------------------------------
! !REVISION HISTORY:
!
!  May 2008      Shrinivas Moorthi Initial code.
!  Aug 2009      Xu Li for DTM-1p
!  Mar 2014      Fanglin Yang   removed pointers for fixing digital filter
!
! !INTERFACE:
!
 MODULE Nst_Var_ESMFMod

 use machine , only : kind_phys

 IMPLICIT none

 TYPE Nst_Var_Data
    real(kind_phys),allocatable:: slmsk    (:,:)
    real(kind_phys),allocatable:: xt       (:,:)
    real(kind_phys),allocatable:: xs       (:,:)
    real(kind_phys),allocatable:: xu       (:,:)
    real(kind_phys),allocatable:: xv       (:,:)
    real(kind_phys),allocatable:: xz       (:,:)
    real(kind_phys),allocatable:: zm       (:,:)
    real(kind_phys),allocatable:: xtts     (:,:)
    real(kind_phys),allocatable:: xzts     (:,:)
    real(kind_phys),allocatable:: dt_cool  (:,:)
    real(kind_phys),allocatable:: z_c      (:,:)
    real(kind_phys),allocatable:: c_0      (:,:)
    real(kind_phys),allocatable:: c_d      (:,:)
    real(kind_phys),allocatable:: w_0      (:,:)
    real(kind_phys),allocatable:: w_d      (:,:)
    real(kind_phys),allocatable:: d_conv   (:,:)
    real(kind_phys),allocatable:: ifd      (:,:)
    real(kind_phys),allocatable:: tref     (:,:)
    real(kind_phys),allocatable:: Qrain    (:,:)
 end type Nst_Var_Data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    contains
    subroutine nstvar_aldata(dim1,dim2,data,iret)
       implicit none
       integer, intent(in)               :: dim1, dim2
       type(nst_var_data),intent(inout)  :: data
       integer, intent(out)              :: iret
!
allocate(                         &
      data%slmsk   (dim1,dim2),   &
      data%xt      (dim1,dim2),   &
      data%xs      (dim1,dim2),   &
      data%xu      (dim1,dim2),   &
      data%xv      (dim1,dim2),   &
      data%xz      (dim1,dim2),   &
      data%zm      (dim1,dim2),   &
      data%xtts    (dim1,dim2),   &
      data%xzts    (dim1,dim2),   &
      data%dt_cool (dim1,dim2),   &
      data%z_c     (dim1,dim2),   &
      data%c_0     (dim1,dim2),   &
      data%c_d     (dim1,dim2),   &
      data%w_0     (dim1,dim2),   &
      data%w_d     (dim1,dim2),   &
      data%d_conv  (dim1,dim2),   &
      data%ifd     (dim1,dim2),   &
      data%tref    (dim1,dim2),   &
      data%Qrain   (dim1,dim2),   &
      stat=iret)
    if(iret.ne.0) iret=-3
    return
  end subroutine nstvar_aldata
 END MODULE Nst_Var_ESMFMod
