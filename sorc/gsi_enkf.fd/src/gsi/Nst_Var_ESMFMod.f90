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
!  Apr 2014      Xu Li   introduce to and modified for GSI (from GSM)
!
! !INTERFACE:
!
 MODULE Nst_Var_ESMFMod

 use kinds, only: i_kind,r_kind

 IMPLICIT none

 TYPE Nst_Var_Data
    real(r_kind),allocatable:: slmsk    (:,:)
    real(r_kind),allocatable:: xt       (:,:)
    real(r_kind),allocatable:: xs       (:,:)
    real(r_kind),allocatable:: xu       (:,:)
    real(r_kind),allocatable:: xv       (:,:)
    real(r_kind),allocatable:: xz       (:,:)
    real(r_kind),allocatable:: zm       (:,:)
    real(r_kind),allocatable:: xtts     (:,:)
    real(r_kind),allocatable:: xzts     (:,:)
    real(r_kind),allocatable:: dt_cool  (:,:)
    real(r_kind),allocatable:: z_c      (:,:)
    real(r_kind),allocatable:: c_0      (:,:)
    real(r_kind),allocatable:: c_d      (:,:)
    real(r_kind),allocatable:: w_0      (:,:)
    real(r_kind),allocatable:: w_d      (:,:)
    real(r_kind),allocatable:: d_conv   (:,:)
    real(r_kind),allocatable:: ifd      (:,:)
    real(r_kind),allocatable:: tref     (:,:)
    real(r_kind),allocatable:: Qrain    (:,:)
 end type Nst_Var_Data
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   contains
   subroutine nstvar_aldata(dim1,dim2,data,iret)
      implicit none
      integer(i_kind), intent(in)       :: dim1, dim2
      type(nst_var_data),intent(inout)  :: data
      integer(i_kind), intent(out)      :: iret
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
