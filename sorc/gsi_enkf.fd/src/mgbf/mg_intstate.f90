module mg_intstate
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_intstate
!   prgmmr: rancic           org: NCEP/EMC            date: 2020
!
! abstract:  Contains declarations and allocations of internal
!            state variables use for filtering (offset version)
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   allocate_mg_intstate -
!   def_mg_weights -
!   init_mg_line -
!   deallocate_mg_intstate -
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
use jp_pkind2, only: fpi
use jp_pbfil3, only: inimomtab,t22_to_3,tritform,t33_to_6,hextform
use mg_parameter,only: mg_parameter_type
implicit none
type,extends( mg_parameter_type):: mg_intstate_type
real(r_kind), allocatable,dimension(:,:,:):: V
!
! Composite control variable on first generation of filter grid
!
real(r_kind), allocatable,dimension(:,:,:):: VALL
!
! Composite control variable on high generations of filter grid
!
real(r_kind), allocatable,dimension(:,:,:):: HALL

real(r_kind), allocatable,dimension(:,:,:):: a_diff_f
real(r_kind), allocatable,dimension(:,:,:):: a_diff_h
real(r_kind), allocatable,dimension(:,:,:):: b_diff_f
real(r_kind), allocatable,dimension(:,:,:):: b_diff_h

!
! Localization weights
!
real(r_kind), allocatable,dimension(:,:,:):: w1_loc
real(r_kind), allocatable,dimension(:,:,:):: w2_loc
real(r_kind), allocatable,dimension(:,:,:):: w3_loc
real(r_kind), allocatable,dimension(:,:,:):: w4_loc

real(r_kind), allocatable,dimension(:,:):: p_eps
real(r_kind), allocatable,dimension(:,:):: p_del
real(r_kind), allocatable,dimension(:,:):: p_sig
real(r_kind), allocatable,dimension(:,:):: p_rho

real(r_kind), allocatable,dimension(:,:,:):: paspx
real(r_kind), allocatable,dimension(:,:,:):: paspy
real(r_kind), allocatable,dimension(:,:,:):: pasp1
real(r_kind), allocatable,dimension(:,:,:,:):: pasp2
real(r_kind), allocatable,dimension(:,:,:,:,:):: pasp3

real(r_kind), allocatable,dimension(:,:,:):: vpasp2
real(r_kind), allocatable,dimension(:,:,:):: hss2
real(r_kind), allocatable,dimension(:,:,:,:):: vpasp3
real(r_kind), allocatable,dimension(:,:,:,:):: hss3

real(r_kind), allocatable,dimension(:):: ssx
real(r_kind), allocatable,dimension(:):: ssy
real(r_kind), allocatable,dimension(:):: ss1
real(r_kind), allocatable,dimension(:,:):: ss2
real(r_kind), allocatable,dimension(:,:,:):: ss3

integer(fpi), allocatable,dimension(:,:,:):: dixs
integer(fpi), allocatable,dimension(:,:,:):: diys
integer(fpi), allocatable,dimension(:,:,:):: dizs

integer(fpi), allocatable,dimension(:,:,:,:):: dixs3
integer(fpi), allocatable,dimension(:,:,:,:):: diys3
integer(fpi), allocatable,dimension(:,:,:,:):: dizs3

integer(fpi), allocatable,dimension(:,:,:,:):: qcols

integer(i_kind),allocatable,dimension(:):: iref,jref
integer(i_kind),allocatable,dimension(:):: irefq,jrefq
integer(i_kind),allocatable,dimension(:):: irefL,jrefL

integer(i_kind),allocatable,dimension(:):: Lref,Lref_h
real(r_kind),allocatable,dimension(:):: cvf1,cvf2,cvf3,cvf4
real(r_kind),allocatable,dimension(:):: cvh1,cvh2,cvh3,cvh4

real(r_kind),allocatable,dimension(:):: cx0,cx1,cx2,cx3
real(r_kind),allocatable,dimension(:):: cy0,cy1,cy2,cy3

real(r_kind),allocatable,dimension(:):: qx0,qx1,qx2
real(r_kind),allocatable,dimension(:):: qy0,qy1,qy2

real(r_kind),allocatable,dimension(:):: Lx0,Lx1
real(r_kind),allocatable,dimension(:):: Ly0,Ly1

real(r_kind),allocatable,dimension(:):: p_coef,q_coef
real(r_kind),allocatable,dimension(:):: a_coef,b_coef

real(r_kind),allocatable,dimension(:,:):: cf00,cf01,cf02,cf03           &
                                         ,cf10,cf11,cf12,cf13           &
                                         ,cf20,cf21,cf22,cf23           &
                                         ,cf30,cf31,cf32,cf33
contains
  procedure :: allocate_mg_intstate,deallocate_mg_intstate
  procedure :: def_mg_weights,init_mg_line
!from mg_interpolate.f90
  procedure :: def_offset_coef
  procedure :: lsqr_mg_coef,lwq_vertical_coef
  procedure :: lwq_vertical_direct,lwq_vertical_adjoint
  procedure :: lwq_vertical_direct_spec,lwq_vertical_adjoint_spec
  procedure :: l_vertical_direct_spec,l_vertical_adjoint_spec
  procedure :: l_vertical_direct_spec2,l_vertical_adjoint_spec2
  procedure :: lsqr_direct_offset,lsqr_adjoint_offset
  procedure :: quad_direct_offset,quad_adjoint_offset
  procedure :: lin_direct_offset,lin_adjoint_offset
!from mg_bocos.f90
  generic :: boco_2d => boco_2d_g1,boco_2d_gh
  procedure :: boco_2d_g1,boco_2d_gh
  generic :: boco_3d => boco_3d_g1,boco_3d_gh
  procedure :: boco_3d_g1,boco_3d_gh
  generic :: bocoT_2d => bocoT_2d_g1,bocoT_2d_gh
  procedure :: bocoT_2d_g1,bocoT_2d_gh
  generic :: bocoTx => bocoTx_2d_g1,bocoTx_2d_gh
  procedure :: bocoTx_2d_g1,bocoTx_2d_gh
  generic :: bocoTy => bocoTy_2d_g1,bocoTy_2d_gh
  procedure :: bocoTy_2d_g1,bocoTy_2d_gh
  generic :: bocoT_3d => bocoT_3d_g1,bocoT_3d_gh
  procedure :: bocoT_3d_g1,bocoT_3d_gh
  generic :: bocox => bocox_2d_g1,bocox_2d_gh
  procedure :: bocox_2d_g1,bocox_2d_gh
  generic :: bocoy => bocoy_2d_g1,bocoy_2d_gh
  procedure :: bocoy_2d_g1,bocoy_2d_gh
  generic :: upsend_all => upsend_all_g1,upsend_all_gh
  procedure :: upsend_all_g1,upsend_all_gh
  generic :: downsend_all => downsend_all_g2,downsend_all_gh
  procedure :: downsend_all_g2,downsend_all_gh
  procedure :: boco_2d_loc
  procedure :: bocoT_2d_loc
  procedure :: upsend_loc_g12
  procedure :: upsend_loc_g23
  procedure :: upsend_loc_g34
  procedure :: downsend_loc_g43
  procedure :: downsend_loc_g32
  procedure :: downsend_loc_g21
!from mg_generation.f90
  procedure:: upsending_all,downsending_all,weighting_all
  procedure:: upsending,downsending
  procedure:: upsending_highest,downsending_highest
  procedure:: upsending2,downsending2
  procedure:: upsending_ens,downsending_ens
  procedure:: upsending2_ens,downsending2_ens
  procedure:: upsending_ens_nearest,downsending_ens_nearest
  generic :: upsending_loc => upsending_loc_g3,upsending_loc_g4
  procedure:: upsending_loc_g3,upsending_loc_g4
  generic :: downsending_loc => downsending_loc_g3,downsending_loc_g4
  procedure:: downsending_loc_g3,downsending_loc_g4
  procedure:: weighting_helm,weighting,weighting_highest,weighting_ens
  generic :: weighting_loc => weighting_loc_g3,weighting_loc_g4
  procedure:: weighting_loc_g3,weighting_loc_g4
  procedure:: adjoint,direct1
  procedure:: adjoint2,direct2
  procedure:: adjoint_nearest,direct_nearest
  procedure:: adjoint_highest,direct_highest
!from mg_filtering.f90
  procedure :: filtering_procedure
  procedure :: filtering_rad3,filtering_lin3
  procedure :: filtering_rad2_bkg,filtering_lin2_bkg,filtering_fast_bkg
  procedure :: filtering_rad2_ens,filtering_lin2_ens,filtering_fast_ens
  procedure :: filtering_rad_highest
  procedure :: sup_vrbeta1T,sup_vrbeta1,sup_vrbeta3T,sup_vrbeta3
  procedure :: sup_vrbeta1_ens,sup_vrbeta1T_ens
  procedure :: sup_vrbeta1_bkg,sup_vrbeta1T_bkg
!from mg_transfer.f90
  procedure :: anal_to_filt_allmap,filt_to_anal_allmap
  procedure :: anal_to_filt_all,filt_to_anal_all
  procedure :: anal_to_filt_all2,filt_to_anal_all2
  procedure :: composite_to_stack,stack_to_composite
  procedure :: C2S_ens,S2C_ens
  procedure :: anal_to_filt,filt_to_anal
!from mg_entrymod.f90
  procedure :: mg_initialize
  procedure :: mg_finalize
end type mg_intstate_type
interface
!from mg_interpolate.f90
   module subroutine def_offset_coef(this)
     class(mg_intstate_type),target::this
   end subroutine
   module subroutine lsqr_mg_coef(this)
     class(mg_intstate_type),target::this
   end subroutine
   module subroutine lwq_vertical_coef &
        (this,nm_in,im_in,c1,c2,c3,c4,iref_out)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: nm_in,im_in
     real(r_kind), dimension(1:nm_in), intent(out):: c1,c2,c3,c4
     integer(i_kind), dimension(1:nm_in), intent(out):: iref_out
   end subroutine
   module subroutine lwq_vertical_direct &
        (this,km_in,nm_in,imin,imax,jmin,jmax,c1,c2,c3,c4,kref,f,w)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,nm_in,imin,imax,jmin,jmax
     real(r_kind), dimension(1:nm_in), intent(in):: c1,c2,c3,c4
     integer(i_kind), dimension(1:nm_in), intent(in):: kref
     real(r_kind), dimension(1:km_in,imin:imax,jmin:jmax), intent(in):: f
     real(r_kind), dimension(1:nm_in,imin:imax,jmin:jmax), intent(out):: w
   end subroutine
   module subroutine lwq_vertical_adjoint &
        (this,nm_in,km_in,imin,imax,jmin,jmax,c1,c2,c3,c4,kref,w,f)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: nm_in,km_in,imin,imax,jmin,jmax
     real(r_kind), dimension(1:nm_in), intent(in):: c1,c2,c3,c4
     integer(i_kind), dimension(1:nm_in), intent(in):: kref
     real(r_kind), dimension(1:nm_in,imin:imax,jmin:jmax), intent(in):: w
     real(r_kind), dimension(1:km_in,imin:imax,jmin:jmax), intent(out):: f
   end subroutine
   module subroutine lwq_vertical_direct_spec &
        (this,km3_in,km_in,nm_in,imin,imax,jmin,jmax,c1,c2,c3,c4,kref,F,W)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km3_in,km_in,nm_in,imin,imax,jmin,jmax
     real(r_kind), dimension(1:nm_in), intent(in):: c1,c2,c3,c4
     integer(i_kind), dimension(1:nm_in), intent(in):: kref
     real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:km_in), intent(in):: F
     real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:nm_in), intent(out):: W
   end subroutine
   module subroutine lwq_vertical_adjoint_spec &
        (this,km3_in,nm_in,km_in,imin,imax,jmin,jmax,c1,c2,c3,c4,kref,W,F)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km3_in,nm_in,km_in,imin,imax,jmin,jmax
     real(r_kind), dimension(1:nm_in), intent(in):: c1,c2,c3,c4
     integer(i_kind), dimension(1:nm_in), intent(in):: kref
     real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:nm_in), intent(in):: W
     real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:km_in), intent(out):: F
   end subroutine
   module subroutine l_vertical_direct_spec &
        (this,km3_in,km_in,nm_in,imin,imax,jmin,jmax,F,W)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km3_in,km_in,nm_in,imin,imax,jmin,jmax
     real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:km_in), intent(in):: F
     real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:nm_in), intent(out):: W
   end subroutine
   module subroutine l_vertical_adjoint_spec &
        (this,km3_in,nm_in,km_in,imin,imax,jmin,jmax,W,F)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km3_in,nm_in,km_in,imin,imax,jmin,jmax
     real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:nm_in), intent(in):: W
     real(r_kind), dimension(1:km3_in,imin:imax,jmin:jmax,1:km_in), intent(out):: F
   end subroutine
   module subroutine l_vertical_direct_spec2 &
        (this,en,km_in,nm_in,imin,imax,jmin,jmax,f,w)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: en,km_in,nm_in,imin,imax,jmin,jmax
     real(r_kind), dimension(1:km_in*en,imin:imax,jmin:jmax), intent(in):: F
     real(r_kind), dimension(1:nm_in*en,imin:imax,jmin:jmax), intent(out):: W
   end subroutine
   module subroutine l_vertical_adjoint_spec2 &
        (this,en,nm_in,km_in,imin,imax,jmin,jmax,w,f)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: en,nm_in,km_in,imin,imax,jmin,jmax
     real(r_kind), dimension(1:nm_in*en,imin:imax,jmin:jmax), intent(in):: W
     real(r_kind), dimension(1:km_in*en,imin:imax,jmin:jmax), intent(out):: F
   end subroutine
   module subroutine lsqr_direct_offset &
        (this,V_in,W,km_in,ibm,jbm)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: km_in,ibm,jbm
     real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(in):: V_in
     real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(out):: W
     real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
   end subroutine
   module subroutine lsqr_adjoint_offset &
        (this,W,V_out,km_in,ibm,jbm)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind):: km_in,ibm,jbm
     real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(in):: W
     real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(out):: V_out
     real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
   end subroutine
   module subroutine quad_direct_offset &
        (this,V_in,W,km_in,ibm,jbm)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: km_in,ibm,jbm
     real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(in):: V_in
     real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(out):: W
     real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
   end subroutine
   module subroutine quad_adjoint_offset &
        (this,W,V_out,km_in,ibm,jbm)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind):: km_in,ibm,jbm
     real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(in):: W
     real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(out):: V_out
     real(r_kind), dimension(km_in,1:this%nm,1-jbm:this%jm+jbm):: VX
   end subroutine
   module subroutine lin_direct_offset &
        (this,V_in,W,km_in,ibm,jbm)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: km_in,ibm,jbm
     real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(in):: V_in
     real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(out):: W
   end subroutine
   module subroutine lin_adjoint_offset &
        (this,W,V_out,km_in,ibm,jbm)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind):: km_in,ibm,jbm
     real(r_kind), dimension(km_in,1:this%nm,1:this%mm),intent(in):: W
     real(r_kind), dimension(km_in,1-ibm:this%im+ibm,1-jbm:this%jm+jbm), intent(out):: V_out
   end subroutine
!from mg_bocos.f90 
   module subroutine boco_2d_g1 &
        (this,W,km_in,im_in,jm_in,nbx,nby)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby
     real(r_kind),dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
   end subroutine 
   module subroutine boco_2d_gh &
        (this,W,km_in,im_in,jm_in,nbx,nby,Fimax_in,Fjmax_in,mygen_min,mygen_max)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby,mygen_min,mygen_max
     real(r_kind),dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine boco_3d_g1 &
        (this,W,km3_in,im_in,jm_in,Lm_in,nbx,nby,nbz,Fimax_in,Fjmax_in)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km3_in,im_in,jm_in,Lm_in,nbx,nby,nbz
     real(r_kind),dimension(km3_in,1-nbx:im_in+nbx,1-nby:jm_in+nby,1-nbz:Lm_in+nbz),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine boco_3d_gh &
        (this,W,km3_in,im_in,jm_in,Lm_in,nbx,nby,nbz,Fimax_in,Fjmax_in,mygen_min,mygen_max)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km3_in,im_in,jm_in,Lm_in,nbx,nby,nbz,mygen_min,mygen_max
     real(r_kind),dimension(km3_in,1-nbx:im_in+nbx,1-nby:jm_in+nby,1-nbz:Lm_in+nbz),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine bocoT_2d_g1 &
        (this,W,km_in,im_in,jm_in,nbx,nby)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby
     real(r_kind), dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
   end subroutine
   module subroutine bocoT_2d_gh &
        (this,W,km_in,im_in,jm_in,nbx,nby,Fimax_in,Fjmax_in,mygen_min,mygen_max)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby,mygen_min,mygen_max
     real(r_kind), dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine bocoTx_2d_g1 &
        (this,W,km_in,im_in,jm_in,nbx,nby)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby
     real(r_kind), dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
   end subroutine
   module subroutine bocoTx_2d_gh &
        (this,W,km_in,im_in,jm_in,nbx,nby,Fimax_in,Fjmax_in,mygen_min,mygen_max)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby,mygen_min,mygen_max
     real(r_kind), dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine bocoTy_2d_g1 &
        (this,W,km_in,im_in,jm_in,nbx,nby)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby
     real(r_kind), dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
   end subroutine
   module subroutine bocoTy_2d_gh &
        (this,W,km_in,im_in,jm_in,nbx,nby,Fimax_in,Fjmax_in,mygen_min,mygen_max)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby,mygen_min,mygen_max
     real(r_kind), dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine bocoT_3d_g1 &
        (this,W,km3_in,im_in,jm_in,Lm_in,nbx,nby,nbz,Fimax_in,Fjmax_in)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km3_in,im_in,jm_in,Lm_in,nbx,nby,nbz
     real(r_kind), dimension(km3_in,1-nbx:im_in+nbx,1-nby:jm_in+nby,1-nbz:Lm_in+nbz),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine bocoT_3d_gh &
        (this,W,km_in,im_in,jm_in,Lm_in,nbx,nby,nbz,Fimax_in,Fjmax_in,mygen_min,mygen_max)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,Lm_in,nbx,nby,nbz,mygen_min,mygen_max
     real(r_kind), dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby,1-nbz:Lm_in+nbz),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine bocox_2d_gh &
        (this,W,km_in,im_in,jm_in,nbx,nby,Fimax_in,Fjmax_in,mygen_min,mygen_max)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby,mygen_min,mygen_max
     real(r_kind),dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine bocox_2d_g1 &
        (this,W,km_in,im_in,jm_in,nbx,nby)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby
     real(r_kind),dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
   end subroutine
   module subroutine bocoy_2d_g1 &
        (this,W,km_in,im_in,jm_in,nbx,nby)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby
     real(r_kind),dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
   end subroutine
   module subroutine bocoy_2d_gh &
        (this,W,km_in,im_in,jm_in,nbx,nby,Fimax_in,Fjmax_in,mygen_min,mygen_max)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby,mygen_min,mygen_max
     real(r_kind),dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine upsend_all_g1 &
        (this,Harray,Warray,km_in)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in
     real(r_kind), dimension(km_in,1:this%imL,1:this%jmL),intent(in):: Harray
     real(r_kind), dimension(km_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: Warray
   end subroutine
   module subroutine upsend_all_gh &
        (this,Harray,Warray,km_in,mygen_dn,mygen_up)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in
     real(r_kind), dimension(km_in,1:this%imL,1:this%jmL),intent(in):: Harray
     real(r_kind), dimension(km_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: Warray
     integer(i_kind),intent(in):: mygen_dn,mygen_up
   end subroutine
   module subroutine downsend_all_gh &
        (this,Warray,Harray,km_in,mygen_up,mygen_dn)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in
     real(r_kind), dimension(km_in,1:this%im,1:this%jm),intent(in):: Warray
     real(r_kind), dimension(km_in,1:this%imL,1:this%jmL),intent(out):: Harray
     integer, intent(in):: mygen_up,mygen_dn
   end subroutine
   module subroutine downsend_all_g2 &
        (this,Warray,Harray,km_in)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in
     real(r_kind), dimension(km_in,1:this%im,1:this%jm),intent(in):: Warray
     real(r_kind), dimension(km_in,1:this%imL,1:this%jmL),intent(out):: Harray
   end subroutine
   module subroutine boco_2d_loc &
        (this,W,km_in,im_in,jm_in,nbx,nby,Fimax_in,Fjmax_in,g)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby,g
     real(r_kind),dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine bocoT_2d_loc &
        (this,W,km_in,im_in,jm_in,nbx,nby,Fimax_in,Fjmax_in,g)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_in,im_in,jm_in,nbx,nby,g
     real(r_kind),dimension(km_in,1-nbx:im_in+nbx,1-nby:jm_in+nby),intent(inout):: W
     integer(i_kind), dimension(this%gm), intent(in):: Fimax_in,Fjmax_in
   end subroutine
   module subroutine upsend_loc_g12 &
        (this,V_in,H,km_4_in,flag)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_4_in,flag
     real(r_kind), dimension(km_4_in,1:this%imL,1:this%jmL),intent(in):: V_in
     real(r_kind), dimension(km_4_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
   end subroutine
   module subroutine upsend_loc_g23 &
        (this,V_in,H,km_16_in,flag)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_16_in,flag
     real(r_kind), dimension(km_16_in,1:this%imL,1:this%jmL),intent(in):: V_in
     real(r_kind), dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
   end subroutine
   module subroutine upsend_loc_g34 &
        (this,V_in,H,km_64_in,flag)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_64_in,flag
     real(r_kind), dimension(km_64_in,1:this%imL,1:this%jmL),intent(in):: V_in
     real(r_kind), dimension(km_64_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
   end subroutine
   module subroutine downsend_loc_g43 &
        (this,W,Z,km_64_in,flag)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_64_in,flag
     real(r_kind), dimension(km_64_in,1:this%im,1:this%jm),intent(in):: W
     real(r_kind), dimension(km_64_in,1:this%imL,1:this%jmL),intent(out):: Z
   end subroutine
   module subroutine downsend_loc_g32 &
        (this,Z,H,km_16_in,flag)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_16_in,flag
     real(r_kind), dimension(km_16_in,1:this%im,1:this%jm),intent(in):: Z
     real(r_kind), dimension(km_16_in,1:this%imL,1:this%jmL),intent(out):: H
   end subroutine
   module subroutine downsend_loc_g21 &
        (this,H,V_out,km_4_in,flag)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind), intent(in):: km_4_in,flag
     real(r_kind), dimension(km_4_in,1:this%im,1:this%jm),intent(in):: H
     real(r_kind), dimension(km_4_in,1:this%imL,1:this%jmL),intent(out):: V_out
   end subroutine
!from mg_generations.f90
   module subroutine upsending_all &
        (this,V,H,lquart)
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
     logical, intent(in):: lquart
   end subroutine
   module subroutine downsending_all &
        (this,H,V,lquart)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
     logical, intent(in):: lquart
   end subroutine
   module subroutine weighting_all &
        (this,V,H,lhelm)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     logical, intent(in):: lhelm
   end subroutine
   module subroutine upsending &
        (this,V,H)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
     real(r_kind),dimension(this%km,-1:this%imL+2,-1:this%jmL+2):: V_INT
     real(r_kind),dimension(this%km,-1:this%imL+2,-1:this%jmL+2):: H_INT
   end subroutine
   module subroutine downsending &
        (this,H,V)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
   end subroutine
   module subroutine upsending2 &
        (this,V,H)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
   end subroutine
   module subroutine downsending2 &
        (this,H,V)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
   end subroutine
   module subroutine upsending_highest &
        (this,V,H)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
   end subroutine
   module subroutine downsending_highest &
        (this,H,V)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
   end subroutine
   module subroutine upsending_ens &
        (this,V,H,kmx)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind), intent(in):: kmx
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
   end subroutine
   module subroutine downsending_ens &
        (this,H,V,kmx)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind), intent(in):: kmx
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
   end subroutine
   module subroutine upsending2_ens &
        (this,V,H,kmx)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind), intent(in):: kmx
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
   end subroutine
   module subroutine downsending2_ens &
        (this,H,V,kmx)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind), intent(in):: kmx
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
   end subroutine
   module subroutine upsending_ens_nearest &
        (this,V,H,kmx)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind), intent(in):: kmx
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
   end subroutine
   module subroutine downsending_ens_nearest &
        (this,H,V,kmx)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind), intent(in):: kmx
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
   end subroutine
   module subroutine upsending_loc_g3 &
        (this,V,H,Z,km_in,km_4_in,km_16_in)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: km_in,km_4_in,km_16_in
     real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
     real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: Z
   end subroutine
   module subroutine upsending_loc_g4 &
        (this,V,H,Z,W,km_in,km_4_in,km_16_in,km_64_in)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: km_in,km_4_in,km_16_in,km_64_in
     real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
     real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
     real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: Z
     real(r_kind),dimension(km_64_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: W
   end subroutine
   module subroutine downsending_loc_g3 &
        (this,Z,H,V,km_in,km_4_in,km_16_in)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: km_in,km_4_in,km_16_in
     real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: Z
     real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
   end subroutine
   module subroutine downsending_loc_g4 &
        (this,W,Z,H,V,km_in,km_4_in,km_16_in,km_64_in)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: km_in,km_4_in,km_16_in,km_64_in
     real(r_kind),dimension(km_64_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: W
     real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: Z
     real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
     real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
   end subroutine
   module subroutine weighting_helm &
        (this,V,H)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
   end subroutine
   module subroutine weighting &
        (this,V,H)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
   end subroutine
   module subroutine weighting_highest &
        (this,H)
     implicit none
     class (mg_intstate_type),target:: this
     real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
   end subroutine
   module subroutine weighting_ens &
        (this,V,H,kmx)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: kmx
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
     real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
   end subroutine
   module subroutine weighting_loc_g3 &
        (this,V,H04,H16,km_in,km_4_in,km_16_in)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: km_in,km_4_in,km_16_in
     real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
     real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H04
     real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H16
   end subroutine
   module subroutine weighting_loc_g4 &
        (this,V,H04,H16,H64,km_in,km_4_in,km_16_in,km_64_in)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: km_in,km_4_in,km_16_in,km_64_in
     real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
     real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H04
     real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H16
     real(r_kind),dimension(km_64_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H64
   end subroutine
   module subroutine adjoint &
        (this,F,W,km_in,g)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: g
     integer(i_kind),intent(in):: km_in
     real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(in):: F
     real(r_kind), dimension(km_in,-1:this%imL+2,-1:this%jmL+2), intent(out):: W
   end subroutine
   module subroutine direct1 &
        (this,W,F,km_in,g)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: g
     integer(i_kind),intent(in):: km_in
     real(r_kind), dimension(km_in,-1:this%imL+2,-1:this%jmL+2), intent(in):: W
     real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(out):: F
   end subroutine
   module subroutine adjoint2 &
        (this,F,W,km_in,g)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: g
     integer(i_kind),intent(in):: km_in
     real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(in):: F
     real(r_kind), dimension(km_in,0:this%imL+1,0:this%jmL+1), intent(out):: W
   end subroutine
   module subroutine direct2 &
        (this,W,F,km_in,g)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: g
     integer(i_kind),intent(in):: km_in
     real(r_kind), dimension(km_in,0:this%imL+1,0:this%jmL+1), intent(in):: W
     real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(out):: F
   end subroutine
   module subroutine adjoint_nearest &
        (this,F,W,km_in,g)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: g
     integer(i_kind),intent(in):: km_in
     real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(in):: F
     real(r_kind), dimension(km_in,-1:this%imL+2,-1:this%jmL+2), intent(out):: W
   end subroutine
   module subroutine direct_nearest &
        (this,W,F,km_in,g)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: g
     integer(i_kind),intent(in):: km_in
     real(r_kind), dimension(km_in,-1:this%imL+2,-1:this%jmL+2), intent(in):: W
     real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(out):: F
   end subroutine
   module subroutine adjoint_highest &
        (this,F,W,km_in,g)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: g
     integer(i_kind),intent(in):: km_in
     real(r_kind), dimension(km_in,1:this%im0(g),1:this%jm0(g)), intent(in):: F
     real(r_kind), dimension(km_in,-1:this%im0(g+1)+2,-1:this%jm0(g+1)+2), intent(out):: W
   end subroutine
   module subroutine direct_highest &
        (this,W,F,km_in,g)
     implicit none
     class (mg_intstate_type),target:: this
     integer(i_kind),intent(in):: g
     integer(i_kind),intent(in):: km_in
     real(r_kind), dimension(km_in,-1:this%im0(g+1)+2,-1:this%jm0(g+1)+2), intent(in):: W
     real(r_kind), dimension(km_in,1:this%im0(g),1:this%jm0(g)), intent(out):: F
   end subroutine
!from mg_filtering
   module subroutine filtering_procedure(this,mg_filt,mg_filt_flag)
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: mg_filt
     integer(i_kind),intent(in):: mg_filt_flag
   end subroutine
   module subroutine filtering_rad3(this)
     class(mg_intstate_type),target::this
   end subroutine
   module subroutine filtering_lin3(this)
     class(mg_intstate_type),target::this
   end subroutine
   module subroutine filtering_rad2_bkg(this)
     class(mg_intstate_type),target::this
   end subroutine
   module subroutine filtering_lin2_bkg(this)
     class(mg_intstate_type),target::this
   end subroutine
   module subroutine filtering_fast_bkg(this)
     class(mg_intstate_type),target::this
   end subroutine
   module subroutine filtering_rad2_ens(this,mg_filt_flag)
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: mg_filt_flag
   end subroutine
   module subroutine filtering_lin2_ens(this,mg_filt_flag)
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: mg_filt_flag
   end subroutine
   module subroutine filtering_fast_ens(this,mg_filt_flag)
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: mg_filt_flag
   end subroutine
   module subroutine filtering_rad_highest(this)
     class(mg_intstate_type),target::this
   end subroutine
   module subroutine sup_vrbeta1 &
        (this,kmax,hx,hy,hz,im,jm,lm, pasp,ss, V)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: kmax,hx,hy,hz,im,jm,lm
     real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1:lm),intent(inout):: V
     real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
     real(r_kind),dimension(1:lm), intent(in):: ss
   end subroutine
   module subroutine sup_vrbeta1T &
     (this,kmax,hx,hy,hz,im,jm,lm,  pasp,ss, V) 
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: kmax,hx,hy,hz,im,jm,lm
     real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1:lm),intent(inout):: V
     real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
     real(r_kind),dimension(1:lm), intent(in):: ss
   end subroutine
   module subroutine sup_vrbeta3 &
        (this,kmax,hx,hy,hz,im,jm,lm, pasp,ss, V)
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: kmax,hx,hy,hz,im,jm,lm
     real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1:lm),intent(inout):: V
     real(r_kind),dimension(3,3,1:im,1:jm,1:lm), intent(in):: pasp
     real(r_kind),dimension(1:im,1:jm,1:lm), intent(in):: ss
   end subroutine
   module subroutine sup_vrbeta3T &
        (this,kmax,hx,hy,hz,im,jm,lm, pasp,ss,V)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: kmax,hx,hy,hz,im,jm,lm
     real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1:lm),intent(inout):: V
     real(r_kind),dimension(3,3,1:im,1:jm,1:lm), intent(in):: pasp
     real(r_kind),dimension(1:im,1:jm,1:lm), intent(in):: ss
   end subroutine
   module subroutine sup_vrbeta1_ens &
        (this,km_en,hx,hy,hz,im,jm,lm,pasp,ss,VALL)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: km_en,hx,hy,hz,im,jm,lm
     real(r_kind),dimension(1:km_en*lm,1-hx:im+hx,1-hy:jm+hy),intent(inout):: VALL
     real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
     real(r_kind),dimension(1:lm), intent(in):: ss
   end subroutine
   module subroutine sup_vrbeta1T_ens &
        (this,km_en,hx,hy,hz,im,jm,lm,pasp,ss,VALL)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: km_en,hx,hy,hz,im,jm,lm
     real(r_kind),dimension(1:km_en*lm,1-hx:im+hx,1-hy:jm+hy),intent(inout):: VALL
     real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
     real(r_kind),dimension(1:lm), intent(in):: ss
   end subroutine
   module subroutine sup_vrbeta1_bkg &
        (this,km,km3,hx,hy,hz,im,jm,lm,pasp,ss,VALL)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: km,km3,hx,hy,hz,im,jm,lm
     real(r_kind),dimension(1:km,1-hx:im+hx,1-hy:jm+hy),intent(inout):: VALL
     real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
     real(r_kind),dimension(1:lm), intent(in):: ss
   end subroutine
   module subroutine sup_vrbeta1T_bkg &
        (this,km,km3,hx,hy,hz,im,jm,lm,pasp,ss,VALL)
     implicit none
     class(mg_intstate_type),target::this
     integer(i_kind),intent(in):: km,km3,hx,hy,hz,im,jm,lm
     real(r_kind),dimension(1:km,1-hx:im+hx,1-hy:jm+hy),intent(inout):: VALL
     real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
     real(r_kind),dimension(1:lm), intent(in):: ss
   end subroutine
!from mg_transfer.f90
   module subroutine anal_to_filt_allmap(this,WORKA)
     class(mg_intstate_type),target::this
     real (r_kind):: WORKA(this%km_a_all,1:this%nm,1:this%mm)
   end subroutine
   module subroutine filt_to_anal_allmap(this,WORKA)
     class(mg_intstate_type),target::this
     real (r_kind):: WORKA(this%km_a_all,1:this%nm,1:this%mm)
   end subroutine
   module subroutine anal_to_filt_all(this,WORKA)
     class(mg_intstate_type),target::this
     real (r_kind):: WORKA(this%km_a_all,1:this%nm,1:this%mm)
   end subroutine
   module subroutine filt_to_anal_all(this,WORKA)
     class(mg_intstate_type),target::this
     real (r_kind):: WORKA(this%km_a_all,1:this%nm,1:this%mm)
   end subroutine
   module subroutine anal_to_filt_all2(this,WORKA)
     class(mg_intstate_type),target::this
     real (r_kind):: WORKA(this%km_a_all,1:this%nm,1:this%mm)
   end subroutine
   module subroutine filt_to_anal_all2(this,WORKA)
     class(mg_intstate_type),target::this
     real (r_kind):: WORKA(this%km_a_all,1:this%nm,1:this%mm)
   end subroutine
   module subroutine stack_to_composite(this,ARR_ALL,A2D,A3D)
     class(mg_intstate_type),target::this
     real(r_kind),dimension(this%km ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),   intent(in):: ARR_ALL
     real(r_kind),dimension(this%km3,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy,this%lm),intent(out):: A3D
     real(r_kind),dimension(this%km2,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)   ,intent(out):: A2D
   end subroutine
   module subroutine composite_to_stack(this,A2D,A3D,ARR_ALL)
     class(mg_intstate_type),target::this
     real(r_kind),dimension(this%km2,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),   intent(in):: A2D
     real(r_kind),dimension(this%km3,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy,this%lm),intent(in):: A3D
     real(r_kind),dimension(this%km ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),   intent(out):: ARR_ALL
   end subroutine
   module subroutine S2C_ens(this,ARR_ALL,A3D,imn,imx,jmn,jmx,lmx,kmx,kmx_all)
     class(mg_intstate_type),target::this
     integer, intent(in):: imn,imx,jmn,jmx,lmx,kmx,kmx_all
     real(r_kind),dimension(kmx_all,imn:imx,jmn:jmx)    ,intent(in):: ARR_ALL
     real(r_kind),dimension(this%km3_all,imn:imx,jmn:jmx,lmx),intent(out):: A3D
   end subroutine
   module subroutine C2S_ens(this,A3D,ARR_ALL,imn,imx,jmn,jmx,lmx,kmx,kmx_all)
     class(mg_intstate_type),target::this
     integer, intent(in):: imn,imx,jmn,jmx,lmx,kmx,kmx_all
     real(r_kind),dimension(this%km3_all,imn:imx,jmn:jmx,lmx),intent(in):: A3D
     real(r_kind),dimension(kmx_all,imn:imx,jmn:jmx)    ,intent(out):: ARR_ALL
   end subroutine
   module subroutine anal_to_filt(this,WORK)
     class(mg_intstate_type),target::this
     real (r_kind):: WORK(this%km_all,1:this%nm,1:this%mm)
   end subroutine
   module subroutine filt_to_anal(this,WORK)
     class(mg_intstate_type),target::this
     real (r_kind):: WORK(this%km_all,1:this%nm,1:this%mm)
   end subroutine
!from mg_entrymod.f90
   module subroutine mg_initialize(this,inputfilename,obj_parameter)
     class (mg_intstate_type):: this
     character*(*),optional,intent(in) :: inputfilename
     class(mg_parameter_type),optional,intent(in)::obj_parameter
   end subroutine
   module subroutine mg_finalize(this)
     implicit none
     class (mg_intstate_type)::this
   end subroutine
end interface

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
contains
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine allocate_mg_intstate(this)
!***********************************************************************
!                                                                      !
! Allocate internal state variables                                    !
!                                                                      !
!***********************************************************************
implicit none
class(mg_intstate_type),target::this

if(this%l_loc) then
   allocate(this%w1_loc(this%km_all   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%w1_loc=0.
   allocate(this%w2_loc(this%km_all/4 ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%w2_loc=0.
   allocate(this%w3_loc(this%km_all/16,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%w3_loc=0.
   allocate(this%w4_loc(this%km_all/64,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%w4_loc=0.
endif

allocate(this%V(1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy,this%lm))        ; this%V=0.
allocate(this%VALL(this%km_all,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%VALL=0.
allocate(this%HALL(this%km_all,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%HALL=0.

allocate(this%a_diff_f(this%km_all,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%a_diff_f=0. 
allocate(this%a_diff_h(this%km_all,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%a_diff_h=0. 
allocate(this%b_diff_f(this%km_all,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%b_diff_f=0. 
allocate(this%b_diff_h(this%km_all,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%b_diff_h=0. 

allocate(this%p_eps(1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%p_eps=0.
allocate(this%p_del(1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%p_del=0.
allocate(this%p_sig(1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%p_sig=0.
allocate(this%p_rho(1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy)) ; this%p_rho=0.

allocate(this%paspx(1,1,1:this%im)) ; this%paspx=0.
allocate(this%paspy(1,1,1:this%jm)) ; this%paspy=0.

allocate(this%pasp1(1,1,1:this%lm))                     ; this%pasp1=0.
allocate(this%pasp2(2,2,1:this%im,1:this%jm))           ; this%pasp2=0.
allocate(this%pasp3(3,3,1:this%im,1:this%jm,1:this%lm)) ; this%pasp3=0.

allocate(this%vpasp2(0:2,1:this%im,1:this%jm)) ; this%vpasp2=0.
allocate(this%hss2(1:this%im,1:this%jm,1:3))   ; this%hss2=0.

allocate(this%vpasp3(1:6,1:this%im,1:this%jm,1:this%lm)) ; this%vpasp3=0.
allocate(this%hss3(1:this%im,1:this%jm,1:this%lm,1:6))   ; this%hss3=0.

allocate(this%ssx(1:this%im))                     ; this%ssx=0.
allocate(this%ssy(1:this%jm))                     ; this%ssy=0.
allocate(this%ss1(1:this%lm))                     ; this%ss1=0.
allocate(this%ss2(1:this%im,1:this%jm))           ; this%ss2=0.
allocate(this%ss3(1:this%im,1:this%jm,1:this%lm)) ; this%ss3=0.

allocate(this%dixs(1:this%im,1:this%jm,3)) ; this%dixs=0
allocate(this%diys(1:this%im,1:this%jm,3)) ; this%diys=0

allocate(this%dixs3(1:this%im,1:this%jm,1:this%lm,6)) ; this%dixs3=0
allocate(this%diys3(1:this%im,1:this%jm,1:this%lm,6)) ; this%diys3=0
allocate(this%dizs3(1:this%im,1:this%jm,1:this%lm,6)) ; this%dizs3=0

allocate(this%qcols(0:7,1:this%im,1:this%jm,1:this%lm)) ; this%qcols=0

!
! for re-decomposition
!

allocate(this%iref(1:this%nm)) ; this%iref=0
allocate(this%jref(1:this%mm)) ; this%jref=0

allocate(this%irefq(1:this%nm)) ; this%irefq=0
allocate(this%jrefq(1:this%mm)) ; this%jrefq=0

allocate(this%irefL(1:this%nm)) ; this%irefL=0
allocate(this%jrefL(1:this%mm)) ; this%jrefL=0

allocate(this%cx0(1:this%nm)) ; this%cx0=0.
allocate(this%cx1(1:this%nm)) ; this%cx1=0.
allocate(this%cx2(1:this%nm)) ; this%cx2=0.
allocate(this%cx3(1:this%nm)) ; this%cx3=0.

allocate(this%cy0(1:this%mm)) ; this%cy0=0.
allocate(this%cy1(1:this%mm)) ; this%cy1=0.
allocate(this%cy2(1:this%mm)) ; this%cy2=0.
allocate(this%cy3(1:this%mm)) ; this%cy3=0.

allocate(this%qx0(1:this%nm)) ; this%qx0=0.
allocate(this%qx1(1:this%nm)) ; this%qx1=0.
allocate(this%qx2(1:this%nm)) ; this%qx2=0.

allocate(this%qy0(1:this%mm)) ; this%qy0=0.
allocate(this%qy1(1:this%mm)) ; this%qy1=0.
allocate(this%qy2(1:this%mm)) ; this%qy2=0.

allocate(this%Lx0(1:this%nm)) ; this%Lx0=0.
allocate(this%Lx1(1:this%nm)) ; this%Lx1=0.

allocate(this%Ly0(1:this%mm)) ; this%Ly0=0.
allocate(this%Ly1(1:this%mm)) ; this%Ly1=0.

allocate(this%p_coef(4)) ; this%p_coef=0.
allocate(this%q_coef(4)) ; this%q_coef=0.

allocate(this%a_coef(3)) ; this%a_coef=0.
allocate(this%b_coef(3)) ; this%b_coef=0.

allocate(this%cf00(1:this%nm,1:this%mm)) ; this%cf00=0.
allocate(this%cf01(1:this%nm,1:this%mm)) ; this%cf01=0.
allocate(this%cf02(1:this%nm,1:this%mm)) ; this%cf02=0.
allocate(this%cf03(1:this%nm,1:this%mm)) ; this%cf03=0.
allocate(this%cf10(1:this%nm,1:this%mm)) ; this%cf10=0.
allocate(this%cf11(1:this%nm,1:this%mm)) ; this%cf11=0.
allocate(this%cf12(1:this%nm,1:this%mm)) ; this%cf12=0.
allocate(this%cf13(1:this%nm,1:this%mm)) ; this%cf13=0.
allocate(this%cf20(1:this%nm,1:this%mm)) ; this%cf20=0.
allocate(this%cf21(1:this%nm,1:this%mm)) ; this%cf21=0.
allocate(this%cf22(1:this%nm,1:this%mm)) ; this%cf22=0.
allocate(this%cf23(1:this%nm,1:this%mm)) ; this%cf23=0.
allocate(this%cf30(1:this%nm,1:this%mm)) ; this%cf30=0.
allocate(this%cf31(1:this%nm,1:this%mm)) ; this%cf31=0.
allocate(this%cf32(1:this%nm,1:this%mm)) ; this%cf32=0.
allocate(this%cf33(1:this%nm,1:this%mm)) ; this%cf33=0.

allocate(this%Lref(1:this%lm_a)) ; this%Lref=0
allocate(this%Lref_h(1:this%lm)) ; this%Lref_h=0

allocate(this%cvf1(1:this%lm_a)) ; this%cvf1=0.
allocate(this%cvf2(1:this%lm_a)) ; this%cvf2=0.
allocate(this%cvf3(1:this%lm_a)) ; this%cvf3=0.
allocate(this%cvf4(1:this%lm_a)) ; this%cvf4=0.

allocate(this%cvh1(1:this%lm)) ; this%cvh1=0.
allocate(this%cvh2(1:this%lm)) ; this%cvh2=0.
allocate(this%cvh3(1:this%lm)) ; this%cvh3=0.
allocate(this%cvh4(1:this%lm)) ; this%cvh4=0.

!-----------------------------------------------------------------------
endsubroutine allocate_mg_intstate

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine def_mg_weights(this)
!***********************************************************************
!                                                                      !
! Define weights and scales                                            !
!                                                                      !
implicit none
class (mg_intstate_type),target::this
!***********************************************************************
integer(i_kind):: i,j,L
real(r_kind):: gen_fac
!-----------------------------------------------------------------------

this%p_eps(:,:)=0.0
this%p_del(:,:)=0.0
this%p_sig(:,:)=0.0
this%p_rho(:,:)=0.0

!--------------------------------------------------------
!
! For localization (for now)
!
if(this%l_loc) then
   this%w1_loc(:,:,:)=this%mg_weig1
   this%w2_loc(:,:,:)=this%mg_weig2
   this%w3_loc(:,:,:)=this%mg_weig3
   this%w4_loc(:,:,:)=this%mg_weig4
endif
!--------------------------------------------------------
gen_fac=1.
this%a_diff_f(:,:,:)=this%mg_weig1 
this%a_diff_h(:,:,:)=this%mg_weig1 

this%b_diff_f(:,:,:)=0.
this%b_diff_h(:,:,:)=0.

select case(this%my_hgen)
case(2) 
   this%a_diff_h(:,:,:)=this%mg_weig2
case(3) 
   this%a_diff_h(:,:,:)=this%mg_weig3 
case default 
   this%a_diff_h(:,:,:)=this%mg_weig4
end select

do L=1,this%lm
   this%pasp1(1,1,L)=this%pasp01
enddo

do i=1,this%im
   this%paspx(1,1,i)=this%pasp02
enddo
do j=1,this%jm
   this%paspy(1,1,j)=this%pasp02
enddo

do j=1,this%jm
do i=1,this%im
   this%pasp2(1,1,i,j)=this%pasp02*(1.+this%p_del(i,j))
   this%pasp2(2,2,i,j)=this%pasp02*(1.-this%p_del(i,j))
   this%pasp2(1,2,i,j)=this%pasp02*this%p_eps(i,j)     
   this%pasp2(2,1,i,j)=this%pasp02*this%p_eps(i,j)     
end do
end do

do L=1,this%lm
   do j=1,this%jm
   do i=1,this%im
      this%pasp3(1,1,i,j,l)=this%pasp03*(1+this%p_del(i,j))
      this%pasp3(2,2,i,j,l)=this%pasp03
      this%pasp3(3,3,i,j,l)=this%pasp03*(1-this%p_del(i,j))
      this%pasp3(1,2,i,j,l)=this%pasp03*this%p_eps(i,j)
      this%pasp3(2,1,i,j,l)=this%pasp03*this%p_eps(i,j)
      this%pasp3(2,3,i,j,l)=this%pasp03*this%p_sig(i,j)
      this%pasp3(3,2,i,j,l)=this%pasp03*this%p_sig(i,j)
      this%pasp3(1,3,i,j,l)=this%pasp03*this%p_rho(i,j)
      this%pasp3(3,1,i,j,l)=this%pasp03*this%p_rho(i,j)
   end do
   end do
end do


if(.not.this%mgbf_line) then
   if(this%nxm*this%nym>1) then
      if(this%l_loc) then
         if(this%l_vertical_filter) then
            call this%cholaspect(1,this%lm,this%pasp1)
            call this%getlinesum(this%hz,1,this%lm,this%pasp1,this%ss1)
            do L=1,this%lm
               this%VALL(L,2,1)=1.
               call this%sup_vrbeta1T_ens(1,0,0,this%hz,1,1,this%lm,this%pasp1,this%ss1,this%VALL(1:this%lm,2,1))
               call this%sup_vrbeta1_ens(1,0,0,this%hz,1,1,this%lm,this%pasp1,this%ss1,this%VALL(1:this%lm,2,1))
               this%VALL(L,1,1)=sqrt(this%VALL(L,2,1))
               this%VALL(1:this%lm,2,1)=0.
            enddo
            this%ss1(1:this%lm)=this%ss1(1:this%lm)/this%VALL(1:this%lm,1,1)
            this%VALL(1:this%lm,1,1)=0.
         endif
         call this%cholaspect(1,this%im,1,this%jm,this%pasp2)
         call this%getlinesum(this%hx,1,this%im,this%hy,1,this%jm,this%pasp2,this%ss2)
         this%VALL(1,this%im/2,this%jm/2)=1.
         call this%rbetaT(this%hx,1,this%im,this%hy,1,this%jm,this%pasp2,this%ss2,this%VALL(1,:,:))
         call this%rbeta(this%hx,1,this%im,this%hy,1,this%jm,this%pasp2,this%ss2,this%VALL(1,:,:))
         this%ss2=this%ss2/sqrt(this%VALL(1,this%im/2,this%jm/2))
         this%VALL(1,:,:)=0.
         call this%cholaspect(1,this%im,this%paspx)
         call this%getlinesum(this%hx,1,this%im,this%paspx,this%ssx)
         this%VALL(1,this%im/2,1)=1.
         call this%rbetaT(this%hx,1,this%im,this%paspx,this%ssx,this%VALL(1,:,1))
         call this%rbeta(this%hx,1,this%im,this%paspx(1,1,:),this%ssx,this%VALL(1,:,1))
         this%ssx=this%ssx/sqrt(this%VALL(1,this%im/2,1))
         this%VALL(1,:,1)=0.
         call this%cholaspect(1,this%jm,this%paspy)
         call this%getlinesum(this%hy,1,this%jm,this%paspy,this%ssy)
         this%VALL(1,1,this%jm/2)=1.
         call this%rbetaT(this%hy,1,this%jm,this%paspy,this%ssy,this%VALL(1,1,:))
         call this%rbeta(this%hy,1,this%jm,this%paspy(1,1,:),this%ssy,this%VALL(1,1,:))
         this%ssy=this%ssy/sqrt(this%VALL(1,1,this%jm/2))
         this%VALL(1,1,:)=0.
      else
         call this%cholaspect(1,this%lm,this%pasp1)
         call this%cholaspect(1,this%im,1,this%jm,this%pasp2)
         call this%cholaspect(1,this%im,1,this%jm,1,this%lm,this%pasp3)
         call this%getlinesum(this%hx,1,this%im,this%paspx,this%ssx)
         call this%getlinesum(this%hy,1,this%jm,this%paspy,this%ssy)
         call this%getlinesum(this%hz,1,this%lm,this%pasp1,this%ss1)
         call this%getlinesum(this%hx,1,this%im,this%hy,1,this%jm,this%pasp2,this%ss2)
         call this%getlinesum(this%hx,1,this%im,this%hy,1,this%jm,this%hz,1,this%lm,this%pasp3,this%ss3)
      end if
   else
      call this%cholaspect(1,this%imH,1,this%jmH,&
           &this%pasp2(:,:,1:this%imH,1:this%jmH))
      call this%getlinesum(this%hx,1,this%imH,this%hy,1,this%jmH,&
           &this%pasp2(:,:,1:this%imH,1:this%jmH),this%ss2(1:this%imH,1:this%jmH))
      this%VALL(1,this%imH/2,this%jmH/2)=1.
      call this%rbetaT(this%hx,1,this%imH,this%hy,1,this%jmH,&
           &this%pasp2(:,:,1:this%imH,1:this%jmH),this%ss2(1:this%imH,1:this%jmH),&
           &this%VALL(1,1-this%hx:this%imH+this%hx,1-this%hy:this%jmH+this%hy))
      call this%rbeta(this%hx,1,this%imH,this%hy,1,this%jmH,&
           &this%pasp2(:,:,1:this%imH,1:this%jmH),this%ss2(1:this%imH,1:this%jmH),&
           &this%VALL(1,1-this%hx:this%imH+this%hx,1-this%hy:this%jmH+this%hy))
      this%ss2=this%ss2/sqrt(this%VALL(1,this%imH/2,this%jmH/2))
      this%VALL(1,1-this%hx:this%imH+this%hx,1-this%hy:this%jmH+this%hy)=0.
   end if
end if
!-----------------------------------------------------------------------
endsubroutine def_mg_weights

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine init_mg_line(this)
implicit none
class(mg_intstate_type),target::this
integer(i_kind):: i,j,L,icol
logical:: ff
!***********************************************************************
!                                                                      !
! Inititate line filters                                               !
!                                                                      !
!***********************************************************************
!-----------------------------------------------------------------------

do j=1,this%jm
do i=1,this%im
   call t22_to_3(this%pasp2(:,:,i,j),this%vpasp2(:,i,j))
enddo
enddo

do l=1,this%lm
do j=1,this%jm
do i=1,this%im
   call t33_to_6(this%pasp3(:,:,i,j,l),this%vpasp3(:,i,j,l))
enddo
enddo
enddo

call inimomtab(this%p,this%nh,ff)

call tritform(1,this%im,1,this%jm,this%vpasp2, this%dixs,this%diys, ff)

do icol=1,3
   this%hss2(:,:,icol)=this%vpasp2(icol-1,:,:)
enddo

call hextform(1,this%im,1,this%jm,1,this%lm,this%vpasp3,this%qcols,this%dixs3,this%diys3,this%dizs3, ff)

do icol=1,6
   this%hss3(:,:,:,icol)=this%vpasp3(icol,:,:,:)
enddo
 
!-----------------------------------------------------------------------
endsubroutine init_mg_line

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
subroutine deallocate_mg_intstate(this)
implicit none
class (mg_intstate_type),target:: this
!***********************************************************************
!                                                                      !
! Deallocate internal state variables                                  !
!                                                                      !
!***********************************************************************

deallocate(this%V)

deallocate(this%HALL,this%VALL)

deallocate(this%a_diff_f,this%b_diff_f)
deallocate(this%a_diff_h,this%b_diff_h)
deallocate(this%p_eps,this%p_del,this%p_sig,this%p_rho,this%pasp1,this%pasp2,this%pasp3,this%ss1,this%ss2,this%ss3)
deallocate(this%dixs,this%diys)
deallocate(this%dixs3,this%diys3,this%dizs3)
deallocate(this%qcols)

!
! for re-decomposition
!
deallocate(this%iref,this%jref)
deallocate(this%irefq,this%jrefq)
deallocate(this%irefL,this%jrefL)

deallocate(this%cf00,this%cf01,this%cf02,this%cf03,this%cf10,this%cf11,this%cf12,this%cf13)
deallocate(this%cf20,this%cf21,this%cf22,this%cf23,this%cf30,this%cf31,this%cf32,this%cf33)

deallocate(this%Lref,this%Lref_h)

deallocate(this%cvf1,this%cvf2,this%cvf3,this%cvf4)

deallocate(this%cvh1,this%cvh2,this%cvh3,this%cvh4)

deallocate(this%cx0,this%cx1,this%cx2,this%cx3)
deallocate(this%cy0,this%cy1,this%cy2,this%cy3)

deallocate(this%qx0,this%qx1,this%qx2)
deallocate(this%qy0,this%qy1,this%qy2)

deallocate(this%Lx0,this%Lx1)
deallocate(this%Ly0,this%Ly1)

deallocate(this%p_coef,this%q_coef)
deallocate(this%a_coef,this%b_coef)

if(this%l_loc) then
  deallocate(this%w1_loc,this%w2_loc,this%w3_loc,this%w4_loc)
endif

end subroutine deallocate_mg_intstate

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end module mg_intstate
