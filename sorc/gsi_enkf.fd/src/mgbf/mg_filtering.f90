submodule(mg_intstate) mg_filtering
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_filtering
!   prgmmr: rancic           org: NCEP/EMC            date: 2020
!
! abstract:  Contains all multigrid filtering prodecures
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   filtering_procedure -
!   filtering_rad3 -
!   filtering_lin3 -
!   filtering_rad2_bkg -
!   filtering_lin2_bkg -
!   filtering_fast_bkg -
!   filtering_rad2_ens -
!   filtering_lin2_ens -
!   filtering_fast_ens -
!   filtering_rad_highest -
!   sup_vrbeta1 -
!   sup_vrbeta1T -
!   sup_vrbeta3 -
!   sup_vrbeta3T -
!   sup_vrbeta1_ens -
!   sup_vrbeta1T_ens -
!   sup_vrbeta1_bkg -
!   sup_vrbeta1T_bkg -
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

use mg_timers
use kinds, only: r_kind,i_kind
use jp_pbfil3, only: dibetat,dibeta
use mpi

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
contains
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_procedure(this,mg_filt,mg_filt_flag) 
!***********************************************************************
!                                                                      !
! Driver for Multigrid filtering procedures with Helmholtz operator    !
!                                                                      !
!***********************************************************************
implicit none 
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: mg_filt
integer(i_kind),intent(in):: mg_filt_flag
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!-----------------------------------------------------------------------
if(this%nxm*this%nym>1) then
   select case(mg_filt)
   case(1)
      call this%filtering_rad3
   case(2)
      call this%filtering_lin3
   case(3)
      call this%filtering_rad2_bkg
   case(4)
      call this%filtering_lin2_bkg
   case(5)
      call this%filtering_fast_bkg
   case(6)
      call this%filtering_rad2_ens(mg_filt_flag)
   case(7)
      call this%filtering_lin2_ens(mg_filt_flag)
   case(8)
      call this%filtering_fast_ens(mg_filt_flag)
   end select
else
  call this%filtering_rad_highest
endif
!-----------------------------------------------------------------------
endsubroutine filtering_procedure    

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_rad3(this)
!***********************************************************************
!                                                                      !
! Multigrid filtering procedure:                                       !
!                                                                      !
!     - Multiple of 2D and 3D variables                                !
!     - 1 upsending and downsending                                    !
!     - Applicaton of Helmholtz differential operator                  !
!     - 3d radial filter                                               !
!                                                                      !
!***********************************************************************
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target::this
real(r_kind), allocatable, dimension(:,:,:):: VM2D
real(r_kind), allocatable, dimension(:,:,:):: HM2D
real(r_kind), allocatable, dimension(:,:,:,:):: VM3D
real(r_kind), allocatable, dimension(:,:,:,:):: HM3D
integer(i_kind) L,i,j
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!----------------------------------------------------------------------
allocate(VM3D(km3,1-hx:im+hx,1-hy:jm+hy,lm))                 ; VM3D=0.
allocate(VM2D(km2,1-hx:im+hx,1-hy:jm+hy   ))                 ; VM2D=0.
allocate(HM3D(km3,1-hx:im+hx,1-hy:jm+hy,lm))                 ; HM3D=0.
allocate(HM2D(km2,1-hx:im+hx,1-hy:jm+hy   ))                 ; HM2D=0.

!***
!*** Adjoint interpolate and upsend 
!***
                                                 call btim(upsend_tim)
     call this%upsending_all(VALL,HALL,lquart)
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations 
!***
                                                 call btim(hfiltT_tim)
     call this%stack_to_composite(VALL,VM2D,VM3D)
     call this%rbetaT(km2,hx,1,im,hy,1,jm,pasp2,ss2,VM2D)
     call this%sup_vrbeta3T(km3,hx,hy,hz,im,jm,lm,pasp3,ss3,VM3D)
     call this%composite_to_stack(VM2D,VM3D,VALL)

  if(l_hgen) then
     call this%stack_to_composite(HALL,HM2D,HM3D)
     call this%rbetaT(km2,hx,1,im,hy,1,jm,pasp2,ss2,HM2D)
     call this%sup_vrbeta3T(km3,hx,hy,hz,im,jm,lm,pasp3,ss3,HM3D)
     call this%composite_to_stack(HM2D,HM3D,HALL)
  endif
                                                 call etim(hfiltT_tim)

                                                 call btim(bocoT_tim)
     call this%bocoT_2d(VALL,km,im,jm,hx,hy)
     call this%bocoT_2d(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_all(VALL,HALL,lhelm)
                                                 call etim(weight_tim)
!***
!*** Apply Beta filter at all generations 
!***
                                                 call btim(boco_tim)
     call this%boco_2d(VALL,km,im,jm,hx,hy)
     call this%boco_2d(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)

                                                 call btim(hfilt_tim)
     call this%stack_to_composite(VALL,VM2D,VM3D)
     call this%rbeta(km2,hx,1,im,hy,1,jm,pasp2,ss2,VM2D(:,:,:))
     call this%sup_vrbeta3(km3,hx,hy,hz,im,jm,lm,pasp3,ss3,VM3D)
     call this%composite_to_stack(VM2D,VM3D,VALL)
  if(l_hgen)  then
     call this%stack_to_composite(HALL,HM2D,HM3D)
     call this%rbeta(km2,hx,1,im,hy,1,jm,pasp2,ss2,HM2D(:,:,:))
     call this%sup_vrbeta3(km3,hx,hy,hz,im,jm,lm,pasp3,ss3,HM3D)
     call this%composite_to_stack(HM2D,HM3D,HALL)
  endif
                                                 call etim(hfilt_tim)
!***
!***  Downsend, interpolate and add 
!***  Then zero high generations 
!***
                                                 call btim(dnsend_tim)
     call this%downsending_all(HALL,VALL,lquart)
                                                 call etim(dnsend_tim)

deallocate(VM3D)
deallocate(VM2D)
deallocate(HM3D)
deallocate(HM2D)
!-----------------------------------------------------------------------
endsubroutine filtering_rad3   

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_lin3(this)
!***********************************************************************
!                                                                      !
! Multigrid filtering procedure:                                       !
!                                                                      !
!     - Multiple of 2D line filter                                     !
!     - 1 upsending and downsending                                    !
!     - Applicaton of Helmholtz differential operator                  !
!     - 3d line filter                                                 !
!                                                                      !
!***********************************************************************
!TEST
use, intrinsic :: ieee_arithmetic
!TEST
use jp_pkind2, only: fpi
implicit none
class (mg_intstate_type),target::this
integer(i_kind) k,i,j,L
integer(i_kind) icol,iout,jout,lout
logical:: ff
real(r_kind), allocatable, dimension(:,:,:):: VM2D
real(r_kind), allocatable, dimension(:,:,:):: HM2D
real(r_kind), allocatable, dimension(:,:,:,:):: VM3D
real(r_kind), allocatable, dimension(:,:,:,:):: HM3D
real(r_kind), allocatable, dimension(:,:,:,:):: W
real(r_kind), allocatable, dimension(:,:,:,:):: H
integer(fpi), allocatable, dimension(:,:,:):: JCOL
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!-----------------------------------------------------------------------
allocate(VM3D(km3,1-hx:im+hx,1-hy:jm+hy,lm))                 ; VM3D=0.
allocate(VM2D(km2,1-hx:im+hx,1-hy:jm+hy   ))                 ; VM2D=0.
allocate(HM3D(km3,1-hx:im+hx,1-hy:jm+hy,lm))                 ; HM3D=0.
allocate(HM2D(km2,1-hx:im+hx,1-hy:jm+hy   ))                 ; HM2D=0.
allocate(W(km3,1-hx:im+hx,1-hy:jm+hy,1-hz:lm+hz))            ; W=0.
allocate(H(km3,1-hx:im+hx,1-hy:jm+hy,1-hz:lm+hz))            ; H=0.
allocate(JCOL(1:im,1:jm,1:Lm))                               ; JCOL=0

!***
!*** Adjoint interpolate and upsend
!***
                                                 call btim(upsend_tim)
     call this%upsending_all(VALL,HALL,lquart)
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations 
!***

!
! From single stack to composite variables
!
                                                 call btim(hfiltT_tim)
     call this%stack_to_composite(VALL,VM2D,VM3D)
  if(l_hgen)  then
     call this%stack_to_composite(HALL,HM2D,HM3D)
  endif
                                                 call etim(hfiltT_tim)
!
!  Apply adjoint filter to 2D variables first
!
  do icol=3,1,-1
                                                 call btim(hfiltT_tim)
     call dibetat(km2,1-hx,1,im,im+hx, 1-hy,1,jm,jm+hy, nfil,  &
                  dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol), VM2D, ff, iout,jout)
                                                 call etim(hfiltT_tim)
                                                 call btim(bocoT_tim)
     call this%bocoT_2d(VM2D,km2,im,jm,hx,hy)
                                                 call etim(bocoT_tim)
  enddo

  do icol=3,1,-1
     if(l_hgen) then
                                                 call btim(hfiltT_tim)
        call dibetat(km2,1-hx,1,im,im+hx, 1-hy,1,jm,jm+hy, nfil,  &
                     dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol), HM2D, ff, iout,jout)
                                                 call etim(hfiltT_tim)
     endif
                                                 call btim(bocoT_tim)
        call this%bocoT_2d(HM2D,km2,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
  enddo
!
! Create and apply adjoint filter to extended 3D variables
!
        W(:,:,:,1:lm)=VM3D(:,:,:,1:lm)
     do icol=7,1,-1
                                                 call btim(hfiltT_tim)
        do L=1,hz
           W(:,:,:,1-L )=W(:,:,:,1+L )
           W(:,:,:,LM+L)=W(:,:,:,LM-L)
        enddo
        call dibetat(km3,1-hx,1,im,im+hx, 1-hy,1,jm,jm+hy, 1-hz,1,lm,lm+hz,icol, nfil  &
                    ,qcols,dixs3,diys3,dizs3,JCOL,vpasp3, W, ff, iout,jout,lout)
                                                 call etim(hfiltT_tim)
                                                 call btim(bocoT_tim)
        call this%bocoT_3d(W,km3,im,jm,Lm,hx,hy,hz,Fimax,Fjmax)
                                                 call etim(bocoT_tim)
     enddo

     if(l_hgen)  then
        H(:,:,:,1:lm)=HM3D(:,:,:,1:lm)
     endif
     do icol=7,1,-1
        if(l_hgen)  then
                                                 call btim(hfiltT_tim)
           do L=1,hz
              H(:,:,:,1-L )=H(:,:,:,1+L )
              H(:,:,:,LM+L)=H(:,:,:,LM-L)
           end do
           call dibetat(km3,1-hx,1,im,im+hx, 1-hy,1,jm,jm+hy, 1-hz,1,lm,lm+hz,icol, nfil  &
                       ,qcols,dixs3,diys3,dizs3,JCOL,vpasp3, H, ff, iout,jout,lout)
                                                 call etim(hfiltT_tim)
        endif
                                                 call btim(bocoT_tim)
           call this%bocoT_3d(H,km3,im,jm,Lm,hx,hy,hz,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
     enddo
!
! Go back from extended 3D variables and combine them with 2D variables in one stacked variable
!
                                                 call btim(hfiltT_tim)
     VM3D(:,:,:,1:lm)=W(:,:,:,1:lm)
     call this%composite_to_stack(VM2D,VM3D,VALL)
  if(l_hgen)  then
     HM3D(:,:,:,1:lm)=H(:,:,:,1:lm)
     call this%composite_to_stack(HM2D,HM3D,HALL)
  endif
                                                 call etim(hfiltT_tim)
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_all(VALL,HALL,lhelm)
                                                 call etim(weight_tim)
!***
!*** Apply Beta filter at all generations
!***

!
! From single stacked to composite variables
!
                                                 call btim(hfilt_tim)
     call this%stack_to_composite(VALL,VM2D,VM3D)
  if(l_hgen)  then
     call this%stack_to_composite(HALL,HM2D,HM3D)
  endif
                                                 call etim(hfilt_tim)
!
!  Apply filter to 2D variables first
!
     do icol=1,3
                                                 call btim(boco_tim)
        call this%boco_2d(VM2D,km2,im,jm,hx,hy)
                                                 call etim(boco_tim)
                                                 call btim(hfilt_tim)
        call dibeta(km2,1-hx,1,im,im+hx, 1-hy,1,jm,jm+hy, nfil,  &
                    dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol), VM2D, ff, iout,jout)
                                                 call etim(hfilt_tim)
     enddo

     do icol=1,3
                                                 call btim(boco_tim)
           call this%boco_2d(HM2D,km2,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)
        if(l_hgen) then
                                                 call btim(hfilt_tim)
           call dibeta(km2,1-hx,1,im,im+hx, 1-hy,1,jm,jm+hy, nfil,  &
                       dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol), HM2D, ff, iout,jout)
                                                 call etim(hfilt_tim)
        endif
     enddo
!
! Create and apply filter to extended 3D variables
!
        W(:,:,:,1:lm)=VM3D(:,:,:,1:lm)
        do L=1,hz
           do j=1-hy,jm+hy
              do i=1-hx,im+hx
                 W(:,i,j,1-L )=W(:,i,j,1+L )
                 W(:,i,j,LM+L)=W(:,i,j,LM-L)
              enddo
           enddo
        enddo

     do icol=1,7
                                                 call btim(boco_tim)
        call this%boco_3d(W,km3,im,jm,lm,hx,hy,hz,Fimax,Fjmax)
                                                 call etim(boco_tim)
                                                 call btim(hfilt_tim)
        call dibeta(km3,1-hx,1,im,im+hx, 1-hy,1,jm,jm+hy, 1-hz,1,lm,lm+hz,icol, nfil  &
                   ,qcols,dixs3,diys3,dizs3,JCOL,vpasp3, W, ff, iout,jout,lout)
                                                 call etim(hfilt_tim)
     enddo

     if(l_hgen) then
        H(:,:,:,1:lm)=HM3D(:,:,:,1:lm)
        do L=1,hz
           do j=1-hy,jm+hy
              do i=1-hx,im+hx
                 H(:,i,j,1-L )=H(:,i,j,1+L )
                 H(:,i,j,LM+L)=H(:,i,j,LM-L)
              enddo
           enddo
        enddo
     endif
     do icol=1,7
                                                 call btim(boco_tim)
           call this%boco_3d(H,km3,im,jm,lm,hx,hy,hz,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)
        if(l_hgen) then
                                                 call btim(hfilt_tim)
           call dibeta(km3,1-hx,1,im,im+hx, 1-hy,1,jm,jm+hy, 1-hz,1,lm,lm+hz,icol, nfil  &
                      ,qcols,dixs3,diys3,dizs3,JCOL,vpasp3, H, ff, iout,jout,lout)
                                                 call etim(hfilt_tim)
        endif
     enddo
!
! Go back from extended 3D variables and combine them with 2D variables in one stacked variable
!
                                                 call btim(hfilt_tim)
        VM3D(:,:,:,1:lm)=W(:,:,:,1:lm)
        call this%composite_to_stack(VM2D,VM3D,VALL)
     if(l_hgen) then
        HM3D(:,:,:,1:lm)=H(:,:,:,1:lm)
        call this%composite_to_stack(HM2D,HM3D,HALL)
     endif
                                                 call etim(hfilt_tim)
!***
!***  Downsend, interpolate and add, then zero high generations 
!***
                                                 call btim(dnsend_tim)
     call this%downsending_all(HALL,VALL,lquart)
                                                 call etim(dnsend_tim)

deallocate(VM3D)
deallocate(VM2D)
deallocate(HM3D)
deallocate(HM2D)
deallocate(W)
deallocate(H)
deallocate(JCOL)
!-----------------------------------------------------------------------
endsubroutine filtering_lin3

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_rad2_bkg(this)
!***********************************************************************
!                                                                      !
! Multigrid filtering procedure:                                       !
!                                                                      !
!     - Apply vertical filter before and after horizontal              !
!     - 2d radial filter                                               !
!                                                                      !
!***********************************************************************
implicit none
class (mg_intstate_type),target::this
integer(i_kind) L,i,j
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!-----------------------------------------------------------------------
!***
!*** Adjoint of beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfiltT_tim)
     call this%sup_vrbeta1T_bkg(km,km3,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfiltT_tim)
  endif
!***
!*** Adjoint interpolate and upsend 
!***
                                                 call btim(upsend_tim)
     call this%upsending_all(VALL,HALL,lquart)
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations 
!***
                                                 call btim(hfiltT_tim)
     call this%rbetaT(km,hx,1,im,hy,1,jm,pasp2,ss2,VALL(:,:,:))
  if(l_hgen)  then
     call this%rbetaT(km,hx,1,im,hy,1,jm,pasp2,ss2,HALL(:,:,:))
  endif
                                                 call etim(hfiltT_tim)

                                                 call btim(bocoT_tim)
     call this%bocoT_2d(VALL,km,im,jm,hx,hy)
     call this%bocoT_2d(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_all(VALL,HALL,lhelm)
                                                 call etim(weight_tim)
!***
!*** Apply Beta filter at all generations
!***
                                                 call btim(boco_tim)
     call this%boco_2d(VALL,km,im,jm,hx,hy)
     call this%boco_2d(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)

                                                 call btim(hfilt_tim)
     call this%rbeta(km,hx,1,im,hy,1,jm,pasp2,ss2,VALL(:,:,:))
  if(l_hgen)  then
     call this%rbeta(km,hx,1,im,hy,1,jm,pasp2,ss2,HALL(:,:,:))
  endif
                                                 call etim(hfilt_tim)
!***
!*** Downsend, interpolate and add, then zero high generations 
!***
                                                 call btim(dnsend_tim)
     call this%downsending_all(HALL,VALL,lquart)
                                                 call etim(dnsend_tim)
!***
!*** Apply beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfilt_tim)
     call this%sup_vrbeta1_bkg(km,km3,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfilt_tim)
  endif
!-----------------------------------------------------------------------
endsubroutine filtering_rad2_bkg

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_lin2_bkg(this)
!***********************************************************************
!                                                                      !
! Multigrid filtering procedure:                                       !
!                                                                      !
!     - Apply vertical filter before and after horizontal              !
!     - 2d line filter                                                 !
!                                                                      !
!***********************************************************************
implicit none
class (mg_intstate_type),target::this
integer(i_kind) L,i,j
integer(i_kind) icol,iout,jout
logical:: ff
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!----------------------------------------------------------------------
!***
!*** Adjoint of beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfiltT_tim)
     call this%sup_vrbeta1T_bkg(km,km3,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfiltT_tim)
  endif
!***
!*** Adjoint interpolate and upsend
!***
     
                                                 call btim(upsend_tim)
     call this%upsending_all(VALL,HALL,lquart)
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations 
!***
     do icol=3,1,-1
                                                 call btim(hfiltT_tim)
        call dibetat(km,1-hx,1,im,im+hx,1-hy,1,jm,jm+hy,nfil, &
                     dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol),VALL,ff,iout,jout)
                                                 call etim(hfiltT_tim)
                                                 call btim(bocoT_tim)
        call this%bocoT_2d(VALL,km,im,jm,hx,hy)
                                                 call etim(bocoT_tim)
     enddo

     do icol=3,1,-1
        if(l_hgen) then
                                                 call btim(hfiltT_tim)
           call dibetat(km,1-hx,1,im,im+hx,1-hy,1,jm,jm+hy,nfil, &
                        dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol),HALL,ff,iout,jout)
                                                 call etim(hfiltT_tim)
        endif
                                                 call btim(bocoT_tim)
           call this%bocoT_2d(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
     enddo
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_all(VALL,HALL,lhelm)
                                                 call etim(weight_tim)
!***
!*** Apply Beta filter at all generations
!***
     do icol=1,3
                                                 call btim(boco_tim)
        call this%boco_2d(VALL,km,im,jm,hx,hy)
                                                 call etim(boco_tim)
                                                 call btim(hfilt_tim)
        call dibeta(km,1-hx,1,im,im+hx,1-hy,1,jm,jm+hy,nfil, &
                    dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol),VALL,ff,iout,jout)
                                                 call etim(hfilt_tim)
     enddo

     do icol=1,3
                                                 call btim(boco_tim)
           call this%boco_2d(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)
        if(l_hgen) then
                                                 call btim(hfilt_tim)
           call dibeta(km,1-hx,1,im,im+hx,1-hy,1,jm,jm+hy,nfil, &
                       dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol),HALL,ff,iout,jout)
                                                 call etim(hfilt_tim)
        endif
     enddo
!***
!*** Downsend, interpolate and add, then zero high generations 
!***
                                                 call btim(dnsend_tim)
     call this%downsending_all(HALL,VALL,lquart)
                                                 call etim(dnsend_tim)
!***
!*** Apply beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfilt_tim)
     call this%sup_vrbeta1_bkg(km,km3,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfilt_tim)
  endif
!-----------------------------------------------------------------------
endsubroutine filtering_lin2_bkg

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_fast_bkg(this)
!***********************************************************************
!                                                                      !
! Fast multigrid filtering procedure:                                  !
!                                                                      !
!     - Apply adjoint of vertical filter before and directec vertical  !
!       filter after horizontal                                        !
!     - 1d+1d horizontal filter                                        !
!                                                                      !
!***********************************************************************
implicit none
class (mg_intstate_type),target::this
integer(i_kind) L,i,j
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!-----------------------------------------------------------------------
!***
!*** Adjoint of beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfiltT_tim)
     call this%sup_vrbeta1T_bkg(km,km3,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfiltT_tim)
  endif
!***
!*** Adjoint interpolate and upsend 
!***
                                                 call btim(upsend_tim)
     call this%upsending_all(VALL,HALL,lquart)
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations 
!***
                                                 call btim(hfiltT_tim)
     do i=im,1,-1
        call this%rbetaT(km,hy,1,jm,paspy,ssy,VALL(:,i,:))
     enddo
                                                 call etim(hfiltT_tim)
                                                 call btim(bocoT_tim)
        call this%bocoTy(VALL,km,im,jm,hx,hy)
                                                 call etim(bocoT_tim)
                                                 call btim(hfiltT_tim)
     do j=jm,1,-1
        call this%rbetaT(km,hx,1,im,paspx,ssx,VALL(:,:,j))
     enddo
                                                 call etim(hfiltT_tim)
                                                 call btim(bocoT_tim)
        call this%bocoTx(VALL,km,im,jm,hx,hy)
                                                 call etim(bocoT_tim)
  if(l_hgen) then
                                                 call btim(hfiltT_tim)
     do i=im,1,-1
        call this%rbetaT(km,hy,1,jm,paspy,ssy,HALL(:,i,:))
     enddo
                                                 call etim(hfiltT_tim)
  endif
                                                 call btim(bocoT_tim)
        call this%bocoTy(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
  if(l_hgen) then
                                                 call btim(hfiltT_tim)
     do j=jm,1,-1
        call this%rbetaT(km,hx,1,im,paspx,ssx,HALL(:,:,j))
     enddo
                                                 call etim(hfiltT_tim)
  endif
                                                 call btim(bocoT_tim)
        call this%bocoTx(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_all(VALL,HALL,lhelm)
                                                 call etim(weight_tim)
!***
!*** Apply Beta filter at all generations
!***
                                                 call btim(boco_tim)
        call this%bocox(VALL,km,im,jm,hx,hy)
                                                 call etim(boco_tim)
                                                 call btim(hfilt_tim)
     do j=1,jm
        call this%rbeta(km,hx,1,im,paspx,ssx,VALL(:,:,j))
     enddo
                                                 call etim(hfilt_tim)
                                                 call btim(boco_tim)
        call this%bocoy(VALL,km,im,jm,hx,hy)
                                                 call etim(boco_tim)
                                                 call btim(hfilt_tim)
     do i=1,im
        call this%rbeta(km,hy,1,jm,paspy,ssy,VALL(:,i,:))
     enddo
                                                 call etim(hfilt_tim)
                                                 call btim(boco_tim)
        call this%bocox(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)
  if(l_hgen)  then
                                                 call btim(hfilt_tim)
     do j=1,jm
        call this%rbeta(km,hx,1,im,paspx,ssx,HALL(:,:,j))
     enddo
                                                 call etim(hfilt_tim)
  endif
                                                 call btim(boco_tim)
        call this%bocoy(HALL,km,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)
  if(l_hgen)  then
                                                 call btim(hfilt_tim)
     do i=1,im
        call this%rbeta(km,hy,1,jm,paspy,ssy,HALL(:,i,:))
     enddo
                                                 call etim(hfilt_tim)
  endif
!***
!*** Downsend, interpolate and add, then zero high generations 
!***
                                                 call btim(dnsend_tim)
     call this%downsending_all(HALL,VALL,lquart)
                                                 call etim(dnsend_tim)
!***
!*** Apply beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfilt_tim)
     call this%sup_vrbeta1_bkg(km,km3,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfilt_tim)
  endif
!-----------------------------------------------------------------------
endsubroutine filtering_fast_bkg

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_rad2_ens(this,mg_filt_flag)
!***********************************************************************
!                                                                      !
! Multigrid filtering procedure for ensemble:                          !
!                                                                      !
!     - Apply vertical filter before and after horizontal              !
!     - 2d radial filter                                               !
!     - Version for localization of ensemble                           !
!                                                                      !
!***********************************************************************
implicit none
class (mg_intstate_type),target::this
integer(i_kind),intent(in):: mg_filt_flag
integer(i_kind) L,i,j
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!-----------------------------------------------------------------------
if(mg_filt_flag==1) then
                                                 call btim(upsend_tim)
  call this%upsending_ens_nearest(VALL,HALL,km_all)
                                                 call etim(upsend_tim)
else
!***
!*** Adjoint of beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfiltT_tim)
     call this%sup_vrbeta1T_ens(km3_all,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfiltT_tim)
  endif
!***
!*** Adjoint interpolate and upsend
!***
                                                 call btim(upsend_tim)
  if(lquart) then
     call this%upsending2_ens(VALL,HALL,km_all)
  else
     call this%upsending_ens(VALL,HALL,km_all)
  endif
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations
!***
                                                 call btim(hfiltT_tim)
  if(l_filt_g1) then
     call this%rbetaT(km_all,hx,1,im,hy,1,jm,pasp2,ss2,VALL(:,:,:))
  endif
  if(l_hgen)  then
     call this%rbetaT(km_all,hx,1,im,hy,1,jm,pasp2,ss2,HALL(:,:,:))
  endif
                                                 call etim(hfiltT_tim)

                                                 call btim(bocoT_tim)
  if(l_filt_g1) then
     call this%bocoT_2d(VALL,km_all,im,jm,hx,hy)
  endif
     call this%bocoT_2d(HALL,km_all,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
endif
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_ens(VALL,HALL,km_all)
                                                 call etim(weight_tim)

if(mg_filt_flag==-1) then
                                                 call btim(dnsend_tim)
  call this%downsending_ens_nearest(HALL,VALL,km_all)
                                                 call etim(dnsend_tim)
else
!***
!*** Apply Beta filter at all generations
!***
                                                 call btim(boco_tim)
  if(l_filt_g1) then
     call this%boco_2d(VALL,km_all,im,jm,hx,hy)
  endif
     call this%boco_2d(HALL,km_all,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)

                                                 call btim(hfilt_tim)
  if(l_filt_g1) then
     call this%rbeta(km_all,hx,1,im,hy,1,jm,pasp2,ss2,VALL(:,:,:))
  endif
  if(l_hgen)  then
     call this%rbeta(km_all,hx,1,im,hy,1,jm,pasp2,ss2,HALL(:,:,:))
  endif
                                                 call etim(hfilt_tim)
!***
!*** Downsend, interpolate and add, then zero high generations 
!***
                                                 call btim(dnsend_tim)
  if(lquart) then
     call this%downsending2_ens(HALL,VALL,km_all)
  else
     call this%downsending_ens(HALL,VALL,km_all)
  endif
                                                 call etim(dnsend_tim)
!***
!*** Apply beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfilt_tim)
     call this%sup_vrbeta1_ens(km3_all,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfilt_tim)
  endif
endif
!-----------------------------------------------------------------------
endsubroutine filtering_rad2_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_lin2_ens(this,mg_filt_flag)
!***********************************************************************
!                                                                      !
! Multigrid filtering procedure for ensemble:                          !
!                                                                      !
!     - Vertical filter before and after horizontal                    !
!     - Line filters in horizontal                                     !
!     - Version for localization of ensemble                           !
!                                                                      !
!***********************************************************************
implicit none
class (mg_intstate_type),target::this
integer(i_kind),intent(in):: mg_filt_flag
integer(i_kind) L,i,j
integer(i_kind) icol,iout,jout
logical:: ff
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!----------------------------------------------------------------------
if(mg_filt_flag==1) then
                                                 call btim(upsend_tim)
  call this%upsending_ens_nearest(VALL,HALL,km_all)
                                                 call etim(upsend_tim)
else
!***
!*** Adjoint of beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfiltT_tim)
     call this%sup_vrbeta1T_ens(km3_all,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfiltT_tim)
  endif
!***
!*** Adjoint interpolate and upsend
!***
                                                 call btim(upsend_tim)
  if(lquart) then
     call this%upsending2_ens(VALL,HALL,km_all)
  else
     call this%upsending_ens(VALL,HALL,km_all)
  endif
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations
!***
  if(l_filt_g1) then
     do icol=3,1,-1
                                                 call btim(hfiltT_tim)
        call dibetat(km_all,1-hx,1,im,im+hx,1-hy,1,jm,jm+hy,nfil, &
                     dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol),VALL,ff,iout,jout)
                                                 call etim(hfiltT_tim)
                                                 call btim(bocoT_tim)
        call this%bocoT_2d(VALL,km_all,im,jm,hx,hy)
                                                 call etim(bocoT_tim)
     enddo
  endif

  do icol=3,1,-1
     if(l_hgen)  then
                                                 call btim(hfiltT_tim)
        call dibetat(km_all,1-hx,1,im,im+hx,1-hy,1,jm,jm+hy,nfil, &
                     dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol),HALL,ff,iout,jout)
                                                 call etim(hfiltT_tim)
     endif
                                                 call btim(bocoT_tim)
        call this%bocoT_2d(HALL,km_all,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
  enddo
endif
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_ens(VALL,HALL,km_all)
                                                 call etim(weight_tim)

if(mg_filt_flag==-1) then
                                                 call btim(dnsend_tim)
  call this%downsending_ens_nearest(HALL,VALL,km_all)
                                                 call etim(dnsend_tim)
else
!***
!*** Apply Beta filter at all generations
!***
  if(l_filt_g1) then
     do icol=1,3
                                                 call btim(boco_tim)
        call this%boco_2d(VALL,km_all,im,jm,hx,hy)
                                                 call etim(boco_tim)
                                                 call btim(hfilt_tim)
        call dibeta(km_all,1-hx,1,im,im+hx,1-hy,1,jm,jm+hy,nfil, &
                    dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol),VALL,ff,iout,jout)
                                                 call etim(hfilt_tim)
     enddo
  endif

  do icol=1,3
                                                 call btim(boco_tim)
        call this%boco_2d(HALL,km_all,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)
     if(l_hgen) then
                                                 call btim(hfilt_tim)
        call dibeta(km_all,1-hx,1,im,im+hx,1-hy,1,jm,jm+hy,nfil, &
                    dixs(:,:,icol),diys(:,:,icol),hss2(:,:,icol),HALL,ff,iout,jout)
                                                 call etim(hfilt_tim)
     endif
  enddo
!***
!*** Downsend, interpolate and add, then zero high generations
!***
                                                 call btim(dnsend_tim)
  if(lquart) then
     call this%downsending2_ens(HALL,VALL,km_all)
  else
     call this%downsending_ens(HALL,VALL,km_all)
  endif
                                                 call etim(dnsend_tim)
!***
!*** Apply beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfilt_tim)
     call this%sup_vrbeta1_ens(km3_all,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfilt_tim)
  endif
endif
!-----------------------------------------------------------------------
endsubroutine filtering_lin2_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_fast_ens(this,mg_filt_flag)
!***********************************************************************
!                                                                      !
! Fast multigrid filtering procedure for ensemble:                     !
!                                                                      !
!     - Apply vertical filter before and after horizontal              !
!     - 1d+1d horizontal filter + 1d vertical filter                   !
!     - Version for localizaiton of ensemble                           !
!                                                                      !
!***********************************************************************
implicit none
class (mg_intstate_type),target::this
integer(i_kind),intent(in):: mg_filt_flag
integer(i_kind) L,i,j
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!-----------------------------------------------------------------------
if(mg_filt_flag==1) then
                                                 call btim(upsend_tim)
  call this%upsending_ens_nearest(VALL,HALL,km_all)
                                                 call etim(upsend_tim)
else
!***
!*** Adjoint of beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfiltT_tim)
     call this%sup_vrbeta1T_ens(km3_all,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfiltT_tim)
  endif
!***
!*** Adjoint interpolate and upsend
!***
                                                 call btim(upsend_tim)
  if(lquart) then
     call this%upsending2_ens(VALL,HALL,km_all)
  else
     call this%upsending_ens(VALL,HALL,km_all)
  endif
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations
!***
  if(l_filt_g1) then
                                                 call btim(hfiltT_tim)
     do i=im,1,-1
        call this%rbetaT(km_all,hy,1,jm,paspy,ssy,VALL(:,i,:))
     enddo
                                                 call etim(hfiltT_tim)
                                                 call btim(bocoT_tim)
        call this%bocoTy(VALL,km_all,im,jm,hx,hy)
                                                 call etim(bocoT_tim)
                                                 call btim(hfiltT_tim)
     do j=jm,1,-1
        call this%rbetaT(km_all,hx,1,im,paspx,ssx,VALL(:,:,j))
     enddo
                                                 call etim(hfiltT_tim)
                                                 call btim(bocoT_tim)
        call this%bocoTx(VALL,km_all,im,jm,hx,hy)
                                                 call etim(bocoT_tim)
  endif
  if(l_hgen) then
                                                 call btim(hfiltT_tim)
     do i=im,1,-1
        call this%rbetaT(km_all,hy,1,jm,paspy,ssy,HALL(:,i,:))
     enddo
                                                 call etim(hfiltT_tim)
  endif
                                                 call btim(bocoT_tim)
        call this%bocoTy(HALL,km_all,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
  if(l_hgen) then
                                                 call btim(hfiltT_tim)
     do j=jm,1,-1
        call this%rbetaT(km_all,hx,1,im,paspx,ssx,HALL(:,:,j))
     enddo
                                                 call etim(hfiltT_tim)
  endif
                                                 call btim(bocoT_tim)
        call this%bocoTx(HALL,km_all,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(bocoT_tim)
endif
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_ens(VALL,HALL,km_all)
                                                 call etim(weight_tim)

if(mg_filt_flag==-1) then
                                                 call btim(dnsend_tim)
  call this%downsending_ens_nearest(HALL,VALL,km_all)
                                                 call etim(dnsend_tim)
else
!***
!*** Apply Beta filter at all generations
!***
  if(l_filt_g1) then
                                                 call btim(boco_tim)
        call this%bocox(VALL,km_all,im,jm,hx,hy)
                                                 call etim(boco_tim)
                                                 call btim(hfilt_tim)
     do j=1,jm
        call this%rbeta(km_all,hx,1,im,paspx,ssx,VALL(:,:,j))
     enddo
                                                 call etim(hfilt_tim)
                                                 call btim(boco_tim)
        call this%bocoy(VALL,km_all,im,jm,hx,hy)
                                                 call etim(boco_tim)
                                                 call btim(hfilt_tim)
     do i=1,im
        call this%rbeta(km_all,hy,1,jm,paspy,ssy,VALL(:,i,:))
     enddo
                                                 call etim(hfilt_tim)
  endif
                                                 call btim(boco_tim)
        call this%bocox(HALL,km_all,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)
  if(l_hgen) then
                                                 call btim(hfilt_tim)
     do j=1,jm
        call this%rbeta(km_all,hx,1,im,paspx,ssx,HALL(:,:,j))
     enddo
                                                 call etim(hfilt_tim)
  endif
                                                 call btim(boco_tim)
        call this%bocoy(HALL,km_all,im,jm,hx,hy,Fimax,Fjmax,2,gm)
                                                 call etim(boco_tim)
  if(l_hgen) then
                                                 call btim(hfilt_tim)
     do i=1,im
        call this%rbeta(km_all,hy,1,jm,paspy,ssy,HALL(:,i,:))
     enddo
                                                 call etim(hfilt_tim)
  endif
!***
!*** Downsend, interpolate and add, then zero high generations
!***
                                                 call btim(dnsend_tim)
  if(lquart) then
     call this%downsending2_ens(HALL,VALL,km_all)
  else
     call this%downsending_ens(HALL,VALL,km_all)
  endif
                                                 call etim(dnsend_tim)
!***
!*** Apply beta filter in vertical direction
!***
  if(l_vertical_filter) then
                                                 call btim(vfilt_tim)
     call this%sup_vrbeta1_ens(km3_all,hx,hy,hz,im,jm,lm,pasp1,ss1,VALL)
                                                 call etim(vfilt_tim)
  endif
endif
!-----------------------------------------------------------------------
endsubroutine filtering_fast_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine filtering_rad_highest(this)
!***********************************************************************
!                                                                      !
! Multigrid filtering procedure:                                       !
!                                                                      !
!     - 2d radial filter only for the highest generation               !
!     - Without horizontal parallelization                             !
!                                                                      !
!***********************************************************************
implicit none
class(mg_intstate_type),target:: this
include "type_parameter_locpointer.inc"
include "type_intstat_locpointer.inc"
include "type_parameter_point2this.inc"
include "type_intstat_point2this.inc"
!-----------------------------------------------------------------------

!***
!*** Adjoint interpolate and upsend
!***
                                                 call btim(upsend_tim)
     call this%upsending_highest(VALL,HALL)
                                                 call etim(upsend_tim)
!***
!*** Apply adjoint of Beta filter at all generations 
!***
                                                 call btim(hfiltT_tim)
     call this%rbetaT(km,hx,1,imH,hy,1,jmH,&
          &pasp2(:,:,1:imH,1:jmH),ss2(1:imH,1:jmH),HALL(:,1-hx:imH+hx,1-hy:jmH+hy))
                                                 call etim(hfiltT_tim)
!***
!*** Apply (a-b\nabla^2)
!***
                                                 call btim(weight_tim)
     call this%weighting_highest(HALL(:,1-hx:imH+hx,1-hy:jmH+hy))
                                                 call etim(weight_tim)
!***
!*** Apply Beta filter at all generations
!***
                                                 call btim(hfilt_tim)
     call this%rbeta(km,hx,1,imH,hy,1,jmH,&
          &pasp2(:,:,1:imH,1:jmH),ss2(1:imH,1:jmH),HALL(:,1-hx:imH+hx,1-hy:jmH+hy))
                                                 call etim(hfilt_tim)
!***
!***  Downsend, interpolate and add, then zero high generations 
!***
                                                 call btim(dnsend_tim)
     call this%downsending_highest(HALL,VALL)
                                                 call etim(dnsend_tim)

!-----------------------------------------------------------------------
endsubroutine filtering_rad_highest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sup_vrbeta1 &
!**********************************************************************
!                                                                     *
!     conversion of vrbeta1                                           *
!                                                                     *
!**********************************************************************
(this,kmax,hx,hy,hz,im,jm,lm,pasp,ss,V)
!----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: kmax,hx,hy,hz,im,jm,lm
real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1:lm),intent(inout):: V
real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
real(r_kind),dimension(1:lm), intent(in):: ss
real(r_kind),dimension(1:kmax,1-hz:lm+hz):: W
integer(i_kind):: i,j,L
!----------------------------------------------------------------------

        do j=1,jm
        do i=1,im
          do L=1,Lm
            W(:,L)=V(:,i,j,L)
          end do
          do L=1,hz
            W(:,1-L)=W(:,1+L)
            W(:,LM+L)=W(:,LM-L)
          end do
             call this%rbeta(kmax,hz,1,lm,  pasp,ss,W)
          do l=1,Lm
            V(:,i,j,L)=W(:,L)
          end do
        end do
        end do
  
!----------------------------------------------------------------------
endsubroutine sup_vrbeta1

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sup_vrbeta1T &
!**********************************************************************
!                                                                     *
!     Adjoint of sup_vrbeta1                                          *
!                                                                     *
!**********************************************************************
(this,kmax,hx,hy,hz,im,jm,lm,pasp,ss,V)
!----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: kmax,hx,hy,hz,im,jm,lm
real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1:lm),intent(inout):: V
real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
real(r_kind),dimension(1:lm), intent(in):: ss
real(r_kind),dimension(1:kmax,1-hz:lm+hz):: W
integer(i_kind):: i,j,L
!----------------------------------------------------------------------

        do j=1,jm
        do i=1,im
          do L=1,Lm
            W(:,L)=V(:,i,j,L)
          end do
          do L=1,hz
            W(:,1-L )=W(:,1+L )
            W(:,LM+L)=W(:,LM-L)
          end do
             call this%rbetaT(kmax,hz,1,lm, pasp,ss,W)
!
! Apply adjoint at the edges of domain
!
          do L=1,hz
            W(:,1+L)=W(:,1+L)+W(:,1-L)
            W(:,LM-L)=W(:,LM-L)+W(:,LM+L)
          enddo
          do l=1,Lm
            V(:,i,j,L)=W(:,L)
          end do
        end do
        end do

!----------------------------------------------------------------------
endsubroutine sup_vrbeta1T

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sup_vrbeta3 &
!**********************************************************************
!                                                                     *
!     conversion of vrbeta3                                           *
!                                                                     *
!**********************************************************************
(this,kmax,hx,hy,hz,im,jm,lm,pasp,ss,V)
!----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: kmax,hx,hy,hz,im,jm,lm
real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1:lm),intent(inout):: V
real(r_kind),dimension(3,3,1:im,1:jm,1:lm), intent(in):: pasp
real(r_kind),dimension(1:im,1:jm,1:lm), intent(in):: ss
real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1-hz:lm+hz):: W
integer(i_kind):: i,j,L
!----------------------------------------------------------------------

          do L=1,Lm
          do j=1-hy,jm+hy
          do i=1-hx,im+hx
            W(:,i,j,L)=V(:,i,j,L)
          end do
          end do
          end do

        do L=1,hz
          do j=1-hy,jm+hy
          do i=1-hx,im+hx
              W(:,i,j,1-L )=W(:,i,j,1+L )
              W(:,i,j,LM+L)=W(:,i,j,LM-L)
          end do
          end do
        end do
    
    
           call this%rbeta(kmax,hx,1,im, hy,1,jm, hz,1,lm, pasp,ss,W)

  
          do l=1,Lm
          do j=1,jm
          do i=1,im
            V(:,i,j,L)=W(:,i,j,L)
          end do
          end do
          end do

!----------------------------------------------------------------------
endsubroutine sup_vrbeta3
 
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sup_vrbeta3T &
!**********************************************************************
!                                                                     *
!     Adjoint of sup_vrbeta3                                          *
!                                                                     *
!**********************************************************************
(this,kmax,hx,hy,hz,im,jm,lm,pasp,ss,V)
!----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: kmax,hx,hy,hz,im,jm,lm
real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1:lm),intent(inout):: V
real(r_kind),dimension(3,3,1:im,1:jm,1:lm), intent(in):: pasp
real(r_kind),dimension(1:im,1:jm,1:lm), intent(in):: ss
real(r_kind),dimension(1:kmax,1-hx:im+hx,1-hy:jm+hy,1-hz:lm+hz):: W
integer(i_kind):: i,j,l
!----------------------------------------------------------------------

          do L=1,Lm
          do j=1-hy,jm+hy
          do i=1-hx,im+hx
            W(:,i,j,L)=V(:,i,j,L)
          end do
          end do
          end do

        do L=1,hz
          do j=1-hy,jm+hy
          do i=1-hx,im+hx
              W(:,i,j,1-L )=W(:,i,j, 1+L)
              W(:,i,j,LM+L)=W(:,i,j,LM-L)
          end do
          end do
        end do
    
    
           call this%rbetaT(kmax,hx,1,im, hy,1,jm, hz,1,lm, pasp,ss,W)

!
! Apply adjoint at the edges of domain
!
        do L=1,hz
          do j=1-hy,jm+hy
          do i=1-hx,im+hx
              W(:,i,j,1+L )=W(:,i,j, 1+L)+W(:,i,j, 1-L)
              W(:,i,j,LM-L)=W(:,i,j,LM-L)+W(:,i,j,LM+L)
          end do
          end do
         end do
  
          do l=1,lm
          do j=1,jm
          do i=1,im
            V(:,i,j,l)=W(:,i,j,l)
          end do
          end do
          end do

!----------------------------------------------------------------------
endsubroutine sup_vrbeta3T

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sup_vrbeta1_ens &
!**********************************************************************
!                                                                     *
!     conversion of vrbeta1                                           *
!                                                                     *
!**********************************************************************
(this,km_en,hx,hy,hz,im,jm,lm,pasp,ss,VALL)
!----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: km_en,hx,hy,hz,im,jm,lm
real(r_kind),dimension(1:km_en*lm,1-hx:im+hx,1-hy:jm+hy),intent(inout):: VALL
real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
real(r_kind),dimension(1:lm), intent(in):: ss
real(r_kind),dimension(1:km_en,1-hz:lm+hz):: W
integer(i_kind):: i,j,L,k,k_ind,kloc
!----------------------------------------------------------------------

    do j=1,jm
    do i=1,im
      do k=1,km_en
        k_ind =(k-1)*Lm
          do L=1,Lm
            kloc=k_ind+L
            W(k,L)=VALL(kloc,i,j)
          end do
      enddo
          do L=1,hz
            W(:,1-L )=W(:,1+L )
            W(:,LM+L)=W(:,LM-L)
          end do

             call this%rbeta(km_en,hz,1,lm,  pasp,ss,W)

      do k=1,km_en
        k_ind =(k-1)*Lm
          do L=1,Lm
            kloc=k_ind+L
            VALL(kloc,i,j)= W(k,L)
          end do
      enddo
   enddo
   enddo

!----------------------------------------------------------------------
endsubroutine sup_vrbeta1_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sup_vrbeta1T_ens &
!**********************************************************************
!                                                                     *
!     Adjoint of sup_vrbeta1_ens                                      *
!                                                                     *
!**********************************************************************
(this,km_en,hx,hy,hz,im,jm,lm,pasp,ss,VALL)
!----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: km_en,hx,hy,hz,im,jm,lm
real(r_kind),dimension(1:km_en*lm,1-hx:im+hx,1-hy:jm+hy),intent(inout):: VALL
real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
real(r_kind),dimension(1:lm), intent(in):: ss
real(r_kind),dimension(1:km_en,1-hz:lm+hz):: W
integer(i_kind):: i,j,L,k,k_ind,kloc
!----------------------------------------------------------------------

        do j=1,jm
        do i=1,im

          do k=1,km_en
            k_ind = (k-1)*Lm
              do L=1,Lm
                kloc=k_ind+L
                W(k,L)=VALL(kloc,i,j)
              end do
          enddo
             do L=1,hz
                W(:,1-L )=W(:,1+L )
                W(:,LM+L)=W(:,LM-L)
             end do

               call this%rbetaT(km_en,hz,1,lm, pasp,ss,W)
!
! Apply adjoint at the edges of domain
!
             do L=1,hz
               W(:,1+L )=W(:,1+L )+W(:,1-L)
               W(:,LM-L)=W(:,LM-L)+W(:,LM+L)
             enddo

          do k=1,km_en
            k_ind = (k-1)*Lm
              do l=1,Lm
                kloc=k_ind+L
                VALL(kloc,i,j)=W(k,L)
              enddo
          end do

        end do
        end do

!----------------------------------------------------------------------
endsubroutine sup_vrbeta1T_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sup_vrbeta1_bkg &
!**********************************************************************
!                                                                     *
!     conversion of vrbeta1                                           *
!                                                                     *
!**********************************************************************
(this,km,km3,hx,hy,hz,im,jm,lm,pasp,ss,VALL)
!----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: km,km3,hx,hy,hz,im,jm,lm
real(r_kind),dimension(1:km,1-hx:im+hx,1-hy:jm+hy),intent(inout):: VALL
real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
real(r_kind),dimension(1:lm), intent(in):: ss
real(r_kind),dimension(1:km3,1-hz:lm+hz):: W
integer(i_kind):: i,j,L,k,k_ind,kloc
!----------------------------------------------------------------------

    do j=1,jm
    do i=1,im
      do k=1,km3
        k_ind =(k-1)*Lm
          do L=1,Lm
            kloc=k_ind+L
            W(k,L)=VALL(kloc,i,j)
          end do
      enddo
          do L=1,hz
            W(:,1-L )=W(:,1+L )
            W(:,LM+L)=W(:,LM-L)
          end do

             call this%rbeta(km3,hz,1,lm,  pasp,ss,W)

      do k=1,km3
        k_ind =(k-1)*Lm
          do L=1,Lm
            kloc=k_ind+L
            VALL(kloc,i,j)= W(k,L)
          end do
      enddo
   enddo
   enddo

!----------------------------------------------------------------------
endsubroutine sup_vrbeta1_bkg

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine sup_vrbeta1T_bkg &
!**********************************************************************
!                                                                     *
!     Adjoint of sup_vrbeta1_bkg                                      *
!                                                                     *
!**********************************************************************
(this,km,km3,hx,hy,hz,im,jm,lm,pasp,ss,VALL)
!----------------------------------------------------------------------
implicit none
class(mg_intstate_type),target::this
integer(i_kind),intent(in):: km,km3,hx,hy,hz,im,jm,lm
real(r_kind),dimension(1:km,1-hx:im+hx,1-hy:jm+hy),intent(inout):: VALL
real(r_kind),dimension(1,1,1:lm), intent(in):: pasp
real(r_kind),dimension(1:lm), intent(in):: ss
real(r_kind),dimension(1:km3,1-hz:lm+hz):: W
integer(i_kind):: i,j,L,k,k_ind,kloc
!----------------------------------------------------------------------

        do j=1,jm
        do i=1,im

          do k=1,km3
            k_ind = (k-1)*Lm
              do L=1,Lm
                kloc=k_ind+L
                W(k,L)=VALL(kloc,i,j)
              end do
          enddo
             do L=1,hz
                W(:,1-L )=W(:,1+L )
                W(:,LM+L)=W(:,LM-L)
             end do

               call this%rbetaT(km3,hz,1,lm, pasp,ss,W)
!
! Apply adjoint at the edges of domain
!
             do L=1,hz
               W(:,1+L )=W(:,1+L )+W(:,1-L)
               W(:,LM-L)=W(:,LM-L)+W(:,LM+L)
             enddo

          do k=1,km3
            k_ind = (k-1)*Lm
              do l=1,Lm
                kloc=k_ind+L
                VALL(kloc,i,j)=W(k,L)
              enddo
          end do

        end do
        end do

!----------------------------------------------------------------------
endsubroutine sup_vrbeta1T_bkg

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end submodule mg_filtering
