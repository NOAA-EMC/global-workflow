      module d3d_def
      use machine
      implicit none
!
      real(kind=kind_rad) ,allocatable :: dt3dt(:,:,:,:)
      real(kind=kind_rad) ,allocatable :: dq3dt(:,:,:,:)
      real(kind=kind_rad) ,allocatable :: du3dt(:,:,:,:)
      real(kind=kind_rad) ,allocatable :: dv3dt(:,:,:,:)
      real(kind=kind_rad) ,allocatable :: upd_mf(:,:,:)
     &,                                   dwn_mf(:,:,:)
     &,                                   det_mf(:,:,:)
      real(kind=kind_rad) ,allocatable :: cldcov(:,:,:)
!
      contains
!
      subroutine d3d_init(lonr,lats_node_r,levs,pl_coeff,
     &                    ldiag3d)
      implicit none
      integer lonr,lats_node_r,levs,pl_coeff
      logical ldiag3d
!
      if (ldiag3d) then
        allocate (dt3dt(lonr,levs,6,lats_node_r))
        allocate (du3dt(lonr,levs,4,lats_node_r))
        allocate (dv3dt(lonr,levs,4,lats_node_r))
      else
        allocate (dt3dt(1,1,6,1))
        allocate (du3dt(1,1,4,1))
        allocate (dv3dt(1,1,4,1))
      endif
      if (ldiag3d) then
        allocate (dq3dt(lonr,levs,5+pl_coeff,lats_node_r))
        allocate (cldcov(lonr,levs,lats_node_r))
        allocate (upd_mf(lonr,levs,lats_node_r))
        allocate (dwn_mf(lonr,levs,lats_node_r))
        allocate (det_mf(lonr,levs,lats_node_r))
      else
        allocate (dq3dt(1,1,5+pl_coeff,1))
        allocate (cldcov(1,1,1))
        allocate (upd_mf(1,1,1))
        allocate (dwn_mf(1,1,1))
        allocate (det_mf(1,1,1))
      endif
!
      end subroutine d3d_init
      subroutine d3d_zero(ldiag3d)
      implicit none
      logical ldiag3d
      real, parameter :: zero=0.0
!
       if (ldiag3d) then
         dt3dt  = zero
         du3dt  = zero
         dv3dt  = zero
       endif
       if (ldiag3d) then
         dq3dt  = zero
         cldcov = zero
         upd_mf = zero
         dwn_mf = zero
         det_mf = zero
       endif
!
      end subroutine d3d_zero
      end module d3d_def
