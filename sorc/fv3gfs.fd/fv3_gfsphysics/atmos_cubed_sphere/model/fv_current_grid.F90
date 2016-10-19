!***********************************************************************
!*                   GNU General Public License                        *
!* This file is a part of fvGFS.                                       *
!*                                                                     *
!* fvGFS is free software; you can redistribute it and/or modify it    *
!* and are expected to follow the terms of the GNU General Public      *
!* License as published by the Free Software Foundation; either        *
!* version 2 of the License, or (at your option) any later version.    *
!*                                                                     *
!* fvGFS is distributed in the hope that it will be useful, but        *
!* WITHOUT ANY WARRANTY; without even the implied warranty of          *
!* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU   *
!* General Public License for more details.                            *
!*                                                                     *
!* For the full text of the GNU General Public License,                *
!* write to: Free Software Foundation, Inc.,                           *
!*           675 Mass Ave, Cambridge, MA 02139, USA.                   *
!* or see:   http://www.gnu.org/licenses/gpl.html                      *
!***********************************************************************
module fv_current_grid_mod

#ifdef FV_CURRENT_GRID

#include <fms_platform.h>
 use mpp_domains_mod,  only: domain2d
 use fms_io_mod,       only: restart_file_type
 use fv_arrays_mod,    only: fv_atmos_type, fv_diag_type, max_step
 use time_manager_mod, only: time_type

  implicit none
  public

     type(fv_atmos_type), pointer :: current_Atm
     integer, pointer :: grid_number

     !Timestep-related variables.
     !Each grid should have its own set of timing utilities

     type(time_type) , pointer :: Time_init, Time, Run_length, Time_end, Time_step_atmos

     logical , pointer :: grid_active 

     !-----------------------------------------------------------------------
     ! Five prognostic state variables for the f-v dynamics
     !-----------------------------------------------------------------------
     ! dyn_state:
     ! D-grid prognostatic variables: u, v, and delp (and other scalars)
     !
     !     o--------u(i,j+1)----------o
     !     |           |              |
     !     |           |              |
     !  v(i,j)------scalar(i,j)----v(i+1,j)
     !     |           |              |
     !     |           |              |
     !     o--------u(i,j)------------o
     !
     ! The C grid component is "diagnostic" in that it is predicted every time step
     ! from the D grid variables.
     real,  pointer :: u(:,:,:)      ! D grid zonal wind (m/s)
     real,  pointer :: v(:,:,:)      ! D grid meridional wind (m/s)
     real,  pointer :: pt(:,:,:)     ! temperature (K)
     real,  pointer :: delp(:,:,:)   ! pressure thickness (pascal)
     real,  pointer :: q(:,:,:,:)    ! specific humidity and constituents

     !----------------------
     ! non-hydrostatic state:
     !----------------------------------------------------------------------
     real,  pointer ::     w(:,:,:)    ! cell center vertical wind (m/s)
     real,  pointer ::  delz(:,:,:)    ! layer thickness (meters)
     real,  pointer ::   ze0(:,:,:)    ! height at layer edges for remapping

     !-----------------------------------------------------------------------
     ! Auxilliary pressure arrays:
     ! The 5 vars below can be re-computed from delp and ptop.
     !-----------------------------------------------------------------------
     ! dyn_aux:
     real,  pointer :: ps (:,:)        ! Surface pressure (pascal)
     real,  pointer :: pe (:,:,: )     ! edge pressure (pascal)
     real,  pointer :: pk  (:,:,:)     ! pe**cappa
     real,  pointer :: peln(:,:,:)     ! ln(pe)
     real,  pointer :: pkz (:,:,:)     ! finite-volume mean pk
#ifdef PKC
     real,  pointer :: pkc (:,:,:)     ! finite-volume edge pk
#endif

     ! For phys coupling:
     real,  pointer :: u_srf(:,:)      ! Surface u-wind
     real,  pointer :: v_srf(:,:)      ! Surface v-wind
     real,  pointer :: sgh(:,:)        ! Terrain standard deviation
     real,  pointer :: oro(:,:)        ! land fraction (1: all land; 0: all water)
     real,  pointer :: ts(:,:)         ! skin temperature (sst) from NCEP/GFS (K) -- tile

     !-----------------------------------------------------------------------
     ! Others:
     !-----------------------------------------------------------------------
     real,  pointer :: phis(:,:)       ! Surface geopotential (g*Z_surf)
     real,  pointer :: omga(:,:,:)     ! Vertical pressure velocity (pa/s)
     real,  pointer :: ua(:,:,:)       ! (ua, va) are mostly used as the A grid winds
     real,  pointer :: va(:,:,:)     
     real,  pointer :: uc(:,:,:)       ! (uc, vc) are mostly used as the C grid winds
     real,  pointer :: vc(:,:,:)     

     real,  pointer :: ak(:)  
     real,  pointer :: bk(:)  

     ! Accumulated Mass flux arrays
     real,  pointer ::  mfx(:,:,:)  
     real,  pointer ::  mfy(:,:,:)  
     ! Accumulated Courant number arrays
     real,  pointer ::  cx(:,:,:)  
     real,  pointer ::  cy(:,:,:)  



!!!!!!!!!!!!!!!!!!
! From fv_mp_mod !
!!!!!!!!!!!!!!!!!!

     integer, pointer, dimension(:) :: pelist

     integer , pointer :: ng !this SHOULD be a constant, but structure elements are not allowed to be constants
     type(domain2D) , pointer :: domain
#if defined(SPMD)

     type(domain2D) , pointer :: domain_for_coupler ! domain used in coupled model with halo = 1.

     integer , pointer :: num_contact, npes_per_tile, tile, npes_this_grid

#endif

!!!!!!!!!!!!!!!!
!fv_diagnostics!
!!!!!!!!!!!!!!!!

     type(fv_diag_type), pointer :: idiag


!!!!!!!!!!!!!!!!!!!!!!
     ! From fv_grid_utils !
!!!!!!!!!!!!!!!!!!!!!!


     real   , pointer :: ptop


!!!!!!!!!!!!!!
! From fv_io !
!!!!!!!!!!!!!!
     type(restart_file_type) , pointer :: Fv_restart, SST_restart, Fv_tile_restart, &
          Rsf_restart, Mg_restart, Lnd_restart, Tra_restart


     !Hold on to coarse-grid global grid, so we don't have to waste processor time getting it again when starting to do grid nesting
     real,  dimension(:,:,:,:) , pointer :: grid_global


  integer, pointer :: atmos_axes(:)

!---- version number -----
  character(len=128) :: version = '$Id$'
  character(len=128) :: tagname = '$Name$'

contains

  subroutine switch_current_grid_pointers(Atm)
    type(fv_atmos_type), intent(IN), target :: Atm

     grid_number                   => Atm%grid_number

     Time_init                     => Atm%Time_init
     Time                          => Atm%Time
     Run_length                    => Atm%Run_length
     Time_end                      => Atm%Time_end
     Time_step_atmos               => Atm%Time_step_atmos
     grid_active                   => Atm%grid_active
     u                             => Atm%u
     v                             => Atm%v
     pt                            => Atm%pt
     delp                          => Atm%delp
     q                             => Atm%q
     w                             => Atm%w
     delz                          => Atm%delz
     ze0                           => Atm%ze0
     ps                            => Atm%ps
     pe                            => Atm%pe
     pk                            => Atm%pk
     peln                          => Atm%peln
     pkz                           => Atm%pkz
#ifdef PKC
     pkc                           => Atm%pkc
#endif
     u_srf                         => Atm%u_srf
     v_srf                         => Atm%v_srf
     sgh                           => Atm%sgh
     oro                           => Atm%oro
     ts                            => Atm%ts
     phis                          => Atm%phis
     omga                          => Atm%omga
     ua                            => Atm%ua
     va                            => Atm%va
     uc                            => Atm%uc
     vc                            => Atm%vc
     ak                            => Atm%ak
     bk                            => Atm%bk
     mfx                           => Atm%mfx
     mfy                           => Atm%mfy
     cx                            => Atm%cx
     cy                            => Atm%cy
     isc                           => Atm%isc
     iec                           => Atm%iec
     jsc                           => Atm%jsc
     jec                           => Atm%jec

     pelist                        => Atm%pelist
     ng                            => Atm%ng
     domain                        => Atm%domain
     domain_for_coupler            => Atm%domain_for_coupler
     num_contact                   => Atm%num_contact
     npes_per_tile                 => Atm%npes_per_tile
     tile                          => Atm%tile
     npes_this_grid                => Atm%npes_this_grid
     is                            => Atm%is
     ie                            => Atm%ie
     js                            => Atm%js
     je                            => Atm%je
     isd                           => Atm%isd
     ied                           => Atm%ied
     jsd                           => Atm%jsd
     jed                           => Atm%jed
     isc                           => Atm%isc
     iec                           => Atm%iec
     jsc                           => Atm%jsc
     jec                           => Atm%jec

     idiag                         => Atm%idiag
     Fv_restart                    => Atm%Fv_restart
     SST_restart                   => Atm%SST_restart
     Fv_tile_restart               => Atm%Fv_tile_restart
     Rsf_restart                   => Atm%Rsf_restart
     Mg_restart                    => Atm%Mg_restart
     Lnd_restart                   => Atm%Lnd_restart
     Tra_restart                   => Atm%Tra_restart

     grid_global                   => Atm%grid_global
     atmos_axes                     => Atm%atmos_axes
   end subroutine switch_current_grid_pointers

#endif

 end module fv_current_grid_mod
