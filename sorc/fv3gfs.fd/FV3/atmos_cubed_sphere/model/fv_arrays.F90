!***********************************************************************
!*                   GNU Lesser General Public License                 
!*
!* This file is part of the FV3 dynamical core.
!*
!* The FV3 dynamical core is free software: you can redistribute it 
!* and/or modify it under the terms of the
!* GNU Lesser General Public License as published by the
!* Free Software Foundation, either version 3 of the License, or 
!* (at your option) any later version.
!*
!* The FV3 dynamical core is distributed in the hope that it will be 
!* useful, but WITHOUT ANYWARRANTY; without even the implied warranty 
!* of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
!* See the GNU General Public License for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with the FV3 dynamical core.  
!* If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************
!>@brief The module 'fv_arrays' contains the 'fv_atmos_type' and associated
!! datatypes

module fv_arrays_mod
#include <fms_platform.h>
  use mpp_domains_mod,       only: domain2d
  use fms_io_mod,            only: restart_file_type
  use time_manager_mod,      only: time_type
  use horiz_interp_type_mod, only: horiz_interp_type
  use mpp_domains_mod,       only: nest_domain_type
  use mpp_mod,               only: mpp_broadcast
  use platform_mod,          only: r8_kind
  public

  integer, public, parameter :: R_GRID = r8_kind

  !Several 'auxiliary' structures are introduced here. These are for
  ! the internal use by certain modules, and although fv_atmos_type
  !  contains one of each of these structures all memory management
  !   is performed by the module in question.


  integer, parameter:: max_step = 1000
!--- MAY NEED TO TEST THIS
#ifdef OVERLOAD_R4
  real, parameter:: real_big = 1.e8    ! big enough to cause blowup if used
#else
  real, parameter:: real_big = 1.e30   ! big enough to cause blowup if used
#endif
  type fv_diag_type


 integer ::id_ps, id_slp, id_ua, id_va, id_pt, id_omga, id_vort,  &
           id_tm, id_pv, id_zsurf, id_oro, id_sgh, id_divg, id_w, &
           id_ke, id_te, id_zs, id_ze, id_mq, id_vorts, id_us, id_vs,    &
           id_tq, id_rh, id_c15, id_c25, id_c35, id_c45,          &
                         id_f15, id_f25, id_f35, id_f45, id_ctp,  &
           id_ppt, id_ts, id_tb, id_ctt, id_pmask, id_pmaskv2,    &
           id_delp, id_delz, id_zratio, id_ws, id_iw, id_lw,      &
           id_pfhy, id_pfnh,                                      &
           id_qn, id_qn200, id_qn500, id_qn850, id_qp, id_mdt,    &
           id_qdt, id_aam, id_amdt,                               &
           id_acly, id_acl, id_acl2,                              &
           id_dbz, id_maxdbz, id_basedbz, id_dbz4km, id_dbztop, id_dbz_m10C, &
           id_ctz, id_w1km, id_wmaxup, id_wmaxdn, id_cape, id_cin,id_diss

! Selected p-level fields from 3D variables:
 integer :: id_vort200, id_vort500, id_w500, id_w700
 integer :: id_vort850, id_w850, id_x850, id_srh25, &
            id_uh03, id_uh25, id_theta_e,  &
            id_w200, id_s200, id_sl12, id_sl13, id_w5km, id_rain5km, id_w2500m
 integer :: id_srh1, id_srh3, id_ustm, id_vstm
! NGGPS 31-level diag
 integer, allocatable :: id_u(:), id_v(:), id_t(:), id_h(:), id_q(:), id_omg(:)

 integer:: id_u_plev, id_v_plev, id_t_plev, id_h_plev, id_q_plev, id_omg_plev
! IPCC diag
 integer :: id_rh10,  id_rh50,  id_rh100, id_rh200,  id_rh250, id_rh300, &
            id_rh500, id_rh700, id_rh850, id_rh925,  id_rh1000
 integer :: id_dp10,  id_dp50,  id_dp100, id_dp200,  id_dp250, id_dp300, &
            id_dp500, id_dp700, id_dp850, id_dp925,  id_dp1000

 integer :: id_rh1000_cmip, id_rh925_cmip, id_rh850_cmip, id_rh700_cmip, id_rh500_cmip, &
            id_rh300_cmip,  id_rh250_cmip, id_rh100_cmip, id_rh50_cmip,  id_rh10_cmip

 integer :: id_hght
 integer :: id_u100m, id_v100m, id_w100m

     ! For initial conditions:
     integer ic_ps, ic_ua, ic_va, ic_ppt
     integer ic_sphum
     integer, allocatable :: id_tracer(:)
! ESM requested diagnostics  -  dry mass/volume mixing ratios
 integer, allocatable :: id_tracer_dmmr(:)
 integer, allocatable :: id_tracer_dvmr(:)
 real,    allocatable :: w_mr(:)

     real, allocatable :: phalf(:)
     real, allocatable :: zsurf(:,:)
     real, allocatable :: zxg(:,:)
     real, allocatable :: pt1(:)


     logical :: initialized = .false.
     real  sphum, liq_wat, ice_wat       ! GFDL physics
     real  rainwat, snowwat, graupel

     real :: efx(max_step), efx_sum, efx_nest(max_step), efx_sum_nest, mtq(max_step), mtq_sum
     integer :: steps

  end type fv_diag_type


!>@brief The type 'fv_grid_type' is made up of grid-dependent information from fv_grid_tools and fv_grid_utils.
!>@details It should not contain any user options (that goes in a different structure) nor data which
!! is altered outside of those two modules.
  type fv_grid_type
     real(kind=R_GRID), allocatable, dimension(:,:,:) :: grid_64, agrid_64
     real(kind=R_GRID), allocatable, dimension(:,:) :: area_64, area_c_64
     real(kind=R_GRID), allocatable, dimension(:,:) :: sina_64, cosa_64
     real(kind=R_GRID), allocatable, dimension(:,:) :: dx_64, dy_64
     real(kind=R_GRID), allocatable, dimension(:,:) :: dxc_64, dyc_64
     real(kind=R_GRID), allocatable, dimension(:,:) :: dxa_64, dya_64

     real, allocatable, dimension(:,:,:) :: grid, agrid
     real, allocatable, dimension(:,:) :: area, area_c
     real, allocatable, dimension(:,:) :: rarea, rarea_c     

     real, allocatable, dimension(:,:) :: sina, cosa
     real, allocatable, dimension(:,:,:) :: e1,e2
     real, allocatable, dimension(:,:) :: dx, dy
     real, allocatable, dimension(:,:) :: dxc, dyc
     real, allocatable, dimension(:,:) :: dxa, dya
     real, allocatable, dimension(:,:) :: rdx, rdy
     real, allocatable, dimension(:,:) :: rdxc, rdyc
     real, allocatable, dimension(:,:) :: rdxa, rdya

     ! Scalars:
     real(kind=R_GRID), allocatable :: edge_s(:)
     real(kind=R_GRID), allocatable :: edge_n(:)
     real(kind=R_GRID), allocatable :: edge_w(:)
     real(kind=R_GRID), allocatable :: edge_e(:)
     ! Vector:
     real(kind=R_GRID), allocatable :: edge_vect_s(:)
     real(kind=R_GRID), allocatable :: edge_vect_n(:)
     real(kind=R_GRID), allocatable :: edge_vect_w(:)
     real(kind=R_GRID), allocatable :: edge_vect_e(:)
     ! scalar:
     real(kind=R_GRID), allocatable :: ex_s(:)
     real(kind=R_GRID), allocatable :: ex_n(:)
     real(kind=R_GRID), allocatable :: ex_w(:)
     real(kind=R_GRID), allocatable :: ex_e(:)

     real, allocatable :: l2c_u(:,:), l2c_v(:,:)
     ! divergence Damping:
     real, allocatable :: divg_u(:,:), divg_v(:,:)    !
     ! del6 diffusion:
     real, allocatable :: del6_u(:,:), del6_v(:,:)    !
     ! Cubed_2_latlon:
     real, allocatable :: a11(:,:)
     real, allocatable :: a12(:,:)
     real, allocatable :: a21(:,:)
     real, allocatable :: a22(:,:)
     ! latlon_2_cubed:
     real, allocatable :: z11(:,:)
     real, allocatable :: z12(:,:)
     real, allocatable :: z21(:,:)
     real, allocatable :: z22(:,:)

!    real, allocatable :: w00(:,:)

     real, allocatable :: cosa_u(:,:)
     real, allocatable :: cosa_v(:,:)
     real, allocatable :: cosa_s(:,:)
     real, allocatable :: sina_u(:,:)
     real, allocatable :: sina_v(:,:)
     real, allocatable :: rsin_u(:,:)
     real, allocatable :: rsin_v(:,:)
     real, allocatable ::  rsina(:,:)
     real, allocatable ::  rsin2(:,:)
     real(kind=R_GRID), allocatable :: ee1(:,:,:)
     real(kind=R_GRID), allocatable :: ee2(:,:,:)
     real(kind=R_GRID), allocatable :: ec1(:,:,:)
     real(kind=R_GRID), allocatable :: ec2(:,:,:)
     real(kind=R_GRID), allocatable :: ew(:,:,:,:)
     real(kind=R_GRID), allocatable :: es(:,:,:,:)


     !- 3D Super grid to contain all geometrical factors --
     ! the 3rd dimension is 9
     real, allocatable :: sin_sg(:,:,:)
     real, allocatable :: cos_sg(:,:,:)
     !--------------------------------------------------

     ! Unit Normal vectors at cell edges:
     real(kind=R_GRID), allocatable :: en1(:,:,:)
     real(kind=R_GRID), allocatable :: en2(:,:,:)

     ! Extended Cubed cross-edge winds
     real, allocatable :: eww(:,:)
     real, allocatable :: ess(:,:)

     ! Unit vectors for lat-lon grid
     real(kind=R_GRID), allocatable :: vlon(:,:,:), vlat(:,:,:)
     real, allocatable :: fC(:,:), f0(:,:)

     integer, dimension(:,:,:), allocatable :: iinta, jinta, iintb, jintb
  
     !Scalar data
     
     integer :: npx_g, npy_g, ntiles_g ! global domain

     real(kind=R_GRID) :: global_area
     logical :: g_sum_initialized = .false. !< Not currently used but can be useful
     logical:: sw_corner, se_corner, ne_corner, nw_corner

     real(kind=R_GRID) :: da_min, da_max, da_min_c, da_max_c

     real  :: acapN, acapS
     real  :: globalarea  !< total Global Area
     
     logical :: latlon = .false.
     logical :: cubed_sphere = .false.
     logical :: have_south_pole = .false.
     logical :: have_north_pole = .false.
     logical :: stretched_grid = .false.

     logical :: square_domain = .false.

     integer, pointer :: grid_type !< Which type of grid to use. If 0, the equidistant gnomonic
                                   !< cubed-sphere will be used. If 4, a doubly-periodic
                                   !< f-plane cartesian grid will be used. If -1, the grid is read 
                                   !< from INPUT/grid_spec.nc. Values 2, 3, 5, 6, and 7 are not 
                                   !< supported and will likely not run. The default value is 0.

     logical, pointer :: nested   !< Whether this is a nested grid. .false. by default.


  end type fv_grid_type

  type fv_flags_type

     !! FOR EACH VARIABLE IN FV_FLAGS:
     !! 1. Must be defined here:
     !! 2. Must be broadcast in fv_atmos_data
     !! 3. If a namelist entry, a pointer must
     !!    be defined and associated in fv_control
     !! 4. Must NOT appear in fv_current_grid_mod.
     !!    (this module will soon be removed)
     !! 5. Must be referenced through Atm%flagstruct,
     !!    not Atm%, unless a convenience
     !!    pointer is defined

!-----------------------------------------------------------------------
! Grid descriptor file setup
!-----------------------------------------------------------------------
   character(len=80) :: grid_name = 'Gnomonic'
   character(len=120):: grid_file = 'Inline'
   integer      :: grid_type = 0     !< -1: read from file; 0: ED Gnomonic
                                     !<  0: the "true" equal-distance Gnomonic grid
                                     !<  1: the traditional equal-distance Gnomonic grid
                                     !<  2: the equal-angular Gnomonic grid
                                     !<  3: the lat-lon grid -- to be implemented
                                     !<  4: double periodic boundary condition on Cartesian grid
                                     !<  5: channel flow on Cartesian grid
!  -> moved to grid_tools

! Momentum (or KE) options:
   integer :: hord_mt = 9   !< Horizontal advection scheme for momentum fluxes. A
                            !< complete list of kord options is given in the 
                            !< corresponding table in Appendix A of the 
                            !< FV3 technical document. The default value is 9, which
                            !< uses the third-order piecewise-parabolic method with the
                            !< monotonicity constraint of Huynh, which is less diffusive 
                            !< than other constraints. For hydrostatic simulation, 8 
                            !< (the L04 monotonicity constraint) is recommended; for 
                            !< nonhydrostatic simulation, the completely unlimited (“linear” 
                            !< or non-monotone) PPM scheme is recommended. If no monotonicity 
                            !< constraint is applied, enabling the flux damping 
                            !< (do_vort_damp = .true.) is highly recommended to control grid-scale
                            !< noise. It is also recommended that hord_mt, hord_vt, hord_tm, and
                            !< hord_dp use the same value, to ensure consistent transport of all 
                            !< dynamical fields, unless a positivity constraint on mass advection
                            !< (hord_dp) is desired.

   integer :: kord_mt = 8   !< Vertical remapping scheme for the winds. 8 by default; 9 is recommended as 
                            !< the safest option, although 10, and 11 can also be useful. See 
                            !< corresponding table in Appendix A of the FV3
                            !< technical document for a complete list of kord options.

   integer :: kord_wz = 8   !< Vertical remapping scheme for vertical velocity in nonhydrostatic simulations. 
                            !< 8 by default; 9 recommended. It is also recommended to use the same value 
                            !< for 'kord_wz' as for 'kord_mt'.

   !> Vorticity & w transport options:
   integer :: hord_vt = 9   !< Horizontal advection scheme for absolute vorticity and for
                            !< vertical velocity in nonhydrostatic simulations. 9 by default.


! Heat & air mass (delp) transport options:
   integer :: hord_tm = 9   !< Horizontal advection scheme for potential temperature and
                            !< layer thickness in nonhydrostatic simulations. 9 by default.

   integer :: hord_dp = 9   !< Horizontal advection scheme for mass. A positivity 
                            !< constraint may be warranted for hord_dp but not strictly 
                            !< necessary. 9 by default.

   integer :: kord_tm = -8   !< Vertical remapping scheme for temperature. If positive
                             !< (not recommended), then vertical remapping is performed on 
                             !< total energy instead of temperature (see 'remap_t'). 
                             !< The default value is -8.
!> Tracer transport options:
   integer :: hord_tr = 12   !< Horizontal advection scheme for tracers. The default is 12.
                             !< This value can differ from the other hord options since
                             !< tracers are subcycled (if inline_q == .false.) and require 
                             !< positive-definite advection to control the appearance of 
                             !< non-physical negative masses. 8 (fastest) or 10 (least diffusive)
                             !< are typically recommended.

   integer :: kord_tr = 8   !< The vertical remapping scheme for tracers. The default is 8. 
                            !< 9 or 11 recommended. It is often recommended to use the same 
                            !< value for 'kord_tr' as for 'kord_tm'.
 
   real :: scale_z = 0.   !< diff_z = scale_z**2 * 0.25
   real :: w_max = 75.    !< max w (m/s) threshold for hydostatiic adjustment 
   real :: z_min = 0.05   !< min ratio of dz_nonhydrostatic/dz_hydrostatic
   real :: lim_fac = 1.0  !< linear scheme limiting factor, 1: hord = 5, 3: hord = 6

   integer :: nord = 1   !< Order of divergence damping: 0 for second-order; 1 for fourth-order 
                         !< (default); 2 for sixth-order; 3 for eighth-order. Sixth-order may
                         !< yield a better solution for low resolutions (one degree or coarser) 
                         !< by virtue of it being more scale-selective and will not damp moderately
                         !< well-resolved disturbances as much as does lower-order damping.

   integer :: nord_tr = 0   !< Order of tracer damping; values mean the same as for 'nord'. 
                            !< The default value is 0.

   real :: dddmp = 0.0   !< Dimensionless coefficient for the second-order Smagorinsky-type 
                         !< divergence damping. The default is value is 0.0. 0.2 
                         !< (the Smagorinsky constant) is recommended if ICs are noisy.

   real :: d2_bg = 0.0   !< Coefficient for background second-order divergence damping.
                         !< This option remains active even if nord is nonzero. The default
                         !< value is 0.0. The proper range is 0 to 0.02.

   real :: d4_bg = 0.16   !< Dimensionless coefficient for background higher-order divergence damping. 
                          !< 0.0 by default. If no second-order divergence damping is used, then values 
                          !< between 0.1 and 0.16 are recommended. Requires 'nord' > 0. Note that the 
                          !< scaling for 'd4_bg' differs from that of 'd2_bg'; 'nord' >= 1 and 
                          !< 'd4_bg' = 0.16 will be less diffusive than 'nord' = 0 and 'd2_bg' = 0.02.

   real :: vtdm4 = 0.0   !< Coefficient for background other-variable damping. The value of 'vtdm4' 
                         !< should be less than that of 'd4_bg'. A good first guess for 'vtdm4' is
                         !< about one-third the value of d4_bg. Requires 'do_vort_damp' 
                         !< to be .true. Disabled for values less than 1.e-3. Other-
                         !< variable damping should not be used if a monotonic horizontal advection
                         !< scheme is used. The default value is 0.0.

   real :: trdm2 = 0.0   !< Coefficient for del-2 tracer damping
   real :: d2_bg_k1 = 4.   !< Strength of second-order diffusion in the top sponge layer.
                           !< Value must be specified. This value, and d2_bg_k2, will be changed 
                           !< appropriately in the model (depending on the height of model 
                           !< top), so the actual damping may be very reduced. See 
                           !< atmos_cubed_sphere/model/dyncore.F90 for details. Recommended 
                           !< range is 0. to 0.2. Note that since diffusion is converted to 
                           !< heat if d_con > 0 larger amounts of sponge-layer diffusion may 
                           !< be less stable.

   real :: d2_bg_k2 = 2.   !< Strength of second-order diffusion in the second sponge
                           !< layer from the model top. This value must be specified, and 
                           !< should be less than 'd2_bg_k1'.

   real :: d2_divg_max_k1 = 0.15 !< d2_divg max value (k=1)
   real :: d2_divg_max_k2 = 0.08 !< d2_divg max value (k=2)
   real :: damp_k_k1 = 0.2       !< damp_k value (k=1)
   real :: damp_k_k2 = 0.12      !< damp_k value (k=2)

!> Additional (after the fact) terrain filter (to further smooth the terrain after cold start)
   integer :: n_zs_filter = 0   !< Number of times to apply a diffusive filter to the topography
                                !< upon startup, if mountain is True and the model is not being
                                !< cold-started. This is applied every time the model is warm-started,
                                !< so if you want to smooth the topography make sure this is set to 0 after
                                !< the first simulation. If initializing the model from cold-start
                                !< the topography is already being filtered by an amount appropriate for
                                !< the model resolution. 0 by default.

   integer :: nord_zs_filter = 4   !< Order of the topography filter applied to n_zs_filter.
                                   !< Set to 2 to get a second-order filter, or 4 to get a fourth-order filter;
                                   !< other values do no filtering. 0 by default. This should not be set to a
                                   !< non-zero value on multiple successive simulations; the filter is applied
                                   !< every time the model restarts. This option is useful for testing the
                                   !< terrain filter, and SHOULD NOT BE USED FOR REGULAR RUNS.  
                                   !< use del-2 (2) OR del-4 (4)

   logical :: full_zs_filter = .false.   !< Whether to apply the on-line topography filter during
                                         !< initialization. Only active if get_nggps_ic = .true. This is so 
                                         !< topography filtering can be performed on the initial conditions output by the
                                         !< pre-processing tools, which currently do not support topography filter-
                                         !< ing for some configurations (such as the nested grid); this also allows
                                         !< the user to easily test changes to the topography filtering on the
                                         !< simulation. Note that for all other initialization methods (if external_ic
                                         !< = .true.) the on-line topography filter will be applied automatically
                                         !< during the initialization of the topography. The default value is .false.

   logical :: RF_fast = .false.   !< Option controlling whether to apply Rayleigh damping (for tau > 0) 
                                  !< on the dynamic/acoustic timestep rather than on the physics timestep.
                                  !< This can help stabilize the model by applying the damping more weakly 
                                  !< more frequently, so the instantaneous amount of damping (and thereby 
                                  !< heat added) is reduced. The default is .false., which applies the Rayleigh
                                  !< drag every physics timestep.

   logical :: consv_am  = .false.   !< Whether to enable Angular momentum fixer. The default is .false.


   logical :: do_sat_adj= .false.   ! 
   logical :: do_f3d = .false.   ! 
   logical :: no_dycore = .false.   !< Disables execution of the dynamical core, only running
                                    !< the initialization, diagnostic, and I/O routines, and 
                                    !< any physics that may be enabled. Essentially turns the 
                                    !< model into a column physics model. The default is .false.

   logical :: convert_ke = .false.   !< If .true., adds energy dissipated through mechanical
                                     !< damping to heat throughout the entire depth of the domain; 
                                     !< if .false. (default) this is only done in the sponge layer
                                     !< at the top of the domain. This option is only enabled if
                                     !< d_con > 1.e-5.
 
   logical :: do_vort_damp = .false.   !< Whether to apply flux damping (of strength governed by 'vtdm4')
                                       !< to the fluxes of vorticity, air mass, and nonhydrostatic
                                       !< vertical velocity (there is no dynamically correct way to add 
                                       !< explicit diffusion to the tracer fluxes). The form is the same 
                                       !< as is used for the divergence damping, including the same order 
                                       !< (from 'nord') damping, unless 'nord' = 0, in which case this 
                                       !< damping is fourth-order, or if 'nord' = 3,in which case this 
                                       !< damping is sixth-order (instead of eighth-order). We recommend 
                                       !< enabling this damping when the linear or non-monotonic
                                       !< horizontal advection schemes are enabled, but is unnecessary and 
                                       !< not recommended when using monotonic advection. The default is .false.

   logical :: use_old_omega = .true. 
! PG off centering:
   real :: beta = 0.0   !< Parameter specifying fraction of time-off-centering for backwards
                        !< evaluation of the pressure gradient force. The default is 0.0, which 
                        !< produces a fully backwards evaluation of the pressure gradient force
                        !< that is entirely evaluated using the updated (time n+1) dynamical fields. 
                        !< A value of 0.5 will equally weight the PGF determined at times n and
                        !< n+1, but may not be stable; values larger than 0.45 are not recommended.
                        !< A value of 0.4 is recommended for most hydrostatic simulations, which 
                        !< allows an improved representation of inertia-gravity waves in the tropics. 
                        !< In non-hydrostatic simulations using the semi-implicit solver (a_imp > 0.5)
                        !< the values of 'a_imp' and 'beta' should add to 1, so that the time-centering is
                        !< consistent between the PGF and the nonhydrostatic solver. 
                        !< The proper range is 0 to 0.45.

#ifdef SW_DYNAMICS
   integer :: n_sponge = 0   !< Controls the number of layers at the upper boundary on
                             !< which the 2Dx filter is applied. This does not control the sponge layer.
                             !< The default value is 0.

   real :: d_ext = 0.   !< Coefficient for external (barotropic) mode damping. The 
                        !< default value is 0.02. The proper range is 0 to 0.02. A value 
                        !< of 0.01 or 0.02 may help improve the models maximum stable 
                        !< time step in low-resolution (2-degree or lower) simulations; 
                        !< otherwise a value of 0 is recommended.
   
   integer :: nwat  = 0   !< Number of water species to be included in condensate and
                          !< water vapor loading. The masses of the first nwat tracer species will be
                          !< added to the dry air mass, so that p is the mass of dry air, water vapor,
                          !< and the included condensate species. The value used depends on the
                          !< microphysics in the physics package you are using. For GFS physics
                          !< with only a single condensate species, set to 2. For schemes with
                          !< prognostic cloud water and cloud ice, such as GFDL AM2/AM3/AM4
                          !< Rotsteyn-Klein or Morrison-Gettlean microphysics, set to 3. For 
                          !< warm-rain (Kessler) microphysics set to 4 (with an inactive ice tracer),
                          !< which only handles three species but uses 4 to avoid interference with the
                          !< R-K physics. For schemes such as WSM5 or Ferrier that have prognostic rain 
                          !< and snow but not hail, set to 5 (not yet implemented). For six-category
                          !< schemes that also have prognostic hail or graupel, such as the GFDL, Thompson, 
                          !< or WSM6 microphysics, set to 6. A value of 0 turns off condensate loading.
                          !< The default value is 3.

   logical :: warm_start = .false.   !< Whether to start from restart files, instead of cold-starting
                                     !< the model. True by default; if this is set to .true. and restart
                                     !< files cannot be found the model will stop.

   logical :: inline_q = .true.    !< Whether to compute tracer transport in-line with the rest
                                   !< of the dynamics instead of sub-cycling, so that tracer transport is done
                                   !< at the same time and on the same time step as is p and potential
                                   !< temperature. False by default; if true, q_split and z_tracer are ignored.

   logical :: adiabatic = .true.   !< Whether to skip any physics. If true, the physics is not
                                   !< called at all and there is no virtual temperature effect. 
                                   !< False by default; this option has no effect if not running solo_core.
#else
   integer :: n_sponge = 1   !< Controls the number of layers at the upper boundary on which the 2Dx filter 
                             !< is applied. This does not control the sponge layer. The default value is 0.

   real    :: d_ext = 0.02   !< Coefficient for external (barotropic) mode damping. Proper range is 0 to 0.02. 
                             !< A value of 0.01 or 0.02 may help improve the models maximum stable time 
                             !< step in low-resolution (2-degree or lower) simulations; otherwise a 
                             !< value of 0 is recommended. The default value is 0.02.

   integer :: nwat  = 3   !< Number of water species to be included in condensate and
                          !< water vapor loading. The masses of the first nwat tracer species will be
                          !< added to the dry air mass, so that p is the mass of dry air, water vapor,
                          !< and the included condensate species. The value used depends on the
                          !< microphysics in the physics package you are using. For GFS physics
                          !< with only a single condensate species, set to 2. For schemes with
                          !< prognostic cloud water and cloud ice, such as GFDL AM2/AM3/AM4
                          !< Rotsteyn-Klein or Morrison-Gettlean microphysics, set to 3. For 
                          !< warm-rain (Kessler) microphysics set to 4 (with an inactive ice tracer),
                          !< which only handles three species but uses 4 to avoid interference with the
                          !< R-K physics. For schemes such as WSM5 or Ferrier that have prognostic rain 
                          !< and snow but not hail, set to 5 (not yet implemented). For six-category
                          !< schemes that also have prognostic hail or graupel, such as the GFDL, Thompson, 
                          !< or WSM6 microphysics, set to 6. A value of 0 turns off condensate loading.
                          !< The default value is 3.

   logical :: warm_start = .true.   !< Whether to start from restart files, instead of cold-starting
                                    !< the model. True by default; if this is set to .true. and restart
                                    !< files cannot be found the model will stop.

   logical :: inline_q = .false.   !< Whether to compute tracer transport in-line with the rest
                                   !< of the dynamics instead of sub-cycling, so that tracer transport is done
                                   !< at the same time and on the same time step as is p and potential
                                   !< temperature. False by default; if true, q_split and z_tracer are ignored.
   logical :: adiabatic = .false.     !< Run without physics (full or idealized).
#endif
!-----------------------------------------------------------
! Grid shifting, rotation, and the Schmidt transformation:
!-----------------------------------------------------------
   real :: shift_fac = 18.   !< Westward zonal rotation (or shift) of cubed-sphere grid from
                             !< its natural orientation with cube face centers at 0, 90, 180, and 270
                             !< degrees longitude. The shift, in degrees, is 180/shift_fac. This shift
                             !< does not move the poles. By default this is set to 18, shifting the grid
                             !< westward 180/18=10 degrees, so that the edges of the cube do not run
                             !< through the mountains of Japan; all standard CM2.x, AM3, CM3, and
                             !< HiRAM simulations use this orientation of the grid. 
                             !< Requires do_schmidt = .false.
 
! Defaults for Schmidt transformation:
   logical :: do_schmidt = .false.   !< Whether to enable grid stretching and rotation using
                                     !< stretch_fac, target_lat, and target_lon. 
                                     !< The default value is .false.
   real(kind=R_GRID) :: stretch_fac = 1.   !< Stretching factor for the Schmidt transformation. This
                                           !< is the factor by which tile 6 of the cubed sphere will 
                                           !< be shrunk, with the grid size shrinking accordingly. 
                                           !< The default value is 1, which performs no grid stretching. 
                                           !< Requires do_schmidt =.true. 
                                           !< THE MODEL WILL CRASH IF stretch_fac IS SET TO ZERO.
                                           !< Values of up to 40 have been found useful and stable 
                                           !< for short-term cloud-scale integrations.

   real(kind=R_GRID) :: target_lat = -90.   !< Latitude (in degrees) to which the center of tile 6 will be
                                            !< rotated; if stretching is done with stretch_fac the center of 
                                            !< the high-resolution part of the grid will be at this latitude. 
                                            !< -90 by default, which does no grid rotation (the Schmidt transformation
                                            !< rotates the south pole to the appropriate target).
                                            !< Requires do_schmidt = .true. 

   real(kind=R_GRID) :: target_lon = 0.   !< Longitude to which the center of tile 6 will be rotated. 
                                          !< 0 by default. Requires do_schmidt = .true.

   !-----------------------------------------------------------------------------------------------
   ! Example #1a: US regional climate simulation, center located over Oklahoma city: (262.4, 35.4)
   !              stretching factor: 2.5
   ! Example #1b: US Hurricane model, center over Miami: (279.7, 25.8)
   !              stretching factor: 3-5
   ! Example #2a: South-East Asia Regional Climate H*** (SERACH), Central Taiwan: (121.0, 23.5)
   ! Example #2b: Typhoon Model: (124.0, 22.0)
   !              stretching factor: 5-10
   !-----------------------------------------------------------------------------------------------

   logical :: reset_eta = .false. 
   real :: p_fac = 0.05   !< Safety factor for minimum nonhydrostatic pressures, which
                          !< will be limited so the full pressure is no less than p_fac 
                          !< times the hydrostatic pressure. This is only of concern in mid-top 
                          !< or high-top models with very low pressures near the model top, and 
                          !< has no effect in most simulations. The pressure limiting activates 
                          !< only when model is in danger of blowup due to unphysical negative 
                          !< total pressures. Only used if 'hydrostatic' = .false.and the 
                          !< semi-implicit solver is used. The proper range is 0 to 0.25. 
                          !< The default value is 0.05. 

   real :: a_imp = 0.75   !< Controls behavior of the non-hydrostatic solver. Values > 0.5
                          !< enable the semi-implicit solver, in which the value of 'a_imp' 
                          !< controls the time-off-centering: use a_imp = 1.0 for a fully 
                          !< backward time stepping. For consistency, the sum of 'beta' and 
                          !< 'a_imp' should be 1 when the semi-implicit solver is used. The 
                          !< semi-implicit algorithm is substantially more efficient except 
                          !< at very high (km-scale) resolutions with an acoustic time step 
                          !< of a few seconds or less. Proper values are 0, or between 0.5  
                          !< and 1. The default value is 0.75. Only used if 
                          !< 'hydrostatic' = .false.

   integer :: n_split = 0   !< The number of small dynamics (acoustic) time steps between
                            !< vertical remapping. 0 by default, in which case the model 
                            !< produces a good first guess by examining the resolution, 
                            !< dt_atmos, and k_split.

   integer :: m_split = 0    ! Number of time splits for Riemann solver
   integer :: k_split = 1   !< Number of vertical remappings per dt_atmos (physics timestep).
                            !< 1 by default.

   logical :: use_logp = .false.   !< Enables a variant of the Lin pressure-gradient force 
                                   !< algorithm, which uses the logarithm of pressure instead 
                                   !< of the Exner function (as in \cite lin1997explicit). This yields 
                                   !< more accurate results for regions that are nearly isothermal. 
                                   !< Ignored if 'hydrostatic' = .true. The default is .false.

!            For doubly periodic domain with sim_phys
!                     5km        150         20 (7.5 s)  2
!
!                     Estimates for Gnomonic grids:
            !===================================================
            !        dx (km)    dt (sc)    n_split    m_split
            !===================================================
            ! C1000:  ~10        150         16          3
            ! C2000:   ~5         90         18 (5 s)    2
            !===================================================
! The nonhydrostatic algorithm is described in Lin 2006, QJ, (submitted)
! C2000 should easily scale to at least 6 * 100 * 100 = 60,000 CPUs  
! For a 1024 system: try 6 x 13 * 13 = 1014 CPUs
  
   integer :: q_split = 0    !< number of time steps for sub-cycled tracer advection.
                             !< The default value is 0 (recommended), in which case 
                             !< the model determines the number of time steps from the
                             !< global maximum wind speed at each call to the tracer advection.

   integer :: print_freq = 0   !< number of hours between print out of max/min and
                               !< air/tracer mass diagnostics to standard output. 0 by default, which
                               !< never prints out any output; set to -1 to see output after every
                               !< dt_at-mos. Computing these diagnostics requires some computational overhead

   logical :: write_3d_diags = .true.   !< whether to write out three-dimensional dynamical diagnostic 
                                        !< fields (those defined in fv_diagnostics.F90). This is useful
                                        !< for runs with multiple grids if you only want very large 3D
                                        !< diagnostics written out for (say) a nested grid, and not for 
                                        !< the global grid. False by default.
!------------------------------------------
! Model Domain parameters
!------------------------------------------
   integer :: npx   !< Number of grid corners in the x-direction on one tile of the domain; 
                    !< so one more than the number of grid cells across a tile. On the cubed sphere
                    !< this is one more than the number of cells across a cube face. Must be set.
          
   integer :: npy   !< Number of grid corners in the y-direction on one tile of the
                    !< domain. This value should be identical to npx on a cubed-sphere grid;
                    !< doubly periodic or nested grids do not have this restriction. Must be set.

   integer :: npz   !< Number of vertical levels. Each choice of npz comes with a
                    !< pre-defined set of hybrid sigma-pressure levels and model top 
                    !< (see fv_eta.F90). Must be set.

   integer :: npz_rst = 0    !< If using a restart file with a different number of vertical
                             !< levels, set npz_rst to be the number of levels in your restart file.
                             !< The model will then remap the restart file data to the vertical coordinates
                             !< specified by npz. 0 by default; if 0 or equal to npz no remapping is done.              
                      
   integer :: ncnst = 0   !< Number of tracer species advected by fv_tracer in the dynamical core.  
                          !< Typically this is set automatically by reading in values from field_table, 
                          !< but ncnst can be set to a smaller value so only the first ncnst tracers 
                          !< listed in field_table are not advected. 0 by default, which will use the value 
                          !< from field_table.

   integer :: pnats = 0   !< The number of tracers not to advect by the dynamical core.
                          !< Unlike dnats, these tracers are not seen by the dynamical core. 
                          !< The last pnats entries in field_table are not advected. 
                          !< The default value is 0.
  
   integer :: dnats = 0   !< The number of tracers which are not to be advected by the dynamical core,
                          !< but still passed into the dynamical core; the last dnats+pnats tracers 
                          !< in field_table are not advected. 0 by default.

   integer :: ntiles = 1   !< Number of tiles on the domain. For the cubed sphere, this
                           !< should be 6, one tile for each face of the cubed sphere; normally for
                           !< most other domains (including nested grids) this should be set to 1.
                           !< Must be set.

   integer :: ndims = 2     ! Lat-Lon Dims for Grid in Radians
   integer :: nf_omega  = 1   !< Number of times to apply second-order smoothing to the
                              !< diagnosed omega. When 0 the filter is disabled. 1 by default.

   integer :: fv_sg_adj = -1   !< Timescale (in seconds) at which to remove two-delta-z
                               !< instability when the local (between two adjacent levels) 
                               !< Richardson number is less than 1. This is achieved by local 
                               !< mixing, which conserves mass, momentum, and total energy. 
                               !< Values of 0 or smaller disable this feature. If n_sponge < 0 
                               !< then the mixing is applied only to the top n_sponge layers of the 
                               !< domain. Set to -1 (inactive) by default. The proper range is 0 to 3600.

   integer :: na_init = 0   !< Number of forward-backward dynamics steps used to initialize
                            !< adiabatic solver. This is useful for spinning up the nonhydrostatic
                            !< state from the hydrostatic GFS analyses. 0 by default. Recommended
                            !< to set this to a non-zero value (1 or 2 is typically sufficient)
                            !< when initializing from GFS or ECMWF analyses.
   

   logical :: nudge_dz = .false.    !< During the adiabatic initialization (na_init > 0), if set
                                    !< to .true., delz is nudged back to the value specified in the initial
                                    !< conditions, instead of nudging the temperature back to the initial value.
                                    !< Nudging delz is simpler (faster), doesn’t require consideration of the
                                    !< virtual temperature effect, and may be more stable. .false.by default.

   real :: p_ref = 1.E5   !< Surface pressure used to construct a horizontally-uniform reference
                          !< vertical pressure profile, used in some simple physics packages
                          !< in the solo_core and in the Rayleigh damping. This should not be 
                          !< confused with the actual, horizontally-varying pressure levels used
                          !< for all other dynamical calculations. The default value is 1.e5. 
                          !< CHANGING THIS VALUE IS STRONGLY DISCOURAGED.

   real :: dry_mass = 98290.   !< If adjust_dry_mass is .true., sets the global dry air mass,
                               !< measured in the globally-averaged surface pressure (Pascals) by adding
                               !< or removing mass from the lowest layer of the atmosphere as needed.
                               !< The default value is 98290. (Pa).

   integer :: nt_prog = 0
   integer :: nt_phys = 0
   real :: tau_h2o = 0.   !< Time-scale (days) for simple methane chemistry to act as
                          !< a source of water in the stratosphere. Can be useful if the
                          !< stratosphere dries out too quickly; consider a value between 
                          !< 60 and 120 days if this is the case. The default value is 0., 
                          !< which disables the methane chemistry. Values less than zero apply 
                          !< the chemistry above 100 mb; else applied above 30 mb. 
                          !< Requires 'adiabatic' to be .false.


   real :: delt_max = 1.   !< Maximum allowed magnitude of the dissipative heating rate, K s−1; 
                           !< larger magnitudes are clipped to this amount. This can help avoid
                           !< instability that can occur due to strong heating when d_con > 0. 
                           !< A value of 0.008 (a rate equivalent to about 800 K/day) is
                           !< sufficient to stabilize the model at 3-km resolution. 
                           !< Set to 1. by default, which effectively disables this limitation.

   real :: d_con = 0.   !< Fraction of kinetic energy lost to explicit damping to be 
                        !< converted to heat. Acts as a dissipative heating mechanism in 
                        !< the dynamical core. The default is 0. Proper range is 0 to 1.
                        !< Note that this is a local, physically correct, energy fixer.

   real :: ke_bg = 0.   !< background KE production (m^2/s^3) over a small step
                        !< Use this to conserve total energy if consv_te=0

   real :: consv_te = 0.   !< Fraction of total energy lost during the adiabatic integration
                           !< between calls of the physics, to be added back globally as heat;
                           !< essentially the strength of the energy fixer in the physics.
                           !< Note that this is a global energy fixer and cannot add back energy 
                           !< locally. The default algorithm increments the potential temperature
                           !< so the pressure gradients are unchanged.  The default value is 0.
                           !< Proper range is 0 to 1. 1 will restore the energy completely to its
                           !< original value before entering the physics; a value of 0.7 roughly 
                           !< causes the energy fixer to compensate for the amount of energy changed
                           !< by the physics in GFDL HiRAM or AM3.
 
   real :: tau = 0.   !< Time scale (in days) for Rayleigh friction applied to horizontal
                      !< and vertical winds; lost kinetic energy is converted to heat, except
                      !< on nested grids. The default value is 0.0, which disables damping.
                      !< Larger values yield less damping. For models with tops at 1 mb or lower 
                      !< values between  10 and 30 are useful for preventing overly-strong polar night 
                      !< jets; for higher-top hydrostatic models values between 5 and 15 should be 
                      !< considered; and for non-hydrostatic models values of 10 or less should be  
                      !< considered, with smaller values for higher-resolution.

   real :: rf_cutoff = 30.E2   !< Pressure below which no Rayleigh damping is applied if tau > 0.

   logical :: filter_phys = .false.
   logical :: dwind_2d = .false.    !< Whether to use a simpler & faster algorithm for interpolating
                                    !< the A-grid (cell-centered) wind tendencies computed from the physics 
                                    !< to the D-grid. Typically, the A-grid wind tendencies are first
                                    !< converted in 3D cartesian coordinates and then interpolated before
                                    !< converting back to 2D local coordinates. When this option enabled,
                                    !< a much simpler but less accurate 2D interpolation is used. False by
                                    !< default.

   logical :: breed_vortex_inline = .false.   !< Whether to bogus tropical cyclones into the model, 
                                              !< which are specified by an external file. Options are set in 
                                              !< fv_nwp_nudge_nml. False by default.

   logical :: range_warn = .false.   !< Checks whether the values of the prognostic variables
                                     !< are within a reasonable range at the end of a dynamics time
                                     !< step, and prints a warning if not. The default is .false.;
                                     !< adds computational, overhead so we only recommend using
                                     !< this when debugging.

   logical :: fill = .false.   !< Fills in negative tracer values by taking positive tracers from
                               !< the cells above and below. This option is useful when the physical
                               !< parameterizations produced negatives. The default is .false.
   logical :: fill_dp = .false.   !< Like 'fill' except for p, the hydrostatic pressure thickness.
                                  !< When the filling occurs a diagnostic message is printed out, 
                                  !< which is helpful for diagnosing where the problem may be occurring.
                                  !< Typically, a crash is inevitable if the pressure filling is needed; 
                                  !< thus, this option is often better for debugging than as a safety valve.
                                  !< The default is .false.
 
   
   logical :: fill_wz = .false.
   logical :: check_negative = .false.   !< Whether to print the most negative global value of microphysical tracers.
   logical :: non_ortho = .true.
   logical :: moist_phys = .true.     !< Run with moist physics
   logical :: do_Held_Suarez = .false.   !< Whether to use Held-Suarez forcing. Requires adiabatic
                                         !< to be false. The default is .false.; this option has no 
                                         !< effect if not running solo_core.
   logical :: do_reed_physics = .false.
   logical :: reed_cond_only = .false.
   logical :: reproduce_sum = .true.   !< uses an exactly-reproducible global sum operation performed 
                                       !< when computing the global energy for consv_te. This is used 
                                       !< because the FMS routine mpp_sum() is not bit-wise reproducible
                                       !< due to its handling of floating-point arithmetic, and so can 
                                       !< return different answers for (say) different processor layouts. 
                                       !< The default is .true.

   logical :: adjust_dry_mass = .false.    !< Whether to adjust the global dry-air mass to the
                                           !< value set by dry_mass. This is only done in an initialization step, 
                                           !< particularly when using an initial condition from an external dataset, 
                                           !< interpolated from another resolution (either horizontal or vertical), or
                                           !< when changing the topography, so that the global mass of the atmosphere
                                           !< matches some estimate of observed value. False by default. It
                                           !< is recommended to only set this to .true. when initializing the model.
 
   logical :: fv_debug = .false.   !< Whether to turn on additional diagnostics in fv_dynamics. 
                                   !< The default is .false.
   logical :: srf_init = .false.
   logical :: mountain = .true.   !< Takes topography into account when initializing the
                                  !< model. Set this to .true. to apply the terrain filter (if n_zs_filter = 2
                                  !< or 4) upon startup; also set to True when cold starting so that the
                                  !< topography can be initialized. Only set this to .false. if you wish to
                                  !< cold-start without any topography; this value is ignored for the aquaplanet
                                  !< test_case = 14. The default is .true. It is highly recommended TO NOT ALTER
                                  !< this value unless you know what you are doing.
   logical :: old_divg_damp = .false. !< parameter to revert damping parameters back to values
                                      !< defined in a previous revision
                                      !< old_values:
                                      !<    d2_bg_k1 = 6.           d2_bg_k2 = 4.
                                      !<    d2_divg_max_k1 = 0.02   d2_divg_max_k2 = 0.01
                                      !<    damp_k_k1 = 0.          damp_k_k2 = 0.
                                      !< current_values:
                                      !<    d2_bg_k1 = 4.           d2_bg_k2 = 2.
                                      !<    d2_divg_max_k1 = 0.15   d2_divg_max_k2 = 0.08
                                      !<    damp_k_k1 = 0.2         damp_k_k2 = 0.12

   logical :: remap_t = .true.   !< Whether the vertical remapping is performed on (virtual) temperature 
                                 !< instead of (virtual) potential temperature. Since typically potential 
                                 !< temperature increases exponentially from layer to layer near the top 
                                 !< boundary, the cubic-spline interpolation in the vertical remapping 
                                 !< will have difficulty with the exponential profile. Temperature
                                 !< does not have this problem and will often yield a more accurate result.
                                 !< The default is .true.

   logical :: z_tracer = .false.   !< Whether to transport sub-cycled tracers layer-by-layer,
                                   !< each with its own computed sub-cycling time step (if q_split = 0).
                                   !< This may improve efficiency for very large numbers of tracers. 
                                   !< The default value is .false.; currently not implemented.

   logical :: fv_land = .false.   !< Whether to create terrain deviation and land fraction for
                                  !< output to mg_drag restart files, for use in mg_drag and in the land
                                  !< model. The default is .false; .true. is recommended when, and only
                                  !< when, initializing the model, since the mg_drag files created provide a
                                  !< much more accurate terrain representation for the mountain gravity
                                  !< wave drag parameterization and for the land surface roughness than
                                  !< either computes internally. This has no effect on the representation of
                                  !< the terrain in the dynamics.
!--------------------------------------------------------------------------------------
! The following options are useful for NWP experiments using datasets on the lat-lon grid
!--------------------------------------------------------------------------------------
   logical :: nudge = .false.   !< Whether to use the nudging towards the state in some externally-supplied 
                                !< file (such as from reanalysis or another simulation). Further
                                !< nudging options are set in fv_nwp_nudge_nml. The default is .false.

   logical :: nudge_ic = .false.   !< Same as nudge, but works in adiabatic solo_core simulations to 
                                   !< nudge the field to a single external analysis file. 
                                   !< The default is .false.

   logical :: ncep_ic = .false.   !< If external_ic = .true., this variable says whether the
                                  !< file in res_latlon_dynamics is an NCEP analysis or reanalysis file. 
                                  !< This option zeros out all tracer fields except specific humidity.
                                  !< The default is .false.

   logical :: nggps_ic = .false.   !< If external_ic = .true., reads initial conditions from
                                   !< horizontally-interpolated output from chgres. The default is .false.
                                   !< Additional options are available through external_ic_nml.

   logical :: ecmwf_ic = .false.   !< If external_ic = .true., reads initial conditions from ECMWF analyses.
                                   !< The default is .false. 

   logical :: gfs_phil = .false.   !< if .T., compute geopotential inside of GFS physics

   logical :: agrid_vel_rst = .false.   !< Whether to write the unstaggered latitude-longitude winds 
                                        !< (ua and va) to the restart files. This is useful for data 
                                        !< assimilation cycling systems which do not handle staggered winds. 
                                        !< The default is .false.

   logical :: use_new_ncep = .false.  ! use the NCEP ICs created after 2014/10/22, if want to read CWAT
   logical :: use_ncep_phy = .false.  ! if .T., separate CWAT by weights of liq_wat and liq_ice in FV_IC
   logical :: fv_diag_ic = .false.    ! reconstruct IC from fv_diagnostics on lat-lon grid

   logical :: external_ic = .false.   !< Whether to initialize the models state using the data
                                      !< in an externally specified file, given in res_latlon_dynamics.
                                      !< By default this file is assumed to be a legacy lat-lon FV core restart file;
                                      !< set either ncep_ic or fv_diag_ic to .true.to override this behavior.
                                      !< The default is .false. Note that external_ic = .true. will cause the
                                      !< model to re-initialize the dynamical fields from the input dataset
                                      !< regardless of whether warm_start is set.

   logical :: external_eta = .false.   !< If .true., reads the interface coefficients ak and bk
                                       !< from either the restart file (if restarting) or from the external initial
                                       !< condition file (if nggps_ic or ecwmf_ic are .true.). This overrides the
                                       !< hard-coded levels in fv_eta. The default is .false.

   logical :: read_increment = .false.   !< read in analysis increment and add to restart
! following are namelist parameters for Stochastic Energy Baskscatter dissipation estimate
   logical :: do_skeb  = .false.         !< save dissipation estimate
   integer :: skeb_npass  = 11           !< Filter dissipation estimate "skeb_npass" times
! Default restart files from the "Memphis" latlon FV core:
   character(len=128) :: res_latlon_dynamics = 'INPUT/fv_rst.res.nc'   !< If external_ic =.true.gives the filename of the 
                                                                       !< input IC file. The default is 'INPUT/fv_rst.res.nc'.
   character(len=128) :: res_latlon_tracers = 'INPUT/atmos_tracers.res.nc'   !< If external_ic =.true.and both ncep_ic and fv_diag_ic
                                                                              !< are.false., this variable gives the filename of the
                                                                              !< initial conditions for the tracers, assumed to be a 
                                                                              !< legacy lat-lon FV core restart file. 
                                                                              !< The default is 'INPUT/atmos_tracers.res.nc'.
! The user also needs to copy the "cold start" cubed sphere restart files (fv_core.res.tile1-6)
! to the INPUT dir during runtime
!------------------------------------------------
! Parameters related to non-hydrostatic dynamics:
!------------------------------------------------
   logical :: hydrostatic = .true.   !< Whether to use the hydrostatic or nonhydrostatic solver.
                                     !< The default is .true.

   logical :: phys_hydrostatic = .true.   !< Option to enable hydrostatic application of heating from the physics 
                                          !< in a nonhydrostatic simulation: heating is applied in hydrostatic 
                                          !< balance, causing the entire atmospheric column to expand instantaneously.
                                          !< If .false., heating from the physics is applied simply as a temperature
                                          !< tendency. The default value is .true.; ignored if hydrostatic = .true.

   logical :: use_hydro_pressure = .false.   !< Whether to compute hydrostatic pressure for input to the physics. 
                                             !< Currently only enabled for the fvGFS model.
                                             !< Ignored in hydrostatic simulations. The default is .false.

   logical :: do_uni_zfull = .false.   !< Whether to compute z_full (the height of each modellayer, 
                                       !< as opposed to z_half, the height of each model interface)
                                       !< as the midpoint of the layer, as is done for the nonhydrostatic 
                                       !< solver, instead of the height of the location where p = p the mean
                                       !< pressure in the layer. This option is not available for fvGFS or 
                                       !< the solo_core. The default is .false.

   logical :: hybrid_z = .false.   !< Whether to use a hybrid-height coordinate, instead of
                                   !< the usual sigma-p coordinate. The default value is .false. 
                                   !< (Not currently maintained.)
  
   logical :: Make_NH = .false.   !< Whether to re-initialize the nonhydrostatic state, by recomputing
                                  !< dz from hydrostatic balance and setting w to 0. The default is 
                                  !< false.

   logical :: make_hybrid_z = .false.   !< Converts the vertical coordinate to a hybrid-height coordinate, 
                                        !< instead of the usual sigma-p coordinate. Requires hybrid_z = .true. 
                                        !< The default value is .false.

   logical :: nudge_qv = .false.   !< During the adiabatic initialization (na_init > 0), if set to .true.,
                                   !< the water vapor profile is nudged to an analytic fit to the
                                   !< HALOE climatology. This is to improve the water vapor concentrations
                                   !< in GFS initial conditions, especially in the stratosphere, where
                                   !< values can be several times higher than observed. This nudging is
                                   !< unnecessary for other ICs, especially the ECMWF initial conditions.
                                   !< The default is .false.

   real :: add_noise = -1.   !< Amplitude of random thermal noise (in K) to add upon startup. 
                             !< Useful for perturbing initial conditions. -1 by default;
                             !< disabled if 0 or negative.

   integer :: a2b_ord = 4   !< Order of interpolation used by the pressure gradient force
                            !< to interpolate cell-centered (A-grid) values to the grid corners. 
                            !< The default value is 4 (recommended), which uses fourth-order 
                            !< interpolation; otherwise second-order interpolation is used.

   integer :: c2l_ord = 4   !< Order of interpolation from the solvers native D-grid winds
                            !< to latitude-longitude A-grid winds, which are used as input to
                            !< the physics routines and for writing to history files. 
                            !< The default value is 4 (recommended); fourth-order interpolation
                            !< is used unless c2l_ord = 2.


   real(kind=R_GRID) :: dx_const = 1000.   !< Specifies the (uniform) grid-cell-width in the x-direction
                                           !< on a doubly-periodic grid (grid_type = 4) in meters. 
                                           !< The default value is 1000.

   real(kind=R_GRID) :: dy_const = 1000.   !< Specifies the (uniform) grid-cell-width in the y-direction
                                           !< on a doubly-periodic grid (grid_type = 4) in meters. 
                                           !< The default value is 1000.

   real(kind=R_GRID) :: deglat = 15.   !< Latitude (in degrees) used to compute the uniform f-plane
                                       !< Coriolis parameter for doubly-periodic simulations
                                       !< (grid_type = 4). The default value is 15.

   !The following deglat_*, deglon_* options are not used.
   real(kind=R_GRID) :: deglon_start = -30., deglon_stop = 30., &  !< boundaries of latlon patch
                        deglat_start = -30., deglat_stop = 30.

   !>Convenience pointers
   integer, pointer :: grid_number

   !f1p
   logical  :: adj_mass_vmr = .false. !TER: This is to reproduce answers for verona patch.  This default can be changed
                                     !     to .true. in the next city release if desired
  
  !integer, pointer :: test_case
  !real,    pointer :: alpha

  end type fv_flags_type

  type fv_nest_BC_type_3D

     !!! CLEANUP: could we have pointers to np[xyz], nest_domain, and the index/weight arrays?

     real, allocatable, dimension(:,:,:) :: west_t1, east_t1, south_t1, north_t1
     real, allocatable, dimension(:,:,:) :: west_t0, east_t0, south_t0, north_t0

     integer :: istag, jstag

     logical :: allocated = .false.
     logical :: initialized = .false.

  end type fv_nest_BC_type_3D

  type fv_nest_BC_type_4D

     real, allocatable, dimension(:,:,:,:) :: west_t1, east_t1, south_t1, north_t1
     real, allocatable, dimension(:,:,:,:) :: west_t0, east_t0, south_t0, north_t0

     integer :: istag, jstag

     logical :: allocated = .false.
     logical :: initialized = .false.

  end type fv_nest_BC_type_4D

  type fv_nest_type

!nested grid flags:

     integer :: refinement = 3   !< Refinement ratio of the nested grid. This is the number
                                 !< of times that each coarse-grid cell face will be divided 
                                 !< into smaller segments on the nested grid. Required to be a 
                                 !< positive integer if nested = true. Nested grids are aligned 
                                 !< with the coarse grid, so non-integer refinements are not 
                                 !< permitted. The default value is 3.


     integer :: parent_tile = 1   !< Number of the tile (ie. face) in which this nested grid
                                  !< is found in its parent. Required to be a positive value if nested = true.
                                  !< If the parent grid is not a cubed sphere, or itself is a nested grid, this
                                  !< should be set to 1. If the parent grid has been rotated (using do_schmidt) with
                                  !< the intent of centering the nested grid at target_lat and target_lon, then 
                                  !< parent_tile should be set to 6. The default value is 1.

     logical :: nested = .false.   !< Whether this is a nested grid. The default value is .false.
                                         
     integer :: nestbctype = 1
     integer :: nsponge = 0
     integer :: nestupdate = 0   !< Type of nested-grid update to use; details are given in
                                 !< model/fv_nesting.F90.  The default is 0.
       
     logical :: twowaynest = .false.   !< Whether to use two-way nesting, the process by which
                                       !< the nested-grid solution can feed back onto the 
                                       !< coarse-grid solution. The default value is .false.
     integer :: ioffset, joffset !<Position of nest within parent grid

     integer :: nest_timestep = 0 !<Counter for nested-grid timesteps
     integer :: tracer_nest_timestep = 0 !<Counter for nested-grid timesteps
     real    :: s_weight = 1.e-6 !<sponge weight
     logical :: first_step = .true.
     integer :: refinement_of_global = 1
     integer :: npx_global
     integer :: upoff = 1 !< currently the same for all variables
     integer :: isu = -999, ieu = -1000, jsu = -999, jeu = -1000 !< limits of update regions on coarse grid 

     type(nest_domain_type) :: nest_domain !<Structure holding link from this grid to its parent
     type(nest_domain_type), allocatable :: nest_domain_all(:)

     !Interpolation arrays for grid nesting
     integer, allocatable, dimension(:,:,:) :: ind_h, ind_u, ind_v, ind_b
     real, allocatable, dimension(:,:,:) :: wt_h, wt_u, wt_v, wt_b
     integer, allocatable, dimension(:,:,:) :: ind_update_h

     !These arrays are not allocated by allocate_fv_atmos_type; but instead
     !allocated for all grids, regardless of whether the grid is
     !on a PE of a concurrent run.
     logical, allocatable, dimension(:) :: child_grids

     logical :: parent_proc, child_proc
     logical :: parent_of_twoway = .false.
   
     !These are for time-extrapolated BCs
     type(fv_nest_BC_type_3D) :: delp_BC, u_BC, v_BC, uc_BC, vc_BC, divg_BC
     type(fv_nest_BC_type_3D), allocatable, dimension(:) :: q_BC
#ifndef SW_DYNAMICS
     type(fv_nest_BC_type_3D) :: pt_BC, w_BC, delz_BC
#ifdef USE_COND
     type(fv_nest_BC_type_3D) :: q_con_BC
#ifdef MOIST_CAPPA
     type(fv_nest_BC_type_3D) :: cappa_BC
#endif
#endif
#endif

     !These are for tracer flux BCs
     logical :: do_flux_BCs, do_2way_flux_BCs !<For a parent grid; determine whether there is a need to send BCs
     type(restart_file_type) :: BCfile_ne, BCfile_sw

  end type fv_nest_type

!>@brief 'allocate_fv_nest_BC_type' is an interface to subroutines
!! that allocate the 'fv_nest_BC_type' structure that holds the nested-grid BCs.
!>@details The subroutines can pass the array bounds explicitly or not.
!! The bounds in Atm%bd are used for the non-explicit case.
  interface allocate_fv_nest_BC_type
     module procedure allocate_fv_nest_BC_type_3D
     module procedure allocate_fv_nest_BC_type_3D_Atm
  end interface

!>@brief 'deallocate_fv_nest_BC_type' is an interface to a subroutine
!! that deallocates the 'fv_nest_BC_type' structure that holds the nested-grid BCs.
  interface deallocate_fv_nest_BC_type
     module procedure deallocate_fv_nest_BC_type_3D
  end interface

  type fv_grid_bounds_type

     integer :: is,  ie,  js,  je
     integer :: isd, ied, jsd, jed
     integer :: isc, iec, jsc, jec

     integer :: ng

  end type fv_grid_bounds_type

  type fv_atmos_type

     logical :: allocated = .false.
     logical :: dummy = .false. ! same as grids_on_this_pe(n)
     integer :: grid_number = 1

     !Timestep-related variables.

     type(time_type) :: Time_init, Time, Run_length, Time_end, Time_step_atmos

     logical :: grid_active = .true. !Always active for now

     !This is kept here instead of in neststruct% simply for convenience
     type(fv_atmos_type), pointer :: parent_grid _NULL

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
    real, _ALLOCATABLE :: u(:,:,:)    _NULL  !< D grid zonal wind (m/s)
    real, _ALLOCATABLE :: v(:,:,:)    _NULL  !< D grid meridional wind (m/s)
    real, _ALLOCATABLE :: pt(:,:,:)   _NULL  !< temperature (K)
    real, _ALLOCATABLE :: delp(:,:,:) _NULL  !< pressure thickness (pascal)
    real, _ALLOCATABLE :: q(:,:,:,:)  _NULL  !< specific humidity and prognostic constituents
    real, _ALLOCATABLE :: qdiag(:,:,:,:)  _NULL  !< diagnostic tracers

!----------------------
! non-hydrostatic state:
!----------------------------------------------------------------------
    real, _ALLOCATABLE ::     w(:,:,:)  _NULL  !< cell center vertical wind (m/s)
    real, _ALLOCATABLE ::  delz(:,:,:)  _NULL  !< layer thickness (meters)
    real, _ALLOCATABLE ::   ze0(:,:,:)  _NULL  !< height at layer edges for remapping
    real, _ALLOCATABLE ::  q_con(:,:,:) _NULL  !< total condensates

!-----------------------------------------------------------------------
! Auxilliary pressure arrays:
! The 5 vars below can be re-computed from delp and ptop.
!-----------------------------------------------------------------------
! dyn_aux:
    real, _ALLOCATABLE :: ps (:,:)      _NULL  !< Surface pressure (pascal)
    real, _ALLOCATABLE :: pe (:,:,: )   _NULL  !< edge pressure (pascal)
    real, _ALLOCATABLE :: pk  (:,:,:)   _NULL  !< pe**cappa
    real, _ALLOCATABLE :: peln(:,:,:)   _NULL  !< ln(pe)
    real, _ALLOCATABLE :: pkz (:,:,:)   _NULL  !< finite-volume mean pk

! For phys coupling:
    real, _ALLOCATABLE :: u_srf(:,:)    _NULL  !< Surface u-wind
    real, _ALLOCATABLE :: v_srf(:,:)    _NULL  !< Surface v-wind
    real, _ALLOCATABLE :: sgh(:,:)      _NULL  !< Terrain standard deviation
    real, _ALLOCATABLE :: oro(:,:)      _NULL  !< land fraction (1: all land; 0: all water)
    real, _ALLOCATABLE :: ts(:,:)       _NULL  !< skin temperature (sst) from NCEP/GFS (K) -- tile
! For stochastic kinetic energy backscatter (SKEB)
    real, _ALLOCATABLE :: diss_est(:,:,:) _NULL !< dissipation estimate taken from 'heat_source'
 
!-----------------------------------------------------------------------
! Others:
!-----------------------------------------------------------------------
    real, _ALLOCATABLE :: phis(:,:)     _NULL  !< Surface geopotential (g*Z_surf)
    real, _ALLOCATABLE :: omga(:,:,:)   _NULL  !< Vertical pressure velocity (pa/s)
    real, _ALLOCATABLE :: ua(:,:,:)     _NULL  !< (ua, va) are mostly used as the A grid winds
    real, _ALLOCATABLE :: va(:,:,:)     _NULL
    real, _ALLOCATABLE :: uc(:,:,:)     _NULL  !< (uc, vc) are mostly used as the C grid winds
    real, _ALLOCATABLE :: vc(:,:,:)     _NULL

    real, _ALLOCATABLE :: ak(:)  _NULL
    real, _ALLOCATABLE :: bk(:)  _NULL

   integer :: ks

! Accumulated Mass flux arrays
    real, _ALLOCATABLE ::  mfx(:,:,:)  _NULL
    real, _ALLOCATABLE ::  mfy(:,:,:)  _NULL
! Accumulated Courant number arrays
    real, _ALLOCATABLE ::  cx(:,:,:)  _NULL
    real, _ALLOCATABLE ::  cy(:,:,:)  _NULL

    type(fv_flags_type) :: flagstruct
    
    !! Convenience pointers
    integer, pointer :: npx, npy, npz, ncnst, ng

    integer, allocatable, dimension(:) :: pelist

    type(fv_grid_bounds_type) :: bd

    type(domain2D) :: domain
#if defined(SPMD)

    type(domain2D) :: domain_for_coupler !< domain used in coupled model with halo = 1.

     integer :: num_contact, npes_per_tile, tile, npes_this_grid
     integer :: layout(2), io_layout(2) = (/ 1,1 /)   !< layout: Processor layout on each tile. 
                                                      !< The number of PEs assigned to a domain must equal 
                                                      !< layout(1)*layout(2)*ntiles. Must be set.
                                                      !< io_layout: Layout of output files on each tile. 1,1 by default,
                                                      !< which combines all restart and history files on a tile into one file.
                                                      !< For 0,0, every process writes out its own restart and history files.
                                                      !< If not equal to 1,1, you will have to use mppnccombine to combine these 
                                                      !< output files prior to post-processing, or if you want to change the 
                                                      !< number of PEs. Both entries must divide the respective value in layout.

#endif
     !These do not actually belong to the grid, but to the process
     !integer :: masterproc
     !integer :: gid 

!!!!!!!!!!!!!!!!
! From fv_grid_tools
!!!!!!!!!!!!!!!!


     real    :: ptop

  type(fv_grid_type) :: gridstruct
  

!!!!!!!!!!!!!!!!
!fv_diagnostics!
!!!!!!!!!!!!!!!!

     type(fv_diag_type) :: idiag

!!!!!!!!!!!!!!
! From fv_io !
!!!!!!!!!!!!!!
     type(restart_file_type) :: Fv_restart, SST_restart, Fv_tile_restart, &
          Rsf_restart, Mg_restart, Lnd_restart, Tra_restart

     type(fv_nest_type) :: neststruct

     !Hold on to coarse-grid global grid, so we don't have to waste processor time getting it again when starting to do grid nesting
     real(kind=R_GRID), allocatable, dimension(:,:,:,:) :: grid_global

  integer :: atmos_axes(4)


  end type fv_atmos_type

contains

!>@brief The subroutine 'allocate_fv_atmos_type' allocates the fv_atmos_type
!>@details It includes an option to define dummy grids that have scalar and 
!! small arrays defined as null 3D arrays.
  subroutine allocate_fv_atmos_type(Atm, isd_in, ied_in, jsd_in, jed_in, is_in, ie_in, js_in, je_in, &
       npx_in, npy_in, npz_in, ndims_in, ncnst_in, nq_in, ng_in, dummy, alloc_2d, ngrids_in)

    !WARNING: Before calling this routine, be sure to have set up the
    ! proper domain parameters from the namelists (as is done in
    ! fv_control.F90)

    implicit none
    type(fv_atmos_type), intent(INOUT), target :: Atm
    integer, intent(IN) :: isd_in, ied_in, jsd_in, jed_in, is_in, ie_in, js_in, je_in
    integer, intent(IN) :: npx_in, npy_in, npz_in, ndims_in, ncnst_in, nq_in, ng_in
    logical, intent(IN) :: dummy, alloc_2d
    integer, intent(IN) :: ngrids_in
    integer:: isd, ied, jsd, jed, is, ie, js, je
    integer:: npx, npy, npz, ndims, ncnst, nq, ng

    !For 2D utility arrays
    integer:: isd_2d, ied_2d, jsd_2d, jed_2d, is_2d, ie_2d, js_2d, je_2d
    integer:: npx_2d, npy_2d, npz_2d, ndims_2d, ncnst_2d, nq_2d, ng_2d

    integer :: i,j,k, ns, n

    if (Atm%allocated) return

    if (dummy) then
       isd     =  0   
       ied=   -1   
       jsd=   0   
       jed=   -1   
       is=   0    
       ie=   -1    
       js=   0    
       je=   -1    
       npx=   1   
       npy=   1   
       npz=   1   
       ndims=   1 
       ncnst=   1 
       nq=   1
       ng     =   1   
    else
       isd     =  isd_in   
       ied=   ied_in   
       jsd=   jsd_in   
       jed=   jed_in   
       is=   is_in    
       ie=   ie_in    
       js=   js_in    
       je=   je_in    
       npx=   npx_in   
       npy=   npy_in   
       npz=   npz_in   
       ndims=   ndims_in 
       ncnst=   ncnst_in 
       nq=   nq_in
       ng     =   ng_in    
    endif

    if ((.not. dummy) .or. alloc_2d) then
       isd_2d     =  isd_in   
       ied_2d=   ied_in   
       jsd_2d=   jsd_in   
       jed_2d=   jed_in   
       is_2d=   is_in    
       ie_2d=   ie_in    
       js_2d=   js_in    
       je_2d=   je_in    
       npx_2d=   npx_in   
       npy_2d=   npy_in   
       npz_2d=   npz_in   
       ndims_2d=   ndims_in 
       ncnst_2d=   ncnst_in 
       nq_2d=   nq_in 
       ng_2d     =   ng_in 
    else
       isd_2d     =  0   
       ied_2d=   -1   
       jsd_2d=   0   
       jed_2d=   -1   
       is_2d=   0    
       ie_2d=   -1    
       js_2d=   0    
       je_2d=   -1    
       npx_2d=   1   
       npy_2d=   1   
       npz_2d=   npz_in !for ak, bk, which are 1D arrays and thus OK to allocate
       ndims_2d=   1 
       ncnst_2d=   1 
       nq_2d=   1 
       ng_2d     =   1        
    endif

!This should be set up in fv_mp_mod
!!$    Atm%bd%isd = isd_in
!!$    Atm%bd%ied = ied_in
!!$    Atm%bd%jsd = jsd_in
!!$    Atm%bd%jed = jed_in
!!$
!!$    Atm%bd%is = is_in
!!$    Atm%bd%ie = ie_in
!!$    Atm%bd%js = js_in
!!$    Atm%bd%je = je_in
!!$
!!$    Atm%bd%isc = Atm%bd%is
!!$    Atm%bd%iec = Atm%bd%ie
!!$    Atm%bd%jsc = Atm%bd%js
!!$    Atm%bd%jec = Atm%bd%je

    Atm%bd%ng  = ng

    !Convenience pointers
    Atm%npx => Atm%flagstruct%npx
    Atm%npy => Atm%flagstruct%npy
    Atm%npz => Atm%flagstruct%npz
    Atm%ncnst => Atm%flagstruct%ncnst

    Atm%ng => Atm%bd%ng

!!$    Atm%npx = npx_in
!!$    Atm%npy = npy_in
!!$    Atm%npz = npz_in
    Atm%flagstruct%ndims = ndims_in

    allocate (    Atm%u(isd:ied  ,jsd:jed+1,npz) )
    allocate (    Atm%v(isd:ied+1,jsd:jed  ,npz) )

    allocate (   Atm%pt(isd:ied  ,jsd:jed  ,npz) )
    allocate ( Atm%delp(isd:ied  ,jsd:jed  ,npz) )
    allocate (    Atm%q(isd:ied  ,jsd:jed  ,npz, nq) )
    allocate (Atm%qdiag(isd:ied  ,jsd:jed  ,npz, nq+1:ncnst) )

    ! Allocate Auxilliary pressure arrays
    allocate (   Atm%ps(isd:ied  ,jsd:jed) )
    allocate (   Atm%pe(is-1:ie+1, npz+1,js-1:je+1) )
    allocate (   Atm%pk(is:ie    ,js:je  , npz+1) )
    allocate ( Atm%peln(is:ie,npz+1,js:je) )
    allocate (  Atm%pkz(is:ie,js:je,npz) )

    allocate ( Atm%u_srf(is:ie,js:je) )
    allocate ( Atm%v_srf(is:ie,js:je) )

    if ( Atm%flagstruct%fv_land ) then
       allocate ( Atm%sgh(is:ie,js:je) )
       allocate ( Atm%oro(is:ie,js:je) )
    else
       allocate ( Atm%oro(1,1) )
    endif

    ! Allocate others
    allocate (   Atm%diss_est(isd:ied  ,jsd:jed  ,npz) )
    allocate ( Atm%ts(is:ie,js:je) )
    allocate ( Atm%phis(isd:ied  ,jsd:jed  ) )
    allocate ( Atm%omga(isd:ied  ,jsd:jed  ,npz) ); Atm%omga=0.
    allocate (   Atm%ua(isd:ied  ,jsd:jed  ,npz) )
    allocate (   Atm%va(isd:ied  ,jsd:jed  ,npz) )
    allocate (   Atm%uc(isd:ied+1,jsd:jed  ,npz) )
    allocate (   Atm%vc(isd:ied  ,jsd:jed+1,npz) )
    ! For tracer transport:
    allocate ( Atm%mfx(is:ie+1, js:je,  npz) )
    allocate ( Atm%mfy(is:ie  , js:je+1,npz) )
    allocate (  Atm%cx(is:ie+1, jsd:jed, npz) )
    allocate (  Atm%cy(isd:ied ,js:je+1, npz) )

    allocate (  Atm%ak(npz_2d+1) )
    allocate (  Atm%bk(npz_2d+1) )

    !--------------------------
    ! Non-hydrostatic dynamics:
    !--------------------------
    if ( Atm%flagstruct%hydrostatic ) then
       !Note length-one initialization if hydrostatic = .true.
       allocate (    Atm%w(isd:isd, jsd:jsd  ,1) )
       allocate ( Atm%delz(isd:isd, jsd:jsd  ,1) )
       allocate (  Atm%ze0(is:is, js:js  ,1) )
    else
       allocate (    Atm%w(isd:ied, jsd:jed  ,npz  ) )
       allocate ( Atm%delz(isd:ied, jsd:jed  ,npz) )
       if( Atm%flagstruct%hybrid_z ) then
          allocate (  Atm%ze0(is:ie, js:je ,npz+1) )
       else
          allocate (  Atm%ze0(is:is, js:js  ,1) )
       endif
       !         allocate ( mono(isd:ied, jsd:jed, npz))
    endif

#ifdef USE_COND
      allocate ( Atm%q_con(isd:ied,jsd:jed,1:npz) )
#else
      allocate ( Atm%q_con(isd:isd,jsd:jsd,1) )
#endif

#ifndef NO_TOUCH_MEM
! Notes by SJL
! Place the memory in the optimal shared mem space
! This will help the scaling with OpenMP
!$OMP parallel do default(none) shared(isd,ied,jsd,jed,npz,Atm,nq,ncnst)
     do k=1, npz
        do j=jsd, jed
           do i=isd, ied
                Atm%ua(i,j,k) = real_big
                Atm%va(i,j,k) = real_big
                Atm%pt(i,j,k) = real_big
              Atm%delp(i,j,k) = real_big
           enddo
        enddo
        do j=jsd, jed+1
           do i=isd, ied
               Atm%u(i,j,k) = real_big
              Atm%vc(i,j,k) = real_big
           enddo
        enddo
        do j=jsd, jed
           do i=isd, ied+1
               Atm%v(i,j,k) = real_big
              Atm%uc(i,j,k) = real_big
           enddo
        enddo
        if ( .not. Atm%flagstruct%hydrostatic ) then
           do j=jsd, jed
              do i=isd, ied
                    Atm%w(i,j,k) = real_big
                 Atm%delz(i,j,k) = real_big
              enddo
           enddo
        endif
        do n=1,nq
        do j=jsd, jed
           do i=isd, ied
              Atm%q(i,j,k,n) = real_big
           enddo
        enddo
        enddo
        do n=nq+1,ncnst
        do j=jsd, jed
           do i=isd, ied
              Atm%qdiag(i,j,k,n) = real_big
           enddo
        enddo
        enddo
     enddo
#endif

    allocate ( Atm%gridstruct% area(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )   ! Cell Centered
    allocate ( Atm%gridstruct% area_64(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) ) ! Cell Centered
    allocate ( Atm%gridstruct%rarea(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )   ! Cell Centered
    
    allocate ( Atm%gridstruct% area_c(isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )   ! Cell Corners
    allocate ( Atm%gridstruct% area_c_64(isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )! Cell Corners
    allocate ( Atm%gridstruct%rarea_c(isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )   ! Cell Corners
    
    allocate ( Atm%gridstruct% dx(isd_2d:ied_2d  ,jsd_2d:jed_2d+1) )
    allocate ( Atm%gridstruct% dx_64(isd_2d:ied_2d  ,jsd_2d:jed_2d+1) )
    allocate ( Atm%gridstruct%rdx(isd_2d:ied_2d  ,jsd_2d:jed_2d+1) )
    allocate ( Atm%gridstruct% dy(isd_2d:ied_2d+1,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct% dy_64(isd_2d:ied_2d+1,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct%rdy(isd_2d:ied_2d+1,jsd_2d:jed_2d  ) )
    
    allocate ( Atm%gridstruct% dxc(isd_2d:ied_2d+1,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct% dxc_64(isd_2d:ied_2d+1,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct%rdxc(isd_2d:ied_2d+1,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct% dyc(isd_2d:ied_2d  ,jsd_2d:jed_2d+1) )
    allocate ( Atm%gridstruct% dyc_64(isd_2d:ied_2d  ,jsd_2d:jed_2d+1) )
    allocate ( Atm%gridstruct%rdyc(isd_2d:ied_2d  ,jsd_2d:jed_2d+1) )
    
    allocate ( Atm%gridstruct% dxa(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct% dxa_64(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct%rdxa(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct% dya(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct% dya_64(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct%rdya(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )
    
    allocate ( Atm%gridstruct%grid (isd_2d:ied_2d+1,jsd_2d:jed_2d+1,1:ndims_2d) )
    allocate ( Atm%gridstruct%grid_64 (isd_2d:ied_2d+1,jsd_2d:jed_2d+1,1:ndims_2d) )
    allocate ( Atm%gridstruct%agrid(isd_2d:ied_2d  ,jsd_2d:jed_2d  ,1:ndims_2d) )
    allocate ( Atm%gridstruct%agrid_64(isd_2d:ied_2d  ,jsd_2d:jed_2d  ,1:ndims_2d) )
    allocate ( Atm%gridstruct% sina(isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )   ! SIN(angle of intersection)
    allocate ( Atm%gridstruct% sina_64(isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )   ! SIN(angle of intersection)
    allocate ( Atm%gridstruct%rsina(is_2d:ie_2d+1,js_2d:je_2d+1) )      ! Why is the size different?
    allocate ( Atm%gridstruct% cosa(isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )   ! COS(angle of intersection)
    allocate ( Atm%gridstruct% cosa_64(isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )   ! COS(angle of intersection)
    
    allocate ( Atm%gridstruct%  e1(3,isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )
    allocate ( Atm%gridstruct%  e2(3,isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )

    allocate (Atm%gridstruct%iinta(4, isd_2d:ied_2d ,jsd_2d:jed_2d), &
         Atm%gridstruct%jinta(4, isd_2d:ied_2d ,jsd_2d:jed_2d),  &
         Atm%gridstruct%iintb(4, is_2d:ie_2d+1 ,js_2d:je_2d+1), &
         Atm%gridstruct%jintb(4, is_2d:ie_2d+1 ,js_2d:je_2d+1) )

    allocate ( Atm%gridstruct%edge_s(npx_2d) )
    allocate ( Atm%gridstruct%edge_n(npx_2d) )
    allocate ( Atm%gridstruct%edge_w(npy_2d) )
    allocate ( Atm%gridstruct%edge_e(npy_2d) )

    allocate ( Atm%gridstruct%edge_vect_s(isd_2d:ied_2d) )
    allocate ( Atm%gridstruct%edge_vect_n(isd_2d:ied_2d) )
    allocate ( Atm%gridstruct%edge_vect_w(jsd_2d:jed_2d) )
    allocate ( Atm%gridstruct%edge_vect_e(jsd_2d:jed_2d) )

    allocate ( Atm%gridstruct%ex_s(npx_2d) )
    allocate ( Atm%gridstruct%ex_n(npx_2d) )
    allocate ( Atm%gridstruct%ex_w(npy_2d) )
    allocate ( Atm%gridstruct%ex_e(npy_2d) )


    allocate (  Atm%gridstruct%l2c_u(is_2d:ie_2d,  js_2d:je_2d+1) )
    allocate (  Atm%gridstruct%l2c_v(is_2d:ie_2d+1,js_2d:je_2d) )

    ! For diveregnce damping:
    allocate (  Atm%gridstruct%divg_u(isd_2d:ied_2d,  jsd_2d:jed_2d+1) )
    allocate (  Atm%gridstruct%divg_v(isd_2d:ied_2d+1,jsd_2d:jed_2d) )
    ! For del6 diffusion:
    allocate (  Atm%gridstruct%del6_u(isd_2d:ied_2d,  jsd_2d:jed_2d+1) )
    allocate (  Atm%gridstruct%del6_v(isd_2d:ied_2d+1,jsd_2d:jed_2d) )

    allocate (  Atm%gridstruct%z11(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )
    allocate (  Atm%gridstruct%z12(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )
    allocate (  Atm%gridstruct%z21(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )
    allocate (  Atm%gridstruct%z22(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )

!   if (.not.Atm%flagstruct%hydrostatic)    &
!   allocate (  Atm%gridstruct%w00(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )

    allocate (  Atm%gridstruct%a11(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )
    allocate (  Atm%gridstruct%a12(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )
    allocate (  Atm%gridstruct%a21(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )
    allocate (  Atm%gridstruct%a22(is_2d-1:ie_2d+1,js_2d-1:je_2d+1) )
    allocate ( Atm%gridstruct%vlon(is_2d-2:ie_2d+2,js_2d-2:je_2d+2,3) )
    allocate ( Atm%gridstruct%vlat(is_2d-2:ie_2d+2,js_2d-2:je_2d+2,3) )
    ! Coriolis parameters:
    allocate ( Atm%gridstruct%f0(isd_2d:ied_2d  ,jsd_2d:jed_2d  ) )
    allocate ( Atm%gridstruct%fC(isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )

    ! Corner unit vectors:
    allocate( Atm%gridstruct%ee1(3,isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )
    allocate( Atm%gridstruct%ee2(3,isd_2d:ied_2d+1,jsd_2d:jed_2d+1) )

    ! Center unit vectors:
    allocate( Atm%gridstruct%ec1(3,isd_2d:ied_2d,jsd_2d:jed_2d) )
    allocate( Atm%gridstruct%ec2(3,isd_2d:ied_2d,jsd_2d:jed_2d) )

    ! Edge unit vectors:
    allocate( Atm%gridstruct%ew(3,isd_2d:ied_2d+1,jsd_2d:jed_2d,  2) )
    allocate( Atm%gridstruct%es(3,isd_2d:ied_2d  ,jsd_2d:jed_2d+1,2) )

    ! Edge unit "Normal" vectors: (for omega computation)
    allocate( Atm%gridstruct%en1(3,is_2d:ie_2d,  js_2d:je_2d+1) )   ! E-W edges
    allocate( Atm%gridstruct%en2(3,is_2d:ie_2d+1,js_2d:je_2d  ) )   ! N-S egdes

    allocate ( Atm%gridstruct%cosa_u(isd_2d:ied_2d+1,jsd_2d:jed_2d) )
    allocate ( Atm%gridstruct%sina_u(isd_2d:ied_2d+1,jsd_2d:jed_2d) )
    allocate ( Atm%gridstruct%rsin_u(isd_2d:ied_2d+1,jsd_2d:jed_2d) )

    allocate ( Atm%gridstruct%cosa_v(isd_2d:ied_2d,jsd_2d:jed_2d+1) )
    allocate ( Atm%gridstruct%sina_v(isd_2d:ied_2d,jsd_2d:jed_2d+1) )
    allocate ( Atm%gridstruct%rsin_v(isd_2d:ied_2d,jsd_2d:jed_2d+1) )

    allocate ( Atm%gridstruct%cosa_s(isd_2d:ied_2d,jsd_2d:jed_2d) )    ! cell center

    allocate (  Atm%gridstruct%rsin2(isd_2d:ied_2d,jsd_2d:jed_2d) )    ! cell center


    ! Super (composite) grid:

    !     9---4---8
    !     |       |
    !     1   5   3
    !     |       |
    !     6---2---7

    allocate ( Atm%gridstruct%cos_sg(isd_2d:ied_2d,jsd_2d:jed_2d,9) )
    allocate ( Atm%gridstruct%sin_sg(isd_2d:ied_2d,jsd_2d:jed_2d,9) )

    allocate( Atm%gridstruct%eww(3,4) )
    allocate( Atm%gridstruct%ess(3,4) )

    if (Atm%neststruct%nested) then

       allocate(Atm%neststruct%ind_h(isd:ied,jsd:jed,4))
       allocate(Atm%neststruct%ind_u(isd:ied,jsd:jed+1,4))
       allocate(Atm%neststruct%ind_v(isd:ied+1,jsd:jed,4))

       allocate(Atm%neststruct%wt_h(isd:ied,   jsd:jed,  4))
       allocate(Atm%neststruct%wt_u(isd:ied,   jsd:jed+1,4))
       allocate(Atm%neststruct%wt_v(isd:ied+1, jsd:jed,  4))
       allocate(Atm%neststruct%ind_b(isd:ied+1,jsd:jed+1,4))
       allocate(Atm%neststruct%wt_b(isd:ied+1, jsd:jed+1,4))

       ns = Atm%neststruct%nsponge

       call allocate_fv_nest_BC_type(Atm%neststruct%delp_BC,Atm,ns,0,0,dummy)
       call allocate_fv_nest_BC_type(Atm%neststruct%u_BC,Atm,ns,0,1,dummy)
       call allocate_fv_nest_BC_type(Atm%neststruct%v_BC,Atm,ns,1,0,dummy)
       call allocate_fv_nest_BC_type(Atm%neststruct%uc_BC,Atm,ns,1,0,dummy)
       call allocate_fv_nest_BC_type(Atm%neststruct%vc_BC,Atm,ns,0,1,dummy)
       call allocate_fv_nest_BC_type(Atm%neststruct%divg_BC,Atm,ns,1,1,dummy)

       if (ncnst > 0) then
          allocate(Atm%neststruct%q_BC(ncnst))
          do n=1,ncnst
             call allocate_fv_nest_BC_type(Atm%neststruct%q_BC(n),Atm,ns,0,0,dummy)
          enddo
       endif

#ifndef SW_DYNAMICS

       call allocate_fv_nest_BC_type(Atm%neststruct%pt_BC,Atm,ns,0,0,dummy)
#ifdef USE_COND
       call allocate_fv_nest_BC_type(Atm%neststruct%q_con_BC,Atm,ns,0,0,dummy)
#ifdef MOIST_CAPPA
       call allocate_fv_nest_BC_type(Atm%neststruct%cappa_BC,Atm,ns,0,0,dummy)
#endif
#endif
       if (.not.Atm%flagstruct%hydrostatic) then
          call allocate_fv_nest_BC_type(Atm%neststruct%w_BC,Atm,ns,0,0,dummy)
          call allocate_fv_nest_BC_type(Atm%neststruct%delz_BC,Atm,ns,0,0,dummy)
       endif

#endif

       if (Atm%neststruct%twowaynest) allocate(&
            Atm%neststruct%ind_update_h( &
              Atm%parent_grid%bd%isd:Atm%parent_grid%bd%ied+1, &
              Atm%parent_grid%bd%jsd:Atm%parent_grid%bd%jed+1,2))

    end if

    !--- Do the memory allocation only for nested model
    if( ngrids_in > 1 ) then
       if (Atm%flagstruct%grid_type < 4) then
          if (Atm%neststruct%nested) then
             allocate(Atm%grid_global(1-ng_2d:npx_2d  +ng_2d,1-ng_2d:npy_2d  +ng_2d,2,1))
          else
             allocate(Atm%grid_global(1-ng_2d:npx_2d  +ng_2d,1-ng_2d:npy_2d  +ng_2d,2,1:6))
          endif
       end if
    endif

    Atm%allocated = .true.
    if (dummy) Atm%dummy = .true.
    
  end subroutine allocate_fv_atmos_type

!>@brief The subroutine 'deallocate_fv_atmos_type' deallocates the fv_atmos_type.
  subroutine deallocate_fv_atmos_type(Atm)

    implicit none
    type(fv_atmos_type), intent(INOUT) :: Atm

    integer :: n

    if (.not.Atm%allocated) return

    deallocate (    Atm%u )
    deallocate (    Atm%v )
    deallocate (   Atm%pt )
    deallocate ( Atm%delp )
    deallocate (    Atm%q )
    deallocate (    Atm%qdiag )
    deallocate (   Atm%ps )
    deallocate (   Atm%pe )
    deallocate (   Atm%pk )
    deallocate ( Atm%peln )
    deallocate (  Atm%pkz )
    deallocate ( Atm%phis )
    deallocate ( Atm%omga )
    deallocate (   Atm%ua )
    deallocate (   Atm%va )
    deallocate (   Atm%uc )
    deallocate (   Atm%vc )
    deallocate ( Atm%mfx )
    deallocate ( Atm%mfy )
    deallocate (  Atm%cx )
    deallocate (  Atm%cy )
    deallocate (  Atm%ak )
    deallocate (  Atm%bk )

    deallocate ( Atm%u_srf )
    deallocate ( Atm%v_srf )
    if( Atm%flagstruct%fv_land ) deallocate ( Atm%sgh )
    deallocate ( Atm%oro )

    deallocate ( Atm%w )
    deallocate ( Atm%delz  )
    deallocate ( Atm%ze0   )
    deallocate ( Atm%q_con )

    deallocate ( Atm%gridstruct% area )   ! Cell Centered
    deallocate ( Atm%gridstruct%rarea )   ! Cell Centered
    
    deallocate ( Atm%gridstruct% area_c )  ! Cell Corners
    deallocate ( Atm%gridstruct%rarea_c )  ! Cell Corners
    
    deallocate ( Atm%gridstruct% dx )
    deallocate ( Atm%gridstruct%rdx )
    deallocate ( Atm%gridstruct% dy )
    deallocate ( Atm%gridstruct%rdy )
    
    deallocate ( Atm%gridstruct% dxc )
    deallocate ( Atm%gridstruct%rdxc )
    deallocate ( Atm%gridstruct% dyc )
    deallocate ( Atm%gridstruct%rdyc )
    
    deallocate ( Atm%gridstruct% dxa )
    deallocate ( Atm%gridstruct%rdxa )
    deallocate ( Atm%gridstruct% dya )
    deallocate ( Atm%gridstruct%rdya )
    
    deallocate ( Atm%gridstruct%grid  )
    deallocate ( Atm%gridstruct%agrid )
    deallocate ( Atm%gridstruct%sina )   ! SIN(angle of intersection)
    deallocate ( Atm%gridstruct%cosa )   ! COS(angle of intersection)
    
    deallocate ( Atm%gridstruct%  e1 )
    deallocate ( Atm%gridstruct%  e2 )




    deallocate (Atm%gridstruct%iinta, &
         Atm%gridstruct%jinta,  &
         Atm%gridstruct%iintb, &
         Atm%gridstruct%jintb )

    deallocate ( Atm%gridstruct%edge_s )
    deallocate ( Atm%gridstruct%edge_n )
    deallocate ( Atm%gridstruct%edge_w )
    deallocate ( Atm%gridstruct%edge_e )

    deallocate ( Atm%gridstruct%edge_vect_s )
    deallocate ( Atm%gridstruct%edge_vect_n )
    deallocate ( Atm%gridstruct%edge_vect_w )
    deallocate ( Atm%gridstruct%edge_vect_e )

    deallocate ( Atm%gridstruct%ex_s )
    deallocate ( Atm%gridstruct%ex_n )
    deallocate ( Atm%gridstruct%ex_w )
    deallocate ( Atm%gridstruct%ex_e )


    deallocate (  Atm%gridstruct%l2c_u )
    deallocate (  Atm%gridstruct%l2c_v )
    ! For diveregnce damping:
    deallocate (  Atm%gridstruct%divg_u )
    deallocate (  Atm%gridstruct%divg_v )
    ! For del6 diffusion:

    deallocate (  Atm%gridstruct%z11 )
    deallocate (  Atm%gridstruct%z12 )
    deallocate (  Atm%gridstruct%z21 )
    deallocate (  Atm%gridstruct%z22 )

    deallocate (  Atm%gridstruct%a11 )
    deallocate (  Atm%gridstruct%a12 )
    deallocate (  Atm%gridstruct%a21 )
    deallocate (  Atm%gridstruct%a22 )
    deallocate ( Atm%gridstruct%vlon )
    deallocate ( Atm%gridstruct%vlat )
    ! Coriolis parameters:
    deallocate ( Atm%gridstruct%f0 )
    deallocate ( Atm%gridstruct%fC )

    ! Corner unit vectors:
    deallocate( Atm%gridstruct%ee1 )
    deallocate( Atm%gridstruct%ee2 )

    ! Center unit vectors:
    deallocate( Atm%gridstruct%ec1 )
    deallocate( Atm%gridstruct%ec2 )

    ! Edge unit vectors:
    deallocate( Atm%gridstruct%ew )
    deallocate( Atm%gridstruct%es )

    ! Edge unit "Normal" vectors: (for omega computation)
    deallocate( Atm%gridstruct%en1 )   ! E-W edges
    deallocate( Atm%gridstruct%en2 )   ! N-S egdes

    deallocate ( Atm%gridstruct%cosa_u )
    deallocate ( Atm%gridstruct%sina_u )
    deallocate ( Atm%gridstruct%rsin_u )

    deallocate ( Atm%gridstruct%cosa_v )
    deallocate ( Atm%gridstruct%sina_v )
    deallocate ( Atm%gridstruct%rsin_v )

    deallocate ( Atm%gridstruct%cosa_s )    ! cell center

    deallocate (  Atm%gridstruct%rsin2 )    ! cell center


    ! Super (composite) grid:

    !     9---4---8
    !     |       |
    !     1   5   3
    !     |       |
    !     6---2---7

    deallocate ( Atm%gridstruct%cos_sg )
    deallocate ( Atm%gridstruct%sin_sg )

    deallocate( Atm%gridstruct%eww )
    deallocate( Atm%gridstruct%ess )

    if (Atm%neststruct%nested) then
       deallocate(Atm%neststruct%ind_h)
       deallocate(Atm%neststruct%ind_u)
       deallocate(Atm%neststruct%ind_v)

       deallocate(Atm%neststruct%wt_h)
       deallocate(Atm%neststruct%wt_u)
       deallocate(Atm%neststruct%wt_v)

       deallocate(Atm%neststruct%ind_b)
       deallocate(Atm%neststruct%wt_b)

       call deallocate_fv_nest_BC_type(Atm%neststruct%delp_BC)
       call deallocate_fv_nest_BC_type(Atm%neststruct%u_BC)
       call deallocate_fv_nest_BC_type(Atm%neststruct%v_BC)
       call deallocate_fv_nest_BC_type(Atm%neststruct%uc_BC)
       call deallocate_fv_nest_BC_type(Atm%neststruct%vc_BC)
       call deallocate_fv_nest_BC_type(Atm%neststruct%divg_BC)

       if (allocated(Atm%neststruct%q_BC)) then
          do n=1,size(Atm%neststruct%q_BC)
             call deallocate_fv_nest_BC_type(Atm%neststruct%q_BC(n))
          enddo
       endif

#ifndef SW_DYNAMICS
       call deallocate_fv_nest_BC_type(Atm%neststruct%pt_BC)
#ifdef USE_COND
       call deallocate_fv_nest_BC_type(Atm%neststruct%q_con_BC)
#ifdef MOIST_CAPPA
       call deallocate_fv_nest_BC_type(Atm%neststruct%cappa_BC)
#endif
#endif
       if (.not.Atm%flagstruct%hydrostatic) then
          call deallocate_fv_nest_BC_type(Atm%neststruct%w_BC)
          call deallocate_fv_nest_BC_type(Atm%neststruct%delz_BC)
       endif
#endif


       if (Atm%neststruct%twowaynest) deallocate(Atm%neststruct%ind_update_h)

    end if

    if (Atm%flagstruct%grid_type < 4) then
       if(allocated(Atm%grid_global)) deallocate(Atm%grid_global)
    end if
    
    Atm%allocated = .false.

  end subroutine deallocate_fv_atmos_type


subroutine allocate_fv_nest_BC_type_3D_Atm(BC,Atm,ns,istag,jstag,dummy)

  type(fv_nest_BC_type_3D), intent(INOUT) :: BC
  type(fv_atmos_type), intent(IN) :: Atm
  integer, intent(IN) :: ns, istag, jstag
  logical, intent(IN) :: dummy

  integer :: is, ie, js, je, isd, ied, jsd, jed, npx, npy, npz, ng

  if (BC%allocated) return

  is = Atm%bd%is
  ie = Atm%bd%ie
  js = Atm%bd%js
  je = Atm%bd%je

  isd = Atm%bd%isd
  ied = Atm%bd%ied
  jsd = Atm%bd%jsd
  jed = Atm%bd%jed

  npx = Atm%npx
  npy = Atm%npy
  npz = Atm%npz

  ng = Atm%ng

  call allocate_fv_nest_BC_type_3D(BC,is,ie,js,je,isd,ied,jsd,jed,npx,npy,npz,ng,ns,istag,jstag,dummy)


end subroutine allocate_fv_nest_BC_type_3D_Atm

subroutine allocate_fv_nest_BC_type_3D(BC,is,ie,js,je,isd,ied,jsd,jed,npx,npy,npz,ng,ns,istag,jstag,dummy)

  type(fv_nest_BC_type_3D), intent(INOUT) :: BC
  integer, intent(IN) :: ns, istag, jstag
  logical, intent(IN) :: dummy

  integer, intent(IN) :: is, ie, js, je, isd, ied, jsd, jed, npx, npy, npz, ng

  if (BC%allocated) return


  if (ie == npx-1 .and. .not. dummy) then
     allocate(BC%east_t1(ie+1-ns+istag:ied+istag,jsd:jed+jstag,npz))
     allocate(BC%east_t0(ie+1-ns+istag:ied+istag,jsd:jed+jstag,npz))
     do k=1,npz
     do j=jsd,jed+jstag
     do i=ie+1-ns+istag,ied+istag
        BC%east_t1(i,j,k) = 0.
        BC%east_t0(i,j,k) = 0.
     enddo
     enddo
     enddo
  else
     allocate(BC%east_t1(1,1,npz))
     allocate(BC%east_t0(1,1,npz))
  end if

  if (js == 1 .and. .not. dummy) then
     allocate(BC%south_t1(isd:ied+istag,jsd:js-1+ns,npz))
     allocate(BC%south_t0(isd:ied+istag,jsd:js-1+ns,npz))
     do k=1,npz
     do j=jsd,js-1+ns
     do i=isd,ied+istag
        BC%south_t1(i,j,k) = 0.
        BC%south_t0(i,j,k) = 0.
     enddo
     enddo
     enddo
  else
     allocate(BC%south_t1(1,1,npz))
     allocate(BC%south_t0(1,1,npz))
  end if

  if (is == 1 .and. .not. dummy) then
     allocate(BC%west_t1(isd:is-1+ns,jsd:jed+jstag,npz))
     allocate(BC%west_t0(isd:is-1+ns,jsd:jed+jstag,npz))
     do k=1,npz
     do j=jsd,jed+jstag
     do i=isd,is-1+ns
        BC%west_t1(i,j,k) = 0.
        BC%west_t0(i,j,k) = 0.
     enddo
     enddo
     enddo
  else
     allocate(BC%west_t1(1,1,npz))
     allocate(BC%west_t0(1,1,npz))
  end if

  if (je == npy-1 .and. .not. dummy) then
     allocate(BC%north_t1(isd:ied+istag,je+1-ns+jstag:jed+jstag,npz))
     allocate(BC%north_t0(isd:ied+istag,je+1-ns+jstag:jed+jstag,npz))
     do k=1,npz
     do j=je+1-ns+jstag,jed+jstag
     do i=isd,ied+istag
        BC%north_t1(i,j,k) = 0.
        BC%north_t0(i,j,k) = 0.
     enddo
     enddo
     enddo
  else
     allocate(BC%north_t1(1,1,npz))
     allocate(BC%north_t0(1,1,npz))
  end if

  BC%allocated = .true.

end subroutine allocate_fv_nest_BC_type_3D

subroutine deallocate_fv_nest_BC_type_3d(BC)

  type(fv_nest_BC_type_3d) :: BC

  if (.not. BC%allocated) return

     deallocate(BC%north_t1)
     deallocate(BC%south_t1)
     deallocate(BC%west_t1)
     deallocate(BC%east_t1)

  if (allocated(BC%north_t0)) then
     deallocate(BC%north_t0)
     deallocate(BC%south_t0)
     deallocate(BC%west_t0)
     deallocate(BC%east_t0)
  endif

  BC%allocated = .false.

end subroutine deallocate_fv_nest_BC_type_3d


end module fv_arrays_mod
