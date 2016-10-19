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
module atmos_model_mod
!-----------------------------------------------------------------------
!<OVERVIEW>
!  Driver for the atmospheric model, contains routines to advance the
!  atmospheric model state by one time step.
!</OVERVIEW>

!<DESCRIPTION>
!     This version of atmos_model_mod has been designed around the implicit
!     version diffusion scheme of the GCM. It requires two routines to advance
!     the atmospheric model one time step into the future. These two routines
!     correspond to the down and up sweeps of the standard tridiagonal solver.
!     Most atmospheric processes (dynamics,radiation,etc.) are performed
!     in the down routine. The up routine finishes the vertical diffusion
!     and computes moisture related terms (convection,large-scale condensation,
!     and precipitation).

!     The boundary variables needed by other component models for coupling
!     are contained in a derived data type. A variable of this derived type
!     is returned when initializing the atmospheric model. It is used by other
!     routines in this module and by coupling routines. The contents of
!     this derived type should only be modified by the atmospheric model.

!</DESCRIPTION>

use mpp_mod,            only: mpp_pe, mpp_root_pe, mpp_clock_id, mpp_clock_begin
use mpp_mod,            only: mpp_clock_end, CLOCK_COMPONENT, MPP_CLOCK_SYNC
use mpp_mod,            only: mpp_min, mpp_max, mpp_error, mpp_chksum
use mpp_domains_mod,    only: domain2d
#ifdef INTERNAL_FILE_NML
use mpp_mod,            only: input_nml_file
#else
use fms_mod,            only: open_namelist_file
#endif
use fms_mod,            only: file_exist, error_mesg, field_size, FATAL, NOTE, WARNING
use fms_mod,            only: close_file,  write_version_number, stdlog, stdout
use fms_mod,            only: clock_flag_default
use fms_mod,            only: check_nml_error
use time_manager_mod,   only: time_type, operator(+), get_time, operator(-)
use field_manager_mod,  only: MODEL_ATMOS
use tracer_manager_mod, only: get_number_tracers, get_tracer_index, NO_TRACER
use xgrid_mod,          only: grid_box_type
use atmosphere_mod,     only: atmosphere_init
use atmosphere_mod,     only: atmosphere_end
use atmosphere_mod,     only: atmosphere_resolution, atmosphere_domain
use atmosphere_mod,     only: atmosphere_boundary, atmosphere_grid_center
use atmosphere_mod,     only: atmosphere_dynamics, get_atmosphere_axes
use atmosphere_mod,     only: get_atmosphere_grid
use atmosphere_mod,     only: atmosphere_restart
use atmosphere_mod,     only: atmosphere_state_update
use atmosphere_mod,     only: atmos_phys_driver_statein
use atmosphere_mod,     only: atmosphere_control_data, atmosphere_pref
use atmosphere_mod,     only: set_atmosphere_pelist
use coupler_types_mod,  only: coupler_2d_bc_type
use block_control_mod,  only: block_control_type, define_blocks
use gfs_physics_driver_mod, only: state_fields_in, state_fields_out, &
                                  kind_phys, phys_rad_driver_init, &
                                  phys_rad_driver_restart, &
                                  phys_rad_driver_end, &
                                  phys_rad_setup_step, &
                                  radiation_driver, physics_driver, skin_temp

!-----------------------------------------------------------------------

implicit none
private

public update_atmos_radiation_physics
public update_atmos_model_state
public update_atmos_model_dynamics
public atmos_model_init, atmos_model_end, atmos_data_type
public atmos_model_restart
!-----------------------------------------------------------------------

!<PUBLICTYPE >
 type atmos_data_type
     type (domain2d)               :: domain             ! domain decomposition
     integer                       :: axes(4)            ! axis indices (returned by diag_manager) for the atmospheric grid 
                                                         ! (they correspond to the x, y, pfull, phalf axes)
     real, pointer, dimension(:,:) :: lon_bnd  => null() ! local longitude axis grid box corners in radians.
     real, pointer, dimension(:,:) :: lat_bnd  => null() ! local latitude axis grid box corners in radians.
     real, pointer, dimension(:,:) :: lon      => null() ! local longitude axis grid box centers in radians.
     real, pointer, dimension(:,:) :: lat      => null() ! local latitude axis grid box centers in radians.
     real, pointer, dimension(:,:) :: t_bot    => null() ! temperature at lowest model level
     real, pointer, dimension(:,:,:) :: tr_bot => null() ! tracers at lowest model level
     real, pointer, dimension(:,:) :: z_bot    => null() ! height above the surface for the lowest model level
     real, pointer, dimension(:,:) :: p_bot    => null() ! pressure at lowest model level
     real, pointer, dimension(:,:) :: u_bot    => null() ! zonal wind component at lowest model level
     real, pointer, dimension(:,:) :: v_bot    => null() ! meridional wind component at lowest model level
     real, pointer, dimension(:,:) :: p_surf   => null() ! surface pressure 
     real, pointer, dimension(:,:) :: slp      => null() ! sea level pressure 
     real, pointer, dimension(:,:) :: gust     => null() ! gustiness factor
     real, pointer, dimension(:,:) :: coszen   => null() ! cosine of the zenith angle
     real, pointer, dimension(:,:) :: flux_sw  => null() ! net shortwave flux (W/m2) at the surface
     real, pointer, dimension(:,:) :: flux_sw_dir            =>null()
     real, pointer, dimension(:,:) :: flux_sw_dif            =>null()
     real, pointer, dimension(:,:) :: flux_sw_down_vis_dir   =>null()
     real, pointer, dimension(:,:) :: flux_sw_down_vis_dif   =>null()
     real, pointer, dimension(:,:) :: flux_sw_down_total_dir =>null()
     real, pointer, dimension(:,:) :: flux_sw_down_total_dif =>null()
     real, pointer, dimension(:,:) :: flux_sw_vis            =>null()
     real, pointer, dimension(:,:) :: flux_sw_vis_dir        =>null()
     real, pointer, dimension(:,:) :: flux_sw_vis_dif        =>null()
     real, pointer, dimension(:,:) :: flux_lw  => null() ! net longwave flux (W/m2) at the surface
     real, pointer, dimension(:,:) :: lprec    => null() ! mass of liquid precipitation since last time step (Kg/m2)
     real, pointer, dimension(:,:) :: fprec    => null() ! ass of frozen precipitation since last time step (Kg/m2)
     logical, pointer, dimension(:,:) :: maskmap =>null()! A pointer to an array indicating which
                                                         ! logical processors are actually used for
                                                         ! the ocean code. The other logical
                                                         ! processors would be all land points and
                                                         ! are not assigned to actual processors.
                                                         ! This need not be assigned if all logical
                                                         ! processors are used. This variable is dummy and need 
                                                         ! not to be set, but it is needed to pass compilation.
     type (time_type)              :: Time               ! current time
     type (time_type)              :: Time_step          ! atmospheric time step.
     type (time_type)              :: Time_init          ! reference time.
     integer, pointer              :: pelist(:) =>null() ! pelist where atmosphere is running.
     logical                       :: pe                 ! current pe.
     type(coupler_2d_bc_type)      :: fields             ! array of fields used for additional tracers
     type(grid_box_type)           :: grid               ! hold grid information needed for 2nd order conservative flux exchange 
                                                         ! to calculate gradient on cubic sphere grid.
     real(kind=kind_phys)          :: dxmin
     real(kind=kind_phys)          :: dxmax
     real(kind=kind_phys), pointer, dimension(:,:) :: xlon
     real(kind=kind_phys), pointer, dimension(:,:) :: xlat
     real(kind=kind_phys), pointer, dimension(:,:) :: dx
     real(kind=kind_phys), pointer, dimension(:,:) :: dy
     real(kind=kind_phys), pointer, dimension(:,:) :: area
 end type atmos_data_type
!</PUBLICTYPE >

integer :: fv3Clock, getClock, updClock, setupClock, radClock, physClock

!-----------------------------------------------------------------------

integer :: ivapor = NO_TRACER ! index of water vapor tracer

!-----------------------------------------------------------------------
integer :: nxblocks = 1
integer :: nyblocks = 1
logical :: surface_debug = .false.
logical :: dycore_only = .false.
logical :: debug = .false.
logical :: sync = .false.
namelist /atmos_model_nml/ nxblocks, nyblocks, surface_debug, dycore_only, debug, sync

!--- concurrent and decoupled radiation and physics variables
type (state_fields_in),  dimension(:), allocatable :: Statein
type (state_fields_out), dimension(:), allocatable :: Stateout
type (block_control_type)    :: Atm_block

integer :: ntrace, ntprog

!-----------------------------------------------------------------------

character(len=128) :: version = '$Id$'
character(len=128) :: tagname = '$Name$'

contains

!#######################################################################
! <SUBROUTINE NAME="update_radiation_physics">
!
!<DESCRIPTION>
!   Called every time step as the atmospheric driver to compute the
!   atmospheric tendencies for dynamics, radiation, vertical diffusion of
!   momentum, tracers, and heat/moisture.  For heat/moisture only the
!   downward sweep of the tridiagonal elimination is performed, hence
!   the name "_down". 
!</DESCRIPTION>

!   <TEMPLATE>
!     call  update_atmos_radiation_physics (Atmos)
!   </TEMPLATE>

! <INOUT NAME="Atmos" TYPE="type(atmos_data_type)">
!   Derived-type variable that contains fields needed by the flux exchange module.
!   These fields describe the atmospheric grid and are needed to
!   compute/exchange fluxes with other component models.  All fields in this
!   variable type are allocated for the global grid (without halo regions).
! </INOUT>

subroutine update_atmos_radiation_physics (Atmos)
!-----------------------------------------------------------------------
  type (atmos_data_type), intent(in) :: Atmos
!--- local variables---
    type(time_type) :: Time_next
    real :: tmax, tmin
    integer :: nb

    Time_next = Atmos%Time + Atmos%Time_step

    if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "statein driver"
!--- get atmospheric state from the dynamic core
    call set_atmosphere_pelist()
    call mpp_clock_begin(getClock)
    call atmos_phys_driver_statein (Statein, Atm_block)
    call mpp_clock_end(getClock)

    if (surface_debug) call check_data ('FV DYNAMICS')

    if (dycore_only) then
      do nb = 1,Atm_block%nblks
        Stateout(nb)%gu0 = Statein(nb)%ugrs
        Stateout(nb)%gv0 = Statein(nb)%vgrs
        Stateout(nb)%gt0 = Statein(nb)%tgrs
        Stateout(nb)%gq0 = Statein(nb)%qgrs
      enddo
    else
      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "setup step"
      call mpp_clock_begin(setupClock)
#ifdef AVEC_TIMERS
      call avec_timer_start(3)
#endif
      call phys_rad_setup_step (Atmos%Time_init, Atmos%Time, Time_next, Atm_block)
#ifdef AVEC_TIMERS
      call avec_timer_stop(3)
#endif
      call mpp_clock_end(setupClock)

      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "radiation driver"
!--- execute the GFS atmospheric radiation subcomponent (RRTM)
      call mpp_clock_begin(radClock)
#ifdef AVEC_TIMERS
      call avec_timer_start(4)
#endif
      call radiation_driver (Atm_block, Statein)
#ifdef AVEC_TIMERS
      call avec_timer_stop(4)
#endif
      call mpp_clock_end(radClock)

      if (surface_debug) call check_data ('RADIATION')

      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "physics driver"
!--- execute the GFS atmospheric physics subcomponent
      call mpp_clock_begin(physClock)
#ifdef AVEC_TIMERS
      call avec_timer_start(5)
#endif
      call physics_driver (Time_next, Atmos%Time_init, Atm_block, Statein, Stateout)
#ifdef AVEC_TIMERS
      call avec_timer_stop(5)
#endif
      call mpp_clock_end(physClock)

      if (surface_debug) call check_data ('PHYSICS')

      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "end of radiation and physics step"
    endif

!-----------------------------------------------------------------------
 end subroutine update_atmos_radiation_physics
! </SUBROUTINE>


 subroutine check_data (name_str)
    character(len=*), intent(in) :: name_str
    real(kind=kind_phys) :: tsmax, tsmin, timax, timin
    real(kind=kind_phys) :: t1max_l, t1min_l, t1max, t1min
    real(kind=kind_phys) :: psmax_l, psmin_l, psmax, psmin
    real(kind=kind_phys) :: plmax_l, plmin_l, plmax, plmin
    integer :: nb
   
    t1max = -99999.0
    t1min = +99999.0
    psmax = -99999.0
    psmin = +99999.0
    plmax = -99999.0
    plmin = +99999.0
    do nb = 1,Atm_block%nblks
      t1max_l = maxval(Statein(nb)%tgrs(:,1))
      t1min_l = minval(Statein(nb)%tgrs(:,1))
      t1max = max(t1max,t1max_l)
      t1min = min(t1min,t1min_l)
      psmax_l = maxval(Statein(nb)%pgr(:))
      psmin_l = minval(Statein(nb)%pgr(:))
      psmax = max(psmax,psmax_l)
      psmin = min(psmin,psmin_l)
      plmax_l = maxval(Statein(nb)%prsl(:,1))
      plmin_l = minval(Statein(nb)%prsl(:,1))
      plmax = max(plmax,plmax_l)
      plmin = min(plmin,plmin_l)
    enddo

    if (mpp_pe() == mpp_root_pe()) write(6,*) 'after ',trim(name_str),' component'
    call skin_temp (tsmin,tsmax, timax, timin, Atm_block%nblks)
    if (mpp_pe() == mpp_root_pe()) write(6,*) '     GFS TSFC  max: ',tsmax,'   min: ',tsmin
    call mpp_max(t1max)
    call mpp_min(t1min)
    if (mpp_pe() == mpp_root_pe()) write(6,*) '    GFDL TL1   max: ',t1max,'   min: ',t1min
    call mpp_max(psmax)
    call mpp_min(psmin)
    if (mpp_pe() == mpp_root_pe()) write(6,*) '    GFDL PSFC  max: ',psmax,'   min: ',psmin
    call mpp_max(plmax)
    call mpp_min(plmin)
    if (mpp_pe() == mpp_root_pe()) write(6,*) '    GFDL PL1   max: ',plmax,'   min: ',plmin

 end subroutine check_data

!#######################################################################
! <SUBROUTINE NAME="atmos_model_init">
!
! <OVERVIEW>
! Routine to initialize the atmospheric model
! </OVERVIEW>

subroutine atmos_model_init (Atmos, Time_init, Time, Time_step)

  type (atmos_data_type), intent(inout) :: Atmos
  type (time_type), intent(in) :: Time_init, Time, Time_step
!--- local variables ---
  integer :: unit, ntdiag, ntfamily, i, j, k
  integer :: mlon, mlat, nlon, nlat, nlev, sec, dt
  integer :: ierr, io, logunit
  integer :: idx
  integer :: isc, iec, jsc, jec
  integer :: isd, ied, jsd, jed
  integer :: blk, ibs, ibe, jbs, jbe
  real(kind=kind_phys) :: dt_phys
  real, allocatable :: q(:,:,:,:), p_half(:,:,:)
  character(len=80) :: control
  character(len=64) :: filename, filename2
  character(len=132) :: text
  logical :: p_hydro, hydro
  logical, save :: block_message = .true.
!-----------------------------------------------------------------------

!---- set the atmospheric model time ------

   Atmos % Time_init = Time_init
   Atmos % Time      = Time
   Atmos % Time_step = Time_step
   call get_time (Atmos % Time_step, sec)
   dt_phys = real(sec)      ! integer seconds

   logunit = stdlog()

!-----------------------------------------------------------------------
! how many tracers have been registered?
!  (will print number below)
   call get_number_tracers ( MODEL_ATMOS, ntrace, ntprog, ntdiag, ntfamily )
   if ( ntfamily > 0 ) call error_mesg ('atmos_model', 'ntfamily > 0', FATAL)
   ivapor = get_tracer_index( MODEL_ATMOS, 'sphum' )
   if (ivapor==NO_TRACER) &
        ivapor = get_tracer_index( MODEL_ATMOS, 'mix_rat' )
   if (ivapor==NO_TRACER) &
        call error_mesg('atmos_model_init', 'Cannot find water vapor in ATM tracer table', FATAL)

!-----------------------------------------------------------------------
! initialize atmospheric model -----

!---------- initialize atmospheric dynamics -------
   call atmosphere_init (Atmos%Time_init, Atmos%Time, Atmos%Time_step,&
                         Atmos%grid, Atmos%dx, Atmos%dy, Atmos%area)

   IF ( file_exist('input.nml')) THEN
#ifdef INTERNAL_FILE_NML
      read(input_nml_file, nml=atmos_model_nml, iostat=io)
      ierr = check_nml_error(io, 'atmos_model_nml')
#else
      unit = open_namelist_file ( )
      ierr=1
      do while (ierr /= 0)
         read  (unit, nml=atmos_model_nml, iostat=io, end=10)
         ierr = check_nml_error(io,'atmos_model_nml')
      enddo
 10     call close_file (unit)
#endif
   endif
!-----------------------------------------------------------------------
   call atmosphere_resolution (nlon, nlat, global=.false.)
   call atmosphere_resolution (mlon, mlat, global=.true.)
   call alloc_atmos_data_type (nlon, nlat, ntprog, Atmos)
   call atmosphere_domain (Atmos%domain)
   call get_atmosphere_axes (Atmos%axes)
   call get_atmosphere_grid (Atmos%dxmax, Atmos%dxmin)
   call atmosphere_boundary (Atmos%lon_bnd, Atmos%lat_bnd, global=.false.)
   allocate(Atmos%xlon(nlon, nlat))
   allocate(Atmos%xlat(nlon, nlat))
   call atmosphere_grid_center (Atmos%xlon, Atmos%xlat)

!-----------------------------------------------------------------------
!--- before going any further check definitions for 'blocks'
!-----------------------------------------------------------------------
   call atmosphere_control_data (isc, iec, jsc, jec, nlev, p_hydro, hydro)
   call define_blocks ('atmos_model', Atm_block, isc, iec, jsc, jec, nlev, &
                       nxblocks, nyblocks, block_message)
   allocate(Statein (Atm_block%nblks))
   allocate(Stateout(Atm_block%nblks))

!---------- initialize physics -------
   call phys_rad_driver_init(Atmos%Time,        &
                             Atmos%Time_init,  &
                             Atmos%xlon(:,:),   &
                             Atmos%xlat(:,:),   &
                             mlon,              &
                             mlat,              &
                             nlev,              &
                             Atmos%axes,        &
                             Atmos%area,        &
                             dt_phys,           &
                             Atm_block,         &
                             Statein, Stateout, &
                             Atmos%domain) 

!---- print version number to logfile ----

   call write_version_number ( version, tagname )
!  write the namelist to a log file
   if (mpp_pe() == mpp_root_pe()) then
      unit = stdlog( )
      write (unit, nml=atmos_model_nml)
      call close_file (unit)
!  number of tracers
      write (unit, '(a,i3)') 'Number of tracers =', ntrace
      write (unit, '(a,i3)') 'Number of prognostic tracers =', ntprog
      write (unit, '(a,i3)') 'Number of diagnostic tracers =', ntdiag
   endif

   setupClock = mpp_clock_id( 'GFS Step Setup        ', flags=clock_flag_default, grain=CLOCK_COMPONENT )
   radClock   = mpp_clock_id( 'GFS Radiation         ', flags=clock_flag_default, grain=CLOCK_COMPONENT )
   physClock  = mpp_clock_id( 'GFS Physics           ', flags=clock_flag_default, grain=CLOCK_COMPONENT )
   getClock   = mpp_clock_id( 'Dynamics get state    ', flags=clock_flag_default, grain=CLOCK_COMPONENT )
   updClock   = mpp_clock_id( 'Dynamics update state ', flags=clock_flag_default, grain=CLOCK_COMPONENT )
   if (sync) then
     fv3Clock = mpp_clock_id( 'FV3 Dycore            ', flags=clock_flag_default+MPP_CLOCK_SYNC, grain=CLOCK_COMPONENT )
   else
     fv3Clock = mpp_clock_id( 'FV3 Dycore            ', flags=clock_flag_default, grain=CLOCK_COMPONENT )
   endif

!-----------------------------------------------------------------------
end subroutine atmos_model_init
! </SUBROUTINE>


!#######################################################################
! <SUBROUTINE NAME="update_atmos_model_dynamics"
!
! <OVERVIEW>
subroutine update_atmos_model_dynamics (Atmos)
! run the atmospheric dynamics to advect the properties
  type (atmos_data_type), intent(in) :: Atmos

    call set_atmosphere_pelist()
    call mpp_clock_begin(fv3Clock)
    call atmosphere_dynamics (Atmos%Time)
    call mpp_clock_end(fv3Clock)

end subroutine update_atmos_model_dynamics
! </SUBROUTINE>


!#######################################################################
! <SUBROUTINE NAME="update_atmos_model_state"
!
! <OVERVIEW>
subroutine update_atmos_model_state (Atmos)
! to update the model state after all concurrency is completed
  type (atmos_data_type), intent(inout) :: Atmos
!--- local variables
  real :: tmax, tmin

    call set_atmosphere_pelist()
    call mpp_clock_begin(fv3Clock)
    call mpp_clock_begin(updClock)
    call atmosphere_state_update (Atmos%Time, Statein, Stateout, Atm_block)
    call mpp_clock_end(updClock)
    call mpp_clock_end(fv3Clock)

!------ advance time ------
    Atmos % Time = Atmos % Time + Atmos % Time_step

    if (surface_debug) call check_data ('STATE UPDATE')

 end subroutine update_atmos_model_state
! </SUBROUTINE>



!#######################################################################
! <SUBROUTINE NAME="atmos_model_end">
!
! <OVERVIEW>
!  termination routine for atmospheric model
! </OVERVIEW>

! <DESCRIPTION>
!  Call once to terminate this module and any other modules used.
!  This routine writes a restart file and deallocates storage
!  used by the derived-type variable atmos_boundary_data_type.
! </DESCRIPTION>

! <TEMPLATE>
!   call atmos_model_end (Atmos)
! </TEMPLATE>

! <INOUT NAME="Atmos" TYPE="type(atmos_data_type)">
!   Derived-type variable that contains fields needed by the flux exchange module.
! </INOUT>

subroutine atmos_model_end (Atmos)
  type (atmos_data_type), intent(inout) :: Atmos
!---local variables
  integer :: idx

!-----------------------------------------------------------------------
!---- termination routine for atmospheric model ----
                                              
    call atmosphere_end (Atmos % Time, Atmos%grid)
    call phys_rad_driver_end (Atm_block, Atmos%domain)

end subroutine atmos_model_end

! </SUBROUTINE>
  !#######################################################################
  ! <SUBROUTINE NAME="atmos_model_restart">
  ! <DESCRIPTION>
  !  Write out restart files registered through register_restart_file
  ! </DESCRIPTION>
  subroutine atmos_model_restart(Atmos, timestamp)
    type (atmos_data_type),   intent(inout) :: Atmos
    character(len=*),  intent(in)           :: timestamp

     call atmosphere_restart(timestamp)
     call phys_rad_driver_restart (Atm_block, Atmos%domain, timestamp)

  end subroutine atmos_model_restart
  ! </SUBROUTINE>

!#######################################################################
!#######################################################################
! <SUBROUTINE NAME="atmos_data_type_chksum">
!
! <OVERVIEW>
!  Print checksums of the various fields in the atmos_data_type.
! </OVERVIEW>

! <DESCRIPTION>
!  Routine to print checksums of the various fields in the atmos_data_type.
! </DESCRIPTION>

! <TEMPLATE>
!   call atmos_data_type_chksum(id, timestep, atm)
! </TEMPLATE>

! <IN NAME="Atm" TYPE="type(atmos_data_type)">
!   Derived-type variable that contains fields in the atmos_data_type.
! </INOUT>
!
! <IN NAME="id" TYPE="character">
!   Label to differentiate where this routine in being called from.
! </IN>
!
! <IN NAME="timestep" TYPE="integer">
!   An integer to indicate which timestep this routine is being called for.
! </IN>
!
subroutine atmos_data_type_chksum(id, timestep, atm)
type(atmos_data_type), intent(in) :: atm 
    character(len=*), intent(in) :: id
    integer         , intent(in) :: timestep
    integer :: n, outunit

100 format("CHECKSUM::",A32," = ",Z20)
101 format("CHECKSUM::",A16,a,'%',a," = ",Z20)

  outunit = stdout()
  write(outunit,*) 'BEGIN CHECKSUM(Atmos_data_type):: ', id, timestep
  write(outunit,100) ' atm%lon_bnd                ', mpp_chksum(atm%lon_bnd               )
  write(outunit,100) ' atm%lat_bnd                ', mpp_chksum(atm%lat_bnd               )
  write(outunit,100) ' atm%lon                    ', mpp_chksum(atm%lon                   )
  write(outunit,100) ' atm%lat                    ', mpp_chksum(atm%lat                   )
  write(outunit,100) ' atm%t_bot                  ', mpp_chksum(atm%t_bot                 )
  do n = 1, size(atm%tr_bot,3)
  write(outunit,100) ' atm%tr_bot(:,:,n)          ', mpp_chksum(atm%tr_bot(:,:,n)         )
  enddo
  write(outunit,100) ' atm%z_bot                  ', mpp_chksum(atm%z_bot                 )
  write(outunit,100) ' atm%p_bot                  ', mpp_chksum(atm%p_bot                 )
  write(outunit,100) ' atm%u_bot                  ', mpp_chksum(atm%u_bot                 )
  write(outunit,100) ' atm%v_bot                  ', mpp_chksum(atm%v_bot                 )
  write(outunit,100) ' atm%p_surf                 ', mpp_chksum(atm%p_surf                )
  write(outunit,100) ' atm%slp                    ', mpp_chksum(atm%slp                   )
  write(outunit,100) ' atm%gust                   ', mpp_chksum(atm%gust                  )
  write(outunit,100) ' atm%coszen                 ', mpp_chksum(atm%coszen                )
  write(outunit,100) ' atm%flux_sw                ', mpp_chksum(atm%flux_sw               )
  write(outunit,100) ' atm%flux_sw_dir            ', mpp_chksum(atm%flux_sw_dir           )
  write(outunit,100) ' atm%flux_sw_dif            ', mpp_chksum(atm%flux_sw_dif           )
  write(outunit,100) ' atm%flux_sw_down_vis_dir   ', mpp_chksum(atm%flux_sw_down_vis_dir  )
  write(outunit,100) ' atm%flux_sw_down_vis_dif   ', mpp_chksum(atm%flux_sw_down_vis_dif  )
  write(outunit,100) ' atm%flux_sw_down_total_dir ', mpp_chksum(atm%flux_sw_down_total_dir)
  write(outunit,100) ' atm%flux_sw_down_total_dif ', mpp_chksum(atm%flux_sw_down_total_dif)
  write(outunit,100) ' atm%flux_sw_vis            ', mpp_chksum(atm%flux_sw_vis           )
  write(outunit,100) ' atm%flux_sw_vis_dir        ', mpp_chksum(atm%flux_sw_vis_dir       )
  write(outunit,100) ' atm%flux_sw_vis_dif        ', mpp_chksum(atm%flux_sw_vis_dif       )
  write(outunit,100) ' atm%flux_lw                ', mpp_chksum(atm%flux_lw               )
  write(outunit,100) ' atm%lprec                  ', mpp_chksum(atm%lprec                 )
  write(outunit,100) ' atm%fprec                  ', mpp_chksum(atm%fprec                 )
!  call surf_diff_type_chksum(id, timestep, atm%surf_diff)

end subroutine atmos_data_type_chksum

! </SUBROUTINE>


  subroutine alloc_atmos_data_type (nlon, nlat, ntprog, Atmos)
   integer, intent(in) :: nlon, nlat, ntprog
   type(atmos_data_type), intent(inout) :: Atmos
    allocate ( Atmos % lon_bnd  (nlon+1,nlat+1), &
               Atmos % lat_bnd  (nlon+1,nlat+1), &
               Atmos % lon      (nlon,nlat), &
               Atmos % lat      (nlon,nlat), &
               Atmos % t_bot    (nlon,nlat), &
               Atmos % tr_bot   (nlon,nlat, ntprog), &
               Atmos % z_bot    (nlon,nlat), &
               Atmos % p_bot    (nlon,nlat), &
               Atmos % u_bot    (nlon,nlat), &
               Atmos % v_bot    (nlon,nlat), &
               Atmos % p_surf   (nlon,nlat), &
               Atmos % slp      (nlon,nlat), &
               Atmos % gust     (nlon,nlat), &
               Atmos % flux_sw  (nlon,nlat), &
               Atmos % flux_sw_dir (nlon,nlat), &
               Atmos % flux_sw_dif (nlon,nlat), &
               Atmos % flux_sw_down_vis_dir (nlon,nlat), &
               Atmos % flux_sw_down_vis_dif (nlon,nlat), &
               Atmos % flux_sw_down_total_dir (nlon,nlat), &
               Atmos % flux_sw_down_total_dif (nlon,nlat), &
               Atmos % flux_sw_vis (nlon,nlat), &
               Atmos % flux_sw_vis_dir (nlon,nlat), &
               Atmos % flux_sw_vis_dif(nlon,nlat), &
               Atmos % flux_lw  (nlon,nlat), &
               Atmos % coszen   (nlon,nlat), &
               Atmos % lprec    (nlon,nlat), &
               Atmos % fprec    (nlon,nlat)  )

    Atmos % flux_sw                 = 0.0
    Atmos % flux_lw                 = 0.0
    Atmos % flux_sw_dir             = 0.0
    Atmos % flux_sw_dif             = 0.0
    Atmos % flux_sw_down_vis_dir    = 0.0
    Atmos % flux_sw_down_vis_dif    = 0.0
    Atmos % flux_sw_down_total_dir  = 0.0
    Atmos % flux_sw_down_total_dif  = 0.0
    Atmos % flux_sw_vis             = 0.0
    Atmos % flux_sw_vis_dir         = 0.0
    Atmos % flux_sw_vis_dif         = 0.0
    Atmos % coszen                  = 0.0

  end subroutine alloc_atmos_data_type

  subroutine dealloc_atmos_data_type (Atmos)
   type(atmos_data_type), intent(inout) :: Atmos
    deallocate (Atmos%lon_bnd,                &
                Atmos%lat_bnd,                &
                Atmos%lon,                    &
                Atmos%lat,                    &
                Atmos%t_bot,                  &
                Atmos%tr_bot,                 &
                Atmos%z_bot,                  &
                Atmos%p_bot,                  &
                Atmos%u_bot,                  &
                Atmos%v_bot,                  &
                Atmos%p_surf,                 &
                Atmos%slp,                    &
                Atmos%gust,                   &
                Atmos%flux_sw,                &
                Atmos%flux_sw_dir,            &
                Atmos%flux_sw_dif,            &
                Atmos%flux_sw_down_vis_dir,   &
                Atmos%flux_sw_down_vis_dif,   &
                Atmos%flux_sw_down_total_dir, &
                Atmos%flux_sw_down_total_dif, &
                Atmos%flux_sw_vis,            &
                Atmos%flux_sw_vis_dir,        &
                Atmos%flux_sw_vis_dif,        &
                Atmos%flux_lw,                &
                Atmos%coszen,                 &
                Atmos%lprec,                  &
                Atmos%fprec  )
  end subroutine dealloc_atmos_data_type

end module atmos_model_mod
