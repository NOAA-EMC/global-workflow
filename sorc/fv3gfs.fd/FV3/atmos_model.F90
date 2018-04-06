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
use mpp_mod,            only: mpp_get_current_pelist_name
#ifdef INTERNAL_FILE_NML
use mpp_mod,            only: input_nml_file
#else
use fms_mod,            only: open_namelist_file
#endif
use fms_mod,            only: file_exist, error_mesg
use fms_mod,            only: close_file, write_version_number, stdlog, stdout
use fms_mod,            only: clock_flag_default
use fms_mod,            only: check_nml_error
use diag_manager_mod,   only: diag_send_complete_instant
use time_manager_mod,   only: time_type, get_time, get_date, &
                              operator(+), operator(-)
use field_manager_mod,  only: MODEL_ATMOS
use tracer_manager_mod, only: get_number_tracers, get_tracer_names
use xgrid_mod,          only: grid_box_type
use atmosphere_mod,     only: atmosphere_init
use atmosphere_mod,     only: atmosphere_restart
use atmosphere_mod,     only: atmosphere_end
use atmosphere_mod,     only: atmosphere_state_update
use atmosphere_mod,     only: atmos_phys_driver_statein
use atmosphere_mod,     only: atmosphere_control_data
use atmosphere_mod,     only: atmosphere_resolution, atmosphere_domain
use atmosphere_mod,     only: atmosphere_grid_bdry, atmosphere_grid_ctr
use atmosphere_mod,     only: atmosphere_dynamics, atmosphere_diag_axes
use atmosphere_mod,     only: atmosphere_etalvls, atmosphere_hgt
!rab use atmosphere_mod,     only: atmosphere_tracer_postinit
use atmosphere_mod,     only: atmosphere_diss_est, atmosphere_nggps_diag
use atmosphere_mod,     only: atmosphere_scalar_field_halo
use atmosphere_mod,     only: set_atmosphere_pelist
use atmosphere_mod,     only: Atm, mytile
use block_control_mod,  only: block_control_type, define_blocks_packed
use IPD_typedefs,       only: IPD_init_type, IPD_control_type, &
                              IPD_data_type, IPD_diag_type,    &
                              IPD_restart_type, kind_phys
use IPD_driver,         only: IPD_initialize, IPD_setup_step,  &
                              IPD_radiation_step,              &
                              IPD_physics_step1,               &
                              IPD_physics_step2
use FV3GFS_io_mod,      only: FV3GFS_restart_read, FV3GFS_restart_write, &
                              FV3GFS_IPD_checksum,                       &
                              gfdl_diag_register, gfdl_diag_output
use fv_iau_mod, only: iau_external_data_type,getiauforcing,iau_initialize

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
     real,                 pointer, dimension(:,:) :: lon_bnd  => null() ! local longitude axis grid box corners in radians.
     real,                 pointer, dimension(:,:) :: lat_bnd  => null() ! local latitude axis grid box corners in radians.
     real(kind=kind_phys), pointer, dimension(:,:) :: lon      => null() ! local longitude axis grid box centers in radians.
     real(kind=kind_phys), pointer, dimension(:,:) :: lat      => null() ! local latitude axis grid box centers in radians.
     type (time_type)              :: Time               ! current time
     type (time_type)              :: Time_step          ! atmospheric time step.
     type (time_type)              :: Time_init          ! reference time.
     integer, pointer              :: pelist(:) =>null() ! pelist where atmosphere is running.
     logical                       :: pe                 ! current pe.
     type(grid_box_type)           :: grid               ! hold grid information needed for 2nd order conservative flux exchange 
                                                         ! to calculate gradient on cubic sphere grid.
     integer                       :: layout(2)          ! computer task laytout
     real(kind=8), pointer, dimension(:)           :: ak, bk
     real(kind=kind_phys), pointer, dimension(:,:) :: dx, dy
     real(kind=8), pointer, dimension(:,:)         :: area
     real(kind=8), pointer, dimension(:,:,:)       :: layer_hgt, level_hgt
 end type atmos_data_type
!</PUBLICTYPE >

integer :: fv3Clock, getClock, updClock, setupClock, radClock, physClock

!-----------------------------------------------------------------------
integer :: blocksize    = 1
logical :: chksum_debug = .false.
logical :: dycore_only  = .false.
logical :: debug        = .false.
logical :: sync         = .false.
integer, parameter     :: maxhr = 4096
real, dimension(maxhr) :: fdiag = 0.
real                   :: fhmax=240.0, fhmaxhf=120.0, fhout=3.0, fhouthf=1.0
namelist /atmos_model_nml/ blocksize, chksum_debug, dycore_only, debug, sync, fdiag, fhmax, fhmaxhf, fhout, fhouthf
type (time_type) :: diag_time

!--- concurrent and decoupled radiation and physics variables
!----------------
!  IPD containers
!----------------
type(IPD_control_type)              :: IPD_Control
type(IPD_data_type),    allocatable :: IPD_Data(:)  ! number of blocks
type(IPD_diag_type)                 :: IPD_Diag(250)
type(IPD_restart_type)              :: IPD_Restart

! IAU container
type(iau_external_data_type)        :: IAU_Data ! number of blocks

!-----------------
!  Block container
!-----------------
type (block_control_type), target   :: Atm_block

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
    integer :: nb, jdat(8)

    if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "statein driver"
!--- get atmospheric state from the dynamic core
    call set_atmosphere_pelist()
    call mpp_clock_begin(getClock)
    if (IPD_control%do_skeb) call atmosphere_diss_est (IPD_control%skeb_npass) !  do smoothing for SKEB
    call atmos_phys_driver_statein (IPD_data, Atm_block)
    call mpp_clock_end(getClock)

!--- if dycore only run, set up the dummy physics output state as the input state
    if (dycore_only) then
      do nb = 1,Atm_block%nblks
        IPD_Data(nb)%Stateout%gu0 = IPD_Data(nb)%Statein%ugrs
        IPD_Data(nb)%Stateout%gv0 = IPD_Data(nb)%Statein%vgrs
        IPD_Data(nb)%Stateout%gt0 = IPD_Data(nb)%Statein%tgrs
        IPD_Data(nb)%Stateout%gq0 = IPD_Data(nb)%Statein%qgrs
      enddo
    else
      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "setup step"

!--- update IPD_Control%jdat(8)
      jdat(:) = 0
      call get_date (Atmos%Time, jdat(1), jdat(2), jdat(3),  &
                                 jdat(5), jdat(6), jdat(7))
      IPD_Control%jdat(:) = jdat(:)
!--- execute the IPD atmospheric setup step
      call mpp_clock_begin(setupClock)
      call IPD_setup_step (IPD_Control, IPD_Data, IPD_Diag, IPD_Restart)
      call mpp_clock_end(setupClock)

      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "radiation driver"

!--- execute the IPD atmospheric radiation subcomponent (RRTM)

      call mpp_clock_begin(radClock)
!$OMP parallel do default (none)       &
!$OMP            schedule (dynamic,1), &
!$OMP            shared   (Atm_block, IPD_Control, IPD_Data, IPD_Diag, IPD_Restart) &
!$OMP            private  (nb)
      do nb = 1,Atm_block%nblks
        call IPD_radiation_step (IPD_Control, IPD_Data(nb), IPD_Diag, IPD_Restart)
      enddo
      call mpp_clock_end(radClock)

      if (chksum_debug) then
        if (mpp_pe() == mpp_root_pe()) print *,'RADIATION STEP  ', IPD_Control%kdt, IPD_Control%fhour
        call FV3GFS_IPD_checksum(IPD_Control, IPD_Data, Atm_block)
      endif

      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "physics driver"

!--- execute the IPD atmospheric physics step1 subcomponent (main physics driver)

      call mpp_clock_begin(physClock)
!$OMP parallel do default (none) &
!$OMP            schedule (dynamic,1), &
!$OMP            shared   (Atm_block, IPD_Control, IPD_Data, IPD_Diag, IPD_Restart) &
!$OMP            private  (nb)
      do nb = 1,Atm_block%nblks
        call IPD_physics_step1 (IPD_Control, IPD_Data(nb), IPD_Diag, IPD_Restart)
      enddo
      call mpp_clock_end(physClock)

      if (chksum_debug) then
        if (mpp_pe() == mpp_root_pe()) print *,'PHYSICS STEP1   ', IPD_Control%kdt, IPD_Control%fhour
        call FV3GFS_IPD_checksum(IPD_Control, IPD_Data, Atm_block)
      endif

      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "stochastic physics driver"

!--- execute the IPD atmospheric physics step2 subcomponent (stochastic physics driver)

      call mpp_clock_begin(physClock)
!$OMP parallel do default (none) &
!$OMP            schedule (dynamic,1), &
!$OMP            shared   (Atm_block, IPD_Control, IPD_Data, IPD_Diag, IPD_Restart) &
!$OMP            private  (nb)
      do nb = 1,Atm_block%nblks
        call IPD_physics_step2 (IPD_Control, IPD_Data(nb), IPD_Diag, IPD_Restart)
      enddo
      call mpp_clock_end(physClock)

      if (chksum_debug) then
        if (mpp_pe() == mpp_root_pe()) print *,'PHYSICS STEP2   ', IPD_Control%kdt, IPD_Control%fhour
        call FV3GFS_IPD_checksum(IPD_Control, IPD_Data, Atm_block)
      endif
      call getiauforcing(IPD_Control,IAU_data)
      if (mpp_pe() == mpp_root_pe() .and. debug) write(6,*) "end of radiation and physics step"
    endif

!-----------------------------------------------------------------------
 end subroutine update_atmos_radiation_physics
! </SUBROUTINE>


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
  real, allocatable    :: q(:,:,:,:), p_half(:,:,:)
  character(len=80)    :: control
  character(len=64)    :: filename, filename2, pelist_name
  character(len=132)   :: text
  logical              :: p_hydro, hydro, fexist
  logical, save        :: block_message = .true.
  type(IPD_init_type)  :: Init_parm
  integer              :: bdat(8), cdat(8)
  integer              :: ntracers, maxhf, maxh
  character(len=32), allocatable, target :: tracer_names(:)
!-----------------------------------------------------------------------

!---- set the atmospheric model time ------

   Atmos % Time_init = Time_init
   Atmos % Time      = Time
   Atmos % Time_step = Time_step
   call get_time (Atmos % Time_step, sec)
   dt_phys = real(sec)      ! integer seconds

   logunit = stdlog()

!-----------------------------------------------------------------------
! initialize atmospheric model -----

!---------- initialize atmospheric dynamics -------
   call atmosphere_init (Atmos%Time_init, Atmos%Time, Atmos%Time_step,&
                         Atmos%grid, Atmos%area)

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
   call alloc_atmos_data_type (nlon, nlat, Atmos)
   call atmosphere_domain (Atmos%domain, Atmos%layout)
   call atmosphere_diag_axes (Atmos%axes)
   call atmosphere_etalvls (Atmos%ak, Atmos%bk, flip=.true.)
   call atmosphere_grid_bdry (Atmos%lon_bnd, Atmos%lat_bnd, global=.false.)
   call atmosphere_grid_ctr (Atmos%lon, Atmos%lat)
   call atmosphere_hgt (Atmos%layer_hgt, 'layer', relative=.false., flip=.true.)
   call atmosphere_hgt (Atmos%level_hgt, 'level', relative=.false., flip=.true.)

!-----------------------------------------------------------------------
!--- before going any further check definitions for 'blocks'
!-----------------------------------------------------------------------
   call atmosphere_control_data (isc, iec, jsc, jec, nlev, p_hydro, hydro)
   call define_blocks_packed ('atmos_model', Atm_block, isc, iec, jsc, jec, nlev, &
                              blocksize, block_message)
   
   allocate(IPD_Data(Atm_block%nblks))

!--- update IPD_Control%jdat(8)
   bdat(:) = 0
   call get_date (Time_init, bdat(1), bdat(2), bdat(3),  &
                             bdat(5), bdat(6), bdat(7))
   cdat(:) = 0
   call get_date (Time,      cdat(1), cdat(2), cdat(3),  &
                             cdat(5), cdat(6), cdat(7))
   call get_number_tracers(MODEL_ATMOS, num_tracers=ntracers)
   allocate (tracer_names(ntracers))
   do i = 1, ntracers
     call get_tracer_names(MODEL_ATMOS, i, tracer_names(i))
   enddo
!--- setup IPD Init_parm
   Init_parm%me              =  mpp_pe()
   Init_parm%master          =  mpp_root_pe()
   Init_parm%isc             =  isc
   Init_parm%jsc             =  jsc
   Init_parm%nx              =  nlon
   Init_parm%ny              =  nlat
   Init_parm%levs            =  nlev
   Init_parm%cnx             =  mlon
   Init_parm%cny             =  mlat
   Init_parm%gnx             =  Init_parm%cnx*4
   Init_parm%gny             =  Init_parm%cny*2
   Init_parm%nlunit          =  9999
   Init_parm%logunit         =  logunit
   Init_parm%bdat(:)         =  bdat(:)
   Init_parm%cdat(:)         =  cdat(:)
   Init_parm%dt_dycore       =  dt_phys
   Init_parm%dt_phys         =  dt_phys
   Init_parm%blksz           => Atm_block%blksz
   Init_parm%ak              => Atmos%ak
   Init_parm%bk              => Atmos%bk
   Init_parm%xlon            => Atmos%lon
   Init_parm%xlat            => Atmos%lat
   Init_parm%area            => Atmos%area
   Init_parm%tracer_names    => tracer_names

#ifdef INTERNAL_FILE_NML
   Init_parm%input_nml_file  => input_nml_file
   Init_parm%fn_nml='using internal file'
#else
   pelist_name=mpp_get_current_pelist_name()
   Init_parm%fn_nml='input_'//trim(pelist_name)//'.nml'
   inquire(FILE=Init_parm%fn_nml, EXIST=fexist)
   if (.not. fexist ) then
      Init_parm%fn_nml='input.nml'
   endif
#endif

   call IPD_initialize (IPD_Control, IPD_Data, IPD_Diag, IPD_Restart, Init_parm)

   Atm(mytile)%flagstruct%do_skeb = IPD_Control%do_skeb

!  initialize the IAU module
   call iau_initialize (IPD_Control,IAU_data,Init_parm)

   Init_parm%blksz           => null()
   Init_parm%ak              => null()
   Init_parm%bk              => null()
   Init_parm%xlon            => null()
   Init_parm%xlat            => null()
   Init_parm%area            => null()
   Init_parm%tracer_names    => null()
   deallocate (tracer_names)

   !--- update tracers in FV3 with any initialized during the physics/radiation init phase
!rab   call atmosphere_tracer_postinit (IPD_Data, Atm_block)

   call atmosphere_nggps_diag (Time, init=.true.)
   call gfdl_diag_register (Time, IPD_Data(:)%Sfcprop, IPD_Data(:)%Cldprop, IPD_Data(:)%IntDiag, IPD_Data(:)%grid, Atm_block, IPD_Control, Atmos%axes)
   call FV3GFS_restart_read (IPD_Data, IPD_Restart, Atm_block, IPD_Control, Atmos%domain)

   !--- set the initial diagnostic timestamp
   diag_time = Time

   !---- print version number to logfile ----

   call write_version_number ( version, tagname )
   !--- write the namelist to a log file
   if (mpp_pe() == mpp_root_pe()) then
      unit = stdlog( )
      write (unit, nml=atmos_model_nml)
      call close_file (unit)
   endif

   !--- get fdiag
#ifdef GFS_PHYS
!--- check fdiag to see if it is an interval or a list
   if (nint(fdiag(2)) == 0) then
     if (fhmaxhf > 0) then
       maxhf = fhmaxhf / fhouthf
       maxh  = maxhf + (fhmax-fhmaxhf) / fhout
       fdiag(1) = fhouthf
       do i=2,maxhf
        fdiag(i) = fdiag(i-1) + fhouthf
       enddo
       do i=maxhf+1,maxh
         fdiag(i) = fdiag(i-1) + fhout
       enddo
     else
       maxh  = fhmax / fhout
       fdiag(1) = fdiag(1) + fhout
       do i = 2, maxh
         fdiag(i) = fdiag(i-1) + fhout
       enddo
     endif
   endif
   if (mpp_pe() == mpp_root_pe()) write(6,*) "---fdiag",fdiag(1:40)
#endif

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
  integer :: isec,seconds
  real(kind=kind_phys) :: time_int, time_intfull

    call set_atmosphere_pelist()
    call mpp_clock_begin(fv3Clock)
    call mpp_clock_begin(updClock)
    call atmosphere_state_update (Atmos%Time, IPD_Data, IAU_Data, Atm_block)
    call mpp_clock_end(updClock)
    call mpp_clock_end(fv3Clock)

    if (chksum_debug) then
      if (mpp_pe() == mpp_root_pe()) print *,'UPDATE STATE    ', IPD_Control%kdt, IPD_Control%fhour
      call FV3GFS_IPD_checksum(IPD_Control, IPD_Data, Atm_block)
    endif

!------ advance time ------
    Atmos % Time = Atmos % Time + Atmos % Time_step

    call get_time (Atmos%Time - diag_time, isec)
    call get_time (Atmos%Time - Atmos%Time_init, seconds)
    if (ANY(nint(fdiag(:)*3600.0) == seconds) .or. (IPD_Control%kdt == 1) ) then
      if (mpp_pe() == mpp_root_pe()) write(6,*) "---isec,seconds",isec,seconds
      time_int = real(isec)
      time_intfull = real(seconds)
      if (mpp_pe() == mpp_root_pe()) write(6,*) ' gfs diags time since last bucket empty: ',time_int/3600.,'hrs'
      call atmosphere_nggps_diag(Atmos%Time)
      call gfdl_diag_output(Atmos%Time, Atm_block, IPD_Control%nx, IPD_Control%ny, &
                            IPD_Control%levs, 1, 1, 1.d0, time_int, time_intfull)
      if (mod(isec,3600*nint(IPD_Control%fhzero)) == 0) diag_time = Atmos%Time
      call diag_send_complete_instant (Atmos%Time)
    endif

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
    call FV3GFS_restart_write (IPD_Data, IPD_Restart, Atm_block, &
                               IPD_Control, Atmos%domain)

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
    call FV3GFS_restart_write (IPD_Data, IPD_Restart, Atm_block, &
                               IPD_Control, Atmos%domain, timestamp)

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

end subroutine atmos_data_type_chksum

! </SUBROUTINE>

  subroutine alloc_atmos_data_type (nlon, nlat, Atmos)
   integer, intent(in) :: nlon, nlat
   type(atmos_data_type), intent(inout) :: Atmos
    allocate ( Atmos % lon_bnd  (nlon+1,nlat+1), &
               Atmos % lat_bnd  (nlon+1,nlat+1), &
               Atmos % lon      (nlon,nlat),     &
               Atmos % lat      (nlon,nlat)      )

  end subroutine alloc_atmos_data_type

  subroutine dealloc_atmos_data_type (Atmos)
   type(atmos_data_type), intent(inout) :: Atmos
    deallocate (Atmos%lon_bnd, &
                Atmos%lat_bnd, &
                Atmos%lon,     &
                Atmos%lat      )
  end subroutine dealloc_atmos_data_type

end module atmos_model_mod
