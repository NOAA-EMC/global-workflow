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

!>@brief The module 'fv_nggps_diags' computes output diagnostics entirely
!! on 3D pressure levels
!>@details The module is designed for applications that process the full
!!3D fields through the NCEP post-processor.

module fv_nggps_diags_mod

! <table>
! <tr>
!     <th>Module Name</th>
!     <th>Functions Included</th>
!   </tr>
!   <tr>
!     <td>constants_mod</td>
!     <td>kappa, grav, rdgas</td>
!   </tr>
!   <tr>
!     <td>diag_manager_mod</td>
!     <td>register_diag_field, send_data</td>
!   </tr>
!   <tr>
!     <td>field_manager_mod</td>
!     <td>MODEL_ATMOS</td>
!   </tr>
!   <tr>
!     <td>fms_io_mod</td>
!     <td>set_domain, nullify_domain</td>
!   </tr>
!   <tr>
!     <td>fv_arrays_mod</td>
!     <td>fv_atmos_type</td>
!   </tr>
!   <tr>
!     <td>fv_diagnostics_mod</td>
!     <td>range_check</td>
!   </tr>
!   <tr>
!     <td>mpp_mod</td>
!     <td>mpp_pe, mpp_root_pe</td>
!   </tr>
!   <tr>
!     <td>tracer_manager_mod</td>
!     <td>get_tracer_names, get_number_tracers, get_tracer_index</td>
!   </tr>
! </table>

 use mpp_mod,            only: mpp_pe, mpp_root_pe
 use constants_mod,      only: grav, rdgas
 use fms_io_mod,         only: set_domain, nullify_domain
 use time_manager_mod,   only: time_type
 use diag_manager_mod,   only: register_diag_field, send_data
 use diag_axis_mod,      only: get_axis_global_length, get_diag_axis, get_diag_axis_name
 use diag_data_mod,      only: output_fields, max_output_fields
 use diag_util_mod,      only: find_input_field
 use tracer_manager_mod, only: get_tracer_names, get_number_tracers, get_tracer_index
 use field_manager_mod,  only: MODEL_ATMOS
 use fv_diagnostics_mod, only: range_check
 use fv_arrays_mod,      only: fv_atmos_type
 use mpp_domains_mod,    only: domain1d, domainUG

 implicit none
 private

 real, parameter:: missing_value = -1.e10
 real, parameter:: stndrd_atmos_ps = 101325.
 real, parameter:: stndrd_atmos_lapse = 0.0065

 logical master
 integer :: id_ua, id_va, id_pt, id_delp, id_pfhy, id_pfnh 
 integer :: id_w, id_delz, id_diss, id_ps, id_hs
 integer :: kstt_ua, kstt_va, kstt_pt, kstt_delp, kstt_pfhy
 integer :: kstt_pfnh, kstt_w, kstt_delz, kstt_diss, kstt_ps,kstt_hs
 integer :: kend_ua, kend_va, kend_pt, kend_delp, kend_pfhy
 integer :: kend_pfnh, kend_w, kend_delz, kend_diss, kend_ps,kend_hs
 integer :: kstt_windvect, kend_windvect
 integer :: isco, ieco, jsco, jeco, npzo, ncnsto
 integer :: nlevs
 logical :: hydrostatico
 integer, allocatable :: id_tracer(:), all_axes(:)
 integer, allocatable :: kstt_tracer(:), kend_tracer(:)
 real,    allocatable :: ak(:), bk(:)
 character(20),allocatable :: axis_name(:),axis_name_vert(:)

 logical :: module_is_initialized=.false.
 logical :: use_wrtgridcomp_output=.false.
 integer :: sphum, liq_wat, ice_wat       !< GFDL physics
 integer :: rainwat, snowwat, graupel
 real :: vrange(2) = (/ -330.,  330. /)  !< winds
 real :: wrange(2) = (/ -100.,  100. /)  !< vertical wind
 real :: trange(2) = (/  100.,  350. /)  !< temperature
 real :: skrange(2) = (/ -10000000.0,  10000000.0 /)  !< dissipation estimate for SKEB

! file name
 character(len=64) :: file_name = 'gfs_dyn'

! tracers
 character(len=128)   :: tname
 character(len=256)   :: tlongname, tunits

! wrtout buffer
 real(4), dimension(:,:,:), allocatable, target   :: buffer_dyn
 real(4), dimension(:,:,:,:), allocatable, target :: windvect
 real(4), dimension(:,:), allocatable, target     :: psurf
 real, dimension(:,:), allocatable                :: lon, lat

 public :: fv_nggps_diag_init, fv_nggps_diag
#ifdef use_WRTCOMP
 public :: fv_dyn_bundle_setup
#endif

contains

 subroutine fv_nggps_diag_init(Atm, axes, Time)
    type(fv_atmos_type), intent(inout), target :: Atm(:)
    integer, intent(in)         :: axes(4)
    type(time_type), intent(in) :: Time

    integer :: n, i, j, nz

    n = 1
    ncnsto = Atm(1)%ncnst
    npzo   = Atm(1)%npz
    isco = Atm(n)%bd%isc; ieco = Atm(n)%bd%iec
    jsco = Atm(n)%bd%jsc; jeco = Atm(n)%bd%jec
    hydrostatico = Atm(n)%flagstruct%hydrostatic

    call set_domain(Atm(1)%domain)  ! Set domain so that diag_manager can access tile information

    sphum   = get_tracer_index (MODEL_ATMOS, 'sphum')
    liq_wat = get_tracer_index (MODEL_ATMOS, 'liq_wat')
    ice_wat = get_tracer_index (MODEL_ATMOS, 'ice_wat')

    rainwat = get_tracer_index (MODEL_ATMOS, 'rainwat')
    snowwat = get_tracer_index (MODEL_ATMOS, 'snowwat')
    graupel = get_tracer_index (MODEL_ATMOS, 'graupel')

!--------------------------------------------------------------
! Register main prognostic fields: ps, (u,v), t, omega (dp/dt)
!--------------------------------------------------------------
    allocate(id_tracer(ncnsto))
    allocate(kstt_tracer(ncnsto), kend_tracer(ncnsto))
    id_tracer(:) = 0
    kstt_tracer(:) = 0
    kend_tracer(:) = 0

    if (Atm(n)%flagstruct%write_3d_diags) then
!-------------------
! A grid winds (lat-lon)
!-------------------
       id_ua = register_diag_field ( trim(file_name), 'ucomp', axes(1:3), Time,        &
            'zonal wind', 'm/sec', missing_value=missing_value, range=vrange )
       if (id_ua>0) then
         kstt_ua = 1; kend_ua = npzo
         nlevs = nlevs + npzo
       endif

       id_va = register_diag_field ( trim(file_name), 'vcomp', axes(1:3), Time,        &
            'meridional wind', 'm/sec', missing_value=missing_value, range=vrange)
       if (id_va>0) then
         kstt_va = nlevs+1; kend_va = nlevs+npzo
         nlevs = nlevs + npzo
       endif

       if(id_ua>0 .and. id_va>0) then
         kstt_windvect = 1;  kend_windvect = npzo
         allocate(windvect(3,isco:ieco,jsco:jeco,npzo))
         windvect = 0.
       endif

       if( Atm(n)%flagstruct%hydrostatic ) then 
          id_pfhy = register_diag_field ( trim(file_name), 'pfhy', axes(1:3), Time,        &
               'hydrostatic pressure', 'pa', missing_value=missing_value )
          if (id_pfhy>0) then
             kstt_pfhy = nlevs+1; kend_pfhy = nlevs+npzo
             nlevs = nlevs + npzo
          endif
       else
          id_pfnh = register_diag_field ( trim(file_name), 'pfnh', axes(1:3), Time,        &
               'non-hydrostatic pressure', 'pa', missing_value=missing_value )
          if (id_pfnh>0) then
             kstt_pfnh = nlevs+1; kend_pfnh = nlevs+npzo
             nlevs = nlevs + npzo
          endif
          id_w = register_diag_field ( trim(file_name), 'w', axes(1:3), Time,        &
               'vertical wind', 'm/sec', missing_value=missing_value, range=wrange )
          if (id_w>0) then
             kstt_w = nlevs+1; kend_w = nlevs+npzo
             nlevs = nlevs + npzo
          endif
          id_delz = register_diag_field ( trim(file_name), 'delz', axes(1:3), Time,        &
               'height thickness', 'm', missing_value=missing_value )
          if (id_delz>0) then
             kstt_delz = nlevs+1; kend_delz = nlevs+npzo
             nlevs = nlevs + npzo
          endif
       endif

       id_pt   = register_diag_field ( trim(file_name), 'temp', axes(1:3), Time,       &
            'temperature', 'K', missing_value=missing_value, range=trange )
       if (id_pt>0) then
          kstt_pt = nlevs+1; kend_pt = nlevs+npzo
          nlevs = nlevs + npzo
       endif

       id_delp = register_diag_field ( trim(file_name), 'delp', axes(1:3), Time,        &
            'pressure thickness', 'pa', missing_value=missing_value )
       if (id_delp>0) then
          kstt_delp = nlevs+1; kend_delp = nlevs+npzo
          nlevs = nlevs + npzo
       endif

       !--- diagnostic output for skeb: dissipation estimate
       id_diss = register_diag_field ( trim(file_name), 'diss_est', axes(1:3), Time,    &
            'dissipation estimate', 'none', missing_value=missing_value, range=skrange )
       if (id_delp>0) then
          kstt_diss = nlevs+1; kend_diss = nlevs+npzo
          nlevs = nlevs + npzo
       endif

!--------------------
! Tracer diagnostics:
!--------------------
       do i=1, ncnsto
         call get_tracer_names ( MODEL_ATMOS, i, tname, tlongname, tunits )
         id_tracer(i) = register_diag_field ( file_name, trim(tname),  &
                                   axes(1:3), Time, trim(tlongname), &
                                   trim(tunits), missing_value=missing_value)
          if (id_tracer(i)>0) then
             kstt_tracer(i) = nlevs+1; kend_tracer(i) = nlevs+npzo
             nlevs = nlevs + npzo
          endif
       enddo
!
       id_ps = register_diag_field ( trim(file_name), 'ps', axes(1:2), Time,    &
           'surface pressure', 'pa',  missing_value=missing_value )
       if( id_ps > 0) then
          kstt_ps = nlevs+1; kend_ps = nlevs+1
          nlevs = nlevs + 1
          allocate(psurf(isco:ieco,jsco:jeco))
       endif
!
       id_hs = register_diag_field ( trim(file_name), 'hs', axes(1:2), Time,    &
           'surface geopotential height', 'gpm',  missing_value=missing_value )
       if( id_hs > 0) then
          kstt_hs = nlevs+1; kend_hs = nlevs+1
          nlevs = nlevs + 1
       endif
!
       nz = size(atm(1)%ak)
       allocate(ak(nz))
       allocate(bk(nz))
       do i=1,nz
         ak(i) = atm(1)%ak(i)
         bk(i) = atm(1)%bk(i)
       enddo
!      print *,'in ngpps diag init, ak=',ak(1:5),' bk=',bk(1:5)

! get lon,lon information
       if(.not.allocated(lon)) then
         allocate(lon(isco:ieco,jsco:jeco))
         do j=jsco,jeco
           do i=isco,ieco
             lon(i,j) = Atm(n)%gridstruct%agrid(i,j,1)
           enddo
         enddo
!        if(mpp_pe()==mpp_root_pe())print *,'in fv_dyn bundle,lon=',lon(isco,jsco),lon(ieco-2:ieco,jeco-2:jeco)*180./3.14157
       endif
       if(.not.allocated(lat)) then
         allocate(lat(isco:ieco,jsco:jeco))
         do j=jsco,jeco
           do i=isco,ieco
             lat(i,j) = Atm(n)%gridstruct%agrid(i,j,2)
           enddo
         enddo
!        if(mpp_pe()==mpp_root_pe())print *,'in fv_dyn bundle,lat=',lat(isco,jsco),lat(ieco-2:ieco,jeco-2:jeco)*180./3.14157
       endif
    endif
!
!------------------------------------
! use wrte grid component for output
!------------------------------------
      use_wrtgridcomp_output = .false.

 end subroutine fv_nggps_diag_init


 subroutine fv_nggps_diag(Atm, zvir, Time)

    type(fv_atmos_type), intent(inout) :: Atm(:)
    real,                intent(in):: zvir
    type(time_type),     intent(in) :: Time

    integer :: i, j, k, n, ngc, nq, itrac
    logical :: bad_range
    real    :: ptop
    real, allocatable :: wk(:,:,:)

    n = 1
    ngc = Atm(n)%ng
    ptop = Atm(n)%ak(1)
    nq = size (Atm(n)%q,4)
    allocate ( wk(isco:ieco,jsco:jeco,npzo) )

    if ( Atm(n)%flagstruct%range_warn ) then
         call range_check('DELP', Atm(n)%delp, isco, ieco, jsco, jeco, ngc, npzo, Atm(n)%gridstruct%agrid,    &
                           0.01*ptop, 200.E2, bad_range)
         call range_check('UA', Atm(n)%ua, isco, ieco, jsco, jeco, ngc, npzo, Atm(n)%gridstruct%agrid,   &
                           -250., 250., bad_range)
         call range_check('VA', Atm(n)%va, isco, ieco, jsco, jeco, ngc, npzo, Atm(n)%gridstruct%agrid,   &
                           -250., 250., bad_range)
         call range_check('TA', Atm(n)%pt, isco, ieco, jsco, jeco, ngc, npzo, Atm(n)%gridstruct%agrid,   &
                           150., 350., bad_range) !DCMIP ICs have very low temperatures
    endif

    !--- A-GRID WINDS
    if ( .not. allocated(buffer_dyn)) allocate(buffer_dyn(isco:ieco,jsco:jeco,nlevs))
    if(id_ua > 0) call store_data(id_ua, Atm(n)%ua(isco:ieco,jsco:jeco,:), Time, kstt_ua, kend_ua)
    
    if(id_va > 0) call store_data(id_va, Atm(n)%va(isco:ieco,jsco:jeco,:), Time, kstt_va, kend_va)

    !--- set up 3D wind vector
    if(id_ua>0 .and. id_va>0) then
      do k=1,npzo
        do j=jsco,jeco
          do i=isco,ieco
            windvect(1,i,j,k) = Atm(n)%ua(i,j,k)*cos(lon(i,j)) - Atm(n)%va(i,j,k)*sin(lat(i,j))*sin(lon(i,j))
            windvect(2,i,j,k) = Atm(n)%ua(i,j,k)*sin(lon(i,j)) + Atm(n)%va(i,j,k)*sin(lat(i,j))*cos(lon(i,j))
            windvect(3,i,j,k) =                                  Atm(n)%va(i,j,k)*cos(lat(i,j))
          enddo
        enddo
      enddo
    endif

    !--- W (non-hydrostatic)
    if ( .not.Atm(n)%flagstruct%hydrostatic .and. id_w>0  ) then
       call store_data(id_w, Atm(n)%w(isco:ieco,jsco:jeco,:), Time, kstt_w, kend_w)
    endif

    !--- TEMPERATURE
    if(id_pt   > 0) call store_data(id_pt, Atm(n)%pt(isco:ieco,jsco:jeco,:), Time, kstt_pt, kend_pt)

    !--- TRACERS
    do itrac=1, ncnsto
      call get_tracer_names (MODEL_ATMOS, itrac, tname)
      if (id_tracer(itrac) > 0 .and. itrac.gt.nq) then
        call store_data (id_tracer(itrac), Atm(n)%qdiag(isco:ieco,jsco:jeco,:,itrac), Time,  &
                         kstt_tracer(itrac),kend_tracer(itrac) )
      else
        call store_data (id_tracer(itrac), Atm(n)%q(isco:ieco,jsco:jeco,:,itrac), Time,      &
                         kstt_tracer(itrac),kend_tracer(itrac) )
      endif
    enddo

    !--- DELZ (non-hydrostatic)
    if((.not. Atm(n)%flagstruct%hydrostatic) .and. id_delz > 0) then
       do k=1,npzo
         do j=jsco,jeco
           do i=isco,ieco
             wk(i,j,k) = -Atm(n)%delz(i,j,k)
           enddo
         enddo
       enddo
       call store_data(id_delz, wk, Time, kstt_delz, kend_delz)
    endif

    !--- PRESSURE (hydrostatic)
    if( Atm(n)%flagstruct%hydrostatic .and. id_pfhy > 0 ) then
       do k=1,npzo
         do j=jsco,jeco
           do i=isco,ieco         
             wk(i,j,k) = 0.5 *(Atm(n)%pe(i,k,j)+Atm(n)%pe(i,k+1,j))
           enddo
         enddo
       enddo
       call store_data(id_pfhy, wk, Time, kstt_pfhy, kend_pfhy)
    endif

#ifdef GFS_PHYS
    !--- DELP
    if(id_delp > 0 .or. ((.not. Atm(n)%flagstruct%hydrostatic) .and. id_pfnh > 0)) then
       do k=1,npzo
         do j=jsco,jeco
           do i=isco,ieco         
             wk(i,j,k) = Atm(n)%delp(i,j,k)*(1.-sum(Atm(n)%q(i,j,k,2:Atm(n)%flagstruct%nwat)))
           enddo
         enddo
       enddo
       call store_data(id_delp, wk, Time, kstt_delp, kend_delp)
    endif

    !--- Surface Pressure (PS)
    ! Re-compute pressure (dry_mass + water_vapor) surface pressure
    if(id_ps > 0) then
      do k=1,npzo
        do j=jsco,jeco
          do i=isco,ieco
            wk(i,j,k) = Atm(n)%delp(i,j,k)*(1.-sum(Atm(n)%q(i,j,k,2:Atm(n)%flagstruct%nwat)))
          enddo
        enddo
      enddo
      do j=jsco,jeco
        do i=isco,ieco
           psurf(i,j) = ptop
           do k=npzo,1,-1
             psurf(i,j)  = psurf(i,j) + wk(i,j,k)
           enddo
        enddo
      enddo
    endif

    !--- PRESSURE (non-hydrostatic)
    if( (.not. Atm(n)%flagstruct%hydrostatic) .and. id_pfnh > 0) then
       do k=1,npzo
         do j=jsco,jeco
           do i=isco,ieco         
             wk(i,j,k) = -wk(i,j,k)/(Atm(n)%delz(i,j,k)*grav)*rdgas*          &
                         Atm(n)%pt(i,j,k)*(1.+zvir*Atm(n)%q(i,j,k,sphum))     
           enddo
         enddo
       enddo
       call store_data(id_pfnh, wk, Time, kstt_pfnh, kend_pfnh)
    endif
#else
    !--- DELP
    if(id_delp > 0) call store_data(id_delp, Atm(n)%delp(isco:ieco,jsco:jeco,:), Time, kstt_delp)

    !--- Surface Pressure (PS)
    if( id_ps > 0) then
      do j=jsco,jeco
        do i=isco,ieco
          psurf(i,j) = Atm(n)%ps(i,j)
        enddo
      enddo
    endif

    !--- PRESSURE (non-hydrostatic)
    if( (.not. Atm(n)%flagstruct%hydrostatic) .and. id_pfnh > 0) then
       do k=1,npzo
         do j=jsco,jeco
           do i=isco,ieco
             wk(i,j,k) = -Atm(n)%delp(i,j,k)/(Atm(n)%delz(i,j,k)*grav)*rdgas*          &
                          Atm(n)%pt(i,j,k)*(1.+zvir*Atm(n)%q(i,j,k,sphum))
           enddo
         enddo
       enddo
       call store_data(id_pfnh, wk, Time, kstt_pfnh, kend_pfnh)
    endif
#endif

    !--- DISS_EST (skeb: dissipation estimate)
    if(id_diss > 0) call store_data(id_diss, Atm(n)%diss_est(isco:ieco,jsco:jeco,:), Time, kstt_diss, kend_diss)
!
    if(id_ps > 0) then
      if(  use_wrtgridcomp_output ) then
        do j=jsco,jeco
          do i=isco,ieco
            wk(i,j,1) = (psurf(i,j)/stndrd_atmos_ps)**(rdgas/grav*stndrd_atmos_lapse)
          enddo
        enddo
      else
        do j=jsco,jeco
          do i=isco,ieco
            wk(i,j,1) = psurf(i,j)
          enddo
        enddo
      endif
!      print *,'in comput ps, i=',isco,'j=',jsco,'psurf=',psurf(isco,jsco),'stndrd_atmos_ps=',stndrd_atmos_ps, &
!       'rdgas=',rdgas,'grav=',grav,'stndrd_atmos_lapse=',stndrd_atmos_lapse,rdgas/grav*stndrd_atmos_lapse
      call store_data(id_ps, wk, Time, kstt_ps, kend_ps)
    endif
    
    if( id_hs > 0) then
      do j=jsco,jeco
        do i=isco,ieco
          wk(i,j,1) = Atm(n)%phis(i,j)/grav
        enddo
      enddo
      call store_data(id_hs, wk, Time, kstt_hs, kend_hs)
    endif

    deallocate ( wk )

    call nullify_domain()

 end subroutine fv_nggps_diag

 subroutine store_data(id, work, Time, nstt, nend)
   integer, intent(in)         :: id
   integer, intent(in)         :: nstt, nend
   real, intent(in)            :: work(isco:ieco,jsco:jeco,nend-nstt+1)
   type(time_type), intent(in) :: Time
!
   integer k,j,i,kb
   logical :: used
!
   if( id > 0 ) then
     if( use_wrtgridcomp_output ) then
       do k=1,nend-nstt+1
         do j= jsco,jeco
           do i=isco,ieco
             kb = k + nstt - 1
             buffer_dyn(i,j,kb) = work(i,j,k)
           enddo
         enddo
       enddo
     else
       used = send_data(id, work, Time)
     endif
   endif

 end subroutine store_data

#ifdef use_WRTCOMP

 subroutine fv_dyn_bundle_setup(axes, dyn_bundle, fcst_grid, quilting )
!
!-------------------------------------------------------------
!*** set esmf bundle for dyn output fields
!------------------------------------------------------------
!
   use esmf
   use diag_data_mod, ONLY:  diag_atttype
!
   integer, intent(in)         :: axes(:)
   type(ESMF_FieldBundle),intent(inout)        :: dyn_bundle
   type(ESMF_Grid),intent(inout)               :: fcst_grid
   logical,intent(in)                          :: quilting


!*** local variables
   integer i, j, k, n, rc
   integer num_axes, id, axis_length, direction, edges
   integer num_attributes, num_field_dyn, axis_typ
   character(255) :: units, long_name, cart_name,axis_direct,edgesS
   character(128) :: output_name, output_file, output_file1, dynbdl_name, shydrostatic
   integer currdate(6), idx1
   logical l3Dvector
   type(domain1d) :: Domain
   type(domainuG) :: DomainU
   real,dimension(:),allocatable :: axis_data
   type(diag_atttype),dimension(:),allocatable :: attributes 
   character(2) axis_id

   type(ESMF_Field)                            :: field
!
!jwtest
!   integer :: fieldcount
!   character(128) :: fld_outfilename
!   character(128),dimension(:),allocatable      :: fieldnamelist
!   type(ESMF_Field),dimension(:),allocatable    :: fieldlist
!
!------------------------------------------------------------
!--- use wrte grid component for output
   use_wrtgridcomp_output = quilting

! data type
   if(.not. allocated(buffer_dyn))allocate(buffer_dyn(isco:ieco,jsco:jeco,nlevs))
   buffer_dyn=0.
   num_field_dyn = 0.
!
! set output files
   call ESMF_FieldBundleGet(dyn_bundle, name=dynbdl_name,rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     return  ! bail out
   idx1 = index(dynbdl_name,'_bilinear')
   if(idx1 > 0) then
     output_file = dynbdl_name(1:idx1-1)
   else
     output_file = 'dyn'
   endif
!
!------------------------------------------------------------
!*** add attributes to the bundle such as subdomain limtis,
!*** axes, output time, etc
!------------------------------------------------------------
!
!*** add attributes
   num_axes = size(axes)
   allocate(all_axes(num_axes))
   all_axes(1:num_axes) = axes(1:num_axes)
!   if(mpp_pe()==mpp_root_pe())print *,'in fv_dyn bundle,num_axes=',num_axes, 'axes=',axes
!
!*** add global attributes in the field bundle:
   call ESMF_AttributeAdd(dyn_bundle, convention="NetCDF", purpose="FV3", &
     attrList=(/"hydrostatic", &
                 "ncnsto    ", &
                 "ak        ", &
                 "bk        "/), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     return  ! bail out
   if (hydrostatico ) then
     shydrostatic = 'hydrostatic'
   else
     shydrostatic = 'non-hydrostatic'
   endif
   call ESMF_AttributeSet(dyn_bundle, convention="NetCDF", purpose="FV3", &
     name="hydrostatic", value=trim(shydrostatic), rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     return  ! bail out
!
   call ESMF_AttributeSet(dyn_bundle, convention="NetCDF", purpose="FV3", &
     name="ncnsto", value=ncnsto, rc=rc)
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     return  ! bail out
!
   call ESMF_AttributeSet(dyn_bundle, convention="NetCDF", purpose="FV3", &
     name="ak", valueList=ak, rc=rc)
!    if(mpp_pe()==mpp_root_pe())print *,'in fv_dyn bundle,after add ak, rc=',rc
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     return  ! bail out
!
   call ESMF_AttributeSet(dyn_bundle, convention="NetCDF", purpose="FV3", &
     name="bk", valueList=bk, rc=rc)
!    if(mpp_pe()==mpp_root_pe())print *,'in fv_dyn bundle,after add bk, rc=',rc
   if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
     line=__LINE__, &
     file=__FILE__)) &
     return  ! bail out
!
!*** get axis names
   allocate(axis_name(num_axes))
   do id = 1,num_axes
     call get_diag_axis_name( axes(id), axis_name(id))
   enddo
   if( num_axes>2 ) then
     allocate(axis_name_vert(num_axes-2))
     do id=3,num_axes
       axis_name_vert(id-2) = axis_name(id)
     enddo
!     if(mpp_pe()==mpp_root_pe())print *,'in fv_dyn bundle,axis_name_vert=',axis_name_vert
     call ESMF_AttributeAdd(fcst_grid, convention="NetCDF", purpose="FV3",  &
       attrList=(/"vertical_dim_labels"/), rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
     call ESMF_AttributeSet(fcst_grid, convention="NetCDF", purpose="FV3", &
       name="vertical_dim_labels", valueList=axis_name_vert, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
       line=__LINE__, &
       file=__FILE__)) &
       return  ! bail out
   endif

   do id = 1,num_axes
     axis_length =  get_axis_global_length(axes(id)) 
     allocate(axis_data(axis_length))
     call get_diag_axis( axes(id), axis_name(id), units, long_name, cart_name, &
                         direction, edges, Domain, DomainU, axis_data,         &
                         num_attributes=num_attributes,              &
                         attributes=attributes)
!
     edgesS=''
     do i = 1,num_axes
       if(axes(i) == edges) edgesS=axis_name(i)
     enddo

!     if(mpp_pe()==mpp_root_pe())print *,'in fv_dyn bundle,id=',id,'edges=',edges,rc, &
!       'num_attributes=',num_attributes,'edgesS=',trim(edgesS)
!
! Add vertical dimension Attributes to Grid
     if( id>2 ) then
!      if(mpp_pe()==mpp_root_pe())print *,' in dyn add grid, axis_name=',     &
!         trim(axis_name(id)),'axis_data=',axis_data
      if(trim(edgesS)/='') then
        call ESMF_AttributeAdd(fcst_grid, convention="NetCDF", purpose="FV3",  &
          attrList=(/trim(axis_name(id)),trim(axis_name(id))//":long_name",    &
                    trim(axis_name(id))//":units", trim(axis_name(id))//":cartesian_axis", &
                    trim(axis_name(id))//":positive", trim(axis_name(id))//":edges"/), rc=rc)
      else
        call ESMF_AttributeAdd(fcst_grid, convention="NetCDF", purpose="FV3",  &
          attrList=(/trim(axis_name(id)),trim(axis_name(id))//":long_name",    &
                    trim(axis_name(id))//":units", trim(axis_name(id))//":cartesian_axis", &
                    trim(axis_name(id))//":positive"/), rc=rc)
      endif
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(fcst_grid, convention="NetCDF", purpose="FV3", &
        name=trim(axis_name(id)), valueList=axis_data, rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(fcst_grid, convention="NetCDF", purpose="FV3", &
        name=trim(axis_name(id))//":long_name", value=trim(long_name), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(fcst_grid, convention="NetCDF", purpose="FV3", &
        name=trim(axis_name(id))//":units", value=trim(units), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      call ESMF_AttributeSet(fcst_grid, convention="NetCDF", purpose="FV3", &
        name=trim(axis_name(id))//":cartesian_axis", value=trim(cart_name), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if(direction>0) then
          axis_direct="up"
      else
          axis_direct="down"
      endif
      call ESMF_AttributeSet(fcst_grid, convention="NetCDF", purpose="FV3", &
        name=trim(axis_name(id))//":positive", value=trim(axis_direct), rc=rc)
      if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
        line=__LINE__, &
        file=__FILE__)) &
        return  ! bail out
      if(trim(edgesS)/='') then
        call ESMF_AttributeSet(fcst_grid, convention="NetCDF", purpose="FV3", &
          name=trim(axis_name(id))//":edges", value=trim(edgesS), rc=rc)
        if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
          line=__LINE__, &
          file=__FILE__)) &
          return  ! bail out
      endif

     endif

     deallocate(axis_data)
   enddo
!
!*** add esmf fields
   if(id_ua > 0) then
     call find_outputname(trim(file_name),'ucomp',output_name)
!     if(mpp_pe()==mpp_root_pe()) print *,'ucomp output name is ',trim(output_name)
     call add_field_to_bundle(trim(output_name),'zonal wind', 'm/sec', "time: point",   &
          axes(1:3), fcst_grid, kstt_ua,kend_ua, dyn_bundle, output_file, &
          range=vrange, rcd=rc)
     if(rc==0)  num_field_dyn=num_field_dyn+1
   endif
!
   if(id_va > 0) then
     call find_outputname(trim(file_name),'vcomp',output_name)
     call add_field_to_bundle(trim(output_name),'meridional wind', 'm/sec', "time: point",   &
          axes(1:3), fcst_grid, kstt_va,kend_va, dyn_bundle, output_file, &
          range=vrange,rcd=rc)
     if(rc==0)  num_field_dyn=num_field_dyn+1
   endif
!
!*** create 3D vector from local u/v winds
   if(id_ua > 0 .and. id_va > 0) then
     output_name = "windvector"
     output_file1 = 'none'
     l3Dvector = .true.
     call add_field_to_bundle(trim(output_name),'3D cartisian wind vector', 'm/sec', "time: point",   &
          axes(1:3), fcst_grid, kstt_windvect,kend_windvect, dyn_bundle, output_file1, range=vrange,   &
          l3Dvector=l3Dvector,rcd=rc)
   endif
!
   if ( .not.hydrostatico ) then
     if( id_w>0  ) then
       call find_outputname(trim(file_name),'w',output_name)
       call add_field_to_bundle(trim(output_name),'vertical wind', 'm/sec', "time: point",   &
            axes(1:3), fcst_grid, kstt_w,kend_w, dyn_bundle, output_file, &
            range=wrange, rcd=rc)
       if(rc==0)  num_field_dyn=num_field_dyn+1
     endif
     if( id_pfnh>0  ) then
       call find_outputname(trim(file_name),'pfnh',output_name)
       call add_field_to_bundle(trim(output_name),'non-hydrostatic pressure', 'pa', "time: point",  &
            axes(1:3), fcst_grid, kstt_pfnh,kend_pfnh, dyn_bundle, output_file, rcd=rc)
       if(rc==0)  num_field_dyn=num_field_dyn+1
     endif
     if( id_delz>0  ) then
       call find_outputname(trim(file_name),'delz',output_name)
       call add_field_to_bundle(trim(output_name),'height thickness', 'm', "time: point",   &
            axes(1:3), fcst_grid, kstt_delz,kend_delz, dyn_bundle, output_file, rcd=rc)
       if(rc==0)  num_field_dyn=num_field_dyn+1
     endif
   else
     if( id_pfhy>0  ) then
       call find_outputname(trim(file_name),'pfhy',output_name)
       call add_field_to_bundle(trim(output_name),'hydrostatic pressure', 'pa', "time: point",   &
            axes(1:3), fcst_grid, kstt_pfhy,kend_pfhy, dyn_bundle, output_file, rcd=rc)
       if(rc==0)  num_field_dyn=num_field_dyn+1
     endif
   endif
!
   if(id_pt > 0) then
     call find_outputname(trim(file_name),'temp',output_name)
     call add_field_to_bundle(trim(output_name),'temperature', 'K', "time: point",   &
          axes(1:3), fcst_grid, kstt_pt,kend_pt, dyn_bundle, output_file, &
          range=trange,rcd=rc)
     if(rc==0)  num_field_dyn=num_field_dyn+1
   endif
!
   if( id_delp > 0) then
     call find_outputname(trim(file_name),'delp',output_name)
     call add_field_to_bundle(trim(output_name),'pressure thickness', 'pa', "time: point",   &
          axes(1:3), fcst_grid, kstt_delp,kend_delp, dyn_bundle, output_file, rcd=rc)
     if(rc==0)  num_field_dyn=num_field_dyn+1
   endif
!
! tracers
   do i=1, ncnsto
     call get_tracer_names ( MODEL_ATMOS, i, tname, tlongname, tunits )
     if (id_tracer(i)>0) then
       call find_outputname(trim(file_name),trim(tname),output_name)
       call add_field_to_bundle(trim(output_name),trim(tlongname), trim(tunits), "time: point",   &
            axes(1:3), fcst_grid, kstt_tracer(i),kend_tracer(i), dyn_bundle, output_file, rcd=rc)
       if(rc==0)  num_field_dyn=num_field_dyn+1
     endif
!     if(mpp_pe()==mpp_root_pe())print *,'in fv_dyn bundle,add trac,i=',i,'output_name=',trim(output_name),' rc=',rc
   enddo
!
!
   if( id_ps > 0) then
     call find_outputname(trim(file_name),'ps',output_name)
     call add_field_to_bundle(trim(output_name),'surface pressure', 'pa', "time: point",   &
          axes(1:2), fcst_grid, kstt_ps,kend_ps, dyn_bundle, output_file, rcd=rc)
     if(rc==0)  num_field_dyn=num_field_dyn+1
   endif
!
   if( id_hs > 0) then
     call find_outputname(trim(file_name),'hs',output_name)
     call add_field_to_bundle(trim(output_name),'surface geopotential height', 'gpm', "time: point",   &
          axes(1:2), fcst_grid, kstt_hs,kend_hs, dyn_bundle, output_file, rcd=rc)
     if(rc==0)  num_field_dyn=num_field_dyn+1
   endif

!jwtest:
!   call ESMF_FieldBundleGet(dyn_bundle, fieldCount=fieldCount, rc=rc)
!   print *,'in dyn_bundle_setup, fieldCount=',fieldCount
!   allocate(fieldnamelist(fieldCount),fieldlist(fieldCount))
!   call ESMF_FieldBundleGet(dyn_bundle, fieldlist=fieldlist,fieldnamelist=fieldnamelist, rc=rc)
!   do i=1,fieldCount
!     call ESMF_AttributeGet(fieldlist(i), convention="NetCDF", purpose="FV3", &
!                    name="output_file", value=fld_outfilename, rc=rc)
!     print *,'in dyn bundle setup, i=',i,' fieldname=',trim(fieldnamelist(i)),' out filename=',trim(fld_outfilename)
!   enddo

 end subroutine fv_dyn_bundle_setup

 subroutine add_field_to_bundle(var_name,long_name,units,cell_methods, axes,dyn_grid, &
                                kstt,kend,dyn_bundle,output_file, range, l3Dvector, rcd)
   use esmf
   implicit none

   character(*), intent(in)             :: var_name, long_name, units, cell_methods
   integer, intent(in)                  :: axes(:)
   type(esmf_grid), intent(in)          :: dyn_grid
   integer, intent(in)                  :: kstt,kend
   type(esmf_fieldbundle),intent(inout) :: dyn_bundle
   character(*), intent(in)             :: output_file
   real, intent(in), optional           :: range(2)
   logical, intent(in), optional        :: l3Dvector
   integer, intent(out), optional       :: rcd
!
!*** local variable
   type(ESMF_Field)         :: field
   type(ESMF_DataCopy_Flag) :: copyflag=ESMF_DATACOPY_REFERENCE
   integer rc, i, j, idx
   real(4),dimension(:,:,:,:),pointer :: temp_r4d
   real(4),dimension(:,:,:),  pointer :: temp_r3d
   real(4),dimension(:,:),    pointer :: temp_r2d
   logical, save :: first=.true.
!
!*** create esmf field  
   if( present(l3Dvector) ) then
     temp_r4d => windvect(1:3,isco:ieco,jsco:jeco,kstt:kend)
     call ESMF_LogWrite('create winde vector esmf field', ESMF_LOGMSG_INFO, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
!jw      field = ESMF_FieldCreate(dyn_grid, temp_r4d, datacopyflag=ESMF_DATACOPY_VALUE, 
     field = ESMF_FieldCreate(dyn_grid, temp_r4d, datacopyflag=ESMF_DATACOPY_REFERENCE, &
                            gridToFieldMap=(/2,3/), ungriddedLBound=(/1,kstt/), ungriddedUBound=(/3,kend/), &
                            name="windvector", indexFlag=ESMF_INDEX_DELOCAL, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out
     call ESMF_LogWrite('create winde vector esmf field', ESMF_LOGMSG_INFO, rc=rc)
     if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
      line=__LINE__, &
      file=__FILE__)) &
      return  ! bail out

     call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"output_file"/), rc=rc)
     call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='output_file',value=trim(output_file),rc=rc)

     call ESMF_FieldBundleAdd(dyn_bundle,(/field/), rc=rc)
     if( present(rcd)) rcd=rc
     return
   else if( kend>kstt ) then
     temp_r3d => buffer_dyn(isco:ieco,jsco:jeco,kstt:kend)
     field = ESMF_FieldCreate(dyn_grid, temp_r3d, datacopyflag=copyflag, &
                            name=var_name, indexFlag=ESMF_INDEX_DELOCAL, rc=rc)
   else if(kend==kstt) then
     temp_r2d => buffer_dyn(isco:ieco,jsco:jeco,kstt)
     field = ESMF_FieldCreate(dyn_grid, temp_r2d, datacopyflag=copyflag, &
                            name=var_name, indexFlag=ESMF_INDEX_DELOCAL, rc=rc)
   endif
!
!*** add field attributes
   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"long_name"/), rc=rc)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='long_name',value=trim(long_name),rc=rc)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"units"/), rc=rc)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='units',value=trim(units),rc=rc)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"missing_value"/), rc=rc)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='missing_value',value=missing_value,rc=rc)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"_FillValue"/), rc=rc)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='_FillValue',value=missing_value,rc=rc)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"cell_methods"/), rc=rc)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='cell_methods',value=trim(cell_methods),rc=rc)

   call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
        attrList=(/"output_file"/), rc=rc)
   call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
        name='output_file',value=trim(output_file),rc=rc)
!
!*** add vertical coord attribute:
   if( size(axes) > 2) then
     do i=3,size(axes)
       idx=0
       do j=1,size(all_axes)
         if (axes(i)==all_axes(j)) then
           idx=j
           exit
         endif
       enddo
       if (idx>0) then
         call ESMF_AttributeAdd(field, convention="NetCDF", purpose="FV3", &
           attrList=(/"ESMF:ungridded_dim_labels"/), rc=rc)
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
         call ESMF_AttributeSet(field, convention="NetCDF", purpose="FV3", &
           name="ESMF:ungridded_dim_labels", valueList=(/trim(axis_name(idx))/), rc=rc)
!         if( first ) then
!             print *,'add axis_name to field,',trim(axis_name(idx))
!         endif
         if (ESMF_LogFoundError(rcToCheck=rc, msg=ESMF_LOGERR_PASSTHRU, &
           line=__LINE__, &
           file=__FILE__)) &
           return  ! bail out
       endif
     enddo
     first=.false.
   endif

!*** add field into bundle
   call ESMF_FieldBundleAdd(dyn_bundle,(/field/), rc=rc)
   if( present(rcd)) rcd=rc
!
 end subroutine add_field_to_bundle
!-------------------------------------------------------------------------------------
 subroutine find_outputname(module_name, field_name, output_name)
   character(*), intent(in)     :: module_name
   character(*), intent(in)     :: field_name
   character(*), intent(out)    :: output_name
!
   integer i,j,in_num, out_num
   integer tile_count
!
   tile_count=1
   in_num = find_input_field(module_name, field_name, tile_count)
!
   output_name=''
   do i=1, max_output_fields
     if(output_fields(i)%input_field == in_num) then
       output_name=output_fields(i)%output_name
       exit
     endif
   enddo
   if(output_name=='') then
     print *,'Error, cant find out put name, field_name=',trim(field_name),'in_num=',in_num
   endif

 end subroutine find_outputname

#endif

end module fv_nggps_diags_mod
