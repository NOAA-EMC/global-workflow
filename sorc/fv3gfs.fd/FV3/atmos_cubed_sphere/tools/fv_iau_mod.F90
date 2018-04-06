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

!-------------------------------------------------------------------------------
!> @brief incremental analysis update module
!> @author Xi.Chen - author of fv_treat_da_inc.F90
!> @author Philip Pegion <philip.pegion@noaa.gov>
!> @date 09/13/2017
!
!  REVISION HISTORY:
!  09/13/2017 - Initial Version based on fv_treat_da_inc.F90
!-------------------------------------------------------------------------------

#ifdef OVERLOAD_R4
#define _GET_VAR1 get_var1_real 
#else
#define _GET_VAR1 get_var1_double
#endif

module fv_iau_mod

  use fms_mod,             only: file_exist
  use mpp_mod,             only: mpp_error, FATAL, NOTE, mpp_pe
  use mpp_domains_mod,     only: domain2d

  use constants_mod,       only: pi=>pi_8 
  use fv_arrays_mod,       only: fv_atmos_type,       &
                                 fv_grid_type,        & 
                                 fv_grid_bounds_type, &
                                 R_GRID
  use fv_mp_mod,           only: is_master
  use sim_nc_mod,          only: open_ncfile,         &
                                 close_ncfile,        &
                                 get_ncdim1,          &
                                 get_var1_double,     &
                                 get_var3_r4,         &
                                 get_var1_real, check_var_exists
  use IPD_typedefs,        only: IPD_init_type, IPD_control_type, &
                                 kind_phys
  use block_control_mod,   only: block_control_type
  use fv_treat_da_inc_mod, only: remap_coef
  use tracer_manager_mod,  only: get_tracer_names,get_tracer_index, get_number_tracers
  use field_manager_mod,   only: MODEL_ATMOS
  implicit none

  private

  real,allocatable::s2c(:,:,:)
!  real:: s2c(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je,4)
!  integer, dimension(Atm(1)%bd%is:Atm(1)%bd%ie,Atm(1)%bd%js:Atm(1)%bd%je):: &
!      id1, id2, jdc
  integer,allocatable,dimension(:,:) :: id1,id2,jdc

  real :: deg2rad,dt,rdt
  integer :: im,jm,km,nfiles,ncid
  integer :: is,  ie,  js,  je
  integer :: npz,ntracers
  character(len=32), allocatable :: tracer_names(:)
  integer, allocatable :: tracer_indicies(:)

  real(kind=4), allocatable:: wk3(:,:,:)
  type iau_internal_data_type
    real,allocatable :: ua_inc(:,:,:)
    real,allocatable :: va_inc(:,:,:)
    real,allocatable :: temp_inc(:,:,:)
    real,allocatable :: delp_inc(:,:,:)
    real,allocatable :: delz_inc(:,:,:)
    real,allocatable :: tracer_inc(:,:,:,:)
  end type iau_internal_data_type
  type iau_external_data_type
    real,allocatable :: ua_inc(:,:,:)
    real,allocatable :: va_inc(:,:,:)
    real,allocatable :: temp_inc(:,:,:)
    real,allocatable :: delp_inc(:,:,:)
    real,allocatable :: delz_inc(:,:,:)
    real,allocatable :: tracer_inc(:,:,:,:)
    logical          :: in_interval = .false.
  end type iau_external_data_type
  type iau_state_type
      type(iau_internal_data_type):: inc1
      type(iau_internal_data_type):: inc2
      real(kind=kind_phys)        :: hr1
      real(kind=kind_phys)        :: hr2
  end type iau_state_type
  type(iau_state_type) :: IAU_state
  public iau_external_data_type,IAU_initialize,getiauforcing

contains
subroutine IAU_initialize (IPD_Control, IAU_Data,Init_parm)
    type (IPD_control_type), intent(in) :: IPD_Control
    type (IAU_external_data_type), intent(inout) :: IAU_Data
    type (IPD_init_type),    intent(in) :: Init_parm
    ! local

    character(len=128) :: fname
    real, dimension(:,:,:), allocatable:: u_inc, v_inc
    real, allocatable:: lat(:), lon(:),agrid(:,:,:)

    integer:: i, j, k
    integer:: i1, i2, j1
    integer:: jbeg, jend

    logical:: found
    integer nfilesall
    integer, allocatable :: idt(:)

    is  = IPD_Control%isc
    ie  = is + IPD_Control%nx-1
    js  = IPD_Control%jsc
    je  = js + IPD_Control%ny-1
    call get_number_tracers(MODEL_ATMOS, num_tracers=ntracers)
    allocate (tracer_names(ntracers))
    allocate (tracer_indicies(ntracers))
    do i = 1, ntracers
       call get_tracer_names(MODEL_ATMOS, i, tracer_names(i))
       tracer_indicies(i)  = get_tracer_index(MODEL_ATMOS,tracer_names(i))
    enddo
    allocate(s2c(is:ie,js:je,4))
    allocate(id1(is:ie,js:je))
    allocate(id2(is:ie,js:je))
    allocate(jdc(is:ie,js:je))
    allocate(agrid(is:ie,js:je,2))
! determine number of increment files to read, and the valid forecast hours

   nfilesall = size(IPD_Control%iau_inc_files)
   nfiles = 0
   if (is_master()) print*,'in iau_init',trim(IPD_Control%iau_inc_files(1)),IPD_Control%iaufhrs(1)
   do k=1,nfilesall
      if (trim(IPD_Control%iau_inc_files(k)) .eq. '' .or. IPD_Control%iaufhrs(k) .lt. 0) exit
      if (is_master()) then
         print *,k,trim(adjustl(IPD_Control%iau_inc_files(k)))
      endif
      nfiles = nfiles + 1
   enddo
   if (is_master()) print *,'nfiles = ',nfiles
   if (nfiles < 1) then
      return
   endif
   if (nfiles > 1) then
      allocate(idt(nfiles-1))
      idt = IPD_Control%iaufhrs(2:nfiles)-IPD_Control%iaufhrs(1:nfiles-1)
      do k=1,nfiles-1
         if (idt(k) .ne. IPD_Control%iaufhrs(2)-IPD_Control%iaufhrs(1)) then
           print *,'forecast intervals in iaufhrs must be constant'
           call mpp_error (FATAL,' forecast intervals in iaufhrs must be constant')
         endif
      enddo
      deallocate(idt)
   endif
   if (is_master()) print *,'iau interval = ',IPD_Control%iau_delthrs,' hours'
   dt = (IPD_Control%iau_delthrs*3600.)
   rdt = 1.0/dt

!  set up interpolation weights to go from GSI's gaussian grid to cubed sphere
    deg2rad = pi/180.

    npz = IPD_Control%levs
    fname = 'INPUT/'//trim(IPD_Control%iau_inc_files(1))

    if( file_exist(fname) ) then
      call open_ncfile( fname, ncid )        ! open the file
      call get_ncdim1( ncid, 'lon',   im)
      call get_ncdim1( ncid, 'lat',   jm)
      call get_ncdim1( ncid, 'lev',   km)

      if (km.ne.npz) then
        if (is_master()) print *, 'km = ', km
        call mpp_error(FATAL, &
            '==> Error in IAU_initialize: km is not equal to npz')
      endif

      if(is_master())  write(*,*) fname, ' DA increment dimensions:', im,jm,km

      allocate (  lon(im) )
      allocate (  lat(jm) )

      call _GET_VAR1 (ncid, 'lon', im, lon )
      call _GET_VAR1 (ncid, 'lat', jm, lat )
      call close_ncfile(ncid)

      ! Convert to radians
      do i=1,im
        lon(i) = lon(i) * deg2rad 
      enddo
      do j=1,jm
        lat(j) = lat(j) * deg2rad
      enddo

    else
      call mpp_error(FATAL,'==> Error in IAU_initialize: Expected file '&
          //trim(fname)//' for DA increment does not exist')
    endif

    ! Initialize lat-lon to Cubed bi-linear interpolation coeff:
    ! populate agrid
!    print*,'is,ie,js,je=',is,ie,js,ie
!    print*,'size xlon=',size(Init_parm%xlon(:,1)),size(Init_parm%xlon(1,:))
!    print*,'size agrid=',size(agrid(:,1,1)),size(agrid(1,:,1)),size(agrid(1,1,:))
    do j = 1,size(Init_parm%xlon,2)
      do i = 1,size(Init_parm%xlon,1)
!         print*,i,j,is-1+j,js-1+j
         agrid(is-1+i,js-1+j,1)=Init_parm%xlon(i,j)
         agrid(is-1+i,js-1+j,2)=Init_parm%xlat(i,j)
      enddo
    enddo
    call remap_coef( is, ie, js, je, &
        im, jm, lon, lat, id1, id2, jdc, s2c, &
        agrid)
    deallocate ( lon, lat,agrid )

   
    allocate(IAU_Data%ua_inc(is:ie, js:je, km))
    allocate(IAU_Data%va_inc(is:ie, js:je, km))
    allocate(IAU_Data%temp_inc(is:ie, js:je, km))
    allocate(IAU_Data%delp_inc(is:ie, js:je, km))
    allocate(IAU_Data%delz_inc(is:ie, js:je, km))
    allocate(IAU_Data%tracer_inc(is:ie, js:je, km,ntracers))
! allocate arrays that will hold iau state
    allocate (iau_state%inc1%ua_inc(is:ie, js:je, km))
    allocate (iau_state%inc1%va_inc(is:ie, js:je, km))
    allocate (iau_state%inc1%temp_inc (is:ie, js:je, km))
    allocate (iau_state%inc1%delp_inc (is:ie, js:je, km))
    allocate (iau_state%inc1%delz_inc (is:ie, js:je, km))
    allocate (iau_state%inc1%tracer_inc(is:ie, js:je, km,ntracers))
    iau_state%hr1=IPD_Control%iaufhrs(1)
    call read_iau_forcing(IPD_Control,iau_state%inc1,'INPUT/'//trim(IPD_Control%iau_inc_files(1)))
    if (nfiles.EQ.1) then  ! only need to get incrments once since constant forcing over window
       call setiauforcing(IPD_Control,IAU_Data)
    endif
    if (nfiles.GT.1) then  !have multiple files, but only read in 2 at a time and interpoalte between them
       allocate (iau_state%inc2%ua_inc(is:ie, js:je, km))
       allocate (iau_state%inc2%va_inc(is:ie, js:je, km))
       allocate (iau_state%inc2%temp_inc (is:ie, js:je, km))
       allocate (iau_state%inc2%delp_inc (is:ie, js:je, km))
       allocate (iau_state%inc2%delz_inc (is:ie, js:je, km))
       allocate (iau_state%inc2%tracer_inc(is:ie, js:je, km,ntracers))
       iau_state%hr2=IPD_Control%iaufhrs(2)
       call read_iau_forcing(IPD_Control,iau_state%inc2,'INPUT/'//trim(IPD_Control%iau_inc_files(2)))
    endif
!   print*,'in IAU init',dt,rdt

end subroutine IAU_initialize

subroutine getiauforcing(IPD_Control,IAU_Data)
        
   implicit none 
   type (IPD_control_type), intent(in) :: IPD_Control
   type(IAU_external_data_type),  intent(inout) :: IAU_Data
   real(kind=kind_phys) t1,t2
   integer n,i,j,k,sphum
  
   IAU_Data%in_interval=.false.
   if (nfiles.LE.0) then
       return
   endif
   if (nfiles.EQ.1) then
!  on check to see if we are in the IAU window,  no need to update the
!  tendencies since they are fixed over the window
      t1=iau_state%hr1 - IPD_Control%iau_delthrs*0.5
      t2=iau_state%hr1 + IPD_Control%iau_delthrs*0.5
      if ( IPD_Control%fhour < t1 .or. IPD_Control%fhour >= t2 ) then
!         if (is_master()) print *,'no iau forcing',t1,IPD_Control%fhour,t2
         IAU_Data%in_interval=.false.
      else 
         if (is_master()) print *,'apply iau forcing',t1,IPD_Control%fhour,t2
         IAU_Data%in_interval=.true.
      endif
      return
   endif

   if (nfiles > 1) then
      t2=2
      if (IPD_Control%fhour < IPD_Control%iaufhrs(1) .or. IPD_Control%fhour >= IPD_Control%iaufhrs(nfiles)) then
!         if (is_master()) print *,'no iau forcing',IPD_Control%iaufhrs(1),IPD_Control%fhour,IPD_Control%iaufhrs(nfiles)
         IAU_Data%in_interval=.false.
      else 
         IAU_Data%in_interval=.true.
         do k=nfiles,1,-1
            if (IPD_Control%iaufhrs(k) > IPD_Control%fhour) then
               t2=k
            endif
         enddo
!         if (is_master()) print *,'t2=',t2
         if (IPD_Control%fhour >= iau_state%hr2) then ! need to read in next increment file
            iau_state%hr1=iau_state%hr2
            iau_state%hr2=IPD_Control%iaufhrs(t2)
            iau_state%inc1=iau_state%inc2
            if (is_master()) print *,'reading next increment file',trim(IPD_Control%iau_inc_files(t2))
            call read_iau_forcing(IPD_Control,iau_state%inc2,'INPUT/'//trim(IPD_Control%iau_inc_files(t2)))
         endif
         call updateiauforcing(IPD_Control,IAU_Data)
      endif
   endif
   sphum=get_tracer_index(MODEL_ATMOS,'sphum')
 end subroutine getiauforcing

subroutine updateiauforcing(IPD_Control,IAU_Data)
      
   implicit none 
   type (IPD_control_type),        intent(in) :: IPD_Control
   type(IAU_external_data_type),  intent(inout) :: IAU_Data
   real(kind_phys) delt
   integer i,j,k,l
  
!   if (is_master()) print *,'in updateiauforcing',nfiles,IPD_Control%iaufhrs(1:nfiles)
   delt = (iau_state%hr2-(IPD_Control%fhour))/(IAU_state%hr2-IAU_state%hr1)
   do j = js,je
      do i = is,ie
         do k = 1,npz
            IAU_Data%ua_inc(i,j,k)    =(delt*IAU_state%inc1%ua_inc(i,j,k)    + (1.-delt)* IAU_state%inc2%ua_inc(i,j,k))*rdt
            IAU_Data%va_inc(i,j,k)    =(delt*IAU_state%inc1%va_inc(i,j,k)    + (1.-delt)* IAU_state%inc2%va_inc(i,j,k))*rdt
            IAU_Data%temp_inc(i,j,k)  =(delt*IAU_state%inc1%temp_inc(i,j,k)  + (1.-delt)* IAU_state%inc2%temp_inc(i,j,k))*rdt
            IAU_Data%delp_inc(i,j,k)  =(delt*IAU_state%inc1%delp_inc(i,j,k)  + (1.-delt)* IAU_state%inc2%delp_inc(i,j,k))*rdt
            IAU_Data%delz_inc(i,j,k)  =(delt*IAU_state%inc1%delz_inc(i,j,k)  + (1.-delt)* IAU_state%inc2%delz_inc(i,j,k))*rdt
            do l=1,ntracers
               IAU_Data%tracer_inc(i,j,k,l) =(delt*IAU_state%inc1%tracer_inc(i,j,k,l) + (1.-delt)* IAU_state%inc2%tracer_inc(i,j,k,l))*rdt
            enddo
         enddo
       enddo
   enddo
 end subroutine updateiauforcing


 subroutine setiauforcing(IPD_Control,IAU_Data)
      
 implicit none 
 type (IPD_control_type),        intent(in) :: IPD_Control
 type(IAU_external_data_type),  intent(inout) :: IAU_Data
 real(kind_phys) delt, dt
 integer i,j,k,l,sphum
!  this is only called if using 1 increment file
 if (is_master()) print *,'in setiauforcing',rdt
 do j = js,je
    do i = is,ie
       do k = 1,npz
          IAU_Data%ua_inc(i,j,k)    =IAU_state%inc1%ua_inc(i,j,k)*rdt
          IAU_Data%va_inc(i,j,k)    =IAU_state%inc1%va_inc(i,j,k)*rdt
          IAU_Data%temp_inc(i,j,k)  =IAU_state%inc1%temp_inc(i,j,k)*rdt
          IAU_Data%delp_inc(i,j,k) =IAU_state%inc1%delp_inc(i,j,k)*rdt
          IAU_Data%delz_inc(i,j,k) =IAU_state%inc1%delz_inc(i,j,k)*rdt
          do l = 1,ntracers
             IAU_Data%tracer_inc(i,j,k,l) =IAU_state%inc1%tracer_inc(i,j,k,l)*rdt
          enddo
       enddo
    enddo
 enddo
 sphum=get_tracer_index(MODEL_ATMOS,'sphum')
 end subroutine setiauforcing

subroutine read_iau_forcing(IPD_Control,increments,fname)
    type (IPD_control_type), intent(in) :: IPD_Control
    type(iau_internal_data_type), intent(inout):: increments  
    character(len=*),  intent(in) :: fname
!locals
    real, dimension(:,:,:), allocatable:: u_inc, v_inc

    integer:: i, j, k, l, npz
    integer:: i1, i2, j1
    integer:: jbeg, jend
    real(kind=R_GRID), dimension(2):: p1, p2, p3
    real(kind=R_GRID), dimension(3):: e1, e2, ex, ey

    logical:: found
    integer :: is,  ie,  js,  je

    is  = IPD_Control%isc
    ie  = is + IPD_Control%nx-1
    js  = IPD_Control%jsc
    je  = js + IPD_Control%ny-1

    deg2rad = pi/180.

    npz = IPD_Control%levs

    if( file_exist(fname) ) then
      call open_ncfile( fname, ncid )        ! open the file
    else
      call mpp_error(FATAL,'==> Error in read_iau_forcing: Expected file '&
          //trim(fname)//' for DA increment does not exist')
    endif

    ! Find bounding latitudes:
    jbeg = jm-1;         jend = 2
    do j=js,je
      do i=is,ie
          j1 = jdc(i,j)
        jbeg = min(jbeg, j1) 
        jend = max(jend, j1+1)
      enddo
    enddo

    allocate ( wk3(1:im,jbeg:jend, 1:km) )
 ! read in 1 time level
    call interp_inc('T_inc',increments%temp_inc(:,:,:),jbeg,jend)
    call interp_inc('delp_inc',increments%delp_inc(:,:,:),jbeg,jend)
    call interp_inc('delz_inc',increments%delz_inc(:,:,:),jbeg,jend)
    call interp_inc('u_inc',increments%ua_inc(:,:,:),jbeg,jend)   ! can these be treated as scalars?
    call interp_inc('v_inc',increments%va_inc(:,:,:),jbeg,jend)
    do l=1,ntracers
       call interp_inc(trim(tracer_names(l))//'_inc',increments%tracer_inc(:,:,:,l),jbeg,jend)
    enddo
    call close_ncfile(ncid)
    deallocate (wk3)


end subroutine read_iau_forcing

subroutine interp_inc(field_name,var,jbeg,jend)
! interpolate increment from GSI gaussian grid to cubed sphere
! everying is on the A-grid, earth relative
 character(len=*), intent(in) :: field_name
 real, dimension(is:ie,js:je,1:km), intent(inout) :: var
 integer, intent(in) :: jbeg,jend
 integer:: i1, i2, j1, k,j,i,ierr
 call check_var_exists(ncid, field_name, ierr)
 if (ierr == 0) then
    call get_var3_r4( ncid, field_name, 1,im, jbeg,jend, 1,km, wk3 )
 else
    print *,'warning: no increment for ',trim(field_name),' found, assuming zero'
    wk3 = 0.
 endif
 do k=1,km
    do j=js,je
       do i=is,ie
          i1 = id1(i,j)
          i2 = id2(i,j)
          j1 = jdc(i,j)
          var(i,j,k) = s2c(i,j,1)*wk3(i1,j1  ,k) + s2c(i,j,2)*wk3(i2,j1  ,k)+&
                       s2c(i,j,3)*wk3(i2,j1+1,k) + s2c(i,j,4)*wk3(i1,j1+1,k)
       enddo
    enddo
 enddo
end subroutine interp_inc

end module fv_iau_mod


