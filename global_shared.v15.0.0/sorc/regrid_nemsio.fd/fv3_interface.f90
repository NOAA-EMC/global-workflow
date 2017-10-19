module fv3_interface

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use constants

  !-----------------------------------------------------------------------

  use gfs_nems_interface
  use interpolation_interface
  use mpi_interface
  use namelist_def
  use netcdfio_interface
  use variable_interface
  use nemsio_module

  !-----------------------------------------------------------------------

  implicit none

  !-----------------------------------------------------------------------

  ! Define all data and structure types for routine; these variables
  ! are variables required by the subroutines within this module

  type analysis_grid
     character(len=500)                                            :: filename
     character(len=500)                                            :: filename2d
     integer                                                       :: nx
     integer                                                       :: ny
     integer                                                       :: nz
     integer                                                       :: ntime
  end type analysis_grid ! type analysis_grid

  ! Define global variables

  integer n2dvar,n3dvar,ntvars,nrecs,nvvars
  real(nemsio_realkind),                   dimension(:,:,:,:),  allocatable :: fv3_var_3d
  real(nemsio_realkind),                   dimension(:,:,:),    allocatable :: fv3_var_2d                   

  !-----------------------------------------------------------------------

  ! Define interfaces and attributes for module routines

  private
  public :: fv3_regrid_nemsio

  !-----------------------------------------------------------------------

contains

  !-----------------------------------------------------------------------

  subroutine fv3_regrid_nemsio()

    ! Define variables computed within routine

    implicit none
    type(analysis_grid)                                                  :: anlygrd(ngrids)
    type(varinfo), allocatable, dimension(:)                             :: var_info,var_info2d,var_info3d
    type(gfs_grid)                                                       :: gfs_grid
    type(gridvar)                                                        :: invar,invar2
    type(gridvar)                                                        :: outvar,outvar2
    type(nemsio_meta)                                                    :: meta_nemsio2d, meta_nemsio3d

    type(esmfgrid)                                                       :: grid_bilin
    type(esmfgrid)                                                       :: grid_nn
    
    character(len=20)                                                    :: var_name
    character(len=20)                                                    :: nems_name
    character(len=20)                                                    :: nems_levtyp
    character(len=20)                                                    :: itrptyp
    logical                                                              :: itrp_bilinear
    logical                                                              :: itrp_nrstnghbr
    real(nemsio_realkind),      dimension(:,:),              allocatable :: workgrid
    real(nemsio_realkind),               dimension(:),                allocatable :: pk
    real(nemsio_realkind),               dimension(:),                allocatable :: bk
    real, dimension(:),  allocatable :: sendbuffer,recvbuffer
    integer                                                              :: fhour
    integer                                                              :: ncoords
    integer nems_lev,ndims,istatus,ncol,levs_fix
    logical clip

    ! Define counting variables

    integer                                                              :: i, j, k, l,nlev,k2,k3,nrec

    !=====================================================================

    ! Define local variables

    call init_constants_derived()
    call gfs_grid_initialize(gfs_grid)

    ! Loop through local variables

    if(mpi_procid .eq. mpi_masternode) then
       print *,'variable table'
       print *,'--------------'
       open(912,file=trim(variable_table),form='formatted')
       ntvars=0; n2dvar=0; n3dvar=0
       nrecs = 0
       loop_read:  do while (istatus == 0)
         read(912,199,iostat=istatus) var_name,nems_name,nems_levtyp,nems_lev,itrptyp,clip,ndims
         if( istatus /= 0 ) exit loop_read
         nrecs = nrecs + 1
         if(var_name(1:1) .ne. "#") then
            ntvars = ntvars + 1
            ntvars = ntvars + 1
            if (ndims == 2) then
               n2dvar = n2dvar+1
            else if (ndims == 3) then
               n3dvar = n3dvar+1
            else
               print *,'ndims must be 2 or 3 in variable_table.txt'
               call mpi_abort(mpi_comm_world,-91,mpi_ierror)
               stop
            endif
            !print *,'ntvars,n2dvar,n3dvar',ntvars,n2dvar,n3dvar
            !write(6,199) var_name, nems_name,nems_levtyp,nems_lev,itrptyp,clip,ndims
         endif
       enddo loop_read 
       close(912)
       print *,'nrecs,ntvars,n2dvar,n3dvar',nrecs,ntvars,n2dvar,n3dvar
    endif
    call mpi_bcast(nrecs,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(n2dvar,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(n3dvar,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    call mpi_bcast(ntvars,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    if (ntvars == 0) then
       print *,'empty variable_table.txt!'
       call mpi_interface_terminate()
       stop
    endif
    allocate(var_info(ntvars))
    open(912,file=trim(variable_table),form='formatted')
    k = 0
    nvvars = 0 ! number of vector variables
    do nrec = 1, nrecs
       read(912,199,iostat=istatus) var_name,nems_name,nems_levtyp,nems_lev,itrptyp,clip,ndims
       if (var_name(1:1) .ne. "#") then
          k = k + 1
          var_info(k)%var_name = var_name
          var_info(k)%nems_name = nems_name
          var_info(k)%nems_levtyp = nems_levtyp
          var_info(k)%nems_lev = nems_lev
          var_info(k)%itrptyp = itrptyp
	  if (itrptyp.EQ.'vector') then
	      nvvars=nvvars+1
	  endif
          var_info(k)%clip = clip
          var_info(k)%ndims = ndims
          if(mpi_procid .eq. mpi_masternode) then
             write(6,199) var_info(k)%var_name, var_info(k)%nems_name,var_info(k)%nems_levtyp, &
             var_info(k)%nems_lev,var_info(k)%itrptyp,var_info(k)%clip,var_info(k)%ndims
          endif
       endif
    end do ! do k = 1, ntvars
    ! assume vectors are in pairs
    nvvars=nvvars/2
    call mpi_bcast(nvvars,1,mpi_integer,mpi_masternode,mpi_comm_world,mpi_ierror)
    close(912)
199 format(a20,1x,a20,1x,a20,1x,i1,1x,a20,1x,l1,1x,i1)
    allocate(var_info3d(n3dvar+2))
    allocate(var_info2d(n2dvar))
    k2 = 0
    k3 = 0
    do k=1,ntvars
       if (var_info(k)%ndims == 2) then
          k2 = k2 + 1
          var_info2d(k2) = var_info(k)
       endif
       if (var_info(k)%ndims == 3 .or.  &
           trim(var_info(k)%nems_name) == 'pres' .or. &
           trim(var_info(k)%nems_name) == 'orog') then
          k3 = k3 + 1
          var_info3d(k3) = var_info(k)
          ! orography called 'hgt' in 3d file, not 'orog'
          if (trim(var_info(k)%nems_name) == 'orog') then
            var_info3d(k3)%nems_name = 'hgt                 '
          endif
       endif
    enddo


    do i = 1, ngrids
       anlygrd(i)%filename = analysis_filename(i)
       anlygrd(i)%filename2d = analysis_filename2d(i)
       call fv3_regrid_initialize(anlygrd(i))
    end do ! do i = 1, ngrids

    ! Define local variables

    ncxdim                 = anlygrd(1)%nx
    ncydim                 = anlygrd(1)%ny
    if (n3dvar > 0) then
      nczdim               = anlygrd(1)%nz
    else
      nczdim               = 0
    endif
    nctdim                 = anlygrd(1)%ntime
    ncoords                = ncxdim*ncydim
    invar%ncoords          = ncoords*ngrids
    invar2%ncoords          = ncoords*ngrids
    outvar%ncoords         = gfs_grid%ncoords
    outvar2%ncoords         = gfs_grid%ncoords
    call interpolation_initialize_gridvar(invar)
    call interpolation_initialize_gridvar(invar2)
    call interpolation_initialize_gridvar(outvar)
    call interpolation_initialize_gridvar(outvar2)
    meta_nemsio3d%modelname  = 'GFS'
    meta_nemsio3d%version    = 200509
    meta_nemsio3d%nrec       = 2 + nczdim*n3dvar
    meta_nemsio3d%nmeta      = 5
    meta_nemsio3d%nmetavari  = 3
    meta_nemsio3d%nmetaaryi  = 1
    meta_nemsio3d%dimx       = gfs_grid%nlons
    meta_nemsio3d%dimy       = gfs_grid%nlats
    meta_nemsio3d%dimz       = nczdim
    meta_nemsio3d%jcap       = ntrunc
    meta_nemsio3d%nsoil      = 4
    meta_nemsio3d%nframe     = 0
    meta_nemsio3d%ntrac      = 3
    meta_nemsio3d%idrt       = 4   
    meta_nemsio3d%ncldt      = 3
    meta_nemsio3d%idvc       = 2 
    meta_nemsio3d%idvm       = 2 
    meta_nemsio3d%idsl       = 1 
    meta_nemsio3d%idate(1:6) = 0
    meta_nemsio3d%idate(7)   = 1
    read(forecast_timestamp(9:10),'(i2)') meta_nemsio3d%idate(4)
    read(forecast_timestamp(7:8), '(i2)') meta_nemsio3d%idate(3)
    read(forecast_timestamp(5:6), '(i2)') meta_nemsio3d%idate(2)
    read(forecast_timestamp(1:4), '(i4)') meta_nemsio3d%idate(1)
    meta_nemsio2d = meta_nemsio3d
    meta_nemsio2d%nrec       = n2dvar
    call mpi_barrier(mpi_comm_world,mpi_ierror)
    call gfs_nems_meta_initialization(meta_nemsio2d,var_info2d,gfs_grid)
    call gfs_nems_meta_initialization(meta_nemsio3d,var_info3d,gfs_grid)

    ! Allocate memory for local variables

    if(.not. allocated(fv3_var_2d) .and. n2dvar > 0)                        &
         & allocate(fv3_var_2d(ngrids,ncxdim,ncydim))
    if (mpi_nprocs /= nczdim+1) then
       call mpi_barrier(mpi_comm_world, mpi_ierror)
       if (mpi_procid .eq. mpi_masternode) then
          print *,'number of mpi tasks must be equal to number of levels + 1'
          print *,'mpi procs = ',mpi_nprocs,' levels = ',nczdim
       endif
       call mpi_interface_terminate()
       stop
    endif
    !print *,'allocate fv3_var_3d',ngrids,ncxdim,ncydim,nczdim,mpi_procid
    if(.not. allocated(fv3_var_3d) .and. n3dvar > 0)                        &
         & allocate(fv3_var_3d(ngrids,ncxdim,ncydim,nczdim))
    !print *,'done allocating fv3_var_3d',ngrids,ncxdim,ncydim,nczdim,mpi_procid

    ! Check local variable and proceed accordingly

    call mpi_barrier(mpi_comm_world,mpi_ierror)
    if(mpi_procid .eq. mpi_masternode) then

       ! Allocate memory for local variables

       if (n3dvar > 0) then
           if(.not. allocated(pk)) allocate(pk(nczdim+1))
           if(.not. allocated(bk)) allocate(bk(nczdim+1))

           ! Define local variables

           if (trim(gfs_hyblevs_filename) == 'NOT USED' ) then
               call netcdfio_values_1d(anlygrd(1)%filename,'pk',pk)
               call netcdfio_values_1d(anlygrd(1)%filename,'bk',bk)
           else
               open(913,file=trim(gfs_hyblevs_filename),form='formatted')
               read(913,*) ncol, levs_fix
               if (levs_fix /= (nczdim+1) ) then
                   call mpi_barrier(mpi_comm_world, mpi_ierror)
                   print *,'levs in ', trim(gfs_hyblevs_filename), ' not equal to',(nczdim+1)
                   call mpi_interface_terminate()
                   stop
               endif
               do k=nczdim+1,1,-1
                   read(913,*) pk(k),bk(k)
               enddo
               close(913)
           endif
           if (minval(pk) < -1.e10 .or. minval(bk) < -1.e10) then
              print *,'pk,bk not found in netcdf file..'
              meta_nemsio3d%vcoord = -9999._nemsio_realkind
           else
           ! Loop through local variable

           do k = 1, nczdim + 1

              ! Define local variables

              meta_nemsio3d%vcoord((nczdim + 1) - k + 1,1,1) = pk(k)
              meta_nemsio3d%vcoord((nczdim + 1) - k + 1,2,1) = bk(k)

           end do ! do k = 1, nczdim + 1
           endif
       endif

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! initialize/read in interpolation weight

      grid_bilin%filename = esmf_bilinear_filename
      call interpolation_initialize_esmf(grid_bilin)
     
      grid_nn%filename = esmf_neareststod_filename
      call interpolation_initialize_esmf(grid_nn)

    do l = 1, nctdim

       ncrec = l  ! time level to read from netcdf file

       ! Define local variables

       call fv3_grid_fhour(anlygrd(1),meta_nemsio2d%nfhour)
       call fv3_grid_fhour(anlygrd(1),meta_nemsio3d%nfhour)
       meta_nemsio3d%nfminute   = int(0.0)
       meta_nemsio3d%nfsecondn  = int(0.0)
       meta_nemsio3d%nfsecondd  = int(1.0)
       meta_nemsio3d%fhour      = meta_nemsio3d%nfhour
       meta_nemsio2d%nfminute   = int(0.0)
       meta_nemsio2d%nfsecondn  = int(0.0)
       meta_nemsio2d%nfsecondd  = int(1.0)
       meta_nemsio2d%fhour      = meta_nemsio2d%nfhour

       ! initialize nemsio file.
       if(mpi_procid .eq. mpi_masternode) then
          call gfs_nems_initialize(meta_nemsio2d, meta_nemsio3d)
       end if

       ! wait here.
       call mpi_barrier(mpi_comm_world,mpi_ierror)

       ! Loop through local variables
       k2=1
       do k = 1, ntvars - nvvars

          ! Define local variables

          itrp_bilinear  = .false.
          itrp_nrstnghbr = .false.

          ! Do 2D variables.

          if(var_info(k2)%ndims .eq. 2) then

             ! Check local variable and proceed accordingly

             if(mpi_procid .eq. mpi_masternode) then

                ! Check local variable and proceed accordingly

                call fv3_grid_read(anlygrd(1:ngrids), var_info(k2)%var_name,.true.,.false.)

                call interpolation_define_gridvar(invar,ncxdim,ncydim, ngrids,fv3_var_2d)
                if (trim(var_info(k2)%nems_name) == 'pres') then
                   ! interpolate in exner(pressure) 
                   invar%var = (invar%var/stndrd_atmos_ps)**(rd_over_g*stndrd_atmos_lapse)
                end if

                if(var_info(k2)%itrptyp .eq. 'bilinear') then
                   call interpolation_esmf(invar,outvar,grid_bilin, .false.)
                end if 
                
                if(var_info(k2)%itrptyp .eq. 'nrstnghbr') then
                   call interpolation_esmf(invar,outvar,grid_nn, .true.)
                end if 

                if (trim(var_info(k2)%nems_name) == 'pres') then
                    outvar%var = stndrd_atmos_ps*(outvar%var**(g_over_rd/stndrd_atmos_lapse))
                end if

                if(var_info(k2)%itrptyp .eq. 'vector') then
		  ! read in u winds
                  call fv3_grid_read(anlygrd(1:ngrids), var_info(k2)%var_name,.true.,.false.)
                  call interpolation_define_gridvar(invar,ncxdim,ncydim,ngrids,fv3_var_2d)
                  ! read in v winds
                  call fv3_grid_read(anlygrd(1:ngrids), var_info(k2+1)%var_name,.true.,.false.)
                  call interpolation_define_gridvar(invar2,ncxdim,ncydim,ngrids,fv3_var_2d)
                  call interpolation_esmf_vect(invar,invar2,grid_bilin,outvar,outvar2)
                end if 

                ! Clip variable to zero if desired.
                if(var_info(k2)%clip) call variable_clip(outvar%var)

                ! Write to NEMSIO file.
                call gfs_nems_write('2d',real(outvar%var),                     &
                var_info(k2)%nems_name,var_info(k2)%nems_levtyp,var_info(k2)%nems_lev)
                if (trim(var_info(k2)%nems_name) == 'pres' .or. &
                    trim(var_info(k2)%nems_name) == 'orog' .or. &
                    trim(var_info(k2)%nems_name) == 'hgt') then
                   ! write surface height and surface pressure to 3d file.
                   ! (surface height called 'orog' in nemsio bin4, 'hgt' in
                   ! grib)
                   if (trim(var_info(k2)%nems_name) == 'orog') then
                      call gfs_nems_write('3d',real(outvar%var),                     &
                      'hgt                 ',var_info(k2)%nems_levtyp,1)
                   else
                      call gfs_nems_write('3d',real(outvar%var),                     &
                      var_info(k2)%nems_name,var_info(k2)%nems_levtyp,1)
                   endif
                endif
                if(var_info(k2)%itrptyp .eq. 'vector') then ! write v winds
                   call gfs_nems_write('2d',real(outvar2%var),                     &
                   var_info(k2+1)%nems_name,var_info(k2+1)%nems_levtyp,var_info(k2+1)%nems_lev)
                endif
             end if ! if(mpi_procid .eq. mpi_masternode)

             ! Define local variables
             call mpi_barrier(mpi_comm_world,mpi_ierror)

          end if ! if(var_info(k2)%ndims .eq. 2)

          ! Do 3D variables.

          if(var_info(k2)%ndims .eq. 3) then

             ! read 3d grid on master node, send to other tasks
             if(mpi_procid .eq. mpi_masternode) then
                call fv3_grid_read(anlygrd(1:ngrids), var_info(k2)%var_name,.false.,.true.)
                do nlev=1,nczdim
                   call mpi_send(fv3_var_3d(1,1,1,nlev),ngrids*ncxdim*ncydim,mpi_real,&
                                 nlev,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
                enddo                  
                if(trim(adjustl(var_info(k2)%itrptyp)) .eq. 'vector') then  ! winds
		   call mpi_barrier(mpi_comm_world,mpi_ierror)
                   call fv3_grid_read(anlygrd(1:ngrids), var_info(k2+1)%var_name,.false.,.true.)
                   do nlev=1,nczdim
                      call mpi_send(fv3_var_3d(1,1,1,nlev),ngrids*ncxdim*ncydim,mpi_real,&
                                   nlev,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
                  enddo
               endif 
             else if (mpi_procid .le. nczdim) then
                ! do interpolation, one level on each task.
                call mpi_recv(fv3_var_3d(1,1,1,mpi_procid),ngrids*ncxdim*ncydim,mpi_real,&
                              0,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
                
                call interpolation_define_gridvar(invar,ncxdim,ncydim, ngrids,fv3_var_3d(:,:,:,mpi_procid))                
                
                if(var_info(k2)%itrptyp .eq. 'bilinear') then
                   call interpolation_esmf(invar,outvar,grid_bilin, .false.)
                end if ! if(var_info(k2)%itrptyp .eq. 'bilinear')
                
                if(var_info(k2)%itrptyp .eq. 'nrstnghbr') then
                   call interpolation_esmf(invar,outvar,grid_nn, .true.)
                end if ! if(var_info(k2)%itrptyp .eq. 'nrstnghbr')

                if(trim(adjustl(var_info(k2)%itrptyp)) .eq. 'vector') then  ! winds
		   call mpi_barrier(mpi_comm_world,mpi_ierror)
                   call mpi_recv(fv3_var_3d(1,1,1,mpi_procid),ngrids*ncxdim*ncydim,mpi_real,&
                              0,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
                   call interpolation_define_gridvar(invar2,ncxdim,ncydim,ngrids,fv3_var_3d(:,:,:,mpi_procid))   
                   call interpolation_esmf_vect(invar,invar2,grid_bilin,outvar,outvar2)
                endif

                if(var_info(k2)%clip) call variable_clip(outvar%var(:))

             end if ! if(mpi_procid .ne. mpi_masternode .and.             &
                    ! mpi_procid .le. nczdim)

             ! gather results back on root node to write out.

             if (mpi_procid == mpi_masternode) then
                  ! receive one level of interpolated data on root task.
                  if (.not. allocated(workgrid))  allocate(workgrid(gfs_grid%ncoords,nczdim))
                  if (.not. allocated(recvbuffer)) allocate(recvbuffer(gfs_grid%ncoords))
                  do nlev=1,nczdim
                     call mpi_recv(recvbuffer,gfs_grid%ncoords,mpi_real,&
                                   nlev,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
                     workgrid(:,nlev) = recvbuffer
                  enddo
                  deallocate(recvbuffer)
             else
                  ! send one level of interpolated data to root task.
                  if (.not. allocated(sendbuffer)) allocate(sendbuffer(gfs_grid%ncoords))
                  sendbuffer(:) = outvar%var(:)
                  call mpi_send(sendbuffer,gfs_grid%ncoords,mpi_real,&
                                0,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
             endif

             ! Write to NEMSIO file.

             if(mpi_procid .eq. mpi_masternode) then

                ! Loop through local variable

                do j = 1, nczdim
                   
                   ! Define local variables

                   call gfs_nems_write('3d',workgrid(:,nczdim - j + 1),  &
                        & var_info(k2)%nems_name,var_info(k2)%nems_levtyp,  &
                        & j)

                end do !  do j = 1, nczdim

             end if ! if(mpi_procid .eq. mpi_masternode)

             if(trim(adjustl(var_info(k2)%itrptyp)) .eq. 'vector') then  ! winds
                if (mpi_procid == mpi_masternode) then
                  ! receive one level of interpolated data on root task.
                  if (.not. allocated(workgrid))  allocate(workgrid(gfs_grid%ncoords,nczdim))
                  if (.not. allocated(recvbuffer)) allocate(recvbuffer(gfs_grid%ncoords))
                  do nlev=1,nczdim
                     call mpi_recv(recvbuffer,gfs_grid%ncoords,mpi_real,&
                                   nlev,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
                     workgrid(:,nlev) = recvbuffer
                  enddo
                  deallocate(recvbuffer)
                else
                  ! send one level of interpolated data to root task.
                  if (.not. allocated(sendbuffer)) allocate(sendbuffer(gfs_grid%ncoords))
                  sendbuffer(:) = outvar2%var(:)
                  call mpi_send(sendbuffer,gfs_grid%ncoords,mpi_real,&
                                0,1,mpi_comm_world,mpi_errorstatus,mpi_ierror)
                endif

             ! Write to NEMSIO file.

                if(mpi_procid .eq. mpi_masternode) then
                   
                   do j = 1, nczdim

                      call gfs_nems_write('3d',workgrid(:,nczdim - j + 1),  &
                           & var_info(k2+1)%nems_name,var_info(k2+1)%nems_levtyp,  &
                           & j)
                   end do !  do j = 1, nczdim
   
                end if ! if(mpi_procid .eq. mpi_masternode)
             endif

             ! wait here

             call mpi_barrier(mpi_comm_world,mpi_ierror)

          end if ! if(var_info(k2)%ndims .eq. 3)
	  if(var_info(k2)%itrptyp .eq. 'vector') then ! skip v record here
             k2=k2+1
          endif
          k2=k2+1
       end do ! do k = 1, ntvars

       ! Wait here.

       call mpi_barrier(mpi_comm_world,mpi_ierror)

       ! Finalize  and cleanup

       if(mpi_procid .eq. mpi_masternode) then
          call gfs_nems_finalize()
       end if 
       call mpi_barrier(mpi_comm_world,mpi_ierror)
       if(allocated(workgrid)) deallocate(workgrid)

    end do ! do l = 1, nctdim


!=====================================================================

  end subroutine fv3_regrid_nemsio

  !=======================================================================

  ! fv3_regrid_initialize.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_regrid_initialize(grid)

    ! Define variables passed to routine

    implicit none
    type(analysis_grid)                                                  :: grid

    !=====================================================================

    ! Define local variables

    call netcdfio_dimension(grid%filename,'grid_xt',grid%nx)
    call netcdfio_dimension(grid%filename,'grid_yt',grid%ny)
    if (n3dvar > 0) then
        call netcdfio_dimension(grid%filename,'pfull',grid%nz)
    else
        grid%nz = 0
    endif
    call netcdfio_dimension(grid%filename,'time',grid%ntime)
    
    !=====================================================================

  end subroutine fv3_regrid_initialize

  !=======================================================================

  ! fv3_grid_read.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_grid_read(anlygrd,varname,is_2d,is_3d)

    ! Define variables passed to subroutine

    type(analysis_grid)                                                  :: anlygrd(ngrids)
    character(len=20)                                                    :: varname
    logical                                                              :: is_2d
    logical                                                              :: is_3d

    ! Define counting variables

    integer                                                              :: i, j, k

    !=====================================================================

    ! Loop through local variable

    do k = 1, ngrids

       ! Check local variable and proceed accordingly

       if(debug) write(6,500) ncrec, k
       if(is_2d) then

          ! Define local variables

          ! orog and psfc are in 3d file.
          if (trim(varname) == 'orog' .or. trim(varname) == 'psfc') then
          call netcdfio_values_2d(anlygrd(k)%filename,varname,             &
               & fv3_var_2d(k,:,:))
          else
          call netcdfio_values_2d(anlygrd(k)%filename2d,varname,             &
               & fv3_var_2d(k,:,:))
          endif

       end if ! if(is_2d)

       ! Check local variable and proceed accordingly

       if(is_3d) then

          ! Define local variables

          call netcdfio_values_3d(anlygrd(k)%filename,varname,             &
               & fv3_var_3d(k,:,:,:))

       end if ! if(is_3d)

    end do ! do k = 1, ngrids

    !=====================================================================

    ! Define format statements

500 format('FV3_GRID_READ: Time record = ', i6, '; Cubed sphere face = ',  &
         & i1,'.')

    !=====================================================================

  end subroutine fv3_grid_read

  !=======================================================================

  ! fv3_grid_fhour.f90:

  !-----------------------------------------------------------------------

  subroutine fv3_grid_fhour(grid,fhour)

    ! Define variables passed to routine

    implicit none
    type(analysis_grid)                                                  :: grid
    integer                                                              :: fhour

    ! Define variables computed within routine

    real(nemsio_realkind)                                                         :: workgrid(grid%ntime)
    real(nemsio_realkind)                                                         :: start_jday
    real(nemsio_realkind)                                                         :: fcst_jday
    integer                                                              :: year
    integer                                                              :: month
    integer                                                              :: day
    integer                                                              :: hour
    integer                                                              :: minute
    integer                                                              :: second, iw3jdn
    character(len=80) timeunits

    !=====================================================================

    ! Define local variables

    read(forecast_timestamp(1:4),  '(i4)') year
    read(forecast_timestamp(5:6),  '(i2)') month
    read(forecast_timestamp(7:8), '(i2)') day
    read(forecast_timestamp(9:10),'(i2)') hour
    minute = 0; second = 0
    
    ! Compute local variables

    ! 'flux day' (days since dec 31 1900)
    !call date2wnday(start_jday,year,month,day)
    ! same as above, but valid after 2099 
    start_jday=real(iw3jdn(year,month,day)-iw3jdn(1900,12,31))
    start_jday = start_jday + real(hour)/24.0 + real(minute)/1440.0 + &
         & real(second)/86400.0
    
    ! Define local variables
    
    call netcdfio_values_1d(grid%filename,'time',workgrid)
    call netcdfio_variable_attr(grid%filename,'time','units',timeunits)
    
    ! Compute local variables

    ! ncrec is a global variable in the netcdfio-interface module
    if (timeunits(1:4) == 'days') then
       fcst_jday = start_jday + workgrid(ncrec)
    else if (timeunits(1:5) == 'hours') then
       fcst_jday = start_jday + workgrid(ncrec)/24.
    else if (timeunits(1:7) == 'seconds') then
       fcst_jday = start_jday + workgrid(ncrec)/86400.0
    else
       print *,'unrecognized time units',trim(timeunits)
       call mpi_interface_terminate()
       stop
    endif
    fhour     = nint((86400*(fcst_jday - start_jday))/3600.0)

    !===================================================================== 

  end subroutine fv3_grid_fhour

!      SUBROUTINE DATE2WNDAY(WDAY, IYR,MON,IDY)
!      IMPLICIT NONE
!      INTEGER IYR,MON,IDY
!      REAL    WDAY
!!
!!**********
!!*
!!  1) CONVERT DATE INTO 'FLUX DAY'.
!!
!!  2) THE 'FLUX DAY' IS THE NUMBER OF DAYS SINCE 001/1901 (WHICH IS 
!!      FLUX DAY 1.0).
!!     FOR EXAMPLE:
!!      A) IYR=1901,MON=1,IDY=1, REPRESENTS 0000Z HRS ON 01/01/1901
!!         SO WDAY WOULD BE 1.0.
!!      A) IYR=1901,MON=1,IDY=2, REPRESENTS 0000Z HRS ON 02/01/1901
!!         SO WDAY WOULD BE 2.0.
!!     YEAR MUST BE NO LESS THAN 1901.0, AND NO GREATER THAN 2099.0.
!!     NOTE THAT YEAR 2000 IS A LEAP YEAR (BUT 1900 AND 2100 ARE NOT).
!!
!!  3) ALAN J. WALLCRAFT, NAVAL RESEARCH LABORATORY, JULY 2002.
!!*
!!**********
!!
!      INTEGER NLEAP
!      REAL    WDAY1
!      REAL MONTH(13)
!      DATA MONTH / 0,  31,  59,  90, 120, 151, 181, &
!                      212, 243, 273, 304, 334, 365 /
!!     FIND THE RIGHT YEAR.
!      NLEAP = (IYR-1901)/4
!      WDAY  = 365.0*(IYR-1901) + NLEAP + MONTH(MON) + IDY
!      IF     (MOD(IYR,4).EQ.0 .AND. MON.GT.2) THEN
!        WDAY  = WDAY + 1.0
!      ENDIF
!      END SUBROUTINE DATE2WNDAY

  !=======================================================================

end module fv3_interface
