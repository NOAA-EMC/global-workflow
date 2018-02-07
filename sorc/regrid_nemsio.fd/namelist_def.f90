module namelist_def

  !=======================================================================

  ! Define associated modules and subroutines

  !-----------------------------------------------------------------------

  use kinds

  !-----------------------------------------------------------------------

  use mpi_interface

  !-----------------------------------------------------------------------

  implicit none
  
  !-----------------------------------------------------------------------

  ! Define global variables

  integer,                             parameter :: max_ngrids                              = 12 
  character(len=500)                             :: analysis_filename(max_ngrids)           = 'NOT USED'
  character(len=500)                             :: analysis_filename2d(max_ngrids)         = 'NOT USED'
  character(len=500)                             :: gfs_hyblevs_filename                    = 'NOT USED'
  character(len=500)                             :: esmf_neareststod_filename               = 'NOT USED'
  character(len=500)                             :: esmf_bilinear_filename                  = 'NOT USED'
  character(len=500)                             :: variable_table                          = 'NOT USED'
  character(len=500)                             :: datapathout2d                           = './'
  character(len=500)                             :: datapathout3d                           = './'
  character(len=19)                              :: forecast_timestamp                      = '0000-00-00_00:00:00'
  character(len=4)                               :: nemsio_opt                              = 'bin4'
  character(len=4)                               :: nemsio_opt2d                            = 'none'
  character(len=4)                               :: nemsio_opt3d                            = 'none'
  logical                                        :: is_ugrid2sgrid                          = .false.
  logical                                        :: debug                                   = .false.
  integer                                        :: nlons                                   = 0
  integer                                        :: nlats                                   = 0
  integer                                        :: ntrunc                                  = 0
  integer                                        :: ngrids                                  = 0
  namelist /share/    debug, nlons,nlats,ntrunc,datapathout2d,datapathout3d,             &
       analysis_filename, forecast_timestamp, nemsio_opt, nemsio_opt2d, nemsio_opt3d,    &
       analysis_filename2d, variable_table
       
  namelist /interpio/ esmf_bilinear_filename, esmf_neareststod_filename, gfs_hyblevs_filename

  !---------------------------------------------------------------------

contains

  !=====================================================================

  ! namelistparams.f90:

  !---------------------------------------------------------------------

  subroutine namelistparams()

    ! Define variables computed within routine

    logical                                                            :: is_it_there
    integer                                                            :: unit_nml
    
    ! Define counting variables

    integer                                                            :: i, j, k

    !===================================================================
    
    ! Define local variables

    unit_nml    = 9
    is_it_there = .false.
    inquire(file='regrid-nemsio.input',exist = is_it_there)

    ! Check local variable and proceed accordingly

    if(is_it_there) then

       ! Define local variables

       open(file   = 'regrid-nemsio.input',                              &
            unit   = unit_nml        ,                                   &
            status = 'old'         ,                                     &
            form   = 'formatted'     ,                                   &
            action = 'read'        ,                                     &
            access = 'sequential'  )
       read(unit_nml,NML = share)
       read(unit_nml,NML = interpio)
       close(unit_nml)
       if (nemsio_opt2d == 'none') nemsio_opt2d=nemsio_opt
       if (nemsio_opt3d == 'none') nemsio_opt3d=nemsio_opt

       ! Loop through local variable

       do i = 1, max_ngrids

          ! Check local variable and proceed accordingly

          if(analysis_filename(i) .ne. 'NOT USED') then

             ! Define local variables

             ngrids = ngrids + 1

          end if ! if(analysis_filename(i) .ne. 'NOT USED')

       end do ! do i = 1, max_ngrids

    else  ! if(is_it_there)

       ! Define local variables

       if(mpi_procid .eq. mpi_masternode) write(6,500)
       call mpi_barrier(mpi_comm_world,mpi_ierror)
       call mpi_interface_terminate()

    end if ! if(.not. is_it_there)

    !===================================================================

    ! Check local variable and proceed accordingly

    if(mpi_procid .eq. mpi_masternode) then

       ! Define local variables

       write(6,*) '&SHARE'
       write(6,*) 'DEBUG                         = ', debug
       write(6,*) 'ANALYSIS_FILENAME             = '
       do k = 1, ngrids
          write(6,*) trim(adjustl(analysis_filename(k)))
          ! if analysis_filename2d not specified, set to analysis_filename
          if (trim(analysis_filename2d(k)) == 'NOT USED') then
             analysis_filename2d(k) = analysis_filename(k)
          endif
       end do ! do k = 1, ngrids
       write(6,*) 'ANALYSIS_FILENAME2D             = '
       do k = 1, ngrids
          write(6,*) trim(adjustl(analysis_filename2d(k)))
       end do ! do k = 1, ngrids
       write(6,*) 'VARIABLE_TABLE                = ',                    &
            & trim(adjustl(variable_table))
       write(6,*) 'FORECAST_TIMESTAMP            = ', forecast_timestamp
       write(6,*) 'OUTPUT DATAPATH (2d)                      = ',                    &
            & trim(adjustl(datapathout2d))
       write(6,*) 'OUTPUT DATAPATH (3d)                      = ',                    &
            & trim(adjustl(datapathout3d))
       write(6,*) 'NEMSIO_OPT (2d)                    = ', nemsio_opt2d
       write(6,*) 'NEMSIO_OPT (3d)                    = ', nemsio_opt3d
       write(6,*) '/'
       write(6,*) '&INTERPIO'
       write(6,*) 'ESMF_BILINEAR_FILENAME        = ',                 &
            & trim(adjustl(esmf_bilinear_filename))
       write(6,*) 'ESMF_NEARESTSTOD_FILENAME     = ',                 &
            & trim(adjustl(esmf_neareststod_filename))
       write(6,*) 'GFS_HYBLEVS_FILENAME     = ',                 &
            & trim(adjustl(gfs_hyblevs_filename))
       write(6,*) '/'

    end if ! if(mpi_procid .eq. mpi_masternode)

    ! Define local variables

    call mpi_barrier(mpi_comm_world,mpi_ierror)

    !===================================================================

    ! Define format statements

500 format('NAMELISTPARAMS: regrid-nemsio.input not found in the',       &
         & ' current working directory. ABORTING!!!!')

    !===================================================================

  end subroutine namelistparams

  !=====================================================================
  
end module namelist_def
