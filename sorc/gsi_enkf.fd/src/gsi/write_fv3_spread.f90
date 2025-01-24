module write_fv3_enspread_mod
!$$$ module documentation block
!           .      .    .                                       .
! module:   write_fv3_enspread 
!   prgmmr: lei       org:                 date: 2021-08-11
!           modified from write_inc module
!           the history of write_inc is inclued below too
!
! abstract: This module contains routines which write out  
!           the FV3-LAM ens. spread on their analysis grids
! program history log:
!   2019-09-04 Martin    Initial version.  Based on ncepnems_io
!   2019-09-13  martin  added option to zero out certain increment fields 
!   2021-08-11  Lei     created write_fv3_enspread_mod from write_inc
! Subroutines Included:
!   sub write_fv3_enspread - writes netCDF spread for FV3-LAM 
!
!$$$ end documentation block

  implicit none
  private
  public write_fv3_enspread

  interface write_fv3_enspread
     module procedure write_fv3_enspread_
  end interface

contains

  subroutine write_fv3_enspread_ (grdin,filename,en_spread,ibin)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    write_fv3_enspread
!
!   prgmmr: Lei            org:                 date: 2021-08-11
!   modified from write_inc 
!
! abstract: This routine dump FV3-LAM ensemble spread on ensemble analysis grid 
!           to a netCDF file 
!
! program history log:
!   2019-09-04  martin  Initial version. Based on write_atm_nemsio 
!   2019-09-13  martin  added option to zero out certain increment fields 
!   2020-01-10  martin  added in parallel write to decrease wallclock time
!   2021-08-11  lei     write_fv3_enspread modified from write_inc
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machines: ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block

! !USES:
    use netcdf, only: &
      nf90_netcdf4,nf90_mpiio,nf90_create,nf90_def_dim,nf90_real,nf90_def_var,&
      nf90_collective,nf90_var_par_access,nf90_global,nf90_put_att,nf90_put_att,&
      nf90_enddef,nf90_put_var,nf90_close,nf90_noerr,nf90_strerror
 
    use kinds, only: r_kind,i_kind

    use mpimod, only: mpi_rtype
    use mpimod, only: mpi_comm_world, mpi_info_null
    use mpimod, only: ierror
    use mpimod, only: mype


    use general_specmod, only: spec_vars
    use general_sub2grid_mod, only: sub2grid_info

    use gsi_bundlemod, only: gsi_bundle, gsi_bundlegetpointer
    use control_vectors, only: control_vector

    use constants, only: one, rad2deg, r1000

    use obsmod, only: ianldate 
    use hybrid_ensemble_parameters, only: region_lat_ens,region_lon_ens
    use mpimod, only: npe


    implicit none

! !INPUT PARAMETERS:

    type(sub2grid_info), intent(in) :: grdin
    character(len=24),   intent(in) :: filename  ! file to open and write to
    type(gsi_bundle),    intent(in) :: en_spread
    integer(i_kind),     intent(in) :: ibin      ! time bin

!-------------------------------------------------------------------------


    real(r_kind),pointer,dimension(:,:,:) ::uptr,vptr,tptr,qptr 
    real(r_kind),pointer,dimension(:,:) ::psptr 
    real(r_kind),dimension(grdin%lat1,grdin%lon1,grdin%nsig) ::uloc,vloc,tloc,qloc
    real(r_kind),dimension(grdin%lat1,grdin%lon1) ::psloc

    real(r_kind),dimension(grdin%nlon,grdin%nlat) :: deglons
    real(r_kind),dimension(grdin%nlon,grdin%nlat) :: deglats
    real(r_kind),dimension(grdin%nsig) :: levsout
    real(r_kind),dimension(grdin%nsig+1) :: ilevsout

    integer(i_kind) :: mm1, i, j, k, jp1, krev
    integer(i_kind) :: iret, istatus 
    integer(i_kind) :: ncid_out, lon_dimid, lat_dimid, lev_dimid, ilev_dimid
    integer(i_kind) :: lonvarid, latvarid, levvarid, pfullvarid, ilevvarid, &
                         uvarid, vvarid,  &
                       tvarid, qvarid, psvarid
    integer(i_kind) :: dimids3(3),nccount(3),ncstart(3), cnksize(3), j1, j2
    integer(i_kind) :: dimids2(2),nccount2(2)



    real(r_kind), allocatable, dimension(:,:,:) :: out3d
    integer(i_kind),allocatable,dimension(:):: jstartloc  ! start lon of the whole array on each pe
    integer(i_kind),allocatable,dimension(:):: istartloc  ! start lat of the whole array on each pe
    integer(i_kind)      :: mype_out=0  ! mpi task the first to reach here  to write output file


!*************************************************************************
!   Initialize local variables
    write(6,*)"now ibin is used as a placeholder ",ibin 
    allocate(istartloc(npe),jstartloc(npe))
    mm1=mype+1

    do i=1,npe
       istartloc(i)    =grdin%istart(i)
       jstartloc(i)    =grdin%jstart(i)
    end do


!   set up state space based off of xhatsave
!   Convert from control space directly to physical
!   space for comparison with obs.
    istatus=0
     
    call gsi_bundlegetpointer(en_spread,'sf', uptr, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(en_spread,'vp', vptr, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(en_spread,'t', tptr, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(en_spread,'q', qptr, iret); istatus=istatus+iret
    call gsi_bundlegetpointer(en_spread,'ps', psptr, iret); istatus=istatus+iret ! needed for delp
    if ( istatus /= 0 ) then
       if ( mype == 0 ) then
         write(6,*) 'write_fv3_spread_: ERROR'
         write(6,*) 'Missing some of the required fields'
         write(6,*) 'Aborting ... '
      endif
      call stop2(999)
    end if
    
    ! create the output netCDF file
    call ncceck_enspread(nf90_create(path=trim(filename)//".nc", cmode=ior(nf90_netcdf4, nf90_mpiio), ncid=ncid_out, &
                                 comm = mpi_comm_world, info = mpi_info_null))
    ! create dimensions based on analysis resolution, not guess
    call ncceck_enspread(nf90_def_dim(ncid_out, "lon", grdin%nlon, lon_dimid))
    call ncceck_enspread(nf90_def_dim(ncid_out, "lat", grdin%nlat, lat_dimid))
    call ncceck_enspread(nf90_def_dim(ncid_out, "lev", grdin%nsig, lev_dimid))
    call ncceck_enspread(nf90_def_dim(ncid_out, "ilev", grdin%nsig+1, ilev_dimid))
    dimids3 = (/ lon_dimid, lat_dimid, lev_dimid /)
    dimids2 = (/ lon_dimid, lat_dimid /)
    cnksize = (/ grdin%lon1, grdin%lat1, grdin%nsig /)
    ! create variables
    call ncceck_enspread(nf90_def_var(ncid_out, "lon", nf90_real, dimids2, lonvarid))
    call ncceck_enspread(nf90_def_var(ncid_out, "lat", nf90_real, dimids2, latvarid))
    call ncceck_enspread(nf90_def_var(ncid_out, "lev", nf90_real, (/lev_dimid/), levvarid))
    call ncceck_enspread(nf90_def_var(ncid_out, "pfull", nf90_real, (/lev_dimid/), pfullvarid)) !clthink do we need this
    call ncceck_enspread(nf90_def_var(ncid_out, "ilev", nf90_real, (/ilev_dimid/), ilevvarid))




    call ncceck_enspread(nf90_def_var(ncid_out, "u", nf90_real, dimids3, uvarid)) 
    call ncceck_enspread(nf90_var_par_access(ncid_out, uvarid, nf90_collective))
    call ncceck_enspread(nf90_def_var(ncid_out, "v", nf90_real, dimids3, vvarid)) 
    call ncceck_enspread(nf90_var_par_access(ncid_out, vvarid, nf90_collective))
    call ncceck_enspread(nf90_def_var(ncid_out, "t", nf90_real, dimids3, tvarid)) 
    call ncceck_enspread(nf90_var_par_access(ncid_out, tvarid, nf90_collective))
    call ncceck_enspread(nf90_def_var(ncid_out, "q", nf90_real, dimids3, qvarid)) 
    call ncceck_enspread(nf90_var_par_access(ncid_out, qvarid, nf90_collective))
    call ncceck_enspread(nf90_def_var(ncid_out, "ps", nf90_real, dimids2, psvarid)) 
    call ncceck_enspread(nf90_var_par_access(ncid_out, psvarid, nf90_collective))
    call ncceck_enspread(nf90_put_att(ncid_out, nf90_global, "source", "GSI"))
    call ncceck_enspread(nf90_put_att(ncid_out, nf90_global, "comment", &
                                    "enspread from write_fv3_enspread"))
    call ncceck_enspread(nf90_put_att(ncid_out, nf90_global, "analysis_time", ianldate))
    ! add units to lat/lon because that's what the calc_increment utility has
    call ncceck_enspread(nf90_put_att(ncid_out, lonvarid, "units", "degrees_east"))
    call ncceck_enspread(nf90_put_att(ncid_out, latvarid, "units", "degrees_north"))
    ! end the netCDF file definition
    call ncceck_enspread(nf90_enddef(ncid_out))


    ! Strip off boundary points from subdomains
    do k=1,grdin%nsig
       do j=1,grdin%lon1
          jp1 = j+1
          do i=1,grdin%lat1
             uloc(i,j,k)=uptr(i+1,jp1,k)
             vloc(i,j,k)=vptr(i+1,jp1,k)
             tloc(i,j,k)=tptr(i+1,jp1,k)
             qloc(i,j,k)=qptr(i+1,jp1,k)
          end do
       end do
    end do
       do j=1,grdin%lon1
          jp1 = j+1
          do i=1,grdin%lat1
             psloc(i,j)=psptr(i+1,jp1)
          end do
       end do


    if (mype == mype_out) then
       ! latitudes
          deglats = transpose(region_lat_ens)*rad2deg
       ! write to file
       call ncceck_enspread(nf90_put_var(ncid_out, latvarid, deglats, &
                         start = (/1/), count = (/grdin%nlon,grdin%nlat/)))
       ! longitudes
          deglons = transpose(region_lon_ens)*rad2deg
       ! write to file
       call ncceck_enspread(nf90_put_var(ncid_out, lonvarid, deglons, &
                         start = (/1/), count = (/grdin%nlon,grdin%nlat/)))
       ! levels
       do k=1,grdin%nsig
         levsout(k) = real(k,r_kind)
         ilevsout(k) = real(k,r_kind)
       end do
       ilevsout(grdin%nsig+1) = real(grdin%nsig+1,r_kind)
       ! write to file
       call ncceck_enspread(nf90_put_var(ncid_out, levvarid, sngl(levsout), &
                         start = (/1/), count = (/grdin%nsig/)))
       ! pfull
       call ncceck_enspread(nf90_put_var(ncid_out, pfullvarid, sngl(levsout), &
                         start = (/1/), count = (/grdin%nsig/)))
       ! ilev
       call ncceck_enspread(nf90_put_var(ncid_out, ilevvarid, sngl(ilevsout), &
                         start = (/1/), count = (/grdin%nsig+1/)))
    end if


    j1 = 1
    j2 = grdin%lat1
    ncstart = (/ jstartloc(mype+1), istartloc(mype+1), 1 /)
    nccount = (/ grdin%lon1, grdin%lat1, grdin%nsig /)
    nccount2 = (/ grdin%lon1, grdin%lat1 /)
    call mpi_barrier(mpi_comm_world,ierror)
    allocate(out3d(nccount(1),nccount(2),grdin%nsig))
    ! u 
    do k=1,grdin%nsig
       krev = grdin%nsig+1-k
       out3d(:,:,krev) = transpose(uloc(j1:j2,:,k))
    end do
    call ncceck_enspread(nf90_put_var(ncid_out, uvarid, sngl(out3d), &
                      start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
!    ! v 
    do k=1,grdin%nsig
       krev = grdin%nsig+1-k
       out3d(:,:,krev) = transpose(vloc(j1:j2,:,k))
    end do
    call ncceck_enspread(nf90_put_var(ncid_out, vvarid, sngl(out3d), &
                      start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)

    do k=1,grdin%nsig
       krev = grdin%nsig+1-k
       out3d(:,:,krev) = transpose(tloc(j1:j2,:,k))
    end do
    call ncceck_enspread(nf90_put_var(ncid_out, tvarid, sngl(out3d), &
                     start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)
    do k=1,grdin%nsig
       krev = grdin%nsig+1-k
       out3d(:,:,krev) = transpose(qloc(j1:j2,:,k))
    end do
    call ncceck_enspread(nf90_put_var(ncid_out, qvarid, sngl(out3d), &
                      start = ncstart, count = nccount))
    call mpi_barrier(mpi_comm_world,ierror)

       out3d(:,:,1) = transpose(psloc(j1:j2,:))
    call ncceck_enspread(nf90_put_var(ncid_out, psvarid, sngl(out3d(:,:,1)), &
                      start = ncstart, count = (/ grdin%lon1, grdin%lat1 /) ))
    call mpi_barrier(mpi_comm_world,ierror)
!    ! cleanup and exit
    call ncceck_enspread(nf90_close(ncid_out))
    if ( mype == mype_out ) then
       write(6,*) "FV3-LAM netCDF ens. spread written, file= "//trim(filename)//".nc"
    end if

  end subroutine write_fv3_enspread_

  !=======================================================================

  subroutine ncceck_enspread(status)
    use netcdf, only: nf90_noerr,nf90_strerror
    implicit none
    integer, intent (in   ) :: status
    if (status /= nf90_noerr) then
      print *, "fv3 write enspread netCDF error ", trim(nf90_strerror(status))
      call stop2(999)
    end if
  end subroutine ncceck_enspread

end module write_fv3_enspread_mod
