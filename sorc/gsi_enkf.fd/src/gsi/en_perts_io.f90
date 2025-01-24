subroutine en_perts_get_from_save_fulldomain
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    en_perts_get_from_save  get content of en_perts from saved
!                                        files on each member
!   prgmmr: Hu               org: np22                date: 2015-04-12
!
! abstract: get en_perts from save.
!
!
! program history log:
!   2015-04-12  Hu     , initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!
!$$$ end documentation block

  use gridmod, only: regional
  use hybrid_ensemble_parameters, only: en_perts
  use hybrid_ensemble_parameters, only: n_ens,grd_ens
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpimod, only: ierror,mype
  use kinds, only: r_kind,i_kind,r_single
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use mpeu_util, only: die
  use general_sub2grid_mod, only: sub2grid_info,general_sub2grid_create_info
  use general_sub2grid_mod, only: general_grid2sub
  use mpimod, only: mpi_comm_world,ierror,mype,mpi_rtype,mpi_mode_rdonly
  use mpimod, only: mpi_rtype,mpi_info_null,mpi_offset_kind

  implicit none

  type(sub2grid_info) grd_arw
  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2

  integer(i_kind) n
  character(255) filename
  integer(i_kind) istatus

  integer(i_kind) ic3,ic2
  integer(i_kind) iunit

  integer(i_kind) :: count
  integer(i_kind) :: i,j,k,im,jm,km
  integer(mpi_offset_kind) :: offset
  integer(i_kind) inner_vars,num_fields
  logical,allocatable :: vector(:)
  real(r_kind),allocatable :: work_sub(:,:,:,:),work_grd(:,:,:,:)
  integer(i_kind) :: iret


  iunit=20
!
  inner_vars=1
  num_fields=nc3d*grd_ens%nsig+nc2d
  allocate(vector(num_fields))
  vector=.false.

  if(mype==0) write(*,*)  &
           ' grd_ens==',inner_vars,grd_ens%nlat,grd_ens%nlon,grd_ens%nsig,num_fields,regional
  call general_sub2grid_create_info(grd_arw,inner_vars,  &
                        grd_ens%nlat,grd_ens%nlon,grd_ens%nsig, &
                        num_fields,regional,vector)

  ! Assume all goes well
  iret = 0

  im=grd_arw%lat2
  jm=grd_arw%lon2
  km=grd_arw%nsig

  do n=1,n_ens
     write(filename,'(a,I3.3)') 'enspreproc_arw_mem',n
     if(mype==0) write(*,*) 'read in perturbation from member ',trim(filename)
!

     call mpi_file_open(mpi_comm_world,trim(filename), &
                      mpi_mode_rdonly,mpi_info_null,iunit,ierror)
     if ( ierror /= 0 ) then
        write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_OPEN failed on task = ', &
                                mype, ' ierror = ', ierror
        iret = ierror
        write(6,*)'PREPROC_READ_GFSATM: ***ERROR*** reading ',&
                 trim(filename),' IRET=',iret
        return
     endif

     allocate(work_grd(grd_arw%inner_vars,grd_arw%nlat,grd_arw%nlon,grd_arw%kbegin_loc:grd_arw%kend_alloc))
     count  = grd_arw%nlat * grd_arw%nlon *  grd_arw%nlevs_alloc
     offset = grd_arw%nlat * grd_arw%nlon * (grd_arw%kbegin_loc-1) * r_kind
     call mpi_file_read_at(iunit,offset,work_grd,count,mpi_rtype,istatus,ierror)                
     if ( ierror /= 0 ) then
        write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_READ_AT failed on task =', &
                                mype, ' ierror = ', ierror
        iret = ierror
        write(6,*)'PREPROC_READ_GFSATM: ***ERROR*** reading ',&
                 trim(filename),' IRET=',iret
        return
     endif

     call mpi_file_close(iunit,ierror)
     if ( ierror /= 0 ) then
        write(6,'(a,i5,a,i5,a)') '***ERROR***  MPI_FILE_CLOSE failed on task = ',&
                                   mype, ' ierror = ', ierror
        iret = ierror
        write(6,*)'PREPROC_READ_GFSATM: ***ERROR*** reading ',&
                 trim(filename),' IRET=',iret
        return
     endif

     allocate(work_sub(grd_arw%inner_vars,im,jm,grd_arw%num_fields))

     call general_grid2sub(grd_arw,work_grd,work_sub)

     deallocate(work_grd)

     do ic3=1,nc3d

        call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars3d(ic3)),w3,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
           call stop2(999)
        end if

        do k=1,grd_arw%nsig
            do j=1,grd_arw%lon2
               do i=1,grd_arw%lat2
                  w3(i,j,k) = work_sub(1,i,j,(ic3-1)*grd_arw%nsig+k)
               end do
            end do
        end do

     end do

     do ic2=1,nc2d

        call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars2d(ic2)),w2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
           call stop2(999)
        end if

        do j=1,grd_arw%lon2
            do i=1,grd_arw%lat2
               w2(i,j) = work_sub(1,i,j,nc3d*grd_arw%nsig+ic2)
            end do
        end do
     end do
!
     deallocate(work_sub)
  end do   ! member n

  return

end subroutine en_perts_get_from_save_fulldomain

subroutine en_perts_get_from_save
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    en_perts_get_from_save  get content of en_perts from saved
!                                        files on each subdomain.
!   prgmmr: Hu               org: np22                date: 2015-01-22
!
! abstract: get en_perts from save.
!
!
! program history log:
!   2014-01-22  Hu     , initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: en_perts,ps_bar
  use hybrid_ensemble_parameters, only: n_ens
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpimod, only: mype
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: max_varname_length
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use mpeu_util, only: die
  implicit none

  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2

  integer(i_kind) n
  character(255) filename
  character(len=max_varname_length) varname
  integer(i_kind) istatus

  integer(i_kind) ic3,ic2
  integer(i_kind) iunit,nn

  iunit=20
  write(filename,'(a,I4.4)') 'saved_en_perts.pe',mype
  open(iunit,file=trim(filename),form='unformatted')
  do n=1,n_ens
!
     read(iunit) nn
     if(nn /= n) then
        write(6,*)' error in ensemble number. read in ',nn,' looking for ',n
        call stop2(999)
     endif
     read(iunit) ps_bar(:,:,1)
!
     do ic3=1,nc3d

        call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars3d(ic3)),w3,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
           call stop2(999)
        end if

        read(iunit) varname
        if(trim(varname) == trim(cvars3d(ic3))) then
           read(iunit) w3
        else
           write(*,*) 'error match field: read in ',trim(varname), &
                      ' in cvars3d',trim(cvars3d(ic3))
        endif
     end do

     do ic2=1,nc2d

        call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars2d(ic2)),w2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
           call stop2(999)
        end if

        read(iunit) varname
        if(trim(varname) == trim(cvars2d(ic2))) then
           read(iunit) w2
        else
           write(*,*) 'error match field: read in ',trim(varname), &
                      ' in cvars2d',trim(cvars2d(ic2))
        endif
     end do
  end do

  close(iunit)
  return

end subroutine en_perts_get_from_save

subroutine en_perts_save
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    en_perts_save  save content in en_perts to a file.
!   prgmmr: Hu               org: np22                date: 2015-01-22
!
! abstract: save en_perts.
!
!
! program history log:
!   2014-01-22  Hu     , initial documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!
!$$$ end documentation block

  use hybrid_ensemble_parameters, only: en_perts,ps_bar
  use hybrid_ensemble_parameters, only: n_ens
  use control_vectors, only: cvars2d,cvars3d,nc2d,nc3d
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use mpimod, only: mype
  use kinds, only: r_kind,i_kind,r_single
  use gsi_bundlemod, only: GSI_BundleGetPointer
  use mpeu_util, only: die
  use hybrid_ensemble_parameters, only: nsclgrp
  implicit none

  real(r_single),pointer,dimension(:,:,:):: w3
  real(r_single),pointer,dimension(:,:):: w2

  integer(i_kind) n
  character(255) filename
  integer(i_kind) istatus

  integer(i_kind) ic3,ic2
  integer(i_kind) iunit
  if(nsclgrp>1) then
    write(6,*)"nsclgrp >1 is not considerred in this part, stop"
    stop
  endif

  iunit=20
  write(filename,'(a,I4.4)') 'saved_en_perts.pe',mype
  if(mype==0) write(*,*) 'save en_perts as ', trim(filename)
  open(iunit,file=trim(filename),form='unformatted')
  do n=1,n_ens
!
     write(iunit) n
     write(iunit) ps_bar(:,:,1)
!
     do ic3=1,nc3d

        call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars3d(ic3)),w3,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars3d(ic3)),' for ensemble member ',n
           call stop2(999)
        end if

        write(iunit) cvars3d(ic3)
        write(iunit) w3

     end do
     do ic2=1,nc2d

        call gsi_bundlegetpointer(en_perts(n,1,1),trim(cvars2d(ic2)),w2,istatus)
        if(istatus/=0) then
           write(6,*)' error retrieving pointer to ',trim(cvars2d(ic2)),' for ensemble member ',n
           call stop2(999)
        end if

        write(iunit) cvars2d(ic2)
        write(iunit) w2

     end do
  end do

  close(iunit)
  return

end subroutine en_perts_save
