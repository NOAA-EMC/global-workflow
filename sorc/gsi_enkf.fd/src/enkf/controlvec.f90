module controlvec
!$$$  module documentation block
!
! module: controlvec       read ensemble members, write out
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract: ensemble IO.
!
! Public Subroutines:
!  init_controlvec: read anavinfo table for EnKF control vector
!  read_control:    read ensemble members for control variables on root
!  write_control:   write out ensemble members. Optionally save ensemble mean analysis increment. (!!!!!)
!  controlvec_cleanup: deallocate allocatable arrays.
!
! Public Variables:
!  nanals: (integer scalar) number of ensemble members (from module params)
!  nlevs: number of analysis vertical levels (from module params).
!  nbackgrounds:  number of time levels in background
!
!  nc3d: number of 3D control variables
!  nc2d: number of 2D control variables
!  cvars3d: names of 3D control variables
!  cvars2d: names of 2D control variables
!  ncdim: total number of 2D fields to update (nc3d*nlevs+nc2d)
!  index_pres: an index array with pressure value for given state variable
!   
! Modules Used: mpisetup, params, kinds, gridio, gridinfo, mpeu_util, constants
!
! program history log:
!   2009-02-23  Initial version (as statevec).
!   2009-11-28  revamped to improve IO speed
!   2015-06-29  add multiple time levels to background
!   2016-05-02  shlyaeva: Modification for reading state vector from table
!   2016-09-07  shlyaeva: moved distribution of ens members to loadbal 
!   2016-11-29  shlyaeva: module renamed to controlvec (from statevec); gridinfo 
!               init and cleanup are called from here now

!
! attributes:
!   language: f95
!
!$$$

use mpimod, only: mpi_comm_world
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc,&
                mpi_integer,mpi_wtime,mpi_status,mpi_real8

use gridio,    only: readgriddata, readgriddata_pnc, writegriddata, writegriddata_pnc, &
                     writeincrement, writeincrement_pnc
use gridinfo,  only: getgridinfo, gridinfo_cleanup,                    &
                     npts, vars3d_supported, vars2d_supported
use params,    only: nlevs, nbackgrounds, fgfileprefixes, reducedgrid, &
                     nanals, pseudo_rh, nlons, nlats,&
                     nanals_per_iotask, ntasks_io, nanal1, nanal2, &
                     fgsfcfileprefixes, paranc, write_fv3_incr, write_ensmean
use kinds,     only: r_kind, i_kind, r_double, r_single
use mpeu_util, only: gettablesize, gettable, getindex
use constants, only: max_varname_length
implicit none

private

public :: read_control, write_control, controlvec_cleanup, init_controlvec
real(r_single), public, allocatable, dimension(:,:,:,:) :: grdin
real(r_double), public, allocatable, dimension(:,:,:,:) :: qsat

integer(i_kind), public :: nc2d, nc3d, ncdim
character(len=max_varname_length), allocatable, dimension(:), public :: cvars3d
character(len=max_varname_length), allocatable, dimension(:), public :: cvars2d
integer(i_kind), public, allocatable, dimension(:) :: index_pres
integer(i_kind), public, allocatable, dimension(:) :: clevels

contains

subroutine init_controlvec()
! read table with control vector variables
! (code adapted from GSI state_vectors.f90 init_anasv routine
implicit none
character(len=*),parameter:: rcname='anavinfo'
character(len=*),parameter:: tbname='control_vector_enkf::'
character(len=256),allocatable,dimension(:):: utable
character(len=20) var,source,funcof
integer(i_kind) luin,ii,i,ntot, k, nvars
integer(i_kind) ilev, itracer

! load file
luin=914
open(luin,file=rcname,form='formatted')

! Scan file for desired table first
! and get size of table
call gettablesize(tbname,luin,ntot,nvars)

! Get contents of table
allocate(utable(nvars))
call gettable(tbname,luin,ntot,nvars,utable)

! release file unit
close(luin)

! Retrieve each token of interest from table and define
! variables participating in control vector

! Count variables first
nc2d=0; nc3d=0; ncdim=0;
do ii=1,nvars
   read(utable(ii),*) var, ilev, itracer, source, funcof
   if(ilev==1) then
       nc2d=nc2d+1
       ncdim=ncdim+1
   else
       nc3d=nc3d+1
       ncdim=ncdim+ilev
   endif
enddo

allocate(cvars3d(nc3d),cvars2d(nc2d),clevels(0:nc3d))

! Now load information from table
nc2d=0;nc3d=0
clevels = 0
do ii=1,nvars
   read(utable(ii),*) var, ilev, itracer, source, funcof
   if(ilev==1) then
      nc2d=nc2d+1
      cvars2d(nc2d)=trim(adjustl(var))
   else if (ilev==nlevs .or. ilev==nlevs+1) then
      nc3d=nc3d+1
      cvars3d(nc3d) = trim(adjustl(var))
      clevels(nc3d) = ilev + clevels(nc3d-1)
   else 
      if (nproc .eq. 0) print *,'Error controlvec: only ', nlevs, ' and ', nlevs+1,' number of levels is supported in current version, got ',ilev
      call stop2(503)
   endif
enddo

deallocate(utable)

allocate(index_pres(ncdim))
ii=0
do i=1,nc3d
  do k=1,clevels(i)-clevels(i-1)
    ii = ii + 1
    index_pres(ii)=k
  end do
end do
do i = 1,nc2d
  ii = ii + 1
  index_pres(ii) = nlevs+1
enddo

! sanity checks
if (ncdim == 0) then
  if (nproc == 0) print *, 'Error: there are no variables to update.'
  call stop2(501)
endif

do i = 1, nc2d
  if (getindex(vars2d_supported, cvars2d(i))<0) then
    if (nproc .eq. 0) then
      print *,'Error: control 2D variable ', cvars2d(i), ' is not supported in current version.'
      print *,'Supported variables: ', vars2d_supported
    endif
    call stop2(502)
  endif
enddo
do i = 1, nc3d
  if (getindex(vars3d_supported, cvars3d(i))<0) then
    if (nproc .eq. 0) then 
       print *,'Error: control 3D variable ', cvars3d(i), ' is not supported in current version.'
       print *,'Supported variables: ', vars3d_supported
    endif
    call stop2(502)
  endif
enddo

if (nproc == 0) then 
  print *, '2D control variables: ', cvars2d
  print *, '3D control variables: ', cvars3d
  print *, 'Control levels: ', clevels
  print *, 'nc2d: ', nc2d, ', nc3d: ', nc3d, ', ncdim: ', ncdim
endif

call getgridinfo(fgfileprefixes(nbackgrounds/2+1), reducedgrid)

end subroutine init_controlvec


subroutine read_control()
! read ensemble members on IO tasks
implicit none
real(r_double)  :: t1,t2
integer(i_kind) :: nb,nlev,ne
integer(i_kind) :: q_ind
integer(i_kind) :: ierr

! must at least nanals tasks allocated.
if (numproc < ntasks_io) then
  print *,'need at least ntasks =',ntasks_io,'MPI tasks, exiting ...'
  call mpi_barrier(mpi_comm_world,ierr)
  call mpi_finalize(ierr)
end if
if (npts < numproc) then
  print *,'cannot allocate more than npts =',npts,'MPI tasks, exiting ...'
  call mpi_barrier(mpi_comm_world,ierr)
  call mpi_finalize(ierr)
end if

! read in whole control vector on i/o procs - keep in memory 
! (needed in write_ensemble)
allocate(grdin(npts,ncdim,nbackgrounds,nanals_per_iotask))
! if only updating the sfc fields, qsat will not be calculated in readgriddata
! only allocate if needed.
q_ind = getindex(cvars3d, 'q')
if (q_ind > 0)  allocate(qsat(npts,nlevs,nbackgrounds,nanals_per_iotask))
if (paranc) then
   if (nproc == 0) t1 = mpi_wtime()
   call readgriddata_pnc(cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,nbackgrounds, &
           fgfileprefixes,fgsfcfileprefixes,reducedgrid,grdin,qsat)
   if (nproc == 0) then
     t2 = mpi_wtime()
     print *,'time in readgrid_pnc on root',t2-t1,'secs'
   end if
end if
if (nproc <= ntasks_io-1) then
   if (.not. paranc) then
      if (nproc == 0) t1 = mpi_wtime()
      call readgriddata(nanal1(nproc),nanal2(nproc),cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,nbackgrounds, &
           fgfileprefixes,fgsfcfileprefixes,reducedgrid,grdin,qsat)
      if (nproc == 0) then
        t2 = mpi_wtime()
        print *,'time in readgrid on root',t2-t1,'secs'
      end if
   end if
   !print *,'min/max qsat',nanal,'=',minval(qsat),maxval(qsat)
   q_ind = getindex(cvars3d, 'q')
   if (pseudo_rh .and. q_ind > 0) then
     do ne=1,nanals_per_iotask
     do nb=1,nbackgrounds
        ! create normalized humidity analysis variable.
        grdin(:,(q_ind-1)*nlevs+1:q_ind*nlevs,nb,ne) = &
        grdin(:,(q_ind-1)*nlevs+1:q_ind*nlevs,nb,ne)/qsat(:,:,nb,ne)
     enddo
     enddo
   end if

endif

end subroutine read_control

subroutine write_control(no_inflate_flag)
! write out each ensemble member to a separate file.
! for now, first nanals tasks are IO tasks.
implicit none
logical, intent(in) :: no_inflate_flag

real(r_double)  :: t1,t2
integer(i_kind) :: nb, nvar,ne,nn
integer(i_kind) :: q_ind, ierr
real(r_single), allocatable, dimension(:,:) :: grdin_mean_tmp
real(r_single), allocatable, dimension(:,:,:,:) :: grdin_mean

character(len=max_varname_length), dimension(nc3d) :: no_vars3d

if (nproc <= ntasks_io-1) then

   ! scale q by ensemble qsat, prior to averaging
   q_ind = getindex(cvars3d, 'q')
   if (pseudo_rh .and. q_ind > 0) then
     do ne=1,nanals_per_iotask
     do nb=1,nbackgrounds
        grdin(:,(q_ind-1)*nlevs+1:q_ind*nlevs,nb,ne) = &
        grdin(:,(q_ind-1)*nlevs+1:q_ind*nlevs,nb,ne)*qsat(:,:,nb,ne)
    enddo
    enddo
   endif


   allocate(grdin_mean_tmp(npts,ncdim))
   if (nproc == 0) then
     allocate(grdin_mean(npts,ncdim,nbackgrounds,1))
     grdin_mean = 0_r_single
     t1 = mpi_wtime()
   endif
   
   do nb=1,nbackgrounds
      if (nproc == 0) then
         print *,'time level ',nb
         print *,'--------------'
      endif
      ! gather ensmean increment on root.
      if (real(npts)*real(ncdim) < 2_r_kind**32/2_r_kind - 1_r_kind) then
         do ne=1,nanals_per_iotask
            call mpi_reduce(grdin(:,:,nb,ne), grdin_mean_tmp, npts*ncdim, mpi_real4,&
                            mpi_sum,0,mpi_comm_io,ierr)
            if (nproc == 0) grdin_mean(:,:,nb,1) = grdin_mean(:,:,nb,1) + grdin_mean_tmp
         enddo
      else
         do nn=1,ncdim
         do ne=1,nanals_per_iotask
            call mpi_reduce(grdin(:,nn,nb,ne), grdin_mean_tmp(:,nn), npts, mpi_real4,&
                            mpi_sum,0,mpi_comm_io,ierr)
            if (nproc == 0) grdin_mean(:,nn,nb,1) = grdin_mean(:,nn,nb,1) + grdin_mean_tmp(:,nn)
         enddo
         enddo
      endif
      ! print out ens mean increment info
      if (nproc == 0) then
         grdin_mean(:,:,nb,1) = grdin_mean(:,:,nb,1)/real(nanals)
         do nvar=1,nc3d
            write(6,100) trim(cvars3d(nvar)),   &
                minval(grdin_mean(:,clevels(nvar-1)+1:clevels(nvar),nb,1)),     &
                maxval(grdin_mean(:,clevels(nvar-1)+1:clevels(nvar),nb,1))
         enddo
         do nvar=1,nc2d
            write(6,100) trim(cvars2d(nvar)),   &
                minval(grdin_mean(:,clevels(nc3d) + nvar,nb,1)),                &
                maxval(grdin_mean(:,clevels(nc3d) + nvar,nb,1))
         enddo
      endif
   enddo
100 format('ens. mean anal. increment min/max  ',a,2x,g19.12,2x,g19.12)
   deallocate(grdin_mean_tmp)

   if (.not. paranc) then
      if (write_fv3_incr) then
         call writeincrement(nanal1(nproc),nanal2(nproc),cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin,no_inflate_flag)
      else
         call writegriddata(nanal1(nproc),nanal2(nproc),cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin,no_inflate_flag)
      end if
      if (nproc == 0) then
        if (write_ensmean) then
           ! also write out ens mean on root task.
           if (write_fv3_incr) then
              call writeincrement(0,0,cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin_mean,no_inflate_flag)
           else
              call writegriddata(0,0,cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin_mean,no_inflate_flag)
           end if
        elseif (nc2d>0) then  ! always write sfc mean increment for land analysis
           no_vars3d=''
           call writeincrement(0,0,no_vars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin_mean,no_inflate_flag)
        endif
        deallocate(grdin_mean)
        t2 = mpi_wtime()
        print *,'time in write_control on root',t2-t1,'secs'
      endif 
   end if

end if ! io task

if (paranc) then
   if (write_fv3_incr) then
      call writeincrement_pnc(cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin,no_inflate_flag)
   else
      call writegriddata_pnc(cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin,no_inflate_flag)
   end if
   if (nproc == 0) then
     ! also write out ens mean on root task
     if (write_ensmean) then ! FIXME use parallel IO to write ensmean
        if (write_fv3_incr) then
           call writeincrement(0,0,cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin_mean,no_inflate_flag)
        else
           call writegriddata(0,0,cvars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin_mean,no_inflate_flag)
        end if
     elseif (nc2d>0) then  ! always write sfc mean increment
        no_vars3d=''
        call writeincrement(0,0,no_vars3d,cvars2d,nc3d,nc2d,clevels,ncdim,grdin_mean,no_inflate_flag)
     endif
     deallocate(grdin_mean)
     t2 = mpi_wtime()
     print *,'time in write_control paranc on root',t2-t1,'secs'
   endif 
end if

end subroutine write_control

subroutine controlvec_cleanup()
! deallocate module-level allocatable arrays.
if (allocated(cvars3d)) deallocate(cvars3d)
if (allocated(cvars2d)) deallocate(cvars2d)
if (allocated(clevels)) deallocate(clevels)
if (allocated(index_pres)) deallocate(index_pres)
if (allocated(grdin)) deallocate(grdin)
if (allocated(qsat)) deallocate(qsat)
call gridinfo_cleanup()
end subroutine controlvec_cleanup

end module controlvec
