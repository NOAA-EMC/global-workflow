module mpi_readobs
!$$$  module documentation block
!
! module: mpi_readobs                  read obs, ob priors and associated
!                                      metadata if called from root task, 
!                                      otherwise receive data from root task.
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:
!
! Public Subroutines:
!  mpi_readobs: called by subroutine readobs in module enkf_obsmod. 
!   Read obs, ob priors and metadata from diag* files
!   created by GSI forward operator code and broadcast to all tasks.
!   
! Public Variables: None
!
! Modules Used:
!  readsatobs: to read satellite radiance diag* files.
!  readconvobs: to read diag_conv* files (obs from prepbufr file).
!  readozobs: to read diag_sbuv* ozone files.
!  mpisetup
!
! program history log:
!   2009-02-23  Initial version.
!   2016-11-29  shlyaeva: Added the option of writing out ensemble spread in
!               diag files
!
! attributes:
!   language: f95
!
!$$$
  
use kinds, only: r_double,i_kind,r_kind,r_single,num_bytes_for_r_single
use params, only: ntasks_io, nanals_per_iotask, nanal1, nanal2
use radinfo, only: npred
use readconvobs
use readsatobs
use readozobs
use mpimod, only: mpi_comm_world
use mpisetup, only: mpi_real4,mpi_sum,mpi_comm_io,mpi_in_place,numproc,nproc,&
                mpi_integer,mpi_wtime,mpi_status,mpi_real8,mpi_max,mpi_realkind,&
                mpi_min,numproc_shm,mpi_comm_shmem,mpi_info_null,nproc_shm,&
                mpi_comm_shmemroot,mpi_mode_nocheck,mpi_lock_exclusive,&
                mpi_address_kind
use, intrinsic :: iso_c_binding

implicit none

private
public :: mpi_getobs

contains

subroutine mpi_getobs(obspath, datestring, nobs_conv, nobs_oz, nobs_sat, nobs_tot, &
                      nobs_convdiag, nobs_ozdiag, nobs_satdiag, nobs_totdiag, &
                      sprd_ob, ensmean_ob, ensmean_obbc, ob, &
                      oberr, oblon, oblat, obpress, &
                      obtime, oberrorig, obcode, obtype, &
                      biaspreds, diagused,  anal_ob, anal_ob_modens, anal_ob_cp, anal_ob_modens_cp, &
                      shm_win, shm_win2, indxsat, nanals, neigv)
    character*500, intent(in) :: obspath
    character*10, intent(in) :: datestring
    character(len=10) :: id,id2
    real(r_single), allocatable, dimension(:)   :: ensmean_ob,ob,oberr,oblon,oblat
    real(r_single), allocatable, dimension(:)   :: obpress,obtime,oberrorig,ensmean_obbc,sprd_ob
    integer(i_kind), allocatable, dimension(:)  :: obcode,indxsat
    integer(i_kind), allocatable, dimension(:)  :: diagused
    ! pointers used for MPI-3 shared memory manipulations.
    real(r_single), pointer, dimension(:,:)     :: anal_ob, anal_ob_modens
    type(c_ptr) anal_ob_cp, anal_ob_modens_cp
    integer shm_win, shm_win2
    real(r_single), allocatable, dimension(:,:) :: biaspreds
    real(r_single), allocatable, dimension(:)   :: mem_ob 
    real(r_single), allocatable, dimension(:,:) :: mem_ob_modens
    real(r_single) :: analsi,analsim1
    real(r_double) t1,t2
    character(len=20), allocatable,  dimension(:) ::  obtype
    integer(i_kind) nob, ierr, iozproc, isatproc, nens1, nens2, na, nmem,&
            nens, nobs_conv, nobs_oz, nobs_sat, nobs_tot, nanal
    integer(i_kind) :: nobs_convdiag, nobs_ozdiag, nobs_satdiag, nobs_totdiag
    integer(i_kind), intent(in) :: nanals, neigv
    integer(MPI_ADDRESS_KIND) :: win_size, nsize, nsize2, win_size2
    integer(MPI_ADDRESS_KIND) :: segment_size, disp_unit

    iozproc=max(0,min(1,numproc-1))
    isatproc=max(0,min(2,numproc-2))
! get total number of conventional and sat obs for ensmean.
    id = 'ensmean'
    if(nproc == 0)call get_num_convobs(obspath,datestring,nobs_conv,nobs_convdiag,id)
    if(nproc == iozproc)call get_num_ozobs(obspath,datestring,nobs_oz,nobs_ozdiag,id)
    if(nproc == isatproc)call get_num_satobs(obspath,datestring,nobs_sat,nobs_satdiag,id)
    call mpi_bcast(nobs_conv,1,mpi_integer,0,mpi_comm_world,ierr)
    call mpi_bcast(nobs_convdiag,1,mpi_integer,0,mpi_comm_world,ierr)
    call mpi_bcast(nobs_oz,1,mpi_integer,iozproc,mpi_comm_world,ierr)
    call mpi_bcast(nobs_ozdiag,1,mpi_integer,iozproc,mpi_comm_world,ierr)
    call mpi_bcast(nobs_sat,1,mpi_integer,isatproc,mpi_comm_world,ierr)
    call mpi_bcast(nobs_satdiag,1,mpi_integer,isatproc,mpi_comm_world,ierr)
    if(nproc == 0)print *,'nobs_conv, nobs_oz, nobs_sat = ',nobs_conv,nobs_oz,nobs_sat
    if(nproc == 0)print *,'total diag nobs_conv, nobs_oz, nobs_sat = ', nobs_convdiag, nobs_ozdiag, nobs_satdiag
    nobs_tot = nobs_conv + nobs_oz + nobs_sat
    nobs_totdiag = nobs_convdiag + nobs_ozdiag + nobs_satdiag
    if (neigv > 0) then
       nens = nanals*neigv ! modulated ensemble size
    else
       nens = nanals
    endif
! if nobs_tot != 0 (there were some obs to read)
    if (nobs_tot > 0) then
       allocate(mem_ob(nobs_tot)) 
       allocate(mem_ob_modens(neigv,nobs_tot))  ! zero size if neigv=0
       allocate(sprd_ob(nobs_tot),ob(nobs_tot),oberr(nobs_tot),oblon(nobs_tot),&
       oblat(nobs_tot),obpress(nobs_tot),obtime(nobs_tot),oberrorig(nobs_tot),obcode(nobs_tot),&
       obtype(nobs_tot),ensmean_ob(nobs_tot),ensmean_obbc(nobs_tot),&
       biaspreds(npred+1, nobs_sat),indxsat(nobs_sat), diagused(nobs_totdiag))
    else
! stop if no obs found (must be an error somewhere).
       print *,'no obs found!'
       call stop2(11)
    end if

! setup shared memory segment on each node that points to
! observation prior ensemble.
! shared window size will be zero except on root task of
! shared memory group on each node.
    disp_unit = int(num_bytes_for_r_single,kind=MPI_ADDRESS_KIND) ! anal_ob is r_single
    nsize = int(nobs_tot,kind=MPI_ADDRESS_KIND)*int(nanals,kind=MPI_ADDRESS_KIND)
    nsize2 = int(nobs_tot,kind=MPI_ADDRESS_KIND)*int(nanals,kind=MPI_ADDRESS_KIND)*int(neigv,kind=MPI_ADDRESS_KIND)
    if (nproc_shm == 0) then
       win_size = nsize*disp_unit
       win_size2 = nsize2*disp_unit
       if (win_size2 < 0) then
          print *,'win_size2 = ',win_size2
          print *,'problem with shared window size, stopping!'
          call stop2(11)
       endif
    else
       win_size = 0
       win_size2 = 0
    endif
    call MPI_Win_allocate_shared(win_size, disp_unit, MPI_INFO_NULL,&
                                 mpi_comm_shmem, anal_ob_cp, shm_win, ierr)
    if (neigv > 0) then
       call MPI_Win_allocate_shared(win_size2, disp_unit, MPI_INFO_NULL,&
                                    mpi_comm_shmem, anal_ob_modens_cp, shm_win2, ierr)
    endif
    ! associate fortran pointer with c pointer to shared memory 
    ! segment (containing observation prior ensemble) on each task.
    call MPI_Win_shared_query(shm_win, 0, segment_size, disp_unit, anal_ob_cp, ierr)
    call c_f_pointer(anal_ob_cp, anal_ob, [nanals, nobs_tot])
    ! initialize shared memory window.
    anal_ob=0
    if (neigv > 0) then
       call MPI_Win_shared_query(shm_win2, 0, segment_size, disp_unit, anal_ob_modens_cp, ierr)
       call c_f_pointer(anal_ob_modens_cp, anal_ob_modens, [nens, nobs_tot])
       anal_ob_modens=0
    endif

! read ensemble mean and every ensemble member
    if (nproc <= ntasks_io-1) then
        nens1 = nanal1(nproc); nens2 = nanal2(nproc)
    else
        nens1 = nanals+1; nens2 = nanals+1
    endif

    nmem = 0
    do nanal=nens1,nens2 ! loop over ens members on this task
    nmem = nmem + 1 ! nmem only used if lobsdiag_forenkf=T
    id = 'ensmean'
    id2 = id
    mem_ob=0
    if (neigv > 0) mem_ob_modens=0
    ! if nanal>nanals, ens member data not read (only ens mean)
    if (nanal <= nanals) then
       write(id2,'(a3,(i3.3))') 'mem',nanal
    endif
! read obs.
! only thing that is different on each task is mem_ob.  All other
! fields are defined from ensemble mean.
! individual members read on 1st nanals tasks, ens mean read on all tasks.
    if (nobs_conv > 0) then
! first nobs_conv are conventional obs.
      call get_convobs_data(obspath, datestring, nobs_conv, nobs_convdiag, &
        ensmean_obbc(1:nobs_conv), ensmean_ob(1:nobs_conv),                &
        mem_ob(1:nobs_conv), mem_ob_modens(1:neigv,1:nobs_conv),           &
        ob(1:nobs_conv),                                                   &
        oberr(1:nobs_conv), oblon(1:nobs_conv), oblat(1:nobs_conv),        &
        obpress(1:nobs_conv), obtime(1:nobs_conv), obcode(1:nobs_conv),    &
        oberrorig(1:nobs_conv), obtype(1:nobs_conv),                       &
        diagused(1:nobs_convdiag), id, nanal, nmem)
    end if
    if (nobs_oz > 0) then
! second nobs_oz are conventional obs.
      call get_ozobs_data(obspath, datestring, nobs_oz, nobs_ozdiag,  &
        ensmean_obbc(nobs_conv+1:nobs_conv+nobs_oz),                  &
        ensmean_ob(nobs_conv+1:nobs_conv+nobs_oz),                    &
        mem_ob(nobs_conv+1:nobs_conv+nobs_oz),                        &
        mem_ob_modens(1:neigv,nobs_conv+1:nobs_conv+nobs_oz),         &
        ob(nobs_conv+1:nobs_conv+nobs_oz),               &
        oberr(nobs_conv+1:nobs_conv+nobs_oz),            &
        oblon(nobs_conv+1:nobs_conv+nobs_oz),            &
        oblat(nobs_conv+1:nobs_conv+nobs_oz),            &
        obpress(nobs_conv+1:nobs_conv+nobs_oz),          &
        obtime(nobs_conv+1:nobs_conv+nobs_oz),           &
        obcode(nobs_conv+1:nobs_conv+nobs_oz),           &
        oberrorig(nobs_conv+1:nobs_conv+nobs_oz),        &
        obtype(nobs_conv+1:nobs_conv+nobs_oz),           &
        diagused(nobs_convdiag+1:nobs_convdiag+nobs_ozdiag),&
        id,nanal,nmem)
    end if
    if (nobs_sat > 0) then
      biaspreds = 0. ! initialize bias predictor array to zero.
! last nobs_sat are satellite radiance obs.
      call get_satobs_data(obspath, datestring, nobs_sat, nobs_satdiag, &
        ensmean_obbc(nobs_conv+nobs_oz+1:nobs_tot),       &
        ensmean_ob(nobs_conv+nobs_oz+1:nobs_tot),         &
        mem_ob(nobs_conv+nobs_oz+1:nobs_tot),                &
        mem_ob_modens(1:neigv,nobs_conv+nobs_oz+1:nobs_tot),            &
        ob(nobs_conv+nobs_oz+1:nobs_tot),                 &
        oberr(nobs_conv+nobs_oz+1:nobs_tot),              &
        oblon(nobs_conv+nobs_oz+1:nobs_tot),              &
        oblat(nobs_conv+nobs_oz+1:nobs_tot),              &
        obpress(nobs_conv+nobs_oz+1:nobs_tot),            &
        obtime(nobs_conv+nobs_oz+1:nobs_tot),             &
        obcode(nobs_conv+nobs_oz+1:nobs_tot),             &
        oberrorig(nobs_conv+nobs_oz+1:nobs_tot),          &
        obtype(nobs_conv+nobs_oz+1:nobs_tot),             &
        biaspreds,indxsat,                                &
        diagused(nobs_convdiag+nobs_ozdiag+1:nobs_totdiag),&
        id,nanal,nmem)
    end if ! read obs.

!   populate obs prior ensemble shared array pointer on each io task.
    if (nproc <= ntasks_io-1) then
       anal_ob(nmem+nproc*nanals_per_iotask,:) = mem_ob(:)
       if (neigv > 0) then
          na = nmem+nproc*nanals_per_iotask
          anal_ob_modens(neigv*(na-1)+1:neigv*na,:) = mem_ob_modens(:,:)
       endif
    endif

    enddo ! nanal loop (loop over ens members on each task)
    ! need this to prevent race condition on shared memory window
    call mpi_barrier(mpi_comm_world,ierr) 
   
! obs prior ensemble now defined on root task, bcast to other tasks.
    if (nproc == 0) print *,'broadcast ob prior ensemble'
    if (nproc == 0) t1 = mpi_wtime()
! exchange obs prior ensemble members across all tasks to fully populate shared
! memory array pointer on each node.
    if (nproc_shm == 0) then
       if (real(nanals)*real(nobs_tot) < 2_r_kind**32/2_r_kind - 1_r_kind) then
          call mpi_allreduce(mpi_in_place,anal_ob,nanals*nobs_tot,mpi_real4,mpi_sum,mpi_comm_shmemroot,ierr)
       else
          ! count won't fit in 32-bit integer and mpi_allreduce doesn't handle
          ! 64 bit counts.  Split up into smaller chunks.
          mem_ob = 0.
          do na=1,nanals
              mem_ob(:) = anal_ob(na,:)
              call mpi_allreduce(mpi_in_place,mem_ob,nobs_tot,mpi_real4,mpi_sum,mpi_comm_shmemroot,ierr)
              anal_ob(na,:) = mem_ob(:)
          enddo
       endif
       !print *,nproc,'min/max anal_ob',minval(anal_ob),maxval(anal_ob)
       if (neigv > 0) then
          mem_ob_modens = 0.
          do na=1,nanals
             mem_ob_modens(:,:) = anal_ob_modens(neigv*(na-1)+1:neigv*na,:)
             call mpi_allreduce(mpi_in_place,mem_ob_modens,neigv*nobs_tot,mpi_real4,mpi_sum,mpi_comm_shmemroot,ierr)
             anal_ob_modens(neigv*(na-1)+1:neigv*na,:) = mem_ob_modens(:,:)
          enddo
       endif
    endif
    if (nproc == 0) then
        t2 = mpi_wtime()
        print *,'time to broadcast ob prior ensemble = ',t2-t1
    endif
    if (allocated(mem_ob)) deallocate(mem_ob)
    if (allocated(mem_ob_modens)) deallocate(mem_ob_modens)
    call mpi_barrier(mpi_comm_world,ierr)
    !print *,nproc,'min/max anal_ob',minval(anal_ob),maxval(anal_ob)

! make anal_ob contain ob prior ensemble *perturbations*
    analsi=1._r_single/float(nanals)
    analsim1=1._r_single/float(nanals-1)
    do nob=1,nobs_tot
       ensmean_obbc(nob)  = sum(anal_ob(:,nob))*analsi
    enddo
    if (nproc_shm == 0) then
       do nob=1,nobs_tot
! remove ensemble mean from each member.
! ensmean_obbc is biascorrected ensemble mean (anal_ob is ens pert)
          anal_ob(:,nob) = anal_ob(:,nob)-ensmean_obbc(nob)
       enddo
       if (neigv > 0) then
          do nob=1,nobs_tot
             anal_ob_modens(:,nob) = anal_ob_modens(:,nob)-ensmean_obbc(nob)
          enddo
       endif
    endif
    call mpi_barrier(mpi_comm_world,ierr)
    do nob=1,nobs_tot
! compute sprd
       sprd_ob(nob) = sum(anal_ob(:,nob)**2)*analsim1
    enddo    
! modulated ensemble.
    if (neigv > 0) then
        do nob=1,nobs_tot
          sprd_ob(nob) = sum(anal_ob_modens(:,nob)**2)*analsim1
        enddo
    endif
    if (nproc == 0) then
       print *, 'prior spread conv: ', minval(sprd_ob(1:nobs_conv)), maxval(sprd_ob(1:nobs_conv))
       print *, 'prior spread oz: ', minval(sprd_ob(nobs_conv+1:nobs_conv+nobs_oz)), &
                                     maxval(sprd_ob(nobs_conv+1:nobs_conv+nobs_oz))
       print *, 'prior spread sat: ',minval(sprd_ob(nobs_conv+nobs_oz+1:nobs_tot)), &
                                     maxval(sprd_ob(nobs_conv+nobs_oz+1:nobs_tot))
       do nob =nobs_conv+nobs_oz+1 , nobs_tot
          if (sprd_ob(nob) > 1000.) then 
             print *, nob, trim(obtype(nob)),ob(nob),' sat spread: ', sprd_ob(nob), ', ensmean_ob: ', ensmean_obbc(nob), &
                           ', anal_ob: ', anal_ob(:,nob)
          endif
       enddo
    endif

 end subroutine mpi_getobs

end module mpi_readobs
