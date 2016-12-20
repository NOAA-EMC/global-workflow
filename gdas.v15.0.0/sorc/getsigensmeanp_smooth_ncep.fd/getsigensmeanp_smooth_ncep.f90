program getsigensmeanp_smooth
!$$$  main program documentation block
!
! program:  getsigensmean              compute ensemble mean
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  create ensemble mean NCEP GFS spectral sigma file.
!
! program history log:
!   2009-02-23  Initial version.
!
! usage:
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!$$$
  
  use sigio_module, only: sigio_head,sigio_data,sigio_srohdc, &
                          sigio_swohdc,sigio_aldata,sigio_axdata
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_charkind8, &
                           nemsio_readrec,nemsio_writerec, &
                           nemsio_readrecv,nemsio_writerecv

  implicit none

  real,parameter :: zero=0.0_4
  integer,parameter :: iunit=21
  integer,parameter :: window=1 ! cosine bell window for smoothing

  logical :: lexist,dosmooth,nemsio,sigio
  logical,allocatable,dimension(:) :: notuv
  character(nemsio_charkind8) :: dtype
  character(len=3) :: charnanal
  character(len=500) :: filenamein,filenameout,filenameouts,datapath,fileprefix,fname
  character(len=16),allocatable,dimension(:) :: recnam
  integer :: iret,nlevs,ntrac,ntrunc,nanals,ngrd,k
  integer :: nsize,nsize2,nsize3,nsize3t
  integer :: mype,mype1,npe,orig_group,new_group,new_comm
  integer :: nrec,latb,lonb,npts,n,idrt
  integer,allocatable,dimension(:) :: new_group_members,reclev,krecu,krecv
  integer,allocatable,dimension(:) :: smoothparm
  real(8) :: rnanals
  real(8),allocatable,dimension(:,:,:) :: smoothfact
  real(4),allocatable,dimension(:) :: sigdatapert_ps,sigdatapert_z,sigdatapert_d,&
                                      sigdatapert_t,sigdatapert_q,sigdatapert_oz,&
                                      sigdatapert_cw
  real(4),allocatable,dimension(:,:) :: rwork_mem,rwork_avg
  real(4),allocatable,dimension(:) :: rwork_hgt
  real(4),allocatable,dimension(:) :: rwork_lev,rwork_lev2,rwork_spc,rwork_spc2


  type(sigio_head) :: sigheadi,sigheadm
  type(sigio_data) :: sigdatai,sigdatam
  type(nemsio_gfile) :: gfile,gfileo,gfileos

! mpi definitions.
  include 'mpif.h'

! Initialize mpi, mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call mpi_comm_rank(mpi_comm_world,mype,iret)
  call mpi_comm_size(mpi_comm_world,npe,iret)

  mype1 = mype + 1

  if ( mype == 0 ) call w3tagb('GETSIGENSMEAN_SMOOTH',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,filenameout)
  call getarg(3,fileprefix)
  call getarg(4,charnanal)
  read(charnanal,'(i3)') nanals

  rnanals = nanals
  rnanals = 1.0_8/rnanals
  filenameout = trim(adjustl(datapath)) // trim(adjustl(filenameout))

  if ( mype == 0 ) then
     write(6,'(a)')  'Command line input'
     write(6,'(a,a)')' datapath    = ',trim(datapath)
     write(6,'(a,a)')' filenameout = ',trim(filenameout)
     write(6,'(a,a)')' fileprefix  = ',trim(fileprefix)
     write(6,'(a,a)')' nanals      = ',trim(charnanal)
     write(6,'(a)')  ' '
  endif
  
  if ( npe < nanals ) then
     write(6,'(2(a,i4))')'***ERROR***  npe too small.  npe = ',npe,' < nanals = ',nanals
     call mpi_abort(mpi_comm_world,99,iret)
     stop
  end if
  
! Create sub-communicator to handle number of cases (nanals)
  call mpi_comm_group(mpi_comm_world,orig_group,iret)

  allocate(new_group_members(nanals))
  do k=1,nanals
     new_group_members(k)=k-1
  end do

  call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
  call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
  if ( iret /= 0 ) then
     write(6,'(a,i5)')'***ERROR*** after mpi_comm_create with iret = ',iret
     call mpi_abort(mpi_comm_world,101,iret)
  endif

  sigio  = .false.
  nemsio = .false.

! Process input files (one file per task)
  if ( mype1 <= nanals ) then

     call nemsio_init(iret)
     
     write(charnanal,'(i3.3)') mype1
     filenamein = trim(adjustl(datapath)) // &
          trim(adjustl(fileprefix)) // '_mem' // charnanal
     
!    Read each ensemble member
     call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)
     if ( iret == 0 ) then
        sigio = .true.
        write(6,'(3a,i5)')'Read sigio ',trim(filenamein),' iret = ',iret
        ntrunc  = sigheadi%jcap
        ntrac   = sigheadi%ntrac
        nlevs   = sigheadi%levs
     else
        call nemsio_open(gfile,trim(filenamein),'READ',iret=iret)
        if ( iret == 0 ) then
           nemsio = .true.
           call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
                dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, gdatatype=dtype, iret=iret)
           write(6,'(5a,i5)')'Read nemsio ',trim(filenamein), ' dtype = ', trim(adjustl(dtype)),' iret = ',iret
           allocate(reclev(nrec),recnam(nrec))
           call nemsio_getfilehead(gfile,reclev=reclev,iret=iret)
           call nemsio_getfilehead(gfile,recname=recnam,iret=iret)
        else
           write(6,'(3a)')'***ERROR*** ',trim(filenamein),' contains unrecognized format. ABORT!'
        endif
     endif
     if ( .not. nemsio .and. .not. sigio ) goto 100
     if ( mype == 0 ) then
        write(6,'(a)')   ' '
        write(6,'(2(a,l1))')'Computing ensemble mean with nemsio = ',nemsio,', sigio = ',sigio
        write(6,'(a)')   ' '
     endif

     nsize2  = (ntrunc+1)*(ntrunc+2)
     nsize3  = nsize2*nlevs
     nsize3t = nsize3*ntrac
     if ( mype == 0 ) then
        write(6,'(a)')   ' '
        write(6,'(2a)')  'Read header information from ',trim(filenamein)
        write(6,'(a,i9)')' ntrunc  = ',ntrunc
        write(6,'(a,i9)')' ntrac   = ',ntrac
        write(6,'(a,i9)')' nlevs   = ',nlevs
        write(6,'(a,i9)')' nsize2  = ',nsize2
        write(6,'(a,i9)')' nsize3  = ',nsize3
        write(6,'(a,i9)')' nsize3t = ',nsize3t
        if ( nemsio ) then
           write(6,'(a,i9)')' lonb    = ',lonb
           write(6,'(a,i9)')' latb    = ',latb
           write(6,'(a,i9)')' nrec    = ',nrec
        endif
        write(6,'(a)')   ' '
     endif

     if ( sigio ) then

!       Compute ensemble sums.
        call sigio_aldata(sigheadi,sigdatam,iret)
        call mpi_allreduce(sigdatai%z,sigdatam%z,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%d,sigdatam%d,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%t,sigdatam%t,  nsize3, mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%q,sigdatam%q,  nsize3t,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%ps,sigdatam%ps,nsize2, mpi_real,mpi_sum,new_comm,iret)
     
!       Compute ensemble mean on all tasks
        sigdatam%hs = sigdatai%hs
        sigdatam%ps = sigdatam%ps*rnanals
        sigdatam%z  = sigdatam%z*rnanals
        sigdatam%d  = sigdatam%d*rnanals
        sigdatam%t  = sigdatam%t*rnanals
        sigdatam%q  = sigdatam%q*rnanals

!       Write ensemble mean from task 0
        if ( mype == 0 ) then
           sigheadm = sigheadi
           ngrd = sigheadi%nxgr
           if ( ngrd > 0 ) sigdatam%xgr = sigdatai%xgr
           
           sigheadm%iens(1) = 1 ! unperturbed control
           sigheadm%iens(2) = 2 ! low res control
           sigheadm%icen2 = 2 ! sub-center, must be 2 or ens info not used
           call sigio_swohdc(iunit,filenameout,sigheadm,sigdatam,iret)
           write(6,'(3a,i5)')'Write sigio ensemble mean ',trim(filenameout),' iret = ',iret
        endif

     elseif ( nemsio ) then

        npts=lonb*latb
        nsize=npts*nrec
        allocate(rwork_mem(npts,nrec))
        allocate(rwork_avg(npts,nrec))
        allocate(rwork_hgt(npts))

        allocate(krecu(nlevs))
        allocate(krecv(nlevs))
        allocate(notuv(nrec ))

        krecu = 0
        krecv = 0
        notuv = .true.

        rwork_mem = zero
        do n = 1,nrec
           call nemsio_readrec(gfile,n,rwork_mem(:,n),iret=iret)
           if ( index(recnam(n),'ugrd') /= 0 ) then
              krecu(reclev(n)) = n
              notuv(n) = .false.
           endif
           if ( index(recnam(n),'vgrd') /= 0 ) then
              krecv(reclev(n)) = n
              notuv(n) = .false.
           endif
        enddo
        call nemsio_readrecv(gfile,'hgt','sfc',1,rwork_hgt,iret)

        rwork_avg = zero
        call mpi_allreduce(rwork_mem,rwork_avg,nsize,mpi_real,mpi_sum,new_comm,iret)
        rwork_avg = rwork_avg * rnanals

        if ( mype == 0 ) then
           gfileo=gfile
           call nemsio_open(gfileo,trim(filenameout),'WRITE',iret=iret )
           do n = 1,nrec
              call nemsio_writerec(gfileo,n,rwork_avg(:,n),iret=iret)
           end do
           call nemsio_writerecv(gfileo,'hgt','sfc',1,rwork_hgt,iret)
           call nemsio_close(gfileo,iret)
           write(6,'(3a,i5)')'Write nemsio ensemble mean ',trim(filenameout),' iret = ', iret
        endif

     endif

!    Read smoothing parameters, if available
     fname='hybens_smoothinfo'
     inquire(file=trim(fname),exist=lexist)
     if ( lexist ) then
        allocate(smoothparm(nlevs))
        smoothparm = -1
        open(9,form='formatted',file=fname)
        do k=1,nlevs
           read(9,'(i3)') smoothparm(k)
        enddo
        close(9)
        dosmooth = maxval(smoothparm)>0
     else
        if ( mype == 0 ) write(6,'(a)')'***WARNING***  hybens_smoothinfo not found - no smoothing'
        dosmooth = .false.
     endif
     if ( mype == 0 ) write(6,'(a,l1)')'dosmooth = ',dosmooth
     
!    If smoothing requested, loop over and smooth analysis files
     if ( dosmooth ) then

!       Set up smoother
        allocate(smoothfact(0:ntrunc,0:ntrunc,nlevs))
        smoothfact = 1.0_8
        call setup_smooth(ntrunc,nlevs,smoothparm,window,smoothfact)
        
        filenameouts = trim(adjustl(datapath)) // &
             trim(adjustl(fileprefix)) // 's' // '_mem' // charnanal

        if ( sigio ) then
        
           allocate(sigdatapert_z(nsize2),sigdatapert_d(nsize2), sigdatapert_t(nsize2), &
                sigdatapert_q(nsize2),sigdatapert_oz(nsize2),sigdatapert_cw(nsize2),&
                sigdatapert_ps(nsize2))
           
           k=1
           if (smoothparm(k)>0) then
              sigdatapert_ps  = sigdatai%ps(:)  - sigdatam%ps(:)
              call smooth(sigdatapert_ps,ntrunc,smoothfact(:,:,k))
              sigdatai%ps(:) = sigdatam%ps(:) + sigdatapert_ps
           endif
           
           do k=2,nlevs
              if (smoothparm(k)>0) then
                 sigdatapert_z  = sigdatai%z(:,k)   - sigdatam%z(:,k)
                 sigdatapert_d  = sigdatai%d(:,k)   - sigdatam%d(:,k)
                 sigdatapert_t  = sigdatai%t(:,k)   - sigdatam%t(:,k)
                 sigdatapert_q  = sigdatai%q(:,k,1) - sigdatam%q(:,k,1)
                 sigdatapert_oz = sigdatai%q(:,k,2) - sigdatam%q(:,k,2)
                 sigdatapert_cw = sigdatai%q(:,k,3) - sigdatam%q(:,k,3)
                 
                 call smooth(sigdatapert_z, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_d, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_t, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_q, ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_oz,ntrunc,smoothfact(:,:,k))
                 call smooth(sigdatapert_cw,ntrunc,smoothfact(:,:,k))
                 
                 sigdatai%z(:,k)   = sigdatam%z(:,k)   + sigdatapert_z
                 sigdatai%d(:,k)   = sigdatam%d(:,k)   + sigdatapert_d
                 sigdatai%t(:,k)   = sigdatam%t(:,k)   + sigdatapert_t
                 sigdatai%q(:,k,1) = sigdatam%q(:,k,1) + sigdatapert_q
                 sigdatai%q(:,k,2) = sigdatam%q(:,k,2) + sigdatapert_oz
                 sigdatai%q(:,k,3) = sigdatam%q(:,k,3) + sigdatapert_cw
                 
              endif
           enddo

           deallocate(sigdatapert_z, sigdatapert_d, sigdatapert_t, sigdatapert_q,&
                      sigdatapert_oz,sigdatapert_cw,sigdatapert_ps)
        
!          Write smoothed member
           call sigio_swohdc(iunit,trim(filenameouts),sigheadi,sigdatai,iret)
           write(6,'(3a,i5)')'Write smoothed sigio ',trim(filenameouts),' iret = ',iret

        elseif ( nemsio ) then

           allocate(rwork_lev(npts),rwork_lev2(npts))
           allocate(rwork_spc((ntrunc+1)*(ntrunc+2)),rwork_spc2((ntrunc+1)*(ntrunc+2)))
           idrt = 4

!          Smoothing loop over fields (first do scalar fields only)
!$omp parallel do schedule(dynamic,1) private(n,rwork_lev,rwork_spc)
           do n = 1,nrec
              if ( notuv(n) .and. smoothparm(reclev(n)) > 0 ) then
                 rwork_lev = rwork_mem(:,n) - rwork_avg(:,n)
                 call sptez(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_lev,-1)
                 call smooth(rwork_spc,ntrunc,smoothfact(:,:,reclev(n)))
                 call sptez(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_lev,1)
                 rwork_mem(:,n) = rwork_avg(:,n) + rwork_lev
              endif
           enddo

!          Smoothing loop over vector fields u and v
!$omp parallel do schedule(dynamic,1) private(k,rwork_lev,rwork_lev2,rwork_spc,rwork_spc2)
           do k = 1,nlevs
              if ( smoothparm(k) > 0 ) then
                 rwork_lev  = rwork_mem(:,krecu(k)) - rwork_avg(:,krecu(k))
                 rwork_lev2 = rwork_mem(:,krecv(k)) - rwork_avg(:,krecv(k))
                 call sptezv(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_spc2,rwork_lev,rwork_lev2,-1)
                 call smooth(rwork_spc, ntrunc,smoothfact(:,:,k))
                 call smooth(rwork_spc2,ntrunc,smoothfact(:,:,k))
                 call sptezv(0,ntrunc,idrt,lonb,latb,rwork_spc,rwork_spc2,rwork_lev,rwork_lev2,1)
                 rwork_mem(:,krecu(k)) = rwork_avg(:,krecu(k)) + rwork_lev
                 rwork_mem(:,krecv(k)) = rwork_avg(:,krecv(k)) + rwork_lev2
              endif
           enddo

           deallocate(rwork_lev,rwork_lev2)
           deallocate(rwork_spc,rwork_spc2)

!          Write smoothed member
           gfileos=gfile
           call nemsio_open(gfileos,trim(filenameouts),'WRITE',iret=iret )
           do n = 1,nrec
              call nemsio_writerec(gfileos,n,rwork_mem(:,n),iret)
           enddo

!          Write unsmoothed member orography to smoothed output file
           call nemsio_writerecv(gfileos,'hgt','sfc',1,rwork_hgt,iret)

           call nemsio_close(gfileos,iret)
           write(6,'(3a,i5)')'Write smoothed nemsio ',trim(filenameouts),' iret = ',iret

        endif

!       Deallocate smoothing factors
        deallocate(smoothfact)

!    End of smoothing block
     endif

!    Deallocate structures and arrays
     if (allocated(smoothparm)) deallocate(smoothparm)
     if ( sigio ) then
        call sigio_axdata(sigdatai,iret)
        call sigio_axdata(sigdatam,iret)
     elseif ( nemsio ) then
        call nemsio_close(gfile,iret)
        if (allocated(rwork_mem)) deallocate(rwork_mem)
        if (allocated(rwork_avg)) deallocate(rwork_avg)
        if (allocated(rwork_hgt)) deallocate(rwork_hgt)
        deallocate(krecu,krecv,notuv)
     endif

! Jump here if more mpi processors than files to process
  else
     write(6,'(a,i5)') 'No files to process for mpi task = ',mype
  endif

100 continue
  call mpi_barrier(mpi_comm_world,iret)
  
  if ( mype1 <= nanals .and. .not. nemsio .and. .not. sigio ) then
     write(6,'(a)')'***ERROR***  invalid atmospheric file format'
     call mpi_abort(mpi_comm_world,98,iret)
     stop
  endif

  if ( mype == 0 ) call w3tage('GETSIGENSMEAN_SMOOTH')
  
 deallocate(new_group_members)
 
 call mpi_finalize(iret)
 

end program getsigensmeanp_smooth

subroutine smooth(specdat,ntrunc,smoothfact)
  implicit none
  integer :: m,nm,n
  integer, intent(in) :: ntrunc
  real(8),intent(in)  ::  smoothfact(0:ntrunc,0:ntrunc) ! smoothing factor
  real(4),intent(inout) :: specdat((ntrunc+1)*(ntrunc+2))
  
  nm = 1
  m_loop: do m=0,ntrunc
     n_loop: do n=m,ntrunc
        specdat(nm)   = smoothfact(n,m)*specdat(nm)
        specdat(nm+1) = smoothfact(n,m)*specdat(nm+1)
        nm = nm + 2
     enddo n_loop
  enddo m_loop
end subroutine smooth

subroutine setup_smooth(ntrunc,nlevs,smoothparm,window,smoothfact)
  implicit none
  integer :: m,n,k
  integer, intent(in) :: ntrunc,nlevs,window
  integer,dimension(nlevs),intent(in) ::  smoothparm ! smoothing parameter.
  real(8),dimension(0:ntrunc,0:ntrunc,nlevs),intent(out) :: smoothfact
  real(8) zero,half,one,pi,smoothval,rsmoothval,waven

  zero = 0.0_8
  half = 0.5_8
  one  = 1.0_8
  pi   = 4.0_8*atan(one)

  k_loop: do k=1,nlevs
     if (smoothparm(k) .le. 0.) cycle k_loop
     smoothval=smoothparm(k)
     rsmoothval=one/smoothval
     m_loop: do m=0,ntrunc
        n_loop: do n=m,ntrunc
           waven=real(n)
           if (window .eq. 1) then
              ! Hann window (cosine bell).
              if (n <= smoothparm(k)) then
                 smoothfact(n,m,k) = half*(one + cos(pi*waven*rsmoothval))
              else
                 smoothfact(n,m,k) = zero
              endif
           else if (window .eq. 2) then
              ! gaussian window.
              smoothfact(n,m,k) = exp(-(waven*rsmoothval)**2)
           else
              ! rectangular window (simple truncation)
              if (n <= smoothparm(k)) then
                 smoothfact(n,m,k) = one
              else
                 smoothfact(n,m,k) = zero
              endif
           endif
        enddo n_loop
     enddo m_loop
  enddo k_loop
end subroutine setup_smooth

