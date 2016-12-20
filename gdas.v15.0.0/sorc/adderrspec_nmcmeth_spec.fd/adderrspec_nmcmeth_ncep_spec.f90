program adderrspec_nmcmeth
!$$$  main program documentation block
!
! program:  adderrspec_nmcmeth         add perturbation
!
! prgmmr: whitaker         org: esrl/psd               date: 2009-02-23
!
! abstract:  Add samples of 48-24 forecast differences with a
!            a specified amplitude and zero mean to analysis ensemle. 
!            Initial dates for forecasts are read in from dates.dat 
!            (this file must be created beforehand). Ensemble perts 
!            are smoothed, ens mean written out.
!
! program history log:
!   2009-02-23  Initial version.
!
! usage:
!   input files:
!     sfg_YYYYMMDDHH_fhr06_ensmean - ensemble mean forecast
!     sanl_YYYYMMDDHH_ensmean - ensemble mean analysis
!     sanl_YYYYMMDDHH_mem* - ensemble member analyses
!     hybens_smoothinfo - level dependent smoothing parameters
!     sigf48_f24.gfs* - f48-f24 forecast differences
!     dates_seq.dat - sequential perturbation dates
!
!   output files:
!     sanlp_YYYYMMDDHH_mem* - perturbed ensemble members
!     sanlpr_YYYYMMDDHH_mem* - perturbed and recentered ensemble members
!     dates_ran.dat - randomized perturbation dates
!
! attributes:
!   language: f95
!
!$$$

  use sigio_module
  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,&
       nemsio_readrec,nemsio_writerec,nemsio_readrecv,nemsio_writerecv
  use mersenne_twister, only: random_setseed, random_number
  implicit none

  real,parameter :: zero=0.0_4
  
  logical :: meanonly,lexist,nemsio,sigio
  
  character(len=3)   :: charnanal
  character(len=4)   :: string
  character(len=10)  :: datestring, datestringpert
  character(len=500) :: filenamein,filenameout,filenamepert, &
       datapath,filenameoutmean,fname,filenameoutr
  character(len=10),allocatable,dimension(:):: datepert

  integer :: iargc,iret,npe,mype,nanals
  integer :: mype1,orig_group, new_group, new_comm
  integer :: nlevs,ntrac,ntrunc,nc,i,j,k,iscalefact
  integer :: iunit,iunitsf,iunitmean,iunitp,iunitpr
  integer :: npert,window,iseed
  integer:: nrec,latb,lonb,npts,n,nsize
  integer,dimension(4) :: iadate,idateout
  integer,dimension(:),allocatable:: new_group_members
  integer,allocatable,dimension(:) ::iwork,smoothparm

  real :: scalefact,rnanals
  real(8) :: rseed
  real,allocatable,dimension(:):: rwork
  real,allocatable,dimension(:,:)   :: rwork1d,swork1d
  real(4),allocatable,dimension(:):: twork1d

  type(sigio_head) :: sigheado,sigheadi,sigheadim,sigheadpin
  type(sigio_data) :: sigdata,sigdatai,sigdataim,sigdatap,sigdatapm,sigdatao,sigdatapin

  type(nemsio_gfile) :: gfile, gfileo, gfilei

! mpi definitions.
  include 'mpif.h'

! -----------------------------------------------------------------------------
! MPI setup
!   mype is process number, npe is total number of processes.

  call mpi_init(iret)
  call mpi_comm_rank(mpi_comm_world,mype,iret)
  call mpi_comm_size(mpi_comm_world,npe,iret)
  
  if (mype==0) call w3tagb('ADDERRSPEC_NMCMETH',2011,0319,0055,'NP25')

! Get command line arguments
!   nanals,datestring,scalefact,datapath,npert

  call getarg(1,string)
  read(string,'(i4)') nanals
  rnanals=1.0/float(nanals)

  call getarg(2,datestring)
  read(datestring,'(i4,i2,i2,i2)') iadate(1),iadate(2),iadate(3),iadate(4)

! scalefact is scaling factor for 48-24 forecast differences.
  call getarg(3,string)
  read(string,'(i4)') iscalefact
  scalefact = iscalefact/100.

  call getarg(4,datapath)
  call getarg(5,string)
  read(string,'(i4)') npert

  if (iargc() > 5) then
     meanonly=.true.
  else
     meanonly=.false.
  endif

! Hardwire smoothing option
  window = 1 ! cosine bell window for smoothing

  if (mype == 0) then
     write(6,*)'number of arguments ',iargc(),' meanonly ',meanonly
     write(6,*)' nanals= ',nanals,' rnanals= ',rnanals
     write(6,*)' datestring= ',trim(datestring),' iadate=',iadate
     write(6,*)' iscalefact= ',iscalefact,' scalefact=',scalefact
     write(6,*)' datapath= ',trim(datapath)
     write(6,*)' npert= ',npert,' meanonly ',meanonly
     write(6,*)' window= ',window
  endif
  
  if (npe < nanals) then
     write(6,*)'***ERROR** npe=',npe,' too small.  nanals=',nanals
     flush(6)
     flush(0)
     call mpi_abort(mpi_comm_world,101,iret)
     stop
  end if

! Map processor number to ensemble member
  mype1 = mype + 1


! Create sub-communicator to handle number of cases (nanals)
  call mpi_comm_group(mpi_comm_world,orig_group,iret)

  allocate(new_group_members(nanals))
  do k=1,nanals
     new_group_members(k)=k-1
  end do
  new_group=orig_group
  if (mype1 <= nanals) then
     call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
  endif
  call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
  if (iret.ne.0) then
     write(6,*)'***ERROR*** after mpi_comm_create with iret=',iret
     call mpi_abort(mpi_comm_world,101,iret)
  endif
  
! Generate random numbers to select perturbations.  
! Use analysis date as random seed
  rseed = 1.0e6_8*iadate(1) + 1.0e4_8*iadate(2) + 1.0e2_8*iadate(3) + iadate(4)
  iseed = rseed
  call random_setseed(iseed)
  
! Generate random number and map into range
  allocate(rwork(nanals),iwork(nanals))
  call random_number(rwork)
  do i=1,nanals
     iwork(i)=rwork(i)*npert+1
     iwork(i)=min(max(1,iwork(i)),npert)
  end do

! Randomize dates (done on single task)
  if (mype == 0) then
     allocate(datepert(npert))
     
!    Read file with sequential pertubration dates.
     open(9,form='formatted',file='dates_seq.dat')
     do i=1,npert
        read(9,'(a10)') datepert(i)
     end do
     close(9)
     write(6,*)'perturbation dates range from ',datepert(1),' to ',datepert(npert)
     
!    Write file with randomized perturbation dates.
     write(6,*)'iseed=',iseed,' nanals=',nanals,' npert=',npert
     open(59,form='formatted',file='dates_ran.dat')
     do i=1,nanals
        write(59,'(a10)') datepert(iwork(i))
     end do
     close(59)
     deallocate(datepert)
  endif

! All tasks wait for mype==0
  call mpi_barrier(mpi_comm_world,iret)
  deallocate(rwork,iwork)


! Set i/o unit numbers
  iunit     = 21
  iunitsf   = 22
  iunitmean = 52
  iunitp    = 53
  iunitpr   = 54

  sigio=.false.
  nemsio=.false.

! Read ensemble mean file for header information
  filenamein = "sfg_"//datestring//"_fhr06_ensmean"
  call nemsio_init(iret)
  call sigio_sropen(iunit,trim(filenamein),iret)
  call sigio_srhead(iunit,sigheado,iret)
  if (iret == 0) then
     sigio=.true.
     write(6,*)'Read sigio ',trim(filenamein),' iret=',iret
     call sigio_sclose(iunit,iret)
     ntrunc = sigheado%jcap
     nlevs  = sigheado%levs
     ntrac  = sigheado%ntrac
     nlevs  = sigheado%levs
  else
     call nemsio_open(gfile,trim(filenamein),'READ',iret)
     if (iret == 0 ) then
        nemsio = .true.
        write(6,*)'Read nemsio ',trim(filenamein),' iret=',iret
        call nemsio_getfilehead(gfile, nrec=nrec, jcap=ntrunc, &
             dimx=lonb, dimy=latb, dimz=nlevs, ntrac=ntrac, iret=iret)
        npts=lonb*latb
        call nemsio_close(gfile,iret)
     else
        write(6,*)'***ERROR*** ',trim(filenamein),' contains unrecognized format.  ABORT'
     endif
  endif
  if (.not.nemsio .and. .not.sigio) goto 100

  nc     = (ntrunc+1)*(ntrunc+2)

  if (mype==0) then
     write(6,*)'computing mean with nemsio=',nemsio,' sigio=',sigio
     write(6,*)' read ',trim(filenamein),' nlevs=',nlevs,&
          ' ntrac=',ntrac,' ntrunc=',ntrunc,' nc=',nc
  endif


! Only processors up to nanals have data to process
  if (mype1 <= nanals) then
     write(charnanal,'(i3.3)') mype1
     if (meanonly) then
        filenamein = "sanl_"//datestring//"_ensmean"
     else
        filenamein = "sanl_"//datestring//"_mem"//charnanal
     endif

!    Do not overwrite input file.  Write output to uniquely named file
     filenameout = "sanlp_"//datestring//"_mem"//charnanal
     filenameoutr = "sanlpr_"//datestring//"_mem"//charnanal
     filenameoutmean = "sanlensmean_"//datestring//"_mem"//charnanal

!    Set up level dependent smoothing parameters
     allocate(smoothparm(nlevs))
     smoothparm = -1

     fname='hybens_smoothinfo'
     inquire(file=trim(fname),exist=lexist)
     if (lexist) then
        open(9,form='formatted',file=fname)
        do k=1,nlevs
           read(9,'(i3)') smoothparm(k)
        enddo
        close(9)
        if (mype == 0) write(6,*)'smoothparm=',smoothparm
     else
        if (mype == 0) write(6,*)'***NOTE*** hybens_smoothinfo not found - no smoothing'
     endif

!    Set initial date and forecast hour
     read(datestring(1:4), '(i4)') idateout(4)
     read(datestring(5:6), '(i2)') idateout(2)
     read(datestring(7:8), '(i2)') idateout(3)
     read(datestring(9:10),'(i2)') idateout(1)
     if (sigio) then
        sigheado%idate = idateout
        sigheado%fhour = 0.

!       Set ensemble info
!         http://www.emc.ncep.noaa.gov/gmb/ens/info/ens_grib.html#gribex
        sigheado%iens(1) = 3 ! pos pert
        sigheado%iens(2) = mype1 ! ensemble member number
        sigheado%icen2 = 2 ! sub-center, must be 2 or ens info not used

!       Copy sigheado to ensemble mean sigheadim (sigheadim altered later)
        sigheadim=sigheado


!       Read each ensemble member analysis.
        call sigio_srohdc(iunit,trim(filenamein),sigheadi,sigdatai,iret)

        call sigio_aldata(sigheado,sigdataim,iret)
        call copy_sigdata(sigheado,sigdatai,sigdataim)


        call mpi_allreduce(sigdatai%z,sigdataim%z,nc*nlevs,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%d,sigdataim%d,nc*nlevs,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%t,sigdataim%t,nc*nlevs,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%q,sigdataim%q,nc*nlevs*ntrac,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatai%ps,sigdataim%ps,nc,mpi_real,mpi_sum,new_comm,iret)

!       Compute ensemble mean full fields
        sigdataim%z  = rnanals*sigdataim%z
        sigdataim%d  = rnanals*sigdataim%d
        sigdataim%t  = rnanals*sigdataim%t
        sigdataim%q  = rnanals*sigdataim%q
        sigdataim%ps = rnanals*sigdataim%ps
        
!       Write out ensemble mean from task 0
        if (mype == 0) then
           sigheadim%iens(1) = 1 ! unperturbed control
           sigheadim%iens(2) = 2 ! low res control
           sigheadim%icen2 = 2 ! sub-center, must be 2 or ens info not used
           call sigio_swohdc(iunitmean,filenameoutmean,sigheadim,sigdataim,iret)
           write(6,*)'write mean data to filenameoutmean= ',trim(filenameoutmean)
        endif

     elseif (nemsio) then
        npts=lonb*latb
        nsize=npts*nrec
        allocate(rwork1d(npts,nrec))
        allocate(swork1d(npts,nrec))
        allocate(twork1d(npts))

        call nemsio_open(gfilei,trim(filenamein),'READ',iret)

        rwork1d=zero
        do n=1,nrec
           call nemsio_readrec(gfilei,n,rwork1d(:,n),iret=iret)
        end do
        swork1d=zero
        call mpi_allreduce(rwork1d,swork1d,nsize,mpi_real,mpi_sum,new_comm,iret)
        swork1d = swork1d * rnanals

        if (mype==0) then
           gfileo=gfilei
           call nemsio_open(gfileo,trim(filenameoutmean),'WRITE',iret=iret )
           do n=1,nrec
              call nemsio_writerec(gfileo,n,swork1d(:,n),iret=iret)
           end do
           call nemsio_readrecv(gfilei,'hgt','sfc',1,twork1d,iret)
           call nemsio_writerecv(gfileo,'hgt','sfc',1,twork1d,iret)
           call nemsio_close(gfileo,iret)
           write(6,*)'write mean data to filenameoutmean= ',trim(filenameoutmean)
        endif

        deallocate(rwork1d)
        deallocate(swork1d)
        deallocate(twork1d)
           
     endif

! Jump here if more mpi processors than files to process
  else
     write(6,*) 'No files to process for mpi task = ',mype
  endif


! Only tasks nanal <= nanals have fields to process
  if ( (mype1 <= nanals) .and. (scalefact > 0.0) ) then

     if (sigio) then

!       Allocate structure for perturbation
        call sigio_aldata(sigheado,sigdatap,iret)

        open(9,form='formatted',file='dates_ran.dat')
        do i=1,mype1
           read(9,'(a10,1x,a10)') datestringpert
        enddo
        close(9)
        filenamepert = trim(datapath)//'sigf48_f24.gfs.'//trim(datestringpert)
        
        call sigio_srohdc(iunitsf,trim(filenamepert),sigheadpin,sigdatapin,iret)
        write(6,*)'member=',trim(filenamein),'   perturbation=',trim(filenamepert)
        
        if (iret /= 0) then
           write(6,*)'***ERROR*** problem opening ',trim(filenamepert),' iret=',iret
           flush(6)
           flush(0)
           call mpi_abort(mpi_comm_world,101,iret)
           stop
        end if
        
        write(6,*) 'compare resolution, jcapout, jcappert = ',sigheado%jcap,sigheadpin%jcap

!       Change resolution of spectral perturbations if necessary
        if (sigheadpin%jcap.ne.sigheado%jcap) then
           call sppad(0,sigheadpin%jcap,sigdatapin%ps,0,sigheado%jcap,sigdatap%ps)
           do k=1,sigheado%levs
              call sppad(0,sigheadpin%jcap,sigdatapin%z(:,k),0,sigheado%jcap,sigdatap%z(:,k))
              call sppad(0,sigheadpin%jcap,sigdatapin%d(:,k),0,sigheado%jcap,sigdatap%d(:,k))
              call sppad(0,sigheadpin%jcap,sigdatapin%t(:,k),0,sigheado%jcap,sigdatap%t(:,k))
              do j=1,ntrac
                 call sppad(0,sigheadpin%jcap,sigdatapin%q(:,k,j),0,sigheado%jcap,sigdatap%q(:,k,j))
              end do
           end do
        else
           sigdatap%z  = sigdatapin%z
           sigdatap%d  = sigdatapin%d
           sigdatap%t  = sigdatapin%t
           sigdatap%q  = sigdatapin%q
           sigdatap%ps = sigdatapin%ps
        end if

!       Rescale the perturbation here
        sigdatap%z  = scalefact*sigdatap%z
        sigdatap%d  = scalefact*sigdatap%d
        sigdatap%t  = scalefact*sigdatap%t
        sigdatap%q  = scalefact*sigdatap%q
        sigdatap%ps = scalefact*sigdatap%ps

        call sigio_axdata(sigdata,iret)

!       Allocate structure for output.  Initialize with input
        call sigio_aldata(sigheado,sigdatao,iret)
        call copy_sigdata(sigheado,sigdatai,sigdatao)

!       Save pertubed fields
        sigdatao%z  = sigdatai%z + sigdatap%z
        sigdatao%d  = sigdatai%d + sigdatap%d
        sigdatao%t  = sigdatai%t + sigdatap%t
        sigdatao%q  = sigdatai%q + sigdatap%q
        sigdatao%ps = sigdatai%ps + sigdatap%ps

!       Write out perturbed analysis
        call sigio_swohdc(iunitpr,filenameout,sigheado,sigdatao,iret)
        call sigio_axdata(sigdatao,iret)
        call sigio_axdata(sigdatapin,iret)


!       Sum perturbation fields over all tasks
        call sigio_aldata(sigheado,sigdatapm,iret)
        call copy_sigdata(sigheado,sigdatai,sigdatapm)

        call mpi_allreduce(sigdatap%z,sigdatapm%z,nc*nlevs,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatap%d,sigdatapm%d,nc*nlevs,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatap%t,sigdatapm%t,nc*nlevs,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatap%q,sigdatapm%q,nc*nlevs*ntrac,mpi_real,mpi_sum,new_comm,iret)
        call mpi_allreduce(sigdatap%ps,sigdatapm%ps,nc,mpi_real,mpi_sum,new_comm,iret)


!       Compute ensemble mean perturbation fields
        sigdatapm%z  = rnanals*sigdatapm%z
        sigdatapm%d  = rnanals*sigdatapm%d
        sigdatapm%t  = rnanals*sigdatapm%t
        sigdatapm%q  = rnanals*sigdatapm%q
        sigdatapm%ps = rnanals*sigdatapm%ps

        sigdatap%z  =  sigdatap%z - sigdatapm%z
        sigdatap%d  =  sigdatap%d - sigdatapm%d
        sigdatap%t  =  sigdatap%t - sigdatapm%t
        sigdatap%q  =  sigdatap%q - sigdatapm%q
        sigdatap%ps =  sigdatap%ps - sigdatapm%ps

!       Compute total perturbations.
        if (mype == 0) write(6,*)'compute total perturbations'
        sigdatap%z  = sigdatai%z - sigdataim%z + sigdatap%z
        sigdatap%d  = sigdatai%d - sigdataim%d + sigdatap%d
        sigdatap%t  = sigdatai%t - sigdataim%t + sigdatap%t
        sigdatap%q  = sigdatai%q - sigdataim%q + sigdatap%q
        sigdatap%ps = sigdatai%ps - sigdataim%ps + sigdatap%ps
        
!       Optionally smooth perturbations
        if (maxval(smoothparm) > 0) then
           if (mype == 0) write(6,*)'call smooth'
           call smooth(sigdatap%z,ntrunc,nlevs,smoothparm,window)
           call smooth(sigdatap%d,ntrunc,nlevs,smoothparm,window)
           call smooth(sigdatap%t,ntrunc,nlevs,smoothparm,window)
           
!       Only smooth q field?  this is what's done in getsigensmeanp_smooth.f90
!       call smooth(sigdatap%q,ntrunc,nlevs,smoothparm,window)
           do k=1,ntrac
              call smooth(sigdatap%q(:,:,k),ntrunc,nlevs,smoothparm,window)
           end do
           call smooth(sigdatap%ps,ntrunc,1,smoothparm(1),window)
        else
           if (mype == 0) write(6,*)'skip call smooth because maxval(smoothparm)=',maxval(smoothparm)
        endif
        deallocate(smoothparm)

!       Allocate structure for output.  Initialize with input
        call sigio_aldata(sigheado,sigdatao,iret)
        call copy_sigdata(sigheado,sigdatai,sigdatao)

!       Add mean back in
        if (mype == 0) write(6,*)'add mean back in'
        sigdatao%z  = sigdataim%z + sigdatap%z
        sigdatao%d  = sigdataim%d + sigdatap%d
        sigdatao%t  = sigdataim%t + sigdatap%t
        sigdatao%q  = sigdataim%q + sigdatap%q
        sigdatao%ps = sigdataim%ps + sigdatap%ps

!       Write out perturbed and recentered analysis
        call sigio_swohdc(iunitpr,filenameoutr,sigheado,sigdatao,iret)
        call sigio_axdata(sigdatao,iret)

!       Deallocate and nullify sigio data structures
        call sigio_axdata(sigdatai,iret)
        call sigio_axdata(sigdataim,iret)
        call sigio_axdata(sigdatap,iret)
        call sigio_axdata(sigdatapm,iret)

     elseif (nemsio) then
        write(6,*)'NEMSIO additive perturbations not coded'
     endif
  endif


! Wait for all tasks to finish
100 continue
  call mpi_barrier(mpi_comm_world,iret)
  if (.not.nemsio .and. .not.sigio) then
     if (mype==0) write(6,*)'***ERROR***  invalid surface file format'
     call MPI_Abort(MPI_COMM_WORLD,98,iret)
     stop
  endif

  if (mype == 0) then
     write(6,*) 'all done!'
     call w3tage('ADDERRSPEC_NMCMETH')
  endif
  call mpi_finalize(iret)
  if (mype == 0 .and. iret .ne. 0) then
     write(6,*)'***ERROR*** MPI_Finalize error status = ',iret
  end if

! Create log file that can be checked for normal completion
  if (mype == 0) then
     open(91,form='formatted',file='adderrspec.log')
     write(91,*) datestring
     close(91)
  endif
  
end program adderrspec_nmcmeth

subroutine smooth(specdat,ntrunc,nlevs,smoothparm,window)
  implicit none
  integer :: m,nm,n,k
  integer, intent(in) :: ntrunc,nlevs,window
  real, intent(inout) :: specdat((ntrunc+1)*(ntrunc+2),nlevs)
  integer, intent(in) ::  smoothparm(nlevs) ! smoothing parameter.
  real pi, smoothfact
  pi = 4.*atan(1.0)
  do k=1,nlevs
     if (smoothparm(k) > 0.) then
        nm = 1
        do m=0,ntrunc
           do n=m,ntrunc
              if (window == 1) then
                 ! Hann window (cosine bell).
                 if (n <= smoothparm(k)) then
                    smoothfact = 0.5*(1.0 + cos(pi*real(n)/smoothparm(k)))
                 else
                    smoothfact = 0.
                 endif
              else if (window == 2) then
                 ! gaussian window.
                 smoothfact = exp(-(real(n)/real(smoothparm(k)))**2)
              else
                 ! rectangular window (simple truncation)
                 if (n <= smoothparm(k)) then
                    smoothfact = 1.
                 else
                    smoothfact = 0.
                 endif
              endif
              specdat(nm,k) = smoothfact*specdat(nm,k)
              specdat(nm+1,k) = smoothfact*specdat(nm+1,k)
              nm = nm + 2
           enddo
        enddo
     endif
  enddo
end subroutine smooth

subroutine init_sigdata(head,data)
  use sigio_module
  type(sigio_head),intent(in):: head
  type(sigio_data),intent(inout):: data
  integer nc,dim1,dim2,dim3q,dim1x,dim2x,dim3x
  integer i,j,k

  nc=(head%jcap+1)*(head%jcap+2)
  dim1=nc
  dim2=head%levs
  dim3q=head%ntrac
  dim1x=head%lonb
  dim2x=head%latb
  dim3x=head%nxgr

  do i=1,dim1
     data%hs(i)=0.0
     data%ps(i)=0.0
  end do
  do j=1,dim2
     do i=1,dim1
        data%t(i,j)=0.0
        data%d(i,j)=0.0
        data%z(i,j)=0.0
     end do
  end do
  do k=1,dim3q
     do j=1,dim2
        do i=1,dim1
           data%q(i,j,k)=0.0
        end do
     end do
  end do
  do k=1,dim3x
     do j=1,dim2x
        do i=1,dim1x
           data%xgr(i,j,k)=0.0
        end do
     end do
  end do
  do i=1,head%nxss
     data%xss(i)=0.0
  end do

  return
end subroutine init_sigdata

subroutine copy_sigdata(head,data1,data2)
  use sigio_module
  type(sigio_head),intent(in):: head
  type(sigio_data),intent(in):: data1
  type(sigio_data),intent(inout):: data2
  integer nc,dim1,dim2,dim3q,dim1x,dim2x,dim3x
  integer i,j,k

  nc=(head%jcap+1)*(head%jcap+2)
  dim1=nc
  dim2=head%levs
  dim3q=head%ntrac
  dim1x=head%lonb
  dim2x=head%latb
  dim3x=head%nxgr

  do i=1,dim1
     data2%hs(i)=data1%hs(i)
     data2%ps(i)=data1%ps(i)
  end do
  do j=1,dim2
     do i=1,dim1
        data2%t(i,j)=data1%t(i,j)
        data2%d(i,j)=data1%d(i,j)
        data2%z(i,j)=data1%z(i,j)
     end do
  end do
  do k=1,dim3q
     do j=1,dim2
        do i=1,dim1
           data2%q(i,j,k)=data1%q(i,j,k)
        end do
     end do
  end do
  do k=1,dim3x
     do j=1,dim2x
        do i=1,dim1x
           data2%xgr(i,j,k)=data1%xgr(i,j,k)
        end do
     end do
  end do
  do i=1,head%nxss
     data2%xss(i)=data1%xss(i)
  end do

  return
end subroutine copy_sigdata
