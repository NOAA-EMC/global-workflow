program getnstensmeanp
!$$$  main program documentation block
!
! program:  getnstensmean (modified from getsfcensmean)              compute ensemble mean
!
! prgmmr: li         org: emc/imsg               date: 2014-02-28
!
! abstract:  compute ensemble mean surface file
!
! program history log:
!   2014-02-28  Initial version.
!   2016-11-18  tic615: change nst mask name from slmsk to land
!
! usage:

!   nstio_data        nst file data fields
!     slmsk             Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       surface mask: 0 = water; 1 = land; 2 = ice
!     xt                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       heat content in DTL
!                       (M*K)
!     xs                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       salinity content in DTL
!                       (M*ppt)
!     xu                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       u-current content in DTL
!                       (M*M/S)
!     xv                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       v-current content in DTL
!                       (M*M/S)
!     xz                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       DTL thickness                                        (M)
!     zm                Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       MXL thickness                                        (M)
!     xtts              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       d(xt)/d(Ts)
!                       (1/M)
!     xzts              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       d(xz)/d(Ts)
!                       (M/K)
!     dt_cool           Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sea surface cooling amount by sub-layer cooling effect
!     z_c               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sea sub-layer depth in m
!     c_0               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in dimensionless
!     c_d               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in
!                       (1/M)
!     w_0               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr) in dimensionless
!     w_d               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       coefficient to calculate d(Tz)/d(tr)
!                       (1/M)
!     d_conv            Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       FCL thickness
!                       (M)
!     ifd               Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       index of time integral started mode: 0 = not yet; 1 =
!                       started already
!     Tref              Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       reference temperature
!                       (K)
!     Qrain             Real(nstio_realkind)(:,:) pointer to lonb*latb
!                       sensible heat flux due to rainfall
!                       (W*M^-2)
!   input files:
!
!   output files:
!
! attributes:
!   language: f95
!
!$$$

! create ensemble mean NCEP GFS NSST file.
  use nstio_module
  use nemsio_module, only:  nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only:  nemsio_gfile,nemsio_getfilehead,nemsio_readrec,&
       nemsio_writerec,nemsio_readrecv,nemsio_writerecv
  implicit none

  real(4),parameter:: zero=0.0_4

  logical:: nemsio, sfcio

  character*500 filenamein,filenameout,datapath,fileprefix
  character*3 charnanal
  integer lunin,lunout,iret,nanals,k
  integer mype,mype1,npe,orig_group, new_group, new_comm
  integer nrec, lonb, latb, n, npts
  integer,dimension(7):: idate
  integer,dimension(:), allocatable:: new_group_members
  real(8) rnanals
  real,allocatable,dimension(:)   :: rwork1d,swork1d

  type(nstio_head):: nstheadi, nstheado
  type(nstio_data):: nstdatai, nstdatao

  type(nemsio_gfile):: gfile, gfileo

! mpi definitions.
  include 'mpif.h'

! Initialize mpi
!  mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)
  mype1=mype+1

  if (mype==0) call w3tagb('GETNSTENSMEAN',2011,0319,0055,'NP25')

! Get user input from command line
  call getarg(1,datapath)
  call getarg(2,filenameout)
  call getarg(3,fileprefix)
  call getarg(4,charnanal)
  read(charnanal,'(i2)') nanals
  rnanals=nanals
  rnanals=1.0_8/rnanals
  filenameout = trim(adjustl(datapath))//filenameout

  if (mype==0) then
     write(6,*)' '
     write(6,*)'Command line input'
     write(6,*)' datapath      = ',trim(datapath)
     write(6,*)' filenameout   = ',trim(filenameout)
     write(6,*)' fileprefix    = ',trim(fileprefix)
     write(6,*)' nanals,rnanals= ',nanals,rnanals
  endif

  if (npe < nanals) then
     write(6,*)'***ERROR***  npe too small.  npe=',npe,' < nanals=',nanals
     call MPI_Abort(MPI_COMM_WORLD,99,iret)
     stop
  end if

  lunin=21
  lunout=61

! Create sub-communicator to handle number of cases (nanals)
  call mpi_comm_group(mpi_comm_world,orig_group,iret)

  allocate(new_group_members(nanals))
  do k=1,nanals
     new_group_members(k)=k-1
  end do
  call mpi_group_incl(orig_group,nanals,new_group_members,new_group,iret)
  call mpi_comm_create(mpi_comm_world,new_group,new_comm,iret)
  if (iret.ne.0) then
     write(6,*)'***ERROR*** after mpi_comm_create with iret=',iret
     call mpi_abort(mpi_comm_world,101,iret)
  endif

  sfcio=.false.
  nemsio=.false.

! Process input files (one file per task)
  if (mype1 <= nanals) then

     call nemsio_init(iret)

     write(charnanal,'(i3.3)') mype1
     filenamein = trim(adjustl(datapath))// &
          trim(adjustl(fileprefix))//'_mem'//charnanal
     call nstio_sropen(lunin,filenamein,iret)
     if ( iret /= 0 )  write(6,*)'***ERROR in open ',trim(filenamein)
     call nstio_srhead(lunin,nstheadi,iret)
     write(6,*)'Read header of ',trim(filenamein),' iret=',iret
     if ( nstheadi%clabnst(1:8) == 'GFS NST ' ) then
        sfcio = .true.
        call nstio_srohdc(lunin,filenamein,nstheadi,nstdatai,iret)
     else
        call nemsio_open(gfile,trim(filenamein),'READ',iret)
        if (iret == 0 ) then
           nemsio = .true.
        else
           write(6,*)'***ERROR*** ',trim(filenamein),' contains unrecognized format.  ABORT'
        endif
     endif
     if (.not.nemsio .and. .not. sfcio) goto 100
     if (mype==0) write(6,*)'computing mean with nemsio=',nemsio,' sfcio=',sfcio

!
     if (sfcio) then
     call nstio_aldata(nstheadi,nstdatao,iret)
     nstheado = nstheadi

!    These fields are fixed.  Do not compute mean
     nstdatao%slmsk = nstdatai%slmsk
     nstdatao%ifd   = nstdatai%ifd

!    For sfcio, Use mpi_reduce to sum fields.  Compute mean
     call mpi_allreduce(nstdatai%xt,     nstdatao%xt,     size(nstdatai%xt),     mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%xs,     nstdatao%xs,     size(nstdatai%xs),     mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%xu,     nstdatao%xu,     size(nstdatai%xu),     mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%xv,     nstdatao%xv,     size(nstdatai%xv),     mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%xz,     nstdatao%xz,     size(nstdatai%xz),     mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%zm,     nstdatao%zm,     size(nstdatai%zm),     mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%xtts,   nstdatao%xtts,   size(nstdatai%xtts),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%xzts,   nstdatao%xzts,   size(nstdatai%xzts),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%dt_cool,nstdatao%dt_cool,size(nstdatai%dt_cool),mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%z_c,    nstdatao%z_c,    size(nstdatai%z_c),    mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%c_0,    nstdatao%c_0,    size(nstdatai%c_0),    mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%c_d,    nstdatao%c_d,    size(nstdatai%c_d),    mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%w_0,    nstdatao%w_0,    size(nstdatai%w_0),    mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%w_d,    nstdatao%w_d,    size(nstdatai%w_d),    mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%d_conv, nstdatao%d_conv, size(nstdatai%d_conv), mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%Tref,   nstdatao%Tref,   size(nstdatai%Tref),   mpi_real,mpi_sum,new_comm,iret)
     call mpi_allreduce(nstdatai%Qrain,  nstdatao%Qrain,  size(nstdatai%Qrain),  mpi_real,mpi_sum,new_comm,iret)

     nstdatao%xt      = nstdatao%xt      * rnanals
     nstdatao%xs      = nstdatao%xs	 * rnanals
     nstdatao%xu      = nstdatao%xu      * rnanals
     nstdatao%xv      = nstdatao%xv	 * rnanals
     nstdatao%xz      = nstdatao%xz	 * rnanals
     nstdatao%zm      = nstdatao%zm      * rnanals
     nstdatao%xtts    = nstdatao%xtts	 * rnanals
     nstdatao%xzts    = nstdatao%xzts	 * rnanals
     nstdatao%dt_cool = nstdatao%dt_cool * rnanals
     nstdatao%z_c     = nstdatao%z_c     * rnanals
     nstdatao%c_0     = nstdatao%c_0     * rnanals
     nstdatao%c_d     = nstdatao%c_d     * rnanals
     nstdatao%w_0     = nstdatao%w_0     * rnanals
     nstdatao%w_d     = nstdatao%w_d     * rnanals
     nstdatao%d_conv  = nstdatao%d_conv  * rnanals
     nstdatao%Tref    = nstdatao%Tref    * rnanals
     nstdatao%Qrain   = nstdatao%Qrain   * rnanals

     call nstio_axdata(nstdatai,iret)

     if (mype==0) then
        call nstio_swohdc(lunout,filenameout,nstheado,nstdatao,iret)
        write(6,*)'Write ensemble mean ',trim(filenameout),' iret=',iret
     endif
! For nemsio, Use mpi_reduce to sum fields.  Compute mean
  elseif (nemsio) then
     call nemsio_getfilehead(gfile, nrec=nrec, idate=idate, dimx=lonb,dimy=latb, iret=iret)

     if (mype==0) then
        gfileo=gfile         ! copy gfile header to gfileo header
        call nemsio_open(gfileo,trim(filenameout),'WRITE',iret )
     end if

     npts=lonb*latb
     if (.not.allocated(rwork1d)) allocate(rwork1d(npts))
     if (.not.allocated(swork1d)) allocate(swork1d(npts))

!
!   get the average for all data records and then write to gfileo
!
     do n=1,nrec
        rwork1d=zero
        call nemsio_readrec (gfile, n,rwork1d,iret)
        swork1d=zero
        call mpi_allreduce(rwork1d,swork1d,npts,mpi_real,mpi_sum,new_comm,iret)
        swork1d = swork1d * rnanals

        if (mype==0) then
           call nemsio_writerec(gfileo,n,swork1d,iret)
        end if
     end do

!       Following fields are not averaged
!         slmsk = surface mask sfc
!         ifd   = ifd sfc

        if (mype==0) then
           rwork1d=zero
           call nemsio_readrecv(gfile,'land','sfc',1,rwork1d,iret)
           call nemsio_writerecv(gfileo,'land','sfc',1,rwork1d,iret)

           rwork1d=zero
           call nemsio_readrecv(gfile,'ifd','sfc',1,rwork1d,iret)
           call nemsio_writerecv(gfileo,'ifd','sfc',1,rwork1d,iret)
        end if

        deallocate(rwork1d)
        deallocate(swork1d)

        call nemsio_close(gfile, iret)
        if (mype==0) then
           call nemsio_close(gfileo,iret)
           write(6,*)'Write ensmemble mean ',trim(filenameout),' iret=',iret
        endif
     endif

! Jump here if more mpi processors than files to process
  else
     write(6,*) 'No files to process for mpi task = ',mype
  endif

100 continue
  call mpi_barrier(mpi_comm_world,iret)

  if (mype1 <= nanals .and. .not.nemsio .and. .not. sfcio) then
     write(6,*)'***ERROR***  invalid surface file format'
     call MPI_Abort(MPI_COMM_WORLD,98,iret)
     stop
  endif

  if (mype==0) then
     write(6,*) 'all done!'
     call w3tage('GETNSTENSMEAN')
  endif

  deallocate(new_group_members)

  call mpi_finalize(iret)

END program getnstensmeanp
