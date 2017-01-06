program getsfcnstensupdp
!$$$  main program documentation block
!
! program:  getsfcnstenupdp              update sfc and nst files for ensemble
!
! prgmmr: Xu Li         org: EMC               date: 2014-05-01
!
! abstract:  update sfc & nst file for ensemble 
!
! program history log:
!   2014-05-01  Initial version.
!   2016-02-15  Add to read nemsio
!   2016-08-18  Fix two bugs  (tsea, dtf_ens)
!   2016-11-18  tic615: change nst mask name from slmsk to land
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

  use mpi
  use kinds, only: r_kind,i_kind,r_single
  use constants, only: two,half,zero,z_w_max,tfrozen,init_constants_derived,pi
  use sfcio_module, only: sfcio_srohdc,sfcio_head,sfcio_data,sfcio_swohdc
  use nstio_module, only: nstio_srohdc,nstio_head,nstio_data,nstio_swohdc
  use nemsio_module, only: nemsio_init,nemsio_open,nemsio_close
  use nemsio_module, only: nemsio_gfile,nemsio_getfilehead,nemsio_readrec,&
                           nemsio_writerec,nemsio_readrecv,nemsio_writerecv

  implicit none
 
  logical:: nemsio, sfcio

  real   (r_kind), parameter :: houra=zero
  integer(i_kind), parameter :: nprep=15
  integer(i_kind) :: istop = 106
  integer(i_kind), parameter :: lun_dtfanl=11,lun_nstges=21,lun_sfcges=22, &
                                lun_sfcgcy=23,lun_nstanl=61,lun_sfcanl=62
  integer(i_kind), parameter :: idrt=4

  character(len=80) :: fname_dtfanl,fname_nstges,fname_sfcges,fname_sfcgcy,fname_nstanl,fname_sfcanl
  character(len=3)  :: charnanal
  character(len=8)  :: charbuf
  character(len=60) :: my_name = 'getsfcnstensupdp'
  character(len=1)  :: null = ' '

  integer(i_kind) :: mype,mype1,npe,iret
  integer nrec_sfc, lonb, latb, n, npts
  integer(i_kind) :: nfhour, nfminute, nfsecondn, nfsecondd
  integer,dimension(7):: idate
  integer(i_kind) :: n_new_water,n_new_seaice
  integer(i_kind) :: i,j,jmax
  integer(i_kind) :: nanals,nst_gsi,zsea1,zsea2
  integer(i_kind) :: nlon_anl,nlat_anl    ! the number of lon/lat of GSI analysis grids, including two extra polar lats
  integer(i_kind) :: nlon_ens,nlat_ens    ! the number of lon/lat of ensemble grids, including two extra polar lats
  integer(i_kind), allocatable, dimension(:,:) :: isli_anl,isli_epd,isli_gsi
  real(r_kind),    allocatable, dimension(:)   :: wlatx,slatx,rlats_anl,rlons_anl,rlats_ens,rlons_ens
  real(r_kind),    allocatable, dimension(:,:) :: dtf_anl,dtf_epd,dtf_gsi,dtf_ens
  real(r_single),  allocatable, dimension(:,:) :: work
  real(r_single),  allocatable, dimension(:,:) :: dtzm
  real(r_single),  allocatable, dimension(:,:) :: slmsk_ges,slmsk_ens
  real(r_single),  allocatable, dimension(:)   :: rwork1d
  real(r_single),  allocatable, dimension(:,:) :: tsea,xt,xs,xu,xv,xz,zm,xtts,xzts,dt_cool,z_c, &
                                                  c_0,c_d,w_0,w_d,d_conv,ifd,tref,qrain
  real(r_single) :: r_zsea1,r_zsea2
  real(r_kind)   :: dlon
  real(r_kind)   :: sumn,sums

  type(nstio_head):: head_nst
  type(nstio_data):: data_nst
  type(sfcio_head):: head_sfcanl,head_sfcges,head_sfcgcy
  type(sfcio_data):: data_sfcanl,data_sfcges,data_sfcgcy

  type(nemsio_gfile) :: gfile_sfcges,gfile_sfcgcy,gfile_sfcanl,gfile_nstges,gfile_nstanl
! Initialize mpi
!  mype is process number, npe is total number of processes.
  call mpi_init(iret)
  call MPI_Comm_rank(MPI_COMM_WORLD,mype,iret)
  call MPI_Comm_size(MPI_COMM_WORLD,npe,iret)

  call init_constants_derived

  if ( mype == 0 ) call w3tagb('GETSFCNSTENSUPD',2014,0921,0055,'NP25')

  call getarg(1,charbuf)
  read(charbuf,'(i3)') nanals

  call getarg(2,charbuf)
  read(charbuf,'(i1)') nst_gsi

  call getarg(3,charbuf)
  read(charbuf,'(i8)') zsea1
  r_zsea1 = 0.001_r_single * real(zsea1,r_single)

  call getarg(4,charbuf)
  read(charbuf,'(i8)') zsea2
  r_zsea2 = 0.001_r_single * real(zsea2,r_single)

  if (mype==0) then
    write(6,'(a)')' '
    write(6,'(a)')'Command line input'
    write(6,'(a,i5)')' nanals  = ',nanals
    write(6,'(a,i5)')' nst_gsi = ',nst_gsi
    write(6,'(a,i5)')' zsea1   = ',zsea1
    write(6,'(a,i5)')' zsea2   = ',zsea2
  endif

!
!   read Tf analysis increment at GSI analysis grids and its grid info and surface mask
!
  fname_dtfanl = 'dtfanl'
  open(lun_dtfanl,file=trim(fname_dtfanl),form='unformatted')
  read(lun_dtfanl) nlon_anl,nlat_anl

  allocate(dtf_anl(nlat_anl,nlon_anl),isli_anl(nlat_anl,nlon_anl))
  allocate(dtf_epd(nlat_anl,nlon_anl),isli_epd(nlat_anl,nlon_anl))

  read(lun_dtfanl) dtf_anl
  read(lun_dtfanl) isli_anl
!
! determine sfcio or nemsio with sfcges_mem001 only
!
  sfcio=.false.
  nemsio=.false.

  call sfcio_srohdc(lun_sfcges,'sfcf06_mem001',head_sfcges,data_sfcges,iret)

  if ( iret == 0 ) then
    sfcio = .true.
    if (mype==0) write(6,'(3a)')'Read ','sfcf06_mem001',' in sfcio format '
    lonb=head_sfcges%lonb
    latb=head_sfcges%latb
  else
    call nemsio_init(iret=iret)
    if (iret /= 0) call error_msg(0,trim(my_name),null,null,'init',istop,iret)
!   open gfile_sfcges (sfcf06_mem001)
    call nemsio_open(gfile_sfcges,'sfcf06_mem001','read',iret=iret)
    if (iret /= 0) call error_msg(0,trim(my_name),'sfcf06_mem001',null,'open',istop,iret)
    if (iret == 0 ) then
      nemsio = .true.
!     read a few surface gcycle file header records: dimensions and time parameters
      call nemsio_getfilehead(gfile_sfcges, nrec=nrec_sfc, idate=idate, &
              dimx=lonb, dimy=latb, nfhour=nfhour, nfminute=nfminute, &
              nfsecondn=nfsecondn, nfsecondd=nfsecondd, iret=iret)
    else
      write(6,*)'***ERROR*** ','sfcf06mem001',' contains unrecognized format.  ABORT'
      call MPI_Abort(MPI_COMM_WORLD,77,iret)
      stop
    endif
  endif

  nlon_ens = lonb
  nlat_ens = latb + 2
  
  allocate(dtf_gsi(nlat_ens,nlon_ens),isli_gsi(nlat_ens,nlon_ens),work(nlat_ens,nlon_ens))
  allocate(rwork1d(lonb*latb))
  allocate(slmsk_ges(lonb,latb),slmsk_ens(lonb,latb))
  allocate(dtf_ens(lonb,latb),dtzm(lonb,latb))
!
! get slmsk for ges and anl of ensembles
!
  if ( sfcio ) then
    slmsk_ges = data_sfcges%slmsk
!   read sfcgcy_mem001 and assign slmsk_ens
    call sfcio_srohdc(lun_sfcgcy,'sfcgcy_mem001',head_sfcgcy,data_sfcgcy,iret)
    slmsk_ens = data_sfcgcy%slmsk
  elseif ( nemsio ) then
!   read land in fname_sfcges to get slmsk_ges
    call nemsio_readrecv(gfile_sfcges, 'land', 'sfc', 1, rwork1d, iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),'sfcf06_mem001','land','read',istop,iret)
    slmsk_ges=reshape(rwork1d,(/size(slmsk_ges,1),size(slmsk_ges,2)/))

!   close gfile_sfcges (not needed any more)
    call nemsio_close(gfile_sfcges, iret=iret)
    if (iret /= 0) call error_msg(0,trim(my_name),'sfcf06_mem001',null,'close',istop,iret)

!   open gfile_sfcgcy (sfcgcy_mem001)
    call nemsio_open(gfile_sfcgcy,'sfcgcy_mem001','read',iret=iret)
    if (iret /= 0) call error_msg(0,trim(my_name),'sfcgcy_mem001',null,'open',istop,iret)
!   read land in fname_sfcgcy to get slmsk_ens
    call nemsio_readrecv(gfile_sfcgcy, 'land', 'sfc', 1, rwork1d, iret=iret)
    if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_sfcgcy),'land','read',istop,iret)
    slmsk_ens=reshape(rwork1d,(/size(slmsk_ens,1),size(slmsk_ens,2)/))

  endif

! get Tf analysis increment at ens resolution from anl resolution in GSI static analysis
!
  if ( (nlat_ens /= nlat_anl) .or. (nlon_ens /= nlon_anl) ) then
    if ( mype == 0 )  write(6,'(a,2(2a,2i5))')'getsfcnstensupdp: grid dimensions differ: ',&
          'nlon_anl,nlat_anl = ',nlon_anl,nlat_anl,' nlon_ens,nlat_ens = ',nlon_ens,nlat_ens
!
!   get lats and lons for GSI analysis grids
!
    jmax=nlat_anl-2
    allocate(slatx(jmax),wlatx(jmax))
    allocate(rlats_anl(nlat_anl),rlons_anl(nlon_anl))
    call splat(idrt,jmax,slatx,wlatx)
    dlon=two*pi / real(nlon_anl,r_kind)
    do i=1,nlon_anl
       rlons_anl(i)=real(i-1,r_kind)*dlon
        enddo
    do i=1,(nlat_anl-1)/2
       rlats_anl(i+1)=-asin(slatx(i))
       rlats_anl(nlat_anl-i)=asin(slatx(i))
    enddo
    rlats_anl(1)=-half*pi
    rlats_anl(nlat_anl)=half*pi
    deallocate(slatx,wlatx)

!   get lats and lons for ensemble grids
!
    jmax=nlat_ens-2
    allocate(slatx(jmax),wlatx(jmax))
    allocate(rlats_ens(nlat_ens),rlons_ens(nlon_ens))
    call splat(idrt,jmax,slatx,wlatx)
    dlon=two*pi / real(nlon_ens,r_kind)
    do i=1,nlon_ens
       rlons_ens(i)=real(i-1,r_kind)*dlon
    enddo
    do i=1,(nlat_ens-1)/2
       rlats_ens(i+1)=-asin(slatx(i))
       rlats_ens(nlat_ens-i)=asin(slatx(i))
    enddo
    rlats_ens(1)=-half*pi
    rlats_ens(nlat_ens)=half*pi
    deallocate(slatx,wlatx)

!   Get isli_gsi from slmsk_ens
!
    sumn = zero
    sums = zero
    do i=1, lonb
       sumn = slmsk_ens(i,1)    + sumn
       sums = slmsk_ens(i,latb) + sums
    end do
    sumn = sumn/float(lonb)
    sums = sums/float(lonb)

!   Transfer from local work array to surface guess array
    do j = 1, lonb
      work(1,j)=sums
      do i=2, latb+1
        work(i,j) = slmsk_ens(j,latb+2-i)
      end do
      work(latb+2,j)=sumn
    end do

    do j=1, nlon_ens
      do i=1, nlat_ens
        isli_gsi(i,j) = nint(work(i,j))
      enddo
    enddo
!
!   Get the expanded values for a surface type (0 = water now) and the new mask
!
    call int2_msk_glb_prep(dtf_anl,isli_anl,dtf_epd,isli_epd,nlat_anl,nlon_anl,0,nprep)
!
!   Interpolate dtf_epd(nlat_anl,nlon_anl) to dtf_gsi(nlat_ens,nlon_ens) with surface mask accounted
!
    call int22_msk_glb(dtf_epd,isli_epd,rlats_anl,rlons_anl,nlat_anl,nlon_anl, &
                       dtf_gsi,isli_gsi,rlats_ens,rlons_ens,nlat_ens,nlon_ens,0)
!
!   transform the dtf_gsi(nlat_ens,nlon_ens) to dtf_ens(lonb,latb) for sfc file format
!
    do j=1,latb
      do i=1,lonb
        dtf_ens(i,j) = dtf_gsi(latb+2-j,i)
      enddo
    enddo
  else
!
!   transform the dtf_gsi(nlat_ens,nlon_ens) to dtf_ens(lonb,latb) for sfc file format
!
    if ( mype == 0 ) write(6,'(a,2(a,2i5))')'getsfcnstensupdp: grid dimensions same: ',&
                     'nlon_anl,nlat_anl-2 = ',nlon_anl,nlat_anl-2,' lonb,latb = ',lonb,latb
    do j=1,latb
      do i=1,lonb
        dtf_ens(i,j)=dtf_anl(latb+2-j,i)
      enddo
    enddo
  endif ! if ( (nlat_ens /= nlat_anl) .or. (nlon_ens /= nlon_anl) ) then
!
  if ( npe < nanals ) then
    write(6,'(2(a,i5))')'***ERROR***  npe too small. npe = ',npe,' < nanals = ',nanals
    call MPI_Abort(MPI_COMM_WORLD,99,iret)
    stop
  endif

  mype1 = mype + 1
  if ( mype1 > nanals ) then
    write (6,'(a,i5)') 'no files to process for mpi task = ',mype
  else

!   get the filenames for each ensemble member
    write(charnanal,'(i3.3)') mype1
    fname_nstges = 'nstf06_mem' // charnanal
    fname_nstanl = 'nstanl_mem' // charnanal
    fname_sfcgcy = 'sfcgcy_mem' // charnanal
    fname_sfcanl = 'sfcanl_mem' // charnanal
!
!   update tsea & tref and then write out sfcanl & nstanl
!
    if (sfcio) then
      call sfcio_srohdc(lun_sfcgcy,trim(fname_sfcgcy),head_sfcgcy,data_sfcgcy,iret)
      if (mype==0) write(6,'(3a)')'Read ',trim(fname_sfcgcy),' in sfcio format '
      call nstio_srohdc(lun_nstges,trim(fname_nstges),head_nst,data_nst,iret)
      if (mype==0) write(6,'(3a)')'Read ',trim(fname_nstges),' in sfcio format '
!
!     Assign sfcanl as sfcgcy
!
      head_sfcanl = head_sfcgcy
      data_sfcanl = data_sfcgcy
!
!     For the new open water (sea ice just melted) grids, (1) set dtf_ens = zero (2) reset the NSSTM variables
!
!     set tref = tfrozen = 271.2_r_kind
!     note: data_sfcges%slmsk is the mask of the guess
!           data_sfcanl%slmsk is the mask of the analysis
!
      where ( (slmsk_ens(:,:) == zero) .and. (slmsk_ges(:,:) == two) )

        dtf_ens(:,:) = zero

        data_nst%xt(:,:)      = zero
        data_nst%xs(:,:)      = zero
        data_nst%xu(:,:)      = zero
        data_nst%xv(:,:)      = zero
        data_nst%xz(:,:)      = z_w_max
        data_nst%zm(:,:)      = zero
        data_nst%xtts(:,:)    = zero
        data_nst%xzts(:,:)    = zero
        data_nst%dt_cool(:,:) = zero
        data_nst%z_c(:,:)     = zero
        data_nst%c_0(:,:)     = zero
        data_nst%c_d(:,:)     = zero
        data_nst%w_0(:,:)     = zero
        data_nst%w_d(:,:)     = zero
        data_nst%d_conv(:,:)  = zero
        data_nst%ifd(:,:)     = zero
        data_nst%tref(:,:)    = tfrozen
        data_nst%qrain(:,:)   = zero
  
      end where
!
!     update analysis variable: Tref (foundation temperature) for nstanl file
!
      where ( slmsk_ens(:,:) == zero )
        data_nst%tref(:,:) = max(data_nst%tref(:,:) + dtf_ens(:,:),tfrozen)
      elsewhere
        data_nst%tref(:,:) = data_sfcanl%tsea(:,:)
      end where

!     Update guess date/time to analysis date/time for nst file
      head_nst%fhour    = head_sfcanl%fhour           ! forecast hour
      head_nst%idate(1) = head_sfcanl%idate(1)        ! hour
      head_nst%idate(2) = head_sfcanl%idate(2)        ! month
      head_nst%idate(3) = head_sfcanl%idate(3)        ! day
      head_nst%idate(4) = head_sfcanl%idate(4)        ! year

!     Write updated information to nst analysis file
      call nstio_swohdc(lun_nstanl,trim(fname_nstanl),head_nst,data_nst,iret)

      write(6,101) trim(fname_nstanl),lonb,latb,head_nst%fhour,(head_nst%idate(i),i=1,4),iret
101   format(' sfcio_getsfcnstupdp: nst analysis written for ',&
             a,1x,2i6,1x,f4.1,4(i4,1x),' with iret = ',i5)

!
!     update SST: tsea for sfcanl file
!
      if ( nst_gsi == 3 ) then

        call dtzm_2d(data_nst%xt,data_nst%xz,data_nst%dt_cool,data_nst%z_c, &
                     data_sfcanl%slmsk,r_zsea1,r_zsea2,lonb,latb,dtzm)
        where ( slmsk_ens(:,:) == zero )
           data_sfcanl%tsea(:,:) = max(data_nst%tref(:,:) + dtzm(:,:),tfrozen)
        end where
 
!       Write updated information to surface analysis file
        call sfcio_swohdc(lun_sfcanl,trim(fname_sfcanl),head_sfcanl,data_sfcanl,iret)

        write(6,102) trim(fname_sfcanl),lonb,latb,head_sfcanl%fhour,(head_sfcanl%idate(i),i=1,4),iret
102     format(' sfcio_getsfcnstupdp: sfc analysis written for ',&
                  a,1x,2i6,1x,f4.1,4(i4,1x),' with iret = ',i5)

      endif ! if ( nst_gsi == 3 ) then

!
!     write out the info on the new open water and new sea ice grids
!
      if ( mype == 0 ) then
         n_new_water = 0
         n_new_seaice = 0
         do j=1,latb
            do i=1,lonb
               if ( slmsk_ens(i,j) == 0.0 .and. slmsk_ges(i,j) == 2.0 ) &
                  n_new_water = n_new_water + 1
               if ( slmsk_ens(i,j) == 2.0 .and. slmsk_ges(i,j) == 0.0 ) &
                  n_new_seaice = n_new_seaice + 1
            enddo
         enddo
         write(6,'(a,I3,2(1x,I8))') 'sfcio_getsfcnstens,nst_gsi,n_new_water,n_new_seaice:',nst_gsi,n_new_water,n_new_seaice
      endif

    elseif (nemsio) then

      if (mype==0) write(6,*)'computing mean with nemsio=',nemsio
!     open gfile_sfcgcy (sfcgcy_mem???)
      call nemsio_open(gfile_sfcgcy,trim(fname_sfcgcy),'read',iret=iret)
      if (iret /= 0) call error_msg(0,trim(my_name),'fname_sfcgcy',null,'open',istop,iret)

!     open gfile_nstges (nstf06_mem???)
      call nemsio_open(gfile_nstges,trim(fname_nstges),'read',iret=iret)
      if (iret /= 0) call error_msg(0,trim(my_name),'fname_nstges',null,'open',istop,iret)
!      copy sfcgcy header info tosfcanl header, need to do this before nemsio_close(gfile)
       gfile_sfcanl=gfile_sfcgcy
!      open nemsio sfcanl
       call nemsio_open(gfile_sfcanl,trim(fname_sfcanl),'write',iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_sfcanl),null,'open',istop,iret)

!      copy nstges header info to nstanl header
       gfile_nstanl=gfile_nstges
!      open nemsio nstanl (with analysis time) 
       call nemsio_open(gfile_nstanl,trim(fname_nstanl),'write',iret=iret,idate=idate, nfhour=nfhour,&
                        nfminute=nfminute, nfsecondn=nfsecondn, nfsecondd=nfsecondd )
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),null,'open',istop,iret)

! Allocate work array (tsea in sfc file
       allocate(tsea(lonb,latb))
! Allocate nsst variables
       allocate(xt(lonb,latb))
       allocate(xs(lonb,latb))
       allocate(xu(lonb,latb))
       allocate(xv(lonb,latb))
       allocate(xz(lonb,latb))
       allocate(zm(lonb,latb))
       allocate(xtts(lonb,latb))
       allocate(xzts(lonb,latb))
       allocate(dt_cool(lonb,latb))
       allocate(z_c(lonb,latb))
       allocate(c_0(lonb,latb))
       allocate(c_d(lonb,latb))
       allocate(w_0(lonb,latb))
       allocate(w_d(lonb,latb))
       allocate(d_conv(lonb,latb))
       allocate(ifd(lonb,latb))
       allocate(tref(lonb,latb))
       allocate(qrain(lonb,latb))

!
!      First copy entire data from sfcgcy to fname_anl, then do selective update
!
!      read the nrec_sfc variables from sfcgcy and then write then to sfcanl
!
       do n = 1, nrec_sfc
          call nemsio_readrec(gfile_sfcgcy,n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'readrec for gfile_sfcgcy nrec_sfc = ', n,'  Status = ', iret
          call nemsio_writerec(gfile_sfcanl,n,rwork1d,iret=iret)
          if ( iret /= 0 ) write(6,*) 'writerec for gfile_sfcanl, nrec_sfc = ',n, '  Status = ', iret
       end do

!
!      read surface temperature from sfcgcy and save to tsea
!
       call nemsio_readrecv(gfile_sfcgcy, 'tmp', 'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_sfcgcy),'tmp','read',istop,iret)
       tsea=reshape(rwork1d,(/size(tsea,1),size(tsea,2)/))

!      For nstanl, Only tref (foundation temperature) is updated by analysis
!                  others are updated for snow melting case
!      read 18 nsst variables from nstges
! xt
       call nemsio_readrecv(gfile_nstges, 'xt',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'xt','read',istop,iret)
       xt=reshape(rwork1d,(/size(xt,1),size(xt,2)/))
! xs
       call nemsio_readrecv(gfile_nstges, 'xs',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'xs','read',istop,iret)
       xs=reshape(rwork1d,(/size(xs,1),size(xs,2)/))
! xu
       call nemsio_readrecv(gfile_nstges, 'xu',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'xu','read',istop,iret)
       xu=reshape(rwork1d,(/size(xu,1),size(xu,2)/))
! xv
       call nemsio_readrecv(gfile_nstges, 'xv',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'xv','read',istop,iret)
       xv=reshape(rwork1d,(/size(xv,1),size(xv,2)/))
! xz
       call nemsio_readrecv(gfile_nstges, 'xz',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'xz','read',istop,iret)
       xz=reshape(rwork1d,(/size(xz,1),size(xz,2)/))
! zm
       call nemsio_readrecv(gfile_nstges, 'zm',    'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'zm','read',istop,iret)
       zm=reshape(rwork1d,(/size(zm,1),size(zm,2)/))
! xtts
       call nemsio_readrecv(gfile_nstges, 'xtts',    'sfc', 1, rwork1d,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'xtts','read',istop,iret)
       xtts=reshape(rwork1d,(/size(xtts,1),size(xtts,2)/))
! xzts
       call nemsio_readrecv(gfile_nstges, 'xzts',    'sfc', 1, rwork1d,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'xzts','read',istop,iret)
       xzts=reshape(rwork1d,(/size(xzts,1),size(xzts,2)/))
! dt_cool
       call nemsio_readrecv(gfile_nstges, 'dtcool','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'dt_cool','read',istop,iret)
       dt_cool=reshape(rwork1d,(/size(dt_cool,1),size(dt_cool,2)/))
! z_c
       call nemsio_readrecv(gfile_nstges, 'zc','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'zc','read',istop,iret)
       z_c=reshape(rwork1d,(/size(z_c,1),size(z_c,2)/))
! c_0
       call nemsio_readrecv(gfile_nstges, 'c0','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'c0','read',istop,iret)
       c_0=reshape(rwork1d,(/size(c_0,1),size(c_0,2)/))
! c_d
       call nemsio_readrecv(gfile_nstges, 'cd','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'cd','read',istop,iret)
       c_d=reshape(rwork1d,(/size(c_d,1),size(c_d,2)/))
! w_0
       call nemsio_readrecv(gfile_nstges, 'w0','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'w0','read',istop,iret)
       w_0=reshape(rwork1d,(/size(w_0,1),size(w_0,2)/))
! w_d
       call nemsio_readrecv(gfile_nstges, 'wd','sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'wd','read',istop,iret)
       w_d=reshape(rwork1d,(/size(w_d,1),size(w_d,2)/))
! tref
       call nemsio_readrecv(gfile_nstges, 'tref',  'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'tref','read',istop,iret)
       tref=reshape(rwork1d,(/size(tref,1),size(tref,2)/))
! d_conv
       call nemsio_readrecv(gfile_nstges, 'dconv',  'sfc', 1, rwork1d,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'dconv','read',istop,iret)
       d_conv=reshape(rwork1d,(/size(d_conv,1),size(d_conv,2)/))
! ifd
       call nemsio_readrecv(gfile_nstges, 'ifd',  'sfc', 1, rwork1d, iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'ifd','read',istop,iret)
       ifd=reshape(rwork1d,(/size(ifd,1),size(ifd,2)/))
! qrain
       call nemsio_readrecv(gfile_nstges, 'qrain',  'sfc', 1, rwork1d,iret=iret)
       if (iret /= 0) call error_msg(mype,trim(my_name),trim(fname_nstges),'qrain','read',istop,iret)
       qrain=reshape(rwork1d,(/size(qrain,1),size(qrain,2)/))
!
!      update tref (in nst file) & tsea (in the surface file) when Tr analysis is on
!      reset NSSTM variables for new open water grids

       where ( slmsk_ens(:,:) == zero .and. slmsk_ges(:,:) == two )

         dtf_ens(:,:) = zero

         xt(:,:)      = zero
         xs(:,:)      = zero
         xu(:,:)      = zero
         xv(:,:)      = zero
         xz(:,:)      = z_w_max
         zm(:,:)      = zero
         xtts(:,:)    = zero
         xzts(:,:)    = zero
         dt_cool(:,:) = zero
         z_c(:,:)     = zero
         c_0(:,:)     = zero
         c_d(:,:)     = zero
         w_0(:,:)     = zero
         w_d(:,:)     = zero
         d_conv(:,:)  = zero
         ifd(:,:)     = zero
         tref(:,:)    = tfrozen
         qrain(:,:)   = zero
       end where
!
!      update analysis variable: Tref (foundation temperature) for nst file
!
       where ( slmsk_ens(:,:) == zero )
          tref(:,:) = max(tref(:,:) + dtf_ens(:,:),tfrozen)
       elsewhere
          tref(:,:) = tsea(:,:)
       end where
!
!      update SST: tsea for sfc file with NSST profile
!
       if ( nst_gsi == 3 ) then
         r_zsea1 = 0.001_r_single*real(zsea1)
         r_zsea2 = 0.001_r_single*real(zsea2)
         call dtzm_2d(xt,xz,dt_cool,z_c,slmsk_ens,r_zsea1,r_zsea2,lonb,latb,dtzm)

         where ( slmsk_ens(:,:) == zero )
            tsea(:,:) = max(tref(:,:) + dtzm(:,:), tfrozen)
         end where

       endif                   ! if ( nst_gsi > 2 ) then
!
!      update tsea record in sfcanl
!
       rwork1d = reshape(tsea, (/size(rwork1d)/) )
       call nemsio_writerecv(gfile_sfcanl,'tmp','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_sfcanl),'tmp','write',istop,iret)
       write(6,100) fname_sfcanl,lonb,latb,houra,idate(1:4),iret
100    format(' nemsio_getsfcnstensupdp:  update tsea in sfcanl',a6,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)
!
!      update nsst records in nstanl
!
! slmsk
       rwork1d = reshape( slmsk_ens,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'land','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'land','write',istop,iret)
! xt
       rwork1d = reshape( xt,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xt','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'xt','write',istop,iret)
! xs
       rwork1d = reshape( xs,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xs','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'xs','write',istop,iret)
! xu
       rwork1d = reshape( xu,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xu','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'xu','write',istop,iret)
! xv
       rwork1d = reshape( xv,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xv','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'xv','write',istop,iret)
! xz
       rwork1d = reshape( xz,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xz','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'xz','write',istop,iret)
! zm
       rwork1d = reshape( zm,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'zm','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'zm','write',istop,iret)
! xtts
       rwork1d = reshape( xtts,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xtts','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'xtts','write',istop,iret)
! xzts
       rwork1d = reshape( xzts,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'xzts','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'xzts','write',istop,iret)
! z_0
       rwork1d = reshape( dt_cool,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'dtcool','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'dtcool','write',istop,iret)
! z_c
       rwork1d = reshape( z_c,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'zc','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'zc','write',istop,iret)
! c_0
       rwork1d = reshape( c_0,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'c0','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'c0','write',istop,iret)
! c_d
       rwork1d = reshape( c_d,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'cd','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'cd','write',istop,iret)
! w_0
       rwork1d = reshape( w_0,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'w0','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'w0','write',istop,iret)
! w_d
       rwork1d = reshape( w_d,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'wd','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'wd','write',istop,iret)
! d_conv
       rwork1d = reshape( d_conv,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'dconv','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'dconv','write',istop,iret)
! ifd
       rwork1d = reshape( ifd,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'ifd','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'ifd','write',istop,iret)
! tref
       rwork1d = reshape( tref,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'tref','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'tref','write',istop,iret)
! qrain
       rwork1d = reshape( qrain,(/size(rwork1d)/) )
       call nemsio_writerecv(gfile_nstanl,'qrain','sfc',1,rwork1d,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),'qrain','write',istop,iret)

       write(6,200) fname_nstanl,lonb,latb,houra,idate(1:4),iret
200    format(' nemsio_getsfcnstensupdp:  update variables in nstanl',a6,2i6,1x,f4.1,4(i4,1x),' with iret=',i2)

       deallocate(xt,xs,xu,xv,xz,zm,xtts,xzts,dt_cool,z_c,c_0,c_d,w_0,w_d,d_conv,ifd,tref,qrain)
       deallocate(rwork1d)

       call nemsio_close(gfile_sfcgcy, iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_sfcgcy),null,'close',istop,iret)

       call nemsio_close(gfile_nstges, iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstges),null,'close',istop,iret)

       call nemsio_close(gfile_sfcanl,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_sfcanl),null,'close',istop,iret)

       call nemsio_close(gfile_nstanl,iret=iret)
       if (iret /= 0) call error_msg(0,trim(my_name),trim(fname_nstanl),null,'close',istop,iret)

    endif       ! if  ( sfcio ) then
  endif ! if ( mype1 < nanals ) then

  call MPI_Barrier(MPI_COMM_WORLD,iret)

  if ( mype == 0 ) call w3tage('GETSFCNSTENSUPDP')

  call MPI_Finalize(iret)
  if ( mype == 0 .and. iret /= 0 ) &
     write(6,'(a,i5)'), 'MPI_Finalize error status, iret = ',iret

  contains

  subroutine error_msg(mype,sub_name,file_name,var_name,action,stop_code,error_code)

    use kinds, only: i_kind
    implicit none

    character(len=*), intent(in) :: sub_name,file_name,var_name,action
    integer(i_kind),  intent(in) :: mype, stop_code, error_code

    if ( mype == 0 ) then
       select case (trim(action))
       case('init')
          write(6,'(a,'':  problem with nemsio_init, Status = '', i3)') &
             trim(sub_name), error_code
       case('open')
          write(6,'(a,'':  problem opening file '',a,'', Status = '', i3)') &
             trim(sub_name), trim(file_name), error_code
       case('close')
          write(6,'(a,'':  problem closing file '',a,'', Status = '', i3)') &
             trim(sub_name), trim(file_name), error_code
       case default
          write(6,'(a,'':  ***ERROR*** '',a,tr1,a,'',variable = '',a,'',Status = '',i3)') &

             trim(sub_name),trim(action),trim(file_name),trim(var_name),error_code
       end select
     end if
     if ( stop_code /= 0 ) then
       call MPI_Abort(MPI_COMM_WORLD,stop_code,iret)
       stop
     endif
  end subroutine error_msg


END program getsfcnstensupdp
