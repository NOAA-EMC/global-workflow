program time
!************************************************************************
!
!  time.f90
!
!  log
!    05/2010	safford  reduce validation to just region 1 (global)
!    07/2010    safford  redefine regions to surface types
!************************************************************************

  use read_diag
  use bad_obs
  use bad_penalty
  use bad_chan
  use valid 

  implicit none
  integer ntype,mregion,surf_nregion,max_surf_region
  parameter (ntype=8,mregion=25, max_surf_region=5)
  integer iglobal, iland, iwater, isnowice, imixed
  parameter (iglobal=1, iland=2, iwater=3, isnowice=4, imixed=5)

  character(10),dimension(ntype):: ftype
  character(8) stid
  character(20) satname,stringd,satsis
  character(10) dum,satype,dplat
  character(40) string,diag_rad,data_file,dfile,ctl_file
  character(40),dimension(max_surf_region):: region
  character(40),dimension(mregion):: surf_region
  character :: command
  character(8) date,suffix,cycle

  integer luname,lungrd,lunctl,lndiag,nregion
  integer iyy,imm,idd,ihh,idhh,incr,iread,iflag
  integer n_chan,j,idsat,i,k,ii,nreg
  integer,dimension(mregion):: jsub
  integer,allocatable,dimension(:):: io_chan,nu_chan
  integer :: ios = 0
  integer :: channel_obs
  integer :: iret, ier, ver
  integer npred_radiag

  real rread, pen, bound
  real rlat, rlon, rmiss, obs
  real,dimension(2):: cor_tot,nbc_omg,bc_omg
  real,dimension(max_surf_region):: rlatmin,rlatmax,rlonmin,rlonmax

  real,allocatable,dimension(:):: wavenumbr,channel_count
  real,allocatable,dimension(:,:):: count,error,use,frequency,penalty,test_pen
  real,allocatable,dimension(:,:,:):: tot_cor,omg_nbc,omg_bc
 
! Variables for reading satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:)

  logical valid_count, valid_penalty
  integer       nsnow, nland, nwater, nice, nmixed, ntotal
  integer       nnsnow, nnland, nnwater, nnmixed, nntotal

! Namelist with defaults
  logical               :: retrieval            = .false.
  integer               :: nchanl               = 19
  integer               :: imkctl               = 1
  integer               :: imkdata              = 1
  character(3)          :: gesanl               = 'ges'
  integer               :: little_endian        = 1
  character(3)          :: rad_area             = 'glb'
  namelist /input/ satname,iyy,imm,idd,ihh,idhh,incr,nchanl,&
       suffix,imkctl,imkdata,retrieval,gesanl,little_endian,rad_area

  data luname,lungrd,lunctl,lndiag / 5, 51, 52, 21 /
  data rmiss /-999./
  data stringd / '.%y4%m2%d2%h2' /
  data ftype / 'count','penalty','omgnbc','total','omgbc','omgnbc2','total2','omgbc2'/
  data region / 'global', 'land', 'water', 'ice/snow', 'mixed'/
  data rlonmin / -180., -180., -180., -180., -180./
  data rlonmax / 180., 180., 180., 180., 180./
  data rlatmin / -90., -90., -90., -90., -90./
  data rlatmax / 90., 90., 90., 90., 90./


!************************************************************************
!
! Initialize variables
  iread=0
  npred_radiag = 12 

! Read namelist input
  read(luname,input)
  write(6,input)
  write(6,*)' '
  write(6,*)'gesanl  = ', gesanl
  write(6,*)'imkdata = ', imkdata

  if ( trim(gesanl) == 'anl' ) then
     ftype(3) = 'omanbc'
     ftype(5) = 'omabc'
     ftype(6) = 'omanbc2'
     ftype(8) = 'omabc2'
  endif

  surf_nregion = 5
  if ( trim(rad_area) == 'rgn' ) then
     surf_nregion = 1
  endif

  nregion=surf_nregion
  write(6,*)'surf_nregion = ', surf_nregion


  nregion=surf_nregion

! Ensure number of requested regions does not exceed specified upper limit
  if (nregion>mregion) then
     write(6,*)'***ERROR*** too many regions specified'
     write(6,*)'   maximum allowed:  mregion=',mregion
     write(6,*)'    user requested:  nregion=',nregion
     call errexit(91)
  endif


! Create filenames for diagnostic input, binary output file
  write(stringd,100) iyy,imm,idd,ihh
100 format('.',i4.4,3i2.2)

  if ( trim(gesanl) == 'ges' ) then
     diag_rad = trim(satname)
     data_file= trim(satname) // trim(stringd) // '.ieee_d'
     ctl_file = trim(satname) // '.ctl'
  else
     diag_rad = trim(satname) // '_anl'
     data_file= trim(satname) // '_anl' // trim(stringd) // '.ieee_d'
     ctl_file = trim(satname) // '_anl.ctl'
  endif

  write(6,*)'diag_rad =',diag_rad
  write(6,*)'data_file=',data_file
  write(6,*)'ctl_file =',ctl_file 
  write(6,*)'suffix   =',suffix

! Open unit to diagnostic file.  Read portion of
! header to see if file exists
  open(lndiag,file=diag_rad,form='unformatted')
  read(lndiag,err=900,end=900) dum
  rewind lndiag

! File exists.  Read header
  call get_radiag ('version',ver,ier)
  write(6,*)'read_diag version = ', ver, ier

  write(6,*)'call read_diag_header'
  call read_radiag_header( lndiag, npred_radiag, retrieval, header_fix,&
        header_chan, data_name, iflag )
  if( iflag/=0 ) then
     write(6,*)'***ERROR*** problem reading diag file header, iflag=',iflag
     call errexit(91)
  endif

! Extract observation type, satellite id, and number of channels
  satype = header_fix%obstype
  satsis = header_fix%isis
  dplat  = header_fix%id
  n_chan = header_fix%nchan

  string = trim(satype)//'_'//trim(dplat)
  write(6,*)'string,satname=',string,' ',satname
  if ( trim(string) /= trim(satname) ) then
     write(6,*)'***ERROR*** inconsistent instrument types'
     write(6,*)'  satname,string  =',satname,' ',string
     call errexit(92)
  endif

!
! initialize data integrity check flags, open output file for email warning(s)
!
  date = stringd(2:9)
  cycle = stringd(10:11) 

!  call open_bad_obs_file( date, cycle, ios )
  if ( trim(gesanl) == 'ges' ) then
     call open_bad_penalty_file( date, cycle, ios )
     call open_bad_chan_file( date, cycle, ios )

     call load_base( satname, ios )
  endif


! Allocate arrays to hold observational information
  allocate (io_chan(n_chan), nu_chan(n_chan), wavenumbr(n_chan))
  allocate (tot_cor(n_chan,mregion,2), omg_nbc(n_chan,mregion,2), &
       omg_bc(n_chan,mregion,2), count(n_chan,mregion), & 
       penalty(n_chan,mregion), test_pen(n_chan,mregion), &
       error(n_chan,mregion), use(n_chan,mregion), &
       frequency(n_chan,mregion), channel_count(n_chan))

! Zero accumulator arrays
  do ii=1,2
     do k=1,mregion
        do j=1,n_chan
           if (ii==1) then
              count(j,k) = 0.0
              penalty(j,k) = 0.0
              if( j==1) then
                 channel_count = 0.0
              endif
           endif
           tot_cor(j,k,ii) = 0.0
           omg_nbc(j,k,ii) = 0.0
           omg_bc(j,k,ii)  = 0.0
        end do
     end do
  end do


! Extract satinfo relative index
  do j=1,n_chan
     nu_chan(j)   = real( header_chan(j)%nuchan, 4 )
     io_chan(j)   = real( header_chan(j)%iochan, 4 )
     wavenumbr(j) = real( header_chan(j)%wave, 4 )
  end do
  do k=1,mregion
     do j=1,n_chan
        error(j,k)     = real( header_chan(j)%varch, 4)
        use(j,k)       = real( header_chan(j)%iuse, 4 )
        frequency(j,k) = real( header_chan(j)%freq, 4)
     end do
  end do
        

! Create control file
  if ( imkctl == 1 ) then
     write(6,*)'call create_ctl_time'

     if ( trim(gesanl) == 'ges' ) then
        dfile = trim(satname)
     else
        dfile = trim(satname) // '_anl'
     endif

     call create_ctl_time(ntype,ftype,n_chan,iyy,imm,idd,ihh,idhh,&
          incr,ctl_file,lunctl,rmiss,dfile,satype,dplat,nregion,&
          region,rlonmin,rlonmax,rlatmin,rlatmax,nu_chan,use(1,1),&
          error(1,1),frequency(1,1),wavenumbr,little_endian)
  endif

  nwater = 0; nnwater=0
  nice   = 0
  nsnow  = 0; nnsnow=0
  nland  = 0; nnland=0
  nmixed = 0; nnmixed=0
  ntotal = 0

! Loop to read entries in diagnostic file
  if ( imkdata == 1 ) then
     iflag = 0
     loopd:  do while (iflag == 0)

!       Read a record.  If read flag, iflag does not equal zero, exit loopd
        call read_radiag_data( lndiag, header_fix, retrieval, data_fix, data_chan,&
           data_extra, iflag )
        if( iflag /= 0 ) exit loopd
        iread=iread+1

!       Extract observation location.
!       Convert (0-360) lon to (-180,180)
        rlat   = data_fix%lat
        rlon   = data_fix%lon
        if (rlon>180.) rlon = rlon - 360.
        rread  = rread +1.0 

!       Detemine which subdomains the observation falls into.
!       These are now based on surface type, not geography.  All
!       obs match global (surf_region 1).

        ii=0; jsub=0;
        jsub(1)=iglobal
        nreg=1

        if ( nregion > 1 ) then
           if ( data_fix%land_frac  > 0.99 ) then
              jsub(2)=iland
              nreg=2
              nnland=nnland+1
           else if ( data_fix%water_frac > 0.99 ) then
              jsub(2)=iwater
              nreg=2
              nnwater=nnwater+1
           else if (( data_fix%snow_frac > 0.99 ) .OR. ( data_fix%ice_frac > 0.99 )) then
              jsub(2)=isnowice
              nreg=2
              nnsnow=nnsnow+1
           else
              jsub(2)=imixed
              nreg=2
              nnmixed=nnmixed+1
              write(6,*)'data_fix%land_frac,water,snow,ice = ',data_fix%land_frac, data_fix%water_frac, data_fix%snow_frac, data_fix%ice_frac
           end if
        end if


!       Channel loop
        do j = 1, n_chan

!          If observation was assimilated, accumulate sums in appropriate regions
           if (data_chan(j)%errinv > 1.e-6) then
!              pen        =  data_chan(j)%errinv*(data_chan(j)%omgbc)**2
              pen        =  (data_chan(j)%errinv*(data_chan(j)%omgbc))**2
              cor_tot(1) =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)
              nbc_omg(1) = - data_chan(j)%omgnbc
              bc_omg(1)  = - data_chan(j)%omgbc

              cor_tot(2) =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)**2
              nbc_omg(2) =  (data_chan(j)%omgnbc)**2
              bc_omg(2)  =  (data_chan(j)%omgbc)**2

              do i=1,nreg
                 k=jsub(i)

                 count(j,k)   = count(j,k) + 1.0 
                 penalty(j,k) = penalty(j,k) + pen
                 channel_count(j) = channel_count(j) + 1.0

                 do ii=1,2
                    tot_cor(j,k,ii) = tot_cor(j,k,ii) + cor_tot(ii)
                    omg_nbc(j,k,ii) = omg_nbc(j,k,ii) + nbc_omg(ii)
                    omg_bc(j,k,ii)  = omg_bc(j,k,ii)  + bc_omg(ii)
                 end do

              end do

           endif

        enddo ! channel loop


!    End of loop over diagnostic file
     enddo loopd


     write(6,*)' '
     write(6,*)'read in ',iread,' obs ',rread
     write(6,*)' '

!
!    Compute average and standard deviation
!
     do k=1,nregion
        do j=1,n_chan

           ! --- validate the count value for region 1 (global)
           !
!           if ( use(j,k) > 0.0 .AND. k == 1 .AND. imkdata == 1 ) then
!              call validate_count( j, k, count(j,k), valid_count, iret )
!              write (*,*) ' valid_count, iret = ', valid_count, iret 
!              if (  (iret == 0) .AND. (valid_count .eqv. .FALSE.) ) then
!                 write (*,*) ' calling write_bad_obs '
!                 call write_bad_obs( satname, nu_chan(j), k, count(j,k) )
!              end if
!           end if

           if (count(j,k)>0) then
               test_pen(j,k)=penalty(j,k)/count(j,k)

              !---  check for valid penalty value for region 1 (global)
              !
              if ( use(j,k) > 0.0 .AND. k == 1 .AND. imkdata == 1 .AND. trim(gesanl) == 'ges' ) then 
                 call validate_penalty( j, k, test_pen(j,k), valid_penalty, bound, iret )
                 if( (iret == 0) .AND. (valid_penalty .eqv. .FALSE.) ) then
                    call write_bad_penalty( satname, nu_chan(j), k, test_pen(j,k), bound )
                 endif
              endif

           else
              count(j,k)=rmiss
              penalty(j,k)=rmiss
           endif

        end do
     end do

! 
!    Check each assimilated channel to see if we
!    have a 0 count (no obs) for that channel.  If so, report
!    it using write_bad_chan().  
!

!
!    This is for testing purposes only
!     channel_count(1) = 0.0
!     write(6,*)' header_chan(j)%iuse, channel_count(1) = ', header_chan(1)%iuse, channel_count(1)
!
     do j=1,n_chan
!        write(6,*)' j, header_chan(j)%iuse, channel_count(j) = ', j, header_chan(j)%iuse, channel_count(j)
!        if( header_chan(j)%iuse > 0.0 ) then
!           write(6,*)' header_chan(j)%iuse > 0.0', header_chan(j)%iuse
!        else
!           write(6,*)' header_chan(j)%iuse <= 0.0', header_chan(j)%iuse
!        endif
 
!        if( channel_count(j) < 1.0 ) then
!           write(6,*)' channel_count(j) < 1.0', channel_count(j)
!        else
!           write(6,*)' channel_count(j) >= 1.0', channel_count(j)
!        endif

        if( header_chan(j)%iuse > 0.0 .and. channel_count(j) < 1.0 .AND. trim(gesanl) == 'ges' ) then
           write(6,*)' calling write_bad_chan, satname, j, nu_chan(j) = ', satname, j, nu_chan(j)
           call write_bad_chan( satname, nu_chan(j) )
        end if	! if header_chan%iuse

     end do	! channel loop

     if ( trim(gesanl) == 'ges' ) then
!       call close_bad_obs_file()
        call close_bad_penalty_file()
        call close_bad_chan_file()
     endif


!    Write output to binary output file
  
     write(6,*)' '
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((count(j,k),j=1,n_chan),k=1,nregion)
     write(lungrd) ((penalty(j,k),j=1,n_chan),k=1,nregion)
     do ii=1,2
        write(lungrd) ((omg_nbc(j,k,ii),j=1,n_chan),k=1,nregion)
        write(lungrd) ((tot_cor(j,k,ii),j=1,n_chan),k=1,nregion)
        write(lungrd) ((omg_bc (j,k,ii),j=1,n_chan),k=1,nregion)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif 
  
! Deallocate arrays
  write(6,*)' '
  write(6,*)'deallocate arrays'
  deallocate(io_chan,nu_chan,wavenumbr,tot_cor,omg_nbc,omg_bc,count,&
       penalty,error,use,frequency)
  goto 950

! Jump to here if eof or error reading diagnostic file.
900 continue
  write(6,*)'***PROBLEM reading diagnostic file.  diag_rad=',diag_rad
  close(lndiag)

  n_chan=nchanl
  if (n_chan<=0) then
     write(6,*)'***ERROR*** invalid nchanl=',nchanl,'  STOP program'
     call errexit(93)
  endif
     
  write(6,*)'load missing value ',rmiss,' into output arrays.  ',&
       nregion,n_chan
  allocate(count(n_chan,nregion),penalty(n_chan,nregion),&
       omg_nbc(n_chan,nregion,2),tot_cor(n_chan,nregion,2),&
       omg_bc(n_chan,nregion,2))
  do ii=1,2
     do k=1,nregion
        do j=1,n_chan
           if (ii==1) then
              count(j,k)  = rmiss
              penalty(j,k)= rmiss
           endif
           omg_nbc(j,k,ii) = rmiss
           tot_cor(j,k,ii) = rmiss
           omg_bc(j,k,ii)  = rmiss
        end do
     end do
  end do

  if ( imkdata == 1 ) then
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((count(j,k),j=1,n_chan),k=1,nregion)
     write(lungrd) ((penalty(j,k),j=1,n_chan),k=1,nregion)
     do ii=1,2
        write(lungrd) ((omg_nbc(j,k,ii),j=1,n_chan),k=1,nregion)
        write(lungrd) ((tot_cor(j,k,ii),j=1,n_chan),k=1,nregion)
        write(lungrd) ((omg_bc (j,k,ii),j=1,n_chan),k=1,nregion)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif
  deallocate(count,penalty,omg_nbc,tot_cor,omg_bc)

! End of program
950 continue
  stop
end program time
