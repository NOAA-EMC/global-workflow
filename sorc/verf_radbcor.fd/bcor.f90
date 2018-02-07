!
!  bcor.f90
!
program bcor
  use read_diag

  implicit none
  integer ntype,mregion,surf_nregion,max_surf_region
  parameter (ntype=30,mregion=25,max_surf_region=5)
  integer iglobal, iland, iwater, isnowice, imixed
  parameter( iglobal=1, iland=2, iwater=3, isnowice=4, imixed=5 )

  character(10),dimension(ntype):: ftype
  character(20) satname,stringd,satsis,mod_satname
  character(10) dum,satype,dplat
  character(40) string,diag_rad,data_file,ctl_file
  character(40),dimension(max_surf_region):: region
  character(10) suffix

  integer luname,lungrd,lunctl,lndiag
  integer iyy,imm,idd,ihh,idhh,incr,iread,iflag
  integer n_chan,j,idsat,i,k,ii,nsub
  integer,dimension(mregion):: jsub
  integer,allocatable,dimension(:):: io_chan,nu_chan
  integer npred_radiag,angord

  real pen,rread
  real weight,rlat,rlon,rmiss,obs,biascor,obsges,obsgesnbc,rterm
  real,dimension(2):: cor_total,cor_fixang,cor_lapse,cor_lapse2,&
       cor_const,cor_scangl,cor_clw,cor_cos_ssmis,cor_sin_ssmis,&
       cor_emiss,cor_ordang4,cor_ordang3,cor_ordang2,cor_ordang1

  real,dimension(max_surf_region):: rlatmin,rlatmax,rlonmin,rlonmax

  real,allocatable,dimension(:):: wavenumbr
  real,allocatable,dimension(:,:):: count,error,use,frequency,penalty
  real,allocatable,dimension(:,:,:):: total_cor,fixang_cor,lapse_cor,&
       lapse2_cor,const_cor,scangl_cor,clw_cor,cos_ssmis_cor,sin_ssmis_cor,&
       emiss_cor,ordang4_cor,ordang3_cor,ordang2_cor,ordang1_cor

  logical no_obs

! Variables for reading satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:)

  integer  nsnow, nland, nwater, nice, nmixed, ntotal
  integer  nnsnow, nnland, nnwater, nnmixed, nntotal

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
  data ftype / 'count', 'penalty', &
       'avgtotal', 'avgfixang', 'avglapse', 'avglapse2', &
       'avgconst', 'avgscangl', 'avgclw', &
       'avgcos', 'avgsin', 'avgemiss', 'avgordang4', &
       'avgordang3', 'avgordang2', 'avgordang1', &
       'sdvtotal', 'sdvfixang', 'sdvlapse', 'sdvlapse2', &
       'sdvconst', 'sdvscangl', 'sdvclw', &
       'sdvcos', 'sdvsin', 'sdvemiss', 'sdvordang4',&
       'sdvordang3', 'sdvordang2', 'sdvordang1' /
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
  write(6,*)'gesanl = ', gesanl
  write(6,*)'rad_area = ', rad_area
  write(6,*)' '

  surf_nregion = 5
  if ( trim(rad_area) == 'rgn' ) then
     surf_nregion = 1
  endif

  write(6,*)'surf_nregion = ', surf_nregion


! Ensure number of requested regions does not exceed specified upper limit
  if (surf_nregion>mregion) then
     write(6,*)'***ERROR*** too many regions specified'
     write(6,*)'   maximum allowed:  mregion=',mregion
     write(6,*)'    user requested:  surf_nregion=',surf_nregion
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


! Open unit to diagnostic file.  Read portion of header to 
! see if file exists
  open(lndiag,file=diag_rad,form='unformatted')
  read(lndiag,err=900,end=900) dum
  rewind lndiag

! File exists.  Read header
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
  angord = header_fix%angord

  write(6,*)'satype,dplat,n_chan,angord=',satype,' ',dplat,n_chan,angord

  string = trim(satype)//'_'//trim(dplat)
  write(6,*)'string,satname=',string,' ',satname
  if ( trim(string) /= trim(satname) ) then
     write(6,*)'***ERROR*** inconsistent instrument types'
     write(6,*)'  satname,string  =',satname,' ',string
     call errexit(92)
  endif


! Allocate arrays to hold observational information
  write(6,*)' '
  write(6,*)'allocate arrays'
  allocate (io_chan(n_chan), nu_chan(n_chan), wavenumbr(n_chan))
  allocate (total_cor(n_chan,mregion,2), fixang_cor(n_chan,mregion,2), &
       lapse_cor(n_chan,mregion,2), lapse2_cor(n_chan,mregion,2), &
       const_cor(n_chan,mregion,2), scangl_cor(n_chan,mregion,2), &
       clw_cor(n_chan,mregion,2), cos_ssmis_cor(n_chan,mregion,2), &
       sin_ssmis_cor(n_chan,mregion,2), emiss_cor(n_chan,mregion,2), &
       ordang4_cor(n_chan,mregion,2), ordang3_cor(n_chan,mregion,2), &
       ordang2_cor(n_chan,mregion,2), ordang1_cor(n_chan,mregion,2), &
       count(n_chan,mregion), penalty(n_chan,mregion), &
       error(n_chan,mregion), use(n_chan,mregion), &
       frequency(n_chan,mregion))

! Zero accumulator arrays
  do ii=1,2
     do k=1,mregion
        do j=1,n_chan
           if (ii==1) then
              count(j,k) = 0.0
              penalty(j,k) = 0.0
           endif
           total_cor(j,k,ii)      = 0.0
           fixang_cor(j,k,ii)     = 0.0
           lapse_cor(j,k,ii)      = 0.0
           lapse2_cor(j,k,ii)     = 0.0
           const_cor(j,k,ii)      = 0.0
           scangl_cor(j,k,ii)     = 0.0
           clw_cor(j,k,ii)        = 0.0
           cos_ssmis_cor(j,k,ii)  = 0.0
           sin_ssmis_cor(j,k,ii)  = 0.0
           emiss_cor(j,k,ii)      = 0.0
           ordang4_cor(j,k,ii)    = 0.0
           ordang3_cor(j,k,ii)    = 0.0
           ordang2_cor(j,k,ii)    = 0.0
           ordang1_cor(j,k,ii)    = 0.0
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
        

! Create GrADS control file
  if ( imkctl == 1 ) then
     write(6,*)'call create_ctl_bcor'

     if ( trim(gesanl) == 'ges' ) then
        mod_satname = trim(satname)
     else
        mod_satname = trim(satname) // '_anl'
     endif


     call create_ctl_bcor(ntype,ftype,n_chan,iyy,imm,idd,ihh,idhh,&
          incr,ctl_file,lunctl,rmiss,mod_satname,satype,dplat,surf_nregion,&
          region,rlonmin,rlonmax,rlatmin,rlatmax,nu_chan,use(1,1),error(1,1),&
          frequency(1,1),wavenumbr,little_endian)
     
  endif

  nwater = 0; nnwater = 0
  nland  = 0; nnland  = 0
  nsnow  = 0; nnsnow  = 0
  nice   = 0
  nmixed = 0; nnmixed = 0
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

!       Extract obervation location and mpi weight.  Convert (0-360) lon to (-180,180)
        rlat   = data_fix%lat
        rlon   = data_fix%lon
        if (rlon>180.) rlon = rlon - 360.
        rread  = rread + 1.0 

        ntotal = ntotal + 1
        jsub(1)= iglobal
        nsub   = 1

        if ( surf_nregion > 1 ) then
           if ( data_fix%water_frac > 0.99 ) then
              nwater = nwater + 1
           else if ( data_fix%land_frac  > 0.99 ) then
              nland  = nland  + 1
           else if ( data_fix%snow_frac  > 0.99 ) then
              nsnow  = nsnow  + 1
           else if ( data_fix%ice_frac   > 0.99 ) then
              nice   = nice   + 1
           else
              nmixed = nmixed + 1
           end if



!          Detemine into which subdomains the observation falls.
!          These are now based on surface type, not geography.  All
!          obs match global (surf_region 1).
!
           ii=0; 
!jsub=0;
           if ( data_fix%land_frac  > 0.99 ) then
              jsub(2)=iland
              nsub=2
              nnland=nnland+1
           else if ( data_fix%water_frac > 0.99 ) then
              jsub(2)=iwater
              nsub=2
              nnwater=nnwater+1
           else if (( data_fix%snow_frac > 0.99 ) .OR. ( data_fix%ice_frac > 0.99 )) then
              jsub(2)=isnowice
              nsub=2
              nnsnow=nnsnow+1
           else
              jsub(2)=imixed
              nsub=2
              nnmixed=nnmixed+1
              write(6,*)'data_fix%land_frac,water,snow,ice = ',data_fix%land_frac, data_fix%water_frac, data_fix%snow_frac, data_fix%ice_frac
           end if
        end if


!       Channel loop
        do j = 1, n_chan

!          If observation was assimilated, accumulate sums in appropriate regions
           if (data_chan(j)%errinv > 1.e-6) then
!              pen              =  data_chan(j)%errinv*(data_chan(j)%omgbc)**2
              pen              =  (data_chan(j)%errinv*(data_chan(j)%omgbc))**2
              cor_total(1)     =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)
              cor_fixang(1)    =  data_chan(j)%bifix(angord+1)
              cor_lapse(1)     =  data_chan(j)%bilap
              cor_lapse2(1)    =  data_chan(j)%bilap2
              cor_const(1)     =  data_chan(j)%bicons
              cor_scangl(1)    =  data_chan(j)%biang
              cor_clw(1)       =  data_chan(j)%biclw
              cor_cos_ssmis(1) =  data_chan(j)%bicos
              cor_sin_ssmis(1) =  data_chan(j)%bisin
              cor_emiss(1)     =  data_chan(j)%biemis
              if (angord >= 4 ) then
                 cor_ordang4(1)   =  data_chan(j)%bifix(1)
                 cor_ordang3(1)   =  data_chan(j)%bifix(2)
                 cor_ordang2(1)   =  data_chan(j)%bifix(3)
                 cor_ordang1(1)   =  data_chan(j)%bifix(4)
              else
                 cor_ordang4(1)   =  0.0 
                 cor_ordang3(1)   =  0.0 
                 cor_ordang2(1)   =  0.0 
                 cor_ordang1(1)   =  0.0 
              endif
!              do i=1,angord
!                 cor_ordang4
!           write(string,'(i2.2)') header_fix%angord-i+1
!           data_name%chn(15+i)= 'bifix' // string
!              end do


              cor_total(2)     =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)**2
              cor_fixang(2)    =  (data_chan(j)%bifix(angord+1))**2
              cor_lapse(2)     =  (data_chan(j)%bilap)**2
              cor_lapse2(2)    =  (data_chan(j)%bilap2)**2
              cor_const(2)     =  (data_chan(j)%bicons)**2
              cor_scangl(2)    =  (data_chan(j)%biang)**2
              cor_clw(2)       =  (data_chan(j)%biclw)**2
              cor_cos_ssmis(2) =  data_chan(j)%bicos**2
              cor_sin_ssmis(2) =  data_chan(j)%bisin**2
              cor_emiss(2)     =  data_chan(j)%biemis**2
              if (angord >= 4 ) then
                 cor_ordang4(1)   =  data_chan(j)%bifix(1)**2
                 cor_ordang3(1)   =  data_chan(j)%bifix(2)**2
                 cor_ordang2(1)   =  data_chan(j)%bifix(3)**2
                 cor_ordang1(1)   =  data_chan(j)%bifix(4)**2
              else
                 cor_ordang4(2)   =  0.0 
                 cor_ordang3(2)   =  0.0 
                 cor_ordang2(2)   =  0.0 
                 cor_ordang1(2)   =  0.0 
              end if
 
              do i=1,nsub
                 k=jsub(i)
                 count(j,k) = count(j,k) +1.0 
                 penalty(j,k) = penalty(j,k) + pen

                 do ii=1,2
                    total_cor(j,k,ii)     = total_cor(j,k,ii)     + cor_total(ii)
                    fixang_cor(j,k,ii)    = fixang_cor(j,k,ii)    + cor_fixang(ii)
                    lapse_cor(j,k,ii)     = lapse_cor(j,k,ii)     + cor_lapse(ii)
                    lapse2_cor(j,k,ii)    = lapse2_cor(j,k,ii)    + cor_lapse2(ii)
                    const_cor(j,k,ii)     = const_cor(j,k,ii)     + cor_const(ii)
                    scangl_cor(j,k,ii)    = scangl_cor(j,k,ii)    + cor_scangl(ii)
                    clw_cor(j,k,ii)       = clw_cor(j,k,ii)       + cor_clw(ii)
                    cos_ssmis_cor(j,k,ii) = cos_ssmis_cor(j,k,ii) + cor_cos_ssmis(ii)
                    sin_ssmis_cor(j,k,ii) = sin_ssmis_cor(j,k,ii) + cor_sin_ssmis(ii)
                    emiss_cor(j,k,ii)     = emiss_cor(j,k,ii)     + cor_emiss(ii)
                    ordang4_cor(j,k,ii)   = ordang4_cor(j,k,ii)   + cor_ordang4(ii)
                    ordang3_cor(j,k,ii)   = ordang3_cor(j,k,ii)   + cor_ordang3(ii)
                    ordang2_cor(j,k,ii)   = ordang2_cor(j,k,ii)   + cor_ordang2(ii)
                    ordang1_cor(j,k,ii)   = ordang1_cor(j,k,ii)   + cor_ordang1(ii)
                 end do

              end do
           endif

        enddo ! channel loop

!    End of loop over diagnostic file
     enddo loopd
     close(lndiag)
     write(6,*)' '
     write(6,*)'read in ',iread,' obs ',rread
     write(6,*)' '

     write(6,*)'nwater, nland, nice, nsnow, nmixed, ntotal = ', nwater, nland, nice, nsnow, nmixed, ntotal
     nntotal=nnwater+nnland+nnsnow+nnmixed
     write(6,*)'nnwater, nnland, nnsnow, nnmixed, nntotal = ', nnwater, nnland, nnsnow, nnmixed, nntotal

!    Compute average and standard deviation
     do k=1,surf_nregion
        do j=1,n_chan
           call avgsdv(count(j,k),total_cor(j,k,1),     total_cor(j,k,2),     rmiss)
           call avgsdv(count(j,k),fixang_cor(j,k,1),    fixang_cor(j,k,2),    rmiss)
           call avgsdv(count(j,k),lapse_cor(j,k,1),     lapse_cor(j,k,2),     rmiss)
           call avgsdv(count(j,k),lapse2_cor(j,k,1),    lapse2_cor(j,k,2),    rmiss)
           call avgsdv(count(j,k),const_cor(j,k,1),     const_cor(j,k,2),     rmiss)
           call avgsdv(count(j,k),scangl_cor(j,k,1),    scangl_cor(j,k,2),    rmiss)
           call avgsdv(count(j,k),clw_cor(j,k,1),       clw_cor(j,k,2),       rmiss)
           call avgsdv(count(j,k),cos_ssmis_cor(j,k,1), cos_ssmis_cor(j,k,2), rmiss)
           call avgsdv(count(j,k),sin_ssmis_cor(j,k,1), sin_ssmis_cor(j,k,2), rmiss)
           call avgsdv(count(j,k),emiss_cor(j,k,1),     emiss_cor(j,k,2),     rmiss)
           call avgsdv(count(j,k),ordang4_cor(j,k,1),   ordang4_cor(j,k,2),   rmiss)
           call avgsdv(count(j,k),ordang3_cor(j,k,1),   ordang3_cor(j,k,2),   rmiss)
           call avgsdv(count(j,k),ordang2_cor(j,k,1),   ordang2_cor(j,k,2),   rmiss)
           call avgsdv(count(j,k),ordang1_cor(j,k,1),   ordang1_cor(j,k,2),   rmiss)

           if (count(j,k)>0) then
              penalty(j,k)=penalty(j,k)/count(j,k)
           else
              count(j,k)=rmiss
              penalty(j,k)=rmiss
           endif
        end do
     end do

!    Write output to binary output file
     write(6,*)' '
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((count(j,k),j=1,n_chan),k=1,surf_nregion)
     write(lungrd) ((penalty(j,k),j=1,n_chan),k=1,surf_nregion)
     do ii=1,2
        write(lungrd) ((total_cor    (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((fixang_cor   (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((lapse_cor    (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((lapse2_cor   (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((const_cor    (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((scangl_cor   (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((clw_cor      (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((cos_ssmis_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((sin_ssmis_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((emiss_cor    (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((ordang4_cor  (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((ordang3_cor  (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((ordang2_cor  (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((ordang1_cor  (j,k,ii),j=1,n_chan),k=1,surf_nregion)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif 

! Deallocate arrays
  write(6,*)' '
  write(6,*)'deallocate arrays'
  if(allocated(io_chan))       deallocate (io_chan)
  if(allocated(nu_chan))       deallocate (nu_chan)
  if(allocated(wavenumbr))     deallocate (wavenumbr)
  if(allocated(total_cor))     deallocate (total_cor)
  if(allocated(fixang_cor))    deallocate (fixang_cor)
  if(allocated(lapse_cor))     deallocate (lapse_cor)
  if(allocated(lapse2_cor))    deallocate (lapse2_cor)
  if(allocated(const_cor))     deallocate (const_cor)
  if(allocated(scangl_cor))    deallocate (scangl_cor)
  if(allocated(clw_cor))       deallocate (clw_cor)
  if(allocated(cos_ssmis_cor)) deallocate (cos_ssmis_cor)
  if(allocated(sin_ssmis_cor)) deallocate (sin_ssmis_cor)
  if(allocated(emiss_cor))     deallocate (emiss_cor)
  if(allocated(ordang4_cor))   deallocate (ordang4_cor)
  if(allocated(ordang3_cor))   deallocate (ordang3_cor)
  if(allocated(ordang2_cor))   deallocate (ordang2_cor)
  if(allocated(ordang1_cor))   deallocate (ordang1_cor)
  if(allocated(count))         deallocate (count)
  if(allocated(penalty))       deallocate (penalty)
  if(allocated(error))         deallocate (error)
  if(allocated(use))           deallocate (use)
  if(allocated(frequency))     deallocate (frequency)
  

!  deallocate(io_chan,nu_chan,wavenumbr,total_cor,fixang_cor,lapse_cor,&
!       lapse2_cor,const_cor,scangl_cor,clw_cor,count,penalty,error,use,&
!       frequency)

  write(6,*)'deallocated arrays'
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
       surf_nregion,n_chan
  allocate(count(n_chan,surf_nregion),penalty(n_chan,surf_nregion))
  allocate(total_cor(n_chan,mregion,2), fixang_cor(n_chan,mregion,2), &
       lapse_cor(n_chan,mregion,2), lapse2_cor(n_chan,mregion,2), &
       const_cor(n_chan,mregion,2), scangl_cor(n_chan,mregion,2), &
       clw_cor(n_chan,mregion,2))

  write(6,*)'load missing value ',rmiss,' into output arrays'
  do ii=1,2
     do k=1,surf_nregion
        do j=1,n_chan
           if (ii==1) then
              count(j,k)  =rmiss
              penalty(j,k)=rmiss
           endif
           total_cor(j,k,ii) =rmiss
           fixang_cor(j,k,ii)=rmiss
           lapse_cor(j,k,ii) =rmiss
           lapse2_cor(j,k,ii)=rmiss
           const_cor(j,k,ii) =rmiss
           scangl_cor(j,k,ii)=rmiss
           clw_cor(j,k,ii)   =rmiss
        end do
     end do
  end do

  if ( imkdata == 1 ) then
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((count(j,k),j=1,n_chan),k=1,surf_nregion)
     write(lungrd) ((penalty(j,k),j=1,n_chan),k=1,surf_nregion)
     do ii=1,2
        write(lungrd) ((total_cor    (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((fixang_cor   (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((lapse_cor    (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((lapse2_cor   (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((const_cor    (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((scangl_cor   (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((clw_cor      (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((cos_ssmis_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((sin_ssmis_cor(j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((emiss_cor    (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((ordang4_cor  (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((ordang3_cor  (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((ordang2_cor  (j,k,ii),j=1,n_chan),k=1,surf_nregion)
        write(lungrd) ((ordang1_cor  (j,k,ii),j=1,n_chan),k=1,surf_nregion)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif
  deallocate(count,penalty,total_cor,fixang_cor,lapse_cor,lapse2_cor,&
       const_cor,scangl_cor,clw_cor)

! End of program
950 continue
  write(6,*) 'exiting program bcor'
  stop
end program bcor
