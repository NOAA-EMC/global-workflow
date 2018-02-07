program angle
  use read_diag

  implicit none
  integer ntype,mregion,mstep,surf_nregion,max_surf_region
  parameter (ntype=35,mregion=25,mstep=100,max_surf_region=5)
  integer iglobal, iland, iwater, isnowice, imixed
  parameter (iglobal=1, iland=2, iwater=3, isnowice=4, imixed=5)
 
  character(10),dimension(ntype):: ftype
  character(8) stid
  character(20) satname,stringd
  character(10) satype,dplat
  character(20) dum,satsis,satscan_sis
  character(40) string,diag_rad,data_file,dfile,ctl_file
  character(40),dimension(max_surf_region):: surf_region
  character(8)  date,suffix,cycle
  character(len=1024) :: command

  integer luname,lungrd,lndiag,lunang,lunctl
  integer iyy,imm,idd,ihh,idhh,incr,iread,iflag,ipos
  integer n_chan,j,i,k,ii,nsub,jiter,jj
  integer,dimension(mregion):: jsub
  integer,allocatable,dimension(:):: io_chan,nu_chan
  integer imatch, npred_radiag, angord
  
  real start,step
  integer nstep,iscan
  character(1) cflg
  real rang,pen
  real weight,rlat,rlon,rmiss,obs,biascor,obsges,obsgesnbc,rterm,rread
  real,dimension(2):: cor_tot,nbc_omg,bc_omg
  real,dimension(2):: cor_fixang,cor_lapse,cor_lapse2,cor_const,cor_scangl,cor_clw
  real,dimension(2):: cor_cos,cor_sin,cor_emiss
  real,dimension(2):: cor_ordang4,cor_ordang3,cor_ordang2,cor_ordang1
  real,dimension(max_surf_region):: surf_rlatmin,surf_rlatmax,surf_rlonmin,surf_rlonmax

  real,allocatable,dimension(:):: wavenumbr,error,use,frequency
  real,allocatable,dimension(:,:):: timang
  real,allocatable,dimension(:,:,:):: count,penalty
  real,allocatable,dimension(:,:,:,:):: tot_cor,omg_nbc,omg_bc
  real,allocatable,dimension(:,:,:,:):: fixang_cor,lapse_cor,lapse2_cor
  real,allocatable,dimension(:,:,:,:):: const_cor,scangl_cor,clw_cor 
  real,allocatable,dimension(:,:,:,:):: cos_cor,sin_cor,emiss_cor
  real,allocatable,dimension(:,:,:,:):: ordang4_cor,ordang3_cor,ordang2_cor,ordang1_cor

! Variables for reading satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list) ,allocatable :: data_extra(:,:)

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

  data luname,lungrd,lunctl,lndiag,iscan / 5, 51, 52, 21, 31 /
  data lunang / 22 /
  data rmiss /-999./
  data stringd / '.%y4%m2%d2%h2' /
  data ftype / 'satang', 'count', 'penalty', &
       'omgnbc', 'total', 'omgbc', &
       'fixang', 'lapse', 'lapse2', &
       'const', 'scangl', 'clw', &
       'cos','sin','emiss','ordang4',&
       'ordang3','ordang2','ordang1',&
       'omgnbc_2', 'total_2', 'omgbc_2', &
       'fixang_2', 'lapse_2', 'lapse2_2', & 
       'const_2', 'scangl_2', 'clw_2', &
       'cos_2', 'sin_2', 'emiss_2', &
       'ordang4_2','ordang3_2','ordang2_2', &
       'ordang1_2' /
  data surf_region / 'global', 'land', 'water', 'ice/snow', 'mixed'/
  data surf_rlonmin / -180., -180., -180., -180., -180./
  data surf_rlonmax / 180., 180., 180., 180., 180./
  data surf_rlatmin / -90., -90., -90., -90., -90./
  data surf_rlatmax / 90., 90., 90., 90., 90./
!************************************************************************
!
! Initialize variables
  iread=0
  npred_radiag = 12

! Read namelist input
  read(luname,input)
  write(6,input)
  write(6,*)' '
  write(6,*)'gesanl = ', gesanl
  write(6,*)'rad_area = ', rad_area

  surf_nregion = 5
  if ( trim(rad_area) == 'rgn' ) then
     surf_nregion = 1
  endif

  if ( trim(gesanl) == 'anl' ) then
     ftype(4)  = 'omanbc'
     ftype(6)  = 'omabc'
     ftype(13) = 'omanbc_2'
     ftype(15) = 'omabc_2' 
  endif

! Ensure number of requested regions does not exceed specified upper limit
  if (surf_nregion>mregion) then
     write(6,*)'***ERROR*** too many regions specified'
     write(6,*)'   maximum allowed:  mregion=',mregion
     write(6,*)'    user requested:  surf_nregion=',surf_nregion
     call errexit(91)
  endif

  date  = stringd(2:9)
  cycle = stringd(10:11)

! Create filenames for diagnostic input, binary output files
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
  write(6,*)'suffix   =',suffix
  write(6,*)'ctl_file =',ctl_file
  write(6,*)'imkctl    =',imkctl
  write(6,*)'imkdata   =',imkdata
  write(6,*)'little_endian =', little_endian


! Open unit to diagnostic file.  Read portion of 
!  header to see if file exists
  open(lndiag,file=diag_rad,form='unformatted')
  read(lndiag,err=900,end=900) dum
  rewind lndiag

! File exists.  Read header
  write(6,*)'call read_diag_header'
  call read_radiag_header( lndiag, npred_radiag, retrieval,&
        header_fix, header_chan, data_name, iflag )
!
! If there was an error reading the header try to convert from little endian
! and re-read.  If that fails exit.
!
  if( iflag/=0 ) then
     write(6,*)'***ERROR*** problem reading diag file header, iflag=',iflag
     call errexit(91)
  endif


! Extract observation type, satellite id, and number of channels
  satype = header_fix%obstype
  satsis = header_fix%isis       
  dplat  = header_fix%id
  n_chan = header_fix%nchan
  jiter  = header_fix%jiter
  angord = header_fix%angord
 
  write(6,*)'satype,satid,n_chan,angord=',satype,' ',dplat,' ',n_chan,' ',angord

  string = trim(satype) //'_'// trim(dplat)
  write(6,*)'string,satname=',string,' ',satname
  if ( trim(string) /= trim(satname) ) then
     write(6,*)'***ERROR*** inconsistent instrument types'
     write(6,*)'  satname,string  =',satname,' ',string
     call errexit(92)
  endif

!  open scan info file compiled in the source directory
   open(iscan,file='scaninfo.txt',form='formatted')
  do 
   read(iscan,1000,IOSTAT=iflag) cflg,satscan_sis,start,step,nstep
   write(6,*) 'satscan_sis,start,step,nstep=',satscan_sis,start,step,nstep
   if( iflag /= 0 ) exit
   if(trim(satname) == trim(satscan_sis)) exit
  enddo
!1000 format(a1,a20,2f10.2,i10)
1000 format(a1,a20,2f10.3,i10)

  write(6,*) 'satscan_sis,start,step,nstep=',satscan_sis,start,step,nstep


! Allocate arrays to hold observational information
  write(6,*)' '
  write(6,*)'allocate arrays'
  allocate (io_chan(n_chan), nu_chan(n_chan), wavenumbr(n_chan), &
       error(n_chan), use(n_chan), frequency(n_chan))
  allocate (timang(mstep,n_chan))
  allocate (tot_cor(mstep,n_chan,surf_nregion,2), &
       omg_nbc(mstep,n_chan,surf_nregion,2), &
       omg_bc(mstep,n_chan,surf_nregion,2), &
       count(mstep,n_chan,surf_nregion), &
       penalty(mstep,n_chan,surf_nregion),&
       fixang_cor(mstep,n_chan,surf_nregion,2),lapse_cor(mstep,n_chan,surf_nregion,2),&
       lapse2_cor(mstep,n_chan,surf_nregion,2),clw_cor(mstep,n_chan,surf_nregion,2),&
       const_cor(mstep,n_chan,surf_nregion,2), scangl_cor(mstep,n_chan,surf_nregion,2),&
       cos_cor(mstep,n_chan,surf_nregion,2),   sin_cor(mstep,n_chan,surf_nregion,2),&
       emiss_cor(mstep,n_chan,surf_nregion,2), ordang4_cor(mstep,n_chan,surf_nregion,2),&
       ordang3_cor(mstep,n_chan,surf_nregion,2),   ordang2_cor(mstep,n_chan,surf_nregion,2),&
       ordang1_cor(mstep,n_chan,surf_nregion,2))

! Zero accumulator arrays
  do ii=1,2
     do k=1,surf_nregion
        do j=1,n_chan
           do i=1,mstep
              if (ii==1) then
                 count(i,j,k) = 0.0
                 penalty(i,j,k) = 0.0
              endif
              tot_cor(i,j,k,ii) = 0.0
              omg_nbc(i,j,k,ii) = 0.0
              omg_bc(i,j,k,ii)  = 0.0
              fixang_cor(i,j,k,ii)  = 0.0
              lapse_cor(i,j,k,ii)  = 0.0
              lapse2_cor(i,j,k,ii)  = 0.0
              clw_cor(i,j,k,ii)  = 0.0
              const_cor(i,j,k,ii)  = 0.0
              scangl_cor(i,j,k,ii)  = 0.0
              cos_cor(i,j,k,ii)  = 0.0
              sin_cor(i,j,k,ii)  = 0.0
              emiss_cor(i,j,k,ii)  = 0.0
              ordang4_cor(i,j,k,ii)  = 0.0
              ordang3_cor(i,j,k,ii)  = 0.0
              ordang2_cor(i,j,k,ii)  = 0.0
              ordang1_cor(i,j,k,ii)  = 0.0
           end do
        end do
     end do
  end do

! Note:  timang has been deprecated and the satang file is no longer 
!   used.  See the plot_angle_sep.*.gs scripts for how the timang
!   values are derived from the diagnostic file contents.  The 
!   timang variable has been kept to avoid making a change in
!   ieee_d file format.
  do j=1,n_chan
     do i=1,mstep
        timang(i,j) = rmiss
     end do
  end do

! Extract satinfo relative index
  do j=1,n_chan
     nu_chan(j)   = real( header_chan(j)%nuchan, 4 )
     io_chan(j)   = real( header_chan(j)%iochan, 4 )
     wavenumbr(j) = real( header_chan(j)%wave, 4 )
     error(j)     = real( header_chan(j)%varch, 4)
     use(j)       = real( header_chan(j)%iuse, 4 )
     frequency(j) = real( header_chan(j)%freq, 4)
!    print *,nu_chan(j),io_chan(j),wavenumbr(j),error(j),use(j),frequency(j)
  end do
        


  write(6,*)'beginning read entries in diag file'
  nwater = 0; nnwater=0
  nice   = 0
  nsnow  = 0; nnsnow=0
  nland  = 0; nnland=0
  nmixed = 0; nnmixed=0;
  ntotal = 0; 

! Loop to read entries in diagnostic file
  iflag = 0
  loopd:  do while (iflag == 0)

!    Read a record.  If read flag, iflag does not equal zero, exit loopd
     call read_radiag_data( lndiag, header_fix, retrieval, data_fix, data_chan,&
           data_extra, iflag )
     if( iflag /= 0 ) exit loopd
     iread=iread+1

!    Extract observation location, scan position, and mpi weight.  
!    Convert (0-360) lon to (-180,180)
     rlat   = data_fix%lat
     rlon   = data_fix%lon
     ipos   = data_fix%senscn_pos         !! sensor scan position(integer)     
     rang   = data_fix%satzen_ang         !! satellite zenith angle (deg) 

     ntotal = ntotal + 1
     jsub(1)=iglobal

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


        if (rlon>180.) rlon = rlon - 360.
        if (ipos<1) then
           write(6,*)'scan position less than 1.  ipos=',ipos
           ipos=1
        endif
        if (ipos>nstep) then
           write(6,*)'scan position > nstep.  ipos,nstep,',&
                ipos,nstep
           ipos=nstep
        endif
        rread  = rread + 1.0 


!       Detemine which subdomains the observation falls into
!       These are now based on surface type, not geography.  All
!       obs match global (surf_region 1).
!
        ii=0; jsub=0; 
        jsub(1)=iglobal
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


!    Channel loop
     do j = 1, n_chan

!       If observation was assimilated, accumulate sums for appropriate 
!        scan angle and regions
        cor_cos       = 0.0
        cor_sin       = 0.0
        cor_emiss     = 0.0
        cor_ordang4   = 0.0
        cor_ordang3   = 0.0
        cor_ordang2   = 0.0
        cor_ordang1   = 0.0

        if (data_chan(j)%errinv > 1.e-6) then
!           pen        =  data_chan(j)%errinv*(data_chan(j)%omgbc)**2
           pen        =  (data_chan(j)%errinv*(data_chan(j)%omgbc))**2


           cor_tot(1) =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)
           nbc_omg(1) = - (data_chan(j)%omgnbc)
           bc_omg(1)  = - (data_chan(j)%omgbc)


           if ( trim(rad_area) == 'rgn' ) then
              write(6,*) 'chan,pen,cor_tot(1),nbc_omg(1),bc_omb(1) = ', j,pen,cor_tot(1),nbc_omg(1),bc_omg(1)
           endif

           cor_tot(2) =  (data_chan(j)%omgnbc - data_chan(j)%omgbc)**2
           nbc_omg(2) =  (data_chan(j)%omgnbc)**2
           bc_omg(2)  =  (data_chan(j)%omgbc)**2

           cor_fixang(1) =  data_chan(j)%bifix(angord+1)
           cor_lapse(1)  =  data_chan(j)%bilap
           cor_lapse2(1) =  data_chan(j)%bilap2
           cor_const(1)  =  data_chan(j)%bicons
           cor_scangl(1) =  data_chan(j)%biang
           cor_clw(1)    =  data_chan(j)%biclw
           cor_cos(1)    =  data_chan(j)%bicos
           cor_sin(1)    =  data_chan(j)%bisin
           cor_emiss(1)  =  data_chan(j)%biemis
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

           cor_fixang(2) =  (data_chan(j)%bifix(angord+1))**2
           cor_lapse(2)  =  (data_chan(j)%bilap)**2
           cor_lapse2(2) =  (data_chan(j)%bilap2)**2
           cor_const(2)  =  (data_chan(j)%bicons)**2
           cor_scangl(2) =  (data_chan(j)%biang)**2
           cor_clw(2)    =  (data_chan(j)%biclw)**2
           cor_cos(2)    =  (data_chan(j)%bicos)**2
           cor_sin(2)    =  (data_chan(j)%bisin)**2
           cor_emiss(2)  =  (data_chan(j)%biemis)**2
           if (angord >= 4 ) then
              cor_ordang4(2)   =  (data_chan(j)%bifix(1))**2
              cor_ordang3(2)   =  (data_chan(j)%bifix(2))**2
              cor_ordang2(2)   =  (data_chan(j)%bifix(3))**2
              cor_ordang1(2)   =  (data_chan(j)%bifix(4))**2
           else
              cor_ordang4(2)   =  0.0
              cor_ordang3(2)   =  0.0
              cor_ordang2(2)   =  0.0
              cor_ordang1(2)   =  0.0
           endif

           
           if ( trim(rad_area) == 'rgn' ) then
              nsub = 1 
           endif
           do i=1,nsub
              if ( trim(rad_area) == 'rgn' ) then
                 write(6,*) 'INSIDE i 1 to nsub do loop'
              endif
              k=jsub(i)
              count(ipos,j,k)  = count(ipos,j,k) + 1.0 
              penalty(ipos,j,k) = penalty(ipos,j,k) + pen

              do ii=1,2
                 tot_cor(ipos,j,k,ii) = tot_cor(ipos,j,k,ii) + cor_tot(ii)
                 omg_nbc(ipos,j,k,ii) = omg_nbc(ipos,j,k,ii) + nbc_omg(ii)
                 omg_bc(ipos,j,k,ii)  = omg_bc(ipos,j,k,ii)  + bc_omg(ii)
                 fixang_cor(ipos,j,k,ii)  = fixang_cor(ipos,j,k,ii)  + cor_fixang(ii)
                 lapse_cor(ipos,j,k,ii)   = lapse_cor(ipos,j,k,ii)   + cor_lapse(ii)
                 lapse2_cor(ipos,j,k,ii)  = lapse2_cor(ipos,j,k,ii)  + cor_lapse2(ii)
                 const_cor(ipos,j,k,ii)   = const_cor(ipos,j,k,ii)   + cor_const(ii)
                 scangl_cor(ipos,j,k,ii)  = scangl_cor(ipos,j,k,ii)  + cor_scangl(ii)
                 clw_cor(ipos,j,k,ii)     = clw_cor(ipos,j,k,ii)     + cor_clw(ii)
                 cos_cor(ipos,j,k,ii)     = cos_cor(ipos,j,k,ii)     + cor_cos(ii)
                 sin_cor(ipos,j,k,ii)     = sin_cor(ipos,j,k,ii)     + cor_sin(ii)
                 emiss_cor(ipos,j,k,ii)   = emiss_cor(ipos,j,k,ii)   + cor_emiss(ii)
                 ordang4_cor(ipos,j,k,ii) = ordang4_cor(ipos,j,k,ii) + cor_ordang4(ii)
                 ordang3_cor(ipos,j,k,ii) = ordang3_cor(ipos,j,k,ii) + cor_ordang3(ii)
                 ordang2_cor(ipos,j,k,ii) = ordang2_cor(ipos,j,k,ii) + cor_ordang2(ii)
                 ordang1_cor(ipos,j,k,ii) = ordang1_cor(ipos,j,k,ii) + cor_ordang1(ii)

              end do
           end do

        endif

     enddo ! channel loop

! End of loop over diagnostic file
  enddo loopd
  close(lndiag)
  write(6,*)' '
  write(6,*)'read in ',iread,' obs ',rread
  write(6,*)' '

  write(6,*)'nwater, nland, nice, nsnow, nmixed, ntotal = ', nwater, nland, nice, nsnow, nmixed, ntotal
  nntotal=nnwater+nnland+nnsnow+nnmixed
  write(6,*)'nnwater, nnland, nnsnow, nnmixed, nntotal = ', nnwater, nnland, nnsnow, nnmixed, nntotal


  ! Create Control file
  if ( imkctl == 1 ) then
     write(6,*)'call create_ctl_angle'
     if ( trim(gesanl) == 'ges' ) then
        dfile = trim(satname)
     else 
        dfile = trim(satname) // '_anl'
     endif

     call create_ctl_angle(ntype,ftype,n_chan,iyy,imm,idd,ihh,&
       ctl_file,lunctl,rmiss,dfile,satype,dplat,surf_nregion,&
       surf_region,surf_rlonmin,surf_rlonmax,surf_rlatmin,surf_rlatmax,&
       nu_chan,use, error, frequency,wavenumbr,nstep,start,step, little_endian)
  else
     write(6,*) 'imkctl =',imkctl
  endif


! Write output to data file
  if ( imkdata == 1 ) then
     write(6,*)' '
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((timang(i,j),i=1,nstep),j=1,n_chan)
     do k=1,surf_nregion
        write(lungrd) ((count(i,j,k),i=1,nstep),j=1,n_chan)
     end do
     do k=1,surf_nregion
        write(lungrd) ((penalty(i,j,k),i=1,nstep),j=1,n_chan)
     end do
     do ii=1,2
        do k=1,surf_nregion
           write(lungrd)((omg_nbc(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((tot_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((omg_bc(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((fixang_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((lapse_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((lapse2_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((const_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((scangl_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((clw_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((cos_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((sin_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((emiss_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((ordang4_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((ordang3_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((ordang2_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((ordang1_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  else
     write(6,*) 'imkctl =',imkctl
  endif


! Deallocate arrays
  write(6,*)' '
  write(6,*)'deallocate arrays'
  deallocate(penalty,io_chan,nu_chan,wavenumbr,error,use,frequency)
  deallocate(timang,tot_cor,omg_nbc,omg_bc,count)
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

  allocate(timang(mstep,n_chan))
  allocate(count(mstep,n_chan,surf_nregion),&
       penalty(mstep,n_chan,surf_nregion),&
       omg_nbc(mstep,n_chan,surf_nregion,2),&
       tot_cor(mstep,n_chan,surf_nregion,2),&
       omg_bc(mstep,n_chan,surf_nregion,2))

!
! Initialze all output variables to rmiss
!
!    
  do j=1,n_chan
     do i=1,mstep
        timang(i,j) = rmiss
     end do
  end do

  write(6,*)'load missing value ',rmiss,' into output arrays.  ',&
       nstep,surf_nregion,n_chan
  do ii=1,2
     do k=1,surf_nregion
        do j=1,n_chan
           do i=1,mstep
              if (ii==1) then
                 count(i,j,k)  = rmiss
                 penalty(i,j,k)= rmiss
              endif
              omg_nbc(i,j,k,ii) = rmiss
              tot_cor(i,j,k,ii) = rmiss
              omg_bc(i,j,k,ii)  = rmiss
              fixang_cor(i,j,k,ii)  = rmiss 
              lapse_cor(i,j,k,ii)  =  rmiss
              lapse2_cor(i,j,k,ii)  =  rmiss
              clw_cor(i,j,k,ii)  =  rmiss
              const_cor(i,j,k,ii)  =  rmiss
              scangl_cor(i,j,k,ii)  = rmiss
              cos_cor(i,j,k,ii)     = rmiss
              sin_cor(i,j,k,ii)     = rmiss
              emiss_cor(i,j,k,ii)   = rmiss
              ordang4_cor(i,j,k,ii) = rmiss
              ordang3_cor(i,j,k,ii) = rmiss
              ordang2_cor(i,j,k,ii) = rmiss
              ordang1_cor(i,j,k,ii) = rmiss
           end do
        end do
     end do
  end do

  if ( imkdata == 1 ) then
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) ((timang(i,j),i=1,nstep),j=1,n_chan)
     do k=1,surf_nregion
        write(lungrd) ((count(i,j,k),i=1,nstep),j=1,n_chan)
     end do
     do k=1,surf_nregion
        write(lungrd) ((penalty(i,j,k),i=1,nstep),j=1,n_chan)
     end do
     do ii=1,2
        do k=1,surf_nregion
           write(lungrd)((omg_nbc(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((tot_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((omg_bc(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((fixang_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((lapse_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((lapse2_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((const_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((scangl_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((clw_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((cos_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((sin_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((emiss_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((ordang4_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((ordang3_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((ordang2_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
        do k=1,surf_nregion
           write(lungrd)((ordang1_cor(i,j,k,ii),i=1,nstep),j=1,n_chan)
        end do
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif
  deallocate(timang,count,penalty,omg_nbc,omg_bc,tot_cor,&
             fixang_cor,lapse_cor,lapse2_cor,const_cor,scangl_cor,clw_cor)
  deallocate(cos_cor,sin_cor,emiss_cor,ordang1_cor,ordang2_cor,ordang3_cor,ordang4_cor)

! End of program
950 continue
  stop
end program angle
