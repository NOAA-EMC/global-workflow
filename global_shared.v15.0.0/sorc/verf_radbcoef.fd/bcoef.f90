program bcoef
  use read_diag
  use kinds, only: r_kind,i_kind,r_quad

  implicit none
  integer ntype,maxpred
  parameter (ntype=13)
  parameter (maxpred=12)

  logical eof

  character(10),dimension(ntype):: ftype
  character(20) dum,satname,stringd,satsis,isis,mod_satname
  character(10) satype,dplat
  character(40) string,diag_rad,data_file,ctl_file
  character(10) suffix

  integer luname,lungrd,lunctl,lncoef,lndiag,ich
  integer iyy,imm,idd,ihh,idhh,incr,iread,iflag
  integer n_chan,j,i,k,idum,ichan
  integer,allocatable,dimension(:):: io_chan,nu_chan
  integer npred_radiag
  integer(i_kind) angord,ntlapupdate 

  real pen,rmiss,weight,rread
  real,allocatable,dimension(:):: wavenumbr,count,error,&
       use,frequency,penalty,predr
  real,allocatable,dimension(:,:):: coefs
  real(r_kind)  tlapm, tsum

! Variables for reading satellite data
  type(diag_header_fix_list )             :: header_fix
  type(diag_header_chan_list),allocatable :: header_chan(:)
  type(diag_data_name_list  )             :: data_name
  type(diag_data_fix_list   )             :: data_fix
  type(diag_data_chan_list  ),allocatable :: data_chan(:)
  type(diag_data_extra_list ),allocatable :: data_extra(:,:)

! Namelist with defaults
  integer               :: npredr               = 12
  logical               :: retrieval            = .false.
  integer               :: nchanl               = 19
  integer               :: imkctl               = 1
  integer               :: imkdata              = 1
  character(3)          :: gesanl               ='ges'
  integer               :: little_endian        = 1
  namelist /input/ satname,npredr,nchanl,iyy,imm,idd,ihh,idhh,&
       incr,suffix,imkctl,imkdata,retrieval,gesanl,little_endian

  data luname,lungrd,lunctl / 5, 51, 52 /
  data lncoef,lndiag /  21, 22 /
  data rmiss /-999./
  data stringd / '.%y4%m2%d2%h2' /
!************************************************************************
!  Explanation of the fype array
!     These are values used within the generated GrADS control file
!     (bcoef.ctl).  A fuller discription of the terms is thus:
!
!        mean       pred(1) = global offset (mean)
!        atmpath    pred(2) = not used when adp_anglebc=.true. and newpc4pred=.true.
!        clw        pred(3) = cloud liquid water term
!        lapse2     pred(4) = (temperature lapse rate)**2
!        lapse      pred(5) = temperature lapse rate
!        cos_ssmis  pred(6) = cosine term for SSMIS
!        sin_ssmis  pred(7) = sine term for SSMIS
!        emiss      pred(8) = emissivity sensitivity term
!        ordang4    pred(9) = 4th order angle term
!        ordang3    pred(10) = 3rd order angle term
!        ordang2    pred(11) = 2nd order angle term
!        ordang1    pred(12) = 1st order angle term
!
!     Note that the npred namelist indicates how many of these terms
!     will to be processed by this program.  The default value is 12, 
!     meaning all will be processed.  Earlier versions processed only the
!     first 5.  In all cases 12 values will be written to the resulting
!     data file though if npred < 12 then the unprocessed values will
!     all be set to rmiss.
!************************************************************************
  data ftype / 'penalty','mean','atmpath','clw','lapse2','lapse',&
               'cos_ssmis','sin_ssmis','emiss','ordang4','ordang3',&
               'ordang2','ordang1' /
!************************************************************************
!


!************************************************************************
!
! Initialize variables
  iread=0
  npred_radiag = 12 
  angord = 0

! Read namelist input
  read(luname,input)
  write(6,input)
  write(6,*)'gesanl = ', gesanl
  write(6,*)' '

! Check for user requests exceeding assumed limits
  if (npredr>maxpred) then
     write(6,*)' '
     write(6,*)'***ERROR*** user specified predictors > maximum allowed'
     write(6,*)'   npredr,maxpred=',npredr,maxpred
     call errexit(91)
  endif


! Set satellite id for specified satellite/sensor
  write(6,*)'satname ',satname


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
  
  if (angord == 4 ) then
    npredr = 12
  endif
 
  write(6,*)'satype,n_chan=',satype,' ',dplat,n_chan
  write(6,*)'angord = ', angord

  string = trim(satype)//'_'//trim(dplat)
  write(6,*)'string,satname=',string,' ',satname
  if ( trim(string) /= trim(satname) ) then
     write(6,*)'***ERROR*** inconsistent instrument types'
     write(6,*)'  satname,string  =',satname,' ',string
     call errexit(93)
  endif


! Allocate arrays to hold observational information
  write(6,*)' '
  write(6,*)'allocate arrays'
  allocate (io_chan(n_chan), nu_chan(n_chan), wavenumbr(n_chan))
  allocate (count(n_chan), penalty(n_chan), use(n_chan), &
       frequency(n_chan))
  allocate(coefs(n_chan,maxpred))
  
! Zero accumulator arrays
  do j=1,n_chan
     count(j) = 0.0
     penalty(j) = 0.0
  end do
  coefs = rmiss


! Extract satinfo relative index
  do j=1,n_chan
     nu_chan(j)   = real( header_chan(j)%nuchan, 4 )
     io_chan(j)   = real( header_chan(j)%iochan, 4 )
     wavenumbr(j) = real( header_chan(j)%wave, 4 )
     use(j)       = real( header_chan(j)%iuse, 4 )
     frequency(j) = real( header_chan(j)%freq, 4)
  end do
        
! Loop to read entries in diagnostic file
  iflag = 0
  loopd:  do while (iflag == 0)

!    Read a record.  If read flag, iflag does not equal zero, exit loopd
     call read_radiag_data( lndiag, header_fix, retrieval, data_fix, data_chan,&
           data_extra, iflag )
     if( iflag /= 0 ) exit loopd
     iread=iread+1
     rread=rread+1.0


!    Channel loop
     do j = 1, n_chan

!       If observation was assimilated, accumulate sum
        if (data_chan(j)%errinv > 1.e-6) then
!           pen        =  data_chan(j)%errinv*(data_chan(j)%omgbc)**2
           pen        =  (data_chan(j)%errinv*(data_chan(j)%omgbc))**2
           count(j)   = count(j) + 1.0 
           penalty(j) = penalty(j) + pen
        endif

     enddo ! channel loop

! End of loop over diagnostic file
  enddo loopd
  close(lndiag)
  write(6,*)' '
  write(6,*)'read in ',iread,' obs ',rread
  write(6,*)' '

! Compute average.
  do j=1,n_chan
     if (count(j)>0) then
        penalty(j)=penalty(j)/count(j)
     else
        count(j)=rmiss
        penalty(j)=rmiss
     endif
  end do


! Open unit to input data file.  See if file exists
  open(lncoef,file='biascr.txt',form='formatted')
  if (angord == 4 ) then
     read(lncoef,122,end=920,err=920) idum
  else
     read(lncoef,120,end=920,err=920) idum
  endif
  rewind(lncoef)
120 format(I5,1x,A20,1x,I5,10f12.6)
122 format(I5,1x,A20,1x,I5,2e15.6,1x,I5/2(4x,10f12.6/))

!read(lunin,'(I5,1x,A20,1x,I5,2e15.6,1x,I5/2(4x,10f12.6/))',iostat=istat,end=1333)
!ich,isis,& ichan,tlapm,tsum,ntlapupdate,(predr(ip),ip=1,npred), where npred=12

! Read coefficient file
  allocate(predr(npredr))
  i=0
  k=1
  eof=.false.
  do  
     if (angord /= 4 ) then
        read(lncoef,120,IOSTAT=iflag) ich,isis,ichan,(predr(j),j=1,npredr)
     else
        read(lncoef,122,IOSTAT=iflag) ich,isis,ichan, tlapm,tsum,ntlapupdate,(predr(j),j=1,npredr)
     endif
     if(iflag /=0) exit
     if (trim(isis)==trim(satsis)) then

        if ( k <= n_chan ) then
           io_chan(k)=ichan
           do j=1,npredr
              coefs(k,j)=predr(j)
           end do
        endif

        k=k+1
        cycle 
     endif
  end do
  close(lncoef)
  deallocate(predr)

  ! Create Control file
  if ( imkctl == 1 ) then
     write(6,*)'call create_ctl_bcoef'

     if ( trim(gesanl) == 'ges' ) then
        mod_satname = trim(satname)
     else
        mod_satname = trim(satname) // '_anl'
     endif
   
     call create_ctl_bcoef(ntype,ftype,n_chan,iyy,imm,idd,ihh,idhh,&
          incr,ctl_file,lunctl,rmiss,mod_satname,satype,dplat,&
          nu_chan,use,penalty,frequency,wavenumbr,little_endian)
  else
     write(6,*) 'imkctl =',imkctl
  endif


! Write data to binary output file
  if ( imkdata == 1 ) then
     write(6,*)'write data to lungrd=',lungrd
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) (penalty(k),k=1,n_chan)
     do j=1,maxpred
        write(lungrd) (coefs(k,j),k=1,n_chan)
     end do
     close(lungrd)
  endif

! Deallocate arrays
  write(6,*)'deallocate arrays'
  deallocate(coefs,io_chan,nu_chan,wavenumbr,count,penalty,use,frequency)
  goto 950


! Jump here if problem reading diagnostic file
900 continue
  write(6,*)'***PROBLEM*** reading diagnostic file'

  n_chan=nchanl
  if (n_chan<=0) then
     write(6,*)'***ERROR*** invalid nchanl=',nchanl,'  STOP program'
     call errexit(94)
  endif

  write(6,*)'load missing value ',rmiss,' into output arrays.  ',&
       n_chan,npredr
  allocate(penalty(n_chan),coefs(n_chan,npredr))
  do k=1,n_chan
     penalty(k)=rmiss
  end do
  do j=1,npredr
     do k=1,n_chan
        coefs(k,j)=rmiss
     end do
  end do

  if ( imkdata == 1 ) then
     open(lungrd,file=data_file,form='unformatted')
     write(lungrd) (penalty(k),k=1,n_chan)
     do j=1,npredr
        write(lungrd) (coefs(k,j),k=1,n_chan)
     end do
     write(6,*)'write output to lungrd=',lungrd,', file=',trim(data_file)
     close(lungrd)
  endif
  deallocate(penalty,coefs)

  goto 950


! Jump here if problem reading coefficient file
920 continue
  write(6,*)'***PROBLEM*** reading coefficient file'


! End of program
950 continue
  stop
end program bcoef
