program main
  use read_diag

  implicit none
  integer ntype,mregion,mls2_levs,mls3_levs
  parameter (ntype=4,mregion=25,mls2_levs=37,mls3_levs=55)

  character(10),dimension(ntype):: ftype
  character(20) satname,stringd,satsis
  character(10) dum,satype,dplat
  character(40) string,diag_oz,grad_file,ctl_file
  character(40),dimension(mregion):: region

  integer luname,lungrd,lunctl,lndiag,nregion
  integer iyy,imm,idd,ihh,idhh,incr,iflag
  integer n_levs,j,idsat,i,k,ii,nreg,nlevs,iobs,iread,nobs
  integer,dimension(mregion):: jsub
  real,allocatable,dimension(:):: prs_nlev

  real pen
  real weight,rlat,rlon,rmiss,obs,biascor,obsges,obsgesnbc,rterm
  real,dimension(2):: cor_omg
  real,dimension(mregion):: rlatmin,rlatmax,rlonmin,rlonmax

  real,allocatable,dimension(:,:):: count,error,use,penalty
  real,allocatable,dimension(:,:,:):: omg_cor


! Variables for reading ozone data
  type(diag_header_fix_list )         :: header_fix
  type(diag_header_nlev_list),pointer :: header_nlev(:)
  type(diag_data_fix_list   ),pointer :: data_fix(:)
  type(diag_data_nlev_list  ),pointer :: data_nlev(:,:)
  type(diag_data_extra_list) ,pointer :: data_extra(:,:)


  namelist /input/ satname,iyy,imm,idd,ihh,idhh,incr,&
       nregion,region,rlonmin,rlonmax,rlatmin,rlatmax

  data luname,lungrd,lunctl,lndiag / 5, 100, 51, 21 /
  data rmiss /-999./
  data stringd / '.%y4%m2%d2%h2' /
  data ftype / 'count', 'cpen', 'avgomg', 'sdvomg' /



!************************************************************************
!
! Initialize variables
  nobs=0
  region=' '
  rlatmin=0.; rlatmax=0.; rlonmin=0.; rlonmax=0.

! Read namelist input
  read(luname,input)
  write(6,input)
  write(6,*)' '

! Ensure number of requested regions does not exceed specified upper limit
  if (nregion>mregion) then
     write(6,*)'***ERROR*** too many regions specified'
     write(6,*)'   maximum allowed:  mregion=',mregion
     write(6,*)'    user requested:  nregion=',nregion
     call errexit(91)
  endif


! Create filenames for diagnostic input, GrADS output, and GrADS control files    
  write(stringd,100) iyy,imm,idd,ihh
100 format('.',i4.4,3i2.2)
  diag_oz = trim(satname)
  grad_file= trim(satname) // trim(stringd) // '.ieee_d'
  ctl_file = trim(satname) // '.ctl'

  write(6,*)'diag_oz =',diag_oz
  write(6,*)'grad_file=',grad_file
  write(6,*)'ctl_file =',ctl_file


! Open unit to diagnostic file.  Read portion of header to 
! see if file exists
  open(lndiag,file=diag_oz,form='unformatted')
! open(lndiag,file='omi_aura.txt',status='old')
  read(lndiag,err=900,end=900) dum
  print*, 'dum=', dum
  rewind lndiag

! File exists.  Read header
  write(6,*)'call read_diag_header'
  call read_diag_header( lndiag, header_fix, header_nlev )

! Extract observation type, satellite id, and number of levels
  satype = header_fix%obstype
  satsis = header_fix%isis
  dplat  = header_fix%id
  n_levs = header_fix%nlevs

  if(index(satype,'mls2')/=0 ) then
    n_levs = mls2_levs
  end if
  if(index(satype,'mls3')/=0 ) then
    n_levs = mls3_levs
  end if

  write(6,*)'satype,dplat,n_levs=',satype,' ',dplat,n_levs

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
  allocate ( prs_nlev(n_levs))
  allocate (omg_cor(n_levs,mregion,2), &
       count(n_levs,mregion), & 
       penalty(n_levs,mregion), &
       error(n_levs,mregion), use(n_levs,mregion))

! Zero accumulator arrays
  do ii=1,2
     do k=1,mregion
        do j=1,n_levs
           if (ii==1) then
              count(j,k) = 0.0
              penalty(j,k) = 0.0
           endif
           omg_cor(j,k,ii) = 0.0
        end do
     end do
  end do

! Extract ozinfo relative index
  do j=1,n_levs
     prs_nlev(j)       = real( header_nlev(j)%pob, 4)
  end do
  do k=1,mregion
     do j=1,n_levs
        error(j,k)     = real( header_nlev(j)%err, 4)
        use(j,k)       = real( header_nlev(j)%iouse, 4 )
     end do
  end do
        

! Create GrADS control file
  write(6,*)'call create_ctl_oz'
  call create_ctl_oz(ntype,ftype,n_levs,iyy,imm,idd,ihh,idhh,&
       incr,ctl_file,lunctl,rmiss,satname,satype,dplat,nregion,&
       region,rlonmin,rlonmax,rlatmin,rlatmax,prs_nlev,use(1,1),error(1,1))


! Loop to read entries in diagnostic file
  iflag = 0
  if(index(satype,'mls')/=0 ) then
     print*, 'deal with MLS data'
  end if

  loopd:  do while (iflag == 0)

!    Read a record.  If read flag, iflag does not equal zero, exit loopd
     call read_diag_data( lndiag, header_fix, data_fix, data_nlev, data_extra, iread, iflag )
     if( iflag /= 0 ) exit loopd
     nobs=nobs+iread

!    Extract obervation location and mpi weight.  Convert (0-360) lon to (-180,180)

   do iobs=1,iread
     rlat   = data_fix(iobs)%lat
     rlon   = data_fix(iobs)%lon
     if (rlon>180.) rlon = rlon - 360.
!    print*,'rlat,rlon=',rlat,rlon

!    Detemine subdomain based on observation location
     ii=0; jsub=0
     do k=1,nregion
        if ( (rlonmin(k)<=rlon .and. rlon<rlonmax(k)) .and. &
             (rlatmin(k)<=rlat .and. rlat<rlatmax(k)) ) then
           ii=ii+1
           jsub(ii)=k
        endif
     end do
     nreg=ii

     if(index(satype,'mls')==0 ) then
!       Level loop
        do j = 1, n_levs
!          If observation was assimilated, accumulate sums in appropriate regions
!          if (data_nlev(j,iobs)%varinv > 1.e-6) then
              pen           =  data_nlev(j,iobs)%varinv*(data_nlev(j,iobs)%ozone_inv)**2
              cor_omg(1)  =  data_nlev(j,iobs)%ozone_inv
              cor_omg(2)  =  (cor_omg(1))**2
              do i=1,nreg
                 k=jsub(i)
                 count(j,k) = count(j,k) +1.0 
                 penalty(j,k) = penalty(j,k) + pen
                 do ii=1,2
                    omg_cor(j,k,ii)  = omg_cor(j,k,ii)  + cor_omg(ii)
                 end do
              end do
!          endif
        enddo ! level loop
     else
!       If observation was assimilated, accumulate sums in appropriate regions
        if (data_nlev(1,iobs)%varinv > 1.e-6) then
!       since the old gsi executable contains the case where the obs is above model top and ratio_error is set to 0 
!       but varinv is not set to 0 and the ozone_inv is still calculated.
!       if (abs(data_nlev(1,iobs)%ozone_inv) < 1.0e+02 .and. data_nlev(1,iobs)%varinv<1.0e+04) then  
!       if (abs(data_nlev(1,iobs)%ozone_inv) < 1.0e+02 ) then  
           pen           =  data_nlev(1,iobs)%varinv*(data_nlev(1,iobs)%ozone_inv)**2
           cor_omg(1)  =  data_nlev(1,iobs)%ozone_inv
           cor_omg(2)  =  (cor_omg(1))**2
           j=mod(iobs,n_levs)
           if(j==0) j=n_levs
           do i=1,nreg
              k=jsub(i)
              count(j,k) = count(j,k) +1.0 
              penalty(j,k) = penalty(j,k) + pen
              do ii=1,2
                 omg_cor(j,k,ii)  = omg_cor(j,k,ii)  + cor_omg(ii)
              end do
           end do
        endif
     endif

   enddo   ! END do iobs=1,iread

!  End of loop over diagnostic file
  enddo loopd

  close(lndiag)
  print*, 'read in ', nobs, ' observations in total',count(12,1),count(12,4),sum(omg_cor),sum(penalty)
  write(6,*)' '
  write(6,*)' '

! Compute average and standard deviation
  do k=1,nregion
     do j=1,n_levs
        call avgsdv(count(j,k),omg_cor(j,k,1), omg_cor(j,k,2), rmiss)
          write(6,*)'level j=',j,', region k=',k,' with count,avg,sdv=', &
               count(j,k),omg_cor(j,k,1),omg_cor(j,k,2)
        if (count(j,k)>0) then
           penalty(j,k)=penalty(j,k)/count(j,k) ! convert penalty to cpen
        else
           count(j,k)=rmiss
           penalty(j,k)=rmiss
        endif
     end do
  end do

! Write output to GrADS ready file
  write(6,*)' '
  open(lungrd,file=grad_file,form='unformatted')
  write(lungrd) ((count(j,k),j=1,n_levs),k=1,nregion)
  write(lungrd) ((penalty(j,k),j=1,n_levs),k=1,nregion)
if(index(satype,'mls')/=0 ) then
  print*,  ' write out the data to temp.txt'
  open(8,file='temp.txt',form='formatted')
  write(8,*) ((count(j,k),j=1,n_levs),k=1,nregion)
  write(8,*) ((penalty(j,k),j=1,n_levs),k=1,nregion)
end if
  do ii=1,2
     write(lungrd) ((omg_cor (j,k,ii),j=1,n_levs),k=1,nregion)
     if(index(satype,'mls')/=0 ) then
          write(8,*) 'ii=',ii, ((omg_cor (j,k,ii),j=1,n_levs),k=1,nregion)
     end if
  end do
  write(6,*)'write output to lungrd=',lungrd,', file=',trim(grad_file)
  close(lungrd)
if(index(satype,'mls')/=0 ) then
  close(8)
end if


! Deallocate arrays
  write(6,*)' '
  write(6,*)'deallocate arrays'
  deallocate(prs_nlev,omg_cor,count,penalty,error,use)
  goto 950

! Jump to here if eof or error reading diagnostic file.
900 continue
  write(6,*)'***PROBLEM reading diagnostic file.  diag_oz=',diag_oz
  close(lndiag)

  if (n_levs<=0) then
     write(6,*)'***ERROR*** invalid nlevs=',n_levs,'  STOP program'
     call errexit(93)
  endif
     
  write(6,*)'update date for control file'
  call update_ctl_oz(n_levs,iyy,imm,idd,ihh,idhh,incr,&
       ctl_file,lunctl)

  write(6,*)'load missing value ',rmiss,' into output arrays.  ',&
       nregion,n_levs
  allocate(count(n_levs,nregion),penalty(n_levs,nregion))
  allocate(omg_cor(n_levs,mregion,2))

  write(6,*)'load missing value ',rmiss,' into output arrays'
  do ii=1,2
     do k=1,nregion
        do j=1,n_levs
           if (ii==1) then
              count(j,k)  =rmiss
              penalty(j,k)=rmiss
           endif
           omg_cor(j,k,ii) =rmiss
        end do
     end do
  end do
  open(lungrd,file=grad_file,form='unformatted')
  write(lungrd) ((count(j,k),j=1,n_levs),k=1,nregion)
  write(lungrd) ((penalty(j,k),j=1,n_levs),k=1,nregion)
if(index(satype,'mls')/=0 ) then
  open(8,file='temp.txt',form='formatted',status='new')
  write(8,*) ((count(j,k),j=1,n_levs),k=1,nregion)
  write(8,*) ((penalty(j,k),j=1,n_levs),k=1,nregion)
end if
  do ii=1,2
     write(lungrd) ((omg_cor (j,k,ii),j=1,n_levs),k=1,nregion)
if(index(satype,'mls')/=0 ) then
     write(8,*) 'ii=',ii,((omg_cor (j,k,ii),j=1,n_levs),k=1,nregion)
end if
  end do
  write(6,*)'write output to lungrd=',lungrd,', file=',trim(grad_file)
  close(lungrd)
if(index(satype,'mls')/=0 ) then
  close(8)
end if
  deallocate(count,penalty,omg_cor)

! End of program
950 continue
  stop
end program main
