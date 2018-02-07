program horiz
  use read_diag

  implicit none
  integer ntype, mls2_levs,mls3_levs
  parameter (ntype=4)
  parameter (mls2_levs=37)
  parameter (mls3_levs=55)

  logical first

  character(6),dimension(ntype):: ftype
  character(8) stid
  character(20) satname,stringd,satsis
  character(10) dum,satype,dplat
  character(40) string,diag_oz,grad_file,ctl_file

  integer luname,lungrd,lunctl,lndiag,isave
  integer iyy,imm,idd,ihh,idhh,incr,iread,irite,iflag
  integer n_levs,j,nlev,nflag,i,nlevs,nobs,iobs,m_levs,k,lev_nobs
  integer,allocatable,dimension(:):: iuse

  real weight,rlat,rlon,rtim,rmiss,obs,ges,obsges,sza,fovn,toqf
  real,allocatable,dimension(:):: error,dlat,dlon
  real,allocatable,dimension(:):: prs_nlev
  real,allocatable,dimension(:,:):: var,var1

  integer :: klev
  real,allocatable,dimension(:)::  maxval,minobs
  integer,allocatable,dimension(:):: jmax,jmin

  real :: tmp_ozmp
  integer :: nobs_mls_oncpu
  real,allocatable,dimension(:,:) :: ozmp,ozmr

! Variables for reading satellite data
  type(diag_header_fix_list )         :: header_fix
  type(diag_header_nlev_list),pointer :: header_nlev(:)
  type(diag_data_fix_list   ),pointer :: data_fix(:)
  type(diag_data_nlev_list  ),pointer :: data_nlev(:,:)
  type(diag_data_extra_list) ,pointer :: data_extra(:,:)


  namelist /input/ satname,iyy,imm,idd,ihh,idhh,incr

  data luname,lungrd,lunctl,lndiag / 5, 100, 51, 21 /
  data rmiss /-999./
  data ftype / 'obs', 'ges', 'obsges', 'ozmp' /
  data first / .true. /
  data stringd / '.%y4%m2%d2%h2' /
  


!************************************************************************
!
! Initialize variables
  nobs=0; irite=0
  rtim=0.0; nlev=1; nflag=1

! Read namelist input
  read(luname,input)
  write(6,input)
  write(6,*)' '

!   Create filenames for diagnostic input, GrADS output, and
!   GrADS control files    

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
  read(lndiag,err=900,end=900) dum
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
     call errexit(91)
  endif


! Allocate arrays to hold observational information
  write(6,*)'allocate arrays'
  allocate ( prs_nlev(n_levs))
  allocate (var(n_levs,ntype), iuse(n_levs),  &
       error(n_levs))
  allocate(maxval(n_levs))
  allocate(minobs(n_levs))
  allocate(jmax(n_levs))
  allocate(jmin(n_levs))

! Extract ozinfo relative index
  do j=1,n_levs
     error(j)     = real( header_nlev(j)%err, 4)
     prs_nlev(j)  = real( header_nlev(j)%pob, 4)
     iuse(j)      = real( header_nlev(j)%iouse, 4 )
  end do


! Create GrADS control file
  write(6,*)'call create_ctl_horiz'
  call create_ctl_horiz(ntype,ftype,n_levs,iyy,imm,idd,ihh,idhh,incr,&
       ctl_file,lunctl,rmiss,satname,prs_nlev,&
       error,iuse,satype,dplat)

! Loop to read entries in diagnostic file
  iflag = 0
  m_levs=n_levs
  if(index(satype,'mls')/=0 ) then
     print*, 'deal with MLS data, reset n_levs=1'
     m_levs=1
  end if

  maxval=0.0
  minobs=500.0
  if(index(satype,'mls')/=0 ) then
    lev_nobs=0
  end if

  loopd:  do while (iflag == 0)

!    Read a record.  If read flag, iflag does not equal zero, exit loopd
     call read_diag_data( lndiag, header_fix, data_fix, data_nlev, data_extra, iread, iflag )
     if( iflag /= 0 ) exit loopd
     nobs=nobs+iread

     if(index(satype,'mls')/=0 ) then
        print*, 'nobs, iread for case MLS data= ', nobs, iread
        allocate(dlat(iread))
        allocate(dlon(iread))
        allocate(var1(iread,ntype))
     end if

!    Extract obervation location

   do iobs=1,iread

     rlat   = data_fix(iobs)%lat
     rlon   = data_fix(iobs)%lon
     if(index(satype,'mls')/=0) then
        dlat(iobs)=rlat
        dlon(iobs)=rlon
     end if
     
!    Level loop
     isave=0
     do j = 1, m_levs

!       If observation was assimilated, save it
!        if (data_nlev(j,iobs)%varinv > 1.e-6 ) then
           isave = 1
           obs   = data_nlev(j,iobs)%ozobs
           ges   = data_nlev(j,iobs)%ozobs - data_nlev(j,iobs)%ozone_inv
           obsges = data_nlev(j,iobs)%ozone_inv
           tmp_ozmp = data_nlev(j,iobs)%toqf
!          MLS v2 obs between levels 8 and 23 should be good
           if(index(satype,'mls')/=0 .and. j<24 .and. j>7 .and. (obs<1.0e-8 .or. ges<1.0e-8) ) print*, 'obs,ges,omg=',obs,ges,obsges, m_levs,rlat,rlon,iobs
!         Set data values to missing flag
!        else
!           obs       = rmiss
!           ges       = rmiss
!           obsges    = rmiss
!           tmp_ozmp  = rmiss
!        endif

!       Load into output array
        var(j,1) = obs
        var(j,2) = ges
        var(j,3) = obsges
        var(j,4) = tmp_ozmp
        if(index(satype,'mls')/=0) then
          do i=1,ntype
             var1(iobs,i)=var(1,i)  !MLS is set to 1 level
          end do
!         MLS obs between levels 8 and 23 should be good
          klev=mod(iobs,n_levs)
          if(klev==0) klev=n_levs
          if( klev<24 .and. klev>7 ) then
            if( (var1(iobs,1)<=0. .or. var1(iobs,1)>100.0) .and. (var1(iobs,1) /= rmiss) ) then   !if obs<0. or obs>100
              print*, 'iobs= ', iobs, ' obs is unreasonable', klev, iobs, var(1,1), var1(iobs,1),var1(iobs,3)
!             var1(iobs,1)=rmiss
!             var1(iobs,2)=rmiss
!             var1(iobs,3)=rmiss
            end if
            if( (var1(iobs,2)<=0. .or. var1(iobs,2)>100.0) .and. (var1(iobs,1) /= rmiss) ) then  !if ges<0. or ges>100
              print*, 'iobs= ', iobs, ' ges is unreasonable', klev, iobs, var(1,2), var1(iobs,2),var1(iobs,3)
!             var1(iobs,1)=rmiss
!             var1(iobs,2)=rmiss
!             var1(iobs,3)=rmiss
            end if
          end if
        end if
     enddo ! level loop
  
!    Write GrADS record
     if (isave==1) then
        if (first) then
           first=.false.
           open(lungrd,file=grad_file,form='unformatted')
        endif
        irite=irite+1
        if(index(satype,'mls')==0 ) then  !non-MLS case
          write(stid,'(i8)') irite
          write(lungrd) stid,rlat,rlon,rtim,nlev,nflag
          write(lungrd) ((var(j,i),j=1,m_levs),i=1,ntype)
        end if
     end if

   enddo ! do iobs=1,iread

   if(index(satype,'mls')/=0 ) then
     allocate(ozmp(n_levs,1000))
     allocate(ozmr(n_levs,1000))
     ozmp=rmiss
     ozmr=rmiss
     nobs_mls_oncpu=0
     print*, 'total # of MLS obs is: ', iobs-1
     do i=1,iread,n_levs
       lev_nobs=lev_nobs+1   !lev_nobs represents the profile ID
       nobs_mls_oncpu=nobs_mls_oncpu+1
       write(stid,'(i8)') lev_nobs
       write(lungrd) stid,dlat(i),dlon(i),rtim,nlev,nflag
       write(lungrd) ((var1(k,j),k=i,i+n_levs-1),j=1,ntype)
       do k=i,i+n_levs-1
          klev=mod(k,n_levs)
          if(klev==0) klev=n_levs
          if(var1(k,4)>0.) then
            ozmp(klev,nobs_mls_oncpu)=var1(k,4)
            ozmr(klev,nobs_mls_oncpu)=var1(k,1)
          end if
          if(var1(k,1)>maxval(klev) .and. var1(k,1)/=rmiss ) then
              maxval(klev)=var1(k,1)
              jmax(klev)=lev_nobs
           end if
          if(var1(k,1)<minobs(klev) .and. var1(k,1)/=rmiss ) then
              minobs(klev)=var1(k,1)
              jmin(klev)=lev_nobs
           end if
       end do
!      write(60,*) stid,dlat(i),dlon(i),rtim,nlev,nflag
!      write(60,*) ((var1(k,j),k=i,i+n_levs-1),j=1,ntype)
     end do
     deallocate(dlat)
     deallocate(dlon)
     deallocate(var1)

     open(10,file='ozmp.dat',form='unformatted')
     do k=1,n_levs
        write(10) (ozmr(k,i),i=1,nobs_mls_oncpu)
     end do
     do k=1,n_levs
        write(10) (ozmp(k,i),i=1,nobs_mls_oncpu)
     end do
     close(10)
     write(20,*) nobs_mls_oncpu,header_fix%iint
     do i=1,nobs_mls_oncpu
        write(30,*) i,ozmp(:,i)
     end do
     deallocate(ozmp)
     deallocate(ozmr)
   end if

!   End of loop over diagnostic file
  enddo loopd

  write(6,*)'read in ',nobs,' obs & write out ',irite,' obs'

! Deallocate arrays
  deallocate(var,iuse,error)
  goto 950


!   Jump to here if eof or error reading diagnostic file.
900 continue
  write(6,*)'***PROBLEM reading diagnostic file.  diag_oz=',diag_oz

  if (m_levs<=0) then
     write(6,*)'***ERROR*** invalid nlevs=',n_levs,'  STOP program'
     call errexit(93)
  endif
     
  write(6,*)'update date for control file'
  call update_ctl_horiz(n_levs,iyy,imm,idd,ihh,idhh,incr,&
       ctl_file,lunctl)

  write(6,*)'load missing value ',rmiss,' into output arrays.  ',&
       n_levs,ntype
  allocate(var(n_levs,ntype))
  do j=1,ntype
     do i=1,n_levs
        var(i,j)=rmiss
     end do
  end do
  open(lungrd,file=grad_file,form='unformatted')
  stid='missing'
  rlat=0.0
  rlon=0.0
  write(lungrd) stid,rlat,rlon,rtim,nlev,nflag
  write(lungrd) ((var(i,j),i=1,n_levs),j=1,ntype)
  irite=1
  write(6,*)'write output to lungrd=',lungrd,', file=',trim(grad_file)
  deallocate(var)


!   Close unit to diagnostic file
950 continue
  close(lndiag)


!   If data was written to GrADS file, write terminator and close file

  if (irite>0) then
     stid ='ozone'
     rlat =0.0
     rlon =0.0
     rtim =0.0
     nlev =0
     nflag=0
     write(lungrd) stid,rlat,rlon,rtim,nlev,nflag
     close(lungrd)
  endif

  if(index(satype,'mls')/=0 ) then
     do i=1,n_levs
        print*, 'max,min=',i,maxval(i),minobs(i),jmax(i),jmin(i)
     end do
  end if

! End of program
  stop
end program horiz
