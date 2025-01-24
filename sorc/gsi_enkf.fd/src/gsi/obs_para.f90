subroutine obs_para(ndata,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    obs_para    assign and distribute observations to subdomains
!   prgmmr: weiyu yang       org: np20                date: 1998-05-27
!
! abstract: Based on observation location and domain decomposition, assign
!           and distribute observations to subdomains
!
! program history log:
!   1998-05-27  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-15  treadon, reformat documenation
!   2004-07-23  derber - modify to include conventional sst
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-11-19  derber - modify to eliminate file and change to use logical 
!                        rather than weights
!   2005-06-14  wu      - add OMI total ozone
!   2005-09-08  derber - simplify data set handling
!   2005-10-17  treadon - fix bug in defnition of mype_diaghdr
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-07-28  derber  - eliminate counting of data and clean up
!   2008-04-29  safford - rm unused vars and uses
!   2008-06-30  derber  - optimize calculation of criterion
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-04-21  derber  - reformulate to remove communication
!   2008-05-10  meunier - handle for lagrangian data
!   2013-01-26  parrish - attempt fix for bug flagged by WCOSS debug compiler.
!                            Replace 
!                              "call dislag(.....,nobs_s)"
!                            with
!                              "call dislag(.....,nobs_s(mm1))"
!                           nobs_s is an array in current subroutine, but is a
!                           scalar inside subroutine dislag.
!   2014-10-03  carley  - add creation mpi subcommunicator needed for 
!                          buddy check QC to distinguish among pe subdomains 
!                          with and without obs (only for t obs and twodvar_regional 
!                          at the moment) 
!   2015-10-28  guo     - added ioid_s(:) to support split-observer GSI with a
!                         control vector grid different from the guess state
!                         grid.
!
!   input argument list:
!     ndata(*,1)- number of profiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
!     mype     - mpi task number
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: i_kind
  use constants, only: zero
  use jfunc, only: factqmin,factqmax
  use mpimod, only: npe,mpi_itype,mpi_comm_world,ierror
  use obsmod, only: obs_setup,dtype,mype_diaghdr,ndat,nsat1
  use obsmod, only: obsfile_all,dplat,nobs_sub,obs_sub_comm 
  use gridmod, only: twodvar_regional 
  use qcmod, only: buddycheck_t,buddydiag_save 
  use gsi_io, only: verbose, print_obs_para
  implicit none

! Declare passed variables
  integer(i_kind)                  ,intent(in   ) :: mype
  integer(i_kind),dimension(ndat,3),intent(in   ) :: ndata

! Declare local variables
  integer(i_kind) lunout,is,ii
  integer(i_kind) mm1
  integer(i_kind) ndatax_all,ikey_yes,ikey_no,newprocs,newrank 
  integer(i_kind),dimension(npe):: ikey,icolor 
  integer(i_kind) nobs_s
  logical print_verbose

  print_verbose=.false.
  if(verbose)print_verbose=.true.
!
!****************************************************************
! Begin obs_para here
!
! Distribute observations as a function of pe number.
  nsat1=0
  obs_sub_comm=0 
  mype_diaghdr = -999
  mm1=mype+1
  ndatax_all=0
  lunout=55

!
! Set number of obs on each pe for each data type
  open(lunout,file=obs_setup,form='unformatted')
  rewind lunout
  do is=1,ndat

     if(dtype(is) /= ' ' .and. ndata(is,1) > 0)then

        ndatax_all=ndatax_all + ndata(is,1)

        if (dtype(is)=='lag') then    ! lagrangian data
           call dislag(ndata(is,1),mm1,lunout,obsfile_all(is),dtype(is),&
                nobs_s) 
           nsat1(is)= nobs_sub(mm1,is)
        else 
           obproc:do ii=1,npe
             if(nobs_sub(ii,is) > 0)then
               mype_diaghdr(is) = ii-1
               exit obproc
             end if
           end do obproc
                
           if(nobs_sub(mm1,is) > zero)then    ! classical observations
              call disobs(ndata(is,1),nobs_sub(mm1,is),mm1,lunout, &
                  obsfile_all(is),dtype(is))
           end if
        end if
        nsat1(is)= nobs_sub(mm1,is)
        if(mm1 == 1 .and. (print_verbose .or. print_obs_para))then
           write(6,1000)dtype(is),dplat(is),(nobs_sub(ii,is),ii=1,npe)
1000       format('OBS_PARA: ',2A10,8I10,/,(10X,10I10))
        end if

        ! Simple logic to organize which tasks do and do not have obs. 
        !  Needed for buddy check QC.   
        if (twodvar_regional .and. dtype(is) == 't' .and.  buddycheck_t) then 
           ikey_yes=0 
           ikey_no=0 
           ikey=0 
           do ii=1,npe 
              if (nobs_sub(ii,is)>0) then 
                 icolor(ii)=1 
                 ikey(ii)=ikey_yes 
                 ikey_yes=ikey_yes+1 
              else 
                 icolor(ii)=2 
                 ikey(ii)=ikey_no 
                 ikey_no=ikey_no+1 
              end if 
           end do 
 
           ! With organized colors and keys, now create the new MPI communicator 
           !   which only talks to pe's who have obs on their subdomains.  This is 
           !   needed for MPI communication within the setup* routines (e.g. a buddy check). 
               
           call mpi_comm_split(mpi_comm_world,icolor(mm1),ikey(mm1),obs_sub_comm(is),ierror)   
           CALL MPI_COMM_SIZE(obs_sub_comm(is), newprocs, ierror) 
           CALL MPI_COMM_RANK(obs_sub_comm(is), newrank, ierror) 
           if (buddydiag_save) write(6,'(A,I3,I10,A,I20,A,I3,A,I3)') 'obs_para: mype/myobs=',& 
                              mype,nobs_sub(mm1,is),'newcomm=',obs_sub_comm(is),'newprocs=', & 
                              newprocs,'newrank=',newrank            
        end if 
        
     end if

  end do
  close(lunout)


! If there are no obs available, turn off moisture constraint.  
! If the user still wants the moisture constraint active when no obs are
! present, comment out the block of code below.
  if (ndatax_all == 0) then
     factqmin=zero
     factqmax=zero
     if (mype==0) write(6,*)'OBS_PARA:  ***WARNING*** no observations to be  ',&
          ' assimilated. reset factqmin,factqmax=',factqmin,factqmax
  endif


  return
end subroutine obs_para

subroutine disobs(ndata,nobs,mm1,lunout,obsfile,obstypeall)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    disobs  distribute observations into each pe subdomain
!   prgmmr: weiyu yang                                date: 1998-04-06
!
! abstract: distribute observations into each pe subdomain
!
! program history log:
!   1998-04-06  weiyu yang
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-15  treadon, reformat documenation
!   2004-07-28  treadon - add only to module use, add intent in/out
!   2004-11-19  derber - change to eliminate additional file and use logical
!                        rather than weights
!   2005-10-17  treadon - remove conversion to grid-relative cooridnates
!   2006-04-06  middlecoff - changed lunin from 15 to 11
!   2008-04-29  safford - rm unused vars and uses
!   2008-09-08  lueken  - merged ed's changes into q1fy09 code
!   2009-04-21  derber  - reformulate to remove communication
!
!   input argument list:
!     nn_obs   - number of an observation data
!     ndata    - number of observations
!     lunin    - unit from which to read all obs of a given type
!     lunout   - unit to which to write subdomain specific observations
!     mm1      - mpi task number + 1
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: periodic_s,nlon,nlat,jlon1,ilat1,istart,jstart
  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: ndata,lunout,mm1,nobs
  character(len=*)              ,intent(in   ) :: obsfile
  character(len=*)              ,intent(in   ) :: obstypeall

! Declare local variables
  integer(i_kind) lon,lat,lat_data,lon_data,n,k,lunin
  integer(i_kind) jj,nreal,nchanl,nn_obs
  integer(i_kind) ndata_s
  integer(i_kind),dimension(mm1):: ibe,ibw,ibn,ibs
  logical,allocatable,dimension(:):: luse_s
  integer(i_kind),allocatable,dimension(:):: ioid_s
        ! IOID represents Initial-Obs-ID, which is the serial index of given
        ! observation record of a given observation source, before they are
        ! distributed according to the guess-grid partition.  This ID is saved
        ! for all future needs of obs. redistribution, such that the sorted
        ! sequences can be reproduced.
  real(r_kind),allocatable,dimension(:,:):: obs_data,data1_s
  character(10):: obstype
  character(20):: isis

! Read and write header

  do k=1,mm1

!    ibw,ibe,ibs,ibn west,east,south and north boundaries of total region
     ibw(k)=jstart(k)-1
     ibe(k)=jstart(k)+jlon1(k)-1
     ibs(k)=istart(k)-1
     ibn(k)=istart(k)+ilat1(k)-1

  end do

  lunin=11
  open(lunin,file=trim(obsfile),form='unformatted')
  read(lunin)obstype,isis,nreal,nchanl,lat_data,lon_data
  if(trim(obstype) /=trim(obstypeall)) &
        write(6,*)'DISOBS:  ***ERROR***   obstype,obstypeall=',trim(obstype),trim(obstypeall)

  nn_obs = nreal + nchanl

  allocate(obs_data(nn_obs,ndata))
!  Read in all observations of a given type along with subdomain flags
  read(lunin) obs_data
  close(lunin)

  ndata_s=0
  allocate(data1_s(nn_obs,nobs),luse_s(nobs),ioid_s(nobs))

! Loop over all observations.  Locate each observation with respect
! to subdomains.
  obs_loop: do n=1,ndata
     lat=obs_data(lat_data,n)
     lat=min(max(1,lat),nlat)

     if(lat>=ibs(mm1).and.lat<=ibn(mm1))then
        lon=obs_data(lon_data,n)
        lon=min(max(0,lon),nlon)
        if((lon >= ibw(mm1).and. lon <=ibe(mm1))  .or.  &
           (lon == 0   .and. ibe(mm1) >=nlon) .or.  &
           (lon == nlon    .and. ibw(mm1) <=1) .or. periodic_s(mm1)) then
           ndata_s=ndata_s+1
           ioid_s(ndata_s)=n
           luse_s(ndata_s)= .true.
           do jj= 1,nn_obs
              data1_s(jj,ndata_s) = obs_data(jj,n)
           end do
          
           prec_loop: do k=1,mm1-1
              if(lat>=ibs(k).and.lat<=ibn(k)) then
                 if((lon >= ibw(k).and. lon <=ibe(k))  .or.  &
                    (lon == 0     .and. ibe(k) >=nlon) .or.  &
                    (lon == nlon  .and. ibw(k) <=1) .or. periodic_s(k)) then
                    luse_s(ndata_s)= .false.
                    exit prec_loop
                 end if
              end if
           end do prec_loop
           if(nobs == ndata_s) exit obs_loop
         end if
     end if
  end do obs_loop
  deallocate(obs_data)

! Write observations for given task to output file
  write(lunout) obstypeall,isis,nreal,nchanl
  write(lunout) data1_s,luse_s,ioid_s
  deallocate(data1_s,luse_s,ioid_s)


  return
end subroutine disobs
subroutine count_obs(ndata,nn_obs,lat_data,lon_data,obs_data,nobs_s)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    count_obs  counts number of observations on each subdomain
!   prgmmr: derber                                    date: 2014-12-16
!
! abstract: count observations in each pe subdomain
!
! program history log:
!   2014-12-16  derber
!
!   input argument list:
!     ndata    - number of observations
!     nn_obs   - number of an observation data for each ob 
!     lat_data - location of lattitude
!     lon_data - location of longitude
!     obs_data - observation information array
!
!   output argument list:
!     nobs_s   - number of observations on all subdomains
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use gridmod, only: periodic_s,nlon,nlat,jlon1,ilat1,istart,jstart
  use mpimod, only: npe
  implicit none

! Declare passed variables
  integer(i_kind)               ,intent(in   ) :: ndata,lat_data,lon_data
  integer(i_kind)               ,intent(in   ) :: nn_obs
  integer(i_kind),dimension(npe),intent(inout) :: nobs_s
  real(r_kind),dimension(nn_obs,*),intent(in) :: obs_data
! real(r_kind),dimension(nn_obs,ndata),intent(in) :: obs_data

! Declare local variables
  integer(i_kind) lon,lat,n,k
  integer(i_kind),dimension(npe):: ibe,ibw,ibn,ibs

! Read and write header

  do k=1,npe

!    ibw,ibe,ibs,ibn west,east,south and north boundaries of total region
     ibw(k)=jstart(k)-1
     ibe(k)=jstart(k)+jlon1(k)-1
     ibs(k)=istart(k)-1
     ibn(k)=istart(k)+ilat1(k)-1

  end do


  nobs_s=0

! Loop over all observations.  Locate each observation with respect
! to subdomains.
  do n=1,ndata
     lat=obs_data(lat_data,n)
     lat=min(max(1,lat),nlat)

     do k=1,npe
        if(lat>=ibs(k).and.lat<=ibn(k)) then
           lon=obs_data(lon_data,n)
           lon=min(max(0,lon),nlon)
           if((lon >= ibw(k).and. lon <=ibe(k))  .or.  &
              (lon == 0   .and. ibe(k) >=nlon) .or.  &
              (lon == nlon    .and. ibw(k) <=1) .or. periodic_s(k)) then
                 nobs_s(k)=nobs_s(k)+1
           end if
        end if
     end do
  end do 
  return
end subroutine count_obs

! ------------------------------------------------------------------------
subroutine dislag(ndata,mm1,lunout,obsfile,obstypeall,ndata_s)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dislag  distribute lagrangian observations into each pe 
!                subdomain
!   prgmmr: lmeunier                                  date: 2009-03-12
!
! abstract: distribute lagrangian observations into each pe subdomain
!           (based on disobs). All observations of one balloon are
!           are associated with a given processor
!
! program history log:
!   2009-03-12  lmeunier
!
!   input argument list:
!     ndata_s  - number of observations in each pe sub-domain
!     ndata    - number of observations
!     obsfile  - unit from which to read all obs of a given type
!     lunout   - unit to which to write subdomain specific observations
!     mm1      - mpi task number + 1
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind
  use lag_fields, only: orig_lag_num
  implicit none

! Declare passed variables
  integer(i_kind),intent(in   ) :: ndata,lunout,mm1
  integer(i_kind),intent(inout) :: ndata_s
  character(14)  ,intent(in   ) :: obsfile
  character(10)  ,intent(in   ) :: obstypeall

! Declare local variables
  integer(i_kind) num_data,n,lunin,num
  integer(i_kind) jj,nreal,nchanl,nn_obs,ndatax
  logical,allocatable,dimension(:):: luse,luse_s,luse_x
  real(r_kind),allocatable,dimension(:,:):: obs_data,data1_s
  character(10):: obstype
  character(20):: isis

  ndata_s=0

  lunin=11
  open(lunin,file=obsfile,form='unformatted')
  read(lunin)obstype,isis,nreal,nchanl,num_data
  if(obstype /=obstypeall) &
        write(6,*)'DISLAG:  ***ERROR***   obstype,obstypeall=',obstype,obstypeall

  nn_obs = nreal + nchanl

  allocate(obs_data(nn_obs,ndata))
! Read in all observations of a given type along with subdomain flags
  read(lunin) obs_data
  close(lunin)

  allocate(luse(ndata),luse_x(ndata))
  luse=.false.
  luse_x=.false.

! Loop over all observations.  Locate each observation with respect
! to subdomains.
  use: do n=1,ndata

!    Does the observation belong to the subdomain for this task?
     num=int(obs_data(num_data,n),i_kind)  
     if ((mm1-1)==orig_lag_num(num,2)) then
        ndata_s=ndata_s+1
        luse(n)=.true.
        luse_x(n)=.true.  ! Never on another subdomain
     end if

  end do use

  if(ndata_s > 0)then
     allocate(data1_s(nn_obs,ndata_s),luse_s(ndata_s))
     ndatax=0
     do n=1,ndata

        if(luse(n))then

           ndatax=ndatax+1
           luse_s(ndatax)=luse_x(n)
 
           do jj= 1,nn_obs
              data1_s(jj,ndatax) = obs_data(jj,n)
           end do

        end if

     end do

!    Write observations for given task to output file
     write(lunout) obstypeall,isis,nreal,nchanl
     write(lunout) data1_s,luse_s
     deallocate(data1_s,luse_s)
  endif
  deallocate(obs_data,luse,luse_x)

  return
end subroutine dislag
