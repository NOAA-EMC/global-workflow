subroutine read_gps(nread,ndata,nodata,infile,lunout,obstype,twind, &
             nprof_gps,sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: read_gps                   read in and reformat gps data
!   prgmmr: l.cucurull       org: JCSDA/NCEP          date: 2004-03-18
!
! abstract:  This routine reads in and reformats gps radio occultation data.
!
!            When running the gsi in regional mode, the code only
!            retains those observations that fall within the regional
!            domain
!
! program history log:
!   2004-03-18  cucurull - testing a gps ref profile
!   2004-06-04  cucurull - reading available gps ref profiles at analysis time
!   2004-06-24  treadon  - update documentation
!   2004-07-29  treadon  - add only to module use, add intent in/out
!   2004-11-18  cucurull - increase number of fields read
!   2004-01-26  cucurull - replace error estimation, add check for time
!   2005-03-03  cucurull - reading files in bufr format
!   2005-03-28  cucurull - reading satellite information from bufr file for diagnostics
!   2005-06-01  cucurull - update time QC 
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-10-11  treadon - change convinfo read to free format
!   2005-10-17  treadon - add grid and earth relative obs location to output file
!   2005-10-18  treadon - remove array obs_load and call to sumload
!   2005-10-26  treadon - add routine tag to convinfo printout
!   2005-12-01  cucurull - add logical ref_obs
!                          .true.  will read refractivity
!                          .false. will read bending angle
!                        - add preliminary QC checks for refractivity and bending
!                        - add errors for bending angle
!   2006-02-03  derber  - modify for new obs control and obs count
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-13 cucurull - modify errors for refractivity and increase QC checks
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-09-08 cucurull - modify bufr variables for COSMIC
!   2006-10-13 cucurull - add QC checks
!   2007-03-01 tremolet - measure time from beginning of assimilation window
!   2008-02-02 treadon  - sort out gpsro bufr by satellite id
!   2008-02-06 cucurull - modify to support move from DDS to GTS/NC gpsro data feed
!   2008-04-21 safford  - rm unused vars and uses
!   2008-09-25 treadon  - skip report if ref_obs=.t. but no refractivity data
!   2009-02-05 cucurull - assing instrument error (ref) to a nominal value
!   2009-04-01 cucurull - add QC for Metop/GRAS
!   2010-05-26 cucurull - add QC flag for Metop/GRAS bending angle
!                       - modify code to read nested delayed replication to get GRAS
!                         ionospheric-compensated bending angles 
!   2010-11-8 cucurull  - skip profile in gpsro bufr when satellite is not
!                         listed in the convinfo file. Also, remove some QC dependencies
!                         on the order of the satellites in the convinfo file
!   2011-01-06 cucurull - replace obstype (gps_ref/gps_bnd) with sis (gps) due to replacing
!                         gps_ref/gps_bnd with gps in convinfo files 
!   2011-08-24 cucurull - add preliminaty qc flags for C/NOFS, SAC-C, Oceansat-2, METOP-B, SAC-D, and M-T
!   2012-10-25 cucurull - add qc flag for bnd=0 case
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2015-02-23  Rancic/Thomas - add l4densvar to time window logical
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of gps observations read
!     ndata    - number of gps profiles retained for further processing
!     nodata   - number of gps observations retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

  use kinds, only: r_kind,i_kind,r_double
  use constants, only: deg2rad,zero,rad2deg,r60inv,r100
  use obsmod, only: iadate,ref_obs
  use gsi_4dvar, only: l4dvar,l4densvar,iwinbgn,winlen
  use convinfo, only: nconvtype,ctwind,cermax, &
        ncmiter,ncgroup,ncnumgrp,icuse,ictype,ioctype
  use gridmod, only: regional,nlon,nlat,tll2xy,rlats,rlons
  use mpimod, only: npe
  implicit none

! Declare passed variables
  character(len=*),intent(in   ) :: obstype,infile
  character(len=20),intent(in  ) :: sis
  real(r_kind)    ,intent(in   ) :: twind
  integer(i_kind) ,intent(in   ) :: lunout
  integer(i_kind) ,intent(inout) :: nread,ndata,nodata
  integer(i_kind),dimension(npe) ,intent(inout) :: nobs
  integer(i_kind) ,intent(inout) :: nprof_gps

! Declare local parameters  
  integer(i_kind),parameter:: maxlevs=500
  integer(i_kind),parameter:: maxinfo=16
  real(r_kind),parameter:: r10000=10000.0_r_kind
  real(r_kind),parameter:: r360=360.0_r_kind

! Declare local variables
  logical good,outside
  
  character(10) nemo
  character(80) hdr1a
  character,dimension(8):: subset
  character(len=16),allocatable,dimension(:):: gpsro_ctype

  
  integer(i_kind) lnbufr,i,k,m,maxobs,ireadmg,ireadsb,said,ptid
  integer(i_kind) nmrecs
  integer(i_kind) notgood,idate
  integer(i_kind) iret,levs,levsr,nreps_ROSEQ1,mincy,minobs
  integer(i_kind) nreal,nchanl,ilat,ilon
  integer(i_kind),dimension(5):: idate5
  integer(i_kind)             :: ikx
  integer(i_kind):: ngpsro_type,igpsro_type
  integer(i_kind),parameter:: mxib=31
  integer(i_kind) ibit(mxib),nib
  logical lone


  integer(i_kind),allocatable,dimension(:):: gpsro_itype,gpsro_ikx,nmrecs_id
  
  real(r_kind) timeo,t4dv
  real(r_kind) pcc,qfro,usage,dlat,dlat_earth,dlon,dlon_earth,freq_chk,freq
  real(r_kind) height,rlat,rlon,ref,bend,impact,roc,geoid,&
               bend_error,ref_error,bend_pccf,ref_pccf

  real(r_kind),allocatable,dimension(:,:):: cdata_all
 
  integer(i_kind),parameter:: n1ahdr=10
  real(r_double),dimension(n1ahdr):: bfr1ahdr
  real(r_double),dimension(50,maxlevs):: data1b
  real(r_double),dimension(50,maxlevs):: data2a
  real(r_double),dimension(maxlevs):: nreps_this_ROSEQ2
 
  data lnbufr/10/
  data hdr1a / 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU' / 
  data nemo /'QFRO'/
  
!***********************************************************************************

  maxobs=2e6
  nreal=maxinfo
  nchanl=0
  ilon=2
  ilat=3

  nmrecs=0
  notgood=0

! Check convinfo file to see requesting to process gpsro data
  ikx = 0
  do i=1,nconvtype
      if ( trim(sis)==trim(ioctype(i))) ikx=ikx+1
  end do

! If no data requested to be process, exit routine
  if(ikx==0)then
     write(6,*)'READ GPS:  CONVINFO DOES NOT INCLUDE ANY ',trim(sis),' DATA'
     return
  end if

! Allocate and load arrays to contain gpsro types.
  ngpsro_type=ikx
  allocate(gpsro_ctype(ngpsro_type), gpsro_itype(ngpsro_type), &
       gpsro_ikx(ngpsro_type),nmrecs_id(ngpsro_type))
  nmrecs_id=0
  ikx=0
  do i=1,nconvtype
      if ( trim(sis)==trim(ioctype(i))) then
        ikx=ikx+1
        gpsro_ctype(ikx)=ioctype(i)
        gpsro_itype(ikx)=ictype(i)
        gpsro_ikx(ikx)  =i
     endif
  end do


! Open file for input, then read bufr data
  open(lnbufr,file=trim(infile),form='unformatted')
  call openbf(lnbufr,'IN',lnbufr)
  call datelen(10)
  call readmg(lnbufr,subset,idate,iret)
  if (iret/=0) goto 1010

! Allocate work array to hold observations
  allocate(cdata_all(nreal,maxobs))

! Big loop over the bufr file

  do while(ireadmg(lnbufr,subset,idate)==0)
     read_loop:  do while(ireadsb(lnbufr)==0)

! Read/decode data in subset (profile)

! Extract header information
        call ufbint(lnbufr,bfr1ahdr,n1ahdr,1,iret,hdr1a)
        call ufbint(lnbufr,qfro,1,1,iret,nemo)

! observation time in minutes
        idate5(1) = bfr1ahdr(1) ! year
        idate5(2) = bfr1ahdr(2) ! month
        idate5(3) = bfr1ahdr(3) ! day
        idate5(4) = bfr1ahdr(4) ! hour
        idate5(5) = bfr1ahdr(5) ! minute
        pcc=bfr1ahdr(6)         ! profile per cent confidence
        roc=bfr1ahdr(7)         ! Earth local radius of curvature
        said=bfr1ahdr(8)        ! Satellite identifier
        ptid=bfr1ahdr(9)        ! Platform transmitter ID number
        geoid=bfr1ahdr(10)      ! Geoid undulation
        call w3fs21(idate5,minobs)

! Locate satellite id in convinfo file
        ikx = 0
        find_loop: do i=1,ngpsro_type
            if ( (trim(sis)==trim(gpsro_ctype(i))) .and. (said == gpsro_itype(i)) ) then
              ikx=gpsro_ikx(i)
              igpsro_type = i
              exit find_loop
           endif
        end do find_loop
        if (ikx==0) then 
           cycle read_loop
        endif
   
! check time window in subset
        t4dv=real((minobs-iwinbgn),r_kind)*r60inv
        if (l4dvar.or.l4densvar) then
           if (t4dv<zero .OR. t4dv>winlen) then
              write(6,*)'READ_GPS:      time outside window ',&
                   t4dv,' skip this report'
              cycle read_loop
           endif
        else
           call w3fs21(iadate,mincy) ! analysis time in minutes
           timeo=real(minobs-mincy,r_kind)*r60inv
           if (abs(timeo)>ctwind(ikx) .or. abs(timeo) > twind) then
              write(6,*)'READ_GPS:      time outside window ',&
                   timeo,' skip this report'
              cycle read_loop
           endif
        endif
 
! Check profile quality flags
        if ( ((said > 739).and.(said < 746)).or.(said == 820).or.(said == 786)) then  !CDAAC processing
           if(pcc==zero) then
              write(6,*)'READ_GPS:  bad profile said=',said,'ptid=',ptid,&
                  ' SKIP this report'
              cycle read_loop
           endif
        endif

        if ((said == 4).or.(said == 3).or.(said == 421).or.(said == 440).or.&
            (said == 821)) then ! GRAS SAF processing
           call upftbv(lnbufr,nemo,qfro,mxib,ibit,nib)
           lone = .false.
             if(nib > 0) then
               do i=1,nib
                 if(ref_obs) then
                    if(ibit(i)== 6) then
                       lone = .true.
                       exit
                    endif
                 else
                    if(ibit(i)== 5) then
                       lone = .true.
                       exit
                    endif
                 endif
               enddo
             endif 

           if(lone) then
              write(6,*)'READ_GPS:  bad profile said=',said,'ptid=',ptid,&
                   ' SKIP this report'
              cycle read_loop
           endif
        endif


! Read bending angle information
! Get the number of occurences of sequence ROSEQ2 in this subset
! (will also be the number of replications of sequence ROSEQ1), nreps_ROSEQ1
! Also determine the number of replications of sequence ROSEQ2 nested inside
! each replication of ROSEQ1,
! nreps_this_ROSEQ2(1:nreps_ROSEQ1) - currently = 3 frequencies (L1, L2, zero)

        call ufbint(lnbufr,nreps_this_ROSEQ2,1,maxlevs,nreps_ROSEQ1,'{ROSEQ2}')

! Store entire contents of ROSEQ1 sequence (including contents of nested ROSEQ2 sequence)
! in array data1b
        call ufbseq(lnbufr,data1b,50,maxlevs,levs,'ROSEQ1') 

        if(levs.ne.nreps_ROSEQ1) then
           write(6,*) 'READ_GPS:  **WARNING** said,ptid=',said,ptid,&
                ' mismatch between sequence of ROSEQ1 and ROSEQ2 occurence',levs,nreps_ROSEQ1, &
                ' SKIP this report'
           cycle read_loop
        endif

! Check we have the same number of levels for ref and bending angle
! when ref_obs on to get lat/lon information

        call ufbseq(lnbufr,data2a,50,maxlevs,levsr,'ROSEQ3') ! refractivity
        if ((ref_obs).and.(levs/=levsr)) then
           write(6,*) 'READ_GPS:  **WARNING** said,ptid=',said,ptid,&
                ' with gps_bnd levs=',levs,&
                ' and gps_ref levsr=',levsr,&
                ' SKIP this report'
           cycle read_loop
        endif

! Increment report counters
        nmrecs = nmrecs + 1      ! count reports in bufr file
        nmrecs_id(igpsro_type) = nmrecs_id(igpsro_type) + 1

! Set usage flag
        usage = zero
        if(icuse(ikx) < 0)usage=r100
        if(ncnumgrp(ikx) > 0 )then                     ! cross validation on
           if(mod(nmrecs,ncnumgrp(ikx))== ncgroup(ikx)-1)usage=ncmiter(ikx)
        end if

! Loop over levs in profile
        do k=1, levs
           nread=nread+1  ! count observations
           rlat=data1b(1,k)  ! earth relative latitude (degrees)
           rlon=data1b(2,k)  ! earth relative longitude (degrees)
           height=data2a(1,k)
           ref=data2a(2,k)
           ref_error=data2a(4,k)
           ref_pccf=data2a(6,k)

       ! Loop over number of replications of ROSEQ2 nested inside this particular replication of ROSEQ1
           do i=1,nreps_this_ROSEQ2(k)
              m=(6*i)-2
              freq_chk=data1b(m,k)      ! frequency (hertz)
              if(nint(freq_chk).ne.0) cycle ! do not want non-zero freq., go on to next replication of ROSEQ2
              freq=data1b(m,k)
              impact=data1b(m+1,k)      ! impact parameter (m)
              bend=data1b(m+2,k)        ! bending angle (rad)
              bend_error=data1b(m+4,k)  ! RMSE in bending angle (rad)
           enddo
           bend_pccf=data1b((6*nreps_this_ROSEQ2(k))+4,k)  ! percent confidence for this ROSEQ1 replication

! Check domain in regional model

! Preliminary (sanity) QC checks for bad and missing data
           good=.true.
           if((abs(rlat)>90._r_kind).or.(abs(rlon)>r360).or.(height<=zero)) then
              good=.false.
           endif
           if (ref_obs) then
              if ((ref>=1.e+9_r_kind).or.(ref<=zero).or.(height>=1.e+9_r_kind)) then
                 good=.false.
              endif
           else
              if ((bend>=1.e+9_r_kind).or.(bend<=zero).or.(impact>=1.e+9_r_kind).or.(impact<roc)) then
                 good=.false.
              endif
           endif

! If observation is "good" load into output array
           if(good) then

! Assign preliminary errors

              if(ref_obs) then
                 ref_error = ref*0.01_r_kind
              else                      ! bending angle
                 if((impact-roc) <= r10000) then
                    bend_error=(-bend*0.09_r_kind/r10000)*(impact-roc)+bend*1.e-1_r_kind
                 else
                    bend_error=max(7.e-6_r_kind,bend*1.e-2_r_kind)
                 endif
              endif

              if (rlon==r360)  rlon=zero
              if (rlon<zero  ) rlon=rlon+r360

              dlat_earth = rlat * deg2rad  !convert to radians
              dlon_earth = rlon * deg2rad
 
              if(regional)then
                 call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)
                 if (outside) cycle read_loop
              else
                 dlat = dlat_earth
                 dlon = dlon_earth
                 call grdcrd1(dlat,rlats,nlat,1)
                 call grdcrd1(dlon,rlons,nlon,1)
              endif

              ndata  = min(ndata +1,maxobs)
              nodata = min(nodata+1,maxobs)
 
       
              if (ref_obs) then
                 cdata_all(1,ndata) = ref_error      ! gps ref obs error (units of N)
                 cdata_all(4,ndata) = height         ! geometric height above geoid (m)
                 cdata_all(5,ndata) = ref            ! refractivity obs (units of N)
!                cdata_all(9,ndata) = ref_pccf       ! per cent confidence (%)
              else
                 cdata_all(1,ndata) = bend_error     ! gps bending error (radians)
                 cdata_all(4,ndata) = impact         ! impact parameter (m)
                 cdata_all(5,ndata) = bend           ! bending angle obs (radians)
!                cdata_all(9,ndata) = bend_pccf      ! per cent confidence (%)
              endif
              cdata_all(9,ndata) = pcc             ! profile per cent confidence (0 or 100)
              cdata_all(2,ndata) = dlon            ! grid relative longitude
              cdata_all(3,ndata) = dlat            ! grid relative latitude
              cdata_all(6,ndata) = t4dv            ! time relative to analysis (hour) 
              cdata_all(7,ndata) = ikx             ! type assigned to ref data
              cdata_all(8,ndata) = nmrecs          ! profile number
              cdata_all(10,ndata)= roc             ! local radius of curvature (m)
              cdata_all(11,ndata)= said            ! satellite identifier
              cdata_all(12,ndata)= ptid            ! platform transmitter id number
              cdata_all(13,ndata)= usage           ! usage parameter
              cdata_all(14,ndata)= dlon_earth*rad2deg  ! earth relative longitude (degrees)
              cdata_all(15,ndata)= dlat_earth*rad2deg  ! earth relative latitude (degrees)
              cdata_all(16,ndata)= geoid           ! geoid undulation (m)

           else
              notgood = notgood + 1
           end if


! End of k loop over levs
        end do

     enddo read_loop        ! subsets
  enddo                     ! messages

! Write observation to scratch file
  call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
  write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,nmrecs
  write(lunout) ((cdata_all(k,i),k=1,nreal),i=1,ndata)
  deallocate(cdata_all)
  
! Close unit to input file
1010 continue
  call closbf(lnbufr)

  nprof_gps = nmrecs
  write(6,*)'READ_GPS:  # bad or missing data=', notgood
  do i=1,ngpsro_type
     if (nmrecs_id(i)>0) &
          write(6,1020)'READ_GPS:  LEO_id,nprof_gps = ',gpsro_itype(i),nmrecs_id(i)
  end do
  write(6,1020)'READ_GPS:  ref_obs,nprof_gps= ',ref_obs,nprof_gps
1020 format(a31,2(i6,1x))

! Deallocate arrays
  deallocate(gpsro_ctype,gpsro_itype,gpsro_ikx,nmrecs_id)

  return
end subroutine read_gps



