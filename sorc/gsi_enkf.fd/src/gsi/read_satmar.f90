subroutine read_satmar (nread, ndata, nodata,                                 &
                        infile, obstype, lunout, gstime, twind, sis,          &
                        nobs  )
!
!subroutine read_goesimg(mype,val_img,ithin,rmesh,jsatid,gstime,&
!     infile,lunout,obstype,nread,ndata,nodata,twind,sis, &
!     mype_root,mype_sub,npe_sub,mpi_comm_sub,nobs, &
!     nrec_start,dval_use)
! *#* Documentation block - Start *#*
!
! =>  Subroutine read_satmar        :     Reads Hs from Altimeters
! =>  Coder                         :     stelios flampouris - stylianos.flampouris@noaa.gov
! => Abstract                       :     
!      1. This routine reads data from CNES and OCEANAVO:
!      OCEANAVO: xx114 (NC031114) // xx120 (NC031120) // xx121 (NC031121) // xx127 (NC031127) // xx130 (NC031130)
!      CNES    : xx115 (NC031115) // xx122 (NC031122) // xx123 (NC031123) // xx124 (NC031124)
!
!      DATA SET      |     Corresponding Satellite
!      xx114, xx115  |     JASON-2 
!      xx120, xx123  |     CRYOSAT-2
!      xx121, xx122  |     SARAL/ATK 
!      xx124, xx127  |     JASON-3
!      xx130         |     SENTINEL3a
!      
!      2. It uses the provided flags for QC.
!      3. Observations only within the domain of interest are retained.
!
!      For reading the data of interest, the "headers" (hdr_ variables) have to be
!      modified accordingly; in this case, Significant Wave Height (howv or hs) data are
!      imported.
!
! => History log                    :
! 2016.03.07      :     stelios flampouris
! 2017.05.03      :     pondeca: add c_station_id and set station id to "SATMAR"
!                                for now
! 2017.08.07      :     stelios: 1. The data from CNES and OCEANAVO can be used.
!                 :              2. The c_station_id is variable and gets the
!                 input from the "subset" according to the values given at the
!                 datasets when dumped at the tanks.
! 2017.08.12      :     stelios: Imports Sentinel3a howv obs
! 2017.10.01      :     jacob  : Fix bug
! 2017.10.23      :     stelios: Keep unique data 
!
!   input argument list:
!     ithin    - flag to thin data
!     rmesh    - thinning mesh size (km)
!     gstime   - analysis time in minutes from reference date
!     infile   - unit from which to read BUFR data
!     lunout   - unit to which to write data for further processing
!     obstype  - observation type to process
!     twind    - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!
!   output argument list:
!     nread    - number of obs read
!     ndata    - number of obs retained for further processing
!     nodata   - number of obs retained for further processing
!     nobs     - array of observations on each subdomain for each processor
!
! => Attributes                     :
!     language: f90
!
! *#* Documentation block - End  *#*
!
! *#* Variables Declaration - Start *#*       
   use kinds, only: r_kind,r_double,i_kind
   use gsi_4dvar, only: l4dvar,l4densvar,winlen,iwinbgn,thin4d,time_4dvar
   use constants, only: zero, deg2rad,rad2deg,one,two,three,four,ten,half, &
       r60inv,r60,r3600,tiny_r_kind !,init_constants_derived
   use gridmod, only: regional, rlats,rlons,nlat,nlon,txy2ll,tll2xy, &
       twodvar_regional
   use satthin, only: map2tgrid,destroygrids,makegrids
   use convinfo, only: ithin_conv,rmesh_conv,nconvtype,icuse,ictype,ioctype,ctwind, &
                 pmot_conv
!  use convinfo, only: icsubtype
   use convthin, only: make3grids,use_all,map3grids_m,del3grids
   use obsmod, only: bmiss,hilbert_curve,reduce_diag
   use mpimod, only: npe
   
   implicit none
!
!! %%% Declare passed variables IN
   integer(i_kind)                     , intent(in) :: lunout
   character(len=*)                    , intent(in) :: infile, obstype
   real(r_kind)                        , intent(in) :: gstime,twind
!! %%%% Declare Passed Variables INOUT
   integer(i_kind)                     , intent(inout) :: nread,ndata,nodata
   integer(i_kind),dimension(npe)      , intent(inout) :: nobs
!! %%% Declare local varables
!  integer
   integer(i_kind) :: ithin
   integer(i_kind),parameter  :: lun11 = 11
   integer(i_kind),parameter  :: nosat =  9 
   integer(i_kind),parameter  :: n_tm  =  6
   integer(i_kind),parameter  :: n_lc  =  2
   integer(i_kind),parameter  :: n_howv=  2
   integer(i_kind),parameter  :: dtLen =  10
   integer(i_kind),parameter  :: nreal =  23
   real   (r_kind),parameter  :: r90   =  90.0_r_kind
   real   (r_kind),parameter  :: r6    =  6.0_r_kind
   real   (r_kind),parameter :: dflt_err = 0.2_r_kind
!
   integer(i_kind) :: tot,cnt,cnt1,k,iout,i
   integer(i_kind) :: ireadmg,ireadsb,idate
   integer(i_kind) :: iRec,ierr,nc,i1,ilat,ilon,nchanl,nlevp,indsat
   integer(i_kind) :: nmind, nrec
   integer(i_kind) :: thisobtype_usage, iuse
!  real
   real(r_kind),allocatable,dimension(:, :) :: data_all
   real(r_kind),allocatable,dimension(:):: DumForThin
!   real(r_kind),allocatable,dimension(:   ) :: data_1d
   real(r_kind) :: dlon,dlat
   real(r_kind) :: tdiff,crit1,timedif,toff
   !
   real(r_kind) :: nsec, rminobs
   real(r_kind) :: dlon_earth,dlat_earth
   real(r_kind) :: depth,usage
   real(r_kind) :: rmesh,xmesh
!   real(r_kind),dimension(1,1):: r_prvstg,r_sprvstg
!  character
   character(len=20),dimension(nosat) :: namesat
   character(len=20) :: sis
   character(len=20) :: subset
   character(len=11), parameter :: myname='read_satmar '
!  logical
   logical outside, luse
   logical lhilbert
   logical, dimension(nosat) :: satuse
!!!! #####     JS2      ##### !!!!
   integer(i_kind),parameter  :: n_fltJS2 = 7
!
   real(r_double), dimension (n_fltJS2)  :: flt_1dJS2
   real(r_double), dimension (n_tm)   :: time_1d
   integer(i_kind), dimension (n_tm-1) :: time_1dMN
   real(r_double), dimension (n_lc)   :: loc_1d
   real(r_double), dimension (n_howv) :: howv_1d
   real(r_kind) :: t4dv
   character(80),parameter:: hdr_fltJS2   = 'RSST AETP ASFL ADQF ALRF IPIN ODLE'
   character(80),parameter:: hdr_time  = 'YEAR MNTH DAYS HOUR MINU SECW'
!   character(80),parameter:: hdr_loc   = 'CLATH CLONH'
   character(80)  :: hdr_loc
   character(80),parameter:: hdr_howvJS2  = 'KBSW RKSW ' !CBSW RCSW'
!!!! #####     SARAL    ##### !!!!
   integer(i_kind),parameter  :: n_fltSAL = 5
!
   real(r_double), dimension (n_fltSAL)  :: flt_1dSAL
   !
   character(80),parameter:: hdr_fltSAL = 'RSST BSADQF NVPSWH IPIN ODLE'
   character(80),parameter:: hdr_howvSAL  = 'SBSWH RMSSWH'
!!!! #####     CS2    ##### !!!!
   integer(i_kind),parameter  :: n_fltCS2 = 5
   integer(i_kind),parameter  :: n_howvCS2 = 4
!
   real(r_double), dimension (n_fltCS2)  :: flt_1dCS2
!   real(r_kind), dimension (n_howvCS2) :: howv_1dCS2
!
   integer(i_kind),parameter  :: n_howvNO = 2   !NO
!
   character(80),parameter:: hdr_timeCS2  = 'YEAR MNTH DAYS HOUR MINU SECO'
   character(80),parameter:: hdr_fltCS2   = 'DSST ODLE L1PQ L1PF L2PF'
   character(80),parameter:: hdr_howvCS2  = 'KBSW NVPK2'
!   character(80),parameter:: hdr_howvCS2  = 'KBSW NVPK2 SBSW SWHS '
   character(80),parameter:: hdr_howvNO  = 'HOWV SDWH'   !NO   
!   
   character(80),parameter::hdr_station = 'SAID'
   real(r_double) :: rstation_id
   character(8) c_station_id
!
   equivalence(rstation_id,c_station_id)
! Swords
   integer(i_kind),parameter :: howvMax = 12
   integer(i_kind),parameter :: howvRatMiuSigma = 3
   integer(i_kind),parameter :: howvRathowvDpth = 2
   real(r_kind),parameter    :: howvDistm = 10000.0_r_kind
   logical,allocatable,dimension(:)::rthin,rusage
   logical save_all
!  integer(i_kind) numthin,numqc,numrem,numall
   integer(i_kind) nxdata,pmot

!
!   call init_constants_derived
   lhilbert = twodvar_regional .and. hilbert_curve
   namesat(1:nosat) = (/'NC031115','NC031122','NC031123','NC031127' &
                       ,'NC031114','NC031121','NC031120','NC031124' &
                       ,'NC031130'                                  /)

   satuse(1:nosat) = .true.
   tot = 0
   cnt = 0
   irec = 0
   cnt1 = 0
   nchanl=0
   nread = 0
   ilon=2
   ilat=3
   nrec = 0
!
   ithin=-9
   nc=zero
   conv: do i1=1,nconvtype
      if(trim(obstype) == trim(ioctype(i1)) .and. ictype(i1)==547) then
         nc=i1
         exit conv
      end if
   end do conv
   if(nc == 0)then
      write(6,*) myname,' no matching obstype found in convinfo ',obstype
      return
   end if
!
!  *#* Thinning *#*!
   use_all = .true.
   ithin=ithin_conv(nc)
   if (ithin > 0 ) then
     rmesh=rmesh_conv(nc)
     use_all = .false.
     nlevp=1   !Dummy for using make3grids
     allocate(DumForThin(nlevp)) !Dummy for using make3grids
     xmesh=rmesh
     call make3grids(xmesh,nlevp)
     write(6,'(A,1x,A,1x,A,I4,1x,f8.2,1x,I3,1x,I3)')myname,': ioctype(nc),ictype(nc),rmesh,nlevp,nc ',&
                 trim(ioctype(nc)),ictype(nc),rmesh,nlevp,nc
   endif
!
!  *#* Main - Start *#*!
   open(lun11,file=trim(infile),action='read',form='unformatted', iostat=ierr)
   if (ierr/=0) then
      print*, myname,' : ERROR : File ', trim(infile),' not existing. '
      return
   end if
!
   call openbf(lun11,'IN',lun11)
!
!  Counting all the data
   do while(ireadmg(lun11,subset,idate) == 0)
      do while (ireadsb(lun11) == 0)
         cnt = cnt+1
         if(cnt == 1) call time_4dvar(idate,toff)
      end do   
   end do
   call closbf(lun11)
   close(lun11)
!
! Allocate Arrays for all the data
   allocate (data_all (nreal, cnt),rusage(cnt),rthin(cnt))
!
!  Loop over file
   open(lun11,file=trim(infile),action='read',form='unformatted')
   call openbf(lun11,'IN',lun11)
   call datelen(dtLen)
   pmot=nint(pmot_conv(nc))
   if(pmot < 2 .and. reduce_diag)pmot=pmot+2
   save_all=.false.
   if(pmot /= 2 .and. pmot /= 0) save_all=.true.
   rusage = .true.
   rthin = .false.
   use_all=.true.
!
   read_msg: do while(ireadmg(lun11,subset,idate) == 0)
      do i1 = 1,nosat
         if (index(trim(subset),trim(namesat(i1))) > 0) then
            indsat=i1
            exit   
         end if 
      end do
      if ( .not.satuse(indsat) ) cycle
! 
!     Read through each record
      read_loop: do while (ireadsb(lun11) == 0)
         nrec = nrec + 1
!
         time_1d  = zero
         howv_1d  = zero
         loc_1d   = zero
         depth    = -99999.0_r_kind
!
         if (     (index(trim(subset),trim(namesat(1))) > 0)      &              !JS2
             .or. (index(trim(subset),trim(namesat(4))) > 0)      ) then         !JS4
!            sis = namesat(1)
            call ufbint(lun11,flt_1dJS2,n_fltJS2,1,irec,hdr_fltJS2)
!  Qc flags
            if (  (flt_1dJS2(1)>1.0_r_double)  .or. &
                  (flt_1dJS2(2)/=0.0_r_double)  .or. &
                  (flt_1dJS2(3)/=0.0_r_double)  .or. &
                  (flt_1dJS2(4)/=0.0_r_double)  .or. &
                  (flt_1dJS2(5)/=0.0_r_double)  .or. &
                  (flt_1dJS2(6)/=0.0_r_double)       )    cycle
             
            depth = abs(flt_1dJS2(7))
!  Time
            call ufbint(lun11,time_1d,n_tm,1,irec,hdr_time)
!  Howv
            call ufbint(lun11,howv_1d,n_howv,1,irec,hdr_howvJS2)
            hdr_loc   = 'CLATH CLONH'
!
         else if   (index(trim(subset),trim(namesat(2)))>0) then                 !SRLTK
!            sis = namesat(2)           
            call ufbint(lun11,flt_1dSAL,n_fltSAL,1,irec,hdr_fltSAL)
            if ( (flt_1dSAL(1)>1.0_r_double ) .or.   &
                 (flt_1dSAL(2)/=0.0_r_double) .or.   &
                 (flt_1dSAL(3)<=30.0_r_double) .or.   &
                 (flt_1dSAL(4)/=0.0_r_double )        ) cycle
           
            depth = abs(flt_1dSAL(5))
!  Time
            call ufbint(lun11,time_1d,n_tm,1,irec,hdr_time)
!  Howv
            call ufbint(lun11,howv_1d,n_howv,1,irec,hdr_howvSAL)
!
            hdr_loc   = 'CLATH CLONH'
         else if   (index(trim(subset),trim(namesat(3)))>0) then                 !CS2
!            sis=namesat(3)
            call ufbint(lun11,flt_1dCS2,n_fltCS2,1,irec,hdr_fltCS2)
            if ( (flt_1dCS2(1) > 1.0_r_double    ) .or.   &
                 (flt_1dCS2(2) > 0.0_r_double    ) .or.   &
                 (flt_1dCS2(3) < 90.0_r_double   ) .or.   &
                 (flt_1dCS2(4) /= 0.0_r_double   ) .or.   &
                 (flt_1dCS2(5) /= 0.0_r_double   )        ) cycle
!
            depth = abs(flt_1dCS2(2))
!  Time
            call ufbint(lun11,time_1d,n_tm,1,irec,hdr_timeCS2)
!  Howv
            call ufbint(lun11,howv_1d,n_howv,1,irec,hdr_howvCS2)
            howv_1d(2)=zero
!
            hdr_loc   = 'CLATH CLONH'
          else if(  (index(trim(subset),trim(namesat(5))) > 0)  &   
                .or.(index(trim(subset),trim(namesat(6))) > 0)  &
                .or.(index(trim(subset),trim(namesat(7))) > 0)  &
                .or.(index(trim(subset),trim(namesat(8))) > 0)  &
                .or.(index(trim(subset),trim(namesat(9))) > 0)  ) then       !NO
!  Time 
            call ufbint(lun11,time_1d,n_tm,1,irec,hdr_timeCS2)
!  Howv
            call ufbint(lun11,howv_1d,n_howv,1,irec,hdr_howvNO)
!
            hdr_loc   = 'CLAT CLON  '         
         end if
!  Temporal space
         time_1dMN = int(time_1d(1:5))
         call w3fs21(time_1dMN, nmind)
!
         rminobs=real(nmind,r_double)+(real(time_1d(6),r_double)*r60inv)
!
         t4dv = (rminobs-real(iwinbgn,r_kind))*r60inv
         tdiff=(rminobs-gstime)*r60inv
!
         if (l4dvar.or.l4densvar) then
            if (t4dv<zero .OR. t4dv>winlen) cycle
         else
            if (abs(tdiff) > ctwind(nc) .or. (abs(tdiff) > twind) )then
               cycle
            end if
         endif
!
!  Physical Space
         call ufbint(lun11,loc_1d,n_lc,1,irec,hdr_loc)
         if(abs(loc_1d(1))>r90 .or. abs(loc_1d(2))>360.0_r_kind) cycle
!
         if (loc_1d(2)>=360.0) loc_1d(2)=loc_1d(2)-360.0_r_kind
         if (loc_1d(2)< zero) loc_1d(2)=loc_1d(2)+360.0_r_kind
!       
         dlon_earth=loc_1d(2)*deg2rad
         dlat_earth=loc_1d(1)*deg2rad
         nread = nread + 1
!         
         if(regional)then                                            ! Regional

            call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)     ! *Convert to rotated coordinate
            if(outside) cycle

         else                                                        ! Global case
            dlon=dlon_earth
            dlat=dlat_earth
            call grdcrd1(dlat,rlats,nlat,1)
            call grdcrd1(dlon,rlons,nlon,1)
         endif
!  Slayer
         call datesec(time_1d, nsec)
         if (howv_1d(1)/=howv_1d(1)                                  ) cycle  !qc0
         if ( howv_1d(1)              > howvMax                      ) cycle  !qc1
         if ((howv_1d(2)              > zero)            .and.       &
             (howv_1d(1) / howv_1d(2) < howvRatMiuSigma)             ) cycle  !qc2
         if ((depth<=-99998.0_r_kind)    .and.       &
             (howv_1d(1) / depth)     > howvRathowvDpth              ) cycle  !qc3
! Next lines need update. #ww3
!         if (tot==one) then
!            nsec_m1 = nsec
!            loc_1d_m1 = loc_1d
!            howv_1d_m1 = howv_1d(1)
!         else if(tot>one)then
!            dt_sec = nsec-nsec_m1
!            call lldistm(loc_1d,loc_1d_m1, Hdistm, Pdistm)
!            if ( (abs(Hdistm)<howvDistm).and.(dt_sec<two) ) then
!               if (abs(howv_1d(1)-howv_1d_m1(1)) > hs_max_diff             ) cycle  !qc4
!               if (abs(howv_1d(1)-howv_1d_m1(1))/(dt_sec)**two > grav/two  ) cycle  !qc5
!            end if
!            nsec_m1 = nsec
!            loc_1d_m1 = loc_1d
!            howv_1d_m1 = howv_1d(1)
!         end if
         tot = tot+1
!
!  Dragon on Diet
         cnt = 0
         iuse=icuse(nc)
         if (ithin > 0 .and. iuse >=0) then
            if (thin4d) then
               timedif = zero                ! crit1=0.01_r_kind
            else
               timedif=abs(t4dv-toff)        ! timedif = 6.0_r_kind*abs(tdiff) & crit1=0.01_r_kind+timedif
            end if
            crit1 = timedif/r6+half
!
            call map3grids_m(-1,save_all,0,DumForThin,nlevp, &
                dlat_earth,dlon_earth,one,crit1,ndata,&
                luse,cnt,rthin,.false.,.false.)

               if (.not. luse) cycle
         else  ! - no thinnning
               ndata=ndata+1
         endif
         iout=ndata
!
         usage = zero !-  Set usage variable :: practically useless
         if (howv_1d(2)<=tiny_r_kind) howv_1d(2)=dflt_err
!        
!        call ufbint(lun11,c_station_id,1,1,irec,hdr_station)
!         c_station_id='SATMAR'
         c_station_id=trim(subset)
!
         if (index(trim(subset),trim(namesat(1))) > 0) satuse(5)=.false.
         if (index(trim(subset),trim(namesat(2))) > 0) satuse(6)=.false.
         if (index(trim(subset),trim(namesat(3))) > 0) satuse(7)=.false.
         if (index(trim(subset),trim(namesat(4))) > 0) satuse(8)=.false.
         if (index(trim(subset),trim(namesat(5))) > 0) satuse(1)=.false.
         if (index(trim(subset),trim(namesat(6))) > 0) satuse(2)=.false.
         if (index(trim(subset),trim(namesat(7))) > 0) satuse(3)=.false.
         if (index(trim(subset),trim(namesat(8))) > 0) satuse(4)=.false.

         data_all(1,iout) = howv_1d(2)                 ! significant wave height error (m)
         data_all(2,iout) = dlon                       ! grid relative longitude
         data_all(3,iout) = dlat                       ! grid relative latitude
         data_all(4,iout) = zero                       ! pressure (in cb)
         data_all(5,iout) = howv_1d(1)                 ! significant wave height (in m)
         data_all(6,iout) = rstation_id                ! station id
         data_all(7,iout) = t4dv                       ! time
         data_all(8,iout) = nc                         ! type
         data_all(9,iout) = 0_r_kind                   ! quality mark
         data_all(10,iout) = 0.2_r_kind                ! original obs error (m)
         data_all(11,iout) = usage                     ! usage parameter
         if (lhilbert) thisobtype_usage=11             ! save INDEX of where usage is stored for hilbertcurve cross validation (if requested)
         data_all(12,iout) = zero                      ! dominate surface type
         data_all(13,iout) = 295_r_kind                ! skin temperature
         data_all(14,iout) = 1.0                         ! 10 meter wind factor
         data_all(15,iout) = bmiss                     ! surface roughness
         data_all(16,iout) = dlon_earth*rad2deg        ! earth relative longitude (degrees)
         data_all(17,iout) = dlat_earth*rad2deg        ! earth relative latitude (degrees)
         data_all(18,iout) = zero                      ! station elevation (m)
         data_all(19,iout) = zero                      ! observation height (m)
         data_all(20,iout) = -depth                    ! terrain height at ob location
         data_all(21,iout) = 100000000000.000_r_kind    ! provider name !r_prvstg(1,1)
         data_all(22,iout) = 100000000000.000_r_kind    ! subprovider name !r_sprvstg(1,1)
         data_all(23,iout) = 0_r_kind                     ! cat
!
      enddo read_loop
   enddo read_msg
   call closbf(lun11)
   ! Write header record and data to output file for further processing
!
   nxdata=ndata
   ndata=0
   if(nxdata > 0)then
!     numthin=0
!     numqc=0
!     numrem=0
!     do i=1,nxdata
!        if(.not. rusage(i))then
!           numqc=numqc+1
!        else if(rthin(i))then
!           numthin=numthin+1
!        else
!           numrem=numrem+1
!        end if
!     end do
!     write(6,*) ' smar ',trim(ioctype(nc)),ictype(nc),icsubtype(nc),numall,numrem,numqc,numthin
!   If thinned data set usage
      do i=1,nxdata
         if(rthin(i))data_all(11,i)=100._r_kind
      end do
!     If flag to not save thinned data is set - compress data
      if(pmot /= 1)then
         do i=1,nxdata
!         pmot=0 - all obs - thin obs
!         pmot=1 - all obs
!         pmot=2 - use obs
!         pmot=3 - use obs + thin obs
            if((pmot == 0 .and. .not. rthin(i)) .or. &
               (pmot == 2 .and. (rusage(i) .and. .not. rthin(i)))  .or. &
               (pmot == 3 .and. rusage(i))) then

               ndata=ndata+1
               if(i > ndata)then
                  do k=1,nreal
                     data_all(k,ndata)=data_all(k,i)
                  end do
               end if
            end if
         end do
      end if
      nodata=nodata+ndata
   end if

! Deallocate local arrays
   if (ithin > 0 ) then
      deallocate(DumForThin)
      call del3grids
   end if
 
   call count_obs(ndata,nreal,ilat,ilon,data_all,nobs)
 
   write(lunout) obstype,sis,nreal,nchanl,ilat,ilon,ndata
   write(lunout) ((data_all(k,i1),k=1,nreal),i1=1,ndata)
   deallocate(data_all,rusage,rthin)
 
   if (ndata == 0) then
      write(6,*)myname,':  closbf(',lun11,') no data'
   endif
   close(lun11)
!
!
end subroutine read_satmar
!
! ###
subroutine lldistm(latlon1,latlon2, Hdistm, Pdistm)
! *#* Documentation block - Start *#*
! => Subroutine lldistm : Calculates the distance in m between two points in
! spherical coordinates (Earth in this case).
! => Coder              : stelios flampouris (stylianos.flampouris@noaa.gov)
! => Abstract           : Fortran code for Haversine and Pythagorian distance
!
! => History log        :
! 2016.03.08            : stelios - Prototype
! 2016.08.24            : stelios - Fully compatible with GSI
!
! => Input Arguments    : latlon1 latitude & longtitude of point 1
!                       : latlon1 latitude & longtitude of point 2
! => Output Arguments   : Hdistm Haversine distance between the point1 and point2 in meters(m)
!                       : Pdistm Pythagorian distance between the point1 and point2 in meters(m)
! => Attributes         : Machine Theia
!
! *#* Documentation block - End *#*
   use kinds, only: r_kind,r_double,i_kind
   use constants, only: one,deg2rad,rearth
   implicit none
   real(r_kind), dimension(2), intent(in) :: latlon1, latlon2
   real(r_kind), intent(out):: Hdistm, Pdistm
!local variables
   real(r_kind) :: lat1, lat2, lon1, lon2, dLat, dLon, dum1, dum2, x, y
!   integer(i_kind), parameter :: rearth=6371000 !(m)
   !
   lat1=latlon1(1)*deg2rad
   lat2=latlon2(1)*deg2rad;
   lon1=latlon1(2)*deg2rad;
   lon2=latlon2(2)*deg2rad;
   dLat=lat2-lat1;
   dLon=lon2-lon1;
!Haversine distance
   dum1=sin((dLat)/2)**2 + cos(lat1)*cos(lat2) * sin(dLon/2)**2;
   dum2=2*atan2(sqrt(dum1),sqrt(1-dum1))
   Hdistm=rearth*dum2
!Pythagoran distance
   x=dLon*cos((lat1+lat2)/2);
   y=dLat;
   Pdistm=rearth*sqrt(x*x + y*y);
!   
end subroutine lldistm
!
!
!
!#### subroutine datesec(idate, nsec)
subroutine datesec(idate, nsec)
! *#* Documentation block - Start *#*
! => Subroutine datenum : Calculates the number of seconds since 00:00:00,
!    1 January 1978 (RefDate : in days)
! => Coder              : stelios flampouris (stylianos.flampouris@noaa.gov)
! => Abstract/FlowChart :
!
! [YYYY,MM,DD]-->Convert to Julian Days (JD)-->NDays=JD-Refdate-->Convert to
! Seconds (NDaysInSec)--> nsec=NDaysInSec+HH*3600+MN*60+SS
!
! => History log        :
! 2016.03.08            : stelios flampouris
!
! => Input Arguments    : idate real array with size 6:
!                         idate(1)=YYYY   !Year
!                         idate(2)=MM     !Month
!                         idate(3)=DD     !Day
!                         idate(4)=HH     !Hour
!                         idate(5)=MN     !Minute
!                         idate(6)=SS     !Second
!
! => Output Arguments   : nsec integer Number of seconds
!
! => Attributes         : Machine Theia
!
! *#* Documentation block - End *#*
!
   implicit none
   real(8), intent(in) :: idate(6)
   real(8), intent(out) :: nsec
!
! local parameters
   real(8), parameter :: ReFDate = 2443510.
   real(8) :: JD
!
! Initializing variables
   nsec = 0
   !
! Convert idate(1:3) to JD
   JD = idate(3) - 32075                                                       &
                 + 1461 * ( idate(1) + 4800 + (idate(2) - 14) / 12) / 4        &
                 +  367 * ( idate(2) - 2    - (idate(2) - 14) / 12 * 12) / 12  &
                 -    3 * ((idate(1) + 4900 + (idate(2) - 14) / 12) / 100) / 4
!
! Number of days from the reference days
   JD = JD - ReFDate
!
! Number of seconds
   nsec = (JD* 86400) + (idate(4) * 3600) + (idate(5) * 60) + idate(6)
!
end subroutine datesec
!
!
! ###
! Unused Code but Useful
!
!! ### Not necessary but implemented + All the Variables are Declared
!  ### Plug it in the "Regional Check"
!           if(diagnostic_reg) then
!              call txy2ll(dlon,dlat,dlon00,dlat00)
!              cnt1=cnt1+1
!              cdist=sin(dlat_earth)*sin(dlat00)+cos(dlat_earth)*cos(dlat00)* &
!                   (sin(dlon_earth)*sin(dlon00)+cos(dlon_earth)*cos(dlon00))
!              cdist=max(-one,min(cdist,one))
!              disterr=acos(cdist)*rad2deg
!              disterrmax=max(disterrmax,disterr)
!           end if
! ###
