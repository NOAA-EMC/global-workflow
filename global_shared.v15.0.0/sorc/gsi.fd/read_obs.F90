module read_obsmod
!$$$   module documentation block
!                .      .    .                                       .
! module:    read_obsmod extra inquire routine for reading obs
!   prgmmr: parrish          org: np22                date: 2005-06-06
!
! abstract:
!
! program history log:
!   2009-01-05  todling - add gsi_inquire
!   2015-05-01  Liu Ling - Add call to read_rapidscat 
!
! subroutines included:
!   sub gsi_inquire   -  inquire statement supporting fortran earlier than 2003
!   sub read_obs      -  read, select, reformat obs data
!
! Variable Definitions:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$ end documentation block

! set default to private
  private
! set subroutines to public
  public :: gsi_inquire
  public :: read_obs

contains

subroutine gsi_inquire (lbytes,lexist,filename,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    gsi_inquire        inquire file presence and size
!   prgmmr: todling      org: np22                date: 2009-01-05
!
! abstract:  Inquire file presence and size; to be used when fortran
!            2003 not available or non-compliant.
!
! program history log:
!   2009-01-05  todling
!   2013-05-14  guo     - changed the compiler #ifdef dependency on _size_
!                         inquire to a more portable way.
!
!   input argument list:
!     mype     - mpi task id
!    filename  - input filename
!
!   output argument list:
!    lexist     - file presence flag
!    lbytes     - file size (bytes)
!
! attributes:
!   language: f90
!   machine:  Linux-cluster
!
!$$$  end documentation block

  use kinds, only: i_kind,i_llong

  implicit none

  integer(i_llong),intent(  out) :: lbytes
  logical         ,intent(  out) :: lexist
  character(len=*),intent(in   ) :: filename
  integer(i_kind) ,intent(in   ) :: mype

  character(len=256) command, fname

#if defined(__INTEL_COMPILER) && (__INTEL_COMPILER < 1110)
#define __X_OR_Y_OR_Z_FORTRAN_COMPILERS__
#endif

#ifdef __X_OR_Y_OR_Z_FORTRAN_COMPILERS__
#define _DO_NOT_SUPPORT_SIZE_INQUIRE_
#endif

  lbytes=-1  ! in case that _size_ specifier is not supported.
#ifndef _DO_NOT_SUPPORT_SIZE_INQUIRE_
  inquire(file=trim(filename),exist=lexist,size=lbytes)
  ! Note that the value of _size_ is defined by Fortran in "file storage units",
  ! which is not neccesary in byte units.  It is not clear if this code had
  ! assumed the size value to be in byte units, or in whatever units.
#else
  inquire(file=trim(filename),exist=lexist)
#endif
  if(lexist) then
     ! Even with a compiler supporting 'size=' specifier, the size value may
     ! return -1, if a compiler considers that the size can not be determined.
     ! In that case, the size may be obtained through a user supported
     ! mechanism, such as reading the size from a system("wc -c") call result.
     if(lbytes<0) then
        write(fname,'(2a,i4.4)') 'fsize_',trim(filename),mype
        write(command,'(4a)') 'wc -c ', trim(filename),' > ', trim(fname)
        call system(command)
        open(unit=999,file=trim(fname),form='formatted')
        read(999,*) lbytes
        close(999)
        lexist = lbytes>0_i_llong ! skip this file if lbytes <=0
     endif
  endif

  return
end subroutine gsi_inquire

subroutine read_obs_check (lexist,filename,jsatid,dtype,minuse,nread)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_obs_check     inquire file presence and size
!   prgmmr: todling      org: np22                date: 2010-03-05
!
! abstract:  Reset file status depending on whether observation time
!            matches analysis time and how offtime_date is set. This
!            also checks for consistency in satellite data files and 
!            known types.  
!            WARNING: some of it looks inconsistent with long-window 4dvar
!
! program history log:
!   2009-xx-xx  derber   - originally placed inside inquire
!   2009-01-05  todling  - move time/type-check out of inquire
!   2010-09-13  pagowski - add anow bufr and one obs chem
!   2013-01-26  parrish - WCOSS debug compile fails with satid not initialized.
!                         Set satid=1 at start of subroutine to allow debug compile.
!   2013-02-13  eliu     - add ssmis 
!   2013-07-01  todling/guo - allow user to bypass this check (old bufr support)
!   2014-10-01  ejones   - add gmi and amsr2
!   2015-01-16  ejones   - add saphir
!                           
!
!   input argument list:
!    lexist    - file status
!    filename  - input filename
!    jsatid    - satellite id
!    dtype     - satellite type
!
!   output argument list:
!    lexist    - file status
!
! attributes:
!   language: f90
!   machine:  Linux-cluster
!
!$$$  end documentation block

  use kinds, only: i_kind,i_llong,r_kind,r_double
  use gsi_4dvar, only: iadatebgn,iadateend
  use obsmod, only: offtime_data
  use convinfo, only: nconvtype,ictype,ioctype,icuse
  use chemmod, only : oneobtest_chem,oneob_type_chem,&
       code_pm25_ncbufr,code_pm25_anowbufr,code_pm10_ncbufr,code_pm10_anowbufr

  implicit none

  logical         ,intent(inout)  :: lexist
  character(len=*),intent(in)     :: filename
  character(len=*),intent(in)     :: jsatid
  character(len=*),intent(in)     :: dtype
  integer(i_kind) ,intent(in)     :: minuse
  integer(i_kind) ,intent(inout)  :: nread

  integer(i_kind) :: lnbufr,idate,idate2,iret,kidsat
  integer(i_kind) :: ireadsb,ireadmg,kx,nc,said
  real(r_double) :: satid,rtype
  character(8) subset

  satid=1      ! debug executable wants default value ???
  idate=0
#ifdef _SKIP_READ_OBS_CHECK_
  return
#endif
  if(trim(dtype) == 'tcp' .or. trim(filename) == 'tldplrso')return
  if(trim(filename) == 'mitmdat' .or. trim(filename) == 'mxtmdat')return
! Use routine as usual
  if(lexist)then
      lnbufr = 15
      open(lnbufr,file=trim(filename),form='unformatted',status ='unknown')
      call openbf(lnbufr,'IN',lnbufr)
      call datelen(10)
      call readmg(lnbufr,subset,idate,iret)
      if(iret == 0)then

!        Extract date and check for consistency with analysis date
         if (idate<iadatebgn.or.idate>iadateend) then
            if(offtime_data) then
              write(6,*)'***read_obs_check analysis and data file date differ, but use anyway'
            else
               write(6,*)'***read_obs_check*** ',&
                 'incompatable analysis and observation date/time',trim(filename),trim(dtype)
               lexist=.false.
            end if
            write(6,*)'Analysis start  :',iadatebgn
            write(6,*)'Analysis end    :',iadateend
            write(6,*)'Observation time:',idate
        endif
      else
         write(6,*)'***read_obs_check*** iret/=0 for reading date for ',trim(filename),dtype,jsatid,iret
         lexist=.false.
      end if
      if(lexist)then
       if(jsatid == '')then
         kidsat=0
       else if(jsatid == 'metop-a')then
         kidsat=4
       else if(jsatid == 'metop-b')then
         kidsat=3
       else if(jsatid == 'metop-c')then
         kidsat=5
       else if(jsatid == 'm08')then
         kidsat = 55 
       else if(jsatid == 'm09')then
         kidsat = 56 
       else if(jsatid == 'm10')then
         kidsat = 57 
       else if(jsatid == 'n08')then
         kidsat=200
       else if(jsatid == 'n09')then
         kidsat=201
       else if(jsatid == 'n10')then
         kidsat=202
       else if(jsatid == 'n11')then
         kidsat=203
       else if(jsatid == 'n12')then
         kidsat=204
       else if(jsatid == 'n14')then
         kidsat=205
       else if(jsatid == 'n15')then
         kidsat=206
       else if(jsatid == 'n16')then
         kidsat=207
       else if(jsatid == 'n17')then
         kidsat=208
       else if(jsatid == 'n18')then
         kidsat=209
       else if(jsatid == 'n19')then
         kidsat=223
       else if(jsatid == 'npp')then
         kidsat=224
       else if(jsatid == 'f08')then
         kidsat=241
       else if(jsatid == 'f10')then
         kidsat=243
       else if(jsatid == 'f11')then
         kidsat=244
       else if(jsatid == 'f13')then
         kidsat=246
       else if(jsatid == 'f14')then
         kidsat=247
       else if(jsatid == 'f15')then
         kidsat=248
       else if(jsatid == 'f16')then
         kidsat=249    
       else if(jsatid == 'trmm')then
         kidsat=282    
       else if(jsatid == 'f17')then
         kidsat=285                  
       else if(jsatid == 'f18')then  
         kidsat=286                  
       else if(jsatid == 'f19')then  
         kidsat=287                  
       else if(jsatid == 'g08' .or. jsatid == 'g08_prep')then
         kidsat=252
       else if(jsatid == 'g09' .or. jsatid == 'g09_prep')then
         kidsat=253
       else if(jsatid == 'g10' .or. jsatid == 'g10_prep')then
         kidsat=254
       else if(jsatid == 'g11' .or. jsatid == 'g11_prep')then
         kidsat=255
       else if(jsatid == 'g12' .or. jsatid == 'g12_prep')then
         kidsat=256
       else if(jsatid == 'g13' .or. jsatid == 'g13_prep')then
         kidsat=257
       else if(jsatid == 'g14' .or. jsatid == 'g14_prep')then
         kidsat=258
       else if(jsatid == 'g15' .or. jsatid == 'g15_prep')then
         kidsat=259
       else if(jsatid == 'n05')then
         kidsat=705
       else if(jsatid == 'n06')then
         kidsat=706
       else if(jsatid == 'n07')then
         kidsat=707
       else if(jsatid == 'tirosn')then
         kidsat=708
       else if ( jsatid == 'terra' ) then
         kidsat = 783
       else if ( jsatid == 'aqua'  ) then
         kidsat = 784
       else if ( jsatid == 'aura'  ) then
         kidsat = 785
       else if ( jsatid == 'gcom-w1' ) then
         kidsat = 122
! Temporary comment gpm out here; discrepancy between SAID in bufr file and
! kidsat.
!       else if ( jsatid == 'gpm' ) then
!         kidsat = 288
       else if ( jsatid == 'meghat' ) then
         kidsat = 440
       else
         kidsat = 0
       end if

       call closbf(lnbufr)
       open(lnbufr,file=trim(filename),form='unformatted',status ='unknown')
       call openbf(lnbufr,'IN',lnbufr)
       call datelen(10)

       if(kidsat /= 0)then
        lexist = .false.
        satloop: do while(ireadmg(lnbufr,subset,idate2) >= 0)
           if(ireadsb(lnbufr)==0)then
              call ufbint(lnbufr,satid,1,1,iret,'SAID')
           end if
           if(nint(satid) == kidsat) then
             lexist=.true.
             exit satloop
           end if
           nread = nread + 1
        end do satloop
       else if(trim(filename) == 'prepbufr')then  ! RTod: wired-in filename is not a good idea
         lexist = .false.
         fileloop: do while(ireadmg(lnbufr,subset,idate2) >= 0)
          do while(ireadsb(lnbufr)>=0)
           call ufbint(lnbufr,rtype,1,1,iret,'TYP')
           kx=nint(rtype)
           do nc=1,nconvtype
             if(trim(ioctype(nc)) == trim(dtype) .and. kx == ictype(nc) .and. icuse(nc) > minuse)then
               lexist = .true.
               exit fileloop
             end if
           end do
          end do 
          nread = nread + 1
         end do fileloop
       else if(trim(filename) == 'gps_ref' .or.  trim(filename) == 'gps_bnd')then
         lexist = .false.
         gpsloop: do while(ireadmg(lnbufr,subset,idate2) >= 0)
           if(ireadsb(lnbufr)==0)then 
              call ufbint(lnbufr,satid,1,1,iret,'SAID') 
           end if 
 
           said=nint(satid) 
           if(((said > 739) .and.(said < 746)).or.(said == 820) .or. &
               (said == 786).or. (said == 4)  .or.(said == 3).or. &
               (said == 421).or. (said == 440).or.(said == 821)) then
             lexist=.true. 
             exit gpsloop 
           end if 
           nread = nread + 1
         end do gpsloop
       else if(trim(filename) == 'prepbufr_profl')then  
         lexist = .false.
         airploop: do while(ireadmg(lnbufr,subset,idate2) >= 0)
          do while(ireadsb(lnbufr)>=0)
           call ufbint(lnbufr,rtype,1,1,iret,'TYP')
           kx=nint(rtype)
           if (trim(dtype)=='uv') then
              if (kx==330 .or. kx==430 .or. kx==530) kx=230
              if (kx==331 .or. kx==431 .or. kx==531) kx=231
              if (kx==332 .or. kx==432 .or. kx==532) kx=232
              if (kx==333 .or. kx==433 .or. kx==533) kx=233
              if (kx==334 .or. kx==434 .or. kx==534) kx=234
              if (kx==335 .or. kx==435 .or. kx==535) kx=235
           else
              if (kx==330 .or. kx==430 .or. kx==530) kx=130
              if (kx==331 .or. kx==431 .or. kx==531) kx=131
              if (kx==332 .or. kx==432 .or. kx==532) kx=132
              if (kx==333 .or. kx==433 .or. kx==533) kx=133
              if (kx==334 .or. kx==434 .or. kx==534) kx=134
              if (kx==335 .or. kx==435 .or. kx==535) kx=135
           end if
           do nc=1,nconvtype
             if(trim(ioctype(nc)) == trim(dtype) .and. kx == ictype(nc) .and. icuse(nc) > minuse)then
               lexist = .true.
               exit airploop
             end if
           end do
          end do
          nread = nread + 1
         end do airploop
       else if(trim(filename) == 'satwndbufr')then
         lexist = .false.
         loop: do while(ireadmg(lnbufr,subset,idate2) >= 0)
            if(trim(subset) == 'NC005010' .or. trim(subset) == 'NC005011' .or.&
               trim(subset) == 'NC005070' .or. trim(subset) == 'NC005071' .or.&
               trim(subset) == 'NC005044' .or. trim(subset) == 'NC005045' .or.&
               trim(subset) == 'NC005046' .or. trim(subset) == 'NC005064' .or.&
               trim(subset) == 'NC005065' .or. trim(subset) == 'NC005066') then 
               lexist = .true.
               exit loop
            endif
            nread = nread + 1
         end do loop
       else if(trim(filename) == 'oscatbufr')then
         lexist = .false.
         oscatloop: do while(ireadmg(lnbufr,subset,idate2) >= 0)
            if(trim(subset) == 'NC012255') then
               lexist = .true.
               exit oscatloop
            endif
         end do oscatloop
       else if(trim(filename) == 'rapidscatbufr')then
         lexist = .false.
         rapidscatloop: do while(ireadmg(lnbufr,subset,idate2) >= 0)
            if(trim(subset) == 'NC012255') then
               lexist = .true.
               exit rapidscatloop
            endif
            nread = nread + 1
         end do rapidscatloop
       else if(trim(filename) == 'hdobbufr')then
         lexist = .false.
         loop_hdob: do while(ireadmg(lnbufr,subset,idate2) >= 0)
            if(trim(subset) == 'NC004015') then
               lexist = .true.
               exit loop_hdob
            endif
            nread = nread + 1
         end do loop_hdob
       else if(trim(dtype) == 'pm2_5')then
          if (oneobtest_chem .and. oneob_type_chem=='pm2_5') then
             lexist=.true.
          else
             lexist = .false.
             fileloopanow_pm2_5:do while(ireadmg(lnbufr,subset,idate2) >= 0)
                do while(ireadsb(lnbufr)>=0)
                   if (subset == 'ANOWPM') then
                      call ufbint(lnbufr,rtype,1,1,iret,'TYP')
                      kx=nint(rtype)
                   else if ( (subset == 'NC008031') .or. &
                          (subset == 'NC008032' ) ) then
                      call ufbint(lnbufr,rtype,1,1,iret,'TYPO')
                      kx=nint(rtype)
                      if (kx/=code_pm25_ncbufr) then
                         cycle
                      else
                         kx=code_pm25_anowbufr
                      endif
                   else
                      cycle
                   endif
                   
                   do nc=1,nconvtype
                      if(trim(ioctype(nc)) == trim(dtype) .and. &
                           kx == ictype(nc) .and. icuse(nc) > minuse)then
                         lexist = .true.
                         exit fileloopanow_pm2_5
                      end if
                   end do
                end do
                nread = nread + 1
             enddo fileloopanow_pm2_5
          endif

          if (lexist) then
             write(6,*)'found pm2_5 in anow bufr'
          else
             write(6,*)'did not find pm2_5 in anow bufr'
          endif
           
       else if(trim(dtype) == 'pm10')then
          lexist = .false.
          fileloopanow_pm10:do while(ireadmg(lnbufr,subset,idate2) >= 0)
             do while(ireadsb(lnbufr)>=0)
                if (subset == 'NC008033') then
                   call ufbint(lnbufr,rtype,1,1,iret,'TYPO')
                   kx=nint(rtype)
                   IF (kx/=code_pm10_ncbufr) then
                      cycle
                   else
                      kx=code_pm10_anowbufr
                   endif
                else
                   cycle
                endif

                do nc=1,nconvtype
                   if(trim(ioctype(nc)) == trim(dtype) .and. &
                        kx == ictype(nc) .and. icuse(nc) > minuse)then
                      lexist = .true.
                      exit fileloopanow_pm10
                   end if
                end do
             end do
             nread = nread + 1
          enddo fileloopanow_pm10

          if (lexist) then
             write(6,*)'found pm10 in anow bufr'
          else
             write(6,*)'did not find pm10 in anow bufr'
          endif


       end if
      end if

      call closbf(lnbufr)
  end if
  if(lexist)then
      write(6,*)'read_obs_check: bufr file date is ',idate,trim(filename),' ',dtype,jsatid
  else
      write(6,*)'read_obs_check: bufr file ',dtype,jsatid,' not available ',trim(filename)
  end if
  return
end subroutine read_obs_check

subroutine read_obs(ndata,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    read_obs              read, select, reformat obs data
!   prgmmr: parrish          org: np22                date: 1990-10-07
!
! abstract:  This routine is a driver for routines which read different
!            types of observational data.
!
! program history log:
!   1990-10-07  parrish
!   1998-05-15  weiyu yang 
!   1999-08-24  derber, j., treadon, r., yang, w., first frozen mpp version
!   2004-06-16  treadon - update documentation
!   2004-07-23  derber - modify to include conventional sst
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2005-01-20  okamoto - add calling read_ssmi
!   2005-06-14  wu      - add OMI oz
!   2005-07-06  derber - add mhs, hirs/4 and ears data 
!   2005-08-02  derber - modify to use convinfo file
!   2005-09-08  derber - modify to use input group time window
!   2005-09-20  xu & pawlak - modify calling read_ssmis and read_amsre
!   2005-09-28  derber - modify to simplify obs handling      
!   2005-10-17  derber - pass obs_load1 into read_amsre and read_ssmis 
!   2005-10-18  treadon - remove obs_load and obs_load1
!   2005-10-20  kazumori - modify to read real AMSR-E data
!   2005-11-28  derber move determination of which ob data sets to read inside read_obs
!   2005-11-14  li, xu - modify sst obs read and add avhrr gac 1b obs read
!   2006-01-25  treadon - remove read_ieeetovs
!   2006-02-01  parrish - add getsfc and destroy_sfc for full surface fields
!   2006-02-03  derber  - modify for new obs control and obs count
!   2005-02-03  treadon - gather guess 3d pressure to full grid array
!   2006-02-08  derber  - modify to use new convinfo module
!   2006-02-01  liu - add ssu
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-03-07  derber - consolidate processing of 1x1 and 5x5 goessndr
!   2006-04-20  kistler - moved conv_read to gsisub
!   2006-05-25  treadon - rename goesimg and goes_img and pcp_ssm/i as
!                         pcp_ssmi to make consistent with other obstype
!   2006-09-20  treadon - add mpi_io for select data file (obstype)s
!   2006-10-12  treadon - remove tendsflag check for pcp data (now in gsimain)
!   2007-02-21  sienkiewicz - bring in changes for MLS ozone
!   2007-03-15       su - add reading conventional error table option
!   2007-06-05  treadon - restructure mpi_querybf section to improve efficiency
!   2007-10-03  todling - skip most of this in 4dvar inner loop
!   2008-03-28       wu - move random seed for perturb_obs from setuprhsall
!   2008-04-18  safford - rm unused vars and uses
!   2008-05-01    h.liu - add gome ozone
!   2008-06-20   derber - move destroy_sfc to this routine 
!   2008-09-08   lueken - merged ed''s cahnges into q1fy09 code
!   2008-12-30  todling - handle inquire for diff versions of fortran
!   2009-01-05  todling - need tendency alloc in observer mode
!   2009-01-23  todling - echo surface state info 
!   2009-03-18  meunier - add a if statement to read lagrangian data
!   2009-08-19  guo     - moved destroy_sfc_grid() to observer_finalize().
!   2009-12-20  gayno - modify argument lists so that fov-based surface
!                       calculation may be used.
!   2010-03-29  hu    - add code to read in cloud observations  including:
!                            prepbufr (metar, nesdis cloud product)
!                            radar reflectivity, lightning, NASA LaRC cloud
!   2010-04-01  treadon - move strip and reorder to gridmod
!   2010-04-08  hliu - add seviri
!
!   2010-04-05  huang   - add aero and modis for reading modis aod from satellite
!                         currently read BUFR file only
!   2010-04-22  tangborn - read for carbon monoxide
!   2010-08-23  tong    - add calcuation of hgtl_full used in read_radar.f90
!   2011-04-02  li       - add nst_gsi, getnst and destroy_nst
!   2011-05-20  mccarty  - add cris/atms handling
!   2011-05-26  todling  - add call to create_nst
!   2012-03-07  akella   - set NST variables; init & destroy in guess_grids 
!                          [see create_sfc_grids & destroy_sfc_grids] 
!   2012-05-16  todling  - protect call to prebufr when NST is on (should be mods)
!   2012-09-08  wargan   - add OMI with efficiency factors
!   2013-01-26  parrish - WCOSS debug compile fails--extra arguments in call read_aerosol.
!                         Commented out extra line of arguments not used.
!   2013-02-13  eliu     - turn off parallel I/O for SSMIS (due to the need to
!                          do spatial averaging for noise reduction) 
!   2013-06-01  zhu     - add mype_airobst to handle aircraft temperature bias correction 
!   2013-08-08  s.liu     - add read NASA_LaRC_cloud product
!   2013-10-25  todling - reposition ltosi and others to commvars
!   2014-01-01  xli     - add option to read NSST marine BUFR data file nsstbufr (on the
!                         top of prepbufr and modsbufr)
!   2014-02-03  guo     - Hid processing (read) of non-EMC ozone obstypes
!                         through module m_extOzone, separated from read_ozone.
!                       - Added some -do- and -if- construct names, for easier
!                         understanding of the code.
!   2014-06-19  carley/zhu - Add tcamt and lcbas
!   2014-11-12  carley  - Add call to read goes imager sky cover data for tcamt
!   2014-12-03  derber - modify for no radiance cases and read processor for
!                        surface fields
!   2015-01-16  ejones  - added saphir, gmi, and amsr2 handling
!   2015-03-23  zaizhong ma - add Himawari-8 ahi
!   2015-05-30  li     - modify for no radiance cases but sst (nsstbufr) and read processor for
!                        surface fields (use_sfc = .true. for data type of sst),
!                        to use deter_sfc in read_nsstbufr.f90)
!   2015-07-10  pondeca - add cloud ceiling height (cldch)
!   2015-08-12  pondeca - add capability to read min/maxT obs from ascii file
!   2015-09-04  J. Jung - Added mods for CrIS full spectral resolution (FSR)
!   2016-03-02  s.liu/carley - remove use_reflectivity and use i_gsdcldanal_type
!   2016-04-28  J. Jung - added logic for RARS and direct broadcast data from NESDIS/UW.
!   
!
!   input argument list:
!     mype     - mpi task id
!
!   output argument list:
!     ndata(*,1)- number of prefiles retained for further processing
!     ndata(*,2)- number of observations read
!     ndata(*,3)- number of observations keep after read
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$  end documentation block
    use kinds, only: r_kind,i_kind,i_llong,r_double
    use gridmod, only: nlon,nlat,nsig,iglobal,ijn,itotsub,lat1,lon1,&
         displs_g,strip,reorder
    use general_commvars_mod, only: ltosi,ltosj
    use obsmod, only: iadate,ndat,time_window,dplat,dsfcalc,dfile,dthin, &
           dtype,dval,dmesh,obsfile_all,ref_obs,nprof_gps,dsis,ditype,&
           oberrflg,perturb_obs,lobserver,lread_obs_save,obs_input_common, &
           reduce_diag,nobs_sub,dval_use
    use qcmod, only: njqc
    use gsi_4dvar, only: l4dvar
    use satthin, only: super_val,super_val1,superp,makegvals,getsfc,destroy_sfc
    use mpimod, only: ierror,mpi_comm_world,mpi_sum,mpi_rtype,mpi_integer,npe,&
         setcomm
    use constants, only: one,zero
    use converr, only: converr_read
    use converr_ps, only: converr_ps_read
    use converr_q, only: converr_q_read
    use converr_t, only: converr_t_read
    use converr_uv, only: converr_uv_read
    use converr_pw, only: converr_pw_read
    use convb_ps,only: convb_ps_read
    use convb_q,only:convb_q_read
    use convb_t,only:convb_t_read
    use convb_uv,only:convb_uv_read
    use guess_grids, only: ges_prsl,geop_hgtl,ntguessig
    use radinfo, only: nusis,iuse_rad,jpch_rad,diag_rad
    use insitu_info, only: mbuoy_info,read_ship_info
    use aeroinfo, only: nusis_aero,iuse_aero,jpch_aero,diag_aero
    use ozinfo, only: nusis_oz,iuse_oz,jpch_oz,diag_ozone
    use pcpinfo, only: npcptype,nupcp,iusep,diag_pcp
    use convinfo, only: nconvtype,ioctype,icuse,diag_conv,ithin_conv
    use chemmod, only : oneobtest_chem,oneob_type_chem,oneobschem
    use aircraftinfo, only: aircraft_t_bc,aircraft_t_bc_pof,aircraft_t_bc_ext,mype_airobst
    use gsi_nstcouplermod, only: nst_gsi
    use gsi_nstcouplermod, only: gsi_nstcoupler_set,gsi_nstcoupler_final
    use gsi_io, only: mype_io
    use rapidrefresh_cldsurf_mod, only: i_gsdcldanal_type

    use m_extOzone, only: is_extOzone
    use m_extOzone, only: extOzone_read
    implicit none

!   Declare passed variables
    integer(i_kind)                  ,intent(in   ) :: mype
    integer(i_kind),dimension(ndat,3),intent(  out) :: ndata

!   Declare local parameters
    integer(i_llong),parameter:: lenbuf=8388608_i_llong  ! lenbuf=8*1024*1024

!   Declare local variables
    logical :: lexist,ssmis,amsre,sndr,hirs,avhrr,lexistears,lexistdb,use_prsl_full,use_hgtl_full
    logical :: use_sfc,nuse,use_prsl_full_proc,use_hgtl_full_proc,seviri,mls
    logical,dimension(ndat):: belong,parallel_read,ears_possible,db_possible
    logical :: modis,use_sfc_any
    logical :: acft_profl_file
    character(10):: obstype,platid
    character(22):: string
    character(15):: infile
    character(20):: sis
    integer(i_kind) i,j,k,ii,nmind,lunout,isfcalc,ithinx,ithin,nread,npuse,nouse
    integer(i_kind) nprof_gps1,npem1,krsize,len4file,npemax,ilarge,nlarge,npestart
    integer(i_llong) :: lenbytes
    integer(i_kind):: npetot,npeextra,mmdat
    integer(i_kind):: iworld,iworld_group,next_mype,mm1,iix
    integer(i_kind):: mype_root
    integer(i_kind):: minuse,lunsave,maxproc,minproc
    integer(i_kind),dimension(ndat):: npe_sub,npe_sub3,mpi_comm_sub,mype_root_sub,npe_order
    integer(i_kind),dimension(ndat):: ntasks1,ntasks
    integer(i_kind),dimension(ndat):: read_rec1,read_rec
    integer(i_kind),dimension(ndat):: read_ears_rec1,read_ears_rec
    integer(i_kind),dimension(ndat):: read_db_rec1,read_db_rec
    integer(i_kind),dimension(ndat,3):: ndata1
    integer(i_kind),dimension(npe,ndat):: mype_work,nobs_sub1
    integer(i_kind),dimension(npe,ndat):: mype_sub
    integer(i_kind),allocatable,dimension(:):: nrnd
    integer(i_kind):: nmls_type,mype_io_sfc
    integer(i_kind):: iread,ipuse,iouse

    real(r_kind) gstime,val_dat,rmesh,twind,rseed
    real(r_kind),allocatable,dimension(:) :: prslsm,hgtlsm,work1
    real(r_kind),allocatable,dimension(:,:,:):: prsl_full,hgtl_full

    data lunout / 81 /
    data lunsave  / 82 /

!*****************************************************************************

!   Set analysis time and allocate/initialize arrays and variables
    call w3fs21(iadate,nmind)
    gstime=real(nmind,r_kind)
    allocate(nobs_sub(npe,ndat))
    nobs_sub = 0
    nobs_sub1 = 0

    call makegvals
    do ii=1,ndat
       ndata1(ii,1)=0
       ndata1(ii,2)=0
       ndata1(ii,3)=0
       ntasks1(ii) =0
       parallel_read=.false.
    end do
    npem1=npe-1
    nprof_gps1=0

    if(njqc) then
       call converr_ps_read(mype)
       call converr_q_read(mype)
       call converr_t_read(mype)
       call converr_uv_read(mype)
       call converr_pw_read(mype)
       call convb_ps_read(mype)
       call convb_q_read(mype)
       call convb_t_read(mype)
       call convb_uv_read(mype)
    else
       call converr_read(mype)
    endif

!   Optionally set random seed to perturb observations
    if (perturb_obs) then
       rseed=iadate(4)+iadate(3)*100+iadate(2)*10000+iadate(1)*1000000+mype
       call random_seed(size=krsize)
       allocate(nrnd(krsize))
       do i=1,krsize
          nrnd(i)=rseed
       end do
       call random_seed(put=nrnd)
       deallocate(nrnd)
    endif



!   Set data class and number of reader tasks.  Set logical flag to indicate 
!   type type of GPS data (if present)
    ii=0
    ref_obs = .false.    !.false. = assimilate GPS bending angle
    ears_possible = .false.
    db_possible = .false.
    nmls_type=0
    read_rec1 = 0
    read_ears_rec1=0
    read_db_rec1=0
    do i=1,ndat
       obstype=dtype(i)                   !     obstype  - observation types to process
       amsre= index(obstype,'amsre') /= 0
       ssmis= index(obstype,'ssmis') /= 0
       sndr = index(obstype,'sndr') /= 0
       hirs = index(obstype,'hirs') /= 0
       avhrr = index(obstype,'avhrr') /= 0
       modis = index(obstype,'modis') /= 0
       seviri = index(obstype,'seviri') /= 0
       mls = index(obstype,'mls') /= 0
       if(obstype == 'mls20' ) nmls_type=nmls_type+1
       if(obstype == 'mls22' ) nmls_type=nmls_type+1
       if(obstype == 'mls30' ) nmls_type=nmls_type+1
       if(nmls_type>1) then
          write(6,*) '******ERROR***********: there is more than one MLS data type, not allowed, please check'
          call stop2(339)
       end if
       if (obstype == 't'  .or. obstype == 'uv' .or. &
           obstype == 'q'  .or. obstype == 'ps' .or. &
           obstype == 'pw' .or. obstype == 'spd'.or. &
           obstype == 'sst'.or. obstype == 'srw'.or. &
           obstype == 'tcp'.or. obstype == "lag".or. &
           obstype == 'dw' .or. obstype == 'rw' .or. &
           obstype == 'mta_cld' .or. obstype == 'gos_ctp' .or. &
           obstype == 'rad_ref' .or. obstype=='lghtn' .or. &
           obstype == 'larccld' .or. obstype == 'pm2_5' .or. obstype == 'pm10' .or. &
           obstype == 'gust' .or. obstype=='vis' .or. &
           obstype == 'pblh' .or. obstype=='wspd10m' .or. &
           obstype == 'td2m' .or. obstype=='mxtm' .or. &
           obstype == 'mitm' .or. obstype=='pmsl' .or. &
           obstype == 'howv' .or. obstype=='tcamt' .or. &
           obstype=='lcbas' .or. obstype=='cldch') then
          ditype(i) = 'conv'
       else if( hirs   .or. sndr      .or.  seviri .or. &
               obstype == 'airs'      .or. obstype == 'amsua'     .or.  &
               obstype == 'msu'       .or. obstype == 'iasi'      .or.  &
               obstype == 'amsub'     .or. obstype == 'mhs'       .or.  &
               obstype == 'hsb'       .or. obstype == 'goes_img'  .or.  &
               obstype == 'ahi'       .or. avhrr                  .or.  &
               amsre  .or. ssmis      .or. obstype == 'ssmi'      .or.  &
               obstype == 'ssu'       .or. obstype == 'atms'      .or.  &
               obstype == 'cris'      .or. obstype == 'cris-fsr'  .or.  &
               obstype == 'amsr2'     .or.  &
               obstype == 'gmi'       .or. obstype == 'saphir'   ) then
          ditype(i) = 'rad'
       else if (is_extOzone(dfile(i),obstype,dplat(i))) then
          ditype(i) = 'ozone'
       else if (obstype == 'sbuv2' &
           .or. obstype == 'omi' &
           .or. obstype == 'gome' &
           .or. mls &
           ) then
          ditype(i) = 'ozone'
       else if (obstype == 'mopitt') then
          ditype(i) = 'co'
       else if (index(obstype,'pcp')/=0 )then
          ditype(i) = 'pcp'
       else if (obstype == 'gps_ref' .or. obstype == 'gps_bnd') then
          ditype(i) = 'gps'
       else if ( index(obstype,'aod') /= 0 ) then
          ditype(i) = 'aero'
       else
          write(6,*)'READ_OBS:  ***ERROR*** - unknown ob type ',trim(obstype)
       end if

!   Set data class and number of reader tasks.  Set logical flag to indicate 
!   type type of GPS data (if present)
       if (index(dtype(i),'gps_ref') /= 0) ref_obs = .true.

!   Check info files to see if data is read.

       nuse=.false.
       minuse=-1
       if(ditype(i) == 'conv')then
          if(diag_conv .and. .not. reduce_diag)minuse=-2
          do j=1,nconvtype
             if(trim(dtype(i)) == trim(ioctype(j)) .and. icuse(j) > minuse)nuse=.true.
          end do
       else if(ditype(i) == 'rad')then
          if(diag_rad .and. .not. reduce_diag)minuse=-2
          do j=1,jpch_rad
             if(trim(dsis(i)) == trim(nusis(j)) .and. iuse_rad(j) > minuse)nuse=.true.
          end do
       else if(ditype(i) == 'ozone')then
          if(diag_ozone .and. .not. reduce_diag)minuse=-2
          do j=1,jpch_oz
             if(trim(dsis(i)) == trim(nusis_oz(j)) .and. iuse_oz(j) > minuse)nuse=.true.
          end do
       else if(ditype(i) == 'pcp')then
          if(diag_pcp .and. .not. reduce_diag)minuse=-2
          do j=1,npcptype
             if(trim(dsis(i)) == trim(nupcp(j)) .and. iusep(j) > minuse)nuse=.true.
          end do
       else if(ditype(i) == 'aero')then
          if(diag_aero .and. .not. reduce_diag)minuse=-2
          do j=1,jpch_aero
             if(trim(dsis(i)) == trim(nusis_aero(j)) .and. iuse_aero(j) > minuse)nuse=.true.
          end do
       else
          nuse=.true.
       end if

       if(nuse)then

!     Control parallel read for each ob type (currently just rad obs).  
!     To remove parallel read comment out line.
          ithin=dthin(i)
          if(ithin > 0 )then
            if(dmesh(ithin) > one)then
             if(hirs)then
                parallel_read(i)= .true.
             else if(obstype == 'amsua')then
                parallel_read(i)= .true.
             else if(obstype == 'airs' )then
                parallel_read(i)= .true.
             else if(obstype == 'iasi')then
                parallel_read(i)= .true.
             else if(obstype == 'amsub')then
                parallel_read(i)= .true.
             else if(obstype == 'mhs' )then
                parallel_read(i)= .true.
             else if(sndr )then
                parallel_read(i)= .true.
! N.B. ATMS must be run on one processor for the filtering code to work.
             else if(obstype == 'atms')then
!                 parallel_read(i)= .true.
             else if(ssmis)then
!               parallel_read(i)= .true.  
             else if(seviri)then
                parallel_read(i)= .true.
             else if(obstype == 'cris' .or. obstype == 'cris-fsr')then
                parallel_read(i)= .true.
             else if(avhrr)then
                parallel_read(i)= .true.
             else if(amsre)then
                parallel_read(i)= .true.
             else if(obstype == 'goes_img' )then
                parallel_read(i)= .true.
             else if(obstype == 'ahi' )then
                parallel_read(i)= .true.
             else if(obstype == 'hsb' )then
                parallel_read(i)= .true.
             else if(obstype == 'ssmi' )then
                parallel_read(i)= .true.
             else if(obstype == 'ssu' )then
                parallel_read(i)= .true.
             else if(obstype == 'amsr2')then    
!                parallel_read(i)= .true.     ! turn parallel read off for spatial averaging
             else if(obstype == 'gmi')then
!                parallel_read(i)= .true.     ! turn parallel read off for spatial averaging
!   Parallel read for SAPHIR not currently working. Leave parallel read off.
!             else if(obstype == 'saphir')then
!                parallel_read(i)= .true.

             end if
           end if
          end if
! direct broadcast from EUMETSAT (EARS)
          ears_possible(i) = ditype(i) == 'rad'  .and. & 
                  (obstype == 'amsua' .or. obstype == 'amsub' .or. & 
                   obstype == 'mhs'   .or. obstype == 'hirs3' .or. &
                   obstype == 'cris'  .or. obstype == 'cris-fsr' .or. &
                   obstype == 'iasi'  .or. obstype == 'atms') .and. &
                  (dplat(i) == 'n17' .or. dplat(i) == 'n18' .or. & 
                   dplat(i) == 'n19' .or. dplat(i) == 'npp' .or. &
                   dplat(i) == 'n20' .or. &
                   dplat(i) == 'metop-a' .or. dplat(i) == 'metop-b' .or. &
                   dplat(i) == 'metop-c') 
! direct broadcast from NESDIS/UW
          db_possible(i) = ditype(i) == 'rad'  .and.       & 
                  (obstype == 'amsua' .or.  obstype == 'amsub' .or.  & 
                   obstype == 'mhs' .or. obstype == 'atms' .or. &
                   obstype == 'cris' .or. obstype == 'cris-fsr' .or. &
                   obstype == 'iasi') .and. &
                  (dplat(i) == 'n17' .or. dplat(i) == 'n18' .or. & 
                   dplat(i) == 'n19' .or. dplat(i) == 'npp' .or. &
                   dplat(i) == 'n20' .or. &
                   dplat(i) == 'metop-a' .or. dplat(i) == 'metop-b' .or. &
                   dplat(i) == 'metop-c') 

!   Inquire data set to deterimine if input data available and size of dataset
          ii=ii+1
          if (ii>npem1) ii=0
          if(mype==ii)then
             call gsi_inquire(lenbytes,lexist,trim(dfile(i)),mype)
             call read_obs_check (lexist,trim(dfile(i)),dplat(i),dtype(i),minuse,read_rec1(i))
             
!   If no data set starting record to be 999999.  Note if this is not large
!   enough code should still work - just does a bit more work.

             if(.not. lexist)read_rec1(i) = 999999
             len4file=lenbytes/4
             if (ears_possible(i))then

                call gsi_inquire(lenbytes,lexistears,trim(dfile(i))//'ears',mype)
                call read_obs_check (lexistears,trim(dfile(i))//'ears',dplat(i),dtype(i),minuse, &
                    read_ears_rec1(i))

!   If no data set starting record to be 999999.  Note if this is not large
!   enough code should still work - just does a bit more work.

                if(.not. lexistears)read_ears_rec1(i) = 999999
                lexist=lexist .or. lexistears
                len4file=len4file+lenbytes/4
             end if
             if (db_possible(i))then

                call gsi_inquire(lenbytes,lexistdb,trim(dfile(i))//'_db',mype)
                call read_obs_check (lexistdb,trim(dfile(i))//'_db',dplat(i),dtype(i),minuse, &
                    read_db_rec1(i))

!   If no data set starting record to be 999999.  Note if this is not large
!   enough code should still work - just does a bit more work.

                if(.not. lexistdb)read_db_rec1(i) = 999999
                lexist=lexist .or. lexistdb
                len4file=len4file+lenbytes/4
             end if

 
             if(lexist) then
!      Initialize number of reader tasks to 1.  For the time being
!      only allow number of reader tasks >= 1 for select obstype.

                ntasks1(i)=1
                if(parallel_read(i)) then

!  Allow up to 16 processors/file increase loop bounds to increase number of processors allowed
                   do j=1,4
                      if(len4file < lenbuf)exit
                      ntasks1(i)=2*ntasks1(i)
                      len4file=len4file/2
                   end do
!               if(ntasks1(i)*lenbuf < len4file) ntasks1(i)=ntasks1(i)+1
                end if
             end if
          end if
       else
          if(mype == 0)write(6,*) 'data type ',dsis(i), &
                'not used in info file -- do not read file ',trim(dfile(i))
       end if
    end do


!   Distribute optimal number of reader tasks to all mpi tasks
    call mpi_allreduce(ntasks1,ntasks,ndat,mpi_integer,mpi_sum,mpi_comm_world,ierror)
    call mpi_allreduce(read_rec1,read_rec,ndat,mpi_integer,mpi_sum,mpi_comm_world,ierror) 
    call mpi_allreduce(read_ears_rec1,read_ears_rec,ndat,mpi_integer,mpi_sum,mpi_comm_world,ierror) 
    call mpi_allreduce(read_db_rec1,read_db_rec,ndat,mpi_integer,mpi_sum,mpi_comm_world,ierror) 

!   Limit number of requested tasks per type to be <= total available tasks
    npemax=0
    npetot=0
    do i=1,ndat
       if (ntasks(i)>npe) then
          write(6,*)'read_obs:  ***WARNING*** i=',i,' dtype=',dtype(i),' dsis=',dsis(i),&
               ' requested ntasks=',ntasks(i),' > npe=',npe,' reset ntasks=',npe
          ntasks(i)=npe
       endif
       npe_sub(i)=ntasks(i)
       npetot=npetot+npe_sub(i)
       npemax=max(npemax,npe_sub(i))
    end do

    if(l4dvar.and.(.not.lobserver)) then
!_RTod use_sfc=.false.
!_RTod call getsfc(mype,use_sfc)
       return
    endif
    
    npeextra=0
    if(mod(npetot,npe) > 0) npeextra=npe-mod(npetot,npe)
    maxproc=32
    if(npeextra > 0)then
       if(mype == 0)write(6,*) ' number of extra processors ',npeextra
       extraloop: do j=1,npeextra
          npe_sub3=ntasks
          minproc=999999
          do i=1,ndat
            if(ntasks(i) > 0 .and.  parallel_read(i))minproc=min(minproc,ntasks(i))
          end do
          if(npeextra < minproc) exit extraloop
          iix=minproc
          do ii=1,6
             do i=1,ndat
                if(iix == npe_sub3(i) .and. parallel_read(i))then
                   if(ntasks(i) <= npeextra .and. ntasks(i) < maxproc)then
                      npeextra=npeextra-ntasks(i)
                      ntasks(i)=2*ntasks(i)
                      if(npeextra < iix)cycle extraloop
                   end if
                end if
             end do
             iix=2*iix
             if(iix >= maxproc) cycle extraloop
          end do
       end do extraloop
    end if

!   Set up locations of first processor

    ilarge=0
    npestart=0
    npe_sub3=npe_sub
    mype_root_sub=0
    mmdat=0
    loopx: do j=1,ndat
       nlarge=0
       do i=1,ndat
          if(npe_sub3(i) > nlarge .and. npe_sub3(i)+npestart <= npe)then
             ilarge=i
             nlarge=npe_sub3(i)
          end if
       end do
       if(nlarge == 0)exit loopx
       npe_order(j)=ilarge
       mype_root_sub(ilarge)=npestart
       npestart=npestart+npe_sub3(ilarge)
       mmdat=mmdat+1
       if(npestart == npe)npestart=0
       npe_sub3(ilarge)=0
    end do loopx

!   Define sub-communicators for each data file
    mm1=mype+1
    belong=.false.
    mype_sub=-999
    mype_root=0
    next_mype=0
    do ii=1,mmdat
       i=npe_order(ii)
       if(npe_sub(i) > 0)then
          next_mype=mype_root_sub(i)
          do k=1,npe_sub(i)
             mype_work(k,i) = next_mype
             mype_sub(mype_work(k,i)+1,i)=k-1
             if(next_mype == mype)belong(i) = .true.
             next_mype = next_mype + 1
             if (next_mype>npem1) next_mype=0
          end do               

          call setcomm(iworld,iworld_group,npe_sub(i),mype_work(1,i),&
                 mpi_comm_sub(i),ierror)
       end if

    end do
    mype_airobst = mype_root
    do ii=1,mmdat
       i=npe_order(ii)
       if(mype == 0 .and. npe_sub(i) > 0) write(6,'(1x,a,i4,1x,a,1x,2a,2i4,1x,i6,1x,i6,1x,i6)') &
        'READ_OBS:  read ',i,dtype(i),dsis(i),' using ntasks=',ntasks(i),mype_root_sub(i), & 
               read_rec(i),read_ears_rec(i),read_db_rec(i)

       acft_profl_file = index(dfile(i),'_profl')/=0
       if ((aircraft_t_bc_pof .or. aircraft_t_bc_ext .or. &
          (aircraft_t_bc .and. acft_profl_file)) .and. dtype(i) == 't') &
                   mype_airobst = mype_root_sub(i)

    end do


    use_prsl_full=.false.
    use_hgtl_full=.false.
    use_sfc=.false.
    use_prsl_full_proc=.false.
    use_hgtl_full_proc=.false.
    mype_io_sfc=mype_io
    do ii=1,mmdat
       i=npe_order(ii)
       if(ditype(i) =='conv')then
          obstype=dtype(i)
          if (obstype == 't' .or. obstype == 'q'  .or. &
              obstype == 'uv' .or. obstype == 'wspd10m') then
              use_prsl_full=.true.
              if(belong(i))use_prsl_full_proc=.true.
          else
            do j=1,nconvtype
               if(obstype == trim(ioctype(j)) .and. ithin_conv(j) > 0)then
                  use_prsl_full=.true.
                  if(belong(i))use_prsl_full_proc=.true.
               end if
            end do
          end if
          if(obstype == 'rw')then
             use_hgtl_full=.true.
             if(belong(i))use_hgtl_full_proc=.true.
          end if
          if(obstype == 'sst')then
            if(belong(i))use_sfc=.true.
          endif
       else if(ditype(i) == 'rad' )then
          if(belong(i)) use_sfc=.true.
       end if
    end do
    use_sfc_any=.false.
    loop: do ii=1,mmdat
       i=npe_order(ii)
       if(ditype(i) == 'rad' .or. ditype(i) == 'sst')then
          mype_io_sfc=mype_root_sub(i)
          use_sfc_any=.true.
          exit loop
       end if
    end do loop
    if(use_sfc_any .and. mype == mype_io)use_sfc=.true.

!   Create full horizontal surface fields from local fields in guess_grids
    call getsfc(mype,mype_io_sfc,use_sfc,use_sfc_any)
    if(mype == mype_io) call prt_guessfc2('sfcges2',use_sfc)

!   Get guess 3d pressure on full grid
    allocate(work1(max(iglobal,itotsub)),prslsm(lat1*lon1))
    if(use_prsl_full)then
       if(use_prsl_full_proc)then
          allocate(prsl_full(nlat,nlon,nsig))
       else
          allocate(prsl_full(1,1,1))
       end if
       do k=1,nsig
          call strip(ges_prsl(:,:,k,ntguessig),prslsm)
          call mpi_allgatherv(prslsm,ijn(mype+1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
          if(use_prsl_full_proc)then
             call reorder(work1,1,1)
             do ii=1,iglobal
                i=ltosi(ii)
                j=ltosj(ii)
                prsl_full(i,j,k)=work1(ii)
             end do
          end if
       end do
    else
       allocate(prsl_full(1,1,1))
    end if
!   Get guess 3d geopotential height on full grid
    if(use_hgtl_full)then
       allocate(hgtlsm(lat1*lon1))
       if(use_hgtl_full_proc)then
          allocate(hgtl_full(nlat,nlon,nsig))
       else
          allocate(hgtl_full(1,1,1))
       end if
       do k=1,nsig
          call strip(geop_hgtl(:,:,k,ntguessig),hgtlsm)
          call mpi_allgatherv(hgtlsm,ijn(mype+1),mpi_rtype,&
               work1,ijn,displs_g,mpi_rtype,mpi_comm_world,ierror)
          if(use_hgtl_full_proc)then
             call reorder(work1,1,1)
             do ii=1,iglobal
                i=ltosi(ii)
                j=ltosj(ii)
                hgtl_full(i,j,k)=work1(ii)
             end do
           end if
       end do
       deallocate(hgtlsm)
    else
      allocate(hgtl_full(1,1,1))
    end if
    deallocate(work1,prslsm)

!   Create full horizontal nst fields from local fields in guess_grids/read it from nst file
    if (nst_gsi > 0) then
      call gsi_nstcoupler_set(mype_io_sfc)         ! Set NST fields (each proc needs full NST fields)
    endif
!   Create moored buoy station ID
    call mbuoy_info(mype)

!   Create ships info(ID, Depth & Instrument)
    call read_ship_info(mype)

!   Loop over data files.  Each data file is read by a sub-communicator
    loop_of_obsdata_files: &
    do ii=1,mmdat

       i=npe_order(ii)
       task_belongs: &
       if (i > 0 .and. belong(i)) then

          platid=dplat(i)                    !     platid   - satellites to read
          obstype=dtype(i)                   !     obstype  - observation types to process
          infile=trim(dfile(i))              !     infile   - units from which to read data
          sis=dsis(i)                        !     sensor/instrument/satellite indicator
          val_dat=dval(i)                    !     weighting factors applied to super obs
          ithin=dthin(i)                     !     ithin    - flags to thin data
          ithinx=max(1,abs(ithin))
          rmesh=dmesh(ithinx)                !     rmesh    - thinning mesh sizes (km)
          twind=time_window(i)               !     time window (hours) for input group
          isfcalc=dsfcalc(i)                 !     method to calculate surface fields within fov
          nread=0
          nouse=0
          npuse=0

          if (mype_sub(mm1,i)==mype_root) then
             open(lunout,file=obsfile_all(i),form='unformatted')
             rewind(lunout)
          endif

!         Process conventional (prepbufr) data
          ditype_select: &
          if(ditype(i) == 'conv')then
             conv_obstype_select: &
             if (obstype == 't' .or. obstype == 'q'  .or. obstype == 'ps' .or. &
                 obstype == 'pw' .or. obstype == 'spd'.or. & 
                 obstype == 'gust' .or. obstype == 'vis'.or. &
                 obstype == 'td2m' .or. &
!                obstype=='mxtm' .or. obstype == 'mitm' .or. &
                 obstype=='howv' .or. obstype=='pmsl' .or. &
                 obstype == 'mta_cld' .or. obstype == 'gos_ctp' .or. &
                 obstype == 'lcbas' .or. obstype == 'cldch' ) then

!               Process flight-letel high-density data not included in prepbufr
                if ( index(infile,'hdobbufr') /=0 ) then
                  call read_fl_hdob(nread,npuse,nouse,infile,obstype,lunout,gstime,twind,sis,&
                                    prsl_full,nobs_sub1(1,i))
                  string='READ_FL_HDOB'
                else
                   call read_prepbufr(nread,npuse,nouse,infile,obstype,lunout,twind,sis,&
                        prsl_full,nobs_sub1(1,i),read_rec(i))
                   string='READ_PREPBUFR'

                endif

             else if(obstype == 'mitm') then
                if ( index(infile,'mitmdat') /=0) then
                   call read_mitm_mxtm(nread,npuse,nouse,infile,obstype,lunout,gstime,sis, & 
                                       nobs_sub1(1,i))
                   string='READ_ASCII_MITM'
                 else
                   call read_prepbufr(nread,npuse,nouse,infile,obstype,lunout,twind,sis,&
                        prsl_full,nobs_sub1(1,i),read_rec(i))
                   string='READ_PREPBUFR'
                 endif

             else if(obstype == 'mxtm') then
                if ( index(infile,'mxtmdat') /=0) then
                   call read_mitm_mxtm(nread,npuse,nouse,infile,obstype,lunout,gstime,sis, & 
                                       nobs_sub1(1,i))
                   string='READ_ASCII_MXTM'
                 else
                   call read_prepbufr(nread,npuse,nouse,infile,obstype,lunout,twind,sis,&
                        prsl_full,nobs_sub1(1,i),read_rec(i))
                   string='READ_PREPBUFR'
                 endif

!            Process total cloud amount (tcamt) in prepbufr -or- from goes imager sky cover products
             else if(obstype == 'tcamt') then
!             Process GOES Imager Sky Cover product separately from prepbufr-based sky cover obs
                if ( index(infile,'goessky') /=0 ) then
                   call read_goesimgr_skycover(nread,npuse,nouse,infile,obstype,lunout,gstime,twind,sis,&
                        prsl_full,nobs_sub1(1,i))
                   string='READ_GOESIMGR_SKYCOVER'
                else
!              else read from prepbufr
                   call read_prepbufr(nread,npuse,nouse,infile,obstype,lunout,twind,sis,prsl_full, &
                         nobs_sub1(1,i),read_rec(i))
                   string='READ_PREPBUFR'
                end if

!             Process winds in the prepbufr
            else if(obstype == 'uv' .or. obstype == 'wspd10m') then
!             Process satellite winds which seperate from prepbufr
                if ( index(infile,'satwnd') /=0 ) then
                  call read_satwnd(nread,npuse,nouse,infile,obstype,lunout,gstime,twind,sis,&
                     prsl_full,nobs_sub1(1,i))
                  string='READ_SATWND'
!             Process oscat winds which seperate from prepbufr
                elseif ( index(infile,'oscatbufr') /=0 ) then
                  call read_sfcwnd(nread,npuse,nouse,infile,obstype,lunout,gstime,twind,sis,&
                     prsl_full,nobs_sub1(1,i))
                  string='READ_SFCWND'
!             Process rapidscat winds which seperate from prepbufr
                elseif ( index(infile,'rapidscatbufr') /=0 ) then
                  call read_rapidscat(nread,npuse,nouse,infile,obstype,lunout,gstime,twind,sis,&
                     prsl_full,nobs_sub1(1,i))
                  string='READ_RAPIDSCAT'
                else if ( index(infile,'hdobbufr') /=0 ) then
                  call read_fl_hdob(nread,npuse,nouse,infile,obstype,lunout,gstime,twind,sis,&
                       prsl_full,nobs_sub1(1,i))
                  string='READ_FL_HDOB'
                else
                  call read_prepbufr(nread,npuse,nouse,infile,obstype,lunout,twind,sis,&
                     prsl_full,nobs_sub1(1,i),read_rec(i))
                  string='READ_PREPBUFR'
                endif

!            Process conventional SST (nsstbufr, at this moment) data
             elseif ( obstype == 'sst' ) then
                if ( platid == 'nsst') then
                   call read_nsstbufr(nread,npuse,nouse,gstime,infile,obstype, &
                        lunout,twind,sis,nobs_sub1(1,i))
                   string='READ_NSSTBUFR'
                elseif ( platid == 'mods') then
                   call read_modsbufr(nread,npuse,nouse,gstime,infile,obstype, &
                        lunout,twind,sis,nobs_sub1(1,i))
                   string='READ_MODSBUFR'
                elseif ( platid == 'prep') then
                   if(nst_gsi>0)then
                      write(6,*)'read_obs: should not handle SST via read_prepbufr when NSST on'
                      call stop2(999)
                   endif

                   call read_prepbufr(nread,npuse,nouse,infile,obstype,lunout,twind,sis,&
                        prsl_full,nobs_sub1(1,i),read_rec(i))
                   string='READ_PREPBUFR'
                endif

!            Process radar reflectivity Mosaic
             else if (obstype == 'rad_ref' ) then
                call read_radarref_mosaic(nread,npuse,infile,obstype,lunout,twind,sis, &
                      nobs_sub1(1,i))
                string='READ_RADARREF_MOSAIC'

!            Process  lightning
             else if (obstype == 'lghtn' ) then
                call read_lightning(nread,npuse,infile,obstype,lunout,twind,sis,nobs_sub1(1,i))
                string='READ_LIGHTNING'

!            Process  NASA LaRC 
             else if (obstype == 'larccld' ) then
                if(i_gsdcldanal_type==2) then
                   call read_NASA_LaRC_cloud(nread,npuse,nouse,obstype,lunout,sis,nobs_sub1(1,i))
                else if( i_gsdcldanal_type==1) then
                   call read_nasa_larc(nread,npuse,infile,obstype,lunout,twind,sis,nobs_sub1(1,i))
                end if
                string='READ_NASA_LaRC'

!            Process radar winds
             else if (obstype == 'rw') then
                call read_radar(nread,npuse,nouse,infile,lunout,obstype,twind,sis,&
                                hgtl_full,nobs_sub1(1,i))
                string='READ_RADAR'

!            Process lagrangian data
             else if (obstype == 'lag') then
                call read_lag(nread,npuse,nouse,infile,lunout,obstype,&
                 &twind,gstime,sis)
                string='READ_LAG'

!            Process lidar winds
             else if (obstype == 'dw') then
                call read_lidar(nread,npuse,nouse,infile,obstype,lunout,twind,sis, &
                     nobs_sub1(1,i))
                string='READ_LIDAR'

!            Process synthetic tc-mslp obs
             else if (obstype == 'tcp') then
                call read_tcps(nread,npuse,nouse,infile,obstype,lunout,sis, &
                     nobs_sub1(1,i))
                string='READ_TCPS'

!            Process radar superob winds
             else if (obstype == 'srw') then
                call read_superwinds(nread,npuse,nouse,infile,obstype,lunout, &
                     twind,sis,nobs_sub1(1,i))
                string='READ_SUPRWNDS'

             else if (obstype == 'pm2_5' .or. obstype == 'pm10') then

                if (oneobtest_chem .and. oneob_type_chem=='pm2_5') then
                   call oneobschem(nread,npuse,nouse,gstime,&
                        &infile,obstype,lunout,sis,nobs_sub1(1,i))
                   string='ONEOBSCHEM'
                else
                   call read_anowbufr(nread,npuse,nouse,gstime,&
                        &infile,obstype,lunout,twind,sis,nobs_sub1(1,i))
                   string='READ_ANOWBUFR'
                endif

!            Process pblh
             else if (obstype == 'pblh') then
                call read_pblh(nread,npuse,nouse,infile,obstype,lunout,twind,sis, &
                    nobs_sub1(1,i))
                string='READ_PBLH'
             end if conv_obstype_select

          else if (ditype(i) == 'rad')then


!            Process TOVS 1b data
             rad_obstype_select: &
             if (platid /= 'aqua' .and. (obstype == 'amsua' .or. &
                  obstype == 'amsub' .or. obstype == 'msu'   .or.  &
                  obstype == 'mhs'   .or. obstype == 'hirs4' .or.  &
                  obstype == 'hirs3' .or. obstype == 'hirs2' .or.  &
                  obstype == 'ssu' )) then
                call read_bufrtovs(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), nobs_sub1(1,i), &
                     read_rec(i),read_ears_rec(i),read_db_rec(i),dval_use)
                string='READ_BUFRTOVS'

!            Process atms data
             else if (obstype == 'atms') then
                call read_atms(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i),nobs_sub1(1,i),&
                     read_rec(i),read_ears_rec(i),read_db_rec(i),dval_use)
                string='READ_ATMS'

!            Process saphir data
             else if (obstype == 'saphir') then
                call read_saphir(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i),  &
                     nobs_sub1(1,i),dval_use)
                string='READ_SAPHIR'


!            Process airs data        
             else if(platid == 'aqua' .and. (obstype == 'airs' .or.   &
                  obstype == 'amsua'  .or.  obstype == 'hsb' ))then
                call read_airs(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                     nobs_sub1(1,i),read_rec(i),dval_use)
                string='READ_AIRS'

!            Process iasi data
             else if(obstype == 'iasi')then
                call read_iasi(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i),nobs_sub1(1,i), &
                     read_rec(i),read_ears_rec(i),read_db_rec(i),dval_use)
                string='READ_IASI'

!            Process cris data
             else if(obstype == 'cris' .or. obstype =='cris-fsr' )then
                call read_cris(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i),nobs_sub1(1,i), &
                     read_rec(i),read_ears_rec(i),read_db_rec(i),dval_use)
                string='READ_CRIS'

!            Process GOES sounder data
!            Process raw or prepbufr files (1x1 or 5x5)
             else if (obstype == 'sndr' .or.                            &
                      obstype == 'sndrd1' .or. obstype == 'sndrd2' .or. &
                      obstype == 'sndrd3' .or. obstype == 'sndrd4') then
                call read_goesndr(mype,val_dat,ithin,rmesh,platid,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,gstime,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                     nobs_sub1(1,i),read_rec(i),dval_use)
                string='READ_GOESNDR'

!            Process ssmi data
             else if (obstype == 'ssmi' ) then
                call read_ssmi(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                     nobs_sub1(1,i),read_rec(i),dval_use)
                string='READ_SSMI'

!            Process amsre data
             else if ( obstype == 'amsre_low' .or. obstype == 'amsre_mid' .or. &
                       obstype == 'amsre_hig' ) then
                call read_amsre(mype,val_dat,ithin,isfcalc,rmesh,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                     nobs_sub1(1,i),read_rec(i),dval_use)
                string='READ_AMSRE'
                
!            Process ssmis data
             else if (obstype == 'ssmis'     .or. &
                      obstype == 'ssmis_las' .or. obstype == 'ssmis_uas' .or. &
                      obstype == 'ssmis_img' .or. obstype == 'ssmis_env' ) then
                call read_ssmis(mype,val_dat,ithin,isfcalc,rmesh,platid,gstime,&
                      infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                      mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                      nobs_sub1(1,i),read_rec(i),dval_use)
                string='READ_SSMIS'

!            Process AMSR2 data
             else if(obstype == 'amsr2')then
                call read_amsr2(mype,val_dat,ithin,rmesh,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i),  &
                     nobs_sub1(1,i))
                string='READ_AMSR2'

!            Process GOES IMAGER RADIANCE  data
             else if(obstype == 'goes_img') then
                call read_goesimg(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                     nobs_sub1(1,i),read_rec(i),dval_use)
                string='READ_GOESMIMG'

!            Process GMI data
             else if (obstype == 'gmi') then
                call read_gmi(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis,&
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i),  &
                     nobs_sub1(1,i),dval_use)
                string='READ_GMI'

!            Process Meteosat SEVIRI RADIANCE  data
             else if(obstype == 'seviri') then
                 call read_seviri(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                     nobs_sub1(1,i),read_rec(i),dval_use)
                string='READ_SEVIRI'

        !    Process Himawari-8 AHI RADIANCE  data
             else if(obstype == 'ahi') then
                call read_ahi(mype,val_dat,ithin,rmesh,platid,gstime,&
                     infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                     mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i),  &
                     nobs_sub1(1,i))
                string='READ_AHI'


!            Process NAVY AVHRR RADIANCE  data
               else if(obstype == 'avhrr_navy') then
                  call read_avhrr_navy(mype,val_dat,ithin,rmesh,platid,gstime,&
                       infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                       mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                       nobs_sub1(1,i),read_rec(i),dval_use)
                  string='READ_AVH_NAVY'

  !            Process NESDIS AVHRR RADIANCE  data
               else if(obstype == 'avhrr') then
                  call read_avhrr(mype,val_dat,ithin,rmesh,platid,gstime,&
                       infile,lunout,obstype,nread,npuse,nouse,twind,sis, &
                       mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                       nobs_sub1(1,i),read_rec(i),dval_use)
                  string='READ_AVHRR'
               end if rad_obstype_select

!         Process ozone data
          else if (ditype(i) == 'ozone')then
             ozone_obstype_select: &
             if (is_extOzone(infile,obstype,dplat(i))) then

                call extOzone_read(infile,obstype,dplat(i),dsis(i), &
                   iread,ipuse,iouse, platid,gstime,lunout,twind,ithin,rmesh, &
                   nobs_sub1(:,i))
                string='extOzone_read'

                nread=nread+iread
                npuse=npuse+ipuse
                nouse=nouse+iouse

             else
                call read_ozone(nread,npuse,nouse,&
                   platid,infile,gstime,lunout,obstype,twind,sis,ithin,rmesh, &
                   nobs_sub1(1,i))
                string='READ_OZONE'
             endif ozone_obstype_select

!         Process co data
          else if (ditype(i) =='co')then 
             call read_co(nread,npuse,nouse,infile,gstime,lunout,obstype,sis, &
                  nobs_sub1(1,i))
             string='READ_CO'

!         Process precipitation             
          else if (ditype(i) == 'pcp')then
             call read_pcp(nread,npuse,nouse,gstime,infile,lunout,obstype,twind,sis, &
                  nobs_sub1(1,i))
             string='READ_PCP'

!         Process gps observations
          else if (ditype(i) == 'gps')then
             call read_gps(nread,npuse,nouse,infile,lunout,obstype,twind, &
                  nprof_gps1,sis,nobs_sub1(1,i))
             string='READ_GPS'

!         Process aerosol data
          else if (ditype(i) == 'aero' )then
             call read_aerosol(nread,npuse,nouse,&
                  platid,infile,gstime,lunout,obstype,twind,sis,ithin,rmesh, &
                  mype_root,mype_sub(mm1,i),npe_sub(i),mpi_comm_sub(i), &
                  nobs_sub1(1,i))
             string='READ_AEROSOL'
                     
          end if ditype_select

!         Close unit to data file

!         Accumulate data counts on "root" task
          if (mype_sub(mm1,i)==mype_root) then
             close(lunout)
             ndata1(i,1)=ndata1(i,1)+npuse
             ndata1(i,2)=ndata1(i,2)+nread
             ndata1(i,3)=ndata1(i,3)+nouse

             write(6,8000) adjustl(string),infile,obstype,sis,nread,ithin,&
                  rmesh,isfcalc,nouse,npe_sub(i)
8000         format(1x,a22,': file=',a15,&
                  ' type=',a10,  ' sis=',a20,  ' nread=',i10,&
                  ' ithin=',i2, ' rmesh=',f11.6,' isfcalc=',i2,&
                  ' ndata=',i10,' ntask=',i3)

          endif
       endif task_belongs

    end do loop_of_obsdata_files
    deallocate(prsl_full)
    deallocate(hgtl_full)

!   Broadcast aircraft new tail numbers for aircraft
!   temperature bias correction
!   if (aircraft_t_bc) then
!      call mpi_barrier(mpi_comm_world,ierror)
!      call mpi_bcast(ntail_update,1,mpi_itype,mype_airobst,mpi_comm_world,ierror)
!      call mpi_bcast(idx_tail,max_tail,mpi_itype,mype_airobst,mpi_comm_world,ierror)
!      call mpi_bcast(taillist,max_tail,MPI_CHARACTER,mype_airobst,mpi_comm_world,ierror)
!   end if

!   Deallocate arrays containing full horizontal surface fields
    call destroy_sfc
!   Deallocate arrays containing full horizontal nsst fields
    if (nst_gsi > 0) call gsi_nstcoupler_final()
!   Sum and distribute number of obs read and used for each input ob group
    call mpi_allreduce(ndata1,ndata,ndat*3,mpi_integer,mpi_sum,mpi_comm_world,&
       ierror)

!   Collect super obs factors
    if(mype == 0)write(6,*) ' dval_use = ',dval_use
    if(dval_use)then
       call mpi_allreduce(super_val,super_val1,superp+1,mpi_rtype,&
            mpi_sum,mpi_comm_world,ierror)
    else
       super_val1=zero
    end if
    super_val1(0)=one
    deallocate(super_val)

!   Collect number of gps profiles (needed later for qc)
    call mpi_allreduce(nprof_gps1,nprof_gps,1,mpi_integer,mpi_sum,mpi_comm_world,ierror)
    call mpi_allreduce(nobs_sub1,nobs_sub,npe*ndat,mpi_integer,mpi_sum,mpi_comm_world,& 
         ierror)

!   Write collective obs selection information to scratch file.
    if (lread_obs_save .and. mype==0) then
       write(6,*)'READ_OBS:  write collective obs selection info to ',trim(obs_input_common)
       open(lunsave,file=obs_input_common,form='unformatted')
       write(lunsave) ndata,ndat,npe,superp,nprof_gps,ditype
       write(lunsave) super_val1
       write(lunsave) nobs_sub
       close(lunsave)
    endif

!   End of routine
    return
end subroutine read_obs


end module read_obsmod
