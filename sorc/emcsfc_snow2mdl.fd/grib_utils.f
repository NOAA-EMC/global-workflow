 subroutine grib_check(file_name, isgrib)
!$$$  subprogram documentation block
!
! subprogram:    grib_check
!   prgmmr: gayno          org: w/np2     date: 2007-nov-28
!
! abstract:  determine whether file is grib or not.
!  
! program history log:
! 2007-nov-28  gayno    - initial version
! 2011-apr-26  gayno    - replace my simple-minded logic
!                         with call to w3lib routin skgb.
! 2014-feb-07  gayno    - determine whether file is
!                         grib1 or grib2.
!
! usage: call grib_check(file_name, isgrib)
!
!   input argument list:  file_name - file to be checked
!
!   output argument list: isgrib - '1' or '2' if grib1/2 file
!                                  '0' if not grib
!
! files: 
!   input:
!     - file to be checked, fort.11
!
!   output: none
!
! condition codes:  all fatal
!     - bad file open, fort.11
! 
! remarks: none.
!          
!$$$

 implicit none

 character*(*), intent(in)         :: file_name
 integer, parameter                :: iunit=11
 integer                           :: istat, iseek, mseek, lskip, lgrib, version
 integer, intent(out)              :: isgrib

 print*,"- CHECK FILE TYPE OF: ", trim(file_name)
 call baopenr (iunit, file_name, istat)

 if (istat /= 0) then
   print*,'- FATAL ERROR: BAD FILE OPEN. ISTAT IS ',istat
   call w3tage('SNOW2MDL')
   call errexit(40)
 end if
 
 iseek = 0
 mseek = 64
 call skgb2(iunit, iseek, mseek, lskip, lgrib, version)

 call baclose(iunit, istat)
 
 if (lgrib > 0) then
   isgrib = version
   if (isgrib == 1) print*,"- FILE IS GRIB1"
   if (isgrib == 2) print*,"- FILE IS GRIB2"
 else
   isgrib = 0
   print*,"- FILE IS BINARY"
 endif

 return

 end subroutine grib_check

 SUBROUTINE SKGB2(LUGB,ISEEK,MSEEK,LSKIP,LGRIB,I1)
!$$$  subprogram documentation block
!
! subprogram:   skgb2
!   prgmmr: gayno          org: w/np2     date: 2014-feb-07
!
! abstract:  determine whether file is grib or not.
!            based on w3nco library routine skgb.
!  
! program history log:
! 2014-feb-07  gayno    - initial version
!
! usage: call SKGB2(LUGB,ISEEK,MSEEK,LSKIP,LGRIB,I1)
!
!   input argument list:  lugb  - file unit number
!                         iseek - number of bits to skip
!                                 before search.
!                         mseek - max number of bytes 
!                                 to search.
!
!   output argument list:  lskip  - number of bytes to skip 
!                                   before message
!                          lgrib  - number of bytes in message.
!                                   '0' if not grib.
!                          i1     - '1' or '2' if grib1/2 file.
!                                   '0' if not grib.
!
! files:
!    input:
!      - file to be checked, unit=lugb
!
!    output: none
!
! condition codes: none
!
! remarks: none.
!
!$$$
 implicit none
 INTEGER, INTENT( IN)     :: LUGB, ISEEK, MSEEK
 INTEGER, INTENT(OUT)     :: LSKIP, LGRIB, I1
 INTEGER, PARAMETER       :: LSEEK=128
 INTEGER                  :: K, KZ, KS, KG, KN, KM, I4, K4
 CHARACTER Z(LSEEK)
 CHARACTER Z4(4)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 I1=0
 LGRIB=0
 KS=ISEEK
 KN=MIN(LSEEK,MSEEK)
 KZ=LSEEK
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  LOOP UNTIL GRIB MESSAGE IS FOUND
 DO WHILE(LGRIB.EQ.0.AND.KN.GE.8.AND.KZ.EQ.LSEEK)
!  READ PARTIAL SECTION
   CALL BAREAD(LUGB,KS,KN,KZ,Z)
   KM=KZ-8+1
   K=0
!  LOOK FOR 'GRIB...1' IN PARTIAL SECTION
   DO WHILE(LGRIB.EQ.0.AND.K.LT.KM)
     CALL GBYTEC(Z,I4,(K+0)*8,4*8)
     CALL GBYTEC(Z,I1,(K+7)*8,1*8)
     IF(I4.EQ.1196575042.AND.(I1.EQ.1.OR.I1.EQ.2)) THEN
!  LOOK FOR '7777' AT END OF GRIB MESSAGE
       IF (I1.EQ.1) CALL GBYTEC(Z,KG,(K+4)*8,3*8)
       IF (I1.EQ.2) CALL GBYTEC(Z,KG,(K+12)*8,4*8)
       CALL BAREAD(LUGB,KS+K+KG-4,4,K4,Z4)
       IF(K4.EQ.4) THEN
         CALL GBYTEC(Z4,I4,0,4*8)
         IF(I4.EQ.926365495) THEN
!  GRIB MESSAGE FOUND
           LSKIP=KS+K
           LGRIB=KG
         ENDIF
       ENDIF
     ENDIF
     K=K+1
   ENDDO
   KS=KS+KM
   KN=MIN(LSEEK,ISEEK+MSEEK-KS)
 ENDDO
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 RETURN
 END subroutine skgb2

 subroutine gdt_to_gds(igdtnum, igdstmpl, igdtlen, kgds, ni, nj, res)
!$$$  subprogram documentation block
!
! subprogram:    gdt_to_gds
!   prgmmr: gayno          org: w/np2     date: 2014-sep-26
!
! abstract:  convert from the grib2 grid description template array
!            used by the ncep grib2 library, to the grib1 grid
!            description section array used by ncep ipolates library.
!  
! program history log:
! 2014-sep-26  gayno    - initial version
!
! usage: call gds_to_gds(igdtnum,igdstmpl,igdtlen,kgds,ni,nj,res)
!
!   input argument list:  
!     igdtnum  - grib2 grid desc template number
!     igdstmpl - grib2 grid desc template array
!     igdtlen  - grib2 grid desc template array size
!
!   output argument list: 
!     kgds     - grib1 grid description section array 
!                used by ncep ipolates library.
!     ni,nj    - i/j grid dimensions
!     res      - grid resolution in km
!                        
! files: none
!
! condition codes:
!   50 - unrecognized model grid type; fatal
!
! remarks: none.
!
!$$$

 implicit none

 integer, intent(in   )  :: igdtnum, igdtlen, igdstmpl(igdtlen)
 integer, intent(  out)  :: kgds(200), ni, nj
 integer                 :: iscale

 real,    intent(  out)  :: res

 kgds=0

 if (igdtnum.eq.0) then        ! lat/lon grid

   iscale=igdstmpl(10)*igdstmpl(11)
   if (iscale == 0) iscale = 1e6
   kgds(1)=0                   ! oct 6
   kgds(2)=igdstmpl(8)         ! octs 7-8, Ni
   ni = kgds(2)
   kgds(3)=igdstmpl(9)         ! octs 9-10, Nj
   nj = kgds(3)
   kgds(4)=nint(float(igdstmpl(12))/float(iscale)*1000.)  ! octs 11-13, Lat of 1st grid point
   kgds(5)=nint(float(igdstmpl(13))/float(iscale)*1000.)  ! octs 14-16, Lon of 1st grid point

   kgds(6)=0                   ! oct 17, resolution and component flags
   if (igdstmpl(1)==2 ) kgds(6)=64
   if ( btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5) ) kgds(6)=kgds(6)+128
   if ( btest(igdstmpl(14),3) ) kgds(6)=kgds(6)+8

   kgds(7)=nint(float(igdstmpl(15))/float(iscale)*1000.)  ! octs 18-20, Lat of last grid point
   kgds(8)=nint(float(igdstmpl(16))/float(iscale)*1000.)  ! octs 21-23, Lon of last grid point
   kgds(9)=nint(float(igdstmpl(17))/float(iscale)*1000.)  ! octs 24-25, di
   kgds(10)=nint(float(igdstmpl(18))/float(iscale)*1000.) ! octs 26-27, dj

   kgds(11) = 0              ! oct 28, scan mode
   if (btest(igdstmpl(19),7)) kgds(11) = 128
   if (btest(igdstmpl(19),6)) kgds(11) = kgds(11) +  64
   if (btest(igdstmpl(19),5)) kgds(11) = kgds(11) +  32

   kgds(12)=0      ! octs 29-32, reserved
   kgds(19)=0      ! oct 4, # vert coordinate parameters
   kgds(20)=255    ! oct 5, used for thinned grids, set to 255

   res = float(kgds(9)) / 1000.0 * 111.0

 elseif (igdtnum.eq.40) then       !  Gaussian Lat/Lon grid

   iscale=igdstmpl(10)*igdstmpl(11)
   if (iscale==0) iscale=1e6
   kgds(1)=4                   ! oct 6
   kgds(2)=igdstmpl(8)         ! octs 7-8, Ni
   ni = kgds(2)
   kgds(3)=igdstmpl(9)         ! octs 9-10, Nj
   nj = kgds(3)
   kgds(4)=nint(float(igdstmpl(12))/float(iscale)*1000.)  ! octs 11-13, Lat of 1st grid point
   kgds(5)=nint(float(igdstmpl(13))/float(iscale)*1000.)  ! octs 14-16, Lon of 1st grid point

   kgds(6)=0                   ! oct 17, resolution and component flags
   if (igdstmpl(1)==2 ) kgds(6)=64
   if ( btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5) ) kgds(6)=kgds(6)+128
   if ( btest(igdstmpl(14),3) ) kgds(6)=kgds(6)+8

   kgds(7)=nint(float(igdstmpl(15))/float(iscale)*1000.) ! octs 18-20, Lat of last grid point
   kgds(8)=nint(float(igdstmpl(16))/float(iscale)*1000.) ! octs 21-23, Lon of last grid point
   kgds(9)=nint(float(igdstmpl(17))/float(iscale)*1000.) ! octs 24-25, Di
   kgds(10)=igdstmpl(18)                                 ! octs 26-27, Number of parallels

   kgds(11) = 0              ! oct 28, scan mode
   if (btest(igdstmpl(19),7)) kgds(11) = 128
   if (btest(igdstmpl(19),6)) kgds(11) = kgds(11) +  64
   if (btest(igdstmpl(19),5)) kgds(11) = kgds(11) +  32

   kgds(12)=0      ! octs 29-32, reserved
   kgds(19)=0      ! oct 4, # vert coordinate parameters
   kgds(20)=255    ! oct 5, used for thinned grids, set to 255

   res = float(kgds(9)) / 1000.0 * 111.0

 elseif (igdtnum.eq.20) then       ! Polar Stereographic Grid

   iscale=1e6
   kgds(1)=5                      ! oct 6, data representation type, polar
   kgds(2)=igdstmpl(8)            ! octs 7-8, nx 
   ni = kgds(2)
   kgds(3)=igdstmpl(9)            ! octs 8-10, ny
   nj = kgds(3)
   kgds(4)=nint(float(igdstmpl(10))/float(iscale)*1000.)  ! octs 11-13, lat of 1st grid point
   kgds(5)=nint(float(igdstmpl(11))/float(iscale)*1000.)  ! octs 14-16, lon of 1st grid point

   kgds(6)=0                      ! oct 17, resolution and component flags
   if (igdstmpl(1) >= 2 .or. igdstmpl(1) <= 5) kgds(6)=64
   if (igdstmpl(1) == 7) kgds(6)=64
   if ( btest(igdstmpl(12),4).OR.btest(igdstmpl(12),5) ) kgds(6)=kgds(6)+128
   if ( btest(igdstmpl(12),3) ) kgds(6)=kgds(6)+8

   kgds(7)=nint(float(igdstmpl(14))/float(iscale)*1000.)  ! octs 18-20, lon of orientation
   kgds(8)=nint(float(igdstmpl(15))/float(iscale)*1000.)  ! octs 21-23, dx
   kgds(9)=nint(float(igdstmpl(16))/float(iscale)*1000.)  ! octs 24-26, dy

   kgds(10)=0                ! oct 27, projection center flag
   if (btest(igdstmpl(17),1)) kgds(10) = 128

   kgds(11) = 0              ! oct 28, scan mode
   if (btest(igdstmpl(18),7)) kgds(11) = 128
   if (btest(igdstmpl(18),6)) kgds(11) = kgds(11) +  64
   if (btest(igdstmpl(18),5)) kgds(11) = kgds(11) +  32

   kgds(19)=0    ! oct 4, # vert coordinate parameters
   kgds(20)=255  ! oct 5, used for thinned grids, set to 255

   res = 0.5 * float(kgds(8)+kgds(9)) / 1000. 

 elseif (igdtnum.eq.1) then    ! Rotated Lat/Lon grid

   if (btest(igdstmpl(19),2)) then  ! e-stagger, bit 6 of scan mode is '1'

     iscale=igdstmpl(10)*igdstmpl(11)
     if (iscale == 0) iscale = 1e6
     kgds(1)=203                    ! oct 6, "E" grid
     kgds(2)=igdstmpl(8)            ! octs 7-8, Ni
     ni = kgds(2)
     kgds(3)=igdstmpl(9)            ! octs 9-10, Nj
     nj = kgds(3)
     kgds(4)=nint(float(igdstmpl(12))/float(iscale)*1000.)  ! octs 11-13, Lat of 1st grid point
     kgds(5)=nint(float(igdstmpl(13))/float(iscale)*1000.)  ! octs 14-16, Lon of 1st grid point

     kgds(6)=0                      ! oct 17, resolution and component flags
     if (igdstmpl(1)==2 ) kgds(6)=64
     if ( btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5) ) kgds(6)=kgds(6)+128
     if ( btest(igdstmpl(14),3) ) kgds(6)=kgds(6)+8

     kgds(7)=nint(float(igdstmpl(20))/float(iscale)*1000.)+90000  ! octs 18-20, Lat of cent of rotation
     kgds(8)=nint(float(igdstmpl(21))/float(iscale)*1000.)        ! octs 21-23, Lon of cent of rotation
     kgds(9)=nint(float(igdstmpl(17))/float(iscale)*500.)         ! octs 24-25, Di
                                                                  ! Note!! grib 2 convention twice grib 1
     kgds(10)=nint(float(igdstmpl(18))/float(iscale)*1000.)       ! octs 26-27, Dj

     kgds(11) = 0                   ! oct 28, scan mode
     if (btest(igdstmpl(19),7)) kgds(11) = 128
     if (btest(igdstmpl(19),6)) kgds(11) = kgds(11) +  64
     if (btest(igdstmpl(19),5)) kgds(11) = kgds(11) +  32

     kgds(12)=0    ! octs 29-32, reserved
     kgds(19)=0    ! oct 4, # vert coordinate parameters
     kgds(20)=255  ! oct 5, used for thinned grids, set to 255

     res = sqrt( (float(kgds(9)) / 1000.0)**2   +    &
                 (float(kgds(10)) / 1000.0)**2  )
     res = res * 111.0

   else   ! b-stagger

     iscale=igdstmpl(10)*igdstmpl(11)
     if (iscale == 0) iscale = 1e6
     kgds(1)=205                    ! oct 6,     rotated lat/lon for Non-E Stagger grid
     kgds(2)=igdstmpl(8)            ! octs 7-8,  Ni
     ni = kgds(2)
     kgds(3)=igdstmpl(9)            ! octs 9-10, Nj
     nj = kgds(3)
     kgds(4)=nint(float(igdstmpl(12))/float(iscale)*1000.)  ! octs 11-13, Lat of 1st grid point
     kgds(5)=nint(float(igdstmpl(13))/float(iscale)*1000.)  ! octs 14-16, Lon of 1st grid point

     kgds(6)=0                      ! oct 17, resolution and component flags
     if (igdstmpl(1)==2 ) kgds(6)=64
     if ( btest(igdstmpl(14),4).OR.btest(igdstmpl(14),5) )  kgds(6)=kgds(6)+128
     if ( btest(igdstmpl(14),3) ) kgds(6)=kgds(6)+8

     kgds(7)=nint(float(igdstmpl(20))/float(iscale)*1000.)+90000 ! octs 18-20, Lat of cent of rotation
     kgds(8)=nint(float(igdstmpl(21))/float(iscale)*1000.)       ! octs 21-23, Lon of cent of rotation
     kgds(9)=nint(float(igdstmpl(17))/float(iscale)*1000.)       ! octs 24-25, Di
     kgds(10)=nint(float(igdstmpl(18))/float(iscale)*1000.)      ! octs 26-27, Dj

     kgds(11) = 0                   ! oct 28, scan mode
     if (btest(igdstmpl(19),7)) kgds(11) = 128
     if (btest(igdstmpl(19),6)) kgds(11) = kgds(11) +  64
     if (btest(igdstmpl(19),5)) kgds(11) = kgds(11) +  32

     kgds(12)=nint(float(igdstmpl(15))/float(iscale)*1000.) ! octs 29-31, Lat of last grid point
     kgds(13)=nint(float(igdstmpl(16))/float(iscale)*1000.) ! octs 32-34, Lon of last grid point

     kgds(19)=0    ! oct 4, # vert coordinate parameters
     kgds(20)=255  ! oct 5, used for thinned grids, set to 255

     res = ((float(kgds(9)) / 1000.0) + (float(kgds(10)) / 1000.0)) &
             * 0.5 * 111.0

   endif

 else

   print*,'- FATAL ERROR CONVERTING TO GRIB2 GDT.'
   print*,'- UNRECOGNIZED GRID TYPE.'
   call w3tage('SNOW2MDL')
   call errexit(50)

 endif
 
 end subroutine gdt_to_gds

 subroutine grib2_check (kgds, igdstmplen)
!$$$  subprogram documentation block
!
! subprogram:    grib2_check
!   prgmmr: gayno          org: w/np2     date: 2014-sep-28
!
! abstract:  determine length of grib2 gds template array,
!            which is a function of the map projection.
!  
! program history log:
! 2014-sep-28  gayno    - initial version
!
! usage: call grib2_check (kgds, igdstmplen)
!
!   input argument list:  kgds - grib1 gds array
!
!   output argument list: igdstmplen - length of gds template
!                                      array.
!
! files: none
!
! condition codes:
!   47 - unrecognized grid type; fatal
!
! remarks: call this routine before init_grib2.  
!          
!$$$

 implicit none

 integer, intent(in)         :: kgds(200)
 integer, intent(  out)      :: igdstmplen

 select case (kgds(1))
   case(4)         ! gaussian
     igdstmplen = 19
   case(203, 205)  ! rotated lat/lon "B" or "E" stagger
     igdstmplen = 22
   case default
     print*,'- FATAL ERROR IN ROUTINE GRIB2_CHECK.'
     print*,'- UNRECOGNIZED GRID TYPE.'
     call w3tage('SNOW2MDL')
     call errexit(47)
 end select

 end subroutine grib2_check

 subroutine init_grib2(century, year, month, day, hour, kgds, &
                       lat11, latlast, lon11, lonlast, &
                       listsec0, listsec1, igds, ipdstmpl, ipdsnum, igdstmpl,  &
                       igdstmplen, idefnum, ideflist, ngrdpts)
!$$$  subprogram documentation block
!
! subprogram:    init_grib2
!   prgmmr: gayno          org: w/np2     date: 2014-sep-28
!
! abstract:  initialize grib2 arrays required by the ncep g2 library
!            according to grib1 gds information.  the grib1 gds is
!            held in the kgds array, which is used by the ncep ipolates
!            and w3nco (grib 1) libraries.
!  
! program history log:
! 2014-sep-28  gayno    - initial version
!
! usage: init_grib2(century, year, month, day, hour, kgds, &
!                   lat11, latlast, lon11, lonlast, &
!                   listsec0, listsec1, igds, ipdstmpl, ipdsnum, igdstmpl,  &
!                   igdstmplen, idefnum, ideflist, ngrdpts)
!
!   input argument list:  
!      century/year/month/day/hour  - current date/time info
!      kgds                         - grib1 gds information
!      igdstmplen                   - length of grib2 gdt
!                                     template.
!      lat11, lon11                 - lat/lon of first grid point
!      latlast, lonlast             - lat/lon of last grid point
!
!   output argument list: 
!      igds                         - grib2 section 3 information.
!      listsec0                     - grib2 section 0 information.
!      listsec1                     - grib2 section 1 information.
!      ipdsnum                      - grib2 pds template number
!      ipdstmpl                     - grib2 pds template array
!      igdstmpl                     - grib2 gds template array
!      idefnum/ideflist             - information for non-reg grid,
!                                     # grid points in each row. 
!      ngrdpts                      - number of model grid points.
!
! files:
!    input: none
!
!    output: none
!
! condition codes: none
!
! remarks: call routine grib2_check first to determine igdstmplen.
!          
!$$$

 implicit none

 integer, intent(in   )        :: century, year, month, day, hour
 integer, intent(in   )        :: kgds(200), igdstmplen
 integer, intent(  out)        :: igds(5)
 integer, intent(  out)        :: listsec0(2)
 integer, intent(  out)        :: listsec1(13)
 integer, intent(  out)        :: ipdstmpl(15), ipdsnum
 integer, intent(  out)        :: igdstmpl(igdstmplen)
 integer, intent(  out)        :: idefnum, ideflist
 integer, intent(  out)        :: ngrdpts

 real,    intent(in   )        :: lat11, latlast, lon11, lonlast
 real                          :: scale

! Section 0

 listsec0(1)=0  ! discipline, meteorological fields
 listsec0(2)=2  ! grib version 2

! Section 1

 listsec1(1)=7    ! id of center (ncep)
 listsec1(2)=4    ! subcenter (emc)
 listsec1(3)=8    ! master table version number. wgrib2 does not recognize later tables
 listsec1(4)= 0   ! local table not used
 listsec1(5)= 0   ! signif of ref time - analysis
 if (year == 100) then
   listsec1(6)=century*100 + year
 else
   listsec1(6)=(century-1)*100 + year
 endif
 listsec1(7)=month
 listsec1(8)=day
 listsec1(9)=hour
 listsec1(10:11)=0      ! minutes/secs
 listsec1(12)=0         ! production status of data - ops products
 listsec1(13)=0         ! type of processed products - analysis

! Section 2 - not used

! Section 3 - grid description section

 if (kgds(1) == 4) then  ! gaussian

   igdstmpl(1)=5          ! oct 15; shape of the earth, wgs84
   igdstmpl(2)=255        ! oct 16; scale factor of radius of spherical earth, not used.
   igdstmpl(3)=-1         ! octs 17-20; scale value of radius of spherical earth, not used.
   igdstmpl(4)=255        ! oct 21; scale factor of major axis of elliptical earth, not used.
   igdstmpl(5)=-1         ! octs 22-25; scaled value of major axis of elliptical earth, not used.
   igdstmpl(6)=255        ! oct 26; scale factor of minor axis of elliptical earth, not used.
   igdstmpl(7)=-1         ! octs 27-30; scaled value of minor axis of elliptical earth, not used.
   igdstmpl(8)=kgds(2)    ! octs 31-34; # "i" points
   igdstmpl(9)=kgds(3)    ! octs 35-38; # "j" points
   igdstmpl(10)=1         ! octs 39-42; basic angle
   igdstmpl(11)=10**6     ! octs 43-46; subdivisions of basic angle

   scale=float(igdstmpl(10)*igdstmpl(11))

   igdstmpl(12)=nint(lat11*scale) ! octs 47-50; lat of first grid point

   if (lon11 < 0) then
     igdstmpl(13)=nint((lon11+360.)*scale)  ! octs 51-54; lon of first grid point
   else
     igdstmpl(13)=nint(lon11*scale)
   endif

   igdstmpl(14) = 0  ! oct 55; resolution and component flags
   if (btest(kgds(6),7)) igdstmpl(14) = 48
   if (btest(kgds(6),3)) igdstmpl(14) = igdstmpl(14) + 8

   igdstmpl(15)= nint(latlast*scale) ! octs 56-59; lat of last grid point

   if (lonlast < 0) then
     igdstmpl(16)=nint((lonlast+360.)*scale)  ! octs 60-63; lon of last grid point
   else
     igdstmpl(16)=nint(lonlast*scale)
   endif

   igdstmpl(17)= nint(360.0/float(kgds(2)-1)*scale) ! octs 64-67; di of grid
   igdstmpl(18)= kgds(3)/2 ! octs 68-71; # grid pts between pole and equator

   igdstmpl(19)=0   ! oct 72; scanning mode flag
   if(btest(kgds(11),7)) igdstmpl(19)=128
   if(btest(kgds(11),6)) igdstmpl(19)=igdstmpl(19) + 64
   if(btest(kgds(11),5)) igdstmpl(19)=igdstmpl(19) + 32

   igds(1) = 0 ! oct 6; source of grid def. specif in table 3.1
   igds(2) = kgds(2)*kgds(3)  ! num grid points
   igds(3) = 0 ! # octets for additional grid pt def (use '0' for regular grid)
   igds(4) = 0 ! regular grid, no appended list
   igds(5) = 40 ! gaussian

   ngrdpts = igds(2)

!  These variables used for non-regular grids.  We are using regular grids
!  (igds(3) equals 0).

   idefnum=1
   ideflist=0

 elseif (kgds(1) == 203 .or. kgds(1) == 205) then

   igdstmpl(1)=5          ! oct 15; shape of the earth, wgs84
   igdstmpl(2)=255        ! oct 16; scale factor of radius of spherical earth, not used.
   igdstmpl(3)=-1         ! octs 17-20; scale value of radius of spherical earth, not used.
   igdstmpl(4)=255        ! oct 21; scale factor of major axis of elliptical earth, not used.
   igdstmpl(5)=-1         ! octs 22-25; scaled value of major axis of elliptical earth, not used.
   igdstmpl(6)=255        ! oct 26; scale factor of minor axis of elliptical earth, not used.
   igdstmpl(7)=-1         ! octs 27-30; scaled value of minor axis of elliptical earth, not used.
   igdstmpl(8)=kgds(2)    ! octs 31-34; # "i" points
   igdstmpl(9)=kgds(3)    ! octs 35-38; # "j" points
   igdstmpl(10)=1         ! octs 39-42; basic angle
   igdstmpl(11)=10**6     ! octs 43-46; subdivisions of basic angle

   scale=float(igdstmpl(10)*igdstmpl(11))

   igdstmpl(12)=nint(lat11*scale) ! octs 47-50; lat of first grid point

   if (lon11 < 0) then
     igdstmpl(13)=nint((lon11+360.)*scale)  ! octs 51-54; lon of first grid point
   else
     igdstmpl(13)=nint(lon11*scale)
   endif

   igdstmpl(14) = 0  ! oct 55; resolution and component flags
   if (btest(kgds(6),7)) igdstmpl(14) = 48
   if (btest(kgds(6),3)) igdstmpl(14) = igdstmpl(14) + 8

   igdstmpl(15)= nint(latlast*scale) ! octs 56-59; lat of last grid point

   if (lonlast < 0) then
     igdstmpl(16)=nint((lonlast+360.)*scale)  ! octs 60-63; lon of last grid point
   else
     igdstmpl(16)=nint(lonlast*scale)
   endif

   if (kgds(1) == 203) igdstmpl(17)= nint(float(kgds(9))*scale/500.)  ! octs 64-67; di of grid.
                                                                      ! iplib "e" grid convention
                                                                      ! is 1/2 the grib convention.
   if (kgds(1) == 205) igdstmpl(17)= nint(float(kgds(9))*scale/1000.) ! octs 64-67; di of grid

   igdstmpl(18)= nint(float(kgds(10))*scale/1000.) ! octs 68-71; dj of grid

   igdstmpl(19)=0   ! oct 72; scanning mode flag
   if(btest(kgds(11),7)) igdstmpl(19)=128
   if(btest(kgds(11),6)) igdstmpl(19)=igdstmpl(19) + 64
   if(btest(kgds(11),5)) igdstmpl(19)=igdstmpl(19) + 32
   if (kgds(1) == 203) igdstmpl(19)=igdstmpl(19) + 4

   igdstmpl(20) = nint(float(kgds(7)-90000)*scale/1000.)  ! octs 73-76; lat of south pole of projection

   if (kgds(8) < 0) then
     igdstmpl(21) = nint(float(kgds(8)+360000)*scale/1000.) ! octs 77-80; long of southern pole of projection.
   else
     igdstmpl(21) = nint(float(kgds(8))*scale/1000.) ! octs 77-80; long of southern pole of projection.
   endif

   igdstmpl(22)=0  ! octs 81-84; angle of rotation of projection

   igds(1) = 0 ! oct 6; source of grid def. specif in table 3.1
   igds(2) = kgds(2)*kgds(3)  ! num grid points
   igds(3) = 0 ! # octets for additional grid pt def (use '0' for regular grid)
   igds(4) = 0 ! regular grid, no appended list
   igds(5) = 1 ! rotated lat/lon

   ngrdpts = igds(2)

!  These variables used for non-regular grids.  We are using regular grids
!  (igds(3) equals 0).

   idefnum=1
   ideflist=0

 end if

! Section 4 - product definition section

 ipdsnum = 0            ! pds template number - table 4.0

 ipdstmpl(1)= 1         ! oct 10; parameter category
! note!! to use a parmeter number >= 192 you must set the local table to '1'
 ipdstmpl(2)= 42        ! oct 11; parameter
 ipdstmpl(3)= 0         ! oct 12; type of generating process
 ipdstmpl(4)= 255       ! oct 13; background generating process identifier
 ipdstmpl(5)= 84        ! oct 14; analysis generating process identifier
 ipdstmpl(6)= 0         ! octs 15-16; hours after ob cutoff
 ipdstmpl(7)= 0         ! oct 17; minutes after ob cutoff
 ipdstmpl(8)= 1         ! oct 18; unit of time range
 ipdstmpl(9)= 0         ! octs 19-22; forecast time in units defined by oct 18
 ipdstmpl(10)=1         ! oct 23; type of first fixed surface
 ipdstmpl(11)=0         ! oct 24; scale factor of first fixed surface
 ipdstmpl(12)=0         ! octs 25-28; scale value of first fixed surface
 ipdstmpl(13)=255       ! oct 29; type of second fixed surface
 ipdstmpl(14)=255       ! oct 30; scale factor of second fixed surface
 ipdstmpl(15)=-2147483647 ! octs 31-34; scaled value of second fixed surface
                              ! note! for these particular octets, using -1 as
                              ! missing does not work because -1 may be an actual
                              ! scaled value.  after looking thru the g2 library
                              ! and some trial and error, i determined that missing
                              ! is minus 2**31-1.

 end subroutine init_grib2

 subroutine grib2_null(gfld)
!$$$  subprogram documentation block
!
! subprogram:    grib2_null
!   prgmmr: gayno          org: w/np2     date: 2014-sep-28
!
! abstract:  nullify the grib2 gribfield pointers.
!
! program history log:
! 2014-sep-28  gayno    - initial version
!
! usage: call grib2_null with a gribfield data structure
!
!   input argument list:
!     gfld - a gribfield data structure
!
!   output argument list:
!     gfld - a gribfield data structure
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 use grib_mod

 implicit none

 type(gribfield), intent(inout)           :: gfld

 nullify(gfld%idsect)
 nullify(gfld%local)
 nullify(gfld%list_opt)
 nullify(gfld%igdtmpl)
 nullify(gfld%ipdtmpl)
 nullify(gfld%coord_list)
 nullify(gfld%idrtmpl)
 nullify(gfld%bmap)
 nullify(gfld%fld)

 end subroutine grib2_null

 subroutine grib2_free(gfld)
!$$$  subprogram documentation block
!
! subprogram:    grib2_free
!   prgmmr: gayno          org: w/np2     date: 2014-sep-28
!
! abstract:  deallocate the grib2 gribfield pointers.
!
! program history log:
! 2014-sep-28  gayno    - initial version
!
! usage: call grib2_free with a gribfield data structure
!
!   input argument list:
!     gfld - a gribfield data structure
!
!   output argument list:
!     gfld - a gribfield data structure
!
! files: none
!
! condition codes: none
!
! remarks: none
!
!$$$

 use grib_mod

 implicit none

 type(gribfield), intent(inout)    :: gfld

 if (associated(gfld%idsect)) deallocate(gfld%idsect)
 if (associated(gfld%local)) deallocate(gfld%local)
 if (associated(gfld%list_opt)) deallocate(gfld%list_opt)
 if (associated(gfld%igdtmpl)) deallocate(gfld%igdtmpl)
 if (associated(gfld%ipdtmpl)) deallocate(gfld%ipdtmpl)
 if (associated(gfld%coord_list)) deallocate(gfld%coord_list)
 if (associated(gfld%idrtmpl)) deallocate(gfld%idrtmpl)
 if (associated(gfld%bmap)) deallocate(gfld%bmap)
 if (associated(gfld%fld)) deallocate(gfld%fld)

 end subroutine grib2_free
