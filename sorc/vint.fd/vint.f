      program vint
c
c     ABSTRACT: This program interpolates from various pressure levels
c     onto regularly-spaced, 50-mb vertical levels.  The intent is that
c     we can use data with relatively coarse vertical resolution to 
c     get data on the necessary 50-mb intervals that we need for Bob 
c     Hart's cyclone phase space.  For each model, we will need to read
c     in a control file that contains the levels that we are 
c     interpolating from.
c
c     Written by Tim Marchok

      USE params
      USE grib_mod

      implicit none

      type(gribfield) :: holdgfld
      integer, parameter :: lugb=11,lulv=16,lugi=31,lout=51,maxlev=200
      integer  kpds(200),kgds(200)
      integer  nlevsin,iriret,iogret,kf,iggret,igdret,iidret,ixo,k,n
      integer  iha,iho,iva,irfa,iodret,ifcsthour,iia,iparm,nlevsout
      integer  gribver,g2_jpdtn
      integer  ilevs(maxlev)
      real, allocatable :: xinpdat(:,:),xoutdat(:,:),xoutlevs_p(:)
      logical(1), allocatable :: valid_pt(:),readflag(:)

      namelist/timein/ifcsthour,iparm,gribver,g2_jpdtn
c
      read (5,NML=timein,END=201)
  201 continue
      print *,' '
      print *,'*----------------------------------------------------*'
      print *,' '
      print *,' +++ Top of vint +++'
      print *,' '
      print *,'After namelist read, input forecast hour = ',ifcsthour
      print *,'                         input grib parm = ',iparm
      print *,'                         GRIB version= ',gribver
      print *,'                         GRIB2 JPDTN= g2_jpdtn= '
     &                                                      ,g2_jpdtn

      if (iparm == 7 .or. iparm == 156) then
        nlevsout = 13  ! dealing with height
      else
        nlevsout =  5  ! dealing with temperature
      endif

      allocate (xoutlevs_p(nlevsout),stat=ixo)
      if (ixo /= 0) then
        print *,' '
        print *,'!!! ERROR in vint allocating the xoutlevs_p array.'
        print *,'!!! ixo= ',ixo
        print *,' '
        goto 899
      endif

      do k = 1,nlevsout
        xoutlevs_p(k) = 300. + float((k-1)*50)
      enddo

      ilevs = -999
      call read_input_levels (lulv,maxlev,nlevsin,ilevs,iriret)

      if (iriret /= 0) then
        print *,' '
        print *,'!!! ERROR in vint. '
        print *,'!!! RETURN CODE FROM read_input_levels /= 0'
        print *,'!!! RETURN CODE = iriret = ',iriret
        print *,'!!! EXITING....'
        print *,' '
        goto 899
      endif

      call open_grib_files (lugb,lugi,lout,gribver,iogret)

      if (iogret /= 0) then
        print '(/,a45,i4,/)','!!! ERROR: in vint open_grib_files, rc= '
     &        ,iogret
        goto 899
      endif

      call getgridinfo (lugb,lugi,kf,kpds,kgds,holdgfld,ifcsthour,iparm
     &                 ,gribver,g2_jpdtn,iggret)

      allocate (xinpdat(kf,nlevsin),stat=iha)
      allocate (xoutdat(kf,nlevsout),stat=iho)
      allocate (valid_pt(kf),stat=iva)
      allocate (readflag(nlevsin),stat=irfa)
      if (iha /= 0 .or. iho /= 0 .or. iva /= 0 .or. irfa /= 0) then
        print *,' '
        print *,'!!! ERROR in vint.'
        print *,'!!! ERROR allocating the xinpdat, readflag, or the'
        print *,'!!! valid_pt array, iha= ',iha,' iva= ',iva
        print *,'!!! irfa= ',irfa,' iho= ',iho
        print *,' '
        goto 899
      endif

      print *,'hold check, holdgfld%ipdtlen = ',holdgfld%ipdtlen
      do n = 1,holdgfld%ipdtlen
        print *,'hold check, n= ',n,' holdgfld%ipdtmpl= '
     &         ,holdgfld%ipdtmpl(n)
      enddo

      call getdata (lugb,lugi,kf,valid_pt,nlevsin,ilevs,maxlev
     &           ,readflag,xinpdat,ifcsthour,iparm,gribver,g2_jpdtn
     &           ,igdret)

      call interp_data (kf,valid_pt,nlevsin,ilevs,maxlev,readflag
     &                 ,xinpdat,xoutdat,xoutlevs_p,nlevsout,iidret)

      call output_data (lout,kf,kpds,kgds,holdgfld,xoutdat,valid_pt
     &                ,xoutlevs_p,nlevsout,gribver,iodret)

      deallocate (xinpdat)
      deallocate (xoutdat)
      deallocate (valid_pt)
      deallocate (readflag)
      deallocate (xoutlevs_p)

  899 continue
c 
      stop
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_input_levels (lulv,maxlev,nlevsin,ilevs,iriret)
c 
c     ABSTRACT: This subroutine reads in a text file that contains
c     the number of input pressure levels for a given model.  The
c     format of the file goes like this, from upper levels to 
c     lower, for example:
c
c        1       200
c        2       400
c        3       500
c        4       700
c        5       850
c        6       925
c        7      1000
c
c
      implicit none

      integer    lulv,nlevsin,maxlev,iriret,inplev,ict,lvix
      integer    ilevs(maxlev)
c
      iriret=0
      ict = 0
      do while (.true.)
        
        print *,'Top of while loop in vint read_input_levels'

        read (lulv,85,end=130) lvix,inplev

        if (inplev > 0 .and. inplev <= 1000) then
          ict = ict + 1
          ilevs(ict) = inplev
        else
          print *,' '
          print *,'!!! ERROR: Input level not between 0 and 1000'
          print *,'!!!        in vint.  inplev= ',inplev
          print *,'!!! STOPPING EXECUTION'
          STOP 91
        endif

        print *,'vint readloop, ict= ',ict,' inplev= ',inplev

      enddo

   85 format (i4,1x,i4)
  130 continue

      nlevsin = ict

      print *,' '
      print *,'Total number of vint levels read in = ',nlevsin
c 
      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine getgridinfo (lugb,lugi,kf,kpds,kgds,holdgfld,ifcsthour
     &                       ,iparm,gribver,g2_jpdtn,iggret)      
c
c     ABSTRACT: The purpose of this subroutine is just to get the max
c     values of i and j and the dx and dy grid spacing intervals for the
c     grid to be used in the rest of the program.  So just read the
c     grib file to get the lon and lat data.  Also, get the info for
c     the data grid's boundaries.  This boundary information will be
c     used later in the tracking algorithm, and is accessed via Module
c     grid_bounds.
c
C     INPUT:
C     lugb     The Fortran unit number for the GRIB data file
C     lugi     The Fortran unit number for the GRIB index file
c     ifcsthour input forecast hour to search for
c     iparm    input grib parm to search for
c     gribver  integer (1 or 2) to indicate if using GRIB1 / GRIB2
c     g2_jpdtn If GRIB2 data being read, this is the value for JPDTN
c              that is input to getgb2.
C
C     OUTPUT:
c     kf       Number of gridpoints on the grid
c     kpds     pds array for a GRIB1 record
c     kgds     gds array for a GRIB1 record
c     holdgfld info for a GRIB2 record
c
C     iggret   The return code from this subroutine
c
      USE params
      USE grib_mod

      implicit none
c
      type(gribfield) :: gfld,prevfld,holdgfld
      integer,dimension(200) :: jids,jpdt,jgdt
      logical(1), allocatable :: lb(:)
      integer, parameter :: jf=4000000
      integer   jpds(200),jgds(200)
      integer   kpds(200),kgds(200)
      integer :: listsec1(13)
      integer   ila,ifa,iret,ifcsthour,imax,jmax,jskp,jdisc
      integer   lugb,lugi,kf,j,k,iggret,iparm,gribver,g2_jpdtn
      integer   jpdtn,jgdtn,npoints,icount,ipack,krec
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      logical :: unpack=.true.
      logical :: open_grb=.false.
      real, allocatable :: f(:)
      real      dx,dy
c
      iggret = 0

      allocate (lb(jf),stat=ila) 
      allocate (f(jf),stat=ifa)
      if (ila /= 0 .or. ifa /= 0) then
        print *,' '
        print *,'!!! ERROR in vint.'
        print *,'!!! ERROR in getgridinfo allocating either lb or f'
        print *,'!!! ila = ',ila,' ifa= ',ifa
        iggret = 97
        return
      endif

      if (gribver == 2) then

        ! Search for a record from a GRIB2 file

        !
        ! ---  Initialize Variables ---
        !

        gfld%idsect => NULL()
        gfld%local => NULL()
        gfld%list_opt => NULL()
        gfld%igdtmpl => NULL()
        gfld%ipdtmpl => NULL()
        gfld%coord_list => NULL()
        gfld%idrtmpl => NULL()
        gfld%bmap => NULL()
        gfld%fld => NULL()

        jdisc=0 ! meteorological products
        jids=-9999
        jpdtn=g2_jpdtn ! 0 = analysis or forecast; 1 = ens fcst
        jgdtn=0 ! lat/lon grid
        jgdt=-9999
        jpdt=-9999

        npoints=0
        icount=0
        jskp=0

c       Search for Temperature or GP Height by production template....

        JPDT(1:15)=(/-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999
     &             ,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)

        if (iparm == 7) then  ! GP Height
          jpdt(1) = 3   ! Param category from Table 4.1
          jpdt(2) = 5   ! Param number from Table 4.2-0-3
        elseif (iparm == 11) then  ! Temperature
          jpdt(1) = 0   ! Param category from Table 4.1
          jpdt(2) = 0   ! Param category from Table 4.2
        endif

        jpdt(9) = ifcsthour

        call getgb2(lugb,lugi,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt
     &             ,unpack,krec,gfld,iret)
        if ( iret.ne.0) then
          print *,' '
          print *,' ERROR: getgb2 error in getgridinfo = ',iret
        endif

c       Determine packing information from GRIB2 file
c       The default packing is 40  JPEG 2000

        ipack = 40

        print *,' gfld%idrtnum = ', gfld%idrtnum

        !   Set DRT info  ( packing info )
        if ( gfld%idrtnum.eq.0 ) then      ! Simple packing
          ipack = 0
        elseif ( gfld%idrtnum.eq.2 ) then  ! Complex packing
          ipack = 2
        elseif ( gfld%idrtnum.eq.3 ) then  ! Complex & spatial packing
          ipack = 31
        elseif ( gfld%idrtnum.eq.40.or.gfld%idrtnum.eq.15 ) then
          ! JPEG 2000 packing
          ipack = 40
        elseif ( gfld%idrtnum.eq.41 ) then  ! PNG packing
          ipack = 41
        endif

        print *,'After check of idrtnum, ipack= ',ipack

        print *,'Number of gridpts= gfld%ngrdpts= ',gfld%ngrdpts
        print *,'Number of elements= gfld%igdtlen= ',gfld%igdtlen
        print *,'PDT num= gfld%ipdtnum= ',gfld%ipdtnum
        print *,'GDT num= gfld%igdtnum= ',gfld%igdtnum

        imax = gfld%igdtmpl(8)
        print *,'at A'
        jmax = gfld%igdtmpl(9)
        print *,'at B'
        dx   = float(gfld%igdtmpl(17))/1.e6
        print *,'at C'
        dy   = float(gfld%igdtmpl(17))/1.e6
        print *,'at D'
        kf   = gfld%ngrdpts
        print *,'at E'

        holdgfld = gfld

      else

        ! Search for a record from a GRIB1 file

        jpds = -1
        jgds = -1

        j=0

        jpds(5)  = iparm ! Get a record for the input parm selected
        jpds(6)  = 100   ! Get a record on a standard pressure level
        jpds(14) = ifcsthour
         
        call getgb(lugb,lugi,jf,j,jpds,jgds,
     &                       kf,k,kpds,kgds,lb,f,iret)
      
        if (iret.ne.0) then
          print *,' '
          print *,'!!! ERROR in vint getgridinfo calling getgb'
          print *,'!!! Return code from getgb = iret = ',iret
          iggret = iret
          return
        else
          iggret=0
          imax = kgds(2)
          jmax = kgds(3)
          dx   = float(kgds(9))/1000.
          dy   = float(kgds(10))/1000.
        endif

      endif
      
      print *,' '
      print *,'In vint getgridinfo, grid dimensions follow:'
      print *,'imax= ',imax,' jmax= ',jmax
      print *,'  dx= ',dx,'  dy= ',dy
      print *,'number of gridpoints = ',kf
      
      deallocate (lb); deallocate(f)
      
      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine getdata (lugb,lugi,kf,valid_pt,nlevsin,ilevs,maxlev
     &             ,readflag,xinpdat,ifcsthour,iparm,gribver,g2_jpdtn
     &             ,igdret)
c
c     ABSTRACT: This subroutine reads the input GRIB file for the
c     tracked parameters.

      USE params
      USE grib_mod

      implicit none
c
      type(gribfield) :: gfld,prevfld
      CHARACTER(len=8) :: pabbrev
      integer,dimension(200) :: jids,jpdt,jgdt
      logical(1)  valid_pt(kf),lb(kf),readflag(nlevsin)
      integer, parameter :: jf=4000000
      integer   ilevs(maxlev)
      integer   jpds(200),jgds(200),kpds(200),kgds(200)
      integer   lugb,lugi,kf,nlevsin,maxlev,igdret,jskp,jdisc
      integer   i,j,k,ict,np,lev,ifcsthour,iret,iparm,gribver,g2_jpdtn
      integer   jpdtn,jgdtn,npoints,icount,ipack,krec
      integer   pdt_4p0_vert_level,pdt_4p0_vtime,mm
      integer :: listsec0(2)=(/0,2/)
      integer :: listsec1(13)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      logical :: unpack=.true.
      logical :: open_grb=.false.
      real      f(kf),xinpdat(kf,nlevsin),xtemp(kf)
      real      dmin,dmax,firstval,lastval
c
      igdret=0
      ict = 0

      level_loop: do lev = 1,nlevsin

        print *,' '
        print *,'In vint getdata read loop, lev= ',lev,' level= '
     &         ,ilevs(lev)

        if (gribver == 2) then

          !
          ! ---  Initialize Variables ---
          !

          gfld%idsect => NULL()
          gfld%local => NULL()
          gfld%list_opt => NULL()
          gfld%igdtmpl => NULL()
          gfld%ipdtmpl => NULL()
          gfld%coord_list => NULL()
          gfld%idrtmpl => NULL()
          gfld%bmap => NULL()
          gfld%fld => NULL()

          jdisc=0  ! meteorological products
          jids=-9999
          jpdtn=g2_jpdtn  ! 0 = analysis or forecast; 1 = ens fcst
          jgdtn=0  ! lat/lon grid
          jgdt=-9999
          jpdt=-9999

          npoints=0
          icount=0
          jskp=0

c         Search for input parameter by production template 4.0.  This
c         vint program is used primarily for temperature, but still we
c         will leave that as a variable and not-hard wire it in case we
c         choose to average something else in the future.

          ! We are looking for Temperature or GP Height here.  This 
          ! block of code, or even the smaller subset block of code that
          ! contains the JPDT(1) and JPDT(2) assignments, can of course 
          ! be modified if this program is to be used for interpolating
          ! other variables....

          ! Set defaults for JPDT, then override in array
          ! assignments below...

          JPDT(1:15)=(/-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999
     &               ,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)

          print *,' '
          print *,'In getdata vint, iparm= ',iparm

          if (iparm == 7) then  ! GP Height
            jpdt(1) = 3   ! Param category from Table 4.1
            jpdt(2) = 5   ! Param number from Table 4.2-0-3
          elseif (iparm == 11) then  ! Temperature
            jpdt(1) = 0   ! Param category from Table 4.1
            jpdt(2) = 0   ! Param category from Table 4.2
          endif

          JPDT(9)  = ifcsthour
          JPDT(10) = 100 ! Isobaric surface requested (Table 4.5)
          JPDT(12) = ilevs(lev) * 100 ! value of specific level

          print *,'before getgb2 call, value of unpack = ',unpack

          do mm = 1,15
            print *,'VINT getdata mm= ',mm,' JPDT(mm)= ',JPDT(mm)
          enddo

          call getgb2(lugb,lugi,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt
     &             ,unpack,krec,gfld,iret)

          print *,'iret from getgb2 in getdata = ',iret

          print *,'after getgb2 call, value of unpacked = '
     &           ,gfld%unpacked

          print *,'after getgb2 call, gfld%ndpts = ',gfld%ndpts
          print *,'after getgb2 call, gfld%ibmap = ',gfld%ibmap

          if ( iret == 0) then

c           Determine packing information from GRIB2 file
c           The default packing is 40  JPEG 2000

            ipack = 40

            print *,' gfld%idrtnum = ', gfld%idrtnum

            !   Set DRT info  ( packing info )
            if ( gfld%idrtnum.eq.0 ) then      ! Simple packing
              ipack = 0
            elseif ( gfld%idrtnum.eq.2 ) then  ! Complex packing
              ipack = 2
            elseif ( gfld%idrtnum.eq.3 ) then  ! Complex & spatial
     &                                         ! packing
              ipack = 31
            elseif ( gfld%idrtnum.eq.40.or.gfld%idrtnum.eq.15 ) then
              ! JPEG 2000 packing
              ipack = 40
            elseif ( gfld%idrtnum.eq.41 ) then  ! PNG packing
              ipack = 41
            endif

            print *,'After check of idrtnum, ipack= ',ipack

            print *,'Number of gridpts= gfld%ngrdpts= ',gfld%ngrdpts
            print *,'Number of elements= gfld%igdtlen= ',gfld%igdtlen
            print *,'GDT num= gfld%igdtnum= ',gfld%igdtnum

            kf = gfld%ndpts  ! Number of gridpoints returned from read

            do np = 1,kf
              xinpdat(np,lev)  = gfld%fld(np)
              xtemp(np)        = gfld%fld(np)
              if (gfld%ibmap == 0) then
                valid_pt(np)     = gfld%bmap(np)
              else
                valid_pt(np)     = .true.
              endif
            enddo

            readflag(lev) = .TRUE.
c            call bitmapchk(kf,gfld%bmap,gfld%fld,dmin,dmax)
            call bitmapchk(kf,valid_pt,xtemp,dmin,dmax)

            if (ict == 0) then
c              do np = 1,kf
c                valid_pt(np) = gfld%bmap(np)
c              enddo
              ict = ict + 1
            endif

            firstval=gfld%fld(1)
            lastval=gfld%fld(kf)

            print *,' '
            print *,' SECTION 0: discipl= ',gfld%discipline
     &             ,' gribver= ',gfld%version
            print *,' '
            print *,' SECTION 1: '

            do j = 1,gfld%idsectlen
              print *,'     sect1, j= ',j,' gfld%idsect(j)= '
     &               ,gfld%idsect(j)
            enddo

            if ( associated(gfld%local).AND.gfld%locallen.gt.0) then
              print *,' '
              print *,' SECTION 2: ',gfld%locallen,' bytes'
            else
              print *,' '
              print *,' SECTION 2 DOES NOT EXIST IN THIS RECORD'
            endif

            print *,' '
            print *,' SECTION 3: griddef= ',gfld%griddef
            print *,'            ngrdpts= ',gfld%ngrdpts
            print *,'            numoct_opt= ',gfld%numoct_opt
            print *,'            interp_opt= ',gfld%interp_opt
            print *,'            igdtnum= ',gfld%igdtnum
            print *,'            igdtlen= ',gfld%igdtlen

            print *,' '
            print '(a17,i3,a2)',' GRID TEMPLATE 3.',gfld%igdtnum,': '
            do j=1,gfld%igdtlen
              print *,'    j= ',j,' gfld%igdtmpl(j)= ',gfld%igdtmpl(j)
            enddo

            print *,' '
            print *,'     PDT num (gfld%ipdtnum) = ',gfld%ipdtnum
            print *,' '
            print '(a20,i3,a2)',' PRODUCT TEMPLATE 4.',gfld%ipdtnum,': '
            do j=1,gfld%ipdtlen
              print *,'    sect 4  j= ',j,' gfld%ipdtmpl(j)= '
     &               ,gfld%ipdtmpl(j)
            enddo

c           Print out values for data representation type

            print *,' '
            print '(a21,i3,a2)',' DATA REP TEMPLATE 5.',gfld%idrtnum
     &            ,': '
            do j=1,gfld%idrtlen
              print *,'    sect 5  j= ',j,' gfld%idrtmpl(j)= '
     &               ,gfld%idrtmpl(j)
            enddo

c           Get parameter abbrev for record that was retrieved

            pdt_4p0_vtime      = gfld%ipdtmpl(9)
            pdt_4p0_vert_level = gfld%ipdtmpl(12)

            pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1)
     &                              ,gfld%ipdtmpl(2))

            print *,' '
            write (6,131)
 131        format (' rec#   param     level  byy  bmm  bdd  bhh  '
     &             ,'fhr      npts  firstval    lastval     minval   '
     &             ,'   maxval')
            print '(i5,3x,a8,2x,6i5,2x,i8,4g12.4)'
     &          ,krec,pabbrev,pdt_4p0_vert_level/100,gfld%idsect(6)
     &             ,gfld%idsect(7),gfld%idsect(8),gfld%idsect(9)
     &             ,pdt_4p0_vtime,gfld%ndpts,firstval,lastval,dmin,dmax

            do np = 1,kf
              xinpdat(np,lev) = gfld%fld(np)
            enddo

          else

            print *,' '
            print *,'!!! ERROR: GRIB2 VINT READ IN GETDATA FAILED FOR '
     &             ,'LEVEL LEV= ',LEV
            print *,' '

            readflag(lev) = .FALSE.

            do np = 1,kf
              xinpdat(np,lev) = -99999.0
            enddo

          endif

        else

        ! Reading a GRIB1 file....

          jpds = -1
          jgds = -1
          j=0

          jpds(5) = iparm       ! grib parameter id to read in
          jpds(6) = 100         ! level id to indicate a pressure level
          jpds(7) = ilevs(lev)  ! actual level of the layer
          jpds(14) = ifcsthour  ! lead time to search for

          call getgb (lugb,lugi,jf,j,jpds,jgds,
     &                          kf,k,kpds,kgds,lb,f,iret)

          print *,' '
          print *,'After vint getgb call, j= ',j,' k= ',k,' level= '
     &           ,ilevs(lev),' iret= ',iret

          if (iret == 0) then

            readflag(lev) = .TRUE.
            call bitmapchk(kf,lb,f,dmin,dmax)

            if (ict == 0) then
              do np = 1,kf
                valid_pt(np) = lb(np)
              enddo
              ict = ict + 1
            endif

            write (6,31)
  31        format (' rec#  parm# levt lev  byy   bmm  bdd  bhh  fhr   '
     &             ,'npts  minval       maxval')
            print '(i4,2x,8i5,i8,2g12.4)',
     &           k,(kpds(i),i=5,11),kpds(14),kf,dmin,dmax

            do np = 1,kf
              xinpdat(np,lev) = f(np) 
            enddo

          else

            print *,' '
            print *,'!!! ERROR: VINT READ FAILED FOR LEVEL LEV= ',LEV
            print *,' '
  
            readflag(lev) = .FALSE.

            do np = 1,kf
              xinpdat(np,lev) = -99999.0
            enddo

          endif

        endif

      enddo level_loop
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine interp_data (kf,valid_pt,nlevsin,ilevs,maxlev,readflag
     &                 ,xinpdat,xoutdat,xoutlevs_p,nlevsout,iidret)
c
c     ABSTRACT: This routine interpolates data in between available
c     pressure levels to get data resolution at the 50-mb
c     resolution that we need for the cyclone phase space
c     diagnostics.

      implicit none

      logical(1)   valid_pt(kf),readflag(nlevsin)
      integer      ilevs(maxlev)
      integer      nlevsin,nlevsout,maxlev,kf,kout,kin,k,n,kup,klo
      integer      iidret
      real         xinpdat(kf,nlevsin),xoutdat(kf,nlevsout)
      real         xoutlevs_p(nlevsout),xoutlevs_lnp(nlevsout)
      real         xinlevs_p(nlevsin),xinlevs_lnp(nlevsin)
      real         pdiff,pdiffmin,xu,xo,xl,yu,yl
c
      iidret=0
      print *,' '
      print *,'*----------------------------------------------*'
      print *,' Listing of standard output levels follows....'
      print *,'*----------------------------------------------*'
      print *,' '

      do k = 1,nlevsout
        xoutlevs_lnp(k) = log(xoutlevs_p(k))
        write (6,81) k,xoutlevs_p(k),xoutlevs_lnp(k)
      enddo
   81 format (1x,'k= ',i3,'    p= ',f6.1,'   ln(p)= ',f9.6)

      do k = 1,nlevsin
        xinlevs_p(k) = float(ilevs(k))
        xinlevs_lnp(k) = log(xinlevs_p(k))
      enddo

c     -----------------------------------------------------------------
c     We want to loop through for all the *output* levels that we need.
c     We may have some input levels that match perfectly, often at 
c     least the standard levels like 500, 700, 850.  For these levels, 
c     just take the data directly from the input file.  For other 
c     output levels that fall between the input levels, we need to 
c     find the nearest upper and lower levels.

      output_loop: do kout = 1,nlevsout

        print *,' '
        print *,'+------------------------------------------------+'
        print *,'Top of vint output_loop, kout= ',kout,'  pressure= '
     &         ,xoutlevs_p(kout)
        
        ! Loop through all of the input levels and find the level 
        ! that is closest to the  output level from the *upper* side.
        ! And again, in this upper loop, if we hit a level that 
        ! exactly matches a needed output level, just copy that data
        ! and then cycle back to the top of output_loop.

        kup = -999
        klo = -999

        pdiffmin = 9999.0

        inp_loop_up: do kin = 1,nlevsin
          if (xinlevs_p(kin) == xoutlevs_p(kout)) then
            print *,' '
            print *,'+++ Exact level found.  kout= ',kout
            print *,'+++                    level= ',xoutlevs_p(kout)
            print *,'+++ Data copied.  No interpolation needed.'
            if (readflag(kin)) then
              do n = 1,kf
                xoutdat(n,kout) = xinpdat(n,kin)
              enddo
              cycle  output_loop
            else
              print *,' '
              print *,'!!! ERROR: readflag is FALSE in interp_data for'
              print *,'!!! level kin= ',kin,', which is a level that '
              print *,'!!! exactly matches a required output level, and'
              print *,'!!! the user has identified as being an input '
              print *,'!!! level with valid data for this model.  We '
              print *,'!!! will get the data from a different level.'
            endif
          else
            pdiff = xoutlevs_p(kout) - xinlevs_p(kin)
            if (pdiff > 0.) then  ! We have a level higher than outlev
              if (pdiff < pdiffmin) then
                pdiffmin = pdiff
                kup = kin
              endif
            endif
          endif
        enddo inp_loop_up

        pdiffmin = 9999.0

        inp_loop_lo: do kin = 1,nlevsin
          pdiff = xinlevs_p(kin) - xoutlevs_p(kout)
          if (pdiff > 0.) then  ! We have a level lower than outlev
            if (pdiff < pdiffmin) then
              pdiffmin = pdiff
              klo = kin
            endif
          endif 
        enddo inp_loop_lo

        if (kup == -999 .or. klo == -999) then
          print *,' '
          print *,'!!! ERROR: While interpolating, could not find '
          print *,'!!! either an upper or lower input level to use'
          print *,'!!! for interpolating *from*.'
          print *,'!!! kup= ',kup,' klo= ',klo
          print *,' '
          print *,'!!! STOPPING....'
          stop 91
        endif

        if (.not. readflag(kup) .or. .not. readflag(klo)) then
          print *,' '
          print *,'!!! ERROR: In interp_data, either the upper or the'
          print *,'!!! lower input level closest to the target output'
          print *,'!!! level did not have valid data read in.'
          print *,'!!! '
          write (6,91) '  upper level k= ',kup,xinlevs_p(kup)
     &                ,xinlevs_lnp(kup)
          write (6,101) xoutlevs_p(kout),xoutlevs_lnp(kout)
          write (6,91) '  lower level k= ',klo,xinlevs_p(klo)
     &                ,xinlevs_lnp(klo)
          print *,'!!! readflag upper = ',readflag(kup)
          print *,'!!! readflag lower = ',readflag(klo)
          print *,'!!! EXITING....'
          stop 92
        endif

        print *,' '
        write (6,91) '  upper level k= ',kup,xinlevs_p(kup)
     &              ,xinlevs_lnp(kup)
        write (6,101) xoutlevs_p(kout),xoutlevs_lnp(kout)
        write (6,91) '  lower level k= ',klo,xinlevs_p(klo)
     &              ,xinlevs_lnp(klo)

   91   format (1x,a17,1x,i3,'  pressure= ',f6.1,' ln(p)= ',f9.6)
  101   format (13x,'Target output pressure= ',f6.1,' ln(p)= ',f9.6)

        !--------------------------------------------------------------
        ! Now perform the linear interpolation.  Here is the notation 
        ! used in the interpolation:
        ! 
        !  xu = ln of pressure at upper level
        !  xo = ln of pressure at output level
        !  xl = ln of pressure at lower level
        !  yu = data value at upper level
        !  yl = data value at lower level
        !--------------------------------------------------------------

        xu = xinlevs_lnp(kup)
        xo = xoutlevs_lnp(kout)
        xl = xinlevs_lnp(klo)

        do n = 1,kf
          yu = xinpdat(n,kup)
          yl = xinpdat(n,klo)
          xoutdat(n,kout) = ((yl * (xo - xu)) - (yu * (xo - xl))) 
     &                    / (xl - xu)
        enddo

      enddo output_loop
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine output_data (lout,kf,kpds,kgds,holdgfld,xoutdat
     &                  ,valid_pt,xoutlevs_p,nlevsout,gribver,iodret)
c
c     ABSTRACT: This routine writes out the  output data on the 
c     specified output pressure levels.

      USE params
      USE grib_mod

      implicit none

      CHARACTER(len=1),pointer,dimension(:) :: cgrib
      type(gribfield) :: holdgfld
      logical(1) valid_pt(kf),bmap(kf)
      integer  lout,kf,lugb,lugi,iodret,nlevsout,igoret,ipret,lev
      integer  gribver,ierr,ipack,lengrib,npoints,newlen,idrsnum
      integer  numcoord,ica,n,j
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer  kpds(200),kgds(200)
      integer(4), parameter::idefnum=1
      integer(4) ideflist(idefnum),ibmap
      real     coordlist
      real     xoutdat(kf,nlevsout),xoutlevs_p(nlevsout)
c
      iodret=0
      call baopenw (lout,"fort.51",igoret)
      print *,'baopenw: igoret= ',igoret

      if (igoret /= 0) then
        print *,' '
        print *,'!!! ERROR in vint in sub output_data opening'
        print *,'!!! **OUTPUT** grib file.  baopenw return codes:'
        print *,'!!! grib file 1 return code = igoret = ',igoret
        STOP 95
        return
      endif

      levloop: do lev = 1,nlevsout

        if (gribver == 2) then
  
          ! Write data out as a GRIB2 message....

          allocate(cgrib(currlen),stat=ica)
          if (ica /= 0) then
            print *,' '
            print *,'ERROR in output_data allocating cgrib'
            print *,'ica=  ',ica
            iodret=95
            return
          endif

          !  Ensure that cgrib array is large enough

          if (holdgfld%ifldnum == 1 ) then    ! start new GRIB2 message
             npoints=holdgfld%ngrdpts
          else
             npoints=npoints+holdgfld%ngrdpts
          endif
          newlen=npoints*4
          if ( newlen.gt.currlen ) then
ccc            if (allocated(cgrib)) deallocate(cgrib)
            if (associated(cgrib)) deallocate(cgrib)
            allocate(cgrib(newlen),stat=ierr)
c            call realloc (cgrib,currlen,newlen,ierr)
            if (ierr == 0) then
              print *,' '
              print *,'re-allocate for large grib msg: '
              print *,'  currlen= ',currlen
              print *,'  newlen=  ',newlen
              currlen=newlen
            else
              print *,'ERROR returned from 2nd allocate cgrib = ',ierr
              stop 95
            endif
          endif

          !  Create new GRIB Message
          listsec0(1)=holdgfld%discipline
          listsec0(2)=holdgfld%version

          print *,'output, holdgfld%idsectlen= ',holdgfld%idsectlen
          do j = 1,holdgfld%idsectlen
            print *,'     sect1, j= ',j,' holdgfld%idsect(j)= '
     &             ,holdgfld%idsect(j)
          enddo

          call gribcreate(cgrib,currlen,listsec0,holdgfld%idsect,ierr)
          if (ierr.ne.0) then
             write(6,*) ' ERROR creating new GRIB2 field (gribcreate)= '
     &                  ,ierr
             stop 95
          endif

          previgds=igds
          igds(1)=holdgfld%griddef
          igds(2)=holdgfld%ngrdpts
          igds(3)=holdgfld%numoct_opt
          igds(4)=holdgfld%interp_opt
          igds(5)=holdgfld%igdtnum

          if (igds(3) == 0) then
            ideflist = 0
          endif

          call addgrid (cgrib,currlen,igds,holdgfld%igdtmpl
     &                 ,holdgfld%igdtlen,ideflist,idefnum,ierr)

          if (ierr.ne.0) then
            write(6,*) ' ERROR from addgrid adding GRIB2 grid = ',ierr
            stop 95
          endif

          holdgfld%ipdtmpl(12) = int(xoutlevs_p(lev)) * 100

          ipack      = 40
          idrsnum    = ipack
          idrstmpl   = 0

          idrstmpl(2)= holdgfld%idrtmpl(2)
          idrstmpl(3)= holdgfld%idrtmpl(3)
          idrstmpl(6)= 0
          idrstmpl(7)= 255

          numcoord=0
          coordlist=0.0  ! Only needed for hybrid vertical coordinate,
                         ! not here, so set it to 0.0

         ! 0   - A bit map applies to this product and is specified in
         ! this section
         ! 255 - A bit map does not apply to this product
         ibmap=255     ! Bitmap indicator (see Code Table 6.0)

         print *,' '
         print *,'output, holdgfld%ipdtlen= ',holdgfld%ipdtlen
         do n = 1,holdgfld%ipdtlen
           print *,'output, n= ',n,' holdgfld%ipdtmpl= '
     &            ,holdgfld%ipdtmpl(n)
         enddo

         print *,'output, kf= ',kf
c         do n = 1,kf
c           print *,'output, n= ',n,' xoutdat(n)= ',xoutdat(n)
c         enddo

         call addfield (cgrib,currlen,holdgfld%ipdtnum,holdgfld%ipdtmpl
     &                 ,holdgfld%ipdtlen,coordlist
     &                 ,numcoord
     &                 ,idrsnum,idrstmpl,200
     &                 ,xoutdat(1,lev),kf,ibmap,bmap,ierr)

          if (ierr /= 0) then
            write(6,*) ' ERROR from addfield adding GRIB2 data = ',ierr
            stop 95
          endif

!         Finalize  GRIB message after all grids
!         and fields have been added.  It adds the End Section ( "7777" )

          call gribend(cgrib,currlen,lengrib,ierr)
          call wryte(lout,lengrib,cgrib)

          if (ierr == 0) then
            print *,' '
            print *,'+++ GRIB2 write successful. '
            print *,'    Len of message = currlen= ',currlen
            print *,'    Len of entire GRIB2 message = lengrib= '
     &             ,lengrib
          else
            print *,' ERROR from gribend writing GRIB2 msg = ',ierr
            stop 95
          endif

        else

          ! Write data out as a GRIB1 message....

          kpds(7) = int(xoutlevs_p(lev))

          print *,'In vint, just before call to putgb, kf= ',kf
          call putgb (lout,kf,kpds,kgds,valid_pt,xoutdat(1,lev),ipret)
          print *,'In vint, just after call to putgb, kf= ',kf
          if (ipret == 0) then
            print *,' '
            print *,'+++ IPRET = 0 after call to putgb in vint'
            print *,' '
          else
            print *,' '
            print *,'!!!!!! ERROR in vint.'
            print *,'!!!!!! ERROR: IPRET NE 0 AFTER CALL TO PUTGB !!!'
            print *,'!!!!!!        Level index= ',lev
            print *,'!!!!!!           pressure= ',xoutlevs_p(lev)
            print *,' '
          endif

          write(*,980) kpds(1),kpds(2)
          write(*,981) kpds(3),kpds(4)
          write(*,982) kpds(5),kpds(6)
          write(*,983) kpds(7),kpds(8)
          write(*,984) kpds(9),kpds(10)
          write(*,985) kpds(11),kpds(12)
          write(*,986) kpds(13),kpds(14)
          write(*,987) kpds(15),kpds(16)
          write(*,988) kpds(17),kpds(18)
          write(*,989) kpds(19),kpds(20)
          write(*,990) kpds(21),kpds(22)
          write(*,991) kpds(23),kpds(24)
          write(*,992) kpds(25)
          write(*,880) kgds(1),kgds(2)
          write(*,881) kgds(3),kgds(4)
          write(*,882) kgds(5),kgds(6)
          write(*,883) kgds(7),kgds(8)
          write(*,884) kgds(9),kgds(10)
          write(*,885) kgds(11),kgds(12)
          write(*,886) kgds(13),kgds(14)
          write(*,887) kgds(15),kgds(16)
          write(*,888) kgds(17),kgds(18)
          write(*,889) kgds(19),kgds(20)
          write(*,890) kgds(21),kgds(22)

  980     format('    kpds(1)  = ',i7,'  kpds(2)  = ',i7)
  981     format('    kpds(3)  = ',i7,'  kpds(4)  = ',i7)
  982     format('    kpds(5)  = ',i7,'  kpds(6)  = ',i7)
  983     format('    kpds(7)  = ',i7,'  kpds(8)  = ',i7)
  984     format('    kpds(9)  = ',i7,'  kpds(10) = ',i7)
  985     format('    kpds(11) = ',i7,'  kpds(12) = ',i7)
  986     format('    kpds(13) = ',i7,'  kpds(14) = ',i7)
  987     format('    kpds(15) = ',i7,'  kpds(16) = ',i7)
  988     format('    kpds(17) = ',i7,'  kpds(18) = ',i7)
  989     format('    kpds(19) = ',i7,'  kpds(20) = ',i7)
  990     format('    kpds(21) = ',i7,'  kpds(22) = ',i7)
  991     format('    kpds(23) = ',i7,'  kpds(24) = ',i7)
  992     format('    kpds(25) = ',i7)
  880     format('    kgds(1)  = ',i7,'  kgds(2)  = ',i7)
  881     format('    kgds(3)  = ',i7,'  kgds(4)  = ',i7)
  882     format('    kgds(5)  = ',i7,'  kgds(6)  = ',i7)
  883     format('    kgds(7)  = ',i7,'  kgds(8)  = ',i7)
  884     format('    kgds(9)  = ',i7,'  kgds(10) = ',i7)
  885     format('    kgds(11) = ',i7,'  kgds(12) = ',i7)
  886     format('    kgds(13) = ',i7,'  kgds(14) = ',i7)
  887     format('    kgds(15) = ',i7,'  kgds(16) = ',i7)
  888     format('    kgds(17) = ',i7,'  kgds(18) = ',i7)
  889     format('    kgds(19) = ',i7,'  kgds(20) = ',i7)
  890     format('    kgds(20) = ',i7,'  kgds(22) = ',i7)

        endif

      enddo levloop
c
      return
      end
c
c-----------------------------------------------------------------------
c
c-----------------------------------------------------------------------
      subroutine open_grib_files (lugb,lugi,lout,gribver,iret)

C     ABSTRACT: This subroutine must be called before any attempt is
C     made to read from the input GRIB files.  The GRIB and index files
C     are opened with a call to baopenr.  This call to baopenr was not
C     needed in the cray version of this program (the files could be
C     opened with a simple Cray assign statement), but the GRIB-reading
C     utilities on the SP do require calls to this subroutine (it has
C     something to do with the GRIB I/O being done in C on the SP, and
C     the C I/O package needs an explicit open statement).
C
C     INPUT:
C     lugb     The Fortran unit number for the GRIB data file
C     lugi     The Fortran unit number for the GRIB index file
C     lout     The Fortran unit number for the  output grib file
c     gribver  integer (1 or 2) to indicate if using GRIB1 / GRIB2
C
C     OUTPUT:
C     iret     The return code from this subroutine

      implicit none

      character fnameg*7,fnamei*7,fnameo*7
      integer   iret,gribver,lugb,lugi,lout,igoret,iioret,iooret

      iret=0
      fnameg(1:5) = "fort."
      fnamei(1:5) = "fort."
      fnameo(1:5) = "fort."
      write(fnameg(6:7),'(I2)') lugb
      write(fnamei(6:7),'(I2)') lugi
      write(fnameo(6:7),'(I2)') lout
      call baopenr (lugb,fnameg,igoret)
      call baopenr (lugi,fnamei,iioret)
      call baopenw (lout,fnameo,iooret)

      print *,' '
      print *,'vint:  baopen: igoret= ',igoret,' iioret= ',iioret
     &       ,' iooret= ',iooret
      
      if (igoret /= 0 .or. iioret /= 0 .or. iooret /= 0) then
        print *,' '
        print *,'!!! ERROR in vint.'
        print *,'!!! ERROR in sub open_grib_files opening grib file'
        print *,'!!! or grib index file.  baopen return codes:'
        print *,'!!! grib  file return code = igoret = ',igoret
        print *,'!!! index file return code = iioret = ',iioret
        print *,'!!! output file return code = iooret = ',iooret
        iret = 93
        return
      endif
      
      return
      end
c
c-------------------------------------------------------------------
c
c-------------------------------------------------------------------
      subroutine bitmapchk (n,ld,d,dmin,dmax)
c
c     This subroutine checks the bitmap for non-existent data values.
c     Since the data from the regional models have been interpolated
c     from either a polar stereographic or lambert conformal grid
c     onto a lat/lon grid, there will be some gridpoints around the
c     edges of this lat/lon grid that have no data; these grid
c     points have been bitmapped out by Mark Iredell's interpolater.
c     To provide another means of checking for invalid data points
c     later in the program, set these bitmapped data values to a
c     value of -999.0.  The min and max of this array are also
c     returned if a user wants to check for reasonable values.
c
      logical(1) ld
      dimension  ld(n),d(n)
c
      dmin=1.E15
      dmax=-1.E15
c
      do i=1,n
        if (ld(i)) then
          dmin=min(dmin,d(i))
          dmax=max(dmax,d(i))
        else
          d(i) = -999.0
        endif
      enddo
c
      return
      end
