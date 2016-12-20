      program tave
c
c     ABSTRACT: This program averages the temperatures from an input 
c     grib file and produces an output grib file containing the mean
c     temperature in the 300-500 mb layer.  For each model and each
c     lead time, there will need to be data from 300 to 500 mb in 
c     50 mb increments, such that all 5 of these layers then get
c     averaged together.
c
c     Written by Tim Marchok

      USE params
      USE grib_mod

      implicit none

      type(gribfield) :: holdgfld
      integer, parameter :: lugb=11,lulv=16,lugi=31,lout=51
      integer, parameter :: nlevsout=1,nlevsin=5
      integer  kpds(200),kgds(200)
      integer  iriret,iogret,kf,iggret,igdret,iidret,gribver,g2_jpdtn
      integer  iha,iho,iva,irfa,iodret,ifcsthour,iia,iparm
      integer  ilevs(nlevsin)
      real, allocatable :: xinptmp(:,:),xouttmp(:)
      logical(1), allocatable :: valid_pt(:),readflag(:)
      real     xoutlev

      namelist/timein/ifcsthour,iparm,gribver,g2_jpdtn
c
      data ilevs   /300, 350, 400, 450, 500/
      xoutlev = 401.
c
      read (5,NML=timein,END=201)
  201 continue
      print *,' '
      print *,'*---------------------------------------------*'
      print *,' '
      print *,' +++ Top of tave +++ '
      print *,' '
      print *,'After tave namelist read, input forecast hour= '
     &       ,ifcsthour
      print *,'                          input GRIB parm= ',iparm
      print *,'                          GRIB version= ',gribver
      print *,'                         GRIB2 JPDTN= g2_jpdtn= '
     &                                                      ,g2_jpdtn

c      ilevs = -999
c      call read_input_levels (lulv,nlevsin,ilevs,iriret)
c
c      if (iriret /= 0) then
c        print *,' '
c        print *,'!!! RETURN CODE FROM read_input_levels /= 0'
c        print *,'!!! RETURN CODE = iriret = ',iriret
c        print *,'!!! EXITING....'
c        print *,' '
c        goto 899
c      endif

      call open_grib_files (lugb,lugi,lout,gribver,iogret)
      if (iogret /= 0) then
        print '(/,a35,a5,i4,/)','!!! ERROR: in tave open_grib_files,'
     &        ,' rc= ',iogret
        goto 899
      endif
      call getgridinfo (lugb,lugi,kf,kpds,kgds,holdgfld,ifcsthour,iparm
     &                 ,gribver,g2_jpdtn,iggret)

      allocate (xinptmp(kf,nlevsin),stat=iha)
      allocate (xouttmp(kf),stat=iho)
      allocate (valid_pt(kf),stat=iva)
      allocate (readflag(nlevsin),stat=irfa)
      if (iha /= 0 .or. iho /= 0 .or. iva /= 0 .or. irfa /= 0) then
        print *,' '
        print *,'!!! ERROR in tave allocating arrays.'
        print *,'!!! ERROR allocating the xinptmp, readflag, or the'
        print *,'!!! valid_pt array, iha= ',iha,' iva= ',iva
        print *,'!!! irfa= ',irfa,' iho= ',iho
        print *,'  '
        goto 899
      endif

      call getdata (lugb,lugi,kf,valid_pt,nlevsin,ilevs
     &             ,readflag,xinptmp,ifcsthour,iparm,gribver
     &             ,g2_jpdtn,igdret)

      call average_data (kf,valid_pt,nlevsin,ilevs,readflag
     &                  ,xinptmp,xouttmp,iidret)

      call output_data (lout,kf,kpds,kgds,holdgfld,xouttmp,valid_pt
     &                ,xoutlev,nlevsout,gribver,ifcsthour,iodret)

      deallocate (xinptmp)
      deallocate (xouttmp)
      deallocate (valid_pt)
      deallocate (readflag)

  899 continue
c 
      stop
      end
c
c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine read_input_levels (lulv,nlevsin,ilevs,iriret)
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

      integer    lulv,nlevsin,iriret,inplev,ict,lvix
      integer    ilevs(nlevsin)
c
      iriret=0
      ict = 0
      do while (.true.)
        
        print *,'Top of while loop in tave read_input_levels'

        read (lulv,85,end=130) lvix,inplev

        if (inplev > 0 .and. inplev <= 1000) then
          ict = ict + 1
          ilevs(ict) = inplev
        else
          print *,' '
          print *,'!!! ERROR: Input level not between 0 and 1000'
          print *,'!!!        in tave.  inplev= ',inplev
          print *,'!!! STOPPING EXECUTION'
          STOP 91
        endif

        print *,'tave readloop, ict= ',ict,' inplev= ',inplev

      enddo

   85 format (i4,1x,i4)
  130 continue

      nlevsin = ict

      print *,' '
      print *,'Total number of tave levels read in = ',nlevsin
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
c     the data grids boundaries.  This boundary information will be
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
      CHARACTER(len=8) :: ctemp
      CHARACTER(len=80) :: ftemplate
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
        print *,'!!! ERROR in tave.'
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

        jdisc=0  ! Meteorological products
        jids=-9999
        jpdtn=g2_jpdtn  ! 0 = analysis or forecast; 1 = ens fcst
        jgdtn=0  ! lat/lon grid
        jgdt=-9999
        jpdt=-9999

        npoints=0
        icount=0
        jskp=0

c       Search for Temperature by production template 4.0

        JPDT(1:15)=(/ -9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999
     &             ,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)

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
        jmax = gfld%igdtmpl(9)
        dx   = float(gfld%igdtmpl(17))/1.e6
        dy   = float(gfld%igdtmpl(17))/1.e6
        kf   = gfld%ngrdpts

        holdgfld = gfld
    
      else

        ! Search for a record from a GRIB1 file

        jpds = -1
        jgds = -1

        j=0

        jpds(5)  = iparm  ! Get a temperature record
        jpds(6)  = 100    ! Get a record on a standard pressure level
        jpds(14) = ifcsthour
         
        call getgb(lugb,lugi,jf,j,jpds,jgds,
     &                       kf,k,kpds,kgds,lb,f,iret)

        if (iret.ne.0) then
          print *,' '
          print *,'!!! ERROR in tave  getgridinfo calling getgb'
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
      print *,'In getgridinfo, grid dimensions follow:'
      print *,'imax= ',imax,' jmax= ',jmax
      print *,'  dx= ',dx,'  dy= ',dy
      print *,'number of gridpoints = ',kf
      
      deallocate (lb); deallocate(f)
      
      return
      end

c---------------------------------------------------------------------
c
c---------------------------------------------------------------------
      subroutine getdata (lugb,lugi,kf,valid_pt,nlevsin,ilevs
     &             ,readflag,xinptmp,ifcsthour,iparm,gribver
     &             ,g2_jpdtn,igdret)
c
c     ABSTRACT: This subroutine reads the input GRIB file for the
c     tracked parameters.

      USE params
      USE grib_mod

      implicit none
c
      type(gribfield) :: gfld,prevfld
      CHARACTER(len=8) :: ctemp,pabbrev
      CHARACTER(len=80) :: ftemplate
      integer,dimension(200) :: jids,jpdt,jgdt
      integer, parameter :: jf=4000000
      integer   ilevs(nlevsin)
      integer   jpds(200),jgds(200),kpds(200),kgds(200)
      integer   lugb,lugi,kf,nlevsin,igdret,iparm,jskp,jdisc
      integer   jpdtn,jgdtn,npoints,icount,ipack,krec
      integer   i,j,k,ict,np,lev,ifcsthour,iret,gribver,g2_jpdtn
      integer   pdt_4p0_vert_level,pdt_4p0_vtime,mm
      integer :: listsec0(2)=(/0,2/)
      integer :: listsec1(13)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      logical :: unpack=.true.
      logical :: open_grb=.false.
      logical(1)  valid_pt(kf),lb(kf),readflag(nlevsin)
      real      f(kf),xinptmp(kf,nlevsin),xtemp(kf)
      real      dmin,dmax,firstval,lastval
c
      igdret=0
      ict = 0

      print *,'At top of getdata, ifcsthour= ',ifcsthour

      level_loop: do lev = 1,nlevsin

        print *,' '
        print *,'------------------------------------------------'
        print *,'In tave  getdata read loop, lev= ',lev,' level= '
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

          jdisc=0  ! Meteorological products
          jids=-9999
          jpdtn=g2_jpdtn ! 0 = analysis or forecast; 1 = ens fcst
          jgdtn=0  ! lat/lon grid
          jgdt=-9999
          jpdt=-9999

          npoints=0
          icount=0
          jskp=0

c         Search for input parameter by production template 4.0.  This
c         tave program is used primarily for temperature, but still we
c         will leave that as a variable and not-hard wire it in case we
c         choose to average something else in the future.

          if (iparm == 11) then

            ! Set defaults for JPDT, then override in array 
            ! assignments below...

            JPDT(1:15)=(/ -9999,-9999,-9999,-9999,-9999,-9999,-9999
     &             ,-9999,-9999,-9999,-9999,-9999,-9999,-9999,-9999/)
            JPDT(1)  = 0   ! Param category from Table 4.1
            JPDT(2)  = 0   ! Param number from Table 4.2
            JPDT(9)  = ifcsthour
            JPDT(10) = 100 ! Isobaric surface requested (Table 4.5)
            JPDT(12) = ilevs(lev) * 100 ! value of specific level

            print *,'In getdata, just set JPDT inputs....'

          endif

          print *,'before getgb2 call, value of unpack = ',unpack
 
          do mm = 1,15
            print *,'tave getdata mm= ',mm,' JPDT(mm)= ',JPDT(mm)
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
              xinptmp(np,lev)  = gfld%fld(np)
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

            pdt_4p0_vtime      = gfld%ipdtmpl(9)
            pdt_4p0_vert_level = gfld%ipdtmpl(12)

c           Get parameter abbrev for record that was retrieved

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

c            do np = 1,kf
c              xinptmp(np,lev) = gfld%fld(np)
c            enddo

          else

            print *,' '
            print *,'!!! ERROR: GRIB2 TAVE READ IN GETDATA FAILED FOR '
     &             ,'LEVEL LEV= ',LEV
            print *,' '

            readflag(lev) = .FALSE.

            do np = 1,kf
              xinptmp(np,lev) = -99999.0
            enddo

          endif

        else

          ! Reading a GRIB1 file....

          jpds = -1
          jgds = -1
          j=0

          jpds(5) = iparm       ! parameter id for temperature
          jpds(6) = 100         ! level id to indicate a pressure level
          jpds(7) = ilevs(lev)  ! actual level of the layer
          jpds(14) = ifcsthour  ! lead time to search for

          call getgb (lugb,lugi,jf,j,jpds,jgds,
     &                          kf,k,kpds,kgds,lb,f,iret)

          print *,' '
          print *,'After tave getgb call, j= ',j,' k= ',k,' level= '
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
  31        format (' rec#  parm# levt lev  byy   bmm  bdd  bhh  fhr  '
     &             ,'npts  minval       maxval')
            print '(i4,2x,8i5,i8,2g12.4)',
     &           k,(kpds(i),i=5,11),kpds(14),kf,dmin,dmax

            do np = 1,kf
              xinptmp(np,lev) = f(np) 
            enddo

          else
  
            print *,' '
            print *,'!!! ERROR: TAVE READ FAILED FOR LEVEL LEV= ',LEV
            print *,' '

            readflag(lev) = .FALSE.

            do np = 1,kf
              xinptmp(np,lev) = -99999.0
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
      subroutine average_data (kf,valid_pt,nlevsin,ilevs,readflag
     &                 ,xinptmp,xouttmp,iidret)
c
c     ABSTRACT: This routine averages data between 300 and 500 mb to get
c     a mean temperature at 400 mb.  The input data should be at 50 mb 
c     resolution, giving 5 input levels in total.

      implicit none

      logical(1)   valid_pt(kf),readflag(nlevsin)
      integer      ilevs(nlevsin)
      integer      nlevsin,kf,k,n,iidret
      real         xinptmp(kf,nlevsin),xouttmp(kf)
      real         xinlevs_p(nlevsin),xinlevs_lnp(nlevsin)
      real         xsum
c
      iidret=0
      print *,'*----------------------------------------------*'
      print *,'         Top of average data routine'
      print *,'*----------------------------------------------*'
      print *,' '

      do n = 1,kf
        xsum = 0.0
c        print *,' '
        do k = 1,nlevsin
          xsum = xsum + xinptmp(n,k)
c          print *,'n= ',n,' k= ',k,' xsum= ',xsum
        enddo
        xouttmp(n) = xsum / float(nlevsin)
c        print *,'n= ',n,' mean= ',xouttmp(n)
      enddo
c
      return
      end
c
c----------------------------------------------------------------------
c
c----------------------------------------------------------------------
      subroutine output_data (lout,kf,kpds,kgds,holdgfld,xouttmp
     &             ,valid_pt,xoutlev,nlevsout,gribver,ifcsthour,iodret)
c
c     ABSTRACT: This routine writes out the  output data on the 
c     specified output pressure levels.

      USE params
      USE grib_mod

      implicit none

      CHARACTER(len=1),pointer,dimension(:) :: cgrib
c      CHARACTER(len=1),pointer,allocatable :: cgrib(:)
      type(gribfield) :: holdgfld
      logical(1) valid_pt(kf),bmap(kf)
      integer  lout,kf,lugb,lugi,iodret,nlevsout,igoret,ipret,lev
      integer  gribver,ierr,ipack,lengrib,npoints,newlen,idrsnum
      integer  numcoord,ica,n,j,ifcsthour
      integer :: idrstmpl(200)
      integer :: currlen=1000000
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer  kpds(200),kgds(200)
      integer(4), parameter::idefnum=1
      integer(4) ideflist(idefnum),ibmap
      real     xouttmp(kf),xoutlev,coordlist
c
      iodret=0
      call baopenw (lout,"fort.51",igoret)
      print *,'baopenw: igoret= ',igoret

      if (igoret /= 0) then
        print *,' '
        print *,'!!! ERROR in sub output_data opening'
        print *,'!!! **OUTPUT** grib file.  baopenw return codes:'
        print *,'!!! grib file 1 return code = igoret = ',igoret
        STOP 95
        return
      endif

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
        
        if (holdgfld%ifldnum == 1 ) then     ! start new GRIB2 message
           npoints=holdgfld%ngrdpts
        else
           npoints=npoints+holdgfld%ngrdpts
        endif
        newlen=npoints*4
        if ( newlen.gt.currlen ) then
ccc          if (allocated(cgrib)) deallocate(cgrib)
          if (associated(cgrib)) deallocate(cgrib)
          allocate(cgrib(newlen),stat=ierr)
c          call realloc (cgrib,currlen,newlen,ierr)
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
     &           ,holdgfld%idsect(j)
        enddo

        call gribcreate(cgrib,currlen,listsec0,holdgfld%idsect,ierr)
        if (ierr.ne.0) then
           write(6,*) ' ERROR creating new GRIB2 field (gribcreate)= '
     &                ,ierr
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
     &               ,holdgfld%igdtlen,ideflist,idefnum,ierr)

        if (ierr.ne.0) then
          write(6,*) ' ERROR from addgrid adding GRIB2 grid = ',ierr
          stop 95
        endif


        holdgfld%ipdtmpl(12) = int(xoutlev) * 100 

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
     &          ,holdgfld%ipdtmpl(n)
       enddo

       print *,'output, kf= ',kf

c       if (ifcsthour < 6) then
c         do n = 1,kf
cc           print *,'output, n= ',n,' xouttmp(n)= ',xouttmp(n)
c           write (92,151) n,xouttmp(n)
c  151      format (1x,'n= ',i6,'  xouttmp(n)= ',f10.4)
c         enddo
c       endif

       call addfield (cgrib,currlen,holdgfld%ipdtnum,holdgfld%ipdtmpl
     &               ,holdgfld%ipdtlen,coordlist
     &               ,numcoord
     &               ,idrsnum,idrstmpl,200
     &               ,xouttmp,kf,ibmap,bmap,ierr)

        if (ierr /= 0) then
          write(6,*) ' ERROR from addfield adding GRIB2 data = ',ierr
          stop 95
        endif

!       Finalize  GRIB message after all grids
!       and fields have been added.  It adds the End Section ( "7777" )

        call gribend(cgrib,currlen,lengrib,ierr)
        call wryte(lout,lengrib,cgrib)

        if (ierr == 0) then
          print *,' '
          print *,'+++ GRIB2 write successful. '
          print *,'    Len of message = currlen= ',currlen
          print *,'    Len of entire GRIB2 message = lengrib= ',lengrib
        else
          print *,' ERROR from gribend writing GRIB2 msg = ',ierr
          stop 95
        endif

      else

        ! Write data out as a GRIB1 message....

        kpds(6) = 100

        do lev = 1,nlevsout 

          kpds(7) = int(xoutlev)

          print *,'tave:  just before call to putgb, kf= ',kf

          print *,'output, kf= ',kf
c          do n = 1,kf
c            print *,'output, n= ',n,' xouttmp(n)= ',xouttmp(n)
c          enddo

          if (ifcsthour < 6) then
            do n = 1,kf
c              print *,'output, n= ',n,' xouttmp(n)= ',xouttmp(n)
              write (91,161) n,xouttmp(n)
  161         format (1x,'n= ',i6,'  xouttmp(n)= ',f10.4)
            enddo
          endif

          call putgb (lout,kf,kpds,kgds,valid_pt,xouttmp,ipret)
          print *,'tave:  just after call to putgb, kf= ',kf
          if (ipret == 0) then
            print *,' '
            print *,'+++ IPRET = 0 after call to putgb'
            print *,' '
          else
            print *,' '
            print *,'!!!!!! ERROR in tave'
            print *,'!!!!!! ERROR: IPRET NE 0 AFTER CALL TO PUTGB !!!'
            print *,'!!!!!!        Level index= ',lev
            print *,'!!!!!!           pressure= ',xoutlev
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

        enddo

  980   format('    kpds(1)  = ',i7,'  kpds(2)  = ',i7)
  981   format('    kpds(3)  = ',i7,'  kpds(4)  = ',i7)
  982   format('    kpds(5)  = ',i7,'  kpds(6)  = ',i7)
  983   format('    kpds(7)  = ',i7,'  kpds(8)  = ',i7)
  984   format('    kpds(9)  = ',i7,'  kpds(10) = ',i7)
  985   format('    kpds(11) = ',i7,'  kpds(12) = ',i7)
  986   format('    kpds(13) = ',i7,'  kpds(14) = ',i7)
  987   format('    kpds(15) = ',i7,'  kpds(16) = ',i7)
  988   format('    kpds(17) = ',i7,'  kpds(18) = ',i7)
  989   format('    kpds(19) = ',i7,'  kpds(20) = ',i7)
  990   format('    kpds(21) = ',i7,'  kpds(22) = ',i7)
  991   format('    kpds(23) = ',i7,'  kpds(24) = ',i7)
  992   format('    kpds(25) = ',i7)
  880   format('    kgds(1)  = ',i7,'  kgds(2)  = ',i7)
  881   format('    kgds(3)  = ',i7,'  kgds(4)  = ',i7)
  882   format('    kgds(5)  = ',i7,'  kgds(6)  = ',i7)
  883   format('    kgds(7)  = ',i7,'  kgds(8)  = ',i7)
  884   format('    kgds(9)  = ',i7,'  kgds(10) = ',i7)
  885   format('    kgds(11) = ',i7,'  kgds(12) = ',i7)
  886   format('    kgds(13) = ',i7,'  kgds(14) = ',i7)
  887   format('    kgds(15) = ',i7,'  kgds(16) = ',i7)
  888   format('    kgds(17) = ',i7,'  kgds(18) = ',i7)
  889   format('    kgds(19) = ',i7,'  kgds(20) = ',i7)
  890   format('    kgds(20) = ',i7,'  kgds(22) = ',i7)

      endif
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
      print *,'tave baopen: igoret= ',igoret,' iioret= ',iioret
     &       ,' iooret= ',iooret
      
      if (igoret /= 0 .or. iioret /= 0 .or. iooret /= 0) then
        print *,' '
        print *,'!!! ERROR in tave'
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
