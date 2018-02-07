      program cnvgrib
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C  
C MAIN PROGRAM:  cnvgrib
C   PRGMMR: Gilbert        ORG: NP11        DATE: 2003-06-06
C
C ABSTRACT: This program converts every GRIB field in a file from
C   (1) GRIB1 to GRIB2   (2) GRIB2 to GRIB1  or (3) GRIB2 to GRIB2.
C
C PROGRAM HISTORY LOG:
C 2003-06-06  Gilbert
C 2008-05-14  Vuong    Added the option -m0 (No explicit missing values 
C                      included within the datavalues, modified the options
C                      and help messages
C 2010-12-02  Vuong    Changed Master Table Version Number from 2 to 6.
C                      Add option -mastertable_ver_x where x is mater table
C                      version 2 to 10 
C 2012-03-29  Vuong    Changed Master Table Version Number from 2 to 8.
C 2013-07-24  Vuong    Changed Master Table Version Number from 2 to 11
C 2014-05-20  Vuong    Modified to correct time stamp after F252
C
C USAGE:
C   INPUT FILES:
C     UNIT 10  - Input GRIB file
C
C   OUTPUT FILES: 
C     UNIT 50  - Output GRIB file
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - cnv12, cnv21, cnv22, usage
C     LIBRARY:
C       W3LIB    - errexit
C       BACIO    - baopenr, baopenw, baclose
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   2 - Problem processing command line arguments
C          =   3 - Problem opening input GRIB file
C          =   4 - Problem opening output GRIB file
C          =   5 - Unknown conversion option
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$

      integer :: inver=0,outver=0,ipack=-1
      character(len=500) :: gfilein,gfileout,copt
      character(len=2) :: master_table_ver,curmastertab_ver
      INTEGER(4) NARG,IARGC, table_ver, mastertab
      logical :: usemiss=.false., uvvect=.true.
C
C     Set current Master table version 2
C
      curmastertab_ver='2'
      table_ver=2
      mastertab=11    ! WMO GRIB2 version 11 (released in May 8, 2013)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET ARGUMENTS


      NARG=IARGC()
      IF(NARG.lt.3) THEN       ! may be a problem with args
         IF(NARG.eq.0) THEN
            !CALL ERRMSG('cnvgrib:  Incorrect usage')
            call usage(0)
            CALL ERREXIT(2)
         ELSE                !  look for -h "help" option
            do j=1,NARG
               call getarg(j,copt)
               if (copt.eq.'-h' .or. copt.eq.'-help') then
                  call usage(1)
                  CALL ERREXIT(0)
               endif
            ENDDO
            call usage(0)
            CALL ERREXIT(2)
         ENDIF
      ELSE           
         j=1
         dowhile (j.le.NARG-2)        ! parse first narg-2 args
            call getarg(j,copt)
            j=j+1
            selectcase(copt)
             case('-g12')
                inver=1
                outver=2
             case('-g21')
                inver=2
                outver=1
             case('-g22')
                inver=2
                outver=2
             case('-p0')
                ipack=0
             case('-p2')
                ipack=2
             case('-p31')
                ipack=31
             case('-p32')
                ipack=32
             case('-p40')
                ipack=40
             case('-p41')
                ipack=41
             case('-p40000')       ! Obsolete 
                ipack=40000
             case('-p40010')       ! Obsolete 
                ipack=40010
             case('-m')
                usemiss=.true.
                imiss=1
             case('-m0')
                usemiss=.true.
                imiss=0
             case('-nv')
                uvvect=.false.
             case('-mastertable_ver_1')
                table_ver=1
                master_table_ver='1'
             case('-mastertable_ver_2')
                table_ver=2
                master_table_ver='2'
             case('-mastertable_ver_3')
                table_ver=3
                master_table_ver='3'
             case('-mastertable_ver_4')
                table_ver=4
                master_table_ver='4'
             case('-mastertable_ver_5')
                table_ver=5
                master_table_ver='5'
             case('-mastertable_ver_6')
                table_ver=6
                master_table_ver='6'
             case('-mastertable_ver_7')
                table_ver=7
                master_table_ver='7'
             case('-mastertable_ver_8')
                table_ver=8
                master_table_ver='8'
             case('-mastertable_ver_9')
                table_ver=9
                master_table_ver='9'
             case('-mastertable_ver_10')
                table_ver=10
                master_table_ver='10'
             case('-mastertable_ver_11')
                table_ver=11
                master_table_ver='11'
             case('-mastertable_ver_12')
                table_ver=12
                master_table_ver='12'
             case('-mastertable_ver_13')
                table_ver=13
                master_table_ver='13'
             case('-mastertable_ver_14')
                table_ver=14
                master_table_ver='14'
             case('-mastertable_ver_15')
                table_ver=15
                master_table_ver='15'
             case('-mastertable_ver_16')
                table_ver=16
                master_table_ver='16'
             case('-mastertable_ver_17')
                table_ver=17
                master_table_ver='17'
             case('-mastertable_ver_18')
                table_ver=18
                master_table_ver='18'
             case('-mastertable_ver_19')
                table_ver=19
                master_table_ver='19'
             case('-mastertable_ver_20')
                table_ver=20
                master_table_ver='20'
             case default
                call usage(0)
                CALL ERREXIT(2)
            end select
         ENDDO

         if ( table_ver .le. 1 .OR. 
     &      table_ver .gt. mastertab ) then
            call usage(0)
            call errmsg ('  ')
            call errmsg('cnvgrib: cannot change to master table '// 
     &                   'version ' // master_table_ver)
            call errmsg ('  ')
            call errmsg('Current GRIB master table version is '//
     &                  curmastertab_ver)
            call errmsg ('  ')
            CALL ERREXIT(2)
         end if
         !
         !   get filenames from last two arguments
         !
         CALL GETARG(NARG-1,gfilein)
         CALL GETARG(NARG,gfileout)
         !
         !   If -p option specified, must be writing out grib2
         !
         if ( (ipack.ne.-1).and.(outver.eq.1) ) then
            CALL ERRMSG('cnvgrib: -pxx option ignored when using -g21')
         endif
         !
         !   Must have -g option
         !
         if ( (inver.eq.0).or.(outver.eq.0) ) then
            CALL ERRMSG('cnvgrib: must use one -gxx option')
            call usage(0)
            CALL ERREXIT(2)
         endif
         !
         !   If -m or -m0 option specified, must be writing out grib2
         !   and using DRT 5.2 or 5.3
         !
         if ( (usemiss).and.(ipack.ne.2 .AND. ipack.ne.31 .AND.
     &                       ipack.ne.32) ) then
            CALL ERRMSG('cnvgrib: -m or -m0 option ignored when not '//
     &                 'using -p2, -p31 or -p32.')
            usemiss=.false.
         endif
      ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  Open input and output grib files
!
      IFL1=10
      IFL2=50
      NCGB=LEN_TRIM(gfilein)
      CALL BAOPENR(ifl1,gfilein(1:NCGB),IOS)
      if (IOS.NE.0) then
         call errmsg('cnvgrib: cannot open input GRIB file '//
     &               gfilein(1:NCGB))
         call errexit(3)
      endif
      NCGB=LEN_TRIM(gfileout)
      CALL BAOPENW(ifl2,gfileout(1:NCGB),IOS)
      if (IOS.NE.0) then
         call errmsg('cnvgrib: cannot open output GRIB file '//
     &               gfileout(1:NCGB))
         call errexit(4)
      endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  convert grib file
!
      if ((inver.eq.1).AND.(outver.eq.2)) then
         call cnvgfs12(ifl1,ifl2,ipack,usemiss,imiss,uvvect, table_ver)
      elseif ((inver.eq.2).AND.(outver.eq.1)) then
         call cnvgfs21(ifl1,ifl2)
      elseif ((inver.eq.2).AND.(outver.eq.2)) then
         call cnvgfs22(ifl1,ifl2,ipack,usemiss,imiss,table_ver)
      else
         print *,' Unknown conversion option.'
         call errexit(5)
      endif
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  close grib files
!
      CALL BACLOSE(ifl1,IOS)
      CALL BACLOSE(ifl2,IOS)

      stop
      end

      subroutine usage(iopt)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    usage
C   PRGMMR: Gilbert     ORG: W/NP11    DATE: 2003-06-06
C
C ABSTRACT: This routine prints out the command "usage" 
C   or a brief description of the command line options.
C
C PROGRAM HISTORY LOG:
C 2003-06-06  Gilbert
C 2007-04-25  Vuong   -  Changed the cnvgrib_ver
C 2008-08-12  Vuong   -  Changed the cnvgrib_ver
C 2009-06-01  Vuong   -  Changed the cnvgrib_ver
C 2010-01-28  Vuong   -  Changed the cnvgrib_ver
C 2010-08-05  Vuong   -  Changed the cnvgrib_ver
C 2010-12-02  Vuong   -  Changed the cnvgrib_ver
C 2011-07-12  Vuong   -  Changed the cnvgrib_ver
C 2013-07-24  Vuong   -  Changed the cnvgrib_ver
C 2017-01-24  Vuong   -  Changed the cnvgrib_ver v3.1.0
C
C USAGE:    CALL usage(iopt)
C   INPUT ARGUMENT LIST:
C     iopt   - ouput option:
C                   1 = print description of arguments
C                   otherwise, print command usage summary
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C$$$
         character(len=20) :: cnvgrib_ver="cnvgrib21_gfs-v3.1.0"
         integer,intent(in) :: iopt 

         if ( iopt.eq.0 ) then
         call errmsg ('  ')
         call errmsg('Usage: cnvgrib21_gfs [-h] {-g12|-g21|-g22} '//
     &        ' [-m|-m0] [-nv] [-mastertable_ver_x]')
         call errmsg('               [{-p0|-p2|-p31|-p32|-p40'//
     &               '|-p41}]  ingribfile   outgribfile')
         call errmsg ('  ')
         call errmsg('Usage: cnvgrib21_gfs  -h  For helps and '//
     &                                  ' shows all options') 
         call errmsg ('  ')
         endif

         if ( iopt.eq.1 ) then
            call errmsg ('  ')
            call errmsg('cnvgrib21_gfs:  version '//cnvgrib_ver)
            call errmsg ('  ')
            call errmsg('Must use one of the following options:')
            call errmsg('   -g12     converts GRIB1 to GRIB2')
            call errmsg('   -g21     converts GRIB2 to GRIB1')
            call errmsg('   -g22     converts GRIB2 to GRIB2 '//
     &                  ' (used to change packing option)')
            call errmsg ('  ')
            call errmsg('Optional packing options: (for use with '//
     &                   ' -g12 and -g22 only)')
            call errmsg('   -p0      simple packing')
            call errmsg('   -p2      complex packing')
            call errmsg('   -p31     complex pack with 1st order diffs')
            call errmsg('   -p32     complex pack with 2nd order diffs')
            call errmsg('   -p40     JPEG2000 encoding')
            call errmsg('   -p41     PNG encoding')
            call errmsg ('  ')
            call errmsg('Other Optional options: ')
         call errmsg('   -nv      Do not combine U, V wind components')
         call errmsg ('  ')
         call errmsg('   Use missing value management'//
     &                 ' instead of bitmap')
         call errmsg('   (ONLY valid with Complex Packing options:'//
     &               ' -p2, -p31 or -p32 )')
         call errmsg ('  ')
         call errmsg('   -m      Primary missing values'//
     &               ' included within the data values')
         call errmsg('   -m0     No explicit missing values'//
     &               ' included within the data values')
         call errmsg('   -mastertable_ver_x     Master Table version'//
     &               ' where x is number from 2 to 11')
         call errmsg ('  ')
         endif
      return
      end
