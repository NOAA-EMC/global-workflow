      program degrib2
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  degrib2
C   PRGMMR: Vuong          ORG: SIB         DATE: 2010-09-08
C
C ABSTRACT: This program reads GRIB2 file and makes inventory
C           of GRIB file 
C
C PROGRAM HISTORY LOG:
C 2010-09-06  Vuong
C 2011-10-03  Vuong    Added to check for reference time for PDT 4.15
C 2012-06-07  Vuong    Changed PRINT statement to WRITE with format specifier
C 2017-01-21  Vuong    Added to check for undefine values 
C
C USAGE:
C   INPUT FILES:
C     UNIT 10  - Input GRIB file
C
C   OUTPUT FILES:
C     UNIT 50  - Output GRIB file
C
C USAGE:
C COMMAND LINE:
C     degrib2  grib2_file
C   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
C     6        - STANDARD FORTRAN PRINT FILE
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     LIBRARY:
C       G2LIB    - GB_INFO, GT_GETFLD, PRLEVEL, PRVTIME
C       W3LIB    - GBYTE, SKGB
C       BACIO    - BAOPENR, BAREAD, BACLOSE
C       SYSTEM   - IARGC   FUNCTION RETURNS NUMBER OF ARGUMENT ON
C                          COMMAND LINE
C                - GETARG  ROUTINE RETURNS COMMAND LINE ARGUMENT
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C
C REMARKS: COMMAND LINE CAN HAVE ONE FILE NAME.
C     
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE:  IBM RS/6000
C
      use grib_mod
      use params
      parameter(msk1=32000,msk2=4000)
      CHARACTER(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
!      integer :: igds(5),igdstmpl(200),ipdstmpl(200),idrstmpl(200)
!      integer :: ideflist(500)
      character(len=250) :: gfile1
      character(len=8) :: pabbrev
      character(len=30) :: labbrev
      character(len=90) :: tabbrev
      INTEGER(4) NARG,IARGC,temparg
      integer :: currlen=0, numpts=0
      logical :: unpack,expand
      type(gribfield) :: gfld
      call start()
      unpack=.true.
      expand=.false.
      
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET ARGUMENTS
      NARG=IARGC()
      IF(NARG.NE.1) THEN
        CALL ERRMSG('degrib2:  Incorrect usage')
        CALL ERRMSG('Usage: degrib2 grib2file')
        CALL ERREXIT(2)
      ENDIF

      IFL1=10
      temparg=1
      CALL GETARG(temparg,gfile1)
      NCGB=LEN_TRIM(gfile1)
      CALL BAOPENR(ifl1,gfile1(1:NCGB),IOS)

      itot=0
      icount=0
      iseek=0
      do
         call skgb(ifl1,iseek,msk1,lskip,lgrib)
         if (lgrib.eq.0) exit    ! end loop at EOF or problem
         if (lgrib.gt.currlen) then
            if (allocated(cgrib)) deallocate(cgrib)
            allocate(cgrib(lgrib),stat=is)
            currlen=lgrib
         endif
         call baread(ifl1,lskip,lgrib,lengrib,cgrib)
         if (lgrib.ne.lengrib) then
            write(6,*)' degrib2: IO Error.'
            call errexit(9)
         endif
         iseek=lskip+lgrib
         icount=icount+1
         write (6,*)
         write(6,'(A,I0,A,I0)') ' GRIB MESSAGE  ',icount,'  starts at ',
     &         lskip+1
         write (6,*)

! Unpack GRIB2 field
         call gb_info(cgrib,lengrib,listsec0,listsec1,
     &                numfields,numlocal,maxlocal,ierr)
         if (ierr.ne.0) then
           write(6,'(A,I0)') ' ERROR extracting field = ',ierr
           stop 10
         endif
         itot=itot+numfields
         write(6,'(A,3(1x,I0))')'  SECTION 0: ',(listsec0(j),j=1,3)
         write(6,'(A,13(1x,I0))')'  SECTION 1: ',(listsec1(j),j=1,13)
         write(6,'(A,1x,I0,1x,A,I0,1x,A)') '  Contains ',numlocal,
     &           ' Local Sections  and  ',numfields,' data fields.'
         do n=1,numfields
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           if (ierr.ne.0) then
             write(6,'(A,I0)') ' ERROR extracting field = ',ierr
             cycle
           endif

           write (6,*)
           write(6,'(A,1x,I0)')'  FIELD ',n
           if (n==1) then
            write(6,'(A,2(1x,I0))')'  SECTION 0: ',gfld%discipline,
     &                             gfld%version
            write(6,'(A,20(1x,I0))')'  SECTION 1: ',
     &            (gfld%idsect(j),j=1,gfld%idsectlen)
           endif
           if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
              write(6,'(A,I0,A)')'  SECTION 2: ',gfld%locallen,' bytes'
           endif
           write(6,'(A,5(1x,I0))')'  SECTION 3: ',gfld%griddef,
     &                 gfld%ngrdpts, gfld%numoct_opt,
     &                 gfld%interp_opt,gfld%igdtnum
           write(6,'(A,1x,I0,A,100(1x,I0))')'  GRID TEMPLATE 3.',
     &         gfld%igdtnum,' : ', (gfld%igdtmpl(j),j=1,gfld%igdtlen)
           if ( gfld%num_opt .eq. 0 ) then
             write(6,*)' NO Optional List Defining Number of Data ' 
     &                 //'Points.'
           else
             write(6,'(A,1x,150(1x,I0))')'  Section 3 Optional List:',
     &                (gfld%list_opt(j),j=1,gfld%num_opt)
           endif

           pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),
     &                              gfld%ipdtmpl(2))
           call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)
           call prvtime(gfld%ipdtnum,gfld%ipdtmpl,listsec1,tabbrev)

           write(6,'(A,1x,I0,A,A,3(1X,I0),A,80(1x,I0))')
     &         '  PRODUCT TEMPLATE 4.', gfld%ipdtnum,
     &         ': ( PARAMETER = ', pabbrev, gfld%discipline,
     &         gfld%ipdtmpl(1),gfld%ipdtmpl(2),' ) ',
     &         (gfld%ipdtmpl(j),j=1,gfld%ipdtlen)

           write(6,'(A,A,A,A,A)')'  FIELD: ',pabbrev,trim(labbrev),
     &           " ",trim(tabbrev)
           if ( gfld%num_coord .eq. 0 ) then
             write(6,*)' NO Optional Vertical Coordinate List.'
           else
             write(6,'(A,1X,150(1x,I0))')
     &         '  Section 4 Optional & Coordinates: ',
     &             (gfld%coord_list(j),j=1,gfld%num_coord)
           endif
           if ( gfld%ibmap .ne. 255 ) then
              write(6,'(A,I0,A,I0)')'  Num. of Data Points =  ',
     &            gfld%ndpts,'    with BIT-MAP  ',gfld%ibmap
           else
              write(6,'(A,I0,A)')'  Num. of Data Points =  ',
     &            gfld%ndpts,'     NO BIT-MAP '
           endif
           write(6,'(A,I0,A,20(1x,I0))')'  DRS TEMPLATE 5. '
     &           ,gfld%idrtnum,' : ',
     &          (gfld%idrtmpl(j),j=1,gfld%idrtlen)
           if (gfld%fld(1) .eq. 9.9990003E+20 ) then  ! checking undefined values
             fldmax=0.0
             fldmin=99999.99
             sum=0.0
             numpts=0
           else
             fldmax=gfld%fld(1)
             fldmin=gfld%fld(1)
             sum=gfld%fld(1)
             numpts=1
           end if
           do j=2,gfld%ndpts
             if (gfld%fld(j) .eq. 9.9990003E+20 ) then ! checking undefined values
                cycle
             end if
             if (gfld%fld(j).gt.fldmax) fldmax=gfld%fld(j)
             if (gfld%fld(j).lt.fldmin) fldmin=gfld%fld(j)
             sum=sum+gfld%fld(j)
             numpts=numpts + 1
           enddo

           write(6,*)' Data Values:'
           write(6,'(A,I0,A,I0)')'  Num. of Data Points =  ',
     &          gfld%ndpts,'   Num. of Data Undefined = ',
     &          gfld%ndpts-numpts
           write(6,fmt='( "( PARM= ",A," ) : ",
     &     " MIN=",f25.8," AVE=",f25.8,
     &     " MAX=",f25.8)')trim(pabbrev),fldmin,
     &     sum/numpts,fldmax
          call gf_free(gfld)
         enddo

      enddo
      write(6,*)" "
      write(6,'(A,I0)')'  Total Number of Fields Found =  ',itot
      stop
      end
