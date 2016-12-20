      program overpdtg2
C     SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C MAIN PROGRAM:   OVERPDTG2
C   PRGMMR: BOI VUONG           ORG: SIB        DATE: 2014-10-27
C
C ABSTRACT: THIS PROGRAM READS AN ENTIRE GRIB2 FILE FROM UNIT 11
C   AND WRITES IT BACK OUT TO UNIT 51, REPLACING THE INTERNAL
C   TYPE OF FIXED SURFACE LEVEL FROM 200 TO 10  (Entire atmosphere)
C   TO ONLT FIELD PWAT.   IT READS IN FROM UNIT 5 IN BASE 10.
C
C PROGRAM HISTORY LOG:
C   2010-09-01  BOI VUONG
C   2014-10-27  BOI VUONG   Modified program overpdtg2 to change
C                           type of fixed surface from 200 to 10200 to 10                    (Entire atmosphere) for field PWAT in
C                           AWIPS grids products
C USAGE:
C   INPUT ARGUMENT LIST:
C     UNIT  5    ID - Type of fixed surface octet(10) in PDT 4.0 (In base 10)
C     UNIT 11    Fortran unit number of input GRIB2 file
C
C   OUTPUT FILES:
C     UNIT 51    Output GRIB2 file
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C
C     LIBRARY:
C       W3LIB    - errexit
C       BACIO    - baopenr, baopenw, baclose
C
C REMARKS: None
C
C ATTRIBUTES:
C   LANGUAGE: Fortran 90
C   MACHINE:  IBM SP
C
C   
      use params
      use params_ecmwf
      use grib_mod
      use re_alloc

      integer :: ifl1,ifl3
      integer :: ipack
      character * 6  envvar
C
      CHARACTER(len=1),pointer,dimension(:) :: cgrib
      CHARACTER(len=8) :: ctemp
      type(gribfield) :: gfld,prevfld
      integer,dimension(200) :: jids,jpdt,jgdt
      character(len=80) :: gfilein,ftemplate,gfileout
      integer :: listsec0(2)=(/0,2/)
      integer :: igds(5)=(/0,0,0,0,0/),previgds(5)
      integer :: idrstmpl(200), id 
      integer :: currlen=1000000
      logical :: unpack=.true.
      logical :: open_grb=.false.
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

      allocate(cgrib(currlen))
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!  Open input and output grib files
!
      IFL1=11
      IFL3=51
C
C     Read GRIB2 data  file names from the FORTnn
C     environment variables, and open the files.
C
      envvar='FORT  '
      write(envvar(5:6),fmt='(I2)') ifl1
      call getenv(envvar,gfilein)
      CALL BAOPENR(ifl1,gfilein,IOS)
      if (IOS.NE.0) then
         call errmsg('overpdtg2: cannot open input GRIB file '//
     &               gfilein)
         call errexit(3)
      endif
C
C     Write output GRIB2 file name from FORT
C     environment variable, and open file.
C
      envvar='FORT  '
      write(envvar(5:6),fmt='(I2)') ifl3
      call getenv(envvar,gfileout)
      CALL BAOPENW(ifl3,gfileout,IOS)
      if (IOS.NE.0) then
         call errmsg('overpdtg2: cannot open output GRIB file '//
     &               gfileout)
         call errexit(4)
      endif
C
      read *,id   ! input in Model Indentifier (IN BASE 10)
C
C
      ifli1=0
      jdisc=-1
      jids=-9999
      jpdt=-9999
!     PWAT (Disc.=0; Cat=1, Parm=3)
      jgdt=-9999
      jpdtn=-1
      jgdtn=-1
!
      npoints=0
      icount=0
      jskp=0      !0 TO SEARCH FROM BEGINNING
      iret = 0
!
!     Search for PWAT (Precipitable Water) at fixed surface 
!     level 200 (Entire atmosphere) in PDT 4.0
!     PWAT (Disc.=0; Cat=1, Parm=3)
!
      foreachinputrecord: do
         prevfld=gfld
         call getgb2(ifl1,ifli1,jskp,jdisc,jids,jpdtn,jpdt,jgdtn,jgdt,
     &               unpack,jskp,gfld,iret)
         if ( iret.ne.0) then
           if ( iret.eq.99 ) exit 
            print *,' getgb2 error = ',iret
            cycle
         endif
!
! This program uses only for AWIPS grid products
! for field PWAT to change type of fixed surface
! from 200 to 10 (Entire atmosphere) PWAT (Disc.=0; Cat=1, Parm=3)
! REPLACES TYPE OF FIXED SURFACE LEVEl IN PRODUCT TEMPLATE 4.0
!
         if (gfld%ipdtmpl(1).eq.1.AND.gfld%ipdtmpl(2).eq.3
     &     .AND.gfld%ipdtmpl(3).eq.2) then
           if (gfld%ipdtmpl(10).eq.200) then
               print *,' '
               write(6,'(A,I0,A,150(1X,I0))')
     &                ' OLD PDT 4.',gfld%ipdtnum,' :',
     &                gfld%ipdtmpl(1:gfld%ipdtlen)

                gfld%ipdtmpl(10)=id

                write(6,'(A,I0,A,150(1x,I0))')
     &                  ' NEW PDT 4.',gfld%ipdtnum,' :',
     &                gfld%ipdtmpl(1:gfld%ipdtlen)
                icount=icount+1
             end if
         end if
!        
!        Determine packing information from GRIB2 file
!        The default packing is 31
!
!        print *,' gfld%idrtnum = ', gfld%idrtnum
!
         !   Set DRT info  ( packing info )
         if ( gfld%idrtnum.eq.0 ) then      ! Simple packing
            ipack = 0
         elseif ( gfld%idrtnum.eq.2 ) then  ! Complex packing
            ipack = 2
         elseif ( gfld%idrtnum.eq.3 ) then  ! Complex and spatial packing
            ipack = 31
         elseif ( gfld%idrtnum.eq.40.or.gfld%idrtnum.eq.15 ) then  ! JPEG 2000 packing
            ipack = 40
         elseif ( gfld%idrtnum.eq.41 ) then  ! PNG packing
            ipack = 41
         endif
         !
         !  Ensure that cgrib array is large enough
         !
C        print *,' gfld%ifldnum = ', gfld%ifldnum
C        print *,' gfld%ngrdpts = ', gfld%ngrdpts

         if (gfld%ifldnum == 1 ) then         ! start new GRIB2 message
            npoints=gfld%ngrdpts
         else
            npoints=npoints+gfld%ngrdpts
         endif
         newlen=npoints*4
         if ( newlen.gt.currlen ) then
            !if (allocated(cgrib)) deallocate(cgrib)
            !allocate(cgrib(newlen),stat=is)
            call realloc(cgrib,currlen,newlen,is)
            currlen=newlen
         endif
         if (gfld%ifldnum == 1 ) then         ! start new GRIB2 message
            if (open_grb) then           ! close previous GRIB2 message first
               call gribend(cgrib,lcgrib,lengrib,ierr)
               if (ierr.ne.0) then
                 write(6,*) ' ERROR ending new GRIB2 message = ',ierr
                 cycle
               endif
               open_grb=.false.
               call wryte(ifl3,lengrib,cgrib)
            endif
            !
            !   Create new GRIB Message
            !
            listsec0(1)=gfld%discipline
            listsec0(2)=gfld%version
C
            call gribcreate(cgrib,lcgrib,listsec0,gfld%idsect,ierr)
            if (ierr.ne.0) then
               write(6,*) ' ERROR creating new GRIB2 field = ',ierr
               cycle
            endif
            open_grb=.true.
         endif
         !
         !   Add grid to GRIB message, if previous grid in same
         !   message is not the same.
         !
         previgds=igds
         igds(1)=gfld%griddef
         igds(2)=gfld%ngrdpts
         igds(3)=gfld%numoct_opt
         igds(4)=gfld%interp_opt
         igds(5)=gfld%igdtnum
         if ( .NOT. associated(gfld%list_opt) )
     &                          allocate(gfld%list_opt(1))
         if (gfld%ifldnum == 1 ) then         ! add grid to GRIB2 message
            call addgrid(cgrib,lcgrib,igds,gfld%igdtmpl,gfld%igdtlen,
     &                   gfld%list_opt,gfld%num_opt,ierr)
         else         ! check if previous grid is the same as the current
           if ( gfld%igdtlen.ne.prevfld%igdtlen .OR.
     &          gfld%num_opt.ne.prevfld%num_opt .OR.
     &          any(igds.ne.previgds) .OR.
     &          any(gfld%igdtmpl(1:gfld%igdtlen).NE.
     &           prevfld%igdtmpl(1:prevfld%igdtlen)) .OR.
     &          any(gfld%list_opt(1:gfld%num_opt).NE.
     &           prevfld%list_opt(1:prevfld%num_opt)) ) then
            call addgrid(cgrib,lcgrib,igds,gfld%igdtmpl,gfld%igdtlen,
     &                   gfld%list_opt,gfld%num_opt,ierr)
           endif
         endif
         if (ierr.ne.0) then
           write(6,*) ' ERROR adding GRIB2 grid = ',ierr
           cycle
         endif
         call gf_free(prevfld)
         idrstmpl=0
         !
         !   Add field to GRIB message
         !
         !   Set DRT info  ( packing info )
         if ( ipack.eq.0 ) then
            idrsnum=0
         elseif ( ipack.eq.2 ) then
            idrsnum=2
            idrstmpl(6)=1
         elseif ( ipack.eq.31.OR.ipack.eq.32 ) then
            idrsnum=ipack/10
            idrstmpl(6)=1
            idrstmpl(17)=mod(ipack,10)      ! order of s.d.
        elseif ( ipack.eq.40 .OR. ipack.eq.41 .OR.
     &           ipack.eq.40000 .OR. ipack.eq.40010 ) then
           idrsnum=ipack
           idrstmpl(6)=0
           idrstmpl(7)=255
         else
            idrsnum=3
            idrstmpl(17)=1                  ! order of s.d.
            idrstmpl(6)=1                   ! general group split
            ctemp=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),
     &            gfld%ipdtmpl(2))
            if (ctemp.eq.'A PCP   ') idrsnum=2
         endif
         idrstmpl(2)=gfld%idrtmpl(2)
         idrstmpl(3)=gfld%idrtmpl(3)
         if ( .NOT. associated(gfld%coord_list) )
     &                        allocate(gfld%coord_list(1))
         if ( gfld%ibmap.ne.0 .AND. gfld%ibmap.ne.254) then
            if ( .NOT. associated(gfld%bmap) ) allocate(gfld%bmap(1))
         endif
         !
         !   Add field to GRIB message
         !
         call addfield(cgrib,lcgrib,gfld%ipdtnum,gfld%ipdtmpl,
     &                 gfld%ipdtlen,gfld%coord_list,gfld%num_coord,
     &                 idrsnum,idrstmpl,200,
     &                 gfld%fld,gfld%ngrdpts,gfld%ibmap,gfld%bmap,ierr)
         if (ierr.ne.0) then
           write(6,*) ' ERROR adding GRIB2 field = ',ierr
           cycle
         endif
      enddo foreachinputrecord

      if (open_grb) then           ! close last GRIB2 message
         call gribend(cgrib,lcgrib,lengrib,ierr)
         if (ierr.ne.0) then
           write(6,*) ' ERROR ending new GRIB2 message = ',ierr
           if (associated(cgrib)) deallocate(cgrib)
           call gf_free(gfld)
           call gf_free(prevfld)
!          return
         endif
         open_grb=.false.
         call wryte(ifl3,lengrib,cgrib)
      endif

      if (associated(cgrib)) deallocate(cgrib)
      call gf_free(gfld)
      call gf_free(prevfld)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  close grib files
!
      print *, '  '
      print *, ' Total Number of Records Found = ',icount
      print *, '  '
C
      CALL BACLOSE(ifl1,IOS)
      CALL BACLOSE(ifl3,IOS)

      stop
      end
