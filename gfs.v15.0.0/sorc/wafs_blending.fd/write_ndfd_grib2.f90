!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
       subroutine write_ndfd_grib2(g,npt,nflds,npdt,ncat,nparm,ifcsthr,nlevtype,  &
                 nlev,nspatial,typeproc, & 
                 isign,y0,m0,d0,h0,m1,s0,ityped,filename)

!******************************************************************
!  prgmmr: pondeca           org: np20         date: 2006-03-03   *
!                                                                 *
!  abstract:                                                      *
!  use steve gilbert's gribcreate, addgrid, addfield, and gribend *
!  subroutines to write NDFD files in Grib2 format                * 
!                                                                 *
!  program history log:                                           *
!    2006-03-03  pondeca                                          *
!                                                                 *
!  input argument list:                                           *
!                                                                 *              
! 1. g(nx,ny,nflds): floating point array that contains all the   * 
!    nflds fields of dimension nx*ny each                         *
!                                                                 *
!                                                                 *
! 2. typeproc(nflds): character array that contains process types *
!    (analysis, forecast, or analysis error)                      *
!                                                                 *
! 3. y0,m0,d0,h0,m1,s0: reference time (year, month, day, hour,   *
!    minutes and seconds)                                         *
!                                                                 *
! 4. isign: significance of reference time. Values of interest to *
!    NDFD are 0 for analysis and 1 for start of forecast.         *
!                                                                 *
! 5. ityped: type of processed data. Values of interest to        *
!    NDFD are 0 for analysis products and 1 for forecast products *
!                                                                 *
!  output argument list:                                          *
! 1. filename: Name of output Grib2 file                          *
!                                                                 *
! attributes:                                                     *
!   language: f90                                                 *
!   machine:  ibm RS/6000 SP                                      *
!******************************************************************
      implicit none

      integer(4) npt,nflds,isign,y0,m0,d0,h0,m1,s0,ityped
 
      integer(4), parameter::max_bytes=200*130000
      integer(4), parameter::igdstmplen=19
      integer(4), parameter::idefnum=1
      integer(4), parameter::ipdstmplen=18
      integer(4), parameter::idrstmplen=5

      integer(4) listsec0(2)
      integer(4) listsec1(13)
      integer(4) ierr,n,i,j,ij
      integer(4) lengrib,dscal
      integer(4) lunout

      integer(4) igds(5)
      integer(4) igdstmpl(igdstmplen) 
      integer(4) ideflist(idefnum)     
      integer(4) ipdstmpl(ipdstmplen)
      integer(4) idrstmpl(idrstmplen)
      integer(4) idrsnum,ibmap,numcoord,ipdsnum
      
!      integer(4) ncat,nparm,nlevtype,nlev,ifcsthr,npdt,nspatial
      integer(4),dimension(nflds) :: ncat,nparm,nlevtype,nlev,ifcsthr,npdt,nspatial
      character*60 filename
!      character*60 varname(nflds)
      character*60 typeproc
      character*1 cgrib(max_bytes)

      logical*1 bmap(npt)
 
      real(4) g(npt,nflds)
      real(4) coordlist(1)
      real(4) fld(npt)

!==>open file that will store grib2 messages
      call getlun90(lunout,1)

      print*,'about to open',lunout,trim(filename)

      call baopenw(lunout,trim(filename),ierr)

      print*,'write_ndfd_grib2:  opened ',lunout, &
             'for grib2 data  ',trim(filename), &
             'return code is ',ierr

!      print*,'Debug: write_ndfd_grib',npt,nflds,npdt,ncat,nparm,ifcsthr,nlevtype,nlev,nspatial,typeproc, & 
!                 isign,y0,m0,d0,h0,m1,s0,ityped
		 		 
      do n=1,nflds
       print*,'Debug: write_ndfd_grib',npt,nflds,npdt(n),ncat(n),nparm(n) &
        ,ifcsthr(n),nlevtype(n),nlev(n),nspatial(n),typeproc, & 
                 isign,y0,m0,d0,h0,m1,s0,ityped

!==>initialize new GRIB2 message and pack
! GRIB2 sections 0 (Indicator Section) and 1 (Identification 
! Section)

       listsec0(1)=0 ! Discipline-GRIB Master Table Number (see Code Table 0.0)
       listsec0(2)=2 ! GRIB Edition Number (currently 2)
       
       listsec1(1)=7       ! Id of orginating centre (Common Code Table C-1)
       listsec1(2)=4 !"EMC"! Id of orginating sub-centre (local table)/Table C of ON388
! Per Vuong, GFS master table is 6 for WAFS
       listsec1(3)=2       ! GRIB Master Tables Version Number (Code Table 1.0)
       listsec1(4)=1 !per Brent! GRIB Local Tables Version Number (Code Table 1.1)
       listsec1(5)=isign   ! Significance of Reference Time (Code Table 1.2)
       listsec1(6)=y0      ! Reference Time - Year (4 digits)
       listsec1(7)=m0      ! Reference Time - Month
       listsec1(8)=d0      ! Reference Time - Day
       listsec1(9)=h0      ! Reference Time - Hour
       listsec1(10)=m1     ! Reference Time - Minute
       listsec1(11)=s0     ! Reference Time - Second
       listsec1(12)=0      ! Production status of data (Code Table 1.3)
       listsec1(13)=ityped ! Type of processed data (Code Table 1.4)
        
       call gribcreate(cgrib,max_bytes,listsec0,listsec1,ierr)
       print*,'gribcreate status=',ierr

!==> Pack up Grid Definition Section (Section 3) add to GRIB2 message.

       igds(1)=0      !Source of grid definition (see Code Table 3.0)
       igds(2)=npt  !Number of grid points in the defined grid.
       igds(3)=0      !Number of octets needed for each additional grid points definition
       igds(4)=0      !Interpretation of list for optional points definition (Code Table 3.11)
       igds(5)=0     !Grid Definition Template Number (Code Table 3.1)

       call apply_template_300_ndfd(igdstmpl,igdstmplen) 

       ideflist=0     !Used if igds(3) .ne. 0. Dummy array otherwise

       call addgrid(cgrib,max_bytes,igds,igdstmpl,igdstmplen,ideflist,idefnum,ierr)
       print*,'addgrid status=',ierr

       
!==> pack up sections 4 through 7 for a given field and add them to a GRIB2 message.  
! They are Product Definition Section, Data Representation Section, Bit-Map Section 
! and Data Section, respectively.


       ipdsnum=npdt(n)    !Product Definition Template Number ( see Code Table 4.0)
!       ipdsnum=0 ! use 0 product template for now to use grads

!       print*,'write_ndfd_grib2:, n,varname,typeproc=', & 
!                                n,varname(n),typeproc(n)

       call apply_template_40_ndfd(ipdstmpl,ipdstmplen,ifcsthr(n), & 
                                ncat(n),nparm(n),nlevtype(n),nlev(n) &
				,nspatial(n),typeproc)
       print*,'product template in new Grib file= ',ipdstmpl
       numcoord=0
       coordlist=0. !needed for hybrid vertical coordinate
       idrsnum=0    !Data Representation Template Number ( see Code Table 5.0 )

       call apply_template_50_ndfd(idrstmpl,idrstmplen, & 
                                ncat(n),nparm(n),nlevtype(n))
       dscal=idrstmpl(3)

!       ij=0 
!       do j=1,ny
!       do i=1,nx
       do ij=1,npt
!          ij=ij+1
          fld(ij)=g(ij,n)
       enddo
!       enddo

       ibmap=255     ! Bitmap indicator ( see Code Table 6.0 )

       call addfield(cgrib,max_bytes,ipdsnum,ipdstmpl,ipdstmplen, &
                          coordlist,numcoord,idrsnum,idrstmpl, &
                          idrstmplen,fld,npt,ibmap,bmap,ierr)
       print*,'addfield status=',ierr

!==> finalize  GRIB message after all grids
! and fields have been added.  It adds the End Section ( "7777" )

       call gribend(cgrib,max_bytes,lengrib,ierr)
       print*,'gribend status=',ierr
       print*,'length of the final GRIB2 message in octets =',lengrib
       call wryte(lunout, lengrib, cgrib)
      enddo 

      return
      end
!===========================================================================
       subroutine apply_template_300_ndfd(ifield3,len3) 

       implicit none

       integer(4),parameter::nx=288
       integer(4),parameter::ny=145
       integer(4),parameter::lat1=90000000   !lat of 1st grd pt in micro-deg
       integer(4),parameter::lon1=0  !east-long of 1st grd pt in micro-deg 
!      integer(4),parameter::lat1=20192000   !lat of 1st grd pt in micro-deg
!      integer(4),parameter::lon1=238446000  !east-long of 1st grd pt in micro-deg 
       integer(4),parameter::latan1=25000000 !true lat in micro-deg 
       integer(4),parameter::lonv=265000000  !y-axis || to long-circle at this long
       integer(4),parameter::ds1=5079406     !grid spacing in x and y
!      integer(4),parameter::ds1=5079000     !grid spacing in x and y

       integer(4) len3
       integer(4) ifield3(len3)
                                                                                                        
       ifield3(1) = 6 !Earth assumed spherical with radius of 6,371,229.0m
       ifield3(2) = 0
       ifield3(3) = 0
       ifield3(4) = 0
       ifield3(5) = 0
       ifield3(6) = 0
       ifield3(7) = 0
       ifield3(8) = nx
       ifield3(9) = ny
       ifield3(10) = 0
       ifield3(11) = 0
       ifield3(12) = lat1 
       ifield3(13) = lon1
       ifield3(14) = 48
       ifield3(15) = -90000000
       ifield3(16) = 358750000
       ifield3(17) = 1250000
       ifield3(18) = 1250000
       ifield3(19) = 0
 
       return
       end
!===========================================================================
       subroutine apply_template_40_ndfd(ifield4,len4,ifcsthr,ncat,nparm,nlevtype, &
        nlev,nspatial,typep) 

       implicit none

       integer(4) len4,ifcsthr
       integer ncat,nparm,nlevtype,nlev,nspatial
       integer(4) ifield4(len4)
   
       character*60 var
       character*60 typep
       
!==> ifield4(1):parameter category (see Code Table 4.1)
!==> ifield4(2):parameter number (see Code Table 4.2)
       ifield4(1) = ncat
       ifield4(2) = nparm

!==> ifield4(3):type of generating process (see Code Table 4.3)
!       if (trim(typep) .eq. 'analysis') then
!         ifield4(3) = 0
!        elseif (trim(typep) .eq. 'forecast') then
         ifield4(3) = 2
!        elseif (trim(typep) .eq. 'analysis error') then
!         ifield4(3) = 7
!        else
!         print*,'for RTMA,typep must be either analysis, forecast, &
!     &           or analysis error and your typep= ',trim(typep)
!         print*,'sorry, ... aborting in apply_template_40_ndfd' 
!         call abort
!       endif

!==> ifield4(4):background generating process identifier 
!                         (defined by originating Center)
       ifield4(4) = 0 !hasn't been defined yet 

!==> ifield4(5):analysis or forecast generating process identifier 
!                         (defined by originating Center)
       ifield4(5) = 96 

!==> ifield4(6):hours of observational data cutoff after reference time 
!==> ifield4(7):minutes of observational data cutoff after reference time 

       ifield4(6) = 0   !per steve
       ifield4(7) = 0   

!==> ifield4(8):indicator of unit of time range (see Code Table 4.4) 
       ifield4(8) = 1   

!==> ifield4(9):forecast time in units defined by ifield4(8) 
       ifield4(9) = ifcsthr
!       if (trim(typep) .eq. 'forecast') ifield4(9) = ifcsthr

!==> ifield4(10):type of first fixed surface (see Code Table 4.5)
!==> ifield4(11):scale factor of first fixed surface
!==> ifield4(12):scaled value of first fixed surface

       ifield4(11) = 0 !Because not saving any precision
       ifield4(10) = nlevtype
       ifield4(12) = nlev


!==> ifield4(13):type of second fixed surface(See Code Table 4.5)
!==> ifield4(14):scale factor of second fixed surface
!==> ifield4(15):scaled value of second fixed surface

       ifield4(13) = 255
       ifield4(14) = 0
       ifield4(15) = 0
       ifield4(16) = nspatial
       ifield4(17) = 3
       ifield4(18) = 1

       return
       end
!===========================================================================
       subroutine apply_template_50_ndfd(ifield5,len5,ncat,nparm,nlevtype) 

!********************************************************************************
! ifield5(1): reference value(R) (IEEE 32-bit floating-point value)             *
! ifield5(2): binary scale factor (E)                                           *
! ifield5(3): decimal scale factor (D)                                          *
! ifield5(4): number of bits used for each packed value for simple packing      *
!             or for each group reference value for complex packing or          *
!             spatial differencing                                              *
! ifield5(5): type of original field values (See Code Table 5.1)                *
!********************************************************************************
       implicit none

       integer(4) len5
       integer(4) ifield5(len5)
       integer ncat,nparm,nlevtype
!       character*60 var

   
       ifield5(1)=0 !Any value. Will be overwritten
       ifield5(2)=0 

       if (ncat==3 .and. nparm==3 .and. nlevtype==11) then ! Cb base
         ifield5(3) = 0
       else if (ncat==3 .and. nparm==3 .and. nlevtype==12) then ! Cb top
         ifield5(3) = 0	 
       else if (ncat==6 .and. nparm==25) then ! Cb ext
         ifield5(3) = 1
       else if (ncat==19 .and. nparm==22) then ! CAT
         ifield5(3) = 1
       else if (ncat==19 .and. nparm==21) then ! in cloud turb
         ifield5(3) = 3
       else if (ncat==19 .and. nparm==20) then ! icing
         ifield5(3) = 2	 	 	  	 
       else
	 ifield5(3) = 5
         print*,'define decimal scale factor to 6'
!         print*,'sorry, ... aborting in apply_template_50_ndfd'
!         call abort
       endif

       ifield5(4) = 0 !Must reset to 0
       ifield5(5) = 0
      
       return
       end
!===========================================================================
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!                .      .    .                                       . 
! SUBPROGRAM:    GETLUN      GET UNIQUE LOGICAL UNIT NUMBERS
!   PRGMMR: SMITH, TRACY     ORG: FSL/PROFS  DATE: 90-06-15 
! 
! ABSTRACT: THIS PROGRAM GETS UNIQUE LOGICAL UNIT NUMBERS FOR OPFILE
!   OR RETURNS THEM TO THE POOL FOR CLFILE.
! 
! PROGRAM HISTORY LOG: 
! FORTRAN 90 VERSION IS GETLUN90:  PONDECA,      DATE: 2006-03-08
! 
! USAGE:    CALL GETLUN(LUN,OPTN) 
!   INPUT ARGUMENT LIST: 
!     LUN      - INTEGER  LOGICAL UNIT NUMBER
!     OPTN     - INTEGER  CNCT=1, DSCT=2.
!                IF CONNECTING A FILE(CNCT) SET THE NUMBER TO
!                NEGATIVE SO IT WON'T BE USED UNTIL AFTER
!                DSCT SETS IT POSITIVE.
! 
!   OUTPUT ARGUMENT LIST:   
!     LUN      - INTEGER  LOGICAL UNIT NUMBER
! 
! REMARKS: 
! 
! ATTRIBUTES: 
!   LANGUAGE: FORTRAN-90
!   MACHINE:  NAS-9000 
!$$$ 

       SUBROUTINE GETLUN90(LUN,OPTN)
!* THIS PROGRAM GETS UNIQUE LOGICAL UNIT NUMBERS FOR OPFILE
!* OR RETURNS THEM TO THE POOL FOR CLFILE
       IMPLICIT NONE
       INTEGER LUN,NUM(80),OPTN,CNCT,DSCT,I
       PARAMETER (CNCT=1,DSCT=2)
       SAVE NUM
! 
       DATA NUM/99,98,97,96,95,94,93,92,91,90, &
                  89,88,87,86,85,84,83,82,81,80, &
                  79,78,77,76,75,74,73,72,71,70, &
                  69,68,67,66,65,64,63,62,61,60, &
                  59,58,57,56,55,54,53,52,51,50, &
                  49,48,47,46,45,44,43,42,41,40, &
                  39,38,37,36,35,34,33,32,31,30, &
                  29,28,27,26,25,24,23,22,21,20/
!* START
       IF(OPTN.EQ.CNCT) THEN
        DO 10 I=1,80
         IF(NUM(I).GT.0) THEN
           LUN=NUM(I)
           NUM(I)=-NUM(I)
           GOTO 20
         ENDIF
10      CONTINUE
         PRINT*, 'NEED MORE THAN 80 UNIT NUMBERS'
20      CONTINUE

       ELSE IF(OPTN.EQ.DSCT) THEN
!* MAKE THE NUMBER AVAILABLE BY SETTING POSITIVE
         DO 30 I=1,80
           IF(LUN.EQ.-NUM(I)) NUM(I)=ABS(NUM(I))
30       CONTINUE
       END IF
       RETURN
       END
!===========================================================================
