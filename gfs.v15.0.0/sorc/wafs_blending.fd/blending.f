      program blending
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM:  cnvgrib
C   PRGMMR: Gilbert        ORG: NP11        DATE: 2003-06-06
C
C ABSTRACT: This program reads GRIB2 file and makes inventory
C           of GRIB file 
C
C PROGRAM HISTORY LOG:
C 2003-06-06  Gilbert
C 2010-01-26  Vuong    Modified the options and help messages
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
      parameter(maxfield=40)
      CHARACTER(len=1),allocatable,dimension(:) :: cgrib
      integer :: listsec0(3)
      integer :: listsec1(13)
!      integer :: igds(5),igdstmpl(200),ipdstmpl(200),idrstmpl(200)
!      integer :: ideflist(500)
      character(len=60) :: gfile1,gfile2,gfile3,typeproc
      character(len=8) :: pabbrev
      character(len=20) :: labbrev
      character(len=80) :: tabbrev
      INTEGER(4) NARG,IARGC,temparg
      integer :: currlen=0
      integer :: jids(20),jpdt(20),jgdt(20)
      integer(4),dimension(maxfield) :: ncat,nparm,nlevtype,nlev
     &      ,ifcsthr,npdt,nspatial
      logical :: unpack,expand
      type(gribfield) :: gfld,gfld2
      real, allocatable :: usdata(:,:),ukdata(:,:)
     +      ,avgdata2d(:,:)
      real avgdata(41760,maxfield)
      call start()
      unpack=.true.
      expand=.true.
      
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
C  GET ARGUMENTS
      NARG=IARGC()
      IF(NARG.NE.3) THEN
        CALL ERRMSG('blending:  Incorrect usage')
        CALL ERRMSG('Usage: degrib2 grib2file1 grib2file2 grib2file3')
        CALL ERREXIT(2)
      ENDIF

      IFL1=10
      IFL2=20
      IFL3=30
      temparg=1
      CALL GETARG(temparg,gfile1)
      NCGB=LEN_TRIM(gfile1)
      CALL BAOPENR(ifl1,gfile1(1:NCGB),IOS)
      if(ios/=0)print*,'cant open ',gfile1(1:NCGB)
      temparg=2
      CALL GETARG(temparg,gfile2)
      NCGB=LEN_TRIM(gfile2)
      CALL BAOPENR(ifl2,gfile2(1:NCGB),IOS)
      if(ios/=0)print*,'cant open ',gfile2(1:NCGB)
      
      temparg=3
      CALL GETARG(temparg,gfile3)
      NCGB=LEN_TRIM(gfile3)
!      CALL BAOPENwt(ifl3,gfile3(1:NCGB),IOS)
!      if(ios/=0)print*,'cant open ',gfile3(1:NCGB)

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
            print *,' degrib2: IO Error.'
            call errexit(9)
         endif
         iseek=lskip+lgrib
         icount=icount+1
         PRINT *
         PRINT *,'GRIB MESSAGE ',icount,' starts at',lskip+1
         PRINT *

! Unpack GRIB2 field
         call gb_info(cgrib,lengrib,listsec0,listsec1,
     &                numfields,numlocal,maxlocal,ierr)
         if (ierr.ne.0) then
           write(6,*) ' ERROR querying GRIB2 message = ',ierr
           stop 10
         endif
         itot=itot+numfields
         print *,' SECTION 0: ',(listsec0(j),j=1,3)
         print *,' SECTION 1: ',(listsec1(j),j=1,13)
         print *,' Contains ',numlocal,' Local Sections ',
     &           ' and ',numfields,' data fields.'

         do n=1,numfields
           call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,ierr)
           if (ierr.ne.0) then
             write(6,*) ' ERROR extracting field = ',ierr
             cycle
           endif

           print *
           print *,' FIELD ',n
           if (n==1) then
            print *,' SECTION 0: ',gfld%discipline,gfld%version
            print *,' SECTION 1: ',(gfld%idsect(j),j=1,gfld%idsectlen)
           endif
           if ( associated(gfld%local).AND.gfld%locallen.gt.0 ) then
              print *,' SECTION 2: ',gfld%locallen,' bytes'
           endif
           print *,' SECTION 3: ',gfld%griddef,gfld%ngrdpts,
     &                            gfld%numoct_opt,gfld%interp_opt,
     &                            gfld%igdtnum
           print *,' GRID TEMPLATE 3.',gfld%igdtnum,': ',
     &            (gfld%igdtmpl(j),j=1,gfld%igdtlen)
           if ( gfld%num_opt .eq. 0 ) then
             print *,' NO Optional List Defining Number of Data Points.'
           else
             print *,' Section 3 Optional List: ',
     &                (gfld%list_opt(j),j=1,gfld%num_opt)
           endif
           print *,' PRODUCT TEMPLATE 4.',gfld%ipdtnum,': ',
     &          (gfld%ipdtmpl(j),j=1,gfld%ipdtlen)

           pabbrev=param_get_abbrev(gfld%discipline,gfld%ipdtmpl(1),
     &                              gfld%ipdtmpl(2))
           call prlevel(gfld%ipdtnum,gfld%ipdtmpl,labbrev)
           call prvtime(gfld%ipdtnum,gfld%ipdtmpl,listsec1,tabbrev)
           print *,' TEXT: ',pabbrev,trim(labbrev)," ",trim(tabbrev)

           if ( gfld%num_coord .eq. 0 ) then
             print *,' NO Optional Vertical Coordinate List.'
           else
             print *,' Section 4 Optional Coordinates: ',
     &             (gfld%coord_list(j),j=1,gfld%num_coord)
           endif
           if ( gfld%ibmap .ne. 255 ) then
              print *,' Num. of Data Points = ',gfld%ndpts,
     &             '    with BIT-MAP ',gfld%ibmap
           else
              print *,' Num. of Data Points = ',gfld%ndpts,
     &                '    NO BIT-MAP '
           endif
           print *,' DRS TEMPLATE 5.',gfld%idrtnum,': ',
     &          (gfld%idrtmpl(j),j=1,gfld%idrtlen)
           fldmax=gfld%fld(1)
           fldmin=gfld%fld(1)
           sum=gfld%fld(1)
           do j=2,gfld%ndpts
             if (gfld%fld(j).gt.fldmax) fldmax=gfld%fld(j)
             if (gfld%fld(j).lt.fldmin) fldmin=gfld%fld(j)
             sum=sum+gfld%fld(j)
           enddo
           print *,' Data Values:'
           write(6,fmt='("  MIN=",f21.8,"  AVE=",f21.8,
     &          "  MAX=",f21.8)') fldmin,sum/gfld%ndpts,fldmax
          !do j=1,gfld%ndpts
          !   write(22,*) gfld%fld(j)
          !enddo

!          call gf_free(gfld)
         enddo
	 
! read UK's matching products
         print*,'reading UK WAFS'
         jids= -9999
!	 jids(5)=listsec1(5)
	 jids(6)=listsec1(6) ! year
	 jids(7)=listsec1(7) ! mon
	 jids(8)=listsec1(8) ! day
	 jids(9)=listsec1(9) ! hr
	 print*,'disipline,jpd= ',listsec0(1),jids(6:9)
	 jpdtn=gfld%ipdtnum
	 jpdt=-9999
	 jpdt(1)=gfld%ipdtmpl(1) ! cat number
	 jpdt(2)=gfld%ipdtmpl(2) ! parm number
	 jpdt(3)=gfld%ipdtmpl(3)
	 jpdt(9)=gfld%ipdtmpl(9)  ! forecast hour
	 jpdt(10)=gfld%ipdtmpl(10) ! level ID
	 jpdt(12)=gfld%ipdtmpl(12)  ! level value
	 jpdt(16)=gfld%ipdtmpl(16) ! spatial statistical processing
	 print*,'jpdtn,jpdt= ',jpdtn,jpdt(1:10)
	 jgdtn=gfld%igdtnum
	 JGDT=-9999
	 jgdt(1)=gfld%igdtmpl(1)
	 jgdt(8)=gfld%igdtmpl(8)
	 jgdt(9)=gfld%igdtmpl(9)
	 print*,'jgdtn,jgdt= ',jgdtn,jgdt(1:9)
         call getgb2(IFL2,0,0,listsec0(1),jids,jpdtn,jpdt,
     *              gfld%igdtnum,JGDT,.TRUE.,
     *              K,gfld2,iret)
         print*,'US and UK dimensions= ',gfld%ndpts,gfld2%ndpts
         if(iret==0)then
	   im=gfld%igdtmpl(8)
	   jm=gfld%igdtmpl(9)
	   allocate(usdata(im,jm))
	   allocate(ukdata(im,jm))
	   allocate(avgdata2d(im,jm))
	   do j=1,jm
	    do i=1,im
	     usdata(i,j)=gfld%fld((j-1)*im+i)
	     ukdata(i,j)=gfld2%fld((j-1)*im+i)
	     if(j==97 .and. i>49 .and. i<97)print*,'sample 2D data ',
     +       i,j,usdata(i,j),ukdata(i,j)
	    end do
	   end do  
! specify UK missing data values for different fields
           if(gfld%ipdtmpl(1)==19 .and. gfld%ipdtmpl(2)==21)then  ! CTP
	    ukmissing=-0.004
	   else if(gfld%ipdtmpl(1)==19 .and. gfld%ipdtmpl(2)==22)then  ! CAT
	    ukmissing=-0.5 
	   else if(gfld%ipdtmpl(1)==19 .and. gfld%ipdtmpl(2)==20)then  ! ICING   
	    ukmissing=-0.01
	   else if(gfld%ipdtmpl(2)==3 .and. gfld%ipdtmpl(10)==11)then  ! Cb bot
	    ukmissing=-1.0
	   else if(gfld%ipdtmpl(2)==3 .and. gfld%ipdtmpl(10)==12)then  ! Cb top
	    ukmissing=-1.0  
	   else if(gfld%ipdtmpl(2)==25 .and. gfld%ipdtmpl(10)==10)then  ! Cb ext
	    ukmissing=-0.1 
	   end if 
	   
	   do j=1,jm
	    do i=1,im
	     jj=jm-j+1
	     if(gfld%ipdtlen>=16)then ! non Cb fields
	      if(usdata(i,j)<=ukmissing .and. ukdata(i,jj)>ukmissing)then !missing US data
	        avgdata2d(i,j)=ukdata(i,jj)
	      else if(ukdata(i,jj)<=ukmissing .and. usdata(i,j)>ukmissing)
     &  	 then !missing UK data
	        avgdata2d(i,j)=usdata(i,j)
	      else if(usdata(i,j)<=ukmissing.and.ukdata(i,jj)<=ukmissing)
     &  	 then !missing both data
	        avgdata2d(i,j)=ukmissing
!		avgdata2d(i,j)=0.
	      else ! both indicates values	       
	        if(jpdt(16)==0)then ! perform averaging for mean products
	         avgdata2d(i,j)=(usdata(i,j)+ukdata(i,jj))*0.5
                 if(avgdata2d(i,j)<0.)print*,'negative blended data ',i,j,
     +           usdata(i,j),ukdata(i,jj),avgdata2d(i,j)
	        else if(jpdt(16)==2)then ! take max for max products
	         avgdata2d(i,j)=max(usdata(i,j),ukdata(i,jj))
	        end if
	      end if	
	     else ! Cb field
	      if(gfld%ipdtmpl(2)==3 .and. gfld%ipdtmpl(10)==11)then  ! Cb bot
	       if(usdata(i,j)<=0. .and. ukdata(i,jj)>-1.0)then !missing US data
	        avgdata2d(i,j)=ukdata(i,jj)
	       else if(usdata(i,j)>0. .and. ukdata(i,jj)<=-1.0)then !missing UK data
	        avgdata2d(i,j)=usdata(i,j)
	       else if(usdata(i,j)<=0. .and. ukdata(i,jj)<=-1.0)then !missing both data
	        avgdata2d(i,j)=-1.
	       else ! both indicates Cb
	        avgdata2d(i,j)=min(usdata(i,j),ukdata(i,jj))
	       end if
	      else if(gfld%ipdtmpl(2)==3 .and. gfld%ipdtmpl(10)==12)then  ! Cb top
	       if(usdata(i,j)<=0. .and. ukdata(i,jj)>-1.0)then !missing US data
	        avgdata2d(i,j)=ukdata(i,jj)
	       else if(usdata(i,j)>0. .and. ukdata(i,jj)<=-1.0)then !missing UK data
	        avgdata2d(i,j)=usdata(i,j)
	       else if(usdata(i,j)<=0. .and. ukdata(i,jj)<=-1.0)then !missing both data
	        avgdata2d(i,j)=-1.
	       else ! both indicates Cb
	        avgdata2d(i,j)=max(usdata(i,j),ukdata(i,jj))
	       end if 
	      else if(gfld%ipdtmpl(2)==25 .and. gfld%ipdtmpl(10)==10)then  ! Cb ext
	       if(usdata(i,j)<=0. .and. ukdata(i,jj)>-0.1)then !missing US data
	        avgdata2d(i,j)=ukdata(i,jj)
	       else if(usdata(i,j)>0. .and. ukdata(i,jj)<=-0.1)then !missing UK data
	        avgdata2d(i,j)=usdata(i,j)
	       else if(usdata(i,j)<=0. .and. ukdata(i,jj)<=-0.1)then !missing both data
	        avgdata2d(i,j)=-0.1
	       else ! both indicates Cb
	        avgdata2d(i,j)=(usdata(i,j)+ukdata(i,jj))*0.5
	       end if 	
	      end if ! end of 3 different Cb blending 		
	     end if  ! end of all blending   
	    end do
	   end do  
!	   allocate(avgdata(gfld%ndpts))
	   do j=1,jm
	    do i=1,im
	     jj=(j-1)*im+i
	     if(j==1 .or. j==jm)avgdata2d(i,j)=ukmissing
	     avgdata(jj,icount)=avgdata2d(i,j)
	    end do
	   end do  

!!           do j=1,gfld%ndpts
!!	     jdum=int(j/im)
!!	     jres=mod(j,im)
!!	     jj=im*jm-(idum+1)*im+ires
!!            avgdata(j)=(gfld%fld(j)+gfld2%fld(j))/2.0
!!	     if(mod(j,1000)==0)print*,'check averaging ',j,gfld%fld(j),
!!     +	       gfld2%fld(jj),avgdata(j)
!!           enddo
!	   deallocate(avgdata)
	   call gf_free(gfld2)
	   deallocate(avgdata2d)
	   deallocate(usdata)
	   deallocate(ukdata)
!           print*,'writing to new grib2 file'	   
!	   call putgb2(ifl3,gfld,iret)
!           kpds(1)=7
!	   kpds(2)=96	   
!           call putgb(51,gfld%ndpts,KPDS,KGDS,LBMS,SO(1:idim,1:jdim,nl)
!     &  ,IRET)
!	   if(iret/=0)print*,'problem writing a grib2 file ',iret
!	   call gf_free(gfld)
	 else
	   print*, pabbrev,trim(labbrev),
     +	   ' not found, writting US data as blended' 
!Chuang: use US data when UK missing
           !icount=icount-1
           !itot=itot-1
           do j=1,jm
            do i=1,im
             jj=(j-1)*im+i
             avgdata(jj,icount)=gfld%fld((j-1)*im+i)
            end do
           end do
	   !go to 15
	       
!	   do j=1,jm
!	    do i=1,im
!	     jj=(j-1)*im+i
!	     avgdata(jj,icount)=-10000.
!	    end do
!	   end do
	   print*,'error code= ',iret   
         end if
	 npdt(icount)=gfld%ipdtnum
	 ncat(icount)=gfld%ipdtmpl(1)
	 nparm(icount)=gfld%ipdtmpl(2)
	 nlevtype(icount)=gfld%ipdtmpl(10)
	 nlev(icount)=gfld%ipdtmpl(12)
	 nspatial(icount)=gfld%ipdtmpl(16)
	 ifcsthr(icount)=gfld%ipdtmpl(9) 
	 
! write averged data to new grib file
!         call putgb2(IFL3,gfld,iret)	 
 15      continue	 
	 call gf_free(gfld)
!	 call gf_free(gfld2)

      enddo
      print *," "
      print *, ' Total Number of Fields Found = ',itot
      print*,'sending the following to write_ndfd_grib2'
      typeproc(1:9)='forecast'
      do n=1,itot
       print*,gfld%ndpts,itot,
     &            npdt(n),ncat(n),nparm(n),
     &            ifcsthr(n),nlevtype(n),nlev(n),
     &            nspatial(n),
     &            typeproc,1, listsec1(6),listsec1(7),listsec1(8),
     &             listsec1(9),
     &            0,0,1
      end do
! writing grib2 file
!      call write_ndfd_grib2(avgdata(1:41760,1:itot),gfld%ndpts,itot,
!     &            gfld%ipdtnum,gfld%ipdtmpl(1),gfld%ipdtmpl(2),
!     &            gfld%ipdtmpl(9),gfld%ipdtmpl(10),gfld%ipdtmpl(12),
!     &            gfld%ipdtmpl(16), 
!     &            typeproc,1, listsec1(6),listsec1(7),listsec1(8),
!     &             listsec1(9),
!     &            0,0,1,gfile3) 
      call write_ndfd_grib2(avgdata(1:41760,1:itot),gfld%ndpts,itot,
     &            npdt(1:itot),ncat(1:itot),nparm(1:itot),
     &            ifcsthr(1:itot),nlevtype(1:itot),nlev(1:itot),
     &            nspatial(1:itot), 
     &            typeproc,1, listsec1(6),listsec1(7),listsec1(8),
     &             listsec1(9),
     &            0,0,1,gfile3)       
!      call gf_free(gfld)   
      stop
      end
