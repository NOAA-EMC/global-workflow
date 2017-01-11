program  setmissing
!$$$  MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM:  copied from blending.f
!   2016-09-13
!
! ABSTRACT: This program is designged for WAFS products only,
!           including CB CTP CAT ICING.
!           it reads GRIB2 file and reset missing round-off
!           values to the exact missing values, following
!           the round-off error after grid conversion by
!           wgrib2 using neighbor interpolation.
!
! PROGRAM HISTORY LOG:
! 2016-09-13  Mao
!
! USAGE:
!   INPUT FILES:
!     UNIT 10  - Input GRIB file
!
!   OUTPUT FILES:
!     UNIT 50  - Output GRIB file
!
! USAGE:
! COMMAND LINE:
!     setmissing  inputfile outputfile
!   OUTPUT FILES:  (INCLUDING SCRATCH FILES)
!     6        - STANDARD FORTRAN PRINT FILE
!
!   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
!     LIBRARY:
!       G2LIB    - GB_INFO, GT_GETFLD
!       W3LIB    - GBYTE, SKGB
!       BACIO    - BAOPENR, BAREAD, BACLOSE
!       SYSTEM   - IARGC   FUNCTION RETURNS NUMBER OF ARGUMENT ON
!                          COMMAND LINE
!                - GETARG  ROUTINE RETURNS COMMAND LINE ARGUMENT
!
!   EXIT STATES:
!     COND =   0 - SUCCESSFUL RUN
!
! REMARKS: COMMAND LINE CAN HAVE ONE FILE NAME.
!     
! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90
!   MACHINE:  IBM 
!

  use grib_mod
  use params

  implicit none

  integer, parameter :: msk1=32000,msk2=4000
  integer, parameter :: maxfield=40
  CHARACTER(len=1),allocatable,dimension(:) :: cgrib
  integer :: listsec0(3),listsec1(13)
  character(len=10) :: typeproc
  character(len=8) :: pabbrev
  integer :: currlen=0
  integer(4),dimension(maxfield) :: ncat,nparm,nlevtype,nlev &
        ,ifcsthr,npdt,nspatial
  logical :: unpack,expand
  type(gribfield) :: gfld
  real, allocatable :: inputdata(:)
  integer :: itot, icount, iseek,lskip,lgrib,lengrib,j
  integer :: numfields,numlocal,maxlocal
  real :: avgdata(41760,maxfield)

  real, parameter :: EPSILON=0.000001
  character(100) :: inputfile,outputfile,avar
  real :: missing, nocloud
  INTEGER :: NARG,IARGC,n,i
  integer :: IFL1,IFL2,iret,nxy

  call start()
  unpack=.true.
  expand=.true.
      
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!  GET ARGUMENTS
  NARG=IARGC()
  IF(NARG /= 2) THEN
     CALL ERRMSG('setmissing:  Incorrect usage')
     CALL ERRMSG('Usage: setmissing inputfile outputfile')
     CALL ERREXIT(2)
  ENDIF

  IFL1=10
  IFL2=20

  !
  CALL GETARG(1,inputfile)
  CALL BAOPENR(ifl1,trim(inputfile),iret)
  if(iret/=0)print*,'cant open ',trim(inputfile)
  !
  CALL GETARG(2,outputfile)
  call baopenw(IFL2, outputfile, iret)
  if(iret/=0)print*,'cant open ',trim(outputfile)

  itot=0
  icount=0
  iseek=0
  do
     call skgb(ifl1,iseek,msk1,lskip,lgrib)
     if (lgrib==0) exit    ! end loop at EOF or problem
     if (lgrib>currlen) then
        if (allocated(cgrib)) deallocate(cgrib)
        allocate(cgrib(lgrib),stat=iret)
        currlen=lgrib
     endif
     call baread(ifl1,lskip,lgrib,lengrib,cgrib)
     if (lgrib/=lengrib) then
        print *,' setmissing: IO Error.'
        call errexit(9)
     endif
     iseek=lskip+lgrib
     icount=icount+1
     PRINT *
     PRINT *,'GRIB MESSAGE ',icount,' starts at',lskip+1
     PRINT *

! Unpack GRIB2 field
     call gb_info(cgrib,lengrib,listsec0,listsec1,&
                  numfields,numlocal,maxlocal,iret)
     if (iret/=0) then
        write(6,*) ' ERROR querying GRIB2 message = ',iret
        stop 10
     endif
     itot=itot+numfields

     do n=1,numfields
        call gf_getfld(cgrib,lengrib,n,unpack,expand,gfld,iret)
        if (iret/=0) then
           write(6,*) ' ERROR extracting field = ',iret
           cycle
        endif

     enddo
	 
     nxy = gfld%igdtmpl(8)*gfld%igdtmpl(9)
     print*,'Input data dimensions= ',nxy, "decoded #=", gfld%ndpts

     if(iret==0)then
        if(.not. allocated(inputdata))  allocate(inputdata(nxy))
        inputdata = gfld%fld
! specify missing data values
! For CTP, -0.002 for nocloud, -.004 for missing value
        if(gfld%ipdtmpl(1)==19 .and. gfld%ipdtmpl(2)==21)then  ! CTP
           missing=-0.004
           nocloud=-0.002
        else if(gfld%ipdtmpl(1)==19 .and. gfld%ipdtmpl(2)==22)then  ! CAT
           missing=-0.5 
        else if(gfld%ipdtmpl(1)==19 .and. gfld%ipdtmpl(2)==20)then  ! ICING   
           missing=-0.01
        else if(gfld%ipdtmpl(2)==3 .and. gfld%ipdtmpl(10)==11)then  ! Cb bot
           missing=-1.0
        else if(gfld%ipdtmpl(2)==3 .and. gfld%ipdtmpl(10)==12)then  ! Cb top
           missing=-1.0  
        else if(gfld%ipdtmpl(2)==25 .and. gfld%ipdtmpl(10)==10)then  ! Cb ext
           missing=-0.1 
        end if
	   
        do i=1,nxy
           if(abs(inputdata(i)-missing)<=EPSILON) then
              inputdata(i) = missing
           elseif(gfld%ipdtmpl(1)==19 .and. gfld%ipdtmpl(2)==2 .and. &
                  abs(inputdata(i)-nocloud)<=EPSILON) then
              inputdata(i) = -nocloud
           end if
           avgdata(i,icount)=inputdata(i)
        end do
     end if

     gfld%fld = inputdata

     call putgb2(IFL2,gfld,iret)
     call gf_free(gfld)

  enddo

  if( allocated(inputdata)) deallocate(inputdata)
  call BACLOSE(IFL1,iret)
  call BACLOSE(IFL2,iret)

  print *," "
  print *, ' Total Number of Fields Found = ',itot
  print*,'sending the data for output'
  stop
end program setmissing
