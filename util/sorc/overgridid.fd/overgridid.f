      program overgridid
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C
C MAIN PROGRAM: OVERGRIDID   REPLACE iGRID ID IN A GRIB FILE
C   PRGMMR: VUONG            ORG: NP23        DATE: 2014-05-21
C
C ABSTRACT: THIS PROGRAM READS AN ENTIRE GRIB FILE FROM UNIT 11
C   AND WRITES IT BACK OUT TO UNIT 51, REPLACING THE INTERNAL
C   GRID ID WITH THE GRID ID READ IN VIA UNIT 5.    
C
C PROGRAM HISTORY LOG:
C   1998-01-01  IREDELL
C   1998-06-17  FARLEY   MODIFIED OVERDATE ROUTINE
C   1999-05-24  Gilbert  - added calls to BAOPEN.
C   2014-05-21  Vuong    - Modified to change grid id in a grib file
C
C INPUT FILES:
C   UNIT    5    2-DIGIT MODEL ID (in base 10)
C   UNIT   11    INPUT GRIB FILE = "fort.11"
C
C OUTPUT FILES:
C   UNIT   51    OUTPUT GRIB FILE = "fort.51"
C
C SUBPROGRAMS CALLED:
C   SKGB     - Find next GRIB field 
C   BAREAD   - Read GRIB field
C   WRYTE    - Read GRIB field
C
C REMARKS:
C   ANY NON-GRIB INFORMATION IN THE INPUT GRIB FILE WILL BE LOST.
C   AN OUTPUT LINE WILL BE WRITTEN FOR EACH GRIB MESSAGE COPIED.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C
C$$$
      parameter(msk1=32000,msk2=4000,mgrib=10000000)
      character cgrib(mgrib)
C
      read *,id   ! grid id, ie 03 for 1.0 deg grib
      call baopenr(11,"fort.11",iret1)
      call baopenw(51,"fort.51",iret2)
C
      n=0
      iseek=0
      call skgb(11,iseek,msk1,lskip,lgrib)
      dowhile(lgrib.gt.0.and.lgrib.le.mgrib)
        call baread(11,lskip,lgrib,ngrib,cgrib)
        if(ngrib.ne.lgrib) call exit(2)
        n=n+1
        id0=mova2i(cgrib(8+7))
        cgrib(8+7)=char(id)
        call wryte(51,lgrib,cgrib)
        print '("msg",i6,4x,"len",i8,4x,"was",i4.2,4x,"now",i4.2)',
     &   n,lgrib,id0,id
        iseek=lskip+lgrib
        call skgb(11,iseek,msk2,lskip,lgrib)
      enddo
      end
