      subroutine extflx(lugb,fngrib,sfc,kdim,kpoint,
     &           ib,ibp,jb,jbp,di,dj,idim,jdim,colat,imask)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    PROGRAM NAME (up to 20 characters)
C   PRGMMR: YOUR NAME        ORG: W/NMCXX    DATE: YY-MM-DD
C
C ABSTRACT: START ABSTRACT HERE AND INDENT TO COLUMN 5 ON THE
C   FOLLOWING LINES.  PLEASE PROVIDE A BRIEF DESCRIPTION OF
C   WHAT THE SUBPROGRAM DOES.
C
C PROGRAM HISTORY LOG:
C   YY-MM-DD  ORIGINAL PROGRAMMER'S NAME HERE
C   YY-MM-DD  MODIFIER1   DESCRIPTION OF CHANGE
C   YY-MM-DD  MODIFIER2   DESCRIPTION OF CHANGE
C
C USAGE:    CALL PROGRAM-NAME(INARG1, INARG2, WRKARG, OUTARG1, ... )
C   INPUT ARGUMENT LIST:
C     INARG1   - GENERIC DESCRIPTION, INCLUDING CONTENT, UNITS,
C     INARG2   - TYPE.  EXPLAIN FUNCTION IF CONTROL VARIABLE.
C
C   OUTPUT ARGUMENT LIST:      (INCLUDING WORK ARRAYS)
C     WRKARG   - GENERIC DESCRIPTION, ETC., AS ABOVE.
C     OUTARG1  - EXPLAIN COMPLETELY IF ERROR RETURN
C     ERRFLAG  - EVEN IF MANY LINES ARE NEEDED
C
C   INPUT FILES:   (DELETE IF NO INPUT FILES IN SUBPROGRAM)
C
C   OUTPUT FILES:  (DELETE IF NO OUTPUT FILES IN SUBPROGRAM)
C
C REMARKS: LIST CAVEATS, OTHER HELPFUL HINTS OR INFORMATION
C
C ATTRIBUTES:
C   LANGUAGE: INDICATE EXTENSIONS, COMPILER OPTIONS
C   MACHINE:  IBM SP
C
C$$$
      PARAMETER(IPRS=1,ITEMP=11,IZNLW=33,IMERW=34,ISPHUM=51,IPWAT=54,
     &          ITMAX=15,ITMIN=16,
     $          IPCPR=59,ISNOWD=65,ICLDF=71,ICCLDF=72,
     $          ISLMSK=81,IZORL=83,IALBDO=84,ISOILM=144,ICEMSK=91,
     $          ILHFLX=121,ISHFLX=122,IZWS=124,IMWS=125,IGHFLX=155,
     $          IUSWFC=160,IDSWFC=161,IULWFC=162,IDLWFC=163,
     $          INSWFC=164,INLWFC=165,
     $          IDSWVB=166,IDSWVD=167,IDSWNB=168,IDSWND=169,
     $          ITMX=15,ITMN=16,IRNOF=90,IEP=145,
     &          ICLDWK=146,IZGW=147,IMGW=148,IHPBL=221,
     $          IDSWF=204,IDLWF=205,IUSWF=211,IULWF=212,ICPCPR=214)
      PARAMETER(ISFC=1,ITOA=8,IELEV=105,
     $          ISGLEV=107,IDBLS=111,I2DBLS=112,ICOLMN=200,
cbl
     $          IBLBL=209,IBLTL=210,IBLLYR=211,
cbl
     $          ILCBL=212,ILCTL=213,ILCLYR=214,
     $          IMCBL=222,IMCTL=223,IMCLYR=224,
     $          IHCBL=232,IHCTL=233,IHCLYR=234,
     $          ICVBL=242,ICVTL=243,ICVLYR=244)
      real sfc(kdim,kpoint), di(kpoint), dj(kpoint)
      real colat(jdim)
      integer ib(kpoint), ibp(kpoint), jb(kpoint), jbp(kpoint)
      integer imask(kpoint)
      integer jpds(200), jgds(200)
      integer kpds(200), kgds(200)
      character*80 fngrib 
      logical lbms(idim,jdim)
      real data(idim,jdim), tsfc(idim,jdim)
      real lsmask(idim,jdim)
      integer mdata,lskip,ndata,iret
c     open(unit=lugb,file=fngrib,form='unformatted',err=910)
      call baopenr(lugb,fngrib,iret)
      if(iret.ne.0) then
        PRINT *,'ERROR IN OPENING FILE ',FNGRIB(1:50)
        print *, ' iret =', iret
        CALL ABORT
      endif
c      print *, ' file ', fngrib(1:50),' opened. unit=',lugb
      lugi = 0
      sfc = 9.9e20
      mdata = idim * jdim
c
c  read the flux files
c
      lskip = -1
      jpds = -1
      jgds = -1
c  first read in lsmask
      jpds(5) = islmsk
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,lsmask,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,3,kdim,kpoint,ib,ibp,jb,jbp,di,dj,lsmask,
     &     idim,jdim,imask,lsmask)
c
c  read the flux files in sequence
c
      lskip = -1
      jpds = -1
      jgds = -1
c  ustr
      jpds(5) = izws
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        print *, ' iret, ndata =', iret, ndata
        call abort
      endif
c      print *, ' iyr, imo, idy, ihr =', kpds(8),kpds(9),
c     &         kpds(10), kpds(11)
      call fillken(sfc,20,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  vstr
      jpds(5) = imws
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,21,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  shf
      jpds(5) = ishflx
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,18,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  lhf
      jpds(5) = ilhflx
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,17,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  tsfc
      jpds(5) = itemp
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      tsfc = data
      call fillken(sfc,5,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  soil moisture layer 1
      jpds(5) = isoilm
      jpds(6) = i2dbls
      jpds(7) = 10
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
c  fill with saturation value over ocean
      do j = 1, jdim
        do i = 1, idim
          if(lsmask(i,j).eq.0.) data(i,j) = .47
        enddo
      enddo
      call fillken(sfc,9,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  soil moisture layer 2
      jpds(7) = 2600
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
c  fill with saturation value over ocean
      do j = 1, jdim
        do i = 1, idim
          if(lsmask(i,j).eq.0.) data(i,j) = .47
        enddo
      enddo
      call fillken(sfc,61,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  soil temperature layer 1
      jpds(5) = itemp
      jpds(6) = i2dbls
      jpds(7) = 10
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
c  fill with surface temperature over ocean
      do j = 1, jdim
        do i = 1, idim
          if(lsmask(i,j).eq.0.) data(i,j) = tsfc(i,j)
        enddo
      enddo
      call fillken(sfc,6,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  soil temperature layer 2
      jpds(7) = 2600
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
c  fill with surface temperature over ocean
      do j = 1, jdim
        do i = 1, idim
          if(lsmask(i,j).eq.0.) data(i,j) = tsfc(i,j)
        enddo
      enddo
      call fillken(sfc,7,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  snow depth
      jpds(5) = isnowd
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,10,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  dsws
      jpds(5) = idswf
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,14,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  cldh
      jpds(5) = icldf
      jpds(6) = ihclyr
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,25,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  cldm
      jpds(5) = icldf
      jpds(6) = imclyr
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,26,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  cldl
      jpds(5) = icldf
      jpds(6) = ilclyr
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,27,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  precip
      jpds(5) = ipcpr
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,12,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  conv precip
      jpds(5) = icpcpr
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,11,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  gflx
      jpds(5) = ighflx
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,19,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  u10
      jpds(5) = iznlw
      jpds(6) = ielev
      jpds(7) = 10
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,34,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  v10
      jpds(5) = imerw
      jpds(6) = ielev
      jpds(7) = 10
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,35,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  t2
      jpds(5) = itemp
      jpds(6) = ielev
      jpds(7) = 2
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,30,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  q2
      jpds(5) = isphum
      jpds(6) = ielev
      jpds(7) = 2
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,31,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  psfc
      jpds(5) = iprs
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,4,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  tmax
      jpds(5) = itmax
      jpds(6) = ielev
      jpds(7) = 2
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,64,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  tmin
      jpds(5) = itmin
      jpds(6) = ielev
      jpds(7) = 2
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,65,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  ugrw 
      jpds(5) = izgw
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,28,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  vgrw 
      jpds(5) = imgw
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,29,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
c  hpbl 
      jpds(5) = ihpbl
      jpds(6) = isfc
      jpds(7) = 0
      call getgb(lugb,lugi,mdata,lskip,jpds,jgds,ndata,lskip,
     &           kpds,kgds,lbms,data,iret)
      if(iret.ne.0.or.ndata.eq.0) then
        print *, ' error in getgb'
        call abort
      endif
      call fillken(sfc,63,kdim,kpoint,ib,ibp,jb,jbp,di,dj,data,
     &     idim,jdim,imask,lsmask)
      return
 910  print *, ' error in opening file ',fngrib(1:50)
      call abort
      end
