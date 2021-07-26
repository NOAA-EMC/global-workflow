       program reg2grb2

!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!
!                                                                              !
!  This program reads lat-lon ocean and ice data, calculates diagnostics,      !
!  and saves them in grib2 format.                                             !
!                                                                              !
!  Xingren Wu   (Xingren.Wu@noaa.gov)                                          !
!     April 20, 2007                                                           !
!                                                                              !
!  Xingren Wu - Update for MOM5/CICE with grib2 output                         !
!     February 16, 2017                                                        !
!                                                                              !
!  Christopher Melhauesr - Update for MOM6/CICE5 with grib2 output             !
!     September 8th, 2017                                                      !
!                                                                              !
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~!

       use regdiag_mod, only: diag
       use wgrib2api

       implicit none

       integer iyr,imth,iday,ihr,im,jm,km,imo,jmo
       integer isyr,ismth,isday,ishr
       integer mfh,mfhout,mfcstcpl,igenocnp
!       integer idbug,mkmoc,jmtpo,jmtp,imos,nreg

       real*4  dlat,dlon,flats,flatn,flonw,flone
!       real*4 tripolat
       CHARACTER*1  kmfcstcpl
!      CHARACTER*1 kdbug,kmkmoc
       CHARACTER*300 icefile,ocnfile,outfile
!       CHARACTER*120 mocfile,cicefile
       CHARACTER*10 syr,smth,sday,shr
       CHARACTER*10 kyr,kmth,kday,khr
       CHARACTER*10 kfh,kfhout
       CHARACTER*10 kim,kjm,kkm
!       CHARACTER*10 knreg,kimos,kjmtp,kjmtpo
       CHARACTER*10 kimo,kjmo
       CHARACTER*10 klats,klatn,klonw,klone,kigenocnp
!       CHARACTER*10 ktripolat

       call start()
!       call getenv("idbug",kdbug)
!       read(kdbug,'(i1)') idbug
!       write(*,*) "idbug= ",idbug
!!
!       call getenv("mkmoc",kmkmoc)
!       read(kmkmoc,'(i1)') mkmoc
!       write(*,*) "mkmoc= ",mkmoc
!
       call getenv("mfcstcpl",kmfcstcpl)
       read(kmfcstcpl,'(i1)') mfcstcpl
       write(*,*) "mfcstcpl= ",mfcstcpl
!
       call getenv("IGEN_OCNP",kigenocnp)
       read(kigenocnp,'(i3)') igenocnp
       write(*,*) "IGEN_OCNP= ",igenocnp
!
       call getenv("icefile",icefile)
       write(*,*) "icefile= ",icefile
!
       call getenv("ocnfile",ocnfile)
       write(*,*) "ocnfile= ",ocnfile
!!
!       call getenv("cicefile",cicefile)
!       write(*,*) "cicefile= ",cicefile
!!
       call getenv("outfile",outfile)
       write(*,*) "outfile= ",outfile
!!
!       call getenv("mocfile",mocfile)
!       write(*,*) "mocfile= ",mocfile
!!
       call getenv("syear",syr)
       read(syr,'(I4)') isyr
       write(*,*) "syear= ",isyr
!
       call getenv("smonth",smth)
       read(smth,'(I2)') ismth
       write(*,*) "smonth= ",ismth
!
       call getenv("sday",sday)
       read(sday,'(I2)') isday
       write(*,*) "sday= ",isday
!
       call getenv("shour",shr)
       read(shr,'(I2)') ishr
       write(*,*) "shour= ",ishr
!
       call getenv("year",kyr)
       read(kyr,'(I4)') iyr
       write(*,*) "year= ",iyr
!
       call getenv("month",kmth)
       read(kmth,'(I2)') imth
       write(*,*) "month= ",imth
!
       call getenv("day",kday)
       read(kday,'(I2)') iday
       write(*,*) "day= ",iday
!
       call getenv("hour",khr)
       read(khr,'(I2)') ihr
       write(*,*) "hour= ",ihr
!
       call getenv("fh",kfh)
       read(kfh,'(I10)') mfh
       write(*,*) "fh= ",mfh
!
       call getenv("hh_inc_ocn",kfhout)
       read(kfhout,'(I10)') mfhout
       write(*,*) "hh_inc_ocn= ",mfhout
!
       call getenv("im",kim)
       read(kim,'(I10)') im
       write(*,*) "im= ",im
!
       call getenv("jm",kjm)
       read(kjm,'(I10)') jm
       write(*,*) "jm= ",jm
!
       call getenv("imo",kimo)
       read(kimo,'(I10)') imo
       write(*,*) "imo= ",imo
!
       call getenv("jmo",kjmo)
       read(kjmo,'(I10)') jmo
       write(*,*) "jmo= ",jmo
!!
!       call getenv("jmtp",kjmtp)
!       read(kjmtp,'(I10)') jmtp
!       write(*,*) "jmtp= ",jmtp
!!
!       call getenv("jmtpo",kjmtpo)
!       read(kjmtpo,'(I10)') jmtpo
!       write(*,*) "jmtpo= ",jmtpo
!!
!       call getenv("imos",kimos)
!       read(kimos,'(I10)') imos
!       write(*,*) "imos= ",imos
!!
       call getenv("km",kkm)
       read(kkm,'(I10)') km
       write(*,*) "km= ",km
!
       call getenv("flats",klats)
       read(klats,'(f8.1)') flats
       write(*,*) "flats= ",flats
!
       call getenv("flatn",klatn)
       read(klatn,'(f8.1)') flatn
       write(*,*) "flatn= ",flatn
!
       call getenv("flonw",klonw)
       read(klonw,'(f8.1)') flonw
       write(*,*) "flonw= ",flonw
!
       call getenv("flone",klone)
       read(klone,'(f8.1)') flone
       write(*,*) "flone= ",flone
!
!       call getenv("nreg",knreg)
!       read(knreg,'(i10)') nreg
!       write(*,*) "nreg= ",nreg
!!
!       call getenv("tripolat",ktripolat)
!       read(ktripolat,'(f8.1)') tripolat
!       write(*,*) "tripolat= ",tripolat
!!
       dlat = (flatn-flats) / (jmo-1)
!
       dlon = (flone-flonw) / (imo-1)
!
       call diag(im,jm,km,icefile,ocnfile,                  &
       isyr,ismth,isday,ishr,iyr,imth,iday,ihr,mfh,mfhout,  &
       flonw,flone,dlon,flatn,flats,dlat,imo,jmo,           &
       outfile,mfcstcpl,igenocnp)
!
       call summary()
       call errexit(0)
       stop
       end

