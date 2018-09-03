      subroutine get3(lun,luni,jpds,array,c132)
      common/grbpds/kpds
      COMMON/PACKRA/IRAS(10)
c  array is an NMC GRIB TYPE 3 field on output
c   jpds is set in the CALLER!! except for fields 1-3
      dimension array(360,181)
      dimension jpds(25),jgds(25)
      dimension grib(360,181)
      dimension kpds(25),kgds(25)
      character*132 c132
      logical lb(360,181)
ckumar
       character*11 envvar            ! for ibm_sp_6000
       character*80 fileg,filegi      ! for ibm_sp_6000
       character*2 clugrb,clugrbix
ckumar
      jf=360*181
      jflag=-1 
      kf=jf
      jpds(2)=-1
      jpds(1)=7
      jpds(3)=3
        print 101,lun,luni
       print 109,(jpds(ll),ll=1,25)
 109   format(5z17)
c
c*** kumar opening grib & index files here
c
      write(clugrb,fmt='(i2)')lun
      write(clugrbix,fmt='(i2)')luni
      print*,'grib & index files unit #s ',clugrb,clugrbix
c
       envvar='FORT   '
       write(envvar(5:6),fmt='(I2)') lun
       call getenv(envvar,fileg)
       call baopen(lun,fileg,iret)
c
       envvar='FORT   '
       write(envvar(5:6),fmt='(I2)') luni
       call getenv(envvar,filegi)
       call baopen(luni,filegi,iret)
c
c******
c
      call getgb1(lun,luni,jf,jflag,jpds,jgds,
     1 grib,kf,k,kpds,kgds,lb,array,ier)
c      if ( ier .ne. 0) stop 99
       call w3fp11(grib,grib(2,1),c132,ierr)
      print 106,k,c132
 106  format(i6, 'LAB ',a132)
      print 101,ier,k,kf
      if(ier .ne.0 ) stop 9999
      print 102,(array(90,k),k=1,181)
 102  format(10f8.2)
 101   format(i9)
cj       do 45,k=1,181
c         do 45,j=1,360
c 45        if(j .gt. 70  .or. k .gt. 70) array(j,k)=array(70,70)
c  SET IRAS VARIABLES
      iras(1)=kpds(14)
      iras(7)=kpds(11)
      iras(8)=kpds(10)
      iras(9)=kpds(9)
      iras(10)=kpds(8)
      call updatr
c      iras(3)=iras(7)
c      iras(4)=iras(8)
c       iras(5)=iras(9)
c       iras(6)=iras(10)
       return
      end
