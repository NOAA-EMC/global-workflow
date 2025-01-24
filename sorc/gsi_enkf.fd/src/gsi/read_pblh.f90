      subroutine read_pblh(nread,ndata,nodata,infile,obstype,lunout,twindin,&
         sis,nobs)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:  read_pblh     read obs from msgs in PREPFITS files (rpf == read aircraft)
!
! program history log:
!   2009-06     whiting - coding rpf
!   2009-10-20    zhu   - modify rpf for reading in pblh data in GSI
!   2009-10-21  whiting - modify cnem & pblhob for reading Caterina's files
!   2013-01-26  parrish - change from grdcrd to grdcrd1 (to allow successful debug compile on WCOSS)
!   2015-02-23  Rancic/Thomas - add l4densvar to time window logical
!   2015-10-01  guo     - consolidate use of ob location (in deg)
!
!   input argument list:
!     infile   - unit from which to read BUFR data
!     obstype  - observation type to process
!     lunout   - unit to which to write data for further processing
!     prsl_full- 3d pressure on full domain grid
!
!   output argument list:
!     nread    - number of type "obstype" observations read
!     nodata   - number of individual "obstype" observations read
!     ndata    - number of type "obstype" observations retained for further processing
!     twindin  - input group time window (hours)
!     sis      - satellite/instrument/sensor indicator
!     nobs     - array of observations on each subdomain for each processor
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
      use kinds, only: r_kind,r_double,i_kind
      use constants, only: zero,one_tenth,one,deg2rad,rad2deg,three
      use gridmod, only: diagnostic_reg,regional,nlon,nlat,&
           tll2xy,txy2ll,rotate_wind_ll2xy,rotate_wind_xy2ll,&
           rlats,rlons
      use convinfo, only: nconvtype,ctwind, &
           icuse,ictype,ioctype
      use gsi_4dvar, only: l4dvar,l4densvar,time_4dvar,winlen
      use obsmod, only: iadate,offtime_data,bmiss
      use deter_sfc_mod, only: deter_sfc2
      use mpimod, only: npe
      implicit none

!     Declare passed variables
      character(len=*),intent(in):: infile,obstype
      character(20),intent(in):: sis
      integer(i_kind),intent(in):: lunout
      integer(i_kind),intent(inout):: nread,ndata,nodata
      integer(i_kind),dimension(npe),intent(inout):: nobs
      real(r_kind),intent(in):: twindin

!     Declare local parameters
      integer(i_kind),parameter:: MXNM=25                 ! max Nems, max Replications
      integer(i_kind),parameter:: MXRP=255                ! max Nems, max Replications
      real(r_kind),parameter:: r360 = 360.0_r_kind

      integer(i_kind) lunin,msgt,ICOMP,idate,nlevp,nlevo,nlevc
      integer(i_kind) iout,nc,nr,nmsg,nrtyp,nrtmax,nchanl
      integer(i_kind) msub,nmsub,ireadsb,ireadmg
      character(80) cnem
      character(8)  ctyp, ctyp0

      character(8) cval(5)
      real(r_kind) rval(5)
      equivalence (cval(1),rval(1))

      real(r_kind) hdr(MXNM)
!     real(r_kind) plv(MXNM,MXRP), olv(MXNM,MXRP,MXRP), &
!          slv(mxnm,mxrp,mxrp)
      real(r_kind) clv(MXNM,MXRP,MXRP)
      real(r_kind),allocatable,dimension(:,:):: cdata_all

      character(10) date
      logical first,outside,inflate_error
      integer(i_kind) ihh,idd,iret,im,iy
      integer(i_kind) ikx,nkx,kx,nreal,ilat,ilon
      integer(i_kind) pblhqm,i,maxobs,j,idomsfc
      integer(i_kind) ntest
!     integer(i_kind),dimension(8):: obs_time,anal_time,lobs_time
!     real(r_kind) ltime,cenlon_tmp
      real(r_kind) usage
      real(r_kind) cdist,disterr,disterrmax,rlon00,rlat00
      real(r_kind) pblhob,pblhoe,pblhelev,pblbak
      real(r_kind) dlat,dlon,dlat_earth,dlon_earth,stnelev
      real(r_kind) dlat_earth_deg,dlon_earth_deg
      real(r_kind) :: tsavg,ff10,sfcr,zz
!     real(r_kind),dimension(5):: tmp_time

      integer(i_kind) idate5(5),minobs,minan
      real(r_kind) time_correction,timeobs,time,toff,t4dv,zeps

      data lunin /50/

!     Initialize variables
      nreal=14
      ntest=0
      nrtmax=0                       ! # rpts to print per msg type (0=all)

!     call closbf(lunin)
      open(lunin,file=trim(infile),form='unformatted')
      call mesgbc(lunin,msgt,icomp)
      call openbf(lunin,'IN',lunin)
      call datelen(10)                          ! set dates to 8-digit

      maxobs=0
      ctyp0=' '
      first=.true.
      do while( ireadmg(lunin,ctyp,idate).EQ.0 )
         if ( ctyp .ne. ctyp0 ) then
           ctyp0=ctyp
           nrtyp=0                        ! counter - rpts per type
         else ! ctyp = ctyp0
           if ( nrtyp .ge. nrtmax .and. nrtmax.ne.0 ) then
             cycle
           endif ! nrtyp >= nrtmax
         endif ! ctyp != ctyp0

         if ( ctyp(1:6).ne.'ADPUPA' .and.  &
              ctyp(1:6).ne.'AIRCAR' .and.  &
              ctyp(1:6).ne.'PROFLR' .and.  &
              ctyp(1:6).ne.'AIRCFT' ) cycle

!        Time offset
         if (first) then
            call time_4dvar(idate,toff)
            first=.false.
         end if

         do while (ireadsb(lunin) .eq. 0)
            nrtyp=nrtyp+1
            cnem='SID XOB YOB DHR ELV TYP T29 ITP'
            call ufbint(lunin,hdr,MXNM,1,iret,cnem)
            if ( iret.ne.1 ) write(*,*)'ERROR - ufbseq(HEADR) iret=',iret
            kx=hdr(6)
            if(kx == 431 .or. kx == 531) nkx= 131
            if(kx == 433 .or. kx == 533) nkx= 133
            if(kx == 435 .or. kx == 535) nkx= 135
            if(kx == 120) nkx= 120
            if(kx == 227) nkx= 181

            loop_convinfo_test: do nc=1,nconvtype
              if (trim(ioctype(nc)) /= trim(obstype))cycle loop_convinfo_test
              if (nkx == ictype(nc)) then
                 ikx=nc
                 maxobs=maxobs+1
              end if
            end do loop_convinfo_test
         end do ! while ireadsb
      end do ! while ireadmg

      allocate(cdata_all(nreal,maxobs))
      nread=0
      nchanl=0
      ilon=2
      ilat=3
      call closbf(lunin)
      close(lunin)
      open(lunin,file=trim(infile),form='unformatted')
      call mesgbc(lunin,msgt,icomp)
      call openbf(lunin,'IN',lunin)
      call datelen(10)                          ! set dates to 8-digit

      nr=0                           ! rpt counter
      nmsg=0                         ! msg counter

      ctyp0=' '
      do while( ireadmg(lunin,ctyp,idate).eq.0 )
      nmsg=nmsg+1
      msub = nmsub(lunin)

      if ( ctyp .ne. ctyp0 ) then 
        if ( ctyp0 .ne. " " ) write(*,*) ! linefeed
        ctyp0=ctyp
        write(*,'(/,a,1x,i10,$)')'  new ctyp="'//ctyp//'" idate=',idate
        write(*,'(3x,a,1x,i3,$)') 'msub=',msub
        nrtyp=0                        ! counter - rpts per type
      else ! ctyp = ctyp0
        if ( nrtyp .ge. nrtmax .and. nrtmax.ne.0 ) then 
          cycle
        endif ! nrtyp >= nrtmax
      endif ! ctyp != ctyp0
       
      if ( ctyp(1:6).ne.'ADPUPA' .and.  &
          ctyp(1:6).ne.'AIRCAR' .and.  &
          ctyp(1:6).ne.'PROFLR' .and.  &
          ctyp(1:6).ne.'AIRCFT' ) then
         nr=nr+msub
         cycle
      endif 

      do while (ireadsb(lunin) .eq. 0) 
      nr=nr+1
      nrtyp=nrtyp+1
      nread=nread+1

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!c===
! prepfits processing
!  based on /meso/save/wx20ps/ucl4/prepfits.tab of 15 Oct 10:45

! dtyps == 
!   ADPUPA AIRCAR AIRCFT SATWND PROFLR ADPSFC SFCSHP VADWND 
!   SATBOG SATEMP SFCBOG SPSSMI SYNDAT ERS1DA GOESND MSONET GPSIPW 

! dtyp => HEADR {PLEVL}
! HEADR == SID XOB YOB DHR ELV TYP T29 ITP
! PLEVL(255) == CAT PRC PQM QQM TQM ZQM WQM  [OBLVL]
! OBLVL(255) == SRC FHR  <PEVN> <QEVN> <TEVN> <ZEVN> <WEVN> <CEVN> <SEVN>
!       PEVN == POB  PMO                                     ! pressure seq
!       QEVN == QOB                                          ! sp hum seq
!       TEVN == TOB                                          ! temperature seq
!       ZEVN == ZOB                                          ! hgt seq
!       WEVN == UOB VOB                                      ! wind seq
!       CEVN == CAPE CINH LI PBL TROP PWO                    ! convective ndx seq
!       SEVN == HOVI MXTM MITM TDO TOCC MXGS THI TCH CDBZ    ! sensible WX seq


! HEADR == SID XOB YOB DHR ELV TYP T29 ITP
!    SID: Station identification               (ascii,     character*8)     a8
!    XOB: Longitude (coarse accuracy)          (deg E,-180.00 -> 475.35)  f7.2
!    YOB: Latitude                             (deg N, -90.00 -> 237.67)  f6.2
!    DHR: Observation time minus cycle time    (hours, -24.00 -> 57.91)   f6.2
!    ELV: Station elevation                        (m,  -1000 -> 130072)    i6
!    TYP: Report type                       (code tbl,      0 -> 511)       i3
!    T29: Data dump report type             (code tbl,      0 -> 1023)      i4
!    ITP: Instrument type                   (code tbl,      0 -> 255)       i3

! PLEVL(255) == CAT PRC PQM QQM TQM ZQM WQM  [OBLVL]
!    CAT: Data level category               (code tbl,      0 -> 63)        i2
!    PRC: Pressure coordinate                     (mb,     .0 -> 1638.3)  f6.1
!    PQM: Pressure (quality) marker         (code tbl,      0 -> 31)        i2
!    QQM: Sp humidity (quality) marker      (code tbl,      0 -> 31)        i2
!    TQM: Temp (TOB) (quality) marker       (code tbl,      0 -> 31)        i2
!    ZQM: Height (quality) marker           (code tbl,      0 -> 31)        i2
!    WQM: u-, v- wind (quality) marker      (code tbl,      0 -> 31)        i2

! OBLVL(255) == SRC FHR  <PEVN> <QEVN> <TEVN> <ZEVN> <WEVN> <CEVN> <SEVN>
!    SRC: File name of data source             (ascii,    character*24)    a24
!    FHR: Forecast length                       (hour,     .0 -> 409.5)   f5.1

! PEVN == POB PMO
!    POB: Pressure observation                    (mb,     .0 -> 1638.3)  f6.1
!    PMO: Mean sea-level pressure observation     (mb,     .0 -> 1638.3)  f6.1

! QEVN == QOB
!    QOB: Specific humidity observation        (mg/kg,      0 -> 65535)     i5

! TEVN == TOB
!    TOB: Temp observation (drybulb or virt)   (deg c, -273.2 -> 1365.1)  f6.1

! ZEVN == ZOB
!    ZOB: Height observation                       (m,  -1000 -> 130071)    i6

! WEVN == UOB VOB
!    UOB: Wind u- component observation          (m/s,  -409.6 -> 409.5)  f6.1
!    VOB: Wind v- component observation          (m/s,  -409.6 -> 409.5)  f6.1

! CEVN == CAPE CINH LI PBL TROP PWO
!   CAPE: convective available pot energy       (j/kg,      0 -> 16383)     i5
!   CINH: convective inhibition                 (j/kg,  -3890 -> 205)       i5
!     LI: Lifted Index                             (K,   -5.0 -> 97.3)    f4.1
!c PLFTI: Lifted Index                             (K,    -20 -> 43)        i2  (v2)
!c  LFTI: PREPBUFR: Lifted Index                   (K,   -5.0 -> 97.3)    f4.1  (v2)
!    PBL: Height of planetary boundary layer       (m,   -4.0 -> 6549.5)  f6.1
!c  HPBL: Height of planetary boundary layer       (m,      0 -> 8191)      i4  (v2)
!   TROP: tropopause level                        (mb,     .0 -> 1638.3)  f5.1
!c  PRLC: Pressure                                (Pa,      0 -> 163830)    i6  (v2)
!    PWO: Tot column precipitable water (kg/m^2 or mm,     .0 -> 204.7)   f5.1

!  SEVN == HOVI MXTM MITM TDO TOCC MXGS THI TCH CDBZ
!   HOVI: Horizontal Visibility                    (m,     .0 -> 819.1)   f5.1
!   MXTM: Maximum temperature                      (K,    .00 -> 655.35)  f6.2
!   MITM: Minimum temperature                      (K,    .00 -> 655.35)  f6.2
!    TDO: Dew-point temperature observation    (deg c, -273.1 -> 1365.2)  f6.1
!   TOCC: cloud cover (total)                      (%,      0 -> 127)       i3
!   MXGS: Maximum wind gust speed                (m/s,     .0 -> 409.5)   f5.1
!    THI: Heat index                           (deg F,     .0 -> 6553.5)  f5.1
!    TCH: wind chill                           (deg F,     .0 -> 6553.5)  f5.1
!   CDBZ: ?                                        (m,     .0 -> 1638.3)  f5.1
!c  HOCB: Height of base of cloud                  (m,   -400 -> 20070)     i5  (v2)
!c===

! HEADR == SID XOB YOB DHR ELV TYP T29 ITP
      cnem='SID XOB YOB DHR ELV TYP T29 ITP'
      call ufbint(lunin,hdr,MXNM,1,iret,cnem)
      if ( iret.ne.1 ) write(*,*)'ERROR - ufbseq(HEADR) iret=',iret
      kx=hdr(6)
      if(kx == 431 .or. kx == 531) nkx= 131
      if(kx == 433 .or. kx == 533) nkx= 133
      if(kx == 435 .or. kx == 535) nkx= 135
      if(kx == 120) nkx= 120
      if(kx == 227) nkx= 181

!     Extract station id, type, date, and location information
      rval(1)=hdr(1)      ! SID

      if(hdr(2)>= r360)hdr(2)=hdr(2)-r360
      if(hdr(2) < zero)hdr(2)=hdr(2)+r360
      dlon_earth_deg=hdr(2)
      dlat_earth_deg=hdr(3)
      dlon_earth=hdr(2)*deg2rad
      dlat_earth=hdr(3)*deg2rad
      if(regional)then
         call tll2xy(dlon_earth,dlat_earth,dlon,dlat,outside)    ! convert to rotated coordinate
         if(diagnostic_reg) then
            call txy2ll(dlon,dlat,rlon00,rlat00)
            ntest=ntest+1
            cdist=sin(dlat_earth)*sin(rlat00)+cos(dlat_earth)*cos(rlat00)* &
                 (sin(dlon_earth)*sin(rlon00)+cos(dlon_earth)*cos(rlon00))
            cdist=max(-one,min(cdist,one))
            disterr=acos(cdist)*rad2deg
            disterrmax=max(disterrmax,disterr)
         end if
         if(outside) cycle   ! check to see if outside regional domain
      else
         dlat = dlat_earth
         dlon = dlon_earth
         call grdcrd1(dlat,rlats,nlat,1)
         call grdcrd1(dlon,rlons,nlon,1)
      endif

      if(offtime_data) then

!       in time correction for observations to account for analysis
!                time being different from obs file time.
        write(date,'( i10)') idate
        read (date,'(i4,3i2)') iy,im,idd,ihh
        idate5(1)=iy
        idate5(2)=im
        idate5(3)=idd
        idate5(4)=ihh
        idate5(5)=0
        call w3fs21(idate5,minobs)    !  obs ref time in minutes relative to historic date
        idate5(1)=iadate(1)
        idate5(2)=iadate(2)
        idate5(3)=iadate(3)
        idate5(4)=iadate(4)
        idate5(5)=0
        call w3fs21(idate5,minan)    !  analysis ref time in minutes relative to historic date

!       Add obs reference time, then subtract analysis time to get obs time relative to analysis

        time_correction=real(minobs-minan,r_kind)/60._r_kind

      else
        time_correction=zero
      end if

      timeobs=real(real(hdr(4),4),8)
      time=timeobs + time_correction
      t4dv=timeobs + toff
      zeps=1.0e-8_r_kind
      if (t4dv<zero  .and.t4dv>      -zeps) t4dv=zero
      if (t4dv>winlen.and.t4dv<winlen+zeps) t4dv=winlen
      t4dv=t4dv + time_correction
      nc=ikx
      if (l4dvar.or.l4densvar) then
           if (t4dv<zero.OR.t4dv>winlen) cycle
      else
           if((real(abs(time)) > real(ctwind(nc)) .or. real(abs(time)) > real(twindin))) cycle 
      end if

      stnelev=hdr(5)
      pblhelev=stnelev


! PLEVL == CAT PRC PQM QQM TQM ZQM WQM [OBLVL]
!     cnem='CAT PRC PQM QQM TQM ZQM WQM'
!     call ufbint(lunin,plv,MXNM,MXRP, nlev,cnem)

! OBLVL == SRC FHR <PEVN> <QEVN> <TEVN> <ZEVN> <WEVN> <CEVN> <SEVN>
!       == SRC FHR POB PMO QOB    TOB    ZOB   UOB VOB ...  ! {P,Q,T,Z,W}EVN
!     cnem='SRC FHR POB PMO QOB TOB ZOB UOB VOB'
!     call ufbin3(lunin,olv,MXNM,MXRP,MXRP, nlevp,nlevo,cnem)

! CEVN == CAPE CINH LI PBL TROP PWO
!     cnem='CAPE CINH LI PBL TROP PWO'
      cnem='PBL'   ! for Caterina's files (ruc_raobs)
      clv=bmiss
      call ufbin3(lunin,clv,MXNM,MXRP,MXRP, nlevp,nlevc,cnem)
!     pblhob=clv(4,1,2)
      pblhob=clv(1,1,2)
      pblbak=clv(1,1,1)   ! model PBL; from Caterina's files
      if (abs(pblbak-bmiss).lt.10.e5 .or. abs(pblhob-bmiss).lt.10.e5) cycle ! <skip processing of this report>
      
      pblhqm=0
      if (pblhob .lt. 0.0) pblhqm=15
!     if (nkx==131 .or. nkx==133 .or. nkx==135) then
!       anal_time=0
!       obs_time=0
!       tmp_time=zero
!       tmp_time(2)=timeobs
!       anal_time(1)=iadate(1)
!       anal_time(2)=iadate(2)
!       anal_time(3)=iadate(3)
!       anal_time(5)=iadate(4)
!       call w3movdat(tmp_time,anal_time,obs_time) ! observation time

!       lobs_time=0
!       tmp_time=zero
!       cenlon_tmp=hdr(2)
!       if (hdr(2) > 180.0) cenlon_tmp=hdr(2)-360.0_r_kind
!       tmp_time(2)=cenlon_tmp/15.0_r_kind
!       call w3movdat(tmp_time,obs_time,lobs_time) ! local observation time
!       ltime = lobs_time(5)+lobs_time(6)/60.0_r_kind+lobs_time(7)/3600.0_r_kind
!       if ((ltime.gt.21.0) .and. (ltime.lt.5.0)) pblhqm=3
!     end if

      if (kx==531 .or. kx==533 .or. kx==535) pblhqm=3 ! descending profile

!     Set usage variable
      usage = 0.
      if(icuse(nc) <= 0) usage=150.
      if(pblhqm == 15 .or. pblhqm == 9) usage=150.

!     Set inflate_error logical 
      inflate_error=.false.
      if (pblhqm == 3) inflate_error=.true.

! SEVN == HOVI MXTM MITM TDO TOCC MXGS THI TCH CDBZ
!     cnem='HOVI MXTM MITM TDO TOCC MXGS THI TCH CDBZ'
!     call ufbin3(lunin,slv,MXNM,MXRP,MXRP, nlevp,nlevs,cnem)

!--Outputs

! ---HEADR
      if (nrtyp .eq. 1)  write(*,'(/,11x,a,$)') &
       'SID       XOB    YOB     DHR    ELV  TYP  T29 ITP'

      write(*,'(/,i3,i4,$)') nmsg, nr                            ! msg#, rpt#

      rval(1)=hdr(1) ; write(*,'(1x,a,$)') "'"//cval(1)//"'"     ! SID
      write(*,'(1x,f7.2,2(1x,f6.2),$)') (hdr(i),i=2,4)           ! XOB,YOB,DHR
      write(*,'(1x,i6,1x,i3,1x,i4,1x,i3,$)') (int(hdr(i)),i=5,8) ! ELV,TYP,T29,ITP

      write(*,'(1x,2(2x,a,1x,i3),a,$)') '(olv=',nlevo,'nlevp=',nlevp,')'
      write(*,'(1x,2(2x,a,1x,i3),a,$)') '(clv=',nlevc,'nlevp=',nlevp,')'

      ndata=ndata+1
      nodata=nodata+1
      iout=ndata

      if(ndata > maxobs) then
           write(6,*)'READ_PREPFITS:  ***WARNING*** ndata > maxobs for ',obstype
           ndata = maxobs
      end if

!     Get information from surface file necessary for conventional data here
      call deter_sfc2(dlat_earth,dlon_earth,t4dv,idomsfc,tsavg,ff10,sfcr,zz)

!     setup for sort of averaged obs 
      pblhoe=400.0_r_kind  ! temporarily
      if (nkx==120) pblhoe=200.
      if (nkx==181) pblhoe=300.
      if (inflate_error) pblhoe=pblhoe*1.5_r_kind

      cdata_all(1,iout)=pblhoe                  ! pblh error (cb)
      cdata_all(2,iout)=dlon                    ! grid relative longitude
      cdata_all(3,iout)=dlat                    ! grid relative latitude
      cdata_all(4,iout)=pblhelev                ! pblh obs elevation
      cdata_all(5,iout)=pblhob                  ! pblh obs
      cdata_all(6,iout)=rval(1)                 ! station id
      cdata_all(7,iout)=t4dv                    ! time
      cdata_all(8,iout)=nc                      ! type
      cdata_all(9,iout)=pblhoe*three            ! max error
      cdata_all(10,iout)=pblhqm                 ! quality mark
      cdata_all(11,iout)=usage                  ! usage parameter
      cdata_all(12,iout)=dlon_earth_deg         ! earth relative longitude (degrees)
      cdata_all(13,iout)=dlat_earth_deg         ! earth relative latitude (degrees)
      cdata_all(14,iout)=stnelev                ! station elevation (m)

      end do ! while ireadsb

      end do ! while ireadmg
      write(*,*) ! closing linefeed, debug?

      call closbf(lunin)
      close(lunin)
!   Normal exit

!   Write observation to scratch file
     call count_obs(ndata,nreal,ilat,ilon,cdata_all,nobs)
     write(lunout) obstype,sis,nreal,nchanl,ilat,ilon
     write(lunout) ((cdata_all(j,i),j=1,nreal),i=1,ndata)
     deallocate(cdata_all)
 
     if (ndata == 0) then
        write(6,*)'READ_PREPFITS no data'
     endif

     end subroutine read_pblh
