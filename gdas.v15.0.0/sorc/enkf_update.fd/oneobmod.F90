module oneobmod
!$$$ module documentation block
!           .      .    .                                       .
! module:   oneobmod
!   prgmmr: kleist      org: np20                date: 2003-10-20
!
! abstract: module contains everything necessary for running single
!           observation experiments
!
! program history log:
!   2003-10-20  kleist
!   2004-05-13  kleist, documentation
!   2005-04-04  todling, fixed little endian ouput of prepqc file
!   2009-04-28  sienkiewicz - add text output for ozone level obs testing
!   2012-07-14  todling - only do it once (in observer mode)
!   2014-05-29  thomas - add lsingleradob parameter for single radiance
!                        assimilation (originally of mccarty)
!
! subroutines included:
!   sub init_oneobmod
!   sub oneobmakebufr
!   sub oneobo3lv
!
! variable definitions:
!   def maginnov   - magnitude of innovation for one ob exp
!   def magoberr   - magnitude of observational error for one ob exp
!   def oblat      - observation latitude for one ob exp
!   def oblon      - observation longitude for one ob exp
!   def obhourset  - observation delta time from analysis time for 
!                    one ob exp
!   def obpres     - observation pressure (hPa) or one ob exp
!   def obdattim   - observation date for one ob exp
!   def oneob_type - observation type for one ob exp
!   def oneobtest  - single observation test flag (true=on)
!   def pctswitch  - if true, innovation and error expressed as percentage
!                        of background value
!
!$$$
  use kinds, only: r_kind,i_kind

  implicit none

! set default to private
  private
! set subroutines to public
  public :: init_oneobmod
  public :: oneobmakebufr
  public :: oneobo3lv
! set passed variables to public
  public :: oneobtest,oneob_type,magoberr,pctswitch,maginnov
  public :: oblat,oblon,obpres,obdattim,obhourset
  public :: lsingleradob, obchan

  real(r_kind) maginnov, magoberr, oblat, oblon,&
    obhourset, obpres
  integer(i_kind) obdattim
  character(10) oneob_type
  logical oneobtest
  logical pctswitch
  logical lsingleradob
  integer(i_kind) obchan

  logical :: oneobmade

contains

  subroutine init_oneobmod
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    init_oneobmod
!   prgmmr: kleist           org: np20                date: 2003-10-20
!
! abstract: initialize defaults for single ob experiment vars
!
! program history log:
!   2003-10-20  kleist
!   2004-05-13  kleist, documentation
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: zero, one, r1000
    implicit none

    oneobtest=.false.
    maginnov=one
    magoberr=one
    oneob_type=' '
    oblat=zero
    oblon=zero
    obpres=r1000
    obdattim=2000010100
    obhourset=zero
    pctswitch=.false.
    lsingleradob=.false.
    obchan=zero

    oneobmade=.false.
    return
  end subroutine init_oneobmod

  subroutine oneobmakebufr
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    oneobmakebufr
!   prgmmr: kleist           org: np20                date: 2003-10-20
!
! abstract: create prepbufr file for single ob experiment
!
! program history log:
!   2003-10-20  kleist
!   2004-05-13  kleist  documentation
!   2006-04-06  middlecoff - changed lumk from 52 to lendian_in so one-obs prepqc 
!                            file can be read as little endian
!   2014-08-04  carley - modify for tcamt and howv obs
!   2014-08-18  carley - added td2m, mxtm, mitm, pmsl, and wsdp10m
!   2016-01-18  pondeca - added cldch
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use constants, only: zero, one, five, one_tenth, r100, r0_01
    use gsi_io, only: lendian_in
    use obsmod, only: offtime_data,iadate,bmiss
    implicit none

    real(r_kind),parameter:: r20=20.0_r_kind

    integer(i_kind) ludx,nobs,nlev,idate
    character(8) subset,sid(1)
    real(r_kind),dimension(1):: typ
    real(r_kind),dimension(1,1):: qob,tob,zob,uob,vob,cat
    real(r_kind),dimension(1,1):: pqm,qqm,tqm,zqm,wqm
    real(r_kind),dimension(1,1):: poe,qoe,toe,woe
    real(r_kind),dimension(1):: xob,yob,dhr
    real(r_kind),dimension(1,1):: pob    
    integer(i_kind) n,k,iret
    real(r_kind) hdr(10),obs(13,255),qms(10,255),err(10,255),cld2seq(2,1), &
                 cldseq(3,10),owave(1,255),maxtmint(2,255),cldceilh(1,255)
    character(80):: hdrstr='SID XOB YOB DHR TYP'
    character(80):: obsstr='POB QOB TOB ZOB UOB VOB CAT PWO MXGS HOVI PRSS TDO PMO'
    character(80):: qmsstr='PQM QQM TQM ZQM WQM PWQ PMQ'
    character(80):: errstr='POE QOE TOE WOE'
    character(80):: cld2seqstr='TOCC HBLCS'      ! total cloud cover and height above surface of base of lowest cloud seen
    character(80):: cldseqstr='VSSO CLAM HOCB'   ! vertical significance, cloud amount and cloud base height
    character(80):: owavestr='HOWV'              ! significant wave height
    character(80):: maxtmintstr='MXTM MITM'      ! Max T and Min T
!   character(80):: cldceilhstr='CEILING'        ! cldch
    if (oneobmade) return

    if (oneob_type == 'o3lev') then
       call oneobo3lv
       return
    end if
! set values from parameter list
    xob=oblon
    yob=oblat
    dhr=obhourset
    idate=iadate(1)*1000000+iadate(2)*10000+iadate(3)*100+iadate(4)
    write(6,*)'OneObMake: ', idate
    pob=obpres
! set default values for this routine
    ludx=22
    nobs=1
    nlev=1
    subset='ADPUPA'
    sid='SID00001'
    qob=r100
    tob=r20
    zob=zero
    uob=five
    vob=five
    owave=bmiss
    maxtmint=bmiss
    cldceilh=bmiss
    cld2seq=bmiss
    cldseq=bmiss
    pqm=one
    qqm=one
    tqm=one
    zqm=one
    wqm=one
    offtime_data=.true.
    if (oneob_type=='ps') then
       typ(1)=87._r_kind
       cat(1,1)=zero
    else if (oneob_type=='tcamt') then
       subset='ADPSFC'
       typ(1)=87._r_kind
       cat(1,1)=zero
       cld2seq(1,1)=25._r_kind !TOCC - total cloud amount (%)       
    else if (oneob_type=='howv') then
       subset='SFCSHP'
       typ(1)=80._r_kind
       cat(1,1)=zero
       owave(1,1)=4._r_kind !Significant wave height (m - includes wind+swell waves)
    else if (oneob_type=='td2m') then
       subset='ADPSFC'
       typ(1)=87._r_kind
       cat(1,1)=zero
       obs(12,1)=280.0_r_kind !Dewpoint in Kelvin
    else if (oneob_type=='mxtm') then
       subset='ADPSFC'
       typ(1)=87._r_kind
       cat(1,1)=zero
       maxtmint(1,1)=280.0_r_kind !Max T in Kelvin
    else if (oneob_type=='mitm') then
       subset='ADPSFC'
       typ(1)=87._r_kind
       cat(1,1)=zero
       maxtmint(2,1)=273.15_r_kind !Min T in Kelvin
    else if (oneob_type=='pmsl') then
       subset='ADPSFC'
       typ(1)=87._r_kind
       cat(1,1)=zero
       obs(13,1)=1008.10_r_kind !PMSL in MB
       qms(7,1)=one
    else if (oneob_type=='wspd10m') then !10m AGL wind speed - need to store both u and v (already by this point done)
       subset='ADPSFC'
       typ(1)=87._r_kind
       cat(1,1)=zero
    else if (oneob_type=='cldch') then
       subset='ADPSFC'
       typ(1)=87._r_kind
       cat(1,1)=zero
       cldceilh(1,1)=1000._r_kind !clch in m
    else
       typ(1)=20._r_kind
       cat(1,1)=one
    endif
! keep errs small so the single ob passes the QC check
    poe=r0_01
    qoe=one_tenth
    toe=one_tenth
    woe=one_tenth

    open(ludx,file='prepobs_prep.bufrtable',action='read')
#if defined(__osf__) || defined(__ia64__) && (__INTEL_COMPILER>799)
    open(lendian_in,file='prepqc',action='write',form='unformatted',convert='little_endian')
#else
    open(lendian_in,file='prepqc',action='write',form='unformatted')
#endif

    call datelen(10)
    call openbf(lendian_in,'OUT',ludx)
    do n=1,nobs
       hdr(1)=transfer(sid(n),hdr(1))
       hdr(2)=xob(n)
       hdr(3)=yob(n)
       hdr(4)=dhr(n)
       hdr(5)=r100+typ(n)
       obs=bmiss
       qms=bmiss
       err=bmiss
       do k=1,nlev
          obs(1,k)=pob(k,n)
          obs(2,k)=qob(k,n)
          obs(3,k)=tob(k,n)
          obs(4,k)=zob(k,n)
          obs(7,k)=cat(k,n)
          qms(1,k)=pqm(k,n)
          qms(2,k)=qqm(k,n)
          qms(3,k)=tqm(k,n)
          qms(4,k)=zqm(k,n)
          err(1,k)=poe(k,n)
          err(2,k)=qoe(k,n)
          err(3,k)=toe(k,n)
       enddo
       call openmb(lendian_in,subset,idate)
       call ufbint(lendian_in,hdr,10,1,iret,hdrstr)
       call ufbint(lendian_in,obs,13,nlev,iret,obsstr)
       call ufbint(lendian_in,qms,10,nlev,iret,qmsstr)
       call ufbint(lendian_in,err,10,nlev,iret,errstr)
       if (oneob_type=='tcamt') then
         call ufbint(lendian_in,cldseq,3,10,iret,cldseqstr)
         call ufbint(lendian_in,cld2seq,2,1,iret,cld2seqstr)
       else if (oneob_type=='howv') then
         call ufbint(lendian_in,owave,1,nlev,iret,owavestr)
       else if ( oneob_type=='mxtm' .or. oneob_type=='mitm') then
         call ufbint(lendian_in,maxtmint,2,nlev,iret,maxtmintstr)
       end if                              
       call writsb(lendian_in)
       hdr(1)=transfer(sid(n),hdr(1))
       hdr(2)=xob(n)
       hdr(3)=yob(n)
       hdr(4)=dhr(n)
       hdr(5)=200_r_kind+typ(n)
       obs=bmiss
       qms=bmiss
       err=bmiss
       do k=1,nlev
          obs(1,k)=pob(k,n)
          obs(5,k)=uob(k,n)
          obs(6,k)=vob(k,n)
          obs(7,k)=cat(k,n)
          qms(1,k)=pqm(k,n)
          qms(5,k)=wqm(k,n)
          err(1,k)=poe(k,n)
          err(4,k)=woe(k,n)
       enddo
       call openmb(lendian_in,subset,idate)
       call ufbint(lendian_in,hdr,10,1,iret,hdrstr)
       call ufbint(lendian_in,obs,13,nlev,iret,obsstr)
       call ufbint(lendian_in,qms,10,nlev,iret,qmsstr)
       call ufbint(lendian_in,err,10,nlev,iret,errstr)
       call writsb(lendian_in)
    enddo
    call closbf(lendian_in)
 
    oneobmade=.true.

    return
  end subroutine oneobmakebufr

  subroutine oneobo3lv
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    oneobmls
!
! abstract: create ozone level text file for single ob experiment
!
! program history log:
!   2007-09-11  Sienkiewicz - extend to create MLS text file on request
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:  ?
!
!$$$
    use constants, only: zero, one
    implicit none

    integer(i_kind) lumk                          ! output unit
    integer(i_kind) ilev, isnd
    integer(i_kind) ildat(8),jldat(8)             ! "local" date/time
    real(r_kind)    rsec,rlnc(5)
    real(r_kind)    ppmv

2   format(i5,4i3,f6.2,i7,i5,f10.4,f11.4,e16.7,i7,i5,g16.7,g15.7,f6.3)

    lumk = 22
    ilev = 1               ! ilev > 24 is passive
    isnd = 1
    ppmv = one                ! dummy value 

!    obdattim=2000010100

    rlnc = zero
    rlnc(2) = obhourset
    ildat(1) = obdattim  / 1000000            ! year
    ildat(2) = mod(obdattim,1000000)/10000    ! month
    ildat(3) = mod(obdattim,10000)/100        ! day
    ildat(4) = 0
    ildat(5) = mod(obdattim,100)              ! hour

    ildat(6:8) = 0                               ! (no minute/sec in obdattim)

    call w3movdat(rlnc,ildat,jldat)

    rsec = jldat(7)+jldat(8)*1.e-3_r_kind

! open data file for output.  for oneobtype gsimain sets the dfile(1) 
! to be prepqc
    open(unit=lumk,file='prepqc',form='formatted')

    write(lumk,2) jldat(1),jldat(2),jldat(3),jldat(5),jldat(6),rsec,isnd,&
         ilev,oblat,oblon,ppmv,isnd,isnd,magoberr,obpres,magoberr
    close(lumk)

    return

  end subroutine oneobo3lv

end module oneobmod
