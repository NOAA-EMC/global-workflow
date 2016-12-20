      subroutine wrtsfc(ioproc,noflx,zhour,fhour,idate,colat1,secswr,
     &                  seclwr,slmsk,
     &                  hice ,fice ,                                    ! for sea-ice - xw nov04
     &                  dusfc,dvsfc,dtsfc,dqsfc,tsea,smc,stc,
     &                  gflux,fluxr,dlwsfc,ulwsfc,
     &                  sheleg,geshem,bengsh,cldwrk,u10m,v10m,
     &                  t2m,q2m,psurf,
     &                  tmpmax,tmpmin,runoff,ep,dugwd,dvgwd,
     &                  hpbl,pwat,cv,cvt,cvb,
!clu [+1l]: add additional state variables (canopy,slc,snwdph)
     &                  canopy,slc,snwdph,
!cwei: additional 30 fields
     &                  zorl,vfrac,vtype,stype,slope,uustar,oro,
     &                  srflag,chh,cmm,epi,dlwsfci,ulwsfci,uswsfci,
     &                  dswsfci,dtsfci,dqsfci,gfluxi,srunoff,tt1,
     &                  q1,u1,v1,zlvl,evbsa,evcwa,transa,sbsnoa,
     &                  snowca,soilm,
     &           global_lats_r,lonsperlar)
!!
      use machine
      use resol_def
      use layout1
      use sig_io
      use namelist_def
!     use module_nst_parameters, only : nj_nst
      implicit none
!!
      integer              global_lats_r(latr)
      integer              lonsperlar(latr)
      integer   ioproc
!!
      integer   iprs,itemp,iznlw,imerw,isphum,ipwat,
     $          ipcpr,isnowd,icldf,iccldf,
     $          islmsk,izorl,ialbdo,isoilm,icemsk,
     $          ilhflx,ishflx,izws,imws,ighflx,
     $          iuswfc,idswfc,iulwfc,idlwfc,
     $          inswfc,inlwfc,
     $          idswvb,idswvd,idswnb,idswnd,
     $          itmx,itmn,irnof,iep,
     &          icldwk,izgw,imgw,ihpbl,
     $          idswf,idlwf,iuswf,iulwf,icpcpr,
     $          isfc,itoa,ielev,
     $          isglev,idbls,i2dbls,icolmn,
     $          iblbl,ibltl,ibllyr,
     $          ilcbl,ilctl,ilclyr,
     $          imcbl,imctl,imclyr,
     $          ihcbl,ihctl,ihclyr,
     $          icvbl,icvtl,icvlyr,
     $          inst,iwin,iavg,iacc,
     $          ifhour,ifday,
!    $          len,nfld,
     $          nfld,
     $          iuvbf,iuvbfc,
     $          j,i,k,k4,l,noflx
     &,         isik                                    ! for sea-ice - xw nov04
!clu [+1l]: declare additional parameter index
     +,         islc,isnod,icnp
     &,  iveg, ivtp, istp, islo,iust,ihgt,irst,ichh
     &,  icmm,isrf,ievbs,ievcw,itran,isbs,isnc,istc
      parameter(nfld=18)
       integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
       real (kind=kind_io8) rtimer(nfld),rtime,rtimsw,rtimlw
       real (kind=kind_io8) colat1
       real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
c
      parameter(iprs=1,itemp=11,iznlw=33,imerw=34,isphum=51,ipwat=54,
     $          ipcpr=59,isnowd=65,icldf=71,iccldf=72,
     $          islmsk=81,izorl=83,ialbdo=84,isoilm=144,icemsk=91,
     $          isik=92,                                ! for sea-ice - xw nov04
     $          ilhflx=121,ishflx=122,izws=124,imws=125,ighflx=155,
     $          iuswfc=160,idswfc=161,iulwfc=162,idlwfc=163,
     $          inswfc=164,inlwfc=165,
     $          idswvb=166,idswvd=167,idswnb=168,idswnd=169,
     $          itmx=15,itmn=16,irnof=90,iep=145,
     &          icldwk=146,izgw=147,imgw=148,ihpbl=221,
     $          idswf=204,idlwf=205,iuswf=211,iulwf=212,icpcpr=214,
     &          iuvbf=200,iuvbfc=201)
      parameter(isfc=1,itoa=8,ielev=105,
     $          isglev=109,idbls=111,i2dbls=112,icolmn=200,
!cwei    $          isglev=107,idbls=111,i2dbls=112,icolmn=200,
     $          iblbl=209,ibltl=210,ibllyr=211,
     $          ilcbl=212,ilctl=213,ilclyr=214,
     $          imcbl=222,imctl=223,imclyr=224,
     $          ihcbl=232,ihctl=233,ihclyr=234,
     $          icvbl=242,icvtl=243,icvlyr=244)

!clu [+1l]: define parameter index, using table 130
      parameter(islc=160,isnod=66)
!cwei
      parameter(islo=222,isbs=198,isnc=238,icmm=179)
!clu [+1l]: define parameter index, using table 2
      parameter(icnp=223)
!cwei
      parameter(iveg=87,ivtp=225,istp=224,iust=253,ihgt=7,
     $          irst=140,ichh=208,isrf=235,ievbs=199,
     $          ievcw=200,itran=210,istc=86)

      parameter(inst=10,iwin=2,iavg=3,iacc=4)
      parameter(ifhour=1,ifday=2)
!     parameter(len=lonr*latr)
      real(kind=kind_io4) wrkga(lonr*latr)
      real(kind=kind_io8) slmskful(lonr*latr)
      real(kind=kind_io8) slmskloc(lonr,lats_node_r)
c
      logical(1) lbm(lonr*latr)
      character g(200+lonr*latr*(16+1)/8)
      integer   ipur(nfld),itlr(nfld)
      data      ipur/iulwf , iuswf , iuswf , idswf ,  icldf,   iprs,
     $                 iprs, itemp ,  icldf,   iprs,   iprs, itemp ,
     $                icldf,   iprs,   iprs, itemp ,  iuvbf, iuvbfc /
      data      itlr/itoa  , itoa  , isfc  , isfc  , ihclyr, ihctl ,
     $               ihcbl , ihctl , imclyr, imctl , imcbl , imctl ,
     $               ilclyr, ilctl , ilcbl , ilctl , isfc  , isfc /
!    $               ilclyr, ilctl , ilcbl , ilctl /
      integer     idate(4), ids(255),iens(5)
      real (kind=kind_io8) si(levp1)
c
csela..................................................................
      real (kind=kind_io8)   rflux(lonr,lats_node_r,27)
      real (kind=kind_io8)   glolal(lonr,lats_node_r)
      real (kind=kind_io8)   buffo(lonr,lats_node_r)
      real (kind=kind_io4)   buff1l(lonr*latr)
csela..................................................................
      real (kind=kind_io8)  fluxr(nfxr,lonr,lats_node_r)
      real (kind=kind_io8) slmsk (lonr,lats_node_r),
     &    sheleg(lonr,lats_node_r),tsea  (lonr,lats_node_r),
     &     cv    (lonr,lats_node_r),cvt   (lonr,lats_node_r),
     &     cvb   (lonr,lats_node_r),
     &     tmpmin (lonr,lats_node_r),tmpmax (lonr,lats_node_r),
     &     geshem (lonr,lats_node_r),
     &     dusfc (lonr,lats_node_r),dvsfc (lonr,lats_node_r),
     &     dtsfc (lonr,lats_node_r),dqsfc(lonr,lats_node_r),
     &     dlwsfc (lonr,lats_node_r),ulwsfc(lonr,lats_node_r),
     &     gflux (lonr,lats_node_r),runoff(lonr,lats_node_r),
     &     ep (lonr,lats_node_r),cldwrk(lonr,lats_node_r),
     &     dugwd (lonr,lats_node_r),dvgwd(lonr,lats_node_r),
     &     bengsh (lonr,lats_node_r),psurf(lonr,lats_node_r),
     &     u10m (lonr,lats_node_r),v10m(lonr,lats_node_r),
     &     t2m (lonr,lats_node_r),q2m(lonr,lats_node_r),
     &     hpbl (lonr,lats_node_r),pwat(lonr,lats_node_r),
     &     smc (lsoil,lonr,lats_node_r),
     &     stc (lsoil,lonr,lats_node_r)
!clu [+2l]: add slc and snwdph
     +,    slc (lsoil,lonr,lats_node_r)
     +,    snwdph(lonr,lats_node_r)
     +,    canopy(lonr,lats_node_r)
!cwei: additional 30 fields
     +,    zorl(lonr,lats_node_r),vfrac(lonr,lats_node_r)
     +,    vtype(lonr,lats_node_r),stype(lonr,lats_node_r)
     +,    slope(lonr,lats_node_r),uustar(lonr,lats_node_r)
     +,    oro(lonr,lats_node_r),srflag(lonr,lats_node_r)
     +,    chh(lonr,lats_node_r),cmm(lonr,lats_node_r)
     +,    epi(lonr,lats_node_r),dlwsfci(lonr,lats_node_r)
     +,    ulwsfci(lonr,lats_node_r),uswsfci(lonr,lats_node_r)
     +,    dswsfci(lonr,lats_node_r),dtsfci(lonr,lats_node_r)
     +,    dqsfci(lonr,lats_node_r),gfluxi(lonr,lats_node_r)
     +,    srunoff(lonr,lats_node_r)
     +,    tt1(lonr,lats_node_r),q1(lonr,lats_node_r)
     +,    u1(lonr,lats_node_r),v1(lonr,lats_node_r)
     +,    zlvl(lonr,lats_node_r),evbsa(lonr,lats_node_r)
     +,    evcwa(lonr,lats_node_r),transa(lonr,lats_node_r)
     +,    sbsnoa(lonr,lats_node_r),snowca(lonr,lats_node_r)
     +,    soilm(lonr,lats_node_r)
!c-- xw: for sea-ice nov04
      real (kind=kind_io8) hice(lonr,lats_node_r),
     &     fice(lonr,lats_node_r)
!c-- xw: end sea-ice
csela..................................................................
      integer kmsk(lonr,lats_node_r),kmsk0(lonr,lats_node_r)
      integer kmskcv(lonr,lats_node_r)
cjfe
      ids=0
      g=' '
cjfe
!!
      kmsk=nint(slmsk)
      kmsk0=0
      call uninterpred(1,kmsk,glolal,slmsk,global_lats_r,lonsperlar)
      slmskloc=glolal
      call unsplit2d(ioproc,buff1l,glolal,global_lats_r)
      slmskful=buff1l
c
      do k=1,27
       do j=1,lats_node_r
        do i=1,lonr
         rflux(i,j,k)=fluxr(k,i,j)
        enddo
       enddo
      enddo
!!
      call idsdef(1,ids)
! uv-b scaling factor, if set up already, comment the next 2 lines out
      ids(iuvbf)  = 2
      ids(iuvbfc) = 2
! ice conentration and thickness scaling factor
      ids(icemsk) = 3      ! ice concentration ()
      ids(isik)   = 2      ! ice thickness (m)
!
!wei added 10/24/2006
      ids(izorl)=4
      ids(ihgt)=3
      ids(iveg)=2
      ids(iust)=3
      ids(ichh)=4
      ids(icmm)=4
      ids(isrf)=5
      ids(itemp)=3
      ids(isphum)=6
      ids(iznlw)=2
      ids(imerw)=2
      ids(isnc)=3
      ids(istc)=4
      ids(isoilm)=4
      ids(isnod)=6
      ids(isnowd)=5
      ids(icnp)=5
      ids(ipcpr)=6
      ids(icpcpr)=6
      ids(irnof)=5                                                                                                                          

      ilpds=28
      if(icen2.eq.2) ilpds=45
      iens(1)=1
      iens(2)=ienst
      iens(3)=iensi
      iens(4)=1
      iens(5)=255
      iyr=idate(4)
      imo=idate(2)
      ida=idate(3)
      ihr=idate(1)
      ifhr=nint(zhour)
      ithr=nint(fhour)
      if(fhour.gt.zhour) then
        rtime=1./(3600.*(fhour-zhour))
      else
        rtime=0.
      endif
      if(secswr.gt.0.) then
        rtimsw=1./secswr
      else
        rtimsw=1.
      endif
      if(seclwr.gt.0.) then
        rtimlw=1./seclwr
      else
        rtimlw=1.
      endif
      rtimer=rtimsw
      rtimer(1)=rtimlw
      cl1=colat1         
cc
c..........................................................
      glolal=dusfc*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
!
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,izws,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(izws),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '01)zonal compt of momentum flux (n/m**2) land and sea surface '
      endif

      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=dvsfc*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,imws,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(imws),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '02)merid compt of momentum flux (n/m**2) land and sea surface '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=dtsfc*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ishflx,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ishflx),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '03)sensible heat flux (w/m**2) land and sea surface           '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=dqsfc*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ilhflx,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ilhflx),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '04)latent heat flux (w/m**2) land and sea surface             '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,tsea,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,itemp,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '05)temperature (k) land and sea surface                       '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal(:,:)=smc(1,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isoilm,i2dbls,0,10,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '06)volumetric soil moist content (frac) layer 10cm and 0cm    '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
c..........................................................
clu [-3l/+3l]: change 10-200cm to 10-40cm for smc(2)
      glolal(:,:)=smc(2,:,:)
clu   call uninterpred(1,kmsk,buffo,glolal,
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      if(lsoil.gt.2)then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
clu  &            1,isoilm,i2dbls,10,200,iyr,imo,ida,ihr,
     +            1,isoilm,i2dbls,10,40,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
clu  x '07)volumetric soil moist content (frac) layer 200cm and 10cm  '
     + '07)volumetric soil moist content (frac) layer 40cm and 10cm  '
      else
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isoilm,i2dbls,10,200,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '07)volumetric soil moist content (frac) layer 200cm and 10cm  '
      endif
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal(:,:)=stc(1,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,itemp,i2dbls,0,10,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '08)temp (k) layer betw two depth below land sfc 10cm and 0cm  '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
clu [-2l/+2l]: change 10-200 to 10-40 for stc(2)
      glolal(:,:)=stc(2,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      if(lsoil.gt.2)then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
clu  &            1,itemp,i2dbls,10,200,iyr,imo,ida,ihr,
     +            1,itemp,i2dbls,10,40,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
clu  x '09)temp (k) layer betw two depth below land sfc 200cm and 10cm'
     + '09)temp (k) layer betw two depth below land sfc 40cm and 10cm'
      else
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,itemp,i2dbls,10,200,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '09)temp (k) layer betw two depth below land sfc 200cm and 10cm'
      endif
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk,buffo,sheleg,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,isnowd,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isnowd),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '10)water equiv of accum snow depth (kg/m**2) land sea surface '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=dlwsfc*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,idlwf,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idlwf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '11)downward long wave radiation flux (w/m**2) land sea surface'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=ulwsfc*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iulwf,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iulwf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '12)upward long wave radiation flux (w/m**2) land sea surface  '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
c.......  fix fluxes for approx diurnal cycle
      do 113 k=1,4
       do j=1,lats_node_r
        do i=1,lonr
         glolal(i,j)=rflux(i,j,k)*rtimer(k)
        enddo
       enddo
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ipur(k),itlr(k),0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(k)),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0.and.k.eq.1)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '13)upward long wave radiation flux (w/m**2) top of atmosphere '
      if(ierr.ne.0.and.k.eq.2)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '14)upward solar radiation flux (w/m**2) top of atmosphere     '
      if(ierr.ne.0.and.k.eq.3)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '15)upward solar radiation flux (w/m**2) land and sea surface  '
      if(ierr.ne.0.and.k.eq.4)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '16)downward solar radiation flux (w/m**2) land and sea surface'
      endif
        if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
  113 continue
c..........................................................
!
!     for uv-b fluxes
!
      do j=1,lats_node_r
        do i=1,lonr
          glolal(i,j)=rflux(i,j,21)*rtimsw
        enddo
      enddo
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,129,icen,igen,
     &            0,iuvbf,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iuvbf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '17)uv-b downward solar flux (w/m**2) land sea surface'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
      do j=1,lats_node_r
        do i=1,lonr
          glolal(i,j)=rflux(i,j,22)*rtimsw
        enddo
      enddo
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,129,icen,igen,
     &            0,iuvbfc,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iuvbfc),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '18)clear sky uv-b downward solar flux (w/m**2) land sea surface'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
!
!     end uv-b fluxes
!
c..........................................................
c..........................................................
      do 813 k=5,7
cxxxxxxxxxxxxxxxx
       do j=1,lats_node_r
        do i=1,lonr
         glolal(i,j)=rflux(i,j,k)*100.*rtimsw      
        enddo
       enddo
      where(glolal.ge.0.5)
        kmskcv=1
      elsewhere
        kmskcv=0
      endwhere
!!
      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
        k4=4+(k-5)*4
        l=k4+1
        lbm=wrkga.ge.0.5_kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '19)total cloud cover (percent) high cloud layer               '
      if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '23)total cloud cover (percent) middle cloud layer             '
      if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '27)total cloud cover (percent) low cloud layer                '
      endif
        if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
cxxxxxxxxxxxxxxxx
       do j=1,lats_node_r
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j)=rflux(i,j,k+3)*1000./rflux(i,j,k)      
         else
          glolal(i,j)=0.
         endif
        enddo
       enddo
      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
        l=k4+2
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '20)pressure (pa) high cloud top level                         '
      if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '24)pressure (pa) middle cloud top level                       '
      if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '28)pressure (pa) low cloud top level                          '
      endif
        if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
cxxxxxxxxxxxxxxxx
       do j=1,lats_node_r
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j)=rflux(i,j,k+6)*1000./rflux(i,j,k)      
         else
          glolal(i,j)=0.
         endif
        enddo
       enddo
      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
        l=k4+3
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '21)pressure (pa) high cloud bottom level                      '
      if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '25)pressure (pa) middle cloud bottom level                    '
      if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '29)pressure (pa) low cloud bottom level                       '
      endif
        if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
cxxxxxxxxxxxxxxxx
       do j=1,lats_node_r
        do i=1,lonr
         if(rflux(i,j,k).gt.0.)then
          glolal(i,j)=rflux(i,j,k+9)/rflux(i,j,k)      
         else
          glolal(i,j)=0.
         endif
        enddo
       enddo
      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
        l=k4+4
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0.and.k.eq.5)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '22)temperature (k) high cloud top level                       '
      if(ierr.ne.0.and.k.eq.6)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '26)temperature (k) middle cloud top level                     '
      if(ierr.ne.0.and.k.eq.7)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '30)temperature (k) low cloud top level                        '
      endif
        if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c
  813 continue
cc
c...................................................................
      glolal=geshem*1.e3*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ipcpr,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipcpr),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '31)precipitation rate (kg/m**2/s) land and sea surface        '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      glolal=bengsh*1.e3*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,icpcpr,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icpcpr),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '32)convective precipitation rate (kg/m**2/s) land sea surface '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      glolal=gflux*rtime
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.ne.0._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,ighflx,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ighflx),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '33)ground heat flux (w/m**2) land and sea surface             '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      buffo=mod(slmskloc,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,islmsk,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(islmsk),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '34)land-sea mask (1=land; 0=sea) (integer) land sea surface   '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
!c-- xw: for sea-ice nov04
!     buffo=max(slmskloc-1._kind_io8,0._kind_io8)
!     call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
!     if(me.eq.ioproc) then
!     call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
!    &            0,icemsk,isfc,0,0,iyr,imo,ida,ihr,
!    &            ifhour,ithr,0,inst,0,0,icen2,ids(icemsk),iens,
!    &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
!     if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
!    x '35)ice concentration (ice=1; no ice=0) (1/0) land sea surface '
!     endif
      call uninterpred(2,kmsk0,buffo,fice,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,icemsk,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(icemsk),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '35)ice concentration (ice>0; no ice=0) (1/0) land sea surface '
      endif
!c-- xw: end sea-ice
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      call uninterpred(2,kmsk0,buffo,u10m,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iznlw,ielev,0,10,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iznlw),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '36)u wind (m/s) height above ground                           '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      call uninterpred(2,kmsk0,buffo,v10m,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,imerw,ielev,0,10,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(imerw),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '37)v wind (m/s) height above ground                           '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      call uninterpred(2,kmsk0,buffo,t2m,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,itemp,ielev,0,2,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '38)temperature (k) height above ground                        '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      call uninterpred(2,kmsk0,buffo,q2m,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,isphum,ielev,0,2,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isphum),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '39)specific humidity (kg/kg) height above ground              '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      glolal=psurf*1.e3
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iprs,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '40)pressure (pa) land and sea surface                         '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      call uninterpred(2,kmsk0,buffo,tmpmax,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,itmx,ielev,0,2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iwin,0,0,icen2,ids(itmx),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '41)maximum temperature (k) height above ground                '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      call uninterpred(2,kmsk0,buffo,tmpmin,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,itmn,ielev,0,2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iwin,0,0,icen2,ids(itmn),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '42)minimum temperature (k) height above ground                '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      glolal=runoff * 1.e3
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.ne.0._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,irnof,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iacc,0,0,icen2,ids(irnof),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '43)runoff (kg/m**2) land and sea surface                      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      glolal=ep * rtime
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.ne.0._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,iep,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iep),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '44)potential evaporation rate (w/m**/) land and sea surface   '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      glolal=cldwrk * rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,icldwk,icolmn,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldwk),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '45)cloud work function (j/kg) total atmospheric column        '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      glolal=dugwd*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,izgw,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(izgw),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '46)zonal gravity wave stress (n/m**2) land and sea surface    '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      glolal=dvgwd*rtime
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,imgw,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(imgw),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '47)meridional gravity wave stress (n/m**2) land sea surface   '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      call uninterpred(2,kmsk0,buffo,hpbl,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ihpbl,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ihpbl),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '48)boundary layer height '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
      call uninterpred(2,kmsk0,buffo,pwat,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ipwat,icolmn,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ipwat),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '49)precipitable water (kg/m**2) total atmospheric column      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c...................................................................
cxxxxxxxxxxxxxxxx
       do j=1,lats_node_r
        do i=1,lonr
         if (rflux(i,j,4).gt.0.) then
          glolal(i,j)=rflux(i,j,3)/rflux(i,j,4) * 100.
          if (glolal(i,j).gt.100.) glolal(i,j)=100.
         else
          glolal(i,j)=0.
         endif
        enddo
       enddo
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ialbdo,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ialbdo),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '50)albedo (percent) land and sea surface                      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
cxxxxxxxxxxxxxxxx
       do j=1,lats_node_r
        do i=1,lonr
         glolal(i,j)=rflux(i,j,26)*100.*rtimsw
        enddo
       enddo
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldf,icolmn,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '51)total cloud cover (percent) total atmospheric column       '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c
c convective clouds
c labeled instantaneous but actually averaged over fhswr hours
c
      glolal=cv*1.e2
      where(glolal.ge.0.5)
        kmskcv=1
      elsewhere
        kmskcv=0
      endwhere
      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=wrkga.ge.0.5_kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,icldf,icvlyr,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(icldf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '52)total cloud cover (percent) convective cloud layer         '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c.................................................
       do j=1,lats_node_r
        do i=1,lonr
        glolal(i,j) = 0.
        if(cv(i,j).gt.0.) then
!        itop=nint(cvt(i,j))
!        if(itop.ge.1.and.itop.le.levs)
!    &   glolal(i,j)=si(itop+1)*psurf(i,j)*1.e3
c...      cvt already a pressure (cb)...convert to pa
         glolal(i,j)=cvt(i,j)*1.e3
        end if
       enddo
      enddo
      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,iprs,icvtl,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '53)pressure (pa) convective cloud top level                   '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c.................................................
       do j=1,lats_node_r
        do i=1,lonr
        glolal(i,j) = 0.
        if(cv(i,j).gt.0.) then
!        ibot=nint(cvb(i,j))
!        if(ibot.ge.1.and.ibot.le.levs)
!    &   glolal(i,j)=si(ibot)*psurf(i,j)*1.e3
c...      cvb already a pressure (cb)...convert to pa
         glolal(i,j)=cvb(i,j)*1.e3
        end if
       enddo
      enddo
      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,iprs,icvbl,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '54)pressure (pa) convective cloud bottom level                '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c.................................................
c...   save b.l. cloud amount
cxxxxxxxxxxxxxxxx
       do j=1,lats_node_r
        do i=1,lonr
         glolal(i,j)=rflux(i,j,27)*100.*rtimsw
        enddo
       enddo
      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
cxxxxxxxxxxxxxxxx
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldf,ibllyr,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '55)total cloud cover (percent) boundary layer cloud layer     '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
!c-- xw: for sea-ice nov04
      call uninterpred(2,kmsk0,buffo,hice,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.2._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isik,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isik),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '56)sea ice thickness (m) category 1'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
!c-- xw: end sea-ice
c.................................................
clu: add smc(3:4), stc(3:4), slc(1:4), snwdph, canopy
clu: addition of 10 records starts here -------------------------------
      if(lsoil.gt.2)then
      glolal(:,:)=smc(3,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isoilm,i2dbls,40,100,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '57)volumetric soil moist content (frac) layer 100cm and 40cm '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal(:,:)=smc(4,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isoilm,i2dbls,100,200,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '58)volumetric soil moist content (frac) layer 200cm and 100cm '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal(:,:)=stc(3,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,itemp,i2dbls,40,100,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '59)temp (k) layer betw two depth below land sfc 100cm and 40cm'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal(:,:)=stc(4,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,itemp,i2dbls,100,200,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '60)temp (k) layer betw two depth below land sfc 200cm and 100cm'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
      endif
c..........................................................
      glolal(:,:)=slc(1,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &            1,islc,i2dbls,0,10,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '61)liquid soil moist content (frac) layer 10cm and 0cm  '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal(:,:)=slc(2,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      if(lsoil.gt.2)then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &            1,islc,i2dbls,10,40,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '62)liquid soil moist content (frac) layer 40cm and 10cm '
      else
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &            1,islc,i2dbls,10,200,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '62)liquid soil moist content (frac) layer 200cm and 10cm '
      endif
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      if(lsoil.gt.2)then
      glolal(:,:)=slc(3,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &            1,islc,i2dbls,40,100,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '63)liquid soil moist content (frac) layer 100cm and 40cm'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal(:,:)=slc(4,:,:)
      call uninterpred(2,kmsk,buffo,glolal,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &            1,islc,i2dbls,100,200,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '64)liquid soil moist content (frac) layer 200cm and 100cm'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
      endif
c..........................................................
      glolal=snwdph / 1.e3       !! convert from mm to m
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isnod,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isnod),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '65)snow depth (m) land surface                  '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk,buffo,canopy,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,icnp,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(icnp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '66)canopy water content (kg/m^2) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
clu: addition of 10 records ends here -------------------------------
c
cwei: addition of 30 records starts here -------------------------------
c..........................................................
      glolal=zorl / 1.e3       !! convert from mm to m
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,izorl,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(izorl),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '67)surface roughness (m)    '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=vfrac*100.
      call uninterpred(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,iveg,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iveg),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '68)vegetation fraction (fractional) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(1,kmsk,glolal,vtype,global_lats_r,lonsperlar)
      buffo=mod(glolal,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,ivtp,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ivtp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '69)vegetation type land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(1,kmsk,glolal,stype,global_lats_r,lonsperlar)
      buffo=mod(glolal,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,istp,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(istp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '70)soil type land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(1,kmsk,glolal,slope,global_lats_r,lonsperlar)
      buffo=mod(glolal,2._kind_io8)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,islo,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(islo),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '71)slope type land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,uustar,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iust,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iust),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '72)frictional velocity (m/s)'
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(1,kmsk0,buffo,oro,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ihgt,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ihgt),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '73)surface height (m)       '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(1,kmsk,buffo,srflag,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,irst,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(irst),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '74)freezing precip flag land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,chh,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ichh,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ichh),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '75)exchange coefficient ch(m/s)       '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,cmm,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &            0,icmm,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(icmm),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '76)exchange coefficient cm(m/s)      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk,buffo,epi,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,iep,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iep),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '77)potential evaporation rate (w/m**2) land and sea surface   '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,dlwsfci,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,idlwf,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(idlwf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '78)downward long wave radiation flux (w/m**2) '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,ulwsfci,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iulwf,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iulwf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '79)upward long wave radiation flux (w/m**2)  '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,uswsfci,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iuswf,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iuswf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '80)upward short wave radiation flux (w/m**2)  '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,dswsfci,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,idswf,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(idswf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '81)downward short wave radiation flux (w/m**2)  '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,dtsfci,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ishflx,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ishflx),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
        if(ierr.eq.0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '82)sensible heat flux (w/m**2) land and sea surface           '
        endif
      endif
c..........................................................
      call uninterpred(2,kmsk0,buffo,dqsfci,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,ilhflx,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ilhflx),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
        if(ierr.eq.0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '83)latent heat flux (w/m**2) land and sea surface             '
        endif
      endif
c..........................................................
      call uninterpred(2,kmsk,buffo,gfluxi,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
        lbm=slmskful.ne.0._kind_io8
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,ighflx,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ighflx),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
        if(ierr.eq.0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '84)ground heat flux (w/m**2) land and sea surface             '
        endif
      endif
c..........................................................
      glolal=srunoff * 1.e3
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isrf,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isrf),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '85)surface runoff (kg/m^2) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,tt1,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,itemp,isglev,1,1,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '86)lowest model level temp (k)       '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,q1,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,isphum,isglev,1,1,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(isphum),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '87)lowest model specific humidity (kg/kg)      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,u1,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iznlw,isglev,1,1,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iznlw),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '88)lowest model u wind (m/s)      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk0,buffo,v1,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,imerw,isglev,1,1,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(imerw),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '89)lowest model v wind (m/s)      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      call uninterpred(2,kmsk,buffo,zlvl,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,ihgt,isglev,1,1,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(ihgt),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '90)lowest model level height (m) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=evbsa*rtime
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,ievbs,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ievbs),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '91)direct evaporation from bare soil(w/m^2) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=evcwa*rtime
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,ievcw,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ievbs),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '92)canopy water evaporation(w/m^2) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=transa*rtime
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,itran,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(itran),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '93)transpiration (w/m^2) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=sbsnoa*rtime
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isbs,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(isbs),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '94)snow sublimation (w/m^2) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=snowca*rtime*100.
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,isnc,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(isnc),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '95)snow cover (fraction) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)
c..........................................................
      glolal=soilm*1.e3       !! convert from m to (mm)kg/m^2
      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me.eq.ioproc) then
      lbm=slmskful.eq.1._kind_io8
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            1,istc,i2dbls,0,200,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(istc),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '96)total column soil moisture (kg/m^2) land surface      '
      endif
      if(ierr.eq.0 .and. me.eq.ioproc) call wryte(noflx,lg,g)


cc
      if(me.eq.ioproc)
     &   print *,'grib flux file written ',fhour,idate,noflx  
!!
      return
      end
