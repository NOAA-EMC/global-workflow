      subroutine wrt3d_hyb(ioproc,no3d,zhour,fhour,idate,colat1,
     &                     global_lats_r,lonsperlar,pl_coeff,
     &                     secswr,seclwr,slmsk,psurf)
      use resol_def
      use layout1
      use sig_io
      use namelist_def
      use d3d_def
      implicit none
!!
      integer ioproc, pl_coeff
      integer idt3(6), idq3(5+pl_coeff), icc, idu3(4), idv3(4)
     &,       idq3a(7), idq3b(9), icmf(3)
      data    idt3/251, 250, 246, 242, 244, 241/
      data    idq3a/249, 243, 245, 173, 174, 175, 188/
      data    idq3b/249, 243, 245, 173, 174, 175, 188,139,239/
      data    idu3/247, 181, 183, 196/
      data    idv3/248, 182, 184, 197/
      data    icc/213/, icmf/202,209,219/
!
      save idt3, idq3a, idq3b, icc, idu3, idv3, icmf
!
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
     $          ifhour,ifday
      parameter(iprs=1,itemp=11,iznlw=33,imerw=34,isphum=51,ipwat=54,
     $          ipcpr=59,isnowd=65,icldf=71,iccldf=72,
     $          islmsk=81,izorl=83,ialbdo=84,isoilm=144,icemsk=91,
     $          ilhflx=121,ishflx=122,izws=124,imws=125,ighflx=155,
     $          iuswfc=160,idswfc=161,iulwfc=162,idlwfc=163,
     $          inswfc=164,inlwfc=165,
     $          idswvb=166,idswvd=167,idswnb=168,idswnd=169,
     $          itmx=15,itmn=16,irnof=90,iep=145,
     &          icldwk=146,izgw=147,imgw=148,ihpbl=221,
     $          idswf=204,idlwf=205,iuswf=211,iulwf=212,icpcpr=214)
      parameter(isfc=1,itoa=8,ielev=105,
     $          isglev=109,idbls=111,i2dbls=112,icolmn=200,
     $          iblbl=209,ibltl=210,ibllyr=211,
     $          ilcbl=212,ilctl=213,ilclyr=214,
     $          imcbl=222,imctl=223,imclyr=224,
     $          ihcbl=232,ihctl=233,ihclyr=234,
     $          icvbl=242,icvtl=243,icvlyr=244)
      parameter(inst=10,iwin=2,iavg=3,iacc=4)
      parameter(ifhour=1,ifday=2)
      real(kind=kind_io4) wrkga(lonr*latr)
!     real(kind=kind_io8) slmskful(lonr*latr)
      logical(1) lbm(lonr*latr)
      character g(200+lonr*latr*(16+1)/8)
      integer     idate(4), ids(255)
!     integer     idate(4), ids(255),iens(5)
      integer nv,il1,il2,j,i,k,l,no3d
      integer ilpds,iyr,imo,ida,ihr,ifhr,ithr,lg,ierr
      real (kind=kind_io8) rtime,rtimsw,rtimlw
      real (kind=kind_io8) colat1
      real (kind=kind_io8) cl1,secswr,zhour,fhour,seclwr
!
      real (kind=kind_io8) glolal(lonr,lats_node_r)
      real (kind=kind_io8) buffo(lonr,lats_node_r)
!!
      real (kind=kind_io8) psurf(lonr,lats_node_r)
      real (kind=kind_io8) slmsk(lonr,lats_node_r)
!
      integer              kmsk0(lonr*latr)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer lan,lat,lons_lat,gtn
!
      if (pl_coeff == 2) then
        idq3(1:7) = idq3a(1:7)
      else
        idq3(1:9) = idq3b(1:9)
      endif
!
      ids = 0
      g   = ' '
!
      kmsk0 = 0
!
      call idsdef(1,ids)
!                     for the ozone diagnostics
!                     -------------------------
      ids(139) = 13
      ids(174) = 13   ! ozone vertical diffusion rate - table 133
      ids(175) = 13   ! ozone production rate         - table 133
      ids(188) = 13   ! ozone total tendency          - table 133
      ids(239) = 15
!
      ids(173) = 10   ! large-scale moistening rate   - table 133

!                     for momentum diagnostics
!                     -------------------------
      ids(181) = 7
      ids(182) = 7
      ids(183) = 7
      ids(184) = 7
!
      ids(196) = 7
      ids(197) = 7

!
      ids(202) = 5    ! cumulus updraft massflux
      ids(209) = 5    ! cumulus downdraft massflux
      ids(219) = 5    ! cumulus detrainment massflux
!
      ilpds = 28
      if (icen2 == 2) ilpds = 45
      iens(1) = 1
      iens(2) = ienst
      iens(3) = iensi
      iens(4) = 1
      iens(5) = 255
      iyr     = idate(4)
      imo     = idate(2)
      ida     = idate(3)
      ihr     = idate(1)
      ifhr    = nint(zhour)
      ithr    = nint(fhour)
      if (fhour > zhour) then
        rtime = 1./(3600.*(fhour-zhour))
      else
        rtime = 0.
      endif
      if (secswr > 0.) then
        rtimsw = 1./secswr
      else
        rtimsw = 1.
      endif
      if (seclwr > 0.) then
        rtimlw = 1./seclwr
      else
        rtimlw = 1.
      endif
      cl1 = colat1
!..........................................................
!     temperature tendencies
!
      do nv=1,6
       do k=1,levs
        il1 = k
        il2 = il1
        glolal = 0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         do i=1,lons_lat
           glolal(i,lan) = dt3dt(i,k,nv,lan) * rtime
         enddo
        enddo
        call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me == ioproc) then
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &           0,idt3(nv),isglev,il1,il2,iyr,imo,ida,ihr,
     &           ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idt3(nv)),iens,
     &           0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' k=',k
!    x  '01)temperature tendency (k/s) '
        endif
        if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
       enddo
      enddo
!
!     moisture and ozone tendencies
!
      do nv=1,5+pl_coeff
       do k=1,levs
         il1 = k
         il2 = il1
        glolal = 0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         do i=1,lons_lat
             glolal(i,lan) = dq3dt(i,k,nv,lan) * rtime
         enddo
        enddo
        call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me == ioproc) then
         gtn = 2
         if (nv > 3) gtn = 133
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,gtn,icen,igen,
     &            0,idq3(nv),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idq3(nv)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' k=',k
!    x   '01)moisture tendency (g/kg/s) '
        endif
        if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
       enddo
      enddo
!
!     zonal velocity (u)  tendencies
!
      do nv=1,4
       do k=1,levs
        il1 = k
        il2 = il1
        glolal = 0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         do i=1,lons_lat
           glolal(i,lan) = du3dt(i,k,nv,lan) * rtime
         enddo
        enddo
        call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
 
        if(me == ioproc) then
         gtn = 2
         if (nv > 1) gtn = 133
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,gtn,icen,igen,
     &            0,idu3(nv),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idu3(nv)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' k=',k
!    x '01)zonal compt of momentum tendency (m/s**2) '
        endif
        if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
       enddo
      enddo
!
!     meridional velocity (v)  tendencies
!
      do nv=1,4
       do k=1,levs
        il1 = k
        il2 = il1
        glolal = 0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         do i=1,lons_lat
           glolal(i,lan) = dv3dt(i,k,nv,lan) * rtime
         enddo
        enddo
        call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me == ioproc) then
         gtn = 2
         if (nv > 1) gtn = 133
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,gtn,icen,igen,
     &            0,idv3(nv),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idv3(nv)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' nv=',nv
     &,  ' k=',k
!    x   '01)meridional compt of momentum tendency (m/s**2) '
        endif
        if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
       enddo
      enddo
!
!     cloud cover
!
      do k=1,levs
        il1 = k
        il2 = il1
         do j=1,lats_node_r
           do i=1,lonr
            glolal(i,j) = cldcov(i,k,j) * (rtimsw * 100.0)
          enddo
        enddo
        call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me == ioproc) then
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,icc,isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icc),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' k=',k
!    x  '01)zonal compt of momentum flux (n/m**2) land and sea surface '
        endif
 
        if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
      enddo
!
!     surface pressure
!
      glolal = psurf
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if(me == ioproc)then
      call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &            0,iprs,isfc,0,0,iyr,imo,ida,ihr,
     &            ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
      if(ierr.ne.0)print*,'wrtsfc gribit ierr=',ierr,'  ',
     x '38)pressure (pa) land and sea surface                         '
      endif
      if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
!
      if (ras) then
!
!     convective updraft massflux
!
       do k=1,levs
        il1 = k
        il2 = il1
        glolal = 0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         do i=1,lons_lat
           glolal(i,lan) = upd_mf(i,k,lan)* rtime
         enddo
        enddo
      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &            0,icmf(1),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icmf(1)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
        if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' k=',k
!    x '01)convective updraft massflux (kg/m**2) '
        endif

        if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
      enddo
!
!     convective downdraft massflux
!
       do k=1,levs
        il1 = k
        il2 = il1
        glolal = 0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         do i=1,lons_lat
            glolal(i,lan) = dwn_mf(i,k,lan)* rtime
         enddo
        enddo
        call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &            0,icmf(2),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icmf(2)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' k=',k
!    x   '01)convective downdraft massflux (kg/m**2) '
        endif

        if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
      enddo
!
!     convective detrainment massflux
!
       do k=1,levs
        il1 = k
        il2 = il1
        glolal = 0.
        do lan=1,lats_node_r
         lat = global_lats_r(ipt_lats_node_r-1+lan)
         lons_lat = lonsperlar(lat)
         do i=1,lons_lat
            glolal(i,lan) = det_mf(i,k,lan)* rtime
         enddo
        enddo
        call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
        if(me == ioproc) then
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &            0,icmf(3),isglev,il1,il2,iyr,imo,ida,ihr,
     &            ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icmf(3)),iens,
     &            0.,0.,0.,0.,0.,0.,g,lg,ierr)
         if(ierr.ne.0)print*,'wrt3d gribit ierr=',ierr,'  ',' k=',k
!    x   '01)convective detrainment massflux (kg/m**2) '
        endif

        if(ierr == 0 .and. me == ioproc) call wryte(no3d,lg,g)
      enddo
      endif
!
      return
      end
