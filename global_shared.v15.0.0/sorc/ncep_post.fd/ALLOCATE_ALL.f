!!!@PROCESS NOEXTCHK
      SUBROUTINE ALLOCATE_ALL()
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
! SUBPROGRAM:    MPI_FIRST   SET UP MESSGAE PASSING INFO
!   PRGRMMR: TUCCILLO        ORG: IBM
!
! ABSTRACT:
!     SETS UP MESSAGE PASSING INFO
!   .
!
! PROGRAM HISTORY LOG:
!   00-01-06  TUCCILLO - ORIGINAL
!   01-10-25  H CHUANG - MODIFIED TO PROCESS HYBRID MODEL OUTPUT
!   02-06-19  MIKE BALDWIN - WRF VERSION
!   11-12-16  SARAH LU - MODIFIED TO INITIALIZE AEROSOL FIELDS
!   12-01-07  SARAH LU - MODIFIED TO INITIALIZE AIR DENSITY/LAYER THICKNESS
!
! USAGE:    CALL MPI_FIRST
!   INPUT ARGUMENT LIST:
!
!   OUTPUT ARGUMENT LIST:
!
!   OUTPUT FILES:
!     STDOUT  - RUN TIME STANDARD OUT.
!
!   SUBPROGRAMS CALLED:
!       PARA_RANGE
!     UTILITIES:
!       NONE
!     LIBRARY:
!       COMMON - CTLBLK.comm
!
!   ATTRIBUTES:
!     LANGUAGE: FORTRAN
!     MACHINE : IBM RS/6000 SP
!$$$
!
      use vrbls4d
      use vrbls3d
      use vrbls2d
      use soil
      use masks
!
      !use params_mod
      use ctlblk_mod
!- - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - 
      implicit none
!
      include 'mpif.h'
!
      integer ierr,i,jsx,jex
! Allocate arrays
      allocate(u(im+1,jsta_2l:jend_2u,lm))
      allocate(v(im,jsta_2l:jvend_2u,lm))
      allocate(t(im,jsta_2l:jend_2u,lm))
! CHUANG ADD POTENTIAL TEMP BECAUSE WRF OUTPUT THETA
!      allocate(th(im,jsta_2l:jend_2u,lm))   
      allocate(q(im,jsta_2l:jend_2u,lm))
!      allocate(w(im,jsta_2l:jend_2u,lp1))
      allocate(uh(im,jsta_2l:jend_2u,lm))
      allocate(vh(im,jsta_2l:jend_2u,lm))
      allocate(wh(im,jsta_2l:jend_2u,lm))
      allocate(pmid(im,jsta_2l:jend_2u,lm))
      allocate(pmidv(im,jsta_2l:jend_2u,lm))
      allocate(pint(im,jsta_2l:jend_2u,lp1))
      allocate(alpint(im,jsta_2l:jend_2u,lp1))
      allocate(zmid(im,jsta_2l:jend_2u,lm))
      allocate(zint(im,jsta_2l:jend_2u,lp1))
!      allocate(rainw(im,jsta_2l:jend_2u,lm))
      allocate(q2(im,jsta_2l:jend_2u,lm))
      allocate(omga(im,jsta_2l:jend_2u,lm))
!     allocate(dpres(im,jsta_2l:jend_2u,lm))
      allocate(T_ADJ(im,jsta_2l:jend_2u,lm))
      allocate(ttnd(im,jsta_2l:jend_2u,lm))
      allocate(rswtt(im,jsta_2l:jend_2u,lm))
      allocate(rlwtt(im,jsta_2l:jend_2u,lm))
      allocate(exch_h(im,jsta_2l:jend_2u,lm)) 
      allocate(train(im,jsta_2l:jend_2u,lm))
      allocate(tcucn(im,jsta_2l:jend_2u,lm))
      allocate(EL_PBL(im,jsta_2l:jend_2u,lm))
!     MP FIELD   
      allocate(cwm(im,jsta_2l:jend_2u,lm))
      allocate(F_ice(im,jsta_2l:jend_2u,lm))
      allocate(F_rain(im,jsta_2l:jend_2u,lm))
      allocate(F_RimeF(im,jsta_2l:jend_2u,lm))
      allocate(QQW(im,jsta_2l:jend_2u,lm))
      allocate(QQI(im,jsta_2l:jend_2u,lm))
      allocate(QQR(im,jsta_2l:jend_2u,lm))
      allocate(QQS(im,jsta_2l:jend_2u,lm))
      allocate(QQG(im,jsta_2l:jend_2u,lm))
      allocate(QQNI(im,jsta_2l:jend_2u,lm))
      allocate(QQNR(im,jsta_2l:jend_2u,lm))
      allocate(QQNWFA(im,jsta_2l:jend_2u,lm))
      allocate(QQNIFA(im,jsta_2l:jend_2u,lm))
      allocate(EXTCOF55(im,jsta_2l:jend_2u,lm))
      allocate(CFR(im,jsta_2l:jend_2u,lm))
      allocate(DBZ(im,jsta_2l:jend_2u,lm))
      allocate(DBZR(im,jsta_2l:jend_2u,lm))
      allocate(DBZI(im,jsta_2l:jend_2u,lm))
      allocate(DBZC(im,jsta_2l:jend_2u,lm))
      allocate(mcvg(im,jsta_2l:jend_2u,lm))
      allocate(NLICE(im,jsta_2l:jend_2u,lm))
!     Wm Lewis: added 
      allocate(NRAIN(im,jsta_2l:jend_2u,lm))
      allocate(radius_cloud(im,jsta_2l:jend_2u,lm))
      allocate(radius_ice(im,jsta_2l:jend_2u,lm))
      allocate(radius_snow(im,jsta_2l:jend_2u,lm))
!GFS FIELD
      allocate(o3(im,jsta_2l:jend_2u,lm))
      allocate(o(im,jsta_2l:jend_2u,lm))
      allocate(o2(im,jsta_2l:jend_2u,lm))
      allocate(tcucns(im,jsta_2l:jend_2u,lm))
! Add GFS d3d fields
      if (me == 0) print *,' d3d_on=',d3d_on
      if (d3d_on) then
        allocate(vdifftt(im,jsta_2l:jend_2u,lm))
!       allocate(tcucns(im,jsta_2l:jend_2u,lm))
        allocate(vdiffmois(im,jsta_2l:jend_2u,lm))
        allocate(dconvmois(im,jsta_2l:jend_2u,lm))
        allocate(sconvmois(im,jsta_2l:jend_2u,lm))
        allocate(nradtt(im,jsta_2l:jend_2u,lm))
        allocate(o3vdiff(im,jsta_2l:jend_2u,lm))
        allocate(o3prod(im,jsta_2l:jend_2u,lm))
        allocate(o3tndy(im,jsta_2l:jend_2u,lm))
        allocate(mwpv(im,jsta_2l:jend_2u,lm))
        allocate(unknown(im,jsta_2l:jend_2u,lm))
        allocate(vdiffzacce(im,jsta_2l:jend_2u,lm))
        allocate(zgdrag(im,jsta_2l:jend_2u,lm))
        allocate(cnvctummixing(im,jsta_2l:jend_2u,lm))
        allocate(vdiffmacce(im,jsta_2l:jend_2u,lm))
        allocate(mgdrag(im,jsta_2l:jend_2u,lm))
        allocate(cnvctvmmixing(im,jsta_2l:jend_2u,lm))
        allocate(ncnvctcfrac(im,jsta_2l:jend_2u,lm))
        allocate(cnvctumflx(im,jsta_2l:jend_2u,lm))
        allocate(cnvctdmflx(im,jsta_2l:jend_2u,lm))
        allocate(cnvctdetmflx(im,jsta_2l:jend_2u,lm))
        allocate(cnvctzgdrag(im,jsta_2l:jend_2u,lm))
        allocate(cnvctmgdrag(im,jsta_2l:jend_2u,lm))
      endif
!
      allocate(htm(im,jsta_2l:jend_2u,lm))
      allocate(vtm(im,jsta_2l:jend_2u,lm))
! add GFIP ICING
      allocate(icing_gfip(im,jsta_2l:jend_2u,lm))        
      allocate(icing_gfis(im,jsta_2l:jend_2u,lm))        
!
!
!     FROM SOIL
!
      allocate(smc(im,jsta_2l:jend_2u,nsoil))
      allocate(stc(im,jsta_2l:jend_2u,nsoil))
      allocate(sh2o(im,jsta_2l:jend_2u,nsoil))
      allocate(SLDPTH(NSOIL))
      allocate(RTDPTH(NSOIL))
      allocate(SLLEVEL(NSOIL))
!
!     FROM VRBLS2D
!
! SRD
      allocate(wspd10max(im,jsta_2l:jend_2u))
      allocate(w_up_max(im,jsta_2l:jend_2u))
      allocate(w_dn_max(im,jsta_2l:jend_2u))
      allocate(w_mean(im,jsta_2l:jend_2u))
      allocate(refd_max(im,jsta_2l:jend_2u))
      allocate(prate_max(im,jsta_2l:jend_2u))
      allocate(fprate_max(im,jsta_2l:jend_2u))
      allocate(up_heli_max(im,jsta_2l:jend_2u))
      allocate(up_heli_max16(im,jsta_2l:jend_2u))
      allocate(grpl_max(im,jsta_2l:jend_2u))
      allocate(up_heli(im,jsta_2l:jend_2u))
      allocate(up_heli16(im,jsta_2l:jend_2u))
      allocate(ltg1_max(im,jsta_2l:jend_2u))
      allocate(ltg2_max(im,jsta_2l:jend_2u))
      allocate(ltg3_max(im,jsta_2l:jend_2u))
      allocate(nci_ltg(im,jsta_2l:jend_2u))
      allocate(nca_ltg(im,jsta_2l:jend_2u))
      allocate(nci_wq(im,jsta_2l:jend_2u))
      allocate(nca_wq(im,jsta_2l:jend_2u))
      allocate(nci_refd(im,jsta_2l:jend_2u))
      allocate(nca_refd(im,jsta_2l:jend_2u))
! SRD
! CRA
      allocate(REF_10CM(im,jsta_2l:jend_2u,lm))
      allocate(REFC_10CM(im,jsta_2l:jend_2u))
      allocate(REF1KM_10CM(im,jsta_2l:jend_2u))
      allocate(REF4KM_10CM(im,jsta_2l:jend_2u))
! CRA
      allocate(u10(im,jsta_2l:jend_2u))
      allocate(v10(im,jsta_2l:jend_2u))
      allocate(tshltr(im,jsta_2l:jend_2u))
      allocate(qshltr(im,jsta_2l:jend_2u))
      allocate(mrshltr(im,jsta_2l:jend_2u))
      allocate(smstav(im,jsta_2l:jend_2u))
      allocate(ssroff(im,jsta_2l:jend_2u))
      allocate(bgroff(im,jsta_2l:jend_2u))
      allocate(vegfrc(im,jsta_2l:jend_2u))
      allocate(acsnow(im,jsta_2l:jend_2u))
      allocate(acsnom(im,jsta_2l:jend_2u))
      allocate(cmc(im,jsta_2l:jend_2u))
      allocate(sst(im,jsta_2l:jend_2u))
      allocate(qz0(im,jsta_2l:jend_2u))
      allocate(thz0(im,jsta_2l:jend_2u))
      allocate(uz0(im,jsta_2l:jend_2u))
      allocate(vz0(im,jsta_2l:jend_2u))
      allocate(qs(im,jsta_2l:jend_2u))
      allocate(ths(im,jsta_2l:jend_2u))
      allocate(sno(im,jsta_2l:jend_2u))
      allocate(snonc(im,jsta_2l:jend_2u))
! Time-averaged fileds
      allocate(u10mean(im,jsta_2l:jend_2u))
      allocate(v10mean(im,jsta_2l:jend_2u))
      allocate(spduv10mean(im,jsta_2l:jend_2u))
      allocate(swradmean(im,jsta_2l:jend_2u))
      allocate(swnormmean(im,jsta_2l:jend_2u))
!NAMstart
      allocate(snoavg(im,jsta_2l:jend_2u))
      allocate(psfcavg(im,jsta_2l:jend_2u))
      allocate(t10m(im,jsta_2l:jend_2u))
      allocate(t10avg(im,jsta_2l:jend_2u))
      allocate(akmsavg(im,jsta_2l:jend_2u))
      allocate(akhsavg(im,jsta_2l:jend_2u))
      allocate(u10max(im,jsta_2l:jend_2u))
      allocate(v10max(im,jsta_2l:jend_2u))
      allocate(u10h(im,jsta_2l:jend_2u))
      allocate(v10h(im,jsta_2l:jend_2u))
!NAMend
      allocate(akms(im,jsta_2l:jend_2u))
      allocate(akhs(im,jsta_2l:jend_2u))
      allocate(cuprec(im,jsta_2l:jend_2u))
      allocate(acprec(im,jsta_2l:jend_2u))
      allocate(ancprc(im,jsta_2l:jend_2u))
      allocate(cuppt(im,jsta_2l:jend_2u))
! GSDstart
      allocate(rainc_bucket(im,jsta_2l:jend_2u))
      allocate(rainnc_bucket(im,jsta_2l:jend_2u))
      allocate(pcp_bucket(im,jsta_2l:jend_2u))
      allocate(snow_bucket(im,jsta_2l:jend_2u))
      allocate(graup_bucket(im,jsta_2l:jend_2u))
      allocate(qrmax(im,jsta_2l:jend_2u))
      allocate(tmax(im,jsta_2l:jend_2u))
      allocate(snownc(im,jsta_2l:jend_2u))
      allocate(graupelnc(im,jsta_2l:jend_2u))
      allocate(tsnow(im,jsta_2l:jend_2u))
      allocate(qvg(im,jsta_2l:jend_2u))
      allocate(qv2m(im,jsta_2l:jend_2u))
      allocate(qvl1(im,jsta_2l:jend_2u))
      allocate(snfden(im,jsta_2l:jend_2u))
      allocate(sndepac(im,jsta_2l:jend_2u))
! GSDend
      allocate(rswin(im,jsta_2l:jend_2u))
      allocate(swddni(im,jsta_2l:jend_2u))
      allocate(swddif(im,jsta_2l:jend_2u))
      allocate(rlwin(im,jsta_2l:jend_2u))
      allocate(rlwtoa(im,jsta_2l:jend_2u))
      allocate(tg(im,jsta_2l:jend_2u))
      allocate(sfcshx(im,jsta_2l:jend_2u))
      allocate(sfclhx(im,jsta_2l:jend_2u))
      allocate(fis(im,jsta_2l:jend_2u))
      allocate(t500(im,jsta_2l:jend_2u))
      allocate(cfracl(im,jsta_2l:jend_2u))
      allocate(cfracm(im,jsta_2l:jend_2u))
      allocate(cfrach(im,jsta_2l:jend_2u))
      allocate(acfrst(im,jsta_2l:jend_2u))
      allocate(acfrcv(im,jsta_2l:jend_2u))
      allocate(hbot(im,jsta_2l:jend_2u))
      allocate(htop(im,jsta_2l:jend_2u))
      allocate(aswin(im,jsta_2l:jend_2u))
      allocate(alwin(im,jsta_2l:jend_2u))
      allocate(aswout(im,jsta_2l:jend_2u))
      allocate(alwout(im,jsta_2l:jend_2u))
      allocate(aswtoa(im,jsta_2l:jend_2u))
      allocate(alwtoa(im,jsta_2l:jend_2u))
      allocate(czen(im,jsta_2l:jend_2u))
      allocate(czmean(im,jsta_2l:jend_2u))
      allocate(sigt4(im,jsta_2l:jend_2u))
      allocate(rswout(im,jsta_2l:jend_2u))
      allocate(radot(im,jsta_2l:jend_2u))
      allocate(ncfrst(im,jsta_2l:jend_2u))  ! real
      allocate(ncfrcv(im,jsta_2l:jend_2u))  ! real
      allocate(smstot(im,jsta_2l:jend_2u))
      allocate(pctsno(im,jsta_2l:jend_2u))
      allocate(pshltr(im,jsta_2l:jend_2u))
      allocate(th10(im,jsta_2l:jend_2u))
      allocate(q10(im,jsta_2l:jend_2u))
      allocate(sr(im,jsta_2l:jend_2u))
      allocate(prec(im,jsta_2l:jend_2u))
      allocate(subshx(im,jsta_2l:jend_2u))
      allocate(snopcx(im,jsta_2l:jend_2u))
      allocate(sfcuvx(im,jsta_2l:jend_2u))
      allocate(sfcevp(im,jsta_2l:jend_2u))
      allocate(potevp(im,jsta_2l:jend_2u))
      allocate(z0(im,jsta_2l:jend_2u))
      allocate(ustar(im,jsta_2l:jend_2u))
      allocate(pblh(im,jsta_2l:jend_2u))
      allocate(mixht(im,jsta_2l:jend_2u))
      allocate(twbs(im,jsta_2l:jend_2u))
      allocate(qwbs(im,jsta_2l:jend_2u))
      allocate(sfcexc(im,jsta_2l:jend_2u))
      allocate(grnflx(im,jsta_2l:jend_2u))
      allocate(soiltb(im,jsta_2l:jend_2u))
      allocate(z1000(im,jsta_2l:jend_2u))
      allocate(slp(im,jsta_2l:jend_2u))
      allocate(pslp(im,jsta_2l:jend_2u))
      allocate(f(im,jsta_2l:jend_2u))
      allocate(albedo(im,jsta_2l:jend_2u))
      allocate(albase(im,jsta_2l:jend_2u))
      allocate(cldfra(im,jsta_2l:jend_2u))
      allocate(cprate(im,jsta_2l:jend_2u))
      allocate(cnvcfr(im,jsta_2l:jend_2u))
      allocate(ivgtyp(im,jsta_2l:jend_2u))
      allocate(isltyp(im,jsta_2l:jend_2u))
      allocate(hbotd(im,jsta_2l:jend_2u))
      allocate(htopd(im,jsta_2l:jend_2u))
      allocate(hbots(im,jsta_2l:jend_2u))
      allocate(htops(im,jsta_2l:jend_2u))
      allocate(cldefi(im,jsta_2l:jend_2u))
      allocate(islope(im,jsta_2l:jend_2u))
      allocate(si(im,jsta_2l:jend_2u))
      allocate(lspa(im,jsta_2l:jend_2u))
      allocate(rswinc(im,jsta_2l:jend_2u))
      allocate(vis(im,jsta_2l:jend_2u))
      allocate(pd(im,jsta_2l:jend_2u))
      allocate(mxsnal(im,jsta_2l:jend_2u))
      allocate(epsr(im,jsta_2l:jend_2u))
! add GFS fields
      allocate(sfcux(im,jsta_2l:jend_2u))
      allocate(sfcvx(im,jsta_2l:jend_2u))
      allocate(avgalbedo(im,jsta_2l:jend_2u))
      allocate(avgcprate(im,jsta_2l:jend_2u))
      allocate(avgprec(im,jsta_2l:jend_2u))
      allocate(ptop(im,jsta_2l:jend_2u))
      allocate(pbot(im,jsta_2l:jend_2u))
      allocate(avgcfrach(im,jsta_2l:jend_2u))
      allocate(avgcfracm(im,jsta_2l:jend_2u))
      allocate(avgcfracl(im,jsta_2l:jend_2u))
      allocate(avgtcdc(im,jsta_2l:jend_2u))
      allocate(auvbin(im,jsta_2l:jend_2u))
      allocate(auvbinc(im,jsta_2l:jend_2u))
      allocate(ptopl(im,jsta_2l:jend_2u))
      allocate(pbotl(im,jsta_2l:jend_2u))
      allocate(Ttopl(im,jsta_2l:jend_2u))
      allocate(ptopm(im,jsta_2l:jend_2u))
      allocate(pbotm(im,jsta_2l:jend_2u))
      allocate(Ttopm(im,jsta_2l:jend_2u))
      allocate(ptoph(im,jsta_2l:jend_2u))
      allocate(pboth(im,jsta_2l:jend_2u))
      allocate(Ttoph(im,jsta_2l:jend_2u))
      allocate(sfcugs(im,jsta_2l:jend_2u))
      allocate(sfcvgs(im,jsta_2l:jend_2u))
      allocate(pblcfr(im,jsta_2l:jend_2u))
      allocate(cldwork(im,jsta_2l:jend_2u))
      allocate(gtaux(im,jsta_2l:jend_2u))
      allocate(gtauy(im,jsta_2l:jend_2u))
      allocate(mdltaux(im,jsta_2l:jend_2u))
      allocate(mdltauy(im,jsta_2l:jend_2u))
      allocate(runoff(im,jsta_2l:jend_2u))
      allocate(maxtshltr(im,jsta_2l:jend_2u))
      allocate(mintshltr(im,jsta_2l:jend_2u))
      allocate(maxrhshltr(im,jsta_2l:jend_2u))
      allocate(minrhshltr(im,jsta_2l:jend_2u))
      allocate(maxqshltr(im,jsta_2l:jend_2u))
      allocate(minqshltr(im,jsta_2l:jend_2u))
      allocate(dzice(im,jsta_2l:jend_2u))
      allocate(alwinc(im,jsta_2l:jend_2u))
      allocate(alwoutc(im,jsta_2l:jend_2u))
      allocate(alwtoac(im,jsta_2l:jend_2u))
      allocate(aswinc(im,jsta_2l:jend_2u))
      allocate(aswoutc(im,jsta_2l:jend_2u))
      allocate(aswtoac(im,jsta_2l:jend_2u))
      allocate(aswintoa(im,jsta_2l:jend_2u))
      allocate(smcwlt(im,jsta_2l:jend_2u))
      allocate(suntime(im,jsta_2l:jend_2u))
      allocate(fieldcapa(im,jsta_2l:jend_2u))
      allocate(avisbeamswin(im,jsta_2l:jend_2u))
      allocate(avisdiffswin(im,jsta_2l:jend_2u))
      allocate(airbeamswin(im,jsta_2l:jend_2u))
      allocate(airdiffswin(im,jsta_2l:jend_2u))
      allocate(snowfall(im,jsta_2l:jend_2u))
      allocate(acond(im,jsta_2l:jend_2u))
      allocate(edir(im,jsta_2l:jend_2u))
      allocate(ecan(im,jsta_2l:jend_2u))
      allocate(etrans(im,jsta_2l:jend_2u))
      allocate(esnow(im,jsta_2l:jend_2u))
      allocate(avgedir(im,jsta_2l:jend_2u))
      allocate(avgecan(im,jsta_2l:jend_2u))
      allocate(avgetrans(im,jsta_2l:jend_2u))
      allocate(avgesnow(im,jsta_2l:jend_2u))
      allocate(avgpotevp(im,jsta_2l:jend_2u))
!
!     FROM MASKS
!
      allocate(hbm2(im,jsta_2l:jend_2u))
      allocate(sm(im,jsta_2l:jend_2u))
      allocate(sice(im,jsta_2l:jend_2u))
      allocate(lmh(im,jsta_2l:jend_2u))  ! real
      allocate(lmv(im,jsta_2l:jend_2u))  ! real
      allocate(gdlat(im,jsta_2l:jend_2u))
      allocate(gdlon(im,jsta_2l:jend_2u))
      allocate(dx(im,jsta_2l:jend_2u))
      allocate(dy(im,jsta_2l:jend_2u))
      allocate(dpres(im,jsta_2l:jend_2u,lm))

      if (me == 0) print *,' gocart_on=',gocart_on
      if (gocart_on) then
!  
! Add GOCART fields
! vrbls4d
        allocate(dust(im,jsta_2l:jend_2u,lm,nbin_du))
        allocate(salt(im,jsta_2l:jend_2u,lm,nbin_ss))
        allocate(soot(im,jsta_2l:jend_2u,lm,nbin_bc))
        allocate(waso(im,jsta_2l:jend_2u,lm,nbin_oc))
        allocate(suso(im,jsta_2l:jend_2u,lm,nbin_su))
! vrbls3d
        allocate(ext(im,jsta_2l:jend_2u,lm))
        allocate(asy(im,jsta_2l:jend_2u,lm))
        allocate(ssa(im,jsta_2l:jend_2u,lm))
        allocate(duem(im,jsta_2l:jend_2u,nbin_du))
        allocate(dusd(im,jsta_2l:jend_2u,nbin_du))
        allocate(dudp(im,jsta_2l:jend_2u,nbin_du))
        allocate(duwt(im,jsta_2l:jend_2u,nbin_du))
        allocate(suem(im,jsta_2l:jend_2u,nbin_su))
        allocate(susd(im,jsta_2l:jend_2u,nbin_su))
        allocate(sudp(im,jsta_2l:jend_2u,nbin_su))
        allocate(suwt(im,jsta_2l:jend_2u,nbin_su))
        allocate(ocem(im,jsta_2l:jend_2u,nbin_oc))
        allocate(ocsd(im,jsta_2l:jend_2u,nbin_oc))
        allocate(ocdp(im,jsta_2l:jend_2u,nbin_oc))
        allocate(ocwt(im,jsta_2l:jend_2u,nbin_oc))
        allocate(bcem(im,jsta_2l:jend_2u,nbin_bc))
        allocate(bcsd(im,jsta_2l:jend_2u,nbin_bc))
        allocate(bcdp(im,jsta_2l:jend_2u,nbin_bc))
        allocate(bcwt(im,jsta_2l:jend_2u,nbin_bc))
        allocate(ssem(im,jsta_2l:jend_2u,nbin_ss))
        allocate(sssd(im,jsta_2l:jend_2u,nbin_ss))
        allocate(ssdp(im,jsta_2l:jend_2u,nbin_ss))
        allocate(sswt(im,jsta_2l:jend_2u,nbin_ss))
!       allocate(dpres(im,jsta_2l:jend_2u,lm))
        allocate(rhomid(im,jsta_2l:jend_2u,lm))
! vrbls2d
        allocate(dusmass(im,jsta_2l:jend_2u))
        allocate(ducmass(im,jsta_2l:jend_2u))
        allocate(dusmass25(im,jsta_2l:jend_2u))
        allocate(ducmass25(im,jsta_2l:jend_2u))
        allocate(susmass(im,jsta_2l:jend_2u))
        allocate(sucmass(im,jsta_2l:jend_2u))
        allocate(susmass25(im,jsta_2l:jend_2u))
        allocate(sucmass25(im,jsta_2l:jend_2u))
        allocate(ocsmass(im,jsta_2l:jend_2u))
        allocate(occmass(im,jsta_2l:jend_2u))
        allocate(ocsmass25(im,jsta_2l:jend_2u))
        allocate(occmass25(im,jsta_2l:jend_2u))
        allocate(bcsmass(im,jsta_2l:jend_2u))
        allocate(bccmass(im,jsta_2l:jend_2u))
        allocate(bcsmass25(im,jsta_2l:jend_2u))
        allocate(bccmass25(im,jsta_2l:jend_2u))
        allocate(sssmass(im,jsta_2l:jend_2u))
        allocate(sscmass(im,jsta_2l:jend_2u))
        allocate(sssmass25(im,jsta_2l:jend_2u))
        allocate(sscmass25(im,jsta_2l:jend_2u))
      endif
! 
      end
