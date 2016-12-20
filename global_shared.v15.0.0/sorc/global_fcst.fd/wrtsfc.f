!-----------------------------------
      subroutine wrtsfc                                                 &
!...................................
!  ---  inputs:
     &     ( ioproc,noflx,zhour,fhour,idate,colat1,secswr,seclwr,       &
     &       sfc_fld,flx_fld,fluxr,global_lats_r,lonsperlar)
!  ---  outputs: ( none )
! =================   subprogram documentation block   ================ !
!                                                                       !
!    this program writes out surface flux file in grib form.            !
!                                                                       !
!    usage:        call wrtsfc                                          !
!                                                                       !
!    subprograms called:                                                !
!                  uninterpred, unsplit2d, idsdef, gribit, wryte        !
!                                                                       !
!    attributes:                                                        !
!      language:   fortran 90                                           !
!      machine:    ibm-sp, sgi                                          !
!                                                                       !
!    external modules referenced:                                       !
!      'module machine'              in 'machine.f'                     !
!      'module resol_def             in 'resol_def.f'                   !
!      'module layout1               in 'layout1.f'                     !
!      'module sig_io                in 'sig_io.f'                      !
!      'module namelist_def          in 'namelist_def.f'                !
!      'module sfc_flx_esmfmod       in 'sfc_var_esmfmod.f'             !
!      'module funcphys'             in 'funcphys_v.f'                  !
!                                                                       !
!    program history log:                                               !
!      mmm-yyyy  ncep (?)      - created program wrtsfc                 !
!                sarah lu      - added smc(3:4), stc(3:4), slc(1:4),    !
!                  snwdph, canopy, changed 10-200cm to 10-40cm for      !
!                  smc(2), and changed 10-200 to 10-40 for stc(2).      !
!                jun wang      - modified to add spfhmax/spfhmin.       !
!      nov 2004  xingren wu    - modified to add sea-ice fields.        !
!      oct 2006  helin wei     - modified to add 30 records for land mdl!
!                  monitoring and analysis purpose.                     !
!      nov 2007  ho-chun huang - code change for gocart, added lggfs3d  !
!                  option.                                              !
!       -   --   s. moorthi   - a multi-year continuation to improve    !
!                  the support of requests for adding output fields and !
!                  model upgrades, including changing fields, logical   !
!                  controls, grib data conversions, and incorporating   !
!                  new developments, etc.                               !
!       -  2009  sarah lu      - added 7 clear-sky radiation racords.   !
!      apr 2012  yu-tai hou    - added 4 sw sfc flux components vis/nir !
!                  beam/diffused. the written out fields have been      !
!                  re-arranged that related fields are closer together, !
!                  and reduced duplications of field labels.            !
!      jan 2013  s. moorthi    - modified scale factor for spfhmax and  !
!                  spfhmin fields with additional variable ids_iq to    !
!                  avoid conflict with fields dnwd sw/lw (idswf/idlwf). !
!      mar 2013  yu-tai hou    - modified scale factor for sfc-uvb and  !
!                  sfc-csuvb fields with additional variable ids_uvb to !
!                  avoid conflict with field canopy water evap (ievcw). !
!      june 2013 h chuang - add frozen precipitation fraction to flux   !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!    input variables:                                            size   !
!      ioproc        : integer, processor id num                   1    !
!      noflx         : integer,                                    1    !
!      zhour         : real, accumulator reset time in hr.         1    !
!      fhour         : real, forecast time in hr.                  1    !
!      idate         : integer, init cond time (h,m,d,y)           4    !
!      colat1        : real, colatitude                            1    !
!      secswr        : real, sw radiation call duration in sec     1    !
!      seclwr        : real, lw radiation call duration in sec     1    !
!      sfc_fld       : sfc_var_data type, surface related fields        !
!      flx_fld       : flx_var_data type, rediation related fields      !
!      fluxr         : real, time accum radiation related fields        !
!                                                (lonr,nfxr,lats_node_r)!
!      global_lats_r : integer, index for global latitudes        latr  !
!      lonsperlar    : integer, num of pts on a given lat circle  latr  !
!                                                                       !
!  ======================  end of definations  =======================  !
!
      use machine,         only : kind_io4, kind_io8
      use resol_def,       only : lonr, latr, levp1, lsoil, nfxr
      use layout1,         only : me, lats_node_r
      use sig_io,          only : icen, icen2, ienst, iensi, iens
      use namelist_def
      use sfc_flx_esmfmod, only : sfc_var_data, flx_var_data
!
      implicit none
!
!  ---  inputs:
      integer, intent(in) :: ioproc, noflx, idate(4),                   &
     &       global_lats_r(latr), lonsperlar(latr)

      real (kind=kind_io8), intent(in) :: zhour, fhour, colat1,         &
     &                                    secswr, seclwr
      real (kind=kind_io8), intent(in) :: fluxr(lonr,nfxr,lats_node_r)

      type (sfc_var_data), intent(in) :: sfc_fld
      type (flx_var_data), intent(in) :: flx_fld

!  ---  outputs: (none)

!  ---  parameters:
      integer, parameter :: nfld = 29

      integer, parameter ::        iprs  =  1, ihgt  =  7, itemp = 11,  &
     &     itmx  = 15, itmn  = 16, iznlw = 33, imerw = 34, isphum= 51,  &
     &     ipwat = 54, ipcpr = 59, isrweq= 64, isnowd= 65, isnod = 66,  &
     &     icldf = 71, iccldf= 72, islmsk= 81, izorl = 83, ialbdo= 84,  &
     &     istc  = 86, iveg  = 87, irnof = 90, icemsk= 91, isik  = 92,  &
     &     ilhflx=121, ishflx=122, izws  =124, imws  =125, irst  =140,  &
     &     isoilm=144, iep   =145, icldwk=146, izgw  =147, imgw  =148,  &
     &     ighflx=155, icsusw=160, icsdsw=161, icsulw=162, icsdlw=163,  &
     &     iuswfc=160, idswfc=161, iulwfc=162, idlwfc=163, inswfc=164,  &
     &     inlwfc=165, idswvb=166, idswvd=167, idswnb=168, idswnd=169,  &
     &     icmm  =179, isuntm=191, isr   =194, isbs  =198, ievbs =199,  &
     &     ievcw =200, iuvbf =200, iuvbfc=201, idswf =204, idlwf =205,  &
     &     iqmx  =204, iqmn  =205, ichh  =208, itran =210, iuswf =211,  &
     &     iulwf =212, icpcpr=214, ismcwlt=219,ismcref=220,ihpbl =221,  &
     &     islo  =222, icnp  =223, istp  =224, ivtp  =225, isnohf=229,  &
     &     isrf  =235,  isnc  =238, iust  =253 

      integer, parameter ::        isfc  =  1, itoa  =  8, ielev =105,  &
     &     isglev=109, idbls =111, i2dbls=112, islc  =160, icolmn=200,  &
     &     iblbl =209, ibltl =210, ibllyr=211, ilcbl =212, ilctl =213,  &
     &     ilclyr=214, imcbl =222, imctl =223, imclyr=224, ihcbl =232,  &
     &     ihctl =233, ihclyr=234, icvbl =242, icvtl =243, icvlyr=244

      integer, parameter ::        ifhour=  1, ifday =  2, inst  = 10,  &
     &     iwin  =  2, iavg  =  3, iacc  =  4

!  ---  local variables:
      integer :: i, j, k, k4, l
      integer :: ilpds, iyr, imo, ida, ihr, ifhr, ithr, lg, ierr
      integer :: ids(255), ids_iq, ids_uvb
      integer, dimension(lonr,lats_node_r) :: kmsk, kmsk0, kmskcv

      real (kind=kind_io8) :: rtimer(nfld), rtime, rtimsw, rtimlw
      real (kind=kind_io8) :: cl1, si(levp1)

      real (kind=kind_io4), dimension(lonr*latr) :: buff1l, wrkga

      real (kind=kind_io8), dimension(lonr*latr) :: slmskful
      real (kind=kind_io8), dimension(lonr,lats_node_r) :: slmskloc,    &
     &       glolal, buffo
      real (kind=kind_io8) :: rflux(lonr,lats_node_r,nfxr)

      logical(1) :: lbm(lonr*latr)
      character  :: g(200+lonr*latr*(16+1)/8)

!  ---  label indexes:
      integer, dimension(nfld) :: ipur, itlr
      data  ipur / iulwf , iuswf , iuswf , idswf , icldf , iprs  ,      &
     &             iprs  , itemp , icldf , iprs  , iprs  , itemp ,      &
     &             icldf , iprs  , iprs  , itemp , iuvbf , iuvbfc,      &
     &             idswf , icsulw, icsusw, icsdlw, icsusw, icsdsw,      &
     &             icsulw, idswvb, idswvd, idswnb, idswnd /

      data  itlr / itoa  , itoa  , isfc  , isfc  , ihclyr, ihctl ,      &
     &             ihcbl , ihctl , imclyr, imctl , imcbl , imctl ,      &
     &             ilclyr, ilctl , ilcbl , ilctl , isfc  , isfc  ,      &
     &             itoa  , itoa  , itoa  , isfc  , isfc  , isfc  ,      &
     &             isfc  , isfc  , isfc  , isfc  , isfc   /
!
!===>  begin here
!
      g=' '

      kmsk  = nint(sfc_fld%slmsk)
      kmsk0 = 0

      call uninterpred(1,kmsk,glolal,sfc_fld%slmsk,
     &                 global_lats_r,lonsperlar)

      slmskloc = glolal
      call unsplit2d(ioproc,buff1l,glolal,global_lats_r)
      slmskful = buff1l

      do k = 1, nfxr
        do j = 1, lats_node_r
          do i = 1, lonr
            rflux(i,j,k) = fluxr(i,k,j)
          enddo
        enddo
      enddo

!  ---  set defalt decimal scaling factor array
      ids = 0
      ids_iq      = 5      ! used for iqmx/iqmn due to conflict with idswf/idlwf
      ids_uvb     = 2      ! used for iuvbf/iuvbfc due to conflict with ievcw
      call idsdef(1,ids)

!  ---  make adjustment if diff from the defaults
      ids(ihgt)   = 3      ! (007) geopotential height            (def=0)
      ids(itemp)  = 3      ! (011) temperature                    (def=1)
      ids(iznlw)  = 2      ! (033) zonal wind                     (def=1)
      ids(imerw)  = 2      ! (034) meridional wind                (def=1)
      ids(isphum) = 6      ! (051) specific humidity              (def=4)
      ids(ipcpr)  = 6      ! (059) precipitation rate             (def=6)
      ids(isnowd) = 5      ! (065) water equivalent of snow depth (def=0)
      ids(isnod)  = 6      ! (066) snow depth/mixed-layer depth   (def=2)
      ids(izorl)  = 4      ! (083) roughness                      (def=5)
      ids(istc)   = 4      ! (086) soil wetness                   (def=0)
      ids(iveg)   = 2      ! (087) vegetation/salinity            (def=0)
      ids(irnof)  = 5      ! (090) runoff                         (def=1)
      ids(icemsk) = 3      ! (091) ice concentration              (def=0)
      ids(isik)   = 2      ! (092) ice thickness                  (def not set)
      ids(isoilm) = 4      ! (144) vol soil moisture content      (def=3)
      ids(icmm)   = 4      ! (179) exchange coefficient           (def not set) *table 130
      ids(ievcw)  = 2      ! (200) canopy water evaporation       (def not set) *table 2
!     ids(iuvbf)  = 0      ! (200) sfc dnwd uv-b flux tot sky     (def not set) *table 129
!     ids(iuvbfc) = 0      ! (201) sfc dnwd uv-b flux clr sky     (def not set) *table 129
!     ids(iqmx)   = 5      ! (204) max specific humidity          (def=0) *** table 133
!     ids(iqmn)   = 5      ! (205) min specific humidity          (def=0) *** table 133
      ids(ichh)   = 4      ! (208) exchange coefficient           (def not set)
      ids(icpcpr) = 6      ! (214) convective precipitation rate  (def=6) ***
      ids(ismcwlt)= 4      ! (219) wilting point                  (def=1) *** table 130
      ids(ismcref)= 4      ! (220) field capacity                 (def=4) *** table 130
      ids(icnp)   = 5      ! (223) plant canopy surface water     (def=1)
      ids(isrf)   = 5      ! (235) storm surface runoff           (def=1)
      ids(isnc)   = 3      ! (238) snow cover                     (def=0)
      ids(iust)   = 3      ! (253) friction velocity              (def not set)
!
      ilpds   = 28
      if (icen2 == 2) ilpds=45

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
      rtimer    = rtimsw
      rtimer( 1)= rtimlw
      rtimer(20)= rtimlw       ! csulf_toa
      rtimer(22)= rtimlw       ! csdlf_sfc
      rtimer(25)= rtimlw       ! csulf_sfc
      cl1       = colat1         

      ierr = 0
!..................................................................
!  - zonal component of momentum flux:
!
      glolal = flx_fld%dusfc*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,izws,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(izws),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  01) ',
     &     'zonal compt of momentum flux (n/m**2) land and sea surface'
        endif
      endif
!..................................................................
!  - meridional component of momentum flux:
!
      glolal = flx_fld%dvsfc*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,imws,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(imws),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  02) ',
     &     'merid compt of momentum flux (n/m**2) land and sea surface'
        endif
      endif
!..................................................................
!  - sensible heat flux:
!
      glolal = flx_fld%dtsfc*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ishflx,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ishflx),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  03) ',
     &     'sensible heat flux (w/m**2) land and sea surface'
        endif
      endif
!..................................................................
!  - latent heat flux:
!
      glolal = flx_fld%dqsfc*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ilhflx,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ilhflx),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  04) ',
     &     'latent heat flux (w/m**2) land and sea surface'
        endif
      endif
!..................................................................
!  - surface temperature:
!
      call uninterpred(2,kmsk0,buffo,sfc_fld%tsea,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itemp,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  05) ',
     &     'temperature (k) land and sea surface'
        endif
      endif
!..................................................................
!  - volumetric soil moist content at layer 10cm and 0cm:
!
      glolal(:,:) = sfc_fld%smc(:,1,:)

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isoilm,i2dbls,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  06) ',
     &     'volumetric soil moist content (frac) layer 10cm and 0cm'
        endif
      endif
!..................................................................
!  - volumetric soil moist content at layer 40cm and 10cm:
!
      glolal(:,:) = sfc_fld%smc(:,2,:)

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        if (lsoil > 2) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,isoilm,i2dbls,10,40,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  07) ',
     &       'volumetric soil moist content (frac) layer 40cm and 10cm'
          endif
        else
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,isoilm,i2dbls,10,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  07) ',
     &       'volumetric soil moist content (frac) layer 200cm and 10cm'
          endif
        endif   ! end if_lsoil>2
      endif
!..................................................................
!  - temperature at layer 10cm and 0cm:
!
      glolal(:,:) = sfc_fld%stc(:,1,:)

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,itemp,i2dbls,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  08) ',
     &    'temp (k) layer betw two depth below land sfc 10cm and 0cm'
        endif
      endif
!..................................................................
!  - temperature at layer 40cm and 10cm:
!
      glolal(:,:) = sfc_fld%stc(:,2,:)

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)

        if (lsoil > 2) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,itemp,i2dbls,10,40,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  09) ',
     &       'temp (k) layer betw 2 depth below land sfc 40cm and 10cm'
          endif
        else
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,itemp,i2dbls,10,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  09) ',
     &       'temp (k) layer betw 2 depth below land sfc 200cm and 10cm'
          endif
        endif    ! end if_lsoil>2
      endif
!..................................................................
!  - water equivalent of accummulated snow depth:
!
      call uninterpred(2,kmsk,buffo,sfc_fld%sheleg,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,isnowd,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isnowd),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  10) ',
     &     'water equiv of accum snow depth (kg/m**2) at surface'
        endif
      endif
!..................................................................
!  - total sky radiation fluxes at toa and surface:
!
      do k = 1, 4
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = rflux(i,j,k)*rtimer(k)
          enddo
        enddo

        call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ipur(k),itlr(k),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(k)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 1) print*,'wrtsfc gribit ierr=',ierr,'  11) ',
     &       'upward long wave radiation flux (w/m**2) at toa'
            if (k == 2) print*,'wrtsfc gribit ierr=',ierr,'  12) ',
     &       'upward solar radiation flux (w/m**2) at toa'
            if (k == 3) print*,'wrtsfc gribit ierr=',ierr,'  13) ',
     &       'upward solar radiation flux (w/m**2) at surface'
            if (k == 4) print*,'wrtsfc gribit ierr=',ierr,'  14) ',
     &       'downward solar radiation flux (w/m**2) at surface'
          endif
        endif
      enddo
!..................................................................
!  - for high, mid, low cloud (cover, pressure, temperature)
!
      lab_do_cloud : do k = 5, 7    ! (high, mid, low clouds)
        k4 = 4 + (k-5)*4

!  - cloud cover (h,m,l):
!
        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = rflux(i,j,k)*100.*rtimsw
          enddo
        enddo
        where (glolal >= 0.5)
          kmskcv = 1
        elsewhere
          kmskcv = 0
        endwhere

        call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          l = k4 + 1
          lbm = (wrkga >= 0.5_kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 5) print*,'wrtsfc gribit ierr=',ierr,'  15) ',
     &       'total cloud cover (percent) high cloud layer'
            if (k == 6) print*,'wrtsfc gribit ierr=',ierr,'  19) ',
     &       'total cloud cover (percent) middle cloud layer'
            if (k == 7) print*,'wrtsfc gribit ierr=',ierr,'  23) ',
     &       'total cloud cover (percent) low cloud layer'
          endif
        endif

!  - pressure at cloud top:
!
        do j = 1, lats_node_r
          do i = 1, lonr
            if (rflux(i,j,k) > 0.) then
              glolal(i,j) = rflux(i,j,k+3)*1000./rflux(i,j,k)
            else
              glolal(i,j) = 0.
            endif
          enddo
        enddo

        call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          l = k4 + 2
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 5) print*,'wrtsfc gribit ierr=',ierr,'  16) ',
     &       'pressure (pa) high cloud top level'
            if (k == 6) print*,'wrtsfc gribit ierr=',ierr,'  20) ',
     &       'pressure (pa) middle cloud top level'
            if (k == 7) print*,'wrtsfc gribit ierr=',ierr,'  24) ',
     &       'pressure (pa) low cloud top level'
          endif
        endif

!  - pressure at cloud base:
!
        do j = 1, lats_node_r
          do i = 1, lonr
            if (rflux(i,j,k) > 0.) then
              glolal(i,j) = rflux(i,j,k+6)*1000./rflux(i,j,k)
            else
              glolal(i,j) = 0.
            endif
          enddo
        enddo

        call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          l = k4 + 3
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 5) print*,'wrtsfc gribit ierr=',ierr,'  17) ',
     &       'pressure (pa) high cloud bottom level'
            if (k == 6) print*,'wrtsfc gribit ierr=',ierr,'  21) ',
     &       'pressure (pa) middle cloud bottom level'
            if (k == 7) print*,'wrtsfc gribit ierr=',ierr,'  25) ',
     &       'pressure (pa) low cloud bottom level'
          endif
        endif

!  - temperature at cloud top:
!
        do j = 1, lats_node_r
          do i = 1, lonr
            if (rflux(i,j,k) > 0.) then
              glolal(i,j) = rflux(i,j,k+9)/rflux(i,j,k)
            else
              glolal(i,j) = 0.
            endif
          enddo
        enddo

        call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          l = k4 + 4
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,ipur(l),itlr(l),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(l)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (k == 5) print*,'wrtsfc gribit ierr=',ierr,'  18) ',
     &       'temperature (k) high cloud top level'
            if (k == 6) print*,'wrtsfc gribit ierr=',ierr,'  22) ',
     &       'temperature (k) middle cloud top level'
            if (k == 7) print*,'wrtsfc gribit ierr=',ierr,'  26) ',
     &       'temperature (k) low cloud top level'
          endif
        endif

      enddo  lab_do_cloud
!
!..................................................................
!  - total cloud amount:
!
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,17)*100.*rtimsw
        enddo
      enddo

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldf,icolmn,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  27) ',
     &     'total cloud cover (percent) total atmospheric column'
        endif
      endif
!..................................................................
!  - boundary layer cloud amount:
!
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,18)*100.*rtimsw
        enddo
      enddo

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldf,ibllyr,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  28) ',
     &     'total cloud cover (percent) boundary layer cloud layer'
        endif
      endif
!..................................................................
!  - surface downeard lw fluxes: (use the surface temp adjusted quantities
!    to replace the original on in rec 19 of fluxr)
!
      glolal = flx_fld%dlwsfc*rtimlw

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,idlwf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(idlwf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  29) ',
     &     'downward long wave radiation flux (w/m**2) at surface'
        endif
      endif
!..................................................................
!  - surface upward lw fluxes: (use the one recalc'ed from surface temp
!    to replace the original one in rec 20 of fluxr)
!
      glolal = flx_fld%ulwsfc*rtimlw

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iulwf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iulwf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  30) ',
     &     'upward long wave radiation flux (w/m**2) at surface'
        endif
      endif
!..................................................................
!  - uv-b flux at surface for total sky:
!
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,21)*rtimsw
        enddo
      enddo

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,129,icen,igen,
     &              0,iuvbf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids_uvb,iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  31) ',
     &     'uv-b downward solar flux (w/m**2) at surface'
        endif
      endif
!..................................................................
!  - uv-b flux at surface for clear sky:
!
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,22)*rtimsw
        enddo
      enddo

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,129,icen,igen,
     &              0,iuvbfc,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids_uvb,iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  32) ',
     &     'clear sky uv-b downward solar flux (w/m**2) at surface'
        endif
      endif
!..................................................................
!  - incoming solar radiation at toa:
!
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = rflux(i,j,23)*rtimsw
        enddo
      enddo

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ipur(19),itlr(19),0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(19)),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  33) ',
     &     'downward solar radiation flux (w/m**2) at toa'
        endif
      endif
!..................................................................
!  - sw downward surface flux components:
!
      do l = 24, 27
        k = l + 2

        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = rflux(i,j,l)*rtimsw
          enddo
        enddo

        call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ipur(k),itlr(k),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(k)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (l == 24) print*,'wrtsfc gribit ierr=',ierr,'  34) ',
     &       'downward sw uv+vis beam radiation flux (w/m**2) sfc '
            if (l == 25) print*,'wrtsfc gribit ierr=',ierr,'  35) ',
     &       'downward sw uv+vis diffuse radiation flux (w/m**2) sfc'
            if (l == 26) print*,'wrtsfc gribit ierr=',ierr,'  36) ',
     &       'downward sw nir beam radiation flux (w/m**2) sfc '
            if (l == 27) print*,'wrtsfc gribit ierr=',ierr,'  37) ',
     &       'downward sw nir diffuse radiation flux (w/m**2) sfc '
          endif
        endif
      enddo
!..................................................................
!  -  clear sky radiative fluxes at toa and surface:
!
      do l = 28, 33
        k = l - 8

        do j = 1, lats_node_r
          do i = 1, lonr
            glolal(i,j) = rflux(i,j,l)*rtimer(k)
          enddo
        enddo

        call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ipur(k),itlr(k),0,0,iyr,imo,ida,ihr,
     &                ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipur(k)),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            if (l == 28) print*,'wrtsfc gribit ierr=',ierr,'  38) ',
     &       'cs upward long wave radiation flux (w/m**2) at toa'
            if (l == 29) print*,'wrtsfc gribit ierr=',ierr,'  39) ',
     &       'cs upward solar radiation flux (w/m**2) at toa'
            if (l == 30) print*,'wrtsfc gribit ierr=',ierr,'  40) ',
     &       'cs downward long radiation flux (w/m**2) at surface'
            if (l == 31) print*,'wrtsfc gribit ierr=',ierr,'  41) ',
     &       'cs upward solar radiation flux (w/m**2)  at surface'
            if (l == 32) print*,'wrtsfc gribit ierr=',ierr,'  42) ',
     &       'cs downward solar radiation flux (w/m**2) at surface'
            if (l == 33) print*,'wrtsfc gribit ierr=',ierr,'  43) ',
     &       'cs upward long wave radiation (w/m**2) at surface'
          endif
        endif
      enddo
!...................................................................
!  - surface albedo (derived from radiative fluxes at surface):
!
      do j = 1, lats_node_r
        do i = 1, lonr
          if (rflux(i,j,4) > 0.) then
            glolal(i,j) = rflux(i,j,3)/rflux(i,j,4) * 100.
            if (glolal(i,j) > 100.) glolal(i,j) = 100.
          else
            glolal(i,j) = 0.
          endif
        enddo
      enddo

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ialbdo,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ialbdo),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  45) ',
     &     'albedo (percent) land and sea surface '
        endif
      endif
!..........................................................
!  - precipitation rate (geshem unit in m, final unit = mm/s = kg/m2/s)
!
      glolal = flx_fld%geshem*1.e3*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ipcpr,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ipcpr),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  46) ',
     &     'precipitation rate (kg/m**2/s) land and sea surface'
        endif
      endif
!...................................................................
!  - convective precipitation rate:
!
      glolal = flx_fld%bengsh*1.e3*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icpcpr,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icpcpr),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  47) ',
     &     'convective precipitation rate (kg/m**2/s) at surface'
        endif
      endif
!...................................................................
!  - ground heat flux:
!
      glolal = flx_fld%gflux*rtime

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful /= 0._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ighflx,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ighflx),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  48) ',
     &     'ground heat flux (w/m**2) land and sea surface'
        endif
      endif
!...................................................................
!  - land-sea mask:
!
      buffo = mod(slmskloc,2._kind_io8)

      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,islmsk,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(islmsk),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  49) ',
     &     'land-sea mask (1=land; 0=sea) (integer) land sea surface'
        endif
      endif
!...................................................................
!  - sea-ice concentration:
!
      call uninterpred(2,kmsk0,buffo,sfc_fld%fice,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icemsk,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(icemsk),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  50) ',
     &     'ice concentration (ice>0; no ice=0) land sea surface'
        endif
      endif
!...................................................................
!  - 10m u wind:
!
      call uninterpred(2,kmsk0,buffo,flx_fld%u10m,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iznlw,ielev,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iznlw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  51) ',
     &     'u wind (m/s) height above ground'
        endif
      endif
!...................................................................
!  - 10m v wind:
!
      call uninterpred(2,kmsk0,buffo,flx_fld%v10m,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,imerw,ielev,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(imerw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  52) ',
     &     'v wind (m/s) height above ground'
        endif
      endif
!...................................................................
!  - 2m temperature:
!
      call uninterpred(2,kmsk0,buffo,sfc_fld%t2m,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itemp,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  53) ',
     &     'temperature (k) height above ground'
        endif
      endif
!...................................................................
!  - 2m specific humidity:
!
      call uninterpred(2,kmsk0,buffo,sfc_fld%q2m,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,isphum,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isphum),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  54) ',
     &     'specific humidity (kg/kg) height above ground'
        endif
      endif
!...................................................................
!  - surface pressure:
!
      glolal = flx_fld%psurf

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iprs,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  55) ',
     &     'pressure (pa) land and sea surface'
        endif
      endif
!...................................................................
!  - maximum temperature:
!
      call uninterpred(2,kmsk0,buffo,flx_fld%tmpmax,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itmx,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iwin,0,0,icen2,ids(itmx),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  56) ',
     &     'maximum temperature (k) height above ground'
        endif
      endif
!...................................................................
!  - minimum temperature:
!
      call uninterpred(2,kmsk0,buffo,flx_fld%tmpmin,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itmn,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iwin,0,0,icen2,ids(itmn),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  57) ',
     &     'minimum temperature (k) height above ground'
        endif
      endif
!...................................................................
!  - maximum specific humidity
!
      call uninterpred(2,kmsk0,buffo,flx_fld%spfhmax,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &              0,iqmx,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iwin,0,0,icen2,ids_iq,iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  58) ',
     &     'maximum specific humidity (kg/kg) height above ground'
        endif
      endif
!...................................................................
!  - minimum specific humidity
!
      call uninterpred(2,kmsk0,buffo,flx_fld%spfhmin,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &              0,iqmn,ielev,0,2,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iwin,0,0,icen2,ids_iq,iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  59) ',
     &     'minimum specific humidity (kg/kg) height above ground'
        endif
      endif
!...................................................................
!  - runoff (accumulative value)
!
      glolal = flx_fld%runoff * 1.e3

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful /= 0._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,irnof,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iacc,0,0,icen2,ids(irnof),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  60) ',
     &     'runoff (kg/m**2) land and sea surface'
        endif
      endif
!...................................................................
!  - potential evaporation rate
!
      glolal = flx_fld%ep * rtime

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful /= 0._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iep,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iep),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  61) ',
     &     'potential evaporation rate (w/m**/) land and sea surface'
        endif
      endif
!...................................................................
!  - cloud work function
!
      glolal = flx_fld%cldwrk * rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldwk,icolmn,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(icldwk),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  62) ',
     &     'cloud work function (j/kg) total atmospheric column'
        endif
      endif
!...................................................................
!  - zonal gravity wave stress
!
      glolal = flx_fld%dugwd*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,izgw,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(izgw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  63) ',
     &     'zonal gravity wave stress (n/m**2) land and sea surface'
        endif
      endif
!...................................................................
!  - meridional gravity wave stress
!
      glolal = flx_fld%dvgwd*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,imgw,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(imgw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  64) ',
     &     'meridional gravity wave stress (n/m**2) land sea surface'
        endif
      endif
!...................................................................
!  - boundary layer height
!
      call uninterpred(2,kmsk0,buffo,flx_fld%hpbl,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ihpbl,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ihpbl),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  65) ',
     &     'boundary layer height'
        endif
      endif
!...................................................................
!  - precipitable water
!
      call uninterpred(2,kmsk0,buffo,flx_fld%pwat,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ipwat,icolmn,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ipwat),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  66) ',
     &     'precipitable water (kg/m**2) total atmospheric column'
        endif
      endif
!..........................................................
!  - convective clouds
!    * labeled instantaneous but actually averaged over fhswr seconds
!
      glolal = sfc_fld%cv*1.e2

      where (glolal >= 0.5)
        kmskcv = 1
      elsewhere
        kmskcv = 0
      endwhere

      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (wrkga >= 0.5_kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,icldf,icvlyr,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(icldf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  67) ',
     &     'total cloud cover (percent) convective cloud layer'
        endif
      endif
!..........................................................
!  - pressure at convective cloud top
!
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = 0.
          if (sfc_fld%cv(i,j) > 0.) then
            glolal(i,j) = sfc_fld%cvt(i,j)*1.e3
          endif
        enddo
      enddo

      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iprs,icvtl,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  68) ',
     &     'pressure (pa) convective cloud top level'
        endif
      endif
!..........................................................
!  - pressure at convective cloud bottom
!
      do j = 1, lats_node_r
        do i = 1, lonr
          glolal(i,j) = 0.
          if (sfc_fld%cv(i,j) > 0.) then
            glolal(i,j) = sfc_fld%cvb(i,j)*1.e3
          endif
        enddo
      enddo

      call uninterpred(2,kmskcv,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iprs,icvbl,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iprs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  69) ',
     &     'pressure (pa) convective cloud bottom level'
        endif
      endif
!..........................................................
!  - sea ice thickness
!
      call uninterpred(2,kmsk0,buffo,sfc_fld%hice,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful /= 1._kind_io8)
!       lbm = (slmskful == 2._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isik,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isik),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  70) ',
     &     'sea ice thickness (m) category 1'
        endif
      endif
!..........................................................
!  - volumetric soil moist content (layer 100cm and 40cm)
!
      if (lsoil > 2) then
        glolal(:,:) = sfc_fld%smc(:,3,:)

        call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,isoilm,i2dbls,40,100,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  71) ',
     &       '71) volumetric soil moist content (frac) 100cm and 40cm'
          endif
        endif
!..........................................................
!  - volumetric soil moist content (layer 200cm and 100cm)
!
        glolal(:,:) = sfc_fld%smc(:,4,:)

        call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,isoilm,i2dbls,100,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  72) ',
     &       'volumetric soil moist content (frac) 200cm and 100cm'
          endif
        endif
!..........................................................
!  - temperature for layer 100cm and 40cm below sfc
!
        glolal(:,:) = sfc_fld%stc(:,3,:)

        call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,itemp,i2dbls,40,100,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  73) ',
     &       'temp (k) layer betw two depth below sfc 100cm and 40cm'
          endif
        endif
!..........................................................
!  - temperature for layer 200cm and 100cm below sfc
!
        glolal(:,:) = sfc_fld%stc(:,4,:)

        call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,itemp,i2dbls,100,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  74) ',
     &       'temp (k) layer betw two depth below sfc 200cm and 100cm'
          endif
        endif
      endif   ! end_if_lsoil
!..........................................................
!  - liquid soil moist content layer 10cm and 0cm
!
      glolal(:,:) = sfc_fld%slc(:,1,:)

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,islc,i2dbls,0,10,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  75) ',
     &     'liquid soil moist content (frac) layer 10cm and 0cm '
        endif
      endif
!..........................................................
!  - liquid soil moist content layer 40cm and 10cm
!
      glolal(:,:) = sfc_fld%slc(:,2,:)

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)

        if (lsoil > 2) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &                1,islc,i2dbls,10,40,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  76) ',
     &       'liquid soil moist content (frac) layer 40cm and 10cm'
          endif
        else
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &                1,islc,i2dbls,10,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  76) ',
     &       'liquid soil moist content (frac) layer 200cm and 10cm'
          endif
        endif   ! end if_lsoil>2
      endif
!..........................................................
!  - liquid soil moist content layer 100cm and 40cm
!
      if (lsoil > 2) then
        glolal(:,:) = sfc_fld%slc(:,3,:)

        call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &                1,islc,i2dbls,40,100,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  77) ',
     &       'liquid soil moist content (frac) layer 100cm and 40cm'
          endif
        endif
!..........................................................
!  - liquid soil moist content layer 200cm and 100cm
!
        glolal(:,:) = sfc_fld%slc(:,4,:)

        call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          lbm = (slmskful == 1._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &                1,islc,i2dbls,100,200,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(isoilm),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  78) ',
     &       'liquid soil moist content (frac) layer 200cm and 100cm'
          endif
        endif
      endif    ! end if_lsoil>2
!..........................................................
!  - snow depth
!
      glolal = sfc_fld%snwdph / 1.e3       !! convert from mm to m

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
!       lbm = (slmskful == 1._kind_io8)
        lbm = (slmskful == 1._kind_io8 .or. slmskful == 2._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isnod,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isnod),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  79) ',
     &     'snow depth (m) land surface'
        endif
      endif
!..........................................................
!  - canopy water content
!
      call uninterpred(2,kmsk,buffo,sfc_fld%canopy,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,icnp,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(icnp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  80) ',
     &     'canopy water content (kg/m^2) land surface'
        endif
      endif
!..........................................................
!  - the following 30 records are for land mdl use
!  - surface roughness
!
      glolal = sfc_fld%zorl / 1.e2       !! convert from cm to m

      call uninterpred(1,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,izorl,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(izorl),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  81) ',
     &     'surface roughness (m)'
        endif
      endif
!..........................................................
!  - vegetation fraction
!
      glolal = sfc_fld%vfrac*100.

      call uninterpred(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iveg,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iveg),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  82) ',
     &     'vegetation fraction (fractional) land surface'
        endif
      endif
!..........................................................
!  - vegetation type
!
      call uninterpred(1,kmsk,buffo,sfc_fld%vtype,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ivtp,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ivtp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  83) ',
     &     'vegetation type land surface'
        endif
      endif
!..........................................................
!  - soil type
!
      call uninterpred(1,kmsk,buffo,sfc_fld%stype,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,istp,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(istp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  84) ',
     &     'soil type land surface'
        endif
      endif
!..........................................................
!  - slope type
!
      call uninterpred(1,kmsk,buffo,sfc_fld%slope,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,islo,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(islo),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  85) ',
     &     'slope type land surface'
        endif
      endif
!..........................................................
!  - frictional velocity
!
      call uninterpred(2,kmsk0,buffo,sfc_fld%uustar,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iust,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iust),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  86) ',
     &     'frictional velocity (m/s)'
        endif
      endif
!..........................................................
!  - surface height
!
      call uninterpred(1,kmsk0,buffo,sfc_fld%oro,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ihgt,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ihgt),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  87) ',
     &     'surface height (m)'
        endif
      endif
!..........................................................
!  - freezing precip flag
!
      call uninterpred(1,kmsk,buffo,sfc_fld%srflag,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,irst,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(irst),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  88) ',
     &     'freezing precip flag land surface'
        endif
      endif
!..........................................................
!  - exchange coefficient ch
!
      call uninterpred(2,kmsk0,buffo,flx_fld%chh,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,ichh,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ichh),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  89) ',
     &     'exchange coefficient ch(m/s)'
        endif
      endif
!..........................................................
!  - exchange coefficient cm
!
      call uninterpred(2,kmsk0,buffo,flx_fld%cmm,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              0,icmm,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(icmm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  90) ',
     &     'exchange coefficient cm(m/s)'
        endif
      endif
!..........................................................
!  - potential evaporation rate
!
      call uninterpred(2,kmsk,buffo,flx_fld%epi,
     &                 global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,iep,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iep),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  91) ',
     &     'potential evaporation rate (w/m**2) land and sea surface'
        endif
      endif
!..........................................................
!  - downward long wave radiation flux (instantaneous value)
!
      if (.not. climate) then         ! do not output those fields in climate mode

        call uninterpred(2,kmsk0,buffo,flx_fld%dlwsfci,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,idlwf,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(idlwf),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  92) ',
     &       'downward long wave radiation flux (w/m**2)'
          endif
        endif
!..........................................................
!  - upward long wave radiation flux (instantaneous value)
!
        call uninterpred(2,kmsk0,buffo,flx_fld%ulwsfci,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,iulwf,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(iulwf),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  93) ',
     &       'upward long wave radiation flux (w/m**2)'
          endif
        endif
!..........................................................
!  - upward short wave radiation flux (instantaneous value)
!
        call uninterpred(2,kmsk0,buffo,flx_fld%uswsfci,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,iuswf,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(iuswf),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  94) ',
     &       'upward short wave radiation flux (w/m**2)'
          endif
        endif
!..........................................................
!  - downward short wave radiation flux (instantaneous value)
!
        call uninterpred(2,kmsk0,buffo,flx_fld%dswsfci,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,idswf,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(idswf),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  95) ',
     &       'downward short wave radiation flux (w/m**2)'
          endif
        endif
!..........................................................
!  - sensible heat flux (instantaneous value)
!
        call uninterpred(2,kmsk0,buffo,flx_fld%dtsfci,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ishflx,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(ishflx),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  96) ',
     &       'sensible heat flux (w/m**2) land and sea surface'
          endif
        endif
!..........................................................
!  - latent heat flux (instantaneous value)
!
        call uninterpred(2,kmsk0,buffo,flx_fld%dqsfci,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                0,ilhflx,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(ilhflx),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  97) ',
     &       'latent heat flux (w/m**2) land and sea surface'
          endif
        endif
!..........................................................
!  - ground heat flux (instantaneous value)
!
        call uninterpred(2,kmsk,buffo,flx_fld%gfluxi,
     &                   global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
          lbm = (slmskful /= 0._kind_io8)
          call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &                1,ighflx,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,ids(ighflx),iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

          if (ierr == 0) then
            call wryte(noflx,lg,g)
          else
            print*,'wrtsfc gribit ierr=',ierr,'  98) ',
     &       'ground heat flux (w/m**2) land and sea surface'
          endif
        endif
!
      endif          ! end of if(.not. climate)
!..........................................................
!  - surface runoff
!
      glolal = flx_fld%srunoff * 1.e3

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isrf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isrf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  99) ',
     &     'surface runoff (kg/m^2) land surface'
        endif
      endif
!..........................................................
!  - lowest model level temp
!
      call uninterpred(2,kmsk0,buffo,flx_fld%t1,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,itemp,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(itemp),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  100) ',
     &     ' lowest model level temp (k)'
        endif
      endif
!..........................................................
!  - lowest model specific humidity
!
      call uninterpred(2,kmsk0,buffo,flx_fld%q1,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,isphum,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(isphum),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  101) ',
     &     'lowest model specific humidity (kg/kg)'
        endif
      endif
!..........................................................
!  - lowest model u wind
!
      call uninterpred(2,kmsk0,buffo,flx_fld%u1,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,iznlw,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(iznlw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  102) ',
     &     'lowest model u wind (m/s)'
        endif
      endif
!..........................................................
!  - lowest model v wind
!
      call uninterpred(2,kmsk0,buffo,flx_fld%v1,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              0,imerw,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(imerw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  103) ',
     &     'lowest model v wind (m/s)'
        endif
      endif
!..........................................................
!  - lowest model level height
!
      call uninterpred(2,kmsk,buffo,flx_fld%zlvl,
     &                 global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ihgt,isglev,1,1,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ihgt),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  104) ',
     &     'lowest model level height (m) land surface'
        endif
      endif
!..........................................................
!  - direct evaporation from bare soil
!
      glolal = flx_fld%evbsa*rtime

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ievbs,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ievbs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  105) ',
     &     'direct evaporation from bare soil(w/m^2) land surface'
        endif
      endif
!..........................................................
!  - canopy water evaporation
!
      glolal = flx_fld%evcwa*rtime

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,ievcw,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(ievcw),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  106) ',
     &     'canopy water evaporation(w/m^2) land surface'
        endif
      endif
!..........................................................
!  - transpiration
!
      glolal = flx_fld%transa*rtime

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,itran,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(itran),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  107) ',
     &     'transpiration (w/m^2) land surface'
        endif
      endif
!..........................................................
!  - snow sublimation
!
      glolal = flx_fld%sbsnoa*rtime

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,isbs,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(isbs),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  108) ',
     &     'snow sublimation (w/m^2) land surface'
        endif
      endif
!..........................................................
!  - snow cover
!
      glolal = flx_fld%snowca*rtime*100.

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,isnc,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(isnc),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  109) ',
     &     'snow cover (fraction) land surface'
        endif
      endif
!..........................................................
!  - total column soil moisture
!
      glolal = flx_fld%soilm*1.e3       !! convert from m to (mm)kg/m^2

      call uninterpred(2,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,
     &              1,istc,i2dbls,0,200,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(istc),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  110) ',
     &     'total column soil moisture (kg/m^2) land surface'
        endif
      endif
!..........................................................
!  - snow phase-change heat flux
!
      glolal = flx_fld%snohfa*rtime

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,isnohf,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(isnohf),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  111) ',
     &     'snow phase-change heat flux [w/m^2] land surface'
        endif
      endif
!..........................................................
!  - wilting point
!
      glolal = flx_fld%smcwlt2

      call uninterpred(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,ismcwlt,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ismcwlt),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  112) ',
     &     'wilting point [fraction] land surface'
        endif
      endif
!..........................................................
!  - field capacity
!
      glolal = flx_fld%smcref2

      call uninterpred(1,kmsk,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

      if (me == ioproc) then
        lbm = (slmskful == 1._kind_io8)
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,130,icen,igen,
     &              1,ismcref,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ithr,0,inst,0,0,icen2,ids(ismcref),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  113) ',
     &     'field capacity [fraction] land surface'
        endif
      endif
!..........................................................
!  - accumulated sunshine duration time
!
      glolal = flx_fld%suntim

      call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
      call unsplit2d(ioproc,wrkga,buffo,global_lats_r)
      if (me == ioproc) then
        call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,133,icen,igen,
     &              0,isuntm,isfc,0,0,iyr,imo,ida,ihr,
     &              ifhour,ifhr,ithr,iacc,0,0,icen2,ids(isuntm),iens,
     &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

        if (ierr == 0) then
          call wryte(noflx,lg,g)
        else
          print*,'wrtsfc gribit ierr=',ierr,'  114) ',
     &     'accumulated sunshine duration time (sec)'
        endif
      endif
!
!..........................................................
!  - frozen precipitation fraction
!
        glolal = flx_fld%sr

        call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
        call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

        if (me == ioproc) then
         call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen, 
     &                0,isr,isfc,0,0,iyr,imo,ida,ihr,
     &                ifhour,ithr,0,inst,0,0,icen2,1,iens,
     &                0.,0.,0.,0.,0.,0.,g,lg,ierr)

         if (ierr == 0) then
           call wryte(noflx,lg,g)
         else
           print*,'wrtsfc gribit ierr=',ierr,'  123) ', 
     &       'frozen precipitation fraction at surface'
         endif
       endif
!..........................................................
!  ---  aerosols optical depth at 550nm  (optional)
!
!     do j = 1, lats_node_r
!       do i = 1, lonr
!         glolal(i,j) = rflux(i,j,34)*rtimsw
!       enddo
!     enddo

!     call uninterpred(2,kmsk0,buffo,glolal,global_lats_r,lonsperlar)
!     call unsplit2d(ioproc,wrkga,buffo,global_lats_r)

!     if (me == ioproc) then
!       call gribit(wrkga,lbm,4,lonr,latr,16,cl1,ilpds,2,icen,igen,     &
!    &              0,iaod,icolmn,0,0,iyr,imo,ida,ihr,                  &
!    &              ifhour,ifhr,ithr,iavg,0,0,icen2,ids(iaod),iens,     &
!    &              0.,0.,0.,0.,0.,0.,g,lg,ierr)

!       if (ierr == 0) then
!         call wryte(noflx,lg,g)
!       else
!         print*,'wrtsfc gribit ierr=',ierr,'  124',                    &
!    &     'total aerosol optical depth at 550 nm wavelength '
!       endif
!     endif
!..........................................................

      if (me == ioproc) then
        print *,'grib flux file written ',fhour,idate,noflx
      endif
!
      return
!...................................
      end subroutine wrtsfc
!-----------------------------------
