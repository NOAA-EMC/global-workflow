!!!!!              module_radiation_clouds description             !!!!!
!!!!!  ==========================================================  !!!!!
!                                                                      !
!    the 'radiation_clouds.f' contains:                                !
!                                                                      !
!       'module_radiation_clouds' ---  compute cloud related quantities!
!                for radiation computations                            !
!                                                                      !
!    the following are the externally accessable subroutines:          !
!                                                                      !
!       'cld_init'           --- initialization routine                !
!          inputs:                                                     !
!           ( si, nlay, me )                                           !
!          outputs:                                                    !
!           ( none )                                                   !
!                                                                      !
!       'progcld1'           --- zhao/moorthi prognostic cloud scheme  !
!          inputs:                                                     !
!           (plyr,plvl,tlyr,tvly,qlyr,qstl,rhly,clw,                   !
!            xlat,xlon,slmsk,                                          !
!            ix, nlay, nlp1,                                           !
!          outputs:                                                    !
!            clouds,clds,mtop,mbot)                                    !
!                                                                      !
!       'progcld2'           --- ferrier prognostic cloud microphysics !
!          inputs:                                                     !
!           (plyr,plvl,tlyr,tvly,qlyr,qstl,rhly,clw,                   !
!            xlat,xlon,slmsk, f_ice,f_rain,r_rime,flgmin,              !
!            ix, nlay, nlp1,                                           !
!          outputs:                                                    !
!            clouds,clds,mtop,mbot)                                    !
!       'progcld3'           --- zhao/moorthi prognostic cloud + pdfcld!
!          inputs:                                                     !
!           (plyr,plvl,tlyr,tvly,qlyr,qstl,rhly,clw, cnvw,cnvc,        !
!            xlat,xlon,slmsk,                                          !
!            ix, nlay, nlp1,                                           !
!            deltaq,sup,kdt,me,                                        !
!          outputs:                                                    !
!            clouds,clds,mtop,mbot)                                    !
!                                                                      !
!       'diagcld1'           --- diagnostic cloud calc routine         !
!          inputs:                                                     !
!           (plyr,plvl,tlyr,rhly,vvel,cv,cvt,cvb,                      !
!            xlat,xlon,slmsk,                                          !
!            ix, nlay, nlp1,                                           !
!          outputs:                                                    !
!            clouds,clds,mtop,mbot)                                    !
!                                                                      !
!    internal accessable only subroutines:                             !
!       'gethml'             --- get diagnostic hi, mid, low clouds    !
!                                                                      !
!       'rhtable'            --- rh lookup table for diag cloud scheme !
!                                                                      !
!                                                                      !
!    cloud array description:                                          !
!                ---  for prognostic cloud: icldflg=1  ---             !
!          clouds(:,:,1)  -  layer total cloud fraction                !
!          clouds(:,:,2)  -  layer cloud liq water path                !
!          clouds(:,:,3)  -  mean effective radius for liquid cloud    !
!          clouds(:,:,4)  -  layer cloud ice water path                !
!          clouds(:,:,5)  -  mean effective radius for ice cloud       !
!          clouds(:,:,6)  -  layer rain drop water path                !
!          clouds(:,:,7)  -  mean effective radius for rain drop       !
!   **     clouds(:,:,8)  -  layer snow flake water path               !
!          clouds(:,:,9)  -  mean effective radius for snow flake      !
!   ** fu's scheme need to be normalized by snow density (g/m**3/1.0e6)!
!                ---  for diagnostic cloud: icldflg=0  ---             !
!          clouds(:,:,1)  -  layer total cloud fraction                !
!          clouds(:,:,2)  -  layer cloud optical depth                 !
!          clouds(:,:,3)  -  layer cloud single scattering albedo      !
!          clouds(:,:,4)  -  layer cloud asymmetry factor              !
!                                                                      !
!    external modules referenced:                                      !
!                                                                      !
!       'module physpara'            in 'physpara.f'                   !
!       'module physcons'            in 'physcons.f'                   !
!       'module module_microphysics' in 'module_bfmicrophysics.f'      !
!                                                                      !
!                                                                      !
! program history log:                                                 !
!      nov 1992,   y.h., k.a.c, a.k. - cloud parameterization          !
!        'cldjms' patterned after slingo and slingo's work (jgr,       !
!        1992), stratiform clouds are allowed in any layer except      !
!        the surface and upper stratosphere. the relative humidity     !
!        criterion may cery in different model layers.                 !
!      mar 1993,   kenneth campana   - created original crhtab tables  !
!        for critical rh look up references.
!      feb 1994,   kenneth campana   - modified to use only one table  !
!        for all forecast hours.                                       !
!      oct 1995,   kenneth campana   - tuned cloud rh curves           !
!        rh-cld relation from tables created using mitchell-hahn       !
!        tuning technique on airforce rtneph observations.             !
!      nov 1995,   kenneth campana   - the bl relationships used       !
!        below llyr, except in marine stratus regions.                 !
!      apr 1996,   kenneth campana   - save bl cld amt in cld(,5)      !
!      aug 1997,   kenneth campana   - smooth out last bunch of bins   !
!        of the rh look up tables                                      !
!      dec 1998,   s. moorthi        - added prognostic cloud method   !
!      apr 2003,   yu-tai hou        - rewritten in frotran 90         !
!        modulized form 'module_rad_clouds' from combining the original!
!        subroutines 'cldjms', 'cldprp', and 'gcljms'. and seperated   !
!        prognostic and diagnostic methods into two packages.          !
!      --- 2003,   s. moorthi        - adapted b. ferrier's prognostic !
!        cloud scheme to ncep gfs model as additional option.          !
!      apr 2004,   yu-tai hou        - separated calculation of the    !
!        averaged h,m,l,bl cloud amounts from each of the cld schemes  !
!        to become an shared individule subprogram 'gethml'.           !
!      may 2004,   yu-tai hou        - rewritten ferrier's scheme as a !
!        separated program 'progcld2' in the cloud module.             !
!      apr 2005,   yu-tai hou        - modified cloud array and module !
!        structures.                                                   !
!      dec 2008,   yu-tai hou        - changed low-cld calculation,    !
!        now cantains clds from sfc layer and upward to the low/mid    !
!        boundary (include bl-cld). h,m,l clds domain boundaries are   !
!        adjusted for better agreement with observations.              !
!      jan 2011,   yu-tai hou        - changed virtual temperature     !
!        as input variable instead of originally computed inside the   !
!        two prognostic cld schemes 'progcld1' and 'progcld2'.         !
!      aug 2012,   yu-tai hou        - modified subroutine cld_init    !
!        to pass all fixed control variables at the start. and set     !
!        their correponding internal module variables to be used by    !
!        module subroutines.                                           !
!      nov 2012,   yu-tai hou        - modified control parameters     !
!        thru module 'physpara'.                                       !
!      apr 2013,   h.lin/y.hou       - corrected error in calculating  !
!        llyr for different vertical indexing directions.              !
!      jul 2013, r. sun/h. pan       - modified to use pdf cloud and   !
! 	 convective cloud cover and water for radiation                !
!                                                                      !
!!!!!  ==========================================================  !!!!!
!!!!!                       end descriptions                       !!!!!
!!!!!  ==========================================================  !!!!!


!========================================!
      module module_radiation_clouds     !
!........................................!
!
      use physpara,            only : icldflg, icmphys, iovrsw, iovrlw, &
     &                                lcrick, lcnorm, lnoprec,          &
     &                                ivflip, kind_phys, kind_io4
      use physcons,            only : con_fvirt, con_ttp, con_rocp,     &
     &                                con_t0c, con_pi, con_g, con_rd,   & 
     &                                con_thgni
      use module_microphysics, only : rsipath2
      use module_iounitdef,    only : nicltun
!
      implicit   none
!
      private

!  ---  version tag and last revision date
      character(40), parameter ::                                       &
     &   vtagcld='ncep-radiation_clouds    v5.1  nov 2012 '
!    &   vtagcld='ncep-radiation_clouds    v5.0  aug 2012 '

!  ---  set constant parameters
      real (kind=kind_phys), parameter :: gfac=1.0e5/con_g              &
     &,                                   gord=con_g/con_rd
      integer, parameter, public :: nf_clds = 9   ! number of fields in cloud array
      integer, parameter, public :: nk_clds = 3   ! number of cloud vertical domains

!  ---  pressure limits of cloud domain interfaces (low,mid,high) in mb (0.1kpa)
      real (kind=kind_phys), save :: ptopc(nk_clds+1,2)

!org  data ptopc / 1050., 642., 350., 0.0,  1050., 750., 500., 0.0 /
      data ptopc / 1050., 650., 400., 0.0,  1050., 750., 500., 0.0 /

!     real (kind=kind_phys), parameter :: climit = 0.01
      real (kind=kind_phys), parameter :: climit = 0.001, climit2=0.05
      real (kind=kind_phys), parameter :: ovcst  = 1.0 - 1.0e-8

!  ---  set default quantities as parameters (for prognostic cloud)
      real (kind=kind_phys), parameter :: reliq_def = 10.0    ! default liq radius to 10 micron
      real (kind=kind_phys), parameter :: reice_def = 50.0    ! default ice radius to 50 micron
      real (kind=kind_phys), parameter :: rrain_def = 1000.0  ! default rain radius to 1000 micron
      real (kind=kind_phys), parameter :: rsnow_def = 250.0   ! default snow radius to 250 micron

!  ---  set look-up table dimensions and other parameters (for diagnostic cloud)
      integer, parameter :: nbin=100     ! rh in one percent interval
      integer, parameter :: nlon=2       ! =1,2 for eastern and western hemispheres
      integer, parameter :: nlat=4       ! =1,4 for 60n-30n,30n-equ,equ-30s,30s-60s
      integer, parameter :: mcld=4       ! =1,4 for bl,low,mid,hi cld type
      integer, parameter :: nseal=2      ! =1,2 for land,sea

      real (kind=kind_phys), parameter :: cldssa_def = 0.99   ! default cld single scat albedo
      real (kind=kind_phys), parameter :: cldasy_def = 0.84   ! default cld asymmetry factor

!  ---  xlabdy: lat bndry between tuning regions, +/- xlim for transition
!       xlobdy: lon bndry between tuning regions
      real (kind=kind_phys), parameter :: xlim=5.0
      real (kind=kind_phys)            :: xlabdy(3), xlobdy(3)

      data xlabdy / 30.0,  0.0, -30.0 /,  xlobdy / 0.0, 180., 360. /

!  ---  low cloud vertical velocity adjustment boundaries in mb/sec
      real (kind=kind_phys), parameter :: vvcld1= 0.0003e0
      real (kind=kind_phys), parameter :: vvcld2=-0.0005e0

!  ---  those data will be set up by "cld_init"
!     rhcl : tuned rh relation table for diagnostic cloud scheme

      real (kind=kind_phys) :: rhcl(nbin,nlon,nlat,mcld,nseal)

      integer  :: llyr   = 2           ! upper limit of boundary layer clouds
      integer  :: iovr   = 1           ! cloud over lapping method for diagnostic 3-domain
                                       ! output calc (see iovrsw/iovrlw description)

      public progcld1, progcld2, progcld3, diagcld1, cld_init


! =================
      contains
! =================


!-----------------------------------
      subroutine cld_init                                               &
!...................................

!  ---  inputs:
     &     ( si, nlay, me )
!  ---  outputs:
!          ( none )

!  ===================================================================  !
!                                                                       !
! abstract: cld_init is an initialization program for cloud-radiation   !
!   calculations. it sets up boundary layer cloud top.                  !
!                                                                       !
!                                                                       !
! inputs:                                                               !
!   si (l+1)        : model vertical sigma layer interface              !
!   nlay            : vertical layer number                             !
!   me              : print control flag                                !
!                                                                       !
!  outputs: (none)                                                      !
!           to module variables                                         !
!                                                                       !
!  external module variables: (in physpara)                             !
!   icldflg         : cloud optical property scheme control flag        !
!                     =0: model use diagnostic cloud method             !
!                     =1: model use prognostic cloud method             !
!   icmphys         : cloud microphysics scheme control flag            !
!                     =1: zhao/carr/sundqvist microphysics cloud        !
!                     =2: ferrier microphysics cloud scheme             !
!		      =3: zhao/carr/sundqvist microphysics cloud +pdfcld!
!   iovrsw/iovrlw   : sw/lw control flag for cloud overlapping scheme   !
!                     =0: random overlapping clouds                     !
!                     =1: max/ran overlapping clouds                    !
!   ivflip          : control flag for direction of vertical index      !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!  usage:       call cld_init                                           !
!                                                                       !
!  subroutines called:    rhtable                                       !
!                                                                       !
!  ===================================================================  !
!
      implicit none

!  ---  inputs:
      integer, intent(in) :: nlay, me

      real (kind=kind_phys), intent(in) :: si(:)

!  ---  outputs: (none)

!  ---  locals:
      integer :: k, kl, ier

!
!===> ...  begin here
!
!  ---  set up module variables

      iovr    = max( iovrsw, iovrlw )    !cld ovlp used for diag hml cld output

      if (me == 0) print *, vtagcld      !print out version tag

      if ( icldflg == 0 ) then
        if (me == 0) print *,' - using diagnostic cloud method'

!  ---  set up tuned rh table

        call rhtable( me, ier )

        if (ier < 0) then
          write(6,99) ier
  99      format(3x,' *** error in finding tuned rh table ***'          &
     &,         /3x,'     stop at calling subroutine rhtable !!'/)
          stop 99
        endif
      else
        if (me == 0) then
          print *,' - using prognostic cloud method'
          if (icmphys == 1) then
            print *,'   --- zhao/carr/sundqvist microphysics'
          elseif (icmphys == 2) then
            print *,'   --- ferrier cloud microphysics'
          elseif (icmphys == 3) then
            print *,'   --- zhao/carr/sundqvist + pdf cloud'
          else
            print *,'  !!! error in cloud microphysc specification!!!', &
     &              '  icmphys (np3d) =',icmphys
            stop
          endif
        endif
      endif

!  ---  compute llyr - the top of bl cld and is topmost non cld(low) layer
!       for stratiform (at or above lowest 0.1 of the atmosphere)

      if ( ivflip == 0 ) then    ! data from toa to sfc
        lab_do_k0 : do k = nlay, 2, -1
          kl = k
          if (si(k) < 0.9e0) exit lab_do_k0
        enddo  lab_do_k0

        llyr = kl
      else                      ! data from sfc to top
        lab_do_k1 : do k = 2, nlay
          kl = k
          if (si(k) < 0.9e0) exit lab_do_k1
        enddo  lab_do_k1

        llyr = kl - 1
      endif                     ! end_if_ivflip

!
      return
!...................................
      end subroutine cld_init
!-----------------------------------


!-----------------------------------
      subroutine progcld1                                               &
!...................................

!  ---  inputs:
     &     ( plyr,plvl,tlyr,tvly,qlyr,qstl,rhly,clw,                    &
     &       xlat,xlon,slmsk,                                           &
     &       ix, nlay, nlp1,                                            &
!  ---  outputs:
     &       clouds,clds,mtop,mbot                                      &
     &      )

! =================   subprogram documentation block   ================ !
!                                                                       !
! subprogram:    progcld1    computes cloud related quantities using    !
!   zhao/moorthi's prognostic cloud microphysics scheme.                !
!                                                                       !
! abstract:  this program computes cloud fractions from cloud           !
!   condensates, calculates liquid/ice cloud droplet effective radius,  !
!   and computes the low, mid, high, total and boundary layer cloud     !
!   fractions and the vertical indices of low, mid, and high cloud      !
!   top and base.  the three vertical cloud domains are set up in the   !
!   initial subroutine "cld_init".                                      !
!                                                                       !
! usage:         call progcld1                                          !
!                                                                       !
! subprograms called:   gethml                                          !
!                                                                       !
! attributes:                                                           !
!   language:   fortran 90                                              !
!   machine:    ibm-sp, sgi                                             !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
! input variables:                                                      !
!   plyr  (ix,nlay) : model layer mean pressure in mb (100pa)           !
!   plvl  (ix,nlp1) : model level pressure in mb (100pa)                !
!   tlyr  (ix,nlay) : model layer mean temperature in k                 !
!   tvly  (ix,nlay) : model layer virtual temperature in k              !
!   qlyr  (ix,nlay) : layer specific humidity in gm/gm                  !
!   qstl  (ix,nlay) : layer saturate humidity in gm/gm                  !
!   rhly  (ix,nlay) : layer relative humidity (=qlyr/qstl)              !
!   clw   (ix,nlay) : layer cloud condensate amount                     !
!   xlat  (ix)      : grid latitude in radians, default to pi/2 -> -pi/2!
!                     range, otherwise see in-line comment              !
!   xlon  (ix)      : grid longitude in radians  (not used)             !
!   slmsk (ix)      : sea/land mask array (sea:0,land:1,sea-ice:2)      !
!   ix              : horizontal dimention                              !
!   nlay,nlp1       : vertical layer/level dimensions                   !
!                                                                       !
! output variables:                                                     !
!   clouds(ix,nlay,nf_clds) : cloud profiles                            !
!      clouds(:,:,1) - layer total cloud fraction                       !
!      clouds(:,:,2) - layer cloud liq water path         (g/m**2)      !
!      clouds(:,:,3) - mean eff radius for liq cloud      (micron)      !
!      clouds(:,:,4) - layer cloud ice water path         (g/m**2)      !
!      clouds(:,:,5) - mean eff radius for ice cloud      (micron)      !
!      clouds(:,:,6) - layer rain drop water path         not assigned  !
!      clouds(:,:,7) - mean eff radius for rain drop      (micron)      !
!  *** clouds(:,:,8) - layer snow flake water path        not assigned  !
!      clouds(:,:,9) - mean eff radius for snow flake     (micron)      !
!  *** fu's scheme need to be normalized by snow density (g/m**3/1.0e6) !
!   clds  (ix,5)    : fraction of clouds for low, mid, hi, tot, bl      !
!   mtop  (ix,3)    : vertical indices for low, mid, hi cloud tops      !
!   mbot  (ix,3)    : vertical indices for low, mid, hi cloud bases     !
!                                                                       !
! module variables:                                                     !
!   ivflip          : control flag of vertical index direction          !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!   lcrick          : control flag for eliminating crick                !
!                     =t: apply layer smoothing to eliminate crick      !
!                     =f: do not apply layer smoothing                  !
!   lcnorm          : control flag for in-cld condensate                !
!                     =t: normalize cloud condensate                    !
!                     =f: not normalize cloud condensate                !
!                                                                       !
!  ====================    end of description    =====================  !
!
      implicit none

!  ---  inputs
      integer,  intent(in) :: ix, nlay, nlp1

      real (kind=kind_phys), dimension(:,:), intent(in) :: plvl, plyr,  &
     &       tlyr, tvly, qlyr, qstl, rhly, clw

      real (kind=kind_phys), dimension(:),   intent(in) :: xlat, xlon,  &
     &       slmsk

!  ---  outputs
      real (kind=kind_phys), dimension(:,:,:), intent(out) :: clouds

      real (kind=kind_phys), dimension(:,:),   intent(out) :: clds

      integer,               dimension(:,:),   intent(out) :: mtop,mbot

!  ---  local variables:
      real (kind=kind_phys), dimension(ix,nlay) :: cldtot, cldcnv,      &
     &       cwp, cip, crp, csp, rew, rei, res, rer, delp, tem2d, clwf

      real (kind=kind_phys) :: ptop1(ix,nk_clds+1)

      real (kind=kind_phys) :: clwmin, clwm, clwt, onemrh, value,       &
     &       tem1, tem2, tem3

      integer :: i, k, id, nf

!
!===> ... begin here
!
      do nf=1,nf_clds
        do k=1,nlay
          do i=1,ix
            clouds(i,k,nf) = 0.0
          enddo
        enddo
      enddo
!     clouds(:,:,:) = 0.0

      do k = 1, nlay
        do i = 1, ix
          cldtot(i,k) = 0.0
          cldcnv(i,k) = 0.0
          cwp   (i,k) = 0.0
          cip   (i,k) = 0.0
          crp   (i,k) = 0.0
          csp   (i,k) = 0.0
          rew   (i,k) = reliq_def            ! default liq radius to 10 micron
          rei   (i,k) = reice_def            ! default ice radius to 50 micron
          rer   (i,k) = rrain_def            ! default rain radius to 1000 micron
          res   (i,k) = rsnow_def            ! default snow radius to 250 micron
          tem2d (i,k) = min( 1.0, max( 0.0, (con_ttp-tlyr(i,k))*0.05 ) )
          clwf(i,k)   = 0.0
        enddo
      enddo
!
      if ( lcrick ) then
        do i = 1, ix
          clwf(i,1)    = 0.75*clw(i,1)    + 0.25*clw(i,2)
          clwf(i,nlay) = 0.75*clw(i,nlay) + 0.25*clw(i,nlay-1)
        enddo
        do k = 2, nlay-1
          do i = 1, ix
            clwf(i,k) = 0.25*clw(i,k-1) + 0.5*clw(i,k) + 0.25*clw(i,k+1)
          enddo
        enddo
      else
        do k = 1, nlay
          do i = 1, ix
            clwf(i,k) = clw(i,k)
          enddo
        enddo
      endif

!  ---  find top pressure for each cloud domain for given latitude
!       ptopc(k,i): top presure of each cld domain (k=1-4 are sfc,l,m,h;
!  ---  i=1,2 are low-lat (<45 degree) and pole regions)

      do id = 1, 4
        tem1 = ptopc(id,2) - ptopc(id,1)

        do i =1, ix
          tem2 = xlat(i) / con_pi        ! if xlat in pi/2 -> -pi/2 range
!         tem2 = 0.5 - xlat(i)/con_pi    ! if xlat in 0 -> pi range

          ptop1(i,id) = ptopc(id,1) + tem1*max( 0.0, 4.0*abs(tem2)-1.0 )
        enddo
      enddo

!  ---  compute liquid/ice condensate path in g/m**2

      if ( ivflip == 0 ) then          ! input data from toa to sfc
        do k = 1, nlay
          do i = 1, ix
            delp(i,k) = plvl(i,k+1) - plvl(i,k)
            clwt     = max(0.0, clwf(i,k)) * gfac * delp(i,k)
            cip(i,k) = clwt * tem2d(i,k)
            cwp(i,k) = clwt - cip(i,k)
          enddo
        enddo
      else                             ! input data from sfc to toa
        do k = 1, nlay
          do i = 1, ix
            delp(i,k) = plvl(i,k) - plvl(i,k+1)
            clwt     = max(0.0, clwf(i,k)) * gfac * delp(i,k)
            cip(i,k) = clwt * tem2d(i,k)
            cwp(i,k) = clwt - cip(i,k)
          enddo
        enddo
      endif                            ! end_if_ivflip

!  ---  effective liquid cloud droplet radius over land

      do i = 1, ix
        if (nint(slmsk(i)) == 1) then
          do k = 1, nlay
            rew(i,k) = 5.0 + 5.0 * tem2d(i,k)
          enddo
        endif
      enddo

!  ---  layer cloud fraction

      if ( ivflip == 0 ) then              ! input data from toa to sfc

        clwmin = 0.0
        do k = nlay, 1, -1
        do i = 1, ix
          clwt = 1.0e-6 * (plyr(i,k)*0.001)
!         clwt = 2.0e-6 * (plyr(i,k)*0.001)

          if (clwf(i,k) > clwt) then
            onemrh= max( 1.e-10, 1.0-rhly(i,k) )
            clwm  = clwmin / max( 0.01, plyr(i,k)*0.001 )

!           tem1  = min(max(sqrt(sqrt(onemrh*qstl(i,k))),0.0001),1.0)
!           tem1  = 2000.0 / tem1

            tem1  = min(max((onemrh*qstl(i,k))**0.49,0.0001),1.0)  !jhan
            tem1  = 100.0 / tem1
!
!           tem1  = 2000.0 / tem1
!           tem1  = 1000.0 / tem1
!

            value = max( min( tem1*(clwf(i,k)-clwm), 50.0 ), 0.0 )
            tem2  = sqrt( sqrt(rhly(i,k)) )
            cldtot(i,k) = max( tem2*(1.0-exp(-value)), 0.0 )
          endif
         enddo
         enddo

      else                                 ! input data from sfc to toa

        clwmin = 0.0
        do k = 1, nlay
        do i = 1, ix
          clwt = 1.0e-6 * (plyr(i,k)*0.001)
!         clwt = 2.0e-6 * (plyr(i,k)*0.001)

          if (clwf(i,k) > clwt) then
            onemrh= max( 1.e-10, 1.0-rhly(i,k) )
            clwm  = clwmin / max( 0.01, plyr(i,k)*0.001 )

!           tem1  = min(max(sqrt(sqrt(onemrh*qstl(i,k))),0.0001),1.0)
!           tem1  = 2000.0 / tem1

            tem1  = min(max((onemrh*qstl(i,k))**0.49,0.0001),1.0)  !jhan
            tem1  = 100.0 / tem1
!
!           tem1  = 2000.0 / tem1
!           tem1  = 1000.0 / tem1
!

            value = max( min( tem1*(clwf(i,k)-clwm), 50.0 ), 0.0 )
            tem2  = sqrt( sqrt(rhly(i,k)) )

            cldtot(i,k) = max( tem2*(1.0-exp(-value)), 0.0 )
          endif
        enddo
        enddo

      endif                                ! end_if_flip

      do k = 1, nlay
        do i = 1, ix
          if (cldtot(i,k) < climit) then
            cldtot(i,k) = 0.0
            cwp(i,k)    = 0.0
            cip(i,k)    = 0.0
            crp(i,k)    = 0.0
            csp(i,k)    = 0.0
          endif
        enddo
      enddo

      if ( lcnorm ) then
        do k = 1, nlay
          do i = 1, ix
            if (cldtot(i,k) >= climit) then
              tem1 = 1.0 / max(climit2, cldtot(i,k))
              cwp(i,k) = cwp(i,k) * tem1
              cip(i,k) = cip(i,k) * tem1
              crp(i,k) = crp(i,k) * tem1
              csp(i,k) = csp(i,k) * tem1
            endif
          enddo
        enddo
      endif

!  ---  effective ice cloud droplet radius

      do k = 1, nlay
        do i = 1, ix
          tem2 = tlyr(i,k) - con_ttp

          if (cip(i,k) > 0.0) then
!           tem3 = gord * cip(i,k) * (plyr(i,k)/delp(i,k)) / tvly(i,k)
            tem3 = gord * cip(i,k) * plyr(i,k) / (delp(i,k)*tvly(i,k))

            if (tem2 < -50.0) then
              rei(i,k) = (1250.0/9.917) * tem3 ** 0.109
            elseif (tem2 < -40.0) then
              rei(i,k) = (1250.0/9.337) * tem3 ** 0.08
            elseif (tem2 < -30.0) then
              rei(i,k) = (1250.0/9.208) * tem3 ** 0.055
            else
              rei(i,k) = (1250.0/9.387) * tem3 ** 0.031
            endif
!           rei(i,k)   = max(20.0, min(rei(i,k), 300.0))
!           rei(i,k)   = max(10.0, min(rei(i,k), 100.0))
            rei(i,k)   = max(10.0, min(rei(i,k), 150.0))
!           rei(i,k)   = max(5.0, min(rei(i,k), 130.0))
          endif
        enddo
      enddo

!
      do k = 1, nlay
        do i = 1, ix
          clouds(i,k,1) = cldtot(i,k)
          clouds(i,k,2) = cwp(i,k)
          clouds(i,k,3) = rew(i,k)
          clouds(i,k,4) = cip(i,k)
          clouds(i,k,5) = rei(i,k)
!         clouds(i,k,6) = 0.0
          clouds(i,k,7) = rer(i,k)
!         clouds(i,k,8) = 0.0
          clouds(i,k,9) = rei(i,k)
        enddo
      enddo


!  ---  compute low, mid, high, total, and boundary layer cloud fractions
!       and clouds top/bottom layer indices for low, mid, and high clouds.
!       the three cloud domain boundaries are defined by ptopc.  the cloud
!       overlapping method is defined by control flag 'iovr', which may
!       be different for lw and sw radiation programs.

      call gethml                                                       &
!  ---  inputs:
     &     ( plyr, ptop1, cldtot, cldcnv,                               &
     &       ix,nlay,                                                   &
!  ---  outputs:
     &       clds, mtop, mbot                                           &
     &     )


!
      return
!...................................
      end subroutine progcld1
!-----------------------------------

!-----------------------------------
      subroutine progcld3                                               &
!...................................

!  ---  inputs:
     &     ( plyr,plvl,tlyr,tvly,qlyr,qstl,rhly,clw,cnvw,cnvc,          &
     &       xlat,xlon,slmsk,                                           &
     &       ix, nlay, nlp1,                                            &
     &       deltaq,sup,kdt,me,                                         &
!  ---  outputs:
     &       clouds,clds,mtop,mbot                                      &
     &      )

! =================   subprogram documentation block   ================ !
!                                                                       !
! subprogram:    progcld3    computes cloud related quantities using    !
!   zhao/moorthi's prognostic cloud microphysics scheme.                !
!                                                                       !
! abstract:  this program computes cloud fractions from cloud           !
!   condensates, calculates liquid/ice cloud droplet effective radius,  !
!   and computes the low, mid, high, total and boundary layer cloud     !
!   fractions and the vertical indices of low, mid, and high cloud      !
!   top and base.  the three vertical cloud domains are set up in the   !
!   initial subroutine "cld_init".                                      !
!                                                                       !
! usage:         call progcld3                                          !
!                                                                       !
! subprograms called:   gethml                                          !
!                                                                       !
! attributes:                                                           !
!   language:   fortran 90                                              !
!   machine:    ibm-sp, sgi                                             !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
! input variables:                                                      !
!   plyr  (ix,nlay) : model layer mean pressure in mb (100pa)           !
!   plvl  (ix,nlp1) : model level pressure in mb (100pa)                !
!   tlyr  (ix,nlay) : model layer mean temperature in k                 !
!   tvly  (ix,nlay) : model layer virtual temperature in k              !
!   qlyr  (ix,nlay) : layer specific humidity in gm/gm                  !
!   qstl  (ix,nlay) : layer saturate humidity in gm/gm                  !
!   rhly  (ix,nlay) : layer relative humidity (=qlyr/qstl)              !
!   clw   (ix,nlay) : layer cloud condensate amount                     !
!   xlat  (ix)      : grid latitude in radians, default to pi/2 -> -pi/2!
!                     range, otherwise see in-line comment              !
!   xlon  (ix)      : grid longitude in radians  (not used)             !
!   slmsk (ix)      : sea/land mask array (sea:0,land:1,sea-ice:2)      !
!   ix              : horizontal dimention                              !
!   nlay,nlp1       : vertical layer/level dimensions                   !
!   cnvw  (ix,nlay) : layer convective cloud condensate                 !
!   cnvc  (ix,nlay) : layer convective cloud cover                      !
!   deltaq(ix,nlay) : half total water distribution width               !
!   sup             : supersaturation                                   ! 

!                                                                       !
! output variables:                                                     !
!   clouds(ix,nlay,nf_clds) : cloud profiles                            !
!      clouds(:,:,1) - layer total cloud fraction                       !
!      clouds(:,:,2) - layer cloud liq water path         (g/m**2)      !
!      clouds(:,:,3) - mean eff radius for liq cloud      (micron)      !
!      clouds(:,:,4) - layer cloud ice water path         (g/m**2)      !
!      clouds(:,:,5) - mean eff radius for ice cloud      (micron)      !
!      clouds(:,:,6) - layer rain drop water path         not assigned  !
!      clouds(:,:,7) - mean eff radius for rain drop      (micron)      !
!  *** clouds(:,:,8) - layer snow flake water path        not assigned  !
!      clouds(:,:,9) - mean eff radius for snow flake     (micron)      !
!  *** fu's scheme need to be normalized by snow density (g/m**3/1.0e6) !
!   clds  (ix,5)    : fraction of clouds for low, mid, hi, tot, bl      !
!   mtop  (ix,3)    : vertical indices for low, mid, hi cloud tops      !
!   mbot  (ix,3)    : vertical indices for low, mid, hi cloud bases     !
!                                                                       !
! module variables:                                                     !
!   ivflip          : control flag of vertical index direction          !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!   lcrick          : control flag for eliminating crick                !
!                     =t: apply layer smoothing to eliminate crick      !
!                     =f: do not apply layer smoothing                  !
!   lcnorm          : control flag for in-cld condensate                !
!                     =t: normalize cloud condensate                    !
!                     =f: not normalize cloud condensate                !
!                                                                       !
!  ====================    end of description    =====================  !
!
      implicit none

!  ---  inputs
      integer,  intent(in) :: ix, nlay, nlp1,kdt

      real (kind=kind_phys), dimension(:,:), intent(in) :: plvl, plyr,  &
     &       tlyr, tvly, qlyr, qstl, rhly, clw
!     &       tlyr, tvly, qlyr, qstl, rhly, clw, cnvw, cnvc
!      real (kind=kind_phys), dimension(:,:), intent(in) :: deltaq
      real (kind=kind_phys), dimension(:,:) :: deltaq, cnvw, cnvc
      real (kind=kind_phys) qtmp,qsc,rhs
      real (kind=kind_phys), intent(in) :: sup
      real (kind=kind_phys), parameter :: epsq = 1.0e-12

      real (kind=kind_phys), dimension(:),   intent(in) :: xlat, xlon,  &
     &       slmsk
      integer :: me  

!  ---  outputs
      real (kind=kind_phys), dimension(:,:,:), intent(out) :: clouds

      real (kind=kind_phys), dimension(:,:),   intent(out) :: clds

      integer,               dimension(:,:),   intent(out) :: mtop,mbot

!  ---  local variables:
      real (kind=kind_phys), dimension(ix,nlay) :: cldtot, cldcnv,      &
     &       cwp, cip, crp, csp, rew, rei, res, rer, delp, tem2d, clwf

      real (kind=kind_phys) :: ptop1(ix,nk_clds+1)

      real (kind=kind_phys) :: clwmin, clwm, clwt, onemrh, value,       &
     &       tem1, tem2, tem3

      integer :: i, k, id, nf

!
!===> ... begin here
!
      do nf=1,nf_clds
        do k=1,nlay
          do i=1,ix
            clouds(i,k,nf) = 0.0
          enddo
        enddo
      enddo
!     clouds(:,:,:) = 0.0

      do k = 1, nlay
        do i = 1, ix
          cldtot(i,k) = 0.0
          cldcnv(i,k) = 0.0
          cwp   (i,k) = 0.0
          cip   (i,k) = 0.0
          crp   (i,k) = 0.0
          csp   (i,k) = 0.0
          rew   (i,k) = reliq_def            ! default liq radius to 10 micron
          rei   (i,k) = reice_def            ! default ice radius to 50 micron
          rer   (i,k) = rrain_def            ! default rain radius to 1000 micron
          res   (i,k) = rsnow_def            ! default snow radius to 250 micron
          tem2d (i,k) = min( 1.0, max( 0.0, (con_ttp-tlyr(i,k))*0.05 ) )
          clwf(i,k)   = 0.0
        enddo
      enddo
!
      if ( lcrick ) then
        do i = 1, ix
          clwf(i,1)    = 0.75*clw(i,1)    + 0.25*clw(i,2)
          clwf(i,nlay) = 0.75*clw(i,nlay) + 0.25*clw(i,nlay-1)
        enddo
        do k = 2, nlay-1
          do i = 1, ix
            clwf(i,k) = 0.25*clw(i,k-1) + 0.5*clw(i,k) + 0.25*clw(i,k+1)
          enddo
        enddo
      else
        do k = 1, nlay
          do i = 1, ix
            clwf(i,k) = clw(i,k)
          enddo
        enddo
      endif

      if(kdt==1) then
        do k = 1, nlay
          do i = 1, ix
            deltaq(i,k) = (1.-0.95)*qstl(i,k)
          enddo
        enddo
      endif

!  ---  find top pressure for each cloud domain for given latitude
!       ptopc(k,i): top presure of each cld domain (k=1-4 are sfc,l,m,h;
!  ---  i=1,2 are low-lat (<45 degree) and pole regions)

      do id = 1, 4
        tem1 = ptopc(id,2) - ptopc(id,1)

        do i =1, ix
          tem2 = xlat(i) / con_pi        ! if xlat in pi/2 -> -pi/2 range
!         tem2 = 0.5 - xlat(i)/con_pi    ! if xlat in 0 -> pi range

          ptop1(i,id) = ptopc(id,1) + tem1*max( 0.0, 4.0*abs(tem2)-1.0 )
        enddo
      enddo

!  ---  compute liquid/ice condensate path in g/m**2

      if ( ivflip == 0 ) then          ! input data from toa to sfc
        do k = 1, nlay
          do i = 1, ix
            delp(i,k) = plvl(i,k+1) - plvl(i,k)
            clwt  = max(0.0,(clwf(i,k)+cnvw(i,k))) * gfac * delp(i,k)
            cip(i,k) = clwt * tem2d(i,k)
            cwp(i,k) = clwt - cip(i,k)
          enddo
        enddo
      else                             ! input data from sfc to toa
        do k = 1, nlay
          do i = 1, ix
            delp(i,k) = plvl(i,k) - plvl(i,k+1)
            clwt = max(0.0,(clwf(i,k)+cnvw(i,k))) * gfac * delp(i,k)
            cip(i,k) = clwt * tem2d(i,k)
            cwp(i,k) = clwt - cip(i,k)
          enddo
        enddo
      endif                            ! end_if_ivflip

!  ---  effective liquid cloud droplet radius over land

      do i = 1, ix
        if (nint(slmsk(i)) == 1) then
          do k = 1, nlay
            rew(i,k) = 5.0 + 5.0 * tem2d(i,k)
          enddo
        endif
      enddo

!  ---  layer cloud fraction

      if ( ivflip == 0 ) then              ! input data from toa to sfc
          do k = nlay, 1, -1
          do i = 1, ix
            tem1 = tlyr(i,k) - 273.16
            if(tem1 < con_thgni) then  ! for pure ice, has to be consistent with gscond
              qsc = sup * qstl(i,k)
              rhs = sup
            else
              qsc = qstl(i,k)
              rhs = 1.0
            endif
           if(rhly(i,k) >= rhs) then
             cldtot(i,k) = 1.0
           else
             qtmp = qlyr(i,k) + clwf(i,k) - qsc
             if(deltaq(i,k) > epsq) then
               if(qtmp < -deltaq(i,k) .or. clwf(i,k) < epsq) then
!               if(qtmp < -deltaq(i,k)) then
                 cldtot(i,k) = 0.0
               elseif(qtmp >= deltaq(i,k)) then
                 cldtot(i,k) = 1.0
               else
                 cldtot(i,k) = 0.5*qtmp/deltaq(i,k) + 0.5
                 cldtot(i,k) = max(cldtot(i,k),0.0)
                 cldtot(i,k) = min(cldtot(i,k),1.0)
               endif
             else
               if(qtmp.gt.0) then
                 cldtot(i,k) = 1.0
               else
                 cldtot(i,k) = 0.0
               endif
             endif
           endif
           cldtot(i,k) = cnvc(i,k)+(1-cnvc(i,k))*cldtot(i,k)
           cldtot(i,k) = max(cldtot(i,k),0.)
           cldtot(i,k) = min(cldtot(i,k),1.)
          enddo
          enddo
      else                                 ! input data from sfc to toa
          do k = 1, nlay
          do i = 1, ix
            tem1 = tlyr(i,k) - 273.16
            if(tem1 < con_thgni) then  ! for pure ice, has to be consistent with gscond
              qsc = sup * qstl(i,k)
              rhs = sup
            else
              qsc = qstl(i,k)
              rhs = 1.0
            endif
           if(rhly(i,k) >= rhs) then
             cldtot(i,k) = 1.0
           else
             qtmp = qlyr(i,k) + clwf(i,k) - qsc
             if(deltaq(i,k) > epsq) then
!              if(qtmp <= -deltaq(i,k) .or. cwmik < epsq) then
               if(qtmp <= -deltaq(i,k)) then
                 cldtot(i,k) = 0.0
               elseif(qtmp >= deltaq(i,k)) then
                 cldtot(i,k) = 1.0
               else
                 cldtot(i,k) = 0.5*qtmp/deltaq(i,k) + 0.5
                 cldtot(i,k) = max(cldtot(i,k),0.0)
                 cldtot(i,k) = min(cldtot(i,k),1.0)
               endif
             else
               if(qtmp > 0.) then
                 cldtot(i,k) = 1.0
               else
                 cldtot(i,k) = 0.0
               endif
             endif
           endif
           cldtot(i,k) = cnvc(i,k) + (1-cnvc(i,k))*cldtot(i,k)
           cldtot(i,k) = max(cldtot(i,k),0.)
           cldtot(i,k) = min(cldtot(i,k),1.)

          enddo
          enddo
      endif                                ! end_if_flip

      do k = 1, nlay
        do i = 1, ix
          if (cldtot(i,k) < climit) then
            cldtot(i,k) = 0.0
            cwp(i,k)    = 0.0
            cip(i,k)    = 0.0
            crp(i,k)    = 0.0
            csp(i,k)    = 0.0
          endif
        enddo
      enddo

      if ( lcnorm ) then
        do k = 1, nlay
          do i = 1, ix
            if (cldtot(i,k) >= climit) then
              tem1 = 1.0 / max(climit2, cldtot(i,k))
              cwp(i,k) = cwp(i,k) * tem1
              cip(i,k) = cip(i,k) * tem1
              crp(i,k) = crp(i,k) * tem1
              csp(i,k) = csp(i,k) * tem1
            endif
          enddo
        enddo
      endif

!  ---  effective ice cloud droplet radius

      do k = 1, nlay
        do i = 1, ix
          tem2 = tlyr(i,k) - con_ttp

          if (cip(i,k) > 0.0) then
!           tem3 = gord * cip(i,k) * (plyr(i,k)/delp(i,k)) / tvly(i,k)
            tem3 = gord * cip(i,k) * plyr(i,k) / (delp(i,k)*tvly(i,k))

            if (tem2 < -50.0) then
              rei(i,k) = (1250.0/9.917) * tem3 ** 0.109
            elseif (tem2 < -40.0) then
              rei(i,k) = (1250.0/9.337) * tem3 ** 0.08
            elseif (tem2 < -30.0) then
              rei(i,k) = (1250.0/9.208) * tem3 ** 0.055
            else
              rei(i,k) = (1250.0/9.387) * tem3 ** 0.031
            endif
!           rei(i,k)   = max(20.0, min(rei(i,k), 300.0))
!           rei(i,k)   = max(10.0, min(rei(i,k), 100.0))
            rei(i,k)   = max(10.0, min(rei(i,k), 150.0))
!           rei(i,k)   = max(5.0, min(rei(i,k), 130.0))
          endif
        enddo
      enddo

!
      do k = 1, nlay
        do i = 1, ix
          clouds(i,k,1) = cldtot(i,k)
          clouds(i,k,2) = cwp(i,k)
          clouds(i,k,3) = rew(i,k)
          clouds(i,k,4) = cip(i,k)
          clouds(i,k,5) = rei(i,k)
!         clouds(i,k,6) = 0.0
          clouds(i,k,7) = rer(i,k)
!         clouds(i,k,8) = 0.0
          clouds(i,k,9) = rei(i,k)
        enddo
      enddo


!  ---  compute low, mid, high, total, and boundary layer cloud fractions
!       and clouds top/bottom layer indices for low, mid, and high clouds.
!       the three cloud domain boundaries are defined by ptopc.  the cloud
!       overlapping method is defined by control flag 'iovr', which may
!       be different for lw and sw radiation programs.

      call gethml                                                       &
!  ---  inputs:
     &     ( plyr, ptop1, cldtot, cldcnv,                               &
     &       ix,nlay,                                                   &
!  ---  outputs:
     &       clds, mtop, mbot                                           &
     &     )


!
      return
!...................................
      end subroutine progcld3
!-----------------------------------

!-----------------------------------
      subroutine progcld2                                               &
!...................................

!  ---  inputs:
     &     ( plyr,plvl,tlyr,tvly,qlyr,qstl,rhly,clw,                    &
     &       xlat,xlon,slmsk, f_ice,f_rain,r_rime,flgmin,               &
     &       ix, nlay, nlp1,                                            &
!  ---  outputs:
     &       clouds,clds,mtop,mbot                                      &
     &      )

! =================   subprogram documentation block   ================ !
!                                                                       !
! subprogram:    progcld2    computes cloud related quantities using    !
!   ferrier's prognostic cloud microphysics scheme.                     !
!                                                                       !
! abstract:  this program computes cloud fractions from cloud           !
!   condensates, calculates liquid/ice cloud droplet effective radius,  !
!   and computes the low, mid, high, total and boundary layer cloud     !
!   fractions and the vertical indices of low, mid, and high cloud      !
!   top and base.  the three vertical cloud domains are set up in the   !
!   initial subroutine "cld_init".                                      !
!                                                                       !
! usage:         call progcld2                                          !
!                                                                       !
! subprograms called:   gethml                                          !
!                                                                       !
! attributes:                                                           !
!   language:   fortran 90                                              !
!   machine:    ibm-sp, sgi                                             !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
! input variables:                                                      !
!   plyr  (ix,nlay) : model layer mean pressure in mb (100pa)           !
!   plvl  (ix,nlp1) : model level pressure in mb (100pa)                !
!   tlyr  (ix,nlay) : model layer mean temperature in k                 !
!   tvly  (ix,nlay) : model layer virtual temperature in k              !
!   qlyr  (ix,nlay) : layer specific humidity in gm/gm                  !
!   qstl  (ix,nlay) : layer saturate humidity in gm/gm                  !
!   rhly  (ix,nlay) : layer relative humidity (=qlyr/qstl)              !
!   clw   (ix,nlay) : layer cloud condensate amount                     !
!   f_ice (ix,nlay) : fraction of layer cloud ice  (ferrier micro-phys) !
!   f_rain(ix,nlay) : fraction of layer rain water (ferrier micro-phys) !
!   r_rime(ix,nlay) : mass ratio of total ice to unrimed ice (>=1)      !
!   flgmin(ix)      : minimim large ice fraction                        !
!   xlat  (ix)      : grid latitude in radians, default to pi/2 -> -pi/2!
!                     range, otherwise see in-line comment              !
!   xlon  (ix)      : grid longitude in radians  (not used)             !
!   slmsk (ix)      : sea/land mask array (sea:0,land:1,sea-ice:2)      !
!   ix              : horizontal dimention                              !
!   nlay,nlp1       : vertical layer/level dimensions                   !
!                                                                       !
! output variables:                                                     !
!   clouds(ix,nlay,nf_clds) : cloud profiles                            !
!      clouds(:,:,1) - layer total cloud fraction                       !
!      clouds(:,:,2) - layer cloud liq water path         (g/m**2)      !
!      clouds(:,:,3) - mean eff radius for liq cloud      (micron)      !
!      clouds(:,:,4) - layer cloud ice water path         (g/m**2)      !
!      clouds(:,:,5) - mean eff radius for ice cloud      (micron)      !
!      clouds(:,:,6) - layer rain drop water path         (g/m**2)      !
!      clouds(:,:,7) - mean eff radius for rain drop      (micron)      !
!  *** clouds(:,:,8) - layer snow flake water path        (g/m**2)      !
!      clouds(:,:,9) - mean eff radius for snow flake     (micron)      !
!  *** fu's scheme need to be normalized by snow density (g/m**3/1.0e6) !
!   clds  (ix,5)    : fraction of clouds for low, mid, hi, tot, bl      !
!   mtop  (ix,3)    : vertical indices for low, mid, hi cloud tops      !
!   mbot  (ix,3)    : vertical indices for low, mid, hi cloud bases     !
!                                                                       !
! external module variables:                                            !
!   ivflip          : control flag of vertical index direction          !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!   lcrick          : control flag for eliminating crick                !
!                     =t: apply layer smoothing to eliminate crick      !
!                     =f: do not apply layer smoothing                  !
!   lcnorm          : control flag for in-cld condensate                !
!                     =t: normalize cloud condensate                    !
!                     =f: not normalize cloud condensate                !
!   lnoprec         : precip effect in radiation flag (ferrier scheme)  !
!                     =t: snow/rain has no impact on radiation          !
!                     =f: snow/rain has impact on radiation             !
!                                                                       !
!  ====================    end of description    =====================  !
!
      implicit none

!  ---  constants

!  ---  inputs
      integer,  intent(in) :: ix, nlay, nlp1

      real (kind=kind_phys), dimension(:,:), intent(in) :: plvl, plyr,  &
     &       tlyr, tvly, qlyr, qstl, rhly, clw, f_ice, f_rain, r_rime

      real (kind=kind_phys), dimension(:),   intent(in) :: xlat, xlon,  &
     &       slmsk
      real (kind=kind_phys), dimension(:), intent(in) :: flgmin

!  ---  outputs
      real (kind=kind_phys), dimension(:,:,:), intent(out) :: clouds

      real (kind=kind_phys), dimension(:,:),   intent(out) :: clds

      integer,               dimension(:,:),   intent(out) :: mtop,mbot

!  ---  local variables:
      real (kind=kind_phys), dimension(ix,nlay) :: cldtot, cldcnv,      &
     &       cwp, cip, crp, csp, rew, rei, res, rer, tem2d, clw2,       &
     &       qcwat, qcice, qrain, fcice, frain, rrime, rsden, clwf

      real (kind=kind_phys) :: ptop1(ix,nk_clds+1)

      real (kind=kind_phys) :: clwmin, clwm, clwt, onemrh, value,       &
     &       tem1, tem2, tem3

      integer :: i, k, id

!
!===> ... begin here
!
!     clouds(:,:,:) = 0.0

      do k = 1, nlay
        do i = 1, ix
          cldtot(i,k) = 0.0
          cldcnv(i,k) = 0.0
          cwp   (i,k) = 0.0
          cip   (i,k) = 0.0
          crp   (i,k) = 0.0
          csp   (i,k) = 0.0
          rew   (i,k) = reliq_def            ! default liq radius to 10 micron
          rei   (i,k) = reice_def            ! default ice radius to 50 micron
          rer   (i,k) = rrain_def            ! default rain radius to 1000 micron
          res   (i,k) = rsnow_def            ! default snow radius to 250 micron
          fcice (i,k) = max(0.0, min(1.0, f_ice(i,k)))
          frain (i,k) = max(0.0, min(1.0, f_rain(i,k)))
          rrime (i,k) = max(1.0, r_rime(i,k))
          tem2d (i,k) = tlyr(i,k) - con_t0c
        enddo
      enddo
!
      if ( lcrick ) then
        do i = 1, ix
          clwf(i,1)    = 0.75*clw(i,1)    + 0.25*clw(i,2)
          clwf(i,nlay) = 0.75*clw(i,nlay) + 0.25*clw(i,nlay-1)
        enddo
        do k = 2, nlay-1
          do i = 1, ix
            clwf(i,k) = 0.25*clw(i,k-1) + 0.5*clw(i,k) + 0.25*clw(i,k+1)
          enddo
        enddo
      else
        do k = 1, nlay
          do i = 1, ix
            clwf(i,k) = clw(i,k)
          enddo
        enddo
      endif

!  ---  find top pressure for each cloud domain for given latitude
!       ptopc(k,i): top presure of each cld domain (k=1-4 are sfc,l,m,h;
!  ---  i=1,2 are low-lat (<45 degree) and pole regions)

      do id = 1, 4
        tem1 = ptopc(id,2) - ptopc(id,1)

        do i =1, ix
          tem2 = xlat(i) / con_pi        ! if xlat in pi/2 -> -pi/2 range
!         tem2 = 0.5 - xlat(i)/con_pi    ! if xlat in 0 -> pi range

          ptop1(i,id) = ptopc(id,1) + tem1*max( 0.0, 4.0*abs(tem2)-1.0 )
        enddo
      enddo

!  ---  separate cloud condensate into liquid, ice, and rain types, and
!       save the liquid+ice condensate in array clw2 for later
!       calculation of cloud fraction

      do k = 1, nlay
        do i = 1, ix
          if (tem2d(i,k) > -40.0) then
            qcice(i,k) = clwf(i,k) * fcice(i,k)
            tem1       = clwf(i,k) - qcice(i,k)
            qrain(i,k) = tem1 * frain(i,k)
            qcwat(i,k) = tem1 - qrain(i,k)
            clw2 (i,k) = qcwat(i,k) + qcice(i,k)
          else
            qcice(i,k) = clwf(i,k)
            qrain(i,k) = 0.0
            qcwat(i,k) = 0.0
            clw2 (i,k) = clwf(i,k)
          endif
        enddo
      enddo

      call  rsipath2                                                    &
!  ---  inputs:
     &     ( plyr, plvl, tlyr, qlyr, qcwat, qcice, qrain, rrime,        &
     &       ix, nlay, ivflip, flgmin,                                  &
!  ---  outputs:
     &       cwp, cip, crp, csp, rew, rer, res, rsden                   &
     &     )


      if ( ivflip == 0 ) then          ! input data from toa to sfc
        do k = 1, nlay
          do i = 1, ix
            tem2d(i,k) = (con_g * plyr(i,k))                            &
     &                 / (con_rd* (plvl(i,k+1) - plvl(i,k)))
          enddo
        enddo
      else                             ! input data from sfc to toa
        do k = 1, nlay
          do i = 1, ix
            tem2d(i,k) = (con_g * plyr(i,k))                            &
     &                 / (con_rd* (plvl(i,k) - plvl(i,k+1)))
          enddo
        enddo
      endif                            ! end_if_ivflip

!  ---  layer cloud fraction

      if ( ivflip == 0 ) then              ! input data from toa to sfc

        clwmin = 0.0
        do k = nlay, 1, -1
        do i = 1, ix
!         clwt = 1.0e-6 * (plyr(i,k)*0.001)
          clwt = 2.0e-6 * (plyr(i,k)*0.001)

          if (clw2(i,k) > clwt) then
            onemrh= max( 1.e-10, 1.0-rhly(i,k) )
            clwm  = clwmin / max( 0.01, plyr(i,k)*0.001 )

            tem1  = min(max((onemrh*qstl(i,k))**0.49,0.0001),1.0)    !jhan
            tem1  = 100.0 / tem1

!           tem1  = min(max(sqrt(sqrt(onemrh*qstl(i,k))),0.0001),1.0)
!           tem1  = 2000.0 / tem1
!
!           tem1  = min(max(sqrt(sqrt(onemrh*qstl(i,k))),0.0001),1.0)
!           tem1  = 2200.0 / tem1
!           tem1  = 2400.0 / tem1
!           tem1  = 2500.0 / tem1
!           tem1  = min(max(sqrt(onemrh*qstl(i,k)),0.0001),1.0)
!           tem1  = 2000.0 / tem1
!           tem1  = 1000.0 / tem1
!           tem1  = 100.0 / tem1

            value = max( min( tem1*(clw2(i,k)-clwm), 50.0 ), 0.0 )
            tem2  = sqrt( sqrt(rhly(i,k)) )

            cldtot(i,k) = max( tem2*(1.0-exp(-value)), 0.0 )
          endif
        enddo
        enddo

      else                                 ! input data from sfc to toa

        clwmin = 0.0e-6
        do k = 1, nlay
        do i = 1, ix
!         clwt = 1.0e-6 * (plyr(i,k)*0.001)
          clwt = 2.0e-6 * (plyr(i,k)*0.001)

          if (clw2(i,k) > clwt) then
            onemrh= max( 1.e-10, 1.0-rhly(i,k) )
            clwm  = clwmin / max( 0.01, plyr(i,k)*0.001 )

            tem1  = min(max((onemrh*qstl(i,k))**0.49,0.0001),1.0)   !jhan
            tem1  = 100.0 / tem1

!           tem1  = min(max(sqrt(sqrt(onemrh*qstl(i,k))),0.0001),1.0)
!           tem1  = 2000.0 / tem1
!
!           tem1  = min(max(sqrt(sqrt(onemrh*qstl(i,k))),0.0001),1.0)
!           tem1  = 2200.0 / tem1
!           tem1  = 2400.0 / tem1
!           tem1  = 2500.0 / tem1
!           tem1  = min(max(sqrt(onemrh*qstl(i,k)),0.0001),1.0)
!           tem1  = 2000.0 / tem1
!           tem1  = 1000.0 / tem1
!           tem1  = 100.0 / tem1

            value = max( min( tem1*(clw2(i,k)-clwm), 50.0 ), 0.0 )
            tem2  = sqrt( sqrt(rhly(i,k)) )

            cldtot(i,k) = max( tem2*(1.0-exp(-value)), 0.0 )
          endif
        enddo
        enddo

      endif                                ! end_if_flip

      do k = 1, nlay
        do i = 1, ix
          if (cldtot(i,k) < climit) then
            cldtot(i,k) = 0.0
            cwp(i,k)    = 0.0
            cip(i,k)    = 0.0
            crp(i,k)    = 0.0
            csp(i,k)    = 0.0
          endif
        enddo
      enddo

!     when lnoprec = .true. snow/rain has no impact on radiation
      if ( lnoprec ) then
        do k = 1, nlay
          do i = 1, ix
            crp(i,k) = 0.0
            csp(i,k) = 0.0
          enddo
        enddo
      endif
!
      if ( lcnorm ) then
        do k = 1, nlay
          do i = 1, ix
            if (cldtot(i,k) >= climit) then
              tem1 = 1.0 / max(climit2, cldtot(i,k))
              cwp(i,k) = cwp(i,k) * tem1
              cip(i,k) = cip(i,k) * tem1
              crp(i,k) = crp(i,k) * tem1
              csp(i,k) = csp(i,k) * tem1
            endif
          enddo
        enddo
      endif

!  ---  effective ice cloud droplet radius

      do k = 1, nlay
        do i = 1, ix
          tem1 = tlyr(i,k) - con_ttp
          tem2 = cip(i,k)

          if (tem2 > 0.0) then
            tem3 = tem2d(i,k) * tem2 / tvly(i,k)

            if (tem1 < -50.0) then
              rei(i,k) = (1250.0/9.917) * tem3 ** 0.109
            elseif (tem1 < -40.0) then
              rei(i,k) = (1250.0/9.337) * tem3 ** 0.08
            elseif (tem1 < -30.0) then
              rei(i,k) = (1250.0/9.208) * tem3 ** 0.055
            else
              rei(i,k) = (1250.0/9.387) * tem3 ** 0.031
            endif

!           if (lprnt .and. k == l) print *,' reil=',rei(i,k),' icec=', &
!    &        icec,' cip=',cip(i,k),' tem=',tem,' delt=',delt

            rei(i,k)   = max(10.0, min(rei(i,k), 300.0))
!           rei(i,k)   = max(20.0, min(rei(i,k), 300.0))
!!!!        rei(i,k)   = max(30.0, min(rei(i,k), 300.0))
!           rei(i,k)   = max(50.0, min(rei(i,k), 300.0))
!           rei(i,k)   = max(100.0, min(rei(i,k), 300.0))
          endif
        enddo
      enddo
!
      do k = 1, nlay
        do i = 1, ix
          clouds(i,k,1) = cldtot(i,k)
          clouds(i,k,2) = cwp(i,k)
          clouds(i,k,3) = rew(i,k)
          clouds(i,k,4) = cip(i,k)
          clouds(i,k,5) = rei(i,k)
          clouds(i,k,6) = crp(i,k)
          clouds(i,k,7) = rer(i,k)
!         clouds(i,k,8) = csp(i,k)               !ncar scheme
          clouds(i,k,8) = csp(i,k) * rsden(i,k)  !fu's scheme
          clouds(i,k,9) = rei(i,k)
        enddo
      enddo


!  ---  compute low, mid, high, total, and boundary layer cloud fractions
!       and clouds top/bottom layer indices for low, mid, and high clouds.
!       the three cloud domain boundaries are defined by ptopc.  the cloud
!       overlapping method is defined by control flag 'iovr', which may
!       be different for lw and sw radiation programs.

      call gethml                                                       &
!  ---  inputs:
     &     ( plyr, ptop1, cldtot, cldcnv,                               &
     &       ix,nlay,                                                   &
!  ---  outputs:
     &       clds, mtop, mbot                                           &
     &     )


!
      return
!...................................
      end subroutine progcld2
!-----------------------------------


!-----------------------------------
      subroutine diagcld1                                               &
!...................................

!  ---  inputs:
     &     ( plyr,plvl,tlyr,rhly,vvel,cv,cvt,cvb,                       &
     &       xlat,xlon,slmsk,                                           &
     &       ix, nlay, nlp1,                                            &
!  ---  outputs:
     &       clouds,clds,mtop,mbot                                      &
     &      )

! =================   subprogram documentation block   ================ !
!                                                                       !
! subprogram:    diagcld1    computes cloud fractions for radiation     !
!   calculations.                                                       !
!                                                                       !
! abstract:  clouds are diagnosed from layer relative humidity, and     !
!   estimate cloud optical depth from temperature and layer thickness.  !
!   then computes the low, mid, high, total and boundary layer cloud    !
!   fractions and the vertical indices of low, mid, and high cloud top  !
!   and base.  the three vertical cloud domains are set up in the       !
!   initial subroutine "cld_init".                                      !
!                                                                       !
! usage:         call diagcld1                                          !
!                                                                       !
! subprograms called:   gethml                                          !
!                                                                       !
! attributes:                                                           !
!   language:   fortran 90                                              !
!   machine:    ibm-sp, sgi                                             !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
! input variables:                                                      !
!   plyr  (ix,nlay) : model layer mean pressure in mb (100pa)           !
!   plvl  (ix,nlp1) : model level pressure in mb (100pa)                !
!   tlyr  (ix,nlay) : model layer mean temperature in k                 !
!   rhly  (ix,nlay) : layer relative humidity                           !
!   vvel  (ix,nlay) : layer mean vertical velocity in mb/sec            !
!   clw   (ix,nlay) : layer cloud condensate amount         (not used)  !
!   xlat  (ix)      : grid latitude in radians, default to pi/2 -> -pi/2!
!                     range, otherwise see in-line comment              !
!   xlon  (ix)      : grid longitude in radians, ok for both 0->2pi or  !
!                     -pi -> +pi ranges                                 !
!   slmsk (ix)      : sea/land mask array (sea:0,land:1,sea-ice:2)      !
!   cv    (ix)      : fraction of convective cloud                      !
!   cvt, cvb (ix)   : conv cloud top/bottom pressure in mb              !
!   ix              : horizontal dimention                              !
!   nlay,nlp1       : vertical layer/level dimensions                   !
!                                                                       !
! output variables:                                                     !
!   clouds(ix,nlay,nf_clds) : cloud profiles                            !
!      clouds(:,:,1) - layer total cloud fraction                       !
!      clouds(:,:,2) - layer cloud optical depth                        !
!      clouds(:,:,3) - layer cloud single scattering albedo             !
!      clouds(:,:,4) - layer cloud asymmetry factor                     !
!   clds  (ix,5)    : fraction of clouds for low, mid, hi, tot, bl      !
!   mtop  (ix,3)    : vertical indices for low, mid, hi cloud tops      !
!   mbot  (ix,3)    : vertical indices for low, mid, hi cloud bases     !
!                                                                       !
! external module variables:                                            !
!   ivflip          : control flag of vertical index direction          !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!                                                                       !
!  ====================    end of description    =====================  !
!
      implicit none

!  ---  inputs
      integer,  intent(in) :: ix, nlay, nlp1

      real (kind=kind_phys), dimension(:,:), intent(in) :: plvl, plyr,  &
     &       tlyr, rhly, vvel

      real (kind=kind_phys), dimension(:),   intent(in) :: xlat, xlon,  &
     &       slmsk, cv, cvt, cvb

!  ---  outputs
      real (kind=kind_phys), dimension(:,:,:), intent(out) :: clouds

      real (kind=kind_phys), dimension(:,:),   intent(out) :: clds

      integer,               dimension(:,:),   intent(out) :: mtop,mbot

!  ---  local variables:
      real (kind=kind_phys), dimension(ix,nlay) :: cldtot, cldcnv,      &
     &       cldtau, taufac, dthdp, tem2d

      real (kind=kind_phys) :: ptop1(ix,nk_clds+1)

      real (kind=kind_phys) :: cr1, cr2, crk, pval, cval, omeg, value,  &
     &       tem1, tem2

      integer, dimension(ix):: idom, kcut

!  ---  for rh-cl calculation
      real (kind=kind_phys) :: xlatdg, xlondg, xlnn, xlss, xrgt, xlft,  &
     &       rhcla(nbin,nlon,mcld,nseal),  rhcld(ix,nbin,mcld)

      integer :: ireg, ib, ic, id, id1, il, is, nhalf

      integer :: i, j, k, klowt
!     integer :: klowb

      logical :: notstop

!
!===> ... begin here
!
      clouds(:,:,:) = 0.0

      tem1 = 180.0 / con_pi

      lab_do_i_ix : do i = 1, ix

        xlatdg = xlat(i) * tem1                    ! if xlat in pi/2 -> -pi/2 range
!       xlatdg = 90.0 - xlat(i)*tem1               ! if xlat in 0 -> pi range

        xlondg = xlon(i) * tem1
        if (xlondg < 0.0) xlondg = xlondg + 360.0  ! if in -180->180, chg to 0->360 range

        ireg = 4

!  ---  get rh-cld relation for this lat

        lab_do_j : do j = 1, 3
          if (xlatdg > xlabdy(j)) then
            ireg = j
            exit lab_do_j
          endif
        enddo  lab_do_j

        do is = 1, nseal
          do ic = 1, mcld
            do il = 1, nlon
              do ib = 1, nbin
                rhcla(ib,il,ic,is) = rhcl(ib,il,ireg,ic,is)
              enddo
            enddo
          enddo
        enddo

!  ---  linear transition between latitudinal regions...
        do j = 1, 3
          xlnn = xlabdy(j) + xlim
          xlss = xlabdy(j) - xlim

          if (xlatdg < xlnn .and. xlatdg > xlss) then
            do is = 1, nseal
              do ic = 1, mcld
                do il = 1, nlon
                  do ib = 1, nbin
                    rhcla(ib,il,ic,is) = rhcl(ib,il,j+1,ic,is)          &
     &                + (rhcl(ib,il,j,ic,is)-rhcl(ib,il,j+1,ic,is))     &
     &                * (xlatdg-xlss) / (xlnn-xlss)
                  enddo
                enddo
              enddo
            enddo
          endif

        enddo        ! end_j_loop

!  ---  get rh-cld relationship for each grid point, interpolating
!       longitudinally between regions if necessary..

        if (slmsk(i) < 1.0) then
          is = 2
        else
          is = 1
        endif

!  ---  which hemisphere (e,w)

        if (xlondg > 180.e0) then
          il = 2
        else
          il = 1
        endif

        do ic = 1, mcld
          do ib = 1, nbin
            rhcld(i,ib,ic) = rhcla(ib,il,ic,is)
          enddo

          lab_do_k : do k = 1, 3
            tem2 = abs(xlondg - xlobdy(k))

            if (tem2 < xlim) then
              id = il
              id1= id + 1
              if (id1 > nlon) id1 = 1

              xlft = xlobdy(k) - xlim
              xrgt = xlobdy(k) + xlim

              do ib = 1, nbin
                rhcld(i,ib,ic) = rhcla(ib,id1,ic,is)                    &
     &            + (rhcla(ib,id,ic,is) - rhcla(ib,id1,ic,is))          &
     &            * (xlondg-xrgt)/(xlft-xrgt)
              enddo
              exit lab_do_k
            endif

          enddo  lab_do_k

        enddo   ! end_do_ic_loop
      enddo  lab_do_i_ix

!  ---  find top pressure for each cloud domain

      do j = 1, 4
        tem1 = ptopc(j,2) - ptopc(j,1)

        do i = 1, ix
          tem2 = xlat(i) / con_pi        ! if xlat in pi/2 -> -pi/2 range
!         tem2 = 0.5 - xlat(i)/con_pi    ! if xlat in 0 -> pi range

          ptop1(i,j) = ptopc(j,1) + tem1*max( 0.0, 4.0*abs(tem2)-1.0 )
        enddo
      enddo

!  ---  stratiform cloud optical depth

      do k = 1, nlay
        do i = 1, ix
          tem1 = tlyr(i,k) - con_ttp
          if (tem1 <= -10.0) then
            cldtau(i,k) = max( 0.1e-3, 2.0e-6*(tem1+82.5)**2 )
          else
            cldtau(i,k) = min( 0.08, 6.949e-3*tem1+0.08 )
          endif
        enddo
      enddo

!  ---  potential temperature and its lapse rate

      do k = 1, nlay
        do i = 1, ix
          cldtot(i,k) = 0.0
          cldcnv(i,k) = 0.0
          tem1        = (plyr(i,k)*0.001) ** (-con_rocp)
          tem2d(i,k)  = tem1 * tlyr(i,k)
        enddo
      enddo

      do k = 1, nlay-1
        do i = 1, ix
          dthdp(i,k) = (tem2d(i,k+1)-tem2d(i,k))/(plyr(i,k+1)-plyr(i,k))
        enddo
      enddo
!
!===> ... diagnostic method to find cloud amount cldtot, cldcnv
!

      if ( ivflip == 0 ) then                 ! input data from toa to sfc

!  ---  find the lowest low cloud top sigma level, computed for each lat cause
!       domain definition changes with latitude...

!       klowb = 1
        klowt = 1
        do k = 1, nlay
          do i = 1, ix
!           if (plvl(i,k) < ptop1(i,2))  klowb = k
            if (plvl(i,k) < ptop1(i,2))  klowt = max(klowt,k)
            taufac(i,k) = plvl(i,k+1) - plvl(i,k)
          enddo
        enddo

        do i = 1, ix

!  ---  find the stratosphere cut off layer for high cloud (about 250mb).
!       it is assumed to be above the layer with dthdp less than -0.25 in
!       the high cloud domain

          kcut(i) = 2
          lab_do_kcut0 : do k = klowt-1, 2, -1
            if (plyr(i,k) <= ptop1(i,3) .and.                           &
     &          dthdp(i,k) < -0.25e0) then
              kcut(i) = k
              exit lab_do_kcut0
            endif
          enddo  lab_do_kcut0

!  ---  put convective cloud into 'cldcnv', no merge at this point..

          if (cv(i) >= climit .and. cvt(i) < cvb(i)) then
            id  = nlay
            id1 = nlay

            lab_do_k_cvt0 : do k = 2, nlay
              if (cvt(i) <= plyr(i,k)) then
                id = k - 1
                exit lab_do_k_cvt0
              endif
            enddo  lab_do_k_cvt0

            lab_do_k_cvb0 : do k = nlay-1, 1, -1
              if (cvb(i) >= plyr(i,k)) then
                id1 = k + 1
                exit lab_do_k_cvb0
              endif
            enddo  lab_do_k_cvb0

            tem1 = plyr(i,id1) - plyr(i,id)
            do k = id, id1
              cldcnv(i,k) = cv(i)
              taufac(i,k) = taufac(i,k) * max( 0.25, 1.0-0.125*tem1 )
              cldtau(i,k) = 0.06
            enddo
          endif

        enddo                ! end_do_i_loop

!  ---  calculate stratiform cloud and put into array 'cldtot' using
!       the cloud-rel.humidity relationship from table look-up..where
!       tables obtained using k.mitchell frequency distribution tuning
!bl       (observations are daily means from us af rtneph).....k.a.c.
!bl       tables created without lowest 10 percent of atmos.....k.a.c.
!      (observations are synoptic using -6,+3 window from rtneph)
!       tables are created with lowest 10-percent-of-atmos, and are
!  ---  now used..  25 october 1995 ... kac.

        do k = nlay-1, 2, -1

          if (k < llyr) then
            do i = 1, ix
              idom(i) = 0
            enddo

            do i = 1, ix
              lab_do_ic0 : do ic = 2, 4
                if(plyr(i,k) >= ptop1(i,ic)) then
                  idom(i) = ic
                  exit lab_do_ic0
                endif
              enddo  lab_do_ic0
            enddo
          else
            do i = 1, ix
              idom(i) = 1
            enddo
          endif

          do i = 1, ix
            id = idom(i)
            nhalf = (nbin + 1) / 2

            if (id <= 0 .or. k < kcut(i)) then
              cldtot(i,k) = 0.0
            elseif (rhly(i,k) <= rhcld(i,1,id)) then
              cldtot(i,k) = 0.0
            elseif (rhly(i,k) >= rhcld(i,nbin,id)) then
              cldtot(i,k) = 1.0
            else
              ib = nhalf
              crk = rhly(i,k)

              notstop = .true.
              do while ( notstop )
                nhalf = (nhalf + 1) / 2
                cr1 = rhcld(i,ib,  id)
                cr2 = rhcld(i,ib+1,id)

                if (crk <= cr1) then
                  ib = max( ib-nhalf, 1 )
                elseif (crk > cr2) then
                  ib = min( ib+nhalf, nbin-1 )
                else
                  cldtot(i,k) = 0.01 * (ib + (crk - cr1)/(cr2 - cr1))
                  notstop = .false.
                endif
              enddo      ! end_do_while
            endif
          enddo          ! end_do_i_loop

        enddo            ! end_do_k_loop

! --- vertical velocity adjustment on low clouds

        value = vvcld1 - vvcld2
        do k = klowt, llyr+1
          do i = 1, ix

            omeg = vvel(i,k)
            cval = cldtot(i,k)
            pval = plyr(i,k)

! --- vertical velocity adjustment on low clouds

            if (cval >= climit .and. pval >= ptop1(i,2)) then
              if (omeg >= vvcld1) then
                cldtot(i,k) = 0.0
              elseif (omeg > vvcld2) then
                tem1 = (vvcld1 - omeg) / value
                cldtot(i,k) = cldtot(i,k) * sqrt(tem1)
              endif
            endif

          enddo     ! end_do_i_loop
        enddo       ! end_do_k_loop

      else                                    ! input data from sfc to toa

!  ---  find the lowest low cloud top sigma level, computed for each lat cause
!       domain definition changes with latitude...

!       klowb = nlay
        klowt = nlay
        do k = nlay, 1, -1
          do i = 1, ix
!           if (plvl(i,k) < ptop1(i,2))  klowb = k
            if (plvl(i,k) < ptop1(i,2))  klowt = min(klowt,k)
            taufac(i,k) = plvl(i,k) - plvl(i,k+1)       ! dp for later cal cldtau use
          enddo
        enddo

        do i = 1, ix

!  ---  find the stratosphere cut off layer for high cloud (about 250mb).
!       it is assumed to be above the layer with dthdp less than -0.25 in
!       the high cloud domain

          kcut(i) = nlay - 1
          lab_do_kcut1 : do k = klowt+1, nlay-1
            if (plyr(i,k) <= ptop1(i,3) .and.                           &
     &          dthdp(i,k) < -0.25e0) then
              kcut(i) = k
              exit lab_do_kcut1
            endif
          enddo  lab_do_kcut1

!  ---  put convective cloud into 'cldcnv', no merge at this point..

          if (cv(i) >= climit .and. cvt(i) < cvb(i)) then
            id  = 1
            id1 = 1

            lab_do_k_cvt : do k = nlay-1, 1, -1
              if (cvt(i) <= plyr(i,k)) then
                id = k + 1
                exit lab_do_k_cvt
              endif
            enddo  lab_do_k_cvt

            lab_do_k_cvb : do k = 2, nlay
              if (cvb(i) >= plyr(i,k)) then
                id1 = k - 1
                exit lab_do_k_cvb
              endif
            enddo  lab_do_k_cvb

            tem1 = plyr(i,id1) - plyr(i,id)
            do k = id1, id
              cldcnv(i,k) = cv(i)
              taufac(i,k) = taufac(i,k) * max( 0.25, 1.0-0.125*tem1 )
              cldtau(i,k) = 0.06
            enddo
          endif

        enddo     ! end_do_i_loop

!  ---  calculate stratiform cloud and put into array 'cldtot' using
!       the cloud-rel.humidity relationship from table look-up..where
!       tables obtained using k.mitchell frequency distribution tuning
!bl       (observations are daily means from us af rtneph).....k.a.c.
!bl       tables created without lowest 10 percent of atmos.....k.a.c.
!      (observations are synoptic using -6,+3 window from rtneph)
!       tables are created with lowest 10-percent-of-atmos, and are
!  ---  now used..  25 october 1995 ... kac.

        do k = 2, nlay-1

          if (k > llyr) then
            do i = 1, ix
              idom(i) = 0
            enddo

            do i = 1, ix
              lab_do_ic1 : do ic = 2, 4
                if(plyr(i,k) >= ptop1(i,ic)) then
                  idom(i) = ic
                  exit lab_do_ic1
                endif
              enddo  lab_do_ic1
            enddo
          else
            do i = 1, ix
              idom(i) = 1
            enddo
          endif

          do i = 1, ix
            id = idom(i)
            nhalf = (nbin + 1) / 2

            if (id <= 0 .or. k > kcut(i)) then
              cldtot(i,k) = 0.0
            elseif (rhly(i,k) <= rhcld(i,1,id)) then
              cldtot(i,k) = 0.0
            elseif (rhly(i,k) >= rhcld(i,nbin,id)) then
              cldtot(i,k) = 1.0
            else
              ib = nhalf
              crk = rhly(i,k)

              notstop = .true.
              do while ( notstop )
                nhalf = (nhalf + 1) / 2
                cr1 = rhcld(i,ib,  id)
                cr2 = rhcld(i,ib+1,id)

                if (crk <= cr1) then
                  ib = max( ib-nhalf, 1 )
                elseif (crk > cr2) then
                  ib = min( ib+nhalf, nbin-1 )
                else
                  cldtot(i,k) = 0.01 * (ib + (crk - cr1)/(cr2 - cr1))
                  notstop = .false.
                endif
              enddo      ! end_do_while
            endif
          enddo          ! end_do_i_loop

        enddo            ! end_do_k_loop

! --- vertical velocity adjustment on low clouds

        value = vvcld1 - vvcld2
        do k = llyr-1, klowt
          do i = 1, ix

            omeg = vvel(i,k)
            cval = cldtot(i,k)
            pval = plyr(i,k)

! --- vertical velocity adjustment on low clouds

            if (cval >= climit .and. pval >= ptop1(i,2)) then
              if (omeg >= vvcld1) then
                cldtot(i,k) = 0.0
              elseif (omeg > vvcld2) then
                tem1 = (vvcld1 - omeg) / value
                cldtot(i,k) = cldtot(i,k) * sqrt(tem1)
              endif
            endif

          enddo     ! end_do_i_loop
        enddo       ! end_do_k_loop

      endif                                   ! end_if_ivflip

!  ---  diagnostic cloud optical depth
!     cldtau = cldtau * taufac

      where (cldtot < climit)
        cldtot = 0.0
      endwhere
      where (cldcnv < climit)
        cldcnv = 0.0
      endwhere

      where (cldtot < climit .and. cldcnv < climit)
        cldtau = 0.0
      endwhere

      do k = 1, nlay
        do i = 1, ix
          clouds(i,k,1) = max(cldtot(i,k), cldcnv(i,k))
          clouds(i,k,2) = cldtau(i,k) * taufac(i,k)
          clouds(i,k,3) = cldssa_def
          clouds(i,k,4) = cldasy_def
        enddo
      enddo

!
!===> ... compute low, mid, high, total, and boundary layer cloud fractions
!         and clouds top/bottom layer indices for low, mid, and high clouds.
!         the three cloud domain boundaries are defined by ptopc.  the cloud
!         overlapping method is defined by control flag 'iovr', which may
!         be different for lw and sw radiation programs.

      call gethml                                                       &
!  ---  inputs:
     &     ( plyr, ptop1, cldtot, cldcnv,                               &
     &       ix, nlay,                                                  &
!  ---  outputs:
     &       clds, mtop, mbot                                           &
     &     )

!
      return
!...................................
      end subroutine diagcld1
!-----------------------------------


!-----------------------------------                                    !
      subroutine gethml                                                 &
!...................................                                    !

!  ---  inputs:
     &     ( plyr, ptop1, cldtot, cldcnv,                               &
     &       ix, nlay,                                                  &
!  ---  outputs:
     &       clds, mtop, mbot                                           &
     &     )

!  ===================================================================  !
!                                                                       !
! abstract: compute high, mid, low, total, and boundary cloud fractions !
!   and cloud top/bottom layer indices for model diagnostic output.     !
!   the three cloud domain boundaries are defined by ptopc.  the cloud  !
!   overlapping method is defined by control flag 'iovr', which is also !
!   used by lw and sw radiation programs.                               !
!                                                                       !
! usage:         call gethml                                            !
!                                                                       !
! subprograms called:  none                                             !
!                                                                       !
! attributes:                                                           !
!   language:   fortran 90                                              !
!   machine:    ibm-sp, sgi                                             !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
! input variables:                                                      !
!   plyr  (ix,nlay) : model layer mean pressure in mb (100pa)           !
!   ptop1 (ix,4)    : pressure limits of cloud domain interfaces        !
!                     (sfc,low,mid,high) in mb (100pa)                  !
!   cldtot(ix,nlay) : total or straiform cloud profile in fraction      !
!   cldcnv(ix,nlay) : convective cloud (for diagnostic scheme only)     !
!   ix              : horizontal dimention                              !
!   nlay            : vertical layer dimensions                         !
!                                                                       !
! output variables:                                                     !
!   clds  (ix,5)    : fraction of clouds for low, mid, hi, tot, bl      !
!   mtop  (ix,3)    : vertical indices for low, mid, hi cloud tops      !
!   mbot  (ix,3)    : vertical indices for low, mid, hi cloud bases     !
!                                                                       !
! external module variables:  (in physpara)                             !
!   ivflip          : control flag of vertical index direction          !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!                                                                       !
! internal module variables:                                            !
!   iovr            : control flag for cloud overlap                    !
!                     =0 random overlapping clouds                      !
!                     =1 max/ran overlapping clouds                     !
!                                                                       !
!                                                                       !
!  ====================    end of description    =====================  !
!
      implicit none!

!  ---  inputs:
      integer, intent(in) :: ix, nlay

      real (kind=kind_phys), dimension(:,:), intent(in) :: plyr, ptop1, &
     &       cldtot, cldcnv

!  ---  outputs
      real (kind=kind_phys), dimension(:,:), intent(out) :: clds

      integer,               dimension(:,:), intent(out) :: mtop, mbot

!  ---  local variables:
      real (kind=kind_phys) :: cl1(ix), cl2(ix)
      real (kind=kind_phys) :: pcur, pnxt, ccur, cnxt

      integer, dimension(ix):: idom, kbt1, kth1, kbt2, kth2
      integer :: i, k, id, id1, kstr, kend, kinc

!
!===> ... begin here
!
      clds(:,:) = 0.0

      do i = 1, ix
        cl1(i) = 1.0
        cl2(i) = 1.0
      enddo

!  ---  total and bl clouds, where cl1, cl2 are fractions of clear-sky view
!       layer processed from surface and up

      if ( ivflip == 0 ) then                   ! input data from toa to sfc
        kstr = nlay
        kend = 1
        kinc = -1
      else                                      ! input data from sfc to toa
        kstr = 1
        kend = nlay
        kinc = 1
      endif                                     ! end_if_ivflip

      if ( iovr == 0 ) then                     ! random overlap

        do k = kstr, kend, kinc
          do i = 1, ix
            ccur = min( ovcst, max( cldtot(i,k), cldcnv(i,k) ))
            if (ccur >= climit) cl1(i) = cl1(i) * (1.0 - ccur)
          enddo

          if (k == llyr) then
            do i = 1, ix
              clds(i,5) = 1.0 - cl1(i)          ! save bl cloud
            enddo
          endif
        enddo

        do i = 1, ix
          clds(i,4) = 1.0 - cl1(i)              ! save total cloud
        enddo

      else                                      ! max/ran overlap

        do k = kstr, kend, kinc
          do i = 1, ix
            ccur = min( ovcst, max( cldtot(i,k), cldcnv(i,k) ))
            if (ccur >= climit) then             ! cloudy layer
              cl2(i) = min( cl2(i), (1.0 - ccur) )
            else                                ! clear layer
              cl1(i) = cl1(i) * cl2(i)
              cl2(i) = 1.0
            endif
          enddo

          if (k == llyr) then
            do i = 1, ix
              clds(i,5) = 1.0 - cl1(i) * cl2(i) ! save bl cloud
            enddo
          endif
        enddo

        do i = 1, ix
          clds(i,4) = 1.0 - cl1(i) * cl2(i)     ! save total cloud
        enddo

      endif                                     ! end_if_iovr

!  ---  high, mid, low clouds, where cl1, cl2 are cloud fractions
!       layer processed from one layer below llyr and up
!  ---  change! layer processed from surface to top, so low clouds will
!       contains both bl and low clouds.

      if ( ivflip == 0 ) then                   ! input data from toa to sfc

        do i = 1, ix
          cl1 (i) = 0.0
          cl2 (i) = 0.0
          kbt1(i) = nlay
          kbt2(i) = nlay
          kth1(i) = 0
          kth2(i) = 0
          idom(i) = 1
          mbot(i,1) = nlay
          mtop(i,1) = nlay
          mbot(i,2) = nlay - 1
          mtop(i,2) = nlay - 1
          mbot(i,3) = nlay - 1
          mtop(i,3) = nlay - 1
        enddo

!org    do k = llyr-1, 1, -1
        do k = nlay, 1, -1
          do i = 1, ix
            id = idom(i)
            id1= id + 1

            pcur = plyr(i,k)
            ccur = min( ovcst, max( cldtot(i,k), cldcnv(i,k) ))

            if (k > 1) then
              pnxt = plyr(i,k-1)
              cnxt = min( ovcst, max( cldtot(i,k-1), cldcnv(i,k-1) ))
            else
              pnxt = -1.0
              cnxt = 0.0
            endif

            if (pcur < ptop1(i,id1)) then
              id = id + 1
              id1= id1 + 1
              idom(i) = id
            endif

            if (ccur >= climit) then
              if (kth2(i) == 0) kbt2(i) = k
              kth2(i) = kth2(i) + 1

              if ( iovr == 0 ) then
                cl2(i) = cl2(i) + ccur - cl2(i)*ccur
              else
                cl2(i) = max( cl2(i), ccur )
              endif

              if (cnxt < climit .or. pnxt < ptop1(i,id1)) then
                kbt1(i) = nint( (cl1(i)*kbt1(i) + cl2(i)*kbt2(i) )      &
     &                  / (cl1(i) + cl2(i)) )
                kth1(i) = nint( (cl1(i)*kth1(i) + cl2(i)*kth2(i) )      &
     &                  / (cl1(i) + cl2(i)) )
                cl1 (i) = cl1(i) + cl2(i) - cl1(i)*cl2(i)

                kbt2(i) = k - 1
                kth2(i) = 0
                cl2 (i) = 0.0
              endif   ! end_if_cnxt_or_pnxt
            endif     ! end_if_ccur

            if (pnxt < ptop1(i,id1)) then
              clds(i,id) = cl1(i)
              mtop(i,id) = min( kbt1(i), kbt1(i)-kth1(i)+1 )
              mbot(i,id) = kbt1(i)

              cl1 (i) = 0.0
              kbt1(i) = k - 1
              kth1(i) = 0

              if (id1 <= nk_clds) then
                mbot(i,id1) = kbt1(i)
                mtop(i,id1) = kbt1(i)
              endif
            endif     ! end_if_pnxt

          enddo       ! end_do_i_loop
        enddo         ! end_do_k_loop

      else                                      ! input data from sfc to toa

        do i = 1, ix
          cl1 (i) = 0.0
          cl2 (i) = 0.0
          kbt1(i) = 1
          kbt2(i) = 1
          kth1(i) = 0
          kth2(i) = 0
          idom(i) = 1
          mbot(i,1) = 1
          mtop(i,1) = 1
          mbot(i,2) = 2
          mtop(i,2) = 2
          mbot(i,3) = 2
          mtop(i,3) = 2
        enddo

!org    do k = llyr+1, nlay
        do k = 1, nlay
          do i = 1, ix
            id = idom(i)
            id1= id + 1

            pcur = plyr(i,k)
            ccur = min( ovcst, max( cldtot(i,k), cldcnv(i,k) ))

            if (k < nlay) then
              pnxt = plyr(i,k+1)
              cnxt = min( ovcst, max( cldtot(i,k+1), cldcnv(i,k+1) ))
            else
              pnxt = -1.0
              cnxt = 0.0
            endif

            if (pcur < ptop1(i,id1)) then
              id = id + 1
              id1= id1 + 1
              idom(i) = id
            endif

            if (ccur >= climit) then
              if (kth2(i) == 0) kbt2(i) = k
              kth2(i) = kth2(i) + 1

              if ( iovr == 0 ) then
                cl2(i) = cl2(i) + ccur - cl2(i)*ccur
              else
                cl2(i) = max( cl2(i), ccur )
              endif

              if (cnxt < climit .or. pnxt < ptop1(i,id1)) then
                kbt1(i) = nint( (cl1(i)*kbt1(i) + cl2(i)*kbt2(i))       &
     &                  / (cl1(i) + cl2(i)) )
                kth1(i) = nint( (cl1(i)*kth1(i) + cl2(i)*kth2(i))       &
     &                  / (cl1(i) + cl2(i)) )
                cl1 (i) = cl1(i) + cl2(i) - cl1(i)*cl2(i)

                kbt2(i) = k + 1
                kth2(i) = 0
                cl2 (i) = 0.0
              endif     ! end_if_cnxt_or_pnxt
            endif       ! end_if_ccur

            if (pnxt < ptop1(i,id1)) then
              clds(i,id) = cl1(i)
              mtop(i,id) = max( kbt1(i), kbt1(i)+kth1(i)-1 )
              mbot(i,id) = kbt1(i)

              cl1 (i) = 0.0
              kbt1(i) = k + 1
              kth1(i) = 0

              if (id1 <= nk_clds) then
                mbot(i,id1) = kbt1(i)
                mtop(i,id1) = kbt1(i)
              endif
            endif     ! end_if_pnxt

          enddo       ! end_do_i_loop
        enddo         ! end_do_k_loop

      endif                                     ! end_if_ivflip

!
      return
!...................................
      end subroutine gethml
!-----------------------------------


!-----------------------------------                                    !
      subroutine rhtable                                                &
!...................................                                    !

!  ---  inputs:
     &     ( me                                                         &
!  ---  outputs:
     &,      ier )

!  ===================================================================  !
!                                                                       !
! abstract: cld-rh relations obtained from mitchell-hahn procedure,     !
!   here read cld/rh tuning tables for day 0,1,...,5 and merge into 1   !
!   file.                                                               !
!                                                                       !
! inputs:                                                               !
!   me              : check print control flag                          !
!                                                                       !
! outputs:                                                              !
!   ier             : error flag                                        !
!                                                                       !
!  ===================================================================  !
!
      implicit none!

!  ---  inputs:
      integer, intent(in) :: me

!  ---  output:
      integer, intent(out) :: ier

!  ---  locals:
      real (kind=kind_phys), dimension(nbin,nlon,nlat,mcld,nseal) ::    &
     &      rhfd, rtnfd, rhcf, rtncf, rhcla

      real (kind=kind_io4), dimension(nbin,nlon,nlat,mcld,nseal) ::     &
     &      rhfd4, rtnfd4

      real(kind=kind_io4)  :: fhour

      real(kind=kind_phys) :: binscl, cfrac, clsat, rhsat, cstem

      integer, dimension(nlon,nlat,mcld,nseal) :: kpts, kkpts

      integer :: icdays(15), idate(4), nbdayi, isat

      integer :: i, i1, j, k, l, m, id, im, iy

!
!===> ...  begin here
!

      ier = 1

      rewind nicltun

      binscl = 1.0 / nbin

!  ---  array initializations

      do m=1,nseal
       do l=1,mcld
        do k=1,nlat
         do j=1,nlon
          do i=1,nbin
            rhcf (i,j,k,l,m) = 0.0
            rtncf(i,j,k,l,m) = 0.0
            rhcla(i,j,k,l,m) = -0.1
          enddo
         enddo
        enddo
       enddo
      enddo

      kkpts = 0

!  ---  read the data off the rotating file

      read (nicltun,err=998,end=999) nbdayi, icdays

      if (me == 0) print 11, nbdayi
  11  format('   from rhtable days on file =',i5)

      do i = 1, nbdayi
       id = icdays(i) / 10000
       im = (icdays(i)-id*10000) / 100
       iy = icdays(i)-id*10000-im*100
       if (me == 0) print 51, id,im,iy
  51   format('   from rhtable archv data from da,mo,yr=',3i4)
      enddo

      read (nicltun,err=998,end=999) fhour,idate

      do i1 = 1, nbdayi
        read (nicltun) rhfd4
        rhfd = rhfd4

        read (nicltun) rtnfd4
        rtnfd = rtnfd4

        read (nicltun) kpts

        do m=1,nseal
         do l=1,mcld
          do k=1,nlat
           do j=1,nlon
            do i=1,nbin
              rhcf (i,j,k,l,m) = rhcf (i,j,k,l,m) + rhfd (i,j,k,l,m)
              rtncf(i,j,k,l,m) = rtncf(i,j,k,l,m) + rtnfd(i,j,k,l,m)
            enddo
           enddo
          enddo
         enddo
        enddo

        kkpts = kkpts + kpts

      enddo     ! end_do_i1_loop

      do m = 1, nseal
       do l = 1, mcld
        do k = 1, nlat
         do j = 1, nlon

!  ---  compute the cumulative frequency distribution

           do i = 2, nbin
             rhcf (i,j,k,l,m) = rhcf (i-1,j,k,l,m) + rhcf (i,j,k,l,m)
             rtncf(i,j,k,l,m) = rtncf(i-1,j,k,l,m) + rtncf(i,j,k,l,m)
           enddo   ! end_do_i_loop

           if (kkpts(j,k,l,m) > 0) then
             do i = 1, nbin
               rhcf (i,j,k,l,m)= rhcf (i,j,k,l,m)/kkpts(j,k,l,m)
               rtncf(i,j,k,l,m)=min(1., rtncf(i,j,k,l,m)/kkpts(j,k,l,m))
             enddo

!  ---  cause we mix calculations of rh retune with cray and ibm words
!       the last value of rhcf is close to but ne 1.0,
!  ---  so we reset it in order that the 360 loop gives complete tabl

             rhcf(nbin,j,k,l,m) = 1.0

             do i = 1, nbin
               lab_do_i1 : do i1 = 1, nbin
                 if (rhcf(i1,j,k,l,m) >= rtncf(i,j,k,l,m)) then
                   rhcla(i,j,k,l,m) = i1 * binscl
                   exit  lab_do_i1
                 endif
               enddo  lab_do_i1
             enddo

           else                   ! if_kkpts
!  ---  no critical rh

             do i = 1, nbin
               rhcf (i,j,k,l,m) = -0.1
               rtncf(i,j,k,l,m) = -0.1
             enddo

             if (me == 0) then
               print 210, k,j,m
 210           format('  no crit rh for lat=',i3,' and lon band=',i3,   &
     &                ' land(=1) sea=',i3//'  model rh ',' obs rtcld')
               do i = 1, nbin
                 print 203, rhcf(i,j,k,l,m), rtncf(i,j,k,l,m)
 203             format(2f10.2)
               enddo
             endif

           endif               ! if_kkpts

         enddo    ! end_do_j_loop
        enddo     ! end_do_k_loop
       enddo      ! end_do_l_loop
      enddo       ! end_do_m_loop

      do m = 1, nseal
       do l = 1, mcld
        do k = 1, nlat
         do j = 1, nlon

           isat = 0
           do i = 1, nbin-1
             cfrac = binscl * (i - 1)

             if (rhcla(i,j,k,l,m) < 0.0) then
               print 1941, i,m,l,k,j
 1941          format('  neg rhcla for it,nsl,nc,lat,lon=',5i4          &
     &,               '...stoppp..')
               stop
             endif

             if (rtncf(i,j,k,l,m) >= 1.0) then
               if (isat <= 0) then
                 isat  = i
                 rhsat = rhcla(i,j,k,l,m)
                 clsat = cfrac
               endif

               rhcla(i,j,k,l,m) = rhsat + (1.0 - rhsat)                 &
     &                         * (cfrac - clsat) / (1.0 - clsat)
             endif
           enddo

           rhcla(nbin,j,k,l,m) = 1.0

         enddo    ! end_do_j_loop
        enddo     ! end_do_k_loop
       enddo      ! end_do_l_loop
      enddo       ! end_do_m_loop

!  ---  smooth out the table as it reaches rh=1.0, via linear interpolation
!       between location of rh ge .98 and the nbin bin (where rh=1.0)
!       previously rh=1.0 occurred for many of the latter bins in the
!  ---  table, thereby giving a cloud value of less then 1.0 for rh=1.0

      rhcl = rhcla

      do m = 1, nseal
       do l = 1, mcld
        do k = 1, nlat
         do j = 1, nlon

           lab_do_i : do i = 1, nbin - 2
             cfrac = binscl * i

             if (rhcla(i,j,k,l,m) >= 0.98) then
               do i1 = i, nbin
                 cstem = binscl * i1

                 rhcl(i1,j,k,l,m) = rhcla(i,j,k,l,m)                    &
     &                    + (rhcla(nbin,j,k,l,m) - rhcla(i,j,k,l,m))    &
     &                    * (cstem - cfrac) / (1.0 - cfrac)
               enddo
               exit  lab_do_i
             endif
           enddo  lab_do_i

         enddo    ! end_do_j_loop
        enddo     ! end_do_k_loop
       enddo      ! end_do_l_loop
      enddo       ! end_do_m_loop

      if (me == 0) then
        print *,'completed rhtable for cloud tuninig tables'
      endif
      return

 998  print 988
 988  format(' from rhtable error reading tables')
      ier = -1
      return

 999  print 989
 989  format(' from rhtable e.o.f reading tables')
      ier = -1
      return

!...................................
      end subroutine rhtable
!-----------------------------------


!
!........................................!
      end module module_radiation_clouds !
!========================================!

