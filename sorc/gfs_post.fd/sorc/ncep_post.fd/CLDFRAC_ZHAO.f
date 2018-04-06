!-----------------------------------
      subroutine progcld1                                               &
!...................................

!  ---  inputs:
          ( plyr,tlyr,qlyr,qstl,clw,                        &
!           xlat,xlon,                                      &
            IX, NLAY, iflip,                                &
!  ---  outputs:
            cldtot                                          &
           )

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
!   initial subroutine "cldinit".                                       !
!                                                                       !
! program history log:                                                  !
!      11-xx-1992   y.h., k.a.c, a.k. - cloud parameterization          !
!         'cldjms' patterned after slingo and slingo's work (jgr,       !
!         1992), stratiform clouds are allowed in any layer except      !
!         the surface and upper stratosphere. the relative humidity     !
!         criterion may cery in different model layers.                 !
!      10-25-1995   kenneth campana   - tuned cloud rh curves           !
!         rh-cld relation from tables created using mitchell-hahn       !
!         tuning technique on airforce rtneph observations.             !
!      11-02-1995   kenneth campana   - the bl relationships used       !
!         below llyr, except in marine stratus regions.                 !
!      04-11-1996   kenneth campana   - save bl cld amt in cld(,5)      !
!      12-29-1998   s. moorthi        - prognostic cloud method         !
!      04-15-2003   yu-tai hou        - rewritten in frotran 90         !
!         modulized form, seperate prognostic and diagnostic methods    !
!         into two packages.                                            !
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
!   plyr  (IX,NLAY) : model layer mean pressure in mb (100Pa)           !
!   tlyr  (IX,NLAY) : model layer mean temperature in k                 !
!   qlyr  (IX,NLAY) : layer specific humidity in gm/gm                  !
!   qstl  (IX,NLAY) : layer saturate humidity in gm/gm                  !
!   rhly  (IX,NLAY) : layer relative humidity (=qlyr/qstl)              !
!   clw   (IX,NLAY) : layer cloud condensate amount                     !
!   xlat  (IX)      : grid latitude in radians                          !
!   xlon  (IX)      : grid longitude in radians  (not used)             !
!   slmsk (IX)      : sea/land mask array (sea:0,land:1,sea-ice:2)      !
!   IX              : horizontal dimention                              !
!   NLAY            : vertical layer/level dimensions                   !
!   iflip           : control flag for in/out vertical indexing         !
!                     =0: index from toa to surface                     !
!                     =1: index from surface to toa                     !
!   iovr            : control flag for cloud overlap                    !
!                     =0 random overlapping clouds                      !
!                     =1 max/ran overlapping clouds                     !
!                                                                       !
! output variables:                                                     !
!   clouds(IX,NLAY,NF_CLDS) : cloud profiles                            !
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
!   clds  (IX,5)    : fraction of clouds for low, mid, hi, tot, bl      !
!   mtop  (IX,3)    : vertical indices for low, mid, hi cloud tops      !
!   mbot  (IX,3)    : vertical indices for low, mid, hi cloud bases     !
!                                                                       !
!  ====================    end of description    =====================  !
!
      use kinds, only: r_kind
      implicit none
      real, parameter:: climit=0.001
!  ---  inputs
      integer,  intent(in) :: IX, NLAY, iflip

      real (kind=r_kind), dimension(ix,nlay), intent(in) :: plyr,  &
            tlyr, qlyr, qstl, clw

!     real (kind=r_kind), dimension(ix),   intent(in) :: xlat, xlon

!  ---  outputs
!      real (kind=kind_phys), dimension(:,:,:), intent(out) :: clouds

      real (kind=r_kind), dimension(ix,nlay),   intent(out) :: cldtot

!      integer,               dimension(:,:),   intent(out) :: mtop,mbot

!  ---  local variables:
!      real (kind=kind_phys), dimension(IX,NLAY) :: cldtot, cldcnv,      &
!     &       cwp, cip, crp, csp, rew, rei, res, rer, delp, tem2d

!      real (kind=kind_phys) :: ptop1(IX,4)
      real (kind=r_kind) :: rhly(ix,nlay)
      real (kind=r_kind) :: clwmin, clwm, clwt, onemrh, value,       &
            tem1, tem2, tem3

      integer, dimension(IX) :: kinver

      integer :: i, k, id, id1

      logical :: inversn(IX)

!
!===> ... begin here
!
!      clouds(:,:,:) = 0.0

!$omp parallel do private(i,k)
      do k = 1, NLAY
        do i = 1, IX
          cldtot(i,k) = 0.0
          rhly(i,k)   = qlyr(i,k)/qstl(i,k) ! Chuang: add RH computation here
          rhly(i,k)   = max(0.0,min(1.0,rhly(i,k))) ! moorthi
        enddo
      enddo

!  ---  layer cloud fraction

      if (iflip == 0) then                 ! input data from toa to sfc

        do i = 1, IX
          inversn(i) = .false.
          kinver (i) = 1
        enddo

        do k = NLAY-1, 2, -1
!$omp parallel do private(i,tem1)
          do i = 1, IX
            if (plyr(i,k) > 600.0 .and. (.not.inversn(i))) then
              tem1 = tlyr(i,k-1) - tlyr(i,k)

              if (tem1 > 0.1 .and. tlyr(i,k) > 278.0) then
                inversn(i) = .true.
                kinver(i)  = k
              endif
            endif
          enddo
        enddo

        clwmin = 0.0
        do k = NLAY, 1, -1
!$omp parallel do private(i,tem1,tem2,clwt,clwm,onemrh,value)
          do i = 1, IX
            clwt = 1.0e-6 * (plyr(i,k)*0.001)
!           clwt = 2.0e-6 * (plyr(i,k)*0.001)

            if (clw(i,k) > clwt .or.                                    &
              (inversn(i) .and. k >= kinver(i)) ) then

              onemrh= max( 1.e-10, 1.0-rhly(i,k) )
              clwm  = clwmin / max( 0.01, plyr(i,k)*0.001 )

              tem1  = min(max(sqrt(sqrt(onemrh*qstl(i,k))),0.0001),1.0)
              tem1  = 2000.0 / tem1
!             tem1  = 1000.0 / tem1
!             if (inversn(i) .and. k >= kinver(i)) tem1 = tem1 * 5.0

              value = max( min( tem1*(clw(i,k)-clwm), 50.0 ), 0.0 )
              tem2  = sqrt( sqrt(rhly(i,k)) )

              cldtot(i,k) = max( tem2*(1.0-exp(-value)), 0.0 )
            endif
          enddo
        enddo

      else                                 ! input data from sfc to toa

        do i = 1, IX
          inversn(i) = .false.
          kinver (i) = NLAY
        enddo

        do k = 2, NLAY
!$omp parallel do private(i,tem1)
          do i = 1, IX
            if (plyr(i,k) > 600.0 .and. (.not.inversn(i))) then
              tem1 = tlyr(i,k+1) - tlyr(i,k)

              if (tem1 > 0.1 .and. tlyr(i,k) > 278.0) then
                inversn(i) = .true.
                kinver(i)  = k
              endif
            endif
          enddo
        enddo

        clwmin = 0.0
        do k = 1, NLAY
!$omp parallel do private(i,tem1,tem2,clwt,clwm,onemrh,value)
          do i = 1, IX
            clwt = 1.0e-6 * (plyr(i,k)*0.001)
!           clwt = 2.0e-6 * (plyr(i,k)*0.001)

            if (clw(i,k) > clwt .or.                                    &
              (inversn(i) .and. k <= kinver(i)) ) then

              onemrh= max( 1.e-10, 1.0-rhly(i,k) )
              clwm  = clwmin / max( 0.01, plyr(i,k)*0.001 )

              tem1  = min(max(sqrt(sqrt(onemrh*qstl(i,k))),0.0001),1.0)
              tem1  = 2000.0 / tem1
!             tem1  = 1000.0 / tem1
!             if (inversn(i) .and. k <= kinver(i)) tem1 = tem1 * 5.0

              value = max( min( tem1*(clw(i,k)-clwm), 50.0 ), 0.0 )
              tem2  = sqrt( sqrt(rhly(i,k)) )

              cldtot(i,k) = max( tem2*(1.0-exp(-value)), 0.0 )
            endif
          enddo
        enddo

      endif                                ! end_if_flip

      where (cldtot < climit)
        cldtot = 0.0
      endwhere
!
      return
!...................................
      end subroutine progcld1
!-----------------------------------


