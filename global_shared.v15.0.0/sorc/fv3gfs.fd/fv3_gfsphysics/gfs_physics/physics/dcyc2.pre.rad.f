! ===================================================================== !
!  description:                                                         !
!                                                                       !
!    dcyc2t3_pre_rad is a testing/debuging utility program mimic the    !
!    original dcyc2t3 program that fits radiatibe fluxes and heating    !
!    rates from a coarse time interval of radiation calculations onto   !
!    the model's more frequent time steps.                              !
!    (note: although contains some side-effect, this program is mainly  !
!           used for testing purpose, but not for regular fcst mode.    !
!           no significant modifications done after it was created)     !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call dcyc2t3_pre rad                                               !
!      inputs:                                                          !
!          ( solhr,slag,sdec,cdec,sinlat,coslat,                        !
!            xlon,coszen,tsea,tf,tsflw,                                 !
!            sfcdsw,sfcnsw,sfcdlw,swh,hlw,                              !
!            sfcnirbmu,sfcnirdfu,sfcvisbmu,sfcvisdfu,                   !
!            sfcnirbmd,sfcnirdfd,sfcvisbmd,sfcvisdfd,                   !
!            ix, im, levs,                                              !
!      input/output:                                                    !
!            dtdt,                                                      !
!      outputs:                                                         !
!            adjsfcdsw,adjsfcnsw,adjsfcdlw,adjsfculw,xmu,xcosz,         !
!            adjnirbmu,adjnirdfu,adjvisbmu,adjvisdfu,                   !
!            adjnirbmd,adjnirdfd,adjvisbmd,adjvisdfd)                   !
!                                                                       !
!                                                                       !
!  program history:                                                     !
!          198?  nmc mrf    - created subr dcyc2, similar to treatment  !
!                             in gfdl radiation approximation scheme    !
!          1994  y. hou     - modified solar zenith angle calculation   !
!          200?  j. sala    - modified from dcyc2 to create a testing   !
!                             debuging program                          !
!     nov  2004  x. wu      - add sfc sw downward flux to the variable  !
!                             list for sea-ice model                    !
!     mar  2008  y. hou     - following updates in dcyc2t3, add cosine  !
!                             of zenith angle as output for sunshine    !
!                             duration time calculation.                !
!     sep  2008  y. hou     - separate net sw and downward lw in slrad, !
!                 change the sign of sfc net sw to consistent with other!
!                 parts of the mdl (positive value defines from atmos to!
!                 the ground). rename output fluxes as adjusted fluxes. !
!                 other minor changes such as renaming some passing     !
!                 argument names to be consistent with calling subr.    !
!     mar  2014  x. wu      - add sfc nir/vis bm/df to the variable     !
!                             list for the coupled model input          !
!     jun  2014  y. hou     - revised to include both up and down sw    !
!                 spectral component fluxes                             !
!                                                                       !
!                                                                       !
!  subprograms called:  none                                            !
!                                                                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!  inputs:                                                              !
!     solhr        - real, forecast time in 24-hour form (hr)           !
!     slag         - real, equation of time in radians                  !
!     sdec, cdec   - real, sin and cos of the solar declination angle   !
!     sinlat(im), coslat(im):                                           !
!                  - real, sin and cos of latitude                      !
!     xlon   (im)  - real, longitude in radians                         !
!     coszen (im)  - real, avg of cosz over daytime sw call interval    !
!     tsea   (im)  - real, ground surface temperature (k)               !
!     tf     (im)  - real, surface air (layer 1) temperature (k)        !
!     tsflw  (im)  - real, sfc air (layer 1) temp in k saved in lw call !
!     sfcdsw (im)  - real, total sky sfc downward sw flux (w/m**2)      !
!     sfcnsw (im)  - real, total sky sfc net sw into ground (w/m**2)    !
!     sfcdlw (im)  - real, total sky sfc downward lw flux (w/m**2)      !
!     swh(ix,levs) - real, total sky sw heating rates ( k/s )           !
!     hlw(ix,levs) - real, total sky lw heating rates ( k/s )           !
!     sfcnirbmu(im)- real, tot sky sfc nir-beam sw upward flux (w/m2)   !
!     sfcnirdfu(im)- real, tot sky sfc nir-diff sw upward flux (w/m2)   !
!     sfcvisbmu(im)- real, tot sky sfc uv+vis-beam sw upward flux (w/m2)!
!     sfcvisdfu(im)- real, tot sky sfc uv+vis-diff sw upward flux (w/m2)!
!     sfcnirbmd(im)- real, tot sky sfc nir-beam sw downward flux (w/m2) !
!     sfcnirdfd(im)- real, tot sky sfc nir-diff sw downward flux (w/m2) !
!     sfcvisbmd(im)- real, tot sky sfc uv+vis-beam sw dnward flux (w/m2)!
!     sfcvisdfd(im)- real, tot sky sfc uv+vis-diff sw dnward flux (w/m2)!
!     ix, im       - integer, horiz. dimention and num of used points   !
!     levs         - integer, vertical layer dimension                  !
!                                                                       !
!  input/output:                                                        !
!     dtdt(im,levs)- real, model time step adjusted total radiation     !
!                    heating rates ( k/s )                              !
!                                                                       !
!  outputs:                                                             !
!     adjsfcdsw(im)- real, time step adjusted sfc dn sw flux (w/m**2)   !
!     adjsfcnsw(im)- real, time step adj sfc net sw into ground (w/m**2)!
!     adjsfcdlw(im)- real, time step adjusted sfc dn lw flux (w/m**2)   !
!     adjsfculw(im)- real, sfc upward lw flux at current time (w/m**2)  !
!     adjnirbmu(im)- real, t adj sfc nir-beam sw upward flux (w/m2)     !
!     adjnirdfu(im)- real, t adj sfc nir-diff sw upward flux (w/m2)     !
!     adjvisbmu(im)- real, t adj sfc uv+vis-beam sw upward flux (w/m2)  !
!     adjvisdfu(im)- real, t adj sfc uv+vis-diff sw upward flux (w/m2)  !
!     adjnirbmd(im)- real, t adj sfc nir-beam sw downward flux (w/m2)   !
!     adjnirdfd(im)- real, t adj sfc nir-diff sw downward flux (w/m2)   !
!     adjvisbmd(im)- real, t adj sfc uv+vis-beam sw dnward flux (w/m2)  !
!     adjvisdfd(im)- real, t adj sfc uv+vis-diff sw dnward flux (w/m2)  !
!     xmu    (im)  - real, time step zenith angle adjust factor for sw  !
!     xcosz  (im)  - real, cosine of zenith angle at current time step  !
!                                                                       !
!  ====================    end of description    =====================  !

!-----------------------------------
      subroutine dcyc2t3_pre_rad                                        &
!...................................
!  ---  inputs:
     &     ( solhr,slag,sdec,cdec,sinlat,coslat,                        &
     &       xlon,coszen,tsea,tf,tsflw,                                 &
     &       sfcdsw,sfcnsw,sfcdlw,swh,hlw,                              &
     &       sfcnirbmu,sfcnirdfu,sfcvisbmu,sfcvisdfu,                   &
     &       sfcnirbmd,sfcnirdfd,sfcvisbmd,sfcvisdfd,                   &
     &       ix, im, levs,                                              &
!  ---  input/output:
     &       dtdt,                                                      &
!  ---  outputs:
     &       adjsfcdsw,adjsfcnsw,adjsfcdlw,adjsfculw,xmu,xcosz,         &
     &       adjnirbmu,adjnirdfu,adjvisbmu,adjvisdfu,                   &
     &       adjnirbmd,adjnirdfd,adjvisbmd,adjvisdfd                    &
     &     )
!
      use machine,      only : kind_phys
      use physcons,     only : con_pi, con_sbc, con_jcal

      implicit none
!
!  ---  constant parameters:
      real(kind=kind_phys), parameter :: cnwatt = -con_jcal*1.0e4/60.0

!  ---  inputs:
      integer, intent(in) :: ix, im, levs

      real(kind=kind_phys), intent(in) :: solhr, slag, cdec, sdec

      real(kind=kind_phys), dimension(im), intent(in) ::                &
     &      sinlat, coslat, xlon, coszen, tsea, tf, tsflw, sfcdlw,      &
     &      sfcdsw, sfcnsw
      real(kind=kind_phys), dimension(im), intent(in) ::                &
     &      sfcnirbmu, sfcnirdfu, sfcvisbmu, sfcvisdfu,                 &
     &      sfcnirbmd, sfcnirdfd, sfcvisbmd, sfcvisdfd

      real(kind=kind_phys), dimension(ix,levs), intent(in) :: swh, hlw

!  ---  input/output:
      real(kind=kind_phys), dimension(im,levs), intent(inout) :: dtdt

!  ---  outputs:
      real(kind=kind_phys), dimension(im), intent(out) ::               &
     &      adjsfcdsw, adjsfcnsw, adjsfcdlw, adjsfculw, xmu, xcosz,     &
     &      adjnirbmu,adjnirdfu,adjvisbmu,adjvisdfu,                    &
     &      adjnirbmd,adjnirdfd,adjvisbmd,adjvisdfd

!  ---  locals:
      integer :: i, k
      real(kind=kind_phys) :: cns, ss, cc, ch, tem
!
!===> ...  begin here
!
      xmu(:) = 1.0
      cc     = 0.1
      ss     = 0.0
      ch     = 350.0 / cnwatt

      do i = 1, im
        xcosz(i) = xmu(i)
        if (xmu(i) > 0.01 .and. cc > 0.01) then
          xmu(i) = xmu(i) / cc
        else
          xmu(i) = 0.0
        endif

        adjsfcdsw(i) = sfcdsw(i) * xmu(i)
        adjsfcnsw(i) = ss * xmu(i)

        adjnirbmu(i)  = sfcnirbmd(i) * xmu(i)
        adjnirdfu(i)  = sfcnirdfd(i) * xmu(i)
        adjvisbmu(i)  = sfcvisbmd(i) * xmu(i)
        adjvisdfu(i)  = sfcvisdfd(i) * xmu(i)

        adjnirbmd(i)  = sfcnirbmd(i) * xmu(i)
        adjnirdfd(i)  = sfcnirdfd(i) * xmu(i)
        adjvisbmd(i)  = sfcvisbmd(i) * xmu(i)
        adjvisdfd(i)  = sfcvisdfd(i) * xmu(i)

        tem       = tf(i) / tsflw(i)
        tem       = tem * tem
        adjsfcdlw(i) = ch * tem * tem
        tem       = tsea(i) * tsea(i)
        adjsfculw(i) = con_sbc * tem * tem
      enddo

!  --- ...  will not change dtdt value
!     do k = 1, levs
!       do i = 1, im
!         dtdt(i,k) = dtdt(i,k) + 0.0
!       enddo
!     enddo
!
      return
!...................................
      end subroutine dcyc2t3_pre_rad
!-----------------------------------

