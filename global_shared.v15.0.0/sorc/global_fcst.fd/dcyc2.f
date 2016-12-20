! ===================================================================== !
!  description:                                                         !
!                                                                       !
!    dcyc2t3 fits radiative fluxes and heating rates from a coarse      !
!    radiation calc time interval into model's more frequent time steps.!
!    solar heating rates and fluxes are scaled by the ratio of cosine   !
!    of zenith angle at the current time to the mean value used in      !
!    radiation calc.  surface downward lw flux is scaled by the ratio   !
!    of current surface air temperature (temp**4) to the corresponding  !
!    temperature saved during lw radiation calculation. upward lw flux  !
!    at the surface is computed by current ground surface temperature.  !
!    surface emissivity effect will be taken in other part of the model.!
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call dcyc2t3                                                       !
!      inputs:                                                          !
!          ( solhr,slag,sdec,cdec,sinlat,coslat,                        !
!            xlon,coszen,tsea,tf,tsflw,                                 !
!            sfcdsw,sfcnsw,sfcdlw,swh,swhc,hlw,hlwc,                    !
!            ix, im, levs,                                              !
!      input/output:                                                    !
!            dtdt,dtdtc,                                                !
!      outputs:                                                         !
!            adjsfcdsw,adjsfcnsw,adjsfcdlw,adjsfculw,xmu,xcosz)         !
!                                                                       !
!                                                                       !
!  program history:                                                     !
!          198?  nmc mrf    - created, similar as treatment in gfdl     !
!                             radiation treatment                       !
!          1994  y. hou     - modified solar zenith angle calculation   !
!     nov  2004  x. wu      - add sfc sw downward flux to the variable  !
!                             list for sea-ice model                    !
!     mar  2008  y. hou     - add cosine of zenith angle as output for  !
!                             sunshine duration time calc.              !
!     sep  2008  y. hou     - separate net sw and downward lw in slrad, !
!                 changed the sign of sfc net sw to consistent with     !
!                 other parts of the mdl (positive value defines from   !
!                 atmos to the ground). rename output fluxes as adjusted!
!                 fluxes. other minor changes such as renaming some of  !
!                 passing argument names to be consistent with calling  !
!                 program.                                              !
!     apr  2009  y. hou     - integrated with the new parallel model    !
!                 along with other modifications                        !
!     mar  2011  y. hou     - minor modification including rearrange    !
!                 loop orders and loop structures to improve efficiency !
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
!     xlon   (im)  - real, longitude in radians. ok in both zonal       !
!                    0->2pi or -pi->+pi arrangements                    !
!     coszen (im)  - real, avg of cosz over daytime sw call interval    !
!     tsea   (im)  - real, ground surface temperature (k)               !
!     tf     (im)  - real, surface air (layer 1) temperature (k)        !
!     tsflw  (im)  - real, sfc air (layer 1) temp in k saved in lw call !
!     sfcdsw (im)  - real, total sky sfc downward sw flux ( w/m**2 )    !
!     sfcnsw (im)  - real, total sky sfc net sw into ground (w/m**2)    !
!     sfcdlw (im)  - real, total sky sfc downward lw flux ( w/m**2 )    !
!     swh(ix,levs) - real, total sky sw heating rates ( k/s )           !
!     swhc(ix,levs) - real, clear sky sw heating rates ( k/s )          !
!     hlw(ix,levs) - real, total sky lw heating rates ( k/s )           !
!     hlwc(ix,levs) - real, clear sky lw heating rates ( k/s )          !
!     ix, im       - integer, horiz. dimention and num of used points   !
!     levs         - integer, vertical layer dimension                  !
!                                                                       !
!  input/output:                                                        !
!     dtdt(im,levs)- real, model time step adjusted total radiation     !
!                          heating rates ( k/s )                        !
!     dtdtc(im,levs)- real, model time step adjusted clear sky radiation! 
!                          heating rates ( k/s )                        !
!                                                                       !
!  outputs:                                                             !
!     adjsfcdsw(im)- real, time step adjusted sfc dn sw flux (w/m**2)   !
!     adjsfcnsw(im)- real, time step adj sfc net sw into ground (w/m**2)!
!     adjsfcdlw(im)- real, time step adjusted sfc dn lw flux (w/m**2)   !
!     adjsfculw(im)- real, sfc upward lw flux at current time (w/m**2)  !
!     xmu   (im)   - real, time step zenith angle adjust factor for sw  !
!     xcosz (im)   - real, cosine of zenith angle at current time step  !
!                                                                       !
!  ====================    end of description    =====================  !

!-----------------------------------
      subroutine dcyc2t3                                                &
!...................................
!  ---  inputs:
     &     ( solhr,slag,sdec,cdec,sinlat,coslat,                        &
     &       xlon,coszen,tsea,tf,tsflw,                                 &
     &       sfcdsw,sfcnsw,sfcdlw,swh,swhc,hlw,hlwc,                    &
     &       ix, im, levs,                                              &
!  ---  input/output:
     &       dtdt,dtdtc,                                                &
!  ---  outputs:
     &       adjsfcdsw,adjsfcnsw,adjsfcdlw,adjsfculw,xmu,xcosz          &
     &     )
!
      use machine,         only : kind_phys
      use physcons,        only : con_pi, con_sbc

      implicit none
!
!  ---  constant parameters:
      real(kind=kind_phys), parameter :: f_eps  = 0.0001
      real(kind=kind_phys), parameter :: hour12 = 12.0

!  ---  inputs:
      integer, intent(in) :: ix, im, levs

      real(kind=kind_phys), intent(in) :: solhr, slag, cdec, sdec

      real(kind=kind_phys), dimension(im), intent(in) ::                &
     &      sinlat, coslat, xlon, coszen, tsea, tf, tsflw, sfcdlw,      &
     &      sfcdsw, sfcnsw

      real(kind=kind_phys), dimension(ix,levs),intent(in) :: swh, hlw
      real(kind=kind_phys), dimension(ix,levs),intent(in) :: swhc,hlwc

!  ---  input/output:
      real(kind=kind_phys), dimension(im,levs),intent(inout) :: dtdt
      real(kind=kind_phys), dimension(im,levs),intent(inout) :: dtdtc

!  ---  outputs:
      real(kind=kind_phys), dimension(im), intent(out) ::               &
     &      adjsfcdsw, adjsfcnsw, adjsfcdlw, adjsfculw, xmu, xcosz

!  ---  locals:
      integer :: i, k
      real(kind=kind_phys) :: cns, ss, cc, ch, tem1, tem2, xlw
!
!===> ...  begin here
!
!  --- ...  lw time-step adjustment

      do i = 1, im

!  --- ...  adjust sfc downward lw flux to account for t changes in layer 1
!           compute 4th power of the ratio of layer 1 tf over the mean value tsflw

        tem1 = tf(i) / tsflw(i)
        xlw  = tem1 * tem1
        adjsfcdlw(i) = sfcdlw(i) * xlw * xlw

!  --- ...  compute sfc upward lw flux from current sfc temp,
!      note: sfc emiss effect is not appied here, and will be dealt in other place

        tem2 = tsea(i) * tsea(i)
        adjsfculw(i) =  con_sbc * tem2 * tem2
      enddo

!  --- ...  sw time-step adjustment

      cns = con_pi * (solhr - hour12) / hour12 + slag

      do i = 1, im
        ss     = sinlat(i) * sdec
        cc     = coslat(i) * cdec
        ch     = cc * cos( xlon(i)+cns )
        xcosz(i) = ch + ss        ! cosine of solar zenith angle at current time

!  --- ...  normalize by average value over radiation period for daytime.

        if ( xcosz(i) > f_eps .and. coszen(i) > f_eps ) then
          xmu(i) = xcosz(i) / coszen(i)
        else
          xmu(i) = 0.0
        endif

!  --- ...  adjust sfc net and downward sw fluxes for zenith angle changes
!      note: sfc emiss effect will not be appied here

        adjsfcnsw(i) = sfcnsw(i) * xmu(i)
        adjsfcdsw(i) = sfcdsw(i) * xmu(i)
      enddo

!  --- ...  adjust sw heating rates with zenith angle change and
!           add with lw heating to temperature tendency

      do k = 1, levs
        do i = 1, im
          dtdt(i,k) = dtdt(i,k) + swh(i,k)*xmu(i) + hlw(i,k)
          dtdtc(i,k) = dtdtc(i,k) + swhc(i,k)*xmu(i) + hlwc(i,k)
        enddo
      enddo
!
      return
!...................................
      end subroutine dcyc2t3
!-----------------------------------

