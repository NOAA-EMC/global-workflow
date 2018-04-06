!-----------------------------------
      subroutine sfc_cice                                               &
!...................................
!  ---  inputs:
     &     ( im, u1, v1, t1, q1, cm, ch, prsl1, prslki,                 &
     &       islimsk, ddvel, flag_iter, dqsfc, dtsfc,                   &
!  ---  outputs:
     &       qsurf, cmm, chh, evap, hflx )

! ===================================================================== !
!  description:                                                         !
!  Sep 2015  --  Xingren Wu created from sfc_sice for coupling to CICE  !
!                                                                       !
!  usage:                                                               !
!                                                                       !
!    call sfc_cice                                                      !
!       inputs:                                                         !
!          ( im, u1, v1, t1, q1, cm, ch, prsl1, prslki,                 !
!            islimsk, ddvel, flag_iter, dqsfc, dtsfc,                   !
!       outputs:                                                        !
!            qsurf, cmm, chh, evap, hflx)                       !
!                                                                       !
!  ====================  defination of variables  ====================  !
!                                                                       !
!  inputs:
!     im, - integer, horiz dimension
!     u1, v1   - real, u/v component of surface layer wind
!     t1       - real, surface layer mean temperature ( k )
!     q1       - real, surface layer mean specific humidity
!     cm       - real, surface exchange coeff for momentum (m/s)
!     ch       - real, surface exchange coeff heat & moisture(m/s)
!     prsl1    - real, surface layer mean pressure
!     prslki   - real, ?
!     islimsk  - integer, sea/land/ice mask
!     ddvel    - real, ?
!     flag_iter- logical
!     dqsfc    - real, latent heat flux
!     dtsfc    - real, sensible heat flux
!  outputs:
!     qsurf    - real, specific humidity at sfc
!     cmm      - real, ?
!     chh      - real, ?
!     evap     - real, evaperation from latent heat
!     hflx     - real, sensible heat
! ===================================================================== !
!
      use machine , only : kind_phys
      use funcphys, only : fpvs
      use physcons, only : hvap => con_hvap,  cp => con_cp,             &
     &                     eps => con_eps, epsm1 => con_epsm1,          &
     &                     rvrdm1 => con_fvirt, rd => con_rd
!
      implicit none
!
!  ---  constant parameters:
      real(kind=kind_phys), parameter :: cpinv = 1.0/cp
      real(kind=kind_phys), parameter :: hvapi = 1.0/hvap
      real(kind=kind_phys), parameter :: elocp = hvap/cp

!  ---  inputs:
      integer, intent(in) :: im

      real (kind=kind_phys), dimension(im), intent(in) :: u1, v1,       &
     &       t1, q1, cm, ch, prsl1, prslki, ddvel, dqsfc, dtsfc

      integer, dimension(im), intent(in) :: islimsk

      logical, intent(in) :: flag_iter(im)

!  ---  outputs:
      real (kind=kind_phys), dimension(im), intent(out) :: qsurf,       &
     &       cmm, chh, evap, hflx

!  ---  locals:
      real (kind=kind_phys), dimension(im) :: q0, rch, rho, tv1, wind

      real (kind=kind_phys) :: tem

      integer :: i
 
      logical :: flag(im)
!

      do i = 1, im
         flag(i) = (islimsk(i) == 4) .and. flag_iter(i)
      enddo
!
      do i = 1, im
        if (flag(i)) then

          wind(i)   = sqrt(u1(i)*u1(i) + v1(i)*v1(i))                   &
     &              + max(0.0, min(ddvel(i), 30.0))
          wind(i)   = max(wind(i), 1.0)

          q0(i)     = max(q1(i), 1.0e-8)
          tv1(i)    = t1(i) * (1.0 + rvrdm1*q0(i))
          rho(i)    = prsl1(i) / (rd*tv1(i))

          cmm(i) = cm(i)  * wind(i)
          chh(i) = rho(i) * ch(i) * wind(i)
          rch(i) = chh(i) * cp

          qsurf(i) = q1(i) + dqsfc(i) / (elocp*rch(i))
          tem     = 1.0 / rho(i)
          hflx(i) = dtsfc(i) * tem * cpinv
          evap(i) = dqsfc(i) * tem * hvapi
        endif
      enddo
 
      return

      end subroutine sfc_cice
