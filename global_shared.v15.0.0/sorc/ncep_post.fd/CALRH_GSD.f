!**********************************************************************c
      SUBROUTINE CALRH_GSD(P1,T1,Q1,RHB)

! Algorithm use at GSD for RUC and Rapid Refresh                           
!------------------------------------------------------------------
!

      use ctlblk_mod, only: jsta, jend, im

      implicit none

      integer :: j, i
      real :: tx, pol, esx, es, e
      real, dimension(im,jsta:jend) :: P1, T1, Q1, RHB


      DO J=JSTA,JEND
        DO I=1,IM

! - compute relative humidity
          Tx=T1(I,J)-273.15
          POL = 0.99999683       + TX*(-0.90826951E-02 +    &
             TX*(0.78736169E-04   + TX*(-0.61117958E-06 +   &
             TX*(0.43884187E-08   + TX*(-0.29883885E-10 +   &
             TX*(0.21874425E-12   + TX*(-0.17892321E-14 +   &
             TX*(0.11112018E-16   + TX*(-0.30994571E-19)))))))))
          esx = 6.1078/POL**8

          ES = esx
          E = P1(I,J)/100.*Q1(I,J)/(0.62197+Q1(I,J)*0.37803)
          RHB(I,J) = MIN(1.,E/ES)

        ENDDO
      ENDDO

      RETURN
      END
