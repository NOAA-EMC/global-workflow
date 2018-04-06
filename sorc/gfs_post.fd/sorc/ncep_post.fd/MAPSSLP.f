!
      SUBROUTINE MAPSSLP(TPRES)
!$$$  SUBPROGRAM DOCUMENTATION BLOCK
!                .      .    .
!   INPUT ARGUMENT LIST:
!     TPRES    - TEMPERATURE at pressure levels 
!
!   OUTPUT ARGUMENT LIST:
!     PSLP - THE FINAL REDUCED SEA LEVEL PRESSURE ARRAY
!
!-----------------------------------------------------------------------
      use ctlblk_mod, only: jsta, jend, spl, smflag, lm, im, jsta_2l, jend_2u, &
                            lsm, jm, grib
      use gridspec_mod, only: maptype, dxval
      use vrbls3d, only: pmid, t, pint
      use vrbls2d, only: pslp, fis
      use masks, only: lmh
      use params_mod, only: rog, p1000, capa, erad, pi ,gi

      implicit none
!      
      INCLUDE "mpif.h"
!
      REAL TPRES(IM,JSTA_2L:JEND_2U,LSM)
 
      real LAPSES, EXPo,EXPINV,TSFCNEW

      REAL,dimension(im, jsta_2l:jend_2u) :: T700
      real,dimension(im,2) :: sdummy
      REAL,dimension(im,jm) :: GRID1, TH700
      INTEGER NSMOOTH
      integer l, j, i, k, ii, jj 
      real dxm
!-----------------------------------------------------------------------
!***
        LAPSES = 0.0065
! deg K / meter
        EXPo   = ROG*LAPSES
        EXPINV = 1./EXPo

      DO L=1,LSM

!$omp parallel do private(i,j)
        DO J=JSTA,JEND
          DO I=1,IM
            if(SPL(L) == 70000.)THEN
              T700(i,j)  = TPRES(I,J,L) 
              TH700(I,J) = T700(I,J)*(P1000/70000.)**CAPA
            endif
          ENDDO
        ENDDO

      ENDDO


! smooth 700 mb temperature first
       if(MAPTYPE.EQ.6) then
         if(grib=='grib1') then
            dxm = (DXVAL / 360.)*(ERAD*2.*pi)/1000. ! [m]
         else if (grib=='grib2') then
            dxm=(DXVAL / 360.)*(ERAD*2.*pi)/1.d6  ! [mm]
         endif
       else
         dxm = dxval
       endif
       if(grib == 'grib2')then
         dxm = dxm/1000.0 ! [m]
       endif

       IF (SMFLAG) THEN   
         NSMOOTH=nint(10.*(13500./dxm))
        call AllGETHERV(TH700)
        do k = 1,NSMOOTH
          CALL SMOOTH(TH700,SDUMMY,IM,JM,0.5)
        end do
        ENDIF
          ii=im/2
          jj=(jsta+jend)/2
          if(i.eq.ii.and.j.eq.jj)                              &
             print*,'Debug TH700(i,j), i,j',TH700(i,j), i,j

       DO J=JSTA,JEND
         DO I=1,IM
         T700(I,J) = TH700(I,J)*(70000./P1000)**CAPA
          IF (T700(I,J).GT.100.) THEN
           TSFCNEW = T700(I,J)*(PMID(I,J,LM)/70000.)**EXPo
!     effective sfc T based on 700 mb temp
          ELSE
           TSFCNEW = T(I,J,LM)
           ENDIF
          PSLP(I,J) = PINT(I,J,NINT(LMH(I,J))+1)*               &
              ((TSFCNEW+LAPSES*FIS(I,J)*GI)/TSFCNEW)**EXPINV
!          print*,'PSLP(I,J),I,J',PSLP(I,J),I,J
           GRID1(I,J)=PSLP(I,J)
         ENDDO
       ENDDO

         IF (SMFLAG) THEN
! - in WRF number of passes depends on the resolution: nsmooth=int(15*(13/dxval))
         NSMOOTH=nint(15.*(13500./dxm))
         call AllGETHERV(GRID1)
         do k=1,NSMOOTH
          CALL SMOOTH(GRID1,SDUMMY,IM,JM,0.5)
         end do
         DO J=JSTA,JEND
            DO I=1,IM
               PSLP(I,J)=GRID1(I,J)
            ENDDO
         ENDDO
         ENDIF
!

      RETURN
      END
