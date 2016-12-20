!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!                .      .    .                                       . 
! SUBPROGRAM:    SMOOTH      SMOOTH A METEOROLOGICAL FIELD
!   PRGMMR: STAN BENJAMIN    ORG: FSL/PROFS  DATE: 90-06-15 
! 
! ABSTRACT: SHAPIRO SMOOTHER. 
! 
! PROGRAM HISTORY LOG: 
!   85-12-09  S. BENJAMIN   ORIGINAL VERSION
!   14-03-03  S. Moorthi    Threading and slight cleanup
! 
! USAGE:    CALL SMOOTH (FIELD,HOLD,IX,IY,SMTH) 
!   INPUT ARGUMENT LIST: 
!     FIELD    - REAL ARRAY  FIELD(IX,IY)
!                            METEOROLOGICAL FIELD
!     HOLD     - REAL ARRAY  HOLD(IX,2)
!                            HOLDING THE VALUE FOR FIELD
!     IX       - INTEGER     X COORDINATES OF FIELD
!     IY       - INTEGER     Y COORDINATES OF FIELD
!     SMTH     - REAL      
!
!   OUTPUT ARGUMENT LIST:   
!     FIELD    - REAL ARRAY  FIELD(IX,IY)
!                            SMOOTHED METEOROLOGICAL FIELD
! 
! REMARKS: REFERENCE: SHAPIRO, 1970: "SMOOTHING, FILTERING, AND
!   BOUNDARY EFFECTS", REV. GEOPHYS. SP. PHYS., 359-387.
!   THIS FILTER IS OF THE TYPE 
!         Z(I) = (1-S)Z(I) + S(Z(I+1)+Z(I-1))/2
!   FOR A FILTER WHICH IS SUPPOSED TO DAMP 2DX WAVES COMPLETELY
!   BUT LEAVE 4DX AND LONGER WITH LITTLE DAMPING,
!   IT SHOULD BE RUN WITH 2 PASSES USING SMTH (OR S) OF 0.5
!   AND -0.5.
!   
! ATTRIBUTES: 
!   LANGUAGE: FORTRAN-77 + EXTENSIONS
!   MACHINE:  NAS-9000, VAX, UNIX
!$$$ 
!**********************************************************************
!**********************************************************************

      SUBROUTINE SMOOTH (FIELD,HOLD,IX,IY,SMTH)

!**********************************************************************
!**********************************************************************

      implicit none

      integer :: i1, i2, j, it, i, ix, iy
      real    :: smth1, smth, smth2, smth3, smth4, smth5 
      real    :: sum1, sum2
      REAL      FIELD(IX,IY), HOLD (IX,2)
      SMTH1 = 0.25 * SMTH * SMTH
      SMTH2 = 0.5  * SMTH * (1.-SMTH)
      SMTH3 = (1.-SMTH) * (1.-SMTH)
      SMTH4 = (1.-SMTH)
      SMTH5 = 0.5 * SMTH
      I1 = 2
      I2 = 1
       DO J=2,IY-1
         IT = I1
         I1 = I2
         I2 = IT
!$omp parallel do private(i,sum1,sum2)
         DO I = 2,IX-1
           SUM1 = FIELD (I-1,J+1) + FIELD (I-1,J-1)                 &
                + FIELD (I+1,J+1) + FIELD (I+1,J-1)
           SUM2 = FIELD (I  ,J+1) + FIELD (I+1,J  )                 &
                + FIELD (I  ,J-1) + FIELD (I-1,J  )
           HOLD(I,I1) = SMTH1*SUM1 + SMTH2*SUM2 + SMTH3*FIELD(I,J)
         ENDDO
         IF (J > 2) then
!$omp parallel do private(i)
           DO I=2,IX-1
             FIELD(I,J-1) = HOLD(I,I2)
           ENDDO
         endif
       ENDDO

!$omp parallel do private(i)
       DO I = 2,IX-1
         FIELD (I,IY-1) = HOLD(I,I1)
       ENDDO

       DO I = 2,IX-1
         FIELD(I,1)  = SMTH4 * FIELD(I,1)                             &
                     + SMTH5 * (FIELD(I-1,1) + FIELD(I+1,1))
         FIELD(I,IY) = SMTH4 * FIELD(I,IY)                            &
                     + SMTH5 * (FIELD(I-1,IY) + FIELD(I+1,IY))
       ENDDO

       DO J = 2,IY-1
         FIELD(1,J)  = SMTH4 * FIELD(1,J)                             &
                     + SMTH5 * (FIELD(1,J-1) + FIELD(1,J+1))
         FIELD(IX,J) = SMTH4 * FIELD(IX,J)                            &
                     + SMTH5 * (FIELD(IX,J-1) + FIELD(IX,J+1))
       ENDDO

      RETURN
      END
!$$$  SUBPROGRAM DOCUMENTATION BLOCK 
!                .      .    .                                       . 
! SUBPROGRAM:    SMOOTHC     SMOOTH A METEOROLOGICAL FIELD
!   PRGMMR: STAN BENJAMIN    ORG: FSL/PROFS  DATE: 90-06-15 
! 
! ABSTRACT: SHAPIRO SMOOTHER. 
! 
! PROGRAM HISTORY LOG: 
!   85-12-09  S. BENJAMIN   ORIGINAL VERSION os SMOOTH
!   14-03-03  S. Moorthi    Threading and slight cleanup
!   16-08-08  S. Moorthi    modify for cyclic domain 
! 
! USAGE:    CALL SMOOTH (FIELD,HOLD,IX,IY,SMTH) 
!   INPUT ARGUMENT LIST: 
!     FIELD    - REAL ARRAY  FIELD(IX,IY)
!                            METEOROLOGICAL FIELD
!     HOLD     - REAL ARRAY  HOLD(IX,2)
!                            HOLDING THE VALUE FOR FIELD
!     IX       - INTEGER     X COORDINATES OF FIELD
!     IY       - INTEGER     Y COORDINATES OF FIELD
!     SMTH     - REAL      
!
!   OUTPUT ARGUMENT LIST:   
!     FIELD    - REAL ARRAY  FIELD(IX,IY)
!                            SMOOTHED METEOROLOGICAL FIELD
! 
! REMARKS: REFERENCE: SHAPIRO, 1970: "SMOOTHING, FILTERING, AND
!   BOUNDARY EFFECTS", REV. GEOPHYS. SP. PHYS., 359-387.
!   THIS FILTER IS OF THE TYPE 
!         Z(I) = (1-S)Z(I) + S(Z(I+1)+Z(I-1))/2
!   FOR A FILTER WHICH IS SUPPOSED TO DAMP 2DX WAVES COMPLETELY
!   BUT LEAVE 4DX AND LONGER WITH LITTLE DAMPING,
!   IT SHOULD BE RUN WITH 2 PASSES USING SMTH (OR S) OF 0.5
!   AND -0.5.
!   
! ATTRIBUTES: 
!   LANGUAGE: FORTRAN-77 + EXTENSIONS
!   MACHINE:  NAS-9000, VAX, UNIX
!$$$ 
!**********************************************************************
!**********************************************************************

      SUBROUTINE SMOOTHC (FIELD,HOLD,IX,IY,SMTH)

!**********************************************************************
!**********************************************************************

      implicit none

      integer :: i1, i2, j, it, i, ix, iy, im1, ip1
      real    :: smth1, smth, smth2, smth3, smth4, smth5 
      real    :: sum1, sum2
      REAL      FIELD(IX,IY), HOLD (IX,2)
      integer :: iw(ix), ie(ix)
!
      SMTH1 = 0.25 * SMTH * SMTH
      SMTH2 = 0.5  * SMTH * (1.-SMTH)
      SMTH3 = (1.-SMTH) * (1.-SMTH)
      SMTH4 = (1.-SMTH)
      SMTH5 = 0.5 * SMTH
!
      do i=2,ix-1
        ie(i) = i + 1
        iw(i) = i - 1
      enddo
      ie(ix) = 1
      iw(1)  =  ix
!
      I1 = 2
      I2 = 1
       DO J=2,IY-1
         IT = I1
         I1 = I2
         I2 = IT
!$omp parallel do private(i,sum1,sum2,ip1,im1)
         DO I = 1,IX
           ip1 = ie(i)
           im1 = iw(i)
           SUM1 = FIELD (Im1,J+1) + FIELD (Im1,J-1)                 &
                + FIELD (Ip1,J+1) + FIELD (Ip1,J-1)
           SUM2 = FIELD (I  ,J+1) + FIELD (Ip1,J  )                 &
                + FIELD (I  ,J-1) + FIELD (Im1,J  )
           HOLD(I,I1) = SMTH1*SUM1 + SMTH2*SUM2 + SMTH3*FIELD(I,J)
         ENDDO
         IF (J > 2) then
!$omp parallel do private(i)
           DO I=1,IX
             FIELD(I,J-1) = HOLD(I,I2)
           ENDDO
         endif
       ENDDO

!$omp parallel do private(i)
       DO I = 1,IX
         FIELD (I,IY-1) = HOLD(I,I1)
       ENDDO

       DO I = 1,IX
         ip1 = ie(i)
         im1 = iw(i)
         FIELD(I,1)  = SMTH4 * FIELD(I,1)                             &
                     + SMTH5 * (FIELD(Im1,1) + FIELD(Ip1,1))
         FIELD(I,IY) = SMTH4 * FIELD(I,IY)                            &
                     + SMTH5 * (FIELD(Im1,IY) + FIELD(Ip1,IY))
       ENDDO


      RETURN
      END
