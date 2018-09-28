C     ...ANDREW COLLETTI                                                00142700
C                                                                       00142800
C     ...APRIL 17,1981                                                  00142900
C                                                                       00143000
C     ...MAKE ISOTACHS FROM U'S AND V'S IN A VARIABLE LENGTH FIELD      00143100
C  **************************************************************       00143200
      SUBROUTINE ISOTAC(U,V,K,L,T)
C     ...U=INPUT U FIELD                                                00143400
C     ...V=INPUT V FIELD                                                00143500
C     ...K=1ST DIMENSION OF FIELD                                       00143600
C     ...L=2ND DIMENSION OF FIELD                                       00143700
C     ...T=OUTPUT FIELD WITH RESULTANT ISOTACHS                         00143800
      REAL       U(K,L)
      REAL       V(K,L)
      REAL       T(K,L)
C                                                                       00144200
      DO 10 J=1,L
         DO 9 I=1,K
            T(I,J) = ABS(SQRT((U(I,J)**2) + (V(I,J)**2)))
    9    CONTINUE
   10 CONTINUE
      RETURN
      END
