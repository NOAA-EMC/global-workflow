      SUBROUTINE TRIDI2T3(L,N,CL,CM,CU,R1,R2,AU,A1,A2)
!
      USE MACHINE , ONLY : kind_phys
      implicit none
      integer   L,N
!
      real(kind=kind_phys)
     &          CL(L,2:N),CM(L,N),CU(L,N-1),R1(L,N),R2(L,N),
     &          AU(L,N-1),A1(L,N),A2(L,N)
!
      real(kind=kind_phys) fk
      integer              k,i
!
      DO I=1,L
        FK      = 1. / CM(I,1)
        AU(I,1) = FK*CU(I,1)
        A1(I,1) = FK*R1(I,1)
        A2(I,1) = FK*R2(I,1)
      ENDDO
      DO K=2,N-1
        DO I=1,L
          FK      = 1./(CM(I,K)-CL(I,K)*AU(I,K-1))
          AU(I,K) = FK*CU(I,K)
          A1(I,K) = FK*(R1(I,K)-CL(I,K)*A1(I,K-1))
          A2(I,K) = FK*(R2(I,K)-CL(I,K)*A2(I,K-1))
        ENDDO
      ENDDO
      DO I=1,L
        FK      = 1./(CM(I,N)-CL(I,N)*AU(I,N-1))
        A1(I,N) = FK*(R1(I,N)-CL(I,N)*A1(I,N-1))
        A2(I,N) = FK*(R2(I,N)-CL(I,N)*A2(I,N-1))
      ENDDO
      DO K=N-1,1,-1
        DO I=1,L
          A1(I,K) = A1(I,K) - AU(I,K)*A1(I,K+1)
          A2(I,K) = A2(I,K) - AU(I,K)*A2(I,K+1)
        ENDDO
      ENDDO
!-----------------------------------------------------------------------
      RETURN
      END
