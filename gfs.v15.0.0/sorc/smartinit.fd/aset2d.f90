  MODULE aset2d
!=======================================================================
!  Initialize 2-D atmosphere arrays
!=======================================================================
   REAL,    ALLOCATABLE :: PSFC(:,:),ZSFC(:,:),PBLMARK(:,:),BLR(:,:)
   REAL,    ALLOCATABLE :: T950(:,:),T850(:,:),T700(:,:),T500(:,:)
   REAL,    ALLOCATABLE :: RH850(:,:),RH700(:,:),U10(:,:),V10(:,:)
   REAL,    ALLOCATABLE :: T2(:,:),Q2(:,:),BLI(:,:),REFC(:,:),GUST(:,:)
   REAL,    ALLOCATABLE :: T1(:,:),D2(:,:),WX(:,:),VIS(:,:)
   REAL,    ALLOCATABLE :: LCLD(:,:),MCLD(:,:),HCLD(:,:),TCLD(:,:)

  END MODULE aset2d



