      module PMICRPH_mod

! -----  Constants related to microphysics
!          -- computed in paramr.f
!

!      REAL ABER1(31),ABER2(31)
! LOOKUP TABLE FOR A1 AND A2 IN BERGERON PROCESS

      REAL PI,RON,SON,GON,BR,BS,BG,DRAIN,DSNOW,          &
      DGRAUPEL,RON2,DIACE_min,                           &
      drain2,dsnow2,                                     &
      TOPR,TOPS,TOPG,ARAIN,ASNOW,AGRAUPEL,               &
      TNO,ATO,XSMAX,BERC1,BP,AP,CNP,FRD1,FRA1,EFIS,      &
      EFIR,EFSR,EFCS,EFGI,EFGC,EFGR,EFGS,EFCR,ACRIS,     &
      BACRIS,CIR,CIRF,cpiacr0,cpiacr1,cpiacr2,cpiacr3,   &
      FRAIN,FSNOW,FGRAUPEL,CSR,CRS,                      &
      ACRCS,BACRCS,RMC,ACRLS,BACLS,ACRCG,BACRCG,ACRIG,   &
      BACRIG,CRG,CSG,DEPG1,DEPG2,DEPG3,DEPG4,DEPS1,      &
      DEPS2,DEPS3,DEPS4,ACRCR,BACRCR,DEPR1,DEPR2,DEPR3,  &
      DEPR4,PSM1,PSM2,PSM3,PSM4,PGM1,PGM2,PGM3,PGM4,     &
      CW,HGFR,XM01,CNP1,DICE,C1,ALPHA1,BETA1,GAMMA3      &
!jmb--removed INT0 frm the real declaration since declared integer blo
      ,CONST1A,CONST1B,XM0S,XR0S,XM0G                    &
      ,ACRCS_new,BACRCS_new,ACRCG_new,BACRCG_new         &
      ,const_ns1,const_ns2,const_ng1,const_ng2,xr0g      & 
      ,r1,slor_r1,slos_r1,slog_r1,rho_not                &
      ,qck1,qcth,ron_min,qr0,delqr0,const1r,const2r      &
      ,xnu

      INTEGER INT0

     end module PMICRPH_mod
