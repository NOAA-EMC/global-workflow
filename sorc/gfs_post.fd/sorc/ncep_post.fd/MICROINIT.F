      SUBROUTINE MICROINIT(imp_physics)
!
!-- ABSTRACT:
!     Initializes arrays for new cloud microphysics
!
!-- Program History Log:
!     02-02-08  B. Ferrier
!     04-11-19 H CHUANG - WRF VERSION
!
!-- Input argument list:
!     None
!
!-- Output argument list:
!     None
!
!-- Subprograms called:
!     Function FPVS
!
!-- Common blocks:
!     CMASSI
!     RMASS_TABLES
!     MAPOT
!     CRHgrd
!
!-- Attributes:
!     Language: FORTRAN 90
!     Machine : IBM SP
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use params_mod, only: tfrz, pi
      use cmassi_mod, only: dmrmax, t_ice, nlimax, flarge2, xmrmax, &
                            mdrmax, mdrmin, trad_ice, massi, &
                            rqr_drmin, n0r0, rqr_drmax, cn0r0, &
                            cn0r_dmrmin, cn0r_dmrmax, dmrmin
      use gridspec_mod,only : gridtype
      use rhgrd_mod, only: rhgrd
      use ctlblk_mod, only: me
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
!
      REAL, PARAMETER :: RHOL=1000.
      real ax,C_N0r0
      integer i
      integer, intent(in):: imp_physics
      real, allocatable:: MASSR(:)
      character filename*80
!
!------------------------ START EXECUTION ------------------------
!
!---  READ IN MASSI FROM LOOKUP TABLES 
!
      if(imp_physics==5)then
!-- Changes associated with the Ferrier-Aligo microphysics in NMMB:
!   NLImax is now defined internally and FLARGE2 is no longer used.
         RHgrd=0.98
         DMRmax=1.E-3
      else if(imp_physics==85)then
         RHgrd=1.         !-- Approximation, as it varies in HWRF for different grids
         NLImax=20.E3
         FLARGE2=0.2
         DMRmax=.45E-3
      else if(imp_physics==95)then
         RHgrd=1.
         NLImax=5.E3
         if(gridtype=="B") then
            FLARGE2=0.03
            DMRmax=.45E-3
         else
            FLARGE2=0.2
            DMRmax=1.E-3
         endif
      endif 

#if defined(COMMCODE)
    ! If community code, check DMRmax to determine which file to link
      if (DMRmax<=0.45E-3) then
         filename = "nam_micro_lookup.dat"
      else
         filename = "hires_micro_lookup.dat"
      endif 
#else 
    ! Else operational code and use this file
         filename = "eta_micro_lookup.dat"
#endif

      T_ICE=-40.     !-- Now used in all versions.
      XMRmax=1.E6*DMRmax 
      MDRmax=XMRmax
      allocate(MASSR(MDRmin:MDRmax))
      TRAD_ice=0.5*T_ICE+TFRZ
      
      OPEN (UNIT=1,FILE=filename,convert='big_endian',FORM="UNFORMATTED")
      DO I=1,3
        READ(1)
      ENDDO
      READ(1) MASSR
      DO I=1,5
        READ(1)
      ENDDO
      READ(1) MASSI
      CLOSE(1)
      RQR_DRmin=N0r0*MASSR(MDRmin)    ! Rain content for mean drop diameter of .05 mm
      RQR_DRmax=N0r0*MASSR(MDRmax)    ! Rain content for mean drop diameter of .45 mm
!      PI=ACOS(-1.) ! defined in params now
      C_N0r0=PI*RHOL*N0r0
      CN0r0=1.E6/SQRT(SQRT(C_N0r0))
      CN0r_DMRmin=1./(PI*RHOL*DMRmin*DMRmin*DMRmin*DMRmin)
      CN0r_DMRmax=1./(PI*RHOL*DMRmax*DMRmax*DMRmax*DMRmax)
      if(me==0)print *,'MICROINIT: MDRmin, MASSR(MDRmin)=',MDRmin,MASSR(MDRmin)
      if(me==0)print *,'MICROINIT: MDRmax, MASSR(MDRmax)=',MDRmax,MASSR(MDRmax)
!      print *,  'ETA2P:MASSI(50)= ', MASSI(50)
!      print *,  'ETA2P:MASSI(450)= ', MASSI(450)
!      print *,  'ETA2P:MASSI(1000)= ', MASSI(1000)
!
!--- Initialize saturation vapor pressure lookup tables (functions FPVS, FPVS0)
!
      CALL GPVS
!
!--- Initialize RHgrd, grid-scale RH for onset of condensation. 
!    See GSMCONST in Eta model for algorithm with grid-size dependence.
!
!      AX=111.*(DPHD**2+DLMD**2)**.5
!      AX=111.*(DYVAL/1000.**2+DXVAL/1000.**2)**.5
!      AX=MIN(100., MAX(5., AX) )
!      RHgrd=0.90+.08*((100.-AX)/95.)**.5
      deallocate(MASSR)
!--- 
      RETURN
      END
