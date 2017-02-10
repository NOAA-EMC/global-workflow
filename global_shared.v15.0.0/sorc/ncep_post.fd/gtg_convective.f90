!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!    (c) University Corporation for Atmospheric Research (UCAR) 2013.  All
!    rights reserved.  The Government's right to use this data and/or
!    software (the "Work") is restricted, per the terms of Cooperative
!    Agreement (ATM (AGS)-0753581 10/1/08) between UCAR and the National
!    Science Foundation, to a *nonexclusive, nontransferable,
!    irrevocable, royalty-free license to exercise or have exercised for
!    or on behalf of the U.S. throughout the world all the exclusive
!    rights provided by copyrights.  Such license, however, does not
!    include the right to sell copies or phonorecords of the copyrighted
!    works to the public.  The Work is provided "AS IS" and without
!    warranty of any kind.  UCAR EXPRESSLY DISCLAIMS ALL OTHER
!    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF
!    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*

      subroutine convec_indices_gtg(pm,Tm,ugm,vgm,zm,qvm,TI1,TI12D,
     1  nx,ny,nz,imin,imax,jmin,jmax,pLCL,TLCL,zLCL,PWTR,LIFT,CAPE,CINS,
     2  EQLV,LFCV,SHOW,VTOT,CTOT,TOTALS,SWEAT,KINDEX,BRCH,MLTH,MLMR,
     3  pratem,ltngm,ioutputflag,outDir,ic,jc,printflag,iprt)
!     --- Computes convective parameters and store to disk as 3XX.F
!     --- Inputs:
!         nx: number of grid pts in x (nominally west-east direction)
!         ny: number of grid pts in y (nominally south-north direction)
!         nz: number of vertical levels (grid pts in z)
!         pm(nx,ny,nz): grid pt. pressure, Pa
!         Tm(nx,ny,nz): grid pt. temperature, K
!         ugm(nx,ny,nz): grid relative x velocity (m/s)
!         vgm(nx,ny,nz): grid relative y velocity (m/s)
!         zm(nx,ny,nz): grid pt. altitude: geopotential m
!         qvm(nx,ny,nz): grid pt. absolute humidity (kg/kg)
!         imin: minimum grid pt. to compute indices in x direction (nominally e-w) (nd)
!         imax: maximum grid pt. to compute indices in x direction (nominally e-w) (nd)
!         jmin: minimum grid pt. to compute indices in y direction (nominally n-s) (nd)
!         jmax: maximum grid pt. to compute indices in y direction (nominally n-s) (nd)
!         printflag: > 0 to print index values at (ic,jc)
!         ioutputflag > 0 to write individual indices to disk
!         ioutputflag > 0 to write individual indices to disk as *.F files
!         outDir: output (*.F) directory 
!     --- Work arrays:
!         TI1,TI12d
!     --- On output pLCL(i,j)=TI1(i,j,1)      PRESSURE AT THE LCL (mb)
!     ---           TLCL(i,j)=TI1(i,j,2)      TEMP AT LCL (DEG K)
!     ---           zLCL(i,j)=TI1(i,j,3)      HEIGHT OF LCL (M)  
!     ---           PWTR(i,j)=TI1(i,j,4)      precititable water (cm)
!     ---           LIFT(i,j)=TI1(i,j,5)      lifted index at 500 mb (C) 
!     ---           CAPE(i,j)=TI1(i,j,6)      Convective Available Potential Energy (J/kg)
!     ---           CINS(i,j)=TI1(i,j,7)      Convective Inhibition (J/kg) 
!     ---           EQLV(i,j)=TI1(i,j,8)      Equilibrum Level (m)
!     ---           LFCV(i,j)=TI1(i,j,9)      Level of Free Convection (m)
!     ---           SHOW(i,j)=TI1(i,j,10)     Showalter index (K)
!     ---           VTOT(i,j)=TI1(i,j,11)     Vertical totals index (C)
!     ---           CTOT(i,j)=TI1(i,j,12)     Cross totals index (C)
!     ---           TOTALS(i,j)=TI1(i,j,13)   Totals total index (C)
!     ---           SWEAT(i,j)=TI1(i,j,14)    Sweat index 
!     ---           KINDEX(i,j)=TI1(i,j,15)   K index (C)
!     ---           BRCH(i,j)=TI1(i,j,16)     Bulk Richardson number = CAPE / ( 0.5 * dU/dz**2 )
!     ---           MLTH(i,j)=TI1(i,j,17)     Mean mixed layer potential temperature (K)
!     ---           MLMR(i,j)=TI1(i,j,18)     Mean mixed layer mixing ratio (g/kg)
!     ---           pratem(i,j)=TI1(i,j,19)   NWP precip rate (pass through)
!     ---           ltngm(i,j)=TI1(i,j,20)    NWP lighning flag (pass through)
!         3XX.F 2d convective parameters files output if ioutputflag > 0 
!         300.F Pressure at the LCL (mb)
!         301.F Temperature at the LCL (deg K)
!         302.F Height of the LCL (m)
!         303.F Precipitable water (cm)
!         304.F Lifted Index at 500 mb (deg C)
!         305.F Convective Available Potential Energy (CAPE)  (J/kg)
!         306.F Convective inhibition (CINS) (J/kg) 
!         307.F Equilibrium Level (mb)
!         308.F Level of Free Convection (mb)
!         309.F Showalter Index (deg K)
!         310.F Vertical totals index (deg C)
!         311.F Cross totals index (deg C)
!         312.F Total totals Index (deg C)
!         313.F SWEAT Index 
!         314.F K Index (deg C)
!         315.F Bulk Richardson Number= CAPE / ( 0.5 * dU/dz**2 )
!         316.F Mean mixed layer potential temperature (deg K)
!         317.F Mean mixed layer mixing ratio (g/kg)
!         318.F NWP model precip rate (kg m-2 s-1)
!         319.F NWP grid point lightning flag (0 or 1)
!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
!    (c) University Corporation for Atmospheric Research (UCAR) 2013.  All
!    rights reserved.  The Government's right to use this data and/or
!    software (the "Work") is restricted, per the terms of Cooperative
!    Agreement (ATM (AGS)-0753581 10/1/08) between UCAR and the National
!    Science Foundation, to a *nonexclusive, nontransferable,
!    irrevocable, royalty-free license to exercise or have exercised for
!    or on behalf of the U.S. throughout the world all the exclusive
!    rights provided by copyrights.  Such license, however, does not
!    include the right to sell copies or phonorecords of the copyrighted
!    works to the public*.  The Work is provided "AS IS" and without
!    warranty of any kind.  UCAR EXPRESSLY DISCLAIMS ALL OTHER
!    WARRANTIES, INCLUDING, BUT NOT LIMITED TO, ANY IMPLIED WARRANTIES OF
!    MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE./
!    *=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*=*
      implicit none
!-----------------------------------------------------------------------
!     --- inputs declarations
      integer nx,ny,nz
      real    ugm(nx,ny,nz),vgm(nx,ny,nz),zm(nx,ny,nz),
     1        pm(nx,ny,nz),Tm(nx,ny,nz),qvm(nx,ny,nz)
      real    pLCL(nx,ny),TLCL(nx,ny),zLCL(nx,ny),
     1        PWTR(nx,ny),LIFT(nx,ny),CAPE(nx,ny),
     2        CINS(nx,ny),EQLV(nx,ny),LFCV(nx,ny),
     3        SHOW(nx,ny),VTOT(nx,ny),CTOT(nx,ny),
     4        TOTALS(nx,ny),SWEAT(nx,ny),KINDEX(nx,ny),
     5        BRCH(nx,ny),MLTH(nx,ny),MLMR(nx,ny),
     6        pratem(nx,ny),ltngm(nx,ny)
      integer imin,imax,jmin,jmax
      integer ic,jc,printflag,iprt,ioutputflag
      character*200 outDir
!     --- outputs declarations
      real    TI1(nx,ny,nz)
!     --- work arrays declarations
      real    ti12d(nx,ny)
!-----------------------------------------------------------------------
      include 'consts.inc'
      include 'fldsz.inc'
!-----------------------------------------------------------------------
      integer i,j,k,idx,ierr
!     --- Convective parameters declarations
      real    pLCLij,TLCLij,zLCLij,PWTRij,LIFTij,CAPEij,CINSij,EQLVij,
     1        LFCVij,SHOWij,VTOTij,CTOTij,TOTALSij,SWEATij,KINDEXij,
     2        BRCHij,MLTHij,MLMRij
      real    CAPEMAX
      real    qmin,qmax
      integer icapemx,jcapemx,ipcflag
      character*24 ciname(20)
      data ciname/'pLCL','TLCL','zLCL','PWTR','LIFT','CAPE',
     1            'CINS','EQLV','LFCV','SHOW','VTOT','CTOT',
     2            'TOTALS','SWEAT','KINDEX','BRCH','MLTH','MLMR',
     3            'PRATEM','LTNGM'/
!-----------------------------------------------------------------------
!
      if(printflag.ge.1) write(iprt,*) 'enter convec_indices_gtg'
      klev=nz
      call TIinit(TI1,nx,ny,nz)
      CAPEMAX=0.
      icapemx=-1
      jcapemx=-1
      do j=jmin,jmax
      do i=imin,imax
        do k = 1, klev
          p(k) = pm(i,j,k)/100.           ! mb
          T(k) = Tm(i,j,k)-T0C            ! C
          u(k) = ugm(i,j,k)
          v(k) = vgm(i,j,k)
          z(k) = zm(i,j,k)
          q(k) = qvm(i,j,k)
          if((printflag.ge.2).and.(i.eq.ic .and. j.eq.jc)) then
            write(iprt,*) 'i,j,k,z,p,T,u,v=',i,j,k,z(k),p(k),T(k),
     1        u(k),v(k)
          endif
        enddo  ! k loop
!       --- Input z (m), p (mb), T (C), q (nd), u,v (m/s)
        ipcflag=0
        if((printflag.ge.1).and.(i.eq.ic .and. j.eq.jc)) 
     1    ipcflag=printflag
        call convect_params(z,p,T,q,u,v,klev,pLCLij,TLCLij,zLCLij,
     1   PWTRij,LIFTij,CAPEij,CINSij,EQLVij,LFCVij,SHOWij,VTOTij,CTOTij,
     2   TOTALSij,SWEATij,KINDEXij,BRCHij,MLTHij,MLMRij,ipcflag,iprt)
         if(CAPEij.gt.CAPEMAX) then
           CAPEMAX=CAPEij
           icapemx=i
           jcapemx=j
         endif
        if((printflag.ge.1).and.(i.eq.ic .and. j.eq.jc)) then
          write(iprt,*) 'pLCL,TLCL,zLCL=',pLCLij,TLCLij,zLCLij
          write(iprt,*) 'PWTR=',PWTRij
          write(iprt,*) 'LIFT=',LIFTij
          write(iprt,*) 'CAPE,CINS=',CAPEij,CINSij
          write(iprt,*) 'EQLV,LFCV=',EQLVij,LFCVij
          write(iprt,*) 'SHOW=',SHOWij
          write(iprt,*) 'VTOT,CTOT,TOTALS=',VTOTij,CTOTij,TOTALSij
          write(iprt,*) 'SWEAT=',SWEATij
          write(iprt,*) 'KINDEX=',KINDEXij
          write(iprt,*) 'BRCH=',BRCHij
          write(iprt,*) 'MLTH,MLMR=',MLTHij,MLMRij
          write(iprt,*) 'pratem=',pratem(i,j)
          write(iprt,*) 'ltngm=',NINT(ltngm(i,j))
        endif
        pLCL(i,j)=pLCLij      ! PRESSURE AT THE LCL (mb)
        TLCL(i,j)=TLCLij      ! TEMP AT LCL (DEG K)
        zLCL(i,j)=zLCLij      ! HEIGHT OF LCL (M)  
        PWTR(i,j)=PWTRij      ! precititable water (cm)
        LIFT(i,j)=LIFTij      ! lifted index at 500 mb (C) 
        CAPE(i,j)=CAPEij      ! Convective Available Potential Energy (J/kg)
        CINS(i,j)=CINSij      ! Convective Inhibition (J/kg) 
        EQLV(i,j)=EQLVij      ! Equilibrum Level (mb)
        LFCV(i,j)=LFCVij      ! Level of Free Convection (mb)
        SHOW(i,j)=SHOWij      ! Showalter index (K)
        VTOT(i,j)=VTOTij      ! Vertical totals index (C)
        CTOT(i,j)=CTOTij      ! Cross totals index (C)
        TOTALS(i,j)=TOTALSij  ! Totals total index (C)
        SWEAT(i,j)=SWEATij    ! Sweat index 
        KINDEX(i,j)=KINDEXij  ! K index (C)
        BRCH(i,j)=BRCHij      ! Bulk Richardson number = CAPE / (0.5 * dU/dz**2)
        MLTH(i,j)=MLTHij      ! Mean mixed layer potential temperature (K)
        MLMR(i,j)=MLMRij      ! Mean mixed layer mixing ratio (g/kg)
!
        TI1(i,j,1)=pLCLij     ! PRESSURE AT THE LCL (mb)
        TI1(i,j,2)=TLCLij     ! TEMP AT LCL (DEG K)
        TI1(i,j,3)=zLCLij     ! HEIGHT OF LCL (M)  
        TI1(i,j,4)=PWTRij     ! precititable water (cm)
        TI1(i,j,5)=LIFTij     ! lifted index at 500 mb (C) 
        TI1(i,j,6)=CAPEij     ! Convective Available Potential Energy (J/kg)
        TI1(i,j,7)=CINSij     ! Convective Inhibition (J/kg) 
        TI1(i,j,8)=EQLVij     ! Equilibrum Level (mb)
        TI1(i,j,9)=LFCVij     ! Level of Free Convection (mb)
        TI1(i,j,10)=SHOWij    ! Showalter index (K)
        TI1(i,j,11)=VTOTij    ! Vertical totals index (C)
        TI1(i,j,12)=CTOTij    ! Cross totals index (C)
        TI1(i,j,13)=TOTALSij  ! Totals total index (C)
        TI1(i,j,14)=SWEATij   ! Sweat index 
        TI1(i,j,15)=KINDEXij  ! K index (C)
        TI1(i,j,16)=BRCHij    ! Bulk Richardson number = CAPE / (0.5 * dU/dz**2)
        TI1(i,j,17)=MLTHij    ! Mean mixed layer potential temperature (K)
        TI1(i,j,18)=MLMRij    ! Mean mixed layer mixing ratio (g/kg)
        TI1(i,j,19)=pratem(i,j)  ! precip rate (pass through)
        TI1(i,j,20)=ltngm(i,j)  ! lighning flag (pass through)
      enddo
      enddo
      if(printflag.ge.1) then
        write(iprt,*) 'CAPEMAX at i,j=',CAPEMAX,icapemx,jcapemx
      endif
      if(ioutputflag.gt.0) then
!       --- Write variables as 2D arrays to 3XX.F file
        do k=1,20
          idx=300+(k-1)
          do j=1,ny
          do i=1,nx
            TI12D(i,j)=TI1(i,j,k)
          enddo
          enddo
          call writeF(TI12D,nx,ny,1,idx,ciname(k),outDir,printflag,iprt,
     1      ierr)
          if(printflag.ge.1) then
            call TIstats2D(TI12d,nx,ny,imin,imax,jmin,jmax,qmax,qmin,
     1       idx,iprt,ciname(k))
          endif
        enddo
      endif
!
      if(printflag.ge.1) then
        write(iprt,*) 'i,j,pLCL  =',ic,jc,pLCL(ic,jc)
        write(iprt,*) 'i,j,TLCL  =',ic,jc,TLCL(ic,jc)
        write(iprt,*) 'i,j,zLCL  =',ic,jc,zLCL(ic,jc)
        write(iprt,*) 'i,j,PWTR  =',ic,jc,PWTR(ic,jc)
        write(iprt,*) 'i,j,LIFT  =',ic,jc,LIFT(ic,jc)
        write(iprt,*) 'i,j,CAPE  =',ic,jc,CAPE(ic,jc)
        write(iprt,*) 'i,j,CINS  =',ic,jc,CINS(ic,jc)
        write(iprt,*) 'i,j,EQLV  =',ic,jc,EQLV(ic,jc)
        write(iprt,*) 'i,j,LFCV  =',ic,jc,LFCV(ic,jc)
        write(iprt,*) 'i,j,SHOW  =',ic,jc,SHOW(ic,jc)
        write(iprt,*) 'i,j,VTOT  =',ic,jc,VTOT(ic,jc)
        write(iprt,*) 'i,j,CTOT  =',ic,jc,CTOT(ic,jc)
        write(iprt,*) 'i,j,TOTALS=',ic,jc,TOTALS(ic,jc)
        write(iprt,*) 'i,j,SWEAT =',ic,jc,SWEAT(ic,jc)
        write(iprt,*) 'i,j,KINDEX=',ic,jc,KINDEX(ic,jc)
        write(iprt,*) 'i,j,BRCH  =',ic,jc,BRCH(ic,jc)
        write(iprt,*) 'i,j,MLTH  =',ic,jc,MLTH(ic,jc)
        write(iprt,*) 'i,j,MLMR  =',ic,jc,MLMR(ic,jc)
        write(iprt,*) 'i,j,pratem=',ic,jc,pratem(ic,jc)
        write(iprt,*) 'i,j,ltngm =',ic,jc,ltngm(ic,jc)
!
        write(iprt,*) 'CAPEMAX at i,j=',CAPEMAX,icapemx,jcapemx
        write(iprt,*) 'exit  convec_indices_gtg'
      endif
!
      return
      end
!
      include 'convect_params32.f'
