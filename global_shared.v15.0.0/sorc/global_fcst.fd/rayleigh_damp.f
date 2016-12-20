      subroutine rayleigh_damp(im,ix,iy,km,a,b,c,u1,v1,dt,cp)
!
!   ********************************************************************
! ----->  i m p l e m e n t a t i o n    v e r s i o n   <----------
!
!          --- rayleigh friction with total energy conserving ---
!              ----------------     -----------------------
!
!------ friction coefficient is based on deldif ----
!----------------------------------------------------------------------c
!    use
!        routine is called from gbphys  (after call to gwdps)
!
!    purpose
!        using the gwd parameterizations of ps-glas and ph-
!        gfdl technique.  the time tendencies of u v are 
!        altered to include/mimic the effect of non-stationary 
!        gravity wave drag from convection, frontgenosis,
!        wind shear etc.  loss of kinetic energy form gwd drag
!        is converted into internal energy.   
!
!  input
!        a(iy,km)  non-lin tendency for v wind component
!        b(iy,km)  non-lin tendency for u wind component
!        c(iy,km)  non-lin tendency for temperature
!        u1(ix,km) zonal wind m/sec  at t0-dt
!        v1(ix,km) meridional wind m/sec at t0-dt
!        t1(ix,km) temperature deg k at t0-dt
!
!        dt  time step    secs
!        sl(n)   p/psfc at middle of layer n
!
!  output
!        a, b, c as augmented by tendency due to rayleigh friction
!   ********************************************************************
      use machine , only : kind_phys
      use resol_def      , only : levr
      use vert_def       , only : sl
      use namelist_def   , only : slrd0
      implicit none
      integer im, ix, iy, km
      real(kind=kind_phys) dt, cp
      real(kind=kind_phys) a(iy,km),    b(iy,km),   c(iy,km),
     &                     u1(ix,km),   v1(ix,km)
      real(kind=kind_phys) rtrd
      real(kind=kind_phys) cons1, cons2, half
      real(kind=kind_phys) dtaux, dtauy, wrk1, rtrd1, rfactrd
      real(kind=kind_phys) eng0, eng1
      integer i, k, kstr
!
!     some constants
!
      cons1 = 1.0
      cons2 = 2.0
      half  = cons1/cons2
!-----initialize some arrays
!
      rtrd1 = 1./(5*86400) ! reciprocal of time scale per scale height
                           !  above beginning sigma level for rayleigh
                           !  damping
!
      do k=1,km
        if(sl(k) < slrd0) then
          wrk1 = log(slrd0/sl(k))
          if (k > levr) then
            rtrd = rtrd1 * wrk1 * wrk1
          else
            rtrd = rtrd1 * wrk1
          endif

          do i = 1,im
            rfactrd    = cons1/(cons1+dt*rtrd)
            dtaux      = u1(i,k)*(rfactrd-cons1)/dt
            dtauy      = v1(i,k)*(rfactrd-cons1)/dt
            eng0       = half*(u1(i,k)**cons2+v1(i,k)**cons2)
            eng1       = half*((u1(i,k)+dtaux*dt)**cons2+
     &                         (v1(i,k)+dtauy*dt)**cons2)
            a(i,k)     = a(i,k) + dtauy
            b(i,k)     = b(i,k) + dtaux
            c(i,k)     = c(i,k) + max((eng0-eng1),0.0)/cp/dt
          enddo
        endif
      enddo

      return
      end
