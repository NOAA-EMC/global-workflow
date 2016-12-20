      subroutine hyb2press_gc(njeff,nsize_ar,pgr,thgr,prsi,prsl
     &,                                              prsik, prslk)
!
! hmhj : this is modified hybrid by finite difference from henry juang
!        thgr can be t or h
!
 
      use machine , only : kind_phys
 
      use resol_def
      use namelist_def
      use vert_def
      use coordinate_def
      use physcons, cp => con_cp , rd => con_rd, rk => con_rocp
      implicit none

      real (kind=kind_phys), parameter :: pt01=0.01, rkappa = cp / rd
      real(kind=kind_phys) prsl(nsize_ar,levs),prslk(nsize_ar,levs)
      real(kind=kind_phys) prsi(nsize_ar,levs+1),prsik(nsize_ar,levs+1)
      real(kind=kind_phys) pgr(nsize_ar)
      real(kind=kind_phys) thgr(nsize_ar,levs)

!     real(kind=kind_phys) ppi(njeff,levs+1)
      real(kind=kind_phys) tki(njeff,levs+1)
      real(kind=kind_phys) tkrt0
 
!     logical adjusted
      integer njeff,nsize_ar
      integer i,k
 
!     print *,' enter  hyb2press_gc_h '
!     print *,' in hyb2press_gc_h vertcoord_id =',vertcoord_id


      tki = 0.0
! -------------------------------
      if( vertcoord_id.eq.3. ) then
      do k=2,levs
        do i=1,njeff
          tkrt0 = (thgr(i,k-1)+thgr(i,k))/(thref(k-1)+thref(k))
          tki (i,k)=ck5(k)*tkrt0**rkappa
        enddo
      enddo
      endif	! vertcoord_id=3
! -----------------------------
      do k=1,levp1
        do i=1,njeff
          prsi(i,k)  = ak5(k)+bk5(k)*pgr(i)+tki(i,k) ! si are now pressures
          prsik(i,k) = (prsi(i,k)*pt01) ** rk
        enddo
      enddo
!
!     if( vctype.eq.3. ) then
!      do k=1,levp1
!       do i=1,njeff
!        ppi(i,k)=prsi(i,k)
!       enddo
!      enddo
!      call adj_dpp(njeff,nsize_ar,levs,ppi,dpdti,adjusted)
!      if( adjusted ) then
!      do k=2,levs
!       do i=1,njeff
!        prsi(i,k)=ppi(i,k)
!       enddo
!      enddo
!      endif
!     endif
!
      do k=1,levs
        do i=1,njeff
          prsl(i,k)  = (prsi(i,k)+prsi(i,k+1))*0.5
          prslk(i,k) = (prsl(i,k)*pt01) ** rk
          if( prsi(i,k)-prsi(i,k+1) .le. 0.0 ) then
            print *,' dpp adjust fails at ',i,k,' dpp=',
     &      prsi(i,k)-prsi(i,k+1)
          endif
        enddo
      enddo

!     print *,' leave hyb2press_gc_h. '

      return
      end
                                                                                
      subroutine adj_dpp(im,ix,km,ppi,dpdti,adjusted)
                                                                                
      use machine , only : kind_phys
      use resol_def
      use coordinate_def
                                                                                
      implicit none
      integer im,ix,km
      real dpdti(ix,levs+1)
      real ppi(im,levs+1)
      real, parameter :: dppmin=0.000, dppmod=0.002001
      real dpp,ppmean,ppsave
      integer i,k
      logical check,adjusted
       
       
      adjusted=.false.
      dpdti=0.0
      do i=1,im
        check=.true.
        do while( check )
          check=.false.
          do k=2,levs-1
            dpp=ppi(i,k)-ppi(i,k+1)
            if( dpp.lt.dppmin ) then
              print *,' thin layer at i k ',i,k,
     &                  ppi(i,k-1),ppi(i,k),ppi(i,k+1),ppi(i,k+2)
              ppmean=0.5*(ppi(i,k)+ppi(i,k+1))
              ppsave = ppi(i,k)
              ppi(i,k  )=ppmean+0.5*dppmod
              dpdti(i,k) = dpdti(i,k) + ppi(i,k)-ppsave
              ppsave = ppi(i,k+1)
              ppi(i,k+1)=ppmean-0.5*dppmod
              dpdti(i,k+1) = dpdti(i,k+1) + ppi(i,k+1)-ppsave
              print *,' adjust layer at i k ',i,k,
     &                  ppi(i,k-1),ppi(i,k),ppi(i,k+1),ppi(i,k+2)
              adjusted=.true.
              check=.true.
            endif
          enddo
        enddo
      enddo
                                                                                
      return
      end

