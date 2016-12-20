      subroutine deldifs_tracers(rte,we,qme,xe,ye,teme,
     &                   rto,wo,qmo,xo,yo,temo,deltim,sl,
     &                   ls_node,coef00,k_level,hybrid,gen_coord_hybrid)
!
      use machine        , only : kind_evod
      use resol_def      , only : jcap,levr,levs,ntrac
      use namelist_def   , only : hdif_fac, hdif_fac2, slrd0, adiab
     &,                           cdamp, k2o
      use layout1        , only : len_trie_ls,len_trio_ls,
     &                            ls_dim,ls_max_node,me
      use coordinate_def , only : bk5,ck5,thref
      use deldifs_def    , only : bkly,ckly,dne,dno,rthk,rtrd,sf,
     &                            rtnp,jdel,dneh,dnoh,cthk
      use physcons       , only : rerth => con_rerth,
     &                               rd => con_rd,
     &                               cp => con_cp
      implicit none
!
      logical hybrid, gen_coord_hybrid
      real(kind=kind_evod), dimension(len_trie_ls,2,levs,ntrac) :: rte
      real(kind=kind_evod), dimension(len_trie_ls,2) :: we, qme, xe
     &,                                                 ye, teme
!
      real(kind=kind_evod), dimension(len_trio_ls,2,levs,ntrac) :: rto
      real(kind=kind_evod), dimension(len_trio_ls,2) :: wo, qmo, xo
     &,                                                 yo, temo
      real(kind=kind_evod) :: pe1, pe2, po1, po2, deltim, sl(levs)
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      real(kind=kind_evod) coef00(levs,ntrac)
!
      integer              k_level
      integer              jdelh,k,kd,ku,l,locl,n,n0,nd,np,npd
      integer              indev, indod, indev1, indev2, indod1,indod2
!
      real(kind=kind_evod) dn1,realval,fact
     &,                    rfact,rfactrd,rtrd1,fshk,wrk1,wrk2,wrk3
!
      real(kind=kind_evod), parameter :: rkappa = cp / rd
      real(kind=kind_evod), parameter :: cons0=0.0, cons1=1.0, cons2=2.0
!
      integer              indlsev,jbasev,indlsod,jbasod
!
      include 'function_indlsev'
      include 'function_indlsod'
!
!......................................................................
!
      if (k_level == 0) then
!
        allocate(rtrd(levs),rthk(levs),sf(levs),cthk(levs))
        allocate ( dne(len_trie_ls) )
        allocate ( dno(len_trio_ls) )
        allocate ( dneh(len_trie_ls) )
        allocate ( dnoh(len_trio_ls) )
        allocate ( bkly(levs) )        					! hmhj
        allocate ( ckly(levs) )        					! hmhj
        do k=1,levs
          bkly(k) = 1.0
          ckly(k) = 0.0
        enddo

        if (gen_coord_hybrid) then					! hmhj
          do  k=1,levs							! hmhj
! hmhj ak5, bk5, ck5 in gen_coord_hybrid is the same order as model index
            bkly(k) = 0.5*(bk5(k)+bk5(k+1))				! hmhj
            ckly(k) = 0.5*(ck5(k)+ck5(k+1))*rkappa/thref(k)	        ! hmhj
!           if (me == 0 )						! hmhj
!    & print*,'gen_cor sl bkly ckly  in deldif=',k,sl(k),bkly(k),ckly(k)! hmhj
          enddo								! hmhj
        else if (hybrid) then						! hmhj
          do  k=1,levs
! hmhj   sl(k) go bottom to top but bk(k) go top to bottom
            bkly(k) = 0.5*(bk5(levs-k+1)+bk5(levs-k+2))/sl(k)
!           if (me == 0 )						! hmhj
!    &  print*,'hybrid sl bkly ckly  in deldif=',k,sl(k),bkly(k),ckly(k)! hmhj
!       print*,'k_level=',k_level
          enddo
        endif
!
        n0   = 0            ! maximum wavenumber for zero diffusion
        jdel = 8            ! order of diffusion (even power to raise del)
        np   = jcap
        fshk = 1.0*hdif_fac ! extra height-dependent diffusion factor per scale height
!
        if(jcap > 170) then
!         reciprocal of time scale of diffusion at reference wavenumber np
          rtnp = hdif_fac2*(jcap/170.)**4*1.1/3600
!         rtnp =(jcap/170.)**4*1.1/3600
          fshk = 2.2*hdif_fac         ! extra height-dependent diffusion factor per scale height
        elseif(jcap == 170) then
!         reciprocal of time scale of diffusion at reference wavenumber np
          rtnp = hdif_fac2*4*3.e15/(rerth**4)*float(80*81)**2
!         rtnp = 4*3.e15/(rerth**4)*float(80*81)**2
        elseif(jcap == 126) then                                      ! hmhj
!         below has been tested in sigma-theta for 2 year cfs run     ! hmhj
          rtnp = hdif_fac2*4*3.e15/(rerth**4)*float(80*81)**2         ! hmhj
!         rtnp = 4*3.e15/(rerth**4)*float(80*81)**2                   ! hmhj
          fshk = 1.5*hdif_fac         ! extra height-dependent diffusion factor per scale height
        else
!         reciprocal of time scale of diffusion at reference wavenumber np
          rtnp = hdif_fac2*1*3.e15/(rerth**4)*float(80*81)**2
!         rtnp = 3.e15/(rerth**4)*float(80*81)**2
        endif
!
        if (me == 0) then
          write(0,6) rtnp,np,n0,jdel
    6     format(' horizontal diffusion parameters'/
     &  '   effective ',f15.6,' microhertz at wavenumber ',i4/
!    &  '   effective ',6pf10.3,' microhertz at wavenumber ',i4/
     &  '   maximum wavenumber for zero diffusion ',i4/
     &  '   order of diffusion ',i2)
          write(0,*)'tracers, including moisture, are not diffused'
        endif
!
!       slrd0=0.002        ! sigma level at which to begin rayleigh damping
!       slrd0=0.0005       ! sigma level at which to begin rayleigh damping

        rtrd1 = 0.0          ! rayleigh damping is now done in gbphys
                             ! on the grid using rayleigh_damp.f when adiab=.false.
        
        if(adiab) rtrd1 = 1./(5*86400) ! reciprocal of time scale per scale height
                                       ! above beginning sigma level for rayleigh damping
!
        wrk2 = log(1/fshk)
        wrk3 = log(1/cdamp(2))
        do k=1,levs
          if(sl(k) < slrd0) then
            wrk1 = log(slrd0/sl(k))
            if (k > levr) then
              rtrd(k) = rtrd1 * wrk1 * wrk1
            else
              rtrd(k) = rtrd1 * wrk1
            endif
          else
            rtrd(k) = 0
          endif
          rthk(k) = (sl(k))**wrk2
          cthk(k) = (sl(k))**wrk3
        enddo
!
        jdelh   = jdel/2
        npd     = max(np-n0,0)
        realval = npd*(npd+1)
        dn1     = cons2*rtnp/realval**jdelh
!       dneh = 0.0
!       dnoh = 0.0
        wrk1 = cdamp(1) / (rerth*rerth)
!
!......................................................................
!
        do locl=1,ls_max_node
               l = ls_node(locl,1)
          jbasev = ls_node(locl,2)
          indev  = indlsev(l,l)
          do n=l,jcap,2
            nd         = max(n-n0,0)
            realval    = nd*(nd+1)
            dne(indev) = dn1*realval**jdelh
            dneh(indev)=  wrk1 * realval
            indev      = indev + 1
          enddo
        enddo
!
!......................................................................
!
        do locl=1,ls_max_node
               l = ls_node(locl,1)
          jbasev = ls_node(locl,2)
          if (mod(l,2) == mod(jcap+1,2)) then
            dne(indlsev(jcap+1,l))  = cons0 ! set the even (n-l) terms of the top row to zero
            dneh(indlsev(jcap+1,l)) = cons0 ! set the even (n-l) terms of the top row to zero
          endif
        enddo
!
!......................................................................
!
        do locl=1,ls_max_node
               l = ls_node(locl,1)
          jbasod = ls_node(locl,3)
          indod  = indlsod(l+1,l)
          do n=l+1,jcap,2
            nd         = max(n-n0,0)
            realval    = nd*(nd+1)
            dno(indod) = dn1*realval**jdelh
            dnoh(indod)=  wrk1 * realval
            indod      = indod + 1
          enddo
        enddo
!
!......................................................................
!
        do locl=1,ls_max_node
               l = ls_node(locl,1)
          jbasod = ls_node(locl,3)
          if (mod(l,2) /= mod(jcap+1,2)) then
            dno(indlsod(jcap+1,l))  = cons0 ! set the odd (n-l) terms of the top row to zero
            dnoh(indlsod(jcap+1,l)) = cons0 ! set the odd (n-l) terms of the top row to zero
          endif
        enddo
!
!......................................................................
!
        do k=1,levs
          kd    = max(k-1,1)
          ku    = min(k+1,levs)
          sf(k) = sl(k)/(sl(ku)-sl(kd))/sqrt(cons2)     !constant
        enddo
!
        return
      endif
!
!......................................................................
!
      k = k_level
!!
!
!     tem = coef00(k,1) * bkly(k)
!
      do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasev = ls_node(locl,2)
         if (l == 0) then
            n0 = 2
         else
            n0 = l
         endif
         indev1 = indlsev(n0,l)
         if (mod(l,2) == mod(jcap+1,2)) then
           indev2 = indlsev(jcap+1,l)
         else
           indev2 = indlsev(jcap  ,l)
         endif
!!       do n = n0, jcap+1, 2
         do indev = indev1 , indev2
 
           fact             = deltim*dne(indev)*rthk(k)
           rfact            = cons1/(cons1+fact)
           rfactrd          = cons1/(cons1+fact+deltim*rtrd(k))
 
           we(indev,1)      = we(indev,1)*rfactrd
           we(indev,2)      = we(indev,2)*rfactrd
 
           if (k > k2o) then
             fact           = deltim*dneh(indev)*cthk(k)
     &                      + fact
           endif
           wrk1             = cons1/(cons1+fact+deltim*rtrd(k))
           xe(indev,1)      = xe(indev,1)*wrk1
           xe(indev,2)      = xe(indev,2)*wrk1
!          xe(indev,1)      = xe(indev,1)*rfactrd
!          xe(indev,2)      = xe(indev,2)*rfactrd
 
           pe1 = bkly(k)*qme(indev,1) + ckly(k)*teme(indev,1)       	! hmhj
           pe2 = bkly(k)*qme(indev,2) + ckly(k)*teme(indev,2)       	! hmhj
 
           ye(indev,1)      = ( ye(indev,1)
     &                      +   fact*coef00(k,1)* pe1 )*rfact    	! hmhj
 
           ye(indev,2)      = ( ye(indev,2)
     &                      +   fact*coef00(k,1)* pe2 )*rfact   	! hmhj
 
         enddo
       enddo
!
!......................................................................
!
!      do l = 0, jcap
       do locl=1,ls_max_node
              l = ls_node(locl,1)
         jbasod = ls_node(locl,3)
         indod1 = indlsod(l+1,l)
         if (mod(l,2) == mod(jcap+1,2)) then
           indod2 = indlsod(jcap  ,l)
         else
           indod2 = indlsod(jcap+1,l)
         endif
!        do n = l+1, jcap+1, 2
         do indod = indod1 , indod2
 
           fact             = deltim*dno(indod)*rthk(k)
           rfact            = cons1/(cons1+fact)
           rfactrd          = cons1/(cons1+fact+deltim*rtrd(k))
 
           wo(indod,1)      = wo(indod,1)*rfactrd
           wo(indod,2)      = wo(indod,2)*rfactrd
 
           if (k > k2o) then
             fact           = deltim*dnoh(indod)*cthk(k)
     &                      + fact
           endif
           wrk1             = cons1/(cons1+fact+deltim*rtrd(k))
           xo(indod,1)      = xo(indod,1)*wrk1
           xo(indod,2)      = xo(indod,2)*wrk1
!          xo(indod,1)      = xo(indod,1)*rfactrd
!          xo(indod,2)      = xo(indod,2)*rfactrd

           po1 = bkly(k)*qmo(indod,1) + ckly(k)*temo(indod,1)       	! hmhj
           po2 = bkly(k)*qmo(indod,2) + ckly(k)*temo(indod,2)       	! hmhj
 
           yo(indod,1)      = ( yo(indod,1)
     &                      +   fact*coef00(k,1)* po1 )*rfact   	! hmhj
 
           yo(indod,2)      = ( yo(indod,2)
     &                      +   fact*coef00(k,1)* po2)*rfact    	! hmhj
 
         enddo
       enddo
!
!     print *,' leave deldifs_tracers '                                 !  hmhj
!!
      return
      end
