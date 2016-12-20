      subroutine spect_to_grid
!    x    (trie_zs,trio_zs,trie_ps,trio_ps,
     x    (trie_ps,trio_ps,
     x     trie_di,trio_di,trie_ze,trio_ze,
     x     trie_te,trio_te,trie_rq,trio_rq,
     &     zsg,psg,uug,vvg,teg,rqg,dpg,
     x     ls_node,ls_nodes,max_ls_nodes,
     x     lats_nodes_r,global_lats_r,lonsperlar,
     x     epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r)
!!
!! hmhj - this routine do spectral to grid transform in model partial reduced grid
!!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use coordinate_def 
      use tracer_const
      use physcons, fv => con_fvirt, rk => con_rocp, rerth => con_rerth,
     &              grav => con_g
      implicit none
!!
!     real(kind=kind_evod) trie_zs(len_trie_ls,2)
!     real(kind=kind_evod) trio_zs(len_trio_ls,2)

      real(kind=kind_evod) trie_ps(len_trie_ls,2)
      real(kind=kind_evod) trio_ps(len_trio_ls,2)
      real(kind=kind_evod) trie_di(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_di(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_ze(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_ze(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_te(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_te(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_rq(len_trie_ls,2,levh)
      real(kind=kind_evod) trio_rq(len_trio_ls,2,levh)
!
!!!!  parameter            ( lota = 3*levs+1*levh+1 )

      real(kind=kind_evod) trie_ls(len_trie_ls,2,lota)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,lota)
      real(kind=kind_evod) for_gr_r_1(lonrx,lota,lats_dim_r)
      real(kind=kind_evod) for_gr_r_2(lonrx,lota,lats_dim_r)
!
!     real(kind=kind_evod) trie_ls(len_trie_ls,2,lota+1)
!     real(kind=kind_evod) trio_ls(len_trio_ls,2,lota+1)
!!
!     real(kind=kind_evod) for_gr_r_1(lonrx*(lota+1),lats_dim_r)
!     real(kind=kind_evod) for_gr_r_2(lonrx*(lota+1),lats_dim_r)
!
      real(kind=kind_rad) zsg(lonr,lats_node_r)
      real(kind=kind_rad) psg(lonr,lats_node_r)
      real(kind=kind_rad) uug(lonr,lats_node_r,levs)
      real(kind=kind_rad) vvg(lonr,lats_node_r,levs)
      real(kind=kind_rad) teg(lonr,lats_node_r,levs)
      real(kind=kind_rad) rqg(lonr,lats_node_r,levh)
      real(kind=kind_rad) dpg(lonr,lats_node_r,levs)
!
!
      integer              ls_node(ls_dim,3)
!
!cmr  ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!cmr  ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!cmr  ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer              ls_nodes(ls_dim,nodes)
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_r(nodes)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
!     integer dimg
!
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
!
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
!
      real(kind=kind_evod)   plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnod_r(len_trio_ls,latr2)
!
!
      real(kind=kind_evod)   tfac(lonr,levs), sumq(lonr,levs)
      real(kind=kind_evod)   tki(lonr,levs+1)
      real(kind=kind_evod)   tkrt0, tx2(levs)
!     real(kind=kind_evod)   tem,ga2
      real(kind=kind_evod), parameter :: one=1.0, cb2pa=1000.0
!
      integer              i,j,k,kap,kar,kat,kau,kav,kk,nn,nnl,ksr
!     integer              kaz
      integer              l,lan,lat,lotx
      integer              lons_lat
!
!     integer              locl,n
!     integer              indev
!     integer              indod
!     integer              indev1,indev2
!     integer              indod1,indod2
!     integer              indlsev,jbasev
!     integer              indlsod,jbasod
!
      logical   lslag
!
!
!
      real(kind=kind_evod), parameter :: qmin=1.0e-10
!
      real (kind=kind_evod) tx1
!
!     include 'function_indlsev'
!     include 'function_indlsod'
!
!--------------------------------------------------------------------
!
      lslag   = .false.
      lotx    = lota
!     lotx    = lota+1

      kau     =0*levs+0*levh+1
      kav     =1*levs+0*levh+1
      kat     =2*levs+0*levh+1
      kar     =3*levs+0*levh+1
      kap     =3*levs+1*levh+1
!     kaz     =3*levs+1*levh+2
      ksr     =5*levs+0*levh+4  ! warning - this may not be right!!!!!!!!
!                                 --------------------------------------
!
!--------------------------------------------------------------------
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(trie_di,trie_ze,trie_te,kau,kav,kat)
!$omp+shared(trio_di,trio_ze,trio_te)
!$omp+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$omp+private(k)
      do k=1,levs
        call dezouv(trie_di(1,1,k),       trio_ze(1,1,k),
     x              trie_ls(1,1,kau+k-1), trio_ls(1,1,kav+k-1),
     x              epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!
        call dozeuv(trio_di(1,1,k),       trie_ze(1,1,k),
     x              trio_ls(1,1,kau+k-1), trie_ls(1,1,kav+k-1),
     x              epsedn,epsodn,snnp1ev,snnp1od,ls_node)
        trie_ls(:,:,kat+k-1) = trie_te(:,:,k)
        trio_ls(:,:,kat+k-1) = trio_te(:,:,k)
      enddo

!$omp parallel do private(k,kk,i)
      do k=1,levh
        kk = kar+k-1
        do i=1,len_trie_ls
          trie_ls(i,1,kk) = trie_rq(i,1,k)
          trie_ls(i,2,kk) = trie_rq(i,2,k)
        enddo
        do i=1,len_trio_ls
          trio_ls(i,1,kk) = trio_rq(i,1,k)
          trio_ls(i,2,kk) = trio_rq(i,2,k)
        enddo
      enddo

!     trie_ls(:,:,kaz) = trie_zs(:,:)
!     trio_ls(:,:,kaz) = trio_zs(:,:)

      do i=1,len_trie_ls
        trie_ls(i,1,kap) = trie_ps(i,1)
        trie_ls(i,2,kap) = trie_ps(i,2)
      enddo
      do i=1,len_trio_ls
        trio_ls(i,1,kap) = trio_ps(i,1)
        trio_ls(i,2,kap) = trio_ps(i,2)
      enddo
!
!     ga2 = (rerth*rerth) / grav
!     do locl=1,ls_max_node
!            l=ls_node(locl,1)
!       jbasev=ls_node(locl,2)
!       indev1 = indlsev(l,l)
!       if (mod(l,2).eq.mod(jcap+1,2)) then
!         indev2 = indlsev(jcap+1,l)
!       else
!         indev2 = indlsev(jcap  ,l)
!       endif
!       do indev = indev1 , indev2
!         tem = ga2 / snnp1ev(indev)
!     print *,' indev=',indev,' tem=',tem,' trie=',trie_ls(indev,2,kaz)
!         trie_ls(indev,1,kaz) = trie_ls(indev,1,kaz) * tem
!         trie_ls(indev,2,kaz) = trie_ls(indev,2,kaz) * tem
!       end do
!     end do
!     do locl=1,ls_max_node
!            l=ls_node(locl,1)
!       jbasod=ls_node(locl,3)
!       indod1 = indlsod(l+1,l)
!       if (mod(l,2).eq.mod(jcap+1,2)) then
!         indod2 = indlsod(jcap  ,l)
!       else
!         indod2 = indlsod(jcap+1,l)
!       endif
!       do indod = indod1 , indod2
!         tem = ga2 / snnp1od(indod)
!     print *,' indod=',indod,' tem=',tem,' trio=',trio_ls(indev,2,kaz)
!         trio_ls(indod,1,kaz) = trio_ls(indod,1,kaz) * tem
!         trio_ls(indod,2,kaz) = trio_ls(indod,2,kaz) * tem
!       end do
!     end do
!!
      call sumfln_slg_gg(trie_ls,
     x                   trio_ls,
     x            lat1s_r,
     x            plnev_r,plnod_r,
     x            5*levs+3,ls_node,latr2,
     x            lats_dim_r,lots,for_gr_r_1,
     x            ls_nodes,max_ls_nodes,
     x            lats_nodes_r,global_lats_r,
!mjr x            lats_node_r,ipt_lats_node_r,lon_dims_r,
     x            lats_node_r,ipt_lats_node_r,lon_dim_r,
     x            lonsperlar,lonrx,latr,0)
      if(.not.gg_tracers)then
        call sumfln_slg_gg(trie_ls(1,1,p_rm),
     x                     trio_ls(1,1,p_rm),
     x              lat1s_r,
     x              plnev_r,plnod_r,
     x              levh,ls_node,latr2,
     x              lats_dim_r,lots,for_gr_r_1,
     x              ls_nodes,max_ls_nodes,
     x              lats_nodes_r,global_lats_r,
!mjr x              lats_node_r,ipt_lats_node_r,lon_dims_r,
     x              lats_node_r,ipt_lats_node_r,lon_dim_r,
     x              lonsperlar,lonrx,latr,5*levs+3)
      endif ! if(.not.gg_tracers)then
!
!     dimg=0
!
!     call sumflna(trie_ls,trio_ls,
!    x            lat1s_r,
!    x            plnev_r,plnod_r,
!    x            lotx,ls_node,latr2,
!    x            lslag,lats_dim_r,lotx,for_gr_r_1,
!    x            ls_nodes,max_ls_nodes,
!    x            lats_nodes_r,global_lats_r,
!    x            lats_node_r,ipt_lats_node_r,lon_dims_r,dimg,
!    x            lonsperlar,lonrx,latr)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     if(gg_tracers .and. shuff_lats_r) then
!       print*,' gloopr mpi_tracers_a_to_b shuff_lats_r',shuff_lats_r
!       call mpi_tracers_a_to_b(
!    x       rg1_a,rg2_a,rg3_a,lats_nodes_a,global_lats_a,
!    x       for_gr_r_2(1,1,1),
!    x       lats_nodes_r,global_lats_r,ksr,0)
!     endif ! gg_tracers .and.  shuff_lats_r
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      do lan=1,lats_node_r
!
         lat = global_lats_r(ipt_lats_node_r-1+lan)
!
!        lon_dim = lon_dims_r(lan)
!
         lons_lat = lonsperlar(lat)
!
         call four_to_grid(for_gr_r_1(1,1,lan),for_gr_r_2(1,1,lan),
!mjr &                     lon_dim  ,lon_dim    ,lons_lat,5*levs+3)
     &                     lon_dim_r,lon_dim_r-2,lons_lat,5*levs+3)

!     if(gg_tracers)then
!
!   set tracers grid values from layout_grid_tracers
!
!        if (.not.shuff_lats_r) then
!          set for_gr_r_2 to rg1_a rg2_a rg3_a from gloopa
!          do k=1,levs
!            item = ksr - 1 + k
!            jtem = lats_node_a+1-lan
!            do i=1,min(lonf,lons_lat)
!              for_gr_r_2(i,item       ,lan) = rg1_a(i,k,jtem)
!              for_gr_r_2(i,item+  levs,lan) = rg2_a(i,k,jtem)
!              for_gr_r_2(i,item+2*levs,lan) = rg3_a(i,k,jtem)
!            enddo
!          enddo
!        endif ! not shuff_lats_r

!     else
         call four_to_grid(for_gr_r_1(1,ksr,lan),
     &                     for_gr_r_2(1,ksr,lan),
!mjr &                     lon_dim  ,lon_dim    ,lons_lat,levh)
     &                     lon_dim_r,lon_dim_r-2,lons_lat,levh)
!     endif
!
!        call four2grid_thread(for_gr_r_1(1,lan),for_gr_r_2(1,lan),
!    &                  lon_dim,lons_lat,lonrx,lotx,lan,me)
!
      enddo   !lan
!
      if (gen_coord_hybrid) then        ! for general sigma-thera-p hybrid
!       if (thermodyn_id == 3) then
!         do k=2,levs
!           tx2(k) = one / (cpi(0)*(ttref(k-1)+ttref(k)))
!         enddo
!       else
          do k=2,levs
            tx2(k) = one / (thref(k-1)+thref(k))
          enddo
!       endif
      endif
      do lan=1,lats_node_r
!       lon_dim  = lon_dims_r(lan)
        lat      = global_lats_r(ipt_lats_node_r-1+lan)
        lons_lat = lonsperlar(lat)
        tx1      = one / coslat_r(lat)
!
        do k=1,levh
          do i=1,lons_lat
            rqg(i,lan,k) = for_gr_r_2(i,(kar+k-1),lan)
          enddo
        enddo
!
        if (thermodyn_id == 3) then
!$omp parallel do private(k,i)
          do k=1,levs
            do i=1,lons_lat
              tfac(i,k) = 0.0
              sumq(i,k) = 0.0
            enddo
          enddo
          do nn=1,ntrac
            nnl = (nn-1)*levs
            if (cpi(nn) .ne. 0.0) then
!$omp parallel do private(k,i)
              do k=1,levs
                do i=1,lons_lat
                  sumq(i,k) = sumq(i,k) + rqg(i,lan,nnl+k)
                  tfac(i,k) = tfac(i,k) + cpi(nn)*rqg(i,lan,nnl+k)
                enddo
              enddo
            endif
          enddo
!$omp parallel do private(k,i)
          do k=1,levs
            do i=1,lons_lat
              tfac(i,k) = (one-sumq(i,k))*cpi(0) + tfac(i,k)
            enddo
          enddo
        else
!$omp parallel do private(k,i)
          do k=1,levs
            do i=1,lons_lat
              tfac(i,k) = one + fv*max(rqg(i,lan,k),qmin)
            enddo
          enddo
        endif
!$omp parallel do private(k,i)
        do k=1,levs
          do i=1,lons_lat
            uug(i,lan,k) = for_gr_r_2(i,(kau+k-1),lan) * tx1
            vvg(i,lan,k) = for_gr_r_2(i,(kav+k-1),lan) * tx1
            teg(i,lan,k) = for_gr_r_2(i,(kat+k-1),lan)
!    &                                                    / tfac(i,k)
          enddo
        enddo
        if (gen_coord_hybrid) then  
!$omp parallel do private(i)
          do i=1,lons_lat
!           zsg(i,lan) = for_gr_r_2(i+(kaz-1)*lon_dim,lan)
            psg(i,lan) = for_gr_r_2(i,(kap-1),lan) * cb2pa
          enddo
        else
!$omp parallel do private(i)
          do i=1,lons_lat
!           zsg(i,lan) = for_gr_r_2(i+(kaz-1)*lon_dim,lan)
            psg(i,lan) = exp(for_gr_r_2(i,(kap-1),lan)) * cb2pa
          enddo
        endif
        if (gen_coord_hybrid) then        ! for general sigma-thera-p hybrid
          tki(:,1) = 0.0
!$omp parallel do private(k,i,tkrt0)
          do k=2,levs
            do i=1,lons_lat
              tkrt0     = (teg(i,lan,k-1)+teg(i,lan,k))*tx2(k)
              tki (i,k) = cb2pa*ck5(k)*tkrt0**rk
            enddo
          enddo
!$omp parallel do private(k,i)
          do k=1,levs
            do i=1,lons_lat
              dpg(i,lan,k) = cb2pa*(ak5(k)-ak5(k+1))+(bk5(k)-bk5(k+1))
     &                     * psg(i,lan) + tki(i,k) - tki(i,k+1)
              teg(i,lan,k) = teg(i,lan,k) / tfac(i,k)
            enddo
          enddo
        elseif (hybrid) then              ! for sigma-p hybrid (ecwmf)
!$omp parallel do private(k,kk,i)
          do k=1,levs
            kk = levs - k + 1
            do i=1,lons_lat
              dpg(i,lan,k) = cb2pa*(ak5(kk+1)-ak5(kk))
     &                     + (bk5(kk+1)-bk5(kk)) * psg(i,lan)
              teg(i,lan,k) = teg(i,lan,k) / tfac(i,k)
            enddo
          enddo
        endif
      enddo
cc
!     print *,' exit spect_to_grid '
!!
      return
      end
