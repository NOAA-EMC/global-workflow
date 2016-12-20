      subroutine spect_fields(n1,n2, pdryini,trie_ls,trio_ls,
     &                 ls_node,ls_nodes,max_ls_nodes,snnp1ev,snnp1od,
     &                 phy_f3d,phy_f2d,dyn_f3d,dyn_f2d,
     &                 global_lats_r,lonsperlar,
     &                 global_lats_a,lonsperlat,
     &                 epse,epso,plnev_r,plnod_r,
     &                 plnew_r,plnow_r,lats_nodes_r,lats_nodes_a,
     &                 cread, cread2)
!
      use resol_def
      use layout1
      use gg_def
      use vert_def
!     use sig_io
      use date_def
      use namelist_def
      use gfsio_module
      use gfsio_def
      use mpi_def
      implicit none
!!
 
cmy fix pdryini type
cmy      real(kind=kind_evod) pdryini
      real(kind=kind_phys) pdryini
      integer              n1,n2
      character (len=*)   :: cread, cread2
      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      real(kind=kind_evod) epse   (len_trie_ls)
      real(kind=kind_evod) epso   (len_trie_ls)
      real(kind=kind_evod) plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnod_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod) plnow_r(len_trie_ls,latr2)
!
!
      real (kind=kind_phys)
     &     phy_f3d(lonr,levs,num_p3d,lats_node_r),
     &     phy_f2d(lonr,num_p2d,lats_node_r)
      real (kind=kind_evod)
     &     dyn_f3d(lonf,levs,num_a3d,lats_node_a),
     &     dyn_f2d(lonf,num_a2d,lats_node_a)
      integer global_lats_r(latr), lonsperlar(latr)
      integer global_lats_a(latg), lonsperlat(latg)
 
cmy bug fix on dimension of ls_node
      integer              ls_node (ls_dim*3)
      integer              ls_nodes(ls_dim,nodes)
      integer          max_ls_nodes(nodes)
      integer            lats_nodes_r(nodes)
      integer            lats_nodes_a(nodes)
!
c$$$      integer                lots,lotd,lota
c$$$      parameter            ( lots = 5*levs+1*levh+3 )
c$$$      parameter            ( lotd = 6*levs+2*levh+0 )
c$$$      parameter            ( lota = 3*levs+1*levh+1 )
c$$$      integer   p_gz,p_zem,p_dim,p_tem,p_rm,p_qm
c$$$      integer   p_ze,p_di,p_te,p_rq,p_q,p_dlam,p_dphi,p_uln,p_vln
c$$$      integer   p_w,p_x,p_y,p_rt,p_zq
c$$$      parameter(p_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
c$$$     x          p_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
c$$$     x          p_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
c$$$     x          p_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
c$$$     x          p_rm  = 3*levs+0*levh+2,  !      rme/o(lnte/od,2,levh),
c$$$     x          p_qm  = 3*levs+1*levh+2,  !      qme/o(lnte/od,2),
c$$$     x          p_ze  = 3*levs+1*levh+3,  !      zee/o(lnte/od,2,levs),
c$$$     x          p_di  = 4*levs+1*levh+3,  !      die/o(lnte/od,2,levs),
c$$$     x          p_te  = 5*levs+1*levh+3,  !      tee/o(lnte/od,2,levs),
c$$$     x          p_rq  = 6*levs+1*levh+3,  !      rqe/o(lnte/od,2,levh),
c$$$     x          p_q   = 6*levs+2*levh+3,  !       qe/o(lnte/od,2),
c$$$     x          p_dlam= 6*levs+2*levh+4,  !  dpdlame/o(lnte/od,2),
c$$$     x          p_dphi= 6*levs+2*levh+5,  !  dpdphie/o(lnte/od,2),
c$$$     x          p_uln = 6*levs+2*levh+6,  !     ulne/o(lnte/od,2,levs),
c$$$     x          p_vln = 7*levs+2*levh+6,  !     vlne/o(lnte/od,2,levs),
c$$$     x          p_w   = 8*levs+2*levh+6,  !       we/o(lnte/od,2,levs),
c$$$     x          p_x   = 9*levs+2*levh+6,  !       xe/o(lnte/od,2,levs),
c$$$     x          p_y   =10*levs+2*levh+6,  !       ye/o(lnte/od,2,levs),
c$$$     x          p_rt  =11*levs+2*levh+6,  !      rte/o(lnte/od,2,levh),
c$$$     x          p_zq  =11*levs+3*levh+6)  !      zqe/o(lnte/od,2)

      integer              iprint,j,k,l,n,i
      integer              indlsev,jbasev
      integer              indlsod,jbasod
!
      if (me == 0) print  9876,n1,n2,fhour,idate
 9876 format(1h ,'n1,n2,fhour in spect_fields ',2(i4,1x),f6.2,
     & ' idate no yet read in',4(1x,i4))
      iprint = 0
c$$$  if ( me .eq. 0 ) iprint = 1
!
      if (me == 0) print *,' cread=',cread
      if (.not. gfsio_in) then
        call treadeo(n1,fhour,idate,
     &               trie_ls(1,1,p_gz ), trie_ls(1,1,p_qm ),
     &               trie_ls(1,1,p_tem), trie_ls(1,1,p_dim),
     &               trie_ls(1,1,p_zem), trie_ls(1,1,p_rm ),
     &               trio_ls(1,1,p_gz ), trio_ls(1,1,p_qm ),
     &               trio_ls(1,1,p_tem), trio_ls(1,1,p_dim),
     &               trio_ls(1,1,p_zem), trio_ls(1,1,p_rm ),
     &               ls_node,ls_nodes,max_ls_nodes,
     &               plnev_r, plnod_r, plnew_r, plnow_r,
     &               lats_nodes_r, lats_nodes_a,
     &               snnp1ev,snnp1od,pdryini,iprint,
     &               phy_f3d, phy_f2d, global_lats_r, lonsperlar,
     &               dyn_f3d, dyn_f2d, global_lats_a, lonsperlat,
     &               cread)
      else
        call treadeo_gfsio(fhour,idate,
     &               trie_ls(1,1,p_gz ), trie_ls(1,1,p_qm ),
     &               trie_ls(1,1,p_tem), trie_ls(1,1,p_dim),
     &               trie_ls(1,1,p_zem), trie_ls(1,1,p_rm ),
     &               trio_ls(1,1,p_gz ), trio_ls(1,1,p_qm ),
     &               trio_ls(1,1,p_tem), trio_ls(1,1,p_dim),
     &               trio_ls(1,1,p_zem), trio_ls(1,1,p_rm ),
     &               ls_node,ls_nodes,max_ls_nodes,
     &               snnp1ev,snnp1od,pdryini,iprint,
     &               global_lats_r,lats_nodes_r,lonsperlar, cread,
     &               epse, epso, plnew_r, plnow_r)
      endif
 
      fhini = fhour
      if (me == 0) print 9877, n1,fhour
 9877 format(1h ,'n1,fhour after tread',1(i4,1x),f6.2)
 
      if (me == 0) print *,' fhini=',fhini
      if (.not.liope.or.icolor.ne.2) then
!sela   print*,'liope=',liope,' icolor=',icolor
        call rms_spect(trie_ls(1,1,p_qm ), trie_ls(1,1,p_dim),
     &             trie_ls(1,1,p_tem), trie_ls(1,1,p_zem),
     &             trie_ls(1,1,p_rm ),
     &             trio_ls(1,1,p_qm ), trio_ls(1,1,p_dim),
     &             trio_ls(1,1,p_tem), trio_ls(1,1,p_zem),
     &             trio_ls(1,1,p_rm ),
     &             ls_nodes,max_ls_nodes)
      endif
!---------------------------------------------------------------
      if(fhini == fhrot) then
!set n time level values to n-1 time
        do i=1,len_trie_ls
           trie_ls(i,1,p_q ) = trie_ls(i,1,p_qm )
           trie_ls(i,2,p_q ) = trie_ls(i,2,p_qm )
        enddo
        do i=1,len_trio_ls
           trio_ls(i,1,p_q ) = trio_ls(i,1,p_qm )
           trio_ls(i,2,p_q ) = trio_ls(i,2,p_qm )
        enddo
 
!$omp parallel do private(k,i)
        do k=1,levs
          do i=1,len_trie_ls
            trie_ls(i,1,p_te +k-1) = trie_ls(i,1,p_tem +k-1)
            trie_ls(i,2,p_te +k-1) = trie_ls(i,2,p_tem +k-1)
 
            trie_ls(i,1,p_di +k-1) = trie_ls(i,1,p_dim +k-1)
            trie_ls(i,2,p_di +k-1) = trie_ls(i,2,p_dim +k-1)
 
            trie_ls(i,1,p_ze +k-1) = trie_ls(i,1,p_zem +k-1)
            trie_ls(i,2,p_ze +k-1) = trie_ls(i,2,p_zem +k-1)
          enddo
          do i=1,len_trio_ls
            trio_ls(i,1,p_te +k-1) = trio_ls(i,1,p_tem+k-1)
            trio_ls(i,2,p_te +k-1) = trio_ls(i,2,p_tem+k-1)
 
            trio_ls(i,1,p_di +k-1) = trio_ls(i,1,p_dim+k-1)
            trio_ls(i,2,p_di +k-1) = trio_ls(i,2,p_dim+k-1)
 
            trio_ls(i,1,p_ze +k-1) = trio_ls(i,1,p_zem+k-1)
            trio_ls(i,2,p_ze +k-1) = trio_ls(i,2,p_zem+k-1)
          enddo
        enddo
 
!$omp parallel do private(k,i)
        do k=1,levh
          do i=1,len_trie_ls
            trie_ls(i,1,p_rq +k-1) = trie_ls(i,1,p_rm +k-1)
            trie_ls(i,2,p_rq +k-1) = trie_ls(i,2,p_rm +k-1)
          enddo
          do i=1,len_trio_ls
            trio_ls(i,1,p_rq +k-1) = trio_ls(i,1,p_rm+k-1)
            trio_ls(i,2,p_rq +k-1) = trio_ls(i,2,p_rm+k-1)
          enddo
        enddo
!--------------------------------------------------------
      else
!--------------------------------------------------------
        iprint = 0
c$$$      if ( me .eq. 0 ) iprint = 1
      if (me == 0) print *,' cread2=',cread2
        if (.not. gfsio_in) then
          call treadeo(n2,fhour,idate,
     &                 trie_ls(1,1,p_gz), trie_ls(1,1,p_q ),
     &                 trie_ls(1,1,p_te), trie_ls(1,1,p_di),
     &                 trie_ls(1,1,p_ze), trie_ls(1,1,p_rq),
     &                 trio_ls(1,1,p_gz), trio_ls(1,1,p_q ),
     &                 trio_ls(1,1,p_te), trio_ls(1,1,p_di),
     &                 trio_ls(1,1,p_ze), trio_ls(1,1,p_rq),
     &                 ls_node,ls_nodes,max_ls_nodes,
     &                 plnev_r, plnod_r, plnew_r, plnow_r,
     &                 lats_nodes_r, lats_nodes_a,
     &                 snnp1ev,snnp1od,pdryini,iprint,
     &                 phy_f3d, phy_f2d, global_lats_r, lonsperlar,
     &                 dyn_f3d, dyn_f2d, global_lats_a, lonsperlat,
     &                 cread2)
        else
          call treadeo_gfsio(fhour,idate,
     &                 trie_ls(1,1,p_gz), trie_ls(1,1,p_q ),
     &                 trie_ls(1,1,p_te), trie_ls(1,1,p_di),
     &                 trie_ls(1,1,p_ze), trie_ls(1,1,p_rq),
     &                 trio_ls(1,1,p_gz), trio_ls(1,1,p_q ),
     &                 trio_ls(1,1,p_te), trio_ls(1,1,p_di),
     &                 trio_ls(1,1,p_ze), trio_ls(1,1,p_rq),
     &                 ls_node,ls_nodes,max_ls_nodes,
     &                 snnp1ev,snnp1od,pdryini,iprint,
     &                 global_lats_r,lats_nodes_r,lonsperlar, cread2,
     &                 epse, epso, plnew_r, plnow_r)
        endif
        if (me == 0) print 9878, n2,fhour
 9878   format(1h ,'n2,fhour after tread',1(i4,1x),f6.2)
      endif
!--------------------------------------------------------
!!
!sela if (me.eq.0) then
!sela   write(*,*)'initial values'
!sela   write(*,*)'*********'
!sela   call bar3(trie_ls(1,1,p_ze),trio_ls(1,1,p_ze),'ze ',levs)
!sela   call bar3(trie_ls(1,1,p_di),trio_ls(1,1,p_di),'di ',levs)
!sela   call bar3(trie_ls(1,1,p_te),trio_ls(1,1,p_te),'te ',levs)
!sela   call bar3(trie_ls(1,1,p_rq),trio_ls(1,1,p_rq),'rq ',levs)
!sela   call bar3(trie_ls(1,1,p_rq+levs),trio_ls(1,1,p_rq+levs),
!sela&            'oz1 ',levs)
!sela   call bar3(trie_ls(1,1,p_rq+2*levs),trio_ls(1,1,p_rq+2*levs),
!sela&            'oz2 ',levs)
!sela   call bar3(trie_ls(1,1,p_q),trio_ls(1,1,p_q),'q ',1)
!sela   call bar3(trie_ls(1,1,p_gz),trio_ls(1,1,p_gz),'gz ',1)
!sela endif
!!
      return
      end
