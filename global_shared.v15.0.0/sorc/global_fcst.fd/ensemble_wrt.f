      subroutine ensemble_wrt(kdt,phour,
     &                 trie_ls,trio_ls,
     &                 ls_node,ls_nodes,max_ls_nodes,
     &                 lats_nodes_r,global_lats_r,lonsperlar,
     &                 lats_nodes_a,global_lats_a,lonsperlat,
     &                 epsedn,epsodn,snnp1ev,snnp1od,
     &                 plnev_r,plnod_r,pddev_r,pddod_r,
     &                 plnew_r,plnow_r,
     &                 xlon,xlat,sfc_fld, flx_fld, nst_fld,
     &                 fluxr,pdryini, phy_f3d,  phy_f2d,
     &                 dyn_f3d,  dyn_f2d,
     &                 zhour,lsout,colat1,cfhour1)
!
      use resol_def
      use layout1
      use gg_def
      use vert_def
      use date_def
      use namelist_def
      use mpi_def
      use ozne_def
      use sfc_flx_esmfmod
      use nst_var_esmfmod
      use d3d_def
!
      implicit none
!!     
      type(sfc_var_data)                :: sfc_fld
      type(flx_var_data)                :: flx_fld
      type(nst_var_data)                :: nst_fld
      character(16)                     :: cfhour1
!!     
      real(kind=kind_evod),intent(in)   :: phour
      real(kind=kind_evod),intent(inout):: zhour
!!     
      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)
!
      integer              ls_node(ls_dim,3)
!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!
      integer          ls_nodes(ls_dim,nodes)
      integer          max_ls_nodes(nodes),      lats_nodes_r(nodes)
      integer          global_lats_r(latr),  lonsperlar(latr)
      integer          lats_nodes_a(nodes)
      integer          global_lats_a(latg),  lonsperlat(latg)
!
      real(kind=kind_evod) colat1
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
      real(kind=kind_evod)   plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnod_r(len_trio_ls,latr2)
      real(kind=kind_evod)   pddev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   pddod_r(len_trio_ls,latr2)
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
!!     
      real (kind=kind_rad) xlon(lonr,lats_node_r)
      real (kind=kind_rad) xlat(lonr,lats_node_r)

      real (kind=kind_phys) phy_f3d(lonr,levs,num_p3d,lats_node_r),
     &                      phy_f2d(lonr,num_p2d,lats_node_r)
      real (kind=kind_evod) dyn_f3d(lonf,levs,num_a3d,lats_node_a),
     &                      dyn_f2d(lonf,num_a2d,lats_node_a)
      real (kind=kind_rad) fluxr(nfxr,lonr,lats_node_r)
!!     
      real (kind=kind_phys) pdryini
      integer   kdt
      logical lsout

!
!dhou 05-29-2008, this subroutine is adapted from do_tstep 
!      only the output calls are retained with modified control (if)
!      this routine is called in at begining of gfs_run only if sps=.true.
!----------------------------------------------------------
      if (lsout) then
!
      call wrtout(phour,fhour,zhour,idate,
     x            trie_ls,trio_ls,
     x            sl,si,
     x            ls_node,ls_nodes,max_ls_nodes,
     &            sfc_fld, flx_fld, nst_fld,
     &            fluxr,pdryini,
     &            lats_nodes_r,global_lats_r,lonsperlar,
     &            colat1,cfhour1,pl_coeff,
     &            epsedn,epsodn,snnp1ev,snnp1od,plnev_r,plnod_r,
     &            plnew_r,plnow_r,
     &           'SIG.S','SFB.S','FLX.S') 
!
      endif ! if ls_out
!
!!
      if (mod(kdt,nsres).eq.0) then
        call wrt_restart(trie_ls,trio_ls,
     &     sfc_fld, nst_fld,
     &     si,sl,fhour,idate,
     &     igen,pdryini,
     x     ls_node,ls_nodes,max_ls_nodes,
     &     global_lats_r,lonsperlar,snnp1ev,snnp1od,
     &     global_lats_a,lonsperlat,
     &     phy_f3d, phy_f2d, dyn_f3d, dyn_f2d,
     &     ngptc, adiab, ens_nam,
     &     nst_fcst,
     &     'SIGS1','SIGS2','SFCS','NSTS') 

      endif
!
      return
      end
