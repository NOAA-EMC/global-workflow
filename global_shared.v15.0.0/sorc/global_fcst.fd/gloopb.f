      subroutine gloopb(trie_ls,trio_ls,
     &                  ls_node,ls_nodes,max_ls_nodes,
     &                  lats_nodes_a,global_lats_a,
     &                  lats_nodes_r,global_lats_r,
     &                  lonsperlar,lonsperlat,
     &                  epse,epso,epsedn,epsodn,
     &                  snnp1ev,snnp1od,ndexev,ndexod,
     &                  plnev_r,plnod_r,pddev_r,pddod_r,plnew_r,plnow_r,
     &                  tstep,phour,sfc_fld, flx_fld, nst_fld, sfalb,
     &                  xlon,
     &                  swh,swhc,hlw,hlwc,hprime,slag,sdec,cdec,
     &                  ozplin,jindx1,jindx2,ddy,pdryini,
     &                  phy_f3d, phy_f2d,xlat,kdt,
     &                  global_times_b,batah,lsout,fscav)
!!
!!
      use machine             , only : kind_evod,kind_phys,kind_rad
!    &,                                kind_io4
      use resol_def           , only : jcap,jcap1,latg,latr,latr2,
     &                                 levh,levp1,levs,lnt2,
     &                                 lonf,lonr,lonrx,lota,lotd,lots,
     &                                 lsoil,ncld,nmtvr,nrcm,ntcw,ntoz,
     &                                 ntrac,num_p2d,num_p3d,
     &                                 p_di,p_dlam,p_dphi,p_q,
     &                                 p_rq,p_rt,p_te,p_uln,p_vln,
     &                                 p_w,p_x,p_y,p_ze,p_zq,
     &                                 thermodyn_id,sfcpress_id,nfxr

      use layout1             , only : ipt_lats_node_r,
     &                                 lat1s_r,lats_dim_r,
     &                                 lats_node_a,lats_node_r,
     &                                 len_trie_ls,len_trio_ls,
     &                                 lon_dim_r,ls_dim,ls_max_node,
     &                                 me,me_l_0,nodes
      use layout_grid_tracers , only : rgt_a,xhalo,
     &                                 rgt_h,yhalo
      use gg_def              , only : coslat_r,rcs2_r,sinlat_r,wgt_r
      use vert_def            , only : am,bm,del,si,sik,sl,slk,sv
      use date_def            , only : fhour,idate
      use namelist_def        , only : crtrh,fhswr,flgmin,
     &                                 gen_coord_hybrid,gg_tracers,
     &                                 hybrid,ldiag3d,lscca,lsfwd,
     &                                 lssav,lsswr,ncw,ngptc,
     &                                 pre_rad,random_clds,
     &                                 ras,semilag,shuff_lats_r,
     &                                 mom4ice,
     &                                 redrag,hybedmf,dspheat,
     &                                 ccwf,cnvgwd,trans_trac,
     &                                 cal_pre,nst_fcst,dtphys,
     &                                 dlqf,cdmbgwd,
     &                                 bkgd_vdif_m, bkgd_vdif_h,
     &                                 bkgd_vdif_s,shal_cnv,
     &                                 psautco, prautco, evpco, wminco,
     &                                 sup,pdfcld,shcnvcw,cgwf,fixtrc,
     &                                 n3dzhaocld,n3dfercld,n3dcldpdf,

! for stochastic physics perturbations 
!-------------------------------------
     &                                 sppt,shum,skeb,vcamp,vc,stochphys
! for iau
!--------
     &,                                iau

      use iau_module, only: init_iau,getiauforcing,iau_initialized,
     &                      iauforcing
      use coordinate_def      , only : ak5,bk5,vertcoord_id               ! hmhj
      use bfilt_def           , only : bfilte,bfilto
      use module_ras          , only : ras_init
      use physcons            , only :  grav => con_g,
     &                                 rerth => con_rerth,   ! hmhj
     &                                    fv => con_fvirt,   ! mjr
     &                                 rvrdm1 => con_fvirt,
     &                                    rd => con_rd
      use ozne_def            , only : latsozp,levozp,
     &                                 pl_coeff,pl_pres,timeoz
!-> coupling insertion
      use surface_cc
!<- coupling insertion

      use sfc_flx_esmfmod
      use nst_var_esmfmod
      use mersenne_twister
      use d3d_def
      use tracer_const
!     use mpi_def, only: mc_comp
!
      implicit none
      include 'mpif.h'
!
      type(sfc_var_data)        :: sfc_fld
      type(flx_var_data)        :: flx_fld
      type(nst_var_data)        :: nst_fld
!
      real(kind=kind_phys), parameter :: rlapse=0.65e-2
      real(kind=kind_evod), parameter :: cons_0=0.0,   cons_24=24.0
     &,                                  cons_99=99.0, cons_1p0d9=1.0e9


!$$$      integer n1rac, n2rac,nlons_v(ngptc)
!$$$      parameter (n1rac=ntrac-ntshft-1, n2rac=n1rac+1)
!
!     integer id,njeff,istrt,lon,kdt
      integer id,njeff,      lon,kdt
!!
      real(kind=kind_evod) save_qe_ls(len_trie_ls,2)
      real(kind=kind_evod) save_qo_ls(len_trio_ls,2)

      real(kind=kind_evod) sum_k_rqchange_ls(len_trie_ls,2)
      real(kind=kind_evod) sum_k_rqchango_ls(len_trio_ls,2)
!!
      real(kind=kind_evod) trie_ls(len_trie_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trio_ls(len_trio_ls,2,11*levs+3*levh+6)
      real(kind=kind_evod) trie_ls_rqt(len_trie_ls,2,levs)
      real(kind=kind_evod) trio_ls_rqt(len_trio_ls,2,levs)
      real(kind=kind_evod) trie_ls_sfc(len_trie_ls,2)                   ! hmhj
      real(kind=kind_evod) trio_ls_sfc(len_trio_ls,2)                   ! hmhj

!! iau stuff
!-----------
      real(kind_evod) dtiau

!     real(kind_io8), allocatable, dimension(:,:)    :: glolal
!     integer, allocatable, dimension(:,:)           :: kmsk0
!     real(kind_phys), allocatable, dimension(:,:,:) :: tmpg
!     real(kind_io4), allocatable, dimension(:)      :: wrkga
!     real(kind_io4), allocatable, dimension(:,:,:)  :: workg,workg_out
!! iau stuff over
!!
      real(kind=kind_phys)    typdel(levs), batah
      real(kind=kind_phys)    prsl(ngptc,levs)
      real(kind=kind_phys)   prslk(ngptc,levs),dpshc(ngptc)
      real(kind=kind_phys)    prsi(ngptc,levs+1),phii(ngptc,levs+1)
      real(kind=kind_phys)   prsik(ngptc,levs+1),phil(ngptc,levs)
!!
      real (kind=kind_phys) gu(ngptc,levs),  gv1(ngptc,levs)
      real (kind=kind_phys) ugrd(ngptc,levs),vgrd(ngptc,levs)
      real (kind=kind_phys) gphi(ngptc),     glam(ngptc)
      real (kind=kind_phys) gq(ngptc),       gt(ngptc,levs), pgr(ngptc)
      real (kind=kind_phys) gr(ngptc,levs,ntrac)
      real (kind=kind_phys) gd(ngptc,levs)
      real (kind=kind_phys) adt(ngptc,levs), adr(ngptc,levs,ntrac)
      real (kind=kind_phys) adu(ngptc,levs), adv(ngptc,levs)
      real (kind=kind_phys) gtv(ngptc,levs)                              ! hmhj
      real (kind=kind_phys) gtvx(ngptc,levs), gtvy(ngptc,levs)           ! hmhj
      real (kind=kind_phys) sumq(ngptc,levs), xcp(ngptc,levs)
!! sppt additional variables
      real (kind=kind_phys) ugrd0(ngptc,levs),vgrd0(ngptc,levs)
      real (kind=kind_phys) gr0(ngptc,levs,ntrac),gt0(ngptc,levs)
!   variables for stochastic physics
!-----------------------------------
      real (kind=kind_phys),dimension(ngptc,levs)        ::
     &                      dtdt
      real (kind=kind_phys) :: uphys,vphys,tphys,qphys,sppt_wts
      real (kind=kind_phys),allocatable,dimension(:,:,:) :: 
     &                 sppt_wt,shum_wt,vcu_wt,vcv_wt,skebu_wt,skebv_wt
!
      real (kind=kind_phys) dt3dt_v(ngptc,levs,6),
     &                      dq3dt_v(ngptc,levs,5+pl_coeff),
     &                      du3dt_v(ngptc,levs,4),
     &                      dv3dt_v(ngptc,levs,4)
     &,                     upd_mf_v(ngptc,levs), dwn_mf_v(ngptc,levs)
     &,                     det_mf_v(ngptc,levs)
!!
      real(kind=kind_evod) gq_save(lonr,lats_dim_r)
!!
      real (kind=kind_rad) slag,sdec,cdec,phour
      real (kind=kind_rad) xlon(lonr,lats_node_r)
      real (kind=kind_rad) xlat(lonr,lats_node_r)
      real (kind=kind_rad) coszdg(lonr,lats_node_r),
     &                     hprime(lonr,nmtvr,lats_node_r),
!    &                     fluxr(lonr,nfxr,lats_node_r),
     &                     sfalb(lonr,lats_node_r)
      real (kind=kind_rad) swh(lonr,levs,lats_node_r)
      real (kind=kind_rad) hlw(lonr,levs,lats_node_r)
      real (kind=kind_rad) swhc(lonr,levs,lats_node_r)
      real (kind=kind_rad) hlwc(lonr,levs,lats_node_r)
!!
      real (kind=kind_phys)
     &     phy_f3d(lonr,levs,num_p3d,lats_node_r),
     &     phy_f2d(lonr,num_p2d,lats_node_r), fscav(ntrac-ncld-1)
!
!
!     real (kind=kind_phys) exp,dtphys,dtp,dtf,sumed(2)
      real (kind=kind_phys) exp,       dtp,dtf,sumed(2)
      real (kind=kind_evod) tstep
      real (kind=kind_phys) pdryini,sigshc,rk
!!
      integer              ls_node(ls_dim,3)
!!
!     ls_node(1,1) ... ls_node(ls_max_node,1) : values of l
!     ls_node(1,2) ... ls_node(ls_max_node,2) : values of jbasev
!     ls_node(1,3) ... ls_node(ls_max_node,3) : values of jbasod
!!
      integer              ls_nodes(ls_dim,nodes)
!!
      integer              max_ls_nodes(nodes)
      integer              lats_nodes_a(nodes)
      integer              lats_nodes_r(nodes)
!!
      integer              global_lats_a(latg)
      integer              global_lats_r(latr)
      integer                 lonsperlar(latr)
      integer                 lonsperlat(latg)
      integer dimg
!!
      real(kind=kind_evod)    epse(len_trie_ls)
      real(kind=kind_evod)    epso(len_trio_ls)
      real(kind=kind_evod)  epsedn(len_trie_ls)
      real(kind=kind_evod)  epsodn(len_trio_ls)
!!
      real(kind=kind_evod) snnp1ev(len_trie_ls)
      real(kind=kind_evod) snnp1od(len_trio_ls)
!!
      integer               ndexev(len_trie_ls)
      integer               ndexod(len_trio_ls)
!!
      real(kind=kind_evod)   plnev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnod_r(len_trio_ls,latr2)
      real(kind=kind_evod)   pddev_r(len_trie_ls,latr2)
      real(kind=kind_evod)   pddod_r(len_trio_ls,latr2)
      real(kind=kind_evod)   plnew_r(len_trie_ls,latr2)
      real(kind=kind_evod)   plnow_r(len_trio_ls,latr2)
!!
!$$$      integer                lots,lotd,lota
      integer lotn
!$$$cc
!$$$      parameter            ( lots = 5*levs+1*levh+3 )
!$$$      parameter            ( lotd = 6*levs+2*levh+0 )
!$$$      parameter            ( lota = 3*levs+1*levh+1 )
!!
      real(kind=kind_evod) for_gr_r_1(lonrx,lots,lats_dim_r)
      real(kind=kind_evod) dyn_gr_r_1(lonrx,lotd,lats_dim_r)            ! hmhj
      real(kind=kind_evod) bak_gr_r_1(lonrx,lota,lats_dim_r)
!!
!     real(kind=kind_evod) for_gr_r_2(lonrx*lots,lats_dim_r)
!     real(kind=kind_evod) dyn_gr_r_2(lonrx*lotd,lats_dim_r)            ! hmhj
!     real(kind=kind_evod) bak_gr_r_2(lonrx*lota,lats_dim_r)
!
      real(kind=kind_evod) for_gr_r_2(lonr,lots,lats_dim_r)
      real(kind=kind_evod) dyn_gr_r_2(lonr,lotd,lats_dim_r)             ! hmhj
      real(kind=kind_evod) bak_gr_r_2(lonr,lota,lats_dim_r)
!!
      integer              i,ierr,iter,j,k,kap,kar,kat,kau,kav,ksq,jj,kk
      integer              kst,kdtphi,kdtlam                            ! hmhj
      integer              l,lan,lan0,lat,lmax,locl,ii,lonrbm
!     integer              lon_dim,lons_lat,n,node
      integer                      lons_lat,n,node
      integer nsphys
!
      real(kind=kind_evod) pwatg(latr),pwatj(lats_node_r),
     &                     pwatp,ptotg(latr),sumwa,sumto,
     &                     ptotj(lats_node_r),pcorr,pdryg,
     &                     solhr,clstp

      real(kind=kind_evod) trcg(latr,ntrac,2),trcj(lats_node_r,ntrac,2)
     &,                    trcp(lonr,ntrac,2)

      real(kind=kind_phys), allocatable, save :: sumtrc(:,:), adjtrc(:)
!!
      integer              ipt_ls                                       ! hmhj
      real(kind=kind_evod) reall                                        ! hmhj
      real(kind=kind_evod) rlcs2(jcap1)                                 ! hmhj
 
       real(kind=kind_evod) typical_pgr
!
!timers______________________________________________________---
 
!     real*8 rtc ,timer1,timer2
      real(kind=kind_evod) global_times_b(latr,nodes)
 
!timers______________________________________________________---
!
!
!$$$      parameter(ksq     =0*levs+0*levh+1,
!$$$     x          ksplam  =0*levs+0*levh+2,
!$$$     x          kspphi  =0*levs+0*levh+3,
!$$$     x          ksu     =0*levs+0*levh+4,
!$$$     x          ksv     =1*levs+0*levh+4,
!$$$     x          ksz     =2*levs+0*levh+4,
!$$$     x          ksd     =3*levs+0*levh+4,
!$$$     x          kst     =4*levs+0*levh+4,
!$$$     x          ksr     =5*levs+0*levh+4)
!
!$$$      parameter(kdtphi  =0*levs+0*levh+1,
!$$$     x          kdrphi  =1*levs+0*levh+1,
!$$$     x          kdtlam  =1*levs+1*levh+1,
!$$$     x          kdrlam  =2*levs+1*levh+1,
!$$$     x          kdulam  =2*levs+2*levh+1,
!$$$     x          kdvlam  =3*levs+2*levh+1,
!$$$     x          kduphi  =4*levs+2*levh+1,
!$$$     x          kdvphi  =5*levs+2*levh+1)
!
!$$$      parameter(kau     =0*levs+0*levh+1,
!$$$     x          kav     =1*levs+0*levh+1,
!$$$     x          kat     =2*levs+0*levh+1,
!$$$     x          kar     =3*levs+0*levh+1,
!$$$     x          kap     =3*levs+1*levh+1)
!
!
!$$$      integer   p_gz,p_zem,p_dim,p_tem,p_rm,p_qm
!$$$      integer   p_ze,p_di,p_te,p_rq,p_q,p_dlam,p_dphi,p_uln,p_vln
!$$$      integer   p_w,p_x,p_y,p_rt,p_zq
!$$$cc
!$$$cc                                               old common /comfspec/
!$$$      parameter(p_gz  = 0*levs+0*levh+1,  !      gze/o(lnte/od,2),
!$$$     x          p_zem = 0*levs+0*levh+2,  !     zeme/o(lnte/od,2,levs),
!$$$     x          p_dim = 1*levs+0*levh+2,  !     dime/o(lnte/od,2,levs),
!$$$     x          p_tem = 2*levs+0*levh+2,  !     teme/o(lnte/od,2,levs),
!$$$     x          p_rm  = 3*levs+0*levh+2,  !      rme/o(lnte/od,2,levh),
!$$$     x          p_qm  = 3*levs+1*levh+2,  !      qme/o(lnte/od,2),
!$$$     x          p_ze  = 3*levs+1*levh+3,  !      zee/o(lnte/od,2,levs),
!$$$     x          p_di  = 4*levs+1*levh+3,  !      die/o(lnte/od,2,levs),
!$$$     x          p_te  = 5*levs+1*levh+3,  !      tee/o(lnte/od,2,levs),
!$$$     x          p_rq  = 6*levs+1*levh+3,  !      rqe/o(lnte/od,2,levh),
!$$$     x          p_q   = 6*levs+2*levh+3,  !       qe/o(lnte/od,2),
!$$$     x          p_dlam= 6*levs+2*levh+4,  !  dpdlame/o(lnte/od,2),
!$$$     x          p_dphi= 6*levs+2*levh+5,  !  dpdphie/o(lnte/od,2),
!$$$     x          p_uln = 6*levs+2*levh+6,  !     ulne/o(lnte/od,2,levs),
!$$$     x          p_vln = 7*levs+2*levh+6,  !     vlne/o(lnte/od,2,levs),
!$$$     x          p_w   = 8*levs+2*levh+6,  !       we/o(lnte/od,2,levs),
!$$$     x          p_x   = 9*levs+2*levh+6,  !       xe/o(lnte/od,2,levs),
!$$$     x          p_y   =10*levs+2*levh+6,  !       ye/o(lnte/od,2,levs),
!$$$     x          p_rt  =11*levs+2*levh+6,  !      rte/o(lnte/od,2,levh),
!$$$     x          p_zq  =11*levs+3*levh+6)  !      zqe/o(lnte/od,2)
!
!
      integer              indlsev,jbasev,n0
      integer              indlsod,jbasod
!!
      include 'function_indlsev'
      include 'function_indlsod'
!!
      logical lsout
      logical, parameter :: flipv = .true.
!!
! for nasa/nrl ozone production and distruction rates:(input through fixio)
      real ozplin(latsozp,levozp,pl_coeff,timeoz)
      integer jindx1(lats_node_r),jindx2(lats_node_r)!for ozone interpolaton
      real ddy(lats_node_r)                          !for ozone interpolaton
      real ozplout(levozp,lats_node_r,pl_coeff)
!moor real ozplout(lonr,levozp,pl_coeff,lats_node_r)
!!
      real(kind=kind_phys), allocatable :: acv(:,:),acvb(:,:),acvt(:,:)
      save acv,acvb,acvt
!!
!     integer, parameter :: maxran=6000, maxsub=6,  maxrs=maxran/maxsub
!     integer, parameter :: maxran=3000, maxsub=6,  maxrs=maxran/maxsub
!     integer, parameter :: maxran=3000, maxsub=15, maxrs=maxran/maxsub
      integer, parameter :: maxran=3000, maxsub=30, maxrs=maxran/maxsub
      type (random_stat) :: stat
      real (kind=kind_phys), allocatable, save :: rannum_tank(:,:,:)
      real (kind=kind_phys), allocatable       :: rannum(:)
!     real (kind=kind_phys), dimension(lonr*maxrs) :: rannum
!     integer iseed, nrc, seed0, kss, ksr, indxr(nrcm), iseedl, latseed
      integer iseed, nrc, seed0, kss, ksr, indxr(nrcm), iseedl, latseed
      integer nf0,nf1,ind,nt,indod,indev
      real(kind=kind_evod) fd2, wrk(1), wrk2(nrcm)

      logical first,ladj
      parameter (ladj=.true.)
      data first/.true./
      save    first, seed0
!!
      integer nlons_v(ngptc)
      real(kind=kind_phys) smc_v(ngptc,lsoil),stc_v(ngptc,lsoil)
     &,                    slc_v(ngptc,lsoil)
     &,                    swh_v(ngptc,levs), hlw_v(ngptc,levs)
     &,                    swhc_v(ngptc,levs), hlwc_v(ngptc,levs)
     &,                    vvel(ngptc,levs)
     &,                    hprime_v(ngptc,nmtvr)
      real(kind=kind_phys) phy_f3dv(ngptc,levs,num_p3d),
     &                     phy_f2dv(ngptc,num_p2d)
     &,                    rannum_v(ngptc,nrcm)
      real(kind=kind_phys) sinlat_v(ngptc),coslat_v(ngptc)
     &,                    ozplout_v(ngptc,levozp,pl_coeff)
      real (kind=kind_rad) rqtk(ngptc), rcs2_lan, rcs_lan

!     real (kind=kind_rad) rcs_v(ngptc), rqtk(ngptc), rcs2_lan
!
!--------------------------------------------------------------------
!     print *,' in gloopb vertcoord_id =',vertcoord_id

!     real(kind=kind_evod) sinlat_v(lonr),coslat_v(lonr),rcs2_v(lonr)
!     real(kind=kind_phys) dpshc(lonr)

      real (kind=kind_rad) work1, tem
      real (kind=kind_rad), parameter :: qmin=1.0e-10
      integer              ksd,ksplam,kspphi
      integer              ksu,ksv,ksz,item,jtem,ktem,ltem,mtem
      integer              nn, nnr, nnrcm, ntr

!
!  ---  for debug test use
      real (kind=kind_phys) :: temlon, temlat, alon, alat
      integer :: ipt
      logical :: lprnt
!
!
      ksq     =0*levs+0*levh+1
      ksplam  =0*levs+0*levh+2
      kspphi  =0*levs+0*levh+3
      ksu     =0*levs+0*levh+4
      ksv     =1*levs+0*levh+4
      ksz     =2*levs+0*levh+4
      ksd     =3*levs+0*levh+4
      kst     =4*levs+0*levh+4
      ksr     =5*levs+0*levh+4
!
      kau     =0*levs+0*levh+1
      kav     =1*levs+0*levh+1
      kat     =2*levs+0*levh+1
      kap     =3*levs+0*levh+1
      kar     =3*levs+0*levh+2
!
!     ksq     = 0*levs + 0*levh + 1
!     kst     = 4*levs + 0*levh + 4                          ! hmhj
      kdtphi  = 0*levs + 0*levh + 1                          ! hmhj
      kdtlam  = 1*levs+1*levh+1                              ! hmhj
!
!$$$      kau     =0*levs+0*levh+1
!$$$      kav     =1*levs+0*levh+1
!$$$      kat     =2*levs+0*levh+1
!$$$      kar     =3*levs+0*levh+1
!$$$      kap     =3*levs+1*levh+1
!!
!!--------------------------------------------------------------------
!!
      do i=1,len_trie_ls
        save_qe_ls(i,1) = trie_ls(i,1,p_q)
        save_qe_ls(i,2) = trie_ls(i,2,p_q)
      enddo
      do i=1,len_trio_ls
        save_qo_ls(i,1) = trio_ls(i,1,p_q)
        save_qo_ls(i,2) = trio_ls(i,2,p_q)
      enddo


!
      if (first) then
        allocate (bfilte(lnt2),bfilto(lnt2))

        allocate (sumtrc(ntrac,3), adjtrc(ntrac))
        sumtrc(:,3) = -1.e30
        adjtrc = 1.0
!
!     initializations for the gloopb filter
!     *************************************
        if (semilag) then
          nf0 = (jcap+1)*9/10 ! highest wavenumber gloopb filter keeps fully - fy
        else
          nf0 = (jcap+1)*2/3  ! highest wavenumber gloopb filter keeps fully
        endif
        nf1 = (jcap+1)        ! lowest wavenumber gloopb filter removes fully
        fd2 = 1./(nf1-nf0)**2
        do locl=1,ls_max_node
             l   = ls_node(locl,1)
          jbasev = ls_node(locl,2)
          indev = indlsev(l,l)
          do n=l,jcap,2
            bfilte(indev) = max(1.-fd2*max(n-nf0,0)**2,cons_0)
            indev         = indev + 1
          enddo
          if (mod(l,2).eq.mod(jcap+1,2)) bfilte(indev) = 1.
        enddo
!!
        do locl=1,ls_max_node
              l  = ls_node(locl,1)
          jbasod = ls_node(locl,3)
          indod  = indlsod(l+1,l)
          do n=l+1,jcap,2
            bfilto(indod) = max(1.-fd2*max(n-nf0,0)**2,cons_0)
            indod = indod+1
          enddo
          if (mod(l,2).ne.mod(jcap+1,2)) bfilto(indod) = 1.
        enddo
!!
        allocate (acv(lonr,lats_node_r))
        allocate (acvb(lonr,lats_node_r))
        allocate (acvt(lonr,lats_node_r))
!
!
        if (cal_pre) then  ! random number needed for ras and old sas
          if (random_clds) then              ! create random number tank
                                             ! -------------------------
            seed0 = idate(1) + idate(2) + idate(3) + idate(4)
            call random_setseed(seed0)
            call random_number(wrk)
            seed0 = seed0 + nint(wrk(1)*1000.0)
            if (me == 0) print *,' seed0=',seed0,' idate=',idate,
     &                           ' wrk=',wrk
!
            if (.not. allocated(rannum_tank))
     &                allocate (rannum_tank(lonr,maxran,lats_node_r))
            if (.not. allocated(rannum)) allocate (rannum(lonr*maxrs))
            lonrbm = lonr / maxsub
            if (me == 0) write(0,*)' maxran=',maxran,' maxrs=',maxrs,
     &          'maxsub=',maxsub,' lonrbm=',lonrbm,
     &          ' lats_node_r=',lats_node_r
!!$omp parallel do  private(j,iseedl,rannum,nrc,nn,i,ii,k,kk)
            do j=1,lats_node_r
              iseedl = global_lats_r(ipt_lats_node_r-1+j) + seed0
              call random_setseed(iseedl)
              call random_number(rannum)
!$omp parallel do  shared(j,lonr,lonrbm,rannum,rannum_tank)
!$omp+private(nrc,nn,i,ii,k,kk)
              do nrc=1,maxrs
                nn = (nrc-1)*lonr
                do k=1,maxsub
                  kk = k - 1
                  do i=1,lonr
                    ii = kk*lonrbm + i
                    if (ii > lonr) ii = ii - lonr
                    rannum_tank(i,nrc+kk*maxrs,j) = rannum(ii+nn)
                  enddo
                enddo
              enddo
            enddo
            if (allocated(rannum)) deallocate (rannum)
          endif
        endif
!
        if (me == 0) then
          if (n3dfercld >0)  print *,' using ferrier-microphysics'
          if (n3dzhaocld >0) print *,' using zhao-microphysics'
          if (n3dcldpdf >0 .and. n3dzhaocld >0)
     &                       print*,' using zhao+pdfcld'
        endif
        if (fhour  == 0.0) then
!$omp parallel do private(i,j)
          do j=1,lats_node_r
            do i=1,lonr
              phy_f2d(i,1,j) = 0.0
            enddo
          enddo
        endif
       
        if (ras) call ras_init(levs, me)
       
        first = .false.
      endif                  ! if (first) done
!

!     Initialize IAU stuff
      if ( iau ) then
         if (.not. iau_initialized) then 
             call init_iau(ls_node,ls_nodes,max_ls_nodes)
         endif
      endif

      if (semilag) then
!       dtphys = 300.0
        nsphys = max(int(tstep/dtphys),1)
        dtp    = tstep / nsphys
        dtf    = dtp
      else
!       dtphys = 3600.
        nsphys = max(int((tstep+tstep)/dtphys+0.9999),1)
        dtp    = (tstep+tstep)/nsphys
        dtf    = 0.5*dtp
      endif
      nnrcm  = max(1, nrcm/nsphys)
!
      if(lsfwd) dtf = dtp
!
      solhr = mod(phour+idate(1),cons_24)

! **************  ken campana stuff  ********************************
!...  set switch for saving convective clouds
      if(lscca.and.lsswr) then
        clstp = 1100+min(fhswr,fhour,cons_99)  !initialize,accumulate,convert
      elseif(lscca) then
        clstp = 0100+min(fhswr,fhour,cons_99)  !accumulate,convert
      elseif(lsswr) then
        clstp = 1100                           !initialize,accumulate
      else
        clstp = 0100                           !accumulate
      endif
! **************  ken campana stuff  ********************************
!
!


      if (cal_pre) then  ! random number needed for ras and old sas
        if (random_clds) then
          iseed = mod(100.0*sqrt(fhour*3600),cons_1p0d9) + 1 + seed0
          call random_setseed(iseed)
          call random_number(wrk2)
          do nrc=1,nrcm
            indxr(nrc) = max(1, min(nint(wrk2(nrc)*maxran)+1,maxran))
          enddo
        endif
      endif
!
! doing ozone i/o and latitudinal interpolation to local gauss lats
!     ifozphys=.true.
 
      if (ntoz > 0) then
       call ozinterpol(me,lats_node_r,lats_node_r,idate,fhour,
     &                 jindx1,jindx2,ozplin,ozplout,ddy)

!moor   call ozinterpol(lats_node_r,lats_node_r,idate,fhour,
!    &                  jindx1,jindx2,ozplin,ozplout,ddy,
!    &                  global_lats_r,lonsperlar)
      endif

!
!
      call delnpe(trie_ls(1,1,p_zq   ),
     x            trio_ls(1,1,p_dphi),
     x            trie_ls(1,1,p_dlam),
     x            epse,epso,ls_node)

      call delnpo(trio_ls(1,1,p_zq   ),
     x            trie_ls(1,1,p_dphi),
     x            trio_ls(1,1,p_dlam),
     x            epse,epso,ls_node)
!
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!$omp+private(k)
      do k=1,levs
         call dezouv(trie_ls(1,1,p_x  +k-1), trio_ls(1,1,p_w  +k-1),
     x               trie_ls(1,1,p_uln+k-1), trio_ls(1,1,p_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
!
         call dozeuv(trio_ls(1,1,p_x  +k-1), trie_ls(1,1,p_w  +k-1),
     x               trio_ls(1,1,p_uln+k-1), trie_ls(1,1,p_vln+k-1),
     x               epsedn,epsodn,snnp1ev,snnp1od,ls_node)
      enddo
!
      dimg=0
!
      call sumfln_slg_gg(trie_ls(1,1,p_q),
     &                   trio_ls(1,1,p_q),
     &                   lat1s_r,
     &                   plnev_r,plnod_r,
     &                   5*levs+3,ls_node,latr2,
     &                   lats_dim_r,lots,for_gr_r_1,
     &                   ls_nodes,max_ls_nodes,
     &                   lats_nodes_r,global_lats_r,
     &                   lats_node_r,ipt_lats_node_r,lon_dim_r,
     &                   lonsperlar,lonrx,latr,0)
!!
      if(.not.gg_tracers)then
        call sumfln_slg_gg(trie_ls(1,1,p_rt),
     &                     trio_ls(1,1,p_rt),
     &                     lat1s_r,
     &                     plnev_r,plnod_r,
     &                     levh,ls_node,latr2,
     &                     lats_dim_r,lots,for_gr_r_1,
     &                     ls_nodes,max_ls_nodes,
     &                     lats_nodes_r,global_lats_r,
     &                     lats_node_r,ipt_lats_node_r,lon_dim_r,
     &                     lonsperlar,lonrx,latr,5*levs+3)
      endif ! if(.not.gg_tracers)then
!
      pwatg = 0.
      ptotg = 0.
!-------------------------------------------------------------------------
!  for stochastic physics
      if (stochphys) then
       if (sppt(1) > 0) then ! stoch. perturbed physics tend.
         allocate(sppt_wt(lonr,lats_node_r,levs))
         call get_pattern_sppt(
     &     ls_node,ls_nodes,max_ls_nodes,
     &     lats_nodes_r,global_lats_r,lonsperlar,
     &     plnev_r,plnod_r,
     &     sppt_wt,nsphys*dtf)
       endif
       if (shum(1) > 0) then ! perturbed pbl humidity
         allocate(shum_wt(lonr,lats_node_r,levs))
         call get_pattern_shum(
     &     ls_node,ls_nodes,max_ls_nodes,
     &     lats_nodes_r,global_lats_r,lonsperlar,
     &     plnev_r,plnod_r,
     &     shum_wt,nsphys*dtf)
       endif
       if (skeb(1) > 0) then ! stochastic ke backscatter
         allocate(skebu_wt(lonr,lats_node_r,levs))
         allocate(skebv_wt(lonr,lats_node_r,levs))
         call get_pattern_skeb(trie_ls(1,1,p_ze),
     &         trio_ls(1,1,p_ze),
     &         trie_ls(1,1,p_di),
     &         trio_ls(1,1,p_di),
     &         ls_node,ls_nodes,max_ls_nodes,
     &         lats_nodes_r,global_lats_r,lonsperlar,
     &         epsedn,epsodn,snnp1ev,snnp1od,
     &         plnev_r,plnod_r,plnew_r,plnow_r,
     &         skebu_wt,
     &         skebv_wt,nsphys*dtf)
       endif
       if (vc > tiny(vc) .or. vcamp(1) > 0) then ! vorticity confinement
         allocate(vcu_wt(lonr,lats_node_r,levs))
         allocate(vcv_wt(lonr,lats_node_r,levs))
         call get_pattern_vc(trie_ls(1,1,p_ze),
     &         trio_ls(1,1,p_ze),
     &         trie_ls(1,1,p_di),
     &         trio_ls(1,1,p_di),
     &         ls_node,ls_nodes,max_ls_nodes,
     &         lats_nodes_r,global_lats_r,lonsperlar,
     &         epsedn,epsodn,snnp1ev,snnp1od,
     &         plnev_r,plnod_r,plnew_r,plnow_r,
     &         vcu_wt,
     &         vcv_wt,nsphys*dtf)
       endif
      endif
!-------------------------------------------------------------------------

        do lan=1,lats_node_r
!         timer1=rtc()
          lat = global_lats_r(ipt_lats_node_r-1+lan)
          lmax = min(jcap,lonsperlar(lat)/2)
          if ( (lmax+1)*2+1 .le. lonsperlar(lat)+2 ) then
            do k=levs+1,4*levs+2*levh
              do i = (lmax+1)*2+1, lonsperlar(lat)+2
                 dyn_gr_r_1(i,k,lan) = cons_0
              enddo
            enddo
          endif
        enddo
!
        call sumder2_slg(trie_ls(1,1,p_te),                             ! hmhj
     x                   trio_ls(1,1,p_te),                             ! hmhj
     x               lat1s_r,                                           ! hmhj
     x               pddev_r,pddod_r,                                   ! hmhj
     x               levs,ls_node,latr2,                                ! hmhj
     x               lats_dim_r,lotd,                                   ! hmhj
     x               dyn_gr_r_1,                                        ! hmhj
     x               ls_nodes,max_ls_nodes,                             ! hmhj
     x               lats_nodes_r,global_lats_r,                        ! hmhj
     x               lats_node_r,ipt_lats_node_r,lon_dim_r,             ! hmhj
     x               lonsperlar,lonrx,latr,0)                           ! hmhj
!
      do lan=1,lats_node_r
!

        lat = global_lats_r(ipt_lats_node_r-1+lan)
!
        lons_lat = lonsperlar(lat)
!-----------------------------------------
!!        calculate t rq u v zonal derivs. by multiplication with i*l
!!        note rlcs2=rcs2*l/rerth

          lmax = min(jcap,lons_lat/2)                                   ! hmhj
!
          ipt_ls=min(lat,latr-lat+1)                                    ! hmhj

          do i=1,lmax+1                                                 ! hmhj
            if ( ipt_ls .ge. lat1s_r(i-1) ) then                        ! hmhj
               reall=i-1                                                ! hmhj
               rlcs2(i)=reall*rcs2_r(ipt_ls)/rerth                      ! hmhj
            else                                                        ! hmhj
               rlcs2(i)=cons_0                                          ! hmhj
            endif                                                       ! hmhj
          enddo                                                         ! hmhj
!
!$omp parallel do private(k,i)
          do k=1,levs                                                   ! hmhj
            do i=1,lmax+1                                               ! hmhj
!
!           d(t)/d(lam)                                                 ! hmhj
               dyn_gr_r_1(2*i-1,(kdtlam-1+k),lan)=                      ! hmhj
     x        -for_gr_r_1(2*i  ,(kst   -1+k),lan)*rlcs2(i)              ! hmhj
               dyn_gr_r_1(2*i  ,(kdtlam-1+k),lan)=                      ! hmhj
     x         for_gr_r_1(2*i-1,(kst   -1+k),lan)*rlcs2(i)              ! hmhj
            enddo                                                       ! hmhj
          enddo                                                         ! hmhj
! -----------------------
!       endif
! -----------------------

!!
        call four_to_grid(for_gr_r_1(1,1,lan),for_gr_r_2(1,1,lan),
     &                    lon_dim_r,lon_dim_r-2,lons_lat,5*levs+3)

        if(.not.gg_tracers)then
          call four_to_grid(for_gr_r_1(1,ksr,lan),
     &                      for_gr_r_2(1,ksr,lan),
     &                      lon_dim_r,lon_dim_r-2,lons_lat,levh)
        else  ! gg_tracers
          if (.not.shuff_lats_r) then
!                  set for_gr_r_2 to rg1_a rg2_a rg3_a from gloopa
            do n=1,ntrac
!$omp parallel do private(k,i,item,jtem)
              do k=1,levs
                item = ksr - 1 + k +(n-1)*levs
                jtem = lats_node_a+1-lan
                do i=1,min(lonf,lons_lat)
                  for_gr_r_2(i,item,lan) = rgt_a(i,k,jtem,n)
                enddo
              enddo
            enddo
          endif              ! not shuff_lats_r
        endif                ! gg_tracers

! ----------------------------------
          call four_to_grid(dyn_gr_r_1(1,kdtphi,lan),                ! hmhj
     &                      dyn_gr_r_2(1,kdtphi,lan),                ! hmhj
     &                      lon_dim_r,lon_dim_r-2,lons_lat,levs)     ! hmhj
          call four_to_grid(dyn_gr_r_1(1,kdtlam,lan),                ! hmhj
     &                      dyn_gr_r_2(1,kdtlam,lan),                ! hmhj
     &                      lon_dim_r,lon_dim_r-2,lons_lat,levs)     ! hmhj
!
!!
      enddo   !lan
!!

      if(gg_tracers .and. shuff_lats_r) then
         call mpi_tracers_a_to_b(rgt_a,lats_nodes_a,global_lats_a,
     &                           for_gr_r_2(1,1,1),
     &                           lats_nodes_r,global_lats_r,ksr,0)
      endif                       ! gg_tracers .and.  shuff_lats_r


      do lan=1,lats_node_r

!
        lat = global_lats_r(ipt_lats_node_r-1+lan)
!!
!       lon_dim  = lon_dims_r(lan)
        lons_lat = lonsperlar(lat)
        pwatp       = 0.
        trcp(:,:,:) = 0.0
        rcs2_lan = rcs2_r(min(lat,latr-lat+1))
        rcs_lan  = sqrt(rcs2_lan)

!$omp parallel do  schedule(dynamic,1) private(lon)
!$omp+private(sumq,xcp,hprime_v,swh_v,hlw_v)
!$omp+private(swhc_v,hlwc_v,stc_v,smc_v,slc_v)
!$omp+private(nlons_v,sinlat_v,coslat_v,ozplout_v,rannum_v)
!$omp+private(prslk,prsl,prsik,prsi,phil,phii,dpshc,work1,tem)
!$omp+private(gu,gv1,gd,gq,gphi,glam,gt,gtv,gr,vvel,gtvx,gtvy)
!$omp+private(gt0,gr0,ugrd0,vgrd0)
!$omp+private(adt,adr,adu,adv,pgr,ugrd,vgrd,rqtk)
!$omp+private(phy_f3dv,phy_f2dv)
!$omp+private(dt3dt_v,dq3dt_v,du3dt_v,dv3dt_v,upd_mf_v,dwn_mf_v)
!$omp+private(det_mf_v)
!$omp+private(njeff,item,jtem,ktem,i,j,k,kss,n,nn,nnr)
!$omp+private(uphys,vphys,tphys,qphys)
!$omp+private(sppt_wts)
!$omp+private(dtdt)

!!!$omp+private(temlon,temlat,lprnt,ipt)

        do lon=1,lons_lat,ngptc
          njeff = min(ngptc,lons_lat-lon+1)
!!
          do k = 1, levs
            do j = 1, njeff
             jtem = lon-1+j
             gu (j,k)  = for_gr_r_2(jtem,ksu-1+k,lan)
             gv1(j,k)  = for_gr_r_2(jtem,ksv-1+k,lan)
             gd (j,k)  = for_gr_r_2(jtem,ksd-1+k,lan)
            enddo
          enddo
!
! p in cb by finite difference from henry juang not ln(p)               ! hmhj
          if(.not.gen_coord_hybrid) then                                ! hmhj
            do j=1,njeff
              item = lon+j-1
              work1 = exp(for_gr_r_2(item,ksq,lan))
              for_gr_r_2(item,ksq,lan) = work1
!             for_gr_r_2(item,ksq,lan) = exp(for_gr_r_2(item,ksq,lan))
            enddo
          endif     ! .not.gen_coord_hybrid                             ! hmhj
          do i=1,njeff
            item = lon+i-1
            gq(i)   = for_gr_r_2(item,ksq,lan)
            gphi(i) = for_gr_r_2(item,kspphi,lan)
            glam(i) = for_gr_r_2(item,ksplam,lan)
          enddo
!  tracers
          do n=1,ntrac
            do k=1,levs
              item = ksr-1+k+(n-1)*levs
              do j=1,njeff
                gr(j,k,n) = for_gr_r_2(lon-1+j,item,lan)
              enddo
            enddo
          enddo

!
!   for omega in gen_coord_hybrid                                         ! hmhj
!   the same variables for thermodyn_id=3 for enthalpy                    ! hmhj
          if( gen_coord_hybrid ) then
            do k=1,levs                                                   ! hmhj
              do j=1,njeff                                                ! hmhj
                gtv(j,k) = for_gr_r_2(lon-1+j,kst-1+k,lan) 
              enddo
            enddo
! --------------------------------------
            if( vertcoord_id == 3. ) then
! --------------------------------------
              do k=1,levs                                                ! hmhj
                do j=1,njeff                                             ! hmhj
                  jtem = lon-1+j
                  gtvx(j,k) = dyn_gr_r_2(jtem,kdtlam-1+k,lan)
                  gtvy(j,k) = dyn_gr_r_2(jtem,kdtphi-1+k,lan)
                enddo                                                    ! hmhj
              enddo
! -----------------------------
            endif
! -----------------------------
            if( thermodyn_id == 3 ) then                                ! hmhj
!               get dry temperature from enthalpy                       ! hmhj
              do k=1,levs
                do j=1,njeff
                  sumq(j,k) = 0.0
                  xcp(j,k)  = 0.0
                enddo
              enddo
              do n=1,ntrac                                                ! hmhj
                if( cpi(n) /= 0.0 ) then                                  ! hmhj
!                 kss = ksr+(n-1)*levs                                    ! hmhj
                  do k=1,levs                                             ! hmhj
!                   ktem = kss+k-1
                    do j=1,njeff                                          ! hmhj
                      sumq(j,k) = sumq(j,k) + gr(j,k,n)                   ! hmhj
                      xcp(j,k)  = xcp(j,k)  + cpi(n)*gr(j,k,n)            ! hmhj
                    enddo                                                 ! hmhj
                  enddo                                                   ! hmhj
                endif                                                     ! hmhj
              enddo                                                       ! hmhj
              do k=1,levs                                                 ! hmhj
                do j=1,njeff                                              ! hmhj
                  work1 = (1.-sumq(j,k))*cpi(0) + xcp(j,k)                ! hmhj
                  gt(j,k) = gtv(j,k) / work1                              ! hmhj
                enddo                                                     ! hmhj
              enddo                                                       ! hmhj
! get dry temperature from virtual temperature                            ! hmhj
            else if( thermodyn_id <= 1 ) then                             ! hmhj
              do k=1,levs
                do j=1,njeff
                  gt(j,k) = gtv(j,k) / (1.0 + fv*max(gr(j,k,1),qmin))
                enddo
              enddo
            else
! get dry temperature from dry temperature                              ! hmhj
              do k=1,levs                                               ! hmhj
                do j=1,njeff                                            ! hmhj
                  gt(j,k) = gtv(j,k)                                    ! hmhj
                enddo                                                   ! hmhj
              enddo
            endif               ! if(thermodyn_id.eq.3)
          else
            do k=1,levs
              do j=1,njeff
                gt(j,k) = for_gr_r_2(lon+j-1,kst+k-1,lan)
     &                  / (1.0 + fv*max(gr(j,k,1),qmin))
              enddo
            enddo

          endif               ! if(gen_coord_hybrid)
!
          do j=1,njeff
            item = lon+j-1
            gq_save(item,lan) = for_gr_r_2(item,ksq,lan)
          enddo
!
!                                   hmhj for gen_coord_hybrid
          if( gen_coord_hybrid ) then                                   ! hmhj

            call hyb2press_gc(njeff,ngptc,gq, gtv, prsi,prsl            ! hmhj
     &,                                            prsik, prslk)
            call omegtes_gc(njeff,ngptc,rcs2_lan,                       ! hmhj
     &                   gq,gphi,glam,gtv,gtvx,gtvy,gd,gu,gv1,vvel)     ! hmhj
          elseif( hybrid )then                                          ! hmhj
!               vertical structure variables:   del,si,sl

            call  hyb2press(njeff,ngptc,gq, prsi, prsl,prsik,prslk)
            call omegtes(njeff,ngptc,rcs2_lan,
     &                   gq,gphi,glam,gd,gu,gv1,vvel)


          endif
!
          do i=1,njeff
            phil(i,levs) = 0.0 ! forces calculation of geopotential in gbphys
            pgr(i)       = gq(i) * 1000.0  ! convert from kpa to pa for physics
            prsi(i,1)    = pgr(i)
            dpshc(i)     = 0.3  * prsi(i,1)
!
            nlons_v(i)   = lons_lat
            sinlat_v(i)  = sinlat_r(lat)
            coslat_v(i)  = coslat_r(lat)
!           rcs_v(i)     = sqrt(rcs2_lan)
!           rcs_v(i)     = sqrt(rcs2_r(min(lat,latr-lat+1)))
          enddo
          do k=1,levs
            do i=1,njeff
              ugrd(i,k)   = gu(i,k)     * rcs_lan
              vgrd(i,k)   = gv1(i,k)    * rcs_lan
!             ugrd(i,k)   = gu(i,k)     * rcs_v(i)
!             vgrd(i,k)   = gv1(i,k)    * rcs_v(i)
!             work1       = prsl(i,k)   * 1000.0
!             prsl(i,k)   = work1
!             work1       = prsi(i,k+1) * 1000.0
!             prsi(i,k+1) = work1
!             work1       = vvel(i,k)   * 1000.0  ! convert from cb/s to pa/s
!             vvel(i,k)   = work1
              prsl(i,k)   = prsl(i,k)   * 1000.0
              prsi(i,k+1) = prsi(i,k+1) * 1000.0
              vvel(i,k)   = vvel(i,k)   * 1000.0  ! convert from cb/s to pa/s
            enddo
          enddo
          if (gen_coord_hybrid .and. thermodyn_id == 3) then
            do i=1,ngptc
              prslk(i,1) = 0.0 ! forces calculation of geopotential in gbphys
              prsik(i,1) = 0.0 ! forces calculation of geopotential in gbphys
            enddo
          endif
!
          if (ntoz > 0) then
            do j=1,pl_coeff
              do k=1,levozp
                do i=1,ngptc
!moorthi          ozplout_v(i,k,j) = ozplout(lon+i-1,k,j,lan)
                  ozplout_v(i,k,j) = ozplout(k,lan,j)
                enddo
              enddo
            enddo
          endif
          do k=1,lsoil
            do i=1,njeff
              item = lon+i-1
              smc_v(i,k) = sfc_fld%smc(item,k,lan)
              stc_v(i,k) = sfc_fld%stc(item,k,lan)
              slc_v(i,k) = sfc_fld%slc(item,k,lan)
            enddo
          enddo
          do k=1,nmtvr
            do i=1,njeff
              hprime_v(i,k) = hprime(lon+i-1,k,lan)
            enddo
          enddo
!!
          do j=1,num_p3d
            do k=1,levs
              do i=1,njeff
                phy_f3dv(i,k,j) = phy_f3d(lon+i-1,k,j,lan)
              enddo
            enddo
          enddo
          do j=1,num_p2d
            do i=1,njeff
              phy_f2dv(i,j) = phy_f2d(lon+i-1,j,lan)
            enddo
          enddo
          if (cal_pre) then
            if (random_clds) then
              do j=1,nrcm
                do i=1,njeff
                  rannum_v(i,j) = rannum_tank(lon+i-1,indxr(j),lan)
                enddo
              enddo
            else
              do j=1,nrcm
                do i=1,njeff
                  rannum_v(i,j) = 0.6    ! this is useful for debugging
                enddo
              enddo
            endif
          endif
          do k=1,levs
            do i=1,njeff
              item = lon+i-1
              swh_v(i,k) = swh(item,k,lan)
              hlw_v(i,k) = hlw(item,k,lan)
              swhc_v(i,k) = swhc(item,k,lan)
              hlwc_v(i,k) = hlwc(item,k,lan)
            enddo
          enddo
          if (ldiag3d) then
            do n=1,6
             do k=1,levs
              do i=1,njeff
               dt3dt_v(i,k,n) = dt3dt(lon+i-1,k,n,lan)
              enddo
             enddo
            enddo
            do n=1,4
             do k=1,levs
              do i=1,njeff
               du3dt_v(i,k,n) = du3dt(lon+i-1,k,n,lan)
               dv3dt_v(i,k,n) = dv3dt(lon+i-1,k,n,lan)
              enddo
             enddo
            enddo
            do n=1,5+pl_coeff
             do k=1,levs
              do i=1,njeff
               dq3dt_v(i,k,n) = dq3dt(lon+i-1,k,n,lan)
              enddo
             enddo
            enddo
            do k=1,levs
             do i=1,njeff
              upd_mf_v(i,k) = upd_mf(lon+i-1,k,lan)
              dwn_mf_v(i,k) = dwn_mf(lon+i-1,k,lan)
              det_mf_v(i,k) = det_mf(lon+i-1,k,lan)
             enddo
            enddo
          endif
! set dtdt (clear sky radiation change) to zero before gbphys for sppt stochastic physics
          do k=1,levs
            do i=1,njeff
              dtdt(i,k) = 0.0
            enddo
          enddo
!
!  save a copy of the state before going into physics loop for sppt
          if (sppt(1) > 0) then
             gt0   = gt
             gr0   = gr
             ugrd0 = ugrd
             vgrd0 = vgrd
          endif

!     for tracer fixer

          do ntr=1,ntrac
            if (fixtrc(ntr) .and. .not. iauforcing) then
              do k=1,levs
                do i=1,njeff
                  trcp(lon+i-1,ntr,1) = trcp(lon+i-1,ntr,1)
     &                         + gr(i,k,ntr) * (prsi(i,k) - prsi(i,k+1))
                enddo
              enddo
            endif
          enddo
          do nn=1,nsphys
            nnr = mod((nn-1)*nnrcm, nrcm) + 1
            call gbphys                                                 &
!  ---  inputs:
     &    ( njeff,ngptc,levs,lsoil,ntrac,ncld,ntoz,ntcw,                &
     &      nmtvr,nnrcm,levozp,lonr,latr,jcap,num_p3d,num_p2d,          &
     &      kdt,lat,me,pl_coeff,nlons_v,ncw,flgmin,crtrh,cdmbgwd,       &
     &      ccwf,dlqf,clstp,cgwf,dtp,dtf,fhour,solhr,                   &
     &      slag,sdec,cdec,sinlat_v,coslat_v,pgr,ugrd,vgrd,             &
     &      gt,gr,vvel,prsi,prsl,prslk,prsik,phii,phil,                 &
     &      rannum_v(1,nnr),ozplout_v,pl_pres,dpshc,                    &
     &      hprime_v, xlon(lon,lan),xlat(lon,lan),                      &
     &      sfc_fld%slope (lon,lan),    sfc_fld%shdmin(lon,lan),        &
     &      sfc_fld%shdmax(lon,lan),    sfc_fld%snoalb(lon,lan),        &
     &      sfc_fld%tg3   (lon,lan),    sfc_fld%slmsk (lon,lan),        &
     &      sfc_fld%vfrac (lon,lan),    sfc_fld%vtype (lon,lan),        &
     &      sfc_fld%stype (lon,lan),    sfc_fld%uustar(lon,lan),        &
     &      sfc_fld%oro   (lon,lan),    sfc_fld%oro_uf(lon,lan),        &
     &      flx_fld%coszen(lon,lan),                                    &
     &      flx_fld%sfcdsw(lon,lan),    flx_fld%sfcnsw(lon,lan),        &
     &      flx_fld%sfcdlw(lon,lan),    flx_fld%tsflw (lon,lan),        &
     &      flx_fld%sfcemis(lon,lan),   sfalb(lon,lan),                 &
     &      swh_v,swhc_v,               hlw_v,hlwc_v,                   &
     &      ras,pre_rad,ldiag3d,lssav,lssav_cc,                         &
     &      bkgd_vdif_m,bkgd_vdif_h,bkgd_vdif_s,psautco,prautco, evpco, &
     &      wminco,sup,pdfcld,shcnvcw,                                  &
     &      flipv,cnvgwd,shal_cnv,                                      &
     &      redrag,hybedmf,dspheat,cal_pre,                             &
     &      mom4ice,trans_trac,nst_fcst,fscav,                          &
     &      thermodyn_id, sfcpress_id, gen_coord_hybrid,  adjtrc, nn,   &
!  ---  input/outputs:
     &      sfc_fld%hice  (lon,lan),    sfc_fld%fice  (lon,lan),        &
     &      sfc_fld%tisfc (lon,lan),    sfc_fld%tsea  (lon,lan),        &
     &      sfc_fld%tprcp (lon,lan),    sfc_fld%cv    (lon,lan),        &
     &      sfc_fld%cvb   (lon,lan),    sfc_fld%cvt   (lon,lan),        &
     &      sfc_fld%srflag(lon,lan),    sfc_fld%snwdph(lon,lan),        &
     &      sfc_fld%sheleg(lon,lan),    sfc_fld%sncovr(lon,lan),        &
     &      sfc_fld%zorl  (lon,lan),    sfc_fld%canopy(lon,lan),        &
     &      sfc_fld%ffmm  (lon,lan),    sfc_fld%ffhh  (lon,lan),        &
     &      sfc_fld%f10m  (lon,lan),    flx_fld%srunoff(lon,lan),       &
     &      flx_fld%evbsa (lon,lan),    flx_fld%evcwa (lon,lan),        &
     &      flx_fld%snohfa(lon,lan),    flx_fld%transa(lon,lan),        &
     &      flx_fld%sbsnoa(lon,lan),    flx_fld%snowca(lon,lan),        &
     &      flx_fld%soilm (lon,lan),    flx_fld%tmpmin(lon,lan),        &
     &      flx_fld%tmpmax(lon,lan),    flx_fld%dusfc (lon,lan),        &
     &      flx_fld%dvsfc (lon,lan),    flx_fld%dtsfc (lon,lan),        &
     &      flx_fld%dqsfc (lon,lan),    flx_fld%geshem(lon,lan),        &
     &      flx_fld%gflux (lon,lan),    flx_fld%dlwsfc(lon,lan),        &
     &      flx_fld%ulwsfc(lon,lan),    flx_fld%suntim(lon,lan),        &
     &      flx_fld%runoff(lon,lan),    flx_fld%ep    (lon,lan),        &
     &      flx_fld%cldwrk(lon,lan),    flx_fld%dugwd (lon,lan),        &
     &      flx_fld%dvgwd (lon,lan),    flx_fld%psmean(lon,lan),        &
     &      flx_fld%bengsh(lon,lan),    flx_fld%spfhmin(lon,lan),       &
     &      flx_fld%spfhmax(lon,lan),                                   &
     &      dt3dt_v, dq3dt_v, du3dt_v, dv3dt_v,                         &
     &      acv(lon,lan), acvb(lon,lan), acvt(lon,lan),                 &
     &      slc_v, smc_v, stc_v,                                        &
     &      upd_mf_v, dwn_mf_v, det_mf_v,                               &
     &      phy_f3dv, phy_f2dv,                                         &
     &      dlwsfc_cc(lon,lan),  ulwsfc_cc(lon,lan),                    &
     &      dtsfc_cc(lon,lan),   swsfc_cc(lon,lan),                     &
     &      dusfc_cc(lon,lan),   dvsfc_cc(lon,lan),                     &
     &      dqsfc_cc(lon,lan),   precr_cc(lon,lan),                     &

     &      nst_fld%xt(lon,lan),        nst_fld%xs(lon,lan),            &
     &      nst_fld%xu(lon,lan),        nst_fld%xv(lon,lan),            &
     &      nst_fld%xz(lon,lan),        nst_fld%zm(lon,lan),            &
     &      nst_fld%xtts(lon,lan),      nst_fld%xzts(lon,lan),          &
     &      nst_fld%d_conv(lon,lan),    nst_fld%ifd(lon,lan),           &
     &      nst_fld%dt_cool(lon,lan),   nst_fld%qrain(lon,lan),         &
!  ---  outputs:
     &      adt, adr, adu, adv,                                         &
     &      sfc_fld%t2m   (lon,lan),    sfc_fld%q2m   (lon,lan),        &
     &      flx_fld%u10m  (lon,lan),    flx_fld%v10m  (lon,lan),        &
     &      flx_fld%zlvl  (lon,lan),    flx_fld%psurf (lon,lan),        &
     &      flx_fld%hpbl  (lon,lan),    flx_fld%pwat  (lon,lan),        &
     &      flx_fld%t1    (lon,lan),    flx_fld%q1    (lon,lan),        &
     &      flx_fld%u1    (lon,lan),    flx_fld%v1    (lon,lan),        &
     &      flx_fld%chh   (lon,lan),    flx_fld%cmm   (lon,lan),        &
     &      flx_fld%dlwsfci(lon,lan),   flx_fld%ulwsfci(lon,lan),       &
     &      flx_fld%dswsfci(lon,lan),   flx_fld%uswsfci(lon,lan),       &
     &      flx_fld%dtsfci(lon,lan),    flx_fld%dqsfci(lon,lan),        &
     &      flx_fld%gfluxi(lon,lan),    flx_fld%epi   (lon,lan),        &
     &      flx_fld%smcwlt2(lon,lan),   flx_fld%smcref2(lon,lan),       &
     &      flx_fld%sr(lon,lan),                                        &
     &      xmu_cc(lon,lan), dlw_cc(lon,lan), dsw_cc(lon,lan),          &
     &      snw_cc(lon,lan), lprec_cc(lon,lan),                         &

     &      nst_fld%tref(lon,lan),       nst_fld%z_c(lon,lan),          &
     &      nst_fld%c_0 (lon,lan),       nst_fld%c_d(lon,lan),          &
     &      nst_fld%w_0 (lon,lan),       nst_fld%w_d(lon,lan),          &
!     &      rqtk                                                        &! rqtkd
! get radiation tendencies (so they can be left out of sppt stochastic physics)
     &      rqtk,dtdt)                                                  &! rqtkd
!!
            if (nn < nsphys) then
              gt   = adt
              gr   = adr
              ugrd = adu
              vgrd = adv
            else
              do ntr=1,ntrac
                if (fixtrc(ntr) .and. .not. iauforcing) then
                  do k=1,levs
                    do i=1,njeff
                      trcp(lon+i-1,ntr,2) = trcp(lon+i-1,ntr,2)
     &                        + adr(i,k,ntr) * (prsi(i,k) - prsi(i,k+1))
                    enddo
                  enddo
                endif
              enddo
            endif
          enddo                                ! end of nsphys loop
!-------------------------------------------------------------------------

          if (stochphys) then    !!! code section for sppt stochastic physics
                                   ! ----------------------------------------
            if (sppt(1) > 0) then
              do k=1,levs
                do j=1,njeff
                  sppt_wts  = sppt_wt(lon+j-1,lan,k)
                  uphys = adu(j,k) - ugrd0(j,k)
                  vphys = adv(j,k) - vgrd0(j,k)
                  qphys = adr(j,k,1) - gr0(j,k,1)
!   perturb increments (adding radiation contribution back in)
                  uphys = uphys*sppt_wts + ugrd0(j,k)
                  vphys = vphys*sppt_wts + vgrd0(j,k)
                  qphys = qphys*sppt_wts + gr0(j,k,1)
!   make sure sppt perturbation does not create neg humidity.
!   where perturbed humidity is negative, do not change temp and humidity
!   values.
! put new grid back into adjusted variables
                  if(qphys >  qmin) then
                    tphys = adt(j,k) - gt0(j,k) - dtdt(j,k) ! remove clear sky radiation contribution
                    tphys = tphys*sppt_wts + gt0(j,k)+dtdt(j,k)
                    adt(j,k)   = tphys
                    adr(j,k,1) = qphys
                  end if
                  adu(j,k)   = uphys
                  adv(j,k)   = vphys
                enddo
              enddo
            endif  ! do_sppt

!!! code section for stochastic boundary layer humidity perturbation
            if (shum(1) > 0) then
              do k=1,levs
                do j=1,njeff
                 adr(j,k,1) = adr(j,k,1) * (1. + shum_wt(lon+j-1,lan,k))
                enddo
              enddo
            endif ! do_shum

!!! code section for additive noise (skeb) perturbation
            if (skeb(1) > 0) then
              do k=1,levs
                do j=1,njeff
                  adu(j,k) = adu(j,k) + skebu_wt(lon+j-1,lan,k)
                  adv(j,k) = adv(j,k) + skebv_wt(lon+j-1,lan,k)
                enddo
              enddo
            endif ! do_skeb
!!! code section for vorticity confinement perturbation.
            if (vc > tiny(vc) .or. vcamp(1) > 0) then
              do k=1,levs
                do j=1,njeff
                  adu(j,k) = adu(j,k) + vcu_wt(lon+j-1,lan,k)
                  adv(j,k) = adv(j,k) + vcv_wt(lon+j-1,lan,k)
                enddo
              enddo
            endif ! do_vc
          endif                 !! end of stochastic physics code
                                 ! ----------------------------------------
!
          do k=1,levs
            do i=1,njeff
              item = lon + i - 1
              bak_gr_r_2(item,kau+k-1,lan) = adu(i,k) * rcs_lan
              bak_gr_r_2(item,kav+k-1,lan) = adv(i,k) * rcs_lan
!             bak_gr_r_2(item,kau+k-1,lan) = adu(i,k) * rcs_v(i)
!             bak_gr_r_2(item,kav+k-1,lan) = adv(i,k) * rcs_v(i)
              bak_gr_r_2(item,kat+k-1,lan) = adt(i,k)
            enddo
          enddo
          do n=1,ntrac
            do k=1,levs
              ktem = kar+k-1+(n-1)*levs
              do i=1,njeff
                item = lon + i - 1
                bak_gr_r_2(item,ktem,lan) = adr(i,k,n)
              enddo
            enddo
          enddo
          if (gg_tracers) then
            do i=1,njeff
              bak_gr_r_2(lon+i-1,kap,lan) = rqtk(i)
            enddo
          else
            do i=1,njeff
              bak_gr_r_2(lon+i-1,kap,lan) = 0.0
            enddo
          endif
!!
!<-- cpl insertion: instantanious variables
          do i=1,njeff
            item = lon+i-1
            u_bot_cc(item,lan)  = adu(i,1)
            v_bot_cc(item,lan)  = adv(i,1)
            q_bot_cc(item,lan)  = adr(i,1,1)
            p_bot_cc(item,lan)  = prsl(i,1)
            p_surf_cc(item,lan) = prsi(i,1)
          enddo

          do i=1,njeff
            item = lon+i-1
            t_bot_cc(item,lan) = adt(i,1)
            tem                = adt(i,1)*(1+rvrdm1*adr(i,1,1))
            z_bot_cc(item,lan) = -(rd/grav)*tem
     &                         * log(prsl(i,1)/prsi(i,1))
!
            ffmm_cc(item,lan)  = sfc_fld%ffmm(item,lan)
            ffhh_cc(item,lan)  = sfc_fld%ffhh(item,lan)
            if (sfc_fld%slmsk(item,lan) .lt. 0.01) then
              t_sfc_cc(item,lan) = sfc_fld%tsea(item,lan)
     &                           + (sfc_fld%oro(item,lan)
     &                           -  sfc_fld%oro_uf(item,lan))*rlapse
            else
              t_sfc_cc(item,lan) = sfc_fld%tsea(item,lan)
            end if
            fice_sfc_cc(item,lan) = sfc_fld%fice(item,lan)
            hice_sfc_cc(item,lan) = sfc_fld%hice(item,lan)
     &                            * sfc_fld%fice(item,lan)
          enddo
!-----------------------------------------------------------------------------
!--> cpl insertion

          if( gen_coord_hybrid .and. thermodyn_id.eq.3 ) then           ! hmhj

!            convert dry temperature to enthalpy                        ! hmhj
            do k=1,levs
              do j=1,njeff
                item = lon+j-1
                sumq(j,k) = 0.0
                xcp(j,k)  = 0.0
              enddo
            enddo
            do i=1,ntrac                                                ! hmhj
              kss = kar+(i-1)*levs
              if( cpi(i).ne.0.0 ) then                                  ! hmhj
                do k=1,levs                                             ! hmhj
                  ktem = kss+k-1
                  do j=1,njeff                                          ! hmhj
                    item      = lon+j-1
                    work1     = bak_gr_r_2(item,ktem,lan)               ! hmhj
                    sumq(j,k) = sumq(j,k) + work1                       ! hmhj
                    xcp(j,k)  = xcp(j,k)  + cpi(i)*work1                ! hmhj
                enddo                                                   ! hmhj
               enddo                                                    ! hmhj
             endif                                                      ! hmhj
            enddo                                                       ! hmhj
            do k=1,levs                                                 ! hmhj
              ktem = kat+k-1
              do j=1,njeff                                              ! hmhj
                item  = lon+j-1
                work1 = (1.-sumq(j,k))*cpi(0) + xcp(j,k)                ! hmhj
                bak_gr_r_2(item,ktem,lan) = bak_gr_r_2(item,ktem,lan)
     &                                    * work1                       ! hmhj
                adt(j,k) = adt(j,k)*work1
              enddo                                                     ! hmhj
            enddo                                                       ! hmhj

          else                                                          ! hmhj

!           convert dry temperture to virtual temperature               ! hmhj
            do k=1,levs                                                 ! hmhj
              ktem = kar+k-1
              jtem = kat+k-1
              do j=1,njeff                                              ! hmhj
                item  = lon+j-1
                work1 = 1.0 + fv * max(bak_gr_r_2(item,ktem,lan),qmin)  ! hmhj
                bak_gr_r_2(item,jtem,lan) = bak_gr_r_2(item,jtem,lan)
     &                                    * work1                       ! hmhj
                adt(j,k) = adt(j,k)*work1
              enddo                                                     ! hmhj
            enddo                                                       ! hmhj

          endif
          if( gen_coord_hybrid .and. vertcoord_id == 3. ) then          ! hmhj
            prsi  = prsi * 0.001                 ! convert from pa to kpa
            if( thermodyn_id == 3. ) then                               ! hmhj
              call gbphys_adv_h(njeff,ngptc,dtf,gtv,gu,gv1,gr , gq,    ! hmhj
     &                              adt,adu,adv,adr,prsi )
!             call gbphys_adv_h(njeff,ngptc,dtf,
!    &                  for_gr_r_2(lon,kst,lan),
!    &                  for_gr_r_2(lon,ksu,lan),
!    &                  for_gr_r_2(lon,ksv,lan),
!    &                  for_gr_r_2(lon,ksr,lan),
!    &                  for_gr_r_2(lon,ksq,lan),
!    &                  bak_gr_r_2(lon,kat,lan),
!    &                  bak_gr_r_2(lon,kau,lan),
!    &                  bak_gr_r_2(lon,kav,lan),
!    &                  bak_gr_r_2(lon,kar,lan),
!    &                  prsi )
            else
              call gbphys_adv(njeff,ngptc,dtf,gtv,gu,gv1,gr,gq,       ! hmhj
     &                              adt,adu,adv,adr,prsi )
!             call gbphys_adv(njeff,ngptc,dtf,
!    &                  for_gr_r_2(lon,kst,lan),
!    &                  for_gr_r_2(lon,ksu,lan),
!    &                  for_gr_r_2(lon,ksv,lan),
!    &                  for_gr_r_2(lon,ksr,lan),
!    &                  for_gr_r_2(lon,ksq,lan),
!    &                  bak_gr_r_2(lon,kat,lan),
!    &                  bak_gr_r_2(lon,kau,lan),
!    &                  bak_gr_r_2(lon,kav,lan),
!    &                  bak_gr_r_2(lon,kar,lan),
!    &                  prsi )
              endif                                                       ! hmhj
            endif                                                         ! hmhj
!!
            do k=1,lsoil
              do i=1,njeff
                item = lon + i - 1
                sfc_fld%smc(item,k,lan) = smc_v(i,k)
                sfc_fld%stc(item,k,lan) = stc_v(i,k)
                sfc_fld%slc(item,k,lan) = slc_v(i,k)
              enddo
            enddo
!!
            do j=1,num_p3d
              do k=1,levs
                do i=1,njeff
                  phy_f3d(lon+i-1,k,j,lan) = phy_f3dv(i,k,j)
                enddo
              enddo
            enddo
            do j=1,num_p2d
              do i=1,njeff
                phy_f2d(lon+i-1,j,lan) = phy_f2dv(i,j)
              enddo
            enddo
!
          if (ldiag3d) then
            do n=1,6
              do k=1,levs
                do i=1,njeff
                  dt3dt(lon+i-1,k,n,lan) = dt3dt_v(i,k,n)
                enddo
              enddo
            enddo
            do n=1,4
              do k=1,levs
                do i=1,njeff
                  du3dt(lon+i-1,k,n,lan) = du3dt_v(i,k,n)
                  dv3dt(lon+i-1,k,n,lan) = dv3dt_v(i,k,n)
                enddo
              enddo
            enddo
            do n=1,5+pl_coeff
              do k=1,levs
                do i=1,njeff
                  dq3dt(lon+i-1,k,n,lan) = dq3dt_v(i,k,n)
                enddo
              enddo
            enddo
            do k=1,levs
              do i=1,njeff
                upd_mf(lon+i-1,k,lan) = upd_mf_v(i,k)
                dwn_mf(lon+i-1,k,lan) = dwn_mf_v(i,k)
                det_mf(lon+i-1,k,lan) = det_mf_v(i,k)
              enddo
            enddo
          endif
!
        enddo   ! lon loop   - end of big threaded loop
                !              ------------------------
!
!
!       call dscal(levs*lonr,rcs2_v,bak_gr_r_2(1,kau,lan),1)
!       call dscal(levs*lonr,rcs2_v,bak_gr_r_2(1,kav,lan),1)
!
!
        ptotj(lan) = 0.
        do j=1,lons_lat
          ptotj(lan) = ptotj(lan) + gq_save(j,lan)
          pwatp      = pwatp + flx_fld%pwat(j,lan)
        enddo
        tem = 0.5 / lonsperlar(lat)
        pwatj(lan) = pwatp * grav * tem * 1.0e-3
        ptotj(lan) = ptotj(lan) * tem
!
        do ntr=1,ntrac
          if (fixtrc(ntr) .and. .not. iauforcing) then
            trcj(lan,ntr,1) = 0.0
            trcj(lan,ntr,2) = 0.0
            do j=1,lons_lat
              trcj(lan,ntr,1) = trcj(lan,ntr,1) + trcp(j,ntr,1)
              trcj(lan,ntr,2) = trcj(lan,ntr,2) + trcp(j,ntr,2)
            enddo
            trcj(lan,ntr,1) = trcj(lan,ntr,1) * tem
            trcj(lan,ntr,2) = trcj(lan,ntr,2) * tem
          endif
        enddo
! 
! 
      enddo   ! lan loop
!
!     lotn=3*levs+1*levh
!
      do lan=1,lats_node_r    !    four_to_grid lan loop
!
         lat = global_lats_r(ipt_lats_node_r-1+lan)
!        lon_dim = lon_dims_r(lan)
         lons_lat = lonsperlar(lat)
!
!
         call grid_to_four(bak_gr_r_2(1,1,lan),bak_gr_r_1(1,1,lan),
     &                     lon_dim_r-2,lon_dim_r,lons_lat,3*levs+1)
!
         if (.not. gg_tracers .or. lsout) then
            call grid_to_four(bak_gr_r_2(1,kar,lan),
     &                        bak_gr_r_1(1,kar,lan),
     &                        lon_dim_r-2,lon_dim_r,lons_lat,levh)
         endif

        if (gg_tracers) then
          if (.not. shuff_lats_r) then
            item = lats_node_a + 1 - lan + yhalo
            do n=1,ntrac
              do k=1,levs
                jtem = levs + 1 - k
                ktem = kar  - 1 + k + (n-1)*levs
                do i=1,min(lonf,lons_lat)
                  rgt_h(xhalo+i,jtem,item,n) = bak_gr_r_2(i,ktem,lan)

                enddo
              enddo
            enddo
          endif ! .not.shuff_lats_r
        endif ! gg_tracers
!
      enddo  !    fin four_to_grid lan loop
!
      if (gg_tracers .and. shuff_lats_r) then
         call mpi_tracers_b_to_a(bak_gr_r_2(1,1,1),
     &                           lats_nodes_r,global_lats_r,
     &                           rgt_h,lats_nodes_a,global_lats_a,kar,0)
       endif ! gg_tracers .and. shuff_lats_r

!!
      call excha(lats_nodes_r,global_lats_r,ptotj,pwatj,ptotg,pwatg
     &,          trcj, trcg)
      sumwa = 0.
      sumto = 0.
      do lat=1,latr
         sumto = sumto + wgt_r(min(lat,latr-lat+1))*ptotg(lat)
         sumwa = sumwa + wgt_r(min(lat,latr-lat+1))*pwatg(lat)
      enddo
      do ntr=1,ntrac
        if (fixtrc(ntr) .and. .not. iauforcing) then
          sumtrc(ntr,1) = 0.0
          sumtrc(ntr,2) = 0.0
          do lat=1,latr
            sumtrc(ntr,1) = sumtrc(ntr,1)
     &                    + wgt_r(min(lat,latr-lat+1))*trcg(lat,ntr,1)
            sumtrc(ntr,2) = sumtrc(ntr,2)
     &                    + wgt_r(min(lat,latr-lat+1))*trcg(lat,ntr,2)
          enddo


!         if (abs(sumtrc(ntr,1)) > 1.0e-12 .and. kdt > 1) then
          if (abs(sumtrc(ntr,1)) > 1.0e-12 .and. sumtrc(ntr,3) > 0) then
!         if (abs(sumtrc(ntr,1)) > 1.0e-12 .and. kdt > 1
!    &                             .and. ntr == 2) then
            tem = (sumtrc(ntr,3)-sumtrc(ntr,1)) / sumtrc(ntr,1)
            if (abs(tem) < 0.1) then
              adjtrc(ntr) = 1 + tem
            else
              adjtrc(ntr) = 1.0
            endif
          else
            adjtrc(ntr) = 1.0
          endif
          sumtrc(ntr,3) = sumtrc(ntr,2)
        endif
      enddo
      if (fixtrc(2) .and. me == 0) 
     &print *,'adjtrc oz',phour*3600,iauforcing,adjtrc(2)
      pdryg = sumto - sumwa
!!
      if(pdryini == 0.) pdryini = pdryg

      if( gen_coord_hybrid ) then                               ! hmhj
        pcorr = (pdryini-pdryg)         * sqrt(2.)              ! hmhj
      else                                                      ! hmhj
        pcorr = (pdryini-pdryg) / sumto * sqrt(2.)
      endif                                                     ! hmhj
!!
      call four2fln_gg(lats_dim_r,lota,3*levs+1,bak_gr_r_1,
     &              ls_nodes,max_ls_nodes,
     &              lats_nodes_r,global_lats_r,lon_dim_r,
     &              lats_node_r,ipt_lats_node_r,
     &              lat1s_r,lonrx,latr,latr2,
     &              trie_ls(1,1,p_ze), trio_ls(1,1,p_ze),
     &              plnew_r, plnow_r,
     &              ls_node,0,
     &              2*levs+1,3*levs+1)

      do i=1,len_trie_ls
        sum_k_rqchange_ls(i,1) = trie_ls(i,1,p_q)
        sum_k_rqchange_ls(i,2) = trie_ls(i,2,p_q)
      enddo
      do i=1,len_trio_ls
        sum_k_rqchango_ls(i,1) = trio_ls(i,1,p_q)
        sum_k_rqchango_ls(i,2) = trio_ls(i,2,p_q)
      enddo

      do i=1,len_trie_ls
        trie_ls(i,1,p_q)       = save_qe_ls(i,1)
        trie_ls(i,2,p_q)       = save_qe_ls(i,2)
      enddo
      do i=1,len_trio_ls
        trio_ls(i,1,p_q)       = save_qo_ls(i,1)
        trio_ls(i,2,p_q)       = save_qo_ls(i,2)
      enddo
!!
      if (.not. gg_tracers .or.lsout ) then
         call four2fln_gg(lats_dim_r,lota,levh,bak_gr_r_1,
     &              ls_nodes,max_ls_nodes,
     &              lats_nodes_r,global_lats_r,lon_dim_r,
     &              lats_node_r,ipt_lats_node_r,
     &              lat1s_r,lonrx,latr,latr2,
     &              trie_ls(1,1,p_rq), trio_ls(1,1,p_rq),
     &              plnew_r, plnow_r,
     &              ls_node,3*levs+1,
     &              1,levh)
      endif
!
!$omp parallel do shared(trie_ls,trio_ls)
!$omp+shared(epse,epso,ls_node)
!$omp+private(k)
      do k=1,levs
         call uveodz(trie_ls(1,1,p_ze +k-1), trio_ls(1,1,p_di +k-1),
     &               trie_ls(1,1,p_uln+k-1), trio_ls(1,1,p_vln+k-1),
     &               epse,epso,ls_node)
!!
         call uvoedz(trio_ls(1,1,p_ze +k-1), trie_ls(1,1,p_di +k-1),
     &               trio_ls(1,1,p_uln+k-1), trie_ls(1,1,p_vln+k-1),
     &               epse,epso,ls_node)
      enddo
!
!.............................................................
!$omp parallel do private(k,i,jtem,ktem)
      do k=1,levs
        ktem = p_w   + k - 1
        jtem = p_vln + k - 1
        do i=1,len_trie_ls
          trie_ls(i,1,ktem) = trie_ls(i,1,ktem) + bfilte(i)*
     &                       (trie_ls(i,1,jtem)-trie_ls(i,1,ktem))

          trie_ls(i,2,ktem) = trie_ls(i,2,ktem) + bfilte(i)*
     &                       (trie_ls(i,2,jtem)-trie_ls(i,2,ktem))
        enddo
        do i=1,len_trio_ls
          trio_ls(i,1,ktem) = trio_ls(i,1,ktem) + bfilto(i)*
     &                       (trio_ls(i,1,jtem)-trio_ls(i,1,ktem))

          trio_ls(i,2,ktem) = trio_ls(i,2,ktem) + bfilto(i)*
     &                       (trio_ls(i,2,jtem)-trio_ls(i,2,ktem))
        enddo
      enddo
cc.............................................................
      if(.not.gg_tracers)then
!$omp parallel do private(k,i,jtem,ktem,tem)
        do k=1,levs
          ktem = p_rt + k - 1
          jtem = p_rq + k - 1
          do i=1,len_trie_ls
            tem = bfilte(i)*(trie_ls(i,1,jtem)-trie_ls(i,1,ktem))
            trie_ls_rqt(i,1,k) = tem
            trie_ls(i,1,ktem)  = trie_ls(i,1,ktem) + tem
!
            tem = bfilte(i)*(trie_ls(i,2,jtem)-trie_ls(i,2,ktem))
            trie_ls_rqt(i,2,k) = tem
            trie_ls(i,2,ktem)  = trie_ls(i,2,ktem) + tem
          enddo
!!
          do i=1,len_trio_ls
            tem = bfilto(i)*(trio_ls(i,1,jtem)-trio_ls(i,1,ktem))
            trio_ls_rqt(i,1,k)   = tem
            trio_ls(i,1,ktem)     = trio_ls(i,1,ktem) + tem
!
            tem = bfilto(i)*(trio_ls(i,2,jtem)-trio_ls(i,2,ktem))
            trio_ls_rqt(i,2,k)    = tem
            trio_ls(i,2,p_rt+k-1) = trio_ls(i,2,ktem) + tem
          enddo
        enddo
!
!!.............................................................
!
        do nt=2,ntrac
!$omp parallel do private(k,i,jtem,ktem)
          do k=levs*(nt-2)+1,levs*(nt-1)
            ktem = p_rt + levs + k - 1
            jtem = p_rq + levs + k - 1
            do i=1,len_trie_ls
              trie_ls(i,1,ktem) = trie_ls(i,1,ktem) + bfilte(i)*
     &                           (trie_ls(i,1,jtem)-trie_ls(i,1,ktem))
              trie_ls(i,2,ktem) = trie_ls(i,2,ktem) + bfilte(i)*
     &                           (trie_ls(i,2,jtem)-trie_ls(i,2,ktem))
            enddo
            do i=1,len_trio_ls
              trio_ls(i,1,ktem) = trio_ls(i,1,ktem) + bfilto(i)*
     &                           (trio_ls(i,1,jtem)-trio_ls(i,1,ktem))
              trio_ls(i,2,ktem) = trio_ls(i,2,ktem) + bfilto(i)*
     &                           (trio_ls(i,2,jtem)-trio_ls(i,2,ktem))
            enddo
          enddo
        enddo
      endif  ! if(.not.gg_tracers)
!!
!----------------------------------------------------------------------
!!
      if(hybrid)then
 
!     get some sigma distribution and compute   typdel from it.
 
        typical_pgr=85.
        do k=1,levp1
          si(levs+2-k) =  ak5(k)/typical_pgr + bk5(k)
        enddo
      endif

      do k=1,levs
        typdel(k)= si(k)-si(k+1)
      enddo
 
!----------------------------------------------------------------------
 
      if (ladj) then
        do i=1,len_trie_ls
          trie_ls(i,1,p_zq) = 0.
          trie_ls(i,2,p_zq) = 0.
        enddo
        do i=1,len_trio_ls
          trio_ls(i,1,p_zq) = 0.
          trio_ls(i,2,p_zq) = 0.
        enddo
        if (me == me_l_0) then
          trie_ls(1,1,p_zq) = pcorr
        endif
!!
        if( gen_coord_hybrid ) then                                       ! hmhj
          trie_ls_sfc = 0.0                                               ! hmhj
          trio_ls_sfc = 0.0                                               ! hmhj
          do k=1,levs                                                     ! hmhj
            do i=1,len_trie_ls                                            ! hmhj
              trie_ls_sfc(i,1) = trie_ls_sfc(i,1)
     &                         + typdel(k)*trie_ls_rqt(i,1,k)             ! hmhj
              trie_ls_sfc(i,2) = trie_ls_sfc(i,2)
     &                         + typdel(k)*trie_ls_rqt(i,2,k)             ! hmhj
            enddo                                                         ! hmhj
            do i=1,len_trio_ls                                            ! hmhj
              trio_ls_sfc(i,1) = trio_ls_sfc(i,1)
     &                         + typdel(k)*trio_ls_rqt(i,1,k)             ! hmhj
              trio_ls_sfc(i,2) = trio_ls_sfc(i,2)
     &                         + typdel(k)*trio_ls_rqt(i,2,k)             ! hmhj
            enddo                                                         ! hmhj
          enddo                                                           ! hmhj

          do i=1,len_trie_ls                                              ! hmhj
            trie_ls(i,1,p_zq) = trie_ls(i,1,p_zq)                         ! hmhj
     &                        + trie_ls(i,1,p_q )*trie_ls_sfc(i,1)        ! hmhj
            trie_ls(i,2,p_zq) = trie_ls(i,2,p_zq)                         ! hmhj
     &                        + trie_ls(i,2,p_q )*trie_ls_sfc(i,2)        ! hmhj
          enddo
          do i=1,len_trio_ls                                              ! hmhj
            trio_ls(i,1,p_zq) = trio_ls(i,1,p_zq)                         ! hmhj
     &                        + trio_ls(i,1,p_q )*trio_ls_sfc(i,1)        ! hmhj
            trio_ls(i,2,p_zq) = trio_ls(i,2,p_zq)                         ! hmhj
     &                        + trio_ls(i,2,p_q )*trio_ls_sfc(i,2)        ! hmhj
          enddo

        else                  ! for hybrid coordinate

          if(gg_tracers)then
            do i=1,len_trie_ls
              trie_ls(i,1,p_zq) = trie_ls(i,1,p_zq)
     &                          + sum_k_rqchange_ls(i,1)
              trie_ls(i,2,p_zq) = trie_ls(i,2,p_zq)
     &                          + sum_k_rqchange_ls(i,2)
            enddo
            do i=1,len_trio_ls
              trio_ls(i,1,p_zq) = trio_ls(i,1,p_zq)
     &                          + sum_k_rqchango_ls(i,1)
              trio_ls(i,2,p_zq) = trio_ls(i,2,p_zq)
     &                          + sum_k_rqchango_ls(i,2)
            enddo
          else
            do k=1,levs
              do i=1,len_trie_ls
                trie_ls(i,1,p_zq) = trie_ls(i,1,p_zq)
     &                            + typdel(k)*trie_ls_rqt(i,1,k)
                trie_ls(i,2,p_zq) = trie_ls(i,2,p_zq)
     &                            + typdel(k)*trie_ls_rqt(i,2,k)
              enddo
              do i=1,len_trio_ls
                trio_ls(i,1,p_zq) = trio_ls(i,1,p_zq)
     &                            + typdel(k)*trio_ls_rqt(i,1,k)
                trio_ls(i,2,p_zq) = trio_ls(i,2,p_zq)
     &                            + typdel(k)*trio_ls_rqt(i,2,k)
              enddo
            enddo
          endif               !fin if(gg_tracers)

        endif                 !fin  if (gen_coord_hybrid)                 ! hmhj
!!
!$omp parallel do private(k,i,item,jtem,ktem,ltem,mtem)
        do k=1,levs
          item = p_di+k-1
          jtem = p_uln+k-1
          ktem = p_x+k-1
          ltem = p_te+k-1
          mtem = p_y+k-1

          do i=1,len_trie_ls
            trie_ls(i,1,item) = bfilte(i)
     &                        * (trie_ls(i,1,jtem)-trie_ls(i,1,ktem))
            trie_ls(i,1,ltem) = bfilte(i)
     &                        * (trie_ls(i,1,ltem)-trie_ls(i,1,mtem))
            trie_ls(i,2,item) = bfilte(i)
     &                        * (trie_ls(i,2,jtem)-trie_ls(i,2,ktem))
            trie_ls(i,2,ltem) = bfilte(i)
     &                        * (trie_ls(i,2,ltem)-trie_ls(i,2,mtem))
          enddo
          do i=1,len_trio_ls
            trio_ls(i,1,item) = bfilto(i)
     &                        * (trio_ls(i,1,jtem)-trio_ls(i,1,ktem))
            trio_ls(i,1,ltem) = bfilto(i)
     &                        * (trio_ls(i,1,ltem)-trio_ls(i,1,mtem))
            trio_ls(i,2,item) = bfilto(i)
     &                        * (trio_ls(i,2,jtem)-trio_ls(i,2,ktem))
            trio_ls(i,2,ltem) = bfilto(i)
     &                        * (trio_ls(i,2,ltem)-trio_ls(i,2,mtem))
          enddo
        enddo
 
!---------------------------------------------------------
        if( gen_coord_hybrid ) then                                       ! hmhj

!$omp parallel do private(locl)
          do locl=1,ls_max_node                                           ! hmhj

            call impadje_hyb_gc(trie_ls(1,1,p_x),trie_ls(1,1,p_y),        ! hmhj
     &                    trie_ls(1,1,p_q),trie_ls(1,1,p_di),             ! hmhj
     &                    trie_ls(1,1,p_te),trie_ls(1,1,p_zq),            ! hmhj
     &                      tstep,                                        ! hmhj
     &                    trie_ls(1,1,p_uln),trie_ls(1,1,p_vln),          ! hmhj
     &                    snnp1ev,ndexev,ls_node,locl)                    ! hmhj
!!
            call impadjo_hyb_gc(trio_ls(1,1,p_x),trio_ls(1,1,p_y),        ! hmhj
     &                    trio_ls(1,1,p_q),trio_ls(1,1,p_di),             ! hmhj
     &                    trio_ls(1,1,p_te),trio_ls(1,1,p_zq),            ! hmhj
     &                      tstep,                                        ! hmhj
     &                    trio_ls(1,1,p_uln),trio_ls(1,1,p_vln),          ! hmhj
     &                    snnp1od,ndexod,ls_node,locl)                    ! hmhj
          enddo                                                           ! hmhj
        elseif(hybrid) then           ! for sigma/p hybrid coordinate    ! hmhj
          if (.not. semilag) then     ! for eulerian hybrid case

 
!$omp parallel do private(locl)
            do locl=1,ls_max_node
              call impadje_hyb(trie_ls(1,1,p_x),trie_ls(1,1,p_y),
     &             trie_ls(1,1,p_q),trie_ls(1,1,p_di),
     &             trie_ls(1,1,p_te),trie_ls(1,1,p_zq),
     &                      tstep,
     &             trie_ls(1,1,p_uln),trie_ls(1,1,p_vln),
     &             snnp1ev,ndexev,ls_node,locl)
!!
              call impadjo_hyb(trio_ls(1,1,p_x),trio_ls(1,1,p_y),
     &             trio_ls(1,1,p_q),trio_ls(1,1,p_di),
     &             trio_ls(1,1,p_te),trio_ls(1,1,p_zq),
     &                      tstep,
     &             trio_ls(1,1,p_uln),trio_ls(1,1,p_vln),
     &             snnp1od,ndexod,ls_node,locl)
            enddo
          else        ! for semi-lagrangian hybrid case
!$omp parallel do private(locl)
            do locl=1,ls_max_node


              call impadje_slg(trie_ls(1,1,p_x),trie_ls(1,1,p_y),
     &             trie_ls(1,1,p_q),trie_ls(1,1,p_di),
     &             trie_ls(1,1,p_te),trie_ls(1,1,p_zq),
     &                      tstep,
     &             trie_ls(1,1,p_uln),trie_ls(1,1,p_vln),
     &             snnp1ev,ndexev,ls_node,locl,batah)
!!
              call impadjo_slg(trio_ls(1,1,p_x),trio_ls(1,1,p_y),
     &             trio_ls(1,1,p_q),trio_ls(1,1,p_di),
     &             trio_ls(1,1,p_te),trio_ls(1,1,p_zq),
     &                      tstep,
     &             trio_ls(1,1,p_uln),trio_ls(1,1,p_vln),
     &             snnp1od,ndexod,ls_node,locl,batah)
            enddo
          endif
 
 
        endif  ! fin massadj
!---------------------------------------------------------
 
      else  ! fin massadj,    following is with no masadj
        do k=1,levs
          del(k) = typdel(k)                 ! sela 4.5.07
        enddo
        if (me == me_l_0) then
          trie_ls(1,1,p_q) = trie_ls(1,1,p_q) + pcorr
        endif
!
! testing mass correction on sep 25
!!
        if(gg_tracers)then
          do i=1,len_trie_ls
            trie_ls(i,1,p_q) = trie_ls(i,1,p_q) + sum_k_rqchange_ls(i,1)
            trie_ls(i,2,p_q) = trie_ls(i,2,p_q) + sum_k_rqchange_ls(i,2)
          enddo
          do i=1,len_trio_ls
            trio_ls(i,1,p_q) = trio_ls(i,1,p_q) + sum_k_rqchango_ls(i,1)
            trio_ls(i,2,p_q) = trio_ls(i,2,p_q) + sum_k_rqchango_ls(i,2)
          enddo
        else
          do k=1,levs
            do i=1,len_trie_ls
             trie_ls(i,1,p_q)=trie_ls(i,1,p_q)+del(k)*trie_ls_rqt(i,1,k)
             trie_ls(i,2,p_q)=trie_ls(i,2,p_q)+del(k)*trie_ls_rqt(i,2,k)
            enddo
            do i=1,len_trio_ls
             trio_ls(i,1,p_q)=trio_ls(i,1,p_q)+del(k)*trio_ls_rqt(i,1,k)
             trio_ls(i,2,p_q)=trio_ls(i,2,p_q)+del(k)*trio_ls_rqt(i,2,k)
            enddo
          enddo
        endif
!
! testing mass correction on sep 25
!
!$omp parallel do private(k,i,item,jtem,ktem,ltem,mtem)
        do k=1,levs
          item = p_di+k-1
          jtem = p_uln+k-1
          ktem = p_x+k-1
          ltem = p_te+k-1
          mtem = p_y+k-1
          do i=1,len_trie_ls
            trie_ls(i,1,ktem) = trie_ls(i,1,ktem) + bfilte(i)
     &                        *(trie_ls(i,1,jtem)-trie_ls(i,1,ktem))
            trie_ls(i,2,ktem) = trie_ls(i,2,ktem) + bfilte(i)
     &                        *(trie_ls(i,2,jtem)-trie_ls(i,2,ktem))
            trie_ls(i,1,mtem) = trie_ls(i,1,mtem) + bfilte(i)
     &                        *(trie_ls(i,1,ltem)-trie_ls(i,1,mtem))
            trie_ls(i,2,mtem) = trie_ls(i,2,mtem) + bfilte(i)
     &                        *(trie_ls(i,2,ltem)-trie_ls(i,2,mtem))
          enddo

          do i=1,len_trio_ls
            trio_ls(i,1,ktem) = trio_ls(i,1,ktem)+bfilto(i)
     &                        *(trio_ls(i,1,jtem)-trio_ls(i,1,ktem))
            trio_ls(i,2,ktem) = trio_ls(i,2,ktem) + bfilto(i)
     &                        *(trio_ls(i,2,jtem)-trio_ls(i,2,ktem))
            trio_ls(i,1,mtem) = trio_ls(i,1,mtem) + bfilto(i)
     &                        *(trio_ls(i,1,ltem)-trio_ls(i,1,mtem))
            trio_ls(i,2,mtem) = trio_ls(i,2,mtem) + bfilto(i)
     &                        *(trio_ls(i,2,ltem)-trio_ls(i,2,mtem))
          enddo
        enddo
      endif   ! fin no ladj (i.e. no massadj)

! deallocate arrays used for stochastic physics.
!-----------------------------------------------
      if (allocated(sppt_wt))    deallocate(sppt_wt)
      if (allocated(shum_wt))    deallocate(shum_wt)
      if (allocated(skebu_wt))   deallocate(skebu_wt,skebv_wt)
      if (allocated(vcu_wt))     deallocate(vcu_wt,vcv_wt)

!-------------------------------------------------------------------------
! add iau increment to updated spectral fields.
! p_q is updated lnps, p_w is updated vort,
! p_x is updated div, p_rt are updated tracers,
! p_y is updated (virt) temp

      if (iau) then
         if (semilag) then
            dtiau = tstep
         else
            dtiau = 2.*tstep
         endif
         if (me == 0) print *,'call getiauforcing',phour*3600.,dtiau
         call getiauforcing(
     &         trie_ls(:,:,p_w:p_w+levs-1),trie_ls(:,:,p_x:p_x+levs-1),
     &         trie_ls(:,:,p_y:p_y+levs-1),
     &         trie_ls(:,:,p_rt:p_rt+ntrac*levs-1),
     &         trie_ls(:,:,p_q),
     &         trio_ls(:,:,p_w:p_w+levs-1),trio_ls(:,:,p_x:p_x+levs-1),
     &         trio_ls(:,:,p_y:p_y+levs-1),
     &         trio_ls(:,:,p_rt:p_rt+ntrac*levs-1),
     &         trio_ls(:,:,p_q),dtiau,phour*3600.)
         if (me == 0) 
     &   print *,'after call getiauforcing',phour*3600,iau,adjtrc(2)
      endif                                           !  end of iau step
                                                      !-----------------
!!
      return
      end
