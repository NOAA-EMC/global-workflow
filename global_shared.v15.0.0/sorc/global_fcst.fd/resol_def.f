      module resol_def
      use machine
      implicit none
      save
      integer   jcap,jcap1,jcap2,latg,latg2,latr,latr2
      integer   levh,levm1,levp1,levs,lnt,lnt2,lnt22,levr
      integer   lnte,lnted,lnto,lntod,lnuv
      integer   lonf,lonfx,lonr,lonrx
      integer   ntrac
!     integer   nxpt,nypt,jintmx,latgd
      integer                    latgd
      integer   ntoz,ntcw
      integer   lsoil,nmtvr,ncld,nrcm
      integer   num_p3d,num_p2d,num_a3d,num_a2d   
      integer   ngrids_sfcc, ngrids_flx, nfxr, ngrids_gg
      integer   ngrids_nst,nr_nst,nf_nst
      integer   ivsupa, ivssfc, ivssfc_restart, ivsinp
      integer   ivsnst
      integer   nlunit
      integer   thermodyn_id, sfcpress_id			! hmhj
      integer   idvt
!
      integer   p_gz,p_zem,p_dim,p_tem,p_rm,p_qm
      integer   p_ze,p_di,p_te,p_rq,p_q,p_dlam,p_dphi,p_uln,p_vln
      integer   p_w,p_x,p_y,p_rt,p_zq
      integer   lots,lotd,lota

      integer kwq,kwte,kwdz,kwrq

!     for ensemble concurrency run. weiyu
!     integer :: ensemble_id, total_member

      end module resol_def
!
      module ozne_def
      use machine , only : kind_phys
      implicit none
      save
      integer, parameter :: kozpl=28, kozc=48
      integer latsozp, levozp, timeoz, latsozc, levozc, timeozc
     &,       pl_coeff
      real (kind=kind_phys) blatc, dphiozc
      real (kind=kind_phys), allocatable :: pl_lat(:), pl_pres(:)
     &,                                     pl_time(:)
      end module ozne_def
