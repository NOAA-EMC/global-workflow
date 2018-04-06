      module stochy_resol_def

! program log:
! 20110220:   Henry Juang update index for MASS_DP and NDSLFV
! 20130202:   Henry Juang revise reduced grid and add x number
!
      implicit none
      
      integer   jcap,jcap1,jcap2,latg,latg2
      integer   levh,levm1,levp1,skeblevs,levs,lnt,lnt2,lnt22,levr
      integer   lnte,lnted,lnto,lntod,lnuv
      integer   lonf,lonfx,num_p2d,num_p3d
      integer   nxpt,nypt,jintmx,latgd
      integer   ntoz,ntcw,ncld,ntke,ixgr,ntiw,ntlnc,ntinc,nto,nto2
      integer   ivsupa, ivsinp
      integer   nlunit, kdt_start
      integer,target ::   ntrac
      integer,target ::   ngrids_gg
      integer,target ::   thermodyn_id, sfcpress_id             ! hmhj
      logical,target ::   adiabatic
!
      INTEGER   p_gz,p_lapgz,p_zslam,p_zsphi,p_dlam,p_dphi,p_uln,p_vln
      INTEGER   p_zem,p_dim,p_tem,p_rm,p_dpm,p_qm
      INTEGER   p_ze ,p_di ,p_te ,p_rq,p_dp ,p_q
      INTEGER   p_w  ,p_x  ,p_y  ,p_rt,p_dpn,p_zq
      INTEGER   p_zz ,p_dpphi,p_dplam,p_zzphi,p_zzlam
      INTEGER   g_uum,g_vvm,g_ttm,g_rm ,g_dpm,g_qm,g_gz,g_zz
      INTEGER   g_uu ,g_vv ,g_tt ,g_rq ,g_dp ,g_q
      INTEGER   g_uup,g_vvp,g_ttp,g_rqp,g_dpp,g_zqp,g_rqtk
      INTEGER   g_u  ,g_v  ,g_t  ,g_rt ,g_dpn,g_zq, g_p, g_dpdt
      INTEGER   lots,lots_slg,lotd,lota,lotp,lotls,lotgr,lotgr6

      integer   ksz, ksd, kst, ksr, ksdp, ksq, ksplam, kspphi
      integer   ksu, ksv, kzslam, kzsphi
!
      integer   kau, kav, kat, kar, kadp, kaps, kazs, kap2
!
      integer   kdpphi, kzzphi, kdplam, kzzlam
!
      integer   kdtphi, kdrphi, kdtlam, kdrlam 
      integer   kdulam, kdvlam, kduphi, kdvphi


      end module stochy_resol_def
