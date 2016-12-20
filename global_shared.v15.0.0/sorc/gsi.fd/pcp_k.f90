subroutine pcp_k(km,dtp,del_in,sl_in,rbs,&
     slmask,xkt2,ncloud,frain,rmmhr,&
     psexp,dplat,dplon,&

     t_in,q_in,u_in,v_in,div_in,cwm_in,&
     tsen_ten_in,q_ten_in,p_ten_in,&
     tsas_o,qsas_o,rnsas_of,cldwrk,kbcon,ktcon,jmin,kuo,&
     tlrg_o,qlrg_o,rnlrg_of,&
     t_out,q_out,cwm_out,u_out,v_out,rn_out,&
     t_in_ad,q_in_ad,cwm_in_ad,u_in_ad,v_in_ad,div_in_ad,&
     t_out_ad,q_out_ad,cwm_out_ad,u_out_ad,v_out_ad,rn_out_ad)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    pcp_k      driver for precipitation forward & adjoint models
!     prgmmr:  treadon       org: np23                date: 2003-12-18
!
! abstract: This subroutine calls GFS precipitation physics routines followed
!           by a call to adjoint of these routines.   The GFS precipitation
!           physics include both convective and grid-scale (explicit) processes.
!           Convective precipitation is parameterized using the Simplified 
!           Arakawa-Schubert scheme originally developed by George Grell.  
!           Explicit precipitation processes are modeled folliwng the work of
!           Q. Zhao.
!
! program history log:
!   2003-12-18  treadon - initial routine
!   2004-06-14  treadon - reformat documenation
!   2006-04-12  treadon - change del and sl from 1d to 2d arrays
!   2006-09-15  treadon - change (k,i) arrays to (k) arrays
!   2006-10-12  treadon - remove virtual temperature
!   2008-04-29  safford - rm unused uses
!   2010-03-31  treadon - replace jcap with sp_a%jcap
!   2013-01-15  parrish - convert gscond_ad.f90,nlmsas_ad.f90,omegas_ad.f90 to modules
!                           and add interfaces to account for type mismatch
!
!   input argument list:
!     km        - number of levels in vertical profile
!     dtp       - physics timestep
!     del       - "sigma" thickness of layers
!     sl        - "sigma" value at layer midpoints
!     rbs       - 1/(sin(latitude)**2)
!     slmask    - sea (=0), land (=1), ice/snow (=2) mask
!     xkt2      - random number for cloud top selection in SAS
!     ncloud    - flag to turn on cloud liquid water detrainment in SAS
!     frain     - factor to account for difference between physics and model timestep
!     rmmhr     - conversion factor to get rain rate units of mm/hr
!     psexp     - surface pressure (cb)
!     dplat     - partial derivative of ln(ps) with respect to latitude
!     dplon     - partial derivative of ln(ps) with respect to longitude
!     del_in    -
!     sl_in     -
!     t_in      - sensible temperature
!     q_in      - specific humidity
!     u_in      - zonal wind component
!     v_in      - meridional wind component
!     div_in    - divergence
!     cwm_in    - cloud condensate mixing ratio
!     t_out_ad  - temperature perturbation
!     q_out_ad  - q perturbation
!     cwm_out_ad- cloud condensate mixing ratio perturbation
!     u_out_ad  - zonal wind perturbation
!     v_out_ad  - meridional wind perturbation
!     rn_out_ad - rain rate perturbation
!     tsen_ten_in
!     q_ten_in
!     p_ten_in
!
!   output argument list:
!     tsas_o    - temperature following call to SAS
!     qsas_o    - q following call to SAS
!     rnsas_of  - convective precipitation rate
!     cldwrk    - cloud work function computed/used by SAS
!     kbcon     - integer index of model level at which SAS updraft originates
!     ktcon     - integer index of model level for SAS cloud top
!     jmin      - integer index of model level at which SAS downdraft originates
!     kuo       - integer flag for convective activity (0=no convection, 1=convection)
!     tlrg_o    - temperature following call to explicit precipitation routines
!     qlrg_o    - q following call to explicit precipitation routines
!     rnlrg_of  - precipitation rate following call to explicit precipitation routines
!     t_out     - temperature following call to all GFS precipitation physics
!     q_out     - q following call to all GFS precipitation physics
!     cwm_out   - cloud condensate mixing ratio following call to all GFS precipitation physics
!     u_out     - zonal wind component following call to all GFS precipitation physics
!     v_out     - meridional wind component following call to all GFS precipitation physics
!     rn_out    - precipitation rate following call to all GFS precipitation physics
!     t_in_ad   - partial derivative of temperature with respect to rain rate
!     q_in_ad   - partial derivative of q with respect to rain rate
!     cwm_in_ad - partial derivative of cloud condensate mixing ratio with respect to rain rate
!     u_in_ad   - partial derivative of zonal wind with respect to rain rate
!     v_in_ad   - partial derivative of meridional  with respect to rain rate
!     div_in_ad - partial derivative of divergence  with respect to rain rate
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!    
  use kinds, only: r_kind,i_kind
  use constants, only: rhcbot,rhctop,dx_inv,dx_min,one,zero
  use pcpinfo, only: tiny_obs
  use gridmod, only: nlon,sp_a
  use gscond_ad_mod, only: gscond_ad
  use nlmsas_ad_mod, only: nlmsas_ad
  use omegas_ad_mod, only: omegas_ad
  implicit none


! Declare passed variables
  integer(i_kind)           , intent(in   ) :: km,ncloud
  integer(i_kind)           , intent(  out) :: kbcon,jmin,ktcon,kuo

  real(r_kind)              , intent(in   ) :: dtp,frain,rmmhr
  real(r_kind)              , intent(in   ) :: rbs,dplat,dplon,slmask,psexp
  real(r_kind)              , intent(  out) :: cldwrk,rn_out

  real(r_kind),dimension(km), intent(in   ) :: del_in,sl_in
  real(r_kind),dimension(km), intent(in   ) :: t_in,q_in,cwm_in,u_in,v_in,div_in,&
       t_out_ad,q_out_ad,cwm_out_ad,u_out_ad,v_out_ad,&
       tsen_ten_in,q_ten_in,p_ten_in
  real(r_kind),dimension(km), intent(  out) :: t_out,q_out,cwm_out,u_out,v_out,&
       t_in_ad,q_in_ad,u_in_ad,v_in_ad,div_in_ad,cwm_in_ad

! Declare local parameters
  real(r_kind),parameter:: r0_99=0.99_r_kind

! Declare local arrays
  logical:: skipsas,skiplrg
  logical adjoint

  integer(i_kind):: k,kb,im,ix
  real(r_kind):: ps,rcl,rcs,xkt,xkt2,rnlrg_o,&
       rnlrg_o_ad,rnsas_o,rnsas_o_ad,rnsas_of,rnlrg_of,rnsas_of_ad,&
       rnlrg_of_ad,rn_out_ad,work2,tem,work1
  
  real(r_kind),dimension(km):: rhc,u_i,v_i,div_i,vvel_o,&
       u_i_ad,v_i_ad,div_i_ad,vvel_o_ad,&
       t_ten_i,q_ten_i,p_ten_i,qgs_i,cwmgs_i,tgs_i,&
       qgs_o,cwmgs_o,tgs_o, qlrg_i, cwmlrg_i, tlrg_i,&
       qlrg_o, cwmlrg_o, tlrg_o,t_ten_i_ad,q_ten_i_ad,p_ten_i_ad,&
       qgs_i_ad,cwmgs_i_ad,tgs_i_ad,qgs_o_ad,cwmgs_o_ad,tgs_o_ad,&
       qlrg_i_ad, cwmlrg_i_ad, tlrg_i_ad,&
       qlrg_o_ad, cwmlrg_o_ad, tlrg_o_ad,&
       tsas_i,qsas_i,cwmsas_i,usas_i,vsas_i,wsas_i,&
       tsas_o,qsas_o,cwmsas_o,usas_o,vsas_o,&
       tsas_i_ad,qsas_i_ad,cwmsas_i_ad,usas_i_ad,vsas_i_ad,wsas_i_ad,&
       tsas_o_ad,qsas_o_ad,cwmsas_o_ad,usas_o_ad,vsas_o_ad,&
       save_tlrg,save_qlrg,save_cwmlrg,&
       save_tsas,save_qsas,save_cwmsas,save_usas,save_vsas,&
       save_wsas,del_i,sl_i


!**************************************************************************
! Initialize output arrays to zero
  im=1
  ix=1
  cldwrk = zero
  kb     = 0
  jmin   = 0
  kbcon  = 0
  ktcon  = 0
  kuo    = 0
  rn_out = zero
  do k = 1,km
     t_out(k)   = zero
     q_out(k)   = zero
     cwm_out(k) = zero
     u_out(k)   = zero
     v_out(k)   = zero
     t_in_ad(k) = zero
     q_in_ad(k) = zero
     u_in_ad(k) = zero
     v_in_ad(k) = zero
     div_in_ad(k)= zero
     cwm_in_ad(k)= zero
  end do


! SASCNV needs vertical velocity, so compute it.
  rcl = rbs
  ps  = psexp
  do k = 1,km
     u_i(k)      = u_in(k)
     v_i(k)      = v_in(k)
     div_i(k)    = div_in(k)
     del_i(k)    = del_in(k)
     sl_i(k)     = sl_in(k)
     vvel_o(k)   = zero
     u_i_ad(k)   = zero
     v_i_ad(k)   = zero
     div_i_ad(k) = zero
     vvel_o_ad(k)= zero
  end do
  adjoint = .false.
  call omegas_ad(im,ix,km,dplat,dplon,u_i,v_i,div_i,ps,rcl,&
       del_i,sl_i,vvel_o,u_i_ad,v_i_ad,div_i_ad,vvel_o_ad,adjoint)


! Call convective parameterization, SASCNV.
  rnsas_o = zero
  rnlrg_o = zero
  rcs     = sqrt(rcl)
  xkt     = xkt2
  rnsas_o_ad = zero
  do k = 1,km
     usas_i(k)    = u_in(k)
     vsas_i(k)    = v_in(k)
     wsas_i(k)    = vvel_o(k)
     tsas_i(k)    = t_in(k)
     qsas_i(k)    = q_in(k)
     cwmsas_i(k)  = cwm_in(k)
     del_i(k)     = del_in(k)
     sl_i(k)      = sl_in(k)
     
     usas_o(k)    = zero
     vsas_o(k)    = zero
     tsas_o(k)    = zero
     qsas_o(k)    = zero
     cwmsas_o(k)  = zero
     
     usas_o_ad(k)   = zero
     vsas_o_ad(k)   = zero
     tsas_o_ad(k)   = zero
     qsas_o_ad(k)   = zero
     cwmsas_o_ad(k) = zero
     
     tsas_i_ad(k)   = zero
     qsas_i_ad(k)   = zero
     cwmsas_i_ad(k) = zero
     usas_i_ad(k)   = zero
     vsas_i_ad(k)   = zero
     wsas_i_ad(k)   = zero
  end do

  adjoint = .false.
  call nlmsas_ad(im,ix,km,sp_a%jcap,dtp,del_i,sl_i,rcs,&
       slmask,xkt,ncloud,psexp,&
       
       tsas_i,qsas_i,cwmsas_i,usas_i,vsas_i,wsas_i,&
       tsas_o,qsas_o,cwmsas_o,usas_o,vsas_o,rnsas_o,&
       cldwrk,kbcon,ktcon,jmin,kuo,kb,&
       
       tsas_i_ad,qsas_i_ad,cwmsas_i_ad,usas_i_ad,vsas_i_ad,wsas_i_ad,&
       tsas_o_ad,qsas_o_ad,cwmsas_o_ad,usas_o_ad,vsas_o_ad,rnsas_o_ad,&
       adjoint)
  

! Transfer u and v to output arrays
  do k=1,km
     u_out(k) = usas_o(k)
     v_out(k) = vsas_o(k)
  end do


! Compute critical threshold relative humidities
  tem = (rhctop-rhcbot)/(km-one)
  work1 = (log(one/(rcs*nlon))-dx_min) * dx_inv
  work2 = one - work1
  do k=1,km
     rhc(k) = rhcbot + tem*(k-1)
     rhc(k) = r0_99*work1 + rhc(k)*work2
  end do


! Call gridscale condensation routine, GSCOND
  do k = 1,km
     t_ten_i(k)  = tsen_ten_in(k)
     q_ten_i(k)  = q_ten_in(k)
     p_ten_i(k)  = p_ten_in(k)
     sl_i(k)     = sl_in(k)
     tgs_i(k)    = tsas_o(k)
     qgs_i(k)    = qsas_o(k)
     cwmgs_i(k)  = cwmsas_o(k)
     tgs_o(k)    = zero
     qgs_o(k)    = zero
     cwmgs_o(k)  = zero
     t_ten_i_ad(k)   = zero
     q_ten_i_ad(k)   = zero
     p_ten_i_ad(k)   = zero
     tgs_i_ad(k)    = zero
     qgs_i_ad(k)    = zero
     cwmgs_i_ad(k)  = zero
     tgs_o_ad(k)    = zero
     qgs_o_ad(k)    = zero
     cwmgs_o_ad(k)  = zero
  end do
  adjoint = .false.
  call gscond_ad(im,ix,km,dtp,sl_i,psexp,rhc,&
       t_ten_i,q_ten_i,p_ten_i,&
       qgs_i,cwmgs_i,tgs_i,&
       qgs_o,cwmgs_o,tgs_o,&
       t_ten_i_ad,q_ten_i_ad,p_ten_i_ad,&
       qgs_i_ad,cwmgs_i_ad,tgs_i_ad,&
       qgs_o_ad,cwmgs_o_ad,tgs_o_ad,&
       adjoint)


! Call gridscale precipitation routine, PRECPD
  rnlrg_o    = zero
  rnlrg_o_ad = zero
  do k = 1,km
     tlrg_i(k)       = tgs_o(k)
     qlrg_i(k)       = qgs_o(k)
     cwmlrg_i(k)     = cwmgs_o(k)
     del_i(k)        = del_in(k)
     sl_i(k)         = sl_in(k)
     tlrg_o(k)       = zero
     qlrg_o(k)       = zero
     cwmlrg_o(k)     = zero
     tlrg_i_ad(k)    = zero
     qlrg_i_ad(k)    = zero
     cwmlrg_i_ad(k)  = zero
     tlrg_o_ad(k)    = zero
     qlrg_o_ad(k)    = zero
     cwmlrg_o_ad(k)  = zero
  end do
  adjoint = .false.
  call precpd_ad(km,dtp,del_i,sl_i,psexp,rhc,&
       qlrg_i, cwmlrg_i, tlrg_i,&
       qlrg_o, cwmlrg_o, tlrg_o,rnlrg_o,&
       qlrg_i_ad, cwmlrg_i_ad, tlrg_i_ad,&
       qlrg_o_ad, cwmlrg_o_ad, tlrg_o_ad,rnlrg_o_ad,&
       adjoint)


! Combine convective and gridscale precipitation to get the total
  rnsas_of = frain*rnsas_o*rmmhr
  rnlrg_of = frain*rnlrg_o*rmmhr
  rn_out   = rnsas_of + rnlrg_of
  skipsas  = .false.
  skiplrg  = .false.


! If convective or gridscale precipitation are too small (ie, < tiny_obs),
! then reset precipitation contribution to zero
  if (rnsas_of<tiny_obs) then
     skipsas  = .true.
     rnsas_of = zero
  endif
  if (rnlrg_of<tiny_obs) then
     skiplrg  = .true.
     rnlrg_of = zero
  endif
  rn_out = rnsas_of + rnlrg_of


! Load output arrays with adjusted temperature and
! moisture profiles.  Convert dry temperature to
! virtual temperature.
  do k = 1,km
     cwm_out(k) = cwmlrg_o(k)
     q_out(k)   = qlrg_o(k)
     t_out(k)   = tlrg_o(k)
  end do


!==================================================================
! BEGIN ADJOINT  
!
! Adjoint of combine convective and gridscale precipitation
! to get the total.
  rnsas_of_ad = rn_out_ad
  rnlrg_of_ad = rn_out_ad
  
  if (skipsas) rnsas_of_ad = zero
  if (skiplrg) rnlrg_of_ad = zero
  
  rnsas_o_ad  = frain*rnsas_of_ad*rmmhr
  rnlrg_o_ad  = frain*rnlrg_of_ad*rmmhr
  

! Call adjoint of gridscale precipitation routine, PRECPD
  rnlrg_o    = zero

  do k = 1,km
     tlrg_i(k)       = tgs_o(k)
     qlrg_i(k)       = qgs_o(k)
     cwmlrg_i(k)     = cwmgs_o(k)
     del_i(k)        = del_in(k)
     sl_i(k)         = sl_in(k)
     tlrg_o(k)       = zero
     qlrg_o(k)       = zero
     cwmlrg_o(k)     = zero
     tlrg_i_ad(k)    = zero
     qlrg_i_ad(k)    = zero
     cwmlrg_i_ad(k)  = zero
     tlrg_o_ad(k)    = t_out_ad(k)
     qlrg_o_ad(k)    = q_out_ad(k)
     cwmlrg_o_ad(k)  = cwm_out_ad(k)
     save_tlrg(k)    = tlrg_o_ad(k)
     save_qlrg(k)    = qlrg_o_ad(k)
     save_cwmlrg(k)  = cwmlrg_o_ad(k)
  end do
  adjoint = .true.
  call precpd_ad(km,dtp,del_i,sl_i,psexp,rhc,&
       qlrg_i, cwmlrg_i, tlrg_i,&
       qlrg_o, cwmlrg_o, tlrg_o,rnlrg_o,&
       qlrg_i_ad, cwmlrg_i_ad, tlrg_i_ad,&
       qlrg_o_ad, cwmlrg_o_ad, tlrg_o_ad,rnlrg_o_ad,&
       adjoint)


! If gridscale precipitation < tiny_obs, do not include forcing from 
! gridscale precipitation in adjoint (ie, gradient).
  if (skiplrg) then
     do k=1,km
        tlrg_i_ad(k)   = save_tlrg(k)
        qlrg_i_ad(k)   = save_qlrg(k)
        cwmlrg_i_ad(k) = save_cwmlrg(k)
     end do
  endif


! Adjoint of call to GSCOND
  do k = 1,km
     t_ten_i(k)  = tsen_ten_in(k)
     q_ten_i(k)  = q_ten_in(k)
     p_ten_i(k)  = p_ten_in(k)
     sl_i(k)     = sl_in(k)
     tgs_i(k)    = tsas_o(k)
     qgs_i(k)    = qsas_o(k)
     cwmgs_i(k)  = cwmsas_o(k)
     
     tgs_o(k)    = zero
     qgs_o(k)    = zero
     cwmgs_o(k)  = zero
     
     t_ten_i_ad(k)   = zero
     q_ten_i_ad(k)   = zero
     p_ten_i_ad(k)   = zero
     tgs_i_ad(k)    = zero
     qgs_i_ad(k)    = zero
     cwmgs_i_ad(k)  = zero
     
     tgs_o_ad(k)    = tlrg_i_ad(k)
     qgs_o_ad(k)    = qlrg_i_ad(k)
     cwmgs_o_ad(k)  = cwmlrg_i_ad(k)
  end do
  adjoint = .true.
  call gscond_ad(im,ix,km,dtp,sl_i,psexp,rhc,&
       t_ten_i,q_ten_i,p_ten_i,&
       qgs_i,cwmgs_i,tgs_i,&
       qgs_o,cwmgs_o,tgs_o,&
       t_ten_i_ad,q_ten_i_ad,p_ten_i_ad,&
       qgs_i_ad,cwmgs_i_ad,tgs_i_ad,&
       qgs_o_ad,cwmgs_o_ad,tgs_o_ad,&
       adjoint)


! Adjoint of u and v transfers
  do k=1,km
     usas_o_ad(k) = u_out_ad(k)
     vsas_o_ad(k) = v_out_ad(k)
  end do


! Adjoint of call to convective parameterization, SASCNV
  rnsas_o = zero
  xkt     = xkt2
  do k = 1,km
     usas_i(k)    = u_in(k)
     vsas_i(k)    = v_in(k)
     wsas_i(k)    = vvel_o(k)
     tsas_i(k)    = t_in(k)
     qsas_i(k)    = q_in(k)
     cwmsas_i(k)  = cwm_in(k)
     del_i(k)     = del_in(k)
     sl_i(k)      = sl_in(k)
     
     usas_o(k)    = zero
     vsas_o(k)    = zero
     tsas_o(k)    = zero
     qsas_o(k)    = zero
     cwmsas_o(k)  = zero
     
     tsas_o_ad(k)   = tgs_i_ad(k)
     qsas_o_ad(k)   = qgs_i_ad(k)
     cwmsas_o_ad(k) = cwmgs_i_ad(k)
     
     tsas_i_ad(k)   = zero
     qsas_i_ad(k)   = zero
     cwmsas_i_ad(k) = zero
     usas_i_ad(k)   = zero
     vsas_i_ad(k)   = zero
     wsas_i_ad(k)   = zero
     
     save_usas(k)   = usas_o_ad(k)
     save_vsas(k)   = vsas_o_ad(k)
     save_wsas(k)   = zero
     save_qsas(k)   = qgs_i_ad(k)
     save_tsas(k)   = tgs_i_ad(k)
     save_cwmsas(k) = cwmgs_i_ad(k)
     
  end do
  adjoint = .true.
  call nlmsas_ad(im,ix,km,sp_a%jcap,dtp,del_i,sl_i,rcs,&
       slmask,xkt,ncloud,psexp,&
       
       tsas_i,qsas_i,cwmsas_i,usas_i,vsas_i,wsas_i,&
       tsas_o,qsas_o,cwmsas_o,usas_o,vsas_o,rnsas_o,&
       cldwrk,kbcon,ktcon,jmin,kuo,kb,&
       
       tsas_i_ad,qsas_i_ad,cwmsas_i_ad,usas_i_ad,vsas_i_ad,wsas_i_ad,&
       tsas_o_ad,qsas_o_ad,cwmsas_o_ad,usas_o_ad,vsas_o_ad,rnsas_o_ad,&
       adjoint)


! If convective precipitation < tiny_obs, do not include forcing from
! convective precipitation in adjoint (ie, gradient).
  if (skipsas) then
     do k=1,km
        usas_i_ad(k)   = save_usas(k)
        vsas_i_ad(k)   = save_vsas(k)
        wsas_i_ad(k)   = save_wsas(k)
        qsas_i_ad(k)   = save_qsas(k)
        tsas_i_ad(k)   = save_tsas(k)
        cwmsas_i_ad(k) = save_cwmsas(k)
     end do
  endif


! Adjoint of "0" --> "sas0" transfer
  do k = 1,km
     u_in_ad(k)   = usas_i_ad(k)
     v_in_ad(k)   = vsas_i_ad(k)
     vvel_o_ad(k) = wsas_i_ad(k)
     q_in_ad(k)   = qsas_i_ad(k)
     t_in_ad(k)   = tsas_i_ad(k)
     cwm_in_ad(k) = cwmsas_i_ad(k)
  end do


! Adjoint of call to omegas
  rcl = rbs
  ps  = psexp
  do k = 1,km
     u_i(k)      = u_in(k)
     v_i(k)      = v_in(k)
     div_i(k)    = div_in(k)
     del_i(k)    = del_in(k)
     sl_i(k)     = sl_in(k)
     vvel_o(k)   = zero
!    u_i_ad(k)   = u_in_ad(k)
!    v_i_ad(k)   = v_in_ad(k)
     u_i_ad(k)   = zero
     v_i_ad(k)   = zero
     div_i_ad(k) = zero
  end do
  adjoint = .true.
  call omegas_ad(im,ix,km,dplat,dplon,u_i,v_i,div_i,ps,rcl,&
       del_i,sl_i,vvel_o,u_i_ad,v_i_ad,div_i_ad,vvel_o_ad,adjoint)


! Adjoint of _i --> _in transfer
  do k = 1,km
     u_in_ad(k)   = u_in_ad(k) + u_i_ad(k)
     v_in_ad(k)   = v_in_ad(k) + v_i_ad(k)
     div_in_ad(k) = div_i_ad(k)
  end do


! End of routine
  return
end subroutine pcp_k

