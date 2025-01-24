!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !MODULE: m_berror_stats_reg - a module of berror_stats input
!
! !DESCRIPTION:
!
! !INTERFACE:

    module m_berror_stats_reg
      use kinds,only : i_kind,r_kind
      use constants, only: zero,one,max_varname_length,half
      use gridmod, only: nsig
      use chemmod, only : berror_chem,berror_fv3_cmaq_regional,berror_fv3_sd_regional,upper2lower,lower2upper
      use m_berror_stats, only: usenewgfsberror,berror_stats

      implicit none

      private   ! except

        ! reconfigurable parameters, via NAMELIST/setup/
      public :: berror_stats    ! reconfigurable filename

        ! interfaces to file berror_stats.
      public :: berror_set_reg          ! set internal parameters
      public :: berror_get_dims_reg     ! get dimensions, jfunc::createj_func()
      public :: berror_read_bal_reg     ! get cross-cov.stats., balmod::prebal()
      public :: berror_read_wgt_reg     ! get auto-cov.stats., prewgt()

! !REVISION HISTORY:
!       25Feb10 - Zhu - adopt code format from m_berror_stats
!                     - extract from rdgstat_reg
!                     - change sructure of error file
!                     - make changes for generalized control variables
!       01Oct15   Zhu - use centralized cloud_names_fwd and n_clouds_fwd to add 
!                       flexibility for clouds (either cw or individual hydrometeros) 
!                       variances/correlations for all-sky radiance assimilation  
!       24Jun16 - Guo - replaced the local berror_stats, with a global user
!                       configurable m_berror_stats::berror_stats, for the
!                       filename.  This ensure the consistency, as well as the
!                       reconfigurability of this variable through the GSI.
!       20Apr22 - x.zhang - add variable dlsig_glb for calculating the delta
!                       sigma from the global 127-L BE,which is used to
!                       convert the grid unit of vertical length scale (vz) from
!                       1/layer to the unit of 1/sigma.   
!       2022-05-24 ESRL(H.Wang) - Add B for reginal FV3-CMAQ (berror_fv3_cmaq_regional=.true.) . 
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_berror_stats_reg'

        ! The same default filename is used as in m_berror_stats,  to take the
        ! advantage of the same berror_stats= entry in the /setup/ namelist, to
        ! override the default value.  Otherwise, a special entry would have to
        ! be defined for this module in the /setup/ namelist.

  integer(i_kind),parameter :: default_unit_ = 22
  integer(i_kind),parameter :: ERRCODE=2
  logical,save:: cwcoveqqcov_

  integer(i_kind),allocatable,dimension(:):: lsig
  real(r_kind),allocatable,dimension(:):: coef1,coef2
  real(r_kind),allocatable,dimension(:):: dlsig_glb

contains
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: berror_get_dims_reg - get dimensions
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine berror_get_dims_reg(msig,mlat,unit)

      implicit none

      integer(i_kind)         ,intent(  out) :: msig  ! dimension of levels
      integer(i_kind)         ,intent(  out) :: mlat  ! dimension of latitudes
      integer(i_kind),optional,intent(in   ) :: unit  ! logical unit [22]

! !REVISION HISTORY:
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::berror_get_dims_reg'

  integer(i_kind) :: inerr

! Read dimension of stats file
  inerr=default_unit_
  if(present(unit)) inerr = unit
  open(inerr,file=berror_stats,form='unformatted',status='old')
  rewind inerr
  read(inerr) msig,mlat
  close(inerr)
end subroutine berror_get_dims_reg
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: berror_set_reg - set (logical) parameter options internal to B
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine berror_set_reg(opt,value)

      implicit none
      character(len=*),intent(in) :: opt
      logical(i_kind), intent(in) :: value

! !REVISION HISTORY:
!      2014-10-15 - Zhu - adopted from m_berror_stat to make code structure similar
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::berror_set_reg'
  logical found
  found=.false.
  if(trim(opt)=='cwcoveqqcov') then
     cwcoveqqcov_=value
     found=.true.
  endif
  if(.not.found) then
     write(6,*) myname_,'(PREBAL_reg):  ***ERROR*** cannot find:', trim(opt)
     call stop2(999)
  endif

end subroutine berror_set_reg
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: berror_read_bal_reg - get cross-corr. coefficients
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine berror_read_bal_reg(msig,mlat,agvi,bvi,wgvi,mype,unit)
      use kinds,only : r_single
      use guess_grids, only:  ges_psfcavg,ges_prslavg

      implicit none

      integer(i_kind)                              ,intent(in   ) :: msig,mlat
      real(r_kind),dimension(0:mlat+1,nsig,nsig),intent(  out) :: agvi
      real(r_kind),dimension(0:mlat+1,nsig)     ,intent(  out) :: wgvi
      real(r_kind),dimension(0:mlat+1,nsig)     ,intent(  out) :: bvi
      integer(i_kind)                              ,intent(in   ) :: mype  ! "my" processor ID
      integer(i_kind),optional                     ,intent(in   ) :: unit ! an alternative unit

! !REVISION HISTORY:
!       25Feb10 - Zhu  - extracted from rdgstat_reg
!                      - change the structure of error file
!                      - make changes for generalized control variables
!       20Apr22 - x.zhang - add the code to calculate the delta sigma from the
!                           global 127-L BE  
!EOP ___________________________________________________________________

    character(len=*),parameter :: myname_=myname//'::berror_read_bal_reg'

!     workspaces/variables for data not returned

    integer(i_kind) k,i,m,j,m1,l1,l
    integer(i_kind):: nsigstat,nlatstat
    integer(i_kind):: inerr

    real(r_kind),dimension(nsig) :: rlsig
    real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
    real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn
    real(r_single),dimension(:,:,:),allocatable:: agv_avn
    real(r_kind),dimension(:),allocatable::  rlsigo

!   Open background error statistics file
    inerr=default_unit_
    if(present(unit)) inerr=unit
    open(inerr,file=berror_stats,form='unformatted',status='old')

!   Read header. 
    rewind inerr
    read(inerr) nsigstat,nlatstat

    if(mype==0) then
       write(6,*) myname_,'(PREBAL_REG):  get balance variables', &
         '"',trim(berror_stats),'".  ', &
         'mype,nsigstat,nlatstat =', &
          mype,nsigstat,nlatstat
    end if

    allocate ( clat_avn(mlat) )
    allocate ( sigma_avn(1:msig) )
    allocate ( rlsigo(1:msig) )
    if(usenewgfsberror)then
      allocate ( agv_avn(mlat,1:msig,1:msig) )
      allocate ( bv_avn(mlat,1:msig),wgv_avn(mlat,1:msig) )
    else
      allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
      allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )
    end if

!   Read background error file to get balance variables
    read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
          

    read(inerr)agv_avn,bv_avn,wgv_avn
    close(inerr)

!   compute vertical(pressure) interpolation index and weight
    do k=1,nsig
       rlsig(k)=log(ges_prslavg(k)/ges_psfcavg)
    enddo
    do k=1,msig
       rlsigo(k)=log(sigma_avn(k))
    enddo

!   compute delta sigma of global level
    if(usenewgfsberror)then
      allocate(dlsig_glb(msig))
      dlsig_glb(1)=log(sigma_avn(1))-log(sigma_avn(2))
      do k=2,msig-1
         dlsig_glb(k)=half*(log(sigma_avn(k-1))-log(sigma_avn(k+1)))
      enddo
      dlsig_glb(msig)=log(sigma_avn(msig-1))-log(sigma_avn(msig))
    end if
!

    allocate(lsig(nsig),coef1(nsig),coef2(nsig))
    do k=1,nsig
       if(rlsig(k)>=rlsigo(1))then
          m=1
          m1=2
          lsig(k)=1
          coef1(k)=one
          coef2(k)=zero
       else if(rlsig(k)<=rlsigo(msig))then
          m=msig-1
          m1=msig
          lsig(k)=msig-1
          coef1(k)=zero
          coef2(k)=one
       else
          m_loop: do m=1,msig-1
             m1=m+1
             if((rlsig(k)<=rlsigo(m))   .and.  &
                  (rlsig(k)>rlsigo(m1))     )then
                lsig(k)=m
                exit m_loop
             end if
          enddo m_loop
          coef1(k)=(rlsigo(m1)-rlsig(k))/(rlsigo(m1)-rlsigo(m))
          coef2(k)=one-coef1(k)
          if(lsig(k)==msig)then
             lsig(k)=msig-1
             coef2(k)=one
             coef1(k)=zero
          endif
       endif
    end do

!   Load agv wgv bv
    do k=1,nsig
       m=lsig(k)
       m1=m+1
       do i=1,mlat
          wgvi(i,k)=wgv_avn(i,m)*coef1(k)+wgv_avn(i,m1)*coef2(k)
          bvi (i,k)=bv_avn (i,m)*coef1(k)+bv_avn (i,m1)*coef2(k)
       enddo

       do j=1,nsig
          l=lsig(j)
          l1=l+1
          do i=1,mlat
             agvi(i,j,k)=(agv_avn(i,l,m)*coef1(j)+agv_avn(i,l1,m)*coef2(j))*coef1(k) &
                      +(agv_avn(i,l,m1)*coef1(j)+agv_avn(i,l1,m1)*coef2(j))*coef2(k)
          enddo
       enddo
    enddo

    deallocate (agv_avn,bv_avn,wgv_avn,clat_avn,sigma_avn,rlsigo)

    agvi(0,:,:)=agvi(1,:,:)
    wgvi(0,:)=wgvi(1,:)
    bvi(0,:)=bvi(1,:)
    agvi(mlat+1,:,:)=agvi(mlat,:,:)
    wgvi(mlat+1,:)=wgvi(mlat,:)
    bvi(mlat+1,:)=bvi(mlat,:)
     
    return
end subroutine berror_read_bal_reg
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! NASA/GSFC, Global Modeling and Assimilation Office, 900.3, GEOS/DAS  !
!BOP -------------------------------------------------------------------
!
! !IROUTINE: berror_read_wgt_reg - read auto-corr. coeffs.
!
! !DESCRIPTION:
!
! !INTERFACE:

    subroutine berror_read_wgt_reg(msig,mlat,corz,corp,hwll,hwllp,vz,rlsig,varq,qoption,varcw,cwoption,mype,unit)

      use kinds,only : r_single,r_kind
      use gridmod,only : nsig, twodvar_regional
      use gsi_io, only : verbose
      use control_vectors,only: nrf,nc2d,nc3d,mvars,nvars
      use control_vectors,only: cvars => nrf_var
      use control_vectors,only: cvars2d,cvars3d,cvarsmd
      use guess_grids, only:  ges_psfcavg,ges_prslavg
      use constants, only: zero,one,ten,three
      use mpeu_util,only: getindex
      use radiance_mod, only: icloud_cv,n_clouds_fwd,cloud_names_fwd
      use chemmod, only: berror_fv3_cmaq_regional,berror_fv3_sd_regional
      use rapidrefresh_cldsurf_mod, only: corp_gust, hwllp_gust, l_rtma3d

      implicit none

      integer(i_kind)                    ,intent(in   ) :: qoption
      integer(i_kind)                    ,intent(in   ) :: cwoption
      integer(i_kind)                    ,intent(in   ) :: msig,mlat
      integer(i_kind)                    ,intent(in   ) :: mype  ! "my" processor ID
      integer(i_kind),optional           ,intent(in   ) :: unit ! an alternative unit

      real(r_kind),dimension(:,:,:),intent(inout):: corz
      real(r_kind),dimension(:,:)  ,intent(inout)  :: corp

      real(r_kind),dimension(0:mlat+1,1:nsig,1:nc3d), intent(inout):: hwll
      real(r_kind),dimension(0:mlat+1,nvars-nc3d)   , intent(inout):: hwllp
      real(r_kind),dimension(nsig,0:mlat+1,1:nc3d),intent(inout):: vz
      real(r_kind),dimension(mlat,nsig),intent(inout)::varq
      real(r_kind),dimension(mlat,nsig),intent(inout)::varcw

      real(r_kind),dimension(nsig),intent(out):: rlsig

! !REVISION HISTORY:
!       25Feb10 - Zhu - extract from rdgstat_reg
!                     - change the structure of background error file
!                     - make changes for generalizing control variables
!                     - move varq,factoz here from prewgt_reg
!       28May10 Todling Obtain variable id's on the fly (add getindex)
!       01Jun10 Todling These are now alloctable: corz,corp,hwll,hwllp,vz
!       22Jun10 Treadon - move nrf3_loc and nrf2_loc allocate outside read loop
!       23Jun10 Treadon - explicitly specify dimensions for hwll,hwllp,vz
!       20Nov10 Pagowski - make var name longer for chemical berror and
!                          related change in read
!       16Feb11 Zhu - add gust,vis,pblh
!       15Dec12 Zhu - add varcw and cwoption for all-sky radiance assimiation
!       03Feb14 Todling - varq & qoption in arg list (remove dep on jfunc)
!       19Mar14 pondeca - add wspd10m
!       10Apr14 pondeca - add td2m,mxtm,mitm,pmsl
!       07May14 pondeca - add howv
!       10Jun14 Zhu - tune error variance and correlation lengths of cw for
!                     all-sky radiance assimilation
!       19Jun14 carley/zhu - add tcamt and lcbas
!       10Jul15 pondeca - add cldch
!       01Oct15 Zhu - use centralized cloud_names_fwd and n_clouds_fwd to add 
!                     flexibility for clouds (either cw or individual hydrometeros) 
!                     variances/correlations for all-sky radiance assimilation
!       05May16 pondeca - add uwnd10m, vuwn10m
!       29Aug16 stelios -  Update the constants for howv #ww3
!       2016-09-xx CAPS(G. Zhao) 
!                       - add same error stats as q for hydrometeor variables,
!                       - which can be tuned in sub prewgt_reg.f90.
!                       - extra caution required for using logarithmic transformation
!       2018-10-22 CAPS(C.Liu) - add w
!       20Apr22 x.zhang - Add the code to convert the unit of global 127-L BE
!                         vertical length scale from 1/layer to 1/sigma
!       2022-05-24 ESRL(H.Wang) - Add B for reginal FV3-CMAQ
!                        (berror_fv3_cmaq_regional=.true.) . 
!
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::berror_read_wgt_reg'

  real(r_kind),parameter:: r25      = one/25.0_r_kind
  real(r_kind),parameter:: zero_3   = 0.3_r_kind
  real(r_kind),parameter:: vz_oz    = 0.53333333_r_kind

!  workspace variables not returned
  real(r_single),dimension(:,:),allocatable::  corqq_avn
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn

  real(r_single),dimension(1:mlat,msig,nrf):: corz_tmp
  real(r_single),dimension(0:mlat+1,msig,nrf):: hwll_tmp
  real(r_single),dimension(msig,0:mlat+1,nrf):: vz_tmp


  character*5 :: varshort
! varlong is for regional FV3-CMAQ model 
! the B file should include stats of both meterological  and aerosol control
! variables that are listed in anavinfo (control_vector section). 
  character*10 :: varlong

  character(len=max_varname_length) :: var
  logical,dimension(nrf):: nrf_err

  integer(i_kind) :: nrf3_oz,nrf3_q,nrf3_cw,nrf3_sf,nrf3_vp,nrf2_sst
  integer(i_kind) :: nrf3_t,nrf2_gust,nrf2_vis,nrf2_pblh,nrf2_ps,nrf2_wspd10m
  integer(i_kind) :: nrf2_td2m,nrf2_mxtm,nrf2_mitm,nrf2_pmsl,nrf2_howv,nrf2_tcamt,nrf2_lcbas,nrf2_cldch
  integer(i_kind) :: nrf2_uwnd10m,nrf2_vwnd10m
  integer(i_kind) :: nrf3_sfwter,nrf3_vpwter
  integer(i_kind) :: nrf3_dbz,nrf3_fed
  integer(i_kind) :: nrf3_ql,nrf3_qi,nrf3_qr,nrf3_qs,nrf3_qg,nrf3_qnr,nrf3_w
  integer(i_kind) :: inerr,istat
  integer(i_kind) :: nsigstat,nlatstat,isig
  integer(i_kind) :: loc,m1,m,i,n,j,k,ivar,ic
  integer(i_kind),allocatable,dimension(:) :: nrf2_loc,nrf3_loc,nmotl_loc
  real(r_kind) :: factoz
  real(r_kind) :: raux
  real(r_kind),dimension(nsig):: dlsig

  ! corz = sqrt(corz)
  real(r_kind), parameter :: corz_default=one,hwll_default=100000_r_kind,vz_default=one
  !real(r_kind), parameter :: corz_default=one,hwll_default=27000.00000000,vz_default=one*10
  logical :: print_verbose
  real(r_kind) ,dimension(mlat,1,2) :: cov_dum
!  character(256) :: filename 
!  filename = 'howv_var_berr.bin'

  print_verbose=.false.
  if(verbose)print_verbose=.true.

  do k=1,nsig
     rlsig(k)=log(ges_prslavg(k)/ges_psfcavg)
  enddo

! Open background error statistics file
  inerr=default_unit_
  if(present(unit)) inerr=unit
  open(inerr,file=berror_stats,form='unformatted',status='old')

! Read header.
  rewind inerr
  read(inerr) nsigstat,nlatstat


  if(mype==0) then
     write(6,*) myname_,'(PREWGT_REG):  read error amplitudes ', &
       '"',trim(berror_stats),'".  ', &
       'mype,nsigstat,nlatstat =', &
        mype,nsigstat,nlatstat
  end if

! Read background error file to get past alance variables
  read(inerr)
  read(inerr)

  allocate(nrf3_loc(nc3d),nrf2_loc(nc2d),nmotl_loc(mvars))
  do n=1,nc3d
     nrf3_loc(n)=getindex(cvars,cvars3d(n))
  enddo
  do n=1,nc2d
     nrf2_loc(n)=getindex(cvars,cvars2d(n))
  enddo
  do n=1,mvars
     nmotl_loc(n)=getindex(cvars,cvarsmd(n))
  enddo


! Read amplitudes
  nrf_err=.false.
  read: do
     if (berror_chem) then
        read(inerr,iostat=istat) varshort,isig
        var=upper2lower(varshort)
        if (trim(var) == 'pm25') var = 'pm2_5'
     else 
        if ( berror_fv3_cmaq_regional .or. berror_fv3_sd_regional) then
          read(inerr,iostat=istat) varlong, isig
          var=varlong
        else
          read(inerr,iostat=istat) varshort, isig
          var=varshort
        endif
     endif
     if (istat /= 0) exit read
     do n=1,nrf
        if (trim(var)==cvars(n)) then
           nrf_err(n)=.true.
           loc=n
           exit
        else
           loc=-999
        end if
     end do
     if(loc == -999)then
        if(mype == 0)write(6,*) 'variable in input file, but not used in analysis ',var,isig
        read(inerr)
        read(inerr)
        if(isig > 1)read(inerr)
        cycle read
     end if

     allocate ( corz_avn(1:mlat,1:isig) )

     if (trim(var)/='q' .or. (trim(var)=='cw' .and. cwoption==2)) then
        read(inerr) corz_avn
     else
        allocate ( corqq_avn(1:mlat,1:isig) )
        read(inerr) corz_avn,corqq_avn
     end if

     if(usenewgfsberror)then
       allocate ( hwll_avn(mlat,1:isig) )
     else
       allocate ( hwll_avn(0:mlat+1,1:isig) )
     end if
     read(inerr) hwll_avn


     if (isig==msig) then
        if(usenewgfsberror)then
          allocate ( vztdq_avn(mlat,1:isig) )
        else
          allocate ( vztdq_avn(1:isig,0:mlat+1) )
        end if
        read(inerr) vztdq_avn
        do n=1,nc3d
           if (nrf3_loc(n)==loc) then
              if ((trim(var)=='q' .and. qoption==2) .or. (trim(var)=='cw' .and. cwoption==2)) then
!                choose which q stat to use
                 do k=1,msig
                    do i=1,mlat
                       corz_tmp(i,k,n)=corqq_avn(i,k)
                    end do
                 end do
              else
                 do k=1,msig
                    do i=1,mlat
                       corz_tmp(i,k,n)=corz_avn(i,k)
                    end do
                 end do
              end if
              do k=1,msig
                 if(usenewgfsberror)then
                    do i=1,mlat
                       hwll_tmp(i,k,n)=hwll_avn(i,k)

!                      convert global BE vz from the unit of 1/layer to 1/sigma
                       vz_tmp(k,i,n)=vztdq_avn(i,k)/dlsig_glb(k)
                    end do
                    hwll_tmp(0,k,n)=hwll_avn(1,k)
                    hwll_tmp(mlat+1,k,n)=hwll_avn(mlat,k)

!                   convert global BE vz from the unit of 1/layer to 1/sigma
                    vz_tmp(k,0,n)=vztdq_avn(1,k)/dlsig_glb(k)
                    vz_tmp(k,mlat+1,n)=vztdq_avn(mlat,k)/dlsig_glb(k)
                 else
                    do i=0,mlat+1
                       hwll_tmp(i,k,n)=hwll_avn(i,k)
                       vz_tmp(k,i,n)=vztdq_avn(k,i)
                    end do
                 end if
              end do
              exit
           end if
        end do
        deallocate ( vztdq_avn )

     else if(isig == 1)then
       do n=1,nc2d
          if (nrf2_loc(n)==loc) then
             do i=1,mlat
                 corp(i,n)=corz_avn(i,1)
                 hwllp(i,n)=hwll_avn(i,1)
             end do
             if(usenewgfsberror)then
                hwllp(0,n)=hwll_avn(1,1)
                hwllp(mlat+1,n)=hwll_avn(mlat,1)
             else
                hwllp(0,n)=hwll_avn(0,1)
                hwllp(mlat+1,n)=hwll_avn(mlat+1,1)
             end if
             exit
          end if
       end do
     end if
     deallocate ( corz_avn )
     deallocate ( hwll_avn )
     if(allocated(corqq_avn)) deallocate ( corqq_avn )
  enddo read
  close(inerr)

! 3d variable
  do n=1,nc3d
     loc=nrf3_loc(n)
     if (loc <= 0)cycle
     if (nrf_err(loc)) then
        do k=1,nsig
           m=lsig(k)
           m1=m+1
           do i=1,mlat
              corz(i,k,n)=corz_tmp(i,m,n)*coef1(k)+corz_tmp(i,m1,n)*coef2(k)
           enddo

           do i=0,mlat+1
              hwll(i,k,n)=hwll_tmp(i,m,n)*coef1(k)+hwll_tmp(i,m1,n)*coef2(k)
              vz(k,i,n)=vz_tmp(m,i,n)*coef1(k)+vz_tmp(m1,i,n)*coef2(k)
           enddo
        enddo
     else
        do k=1,nsig
           do i=1,mlat
              corz(i,k,n)=corz_default
           enddo

           do i=0,mlat+1
              hwll(i,k,n)=hwll_default
              vz(k,i,n)=vz_default
           enddo
        enddo
        if(mype==0) then
           write(6,*)'Assigned default statistics to variable ',cvars(loc)
        endif
     end if
  enddo

! Get control variable indexes
  nrf3_t  =getindex(cvars3d,'t')
  nrf3_q  =getindex(cvars3d,'q')
  nrf3_oz =getindex(cvars3d,'oz')
  nrf3_cw =getindex(cvars3d,'cw')
  nrf3_sf =getindex(cvars3d,'sf')
  nrf3_vp =getindex(cvars3d,'vp')
  nrf3_dbz=getindex(cvars3d,'dbz')
  nrf3_fed=getindex(cvars3d,'fed')
  nrf2_sst=getindex(cvars2d,'sst')
  nrf2_gust=getindex(cvars2d,'gust')
  nrf2_vis=getindex(cvars2d,'vis')
  nrf2_pblh=getindex(cvars2d,'pblh')
  nrf2_ps=getindex(cvars2d,'ps')
  nrf2_wspd10m=getindex(cvars2d,'wspd10m')
  nrf2_td2m=getindex(cvars2d,'td2m')
  nrf2_mxtm=getindex(cvars2d,'mxtm')
  nrf2_mitm=getindex(cvars2d,'mitm')
  nrf2_pmsl=getindex(cvars2d,'pmsl')
  nrf2_howv=getindex(cvars2d,'howv')
  nrf3_sfwter =getindex(cvars3d,'sfwter')
  nrf3_vpwter =getindex(cvars3d,'vpwter')
  nrf2_tcamt=getindex(cvars2d,'tcamt')
  nrf2_lcbas=getindex(cvars2d,'lcbas')
  nrf2_cldch=getindex(cvars2d,'cldch')
  nrf2_uwnd10m=getindex(cvars2d,'uwnd10m')
  nrf2_vwnd10m=getindex(cvars2d,'vwnd10m')

! W, Cloud and hydrometeor fields
  nrf3_ql  =getindex(cvars3d,'ql')
  nrf3_qi  =getindex(cvars3d,'qi')
  nrf3_qr  =getindex(cvars3d,'qr')
  nrf3_qs  =getindex(cvars3d,'qs')
  nrf3_qg  =getindex(cvars3d,'qg')
  nrf3_qnr =getindex(cvars3d,'qnr')
  nrf3_w   =getindex(cvars3d,'w')

  if(nrf3_q>0 .and. qoption==2)then
     do k=1,nsig
        do j=1,mlat
           varq(j,k)=min(max(real(corz(j,k,nrf3_q),r_kind),0.0015_r_kind),one)
           corz(j,k,nrf3_q)=one
        enddo
     enddo
  endif

  if( nrf3_dbz>0 )then
    if(.not. nrf3_t>0) then
      write(6,*)'not as expect,stop'
      stop
    endif
    corz(:,:,nrf3_dbz)=10.0_r_kind
    hwll(:,:,nrf3_dbz)=hwll(:,:,nrf3_t)
    vz(:,:,nrf3_dbz)=vz(:,:,nrf3_t)
  endif

  if( nrf3_fed>0 )then
    if(.not. nrf3_t>0) then
      write(6,*)'not as expect,stop'
      stop
    endif
    corz(:,:,nrf3_fed)=10.0_r_kind
    hwll(:,:,nrf3_fed)=hwll(:,:,nrf3_t)
    vz(:,:,nrf3_fed)=vz(:,:,nrf3_t)
  endif

  if (nrf3_oz>0) then 
     factoz = 0.0002_r_kind*r25
     corz(:,:,nrf3_oz)=factoz
!    hwll:,:,nrf3_oz)=400000.0_r_kind
     vz(:,:,nrf3_oz)=vz_oz
  end if

  if(nrf3_cw > 0)then
     if (cwcoveqqcov_ ) then
        corz(:,:,nrf3_cw)=corz(:,:,nrf3_q)
        hwll(:,:,nrf3_cw)=hwll(:,:,nrf3_q)
        vz(:,:,nrf3_cw)=vz(:,:,nrf3_q)

     else
        corz(:,:,nrf3_cw)=zero
        if (cwoption==2) then
           do k=1,nsig
              if (ges_prslavg(k)>15.0_r_kind) then
                 do j=1,mlat
                    varcw(j,k)=max(real(corz(j,k,nrf3_cw),r_kind),zero)
                    corz(j,k,nrf3_cw)=one
                 enddo
              end if
           enddo
        end if

        if (cwoption==1 .or. cwoption==3) then
           do k=1,nsig
              if (ges_prslavg(k)>15.0_r_kind) then
                 do j=1,mlat
                    corz(j,k,nrf3_cw)=one
                 end do
              end if
           end do
           hwll(:,:,nrf3_cw)=0.5_r_kind*hwll(:,:,nrf3_q)
           vz(:,:,nrf3_cw)=0.5_r_kind*vz(:,:,nrf3_q)
        end if
     end if
  else if (icloud_cv .and. n_clouds_fwd>0 .and. cwoption==3) then
     do n=1,size(cvars3d)
        do ic=1,n_clouds_fwd
           if(trim(cvars3d(n))==trim(cloud_names_fwd(ic))) then
              ivar=n
              do k=1,nsig
                 do i=1,mlat
                    corz(i,k,ivar)=one
                 end do
              end do
              hwll(:,:,ivar)=0.5_r_kind*hwll(:,:,nrf3_q)
              vz  (:,:,ivar)=0.5_r_kind*vz  (:,:,nrf3_q)
              exit
           end if
        end do
     end do
  end if ! n_clouds_fwd>0 .and. nrf3_cw<=0 .and. cwoption==3


  if (nrf3_sfwter>0) then
      if(mype==0) write(6,*)'Replace default with appropriate statistics for variable sfwter'
      corz(1:mlat,1:nsig,nrf3_sfwter)=corz(1:mlat,1:nsig,nrf3_sf)
      hwll(0:mlat+1,1:nsig,nrf3_sfwter)=hwll(0:mlat+1,1:nsig,nrf3_sf)
  endif

  if (nrf3_vpwter>0) then
      if(mype==0) write(6,*)'Replace default with appropriate statistics for variable vpwter'
      corz(1:mlat,1:nsig,nrf3_vpwter)=corz(1:mlat,1:nsig,nrf3_vp)
      hwll(0:mlat+1,1:nsig,nrf3_vpwter)=hwll(0:mlat+1,1:nsig,nrf3_vp)
  endif

! W and Cloud/hydrometeor variables share same error structure with humidity q by default
!  (not with log transformed cloud variables)
  if (nrf3_ql>0) then
      if(mype==0) &
          write(6,*)myname_,"@pe=",mype," set b_error stats of ql = qv(moisture) and nrf3_ql=",nrf3_ql
      corz(:,:,nrf3_ql) = corz(:,:,nrf3_q)
      hwll(:,:,nrf3_ql) = hwll(:,:,nrf3_q)
      vz(:,:,  nrf3_ql) = vz(:,:,  nrf3_q)
  end if

  if (nrf3_qi>0) then
      if(mype==0) &
          write(6,*)myname_,"@pe=",mype," set b_error stats of qi = qv(moisture) and nrf3_qi=",nrf3_qi
      corz(:,:,nrf3_qi) = corz(:,:,nrf3_q)
      hwll(:,:,nrf3_qi) = hwll(:,:,nrf3_q)
      vz(:,:,  nrf3_qi) = vz(:,:,  nrf3_q)
  end if

  if (nrf3_qr>0) then
      if(mype==0) &
          write(6,*)myname_,"@pe=",mype," set b_error stats of qr = qv(moisture) and nrf3_qr=",nrf3_qr
      corz(:,:,nrf3_qr) = corz(:,:,nrf3_q)
      hwll(:,:,nrf3_qr) = hwll(:,:,nrf3_q)
      vz(:,:,  nrf3_qr) = vz(:,:,  nrf3_q)
  end if

  if (nrf3_qs>0) then
      if(mype==0) &
          write(6,*)myname_,"@pe=",mype," set b_error stats of qs = qv(moisture) and nrf3_qs=",nrf3_qs
      corz(:,:,nrf3_qs) = corz(:,:,nrf3_q)
      hwll(:,:,nrf3_qs) = hwll(:,:,nrf3_q)
      vz(:,:,  nrf3_qs) = vz(:,:,  nrf3_q)
  end if

  if (nrf3_qg>0) then
      if(mype==0) &
          write(6,*)myname_,"@pe=",mype," set b_error stats of qg = qv(moisture) and nrf3_qg=",nrf3_qg
      corz(:,:,nrf3_qg) = corz(:,:,nrf3_q)
      hwll(:,:,nrf3_qg) = hwll(:,:,nrf3_q)
      vz(:,:,  nrf3_qg) = vz(:,:,  nrf3_q)
  end if

  if (nrf3_qnr>0) then
      if(mype==0) &
          write(6,*)myname_,"@pe=",mype," set b_error stats of qnr = qv(moisture) and nrf3_nr=",nrf3_qnr
      corz(:,:,nrf3_qnr) = corz(:,:,nrf3_q)
      hwll(:,:,nrf3_qnr) = hwll(:,:,nrf3_q)
      vz(:,:,  nrf3_qnr) = vz(:,:,  nrf3_q)
  end if

  if (nrf3_w>0) then
      if(mype==0) &
          write(6,*)myname_,"@pe=",mype," set b_error stats of w = qv(moisture) and nrf3_w=",nrf3_w
      corz(:,:,nrf3_w) = corz(:,:,nrf3_q)
      hwll(:,:,nrf3_w) = hwll(:,:,nrf3_q)
      vz(:,:,  nrf3_w) = vz(:,:,  nrf3_q)
  end if

! 2d variable
  do n=1,nc2d
     loc=nrf2_loc(n)
     if(loc <= 0)cycle
!  If we want to use the sst in global file uncomment the following if statement
     if (n==nrf2_sst) then
!       if(.not. usenewgfsberror)then
          do i=1,mlat
             corp(i,n)=zero_3
          end do
          do i=0,mlat+1
             hwllp(i,n)=hwll(i,1,nrf3_sf)
          end do
!       end if
     else if (n==nrf2_gust) then
        do i=1,mlat
           corp(i,n)=three    ! background error stddev of wind gust = 3 m/s (default: legacy code from 2DRTMA) 
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_q)  ! de-correlation length of bkgd error of gust is
                                        ! same as the value of q at bottom level (default: legacy code from 2DRTMA)
                                        ! for other DA apps, it is recommended to change it 
                                        ! by setting hwllp_gust in GSI namelist.
        end do
        if ( l_rtma3d ) then  ! For 3drtma only: allowing to change the stddev and 
                              ! de-correlation length of bkgd error of gust:
                              !   corp_gust : set in namelist(if <=0, using default value above (3.0)
                              !   hwllp_gust: set in namelist(if <=0, using default value above (value of q)
           if ( corp_gust  .gt. 0.0_r_kind ) then
              corp(1:mlat,   n) = corp_gust
              if (mype==0) write(6,'(1x,A,A,I5.5,A,F8.3)') &
                           myname_,"@pe=",mype," (3drtma) set b_error stddev of gust = ",corp_gust
           else
              if (mype==0) write(6,'(1x,A,A,I5.5,A,F8.3)') &
                           myname_,"@pe=",mype," (3drtma) set b_error stddev of gust (default) = ",three
           end if
           if ( hwllp_gust .gt. 0.0_r_kind ) then
              hwllp(0:mlat+1,n) = hwllp_gust
              if (mype==0) write(6,'(1x,A,A,I5.5,A,F12.3)') &
                           myname_,"@pe=",mype," (3drtma) set b_error de-corr length of gust = ",hwllp_gust
           else
              if (mype==0) write(6,'(1x,A,A,I5.5,A)') &
                           myname_,"@pe=",mype," (3drtma) set b_error de-corr length of gust is same as length of q."
           end if
        end if
     else if (n==nrf2_vis) then
        do i=1,mlat
           corp(i,n)=3.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
     else if (n==nrf2_pblh) then
        do i=1,mlat
           corp(i,n)=500.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=three*hwll(i,1,nrf3_t)
        end do
     else if (n==nrf2_wspd10m) then
        do i=1,mlat
           corp(i,n)=three
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_q)
        end do
     else if (n==nrf2_td2m) then
        raux=maxval(corz(1:mlat,1,nrf3_q))
        do i=1,mlat
           corp(i,n)=(corz(i,1,nrf3_q)/raux)*three !tentatively
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_q) !tentatively
        end do
     else if (n==nrf2_mxtm) then
        do i=1,mlat
           corp(i,n)=corz(i,1,nrf3_t)
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
     else if (n==nrf2_mitm) then
        do i=1,mlat
           corp(i,n)=corz(i,1,nrf3_t)
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
     else if (n==nrf2_pmsl) then
        do i=1,mlat
           corp(i,n)=corp(i,nrf2_ps)
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwllp(i,nrf2_ps)
        end do
     else if (n==nrf2_howv) then
         call read_howv_stats(mlat,1,2,cov_dum,mype)
         do i=1,mlat
            corp(i,n)=cov_dum(i,1,1)     !#ww3
            hwllp(i,n) = cov_dum(i,1,2) 
         end do
         hwllp(0,n) = hwllp(1,n)
         hwllp(mlat+1,n) = hwllp(mlat,n)
         if (mype==0) then
            print*, myname_, ' static BE corp( :,n) (for ', trim(adjustl(cvars2d(n))), ')= ', corp(:,n)
            print*, myname_, ' static BE hwllp(:,n) (for ', trim(adjustl(cvars2d(n))), ')= ', hwllp(:,n)
         end if
!         corp(:,n)=cov_dum(:,1)
        !do i=1,mlat
        !   corp(i,n)=0.4_r_kind     !#ww3
        !end do
!        do i=0,mlat+1
!           hwllp(i,n)=hwll(i,1,nrf3_sf) !tentatively !#ww3  hwllp(i,n)=150000_r_kind  !
!        end do
     else if (n==nrf2_tcamt) then
        do i=1,mlat
           corp(i,n)=50.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
     else if (n==nrf2_lcbas) then
        do i=1,mlat
           corp(i,n)=40000.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
        if(print_verbose)print*, 'm_berror_reg: maxhwllp_lcbas=',maxval(hwllp(:,n))
     else if (n==nrf2_cldch) then
        do i=1,mlat
           corp(i,n)=3.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
        if(print_verbose)print*, 'm_berror_reg: maxhwllp_cldch=',maxval(hwllp(:,n))
     else if (n==nrf2_uwnd10m) then
        do i=1,mlat
           corp(i,n)=three
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_q)
        end do
     else if (n==nrf2_vwnd10m) then
        do i=1,mlat
           corp(i,n)=three
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_q)
        end do
     end if

  enddo


! motley variable
  do n=1,mvars
     loc=nmotl_loc(n)
     if(loc <=0)cycle
     loc=n+nc2d
     if (cvarsmd(n)=='sti' .or. cvarsmd(n)=='stl') then
        do i=1,mlat
           corp(i,loc)=corz(i,1,nrf3_sf)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwll(i,1,nrf3_sf) 
        end do

     else if (cvarsmd(n)=='pswter') then
        do i=1,mlat
           corp(i,loc)=corp(i,nrf2_ps)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwllp(i,nrf2_ps)
        end do

     else if (cvarsmd(n)=='twter') then
        do i=1,mlat
           corp(i,loc)=corz(i,1,nrf3_t)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwll(i,1,nrf3_t)
        end do

     else if (cvarsmd(n)=='qwter') then
        do i=1,mlat
           corp(i,loc)=corz(i,1,nrf3_q)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwll(i,1,nrf3_q)
        end do

     else if (cvarsmd(n)=='gustwter') then
        do i=1,mlat
           corp(i,loc)=corp(i,nrf2_gust)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwllp(i,nrf2_gust)
        end do

     else if (cvarsmd(n)=='wspd10mwter') then
        do i=1,mlat
           corp(i,loc)=corp(i,nrf2_wspd10m)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwllp(i,nrf2_wspd10m)
        end do

     else if (cvarsmd(n)=='td2mwter') then
        do i=1,mlat
           corp(i,loc)=corp(i,nrf2_td2m)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwllp(i,nrf2_td2m)
        end do

     else if (cvarsmd(n)=='mxtmwter') then
        do i=1,mlat
           corp(i,loc)=corp(i,nrf2_mxtm)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwllp(i,nrf2_mxtm)
        end do

     else if (cvarsmd(n)=='mitmwter') then
        do i=1,mlat
           corp(i,loc)=corp(i,nrf2_mitm)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwllp(i,nrf2_mitm)
        end do

     else if (cvarsmd(n)=='uwnd10mwter') then
        do i=1,mlat
           corp(i,loc)=corp(i,nrf2_uwnd10m)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwllp(i,nrf2_uwnd10m)
        end do

     else if (cvarsmd(n)=='vwnd10mwter') then
        do i=1,mlat
           corp(i,loc)=corp(i,nrf2_vwnd10m)
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwllp(i,nrf2_vwnd10m)
        end do
!  if not found use default
     else 
        do i=1,mlat
           corp(i,loc)=corz_default
        end do
        do i=0,mlat+1
           hwllp(i,loc)=hwll_default
        end do
     end if
  enddo

  deallocate(nrf3_loc,nrf2_loc,nmotl_loc)
! Normalize vz with del sigmma and convert to vertical grid units!
  if(.not. twodvar_regional)then
     dlsig(1)=rlsig(1)-rlsig(2)
     do k=2,nsig-1
        dlsig(k)=half*(rlsig(k-1)-rlsig(k+1))
     enddo
     dlsig(nsig)=rlsig(nsig-1)-rlsig(nsig)

     do n=1,nc3d
        do j=0,mlat+1
           do k=1,nsig
              vz(k,j,n)=vz(k,j,n)*dlsig(k)
           end do
        end do
     end do
  end if


  return
end subroutine berror_read_wgt_reg

!++++
subroutine read_howv_stats(nlat,nlon,npar,arrout,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: read_howv_stats   
! prgmmr: stelios          org: ???                date: 2016-08-03
!
! abstract: improrts the bkerror stats for howv (variance, correlation lengths,
!           etc). Each quantity has been calculated externaly, it is interpolated at
!           the grid of URMA and it is saved as binary file. The exact format 
!           is shown in the code. 
!           The imported arrays can be 2d or 1d, aka can be lon/lat dependent or
!           two one of the two (most probably on lat)
!
!           For cross reference the MATLAB code is provided
!           Export ::
!              fid = fopen(['URMA_variance_Q',num2str(i1),'.bin'],'w');
!              dum =squeeze(variance(i1,:,:));
!              fwrite(fid,dum,'double');
!              fclose(fid);
!
!           Import :: 
!              fid = fopen(['URMA_variance_Q',num2str(i1),'1.bin'],'r');
!              dum = fread(fid, 'double');
!              A(:,:,i1) = reshape(dum,1597,2345);
!              fclose(fid);
!
!           To use it: 1. the data files (eg variance, correlation lengths have
!           to be located at the running path. So the driving scripts have to
!           copying the file of interest to the tmp dir used for e.g.:  
!
!              cp [...]/fix/urma2p5/URMA_variance_lat.bin URMA_variance1d.bin
!     
! program history log:
!   2016-08-03  stelios
!   2016-08-26  stelios : Compatible with GSI.
!   2023-07-30  Zhao    - added code to set the background error 
!                         standard deviation (corp_howv) and de-correlation
!                         length scale (hwllp_howv) for non-2DRTMA run
!   input argument list:
!     filename -  The name of the file 
!   output argument list:
!     arr_out   -  One or Two dimensional field of quantity of interest 
!
! attributes:
!   language: f90
!   machine: theia/gyre 
!
!$$$ end documentation block
!
   use kinds,only : r_kind, i_kind
   use gridmod, only : twodvar_regional
   use rapidrefresh_cldsurf_mod, only : corp_howv, hwllp_howv
   use gsi_io, only : verbose
!
   implicit none
! Declare passed variables
   integer(i_kind),   intent(in   )::nlat,nlon,npar
   integer(i_kind),   intent(in   ) :: mype  ! "my" processor ID
   real(r_kind), dimension(nlat ,nlon, npar),  intent(  out)::arrout
! Declare local variables
   integer(i_kind) :: reclength,i,j,i_npar
   character(256) ,  parameter :: myname='read_how_stats : '
   logical :: file_exists
   integer(i_kind), parameter :: lun34=34
   character(len=256),dimension(npar) :: filename
   integer(i_kind), parameter :: dp1 = kind(1D0)
!
   filename(1) = 'howv_var_berr.bin'
   filename(2) = 'howv_lng_berr.bin'
!-- first, assign the pre-defined values to corp and hwllp
   if ( twodvar_regional ) then
      arrout(:,:,1)=0.42_r_kind           ! values were specified by Manuel and Stelio for 2DRTMA
      arrout(:,:,2)=50000.0_r_kind        ! values were specified by Manuel and Stelio for 2DRTMA
   else
      arrout(:,:,1) = corp_howv           ! 0.42_r_kind used in 3dvar (default) if not set in namelist
      arrout(:,:,2) = hwllp_howv          ! 17000.0_r_kind used in 3dvar (default) if not set in namelist
   end if

   reclength=nlat*r_kind
!-- secondly, if files for corp and hwllp are available, then read them in for
!     corp and hwllp. If the files are not found, then use the pre-defined values.
   do i_npar = 1,npar
      inquire(file=trim(filename(i_npar)), exist=file_exists)
      if (file_exists)then
         open (unit=lun34  ,file=trim(filename(i_npar))  ,status='old'  &
               ,form='unformatted'   ,access='direct' ,recl=reclength )
         do j = 1,nlon
            read(unit=lun34 ,rec=j) (arrout(i,j,i_npar), i=1,nlat)
         enddo
         close(unit=lun34)
         if (verbose .and. mype .eq. 0) then         
           write(6,'(1x,A,1x,A2,1x,A)') trim(adjustl(myname)), '::',    &
             trim(filename(i_npar))//' is used for background error of howv.'
         end if

      else 
         if (verbose .and. mype .eq. 0) then         
           write(6,'(1x,A,1x,A2,1x,A)') trim(adjustl(myname)), '::',    &
             trim(filename(i_npar))//' does not exist for static BE of howv, using pre-defined values.'
         end if
      end if
   end do
end subroutine read_howv_stats

end module m_berror_stats_reg
