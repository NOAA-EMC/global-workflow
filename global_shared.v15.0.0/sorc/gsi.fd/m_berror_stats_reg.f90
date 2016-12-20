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
      use constants, only: zero,one,max_varname_length
      use gridmod, only: nsig
      use chemmod, only : berror_chem,upper2lower,lower2upper

      implicit none

      private   ! except

        ! reconfigurable parameters, via NAMELIST/setup/
      public :: berror_stats    ! reconfigurable filename

        ! interfaces to file berror_stats.
      public :: berror_set_reg          ! set internal parameters
      public :: berror_get_dims_reg	! get dimensions, jfunc::createj_func()
      public :: berror_read_bal_reg	! get cross-cov.stats., balmod::prebal()
      public :: berror_read_wgt_reg	! get auto-cov.stats., prewgt()

! !REVISION HISTORY:
!       25Feb10 - Zhu - adopt code format from m_berror_stats
!                     - extract from rdgstat_reg
!                     - change sructure of error file
!                     - make changes for generalized control variables
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname='m_berror_stats_reg'

     ! Reconfigurable parameters, vai NAMELISt/setup/
  character(len=256),save :: berror_stats = "berror_stats"   ! filename

  integer(i_kind),parameter :: default_unit_ = 22
  integer(i_kind),parameter :: ERRCODE=2
  logical,save:: cwcoveqqcov_

  integer(i_kind),allocatable,dimension(:):: lsig
  real(r_kind),allocatable,dimension(:):: coef1,coef2

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
      use gridmod,only : nlat,nlon
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
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::berror_read_bal_reg'

!   workspaces/variables for data not returned

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
    allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
    allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )

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

    agvi(0,:,:)=agvi(1,:,:)
    wgvi(0,:)=wgvi(1,:)
    bvi(0,:)=bvi(1,:)
    agvi(mlat+1,:,:)=agvi(mlat,:,:)
    wgvi(mlat+1,:)=wgvi(mlat,:)
    bvi(mlat+1,:)=bvi(mlat,:)
     
    deallocate (agv_avn,bv_avn,wgv_avn,clat_avn,sigma_avn,rlsigo)
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
      use gridmod,only : nlat,nlon,nsig
      use control_vectors,only: nrf,nc2d,nc3d,mvars,nvars
      use control_vectors,only: cvars => nrf_var
      use control_vectors,only: cvars2d,cvars3d,cvarsmd
      use guess_grids, only:  ges_psfcavg,ges_prslavg
      use constants, only: zero,one,ten,three
      use mpeu_util,only: getindex

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
!
!EOP ___________________________________________________________________

  character(len=*),parameter :: myname_=myname//'::berror_read_wgt_reg'

  real(r_kind),parameter:: r25      = one/25.0_r_kind
  real(r_kind),parameter:: zero_3   = 0.3_r_kind
  real(r_kind),parameter:: vz_oz    = 0.53333333_r_kind

!  workspace variables not returned
  real(r_single),dimension(:),allocatable::  clat_avn,sigma_avn
  real(r_single),dimension(:,:),allocatable::  bv_avn,wgv_avn,corqq_avn
  real(r_single),dimension(:,:,:),allocatable:: agv_avn
  real(r_single),dimension(:,:),allocatable:: corz_avn,hwll_avn,vztdq_avn

  real(r_single),dimension(1:mlat,msig,nrf):: corz_tmp
  real(r_single),dimension(0:mlat+1,msig,nrf):: hwll_tmp
  real(r_single),dimension(msig,0:mlat+1,nrf):: vz_tmp


  character*5 :: varshort
  character(len=max_varname_length) :: var
  logical,dimension(nrf):: nrf_err

  integer(i_kind) :: nrf3_oz,nrf3_q,nrf3_cw,nrf3_sf,nrf3_vp,nrf2_sst
  integer(i_kind) :: nrf3_t,nrf2_gust,nrf2_vis,nrf2_pblh,nrf2_ps,nrf2_wspd10m
  integer(i_kind) :: nrf2_td2m,nrf2_mxtm,nrf2_mitm,nrf2_pmsl,nrf2_howv,nrf2_tcamt,nrf2_lcbas,nrf2_cldch
  integer(i_kind) :: nrf3_sfwter,nrf3_vpwter
  integer(i_kind) :: inerr,istat
  integer(i_kind) :: nsigstat,nlatstat,isig
  integer(i_kind) :: loc,m1,m,i,n,j,k,n0
  integer(i_kind),allocatable,dimension(:) :: nrf2_loc,nrf3_loc,nmotl_loc
  real(r_kind) :: factoz
  real(r_kind) :: raux

  real(r_kind), parameter :: corz_default=one,hwll_default=100000_r_kind,&
                             vz_default=one

  allocate ( clat_avn(mlat) )
  allocate ( sigma_avn(1:msig) )
  allocate ( agv_avn(0:mlat+1,1:msig,1:msig) )
  allocate ( bv_avn(0:mlat+1,1:msig),wgv_avn(0:mlat+1,1:msig) )

! Open background error statistics file
  inerr=default_unit_
  if(present(unit)) inerr=unit
  open(inerr,file=berror_stats,form='unformatted',status='old')

! Read header.
  rewind inerr
  read(inerr) nsigstat,nlatstat

! Read background error file to get balance variables
  read(inerr)clat_avn,(sigma_avn(k),k=1,msig)
  read(inerr)agv_avn,bv_avn,wgv_avn

! compute vertical(pressure) interpolation index and weight
  do k=1,nsig
     rlsig(k)=log(ges_prslavg(k)/ges_psfcavg)
  enddo

  if(mype==0) then
     write(6,*) myname_,'(PREWGT_REG):  read error amplitudes ', &
       '"',trim(berror_stats),'".  ', &
       'mype,nsigstat,nlatstat =', &
        mype,nsigstat,nlatstat
  end if

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
        if (var == 'pm25') var = 'pm2_5'
     else 
        read(inerr,iostat=istat) varshort, isig
        var=varshort
     endif

     if (istat /= 0) exit
     allocate ( corz_avn(1:mlat,1:isig) )
     allocate ( hwll_avn(0:mlat+1,1:isig) )
     allocate ( vztdq_avn(1:isig,0:mlat+1) )

     if (var/='q' .or. (var=='cw' .and. cwoption==2)) then
        read(inerr) corz_avn
     else
        allocate ( corqq_avn(1:mlat,1:isig) )
        read(inerr) corz_avn,corqq_avn
     end if

     read(inerr) hwll_avn
     if (isig>1) then
        read(inerr) vztdq_avn
     end if

!    load the variances
     do n=1,nrf
        if (var==cvars(n)) then
           nrf_err(n)=.true.
           loc=n
           exit
        else
           loc=-999
        end if
     end do

     if (isig==msig) then
        do n=1,nc3d
           if (nrf3_loc(n)==loc) then
              if ((var=='q' .and. qoption==2) .or. (var=='cw' .and. cwoption==2)) then
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
                 do i=0,mlat+1
                    hwll_tmp(i,k,n)=hwll_avn(i,k)
                    vz_tmp(k,i,n)=vztdq_avn(k,i)
                 end do
              end do
              exit
           end if
        end do
     end if

     if (isig==1) then
       do n=1,nc2d
          if (nrf2_loc(n)==loc) then
             do i=1,mlat
                 corp(i,n)=corz_avn(i,1)
                 hwllp(i,n)=hwll_avn(i,1)
             end do
             hwllp(0,n)=hwll_avn(0,1)
             hwllp(mlat+1,n)=hwll_avn(mlat+1,1)
             exit
          end if
       end do
     end if

     deallocate ( corz_avn )
     deallocate ( hwll_avn )
     deallocate ( vztdq_avn )
     if (var=='q' .or. var=='cw') deallocate ( corqq_avn )
  enddo read
  close(inerr)

  deallocate(clat_avn,sigma_avn)
  deallocate(agv_avn,bv_avn,wgv_avn)

! 3d variable
  do n=1,nc3d
     loc=nrf3_loc(n)
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

  if(nrf3_q>0 .and. qoption==2)then
     do k=1,nsig
        do j=1,mlat
           varq(j,k)=min(max(real(corz(j,k,nrf3_q),r_kind),0.0015_r_kind),one)
           corz(j,k,nrf3_q)=one
        enddo
     enddo
  endif

  if (nrf3_oz>0) then 
     factoz = 0.0002_r_kind*r25
     corz(:,:,nrf3_oz)=factoz
!    hwll:,:,nrf3_oz)=400000.0_r_kind
     vz(:,:,nrf3_oz)=vz_oz
  end if

  if (cwcoveqqcov_ .and. nrf3_cw>0) then
     corz(:,:,nrf3_cw)=corz(:,:,nrf3_q)
     hwll(:,:,nrf3_cw)=hwll(:,:,nrf3_q)
     vz(:,:,nrf3_cw)=vz(:,:,nrf3_q)
  end if
  if ((.not. cwcoveqqcov_) .and. nrf3_cw>0) then
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
     end if
     hwll(:,:,nrf3_cw)=0.5_r_kind*hwll(:,:,nrf3_q)
     vz(:,:,nrf3_cw)=0.5_r_kind*vz(:,:,nrf3_q)
  end if


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

! 2d variable
  do n=1,nc2d
     loc=nrf2_loc(n)
     if (nrf_err(loc)) cycle
     if (n==nrf2_sst) then
        do i=1,mlat
           corp(i,n)=zero_3
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_sf)
           hwllp(i,nc2d+1)=hwll(i,1,nrf3_sf) !not very nice, since it assumes that stl and sti
           hwllp(i,nc2d+2)=hwll(i,1,nrf3_sf) !are always the first motley variables in convinfo
        end do
     end if
     if (n==nrf2_gust) then
        do i=1,mlat
           corp(i,n)=three
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_q)
        end do
     end if
     if (n==nrf2_vis) then
        do i=1,mlat
           corp(i,n)=20000.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
     end if
     if (n==nrf2_pblh) then
        do i=1,mlat
           corp(i,n)=500.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=three*hwll(i,1,nrf3_t)
        end do
     end if
     if (n==nrf2_wspd10m) then
        do i=1,mlat
           corp(i,n)=three
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_q)
        end do
     end if
     if (n==nrf2_td2m) then
        raux=maxval(corz(1:mlat,1,nrf3_q))
        do i=1,mlat
           corp(i,n)=(corz(i,1,nrf3_q)/raux)*three !tentatively
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_q) !tentatively
        end do
     end if
     if (n==nrf2_mxtm) then
        do i=1,mlat
           corp(i,n)=corz(i,1,nrf3_t)
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
     end if
     if (n==nrf2_mitm) then
        do i=1,mlat
           corp(i,n)=corz(i,1,nrf3_t)
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
     end if
     if (n==nrf2_pmsl) then
        do i=1,mlat
           corp(i,n)=corp(i,nrf2_ps)
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwllp(i,nrf2_ps)
        end do
     end if
     if (n==nrf2_howv) then
        do i=1,mlat
           corp(i,n)=0.3_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_sf) !tentatively
        end do
     end if
     if (n==nrf2_tcamt) then
        do i=1,mlat
           corp(i,n)=50.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
     end if
     if (n==nrf2_lcbas) then
        do i=1,mlat
           corp(i,n)=40000.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
        print*, 'm_berror_reg: maxhwllp_lcbas=',maxval(hwllp(:,n))
     end if
     if (n==nrf2_cldch) then
        do i=1,mlat
           corp(i,n)=40000.0_r_kind
        end do
        do i=0,mlat+1
           hwllp(i,n)=hwll(i,1,nrf3_t)
        end do
        print*, 'm_berror_reg: maxhwllp_cldch=',maxval(hwllp(:,n))
     end if

  enddo


! motley variable
  n0=nc2d 
  do n=1,mvars
     if (cvarsmd(n)=='stl') cycle
     if (cvarsmd(n)=='sti') cycle

     if (cvarsmd(n)=='pswter') then
        do i=1,mlat
           corp(i,n0+n)=corp(i,nrf2_ps)
        end do
        do i=0,mlat+1
           hwllp(i,n0+n)=hwllp(i,nrf2_ps)
        end do
     endif

     if (cvarsmd(n)=='twter') then
        do i=1,mlat
           corp(i,n0+n)=corz(i,1,nrf3_t)
        end do
        do i=0,mlat+1
           hwllp(i,n0+n)=hwll(i,1,nrf3_t)
        end do
     endif

     if (cvarsmd(n)=='qwter') then
        do i=1,mlat
           corp(i,n0+n)=corz(i,1,nrf3_q)
        end do
        do i=0,mlat+1
           hwllp(i,n0+n)=hwll(i,1,nrf3_q)
        end do
     endif

     if (cvarsmd(n)=='gustwter') then
        do i=1,mlat
           corp(i,n0+n)=corp(i,nrf2_gust)
        end do
        do i=0,mlat+1
           hwllp(i,n0+n)=hwllp(i,nrf2_gust)
        end do
     endif

     if (cvarsmd(n)=='wspd10mwter') then
        do i=1,mlat
           corp(i,n0+n)=corp(i,nrf2_wspd10m)
        end do
        do i=0,mlat+1
           hwllp(i,n0+n)=hwllp(i,nrf2_wspd10m)
        end do
     endif

     if (cvarsmd(n)=='td2mwter') then
        do i=1,mlat
           corp(i,n0+n)=corp(i,nrf2_td2m)
        end do
        do i=0,mlat+1
           hwllp(i,n0+n)=hwllp(i,nrf2_td2m)
        end do
     endif

     if (cvarsmd(n)=='mxtmwter') then
        do i=1,mlat
           corp(i,n0+n)=corp(i,nrf2_mxtm)
        end do
        do i=0,mlat+1
           hwllp(i,n0+n)=hwllp(i,nrf2_mxtm)
        end do
     endif

     if (cvarsmd(n)=='mitmwter') then
        do i=1,mlat
           corp(i,n0+n)=corp(i,nrf2_mitm)
        end do
        do i=0,mlat+1
           hwllp(i,n0+n)=hwllp(i,nrf2_mitm)
        end do
     endif
  enddo

  deallocate(nrf3_loc,nrf2_loc,nmotl_loc)

  return
end subroutine berror_read_wgt_reg

end module m_berror_stats_reg
