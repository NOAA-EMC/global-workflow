subroutine setuppm10(lunin,mype,nreal,nobs,isis,is,conv_diagsave)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setuppm10 --- Compute rhs of oi for in-situ pm10 obs
!
!   prgrmmr:     parrish          org: np22                date: 1990-10-06
!
! abstract:      For pm10 observations, this routine
!                  a) reads obs assigned to given mpi task (geographic region),
!                  b) simulates obs from guess,
!                  c) apply some quality control to obs,
!                  d) load weight and innovation arrays used in minimization
!                  e) collects statistics for runtime diagnostic output
!                  f) writes additional diagnostic information to output file
!

! program history log:
!   2016-02-20  pagowski - based on setupmp2_5; converted for pm10
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nreal          - number of pieces of info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     
!     obstype        - type of pm10 obs
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
     
! !uses:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,i_kind,r_single
  
  use constants, only : zero,half,one,two,tiny_r_kind
  use constants, only : cg_term,wgtlim
  use constants, only: huge_single,r10
  use constants, only: r1000,rd,max_varname_length

  use obsmod, only : pm10head,pm10tail,&
       pm10_ob_type,i_pm10_ob_type,time_offset
  use obsmod, only : obsdiags,lobsdiag_allocated,lobsdiagsave
  use obsmod, only : obs_diag,luse_obsdiag
  use qcmod, only : dfact,dfact1
  
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  
  use gridmod, only : get_ij,get_ijk,nsig
  
  use guess_grids, only : nfldsig,hrdifsig
  use gsi_bundlemod, only : gsi_bundlegetpointer,GSI_BundlePrint
  use gsi_chemguess_mod, only : gsi_chemguess_get,gsi_chemguess_bundle
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  
  use convinfo, only: cgross,cvar_b,cvar_pg,&
        icuse,ictype,icsubtype
  
  use jfunc, only : jiter,last,miter
  
  use m_dtime, only: dtime_setup, dtime_check

  use chemmod, only : &
        iconc,ierror,ilat,ilon,itime,iid,ielev,isite,iikx,&
        elev_tolerance,elev_missing,pm10_teom_max,ilate,ilone
  use chemmod, only : oneobtest_chem,maginnov_chem,conconeobs
  use chemmod, only : d_10,nh4_mfac,oc_mfac,ppmv_conv
  use chemmod, only: naero_gocart_wrf,aeronames_gocart_wrf,&
      upper2lower,lower2upper,laeroana_gocart


  use gridmod, only : cmaq_regional,wrf_mass_regional

  implicit none
  
! !input parameters:

  character(len=3) :: cvar='pm1'
  integer(i_kind)                  , intent(in   ) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in   ) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in   ) :: nreal  ! number of pieces of non-co info (location, time, etc) per obs
  integer(i_kind)                  , intent(inout) :: nobs   ! number of observations
  character(20)                    , intent(in   ) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in   ) :: is     
  logical                          , intent(in   ) :: conv_diagsave   ! logical to save innovation dignostics
  
! a function of level
!-------------------------------------------------------------------------
  
! declare local parameters  

  character(len=*),parameter:: myname="setuppm10"
  
! declare local variables  
  
  real(r_kind) rat_err2,dlat,dtime,dlon
  real(r_kind) cg_pm10,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) :: pm10ges
  real(r_kind) :: ratio_errors,error
  real(r_kind) :: innov,innov_error2,rwgt,valqc,tfact,innov_error,elevges,&
        elevdiff,conc,elevobs,ps_ges,site_id,tv_ges
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final

  real(r_kind) ,dimension(nreal,nobs):: data
  real(r_kind),pointer,dimension(:,:,:):: rank3
  
  INTEGER(i_kind) i,k,ier,ibin,l,istat,ikx,ii,jj,idia,ifld
  integer(i_kind) mm1
  integer(i_kind) :: nchar,nrealdiag


  character(len=8) :: station_id
  character(len=8),allocatable,dimension(:) :: cdiagbuf
  real(r_single),allocatable,dimension(:,:) :: rdiagbuf

  real(r_kind),dimension(4):: tempwij

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs) :: dup

  logical:: in_curbin, in_anybin
  logical proceed
  integer(i_kind),dimension(nobs_bins) :: n_alloc
  integer(i_kind),dimension(nobs_bins) :: m_alloc
  type(pm10_ob_type),pointer:: my_head
  type(obs_diag),pointer:: my_diag

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_pm10
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

  character(len=max_varname_length) :: aeroname

  integer(i_kind) :: ipm10,n_gocart_var


! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  n_alloc(:)=0
  m_alloc(:)=0

  nchar=1
  nrealdiag=19
  mm1=mype+1

!
!*********************************************************************************
! get pointer to pm10 guess state, if not present return 

  if (cmaq_regional) then
     WRITE(6,*) 'pm10 for cmaq not implemented. Stopping'
     CALL stop2(451)
  endif

  if (wrf_mass_regional .and. laeroana_gocart) then

!check if aerosol species in control

     call gsi_chemguess_get ( 'aerosols::3d', n_gocart_var, ier )

     if (n_gocart_var /= naero_gocart_wrf) then
        write(6,*) 'setuppm10: not all gocart aerosols in anavinfo'
        call stop2(451)
     endif

     do i=1,naero_gocart_wrf
        aeroname=upper2lower(aeronames_gocart_wrf(i))
        call gsi_chemguess_get ('var::'//trim(aeroname), ipm10, ier )
        if (ier > 0 .or. ipm10 <= 0) then
           write(6,*) 'convinfo: ',trim(aeroname),' missing in anavinfo'
           call stop2(452)
        endif
     enddo

     if (size(gsi_chemguess_bundle)==nfldsig) then
        aeroname='bc1'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           allocate(ges_pm10(size(rank3,1),size(rank3,2),size(rank3,3),&
                nfldsig))
           ges_pm10(:,:,:,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=rank3
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='bc2'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='sulf'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3*nh4_mfac
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3*nh4_mfac
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='p25'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='oc1'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3*oc_mfac
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3*oc_mfac
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='oc2'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3*oc_mfac
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3*oc_mfac
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='seas1'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='seas2'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='seas3'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),TRIM(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),TRIM(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm10: ',TRIM(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='dust1'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='dust2'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm10: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='dust3'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),TRIM(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),TRIM(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm10: ',TRIM(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='dust4'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),TRIM(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm10(:,:,:,1)=ges_pm10(:,:,:,1)+rank3*d_10
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),TRIM(aeroname),rank3,ier)
              ges_pm10(:,:,:,ifld)=ges_pm10(:,:,:,ifld)+rank3*d_10
           enddo
        else
           write(6,*) 'setuppm10: ',TRIM(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif


     else
        write(6,*) 'setuppm10: inconsistent vector sizes (nfldsig,size(chemguess_bundle) ',&
             nfldsig,size(gsi_chemguess_bundle)
        call stop2(420)
     endif

  endif


! initialize arrays

  read(lunin)data,luse

  dup=one

  do i=1,nobs
     muse(i) = (icuse(nint(data(iikx,i))) <= jiter)
  enddo

  do k=1,nobs
     do l=k+1,nobs
        if(data(ilat,k) == data(ilat,l) .and.  &
             data(ilon,k) == data(ilon,l) .and.  &
             data(ielev,k) == data(ielev,l) .and.  &
             data(isite,k) == data(isite,l) .and.  &
             muse(k) .and. muse(l)) then
           tfact = min(one,abs(data(itime,k)-data(itime,l))/dfact1)
           dup(k)=dup(k)+one-tfact*tfact*(one-dfact)
           dup(l)=dup(l)+one-tfact*tfact*(one-dfact)
        end if
     end do
  end do
  
! if requested, save select data for output to diagnostic file
  if(conv_diagsave)then
     ii=0
     if (lobsdiagsave) nrealdiag=nrealdiag+4*miter+1
     allocate(cdiagbuf(nobs),rdiagbuf(nrealdiag,nobs))
  end if
  mm1=mype+1


  call dtime_setup()
  
  if (oneobtest_chem) then
     WRITE(6,*)'setuppm10: oneobtest_chem available only for pm2_5'
     CALL stop2(424)
  endif
     

  if (trim(isis)=='TEOM') then

     DO i=1,nobs
        
        dtime=data(itime,i)

        call dtime_check(dtime, in_curbin, in_anybin)
        if(.not.in_anybin) cycle

        if (nobs_bins > 1) then
           ibin = nint( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif

        if (ibin < 1 .or. ibin > nobs_bins) &
              write(6,*)mype,'error nobs_bins,ibin= ',nobs_bins,ibin
        
!    link obs to diagnostics structure
        if(luse_obsdiag)then
           if (.not.lobsdiag_allocated) then
              if (.not.associated(obsdiags(i_pm10_ob_type,ibin)%head)) then
                 allocate(obsdiags(i_pm10_ob_type,ibin)%head,stat=istat)
                 if (istat/=0) then
                    write(6,*)'setupq: failure to allocate obsdiags',istat
                    call stop2(421)
                 end if
                 obsdiags(i_pm10_ob_type,ibin)%tail => obsdiags(i_pm10_ob_type,ibin)%head
              else
                 allocate(obsdiags(i_pm10_ob_type,ibin)%tail%next,stat=istat)
                 if (istat/=0) then
                    write(6,*)'setupq: failure to allocate obsdiags',istat
                    call stop2(422)
                 end if
                 obsdiags(i_pm10_ob_type,ibin)%tail => obsdiags(i_pm10_ob_type,ibin)%tail%next
              end if

              allocate(obsdiags(i_pm10_ob_type,ibin)%tail%muse(miter+1))
              allocate(obsdiags(i_pm10_ob_type,ibin)%tail%nldepart(miter+1))
              allocate(obsdiags(i_pm10_ob_type,ibin)%tail%tldepart(miter))
              allocate(obsdiags(i_pm10_ob_type,ibin)%tail%obssen(miter))

              obsdiags(i_pm10_ob_type,ibin)%tail%indxglb=i
              obsdiags(i_pm10_ob_type,ibin)%tail%nchnperobs=-99999
              obsdiags(i_pm10_ob_type,ibin)%tail%luse=.false.
              obsdiags(i_pm10_ob_type,ibin)%tail%muse(:)=.false.
              obsdiags(i_pm10_ob_type,ibin)%tail%nldepart(:)=-huge(zero)
              obsdiags(i_pm10_ob_type,ibin)%tail%tldepart(:)=zero
              obsdiags(i_pm10_ob_type,ibin)%tail%wgtjo=-huge(zero)
              obsdiags(i_pm10_ob_type,ibin)%tail%obssen(:)=zero
           
              n_alloc(ibin) = n_alloc(ibin) +1
              my_diag => obsdiags(i_pm10_ob_type,ibin)%tail
              my_diag%idv = is
              my_diag%iob = i
              my_diag%ich = 1
              
           else
              if (.not.associated(obsdiags(i_pm10_ob_type,ibin)%tail)) then
                 obsdiags(i_pm10_ob_type,ibin)%tail => obsdiags(i_pm10_ob_type,ibin)%head
              else
                 obsdiags(i_pm10_ob_type,ibin)%tail => obsdiags(i_pm10_ob_type,ibin)%tail%next
              end if
              if (obsdiags(i_pm10_ob_type,ibin)%tail%indxglb/=i) then
                 write(6,*)'setuppm10: index error'
                 call stop2(423)
              end if
           endif
        endif
        
        if(.not.in_curbin) cycle
        
        dlat=data(ilat,i)
        dlon=data(ilon,i)
        conc=data(iconc,i)
        elevobs=data(ielev,i)
        ikx=nint(data(iikx,i))
        site_id=data(iid,i)

        call tintrp2a11(ges_z,elevges,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
        
!obs are conc
!wrf state vars are as mix ratio
!cmaq pm10 is as conc
!might convert for cmaq at some point as well


        if (wrf_mass_regional) then
           call tintrp2a11(ges_ps,ps_ges,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)

           call tintrp2a11(ges_tv(:,:,1,nfldsig),tv_ges,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)

           conc=conc/(ps_ges*r1000/(rd*tv_ges))
        endif



!if elevobs is known than calculate difference otherwise
!assume that difference is acceptable
        
        if (elevobs > elev_missing) then
           elevdiff=abs(elevobs-elevges)
        else
           elevdiff=zero
!if elevation unknown include observation nevertheless
        endif
        
        call tintrp2a11(ges_pm10,pm10ges,dlat,dlon,dtime,hrdifsig,&
             mype,nfldsig)
        innov = conc - pm10ges

        error=one/data(ierror,i)
        ratio_errors=one/sqrt(real(dup(i)))
        innov_error = error*innov
        
        if (abs(innov) > cgross(ikx) .or. &
              conc > pm10_teom_max  .or. &
              elevdiff > elev_tolerance) then
           muse(i)=.false.
        endif

        rat_err2 = ratio_errors**2
        
        if(luse(i))then
           
           innov_error2     = innov_error*innov_error
           exp_arg = -half*innov_error2
           rat_err2 = ratio_errors**2
                 
           if (cvar_pg(ikx) > tiny_r_kind ) then
              arg  = exp(exp_arg)
              wnotgross= one-cvar_pg(ikx)
              cg_pm10=cvar_b(ikx)
              wgross = cg_term*cvar_pg(ikx)/(cg_pm10*wnotgross)
              term = log((arg+wgross)/(one+wgross))
              wgt  = one-wgross/(arg+wgross)
              rwgt = wgt/wgtlim
           else
              term = exp_arg
              wgt  = wgtlim
              rwgt = wgt/wgtlim
           endif
           
           valqc = -two*rat_err2*term

        endif

        if(luse_obsdiag)then
           obsdiags(i_pm10_ob_type,ibin)%tail%luse=luse(i)
           obsdiags(i_pm10_ob_type,ibin)%tail%muse(jiter)=muse(i)
           obsdiags(i_pm10_ob_type,ibin)%tail%nldepart(jiter)=innov
           obsdiags(i_pm10_ob_type,ibin)%tail%wgtjo= (error*ratio_errors)**2
        end if

        if (.not. last .and. muse(i)) then
           
           if(.not. associated(pm10head(ibin)%head))then
              allocate(pm10head(ibin)%head,stat=istat)
              if(istat /= 0)write(6,*)' failure to write pm10head '
              pm10tail(ibin)%head => pm10head(ibin)%head
           else
              allocate(pm10tail(ibin)%head%llpoint,stat=istat)
              if(istat /= 0)write(6,*)' failure to write pm10tail%llpoint '
              pm10tail(ibin)%head => pm10tail(ibin)%head%llpoint
           end if
           
           m_alloc(ibin) = m_alloc(ibin) +1
           my_head => pm10tail(ibin)%head
           my_head%idv = is
           my_head%iob = i
           
           call get_ij(mm1,dlat,dlon,&
                 pm10tail(ibin)%head%ij(1),tempwij(1))

           pm10tail(ibin)%head%ij(5:8)=pm10tail(ibin)%head%ij(1:4)
           pm10tail(ibin)%head%wij(1:4)=tempwij
           pm10tail(ibin)%head%wij(5:8)=zero
           pm10tail(ibin)%head%res    = innov
           pm10tail(ibin)%head%err2   = error**2
           pm10tail(ibin)%head%raterr2= ratio_errors**2
           pm10tail(ibin)%head%time   = dtime
           pm10tail(ibin)%head%b      = cvar_b(ikx)
           pm10tail(ibin)%head%pg     = cvar_pg(ikx)
           pm10tail(ibin)%head%luse   = luse(i)
           if(luse_obsdiag)then
              pm10tail(ibin)%head%diags => &
                    obsdiags(i_pm10_ob_type,ibin)%tail

              my_head => pm10tail(ibin)%head
              my_diag => pm10tail(ibin)%head%diags
              if(my_head%idv /= my_diag%idv .or. &
                   my_head%iob /= my_diag%iob ) then
                 call perr(myname,'mismatching %[head,diags]%(idv,iob,ibin) =',&
                       (/is,i,ibin/))
                 call perr(myname,'my_head%(idv,iob) =',&
                       (/my_head%idv,my_head%iob/))
                 call perr(myname,'my_diag%(idv,iob) =',&
                       (/my_diag%idv,my_diag%iob/))
                 call die(myname)
              endif
           end if

        endif
        
! save select output for diagnostic file
        if (conv_diagsave) then

           ii=ii+1

           call tintrp2a11(ges_ps,ps_ges,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)

           ps_ges=ps_ges*r10 ! convert from cb to hpa

           write(station_id,'(Z8)')nint(site_id)

           cdiagbuf(ii)    = station_id         ! station id

           rdiagbuf(1,ii)  = ictype(ikx)        ! observation type

           rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
           
           rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
           rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
           rdiagbuf(5,ii)  = data(ielev,i)    ! station elevation (meters)
           rdiagbuf(6,ii)  = ps_ges              ! observation pressure (hpa)
           rdiagbuf(7,ii)  = data(ielev,i)    ! observation height (meters)
           rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)
           
           rdiagbuf(9,ii)  = zero !data(iqc,i)  input prepbufr qc or event mark
           rdiagbuf(10,ii) = zero !data(iqt,i)  setup qc or event mark (currently qtflg only)
           rdiagbuf(11,ii) = one       ! read_prepbufr data usage flag
           if(muse(i)) then
              rdiagbuf(12,ii) = one            ! analysis usage flag (1=use, -1=not used)
           else
              rdiagbuf(12,ii) = -one
           endif
           
           err_input = data(ierror,i)
           err_adjst = data(ierror,i)

           if (ratio_errors*error>tiny_r_kind) then
              err_final = one/(ratio_errors*error)
           else
              err_final = huge_single
           endif
           
           errinv_input = huge_single
           errinv_adjst = huge_single
           errinv_final = huge_single
           if (err_input>tiny_r_kind) errinv_input=one/err_input
           if (err_adjst>tiny_r_kind) errinv_adjst=one/err_adjst
           if (err_final>tiny_r_kind) errinv_final=one/err_final
           
           rdiagbuf(13,ii) = rwgt               ! nonlinear qc relative weight
           rdiagbuf(14,ii) = errinv_input       ! prepbufr inverse obs error (k**-1)
           rdiagbuf(15,ii) = errinv_adjst       ! read_prepbufr inverse obs error (k**-1)
           rdiagbuf(16,ii) = errinv_final       ! final inverse observation error (k**-1)
           
           rdiagbuf(17,ii) = data(iconc,i)       ! temperature observation (k)
           rdiagbuf(18,ii) = innov   ! obs-ges used in analysis (ugm^-3)
           rdiagbuf(19,ii) = innov   ! obs-ges w/o bias correction (ugm^-3) (future slot)

           idia=nrealdiag
           if (lobsdiagsave) then
              do jj=1,miter
                 idia=idia+1
                 if (obsdiags(i_pm10_ob_type,ibin)%tail%muse(jj)) then
                    rdiagbuf(idia,ii) = one
                 else
                    rdiagbuf(idia,ii) = -one
                 endif
              enddo
              
              do jj=1,miter+1
                 idia=idia+1
                 rdiagbuf(idia,ii) = obsdiags(i_pm10_ob_type,ibin)%tail%nldepart(jj)
              enddo

              do jj=1,miter
                 idia=idia+1
                 rdiagbuf(idia,ii) = obsdiags(i_pm10_ob_type,ibin)%tail%tldepart(jj)
              enddo

              do jj=1,miter
                 idia=idia+1
                 rdiagbuf(idia,ii) = obsdiags(i_pm10_ob_type,ibin)%tail%obssen(jj)
              enddo

           endif

        endif

! end of loop over observations
     enddo

  else
     
     
!!will be similar except for get_ijk 
!!if not teom isis fill in 
!!should be used for other in-situ obs e.g. soundings/aircraft e.g.
     
  endif

! Release memory of local guess arrays
  call final_vars_

!! write information to diagnostic file
  if(conv_diagsave .and.ii>0) then
     write(7)cvar,nchar,nrealdiag,ii,mype,nrealdiag
     write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
     deallocate(cdiagbuf,rdiagbuf)
  end if
!  
  return
  contains

  subroutine check_vars_ (proceed)
    use chemmod, only: naero_gocart_wrf,aeronames_gocart_wrf
  logical,intent(inout) :: proceed
  integer(i_kind) ivar, istatus
! Check to see if required guess fields are available
  call gsi_metguess_get ('var::ps', ivar, istatus )
  proceed=ivar>0
  call gsi_metguess_get ('var::z' , ivar, istatus )
  proceed=proceed.and.ivar>0
!
  if (wrf_mass_regional .and. laeroana_gocart) then
     do i=1,naero_gocart_wrf
        aeroname=upper2lower(aeronames_gocart_wrf(i))
        call gsi_chemguess_get ('var::'//trim(aeroname), ivar, istatus )
        if (ivar == 0) exit
     enddo
  endif

  proceed=proceed.and.ivar>0

  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:  ),pointer:: rank2=>NULL()
  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  character(len=5) :: varname
  integer(i_kind) ifld, istatus

! If require guess vars available, extract from bundle ...
  if(size(gsi_metguess_bundle)==nfldsig) then
!    get ps ...
     varname='ps'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_ps))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_ps(size(rank2,1),size(rank2,2),nfldsig))
         ges_ps(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_ps(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get z ...
     varname='z'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank2,istatus)
     if (istatus==0) then
         if(allocated(ges_z))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_z(size(rank2,1),size(rank2,2),nfldsig))
         ges_z(:,:,1)=rank2
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank2,istatus)
            ges_z(:,:,ifld)=rank2
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus
         call stop2(999)
     endif
!    get tv ...
     varname='tv'
     call gsi_bundlegetpointer(gsi_metguess_bundle(1),trim(varname),rank3,istatus)                   
     if (istatus==0) then
         if(allocated(ges_tv))then
            write(6,*) trim(myname), ': ', trim(varname), ' already incorrectly alloc '      
            call stop2(999)
         endif
         allocate(ges_tv(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_tv(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_metguess_bundle(ifld),trim(varname),rank3,istatus)       
            ges_tv(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) trim(myname),': ', trim(varname), ' not found in met bundle, ier= ',istatus    
         call stop2(999)
     endif
  else
     write(6,*) trim(myname), ': inconsistent vector sizes (nfldsig,size(metguess_bundle) ',&
                 nfldsig,size(gsi_metguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine final_vars_
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_pm10)) deallocate(ges_pm10)
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setuppm10
