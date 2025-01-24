module pm2_5_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setuppm2_5; end interface

contains
subroutine setuppm2_5(obsLL,odiagLL,lunin,mype,nreal,nobs,isis,is,conv_diagsave)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setuppm2_5 --- Compute rhs of oi for in-situ pm2_5 obs
!
!   prgrmmr:     parrish          org: np22                date: 1990-10-06
!
! abstract:      For pm2_5 observations this routine
!                  a) reads obs assigned to given mpi task (geographic region),
!                  b) simulates obs from guess,
!                  c) apply some quality control to obs,
!                  d) load weight and innovation arrays used in minimization
!                  e) collects statistics for runtime diagnostic output
!                  f) writes additional diagnostic information to output file
!                  g) converted to pm2_5
!

! program history log:
!   2010-10-03  pagowski - based on setupX; converted for pm2_5
!   2013-01-26  parrish - convert tintrp2a to tintrp2a1, tintrp2a11 (so debug compile works on WCOSS)
!   2013-10-19  todling - metguess now holds background
!   2013-11-26  guo     - removed nkeep==0 escaping to allow more than one obstype sources.
!   2014-01-28  todling - write sensitivity slot indicator (idia) to header of diagfile
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2017-02-06  todling - add netcdf_diag capability; hidden as contained code
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!   2022-04-19  h.wang  - add code for fv3_cmaq_regional
!                          - fv3_cmaq_regional=.true. : model is regional FV3-CMAQ
!                          - laeroana_fv3cmaq=.true.  : produce the analysis for regional FV3-CMAQ
!   2022-08-10  h.Wang  - add code for regional FV3-SD (RRFS-SMOKE/DUST) model
!                          - laeroana_fv3smoke=.true. : produce the analysis for RRFS-SD
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nreal          - number of pieces of info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     
!     obstype        - type of pm2_5 obs
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

  use m_obsdiagNode, only : obs_diag
  use m_obsdiagNode, only : obs_diags
  use m_obsdiagNode, only : obsdiagLList_nextNode
  use m_obsdiagNode, only : obsdiagNode_set
  use m_obsdiagNode, only : obsdiagNode_get
  use m_obsdiagNode, only : obsdiagNode_assert

  use m_obsNode, only: obsNode
  use m_pm2_5Node, only : pm2_5Node
  use m_pm2_5Node, only : pm2_5Node_appendto
  use m_obsLList, only: obsLList
  use obsmod, only : time_offset
  use obsmod, only : lobsdiag_allocated,lobsdiagsave
  use obsmod, only : luse_obsdiag,ianldate

  use obsmod, only: netcdf_diag, binary_diag, dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close

  use qcmod, only : dfact,dfact1
  
  use gsi_4dvar, only: nobs_bins,hr_obsbin
  
  use gridmod, only : get_ij,get_ijk
  use guess_grids, only : nfldsig,hrdifsig
  use guess_grids, only : veg_type
  use gsi_bundlemod, only : gsi_bundlegetpointer,GSI_BundlePrint
  use gsi_chemguess_mod, only : gsi_chemguess_get,gsi_chemguess_bundle
  use gsi_metguess_mod, only : gsi_metguess_get,gsi_metguess_bundle
  
  use convinfo, only: cgross,cvar_b,cvar_pg,&
        icuse,ictype,icsubtype
  
  use jfunc, only : jiter,last,miter
  
  use m_dtime, only: dtime_setup, dtime_check

  use chemmod, only : &
        iconc,ierror,ilat,ilon,itime,iid,ielev,isite,iikx,&
        elev_tolerance,elev_missing,pm2_5_teom_max,ilate,ilone
  use chemmod, only : oneobtest_chem,maginnov_chem,conconeobs
  use chemmod, only : s_2_5,d_2_5,nh4_mfac,oc_mfac
  use chemmod, only: naero_gocart_wrf,aeronames_gocart_wrf,&
      upper2lower,lower2upper,laeroana_gocart,wrf_pm2_5
  use chemmod, only: naero_cmaq_fv3,aeronames_cmaq_fv3,imodes_cmaq_fv3,laeroana_fv3cmaq
  use chemmod, only: naero_smoke_fv3,aeronames_smoke_fv3,laeroana_fv3smoke
  use chemmod, only: pm2_5_innov_threshold,pm2_5_urban_innov_threshold,pm2_5_bg_threshold
  use gridmod, only : cmaq_regional,wrf_mass_regional,fv3_cmaq_regional 
  implicit none
  
! !input parameters:
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  character(len=3) :: cvar='pm2'
  integer(i_kind)                  , intent(in) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in) :: nreal  ! number of pieces of non-co info (location, time, etc) per obs
  integer(i_kind)                  , intent(in) :: nobs   ! number of observations
  character(20)                    , intent(in) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in) :: is     
  logical                          , intent(in) :: conv_diagsave   ! logical to save innovation dignostics
  
! a function of level
!-------------------------------------------------------------------------
  
! declare local parameters  

  character(len=*),parameter:: myname="setuppm2_5"
  
! declare local variables  
  
  real(r_kind) rat_err2,dlat,dtime,dlon
  real(r_kind) cg_pm2_5,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) :: pm2_5ges
  real(r_kind) :: ratio_errors,error
  real(r_kind) :: innov,innov_error2,rwgt,valqc,tfact,innov_error,elevges,&
        elevdiff,conc,elevobs,ps_ges,site_id,tv_ges,veg_type_ges
  real(r_kind) errinv_input,errinv_adjst,errinv_final
  real(r_kind) err_input,err_adjst,err_final

  real(r_kind) ,dimension(nreal,nobs):: data
  real(r_kind),pointer,dimension(:,:,:):: rank3
  
  integer(i_kind) i,k,ier,ibin,l,ikx,ii,jj,idia,ifld
  integer(i_kind) mm1
  integer(i_kind) :: nchar,nrealdiag


  character(len=8) :: station_id
  character(len=8),allocatable,dimension(:) :: cdiagbuf
  real(r_single),allocatable,dimension(:,:) :: rdiagbuf

  real(r_kind),dimension(4):: tempwij

  logical,dimension(nobs):: luse,muse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  integer(i_kind),dimension(nobs) :: dup

  logical:: in_curbin, in_anybin
  logical proceed
  type(pm2_5Node),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_ps
  real(r_kind),allocatable,dimension(:,:,:  ) :: ges_z
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_pm2_5
  real(r_kind),allocatable,dimension(:,:,:,:) :: ges_tv

  character(len=max_varname_length) :: aeroname

  integer(i_kind) :: ipm2_5,n_gocart_var

  integer(i_kind) :: n_cmaq_var,n_smoke_var
  real(r_kind),allocatable,dimension(:,:,:,:,:) :: pm25wc
  real(r_kind) :: pm25wc_ges(3)

  type(obsLList),pointer,dimension(:):: pm2_5head
  pm2_5head => obsLL(:)


! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  nchar=1
  nrealdiag=19
  mm1=mype+1

!
!*********************************************************************************
! get pointer to pm2_5 guess state, if not present return 

  if ( cmaq_regional .or. (wrf_mass_regional .and. wrf_pm2_5) ) then

     call gsi_chemguess_get ('var::pm2_5', ipm2_5, ier )
     if (ipm2_5 <= 0) then
        write(6,*)'pm2_5 not in chem_guess - returning from setuppm2_5'
        return
     endif

     if (size(gsi_chemguess_bundle)==nfldsig) then
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),'pm2_5',rank3,ier)
        if (ier==0) then
           allocate(ges_pm2_5(size(rank3,1),size(rank3,2),size(rank3,3),&
                nfldsig))
           ges_pm2_5(:,:,:,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),'pm2_5',rank3,ier)
              ges_pm2_5(:,:,:,ifld)=rank3
           enddo
        else
           write(6,*) 'setuppm2_5: pm2_5 not found in chem bundle, ier= ',ier
           call stop2(453)
        endif
     else
        write(6,*) 'setuppm2_5: inconsistent vector sizes (nfldsig,size(chemguess_bundle) ',&
          nfldsig,size(gsi_chemguess_bundle)
        call stop2(420)
     endif
     
  endif

  if (laeroana_fv3smoke)then
!    check if aerosol species in control
     call gsi_chemguess_get ( 'aerosols::3d', n_smoke_var, ier )

!    n_smoke_var ges vars in anainfo; naero_smoke_fv3 in chemmod
!    if naero_smoke_fv3 is greater than 3, change the dimension of pm25wc_ges(naero_smoke_fv3)
     if (n_smoke_var /= naero_smoke_fv3) then
        if (n_smoke_var < naero_smoke_fv3) then
           write(6,*) 'setuppm2_5: not all smoke aerosols in anavinfo',n_smoke_var,naero_smoke_fv3
           call stop2(451)
        endif
     endif

     do i=1,naero_smoke_fv3
        aeroname=aeronames_smoke_fv3(i)
        call gsi_chemguess_get ('var::'//trim(aeroname), ipm2_5, ier )
        if (ier > 0 .or. ipm2_5 <= 0) then
           write(6,*) 'setuppm2_5: ',trim(aeroname),' missing in anavinfo'
           call stop2(452)
        endif
     enddo

     if (size(gsi_chemguess_bundle)==nfldsig) then
        aeroname='smoke'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           allocate(ges_pm2_5(size(rank3,1),size(rank3,2),size(rank3,3),&
                nfldsig))
           ges_pm2_5(:,:,:,1)=rank3
           allocate(pm25wc(size(rank3,1),size(rank3,2),size(rank3,3),naero_smoke_fv3,nfldsig))
           pm25wc(:,:,:,1,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=rank3
              pm25wc(:,:,:,1,ifld)=rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chembundle,ier= ',ier
           call stop2(453)
        endif

        do i=2,naero_smoke_fv3-1 ! remove contribution from coarsepm
           aeroname=trim(aeronames_smoke_fv3(i))
           call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
           pm25wc(:,:,:,i,1)=rank3
           if (ier==0) then
             ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3
             do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
               ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3
               pm25wc(:,:,:,i,ifld)=rank3
             enddo
           else
             write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chembundle,ier= ',ier
             call stop2(453)
           end if
        end do
     else
       write(6,*) 'setuppm2_5: size(gsi_chemguess_bundle)/=nfldsig ges_pm2_5 not setup !!!'
       call stop2(454)
     end if ! eq. nfldsig

  endif 

  if (fv3_cmaq_regional .and. laeroana_fv3cmaq) then
!check if pm25at, pm25ac and pm25co are in ges 
     call gsi_chemguess_get ('var::pm25at', ipm2_5, ier )
     if (ipm2_5 <= 0) then
        write(6,*)'pm25at not in chem_guess - returning from setuppm2_5'
        return
     else 
         write(6,*)'pm25at is in chem_guess'
     endif

     call gsi_chemguess_get ('var::pm25at', ipm2_5, ier )
     if (ipm2_5 <= 0) then
        write(6,*)'pm25ac not in chem_guess - returning from setuppm2_5'
        return
     else 
        write(6,*)'pm25ac is in chem_guess'
     endif

     call gsi_chemguess_get ('var::pm25co', ipm2_5, ier )
     if (ipm2_5 <= 0) then
        write(6,*)'pm25co not in chem_guess - returning from setuppm2_5'
        return
     else
        write(6,*)'pm25co is in chem_guess'
     endif

!check if aerosol species in control
     call gsi_chemguess_get ( 'aerosols::3d', n_cmaq_var, ier )

!n_cmaq_var ges vars in anainfo; naero_cmaq_fv3 in chemmod

     if (n_cmaq_var /= naero_cmaq_fv3) then
        if (n_cmaq_var < naero_cmaq_fv3) then       
           write(6,*) 'setuppm2_5: not all cmaq aerosols in anavinfo',n_cmaq_var,naero_cmaq_fv3
           call stop2(451)
        endif
     endif

     do i=1,naero_cmaq_fv3
        aeroname=aeronames_cmaq_fv3(i)
        call gsi_chemguess_get ('var::'//trim(aeroname), ipm2_5, ier )
        if (ier > 0 .or. ipm2_5 <= 0) then
           write(6,*) 'setuppm2_5: ',trim(aeroname),' missing in anavinfo'
           call stop2(452)
        endif
     enddo

     if (size(gsi_chemguess_bundle)==nfldsig) then

        aeroname='pm25at'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           allocate(pm25wc(size(rank3,1),size(rank3,2),size(rank3,3),3,nfldsig))
           pm25wc(:,:,:,1,1)=rank3

           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              pm25wc(:,:,:,1,ifld)=rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle,ier= ',ier
           call stop2(453)
        endif

        aeroname='pm25ac'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           pm25wc(:,:,:,2,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              pm25wc(:,:,:,2,ifld)=rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle,ier= ',ier
           call stop2(453)
        endif

        aeroname='pm25co'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           pm25wc(:,:,:,3,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              pm25wc(:,:,:,3,ifld)=rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle,ier= ',ier
           call stop2(453)
        endif

        aeroname='aso4i'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           allocate(ges_pm2_5(size(rank3,1),size(rank3,2),size(rank3,3),&
                nfldsig))
           ges_pm2_5(:,:,:,1)=pm25wc(:,:,:,1,1)*rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=pm25wc(:,:,:,1,ifld)*rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle,ier= ',ier
           call stop2(453)
        endif
        !!!
        do i=2,naero_cmaq_fv3
           aeroname=trim(aeronames_cmaq_fv3(i))
           call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
           if (ier==0) then
             ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+pm25wc(:,:,:,imodes_cmaq_fv3(i),1)*rank3
             do ifld=2,nfldsig
               call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
               ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+pm25wc(:,:,:,imodes_cmaq_fv3(i),ifld)*rank3
             enddo
           else
             write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle,ier= ',ier
             call stop2(453)
           end if
        end do
     else 
       write(6,*) 'setuppm2_5: size(gsi_chemguess_bundle)/=nfldsig ges_pm2_5 not setup !!!'
       call stop2(454)
     end if ! eq. nfldsig
  end if

  if (wrf_mass_regional .and. laeroana_gocart) then

!check if aerosol species in control

     call gsi_chemguess_get ( 'aerosols::3d', n_gocart_var, ier )

     if (n_gocart_var /= naero_gocart_wrf) then
        write(6,*) 'setuppm2_5: not all gocart aerosols in anavinfo'
        call stop2(451)
     endif

     do i=1,naero_gocart_wrf
        aeroname=upper2lower(aeronames_gocart_wrf(i))
        call gsi_chemguess_get ('var::'//trim(aeroname), ipm2_5, ier )
        if (ier > 0 .or. ipm2_5 <= 0) then
           write(6,*) 'convinfo: ',trim(aeroname),' missing in anavinfo'
           call stop2(452)
        endif
     enddo

     if (size(gsi_chemguess_bundle)==nfldsig) then
        aeroname='bc1'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           allocate(ges_pm2_5(size(rank3,1),size(rank3,2),size(rank3,3),&
                nfldsig))
           ges_pm2_5(:,:,:,1)=rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='bc2'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='sulf'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3*nh4_mfac
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3*nh4_mfac
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='p25'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='oc1'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3*oc_mfac
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3*oc_mfac
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='oc2'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3*oc_mfac
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3*oc_mfac
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='seas1'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='seas2'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3*s_2_5
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3*s_2_5
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='dust1'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

        aeroname='dust2'
        call gsi_bundlegetpointer(gsi_chemguess_bundle(1),trim(aeroname),&
             rank3,ier)
        if (ier==0) then
           ges_pm2_5(:,:,:,1)=ges_pm2_5(:,:,:,1)+rank3*d_2_5
           do ifld=2,nfldsig
              call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),trim(aeroname),rank3,ier)
              ges_pm2_5(:,:,:,ifld)=ges_pm2_5(:,:,:,ifld)+rank3*d_2_5
           enddo
        else
           write(6,*) 'setuppm2_5: ',trim(aeroname),' not found in chem bundle, ier= ',ier
           call stop2(453)
        endif

     else
        write(6,*) 'setuppm2_5: inconsistent vector sizes (nfldsig,size(chemguess_bundle) ',&
             nfldsig,size(gsi_chemguess_bundle)
        call stop2(420)
     endif

  endif


! initialize arrays

  read(lunin)data,luse,ioid

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
     if (netcdf_diag) call init_netcdf_diag_
  end if
  mm1=mype+1


  call dtime_setup()
  
  if (trim(isis)=='TEOM') then

     do i=1,nobs
        
        dtime=data(itime,i)

        call dtime_check(dtime, in_curbin, in_anybin)
        if(.not.in_anybin) then 
           if (oneobtest_chem) then
              write(6,*)'oneobtest_chem outside time window'
              call stop2(414)
           else
              cycle
           endif
        endif

        if (nobs_bins > 1) then
           ibin = nint( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif

        if (ibin < 1 .or. ibin > nobs_bins) then
          call die(myname,'unexpected index, (nobs_bins,ibin) =',(/nobs_bins,ibin/))
        endif

        if (luse_obsdiag) my_diagLL => odiagLL(ibin)
        
!    link obs to diagnostics structure
        if (luse_obsdiag) then
           my_diag => obsdiagLList_nextNode(my_diagLL   ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = 1              ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

           if(.not.associated(my_diag)) call die(myname, &
                'obsdiagLList_nextNode(), create =', .not.lobsdiag_allocated)
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
        
!obs are conc, kg/M3
!wrf state vars are as mix ratio
!cmaq state vars are as mix ratio too,ug/Kg 
!convert for cmaq as well


        if (wrf_mass_regional .or. fv3_cmaq_regional .or. laeroana_fv3smoke) then
           call tintrp2a11(ges_ps,ps_ges,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
           call tintrp2a11(ges_tv(:,:,1,nfldsig),tv_ges,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
           conc=conc/(ps_ges*r1000/(rd*tv_ges))
        endif
!
        if (laeroana_fv3smoke) then
          if (.not. allocated(veg_type)) then 
             print*,"VEG_TYPE NOT ALLOCATED, WILL NOT BE USED IN PM2.5 DA FOR RRFS_SD",mype 
          else
             call intrp2a11(veg_type(:,:,1),veg_type_ges,dlat,dlon,mype)
          endif
        endif

!if elevobs is known than calculate difference otherwise
!assume that difference is acceptable
        
        if (elevobs > elev_missing) then
           elevdiff=abs(elevobs-elevges)
        else
           elevdiff=zero
!if elevation unknown include observation nevertheless
        endif
        
        if (oneobtest_chem) then
           call tintrp2a11(ges_pm2_5,pm2_5ges,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
           if (jiter==1) then
              innov = min(maginnov_chem,cgross(ikx))
              conconeobs=pm2_5ges+innov
              conc=conconeobs
           else
              conc=conconeobs
              innov=conc - pm2_5ges
           endif

        else
           call tintrp2a11(ges_pm2_5,pm2_5ges,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
           innov = conc - pm2_5ges
           if (laeroana_fv3smoke) then
              if ( veg_type_ges == 13.0_r_kind ) then 
                 if (abs(innov) < pm2_5_urban_innov_threshold) then
                    muse(i)=.false.
                 end if 
              else 
                 if (abs(innov) < pm2_5_innov_threshold) then
                    muse(i)=.false.
                 end if  
              end if

              if (pm2_5ges < pm2_5_bg_threshold) then 
                 muse(i)=.false.
              end if
              if (tv_ges-273.15_r_kind < 5.0_r_kind) then
                 muse(i)=.false.
              end if

           end if
        end if

        if ( fv3_cmaq_regional .and. laeroana_fv3cmaq) then
          ! interpoloate pm25ac
          call tintrp2a11(pm25wc(:,:,:,1,nfldsig),pm25wc_ges(1),dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig) 
          call tintrp2a11(pm25wc(:,:,:,2,nfldsig),pm25wc_ges(2),dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
          call tintrp2a11(pm25wc(:,:,:,3,nfldsig),pm25wc_ges(3),dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
        elseif (laeroana_fv3smoke) then
          call tintrp2a11(pm25wc(:,:,:,1,nfldsig),pm25wc_ges(1),dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
          call tintrp2a11(pm25wc(:,:,:,2,nfldsig),pm25wc_ges(2),dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)
          if (pm25wc_ges(1) >= pm2_5_bg_threshold) then
            pm25wc_ges(1)=1.0_r_kind
          else
            pm25wc_ges(1)=0.0_r_kind
          end if
          if (pm25wc_ges(2) >= pm2_5_bg_threshold) then
            pm25wc_ges(2)=1.0_r_kind
          else
            pm25wc_ges(2)=0.0_r_kind
          end if
          if ( (pm25wc_ges(1)+pm25wc_ges(2)) < 1.0_r_kind ) then
            muse(i) = .false. 
          end if
        else
          pm25wc_ges = 0.0_r_kind
        end if

        if (oneobtest_chem) then
           pm25wc_ges=1.0_r_kind
           muse(i) = .true.
        end if

        error=one/data(ierror,i)
        ratio_errors=one/sqrt(real(dup(i)))
        innov_error = error*innov
        
        if (abs(innov) > cgross(ikx) .or. &
              conc > pm2_5_teom_max  .or. &
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
              cg_pm2_5=cvar_b(ikx)
              wgross = cg_term*cvar_pg(ikx)/(cg_pm2_5*wnotgross)
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

        if (luse_obsdiag) then
           call obsdiagNode_set(my_diag, wgtjo=(error*ratio_errors)**2, &
                jiter=jiter,muse=muse(i),nldepart=innov)
        endif

        if (.not. last .and. muse(i)) then
           
           allocate(my_head)
           call pm2_5Node_appendto(my_head,pm2_5head(ibin))

           my_head%idv = is
           my_head%iob = ioid(i)
           my_head%elat= data(ilate,i)
           my_head%elon= data(ilone,i)
           
           call get_ij(mm1,dlat,dlon,&
                 my_head%ij,tempwij)

           my_head%ij (5:8)=my_head%ij(1:4)
           my_head%wij(1:4)=tempwij
           my_head%wij(5:8)=zero
           my_head%res     = innov
           my_head%err2    = error**2
           my_head%raterr2 = ratio_errors**2
           my_head%time    = dtime
           my_head%b       = cvar_b(ikx)
           my_head%pg      = cvar_pg(ikx)
           my_head%luse    = luse(i)
           my_head%pm25wc  = pm25wc_ges

           if (luse_obsdiag) then
              call obsdiagNode_assert(my_diag, my_head%idv,my_head%iob,1,myname,'my_diag:my_head')
              my_head%diags  => my_diag
           endif

           my_head => null()
        endif
        
! save select output for diagnostic file
        if (conv_diagsave) then

           ii=ii+1

           call tintrp2a11(ges_ps,ps_ges,dlat,dlon,dtime,hrdifsig,&
                mype,nfldsig)

           ps_ges=ps_ges*r10 ! convert from cb to hpa

           write(station_id,'(Z8)')nint(site_id)

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
 
           if (binary_diag) call contents_binary_diag_(my_diag)
           if (netcdf_diag) call contents_netcdf_diag_(my_diag)

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
  if(conv_diagsave) then
     if(netcdf_diag) call nc_diag_write
     if(binary_diag .and.ii>0) then
        write(7)cvar,nchar,nrealdiag,ii,mype,nrealdiag
        write(7)cdiagbuf(1:ii),rdiagbuf(:,1:ii)
        deallocate(cdiagbuf,rdiagbuf)
     end if
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
  if (  (fv3_cmaq_regional .and. .not.laeroana_fv3cmaq) .or. cmaq_regional .or. (wrf_mass_regional .and. wrf_pm2_5) ) then
     call gsi_chemguess_get ('var::pm2_5', ivar, istatus )
  else if (wrf_mass_regional .and. laeroana_gocart) then

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

  subroutine init_netcdf_diag_
  character(len=80) string
  character(len=128) diag_conv_file
  integer(i_kind) ncd_fileid,ncd_nobs
  logical append_diag
  logical,parameter::verbose=.false. 
     write(string,900) jiter
900  format('conv_pm2_5_',i2.2,'.nc4')
     diag_conv_file=trim(dirname) // trim(string)

     inquire(file=diag_conv_file, exist=append_diag)

     if (append_diag) then
        call nc_diag_read_init(diag_conv_file,ncd_fileid)
        ncd_nobs = nc_diag_read_get_dim(ncd_fileid,'nobs')
        call nc_diag_read_close(diag_conv_file)

        if (ncd_nobs > 0) then
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists.  Appending.  nobs,mype=',ncd_nobs,mype
        else
           if(verbose) print *,'file ' // trim(diag_conv_file) // ' exists but contains no obs.  Not appending. nobs,mype=',ncd_nobs,mype
           append_diag = .false. ! if there are no obs in existing file, then do not try to append
        endif
     end if

     call nc_diag_init(diag_conv_file, append=append_diag)

     if (.not. append_diag) then ! don't write headers on append - the module will break?
        call nc_diag_header("date_time",ianldate )
     endif
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
           cdiagbuf(ii)    = station_id         ! station id

           rdiagbuf(1,ii)  = ictype(ikx)        ! observation type

           rdiagbuf(2,ii)  = icsubtype(ikx)     ! observation subtype
           
           rdiagbuf(3,ii)  = data(ilate,i)      ! observation latitude (degrees)
           rdiagbuf(4,ii)  = data(ilone,i)      ! observation longitude (degrees)
           rdiagbuf(5,ii)  = data(ielev,i)      ! station elevation (meters)
           rdiagbuf(6,ii)  = ps_ges             ! observation pressure (hpa)
           rdiagbuf(7,ii)  = data(ielev,i)      ! observation height (meters)
           rdiagbuf(8,ii)  = dtime-time_offset  ! obs time (hours relative to analysis time)
           
           rdiagbuf(9,ii)  = zero !data(iqc,i) input prepbufr qc or event mark
           rdiagbuf(10,ii) = zero !data(iqt,i) setup qc or event mark (currently qtflg only)
           rdiagbuf(11,ii) = one       ! read_prepbufr data usage flag
           if(muse(i)) then
              rdiagbuf(12,ii) = one            ! analysis usage flag (1=use, -1=not used)
           else
              rdiagbuf(12,ii) = -one
           endif
           
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
                 if (odiag%muse(jj)) then
                    rdiagbuf(idia,ii) = one
                 else
                    rdiagbuf(idia,ii) = -one
                 endif
              enddo
              
              do jj=1,miter+1
                 idia=idia+1
                 rdiagbuf(idia,ii) = odiag%nldepart(jj)
              enddo

              do jj=1,miter
                 idia=idia+1
                 rdiagbuf(idia,ii) = odiag%tldepart(jj)
              enddo

              do jj=1,miter
                 idia=idia+1
                 rdiagbuf(idia,ii) = odiag%obssen(jj)
              enddo
           endif
  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_(odiag)
  type(obs_diag),pointer,intent(in):: odiag
! Observation class
  character(7),parameter     :: obsclass = '  pm2_5'
  real(r_kind),dimension(miter) :: obsdiag_iuse
           call nc_diag_metadata("Station_ID",              station_id             )
           call nc_diag_metadata("Observation_Class",       obsclass               )
           call nc_diag_metadata("Observation_Type",        ictype(ikx)            )
           call nc_diag_metadata("Observation_Subtype",     icsubtype(ikx)         )
           call nc_diag_metadata("Latitude",                data(ilate,i)          )
           call nc_diag_metadata("Longitude",               data(ilone,i)          )
           call nc_diag_metadata("Station_Elevation",       data(ielev,i)          )
           call nc_diag_metadata("Station_Veg_Type",        veg_type_ges           )
           call nc_diag_metadata("Pressure",                ps_ges                 )
           call nc_diag_metadata("Height",                  data(ielev,i)          )
           call nc_diag_metadata("Time",                    dtime-time_offset      )
           call nc_diag_metadata("Prep_QC_Mark",            zero                   )
           call nc_diag_metadata("Prep_Use_Flag",           one                    )
!          call nc_diag_metadata("Nonlinear_QC_Var_Jb",     var_jb                 )
           call nc_diag_metadata("Nonlinear_QC_Rel_Wgt",    rwgt                   )                 
           if(muse(i)) then
              call nc_diag_metadata("Analysis_Use_Flag",    one                    )
           else
              call nc_diag_metadata("Analysis_Use_Flag",    -one                   )              
           endif

           call nc_diag_metadata("Errinv_Input",            errinv_input           )
           call nc_diag_metadata("Errinv_Adjust",           errinv_adjst           )
           call nc_diag_metadata("Errinv_Final",            errinv_final           )

           call nc_diag_metadata("Observation",                   data(iconc,i)    )
           call nc_diag_metadata("Obs_Minus_Forecast_adjusted",   innov            )
           call nc_diag_metadata("Obs_Minus_Forecast_unadjusted", innov            )
 
           if (lobsdiagsave) then
              do jj=1,miter
                 if (odiag%muse(jj)) then
                       obsdiag_iuse(jj) =  one
                 else
                       obsdiag_iuse(jj) = -one
                 endif
              enddo
   
              call nc_diag_data2d("ObsDiagSave_iuse",     obsdiag_iuse                             )
              call nc_diag_data2d("ObsDiagSave_nldepart", odiag%nldepart )
              call nc_diag_data2d("ObsDiagSave_tldepart", odiag%tldepart )
              call nc_diag_data2d("ObsDiagSave_obssen",   odiag%obssen   )             
           endif
   
  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_tv)) deallocate(ges_tv)
    if(allocated(ges_pm2_5)) deallocate(ges_pm2_5)
    if(allocated(ges_z )) deallocate(ges_z )
    if(allocated(ges_ps)) deallocate(ges_ps)
  end subroutine final_vars_

end subroutine setuppm2_5
end module pm2_5_setup
