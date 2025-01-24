module colvk_setup
  implicit none
  private
  public:: setup
        interface setup; module procedure setupco; end interface

contains
subroutine setupco(obsLL,odiagLL,lunin,mype,stats_co,nlevs,nreal,nobs,&
     obstype,isis,is,co_diagsave,init_pass)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setupco --- Compute rhs of oi for sbuv co obs
!
!   prgrmmr:     parrish          org: np22                date: 1990-10-06
!
! abstract:      For sbuv ozone observations (layer amounts and total 
!                column, this routine 
!                  a) reads obs assigned to given mpi task (geographic region),
!                  b) simulates obs from guess,
!                  c) apply some quality control to obs,
!                  d) load weight and innovation arrays used in minimization
!                  e) collects statistics for runtime diagnostic output
!                  f) writes additional diagnostic information to output file
!
! program history log:
!   1990-10-06  parrish
!   2010-04-01  tangborn - created from Parrish et al. setupoz.f90
!   2010-05-29  todling - add ihave-co check; revisit treatment of guess
!   2013-01-26  parrish - change from grdcrd to grdcrd1, tintrp2a to tintrp2a1, tintrp2a1,
!                          tintrp3 to tintrp31v, intrp2a to intrp2a1. (for successful debug compile on WCOSS)
!   2013-10-19  todling - metguess now holds background
!   2013-11-26  guo     - removed nkeep==0 escaping to allow more than one obstype sources.
!   2014-12-30  derber - Modify for possibility of not using obsdiag
!   2015-10-01  guo   - full res obvsr: index to allow redistribution of obsdiags
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-06-24  guo     - fixed the default value of obsdiags(:,:)%tail%luse to luse(i)
!                       . removed (%dlat,%dlon) debris.
!   2017-02-09  guo     - Remove m_alloc, n_alloc.
!                       . Remove my_node with corrected typecast().
!
!   input argument list:
!     lunin          - unit from which to read observations
!     mype           - mpi task id
!     nlevs          - number of levels (layer amounts + total column) per obs   
!                     for MOPITT CO, this should just be number of averaging 
!                      kernel levels, which is the same as the number of obs levels. (AVT)
!     nreal          - number of pieces of non-co info (location, time, etc) per obs
!     nobs           - number of observations
!     isis           - sensor/instrument/satellite id
!     is             - integer(i_kind) counter for number of obs types to process
!     obstype        - type of co obs
!     ozone_diagsave - switch on diagnostic output (.false.=no output)
!     stats_oz       - sums for various statistics as a function of level
!
!   output argument list:
!     stats_oz       - sums for various statistics as a function of level
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP; SGI Origin 2000; Compaq HP
!
!$$$ end documentation block
     
! !USES:

  use mpeu_util, only: die,perr
  use kinds, only: r_kind,r_single,i_kind

  use constants, only : zero,half,one,two,tiny_r_kind
  use constants, only : cg_term,wgtlim,h300   ! AVT need to find value for co
                                                     ! use the ozone values for the moment
  use m_obsdiagNode, only : obs_diag
  use m_obsdiagNode, only : obs_diags
  use m_obsdiagNode, only : obsdiagLList_nextNode
  use m_obsdiagNode, only : obsdiagNode_set
  use m_obsdiagNode, only : obsdiagNode_get
  use m_obsdiagNode, only : obsdiagNode_assert

  use obsmod, only : dplat,nobskeep
  use obsmod, only : mype_diaghdr,dirname,time_offset,ianldate
  use obsmod, only : lobsdiag_allocated,lobsdiagsave
  use obsmod, only: dirname
  use nc_diag_write_mod, only: nc_diag_init, nc_diag_header, nc_diag_metadata, &
       nc_diag_write, nc_diag_data2d
  use nc_diag_read_mod, only: nc_diag_read_init, nc_diag_read_get_dim, nc_diag_read_close
  use m_obsNode, only: obsNode
  use m_colvkNode, only : colvkNode
  use m_colvkNode, only : colvkNode_appendto
  use m_obsLList , only : obsLList
  use m_obsLList , only : obsLList_tailNode
  use obsmod, only : luse_obsdiag

  use gsi_4dvar, only: nobs_bins,hr_obsbin

  use gridmod, only : get_ij,nsig

  use guess_grids, only : nfldsig,ges_prsi,ntguessig,hrdifsig
  use gsi_bundlemod, only : gsi_bundlegetpointer
  use gsi_chemguess_mod, only : gsi_chemguess_get,gsi_chemguess_bundle

  use coinfo, only : jpch_co,error_co,pob_co,gross_co,nusis_co
  use coinfo, only : iuse_co,b_co,pg_co

  use jfunc, only : jiter,last,miter
  
  use m_dtime, only: dtime_setup, dtime_check
  implicit none
  
! !INPUT PARAMETERS:
  type(obsLList ),target,dimension(:),intent(in):: obsLL
  type(obs_diags),target,dimension(:),intent(in):: odiagLL

  integer(i_kind)                  , intent(in   ) :: lunin  ! unit from which to read observations
  integer(i_kind)                  , intent(in   ) :: mype   ! mpi task id
  integer(i_kind)                  , intent(in   ) :: nlevs  ! number of levels (layer amounts + total column) per obs   
                                                             ! layer amounts only for CO 
  integer(i_kind)                  , intent(in   ) :: nreal  ! number of pieces of non-co info (location, time, etc) per obs
  integer(i_kind)                  , intent(in   ) :: nobs   ! number of observations
  character(20)                    , intent(in   ) :: isis   ! sensor/instrument/satellite id
  integer(i_kind)                  , intent(in   ) :: is     ! integer(i_kind) counter for number of obs types to process

  character(10)                    , intent(in   ) :: obstype          ! type of co obs
  logical                          , intent(in   ) :: co_diagsave   ! switch on diagnostic output (.false.=no output)
  logical                          , intent(in   ) :: init_pass     ! state of "setup" processing

! !INPUT/OUTPUT PARAMETERS:

  real(r_kind),dimension(9,jpch_co), intent(inout) :: stats_co ! sums for various statistics as 
                                                               ! a function of level
!-------------------------------------------------------------------------

! Declare local parameters  
  integer(i_kind),parameter:: iint=1
  integer(i_kind),parameter:: ireal=3
  real(r_kind),parameter:: r10=10.0_r_kind
  character(len=*),parameter:: myname="setupco"

! Declare local variables  
  
  real(r_kind) wk1,wk2 
  real(r_kind) coobs,omg,rat_err2,dlat,dtime,dlon
  real(r_kind) cg_co,wgross,wnotgross,wgt,arg,exp_arg,term
  real(r_kind) psi,errorinv
  real(r_kind),dimension(nlevs):: coges,coakl,varinv3,co_inv
  real(r_kind),dimension(nlevs):: ratio_errors,error
!  real(r_kind),dimension(nlevs-1):: ozp
  real(r_kind),dimension(nlevs):: cop  ! nlevs=10 for MOPITT (AVT)
  real(r_kind),dimension(nlevs,nlevs):: avk ! CO averaging kernel
  real(r_kind),dimension(nlevs):: coap  ! CO apriori 

  real(r_kind),dimension(nlevs):: pobs,gross,tnoise
  real(r_kind),dimension(nreal+nlevs,nobs):: data
  real(r_kind),dimension(nsig+1)::prsitmp
  real(r_single),dimension(nlevs):: pob4,grs4,err4
  real(r_single),dimension(ireal,nobs):: diagbuf
  real(r_single),allocatable,dimension(:,:,:)::rdiagbuf
  real(r_kind),allocatable,dimension(:,:,:,:):: ges_co
  

  integer(i_kind) i,nlev,ii,jj,iextra,ibin
  integer(i_kind) k,j,nz,jc,idia,irdim1,ier,istatus,k1,k2 
  integer(i_kind) ioff,itoss,ikeep,ierror_toq,ierror_poq
  integer(i_kind) isolz
  integer(i_kind) mm1,itime,ilat,ilon,isd,ilate,ilone,itoq,ipoq
  integer(i_kind),dimension(iint,nobs):: idiagbuf
  integer(i_kind),dimension(nlevs):: ipos,iouse

  real(r_kind),dimension(4):: tempwij
  integer(i_kind) nlevp
  logical,parameter::debug=.false.
  
  character(12) string
  character(10) filex
  character(128) diag_co_file

  logical,dimension(nobs):: luse
  integer(i_kind),dimension(nobs):: ioid ! initial (pre-distribution) obs ID
  logical:: l_may_be_passive,proceed

  logical:: in_curbin, in_anybin
  type(colvkNode),pointer:: my_head
  type(obs_diag),pointer:: my_diag
  type(obs_diags),pointer:: my_diagLL

  type(obsLList),pointer,dimension(:):: colvkhead
  colvkhead => obsLL(:)

! Check to see if required guess fields are available
  call check_vars_(proceed)
  if(.not.proceed) return  ! not all vars available, simply return

! If require guess vars available, extract from bundle ...
  call init_vars_

  mm1=mype+1
!
!*********************************************************************************

! Initialize arrays
  do j=1,nlevs
     ipos(j)=0
     iouse(j)=-2
     tnoise(j)=1.e10_r_kind
     gross(j)=1.e10_r_kind
     pobs(j)=1.e10_r_kind
  end do


! Locate data for satellite in coinfo arrays
  itoss =1
  l_may_be_passive=.false.
  jc=0
  if(debug) print*,'jpch_co=',jpch_co,'nlevs=',nlevs
  do j=1,jpch_co
     if(debug) print*,'isis=',isis,'nusis_co(j)=',nusis_co(j)
     if (isis == nusis_co(j)) then
        jc=jc+1
        if (jc > nlevs) then
           write(6,*)'SETUPCO:  ***ERROR*** in level numbers, jc,nlevs=',jc,nlevs,&
                ' ***STOP IN SETUPCO***'
           call stop2(71)
        endif
        ipos(jc)=j

        iouse(jc)=iuse_co(j)
        tnoise(jc)=error_co(j)
        if(debug) print*,'error_co=',error_co(j)
        if(debug) print*,'gross=',gross(jc)
        gross(jc)=min(r10*gross_co(j),h300)
!  Need to find out what to do here for CO (AVT).
        if (obstype == 'sbuv2' ) then
           pobs(jc)=pob_co(j) * 1.01325_r_kind
        else
           pobs(jc)=pob_co(j)
        endif

        if(debug) print*,'pobs=',pobs(jc)
        if(debug) print*,'iouse(jc)=',iouse(jc)
        if(debug) print*,'co_diagsave=',co_diagsave
        if(debug) print*,'tnoise=',tnoise(jc)
       
!        if (iouse(jc)<-1 .or. (iouse(jc)==-1 .and. &
!             .not.co_diagsave)) then
!           tnoise(jc)=1.e10_r_kind
!           gross(jc) =1.e10_r_kind
!           pobs(jc)  = zero
!        endif
        if (iouse(jc)>-1) l_may_be_passive=.true.
        if (tnoise(jc)<1.e4_r_kind) itoss=0
 
        if(debug) print*,'tnoise(jc)=',tnoise(jc)
     endif
  end do
  nlev=jc

! Handle error conditions
  if (nlevs>nlev) write(6,*)'SETUPCO:  level number reduced for ',obstype,' ', &
       nlevs,' --> ',nlev
  if (nlev == 0 .or. itoss==1)then
     if (nlev == 0) then
        if (mype==0) write(6,*)'SETUPCO:  no levels found for ',isis
     end if
     if (itoss==1) then
        if (mype==0) write(6,*)'SETUPCO:  all obs variances > 1.e4.  Do not use ',&
             'data from satellite ',isis
     end if
     if (nobs>0) read(lunin)

!    Release memory of local guess arrays
     call final_vars_

     return
  endif

  if(co_diagsave)then
     irdim1=3
     if(lobsdiagsave) irdim1=irdim1+4*miter+1
     allocate(rdiagbuf(irdim1,nlevs,nobs))
  end if

! Read and transform co data
  read(lunin) data,luse,ioid

!    index information for data array (see reading routine)

  isd=1       ! index of satellite
  itime=2     ! index of analysis relative obs time
  ilon=3      ! index of grid relative obs location (x)
  ilat=4      ! index of grid relative obs location (y)
  ilone=5     ! index of earth relative longitude (degrees)
  ilate=6     ! index of earth relative latitude (degrees)
  ipoq=7      ! index of profile co error flag
  isolz=8     ! index of solar zenith angle   

      
! If requested, save data for diagnostic ouput
  if(co_diagsave)ii=0

! Convert observation (lat,lon) from earth to grid relative values
  call dtime_setup()
  do i=1,nobs
     dtime=data(itime,i)

     call dtime_check(dtime, in_curbin, in_anybin)
     if(.not.in_anybin) then
        deallocate(ges_co)
        return
     endif

if(in_curbin) then
     dlat=data(ilat,i)
     dlon=data(ilon,i)

!   AVT  What does this do????

     if(debug) print*,obstype
     if (obstype == 'mopitt' ) then
        if (nobskeep>0) then
           write(6,*)'setupco: nobskeep',nobskeep
           call stop2(259)
        end if

        ierror_toq = nint(data(itoq,i))
        ierror_poq = nint(data(ipoq,i))

!       Note:  cop as log(pobs)
        call intrp2a1(ges_prsi(1,1,1,ntguessig),prsitmp,dlat,&
          dlon,nsig+1,mype)
  
!       Map observation pressure to guess vertical coordinate
        psi=one/(prsitmp(1)*r10)  ! factor of 10 converts to hPa
        if(debug) print*,'nlevs=',nlevs
        do nz=1,nlevs
           if ((pobs(nz)*psi) < one) then
              cop(nz) = pobs(nz)/r10
           else
              cop(nz) = prsitmp(1)
           end if
           call grdcrd1(cop(nz),prsitmp,nsig+1,-1)
        enddo
! make any obs above surface pressure passive AVT (may need to revisit this)
        do nz=1,nlevs-1 
           if (cop(nz).eq.cop(nz+1))then 
               varinv3(nz)=zero
               ratio_errors(nz)=zero
               rat_err2 = zero
           endif 
        enddo 
     end if

!  MOPITT a priori and averaging kernel

      do k=1,nlev
        coap(k)=data(k+8,i)
      enddo 
      do k=1,nlev 
        do j=1,nlev 
          avk(k,j)=data(j+(k-1)*nlevs+8+nlevs,i)
        enddo 
      enddo 

!  interpolation output at ave ker levels is called coakl

     do k=1,nlev
        call tintrp3(ges_co,coakl(k),dlat,dlon,cop(k),dtime, &
           hrdifsig,1,mype,nfldsig)
     enddo

!  application of averaging kernel for mopitt co 

     call co_mop_ak(coakl,coges,nlevs,avk,coap)
        

     if(co_diagsave)then
        ii=ii+1
        idiagbuf(1,ii)=mype                  ! mpi task number
        diagbuf(1,ii) = data(ilate,i)        ! lat (degree)
        diagbuf(2,ii) = data(ilone,i)        ! lon (degree)
        diagbuf(3,ii) = data(itime,i)-time_offset ! time (hours relative to analysis)
     endif

!    Interpolate interface pressure to obs location
!    Calculate innovations, perform gross checks, and accumualte
!    numbers for statistics
     
     do k=1,nlev
        j=ipos(k)
        ioff=nreal+k

!       Compute innovation and load obs error into local array
        coobs = data(ioff,i)
        co_inv(k) = coobs-coges(k)
        error(k)  = tnoise(k)
        

!       Set inverse obs error squared and ratio_errors
        if (error(k)<1.e4_r_kind) then
           varinv3(k) = one/(error(k)**2)
           ratio_errors(k) = one
        else
           varinv3(k) = zero
           ratio_errors(k) = zero
        endif

!       Perform gross check
        if(abs(co_inv(k)) > gross(k) .or. coobs > 1000._r_kind .or. &
             coges(k)<tiny_r_kind) then
           varinv3(k)=zero
           ratio_errors(k)=zero
!          write(6,*)'SETUPCO:  reset CO varinv3=',varinv3(k)
           if(luse(i))stats_co(2,j) = stats_co(2,j) + one ! number of obs tossed
        endif

!       Accumulate numbers for statistics
        rat_err2 = ratio_errors(k)**2
        if (varinv3(k)>tiny_r_kind .or. &
             (iouse(k)==-1 .and. co_diagsave)) then
           if(luse(i))then
              omg=co_inv(k)
              if(debug) print*,'MOPITT O-G',omg
              stats_co(1,j) = stats_co(1,j) + one                          ! # obs
              stats_co(3,j) = stats_co(3,j) + omg                          ! (o-g)
              stats_co(4,j) = stats_co(4,j) + omg*omg                      ! (o-g)**2
              stats_co(5,j) = stats_co(5,j) + omg*omg*varinv3(k)*rat_err2  ! penalty
              stats_co(6,j) = stats_co(6,j) + coobs                        ! obs
 
              exp_arg = -half*varinv3(k)*omg**2
              errorinv = sqrt(varinv3(k))
              if (pg_co(j) > tiny_r_kind .and. errorinv > tiny_r_kind) then
                 arg  = exp(exp_arg)
                 wnotgross= one-pg_co(j)
                 cg_co=b_co(j)*errorinv
                 wgross = cg_term*pg_co(j)/(cg_co*wnotgross)
                 term = log((arg+wgross)/(one+wgross))
                 wgt  = one-wgross/(arg+wgross)
              else
                 term = exp_arg
                 wgt  = one
              endif
              stats_co(8,j) = stats_co(8,j) -two*rat_err2*term
              if(wgt < wgtlim) stats_co(9,j)=stats_co(9,j)+one
           end if
        endif

!       If not assimilating this observation, reset inverse variance to zero
        if (iouse(k)<1) then
           varinv3(k)=zero
           ratio_errors(k)=zero
           rat_err2 = zero
        end if
        if (rat_err2*varinv3(k)>tiny_r_kind .and. luse(i)) &
           stats_co(7,j) = stats_co(7,j) + one

!       Optionally save data for diagnostics
        if (co_diagsave) then
           rdiagbuf(1,k,ii) = coobs
           rdiagbuf(2,k,ii) = co_inv(k)           ! obs-ges
           rdiagbuf(3,k,ii) = varinv3(k)*rat_err2    ! inverse (obs error )**2
        endif

     end do
!    Check all information for obs.  If there is at least one piece of
!    information that passed quality control, use this observation.
     ikeep=0
     do k=1,nlevs
        if(debug) print*,'ratio_errors=',ratio_errors(k)
        if(debug) print*,'varinv3=',varinv3(k)
        if(debug) print*,'ratio_errors(k)**2*varinv3(k)=',ratio_errors(k)**2*varinv3(k)
        if ((ratio_errors(k)**2)*varinv3(k)>1.e-10_r_kind) ikeep=1
     end do
endif   ! (in_curbin)
        if(debug) print*,'ikeep=',ikeep

!    In principle, we want ALL obs in the diagnostics structure but for
!    passive obs (monitoring), it is difficult to do if rad_diagsave
!    is not on in the first outer loop. For now we use l_may_be_passive...
     if(debug) print*,'l_may_be_passive=',l_may_be_passive
     if (l_may_be_passive) then
!       Link observation to appropriate observation bin
        if (nobs_bins>1) then
           ibin = NINT( dtime/hr_obsbin ) + 1
        else
           ibin = 1
        endif
        IF (ibin<1.OR.ibin>nobs_bins) write(6,*)mype,'Error nobs_bins,ibin= ',nobs_bins,ibin

        if(luse_obsdiag) my_diagLL => odiagLL(ibin)

        if(in_curbin) then
!       Process obs have at least one piece of information that passed qc checks
          if (.not. last .and. ikeep==1) then
   
             allocate(my_head)
             call colvkNode_appendto(my_head,colvkhead(ibin))

             my_head%idv = is
             my_head%iob = ioid(i)
             my_head%elat= data(ilate,i)
             my_head%elon= data(ilone,i)
   
             nlevp=max(nlev,1)
             if(luse_obsdiag) allocate(my_head%diags(nlev))
             allocate(my_head%res(nlev),&
                      my_head%err2(nlev),my_head%raterr2(nlev),&
                      my_head%prs(nlevp), &
                      my_head%wk(nsig), &
                      my_head%wij(8,nsig), &
                      my_head%ipos(nlev), & 
                      my_head%ak(nlev,nlev), &
                      my_head%ap(nlev), &
                      !my_head%wkk1(nlev), &
                      !my_head%wkk2(nlev), &
                      stat=istatus)
                      
             if (istatus/=0) write(6,*)'SETUPCO:  allocate error for co_point, istatus=',istatus
   
!            Set number of levels for this obs
!             oztail(ibin)%head%nloz = nlev-1  ! NOTE: for OMI/GOME, nloz=0
             my_head%nlco = nlev !  AVT: for MOPITT CO, nlev=10. No single level data
   
!            Set (i,j) indices of guess gridpoint that bound obs location
             call get_ij(mm1,dlat,dlon,my_head%ij,tempwij)
   
             call tintrp2a1(ges_prsi,prsitmp,dlat,dlon,dtime,hrdifsig,&
                  nsig+1,mype,nfldsig)
   
   
             do k = 1,nlevs
                k1=int(cop(k))
                k2=k1+1 
                wk1=one-(cop(k)-real(k1))/(real(k2)-real(k1))
                wk2=    (cop(k)-real(k1))/(real(k2)-real(k1)) 
                my_head%wk (  k)=wk1
                my_head%wij(1,k)=tempwij(1)*wk1
                my_head%wij(2,k)=tempwij(2)*wk1
                my_head%wij(3,k)=tempwij(3)*wk1
                my_head%wij(4,k)=tempwij(4)*wk1
                my_head%wij(5,k)=tempwij(1)*wk2
                my_head%wij(6,k)=tempwij(2)*wk2
                my_head%wij(7,k)=tempwij(3)*wk2
                my_head%wij(8,k)=tempwij(4)*wk2
                do j=1,nlevs
                   my_head%ak(k,j)=avk(k,j)
                enddo 
                my_head%ap(k)=coap(k)
             end do
   
!            Increment data counter and save information used in
!            inner loop minimization (int* and stp* routines)
   
             my_head%luse=luse(i)
             my_head%time=dtime
   
             if (obstype == 'mopitt' ) then
!                do k=1,nlevs-1
                do k=1,nlevs   ! AVT should just be nlevs for mopitt.
                   my_head%prs(k) = cop(k)
                enddo
             else
                my_head%prs(1) = zero   ! any value is OK, never used
             endif
   
             my_head => null()
          endif ! < .not.last >
        endif   ! (in_curbin)

!       Link obs to diagnostics structure
        do k=1,nlevs
           if(luse_obsdiag)then
             my_diag => obsdiagLList_nextNode(my_diagLL ,&
                create = .not.lobsdiag_allocated        ,&
                   idv = is             ,&
                   iob = ioid(i)        ,&
                   ich = k              ,&
                  elat = data(ilate,i)  ,&
                  elon = data(ilone,i)  ,&
                  luse = luse(i)        ,&
                 miter = miter          )

             if(.not.associated(my_diag)) call die(myname,      &
                'obsdiagLList_nextNode(), create =', .not.lobsdiag_allocated)
           endif

           if(in_curbin) then
              if(luse_obsdiag)then
                call obsdiagNode_set(my_diag, wgtjo=varinv3(k)*ratio_errors(k)**2, &
                        jiter=jiter,muse=(ikeep==1), nldepart=co_inv(k) )
              endif
   
              if (.not. last .and. ikeep==1) then
                 my_head => tailNode_typecast_(colvkhead(ibin))
                 if(.not.associated(my_head)) &
                    call die(myname,'unexpected, associated(my_head) =',associated(my_head))

                 my_head%idv        = is
                 my_head%iob        = ioid(i)

                 my_head%ipos(k)    = ipos(k)
                 my_head%res(k)     = co_inv(k)
                 my_head%err2(k)    = varinv3(k)
                 my_head%raterr2(k) = ratio_errors(k)**2

                 if(luse_obsdiag)then
                    call obsdiagNode_assert(my_diag,my_head%idv,my_head%iob,k,myname,'my_diag:my_head')
                    my_head%diags(k)%ptr => my_diag
                 endif
                 my_head => null()
             endif
   
             if (co_diagsave.and.lobsdiagsave) then
              associate( odiag => my_diag )
                idia=3
                do jj=1,miter
                   idia=idia+1
                   if (odiag%muse(jj)) then
                      rdiagbuf(idia,k,ii) = one
                   else
                      rdiagbuf(idia,k,ii) = -one
                   endif
                enddo
                do jj=1,miter+1
                   idia=idia+1
                   rdiagbuf(idia,k,ii) = odiag%nldepart(jj)
                enddo
                do jj=1,miter
                   idia=idia+1
                   rdiagbuf(idia,k,ii) = odiag%tldepart(jj)
                enddo
                do jj=1,miter
                   idia=idia+1
                   rdiagbuf(idia,k,ii) = odiag%obssen(jj)
                enddo
              end associate ! odiag
             endif
           endif        ! (in_curbin)

        enddo ! < over nlevs >

     else

        if(in_curbin) then
           if (co_diagsave.and.lobsdiagsave) then
              rdiagbuf(4:irdim1,1:nlevs,ii) = zero
           endif
        endif   ! (in_curbin)
 
     endif ! < l_may_be_passive >

  end do   ! end do i=1,nobs

! If requested, write to diagnostic file
  if (co_diagsave) then
     filex=obstype
     write(string,100) jiter
100  format('_',i2.2)
     diag_co_file = trim(dirname) // trim(filex) // '_' // trim(dplat(is)) // (string)
     if(init_pass) then
       open(4,file=diag_co_file,form='unformatted',status='unknown',position='rewind')
     else
       open(4,file=diag_co_file,form='unformatted',status='old',position='append')
     endif
     iextra=0
     if (init_pass.and.mype==mype_diaghdr(is)) then
        write(4) isis,dplat(is),obstype,jiter,nlevs,ianldate,iint,ireal,iextra
        write(6,*)'SETUPCO:   write header record for ',&
             isis,iint,ireal,iextra,' to file ',trim(diag_co_file),' ',ianldate
        do i=1,nlevs
           pob4(i)=pobs(i)
           grs4(i)=gross(i)
           err4(i)=tnoise(i)
        end do
        write(4) pob4,grs4,err4,iouse
     endif
     write(4) ii
     write(4) idiagbuf(:,1:ii),diagbuf(:,1:ii),rdiagbuf(:,:,1:ii)
     close(4)
  endif

! Release memory of local guess arrays
  call final_vars_

! clean up
  if(allocated(ges_co)) deallocate(ges_co)
  if(co_diagsave) deallocate(rdiagbuf)

! End of routine
  return
  contains
  function tailNode_typecast_(oll) result(ptr_)
!>  Cast the tailNode of oll to an colvkNode, as in
!>      ptr_ => typecast_(tailNode_(oll))

    use m_colvkNode, only: colvkNode, typecast_ => colvkNode_typecast
    use m_obsLList , only: obsLList , tailNode_ => obsLList_tailNode
    use m_obsNode  , only: obsNode
    implicit none
    type(colvkNode),pointer:: ptr_
    type(obsLList ),target ,intent(in):: oll

    class(obsNode),pointer:: inode_
    inode_ => tailNode_(oll)
    ptr_   => typecast_(inode_)
  end function tailNode_typecast_

  subroutine check_vars_ (proceed)
  logical,intent(inout) :: proceed
  integer(i_kind) ivar,istatus
! Check to see if required guess fields are available
  call gsi_chemguess_get ('var::co', ivar, istatus )
  proceed=ivar>0
  end subroutine check_vars_ 

  subroutine init_vars_

  real(r_kind),dimension(:,:,:),pointer:: rank3=>NULL()
  integer(i_kind) ifld

! If require guess vars available, extract from bundle ...
  if(size(gsi_chemguess_bundle)==nfldsig) then
     call gsi_bundlegetpointer(gsi_chemguess_bundle(1),'co',rank3,ier)
     if (ier==0) then
         if(allocated(ges_co))then
            write(6,*) 'setupco: ges_co already incorrectly alloc '
            call stop2(999)
         endif
         allocate(ges_co(size(rank3,1),size(rank3,2),size(rank3,3),nfldsig))
         ges_co(:,:,:,1)=rank3
         do ifld=2,nfldsig
            call gsi_bundlegetpointer(gsi_chemguess_bundle(ifld),'co',rank3,ier)
            ges_co(:,:,:,ifld)=rank3
         enddo
     else
         write(6,*) 'setupco: CO not found in chem bundle, ier= ',ier
         call stop2(999)
     endif
  else
     write(6,*) 'setupco: inconsistent vector sizes (nfldsig,size(chemguess_bundle) ',&
                 nfldsig,size(gsi_chemguess_bundle)
     call stop2(999)
  endif
  end subroutine init_vars_

  subroutine init_netcdf_diag_
  end subroutine init_netcdf_diag_
  subroutine contents_binary_diag_
  end subroutine contents_binary_diag_
  subroutine contents_netcdf_diag_
! Observation class
  character(7),parameter     :: obsclass = '     co'
  end subroutine contents_netcdf_diag_

  subroutine final_vars_
    if(allocated(ges_co)) deallocate(ges_co)
  end subroutine final_vars_

end subroutine setupco
end module colvk_setup
