subroutine genstats_gps(bwork,awork,toss_gps_sub,conv_diagsave,mype)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    genstats_gps    generate statistics for gps observations
!   prgmmr: treadon          org: np20                date: 2005-12-21
!
! abstract:  For gps observations, this routine
!              a) collects statistics for runtime diagnostic output
!              f) adjusts observation error ratio based on superobs factor
!
! program history log:
!   2005-12-21  treadon
!   2006-02-24  derber  - modify to take advantage of convinfo module
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!   2006-09-20  cucurull - replace superobs factor for obs in a top (non-full) layer 
!   2007-03-01  treadon - add array toss_gps
!   2007-03-19  tremolet - binning of observations
!   2007-06-21 cucurull - add conv_diagsave and mype in argument list; 
!                         modify qc and output for diagnostic file based on toss_gps
!                         print out diagnostic files if requested
!                         add wgtlim and huge_single in constants module
!   2008-02-27 cucurull - modify diagnostics output file
!   2008-04-14 treadon  - compute super_gps within this routine
!   2008-06-04 safford  - rm unused vars and uses
!   2008-09-05 lueken   - merged ed's changes into q1fy09 code
!   2008-25-08 todling  - adapt obs-binning change to GSI-May2008
!   2009-02-05 cucurull - modify latitude range four statistics output
!   2009-10-22     shen - add high_gps
!   2010-04-09 cucurull - fix several bugs for high_gps (diag information, counters, 
!                       - consider failure of gross check, obs-binning structures, QC for CL profiles)
!                       - reorganize high_gps structure
!                       - modify dimension of diagnostic structure
!   2010-07-23 treadon  - add ratio_error=zero to reqional QC block, replace (izero,ione) with (0,1),
!                         remove _i_kind suffix from integer constants, clean up use statements
!   2010-08-17 treadon  - convert high_gps from m to km one time only; break out regional
!                         QC as separate if/then block (global will bypass); replace 
!                         ratio_errors_reg with logical toss
!   2010-10-25 cucurull - add quality control options for C/NOFS satellite
!   2011-01-18 cucurull - increase the size of nreal and mreal by one element to 
!                         add gps_dtype information
!   2012-10-16 cucurull - increase the size of nreal and mreal by one element to
!                         add qrefges information, replace qcfail=5 by 4, add regional QC for MetOpB
!                         add dtype, dobs to distinguish use of toss_gps between ref/bending, add SR QC for obs
!   2014-01-28  todling - write sensitivity slot indicator (ioff) to header of diagfile
!   2014-12-13 derber   - minor optimization modifications
!   2015-07-28 cucurull - add QC for regional bending angle assimilation
!
!   input argument list:
!     toss_gps_sub  - array of qc'd profile heights
!     conv_diagsave - logical to save innovation diagnostics
!     mype          - mpi task id
!
!   output argument list:
!     bwork    - array containing information about obs-ges statistics
!     awork    - array containing information for data counts and gross checks
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_single
  use obsmod, only: gps_allhead,gps_allptr,nprof_gps,&
       destroy_genstats_gps,gpsptr,obs_diag,lobsdiagsave,luse_obsdiag
  use gridmod, only: nsig,regional
  use constants, only: tiny_r_kind,half,wgtlim,one,two,zero,five,four
  use qcmod, only: npres_print,ptop,pbot
  use mpimod, only: ierror,mpi_comm_world,mpi_rtype,mpi_sum,mpi_max
  use jfunc, only: jiter,miter
  use gsi_4dvar, only: nobs_bins
  use convinfo, only: nconvtype
  implicit none

! Declare passed variables
  logical                                          ,intent(in):: conv_diagsave
  integer(i_kind)                                  ,intent(in) :: mype
  real(r_kind),dimension(100+7*nsig)               ,intent(inout):: awork
  real(r_kind),dimension(npres_print,nconvtype,5,3),intent(inout):: bwork
  real(r_kind),dimension(max(1,nprof_gps))         ,intent(in):: toss_gps_sub

! Declare local parameters
  real(r_kind),parameter:: ten   = 10.0_r_kind
  real(r_kind),parameter:: six   = 6.0_r_kind
  real(r_kind),parameter:: r1em3 = 1.0e-3_r_kind
  real(r_kind),parameter:: r20   = 20.0_r_kind
  real(r_kind),parameter:: scale = 100.0_r_kind

! Declare local variables
  logical:: luse,muse,toss
  integer(i_kind):: k,jsig,icnt,khgt,kprof,ikx,nn,j,nchar,nreal,mreal,ii,ioff
  real(r_kind):: pressure,arg,wgross,wgt,term,cg_gps,valqc,elev,satid,dtype,dobs
  real(r_kind):: ress,val,ratio_errors,val2
  real(r_kind):: exp_arg,data_ikx,data_rinc,cg_term,rat_err2,elat
  real(r_kind):: wnotgross,data_ipg,data_ier,data_ib,factor,super_gps_up,rhgt
  real(r_kind),dimension(nsig,max(1,nprof_gps)):: super_gps_sub,super_gps
  real(r_kind),dimension(max(1,nprof_gps)):: toss_gps
  real(r_kind),dimension(max(1,nprof_gps)):: high_gps,high_gps_sub
  real(r_kind),dimension(max(1,nprof_gps)):: dobs_height,dobs_height_sub

  real(r_single),allocatable,dimension(:,:)::sdiag
  character(8),allocatable,dimension(:):: cdiag
  
  type(obs_diag), pointer :: obsptr => NULL()
  

!*******************************************************************************
! Check to see if there are any profiles to process.  If none, return.
  if (nprof_gps==0) then
     if (mype==0) write(6,*)'GENSTATS_GPS:  no profiles to process (nprof_gfs=',nprof_gps,'), EXIT routine'
     return
  endif

! Reduce sub-domain specific QC'd profile height cutoff values to
! maximum global value for each profile
  toss_gps=zero
  call mpi_allreduce(toss_gps_sub,toss_gps,nprof_gps,mpi_rtype,mpi_max,&
       mpi_comm_world,ierror)

! Get height of maximum bending angle
  dobs_height_sub = zero
  DO ii=1,nobs_bins
     gps_allptr => gps_allhead(ii)%head
     do while (associated(gps_allptr))

!       Load local work variables
        kprof        = gps_allptr%kprof
        dtype        = gps_allptr%rdiag(20)
        dobs         = gps_allptr%rdiag(17)

        if (dtype == one .and. toss_gps(kprof) > zero .and. dobs == toss_gps(kprof)) then
           dobs_height_sub(kprof) = gps_allptr%rdiag(7)
        endif

        gps_allptr => gps_allptr%llpoint

!    End loop over observations
     end do

! End of loop over time bins
  END DO

! Reduce sub-domain specific QC'd profile height to maximum global value for each profile
  dobs_height=zero
  call mpi_allreduce(dobs_height_sub,dobs_height,nprof_gps,mpi_rtype,mpi_max,&
       mpi_comm_world,ierror)


! Compute superobs factor on sub-domains using global QC'd profile height
  super_gps_sub=zero
  high_gps_sub = zero
  DO ii=1,nobs_bins
     gps_allptr => gps_allhead(ii)%head
     do while (associated(gps_allptr))

!       Load local work variables
        ratio_errors = gps_allptr%ratio_err
        data_ier     = gps_allptr%obserr
        luse         = gps_allptr%luse
        kprof        = gps_allptr%kprof
        dtype        = gps_allptr%rdiag(20)

!       Accumulate superobs factors and get highest good gps obs within a profile

        if (dtype == zero) then ! refractivity
          rhgt         = gps_allptr%loc
          if (rhgt >toss_gps(kprof)) then
             if(ratio_errors*data_ier>tiny_r_kind) then
                elev         = gps_allptr%rdiag(7)
                high_gps_sub(kprof)=max(high_gps_sub(kprof),elev)
                if(luse) then
                   khgt         = gps_allptr%loc
                   k=min(max(1,khgt),nsig)
                   super_gps_sub(k,kprof)=super_gps_sub(k,kprof)+one
                endif
             endif
          endif

        else ! bending angle
            dobs         = gps_allptr%rdiag(17)
            if(toss_gps(kprof) == zero .or. (toss_gps(kprof) > zero .and. dobs < toss_gps(kprof))) then ! will not fail SR from obs qc
              elev         = gps_allptr%rdiag(7)
              if(elev > dobs_height(kprof)) then
                if(ratio_errors*data_ier>tiny_r_kind) then
                   high_gps_sub(kprof)=max(high_gps_sub(kprof),elev)
                   if(luse) then
                      khgt         = gps_allptr%loc
                      k=min(max(1,khgt),nsig)
                      super_gps_sub(k,kprof)=super_gps_sub(k,kprof)+one
                   endif
                endif
              endif
            endif
        endif


        gps_allptr => gps_allptr%llpoint
        
!    End loop over observations
     end do

! End of loop over time bins     
  END DO

  super_gps = zero
  high_gps = zero
! Reduce sub-domain specifc superobs factors to global values for each profile
  call mpi_allreduce(super_gps_sub,super_gps,nsig*nprof_gps,mpi_rtype,mpi_sum,&
       mpi_comm_world,ierror)

! Reduce sub-domain specific high_gps values to global values for each profile
  call mpi_allreduce(high_gps_sub,high_gps,nprof_gps,mpi_rtype,mpi_max,&
       mpi_comm_world,ierror)

! Convert high_gps from meters to kilometers
  high_gps = r1em3*high_gps
  

! If generating diagnostic output, need to determine dimension of output arrays.
  nreal=0
  ioff =nreal
  if (conv_diagsave) then
     icnt = zero
     DO ii=1,nobs_bins
        gps_allptr => gps_allhead(ii)%head
        do while (associated(gps_allptr))
           luse         = gps_allptr%luse
           if(luse)icnt=icnt+1
           gps_allptr => gps_allptr%llpoint
        end do
     END DO
     if(icnt > 0)then
        nreal =21
        ioff  =nreal
        if (lobsdiagsave) nreal=nreal+4*miter+1
        allocate(cdiag(icnt),sdiag(nreal,icnt))
     end if
  endif



! Loop over data to apply final qc, superobs factors, accumulate
! statistics and (optionally) load diagnostic output arrays
  icnt=0
  DO ii=1,nobs_bins
     gps_allptr => gps_allhead(ii)%head
     do while (associated(gps_allptr))

!       Load local work variables
        ratio_errors = gps_allptr%ratio_err
        data_ier     = gps_allptr%obserr
        luse         = gps_allptr%luse
        muse         = gps_allptr%muse
        khgt         = gps_allptr%loc
        kprof        = gps_allptr%kprof
        dtype        = gps_allptr%rdiag(20)
        gpsptr       => gps_allptr%mmpoint
        if(muse .and. associated(gpsptr) .and. luse_obsdiag)then
           obsptr       => gpsptr%diags
        endif

!       Transfer diagnostic information to output arrays
        if(conv_diagsave .and. luse) then
           icnt=icnt+1
           cdiag(icnt) = gps_allptr%cdiag
           do j=1,nreal
              sdiag(j,icnt)= gps_allptr%rdiag(j)
           enddo
        endif

!       Determine model level to which observation is mapped to
        k=min(max(1,khgt),nsig)
 
!       Normalize ratio_errors by superobs factor.  Update ratio_error 
!       term used in minimization
        super_gps_up=zero
 
        if (super_gps(k,kprof)>tiny_r_kind) then
           do j=min(k+1,nsig),nsig
              super_gps_up = max(super_gps_up,super_gps(j,kprof))
           enddo

           if (super_gps_up >tiny_r_kind) then
              factor = one / sqrt(super_gps(k,kprof))
           else
              factor = one / sqrt(max(super_gps(k-1,kprof),super_gps(k,kprof)))
           endif
           ratio_errors = ratio_errors * factor
           if(conv_diagsave .and. luse) then
              if(gps_allptr%rdiag(16) >tiny_r_kind) sdiag(16,icnt)=ratio_errors*data_ier
           endif
 
!          Adjust error ratio for observations used in inner loop
           if (associated(gpsptr)) then
              gpsptr%raterr2 = ratio_errors **2
              if(associated(obsptr) .and. luse_obsdiag)then
                 obsptr%wgtjo=(ratio_errors*data_ier)**2
              end if
           endif
        endif


!       For given profile, check if observation level is below level at 
!       which profile data is tossed.   If so, set error parameter to 
!       zero (effectively tossing the obs).
 
        rhgt = gps_allptr%loc
        mreal = 21
        if(dtype == zero) then !refractivity
          if (rhgt<=toss_gps(kprof)) then
             if(ratio_errors*data_ier > tiny_r_kind) then ! obs was good
                if (luse) then
                   if(conv_diagsave) then
                      sdiag(10,icnt) = four
                      sdiag(12,icnt) = -one
                      sdiag(16,icnt) = zero
                      if(lobsdiagsave) sdiag(mreal+jiter,icnt) = -one
                   endif
                   elat         = gps_allptr%rdiag(3)
                   if(elat > r20) then
                      awork(22) = awork(22)+one
                   else if(elat< -r20)then
                      awork(23) = awork(23)+one
                   else
                      awork(24) = awork(24)+one
                   end if
                endif
             endif
             ratio_errors = zero
             if (associated(gpsptr)) then
                gpsptr%raterr2 = ratio_errors **2
                if(associated(obsptr) .and. luse_obsdiag)then
                   obsptr%wgtjo=zero
                   obsptr%muse(jiter)=.false.
                end if
             endif
          endif
        else
          elev         = gps_allptr%rdiag(7)
          dobs         = gps_allptr%rdiag(17)
          if  (toss_gps(kprof) > zero .and. (dobs == toss_gps(kprof) .or. elev < dobs_height(kprof))) then ! SR from obs
              if(ratio_errors*data_ier > tiny_r_kind) then ! obs was good
                 if (luse) then
                    if(conv_diagsave) then
                      sdiag(10,icnt) = four
                      sdiag(12,icnt) = -one
                      sdiag(16,icnt) = zero
                      if(lobsdiagsave) sdiag(mreal+jiter,icnt) = -one
                    endif
                    elat         = gps_allptr%rdiag(3)
                    if(elat > r20) then
                      awork(22) = awork(22)+one
                    else if(elat< -r20)then
                      awork(23) = awork(23)+one
                    else
                      awork(24) = awork(24)+one
                    end if
                 endif
              endif
              ratio_errors = zero
              if (associated(gpsptr)) then
                 gpsptr%raterr2 = ratio_errors **2
                 if(associated(obsptr) .and. luse_obsdiag)then
                    obsptr%wgtjo=zero
                    obsptr%muse(jiter)=.false.
                 end if
              endif
          endif
        endif



!       Regional QC.  Remove obs if highest good obs in
!       profile is below platform specific threshold height.
        if(regional) then
           toss=.false.
           if(ratio_errors*data_ier > tiny_r_kind) then
             if(dtype==zero) then !refractivity
                satid        = gps_allptr%rdiag(1)
                if((satid==41).or.(satid==722).or.(satid==723).or.(satid==4).or.(satid==786).or.(satid==3)) then
                   if ((high_gps(kprof)) < ten)  toss=.true.
                else ! OL
                   if ((high_gps(kprof)) < five) toss=.true.
                endif
             else !bending angle
                if ((high_gps(kprof)) <= six)  toss=.true.
             endif
           endif
           if (toss) then
              if (luse) then
                 if(conv_diagsave) then
                    sdiag(10,icnt) = four
                    sdiag(12,icnt) = -one
                    sdiag(16,icnt) = zero
                    if(lobsdiagsave) sdiag(mreal+jiter,icnt) = -one
                 endif
                 elat         = gps_allptr%rdiag(3)
                 if(elat > r20) then
                    awork(22) = awork(22)+one
                 else if(elat< -r20)then
                    awork(23) = awork(23)+one
                 else
                    awork(24) = awork(24)+one
                 end if
              end if
              ratio_errors = zero
              if (associated(gpsptr)) then
                 gpsptr%raterr2 = ratio_errors **2
                 if(associated(obsptr) .and. luse_obsdiag)then
                    obsptr%wgtjo=zero
                    obsptr%muse(jiter)=.false.
                 end if
              endif
           endif
        endif ! regional

!       Compute penalty terms
        if (ratio_errors*data_ier <= tiny_r_kind) muse = .false.
        if(luse)then
           val          = gps_allptr%dataerr
           val2     = val*val
           exp_arg  = -half*val2
           rat_err2 = ratio_errors**2
           data_ipg     = gps_allptr%pg
           if (data_ipg > tiny_r_kind) then
              data_ib      = gps_allptr%b
              cg_gps=cg_term/data_ib
              wnotgross= one-data_ipg
              wgross   = data_ipg*cg_gps
              arg      = exp(exp_arg)
              term     = log(wnotgross*arg+wgross)
              wgt      = wnotgross*arg/(wnotgross*arg+wgross)
           else
              term = exp_arg
              wgt  = one
           endif
           if(conv_diagsave) sdiag(13,icnt) = wgt/wgtlim
           valqc = -two*rat_err2*term
         

!          Accumulate statistics for obs belonging to this task
!          based on interface (not mid-point) level
           val2=val2*rat_err2
           if(muse)then
              if(wgt < wgtlim) awork(21) = awork(21)+one

!             Accumulate values for penalty and data count
              jsig=max(1,khgt)
              awork(jsig+3*nsig+100)=awork(jsig+3*nsig+100)+valqc
              awork(jsig+5*nsig+100)=awork(jsig+5*nsig+100)+one
              awork(jsig+6*nsig+100)=awork(jsig+6*nsig+100)+val2
              nn=1
           else
              nn=2
              if(ratio_errors*data_ier >=tiny_r_kind)nn=3
           endif

           data_ikx     = gps_allptr%type
           ikx          = nint(data_ikx)
           pressure     = gps_allptr%rdiag(6)
           data_rinc    = gps_allptr%rdiag(5)*scale
!          Loop over pressure level groupings and obs to accumulate
!          statistics as a function of observation type.
           do k = 1,npres_print
              if(pressure>ptop(k) .and. pressure<=pbot(k))then
                 ress=data_rinc
                 
                 bwork(k,ikx,1,nn)  = bwork(k,ikx,1,nn)+one           ! count
                 bwork(k,ikx,2,nn)  = bwork(k,ikx,2,nn)+ress          ! (o-g)
                 bwork(k,ikx,3,nn)  = bwork(k,ikx,3,nn)+ress*ress     ! (o-g)**2
                 bwork(k,ikx,4,nn)  = bwork(k,ikx,4,nn)+val2          ! penalty
                 bwork(k,ikx,5,nn)  = bwork(k,ikx,5,nn)+valqc         ! nonlin qc penalty
                 
              end if
           end do
        end if
        
        gps_allptr => gps_allptr%llpoint

!    End loop over observations
     end do

! End of loop over time bins
  END DO

! If requested, write information to diagnostic file
  if(conv_diagsave .and. icnt > 0)then
     nchar = 1
     write(7)'gps',nchar,nreal,icnt,mype,ioff
     write(7)cdiag,sdiag
     deallocate(cdiag,sdiag)
  endif


! Destroy arrays holding gps data
  call destroy_genstats_gps

end subroutine genstats_gps
