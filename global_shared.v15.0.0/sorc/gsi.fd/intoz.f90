module intozmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intozmod    module for intoz and its tangent linear intoz_tl
!   prgmmr:
!
! abstract: module for intoz and its tangent linear intoz_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intoz and its tangent linear intoz_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intoz_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test
!
! subroutines included:
!   sub intoz_
!   sub intozlay_
!   sub intozlev_
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
PUBLIC intoz

interface intoz; module procedure &
          intoz_
end interface

contains

subroutine intoz_(ozhead,o3lhead,rval,sval)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intoz       call individual ozone obs operators
!   prgmmr: todling       org: np23                date: 2008-11-28
!
! abstract:  This routine calls the individual components of the 
!            ozone observation operator.
!
! program history log:
!   2008-11-28  todling
!   2009-01-08  todling - remove reference to ozohead
!   2010-05-13  todling - update to use gsi_bundle
!
!   input argument list:
!     ozhead  - layer ozone obs type pointer to obs structure
!     o3lhead - level ozone obs type pointer to obs structure
!     soz     - ozone increment in grid space
!
!   output argument list:
!     roz    - ozone results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use obsmod, only: oz_ob_type,o3l_ob_type
  use gsi_bundlemod, only: gsi_bundle
  implicit none

! Declare passed variables
  type( oz_ob_type),pointer,intent(in   ) :: ozhead
  type(o3l_ob_type),pointer,intent(in   ) :: o3lhead
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval

!  If obs exist call int routines
  if(associated(ozhead))call intozlay_( ozhead,rval,sval)
  if(associated(o3lhead))call intozlev_(o3lhead,rval,sval)

end subroutine intoz_

subroutine intozlay_(ozhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intoz       apply nonlin qc obs operator for ozone
!   prgmmr: derber           org: np23                date: 1995-07-11
!
! abstract:  This routine applies the observation operator (forward
!            model) and adjoint of this operator for ozone observations
!            with the addition of nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documentation
!   2004-08-02  treadon - add only to module use, add intent in/out
!   2004-10-08  parrish - add nonlinear qc
!   2005-03-01  parrish - nonlinear qc change to account for inflated obs error
!   2005-04-11  treadon - merge intoz and intoz_qc into single routine
!   2005-06-14  wu      - add OMI total ozone
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2007-02-15  rancic - add foto
!   2007-02-16  sienkiewicz - add call to routine for level ozone contrib.
!   2007-03-19  tremolet - binning of observations
!   2007-05-30  h.liu   - move interpolation weights w1-w4 inside k loop
!   2007-06-05  tremolet - use observation diagnostics structure
!   2007-07-09  tremolet - observation sensitivity
!   2008-01-04  tremolet - Don't apply H^T if l_do_adjoint is false
!   2008-??-??  ??????   - remove nonlinear qc gradient; folded OMI within layer O3
!   2008-11-28  todling  - turn FOTO optional; changed ptr%time handle
!   2009-01-18  todling  - treat val in quad precision (revisit later)
!   2010-05-13  todling  - update to use gsi_bundle; update interface
!   2012-09-08  wargan   - add OMI with efficiency factors
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     ozhead  - layer ozone obs type pointer to obs structure
!     soz     - ozone increment in grid space
!     roz
!
!   output argument list:
!     roz    - ozone results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: oz_ob_type,lsaveobsens,l_do_adjoint,nloz_omi,luse_obsdiag
  use gridmod, only: lat2,lon2,nsig
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use constants, only: one,zero,r3600,zero_quad
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type( oz_ob_type),pointer,intent(in   ) :: ozhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) i,j,ij,ier,istatus
  integer(i_kind) k,j1,j2,j3,j4,kk,iz1,iz2,j1x,j2x,j3x,j4x,kl
  real(r_kind) dz1,pob,delz,time_oz
  real(r_quad) val1,valx
  real(r_kind) w1,w2,w3,w4
  real(r_kind),pointer,dimension(:) :: xhat_dt_oz
  real(r_kind),pointer,dimension(:) :: dhat_dt_oz
  real(r_kind),pointer,dimension(:,:,:)  :: sozp
  real(r_kind),pointer,dimension(:,:,:)  :: rozp
  real(r_kind),allocatable,dimension(:,:) :: soz
  real(r_kind),allocatable,dimension(:,:) :: roz
  type(oz_ob_type), pointer :: ozptr
  real(r_kind),dimension(nloz_omi):: val_lay

! Retrieve pointers
! Simply return if any pointer not found
  ier=0 
  call gsi_bundlegetpointer(sval,'oz',sozp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'oz',rozp,istatus);ier=istatus+ier
  if(l_foto) then
     call gsi_bundlegetpointer(xhat_dt,'oz',xhat_dt_oz,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'oz',dhat_dt_oz,istatus);ier=istatus+ier
  endif
  if(ier/=0)return

! Can't do rank-2 pointer into rank-2, therefore, allocate work space
  allocate(soz(lat2*lon2,nsig),roz(lat2*lon2,nsig))
  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           soz(ij,k) = sozp(i,j,k)
           roz(ij,k) = rozp(i,j,k)
        enddo
     enddo
  enddo
!
! SBUV OZONE: LAYER O3 and TOTAL O3
!
! Loop over ozone observations.
  ozptr => ozhead
  do while (associated(ozptr))

!    Set location
     j1=ozptr%ij(1)
     j2=ozptr%ij(2)
     j3=ozptr%ij(3)
     j4=ozptr%ij(4)


!    Accumulate contribution from layer observations
     dz1=nsig+1
     if ( ozptr%nloz >= 1 ) then

        if(l_foto) time_oz = ozptr%time*r3600
        do k=1,ozptr%nloz
           val1= zero_quad
           pob = ozptr%prs(k)
           iz1=dz1
           if (iz1 > nsig) iz1=nsig
           iz2=pob
           do kk=iz1,iz2,-1
              delz=one
              if (kk==iz1) delz=dz1-iz1
              if (kk==iz2) delz=delz-pob+iz2
              w1=ozptr%wij(1,kk)
              w2=ozptr%wij(2,kk)
              w3=ozptr%wij(3,kk)
              w4=ozptr%wij(4,kk)
              val1=val1 + ( &
                   w1* soz(j1,kk)+ &
                   w2* soz(j2,kk)+ &
                   w3* soz(j3,kk)+ &
                   w4* soz(j4,kk))*delz
              if (l_foto) then
                 j1x=w1+(kk-1)*lat2*lon2
                 j2x=w2+(kk-1)*lat2*lon2
                 j3x=w3+(kk-1)*lat2*lon2
                 j4x=w4+(kk-1)*lat2*lon2
                 val1=val1 + ( &
                     (w1*xhat_dt_oz(j1x)+ &
                      w2*xhat_dt_oz(j2x)+ &
                      w3*xhat_dt_oz(j3x)+ &
                      w4*xhat_dt_oz(j4x))*time_oz)*delz
              endif
           enddo

           if(luse_obsdiag)then
              if (lsaveobsens) then
                 valx=val1*ozptr%err2(k)*ozptr%raterr2(k)
                 ozptr%diags(k)%ptr%obssen(jiter)=valx
              else
                 if (ozptr%luse) ozptr%diags(k)%ptr%tldepart(jiter)=val1
              endif
           endif

           if (l_do_adjoint) then
              if (.not. lsaveobsens) then
                 if(ladtest_obs) then
                    valx=val1
                 else
                    val1=val1-ozptr%res(k)

                    valx     = val1*ozptr%err2(k) 
                    valx     = valx*ozptr%raterr2(k)
                 endif
              endif

              do kk=iz1,iz2,-1
                 delz=one
                 if(kk==iz1)delz=dz1-iz1
                 if(kk==iz2)delz=delz-pob+iz2
                 w1=ozptr%wij(1,kk)
                 w2=ozptr%wij(2,kk)
                 w3=ozptr%wij(3,kk)
                 w4=ozptr%wij(4,kk)
                 roz(j1,kk)  =  roz(j1,kk) + valx*w1*delz
                 roz(j2,kk)  =  roz(j2,kk) + valx*w2*delz
                 roz(j3,kk)  =  roz(j3,kk) + valx*w3*delz
                 roz(j4,kk)  =  roz(j4,kk) + valx*w4*delz
              enddo
              if (l_foto) then
                 do kk=iz1,iz2,-1
                    delz=one
                    if(kk==iz1)delz=dz1-iz1
                    if(kk==iz2)delz=delz-pob+iz2
                    w1=ozptr%wij(1,kk)
                    w2=ozptr%wij(2,kk)
                    w3=ozptr%wij(3,kk)
                    w4=ozptr%wij(4,kk)
                    j1x=w1+(kk-1)*lat2*lon2
                    j2x=w2+(kk-1)*lat2*lon2
                    j3x=w3+(kk-1)*lat2*lon2
                    j4x=w4+(kk-1)*lat2*lon2
                    dhat_dt_oz(j1x) = dhat_dt_oz(j1x) + valx*w1*delz*time_oz
                    dhat_dt_oz(j2x) = dhat_dt_oz(j2x) + valx*w2*delz*time_oz
                    dhat_dt_oz(j3x) = dhat_dt_oz(j3x) + valx*w3*delz*time_oz
                    dhat_dt_oz(j4x) = dhat_dt_oz(j4x) + valx*w4*delz*time_oz
                 enddo
              endif
              dz1=pob
           endif
        end do

     end if   ! (ozptr%nloz >= 1)

!    Add contribution from total column observation
     k=ozptr%nloz+1
     if (ozptr%apriori(1) .lt. zero) then ! non-OMIEFF ozone
        val1= zero
        do kk=nsig,1,-1
           w1=ozptr%wij(1,kk)
           w2=ozptr%wij(2,kk)
           w3=ozptr%wij(3,kk)
           w4=ozptr%wij(4,kk)
           val1=val1 + &
                w1* soz(j1,kk)+ &
                w2* soz(j2,kk)+ &
                w3* soz(j3,kk)+ &
                w4* soz(j4,kk)
        enddo
        if (l_foto) then
           do kk=nsig,1,-1
              w1=ozptr%wij(1,kk)
              w2=ozptr%wij(2,kk)
              w3=ozptr%wij(3,kk)
              w4=ozptr%wij(4,kk)
              j1x=w1+(kk-1)*lat2*lon2
              j2x=w2+(kk-1)*lat2*lon2
              j3x=w3+(kk-1)*lat2*lon2
              j4x=w4+(kk-1)*lat2*lon2
              val1=val1 + &
                   (w1*xhat_dt_oz(j1x)+ &
                   w2*xhat_dt_oz(j2x)+ &
                   w3*xhat_dt_oz(j3x)+ &
                   w4*xhat_dt_oz(j4x))*time_oz
           enddo
        endif
     else  ! OMI ozone with efficiency factor
! Integrate ozone within each layer
        dz1=nsig+1
        do kl=1,nloz_omi
           val_lay(kl)= zero_quad
           pob = ozptr%prs(kl)
           iz1=dz1
           if (iz1 > nsig) iz1=nsig
           iz2=pob
           do kk=iz1,iz2,-1
              delz=one
              if (kk==iz1) delz=dz1-iz1
              if (kk==iz2) delz=delz-pob+iz2
              w1=ozptr%wij(1,kk)
              w2=ozptr%wij(2,kk)
              w3=ozptr%wij(3,kk)
              w4=ozptr%wij(4,kk)
              val_lay(kl)=val_lay(kl) + ( &
                   w1* soz(j1,kk)+ &
                   w2* soz(j2,kk)+ &
                   w3* soz(j3,kk)+ &
                   w4* soz(j4,kk))*delz     
           enddo
           dz1=pob 
        enddo

! Apply the efficiency factor
        val1=zero_quad
        do j=1,nloz_omi 
           val1=val1+ozptr%efficiency(j)*val_lay(j)
        enddo    
     endif ! OMI ozone with efficiency factor
     

     if(luse_obsdiag)then
        if (lsaveobsens) then
           valx=val1*ozptr%err2(k)*ozptr%raterr2(k)
           ozptr%diags(k)%ptr%obssen(jiter)=valx
        else
           if (ozptr%luse) ozptr%diags(k)%ptr%tldepart(jiter)=val1
        endif
     endif

     if (l_do_adjoint) then
        if (ozptr%apriori(1) .lt. zero) then ! non-OMI ozone
           if (.not. lsaveobsens) then
              if(ladtest_obs) then
                 valx=val1
              else
                 val1=val1-ozptr%res(k)
              
                 valx     = val1*ozptr%err2(k)
                 valx     = valx*ozptr%raterr2(k)
              endif
           endif
           
           do kk=nsig,1,-1
              w1=ozptr%wij(1,kk)
              w2=ozptr%wij(2,kk)
              w3=ozptr%wij(3,kk)
              w4=ozptr%wij(4,kk)
              roz(j1,kk)  = roz(j1,kk) + valx*w1
              roz(j2,kk)  = roz(j2,kk) + valx*w2
              roz(j3,kk)  = roz(j3,kk) + valx*w3
              roz(j4,kk)  = roz(j4,kk) + valx*w4
           enddo
           if (l_foto) then
              do kk=nsig,1,-1
                 w1=ozptr%wij(1,kk)
                 w2=ozptr%wij(2,kk)
                 w3=ozptr%wij(3,kk)
                 w4=ozptr%wij(4,kk)
                 j1x=w1+(kk-1)*lat2*lon2
                 j2x=w2+(kk-1)*lat2*lon2
                 j3x=w3+(kk-1)*lat2*lon2
                 j4x=w4+(kk-1)*lat2*lon2
                 dhat_dt_oz(j1x) =dhat_dt_oz(j1x) + valx*w1*time_oz
                 dhat_dt_oz(j2x) =dhat_dt_oz(j2x) + valx*w2*time_oz
                 dhat_dt_oz(j3x) =dhat_dt_oz(j3x) + valx*w3*time_oz
                 dhat_dt_oz(j4x) =dhat_dt_oz(j4x) + valx*w4*time_oz
              enddo
           endif
        else  ! OMI ozone with efficiency factor
           if (lsaveobsens) then
              valx = ozptr%diags(k)%ptr%obssen(jiter)              
           else
              if(ladtest_obs) then
                 valx=val1
              else
                 val1=val1-ozptr%res(k)
                 valx     = val1*ozptr%err2(k) 
                 valx     = valx*ozptr%raterr2(k)
              endif
           endif
           ! Apply the transpose of the efficiency factor
           do j=1,nloz_omi 
              val_lay(j)=ozptr%efficiency(j)*valx
           enddo
! spread the info over GSI levels
           do kl=1,nloz_omi
              pob = ozptr%prs(kl)
              iz1=dz1
              if (iz1 > nsig) iz1=nsig
              iz2=pob              
              do kk=iz1,iz2,-1
                 delz=one
                 if(kk==iz1)delz=dz1-iz1
                 if(kk==iz2)delz=delz-pob+iz2
                 w1=ozptr%wij(1,kk)
                 w2=ozptr%wij(2,kk)
                 w3=ozptr%wij(3,kk)
                 w4=ozptr%wij(4,kk)
                 roz(j1,kk)  =  roz(j1,kk) + val_lay(kl)*w1*delz
                 roz(j2,kk)  =  roz(j2,kk) + val_lay(kl)*w2*delz
                 roz(j3,kk)  =  roz(j3,kk) + val_lay(kl)*w3*delz
                 roz(j4,kk)  =  roz(j4,kk) + val_lay(kl)*w4*delz
              enddo
              dz1=pob 
           end do
        endif  ! OMI
     endif   ! do adjoint

     ozptr => ozptr%llpoint

! End loop over observations
  enddo

! Copy output and clean up 
  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           rozp(i,j,k) = roz(ij,k)
        enddo
     enddo
  enddo
  deallocate(soz,roz)

! End of routine
  return
end subroutine intozlay_

subroutine intozlev_(o3lhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    into3l       apply nonlin qc obs operator for o3 level
!   prgmmr: sienkiewicz      org: GMAO                date: 2006-09-14
!
! abstract:  This routine applies the observation operator (forward
!            model) and adjoint of this operator for ozone level 
!            observations with the addition of nonlinear qc.
!
! to do: add time derivatives correctly (Todling) 
!
! program history log:
!   2006-09-14  sienkiewicz - add level ozone
!   2007-01-02  sienkiewicz - separate from intoz (again)
!   2007-02-16  sienkiewicz - changes for new inner loop obs data structure
!   2009-01-08  todling - remove nonlinear qc
!   2009-01-22  sienkiewicz - add time derivative
!   2010-05-13  todling  - update to use gsi_bundle; update interface
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     o3lhead - level ozone obs type pointer to obs structure
!     soz1d   - ozone increment in grid space
!     roz1d
!
!   output argument list:
!     roz1d   - results from observation operator (0 for no data)
!
! attributes:
!   language: f90
!   machine:  -
!
!$$$
!--------

  use kinds, only: r_kind,i_kind
  use obsmod, only: o3l_ob_type,lsaveobsens, l_do_adjoint,luse_obsdiag
  use gridmod, only: latlon1n
  use constants, only: r3600
  use jfunc, only: jiter,l_foto,xhat_dt,dhat_dt
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(o3l_ob_type),pointer,intent(in   ) :: o3lhead
  type(gsi_bundle),         intent(in   ) :: sval
  type(gsi_bundle),         intent(inout) :: rval

! Declare local variables
  integer(i_kind) ier,istatus
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8
  real(r_kind) val,grad
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,time_o3l
  real(r_kind),pointer,dimension(:) :: xhat_dt_oz
  real(r_kind),pointer,dimension(:) :: dhat_dt_oz
  real(r_kind),pointer,dimension(:) :: soz1d
  real(r_kind),pointer,dimension(:) :: roz1d
  type(o3l_ob_type), pointer :: o3lptr

! Retrieve pointers
! Simply return if any pointer not found
  ier=0 
  call gsi_bundlegetpointer(sval,'oz',soz1d,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'oz',roz1d,istatus);ier=istatus+ier
  if(l_foto) then
     call gsi_bundlegetpointer(xhat_dt,'oz',xhat_dt_oz,istatus);ier=istatus+ier
     call gsi_bundlegetpointer(dhat_dt,'oz',dhat_dt_oz,istatus);ier=istatus+ier
  endif
  if(ier/=0)return

! LEVEL-OZONE OBSERVATIONS

! Loop over ozone observations.


  o3lptr => o3lhead

  do while (associated(o3lptr))
     j1=o3lptr%ij(1)
     j2=o3lptr%ij(2)
     j3=o3lptr%ij(3)
     j4=o3lptr%ij(4)
     j5=o3lptr%ij(5)
     j6=o3lptr%ij(6)
     j7=o3lptr%ij(7)
     j8=o3lptr%ij(8)
     w1=o3lptr%wij(1)
     w2=o3lptr%wij(2)
     w3=o3lptr%wij(3)
     w4=o3lptr%wij(4)
     w5=o3lptr%wij(5)
     w6=o3lptr%wij(6)
     w7=o3lptr%wij(7)
     w8=o3lptr%wij(8)


!    Forward model
     val=w1*soz1d(j1)+w2*soz1d(j2)+w3*soz1d(j3)+w4*soz1d(j4)+ &
          w5*soz1d(j5)+w6*soz1d(j6)+w7*soz1d(j7)+w8*soz1d(j8)

     if ( l_foto ) then
        time_o3l=o3lptr%time*r3600
        val=val+&
            (w1*xhat_dt_oz(j1)+w2*xhat_dt_oz(j2)+ &
             w3*xhat_dt_oz(j3)+w4*xhat_dt_oz(j4)+ &
             w5*xhat_dt_oz(j5)+w6*xhat_dt_oz(j6)+ &
             w7*xhat_dt_oz(j7)+w8*xhat_dt_oz(j8))*time_o3l
     endif

     if (lsaveobsens) then
        o3lptr%diags%obssen(jiter) = val*o3lptr%raterr2*o3lptr%err2
     else
        if (o3lptr%luse) o3lptr%diags%tldepart(jiter)=val
     endif

     if (l_do_adjoint) then
        if (lsaveobsens) then
           grad = o3lptr%diags%obssen(jiter)

        else
           if(ladtest_obs) then
              grad = val
           else
              val=val-o3lptr%res

              grad = val*o3lptr%raterr2*o3lptr%err2
           endif
        endif

!    Adjoint
        roz1d(j1)=roz1d(j1)+w1*grad
        roz1d(j2)=roz1d(j2)+w2*grad
        roz1d(j3)=roz1d(j3)+w3*grad
        roz1d(j4)=roz1d(j4)+w4*grad
        roz1d(j5)=roz1d(j5)+w5*grad
        roz1d(j6)=roz1d(j6)+w6*grad
        roz1d(j7)=roz1d(j7)+w7*grad
        roz1d(j8)=roz1d(j8)+w8*grad

        if ( l_foto ) then
           grad=grad*time_o3l
           dhat_dt_oz(j1)=dhat_dt_oz(j1)+w1*grad
           dhat_dt_oz(j2)=dhat_dt_oz(j2)+w2*grad
           dhat_dt_oz(j3)=dhat_dt_oz(j3)+w3*grad
           dhat_dt_oz(j4)=dhat_dt_oz(j4)+w4*grad
           dhat_dt_oz(j5)=dhat_dt_oz(j5)+w5*grad
           dhat_dt_oz(j6)=dhat_dt_oz(j6)+w6*grad
           dhat_dt_oz(j7)=dhat_dt_oz(j7)+w7*grad
           dhat_dt_oz(j8)=dhat_dt_oz(j8)+w8*grad
        endif

     endif

     o3lptr => o3lptr%llpoint

  end do

! End of routine
  return

end subroutine intozlev_

end module intozmod
