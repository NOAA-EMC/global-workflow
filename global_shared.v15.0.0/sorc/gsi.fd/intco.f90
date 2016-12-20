module intcomod

!$$$ module documentation block
!           .      .    .                                       .
! module:   intcomod    module for intco and its tangent linear intco_tl
!   prgmmr:
!
! abstract: module for intco and its tangent linear intco_tl
!
! program history log:
!   2005-05-13  Yanqiu zhu - wrap intoz and its tangent linear intoz_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-11-26  Todling - remove intoz_tl; add interface back
!   2009-08-13  lueken - update documentation
!   2010-06-02  tangborn - converted intoz into intco 
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - implemented obs adjoint test  
!
! subroutines included:
!   sub intco_
!   sub intcolev_
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
PUBLIC intco

interface intco; module procedure &
          intco_
end interface

contains

subroutine intco_(colvkhead,rval,sval)

!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intco       call individual carbon monoxide obs operators
!   prgmmr: todling       org: np23                date: 2008-11-28
!
! abstract:  This routine calls the individual components of the 
!            carbon monoxide observation operator.
!
! program history log:
!   2008-11-28  todling
!   2009-01-08  todling - remove reference to ozohead
!   2010-05-13  todling - update to use gsi_bundle
!   2010-06-02  tangborn - made version for carbon monoxide
!
!   input argument list:
!     colvkhead  - level carbon monoxide obs type pointer to obs structure for MOPITT
!     sco     - carbon monoxide increment in grid space
!
!   output argument list:
!     rco    - carbon monoxide results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use obsmod, only: colvk_ob_type
  use gsi_bundlemod, only: gsi_bundle
  implicit none

! Declare passed variables
  type(colvk_ob_type),pointer,intent(in   ) :: colvkhead
  type(gsi_bundle),intent(in   ) :: sval
  type(gsi_bundle),intent(inout) :: rval

  call intcolev_(colvkhead,rval,sval)

end subroutine intco_

subroutine intcolev_(colvkhead,rval,sval)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    intco       apply nonlin qc obs operator for carbon monoxide
!   prgmmr: derber           org: np23                date: 1995-07-11
!
! abstract:  This routine applies the observation operator (forward
!            model) and adjoint of this operator for ozone observations
!            with the addition of nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   2010-06-07  tangborn - carbon monoxide based on ozone code
!   2012-09-14  Syed RH Rizvi, NCAR/NESL/MMM/DAS  - introduced ladtest_obs         
!   2014-12-03  derber  - modify so that use of obsdiags can be turned off
!
!   input argument list:
!     colvkhead  - level carbon monoxide obs type pointer to obs structure
!     sco     - carbon monoxide increment in grid space
!
!   output argument list:
!     rco     - carbon monoxide results from observation operator 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
!--------
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: colvk_ob_type,lsaveobsens,l_do_adjoint,luse_obsdiag
  use gridmod, only: lat2,lon2,nsig
  use jfunc, only: jiter,xhat_dt,dhat_dt
  use constants, only: one,zero,r3600,zero_quad
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use gsi_4dvar, only: ladtest_obs
  implicit none

! Declare passed variables
  type(colvk_ob_type),pointer,intent(in   ) :: colvkhead
  type(gsi_bundle)          ,intent(in   ) :: sval
  type(gsi_bundle)          ,intent(inout) :: rval

! Declare local variables
  integer(i_kind) i,j,ij,ier,istatus
  integer(i_kind) k,k1,k2,j1,j2,j3,j4,kk
  real(r_kind) pob
  real(r_quad) val1,valx
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8
  real(r_kind),pointer,dimension(:,:,:)  :: scop
  real(r_kind),pointer,dimension(:,:,:)  :: rcop
  real(r_kind),allocatable,dimension(:,:) :: sco
  real(r_kind),allocatable,dimension(:,:) :: rco
  real(r_kind),allocatable,dimension(:)   :: coak
  real(r_kind),allocatable,dimension(:)   :: vali
  real(r_kind),allocatable,dimension(:)   :: val_ret
  type(colvk_ob_type), pointer :: colvkptr

!  If no co observations return
  if(.not. associated(colvkhead))return
! Retrieve pointers
! Simply return if any pointer not found
  ier=0 
  call gsi_bundlegetpointer(sval,'co',scop,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'co',rcop,istatus);ier=istatus+ier
  if(ier/=0)return

! Can't do rank-2 pointer into rank-2, therefore, allocate work space
  allocate(sco(lat2*lon2,nsig),rco(lat2*lon2,nsig))
  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           sco(ij,k) = scop(i,j,k)
           rco(ij,k) = rcop(i,j,k)
        enddo
     enddo
  enddo
!
! MOPITT CARBON MONOXIDE: LAYER CO 
!
! Loop over carbon monoxide observations.
  colvkptr => colvkhead
  do while (associated(colvkptr))

!    Set location
     j1=colvkptr%ij(1)
     j2=colvkptr%ij(2)
     j3=colvkptr%ij(3)
     j4=colvkptr%ij(4)


!    Accumulate contribution from layer observations
!    This repeats the algorithm inside intrp3co
!    with several of the terms already calculated 


        allocate(vali(colvkptr%nlco))
        allocate(coak(colvkptr%nlco))
        allocate(val_ret(colvkptr%nlco))

        do k=1,colvkptr%nlco   ! loop over MOPITT ave. ker. contribution levels 
           pob = colvkptr%prs(k)
           k1=int(pob)
           k2=min(k1+1,nsig)
           w1=colvkptr%wij(1,k)
           w2=colvkptr%wij(2,k)
           w3=colvkptr%wij(3,k)
           w4=colvkptr%wij(4,k)
           w5=colvkptr%wij(5,k)
           w6=colvkptr%wij(6,k)
           w7=colvkptr%wij(7,k)
           w8=colvkptr%wij(8,k)
           val1=   w1* sco(j1,k1)+ &
                   w2* sco(j2,k1)+ &
                   w3* sco(j3,k1)+ &
                   w4* sco(j4,k1)+ &
                   w5* sco(j1,k2)+ &
                   w6* sco(j2,k2)+ &
                   w7* sco(j3,k2)+ & 
                   w8* sco(j4,k2)
           vali(k)=val1
        enddo

!       Averaging kernel  

        do k=1,colvkptr%nlco   ! loop over MOPITT retrieval levels
           val1=zero_quad
           do j=1,colvkptr%nlco  ! loop over MOPITT ak levels 
              val1=val1+colvkptr%ak(k,j)*vali(j)
           enddo 

           if(luse_obsdiag)then
              if (lsaveobsens) then
                 valx=val1*colvkptr%err2(k)*colvkptr%raterr2(k)
                 colvkptr%diags(k)%ptr%obssen(jiter)=valx
              else
                 if (colvkptr%luse) colvkptr%diags(k)%ptr%tldepart(jiter)=val1
              endif
           endif

           if (l_do_adjoint) then
              if (.not. lsaveobsens) then
                 if( ladtest_obs ) then
                    valx = val1
                 else
                    val1=val1-colvkptr%res(k)

                    valx=val1*colvkptr%err2(k) 
                    valx=valx*colvkptr%raterr2(k)
                 end if
              endif
              val_ret(k)=valx  
           endif 
        enddo ! k

!  Averaging kernel First - spread values to ak contribution levels 

        if(l_do_adjoint)then 
              do k=1,colvkptr%nlco  !loop over ak levels 
                 coak(k)=zero_quad
                 do j=1,colvkptr%nlco  !loop over profile levels  
                    coak(k)=coak(k)+colvkptr%ak(j,k)*val_ret(j) ! Contribution to kth ak level from jth retrieval level
                 enddo
              enddo 

! Adjoint of interpolation - spreads each ave. kernel level to interpolant gridpoints  

              do kk=colvkptr%nlco,1,-1    !loop over averaging kernel levels 
                 pob = colvkptr%prs(kk)
                 k1=int(pob)
                 k2=min(k1+1,nsig)
                 w1=colvkptr%wij(1,kk)
                 w2=colvkptr%wij(2,kk)
                 w3=colvkptr%wij(3,kk)
                 w4=colvkptr%wij(4,kk)
                 w5=colvkptr%wij(5,kk)
                 w6=colvkptr%wij(6,kk)
                 w7=colvkptr%wij(7,kk) 
                 w8=colvkptr%wij(8,kk) 
                 rco(j1,k1)  =  rco(j1,k1) + coak(kk)*w1
                 rco(j2,k1)  =  rco(j2,k1) + coak(kk)*w2
                 rco(j3,k1)  =  rco(j3,k1) + coak(kk)*w3
                 rco(j4,k1)  =  rco(j4,k1) + coak(kk)*w4
                 rco(j1,k2)  =  rco(j1,k2) + coak(kk)*w5
                 rco(j2,k2)  =  rco(j2,k2) + coak(kk)*w6
                 rco(j3,k2)  =  rco(j3,k2) + coak(kk)*w7
                 rco(j4,k2)  =  rco(j4,k2) + coak(kk)*w8
              enddo  ! kk


        deallocate(coak,vali,val_ret)

        endif ! l_do_adjoint
        colvkptr => colvkptr%llpoint

! End loop over observations
  enddo

! Copy output and clean up 
  do k=1,nsig
     ij=0
     do j=1,lon2
        do i=1,lat2
           ij=ij+1
           rcop(i,j,k) = rco(ij,k)
        enddo
     enddo
  enddo
  deallocate(sco,rco)

! End of routine
  return
end subroutine intcolev_


end module intcomod
