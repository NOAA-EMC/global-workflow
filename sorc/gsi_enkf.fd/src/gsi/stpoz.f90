module stpozmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpozmod    module for stpoz and its tangent linear stpoz_tl
!  prgmmr:
!
! abstract: module for stpoz and its tangent linear stpoz_tl
!
! program history log:
!   2005-05-17  Yanqiu zhu - wrap stpoz and its tangent linear stpoz_tl into one module
!   2005-11-16  Derber - remove interfaces
!   2008-12-02  Todling - remove stpoz_tl
!   2009-01-21  Sienkiewicz - add stpo3l (level ozone) again
!   2009-08-12  lueken - update documentation
!   2010-05-13  todling - uniform interface across stp routines
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2016-08-26  guo     - added interfaces for individual obs-types (oz and o3l).
!   2018-07-27  guo     - A single stpozmod module is splitted to stpozmod and stpo3lmod
!                       - Removed unused generic interfaces
!
! subroutines included:
!   sub stpoz
!   sub stpozlay_
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
public:: stpozlay       ! Generic interfaces are disirable, where full TKR
                        ! matching are required if they are involked, such as
                        !   call stpozlay(..,pbcjo(:,i_oz_ob_type,ib),..)

contains

subroutine stpozlay(ozhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpoz       compute contribution to penalty and
!                            stepsize for ozone, using nonlinear qc
!   prgmmr: derber          org: np23                 date: 1995-07-11
!
! abstract: The routine computes the contribution to the penalty from ozone
!           observations.  The routine also computes the contribution of
!           ozone observations to the step size.  This version includes
!           nonlinear qc.
!
! program history log:
!   1995-07-11  derber
!   1999-03-01  wu - port cray90 code to ibm-sp (mpi version)
!   2004-06-16  treadon - update documenation
!   2004-07-29  treadon - add only to module use, add intent in/out
!   2004-10-07  parrish - add nonlinear qc option
!   2005-04-11  treadon - merge stpoz and stpoz_qc into single routine
!   2005-06-14  wu      - add OMI toz
!   2005-09-28  derber  - consolidate location and weight arrays
!   2006-07-28  derber  - modify to use new inner loop obs data structure
!                       - unify NL qc
!   2006-09-18  derber  - modify output values of b1 and b3
!   2007-03-19  tremolet - binning of observations
!   2007-05-30  h.liu   - move interpolation weights w1-w4 inside k loop
!   2007-06-04  derber  - use quad precision to get reproducability over number of processors
!   2010-05-13  todling - udpate to use gsi_bundle
!   2012-09-10  wargan  - add OMI with efficiency factors
!
!   input argument list:
!     ozhead  - layer ozone obs type pointer to obs structure
!     roz  - search direction for ozone
!     soz  - input ozone correction field
!     sges - step size estimates (nstep)
!     nstep- number of stepsize estimates (==0 means use outer iteration value)
!
!   output argument list:
!     out(1:nstep) - contribution of ozone data to penalty sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use obsmod, only: nloz_omi
  use constants, only: one,half,two,zero_quad,r3600,zero
  use gridmod, only: lat2,lon2,nsig
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_ozNode , only: ozNode
  use m_ozNode , only: ozNode_typecast
  use m_ozNode , only: ozNode_nextcast
  implicit none

! Declare passed variables

  class(obsNode), pointer             ,intent(in   ) ::  ozhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) i,j,ij,ier,istatus
  integer(i_kind) k,j1,j2,j3,j4,iz1,iz2,kk,kl
  real(r_kind) dz1,pob,delz
  real(r_kind) w1,w2,w3,w4,oz
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind),allocatable,dimension(:,:) :: roz,soz
  real(r_kind),pointer,dimension(:,:,:)   :: rozp,sozp
  real(r_kind),dimension(nloz_omi):: val_lay, val_lay1
  type(ozNode), pointer ::  ozptr

  real(r_quad) val,val1

! Get pointers and return if not found
  ier=0
  call gsi_bundlegetpointer(sval,'oz',sozp,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'oz',rozp,istatus);ier=istatus+ier
  if(ier/=0) return

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

! SBUV OZONE: LAYER O3 and TOTAL O3
!
! Loop over ozone observations
  ozptr => ozNode_typecast(ozhead)
  do while (associated(ozptr))
     if(ozptr%luse)then

        if(nstep > 0)then
!          Get location
           j1=ozptr%ij(1)
           j2=ozptr%ij(2)
           j3=ozptr%ij(3)
           j4=ozptr%ij(4)

!          Accumulate contribution from layer observations
           dz1=nsig+1
        end if

        if ( ozptr%nloz >= 1 ) then

           do k=1,ozptr%nloz
              if(nstep > 0)then
                 val1= -ozptr%res(k)
                 val = zero_quad
                 pob = ozptr%prs(k)
                 iz1 = dz1
                 if (iz1>nsig) iz1=nsig
                 iz2 = pob

                 do kk=iz2,iz1
                    delz=one
                    if (kk==iz1) delz=dz1-iz1
                    if (kk==iz2) delz=delz-pob+iz2
                    w1=ozptr%wij(1,kk)
                    w2=ozptr%wij(2,kk)
                    w3=ozptr%wij(3,kk)
                    w4=ozptr%wij(4,kk)
                    val=val + ( &
                         w1* roz(j1,kk)+ &
                         w2* roz(j2,kk)+ &
                         w3* roz(j3,kk)+ &
                         w4* roz(j4,kk))*delz
                    val1=val1 + ( &
                         w1* soz(j1,kk)+ &
                         w2* soz(j2,kk)+ &
                         w3* soz(j3,kk)+ &
                         w4* soz(j4,kk))*delz
                 end do
                 do kk=1,nstep
                    oz=val1+sges(kk)*val
                    pen(kk)= ozptr%err2(k)*oz*oz
                 end do
              else
                 pen(1)=ozptr%res(k)*ozptr%res(k)*ozptr%err2(k)
              end if

              out(1) = out(1)+pen(1)*ozptr%raterr2(k)
              do kk=2,nstep
                 out(kk) = out(kk)+(pen(kk)-pen(1))*ozptr%raterr2(k)
              end do
              dz1=pob
           end do
           
        end if

!       Add contribution from total column observation
        if(nstep > 0)then
           if (ozptr%apriori(1) .lt. zero) then ! non-OMI ozone
              k   = ozptr%nloz+1
              val1= -ozptr%res(k)
              val  = zero_quad
              do kk=1,nsig
                 w1=ozptr%wij(1,kk)
                 w2=ozptr%wij(2,kk)
                 w3=ozptr%wij(3,kk)
                 w4=ozptr%wij(4,kk)
                 val=val+  (          &
                      w1* roz(j1,kk)+ &
                      w2* roz(j2,kk)+ &
                      w3* roz(j3,kk)+ &
                      w4* roz(j4,kk))
                 val1=val1 +  (       &
                      w1* soz(j1,kk)+ &
                      w2* soz(j2,kk)+ &
                      w3* soz(j3,kk)+ & 
                      w4* soz(j4,kk))
              enddo
           else ! OMI total ozone
              do kl=1,nloz_omi
                 val_lay(kl) = zero_quad
                 val_lay1(kl)= zero_quad
                 k   = ozptr%nloz+1 ! Iz nloz ZERO?
                 val1= -ozptr%res(k)
                 val  = zero_quad

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
                         w1* roz(j1,kk)+ &
                         w2* roz(j2,kk)+ &
                         w3* roz(j3,kk)+ &
                         w4* roz(j4,kk))*delz   
                    val_lay1(kl)=val_lay1(kl) + ( &
                         w1* soz(j1,kk)+ &
                         w2* soz(j2,kk)+ &
                         w3* soz(j3,kk)+ &
                         w4* soz(j4,kk))*delz     
                 enddo
                 dz1=pob 
              end do
              ! Apply the efficiency factor
              do j=1,nloz_omi 
                 val=val+ozptr%efficiency(j)*val_lay(j)
                 val1=val1+ozptr%efficiency(j)*val_lay1(j)
              enddo
           end if
           do kk=1,nstep
              oz=val1+sges(kk)*val
              pen(kk)= ozptr%err2(k)*oz*oz
           end do
        else
           pen(kk)=ozptr%res(k)*ozptr%res(k)*ozptr%err2(k)
        end if

        out(1) = out(1) +pen(1)*ozptr%raterr2(k)
        do kk=2,nstep
           out(kk) = out(kk) +(pen(kk)-pen(1))*ozptr%raterr2(k)
        end do
     end if

     ozptr => ozNode_nextcast(ozptr)

! End of loop over observations
  enddo

! Clean 
  deallocate(soz,roz)

! End of routine.
  return
end subroutine stpozlay
end module stpozmod

module stpo3lmod

!$$$ module documentation block
!           .      .    .                                       .
! module:   stpo3lmod    module for stpoz and its tangent linear stpoz_tl
!  prgmmr:
!
! abstract: module for stpoz and its tangent linear stpoz_tl
!
! program history log:
!   2018-07-13  J. Guo  - splitted from original module stpozmod into this stpo3lmod
!                         with subroutine stpozlev().  See stpozmod for more
!                         about earlier history logs.
!
! subroutines included:
!   sub stpoz
!   sub stpozlev
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

implicit none

PRIVATE
public:: stpozlev       ! Generic interfaces are disirable, where full TKR
                        ! matching are required if they are involked, such as
                        !   call stpozlev(..,pbcjo(:,i_oz_ob_type,ib),..)

contains

subroutine stpozlev(o3lhead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpozlev    compute contribution to penalty and
!                            stepsize for o3 level obs, using nonlinear qc
!   prgmmr: sienkiewicz     org: GMAO                 date: 2006-09-14
!
! abstract: The routine computes the contribution to the penalty from ozone
!           observations.  The routine also computes the contribution of
!           ozone observations to the step size.  This version includes
!           nonlinear qc.
!
! program history log:
!   2006-09-14  sienkiewicz - add level ozone obs
!   2007-01-02  sienkiewicz - separate subroutine
!   2007-01-05  sienkiewicz - update to 9/2006 GSI (new obs structure)
!   2009-01-21  sienkiewicz - update to 1/2009 GSI, changes based on stpq & stpoz
!   2010-01-04  zhang,b - bug fix: accumulate penalty for multiple obs bins
!   2010-05-13  todling - udpate to use gsi_bundle
!
!   input argument list:
!     o3lhead - level ozone obs type pointer to obs structure
!     roz1d  - search direction for ozone (as 1d var)
!     soz1d  - input ozone correction field (as 1d var)
!     sges - step size estimates (4)
!     nstep- number of stepsizes (==0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep) - contribution of ozone data to penalty sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds, only: r_kind,i_kind,r_quad
  use constants, only: zero,one,half,two,r3600
  use gsi_bundlemod, only: gsi_bundle
  use gsi_bundlemod, only: gsi_bundlegetpointer
  use m_obsNode, only: obsNode
  use m_o3lNode, only: o3lNode
  use m_o3lNode, only: o3lNode_typecast
  use m_o3lNode, only: o3lNode_nextcast
  implicit none

! Declare passed variables
  class(obsNode), pointer             ,intent(in   ) :: o3lhead
  integer(i_kind)                     ,intent(in   ) :: nstep
  real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
  type(gsi_bundle)                    ,intent(in   ) :: rval,sval
  real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges

! Declare local variables
  integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk,ier,istatus
  real(r_kind) oz
  real(r_kind),dimension(max(1,nstep))::pen
  real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,val,val2
  real(r_kind),pointer,dimension(:) :: roz1d,soz1d
  type(o3lNode), pointer :: o3lptr

! Get pointers and return if not found
  ier=0
  call gsi_bundlegetpointer(sval,'oz',soz1d,istatus);ier=istatus+ier
  call gsi_bundlegetpointer(rval,'oz',roz1d,istatus);ier=istatus+ier
  if(ier/=0) return

! Initialize output variables to zero

  o3lptr => o3lNode_typecast(o3lhead)

! Loop over level ozone observations
!
  do while (associated(o3lptr))
     if(o3lptr%luse)then
        if(nstep > 0)then
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


           val= w1*roz1d(j1)+w2*roz1d(j2)+w3*roz1d(j3)+w4*roz1d(j4)+ &
                w5*roz1d(j5)+w6*roz1d(j6)+w7*roz1d(j7)+w8*roz1d(j8)   
           val2=w1*soz1d(j1)+w2*soz1d(j2)+w3*soz1d(j3)+w4*soz1d(j4)+ &
                w5*soz1d(j5)+w6*soz1d(j6)+w7*soz1d(j7)+w8*soz1d(j8)-o3lptr%res

           do kk=1,nstep
              oz=val2+sges(kk)*val
              pen(kk)= oz*oz*o3lptr%err2
           end do
        else
           pen(1) = o3lptr%res*o3lptr%res*o3lptr%err2
        end if

        out(1) = out(1)+pen(1)*o3lptr%raterr2
        do kk=2,nstep
           out(kk) = out(kk)+(pen(kk)-pen(1))*o3lptr%raterr2
        end do
     end if

     o3lptr => o3lNode_nextcast(o3lptr)

  end do

! End of routine.
  return
end subroutine stpozlev

end module stpo3lmod
