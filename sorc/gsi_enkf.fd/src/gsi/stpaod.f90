module stpaodmod
  
!$$$ module documentation block
!           .      .    .                                       .
! module:   stpaodmod    
!  pgrmmr: pagowski org:NOAA/ESRL  date: 2016-02-20
!
! abstract: module to calculate penalty and contribution to stepsize from aod
!
! program history log:
!   2016-02-20  pagowski - a module for aod 
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!   2019-03-21  martin  - changed if in stpaod from wrf_mass_regional to
!                         laeroana_gocart; modified code from S-W Wei (UAlbany)
!
! subroutines included:
!   sub stpaod
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use m_obsNode , only: obsNode
  use m_aeroNode, only: aeroNode
  use m_aeroNode, only: aeroNode_typecast
  use m_aeroNode, only: aeroNode_nextcast
  implicit none

  private
  public stpaod
  
contains
  
  subroutine stpaod(aerohead,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stpaod        calcuate penalty and stepsize from aod
!                            with addition of nonlinear qc.
!   prgmmr: pagowski org:NOAA/ESRL  date: 2016-02-20
!
! abstract: calculate penalty and contribution to stepsize from aod
!
! program history log:
!   2016-01-15  pagowski - original routine 
!   2019-03-21  martin  - changed if in stpaod from wrf_mass_regional to
!                         laeroana_gocart; modified code from S-W Wei (UAlbany)
!
!   input argument list:
!     aerohead
!     rv_chem       - search direction for aero
!     sv_chem       - analysis increment for aero
!     sges     - stepsize estimates (nstep)
!     nstep    - number of stepsize estimates (== 0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution of penalty from aod sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$
    use kinds, only: r_kind,i_kind,r_quad
    use aeroinfo, only: aerojacnames,nsigaerojac
    use constants, only: half,one,two,zero_quad,zero
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gridmod, only: cmaq_regional,latlon11,nsig
    use chemmod, only: laeroana_gocart,laeroana_fv3cmaq
    implicit none
    
! declare passed variables
    class(obsNode), pointer             ,intent(in   ) :: aerohead
    integer(i_kind)                     ,intent(in   ) :: nstep
    real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
    type(gsi_bundle)                    ,intent(in   ) :: rval,sval
    real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
    
! declare local variables
    integer(i_kind) istatus,naero
    integer(i_kind) j1,j2,j3,j4,kk,k,ic,nn
    real(r_kind) val,val2
    integer(i_kind),dimension(nsig) :: j1n,j2n,j3n,j4n
    real(r_kind),dimension(max(1,nstep)):: term,rad
    real(r_kind) w1,w2,w3,w4
    real(r_kind),pointer,dimension(:):: sv_chem,rv_chem
    type(aeroNode), pointer :: aeroptr
    real(r_kind),dimension(nsigaerojac) :: tdir,rdir

    out=zero_quad

    naero = size(aerojacnames)
    if ( naero <= 0 ) return


!   if no aero data return
    if(.not. associated(aerohead))return

    if (cmaq_regional) then

       write(6,*)'aod for cmaq not implemented. stopping'       
       call stop2(460)

    endif

    if (laeroana_gocart .or. laeroana_fv3cmaq) then

       tdir=zero
       rdir=zero

       aeroptr => aeroNode_typecast(aerohead)

       do while (associated(aeroptr))
          if(aeroptr%luse)then
             if(nstep > 0)then

                j1=aeroptr%ij(1)
                j2=aeroptr%ij(2)
                j3=aeroptr%ij(3)
                j4=aeroptr%ij(4)
                w1=aeroptr%wij(1)
                w2=aeroptr%wij(2)
                w3=aeroptr%wij(3)
                w4=aeroptr%wij(4)

                j1n(1) = j1
                j2n(1) = j2
                j3n(1) = j3
                j4n(1) = j4

                do k=2,nsig
                   j1n(k) = j1n(k-1)+latlon11
                   j2n(k) = j2n(k-1)+latlon11
                   j3n(k) = j3n(k-1)+latlon11
                   j4n(k) = j4n(k-1)+latlon11
                enddo

                do ic = 1, naero
                   call gsi_bundlegetpointer (sval,trim(aerojacnames(ic)),sv_chem,istatus)
                   call gsi_bundlegetpointer (rval,trim(aerojacnames(ic)),rv_chem,istatus)

                   do k=1,nsig
                      j1 = j1n(k)
                      j2 = j2n(k)
                      j3 = j3n(k)
                      j4 = j4n(k)
                      tdir(k+nsig*(ic-1))=&
                           w1* sv_chem(j1)+w2* sv_chem(j2)+ &
                           w3* sv_chem(j3)+w4* sv_chem(j4)

                      rdir(k+nsig*(ic-1))=&
                           w1* rv_chem(j1)+w2* rv_chem(j2)+ &
                           w3* rv_chem(j3)+w4* rv_chem(j4)

                   end do
                   nullify(sv_chem,rv_chem)
                end do

             endif


             do nn=1,aeroptr%nlaero
                ic=aeroptr%icx(nn)

                val2=-aeroptr%res(nn)

                if(nstep > 0)then
                   val = zero

                   do k=1,nsigaerojac
                      val2=val2+tdir(k)*aeroptr%daod_dvar(k,nn)
                      val =val +rdir(k)*aeroptr%daod_dvar(k,nn)
                   end do

                   do kk=1,nstep
                      rad(kk)=val2+sges(kk)*val
                   end do

                else
                   rad(1) = aeroptr%res(nn)
                end if

!          calculate contribution to j

                do kk=1,max(1,nstep)
                   term(kk)  = aeroptr%err2(nn)*rad(kk)*rad(kk)
                end do

!          modify penalty term if nonlinear qc

                out(1) = out(1) + term(1)*aeroptr%raterr2(nn)

                do kk=2,nstep
                   out(kk) = out(kk) + (term(kk)-term(1))*aeroptr%raterr2(nn)
                end do

             end do

          endif

          aeroptr => aeroNode_nextcast(aeroptr)

       end do

    endif

    return

  end subroutine stpaod

end module stpaodmod
