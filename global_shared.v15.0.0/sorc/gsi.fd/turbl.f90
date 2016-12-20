subroutine turbl(uges,vges,pges,tges,oges,zges,termu,termv,termt,jstart,jstop)

!$$$  subprogram documentation block
!                .      .    .
! subprogram:    turbl
!
!   prgrmmr: 
!
! abstract:      Calculate tendencies of wind and virtual temperature
!                due to vertical turbulent mixing. 
!
! program history log:
!   2008-04-02  safford -- add subprogram doc block, rm unused uses
!   2010-11-03  derber - added jstart and jstop for threading use
!
!   input argument list:
!     zges     -
!     pges     -
!     uges     - 
!     vges     - 
!     tges     - 
!     termu    - 
!     termv    - 
!     termt    - 
!     oges     - 
!     jstart   - starting point of j loop
!     jstop    - stopping point of j loop
!
!   output argument list:
!     termu    - 
!     termv    - 
!     termt    - 
!     oges     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$

  use kinds,only: r_kind,i_kind
  use constants,only: zero,one,two,half,rd_over_g,rd_over_cp,grav
  use gridmod,only: lat2,lon2,nsig,nsig_hlf
  use turblmod, only: use_pbl
  use turblmod, only: dudz,dvdz,dodz,ri,rf,zi,km,kh,sm,sh
  use turblmod, only: lmix,dudtm,dvdtm,dtdtm,rdzi,rdzl
  use turblmod, only: kar0my20
  use turblmod, only: a0my20,b0my20,c0my20,d0my20,f1my20,f2my20, &
                      f3my20,f4my20,f5my20,f6my20,f7my20,f8my20,b1my20, &
                      karmy20,l0my20
  use turblmod, only: ricmy20,rfcmy20,shcmy20,smcmy20,eps_m
  use turblmod, only: ri_int
  implicit none

! Declare local parameters
  real(r_kind),parameter:: r200 = 200.0_r_kind

! Declare passed variables
  real(r_kind),dimension(lat2,lon2)       ,intent(in   ) :: zges
  real(r_kind),dimension(lat2,lon2,nsig+1),intent(in   ) :: pges
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(in   ) :: uges,vges,tges
  real(r_kind),dimension(lat2,lon2,nsig)  ,intent(inout) :: termu,termv,termt,oges
  integer(i_kind)                         ,intent(in   ) :: jstart,jstop

! Declare local variables
  real(r_kind),dimension(nsig_hlf):: zl,kmaz,khaz
  real(r_kind),dimension(nsig_hlf):: tloc,uloc,vloc,oloc,zmix
  real(r_kind),dimension(nsig_hlf+1):: ploc

  real(r_kind) px,rdzik,rdzlk,kmrdz,khrdz,ssq,aux,l0
  integer(i_kind) i,j,k
  
  if(.not. use_pbl)return

  do k=1,nsig_hlf
     do j=jstart,jstop
        do i=1,lat2
           oges(i,j,k)=tges(i,j,k)*(                  &
               r200/( pges(i,j,k)+pges(i,j,k+1) ))**rd_over_cp
        end do
     end do
  end do

  do j=jstart,jstop
     do i=1,lat2
        do k=1,nsig_hlf
           tloc(k)=tges(i,j,k)
           uloc(k)=uges(i,j,k)
           vloc(k)=vges(i,j,k)
           ploc(k)=pges(i,j,k)
           oloc(k)=oges(i,j,k)
        end do
        ploc(nsig_hlf+1)=pges(i,j,nsig_hlf+1)
        zi(i,j,1) = zges(i,j)
        do k=1,nsig_hlf
           zi(i,j,k+1)=zi(i,j,k)-rd_over_g*two*tloc(k) &
                    *(ploc(k+1)-ploc(k))/(ploc(k+1)+ploc(k))
           zl(k)=half*(zi(i,j,k+1)+zi(i,j,k))
           rdzi(i,j,k)=one/(zi(i,j,k+1)-zi(i,j,k))
        end do

!m      l0=l0my20 *0.1_r_kind   !  8 m
        l0=l0my20 *0.15_r_kind  !  12 m
!m      l0=l0my20 *0.20_r_kind  !  16 m
!m      l0=l0my20 *0.25_r_kind  !  20 m
!m      l0=l0my20 *0.5_r_kind   !  40 m
!m      l0=l0my20        !  80 m
        kar0my20(i,j)=karmy20/l0

        do k=2,nsig_hlf
           rdzl(i,j,k)=one/(zl(k)-zl(k-1))
        end do

        do k=2,nsig_hlf
           rdzlk=rdzl(i,j,k)
           dodz(i,j,k)=(oloc(k)-oloc(k-1))*rdzlk
           dudz(i,j,k)=(uloc(k)-uloc(k-1))*rdzlk
           dvdz(i,j,k)=(vloc(k)-vloc(k-1))*rdzlk
           zmix(k)=zi(i,j,k)-zi(i,j,1)
           ssq=dudz(i,j,k)**2+dvdz(i,j,k)**2
           if(ssq < eps_m) ssq=eps_m
           lmix(i,j,k)=karmy20*zmix(k)/(one+kar0my20(i,j)*zmix(k))
           ri(i,j,k)=two*grav*dodz(i,j,k)/((tloc(k)+tloc(k-1))*ssq)
           if(ri(i,j,k) > ricmy20) then
              ri_int(i,j,k)=1
              ri(i,j,k)=ricmy20
              rf(i,j,k)=rfcmy20
              sh(i,j,k)=shcmy20
              sm(i,j,k)=smcmy20
              km(i,j,k)=zero
              kh(i,j,k)=zero
           else
              ri_int(i,j,k)=0
              rf(i,j,k)=a0my20*(ri(i,j,k)+b0my20-sqrt(ri(i,j,k)**2-c0my20*ri(i,j,k)+d0my20))
              sh(i,j,k)=f1my20*(f2my20-f3my20*rf(i,j,k))/(one-rf(i,j,k))
              sm(i,j,k)=f4my20*(f5my20-f6my20*rf(i,j,k))/(f7my20-f8my20*rf(i,j,k))*sh(i,j,k)
              aux=sqrt(ssq*b1my20*(one-rf(i,j,k))*sm(i,j,k))*lmix(i,j,k)**2
              km(i,j,k)=aux*sm(i,j,k) 
              kh(i,j,k)=aux*sh(i,j,k) 
           end if
        end do
        km(i,j,1)=zero; km(i,j,nsig_hlf+1)=zero
        kh(i,j,1)=zero; kh(i,j,nsig_hlf+1)=zero
     end do
  end do
  
  do j=jstart,jstop
     do i=1,lat2
        do k=1,nsig_hlf
           kmaz(k)=(km(i,j,k)+km(i,j,k+1))*half
           khaz(k)=(kh(i,j,k)+kh(i,j,k+1))*half
        end do
 
        dudtm(i,j,nsig_hlf)=zero
        dvdtm(i,j,nsig_hlf)=zero
        dtdtm(i,j,nsig_hlf)=zero

        do k=1,nsig_hlf
           px=tges(i,j,k)/oges(i,j,k)
           rdzik=rdzi(i,j,k)
           kmrdz=kmaz(k)*rdzik
           khrdz=khaz(k)*rdzik
           if(k < nsig_hlf)then
              dudtm(i,j,k)=kmrdz*dudz(i,j,k+1)
              dvdtm(i,j,k)=kmrdz*dvdz(i,j,k+1)
              dtdtm(i,j,k)=khrdz*dodz(i,j,k+1)*px
           end if
           if(k > 1)then
              dudtm(i,j,k)=dudtm(i,j,k)-kmrdz*dudz(i,j,k)
              dvdtm(i,j,k)=dvdtm(i,j,k)-kmrdz*dvdz(i,j,k)
              dtdtm(i,j,k)=dtdtm(i,j,k)-khrdz*dodz(i,j,k)*px
           end if
           termu(i,j,k)=termu(i,j,k)+dudtm(i,j,k)
           termv(i,j,k)=termv(i,j,k)+dvdtm(i,j,k)
           termt(i,j,k)=termt(i,j,k)+dtdtm(i,j,k)
        end do
     end do
  end do

  return
end subroutine turbl
