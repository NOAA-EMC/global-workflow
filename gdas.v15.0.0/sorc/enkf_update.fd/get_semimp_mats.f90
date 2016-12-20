subroutine get_semimp_mats(tbar,pbar,bhat,chat,amat,bmat,hmat,smat)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    get_semimp_mats       compute matrices for semi implicit
!   prgmmr: kleist           org: np20                date: 2007-05-08
!
! abstract: routine to load matrices used in semi-implicit scheme and 
!           vertical mode decomposition
!
! program history log:
!   2007-05-08  kleist
!   2008-06-04  safford - rm unused uses
!
! usage:
!   input argument list:
!     tbar     - profile reference temperature
!     pbar     - profile reference pressure
!     bhat     - vertical coordinate b-coefficient (sigma)
!     chat     - vertical coordinate c-coefficient (isentropic)
!
!   output argument list:
!     amat     - a matrix for semi-implicit
!     bmat     - b matrix for semi-implicit
!     hmat     - h matrix for semi-implicit
!     smat     - s matrix for semi implicit
!
!   notes:
!     taken from global forecast model routine get_am_bm_hyb_gc.f 
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
  use kinds,only: r_kind,i_kind
  use constants,only: zero,half,one,two,four,rd,rd_over_cp
  use gridmod,only: nsig
  implicit none

! Declare passed variables
  real(r_kind),dimension(nsig)     ,intent(in   ) :: tbar
  real(r_kind),dimension(nsig+1   ),intent(in   ) :: pbar,bhat,chat
  real(r_kind),dimension(nsig,nsig),intent(  out) :: amat,bmat
  real(r_kind),dimension(nsig)     ,intent(  out) :: hmat,smat

! Declare local variables
  real(r_kind),dimension(nsig-1,nsig-1):: zm
  real(r_kind),dimension(nsig-1,nsig):: tm,wtm
  real(r_kind),dimension(nsig,nsig-1):: pm
  integer(i_kind),dimension(nsig-1):: lll,mmm
  real(r_kind),dimension(nsig):: dpkref,rpkref,rp2ref,dbkref,bbkref,rdlref,&
       hmat0,hmatk,alpha,beta,gamma,delta,dup,dum
  real(r_kind),dimension(nsig+1):: c0kref
  real(r_kind) kappa,rkappa,dprp,tcprp2,tcpcprp2,det,wmkm1,wmkp1
  integer(i_kind) i,j,k,n

! Constants
  kappa=rd_over_cp
  rkappa=one/kappa

  do k=1,nsig
     dpkref(k)=pbar(k)-pbar(k+1)
     rdlref(k)=half/dpkref(k)
     rpkref(k)=one/(pbar(k)+pbar(k+1))
     rp2ref(k)=rpkref(k)*rpkref(k)
     dbkref(k)=bhat(k)-bhat(k+1)
     bbkref(k)=bhat(k)+bhat(k+1)
  end do

  c0kref=zero
  do k=2,nsig
     c0kref(k)=chat(k)*rkappa/(tbar(k-1)+tbar(k))
  end do

! SMAT :
  do k=1,nsig
     smat(k)=dpkref(k)
  end do

! HMAT :
  do k=1,nsig
     hmat0(k)=tbar(k)*rpkref(k)*bbkref(k)
  end do
  do k=1,nsig
     hmatk(k)=rpkref(k)*tbar(k)* &
          (dbkref(k)-rpkref(k)*dpkref(k)*bbkref(k))
  end do
  hmat(1)=zero
  do k=1,nsig-1
     hmat(k)  = hmat(k) + hmatk(k)
     hmat(k+1)= hmat(k) + hmatk(k)
  end do
  hmat(nsig) = hmat(nsig) + hmatk(nsig)

  do k=1,nsig
     hmat(k) = rd*(hmat0(k)+hmat(k))
  end do

! AMAT :
  amat=zero

! AMAT 1+
  do i=1,nsig
     amat(i,i)=tbar(i)*rpkref(i)*(c0kref(i)+c0kref(i+1))
  end do
  do i=2,nsig
     amat(i,i-1)=tbar(i)*rpkref(i)*c0kref(i)
  end do
  do i=1,nsig-1
     amat(i,i+1)=tbar(i)*rpkref(i)*c0kref(i+1)
  end do

! AMAT 2+
  do i=2,nsig-1
     tcprp2=tbar(i)*c0kref(i)*pbar(i+1)*rp2ref(i)
     amat(i,i-1)=amat(i,i-1) + two*tcprp2
     do k=i+1,nsig
        amat(k,i-1)=amat(k,i-1) + four*tcprp2
     end do
  end do

! AMAT 3+
  do i=1,nsig-1
     tcpcprp2=tbar(i)*rp2ref(i)* &
         (c0kref(i)*pbar(i+1)-c0kref(i+1)*pbar(i))
     amat(i,i)=amat(i,i) + two*tcpcprp2
     do k=i+1,nsig
        amat(k,i)=amat(k,i) + four*tcpcprp2
     end do
  end do

! AMAT 4+
  do i=1,nsig-1
     tcprp2=tbar(i)*c0kref(i+1)*pbar(i)*rp2ref(i)
     amat(i,i+1)=amat(i,i+1) - two*tcprp2
     do k=i+1,nsig
        amat(k,i+1)=amat(k,i+1) - four*tcprp2
     end do
  end do

! AMAT 5+
  do i=1,nsig
     dprp=dpkref(i)*rpkref(i)
     amat(i,i)=amat(i,i) + dprp
     do k=i+1,nsig
        amat(k,i)=amat(k,i) + two*dprp
     end do
  end do

! Apply raa to the sum
  do j=1,nsig
     do i=1,nsig
        amat(i,j)=rd*amat(i,j)
     end do
  end do

! BMAT:
  bmat = zero
  do i=1,nsig
     bmat(i,i)=kappa*tbar(i)*rpkref(i)*dpkref(i)
  end do
  do j=2,nsig
     do i=1,j-1
        bmat(i,j)=two*kappa*tbar(i)*rpkref(i)*dpkref(j)
     end do
  end do

! need zm, tm and pm for bmat+
! alpha, beta, gamma
  alpha(nsig)=zero
  beta(1)=zero
  do k=2,nsig
     alpha(k-1)=(pbar(k)+pbar(k+1))/(pbar(k-1)+pbar(k))
     alpha(k-1)=alpha(k-1)**kappa
  end do
  do k=1,nsig
     gamma(k)=one - kappa*dpkref(k)*rpkref(k)*two
     delta(k)=one + kappa*DPKREF(k)*RPKREF(k)*two
  end do
  do k=1,nsig-1
     beta(k+1)=(pbar(k)+pbar(k+1))/(pbar(k+1)+pbar(k+2))
     beta(k+1)=beta(k+1)**kappa
  end do

! ZM :
  dup(nsig)=zero
  dum(1)=zero
  do k=1,nsig-1
     dup(k)=delta(k)*tbar(k)-beta(k+1)*tbar(k+1)
     dum(k+1)=alpha(k)*tbar(k)-gamma(k+1)*tbar(k+1)
  end do
!
  zm=zero            ! (levs-1,levs-1)
  k=2
  wmkm1=c0kref(k)*rdlref(k-1)
  wmkp1=c0kref(k)*rdlref(k)
  zm(k-1,k-1)=wmkm1*dup(k-1)+wmkp1*dum(k)-one
  zm(k-1,k)=wmkp1*dup(k)

  do k=3,nsig-1
     wmkm1=c0kref(k)*rdlref(k-1)
     wmkp1=c0kref(k)*rdlref(     k)
     zm(k-1,k-2)=wmkm1*dum(k-1)
     zm(k-1,k-1)=wmkm1*dup(k-1)+wmkp1*dum(k)-one
     zm(k-1,k         )=wmkp1*dup(k)
  end do
  k=nsig
  wmkm1=c0kref(k)*rdlref(k-1)
  wmkp1=c0kref(k)*rdlref(     k)
  zm(k-1,k-2)=wmkm1*dum(k-1)
  zm(k-1,k-1   )=wmkm1*dup(k-1)+wmkp1*dum(k)-one

  call iminv(zm,nsig-1,det,lll,mmm)

! TM :
  tm=zero
  do k=2,nsig
     tm(k-1,k-1)=-c0kref(k)*kappa*tbar(k-1)*dpkref(k-1)*rpkref(k-1)
     tm(k-1,k     )= c0kref(k)*kappa*tbar(k     )*dpkref(k     )*rpkref(k     )
  end do
  do k=2,nsig
     do n=1,nsig
        tm(k-1,n)=tm(k-1,n)-bhat(k)*dpkref(n)
     end do
  end do
  do k=2,nsig
     do n=k,nsig
        tm(k-1,n)=tm(k-1,n)+(one-two*c0kref(k)*kappa*  &
                 (tbar(k-1)*rpkref(k-1) + tbar(k)*rpkref(k)))*dpkref(n)
     enddo
  enddo

! ZM*TM :
  wtm=zero
  do i=1,nsig
     do n=1,nsig-1
        do j=1,nsig-1
           wtm(j,i)=wtm(j,i)+zm(j,n)*tm(n,i)
        end do
     end do
  end do

! PM :
  pm=zero
  k=1
  pm(k,k)=(delta(k)*tbar(k)-beta(k+1)*tbar(k+1))*rdlref(k)
  do k=2,nsig-1
     pm(k,k-1)=(alpha(k-1)*tbar(k-1)-gamma(k)*tbar(k))*rdlref(k)
     pm(k,k     )=(delta(k     )*tbar(k     )-beta(k+1)*tbar(k+1))*rdlref(k)
  end do
  k=nsig
  pm(k,k-1)=(alpha(k-1)*tbar(k-1)-gamma(k)*tbar(k))*rdlref(k)

! BMAT+ = PM*WTM:
  do i=1,nsig
     do k=1,nsig
        do j=1,nsig-1

!          ***NOTE:  NOT SURE ABOUT THIS PART??
!          bmat(k,i)=bmat(k,i)+pm(k,j)*wtm(j,i)

        end do
     end do
  end do

  return
end subroutine get_semimp_mats


      subroutine iminv (a,n,d,l,m)                                              
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    iminv    invert a matrix
!
!   prgrmmr:
!
! abstract:      the standard gauss-jordan method is used. the determinant           
!                is also calculated. a determinant of zero indicates that            
!                the matrix is singular.                                             
!                                                                               
! remarks        matrix a must be a general matrix                                   
!
! program history log:
!   2008-06-04  safford -- add subprogram doc block
!
!   input argument list:
!     a - input matrix, destroyed in computation and replaced by resultant inverse
!     n - order of matrix a                                               
!     d - resultant determinant                                           
!     l - work vector of length n                                         
!     m - work vector of length n                                         
!
!   output argument list:
!     a - input matrix, destroyed in computation and replaced by resultant inverse
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

        use kinds,only: r_kind,i_kind
        use constants,only: zero,one
        implicit none

        integer(i_kind)             ,intent(in   ) :: n
        integer(i_kind),dimension(n),intent(inout) :: l,m
        real(r_kind)                ,intent(inout) :: d
        real(r_kind),dimension(n*n) ,intent(inout) :: a

        integer(i_kind):: nk,k,j,iz,i,ij
        integer(i_kind):: kj,ik,jr,jq,jk,ki,kk,jp,ji

        real(r_kind):: biga,hold
!                                                                               
!        if a double precision version of this routine is desired, the          
!        ! in column 1 should be removed from the double precision              
!        statement which follows.                                               
!                                                                               
!     double precision a, d, biga, hold                                         
!                                                                               
!        the ! must also be removed from double precision statements            
!        appearing in other routines used in conjunction with this              
!        routine.                                                               
!                                                                               
!        the double precision version of this sr........ must also              
!        contain double precision fortran functions.  abs in statemen           
!        10 must be changed to dabs  .                                          
!                                                                               
!        ...............................................................        
!                                                                               
!        search for largest element                                             
!                                                                               
         d=one
         nk=-n
         do 80 k=1,n
            nk=nk+n
            l(k)=k
            m(k)=k
            kk=nk+k
            biga=a(kk)
            do 20 j=k,n
               iz=n*(j-1)
               do 20 i=k,n
                  ij=iz+i
!  10             if (dabs(biga)-dabs(a(ij))) 15,20,20
   10             if(abs(biga)-abs(a(ij))) 15,20,20
   15             biga=a(ij)
                  l(k)=i
                  m(k)=j
   20       continue
!
!        interchange rows
!
            j=l(k)
            if(j-k) 35,35,25
   25       ki=k-n
            do 30 i=1,n
               ki=ki+n
               hold=-a(ki)
               ji=ki-k+j
               a(ki)=a(ji)
   30          a(ji) =hold
!
!        interchange columns
!
   35          i=m(k)
               if(i-k) 45,45,38
   38          jp=n*(i-1)
               do 40 j=1,n
                  jk=nk+j
                  ji=jp+j
                  hold=-a(jk)
                  a(jk)=a(ji)
   40             a(ji) =hold
!
!        divide column by minus pivot (value of pivot element is
!        contained in biga)
!
   45             if(biga) 48,46,48
   46             d=zero 
                  return
   48             do 55 i=1,n
                     if(i-k) 50,55,50
   50                ik=nk+i
                     a(ik)=a(ik)/(-biga)
   55             continue
!
!        reduce matrix
!
                  do 65 i=1,n
                     ik=nk+i
                     ij=i-n
                     do 65 j=1,n
                        ij=ij+n
                        if(i-k) 60,65,60
   60                   if(j-k) 62,65,62
   62                   kj=ij-i+k
                        a(ij)=a(ik)*a(kj)+a(ij)
   65             continue
!
!        divide row by pivot
!
                  kj=k-n
                  do 75 j=1,n
                     kj=kj+n
                     if(j-k) 70,75,70
   70                a(kj)=a(kj)/biga
   75             continue
!
!        product of pivots
!
                  d=d*biga
!
!        replace pivot by reciprocal
!
                  a(kk)=one/biga
   80    continue
!
!        final row and column interchange
!
         k=n
  100    k=(k-1)
         if(k) 150,150,105
  105    i=l(k)
         if(i-k) 120,120,108
  108    jq=n*(k-1)
         jr=n*(i-1)
         do 110 j=1,n
            jk=jq+j
            hold=a(jk)
            ji=jr+j
            a(jk)=-a(ji)
  110       a(ji) =hold
  120       j=m(k)
            if(j-k) 100,100,125
  125       ki=k-n
            do 130 i=1,n
               ki=ki+n
               hold=a(ki)
               ji=ki-k+j
               a(ki)=-a(ji)
  130          a(ji) =hold
               go to 100
  150          return
      end subroutine iminv
