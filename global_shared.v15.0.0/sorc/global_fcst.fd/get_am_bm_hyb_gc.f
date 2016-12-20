      subroutine am_bm_hyb_gc
!
! hmhj : this is modified hybrid by finite difference from henry juang
!        work for temperature and enthalpy
!
      use machine , only : kind_phys
 
      use resol_def
      use namelist_def , only : ref_temp
      use coordinate_def
      use physcons, rd => con_rd, cp => con_cp, rearth => con_rerth
      implicit none 

      real(kind=kind_evod)                                              
     &     pk5ref(levp1),dpkref(levs),rpkref(levs),rp2ref(levs),
     &     dbkref(levs), bbkref(levs),rdlref(levs),
     &     hv0(levs),hv(levs),hvk(levs),
     &     c0kref(levp1),dprp,tcprp2,tcpcprp2,
     &     tref,psref,rkaa,kappa,rkappa
      real alpha(levs),betta(levs),gamma(levs),delta(levs)
      real zm(levs-1,levs-1),tm(levs-1,levs)
      real pm(levs,levs-1),wtm(levs-1,levs)
      real dup(levs),dum(levs)
      real det,wmkm1,wmkp1
      integer lll(levs-1),mmm(levs-1)
      integer k,i,j,n
                                                                        
!     print *,' enter  get_am_bm_hyb_gc_fd'

!     psref=80.
      psref=101.316                                                         
      kappa=rd/cp                                                       
      rkappa=1./kappa

      if( thermodyn_id.eq.3 ) then
        tref=ref_temp*cp
        rkaa=kappa/(rearth*rearth)
      else
        tref=ref_temp                                                         
        rkaa=rd/(rearth*rearth)
      endif
                                                                        
      do k=1,levp1                                                      
       pk5ref(k)=ak5(k)+bk5(k)*psref+ck5(k)                                  
      enddo                                                             
                                                                        
      do k=1,levs                                                       
       thref (k)=tref
       dpkref(k)=pk5ref(k)-pk5ref(k+1)                                  
       rdlref(k)=0.5/dpkref(k)
       rpkref(k)=1.0/(pk5ref(k)+pk5ref(k+1)) 
       rp2ref(k)=rpkref(k)*rpkref(k)
       dbkref(k)=bk5(k)-bk5(k+1)                                  
       bbkref(k)=bk5(k)+bk5(k+1)                                  
      enddo                                                             
 
      c0kref=0
      do k=2,levs
       c0kref(k)=ck5(k)*rkappa/(thref(k-1)+thref(k))
      enddo
                                                                        
c sv
      do k=1,levs                                                       
       svhyb(k)=dpkref(k)
      enddo                                                             
                                                                        
c hv
      do k=1,levs
        hv0(k)=thref(k)*rpkref(k)*bbkref(k)
      enddo
      do k=1,levs
        hvk(k)=rpkref(k)*thref(k)*
     &        (dbkref(k)-rpkref(k)*dpkref(k)*bbkref(k))
      enddo
      hv(1)=0.0
      do k=1,levs-1
        hv(k)  = hv(k) + hvk(k)
        hv(k+1)= hv(k) + hvk(k)
      enddo
      hv(levs)  = hv(levs) + hvk(levs)
      do k=1,levs
        tor_hyb(k)=rkaa*(hv0(k)+hv(k))
      enddo

c am
      amhyb = 0.0
! am1+
      do i=1,levs
        amhyb(i,  i)=thref(i)*rpkref(i)*(c0kref(i)+c0kref(i+1))
      enddo
      do i=2,levs
        amhyb(i,i-1)=thref(i)*rpkref(i)*c0kref(i)
      enddo
      do i=1,levs-1
        amhyb(i,i+1)=thref(i)*rpkref(i)*c0kref(i+1)
      enddo
! am2+
      do i=2,levs-1
        tcprp2=thref(i)*c0kref(i)*pk5ref(i+1)*rp2ref(i)
        amhyb(i,i-1)=amhyb(i,i-1)+2.*tcprp2
        do k=i+1,levs
          amhyb(k,i-1)=amhyb(k,i-1)+4.*tcprp2
        enddo
      enddo
! am3+
      do i=1,levs-1
        tcpcprp2=thref(i)*rp2ref(i)*
     &          (c0kref(i)*pk5ref(i+1)-c0kref(i+1)*pk5ref(i))
        amhyb(i,i)=amhyb(i,i)+2.*tcpcprp2
        do k=i+1,levs
          amhyb(k,i)=amhyb(k,i)+4.*tcpcprp2
        enddo
      enddo
! am4+
      do i=1,levs-1
        tcprp2=thref(i)*c0kref(i+1)*pk5ref(i)*rp2ref(i)
        amhyb(i,i+1)=amhyb(i,i+1)-2.*tcprp2
        do k=i+1,levs
          amhyb(k,i+1)=amhyb(k,i+1)-4.*tcprp2
        enddo
      enddo
! am5+
      do i=1,levs
        dprp=dpkref(i)*rpkref(i)
        amhyb(i,i)=amhyb(i,i)+dprp
        do k=i+1,levs
          amhyb(k,i)=amhyb(k,i)+2.*dprp
        enddo
      enddo
! apply rkaa to the sum
      do j=1,levs
        do i=1,levs
          amhyb(i,j)=rkaa*amhyb(i,j)
        enddo
      enddo
c bm
      bmhyb = 0.0
      do i=1,levs
        bmhyb(i,i)=kappa*thref(i)*rpkref(i)*dpkref(i)
      enddo
      do j=2,levs
        do i=1,j-1
          bmhyb(i,j)=2.*kappa*thref(i)*rpkref(i)*dpkref(j)
        enddo
      enddo

c need zm, tm and pm for bm+
! alpha, betta, gamma
      alpha(levs)=0.0
      betta(   1)=0.0
      do k=2,levs
        alpha(k-1)=(pk5ref(k)+pk5ref(k+1))/(pk5ref(k-1)+pk5ref(k))
        alpha(k-1)=alpha(k-1)**kappa
      enddo
      do k=1,levs
        gamma(k)=1.0 - kappa*dpkref(k)*rpkref(k)*2.0
        delta(k)=1.0 + kappa*dpkref(k)*rpkref(k)*2.0
      enddo
      do k=1,levs-1
        betta(k+1)=(pk5ref(k)+pk5ref(k+1))/(pk5ref(k+1)+pk5ref(k+2))
        betta(k+1)=betta(k+1)**kappa
      enddo
! zm
      dup(levs)=0.0
      dum(1 )=0.0
      do k=1,levs-1
        dup(k  )=delta(k)*thref(k)-betta(k+1)*thref(k+1)
        dum(k+1)=alpha(k)*thref(k)-gamma(k+1)*thref(k+1)
      enddo
!
      zm=0.0		! (levs-1,levs-1)
      k=2
        wmkm1=c0kref(k)*rdlref(k-1)
        wmkp1=c0kref(k)*rdlref(  k)
        zm(k-1,k-1)=wmkm1*dup(k-1)+wmkp1*dum(k)-1.0
        zm(k-1,k  )=wmkp1*dup(k)
      do k=3,levs-1
        wmkm1=c0kref(k)*rdlref(k-1)
        wmkp1=c0kref(k)*rdlref(  k)
        zm(k-1,k-2)=wmkm1*dum(k-1)
        zm(k-1,k-1)=wmkm1*dup(k-1)+wmkp1*dum(k)-1.0
        zm(k-1,k  )=wmkp1*dup(k)
      enddo
      k=levs
        wmkm1=c0kref(k)*rdlref(k-1)
        wmkp1=c0kref(k)*rdlref(  k)
        zm(k-1,k-2)=wmkm1*dum(k-1)
        zm(k-1,k-1)=wmkm1*dup(k-1)+wmkp1*dum(k)-1.0
      call iminv(zm,levs-1,det,lll,mmm)
!
! tm
      tm=0.0
      do k=2,levs
        tm(k-1,k-1)=-c0kref(k)*kappa*thref(k-1)*dpkref(k-1)*rpkref(k-1)
        tm(k-1,k  )= c0kref(k)*kappa*thref(k  )*dpkref(k  )*rpkref(k  )
      enddo
      do k=2,levs
        do n=1,levs
          tm(k-1,n)=tm(k-1,n)-bk5(k)*dpkref(n)
        enddo
      enddo
      do k=2,levs
        do n=k,levs
          tm(k-1,n)=tm(k-1,n)+(1.-2.*c0kref(k)*kappa*
     &          (thref(k-1)*rpkref(k-1)+thref(k)*rpkref(k)))*dpkref(n)
        enddo
      enddo
! zm * tm
      wtm=0.0
      do i=1,levs
        do n=1,levs-1
          do j=1,levs-1
            wtm(j,i)=wtm(j,i)+zm(j,n)*tm(n,i)
          enddo
        enddo
      enddo

! pm
      pm=0.0
      k=1
        pm(k,k  )=(delta(k)*thref(k)-betta(k+1)*thref(k+1))*
     &            rdlref(k)
      do k=2,levs-1
        pm(k,k-1)=(alpha(k-1)*thref(k-1)-gamma(k)*thref(k))*
     &            rdlref(k)
        pm(k,k  )=(delta(k)*thref(k)-betta(k+1)*thref(k+1))*
     &            rdlref(k)
      enddo
      k=levs
        pm(k,k-1)=(alpha(k-1)*thref(k-1)-gamma(k)*thref(k))*
     &            rdlref(k)

!
! bm+ = pm * wtm
!
      do i=1,levs
        do k=1,levs
          do j=1,levs-1
            bmhyb(k,i)=bmhyb(k,i)+pm(k,j)*wtm(j,i)
          enddo
        enddo
      enddo
c
    
!     print *,' end of get_am_bm_hyb_gc_fd. '
!!
      return                                                            
      end                                                               
