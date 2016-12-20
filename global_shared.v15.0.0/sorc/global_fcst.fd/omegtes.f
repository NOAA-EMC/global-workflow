      subroutine omegtes(njeff,nsize_ar,rcl,
     &                   expq,dphi,dlam,dg,ug,vg,vvel)
 
      use machine , only : kind_phys
 
      use resol_def
      use coordinate_def						! hmhj
      implicit none
 
      integer njeff,nsize_ar
      integer i,k,n,ilat
 
      real(kind=kind_phys) rcl
      real(kind=kind_phys) dg(nsize_ar,levs), ug(nsize_ar,levs),
     &                     vg(nsize_ar,levs),
     &                     dphi(nsize_ar), dlam(nsize_ar)
 
 
       real(kind=kind_phys)
     &      pk5(njeff,levp1), 
     &      dpk(njeff,levs),     cg(njeff,levs),
     &       cb(njeff,levs),     db(njeff,levs),
     &    workb(njeff,levs),  workc(njeff,levs),  prs(njeff,levs),
     &     alfa(njeff,levs),   rlnp(njeff,levs), rdel(njeff,levs),
     &     vvel(nsize_ar,levs),
     &     expq(nsize_ar)
 
      real(kind=kind_phys) cons0,cons0p5,cons1,cons2,clog2   !constant
 
!     print *,' enter omegtes_fd ' 		! hmhj

      cons0   = 0.d0      !constant
      cons0p5 = 0.5d0     !constant
      cons1   = 1.d0      !constant
      cons2   = 2.d0      !constant
      clog2=log(cons2)     ! constant
 
      do k=1,levp1
      do i=1,njeff
        pk5(i,k)=ak5(k) + bk5(k)*expq(i)
      enddo
      enddo
 
      do k=1,levs
      do i=1,njeff
         prs(i,k)=   (pk5(i,k+1) + pk5(i,k) )*cons0p5
         dpk(i,k)=    pk5(i,k+1) - pk5(i,k)
        rdel(i,k)=    cons1/dpk(i,k)            ! constant
      enddo
      enddo
 
      k=1
      do i=1,njeff
       alfa(i,1)=clog2                        ! constant
       rlnp(i,1)= 99999.99
      enddo
 
      do k=2,levs
      do i=1,njeff
        rlnp(i,k)= log( pk5(i,k+1)/pk5(i,k) )
        alfa(i,k)= cons1-( pk5(i,k)/dpk(i,k) )*rlnp(i,k)
      enddo
      enddo
 
      do k=1,levs
      do i=1,njeff
       cg(i,k)=(ug(i,levs+1-k)*dlam(i)+vg(i,levs+1-k)*dphi(i))*rcl
      enddo
      enddo
 
      k=1
      do i=1,njeff
       db(i,1)=dg(i,levs)*dpk(i,1)
       cb(i,1)=cg(i,1)*dbk(1)
      enddo
 
      do k=1,levm1
      do i=1,njeff
       db(i,k+1)=db(i,k)+dg(i,levs-k)*dpk(i,k+1)
       cb(i,k+1)=cb(i,k)+cg(i,k+1)*dbk(k+1)
      enddo
      enddo

      k=1
      do i=1,njeff
       workb(i,1)=alfa(i,1)*
     &            ( dg(i,levs)*dpk(i,1)+expq(i)*cb(i,1)*dbk(1) )
      enddo
 
      do k=2,levs
      do i=1,njeff
        workb(i,k)=rlnp(i,k)*( db(i,k-1)+expq(i)*cb(i,k-1) )
     &  +alfa(i,k)*( dg(i,levs+1-k)*dpk(i,k)+expq(i)*cg(i,k)*dbk(k) )
      enddo
      enddo
 
      k=1
      do i=1,njeff
       workc(i,1)=expq(i)*cg(i,1)*dbk(1)
      enddo
 
      do k=2,levs
      do i=1,njeff
        workc(i,k)=expq(i)*cg(i,k)*( dbk(k)+ck(k)*rlnp(i,k)*rdel(i,k) )
      enddo
      enddo
 
      do k=1,levs
      do i=1,njeff
       vvel(i,levs+1-k)=rdel(i,k)*( -workb(i,k) + workc(i,k))*prs(i,k)
      enddo
      enddo
 
!     print *,' leave omegtes_fd ' 		! hmhj

      return
      end
