      subroutine para_we2ns(a,b,lev,global_lats_a,latg)
!
! mpi transport from full dimension of west-east to full dimension of 
! north-south with latitude shuffl. created by hann-ming henry juang
! 
! program log
! 2011 02 20 : henry juang, created for ndsl advection
!
!
      use mpi_def
      use layout1
      implicit none
!
      integer lev,latg
      real a(lonfull,lev,latpart)
      real b(latfull,lev,lonpart)
      integer global_lats_a(latg)
!
!     real (kind=kind_mpi_r) works(2*lev*mylatlen*lonhalf)
!     real (kind=kind_mpi_r) workr(2*lev*mylonlen*lathalf)
      real (kind=kind_mpi) works(2,lev,lonlenmax*latlenmax,nodes)
      real (kind=kind_mpi) workr(2,lev,lonlenmax*latlenmax,nodes)
      integer lensend(nodes),lenrecv(nodes)
      integer locsend(nodes),locrecv(nodes)
      integer i,j,k,n,mn,jj,lat1,lat2,ierr
!
!     print *,' enter ndslfv_we2ns '

!$omp parallel do private(n,mn,i,j,k)
      do n=1,nodes 
        mn=0
        do j=1,mylatlen
          do i=lonstr(n),lonstr(n)+lonlen(n)-1
            mn = mn + 1
            do k=1,lev
              works(1,k,mn,n)=a(i        ,k,j)
              works(2,k,mn,n)=a(i+lonhalf,k,j)
            enddo
          enddo
        enddo
        lensend(n) = mn * 2 * lev
        locsend(n) = (n-1)*lonlenmax*latlenmax*2*lev
        lenrecv(n)=latlen(n)*mylonlen*2*lev
        locrecv(n)=locsend(n)
      enddo
!
      call mpi_barrier (mc_comp,ierr)
!     print *,' mpi_barrier ierr=',ierr
      
!     call mpi_alltoallv(works,lensend,locsend,mpi_r_mpi_r,
!    &                   workr,lenrecv,locrecv,mpi_r_mpi_r,
!    &                   mc_comp,ierr)
      call mpi_alltoallv(works,lensend,locsend,mpi_r_mpi,
     &                   workr,lenrecv,locrecv,mpi_r_mpi,
     &                   mc_comp,ierr)
!     print *,' mpi_alltoallv ierr=',ierr
     
!
!$omp parallel do private(n,mn,i,j,jj,lat1,lat2,k)
      do n=1,nodes
        mn=0
        do j=1,latlen(n)
          jj = latstr(n) + j - 1
          lat1=global_lats_a(jj)
          lat2=latfull+1-lat1
          do i=1,mylonlen
            mn = mn + 1
            do k=1,lev
              b(lat1,k,i) = workr(1,k,mn,n)
              b(lat2,k,i) = workr(2,k,mn,n)
            enddo
          enddo
        enddo
      enddo
!
!     print *,' end of ndslfv_we2ns '

      return
      end

! ======================================================================
      subroutine para_ns2we(a,b,lev,global_lats_a,latg)
!
! mpi transport from full dimension of west-east to full dimension of 
! north-south with latitude shuffl.
!
      use mpi_def
      use layout1
      implicit none
!
      integer lev,latg
      real a(latfull,lev,lonpart)
      real b(lonfull,lev,latpart)
      integer global_lats_a(latg)
!
!     real (kind=kind_mpi_r) works(2*lev*mylonlen*lathalf)
!     real (kind=kind_mpi_r) workr(2*lev*mylatlen*lonhalf)
      real (kind=kind_mpi) works(2,lev,lonlenmax*latlenmax,nodes)
      real (kind=kind_mpi) workr(2,lev,lonlenmax*latlenmax,nodes)
      integer lensend(nodes),lenrecv(nodes)
      integer locsend(nodes),locrecv(nodes)
      integer i,j,k,n,mn,jj,lat1,lat2,ierr
!
!     print *,' enter ndslfv_ns2we '

!$omp parallel do private(n,mn,i,j,jj,lat1,lat2,k)
      do n=1,nodes
        mn=0
        do j=1,latlen(n)
          jj=latstr(n) + j -1
          lat1=global_lats_a(jj)
          lat2=latfull+1-lat1
          do i=1,mylonlen
            mn = mn + 1
            do k=1,lev
              works(1,k,mn,n) = a(lat1,k,i)
              works(2,k,mn,n) = a(lat2,k,i)
            enddo
          enddo
        enddo
        lensend(n)=mn*2*lev
        locsend(n)=(n-1)*lonlenmax*latlenmax*2*lev
        lenrecv(n)=mylatlen*lonlen(n)*2*lev
        locrecv(n)=locsend(n)
      enddo
!
      call mpi_barrier (mc_comp,ierr)
!     call mpi_alltoallv(works,lensend,locsend,mpi_r_mpi_r,
!    &                   workr,lenrecv,locrecv,mpi_r_mpi_r,
!    &                   mc_comp,ierr)
      call mpi_alltoallv(works,lensend,locsend,mpi_r_mpi,
     &                   workr,lenrecv,locrecv,mpi_r_mpi,
     &                   mc_comp,ierr)
!
!$omp parallel do private(n,mn,i,j,k)
      do n=1,nodes 
        mn=0
        do j=1,mylatlen
          do i=lonstr(n),lonstr(n)+lonlen(n)-1
            mn = mn + 1
            do k=1,lev
              b(i        ,k,j) = workr(1,k,mn,n)
              b(i+lonhalf,k,j) = workr(2,k,mn,n)
            enddo
          enddo
        enddo
      enddo
!
!     print *,' end of ndslfv_ns2we '
!
      return
      end
