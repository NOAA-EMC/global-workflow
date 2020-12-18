      subroutine read_netcdf_p(ncid,im,jm,levs,
     &   VarName,Varout,Zreverse,iope,ionproc,
     &   iocomms,iret)
!! This subroutine reads either 2d or 3d NetCDF data in parallel
!! 02/08/2020 Guang Ping Lou 

      use netcdf
      use mpi
      implicit none
!!      include 'mpif.h'
      character(len=20),intent(in) :: VarName
      character(len=3),intent(in) :: Zreverse
      integer,intent(in) :: ncid,im,jm,levs
      real,intent(out)   :: Varout(im,jm,levs)
      real               :: dummy3d(im,jm,levs)
      integer            :: iret,i,j,k,id_var,kk
      integer            :: iope,ionproc,iocomms
      integer            :: chunksize,ionproc1
      real, allocatable  :: dummy(:,:,:)
      integer start(3), count(3)
      integer nskip
      integer, allocatable :: starts(:)
      integer, allocatable :: counts(:)
      integer, allocatable :: chunksizes(:)
      integer, allocatable :: rdispls(:)
      integer, allocatable :: ii(:)

      if(levs > 1) then
        nskip = int(levs/ionproc) + 1
        k=ionproc*nskip
        if(k > levs) then
          kk=(k-levs)/nskip
          ionproc1=ionproc - kk
         else
          ionproc1=ionproc
        endif  
        iret = nf90_inq_varid(ncid,trim(VarName),id_var)
        allocate(starts(ionproc1), counts(ionproc1),ii(ionproc1))
        allocate(chunksizes(ionproc1))
        allocate(rdispls(ionproc1))
        print*,'ionproc,ionproc1,nskip= ',ionproc,ionproc1, nskip
        print*,'trim(VarName)in read= ',trim(VarName)
        starts(1) = 1
        ii(1) = 1
        do i = 2, ionproc1
        starts(i) = 1 + (i-1)*nskip
        ii(i)= ii(i-1) + 1
        end do
        do i=1, ionproc1 - 1
        counts(i) = starts(i+1) - starts(i)
        end do
        counts(ionproc1) = levs - starts(ionproc1)+1
        print*,'starts= ',starts
        print*, 'counts= ', counts
        k=ii(iope+1)
        start = (/1,1,starts(k)/)
        count = (/im,jm,counts(k)/)
        chunksizes(:) = im * jm * counts(:)
        rdispls(:) = im * jm * (starts(:)-1)
        print*, 'iope,k,start,count= ',iope,k,start(3),count(3)
        print*, 'chunksizes= ', chunksizes
        print*, 'rdispls= ', rdispls
        allocate (dummy(im,jm,count(3)))
        iret=nf90_get_var(ncid,id_var,dummy,
     &      start=start,count=count)
           if (iret /= 0) then
             print*,VarName," not found"
           endif
      print*,'start(3),st(3):cnt(3)-1=',start(3),(start(3)+count(3)-1)
      print*,'dummy(im/2,jm/2,:)= ', dummy(im/2,jm/2,:)
      call mpi_allgatherv(dummy,chunksizes(k),mpi_real,dummy3d,
     &     chunksizes, rdispls, mpi_real, iocomms, iret)
             print*,'VarName= ', VarName
      print*,'dummy3d(im/2,jm/2,:)= ', dummy3d(im/2,jm/2,:)
!!      call mpi_alltoallv(dummy, chunksizes, sdispls, mpi_real, dummy3d,
!!     & chunksizes, rdispls, mpi_real, iocomms, iret) 

!      enddo
!For FV3GFS NetCDF output, vertical layers need to be reversed
      if(Zreverse == "yes" ) then
            do k = 1, levs
             kk=levs-k+1
             do j=1, jm
              do i=1, im
               Varout(i,j,k) = dummy3d(i,j,kk)
              enddo
             enddo
            enddo
        else
            do k = 1, levs
             do j=1, jm
              do i=1, im
               Varout(i,j,k) = dummy3d(i,j,k)
              enddo
             enddo
            enddo
        endif
        deallocate(starts, counts,ii)
        deallocate(chunksizes)
        deallocate(rdispls)
        deallocate (dummy)

         else
        iret = nf90_inq_varid(ncid,trim(VarName),id_var)
        print*,'trim(VarName)in read= ',trim(VarName)
        iret = nf90_get_var(ncid,id_var,Varout(:,:,1))
        if (iret /= 0) then
          print*,VarName," not found"
        endif

      end if
      end subroutine read_netcdf_p

