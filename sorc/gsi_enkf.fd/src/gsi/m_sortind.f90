module m_sortind

!$$$ module documentation block
!           .      .    .                                       .
! module:   m_sortind   finds indices to sort an array in ascending order 
!   prgmmr: eliu
!
! abstract: module to find indices to sort an array in ascending order 
! assimilation
!
! program history log:
!   1996-10-01  Joiner/Karki - initial coding from NASA/GMAO
!   2012-02-15  eliu         - reformat to use in GSI
!
! subroutines included:
!
! variable definitions:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

  use kinds,only : i_kind, r_kind
  interface sortind
    module procedure r_sortind
    module procedure i_sortind
  end interface

  contains

  function r_sortind(arr) result(arr2)
  implicit none

  !input parameters:
  real(r_kind), dimension(:) :: arr ! input vector to sort
  !output parameters:
  integer(i_kind), dimension(size(arr)) :: arr2

  call indexx(size(arr),arr, arr2)

  end function r_sortind

  function i_sortind(arr) result(arr2)

  implicit none

  integer(i_kind), dimension(:) :: arr
  integer(i_kind), dimension(size(arr)) :: arr2

  call iindexx(size(arr),arr, arr2)

  end function i_sortind

  SUBROUTINE indexx(n,arr,indx)

  INTEGER(i_kind):: n,indx(n),M,NSTACK
  REAL(r_kind)   ::  arr(n)
  PARAMETER (M=7,NSTACK=50)
  INTEGER(i_kind)::  i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
  REAL(r_kind)   ::  a
   do 11 j=1,n
      indx(j)=j
11 continue
   jstack=0
   l=1
   ir=n
   loop2: do 
      if(ir-l.lt.M)then
         loop: do j=l+1,ir
            indxt=indx(j)
            a=arr(indxt)
            do 12 i=j-1,1,-1
               if(arr(indx(i)).le.a)then
                  indx(i+1)=indxt
                  cycle loop 
               end if
               indx(i+1)=indx(i)
12          continue
            indx(1)=indxt
         end do loop
         if(jstack.eq.0)return
         ir=istack(jstack)
         l=istack(jstack-1)
         jstack=jstack-2
      else
         k=(l+ir)/2
         itemp=indx(k)
         indx(k)=indx(l+1)
         indx(l+1)=itemp
         if(arr(indx(l+1)) > arr(indx(ir)))then
            itemp=indx(l+1)
            indx(l+1)=indx(ir)
            indx(ir)=itemp
         endif
         if(arr(indx(l)) > arr(indx(ir)))then
            itemp=indx(l)
            indx(l)=indx(ir)
            indx(ir)=itemp
         endif
         if(arr(indx(l+1)) >  arr(indx(l)))then
            itemp=indx(l+1)
            indx(l+1)=indx(l)
            indx(l)=itemp
         endif
         i=l+1
         j=ir
         indxt=indx(l)
         a=arr(indxt)
         loop1: do 
            i=i+1
            if(arr(indx(i)).lt.a)cycle loop1
            do 
4           continue
               j=j-1
               if(arr(indx(j))<=a)exit
            end do
            if(j.lt.i)exit loop1
            itemp=indx(i)
            indx(i)=indx(j)
            indx(j)=itemp
         end do loop1
5        indx(l)=indx(j)
         indx(j)=indxt
         jstack=jstack+2
         if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
         if(ir-i+1.ge.j-l)then
            istack(jstack)=ir
            istack(jstack-1)=i
            ir=j-1
         else
            istack(jstack)=j-1
            istack(jstack-1)=l
            l=i
         endif
      endif
   end do loop2
  END subroutine indexx

  SUBROUTINE iindexx(n,arr,indx)
  INTEGER(i_kind):: n,indx(n),M,NSTACK
  INTEGER(i_kind):: arr(n)
  PARAMETER (M=7,NSTACK=50)
  INTEGER(i_kind):: i,indxt,ir,itemp,j,jstack,k,l,istack(NSTACK)
  INTEGER(i_kind):: a
   do 11 j=1,n
      indx(j)=j
11 continue
   jstack=0
   l=1
   ir=n
   loop2: do 
      if(ir-l.lt.M)then
         loop: do j=l+1,ir
            indxt=indx(j)
            a=arr(indxt)
            do 12 i=j-1,1,-1
               if(arr(indx(i)).le.a)then
                 indx(i+1)=indxt
                 cycle loop
               end if
               indx(i+1)=indx(i)
12          continue
            indx(1)=indxt
         end do loop
         if(jstack.eq.0)return
         ir=istack(jstack)
         l=istack(jstack-1)
         jstack=jstack-2
      else
         k=(l+ir)/2
         itemp=indx(k)
         indx(k)=indx(l+1)
         indx(l+1)=itemp
         if(arr(indx(l+1)).gt.arr(indx(ir)))then
            itemp=indx(l+1)
            indx(l+1)=indx(ir)
            indx(ir)=itemp
         endif
         if(arr(indx(l)).gt.arr(indx(ir)))then
            itemp=indx(l)
            indx(l)=indx(ir)
            indx(ir)=itemp
         endif
         if(arr(indx(l+1)).gt.arr(indx(l)))then
            itemp=indx(l+1)
            indx(l+1)=indx(l)
            indx(l)=itemp
         endif
         i=l+1
         j=ir
         indxt=indx(l)
         a=arr(indxt)
         loop1: do 
            do 
               i=i+1
               if(arr(indx(i))>=a)exit
            end do
            do 
               j=j-1
               if(arr(indx(j))<=a)exit
            end do
            if(j.lt.i)exit loop1
           itemp=indx(i)
           indx(i)=indx(j)
           indx(j)=itemp
         end do loop1
         indx(l)=indx(j)
         indx(j)=indxt
         jstack=jstack+2
         if(jstack.gt.NSTACK)pause 'NSTACK too small in indexx'
         if(ir-i+1.ge.j-l)then
            istack(jstack)=ir
            istack(jstack-1)=i
            ir=j-1
         else
            istack(jstack)=j-1
            istack(jstack-1)=l
            l=i
         endif
      endif
   end do loop2
  END subroutine iindexx

end module m_sortind
