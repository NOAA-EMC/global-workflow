!
!                                **********************************************
!                                *             MODULE psort                   *
!                                *  R. J. Purser, NOAA/NCEP/EMC   21 Sep 2012 *
!                                *  jim.purser@noaa.gov                       *
!                                *                                            *
!                                **********************************************
! Module dealing with binary sorting and ranking procedures
!
! DEPENDENCIES
! Modules: kinds 
!=============================================================================
module psort
!=============================================================================
use kinds, only: sp, dp,i_kind
implicit none
private
public:: sort,bsort,mergeab,invertperm
      
interface sort
   module procedure sort_s, sort_d, sort_si, sort_di, &
                    rsort_s,rsort_d,rsort_si,rsort_di;           end interface
interface bsort
   module procedure bsort_s,bsort_d,bsort_si,bsort_di;           end interface
interface mergeab
   module procedure mergeab_s,mergeab_d,mergeab_si,mergeab_di;   end interface
interface invertperm; module procedure invertperm;               end interface
contains

!=============================================================================
subroutine sort_s(v)!                                                   [sort]
!=============================================================================
! Wrapper routine to take care of binary sorting details involved with
! bsort and its linked lists.
!=============================================================================
implicit none
real(sp),dimension(:),intent(inout):: v
!----------------------------------------------------------------------------
integer(i_kind)                          :: i,j,n
real(sp),        allocatable,dimension(:):: vv
integer(i_kind) ,allocatable,dimension(:):: next
!=============================================================================
n=size(v)
allocate(vv(n),next(n))
call bsort(1,n,v,next,i)
vv=v
do j=1,n
   v(j)=vv(i)
   i=next(i)
enddo
if(i/=0)stop 'In sort; apparent anomaly in behavior of sorting routines'
deallocate(vv,next)
end subroutine sort_s

!=============================================================================
subroutine sort_d(v)!                                                   [sort]
!=============================================================================
! Wrapper routine to take care of binary sorting details involved with
! bsort and its linked lists.
!=============================================================================
implicit none
real(dp),dimension(:),intent(inout):: v
!----------------------------------------------------------------------------
integer(i_kind)                          :: i,j,n
real(dp),        allocatable,dimension(:):: vv
integer(i_kind) ,allocatable,dimension(:):: next
!=============================================================================
n=size(v)
allocate(vv(n),next(n))
call bsort(1,n,v,next,i)
vv=v
do j=1,n
   v(j)=vv(i)
   i=next(i)
enddo
if(i/=0)stop 'In sort; apparent anomaly in behavior of sorting routines'
deallocate(vv,next)
end subroutine sort_d
!=============================================================================
subroutine sort_si(v)!                                                   [sort]
!=============================================================================
! Wrapper routine to take care of binary sorting details involved with
! bsort and its linked lists.
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(inout):: v
!----------------------------------------------------------------------------
integer(i_kind)                           :: i,j,n
integer(i_kind), allocatable,dimension(:) :: vv
integer(i_kind) ,allocatable,dimension(:) :: next
!=============================================================================
n=size(v)
allocate(vv(n),next(n))
call bsort(1,n,v,next,i)
vv=v
do j=1,n
   v(j)=vv(i)
   i=next(i)
enddo
if(i/=0)stop 'In sort; apparent anomaly in behavior of sorting routines'
deallocate(vv,next)
end subroutine sort_si
!=============================================================================
subroutine sort_di(v)!                                                   [sort]
!=============================================================================
! Wrapper routine to take care of binary sorting details involved with
! bsort and its linked lists.
!=============================================================================
use kinds, only: dpi
implicit none
integer(dpi),dimension(:),intent(inout):: v
!----------------------------------------------------------------------------
integer(i_kind)                           :: i,j,n
integer(dpi),   allocatable,  dimension(:):: vv
integer(i_kind),allocatable,  dimension(:):: next
!=============================================================================
n=size(v)
allocate(vv(n),next(n))
call bsort(1,n,v,next,i)
vv=v
do j=1,n
   v(j)=vv(i)
   i=next(i)
enddo
if(i/=0)stop 'In sort; apparent anomaly in behavior of sorting routines'
deallocate(vv,next)
end subroutine sort_di

!=============================================================================
subroutine rsort_s(v,rank)!                                             [sort]
!=============================================================================
! Wrapper routine to take care of binary sorting details involved with
! bsort and its linked lists.
! The rsort routine do NOT explicitly sort the data v; instead they return the
! rank, in increasing order, of each item contained in the v array.
! If the user wants instead to have the indices by which the successively
! larger items of v can be located, then invoking invertperm(rank) will do it.
!=============================================================================
use kinds, only: sp
implicit none
real(sp ),           dimension(:),intent(in   ):: v
integer(i_kind),     dimension(:),intent(  out):: rank
!----------------------------------------------------------------------------
integer(i_kind)                              :: i,j,n
integer(i_kind),     allocatable,dimension(:):: next
!=============================================================================
n=size(v)
allocate(next(n))
call bsort(1,n,v,next,i)
do j=1,n
   rank(i)=j
   i=next(i)
enddo
if(i/=0)stop 'In sort; apparent anomaly in behavior of sorting routines'
deallocate(next)
end subroutine rsort_s
!=============================================================================
subroutine rsort_d(v,rank)!                                             [sort]
!=============================================================================
! Wrapper routine to take care of binary sorting details involved with
! bsort and its linked lists.
!=============================================================================
use kinds, only: dp
implicit none
real(dp ),           dimension(:),intent(in   ):: v
integer(i_kind),     dimension(:),intent(  out):: rank
!----------------------------------------------------------------------------
integer(i_kind)                              :: i,j,n
integer(i_kind),     allocatable,dimension(:):: next
!=============================================================================
n=size(v)
allocate(next(n))
call bsort(1,n,v,next,i)
do j=1,n
   rank(i)=j
   i=next(i)
enddo
if(i/=0)stop 'In sort; apparent anomaly in behavior of sorting routines'
deallocate(next)
end subroutine rsort_d
!=============================================================================
subroutine rsort_si(v,rank)!                                            [sort]
!=============================================================================
! Wrapper routine to take care of binary sorting details involved with
! bsort and its linked lists.
!=============================================================================
implicit none
integer(i_kind),     dimension(:),intent(in   ):: v
integer(i_kind),     dimension(:),intent(  out):: rank
!----------------------------------------------------------------------------
integer(i_kind)                              :: i,j,n
integer(i_kind),     allocatable,dimension(:):: next
!=============================================================================
n=size(v)
allocate(next(n))
call bsort(1,n,v,next,i)
do j=1,n
   rank(i)=j
   i=next(i)
enddo
if(i/=0)stop 'In sort; apparent anomaly in behavior of sorting routines'
deallocate(next)
end subroutine rsort_si
!=============================================================================
subroutine rsort_di(v,rank)!                                            [sort]
!=============================================================================
! Wrapper routine to take care of binary sorting details involved with
! bsort and its linked lists.
!=============================================================================
use kinds, only: dpi
implicit none
integer(dpi),   dimension(:),intent(in   ):: v
integer(i_kind),dimension(:),intent(  out):: rank
!----------------------------------------------------------------------------
integer(i_kind)                           :: i,j,n
integer(i_kind),  allocatable,dimension(:):: next
!=============================================================================
n=size(v)
allocate(next(n))
call bsort(1,n,v,next,i)
do j=1,n
   rank(i)=j
   i=next(i)
enddo
if(i/=0)stop 'In sort; apparent anomaly in behavior of sorting routines'
deallocate(next)
end subroutine rsort_di

!=============================================================================
recursive subroutine bsort_s(n1,n2,v,next,first)!                      [bsort]
!=============================================================================
! Recursively sort strings by recursive binary "divide and conquer" until
! the divided strings are of length L or less, where L is empirically chosen
! here in the parameter statement (efficiency is found not to be strongly
! sensitive to the L). The values, v, of items in the string are real.
! An integer representation of all the linked-list pointers is used.
!=============================================================================
implicit none
integer(i_kind),                  intent(IN   ):: n1,n2
real(sp),        dimension(n1:n2),intent(IN   ):: v
integer(i_kind), dimension(n1:n2),intent(  OUT):: next
integer(i_kind),                  intent(  OUT):: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter:: L=6
integer(i_kind)          :: n,na1,na2,nb1,nb2,i,j,k, maxk,maxl,left, firsta,firstb
real(sp)                 :: maxv
!=============================================================================
n=n2+1-n1
if(n<=L)then
! Sort the small number of items by an order (n*n) algorithm:
   do i=n1,n2-1
      next(i)=i+1
   enddo
   next(n2)=0
   first=n1
   do i=n-1,1,-1
      k=first
      left=0
      maxv=v(k)
      maxl=left
      maxk=k
      do j=1,i
         left=k
         k=next(k)
         if(v(k)>=maxv)then
            maxv=v(k)
            maxl=left
            maxk=k
         endif
      enddo
      if(k/=maxk)then
         if(maxl==0)then
            first=next(maxk)
         else
            next(maxl)=next(maxk)
         endif
         next(maxk)=next(k)
         next(k)=maxk
      endif
   enddo
else
   na1=n1; na2=(n1+n2)/2; nb1=na2+1; nb2=n2
   call bsort(na1,na2,v(na1:na2),next(na1:na2),firsta)
   call bsort(nb1,nb2,v(nb1:nb2),next(nb1:nb2),firstb)
   call mergeab(na1,nb2,firsta,firstb, &
                v(na1:nb2),next(na1:nb2),     first)
endif
end subroutine bsort_s

!=============================================================================
recursive subroutine bsort_d(n1,n2,v,next,first)!                      [bsort]
!=============================================================================
implicit none
integer(i_kind),                  intent(IN   ):: n1,n2
real(dp),        dimension(n1:n2),intent(IN   ):: v
integer(i_kind), dimension(n1:n2),intent(  OUT):: next
integer(i_kind),                  intent(  OUT):: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter:: L=6
integer(i_kind)          :: n,na1,na2,nb1,nb2,i,j,k, maxk,maxl,left, firsta,firstb
real(dp)                 :: maxv
!=============================================================================
n=n2+1-n1
if(n<=L)then
! Sort the small number of items by an order (n*n) algorithm:
   do i=n1,n2-1
      next(i)=i+1
   enddo
   next(n2)=0
   first=n1
   do i=n-1,1,-1
      k=first
      left=0
      maxv=v(k)
      maxl=left
      maxk=k
      do j=1,i
         left=k
         k=next(k)
         if(v(k)>=maxv)then
            maxv=v(k)
            maxl=left
            maxk=k
         endif
      enddo
      if(k/=maxk)then
         if(maxl==0)then
            first=next(maxk)
         else
            next(maxl)=next(maxk)
         endif
         next(maxk)=next(k)
         next(k)=maxk
      endif
   enddo
else
   na1=n1; na2=(n1+n2)/2; nb1=na2+1; nb2=n2
   call bsort(na1,na2,v(na1:na2),next(na1:na2),firsta)
   call bsort(nb1,nb2,v(nb1:nb2),next(nb1:nb2),firstb)
   call mergeab(na1,nb2,firsta,firstb, &
                v(na1:nb2),next(na1:nb2),     first)
endif
end subroutine bsort_d
!=============================================================================
recursive subroutine bsort_si(n1,n2,v,next,first)!                     [bsort]
!=============================================================================
implicit none
integer(i_kind),                  intent(IN   ):: n1,n2
integer(i_kind),dimension(n1:n2), intent(IN   ):: v
integer(i_kind), dimension(n1:n2),intent(  OUT):: next
integer(i_kind),                  intent(  OUT):: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter:: L=6
integer(i_kind)          :: n,na1,na2,nb1,nb2,i,j,k, maxk,maxl,left, firsta,firstb
integer(i_kind)          :: maxv
!=============================================================================
n=n2+1-n1
if(n<=L)then
! Sort the small number of items by an order (n*n) algorithm:
   do i=n1,n2-1
      next(i)=i+1
   enddo
   next(n2)=0
   first=n1
   do i=n-1,1,-1
      k=first
      left=0
      maxv=v(k)
      maxl=left
      maxk=k
      do j=1,i
         left=k
         k=next(k)
         if(v(k)>=maxv)then
            maxv=v(k)
            maxl=left
            maxk=k
         endif
      enddo
      if(k/=maxk)then
         if(maxl==0)then
            first=next(maxk)
         else
            next(maxl)=next(maxk)
         endif
         next(maxk)=next(k)
         next(k)=maxk
      endif
   enddo
else
   na1=n1; na2=(n1+n2)/2; nb1=na2+1; nb2=n2
   call bsort(na1,na2,v(na1:na2),next(na1:na2),firsta)
   call bsort(nb1,nb2,v(nb1:nb2),next(nb1:nb2),firstb)
   call mergeab(na1,nb2,firsta,firstb, &
                v(na1:nb2),next(na1:nb2),     first)
endif
end subroutine bsort_si
!=============================================================================
recursive subroutine bsort_di(n1,n2,v,next,first)!                     [bsort]
!=============================================================================
use kinds, only: dpi
implicit none
integer(i_kind),                 intent(IN   ):: n1,n2
integer(dpi),   dimension(n1:n2),intent(IN   ):: v
integer(i_kind),dimension(n1:n2),intent(  OUT):: next
integer(i_kind),                 intent(  OUT):: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter:: L=6
integer(i_kind)          :: n,na1,na2,nb1,nb2,i,j,k,maxk,maxl,left,firsta,firstb
integer(dpi)             :: maxv
!=============================================================================
n=n2+1-n1
if(n<=L)then
! Sort the small number of items by an order (n*n) algorithm:
   do i=n1,n2-1
      next(i)=i+1
   enddo
   next(n2)=0
   first=n1
   do i=n-1,1,-1
      k=first
      left=0
      maxv=v(k)
      maxl=left
      maxk=k
      do j=1,i
         left=k
         k=next(k)
         if(v(k)>=maxv)then
            maxv=v(k)
            maxl=left
            maxk=k
         endif
      enddo
      if(k/=maxk)then
         if(maxl==0)then
            first=next(maxk)
         else
            next(maxl)=next(maxk)
         endif
         next(maxk)=next(k)
         next(k)=maxk
      endif
   enddo
else
   na1=n1; na2=(n1+n2)/2; nb1=na2+1; nb2=n2
   call bsort(na1,na2,v(na1:na2),next(na1:na2),firsta)
   call bsort(nb1,nb2,v(nb1:nb2),next(nb1:nb2),firstb)
   call mergeab(na1,nb2,firsta,firstb, &
                v(na1:nb2),next(na1:nb2),     first)
endif
end subroutine bsort_di

!=============================================================================
subroutine mergeab_s(na1,nb2,firsta,firstb, v,next,first)!           [mergeab]
!=============================================================================
! Merge a pair (a and b) of individually pre-sorted strings of real values,
! connected as respective linked-lists, into a unified string with ALL the
! items returned in ascending order of values v.
!=============================================================================
implicit none
integer(i_kind),                    intent(IN   ):: na1,nb2,firsta,firstb
real(sp),dimension(na1:nb2),        intent(IN   ):: v
integer(i_kind), dimension(na1:nb2),intent(INOUT):: next
integer(i_kind),                    intent(  OUT):: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter                        :: hugeint= 10000000
integer(i_kind)                                  :: idum,ia,ib,ic
!============================================================================
ia=firsta
ib=firstb
if(ia==0)then
   first=firstb
   return
endif
if(ib==0)then
   first=firsta
   return
endif
if(v(ia)<v(ib))then
   first=ia
   ic=ia
   ia=next(ia)
else
   first=ib
   ic=ib
   ib=next(ib)
endif

do idum=1,hugeint
   if(ia==0)then
      next(ic)=ib
      return
   endif
   if(ib==0)then
      next(ic)=ia
      return
   endif
   if(v(ia)<v(ib))then
      next(ic)=ia
      ic=ia
      ia=next(ia)
   else
      next(ic)=ib
      ic=ib
      ib=next(ib)
   endif
enddo
stop 'in mergeab; hugeint too small'
end subroutine mergeab_s

!=============================================================================
subroutine mergeab_d(na1,nb2,firsta,firstb,v,next,first)!            [mergeab]
!=============================================================================
implicit none
integer(i_kind),                    intent(IN   ):: na1,nb2,firsta,firstb
real(dp),        dimension(na1:nb2),intent(IN   ):: v
integer(i_kind), dimension(na1:nb2),intent(INOUT):: next
integer(i_kind),                    intent(  OUT):: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter                        :: hugeint= 10000000
integer(i_kind)                                  :: idum,ia,ib,ic
!=============================================================================
ia=firsta
ib=firstb
if(ia==0)then
   first=firstb
   return
endif
if(ib==0)then
   first=firsta
   return
endif
if(v(ia)<v(ib))then
   first=ia
   ic=ia
   ia=next(ia)
else
   first=ib
   ic=ib
   ib=next(ib)
endif

do idum=1,hugeint
   if(ia==0)then
      next(ic)=ib
      return
   endif
   if(ib==0)then
      next(ic)=ia
      return
   endif
   if(v(ia)<v(ib))then
      next(ic)=ia
      ic=ia
      ia=next(ia)
   else
      next(ic)=ib
      ic=ib
      ib=next(ib)
   endif
enddo
stop 'in mergeab; hugeint too small'
end subroutine mergeab_d

!=============================================================================
subroutine mergeab_si(na1,nb2,firsta,firstb, v,next,first)!          [mergeab]
!=============================================================================
! Merge a pair (a and b) of individually pre-sorted strings of integer(i_kind) values,
! connected as respective linked-lists, into a unified string with ALL the
! items returned in ascending order of values v.
!=============================================================================
implicit none
integer(i_kind),                    intent(IN   ):: na1,nb2,firsta,firstb
integer(i_kind),dimension(na1:nb2), intent(IN   ):: v
integer(i_kind), dimension(na1:nb2),intent(INOUT):: next
integer(i_kind),                    intent(  OUT):: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter                        :: hugeint= 10000000
integer(i_kind)                                  :: idum,ia,ib,ic
!=============================================================================
ia=firsta
ib=firstb
if(ia==0)then
   first=firstb
   return
endif
if(ib==0)then
   first=firsta
   return
endif
if(v(ia)<v(ib))then
   first=ia
   ic=ia
   ia=next(ia)
else
   first=ib
   ic=ib
   ib=next(ib)
endif

do idum=1,hugeint
   if(ia==0)then
      next(ic)=ib
      return
   endif
   if(ib==0)then
      next(ic)=ia
      return
   endif
   if(v(ia)<v(ib))then
      next(ic)=ia
      ic=ia
      ia=next(ia)
   else
      next(ic)=ib
      ic=ib
      ib=next(ib)
   endif
enddo
stop 'in mergeab; hugeint too small'
end subroutine mergeab_si
!=============================================================================
subroutine mergeab_di(na1,nb2,firsta,firstb, v,next,first)!          [mergeab]
!=============================================================================
! Merge a pair (a and b) of individually pre-sorted strings of integer(i_kind) values,
! connected as respective linked-lists, into a unified string with ALL the
! items returned in ascending order of values v.
!=============================================================================
use kinds, only: dpi
implicit none
integer(i_kind),                         intent(IN   ):: na1,nb2,firsta,firstb
integer(dpi),    dimension(na1:nb2),     intent(IN   ):: v
integer(i_kind), dimension(na1:nb2),     intent(INOUT):: next
integer(i_kind),                         intent(  OUT):: first
!-----------------------------------------------------------------------------
integer(i_kind),parameter                        :: hugeint= 10000000
integer(i_kind)                                  :: idum,ia,ib,ic
!=============================================================================
ia=firsta
ib=firstb
if(ia==0)then
   first=firstb
   return
endif
if(ib==0)then
   first=firsta
   return
endif
if(v(ia)<v(ib))then
   first=ia
   ic=ia
   ia=next(ia)
else
   first=ib
   ic=ib
   ib=next(ib)
endif

do idum=1,hugeint
   if(ia==0)then
      next(ic)=ib
      return
   endif
   if(ib==0)then
      next(ic)=ia
      return
   endif
   if(v(ia)<v(ib))then
      next(ic)=ia
      ic=ia
      ia=next(ia)
   else
      next(ic)=ib
      ic=ib
      ib=next(ib)
   endif
enddo
stop 'in mergeab; hugeint too small'
end subroutine mergeab_di

!=============================================================================
subroutine invertperm(perm)!                                      [invertperm]
!=============================================================================
implicit none
integer(i_kind),dimension(:),intent(inout):: perm
!-----------------------------------------------------------------------------
integer(i_kind),allocatable,dimension(:)  :: perma
integer(i_kind)                           :: i,j,n
!=============================================================================
n=size(perm)
allocate(perma(n))
do i=1,n
   j=perm(i)
   perma(j)=i
enddo
perm=perma
deallocate(perma)
end subroutine invertperm



end module psort
