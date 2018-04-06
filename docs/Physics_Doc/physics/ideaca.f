!***********************************************************************
!***********************************************************************
! 07/28/08 File ideaca.f created by Rashid Akmaev for a dry convective
!     adjustment (CA) scheme for IDEA based on early codes written
!     after Akmaev (MWR, 1991).
!     Temperature is assumed to be specified in fixed pressure layers
!     going up (decreasing pressure), but only minor changes are needed
!     for layers going down, for temperature specified at levels, for
!     vertical coordinate spacing depending on geographic location, or
!     for variable critical lapse rate.
!
! Apr 06 2012   Henry Juang, initial implement for nems
! Dec    2012   Jun Wang, move init out of column physics
! Jan    2013   Jun Wang, fix the neutral layer index k when mdoel top 
!                         layer has instability and affects adjacent
!                         layers underneath
! Dec 2015 Rashid Akmaev:
!     1. Fixed the indexing bug found by Jun in 2013
!     2. Reset the offset pressure from .1 (apparently cb?) to 100. (Pa)
!     3. Reset the critical lapse rate to 9.5 K/km typical at ~100 km
!
! Contains
!      module ideaca_mod
!      subroutine ideaca_init(p,nl)
!      subroutine ideaca_up(p,t,ix,im,nlev)
!     
!***********************************************************************

      module ideaca_mod

! Module to keep data for dry CA routines
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! 07/28/08
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Parameters
! - critical lapse rate gamma=g/Cp (K/km)
! - R/g
! - R/Cp

      real,parameter:: gamma=9.5,rdg=.287/9.5,rdcp=rdg*gamma

! - starting pressure level above which dry CA is applied, set
!     currently to the stratopause level (***should be in Pascals if
!     ideaca_init is called from gloopb***)

      real,parameter:: p0=100.
! This was for testing (07/30/08, 12/02/15)
!      real,parameter:: p0=100001.

! Variables
! - model index offset for temperature (i.e., pressure layer number
!     above which CA is applied) and work array dimension
! - CA procedure weigths 

      integer loff,nlay
      real,dimension(:),allocatable:: r,q
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      interface
         subroutine ideaca_init(p,nl)
         integer,intent(in):: nl
         real,dimension(nl),intent(in):: p
         end subroutine ideaca_init
      end interface
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      end module ideaca_mod

!***********************************************************************

      subroutine ideaca_init(p,nl)

! Initialize dry convective adjusment for IDEA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use ideaca_mod, except => ideaca_init
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! INPUT
! Total number of interface pressure levels

      integer,intent(in):: nl

! Interface pressure levels

      real,dimension(nl),intent(in):: p
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Internal variables

      integer:: l
      real,dimension(:),allocatable:: pm,dp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Find index offset, assuming pressure index goes up (in decreasing
!     pressure), calculate the number of layers to adjust

      do l=1,nl
         if(p(l) <= p0) then
            loff=l-1
            exit
         endif
      enddo
      nlay=(nl-1)-loff

! Allocate permanent and temporary arrays

      allocate(r(nlay),q(nlay))
      allocate(pm(nlay),dp(nlay))

      do l=1,nlay
         pm(l)=.5*(p(loff+l)+p(loff+l+1))
         dp(l)=p(loff+l)-p(loff+l+1)
      enddo

! Calculate weight arrays (a more general expression is used, which
!     makes no difference and may be simplified in case of constant
!     gamma)

      r(1)=1.
      do l=2,nlay
         r(l)=r(l-1)*(p(loff+l)/pm(l))**rdcp*                           &
     &        (pm(l-1)/p(loff+l))**rdcp
      enddo
      q(:)=dp(:)/r(:)
!
      deallocate(pm,dp)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      end subroutine ideaca_init

!***********************************************************************

      subroutine ideaca_up(p,t,ix,im,nlev)

! Dry convective adjusment of mid-layer temperatures going up for IDEA
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      use ideaca_mod
      implicit none
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Subroutine arguments
! - input array dimensions, number of interface levels
! - interface pressures
!***  RA: Note pressure is no longer used, it's a leftover for
!***  compatibility with previous version where ideaca_init was
!***  called from inside this subroutine
! - layer temperatures

      integer,intent(in):: ix,im,nlev
      real,intent(in):: p(ix,nlev)
      real,intent(inout):: t(ix,nlev-1)
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Internal variables

      integer:: i,j,k,l,n
      integer,dimension(nlev):: nml
      real,dimension(nlev):: teta,tpp,pdp
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
! Outer (horizontal) loop

      do n=1,im

! Initialize first combined layer with first model layer

         i=1
         k=i
         nml(k)=1
         teta(k)=t(n,loff+1)*r(1)
         pdp(k)=q(1)
         tpp(k)=teta(k)*q(1)

! Scan model layers (e.g., going up from the first layer above offset)

         do l=2,nlay

! Initialize next layer with current model layer

            nml(k+1)=1
            teta(k+1)=t(n,loff+l)*r(l)
            pdp(k+1)=q(l)
            tpp(k+1)=teta(k+1)*q(l)

! Recursively check stability with immediately underlying (combined)
!     layer, until a stable stratification is found or the bottom layer
!     is reached

            do j=k,1,-1

! For model layers going down this inequality should be reversed

               if(teta(j) <= teta(j+1)) then
                  
! Stable stratification - do not combine layers, advance index of
!     combined layers (the number of combined layers created to this
!     point), go to next model layer

                  i=j+1
                  exit
               else

! Unstable - combine the two layers just compared, j+1 and j, into one
!     layer j, remember its index (the number of combined layers
!     created to this point)

                  pdp(j)=pdp(j+1)+pdp(j)
                  tpp(j)=tpp(j+1)+tpp(j)
                  nml(j)=nml(j+1)+nml(j)
                  teta(j)=tpp(j)/pdp(j)
                  i=j
               endif
            enddo
            k=i
         enddo

! Retrieve temperature from potential temperature of (combined) layers,
!     set starting model layer index

         l=1
         i=l
         do j=1,k

! Scan all model layer within each neutral layer, reset starting index

            do l=i,i+nml(j)-1
               t(n,loff+l)=teta(j)/r(l)
            enddo
            i=l
         enddo
      enddo
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      end subroutine ideaca_up

!***********************************************************************
!***********************************************************************
