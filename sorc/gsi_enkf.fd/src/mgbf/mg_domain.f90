submodule(mg_parameter) mg_domain
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_domain
!   prgmmr: rancic           org: NCEP/EMC            date: 2020
!
! abstract:  Definition of a squared integration domain
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   init_mg_domain -
!   init_domain -
!   init_topology_2d -
!   real_itarg -
!
! Functions Included:
!
! remarks:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block

use mpi
use kinds, only: i_kind

implicit none

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
contains
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine init_mg_domain(this)
!***********************************************************************
!                                                                      *
!             Initialize square domain                                 *
!                                                                      *
!***********************************************************************
implicit none
class(mg_parameter_type)::this

call init_domain(this)
call init_topology_2d(this)
 
!-----------------------------------------------------------------------
endsubroutine init_mg_domain

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine init_domain(this)
!***********************************************************************
!                                                                      *
!   Definition of constants that control filtering domain              *
!                                                                      *
!***********************************************************************
implicit none
class(mg_parameter_type),target::this

integer(i_kind) n,nstrd,i,j
logical:: F=.false., T=.true.

integer(i_kind):: loc_pe,g
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!-----------------------------------------------------------------------

      Flwest(1)=nx.eq.1
      Fleast(1)=nx.eq.nxm
      Flsouth(1)=my.eq.1
      Flnorth(1)=my.eq.nym

 if(l_hgen) then 

      loc_pe=mype_hgen-maxpe_fgen(my_hgen-1)
      jy=loc_pe/ixm(my_hgen)+1
      ix=mod(loc_pe,ixm(my_hgen))+1

      Flwest(2)=ix.eq.1
      Fleast(2)=ix.eq.ixm(my_hgen)
      Flsouth(2)=jy.eq.1
      Flnorth(2)=jy.eq.jym(my_hgen)

 else

     jy = -1
     ix = -1

     Flwest(2)=F
     Fleast(2)=F
     Flsouth(2)=F
     Flnorth(2)=F

 endif

    mype_filt(1)=mype
    mype_filt(2)=mype_hgen 

!
! Communication params for analysis grid
!
    if(nx==1) then
      itarg_wA=-1
    else
      itarg_wA=mype-1
    endif

    if(nx==nxm) then
      itarg_eA=-1
    else
      itarg_eA=mype+1
    endif

    if(my==1) then
      itarg_sA=-1
    else
      itarg_sA=mype-nxm
    endif

    if(my==nym) then
      itarg_nA=-1
    else
      itarg_nA=mype+nxm
    endif

      lwestA=nx.eq.1
      leastA=nx.eq.nxm
      lsouthA=my.eq.1
      lnorthA=my.eq.nym

   
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!       write(100+mype,'(a)')'---------------------------------'
!       write(100+mype,'(a)')'From init_domain'
!       write(100+mype,'(a,2i5)')'mype=',mype
!       write(100+mype,'(a,i5)')'nx=',nx
!       write(100+mype,'(a,i5)')'my=',my
!       write(100+mype,'(a)')'---------------------------------'
!       write(100+mype_filt,'(a)')'---------------------------------'
!       write(100+mype_filt,'(a,3i5)')'mype,mype_filt,mygen :',mype,mype_filt,mygen
!       write(100+mype_filt,'(a,2i5)')'ix,jy=           ',ix,jy
!       write(100+mype_filt,'(a,l5)')'lwest =           ',lwest
!       write(100+mype_filt,'(a,l5)')'least =           ',least
!       write(100+mype_filt,'(a,l5)')'lsouth=           ',lsouth
!       write(100+mype_filt,'(a,l5)')'lnorth=           ',lnorth
!       write(100+mype_filt,'(a,l5)')'lcorner_sw        ',lcorner_sw
!       write(100+mype_filt,'(a,l5)')'lcorner_se        ',lcorner_se
!       write(100+mype_filt,'(a,l5)')'lcorner_nw        ',lcorner_nw
!       write(100+mype_filt,'(a,l5)')'lcorner_ne        ',lcorner_ne
!       write(100+mype_filt,'(a)')'----------------------------------'
!       write(100+mype_filt,'(a)')' '
!-----------------------------------------------------------------------
!     if(mype==0) then
!        write(27,'(a,i4)') 'nb=',nb
!        write(27,'(a,i4)') 'mb=',mb
!     endif
!
!        call finishMPI
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!-----------------------------------------------------------------------
endsubroutine init_domain

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine init_topology_2d(this)
!***********************************************************************
!                                                                      *
!                  Define topology of filter grid                      *
!                       - Four generations -                           *
!                                                                      *
!***********************************************************************
implicit none
class(mg_parameter_type),target::this
!-----------------------------------------------------------------------
logical:: F=.false., T=.true.

integer(i_kind) mx2,my2,ix_up,jy_up,ix_dn,jy_dn
integer(i_kind) g,naux,nx_up,my_up
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!-----------------------------------------------------------------------
!
!     Topology of generations of the squared domain
!
!                           G1 
!    _____ _____ _____ _____ _____ _____ _____ _____
!   |     |     |     |     |     |     |     |     |
!   | 56  | 57  | 58  | 59  | 60  | 61  | 62  | 63  |
!   |_____|_____|_____|_____|_____|_____|_____|_____|
!   |     |     |     |     |     |     |     |     |
!   | 48  | 49  | 50  | 51  | 52  | 53  | 54  | 55  |
!   |_____|_____|_____|_____|_____|_____|_____|_____|
!   |     |     |     |     |     |     |     |     |
!   | 40  | 41  | 42  | 43  | 44  | 45  | 46  | 47  |
!   |_____|_____|_____|_____|_____|_____|_____|_____|
!   |     |     |     |     |     |     |     |     |
!   | 32  | 33  | 34  | 35  | 36  | 37  | 38  | 39  |
!   |_____|_____|_____|_____|_____|_____|_____|_____|
!   |     |     |     |     |     |     |     |     |
!   | 24  | 25  | 26  | 27  | 28  | 29  | 30  | 31  |
!   |_____|_____|_____|_____|_____|_____|_____|_____|
!   |     |     |     |     |     |     |     |     |
!   | 16  | 17  | 18  | 19  | 20  | 21  | 22  | 23  |
!   |_____|_____|_____|_____|_____|_____|_____|_____|
!   |     |     |     |     |     |     |     |     |
!   |  8  |  9  | 10  | 11  | 12  | 13  | 14  | 15  |
!   |_____|_____|_____|_____|_____|_____|_____|_____|
!   |     |     |     |     |     |     |     |     |
!   |  0  |  1  |  2  |  3  |  4  |  5  |  6  |  7  |
!   |_____|_____|_____|_____|_____|_____|_____|_____|
!
!
!                           G2 
!    ___________ ___________ ___________ ___________ 
!   |           |           |           |           |
!   |           |           |           |           |
!   |    76     |    77     |    78     |    79     |
!   |           |           |           |           |
!   |           |           |           |           |
!   |___________|___________|___________|___________|
!   |           |           |           |           |
!   |           |           |           |           |
!   |    72     |    73     |    74     |    75     |
!   |           |           |           |           |
!   |           |           |           |           |
!   |___________|___________|___________|___________|
!   |           |           |           |           |
!   |           |           |           |           |
!   |    68     |    69     |    70     |    71     |
!   |           |           |           |           |
!   |           |           |           |           |
!   |___________|___________|___________|___________|
!   |           |           |           |           |
!   |           |           |           |           |
!   |    64     |    65     |    66     |    67     |
!   |           |           |           |           |
!   |           |           |           |           |
!   |___________|___________|___________|___________|
!
!
!                           G3 
!    _______________________ _______________________ 
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |          82           |          83           |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |_______________________|_______________________|
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |          80           |          81           |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |                       |                       |
!   |_______________________|_______________________|
!
!
!                           G4 
!    _______________________________________________ 
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                      84                       |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |                                               |
!   |_______________________________________________|
!
!----------------------------------------------------------------------

       do g = 1,2
!***
!*** Send WEST
!***
       if(Flwest(g)) then
         Fitarg_w(g) = -1
       else 
         if(g==1.or.l_hgen) then
           Fitarg_w(g) = mype_filt(g)-1
         else
           Fitarg_w(g) = -1
         endif
       endif
!***
!*** Send EAST
!***
       if(Fleast(g)) then
         Fitarg_e(g) = -1
       else
         if(g==1.or.l_hgen) then
           Fitarg_e(g) = mype_filt(g)+1
         else
           Fitarg_e(g) = -1
         endif
       endif

!***
!*** Send SOUTH
!***

       if(Flsouth(g)) then
         Fitarg_s(g)=-1
       else
         select case(g)
           case(1)
             naux = nxm
           case(2)
             if(l_hgen) then
               naux = ixm(my_hgen)
             endif
         endselect
         if(g==1.or.l_hgen) then
           Fitarg_s(g)=mype_filt(g)-naux
         else  
           Fitarg_s(g)=-1
         endif
       endif

!***
!*** Send NORTH
!***
       if(Flnorth(g)) then
         Fitarg_n(g)=-1
       else
         select case(g)
           case(1)
             naux = nxm
           case(2)
             if(l_hgen) then
               naux = ixm(my_hgen)
             endif
         endselect
         if(g==1.or.l_hgen) then
           Fitarg_n(g)=mype_filt(g)+naux
         else
           Fitarg_n(g)=-1
         endif
       endif

!***
!*** Send SOUTH-WEST
!***

       if(Flsouth(g).and.Flwest(g)) then
         Fitarg_sw(g)=-1
       else &
       if(Flsouth(g)) then
         Fitarg_sw(g)=Fitarg_w(g)
       else &
       if(Flwest(g)) then
         Fitarg_sw(g)=Fitarg_s(g)
       else
         Fitarg_sw(g)=Fitarg_s(g)-1
       endif
         if(g>1 .and. .not.l_hgen) then
           Fitarg_sw(g)=-1
         endif

!***
!*** Send SOUTH-EAST
!***

       if(Flsouth(g).and.Fleast(g)) then
          Fitarg_se(g)=-1
       else &
       if(Flsouth(g)) then
          Fitarg_se(g)=Fitarg_e(g)
       else &
       if(Fleast(g)) then
          Fitarg_se(g)=Fitarg_s(g)
       else
          Fitarg_se(g)=Fitarg_s(g)+1
       endif 
         if(g>1 .and. .not.l_hgen) then
           Fitarg_se(g)=-1
         endif

!***
!*** Send NORTH-WEST
!***
       if(Flnorth(g).and.Flwest(g)) then
         Fitarg_nw(g)=-1
       else &
       if(Flnorth(g)) then
         Fitarg_nw(g)=Fitarg_w(g)
       else &
       if(Flwest(g)) then
         Fitarg_nw(g)=Fitarg_n(g)
       else
         Fitarg_nw(g)=Fitarg_n(g)-1
       endif
         if(g>1 .and. .not.l_hgen) then
           Fitarg_nw(g)=-1
         endif


!***
!*** Send NORTH-EAST
!***

       if(Flnorth(g).and.Fleast(g)) then
         Fitarg_ne(g)=-1
       else &
       if(Flnorth(g)) then
         Fitarg_ne(g)=Fitarg_e(g)
       else &
       if(Fleast(g)) then
         Fitarg_ne(g)=Fitarg_n(g)
       else
         Fitarg_ne(g)=Fitarg_n(g)+1
       endif
         if(g>1 .and. .not.l_hgen) then
           Fitarg_ne(g)=-1
         endif


       enddo 

!-----------------------------------------------------------------------
!
! Upsending flags
!

    mx2=mod(nx,2)
    my2=mod(my,2)

      if(mx2==1.and.my2==1) then
        Flsendup_sw(1)=T
      else &
      if(mx2==0.and.my2==1) then
        Flsendup_se(1)=T
      else &
      if(mx2==1.and.my2==0) then
        Flsendup_nw(1)=T
      else 
        Flsendup_ne(1)=T
      end if
       
       nx_up=(nx-1)/2   !+1
       my_up=(my-1)/2   !+1


       Fitarg_up(1)=maxpe_fgen(1)+my_up*ixm(2)+nx_up


    if(l_hgen.and.my_hgen < gm) then

    mx2=mod(ix,2)
    my2=mod(jy,2)

      if(mx2==1.and.my2==1) then
        Flsendup_sw(2)=T
      else &
      if(mx2==0.and.my2==1) then
        Flsendup_se(2)=T
      else &
      if(mx2==1.and.my2==0) then
        Flsendup_nw(2)=T
      else 
        Flsendup_ne(2)=T
      end if
       
       ix_up=(ix-1)/2   !+1
       jy_up=(jy-1)/2   !+1

       Fitarg_up(2)=maxpe_fgen(my_hgen)+jy_up*ixm(my_hgen+1)+ix_up

    else 

       Flsendup_sw(2)=F
       Flsendup_se(2)=F
       Flsendup_nw(2)=F
       Flsendup_ne(2)=F

       Fitarg_up(2)=-1

    endif

!
! Downsending flags 
!

     if(my_hgen > 1) then
       
       ix_dn = 2*ix-1
       jy_dn = 2*jy-1

       itargdn_sw=maxpe_fgen(my_hgen-2)+(jy_dn-1)*ixm(my_hgen-1)+(ix_dn-1)
       itargdn_nw=itargdn_sw+ixm(my_hgen-1)
       itargdn_se=itargdn_sw+1
       itargdn_ne=itargdn_nw+1
       
       if(Fimax(my_hgen) <= imL .and. Fleast(2)) then
          itargdn_se=-1
          itargdn_ne=-1
       endif
       if(Fjmax(my_hgen) <= jmL .and. Flnorth(2)) then
          itargdn_nw=-1
          itargdn_ne=-1
       end if

     else 

        itargdn_sw=-1
        itargdn_se=-1
        itargdn_nw=-1
        itargdn_ne=-1

     end if
!
! Convert targets in higher generations into real targets
!
   call real_itarg(this,Fitarg_w(2))
   call real_itarg(this,Fitarg_e(2))
   call real_itarg(this,Fitarg_s(2))
   call real_itarg(this,Fitarg_n(2))

   call real_itarg(this,Fitarg_sw(2))
   call real_itarg(this,Fitarg_se(2))
   call real_itarg(this,Fitarg_nw(2))
   call real_itarg(this,Fitarg_ne(2))

   if(itargdn_sw .ge. maxpe_fgen(1)) call real_itarg(this,itargdn_sw)
   if(itargdn_se .ge. maxpe_fgen(1)) call real_itarg(this,itargdn_se)
   if(itargdn_nw .ge. maxpe_fgen(1)) call real_itarg(this,itargdn_nw)
   if(itargdn_ne .ge. maxpe_fgen(1)) call real_itarg(this,itargdn_ne)

   call real_itarg(this,Fitarg_up(1))
   call real_itarg(this,Fitarg_up(2))

!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!       write(200+mype_filt,'(a)')'---------------------------------'
!       write(200+mype_filt,'(a)')'From init_topology_2d'
!       write(200+mype_filt,'(a,2i5)')'mype=',mype
!       write(200+mype_filt,'(a,i5)')'nx=',nx
!       write(200+mype_filt,'(a,i5)')'my=',my
!       write(200+mype_filt,'(a)')'---------------------------------'
!     if(l_hgen ) then
!       write(100+mype_filt,*)'     '
!       write(100+mype_filt,'(a,2i5)')'I AM (f),(a):',mype_filt,mype
!       write(100+mype_filt,'(a,i5)') 'mygen=       ',mygen
!
!      write(100+mype_filt,'(a,2i5)')'itarg_w=',itarg_w
!       write(100+mype_filt,'(a,2i5)')'itarg_e=',itarg_e
!       write(100+mype_filt,'(a,2i5)')'itarg_s=',itarg_s
!       write(100+mype_filt,'(a,2i5)')'itarg_n=',itarg_n
!
!       write(100+mype_filt,'(a,2i5)')'itarg_sw=',itarg_sw
!       write(100+mype_filt,'(a,2i5)')'itarg_se=',itarg_se
!       write(100+mype_filt,'(a,2i5)')'itarg_nw=',itarg_nw
!       write(100+mype_filt,'(a,2i5)')'itarg_ne=',itarg_ne
!       write(100+mype_filt,'(a)')'   '
!
!       if(lsendup_sw) write(100+mype_filt,'(a,l5)')'lsendup_sw=',lsendup_sw
!       if(lsendup_se) write(100+mype_filt,'(a,l5)')'lsendup_se=',lsendup_se
!       if(lsendup_nw) write(100+mype_filt,'(a,l5)')'lsendup_nw=',lsendup_nw
!       if(lsendup_ne) write(100+mype_filt,'(a,l5)')'lsendup_ne=',lsendup_ne
!
!       write(100+mype_filt,'(a,i5)')'itarg_up=',itarg_up
!
!       if(lsend_dn) write(100+mype_filt,'(a,l5)')'lsend_dn=',lsend_dn
!
!    if(my_hgen > 1) then
!       write(100+mype_hgen,'(a,2i5)')'mype_hgen,itargdn_sw=',mype_hgen,itargdn_sw
!       write(100+mype_hgen,'(a,2i5)')'mype_hgen,itargdn_se=',mype_hgen,itargdn_se
!       write(100+mype_hgen,'(a,2i5)')'mype_hgen,itargdn_nw=',mype_hgen,itargdn_nw
!       write(100+mype_hgen,'(a,2i5)')'mype_hgen,itargdn_ne=',mype_hgen,itargdn_ne
!       write(100+mype_hgen,'(a,2i5)')' '
!    if(Flsendup_sw(2)) then
!       write(mype+600,'(a,i4,l2,i4)')'mype_hgen,Flsendup_sw(2),Fitarg_up(2)= ' &
!                                     ,mype_hgen,Flsendup_sw(2),Fitarg_up(2)
!    endif
!    if(Flsendup_se(2)) then
!       write(mype+600,'(a,i4,l2,i4)')'mype_hgen,Flsendup_se(2),Fitarg_up(2)= ' &
!                                     ,mype_hgen,Flsendup_se(2),Fitarg_up(2)
!    endif
!    if(Flsendup_nw(2)) then
!       write(mype+600,'(a,i4,l2,i4)')'mype_hgen,Flsendup_nw(2),Fitarg_up(2)= ' &
!                                     ,mype_hgen,Flsendup_nw(2),Fitarg_up(2)
!    endif
!    if(Flsendup_ne(2)) then
!       write(mype+600,'(a,i4,l2,i4)')'mype_hgen,Flsendup_ne(2),Fitarg_up(2)= ' &
!                                     ,mype_hgen,Flsendup_ne(2),Fitarg_up(2)
!    endif
!    call finishMPI
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!-----------------------------------------------------------------------
endsubroutine init_topology_2d
!----------------------------------------------------------------------
!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine real_itarg &
!***********************************************************************
!                                                                      *
!             Definite real targets for high generations               *
!                                                                      *
!***********************************************************************
(this,itarg)
!-----------------------------------------------------------------------
implicit none
class(mg_parameter_type),target::this
integer(i_kind), intent(inout):: itarg
include "type_parameter_locpointer.inc"
include "type_parameter_point2this.inc"
!-----------------------------------------------------------------------
if(itarg>-1) then
   itarg = itarg-nxy(1)
endif
!-----------------------------------------------------------------------
endsubroutine real_itarg

!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end submodule mg_domain
