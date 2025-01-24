submodule(mg_intstate) mg_generations
!$$$  submodule documentation block
!                .      .    .                                       .
! module:   mg_generations
!   prgmmr: rancic           org: NCEP/EMC            date: 2022
!
! abstract:  Contains procedures that include differrent generations
!            (offset version)
!
! module history log:
!   2023-04-19  lei     - object-oriented coding
!   2024-01-11  rancic  - optimization for ensemble localization
!   2024-02-20  yokota  - refactoring to apply for GSI
!
! Subroutines Included:
!   upsending_all -
!   downsending_all -
!   weighting_all -
!   upsending -
!   downsending -
!   upsending_highest -
!   downsending_highest -
!   upsending2 -
!   downsending2 -
!   upsending_ens -
!   downsending_ens -
!   upsending_ens_nearest -
!   downsending_ens_nearest -
!   upsending2_ens -
!   downsending2_ens -
!   upsending_loc_g3 -
!   upsending_loc_g4 -
!   downsending_loc_g3 -
!   downsending_loc_g4 -
!   weighting_helm -
!   weighting -
!   weighting_highest -
!   weighting_ens -
!   weighting_loc_g3 -
!   weighting_loc_g4 -
!   adjoint -
!   direct1 -
!   adjoint2 -
!   direct2 -
!   adjoint_nearest -
!   direct_nearest -
!   adjoint_highest -
!   direct_highest -
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

!***********************************************************************
!                                                                      !
!                                                                      !
!                                                     M. Rancic (2022) !
!***********************************************************************
use mpi
use kinds, only: r_kind,i_kind
use mg_timers
!TEST
use, intrinsic:: ieee_arithmetic
!TEST

interface weighting_loc
  module procedure weighting_loc_g3
  module procedure weighting_loc_g4
endinterface

interface upsending_loc
  module procedure upsending_loc_g3
  module procedure upsending_loc_g4
endinterface

interface downsending_loc
  module procedure downsending_loc_g3
  module procedure downsending_loc_g4
endinterface
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
contains

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending_all &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend:                                     !
!                                                                      !
!***********************************************************************
(this,V,H,lquart)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
logical, intent(in):: lquart
!-----------------------------------------------------------------------

        if(lquart) then
           call this%upsending2(V,H) 
        else
           call this%upsending(V,H) 
        endif

!-----------------------------------------------------------------------
endsubroutine upsending_all 

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending_all &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add:                                      !
!      First from gm->g3...->g2                                        !
!      Then  from g2->g1                                               !
!                                                                      !
!***********************************************************************
(this,H,V,lquart)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
logical, intent(in):: lquart
!-----------------------------------------------------------------------

        if(lquart) then
           call this%downsending2(H,V) 
        else
           call this%downsending(H,V) 
        endif

!-----------------------------------------------------------------------
endsubroutine downsending_all

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine weighting_all &
!***********************************************************************
!                                                                      !
!  Apply 2D differential operator to compound variable                 !
!                                                                      !
!***********************************************************************
(this,V,H,lhelm)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
logical, intent(in):: lhelm
!-----------------------------------------------------------------------

        if(lhelm) then
           call this%weighting_helm(V,H) 
        else
           call this%weighting(V,H) 
        endif

!-----------------------------------------------------------------------
endsubroutine weighting_all 

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend:                                     !
!       First from g1->g2 (V -> H)                                     !
!       Then  from g2->...->gn  (H -> H)                               !
!                                                                      !
!***********************************************************************
(this,V,H)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
real(r_kind),dimension(this%km,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(this%km,-1:this%imL+2,-1:this%jmL+2):: H_INT
integer(i_kind):: g,L
!-----------------------------------------------------------------------
!
! From generation 1 to generation 2
!

        call this%adjoint(V(1:this%km,1:this%im,1:this%jm),V_INT,this%km,1) 

        call this%bocoT_2d(V_INT,this%km,this%imL,this%jmL,2,2)

        call this%upsend_all(V_INT(1:this%km,1:this%imL,1:this%jmL),H,this%km)
!
! From generation 2 sequentially to higher generations
!
  do g=2,this%gm-1 

    if(g==this%my_hgen) then
        call this%adjoint(H(1:this%km,1:this%im,1:this%jm),H_INT,this%km,g) 
    endif

        call this%bocoT_2d(H_INT,this%km,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,g,g)

        call this%upsend_all(H_INT(1:this%km,1:this%imL,1:this%jmL),H,this%km,g,g+1)

  end do    

!-----------------------------------------------------------------------
endsubroutine upsending

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add:                                      !
!      First from gm->g3...->g2                                        !
!      Then  from g2->g1                                               !
!                                                                      !
!***********************************************************************
(this,H,V)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(this%km,-1:this%imL+2,-1:this%jmL+2):: H_INT
real(r_kind),dimension(this%km,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(this%km,1:this%im,1:this%jm):: H_PROX
real(r_kind),dimension(this%km,1:this%im,1:this%jm):: V_PROX
integer(i_kind):: g,l,k
integer(i_kind):: iL,jL,i,j
!-----------------------------------------------------------------------
!
! Upper generations
!
    do g=this%gm,3,-1

        call this%downsend_all(H(1:this%km,1:this%im,1:this%jm),H_INT(1:this%km,1:this%imL,1:this%jmL),this%km,g,g-1)
        call this%boco_2d(H_INT,this%km,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,g-1,g-1)

      if(this%my_hgen==g-1) then
        call this%direct1(H_INT,H_PROX,this%km,g-1)
        H(1:this%km,1:this%im,1:this%jm)=H     (1:this%km,1:this%im,1:this%jm) &
                                        +H_PROX(1:this%km,1:this%im,1:this%jm)
      endif

    enddo

!
! From geneartion 2 to generation 1
!

        call this%downsend_all(H(1:this%km,1:this%im,1:this%jm),V_INT(1:this%km,1:this%imL,1:this%jmL),this%km)
          H(:,:,:)=0.

        call this%boco_2d(V_INT,this%km,this%imL,this%jmL,2,2)

        call this%direct1(V_INT,V_PROX,this%km,1)

          V(1:this%km,1:this%im,1:this%jm)=V     (1:this%km,1:this%im,1:this%jm) &
                                          +V_PROX(1:this%km,1:this%im,1:this%jm)

!-----------------------------------------------------------------------
endsubroutine downsending

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending_highest &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend:                                     !
!       First from g1->g2 (V -> H)                                     !
!       Then  from g2->...->gn  (H -> H)                               !
!                                                                      !
!***********************************************************************
(this,V,H)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
real(r_kind),dimension(this%km,-1:this%imL+2,-1:this%jmL+2):: H_INT
integer(i_kind):: g
!-----------------------------------------------------------------------
!
! From generation 1 to higher generations
!
  H(:,:,:)=0.
  H(1:this%km,1:this%im0(1),1:this%jm0(1))=V(1:this%km,1:this%im0(1),1:this%jm0(1))
  do g=1,this%gm-1 
        call this%adjoint_highest(H(1:this%km,1:this%im0(g),1:this%jm0(g)),&
             & H_INT(1:this%km,-1:this%im0(g+1)+2,-1:this%jm0(g+1)+2),this%km,g) 
        H(1:this%km,1:this%im0(g),1:this%jm0(g))=0.
        H(1:this%km,1:this%im0(g+1),1:this%jm0(g+1))=H_INT(1:this%km,1:this%im0(g+1),1:this%jm0(g+1))
        H_INT(1:this%km,-1:this%im0(g+1)+2,-1:this%jm0(g+1)+2)=0.
  end do

!-----------------------------------------------------------------------
endsubroutine upsending_highest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending_highest &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add:                                      !
!      First from gm->g3...->g2                                        !
!      Then  from g2->g1                                               !
!                                                                      !
!***********************************************************************
(this,H,V)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(this%km,-1:this%imL+2,-1:this%jmL+2):: H_INT
integer(i_kind):: g
!-----------------------------------------------------------------------
!
! Upper generations
!
  do g=this%gm,2,-1
     H_INT(1:this%km,-1:this%im0(g)+2,-1:this%jm0(g)+2)=0.
     H_INT(1:this%km,1:this%im0(g),1:this%jm0(g))=H(1:this%km,1:this%im0(g),1:this%jm0(g))
     H(1:this%km,1:this%im0(g-1),1:this%jm0(g-1))=0.
     call this%direct_highest(H_INT(1:this%km,-1:this%im0(g)+2,-1:this%jm0(g)+2),&
          & H(1:this%km,1:this%im0(g-1),1:this%jm0(g-1)),this%km,g-1)
  enddo
  V(:,:,:)=0.
  V(1:this%km,1:this%im0(1),1:this%jm0(1))=H(1:this%km,1:this%im0(1),1:this%jm0(1))
  H(:,:,:)=0.

!-----------------------------------------------------------------------
endsubroutine downsending_highest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending2 &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend:                                     !
!       First from g1->g2 (V -> H)                                     !
!       Then  from g2->...->gn  (H -> H)                               !
!                                                                      !
!***********************************************************************
(this,V,H)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
real(r_kind),dimension(this%km,0:this%imL+1,0:this%jmL+1):: V_INT
real(r_kind),dimension(this%km,0:this%imL+1,0:this%jmL+1):: H_INT
integer(i_kind):: g,L
!-----------------------------------------------------------------------
!
! From generation 1 to generation 2
!

        call this%adjoint2(V(1:this%km,1:this%im,1:this%jm),V_INT,this%km,1) 

        call this%bocoT_2d(V_INT,this%km,this%imL,this%jmL,1,1)

        call this%upsend_all(V_INT(1:this%km,1:this%imL,1:this%jmL),H,this%km)
!
! From generation 2 sequentially to higher generations
!
  do g=2,this%gm-1 

    if(g==this%my_hgen) then
        call this%adjoint2(H(1:this%km,1:this%im,1:this%jm),H_INT,this%km,g) 
    endif

        call this%bocoT_2d(H_INT,this%km,this%imL,this%jmL,1,1,this%FimaxL,this%FjmaxL,g,g)

        call this%upsend_all(H_INT(1:this%km,1:this%imL,1:this%jmL),H,this%km,g,g+1)

  end do    

!-----------------------------------------------------------------------
endsubroutine upsending2

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending2 &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add:                                      !
!      First from gm->g3...->g2                                        !
!      Then  from g2->g1                                               !
!                                                                      !
!***********************************************************************
(this,H,V)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(this%km,0:this%imL+1,0:this%jmL+1):: H_INT
real(r_kind),dimension(this%km,0:this%imL+1,0:this%jmL+1):: V_INT
real(r_kind),dimension(this%km,1:this%im,1:this%jm):: H_PROX
real(r_kind),dimension(this%km,1:this%im,1:this%jm):: V_PROX
integer(i_kind):: g,l,k
integer(i_kind):: iL,jL,i,j
!-----------------------------------------------------------------------
!
! Upper generations
!
    do g=this%gm,3,-1

        call this%downsend_all(H(1:this%km,1:this%im,1:this%jm),H_INT(1:this%km,1:this%imL,1:this%jmL),this%km,g,g-1)
        call this%boco_2d(H_INT,this%km,this%imL,this%jmL,1,1,this%FimaxL,this%FjmaxL,g-1,g-1)

      if(this%my_hgen==g-1) then
        call this%direct2(H_INT,H_PROX,this%km,g-1)
        H(1:this%km,1:this%im,1:this%jm)=H     (1:this%km,1:this%im,1:this%jm) &
                                        +H_PROX(1:this%km,1:this%im,1:this%jm)
      endif

    enddo

!
! From generation 2 to generation 1
!

        call this%downsend_all(H(1:this%km,1:this%im,1:this%jm),V_INT(1:this%km,1:this%imL,1:this%jmL),this%km)
          H(:,:,:)=0.

        call this%boco_2d(V_INT,this%km,this%imL,this%jmL,1,1)

        call this%direct2(V_INT,V_PROX,this%km,1)

          V(1:this%km,1:this%im,1:this%jm)=V     (1:this%km,1:this%im,1:this%jm) &
                                          +V_PROX(1:this%km,1:this%im,1:this%jm)

!-----------------------------------------------------------------------
endsubroutine downsending2

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending_ens &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend:                                     !
!       First from g1->g2 (V -> H)                                     !
!       Then  from g2->...->gn  (H -> H)                               !
!                                                                      !
!***********************************************************************
(this,V,H,kmx)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind), intent(in):: kmx
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
real(r_kind),dimension(kmx,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(kmx,-1:this%imL+2,-1:this%jmL+2):: H_INT
integer(i_kind):: g,L
!-----------------------------------------------------------------------
!
! From generation 1 to generation 2
!

        call this%adjoint(V(1:kmx,1:this%im,1:this%jm),V_INT,kmx,1)

        call this%bocoT_2d(V_INT,kmx,this%imL,this%jmL,2,2)

        call this%upsend_all(V_INT(1:kmx,1:this%imL,1:this%jmL),H,kmx)
!
! From generation 2 sequentially to higher generations
!
  do g=2,this%gm-1

    if(g==this%my_hgen) then
        call this%adjoint(H(1:kmx,1:this%im,1:this%jm),H_INT,kmx,g)
    endif

        call this%bocoT_2d(H_INT,kmx,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,g,g)

        call this%upsend_all(H_INT(1:kmx,1:this%imL,1:this%jmL),H,kmx,g,g+1)

  end do

!-----------------------------------------------------------------------
endsubroutine upsending_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending_ens &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add:                                      !
!      First from gm->g3...->g2                                        !
!      Then  from g2->g1                                               !
!                                                                      !
!***********************************************************************
(this,H,V,kmx)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind), intent(in):: kmx
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(kmx,-1:this%imL+2,-1:this%jmL+2):: H_INT
real(r_kind),dimension(kmx,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(kmx,1:this%im,1:this%jm):: H_PROX
real(r_kind),dimension(kmx,1:this%im,1:this%jm):: V_PROX
integer(i_kind):: g,l,k
integer(i_kind):: iL,jL,i,j
!-----------------------------------------------------------------------
!
! Upper generations
!
    do g=this%gm,3,-1

        call this%downsend_all(H(1:kmx,1:this%im,1:this%jm),H_INT(1:kmx,1:this%imL,1:this%jmL),kmx,g,g-1)

        call this%boco_2d(H_INT,kmx,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,g-1,g-1)

      if(this%my_hgen==g-1) then
        call this%direct1(H_INT,H_PROX,kmx,g-1)
        H(1:kmx,1:this%im,1:this%jm)=H     (1:kmx,1:this%im,1:this%jm) &
                                    +H_PROX(1:kmx,1:this%im,1:this%jm)
      endif

    enddo

!
! From geneartion 2 to generation 1
!

        call this%downsend_all(H(1:kmx,1:this%im,1:this%jm),V_INT(1:kmx,1:this%imL,1:this%jmL),kmx)
          H(:,:,:)=0.

        call this%boco_2d(V_INT,kmx,this%imL,this%jmL,2,2)

        call this%direct1(V_INT,V_PROX,kmx,1)

          V(1:kmx,1:this%im,1:this%jm)=V     (1:kmx,1:this%im,1:this%jm) &
                                      +V_PROX(1:kmx,1:this%im,1:this%jm)

!-----------------------------------------------------------------------
endsubroutine downsending_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending_ens_nearest &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend:                                     !
!       First from g1->g2 (V -> H)                                     !
!       Then  from g2->...->gn  (H -> H)                               !
!                                                                      !
!***********************************************************************
(this,V,H,kmx)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind), intent(in):: kmx
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
real(r_kind),dimension(kmx,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(kmx,-1:this%imL+2,-1:this%jmL+2):: H_INT
integer(i_kind):: g,L
!-----------------------------------------------------------------------
!
! From generation 1 to generation 2
!

        call this%adjoint_nearest(V(1:kmx,1:this%im,1:this%jm),V_INT,kmx,1)

        call this%bocoT_2d(V_INT,kmx,this%imL,this%jmL,2,2)

        call this%upsend_all(V_INT(1:kmx,1:this%imL,1:this%jmL),H,kmx)
!
! From generation 2 sequentially to higher generations
!
  do g=2,this%gm-1

    if(g==this%my_hgen) then
        call this%adjoint_nearest(H(1:kmx,1:this%im,1:this%jm),H_INT,kmx,g)
    endif

        call this%bocoT_2d(H_INT,kmx,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,g,g)

        call this%upsend_all(H_INT(1:kmx,1:this%imL,1:this%jmL),H,kmx,g,g+1)

  end do

!-----------------------------------------------------------------------
endsubroutine upsending_ens_nearest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending_ens_nearest &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add:                                      !
!      First from gm->g3...->g2                                        !
!      Then  from g2->g1                                               !
!                                                                      !
!***********************************************************************
(this,H,V,kmx)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind), intent(in):: kmx
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(kmx,-1:this%imL+2,-1:this%jmL+2):: H_INT
real(r_kind),dimension(kmx,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(kmx,1:this%im,1:this%jm):: H_PROX
real(r_kind),dimension(kmx,1:this%im,1:this%jm):: V_PROX
integer(i_kind):: g,l,k
integer(i_kind):: iL,jL,i,j
!-----------------------------------------------------------------------
!
! Upper generations
!
    do g=this%gm,3,-1

        call this%downsend_all(H(1:kmx,1:this%im,1:this%jm),H_INT(1:kmx,1:this%imL,1:this%jmL),kmx,g,g-1)

        call this%boco_2d(H_INT,kmx,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,g-1,g-1)

      if(this%my_hgen==g-1) then
        call this%direct_nearest(H_INT,H_PROX,kmx,g-1)
        H(1:kmx,1:this%im,1:this%jm)=H     (1:kmx,1:this%im,1:this%jm) &
                                    +H_PROX(1:kmx,1:this%im,1:this%jm)
      endif

    enddo

!
! From geneartion 2 to generation 1
!

        call this%downsend_all(H(1:kmx,1:this%im,1:this%jm),V_INT(1:kmx,1:this%imL,1:this%jmL),kmx)
          H(:,:,:)=0.

        call this%boco_2d(V_INT,kmx,this%imL,this%jmL,2,2)

        call this%direct_nearest(V_INT,V_PROX,kmx,1)

          V(1:kmx,1:this%im,1:this%jm)=V     (1:kmx,1:this%im,1:this%jm) &
                                      +V_PROX(1:kmx,1:this%im,1:this%jm)

!-----------------------------------------------------------------------
endsubroutine downsending_ens_nearest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending2_ens &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend:                                     !
!       First from g1->g2 (V -> H)                                     !
!       Then  from g2->...->gn  (H -> H)                               !
!                                                                      !
!***********************************************************************
(this,V,H,kmx)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind), intent(in):: kmx
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
real(r_kind),dimension(kmx,0:this%imL+1,0:this%jmL+1):: V_INT
real(r_kind),dimension(kmx,0:this%imL+1,0:this%jmL+1):: H_INT
integer(i_kind):: g,L
!-----------------------------------------------------------------------
!
! From generation 1 to generation 2
!

        call this%adjoint2(V(1:kmx,1:this%im,1:this%jm),V_INT,kmx,1)

        call this%bocoT_2d(V_INT,kmx,this%imL,this%jmL,1,1)

        call this%upsend_all(V_INT(1:kmx,1:this%imL,1:this%jmL),H,kmx)
!
! From generation 2 sequentially to higher generations
!
  do g=2,this%gm-1

    if(g==this%my_hgen) then
        call this%adjoint2(H(1:kmx,1:this%im,1:this%jm),H_INT,kmx,g)
    endif

        call this%bocoT_2d(H_INT,kmx,this%imL,this%jmL,1,1,this%FimaxL,this%FjmaxL,g,g)

        call this%upsend_all(H_INT(1:kmx,1:this%imL,1:this%jmL),H,kmx,g,g+1)

  end do

!-----------------------------------------------------------------------
endsubroutine upsending2_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending2_ens &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add:                                      !
!      First from gm->g3...->g2                                        !
!      Then  from g2->g1                                               !
!                                                                      !
!***********************************************************************
(this,H,V,kmx)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind), intent(in):: kmx
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(kmx,0:this%imL+1,0:this%jmL+1):: H_INT
real(r_kind),dimension(kmx,0:this%imL+1,0:this%jmL+1):: V_INT
real(r_kind),dimension(kmx,1:this%im,1:this%jm):: H_PROX
real(r_kind),dimension(kmx,1:this%im,1:this%jm):: V_PROX
integer(i_kind):: g,l,k
integer(i_kind):: iL,jL,i,j
!-----------------------------------------------------------------------
!
! Upper generations
!
    do g=this%gm,3,-1

        call this%downsend_all(H(1:kmx,1:this%im,1:this%jm),H_INT(1:kmx,1:this%imL,1:this%jmL),kmx,g,g-1)

        call this%boco_2d(H_INT,kmx,this%imL,this%jmL,1,1,this%FimaxL,this%FjmaxL,g-1,g-1)

      if(this%my_hgen==g-1) then
        call this%direct2(H_INT,H_PROX,kmx,g-1)
        H(1:kmx,1:this%im,1:this%jm)=H     (1:kmx,1:this%im,1:this%jm) &
                                    +H_PROX(1:kmx,1:this%im,1:this%jm)
      endif

    enddo

!
! From geneartion 2 to generation 1
!

        call this%downsend_all(H(1:kmx,1:this%im,1:this%jm),V_INT(1:kmx,1:this%imL,1:this%jmL),kmx)
          H(:,:,:)=0.

        call this%boco_2d(V_INT,kmx,this%imL,this%jmL,1,1)

        call this%direct2(V_INT,V_PROX,kmx,1)

          V(1:kmx,1:this%im,1:this%jm)=V     (1:kmx,1:this%im,1:this%jm) &
                                      +V_PROX(1:kmx,1:this%im,1:this%jm)

!-----------------------------------------------------------------------
endsubroutine downsending2_ens


!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending_loc_g3 &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend for localization:                    !
!                                                                      !
!       First from g1->g2:  V(km   ) -> H(km_4)                        !
!       Then  from g2->g3:  H(km_4 ) -> Z(km_16)                       !
!                                                                      !
!***********************************************************************
(this,V,H,Z,km_in,km_4_in,km_16_in)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: km_in,km_4_in,km_16_in
real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: Z
real(r_kind),dimension(km_in   ,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(km_4_in ,-1:this%imL+2,-1:this%jmL+2):: H_INT
real(r_kind),dimension(km_16_in,-1:this%imL+2,-1:this%jmL+2):: Z_INT
integer(i_kind):: g,L,ind,k_low,k_hgh
!-----------------------------------------------------------------------
!
! From generation 1 to generation 2
!

        call this%adjoint(V(1:km_in,1:this%im,1:this%jm),V_INT,km_in,1)
        call this%bocoT_2d(V_INT,km_in,this%imL,this%jmL,2,2)         !?????

     do ind=1,1
       k_low=km_4_in*(ind-1)+1
       k_hgh=km_4_in*ind
       call this%upsend_loc_g12(V_INT(k_low:k_hgh,1:this%imL,1:this%jmL),H,km_4_in,ind)
     enddo

!
! From generation 2 to generation 3
!

        call this%adjoint(H(1:km_4_in,1:this%im,1:this%jm),H_INT,km_4_in,2)
        call this%bocoT_2d_loc(H_INT,km_4_in,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,2)

     do ind=1,4
       k_low=km_16_in*(ind-1)+1
       k_hgh=km_16_in*ind
       call this%upsend_loc_g23(H_INT(k_low:k_hgh,1:this%imL,1:this%jmL),Z,km_16_in,ind)
     enddo

!-----------------------------------------------------------------------
endsubroutine upsending_loc_g3

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine upsending_loc_g4 &
!***********************************************************************
!                                                                      !
!  Adjoint interpolate and upsend for localization:                    !
!                                                                      !
!       First from g1->g2:  V(km   ) -> H(km_4)                        !
!       Then  from g2->g3:  H(km_4 ) -> Z(km_16)                       !
!       Then  from g3->g4:  Z(km_16) -> W(km_64)                       !
!                                                                      !
!***********************************************************************
(this,V,H,Z,W,km_in,km_4_in,km_16_in,km_64_in)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: km_in,km_4_in,km_16_in,km_64_in
real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(in):: V
real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: H
real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: Z
real(r_kind),dimension(km_64_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(out):: W
real(r_kind),dimension(km_in   ,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(km_4_in ,-1:this%imL+2,-1:this%jmL+2):: H_INT
real(r_kind),dimension(km_16_in,-1:this%imL+2,-1:this%jmL+2):: Z_INT
real(r_kind),dimension(km_64_in,-1:this%imL+2,-1:this%jmL+2):: W_INT
integer(i_kind):: g,L,ind,k_low,k_hgh
!-----------------------------------------------------------------------
!
! From generation 1 to generation 2
!

        call this%adjoint(V(1:km_in,1:this%im,1:this%jm),V_INT,km_in,1)
        call this%bocoT_2d(V_INT,km_in,this%imL,this%jmL,2,2)         !?????

     do ind=1,4
       k_low=km_4_in*(ind-1)+1
       k_hgh=km_4_in*ind
       call this%upsend_loc_g12(V_INT(k_low:k_hgh,1:this%imL,1:this%jmL),H,km_4_in,ind)
     enddo

!
! From generation 2 to generation 3
!

        call this%adjoint(H(1:km_4_in,1:this%im,1:this%jm),H_INT,km_4_in,2)
        call this%bocoT_2d_loc(H_INT,km_4_in,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,2)

     do ind=1,4
       k_low=km_16_in*(ind-1)+1
       k_hgh=km_16_in*ind
       call this%upsend_loc_g23(H_INT(k_low:k_hgh,1:this%imL,1:this%jmL),Z,km_16_in,ind)
     enddo

!
! From generation 3 to generation 4
!

        call this%adjoint(Z(1:km_16_in,1:this%im,1:this%jm),Z_INT,km_16_in,3)
        call this%bocoT_2d_loc(H_INT,km_4_in,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,3)

     do ind=1,4
       k_low=km_64_in*(ind-1)+1
       k_hgh=km_64_in*ind
       call this%upsend_loc_g34(Z_INT(k_low:k_hgh,1:this%imL,1:this%jmL),W,km_64_in,ind)
     enddo

!-----------------------------------------------------------------------
endsubroutine upsending_loc_g4

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending_loc_g3 &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add for localization:                     !
!                                                                      !
!      Then  from g3->g2:  Z(km_16) -> H(km_4 )                        !
!      Then  from g2->g1:  H(km_4 ) -> V(km   )                        !
!                                                                      !
!***********************************************************************
(this,Z,H,V,km_in,km_4_in,km_16_in)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: km_in,km_4_in,km_16_in
real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: Z
real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(km_16_in,-1:this%imL+2,-1:this%jmL+2):: Z_INT
real(r_kind),dimension(km_4_in ,-1:this%imL+2,-1:this%jmL+2):: H_INT
real(r_kind),dimension(km_in   ,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(km_16_in,1:this%im,1:this%jm):: Z_PROX
real(r_kind),dimension(km_4_in ,1:this%im,1:this%jm):: H_PROX
real(r_kind),dimension(km_in   ,1:this%im,1:this%jm):: V_PROX
integer(i_kind):: g,l,k
integer(i_kind):: iL,jL,i,j,ind,k_low,k_hgh
!-----------------------------------------------------------------------
!
! From generation 3 to generation 2
!
     do ind=1,4
       k_low=km_16_in*(ind-1)+1
       k_hgh=km_16_in*ind
        call this%downsend_loc_g32(Z(1:km_16_in,1:this%im,1:this%jm),H_INT(k_low:k_hgh,1:this%imL,1:this%jmL),km_16_in,ind)
     enddo
          Z(:,:,:)=0.

        call this%boco_2d_loc(H_INT,km_4_in ,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,2)
        call this%direct1(H_INT,H_PROX,km_4_in,2)

        H(1:km_4_in ,1:this%im,1:this%jm)=H     (1:km_4_in ,1:this%im,1:this%jm) &
                                         +H_PROX(1:km_4_in ,1:this%im,1:this%jm)

!
! From geneartion 2 to generation 1
!
     do ind=1,4
       k_low=km_4_in*(ind-1)+1
       k_hgh=km_4_in*ind
        call this%downsend_loc_g21(H(1:km_4_in,1:this%im,1:this%jm),V_INT(k_low:k_hgh,1:this%imL,1:this%jmL),km_4_in,ind)
     enddo
          H(:,:,:)=0.

        call this%boco_2d(V_INT,km_in,this%imL,this%jmL,2,2)
        call this%direct1(V_INT,V_PROX,km_in,1)

          V(1:km_in,1:this%im,1:this%jm)=V     (1:km_in,1:this%im,1:this%jm) &
                                        +V_PROX(1:km_in,1:this%im,1:this%jm)

!-----------------------------------------------------------------------
endsubroutine downsending_loc_g3

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine downsending_loc_g4 &
!***********************************************************************
!                                                                      !
!  Downsend, interpolate and add for localization:                     !
!                                                                      !
!      First from g4->g3:  W(km_16) -> Z(km_64)                        !
!      Then  from g3->g2:  Z(km_16) -> H(km_4 )                        !
!      Then  from g2->g1:  H(km_4 ) -> V(km   )                        !
!                                                                      !
!***********************************************************************
(this,W,Z,H,V,km_in,km_4_in,km_16_in,km_64_in)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: km_in,km_4_in,km_16_in,km_64_in
real(r_kind),dimension(km_64_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: W
real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: Z
real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(km_64_in,-1:this%imL+2,-1:this%jmL+2):: W_INT
real(r_kind),dimension(km_16_in,-1:this%imL+2,-1:this%jmL+2):: Z_INT
real(r_kind),dimension(km_4_in ,-1:this%imL+2,-1:this%jmL+2):: H_INT
real(r_kind),dimension(km_in   ,-1:this%imL+2,-1:this%jmL+2):: V_INT
real(r_kind),dimension(km_16_in,1:this%im,1:this%jm):: Z_PROX
real(r_kind),dimension(km_4_in ,1:this%im,1:this%jm):: H_PROX
real(r_kind),dimension(km_in   ,1:this%im,1:this%jm):: V_PROX
integer(i_kind):: g,l,k
integer(i_kind):: iL,jL,i,j,ind,k_low,k_hgh
!-----------------------------------------------------------------------
!
! From generation 4 to generation 3
!
     do ind=1,4
       k_low=km_64_in*(ind-1)+1
       k_hgh=km_64_in*ind
        call this%downsend_loc_g43(W(1:km_64_in,1:this%im,1:this%jm),Z_INT(k_low:k_hgh,1:this%imL,1:this%jmL),km_64_in,ind)
     enddo
        W(:,:,:)=0.

        call this%boco_2d_loc(Z_INT,km_16_in,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,3)
        call this%direct1(Z_INT,Z_PROX,km_16_in,3)

        Z(1:km_16_in,1:this%im,1:this%jm)=Z     (1:km_16_in,1:this%im,1:this%jm) &
                                         +Z_PROX(1:km_16_in,1:this%im,1:this%jm)

!
! From generation 3 to generation 2
!
     do ind=1,4
       k_low=km_16_in*(ind-1)+1
       k_hgh=km_16_in*ind
        call this%downsend_loc_g32(Z(1:km_16_in,1:this%im,1:this%jm),H_INT(k_low:k_hgh,1:this%imL,1:this%jmL),km_16_in,ind)
     enddo
          Z(:,:,:)=0.

        call this%boco_2d_loc(H_INT,km_4_in ,this%imL,this%jmL,2,2,this%FimaxL,this%FjmaxL,2)
        call this%direct1(H_INT,H_PROX,km_4_in,2)

        H(1:km_4_in ,1:this%im,1:this%jm)=H     (1:km_4_in ,1:this%im,1:this%jm) &
                                         +H_PROX(1:km_4_in ,1:this%im,1:this%jm)

!
! From geneartion 2 to generation 1
!
     do ind=1,4
       k_low=km_4_in*(ind-1)+1
       k_hgh=km_4_in*ind
        call this%downsend_loc_g21(H(1:km_4_in,1:this%im,1:this%jm),V_INT(k_low:k_hgh,1:this%imL,1:this%jmL),km_4_in,ind)
     enddo
          H(:,:,:)=0.


        call this%boco_2d(V_INT,km_in,this%imL,this%jmL,2,2)
        call this%direct1(V_INT,V_PROX,km_in,1)

          V(1:km_in,1:this%im,1:this%jm)=V     (1:km_in,1:this%im,1:this%jm) &
                                        +V_PROX(1:km_in,1:this%im,1:this%jm)

!-----------------------------------------------------------------------
endsubroutine downsending_loc_g4

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine weighting_helm &
!***********************************************************************
!                                                                      !
!  Apply 2D differential operator to compound variable                 !
!                                                                      !
!***********************************************************************
(this,V,H)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
real(r_kind),dimension(this%km,0:this%im, 1:this%jm):: DIFX
real(r_kind),dimension(this%km,1:this%im ,0:this%jm):: DIFY
real(r_kind),dimension(this%km,0:this%im, 1:this%jm):: DIFXH
real(r_kind),dimension(this%km,1:this%im ,0:this%jm):: DIFYH
integer(i_kind):: i,j,l,k,imx,jmx
!-----------------------------------------------------------------------

     do j=1,this%jm
     do i=0,this%im
       DIFX(:,i,j)=V(:,i+1,j)-V(:,i,j)
     enddo
     enddo
     do j=0,this%jm
     do i=1,this%im
       DIFY(:,i,j)=V(:,i,j+1)-V(:,i,j)
     enddo
     enddo

     do j=1,this%jm
     do i=1,this%im
       V(:,i,j)=this%a_diff_f(:,i,j)*V(:,i,j)                      &
               -this%b_diff_f(:,i,j)*(DIFX(:,i,j)-DIFX(:,i-1,j)    &
                                     +DIFY(:,i,j)-DIFY(:,i,j-1))   
     enddo
     enddo

if(this%l_hgen) then

!  imx = Fimax(my_hgen)
!  jmx = Fjmax(my_hgen)

   imx = this%im
   jmx = this%jm

     do j=1,jmx
     do i=0,imx
       DIFXH(:,i,j)=H(:,i+1,j)-H(:,i,j)
     enddo
     enddo
     do j=0,jmx
     do i=1,imx
       DIFYH(:,i,j)=H(:,i,j+1)-H(:,i,j)
     enddo
     enddo

     do j=1,jmx
     do i=1,imx
        H(:,i,j)=this%a_diff_h(:,i,j)*H(:,i,j)                          &
                -this%b_diff_h(:,i,j)*(DIFXH(:,i,j)-DIFXH(:,i-1,j)      &
                                      +DIFYH(:,i,j)-DIFYH(:,i,j-1))  
     enddo
     enddo

endif

!-----------------------------------------------------------------------
endsubroutine weighting_helm

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine weighting &
!***********************************************************************
!                                                                      !
!  Apply 2D differential operator to compound variable                 !
!                                                                      !
!***********************************************************************
(this,V,H)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(this%km,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
integer(i_kind):: i,j,l,k,imx,jmx
!-----------------------------------------------------------------------

     do j=1,this%jm
     do i=1,this%im
       V(:,i,j)=this%a_diff_f(:,i,j)*V(:,i,j)                      
     enddo
     enddo

if(this%l_hgen) then

   imx = this%im
   jmx = this%jm

     do j=1,jmx
     do i=1,imx
        H(:,i,j)=this%a_diff_h(:,i,j)*H(:,i,j)                          
     enddo
     enddo

endif

!-----------------------------------------------------------------------
endsubroutine weighting 

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine weighting_highest &
!***********************************************************************
!                                                                      !
!  Apply 2D differential operator to compound variable                 !
!                                                                      !
!***********************************************************************
(this,H)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
real(r_kind),dimension(this%km,1-this%hx:this%imH+this%hx,1-this%hy:this%jmH+this%hy),intent(inout):: H
integer(i_kind):: i,j,imx,jmx
!-----------------------------------------------------------------------

   imx = this%imH
   jmx = this%jmH

   do j=1,jmx
   do i=1,imx
      H(:,i,j)=this%a_diff_h(:,i,j)*H(:,i,j)                          
   enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine weighting_highest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine weighting_ens &
!***********************************************************************
!                                                                      !
!  Apply 2D differential operator to compound variable for ensemble    !
!                                                                      !
!***********************************************************************
(this,V,H,kmx)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: kmx
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(kmx,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H
integer(i_kind):: i,j,l,k,imx,jmx
!-----------------------------------------------------------------------

if(this%l_filt_g1) then
     do j=1,this%jm
     do i=1,this%im
       V(:,i,j)=this%a_diff_f(:,i,j)*V(:,i,j)
     enddo
     enddo
else
     V(:,:,:)=0.
endif

if(this%l_hgen) then

   imx = this%im
   jmx = this%jm

     do j=1,jmx
     do i=1,imx
        H(:,i,j)=this%a_diff_h(:,i,j)*H(:,i,j)
     enddo
     enddo

endif

!-----------------------------------------------------------------------
endsubroutine weighting_ens

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine weighting_loc_g3 &
!***********************************************************************
!                                                                      !
!  Apply 2D differential operator to compound variable in the case     !
!  of localization                                                     !
!                                                                      !
!***********************************************************************
(this,V,H04,H16,km_in,km_4_in,km_16_in)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind), intent(in):: km_in,km_4_in,km_16_in
real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H04
real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H16
integer(i_kind):: i,j,l,k
!-----------------------------------------------------------------------

     do j=1,this%jm
     do i=1,this%im
       V  (1:km_in   ,i,j)=this%w1_loc(1:km_in   ,i,j)*V  (1:km_in   ,i,j)
       H04(1:km_4_in ,i,j)=this%w2_loc(1:km_4_in ,i,j)*H04(1:km_4_in ,i,j)
       H16(1:km_16_in,i,j)=this%w3_loc(1:km_16_in,i,j)*H16(1:km_16_in,i,j)
     enddo
     enddo

!-----------------------------------------------------------------------
endsubroutine weighting_loc_g3

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine weighting_loc_g4 &
!***********************************************************************
!                                                                      !
!  Apply 2D differential operator to compound variable in the case     !
!  of localization                                                     !
!                                                                      !
!***********************************************************************
(this,V,H04,H16,H64,km_in,km_4_in,km_16_in,km_64_in)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind), intent(in):: km_in,km_4_in,km_16_in,km_64_in
real(r_kind),dimension(km_in   ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: V
real(r_kind),dimension(km_4_in ,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H04
real(r_kind),dimension(km_16_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H16
real(r_kind),dimension(km_64_in,1-this%hx:this%im+this%hx,1-this%hy:this%jm+this%hy),intent(inout):: H64
integer(i_kind):: i,j,l,k
!-----------------------------------------------------------------------

     do j=1,this%jm
     do i=1,this%im
       V  (1:km_in   ,i,j)=this%w1_loc(1:km_in   ,i,j)*V  (1:km_in   ,i,j)
       H04(1:km_4_in ,i,j)=this%w2_loc(1:km_4_in ,i,j)*H04(1:km_4_in ,i,j)
       H16(1:km_16_in,i,j)=this%w3_loc(1:km_16_in,i,j)*H16(1:km_16_in,i,j)
       H64(1:km_64_in,i,j)=this%w4_loc(1:km_64_in,i,j)*H64(1:km_64_in,i,j)
     enddo
     enddo

!-----------------------------------------------------------------------
endsubroutine weighting_loc_g4

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine adjoint &
!***********************************************************************
!                                                                      !
!   Mapping from the high to low resolution grid                       !
!   using linearly squared interpolations                              !
!                         - offset version -                           ! 
!                                                                      !
!***********************************************************************
(this,F,W,km_in,g)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: g 
integer(i_kind),intent(in):: km_in
real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(in):: F
real(r_kind), dimension(km_in,-1:this%imL+2,-1:this%jmL+2), intent(out):: W
real(r_kind), dimension(km_in,1:this%im,-1:this%jmL+2):: W_AUX
integer(i_kind):: i,j,iL,jL
!-----------------------------------------------------------------------
!
! 3)
!
     W_AUX(:,:,:)= 0.

  do j=this%jm-mod(this%jm,2),2,-2
    jL = j/2
    do i=this%im,1,-1
      W_AUX(:,i,jL+2)=W_AUX(:,i,jL+2)+this%p_coef(4)*F(:,i,j)
      W_AUX(:,i,jL+1)=W_AUX(:,i,jL+1)+this%p_coef(3)*F(:,i,j)
      W_AUX(:,i,jL  )=W_AUX(:,i,jL  )+this%p_coef(2)*F(:,i,j)
      W_AUX(:,i,jL-1)=W_AUX(:,i,jL-1)+this%p_coef(1)*F(:,i,j)
    enddo
  enddo
!
! 2)
!
  do j=this%jm-1+mod(this%jm,2),1,-2
    jL=j/2
    do i=this%im,1,-1
      W_AUX(:,i,jL+2)=W_AUX(:,i,jL+2)+this%q_coef(4)*F(:,i,j)
      W_AUX(:,i,jL+1)=W_AUX(:,i,jL+1)+this%q_coef(3)*F(:,i,j)
      W_AUX(:,i,jL  )=W_AUX(:,i,jL  )+this%q_coef(2)*F(:,i,j)
      W_AUX(:,i,jL-1)=W_AUX(:,i,jL-1)+this%q_coef(1)*F(:,i,j)
    enddo
  enddo

    W(:,:,:)=0.
!
! 1)
!
  do jL=this%jmL+2,-1,-1
    do i=this%im-1+mod(this%im,2),1,-2
    iL = i/2
      W(:,iL+2,jL)=W(:,iL+2,jL)+this%q_coef(4)*W_AUX(:,i,jL)
      W(:,iL+1,jL)=W(:,iL+1,jL)+this%q_coef(3)*W_AUX(:,i,jL)
      W(:,iL  ,jL)=W(:,iL  ,jL)+this%q_coef(2)*W_AUX(:,i,jL)
      W(:,iL-1,jL)=W(:,iL-1,jL)+this%q_coef(1)*W_AUX(:,i,jL)
    enddo
    do i=this%im-mod(this%im,2),2,-2
    iL=i/2
      W(:,iL+2,jL)=W(:,iL+2,jL)+this%p_coef(4)*W_AUX(:,i,jL)
      W(:,iL+1,jL)=W(:,iL+1,jL)+this%p_coef(3)*W_AUX(:,i,jL)
      W(:,iL  ,jL)=W(:,iL  ,jL)+this%p_coef(2)*W_AUX(:,i,jL)
      W(:,iL-1,jL)=W(:,iL-1,jL)+this%p_coef(1)*W_AUX(:,i,jL)
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine adjoint

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine direct1 &
!***********************************************************************
!                                                                      !
!   Mapping from the low to high resolution grid                       !
!   using linearly squared interpolations                              !
!                         - offset version -                           !
!                                                                      !
!***********************************************************************
(this,W,F,km_in,g)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: g
integer(i_kind),intent(in):: km_in
real(r_kind), dimension(km_in,-1:this%imL+2,-1:this%jmL+2), intent(in):: W
real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(out):: F
real(r_kind), dimension(km_in,1:this%im,-1:this%jmL+2):: W_AUX
integer(i_kind):: i,j,iL,jL
!-----------------------------------------------------------------------
!
! 1)
!
   do jL=-1,this%jmL+2
     do i=1,this%im-1+mod(this%im,2),2
       iL=i/2
         W_AUX(:,i,jL)=this%q_coef(1)*W(:,iL-1,jL)+this%q_coef(2)*W(:,iL  ,jL) &
                      +this%q_coef(3)*W(:,iL+1,jL)+this%q_coef(4)*W(:,iL+2,jL)
     enddo
     do i=2,this%im-mod(this%im,2),2
       iL=i/2
         W_AUX(:,i,jL)=this%p_coef(1)*W(:,iL-1,jL)+this%p_coef(2)*w(:,iL  ,jL) &
                      +this%p_coef(3)*W(:,iL+1,jL)+this%p_coef(4)*W(:,iL+2,jL)
     enddo
   enddo
!
! 2)
!
   do j=1,this%jm-1+mod(this%jm,2),2
     jL=j/2
     do i=1,this%im
       F(:,i,j)=this%q_coef(1)*W_AUX(:,i,jL-1)+this%q_coef(2)*W_AUX(:,i,jL  ) &
               +this%q_coef(3)*W_AUX(:,i,jL+1)+this%q_coef(4)*W_AUX(:,i,jL+2)
     enddo
   enddo
!
! 3)
!
   do j=2,this%jm-mod(this%jm,2),2
     jL=j/2
     do i=1,this%im
       F(:,i,j)=this%p_coef(1)*W_AUX(:,i,jL-1)+this%p_coef(2)*W_AUX(:,i,jL  ) &
               +this%p_coef(3)*W_AUX(:,i,jL+1)+this%p_coef(4)*W_AUX(:,i,jL+2)
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine direct1

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine adjoint2 &
!***********************************************************************
!                                                                      !
!   Mapping from the high to low resolution grid                       !
!   using quadratics interpolations                                    !
!                         - offset version -                           ! 
!                                                                      !
!***********************************************************************
(this,F,W,km_in,g)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: g 
integer(i_kind),intent(in):: km_in
real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(in):: F
real(r_kind), dimension(km_in,0:this%imL+1,0:this%jmL+1), intent(out):: W
real(r_kind), dimension(km_in,1:this%im,0:this%jmL+1):: W_AUX
integer(i_kind):: i,j,iL,jL
!-----------------------------------------------------------------------
!
! 3)
!
     W_AUX(:,:,:)= 0.

  do j=this%jm-mod(this%jm,2),2,-2
    jL = j/2
    do i=this%im,1,-1
      W_AUX(:,i,jL+1)=W_AUX(:,i,jL+1)+this%b_coef(3)*F(:,i,j)
      W_AUX(:,i,jL  )=W_AUX(:,i,jL  )+this%b_coef(2)*F(:,i,j)
      W_AUX(:,i,jL-1)=W_AUX(:,i,jL-1)+this%b_coef(1)*F(:,i,j)
    enddo
  enddo
!
! 2)
!
  do j=this%jm-1+mod(this%jm,2),1,-2
    jL=(j+1)/2
    do i=this%im,1,-1
      W_AUX(:,i,jL+1)=W_AUX(:,i,jL+1)+this%a_coef(3)*F(:,i,j)
      W_AUX(:,i,jL  )=W_AUX(:,i,jL  )+this%a_coef(2)*F(:,i,j)
      W_AUX(:,i,jL-1)=W_AUX(:,i,jL-1)+this%a_coef(1)*F(:,i,j)
    enddo
  enddo

    W(:,:,:)=0.
!
! 1)
!
  do jL=this%jmL+1,0,-1
    do i=this%im-1+mod(this%im,2),1,-2
    iL = (i+1)/2
      W(:,iL+1,jL)=W(:,iL+1,jL)+this%a_coef(3)*W_AUX(:,i,jL)
      W(:,iL  ,jL)=W(:,iL  ,jL)+this%a_coef(2)*W_AUX(:,i,jL)
      W(:,iL-1,jL)=W(:,iL-1,jL)+this%a_coef(1)*W_AUX(:,i,jL)
    enddo
    do i=this%im-mod(this%im,2),2,-2
    iL=i/2
      W(:,iL+1,jL)=W(:,iL+1,jL)+this%b_coef(3)*W_AUX(:,i,jL)
      W(:,iL  ,jL)=W(:,iL  ,jL)+this%b_coef(2)*W_AUX(:,i,jL)
      W(:,iL-1,jL)=W(:,iL-1,jL)+this%b_coef(1)*W_AUX(:,i,jL)
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine adjoint2

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine direct2 &
!***********************************************************************
!                                                                      !
!   Mapping from the low to high resolution grid                       !
!   using quadratic interpolations                                     !
!                         - offset version -                           !
!                                                                      !
!***********************************************************************
(this,W,F,km_in,g)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: g
integer(i_kind),intent(in):: km_in
real(r_kind), dimension(km_in,0:this%imL+1,0:this%jmL+1), intent(in):: W
real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(out):: F
real(r_kind), dimension(km_in,1:this%im,0:this%jmL+1):: W_AUX
integer(i_kind):: i,j,iL,jL
!-----------------------------------------------------------------------
!
! 1)
!
   do jL=0,this%jmL+1
     do i=1,this%im-1+mod(this%im,2),2
       iL=(i+1)/2
         W_AUX(:,i,jL)=this%a_coef(1)*W(:,iL-1,jL)+this%a_coef(2)*W(:,iL  ,jL) &
                      +this%a_coef(3)*W(:,iL+1,jL)
     enddo
     do i=2,this%im-mod(this%im,2),2
       iL=i/2
         W_AUX(:,i,jL)=this%b_coef(1)*W(:,iL-1,jL)+this%b_coef(2)*w(:,iL  ,jL) &
                      +this%b_coef(3)*W(:,iL+1,jL)
     enddo
   enddo
!
! 2)
!
   do j=1,this%jm-1+mod(this%jm,2),2
     jL=(j+1)/2
     do i=1,this%im
       F(:,i,j)=this%a_coef(1)*W_AUX(:,i,jL-1)+this%a_coef(2)*W_AUX(:,i,jL  ) &
               +this%a_coef(3)*W_AUX(:,i,jL+1)
     enddo
   enddo
!
! 3)
!
   do j=2,this%jm-mod(this%jm,2),2
     jL=j/2
     do i=1,this%im
       F(:,i,j)=this%b_coef(1)*W_AUX(:,i,jL-1)+this%b_coef(2)*W_AUX(:,i,jL  ) &
               +this%b_coef(3)*W_AUX(:,i,jL+1)
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine direct2

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine adjoint_nearest &
!***********************************************************************
!                                                                      !
!   Mapping from the high to low resolution grid                       !
!   selecting the nearest point                                        !
!                         - offset version -                           ! 
!                                                                      !
!***********************************************************************
(this,F,W,km_in,g)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: g 
integer(i_kind),intent(in):: km_in
real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(in):: F
real(r_kind), dimension(km_in,-1:this%imL+2,-1:this%jmL+2), intent(out):: W
real(r_kind), dimension(km_in,1:this%im,-1:this%jmL+2):: W_AUX
integer(i_kind):: i,j,iL,jL
!-----------------------------------------------------------------------
!
! 3)
!
     W_AUX(:,:,:)= 0.

  do j=this%jm-mod(this%jm,2),2,-2
    jL = j/2
    do i=this%im,1,-1
      W_AUX(:,i,jL  )=W_AUX(:,i,jL  )+0.5**0.5*F(:,i,j)
    enddo
  enddo
!
! 2)
!
  do j=this%jm-1+mod(this%jm,2),1,-2
    jL=j/2
    do i=this%im,1,-1
      W_AUX(:,i,jL+1)=W_AUX(:,i,jL+1)+0.5**0.5*F(:,i,j)
    enddo
  enddo

    W(:,:,:)=0.
!
! 1)
!
  do jL=this%jmL+2,-1,-1
    do i=this%im-1+mod(this%im,2),1,-2
    iL = i/2
      W(:,iL+1,jL)=W(:,iL+1,jL)+0.5**0.5*W_AUX(:,i,jL)
    enddo
    do i=this%im-mod(this%im,2),2,-2
    iL=i/2
      W(:,iL  ,jL)=W(:,iL  ,jL)+0.5**0.5*W_AUX(:,i,jL)
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine adjoint_nearest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine direct_nearest &
!***********************************************************************
!                                                                      !
!   Mapping from the low to high resolution grid                       !
!   selecting the nearest point                                        !
!                         - offset version -                           !
!                                                                      !
!***********************************************************************
(this,W,F,km_in,g)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: g
integer(i_kind),intent(in):: km_in
real(r_kind), dimension(km_in,-1:this%imL+2,-1:this%jmL+2), intent(in):: W
real(r_kind), dimension(km_in,1:this%im,1:this%jm), intent(out):: F
real(r_kind), dimension(km_in,1:this%im,-1:this%jmL+2):: W_AUX
integer(i_kind):: i,j,iL,jL
!-----------------------------------------------------------------------
!
! 1)
!
   do jL=-1,this%jmL+2
     do i=1,this%im-1+mod(this%im,2),2
       iL=i/2
         W_AUX(:,i,jL)=0.5**0.5*W(:,iL+1,jL)
     enddo
     do i=2,this%im-mod(this%im,2),2
       iL=i/2
         W_AUX(:,i,jL)=0.5**0.5*w(:,iL  ,jL)
     enddo
   enddo
!
! 2)
!
   do j=1,this%jm-1+mod(this%jm,2),2
     jL=j/2
     do i=1,this%im
       F(:,i,j)=0.5**0.5*W_AUX(:,i,jL+1)
     enddo
   enddo
!
! 3)
!
   do j=2,this%jm-mod(this%jm,2),2
     jL=j/2
     do i=1,this%im
       F(:,i,j)=0.5**0.5*W_AUX(:,i,jL  )
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine direct_nearest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine adjoint_highest &
!***********************************************************************
!                                                                      !
!   Mapping from the high to low resolution grid                       !
!   using linearly squared interpolations                              !
!                         - offset version -                           ! 
!                                                                      !
!***********************************************************************
(this,F,W,km_in,g)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: g 
integer(i_kind),intent(in):: km_in
real(r_kind), dimension(km_in,1:this%im0(g),1:this%jm0(g)), intent(in):: F
real(r_kind), dimension(km_in,-1:this%im0(g+1)+2,-1:this%jm0(g+1)+2), intent(out):: W
real(r_kind), dimension(km_in,1:this%im0(g),-1:this%jm0(g+1)+2):: W_AUX
integer(i_kind):: i,j,iL,jL
!-----------------------------------------------------------------------
!
! 3)
!
     W_AUX(:,:,:)= 0.

  do j=this%jm0(g)-mod(this%jm0(g),2),2,-2
    jL = j/2
    do i=this%im0(g),1,-1
      W_AUX(:,i,jL+2)=W_AUX(:,i,jL+2)+this%p_coef(4)*F(:,i,j)
      W_AUX(:,i,jL+1)=W_AUX(:,i,jL+1)+this%p_coef(3)*F(:,i,j)
      W_AUX(:,i,jL  )=W_AUX(:,i,jL  )+this%p_coef(2)*F(:,i,j)
      W_AUX(:,i,jL-1)=W_AUX(:,i,jL-1)+this%p_coef(1)*F(:,i,j)
    enddo
  enddo
!
! 2)
!
  do j=this%jm0(g)-1+mod(this%jm0(g),2),1,-2
    jL=j/2
    do i=this%im0(g),1,-1
      W_AUX(:,i,jL+2)=W_AUX(:,i,jL+2)+this%q_coef(4)*F(:,i,j)
      W_AUX(:,i,jL+1)=W_AUX(:,i,jL+1)+this%q_coef(3)*F(:,i,j)
      W_AUX(:,i,jL  )=W_AUX(:,i,jL  )+this%q_coef(2)*F(:,i,j)
      W_AUX(:,i,jL-1)=W_AUX(:,i,jL-1)+this%q_coef(1)*F(:,i,j)
    enddo
  enddo

    W(:,:,:)=0.
!
! 1)
!
  do jL=this%jm0(g+1)+2,-1,-1
    do i=this%im0(g)-1+mod(this%im0(g),2),1,-2
    iL = i/2
      W(:,iL+2,jL)=W(:,iL+2,jL)+this%q_coef(4)*W_AUX(:,i,jL)
      W(:,iL+1,jL)=W(:,iL+1,jL)+this%q_coef(3)*W_AUX(:,i,jL)
      W(:,iL  ,jL)=W(:,iL  ,jL)+this%q_coef(2)*W_AUX(:,i,jL)
      W(:,iL-1,jL)=W(:,iL-1,jL)+this%q_coef(1)*W_AUX(:,i,jL)
    enddo
    do i=this%im0(g)-mod(this%im0(g),2),2,-2
    iL=i/2
      W(:,iL+2,jL)=W(:,iL+2,jL)+this%p_coef(4)*W_AUX(:,i,jL)
      W(:,iL+1,jL)=W(:,iL+1,jL)+this%p_coef(3)*W_AUX(:,i,jL)
      W(:,iL  ,jL)=W(:,iL  ,jL)+this%p_coef(2)*W_AUX(:,i,jL)
      W(:,iL-1,jL)=W(:,iL-1,jL)+this%p_coef(1)*W_AUX(:,i,jL)
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine adjoint_highest

!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
module subroutine direct_highest &
!***********************************************************************
!                                                                      !
!   Mapping from the low to high resolution grid                       !
!   using linearly squared interpolations                              !
!                         - offset version -                           !
!                                                                      !
!***********************************************************************
(this,W,F,km_in,g)
!-----------------------------------------------------------------------
implicit none
class (mg_intstate_type),target:: this
integer(i_kind),intent(in):: g
integer(i_kind),intent(in):: km_in
real(r_kind), dimension(km_in,-1:this%im0(g+1)+2,-1:this%jm0(g+1)+2), intent(in):: W
real(r_kind), dimension(km_in,1:this%im0(g),1:this%jm0(g)), intent(out):: F
real(r_kind), dimension(km_in,1:this%im0(g),-1:this%jm0(g+1)+2):: W_AUX
integer(i_kind):: i,j,iL,jL
!-----------------------------------------------------------------------
!
! 1)
!
   do jL=-1,this%jm0(g+1)+2
     do i=1,this%im0(g)-1+mod(this%im0(g),2),2
       iL=i/2
         W_AUX(:,i,jL)=this%q_coef(1)*W(:,iL-1,jL)+this%q_coef(2)*W(:,iL  ,jL) &
                      +this%q_coef(3)*W(:,iL+1,jL)+this%q_coef(4)*W(:,iL+2,jL)
     enddo
     do i=2,this%im0(g)-mod(this%im0(g),2),2
       iL=i/2
         W_AUX(:,i,jL)=this%p_coef(1)*W(:,iL-1,jL)+this%p_coef(2)*w(:,iL  ,jL) &
                      +this%p_coef(3)*W(:,iL+1,jL)+this%p_coef(4)*W(:,iL+2,jL)
     enddo
   enddo
!
! 2)
!
   do j=1,this%jm0(g)-1+mod(this%jm0(g),2),2
     jL=j/2
     do i=1,this%im0(g)
       F(:,i,j)=this%q_coef(1)*W_AUX(:,i,jL-1)+this%q_coef(2)*W_AUX(:,i,jL  ) &
               +this%q_coef(3)*W_AUX(:,i,jL+1)+this%q_coef(4)*W_AUX(:,i,jL+2)
     enddo
   enddo
!
! 3)
!
   do j=2,this%jm0(g)-mod(this%jm0(g),2),2
     jL=j/2
     do i=1,this%im0(g)
       F(:,i,j)=this%p_coef(1)*W_AUX(:,i,jL-1)+this%p_coef(2)*W_AUX(:,i,jL  ) &
               +this%p_coef(3)*W_AUX(:,i,jL+1)+this%p_coef(4)*W_AUX(:,i,jL+2)
     enddo
   enddo

!-----------------------------------------------------------------------
endsubroutine direct_highest

!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
end submodule mg_generations
