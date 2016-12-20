 subroutine sto_per_scheme_step1(work_ini, work, w_step1, &
     trieo_lstot_siz4, total_member, cpl_run_calling_number, sto_coef, center)

! this subroutine is used to compute the first step of the 
! stochastic perturbation scheme, in which x_i_dot = t_i + s_i 
! and s_i ~ sum(w_i,j p_j.
!-------------------------------------------------------------

! work_ini is the six hours ago spectral array.  work is the 
! current spectral array and the w_step1 is the step 1 output
! spectral array.
!------------------------------------------------------------


! in the current code, assume each ensemble member uses the same 
! number of the processors.
!---------------------------------------------------------------

 use esmf_mod
 use gefs_cpl_internalstate_esmfmod
 integer                                                             :: trieo_lstot_siz4
 integer                                                             :: total_member
 real(kind=kind_evod), dimension(trieo_lstot_siz4, total_member)     :: work_ini
 real(kind=kind_evod), dimension(trieo_lstot_siz4, total_member)     :: work 
 real(kind=kind_evod), dimension(trieo_lstot_siz4, total_member)     :: w_step1
!real(kind=kind_evod), dimension(trieo_lstot_siz4, total_member - 1) :: w_step1
 real(kind=kind_evod), dimension(total_member - 1, total_member - 1) :: sto_coef
 integer                                                             :: cpl_run_calling_number
 integer,              dimension(total_member - 1)                   :: jp
 real(kind=kind_evod), dimension(total_member - 1)                   :: rp
!dhou, added 03-28-2007
 real(kind=kind_evod), dimension(trieo_lstot_siz4)                   :: w_mean
 integer                                                             :: center

! working arrays.
!----------------
 real(kind=kind_evod), dimension(trieo_lstot_siz4, total_member)     :: work1_ini
 real(kind=kind_evod), dimension(trieo_lstot_siz4, total_member)     :: work1
 integer                                                             :: i, j, k

! put in the computation of the first step of the formulation of
! the stochastic perturbation scheme.
!---------------------------------------------------------------

!jp(1)            = total_member - 2
!jp(2)            = total_member - 1

!do j = 3, total_member - 1
!    jp(j) = j - 2
!end do

!do j = 3, total_member - 1
!! do j = 1, total_member - 1
!    if(mod(j, 2) == 0) then
!        rp(j) = -1.0
!    else
!        rp(j) =  1.0
!    end if
!end do

! for testing.
!-------------
!rp(1) = 0.0
!rp(2) = 0.0

!sto_coef(k,j) is the weight of the k'th perturabtion in member j's formulation 
 do j = 1, total_member - 1     !for the forcing of each member j
     do i = 1, trieo_lstot_siz4
!        w_step1(i, j) = rp(j) * (work(i, jp(j)) - work_ini(i, jp(j)) &
!                              -  work(i, total_member) + work_ini(i, total_member))
         w_step1(i, j) = 0.0
          do k=1, total_member - 1    !contribution from each perturbation k
           w_step1(i, j) = w_step1(i, j) &
              + sto_coef(k, j) * (work(i, k) - work_ini(i, k) &
                               -  work(i, total_member) + work_ini(i, total_member)) 
          end do
     end do
 end do

! new feature in b series  dhou 03-28-2007
! centralize the perturbations w_step1
! find the average of the perturbations
if (center .eq. 1) then
 do i = 1, trieo_lstot_siz4
     w_mean(i) = 0.0
     do j = 1, total_member - 1     !for the forcing of each member j
       w_mean(i) = w_mean(i) + w_step1(i, j)
     end do
       w_mean(i) = w_mean(i)/float(total_member - 1)
 end do
! subtract the average from the perturbations
 do i = 1, trieo_lstot_siz4
     do j = 1, total_member - 1     !for the forcing of each member j
       w_step1(i,j) = w_step1(i,j) - w_mean(i)
     end do
 end do
endif

!rp1 = 0.1   !(al, constant amplitude)
! am,  linear varaition of amplitude
!rp1 = 0.15*(-cpl_run_calling_number+63)/62.0
! an,linear varaition of amplitud starting at 120h (n=20), reduce from 0.1 to 0.05 at n=63 
!rp1=0.1
!if ( cpl_run_calling_number .ge. 20 ) then
!rp1 = (0.1-0.05)*(63-cpl_run_calling_number)/(63.0 - 20.0) + 0.05
!endif
! ap,linear varaition of amplitud starting at 120h (n=20), reduce from 0.1 to 0.01 at n=63 
!rp1=0.1
!if ( cpl_run_calling_number .ge. 20 ) then
!rp1 = (0.1-0.01)*(63-cpl_run_calling_number)/(63.0 - 20.0) + 0.01
!endif
! aq,  same as ap but rp1 is reduced to 50% 
!rs_global=0.1
!if ( cpl_run_calling_number .ge. 20 ) then
!rs_global = (0.1-0.01)*(63-cpl_run_calling_number)/(63.0 - 20.0) + 0.01
!endif
!
!!rs_global=rs_global*1.0    ! ap, ba, bb
! rs_global=-1.0*rs_global    ! bc
! do j = 1, total_member - 1
!     do i = 1, trieo_lstot_siz4
!         work(i, j) = work(i, j) + rs_global * w_step1(i, j)   !upto bc
!!        w_step1(i, j) = rs_global * w_step1(i, j)
!!        work(i, j) = work(i, j) + w_step1(i, j)
!     end do
! end do

 end subroutine sto_per_scheme_step1

 subroutine sto_per_scheme_step1_2(work, w_step1,                         &
     trieo_lstot_siz4, total_member, cpl_run_calling_number, rs_global)

! this subroutine is used to compute the first step of the 
! stochastic perturbation scheme, in which x_i_dot = t_i + s_i 
! and s_i ~ sum(w_i,j p_j.
!-------------------------------------------------------------

! work_ini is the six hours ago spectral array.  work is the 
! current spectral array and the w_step1 is the step 1 output
! spectral array.
!------------------------------------------------------------


! in the current code, assume each ensemble member uses the same 
! number of the processors.
!---------------------------------------------------------------

 use esmf_mod
 use gefs_cpl_internalstate_esmfmod

 integer                                                             :: trieo_lstot_siz4
 integer                                                             :: total_member
 real(kind=kind_evod), dimension(trieo_lstot_siz4, total_member)     :: work 
 real(kind=kind_evod), dimension(trieo_lstot_siz4, total_member)     :: w_step1
 integer                                                             :: cpl_run_calling_number

 real(kind=kind_evod)                                                :: rs_global

!rp1 = 0.1   !(al, constant amplitude)
! am,  linear varaition of amplitude
!rp1 = 0.15*(-cpl_run_calling_number+63)/62.0
! an,linear varaition of amplitud starting at 120h (n=20), reduce from 0.1 to 0.05 at n=63 
!rp1=0.1
!if ( cpl_run_calling_number .ge. 20 ) then
!rp1 = (0.1-0.05)*(63-cpl_run_calling_number)/(63.0 - 20.0) + 0.05
!endif
! ap,linear varaition of amplitud starting at 120h (n=20), reduce from 0.1 to 0.01 at n=63 
!rp1=0.1
!if ( cpl_run_calling_number .ge. 20 ) then
!rp1 = (0.1-0.01)*(63-cpl_run_calling_number)/(63.0 - 20.0) + 0.01
!endif
! aq,  same as ap but rp1 is reduced to 50% 
!rs_global=0.1
!if ( cpl_run_calling_number .ge. 20 ) then
!rs_global = (0.1-0.01)*(63-cpl_run_calling_number)/(63.0 - 20.0) + 0.01
!endif
!!rp1=rp1*1.0    ! ap, ba, bb
!s_global=-1.0*rs_global    ! bc

 do j = 1, total_member - 1
     do i = 1, trieo_lstot_siz4
         work(i, j) = work(i, j) + rs_global * w_step1(i, j)   !upto bc
!        w_step1(i, j) = rs_global * w_step1(i, j)
!        work(i, j) = work(i, j) + w_step1(i, j)
     end do
 end do

 end subroutine sto_per_scheme_step1_2
