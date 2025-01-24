   subroutine vqc_stp(pen_v,nstep_v,tpg_v,cgt_v,&
                       var_jbv,ibv,ikv)

!    subprogram:  vqc_stpt
!        prgmmr:  Su, Xiujuan                         Date: 2019-05-23
!! !DESCRIPTION:   The subroutine calculate modified penalties in every
!                  iteration for different variational QC schemes.  the
!                  subroutine is called by stp routines.
!
!
!
!
!
!
!  USES

   use kinds, only: r_kind,r_single,r_double,i_kind
   use constants, only: tiny_r_kind,half,two,one
   use qcmod, only: nlnqc_iter,njqc,vqc,nvqc,hub_norm
   use pvqc, only: vqch,vqcs

   implicit none
! !INPUT PARAMETERS:

    real(r_kind),      intent(in   ) :: tpg_v,cgt_v,var_jbv
    integer(i_kind),   intent(in   ) :: ibv,ikv,nstep_v

!  INPUT/OUTPUT PARAMETERS:
    
   real(r_kind),dimension(max(1,nstep_v)),intent(inout)::pen_v 

! Declare local variables
   
   real(r_kind) wnotgross,wgross,qq,g_nvqc,w_nvqc
   integer kk

   
     

! Declare passed variables
!       Note:  if wgross=0 (no gross error, then wnotgross=1 and this
!       all reduces to the linear case (no qc)

        if (vqc .and. nlnqc_iter .and. tpg_v > tiny_r_kind .and. cgt_v >tiny_r_kind) then
           wnotgross= one-tpg_v
           wgross =tpg_v*cgt_v/wnotgross
           do kk=1,max(1,nstep_v)
              pen_v(kk) = -two*log((exp(-half*pen_v(kk))+wgross)/(one+wgross))
           end do
        else if (nvqc .and. ibv >0) then     ! new variational qc
           do kk=1,max(1,nstep_v)
              qq=sqrt(pen_v(kk))
              if(hub_norm) then
                 call vqch(ibv,ikv,qq,g_nvqc,w_nvqc)
              else
                 call vqcs(ibv,ikv,qq,g_nvqc,w_nvqc)
              endif
              pen_v(kk)=-two*g_nvqc
           enddo
        else if(njqc .and. var_jbv  > tiny_r_kind .and. var_jbv <10.0_r_kind) then
           do kk=1,max(1,nstep_v)
              pen_v(kk) = two*two*var_jbv*log(cosh(sqrt(pen_v(kk)/(two*var_jbv))))
           enddo
        endif

        return
        end subroutine vqc_stp
