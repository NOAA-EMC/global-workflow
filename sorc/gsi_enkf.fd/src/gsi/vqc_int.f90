            subroutine vqc_int(error2,rat_error2,t_pgv,cg_tv,var_jbv,ibv,ikv,valv,gradv)

!   $$$ documentation block
!
!   prgmmr:      X.Su                    date: 2019-05-29
!
!   
!  abstract:   Variational package to be called by int subroutines.
!  
!  program history log:
!  
!  2019-05-29   Su
!  
! $$$

      use kinds, only: r_kind,i_kind
      use constants, only: half,one,two,tiny_r_kind
      use qcmod, only: nlnqc_iter,njqc,vqc,nvqc,hub_norm
      use pvqc, only: vqch,vqcs

      implicit none
! !INPUT PARAMETERS:
      real(r_kind),      intent(in   ) :: t_pgv,cg_tv,var_jbv,rat_error2,error2
      integer(i_kind),   intent(in   ) :: ibv,ikv
! INPUT/OUTPUT PARAMETERS:
      real(r_kind),      intent(inout) ::  valv     
      real(r_kind),      intent(out) ::  gradv     
! Declare local variables

      real(r_kind) wnotgross,wgross,g_nvqc,w_nvqc,p0,qq



   
           if (vqc .and. nlnqc_iter .and. t_pgv > tiny_r_kind .and.  &
                                cg_tv  > tiny_r_kind) then
              wnotgross= one-t_pgv
              wgross =t_pgv*cg_tv/wnotgross
              p0=wgross/(wgross+exp(-half*error2*valv**2))
              valv=valv*(one-p0)
              gradv = valv*rat_error2*error2
           else if (njqc .and. var_jbv > tiny_r_kind .and. var_jbv <10.0_r_kind) then
              valv=sqrt(two*var_jbv)*tanh(sqrt(error2)*valv/sqrt(two*var_jbv))
              gradv = valv*rat_error2*sqrt(error2)
           else if (nvqc .and. ibv >0) then
              qq=valv*sqrt(error2)
              if(hub_norm) then
                 call vqch(ibv,ikv,qq,g_nvqc,w_nvqc)
              else
                 call vqcs(ibv,ikv,qq,g_nvqc,w_nvqc)
              endif
              gradv=w_nvqc*qq*sqrt(error2)*rat_error2
           else
              gradv = valv*rat_error2*error2
           endif



        return
        end subroutine vqc_int
