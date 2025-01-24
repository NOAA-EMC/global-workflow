!-------------------------------------------------------------------------
!    NOAA/NCEP, National Centers for Environmental Prediction GSI
!    !
!-------------------------------------------------------------------------
!BOP
!
! !ROUTINE:  vqc_setup ---the subroutine to calculate all the penalties called in setup routine  
!
! !INTERFACE:

           subroutine vqc_setup(vals,ratio_err,obserr,cvar_pgv,cvar_bv,ibv,ikv,var_jbv,rat_err2,wgt,valqc_v)

!    subprogram:  vqc_setup
!        prgmmr:  Su, Xiujuan                         Date: 2019-05-22
! !DESCRIPTION:   The subroutine calculate penalties depending input which variational 
!                 qc scheme we use, this part is used to be in setup
!                 routines.  You can call this subroutine to get
!                 penalties and weight for different variational QC
!                 schemes.
! input parameters:
!           vals     :the difference of observation and background normalied by error.
!           ratio_err:adjusted factor of error due to various qc or time.
!           obserr   : observation erro.
!           cvar_pgv : a parameter for ECMWF VQC
!           cvar_bv  : a parameter for ECMWF VQC.
!           ibv      : a parameter for Hubert Norm and logistic VQC, betta.
!           ikv      : a parameter for Hubert Norm and logistic VQC, kappa.
!           var_jbv  : a parameter for  logistic VQC:b.
!
! output parameter: 
!            wgt      : weight when applied VQC <=1.
!            valqc_v  : penalties         
!            rat_err2: adjusted error facter square
!!  USES

    use kinds, only: r_kind,r_single,r_double,i_kind
    use constants, only: tiny_r_kind,half,two,cg_term
    use constants,only: zero,one,two
    use qcmod,only: njqc,vqc,nvqc,hub_norm
    
    use pvqc, only: vqch,vqcs
    
 implicit none

! !INPUT PARAMETERS:

   real(r_kind),      intent(in   ) :: vals,obserr,cvar_pgv,cvar_bv,var_jbv
   integer(i_kind),   intent(in   ) :: ibv,ikv

!  INPUT/OUTPUT PARAMETERS:

   real(r_kind),      intent(in) :: ratio_err

!  OUTPUT PARAMETERS:

   real(r_kind),      intent(out) :: wgt,valqc_v,rat_err2 


! !REVISION HISTORY:

!   2019-05-23     X. Su
       

      
! Declare local variables

   real(r_kind) val2,term,arg,exp_arg

   real(r_kind) wnotgross,cg_t,wgross
   real(r_kind) g_nvqc,w_nvqc


        val2     = vals*vals
        exp_arg  = -half*val2
        rat_err2 = ratio_err**2
        if(njqc .and. var_jbv>tiny_r_kind .and. var_jbv < 10.0_r_kind .and. obserr >tiny_r_kind)  then
           if(exp_arg  == zero) then
              wgt=one
           else
              wgt=vals/sqrt(two*var_jbv)
              wgt=tanh(wgt)/wgt
           endif
           term=-two*var_jbv*rat_err2*log(cosh((vals)/sqrt(two*var_jbv)))
           valqc_v = -two*term
        else if (vqc .and. cvar_pgv> tiny_r_kind .and.obserr >tiny_r_kind) then
           arg  = exp(exp_arg)
           wnotgross= one-cvar_pgv
           cg_t=cvar_bv
           wgross = cg_term*cvar_pgv/(cg_t*wnotgross)
           term =log((arg+wgross)/(one+wgross))
           wgt  = one-wgross/(arg+wgross)
           valqc_v = -two*rat_err2*term
        else if(nvqc .and. ibv >0) then
           if(hub_norm) then 
              call vqch(ibv,ikv,vals,g_nvqc,w_nvqc)
           else
              call vqcs(ibv,ikv,vals,g_nvqc,w_nvqc)
           endif
           valqc_v=-two*rat_err2*g_nvqc
           if(vals ==zero) then
              wgt=one
           else
               wgt=g_nvqc/exp_arg
           endif 
        else
           term = exp_arg
           wgt  = one
           valqc_v = -two*rat_err2*term
        endif

        return
        end subroutine vqc_setup 
