module stppm10mod
  
!$$$ module documentation block
!           .      .    .                                       .
! module:   stppm10mod    module for pm10 
!  pgrmmr:
!
! abstract: module for stppm10 
!
! program history log:
!   2016-02-20  pagowski - re-write from pm2_5 for pm10
!   2016-05-18  guo     - replaced ob_type with polymorphic obsNode through type casting
!
! subroutines included:
!   sub stppm10
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
  use m_obsNode , only: obsNode
  use m_pm10Node, only: pm10Node
  use m_pm10Node, only: pm10Node_typecast
  use m_pm10Node, only: pm10Node_nextcast
  implicit none

  private
  public stppm10
  
contains
  
  subroutine stppm10(pm10head,rval,sval,out,sges,nstep)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    stppm10        calcuate penalty and stepsize from pm10
!                            with addition of nonlinear qc.
!   prgmmr: derber           org: np23                date: 1991-02-26
!
! abstract: calculate penalty and contribution to stepsize from pm10
!           using nonlinear qc.
!
! program history log:
!   2016-02-20  pagowski - convert from stppm2_5 for stppm10
!
!   input argument list:
!     pm10head
!     rpm10       - search direction for pm10
!     spm10       - analysis increment for pm10
!     sges     - stepsize estimates (nstep)
!     nstep    - number of stepsize estimates (== 0 means use outer iteration values)
!
!   output argument list:
!     out(1:nstep)   - contribution of penalty from pm10 sges(1:nstep)
!
! attributes:
!   language: f90
!   machine:  ibm RS/6000 SP
!
!$$$
    use kinds, only: r_kind,i_kind,r_quad
    use qcmod, only: nlnqc_iter,varqc_iter
    use constants, only: half,one,two,tiny_r_kind,cg_term,zero_quad,max_varname_length,zero
    use gsi_bundlemod, only: gsi_bundle
    use gsi_bundlemod, only: gsi_bundlegetpointer
    use gridmod, only: cmaq_regional,wrf_mass_regional
    use chemmod, only: d_10,nh4_mfac,oc_mfac
    implicit none
    
! declare passed variables
    class(obsNode), pointer             ,intent(in   ) :: pm10head
    integer(i_kind)                     ,intent(in   ) :: nstep
    real(r_quad),dimension(max(1,nstep)),intent(inout) :: out
    type(gsi_bundle)                    ,intent(in   ) :: rval,sval
    real(r_kind),dimension(max(1,nstep)),intent(in   ) :: sges
    
! declare local variables
    integer(i_kind) ier,istatus
    integer(i_kind) j1,j2,j3,j4,j5,j6,j7,j8,kk
    real(r_kind) cg_pm10,val,val2,wgross,wnotgross,pm10_pg
    real(r_kind),dimension(max(1,nstep))::pen
    real(r_kind) w1,w2,w3,w4,w5,w6,w7,w8,qq
    real(r_kind),pointer,dimension(:):: rpm10,spm10
    type(pm10Node), pointer :: pm10ptr

    character(len=max_varname_length) :: aeroname    

    out=zero_quad

!   If no pm10 data return
    if(.not. associated(pm10head))return

    if (cmaq_regional) then
    
       ier=0
       call gsi_bundlegetpointer(sval,'pm10',spm10,istatus);ier=ier+istatus
       call gsi_bundlegetpointer(rval,'pm10',rpm10,istatus);ier=ier+istatus

       if(ier/=0) return

       !pm10ptr => pm10head
       pm10ptr => pm10Node_typecast(pm10head)
       do while (associated(pm10ptr))
          if(pm10ptr%luse)then
             if(nstep > 0)then
                j1=pm10ptr%ij(1)
                j2=pm10ptr%ij(2)
                j3=pm10ptr%ij(3)
                j4=pm10ptr%ij(4)
                j5=pm10ptr%ij(5)
                j6=pm10ptr%ij(6)
                j7=pm10ptr%ij(7)
                j8=pm10ptr%ij(8)
                w1=pm10ptr%wij(1)
                w2=pm10ptr%wij(2)
                w3=pm10ptr%wij(3)
                w4=pm10ptr%wij(4)
                w5=pm10ptr%wij(5)
                w6=pm10ptr%wij(6)
                w7=pm10ptr%wij(7)
                w8=pm10ptr%wij(8)


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)
                val2=w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)-pm10ptr%res

                do kk=1,nstep
                   qq=val2+sges(kk)*val
                   pen(kk)=qq*qq*pm10ptr%err2
                end do
             else
                pen(1)=pm10ptr%res*pm10ptr%res*pm10ptr%err2
             end if

!  modify penalty term if nonlinear qc

             if (nlnqc_iter .and. pm10ptr%pg > tiny_r_kind .and. &
                  pm10ptr%b  > tiny_r_kind) then
                pm10_pg=pm10ptr%pg*varqc_iter
                cg_pm10=cg_term/pm10ptr%b
                wnotgross= one-pm10_pg
                wgross = pm10_pg*cg_pm10/wnotgross
                do kk=1,max(1,nstep)
                   pen(kk)= -two*log((exp(-half*pen(kk))+wgross)/(one+wgross))
                end do
             endif

             out(1) = out(1)+pen(1)*pm10ptr%raterr2
             do kk=2,nstep
                out(kk) = out(kk)+(pen(kk)-pen(1))*pm10ptr%raterr2
             end do
          end if

          !pm10ptr => pm10ptr%llpoint
          pm10ptr => pm10Node_nextcast(pm10ptr)

       end do

    endif

    if (wrf_mass_regional) then

       !pm10ptr => pm10head
       pm10ptr => pm10Node_typecast(pm10head)

       do while (ASSOCIATED(pm10ptr))
          if(pm10ptr%luse)then
             if(nstep > 0)then
                j1=pm10ptr%ij(1)
                j2=pm10ptr%ij(2)
                j3=pm10ptr%ij(3)
                j4=pm10ptr%ij(4)
                j5=pm10ptr%ij(5)
                j6=pm10ptr%ij(6)
                j7=pm10ptr%ij(7)
                j8=pm10ptr%ij(8)
                w1=pm10ptr%wij(1)
                w2=pm10ptr%wij(2)
                w3=pm10ptr%wij(3)
                w4=pm10ptr%wij(4)
                w5=pm10ptr%wij(5)
                w6=pm10ptr%wij(6)
                w7=pm10ptr%wij(7)
                w8=pm10ptr%wij(8)

                istatus=0

                val2=-pm10ptr%res
                val=zero

                aeroname='bc1'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)+val2


                nullify(spm10,rpm10)


                aeroname='bc2'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)+val2


                nullify(spm10,rpm10)


                aeroname='sulf'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=(w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)) * nh4_mfac +val2


                nullify(spm10,rpm10)


                aeroname='p25'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)+val2


                nullify(spm10,rpm10)


                aeroname='oc1'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=(w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)) *oc_mfac +val2


                nullify(spm10,rpm10)

                aeroname='oc2'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=(w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)) *oc_mfac +val2


                nullify(spm10,rpm10)


                aeroname='seas1'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)+val2


                nullify(spm10,rpm10)


                aeroname='seas2'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=(w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)) +val2


                nullify(spm10,rpm10)


                aeroname='seas3'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=(w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)) +val2


                nullify(spm10,rpm10)


                aeroname='dust1'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)+val2


                nullify(spm10,rpm10)


                aeroname='dust2'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=(w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8))  +val2


                nullify(spm10,rpm10)

                aeroname='dust3'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=(w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8))  +val2


                nullify(spm10,rpm10)


                aeroname='dust4'

                call gsi_bundlegetpointer(sval,trim(aeroname),spm10,istatus)
                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(456)
                endif

                call gsi_bundlegetpointer(rval,trim(aeroname),rpm10,istatus)

                if(istatus /= 0) then
                   write(6,*) 'error gsi_bundlegetpointer in stppm10 for ',&
                        aeroname
                   call stop2(457)
                endif


                val= w1* rpm10(j1)+w2* rpm10(j2)+w3* rpm10(j3)+w4* rpm10(j4)+ &
                     w5* rpm10(j5)+w6* rpm10(j6)+w7* rpm10(j7)+w8* rpm10(j8)+val
                val2=(w1* spm10(j1)+w2* spm10(j2)+w3* spm10(j3)+w4* spm10(j4)+ &
                     w5* spm10(j5)+w6* spm10(j6)+w7* spm10(j7)+w8* spm10(j8)) * d_10 +val2


                nullify(spm10,rpm10)


                do kk=1,nstep
                   qq=val2+sges(kk)*val
                   pen(kk)=qq*qq*pm10ptr%err2
                end do
             else
                pen(1)=pm10ptr%res*pm10ptr%res*pm10ptr%err2
             end if

!  modify penalty term if nonlinear qc

             if (nlnqc_iter .and. pm10ptr%pg > tiny_r_kind .and. &
                  pm10ptr%b  > tiny_r_kind) then
                pm10_pg=pm10ptr%pg*varqc_iter
                cg_pm10=cg_term/pm10ptr%b
                wnotgross= one-pm10_pg
                wgross = pm10_pg*cg_pm10/wnotgross
                do kk=1,max(1,nstep)
                   pen(kk)= -two*log((exp(-half*pen(kk))+wgross)/(one+wgross))
                end do
             endif
             
             out(1) = out(1)+pen(1)*pm10ptr%raterr2
             do kk=2,nstep
                out(kk) = out(kk)+(pen(kk)-pen(1))*pm10ptr%raterr2
             end do
          end if
          
          !pm10ptr => pm10ptr%llpoint
          pm10ptr => pm10Node_nextcast(pm10ptr)

       end do

    endif
       
    return
  end subroutine stppm10
  
end module stppm10mod
