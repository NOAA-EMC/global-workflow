      SUBROUTINE MSTADB(Q2,T2,P2,Q1,T1,P1)                              
C                                                                       
C  THIS ROUTINE PROVIDES T2 AND QSAT AT T2 AT PRESSUE P2 THAT           
C  GIVES THE SAME EQUIVALENT POTENTIAL TEMPERATURE AS THE POINT         
C  ( T1, P1). FOR EASE OF COMPUTATION, Q1 IS REQUESTED                  
C                                                                       
      REAL L0, KAPPA                                                    
      parameter (dtdp=4.5e-4,kappa=.286,g=9.81)
      parameter (cp=1004.6,cl=4185.5,cpv=1846.0)
      parameter (rv=461.5,l0=2.500e6,t0=273.16,es0=610.78)
      parameter (cps=2106.0,hfus=3.3358e5,rd=287.05)
      parameter (fact1=(CPV - CL) / RV,fact1i=(cps-cl)/rv)
      parameter (fact2=(L0 + (CL - CPV) * T0) / RV)
      parameter (fact2i=(L0 + hfus + (CL - cps) * T0) / RV)
      parameter (fact3=1. / T0,eps=rd/rv,tmix=t0-20.)
      FUNC(QS,T) = EXP(L0 * QS / (CP * T))                              
      DESDT(ES,T) = ES * (FACT1 / T + FACT2 / T ** 2)                   
      DESDTi(ES,T) = ES * (FACT1i / T + FACT2i / T ** 2)                   
C  FIRST GUESS OF T2                                                    
      T2 = T1 + DTDP * (P2 - P1)                                        
      PFACT = (1.E5 / P2) ** KAPPA                                      
      CONST = T1 * (1.E5 / P1) ** KAPPA * FUNC(Q1,T1)                   
      ITER = 0                                                          
C  ITERATION STARTS                                                     
 10   CALL SVP(Q2,E2,P2,T2)                                             
      FACT4 = FUNC(Q2,T2)                                               
      F = T2 * PFACT * FACT4 - CONST                                    
      if(t2.ge.t0) then
        desdt2 = desdt(e2,t2)
      elseif(t2.lt.tmix) then
        desdt2 = desdti(e2,t2)
      else
        w = (t2 - tmix) / (t0 - tmix)
        desdt2 = w * desdt(e2,t2) + (1.-w) * desdti(e2,t2)
      endif
      DQSDT = (Q2 / E2) * (P2 / (P2 - (1.-EPS) * E2)) * DESDT2
      DFDT = PFACT * FACT4 + PFACT * FACT4 * (L0 * DQSDT / CP           
     &       - L0 * Q2 / (CP * T2))                                     
      DT = - F / DFDT                                                   
      T2 = T2 + DT                                                      
      IF(ABS(DT).LT..1) GOTO 100                                        
      ITER = ITER + 1                                                   
      IF(ITER.LT.50) GOTO 10                                            
      WRITE(6,*) ' MSTADB ITERATION DIVERGED, PROGRAM STOPPED'          
      STOP 'ABEND 240'                                                  
 100  CONTINUE                                                          
      CALL SVP(Q2,E2,P2,T2)                                             
      RETURN                                                            
      END                                                               
