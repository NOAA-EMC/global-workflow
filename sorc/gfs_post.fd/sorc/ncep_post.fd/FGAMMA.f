      REAL FUNCTION fGAMMA(X)
!D    DOUBLE PRECISION FUNCTION fGAMMA(X)                                     
!----------------------------------------------------------------------      
!                                                                           
! This routine calculates the GAMMA function for a real argument X.        
!   Computation is based on an algorithm outlined in reference 1.         
!   The program uses rational functions that approximate the GAMMA       
!   function to at least 20 significant decimal digits.  Coefficients   
!   for the approximation over the interval (1,2) are unpublished.     
!   Those for the approximation for X .GE. 12 are from reference 2.   
!   The accuracy achieved depends on the arithmetic system, the      
!   compiler, the intrinsic functions, and proper selection of the  
!   machine-dependent constants.                                   
!                                                                 
!                                                                            
!*******************************************************************        
!*******************************************************************       
!                                                                         
! Explanation of machine-dependent constants                             
!                                                                       
! beta   - radix for the floating-point representation                 
! maxexp - the smallest positive power of beta that overflows         
! XBIG   - the largest argument for which GAMMA(X) is representable  
!          in the machine, i.e., the solution to the equation       
!                  GAMMA(XBIG) = beta**maxexp                      
! XINF   - the largest machine representable floating-point number;
!          approximately beta**maxexp                            
! EPS    - the smallest positive floating-point number such that              
!          1.0+EPS .GT. 1.0                                                  
! XMININ - the smallest positive floating-point number such that            
!          1/XMININ is machine representable                               
!                                                                         
!     Approximate values for some important machines are:                
!                                                                       
!                            beta       maxexp        XBIG             
!                                                                     
! CRAY-1         (S.P.)        2         8191        966.961         
! Cyber 180/855                                                     
!   under NOS    (S.P.)        2         1070        177.803       
! IEEE (IBM/XT,                                                   
!   SUN, etc.)   (S.P.)        2          128        35.040      
! IEEE (IBM/XT,                                                 
!   SUN, etc.)   (D.P.)        2         1024        171.624   
! IBM 3033       (D.P.)       16           63        57.574   
! VAX D-Format   (D.P.)        2          127        34.844  
! VAX G-Format   (D.P.)        2         1023        171.489
!                                                          
!                            XINF         EPS        XMININ                   
!                                                                            
! CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466               
! Cyber 180/855                                                            
!   under NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294              
! IEEE (IBM/XT,                                                          
!   SUN, etc.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38             
! IEEE (IBM/XT,                                                        
!   SUN, etc.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308          
! IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76          
! VAX D-Format   (D.P.)   1.70D+38     1.39D-17    5.88D-39         
! VAX G-Format   (D.P.)   8.98D+307    1.11D-16    1.12D-308       
!                                                                 
!*******************************************************************         
!*******************************************************************        
!                                                                          
! Error returns                                                           
!                                                                        
!  The program returns the value XINF for singularities or              
!     when overflow would occur.  The computation is believed          
!     to be free of underflow and overflow.                           
!                                                                    
!                                                                   
!  Intrinsic functions required are:                               
!                                                                 
!     INT, DBLE, EXP, LOG, REAL, SIN                             
!                                                               
!                                                              
! References: "An Overview of Software Development for Special                
!              Functions", W. J. Cody, Lecture Notes in Mathematics,         
!              506, Numerical Analysis Dundee, 1975, G. A. Watson           
!              (ed.), Springer Verlag, Berlin, 1976.                       
!                                                                         
!              Computer Approximations, Hart, Et. Al., Wiley and         
!              sons, New York, 1968.                                    
!                                                                      
!  Latest modification: October 12, 1989                              
!                                                                    
!  Authors: W. J. Cody and L. Stoltz                                
!           Applied Mathematics Division                           
!           Argonne National Laboratory                           
!           Argonne, IL 60439                                    
!                                                               
!----------------------------------------------------------------------        
      implicit none

      INTEGER I,N
      LOGICAL PARITY
      REAL C,CONV,EPS,FACT,HALF,ONE,P,PI,Q,RES,SQRTPI,SUM,TWELVE,       &
          TWO,X,XBIG,XDEN,XINF,XMININ,XNUM,Y,Y1,YSQ,Z,ZERO
      DIMENSION C(7),P(8),Q(8)
!----------------------------------------------------------------------      
!  Mathematical constants                                                   
!----------------------------------------------------------------------    
      DATA ONE,HALF,TWELVE,TWO,ZERO/1.0E0,0.5E0,12.0E0,2.0E0,0.0E0/,    &
           SQRTPI/0.9189385332046727417803297E0/,                       &
           PI/3.1415926535897932384626434E0/
!D    DATA ONE,HALF,TWELVE,TWO,ZERO/1.0D0,0.5D0,12.0D0,2.0D0,0.0D0/,          
!D   1     SQRTPI/0.9189385332046727417803297D0/,                            
!D   2     PI/3.1415926535897932384626434D0/                                
!----------------------------------------------------------------------    
!  Machine dependent parameters                                           
!----------------------------------------------------------------------  
      DATA XBIG,XMININ,EPS/35.040E0,1.18E-38,1.19E-7/,                  &
           XINF/3.4E38/
!D    DATA XBIG,XMININ,EPS/171.624D0,2.23D-308,2.22D-16/,                     
!D   1     XINF/1.79D308/                                                    
!----------------------------------------------------------------------     
!  Numerator and denominator coefficients for rational minimax             
!     approximation over (1,2).                                           
!----------------------------------------------------------------------  
      DATA P/-1.71618513886549492533811E+0,2.47656508055759199108314E+1,&
             -3.79804256470945635097577E+2,6.29331155312818442661052E+2,&
             8.66966202790413211295064E+2,-3.14512729688483675254357E+4,&
             -3.61444134186911729807069E+4,6.64561438202405440627855E+4/
      DATA Q/-3.08402300119738975254353E+1,3.15350626979604161529144E+2,&
            -1.01515636749021914166146E+3,-3.10777167157231109440444E+3,&
              2.25381184209801510330112E+4,4.75584627752788110767815E+3,&
            -1.34659959864969306392456E+5,-1.15132259675553483497211E+5/
!D    DATA P/-1.71618513886549492533811D+0,2.47656508055759199108314D+1,     
!D   1       -3.79804256470945635097577D+2,6.29331155312818442661052D+2,    
!D   2       8.66966202790413211295064D+2,-3.14512729688483675254357D+4,   
!D   3       -3.61444134186911729807069D+4,6.64561438202405440627855D+4/  
!D    DATA Q/-3.08402300119738975254353D+1,3.15350626979604161529144D+2, 
!D   1      -1.01515636749021914166146D+3,-3.10777167157231109440444D+3,      
!D   2        2.25381184209801510330112D+4,4.75584627752788110767815D+3,     
!D   3      -1.34659959864969306392456D+5,-1.15132259675553483497211D+5/    
!----------------------------------------------------------------------    
!  Coefficients for minimax approximation over (12, INF).                 
!----------------------------------------------------------------------  
      DATA C/-1.910444077728E-03,8.4171387781295E-04,                   &
           -5.952379913043012E-04,7.93650793500350248E-04,              &
           -2.777777777777681622553E-03,8.333333333333333331554247E-02, &
            5.7083835261E-03/
!D    DATA C/-1.910444077728D-03,8.4171387781295D-04,                         
!D   1     -5.952379913043012D-04,7.93650793500350248D-04,                   
!D   2     -2.777777777777681622553D-03,8.333333333333333331554247D-02,     
!D   3      5.7083835261D-03/                                              
!----------------------------------------------------------------------   
!  Statement functions for conversion between integer and float          
!---------------------------------------------------------------------- 
      CONV(I) = REAL(I)
!D    CONV(I) = DBLE(I)                                                      
      PARITY = .FALSE.
      FACT = ONE
      N = 0
      Y = X
      IF (Y .LE. ZERO) THEN
!----------------------------------------------------------------------     
!  Argument is negative                                                    
!----------------------------------------------------------------------   
            Y = -X
            Y1 = AINT(Y)
            RES = Y - Y1
            IF (RES .NE. ZERO) THEN
                  IF (Y1 .NE. AINT(Y1*HALF)*TWO) PARITY = .TRUE.
                  FACT = -PI / SIN(PI*RES)
                  Y = Y + ONE
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
      END IF
!----------------------------------------------------------------------  
!  Argument is positive                                                 
!----------------------------------------------------------------------
      IF (Y .LT. EPS) THEN
!----------------------------------------------------------------------       
!  Argument .LT. EPS                                                         
!----------------------------------------------------------------------     
            IF (Y .GE. XMININ) THEN
                  RES = ONE / Y
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
         ELSE IF (Y .LT. TWELVE) THEN
            Y1 = Y
            IF (Y .LT. ONE) THEN
!----------------------------------------------------------------------    
!  0.0 .LT. argument .LT. 1.0                                             
!----------------------------------------------------------------------  
                  Z = Y
                  Y = Y + ONE
               ELSE
!---------------------------------------------------------------------- 
!  1.0 .LT. argument .LT. 12.0, reduce argument if necessary           
!----------------------------------------------------------------------       
                  N = INT(Y) - 1
                  Y = Y - CONV(N)
                  Z = Y - ONE
            END IF
!----------------------------------------------------------------------      
!  Evaluate approximation for 1.0 .LT. argument .LT. 2.0                    
!----------------------------------------------------------------------    
            XNUM = ZERO
            XDEN = ONE
            DO 260 I = 1, 8
               XNUM = (XNUM + P(I)) * Z
               XDEN = XDEN * Z + Q(I)
  260       CONTINUE
            RES = XNUM / XDEN + ONE
            IF (Y1 .LT. Y) THEN
!----------------------------------------------------------------------   
!  Adjust result for case  0.0 .LT. argument .LT. 1.0                    
!---------------------------------------------------------------------- 
                  RES = RES / Y1
               ELSE IF (Y1 .GT. Y) THEN
!----------------------------------------------------------------------       
!  Adjust result for case  2.0 .LT. argument .LT. 12.0                       
!----------------------------------------------------------------------     
                  DO 290 I = 1, N
                     RES = RES * Y
                     Y = Y + ONE
  290             CONTINUE
            END IF
         ELSE
!----------------------------------------------------------------------    
!  Evaluate for argument .GE. 12.0,                                       
!----------------------------------------------------------------------  
            IF (Y .LE. XBIG) THEN
                  YSQ = Y * Y
                  SUM = C(7)
                  DO 350 I = 1, 6
                     SUM = SUM / YSQ + C(I)
  350             CONTINUE
                  SUM = SUM/Y - Y + SQRTPI
                  SUM = SUM + (Y-HALF)*LOG(Y)
                  RES = EXP(SUM)
               ELSE
                  RES = XINF
                  GO TO 900
            END IF
      END IF
!----------------------------------------------------------------------      
!  Final adjustments and return                                             
!----------------------------------------------------------------------    
      IF (PARITY) RES = -RES
      IF (FACT .NE. ONE) RES = FACT / RES
  900 fGAMMA = RES                                                       
      RETURN
! ---------- Last line of fGAMMA ----------                              
      END

