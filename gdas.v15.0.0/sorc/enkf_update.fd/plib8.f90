MODULE MODULE_pmat1
!$$$ module documentation block
!              .      .    .                                       .
! module:  module_pmat1
!
! abstract:  Routines for basic algebraic operations on general matrices 
!             and vectors
!
! additional notes:
!  These routines, perform basic algebraic operations on real vectors and
!  matrices. The task performed by each routine is, as far as possible,
!  encoded in each routine's name; three letters describe the
!  operation, the remainder defining the type of operand and, if needed to
!  resolve an ambiguity, the type of result.
!
! program history log:
!   2011-07-04 todling - set to double precision to allow running GSI in
!                        in either single or double precision
!
! remarks:
!   1. routines here must work under REAL*8 (double precision)
!
!  OPERATIONS:
!   DET     evaluate log-determinant
!   DIF     differentiate
!   INT     integrate
!   INV     invert the matrix, or linear system involving the matrix operand
!   L1L     Cholesky LU decomposition, where U is just L-transpose
!   L1U     L-U decomposition of first arg, with 1's along diagonal of L and U
!   LDL     Cholesky LDU decomposition, where U is just L-transpose and D diag.
!   LDU     LDU decomposition
!   NOR     evaluate norm of operand
!   POL     polynomial (first argument) of second argument
!   POW     raise operand to some integer power
!   SWP     swap first two operands
!   TRC     evaluate trace of operand
!   U1L     back substitution with matrix decomposed into LU form, 1's on diag.
!   UDL     back substitution with matrix decomposed into LDU form
!   WRT     write out
!   ZER     set operand to zero
!
!  OPERAND TYPES:
!   B	    banded matrix
!   C	    circulant matrix
!   D	    diagonal matrix
!   H	    symmetric or hermitian matrix
!   L	    lower triangular matrix
!   M	    matrix (rectangular, in general)
!   P	    polynomial or power-series coefficient vector
!   Q	    sQuare matrix with Fortran dimension same as logical dimension
!   R	    row of a matrix
!   S	    scalar
!   T	    transpose of the matrix
!   U	    upper triangular matrix
!   V	    vector, or column of a matrix
!   X	    field of parallel X-vectors (aligned like "columns" of a matrix)
!   Y	    field of parallel Y-vectors (aligned like "rows" of a matrix)
!
! program history:
!   1994-  -    R.J.Purser - initial coding
!   2008-04-25  safford    - add standard documentation blocks
!
! subroutines included:
!   pro333
!   dpro333
!   cro33
!   dcro33
!   norv
!   dnorv
!   norq
!   dnorq
!   swpvv
!   dswpvv
!   mulmd
!   dmulmd
!   multd
!   dmultd
!   muldm
!   dmuldm
!   muldt
!   dmuldt
!   mulpp
!   dmulpp
!   madpp
!   dmadpp
!   msbpp
!   dmsbpp
!   difp
!   ddifp
!   intp
!   dintp
!   invp
!   dinvp
!   prgv
!   dprgv
!   mulcc
!   dmulcc
!   madcc
!   dmadcc
!   msbcc
!   dmsbcc
!   zerl
!   dzerl
!   zeru
!   dzeru
!   ldum
!   dldum
!   udlmm, udlmv
!   dudlmm,dudlmv
!   linvan
!   dlinvan
!   copdm
!   dcopdm
!   condm
!   dcondm
!   copsm
!   dcopsm
!   consm
!   dconsm
!   addmd
!   daddmd
!   submd
!   dsubmd
!   addms
!   daddms
!   subms
!   dsubms
!   l1lm
!   dl1lm
!   ldlm
!   dldlm
!   invh
!   dinvh
!   invl
!   dinvl
!   linlv
!   dlinlv
!   linuv
!   dlinuv
!   powp
!   dpowp
!   polps
!   dpolps
!   polpp
!   dpolpp
!   trcm
!   dtrcm
!   invmt, linmmt, linmvt
!   dinvmt,dlinmmt,dlinmvt
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: i_kind
use kinds, only: r_kind => r_double
use constants,only: zero,one
IMPLICIT NONE

! set default to private
  private
! set subroutines/interfaces to public
  public :: pro333
  public :: pro333_d
  public :: cro33
  public :: cro33_d
  public :: norv
  public :: norv_d
  public :: norq
  public :: norq_d
  public :: swpvv
  public :: swpvv_d
  public :: mulmd
  public :: mulmd_d
  public :: multd
  public :: multd_d
  public :: muldm
  public :: muldm_d
  public :: muldt
  public :: muldt_d
  public :: mulpp
  public :: mulpp_d
  public :: madpp
  public :: madpp_d
  public :: msbpp
  public :: msbpp_d
  public :: difp
  public :: difp_d
  public :: intp
  public :: intp_d
  public :: invp
  public :: invp_d
  public :: prgv
  public :: prgv_d
  public :: mulcc
  public :: mulcc_d
  public :: madcc
  public :: madcc_d
  public :: msbcc
  public :: msbcc_d
  public :: zerl
  public :: zerl_d
  public :: zeru
  public :: zeru_d
  public :: ldum
  public :: ldum_d
  public :: udlmm
  public :: udlmm_d
  public :: linvan
  public :: linvan_d
  public :: copdm
  public :: copdm_d
  public :: condm
  public :: condm_d
  public :: copsm
  public :: copsm_d
  public :: consm
  public :: consm_d
  public :: addmd
  public :: addmd_d
  public :: submd
  public :: submd_d
  public :: addms
  public :: addms_d
  public :: subms
  public :: subms_d
  public :: l1lm
  public :: l1lm_d
  public :: ldlm
  public :: ldlm_d
  public :: invh
  public :: invh_d
  public :: invl
  public :: invl_d
  public :: linlv
  public :: linlv_d
  public :: linuv
  public :: linuv_d
  public :: powp
  public :: powp_d
  public :: polps
  public :: polps_d
  public :: polpp
  public :: polpp_d
  public :: trcm
  public :: trcm_d
  public :: inv
  public :: inv_d

INTERFACE pro333  ; MODULE PROCEDURE pro333;                 END INTERFACE
INTERFACE pro333_d; MODULE PROCEDURE dpro333;                END INTERFACE
INTERFACE cro33   ; MODULE PROCEDURE cro33;                  END INTERFACE
INTERFACE cro33_d;  MODULE PROCEDURE dcro33;                 END INTERFACE
INTERFACE norv;     MODULE PROCEDURE norv;                   END INTERFACE
INTERFACE norv_d;   MODULE PROCEDURE dnorv;                  END INTERFACE
INTERFACE norq;     MODULE PROCEDURE norq;                   END INTERFACE
INTERFACE norq_d;   MODULE PROCEDURE dnorq;                  END INTERFACE
INTERFACE swpvv;    MODULE PROCEDURE swpvv;                  END INTERFACE
INTERFACE swpvv_d;  MODULE PROCEDURE dswpvv;                 END INTERFACE
INTERFACE mulmd;    MODULE PROCEDURE mulmd;                  END INTERFACE
INTERFACE mulmd_d;  MODULE PROCEDURE dmulmd;                 END INTERFACE
INTERFACE multd;    MODULE PROCEDURE multd;                  END INTERFACE
INTERFACE multd_d;  MODULE PROCEDURE dmultd;                 END INTERFACE
INTERFACE muldm;    MODULE PROCEDURE muldm;                  END INTERFACE
INTERFACE muldm_d;  MODULE PROCEDURE dmuldm;                 END INTERFACE
INTERFACE muldt;    MODULE PROCEDURE muldt;                  END INTERFACE
INTERFACE muldt_d;  MODULE PROCEDURE dmuldt;                 END INTERFACE
INTERFACE mulpp;    MODULE PROCEDURE mulpp;                  END INTERFACE
INTERFACE mulpp_d;  MODULE PROCEDURE dmulpp;                 END INTERFACE
INTERFACE madpp;    MODULE PROCEDURE madpp;                  END INTERFACE
INTERFACE madpp_d;  MODULE PROCEDURE dmadpp;                 END INTERFACE
INTERFACE msbpp;    MODULE PROCEDURE msbpp;                  END INTERFACE
INTERFACE msbpp_d;  MODULE PROCEDURE dmsbpp;                 END INTERFACE
INTERFACE difp;     MODULE PROCEDURE difp;                   END INTERFACE
INTERFACE difp_d;   MODULE PROCEDURE ddifp;                  END INTERFACE
INTERFACE intp;     MODULE PROCEDURE intp;                   END INTERFACE
INTERFACE intp_d;   MODULE PROCEDURE dintp;                  END INTERFACE
INTERFACE invp;     MODULE PROCEDURE invp;                   END INTERFACE
INTERFACE invp_d;   MODULE PROCEDURE dinvp;                  END INTERFACE
INTERFACE prgv;     MODULE PROCEDURE prgv;                   END INTERFACE
INTERFACE prgv_d;   MODULE PROCEDURE dprgv;                  END INTERFACE
INTERFACE mulcc;    MODULE PROCEDURE mulcc;                  END INTERFACE
INTERFACE mulcc_d;  MODULE PROCEDURE dmulcc;                 END INTERFACE
INTERFACE madcc;    MODULE PROCEDURE madcc;                  END INTERFACE
INTERFACE madcc_d;  MODULE PROCEDURE dmadcc;                 END INTERFACE
INTERFACE msbcc;    MODULE PROCEDURE msbcc;                  END INTERFACE
INTERFACE msbcc_d;  MODULE PROCEDURE dmsbcc;                 END INTERFACE
INTERFACE zerl;     MODULE PROCEDURE zerl;                   END INTERFACE
INTERFACE zerl_d;   MODULE PROCEDURE dzerl;                  END INTERFACE
INTERFACE zeru;     MODULE PROCEDURE zeru;                   END INTERFACE
INTERFACE zeru_d;   MODULE PROCEDURE dzeru;                  END INTERFACE
INTERFACE ldum;     MODULE PROCEDURE ldum;                   END INTERFACE
INTERFACE ldum_d;   MODULE PROCEDURE dldum;                  END INTERFACE
INTERFACE udlmm;    MODULE PROCEDURE udlmm, udlmv;           END INTERFACE
INTERFACE udlmm_d;  MODULE PROCEDURE dudlmm,dudlmv;          END INTERFACE
INTERFACE linvan;   MODULE PROCEDURE linvan;                 END INTERFACE
INTERFACE linvan_d; MODULE PROCEDURE dlinvan;                END INTERFACE
INTERFACE copdm;    MODULE PROCEDURE copdm;                  END INTERFACE
INTERFACE copdm_d;  MODULE PROCEDURE dcopdm;                 END INTERFACE
INTERFACE condm;    MODULE PROCEDURE condm;                  END INTERFACE
INTERFACE condm_d;  MODULE PROCEDURE dcondm;                 END INTERFACE
INTERFACE copsm;    MODULE PROCEDURE copsm;                  END INTERFACE
INTERFACE copsm_d;  MODULE PROCEDURE dcopsm;                 END INTERFACE
INTERFACE consm;    MODULE PROCEDURE consm;                  END INTERFACE
INTERFACE consm_d;  MODULE PROCEDURE dconsm;                 END INTERFACE
INTERFACE addmd;    MODULE PROCEDURE addmd;                  END INTERFACE
INTERFACE addmd_d;  MODULE PROCEDURE daddmd;                 END INTERFACE
INTERFACE submd;    MODULE PROCEDURE submd;                  END INTERFACE
INTERFACE submd_d;  MODULE PROCEDURE dsubmd;                 END INTERFACE
INTERFACE addms;    MODULE PROCEDURE addms;                  END INTERFACE
INTERFACE addms_d;  MODULE PROCEDURE daddms;                 END INTERFACE
INTERFACE subms;    MODULE PROCEDURE subms;                  END INTERFACE
INTERFACE subms_d;  MODULE PROCEDURE dsubms;                 END INTERFACE
INTERFACE l1lm;     MODULE PROCEDURE l1lm;                   END INTERFACE
INTERFACE l1lm_d;   MODULE PROCEDURE dl1lm;                  END INTERFACE
INTERFACE ldlm;     MODULE PROCEDURE ldlm;                   END INTERFACE
INTERFACE ldlm_d;   MODULE PROCEDURE dldlm;                  END INTERFACE
INTERFACE invh;     MODULE PROCEDURE invh;                   END INTERFACE
INTERFACE invh_d;   MODULE PROCEDURE dinvh;                  END INTERFACE
INTERFACE invl;     MODULE PROCEDURE invl;                   END INTERFACE
INTERFACE invl_d;   MODULE PROCEDURE dinvl;                  END INTERFACE
INTERFACE linlv;    MODULE PROCEDURE linlv;                  END INTERFACE
INTERFACE linlv_d;  MODULE PROCEDURE dlinlv;                 END INTERFACE
INTERFACE linuv;    MODULE PROCEDURE linuv;                  END INTERFACE
INTERFACE linuv_d;  MODULE PROCEDURE dlinuv;                 END INTERFACE
INTERFACE powp;     MODULE PROCEDURE powp;                   END INTERFACE
INTERFACE powp_d;   MODULE PROCEDURE dpowp;                  END INTERFACE
INTERFACE polps;    MODULE PROCEDURE polps;                  END INTERFACE
INTERFACE polps_d;  MODULE PROCEDURE dpolps;                 END INTERFACE
INTERFACE polpp;    MODULE PROCEDURE polpp;                  END INTERFACE
INTERFACE polpp_d;  MODULE PROCEDURE dpolpp;                 END INTERFACE
INTERFACE trcm;     MODULE PROCEDURE trcm;                   END INTERFACE
INTERFACE trcm_d;   MODULE PROCEDURE dtrcm;                  END INTERFACE
INTERFACE inv;      MODULE PROCEDURE invmt, linmmt, linmvt;  END INTERFACE
INTERFACE inv_d;    MODULE PROCEDURE dinvmt,dlinmmt,dlinmvt; END INTERFACE

CONTAINS


FUNCTION pro333(d,e,f) RESULT(pro_res)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    pro333
!
!   prgrmmr:
!
! abstract:  triple product of 3 3-vectors
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d(3), e(3), f(3) - input vectors
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(IN   ) :: d(3), e(3), f(3)

REAL(r_kind)                :: pro_res
REAL(r_kind)                :: g(3)

CALL CRO33(E,F,G)
pro_res=DOT_PRODUCT(d,g)
END FUNCTION pro333


FUNCTION dpro333(d,e,f) RESULT(pro_res)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dpro333
!
!   prgrmmr:
!
! abstract:  triple product of 3 3-vectors
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d(3), e(3), f(3) - input vectors
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: d(3), e(3), f(3)

REAL(r_kind)             :: pro_res
REAL(r_kind)             :: g(3)
CALL CRO33_d(E,F,G)
pro_res=DOT_PRODUCT(d,g)
END FUNCTION dpro333


SUBROUTINE cro33(a,b,c) 
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    cro33
!
!   prgrmmr:
!
! abstract:  special case of 3-dimensions:  cross-product
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a(3), b(3)       - input vectors
!
!   output argument list:
!     c(3)             - resulting cross-product
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block$
implicit none

REAL(r_kind),    INTENT(IN   ) :: a(3), b(3)
REAL(r_kind),    INTENT(  OUT) :: c(3)

c(1)=a(2)*b(3)-a(3)*b(2)
c(2)=a(3)*b(1)-a(1)*b(3)
c(3)=a(1)*b(2)-a(2)*b(1)
END SUBROUTINE cro33


SUBROUTINE dcro33(a,b,c)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dcro33
!
!   prgrmmr:
!
! abstract:  special case of 3-dimensions:  cross-product
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a(3), b(3)       - input vectors
!
!   output argument list:
!     c(3)             - resulting cross-product
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(3), b(3)
REAL(r_kind), INTENT(  OUT) :: c(3)

c(1)=a(2)*b(3)-a(3)*b(2)
c(2)=a(3)*b(1)-a(1)*b(3)
c(3)=a(1)*b(2)-a(2)*b(1)
END SUBROUTINE dcro33


FUNCTION norv(d) RESULT(norv_res)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    norv
!
!   prgrmmr:
!
! abstract:  norm of vector
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d                - input vector
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(IN   ) :: d(:)

REAL(r_kind)                :: norv_res

norv_res=SQRT(DOT_PRODUCT(D,D))
END FUNCTION norv


FUNCTION dnorv(d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dnorv
!
!   prgrmmr:
!
! abstract:  norm of vector
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d                - input vector
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(IN   ) :: d(:)

REAL(r_kind):: dnorv

dnorv=SQRT(DOT_PRODUCT(d,d))
END FUNCTION dnorv


FUNCTION norq(d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    norq
!
!   prgrmmr:
!
! abstract:  norm of a matrix
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d                - input matrix
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN   ) :: d(:,:)

REAL(r_kind):: norq
INTEGER(i_kind) m2,i2

m2=SIZE(d,2)
norq=zero; DO i2=1,m2; norq=norq+dot_PRODUCT(d(:,i2),d(:,i2)); ENDDO
norq=SQRT(norq)
END FUNCTION norq


FUNCTION dnorq(d) ! norm of a matrix
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dnorq
!
!   prgrmmr:
!
! abstract:  norm of a matrix
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d                - input matrix
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN   ) :: d(:,:)

REAL(r_kind):: dnorq
INTEGER(i_kind) m2,i2

m2=SIZE(d,2)
dnorq=zero; DO i2=1,m2; dnorq=dnorq+dot_PRODUCT(d(:,i2),d(:,i2)); ENDDO
dnorq=SQRT(dnorq)
END FUNCTION dnorq


SUBROUTINE swpvv(d,e)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    swpvv 
!
!   prgrmmr:
!
! abstract:  swap first two operands of input vectors
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d, e       - 
!
!   output argument list:
!     d, e       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: d(:), e(:)

REAL(r_kind) :: t(SIZE(d))

t = d; d = e; e = t
END SUBROUTINE swpvv


SUBROUTINE dswpvv(d,e)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dswpvv
!
!   prgrmmr:
!
! abstract:  swap first two operads of input vectors
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d, e       - 
!
!   output argument list:
!     d, e       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: d(:), e(:)

REAL(r_kind) :: t(SIZE(d))

t = d; d = e; e = t
END SUBROUTINE dswpvv


SUBROUTINE mulmd(a,d,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulmd
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       - 
!     d          -
!   output argument list:
!     a, b       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m2,j

m2=SIZE(a,2)
DO j=1,m2; b(:,j)=a(:,j)*d(j); ENDDO
END SUBROUTINE mulmd


SUBROUTINE dmulmd(a,d,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmulmd
!
!   prgrmmr:
!
! abstract:  special case of 3-dimensions:  cross-product
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       - 
!     d          - 
!
!   output argument list:
!     a, b       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:)
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m2,j

m2=SIZE(a,2)
DO j=1,m2; b(:,j)=a(:,j)*d(j); ENDDO
END SUBROUTINE dmulmd


SUBROUTINE multd(a,d,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    multd
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       - 
!     d          - 
!
!   output argument list:
!     a, b       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m2,j

m2=SIZE(a,1)
DO j=1,m2; b(:,j) = a(j,:) * d(j); ENDDO
END SUBROUTINE multd


SUBROUTINE dmultd(a,d,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmultd
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m2,j

m2=SIZE(a,1)
DO j=1,m2; b(:,j) = a(j,:) * d(j); ENDDO
END SUBROUTINE dmultd


SUBROUTINE muldm(d,a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    muldm
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind)                :: m1,i

m1=SIZE(a,1)
DO i=1,m1; b(i,:) = d(i)*a(i,:); ENDDO
END SUBROUTINE muldm


SUBROUTINE dmuldm(d,a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmuldm
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind)             :: m1,i

m1=SIZE(a,1)
DO i=1,m1; b(i,:) = d(i)*a(i,:); ENDDO
END SUBROUTINE dmuldm


SUBROUTINE muldt(d,a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    muldt
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind)                :: m1,i

m1=SIZE(a,2)
DO i=1,m1; b(i,:) = d(i)*a(:,i); ENDDO
END SUBROUTINE muldt


SUBROUTINE dmuldt(d,a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmuldt
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          -
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

REAL(r_kind), INTENT(INOUT) :: a(:,:),b(:,:) 
REAL(r_kind), INTENT(IN   ) :: d(*)

INTEGER(i_kind):: m1,i

m1=SIZE(a,2)
DO i=1,m1; b(i,:) = d(i)*a(:,i); ENDDO
END SUBROUTINE dmuldt


SUBROUTINE mulpp(a,b,c)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulpp
!
!   prgrmmr:
!
! abstract:  multiply polynomials, possibly in place
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a, b       -
!     c          -
!
!   output argument list:
!     c          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(IN   ) :: a(0:), b(0:)
REAL(r_kind),    INTENT(INOUT) :: c(0:)

INTEGER(i_kind)                :: m,mcp, j
REAL(r_kind)                   :: s

m=SIZE(a)-1
mcp=mcmax(a,b,m)
c(mcp:m) = zero
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=s
ENDDO
RETURN
ENTRY madpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=c(j-1)+s
ENDDO
RETURN
ENTRY msbpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=c(j-1)-s
ENDDO
RETURN
CONTAINS
FUNCTION mcmax(a,b,m) RESULT(mmx_res) ! This fn can be contained in mulpp().
!$$$  subprogram documentation block
!                .      .    .                                        .
! subprogram:    mcmax
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-26  lueken - added subprogram doc block
!
!   input argument list:
!    m
!    a,b
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m
REAL(r_kind),    INTENT(IN   ) :: a(0:m), b(0:m)

INTEGER(i_kind)             :: mmx_res
INTEGER(i_kind)             :: ma, mb

mmx_res=0                     ! default for when ALL elements of c are zero
DO ma=m,0,-1                      ! seek last nonzero coefficient of polynomial a
   IF(a(ma) /= zero)THEN
      DO mb=m,0,-1                  ! seek last nonzero coefficient of polynomial b
         IF(b(mb) /= zero)THEN
            mmx_res=MIN(m,ma+mb)+1 ! hence, 1+last non-0 element of their product
            RETURN
         ENDIF
      ENDDO
      RETURN
   ENDIF
ENDDO
END FUNCTION mcmax
END SUBROUTINE mulpp


SUBROUTINE difp(a,b) ! Symbolically differentiate polynomial
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    difp
!
!   prgrmmr:
!
! abstract:  Symbolically differentiate polynomial
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a          -
!
!   output argument list:
!     b          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(0:)
REAL(r_kind), INTENT(  OUT) :: b(0:)

INTEGER(i_kind)           :: m, i
REAL(r_kind)              :: s, b0

m=SIZE(a)-1
DO i=1,m        ! possibly with coincident storage for a and b
   b(i-1)=i*a(i)
ENDDO
b(m)=zero
RETURN
ENTRY intp(a,b) ! Symbolically integrate polynomial
m=SIZE(a)-1
DO i=m,1,-1     ! possibly with coincident storage for a and b
   b(i)=a(i-1)/i
ENDDO
b(0)=zero
RETURN
ENTRY invp(a,b) ! Invert polynomial or power-series
m=SIZE(a)-1
b0=one/a(0)     ! storage of a and b must not be the same
b(0)=b0
DO i=1,m
   s = SUM(b(i-1:0:-1)*a(1:i))
   b(i)=-b0*s
ENDDO
END SUBROUTINE difp


SUBROUTINE dmulpp(a,b,c) !  multiply polynomials, possibly in place
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmulpp
!
!   prgrmmr:
!
! abstract:  multiply polynomials, possibly in place
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a, b       -
!     c          -
!
!   output argument list:
!     c          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(0:), b(0:)
REAL(r_kind), INTENT(INOUT) :: c(0:)

INTEGER(i_kind)            :: m,mcp, j
REAL(r_kind)               :: s

m=SIZE(a)-1
mcp=mcmax(a,b,m)
c(mcp:m) = zero
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=s
ENDDO
RETURN
ENTRY dmadpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=c(j-1)+s
ENDDO
RETURN
ENTRY dmsbpp(a,b,c)
m=SIZE(a)-1
mcp=mcmax(a,b,m)
DO j=mcp,1,-1
   s = SUM(a(j-1:0:-1)*b(0:j-1))
   c(j-1)=c(j-1)-s
ENDDO
RETURN
CONTAINS

FUNCTION mcmax(a,b,m) RESULT(mmx_res)
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    mcmax
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-26  lueken - added subprogram doc block
!
!   input argument list:
!    m
!    a,b
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m
REAL(r_kind)   ,  INTENT(IN   ) :: a(0:m), b(0:m)

INTEGER(i_kind)              :: mmx_res
INTEGER(i_kind)              :: ma, mb

mmx_res=0                     ! default for when all elements of c are zero
DO ma=m,0,-1                      ! seek last nonzero coefficient of polynomial a
   IF(a(ma) /= zero)THEN
      DO mb=m,0,-1                  ! seek last nonzero coefficient of polynomial b
         IF(b(mb) /= zero)THEN
            mmx_res=MIN(m,ma+mb)+1 ! hence, 1+last non-0 element of their product
            RETURN
         ENDIF
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN
END FUNCTION mcmax

END SUBROUTINE dmulpp


SUBROUTINE ddifp(a,b) ! Symbolically differentiate polynomial
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ddifp
!
!   prgrmmr:
!
! abstract:  Symbolically differentiate polynomial
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a          -
!     b          -
!
!   output argument list:
!     b          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(0:)
REAL(r_kind), INTENT(INOUT) :: b(0:)

INTEGER(i_kind)            :: m, i
REAL(r_kind)               :: s, b0

m=SIZE(a)-1
DO i=1,m         ! possibly with coincident storage for a and b
   b(i-1)=i*a(i)
ENDDO
b(m)=zero
RETURN
ENTRY dintp(a,b) ! Symbolically integrate polynomial
m=SIZE(a)-1
DO i=m,1,-1      ! possibly with coincident storage for a and b
   b(i)=a(i-1)/i
ENDDO
b(0)=zero
RETURN
ENTRY dinvp(a,b) ! Invert polynomial or power-series
m=SIZE(a)-1
b0=one/a(0)      ! storage of a and b must not be the same
b(0)=b0
DO i=1,m
   s = SUM(b(i-1:0:-1)*a(1:i))
   b(i)=-b0*s
ENDDO
END SUBROUTINE ddifp


SUBROUTINE prgv(d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    prgv
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d          -
!
!   output argument list:
!     d          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: d(:)

REAL(r_kind), PARAMETER        :: crit=1.E-30_r_kind
INTEGER(i_kind)                :: i,m

m=SIZE(d)
DO i=1,m; IF(ABS(d(i)) <= crit)d(i)=zero; ENDDO
END SUBROUTINE prgv


SUBROUTINE dprgv(d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dprgv
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d          -
!
!   output argument list:
!     d          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: d(:)

REAL(r_kind), PARAMETER     :: crit=1.E-30_r_kind
INTEGER(i_kind)             :: i,m

m=SIZE(d)
DO i=1,m; IF(ABS(d(i)) <= crit)d(i)=zero; ENDDO
END SUBROUTINE dprgv


SUBROUTINE mulcc(a,b,c,m)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulcc
!
!   prgrmmr:
!
! abstract:  Multiply circulant matrices of period M
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b, c    -
!     m          -
!
!   output argument list:
!     a, b, c    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m
REAL(r_kind)   , INTENT(INOUT) :: a(0:m-1), b(0:m-1), c(0:m-1)

INTEGER(i_kind)                :: mm, j

c(0:m-1) = zero
ENTRY madcc(a,b,c,m)
mm=m-1
DO j=0,mm
   c(j:m-1) = c(j:m-1) + a(0:m-j-1)*b(j)
   c(0:j-1) = c(0:j-1) + a(m-j:m-1)*b(j)
ENDDO
RETURN
ENTRY msbcc(a,b,c,m)
mm=m-1
DO j=0,mm
   c(j:m-1) = c(j:m-1) - a(0:m-j-1)*b(j)
   c(0:j-1) = c(0:j-1) - a(m-j:m-1)*b(j)
ENDDO
END SUBROUTINE mulcc


SUBROUTINE dmulcc(a,b,c,m)  ! Multiply circulant matrices of period M
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmulcc
!
!   prgrmmr:
!
! abstract:  Multiply circulant matrices of period M
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b, c    -
!     m          -
!
!   output argument list:
!     a, b, c    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m
REAL(r_kind)   , INTENT(INOUT) :: a(0:m-1), b(0:m-1), c(0:m-1)

INTEGER(i_kind)                :: mm, j

c(0:m-1) = zero
ENTRY dmadcc(a,b,c,m)
mm=m-1
DO j=0,mm
   c(j:m-1) = c(j:m-1) + a(0:m-j-1)*b(j)
   c(0:j-1) = c(0:j-1) + a(m-j:m-1)*b(j)
ENDDO
RETURN
ENTRY dmsbcc(a,b,c,m)
mm=m-1
DO j=0,mm
   c(j:m-1) = c(j:m-1) - a(0:m-j-1)*b(j)
   c(0:j-1) = c(0:j-1) - a(m-j:m-1)*b(j)
ENDDO
END SUBROUTINE dmulcc


SUBROUTINE zerl(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    zerl
!
!   prgrmmr:
!
! abstract:  Zero out the strictly lower triangle of elements
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(INOUT) :: a(:,:)

INTEGER(i_kind)           :: m,j

m=SIZE(a,1); DO j=1,m; a(j+1:m,j) = zero; ENDDO; RETURN

ENTRY zeru(a)       ! Zero out the strictly upper triangle of elements
m=SIZE(a,1); DO j=1,m; a(1:j-1,j) = zero; ENDDO
END SUBROUTINE zerl


SUBROUTINE dzerl(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dmuldm
!
!   prgrmmr:
!
! abstract:  Zero out the strictly lower triangle of elements
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(INOUT) :: a(:,:)

INTEGER(i_kind)           :: m,j

m=SIZE(a,1); DO j=1,m; a(j+1:m,j) = zero; ENDDO; RETURN

ENTRY dzeru(a)      ! Zero out the strictly upper triangle of elements
m=SIZE(a,1); DO j=1,m; a(1:j-1,j) = zero; ENDDO
END SUBROUTINE dzerl


SUBROUTINE ldum(a,ipiv,d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldum
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.	1996
!
! abstract:  perform l-d-u decomposition of square matrix a in place with
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - square matrix to be factorized
!
!   output argument list:
!     a          - square matrix to be factorized
!     ipiv       - ipiv array encoding the pivoting sequence
!     d          - indicator for possible sign change of determinant
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),    INTENT(INOUT) :: a(:,:) 
REAL(r_kind),    INTENT(OUT  ) :: d
INTEGER(i_kind), INTENT(OUT  ) :: ipiv(:)

INTEGER(i_kind)                :: m,i, j, jp, ibig, jm
REAL(r_kind)                   :: s(SIZE(a,1)),  aam, aa, abig,  ajj, ajji, aij

m=SIZE(a,1)
DO i=1,m
   aam=zero
   DO j=1,m
      aa=ABS(a(i,j))
      IF(aa > aam)aam=aa
   ENDDO
   IF(aam == zero)THEN
      PRINT '(" row ",i3," of matrix in ldum vanishes")',i
      STOP
   ENDIF
   s(i)=one/aam
ENDDO
d=one
ipiv(m)=m
DO j=1,m-1
   jp=j+1
   abig=s(j)*ABS(a(j,j))
   ibig=j
   DO i=jp,m
      aa=s(i)*ABS(a(i,j))
      IF(aa > abig)THEN
         ibig=i
         abig=aa
      ENDIF
   ENDDO
!  swap rows, recording changed sign of determinant
   ipiv(j)=ibig
   IF(ibig /= j)THEN
      d=-d
      CALL swpvv(a(j,:),a(ibig,:))
      s(ibig)=s(j)
   ENDIF
   ajj=a(j,j)
   IF(ajj == zero)THEN
      jm=j-1
      PRINT '(" failure in ldum:"/" matrix singular, rank=",i3)',jm
      STOP
   ENDIF
   ajji=one/ajj
   DO i=jp,m
      aij=ajji*a(i,j)
      a(i,j)=aij
      a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
   ENDDO
ENDDO
END SUBROUTINE ldum


SUBROUTINE DLDUM(A,IPIV,D)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dldum
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1996
!
! abstract:  perform l-d-u decomposition of square matrix a in place with
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - square matrix to be factorized
!
!   output argument list:
!     a          - square matrix to be factorized
!     ipiv       - ipiv array encoding the pivoting sequence
!     d          - indicator for possible sign change of determinant
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind)   , INTENT(INOUT) :: a(:,:) 
REAL(r_kind)   , INTENT(  OUT) :: d
INTEGER(i_kind), INTENT(  OUT) :: ipiv(:)

INTEGER(i_kind)                :: m,i, j, jp, ibig, jm
REAL(r_kind)                   :: s(SIZE(a,1)),  aam, aa, abig,  ajj, ajji, aij

m=SIZE(a,1)
DO i=1,m
   aam=zero
   DO j=1,m
      aa=ABS(a(i,j))
      IF(aa > aam)aam=aa
   ENDDO
   IF(aam == zero)THEN
      PRINT '(" row ",i3," of matrix in dldum vanishes")',i
      STOP
   ENDIF
   s(i)=one/aam
ENDDO
d=one
ipiv(m)=m
DO j=1,m-1
   jp=j+1
   abig=s(j)*ABS(a(j,j))
   ibig=j
   DO i=jp,m
      aa=s(i)*ABS(a(i,j))
      IF(aa > abig)THEN
         ibig=i
         abig=aa
      ENDIF
   ENDDO
!  swap rows, recording changed sign of determinant
   ipiv(j)=ibig
   IF(ibig /= j)THEN
      d=-d
      CALL swpvv_d(a(j,:),a(ibig,:))
      s(ibig)=s(j)
   ENDIF
   ajj=a(j,j)
   IF(ajj == zero)THEN
      jm=j-1
      PRINT '(" Failure in dldum:"/" matrix singular, rank=",i3)',jm
      STOP
   ENDIF
   ajji=one/ajj
   DO i=jp,m
      aij=ajji*a(i,j)
      a(i,j)=aij
      a(i,jp:m) = a(i,jp:m) - aij*a(j,jp:m)
   ENDDO
ENDDO
END SUBROUTINE dldum


SUBROUTINE udlmm(a,b,ipiv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlmm
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  use l-u factors in A to back-substitute for mm rhs in B, 
!            using ipiv to define the pivoting permutation used in the l-u 
!            decomposition.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - L-D-U factorization of linear system matrux
!     b          - right-hand-sides on entry, corresponding matrix of solution
!	           vectors on return
!     ipiv       - ipiv array encoding the pivoting sequence
!
!   output argument list:
!     b          - right-hand-sides on entry, corresponding matrix of solution
!	           vectors on return
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: ipiv(:) 
REAL(r_kind),    INTENT(IN   ) :: a(:,:) 
REAL(r_kind),    INTENT(INOUT) :: b(:,:) 

INTEGER(i_kind)                :: m,mm,i, k, l
REAL(r_kind)                   :: s,aiii

m=SIZE(a,1); mm=SIZE(b,2)
DO k=1,mm !loop over columns of b
   DO i=1,m
      l=ipiv(i)
      s=b(l,k)
      b(l,k)=b(i,k)
      s = s - SUM(b(1:i-1,k)*a(i,1:i-1))
      b(i,k)=s
   ENDDO
   b(m,k)=b(m,k)/a(m,m)
   DO i=m-1,1,-1
      aiii=one/a(i,i)
      b(i,k) = b(i,k) - SUM(b(i+1:m,k)*a(i,i+1:m))
      b(i,k)=b(i,k)*aiii
   ENDDO
ENDDO
END SUBROUTINE udlmm


SUBROUTINE dudlmm(a,b,ipiv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dudlmm
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  use l-u factors in A to back-substitute for mm rhs in B,
!            using ipiv to define the pivoting permutation used in the l-u
!            decomposition.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - L-D-U factorization of linear system matrux
!     b          - right-hand-sides on entry, corresponding matrix of solution
!                  vectors on return
!     ipiv       - ipiv array encoding the pivoting sequence
!
!   output argument list:
!     b          - right-hand-sides on entry, corresponding matrix of solution
!                  vectors on return
!     
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: ipiv(:) 
REAL(r_kind)   , INTENT(IN   ) :: a(:,:) 
REAL(r_kind)   , INTENT(INOUT) :: b(:,:) 

INTEGER(i_kind)                :: m,mm,i, k, l
REAL(r_kind)                   :: s,aiii

m=SIZE(a,1); mm=SIZE(b,2)
DO k=1,mm !loop over columns of b
   DO i=1,m
      l=ipiv(i)
      s=b(l,k)
      b(l,k)=b(i,k)
      s = s - SUM(b(1:i-1,k)*a(i,1:i-1))
      b(i,k)=s
   ENDDO
   b(m,k)=b(m,k)/a(m,m)
   DO i=m-1,1,-1
      aiii=one/a(i,i)
      b(i,k) = b(i,k) - SUM(b(i+1:m,k)*a(i,i+1:m))
      b(i,k)=b(i,k)*aiii
   ENDDO
ENDDO
END SUBROUTINE dudlmm


SUBROUTINE udlmv(a,b,ipiv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlmv
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  use l-u factors in A to back-substitute for mm rhs in B, using 
!            ipiv to define the pivoting permutation used in the l-u 
!            decomposition.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - L-D-U factorization of linear system matrux
!     b          - right-hand-side on entry, corresponding vector solution
!                  on return
!     ipiv       - ipiv array encoding the pivoting sequence
!
!   output argument list:
!     b          - right-hand-side on entry, corresponding vector solution
!                  on return
!     
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: ipiv(:) 
REAL(r_kind),    INTENT(IN   ) :: a(:,:) 
REAL(r_kind),    INTENT(INOUT) :: b(:) 

INTEGER(i_kind)                :: m,i, l
REAL(r_kind)                   :: s,aiii

m=SIZE(a,1)
DO i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - SUM(b(1:i-1)*a(i,1:i-1))
   b(i)=s
ENDDO
b(m)=b(m)/a(m,m)
DO i=m-1,1,-1
   aiii=one/a(i,i)
   b(i) = b(i) - SUM(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
ENDDO
END SUBROUTINE udlmv


SUBROUTINE dudlmv(a,b,ipiv)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dudlmv
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  use l-u factors in A to back-substitute for mm rhs in B, using
!            ipiv to define the pivoting permutation used in the l-u
!            decomposition.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - L-D-U factorization of linear system matrux
!     b          - right-hand-side on entry, corresponding vector solution
!                  on return
!     ipiv       - ipiv array encoding the pivoting sequence
!
!   output argument list:
!     b          - right-hand-side on entry, corresponding vector solution
!                  on return
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: ipiv(:) 
REAL(r_kind)   ,  INTENT(IN   ) :: a(:,:) 
REAL(r_kind)   ,  INTENT(INOUT) :: b(:) 

INTEGER(i_kind)                 :: m,i, l
REAL(r_kind)                    :: s,aiii

m=SIZE(a,1)
DO i=1,m
   l=ipiv(i)
   s=b(l)
   b(l)=b(i)
   s = s - SUM(b(1:i-1)*a(i,1:i-1))
   b(i)=s
ENDDO
b(m)=b(m)/a(m,m)
DO i=m-1,1,-1
   aiii=one/a(i,i)
   b(i) = b(i) - SUM(b(i+1:m)*a(i,i+1:m))
   b(i)=b(i)*aiii
ENDDO
END SUBROUTINE dudlmv


SUBROUTINE linvan(w,ab)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linvan
!
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1993
!
! abstract:  
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     W       - Generalized Vandermonde matrix in, conditioning indicator out.
!     AB      - R.h.s. vector in, solution vector of numerical coefficients out.
!
!   output argument list:
!     W       - Generalized Vandermonde matrix in, conditioning indicator out.
!     AB      - R.h.s. vector in, solution vector of numerical coefficients out.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: w(:,:), ab(:)

INTEGER(i_kind), PARAMETER  :: nit=20
REAL(r_kind)                :: d1(SIZE(w,1)), d2(SIZE(w,1)), &
                               w2(SIZE(w,1),SIZE(w,1)),v(SIZE(w,1))
INTEGER(i_kind)             :: i, j, it, jt, ipiv(SIZE(w,1)), nc
REAL(r_kind)                :: p, e, dw, c, d, d2j
REAL(r_kind),ALLOCATABLE    :: wv(:,:) ! work variable for ab(nc) and v(nn)

nc = SIZE(w,DIM=1)
ALLOCATE(wv(nc,1))

w2=w                ! Preserve original W and AB for use
v = ab(1:nc)        ! in later "clean-up" operation.

d1 = one            ! Row scaling factors set to default
d2 = one            ! Column scaling factors set to default

C=1.E-16_r_kind	    ! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
DO i=1,nc
   p=zero
   e=zero
   DO j=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
         e=e+one
         p=p+LOG(dw)
      ENDIF
   ENDDO
   IF(E == zero)STOP 'W effectively singular in LINVAN'
   d1(i)=EXP(-p/e)
ENDDO
CALL muldm(d1,w2,w)

DO j=1,nc
   p=zero
   e=zero
   DO i=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
         e=e+one
         p=p+LOG(dw)
      ENDIF
   ENDDO
   IF(E == zero)STOP 'W effectively singular in LINVAN'
   d2(j)=EXP(-p/e)
ENDDO
CALL mulmd(w,d2,w)

c=1.e-8_r_kind  ! reset the criterion for "negligible" elements

! revert to iterations of the more efficient method without logarithms:
DO jt=1,2
   DO it=1,nit    !	perform nit relaxation iterations
      DO i=1,nc    !	do rows:
         p=one
         e=zero
         DO j=1,nc
            dw=ABS(w(i,j))
            IF(dw > c)THEN
               e=e+one
               p=p*dw
            ENDIF
         ENDDO
         p=one/(p**(one/e))
         w(i,:) = w(i,:) * p            ! rescale this row of w..
         d1(i)=d1(i)*p     ! ..and update d1 consistently
      ENDDO
      DO j=1,nc    !	do columns:
         p=one
         e=zero
         d2j=d2(j)
         DO i=1,nc
            dw=ABS(w(i,j))
            IF(dw > c)THEN
               e=e+one
               p=p*dw
            ENDIF
         ENDDO
         p=one/(p**(one/e))
         w(:,j) = w(:,j) * p        ! rescale this column of w..
         d2(j)=d2(j)*p       ! ..and update d2 consistently
      ENDDO
   ENDDO
   c=1.e-3_r_kind    ! final setting for criterion for "negligible" elements
ENDDO
ab(1:nc) = d1(1:nc) * ab(1:nc) ! rescale r.h.s vector by d1
p=one     ! p becomes product of row-lengths:
DO i=1,nc
   p=p*SQRT(dot_PRODUCT(w(i,:),w(i,:)))
ENDDO
CALL ldum(w,ipiv,d)
DO i=1,nc
   d=d*w(i,i)      ! d becomes the determinant of w
ENDDO
wv(:,1) = ab ! convert shape of array
CALL udlmm(w,wv(:,1:1),ipiv)
ab = d2 * wv(:,1) ! rescale solution vector by d2
!     ab(1:nc) = d2(1:nc) * ab(1:nc) ! rescale solution vector by d2
!  note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of w. we invoke original matrix elements w2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  begin "clean-up" process. substitute solution vector in original
!  equation and leave the residual difference in v
v=v-MATMUL(w2,ab)
v = d1 * v    ! rescale the residual vector by d1
wv(:,1) = v ! convert shape of array
CALL udlmm(w,wv(:,1:1),ipiv) ! solve linear system with this rhs.
ab=ab+wv(:,1)*d2 ! add residual solution vector, 
                                      ! scaled, to ab

DEALLOCATE(wv)
w(1,1)=d/p  ! this ratio is an indicator of the overall conditioning
            ! when d/p is very small, treat the results with suspicion!

END SUBROUTINE linvan


SUBROUTINE dlinvan(w,ab)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dlinvan
!                .      .    .
!   prgrmmr:     R.J.Purser, NCEP, Washington D.C.      1996
!
! abstract: 
!   Take square matrix W and seek row and column scalings to produce non-
!   vanishing elements of rescaled W having magnitudes as close to unity
!   as possible. The approach is make the geometric mean of the nonvanishing
!   elements of each row and of each column +1 or -1. Having rescaled the
!   matrix and the r.h.s. vector AB, compute the product P of row-vector
!   norms, then compute the determinant D and solve the linear system.
!   Rescale the solution vector (now AB) and put the conditioning indicator
!   formed by the ratio D/P into the first element of W.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     W   -  Generalized Vandermonde matrix in, conditioning indicator out.
!     AB  -  R.h.s. vector in, solution vector of numerical coefficients out.
!
!   output argument list:
!     W   -  Generalized Vandermonde matrix in, conditioning indicator out.
!     AB  -  R.h.s. vector in, solution vector of numerical coefficients out.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: w(:,:), ab(:)

INTEGER(i_kind), PARAMETER     :: nit=20
REAL(r_kind)                   :: d1(SIZE(w,1)), d2(SIZE(w,1)), &
                                  w2(SIZE(w,1),SIZE(w,1)),v(SIZE(w,1))
INTEGER(i_kind)                :: i, j, it, jt, ipiv(SIZE(w,1)), nc
REAL(r_kind)                   :: p, e, dw, c, d, d2j
REAL(r_kind),ALLOCATABLE       :: wv(:,:) ! work variable for ab(nc) and v(nn)

nc = SIZE(w,DIM=1)
ALLOCATE(wv(nc,1))

w2=w                ! Preserve original W and AB for use
v = ab(1:nc)        ! in later "clean-up" operation.

d1 = one            ! Row scaling factors set to default
d2 = one            ! Column scaling factors set to default

C=1.E-16_r_kind     ! Set initial criterion for "negligible" elements of W

! In first attempt to estimate row and column scalings, use logarithms
! to avoid the risk of under- or over-flows of the line products of W:
DO i=1,nc
   p=zero
   e=zero
   DO j=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
         e=e+one
         p=p+LOG(dw)
      ENDIF
   ENDDO
   IF(e == zero)STOP 'w effectively singular in linvan'
   d1(i)=EXP(-p/e)
ENDDO
CALL muldm_d(d1,w2,w)

DO j=1,nc
   p=zero
   e=zero
   DO i=1,nc
      dw=ABS(w(i,j))
      IF(dw > c)THEN
         e=e+one
         p=p+LOG(dw)
      ENDIF
   ENDDO
   IF(e == zero)STOP 'w effectively singular in linvan'
   d2(j)=EXP(-p/e)
ENDDO
CALL mulmd_d(w,d2,w)
 
c=1.e-8_r_kind  ! reset the criterion for "negligible" elements

! revert to iterations of the more efficient method without logarithms:
DO jt=1,2
   DO it=1,nit    !	perform nit relaxation iterations
      DO i=1,nc    !	do rows:
         p=one
         e=zero
         DO j=1,nc
            dw=ABS(w(i,j))
            IF(dw > c)THEN
               e=e+one
               p=p*dw
            ENDIF
         ENDDO
         p=one/(p**(one/e))
         w(i,:) = w(i,:) * p            ! rescale this row of w..
         d1(i)=d1(i)*p     ! ..and update d1 consistently
     ENDDO
    DO j=1,nc    ! do columns:
      p=one
      e=zero
      d2j=d2(j)
      DO i=1,nc
        dw=ABS(w(i,j))
        IF(dw > c)THEN
          e=e+one
          p=p*dw
        ENDIF
      ENDDO
      p=one/(p**(one/e))
      w(:,j) = w(:,j) * p        ! rescale this column of w..
      d2(j)=d2(j)*p       ! ..and update d2 consistently
    ENDDO
  ENDDO
  c=1.e-3_r_kind    ! final setting for criterion for "negligible" elements
ENDDO
ab(1:nc) = d1(1:nc) * ab(1:nc) ! rescale r.h.s vector by d1
p=one     ! p becomes product of row-lengths:
DO i=1,nc
   p=p*SQRT(dot_PRODUCT(w(i,:),w(i,:)))
ENDDO
CALL ldum_d(w,ipiv,d)
DO i=1,nc
  d=d*w(i,i)      ! d becomes the determinant of w
ENDDO
wv(:,1) = ab ! convert shape of array
CALL udlmm_d(w,wv(:,1:1),ipiv)
ab = d2 * wv(:,1) ! rescale solution vector by d2
!     ab(1:nc) = d2(1:nc) * ab(1:nc) ! Rescale solution vector by D2
!  Note: it is very likely that round-off errors have accumulated during
!  the iterative rescaling of W. We invoke original matrix elements W2 and
!  substitute the tentative solution vector into the original (unscaled)
!  equation in order to estimate the residual components of roundoff error.

!  Begin "clean-up" process. Substitute solution vector in original
!  equation and leave the residual difference in V
v=v-MATMUL(w2,ab)
v = d1 * v    ! Rescale the residual vector by D1
wv(:,1) = v ! Convert shape of array
CALL UDLMM_d(w,wv(:,1:1),ipiv) ! Solve linear system with THIS rhs.
ab=ab+wv(:,1)*d2 ! Add residual solution vector, 
                                      ! scaled, to AB

DEALLOCATE(wv)
w(1,1)=d/p  ! this ratio is an indicator of the overall conditioning
            ! When D/P is very small, treat the results with suspicion!

END SUBROUTINE dlinvan


SUBROUTINE copdm(d,a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copdm
!
!   prgrmmr:     
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:),INTENT(IN)::d; REAL(r_kind),DIMENSION(:,:),INTENT(OUT)::a; INTEGER(i_kind) i
                  a=zero; DO i=1,SIZE(a,1); a(i,i)= d(i); ENDDO; RETURN
ENTRY condm(d,a); a=zero; DO i=1,SIZE(a,1); a(i,i)=-d(i); ENDDO
END SUBROUTINE copdm


SUBROUTINE dcopdm(d,a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dcopdm
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:),INTENT(IN)::d; REAL(r_kind),DIMENSION(:,:),INTENT(OUT)::a
INTEGER(i_kind) i
                   a=zero; DO i=1,SIZE(a,1); a(i,i)= d(i); ENDDO; RETURN
ENTRY dcondm(d,a); a=zero; DO i=1,SIZE(a,1); a(i,i)=-d(i); ENDDO
END SUBROUTINE dcopdm


SUBROUTINE copsm(s,a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copsm
!
!   prgrmmr:    
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     s          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN) :: s; REAL(r_kind),DIMENSION(:,:),INTENT(OUT):: a; INTEGER(i_kind) i
                  a=zero; DO i=1,SIZE(a,1); a(i,i)= s; ENDDO; RETURN
ENTRY consm(s,a); a=zero; DO i=1,SIZE(a,1); a(i,i)=-s; ENDDO
END SUBROUTINE copsm


SUBROUTINE dcopsm(s,a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dcopsm
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     s          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN) :: s; REAL(r_kind),DIMENSION(:,:),INTENT(OUT):: a; INTEGER(i_kind) i
                   a=zero; DO i=1,SIZE(a,1); a(i,i)= s; ENDDO; RETURN
ENTRY dconsm(s,a); a=zero; DO i=1,SIZE(a,1); a(i,i)=-s; ENDDO
END SUBROUTINE dcopsm


SUBROUTINE addmd(a,b,d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    addmd
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       -
!     d          - 
!
!   output argument list:
!     a, b       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT):: a,b; REAL(r_kind),DIMENSION(:),INTENT(IN):: d
REAL(r_kind) s;  INTEGER(i_kind) i
                   b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+d(i); ENDDO; RETURN
ENTRY submd(a,b,d);b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-d(i); ENDDO; RETURN
ENTRY addms(a,b,s);b=a; DO I=1,SIZE(a,1); b(i,i)=b(i,i)+s;    ENDDO; RETURN
ENTRY SUBMS(A,B,S);b=a; DO I=1,SIZE(a,1); B(I,I)=B(I,I)-S;    ENDDO;
END SUBROUTINE addmd


SUBROUTINE daddmd(a,b,d)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    daddmd
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a, b       - 
!     d          -
!
!   output argument list:
!     a, b       - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT)::A,B;REAL(r_kind),DIMENSION(:),INTENT(IN)::D
REAL(r_kind) s; INTEGER(i_kind) i
                     b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+d(i); ENDDO; RETURN
ENTRY DSUBMD(A,B,D); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-d(i); ENDDO; RETURN
ENTRY DADDMS(A,B,S); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)+s;    ENDDO; RETURN
ENTRY DSUBMS(A,B,S); b=a; DO i=1,SIZE(a,1); b(i,i)=b(i,i)-s;    ENDDO;
END SUBROUTINE daddmd


SUBROUTINE l1lm(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    l1lm
!
!   prgrmmr:    
!
! abstract:  Cholesky, M -> L*U, U(i,j)=L(j,i)
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - 
!     b          - 
!
!   output argument list:
!     b          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)
REAL(r_kind), INTENT(INOUT) :: b(:,:)
INTEGER(i_kind)             :: m,j, jm, jp, i
REAL(r_kind)                :: s, bjji
m=SIZE(a,1)
DO j=1,m
  jm=j-1
  jp=j+1
  s = a(j,j) - SUM(b(j,1:jm)*b(j,1:jm))
  IF(S <= zero)THEN
    PRINT '(" L1LM detects non-positivity at diagonal index",i2)',J
    STOP
  ENDIF
  b(j,j)=SQRT(s)
  bjji=one/b(j,j)
  DO i=jp,m
    s = a(i,j) - SUM(b(i,1:jm)*b(j,1:jm))
    b(i,j)=s*bjji
  ENDDO
  b(1:jm,j) = zero
ENDDO
END SUBROUTINE l1lm


SUBROUTINE DL1LM(A,B)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dl1lm
!
!   prgrmmr:    
!
! abstract:  Cholesky, M -> L*U, U(i,j)=L(j,i)
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
! 
!   input argument list:
!     a          -
!     b          -
!
!   output argument list:
!     b          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:) 
REAL(r_kind), INTENT(INOUT) :: b(:,:) 

INTEGER(i_kind) :: m,j, jm, jp, i
REAL(r_kind) :: s, bjji

m=SIZE(a,1)
DO j=1,m
   jm=j-1
   jp=j+1
   s = a(j,j) - SUM(b(j,1:jm)*b(j,1:jm))
   IF(s <= zero)THEN
      PRINT '(" L1LM detects non-positivity at diagonal index",i2)',J
      STOP
   ENDIF
   b(j,j)=SQRT(s)
   bjji=one/b(j,j)
   DO i=jp,m
      s = a(i,j) - SUM(b(i,1:jm)*b(j,1:jm))
      b(i,j)=s*bjji
   ENDDO
   b(1:jm,j) = zero
ENDDO
RETURN
END SUBROUTINE dl1lm

SUBROUTINE ldlm(a,b,d) ! Modified Cholesky decompose Q --> L*D*U, U(i,j)=L(j,i)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlm
!
!   prgrmmr:    
!
! abstract:  Modified Cholesky decompose Q --> L*D*U, U(i,j)=L(j,i)
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
! 
!   input argument list:
!     a          -
!     b          -
!
!   output argument list:
!     b          -
!     d          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)
REAL(r_kind), INTENT(INOUT) :: b(:,:)
REAL(r_kind), INTENT(  OUT) :: d(:)

INTEGER(i_kind) :: m,j, jm, jp, i
REAL(r_kind) :: bjji

m=SIZE(a,1)
DO j=1,m
   jm=j-1
   jp=j+1
   d(j)=a(j,j) - SUM(b(1:jm,j)*b(j,1:jm))
  
   b(j,j) = one
   IF(d(j) == zero)THEN
      PRINT '(" LDLM detects singularity at diagonal index",i2)',J
      STOP
   ENDIF
   bjji=one/d(j)
   DO i=jp,m
      b(j,i)= a(i,j) - dot_PRODUCT(b(1:jm,j),b(i,1:jm))
      b(i,j)=b(j,i)*bjji
   ENDDO
ENDDO
CALL zeru(b)
RETURN
END SUBROUTINE ldlm


SUBROUTINE dldlm(a,b,d) ! Modified Cholesky  Q --> L*D*U, U(i,j)=L(j,i)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dldlm
!
!   prgrmmr:    
!
! abstract:  Modified Cholesky  Q --> L*D*U, U(i,j)=L(j,i)
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
! 
!   input argument list:
!     a          -
!     b          -
!
!   output argument list:
!     b          -
!     d          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)
REAL(r_kind), INTENT(INOUT) :: b(:,:)
REAL(r_kind), INTENT(  OUT) :: d(:)

INTEGER(i_kind)             :: m,j, jm, jp, i
REAL(r_kind)                :: bjji

m=SIZE(a,1)
DO j=1,m; jm=j-1; jp=j+1
   d(j)=a(j,j) - SUM(b(1:jm,j)*b(j,1:jm))
   b(j,j) = one
   IF(d(j) == zero)THEN
      PRINT '(" DLDLM detects singularity at diagonal index",i2)',J
      STOP
   ENDIF
   bjji=one/d(j)
   DO i=jp,m
      b(j,i)= a(i,j) - dot_PRODUCT(b(1:jm,j),b(i,1:jm))
      b(i,j)=b(j,i)*bjji
   ENDDO
ENDDO
CALL zeru_d(b)
RETURN
END SUBROUTINE dldlm


SUBROUTINE invh(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    invh
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1993
!
! abstract:   Inver,t in place, a symmetric matrix
!
! limitation:  This routine incorporates no pivoting - it is intended for matrices
!              that are already diagonally dominant
!  
! program history log:
!   2008-04-25  safford -- add subprogram doc block
! 
!   input argument list:
!     A          - symmetric square matrix, output as inverse of input
!
!   output argument list:
!     A          - symmetric square matrix, output as inverse of input
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:) 

INTEGER(i_kind)                  :: m,k, kp, i, ip, j
REAL(r_kind),DIMENSION(SIZE(a,1)):: d

m=SIZE(a,1)
!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
CALL ldlm(a,a,d)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
DO k=1,m; kp=k+1
   a(k,k)=one/d(k)
   DO i=kp,m
      a(i,k) = a(i,k) + SUM(a(kp:i-1,k)*a(i,kp:i-1)) ! really??
      a(i,k) =-a(i,k)
   ENDDO
ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
DO i=2,m
   a(1:i-1,i) = a(i,1:i-1) * a(i,i) ! Really?
ENDDO
DO i=1,m
   ip=i+1
   DO j=1,i-1
      a(j,i) = a(j,i) + SUM(a(ip:ip+m-i-1,i)*a(j,ip:ip+m-i-1))
      a(i,j) = a(j,i)
   ENDDO
   a(i,i) = a(i,i) + SUM(a(ip:ip+m-i-1,i)*a(i,ip:ip+m-i-1))
ENDDO
END SUBROUTINE invh


SUBROUTINE dinvh(a)
!$$$  subprogram documentation block
!                .      .    .    
! subprogram:    dinvh 
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1993 
!
! abstract:   Inver,t in place, a symmetric matrix
!
! limitation:  This routine incorporates no pivoting - it is intended for matrices
!              that are already diagonally dominant
!  
! program history log: 
!   2008-04-25  safford -- add subprogram doc block
! 
!   input argument list:
!     A          - symmetric square matrix, output as inverse of input
!
!   output argument list:
!     A          - symmetric square matrix, output as inverse of input
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:) 

INTEGER(i_kind)                  :: m,k, kp, i, ip, j
REAL(r_kind),DIMENSION(SIZE(a,1)):: d

m=SIZE(a,1)
!  PERFORM L.D.U DECOMPOSITION OF THE SYMMETRIC MATRIX:
CALL ldlm_d(a,a,d)

!  INVERT (IN PLACE) THE LOWER TRIANGULAR PART OF A, (ASSUMING UNIT
!  DIAGONAL ELEMENTS), AND INVERT THE DIAGONAL PART OF A (ASSUMING
!  ZERO OFF-DIAGONAL ELEMENTS). PUT TRANSPOSE OF LOWER, TIMES DIAGONAL,
!  INTO UPPER PART OF A.
DO k=1,m
   kp=k+1
   a(k,k)=one/d(k)
   DO i=kp,m
      a(i,k) = a(i,k) + SUM(a(kp:i-1,k)*a(i,kp:i-1)) ! really??
      a(i,k) =-a(i,k)
   ENDDO
ENDDO

!  MULTIPLY: THE TRANSPOSE OF THE LOWER PART OF A (ASSUMING UNIT DIAGS),
!  TIMES THE DIAGONAL PART (ASSUMING ZERO OFF-DIAGS), TIMES THE LOWER
!  PART. THIS PRODUCT IS THE SYMMETRIC INVERSE OF THE ORIGINAL B.
DO i=2,m
   a(1:i-1,i) = a(i,1:i-1) * a(i,i) ! really?
ENDDO
DO i=1,m
   ip=i+1
   DO j=1,i-1
      a(j,i) = a(j,i) + SUM(a(ip:ip+m-i-1,i)*a(j,ip:ip+m-i-1))
      a(i,j) = a(j,i)
   ENDDO
   a(i,i) = a(i,i) + SUM(a(ip:ip+m-i-1,i)*a(i,ip:ip+m-i-1))
ENDDO
END SUBROUTINE dinvh


SUBROUTINE invl(a)
!$$$  subprogram documentation block
!                .      .    .    
! subprogram:    invl 
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:   Invert lower triangular matrix in place if A are same
!
! program history log: 
!   2008-04-25  safford -- add subprogram doc block
! 
!   input argument list:
!     a          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:) 

INTEGER(i_kind)             :: m,j, i
REAL(r_kind)                :: s

m=SIZE(a,1)
DO j=m,1,-1
   a(1:j-1,j) = zero
   a(j,j)=one/a(j,j)
   DO i=j+1,m
      s = SUM(a(j:i-1,j)*a(i,j:i-1))
      a(i,j)=-a(i,i)*s
   ENDDO
ENDDO
END SUBROUTINE invl


SUBROUTINE dinvl(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dinvl
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:   Invert lower triangular matrix in place if A are same
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - 
!
!   output argument list:
!     a          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(INOUT) :: a(:,:) 
INTEGER(i_kind)             :: m,j, i
REAL(r_kind)                :: s
m=SIZE(a,1)
DO j=m,1,-1
   a(1:j-1,j) = zero
   a(j,j)=one/a(j,j)
   DO i=j+1,m
      s = SUM(a(j:i-1,j)*a(i,j:i-1))
      a(i,j)=-a(i,i)*s
   ENDDO
ENDDO
END SUBROUTINE dinvl


SUBROUTINE linlv(a,u)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linvlv
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:   Solve linear system involving lower triangular (LINLV) or upper
!             triangular (LINUV) matrix. u is input as right-hand-side, output
!             as the solution vector.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - 
!     u          - 
!
!   output argument list:
!     u          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ):: a(:,:)
REAL(r_kind), INTENT(INOUT):: u(:)

INTEGER(i_kind)            :: m,i, j, jp

DO i=1,SIZE(a,1);    u(i)=(u(i) - SUM(u(1:i-1)*a(i,1:i-1)))/a(i,i); ENDDO
RETURN
ENTRY linuv(a,u); m=SIZE(a,1)
DO j=m,1,-1; jp=j+1; u(j)=(u(j) - SUM(a(jp:m,j)*u(jp:m)))  /a(j,j); ENDDO
END SUBROUTINE linlv


SUBROUTINE dlinlv(a,u)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dlinvlv
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:   Invert lower triangular matrix in place if A are same
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          - 
!     u          - 
!
!   output argument list:
!     u          - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)
REAL(r_kind), INTENT(INOUT) :: u(:)

INTEGER(i_kind) :: m,i, j, jp

DO i=1,SIZE(a,1); u(i)= (u(i) - SUM(u(1:i-1)*a(i,1:i-1)))/a(i,i); ENDDO
RETURN
ENTRY dlinuv(a,u); m=SIZE(a,1)
DO j=m,1,-1; jp=j+1; u(j) = (u(j) - SUM(a(jp:m,j)*u(jp:m)))/a(j,j); ENDDO
END SUBROUTINE dlinlv


SUBROUTINE powp(a,b,n) 
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    powp
!
!   prgrmmr: 
!
! abstract:  Raise power series A to the power 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -  power series
!     n          -  power to raise to
!
!   output argument list:
!     b          -  output power series
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: n       ! of N and output as B
REAL(r_kind),    INTENT(IN   ) :: a(0:)
REAL(r_kind),    INTENT(  OUT) :: b(0:)

REAL(r_kind),DIMENSION(0:SIZE(a)-1):: t; INTEGER(i_kind) :: k

b(0)=one; b(1:) = zero; DO k=1,n; CALL mulpp(a,b,t); b=t; ENDDO
END SUBROUTINE powp


SUBROUTINE DPOWP(A,B,N)        ! Raise power series A to the power
!$$$  subprogram documentation block
!                .      .    . 
! subprogram:    dpowp
!
!   prgrmmr:  
!
! abstract:  Raise power series A to the power
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -  power series
!     n          -  power to raise to
!
!   output argument list:
!     b          -  output power series
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: n      ! of N and output as B
REAL(r_kind)   , INTENT(IN   ) :: a(0:)
REAL(r_kind)   , INTENT(  OUT) :: b(0:)

REAL(r_kind),DIMENSION(0:SIZE(a)-1):: t; INTEGER(i_kind) :: k

B(0)=one; b(1:) = zero; DO k=1,n; CALL mulpp_d(a,b,t); b=t; ENDDO
END SUBROUTINE dpowp


SUBROUTINE polps(a,s1,s2) ! Apply series A to scalar S1 to obtain S2
!$$$  subprogram documentation block
!                .      .    . 
! subprogram:    polps
!
!   prgrmmr:  
!
! abstract:  Apply series A to scalar S1 to obtain S2
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -  
!     s1         - 
!
!   output argument list:
!     s2         -  
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN   ) :: a(0:)
REAL(r_kind),INTENT(IN   ) :: s1
REAL(r_kind),INTENT(  OUT) :: s2

INTEGER(i_kind) m,k

m=SIZE(a)-1; s2=a(m); DO k=m-1,0,-1; s2=s2*s1+a(k); ENDDO
END SUBROUTINE polps


SUBROUTINE dpolps(a,s1,s2) ! Apply series A to scalar S1 to obtain S2
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dpolps
!
!   prgrmmr:
!
! abstract:  Apply series A to scalar S1 to obtain S2
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -  
!     s1         -  
!
!   output argument list:
!     s2         -  
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(IN   ) :: a(0:)
REAL(r_kind),INTENT(IN   ) :: s1
REAL(r_kind),INTENT(  OUT) :: s2

INTEGER(i_kind) m,k

m=SIZE(a)-1; s2=a(m); DO k=m-1,0,-1; s2=s2*s1+a(k); ENDDO
END SUBROUTINE dpolps


SUBROUTINE polpp(a,b,c)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    polpp
!
!   prgrmmr:
!
! abstract:  Apply power series A to power series B and put
!            the result out as power-series C.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b,c      -  
!
!   output argument list:
!     a,b,c      -  
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(INOUT) :: a(0:),b(0:),c(0:)

REAL(r_kind),DIMENSION(0:SIZE(a)-1):: t
INTEGER(i_kind) m,k

m=SIZE(a)-1; c(0)=a(m); c(1:m) = zero
DO k=m-1,0,-1; CALL mulpp(b,c,t); c=t; c(0)=c(0)+a(k); ENDDO
END SUBROUTINE polpp


SUBROUTINE dpolpp(a,b,c)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dpolpp
!
!   prgrmmr:
!
! abstract:  Apply power series A to power series B and put
!            the result out as power-series C.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b,c      -
!
!   output argument list:
!     a,b,c      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),INTENT(INOUT) :: a(0:),b(0:),c(0:)

REAL(r_kind),DIMENSION(0:SIZE(a)-1):: t
INTEGER(i_kind) m,k

m=SIZE(a)-1
c(0)=a(m); c(1:m) = zero
DO k=m-1,0,-1; CALL mulpp_d(b,c,t); c=t; c(0)=c(0)+a(k); ENDDO
END SUBROUTINE dpolpp


FUNCTION trcm(a) RESULT(trc_res)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    trcm
!
!   prgrmmr:
!
! abstract:  Trace of square matrix A
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     a          -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)

REAL(r_kind)             :: trc_res
INTEGER(i_kind)          :: i

trc_res=zero; DO i=1,SIZE(a,1); trc_res=trc_res+a(i,i); ENDDO
END FUNCTION trcm

FUNCTION dtrcm(a) RESULT(trc_res)	    ! Trace of square matrix A
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    dtrcm
!   prgmmr:
!
! abstract:
!
! program history log:
!   2009-08-26  lueken - added subprogram doc block
!
!   input argument list:
!    a
!
!   output argument list:
!
! attributes:
!   language: f90
!   machine:
!
!$$$ end documentation block
implicit none

REAL(r_kind), INTENT(IN   ) :: a(:,:)

REAL(r_kind)             :: trc_res
INTEGER(i_kind)          :: i

trc_res=zero; DO i=1,SIZE(a,1); trc_res=trc_res+a(i,i); ENDDO
END FUNCTION dtrcm


SUBROUTINE invmt(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    invmt
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a

INTEGER(i_kind) m,i,j,jp,l
REAL(r_kind) d
INTEGER(i_kind),DIMENSION(SIZE(a,1)):: ipiv

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to invmt is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
CALL ldum(a,ipiv,d)

! Invert upper triangular portion U in place:
DO i=1,m; a(i,i)=one/a(i,i); ENDDO
DO i=1,m-1
   DO j=i+1,m; a(i,j)=-a(j,j)*DOT_PRODUCT(a(i:j-1,j),a(i,i:j-1)); ENDDO
ENDDO

! Invert lower triangular portion L in place:
DO j=1,m-1; jp=j+1
   DO i=jp,m; a(i,j)=-a(i,j)-DOT_PRODUCT(a(jp:i-1,j),a(i,jp:i-1)); ENDDO
ENDDO

!  Form the product of U**-1 and L**-1 in place
DO j=1,m-1; jp=j+1
   DO i=1,j; a(i,j)=a(i,j)+DOT_PRODUCT(a(jp:m,j),a(i,jp:m)); ENDDO
   DO i=jp,m; a(i,j)=DOT_PRODUCT(a(i:m,j),a(i,i:m));         ENDDO
ENDDO

!  Permute columns according to ipiv
DO j=m-1,1,-1; l=ipiv(j); CALL swpvv(a(:,j),a(:,l)); ENDDO
END SUBROUTINE invmt


SUBROUTINE dinvmt(a)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dinvmt
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a          -
!
!   output argument list:
!     a          -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a

INTEGER(i_kind)                          :: m,i,j,jp,l
REAL(r_kind)                             :: d
INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to dinvmt is not square'
! Perform a pivoted L-D-U decomposition on matrix a:
CALL ldum_d(a,ipiv,d)

! Invert upper triangular portion U in place:
DO i=1,m; a(i,i)=one/a(i,i); ENDDO
DO i=1,m-1
   DO j=i+1,m; a(i,j)=-a(j,j)*DOT_PRODUCT(a(i:j-1,j),a(i,i:j-1)); ENDDO
ENDDO

! Invert lower triangular portion L in place:
DO j=1,m-1; jp=j+1
   DO i=jp,m; a(i,j)=-a(i,j)-DOT_PRODUCT(a(jp:i-1,j),a(i,jp:i-1)); ENDDO
ENDDO

!  Form the product of U**-1 and L**-1 in place
DO j=1,m-1; jp=j+1
   DO i=1,j; a(i,j)=a(i,j)+DOT_PRODUCT(a(jp:m,j),a(i,jp:m)); ENDDO
   DO i=jp,m; a(i,j)=DOT_PRODUCT(a(i:m,j),a(i,i:m));         ENDDO
ENDDO

!  Permute columns according to ipiv
DO j=m-1,1,-1; l=ipiv(j); CALL swpvv_d(a(:,j),a(:,l)); ENDDO
END SUBROUTINE dinvmt


SUBROUTINE linmmt(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linmmt
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b        -
!
!   output argument list:
!     a,b        -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a,b

INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv
INTEGER(i_kind)                          :: m
REAL(r_kind)                             :: d

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmmt is not square'
IF(m /= SIZE(b,1))STOP 'matrix and vectors in linmmt have unmatched sizes'
CALL ldum(a,ipiv,d); CALL udlmm(a,b,ipiv)
END SUBROUTINE linmmt


SUBROUTINE dlinmmt(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dlinmmt
!
!   prgrmmr:
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b        -
!
!   output argument list:
!     a,b        -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a,b

INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv
INTEGER(i_kind)                          :: m 
REAL(r_kind)                             :: d

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmmt_d is not square'
IF(m /= SIZE(b,1))STOP 'matrix and vectors in linmmt_d have unmatched sizes'
CALL ldum_d(a,ipiv,d); CALL udlmm_d(a,b,ipiv)
END SUBROUTINE dlinmmt


SUBROUTINE linmvt(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linmvt
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b        -
!
!   output argument list:
!     a,b        -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a
REAL(r_kind),DIMENSION(:),  INTENT(INOUT) :: b

INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv
INTEGER(i_kind)                          :: m
REAL(r_kind)                             :: d

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmvt is not square'
IF(m /= SIZE(b))STOP 'matrix and vectors in linmvt have unmatched sizes'
CALL ldum(a,ipiv,d); CALL udlmm(a,b,ipiv)
END SUBROUTINE linmvt


SUBROUTINE dlinmvt(a,b)
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dlinmvt
!
!   prgrmmr:
!
! abstract: 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a,b        -
!
!   output argument list:
!     a,b        -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

REAL(r_kind),DIMENSION(:,:),INTENT(INOUT) :: a
REAL(r_kind),DIMENSION(:),  INTENT(INOUT) :: b

INTEGER(i_kind),DIMENSION(SIZE(a,1))     :: ipiv
INTEGER(i_kind) m; REAL(r_kind) d

m=SIZE(a,1)
IF(m /= SIZE(a,2))STOP 'matrix passed to linmvt_d is not square'
IF(m /= SIZE(b))STOP 'matrix and vectors in linmvt_d have unmatched sizes'
CALL ldum_d(a,ipiv,d); CALL udlmm_d(a,b,ipiv)
END SUBROUTINE dlinmvt

end module module_pmat1



MODULE MODULE_pmat2
!$$$   module documentation block
!                .      .    .                                     .
! module:  module_pmat2
!  prgmmr: purser
!
! abstract:
!
! program history log:
!   1994-  -    purser
!   2008-04-25  safford - add documentation block
!
! subroutines included:
!   avco
!   davco
!   dfco
!   ddfco
!   dfco2
!   ddfco2
!   clib
!   dclib
!   cad1b
!   csb1b
!   cad2b
!   csb2b
!   copbt
!   conbt
!   copmb
!   conmb
!   copbm
!   conbm
!   mulbb
!   madbb
!   msbbb
!   ldub
!   dldub
!   l1ubb
!   dl1ubb
!   l1ueb
!   dl1ueb
!   l1lb
!   ldlb
!   dldlb
!   udub
!   dudub
!   mulbv
!   madbv
!   msbbv
!   mulbx
!   madbx
!   msbbx
!   mulby
!   madby
!   msbby
!   mulvb
!   madvb
!   msbvb
!   mulxb
!   madxb
!   msbxb
!   mulyb
!   madyb
!   msbyb
!   mulbd
!   madbd
!   msbbd
!   muldb
!   maddb
!   msbdb
!   udlbv
!   udlbx
!   udlby
!   udlvb
!   udlxb
!   udlyb
!   u1lbv
!   u1lbx
!   u1lby
!   u1lvb
!   u1lxb
!   u1lyb
!   linbv
!   wrtb
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

USE MODULE_pmat1
use kinds, only: i_kind
use kinds, only: r_kind => r_double
use constants, only: zero,one,two
IMPLICIT NONE

! set default to private
  private
! set subroutines/interfaces to public
  public :: avco
  public :: avco_d
  public :: dfco
  public :: dfco_d
  public :: dfco2
  public :: dfco2_d
  public :: clib
  public :: clib_d
  public :: cad1b
  public :: csb1b
  public :: cad2b
  public :: csb2b
  public :: copbt
  public :: conbt
  public :: copmb
  public :: conmb
  public :: mulbb
  public :: madbb
  public :: msbbb
  public :: ldub
  public :: ldub_d
  public :: l1ubb
  public :: l1ubb_d
  public :: l1ueb
  public :: l1ueb_d
  public :: l1lb
  public :: ldlb
  public :: ldlb_d
  public :: udub
  public :: udub_d
  public :: mulbv
  public :: madbv
  public :: msbbv
  public :: mulbx
  public :: madbx
  public :: msbbx
  public :: mulby
  public :: madby
  public :: msbby
  public :: mulvb
  public :: madvb
  public :: msbvb
  public :: mulxb
  public :: madxb
  public :: msbxb
  public :: mulyb
  public :: madyb
  public :: msbyb
  public :: mulbd
  public :: madbd
  public :: msbbd
  public :: muldb
  public :: maddb
  public :: msbdb
  public :: udlbv
  public :: udlbx
  public :: udlby
  public :: udlvb
  public :: udlxb
  public :: udlyb
  public :: u1lbv
  public :: u1lbx
  public :: u1lby
  public :: u1lvb
  public :: u1lxb
  public :: u1lyb
  public :: linbv
  public :: wrtb

INTERFACE avco;   MODULE PROCEDURE avco;           END INTERFACE
INTERFACE avco_d; MODULE PROCEDURE davco;          END INTERFACE
INTERFACE dfco;   MODULE PROCEDURE dfco;           END INTERFACE
INTERFACE dfco_d; MODULE PROCEDURE ddfco;          END INTERFACE
INTERFACE dfco2;  MODULE PROCEDURE dfco2;          END INTERFACE
INTERFACE dfco2_d;MODULE PROCEDURE ddfco2;         END INTERFACE
INTERFACE clib;   MODULE PROCEDURE clib;           END INTERFACE
INTERFACE clib_d; MODULE PROCEDURE dclib;          END INTERFACE
INTERFACE cad1b;  MODULE PROCEDURE cad1b;          END INTERFACE
INTERFACE csb1b;  MODULE PROCEDURE csb1b;          END INTERFACE
INTERFACE cad2b;  MODULE PROCEDURE cad2b;          END INTERFACE
INTERFACE csb2b;  MODULE PROCEDURE csb2b;          END INTERFACE
INTERFACE copbt;  MODULE PROCEDURE copbt;          END INTERFACE
INTERFACE conbt;  MODULE PROCEDURE conbt;          END INTERFACE
INTERFACE copmb;  MODULE PROCEDURE copmb;          END INTERFACE
INTERFACE conmb;  MODULE PROCEDURE conmb;          END INTERFACE
INTERFACE copbm;  MODULE PROCEDURE copbm;          END INTERFACE
INTERFACE conbm;  MODULE PROCEDURE conbm;          END INTERFACE
INTERFACE mulbb;  MODULE PROCEDURE mulbb;          END INTERFACE
INTERFACE madbb;  MODULE PROCEDURE madbb;          END INTERFACE
INTERFACE msbbb;  MODULE PROCEDURE msbbb;          END INTERFACE
INTERFACE ldub;   MODULE PROCEDURE ldub;           END INTERFACE
INTERFACE ldub_d; MODULE PROCEDURE dldub;          END INTERFACE
INTERFACE l1ubb;  MODULE PROCEDURE l1ubb;          END INTERFACE
INTERFACE l1ubb_d;MODULE PROCEDURE dl1ubb;         END INTERFACE
INTERFACE l1ueb;  MODULE PROCEDURE l1ueb;          END INTERFACE
INTERFACE l1ueb_d;MODULE PROCEDURE dl1ueb;         END INTERFACE
INTERFACE l1lb;   MODULE PROCEDURE l1lb;           END INTERFACE
INTERFACE ldlb;   MODULE PROCEDURE ldlb;           END INTERFACE
INTERFACE ldlb_d; MODULE PROCEDURE dldlb;          END INTERFACE
INTERFACE udub;   MODULE PROCEDURE udub;           END INTERFACE
INTERFACE udub_d; MODULE PROCEDURE dudub;          END INTERFACE
INTERFACE mulbv;  MODULE PROCEDURE mulbv;          END INTERFACE
INTERFACE madbv;  MODULE PROCEDURE madbv;          END INTERFACE
INTERFACE msbbv;  MODULE PROCEDURE msbbv;          END INTERFACE
INTERFACE mulbx;  MODULE PROCEDURE mulbx;          END INTERFACE
INTERFACE madbx;  MODULE PROCEDURE madbx;          END INTERFACE
INTERFACE msbbx;  MODULE PROCEDURE msbbx;          END INTERFACE
INTERFACE mulby;  MODULE PROCEDURE mulby;          END INTERFACE
INTERFACE madby;  MODULE PROCEDURE madby;          END INTERFACE
INTERFACE msbby;  MODULE PROCEDURE msbby;          END INTERFACE
INTERFACE mulvb;  MODULE PROCEDURE mulvb;          END INTERFACE
INTERFACE madvb;  MODULE PROCEDURE madvb;          END INTERFACE
INTERFACE msbvb;  MODULE PROCEDURE msbvb;          END INTERFACE
INTERFACE mulxb;  MODULE PROCEDURE mulxb;          END INTERFACE
INTERFACE madxb;  MODULE PROCEDURE madxb;          END INTERFACE
INTERFACE msbxb;  MODULE PROCEDURE msbxb;          END INTERFACE
INTERFACE mulyb;  MODULE PROCEDURE mulyb;          END INTERFACE
INTERFACE madyb;  MODULE PROCEDURE madyb;          END INTERFACE
INTERFACE msbyb;  MODULE PROCEDURE msbyb;          END INTERFACE
INTERFACE mulbd;  MODULE PROCEDURE mulbd;          END INTERFACE
INTERFACE madbd;  MODULE PROCEDURE madbd;          END INTERFACE
INTERFACE msbbd;  MODULE PROCEDURE msbbd;          END INTERFACE
INTERFACE muldb;  MODULE PROCEDURE muldb;          END INTERFACE
INTERFACE maddb;  MODULE PROCEDURE maddb;          END INTERFACE
INTERFACE msbdb;  MODULE PROCEDURE msbdb;          END INTERFACE
INTERFACE udlbv;  MODULE PROCEDURE udlbv;          END INTERFACE
INTERFACE udlbx;  MODULE PROCEDURE udlbx;          END INTERFACE
INTERFACE udlby;  MODULE PROCEDURE udlby;          END INTERFACE
INTERFACE udlvb;  MODULE PROCEDURE udlvb;          END INTERFACE
INTERFACE udlxb;  MODULE PROCEDURE udlxb;          END INTERFACE
INTERFACE udlyb;  MODULE PROCEDURE udlyb;          END INTERFACE
INTERFACE u1lbv;  MODULE PROCEDURE u1lbv;          END INTERFACE
INTERFACE u1lbx;  MODULE PROCEDURE u1lbx;          END INTERFACE
INTERFACE u1lby;  MODULE PROCEDURE u1lby;          END INTERFACE
INTERFACE u1lvb;  MODULE PROCEDURE u1lvb;          END INTERFACE
INTERFACE u1lxb;  MODULE PROCEDURE u1lxb;          END INTERFACE
INTERFACE u1lyb;  MODULE PROCEDURE u1lyb;          END INTERFACE
INTERFACE linbv;  MODULE PROCEDURE linbv;          END INTERFACE
INTERFACE wrtb;   MODULE PROCEDURE wrtb;           END INTERFACE


CONTAINS


!=============================================================================
SUBROUTINE davco(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    davco
!
!   prgrmmr:    purser					      1999
!
! abstract:  Compute one row of the coefficients for the compact mid-interval
!            interpolation scheme characterized by matrix equation of the form,
!			 A.t = B.s			       (*)
!            Where s is the vector of "source" values, t the staggered "target" 
!            values.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA  - number of t-points operated on by this row of the A of (*)
!     NB  - number of s-points operated on by this row of the B of (*)
!     ZA  - coordinates of t-points used in this row of (*)
!     ZB  - coordinates of s-points used in this row of (*)
!     Z0  - nominal point of application of this row of (*)
!
!   output argument list:
!     A   - the NA coefficients A for this scheme
!     B   - the NB coefficients B for this scheme
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(r_kind)   , INTENT(IN   ) :: za(na),zb(nb),z0
REAL(r_kind)   , INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                    :: na1,nab,i
REAL(r_kind),DIMENSION(na+nb,na+nb):: w
REAL(r_kind),DIMENSION(na)         :: za0,pa
REAL(r_kind),DIMENSION(nb)         :: zb0,pb
REAL(r_kind),DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=2,nab; w(i,1:na)=pa;    pa=pa*za0; w(i,na1:nab)=pb; pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE davco


!=============================================================================
SUBROUTINE avco(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    avco
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for the compact mid-interval
!            interpolation scheme characterized by matrix equation of the form,
!                        A.t = B.s                             (*)
!            Where s is the vector of "source" values, t the staggered "target"
!            values.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA  - number of t-points operated on by this row of the A of (*)
!     NB  - number of s-points operated on by this row of the B of (*)
!     ZA  - coordinates of t-points used in this row of (*)
!     ZB  - coordinates of s-points used in this row of (*)
!     Z0  - nominal point of application of this row of (*)
!
!   output argument list:
!     A   - the NA coefficients A for this scheme
!     B   - the NB coefficients B for this scheme
!   
! attributes:  
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(r_kind),    INTENT(IN   ) :: za(na),zb(nb),z0
REAL(r_kind),    INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(r_kind), DIMENSION(na+nb,na+nb):: w
REAL(r_kind), DIMENSION(na)         :: za0,pa
REAL(r_kind), DIMENSION(nb)         :: zb0,pb
REAL(r_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=2,nab; w(i,1:na)=pa;    pa=pa*za0; w(i,na1:nab)=pb; pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE avco 


!=============================================================================
SUBROUTINE ddfco(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ddfco
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for either the compact 
!            differencing or quadrature scheme characterized by matrix 
!            equation of the form,
!			 A.d = B.c			       (*)
!            In either case, d is the derivative of c.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA  - number of d-points operated on by this row of the A of (*)
!     NB  - number of c-points operated on by this row of the B of (*)
!     ZA  - coordinates of d-points used in this row of (*)
!     ZB  - coordinates of c-points used in this row of (*)
!     Z0  - nominal point of application of this row of (*)
!
!   output argument list:
!     A   - the A-coefficients for this scheme
!     B   - the B-coefficients for this scheme
!   
! attributes:  
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(r_kind)   , INTENT(IN   ) :: za(na),zb(nb),z0
REAL(r_kind)   , INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(r_kind), DIMENSION(na+nb,na+nb):: w
REAL(r_kind), DIMENSION(na)         :: za0,pa
REAL(r_kind), DIMENSION(nb)         :: zb0,pb
REAL(r_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=3,nab; w(i,1:na)   =pa*(i-2); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;              pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE ddfco 


!=============================================================================
SUBROUTINE dfco(na,nb,za,zb,z0,a,b)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dfco
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for either the compact
!            differencing or quadrature scheme characterized by matrix
!            equation of the form,
!                        A.d = B.c                             (*)
!            In either case, d is the derivative of c.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA  - number of d-points operated on by this row of the A of (*)
!     NB  - number of c-points operated on by this row of the B of (*)
!     ZA  - coordinates of d-points used in this row of (*)
!     ZB  - coordinates of c-points used in this row of (*)
!     Z0  - nominal point of application of this row of (*)
!
!   output argument list:
!     A   - the A-coefficients for this scheme
!     B   - the B-coefficients for this scheme
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(r_kind),    INTENT(IN   ) :: za(na),zb(nb),z0
REAL(r_kind),    INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(r_kind), DIMENSION(na+nb,na+nb):: w
REAL(r_kind), DIMENSION(na)         :: za0,pa
REAL(r_kind), DIMENSION(nb)         :: zb0,pb
REAL(r_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=3,nab; w(i,1:na)   =pa*(i-2); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;              pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE dfco 


!=============================================================================
SUBROUTINE ddfco2(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ddfco2
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for either the compact second-
!            differencing scheme characterized by matrix equation of the form,
!			 A.d = B.c			       (*)
!            Where d is the second-derivative of c.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA   - number of d-points operated on by this row of the A of (*)
!     NB   - number of c-points operated on by this row of the B of (*)
!     ZA   - coordinates of d-points used in this row of (*)
!     ZB   - coordinates of c-points used in this row of (*)
!     Z0   - nominal point of application of this row of (*)
!
!   output argument list:
!     A    - the NA coefficients A for this scheme
!     B    - the NB coefficients B for this scheme
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(r_kind)   , INTENT(IN   ) :: za(na),zb(nb),z0
REAL(r_kind)   , INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(r_kind), DIMENSION(na+nb,na+nb):: w
REAL(r_kind), DIMENSION(na)         :: za0,pa
REAL(r_kind), DIMENSION(nb)         :: zb0,pb
REAL(r_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=4,nab; w(i,1:na)   =pa*(i-2)*(i-3); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;                           pb=pb*zb0; ENDDO
CALL inv_d(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE ddfco2 


!=============================================================================
SUBROUTINE dfco2(na,nb,za,zb,z0,a,b) 
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dfco2
!
!   prgrmmr:    purser                                        1999
!
! abstract:  Compute one row of the coefficients for either the compact second-
!            differencing scheme characterized by matrix equation of the form,
!                        A.d = B.c                             (*)
!            Where d is the second-derivative of c.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     NA   - number of d-points operated on by this row of the A of (*)
!     NB   - number of c-points operated on by this row of the B of (*)
!     ZA   - coordinates of d-points used in this row of (*)
!     ZB   - coordinates of c-points used in this row of (*)
!     Z0   - nominal point of application of this row of (*)
!
!   output argument list:
!     A    - the NA coefficients A for this scheme
!     B    - the NB coefficients B for this scheme
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: na,nb
REAL(r_kind),    INTENT(IN   ) :: za(na),zb(nb),z0
REAL(r_kind),    INTENT(  OUT) :: a(na),b(nb)
!-----------------------------------------------------------------------------
INTEGER(i_kind)                     :: na1,nab,i
REAL(r_kind), DIMENSION(na+nb,na+nb):: w
REAL(r_kind), DIMENSION(na)         :: za0,pa
REAL(r_kind), DIMENSION(nb)         :: zb0,pb
REAL(r_kind), DIMENSION(na+nb)      :: ab
!=============================================================================
na1=na+1; nab=na+nb
za0=za-z0  ; zb0=zb-z0
pa=one;     pb=-one
w=zero;     ab=zero
w(1,1:na)=one; ab(1)=one
DO i=4,nab; w(i,1:na)   =pa*(i-2)*(i-3); pa=pa*za0; ENDDO
DO i=2,nab; w(i,na1:nab)=pb;                           pb=pb*zb0; ENDDO
CALL inv(w,ab)
a=ab(1:na); b=ab(na1:nab)
END SUBROUTINE dfco2 


!=============================================================================
SUBROUTINE clib(a,m1,m2,mah1,mah2) ! Clip the dead space of the band matrix, a
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    clib
!
!   prgrmmr:    
!
! abstract:  Clip the dead space of the band matrix 
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2  - 
!
!   output argument list:
!     a                   -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind)   , INTENT(INOUT) :: a(m1,-mah1:mah2)

INTEGER(i_kind)                :: j

IF(m2-m1+mah1 < 0)STOP 'In CLIB, form of band matrix implies redundant rows'
DO j=1,mah1; a(1:min(m1,j),-j)=zero; ENDDO; DO j=m2-m1+1,mah2; a(max(1,m2-j+1):m1,j)=zero; ENDDO
END SUBROUTINE clib


!=============================================================================
SUBROUTINE dclib(a,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dclib
!
!   prgrmmr:    
!
! abstract:  Clip the dead space of the band matrix
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2  - 
!
!   output argument list:
!     a                   - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind)   , INTENT(INOUT) :: a(m1,-mah1:mah2)

INTEGER(i_kind)                :: j

IF(m2-m1+mah1 < 0)STOP 'In CLIB_d, form of band matrix implies redundant rows'
DO j=1,mah1; a(1:min(m1,j),-j)=zero; ENDDO; DO j=m2-m1+1,mah2; a(max(1,m2-j+1):m1,j)=zero; ENDDO
END SUBROUTINE dclib


!=============================================================================
SUBROUTINE cad1b(a,m1,mah1,mah2,mirror2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    cad1b
!
!   prgrmmr:    
!
! abstract:  Incorporate operand symmetry near end-1 of a band matrix operator
!
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A          -  Input as unclipped operator, output as symmetrized and clipped.
!     m1         - Sizes of implied full matrix
!     mah1, mah2 - Left and right semi-bandwidths of A.
!     mirror2    - 2*location of symmetry axis relative to end-1 operand element.
!
!   output argument list:
!     A          -  Input as unclipped operator, output as symmetrized and clipped.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1,mah1,mah2,mirror2
REAL(r_kind),     INTENT(INOUT) :: a(0:m1-1,-mah1:mah2)

INTEGER(i_kind)                :: i,i2,jm,jp,jpmax

IF(mirror2+mah1 > mah2)STOP 'In cad1b, mah2 insufficient'
DO i=0,m1-1; i2=i*2; jpmax=mirror2+mah1-i2; IF(jpmax <= -mah1)EXIT
   DO jm=-mah1,mah2; jp=mirror2-jm-i2; IF(jp <= jm)EXIT
      a(i,jp)=a(i,jp)+a(i,jm) ! Reflect and add
      a(i,jm)=zero            ! zero the exterior part
   ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY     csb1b(a,m1,mah1,mah2,mirror2)
!=============================================================================
! Like cad1b, but for antisymmetric operand
IF(mirror2+mah1 > mah2)STOP 'In csb1b, mah2 insufficient'
DO i=0,m1-1; i2=i*2; jpmax=mirror2+mah1-i2; IF(jpmax < -mah1)EXIT
   DO jm=-mah1,mah2; jp=mirror2-jm-i2; IF(jp < jm)EXIT
      a(i,jp)=a(i,jp)-a(i,jm) ! Reflect and subtract
      a(i,jm)=zero            ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE cad1b


!=============================================================================
SUBROUTINE cad2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    cad2b
!
!   prgrmmr:    
!
! abstract:  Incorporate operand symmetry near end-2 of a band matrix operator
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A          - Input as unclipped operator, output as symmetrized and clipped.
!     m1, m2     - Sizes of implied full matrix
!     mah1, mah2 - Left and right semi-bandwidths of A.
!     mirror2    - 2*location of symmetry axis relative to end-2 operand element.
!
!   output argument list:
!     A         - Input as unclipped operator, output as symmetrized and clipped.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1,m2,mah1,mah2,mirror2
REAL(r_kind),     INTENT(INOUT) :: a(1-m1:0,m1-m2-mah1:m1-m2+mah2)

INTEGER(i_kind)                :: i,i2,jm,jp,jmmin,nah1,nah2,mirror,j0

nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In cad2b, mah1 insufficient'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin >= nah2)EXIT
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm >= jp)EXIT
      a(i,jm)=a(i,jm)+a(i,jp) ! Reflect and add
      a(i,jp)=zero            ! zero the exterior part
   ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY    csb2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In csb2b, mah1 insufficient'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin > nah2)EXIT
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm > jp)EXIT
      a(i,jm)=a(i,jm)-a(i,jp) ! Reflect and subtract
      a(i,jp)=zero            ! zero the exterior part
   ENDDO
ENDDO
!=============================================================================
ENTRY    cex2b(a,m1,m2,mah1,mah2,mirror2)
!=============================================================================
nah1=mah1+m2-m1; nah2=mah2+m1-m2 ! Effective 2nd-index bounds of A
IF(mirror2-nah1 > -nah2)STOP 'In cex2b, mah1 insufficient'
mirror=mirror2/2
IF(mirror*2 /= mirror2)STOP 'In cex2b, mirror2 is not even'
DO i=0,1-m1,-1; i2=i*2; jmmin=mirror2-nah2-i2; IF(jmmin >= nah2)EXIT
   j0=mirror-i
   DO jp=nah2,nah1,-1; jm=mirror2-jp-i2; IF(jm >= jp)EXIT
      a(i,jm)=a(i,jm)-a(i,jp)    ! Reflect and subtract
      a(i,j0)=a(i,j0)+two*a(i,jp)! Apply double the coefficient to end
      a(i,jp)=zero               ! zero the exterior part
   ENDDO
ENDDO
END SUBROUTINE cad2b


!=============================================================================
SUBROUTINE copbt(a,b,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copbt
!
!   prgrmmr:  R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
! abstract:  Copy transpose of rectangular banded matrix A to B
!
!     Note:  This routine expects A and B always to occupy separate storage.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A      - input matrix in banded format
!     M1     - number of rows of A, columns of B
!     M2     - number of columns of A, rows of B
!     MAH1   - left-half-bandwidth of A, right-half-bandwidth of B
!     MAH2   - right-half-bandwidth of A, left-half-bandwidth of B
!
!   output argument list:
!     B      - output matrix in banded format
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2)
REAL(r_kind),     INTENT(  OUT) :: b(m2,-mah2:mah1)

INTEGER(i_kind)                 :: j, i

CALL clib(b,mah2,mah1,m2,m1)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); b(j+i,-j)=a(i,j); ENDDO
ENDDO
RETURN
ENTRY	 conbt(a,b,m1,m2,mah1,mah2)
CALL clib(b,mah2,mah1,m2,m1)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); b(j+i,-j)=-a(i,j); ENDDO
ENDDO
END SUBROUTINE copbt


!=============================================================================
SUBROUTINE copmb(afull,aband,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copmb
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2  - 
!     afull               -
!
!   output argument list:
!     aband               - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),                           INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     DIMENSION(m1,m2),        INTENT(IN   ) :: afull
REAL(r_kind),     DIMENSION(m1,-mah1:mah2),INTENT(  OUT) :: aband

INTEGER(i_kind)                                          :: i1,i2, i, j

CALL clib(aband,m1,m2,mah1,mah2)
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; aband(i,j)= afull(i,j+i); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY      conmb(afull,aband,m1,m2,mah1,mah2)
!=============================================================================
CALL clib(aband,m1,m2,mah1,mah2)
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; aband(i,j)=-afull(i,j+i); ENDDO
ENDDO
END SUBROUTINE copmb


!=============================================================================
SUBROUTINE copbm(aband,afull,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    copbm
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2  - 
!     aband               -
!
!   output argument list:
!     afull               - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),                           INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     DIMENSION(m1,-mah1:mah2),INTENT(IN   ) :: aband
REAL(r_kind),     DIMENSION(m1,m2),        INTENT(  OUT) :: afull

INTEGER(i_kind)                                          :: i1,i2, i, j

afull=zero
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; afull(i,j+i)= aband(i,j); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY      conbm(aband,afull,m1,m2,mah1,mah2)
!=============================================================================
afull=zero
DO j=1,m1; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   DO i=i1,i2; afull(i,j+i)=-aband(i,j); ENDDO
ENDDO
END SUBROUTINE copbm

 
!=============================================================================
SUBROUTINE mulbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulbb
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2     - 
!     mbh1, mbh2, mch1, mch2 - 
!     a, b                   -
!     c                      -
!
!   output argument list:
!     c                   - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL(r_kind),     INTENT(INOUT) :: c(m1,-mch1:mch2)

INTEGER(i_kind)                :: nch1, nch2, j, k, jpk, i1,i2

c=zero
ENTRY      madbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
nch1=mah1+mbh1; nch2=mah2+mbh2
IF(nch1 /= mch1 .OR. nch2 /= mch2)STOP 'In MULBB, dimensions inconsistent'
DO j=-mah1,mah2
   DO k=-mbh1,mbh2; jpk=j+k; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
      c(i1:i2,jpk)=c(i1:i2,jpk)+a(i1:i2,j)*b(j+i1:j+i2,k)
   ENDDO
ENDDO
END SUBROUTINE mulbb


!=============================================================================
SUBROUTINE msbbb(a,b,c,m1,m2,mah1,mah2,mbh1,mbh2,mch1,mch2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    msbbb
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1, m2, mah1, mah2     - 
!     mbh1, mbh2, mch1, mch2 -
!     a, b                   -
!
!   output argument list:
!     c                      - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mbh1, mbh2, mch1, mch2
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), b(m2,-mbh1:mbh2)
REAL(r_kind),     INTENT(  OUT) :: c(m1,-mch1:mch2)

INTEGER(i_kind)                 :: nch1, nch2, j, k, jpk, i1,i2

nch1=mah1+mbh1; nch2=mah2+mbh2
IF(nch1 /= mch1 .OR. nch2 /= mch2)STOP 'In MSBBB, dimensions inconsistent'
DO j=-mah1,mah2
   DO k=-mbh1,mbh2; jpk=j+k; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
      c(i1:i2,jpk)=c(i1:i2,jpk)-a(i1:i2,j)*b(j+i1:j+i2,k)
   ENDDO
ENDDO
END SUBROUTINE msbbb


!=============================================================================
SUBROUTINE LDUB(a,m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldub
!
!   prgrmmr:    R.J.Purser, 1994
!
! abstract:  Compute [L]*[D**-1]*[U] decomposition of asymmetric band-matrix
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A       - input as the asymmetric band matrix. On output, it contains
!               the [L]*[D**-1]*[U] factorization of the input matrix, where
!               [L] is lower triangular with unit main diagonal
!               [D] is a diagonal matrix
!               [U] is upper triangular with unit main diagonal
!     M       - The number of rows of array A
!     MAH1    - The left half-bandwidth of fortran array A
!     MAH2    - The right half-bandwidth of fortran array A
!
!   output argument list:
!     A       - input as the asymmetric band matrix. On output, it contains
!               the [L]*[D**-1]*[U] factorization of the input matrix, where
!               [L] is lower triangular with unit main diagonal
!               [D] is a diagonal matrix
!               [U] is upper triangular with unit main diagonal
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m,mah1, mah2 
REAL(r_kind),    INTENT(INOUT) :: a(m,-mah1:mah2) 

INTEGER(i_kind)               :: j, imost, jmost, jp, i
REAL(r_kind)                  :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)THEN
      PRINT '(" Failure in LDUB:"/" Matrix requires pivoting or is singular")'
      STOP
   ENDIF
   ajji=one/ajj
   a(j,0)=ajji
   DO i=jp,imost
      aij=ajji*a(i,j-i)
      a(i,j-i)=aij
      a(i,jp-i:jmost-i)=a(i,jp-i:jmost-i)-aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,jp-j:jmost-j)=ajji*a(j,jp-j:jmost-j)
ENDDO
END SUBROUTINE LDUB


!=============================================================================
SUBROUTINE DLDUB(a,m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dldub
!
!   prgrmmr:    
!
! abstract:  
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m, mah1, mah2  - 
!     a              - 
!
!   output argument list:
!     a              - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

INTEGER(i_kind),  INTENT(IN   ) :: m,mah1, mah2 
REAL(r_kind)   ,  INTENT(INOUT) :: a(m,-mah1:mah2) 

INTEGER(i_kind)                :: j, imost, jmost, jp, i
REAL(r_kind)                   :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)THEN
      PRINT '(" Fails in LDUB_d:"/" Matrix requires pivoting or is singular")'
      STOP
   ENDIF
   ajji=one/ajj
   a(j,0)=ajji
   DO i=jp,imost
      aij=ajji*a(i,j-i)
      a(i,j-i)=aij
      a(i,jp-i:jmost-i)=a(i,jp-i:jmost-i)-aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,jp-j:jmost-j)=ajji*a(j,jp-j:jmost-j)
ENDDO
END SUBROUTINE DLDUB

!=============================================================================
SUBROUTINE L1UBB(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    l1ubb
!
!   prgrmmr:    R.J.Purser, 1996
!
! abstract:  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix  
!            [A] replace lower triangular elements of [A] by [D**-1]*[L]*[D], 
!            the upper by [U], replace matrix [B] by [D**-1]*[B].
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!     M     - Number of rows of A and B
!     MAH1  - left half-width of fortran array A
!     MAH2  - right half-width of fortran array A
!     MBH1  - left half-width of fortran array B
!     MBH2  - right half-width of fortran array B
!
!   output argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) ::  m,mah1, mah2, mbh1, mbh2 
REAL(r_kind)   , INTENT(INOUT) :: a(m,-mah1:mah2), b(m,-mbh1:mbh2)

INTEGER(i_kind)                :: j, imost, jmost, jleast, jp, i
REAL(r_kind)                   :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jleast=MAX(1,j-mah1)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)STOP 'failure in L1UBB'
   ajji=one/ajj
   a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
   DO i=jp,imost
      aij=a(i,j-i)
      a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,0)=one
   b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE L1UBB


!=============================================================================
SUBROUTINE DL1UBB(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dl1ubb
!
!   prgrmmr:    R.J.Purser, 1996
!
! abstract:  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix
!            [A] replace lower triangular elements of [A] by [D**-1]*[L]*[D],
!            the upper by [U], replace matrix [B] by [D**-1]*[B].
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!     M     - Number of rows of A and B
!     MAH1  - left half-width of fortran array A
!     MAH2  - right half-width of fortran array A
!     MBH1  - left half-width of fortran array B
!     MBH2  - right half-width of fortran array B
!
!   output argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) ::  mah1, mah2, mbh1, mbh2 
REAL(r_kind)   ,  INTENT(INOUT) :: a(m,-mah1:mah2), b(m,-mbh1:mbh2)

INTEGER(i_kind)                :: m,j, imost, jmost, jleast, jp, i
REAL(r_kind)                   :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jleast=MAX(1,j-mah1)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)STOP 'failure in DL1UBB'
   AJJI=one/AJJ
   a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
   DO I=JP,IMOST
      AIJ=A(I,J-I)
      a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
   ENDDO
   A(J,0)=one
   b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE DL1UBB


!=============================================================================
SUBROUTINE l1ueb(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    l1ueb
!
!   prgrmmr:    R.J.Purser, 1998
!
! abstract:  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix
!            [A] replace all but row zero of the lower triangular
!            elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!            replace matrix [B] by [D**-1]*[B].
!            This is a special adaptation of L1UBB used to process quadarature weights
!            for QEDBV etc in which the initial quadrature value is provided as input
!            instead of being implicitly assumed zero (which is the case for QZDBV etc).
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!     M     - number of rows of B, one less than the rows of A (which has "row 0")
!     MAH1  - left half-width of fortran array A
!     MAH2  - right half-width of fortran array A
!     MBH1  - left half-width of fortran array B
!     MBH2  - right half-width of fortran array B
!
!   output argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m,mah1, mah2, mbh1, mbh2 
REAL(r_kind)   , INTENT(INOUT) :: a(0:m,-mah1:mah2), b(m,-mbh1:mbh2)

INTEGER(i_kind) :: j, imost, jmost, jleast, jp, i
REAL(r_kind)    :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jleast=MAX(0,j-mah1)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)STOP 'failure in L1UEB'
   ajji=one/ajj
   a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
   DO i=jp,imost
      aij=a(i,j-i)
      a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,0)=one
   b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE l1ueb


!=============================================================================
SUBROUTINE dl1ueb(a,b,m,mah1,mah2,mbh1,mbh2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dl1ueb
!
!   prgrmmr:    R.J.Purser, 1998
!
! abstract:  Form the [L]*[D]*[U] decomposition of asymmetric band-matrix
!            [A] replace all but row zero of the lower triangular
!            elements of [A] by [D**-1]*[L]*[D], the upper by [U],
!            replace matrix [B] by [D**-1]*[B].
!            This is a special adaptation of L1UBB used to process quadarature weights
!            for QEDBV etc in which the initial quadrature value is provided as input
!            instead of being implicitly assumed zero (which is the case for QZDBV etc).
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!     M     - number of rows of B, one less than the rows of A (which has "row 0")
!     MAH1  - left half-width of fortran array A
!     MAH2  - right half-width of fortran array A
!     MBH1  - left half-width of fortran array B
!     MBH2  - right half-width of fortran array B
!
!   output argument list:
!     A     - input as band matrix, output as lower and upper triangulars with 1s
!             implicitly assumed to lie on the main diagonal. The product of these
!             triangular matrices is [D**-1]*[A], where [D] is a diagonal matrix.
!     B     - in as band matrix, out as same but premultiplied by diagonal [D**-1]
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m,mah1, mah2, mbh1, mbh2 
REAL(r_kind)   ,  INTENT(INOUT) :: a(0:,-mah1:), b(:,-mbh1:)

INTEGER(i_kind)                   :: j, imost, jmost, jleast, jp, i
REAL(r_kind)                      :: ajj, ajji, aij

DO j=1,m
   imost=MIN(m,j+mah1)
   jmost=MIN(m,j+mah2)
   jleast=MAX(0,j-mah1)
   jp=j+1
   ajj=a(j,0)
   IF(ajj == zero)STOP 'failure in L1UEB_d'
   ajji=one/ajj
   a(j,jleast-j:jmost-j) = ajji * a(j,jleast-j:jmost-j)
   DO i=jp,imost
      aij=a(i,j-i)
      a(i,jp-i:jmost-i) = a(i,jp-i:jmost-i) - aij*a(j,jp-j:jmost-j)
   ENDDO
   a(j,0)=one
   b(j,-mbh1:mbh2) = ajji * b(j,-mbh1:mbh2)
ENDDO
END SUBROUTINE dl1ueb


!=============================================================================
SUBROUTINE L1LB(a,b,m,mah)   ! Cholesky LU decomposition of Banded.
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    l1lb
!
!   prgrmmr: 
!
! abstract:  Cholesky LU decomposition of Banded.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A     - 
!     M     - 
!     MAH   - 
!
!   output argument list:
!     B     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah
REAL(r_kind),     INTENT(IN   ) :: a(m,-mah:mah)
REAL(r_kind),     INTENT(  OUT) :: b(m,-mah:0)

INTEGER(i_kind)                 :: i, j,jmi
REAL(r_kind)                    :: s

CALL clib(b,m,m,mah,0)
DO j=1,m
   s=a(j,0)-DOT_PRODUCT(b(j,-mah:-1),b(j,-mah:-1))
   IF(s <= zero)THEN
      PRINT '(" L1LB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   s=SQRT(s); b(j,0)=s; s=one/s
   DO i=j+1,MIN(m,j+mah); jmi=j-i
      b(i,jmi)=s*(a(i,jmi)-DOT_PRODUCT(b(i,-mah:jmi-1),b(j,-mah-jmi:-1)))
   ENDDO
ENDDO
END SUBROUTINE L1LB

!=============================================================================
SUBROUTINE LDLB(a,b,d,m,mah) ! Modified Cholesky [L(D**-1)U, without sqrt]
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb
!
!   prgrmmr:    
!
! abstract:   Modified Cholesky [L(D**-1)U, without sqrt]
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a     - 
!     m     - 
!     mah   - 
!
!   output argument list:
!     b     - 
!     d     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah
REAL(r_kind),     INTENT(IN   ) :: a(m,-mah:mah)
REAL(r_kind),     INTENT(  OUT) :: b(m,-mah:0)
REAL(r_kind),     INTENT(  OUT) :: d(m) 

INTEGER(i_kind)                 :: i, j,k,jmi,lj,li
REAL(r_kind)                    :: s,t

CALL clib(b,m,m,mah,0); b(:,0)=one
DO j=1,m; lj=MAX(-mah,1-j)
   s=a(j,0)
   do k=lj,-1
      s=s-b(j,k)**2*d(k+j)
   enddo
   IF(s <= zero)THEN
      PRINT '(" LDLB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   d(j)=s; s=one/s
   DO i=j+1,MIN(m,j+mah); jmi=j-i; li=MAX(-mah,1-i); lj=li-jmi
      t=a(i,jmi)
      do k=li,jmi-1
         t=t-b(i,k)*b(j,k-jmi)*d(i+k)
      enddo
      b(i,jmi)=s*t
   ENDDO
ENDDO
d=one/d
END SUBROUTINE LDLB

!=============================================================================
SUBROUTINE DLDLB(a,b,d,m,mah) ! Modified Cholesky [L(D**-1)U, without sqrt]
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dl1lb
!
!   prgrmmr:    
!
! abstract:  Modified Cholesky [L(D**-1)U, without sqrt]
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a     - 
!     m     - 
!     mah   - 
!
!   output argument list:
!     b     - 
!     d     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah
REAL(r_kind)   ,  INTENT(IN   ) :: a(m,-mah:mah)
REAL(r_kind)   ,  INTENT(  OUT) :: b(m,-mah:0)
REAL(r_kind)   ,  INTENT(  OUT) :: d(m) 

INTEGER(i_kind)                 :: i, j,k,jmi,lj,li
REAL(r_kind)                    :: s,t

CALL clib_d(b,m,m,mah,0); b(:,0)=one
DO j=1,m; lj=MAX(-mah,1-j)
   s=a(j,0)
   do k=lj,-1
      s=s-b(j,k)**2*d(k+j)
   enddo
   IF(s <= zero)THEN
      PRINT '(" DLDLB detects non-positivity at diagonal index",i5)',j
      STOP
   ENDIF
   d(j)=s; s=one/s
   DO i=j+1,MIN(m,j+mah); jmi=j-i;  
      li=MAX(-mah,1-i); 
      lj=li-jmi; 
      t=a(i,jmi)
      do k=li,jmi-1
         t=t-b(i,k)*b(j,k-jmi)*d(i+k)
      enddo
      b(i,jmi)=s*t
   ENDDO
ENDDO
d=one/d
END SUBROUTINE DLDLB

!=============================================================================
SUBROUTINE UDUB(a,b,d,m,mah) ! Modified reverse Cholesky [U(D**-1)U^t],
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udub
!
!   prgrmmr:    
!
! abstract:   Modified reverse Cholesky [U(D**-1)U^t],
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a     - 
!     m     - 
!     mah   - 
!
!   output argument list:
!     b     - 
!     d     - 
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),        INTENT(IN   ) :: m, mah
REAL(r_kind),           INTENT(IN   ) :: a(m,-mah:mah)
REAL(r_kind),           INTENT(  OUT) :: b(m,0:mah)
REAL(r_kind),           INTENT(  OUT) :: d(m) 

REAL(r_kind), DIMENSION(m,-mah:mah  ) :: at
REAL(r_kind), DIMENSION(m,-mah:0)     :: bt
REAL(r_kind), DIMENSION(m)            :: dt

at=a(m:1:-1,mah:-mah:-1); CALL ldlb(at,bt,dt,m,mah);
b=bt(m:1:-1,0:-mah:-1); d=dt(m:1:-1)
END SUBROUTINE UDUB


!=============================================================================
SUBROUTINE DUDUB(a,b,d,m,mah) ! Modified reverse Cholesky [U(D**-1)U^t],
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    dudub
!
!   prgrmmr:       
!
! abstract:   Modified reverse Cholesky [U(D**-1)U^t],
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     a     - 
!     m     - 
!     mah   - 
!
!   output argument list:
!     b     -
!     d     -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),        INTENT(IN   ) :: m, mah
REAL(r_kind),           INTENT(IN   ) :: a(m,-mah:mah)
REAL(r_kind),           INTENT(  OUT) :: b(m,0:mah)
REAL(r_kind),           INTENT(  OUT) :: d(m) 

REAL(r_kind), DIMENSION(m,-mah:mah  ) :: at
REAL(r_kind), DIMENSION(m,-mah:0)     :: bt
REAL(r_kind), DIMENSION(m)            :: dt

at=a(m:1:-1,mah:-mah:-1); CALL ldlb_d(at,bt,dt,m,mah);
b=bt(m:1:-1,0:-mah:-1);   d=dt(m:1:-1)
END SUBROUTINE DUDUB


!=============================================================================
SUBROUTINE mulbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulbv
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times a Vector.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the matrix
!     V1   - the input vector
!     M1   - the number of rows assumed for A and for V2
!     M2   - the number of columns assumed for A and rows for V1
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V2   - the output vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2)
REAL(r_kind),     INTENT(  OUT) :: v2(m1)

INTEGER(i_kind)                 :: j, i1,i2 

v2 = zero
!=============================================================================
ENTRY	 madbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(i1:i2) = v2(i1:i2) + a(i1:i2,j)*v1(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbv(a,v1,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(i1:i2) = v2(i1:i2) - a(i1:i2,j)*v1(j+i1:j+i2)
ENDDO
END SUBROUTINE mulbv


!=============================================================================
SUBROUTINE mulbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulbx
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times parallel X-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the matrix
!     V1   - the array of input vectors
!     M1   - the number of rows assumed for A and for V2
!     M2   - the number of columns assumed for A and rows for V1
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - the number of parallel X-vectors
!
!   output argument list:
!     V2   - the array of output vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(m2,my)
REAL(r_kind),     INTENT(  OUT) :: v2(m1,my)

INTEGER(i_kind)                 :: i,j

v2=zero
!=============================================================================
ENTRY	 madbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(i,:)=v2(i,:)+a(i,j)*v1(i+j,:); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbx(a,v1,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(i,:)=v2(i,:)-a(i,j)*v1(i+j,:); ENDDO
ENDDO
END SUBROUTINE mulbx


!=============================================================================
SUBROUTINE mulby(a,v1,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulby
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times parallel Y-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the matrix
!     V1   - the array of input vectors
!     M1   - the number of rows assumed for A and for V2
!     M2   - the number of columns assumed for A and rows for V1
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - the length of each of the of parallel Y-vectors
!
!   output argument list:
!     V2   - the array of output vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2), v1(mx,m2)
REAL(r_kind),     INTENT(  OUT) :: v2(mx,m1)

INTEGER(i_kind)                 :: i,j

v2(1:mx,1:m1) = zero
ENTRY	 madby(a,v1,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,i)=v2(:,i)+a(i,j)*v1(:,i+j); ENDDO
ENDDO
RETURN
ENTRY	 msbby(a,v1,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,i)=v2(:,i)-a(i,j)*v1(:,i+j); ENDDO
ENDDO
END SUBROUTINE mulby


!=============================================================================
SUBROUTINE MULVB(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulvb
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Vector times a Banded matrix.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V1   - the input row-vector
!     A    - is the matrix
!     M1   - the number of rows assumed for A and columns for V1
!     M2   - the number of columns assumed for A and for V2
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V2   - the output vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: v1(m1), a(m1,-mah1:mah2)
REAL(r_kind),     INTENT(  OUT) :: v2(m2)

INTEGER(i_kind)                 :: j, i1,i2

v2=zero
!=============================================================================
ENTRY	 madvb(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)+v1(i1:i2)*a(i1:i2,j)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbvb(v1,a,v2, m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   v2(j+i1:j+i2)=v2(j+i1:j+i2)-v1(i1:i2)*a(i1:i2,j)
ENDDO
END SUBROUTINE mulvb


!=============================================================================
SUBROUTINE mulxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulxb
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of X-Vectors times Banded matrix.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V1   - the array of input row-vectors
!     A    - is the matrix
!     M1   - the number of rows assumed for A and columns for V1
!     M2   - the number of columns assumed for A and V2
!     MAH1 -  the left half-bandwidth of fortran array A
!     MAH2 -  the right half-bandwidth of fortran array A
!     MY   - the number of parallel X-vectors
!
!   output argument list:
!     V2   - the array of output vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, my
REAL(r_kind),     INTENT(IN   ) :: v1(m1,my), a(m1,-mah1:mah2)
REAL(r_kind),     INTENT(  OUT) :: v2(m2,my)

INTEGER(i_kind)                 :: i,j

v2=zero
!=============================================================================
ENTRY	 madxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)+v1(i,:)*a(i,j); ENDDO
ENDDO
RETURN
!=============================================================================
ENTRY	 msbxb(v1,a,v2, m1,m2,mah1,mah2,my)
!=============================================================================
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(j+i,:)=v2(j+i,:)-v1(i,:)*a(i,j); ENDDO
ENDDO
END SUBROUTINE mulxb


!=============================================================================
SUBROUTINE mulyb(v1,a,v2, m1,m2,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulyb
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of Y-Vectors times a Banded matrix.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the matrix
!     V1   - the array of input row-vectors
!     M1   - the number of rows assumed for A and colums for V1
!     M2   - the number of columns assumed for A and V2
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - the length of each of the parallel Y-vectors
!
!   output argument list:
!     V2   - the array of output vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2, mx
REAL(r_kind),     INTENT(IN   ) :: v1(mx,m1), a(m1,-mah1:mah2)
REAL(r_kind),     INTENT(  OUT) :: v2(mx,m2)

INTEGER(i_kind)                 :: i,j

v2=zero
ENTRY	 madyb(v1,a,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,j+i)=v2(:,j+i)+v1(:,i)*a(i,j); ENDDO
ENDDO
RETURN
ENTRY	 msbyb(v1,a,v2, m1,m2,mah1,mah2,mx)
DO j=-mah1,mah2
   DO i=MAX(1,1-j),MIN(m1,m2-j); v2(:,j+i)=v2(:,j+i)-v1(:,i)*a(i,j); ENDDO
ENDDO
END SUBROUTINE mulyb


!=============================================================================
SUBROUTINE mulbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    mulbd
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times a Diagonal
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - is the input banded-matrix
!     D    - the diagonal matrix
!     M1   - the number of rows assumed for A and for B
!     M2   - number of columns assumed for A and B, number of elements of D
!     MAH1 - the left half-bandwidth of arrays A and B
!     MAH2 - the right half-bandwidth of arrays A and B
!
!   output argument list:
!     B    - the output matrix
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: d(m2)
REAL(r_kind),     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)

INTEGER(i_kind)                :: j, i1,i2

CALL clib(b,m1,m2,mah1,mah2)
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j)=a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 madbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j) = b(i1:i2,j)+a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
RETURN
!=============================================================================
ENTRY	 msbbd(a,d,b,m1,m2,mah1,mah2)
!=============================================================================
DO j=-mah1,mah2; i1=MAX(1,1-j); i2=MIN(m1,m2-j)
   b(i1:i2,j) = b(i1:i2,j)-a(i1:i2,j)*d(j+i1:j+i2)
ENDDO
END SUBROUTINE mulbd


!=============================================================================
SUBROUTINE muldb(d,a,b,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    muldb
!
!   prgrmmr:     R.J.Purser,  1994
!
! abstract:      MULtipication of a Banded matrix times a Diagonal
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     D    - the diagonal matrix
!     A    - is the input banded-matrix ! <->  if A and B are actually
!     M1   - the number of rows assumed for A and for B
!     M2   - number of columns assumed for A and B, number of elements of D
!     MAH1 - the left half-bandwidth of arrays A and B
!     MAH2 - the right half-bandwidth of arrays A and B
!
!   output argument list:
!     B    - the output matrix          ! <->  equivalent arrays.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: d(m1)
REAL(r_kind),     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)

INTEGER(i_kind)                 :: j

CALL clib(b,m1,m2,mah1,mah2)
DO j=-mah1,mah2; b(:,j)=d(:)*a(:,j); ENDDO
END SUBROUTINE muldb


!=============================================================================
SUBROUTINE maddb(d,a,b,m1,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    maddb
!
!   prgrmmr:     
!
! abstract:      
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d    - 
!     M1   - 
!     MAH1 - 
!     MAH2 -
!     a    -
!     b    -
!
!   output argument list:
!     a    -
!     b    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: d(m1)
REAL(r_kind),     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)

INTEGER(i_kind)                 :: j

DO j=-mah1,mah2; b(:,j)=b(:,j)+d(:)*a(:,j); ENDDO
END SUBROUTINE maddb


!=============================================================================
SUBROUTINE msbdb(d,a,b,m1,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    msbdb
!
!   prgrmmr:
!
! abstract:
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     d    - 
!     M1   - 
!     MAH1 - 
!     MAH2 - 
!     a    -
!     b    -
!
!   output argument list:
!     a    -
!     b    -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: d(m1) 
REAL(r_kind),     INTENT(INOUT) :: a(m1,-mah1:mah2),b(m1,-mah1:mah2)

INTEGER(i_kind)                 :: j

DO j=-mah1,mah2; b(:,j)=b(:,j)-d(:)*a(:,j); ENDDO
END SUBROUTINE msbdb


!=============================================================================
SUBROUTINE udlbv(a,v, m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlbv
!
!   prgrmmr:     R.J.Purser, 1994
!
! abstract:      BACk-substitution step of linear inversion involving
!                Banded matrix and Vector.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB
!     V    - input as right-hand-side vector, output as solution vector
!     M    - the number of rows assumed for A and for V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V    - input as right-hand-side vector, output as solution vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ):: m, mah1, mah2
REAL(r_kind),     INTENT(IN   ):: a(m,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT):: v(m)

INTEGER(i_kind)                :: i, j
REAL(r_kind)                   :: vj

DO j=1,m
   vj=v(j)
   DO i=j+1,MIN(m,j+mah1); v(i)=v(i)-a(i,j-i)*vj; ENDDO; v(j)=a(j,0)*vj
ENDDO
DO j=m,2,-1
   vj=v(j)
   DO i=MAX(1,j-mah2),j-1; v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
END SUBROUTINE udlbv


!=============================================================================
SUBROUTINE udlbx(a,v, mx,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlbx
!
!   prgrmmr:     R.J.Purser, 1994
!
! abstract:      BACk-substitution step of parallel linear inversion involving
!                Banded matrix and X-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB or, if N=NA, by LDUB
!     V    - input as right-hand-side vectors, output as solution vectors
!     MX   - the number of rows assumed for A and length of
!            X-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - number of parallel X-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: mx, mah1, mah2, my
REAL(r_kind),     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: jx, ix

DO jx=1,mx
   DO ix=jx+1,MIN(mx,jx+mah1); v(ix,:) = v(ix,:) - a(ix,jx-ix)*v(jx,:); ENDDO
   v(jx,:) = a(jx,0) * v(jx,:)
ENDDO
DO jx=mx,2,-1
   DO ix=MAX(1,jx-mah2),jx-1; v(ix,:) = v(ix,:) - a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
END SUBROUTINE udlbx


!=============================================================================
SUBROUTINE udlby(a,v, my,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlby
!
!   prgrmmr:     R.J.Purser, 1994
!
! abstract:      BACk-substitution step of parallel linear inversion involving
!                Banded matrix and Y-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB or, if N=NA, by LDUB
!     V    - input as right-hand-side vectors, output as solution vectors
!     MY   - the number of rows assumed for A and length of
!            Y-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - number of parallel Y-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: my, mah1, mah2, mx
REAL(r_kind),     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: iy, jy

DO jy=1,my
   DO iy=jy+1,MIN(my,jy+mah1); v(:,iy) = v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
   v(:,jy)=a(jy,0)*v(:,jy)
ENDDO
DO jy=my,2,-1
   DO iy=MAX(1,jy-mah2),jy-1; v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
END SUBROUTINE udlby


!=============================================================================
SUBROUTINE udlvb(v,a, m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlvb
!
!   prgrmmr:     R.J.Purser, 1994
!
! abstract:      BACk-substitution step of linear inversion involving
!                row-Vector and Banded matrix.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB
!     M    - the number of rows assumed for A and columns for V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V    - input as right-hand-side row-vector, output as solution vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: a(m,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(m)

INTEGER(i_kind)                :: i, j
REAL(r_kind)                   :: vi

DO i=1,m
   vi=v(i)
   DO j=i+1,MIN(m,i+mah2); v(j)=v(j)-vi*a(i,j-i); ENDDO
   v(i)=vi*a(i,0)
ENDDO
DO i=m,2,-1
   vi=v(i)
   DO j=MAX(1,i-mah1),i-1; v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
END SUBROUTINE udlvb


!=============================================================================
SUBROUTINE udlxb(v,a, mx,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlxb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:     BACk-substitution step of parallel linear inversion involving
!               Banded matrix and row-X-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB
!     MX   - the number of rows assumed for A and length of
!            X-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - number of parallel X-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: mx, mah1, mah2, my
REAL(r_kind),     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: ix, jx

DO ix=1,mx
   DO jx=ix+1,MIN(mx,ix+mah2); v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
   v(ix,:)=v(ix,:)*a(ix,0)
ENDDO
DO ix=mx,2,-1
   DO jx=MAX(1,ix-mah1),ix-1; v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
END SUBROUTINE udlxb

!=============================================================================
SUBROUTINE udlyb(v,a, my,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    udlyb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:      BACk-substitution step of parallel linear inversion involving
!                Banded matrix and row-Y-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!     A    - encodes the (L)*(D**-1)*(U) factorization of the linear-system
!            matrix, as supplied by subroutine LDUB
!     MY   - the number of rows assumed for A and length of
!            Y-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - number of parallel Y-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: my, mah1, mah2, mx
REAL(r_kind),     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: iy, jy

DO iy=1,my
   DO jy=iy+1,MIN(my,iy+mah2); v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
   v(:,iy)=v(:,iy)*a(iy,0)
ENDDO
DO iy=my,2,-1
   DO jy=MAX(1,iy-mah1),iy-1; v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
END SUBROUTINE udlyb


!=============================================================================
SUBROUTINE u1lbv(a,v, m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lbv
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      BACk-substitution step ((U**-1)*(L**-1)) of linear inversion 
!                involving special Banded matrix and right-Vector.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     V    - input as right-hand-side vector, output as solution vector
!     M    - the number of rows assumed for A and for V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V    - input as right-hand-side vector, output as solution vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: a(m,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(m)

INTEGER(i_kind)                :: i, j
REAL(r_kind)                   :: vj

DO j=1,m
   vj=v(j)
   DO i=j+1,MIN(m,j+mah1); v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
DO j=m,2,-1
   vj=v(j)
   DO i=MAX(1,j-mah2),j-1; v(i)=v(i)-a(i,j-i)*vj; ENDDO
ENDDO
END SUBROUTINE u1lbv


!=============================================================================
SUBROUTINE u1lbx(a,v, mx,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lbx
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of parallel linear inversion 
!                involving Banded matrix and X-right-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     V    - input as right-hand-side vectors, output as solution vectors
!     MX   - the number of rows assumed for A and length of
!            X-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - number of parallel X-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: mx, mah1, mah2, my
REAL(r_kind),     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: ix, jx

DO jx=1,mx
   DO ix=jx+1,MIN(mx,jx+mah1); v(ix,:)=v(ix,:)-a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
DO jx=mx,2,-1
   DO ix=MAX(1,jx-mah2),jx-1; v(ix,:)=v(ix,:)-a(ix,jx-ix)*v(jx,:); ENDDO
ENDDO
END SUBROUTINE u1lbx


!=============================================================================
SUBROUTINE u1lby(a,v, my,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lby
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of parallel linear inversion 
!                involving Banded matrix and Y-right-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - encodes the [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     V    - input as right-hand-side vectors, output as solution vectors
!     MY   - the number of rows assumed for A and length of
!            Y-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - number of parallel Y-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: my, mah1, mah2, mx
REAL(r_kind),     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: iy, jy

DO jy=1,my
   DO iy=jy+1,MIN(my,jy+mah1); v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
DO jy=my,2,-1
   DO iy=MAX(1,jy-mah2),jy-1; v(:,iy)=v(:,iy)-a(iy,jy-iy)*v(:,jy); ENDDO
ENDDO
END SUBROUTINE u1lby


!=============================================================================
SUBROUTINE u1lvb(v,a, m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lvb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of linear inversion involving
!                left-Vector and Banded matrix.
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side row-vector, output as solution vector
!     A    - encodes the special [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     M    - the number of rows assumed for A and columns for V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!
!   output argument list:
!     V    - input as right-hand-side row-vector, output as solution vector
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m, mah1, mah2
REAL(r_kind),    INTENT(IN   ) :: a(m,-mah1:mah2)
REAL(r_kind),    INTENT(INOUT) :: v(m)

INTEGER(i_kind)               :: i, j
REAL(r_kind)                  :: vi

DO i=1,m
   vi=v(i)
   DO j=i+1,MIN(m,i+mah2); v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
DO i=m,2,-1
   vi=v(i)
   DO j=MAX(1,i-mah1),i-1; v(j)=v(j)-vi*a(i,j-i); ENDDO
ENDDO
END SUBROUTINE u1lvb


!=============================================================================
SUBROUTINE u1lxb(v,a, mx,mah1,mah2,my)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lxb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of parallel linear inversion 
!                involving Banded matrix and X-left-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!     A    - encodes the special [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     MX   - the number of rows assumed for A and length of
!            X-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MY   - number of parallel X-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: mx, mah1, mah2, my
REAL(r_kind),     INTENT(IN   ) :: a(mx,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: ix, jx

DO ix=1,mx
   DO jx=ix+1,MIN(mx,ix+mah2); v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
DO ix=mx,2,-1
   DO jx=MAX(1,ix-mah1),ix-1;  v(jx,:)=v(jx,:)-v(ix,:)*a(ix,jx-ix); ENDDO
ENDDO
END SUBROUTINE u1lxb


!=============================================================================
SUBROUTINE u1lyb(v,a, my,mah1,mah2,mx)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    u1lyb
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1996
!
! abstract:      Special BaCk-substitution step of parallel linear inversion 
!                involving special Banded matrix and Y-left-Vectors.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!     A    - encodes the [L]*[U] factorization of the linear-system
!            matrix, as supplied by subroutine L1UBB
!     MY   - the number of rows assumed for A and length of
!            Y-vectors stored in V
!     MAH1 - the left half-bandwidth of fortran array A
!     MAH2 - the right half-bandwidth of fortran array A
!     MX   - number of parallel Y-vectors inverted
!
!   output argument list:
!     V    - input as right-hand-side vectors, output as solution vectors
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: my, mah1, mah2, mx
REAL(r_kind),     INTENT(IN   ) :: a(my,-mah1:mah2)
REAL(r_kind),     INTENT(INOUT) :: v(mx,my)

INTEGER(i_kind)                :: iy, jy

DO iy=1,my
   DO jy=iy+1,MIN(my,iy+mah2); v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
DO iy=my,2,-1
   DO jy=MAX(1,iy-mah1),iy-1;  v(:,jy)=v(:,jy)-v(:,iy)*a(iy,jy-iy); ENDDO
ENDDO
END SUBROUTINE u1lyb


!=============================================================================
SUBROUTINE linbv(a,v,m,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    linbv
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:      Solve LINear system with square Banded-matrix and vector V
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     A    - system matrix on input, its [L]*[D**-1]*[U] factorization on exit
!     V    - vector of right-hand-sides on input, solution vector on exit
!     M    - order of matrix A
!     MAH1 - left half-bandwidth of A
!     MAH2 - right half-bandwidth of A
!
!   output argument list:
!     A    - system matrix on input, its [L]*[D**-1]*[U] factorization on exit
!     V    - vector of right-hand-sides on input, solution vector on exit
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind), INTENT(IN   ) :: m, mah1, mah2
REAL(r_kind),    INTENT(INOUT) :: a(m,-mah1:mah2), v(m)

CALL ldub(a,m,mah1,mah2)
CALL udlbv(a,v,m,mah1,mah2)
END SUBROUTINE linbv


!=============================================================================
SUBROUTINE wrtb(a,m1,m2,mah1,mah2)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    wrtb
!
!   prgrmmr:     
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     m1   -
!     m2   -
!     mah1 - 
!     mah2 -
!     a    -
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
implicit none

INTEGER(i_kind),  INTENT(IN   ) :: m1, m2, mah1, mah2
REAL(r_kind),     INTENT(IN   ) :: a(m1,-mah1:mah2)

INTEGER(i_kind)              :: i1, i2, i, j1, j2, j, nj1

DO i1=1,m1,20
   i2=MIN(i1+19,m1)
   PRINT '(7x,6(i2,10x))',(j,j=-mah1,mah2)
   DO i=i1,i2
      j1=MAX(-mah1,1-i)
      j2=MIN(mah2,m2-i)
      nj1=j1+mah1
      IF(nj1==0)PRINT '(1x,i3,6(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==1)PRINT '(1x,i3,12x,5(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==2)PRINT '(1x,i3,24x,4(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==3)PRINT '(1x,i3,36x,3(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==4)PRINT '(1x,i3,48x,2(1x,e12.5))',i,(a(i,j),j=j1,j2)
      IF(nj1==5)PRINT '(1x,i3,60x,1(1x,e12.5))',i,(a(i,j),j=j1,j2)
   ENDDO
   READ(*,*)
ENDDO
END SUBROUTINE wrtb

END MODULE MODULE_pmat2

module module_fitcons
!$$$ module documentation block
!           .      .    .                                       .
! module:  module_fitcons
!  prgmmr: purser
!
! abstract:
!
! program history log:
!   1994-  -    purser
!   2008-04-28  safford - add stander module documentation block
!
! subroutines included:
!   setq
!   lagw
!   infit
!
! variable definitions:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

!============================================================================
use kinds, only: i_kind
use kinds, only: r_kind => r_double
use constants, only: zero,one,two,three,five
implicit none

! set default to private
  private
! set subroutines to public
  public :: setq
  public :: lagw
  public :: infit
! set passed variables to public
  public :: qco,dco,ldsig4,ldsig,nohp,sigc,nohm,no,sigb,noh,ico,wt,dwt
  public :: nop,hunit2,nom,nnit,rcrit,q,hunit,dwt1,wt1,q1,hunit1

integer(i_kind),parameter         :: noh=3,    nohm=noh-1,   nohp=noh+1,&
                                     no=noh*2, nom=no-1,     nop=no+1,   nnit=7
real(r_kind),parameter            :: sigc=three,  sigb=two
real(r_kind),dimension(no)        :: hunit,q,wt,dwt
real(r_kind),dimension(nom)       :: hunit1,hunit2,q1,wt1,dwt1
real(r_kind),dimension(-noh:noh)  :: qco
real(r_kind),dimension(-1-noh:noh):: ico,dco
real(r_kind)                      :: rcrit,ldsig,ldsig4
!============================================================================

contains


!============================================================================
SUBROUTINE setq(q,x,n) 
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    setq
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:      Precompute the N constant denominator factors of the 
!                N-point Lagrange polynomial interpolation formula.
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     X -   The N abscissae.
!     N -   The number of points involved.
!
!   output argument list:
!     Q -   The N denominator constants.
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_kind),          INTENT(in   ) :: n
  REAL(r_kind),DIMENSION(n),INTENT(in   ) :: x
  REAL(r_kind),DIMENSION(n),INTENT(  out) :: q
!-----------------------------------------------------------------------------
  INTEGER(i_kind)                         :: i,j
!=============================================================================
DO i=1,n
   q(i)=one
   DO j=1,n
      IF(j /= i)q(i)=q(i)/(x(i)-x(j))
   ENDDO
ENDDO
END SUBROUTINE setq 


!============================================================================
SUBROUTINE lagw(x,xt,q,w,dw,n) 
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    lagw
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:      Construct the Lagrange weights and their derivatives when 
!                target abscissa is known and denominators Q have already 
!                been precomputed
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     X   - Grid abscissae
!     XT  - Target abscissa
!     Q   - Q factors (denominators of the Lagrange weight formula)
!     N   - Number of grid points involved in the interpolation
!
!   output argument list:
!     W   - Lagrange weights
!     DW  - Derivatives, dW/dX, of Lagrange weights W
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

  IMPLICIT NONE

  INTEGER(i_kind),          INTENT(in   ) :: n
  REAL(r_kind),             INTENT(in   ) :: xt
  REAL(r_kind),DIMENSION(n),INTENT(in   ) :: x,q
  REAL(r_kind),DIMENSION(n),INTENT(  out) :: w,dw
!-----------------------------------------------------------------------------
  REAL(r_kind),DIMENSION(n)               :: sdit,d,di
  INTEGER(i_kind)                         :: i,j
  REAL(r_kind)                            :: p,s,sdil,sdir
!============================================================================
p=one      ! ...will become product of all the d(i)=xt-x(i)
DO i=1,n
   d(i)=xt-x(i)
   p=p*d(i)
ENDDO

!   test p to reveal whether any of the d(i) vanish:
IF(p == zero)THEN   ! xt coincides with a grid point - use special code:
   p=one            ! p will become the product of the nonzero d(i),
   s=zero           ! s will become the corresponding sum of q(i)/d(i)
   DO i=1,n
      IF(d(i) == zero)THEN
         j=i            ! identify the grid index corresponding to present xt
         w(j)=one       ! interpolation weighted entirely to this one.
      ELSE
         w(i)=zero
         p=p*d(i)
         dw(i)=q(i)/d(i)
         s=s+dw(i)
      ENDIF
   ENDDO
   dw(j)=-s*p
   DO i=1,n
      IF(i /= j)dw(i)=dw(i)*p
   ENDDO
ELSE             ! xt is not a grid point - use generic code:
   sdil=zero            ! will become the sum of terms to the left.
   sdir=zero            ! will become the sum of terms to the right.
   DO i=1,n
      di(i)=one/d(i)
      sdit(i)=sdil
      sdil=sdil+di(i)
      w(i)=q(i)*p*di(i)
   ENDDO
   DO i=n,1,-1
      sdit(i)=sdit(i)+sdir
      sdir=sdir+di(i)
      dw(i)=w(i)*sdit(i)
   ENDDO
ENDIF
END SUBROUTINE lagw 



!============================================================================
subroutine infit
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    infit
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!
!   output argument list:
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

implicit none

integer(i_kind) :: i
real(r_kind)    :: divq,divd
!============================================================================
! Initialize quantities that relate to interpolations:
do i=1,no; hunit(i)=i-noh; enddo
hunit1=hunit(:nom)    ; hunit2=hunit(2:)
call setq(q,hunit,no) ; call setq(q1,hunit1,nom)
rcrit=SQRT(EPSILON(one))
!------------------------------------
! Initialize coefficients for quadrature, differencing and mdpt interpolation:
divq=967680_r_kind        ; divd=1024_r_kind
qco(0)=862564_r_kind/divq ; dco(0)=1225_r_kind/divd     ; ico(0)=1225_r_kind/(2*divd)
qco(1)= 57249_r_kind/divq ; dco(1)=-245_r_kind/(3*divd) ; ico(1)=-245_r_kind/(2*divd)
qco(2)= -5058_r_kind/divq ; dco(2)=  49_r_kind/(5*divd) ; ico(2)=  49_r_kind/(2*divd)
qco(3)=   367_r_kind/divq ; dco(3)=      -five/(7*divd) ; ico(3)=      -five/(2*divd)
qco(-1:-noh:-1)  = qco(1:noh) ! complete the stencil of quadrature coeffs.
dco(-1:-nohp:-1) =-dco(0:noh) ! complete the stencil of difference coeffs
ico(-1:-nohp:-1) = ico(0:noh) ! complete the stencil of interpolation coeffs.
!------------------------------------
! Initial coefficients related to control of working grid resolution:
ldsig =log(sigc/sigb)
ldsig4=ldsig**4
end subroutine infit
end module module_fitcons



!=============================================================================
subroutine coefrf(sig,nu,n,m,bnf,lnf)
!=============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    coefrf
!
!   prgrmmr:     R.J.Purser, NCEP 2001
!
! abstract:    
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     n, m    -
!     sig, nu - 
!
!   output argument list:
!     bnf     -
!     lnf     -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use module_pmat2
use kinds, only: i_kind
use kinds, only: r_kind => r_double
use constants, only: zero,half,one
implicit none

integer(i_kind),              intent(IN   ) :: n,m
real(r_kind), dimension(n),   intent(IN   ) :: sig,nu
real(r_kind), dimension(n),   intent(  OUT) :: bnf
real(r_kind), dimension(m,n), intent(  OUT) :: lnf
!-------------------------------------------------------------------------- 
integer(i_kind), parameter                  :: irmax=6
real(r_kind), dimension(n,-m:m)             :: s
real(r_kind), dimension(n,-m:0)             :: sl
real(r_kind), dimension(n,-m:m,m)           :: k,l
real(r_kind), dimension(n)                  :: eta
real(r_kind), dimension(irmax)              :: bcofi,bcofh
integer(i_kind)                             :: i,i1,il,ir,ik
!--------------------------------------------------------------------------
! The coefficients bcofi are the reciprocals of the i=1 entries of TABLE 1
! of NCEP O.N. 431:
data bcofi/one, 12._r_kind, 90._r_kind, 560._r_kind, 3150._r_kind, 16632._r_kind/
!=============================================================================
bcofh=half/bcofi
do i=1,n
   eta(i)=sig(i)*sqrt(nu(i))
enddo
k=zero
!-------------------------------------------------------------------------
! Set k(:, -1:1, 1) to be the K-matrix of (4.8)--(4.10) of NCEP O.N. 431: 
!--------------------------------------------------------------------------
do i=1,n-1
   k(i  , 0,1)=k(i  ,0,1)+eta(i+1)/eta(i  )
   k(i+1, 0,1)=k(i+1,0,1)+eta(i  )/eta(i+1)
   k(i  , 1,1)=-one
   k(i+1,-1,1)=-one
enddo

!-------------------------------------------------------------------------
! Set k(:, : , ir) to be the original K-matrix raised to the power of (ir):
!--------------------------------------------------------------------------
do ir=2,m
   il=ir-1
   call mulbb(k(:,-1:1,1),k(:,-il:il,il),k(:,-ir:ir,ir),n,n,1,1,il,il,ir,ir)
enddo

!-------------------------------------------------------------------------
! Pre- and post-multiply each of the m powers of K by the diagonal matrix,
! sigma, of NCEP O.N. 431, where the elements of sigma measure the smoothing
! scale of the quasi-Gaussian filter in grid-space units.
! Also, multiply each of the resulting banded matrices by .5*b_{1,ir} for
! the appropriate index, ir, corresponding to the power by which the original
! K was raised.
!--------------------------------------------------------------------------
do ir=1,m
   call mulbd(k(:,-ir:ir,ir),sig,k(:,-ir:ir,ir),n,n,ir,ir)
   call muldb(sig,k(:,-ir:ir,ir),k(:,-ir:ir,ir),n,n,ir,ir)
   k(:,-ir:ir,ir)=k(:,-ir:ir,ir)*bcofh(ir)
enddo


s=zero
s(:,0)=one

do ir=1,m
   l(:,-ir:ir,ir)=k(:,-ir:ir,ir)
   s(:,-ir:ir)=s(:,-ir:ir)+l(:,-ir:ir,ir)
enddo
do i1=2,m
   do ir=m,i1,-1
      l(:,-ir:ir,ir)=zero
      do ik=1,ir-i1+1
         il=ir-ik
         call madbb(k(:,-ik:ik,ik),l(:,-il:il,il),l(:,-ir:ir,ir), &
              n,n,ik,ik,il,il,ir,ir)
      enddo
      l(:,-ir:ir,ir)=l(:,-ir:ir,ir)/i1
      s(:,-ir:ir)=s(:,-ir:ir)+l(:,-ir:ir,ir)
   enddo
enddo
call ldlb(s,sl,bnf,n,m)
do i1=1,m
   do i=1,n
      lnf(i1,i)=sl(i,-i1)
   enddo
enddo
end subroutine coefrf


!============================================================================
subroutine ldlb1i(nol,lnf,bnf,                                              &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb1i
!
!   prgrmmr:     
!
! abstract:    
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol      -
!     ims, ime -
!     its, ite -
!     bnf      -
!     lnf      -
!
!   output argument list:
!     bnf      -
!     lnf      -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime
  INTEGER(i_kind), INTENT(IN   ) :: its,ite

  REAL(r_kind), DIMENSION(ims:ime),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do i=its,ite
   nola=min(nol,i-its)
   do l=nola,1,-1
      s=lnf(l,i)
      do m=l+1,nola
         s=s-lnf(m,i)*bnf(i-m)*lnf(m-l,i-l)
      enddo
      lnf(l,i)=s/bnf(i-l)
   enddo
   s=bnf(i)
   do l=1,nola
      s=s-lnf(l,i)**2*bnf(i-l)
   enddo
   bnf(i)=s
enddo
end subroutine ldlb1i

   
!============================================================================
subroutine ldlb2i(nol,lnf,bnf,                                              &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb2i
!
!   prgrmmr:     
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol                -
!     ims, ime, jms, jme -
!     its, ite, jts, jte -
!     bnf                -
!     lnf                -
!
!   output argument list:
!     bnf                -
!     lnf                -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do j=jts,jte
   do i=its,ite
      nola=min(nol,i-its)
      do l=nola,1,-1
         s=lnf(l,i,j)
         do m=l+1,nola
            s=s-lnf(m,i,j)*bnf(i-m,j)*lnf(m-l,i-l,j)
         enddo
         lnf(l,i,j)=s/bnf(i-l,j)
      enddo
      s=bnf(i,j)
      do l=1,nola
         s=s-lnf(l,i,j)**2*bnf(i-l,j)
      enddo
      bnf(i,j)=s
   enddo
enddo
end subroutine ldlb2i

   
!============================================================================
subroutine ldlb2j(nol,lnf,bnf,                                              &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb2
!
!   prgrmmr:     
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     bnf              -
!     lnf              -
! 
!   output argument list:
!     bnf              -
!     lnf              -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do j=jts,jte
   nola=min(nol,j-jts)
   do i=its,ite
      do l=nola,1,-1
         s=lnf(l,i,j)
         do m=l+1,nola
            s=s-lnf(m,i,j)*bnf(i,j-m)*lnf(m-l,i,j-l)
         enddo
         lnf(l,i,j)=s/bnf(i,j-l)
      enddo
      s=bnf(i,j)
      do l=1,nola
         s=s-lnf(l,i,j)**2*bnf(i,j-l)
      enddo
      bnf(i,j)=s
   enddo
enddo
end subroutine ldlb2j

   
!============================================================================
subroutine ldlb3i(nol,lnf,bnf,                                              &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb3i
!
!   prgrmmr:    
!
! abstract:    
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do j=jts,jte
   do k=kts,kte
      do i=its,ite
         nola=min(nol,i-its)
         do l=nola,1,-1
            s=lnf(l,i,k,j)
            do m=l+1,nola
               s=s-lnf(m,i,k,j)*bnf(i-m,k,j)*lnf(m-l,i-l,k,j)
            enddo
            lnf(l,i,k,j)=s/bnf(i-l,k,j)
         enddo
         s=bnf(i,k,j)
         do l=1,nola
            s=s-lnf(l,i,k,j)**2*bnf(i-l,k,j)
         enddo
         bnf(i,k,j)=s
      enddo
   enddo
enddo
end subroutine ldlb3i

   
!============================================================================
subroutine ldlb3j(nol,lnf,bnf,                                              &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    ldlb3j
!
!   prgrmmr:     R.J.Purser, National Meteorological Center, 1994
!
! abstract:     
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(INOUT) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,m,nola
  real(r_kind)                   :: s
!============================================================================
do j=jts,jte
   nola=min(nol,j-jts)
   do k=kts,kte
      do i=its,ite
         do l=nola,1,-1
            s=lnf(l,i,k,j)
            do m=l+1,nola
               s=s-lnf(m,i,k,j)*bnf(i,k,j-m)*lnf(m-l,i,k,j-l)
            enddo
            lnf(l,i,k,j)=s/bnf(i,k,j-l)
         enddo
         s=bnf(i,k,j)
         do l=1,nola
            s=s-lnf(l,i,k,j)**2*bnf(i,k,j-l)
         enddo
         bnf(i,k,j)=s
      enddo
   enddo
enddo
end subroutine ldlb3j

   
SUBROUTINE hbnrf1i(a,nol,lnf,bnf,                                           &
       ims,ime,                                                             &
       its,ite                                                              )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf1i
!
!   prgrmmr:    
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                1-dimensional, active index i
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol     -
!     ims,ime -
!     its,ite -
!     a       -
!     bnf     -
!     lnf     -
!
!   output argument list:
!     a       -
!     bnf     -
!     lnf     -
!
! attributes:
!   language:  f90
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime
  INTEGER(i_kind), INTENT(IN   ) :: its,ite

  REAL(r_kind), DIMENSION(ims:ime),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,l,nola
!============================================================================
DO i=its+1,ite
   nola=MIN(nol,i-its)
   DO l=1,nola
      a(i)=a(i)-lnf(l,i)*a(i-l)
   ENDDO
ENDDO
DO i=its,ite
   a(i)=bnf(i)*a(i)
ENDDO
DO i=ite-1,its,-1
   nola=MIN(nol,ite-i)
   DO l=1,nola
      a(i)=a(i)-lnf(l,i+l)*a(i+l)
   ENDDO
ENDDO
END SUBROUTINE hbnrf1i


SUBROUTINE hbnrf2i(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf2i
!
!   prgrmmr:   
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                2-dimensional, active index i
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     a                -
!     bnf              -
!     lnf              -
!
!   output argument list:
!     a                -
!     bnf              -
!     lnf              -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,l,nola
!============================================================================
DO j=jts,jte
   DO i=its+1,ite
      nola=MIN(nol,i-its)
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j)*a(i-l,j)
      ENDDO
   ENDDO
   DO i=its,ite
      a(i,j)=bnf(i,j)*a(i,j)
   ENDDO
   DO i=ite-1,its,-1
      nola=MIN(nol,ite-i)
      DO l=1,nol
         a(i,j)=a(i,j)-lnf(l,i+l,j)*a(i+l,j)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf2i


SUBROUTINE hbnrf2j(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme,                                                    &
       its,ite, jts,jte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf1i
!
!   prgrmmr:   
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                2-dimensional, active index j
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     a                -
!     bnf              -
!     lnf              -
!
!   output argument list:
!     a                -
!     bnf              -
!     lnf              -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,l,nola
!============================================================================
DO j=jts+1,jte
   nola=MIN(nol,j-jts)
   DO i=its,ite
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j)*a(i,j-l)
      ENDDO
   ENDDO
ENDDO
DO j=jts,jte
   DO i=its,ite
      a(i,j)=bnf(i,j)*a(i,j)
   ENDDO
ENDDO
DO j=jte-1,jts,-1
   nola=MIN(nol,jte-j)
   DO i=its,ite
      DO l=1,nola
         a(i,j)=a(i,j)-lnf(l,i,j+l)*a(i,j+l)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf2j


SUBROUTINE hbnrf3i(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf3i
!
!   prgrmmr:   
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                3-dimensional, active index i
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     a                         -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     a                         -
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,nola
!============================================================================
DO j=jts,jte
   DO k=kts,kte
      DO i=its+1,ite
         nola=MIN(nol,i-its)
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i-l,k,j)
         ENDDO
      ENDDO
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
      DO i=ite-1,its,-1
         nola=MIN(nol,ite-i)
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i+l,k,j)*a(i+l,k,j)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf3i


SUBROUTINE hbnrf3j(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnrf3j
!
!   prgrmmr:   
!
! abstract:      Horizontal basic inhomogeneous recursive filter, 
!                3-dimensional, active index j
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     a                         -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     a                         -
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,nola
!============================================================================
DO j=jts+1,jte
   nola=MIN(nol,j-jts)
   DO k=kts,kte
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i,k,j-l)
         ENDDO
      ENDDO
   ENDDO
ENDDO
DO j=jts,jte
   DO k=kts,kte
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
   ENDDO
ENDDO
DO j=jte-1,jts,-1
   nola=MIN(nol,jte-j)
   DO k=kts,kte
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j+l)*a(i,k,j+l)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE hbnrf3j


SUBROUTINE vbnrf1k(a,nol,lnf,bnf,                                           &
       kms,kme,                                                             &
       kts,kte                                                              )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vbnrf1k
!
!   prgrmmr:   
!
! abstract:      Vertical bounded grid inhomogeneous recursive filter, 
!                1-dimensional, active index k
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol     -
!     kms,kme -
!     kts,kte -
!     a       -
!     bnf     -
!     lnf     -
!
!   output argument list:
!     a       -
!     bnf     -
!     lnf     -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: kts,kte

  REAL(r_kind), DIMENSION(kms:kme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(kms:kme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, kms:kme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: k,l,nola
!============================================================================
DO k=kts+1,kte
   nola=MIN(nol,k-kts)
   DO l=1,nola
      a(k)=a(k)-lnf(l,k)*a(k-l)
   ENDDO
ENDDO
DO k=kts,kte
   a(k)=bnf(k)*a(k)
ENDDO
DO k=kte-1,kts,-1
   nola=MIN(nol,kte-k)
   DO l=1,nola
      a(k)=a(k)-lnf(l,k+l)*a(k+l)
   ENDDO
ENDDO
END SUBROUTINE vbnrf1k


SUBROUTINE vbnrf2k(a,nol,lnf,bnf,                                           &
       ims,ime, kms,kme,                                                    &
       its,ite, kts,kte                                                     )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vbnrf2k
!
!   prgrmmr:   
!
! abstract:      Vertical bounded grid inhomogeneous recursive filter, 
!                2-dimensional, active index k
!
! program history log:
!   2008-04-25  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, kms,kme -
!     its,ite, kts,kte -
!     a                -
!     bnf              -
!     lnf              -
!
!   output argument list:
!     a                -
!     bnf              -
!     lnf              -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,k,l,nola
!============================================================================
DO k=kts+1,kte
   nola=MIN(nol,k-kts)
   DO i=its,ite
      DO l=1,nola
         a(i,k)=a(i,k)-lnf(l,i,k)*a(i,k-l)
      ENDDO
   ENDDO
ENDDO
DO k=kts,kte
   DO i=its,ite
      a(i,k)=bnf(i,k)*a(i,k)
   ENDDO
ENDDO
DO k=kte-1,kts,-1
   nola=MIN(nol,kte-k)
   DO i=its,ite
      DO l=1,nola
         a(i,k)=a(i,k)-lnf(l,i,k+l)*a(i,k+l)
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE vbnrf2k


SUBROUTINE vbnrf3k(a,nol,lnf,bnf,                                           &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    vbnrf3k
!
!   prgrmmr:   
!
! abstract:      Vertical bounded grid inhomogeneous recursive filter, 
!                3-dimensional, active index k
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     a                         -
!     bnf                       -
!     lnf                       -
!
!   output argument list:
!     a                         -
!     bnf                       -
!     lnf                       -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: bnf
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnf
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k,l,nola
!============================================================================
DO j=jts,jte
   DO k=kts+1,kte
      nola=MIN(nol,k-kts)
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k,j)*a(i,k-l,j)
         ENDDO
      ENDDO
   ENDDO
   DO k=kts,kte
      DO i=its,ite
         a(i,k,j)=bnf(i,k,j)*a(i,k,j)
      ENDDO
   ENDDO
   DO k=kte-1,kts,-1
      nola=MIN(nol,kte-k)
      DO i=its,ite
         DO l=1,nola
            a(i,k,j)=a(i,k,j)-lnf(l,i,k+l,j)*a(i,k+l,j)
         ENDDO
      ENDDO
   ENDDO
ENDDO
END SUBROUTINE vbnrf3k


SUBROUTINE hbncij(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,                           &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbncij 
!
!   prgrmmr:   
!
! abstract:      
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     hamp,bnfi,bnfj   -
!     lnfi,lnfj        -
!     a                -
!
!   output argument list:
!     a                -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(IN   ) :: hamp,bnfi,bnfj
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(IN   ) :: lnfi,lnfj
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j
!============================================================================
DO j=jts,jte
   DO i=its,ite
      a(i,j)=hamp(i,j)*a(i,j)
   ENDDO
ENDDO
!---------------
CALL hbnrf2i(a,nol,lnfi,bnfi,             &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
CALL hbnrf2j(a,nol,lnfj,bnfj,             &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
END SUBROUTINE hbncij


SUBROUTINE hbncji(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,                           &
     ims,ime, jms,jme,                                                      &
     its,ite, jts,jte                                                       )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbncji
!
!   prgrmmr:   
!
! abstract:     
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol              -
!     ims,ime, jms,jme -
!     its,ite, jts,jte -
!     hamp,bnfi,bnfj   -
!     lnfi,lnfj        -
!     a                -
!
!   output argument list:
!     a                -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte

  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, jms:jme),                       &
                   INTENT(IN   ) :: hamp,bnfi,bnfj
  REAL(r_kind), DIMENSION(nol, ims:ime, jms:jme),                  &
                   INTENT(IN   ) :: lnfi,lnfj
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j
!============================================================================
CALL hbnrf2j(a,nol,lnfj,bnfj,             &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!----------
CALL hbnrf2i(a,nol,lnfi,bnfi,             &
     ims,ime, jms,jme,                    &
     its,ite, jts,jte)
!---------------
DO j=jts,jte
   DO i=its,ite
      a(i,j)=hamp(i,j)*a(i,j)
   ENDDO
ENDDO
!---------------
END SUBROUTINE hbncji


SUBROUTINE hbncijk(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,lnfk,bnfk,                &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbncijk
!
!   prgrmmr:   
!
! abstract: 
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     hamp,bnfi,bnfj,bnfk       -
!     lnfi,lnfj,lnfk            -
!     a                         -
!
!   output argument list:
!     a                         -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: hamp,bnfi,bnfj,bnfk
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnfi,lnfj,lnfk
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k
!============================================================================
DO j=jts,jte
   do k=kts,kte
      DO i=its,ite
         a(i,k,j)=hamp(i,k,j)*a(i,k,j)
      ENDDO
   enddo
ENDDO
!---------------
CALL hbnrf3i(a,nol,lnfi,bnfi,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3j(a,nol,lnfj,bnfj,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
call vbnrf3k(a,nol,lnfk,bnfk,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
END SUBROUTINE hbncijk


SUBROUTINE hbnckji(a,hamp,nol,lnfi,bnfi,lnfj,bnfj,lnfk,bnfk,                &
       ims,ime, jms,jme, kms,kme,                                           &
       its,ite, jts,jte, kts,kte                                            )
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    hbnckji
!
!   prgrmmr:   
!
! abstract:
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     nol                       -
!     ims,ime, jms,jme, kms,kme -
!     its,ite, jts,jte, kts,kte -
!     hamp,bnfi,bnfj,bnfk       -
!     lnfi,lnfj,lnfk            -
!     a                         -
!
!   output argument list:
!     a                         -
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block
  use kinds, only: i_kind
  use kinds, only: r_kind => r_double
  IMPLICIT NONE

  INTEGER(i_kind), INTENT(IN   ) :: nol
  INTEGER(i_kind), INTENT(IN   ) :: ims,ime, jms,jme, kms,kme
  INTEGER(i_kind), INTENT(IN   ) :: its,ite, jts,jte, kts,kte

  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(INOUT) :: a
  REAL(r_kind), DIMENSION(ims:ime, kms:kme, jms:jme),                       &
                   INTENT(IN   ) :: hamp,bnfi,bnfj,bnfk
  REAL(r_kind), DIMENSION(nol, ims:ime, kms:kme, jms:jme),                  &
                   INTENT(IN   ) :: lnfi,lnfj,lnfk
!----------------------------------------------------------------------------
  INTEGER(i_kind)                :: i,j,k
!============================================================================
call vbnrf3k(a,nol,lnfk,bnfk,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3j(a,nol,lnfj,bnfj,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!----------
CALL hbnrf3i(a,nol,lnfi,bnfi,             &
       ims,ime, jms,jme, kms,kme,         &
       its,ite, jts,jte, kts,kte)
!---------------
DO j=jts,jte
   do k=kts,kte
      DO i=its,ite
         a(i,k,j)=hamp(i,k,j)*a(i,k,j)
      ENDDO
   enddo
ENDDO
!---------------
END SUBROUTINE hbnckji


!============================================================================
subroutine rfit(ng,sig,nu, ns,nw,ssig,snu,ins1,wts)  
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    rfit
!
!   prgrmmr:     R. J. Purser, NCEP 2001
!
! abstract:     
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     ng
!     sig,nu
!     ins1
!     wts
!
!   output argument list:
!     ins1
!     wts
!     ns,nw
!     ssig,snu
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: i_kind
use kinds, only: r_kind => r_double
use constants, only: zero,one
use module_fitcons
implicit none

integer(i_kind),                  intent(IN   ) :: ng
real(r_kind)   , dimension(ng),   intent(IN   ) :: sig,nu
integer(i_kind),                  intent(  OUT) :: ns,nw
real(r_kind)   , dimension(ng),   intent(  OUT) :: ssig,snu
integer(i_kind), dimension(ng),   intent(INOUT) :: ins1
real(r_kind)   , dimension(no,ng),intent(INOUT) :: wts
!----------------------------------------------------------------------------
integer(i_kind)                                :: i,i1,im,k,l,is
real(r_kind)                                   :: t
real(r_kind)   , dimension(-nohm:ng+noh)       :: dcdg
real(r_kind)   , dimension(-noh:ng+noh)        :: cofg,cofs
real(r_kind)   , dimension(ng)                 :: dsdg,dhdg
!============================================================================
nw=0
do i=1,ng
   dcdg(i)=one/sig(i)
   if(sig(i) <= sigb)then
!----------------------------------------------------------------------------
! sig(i) below threshold; cleave to original grid spacing with ds/dg and 
! dh/dg set accordingly:
!----------------------------------------------------------------------------
      dsdg(i)=one     ;      dhdg(i)=zero
   else
!----------------------------------------------------------------------------
! sig(i) exceeds basic threshold sigb, allowing working grid with coordinate
! s to differ from original grid with coordinate g. The formula for ds/dg
! is now <1 but tends smoothly to 1 again at the threshold value, sig=sigb.
! [The function for log(ds/dg) is based on the "hyper-hyperbola":
!    y= (1+x**4)**(-4)-1, which rises very gradually from its base at x=y=0]
! Likewise, the perturbative component, dh/dg, is now < 0, but tends
! smoothly to 0 again at the threshold.
!----------------------------------------------------------------------------
      t=ldsig-sqrt(sqrt(ldsig4+(ldsig-log(sigc*dcdg(i)))**4))
      dsdg(i)=exp(t) ;      dhdg(i)=dsdg(i)*t
   endif
enddo

!----------------------------------------------------------------------------
! Apply mirror-symmetry to extrapolate beyond ends:
!----------------------------------------------------------------------------
do l=1,noh
   dcdg(1-l)=dcdg(l); dcdg(ng+l)=dcdg(ng+1-l)
enddo

!----------------------------------------------------------------------------
! Integrate dc/dg wrt g to get c(g) at each of the points of the g-grid
! which is NOT staggered relative to the boundary
!----------------------------------------------------------------------------
cofg(0)=zero
do i=1,ng
   cofg(i)=cofg(i-1)+dot_product(qco,dcdg(i-noh:i+noh))
enddo
do l=1,noh
   cofg(  -l)=-cofg(   l)
   cofg(ng+l)=-cofg(ng-l)+2*cofg(ng)
enddo

im=0
ns=0
!----------------------------------------------------------------------------
! loop over noncontiguous segments where it is numerically beneficial
! to employ a grid of relatively coarse resolution. The adoption of each
! alternative grid segment is subject to some conditions:
! 1) Each coarse-grid segment must span at least 5 points of the original grid
! 2) Each segment must shorten the tally of working grid points by at least 3.
!     Subject to the above conditions, the coarse grid is blended smoothly
! with the original grid at internal thresholds and is designed to provide
! a resolution such that the smoothing scale, sigma, never exceeds the 
! product, sigc*dg/ds, where sigc is a dimensionless parameter (e.g. sigc=3.)
! and dg/ds is the local working grid (s) spacing in units of the original
! grid (g) spacing. 
!
! Each segment found is defined by its end points in the original grid,
! i1 and im. k is the counter for segments along this line.
! ns keeps count of the number of working grid (s-grid) points found so far.
!----------------------------------------------------------------------------
cofs(0)=zero
do k=1,ng 
   do i1=im+1,ng
      if(i1< ng-3 .and. dhdg(i1) /= zero)exit
!----------------------------------------------------------------------------
! working s-grid continues to track the original g-grid; Set indices and 
! weight for the trivial "interpolation" between these coincident grids:
!----------------------------------------------------------------------------
      ns=ns+1
      ins1(i1)=-ns
      cofs(ns)=cofg(i1)
   enddo
   if(i1 > ng)exit
!----------------------------------------------------------------------------
! Having met the basic conditions for the start of a new segment in which
! the s-grid and g-grids may part company, seek the other end, im, of this
! possible segment:
!----------------------------------------------------------------------------
   do im=i1+1,ng
      if(dhdg(im) == zero)exit
   enddo
   im=im-1
   if(im < i1+4)then
!----------------------------------------------------------------------------
! Segment too short to be viable; keep s-grid and g-grids synchronized:
!----------------------------------------------------------------------------
      do i=i1,im
         ns=ns+1
         ins1(i)=-ns
         cofs(ns)=cofg(i)
      enddo
   else
!----------------------------------------------------------------------------
! Segment long enough to be potentially viable. Call jfit to determine if 
! the final condition is met, namely that the number of s-grid points 
! in this segment is smaller than the g-grid tally by at least 3. If so,
! Fit an exact integer number of s-points into this segment and compute
! the indices and weights for the associated nontrivial interpolation
! from these (and neighboring) s-points back to the g-points of the segment:
!----------------------------------------------------------------------------
      call jfit(ng,i1,im,ns,nw,cofg,dsdg,dhdg,cofs,ins1,wts)
      if(ns<0) return
   endif
enddo
if(ns<no .and. nw>0)then ! <- s-grid too short; use copy of g-grid instead
   wts(:,1:nw)=zero
   nw=0
   do i=1,ng
      ins1(i)=-i
      cofs(i)=cofg(i)
   enddo
   ns=ng
endif

do l=1,noh
   cofs(  -l)=-cofs(   l)
   cofs(ns+l)=-cofs(ns-l)+2*cofs(ns)
enddo
do is=1,ns
   ssig(is)=one/dot_product(dco,cofs(is-nohp:is+noh))
enddo

!----------------------------------------------------------------------------
! By applying adjoint-interpolation to the g-grid metric terms, obtain
! the corresponding metric terms for the new s-grid:
!----------------------------------------------------------------------------
call stogt(ns,ng,ins1,wts, snu,nu)

end subroutine rfit


!============================================================================
subroutine jfit(ng,ig1,igm,ns,iw,cofg,dsdg,dhdg,cofs,ins1,wts)
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    jfit
!
!   prgrmmr:     R. J. Purser, NCEP 2001
!
! abstract: 
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block, rm unused vars
!
!   input argument list:
!     ng,ig1,igm
!     ns,iw
!     dsdg,dhdg
!     cofg
!     cofs
!     ins1
!     wts
!
!   output argument list:
!     ns,iw
!     cofs
!     ins1
!     wts
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: i_kind
use kinds, only: r_kind => r_double
use constants, only: zero,half,one,two
use module_fitcons
implicit none

integer(i_kind),                         intent(IN   ) :: ng,ig1,igm
integer(i_kind),                         intent(INOUT) :: ns,iw
real(r_kind)   , dimension(ng),          intent(IN   ) :: dsdg,dhdg
real(r_kind)   , dimension(-noh:ng+noh), intent(IN   ) :: cofg
real(r_kind)   , dimension(-noh:ng+noh), intent(INOUT) :: cofs
integer(i_kind), dimension(ng),          intent(INOUT) :: ins1
real(r_kind)   , dimension(no,ng),       intent(INOUT) :: wts
!----------------------------------------------------------------------------
real(r_kind)   , dimension(-noh:ng+noh)               :: sofg,dsdgt
real(r_kind)                                          :: et,estar,destar,r,dr,sm
integer(i_kind)                                       :: i,l,ie,iep,ie1,ien,ig0,is0,ism,init
!============================================================================

!----------------------------------------------------------------------------
! Form the definite integral sm, of ds/dg, within this segment:
!----------------------------------------------------------------------------
sm=sum(dsdg(ig1:igm)) 

!---------------------------------------------------------------------------
! Test whether it is worthwhile to allow s-grid to deviate from the original
! g-grid within this segment on the basis of the number of grid points that
! could potentially be eliminated (we require a saving > 3 per segment):
!---------------------------------------------------------------------------
if(sm > igm-ig1-2)then
!----------------------------------------------------------------------------
! This putative s-grid segment reduces the total number of grid points by an
! insufficient amount to justify the additional interpolations. Therefore,
! keep the original g-grid instead for this segment, and return:
!---------------------------------------------------------------------------
   do i=ig1,igm
      ns=ns+1
      ins1(i)=-ns
      cofs(ns)=cofg(i)
   enddo
   return
endif
!----------------------------------------------------------------------------
! s-grid segment achieves a worthwhile reduction of the number of points
! of the working grid. The tasks of the rest of this routine are to:
! (1) adjust the segment length in the s-metric to make it an integer;
! (2) find the s-coordinates of each g-grid points in this segment
!     and hence the nontrivial interpolation indices and weights required 
!     to go from the s-grid to the g-grid (or adjoints going the other way);
! (3) use Newton iterations to find the accurate interpolation formulae
!     that enable c(s) to be interpolated from the given c(g).
!----------------------------------------------------------------------------
ig0=ig1-1
is0=ns; ism=sm
!----------------------------------------------------------------------------
! Fractional remainder of sm, divided by the definite integral of dh/dg
! provides the adjustment factor that scales the perturbative component,
! dhdg, by exactly the amount that will make the segment integral of the 
! perturbed grid-to-grid jacobian, dsdgt, the exact integer, ism:
!----------------------------------------------------------------------------
r=(sm-ism)/sum(dhdg(ig1:igm))
do i=ig1,igm
   dsdgt(i)=dsdg(i)-r*dhdg(i)
enddo
!----------------------------------------------------------------------------
! Mirror-extrapolate adjusted ds/dg as an even-symmetry function at the 
! ends of this segment. Note that the grid on which derivatives such as
! ds/dg reside is the one staggered wrt domain boundaries and segment
! end points. The indices of this grid go from ig1 to igm inside the
! segment. (The convention for the companion grid, NOT staggered wrt 
! boundaries, is such that the two segment ends are denoted by indices,
! ig0=ig1-1 and igm.)
!----------------------------------------------------------------------------
do l=1,noh
   dsdgt(ig1-l)=dsdgt(ig0  +l)
   dsdgt(igm+l)=dsdgt(igm+1-l)
enddo
ism=is0+ism ! This integer also becomes (within round-off) the value, sofg(igm)
!----------------------------------------------------------------------------
! Set s(g) at both ends of the segment to be the appropriate integers:
!----------------------------------------------------------------------------
sofg(ig0)=is0; sofg(igm)=ism
!----------------------------------------------------------------------------
! Get s(g) inside the segment by performing a numerical quadrature of dsdgt:
!----------------------------------------------------------------------------
do i=ig1,igm
   sofg(i)=sofg(i-1)+dot_product(qco,dsdgt(i-noh:i+noh))
enddo
!----------------------------------------------------------------------------
! Mirror-extrapolate s(g) as an odd-symmetry function at segment end points.
! Note that, being an inegral, s(g) resides on the grid NOT staggered wrt
! boundaries and segment end points.
!----------------------------------------------------------------------------
do l=1,noh
   sofg(ig0-l)=two*is0-sofg(ig0+l)
   sofg(igm+l)=two*ism-sofg(igm-l)
enddo
do i=ig1,igm
   iw=iw+1 ; wts(:,iw)=zero
   r=dot_product(ico,sofg(i-nohp:i+noh))+half
   ie=r            ! Take integer part...
   ins1(i)=ie-nohm ! ...hence the index of the first point in the stencil...
   r=r-ie          ! ...then the fractional part to find interpolation weights:
   call lagw(hunit1,r,q1,wt1,dwt1,nom)   ! weights for left-biased stencil
   wts(:nom,iw) =            (one-r)*wt1 !   bias weight, 1-r
   call lagw(hunit2,r,q1,wt1,dwt1,nom)   ! weights for right-biased stencil
   wts(2:   ,iw) = wts(2:   ,iw)  +r*wt1 !   bias weight, r.
!----------------------------------------------------------------------------
! Exploit the mirror symmetries to confine the weight stencil to the 
! domain interior, even though this may entail padding innermost end of
! the stencil with useless zeroes:
!----------------------------------------------------------------------------
   L=1-INS1(I)
   IF(L > 0)THEN ! FOLD LEFT OVERLAP OF L ELEMENTS BACK INSIDE: 
      WTS(1:L,IW)      =WTS(L:1:-1,IW)+WTS(L+1:L*2,IW) ! FOLD INTO 1ST L
      WTS(L+1:NO-L,IW) =WTS(L*2+1:NO,IW)            ! SHIFT THE REST LEFT
      WTS(NOP-L:NO,IW)=zero ! SET TRAILING L ELEMENTS TO ZERO
      INS1(I)=1          ! RESET INDEX OF FIRST POINT OF STENCIL
   ENDIF
   l=ins1(i)+nom-ism
   if(l > 0)then ! Fold right overlap of L elements back inside:
      wts(nop-l:no,iw)=wts(no:nop-l:-1,iw)+wts(nop-l*2:no-l,iw) ! Fold last L
      wts(l+1:no-l,iw)=wts(1:no-l*2,iw)                         ! Shift right
      wts(1:l,iw)=zero   ! Set first L elements to zero
      ins1(i)=ism-nom    ! reset index of first point of stencil
   endif
enddo
ns=ism

!----------------------------------------------------------------------------
! Use Newton-Raphson iterations to locate the g-coordinates of all this
! segment's s-grid points. Then interpolate the function c to each of
! these s-grid points. (Note that, in the present context, the
! s- and g-grids are the ones NOT staggered wrt the domain boundaries.)
!----------------------------------------------------------------------------
ie=ig0
do i=is0+1,ism-1 ! Loop over s-grid target points interior to this segment
   et=i
!----------------------------------------------------------------------------
! Find the g-grid interval containing this target: 
!----------------------------------------------------------------------------
   do iep=ie+1,igm-1;  if(sofg(iep) > et)exit; enddo
   do ie=iep-1,ig1,-1; if(sofg(ie) <= et)exit; enddo

   ie1=ie-nohm;   ien=ie+noh   ! <-- Set the interpolation stencil range:

   r=(et-sofg(ie))/(sofg(ie+1)-sofg(ie)) ! Linearly estimate interval fraction

!----------------------------------------------------------------------------
! Perform Newton-Raphson iterations to refine interval fraction, r:
!----------------------------------------------------------------------------
   do init=1,nnit
      call lagw(hunit,r,q,wt,dwt,no) ! Get Lagrange weights, wt and d(wt)/dg
      estar =dot_product(wt, sofg(ie1:ien))-et ! <- Residual error, estar.
      destar=dot_product(dwt,sofg(ie1:ien))    ! <- d(estar)/dg.
      dr=-estar/destar                         ! <- Newton correction to r
      r=r+dr                                   ! <- Refined estimate, r
      if(abs(dr) <= rcrit)goto 1               ! <- Converged enough yet?
   enddo
 ! stop 'Too many Newton iterations'           ! <- It never convergenced! 
   write(6,*)' Too many Newton iterations'           ! <- It never convergenced! 
   ns=-1
   return
1  wt=wt+dr*dwt                                ! <- Final refinement to wt
   cofs(i)=dot_product(wt, cofg(ie1:ien))      ! <- Interpolate c(s)
enddo
cofs(ism)=cofg(igm)                            ! <- End value directly
end subroutine jfit


!============================================================================
subroutine stog(ns,ng,ins1,wts, as,ag) 
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    stog
!
!   prgrmmr:     R. J. Purser NCEP 2001
!
! abstract:      Forward interpolation from s-grid to g-grid
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     ns,ng - sizes of s and g grids
!     ins1  - array of 1st stencil indices (s-grid) for each target (g) point.
!     wts   - interpolation weights for each target (g-grid point).
!     as    - s-grid array of source data.
!
!   output argument list:
!     ag    - g-grid array of interpolated target data.
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: i_kind
use kinds, only: r_kind => r_double
use constants, only: zero
implicit none

integer(i_kind), parameter                      :: noh=3,no=noh*2,nom=no-1
integer(i_kind),                  intent(IN   ) :: ns,ng
integer(i_kind), dimension(ng),   intent(IN   ) :: ins1
real(r_kind)   , dimension(no,ng),intent(IN   ) :: wts
real(r_kind)   , dimension(ns),   intent(IN   ) :: as
real(r_kind)   , dimension(ng),   intent(  OUT) :: ag
!----------------------------------------------------------------------------
integer(i_kind)                                 :: i,is,iw
!============================================================================
iw=0
ag=zero
do i=1,ng
   is=ins1(i)
   if(is>0)then
      iw=iw+1
      ag(i)=dot_product(wts(:,iw),as(is:is+nom))
   else
      ag(i)=as(-is)
   endif
enddo
end subroutine stog


!============================================================================
subroutine stogt(ns,ng,ins1,wts, as,ag) 
!============================================================================
!$$$  subprogram documentation block
!                .      .    .
! subprogram:    stogt
!
!   prgrmmr:     R. J. Purser NCEP 2001
!
! abstract:      Perform the transpose of the operation defined by stog
!
! program history log:
!   2008-04-28  safford -- add subprogram doc block
!
!   input argument list:
!     ns,ng
!     ins1
!     wts
!     ag
!
!   output argument list:
!     as
!
! attributes:
!   language:  f90  
!   machine:   ibm RS/6000 SP
!
!$$$ end documentation block

use kinds, only: i_kind
use kinds, only: r_kind => r_double
use constants, only: zero
implicit none

integer(i_kind), parameter                      :: noh=3,no=noh*2,nom=no-1
integer(i_kind),                  intent(IN   ) :: ns,ng
integer(i_kind), dimension(ng),   intent(IN   ) :: ins1
real(r_kind)   , dimension(no,ng),intent(IN   ) :: wts
real(r_kind)   , dimension(ns),   intent(  OUT) :: as
real(r_kind)   , dimension(ng),   intent(IN   ) :: ag
!----------------------------------------------------------------------------
integer(i_kind)                                 :: i,is,iw
!============================================================================
iw=0
as=zero
do i=1,ng
   is=ins1(i)
   if(is>0)then
      iw=iw+1
      as(is:is+nom)=as(is:is+nom)+wts(:,iw)*ag(i)
   else
      as(-is)=as(-is)+ag(i)
   endif
enddo
end subroutine stogt


