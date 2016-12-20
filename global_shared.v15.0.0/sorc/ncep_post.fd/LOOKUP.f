  module  lookup_mod
!
  implicit none
!
  integer,parameter :: ITB=076,JTB=134,ITBQ=152,JTBQ=440 
  real :: PL,THL,RDQ,RDTH,RDP,RDTHE,PLQ,RDPQ,RDTHEQ

  real,dimension(JTB)  :: QS0,SQS
  real,dimension(ITB)  :: THE0,STHE
  real,dimension(ITBQ) :: THE0Q,STHEQ
  real,dimension(ITB,JTB) :: PTBL
  real,dimension(JTB,ITB) :: TTBL
  real,dimension(JTBQ,ITBQ) :: TTBLQ
!
  end module lookup_mod
