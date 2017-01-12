      subroutine intlon(iord,imon,imsk,m1,m2,k1,f1,f2)
      use machine
      implicit none
      integer,intent(in):: iord,imon,imsk,m1,m2
      integer,intent(in):: k1(m1)
      real (kind=kind_dbl_prec),intent(in) :: f1(m1)
      real (kind=kind_dbl_prec),intent(out):: f2(m2)
      integer i2,in,il,ir
      real (kind=kind_dbl_prec) r,x1
      r = real(m1)/real(m2)
      do i2=1,m2
         x1 = (i2-1)*r
         il = int(x1)+1
         ir = mod(il,m1)+1
          if(iord == 2 .and. (imsk == 0 .or. k1(il) == k1(ir))) then
            f2(i2) = f1(il)*(il-x1) + f1(ir)*(x1-il+1)
          else
            in = mod(nint(x1),m1) + 1
            f2(i2) = f1(in)
          endif
      enddo
      end subroutine
