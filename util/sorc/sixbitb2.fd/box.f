       subroutine box(ix,iy,il,jl)
c draw a box with lower left corner at specified pixel location
c and upper right corner defined by side length(in characters, not
c pixels)
       dimension kp(2)
       character*1 cv
c       call fonts2
       kp(1)=1
       kp(2)=0
       do 46,k=1,il
       kp(1)=0
       cv=char(65)
c  horizontal bars
       call putlab(ix+20*(k-1),iy,16.,cv,90.0,1,kp,0)
       call putlab(ix+20*(k-1),iy+(jl*20),16.,cv,90.0,1,kp,0)
 46    continue
       do 47,k=1,jl
       kp(1)=0
       cv=char(65)
c vertical bars
       call putlab(ix,iy+20*(k-1),17.,cv,90.0,1,kp,0)
       call putlab(ix+(il*20),iy+20*(k-1),17.,cv,90.0,1,kp,0)
 47    continue
       return
       end
