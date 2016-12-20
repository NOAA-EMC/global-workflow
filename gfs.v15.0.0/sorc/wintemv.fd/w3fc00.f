      subroutine w3fc00(u,v,ndir,nspd)
      spd = sqrt(u ** 2 + v ** 2)
      dir = 0.
      if(u.ne.0.or.v.ne.0.) then
        dir = 270. - atan2(v,u) * 180. / 3.1415926535898
      endif
      if(dir.lt.0.) dir = dir + 360.
      if(dir.gt.360.) dir = dir - 360.
      ndir = dir / 10. + .5
      nspd = spd + .5
      return
      end
