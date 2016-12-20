 
      subroutine sortrx(n,data,index)
c===================================================================
c
c     sortrx -- sort, real input, index output
c
c
c     input:  n     integer
c             data  real
c
c     output: index integer (dimension n)
c
c this routine performs an in-memory sort of the first n elements of
c array data, returning into array index the indices of elements of
c data arranged in ascending order.  thus,
c
c    data(index(1)) will be the smallest number in array data;
c    data(index(n)) will be the largest number in data.
c
c the original data is not physically rearranged.  the original order
c of equal input values is not necessarily preserved.
c
c===================================================================
c
c sortrx uses a hybrid quicksort algorithm, based on several
c suggestions in knuth, volume 3, section 5.2.2.  in particular, the
c "pivot key" [my term] for dividing each subsequence is chosen to be
c the median of the first, last, and middle values of the subsequence;
c and the quicksort is cut off when a subsequence has 9 or fewer
c elements, and a straight insertion sort of the entire array is done
c at the end.  the result is comparable to a pure insertion sort for
c very short arrays, and very fast for very large arrays (of order 12
c micro-sec/element on the 3081k for arrays of 10k elements).  it is
c also not subject to the poor performance of the pure quicksort on
c partially ordered data.
c
c created:  15 jul 1986  len moss
c
c===================================================================
 
      integer   n,index(n)
      real      data(n)
 
      integer   lstk(31),rstk(31),istk
      integer   l,r,i,j,p,indexp,indext
      real      datap
 
c     quicksort cutoff
c
c     quit quicksort-ing when a subsequence contains m or fewer
c     elements and finish off at end with straight insertion sort.
c     according to knuth, v.3, the optimum value of m is around 9.
 
      integer   m
      parameter (m=9)
 
c===================================================================
c
c     make initial guess for index
 
      do 50 i=1,n
         index(i)=i
 50          continue
 
c     if array is short, skip quicksort and go directly to
c     the straight insertion sort.
 
      if (n.le.m) goto 900
 
c===================================================================
c
c     quicksort
c
c     the "qn:"s correspond roughly to steps in algorithm q,
c     knuth, v.3, pp.116-117, modified to select the median
c     of the first, last, and middle elements as the "pivot
c     key" (in knuth's notation, "k").  also modified to leave
c     data in place and produce an index array.  to simplify
c     comments, let data[i]=data(index(i)).
 
c q1: initialize
      istk=0
      l=1
      r=n
 
 200   continue
 
c q2: sort the subsequence data[l]..data[r].
c
c     at this point, data[l] <= data[m] <= data[r] for all l < l,
c     r > r, and l <= m <= r.  (first time through, there is no
c     data for l < l or r > r.)
 
      i=l
      j=r
 
c q2.5: select pivot key
c
c     let the pivot, p, be the midpoint of this subsequence,
c     p=(l+r)/2; then rearrange index(l), index(p), and index(r)
c     so the corresponding data values are in increasing order.
c     the pivot key, datap, is then data[p].
 
      p=(l+r)/2
      indexp=index(p)
      datap=data(indexp)
 
      if (data(index(l)) .gt. datap) then
         index(p)=index(l)
         index(l)=indexp
         indexp=index(p)
         datap=data(indexp)
      endif
 
      if (datap .gt. data(index(r))) then
         if (data(index(l)) .gt. data(index(r))) then
            index(p)=index(l)
            index(l)=index(r)
         else
            index(p)=index(r)
         endif
         index(r)=indexp
         indexp=index(p)
         datap=data(indexp)
      endif
 
c     now we swap values between the right and left sides and/or
c     move datap until all smaller values are on the left and all
c     larger values are on the right.  neither the left or right
c     side will be internally ordered yet; however, datap will be
c
 
 300   continue
 
c q3: search for datum on left >= datap
c
c     at this point, data[l] <= datap.  we can therefore start scanning
c     up from l, looking for a value >= datap (this scan is guaranteed
c     to terminate since we initially placed datap near the middle of
c     the subsequence).
 
         i=i+1
         if (data(index(i)).lt.datap) goto 300
 
 400      continue
 
c q4: search for datum on right <= datap
c
c     at this point, data[r] >= datap.  we can therefore start scanning
c     down from r, looking for a value <= datap (this scan is guaranteed
c     to terminate since we initially placed datap near the middle of
c     the subsequence).
 
         j=j-1
         if (data(index(j)).gt.datap) goto 400
 
c q5: have the two scans collided?
 
      if (i.lt.j) then
 
c q6: no, interchange data[i] <--> data[j] and continue
 
         indext=index(i)
         index(i)=index(j)
         index(j)=indext
         goto 300
      else
 
c q7: yes, select next subsequence to sort
c
c     at this point, i >= j and data[l] <= data[i] == datap <= data[r],
c     for all l <= l < i and j < r <= r.  if both subsequences are
c     more than m elements long, push the longer one on the stack and
c     go back to quicksort the shorter; if only one is more than m
c     elements long, go back and quicksort it; otherwise, pop a
c     subsequence off the stack and quicksort it.
 
         if (r-j .ge. i-l .and. i-l .gt. m) then
            istk=istk+1
            lstk(istk)=j+1
            rstk(istk)=r
            r=i-1
         else if (i-l .gt. r-j .and. r-j .gt. m) then
            istk=istk+1
            lstk(istk)=l
            rstk(istk)=i-1
            l=j+1
         else if (r-j .gt. m) then
            l=j+1
         else if (i-l .gt. m) then
            r=i-1
         else
c q8: pop the stack, or terminate quicksort if empty
            if (istk.lt.1) goto 900
            l=lstk(istk)
            r=rstk(istk)
            istk=istk-1
         endif
         goto 200
      endif
 
 900   continue
 
c===================================================================
c
c q9: straight insertion sort
 
      do 950 i=2,n
         if (data(index(i-1)) .gt. data(index(i))) then
            indexp=index(i)
            datap=data(indexp)
            p=i-1
 920               continue
               index(p+1) = index(p)
               p=p-1
               if (p.gt.0) then
                  if (data(index(p)).gt.datap) goto 920
               endif
            index(p+1) = indexp
         endif
 950         continue
 
c===================================================================
c
c     all done
 
      end
