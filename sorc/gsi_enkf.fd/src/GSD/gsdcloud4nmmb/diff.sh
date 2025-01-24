
set -x
rm -f ttt
flnm=`ls *90`
for iflnm in $flnm
do
  echo "**********" >> ttt
  echo $iflnm >> ttt
  diff $iflnm ../gsdcloud_old/$iflnm >> ttt
  echo                          >> ttt
done
