set -a

num=1;>xlflnk

while [ $num -le 99 ]
do
echo "[ -n \"\$XLFUNIT_$num\" ] && ln -sf \$XLFUNIT_$num fort.$num" >>xlflnk
num=`expr $num + 1`
done

chmod +x xlflnk; ./xlflnk; rm xlflnk
echo; echo `date` links.sh produced the following soft links:
echo; ls -ln fort.*; echo
