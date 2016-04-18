a=0

while [ $a -lt 1000 ]
do
   ./test2
   a=`expr $a + 1`
done
