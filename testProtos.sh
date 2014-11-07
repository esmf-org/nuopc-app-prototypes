#!/bin/bash

count=0

function TestProto {
((count++))
testList[count]=$1
echo ---------------------------------------------------------------------------
echo STARTING: $1
cd $1
gmake distclean
gmake
mpirun -np 4 ./$2
if [ $? -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo FINISHED: $1
cd ..
echo ---------------------------------------------------------------------------
echo
}

function TestSelectProto {
echo ---------------------------------------------------------------------------
echo STARTING: $1
cd $1
gmake distclean
./cleanSubs.csh
./buildSubs.csh
gmake ATM=A OCN=A,B,C
echo "OCN_SELECT: A" > esm.config
((count++))
testList[count]=$1
mpirun -np 4 ./$2
if [ $? -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
gmake clean
gmake ATM=B OCN=A,B,C
echo "OCN_SELECT: B" > esm.config
((count++))
testList[count]=$1
mpirun -np 4 ./$2
if [ $? -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
gmake clean
gmake ATM=C OCN=A,B,C
echo "OCN_SELECT: C" > esm.config
((count++))
testList[count]=$1
mpirun -np 4 ./$2
if [ $? -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
gmake clean
gmake ATM=D OCN=A,B,C
echo "OCN_SELECT: A" > esm.config
((count++))
testList[count]=$1
mpirun -np 4 ./$2
if [ $? -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo FINISHED: $1
echo
gmake clean
gmake ATM=E OCN=A,B,C
echo "OCN_SELECT: B" > esm.config
((count++))
testList[count]=$1
mpirun -np 4 ./$2
if [ $? -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
gmake clean
gmake ATM=F OCN=A,B,C
echo "OCN_SELECT: C" > esm.config
#mpirun -np 4 ./$2   --- cannot run this because atmF is not fully implemented
echo FINISHED: $1
cd ..
echo ---------------------------------------------------------------------------
echo
}

TestProto v7-AtmOcnProto esmApp
TestProto v7-AtmOcnCplListProto esmApp
TestProto v7-AtmOcnLndProto esmApp
TestProto v7-AtmOcnMedPetListProto esmApp
TestProto v7-AtmOcnMedPetListTimescalesProto esmApp
TestProto v7-AtmOcnMedPetListTimescalesSplitFastProto esmApp
TestProto v7-AtmOcnTransferGridProto esmApp
TestProto v7-NamespaceProto mainApp
TestSelectProto v7-AtmOcnSelectExternalProto esmApp

i=1
while [[ $i -le $count ]]
do
echo ${testResult[i]}: ${testList[i]}
((i++))
done
