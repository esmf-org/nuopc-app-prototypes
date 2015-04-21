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
gmake ATM=A OCN=A,B
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
gmake ATM=B OCN=A,B
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
gmake ATM=A OCN=B
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
echo FINISHED: $1
cd ..
echo ---------------------------------------------------------------------------
echo
}

function TestSelectExternalProto {
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


TestProto v7-AsyncIOBlockingProto         asyncIOApp
TestProto v7-AsyncIONonblockingProto      asyncIOApp
TestProto v7-AtmOcnConProto               esmApp
TestProto v7-AtmOcnCplListProto           esmApp
TestProto v7-AtmOcnFDSynoProto            esmApp
TestProto v7-AtmOcnImplicitProto          esmApp
TestProto v7-AtmOcnLndProto               esmApp
TestProto v7-AtmOcnMedPetListProto        esmApp
TestProto v7-AtmOcnMedPetListTimescalesProto          esmApp
TestProto v7-AtmOcnMedPetListTimescalesSplitFastProto esmApp
TestProto v7-AtmOcnMedProto               esmApp
TestProto v7-AtmOcnPetListProto           esmApp
TestProto v7-AtmOcnProto                  esmApp
TestProto v7-AtmOcnRtmTwoTimescalesProto  esmApp
TestSelectExternalProto   v7-AtmOcnSelectExternalProto    esmApp
TestSelectProto           v7-AtmOcnSelectProto            esmApp
TestProto v7-AtmOcnSimpleImplicitProto    esmApp
TestProto v7-AtmOcnTransferGridProto      esmApp
#  v7-ComponentExplorer
TestProto v7-DriverInDriverProto mainApp
TestProto v7-GenericMediatorProto app
TestProto v7-NamespaceProto mainApp
TestProto v7-NestingMultipleProto mainApp
TestProto v7-NestingSingleProto mainApp
TestProto v7-SingleModelProto mainApp

i=1
while [[ $i -le $count ]]
do
echo ${testResult[i]}: ${testList[i]}
((i++))
done
