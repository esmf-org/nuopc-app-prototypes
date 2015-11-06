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


TestProto AsyncIOBlockingProto           asyncIOApp
TestProto AsyncIONonblockingProto        asyncIOApp
TestProto AtmOcnConProto                 esmApp
TestProto AtmOcnCplListProto             esmApp
TestProto AtmOcnFDSynoProto              esmApp
TestProto AtmOcnImplicitProto            esmApp
TestProto AtmOcnLndProto                 esmApp
TestProto AtmOcnMedPetListProto          esmApp
TestProto AtmOcnMedPetListTimescalesProto          esmApp
TestProto AtmOcnMedPetListTimescalesSplitFastProto esmApp
TestProto AtmOcnMedProto                 esmApp
TestProto AtmOcnPetListProto             esmApp
TestProto AtmOcnProto                    esmApp
TestProto AtmOcnRtmTwoTimescalesProto    esmApp
TestSelectExternalProto   AtmOcnSelectExternalProto    esmApp
TestSelectProto           AtmOcnSelectProto            esmApp
TestProto AtmOcnSimpleImplicitProto      esmApp
TestProto AtmOcnIceSimpleImplicitProto   esmApp
TestProto AtmOcnTransferGridProto        esmApp
TestProto AtmOcnTransferLocStreamProto   esmApp
#  ComponentExplorer
TestProto DriverInDriverProto            mainApp
TestProto GenericMediatorProto           app
TestProto NamespaceProto                 mainApp
TestProto NestingMultipleProto           mainApp
TestProto NestingSingleProto             mainApp
TestProto NestingTelescopeMultipleProto  mainApp
TestProto SingleModelProto               mainApp

i=1
while [[ $i -le $count ]]
do
echo ${testResult[i]}: ${testList[i]}
((i++))
done
