#!/bin/bash

#==============================================================================
# Earth System Modeling Framework
# Copyright 2002-2022, University Corporation for Atmospheric Research,
# Massachusetts Institute of Technology, Geophysical Fluid Dynamics
# Laboratory, University of Michigan, National Centers for Environmental
# Prediction, Los Alamos National Laboratory, Argonne National Laboratory,
# NASA Goddard Space Flight Center.
# Licensed under the University of Illinois-NCSA License.
#==============================================================================

# Obtain ESMF_INTERNAL_MPIRUN from esmf.mk
command=`grep ESMF_INTERNAL_MPIRUN $ESMFMKFILE`
eval $command

#TOOLRUN="valgrind --leak-check=full"

count=0
failcount=0

function TestProto {
((count++))
testList[count]=$1
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
make
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestProtoArgList {
if [ "$#" -ne 3 ]; then
echo ERROR: TestProtoArg requires 3 arguments.
echo "  $*"
return 1
fi
((count++))
testList[count]=$1
read -ra ARGS <<< "$3"
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
make
for arg in "${ARGS[@]}"; do
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 $arg > $2.$arg.stdout 2>&1
local result=$?
set +x
if [ $result -ne 0 ]
then
break
fi
done
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestSelectProto {
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
make ATM=A OCN=A,B
echo "OCN_SELECT: A" > esm.config
((count++))
testList[count]=$1
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
make clean
#-
make ATM=B OCN=A,B
echo "OCN_SELECT: A" > esm.config
((count++))
testList[count]=$1
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
make clean
#-
make ATM=A OCN=B
echo "OCN_SELECT: B" > esm.config
((count++))
testList[count]=$1
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestSelectExternalProto {
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
./cleanSubs.csh
./buildSubs.csh
#-
make ATM=A OCN=A,B,C
echo "OCN_SELECT: A" > esm.config
((count++))
testList[count]=$1
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
make clean
#-
make ATM=B OCN=A,B,C
echo "OCN_SELECT: B" > esm.config
((count++))
testList[count]=$1
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
make clean
#-
make ATM=C OCN=A,B,C
echo "OCN_SELECT: C" > esm.config
((count++))
testList[count]=$1
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
make clean
#-
make ATM=D OCN=A,B,C
echo "OCN_SELECT: A" > esm.config
((count++))
testList[count]=$1
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
make clean
#-
make ATM=E OCN=A,B,C
echo "OCN_SELECT: B" > esm.config
((count++))
testList[count]=$1
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo
make clean
#-
#make ATM=F OCN=A,B,C
echo "OCN_SELECT: C" > esm.config
#((count++))
#testList[count]=$1
#set -x
#$ESMF_INTERNAL_MPIRUN -np 4 ./$2   --- cannot run this because atmF is not fully implemented
#local result=$?
#set +x
#if [ $result -eq 0 ]
#then
#testResult[count]="PASS"
#else
#testResult[count]="FAIL"
#fi
#-
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestExplorer {
((count++))
testList[count]=$1
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
set -x
./nuopcExplorerScript ../AtmOcnSelectExternalProto/ATM-A/atmA.mk
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

# function    # proto directory                           # executable
TestProto     AsyncIOBlockingProto                        asyncIOApp
TestProto     AsyncIONonblockingProto                     asyncIOApp
TestProto     AtmOcnConOptsProto                          esmApp
TestProto     AtmOcnConProto                              esmApp
TestProto     AtmOcnCplListProto                          esmApp
TestProto     AtmOcnCplSetProto                           esmApp
TestProto     AtmOcnFDSynoProto                           esmApp
TestProto     AtmOcnIceSimpleImplicitProto                esmApp
TestProto     AtmOcnImplicitProto                         esmApp
TestProto     AtmOcnLndProto                              esmApp
TestProto     AtmOcnLogNoneProto                          esmApp
TestProto     AtmOcnMedIngestFromConfigProto              esmApp
TestProto     AtmOcnMedIngestFromInternalProto            esmApp
TestProto     AtmOcnMedPetListProto                       esmApp
TestProto     AtmOcnMedPetListTimescalesProto             esmApp
TestProto     AtmOcnMedPetListTimescalesSplitFastProto    esmApp
TestProto     AtmOcnMedProto                              esmApp
TestProto     AtmOcnMirrorFieldsProto                     esmApp
TestProto     AtmOcnPetListProto                          esmApp
TestProto     AtmOcnProto                                 esmApp
TestProto     AtmOcnRtmTwoTimescalesProto                 esmApp
TestProto     AtmOcnScalarProto                           esmApp
TestSelectExternalProto   AtmOcnSelectExternalProto       esmApp
TestSelectProto           AtmOcnSelectProto               esmApp
TestProto     AtmOcnSimpleImplicitProto                   esmApp
TestProto     AtmOcnTransferGridProto                     esmApp
TestProto     AtmOcnTransferLocStreamProto                esmApp
TestProto     AtmOcnTransferMeshProto                     esmApp
TestExplorer  ComponentExplorer                           nuopcExplorerApp
TestProto     CustomFieldDictionaryProto                  mainApp
TestProto     DriverInDriverDataDepProto                  mainApp
TestProto     DriverInDriverProto                         mainApp
TestProto     DynPhyProto                                 esmApp
TestProto     ExternalDriverAPIProto                      externalApp
TestProto     ExternalDriverAPIWeakCplDAProto             externalApp
TestProto     GenericMediatorProto                        app
TestProto     HierarchyProto                              esmApp
TestProto     NamespaceProto                              mainApp
TestProto     NestingMultipleProto                        mainApp
TestProto     NestingSingleProto                          mainApp
TestProto     NestingTelescopeMultipleProto               mainApp
TestProto     SingleModelProto                            mainApp
TestProto     SingleModelOpenMPProto                      mainApp
export OMP_NUM_THREADS=3
TestProto     SingleModelOpenMPUnawareProto               mainApp

date
echo "== TEST SUMMARY START =="
i=1
while [[ $i -le $count ]]
do
echo ${testResult[i]}: ${testList[i]}
if [ "${testResult[i]}" != "PASS" ]; then ((failcount++)); fi
((i++))
done
echo "== TEST SUMMARY STOP =="
date

echo
echo ---------------------------------------------------------------------------
grep " ERROR " */PET*.ESMF_LogFile

exit $failcount
