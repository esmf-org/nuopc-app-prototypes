#!/bin/bash

#==============================================================================
# Earth System Modeling Framework
# Copyright (c) 2002-2024, University Corporation for Atmospheric Research,
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

RESULTSDIR=NUOPC-PROTO-RESULTS

count=0
failcount=0

if [[ $OSTYPE = *darwin* ]]
then
   # Darwin systems that use Apple Clang need extra environment variables
   # to find the libomp installation. These are important for the ESMX
   # tests where CMake is used under the hood to determine the correct
   # compiler and linker flags. Set them here to be available.

   # We'll try to get the location from a spack installation of llvm-openmp if possible,
   # but if we can't find that, then we'll fall back on assuming that libomp is available
   # via homebrew.
   homebrew_libomp_dir=/opt/homebrew/opt/libomp

   if command -v spack &>/dev/null; then
      # First try getting these from a spack-installed llvm-openmp:
      omp_dir=$(spack location -i --first llvm-openmp 2>/dev/null)
      if [ $? -ne 0 ]; then
         # llvm-openmp isn't installed with spack; fall back on the homebrew location
         omp_dir=${homebrew_libomp_dir}
      fi
   else
      # spack isn't found; fall back on the homebrew location
      omp_dir=${homebrew_libomp_dir}
   fi

   # The spack-based setup requires -Wl,-rpath in addition to -L
   export LDFLAGS="-L${omp_dir}/lib -Wl,-rpath,${omp_dir}/lib"
   export CXXFLAGS=-I${omp_dir}/include
   export CFLAGS=-I${omp_dir}/include
fi

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
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.Log
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
mkdir -p ../$RESULTSDIR
cp $2.$arg.stdout ../$RESULTSDIR/$1.$arg.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.$arg.Log
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
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1.1.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.1.Log
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
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1.2.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.2.Log
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
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1.3.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.3.Log
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
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.Log
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestESMXProto {
((count++))
testList[count]=$1
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
make
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./install/bin/$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.Log
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestESMXProtoRun {
((count++))
testList[count]=$1-$3
echo ---------------------------------------------------------------------------
date
echo STARTING: $1-$3
cd $1
make dust
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./install/bin/$2 esmxRun$3.yaml > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1-$3.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1-$3.Log
echo FINISHED: $1-$3
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestESMXwAltProto {
((count++))
testList[count]=$1
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
make
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./install/bin/$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.Log
echo ---------------------------------------------------------------------------
((count++))
testList[count]=$1-Alt
make dust
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./install/bin/$2 esmxRunAlt.yaml > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1-Alt.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1-Alt.Log
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestESMXwDLProto {
((count++))
testList[count]=$1
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
make
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./install/bin/$2 > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1.Log
echo ---------------------------------------------------------------------------
((count++))
testList[count]=$1-DL
make distclean
make DL
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./install/bin/$2 esmxRunDL.yaml > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1-DL.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1-DL.Log
echo FINISHED: $1
cd ..
date
echo ---------------------------------------------------------------------------
echo
}

function TestESMXoDLProto {
((count++))
testList[count]=$1-DL
echo ---------------------------------------------------------------------------
date
echo STARTING: $1
cd $1
make distclean
make DL
set -x
$ESMF_INTERNAL_MPIRUN -np 4 $TOOLRUN ./install/bin/$2 esmxRunDL.yaml > $2.stdout 2>&1
local result=$?
set +x
if [ $result -eq 0 ]
then
testResult[count]="PASS"
else
testResult[count]="FAIL"
fi
mkdir -p ../$RESULTSDIR
cp $2.stdout ../$RESULTSDIR/$1-DL.stdout
cat PET*.ESMF_LogFile > ../$RESULTSDIR/$1-DL.Log
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
TestProto     AtmOcnMirrorFieldsWithNamespaceProto        esmApp
TestProto     AtmOcnPetListProto                          esmApp
TestProto     AtmOcnProto                                 esmApp
TestProto     AtmOcnRtmTwoTimescalesProto                 esmApp
TestProto     AtmOcnScalarProto                           esmApp
TestSelectProto           AtmOcnSelectProto               esmApp
TestProto     AtmOcnSimpleImplicitProto                   esmApp
TestProto     AtmOcnTransferGridProto                     esmApp
TestProto     AtmOcnTransferLocStreamProto                esmApp
TestProto     AtmOcnTransferMeshProto                     esmApp
#TODO: Currently the ComponentExplorer implementation is based on the deprecated
#TODO: GNU Makefile based component build dependency approach. Consider moving
#TODO: any ComponentExplorer functionality worth preserving into ESMX, then
#TODO: test this approach here.
#TODO: Leaving the following line in as commented out for a reminder of the
#TODO: above for now.
#TestExplorer  ComponentExplorer                           nuopcExplorerApp
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
export OMP_NUM_THREADS=1
# - ESMX tests ----------------------------------------------------------------
TestESMXProto     ESMX_StartHereProto                     esmx_app
TestESMXProtoRun  ESMX_StartHereProto                     esmx_app  Step1
TestESMXProtoRun  ESMX_StartHereProto                     esmx_app  Step2
TestESMXProtoRun  ESMX_StartHereProto                     esmx_app  Step3
TestESMXProtoRun  ESMX_StartHereProto                     esmx_app  Step4
TestESMXwDLProto  ESMX_SingleModelInFortranProto          esmx_app
TestESMXoDLProto  ESMX_SingleModelInCProto                esmx_app
TestESMXwAltProto ESMX_AtmOcnProto                        esmx_app
TestESMXProto     ESMX_AtmOcnFortranAndCProto             esmx_app
TestESMXProto     ESMX_ExternalDriverAPIProto             externalApp
if [[ $ESMF_TEST_NUOPC_JULIA = ON ]]
then
   TestESMXoDLProto  ESMX_SingleModelInJuliaProto         esmx_app
fi

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
