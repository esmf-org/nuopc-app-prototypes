#!/bin/csh

cd ATM-A
gmake distclean; gmake
cd ..

cd ATM-B
gmake distclean; gmake
cd ..

cd ATM-C
gmake distclean; gmake
cd ..

cd ATM-D
gmake distclean; gmake
cd ..

cd ATM-E
gmake distclean; gmake
mv atmE.so ..
cd ..

cd ATM-F
gmake distclean; gmake
cd ..

cd OcnCommon
gmake distclean; gmake
cd ..

cd OcnModelA
gmake distclean; gmake
cd ..

cd OcnModelB/OcnSub
gmake distclean; gmake
cd ../..

cd OcnModelB
gmake distclean; gmake
cd ..

cd OcnModelC/OcnSub
gmake distclean; gmake
cd ../..

cd OcnModelC
gmake distclean; gmake
mv ocn.so ..
cd ..
