#!/bin/csh

cd ATM-A
gmake distclean
cd ..

cd ATM-B
gmake distclean
cd ..

cd ATM-C
gmake distclean
cd ..

cd ATM-D
gmake distclean
cd ..

cd ATM-E
gmake distclean
cd ..

cd ATM-F
gmake distclean
cd ..

cd OcnModelA
gmake distclean
cd ..

cd OcnModelB
gmake distclean
cd ..

cd OcnModelB/OcnSub
gmake distclean
cd ../..

cd OcnModelC
gmake distclean
cd ..

cd OcnModelC/OcnSub
gmake distclean
cd ../..

cd OcnCommon
make distclean
cd ..

