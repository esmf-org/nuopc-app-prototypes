#!/bin/sh

cd ATM-A
make distclean; make
cd ..

cd ATM-B
make distclean; make
cd ..

cd ATM-C
make distclean; make
cd ..

cd ATM-D
make distclean; make
cd ..

cd ATM-E
make distclean; make
mv atmE.so ..
cd ..

cd ATM-F
make distclean; make
cd ..

cd OcnCommon
make distclean; make
cd ..

cd OcnModelA
make distclean; make
cd ..

cd OcnModelB/OcnSub
make distclean; make
cd ../..

cd OcnModelB
make distclean; make
cd ..

cd OcnModelC/OcnSub
make distclean; make
cd ../..

cd OcnModelC
make distclean; make
mv ocn.so ..
cd ..
