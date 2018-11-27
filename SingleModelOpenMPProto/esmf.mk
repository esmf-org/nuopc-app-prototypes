# ESMF application makefile fragment
#
# Use the following ESMF_ variables to compile and link
# your ESMF application against this ESMF build.
#
# !!! VERY IMPORTANT: If the location of this ESMF build is   !!!
# !!! changed, e.g. libesmf.a is copied to another directory, !!!
# !!! this file - esmf.mk - must be edited to adjust to the   !!!
# !!! correct new path                                        !!!
#
# Please see end of file for options used on this ESMF build
#


ESMF_VERSION_STRING=8.0.0 beta snapshot

ESMF_VERSION_MAJOR=8
ESMF_VERSION_MINOR=0
ESMF_VERSION_REVISION=0
ESMF_VERSION_PATCHLEVEL=0
ESMF_VERSION_PUBLIC='F'
ESMF_VERSION_BETASNAPSHOT='T'


ESMF_APPSDIR=/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/apps/appsg/Linux.gfortran.64.mpich3.default
ESMF_LIBSDIR=/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default


ESMF_F90COMPILER=mpif90
ESMF_F90LINKER=mpif90

ESMF_F90COMPILEOPTS=-g -Wall -Wextra -Wconversion -Wno-unused -Wno-unused-dummy-argument -fbacktrace -fimplicit-none -fcheck=array-temps,bounds,do,mem,recursion -fPIC  -m64 -mcmodel=small -pthread -ffree-line-length-none 
ESMF_F90COMPILEPATHS=-I/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/mod/modg/Linux.gfortran.64.mpich3.default -I/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/src/include -I/opt/NetCDF/4.1.3/GCC-7.1.0/include -I/opt/GCC/7.1.0/yaml/0.6.2/include
ESMF_F90COMPILECPPFLAGS=-DESMF_NO_INTEGER_1_BYTE -DESMF_NO_INTEGER_2_BYTE -DESMFVERSIONGIT='ESMF_8_0_0_beta_snapshot_24' -DESMF_LAPACK=1 -DESMF_LAPACK_INTERNAL=1 -DESMF_MOAB=1 -DESMF_NO_ACC_SOFTWARE_STACK=1 -DESMF_NETCDF=1 -DESMF_YAMLCPP=1 -DESMF_YAML=1 -DESMF_PIO=1 -DESMF_MPIIO -DESMF_NO_OPENACC -DESMF_TESTEXHAUSTIVE -DESMF_BOPT_g -DESMF_TESTCOMPTUNNEL -DSx86_64_small=1 -DESMF_OS_Linux=1 -DESMF_COMM=mpich3 -DESMF_DIR=/twixhome/gerhard/WorkESMF/ESMF.nuopcwork
ESMF_F90COMPILEFREECPP=
ESMF_F90COMPILEFREENOCPP=-ffree-form
ESMF_F90COMPILEFIXCPP=-cpp -ffixed-form
ESMF_F90COMPILEFIXNOCPP=

ESMF_F90LINKOPTS=  -m64 -mcmodel=small -pthread -Wl,--no-as-needed 
ESMF_F90LINKPATHS=-L/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default -L/opt/GCC/7.1.0/yaml/0.6.2/lib64 -L/opt/GCC/7.1.0/lib64/gcc/x86_64-pc-linux-gnu/7.1.0/../../../../lib64/
ESMF_F90ESMFLINKPATHS=-L/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default
ESMF_F90LINKRPATHS=-Wl,-rpath,/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default -Wl,-rpath,/opt/NetCDF/4.1.3/GCC-7.1.0/lib -Wl,-rpath,/opt/NetCDF/4.1.3/GCC-7.1.0/lib  -Wl,-rpath,/opt/GCC/7.1.0/yaml/0.6.2/lib64 -Wl,-rpath,/opt/GCC/7.1.0/lib64/gcc/x86_64-pc-linux-gnu/7.1.0/../../../../lib64/
ESMF_F90ESMFLINKRPATHS=-Wl,-rpath,/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default
ESMF_F90LINKLIBS= -lrt -lstdc++ -ldl -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdff -lnetcdf -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdf -lyaml-cpp
ESMF_F90ESMFLINKLIBS=-lesmf  -lrt -lstdc++ -ldl -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdff -lnetcdf -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdf -lyaml-cpp

ESMF_CXXCOMPILER=mpicxx
ESMF_CXXLINKER=mpicxx

ESMF_CXXCOMPILEOPTS=-g -Wall -Wextra -Wno-unused -fcheck-data-deps -fPIC -DESMF_LOWERCASE_SINGLEUNDERSCORE -m64 -mcmodel=small -pthread -std=c++11 
ESMF_CXXCOMPILEPATHS= -I/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/src/include  -I/opt/NetCDF/4.1.3/GCC-7.1.0/include -I/opt/GCC/7.1.0/yaml/0.6.2/include
ESMF_CXXCOMPILECPPFLAGS=-DESMF_NO_INTEGER_1_BYTE -DESMF_NO_INTEGER_2_BYTE -DESMFVERSIONGIT='ESMF_8_0_0_beta_snapshot_24' -DESMF_LAPACK=1 -DESMF_LAPACK_INTERNAL=1 -DESMF_MOAB=1 -DESMF_NO_ACC_SOFTWARE_STACK=1 -DESMF_NETCDF=1 -DESMF_YAMLCPP=1 -DESMF_YAML=1 -DESMF_PIO=1 -DESMF_MPIIO -DESMF_NO_OPENACC -DESMF_TESTEXHAUSTIVE  -DESMF_BOPT_g -DESMF_TESTCOMPTUNNEL -DSx86_64_small=1 -DESMF_OS_Linux=1 -DESMF_COMM=mpich3 -DESMF_DIR=/twixhome/gerhard/WorkESMF/ESMF.nuopcwork -D__SDIR__=''

ESMF_CXXLINKOPTS= -m64 -mcmodel=small -pthread -Wl,--no-as-needed 
ESMF_CXXLINKPATHS=-L/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default -L/opt/GCC/7.1.0/yaml/0.6.2/lib64 -L/opt/GCC/7.1.0/lib64/gcc/x86_64-pc-linux-gnu/7.1.0/../../../../lib64/
ESMF_CXXESMFLINKPATHS=-L/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default
ESMF_CXXLINKRPATHS=-Wl,-rpath,/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default -Wl,-rpath,/opt/NetCDF/4.1.3/GCC-7.1.0/lib -Wl,-rpath,/opt/NetCDF/4.1.3/GCC-7.1.0/lib  -Wl,-rpath,/opt/GCC/7.1.0/yaml/0.6.2/lib64 -Wl,-rpath,/opt/GCC/7.1.0/lib64/gcc/x86_64-pc-linux-gnu/7.1.0/../../../../lib64/
ESMF_CXXESMFLINKRPATHS=-Wl,-rpath,/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default
ESMF_CXXLINKLIBS= -lmpifort -lrt -lgfortran -ldl -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdff -lnetcdf -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdf -lyaml-cpp
ESMF_CXXESMFLINKLIBS=-lesmf  -lmpifort -lrt -lgfortran -ldl -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdff -lnetcdf -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdf -lyaml-cpp

ESMF_SO_F90COMPILEOPTS=-fPIC
ESMF_SO_F90LINKOPTS=-shared
ESMF_SO_F90LINKOPTSEXE=-Wl,-export-dynamic
ESMF_SO_CXXCOMPILEOPTS=-fPIC
ESMF_SO_CXXLINKOPTS=-shared
ESMF_SO_CXXLINKOPTSEXE=-Wl,-export-dynamic

ESMF_OPENMP_F90COMPILEOPTS= -fopenmp
ESMF_OPENMP_F90LINKOPTS= -fopenmp
ESMF_OPENMP_CXXCOMPILEOPTS= -fopenmp
ESMF_OPENMP_CXXLINKOPTS= -fopenmp

ESMF_OPENACC_F90COMPILEOPTS=
ESMF_OPENACC_F90LINKOPTS=
ESMF_OPENACC_CXXCOMPILEOPTS=
ESMF_OPENACC_CXXLINKOPTS=

# ESMF Tracing compile/link options
ESMF_TRACE_LDPRELOAD=/twixhome/gerhard/WorkESMF/ESMF.nuopcwork/lib/libg/Linux.gfortran.64.mpich3.default/libesmftrace_preload.so
ESMF_TRACE_STATICLINKOPTS=-static -Wl,--wrap=c_esmftrace_notify_wrappers -Wl,--wrap=c_esmftrace_isinitialized -Wl,--wrap=write -Wl,--wrap=writev -Wl,--wrap=pwrite -Wl,--wrap=read -Wl,--wrap=open -Wl,--wrap=MPI_Allreduce -Wl,--wrap=MPI_Barrier -Wl,--wrap=MPI_Wait -Wl,--wrap=mpi_allgather_ -Wl,--wrap=mpi_allgather__ -Wl,--wrap=mpi_allgatherv_ -Wl,--wrap=mpi_allgatherv__ -Wl,--wrap=mpi_allreduce_ -Wl,--wrap=mpi_allreduce__ -Wl,--wrap=mpi_alltoall_ -Wl,--wrap=mpi_alltoall__ -Wl,--wrap=mpi_alltoallv_ -Wl,--wrap=mpi_alltoallv__ -Wl,--wrap=mpi_alltoallw_ -Wl,--wrap=mpi_alltoallw__ -Wl,--wrap=mpi_barrier_ -Wl,--wrap=mpi_barrier__ -Wl,--wrap=mpi_bcast_ -Wl,--wrap=mpi_bcast__ -Wl,--wrap=mpi_exscan_ -Wl,--wrap=mpi_exscan__ -Wl,--wrap=mpi_gather_ -Wl,--wrap=mpi_gather__ -Wl,--wrap=mpi_gatherv_ -Wl,--wrap=mpi_gatherv__ -Wl,--wrap=mpi_recv_ -Wl,--wrap=mpi_recv__ -Wl,--wrap=mpi_reduce_ -Wl,--wrap=mpi_reduce__ -Wl,--wrap=mpi_reduce_scatter_ -Wl,--wrap=mpi_reduce_scatter__ -Wl,--wrap=mpi_scatter_ -Wl,--wrap=mpi_scatter__ -Wl,--wrap=mpi_scatterv_ -Wl,--wrap=mpi_scatterv__ -Wl,--wrap=mpi_scan_ -Wl,--wrap=mpi_scan__ -Wl,--wrap=mpi_send_ -Wl,--wrap=mpi_send__ -Wl,--wrap=mpi_wait_ -Wl,--wrap=mpi_wait__ -Wl,--wrap=mpi_waitall_ -Wl,--wrap=mpi_waitall__ -Wl,--wrap=mpi_waitany_ -Wl,--wrap=mpi_waitany__
ESMF_TRACE_STATICLINKLIBS=-lesmftrace_static

# Internal ESMF variables, do NOT depend on these!

ESMF_INTERNAL_DIR=/twixhome/gerhard/WorkESMF/ESMF.nuopcwork

#
# !!! The following options were used on this ESMF build !!!
#
# ESMF_DIR: /twixhome/gerhard/WorkESMF/ESMF.nuopcwork
# ESMF_OS: Linux
# ESMF_MACHINE: x86_64
# ESMF_ABI: 64
# ESMF_COMPILER: gfortran
# ESMF_BOPT: g
# ESMF_COMM: mpich3
# ESMF_SITE: default
# ESMF_PTHREADS: ON
# ESMF_OPENMP: ON
# ESMF_OPENACC: OFF
# ESMF_ARRAY_LITE: FALSE
# ESMF_NO_INTEGER_1_BYTE: TRUE
# ESMF_NO_INTEGER_2_BYTE: TRUE
# ESMF_FORTRANSYMBOLS: default
# ESMF_DEFER_LIB_BUILD: ON
# ESMF_SHARED_LIB_BUILD: ON
# 
# ESMF environment variables pointing to 3rd party software:
# ESMF_MOAB:              internal
# ESMF_LAPACK:            internal
#ESMF_ACC_SOFTWARE_STACK:            none
# ESMF_NETCDF:            /opt/NetCDF/4.1.3/GCC-7.1.0/bin/nc-config
# ESMF_NETCDF_INCLUDE:    /opt/NetCDF/4.1.3/GCC-7.1.0/include
# ESMF_NETCDF_LIBS:       -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdff -lnetcdf -L/opt/NetCDF/4.1.3/GCC-7.1.0/lib -lnetcdf
# ESMF_YAMLCPP:           standard
# ESMF_YAMLCPP_INCLUDE:   /opt/GCC/7.1.0/yaml/0.6.2/include
# ESMF_YAMLCPP_LIBS:      -lyaml-cpp
# ESMF_YAMLCPP_LIBPATH:   /opt/GCC/7.1.0/yaml/0.6.2/lib64
# ESMF_PIO:               internal
