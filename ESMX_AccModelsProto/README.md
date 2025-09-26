# ESMX_AccModels

[ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) is used to implement an uncoupled application, consisting of two model components (CompA and CompB), each of which have the ability to use accelerator devices.

The build systems for CompA and CompB are CMake based, and thus hook easily into the ESMX build procedure.

The components are built in form of static libraries to prevent known issues with memory management, e.g. with OpenAcc and multiple shared libraries.

The ESMX build approach is based on the **ESMX_Builder** utility.

## Primary Artifacts

Files and sub-directories that implement the fundamental concept demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `CompA`             - Simple NUOPC-compliant Model Component, utilizing a CMake based build system.
- `CompB`             - Simple NUOPC-compliant Model Component, utilizing a CMake based build system.
- `esmxBuild.yaml`    - Standard ESMX YAML file describing the build dependencies of the `esmx_app` (the executable) on SiMoCo via the direct linking approach.
- `esmxRun.yaml`      - Standard ESMX YAML file describing the run configuration suitable for the direct linking approach.

### Usage

#### Building the ESMX application

Building the ESMX executable using the `ESMX_Builder` utility that comes with ESMF is a single step process. The approach does *not* require a top-level `CMakeLists.txt` file. The following assumes that the `bin` directory of the desired ESMF installation is present in the user's `PATH` environemnt variable.
     
     ```
     ESMX_Builder -v
     ```
  This uses the default `esmxBuild.yaml` in the current directory. An alternative ESMX build configuration can be specified on the command line:
     
     ```
     ESMX_Builder -v esmxBuildDL.yaml
     ```
  This build the dynamic library version of the test, where the SiMoCo component is built, but not linked into the executable. In this case, the shared object is loaded at run-time.

#### Running the ESMX application

Run the `./install/bin/esmx_app` executable on 4 PETs using the appropriate MPI launch procedure. E.g.:

  ```
  mpirun -np 4 ./install/bin/esmx_app
  ```
Or using the alternative `esmxRunDL.yaml` configuration that loads the SiMoCo component dynamically at run-time:

  ```
  mpirun -np 4 ./install/bin/esmx_app esmxRunDL.yaml
  ```

## Secondary Artifacts

Files that are needed for the integration into ESMF's automated testing infrastructure for regression testing. These artifacts might be interesting to look at, but generally should *not* be used as patterns to follow in actual projects.

- `Makefile`        - GNU Makefile that defines targets that are used by the automated ESMF regression testing script.

### Usage

1. The default target of the `Makefile` calls the `ESMX_Builder` command line tool to build the ESMX executable:
   ```
   make
   ```
2. The `run` target of the `Makefile` uses the MPI launch procedure - identified by ESMF - to run `./install/bin/esmx_app` on 4 PETs.
   ```
   make run
   ```
3. The `distclean` target of the `Makefile` removes all of the generated files.
   ```
   make distclean
   ```
