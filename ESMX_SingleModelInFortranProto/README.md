# ESMX_SingleModelInFortran

[ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) is used to implement an uncoupled application, consisting of only a single model component (SiMoCo). Here the SiMoCo component is implemented as a NUOPC component in Fortran. The build system of SiMoCo is CMake based, and thus hooks easily into the ESMX build procedure. The SiMoCo component is built in form of a shared library. It can be used through direct linking into the ESMX application, or by dynamic loading at run-time. Both options are available through ESMX, and demonstrated here.

## Primary Artifacts

Files and sub-directories that implement the fundamental concept demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `SiMoCo`            - Simple NUOPC-compliant Model Component, utilizing a CMake based build system.
- `esmxBuild.yaml`    - Standard ESMX YAML file describing the build dependencies of the `esmx_app` (the executable) on SiMoCo via the direct linking approach.
- `esmxRun.yaml`      - Standard ESMX YAML file describing the run configuration suitable for the direct linking approach.
- `esmxBuildDL.yaml`- Standard ESMX YAML file describing the build dependencies of the `esmx_app` (the executable) on SiMoCo via the dynamic loading at run-time approach.
- `esmxRunDL.yaml`  - Standard ESMX YAML file describing the run configuration suitable for the dynamic loading at run-time approach.

### Usage

#### Building the ESMX application

The user has two options when it comes to building the ESMX application:

- Use the ***ESMX_Builder*** utility

  Building the ESMX executable using the `ESMX_Builder` utility that comes with ESMF is a single step process. The approach does *not* require a top-level `CMakeLists.txt` file. The following assumes that the `bin` directory of the desired ESMF installation is present in the user's `PATH` environemnt variable.
     
     ```
     ESMX_Builder -v
     ```
  This uses the default `esmxBuild.yaml` in the current directory. An alternative ESMX build configuration can be specified on the command line:
     
     ```
     ESMX_Builder -v esmxBuildDL.yaml
     ```
  This build the dynamic library version of the test, where the SiMoCo component is built, but not linked into the executable. In this case, the shared object is loaded at run-time.
   
- Use ***CMake*** directly

  This approach requires a top-level `CMakeFiles.txt` file, which is provided with the example for demonstration. The standard 3 step CMake approach of configure/build/install is used:
  
  1. Configure:
  ```
  cmake -DCMAKE_INSTALL_PREFIX=./install -B build
  ```
  2. Build:
  ```
  cmake --build ./build --verbose
  ```
  3. Install:
  ```
  cmake --install ./build --verbose
  ```

  Use the `ESMX_BUILD_FILE` variable to specify an alternative ESMX build configuration, e.g. `esmxBuildDL.yaml` during the configuration step:
  
  ```
  cmake -DCMAKE_INSTALL_PREFIX=./install -DESMX_BUILD_FILE=esmxBuildDL.yaml -B build
  ```

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
   Alternatively build the ESMX executable via the `DL` target to use SiMoCo via dynamic linking at run-time:
   ```
   make DL
   ```
2. The `run` target of the `Makefile` uses the MPI launch procedure - identified by ESMF - to run `./install/bin/esmx_app` on 4 PETs.
   ```
   make run
   ```
   Or to run with the alternative `esmxRunDL.yaml` configuration that utilizes the dynamic linking approach to load SiMoCo at run-time:
   ```
   make runDL
   ```
3. The `distclean` target of the `Makefile` removes all of the generated files.
   ```
   make distclean
   ```
