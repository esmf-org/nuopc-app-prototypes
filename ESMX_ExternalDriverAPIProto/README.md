# ESMX_ExternalDriverAPIProto

[ESMX](https://github.com/esmf-org/esmf/tree/develop/src/addon/ESMX) is used to implement a coupled system with two components (ATM and OCN). The `ESMX_Driver` is driven by an external application layer through the [External NUOPC Interface](https://earthsystemmodeling.org/docs/nightly/develop/NUOPC_refdoc/node3.html#SECTION00038000000000000000)

## Primary Artifacts

Files and sub-directories that implement the concept demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `externalApp.F90` - Top level Fortran `program` scope. This layer calls into the `ESMX_Driver` through the "External NUOPC Interface".
- `CMakeLists.txt`  - Top level CMake build that produces the executable. This pulls in ESMX as a subdirectory.
- `esmxBuild.yaml`  - Standard ESMX file describing the build dependencies of `ESMX_Driver` on components Lumo and Tawas.
- `esmxRun.config`  - Standard ESMX file describing the run configuration: Lumo is OCN, Tawas is ATM, and defining the run sequence.
- `Lumo`            - Example OCN component that when built provides a CMake configuration that can be referenced by `esmxBuild.yaml`.
- `TaWaS`           - Example ATM component that when built provides a CMake configuration that can be referenced by `esmxBuild.yaml`.

## Secondary Artifacts

Files and sub-directories that are needed for the integration into the automated testing infrastructure. In some cases these artifacts might be interesting to look at, but caution should be used to not over interpret the implementation details as recommended use patterns.

- `Makefile`        - GNU Makefile that defines targets for convenience. Specifically the default target encapsulates the CMake commands needed for configuring and building `ESMX_Driver`, and ultimately linking into the final executable.
- `buildSubs.sh`    - Convenience script building the example components under the `Lumo` and `TaWaS` sub-directories.
- `cleanSubs.sh`    - Convenience script cleaning up build artifacts under the `Lumo` and `TaWaS` sub-directories.

## Usage

1. Build the example components by executing:<br>
   `./buildSubs.sh`
2. Inspect the `esmf.mk` file of your ESMF installation to find the appropriate `ESMF_ESMXDIR` variable:<br>
   `more $ESMFMKFILE`
3. Configure the top level CMake build to reference ESMX via the desired `ESMF_ESMXDIR` path. The `CMakeLists.txt` implements the ESMF_ESMXDIR option that needs to be set accordingly. This can either be done manually on the shell by just copying the path found in step 2, or setting an environment variable `ESMF_ESMXDIR`. Here the latter is assumed:<br>
   `cmake -H. -Bbuild -DESMF_ESMXDIR=$ESMF_ESMXDIR`
4. Build the local top level executable `externalApp` under the `build` directory that was created in step 3:<br>
   `cmake --build ./build`
5. Executable `./build/externalApp` can now be executed on 4 PETs using the system-specific MPI launch procedure. The executable must be able to access file `esmxRun.config`. The `ESMX_Driver` reads the run configuration and drives the system accordingly.

For convenience, steps 2 through 5 are implemented as targets in the provided GNU Makefile.
