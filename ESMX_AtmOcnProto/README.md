# ESMX_AtmOcnProto

ESMX is used to implement a coupled system with two components (ATM and OCN). Both components are provided as NUOPC-compliant models. 

## Primary Artifacts

Files and sub-directories that implement the concept demonstrated by the prototype. These are the primary artifacts to look at and to pattern actual user code after.

- `esmxBuild.yaml`  - Standard ESMX file describing the build dependencies of the `esmx` (the executable) on components Lumo and Tawas.
- `esmxRun.config`  - Standard ESMX file describing the run configuration: Lumo is OCN, Tawas is ATM, and defining the run sequence.
- `Lumo`            - Example OCN component that when built provides a CMake configuration that can be referenced by `esmxBuild.yaml`.
- `TaWaS`           - Example ATM component that when built provides a CMake configuration that can be referenced by `esmxBuild.yaml`.

## Secondary Artifacts

Files and sub-directories that are needed for the integration into the automated testing infrastructure. In some cases these artifacts might be interesting to look at, but caution should be used to not over interpret the implementation details as recommended use patterns.

- `Makefile`        - GNU Makefile that defines targets for convenience. Specifically the default target encapsulates the CMake commands needed for configuring and building `esmx`.
- `buildSubs.sh`    - Convenience script building the example components under the `Lumo` and `TaWaS` sub-directories.
- `cleanSubs.sh`    - Convenience script cleaning up build artifacts under the `Lumo` and `TaWaS` sub-directories.

## Usage

1. Build the example components by executing:<br>
   `./buildSubs.sh`
2. Inspect the `esmf.mk` file of your ESMF installation to find the appropriate `ESMF_ESMXDIR` variable:<br>
   `more $ESMFMKFILE`
3. Configure the ESMX build by passing the `ESMF_ESMXDIR` path via the `-H` otion to `cmake`. This can either be done manually on the shell by just copying the path found in step 2, or setting an environment variable `ESMF_ESMXDIR`. Here the latter is assumed:<br>
   `cmake -H$(ESMF_ESMXDIR) -Bbuild`
4. Build the executable `esmx` under the `build` directory that was created in step 3:<br>
   `cmake --build ./build`
5. Executable `./build/esmx` can now be executed on 4 PETs using the system-specific MPI launch procedure. The executable must be able to access file `esmxRun.config`. It reads the run configuration and drives the system accordingly.

For convenience, steps 2 through 5 are implemented as targets in the provided GNU Makefile.
