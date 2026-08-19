## Description
<!-- Provide a brief summary of the changes introduced by this PR and which issues they fix -->

Fixes # (issue)

## CI Testing & ESMF Dependency Override
By default, the CI workflow builds against the `develop` branch of ESMF. If this PR introduces functionality or structural updates that rely on a specific version, tag, or experimental branch of ESMF, you can explicitly override the baseline here.

> [!TIP]
> To change the ESMF baseline for this PR's automated runs, uncomment the line below and specify your target branch, tag, or version (e.g., `v8.5.0b23`, `v8.6.0`, or a feature branch name).

<!-- ESMF_VERSION: develop -->
