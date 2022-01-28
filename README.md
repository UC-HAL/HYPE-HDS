# HYPE-HGDM
A repo for the HYPE-HGDM development.


Changes:
- The model can now read parameters from the `1-HGDM_par.txt` file.
- Fixed a bug in writing the output file `1-output.txt` and included mass balance calculations.

This version of the HYPE model is still under development.

The HYPE model was modified by M. Ahmed at UCalgary.

This version of the code/repo is created to be shared with SHMI for revision and QC/QA purposes only.

The main modifications are in the [`model_hype.f90`](HYPE_HGDM_src/model_hype.f90)_ and [`sw_proc.f90`](HYPE_HGDM_src/sw_proc.f90) fortran files.

The code is currently working on a hypothetical watershed. The model setup for that watershed is available in the [`Test_case`](Test_case) folder.

