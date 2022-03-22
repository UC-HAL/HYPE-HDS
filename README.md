# HYPE-HGDM
A repo for the HYPE-HGDM development.


Changes:
- The model can now read parameters from `par.txt` file.
- Keeps the area of the depressions constant as HYPE does not allow for variable ilake area.

This version of the HYPE model is still under development.

The HYPE model was modified by M. Ahmed at UCalgary.

This version of the code/repo is created to be shared with SHMI for revision and QC/QA purposes only.

The main modifications are in the [`model_hype.f90`](HYPE_HGDM_src/model_hype.f90) and [`sw_proc.f90`](HYPE_HGDM_src/sw_proc.f90) fortran files.

The code is currently working on the Smith Creel Research Basin (SCRB). The calibrated model setup for that watershed is available in the [`SCRB_calib_model`](SCRB_calib_model) folder.

