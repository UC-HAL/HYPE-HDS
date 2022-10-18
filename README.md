# HYPE-HDS
A repo for the HYPE-HDS model.


Changes:
- The model can now read parameters from `par.txt` file.
- Keeps the area of the depressions constant as HYPE does not allow for variable ilake area.

New changes:
- The modeloption connectivity 2 set the HDS model to be used. I also added an option 3 for use of HDS and Davids fill-and-spill model for the same model set-up (maybe unnecessary). 
- HDS maximum depth can be added as input in GeoData.txt or par.txt. The hdsdepth input replaces the lakedepth of the ilake and is used in the HDS model. The input is also used as a flag for which subbasins use HDS model.
- Added a test for valid input for modeloption connectivity 2.

This version of the HYPE model is still under development.

The HYPE model was modified by M. Ahmed at UCalgary.

This version of the code/repo is created to be shared with SHMI for revision and QC/QA purposes only.

The main modifications are in the [`model_hype.f90`](HYPE_HDS_src/model_hype.f90) and [`sw_proc.f90`](HYPE_HDS_src/sw_proc.f90) fortran files. Files [`modvar.f90`](HYPE_HDS_src/modvar.f90), [`hypevar.f90`](HYPE_HDS_src/hypevar.f90), [`data.f90`](HYPE_HDS_src/data.f90) and [`model_hype.f90`](HYPE_HDS_src/model_hype.f90) have been changed for the new input. Files [`hypetypes.f90`](HYPE_HDS_src/hypetypes.f90) and [`assimilation_interface.f90`](HYPE_HDS_src/assimilation_interface.f90) have been changed due to changed state variables, and input data test added to [`hype_tests.f90`](HYPE_HDS_src/hype_tests.f90). 

The code is currently working on the Smith Creek Research Basin (SCRB) and St Denis National Wildlife Area above pond 90 (SDNWA-90). The calibrated model setup is available in the [`SCRB_SDNWA90_files`](SCRB_SDNWA90_files) folder. The model set-up has been modified to fit the updated code.

Use description:
To use HDS model the "modeloption connectivity 2" has to be set in info.txt. Otherwise only original ilakes are simulated. The subbasins which should be simulated with HDS need to have an ilake SLC area fraction given in GeoData.txt. For ilregions without potholes the parameter (hdsdepth) or SLC fraction must be set to zero.