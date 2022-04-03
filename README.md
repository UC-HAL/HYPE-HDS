# HYPE-HGDM
A repo for the HYPE-HGDM development.


Changes:
- The model can now read parameters from `par.txt` file.
- Keeps the area of the depressions constant as HYPE does not allow for variable ilake area.

New changes:
- The modeloption connectivity 2 set the HGDM model to be used. I also added an option 3 for use of HGDM and Davids fill-and-spill model for the same model set-up (maybe unnecessary). 
- HGDM maximum depth can be added as input in GeoData.txt or par.txt. The hgdmdepth input replaces the lakedepth of the ilake and is used in the HGDM model. The input is also used as a flag for which subbasins use HGDM model.
- Added a test for valid input for modeloption connectivity 2.

This version of the HYPE model is still under development.

The HYPE model was modified by M. Ahmed at UCalgary.

This version of the code/repo is created to be shared with SHMI for revision and QC/QA purposes only.

The main modifications are in the [`model_hype.f90`](HYPE_HGDM_src/model_hype.f90) and [`sw_proc.f90`](HYPE_HGDM_src/sw_proc.f90) fortran files. Files [`modvar.f90`](HYPE_HGDM_src/modvar.f90), [`hypevar.f90`](HYPE_HGDM_src/hypevar.f90), [`data.f90`](HYPE_HGDM_src/data.f90) and [`model_hype.f90`](HYPE_HGDM_src/model_hype.f90) have been changed for the new input. Files [`hypetypes.f90`](HYPE_HGDM_src/hypetypes.f90) and [`assimilation_interface.f90`](HYPE_HGDM_src/assimilation_interface.f90) have been changed due to changed state variables, and input data test added to [`hype_tests.f90`](HYPE_HGDM_src/hype_tests.f90). 

The code is currently working on the Smith Creel Research Basin (SCRB). The calibrated model setup for that watershed is available in the [`SCRB_calib_model`](SCRB_calib_model) folder. The model set-up has been modified to fit the updated code.

Use description:
To use HGDM model the "modeloption connectivity 2" has to be set in info.txt. Otherwise only original ilakes are simulated. The subbasins which should be simulated with HGDM need to have an ilake SLC area fraction given in GeoData.txt. The whole subbasin will be considered as a potentially contributing to the potholes in the subbasin (aka icatch need to be 1). In addition the subbasins which should be simulated with HGDM need to have the HGDM maximum depth set either for each subbasin in GeoData.txt (column hgdm_depth), or for one (or several) ilregion(s) as parameter in par.txt (parameter hgdmdepth). For ilregions without potholes the parameter must be set to zero.