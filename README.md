[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7221439.svg)](https://doi.org/10.5281/zenodo.7221439)

# HYPE-HDS
A repo for the HYPE-HDS model.

Journal publication: Ahmed, M.I., Shook, K., Pietroniro, A., Stadnyk, T., Pomeroy, J.W., Pers, C., Gustafsson, D., 2023. Implementing a parsimonious variable contributing area algorithm for the prairie pothole region in the HYPE modelling framework. Environmental Modelling & Software 167, 105769. https://doi.org/10.1016/j.envsoft.2023.105769

Users are advised to check the [HYPE official website](https://hypeweb.smhi.se/model-water/) for the most recent version of the HYPE source code (with HDS implementation).


The main modifications are in the [`model_hype.f90`](HYPE_HDS_src/model_hype.f90) and [`sw_proc.f90`](HYPE_HDS_src/sw_proc.f90) fortran files. Files [`modvar.f90`](HYPE_HDS_src/modvar.f90), [`hypevar.f90`](HYPE_HDS_src/hypevar.f90), [`data.f90`](HYPE_HDS_src/data.f90) and [`model_hype.f90`](HYPE_HDS_src/model_hype.f90) have been changed for the new input. Files [`hypetypes.f90`](HYPE_HDS_src/hypetypes.f90) and [`assimilation_interface.f90`](HYPE_HDS_src/assimilation_interface.f90) have been changed due to changed state variables, and input data test added to [`hype_tests.f90`](HYPE_HDS_src/hype_tests.f90). 

The code is currently working on the Smith Creek Research Basin (SCRB) and St Denis National Wildlife Area above pond 90 (SDNWA-90). The calibrated model setup is available in the [`SCRB_SDNWA90_files`](SCRB_SDNWA90_files) folder. The model set-up has been modified to fit the updated code.

Use description:
To use HDS model the "modeloption connectivity 2" has to be set in info.txt. Otherwise only original ilakes are simulated. The subbasins which should be simulated with HDS need to have an ilake SLC area fraction given in GeoData.txt. For ilregions without potholes the parameter (hdsdepth) or SLC fraction must be set to zero.
