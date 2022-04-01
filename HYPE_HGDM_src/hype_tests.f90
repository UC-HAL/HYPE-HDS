!> \file hype_tests.f90
!> Contains module model_test_routines.

!> Module for writing test cases to a log file
MODULE MODEL_TEST_ROUTINES

  !Copyright 2017-2020 SMHI
  !
  !This file is part of HYPE.
  !
  !HYPE is free software: you can redistribute it and/or modify it under
  !the terms of the Lesser GNU General Public License as published by
  !the Free Software Foundation, either version 3 of the License, or (at
  !your option) any later version. HYPE is distributed in the hope that
  !it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  !warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  !the Lesser GNU General Public License for more details. You should
  !have received a copy of the Lesser GNU General Public License along
  !with HYPE. If not, see <http://www.gnu.org/licenses/>.
  !----------------------------------------------------------------------------
  USE LIBDATE
  USE READWRITE_ROUTINES, ONLY : read_parameterline
  USE HYPEVARIABLES, ONLY : n_max_par

  IMPLICIT NONE
  
  SAVE
  PRIVATE
  
  !The tests of HYPE are primarily grouped under hydrological processes,
  !but we have groups for generic tests belonging to an input file and 
  !even a generic group. The groups are called test processes.
  !
  !Parameter used as index for the specific test processes. 
  INTEGER, PARAMETER :: t_temperature_precipition  = 1  !hydrological processes
  INTEGER, PARAMETER :: t_evaporation              = 2  !
  INTEGER, PARAMETER :: t_soil_water               = 3  !
  INTEGER, PARAMETER :: t_snow_routines            = 4  !
  INTEGER, PARAMETER :: t_rivers                   = 5  !
  INTEGER, PARAMETER :: t_lakes                    = 6  !
  INTEGER, PARAMETER :: t_floodplains              = 7  !
  INTEGER, PARAMETER :: t_glaciers                 = 8  !
  INTEGER, PARAMETER :: t_aquifers                 = 9  !
  INTEGER, PARAMETER :: t_reg_grw                  = 10  !
  INTEGER, PARAMETER :: t_info_generic             = 11  !file generic processes
  INTEGER, PARAMETER :: t_geodata_generic          = 12 !
  INTEGER, PARAMETER :: t_geoclass_generic         = 13 !
  INTEGER, PARAMETER :: t_lakedata_generic         = 14 !
  INTEGER, PARAMETER :: t_damdata_generic          = 15 !
  INTEGER, PARAMETER :: t_mgmtdata_generic         = 16 !
  INTEGER, PARAMETER :: t_outregion_generic        = 17 !
  INTEGER, PARAMETER :: t_observation              = 18 !
  INTEGER, PARAMETER :: t_generic                  = 19 !
  INTEGER, PARAMETER :: t_wetland                  = 20 !processes for substances
  INTEGER, PARAMETER :: t_nut_source               = 21 !
  INTEGER, PARAMETER :: t_pointsource              = 22
  INTEGER, PARAMETER :: t_veg_soil_surface         = 23 !
  INTEGER, PARAMETER :: t_nutrient_soil            = 24 !
  INTEGER, PARAMETER :: t_atmdep                   = 25 !
  INTEGER, PARAMETER :: t_soilleakage              = 26 !
  ! This is the last number of processes
  INTEGER, PARAMETER :: nof_test_processes         = 26

  !Parameter used as index for the external tests performed elsewhere
  INTEGER, PARAMETER :: e_lakedata                 = 1  !
  INTEGER, PARAMETER :: e_lakedata_geoid           = 2  ! index for lakedataid == geodatalakeid
  INTEGER, PARAMETER :: e_lakedata_id              = 3  ! index for lakedataid > 0 for ldtype 1,3,4,5
  INTEGER, PARAMETER :: e_lakedata_ldtype          = 4  ! index for ldtype > 0 and <= 6
  INTEGER, PARAMETER :: e_lakedata_area            = 5  ! index for area > 0 for ldtype 2
  INTEGER, PARAMETER :: e_geodata                  = 6  !
  INTEGER, PARAMETER :: e_damdata                  = 7  !
  INTEGER, PARAMETER :: e_flooddata                = 8  !
  INTEGER, PARAMETER :: e_mgmtdata                 = 9  !
  INTEGER, PARAMETER :: e_forckey                  = 10 !
  INTEGER, PARAMETER :: e_par                      = 11 !
  INTEGER, PARAMETER :: e_lakedata_not             = 12 !
  INTEGER, PARAMETER :: e_prepare_update_ok        = 13 !
  INTEGER, PARAMETER :: e_model_base_conf_ok       = 14 !
  INTEGER, PARAMETER :: e_load_branch_data         = 15 !
  INTEGER, PARAMETER :: e_load_aquiferdata         = 16 !
  INTEGER, PARAMETER :: e_load_glacierdata         = 17 !
  INTEGER, PARAMETER :: e_pointsourcedata          = 18 !
  INTEGER, PARAMETER :: e_leakagedata              = 19 !
  INTEGER, PARAMETER :: e_update_file              = 20 !
  INTEGER, PARAMETER :: e_cropdata                 = 21 !
  INTEGER, PARAMETER :: e_info_file                = 22 !
  INTEGER, PARAMETER :: e_set_outvar_for_var       = 23 !
  INTEGER, PARAMETER :: e_load_output_regions      = 24 !
  INTEGER, PARAMETER :: e_output_illegal_input     = 25 !
  INTEGER, PARAMETER :: e_otest                    = 26 !
  INTEGER, PARAMETER :: e_output_range_violation   = 27 !
  INTEGER, PARAMETER :: e_lakedata_elakeoutlet     = 28
  ! This is the last number of external tests
  INTEGER, PARAMETER :: nof_external_tests         = 28
  CHARACTER(LEN=32) :: external_test_names(1:nof_external_tests)
  PARAMETER(external_test_names = [CHARACTER(LEN=32) :: 'ext.lakedata', &
  'ext.lakedata.geoid', 'ext.lakedata.id', 'ext.lakedata.ldtype', 'ext.lakedata.area', &
  'ext.geodata', 'ext.damdata', 'ext.flooddata', 'ext.mgmtdata', 'ext.forckey', &
  'ext.par','ext.lakedata.not','ext.prepare_update_ok','ext.model_base_conf_ok', &
  'ext.load_branch_data','ext.load_aquiferdata','ext.load_glacierdata','ext.pointsourcedata', &
  'ext.leakagedata','ext.update_file','ext.cropdata','ext.info_file','ext.set_outvar_for_var', &
  'ext.load_output_regions','ext.output_illegal_input','ext.otest','ext.output_range_violation', &
  'ext.lakedata.noutlet'])

  !Parameter used as index for the external tests msg types
  INTEGER, PARAMETER :: e_error                    = 1  !
  INTEGER, PARAMETER :: e_warning                  = 2  !
  INTEGER, PARAMETER :: e_info                     = 3  !
  ! This is the last number of external tests msg types
  INTEGER, PARAMETER :: nof_external_msg_types     = 3

  !Parameters used as index for test cases (test_id). 
  !The parameters are used for input, forcing and generic tests. HYPE 
  !parameter tests use the "Model parameter identification variable indices" 
  !defined in module hypevar (n_xxx).
  INTEGER, PARAMETER :: a_tobs                    = n_max_par + 1  !forcing
  INTEGER, PARAMETER :: a_tminobs                 = n_max_par + 2  !forcing
  INTEGER, PARAMETER :: a_tmaxobs                 = n_max_par + 3  !forcing
  INTEGER, PARAMETER :: a_swobs                   = n_max_par + 4  !forcing
  INTEGER, PARAMETER :: a_rhobs                   = n_max_par + 5  !forcing
  INTEGER, PARAMETER :: a_uobs                    = n_max_par + 6  !forcing
  INTEGER, PARAMETER :: a_sfobs                   = n_max_par + 7  !forcing
  INTEGER, PARAMETER :: a_elevation               = n_max_par + 8  !
  INTEGER, PARAMETER :: a_latitude                = n_max_par + 9  !
  INTEGER, PARAMETER :: a_area                    = n_max_par + 10 !
  INTEGER, PARAMETER :: a_ldfastlake              = n_max_par + 11 !
  INTEGER, PARAMETER :: a_grwtolake               = n_max_par + 12 !
  INTEGER, PARAMETER :: a_modeloptionrange        = n_max_par + 13 !
  INTEGER, PARAMETER :: a_snalbrange              = n_max_par + 14 !
  INTEGER, PARAMETER :: a_wcwpwcfcwcep            = n_max_par + 15 !
  INTEGER, PARAMETER :: a_AquiferData             = n_max_par + 16 !
  INTEGER, PARAMETER :: a_CropData                = n_max_par + 17 !
  INTEGER, PARAMETER :: a_FloodData               = n_max_par + 18 !
  INTEGER, PARAMETER :: a_t2sim                   = n_max_par + 19 !
  INTEGER, PARAMETER :: a_sum_macratesrrate       = n_max_par + 20 !
  INTEGER, PARAMETER :: a_sum_macratesrrate2      = n_max_par + 21 !
  INTEGER, PARAMETER :: a_latitude_defined        = n_max_par + 22 !
  INTEGER, PARAMETER :: a_gddsow                  = n_max_par + 23 !
  INTEGER, PARAMETER :: a_daylength               = n_max_par + 24 !
  INTEGER, PARAMETER :: a_firstday                = n_max_par + 25 !
  INTEGER, PARAMETER :: a_percdelay               = n_max_par + 26 !
  INTEGER, PARAMETER :: a_aqarea                  = n_max_par + 27 !
  INTEGER, PARAMETER :: a_porosity                = n_max_par + 28 !
  INTEGER, PARAMETER :: a_retrate                 = n_max_par + 29 !
  INTEGER, PARAMETER :: a_inivol                  = n_max_par + 30 !
  INTEGER, PARAMETER :: a_basedepth               = n_max_par + 31 !
  INTEGER, PARAMETER :: a_parregion2              = n_max_par + 32 !
  INTEGER, PARAMETER :: a_recievefrac             = n_max_par + 33 !
  INTEGER, PARAMETER :: a_rrcscorr                = n_max_par + 34 !
  INTEGER, PARAMETER :: a_eroindex                = n_max_par + 35 !
  INTEGER, PARAMETER :: a_slope                   = n_max_par + 36 !
  INTEGER, PARAMETER :: a_noftimestepts           = n_max_par + 37 !
  INTEGER, PARAMETER :: a_bdaterange              = n_max_par + 38 !
  INTEGER, PARAMETER :: a_edaterange              = n_max_par + 39 !
  INTEGER, PARAMETER :: a_cdaterange              = n_max_par + 40 !
  INTEGER, PARAMETER :: a_timemapduplicates       = n_max_par + 41 !
  INTEGER, PARAMETER :: a_basinarea               = n_max_par + 42 !
  INTEGER, PARAMETER :: a_basinslcfrac            = n_max_par + 43 !
  INTEGER, PARAMETER :: a_subidrange              = n_max_par + 44 !
  INTEGER, PARAMETER :: a_basinslope              = n_max_par + 45 !
  INTEGER, PARAMETER :: a_basinrivlen             = n_max_par + 46 !
  INTEGER, PARAMETER :: a_basinpospart            = n_max_par + 47 !
  INTEGER, PARAMETER :: a_basinpartsum            = n_max_par + 48 !
  INTEGER, PARAMETER :: a_basinodepth             = n_max_par + 49 !
  INTEGER, PARAMETER :: a_basinidepth             = n_max_par + 50 !
  INTEGER, PARAMETER :: a_slcforolake             = n_max_par + 51 !
  INTEGER, PARAMETER :: a_abstrwitholake          = n_max_par + 52 !
  INTEGER, PARAMETER :: a_linkmain                = n_max_par + 53 !
  INTEGER, PARAMETER :: a_linksecondary           = n_max_par + 54 !
  INTEGER, PARAMETER :: a_nclass                  = n_max_par + 55 !
  INTEGER, PARAMETER :: a_classdataluse           = n_max_par + 56 !
  INTEGER, PARAMETER :: a_classdatasoil           = n_max_par + 57 !
  INTEGER, PARAMETER :: a_multiple_classes        = n_max_par + 58 !
  INTEGER, PARAMETER :: a_soildepth               = n_max_par + 59 !
  INTEGER, PARAMETER :: a_inc_soildepth           = n_max_par + 60 !
  INTEGER, PARAMETER :: a_tobselev                = n_max_par + 61 !input
  INTEGER, PARAMETER :: a_forcings_stationid      = n_max_par + 62 !
  INTEGER, PARAMETER :: a_iforc_no_neg_or_no_miss = n_max_par + 63 !
  INTEGER, PARAMETER :: a_consistent_forcing      = n_max_par + 64 !
  INTEGER, PARAMETER :: a_no_neg_value            = n_max_par + 65 !
  INTEGER, PARAMETER :: a_timeperiod_forcing      = n_max_par + 66 !
  INTEGER, PARAMETER :: a_irrigationsubid         = n_max_par + 67 !
  INTEGER, PARAMETER :: a_lakeriverice_class      = n_max_par + 68 !
  INTEGER, PARAMETER :: a_buffer                  = n_max_par + 69 !
  INTEGER, PARAMETER :: a_closewater              = n_max_par + 70 !
  INTEGER, PARAMETER :: a_erosionon               = n_max_par + 71 !
  INTEGER, PARAMETER :: a_ccmax1                  = n_max_par + 72 !
  INTEGER, PARAMETER :: a_ccmax2                  = n_max_par + 73 !
  INTEGER, PARAMETER :: a_gcmax1                  = n_max_par + 74 !
  INTEGER, PARAMETER :: a_gcmax2                  = n_max_par + 75 !
  INTEGER, PARAMETER :: a_uptake1                 = n_max_par + 76 !
  INTEGER, PARAMETER :: a_uptake2                 = n_max_par + 77 !
  INTEGER, PARAMETER :: a_uptake3                 = n_max_par + 78 !
  INTEGER, PARAMETER :: a_uptakeupper             = n_max_par + 79 !
  INTEGER, PARAMETER :: a_uptakeratio             = n_max_par + 80 !
  INTEGER, PARAMETER :: a_bd2                     = n_max_par + 81 !
  INTEGER, PARAMETER :: a_bd3                     = n_max_par + 82 !
  INTEGER, PARAMETER :: a_bd5                     = n_max_par + 83 !
  INTEGER, PARAMETER :: a_scr_part                = n_max_par + 84 !
  INTEGER, PARAMETER :: a_scr_exist               = n_max_par + 85 !
  INTEGER, PARAMETER :: a_wcwp_gt_zero            = n_max_par + 86 !generic
  INTEGER, PARAMETER :: a_dailytimestep           = n_max_par + 87 !
  INTEGER, PARAMETER :: a_file_exist              = n_max_par + 88 !
  INTEGER, PARAMETER :: a_soilleakage             = n_max_par + 89 !
  INTEGER, PARAMETER :: a_fertday1                = n_max_par + 90 !
  INTEGER, PARAMETER :: a_fertday2                = n_max_par + 91 !
  INTEGER, PARAMETER :: a_manday1                 = n_max_par + 92 !
  INTEGER, PARAMETER :: a_manday2                 = n_max_par + 93 !
  INTEGER, PARAMETER :: a_fertdown1               = n_max_par + 94 !
  INTEGER, PARAMETER :: a_fertdown2               = n_max_par + 95 !
  INTEGER, PARAMETER :: a_mandown1                = n_max_par + 96 !
  INTEGER, PARAMETER :: a_mandown2                = n_max_par + 97 !
  INTEGER, PARAMETER :: a_fertnamount1            = n_max_par + 98 !
  INTEGER, PARAMETER :: a_fertnamount2            = n_max_par + 99 !
  INTEGER, PARAMETER :: a_mannamount1             = n_max_par + 100 !
  INTEGER, PARAMETER :: a_mannamount2             = n_max_par + 101 !
  INTEGER, PARAMETER :: a_fertpamount1            = n_max_par + 102 !
  INTEGER, PARAMETER :: a_fertpamount2            = n_max_par + 103 !
  INTEGER, PARAMETER :: a_manpamount1             = n_max_par + 104 !
  INTEGER, PARAMETER :: a_manpamount2             = n_max_par + 105 !
  INTEGER, PARAMETER :: a_resdayno                = n_max_par + 106 !
  INTEGER, PARAMETER :: a_resdown                 = n_max_par + 107 !
  INTEGER, PARAMETER :: a_resfast                 = n_max_par + 108 !
  INTEGER, PARAMETER :: a_resnamount              = n_max_par + 109 !
  INTEGER, PARAMETER :: a_respamount              = n_max_par + 110 !
  INTEGER, PARAMETER :: a_volloc                  = n_max_par + 111 !
  INTEGER, PARAMETER :: a_locconcnp               = n_max_par + 112 !
  INTEGER, PARAMETER :: a_basinindep              = n_max_par + 113 !
  INTEGER, PARAMETER :: a_basinindep2             = n_max_par + 114 !
  INTEGER, PARAMETER :: a_basinindep3             = n_max_par + 115 !
  INTEGER, PARAMETER :: a_damregvol               = n_max_par + 116 !
  INTEGER, PARAMETER :: a_damqprod                = n_max_par + 117 !
  INTEGER, PARAMETER :: a_damlimprod              = n_max_par + 118 !
  INTEGER, PARAMETER :: a_damqamp                 = n_max_par + 119 !
  INTEGER, PARAMETER :: a_damqinfmin              = n_max_par + 120 !
  INTEGER, PARAMETER :: a_damexpxx                = n_max_par + 121 !
  INTEGER, PARAMETER :: a_basinregion             = n_max_par + 122 !
  !INTEGER, PARAMETER :: a_soiltemp                = n_max_par + 123 !
  INTEGER, PARAMETER :: a_consistent_observation  = n_max_par + 124 !
  INTEGER, PARAMETER :: a_branchdefined           = n_max_par + 125 !
  INTEGER, PARAMETER :: a_lrwet_area              = n_max_par + 126 !
  INTEGER, PARAMETER :: a_mrwet_area              = n_max_par + 127 !
  INTEGER, PARAMETER :: a_lrwet_depth             = n_max_par + 128 !
  INTEGER, PARAMETER :: a_mrwet_depth             = n_max_par + 129 !
  INTEGER, PARAMETER :: a_lrwet_part              = n_max_par + 130 !
  INTEGER, PARAMETER :: a_mrwet_part              = n_max_par + 131 !
  INTEGER, PARAMETER :: a_lrwet_inflow            = n_max_par + 132 !
  INTEGER, PARAMETER :: a_mrwet_inflow            = n_max_par + 133 !
  INTEGER, PARAMETER :: a_iwet_area               = n_max_par + 134 !
  INTEGER, PARAMETER :: a_owet_area               = n_max_par + 135 !
  INTEGER, PARAMETER :: a_signfig                 = n_max_par + 136 !
  INTEGER, PARAMETER :: a_olake_area              = n_max_par + 137 !
  INTEGER, PARAMETER :: a_linklbinternal          = n_max_par + 138 !
  INTEGER, PARAMETER :: a_linklbsecondout         = n_max_par + 139 !
  INTEGER, PARAMETER :: a_winddir                 = n_max_par + 140 !
  INTEGER, PARAMETER :: a_uwobs                   = n_max_par + 141 !
  INTEGER, PARAMETER :: a_vwobs                   = n_max_par + 142 !
  INTEGER, PARAMETER :: a_mrivc_c                 = n_max_par + 143 !
  INTEGER, PARAMETER :: a_mrivc_f                 = n_max_par + 144 !
  INTEGER, PARAMETER :: a_mrivc_ci                = n_max_par + 145 !
  INTEGER, PARAMETER :: a_mrivc_fi                = n_max_par + 146 !
  INTEGER, PARAMETER :: a_outregionid             = n_max_par + 147 !
  INTEGER, PARAMETER :: a_outregionsubid          = n_max_par + 148 !
  INTEGER, PARAMETER :: a_lakeriverice_thpo       = n_max_par + 149
  INTEGER, PARAMETER :: a_atmdepvegreset          = n_max_par + 150
  INTEGER, PARAMETER :: a_consistent_pstimeinfo   = n_max_par + 151
  INTEGER, PARAMETER :: a_exist_pstimefile        = n_max_par + 152
  INTEGER, PARAMETER :: a_missing_pstime_forcing  = n_max_par + 153
  INTEGER, PARAMETER :: a_weightsub               = n_max_par + 154
  INTEGER, PARAMETER :: a_lksfarea                = n_max_par + 155
  INTEGER, PARAMETER :: a_lksficatch              = n_max_par + 156
  INTEGER, PARAMETER :: a_sum_lksficatch          = n_max_par + 157
  INTEGER, PARAMETER :: a_sum_lksfarea            = n_max_par + 158
  INTEGER, PARAMETER :: a_aqpassivedepth          = n_max_par + 159
  INTEGER, PARAMETER :: a_aqtopdepth              = n_max_par + 160
  INTEGER, PARAMETER :: a_aqpassivevol            = n_max_par + 161
  INTEGER, PARAMETER :: a_hgdmdep1                = n_max_par + 162
  INTEGER, PARAMETER :: a_hgdmdep2                = n_max_par + 163
  
! This is the maximum number of test cases; modparid + extra for test of generic, input and forcing
  INTEGER, PARAMETER :: max_testcases             = n_max_par + 163
  
  !This is the maximum number of test cases that can be tested for one 
  !test process (hydrological process or input file or other group). 
  INTEGER, PARAMETER :: max_testcase_values   = 64

  !Test case names for input, forcing and generic test cases
  CHARACTER(LEN=32) :: testing_names(n_max_par+1:max_testcases)
  PARAMETER(testing_names=[CHARACTER(LEN=32) :: 'tobs', 'tminobs', 'tmaxobs', 'swobs', &
      'rhobs', 'uobs', 'sfobs', 'elevation', 'latitude', 'area', 'ldfastlake', 'grwtolake', &
      'modeloptionrange', 'snalbrange', 'wcwpwcfcwcep', 'AquiferData.txt', &
      'CropData.txt', 'FloodData.txt', 'T2 simulation', 'summacratesrrate', &
      'summacratesrratepos', 'lat_defined', 'gddsow', &
      'daylength', 'firstday', 'percdelay', 'aqarea', 'porosity', 'retrate', 'inivol', &
      'basedepth', 'parregion2', 'recievefrac', 'rrcscorr', 'eroindex', 'slope', 'noftimestepts', &
      'bdaterange', 'edaterange', 'cdaterange', 'timemapduplicates', 'basinarea', 'basinslcfrac', &
      'subidrange', 'basinslope', 'basinrivlen', 'basinpospart', 'basinpartsum', 'basinodepth', &
      'basinidepth', 'slcforolake', 'abstrwitholake', 'linkmain', 'linksecondary', 'nclass', &
      'classdataluse', 'classdatasoil', 'multiple_classes', 'soildepth', 'inc_soildepth', &
      'tobselev', 'forcings_stationid', 'iforc_no_neg_or_no_miss', 'consistent_forcing', &
      'no_neg_value', 'time period', 'irrigationsubid', 'lakeriverice_class', &
      'buffer', 'closewater','erosion_on','ccmax1','ccmax2','gcmax1','gcmax2','uptake1', &
      'uptake2','uptake3','uptakeupper','PNuptakeRatio','bd2','bd3','bd5', &
      'secondcrop_part','secondcrop_exist','wcwp_positive','daily_timestep', &
      'file_exist','soilleakage','fertday1','fertday2','manday1','manday2','fertdown1','fertdown2', &
      'mandown1','mandown2','fertnamount1','fertnamount2','mannamount1','mannamount2', &
      'fertpamount1','fertpamount2','manpamount1','manpamount2','resdayno','resdown','resfast', &
      'resnamout','respamount','volloc','locconc_np','basinINwetdep','basinINdrydep', &
      'basinINdrydepload','damregvol','damqprod','damlimprod','damqamp','damqinfmin','damexpxx', &
      'basinregion','soiltemperature','consistent_observations','branch defined','lrwet area', &
      'mrwet area','lrwet depth','mrwet depth','lrwet_part','mrwet_part','lrwet_inflow', &
      'mrwet_inflow','iwet area','owet area','fewsignfig','areadiff<0.5%', &
      'links lakebasin internal', 'links lb extra outlets','no wind directions', &
      'uwobs','vwobs','basin_mrivcc','basin_mrivcf','basin_mrivcc_ice','basin_mrivcf_ice', &
      'outregionid','outregion_subid','ricethpo','atmdepveg_set','PSD_exist','PST_exist', &
      'PST_miss','subweightcrit','lks_farea','lks_ficatch','sum_lks_ficatch_0.1%', &
      'sum_lks_farea_0.1%','aq_passivedep','aq_topdepth','aq_passivevol','hgdmdepth','hgdm lake present'])
  
  !Parameter for kind of test case, used as index. The different kind of tests
  !are HYPE parameter, input, forcing or generic test cases.
  !The parameter determine how the extended print out is done.
  INTEGER, PARAMETER :: p_parameter  = 1  !
  INTEGER, PARAMETER :: p_input      = 2  !
  INTEGER, PARAMETER :: p_forcing    = 3  !
  INTEGER, PARAMETER :: p_generic    = 4  !
  
  !Parameter used for specific tolerance tests
  INTEGER, PARAMETER :: eq_zero                        = 1   !
  INTEGER, PARAMETER :: ne_zero                        = 2   !
  INTEGER, PARAMETER :: gt_zero                        = 3   !
  INTEGER, PARAMETER :: lt_zero                        = 4   !
  INTEGER, PARAMETER :: ge_zero                        = 5   !
  INTEGER, PARAMETER :: le_zero                        = 6   !
  INTEGER, PARAMETER :: gt_zero_and_lt_one             = 7   !
  INTEGER, PARAMETER :: ge_zero_and_lt_one             = 8   !
  INTEGER, PARAMETER :: gt_zero_and_le_one             = 9   !
  INTEGER, PARAMETER :: ge_zero_and_le_one             = 10  !
  INTEGER, PARAMETER :: gt_minus_one_and_lt_plus_one   = 11  !
  INTEGER, PARAMETER :: ge_minus_one_and_le_plus_one   = 12  !
  INTEGER, PARAMETER :: gt_minus_one                   = 13  !
  INTEGER, PARAMETER :: ge_minus_one                   = 14  !
  INTEGER, PARAMETER :: plus_minus_90                  = 15  !
  INTEGER, PARAMETER :: plus_minus_180                 = 16  !
  INTEGER, PARAMETER :: one_plus_minus_point_1_percent = 17  !
  INTEGER, PARAMETER :: ge_lowest_natural_point        = 18  !
  INTEGER, PARAMETER :: ge_zero_and_le_hundred         = 19
  INTEGER, PARAMETER :: ge_zero_and_le_366             = 20
  INTEGER, PARAMETER :: gt_zero_and_lt_366             = 21
  INTEGER, PARAMETER :: ge_one                         = 22
  INTEGER, PARAMETER :: ge_one_and_le_three            = 23
  ! This is the last number of tolerances
  INTEGER, PARAMETER :: nof_test_tolerances            = 23

  CHARACTER(LEN=32) :: testing_tolerances(nof_test_tolerances)
  PARAMETER(testing_tolerances=[CHARACTER(LEN=32) :: 'eq_zero', 'ne_zero', &
      'gt_zero', 'lt_zero', &
      'ge_zero', 'le_zero', &
      'gt_zero_and_lt_one', 'ge_zero_and_lt_one', &
      'gt_zero_and_le_one', 'ge_zero_and_le_one', &
      'gt_minus_one_and_lt_plus_one', 'ge_minus_one_and_le_plus_one', &
      'gt_minus_one', 'ge_minus_one', &
      'plus_minus_90', 'plus_minus_180', &
      'one_plus_minus_point_1_percent', 'ge_lowest_natural_point', &
      'ge_zero_and_le_hundred','ge_zero_and_le_366', &
      'gt_zero_and_lt_366','ge_one','ge_one_and_le_three'])
  
  !Type declarations
  
  !>\brief Type for holding tested data for external tests
  TYPE TESTEXTERNALINTERNALTYPE
    INTEGER                :: kind_of_msg = 0   !<
    CHARACTER, ALLOCATABLE :: msg(:)            !<
    REAL, ALLOCATABLE      :: data(:)           !<
  END TYPE TESTEXTERNALINTERNALTYPE

  !>\brief Type for holding data about external tests with test information and tested data
  TYPE TESTEXTERNALTYPE
    LOGICAL            :: propagated = .FALSE.   !<
    LOGICAL            :: passed = .TRUE.        !<
    INTEGER            :: index_ndata = 0        !<
    TYPE(TESTEXTERNALINTERNALTYPE) :: ndata(128) !<
  END TYPE TESTEXTERNALTYPE

  !>\brief Type for holding data about test cases with test information and error messages
  TYPE TESTCASETYPE
    LOGICAL           :: tested     = .FALSE.  !<test performed?
    LOGICAL           :: passed                !<result of test
    INTEGER           :: kind_of_test          !<kind of test variable; parameter, input, forcing, generic
    INTEGER           :: tolerance             !<
    INTEGER           :: status                !<
    INTEGER           :: modpar_index          !<index of the tested model parameter
    REAL              :: minvalue              !<minimum value of the tested input variable
    REAL              :: maxvalue              !<maximum value of the tested input variable
    CHARACTER(LEN=32) :: name                  !<name of the tested variable/te
    CHARACTER(LEN=64) :: errstring             !<
  END TYPE TESTCASETYPE

  !>\brief Type for holding data about test cases included in a test process
  TYPE TESTPROCESSTYPE
    INTEGER           :: test_id      = -1       !<index of a test case (>0), -1 means not set
    LOGICAL           :: test_passed  = .FALSE.  !<
  END TYPE TESTPROCESSTYPE

  TYPE(TESTEXTERNALTYPE) :: external_tests(nof_external_tests)
  TYPE(TESTCASETYPE) :: all_test_cases(max_testcases)
  TYPE(TESTPROCESSTYPE), DIMENSION(max_testcase_values,nof_test_processes) :: tests_c2_process
  
  INTEGER :: funit_used  = 0       ! Use stderr if not specified
  INTEGER :: printout_level = 0    ! Printout-debug-level e.g. 0=passed/failed,1=show-tests,2=show-parameters/inputs
  INTEGER :: onoff_option = 0      ! Option for on/off and if halt on error or not; 0=off,1=abort on error,2=run even though failed tests, 3=abort after tests
  LOGICAL :: error_halt = .TRUE.   ! Halt simulation on error TRUE/FALSE
  LOGICAL :: force_test  = .FALSE. ! Force a test regardless if tested before

  PUBLIC :: stop_simulation_and_finalize_tests,setup_for_hype_tests, &
            finalize_hype_tests,run_hype_observation_tests,run_hype_tests, &
            propagate_external_msg,e_lakedata,e_lakedata_geoid, &
            e_lakedata_ldtype,e_lakedata_not,e_geodata,e_damdata, &
            e_flooddata,e_mgmtdata,e_forckey,e_par,e_lakedata_elakeoutlet, &
            e_prepare_update_ok,e_model_base_conf_ok,e_load_branch_data, &
            e_load_aquiferdata,e_load_glacierdata,e_pointsourcedata, &
            e_leakagedata,e_update_file,e_cropdata,e_info_file, &
            e_set_outvar_for_var,e_load_output_regions, &
            e_output_illegal_input,e_otest,e_output_range_violation, &
            e_error,e_warning,e_info
  
  INTERFACE data_is_within_tolerance
    MODULE PROCEDURE int_data_is_within_tolerance,real_data_is_within_tolerance
  END INTERFACE
  
  INTERFACE data_differs_from_value
    MODULE PROCEDURE int_data_differs_from_value,real_data_differs_from_value
  END INTERFACE
       
  CONTAINS

  !> Stop the simulation and be sure to print external tests to test file
  !> @brief This function should be used if errors are encountered before the actual
  !> test module with all its test have been running. External text output is then
  !> guaranteed to end up in the test file before stop.
  !----------------------------------------------------------------------------
  SUBROUTINE stop_simulation_and_finalize_tests(errnum, errtext)

    !Argument declarations
    INTEGER, INTENT(IN) :: errnum                     ! Error number
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: errtext ! Error text if wanted

    !Local variables
    INTEGER status

    IF (PRESENT(errtext)) THEN
      WRITE(0,*) '-> Simulation will halt: '//errtext
    END IF
    
    ! be sure to print external tests as they are suppose to show the errors
    status = 0
    CALL check_external_tests(status)

    ! be sure to not stop with error number 3 due to a test setting
    ! but stop with the input errnum instead!
    IF(onoff_option>=3) onoff_option = 2
    CALL finalize_hype_tests()

    ! anoying select case due to restrictions in fortran to send a variable to STOP statement
    SELECT CASE(errnum)
      CASE(0)
        STOP 0
      CASE(1)
        STOP 1
      CASE(2)
        STOP 2
      CASE(3)
        STOP 3
      CASE(4)
        STOP 4
      CASE(5)
        STOP 5
      CASE(6)
        STOP 6
      CASE(7)
        STOP 7
      CASE(8)
        STOP 8
      CASE(9)
        STOP 9
    END SELECT
    STOP 10

  END SUBROUTINE stop_simulation_and_finalize_tests

  !>Setup for tests by setting flags to perform chosen test options and opens
  !>file for output of test results.
  !----------------------------------------------------------------------------
  SUBROUTINE setup_for_hype_tests(funit,fname,onoff,level,force)
  
    USE WORLDVAR, ONLY : fileunit_tests

    !Argument declarations
    INTEGER, OPTIONAL, INTENT(IN) :: funit           !<Used funit
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: fname  !<Used filename
    INTEGER, OPTIONAL, INTENT(IN) :: onoff           !<Tests onoff option
    INTEGER, OPTIONAL, INTENT(IN) :: level           !<level for debug printouts
    LOGICAL, OPTIONAL, INTENT(IN) :: force           !<Testing forced, even if already tested

    !Local variables

    error_halt = .TRUE.
    IF (PRESENT(onoff)) THEN
      onoff_option = onoff
      IF (onoff_option == 1) THEN
        error_halt = .TRUE.
      ELSEIF (onoff_option == 2) THEN
        error_halt = .FALSE.
      ELSEIF (onoff_option >= 3) THEN
        onoff_option = 3
        error_halt = .FALSE.
      ENDIF
    END IF

    IF (PRESENT(level)) THEN
      printout_level = level
      IF (printout_level >= 2) THEN
        printout_level = 2
      ENDIF
    ENDIF

    IF (PRESENT(force)) THEN
      force_test = force
    END IF

    IF (PRESENT(funit)) THEN
      funit_used = funit
    ELSEIF (PRESENT(fname) .AND. printout_level >= 0 .AND. onoff_option > 0) THEN    !do not open file if no tests made
      OPEN(UNIT=fileunit_tests,FILE=TRIM(fname),ACTION='write',ERR=700)
      funit_used = fileunit_tests
    ENDIF

    RETURN

    !Error handling
700 WRITE(0,*) 'Error: creating the hype tests file ',TRIM(fname)
    funit_used = 0

    RETURN

  END SUBROUTINE setup_for_hype_tests
  
  !> Finalize tests by closing file, clear all test results and inform of halt cause.
  !> @brief
  !----------------------------------------------------------------------------
  SUBROUTINE finalize_hype_tests()

    !Argument declarations

    !Local variables
    
    IF (onoff_option > 0) THEN
      WRITE(6,*) 'Data checks has been completed'
    END IF

    IF ((funit_used .NE. 0) .AND. (funit_used .NE. 6)) THEN
      CLOSE(funit_used)
      funit_used = 0
    END IF

    CALL clear_tests()

    IF (onoff_option >= 3) THEN
      WRITE(0,*) '-> Simulation will halt: Regardless of tests outcome! (indatacheckonoff >= 3)'
      WRITE(6,*) 'Simulation halted: Regardless of tests outcome! (indatacheckonoff >= 3)'
      STOP 3
    END IF

    RETURN

  END SUBROUTINE finalize_hype_tests

  !>Propagate message (and data if wanted) from an external test.
  !>This function can be called multiple times on the same external id.
  !----------------------------------------------------------------------------
  SUBROUTINE propagate_external_msg(external_id,msg_type,msg,data)

    !Argument declarations
    INTEGER, INTENT(IN) :: external_id                ! external test id
    INTEGER, INTENT(IN) :: msg_type                   ! message type
    CHARACTER(LEN=*), INTENT(IN) :: msg               ! message
    REAL, DIMENSION(:), OPTIONAL, INTENT(IN) :: data  ! data

    !Local variables
    INTEGER msg_size,i,k
    INTEGER, PARAMETER :: MAX_MSG_SIZE = 4096

    IF (external_id <= 0 .OR. external_id > nof_external_tests) THEN
      WRITE(funit_used,*) 'Error adding external test ', nof_external_tests, ', with msg: ', msg
      RETURN
    END IF

    IF (msg_type /= e_info .AND. msg_type /= e_warning) THEN
      external_tests(external_id)%passed = .FALSE.
    END IF

    msg_size = LEN(msg)
    IF (msg_size > MAX_MSG_SIZE) THEN
      msg_size = MAX_MSG_SIZE
    END IF

    i = external_tests(external_id)%index_ndata + 1
    external_tests(external_id)%index_ndata = i

    IF (i > SIZE(external_tests(external_id)%ndata)) THEN
      RETURN
    END IF

    external_tests(external_id)%ndata(i)%kind_of_msg = msg_type
    IF (ALLOCATED(external_tests(external_id)%ndata(i)%msg)) THEN
      DEALLOCATE(external_tests(external_id)%ndata(i)%msg)
    END IF
    ALLOCATE(external_tests(external_id)%ndata(i)%msg(msg_size))
    DO k = 1, msg_size
      external_tests(external_id)%ndata(i)%msg(k) = msg(k:k)
    END DO

    IF(PRESENT(data))THEN
      IF(SIZE(data) > 0) THEN
        IF (ALLOCATED(external_tests(external_id)%ndata(i)%data)) THEN
          DEALLOCATE(external_tests(external_id)%ndata(i)%data)
        END IF
        ALLOCATE(external_tests(external_id)%ndata(i)%data(SIZE(data)))
        external_tests(external_id)%ndata(i)%data = data
      END IF
    END IF

    external_tests(external_id)%propagated = .TRUE.

    RETURN

  END SUBROUTINE propagate_external_msg

  !> Check the external tests if any where propagated
  !> @brief
  !----------------------------------------------------------------------------
  SUBROUTINE check_external_tests(status)

    !Argument declarations
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    INTEGER i,j,failures
    CHARACTER(LEN=6) :: prefix_str   !info,warn,erro or ???? string

    failures = 0
    DO i = 1, nof_external_tests
      IF (external_tests(i)%propagated) THEN
        IF (external_tests(i)%passed) THEN
          WRITE(funit_used,*) '[Passed] ', external_test_names(i)
        ELSE
          WRITE(funit_used,*) '[Failed] ', external_test_names(i)
          status = status + 1
          failures = failures + 1
        END IF
        IF (external_tests(i)%index_ndata > SIZE(external_tests(i)%ndata)) THEN
          WRITE(funit_used,*) 'BEWARE: Too many infos,warnings and errors to show all', external_tests(i)%index_ndata
        END IF
        !Write msg and data if any
        DO j = 1, external_tests(i)%index_ndata
          IF (.NOT. ALLOCATED(external_tests(i)%ndata(j)%msg)) EXIT !break loop in unallocated, should never happen though!
          IF (external_tests(i)%ndata(j)%kind_of_msg == e_info) THEN
            prefix_str = 'INFO: '
          ELSE IF (external_tests(i)%ndata(j)%kind_of_msg == e_warning) THEN
            prefix_str = 'WARN: '
          ELSE IF (external_tests(i)%ndata(j)%kind_of_msg == e_error) THEN
            prefix_str = 'ERRO: '
          ELSE
            prefix_str = '????: '
          END IF
          WRITE(funit_used,*) prefix_str, external_tests(i)%ndata(j)%msg
          IF (ALLOCATED(external_tests(i)%ndata(j)%data) .AND. printout_level > 1) THEN
            WRITE(funit_used,*) 'data:', external_tests(i)%ndata(j)%data
          END IF
        END DO
      END IF
    END DO
    IF (failures .GT. 0) THEN
      WRITE(funit_used,*) 'Tested with number of failures: ',failures
      status = status + failures
    ENDIF

    RETURN

  END SUBROUTINE check_external_tests

  !> Runs tests for hype forcing data and other observations, and prints the result.
  !----------------------------------------------------------------------------
  SUBROUTINE run_hype_observation_tests(status)

    !Argument declarations
    INTEGER, INTENT(OUT)           :: status  !<error number

    !Local variables
    
    status = 0
    IF (onoff_option > 0) THEN
      WRITE(6,*) 'Observation data checks will be performed'
    ELSE
      WRITE(0,*) 'Observation data checks will NOT be performed'
      RETURN
    END IF

    WRITE(funit_used,*,ERR=300) 'indatacheckonoff', onoff_option
    WRITE(funit_used,*) 'indatachecklevel', printout_level
    WRITE(funit_used,*) '---------------------------------------------------------------'
    WRITE(funit_used,*) '---------------- OBSERVATIONS ---------------------------------'
    CALL check_observations(status)
    WRITE(funit_used,*) '-------------------- STATUS -----------------------------------'
    WRITE(funit_used,*) '-- Observations --', status
    WRITE(funit_used,*) '---------------------------------------------------------------'
    
    IF (.NOT. error_halt) THEN
      status = 0
    END IF

    RETURN
	
	!Error handling
300 WRITE(0,*) 'Error: writing to hype tests funit',funit_used
    status = 1
    RETURN

  END SUBROUTINE run_hype_observation_tests

  !> Check forcing data and other observations
  !> @brief Checking ForcKey and coupling between subbasin and observations
  !------------------------------------------------------------
  SUBROUTINE check_observations(status)

    USE WORLDVAR, ONLY : forcingdir, &
                         fileunit_temp, &
                         filename_Qobs, &
                         filename_Xobs, &
                         i_t, &
                         maxcharpath, &
                         get_seq_filename, &
                         forcingdata, &
                         max_forcingdata, &
                         i_pobs,i_tobs, &
                         i_rhobs,i_sfobs, &
                         i_swobs,i_uobs, &
                         bdate,sdate
    USE MODVAR, ONLY : basin, &
                       tobselevation, &
                       nsub_basemodel, conductxoms
    USE READWRITE_ROUTINES, ONLY : check_obs_timeperiod, &
                                   check_file_station_id_order, &
                                   check_station, &
                                   count_data_cols
    USE HYPE_INDATA, ONLY : xoms_filename, max_xoms

    !Argument declaration
    INTEGER, INTENT(INOUT) :: status  !<error number

    !Local variables
    CHARACTER(LEN=16) filename
    CHARACTER(LEN=maxcharpath) filepath
    TYPE(DateType) :: fbdate,fedate
    INTEGER iforc,s,check_miss_only,i,j
    INTEGER obsindex(nsub_basemodel) !Index of observation stations
    INTEGER nobscol                  !Number of columns with observations in file
    INTEGER numneg(nsub_basemodel)
    LOGICAL ret,ret2,fileexist,notimefound

    !> @test Check ForcKey:
    !> checking temperature elevations higher than lowest natural point
    ret = .TRUE.
    IF(ALLOCATED(tobselevation))THEN
      CALL check_input(ret,tobselevation,ge_lowest_natural_point,a_tobselev,t_observation)
      IF (.NOT. ret) THEN
        status = status + 1
      END IF
      CALL print_testresult_logical('ForcKey',ret,suffix='')
      CALL print_test_process(t_observation)
      CALL clear_tests(t_observation)
    ENDIF

    !> @test Check Pobs, Tobs, TMINobs, TMAXobs, RHobs, SFobs, SWobs, Uobs, UWobs, VWobs:
    !> checking stationids, timeperiods, consistency and positive values for some if file is present
    IFORC_LOOP : DO iforc = 1, max_forcingdata
      ret = .TRUE.
      IF (forcingdata(iforc)%readfile) THEN
        ret2 = .TRUE.
        filename = forcingdata(iforc)%filename
        CALL get_seq_filename(filename)
        filepath = TRIM(forcingdir)//TRIM(filename)
!        filepath = TRIM(modeldir)//TRIM(filename)
        INQUIRE(FILE=TRIM(filepath),EXIST=fileexist)
        IF (.NOT.fileexist) THEN
          ret2 = .FALSE.
          CALL add_generic_result(.FALSE.,iforc,a_file_exist,t_observation,'file found')
        ELSE
          CALL add_generic_result(.TRUE.,iforc,a_file_exist,t_observation,'file found')
          CALL check_input(ret2,REAL(forcingdata(iforc)%stationid),gt_zero,a_forcings_stationid,t_observation,force=.TRUE.)
          CALL check_file_station_id_order(fileunit_temp,filepath,nsub_basemodel,forcingdata(iforc)%stationid,obsindex,nobscol,s)
          IF (s .NE. 0) THEN
            ret2 = .FALSE.
            CALL add_generic_result(.FALSE.,iforc,a_consistent_forcing,t_observation,'consistency, model and '//TRIM(forcingdata(iforc)%filename))
          ELSE
            CALL add_generic_result(.TRUE.,iforc,a_consistent_forcing,t_observation,'consistency, model and '//TRIM(forcingdata(iforc)%filename))
          END IF
          CALL check_obs_timeperiod(fileunit_temp,filepath,1,i_t,bdate,sdate,fbdate,fedate,notimefound,s)
          IF (s .NE. 0) THEN
            ret2 = .FALSE.
            CALL add_generic_result(.FALSE.,iforc,a_timeperiod_forcing,t_observation,testing_names(a_timeperiod_forcing))
          ELSE
            CALL add_generic_result(.TRUE.,iforc,a_timeperiod_forcing,t_observation,testing_names(a_timeperiod_forcing))
          END IF
          IF (iforc==i_pobs.OR.iforc==i_rhobs.OR.iforc==i_sfobs.OR.iforc==i_swobs.OR.iforc==i_uobs) THEN
            !Check for all negative values not only missing!
            check_miss_only = 0
            CALL check_data_positive(filepath,notimefound,nobscol,nsub_basemodel,obsindex,check_miss_only,numneg,s)
          ELSE
            !For T,Tmin,Tmax check only for missing values!
            CALL check_data_missing(filepath,notimefound,nobscol,nsub_basemodel,obsindex,numneg,s)
          END IF
          IF (s == 1) THEN
            ret2 = .FALSE.
          END IF
          CALL check_input(ret2,REAL(numneg),eq_zero,a_iforc_no_neg_or_no_miss,t_observation)
        END IF
        !Be sure to print and clear tests after each cycle in the loop
        IF (.NOT. ret2) THEN
          ret = .FALSE.
        END IF
        IF (.NOT. ret) THEN
          status = status + 1
        END IF
        CALL print_testresult_logical(TRIM(filepath),ret,suffix='')
        CALL print_test_process(t_observation)
        CALL clear_tests(t_observation)
      END IF
    END DO IFORC_LOOP

    !> @test Check Qobs:
    !> checking timeperiods and positive values if file is present
    ret = .TRUE.
    filepath = TRIM(forcingdir)//filename_Qobs
    INQUIRE(FILE=filepath,EXIST=fileexist)
    ret2 = .TRUE.
    IF (fileexist) THEN
      !Check stations in Qobs, but allow stations not present in GeoData
      CALL check_station(fileunit_temp,filepath,nsub_basemodel,0,basin(:)%subid,nobscol,obsindex,s)
      IF(s .NE. 0) THEN
        CALL add_generic_result(.FALSE.,0,a_consistent_forcing,t_observation,'consistency, model and Qobs')
      ELSE
        CALL add_generic_result(.TRUE.,0,a_consistent_forcing,t_observation,'consistency, model and Qobs')
      ENDIF
      IF (s == 0) THEN
        CALL check_obs_timeperiod(fileunit_temp,filepath,1,i_t,bdate,sdate,fbdate,fedate,notimefound,s)
        IF (s == 1) THEN
          ret2 = .FALSE.
          CALL add_generic_result(.FALSE.,0,a_timeperiod_forcing,t_observation,testing_names(a_timeperiod_forcing))
        ELSE
          CALL add_generic_result(.TRUE.,0,a_timeperiod_forcing,t_observation,testing_names(a_timeperiod_forcing))
        END IF
        CALL check_data_positive(filepath,notimefound,nobscol,nsub_basemodel,obsindex,1,numneg,s)
        IF (s == 1) THEN
          ret2 = .FALSE.
        END IF
        CALL check_input(ret2,REAL(numneg),eq_zero,a_no_neg_value,t_observation)
      ELSE
        ret2 = .FALSE.
      ENDIF
    ENDIF
    IF (.NOT. ret2) THEN
      ret = .FALSE.
    END IF
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    CALL print_testresult_logical(TRIM(filepath),ret,suffix='')
    CALL print_test_process(t_observation)
    CALL clear_tests(t_observation)

    !> @test Check Xoms:
    !> checking stations, timeperiods and positive values if file is present
    ret = .TRUE.
    xoms_loop : DO i = 1, max_xoms
      DO j = 1,2
        IF(conductxoms) THEN
          filepath = TRIM(forcingdir)//TRIM(xoms_filename(i,j))
          INQUIRE(FILE=filepath,EXIST=fileexist)
          ret2 = .TRUE.
          IF (fileexist) THEN
            !Check stations
            CALL check_station(fileunit_temp,filepath,nsub_basemodel,1,basin(:)%subid,nobscol,obsindex,s)
            IF (s .NE. 0) THEN
              CALL add_generic_result(.FALSE.,s,a_consistent_observation,t_observation,'consistency, model and '//TRIM(xoms_filename(i,j)))
            ELSE
              CALL add_generic_result(.TRUE.,s,a_consistent_observation,t_observation,'consistency, model and '//TRIM(xoms_filename(i,j)))
            END IF
            IF (s == 0) THEN
              !Check time period
              CALL check_obs_timeperiod(fileunit_temp,filepath,2,i_t,bdate,sdate,fbdate,fedate,notimefound,s)
              IF (s == 1) THEN
                ret2 = .FALSE.
                CALL add_generic_result(.FALSE.,0,a_timeperiod_forcing,t_observation,testing_names(a_timeperiod_forcing))
              ELSE
                CALL add_generic_result(.TRUE.,0,a_timeperiod_forcing,t_observation,testing_names(a_timeperiod_forcing))
              END IF
              CALL check_data_positive(filepath,notimefound,nobscol,nsub_basemodel,obsindex,1,numneg,s)
              IF (s == 1) THEN
                ret2 = .FALSE.
              END IF
              !Check data
              CALL check_input(ret2,REAL(numneg),eq_zero,a_no_neg_value,t_observation)
            ELSE
              ret2 = .FALSE.
            ENDIF
          ENDIF
          IF (.NOT. ret2) THEN
            ret = .FALSE.
          END IF
          IF (fileexist) THEN
            CALL print_testresult_logical(TRIM(filepath),ret,suffix='')
            CALL print_test_process(t_observation)
            CALL clear_tests(t_observation)
          END IF
        END IF
      END DO
    END DO xoms_loop
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    !> @test Check Xobs:
    !> checking timeperiods and geodata connections if file is present
    ret = .TRUE.
    filepath = TRIM(forcingdir)//filename_Xobs
    INQUIRE(FILE=filepath,EXIST=fileexist)
    ret2 = .TRUE.
    IF (fileexist) THEN
      CALL count_data_cols(fileunit_temp,filepath,1, nobscol,s)
      IF (s == 0 .AND. nobscol > 0) THEN
        CALL check_xobs_validity(fileunit_temp,filepath,nobscol-1,s)
        IF (s > 0) THEN
          CALL add_generic_result(.FALSE.,s,a_consistent_observation,t_observation,testing_names(a_consistent_observation))
        ELSE
          CALL add_generic_result(.TRUE.,s,a_consistent_observation,t_observation,testing_names(a_consistent_observation))
        END IF
      END IF
      IF (s == 0) THEN
        CALL check_obs_timeperiod(fileunit_temp,filepath,3,i_t,bdate,sdate,fbdate,fedate,notimefound,s)
        IF (s == 1) THEN
          ret2 = .FALSE.
          CALL add_generic_result(.FALSE.,0,a_timeperiod_forcing,t_observation,testing_names(a_timeperiod_forcing))
        ELSE
          CALL add_generic_result(.TRUE.,0,a_timeperiod_forcing,t_observation,testing_names(a_timeperiod_forcing))
        END IF
      ELSE
        ret2 = .FALSE.
      ENDIF
    ENDIF
    IF (.NOT. ret2) THEN
      ret = .FALSE.
    END IF
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    CALL print_testresult_logical(TRIM(filepath),ret,suffix='')
    CALL print_test_process(t_observation)

    RETURN

  END SUBROUTINE check_observations

  !>Opens a file with observations, reads which variables it contains
  !>and checks for corrsponding indexes and ids. Used for Xobs.txt
  !-------------------------------------------------------------------------------
  SUBROUTINE check_xobs_validity(funit,infile,ncols,status)

    USE COMPOUT, ONLY : find_variable_index_type
    USE MODVAR, ONLY : basin

    !Argument declarations
    INTEGER, INTENT(IN)  :: funit             !<File unit for file
    CHARACTER (LEN=*), INTENT(IN) :: infile   !<Name of file to be read
    INTEGER, INTENT(IN)  :: ncols             !<Total number of columns
    INTEGER, INTENT(OUT) :: status            !<error number

    !Local variables
    CHARACTER(LEN=6) varname(ncols)     !Short name of variable in Xobs.txt
    CHARACTER(LEN=100) str
    INTEGER i,j,timeagg,areagg
    INTEGER varnumber(ncols)            !Outvaridnumber of variable for column in Xobs.txt
    INTEGER daroid(ncols)               !Subbasin identification number for corresponding column in Xobs.txt
    INTEGER ret(ncols)                  !Subbasin identification number corresponding to subid in GeoData.txt, Found=1 or NotFound=0

    !Start of subroutine
    status = 0
    OPEN(UNIT = funit,FILE = infile, STATUS = 'old',ACTION='read',ERR=200)
    READ(funit,*,ERR=201)                       !Skip one comment line
    READ(funit,*,ERR=201) str,varname(1:ncols)  !Read variable shortname
    READ(funit,*,ERR=201) str,daroid(1:ncols)   !Read variable subbasin id/outregion id

    !Find outvarid index of read variables
    DO j=1,ncols
      status = find_variable_index_type(varname(j),varnumber(j),timeagg,areagg)
      IF(status/=0)THEN
        ! treat this as a special error value to not confuse with the number of subids not finding a match
        status = 999
        RETURN
      ENDIF
    ENDDO

    !Check subbasins in file exist in model
    DO i=1,ncols
      ret(i) = 0
      DO j=1,SIZE(basin(:)%subid)
        IF(daroid(i) == basin(j)%subid) THEN
          ret(i) = 1
          EXIT
        ENDIF
      ENDDO
    ENDDO

    !Return the number of stations not in model setups
    status = ncols-SUM(ret)

    CLOSE(funit)
    RETURN
200 WRITE(6,*) 'Error: open file',TRIM(infile)
    status = 998
    RETURN
201 WRITE(6,*) 'Error: reading file',TRIM(infile)
    status = 998
    RETURN

  END SUBROUTINE check_xobs_validity

  !> Runs tests for hype and prints the result in file
  !> @brief
  !----------------------------------------------------------------------------
  SUBROUTINE run_hype_tests(status)

    !Argument declarations
    INTEGER, INTENT(OUT)  :: status  !<error number

    !Local variables
    
    !Initialize status
    status = 0

    IF (onoff_option > 0) THEN
      WRITE(6,*) 'Setup- and model data checks will be performed'
    ELSE
      WRITE(0,*) 'Setup- and model data checks will NOT be performed'
      RETURN
    END IF

    WRITE(funit_used,*,ERR=301) '---------------- FROM FILE LOADING ----------------------------'
    CALL check_external_tests(status)

    WRITE(funit_used,*) '---------------------------------------------------------------'
    WRITE(funit_used,*) '------------------ GENERIC ------------------------------------'
    WRITE(funit_used,*) '--info.txt--'
    CALL check_info_generic(status)
    WRITE(funit_used,*) '--GeoData.txt--'
    CALL check_geodata_generic(status)
    WRITE(funit_used,*) '--GeoClass.txt--'
    CALL check_geoclass_generic(status)
    WRITE(funit_used,*) '--LakeData.txt--'
    CALL check_lakedata_generic(status)
    WRITE(funit_used,*) '--DamData.txt--'
    CALL check_damdata_generic(status)
    WRITE(funit_used,*) '--MgmtData.txt--'
    CALL check_mgmtdata_generic(status)
    WRITE(funit_used,*) '--Outregions.txt--'
    CALL check_outregion_generic(status)
    WRITE(funit_used,*) '--mixed generic (no specific file)--'
    CALL check_generic(status)

    WRITE(funit_used,*) '---------------------------------------------------------------'
    WRITE(funit_used,*) '------------------ WATER --------------------------------------'
    
    WRITE(funit_used,*) '-------------- Processes above ground -------------------------'
    WRITE(funit_used,*) '-- Temperature and precipitation --'
    
    WRITE(funit_used,*) '-- Evaporation --'
    CALL check_evaporation(status)
    
    WRITE(funit_used,*) '-- Atmospheric deposition of nitrogen and phosphorus --'
    CALL check_atmospheric_deposition(status)
	
    WRITE(funit_used,*) '-------------- Land routines ----------------------------------'
    WRITE(funit_used,*) '-- Soil water --'
    CALL check_soil_water(status)
    WRITE(funit_used,*) '-- Snow --'
    CALL check_snow(status)
    WRITE(funit_used,*) '-- Glaciers --'
    CALL check_glaciers(status)
    
    WRITE(funit_used,*) '-------------- Rivers and lakes -------------------------------'
    WRITE(funit_used,*) '-- Rivers --'
    CALL check_rivers(status)
    WRITE(funit_used,*) '-- Lakes --'
    CALL check_lakes(status)
    WRITE(funit_used,*) '-- Floodplains --'
    CALL check_floodplains(status)
    WRITE(funit_used,*) '-- Bifurcations --'
    
    WRITE(funit_used,*) '-------------- Water management -------------------------------'
    WRITE(funit_used,*) '-- Constructed wetlands --'
    CALL check_river_wetlands(status)
    WRITE(funit_used,*) '-- Irrigation --'
    WRITE(funit_used,*) '-- Point sources --'
    CALL check_pointsourcedata(status)
    WRITE(funit_used,*) '-- Water transfer --'

    WRITE(funit_used,*) '-------------- Deep processes ---------------------------------'
    CALL check_general_deep(status)
    WRITE(funit_used,*) '-- Regional groundwater flow --'
    WRITE(funit_used,*) '-- Aquifers --'
    CALL check_aquifers(status)

    WRITE(funit_used,*) '---------------------------------------------------------------'
    WRITE(funit_used,*) '------------------ SUBSTANCES ---------------------------------'
    
    WRITE(funit_used,*) '-------------- Nitrogen and phosphorus in land routines -------'
    WRITE(funit_used,*) '-- Nutrient Sources --'
    CALL check_nutrient_sources(status)
    WRITE(funit_used,*) '-- Vegetation and soil surface processes --'
    CALL check_veg_soil_surface(status)
    WRITE(funit_used,*) '-- Nutrient soil processes --'
    CALL check_nutrient_soil(status)
    WRITE(funit_used,*) '-- Nutrient leakage from outer source --'
    CALL check_soil_leakage_as_input(status)
    
    WRITE(funit_used,*) '-------------- Nitrogen and phosphorus in rivers and lakes ----'
    WRITE(funit_used,*) '-- Denitrification --'
    WRITE(funit_used,*) '-- Primary production and mineralization --'
    WRITE(funit_used,*) '-- Sedimentation/Resuspension --'
    WRITE(funit_used,*) '-- Internal load --'

    WRITE(funit_used,*) '-------------- Organic carbon ---------------------------------'
    WRITE(funit_used,*) '-- Source of organic material --'
    WRITE(funit_used,*) '-- Soil processes --'
    WRITE(funit_used,*) '-- Riparian zone --'
    WRITE(funit_used,*) '-- Rivers and lakes --'

    WRITE(funit_used,*) '-------------- Tracers ----------------------------------------'
    WRITE(funit_used,*) '-- General tracer (T1) --'
    WRITE(funit_used,*) '-- Water temperature (T2) --'
    
    WRITE(funit_used,*) '---------------------------------------------------------------'
    WRITE(funit_used,*) '-------------------- STATUS -----------------------------------'
    WRITE(funit_used,*) '-- Total --', status
    WRITE(funit_used,*) '---------------------------------------------------------------'
    WRITE(funit_used,*) 

    IF (.NOT. error_halt) THEN
        status = 0
    END IF
    
    RETURN
	
	!Error handling
301 WRITE(0,*) 'Error: writing to hype tests funit', funit_used
    status = 1
    RETURN

  END SUBROUTINE run_hype_tests

  !> Generic info tests
  !> @brief The info tests include tests of the info.txt
  !--------------------------------------------------------------------
  SUBROUTINE check_info_generic(status)

    USE WORLDVAR, ONLY: ndt,bdate,sdate,outstartdate,output,noutput,&
                        weightsub,subweightcrit

    USE MODVAR, ONLY : outvarid,nsub

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER s,io,ivar,ivar2,old_status
    REAL x

    old_status = status
    status = 0

    !> @test Check simulation period: positive number of times steps and proper dates.
    ret = ndt > 0
    CALL add_generic_result(ret,0,a_noftimestepts,t_info_generic,'nof timesteps')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    ret = bdate%day >= 1 .AND. bdate%day <= 31
    ret = ret .AND. (bdate%month >= 1 .AND. bdate%month <= 12)
    CALL add_generic_result(ret,0,a_bdaterange,t_info_generic,'bdate range')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    ret = sdate%day >= 1 .AND. sdate%day <= 31
    ret = ret .AND. (sdate%month >= 1 .OR. sdate%month <= 12)
    CALL add_generic_result(ret,0,a_edaterange,t_info_generic,'edate range')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF
    ret = outstartdate%day >= 1 .AND. outstartdate%day <= 31
    ret = ret .AND. (outstartdate%month >= 1 .OR. outstartdate%month <= 12)
    CALL add_generic_result(ret,0,a_cdaterange,t_info_generic,'cdate range')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check timeoutput and mapoutput variables for duplicates:
    ret = .TRUE.
    s = 0
    DO io = 1, noutput
      IF(output(io)%fileformat==2 .OR. output(io)%fileformat==3)THEN
        DO ivar = 1,output(io)%nvar
          DO ivar2 = ivar+1,output(io)%nvar
            IF(outvarid(output(io)%variable(ivar)%idindex)%shortname==outvarid(output(io)%variable(ivar2)%idindex)%shortname &
                 .AND. output(io)%variable(ivar)%areaagg==output(io)%variable(ivar2)%areaagg)THEN
              ret = .FALSE.
              s = s + 1
            ENDIF
          ENDDO
        ENDDO
      ENDIF
    ENDDO
    CALL add_generic_result(ret,s,a_timemapduplicates,t_info_generic,'no duplicated time/mapoutputs')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check significant figures for rounding error of missing value (<4 signfigures):
    ret = .TRUE.
    s = 0
    DO io = 1, noutput
      IF(output(io)%signfig>0 .AND. output(io)%signfig<3)THEN    !0:using decimal format, less than 3 dec positions in F cant represent -9999
        ret = .FALSE.
        s = s + 1
      ENDIF
    ENDDO
    CALL add_generic_result(ret,s,a_signfig,t_info_generic,'enough signfigures')
    IF(.NOT. ret)THEN
      status = status + 1
    ENDIF

    !> @test Check that subbasin weights set for flag weightsub:
    ret = .TRUE.
    IF(weightsub)THEN
      x = SUM(subweightcrit)
      IF(x==nsub) ret = .FALSE.
      CALL add_generic_result(ret,s,a_weightsub,t_info_generic,'subweightcrit')
      IF(.NOT. ret)THEN
        status = status + 1
      ENDIF
    ENDIF

    ret = (status == 0)
    CALL print_testresult_logical('info.txt',ret,suffix='')

    CALL print_test_process(t_info_generic)

    status = status + old_status

  END SUBROUTINE check_info_generic

  !> Generic GeoData tests
  !--------------------------------------------------------------------
  SUBROUTINE check_geodata_generic(status)

    USE HYPEVARIABLES, ONLY : n_gldepo

    USE MODVAR, ONLY : basin,classbasin,max_subid,slc_olake,slc_ilake, &
                       load,nsub_basemodel,path,branchdata,nclass

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret,ret2
    INTEGER s,ksource,kbranch,bdim,i,j,k,old_status

    old_status = status
    status = 0

    !> @test Check that at least some subbasins have area greater than zero
    ret = (MINVAL(basin(:)%area) >= 0.0) .AND. (MAXVAL(basin(:)%area) > 0.0)
    CALL add_generic_result(ret,0,a_basinarea,t_geodata_generic,'some basin areas gt zero')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check that if subbasin area is positive, slc-fraction are given.
    ret = ALL(.NOT. ((basin(:)%area <= 0.0) .AND. (SUM(classbasin(:,:)%part, DIM=2) /= 0.0)))
    CALL add_generic_result(ret,0,a_basinslcfrac,t_geodata_generic,'basin area zero')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check subid range to be between one and max_subid
    ret = (MINVAL(basin(:)%subid) > 0) .AND. (MAXVAL(basin(:)%subid) <= max_subid)
    CALL add_generic_result(ret,0,a_subidrange,t_geodata_generic,'subid range')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check subbasin slope greater or equal to zero
    ret = MINVAL(basin(:)%slope) >= 0
    CALL add_generic_result(ret,0,a_basinslope,t_geodata_generic,'basin slope')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @TODO Add the check of lake regions for NPC simulation

    !> @test Check river length greater or equal to zero
    ret = MINVAL(basin(:)%rivlen(2)) >= 0
    CALL add_generic_result(ret,0,a_basinrivlen,t_geodata_generic,'basin rivlen')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check the sum of the slc fractions to be 1 with a tolerance of plus minus 0.1 percent
    CALL check_input(ret,SUM(classbasin(:,:)%part, DIM=2),one_plus_minus_point_1_percent,a_basinpartsum,t_geodata_generic)
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check that each and everyone of the slcs' are greater or equal to zero
    !> @todo Verify the test of each slc greater or equal to zero though!
    ret = MINVAL(SUM(classbasin(:,:)%part, DIM=2)) >= 0.0
    CALL add_generic_result(ret,0,a_basinpospart,t_geodata_generic,'slc fractions positive')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check that each secondary crop fraction is between zero and one.
    ret = .TRUE.
    DO j = 1, nclass
      ret2 = .TRUE.
      CALL check_input(ret2,classbasin(:,j)%part2cr,ge_zero_and_le_one,a_scr_part,t_geodata_generic,force = .TRUE.)
      ret = ret.AND.ret2
    ENDDO
    CALL add_generic_result(ret,0,a_scr_part,t_geodata_generic,'basin part2cr')
    IF(.NOT. ret) status = status + 1

    !> @test Check that lake depths, both ilake and olake, are positive.
    IF (slc_olake > 0) THEN
        ret = ALL(.NOT.((classbasin(:,slc_olake)%part > 0.0) .AND. (basin(:)%lakedepth(2) <= 0.0)))
        CALL add_generic_result(ret,0,a_basinodepth,t_geodata_generic,'basin olake depth')
        IF (.NOT. ret) THEN
            status = status + 1
        END IF
    END IF
    IF (slc_ilake > 0) THEN
        ret = ALL(.NOT.((classbasin(:,slc_ilake)%part > 0.0) .AND. (basin(:)%lakedepth(1) <= 0.0)))
        CALL add_generic_result(ret,0,a_basinidepth,t_geodata_generic,'basin ilake depth')
        IF (.NOT. ret) THEN
            status = status + 1
        END IF
    END IF

    !> @test Check abstractions; no abstractions when no lakes are simulated
    ret = ALL(.NOT. (((load(:)%abstrvol > 0) .AND. (load(:)%abstrind == 2)) .AND. (slc_olake == 0)))
    CALL add_generic_result(ret,0,a_slcforolake,t_geodata_generic,'no abstraction without olake')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF
    !> @test Check abstractions; outlet lake present when water is abstracted from that lake.
    IF(slc_olake>0)THEN
      !Check part only if olake exists.
      ret = ALL(.NOT. (((load(:)%abstrvol > 0) .AND. (load(:)%abstrind == 2)) .AND. (classbasin(:,slc_olake)%part <= 0.0)))
      CALL add_generic_result(ret,0,a_abstrwitholake,t_geodata_generic,'abstraction with olake')
      IF (.NOT. ret) THEN
        status = status + 1
      END IF
    ENDIF

    !> @test Check subbasin linkage (main and branch) and subbasin calculation order.
    IF(ALLOCATED(path)) THEN
      s = 0
      DO i = 1,nsub_basemodel
        DO k = i-1,1,-1
          IF(k==path(i)%main) THEN
            s = s + 1
          ENDIF
        ENDDO
      ENDDO
      ret = (s == 0)
      CALL add_generic_result(ret,s,a_linkmain,t_geodata_generic,'linking main branch')
      IF(.NOT. ret) status = status + 1
    ENDIF

    IF(ALLOCATED(branchdata)) THEN
      s = 0
      bdim=SIZE(branchdata(:)%source)
      DO i = 1,bdim
        ksource = 0
        kbranch = 0
        DO k = 1,nsub_basemodel
          IF(branchdata(i)%source==k)  ksource = k
          IF(branchdata(i)%branch==k)  kbranch = k
        END DO
        IF(kbranch/=0) THEN
          IF(kbranch<ksource) THEN
            s = s + 1
          END IF
        END IF
      END DO
      ret = (s == 0)
      CALL add_generic_result(ret,s,a_linksecondary,t_geodata_generic,'linking secondary branch')
      IF (.NOT. ret) THEN
        status = status + 1
      END IF
    END IF

    ret = (status == 0)
    CALL print_testresult_logical('geodata',ret,suffix='')

    CALL print_test_process(t_geodata_generic)

    status = status + old_status

  END SUBROUTINE check_geodata_generic

  !> Generic GeoClass tests
  !> @brief The GeoClass tests include tests of the GeoClass.txt
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_geoclass_generic(status)

    USE MODVAR, ONLY : nclass,classdata,classmodel,maxsoillayers
    USE WORLDVAR, ONLY : atmdepvegreset

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret,ret2,ret3
    INTEGER i,help1,help2,help3,help4,help5,help6,help7,old_status

    old_status = status
    status = 0

    !> @test Check that the number of slc-classes are greater than zero
    ret = (nclass > 0)
    CALL add_generic_result(ret,0,a_nclass,t_geoclass_generic,'nclass gt zero')
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check landuse code greater than zero
    ret = .TRUE.
    CALL check_input(ret,REAL(classdata(:)%luse),gt_zero,a_classdataluse,t_geoclass_generic)
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check soil type code greater than zero
    ret = .TRUE.
    CALL check_input(ret,REAL(classdata(:)%soil),gt_zero,a_classdatasoil,t_geoclass_generic)
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    !> @test Check vegetation type for atmosperic deposition
    ret = .NOT. atmdepvegreset
    CALL add_generic_result(ret,0,a_atmdepvegreset,t_geoclass_generic,testing_names(a_atmdepvegreset))
    IF (.NOT. ret) THEN
        status = status + 1
    END IF

    IF(ALLOCATED(classmodel)) THEN
      !> @test Check that there is only one class defined for each special class:
      !> ilake, olake, glacier, mriver, lriver, iwet, owet.
      help1=0;help2=0;help3=0;help4=0;help5=0;help6=0;help7=0
      DO i=1,nclass
        IF(classmodel(i)==1) THEN
          help1 = help1 + 1
        ELSE IF(classmodel(i)==2) THEN
          help2 = help2 + 1
        ELSE IF(classmodel(i)==3) THEN
          help3 = help3 + 1
        ELSE IF(classmodel(i)==11) THEN
          help4 = help4 + 1
        ELSE IF(classmodel(i)==12) THEN
          help5 = help5 + 1
        ELSEIF(classmodel(i)==13) THEN
          help6 = help6 + 1
        ELSEIF(classmodel(i)==14) THEN
          help7 = help7 + 1
        END IF
      END DO
      ret = (help1 <= 1 .AND. help2 <= 1 .AND. help3 <= 1 .AND. help4 <= 1 .AND. help5 <= 1 .AND. help6 <= 1 .AND. help7 <= 1)
      CALL add_generic_result(ret,0,a_multiple_classes,t_geoclass_generic,'no multiple classes, ilake,olake,mriver,lriver,glacier,iwet,owet')
      IF (.NOT. ret) THEN
          status = status + 1
      END IF
    END IF

    !> @test Check soildepth: Check that all layers have an increasing depths
    !> (last layer can have the same depth as previous one)
    ret = .TRUE.
    ret2 = .TRUE.
    ret3 = .TRUE.
    CALL add_generic_result(ret2,0,a_inc_soildepth,t_geoclass_generic,'increasing soildepth')
    DO i=1,maxsoillayers
        CALL check_input(ret,classdata(:)%soildepth(i),gt_zero,a_soildepth,t_geoclass_generic,force=.TRUE.)
        IF (i<maxsoillayers) THEN
            ! Check that each soillayer as an increasing depth
            ret2 = (MINVAL(classdata(:)%soildepth(i+1)-classdata(:)%soildepth(i)) >= 0.0)
            CALL add_generic_result(ret2,0,a_inc_soildepth,t_geoclass_generic,'increasing soildepth')
            ! Check the order between layers are ok
            IF (i>1 .AND. ret2) THEN
              ret2 = .NOT. ANY((classdata(:)%soildepth(i)-classdata(:)%soildepth(i-1)) == 0.0 .AND. &
              (classdata(:)%soildepth(i+1)-classdata(:)%soildepth(i)) /= 0.0 )
              ! Only add failure to test if previously soildepths where equal and next depth is higher than the last
              ! to make this test fail eventhough some depths may be ok.
              ! e.g depth1=5m, depth2=5m, depth3=10m => NOK
              ! e.g depth1=5m, depth2=10m, depth3=10m => OK
              IF (.NOT. ret2) THEN
                ret3 = .FALSE.
                CALL add_generic_result(ret3,0,a_inc_soildepth,t_geoclass_generic,'increasing soildepth')
              END IF
            END IF
        END IF
    END DO

    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    IF (.NOT. ret2 .OR. .NOT. ret3) THEN
      status = status + 1
    END IF

    ret = (status == 0)
    CALL print_testresult_logical('geoclass',ret,suffix='')

    CALL print_test_process(t_geoclass_generic)

    status = status + old_status

    !IF(.TRUE.)THEN
    !  !Information about the simulation...not in testfile.
    !  WRITE(109,*) 'INFO: Classes'
    !  WRITE(109,*) 'Number of classes:',nclass
    !  IF(slc_olake>0) WRITE(109,*) 'Outlet lake is class:',slc_olake
    !ENDIF
    !CALL log_petmodels_used(funit_used,petmodels_used,num_petmodels_used) !something similar
      
  END SUBROUTINE check_geoclass_generic

  !> Generic MgmtData tests
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_mgmtdata_generic(status)

    USE MODVAR, ONLY : irrigationsystem

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER old_status

    old_status = status
    status = 0

    !> @test Check that input data (subids) are given for all irrigated subbasins.
    IF(ALLOCATED(irrigationsystem)) THEN
      ret = ALL(irrigationsystem(:)%subid > 0)
      CALL add_generic_result(ret,0,a_irrigationsubid,t_mgmtdata_generic,'mgmtdata has subids defined')
      IF (.NOT. ret) THEN
        status = status + 1
      END IF
    END IF

    ret = (status == 0)
    CALL print_testresult_logical('mgmtdata',ret,suffix='')

    CALL print_test_process(t_mgmtdata_generic)

    status = status + old_status

  END SUBROUTINE check_mgmtdata_generic

  !> Generic Outregions tests
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_outregion_generic(status)

    USE MODVAR, ONLY : basin,nsub
    USE WORLDVAR, ONLY : outregion,noutreg

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER i,i2,num,old_status

    old_status = status
    status = 0

    !> @test Check that subid and outregid do not overlap
    ret = .TRUE.
    num = 0
    DO i = 1,nsub
      DO i2=1,noutreg
        IF(outregion(i2)%outregid==basin(i)%subid)THEN
          num = num + 1
        ENDIF
      ENDDO
    ENDDO
    IF(num>0)THEN
      ret = .FALSE.
      status = status + 1
    ENDIF
    CALL add_generic_result(ret,num,a_outregionid,t_outregion_generic,'outregion ids are unique')
    
    !> @test Check that subids/outregid in outregion exist (are set)
    ret = .TRUE.
    num = 0
    DO i = 1,noutreg
      DO i2 = 1, outregion(i)%nsubbasin
        IF(outregion(i)%subindex(i2)==0)THEN
          num = num + 1
        ENDIF
      ENDDO
    ENDDO
    IF(num>0)THEN
      ret = .FALSE.
      status = status + 1
    ENDIF
    CALL add_generic_result(ret,num,a_outregionsubid,t_outregion_generic,'outregion subids exist')
 
    ret = (status == 0)
    CALL print_testresult_logical('outregion',ret,suffix='')
    
    CALL print_test_process(t_outregion_generic)

    status = status + old_status

  END SUBROUTINE check_outregion_generic

  !> Generic LakeData tests
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_lakedata_generic(status)

    USE MODVAR, ONLY : nsub_basemodel,lakeindex, &
                       basin,classbasin,slc_olake, &
                       lake,elake,path, &
                       lakebasin,lakebasinindex, &
                       branchdata,branchindex
    
    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status
    
    !Local variables
    LOGICAL ret
    INTEGER i, j, k, klast, s, nlake, old_status
    REAL lakearea, parea

    old_status = status
    status = 0

    !!> @test Check that lakes in LakeData has the same area as in GeoData (not lakebasinlakes)
    ret = .TRUE.
    IF(ALLOCATED(lakeindex).AND. slc_olake>0)THEN  !Ordinary lake
      DO i = 1, nsub_basemodel
        IF(lakeindex(i)>0)THEN
          lakearea = basin(i)%area * classbasin(i,slc_olake)%part
          IF(lakearea>0.)THEN
            parea = ABS(lake(lakeindex(i))%area-lakearea)/lakearea
          ELSEIF(lake(lakeindex(i))%area>0.)THEN
            parea = 1.
          ENDIF
          ret = ret .AND. (parea <= 0.005)
        ENDIF
      ENDDO
      CALL add_generic_result(ret,0,a_olake_area,t_lakedata_generic,testing_names(a_olake_area))
    ENDIF
    IF(.NOT. ret) status = 1
    
    !> @test Check lakebasin internal outlet linkage (only main).
    !How check internal branches?
    IF(ALLOCATED(lakebasinindex))THEN
      s = 0
      DO i = 1,nsub_basemodel
        IF(lakebasinindex(i)>0)THEN
          IF(.NOT.lakebasin(lakebasinindex(i))%last)THEN
            IF(lakebasin(lakebasinindex(i))%ilk/=lakebasin(lakebasinindex(path(i)%main))%ilk)THEN
              s = s + 1
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      ret = (s == 0)
      CALL add_generic_result(ret,s,a_linklbinternal,t_lakedata_generic,testing_names(a_linklbinternal))
      IF(.NOT. ret) status = status + 1
    ENDIF

    !> @test Check lakebasin secondary outlet linkage.
    IF(ALLOCATED(lakebasin).AND.ALLOCATED(branchdata))THEN
      IF(ALLOCATED(lakebasinindex))THEN
        s = 0
        nlake=SIZE(elake(:)%noutlet)
        DO i = 1,nlake
          IF(elake(i)%noutlet<=1)CYCLE
          klast = elake(i)%outlet(1)%isb
          DO j = 2,elake(i)%noutlet
            DO k = 1,nsub_basemodel
              IF(branchdata(branchindex(elake(i)%outlet(j)%isb))%branch==k) EXIT
            ENDDO
            IF(k<klast) s = s + 1
          ENDDO
        ENDDO
        ret = (s == 0)
        CALL add_generic_result(ret,s,a_linklbsecondout,t_lakedata_generic,testing_names(a_linklbsecondout))
        IF(.NOT. ret) status = status + 1
      ENDIF
    ENDIF

    ret = (status == 0)
    CALL print_testresult_logical('lakedata',ret,suffix='')
    CALL print_test_process(t_lakedata_generic)

    status = status + old_status

  END SUBROUTINE check_lakedata_generic

  !> Generic DamData tests
  !> TODO: The damdata tests are not complete. Need to be looked into.
  !--------------------------------------------------------------------
  SUBROUTINE check_damdata_generic(status)

    USE MODVAR, ONLY : dam
    USE HYPEVARIABLES, ONLY : n_limprod

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    INTEGER i
    LOGICAL ret,ret2

    ret = .TRUE.
    !> @test Checks for positive regulation volume, production flow amplitude 
    !>between zero and one, and minimum production flow not negative.
    IF (ALLOCATED(dam)) THEN
      !ret = valid_dam_purpose(current_dam)
      !CALL check_input(ret,dam(:)%purpose,gt_zero,a_damregvol,t_damdata_generic)
      CALL check_input(ret,dam(:)%regvol,gt_zero,a_damregvol,t_damdata_generic)
      CALL check_input(ret,dam(:)%qamp,ge_zero_and_le_one,a_damqamp,t_damdata_generic)
      CALL check_input(ret,dam(:)%qinfmin,ge_zero,a_damqinfmin,t_damdata_generic)
      !> @test Check that production flow is given in some form;
      !>If qprod1<=0 then require qinfmed>=0, if qprod1>0 then require qprod2>=0.
      DO i=1,SIZE(dam)
        ret2 = .TRUE.
        IF(dam(i)%qprod1<0.)THEN
          IF(dam(i)%qinfmed<0.) ret2 = .FALSE.
        ELSE
          IF(dam(i)%qprod2<0.) ret2 = .FALSE.
        ENDIF
      ENDDO
      CALL add_generic_result(ret2,0,a_damqprod,t_damdata_generic,testing_names(a_damqprod))
      ret = ret.AND.ret2
      !> @test Check if reduced production flow (limqprod) at low water levels is correctly given (0-1).
      ret2 = .TRUE.
      CALL check_input(ret2,dam(:)%limprod,ge_zero_and_le_one,a_damlimprod,t_damdata_generic)
      IF(.NOT.ret2)THEN
        ret2 = .TRUE.
        CALL check_param(ret2,ge_zero_and_le_one,n_limprod,t_damdata_generic)
        CALL add_generic_result(ret2,0,a_damlimprod,t_damdata_generic,testing_names(a_damlimprod))
      ENDIF
      ret = ret.AND.ret2
        
      IF(.NOT.ret) status = status + 1
    ENDIF

    CALL print_testresult_logical('damdata',ret,suffix='')
    CALL print_test_process(t_damdata_generic)

  END SUBROUTINE check_damdata_generic

  !> Generic tests
  !> @brief The generic tests include tests that doesn't belong to any
  !> specific setup file or tests of a more general nature.
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_generic(status)

    USE HYPEVARIABLES, ONLY : n_snalbmin,n_snalbmax, &
                              n_wcfc,n_wcfc1,n_wcfc2,n_wcfc3, &
                              n_wcwp,n_wcwp1,n_wcwp2,n_wcwp3, &
                              n_wcep,n_wcep1,n_wcep2,n_wcep3, &
                              n_sndens0,n_deepmem,n_surfmem,n_depthrel

    USE MODVAR, ONLY : num_modelprocess,modeloption,maxsoillayers, &
                       maxmodelsinoption,soilpar,modparid,nsoil, &
                       conduct,p_erosion,basin

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    INTEGER i,j,s(maxsoillayers)
    LOGICAL ret,r(maxsoillayers)
    REAL :: val_wcfc(maxsoillayers,nsoil)
    REAL :: val_wcwp(maxsoillayers,nsoil)
    REAL :: val_wcep(maxsoillayers,nsoil)
    REAL :: sum_val

    !> @test Check modeloptions within range:
    !> The modeloptions are checked for illegal choices of numbers
    ret = .TRUE.
    DO i = 1, num_modelprocess
        IF (modeloption(i) .GT. maxmodelsinoption(i) .OR. modeloption(i) .LT. 0) THEN
            status = status + 1
            ret = .FALSE.
        END IF
    END DO
    CALL add_generic_result(ret,status,a_modeloptionrange,t_generic,'modeloption range')
    CALL print_testresult_logical('modeloption range',ret,suffix='')

    !> @test Check snow albedo parameter ranges:
    !> The snalbmin shall always be less than snalbmax as well as
    !> they shall never be a negative value.
    ret = .TRUE.
    IF (n_snalbmin < n_snalbmax) THEN
        CALL check_param(ret,ge_zero_and_le_one,n_snalbmin,t_generic)
        CALL check_param(ret,ge_zero_and_le_one,n_snalbmax,t_generic)
    ELSE
        ret = .FALSE.
        status = status + 1
    END IF
    CALL add_generic_result(ret,0,a_snalbrange,t_generic,'snalbminmax range')
    CALL print_testresult_logical('snalbminmax range',ret,suffix='')

    !> @test Check soil water holding capacities: wcfc,wcwp and wcep between zero and one.
    ret = .TRUE.
    CALL check_param(ret,ge_zero_and_lt_one,n_wcfc,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcwp,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcep,t_generic)

    CALL check_param(ret,ge_zero_and_lt_one,n_wcfc1,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcfc2,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcfc3,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcwp1,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcwp2,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcwp3,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcep1,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcep2,t_generic)
    CALL check_param(ret,ge_zero_and_lt_one,n_wcep3,t_generic)

    ! Check wcfc,wcwp and wcep sums
    val_wcfc(1,:) = soilpar(modparid(n_wcfc1)%parno,:)
    val_wcfc(2,:) = soilpar(modparid(n_wcfc2)%parno,:)
    val_wcfc(3,:) = soilpar(modparid(n_wcfc3)%parno,:)
    val_wcwp(1,:) = soilpar(modparid(n_wcwp1)%parno,:)
    val_wcwp(2,:) = soilpar(modparid(n_wcwp2)%parno,:)
    val_wcwp(3,:) = soilpar(modparid(n_wcwp3)%parno,:)
    val_wcep(1,:) = soilpar(modparid(n_wcep1)%parno,:)
    val_wcep(2,:) = soilpar(modparid(n_wcep2)%parno,:)
    val_wcep(3,:) = soilpar(modparid(n_wcep3)%parno,:)

    !Initialize result and status
    DO j = 1, maxsoillayers
        r(j) = .TRUE.
        s(j) = 0
    END DO

    ret = .TRUE.
    CALL add_generic_result(ret,0,a_wcwpwcfcwcep,t_generic,'wcwp+wcfc+wcep')
    DO i = 1, nsoil
      DO j = 1, maxsoillayers
        !wcfc
        IF (val_wcfc(j,i) <= 0) THEN
          val_wcfc(j,i) = soilpar(modparid(n_wcfc)%parno,i)
        END IF
        !wcwp
        IF (val_wcwp(j,i) <= 0) THEN
          val_wcwp(j,i) = soilpar(modparid(n_wcwp)%parno,i)
        END IF
        !wcep
        IF (val_wcep(j,i) <= 0) THEN
          val_wcep(j,i) = soilpar(modparid(n_wcep)%parno,i)
        END IF
        !> @test Check that for all soil types and for their soillayers the sum
        !> of wilting point, field capacity and effective porosity are within
        !> the range greater than zero to less than or equal to zero
        sum_val = val_wcfc(j,i) + val_wcwp(j,i) + val_wcep(j,i)
        IF (sum_val <= 0 .OR. sum_val > 1) THEN
          ret = .FALSE.
          r(j) = ret
          s(j) = s(j) + 1
          CALL add_generic_result(r(j),s(j),a_wcwpwcfcwcep,t_generic,'wcwp+wcfc+wcep')
        END IF
      END DO
    END DO

    CALL print_testresult_logical('water holding capacity',ret,suffix='')
    status = status + SUM(s(:))

    !> @test Check that region is larger than zero
    IF (conduct%irrigation .OR. conduct%simN .OR. conduct%simP .OR. conduct%simC .OR. (conduct%simS .AND. modeloption(p_erosion)==0)) THEN
      ret = (MINVAL(basin(:)%region) > 0)
      CALL add_generic_result(ret,0,a_basinregion,t_generic,testing_names(a_basinregion))
      IF (.NOT. ret) THEN
        status = status + 1
      END IF
      CALL print_testresult_logical('region set',ret,suffix='')
    END IF
    
    !> @test Check that soil temperature is calculated (parameters set) for WQ-simulations.
    IF (conduct%simN .OR. conduct%simP .OR. conduct%simC) THEN
      ret = .TRUE.
      CALL check_param(ret,ge_one,n_deepmem,t_generic)
      CALL check_param(ret,ge_one,n_surfmem,t_generic)
      CALL check_param(ret,ge_zero,n_depthrel,t_generic)
      CALL check_param(ret,gt_zero,n_sndens0,t_generic)
      IF (.NOT. ret) status = status + 1
      CALL print_testresult_logical('soil temperature',ret,suffix='')
    ENDIF

    !Maybe move some to specific hydrological processes
    CALL print_test_process(t_generic)

  END SUBROUTINE check_generic

  !> Check and test lakes options
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_lakes(status)

    USE MODVAR, ONLY : branchindex,connectivitymodelnames, &
                       lakeout2index, &
                       modeloption,nsub_basemodel, &
                       p_connectivity,p_swtemperature, &
                       swtemperaturemodelnames
                       

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER i

    !> @test Check for valid surface water temperature model options
    ret = valid_swtemperature_options(modeloption(p_swtemperature))
    CALL print_testresult_logical(swtemperaturemodelnames(modeloption(p_swtemperature)),ret,prefix='swtemperaturemodel: ')
    IF(.NOT. ret) THEN
      status = status + 1
    END IF

    !> @test Check for valid ilake connectivity model options
    ret = valid_connectivity_options(modeloption(p_connectivity))
    CALL print_testresult_logical(connectivitymodelnames(modeloption(p_connectivity)),ret,prefix='connectivity: ')
    IF(.NOT. ret) THEN
      status = status + 1
    END IF

    !> @test Check that lakes with two outlets have a branch defined
    ret = .TRUE.
    IF(ALLOCATED(lakeout2index))THEN  !Second outlet of lake/dam
      DO i = 1, nsub_basemodel
        IF(lakeout2index(i)>0)THEN
          ret = ret .AND. (branchindex(i) > 0)
        ENDIF
      ENDDO
      CALL add_generic_result(ret,0,a_branchdefined,t_lakes,testing_names(a_branchdefined))
      CALL print_testresult_logical('lakes with two outlets',ret)
    ELSE
      CALL print_testresult_logical('no lakes with two outlets',ret)
    ENDIF
    IF(.NOT. ret)THEN
      status = status + 1
    ENDIF

    CALL print_test_process(t_lakes)

  END SUBROUTINE check_lakes

  !> Check and test specified swtemperature model for valid options
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_swtemperature_options(current_swtemperaturemodel)

    USE HYPEVARIABLES, ONLY : n_laketemp
    USE MODVAR, ONLY : conduct

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_swtemperaturemodel  !<swtemperaturemodel to be tested for valid options

    !Local variables

    valid_swtemperature_options = .TRUE.
    SELECT CASE(current_swtemperaturemodel)
      CASE(0)
        !laketemp          >= 0.0               --> OK
        CALL check_param(valid_swtemperature_options,ge_zero,n_laketemp,t_lakes)
      CASE(1)
        !i_t2>0            TRUE                 --> OK
        CALL add_generic_result(conduct%simT2,0,a_t2sim,t_lakes,testing_names(a_t2sim))
        valid_swtemperature_options = valid_swtemperature_options .AND. conduct%simT2
    END SELECT

  END FUNCTION valid_swtemperature_options

  !> Check and test specified swtemperature model for valid options
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_connectivity_options(current_connectivitymodel)

    USE HYPEVARIABLES, ONLY : n_laketemp
    USE MODVAR, ONLY : basin,lakesectiondata,nsub_basemodel

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_connectivitymodel  !<connectivity to be tested for valid options

    !Local variables
    INTEGER i
    REAL    x,y,z(1)
    LOGICAL ret,ret2,ret3,ret4
    
    valid_connectivity_options = .TRUE.
    SELECT CASE(current_connectivitymodel)
      CASE(0)
        !check lks_num=0?
        !lakesectiondata already deallocated so cannot check
      CASE(1) !Ilake fill-and-spill model
        ret = .TRUE.
        ret2 = .TRUE.
        ret3 = .TRUE.
        ret4 = .TRUE.
        DO i=1,nsub_basemodel
          IF(basin(i)%lakesection>0)THEN
          CALL check_input(ret,lakesectiondata(i,1:basin(i)%lakesection)%ficatch,ge_zero,a_lksficatch,t_lakes,force=.TRUE.)
          CALL check_input(ret2,lakesectiondata(i,1:basin(i)%lakesection)%farea,gt_zero,a_lksfarea,t_lakes,force=.TRUE.)
          !The two above will only write values for last i checked to test file, missleading.
          !The two below could be tested with check_input if it accepted single value
          x = SUM(lakesectiondata(i,1:basin(i)%lakesection)%ficatch)
          ret3 = ret3 .AND. (x>=0.999) .AND. (x<=1.001) !within 0.1%
          y = SUM(lakesectiondata(i,1:basin(i)%lakesection)%farea)
          ret4 = ret4 .AND. (y>=0.999) .AND. (y<=1.001) !within 0.1%
          ENDIF
        ENDDO
        CALL add_generic_result(ret3,0,a_sum_lksficatch,t_lakes,testing_names(a_sum_lksficatch))
        CALL add_generic_result(ret4,0,a_sum_lksfarea,t_lakes,testing_names(a_sum_lksfarea))
        valid_connectivity_options = valid_connectivity_options .AND. ret .AND. ret2 .AND. ret3 .AND. ret4
        !Check grat,ilrat set? Nah, other place if any ilake
      CASE(2) !HGDM model
        ret = .TRUE.
        CALL check_input(ret,basin(:)%hgdmdepth,ge_zero,a_hgdmdep1,t_lakes)
        z(1) = SUM(basin(:)%hgdmdepth)
        CALL check_input(ret,z,gt_zero,a_hgdmdep2,t_lakes)
        valid_connectivity_options = valid_connectivity_options .AND. ret
    END SELECT

  END FUNCTION valid_connectivity_options

  !> Check nutrient sources; fertilization, plant residues and rural household effluents
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_nutrient_sources(status)

    USE MODVAR, ONLY : modeloption,p_soilleakage,conduct,cropdata,load, &
                       i_in,i_on,i_sp,i_pp,numsubstances
    USE HYPEVARIABLES, ONLY : n_fertdays

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret,ret2,ret3
    INTEGER i
    REAL minvalue(numsubstances)

    ret = .TRUE.
    IF(modeloption(p_soilleakage)==0)THEN
      IF (conduct%simN .OR. conduct%simP) THEN
        !> @test Check fertilization input: day of application >0, amount of 
        !>fertilization >=0, fraction ploughed dow to second soil layer between 
        !>0 and 1, period length of application between 0 and 366.
        ret2 = .TRUE.
        IF (ALLOCATED(cropdata)) THEN
          CALL check_input(ret2,REAL(cropdata(:)%fertday1),ge_zero,a_fertday1,t_nut_source)
          CALL check_input(ret2,REAL(cropdata(:)%fertday2),ge_zero,a_fertday2,t_nut_source)
          CALL check_input(ret2,REAL(cropdata(:)%manday1),ge_zero,a_manday1,t_nut_source)
          CALL check_input(ret2,REAL(cropdata(:)%manday2),ge_zero,a_manday2,t_nut_source)
          CALL check_input(ret2,cropdata(:)%fertdown1,ge_zero_and_le_one,a_fertdown1,t_nut_source)
          CALL check_input(ret2,cropdata(:)%fertdown2,ge_zero_and_le_one,a_fertdown2,t_nut_source)
          CALL check_input(ret2,cropdata(:)%mandown1,ge_zero_and_le_one,a_mandown1,t_nut_source)
          CALL check_input(ret2,cropdata(:)%mandown2,ge_zero_and_le_one,a_mandown2,t_nut_source)
          IF(conduct%simN)THEN
            CALL check_input(ret2,cropdata(:)%fertnamount1,ge_zero,a_fertnamount1,t_nut_source)
            CALL check_input(ret2,cropdata(:)%fertnamount2,ge_zero,a_fertnamount2,t_nut_source)
            CALL check_input(ret2,cropdata(:)%mannamount1,ge_zero,a_mannamount1,t_nut_source)
            CALL check_input(ret2,cropdata(:)%mannamount2,ge_zero,a_mannamount2,t_nut_source)
          ENDIF
          IF(conduct%simP)THEN
            CALL check_input(ret2,cropdata(:)%fertpamount1,ge_zero,a_fertpamount1,t_nut_source)
            CALL check_input(ret2,cropdata(:)%fertpamount2,ge_zero,a_fertpamount2,t_nut_source)
            CALL check_input(ret2,cropdata(:)%manpamount1,ge_zero,a_manpamount1,t_nut_source)
            CALL check_input(ret2,cropdata(:)%manpamount2,ge_zero,a_manpamount2,t_nut_source)
          ENDIF
          CALL check_param(ret2,ge_zero_and_le_366,n_fertdays,t_nut_source)
          CALL print_testresult_logical('fertilization',ret2)
          IF (.NOT. ret2) THEN
              status = status + 1
          END IF
          ret = ret.AND.ret2
        ELSE
          CALL print_testresult_logical('no fertilization applied',ret2,suffix='')
        ENDIF
          
        !> @test Check application of plant residues: day of application >=0, 
        !>amount >=0, fraction ploughed down and fraction of fast degrading 
        !>type between 0 and 1.
        ret2 = .TRUE.
        IF (ALLOCATED(cropdata)) THEN
          CALL check_input(ret2,REAL(cropdata(:)%resdayno),ge_zero,a_resdayno,t_nut_source)
          CALL check_input(ret2,cropdata(:)%resdown,ge_zero_and_le_one,a_resdown,t_nut_source)
          CALL check_input(ret2,cropdata(:)%resfast,ge_zero_and_le_one,a_resfast,t_nut_source)
          IF(conduct%simN) CALL check_input(ret2,cropdata(:)%resnamount,ge_zero,a_resnamount,t_nut_source)
          IF(conduct%simP) CALL check_input(ret2,cropdata(:)%respamount,ge_zero,a_respamount,t_nut_source)
          CALL print_testresult_logical('plant residues',ret2)
          IF (.NOT. ret2) THEN
              status = status + 1
          END IF
          ret = ret.AND.ret2
        ELSE
          CALL print_testresult_logical('no plant residues applied',ret2,suffix='')
        ENDIF

      ELSE
        ret = .TRUE.
        CALL print_testresult_logical('no fert or plant res sources simulated',ret,suffix='')
      ENDIF
    ENDIF
    
    !> @test Check local rural household source: flow not negative, nutrient concentration not negative (N and P)
    ret2 = .TRUE.
    IF(ALLOCATED(load))THEN
      CALL check_input(ret2,load(:)%volloc,ge_zero,a_volloc,t_nut_source)
      IF (conduct%simN .OR. conduct%simP) THEN
        minvalue = 0.
        ret3 = .TRUE.
        IF(conduct%simN) THEN
          DO i = 1,SIZE(load)
            minvalue(i_in) = MIN(minvalue(i_in),load(i)%locconc(i_in))
            minvalue(i_on) = MIN(minvalue(i_on),load(i)%locconc(i_on))
          ENDDO
          ret3 = ret3 .AND. (minvalue(i_in)>= 0.0) .AND. (minvalue(i_on)>= 0.0)
        ENDIF  
        IF(conduct%simP) THEN
          DO i = 1,SIZE(load)
            minvalue(i_sp) = MIN(minvalue(i_sp),load(i)%locconc(i_sp))
            minvalue(i_pp) = MIN(minvalue(i_pp),load(i)%locconc(i_pp))
          ENDDO
          ret3 = ret3 .AND. (minvalue(i_sp)>= 0.0) .AND. (minvalue(i_pp)>= 0.0)
        ENDIF  
        CALL add_generic_result(ret3,0,a_locconcnp,t_nut_source,'locconc NP')
        ret2 = ret2 .AND. ret3
      ENDIF
      CALL print_testresult_logical('rural load',ret2)
      IF (.NOT. ret2) THEN
        status = status + 1
      END IF
      ret = ret.AND.ret2
    ELSE
      CALL print_testresult_logical('no rural household sources simulated',ret2,suffix='')
    ENDIF
          
    CALL print_test_process(t_nut_source)

  END SUBROUTINE check_nutrient_sources

  !> Check vegetation and soil surface processes
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_veg_soil_surface(status)

    USE MODVAR, ONLY : modeloption,p_growthstart,growthstartmodelnames, &
                       p_erosion,erosionmodelnames,p_soilleakage, conduct

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    IF(modeloption(p_soilleakage)==0)THEN
      IF (conduct%simN .OR. conduct%simP) THEN
          !> @test Check and test specified growthstart model for valid options: specific test specified elsewhere
          ret = valid_growthstart_options(modeloption(p_growthstart))
          CALL print_testresult_logical(growthstartmodelnames(modeloption(p_growthstart)),ret,prefix='growthstartmodel: ')
          IF (.NOT. ret) THEN
              status = status + 1
          END IF
          !> @test Check plant uptake parameters larger or equal to zero, 
          !>secondary crops given as input and wilting point above zero.
          ret = check_plant_uptake()
          CALL print_testresult_logical('plant uptake',ret)
          IF (.NOT. ret) THEN
              status = status + 1
          END IF
      END IF

      IF (conduct%simP .OR. conduct%simS) THEN
          !> @test Check if erosion is simulated: parameters can be used to turn it off
          ret = check_erosion_on(modeloption(p_erosion))
          IF(ret)THEN
            !> @test Check specified erosion model for valid options
            ret = valid_erosion_options(modeloption(p_erosion))
            CALL print_testresult_logical(erosionmodelnames(modeloption(p_erosion)),ret,prefix='erosionmodel: ')
            IF (.NOT. ret) THEN
                status = status + 1
            END IF
            !> @test Check filtering of eroded material (and in future release pool): specific test specified elsewhere
            ret = check_eroded_filtering_delay()
            CALL print_testresult_logical('filtering and delay of erosion',ret)
            IF (.NOT. ret) THEN
                status = status + 1
            END IF
          ELSE
            CALL print_testresult_logical('no soil erosion simulated',ret,suffix='')
          ENDIF
      END IF
    ELSE
      ret = .TRUE.
      CALL print_testresult_logical('no vegetation and soil surface processes simulated',ret,suffix='')
    ENDIF

    CALL print_test_process(t_veg_soil_surface)

  END SUBROUTINE check_veg_soil_surface

  !> Check and test specified growthstart model for valid options
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_growthstart_options(current_growthstartmodel)

    USE MODVAR, ONLY : basin,cropdata,timesteps_per_day

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_growthstartmodel  !<growthstartmodel to be tested for valid options

    !Local variables
    LOGICAL r

    valid_growthstart_options = .TRUE.
    SELECT CASE(current_growthstartmodel)
    CASE(0)
      !> @test Checks for growthstartmodel 0: sawing date set
      CALL check_input(valid_growthstart_options,REAL(cropdata(:)%baredayno2),gt_zero,a_bd2,t_veg_soil_surface)  !should also be <366
    CASE(1)
      !> @test Checks for growthstartmodel 1: earliest starting date (firstday)
      !>and dayhour limit (daylength) for degreeday accumulation set (positive) 
      !>as well as degreedaysum determining saw date (gddsow>0).
      CALL check_file_existence(valid_growthstart_options,'CropData.txt',a_CropData,t_veg_soil_surface)
      IF (ALLOCATED(cropdata)) THEN
        CALL check_input(valid_growthstart_options,cropdata(:)%gddsow,gt_zero,a_gddsow,t_veg_soil_surface)
        CALL check_input(valid_growthstart_options,cropdata(:)%daylength,gt_zero,a_daylength,t_veg_soil_surface)
        CALL check_input(valid_growthstart_options,REAL(cropdata(:)%firstday),gt_zero_and_lt_366,a_firstday,t_veg_soil_surface)
      END IF
      IF(timesteps_per_day/=1)THEN
        r = .FALSE.
        CALL add_generic_result(r,0,a_dailytimestep,t_veg_soil_surface,'daily time step')
        valid_growthstart_options = valid_growthstart_options .AND. r
      ENDIF  
    END SELECT

    !>@test Check latitude set and between +-90 degree, necessary for southern hemisphere and/or growthstart model 1
    !Latitude  >= -90.0 AND <= 90.0         --> OK
    !Latitude  MIN() != 0.0 OR MAX() != 0.0 --> OK
    CALL check_input(valid_growthstart_options,basin(:)%latitude,plus_minus_90,a_latitude,t_veg_soil_surface)
    r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
    CALL add_generic_result(r,0,a_latitude_defined,t_veg_soil_surface,'latitude defined')
    valid_growthstart_options = valid_growthstart_options .AND. r

  END FUNCTION valid_growthstart_options

  !> Check and test specified erosion model for valid options
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_erosion_options(current_erosionmodel)

    USE HYPEVARIABLES, ONLY : n_soilcoh,n_soilerod,n_sreroexp, &
                              n_erodluse,n_erodsoil,n_erodslope,n_erodexp,n_erodindex, &
                              n_incorr,n_oncorr,n_phoscorr
    USE MODVAR, ONLY : basin,cropdata,conduct

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_erosionmodel  !<erosionmodel to be tested for valid options

    !Local variables

    valid_erosion_options = .TRUE.
    SELECT CASE(current_erosionmodel)
    CASE(0)
      !> @test Checks for erosionmodel 0: Existance of CropData.txt, allowed 
      !>values for parameters for soil characteristics (soilcoh,soilerod,sreroexp,
      !>incorr,oncorr,phoscorr), crop cover and ground cover between zero and one.
      IF(conduct%simS) THEN
        CALL check_file_existence(valid_erosion_options,'CropData.txt',a_CropData,t_veg_soil_surface)
      ENDIF
      CALL check_param(valid_erosion_options,gt_zero,n_soilcoh,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,gt_zero,n_soilerod,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,ge_zero,n_sreroexp,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,gt_minus_one_and_lt_plus_one,n_incorr,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,gt_minus_one_and_lt_plus_one,n_oncorr,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,gt_minus_one_and_lt_plus_one,n_phoscorr,t_veg_soil_surface)
      CALL check_input(valid_erosion_options,cropdata(:)%ccmax1,ge_zero_and_le_one,a_ccmax1,t_veg_soil_surface)
      CALL check_input(valid_erosion_options,cropdata(:)%ccmax2,ge_zero_and_le_one,a_ccmax2,t_veg_soil_surface)
      CALL check_input(valid_erosion_options,cropdata(:)%gcmax1,ge_zero_and_le_one,a_gcmax1,t_veg_soil_surface)
      CALL check_input(valid_erosion_options,cropdata(:)%gcmax2,ge_zero_and_le_one,a_gcmax2,t_veg_soil_surface)
    CASE(1)
      !> @test Checks for erosion model 1: parameters for soil erosion (erodluse,erodsoil,erodslope,erodexp,erodindex), and erosion index for each subbasins
      CALL check_param(valid_erosion_options,ge_zero,n_erodluse,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,ge_zero,n_erodsoil,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,ge_zero,n_erodslope,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,ge_zero,n_erodexp,t_veg_soil_surface)
      CALL check_param(valid_erosion_options,gt_zero,n_erodindex,t_veg_soil_surface)
      CALL check_input(valid_erosion_options,basin(:)%eroindex,ge_zero,a_eroindex,t_veg_soil_surface)
    END SELECT

    !>Check basin slope greater or equal to zero and below 100.
    CALL check_input(valid_erosion_options,basin(:)%slope,ge_zero_and_le_hundred,a_slope,t_veg_soil_surface)

  END FUNCTION valid_erosion_options

  !> Check if erosion is simulated, i.e if more tests are needed
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION check_erosion_on(current_erosionmodel)

    USE HYPEVARIABLES, ONLY : n_soilcoh,n_soilerod,n_erodsoil,n_erodluse
    USE MODVAR, ONLY : basin, modparid, soilpar, landpar

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_erosionmodel  !<erosionmodel to be tested for valid options

    !Local variables
    INTEGER :: s
    LOGICAL :: ret

    check_erosion_on = .TRUE.

    !>Check parameters for turning off erosion model
    SELECT CASE(current_erosionmodel)
      CASE(0)
        !Initialize result and status
        s = 0

        !>Erosion model 0: Erosion on : MAX(soilcoh) != 0.0 AND MAX(soilerod) != 0.0 --> OK
        ret = (MAXVAL(soilpar(modparid(n_soilcoh)%parno,:)) /= 0.0 .AND. MAXVAL(soilpar(modparid(n_soilerod)%parno,:)) /= 0.0)
        IF(.NOT.ret)THEN
          s = s + 1
          CALL check_param(check_erosion_on,gt_zero,n_soilcoh,t_veg_soil_surface)
          CALL check_param(check_erosion_on,gt_zero,n_soilerod,t_veg_soil_surface)
        ENDIF
        CALL add_generic_result(ret,s,a_erosionon,t_veg_soil_surface,'erosion_on')
    
      CASE(1)
        !Initialize result and status
        s = 0

        !>Erosion model 1: Erosion on : MAX(erodsoil) != 0.0 AND MAX(erodluse) != 0.0 --> OK
        ret = (MAXVAL(soilpar(modparid(n_erodsoil)%parno,:)) /= 0.0 .AND. MAXVAL(landpar(modparid(n_erodluse)%parno,:)) /= 0.0)
        IF(.NOT.ret)THEN
          s = s + 1
          CALL check_param(check_erosion_on,gt_zero,n_erodsoil,t_veg_soil_surface)  !add param values to test output
          CALL check_param(check_erosion_on,gt_zero,n_erodluse,t_veg_soil_surface)
        ENDIF
        CALL add_generic_result(ret,s,a_erosionon,t_veg_soil_surface,'erosion_on')
        
        !>Erosion model 2: Erosion on : MAX(eroindex) > 0.0  --> OK
        IF(MAXVAL(basin(:)%eroindex)==0.)THEN
          s = s + 1
          CALL check_input(ret,basin(:)%eroindex,gt_zero,a_eroindex,t_veg_soil_surface)  !add input values to test output
          ret = .FALSE.
          CALL add_generic_result(ret,s,a_erosionon,t_veg_soil_surface,'erosion_on')
        ENDIF
    END SELECT
    
    check_erosion_on = ret

  END FUNCTION check_erosion_on

  !> Check and test filtering of eroded material and release pool
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION check_eroded_filtering_delay()

    USE HYPEVARIABLES, ONLY : n_filtPbuf,n_filtPinner,n_filtPother,n_macfilt, &
                              n_pprelmax,n_pprelexp
    USE MODVAR, ONLY : basin

    !Argument declaration

    !Local variables

    check_eroded_filtering_delay = .TRUE.

    !> @test Check GeoData input (buffer, closewater) and parameters (bufffilt, innerfilt, otherfilt, macrofilt) used for filtering
    CALL check_input(check_eroded_filtering_delay,basin(:)%buffer,ge_zero_and_le_one,a_buffer,t_veg_soil_surface)
    CALL check_input(check_eroded_filtering_delay,basin(:)%closewater,ge_zero_and_le_one,a_closewater,t_veg_soil_surface)
    CALL check_param(check_eroded_filtering_delay,ge_zero_and_le_one,n_filtPbuf,t_veg_soil_surface)
    CALL check_param(check_eroded_filtering_delay,ge_zero_and_le_one,n_filtPinner,t_veg_soil_surface)
    CALL check_param(check_eroded_filtering_delay,ge_minus_one_and_le_plus_one,n_filtPother,t_veg_soil_surface)
    CALL check_param(check_eroded_filtering_delay,ge_zero_and_le_one,n_macfilt,t_veg_soil_surface)

    !> @test Check parameters for temporary delay of SS and PP from surface 
    !>runoff and tile drains (pprelmax, pprelexp) greater or equal to zero   
    CALL check_param(check_eroded_filtering_delay,ge_zero,n_pprelmax,t_veg_soil_surface)
    CALL check_param(check_eroded_filtering_delay,ge_zero,n_pprelexp,t_veg_soil_surface)
    
  END FUNCTION check_eroded_filtering_delay

  !> Check and test plant uptake parameters larger or equal to zero, 
  !> secondary crops given as input and wilting point above zero.
  !--------------------------------------------------------------------
  LOGICAL FUNCTION check_plant_uptake()

    USE MODVAR, ONLY : conduct,cropdata,classbasin,classdata,nclass,soilpar,&
                       soilthick,slc_ilake,slc_olake,slc_mriver,slc_lriver
    USE HYPEVARIABLES, ONLY : m_wcwp,m_wcwp1,m_wcwp2,m_wcwp3

    !Argument declaration

    !Local variables
    LOGICAL ret
    INTEGER j

    check_plant_uptake = .TRUE.

    !> @test Check fraction of area given for secondary crops
    ret = .TRUE.
    DO j = 1, nclass
      IF(MAXVAL(classbasin(:,j)%part2cr)>0.)THEN
        ret = ret .AND. (classdata(j)%crop2 > 0)
      ENDIF
    ENDDO
    CALL add_generic_result(ret,0,a_scr_exist,t_veg_soil_surface,'a_scr_exist')
    check_plant_uptake = check_plant_uptake.AND.ret
    
    !> @test Check wilting point water volume larger than zero.
    ret = .TRUE.
    DO j= 1, nclass
      IF(soilpar(m_wcwp1,classdata(j)%soil) > 0.)THEN
        IF(soilthick(2,j)>0.) ret = ret .AND. (soilpar(m_wcwp2,classdata(j)%soil) > 0.)
        IF(soilthick(3,j)>0.) ret = ret .AND. (soilpar(m_wcwp3,classdata(j)%soil) > 0.)
      ELSEIF(soilpar(m_wcwp,classdata(j)%soil) > 0.)THEN
!              ret = .TRUE.  !ok
      ELSEIF(j==slc_ilake.OR.j==slc_olake.OR.j==slc_mriver.OR.j==slc_lriver)THEN
        !ok
      ELSE
        ret = .FALSE.
      ENDIF
    ENDDO
    CALL add_generic_result(ret,0,a_wcwp_gt_zero,t_veg_soil_surface,'wcwp greater than zero')
    check_plant_uptake = check_plant_uptake.AND.ret
    
    !> @test Check CropData input for beginning and end of growing season, plant uptake (fraction 
    !> from upper soil layer and NP-relation) as well as parameters for plant uptake set.
    CALL check_input(check_plant_uptake,REAL(cropdata(:)%baredayno3),gt_zero,a_bd3,t_veg_soil_surface)  !should also be <366
    CALL check_input(check_plant_uptake,REAL(cropdata(:)%baredayno5),ge_zero,a_bd5,t_veg_soil_surface)  !should also be <366
    CALL check_input(check_plant_uptake,cropdata(:)%uptakeupper,ge_zero_and_le_one,a_uptakeupper,t_veg_soil_surface)
    IF(conduct%simP) CALL check_input(check_plant_uptake,cropdata(:)%PNuptakeRatio,gt_zero,a_uptakeratio,t_veg_soil_surface)
    CALL check_input(check_plant_uptake,cropdata(:)%uptake1,ge_zero,a_uptake1,t_veg_soil_surface)
    CALL check_input(check_plant_uptake,cropdata(:)%uptake2,ge_zero,a_uptake2,t_veg_soil_surface)
    CALL check_input(check_plant_uptake,cropdata(:)%uptake3,ge_zero,a_uptake3,t_veg_soil_surface)
          
  END FUNCTION check_plant_uptake

  !> Check vegetation and soil surface processes
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_nutrient_soil(status)

    USE MODVAR, ONLY : conduct,modeloption,p_soilleakage
    USE HYPEVARIABLES, ONLY : n_hnhalf,n_humusn0,n_fastn0,n_onconc0, &
                              n_hphalf,n_humusp0,n_fastp0,n_ppconc0, &
                              n_phoscorr,n_partp0,n_pphalf,n_partp1, &
                              n_partp2,n_partp3,n_ppconc0,n_denitrlu, &
                              n_hsatINsoil,n_denit3reg,n_dissolfn,n_dissolfp, &
                              n_dissolhn,n_dissolhp,n_minerfn,n_minerfp, &
                              n_degradhn,n_degradhp,n_incorr,n_oncorr, &
                              n_onpercred,n_pppercred,n_freuc,n_freuexp,n_freurate
    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret,ret2

    IF(modeloption(p_soilleakage)==0)THEN
      IF (conduct%simN .OR. conduct%simP) THEN
        !> @test Check and test initial values set larger or larger and equal to zero.
        ret = .TRUE.
        IF (conduct%simN) THEN
          CALL check_param(ret,gt_zero,n_hnhalf,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_humusn0,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_fastn0,t_nutrient_soil)
          CALL check_param(ret,ge_zero,n_onconc0,t_nutrient_soil)
        ENDIF
        IF (conduct%simP) THEN
          CALL check_param(ret,gt_zero,n_hphalf,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_humusp0,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_fastp0,t_nutrient_soil)
          CALL check_param(ret,gt_minus_one_and_lt_plus_one,n_phoscorr,t_nutrient_soil)
          !PartP may be initialised two ways; if partp0>0 require pphalf>0, else require partp1-3>0
          !TODO: maybe check initalisation parameters per class/landuse instead
          ret2 = .TRUE.
          CALL check_param(ret2,gt_zero,n_partp0,t_nutrient_soil)
          IF(ret2)THEN
            CALL check_param(ret2,gt_zero,n_pphalf,t_nutrient_soil)
          ELSE
            CALL check_param(ret2,gt_zero,n_partp1,t_nutrient_soil)
            CALL check_param(ret2,gt_zero,n_partp2,t_nutrient_soil)
            CALL check_param(ret2,gt_zero,n_partp3,t_nutrient_soil)
          ENDIF
          ret = ret.AND.ret2
          CALL check_param(ret,ge_zero,n_ppconc0,t_nutrient_soil)
        ENDIF
        CALL print_testresult_logical('initial values',ret)
        IF (.NOT. ret) THEN
            status = status + 1
        ENDIF
      
        IF (conduct%simN) THEN
          ret = .TRUE.
          !> @test Check soil denitrification parameters larger or equal to zero.
          CALL check_param(ret,ge_zero,n_denitrlu,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_hsatINsoil,t_nutrient_soil)
          !denitr3lu can be positve, negative and zero, no check
          CALL check_param(ret,ge_zero,n_denit3reg,t_nutrient_soil)
          CALL print_testresult_logical('denitrification',ret)
          IF (.NOT. ret) THEN
              status = status + 1
          ENDIF
        ENDIF

        !> @test Check and test parameters for nutrient soil pool transformations.
        ret = .TRUE.
        IF (conduct%simN) THEN
          CALL check_param(ret,gt_zero,n_dissolfn,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_dissolhn,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_minerfn,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_degradhn,t_nutrient_soil)
          CALL check_param(ret,gt_minus_one_and_lt_plus_one,n_incorr,t_nutrient_soil)
          CALL check_param(ret,gt_minus_one_and_lt_plus_one,n_oncorr,t_nutrient_soil)
        ENDIF
        IF (conduct%simP) THEN
          CALL check_param(ret,gt_zero,n_dissolfp,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_dissolhp,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_minerfp,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_degradhp,t_nutrient_soil)
          CALL check_param(ret,gt_minus_one_and_lt_plus_one,n_phoscorr,t_nutrient_soil)
        ENDIF
        CALL print_testresult_logical('pool transformations',ret)
        IF (.NOT. ret) THEN
            status = status + 1
        ENDIF

        !> @test Check and test parameters for nutrient percolation
        ret = .TRUE.
        IF (conduct%simN) CALL check_param(ret,ge_zero_and_le_one,n_onpercred,t_nutrient_soil)
        IF (conduct%simP) CALL check_param(ret,ge_zero_and_le_one,n_pppercred,t_nutrient_soil)
        CALL print_testresult_logical('percolation',ret)
        IF (.NOT. ret) THEN
            status = status + 1
        ENDIF

        !> @test Check and test parameters for nutrient phosphorus balance.
        ret = .TRUE.
        IF (conduct%simP) THEN
          CALL check_param(ret,gt_zero,n_freuc,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_freuexp,t_nutrient_soil)
          CALL check_param(ret,gt_zero,n_freurate,t_nutrient_soil)
          CALL print_testresult_logical('phosphorus balance',ret)
          IF (.NOT. ret) THEN
              status = status + 1
          ENDIF
        ENDIF

      END IF
    ELSE
      ret = .TRUE.
      CALL print_testresult_logical('no nutrient soil processes simulated',ret,suffix='')
    ENDIF

    CALL print_test_process(t_nutrient_soil)

  END SUBROUTINE check_nutrient_soil

  !> Check vegetation and soil surface processes
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_soil_leakage_as_input(status)

    USE MODVAR, ONLY : modeloption,p_soilleakage, &
                       numsubstances,i_t2,soilleak 

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret,ret2
    INTEGER i,j

    IF(modeloption(p_soilleakage)>0)THEN
      IF (numsubstances>0) THEN

        !> @test Check that soil leakage input is greater or equal to zero (not for T2 though)
        ret = .TRUE.
        DO i = 1, numsubstances
          IF(i==i_t2) CYCLE
          DO j = 1, 12  !month
            ret2 = .TRUE.
            CALL check_input(ret2,soilleak%concentration(i,j,:),ge_zero,a_soilleakage,t_soilleakage,force = .TRUE.)
            ret = ret.AND.ret2
          ENDDO
        ENDDO
        CALL add_generic_result(ret,0,a_soilleakage,t_soilleakage,'leakage conc')
        CALL print_testresult_logical('soilleakage',ret)
        IF(.NOT. ret) status = status + 1
      ENDIF
      CALL print_test_process(t_soilleakage)

    ELSE
      ret = .TRUE.
      CALL print_testresult_logical('no nutrient leakage from outer source simulated',ret,suffix='')
    ENDIF
    
  END SUBROUTINE check_soil_leakage_as_input

  !> Check and test soil water options
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_soil_water(status)

    USE MODVAR, ONLY : modeloption,p_deepgroundwater,p_frozensoil,p_surfacerunoff,p_infiltration, &
                       deepgroundwatermodelnames,frozensoilmodelnames,surfacerunoffmodelnames,infiltrationmodelnames
    USE HYPEVARIABLES, ONLY : n_deepmem,n_surfmem,n_depthrel,n_sndens0

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    !!> @test Check and test groundwater model options: specifics elsewhere
    !ret = valid_deepgroundwater_options(modeloption(p_deepgroundwater))
    !CALL print_testresult_logical(deepgroundwatermodelnames(modeloption(p_deepgroundwater)),ret,prefix='deepgroundwatermodel: ')
    !IF (.NOT. ret) THEN
    !  status = status + 1
    !END IF
    !> @test Check and test infiltration model options: specifics elsewhere
    ret = valid_infiltration_options(modeloption(p_infiltration))
    CALL print_testresult_logical(infiltrationmodelnames(modeloption(p_infiltration)),ret,prefix='infiltrationmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF
    !> @test Check and test surfacerunoff model options: specifics elsewhere
    ret = valid_surfacerunoff_options(modeloption(p_surfacerunoff))
    CALL print_testresult_logical(surfacerunoffmodelnames(modeloption(p_surfacerunoff)),ret,prefix='surfacerunoffmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF
    !> @test Check and test frozen soil model options: specifics elsewhere
    ret = valid_frozensoil_options(modeloption(p_frozensoil))
    CALL print_testresult_logical(frozensoilmodelnames(modeloption(p_frozensoil)),ret,prefix='frozensoilmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    !> @test Check parameters for soil temperature (deepmem, surfmem larger 
    !> than one timestep/day), and positive depth relationship (depthrel).
    ret = .TRUE.
    CALL check_param(ret,ge_one,n_deepmem,t_soil_water)
    CALL check_param(ret,ge_one,n_surfmem,t_soil_water)
    CALL check_param(ret,ge_zero,n_depthrel,t_soil_water)
    CALL check_param(ret,gt_zero,n_sndens0,t_soil_water)
    CALL print_testresult_logical('soil temperature model',ret)
    IF (.NOT. ret) status = status + 1
    !> @test Check that soil temperature is calculated (parameters set) for WQ-simulations.

    CALL print_test_process(t_soil_water)

  END SUBROUTINE check_soil_water

  !> Check and test specified frozensoil model for valid options
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_frozensoil_options(current_frozensoilmodel)

    USE HYPEVARIABLES, ONLY : n_logsatmp, n_bcosby,n_fzsexpand, &
                              n_deepmem,n_surfmem,n_depthrel

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_frozensoilmodel  !<frozensoil model to be tested for valid options

    valid_frozensoil_options = .TRUE.

    SELECT CASE(current_frozensoilmodel)
    CASE(0)
      !> @test Check frozensoil model 0: No ice, but ice expansion parameter still used/checked.
      CALL check_param(valid_frozensoil_options,ge_zero_and_le_one,n_fzsexpand,t_soil_water)
    CASE(1,2)
      !> @test Check frozensoil model 1 and 2: positive parameters for frozen soil, 
      !> ice expansion within (0,1) and soil temperature parameters set.
      CALL check_param(valid_frozensoil_options,gt_zero,n_logsatmp,t_soil_water)
      CALL check_param(valid_frozensoil_options,gt_zero,n_bcosby,t_soil_water)
      CALL check_param(valid_frozensoil_options,ge_zero_and_le_one,n_fzsexpand,t_soil_water)
      !For soil temperature
      CALL check_param(valid_frozensoil_options,ge_one,n_deepmem,t_soil_water)
      CALL check_param(valid_frozensoil_options,ge_one,n_surfmem,t_soil_water)
      CALL check_param(valid_frozensoil_options,ge_zero,n_depthrel,t_soil_water)
    END SELECT

  END FUNCTION valid_frozensoil_options

  !> Check and test specified infiltration model for valid options
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_infiltration_options(current_infiltrationmodel)

    USE HYPEVARIABLES, ONLY : n_bfroznsoil,n_ponatm,n_macrate,n_srrate
    USE MODVAR, ONLY : conduct,modparid,nsoil,soilpar

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_infiltrationmodel  !<infiltrationmodel to be tested for valid options

    !Local variables
    LOGICAL r
    INTEGER i,s
    REAL sum_val

    valid_infiltration_options = .TRUE.
 
    SELECT CASE(current_infiltrationmodel)
    CASE(0,2)
      !No specific test
    CASE(1,3)
      !> @test Check infiltration model 1: 
      !> frozen soil parameters have valid values (>=0).
      CALL check_param(valid_infiltration_options,ge_zero,n_bfroznsoil,t_soil_water)
      !> @test Check sum of macropore flow and saturated surface runoff 
      !> fractions larger than zero
      r = .TRUE.
      s = 0
      DO i = 1, nsoil
        sum_val = soilpar(modparid(n_macrate)%parno,i) + soilpar(modparid(n_srrate)%parno,i)
        IF (sum_val == 0.) THEN
          r = .FALSE.
          s = s + 1
        END IF
      END DO
      CALL add_generic_result(r,s,a_sum_macratesrrate2,t_soil_water,'macrate srrate sum pos')
      
    END SELECT

    !> @test Check organic part of nitrogen atmospheric deposition is between zero and one.
    IF (conduct%simN) THEN
      CALL check_param(valid_infiltration_options,ge_zero_and_le_one,n_ponatm,t_soil_water)
    END IF

  END FUNCTION valid_infiltration_options

  !> Check and test specified surface runoff model for valid options
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_surfacerunoff_options(current_surfacerunoffmodel)

    USE HYPEVARIABLES, ONLY : n_mactrsm,n_mactrinf,n_srrate,n_macrate, &
                              n_srbeta,n_sralpha,n_srgamma,n_srnlayer,n_macfrac
    USE MODVAR, ONLY : soilpar,modparid,nsoil

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_surfacerunoffmodel  !<surfacerunoffmodel to be tested for valid options

    !Local variables
    LOGICAL r
    INTEGER i,s
    REAL sum_val

    valid_surfacerunoff_options = .TRUE.

    SELECT CASE(current_surfacerunoffmodel)
    CASE(0)
      !> @test Check surface runoff and macropore flow model parameters have valid values
      !mactrsm   >= 0.0          --> OK
      !mactrinf  >= 0.0          --> OK
      !srrate    >= 0.0, <= 1.0  --> OK
      !macrate   >= 0.0, <= 1.0  --> OK
      CALL check_param(valid_surfacerunoff_options,ge_zero,n_mactrsm,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_zero,n_mactrinf,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_zero_and_le_one,n_srrate,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_zero_and_le_one,n_macrate,t_soil_water)
      !> @test Check sum of macropore flow and saturated surface runoff 
      !> fractions less than or equal to one.
      r = .TRUE.
      s = 0
      DO i = 1, nsoil
        sum_val = soilpar(modparid(n_macrate)%parno,i) + soilpar(modparid(n_srrate)%parno,i)
        IF (sum_val < 0 .OR. sum_val > 1) THEN
          r = .FALSE.
          s = s + 1
        END IF
      END DO
      CALL add_generic_result(r,s,a_sum_macratesrrate,t_soil_water,'macrate srrate sum')
    CASE(1)
      !> @test Check surface runoff model 1: parameters set
      CALL check_param(valid_surfacerunoff_options,gt_zero,n_srbeta,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_zero_and_le_one,n_macfrac,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_one_and_le_three,n_srnlayer,t_soil_water)
    CASE(2)
      !> @test Check surface runoff model 2: parameters set
      CALL check_param(valid_surfacerunoff_options,gt_zero,n_sralpha,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,gt_zero,n_srgamma,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_zero_and_le_one,n_macfrac,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_one_and_le_three,n_srnlayer,t_soil_water)
    CASE(3)
      !> @test Check surface runoff model 3: parameters set
      CALL check_param(valid_surfacerunoff_options,gt_zero,n_srbeta,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_zero_and_le_one,n_macfrac,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_one_and_le_three,n_srnlayer,t_soil_water)
    CASE(4)
      !> @test Check surface runoff model 4: parameters set
      CALL check_param(valid_surfacerunoff_options,gt_zero,n_sralpha,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,gt_zero,n_srgamma,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_zero_and_le_one,n_macfrac,t_soil_water)
      CALL check_param(valid_surfacerunoff_options,ge_one_and_le_three,n_srnlayer,t_soil_water)
    END SELECT

  END FUNCTION valid_surfacerunoff_options

  !>Check and test river wetlands for nutrient retention
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_river_wetlands(status)

    USE MODVAR, ONLY : modeloption,p_wetland, &
                       wetlandmodelnames, &
                       conduct,wetland,classbasin, &
                       slc_iwet,slc_owet,&
                       nsub_basemodel

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    INTEGER i,j1,j2,wetmask(nsub_basemodel,2)
    LOGICAL ret

    IF(modeloption(p_wetland)==0)THEN
      ret = .TRUE.
    ELSEIF(modeloption(p_wetland)==1)THEN
      IF(conduct%riverwetland)THEN
        !> @test Check river wetlands for correct input (area, depth and inflow part)
        ret = .TRUE.
        CALL check_input(ret,wetland(:,1)%area,ge_zero,a_lrwet_area,t_wetland)
        CALL check_input(ret,wetland(:,2)%area,ge_zero,a_mrwet_area,t_wetland)
        !Find mask for area>0
        j1=0;j2=0
        DO i = 1,nsub_basemodel
          IF(wetland(i,1)%area>0.)THEN
            j1=j1+1
            wetmask(j1,1) = i
          ENDIF
          IF(wetland(i,2)%area>0.)THEN
            j2=j2+1
            wetmask(j2,2) = i
          ENDIF
        ENDDO
        IF(j1>0)CALL check_input(ret,wetland(wetmask(1:j1,1),1)%depth,ge_zero,a_lrwet_depth,t_wetland)
        IF(j2>0)CALL check_input(ret,wetland(wetmask(1:j2,2),2)%depth,ge_zero,a_mrwet_depth,t_wetland)
        IF(j1>0)CALL check_input(ret,wetland(wetmask(1:j1,1),1)%part,gt_zero_and_le_one,a_lrwet_part,t_wetland)
        IF(j2>0)CALL check_input(ret,wetland(wetmask(1:j2,2),2)%part,gt_zero_and_le_one,a_mrwet_part,t_wetland)
        
        !!> @test Additional test for reasonable data, i.e. part>0 if area>0
        !ret2 = .TRUE.;ret3=.TRUE.
        !DO i = 1,nsub_basemodel
        !  IF(wetland(i,1)%area>0.) ret2 = ret2 .AND. wetland(i,1)%part>0.
        !  IF(wetland(i,2)%area>0.) ret3 = ret3 .AND. wetland(i,2)%part>0.
        !ENDDO
        !CALL add_generic_result(ret2,0,a_lrwet_inflow,t_wetland,'inflow to lr wetland')
        !CALL add_generic_result(ret3,0,a_mrwet_inflow,t_wetland,'inflow to mr wetland')
        !ret = ret.AND.ret2.AND.ret3
        IF (.NOT. ret) status = status + 1
      ENDIF
    ELSEIF(modeloption(p_wetland)==2)THEN
      !> @test Check class wetlands for correct input (area only so far)
      ret = .TRUE.
      IF(slc_iwet>0) CALL check_input(ret,classbasin(:,slc_iwet)%part,ge_zero_and_le_one,a_iwet_area,t_wetland)
      IF(slc_owet>0) CALL check_input(ret,classbasin(:,slc_owet)%part,ge_zero_and_le_one,a_owet_area,t_wetland)
      IF (.NOT. ret) status = status + 1
    ENDIF
    CALL print_testresult_logical(wetlandmodelnames(modeloption(p_wetland)),ret,prefix='wetlandmodel: ')
    CALL print_test_process(t_wetland)

  END SUBROUTINE check_river_wetlands

  !> Check point source data
  !> @brief Checking PointSourceData and time series of point sources
  !------------------------------------------------------------
  SUBROUTINE check_pointsourcedata(status)

    USE WORLDVAR, ONLY : modeldir, &
                         maxcharpath, &
                         readpstime, &
                         dailypsfile,monthlypsfile,yearlypsfile
    USE MODVAR, ONLY : psinfo, &
                       absinfo, &
                       npsused,nabsused

    !Argument declaration
    INTEGER, INTENT(INOUT) :: status  !<error number

    !Local variables
    CHARACTER(LEN=maxcharpath) filepath
    INTEGER k,nmiss
    LOGICAL ret,fileexist

    !Check PointSourceData:
    ret = .TRUE.
    filepath = TRIM(modeldir)//'PointSourceData.txt'
    INQUIRE(FILE=filepath,EXIST=fileexist)
    IF(fileexist) THEN
      !Extra subid not in GeoData is not an error. They are skipped while reading the file. But they could be tested here anyway.
    ENDIF
    
    !> @test Check PSTimeSeries:
    !> checking file and subid present
    IF(readpstime)THEN
      IF(.NOT.fileexist)THEN
        CALL add_generic_result(.FALSE.,0,a_consistent_pstimeinfo,t_pointsource,'existing PointSourceData file')
        ret = .FALSE.
      ELSE
        CALL add_generic_result(.TRUE.,0,a_consistent_pstimeinfo,t_pointsource,'existing PointSourceData file')
        IF(dailypsfile)THEN
          filepath = TRIM(modeldir)//'PSDailySeries.txt'
        ELSEIF(monthlypsfile)THEN
          filepath = TRIM(modeldir)//'PSMonthlySeries.txt'
        ELSEIF(yearlypsfile)THEN
          filepath = TRIM(modeldir)//'PSYearlySeries.txt'
        ENDIF
        INQUIRE(FILE=filepath,EXIST=fileexist)
        IF(.NOT.fileexist)THEN
          CALL add_generic_result(.FALSE.,0,a_exist_pstimefile,t_pointsource,'existing PSTimeSeries file')
          ret = .FALSE.
        ELSE
          CALL add_generic_result(.TRUE.,0,a_exist_pstimefile,t_pointsource,'existing PSTimeSeries file')
          !Check that subid in PointSourceData present in PSTimeSerie
          nmiss = 0
          DO k=1,npsused
            IF(psinfo(k)%flowcol==0) nmiss = nmiss + 1
          ENDDO
          DO k=1,nabsused
            IF(absinfo(k)%flowcol==0) nmiss = nmiss + 1
          ENDDO
          IF(nmiss .NE. 0) THEN
            CALL add_generic_result(.FALSE.,0,a_missing_pstime_forcing,t_pointsource,'PS timeseries missing')
            ret = .FALSE.
          ELSE
            CALL add_generic_result(.TRUE.,0,a_missing_pstime_forcing,t_pointsource,'PS timeseries missing')
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    CALL print_testresult_logical(TRIM(filepath),ret,suffix='')
    CALL print_test_process(t_pointsource)


    RETURN

  END SUBROUTINE check_pointsourcedata

  !> Check genearal deep process model option
  !> @brief Checking model option settings
  !------------------------------------------------------------
  SUBROUTINE check_general_deep(status)

    USE MODVAR, ONLY : deepgroundwatermodelnames,modeloption,p_deepgroundwater

    !Argument declaration
    INTEGER, INTENT(INOUT) :: status  !<number of errors

    !Local variables
    LOGICAL ret

    !> @test Check and test groundwater model options: specifics elsewhere
    ret = valid_deepgroundwater_options(modeloption(p_deepgroundwater))
    CALL print_testresult_logical(deepgroundwatermodelnames(modeloption(p_deepgroundwater)),ret,prefix='deepgroundwatermodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    RETURN

  END SUBROUTINE check_general_deep

  !> Check regional groundwater flow model
  !> @brief Printing results of model option test
  !------------------------------------------------------------
  SUBROUTINE check_reggrw(status)

    !Argument declaration
    INTEGER, INTENT(INOUT) :: status  !<number of errors


    !> @test Check and test groundwater model options: specifics elsewhere

    !CALL print_testresult_logical(TRIM(filepath),ret,suffix='')
    CALL print_test_process(t_reg_grw)

    RETURN

  END SUBROUTINE check_reggrw

  !> Check aquifers
  !> @brief Checking AquiferData and model option settings
  !------------------------------------------------------------
  SUBROUTINE check_aquifers(status)

    USE MODVAR, ONLY : aquifer,conduct,naquifers

    !Argument declaration
    INTEGER, INTENT(INOUT) :: status  !<number of errors

    !Local variables
    INTEGER ia
    LOGICAL ret,ret2,ret3

    !> @test Check aquifer depth/volume parameters consistent
    IF(ALLOCATED(aquifer)) THEN
      ret = .TRUE.
      ret2 = .TRUE.
      ret3 = .TRUE.
      DO ia=1,naquifers
        IF(aquifer(ia)%passivedep/=0. .AND. aquifer(ia)%passivedep>aquifer(ia)%basedepth) ret=.FALSE.
        IF(aquifer(ia)%inivol>aquifer(ia)%maxvol .OR. &
           aquifer(ia)%maxvol>-aquifer(ia)%area*aquifer(ia)%porosity*aquifer(ia)%reference) ret2=.FALSE.
      ENDDO
      CALL add_generic_result(ret,0,a_aqpassivedepth,t_aquifers,'passive depth consistent')
      IF(conduct%simN) CALL add_generic_result(ret2,0,a_aqtopdepth,t_aquifers,'top depth consistent')
      CALL check_input(ret3,aquifer(:)%passivevol,ge_zero,a_aqpassivevol,t_aquifers)
      ret = ret.AND.ret2.AND.ret3
      IF(.NOT. ret) status = status + 1
      CALL print_testresult_logical('aquifer volumes',ret,suffix='')
    ENDIF

    
    !CALL print_testresult_logical(TRIM(filepath),ret,suffix='')
    CALL print_test_process(t_aquifers)

    RETURN

  END SUBROUTINE check_aquifers

  !>Check and test specified deepgroundwater model for valid options; 
  !>both regional groundwater routine and aquifers
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_deepgroundwater_options(current_deepgroundwatermodel)

    USE HYPEVARIABLES, ONLY : n_rcgrw,n_aqpercorr,n_aqdelcorr,n_aqretcorr, &
                              n_rcgrwst,n_denitaq,n_hsatINsoil
    USE MODVAR, ONLY : aquifer,path,conduct

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_deepgroundwatermodel  !<deepgroundwatermodel to be tested for valid options

    !Local variables

    valid_deepgroundwater_options = .TRUE.
    SELECT CASE(current_deepgroundwatermodel)
    CASE(1)
      !> @test Check deep groundwater percolation parameters (rcgrw,rcgrwst,aqpercorr) and path (grwtolake).
      CALL check_param(valid_deepgroundwater_options,ge_zero,n_rcgrw,t_reg_grw)
      CALL check_param(valid_deepgroundwater_options,ge_zero,n_rcgrwst,t_reg_grw)
      CALL check_param(valid_deepgroundwater_options,ge_minus_one,n_aqpercorr,t_reg_grw)
      CALL check_input(valid_deepgroundwater_options,path(:)%grwtolake,ge_zero_and_le_one,a_grwtolake,t_reg_grw)
    CASE(2)
      !> @test Check deep groundwater percolation parameters (rcgrw,rcgrwst,aqpercorr) and path (grwtolake).
      CALL check_param(valid_deepgroundwater_options,ge_zero,n_rcgrw,t_aquifers)
      CALL check_param(valid_deepgroundwater_options,ge_zero,n_rcgrwst,t_aquifers)
      CALL check_param(valid_deepgroundwater_options,ge_minus_one,n_aqpercorr,t_aquifers)
      CALL check_input(valid_deepgroundwater_options,path(:)%grwtolake,ge_zero_and_le_one,a_grwtolake,t_aquifers)
      !> @test Check aquifer i.e. deepgroundwater model 2: AquiferData file 
      !> existance, percolation delay and return factor adjustment parameters 
      !>set (aqdelcorr,aqretcorr >=-1), for nitrogen simulation a valid denitrification parameters (denitaq,hsatINsoil>=0).
      CALL check_param(valid_deepgroundwater_options,gt_minus_one,n_aqdelcorr,t_aquifers)
      CALL check_param(valid_deepgroundwater_options,ge_minus_one,n_aqretcorr,t_aquifers)
      CALL check_file_existence(valid_deepgroundwater_options,'AquiferData.txt',a_AquiferData,t_aquifers)
      IF (conduct%simN) THEN
        CALL check_param(valid_deepgroundwater_options,ge_zero,n_denitaq,t_aquifers)
        CALL check_param(valid_deepgroundwater_options,ge_zero,n_hsatINsoil,t_aquifers)
      END IF
    CASE DEFAULT
      RETURN
    END SELECT

    !!> @test Check deep groundwater percolation parameters (rcgrw,rcgrwst,aqpercorr) and path (grwtolake).
    !!rcgrw          >= 0.0               --> OK
    !!rcgrwst        >= 0.0               --> OK
    !!aqpercorr      >= -1.0              --> OK
    !!grwtolake      >= 0.0, <= 1.0       --> OK
    !CALL check_param(valid_deepgroundwater_options,ge_zero,n_rcgrw,t_aquifers)
    !CALL check_param(valid_deepgroundwater_options,ge_zero,n_rcgrwst,t_aquifers)
    !CALL check_param(valid_deepgroundwater_options,ge_minus_one,n_aqpercorr,t_aquifer)
    !CALL check_input(valid_deepgroundwater_options,path(:)%grwtolake,ge_zero_and_le_one,a_grwtolake,t_aquifers)

    !> @test Check aquifer for valid data
    IF(ALLOCATED(aquifer)) THEN
      CALL check_input(valid_deepgroundwater_options,aquifer(:)%percdelay,ge_zero,a_percdelay,t_aquifers)
      CALL check_input(valid_deepgroundwater_options,aquifer(:)%area,gt_zero,a_aqarea,t_aquifers)
      CALL check_input(valid_deepgroundwater_options,aquifer(:)%porosity,gt_zero_and_lt_one,a_porosity,t_aquifers)
      CALL check_input(valid_deepgroundwater_options,aquifer(:)%retrate,ge_zero_and_le_one,a_retrate,t_aquifers)
      CALL check_input(valid_deepgroundwater_options,aquifer(:)%inivol,ge_zero,a_inivol,t_aquifers)
      CALL check_input(valid_deepgroundwater_options,aquifer(:)%basedepth,lt_zero,a_basedepth,t_aquifers)
      CALL check_input(valid_deepgroundwater_options,REAL(aquifer(:)%parregion(2)),gt_zero,a_parregion2,t_aquifers)
      IF(ALLOCATED(path)) THEN
        CALL check_input(valid_deepgroundwater_options,path(:)%recievefraction,ge_zero_and_le_one,a_recievefrac,t_aquifers)
      ENDIF
    ENDIF

  END FUNCTION valid_deepgroundwater_options

  !> Check and test floodplains options
  !> @brief
  !--------------------------------------------------------------------
  SUBROUTINE check_floodplains(status)

    USE MODVAR, ONLY : modeloption,p_floodplain, &
                       floodplainmodelnames, &
                       numsubstances,i_t1
    USE HYPEVARIABLES, ONLY : n_T1evap

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    !> @test Check and test floodplain options: specifics elsewhere
    ret = valid_floodplain_options(modeloption(p_floodplain))

    IF(modeloption(p_floodplain)>0)THEN
      !> @test Check floodplain model: FloodData file exist and tracer evaporation behaviour valid (t1evap>=0,<=1) 
      CALL check_file_existence(ret,'FloodData.txt',a_FloodData,t_floodplains)
      IF (i_t1 > 0 .AND. numsubstances > 0) THEN
        CALL check_param(ret,ge_zero_and_le_one,n_T1evap,t_floodplains)
      END IF
    ENDIF

    CALL print_testresult_logical(floodplainmodelnames(modeloption(p_floodplain)),ret,prefix='floodplainmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF
    CALL print_test_process(t_floodplains)

  END SUBROUTINE check_floodplains

  !> Check and test specified floodplain model for valid options
  !> @brief
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_floodplain_options(current_floodplainmodel)

    USE HYPEVARIABLES, ONLY : basinrrcscorr,n_mactrinf,n_ttmp,n_cmlt,n_srrcs

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_floodplainmodel  !<floodplainmodel to be tested for valid options

    !Local variables

    valid_floodplain_options = .TRUE.
    SELECT CASE(current_floodplainmodel)
      CASE(1)
      CASE(2)
        !> @test Check floodplain model option 2: parameters for snowmelt and 
        !>saturated surface runoff has valid values as well as recession 
        !>coefficient correction parameters (cmlt,mactrinf,srrcs,rrcscorr).
        CALL check_param(valid_floodplain_options,ge_zero,n_mactrinf,t_floodplains)
        CALL check_param(valid_floodplain_options,gt_zero,n_cmlt,t_floodplains)
        CALL check_param(valid_floodplain_options,ge_zero,n_srrcs,t_floodplains)
        !TODO: is it correct to test basinrrcscorr?
        IF (ALLOCATED(basinrrcscorr)) THEN
          CALL check_input(valid_floodplain_options,basinrrcscorr(:),ge_zero,a_rrcscorr,t_floodplains)
        END IF
      CASE DEFAULT
        RETURN
    END SELECT

    !TODO: Should m_optonoff and m_opt1-8 be tested?

  END FUNCTION valid_floodplain_options

  !>Check and test river options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_rivers(status)

    USE MODVAR, ONLY : basin, &
                       modeloption,p_lakeriverice, &
                       lakerivericemodelnames
    USE HYPEVARIABLES, ONLY : n_hygeomc,n_hygeomf,n_hygeomm,n_hygeomg

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret

    !> @test Check lake river ice model options: specifics elsewhere
    ret = valid_lakeriverice_options(modeloption(p_lakeriverice))
    CALL print_testresult_logical(lakerivericemodelnames(modeloption(p_lakeriverice)),ret,prefix='lakerivericemodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    ENDIF

    !> @test Check parameters and input data for hydraulic geometry.
    ret = .TRUE.
    CALL check_param(ret,ge_zero,n_hygeomf,t_rivers)
    CALL check_param(ret,ge_zero,n_hygeomm,t_rivers)
    CALL check_param(ret,ge_zero,n_hygeomc,t_rivers)
    CALL check_param(ret,ge_zero,n_hygeomg,t_rivers)
    CALL print_testresult_logical('hydraulic geometry',ret)
    IF (.NOT. ret) status = status + 1
    ret = .TRUE.
    CALL check_input(ret,basin(:)%mrivc(2),ge_zero,a_mrivc_c,t_rivers)
    CALL check_input(ret,basin(:)%mrivc(3),ge_zero,a_mrivc_f,t_rivers)
    CALL check_input(ret,basin(:)%mrivc(4),ge_zero,a_mrivc_ci,t_rivers)
    CALL check_input(ret,basin(:)%mrivc(5),ge_zero,a_mrivc_fi,t_rivers)
    CALL print_testresult_logical('river rating curve',ret)
    IF (.NOT. ret) status = status + 1

    CALL print_test_process(t_rivers)

  END SUBROUTINE check_rivers

  !>Check and test specified lakeriverice model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_lakeriverice_options(current_lakerivericemodel)

    USE HYPEVARIABLES, ONLY : n_t2trlake,n_t2trriver, &
                              n_tcflake,n_scflake, &
                              n_tcfriver,n_scfriver,n_limt2exch, &
                              n_stbcorr1,n_stbcorr2,n_stbcorr3, &
                              n_ccflake,n_lcflake, &
                              n_ccfriver,n_lcfriver, &
                              n_licetf, n_ricetf, &
                              n_licekika,n_ricekika,n_licekexp,n_ricekexp, &
                              n_licetmelt,n_ricetmelt, &
                              n_ricecwi,n_riceqhmn,n_riceqhmx,n_liceqhw, &
                              n_ricebupo, m_ricebupo,m_ricethpo

    USE MODVAR, ONLY : slc_olake,slc_ilake,slc_lriver,slc_mriver, &
                       genpar

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_lakerivericemodel  !<lakerivericemodel to be tested for valid options

    !Local variables
    LOGICAL tmp_result(2)
    LOGICAL ret

    tmp_result = [.TRUE.,.TRUE.]

    valid_lakeriverice_options = .TRUE.
    SELECT CASE(current_lakerivericemodel)
    CASE(1) 
      !> @test Check for simple air-water temperature exchange model 
      !>(lakerivericemodel 1): exchange parameters (t2trlake,t2trriver) valid (>0).
      CALL check_param(valid_lakeriverice_options,gt_zero,n_t2trlake,t_rivers)
      CALL check_param(valid_lakeriverice_options,gt_zero,n_t2trriver,t_rivers)
    CASE(2) 
      !> @test Check for new heat transfer model (lakerivericemodel 2):
      !>heat transfer parameters correctly given (tcflake,scflake,tcfriver,scfriver >0 or 
      !>ccfriver,lcfriver,ccflake,lcflake>0, and limt2exch>=0) stability parameters correct (stbcorr1-stbcorr3>=0)
      !lake
      CALL check_param(tmp_result(1),gt_zero,n_tcflake,t_rivers)
      CALL check_param(tmp_result(2),gt_zero,n_scflake,t_rivers)
      IF((.NOT. tmp_result(1)) .AND. (.NOT. tmp_result(2))) THEN
        valid_lakeriverice_options = .FALSE.
      ELSEIF (.NOT. tmp_result(1)) THEN
        CALL uncheck(n_tcflake,t_rivers)
      ELSEIF (.NOT. tmp_result(2)) THEN
        CALL uncheck(n_scflake,t_rivers)
        CALL check_param(valid_lakeriverice_options,gt_zero,n_ccflake,t_rivers)
        CALL check_param(valid_lakeriverice_options,gt_zero,n_lcflake,t_rivers)
      ENDIF
      !river
      CALL check_param(tmp_result(1),gt_zero,n_tcfriver,t_rivers)
      CALL check_param(tmp_result(2),gt_zero,n_scfriver,t_rivers)
      IF ((.NOT. tmp_result(1)) .AND. (.NOT. tmp_result(2))) THEN
        valid_lakeriverice_options = .FALSE.
      ELSEIF (.NOT. tmp_result(1)) THEN
        CALL uncheck(n_tcfriver,t_rivers)
      ELSEIF (.NOT. tmp_result(2)) THEN
        CALL uncheck(n_scfriver,t_rivers)
        CALL check_param(valid_lakeriverice_options,gt_zero,n_ccfriver,t_rivers)
        CALL check_param(valid_lakeriverice_options,gt_zero,n_lcfriver,t_rivers)
      ENDIF
      CALL check_param(valid_lakeriverice_options,ge_zero,n_limt2exch,t_rivers)
      CALL check_param(valid_lakeriverice_options,ge_zero,n_stbcorr1,t_rivers)
      CALL check_param(valid_lakeriverice_options,ge_zero,n_stbcorr2,t_rivers)
      CALL check_param(valid_lakeriverice_options,ge_zero,n_stbcorr3,t_rivers)
    END SELECT

    IF (current_lakerivericemodel > 0) THEN
      !> @test Check lake and river ice parameters set (licekika,ricekika,licekexp,
      !>ricekexp,licetmelt,ricetmelt > 0) and that a water class exist.
      CALL check_param(valid_lakeriverice_options,gt_zero,n_licekika,t_rivers)
      CALL check_param(valid_lakeriverice_options,gt_zero,n_ricekika,t_rivers)
      CALL check_param(valid_lakeriverice_options,gt_zero,n_licekexp,t_rivers)
      CALL check_param(valid_lakeriverice_options,gt_zero,n_ricekexp,t_rivers)
      CALL check_param(valid_lakeriverice_options,gt_zero,n_licetmelt,t_rivers)
      CALL check_param(valid_lakeriverice_options,gt_zero,n_ricetmelt,t_rivers)
      ! one of the slc_ilake or slc_olake or slc_lriver or slc_mriver classes must be > 0
      ret = (slc_ilake > 0) .OR. (slc_olake > 0) .OR. (slc_lriver > 0) .OR. (slc_mriver > 0)
      CALL add_generic_result(ret,0,a_lakeriverice_class,t_rivers,'lakeriverice class exists')
      IF (.NOT. ret) valid_lakeriverice_options = .FALSE.
    END IF

    !> @test Check ice freezing temperature not negative (licetf,ricetf)
    CALL check_param(valid_lakeriverice_options,ge_zero,n_licetf,t_rivers)
    CALL check_param(valid_lakeriverice_options,ge_zero,n_ricetf,t_rivers)

    !> @test Check ice breakup porosity parameters set positive and in range
    ret = .TRUE.
    CALL check_param(ret,gt_zero_and_le_one,n_ricebupo,t_rivers)
    IF(ret)THEN
      ret = genpar(m_ricebupo) > genpar(m_ricethpo) .AND. genpar(m_ricethpo) >= 0. .AND. genpar(m_ricethpo) <= 1.
      CALL add_generic_result(ret,0,a_lakeriverice_thpo,t_rivers,'ricethpo in range')
      IF (.NOT. ret) valid_lakeriverice_options = .FALSE.
    ELSE
      ret = .TRUE.
      CALL check_param(ret,ge_zero_and_le_one,n_ricebupo,t_rivers,.TRUE.)
      IF(ret)THEN
        !ricebupo=0, will be set to 1 in initialisation routine later
        ret = 1 > genpar(m_ricethpo) .AND. genpar(m_ricethpo) >= 0. .AND. genpar(m_ricethpo) <= 1.
        CALL add_generic_result(ret,0,a_lakeriverice_thpo,t_rivers,'ricethpo in range')
        IF (.NOT. ret) valid_lakeriverice_options = .FALSE.
      ELSE
        ret = genpar(m_ricebupo) > genpar(m_ricethpo) .AND. genpar(m_ricethpo) >= 0. .AND. genpar(m_ricethpo) <= 1.
        CALL add_generic_result(ret,0,a_lakeriverice_thpo,t_rivers,'ricethpo in range')
        valid_lakeriverice_options = .FALSE.  !for check_param
      ENDIF  
    ENDIF
    
    !> @test Check heat flux from water to ice (ricecwi,liceqhmn,riceqhmx,liceqhw>=0)
    CALL check_param(valid_lakeriverice_options,ge_zero,n_ricecwi,t_rivers)
    CALL check_param(valid_lakeriverice_options,ge_zero,n_liceqhw,t_rivers)
    CALL check_param(valid_lakeriverice_options,ge_zero,n_riceqhmn,t_rivers)
    CALL check_param(valid_lakeriverice_options,ge_zero,n_riceqhmx,t_rivers)

  END FUNCTION valid_lakeriverice_options

  !>Check and test snow routine options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_snow(status)
  
    USE HYPEVARIABLES, ONLY : n_whcsnow
    USE MODVAR, ONLY : modeloption,p_snowfall,p_snowmelt,p_snowevap,p_snowdensity, &
                       p_snowfalldist,p_snowheat,snowfallmodelnames,snowmeltmodelnames, &
                       snowevapmodelnames,snowdensitymodelnames,snowfalldistmodelnames, &
                       snowheatmodelnames

	!Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    
    !> @test Check snowfall model options; forcing data exist or threshold parameters set to calculate fraction of precipitation.
    ret = valid_snowfall_options(modeloption(p_snowfall))
    CALL print_testresult_logical(snowfallmodelnames(modeloption(p_snowfall)),ret,prefix='snowfallmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF
    
    !> @test Check snowfall distribution model options; forcing data exist or threshold parameters set to calculate fraction of precipitation.
    ret = valid_snowfalldistribution_options(modeloption(p_snowfalldist))
    CALL print_testresult_logical(snowfalldistmodelnames(modeloption(p_snowfalldist)),ret,prefix='snowfalldistmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF
    
    !> @test Check snowmelt model options; specifics elsewhere
    ret = valid_snowmelt_options(modeloption(p_snowmelt))
    CALL print_testresult_logical(snowmeltmodelnames(modeloption(p_snowmelt)),ret,prefix='snowmeltmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF
    
    !> @test Check snow evaporation model options; parameters set.
    ret = valid_snowevap_options(modeloption(p_snowevap))
    CALL print_testresult_logical(snowevapmodelnames(modeloption(p_snowevap)),ret,prefix='snowevapmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    !> @test Check snow density model options; specifics elsewhere
    ret = valid_snowdensity_options(modeloption(p_snowdensity))
    CALL print_testresult_logical(snowdensitymodelnames(modeloption(p_snowdensity)),ret,prefix='snowdensitymodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    !> @test Check snow heat model options; parameters set
    ret = valid_snowheat_options(modeloption(p_snowheat))
    CALL print_testresult_logical(snowheatmodelnames(modeloption(p_snowheat)),ret,prefix='snowheatmodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    !> @test Check snow model: snow liquid fraction parameters set
    ret = .TRUE.
    CALL check_param(ret,ge_zero_and_lt_one,n_whcsnow,t_snow_routines)
    CALL print_testresult_logical('snow liquid',ret,suffix='')
    IF (.NOT. ret) THEN
      status = status + 1
    ENDIF

    CALL print_test_process(t_snow_routines)

  END SUBROUTINE check_snow
  
  !>Check and test specified snow fall model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowfall_options(current_snowfallmodel)

    USE WORLDVAR, ONLY : i_sfobs
  
    USE HYPEVARIABLES, ONLY : n_ttpi

	!Argument declaration
    INTEGER, INTENT(IN)  :: current_snowfallmodel  !<snowfallmodel to be tested for valid options

    !Local variables
    
    valid_snowfall_options = .TRUE.
    SELECT CASE(current_snowfallmodel)
      CASE(0) !Original model based on threshold temperatures
        !ttpi      >= 0.0               --> OK
        CALL check_param(valid_snowfall_options,ge_zero,n_ttpi,t_snow_routines)
      CASE(1) !Fraction of precipitation as snowfall is given in input data
        !sfobs
        CALL check_forcing(valid_snowfall_options,i_sfobs,a_sfobs,t_snow_routines)
      CASE DEFAULT
        valid_snowfall_options = .FALSE.
    END SELECT
    
  END FUNCTION valid_snowfall_options
  
  !>Check and test specified snow fall model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowfalldistribution_options(current_snowfalldistmodel)

    USE WORLDVAR, ONLY : i_uwobs,i_vwobs
    USE MODVAR, ONLY : genpar
    USE HYPEVARIABLES, ONLY : n_wsfscale, n_sfdmax, & !n_wsfbias, n_wsfluse, &
                              n_sfdlim, m_numdir

	  !Argument declaration
    INTEGER, INTENT(IN)  :: current_snowfalldistmodel  !<snowfall distribution model to be tested for valid options

    !Local variables
    LOGICAL ret
    
    valid_snowfalldistribution_options = .TRUE.
    SELECT CASE(current_snowfalldistmodel)
      CASE(0) !No snowfall distribution
      CASE(1) !Linear WSF function
        !ws ?
        ret = (genpar(m_numdir) == 4) .OR. (genpar(m_numdir) == 8)
        CALL add_generic_result(ret,0,a_winddir,t_snow_routines,'no wind directions')
        valid_snowfalldistribution_options = valid_snowfalldistribution_options .AND. ret
        CALL check_param(valid_snowfalldistribution_options,gt_zero,n_wsfscale,t_snow_routines)
        CALL check_param(valid_snowfalldistribution_options,gt_zero,n_sfdmax,t_snow_routines)
        CALL check_param(valid_snowfalldistribution_options,ge_zero_and_le_one,n_sfdlim,t_snow_routines)
        CALL check_forcing(valid_snowfalldistribution_options,i_uwobs,a_uwobs,t_snow_routines)
        CALL check_forcing(valid_snowfalldistribution_options,i_vwobs,a_vwobs,t_snow_routines)
      CASE(2) !Log-linear WSF function
    END SELECT
    
  END FUNCTION valid_snowfalldistribution_options
  
  !>Check and test specified snow melt model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowmelt_options(current_snowmeltmodel)

    USE WORLDVAR, ONLY : i_swobs,i_tminobs,i_tmaxobs
  
    USE HYPEVARIABLES, ONLY : n_cmlt,n_fsceff, &
                              n_snalbmin,n_snalbmax, &
                              n_snalbkexp,n_cmrad,n_cmrefr, &
                              n_krs

    USE MODVAR, ONLY : basin

	!Argument declaration
    INTEGER, INTENT(IN)  :: current_snowmeltmodel  !<snowmeltmodel to be tested for valid options

    !Local variables
    LOGICAL tmp_logical;
    
    valid_snowmelt_options = .TRUE.
    !> @test Check snowmelt coefficient and efficiency of snowcover fraction set (cmlt>0,fsceff between zero and one)
    CALL check_param(valid_snowmelt_options,gt_zero,n_cmlt,t_snow_routines)
    CALL check_param(valid_snowmelt_options,ge_zero_and_le_one,n_fsceff,t_snow_routines)
    
    valid_snowmelt_options = .TRUE.
    SELECT CASE(current_snowmeltmodel)
      CASE(0,1) !Original temperature index model, calculated with or without snowcover scaling
      CASE(2) 
        !> @test Check  temperature and radiation index model, with/without snowcover 
        !> scaling and refreezing (snowmelt model option 2): parameters for snow melt 
        !>set (snalbmin,snalbmax,snalbkexp,cmrad,cmrefr), forcing from solar radiation
        !>or minimum and maximum temperature or elevation and parameter (krs) given.
        !snalbmin    >= 0.0, <= 1.0       --> OK
        !snalbmax    >= 0.0, <= 1.0       --> OK
        !snalbkexp   >  0.0               --> OK
        !cmrad       >  0.0               --> OK
        !cmrefr      >= 0.0               --> OK
        CALL check_param(valid_snowmelt_options,ge_zero_and_le_one,n_snalbmin,t_snow_routines)
        CALL check_param(valid_snowmelt_options,ge_zero_and_le_one,n_snalbmax,t_snow_routines)
        CALL check_param(valid_snowmelt_options,gt_zero,n_snalbkexp,t_snow_routines)
        CALL check_param(valid_snowmelt_options,gt_zero,n_cmrad,t_snow_routines)
        CALL check_param(valid_snowmelt_options,ge_zero,n_cmrefr,t_snow_routines)
        !swobs or (TMINobs and TMAXobs) or (krs and elevation)
        tmp_logical = .TRUE.
        CALL check_forcing(tmp_logical,i_swobs,a_swobs,t_snow_routines)
        IF (.NOT. tmp_logical) THEN
          tmp_logical = .TRUE.
          CALL uncheck(a_swobs,t_snow_routines)
          CALL check_forcing(tmp_logical,i_tminobs,a_tminobs,t_snow_routines)
          CALL check_forcing(tmp_logical,i_tmaxobs,a_tmaxobs,t_snow_routines)
          IF (.NOT. tmp_logical) THEN
            tmp_logical = .TRUE.
            CALL uncheck(a_tminobs,t_snow_routines)
            CALL uncheck(a_tmaxobs,t_snow_routines)
            CALL check_param(tmp_logical,ne_zero,n_krs,t_snow_routines)
            CALL check_input(tmp_logical,basin(:)%elev,ge_lowest_natural_point,a_elevation,t_snow_routines)
          END IF
        END IF
        valid_snowmelt_options = valid_snowmelt_options .AND. tmp_logical
      CASE DEFAULT
        valid_snowmelt_options = .FALSE.
    END SELECT
    
  END FUNCTION valid_snowmelt_options

  !>Check and test specified snow evaporation model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowevap_options(current_snowevapmodel)

    USE HYPEVARIABLES, ONLY : n_fepotsnow

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_snowevapmodel  !<snowevapmodel to be tested for valid options

    !Local variables

    valid_snowevap_options = .TRUE.
    SELECT CASE(current_snowevapmodel)
      CASE(0) !off
        !fepotsnow    ==  0.0               --> OK
        CALL check_param(valid_snowevap_options,eq_zero,n_fepotsnow,t_snow_routines)
      CASE(1) !epotsnow = epot * fepotsnow (landuse dependent)
        !fepotsnow    >   0.0               --> OK
        CALL check_param(valid_snowevap_options,gt_zero,n_fepotsnow,t_snow_routines)
      CASE DEFAULT
        valid_snowevap_options = .FALSE.
    END SELECT

  END FUNCTION valid_snowevap_options

  !>Check and test specified snow density model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowdensity_options(current_snowdensitymodel)

    USE MODVAR, ONLY : modeloption,p_lakeriverice
    USE HYPEVARIABLES, ONLY : n_sndens0,n_dsndens,n_ricesndens,n_licesndens, &
                              n_sdnsrate,n_sdnsradd,n_sdnsmax

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_snowdensitymodel  !<snowdensitymodel to be tested for valid options

    !Local variables

    valid_snowdensity_options = .TRUE.
    !> @test Check snow density of new snow parameter set (>0)
    CALL check_param(valid_snowdensity_options,gt_zero,n_sndens0,t_snow_routines)
    SELECT CASE(current_snowdensitymodel)
    CASE(0) 
      !> @test Check snow density model 0 (depending on age of snow): parameters set (snowdensdt,ricesndens,licesndens)
      CALL check_param(valid_snowdensity_options,ge_zero,n_dsndens,t_snow_routines)
      IF (modeloption(p_lakeriverice) == 1) THEN
        CALL check_param(valid_snowdensity_options,gt_zero,n_ricesndens,t_snow_routines)
        CALL check_param(valid_snowdensity_options,gt_zero,n_licesndens,t_snow_routines)
      END IF
    CASE(1)
      !> @test Check snow density model 1 (depending on compactation factor): parameters set (sdnsrate,sdnsradd,sdnsmax)
      CALL check_param(valid_snowdensity_options,gt_zero,n_sdnsrate,t_snow_routines)
      CALL check_param(valid_snowdensity_options,ge_zero,n_sdnsradd,t_snow_routines)
      CALL check_param(valid_snowdensity_options,gt_zero,n_sdnsmax,t_snow_routines)
    CASE DEFAULT
      valid_snowdensity_options = .FALSE.
    END SELECT

  END FUNCTION valid_snowdensity_options

  !>Check and test specified snow heat model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_snowheat_options(current_snowheatmodel)

    USE HYPEVARIABLES, ONLY : n_sndens0,n_snkika

    !Argument declaration
    INTEGER, INTENT(IN)  :: current_snowheatmodel  !<snowheatmodel to be tested for valid options

    !Local variables

    valid_snowheat_options = .TRUE.
    !> @test Check snow density of new snow parameter set (>0)
    SELECT CASE(current_snowheatmodel)
    CASE(0) 
      !No snow heat calculations
    CASE(1)
      !> @test Check snow heat model 1: parameters set (sndens0,snkika)
      CALL check_param(valid_snowheat_options,gt_zero,n_sndens0,t_snow_routines)
      CALL check_param(valid_snowheat_options,gt_zero,n_snkika,t_snow_routines)
    CASE DEFAULT
      valid_snowheat_options = .FALSE.
    END SELECT

  END FUNCTION valid_snowheat_options


  !>Check and test glacier options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_glaciers(status)

    USE MODVAR, ONLY : conduct,modeloption,p_glacierini,glacierinimodelnames, &
                       nclass,nsub,classmodel,classbasin,glacierindex
    USE HYPEVARIABLES, ONLY : n_glacdens,n_glacvcoef,n_glacvexp, &
                              n_glacvcoef1,n_glacvexp1,glacier_model

    !Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER j,i

    !> @test All glaciers in GeoData have index set
    ret = .TRUE.
    IF (conduct%glacier) THEN 
      DO j=1,nclass
        IF(classmodel(j)==glacier_model) EXIT
      ENDDO
      IF(j>0.AND.j<=nclass)THEN
        DO i = 1,nsub
          IF(classbasin(i,j)%part>0.)THEN
            IF(glacierindex(i)==0) ret = .FALSE.
          ENDIF
        ENDDO
      ENDIF
    ENDIF
    CALL print_testresult_logical('all GeoData glaciers in GlacierData',ret,suffix='')
    IF (.NOT. ret) THEN
      status = status + 1
    ENDIF

    !> @test Glacier parameters for volume-area relationship positive
    ret = .TRUE.
    IF (conduct%glacier) THEN
      CALL check_param(ret,gt_zero,n_glacdens,t_glaciers)
      CALL check_param(ret,gt_zero,n_glacvcoef,t_glaciers)
      CALL check_param(ret,gt_zero,n_glacvexp,t_glaciers)
      CALL check_param(ret,gt_zero,n_glacvcoef1,t_glaciers)
      CALL check_param(ret,gt_zero,n_glacvexp1,t_glaciers)
    ENDIF
    CALL print_testresult_logical('glacier volume-area',ret,suffix=' with parameters')
    IF (.NOT. ret) THEN
      status = status + 1
    ENDIF

    !> @test Glacier initialsation model option valid.
    ret = valid_glacierini_options(modeloption(p_glacierini))
    CALL print_testresult_logical(glacierinimodelnames(modeloption(p_glacierini)),ret,prefix='glacierinimodel: ')
    IF (.NOT. ret) THEN
      status = status + 1
    END IF

    CALL print_test_process(t_glaciers)

  END SUBROUTINE check_glaciers

  !>Check and test specified glacierini model for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_glacierini_options(current_glacierinimodel)


    !Argument declaration
    INTEGER, INTENT(IN)  :: current_glacierinimodel  !<glacierinimodel to be tested for valid options

    !Local variables

    valid_glacierini_options = .TRUE.

    SELECT CASE(current_glacierinimodel)
      CASE(0) !Glacier init as usual via SLC+parameters or stated save
        CONTINUE
      CASE(1) !init from SLC+parameters overrides state_save
        CONTINUE
      CASE DEFAULT
        valid_glacierini_options = .FALSE.
    END SELECT

  END FUNCTION valid_glacierini_options

  !>Check and test evaporation options
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_evaporation(status)
    
    USE MODVAR, ONLY : p_petmodel,petmodelnames,maxmodelsinoption

	!Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    LOGICAL ret
    INTEGER s, i, num_petmodels_used
    INTEGER petmodels_used(maxmodelsinoption(p_petmodel))
    
    !> Find all petmodels used in model set up
    num_petmodels_used = find_petmodels_used(petmodels_used, maxmodelsinoption(p_petmodel))
    CALL log_petmodels_used(funit_used,petmodels_used,num_petmodels_used)
    
    !> @test For each petmodel present in model check its needed input
    s = num_petmodels_used
    DO i = 1, num_petmodels_used
      ret = valid_petmodel_options(petmodels_used(i))
      IF (ret) THEN
        s = s - 1
      END IF
      CALL print_testresult_logical(petmodelnames(petmodels_used(i)),ret,prefix='petmodel: ')
    END DO
    
    !> @test Check actual soil evaporation input and parameters valid; specifics elsewhere
    ret = check_actual_soil_evapotranspiration()
    IF (.NOT. ret) THEN
      s = s + 1
    END IF
    CALL print_testresult_logical('actual_soil_evapotranspiration',ret)

    CALL print_test_process(t_evaporation)

    status = status + s

  END SUBROUTINE check_evaporation

  !>Check and test specified petmodel for valid options
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_petmodel_options(current_petmodel)

    USE WORLDVAR, ONLY : i_tobs,i_rhobs, &
                         i_swobs,i_uobs, &
                         i_tminobs,i_tmaxobs
    USE HYPEVARIABLES, ONLY : n_ttmp,n_cevp, &
                              n_kc,n_krs,n_jhtadd,n_jhtscale,n_alfapt, &
                              n_alb,n_roughness,n_zpdh,n_zwind,n_zwish,n_mwind
    USE MODVAR, ONLY : basin

	!Argument declaration
    INTEGER, INTENT(IN)  :: current_petmodel  !<petmodel to be tested for valid options

    !Local variables
    LOGICAL r,tmp_result(3),tmp_kc_result(2)
    
    tmp_result = [.TRUE.,.TRUE.,.TRUE.]
    tmp_kc_result = [.TRUE.,.TRUE.]
    valid_petmodel_options = .TRUE.
    SELECT CASE(current_petmodel)
      CASE(0) 
        !> @test Check petmodel 0 (HYPE original model (with Xobs replacement, if available)):
        !> Temperature forcing valid and evaporation rate parameter (cevp) positive.
        CALL check_param(valid_petmodel_options,gt_zero,n_cevp,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
      CASE(1)
        !> @test Check petmodel 1 (HYPE original model (without Xobs replacement)):
        !> Temperature forcing valid and evaporation rate parameter (cevp) positive.
        CALL check_param(valid_petmodel_options,gt_zero,n_cevp,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
      CASE(2)
        !> @test Check petmodel 2 (Modified Jensen-Haise/McGuinness following Oudin et al (2005)):
        !> Temperature forcing valid, latitude given and evaporation parameters (kc,jhtadd,jhtscale) valid
        CALL check_param(tmp_kc_result(1),gt_zero,n_kc(2),t_evaporation)
        CALL check_param(tmp_kc_result(2),gt_zero,n_kc(1),t_evaporation)
        IF(tmp_kc_result(1))THEN
          CALL uncheck(n_kc(1),t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSEIF(tmp_kc_result(2))THEN
          CALL uncheck(n_kc(2),t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          valid_petmodel_options = .FALSE.
        ENDIF
        CALL check_param(valid_petmodel_options,ne_zero,n_jhtadd,t_evaporation)
        CALL check_param(valid_petmodel_options,ne_zero,n_jhtscale,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%latitude,plus_minus_90,a_latitude,t_evaporation)
        r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
        CALL add_generic_result(r,0,a_latitude_defined,t_evaporation,'latitude defined')
        valid_petmodel_options = valid_petmodel_options .AND. r
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
      CASE(3)
        !> @test Check petmodel 3 (Hargreaves-Samani):
        !>Temperature forcing valid, elevation and latitude given and 
        !>evaporation parameters (kc,krs) valid. Additional forcing present.
        CALL check_param(tmp_kc_result(1),gt_zero,n_kc(3),t_evaporation)
        CALL check_param(tmp_kc_result(2),gt_zero,n_kc(1),t_evaporation)
        IF(tmp_kc_result(1))THEN
          CALL uncheck(n_kc(1),t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSEIF(tmp_kc_result(2))THEN
          CALL uncheck(n_kc(3),t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          valid_petmodel_options = .FALSE.
        ENDIF
        CALL check_param(valid_petmodel_options,ne_zero,n_krs,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%elev,ge_lowest_natural_point,a_elevation,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%latitude,plus_minus_90,a_latitude,t_evaporation)
        r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
        CALL add_generic_result(r,0,a_latitude_defined,t_evaporation,'latitude defined')
        valid_petmodel_options = valid_petmodel_options .AND. r
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
        !Depending on forcing data, different cases are ok
        !swobs OR (tminobs AND tmaxobs)
        CALL check_forcing(tmp_result(1),i_swobs,a_swobs,t_evaporation)
        CALL check_forcing(tmp_result(2),i_tminobs,a_tminobs,t_evaporation)
        CALL check_forcing(tmp_result(3),i_tmaxobs,a_tmaxobs,t_evaporation)
        IF (tmp_result(1)) THEN
          CALL uncheck(a_tminobs,t_evaporation)
          CALL uncheck(a_tmaxobs,t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE IF (tmp_result(2) .AND. tmp_result(3)) THEN
          CALL uncheck(a_swobs,t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          valid_petmodel_options = .FALSE.
        END IF
      CASE(4)
        !> @test Check petmodel 4 (Priestly Taylor):
        !>Temperature forcing valid, elevation and latitude given and 
        !>evaporation parameters (kc,alb,alfapt) valid. Additional forcing or parameters present.
        CALL check_param(tmp_kc_result(1),gt_zero,n_kc(4),t_evaporation)
        CALL check_param(tmp_kc_result(2),gt_zero,n_kc(1),t_evaporation)
        IF(tmp_kc_result(1))THEN
          CALL uncheck(n_kc(1),t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSEIF(tmp_kc_result(2))THEN
          CALL uncheck(n_kc(4),t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          valid_petmodel_options = .FALSE.
        ENDIF
        CALL check_param(valid_petmodel_options,gt_zero,n_alb,t_evaporation)
        CALL check_param(valid_petmodel_options,ne_zero,n_alfapt,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%elev,ge_lowest_natural_point,a_elevation,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%latitude,plus_minus_90,a_latitude,t_evaporation)
        r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
        CALL add_generic_result(r,0,a_latitude_defined,t_evaporation,'latitude defined')
        valid_petmodel_options = valid_petmodel_options .AND. r
        !tobs, tminobs, tmaxobs
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tminobs,a_tminobs,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tmaxobs,a_tmaxobs,t_evaporation)
        !Depending on forcing data, different cases are ok
        !(rhobs AND swobs) OR krs != 0
        CALL check_forcing(tmp_result(1),i_rhobs,a_rhobs,t_evaporation)
        CALL check_forcing(tmp_result(2),i_swobs,a_swobs,t_evaporation)
        IF (tmp_result(1) .AND. tmp_result(2)) THEN
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          CALL uncheck(a_rhobs,t_evaporation)
          CALL uncheck(a_swobs,t_evaporation)
          CALL check_param(valid_petmodel_options,ne_zero,n_krs,t_evaporation)
        END IF
      CASE(5)
        !> @test Check petmodel 5 (FAO Penman Monteith reference crop evapotranspiration):
        !>Temperature forcing valid, elevation and latitude given and 
        !>evaporation parameters (kc,alb) valid. Additional forcing or parameters present.
        CALL check_param(tmp_kc_result(1),gt_zero,n_kc(5),t_evaporation)
        CALL check_param(tmp_kc_result(2),gt_zero,n_kc(1),t_evaporation)
        IF(tmp_kc_result(1))THEN
          CALL uncheck(n_kc(1),t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSEIF(tmp_kc_result(2))THEN
          CALL uncheck(n_kc(5),t_evaporation)
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          valid_petmodel_options = .FALSE.
        ENDIF
        CALL check_param(valid_petmodel_options,gt_zero,n_alb,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%elev,ge_lowest_natural_point,a_elevation,t_evaporation)
        CALL check_input(valid_petmodel_options,basin(:)%latitude,plus_minus_90,a_latitude,t_evaporation)
        r = (MINVAL(basin(:)%latitude) /= 0.0) .OR. (MAXVAL(basin(:)%latitude) /= 0.0)
        CALL add_generic_result(r,0,a_latitude_defined,t_evaporation,'latitude defined')
        valid_petmodel_options = valid_petmodel_options .AND. r
        !tobs, tminobs, tmaxobs
        CALL check_forcing(valid_petmodel_options,i_tobs,a_tobs,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tminobs,a_tminobs,t_evaporation)
        CALL check_forcing(valid_petmodel_options,i_tmaxobs,a_tmaxobs,t_evaporation)
        !Depending on forcing data, different cases are ok
        !(rhobs AND swobs) OR krs  != 0
        CALL check_forcing(tmp_result(1),i_rhobs,a_rhobs,t_evaporation)
        CALL check_forcing(tmp_result(2),i_swobs,a_swobs,t_evaporation)
        IF (tmp_result(1) .AND. tmp_result(2)) THEN
          valid_petmodel_options = valid_petmodel_options .AND. .TRUE.
        ELSE
          CALL uncheck(a_rhobs,t_evaporation)
          CALL uncheck(a_swobs,t_evaporation)
          CALL check_param(valid_petmodel_options,ne_zero,n_krs,t_evaporation)
        END IF
        !(uobs AND (roughness > 0,zwind > 0,zwish > 0)) OR mwind > 0
        !zphd could be zero, therefore not tested
        CALL check_forcing(tmp_result(3),i_uobs,a_uobs,t_evaporation)
        IF (tmp_result(3)) THEN
          CALL check_param(valid_petmodel_options,gt_zero,n_roughness,t_evaporation)
          CALL check_param(valid_petmodel_options,gt_zero,n_zwind,t_evaporation)
          CALL check_param(valid_petmodel_options,gt_zero,n_zwish,t_evaporation)
        ELSE
          CALL uncheck(a_uobs,t_evaporation)
          CALL check_param(valid_petmodel_options,gt_zero,n_mwind,t_evaporation)
        END IF
      CASE DEFAULT
        valid_petmodel_options = .FALSE.
    END SELECT
    
  END FUNCTION valid_petmodel_options
  
  !>Check and test actual soil evapotranspiration
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION check_actual_soil_evapotranspiration()

    USE HYPEVARIABLES, ONLY : n_lp, n_ttrig,n_tredA,n_tredB,n_T1evap,n_epotdist

    !Argument declaration

    !Local variables
    LOGICAL tmp_logical

    tmp_logical = .TRUE.

    !> @test Check soil temperature reduction of evaporation, if used 
    !>(tredA>0.0) parameters checked if positive/valid
    CALL check_param(tmp_logical,gt_zero,n_tredA,t_evaporation)
    IF (tmp_logical) THEN
      CALL check_param(tmp_logical,ge_zero,n_ttrig,t_evaporation)
      CALL check_param(tmp_logical,ge_zero,n_tredB,t_evaporation)
    ELSE
      CALL uncheck(n_tredA,t_evaporation)
      tmp_logical = .TRUE.
    END IF

    !> @test Check parameters for actual soil evaporation (lp,t1evap,epotdist) valid.
    CALL check_param(tmp_logical,ge_zero_and_le_one,n_lp,t_evaporation)
    CALL check_param(tmp_logical,ge_zero_and_le_one,n_T1evap,t_evaporation)
    CALL check_param(tmp_logical,gt_zero,n_epotdist,t_evaporation)
    
    check_actual_soil_evapotranspiration = tmp_logical

  END FUNCTION check_actual_soil_evapotranspiration

  !>Check and test atmospheric deposition
  !--------------------------------------------------------------------
  SUBROUTINE check_atmospheric_deposition(status)
    
    USE MODVAR, ONLY : simulate,i_in,i_sp,i_pp,deposition
    USE HYPEVARIABLES, ONLY : n_wetsp,n_drypp,n_wetspl

  	!Argument declaration
    INTEGER, INTENT(INOUT)  :: status  !<Return status

    !Local variables
    INTEGER j
    LOGICAL ret,ret2

  !> @test Check atmospheric nitrogen deposition input not negative
    ret = .TRUE.
    IF(simulate%substance(i_in))THEN
      IF(ALLOCATED(deposition%inwetconc))THEN
        CALL check_input(ret,deposition%inwetconc(:),ge_zero,a_basinindep,t_atmdep)
      ENDIF
      IF(ALLOCATED(deposition%indryload))THEN
        DO j = 1,SIZE(deposition%indryload(:,:),2)
          ret2 = .TRUE.
          CALL check_input(ret2,deposition%indryload(:,j),ge_zero,a_basinindep2,t_atmdep,force = .TRUE.)
          ret = ret.AND.ret2
        ENDDO
        CALL add_generic_result(ret,0,a_basinindep2,t_atmdep,testing_names(a_basinindep2))
      ENDIF
      IF(ALLOCATED(deposition%inloadwater))THEN
        DO j = 1,12 !month
          ret2 = .TRUE.
          CALL check_input(ret2,deposition%inloadwater(:,j),ge_zero,a_basinindep3,t_atmdep,force = .TRUE.)
          ret = ret.AND.ret2
        ENDDO
        CALL add_generic_result(ret,0,a_basinindep3,t_atmdep,testing_names(a_basinindep3))
      ENDIF
    ENDIF

  !> @test Check atmospheric phosphorus deposition parameters not negative
    IF(simulate%substance(i_sp))THEN
      CALL check_param(ret,ge_zero,n_wetsp,t_atmdep)
      CALL check_param(ret,ge_zero,n_wetspl,t_atmdep)
    ENDIF
    IF(simulate%substance(i_pp))THEN
      CALL check_param(ret,ge_zero,n_drypp,t_atmdep)
    ENDIF
    !TODO: should test wet dep of IN, SP and T1 from Xobs here.

    CALL print_testresult_logical('atmospheric_deposition',ret)

    CALL print_test_process(t_atmdep)

    IF(.NOT. ret) THEN
      status = status + 1
    END IF
    !status = status + s

  END SUBROUTINE check_atmospheric_deposition

  !>Check and test dam purpose for valid options
  !>
  !--------------------------------------------------------------------
  LOGICAL FUNCTION valid_dam_purpose(current_dam)

    USE MODVAR, ONLY : dam

  	!Argument declaration
    INTEGER, INTENT(IN)  :: current_dam  !<petmodel to be tested for valid options

    !Local variables
    
    valid_dam_purpose = .TRUE.
    SELECT CASE(dam(current_dam)%purpose)
      CASE(1) !irrigation dam
      CASE(2) !water supply dam
      CASE(3) !flood control dam
        !add checks qinf, threshpar...
      CASE(4) !hydroelectric dam
        !add checks qamp, snowfrac,..
      CASE DEFAULT
        valid_dam_purpose = .FALSE.
    END SELECT
    
  END FUNCTION valid_dam_purpose
  
  !>Add process tests
  !>
  !--------------------------------------------------------------------
  SUBROUTINE add_tests(test_id, test_process, test_logical)

    !Argument declaration
    INTEGER, INTENT(IN) :: test_id       !<
    INTEGER, INTENT(IN) :: test_process  !<
    LOGICAL, INTENT(IN) :: test_logical  !<
    !Local variables
    INTEGER i

    DO i = 1, max_testcase_values
        IF ((tests_c2_process(i,test_process)%test_id .EQ. -1) .OR. &
            (tests_c2_process(i,test_process)%test_id .EQ. test_id)) THEN
            tests_c2_process(i,test_process)%test_id = test_id
            tests_c2_process(i,test_process)%test_passed = test_logical
            RETURN
        END IF
    END DO

    WRITE(0,*) 'ERROR: Size exceeded, could not add the test', test_id, ', test will not be counted for!'

  END SUBROUTINE add_tests

  !>Clear process tests
  !>
  !--------------------------------------------------------------------
  SUBROUTINE clear_tests(test_process)

    !Argument declaration
    INTEGER, OPTIONAL, INTENT(IN) :: test_process  !< Clear tests for one process if specified
    !Local variables
    INTEGER i,j

    IF (PRESENT(test_process)) THEN
        DO j = 1, max_testcase_values
            tests_c2_process(j,test_process)%test_id = -1
            tests_c2_process(j,test_process)%test_passed = .FALSE.
        END DO
        RETURN
    END IF

    DO i = 1, nof_test_processes
        DO j = 1, max_testcase_values
            tests_c2_process(j,i)%test_id = -1
            tests_c2_process(j,i)%test_passed = .FALSE.
        END DO
    END DO

    DO i = 1, nof_external_tests
      external_tests(i)%propagated = .FALSE.
      external_tests(i)%passed = .TRUE.
      external_tests(i)%index_ndata = 0
      DO j = 1, SIZE(external_tests(i)%ndata)
        external_tests(i)%ndata(j)%kind_of_msg = 0
        IF (ALLOCATED(external_tests(i)%ndata(j)%msg)) DEALLOCATE(external_tests(i)%ndata(j)%msg)
        IF (ALLOCATED(external_tests(i)%ndata(j)%data)) DEALLOCATE(external_tests(i)%ndata(j)%data)
      END DO
    END DO

  END SUBROUTINE clear_tests

  !>Check parameter
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_param(io_result,tolerance,test_id,test_process,force)

    USE MODVAR, ONLY : modparid,m_lpar,m_spar,m_gpar, m_rpar, &
                       genpar,landpar,soilpar,regpar

    !Argument declaration
    LOGICAL, INTENT(INOUT) :: io_result  !<Input and Output logical for "AND" result
    INTEGER, INTENT(IN) :: tolerance     !<The tolerance to test against
    INTEGER, INTENT(IN) :: test_id       !<HYPE model parameter index specifying the current test case
    INTEGER, INTENT(IN) :: test_process  !<Process assigned to this test case
    LOGICAL, OPTIONAL, INTENT(IN) :: force    !<force the test eventhough already tested

    !Local variables
    REAL, DIMENSION(:), POINTER :: ptr
    REAL, TARGET :: v(1)
    INTEGER status
    LOGICAL test_forced

    test_forced = force_test
    IF (PRESENT(force)) THEN
      test_forced = force
    END IF

    SELECT CASE(modparid(test_id)%deptype)
    CASE(m_rpar)
      ptr => regpar(modparid(test_id)%parno,:)
    CASE(m_lpar)
      ptr => landpar(modparid(test_id)%parno,:)
    CASE(m_spar)
      ptr => soilpar(modparid(test_id)%parno,:)
    CASE(m_gpar)
      v(1) = genpar(modparid(test_id)%parno)
      ptr => v
    CASE DEFAULT
      io_result = .FALSE.
      RETURN
    END SELECT

    IF ((.NOT. all_test_cases(test_id)%tested) .OR. (test_forced)) THEN
      all_test_cases(test_id)%passed = real_check_tolerance(ptr,tolerance,status)
      all_test_cases(test_id)%tested = .TRUE.
      all_test_cases(test_id)%kind_of_test = p_parameter
      all_test_cases(test_id)%tolerance = tolerance
      all_test_cases(test_id)%status = status
      all_test_cases(test_id)%modpar_index = test_id
      all_test_cases(test_id)%name = TRIM(modparid(test_id)%shortname)
    END IF

    !Construct the error string
    IF (.NOT. all_test_cases(test_id)%passed) THEN
      WRITE(all_test_cases(test_id)%errstring,*,ERR=400) trim(all_test_cases(test_id)%name),' tolerance ', &
          all_test_cases(test_id)%tolerance,' error!'
    END IF

    io_result = io_result .AND. all_test_cases(test_id)%passed
    CALL add_tests(test_id, test_process, all_test_cases(test_id)%passed)

    RETURN

    !Error handling
400 WRITE(0,*) 'Test case error (',TRIM(all_test_cases(test_id)%name),'), writing error string!'

  END SUBROUTINE check_param
  
  !>\brief Print name of a (group of) test cases and if it passed or failed.
  !>
  !>This output gives an overview of where failed test cases can be found. 
  !>More details on which test cases failed are given with higher level of printout.
  !--------------------------------------------------------------------
  SUBROUTINE print_testresult_logical(name_string,return_logical,prefix,suffix)
  
    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: name_string  !<The name of the (group of) test cases
    LOGICAL, INTENT(IN) :: return_logical        !<The logical result of the (group of) test cases
    CHARACTER(LEN=*), OPTIONAL :: prefix         !<Specify a prefix string to the name if wanted
    CHARACTER(LEN=*), OPTIONAL :: suffix         !<Specify a suffix string otherwise it becomes "with parameter, ..."
    
    !Local variables
    CHARACTER(LEN=128) :: text

    text = TRIM(name_string)
    IF (PRESENT(prefix)) THEN
      text = TRIM(prefix) // TRIM(name_string)
    END IF

    IF (PRESENT(suffix)) THEN
      text = TRIM(text) // TRIM(suffix)
    ELSE
      text = TRIM(text) // TRIM(' with parameter, input and forcing data')
    END IF
    
    IF (return_logical) THEN
      WRITE(funit_used,*) '[Passed] ', TRIM(text)
    ELSE
      WRITE(funit_used,*) '[Failed] ', TRIM(text)
    END IF
    
  END SUBROUTINE print_testresult_logical
  
  !>Finalises the test process by printing the number of failed "logical" test cases
  !within the test process. 
  !>Also prints the details of all the test cases included, their status and
  !>if extended printout also parameters and value intervals.
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_test_process(process)

    !Argument declaration
    INTEGER, INTENT(IN) :: process  !<The process

    !Local variables
    INTEGER t,i,failures
    CHARACTER(LEN=32) tolerance_name

    !Print out more information about the included test cases
    IF (printout_level > 0) THEN
      failures = 0
      DO i = 1, max_testcase_values
        IF (tests_c2_process(i,process)%test_id .GT. 0) THEN
          t = all_test_cases(tests_c2_process(i,process)%test_id)%tolerance
          IF (t > 0 .AND. t <= nof_test_tolerances) THEN
            tolerance_name = testing_tolerances(t)
          ELSE
            tolerance_name = '-'
          END IF
          WRITE(funit_used,*) tests_c2_process(i,process)%test_id, all_test_cases(tests_c2_process(i,process)%test_id)%name, &
          ' passed:',tests_c2_process(i,process)%test_passed,TRIM(tolerance_name)
          IF (.NOT. tests_c2_process(i,process)%test_passed) THEN
            failures = failures + 1;
          END IF
        END IF
      END DO

      !Extended the printouts of test cases further
      IF (printout_level > 1) THEN
        DO i = 1, max_testcase_values
          IF (tests_c2_process(i,process)%test_id .GT. 0) THEN
            CALL print_extended(tests_c2_process(i,process)%test_id)
          END IF
        END DO
      END IF

      !Continue to print out summary, number of failures
      IF (failures .GT. 0) THEN
        WRITE(funit_used,*) 'Tested with number of failures: ',failures
      END IF
    END IF

  END SUBROUTINE print_test_process

  !>Print out of extended information for a test case. 
  !>The print out is different for different kinds of tests; hype 
  !>parameter, input, forcing or generic.
  !--------------------------------------------------------------------
  SUBROUTINE print_extended(test_id)

    !Argument declaration
    INTEGER, INTENT(IN) :: test_id  !<The test case id

    !Local variables
    CHARACTER(LEN=580) :: name_interval

    SELECT CASE(all_test_cases(test_id)%kind_of_test)
    CASE(p_parameter)
      CALL print_param(all_test_cases(test_id)%modpar_index)
    CASE(p_input)
      WRITE(name_interval,*) TRIM(all_test_cases(test_id)%name),': ',all_test_cases(test_id)%minvalue,'-',all_test_cases(test_id)%maxvalue
      CALL print_input(name_interval)
    CASE(p_forcing)
      CALL print_forcing(all_test_cases(test_id)%name)
    CASE(p_generic)
      CALL print_generic(all_test_cases(test_id)%name)
    CASE DEFAULT
      RETURN
    END SELECT

  END SUBROUTINE print_extended
  
  !>Print parameter
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_param(modpar_index)
  
    USE MODVAR, ONLY : modparid,m_lpar,m_spar,m_gpar,m_rpar, &
                       genpar,landpar,soilpar,regpar
    
    !Argument declaration
    INTEGER, INTENT(IN) :: modpar_index  !<The parameter modpar index
    
    !Local variables
    REAL, DIMENSION(:), POINTER :: ptr
    REAL, TARGET :: v(1)
    
    SELECT CASE(modparid(modpar_index)%deptype)
    CASE(m_lpar)
      ptr => landpar(modparid(modpar_index)%parno,:)
    CASE(m_spar)
      ptr => soilpar(modparid(modpar_index)%parno,:)
    CASE(m_rpar)
      ptr => regpar(modparid(modpar_index)%parno,:)
    CASE(m_gpar)
      v(1) = genpar(modparid(modpar_index)%parno)
      ptr => v
    CASE DEFAULT
      RETURN
    END SELECT
    
    WRITE(funit_used,*) trim(modparid(modpar_index)%shortname), ':', ptr
    
  END SUBROUTINE print_param
  
  !>Check input
  !TODO: make check_input accept single values (or make other routine)
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_input(io_result,vector,tolerance,test_id,test_process,force)

    !Argument declaration
    LOGICAL, INTENT(INOUT) :: io_result       !<Input and Output logical for "AND" result
    REAL, DIMENSION(:), INTENT(IN) :: vector  !<The data vector to be tested
    INTEGER, INTENT(IN) :: tolerance          !<The tolerance to test against
    INTEGER, INTENT(IN) :: test_id       !<The index parameter specifying test case
    INTEGER, INTENT(IN) :: test_process       !<Process assigned to this test case
    LOGICAL, OPTIONAL, INTENT(IN) :: force    !<force the test eventhough already tested

    !Local variables
    INTEGER status
    LOGICAL test_forced

    test_forced = force_test
    IF (PRESENT(force)) THEN
      test_forced = force
    END IF

    IF ((test_id .LE. n_max_par) .OR. (test_id .GT. max_testcases)) THEN
      WRITE(*,*) 'ERROR, test input parameter could not be tested!', test_id
      io_result = .FALSE.
      RETURN
    END IF

    IF ((.NOT. all_test_cases(test_id)%tested) .OR. (test_forced)) THEN
      all_test_cases(test_id)%passed = real_check_tolerance(vector,tolerance,status)
      all_test_cases(test_id)%tested = .TRUE.
      all_test_cases(test_id)%kind_of_test = p_input
      all_test_cases(test_id)%tolerance = tolerance
      all_test_cases(test_id)%status = status
      all_test_cases(test_id)%name = testing_names(test_id)
      all_test_cases(test_id)%minvalue = MINVAL(vector)
      all_test_cases(test_id)%maxvalue = MAXVAL(vector)
    END IF

    io_result = io_result .AND. all_test_cases(test_id)%passed
    CALL add_tests(test_id, test_process, all_test_cases(test_id)%passed)

  END SUBROUTINE check_input
  
  !>Print input
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_input(line)
    
    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: line      !<name and interval of input variable
    
    WRITE(funit_used,*) TRIM(line)
!    WRITE(funit_used,*) trim(name), ': incorporated'
    
  END SUBROUTINE print_input
  
  !>Check forcing
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_forcing(io_result,force_param,test_id,test_process)

    USE WORLDVAR, ONLY : forcingdata

    !Argument declaration
    LOGICAL, INTENT(INOUT) :: io_result  !<Input and Output logical for "AND" result
    INTEGER, INTENT(IN) :: force_param   !<The forcing parameter to test
    INTEGER, INTENT(IN) :: test_id       !<The index parameter specifying test case
    INTEGER, INTENT(IN) :: test_process  !<Process assigned to this test case

    !Local variables

    IF ((test_id .LE. n_max_par) .OR. (test_id .GT. max_testcases)) THEN
      WRITE(*,*) 'ERROR, test forcing parameter could not be tested!', test_id
      io_result = .FALSE.
      RETURN
    END IF

    IF ((.NOT. all_test_cases(test_id)%tested) .OR. (force_test)) THEN
      all_test_cases(test_id)%passed = forcingdata(force_param)%readfile
      all_test_cases(test_id)%tested = .TRUE.
      all_test_cases(test_id)%kind_of_test = p_forcing
      all_test_cases(test_id)%tolerance = 0
      all_test_cases(test_id)%status = 0
      all_test_cases(test_id)%name = testing_names(test_id)
    END IF

    io_result = io_result .AND. all_test_cases(test_id)%passed
    CALL add_tests(test_id, test_process, all_test_cases(test_id)%passed)

  END SUBROUTINE check_forcing
  
  !>Print forcing
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_forcing(name)
    
    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: name      !<The name
    
    WRITE(funit_used,*) TRIM(name), ': incorporated'
    
  END SUBROUTINE print_forcing

  !>Check file existence
  !>
  !--------------------------------------------------------------------
  SUBROUTINE check_file_existence(io_result,filename,test_id,test_process)

    USE WORLDVAR, ONLY : modeldir

    !Argument declaration
    LOGICAL, INTENT(INOUT)       :: io_result     !<Input and Output logical for "AND" result
    CHARACTER(LEN=*), INTENT(IN) :: filename      !<The filename to test
    INTEGER, INTENT(IN)          :: test_id       !<The index parameter specifying test case
    INTEGER, INTENT(IN)          :: test_process  !<Process assigned to this test case

    !Local variables
    LOGICAL fileexist

    INQUIRE(FILE=TRIM(modeldir)//TRIM(filename),EXIST=fileexist)
    CALL add_generic_result(fileexist,0,test_id,test_process,TRIM(filename)//' exist')
    io_result = io_result .AND. fileexist

  END SUBROUTINE check_file_existence

  !>Add generic result
  !>
  !--------------------------------------------------------------------
  SUBROUTINE add_generic_result(test_result,status,test_id,test_process,name)

    !Argument declaration
    LOGICAL, INTENT(IN) :: test_result   !<Input logical for result
    INTEGER, INTENT(IN) :: status        !<The status of the test
    INTEGER, INTENT(IN) :: test_id       !<The index parameter specifying test case
    INTEGER, INTENT(IN) :: test_process  !<Process assigned to this test case
    CHARACTER(LEN=*), OPTIONAL, INTENT(IN) :: name !<Name to describe the generic test to show when printed

    !Local variables

    IF ((test_id .LE. n_max_par) .OR. (test_id .GT. max_testcases)) THEN
      WRITE(*,*) 'ERROR, generic parameter could not be added to test!', test_id
      RETURN
    END IF

    all_test_cases(test_id)%passed = test_result
    all_test_cases(test_id)%tested = .TRUE.
    all_test_cases(test_id)%kind_of_test = p_generic
    all_test_cases(test_id)%tolerance = 0
    all_test_cases(test_id)%status = status

    IF (PRESENT(name)) THEN
      all_test_cases(test_id)%name = TRIM(name)
    ELSE
      all_test_cases(test_id)%name = '?'
    END IF

    CALL add_tests(test_id, test_process, all_test_cases(test_id)%passed)

  END SUBROUTINE add_generic_result

  !>Print generic
  !>
  !--------------------------------------------------------------------
  SUBROUTINE print_generic(name)

    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: name      !<The name

    WRITE(funit_used,*) trim(name), ': incorporated'

  END SUBROUTINE print_generic

  !>Uncheck
  !>
  !--------------------------------------------------------------------
  SUBROUTINE uncheck(test_id,test_process)

    !Argument declaration
    INTEGER, INTENT(IN) :: test_id       !<The index parameter specifying test case
    INTEGER, INTENT(IN) :: test_process  !<Process assigned to this test case

    !Local variables
    INTEGER :: i,remove_index,substitute_index

    remove_index = -1
    substitute_index = 0
    LOOP : DO i = 1, max_testcase_values
        IF (tests_c2_process(i,test_process)%test_id .EQ. test_id) THEN
            remove_index = i
        ELSE IF (tests_c2_process(i,test_process)%test_id .GT. 0) THEN
            substitute_index = i
        ELSE
            EXIT LOOP
        END IF
    END DO LOOP

    IF (remove_index .GT. 0) THEN
        tests_c2_process(remove_index,test_process)%test_id = -1
        tests_c2_process(remove_index,test_process)%test_passed = .FALSE.
        IF ((substitute_index .GT. 0) .AND. ((substitute_index .GT. remove_index))) THEN
            tests_c2_process(remove_index,test_process)%test_id = tests_c2_process(substitute_index,test_process)%test_id
            tests_c2_process(remove_index,test_process)%test_passed = tests_c2_process(substitute_index,test_process)%test_passed
            tests_c2_process(substitute_index,test_process)%test_id = -1
            tests_c2_process(substitute_index,test_process)%test_passed = .FALSE.
        END IF
    END IF

  END SUBROUTINE uncheck

  !>Check tolerance real
  !>Real
  !--------------------------------------------------------------------
  LOGICAL FUNCTION real_check_tolerance(vector,tolerance,status)
  
    !Argument declaration
    REAL, DIMENSION(:), INTENT(IN) :: vector     !<The vector to be tested
    INTEGER, INTENT(IN)            :: tolerance  !<The tolerance parameter
    INTEGER, INTENT(OUT)           :: status     !<The returned status

    !Local variables
    LOGICAL retval,needed_retval
    INTEGER needed_status
    REAL aimed_value,tolerance_min,tolerance_max
    
    SELECT CASE(tolerance)
    CASE(eq_zero)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 0.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ne_zero)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 0.0
      needed_retval = .FALSE.
      needed_status = 0
    CASE(gt_zero)
      aimed_value = 0.0
      tolerance_min = TINY(0.0)
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(lt_zero)
      aimed_value = 0.0
      tolerance_min = -HUGE(0.0)
      tolerance_max = -TINY(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_zero)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(le_zero)
      aimed_value = 0.0
      tolerance_min = -HUGE(0.0)
      tolerance_max = 0.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_zero_and_lt_one)
      aimed_value = 0.0
      tolerance_min = TINY(0.0)
      tolerance_max = 1.0-TINY(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_zero_and_lt_one)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 1.0-TINY(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_zero_and_le_one)
      aimed_value = 0.0
      tolerance_min = TINY(0.0)
      tolerance_max = 1.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_zero_and_le_one)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 1.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_minus_one_and_lt_plus_one)
      aimed_value = 0.0
      tolerance_min = -1.0+TINY(0.0)
      tolerance_max = 1.0-TINY(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_minus_one)
      aimed_value = 0.0
      tolerance_min = -1.0+TINY(0.0)
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_minus_one)
      aimed_value = 0.0
      tolerance_min = -1.0
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(plus_minus_90)
      aimed_value = 0.0
      tolerance_min = -90.0
      tolerance_max = 90.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(plus_minus_180)
      aimed_value = 0.0
      tolerance_min = -180.0
      tolerance_max = 180.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(one_plus_minus_point_1_percent)
      aimed_value = 1.0
      tolerance_min = -0.001
      tolerance_max = 0.001
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_lowest_natural_point)
      aimed_value = 0.0
      tolerance_min = -418.0 ![m] below sea level; shore of the dead sea
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_zero_and_le_hundred)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 100.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_zero_and_le_366)
      aimed_value = 0.0
      tolerance_min = 0.0
      tolerance_max = 366.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(gt_zero_and_lt_366)
      aimed_value = 0.0
      tolerance_min = 1.0
      tolerance_max = 365.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_one)
      aimed_value = 1.0
      tolerance_min = 0.0
      tolerance_max = HUGE(0.0)
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE(ge_one_and_le_three)
      aimed_value = 1.0
      tolerance_min = 0.0
      tolerance_max = 2.0
      needed_retval = .TRUE.
      needed_status = SIZE(vector)
    CASE DEFAULT
      !Warning, return false as default
      real_check_tolerance = .FALSE.
      RETURN
    END SELECT
    
    retval = data_is_within_tolerance(vector,aimed_value,tolerance_min,tolerance_max,status)
    IF ((retval .EQV. needed_retval) .AND. (status .EQ. needed_status)) THEN
      real_check_tolerance = .TRUE.
    ELSE
      real_check_tolerance = .FALSE.
    END IF
  
  END FUNCTION real_check_tolerance
  
  
  !>Check that data differs from value
  !>Integer
  !--------------------------------------------------------------------
  LOGICAL FUNCTION int_data_differs_from_value(vector,value)

	!Argument declaration
    INTEGER, DIMENSION(:), INTENT(IN)  :: vector  !<Vector that is being verified
    INTEGER, INTENT(IN)                :: value   !<Value specifying a NOT wanted value
    
    !Local variables
    LOGICAL ret
    INTEGER num
    
    ret = data_is_within_tolerance(vector,value,0,0,num)
    IF ((.NOT. ret) .AND. (num .EQ. 0)) THEN
      int_data_differs_from_value = .TRUE.
    ELSE
      int_data_differs_from_value = .FALSE.
    END IF

  END FUNCTION int_data_differs_from_value
  
  !>Check that data differs from value
  !>Real
  !--------------------------------------------------------------------
  LOGICAL FUNCTION real_data_differs_from_value(vector,value)

	!Argument declaration
    REAL, DIMENSION(:), INTENT(IN)  :: vector  !<Vector that is being verified
    REAL, INTENT(IN)                :: value   !<Value specifying a NOT wanted value
    
    !Local variables
    LOGICAL ret
    INTEGER num
    
    ret = data_is_within_tolerance(vector,value,0.0,0.0,num)
    IF ((.NOT. ret) .AND. (num .EQ. 0)) THEN
      real_data_differs_from_value = .TRUE.
    ELSE
      real_data_differs_from_value = .FALSE.
    END IF

  END FUNCTION real_data_differs_from_value
  
  !>Check that data is within tolerance
  !>Integer
  !--------------------------------------------------------------------
  LOGICAL FUNCTION int_data_is_within_tolerance(vector,aimed_value,tolerance_min,tolerance_max,num_tolerance)

	!Argument declaration
    INTEGER, DIMENSION(:), INTENT(IN)     :: vector  !<Data vector that is being checked
    INTEGER, INTENT(IN)     :: aimed_value    !<Value specifying a wanted value
    INTEGER, INTENT(IN)     :: tolerance_min  !<Value specifying how far below wanted value is ok 
    INTEGER, INTENT(IN)     :: tolerance_max  !<Value specifying how far above wanted value is ok
    INTEGER, INTENT(OUT)    :: num_tolerance  !<Value specifying how many values in vector is within tolerance
    
    !Local variables
    INTEGER k
    
    num_tolerance = 0
    DO k = 1, SIZE(vector)
      IF ((vector(k)-aimed_value) .GE. tolerance_min .AND. &
          (vector(k)-aimed_value) .LE. tolerance_max) THEN
        num_tolerance = num_tolerance + 1
      END IF
    END DO
    
    IF (SIZE(vector) .EQ. num_tolerance) THEN
      int_data_is_within_tolerance = .TRUE.
    ELSE
      int_data_is_within_tolerance = .FALSE.
    END IF

  END FUNCTION int_data_is_within_tolerance

  !>Check that data is within tolerance
  !>Real
  !--------------------------------------------------------------------
  LOGICAL FUNCTION real_data_is_within_tolerance(vector,aimed_value,tolerance_min,tolerance_max,num_tolerance)

	!Argument declaration
    REAL, DIMENSION(:), INTENT(IN)     :: vector  !<Data vector that is being checked
    REAL, INTENT(IN)      :: aimed_value    !<Value specifying a wanted value
    REAL, INTENT(IN)      :: tolerance_min  !<Value specifying how far below wanted value is ok 
    REAL, INTENT(IN)      :: tolerance_max  !<Value specifying how far above wanted value is ok
    INTEGER, INTENT(OUT)  :: num_tolerance  !<Value specifying how many values in vector is within tolerance
    
    !Local variables
    INTEGER k
    
    num_tolerance = 0
    DO k = 1, SIZE(vector)
      IF ((vector(k)-aimed_value) .GE. tolerance_min .AND. &
          (vector(k)-aimed_value) .LE. tolerance_max) THEN
        num_tolerance = num_tolerance + 1
      END IF
    END DO
    
    IF (SIZE(vector) .EQ. num_tolerance) THEN
      real_data_is_within_tolerance = .TRUE.
    ELSE
      real_data_is_within_tolerance = .FALSE.
    END IF

  END FUNCTION real_data_is_within_tolerance

  !>find petmodels used
  !--------------------------------------------------------------------
  INTEGER FUNCTION find_petmodels_used(petmodels_used, petmodels_used_size)
  
    USE MODVAR, ONLY : maxmodelsinoption, &
                       petmodel,          &
                       modeloption,       &
                       p_petmodel

	!Argument declaration
    INTEGER, DIMENSION(:), INTENT(OUT)  :: petmodels_used       !<will hold models found in basins petmodel array
    INTEGER, INTENT(IN)                 :: petmodels_used_size  !<size of the petmodels_used array
    
    !Local variables
    INTEGER i, j
    
    find_petmodels_used = 0
    IF (petmodels_used_size .LE. 0 .OR. petmodels_used_size .GT. maxmodelsinoption(p_petmodel)) THEN
      find_petmodels_used = -1
    ELSE IF (modeloption(p_petmodel) .GT. 0) THEN
      find_petmodels_used = 1
      petmodels_used(find_petmodels_used) = modeloption(p_petmodel)
    ELSE IF (ALLOCATED(petmodel)) THEN
      find_petmodels_used = 1;
      petmodels_used(find_petmodels_used) = petmodel(1)
      DO i = 1, SIZE(petmodel)
        INNER_LOOP : DO j = 1, find_petmodels_used
          IF (petmodels_used(j) .EQ. petmodel(i)) THEN
            !break inner loop if found in petmodels_used
            EXIT INNER_LOOP
          ELSE IF (petmodels_used(j) .NE. petmodel(i) .AND. j .EQ. find_petmodels_used) THEN
            !if j is last element and petmodel index is not found there, add it.
            !be sure though that we do not exceed petmodels_used array to do so.
            IF (find_petmodels_used .LE. maxmodelsinoption(p_petmodel)) THEN
              find_petmodels_used = find_petmodels_used + 1
              petmodels_used(find_petmodels_used) = petmodel(i)
            END IF
            EXIT INNER_LOOP
          END IF
        END DO INNER_LOOP
      END DO
    END IF
  
  END FUNCTION find_petmodels_used

  !>Write petmodels found to the log file
  !--------------------------------------------------------------------
  SUBROUTINE log_petmodels_used(funit,petmodels_used,num_petmodels_used)
  
    USE MODVAR, ONLY : maxmodelsinoption, &
                       petmodelnames,     &
                       modeloptionname,   &
                       p_petmodel

	!Argument declaration
    INTEGER, INTENT(IN)                :: funit               !<File unit of log-file
    INTEGER, DIMENSION(:), INTENT(IN)  :: petmodels_used      !<will hold petmodels used
    INTEGER, INTENT(IN)                :: num_petmodels_used  !<number of used petmodels
    
    !Local variables
    INTEGER i

    !log all PETmodels used
    IF (SIZE(petmodels_used) .GE. num_petmodels_used) THEN
      WRITE(funit, '(A6,A,A7)', ADVANCE='NO') 'INFO: ',TRIM(modeloptionname(p_petmodel)), ' used: '
      DO i = 1, num_petmodels_used
        IF (petmodels_used(i)+1 .LE. maxmodelsinoption(p_petmodel)) THEN
          IF (i .GT. 1) THEN
            WRITE(funit, '(A2)', ADVANCE='NO') ', '
          END IF
          WRITE(funit, '(A1,I1,A1,A)', ADVANCE='NO') '(', petmodels_used(i), ')', TRIM(petmodelnames(petmodels_used(i)))
        END IF
      END DO
      WRITE(funit, *)
    END IF
  
  END SUBROUTINE log_petmodels_used
  
  !>Check for negative data in file maybe also for missing values (Pobs.txt, Qobs.txt)
  !------------------------------------------------------------
  SUBROUTINE check_data_positive(filepath,notimefound,ncols,ns,oindex,miss,numneg,negfound)

    USE WORLDVAR, ONLY : readmatlab,      &
                         fileunit_temp   
    USE MODVAR, ONLY : missing_value

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: filepath  !<File
    LOGICAL, INTENT(IN)  :: notimefound  !<Flag for time format: date or date and time
    INTEGER, INTENT(IN)  :: ncols        !<Number of data columns in file
    INTEGER, INTENT(IN)  :: ns           !<Number of subbasins, basemodel
    INTEGER, INTENT(IN)  :: oindex(ns)   !<Index for columns used in model set-up
    INTEGER, INTENT(IN)  :: miss         !<Flag for counting missing values, 0=count, 1=ignore
    INTEGER, INTENT(OUT) :: numneg(ns)   !<Number of negative values per subbasin
    INTEGER, INTENT(OUT) :: negfound     !<Code for found negative data
    
    !Local variables
    INTEGER timeform
    INTEGER i
    INTEGER :: neg(ncols)
    REAL :: y(ncols)                 !Data (one time step)
    CHARACTER(LEN=16)  d2,d3         !Date yyyy-mm-dd[ hh:mm]

    IF(notimefound)THEN
      timeform = 0
    ELSE
      timeform = 1
    ENDIF

    !Load precipitation forcing data
    OPEN(UNIT = fileunit_temp,FILE = filepath, STATUS = 'old', ACTION='read')
    READ(fileunit_temp,*)  !Skip heading
    neg = 0
    numneg = 0
    DO
      y = missing_value
      IF(readmatlab)THEN
        READ(fileunit_temp,*,END=900) d2,y
      ELSEIF(timeform==0)THEN
        READ(fileunit_temp,*,END=900) d2,y
      ELSEIF(timeform==1)THEN
        READ(fileunit_temp,*,END=900) d2,d3,y    
      ENDIF
      DO i = 1,ncols
        IF(miss==0)THEN
          IF(y(i)<0.) neg(i) = neg(i) + 1
        ELSEIF(miss==1)THEN
          IF(y(i)<0..AND.y(i)/=missing_value) neg(i) = neg(i) + 1
        ENDIF
      ENDDO  
    ENDDO
900 IF(SUM(neg)==0.)THEN
      negfound = 0    !no negative data in file
    ELSE
      negfound = 1
      DO i = 1,ns
        IF(oindex(i)>0)THEN
          numneg(i) = neg(oindex(i))
        ENDIF
      ENDDO
    ENDIF

    CLOSE(fileunit_temp)

  END SUBROUTINE check_data_positive

  !>Check for missing data in file (Tobs.txt)
  !------------------------------------------------------------
  SUBROUTINE check_data_missing(filepath,notimefound,ncols,ns,oindex,nummiss,missfound)

    USE WORLDVAR, ONLY : readmatlab,      &
                         fileunit_temp   
    USE MODVAR, ONLY : missing_value

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: filepath  !<File
    LOGICAL, INTENT(IN)  :: notimefound  !<Flag for time format: date or date and time
    INTEGER, INTENT(IN)  :: ncols        !<Number of data columns in file
    INTEGER, INTENT(IN)  :: ns           !<Number of subbasins, basemodel
    INTEGER, INTENT(IN)  :: oindex(ns)   !<Index for columns used in model set-up
    INTEGER, INTENT(OUT) :: nummiss(ns)  !<Number of missing values per subbasin
    INTEGER, INTENT(OUT) :: missfound    !<Code for found missing data
    
    !Local variables
    INTEGER timeform
    INTEGER i
    INTEGER :: miss(ncols)
    REAL :: y(ncols)                 !Data (one time step)
    CHARACTER(LEN=16)  d2,d3         !Date yyyy-mm-dd[ hh:mm]

    IF(notimefound)THEN
      timeform = 0
    ELSE
      timeform = 1
    ENDIF

    !Load precipitation forcing data
    OPEN(UNIT = fileunit_temp,FILE = filepath, STATUS = 'old', ACTION='read')
    READ(fileunit_temp,*)  !Skip heading
    miss = 0
    nummiss = 0
    DO
      y = missing_value
      IF(readmatlab)THEN
        READ(fileunit_temp,*,END=900) d2,y
      ELSEIF(timeform==0)THEN
        READ(fileunit_temp,*,END=900) d2,y
      ELSEIF(timeform==1)THEN
        READ(fileunit_temp,*,END=900) d2,d3,y    
      ENDIF
      DO i = 1,ncols
        IF(y(i)==missing_value) miss(i) = miss(i) + 1
      ENDDO  
    ENDDO
900 IF(SUM(miss)==0.)THEN
      missfound = 0    !no missing data in file
    ELSE
      missfound = 1
      DO i = 1,ns
        IF(oindex(i)>0)THEN
          nummiss(i) = miss(oindex(i))
        ENDIF
      ENDDO
    ENDIF

    CLOSE(fileunit_temp)

  END SUBROUTINE check_data_missing

END MODULE MODEL_TEST_ROUTINES
