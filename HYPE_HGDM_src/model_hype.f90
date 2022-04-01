!> \file model_hype.f90
!> Contains module modelmodule.

!>Main module for hydrological model HYPE.
!>
MODULE MODELMODULE
!The HYPE model (HYdrological Predictions for the Environment)

!Copyright 2011-2020 SMHI
!
!This file is part of HYPE.
!HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
!You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.

!----------------------------------------------------------------------

!Used modules
  USE STATETYPE_MODULE  !model state variable types
  USE MODVAR            !model variables, general model
  USE HYPEVARIABLES     !model variables, HYPE model
  USE HYPE_WATERBALANCE !water balance variables and indices
  USE UPDATING

!Subroutines also uses GENERAL_WATER_CONCENTRATION, GLACIER_SOILMODEL, SOILMODEL_DEFAULT,
!SOIL_PROCESSES, NPC_SOIL_PROCESSES, SURFACEWATER_PROCESSES, NPC_SURFACEWATER_PROCESSES,
!IRRIGATION_MODULE, REGIONAL_GROUNDWATER_MODULE

  IMPLICIT NONE
  PRIVATE

  ! Private subroutines
  !--------------------------------------------------------------------
  ! calculate_lake_volume_output
  ! initialize_class_outvar_to_zero
  ! calculate_class_outvar_initialize
  ! calculate_class_outvar_add
  ! calculate_class_outvar_add_amount
  ! calculate_class_outvar_add_accumulate
  ! calculate_class_outvar_finish
  ! calculate_class_outvar_finish_scale
  ! set_class_outvar_missing
  ! calculate_outvar_watertemperature
  ! set_outvar_xobs
  ! set_outvar_xobs_scaled
  ! set_outvar_xobsmean
  ! set_outvar_xobsstate
  ! calculate_regional_groundwaterflow_to_outside_system
  ! get_irrigation_parameters
  ! wetland_wbstore
  ! calculate_flow_from_undivided_lake
  ! calculate_flow_for_lakebasin_lake
  ! accumulate_flow_to_downstream_subbasin
  ! calculate_internal_wetland
  ! calculate_outlet_wetland
  ! calculate_local_river
  !--------------------------------------------------------------------
  PUBLIC :: model_version_information, &
            define_output_variables, &
            define_model_parameters, &
            initiate_model_state, &
            initiate_model, &
            set_special_models, &
            set_parameters_region_division, &
            get_special_model_parameters, &
            set_modelconfig_from_parameters, &
            calculate_special_model_parameters, &
            calculate_translation_time, &
            model, &
            load_modeldefined_input, &
            reload_modeldefined_observations, &
            open_modeldefined_outputfiles, &
            close_modeldefined_outputfiles

  CONTAINS

  !>Information about the model version to print in log-file
  !-----------------------------------------------------------
  SUBROUTINE model_version_information(funit)

    !Argument declarations
    INTEGER, INTENT(IN) :: funit !<fileunit for log-file

    IF(funit==0)THEN
      WRITE(*,601) '---------------------------------------------'
      WRITE(*,601) '        HYPE-HGDM version 5.12.1             '
	  WRITE(*,601) '             W A R N I N G                   '
	  WRITE(*,601) 'This version is still under development, and '
	  WRITE(*,601) 'is intended to be used for the prairie       '
	  WRITE(*,601) 'pothole region. Regular ilake is not working '
	  WRITE(*,601) 'and HGDM is activated by default.            '
      WRITE(*,601) '---------------------------------------------'
      WRITE(*,601) 'New versions of HYPE at the HYPE Open Source '
      WRITE(*,601) 'Community website (hypecode.smhi.se), as well'
      WRITE(*,601) 'as information on up-coming courses.         '
      WRITE(*,601) '---------------------------------------------'
    ELSE
      WRITE(funit,600) '----------------------'
      WRITE(funit,600) ' HYPE version 5.12.1  '
      WRITE(funit,600) '----------------------'
    ENDIF
600 FORMAT(A22)
601 FORMAT(A46)

  END SUBROUTINE model_version_information

  !>Set variables holding output information from the HYPE model;
  !!outvarid and loadheadings
  !-----------------------------------------------------------------
  SUBROUTINE define_output_variables()

    max_outvar = o_max_outvar     !number of output variables, set in hypevar
    ALLOCATE(outvarid(max_outvar))

    !Initialize
    outvarid(:) = outvaridtype('?','?',0,0,0,0,'?','?','?')

    outvarid(o_crun) = outvaridtype('crun','computed runoff', i_sum , i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_rrun) = outvaridtype('rrun','recorded runoff', i_sum , i_notdef,i_notdef,i_notdef,'mm' ,'mm' ,'')
    outvarid(o_prec) = outvaridtype('prec','precipitation'  , i_sum , i_notdef,i_basin,i_notdef,'mm' ,'mm' ,'mm')
    outvarid(o_tobs)  = outvaridtype('temp','air temperature'   ,i_mean , i_notdef,i_basin,i_notdef,'deg'  ,'degree Celsius','deg')
    outvarid(o_crunT1)  = outvaridtype('coT1','computed T1 runoff',i_wmean, o_crun,i_notdef,i_class,'?','?','')
    outvarid(o_crunT2)  = outvaridtype('coT2','computed T2 runoff',i_wmean, o_crun,i_notdef,i_class,'deg' ,'degree Celsius','')
    outvarid(o_roum) = outvaridtype('roum','rec. outflow main',i_mean, i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(o_cprecT1) = outvaridtype('cpT1','recorded T1 precip',i_wmean2, o_prec,i_notdef,i_notdef,'?','?','')
    outvarid(o_cevapT1) = outvaridtype('ceT1','computed T1 evap'  ,i_wmean, o_evap,i_notdef,i_class,'?' ,'?','')
    outvarid(o_soim) = outvaridtype('soim','computed soil water',i_mean, i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_csoilT1) = outvaridtype('csT1','computed T1 soil'  ,i_wmean, o_soim,i_notdef,i_class,'?' ,'?','')
    outvarid(o_csoilT2) = outvaridtype('csT2','computed T2 soil'  ,i_wmean, o_soim,i_notdef,i_class,'deg','degree Celsius','')
    outvarid(o_roub) = outvaridtype('roub','rec. outflow branch',i_mean, i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(o_snow) = outvaridtype('snow','snow pack'         ,i_mean , i_notdef,i_land,i_class,'mm' ,'mm water' ,'mm')
    outvarid(o_evap) = outvaridtype('evap','subbasin evaporation',i_sum , i_notdef,i_basin,i_class,'mm'   ,'mm' ,'mm')
    outvarid(o_reT1) = outvaridtype('reT1','recorded T1 outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'?' ,'?','')
    outvarid(o_reT2) = outvaridtype('reT2','recorded T2 outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_snowdens) = outvaridtype('sden','computed snow density',i_wmean, o_snowdepth,i_notdef,i_class,'g/cm3','g/cm3','')
    outvarid(o_grwlevel) = outvaridtype('gwat','computed grw level',i_mean , i_notdef,i_land,i_class,'m' ,'m' ,'m')
    outvarid(o_crunIN) = outvaridtype('coIN','computed IN runoff',i_wmean, o_crun,i_notdef,i_class,'ug/L' ,'ug Inorg-N/L','')
    outvarid(o_crunON) = outvaridtype('coON','computed ON runoff',i_wmean, o_crun,i_notdef,i_class,'ug/L' ,'ug Org-N/L','')
    outvarid(o_crunSP) = outvaridtype('coSP','computed SP runoff',i_wmean, o_crun,i_notdef,i_class,'ug/L' ,'ug SRP-P/L','')
    outvarid(o_crunPP) = outvaridtype('coPP','computed PP runoff',i_wmean, o_crun,i_notdef,i_class,'ug/L' ,'ug PartP-P/L','')
    outvarid(o_reIN) = outvaridtype('reIN','recorded IN outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'ug/L' ,'ug Inorg-N/L','')
    outvarid(o_reON) = outvaridtype('reON','recorded ON outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'ug/L' ,'ug Org-N/L','')
    outvarid(o_reSP) = outvaridtype('reSP','recorded SP outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'ug/L' ,'ug SRP-P/L','')
    outvarid(o_rePP) = outvaridtype('rePP','recorded PP outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'ug/L' ,'ug PartP-P/L','')
    outvarid(o_reTN) = outvaridtype('reTN','recorded TN outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_reTP) = outvaridtype('reTP','recorded TP outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(30) = outvaridtype('qerr','outflow error',i_mean , i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(31) = outvaridtype('cobc','computed outflow before correction',i_mean , i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(32) = outvaridtype('wtmp','computed water temp' ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(33) = outvaridtype('werr','waterstage error',i_mean , i_notdef,i_notdef,i_notdef,'m','m','')
    outvarid(34) = outvaridtype('cwbc','computed wcom before correction',i_mean , i_notdef,i_notdef,i_notdef,'m','m','')
    outvarid(o_reOC) = outvaridtype('reOC','recorded TOC outflow',i_wmean2, o_rout,i_notdef,i_notdef,'mg/L','mg org-C/L','')
    outvarid(o_csoilIN) = outvaridtype('csIN','computed IN soil'    ,i_wmean, o_soim,i_notdef,i_class,'ug/L','ug INORG-N/L','')
    outvarid(o_soilfrost) = outvaridtype('sfst','computed soil frost' ,i_mean , i_notdef,i_land,i_class, 'cm'  ,'cm' ,'cm')
    outvarid(o_soiltmp) = outvaridtype('stmp','computed soil temp'  ,i_mean , i_notdef,i_land,i_class, 'deg' ,'degree Celcius','deg')
    outvarid(o_snowdepth) = outvaridtype('sdep','computed snow depth' ,i_mean , i_notdef,i_land,i_class, 'cm'  ,'cm' ,'cm')
    outvarid(o_epot) = outvaridtype('epot','potential evap'      ,i_sum  , i_notdef,i_basin,i_class, 'mm'  ,'mm' ,'mm')
    outvarid(o_reepot) = outvaridtype('repo','recorded pot. evap',i_sum ,i_notdef,i_notdef,i_notdef,'mm' ,'mm' ,'')
    outvarid(42) = outvaridtype('eobs','recorded evaporation'       ,i_sum , i_notdef,i_notdef,i_notdef,'mm' ,'mm' ,'')
    outvarid(o_cprc) = outvaridtype('cprc','corr precipitation'     ,i_sum , i_notdef,i_basin,i_class,'mm'   ,'mm' ,'mm')
    outvarid(o_crunOC) = outvaridtype('coOC','computed TOC runoff',i_wmean, o_crun,i_notdef,i_class,'mg/L' ,'mg org-C/L','')
    outvarid(o_csoilOC) = outvaridtype('csOC','computed TOC soil'  ,i_wmean, o_soim,i_notdef,i_class,'mg/L','mg org-C/L','')
    outvarid(46) = outvaridtype('ccOC','computed TOC outflow',i_wmean, o_cout,i_notdef,i_notdef,'mg/L','mg org-C/L','')
    outvarid(o_phC(1)) = outvaridtype('phC1','pool humusC soil1',i_mean , i_notdef,i_land,i_class,'kg/km2' ,'kg org-C/km2','kg/km2')
    outvarid(o_pfC(1)) = outvaridtype('pfC1','pool fastC soil1',i_mean , i_notdef,i_land,i_class,'kg/km2' ,'kg org-C/km2','kg/km2')
    outvarid(o_phC(2)) = outvaridtype('phC2','pool humusC soil2',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg org-C/km2','')
    outvarid(o_pfC(2)) = outvaridtype('pfC2','pool fastC soil2',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg org-C/km2','')
    outvarid(o_wcom) = outvaridtype('wcom','olake water stage',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(o_rewstr) = outvaridtype('wstr','rec olake water st',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(o_coul) = outvaridtype('coul','lake total outflow',i_mean , i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    !outvarid(o_cout2) = outvaridtype('coun','neg comp outflow',i_mean , i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(o_cout) = outvaridtype('cout','comp outflow subbasi',i_mean , i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(o_rout) = outvaridtype('rout','rec outflow subbasin',i_mean , i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(55) = outvaridtype('ccIN','comp conc IN olake',i_wmean, o_cout,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(56) = outvaridtype('ccON','comp conc ON olake',i_wmean, o_cout,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(57) = outvaridtype('ccSP','comp conc SP olake',i_wmean, o_cout,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(58) = outvaridtype('ccPP','comp conc PP olake',i_wmean, o_cout,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(59) = outvaridtype('rsnw','recorded snow depth',i_mean , i_notdef,i_notdef,i_notdef,'cm','cm' ,'')
    outvarid(60) = outvaridtype('resf','recorded soil frost',i_mean , i_notdef,i_notdef,i_notdef,'cm','cm' ,'')
    outvarid(61) = outvaridtype('regw','recorded grw level',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(63) = outvaridtype('ccT1','comp conc T1 olake',i_wmean, o_cout,i_notdef,i_notdef,'?','?','')
    outvarid(64) = outvaridtype('ccT2','comp conc T2 olake',i_wmean, o_cout,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_crun3) = outvaridtype('craw','runoff including wetland',i_sum, i_notdef,i_notdef,i_notdef,'mm','mm','')
    outvarid(o_crunTN) = outvaridtype('coTN','computed TN runoff',i_wmean, o_crun,i_notdef,i_class,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_crunTP) = outvaridtype('coTP','computed TP runoff',i_wmean, o_crun,i_notdef,i_class,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_pfN(1)) = outvaridtype('pfN1','pool fastN soil1' ,i_mean , i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_pfN(2)) = outvaridtype('pfN2','pool fastN soil2' ,i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_pfN(3)) = outvaridtype('pfN3','pool fastN soil3' ,i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_phN(1)) = outvaridtype('phN1','pool humusN soil1',i_mean , i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_phN(2)) = outvaridtype('phN2','pool humusN soil2',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_phN(3)) = outvaridtype('phN3','pool humusN soil3',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_psoilIN(1)) = outvaridtype('pIN1','pool InorgN soil1',i_mean , i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_psoilIN(2)) = outvaridtype('pIN2','pool InorgN soil2',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_psoilIN(3)) = outvaridtype('pIN3','pool InorgN soil3',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(77) = outvaridtype('ccTN','computed TN olake',i_wmean, o_cout,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(78) = outvaridtype('ccTP','computed TP olake',i_wmean, o_cout,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_ro1) = outvaridtype('cro1','computed runoff 1',i_sum  , i_notdef,i_land,i_class,'mm'     ,'mm' ,'mm')
    outvarid(o_ro2) = outvaridtype('cro2','computed runoff 2',i_sum  , i_notdef,i_notdef,i_class,'mm'     ,'mm' ,'')
    outvarid(81) = outvaridtype('cgwl','computed groundwater loss',i_mean ,i_notdef, i_notdef,i_notdef,'m3/s' ,'m3/s','')
    outvarid(o_rod) = outvaridtype('crod','computed rf drain',i_sum  , i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_ros) = outvaridtype('cros','computed surface rf'    ,i_sum ,i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_soildenitr) = outvaridtype('deni','denitrifikation'  ,i_sum , i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_cropNupt) = outvaridtype('crut','crop uptake'      ,i_sum , i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_degrfN) = outvaridtype('faIN','fast to inorganic',i_sum , i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_soilNatm) = outvaridtype('atmd','atm deposition N' ,i_sum , i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_ppP(1)) = outvaridtype('ppP1','pool partP soil1',i_mean , i_notdef,i_land,i_class,'kg/km2' ,'kg P/km2','kg/km2')
    outvarid(o_ppP(2)) = outvaridtype('ppP2','pool partP soil2',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg P/km2','')
    outvarid(o_ppP(3)) = outvaridtype('ppP3','pool partP soil3',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg P/km2','')
    outvarid(o_psoilSP(1)) = outvaridtype('pSP1','pool SRP soil1',  i_mean , i_notdef,i_land,i_class,'kg/km2' ,'kg P/km2','kg/km2')
    outvarid(o_psoilSP(2)) = outvaridtype('pSP2','pool SRP soil2',  i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg P/km2','')
    outvarid(o_psoilSP(3)) = outvaridtype('pSP3','pool SRP soil3',  i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg P/km2','')
    outvarid(o_pfP(1)) = outvaridtype('pfP1','pool fastP soil1',i_mean , i_notdef,i_land,i_class,'kg/km2' ,'kg P/km2','kg/km2')
    outvarid(o_pfP(2)) = outvaridtype('pfP2','pool fastP soil2',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg P/km2','')
    outvarid(o_pfP(3)) = outvaridtype('pfP3','pool fastP soil3',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg P/km2','')
    outvarid(o_cprecIN) = outvaridtype('cpIN','recorded IN precip',i_wmean2, o_prec,i_notdef,i_notdef,'ug/L','ug N/L','')
    outvarid(o_cprecSP) = outvaridtype('cpSP','recorded SP precip',i_wmean2, o_prec,i_notdef,i_notdef,'ug/L','ug P/L','')
    outvarid(o_reswe) = outvaridtype('rswe','rec snow water eq.',i_mean , i_notdef,i_notdef,i_notdef,'mm','mm' ,'')
    outvarid(100) = outvaridtype('acdf','accumulated volume error',i_sum , i_notdef,i_notdef,i_notdef,'mm','mm' ,'')
    outvarid(o_cloc) = outvaridtype('cloc','comp local flow',   i_mean , i_notdef,i_notdef,i_notdef, 'm3/s','m3/s','')
    outvarid(102) = outvaridtype('clIN','comp local conc IN',i_wmean, o_cloc,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(103) = outvaridtype('clON','comp local conc ON',i_wmean, o_cloc,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(104) = outvaridtype('clSP','comp local conc SP',i_wmean, o_cloc,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(105) = outvaridtype('clPP','comp local conc PP',i_wmean, o_cloc,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(106) = outvaridtype('clTN','comp local conc TN',i_wmean, o_cloc,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(107) = outvaridtype('clTP','comp local conc TP',i_wmean, o_cloc,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_pfC(3)) = outvaridtype('pfC3','pool fastC soil3',  i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg org-C/km2','')
    outvarid(o_phC(3)) = outvaridtype('phC3','pool humusC soil3', i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg org-C/km2','')
    outvarid(o_ctnl) = outvaridtype('cTNl','comp load TN olake',i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg totN','')
    outvarid(o_ctpl) = outvaridtype('cTPl','comp load TP olake',i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg totP','')
    outvarid(112) = outvaridtype('cinf','comp inflow to olake',i_mean , i_notdef,i_notdef,i_notdef,'m3/s' ,'m3/s','')
    outvarid(113) = outvaridtype('rinf','rec inflow to olake',i_mean , i_notdef,i_notdef,i_notdef,'m3/s' ,'m3/s','')
    outvarid(114) = outvaridtype('clrv','comp local river volume',i_mean , i_notdef,i_scale3,i_notdef,'m3' ,'m3' ,'mm')
    outvarid(115) = outvaridtype('cmrv','comp main river volume', i_mean , i_notdef,i_scale3,i_notdef,'m3' ,'m3' ,'mm')
    outvarid(o_soilPatm) = outvaridtype('atmp','atm deposition TP', i_sum , i_notdef,i_land,i_class,'kg/km2' ,'kg P/km2','kg/km2')
    outvarid(117) = outvaridtype('glcv','comp glacier ice volume', i_mean , i_notdef,i_scale12,i_notdef,'km3' ,'km3','mm')
    outvarid(118) = outvaridtype('glca','comp glacier area', i_mean , i_notdef,i_scale6,i_notdef,'km2' ,'km2','-')
    outvarid(119) = outvaridtype('rtoN','rec load TN olake', i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg totN','')
    outvarid(120) = outvaridtype('rtoP','rec load TN olake', i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg totP','')
    outvarid(o_ctmp) = outvaridtype('ctmp','corrected air temperature'   ,i_mean , i_notdef,i_basin,i_class,'deg'  ,'degree Celsius','deg')
    outvarid(122) = outvaridtype('irel','irrigation evap losses'   ,i_sum , i_notdef,i_scale3,i_notdef,'m3'  ,'m3' ,'mm')
    outvarid(o_coum) = outvaridtype('coum','comp outflow main',i_mean , i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(124) = outvaridtype('irld','abstr local dam f irr'    ,i_sum , i_notdef,i_scale3,i_notdef,'m3'  ,'m3' ,'mm')
    outvarid(125) = outvaridtype('irlr','abstr local river f irr'  ,i_sum , i_notdef,i_scale3,i_notdef,'m3'  ,'m3' ,'mm')
    outvarid(o_applirr) = outvaridtype('irra','applied irrigation water' ,i_sum , i_notdef,i_scale3,i_class,'m3'  ,'m3' ,'mm')
    outvarid(127) = outvaridtype('irrg','gwater abstracted f irr'  ,i_sum , i_notdef,i_scale3,i_notdef,'m3'  ,'m3' ,'mm')
    outvarid(128) = outvaridtype('irrs','irr abstr f other subbasins'   ,i_sum , i_notdef,i_scale3,i_notdef,'m3'  ,'m3' ,'mm')
    outvarid(o_crun2) = outvaridtype('loff','computed runoff',i_mean, i_notdef,i_notdef,i_class,'L/km2s','L/km2/s','')
    outvarid(o_coub) = outvaridtype('coub','comp outflow branch',i_mean , i_notdef,i_notdef,i_notdef,'m3/s','m3/s','')
    outvarid(o_soildef) = outvaridtype('smdf','soil moisture deficit' ,i_mean,i_notdef,i_land,i_class,'mm','mm' ,'mm')
    outvarid(o_phP(1)) = outvaridtype('phP1','pool humusP soil1',i_mean, i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_phP(2)) = outvaridtype('phP2','pool humusP soil2',i_mean, i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_phP(3)) = outvaridtype('phP3','pool humusP soil3',i_mean, i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_cocl) = outvaridtype('cOCl','comp load OC olake',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg org-C','')
    outvarid(o_psoilON(1)) = outvaridtype('pON1','pool orgN soil1',i_mean, i_notdef,i_land,i_class,'kg/km2' ,'kg N/km2','kg/km2')
    outvarid(o_psoilON(2)) = outvaridtype('pON2','pool orgN soil2',i_mean, i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_psoilON(3)) = outvaridtype('pON3','pool orgN soil3',i_mean, i_notdef,i_notdef,i_class,'kg/km2' ,'kg N/km2','')
    outvarid(o_sltmp(1)) = outvaridtype('stm1','computed soillayer 1 temp'  ,i_mean,i_notdef,i_land,i_class, 'deg' ,'degree Celcius','deg')
    outvarid(o_sltmp(2)) = outvaridtype('stm2','computed soillayer 2 temp'  ,i_mean,i_notdef,i_notdef,i_class, 'deg' ,'degree Celcius','')
    outvarid(o_sltmp(3)) = outvaridtype('stm3','computed soillayer 3 temp'  ,i_mean,i_notdef,i_notdef,i_class, 'deg' ,'degree Celcius','')
    outvarid(o_rainfall) = outvaridtype('cpRF','precipitation as rain'     ,i_sum , i_notdef,i_basin,i_class,'mm','mm' ,'mm')
    outvarid(o_snowfall) = outvaridtype('cpSF','precipitation as snow'     ,i_sum , i_notdef,i_basin,i_class,'mm','mm' ,'mm')
    outvarid(144) = outvaridtype('colv','volume of olake/w',i_mean , i_notdef,i_scale9,i_notdef,'M(m3)' ,'M(m3)' ,'mm') !Computed Lake Volume  (basins summed to outlet if any)
    outvarid(145) = outvaridtype('cilv','volume of ilake',i_mean , i_notdef,i_scale9,i_notdef,'M(m3)' ,'M(m3)' ,'mm') !Computed ILake Volume
    outvarid(146) = outvaridtype('clbv','volume of olake/lb',i_mean , i_notdef,i_scale9,i_notdef,'M(m3)' ,'M(m3)' ,'mm') !Computed Olake Volume Computed (volumes for individual basins if any)
    outvarid(o_ro3) = outvaridtype('cro3','computed runoff 3',i_sum  , i_notdef,i_notdef,i_class,'mm'   ,'mm' ,'')
    !Lake and river ice and snow depth variables
    outvarid(148) = outvaridtype('coli','comp olake ice depth'      ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(149) = outvaridtype('cili','comp ilake ice depth'      ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(150) = outvaridtype('colb','comp olake blackice depth' ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(151) = outvaridtype('cilb','comp ilake blackice depth' ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(152) = outvaridtype('cols','comp olake snow depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(153) = outvaridtype('cils','comp ilake snow depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(154) = outvaridtype('roli','rec. olake ice depth'      ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(155) = outvaridtype('rili','rec. ilake ice depth'      ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(156) = outvaridtype('rolb','rec. olake blackice depth' ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(157) = outvaridtype('rilb','rec. ilake blackice depth' ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(158) = outvaridtype('rols','rec. olake snow depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(159) = outvaridtype('rils','rec. ilake snow depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(160) = outvaridtype('cmri','comp main river ice depth'      ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(161) = outvaridtype('clri','comp local river ice depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(162) = outvaridtype('cmrb','comp main river blackice depth' ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(163) = outvaridtype('clrb','comp local river blackice depth',i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(164) = outvaridtype('cmrs','comp main river snow depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(165) = outvaridtype('clrs','comp local river snow depth'    ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(166) = outvaridtype('rmri','rec. main river ice depth'      ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(167) = outvaridtype('rlri','rec. local river ice depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(168) = outvaridtype('rmrb','rec. main river blackice depth' ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(169) = outvaridtype('rlrb','rec. local river blackice depth',i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(170) = outvaridtype('rmrs','rec. main river snow depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    outvarid(171) = outvaridtype('rlrs','rec. local river snow depth'    ,i_mean , i_notdef,i_notdef,i_notdef,'cm' ,'cm','')
    !Water surface temperatures
    outvarid(172) = outvaridtype('olst','comp olake surface temp'        ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(173) = outvaridtype('olut','comp olake upper temp'          ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(174) = outvaridtype('ollt','comp olake lower temp'          ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(175) = outvaridtype('olwt','comp olake mean temp'           ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(176) = outvaridtype('ilst','comp ilake surface temp'        ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(177) = outvaridtype('ilwt','comp ilake mean temp'           ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(178) = outvaridtype('lrst','comp local river surface temp'  ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(179) = outvaridtype('lrwt','comp local river meantemp'      ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(180) = outvaridtype('mrst','comp main river surface temp'   ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(181) = outvaridtype('mrwt','comp main river mean temp'      ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(182) = outvaridtype('rolt','rec. olake surface temp'         ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(183) = outvaridtype('rilt','rec. ilake surface temp'         ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(184) = outvaridtype('rmrt','rec. main river surface temp'    ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(185) = outvaridtype('mrto','OLD comp main river mean temp'   ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(186) = outvaridtype('lrto','OLD comp local river mean temp'  ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(187) = outvaridtype('ilto','OLD comp ilake mean temp'        ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(188) = outvaridtype('olto','OLD comp olake mean temp'        ,i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_snowcover) = outvaridtype('cfsc','comp frac snowcover area'       ,i_mean , i_notdef,i_land,i_class,'-'  ,'fraction','-')
    outvarid(190) = outvaridtype('rfsc','rec frac snowcover area'        ,i_mean , i_notdef,i_notdef,i_notdef,'-'  ,'fraction','')
    outvarid(o_snowmax) = outvaridtype('smax','comp snowmax in winter'   ,i_mean , i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(192) = outvaridtype('rfse','rec frac snowcover area error'  ,i_mean , i_notdef,i_notdef,i_notdef,'-'  ,'fraction','')
    outvarid(193) = outvaridtype('rfsm','rec frac snowcover multi'       ,i_mean , i_notdef,i_notdef,i_notdef,'-'  ,'fraction','')
    outvarid(194) = outvaridtype('rfme','rec frac snowcover multi error' ,i_mean, i_notdef,i_notdef,i_notdef,'-'  ,'fraction','')
    outvarid(o_landevap) = outvaridtype('levp','land evaporation'        ,i_sum, i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_soim12) = outvaridtype('som2','soil water upper 2 l',i_mean , i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_sml1) = outvaridtype('sml1','soil moisture lay 1' ,i_mean , i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_sml2) = outvaridtype('sml2','soil moisture lay 2' ,i_mean , i_notdef,i_notdef,i_class,'mm' ,'mm' ,'')
    outvarid(o_sml3) = outvaridtype('sml3','soil moisture lay 3' ,i_mean , i_notdef,i_notdef,i_class,'mm' ,'mm' ,'')
    outvarid(o_sml0) = outvaridtype('stsw','standing soil water' ,i_mean , i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_smrz) = outvaridtype('smrz','soil moisture root z',i_mean , i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_sm13) = outvaridtype('sm13','soil moisture sl 1-3',i_mean , i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(203) = outvaridtype('clCO','comp local conc OC',i_wmean, o_cloc,i_notdef,i_notdef,'mg/L' ,'mg org-C/L','')    !clOC occupied (cloc)
    outvarid(204) = outvaridtype('lrdp','local river depth'    ,i_mean , i_notdef,i_notdef,i_notdef,'m' ,'m','')
    outvarid(205) = outvaridtype('mrdp','main river depth'     ,i_mean , i_notdef,i_notdef,i_notdef,'m' ,'m','')
    outvarid(o_icloss) = outvaridtype('icpe','interception loss'    ,i_sum , i_notdef,i_basin,i_class,'mm' ,'mm' ,'mm')
    outvarid(207) = outvaridtype('rlIN','load rec IN com flow',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg Inorg-N','')
    outvarid(208) = outvaridtype('rlON','load rec ON com flow',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg Org-N','')
    outvarid(209) = outvaridtype('rlSP','load rec SP com flow',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg SRP-P','')
    outvarid(210) = outvaridtype('rlPP','load rec PP com flow',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg PartP-P','')
    outvarid(211) = outvaridtype('rlTN','load rec TN com flow',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg Tot-N','')
    outvarid(212) = outvaridtype('rlTP','load rec TP com flow',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg Tot-P','')
    outvarid(213) = outvaridtype('rlOC','load rec OC com flow',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg org-C','')
    outvarid(o_smffc)   = outvaridtype('srff','sm frac of field cap'     ,i_mean , i_notdef,i_land,i_class,'-' ,'-','-')
    outvarid(o_smfdep)  = outvaridtype('smfd','sm frac of depth'         ,i_mean , i_notdef,i_land,i_class,'-' ,'-','-')
    outvarid(o_smrzfdep) = outvaridtype('srfd','sm root frac of depth'   ,i_mean , i_notdef,i_land,i_class,'-' ,'-','-')
    outvarid(o_smfpw)   = outvaridtype('smfp','sm frac of pore v'        ,i_mean , i_notdef,i_land,i_class,'-' ,'-','-')
    outvarid(o_smrzfpw) = outvaridtype('srfp','sm root frac of pore v'   ,i_mean , i_notdef,i_land,i_class,'-' ,'-','-')
    outvarid(o_xobsm)   = outvaridtype('xom0','recorded mean var 0',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+1) = outvaridtype('xom1','recorded mean var 1',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+2) = outvaridtype('xom2','recorded mean var 2',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+3) = outvaridtype('xom3','recorded mean var 3',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+4) = outvaridtype('xom4','recorded mean var 4',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+5) = outvaridtype('xom5','recorded mean var 5',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+6) = outvaridtype('xom6','recorded mean var 6',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+7) = outvaridtype('xom7','recorded mean var 7',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+8) = outvaridtype('xom8','recorded mean var 8',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobsm+9) = outvaridtype('xom9','recorded mean var 9',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss)   = outvaridtype('xos0','recorded sum var 0',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+1) = outvaridtype('xos1','recorded sum var 1',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+2) = outvaridtype('xos2','recorded sum var 2',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+3) = outvaridtype('xos3','recorded sum var 3',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+4) = outvaridtype('xos4','recorded sum var 4',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+5) = outvaridtype('xos5','recorded sum var 5',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+6) = outvaridtype('xos6','recorded sum var 6',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+7) = outvaridtype('xos7','recorded sum var 7',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+8) = outvaridtype('xos8','recorded sum var 8',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_xobss+9) = outvaridtype('xos9','recorded sum var 9',i_sum  , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(240) = outvaridtype('aqin','aquifer recharge'   ,i_sum , i_notdef,i_scale3,i_notdef,'m3'  ,'m3' ,'mm')
    outvarid(241) = outvaridtype('aqut','aquifer return flow',i_sum , i_notdef,i_scale3,i_notdef,'m3'  ,'m3' ,'mm')
    outvarid(242) = outvaridtype('aqwl','aquifer water depth',i_mean, i_notdef,i_notdef,i_notdef,'m'   ,'m','')
    outvarid(o_specificq) = outvaridtype('speq','specific discharge',i_sum, i_notdef,i_notdef,i_notdef,'mm','mm' ,'')    !Changed name of variable upro
    outvarid(248) = outvaridtype('coic','comp olake ice cover',i_mean, i_notdef,i_notdef,i_notdef,'-','-','')
    outvarid(249) = outvaridtype('ciic','comp ilake ice cover',i_mean, i_notdef,i_notdef,i_notdef,'-','-','')
    outvarid(250) = outvaridtype('cmic','comp m river ice cov',i_mean, i_notdef,i_notdef,i_notdef,'-','-','')
    outvarid(251) = outvaridtype('clic','comp l river ice cov',i_mean, i_notdef,i_notdef,i_notdef,'-','-','')

    !Glacier outputs and corresponding WGMS observations, using the following 4-letter code structure:
    !  xGnn, where x  = r or c (recorded or computed) and nn = 2-letter code identifying the glacier variable
    outvarid(252) = outvaridtype('cgmb','comp. glacier mass balance',i_mean, i_notdef,i_notdef,i_notdef,'mm','mm' ,'')
    outvarid(253) = outvaridtype('rgmb','rec. glacier mass balance',i_mean, i_notdef,i_notdef,i_notdef,'mm','mm' ,'')
    outvarid(254) = outvaridtype('cgma','area used in com. mass balance',i_mean, i_notdef,i_notdef,i_notdef,'km2','km2','')
    outvarid(255) = outvaridtype('rgma','area used in rec. mass balance',i_mean, i_notdef,i_notdef,i_notdef,'km2','km2','')
    outvarid(256) = outvaridtype('rgmp','rec. mass balance period',i_mean, i_notdef,i_notdef,i_notdef,'days','days','')

    !Glacier Xobs variables not yet included in the code:
    ! ---------------------------------------------------
    !xGEL Equilibrium Line Altitude  ELA                	(m)
    !xGAA Accumulation Area Ratio of total area         	(%)
    !xGGA Area Survey year (GTC, geodetic thicness changes)	(km2)
    !xGAC Area Change (GTC)                                 (1000 m2)
    !xGTC Thickness Change (GTC)                            (mm)
    !xGVC Volume Change (GTC)                               (1000 m3)
    !xGSP GTC Survey period, days since Reference survey    (days)

    !Sovjet Snow Coarse observations in forest and open areas, and corresponding model variables:
    outvarid(257) = outvaridtype('S105','fsusnow fsc surr. open',i_mean, i_notdef,i_notdef,i_notdef,'0-10','0-10','')
    outvarid(258) = outvaridtype('S106','fsusnow fsc course open',i_mean, i_notdef,i_notdef,i_notdef,'0-10','0-10','')
    outvarid(259) = outvaridtype('S108','fsusnow mean depth open',i_mean, i_notdef,i_notdef,i_notdef,'cm','cm','')
    outvarid(260) = outvaridtype('S111','fsusnow mean density open',i_mean, i_notdef,i_notdef,i_notdef,'g/cm3','g/c8m3','')
    outvarid(261) = outvaridtype('S114','fsusnow snow water eq. open',i_mean, i_notdef,i_notdef,i_notdef,'mm','mm' ,'')
    outvarid(262) = outvaridtype('S205','fsusnow fsc surr. forest',i_mean, i_notdef,i_notdef,i_notdef,'0-10','0-10','')
    outvarid(263) = outvaridtype('S206','fsusnow fsc course forest',i_mean, i_notdef,i_notdef,i_notdef,'0-10','0-10','')
    outvarid(264) = outvaridtype('S208','fsusnow mean depth forest',i_mean, i_notdef,i_notdef,i_notdef,'cm','cm','')
    outvarid(265) = outvaridtype('S211','fsusnow mean density forest',i_mean, i_notdef,i_notdef,i_notdef,'g/cm3','g/cm3','')
    outvarid(266) = outvaridtype('S214','fsusnow snow water eq. forest ',i_mean, i_notdef,i_notdef,i_notdef,'mm','mm' ,'')
    outvarid(267) = outvaridtype('C106','comp. fsc course open',i_mean, i_notdef,i_notdef,i_notdef,'0-10','0-10','')
    outvarid(268) = outvaridtype('C108','comp. mean depth open',i_mean, i_notdef,i_notdef,i_notdef,'cm','cm','')
    outvarid(269) = outvaridtype('C111','comp. mean density open',i_mean, i_notdef,i_notdef,i_notdef,'g/cm3','g/cm3','')
    outvarid(270) = outvaridtype('C114','comp. snow water eq. open',i_mean, i_notdef,i_notdef,i_notdef,'mm','mm' ,'')
    outvarid(271) = outvaridtype('C206','comp. fsc course forest',i_mean, i_notdef,i_notdef,i_notdef,'0-10','0-10','')
    outvarid(272) = outvaridtype('C208','comp. mean depth forest',i_mean, i_notdef,i_notdef,i_notdef,'cm','cm' ,'')
    outvarid(273) = outvaridtype('C211','comp. mean density forest',i_mean, i_notdef,i_notdef,i_notdef,'g/cm3','g/cm3','')
    outvarid(274) = outvaridtype('C214','comp. snow water eq. forest ',i_mean, i_notdef,i_notdef,i_notdef,'mm','mm' ,'')
    outvarid(o_ros1) = outvaridtype('ros1','comp. sat surface rf'    ,i_sum ,i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_ros2) = outvaridtype('ros2','comp. excess infilt'    ,i_sum ,i_notdef,i_land,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_evapsnow) = outvaridtype('evsn','snow evaporation',i_sum , i_notdef,i_basin,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_evpt) = outvaridtype('evpt','total evaporation',i_sum , i_notdef,i_basin,i_class,'mm' ,'mm' ,'mm')
    outvarid(o_cleanwcom) = outvaridtype('clwc','cleaned wcom',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(o_cleanwstr) = outvaridtype('clws','cleaned wstr',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(o_wcav) = outvaridtype('wcav','ave. comp waterstage', i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(283) = outvaridtype('wtm0','comp water temp >0' , i_mean , i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_psim) = outvaridtype('psim','sim. precipitation', i_sum , i_notdef,i_basin,i_class,'mm','mm' ,'mm')

    !Soil load output variables; 1-12 soillayer 1-2, 13-24 soillayer 3, odd=gross load, even=net load, 25-36 soillayer3+tiledrain
    outvarid(285) = outvaridtype('sl07','soill 1-2 grs ld SP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(286) = outvaridtype('sl08','soill 1-2 net ld SP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(287) = outvaridtype('sl09','soill 1-2 grs ld PP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(288) = outvaridtype('sl10','soill 1-2 net ld PP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(289) = outvaridtype('sl11','soill 1-2 grs ld TP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(290) = outvaridtype('sl12','soill 1-2 net ld TP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(291) = outvaridtype('sl13','soill 3 grs ld IN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(292) = outvaridtype('sl14','soill 3 net ld IN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(293) = outvaridtype('sl15','soill 3 grs ld ON',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(294) = outvaridtype('sl16','soill 3 net ld ON',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(295) = outvaridtype('sl17','soill 3 grs ld TN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(296) = outvaridtype('sl18','soill 3 net ld TN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(297) = outvaridtype('sl19','soill 3 grs ld SP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(298) = outvaridtype('sl20','soill 3 net ld SP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(299) = outvaridtype('sl21','soill 3 grs ld PP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(300) = outvaridtype('sl22','soill 3 net ld PP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(301) = outvaridtype('sl23','soill 3 grs ld TP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(302) = outvaridtype('sl24','soill 3 net ld TP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(303) = outvaridtype('sl35','soill 3+t grs ld TP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(304) = outvaridtype('sl36','soill 3+t net ld TP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(305) = outvaridtype('sl01','soill 1-2 grs ld IN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(306) = outvaridtype('sl02','soill 1-2 net ld IN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(307) = outvaridtype('sl03','soill 1-2 grs ld ON',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(308) = outvaridtype('sl04','soill 1-2 net ld ON',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(309) = outvaridtype('sl05','soill 1-2 grs ld TN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(310) = outvaridtype('sl06','soill 1-2 net ld TN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(311) = outvaridtype('sl25','soill 3+t grs ld IN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(312) = outvaridtype('sl26','soill 3+t net ld IN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(313) = outvaridtype('sl27','soill 3+t grs ld ON',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(314) = outvaridtype('sl28','soill 3+t net ld ON',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(315) = outvaridtype('sl29','soill 3+t grs ld TN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(316) = outvaridtype('sl30','soill 3+t net ld TN',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(317) = outvaridtype('sl31','soill 3+t grs ld SP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(318) = outvaridtype('sl32','soill 3+t net ld SP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(319) = outvaridtype('sl33','soill 3+t grs ld PP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(320) = outvaridtype('sl34','soill 3+t net ld PP',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg','')
    outvarid(o_soilden3) = outvaridtype('den3','denitrification sl3',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg N','')
    outvarid(o_soildenrz) = outvaridtype('denz','denitrification sl12',i_sum , i_notdef,i_notdef,i_class,'kg' ,'kg N','')
    outvarid(323) = outvaridtype('mrfp','m river flpl depth' ,i_mean , i_notdef,i_notdef,i_notdef,'m'  ,'m','')
    outvarid(324) = outvaridtype('olfp','olake flpl depth'   ,i_mean , i_notdef,i_notdef,i_notdef,'m'  ,'m','')
    outvarid(325) = outvaridtype('mrfg','m river flpl degree',i_mean , i_notdef,i_notdef,i_notdef,'%'  ,'% of flpl area','')
    outvarid(326) = outvaridtype('olfg','olake flpl degree'  ,i_mean , i_notdef,i_notdef,i_notdef,'%'  ,'% of flpl area','')
    outvarid(o_csoillayerIN(1)) = outvaridtype('cIN1','conc IN soil layer 1' ,i_wmean, o_sml9,i_notdef,i_class,'ug/L','ug Inorg-N/L','')
    outvarid(o_csoillayerIN(2)) = outvaridtype('cIN2','conc IN soil layer 2' ,i_wmean, o_sml2,i_notdef,i_class,'ug/L','ug Inorg-N/L','')
    outvarid(o_csoillayerIN(3)) = outvaridtype('cIN3','conc IN soil layer 3' ,i_wmean, o_sml3,i_notdef,i_class,'ug/L','ug Inorg-N/L','')
    outvarid(o_sml9) = outvaridtype('sml9','soil water layer 1' ,i_mean, i_notdef,i_land,i_class,'mm','mm' ,'mm')
    outvarid(o_snowmelt) = outvaridtype('melt','snow melt' ,i_mean, i_notdef,i_land,i_class,'mm','mm' ,'mm')
    !T1 tracer output
    outvarid(o_ppT1(1)) = outvaridtype('aT11','pool adsorbed T1 soil1',i_mean , i_notdef,i_notdef,i_class,'?' ,'?','')
    outvarid(o_ppT1(2)) = outvaridtype('aT12','pool adsorbed T1 soil2',i_mean , i_notdef,i_notdef,i_class,'?' ,'?','')
    outvarid(o_ppT1(3)) = outvaridtype('aT13','pool adsorbed T1 soil3',i_mean , i_notdef,i_notdef,i_class,'?' ,'?','')
    outvarid(o_psoilT1(1)) = outvaridtype('sT11','pool T1 soilwater soil1',i_mean , i_notdef,i_notdef,i_class,'?' ,'?','')
    outvarid(o_psoilT1(2)) = outvaridtype('sT12','pool T1 soilwater soil2',i_mean , i_notdef,i_notdef,i_class,'?' ,'?','')
    outvarid(o_psoilT1(3)) = outvaridtype('sT13','pool T1 soilwater soil3',i_mean , i_notdef,i_notdef,i_class,'?' ,'?','')
    outvarid(o_tsmr) = outvaridtype('Tsmr','T1 pool main river sediments',i_mean , i_notdef,i_notdef,i_notdef,'?' ,'?','')
    outvarid(o_tslr) = outvaridtype('Tslr','T1 pool local river sediments',i_mean , i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_T1sf) = outvaridtype('T1sf','pool T1 soil surface',i_mean , i_notdef,i_notdef,i_class,'?' ,'?','')
    outvarid(342) = outvaridtype('clT1','comp local conc T1',i_wmean, o_cloc,i_notdef,i_notdef,'?' , '?','')
    outvarid(o_cro1T1) = outvaridtype('Tcr1','T1 in comp runoff 1',i_wmean, o_ro1,i_notdef,i_class,'?' , '?','')
    outvarid(o_cro2T1) = outvaridtype('Tcr2','T1 in comp runoff 2',i_wmean, o_ro2,i_notdef,i_class,'?' , '?','')
    outvarid(o_cro3T1) = outvaridtype('Tcr3','T1 in comp runoff 3',i_wmean, o_ro3,i_notdef,i_class,'?' , '?','')
    outvarid(o_crodT1) = outvaridtype('Tcrd','T1 in comp rf drain',i_wmean, o_rod,i_notdef,i_class,'?' , '?','')
    outvarid(o_crosT1) = outvaridtype('Tcrs','T1 in comp surf rf',i_wmean, o_ros,i_notdef,i_class,'?' , '?','')
    outvarid(o_crunSS) = outvaridtype('coSS','computed SS runoff',i_wmean, o_crun,i_notdef,i_class,'mg/L' ,'mg SuspSed/L','')
    outvarid(o_ccSS) = outvaridtype('ccSS','comp SS outflow olake',i_wmean, o_cout,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_reSS) = outvaridtype('reSS','rec SS outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'mg/L' ,'mg SuspSed/L','')
    outvarid(o_ccAE) = outvaridtype('ccAE','comp AE outflow olake',i_wmean, o_cout,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_reAE) = outvaridtype('reAE','rec AE outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'mg/L' ,'mg Algae-N/L','')
    outvarid(o_ccTS) = outvaridtype('ccTS','comp TS outflow olake',i_wmean, o_cout,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_dwtr) = outvaridtype('dwtr','demand water transfer',i_mean, i_notdef,i_notdef,i_notdef,'m3/s' ,'m3/s','')
    outvarid(o_rpwl) = outvaridtype('rpwl','m river flpl w level' ,i_mean , i_notdef,i_notdef,i_notdef,'m'  ,'m in FD-href','')
    outvarid(o_lpwl) = outvaridtype('lpwl','olake flpl w level'   ,i_mean , i_notdef,i_notdef,i_notdef,'m'  ,'m in FD-href','')
    outvarid(o_gmlt) = outvaridtype('gmlt','glacier melt'   ,i_mean , i_notdef,i_notdef,i_notdef,'mm'  ,'mm' ,'')
    outvarid(o_mrfa) = outvaridtype('mrfa','main river fractional area'   ,i_mean , i_notdef,i_notdef,i_notdef,'-'  ,'fraction of river area','')
    outvarid(o_lrfa) = outvaridtype('lrfa','local river fractional area'   ,i_mean , i_notdef,i_notdef,i_notdef,'-'  ,'fraction of river area','')
    outvarid(o_mred) = outvaridtype('mred','main river effective depth'   ,i_mean , i_notdef,i_notdef,i_notdef,'m'  ,'meter','')
    outvarid(o_lred) = outvaridtype('lred','local river effective depth'   ,i_mean , i_notdef,i_notdef,i_notdef,'m'  ,'meter','')

    outvarid(o_cinl) = outvaridtype('cINl','comp load IN olake',i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg Inorg-N','')
    outvarid(o_conl) = outvaridtype('cONl','comp load ON olake',i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg Org-N','')
    outvarid(o_cspl) = outvaridtype('cSPl','comp load SP olake',i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg SRP','')
    outvarid(o_cppl) = outvaridtype('cPPl','comp load PP olake',i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg Part-P','')
    outvarid(o_csSl) = outvaridtype('cSSl','comp load SS olake',i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg SuspSed','')
    outvarid(o_ctsl) = outvaridtype('cTSl','comp load TS olake',i_sum , i_notdef,i_notdef,i_notdef,'kg' ,'kg TotSuspSed','')
    outvarid(o_infi) = outvaridtype('infi','infiltration (+mp)',i_mean , i_notdef,i_land,i_class,'mm' ,'mm','mm')
    outvarid(o_cleanwavg) = outvaridtype('clwa','cleaned wavg',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(o_reTS) = outvaridtype('reTS','rec TS outflow olake',i_wmean2, o_rout,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_clss) = outvaridtype('clSS','comp local conc SS',i_wmean, o_cloc,i_notdef,i_notdef,'mg/L' ,'mg SuspSed/L','')
    outvarid(o_clts) = outvaridtype('clTS','comp local conc TS',i_wmean, o_cloc,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_rnlss) = outvaridtype('nlSS','routing net load SS',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg SuspSed','')
    outvarid(o_rnlts) = outvaridtype('nlTS','routing net load TS',i_sum, i_notdef,i_notdef,i_notdef,'kg' ,'kg TotSuspSed','')
    outvarid(o_wstilake) = outvaridtype('wilk','ilake water stage',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(o_isps) = outvaridtype('ispS','int.med. storage SS',i_mean , i_notdef,i_notdef,i_class,'kg/km2' ,'kg/km2','')
    outvarid(o_ispp) = outvaridtype('ispP','int.med. storage PP',i_mean , i_notdef,i_notdef,i_class,'kg/km2','kg P/km2','')
    outvarid(o_psmr) = outvaridtype('Psmr','PP pool main river sediments',i_mean , i_notdef,i_notdef,i_notdef,'kg' ,'kg P','')
    outvarid(o_pslr) = outvaridtype('Pslr','PP pool local river sediments',i_mean , i_notdef,i_notdef,i_notdef,'kg','kg P','')
    outvarid(o_ssmr) = outvaridtype('Ssmr','SS pool main river sediments',i_mean , i_notdef,i_notdef,i_notdef,'kg' ,'kg','')
    outvarid(o_sslr) = outvaridtype('Sslr','SS pool local river sediments',i_mean , i_notdef,i_notdef,i_notdef,'kg','kg','')
    outvarid(o_wstiwet) = outvaridtype('wiwt','iwet water stage',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(o_wstowet) = outvaridtype('wowt','owet water stage',i_mean , i_notdef,i_notdef,i_notdef,'m','meter','')
    outvarid(o_iwinfl) = outvaridtype('iwin','comp iwet inflow',   i_mean , i_notdef,i_notdef,i_notdef, 'm3/s','m3/s','')
    outvarid(o_iwoutfl) = outvaridtype('iwut','comp iwet outflow',   i_mean , i_notdef,i_notdef,i_notdef, 'm3/s','m3/s','')
    outvarid(o_iwutcoin) = outvaridtype('wcIN','comp iwet conc IN outflow',i_wmean, o_iwoutfl,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_iwutcoon) = outvaridtype('wcON','comp iwet conc ON outflow',i_wmean, o_iwoutfl,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_iwutcosp) = outvaridtype('wcSP','comp iwet conc SP outflow',i_wmean, o_iwoutfl,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_iwutcopp) = outvaridtype('wcPP','comp iwet conc PP outflow',i_wmean, o_iwoutfl,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_iwutcoss) = outvaridtype('wcSS','comp iwet conc SS outflow',i_wmean, o_iwoutfl,i_notdef,i_notdef,'mg/L' ,'mg SuspSed/L','')
    outvarid(o_iwutcoae) = outvaridtype('wcAE','comp iwet conc AE outflow',i_wmean, o_iwoutfl,i_notdef,i_notdef,'mg/L' ,'mg Algae-N/L','')
    outvarid(o_iwutcooc) = outvaridtype('wcOC','comp iwet conc OC outflow',i_wmean, o_iwoutfl,i_notdef,i_notdef,'mg/L' ,'mg Org-C/L','')
    outvarid(o_iwutcot2) = outvaridtype('wcT2','comp iwet conc T2 outflow',i_wmean, o_iwoutfl,i_notdef,i_notdef,'deg' ,'degree Celsius','')
    outvarid(o_iwincoin) = outvaridtype('wiIN','comp iwet conc IN inflow',i_wmean, o_iwinfl,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_iwincoon) = outvaridtype('wiON','comp iwet conc ON inflow',i_wmean, o_iwinfl,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_iwincosp) = outvaridtype('wiSP','comp iwet conc SP inflow',i_wmean, o_iwinfl,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_iwincopp) = outvaridtype('wiPP','comp iwet conc PP inflow',i_wmean, o_iwinfl,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_iwincoss) = outvaridtype('wiSS','comp iwet conc SS inflow',i_wmean, o_iwinfl,i_notdef,i_notdef,'mg/L' ,'mg SuspSed/L','')
    outvarid(o_iwincoae) = outvaridtype('wiAE','comp iwet conc AE inflow',i_wmean, o_iwinfl,i_notdef,i_notdef,'mg/L' ,'mg Algae-N/L','')
    outvarid(o_iwincooc) = outvaridtype('wiOC','comp iwet conc OC inflow',i_wmean, o_iwinfl,i_notdef,i_notdef,'mg/L' ,'mg Org-C/L','')
    outvarid(o_iwincot2) = outvaridtype('wiT2','comp iwet conc T2 inflow',i_wmean, o_iwinfl,i_notdef,i_notdef,'deg' ,'degree Celsius','')
    outvarid(o_iwetvol) = outvaridtype('ciwv','comp iwet water volume',i_mean, i_notdef,i_notdef,i_notdef,'m3' ,'m3','')
    outvarid(o_owetvol) = outvaridtype('cowv','comp owet water volume',i_mean, i_notdef,i_notdef,i_notdef,'m3' ,'m3','')

    !DG20200128: Additional outvars for data assimilation 'observation operators' to derive the 'model equivalents'
    outvarid(o_clrf) = outvaridtype('clrf','calculated local runoff', i_sum, i_notdef,i_notdef,i_notdef,'mm' ,'mm' ,'') !normalized from cloc (m3/s) by local basin area (excluding main river and olake)
    outvarid(o_clrp) = outvaridtype('clrp','calculated prec on river and lake', i_sum, i_notdef,i_notdef,i_notdef,'mm' ,'mm' ,'')
    outvarid(o_clre) = outvaridtype('clre','calculated evap on river and lake', i_sum, i_notdef,i_notdef,i_notdef,'mm' ,'mm' ,'')
    outvarid(o_qcin) = outvaridtype('qcin','calculated (regional) lake inflow',i_mean, i_notdef,i_notdef,i_notdef,'m3/s','m3/s' ,'')
    outvarid(o_qrin) = outvaridtype('qrin','recorded (regional) lake inflow',i_mean, i_notdef,i_notdef,i_notdef,'m3/s','m3/s' ,'')

    outvarid(o_hged) = outvaridtype('hged','river depth (h geom)'     ,i_mean, i_notdef,i_notdef,i_notdef,'m' ,'m','')
    outvarid(408) = outvaridtype('corl','river water level'     ,i_mean, i_notdef,i_notdef,i_notdef,'m' ,'m','')
    outvarid(409) = outvaridtype('hgeu','river velo. (h geom)'     ,i_mean, i_notdef,i_notdef,i_notdef,'m/s' ,'m/s','')
    outvarid(410) = outvaridtype('rerl','rec river water level'    ,i_mean, i_notdef,i_notdef,i_notdef,'m' ,'m','')

    !Snow heat content and snow temperatures
    outvarid(o_snht) = outvaridtype('snht','snow heat content'   ,i_mean , i_notdef,i_notdef,i_class,'MJ'  ,'megajoule','')
    outvarid(o_snte) = outvaridtype('snte','snow temperature'   ,i_mean , i_notdef,i_notdef,i_class,'deg'  ,'degree Celsius','')
    outvarid(o_snts) = outvaridtype('snts','snow surface temperature'   ,i_mean , i_notdef,i_notdef,i_class,'deg'  ,'degree Celsius','')
    outvarid(o_dtmp) = outvaridtype('dtmp','deep soil temp'   ,i_mean , i_notdef,i_notdef,i_class,'deg'  ,'degree Celsius','')
    outvarid(o_cmrp) = outvaridtype('cmrp','mriver ice porosity'   ,i_mean , i_notdef,i_notdef,i_notdef,'-','volfraction','')
    outvarid(o_colp) = outvaridtype('colp','olake ice porosity'   ,i_mean , i_notdef,i_notdef,i_notdef,'-','volfraction','')

    !Snow liquid water content
    outvarid(o_snwc) = outvaridtype('snwc','snow liq water content'   ,i_mean , i_notdef,i_notdef,i_class,'mm'  ,'mm','')

    !Concentration of flows and lakes
    outvarid(o_c1IN) = outvaridtype('c1IN','comp IN main flow',i_wmean, o_coum,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_c1ON) = outvaridtype('c1ON','comp ON main flow',i_wmean, o_coum,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_c1SP) = outvaridtype('c1SP','comp SP main flow',i_wmean, o_coum,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_c1PP) = outvaridtype('c1PP','comp PP main flow',i_wmean, o_coum,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_c1T1) = outvaridtype('c1T1','comp T1 main flow',i_wmean, o_coum,i_notdef,i_notdef,'?','?','')
    outvarid(o_c1T2) = outvaridtype('c1T2','comp T2 main flow',i_wmean, o_coum,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_c1TN) = outvaridtype('c1TN','comp TN main flow',i_wmean, o_coum,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_c1TP) = outvaridtype('c1TP','comp TP main flow',i_wmean, o_coum,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_c1SS) = outvaridtype('c1SS','comp SS main flow',i_wmean, o_coum,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_c1AE) = outvaridtype('c1AE','comp AE main flow',i_wmean, o_coum,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_c1TS) = outvaridtype('c1TS','comp TS main flow',i_wmean, o_coum,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_c2IN) = outvaridtype('c2IN','comp IN branch flow',i_wmean, o_coub,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_c2ON) = outvaridtype('c2ON','comp ON branch flow',i_wmean, o_coub,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_c2SP) = outvaridtype('c2SP','comp SP branch flow',i_wmean, o_coub,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_c2PP) = outvaridtype('c2PP','comp PP branch flow',i_wmean, o_coub,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_c2T1) = outvaridtype('c2T1','comp T1 branch flow',i_wmean, o_coub,i_notdef,i_notdef,'?','?','')
    outvarid(o_c2T2) = outvaridtype('c2T2','comp T2 branch flow',i_wmean, o_coub,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_c2TN) = outvaridtype('c2TN','comp TN branch flow',i_wmean, o_coub,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_c2TP) = outvaridtype('c2TP','comp TP branch flow',i_wmean, o_coub,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_c2SS) = outvaridtype('c2SS','comp SS branch flow',i_wmean, o_coub,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_c2AE) = outvaridtype('c2AE','comp AE branch flow',i_wmean, o_coub,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_c2TS) = outvaridtype('c2TS','comp TS branch flow',i_wmean, o_coub,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_c3IN) = outvaridtype('c3IN','comp IN main flow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_c3ON) = outvaridtype('c3ON','comp ON main flow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_c3SP) = outvaridtype('c3SP','comp SP main flow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_c3PP) = outvaridtype('c3PP','comp PP main flow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_c3T1) = outvaridtype('c3T1','comp T1 main flow',i_mean, i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_c3T2) = outvaridtype('c3T2','comp T2 main flow',i_mean, i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_c3TN) = outvaridtype('c3TN','comp TN main flow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_c3TP) = outvaridtype('c3TP','comp TP main flow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_c3SS) = outvaridtype('c3SS','comp SS main flow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_c3AE) = outvaridtype('c3AE','comp AE main flow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_c3TS) = outvaridtype('c3TS','comp TS main flow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_c4IN) = outvaridtype('c4IN','comp IN branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_c4ON) = outvaridtype('c4ON','comp ON branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_c4SP) = outvaridtype('c4SP','comp SP branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_c4PP) = outvaridtype('c4PP','comp PP branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_c4T1) = outvaridtype('c4T1','comp T1 branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_c4T2) = outvaridtype('c4T2','comp T2 branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_c4TN) = outvaridtype('c4TN','comp TN branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_c4TP) = outvaridtype('c4TP','comp TP branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_c4SS) = outvaridtype('c4SS','comp SS branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_c4AE) = outvaridtype('c4AE','comp AE branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_c4TS) = outvaridtype('c4TS','comp TS branch flow',i_wmean, i_notdef,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_c5IN) = outvaridtype('c5IN','comp IN in ilake',i_wmean, 145,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_c5ON) = outvaridtype('c5ON','comp ON in ilake',i_wmean, 145,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_c5SP) = outvaridtype('c5SP','comp SP in ilake',i_wmean, 145,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_c5PP) = outvaridtype('c5PP','comp PP in ilake',i_wmean, 145,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_c5T1) = outvaridtype('c5T1','comp T1 in ilake',i_wmean, 145,i_notdef,i_notdef,'?','?','')
    outvarid(o_c5T2) = outvaridtype('c5T2','comp T2 in ilake',i_wmean, 145,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_c5TN) = outvaridtype('c5TN','comp TN in ilake',i_wmean, 145,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_c5TP) = outvaridtype('c5TP','comp TP in ilake',i_wmean, 145,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_c5SS) = outvaridtype('c5SS','comp SS in ilake',i_wmean, 145,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_c5AE) = outvaridtype('c5AE','comp AE in ilake',i_wmean, 145,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_c5TS) = outvaridtype('c5TS','comp TS in ilake',i_wmean, 145,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_c6IN) = outvaridtype('c6IN','comp IN in olake',i_wmean, 146,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_c6ON) = outvaridtype('c6ON','comp ON in olake',i_wmean, 146,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_c6SP) = outvaridtype('c6SP','comp SP in olake',i_wmean, 146,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_c6PP) = outvaridtype('c6PP','comp PP in olake',i_wmean, 146,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_c6T1) = outvaridtype('c6T1','comp T1 in olake',i_wmean, 146,i_notdef,i_notdef,'?','?','')
    outvarid(o_c6T2) = outvaridtype('c6T2','comp T2 in olake',i_wmean, 146,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_c6TN) = outvaridtype('c6TN','comp TN in olake',i_wmean, 146,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_c6TP) = outvaridtype('c6TP','comp TP in olake',i_wmean, 146,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_c6SS) = outvaridtype('c6SS','comp SS in olake',i_wmean, 146,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_c6AE) = outvaridtype('c6AE','comp AE in olake',i_wmean, 146,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_c6TS) = outvaridtype('c6TS','comp TS in olake',i_wmean, 146,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_c1OC) = outvaridtype('c1OC','comp OC main flow',i_wmean, o_coum,i_notdef,i_notdef,'mg/L' ,'mg Org-C/L','')
    outvarid(o_c2OC) = outvaridtype('c2OC','comp OC branch flow',i_wmean, o_coub,i_notdef,i_notdef,'mg/L','mg Org-C/L','')
    outvarid(o_c3OC) = outvaridtype('c3OC','comp OC main flow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L' ,'mg Org-C/L','')
    outvarid(o_c4OC) = outvaridtype('c4OC','comp OC branch flow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L','mg Org-C/L','')
    outvarid(o_c5OC) = outvaridtype('c5OC','comp OC in ilake',i_wmean, 145,i_notdef,i_notdef,'mg/L' ,'mg Org-C/L','')
    outvarid(o_c6OC) = outvaridtype('c6OC','comp OC in olake',i_wmean, 146,i_notdef,i_notdef,'mg/L','mg Org-C/L','')
    outvarid(o_crgl) = outvaridtype('crgl','calculated global radiation'   ,i_mean , i_notdef,i_basin,i_class,'MJ/d'  ,'MJ/d','MJ/d')
    outvarid(o_crnt) = outvaridtype('crnt','calculated net radiation'   ,i_mean , i_notdef,i_basin,i_class,'MJ/d'  ,'MJ/d','MJ/d')
    outvarid(o_cmrr) = outvaridtype('cmrr','calc main river radiation'   ,i_mean , i_notdef,i_notdef,i_notdef,'MJ/d'  ,'MJ/d','')
    outvarid(o_crpt) = outvaridtype('crpt','calc potential sw radiation'   ,i_mean , i_notdef,i_basin,i_class,'MJ/d'  ,'MJ/d','MJ/d')
    outvarid(o_crex) = outvaridtype('crex','extraterrestrial radiation'   ,i_mean , i_notdef,i_basin,i_notdef,'MJ/d'  ,'MJ/d','MJ/d')
    outvarid(o_fnca) = outvaridtype('fnca','fraction of non-contributing area',i_mean , 0,0,0,'-','-' ,'')
    outvarid(o_fcon) = outvaridtype('fcon','fraction of (ilake) connectivity',i_mean , 0,0,0,'-','-' ,'')

    outvarid(501) = outvaridtype('acIN','aquifer IN conc',i_mean, 0,0,0,'mg/L' ,'ug InorgN-N/L','')
    outvarid(502) = outvaridtype('acON','aquifer ON conc',i_mean, 0,0,0,'mg/L' ,'ug OrgN-N/L','')
    outvarid(503) = outvaridtype('acSP','aquifer SP conc',i_mean, 0,0,0,'mg/L' ,'ug SRP-P/L','')
    outvarid(504) = outvaridtype('acPP','aquifer PP conc',i_mean, 0,0,0,'mg/L' ,'ug Part-P/L','')
    outvarid(505) = outvaridtype('acSS','aquifer SS conc',i_mean, 0,0,0,'mg/L' ,'mg SuspSed/L','')
    outvarid(506) = outvaridtype('acT1','aquifer T1 conc',i_mean, 0,0,0,'?' ,'?','')
    outvarid(507) = outvaridtype('acOC','aquifer OC conc',i_mean, 0,0,0,'mg/L' ,'mg OrgC/L','')

    outvarid(o_c7IN) = outvaridtype('c7IN','comp IN lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_c7ON) = outvaridtype('c7ON','comp ON lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_c7SP) = outvaridtype('c7SP','comp SP lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_c7PP) = outvaridtype('c7PP','comp PP lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_c7OC) = outvaridtype('c7OC','comp OC lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'mg/L' ,'mg Org-C/L','')
    outvarid(o_c7T1) = outvaridtype('c7T1','comp T1 lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'?','?','')
    outvarid(o_c7T2) = outvaridtype('c7T2','comp T2 lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_c7TN) = outvaridtype('c7TN','comp TN lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_c7TP) = outvaridtype('c7TP','comp TP lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_c7SS) = outvaridtype('c7SS','comp SS lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_c7AE) = outvaridtype('c7AE','comp AE lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_c7TS) = outvaridtype('c7TS','comp TS lake outflow',i_wmean, o_coul,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    outvarid(o_c8IN) = outvaridtype('c8IN','comp IN lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L','ug InorgN-N/L','')
    outvarid(o_c8ON) = outvaridtype('c8ON','comp ON lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L','ug OrgN-N/L','')
    outvarid(o_c8SP) = outvaridtype('c8SP','comp SP lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L','ug SRP-P/L','')
    outvarid(o_c8PP) = outvaridtype('c8PP','comp PP lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L','ug PartP-P/L','')
    outvarid(o_c8OC) = outvaridtype('c8OC','comp OC lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L' ,'mg Org-C/L','')
    outvarid(o_c8T1) = outvaridtype('c8T1','comp T1 lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'?','?','')
    outvarid(o_c8T2) = outvaridtype('c8T2','comp T2 lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'deg'  ,'degree Celsius','')
    outvarid(o_c8TN) = outvaridtype('c8TN','comp TN lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L' ,'ug Tot-N/L','')
    outvarid(o_c8TP) = outvaridtype('c8TP','comp TP lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'ug/L' ,'ug Tot-P/L','')
    outvarid(o_c8SS) = outvaridtype('c8SS','comp SS lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L','mg SuspSed/L','')
    outvarid(o_c8AE) = outvaridtype('c8AE','comp AE lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L' ,'mg Algea-N/L','')
    outvarid(o_c8TS) = outvaridtype('c8TS','comp TS lake outflow',i_mean, i_notdef,i_notdef,i_notdef,'mg/L' ,'mg TotSuspSed/L','')
    ! for shape project
    !outvarid(o_rmwl) = outvaridtype('rmwl','recorded main river water level',i_mean , 0,0,'m','m' ,'mapRMWL.txt','timeRMWL.txt')
    !outvarid(o_rmwd) = outvaridtype('rmwd','recorded main river water flow',i_mean , 0,0,'m3/s','m3/s' ,'mapRMWD.txt','timeRMWD.txt')
    !outvarid(o_cmwl) = outvaridtype('cmwl','calculated main river water level',i_mean , 0,0,'m','m' ,'mapCMWL.txt','timeCMWL.txt')
    !outvarid(o_amwl) = outvaridtype('amwl','altimetry main river water level',i_mean , 0,0,'m','m' ,'mapAMWL.txt','timeAMWL.txt')

    ! for TEP project
    !outvarid(o_aowl) = outvaridtype('aowl','altimetry olake water level',i_mean , 0,0,'m','m' ,'mapAOWL.txt','timeAOWL.txt')
    !outvarid(o_aiwl) = outvaridtype('aiwl','altimetry ilake water level',i_mean , 0,0,'m','m' ,'mapAIWL.txt','timeAIWL.txt')
    !outvarid(o_cowl) = outvaridtype('cowl','calculated olake water level',i_mean , 0,0,'m','m' ,'mapCOWL.txt','timeCOWL.txt')
    !outvarid(o_ciwl) = outvaridtype('ciwl','calculated ilake water level',i_mean , 0,0,'m','m' ,'mapCIWL.txt','timeCIWL.txt')

    ! to be made:
    !  - AMSR2 radiances in different frequencies (CMEM, FASTEM, etc)
    !  - In-situ snow observations (or by additional "station-subbasins" representing the local conditions of the snow observation as good as possible with the model)
    !  - Snow survey observations (mean, variability, etc)
    !  - whatever exciting observation we may come up with...


    !Max length: 1st==4, 2nd=20, 3rd=Int, 4th=Int, 5th=Int, 6th=Int, 7th=6, 8th=20, 9th=6
    !3rd:i_mean=average per timestep, i_sum=sum over mean period (or per year for whole sim period),i_wmean,i_wmean2=volume weighted with water in 4th column
    !4th:i_notdef=0, O_.. for corresponding water of concentration
    !5th:i_notdef, i_basin (represent subbasin), i_land (represent landarea), i_scaleNN (scale with different factors)
    !6th:i_notdef=0, i_class=variable calculated per class

    !Set variable exchange for criterion calculation (outvarid-index)
    ALLOCATE(changecritvar(2))
    changecritvar(1)%rec = (/o_rewstr,o_cleanwstr/)
    changecritvar(1)%comp = (/o_wcom,o_cleanwcom/)
    changecritvar(2)%rec = (/o_rewstr,o_cleanwstr/)
    changecritvar(2)%comp = (/o_wcav,o_cleanwavg/)

    !Set variable for finding outvar for special variables used for regional obsfunc calculations
    ALLOCATE(obsfuncoutvar(4))
    obsfuncoutvar(1:4) = (/o_clrf, o_clrp, o_clre, o_qcin/)

    !Set variable for water stage observations updating
    wobsvarindex = o_rewstr

    !Preparation of heading for load files
    loadheadings=['subid ','WetAtm','DryAtm','Fertil','PDecay','RuralA','GrwSIn','IrrSrc','Runoff','RuralB','Urban1','Urban2','Urban3','Rgrwmr','Wtrans','Rgrwol','A     ','B     ','C     ','D     ','E     ','F     ','G     ','H     ','I     ','J     ','K     ','L     ','MA    ','M     ','N     ','O     ','P     ','Q     ','R     ','S     ']

  END SUBROUTINE define_output_variables

  !>Set variable holding model parameter information of HYPE model
  !------------------------------------------------------------------------------------------
  SUBROUTINE define_model_parameters()
  !>Note that only small letters allowed in parameter names
  !>modparid structure: name, dependence type, index (defined in hypevar)

    max_par = n_max_par          !maximum number of parameters
    ALLOCATE(modparid(max_par))
    modparid = modparidtype('',0,0) !default initialisation

    modparid(n_lp)= modparidtype('lp        ', m_gpar , m_lp)
    modparid(n_cevpa) = modparidtype('cevpam    ', m_gpar , m_cevpam)
    modparid(n_cevpp) = modparidtype('cevpph    ', m_gpar , m_cevpph)
    modparid(4)   = modparidtype('deadl     ', m_gpar , m_deadl)
    modparid(n_fastn0)   = modparidtype('fastn0    ', m_gpar , m_fastN0)
    modparid(n_fastp0)   = modparidtype('fastp0    ', m_gpar , m_fastP0)
    modparid(7)   = modparidtype('init1     ', m_gpar , m_iniT1)
    modparid(n_iniT2)   = modparidtype('init2     ', m_gpar , m_iniT2)
    modparid(n_T1evap)   = modparidtype('t1evap    ', m_gpar , m_T1evap)
    modparid(n_rivv)  = modparidtype('rivvel    ', m_gpar , m_rivvel)
    modparid(n_damp)  = modparidtype('damp      ', m_gpar , m_damp)
    modparid(n_tcalt)  = modparidtype('tcalt     ', m_gpar , m_tcalt)
    modparid(13)  = modparidtype('gratk     ', m_gpar , m_grat1)
    modparid(14)  = modparidtype('gratp     ', m_gpar , m_grat2)
    modparid(n_denitrlu)  = modparidtype('denitrlu  ', m_lpar , m_denitrlu)
    modparid(16)  = modparidtype('denitwrm  ', m_gpar , m_denitwr)
    modparid(17)  = modparidtype('maxwidth  ', m_gpar , m_maxwidth)
    modparid(n_dissolfn)  = modparidtype('dissolfn  ', m_lpar , m_dissolfN)
    modparid(n_dissolfp)  = modparidtype('dissolfp  ', m_lpar , m_dissolfP)
    modparid(20)  = modparidtype('litterdays', m_gpar , m_littdays)
    modparid(n_tcea)  = modparidtype('tcelevadd ', m_gpar , m_tcelevadd)
    modparid(n_pcem)  = modparidtype('pcelevmax ', m_gpar , m_pcelevmax)
    modparid(n_tcorr)  = modparidtype('tempcorr  ', m_rpar , m_tcadd)
    modparid(n_pcea)  = modparidtype('pcelevadd ', m_gpar , m_pcelevadd)
    !modparid(25)  = modparidtype('fastlake  ', m_gpar , m_fastlake)  !free
    modparid(n_epotdist)  = modparidtype('epotdist  ', m_gpar , m_epotdist)
    modparid(27)  = modparidtype('qmean     ', m_gpar , m_qmean)
    modparid(28)  = modparidtype('pcaddg    ', m_gpar , m_pcaddg)
    modparid(n_pcet)  = modparidtype('pcelevth  ', m_gpar , m_pcelevth)
    modparid(30)  = modparidtype('tpmean    ', m_rpar, m_tpmean)
    modparid(n_ttmp)  = modparidtype('ttmp      ', m_lpar , m_ttmp)
    modparid(n_cmlt)  = modparidtype('cmlt      ', m_lpar , m_cmlt)
    modparid(n_cevp)  = modparidtype('cevp      ', m_lpar , m_cevp)
    modparid(34)  = modparidtype('frost     ', m_lpar , m_cfrost)
    modparid(n_srrcs) = modparidtype('srrcs     ', m_lpar , m_srrcs)
    modparid(n_ttpd)  = modparidtype('ttpd      ', m_gpar , m_ttpd)
    modparid(n_ttpi)  = modparidtype('ttpi      ', m_gpar , m_ttpi)
    modparid(n_dissolhn) = modparidtype('dissolhn  ', m_lpar , m_dissolhN)
    modparid(n_dissolhp) = modparidtype('dissolhp  ', m_lpar , m_dissolhP)
    modparid(n_humusn0)  = modparidtype('humusn0   ', m_lpar , m_humusN0)
    modparid(n_partp0)   = modparidtype('partp0    ', m_lpar , m_partP0)
    modparid(n_wsfluse)  = modparidtype('wsfluse   ', m_lpar , m_wsfluse)
    modparid(n_wsfscale) = modparidtype('wsfscale  ', m_gpar , m_wsfscale)
    modparid(n_wsfbias)  = modparidtype('wsfbias   ', m_gpar , m_wsfbias)
    modparid(n_sfdmax)   = modparidtype('sfdmax    ', m_gpar , m_sfdmax)
    modparid(n_numdir)   = modparidtype('numdir    ', m_gpar , m_numdir)
    modparid(n_wcfc)  = modparidtype('wcfc      ', m_spar , m_wcfc)
    modparid(n_wcwp)  = modparidtype('wcwp      ', m_spar , m_wcwp)
    modparid(n_wcep)  = modparidtype('wcep      ', m_spar , m_wcep)
    modparid(n_wcfc1)  = modparidtype('wcfc1     ', m_spar , m_wcfc1)
    modparid(n_wcwp1)  = modparidtype('wcwp1     ', m_spar , m_wcwp1)
    modparid(n_wcep1)  = modparidtype('wcep1     ', m_spar , m_wcep1)
    modparid(n_wcfc2)  = modparidtype('wcfc2     ', m_spar , m_wcfc2)
    modparid(n_wcwp2)  = modparidtype('wcwp2     ', m_spar , m_wcwp2)
    modparid(n_wcep2)  = modparidtype('wcep2     ', m_spar , m_wcep2)
    modparid(n_wcfc3)  = modparidtype('wcfc3     ', m_spar , m_wcfc3)
    modparid(n_wcwp3)  = modparidtype('wcwp3     ', m_spar , m_wcwp3)
    modparid(n_wcep3)  = modparidtype('wcep3     ', m_spar , m_wcep3)
    modparid(59)  = modparidtype('gldepi    ', m_gpar , m_gldepi)
    modparid(60)  = modparidtype('trrcs     ', m_spar , m_trrcs)
    modparid(61)  = modparidtype('mperc1    ', m_spar , m_perc1)
    modparid(62)  = modparidtype('mperc2    ', m_spar , m_perc2)
    modparid(65)  = modparidtype('sswcorr   ', m_gpar , m_sswcorr)
    modparid(n_depthrel)  = modparidtype('depthrel  ', m_lpar , m_depthrel)
    modparid(67)  = modparidtype('regirr    ', m_gpar , m_regirr)
    modparid(68)  = modparidtype('immdepth  ', m_gpar , m_immdep)
    modparid(69)  = modparidtype('iwdfrac   ', m_gpar , m_iwdfrac)
    modparid(70)  = modparidtype('irrdemand ', m_gpar , m_wdpar)
    modparid(n_minerfn)  = modparidtype('minerfn   ', m_lpar , m_minerfN)
    modparid(n_minerfp)  = modparidtype('minerfp   ', m_lpar , m_minerfP)
    modparid(n_degradhn)  = modparidtype('degradhn  ', m_lpar , m_degradhN)
    modparid(n_deepmem) = modparidtype('deepmem   ', m_gpar , m_deepmem)
    modparid(n_surfmem) = modparidtype('surfmem   ', m_lpar , m_surfmem)
    modparid(n_freuc)  = modparidtype('freuc     ', m_spar , m_freuc)
    modparid(n_freuexp)  = modparidtype('freuexp   ', m_spar , m_freuexp)
    modparid(n_freurate)  = modparidtype('freurate  ', m_spar , m_freurate)
    modparid(80)  = modparidtype('wprodn    ', m_gpar , m_wprodn)
    modparid(81)  = modparidtype('sedon     ', m_gpar , m_sedon)
    modparid(82)  = modparidtype('sedpp     ', m_gpar , m_sedpp)
    modparid(83)  = modparidtype('sedexp    ', m_gpar , m_sedexp)
    modparid(n_rcgrw)  = modparidtype('rcgrw     ', m_gpar , m_rcgrw)
    modparid(85)  = modparidtype('rrcs1     ', m_spar , m_rrcs1)
    modparid(86)  = modparidtype('rrcs2     ', m_spar , m_rrcs2)
    modparid(n_rrcs3)  = modparidtype('rrcs3     ', m_gpar , m_rrcs3)
    modparid(n_hnhalf)  = modparidtype('hnhalf    ', m_lpar , m_hNhalf)
    modparid(n_pphalf)  = modparidtype('pphalf    ', m_lpar , m_pPhalf)
    modparid(90)  = modparidtype('locsoil   ', m_gpar , m_locsoil)
    modparid(n_filtPbuf)  = modparidtype('bufffilt  ', m_lpar , m_filtPbuf)
    modparid(n_filtPinner)  = modparidtype('innerfilt ', m_lpar , m_filtPinner)
    modparid(n_filtPother)  = modparidtype('otherfilt ', m_lpar , m_filtPother)
    modparid(n_drypp)  = modparidtype('drydeppp  ', m_lpar , m_drypp)
    modparid(n_wetsp)  = modparidtype('wetdepsp  ', m_gpar , m_wetsp)
    modparid(96)  = modparidtype('rivvel1   ', m_rpar, m_velpar1)
    modparid(97)  = modparidtype('rivvel2   ', m_rpar, m_velpar2)
    modparid(98)  = modparidtype('rivvel3   ', m_rpar, m_velpar3)
    modparid(99)  = modparidtype('rivwidth1 ', m_rpar, m_widpar1)
    modparid(100) = modparidtype('rivwidth2 ', m_rpar, m_widpar2)
    modparid(101) = modparidtype('rivwidth3 ', m_rpar, m_widpar3)
    modparid(n_srrate) = modparidtype('srrate    ', m_spar , m_srrate)
    modparid(n_macrate) = modparidtype('macrate   ', m_spar , m_macrate)
    modparid(n_mactrinf) = modparidtype('mactrinf  ', m_spar , m_mactrinf)
    modparid(n_mactrsm) = modparidtype('mactrsm   ', m_spar , m_mactrsm)
    modparid(n_soilcoh) = modparidtype('soilcoh   ', m_spar , m_soilcoh)
    modparid(n_soilerod) = modparidtype('soilerod  ', m_spar , m_soilerod)
    modparid(108) = modparidtype('wprodp    ', m_gpar , m_wprodp)
    modparid(n_sreroexp) = modparidtype('sreroexp  ', m_gpar , m_sreroexp)
    modparid(n_dsndens) = modparidtype('snowdensdt', m_gpar , m_dsndens)
    modparid(111) = modparidtype('sfrost    ', m_spar , m_sfrost)
    modparid(n_macfilt) = modparidtype('macrofilt ', m_spar , m_macfilt)
    modparid(n_fertdays) = modparidtype('fertdays  ', m_gpar , m_fertdays)
    modparid(n_humusp0) = modparidtype('humusp0   ', m_lpar , m_humusP0)
    modparid(n_hphalf) = modparidtype('hphalf    ', m_lpar , m_hPhalf)
    modparid(n_degradhp) = modparidtype('degradhp  ', m_lpar , m_degradhP)
    modparid(n_cevpc) = modparidtype('cevpcorr  ', m_rpar , m_cevpcorr)
    modparid(118) = modparidtype('minc      ', m_gpar , m_minc)
    modparid(119) = modparidtype('humusc1   ', m_lpar , m_humusC1)
    modparid(120) = modparidtype('fastc1    ', m_lpar , m_fastC1)
    modparid(121) = modparidtype('klh       ', m_gpar , m_crate1)
    modparid(122) = modparidtype('klo       ', m_gpar , m_crate2)
    modparid(123) = modparidtype('kho       ', m_gpar , m_crate3)
    modparid(124) = modparidtype('tcobselev ', m_gpar , m_tcobselev)
    modparid(125) = modparidtype('ripz      ', m_lpar , m_ripz)
    modparid(126) = modparidtype('ripe      ', m_gpar , m_ripe)
    modparid(127) = modparidtype('sedoc     ', m_gpar , m_sedoc)
    modparid(128) = modparidtype('koc       ', m_gpar , m_crate5)
    modparid(129) = modparidtype('kcgwreg   ', m_gpar , m_crate6)
    modparid(130) = modparidtype('rips      ', m_gpar , m_rips)
    modparid(n_sndens0) = modparidtype('sdnsnew   ', m_gpar , m_sndens0)
    modparid(n_pprelmax) = modparidtype('pprelmax  ', m_gpar , m_pprelmax)
    modparid(n_pprelexp) = modparidtype('pprelexp  ', m_gpar , m_pprelexp)
    modparid(134) = modparidtype('tnmean    ', m_rpar, m_tnmean)
    modparid(135) = modparidtype('tocmean   ', m_rpar, m_tocmean)
    modparid(136) = modparidtype('humusc2   ', m_lpar , m_humusC2)
    modparid(137) = modparidtype('humusc3   ', m_lpar , m_humusC3)
    modparid(138) = modparidtype('fastc2    ', m_lpar , m_fastC2)
    modparid(139) = modparidtype('fastc3    ', m_lpar , m_fastC3)
    modparid(n_rrcsc) = modparidtype('rrcscorr  ', m_rpar , m_rrcscorr)
    modparid(141) = modparidtype('kof       ', m_gpar , m_crate9)
    modparid(142) = modparidtype('koflim    ', m_gpar , m_crate10)
    modparid(143) = modparidtype('grata     ', m_gpar , m_grat3)
    modparid(n_limprod) = modparidtype('limqprod  ', m_gpar , m_limprod)
    modparid(n_partp1) = modparidtype('partp1    ', m_lpar , m_partP1)
    modparid(n_partp2) = modparidtype('partp2    ', m_lpar , m_partP2)
    modparid(n_partp3) = modparidtype('partp3    ', m_lpar , m_partP3)
    modparid(148) = modparidtype('wprodc    ', m_gpar , m_wprodc)
    modparid(149) = modparidtype('denitwrl  ', m_gpar,  m_denitwrl) !denitw local river
    modparid(150) = modparidtype('qmean     ', m_ldpar, m_ldqmean)
    modparid(151) = modparidtype('tpmean    ', m_ldpar, m_ldtpmean)
    modparid(152) = modparidtype('tnmean    ', m_ldpar, m_ldtnmean)
    modparid(153) = modparidtype('tocmean   ', m_ldpar, m_ldtocmean)
    modparid(154) = modparidtype('wprodn    ', m_ldpar, m_ldwprodn)
    modparid(155) = modparidtype('sedon     ', m_ldpar, m_ldsedon)
    modparid(156) = modparidtype('sedoc     ', m_ldpar, m_ldsedoc)
    modparid(157) = modparidtype('sedpp     ', m_ldpar, m_ldsedpp)
    modparid(158) = modparidtype('wprodp    ', m_ldpar, m_ldwprodp)
    modparid(n_limt2exch) = modparidtype('limt2exch ', m_gpar , m_limt2exch)
    modparid(n_sfdlim) = modparidtype('sfdlim    ', m_gpar , m_sfdlim)
    modparid(161) = modparidtype('denitwl   ', m_gpar , m_denitwl)
    modparid(162) = modparidtype('denitwl   ', m_ldpar, m_lddenitwl)
    modparid(163) = modparidtype('wprodc    ', m_ldpar, m_ldwprodc)
    modparid(164) = modparidtype('prodpp    ', m_ldpar, m_ldprodpp)
    modparid(165) = modparidtype('prodsp    ', m_ldpar, m_ldprodsp)
    modparid(166) = modparidtype('cmltcorr  ', m_rpar , m_cmltcorr)
    !167 available
    modparid(n_laketemp) = modparidtype('laketemp  ', m_gpar , m_laketemp)
    modparid(169) = modparidtype('deadm     ', m_gpar , m_deadm)
    modparid(n_incorr) = modparidtype('incorr    ', m_rpar, m_incorr)
    modparid(n_oncorr) = modparidtype('oncorr    ', m_rpar, m_oncorr)
    modparid(n_phoscorr) = modparidtype('phoscorr  ', m_rpar, m_phoscorr)
    modparid(173) = modparidtype('ratcorr   ', m_rpar , m_ratcorr)
    modparid(n_ponatm) = modparidtype('ponatm    ', m_lpar , m_ponatm)
    modparid(175) = modparidtype('preccorr  ', m_rpar , m_preccorr)
    modparid(176) = modparidtype('cirrsink  ', m_rpar , m_cirrsink)
    modparid(177) = modparidtype('irrcomp   ', m_gpar , m_irrcomp)
    modparid(178) = modparidtype('ocsoimsat ', m_lpar , m_ocsoim)
    modparid(179) = modparidtype('ocsoimslp ', m_lpar , m_ocsmslp)
    modparid(180) = modparidtype('pirrs     ', m_rpar , m_pirrs)
    modparid(181) = modparidtype('pirrg     ', m_rpar , m_pirrg)
    modparid(n_onpercred) = modparidtype('onpercred ', m_lpar , m_onpercred)
    modparid(n_pppercred) = modparidtype('pppercred ', m_lpar , m_pppercred)
    modparid(n_onconc0) = modparidtype('onconc0   ', m_lpar , m_onconc0)
    modparid(n_ppconc0) = modparidtype('ppconc0   ', m_lpar , m_ppconc0)
    modparid(n_occonc0) = modparidtype('occonc0   ', m_lpar , m_occonc0)
    modparid(n_snalbmin) = modparidtype('snalbmin  ', m_lpar , m_snalbmin)
    modparid(n_snalbmax) = modparidtype('snalbmax  ', m_lpar , m_snalbmax)
    modparid(n_snalbkexp) = modparidtype('snalbkexp ', m_lpar , m_snalbkexp)
    modparid(n_cmrad) = modparidtype('cmrad     ', m_lpar , m_cmrad)
    modparid(191) = modparidtype('pcluse    ', m_lpar , m_pcluse)
    modparid(192) = modparidtype('aloadconst', m_gpar , m_atmload)

    !Water T2 temperature parameters
    modparid(n_t2trriver) = modparidtype('t2trriver ', m_gpar , m_t2trriver)  !temp flow from air to river
    modparid(n_t2trlake) = modparidtype('t2trlake  ', m_gpar , m_t2trlake)   !temp flow from air to lake
    modparid(195) = modparidtype('krelflood ', m_gpar , m_krelflood)  !some flood control dam parameter
    modparid(196) = modparidtype('upper2deep', m_gpar , m_upper2deep) !temp flow from upper to lower lake layer

    !Lake ice parameters
    modparid(197) = modparidtype('licewme   ', m_gpar , m_licewme)    !lake ice, water melt efficiency
    modparid(n_licetf) = modparidtype('licetf    ', m_gpar , m_licetf)     !lake ice, freezing temperature
    modparid(n_licesndens) = modparidtype('licesndens', m_gpar , m_licesndens) !lake ice, snow compaction parameter
    modparid(n_licekika) = modparidtype('licekika  ', m_gpar , m_licekika)   !lake ice, ki/ka
    modparid(n_licekexp) = modparidtype('licekexp  ', m_gpar , m_licekexp)   !lake ice, ks = ki*(dsnow(dice)^kexp, 1.88
    modparid(n_licetmelt) = modparidtype('licetmelt ', m_gpar , m_licetmelt)  !lake ice, degreeday factor for ice melt
    modparid(203) = modparidtype('licewcorr ', m_gpar , m_licewcorr)  !lake ice, snow fall reduction for winddrift
    modparid(n_licessmft) = modparidtype('licessmft ', m_gpar , m_licessmft) !lake ice, subsurface melt fraction, temperature driven melt
    modparid(n_licessmfr) = modparidtype('licessmfr ', m_gpar , m_licessmfr) !lake ice, subsurface melt fraction, radiation driven melt
    modparid(n_licermelt) = modparidtype('licermelt ', m_gpar , m_licermelt) !lake ice, radiation melt efficiency (fraction of radiation used for melt at T>0)
    modparid(n_licebupo) = modparidtype('licebupo  ', m_gpar , m_licebupo)   !lake ice, breakup porosity
    modparid(n_liceqhw)  = modparidtype('liceqhw   ', m_gpar , m_liceqhw)    !lake ice, heat flux from water to ice

    !River ice parameters
    modparid(204) = modparidtype('ricewme   ', m_gpar , m_ricewme)    !river ice, water melt efficiency
    modparid(n_ricetf) = modparidtype('ricetf    ', m_gpar , m_ricetf)     !river ice, freezing temperature
    modparid(n_ricesndens) = modparidtype('ricesndens', m_gpar , m_ricesndens) !river ice, snow compaction parameter
    modparid(n_ricekika) = modparidtype('ricekika  ', m_gpar , m_ricekika)    !river ice, ki/ka
    modparid(n_ricekexp) = modparidtype('ricekexp  ', m_gpar , m_ricekexp)    !river ice, ki/ks
    modparid(n_ricetmelt) = modparidtype('ricetmelt ', m_gpar , m_ricetmelt)  !river ice, degreeday factor for ice melt
    modparid(n_ricessmft) = modparidtype('ricessmft ', m_gpar , m_ricessmft)  !river ice, subsurface melt fraction, temp
    modparid(n_ricessmfr) = modparidtype('ricessmfr ', m_gpar , m_ricessmfr)  !river ice, subsurface melt fraction, radiation
    modparid(n_ricermelt) = modparidtype('ricermelt ', m_gpar , m_ricermelt)  !river ice, radiation melt efficiency
    modparid(n_ricebupo) = modparidtype('ricebupo  ', m_gpar , m_ricebupo)    !river ice, breakup porosity
    modparid(n_ricethpo) = modparidtype('ricethpo  ', m_gpar , m_ricethpo)    !river ice, threshold porosity when the river water level rating curve starts to transition from ice to non-ice
    modparid(n_riceqhmn) = modparidtype('riceqhmn  ', m_gpar , m_riceqhmn)    !river ice, (min) heat flux from water to ice at waterflow <= riceqhqs
    modparid(n_riceqhmx) = modparidtype('riceqhmx  ', m_gpar , m_riceqhmx)    !river ice, (max) heat flux from water to ice at waterflow >= riceqhqe
    modparid(n_ricecwi)  = modparidtype('ricecwi   ', m_gpar , m_ricecwi)     !river ice, heat exchange coefficient between stream flow and river ice

    !snow cover area parameters
    modparid(210) = modparidtype('fscmax    ', m_gpar, m_fscmax)      !maximum snow cover area (0.95 [-])
    modparid(211) = modparidtype('fscmin    ', m_gpar, m_fscmin)      !minimum fsc             (0.001 [-])
    modparid(212) = modparidtype('fsclim    ', m_gpar, m_fsclim)      !fsc limit for onset of snowmax (0.001 [-])
    modparid(213) = modparidtype('fscdistmax', m_lpar, m_fscdistmax)  !maximum snow distribution factor (0.8 [mm^-1])
    modparid(214) = modparidtype('fscdist0  ', m_lpar, m_fscdist0)    !minimum snow distribution factor (0.6 [mm^-1])
    modparid(215) = modparidtype('fscdist1  ', m_lpar, m_fscdist1)    !elev_std coefficient for snow distr factor (0.001 [m^-1])
    modparid(216) = modparidtype('fsck1     ', m_gpar, m_fsck1)       !time constant in decrease of snowmax during melt (0.2 [-])
    modparid(217) = modparidtype('fsckexp   ', m_gpar, m_fsckexp)     !exponential coefficient in decrease of snowmax during melt (1e-6 [s^-1])

    !parameters for radiation and optional potential evaporation calculations
    modparid(n_krs) = modparidtype('krs       ', m_gpar, m_krs)         !Hargreaves adjustment factor in estimation of shortwave radiation from tmin and tmax 0.16 (inland) 0.19 (coastal)
    modparid(n_jhtadd) = modparidtype('jhtadd    ', m_gpar, m_jhtadd)      !PET parameter in Jensen-Haise/McGuinness following Oudin et al (2005), recommended value 5
    modparid(n_jhtscale) = modparidtype('jhtscale  ', m_gpar, m_jhtscale)    !PET parameter in Jensen-Haise/McGuinness following Oudin et al (2005), recommended value 100
    modparid(n_alfapt) = modparidtype('alfapt    ', m_gpar, m_alfapt)      !Priestly-taylor coefficient, recommended value 1.26
    modparid(n_mwind) = modparidtype('mwind     ', m_gpar, m_mwind)       !Mean windspeed, to replace missing data in FAO Penman-Monteith pet-equation, suggested value 2 m/s
    modparid(n_kc(1)) = modparidtype('kc        ', m_lpar, m_kc(1))          !Landuse dependent crop coefficient, used to scale the optional reference PET estimates (Hargreaves, Jensen, Priestly-Taylor,Penman-Monteith....)
    modparid(n_alb) = modparidtype('alb       ', m_lpar, m_alb)         !Landuse dependent albedo, used for net radiation calculation (Priestly-Taylor,Penman-Monteith), suggested value 0.23

    !Glacier parameters (previous non-optional constants in the code)
    modparid(n_glacvcoef) = modparidtype('glacvcoef ', m_gpar, m_glacvcoef)   !Coefficient glacier volume-area relationship (default 0.205)
    modparid(n_glacvexp) = modparidtype('glacvexp  ', m_gpar, m_glacvexp)    !Exponent glacier volume-area relationship (default 1.375)
    modparid(n_glacdens) = modparidtype('glacdens  ', m_gpar, m_glacdens)    !Glacier density (default 0.85 m3 water/m3 ice)
    modparid(n_glacvcoef1) = modparidtype('glacvcoef1', m_gpar, m_glacvcoef1)  !Coefficient glacier volume-area relationship, glaciertype 1 (default 1.701)
    modparid(n_glacvexp1) = modparidtype('glacvexp1 ', m_gpar, m_glacvexp1)   !Exponent glacier volume-area relationship, glaciertype 1 (default 1.25)
    modparid(230) = modparidtype('glac2arlim', m_gpar, m_glac2arlim)  !Area limit to separate glacer type 0 (mountain glacier) from type 1 (ice caps)

    modparid(n_rcgrwst) = modparidtype('rcgrwst   ', m_spar, m_rcgrwst)     !Deep percolation to aquifer
    modparid(n_aqretcorr) = modparidtype('aqretcor  ', m_rpar, m_aqretcorr)   !Aquifer return flow adjustment parameter
    modparid(n_aqdelcorr) = modparidtype('aqdelcor  ', m_rpar, m_aqdelcorr)   !Aquifer percolation delay adjustment parameter
    modparid(n_aqpercorr) = modparidtype('aqpercor  ', m_rpar, m_aqpercorr)   !Aquifer percolation adjustment parameter
    modparid(n_denitaq) = modparidtype('denitaq   ', m_gpar, m_denitaq)     !Aquifer denitrification

    !Additional Lake and River water temperature parameters, to be replacing the t2trlake and t2trriver parameters
    modparid(n_tcfriver) = modparidtype('tcfriver  ', m_gpar, m_tcfriver)    !air-riverwater heat flow, temperature difference coefficient
    modparid(n_scfriver) = modparidtype('scfriver  ', m_gpar, m_scfriver)    !air-riverwater heat flow, solar radiation coefficient
    modparid(n_ccfriver) = modparidtype('ccfriver  ', m_gpar, m_ccfriver)    !air-riverwater heat flow, constant coefficient
    modparid(n_lcfriver) = modparidtype('lcfriver  ', m_gpar, m_lcfriver)    !air-riverwater heat flow, linear coefficient
    modparid(n_tcflake) = modparidtype('tcflake   ', m_gpar, m_tcflake)      !air-lakewater heat flow, temperature difference coefficient
    modparid(n_scflake) = modparidtype('scflake   ', m_gpar, m_scflake)      !air-lakewater heat flow, solar radiation coefficient
    modparid(n_ccflake) = modparidtype('ccflake   ', m_gpar, m_ccflake)      !air-lakewater heat flow, constant coefficient
    modparid(n_lcflake) = modparidtype('lcflake   ', m_gpar, m_lcflake)      !air-lakewater heat flow, linear coefficient
    modparid(n_stbcorr1) = modparidtype('stbcorr1  ', m_gpar, m_stbcorr1)    !parameter for stability correction
    modparid(n_stbcorr2) = modparidtype('stbcorr2  ', m_gpar, m_stbcorr2)    !parameter for stability correction
    modparid(n_stbcorr3) = modparidtype('stbcorr3  ', m_gpar, m_stbcorr3)    !parameter for stability correction
    modparid(n_zwind) = modparidtype('zwind     ', m_gpar, m_zwind)       !wind observation level
    modparid(n_zwish) = modparidtype('zwish     ', m_gpar, m_zwish)       !wanted wind observation level for PET
    modparid(n_zpdh) = modparidtype('zpdh      ', m_gpar, m_zpdh)        !zero plane displacement height for wind
    modparid(n_roughness) = modparidtype('roughness ', m_gpar, m_roughness)   !surface roughness for wind

    !Additional precipitation correction; orographic (pcelevstd) and phase (pcusnow, pcurain) impact on undercatch
    modparid(251) = modparidtype('pcelevstd ', m_gpar, m_pcelevstd)    !fractional increase in precipitation per 100m elev.std
    modparid(n_pcur) = modparidtype('pcurain   ', m_gpar, m_pcurain)      !undercatch correction factor for rain
    modparid(n_pcus) = modparidtype('pcusnow   ', m_gpar, m_pcusnow)      !undercatch correction factor for snow

    !Snow evaporation, refreeze, and efficiency in fractional snowcover to reduce snowmelt and evaporation
    modparid(n_fepotsnow) = modparidtype('fepotsnow ', m_lpar, m_fepotsnow)   !fraction of potential evaporation used for snow evaporation
    modparid(n_cmrefr) = modparidtype('cmrefr    ', m_gpar, m_cmrefr)      !snow refreeze efficiency (fraction of degree day factor cmlt)
    modparid(n_fsceff) = modparidtype('fsceff    ', m_gpar, m_fsceff)      !efficiency of fractional snow cover to reduce melt and evap

    !Separate glacier melt parameters and sublimation/evaporation parameters
    modparid(257) = modparidtype('glacalb   ', m_gpar, m_glacalb)           !glacier ice albedo (0.35)
    modparid(258) = modparidtype('glacttmp  ', m_gpar, m_glacttmp)
    modparid(259) = modparidtype('glaccmlt  ', m_gpar, m_glaccmlt)
    modparid(260) = modparidtype('glaccmrad ', m_gpar, m_glaccmrad)
    modparid(261) = modparidtype('glaccmrefr', m_gpar, m_glaccmrefr)
    modparid(262) = modparidtype('fepotglac ', m_gpar, m_fepotglac)

    modparid(263) = modparidtype('kthrflood ', m_gpar, m_kthrflood)      !Threshold inflow over which a flood control dam save water (fraction of max inflow)
    modparid(264) = modparidtype('klowflood ', m_gpar, m_klowflood)      !Threshold level for extra flood control releases (fraction of regvol, typical 1/3)
    modparid(265) = modparidtype('monthlapse', m_mpar, m_mlapse)
    modparid(266) = modparidtype('limsedon  ', m_gpar, m_limsedon)
    modparid(267) = modparidtype('limsedpp  ', m_gpar, m_limsedpp)

    modparid(268) = modparidtype('opt1      ', m_gpar, m_opt1)        ! special optimisation parameter (to replace a GeoData or LakeData)
    modparid(269) = modparidtype('opt2      ', m_gpar, m_opt2)        ! special optimisation parameter (to replace a GeoData or LakeData)
    modparid(270) = modparidtype('opt3      ', m_gpar, m_opt3)        ! special optimisation parameter (to replace a GeoData or LakeData)
    modparid(271) = modparidtype('opt4      ', m_gpar, m_opt4)        ! special optimisation parameter (to replace a GeoData or LakeData)
    modparid(272) = modparidtype('opt5      ', m_gpar, m_opt5)        ! special optimisation parameter (to replace a GeoData or LakeData)
    modparid(273) = modparidtype('opt6      ', m_gpar, m_opt6)        ! special optimisation parameter (to replace a GeoData or LakeData)
    modparid(274) = modparidtype('opt7      ', m_gpar, m_opt7)        ! special optimisation parameter (to replace a GeoData or LakeData)
    modparid(275) = modparidtype('opt8      ', m_gpar, m_opt8)        ! special optimisation parameter (to replace a GeoData or LakeData)
    modparid(276) = modparidtype('optonoff  ', m_gpar, m_optonoff)    ! parameter "switch" to turn the opt1-opt8 on(1)/off(0) for the floodplainmodel
    modparid(277) = modparidtype('init1sw   ', m_gpar, m_init1sw)     !T1 initial concentration in river and lakes
    modparid(n_sdnsmax) = modparidtype('sdnsmax   ', m_gpar, m_sdnsmax)     !snow density/depth parameter
    modparid(n_sdnsrate) = modparidtype('sdnsrate  ', m_gpar, m_sdnsrate)    !snow density/depth parameter
    modparid(n_sdnsradd) = modparidtype('sdnsradd  ', m_gpar, m_sdnsradd)    !snow density/depth parameter
    modparid(281) = modparidtype('t2mix     ', m_gpar, m_gt2mix)      !T2 of outflow parameter
    modparid(282) = modparidtype('t2mix     ', m_ldpar, m_ldt2mix)    !T2 of outflow parameter
    modparid(283) = modparidtype('denitrlu3 ', m_lpar , m_denitr3)    !denitrification soillayer 3
    modparid(n_hsatINsoil) = modparidtype('hsatins   ', m_gpar , m_hsatINsoil) !half saturation concentration, denitrification in soil (mg N/L)
    modparid(285) = modparidtype('hsatinw   ', m_gpar , m_hsatINwater) !half saturation concentration, denitrification in water (mg N/L)
    modparid(286) = modparidtype('hsattp    ', m_gpar , m_hsatTP)     !half saturation concentration, production in water (mg P/L)
    !T1 parameters
    modparid(287) = modparidtype('t1expdec  ', m_gpar, m_t1expdec)    !half-life of tracer
    modparid(288) = modparidtype('t1freuc   ', m_gpar, m_t1freuc)     !tracer adsorption coefficient
    modparid(289) = modparidtype('t1release ', m_gpar, m_t1rel)       !tracer release from source parameter (mm-1)
    modparid(290) = modparidtype('t1sedvel  ', m_gpar, m_t1sed)       !sedimentation velocity of T1 [m/timestep]
    modparid(291) = modparidtype('t1sedexp  ', m_gpar, m_t1sedexp)    !sedimentation/resuspension of T1 in river sediments
    modparid(292) = modparidtype('t1leaksoil', m_spar, m_t1leakst)    !T1 concentration of leakage
    modparid(293) = modparidtype('t1leakluse', m_lpar, m_t1leaklu)    !T1 concentration of leakage
    modparid(n_denit3reg) = modparidtype('denit3reg ', m_rpar, m_denit3reg)    !denitrification soillayer 3, regional replacement
    modparid(295) = modparidtype('soilcorr  ', m_lpar, m_soilstretch)    !stretching soil layer thickness of soil layer 2 and 3
    modparid(n_ttrig) = modparidtype('ttrig     ', m_lpar, m_ttrig)
    modparid(n_treda) = modparidtype('treda     ', m_lpar, m_treda)
    modparid(n_tredb) = modparidtype('tredb     ', m_lpar, m_tredb)
    modparid(n_gldepo) = modparidtype('gldepo    ', m_gpar, m_gldepo)
    modparid(300) = modparidtype('gicatch   ', m_gpar, m_gicatch)
    modparid(301) = modparidtype('ilratk    ', m_rpar, m_ilrrat1)
    modparid(302) = modparidtype('ilratp    ', m_rpar, m_ilrrat2)
    modparid(303) = modparidtype('illdepth  ', m_rpar, m_ilrldep)
    modparid(304) = modparidtype('ilicatch  ', m_rpar, m_ilricatch)
    modparid(305) = modparidtype('olratk    ', m_rpar, m_olrrat1)
    modparid(306) = modparidtype('olratp    ', m_rpar, m_olrrat2)
    modparid(307) = modparidtype('olldepth  ', m_rpar, m_olrldep)
    modparid(308) = modparidtype('glacannmb ', m_gpar, m_glacannmb) !glacier initial volume scaling coefficient, glacier annual mass balance (mm/year)
    modparid(n_bfroznsoil) = modparidtype('bfroznsoil', m_spar, m_bfroznsoil) !empirical coefficient = 2.10 & 1.14 for prairie & forest soils, respectively
    modparid(n_kc(2)) = modparidtype('kc2       ', m_lpar, m_kc(2))        !Landuse dependent crop coefficient, used to scale the optional PET model 2 (Modified Jensen-Haise/McGuinness)
    modparid(n_kc(3)) = modparidtype('kc3       ', m_lpar, m_kc(3))        !Landuse dependent crop coefficient, used to scale the optional PET model 3 (Hargreaves-Samani)
    modparid(n_kc(4)) = modparidtype('kc4       ', m_lpar, m_kc(4))        !Landuse dependent crop coefficient, used to scale the optional PET model 4 (Priestly-Taylor)
    modparid(n_kc(5)) = modparidtype('kc5       ', m_lpar, m_kc(5))        !Landuse dependent crop coefficient, used to scale the optional PET model 5 (FAO Penman-Monteith)
    modparid(n_erodluse) = modparidtype('erodluse  ', m_lpar, m_erodluse)     !landuse erosion factor
    modparid(n_erodsoil) = modparidtype('erodsoil  ', m_spar, m_erodsoil)     !soil type erosion factor
    modparid(n_erodslope) = modparidtype('erodslope ', m_gpar, m_erodslope)    !slope erosion factor (exponent)
    modparid(n_erodexp) = modparidtype('erodexp   ', m_gpar, m_erodexp)      !erosion precipitation dependence factor (exponent)
    modparid(n_erodindex) = modparidtype('erodindex ', m_gpar, m_erodindex)    !scaling of erosion index
    modparid(319) = modparidtype('sedss    ', m_gpar , m_sedss)       !sedimentation velocity
    modparid(320) = modparidtype('limsedss ', m_gpar , m_limsedss)    !concentration limit for sedimentation
    modparid(n_sedae) = modparidtype('sedae    ', m_gpar , m_sedae)       !sedimentation velocity
    modparid(n_wetspl)  = modparidtype('wetdepspl ', m_gpar , m_wetspload)   !Only used on water

    modparid(n_fraxe) = modparidtype('fraxe    ', m_gpar , m_fraxe)       !mean river depth where fractional river area = 1
    modparid(n_fraxm) = modparidtype('fraxm    ', m_gpar , m_fraxm)       !mean river depth where the slope of the fractional river area has its maximum (must be in the range between 0 and fraxe)
    modparid(n_ppenrmax) = modparidtype('ppenrmax  ', m_spar, m_ppenrmax)     !soil type erosion PP enrichment
    modparid(n_ppenrstab) = modparidtype('ppenrstab ', m_gpar, m_ppenrstab)   !erosion PP enrichment
    modparid(n_ppenrflow) = modparidtype('ppenrflow ', m_gpar, m_ppenrflow)   !erosion PP enrichment

    modparid(328) = modparidtype('wetrate   ', m_gpar , m_wetrate)
    modparid(329) = modparidtype('iwetw0    ', m_gpar , m_iwetw0)
    modparid(330) = modparidtype('owetw0    ', m_gpar , m_owetw0)
    modparid(331) = modparidtype('wetexp    ', m_gpar , m_wetexp)
    modparid(332) = modparidtype('wlsed     ', m_gpar, m_wlsed)
    modparid(333) = modparidtype('wlproddep ', m_gpar, m_wlproddep)
    modparid(334) = modparidtype('wlmphuptin', m_gpar, m_wlmphuptin)
    modparid(335) = modparidtype('wlmphuptsp', m_gpar, m_wlmphuptsp)
    modparid(336) = modparidtype('wlfastfrac', m_gpar, m_wlfastfrac)
    modparid(337) = modparidtype('wlpartfrac', m_gpar, m_wlpartfrac)
    modparid(338) = modparidtype('wltmpexp  ', m_gpar, m_wltmpexp)
    modparid(n_hygeomf) = modparidtype('hygeomf   ', m_gpar, m_hygeomf)
    modparid(n_hygeomm) = modparidtype('hygeomm   ', m_gpar, m_hygeomm)
    modparid(n_hygeomc) = modparidtype('hygeomc   ', m_gpar, m_hygeomc)
    modparid(n_hygeomg) = modparidtype('hygeomk   ', m_gpar, m_hygeomk)
    modparid(343) = modparidtype('muptn     ', m_gpar , m_muptn)        !macrophyte uptake N lake
    modparid(344) = modparidtype('muptn     ', m_ldpar, m_ldmuptn)      !macrophyte uptake N
    modparid(345) = modparidtype('muptp     ', m_gpar , m_muptp)        !macrophyte uptake P lake
    modparid(346) = modparidtype('muptp     ', m_ldpar, m_ldmuptp)      !macrophyte uptake P
    modparid(347) = modparidtype('muptdep   ', m_gpar, m_muptdep)       !macrophyte uptake production depth lake
    modparid(n_muptnriv) = modparidtype('muptnriv  ', m_gpar , m_muptnriver)        !macrophyte uptake N river
    modparid(n_muptpriv) = modparidtype('muptpriv  ', m_gpar , m_muptpriver)        !macrophyte uptake P river
    modparid(n_muptrivdep) = modparidtype('muptdepriv', m_gpar, m_muptdepriver)     !macrophyte uptake production depth river
    modparid(n_snkika) = modparidtype('snkika    ', m_lpar , m_snkika)  !relation between snow thermal conductivity (ks) and surface heat exchange coefficient (ka), unit is in meters, values in the range 10-100 approximately.
    modparid(n_logsatmp) = modparidtype('logsatmp', m_spar, m_logsatmp) !unfrozen soil water content function MM2016, DG2018
    modparid(n_bcosby) = modparidtype('bcosby', m_spar, m_bcosby)       !unfrozen soil water content function MM2016, DG2018
    modparid(n_fzsexpand) = modparidtype('fzsexpand ', m_spar, m_fzsexpand) !unfrozen soil water content function

    modparid(n_whcsnow) = modparidtype('whcsnow   ', m_gpar, m_whcsnow)     !water holding capacity of snow, typical value 0.08, must be smaller than 1 and larger or equal to 0
    modparid(366)  = modparidtype('inrelease ', m_gpar , m_inrel)   !release from on-soil surface pool
    modparid(367)  = modparidtype('onrelease ', m_gpar , m_onrel)   !release from on-soil surface pool
    modparid(368)  = modparidtype('sprelease ', m_gpar , m_sprel)   !release from on-soil surface pool
    modparid(369)  = modparidtype('pprelease ', m_gpar , m_pprel)   !release from on-soil surface pool
    modparid(370)  = modparidtype('ocrelease ', m_gpar , m_ocrel)   !release from on-soil surface pool
    modparid(371)  = modparidtype('ssrelease ', m_gpar , m_ssrel)   !release from on-soil surface pool
    modparid(372)  = modparidtype('aerelease ', m_gpar , m_aerel)   !releease from on-soil surface pool
    modparid(373)  = modparidtype('totexpsl1 ', m_gpar , m_totexp1)   !exponent soil layer 1 exponential decay
    modparid(374)  = modparidtype('totexpsl2 ', m_gpar , m_totexp2)   !exponent soil layer 2 exponential decay
    modparid(375)  = modparidtype('totexpsl3 ', m_gpar , m_totexp3)   !exponent soil layer 3 exponential decay
    modparid(376)  = modparidtype('intimeot  ', m_lpar , m_intot)   !time of travel in soil
    modparid(377)  = modparidtype('ontimeot  ', m_lpar , m_ontot)   !time of travel in soil
    modparid(378)  = modparidtype('sptimeot  ', m_lpar , m_sptot)   !time of travel in soil
    modparid(379)  = modparidtype('pptimeot  ', m_lpar , m_pptot)   !time of travel in soil
    modparid(380)  = modparidtype('octimeot  ', m_lpar , m_octot)   !time of travel in soil
    modparid(381)  = modparidtype('sstimeot  ', m_lpar , m_sstot)   !time of travel in soil
    modparid(382)  = modparidtype('aetimeot  ', m_lpar , m_aetot)   !time of travel in soil
    modparid(383)  = modparidtype('t1timeot  ', m_lpar , m_t1tot)   !time of travel in soil
    modparid(384)  = modparidtype('indecay   ', m_gpar , m_indecay)  !IN exponential decay
    modparid(385)  = modparidtype('ondecay   ', m_gpar , m_ondecay)  !ON exponential decay
    modparid(386)  = modparidtype('spdecay   ', m_gpar , m_spdecay)  !SP exponential decay
    modparid(387)  = modparidtype('ppdecay   ', m_gpar , m_ppdecay)  !PP exponential decay
    modparid(388)  = modparidtype('ocdecay   ', m_gpar , m_ocdecay)  !OC exponential decay
    modparid(389)  = modparidtype('ssdecay   ', m_gpar , m_ssdecay)  !SS exponential decay
    modparid(390)  = modparidtype('aedecay   ', m_gpar , m_aedecay)  !AE exponential decay
    modparid(391)  = modparidtype('t1decay   ', m_gpar , m_t1decay)  !T1 exponential decay
    modparid(392)  = modparidtype('totexp0   ', m_gpar , m_totexp0)   !exponent on soil pool exponential decay
    modparid(393)  = modparidtype('inload1f  ', m_gpar , m_sloadinf1)
    modparid(394)  = modparidtype('inload2f  ', m_gpar , m_sloadinf2)
    modparid(395)  = modparidtype('inload3f  ', m_gpar , m_sloadinf3)
    modparid(396)  = modparidtype('onload1f  ', m_gpar , m_sloadonf1)
    modparid(397)  = modparidtype('onload2f  ', m_gpar , m_sloadonf2)
    modparid(398)  = modparidtype('onload3f  ', m_gpar , m_sloadonf3)
    modparid(399)  = modparidtype('spload1f  ', m_gpar , m_sloadspf1)
    modparid(400)  = modparidtype('spload2f  ', m_gpar , m_sloadspf2)
    modparid(401)  = modparidtype('spload3f  ', m_gpar , m_sloadspf3)
    modparid(402)  = modparidtype('ppload1f  ', m_gpar , m_sloadppf1)
    modparid(403)  = modparidtype('ppload2f  ', m_gpar , m_sloadppf2)
    modparid(404)  = modparidtype('ppload3f  ', m_gpar , m_sloadppf3)
    modparid(405)  = modparidtype('ocload1f  ', m_gpar , m_sloadocf1)
    modparid(406)  = modparidtype('ocload2f  ', m_gpar , m_sloadocf2)
    modparid(407)  = modparidtype('ocload3f  ', m_gpar , m_sloadocf3)
    modparid(408)  = modparidtype('ssload1f  ', m_gpar , m_sloadssf1)
    modparid(409)  = modparidtype('ssload2f  ', m_gpar , m_sloadssf2)
    modparid(410)  = modparidtype('ssload3f  ', m_gpar , m_sloadssf3)
    modparid(411)  = modparidtype('aeload1f  ', m_gpar , m_sloadaef1)
    modparid(412)  = modparidtype('aeload2f  ', m_gpar , m_sloadaef2)
    modparid(413)  = modparidtype('aeload3f  ', m_gpar , m_sloadaef3)
    modparid(414)  = modparidtype('t1load1f  ', m_gpar , m_sloadt1f1)
    modparid(415)  = modparidtype('t1load2f  ', m_gpar , m_sloadt1f2)
    modparid(416)  = modparidtype('t1load3f  ', m_gpar , m_sloadt1f3)
    modparid(417)  = modparidtype('totref    ', m_gpar , m_totref)
    modparid(n_macfrac) = modparidtype('macfrac   ', m_spar , m_macfrac)
    modparid(n_srbeta) = modparidtype('srbeta    ', m_gpar , m_srbeta)      !exponent of the beta curve (S/Smax)^beta for surface runoff
    modparid(n_sralpha) = modparidtype('sralpha   ', m_gpar , m_sralpha)     !exponent of the beta curve (S/Smax)^beta for surface runoff
    modparid(n_srgamma) = modparidtype('srgamma   ', m_gpar , m_srgamma)     !exponent of the beta curve (S/Smax)^beta for surface runoff
    modparid(n_srnlayer) = modparidtype('srnlayer  ', m_gpar , m_srnlayer)    !number of soil layer to use to quantify soil moisture by (S/Smax)^beta for surface runoff
    modparid(n_spdecaq) = modparidtype('spdecaq   ', m_gpar, m_spdecaq)     !Aquifer sp decay
    modparid(n_ppdecaq) = modparidtype('ppdecaq   ', m_gpar, m_ppdecaq)     !Aquifer pp decay
    modparid(n_ssdecaq) = modparidtype('ssdecaq   ', m_gpar, m_ssdecaq)     !Aquifer ss decay
    modparid(n_aedecaq) = modparidtype('aedecaq   ', m_gpar, m_aedecaq)     !Aquifer ae decay
    modparid(n_t1decaq) = modparidtype('t1decaq   ', m_gpar, m_t1decaq)     !Aquifer t1 decay
    modparid(n_ondecaq) = modparidtype('ondecaq   ', m_gpar, m_ondecaq)     !Aquifer on decay
    modparid(n_ocdecaq) = modparidtype('ocdecaq   ', m_gpar, m_ocdecaq)     !Aquifer oc decay
    modparid(n_ricew0por) = modparidtype('ricew0por ', m_gpar , m_ricew0por)    !river ice, threshold when the river ice porosity completely reduce the impact of ice on the rating curve parameters (option 2 and 3)
    modparid(n_ricew1por) = modparidtype('ricew1por ', m_gpar , m_ricew1por)    !river ice, threshold when the river ice porosity starts to reduce the impact of ice on the rating curve (option 2 and 3)
    modparid(n_ricew0ice) = modparidtype('ricew0ice ', m_gpar , m_ricew0ice)    !river ice, threshold when the river ice thickness starts to impact on the rating curve (option 2 and 3)
    modparid(n_ricew1ice) = modparidtype('ricew1ice ', m_gpar , m_ricew1ice)    !river ice, threshold when the river ice thickness impact on rating curve reaches maximum (option 2 and 3)
    modparid(437) = modparidtype('hgdmdepth ', m_rpar, m_ilhgdmdep)

  END SUBROUTINE define_model_parameters

  !>Allocate and set parameter region division
  !-----------------------------------------------------------
  SUBROUTINE set_parameters_region_division(nregpar)

    IMPLICIT NONE

    !Argument declarations
    INTEGER,INTENT(IN) :: nregpar  !<number of region parameters

    IF(.NOT.ALLOCATED(regiondivision)) ALLOCATE(regiondivision(nregpar))
    !Set parameter region division index:
    !1=parregion in GeoData,2=parregion i AquiferData
    !3=wqparreg, 4=ilakereg, 5=olakereg, 6=lakeregion, all in GeoData
    !This can be read from definition file later, if parameters are allowed to be coupled freely to different regions.
    regiondivision(m_cevpcorr) = 1
    regiondivision(m_rrcscorr) = 1
    regiondivision(m_ratcorr) = 1
    regiondivision(m_pirrs) = 1
    regiondivision(m_preccorr) = 1
    regiondivision(m_cirrsink) = 1
    regiondivision(m_tcadd) = 1
    regiondivision(m_pirrg) = 1
    regiondivision(m_aqpercorr) = 1
    regiondivision(m_cmltcorr) = 1
    regiondivision(m_aqretcorr) = 2
    regiondivision(m_aqdelcorr) = 2
    regiondivision(m_incorr) = 3
    regiondivision(m_oncorr) = 3
    regiondivision(m_phoscorr) = 3
    regiondivision(m_denit3reg) = 3
    regiondivision(m_ilrrat1) = 4
    regiondivision(m_ilrrat2) = 4
    regiondivision(m_ilrldep) = 4
    regiondivision(m_ilricatch) = 4
    regiondivision(m_ilhgdmdep) = 4
    regiondivision(m_olrrat1) = 5
    regiondivision(m_olrrat2) = 5
    regiondivision(m_olrldep) = 5
    regiondivision(m_velpar1) = 6
    regiondivision(m_velpar2) = 6
    regiondivision(m_velpar3) = 6
    regiondivision(m_widpar1) = 6
    regiondivision(m_widpar2) = 6
    regiondivision(m_widpar3) = 6
    regiondivision(m_tpmean) = 6  !These three are also present as lakedatapar. Parregion 6 hardcoded for lakedatapar
    regiondivision(m_tnmean) = 6  !-"-
    regiondivision(m_tocmean) = 6 !-"-

  END SUBROUTINE set_parameters_region_division

!>Set flags for special HYPE models that will be used
!------------------------------------------------------------
  SUBROUTINE set_special_models(nc,arr,glacexist,irrexist)

  USE IRRIGATION_MODULE, ONLY : find_irrigated_classes

  !Argument declarations
  INTEGER,INTENT(IN) :: nc         !<dimension, number of classes
  INTEGER,INTENT(IN) :: arr(nc)    !<soilmodels
  LOGICAL, INTENT(OUT) :: glacexist   !<status of glacier model
  LOGICAL, INTENT(OUT) :: irrexist    !<status of irrigation model

  INTEGER i

    !>Check for glacier soilmodel
    glacexist = .FALSE.
    DO i=1,nc
      IF(arr(i)==glacier_model) glacexist=.TRUE.
    ENDDO

    !>Check for irrigation included in model set-up
    irrexist = .TRUE.
    IF(.NOT.ALLOCATED(irrigationsystem))THEN
      irrexist = .FALSE.      !Irrigation information not provided in MgmtData
    ELSEIF(.NOT.ALLOCATED(cropirrdata))THEN
      irrexist = .FALSE.      !Irrigation information not provided in CropData
    ENDIF
    IF(irrexist) CALL find_irrigated_classes()

  END SUBROUTINE set_special_models

!>Set special HYPE parameters needed by HYSS
!------------------------------------------------------------
  SUBROUTINE get_special_model_parameters(velindex,dampindex)

  !Argument declarations
  INTEGER, INTENT(OUT) :: velindex  !<index of rivvel in modparid
  INTEGER, INTENT(OUT) :: dampindex !<index of damp in modparid

    !Index of HYPE parameters are needed by HYSS
    velindex = n_rivv
    dampindex = n_damp

  END SUBROUTINE get_special_model_parameters

!>Calculate special HYPE parameters needed by HYSS
!------------------------------------------------------------
  SUBROUTINE calculate_special_model_parameters(nsubb,optrivvel,optdamp,dimriver)

  USE SURFACEWATER_PROCESSES, ONLY : calculate_landarea, calculate_riverlength
  USE HYPE_INDATA, ONLY : set_regest_parameter

  !Argument declarations
  INTEGER, INTENT(IN)  :: nsubb     !<dimension, number of subbasins (submodel)
  REAL, INTENT(IN)     :: optrivvel !<lower river velocity boundary (optpar)
  REAL, INTENT(IN)     :: optdamp   !<lower damp boundary (optpar)
  INTEGER, INTENT(OUT) :: dimriver  !<maximum size river lag needed

  INTEGER i,itype
  REAL maxtrans
  REAL locallandarea(nsubb)
  REAL rivlength(2,nsubb)   !river length (m)
  REAL totaltime,transtime  !total time in river and time in river train (translation) (in timesteps)
  REAL localrivvel,localdamp

    !Calculate local and main river length
    CALL calculate_landarea(nsubb,locallandarea)
    CALL calculate_riverlength(nsubb,locallandarea,rivlength)

    !Calculate river translation time, and the maximum, for every subbasin
    localrivvel = genpar(m_rivvel)
    localdamp = genpar(m_damp)
    IF(.NOT.conductregest)THEN
      localrivvel = MIN(localrivvel,optrivvel)    !This will give the "worst" combination
      localdamp = MIN(localdamp,optdamp)
      IF(localrivvel==0)THEN !Test for missing rivvel in model set-up
        dimriver = 1
        IF(conductwarning) WRITE(6,*) 'Warning: Parameter rivvel not set. River flow will take one time step.'
        RETURN
      ENDIF
      CALL calculate_translation_time(MAXVAL(rivlength),localrivvel,localdamp,totaltime,transtime)
      dimriver = INT(transtime) + 1
    ELSE
      !If regional parameter estimations is used calculate rivvel and damp
      maxtrans = 0.
      DO i = 1,nsubb
        CALL set_regest_parameter(i,n_rivv,localrivvel)
        CALL set_regest_parameter(i,n_damp,localdamp)
        localrivvel = MIN(localrivvel,optrivvel)    !This will give the "worst" combination
        localdamp = MIN(localdamp,optdamp)
        IF(localrivvel==0)THEN !Test for missing rivvel in model set-up
          dimriver = 1
          IF(conductwarning) WRITE(6,*) 'Warning: Parameter rivvel not set. River flow will take one time step.'
          RETURN
        ENDIF
        DO itype = 1,2
          CALL calculate_translation_time(rivlength(itype,i),localrivvel,localdamp,totaltime,transtime)
          maxtrans = MAX(maxtrans,transtime)
        ENDDO
      ENDDO
      dimriver = INT(maxtrans) + 1
    ENDIF

  END SUBROUTINE calculate_special_model_parameters

!>Calculate delay times in rivers
!------------------------------------------------------------
  SUBROUTINE calculate_translation_time(rivlen,rivvel,damp,totaltime,transtime)

    !Argument declarations
    REAL, INTENT(IN)  :: rivlen !<river length (m)
    REAL, INTENT(IN)  :: rivvel !<river velocity (m3/s)
    REAL, INTENT(IN)  :: damp   !<damp parameter
    REAL, INTENT(OUT) :: totaltime   !<total delay time of river (timesteps)
    REAL, INTENT(OUT) :: transtime   !<translation time of river (timesteps)

    totaltime = rivlen / rivvel / seconds_per_timestep
    transtime = (1. - damp) * totaltime

  END SUBROUTINE calculate_translation_time

  !>Set model configuration data from input parameters; lake
  !>depth and catchment area of ilake
  !--------------------------------------------------------
  SUBROUTINE set_modelconfig_from_parameters()

    USE GLACIER_SOILMODEL, ONLY : get_glacier_parameters

    !Variable declarations
    INTEGER i,j
    INTEGER itype

    !>\b Algorithm \n
    !Change soil layer depth and thicknesses from soilcorr parameter
    IF(simulate%soildepthstretch)THEN
      !>Calculate new soil layer thickness (m)
      DO j = 1,nclass
        IF(landpar(m_soilstretch,classdata(j)%luse)>0.)THEN
          soilthick(2,j) = (classdata(j)%soildepth(2) - classdata(j)%soildepth(1)) * landpar(m_soilstretch,classdata(j)%luse)
          soilthick(3,j) = (classdata(j)%soildepth(3) - classdata(j)%soildepth(2)) * landpar(m_soilstretch,classdata(j)%luse)
          soildepth(2,j) = soildepth(1,j) + soilthick(2,j)
          soildepth(3,j) = soildepth(2,j) + soilthick(3,j)
        ENDIF
      ENDDO
    ENDIF

    !Set ilake lakedepth/icatch from parameter inputs (if not set already by GeoData input)
    IF(slc_ilake>0)THEN
      itype = 1
      DO i = 1,nsub
        !>Set local lake depth from regional or general parameter
        IF(basin(i)%parregion(regiondivision(m_ilrldep))>0) basin(i)%lakedepth(itype) = regpar(m_ilrldep,basin(i)%parregion(regiondivision(m_ilrldep)))
        IF(basin(i)%lakedepth(itype)<=0.) basin(i)%lakedepth(itype) = genpar(m_gldepi)
        !>Replace local lake with hgdm model, if option set and depth value given.
        IF((modeloption(p_connectivity)==2.OR.modeloption(p_connectivity)==3))THEN
          IF(basin(i)%hgdmdepth>0.)THEN
            basin(i)%lakedepth(itype) = basin(i)%hgdmdepth  !Priority
          ELSE
            IF(basin(i)%parregion(regiondivision(m_ilhgdmdep))>0)THEN
              IF(regpar(m_ilhgdmdep,basin(i)%parregion(regiondivision(m_ilhgdmdep)))>0.)THEN
                basin(i)%hgdmdepth = regpar(m_ilhgdmdep,basin(i)%parregion(regiondivision(m_ilhgdmdep)))    !Set to be used as flag
                basin(i)%lakedepth(itype) = regpar(m_ilhgdmdep,basin(i)%parregion(regiondivision(m_ilhgdmdep)))
              ENDIF
            ENDIF
          ENDIF
        ENDIF
        !>Set local lake location (icatch) from parameters.
        IF(simulate%ilakecatpar(i))THEN !flagged to use parameter value
          IF(basin(i)%parregion(regiondivision(m_ilricatch))>0) basin(i)%ilakecatch = regpar(m_ilricatch,basin(i)%parregion(regiondivision(m_ilricatch))) !second choice
          IF(basin(i)%ilakecatch<=0.) basin(i)%ilakecatch = genpar(m_gicatch) !second choice
          IF(basin(i)%ilakecatch<=0.) basin(i)%ilakecatch = 1.  !default value
        ENDIF
      ENDDO
    ENDIF
    !>Set olake lakedepth from parameter inputs (if not set already by GeoData/LakeData/DamData input)
    IF(slc_olake>0)THEN
      itype = 2
      DO i = 1,nsub
        IF(simulate%olakedeppar(i))THEN
          IF(basin(i)%parregion(regiondivision(m_olrldep))>0) basin(i)%lakedepth(itype) = regpar(m_olrldep,basin(i)%parregion(regiondivision(m_olrldep))) !second choice
          IF(basin(i)%lakedepth(itype)<=0.) basin(i)%lakedepth(itype) = genpar(m_gldepo)
        ENDIF
      ENDDO
    ENDIF

    IF(conduct%glacier) CALL get_glacier_parameters(nclass,classmodel)

  END SUBROUTINE set_modelconfig_from_parameters

  !>Initiates the model state for a simulation with no saved states.
  !-----------------------------------------
  SUBROUTINE initiate_model_state(frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate)

    USE GLACIER_SOILMODEL, ONLY :   initiate_glacier_state
    USE SOIL_PROCESSES, ONLY :      initiate_soil_water_state
    USE NPC_SOIL_PROCESSES, ONLY :  initiate_soil_npc_state
    USE SURFACEWATER_PROCESSES, ONLY : sum_upstream_area, &
                                       calculate_landarea, &
                                       calculate_riverlength
    USE NPC_SURFACEWATER_PROCESSES, ONLY : initiate_river_npc_state,     &
                                           initiate_lake_npc_state
    USE REGIONAL_GROUNDWATER_MODULE, ONLY : initiate_aquifer_state

    !Argument declarations
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT)    :: soilstate   !<Soil states
    TYPE(aquiferstatetype),INTENT(INOUT) :: aquiferstate   !<Aquifer states
    TYPE(riverstatetype),INTENT(INOUT)   :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT)    :: lakestate   !<Lake states
    TYPE(miscstatetype),INTENT(INOUT)    :: miscstate   !<Misc states

    !Variable declarations
    INTEGER i,k     !loop-variable
    INTEGER nummaxlayers   !maximum number of soillayers in this model set up

    !Parameter declarations
    REAL, PARAMETER :: seconds_per_day = 86400.

    !Existence of soillayers
    IF(MAXVAL(soilthick(3,1:nclass))>0.)THEN
      nummaxlayers = 3
    ELSEIF(MAXVAL(soilthick(2,1:nclass))>0.)THEN
      nummaxlayers = 2
    ELSE
      nummaxlayers = 1
    ENDIF

    !Initiate states to zero
    CALL initiate_state_zero(statesize,conduct,frozenstate,soilstate, &
                             aquiferstate,riverstate,lakestate,miscstate)

    !Initiate soil water state variables and soil water processes help parameters.
    CALL initiate_soil_water_state(soilstate)

    !Initialize glacier volume
    IF(conduct%glacier) CALL initiate_glacier_state(nclass,classmodel,frozenstate)

    !Initialize lake state for pure water model simulation to total lake volume
    DO i = 1,nsub
!      IF(slc_ilake>0) lakestate%water(1,i) = basin(i)%lakedepth(1)*1000.  !ilake water stage (mm)
      IF(slc_ilake>0) THEN
        lakestate%water(1,i) = basin(i)%lakedepth(1)*1000.  !ilake water stage (mm), at threshold
        !lake section volume fraction initialized by fractional area in each section
        IF(conduct%connectivity)THEN
          IF(basin(i)%lakesection>0)THEN
            DO k=1,basin(i)%lakesection
              lakestate%volfrac(k,i) = lakesectiondata(i,k)%farea
            ENDDO
          ELSEIF(basin(i)%hgdmdepth>0.)THEN
            lakestate%volfrac(1,i) = 1.0 ! set to be full for HGDM MIA (only used 1st lake section)
            lakestate%fcarea(i) = 1.0 ! set to be full for HGDM MIA !CP220231
          !ELSE
            !lakestate%volfrac(:,i) = 0.  !Not used
          ENDIF
        ENDIF
      ENDIF
      !************
      ! set variables to be full ( used by HGDM)
      ! the most downstream depression (1) is used to store since all depressions
      ! are lumped in HGDM
	  ! if(HGDM is active) then
      !lakestate%volfrac(1,i) = 1.0 ! set to be full for HGDM MIA
	  !lakestate%fcarea(1,i) = 1.0 ! set to be full for HGDM MIA
      !************
      IF(slc_olake>0)THEN
        lakestate%water(2,i) = basin(i)%lakedepth(2) * 1000.         !ordinary olake water stage (mm)
        IF(ALLOCATED(damindex))THEN
          IF(damindex(i)>0)THEN
            IF(dam(damindex(i))%purpose==3) THEN
              !dam for flood control start "empty"
              lakestate%water(2,i) = lakestate%water(2,i) - dam(damindex(i))%regvol*1000000./(classbasin(i,slc_olake)%part*basin(i)%area)*1000.
            ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    !Initialize river states
    IF(.NOT. ALLOCATED(landarea))     ALLOCATE(landarea(nsub))
    IF(.NOT. ALLOCATED(riverlength))  ALLOCATE(riverlength(2,nsub))
    IF(.NOT. ALLOCATED(deadriver))    ALLOCATE(deadriver(2,nsub))
    IF(.NOT. ALLOCATED(upstreamarea)) ALLOCATE(upstreamarea(nsub))
    CALL sum_upstream_area(nsub,upstreamarea)             !calculates the area upstream of the outlet point of catchment
    CALL calculate_landarea(nsub,landarea)
    CALL calculate_riverlength(nsub,landarea,riverlength)
    DO i = 1,nsub
      deadriver(1,i) = genpar(m_deadl) * (basin(i)%area/1.0E6) * riverlength(1,i)
      deadriver(2,i) = genpar(m_deadm) * (upstreamarea(i)/1.0E6) * riverlength(2,i)
!      deadriver(2,i) = genpar(m_deadm) * ((basin(i)%area+upstreamarea(i))/1.0E6) * riverlength(2,i)  !basin%area already included in upstreamarea
      riverstate%water(:,i) = deadriver(:,i)    !initialize river damping box to deadvolume (m3)
    ENDDO

    !Initiate average discharge variable, Qmeani = 365 day mean Q before damping
    DO i = 1,nsub
      IF(ALLOCATED(lakedatapar))THEN
        riverstate%Qmean(1,i) = lakedatapar(lakedataparindex(i,1),m_ldqmean)* 0.001 * basin(i)%area / (365. * seconds_per_day)
        riverstate%Qmean(2,i) = lakedatapar(lakedataparindex(i,2),m_ldqmean)* 0.001 * upstreamarea(i) / (365. * seconds_per_day)
      ELSE
        riverstate%Qmean(1,i) = genpar(m_Qmean)* 0.001 * basin(i)%area / (365. * seconds_per_day)
        riverstate%Qmean(2,i) = genpar(m_Qmean)* 0.001 * upstreamarea(i) / (365. * seconds_per_day)
      ENDIF
    ENDDO


    !Initialize soil nutrient variables
    CALL initiate_soil_npc_state(conduct%simN,conduct%simP,conduct%simC,nummaxlayers,soilstate)

    !Initialize soil for tracer concentration
    IF(simulate%substance(i_t1))THEN
      soilstate%conc(i_t1,1,:,:)  = genpar(m_iniT1)
      IF(nummaxlayers>1) soilstate%conc(i_t1,2,:,:) = genpar(m_iniT1)
      IF(nummaxlayers==3) soilstate%conc(i_t1,3,:,:) = genpar(m_iniT1)
    ENDIF
    IF(simulate%substance(i_t2))THEN
      soilstate%conc(i_t2,1,:,:)  = genpar(m_iniT2)
      soilstate%temp(1,:,:) = genpar(m_iniT2)
      soilstate%deeptemp(:,:)    = genpar(m_iniT2)
      IF(nummaxlayers>1)THEN
        soilstate%conc(i_t2,2,:,:) = genpar(m_iniT2)
        soilstate%temp(2,:,:) = genpar(m_iniT2)
      ENDIF
      IF(nummaxlayers==3)THEN
        soilstate%conc(i_t2,3,:,:) = genpar(m_iniT2)
        soilstate%temp(3,:,:) = genpar(m_iniT2)
      ENDIF
    ENDIF

    !Initiate river variables
    CALL initiate_river_npc_state(conduct%simN,conduct%simP,conduct%simC,conduct%simS,conduct%qbank,riverstate)

    !Initialize lake concentrations
    CALL initiate_lake_npc_state(conduct%simN,conduct%simP,conduct%simC,conduct%simS,lakestate)

    !Initialize surface water for tracer concentration (lakestate T2 is set in initiate_lake_npc_state)
    IF(simulate%substance(i_t1))THEN
      riverstate%conc(i_t1,:,:)  = genpar(m_iniT1sw)
      riverstate%cqueue(i_t1,:,:,:)  = genpar(m_iniT1sw)
      lakestate%conc(i_t1,:,:) = genpar(m_iniT1sw)
    ENDIF
    IF(simulate%substance(i_t2)) riverstate%cqueue(i_t2,:,:,:) = genpar(m_iniT2)
    !Why is riverstate%conc(i_t2) missing?

    !Initiate wetland variable
    IF(conduct%riverwetland)THEN
      IF(simulate%substance(i_t2)) riverstate%cwetland(i_t2,:,:) = genpar(m_iniT2)
      !Why is iniT1 missing?
    ENDIF

    !Initiate aquifer state variables
    IF(modeloption(p_deepgroundwater)==2) CALL initiate_aquifer_state(aquiferstate)

  END SUBROUTINE initiate_model_state

  !>\brief Initiates model variables and parameters for a simulation.
  !>This include calculating HYPE variables for holding model set-up
  !>information and more...
  !
  !> \b Consequences Module modvar variables soildepth and soilthick may change.
  !--------------------------------------------------------------------------
  SUBROUTINE initiate_model(frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate)

    USE GLACIER_SOILMODEL, ONLY : initiate_glacier, &
                                  initiate_glacier_state
    USE SOIL_PROCESSES, ONLY : initiate_soil_water
    USE SURFACEWATER_PROCESSES, ONLY : sum_upstream_area, &
                                       set_general_rating_k,  &
                                       calculate_landarea, &
                                       calculate_riverlength, &
                                       set_lake_outlets, &
                                       initiate_lakeriverice
    USE IRRIGATION_MODULE, ONLY :   initiate_irrigation
    USE REGIONAL_GROUNDWATER_MODULE, ONLY : initiate_regional_groundwater_flow, &
                                            initiate_aquifer_model, &
                                            calculate_delayed_water
    USE ATMOSPHERIC_PROCESSES, ONLY : calculate_class_wind_transformation_factor, &
                                      set_atmospheric_parameters_corrections
    USE HYPE_INDATA, ONLY : set_regest_parameter, &
                            deallocate_regest_input_variables

    !Argument declarations
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT)    :: soilstate   !<Soil states
    TYPE(aquiferstatetype),INTENT(INOUT) :: aquiferstate  !<Aquifer states
    TYPE(riverstatetype),INTENT(INOUT)   :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT)    :: lakestate   !<Lake states
    TYPE(miscstatetype),INTENT(INOUT)    :: miscstate   !<Misc states

    !Variable declarations
    INTEGER i,j,k     !loop-variable
    INTEGER itype     !loop-variable, river type
    INTEGER inputdefiningpetmodel
    REAL help
    REAL vel,part,totaltime,kt  !parameters and help variables for river transport
    REAL delayedwater(naquifers)        !water in delay to reach aquifer (m3)
    REAL localrivvol(nsub)  !help variable for calculating local river initial volume
    REAL mainrivvol(nsub)   !help variable for calculating main river initial volume
    REAL landfraction(nsub)   !help variable for calculating iwet fraction


    !Flag for radiation and humidity calculations
    calcSWRAD = .FALSE.
    calcVAPOUR = .FALSE.
    calcWIND = .FALSE.
    IF(modeloption(p_snowmelt).GE.2) calcSWRAD = .TRUE.
    IF(conduct%lakeriverice) calcSWRAD = .TRUE.
    IF(conductbasinpetmodel)THEN
      inputdefiningpetmodel = MAXVAL(petmodel)
    ELSE
      inputdefiningpetmodel = modeloption(p_petmodel)
    ENDIF
    IF(inputdefiningpetmodel.GT.1)THEN
      calcVAPOUR = .TRUE.
      calcSWRAD = .TRUE.
    ENDIF
    IF(inputdefiningpetmodel.EQ.5)THEN
      calcWIND = .TRUE.
      IF(ALLOCATED(windi)) CALL calculate_class_wind_transformation_factor(windtrans)
    ENDIF

    !Set flag for T1 simulation with typical soil leakage
    T1leakage = .FALSE.
    IF(simulate%substance(i_t1))THEN
      help = SUM(landpar(m_t1leaklu,:))+SUM(soilpar(m_t1leakst,:))
      IF(help>0.)THEN
        T1leakage = .TRUE.
      ENDIF
    ENDIF

    !Set precipitation parameter corrections
    CALL set_atmospheric_parameters_corrections()

    !Set potential evaporation correction (not glacier?)
    IF(.NOT.ALLOCATED(basincevpcorr)) ALLOCATE(basincevpcorr(nsub))
    basincevpcorr = 1.
    DO i = 1,nsub
      IF(basin(i)%parregion(regiondivision(m_cevpcorr))>0)THEN
        basincevpcorr(i) = 1. + regpar(m_cevpcorr,basin(i)%parregion(regiondivision(m_cevpcorr)))
      ENDIF
      !Replace parameter values with regional parameter estimates
      IF(conductregest) CALL set_regest_parameter(i,n_cevpc,basincevpcorr(i),1.)
    ENDDO

    !Initiate soil layer and water processes help parameters.
    CALL initiate_soil_water()

    !Re-initialize glacier volume
    IF(conduct%glacier .AND. modeloption(p_glacierini)==1) CALL initiate_glacier_state(nclass,classmodel,frozenstate)

    !Initialize glacier parameters and type
    IF(conduct%glacier) CALL initiate_glacier(nclass,classmodel)

    !Initiate soil temperature parameters and variables
    avertemp =(/5.,10.,20.,30./)*timesteps_per_day    !Number of timestep over which meantemp is calculated
    IF(.NOT.ALLOCATED(soilmem)) ALLOCATE(soilmem(maxsoillayers,nclass))
    soilmem = 0.
    DO j= 1,nclass
      DO k = 1,maxsoillayers
        IF(k>1)THEN
          soilmem(k,j) = timesteps_per_day*landpar(m_surfmem,classdata(j)%luse)*EXP(landpar(m_depthrel,classdata(j)%luse)*(soildepth(k-1,j)+(soilthick(k,j) / 2.)))
        ELSE
          soilmem(k,j) = timesteps_per_day*landpar(m_surfmem,classdata(j)%luse)*EXP(landpar(m_depthrel,classdata(j)%luse)*(soilthick(k,j) / 2.))
        ENDIF
      ENDDO
    ENDDO

    !Initialize lake and river
    IF(.NOT. ALLOCATED(landarea))     ALLOCATE(landarea(nsub))
    IF(.NOT. ALLOCATED(riverlength))  ALLOCATE(riverlength(2,nsub))
    IF(.NOT. ALLOCATED(deadriver))    ALLOCATE(deadriver(2,nsub))
    IF(.NOT. ALLOCATED(upstreamarea)) ALLOCATE(upstreamarea(nsub))
    CALL sum_upstream_area(nsub,upstreamarea)             !calculates the area upstream of the outlet point of catchment
    CALL calculate_landarea(nsub,landarea)
    CALL calculate_riverlength(nsub,landarea,riverlength)
    DO i = 1,nsub
      deadriver(1,i) = genpar(m_deadl) * (basin(i)%area/1.0E6) * riverlength(1,i)
      deadriver(2,i) = genpar(m_deadm) * (upstreamarea(i)/1.0E6) * riverlength(2,i)
!      deadriver(2,i) = genpar(m_deadm) * ((basin(i)%area+upstreamarea(i))/1.0E6) * riverlength(2,i)
    ENDDO
    IF(.NOT. ALLOCATED(deadwidth))    ALLOCATE(deadwidth(2,nsub))
    IF(.NOT. ALLOCATED(ratingk))      ALLOCATE(ratingk(2,nsub))
    DO i = 1,nsub
      deadwidth(1,i) = SQRT(genpar(m_deadl) * (basin(i)%area/1.0E6)*0.1)*10.   !(m)  !width=10*depth, width = sqrt(area/10)
      deadwidth(2,i) = SQRT(genpar(m_deadm) * (upstreamarea(i)/1.0E6)*0.1)*10.   !(m)  !width=10*depth, width = sqrt(area/10)
!      deadwidth(2,i) = SQRT(genpar(m_deadm) * ((basin(i)%area+upstreamarea(i))/1.0E6)*0.1)   !(m)  !width=10*depth, width = sqrt(area/10)
    ENDDO
    CALL set_general_rating_k(2,nsub,landarea,upstreamarea,ratingk)

    !Initialise lake river ice model
    IF(conduct%lakeriverice) CALL initiate_lakeriverice()

    !Initialize internal landclass wetland (iwet).
    IF(slc_iwet>0)THEN
      IF(.NOT.ALLOCATED(iwetnoninflow)) ALLOCATE(iwetnoninflow(nsub))
      !iwetcatch is fraction of basinarea
      landfraction = 1.
      landfraction = landfraction - classbasin(:,slc_iwet)%part
      IF(slc_ilake>0)  landfraction = landfraction - classbasin(:,slc_ilake)%part
      IF(slc_lriver>0) landfraction = landfraction - classbasin(:,slc_lriver)%part
      IF(slc_olake>0)  landfraction = landfraction - classbasin(:,slc_olake)%part !floodplain not included in flow to local river
      IF(slc_mriver>0) landfraction = landfraction - classbasin(:,slc_mriver)%part !floodplain not included in flow to local river
      IF(slc_owet>0) landfraction = landfraction - classbasin(:,slc_owet)%part
      iwetnoninflow = 1.- basin%iwetcatch/landfraction  !fraction of runoff
      WHERE(iwetnoninflow<0.) iwetnoninflow = 0.    !Check in test.
    ENDIF

    !No initiation of floodwater to zero here, have inital value from state-file.
    !Calculate floodplain reference level (deepest bottom of floodplain), if not set in file
    IF(ALLOCATED(floodindex))THEN
      DO i = 1,nsub
        IF(floodindex(i)>0)THEN
          IF(flooding(floodindex(i))%hrefl==missing_value)THEN
            IF(slc_olake>0) flooding(floodindex(i))%hrefl = basin(i)%elev + classbasin(i,slc_olake)%deltah - basin(i)%lakedepth(2) + flooding(floodindex(i))%floll - flooding(floodindex(i))%flolp
          ENDIF
          IF(flooding(floodindex(i))%hrefr==missing_value)THEN
            IF(slc_mriver>0)THEN
              help = 0.   !river depth
              IF(deadriver(2,i)>0.) help = (basin(i)%area*classbasin(i,slc_mriver)%part*(1.-flooding(floodindex(i))%fpfmr))/deadriver(2,i)
              flooding(floodindex(i))%hrefr = basin(i)%elev + classbasin(i,slc_mriver)%deltah - help + flooding(floodindex(i))%flmrr - flooding(floodindex(i))%flmrp
            ENDIF
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    !Calculate river transport time parameters
    IF(.NOT. ALLOCATED(transtime)) ALLOCATE(transtime(2,nsub))
    IF(.NOT. ALLOCATED(ttstep))    ALLOCATE(ttstep(2,nsub))
    IF(.NOT. ALLOCATED(ttpart))    ALLOCATE(ttpart(2,nsub))
    IF(.NOT. ALLOCATED(riverrc))   ALLOCATE(riverrc(2,nsub))
    vel = genpar(m_rivvel)                          !peak velocity of river (m/s)
    IF((.NOT.conductregest).AND.vel==0)THEN
      transtime = 0.
      ttstep = 0
      ttpart = 0.
      riverrc = 1.
    ELSE
      part = genpar(m_damp)                           !part of delay from damping
      DO i = 1,nsub
        !>Replace parameter values with regional parameter estimates
        IF(conductregest)THEN
          CALL set_regest_parameter(i,n_rivv,vel)
          CALL set_regest_parameter(i,n_damp,part)
        ENDIF
        IF(vel<=0.001)THEN
          IF(conductwarning) WRITE(6,*) 'Warning: river velocity parameter less than 0.001 m/s. Set to 0.001 m/s.'
          vel = 0.001
        ENDIF
        IF(part>1.)THEN
          IF(conductwarning) WRITE(6,*) 'Warning: damping parameter larger than one. Was set to one.'
          part = 1.
        ELSEIF(part<0.)THEN
          IF(conductwarning) WRITE(6,*) 'Warning: damping parameter less than zero. Was set to zero.'
          part = 0.
        ENDIF
        DO itype = 1,2
          CALL calculate_translation_time(riverlength(itype,i),vel,part,totaltime,transtime(itype,i))
          ttstep(itype,i) = INT(transtime(itype,i))
          ttpart(itype,i) = transtime(itype,i) - REAL(ttstep(itype,i))
          kt = part * totaltime                    !damptime, k in equation q=(1/k)*S
          IF(kt>0.)THEN
            riverrc(itype,i) = 1. - kt + kt * exp(-1./kt)  !recession coefficient in equation Q=r*S
          ELSE
            riverrc(itype,i) = 1.   !Safe for all lake subbasin
          ENDIF
        ENDDO
      ENDDO
    ENDIF

    !Check of dimension river queue
    help = SIZE(riverstate%qqueue,DIM=1) !number of elements (0:ml)
    DO i = 1,nsub
      DO itype=1,2
        IF(help-1<ttstep(itype,i))THEN
          WRITE(6,*) 'Error: dimension queue',itype,i,ttstep(itype,i),help
        ENDIF
      ENDDO
    ENDDO

    !Set river bankful variables
    IF(conduct%qbank) CALL set_Qvariables_for_bankfulflow(nsub,riverstate%Q365)

    !Initialize lake parameters
    CALL set_lake_outlets()

    !Initiate regional groundwater flow/aquifer
    IF(modeloption(p_deepgroundwater)==1)THEN
      CALL initiate_regional_groundwater_flow(nsub,numsubstances,slc_ilake,slc_olake,slc_lriver,slc_mriver,slc_iwet,slc_owet)
    ELSEIF(modeloption(p_deepgroundwater)==2)THEN
      CALL initiate_aquifer_model(nsub,numsubstances,naquifers)
    ENDIF

    !Allocate and initiate output variable
    IF(.NOT. ALLOCATED(accdiff)) ALLOCATE(accdiff(nsub))
    accdiff = 0.

    !Allocate nutrient (NP) load variables
    IF(conductload)THEN
       IF(.NOT.ALLOCATED(Latmdep))  ALLOCATE(Latmdep(nclass,2,numsubstances))   !Substances in order IN, ON, SP, PP
       IF(.NOT.ALLOCATED(Lcultiv))  ALLOCATE(Lcultiv(nclass,2,numsubstances))   !1=fertiliser, 2=plantdecay
       IF(.NOT.ALLOCATED(Lirrsoil)) ALLOCATE(Lirrsoil(nclass,numsubstances))    !irrigation on soil
       IF(.NOT.ALLOCATED(Lrurala))  ALLOCATE(Lrurala(nclass,numsubstances))     !rural a
       IF(.NOT.ALLOCATED(Lstream))  ALLOCATE(Lstream(nclass,numsubstances))     !runoff from soil to stream
       IF(.NOT.ALLOCATED(Lpathway)) ALLOCATE(Lpathway(numsubstances,npathway))  !class independent, load at points A to R along flow pathway.
       IF(.NOT.ALLOCATED(Lbranch))  ALLOCATE(Lbranch(numsubstances))            !part of outflow
       IF(.NOT.ALLOCATED(Lgrwmr))   ALLOCATE(Lgrwmr(numsubstances))             !reg.grw to main river
       IF(.NOT.ALLOCATED(Lgrwol))   ALLOCATE(Lgrwol(numsubstances))             !reg.grw to olake
       IF(.NOT.ALLOCATED(Ltransf))  ALLOCATE(Ltransf(numsubstances))            !water transfer to main river
    ENDIF
    IF(.NOT.ALLOCATED(Lpoints))  ALLOCATE(Lpoints(numsubstances,max_pstype)) !point sources 1=ps1, 2=ps2, 3=ps3, need to be allocated for subroutine call
    IF(simulatesubstances)THEN
      IF(.NOT.ALLOCATED(Lruralb))  ALLOCATE(Lruralb(numsubstances))            !rural b
      IF(.NOT.ALLOCATED(Lgrwsoil)) ALLOCATE(Lgrwsoil(nclass,numsubstances))       !regional groundwaterflow to soil
      IF(.NOT.ALLOCATED(Lgrwclass)) ALLOCATE(Lgrwclass(nclass,numsubstances,nsub))       !regional groundwater outflow from soil
    ENDIF

    !Initiate irrigation
    IF(conduct%irrigation) CALL initiate_irrigation(nsub,nclass)

    !Initate calculations for water balance output
    IF(conductwb)THEN
      CALL initiate_waterbalance_output(nsub)
      !set initial wb_stores
      wbstores = 0.
      DO i = 1,nsub
        DO j = 1,nclass
          IF(classmodel(j)==glacier_model) wbstores(w_glacier,i) = frozenstate%glacvol(i)*genpar(m_glacdens)
          IF(classmodel(j)==0.OR.classmodel(j)==glacier_model)THEN
            wbstores(w_snow,i) = wbstores(w_snow,i) + frozenstate%snow(j,i)*classbasin(i,j)%part
            wbstores(w_soil1,i) = wbstores(w_soil1,i) + soilstate%water(1,j,i)*classbasin(i,j)%part
            wbstores(w_soil2,i) = wbstores(w_soil2,i) + soilstate%water(2,j,i)*classbasin(i,j)%part
            wbstores(w_soil3,i) = wbstores(w_soil3,i) + soilstate%water(3,j,i)*classbasin(i,j)%part
          ENDIF
          IF(j==slc_iwet) wbstores(w_iwet,i) = wetland_wbstore(slc_iwet,i,frozenstate,soilstate)
          IF(j==slc_owet) wbstores(w_owet,i) = wetland_wbstore(slc_owet,i,frozenstate,soilstate)
          IF(conduct%floodplain)THEN
            IF(floodindex(i)>0)THEN
              IF(j==slc_mriver.AND.flooding(floodindex(i))%fpfmr>0.)THEN
                wbstores(w_snow,i) = wbstores(w_snow,i) + frozenstate%snow(j,i)*classbasin(i,j)%part*flooding(floodindex(i))%fpfmr
                wbstores(w_soil1,i) = wbstores(w_soil1,i) + soilstate%water(1,j,i)*classbasin(i,j)%part*flooding(floodindex(i))%fpfmr
                wbstores(w_soil2,i) = wbstores(w_soil2,i) + soilstate%water(2,j,i)*classbasin(i,j)%part*flooding(floodindex(i))%fpfmr
                wbstores(w_soil3,i) = wbstores(w_soil3,i) + soilstate%water(3,j,i)*classbasin(i,j)%part*flooding(floodindex(i))%fpfmr
              ENDIF
              IF(j==slc_olake.AND.flooding(floodindex(i))%fpfol>0.)THEN
                wbstores(w_snow,i) = wbstores(w_snow,i) + frozenstate%snow(j,i)*classbasin(i,j)%part*flooding(floodindex(i))%fpfol
                wbstores(w_soil1,i) = wbstores(w_soil1,i) + soilstate%water(1,j,i)*classbasin(i,j)%part*flooding(floodindex(i))%fpfol
                wbstores(w_soil2,i) = wbstores(w_soil2,i) + soilstate%water(2,j,i)*classbasin(i,j)%part*flooding(floodindex(i))%fpfol
                wbstores(w_soil3,i) = wbstores(w_soil3,i) + soilstate%water(3,j,i)*classbasin(i,j)%part*flooding(floodindex(i))%fpfol
              ENDIF
            ENDIF
          ENDIF
          IF(conduct%irrigation) wbstores(w_irrcanal,i) = wbstores(w_irrcanal,i) + miscstate%nextirrigation(j,i)*classbasin(i,j)%part
        ENDDO
        localrivvol(i) = riverstate%water(1,i) + (SUM(riverstate%qqueue(1:ttstep(1,i),1,i)) + riverstate%qqueue(ttstep(1,i)+1,1,i) * ttpart(1,i))
        mainrivvol(i)  = riverstate%water(2,i) + (SUM(riverstate%qqueue(1:ttstep(2,i),2,i)) + riverstate%qqueue(ttstep(2,i)+1,2,i) * ttpart(2,i))
      ENDDO
      wbstores(w_snow,:)   = wbstores(w_snow,:)  * basin(:)%area *1.E-3  !m3
      wbstores(w_soil1,:)  = wbstores(w_soil1,:) * basin(:)%area *1.E-3  !m3
      wbstores(w_soil2,:)  = wbstores(w_soil2,:) * basin(:)%area *1.E-3  !m3
      wbstores(w_soil3,:)  = wbstores(w_soil3,:) * basin(:)%area *1.E-3  !m3
      IF(conduct%irrigation) wbstores(w_irrcanal,:) = wbstores(w_irrcanal,:) * basin(:)%area *1.E-3  !m3
      IF(conduct%watertransfer) wbstores(w_wtcanal,:) = miscstate%nexttransfer(:) *seconds_per_timestep !m3
      wbstores(w_iriver,:) = localrivvol(:) !m3
      wbstores(w_mriver,:) = mainrivvol(:)  !m3
      IF(conduct%floodplain)THEN
        DO i = 1,nsub
          IF(floodindex(i)>0)THEN
            IF(flooding(floodindex(i))%fpfmr>0.)THEN
              wbstores(w_riverplain,i) = miscstate%floodwater(1,i)
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      IF(slc_ilake>0) wbstores(w_ilake,:) = lakestate%water(1,:)  !mm
      IF(slc_olake>0) wbstores(w_olake,:)  = lakestate%water(2,:)  !mm
      IF(slc_ilake>0) wbstores(w_ilake,:) = wbstores(w_ilake,:) * basin(:)%area *classbasin(:,slc_ilake)%part *1.E-3  !m3
      IF(slc_olake>0)THEN
        wbstores(w_olake,:) = wbstores(w_olake,:) * basin(:)%area *classbasin(:,slc_olake)%part *1.E-3  !m3
        IF(conduct%floodplain)THEN
          DO i = 1,nsub
            IF(floodindex(i)>0)THEN
              IF(flooding(floodindex(i))%fpfol>0.)THEN
                wbstores(w_olake,i) = wbstores(w_olake,i)*(1.-flooding(floodindex(i))%fpfol)
                wbstores(w_lakeplain,i) = miscstate%floodwater(2,i)
              ENDIF
            ENDIF
          ENDDO
        ENDIF
      ENDIF
      CALL calculate_delayed_water(aquiferstate,naquifers,delayedwater)
      IF(naquifers>0) wbstores(w_aquifer,1:naquifers) = aquiferstate%water + aquiferstate%nextoutflow + delayedwater
      CALL print_initial_waterbalance_stores(nsub,naquifers)
    ENDIF

    IF(conductregest) CALL deallocate_regest_input_variables()

  END SUBROUTINE initiate_model

  !>Model subroutine for HYPE.
  !!Calculates what happen during one timestep.
  !---------------------------------------------------
  SUBROUTINE model(frozenstate,soilstate,aquiferstate,riverstate,lakestate,miscstate)

    USE STATETYPE_MODULE
    USE NPC_SURFACEWATER_PROCESSES, ONLY : add_deposition_to_river_as_load, &
                                           add_deposition_to_lake_as_load, &
                                           np_processes_in_river,  &
                                           np_processes_in_lake,   &
                                           oc_processes_in_river,  &
                                           oc_processes_in_lake,   &
                                           add_diffuse_source_to_local_river,  &
                                           add_point_sources_to_main_river,    &
                                           calculate_river_wetland
    USE SURFACEWATER_PROCESSES, ONLY : add_precipitation_to_river, &
                                       calculate_river_evaporation, &
                                       calculate_actual_lake_evaporation, &
                                       calculate_water_temperature,     &
                                       set_water_temperature, &
                                       calculate_river_characteristics, &
                                       translation_in_river,            &
                                       point_abstraction_from_main_river_inflow, &
                                       point_abstraction_from_main_river, &
                                       point_abstraction_from_outlet_lake, &
                                       water_transfer_from_outlet_lake, &
                                       add_water_transfer_to_main_river, &
                                       calculate_ilake_outflow, &
                                       calculate_ilakesection_outflow, &
                                       !calculate_outflow_from_lakebasin_lake,&
                                       calculate_outflow_from_outlet_lake,     &
                                       calculate_olake_waterstage,      &
                                       calculate_regamp_adjusted_waterstage, &
                                       calculate_branched_flow, &
                                       recalculate_branched_flow, &
                                       calculate_lake_volume, &
                                       calculate_lake_hypolimnion_depth, &
                                       T2_processes_in_river, &
                                       T2_processes_in_lake, &
                                       ice_processes_in_river, &
                                       ice_processes_in_lake, &
                                       add_T2_concentration_in_precipitation_on_water,  &
                                       get_rivertempvol,  &
                                       add_precipitation_to_floodplain, &
                                       calculate_waterbody_floodplain_interflow, &
                                       calculate_floodplain_waterlevel, &
                                       calculate_regional_floodplain_flows, &
                                       calculate_fractional_riverarea, &
                                       wetland_watermodel, &
                                       get_wetland_threshold, &
                                       river_water_level, &
                                       ice_on_river, &
									   calculate_HGDM_depressions_outflow
    USE NPC_SOIL_PROCESSES, ONLY : set_class_precipitation_concentration_and_load,  &
                                   croprotation_soilpoolaverage
    USE TRACER_PROCESSES, ONLY : add_tracer_point_source_to_river,  &
                                 add_tracer_point_source_to_lake,   &
                                 tracer_processes_in_river,  &
                                 tracer_processes_in_lake
    USE IRRIGATION_MODULE, ONLY : initiate_timestep_irrigation,  &
                                  calculate_irrigation
    USE REGIONAL_GROUNDWATER_MODULE, ONLY : calculate_regional_groundwater_flow,  &
                                            calculate_current_flow_from_aquifer, &
                                            add_regional_groundwater_flow_to_olake, &
                                            calculate_river_groundwaterflow_removal, &
                                            calculate_river_floodplain_groundwaterflow_removal, &
                                            calculate_aquifer,  &
                                            add_aquifer_flow_to_river, &
                                            calculate_delayed_water, &
                                            calculate_aquifer_waterlevel, &
                                            calculate_aquifer_conc
    USE SOILMODEL_DEFAULT, ONLY : soilmodel_0
    USE GLACIER_SOILMODEL, ONLY : soilmodel_3, calculate_glacier_massbalance
    USE FLOODPLAIN_SOILMODEL, ONLY : soilmodel_4
    USE SOILMODEL_TRAVELTIME, ONLY : soilmodel_5
    USE ATMOSPHERIC_PROCESSES, ONLY : calculate_class_atmospheric_forcing,  &
                                calculate_subbasin_temperature, &
                                calculate_subbasin_precipitation, &
                                calculate_rain_snow_from_precipitation, &
                                calculate_extraterrestrial_radiation, &
                                set_precipitation_concentration,  &
                                calculate_daylength, &
                                currently_snowing, &
                                calculate_winddirspeed, &
                                calculate_snowfall_distribution, &
                                get_current_cloudiness
    USE SOIL_PROCESSES, ONLY : calculate_potential_evaporation
    USE GENERAL_WATER_CONCENTRATION, ONLY : remove_water,         &
                                            error_remove_water,   &
                                            add_water
    USE HYPE_INDATA, ONLY : get_current_xoms

    !Argument declarations
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT)    :: soilstate   !<Soil states
    TYPE(aquiferstatetype),INTENT(INOUT) :: aquiferstate  !<Aquifer states
    TYPE(riverstatetype),INTENT(INOUT)   :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT)    :: lakestate   !<Lake states
    TYPE(miscstatetype),INTENT(INOUT)    :: miscstate   !<Misc states

    !Variable declarations
    INTEGER i,j,k    !loop-variables: subbasin, class, div.
    INTEGER itype    !loop-variable: ilake=1,olake=2
    INTEGER isl      !loop-variable: soil layer
    INTEGER pseudo_dayno

    REAL a,aadj                 !area fraction of class, floodplain-adjusted land fraction of class
    REAL asum(maxsoillayers)    !sum of land area parts
    REAL divasum(maxsoillayers) !inverse of area sums
    REAL incorr, oncorr    !correction of inorganic and organic nitrogen level
    REAL phoscorr     !correction of phosphorus level

    !Help variables for class summation to subbasin output
    REAL eacti,runoffi,snowi
    REAL runf,loadrf(numsubstances),concrf(numsubstances)         !(mm,kg/km2,mg/L)
    REAL snowdepthi
    REAL soilwateri(maxsoillayers),standingwater
    REAL qcinfli
    REAL runofftsi,crunofftsi(numsubstances)  !Runoff to stream (mm,mg/L)
    REAL snowcov
    REAL pcorricep(nsub)
    REAL soillgrossload(3,numsubstances), soillnetload(3,numsubstances)   !1=sl 1-2, 2=sl 3, 3=sl3+tile

    !Current forcing variables for estimating radiation for snow and evaporation calculations
    REAL radexti(nsub)           !Extraterrestrial solar radiation [MJ/m2/day] for current time step
    REAL cloudi(nsub)            !cloudiness (-) for current time step
    REAL tmin,tmax,rhmin,netrad,actvap,satvap,wind
    REAL swrad,swpot     !shortwave radiation downward [MJ/m2/day], clearsky radiation
    REAL daylength  !day length (hours)

    !Variables for class values
    REAL classarea            !class area (km2)
    REAL lakeareakm2
    REAL gwat,epot,evap,evapsnow,epotsnow   !calculated class values
    REAL totalsurfaceflow,surfaceflow(2),icpevap
    REAL nitrif, denitrif(maxsoillayers)
    REAL cropuptake          !calculate class values, uptake of IN
    REAL runoffd,crunoffd (numsubstances)         !calculated class values
    REAL crunoff1(numsubstances),crunoff2(numsubstances), crunoff3(numsubstances)   !<calculated class values (mg/L)
    REAL csrunoff(numsubstances)
    REAL cevap(numsubstances)   !calculated class values (mg/L)
    REAL cprec(numsubstances),cprecj(numsubstances)   !concentration of precipitation, subbasin and class
    REAL evapl,cevapl(numsubstances)  !evaporation lake
    REAL temp                 !class elevation corrected temperature
    REAL precorg(nsub) !precipitation original from Pobs.txt (mm)
    REAL prec          !class elevation corrected precipitation (mm,m3) (and snowfall-distributed)
    REAL smdef    !soil moisture deficit
    REAL frostdepth                    !soil frost variables
    REAL glac_part        !glacier fraction of glacier class (-)
    REAL snowfall, rainfall  !Precipiation as snow, rain (mm)
    REAL snowplain_area_fraction, old_snow_part
    REAL atmdepload1(numsubstances),atmdepload1_temp(numsubstances),atmdepload2(numsubstances)  !wet dep (including temp variable used to add dep as load to dep with rain) and dry dep
    REAL irrload(numsubstances)
    REAL grwloadlake(numsubstances),grwloadmr(numsubstances)
    REAL rgrwload(numsubstances)
    REAL ruralaload(numsubstances)
    REAL cultivload(2,numsubstances)
    REAL soil4runoff(2)       !runoff from floodplain :1=from main river fp,2=from olake fp
    REAL infiltrationflows(7) !fraction from snow melt,infiltration,surface runoff,macroporeflow to soil layer 1,2,and 3,fraction from glacier melt
    REAL glacierflows(2)      !flow snow to glacier, precipitation on glacier
    REAL floodplainflows(3)   !snow incorporation to floodplain, precipitation on floodplain,infiltration [m3]
    REAL evapflows(4)         !flow evapotranspiration (land classes), 1=sl1, 2=sl2,3=snow,4=glacier
    REAL runofflows(7)        !different runoff flows:1-3=soil runoff sl 1-3,4-6=tile runoff sl 1-3,7=saturated surface runoff
    REAL verticalflows(6)     !vertical flows:1-2=percolation,3-4=upwelling due to rural,5-6=upwelling due to reg. grw flows
    REAL cverticalflows(2,numsubstances) !<concentration of vertical flows:1-2=percolation
    REAL horizontalflows(3)   !horizontal flows:1-3=recieved rural load flow
    REAL horizontalflows2(3,nsub)   !horizontal flows:1-3=division of regional groundwater flows to grwdown
    REAL cruralflow(numsubstances)   !concentration of rural load flow
    REAL nutrientloadflows(6) !1=rural load flow to local stream,2=point source to main river,3=abstraction from main river,4=abstraction from outlet lake,5=transfer to main river, 6=transfer from olake
    REAL irrigationflows(7)   !withdrawal from 1=deep groundwater(unlimited),2=internal lake,3=outlet lake,4=main river volume and inflow,5=local evaporation losses (local sources),6=unlimited external source (alternative),7=akvifer(modelled)
    REAL regionalirrflows(4,nsub) !withdrawal from 1=outlet lake,2=main river volume and inflow,3=evaporation losses olake as regional source,4=evaporation losses main river as regional source
    REAL regionalirrevap(nsub)    !local evaporation losses (regional source)
    REAL glacperiod,computedMassBalance,computedMBArea  !mass balance of glacier
    REAL snowtemp,snowsurftemp !snow temperature and snow surface temperature

    !Variables for routing
    REAL aolake   !Area fraction for olake (used for check if olake present)
    REAL accinflow(nsub)                  !accumulated upstream inflow to subbasin (m3/s)
    REAL acccinflow(numsubstances,nsub)   !concentration of upstream inflow (mg/L)
    REAL mainflow,branchflow              !divided discharge for output (m3/s)
    REAL qinmm,qin,cin(numsubstances)
    REAL localriverflow,clocalriverflow(numsubstances)
    REAL inflowpart !part of runoff to ilake
    REAL qunitfactor
    REAL transq,transc(numsubstances),dampq,dampc(numsubstances)
    REAL concout(numsubstances)   !variable for concentration of lake outflow
    REAL bconcout(numsubstances)  !variable for concentration of basin outflow
    REAL lakeoutflow(2)                    !Outflow of lakes (m3/s)
    REAL basinoutflow(2)                   !Outflow of subbasin (local and total) (m3/s) (local is outflow of local river and lake)
    REAL,ALLOCATABLE :: cbasinoutflow(:,:) !Concentration of outflow of subbasin (lokal and total) (mg/L)
    REAL outflowsim,wstlakesim             !simulated variables for lakeoutflow/wstlake before update is done
    REAL olakewstold                       !wst olake last time step (mm)
    REAL oldolakewst !olake water stage before updating (m)
    REAL wcomaver                         !average wst olake over time step (mm)
    REAL lakewst(2)   !lake water stage for ilake and olake (mm)
    REAL wstlake,wstlakeadj      !temporary variables for olake water stage output
    REAL w0ref        !olake reference level for water stage
    REAL ilakevol(nsub),olakevol(nsub)  !volume of ilake and olake (m3)
    REAL basinlakevol(nsub),ilakevolmiss(nsub),olakevolmiss(nsub)  !volume of whole lake,ilake and olake (Mm3 or missing value)
    REAL grwout(4,nsub),grwout2         !Outflow from soil to reg. groundwater for print out (all, and resp soillayer)(main river)
    REAL grwtomr,grwtool                !Flow of regional groundwater to main river and olake
    REAL outofsystem                    !loss of water from the model domain via groundwater flow, m3/ts
    REAL riverQbank,riverarea(nrivertypes),riverdepth,riverareakm2
    REAL rivervolume(nrivertypes)   !help for output (m3)
    REAL evapr,cevaprT1
    REAL lakearea(nlaketypes)             !lake area (m2) for lake
    REAL flow1000m3ts         !olake outflow in 1000 m3/ts
    REAL netroutload(numsubstances,nsub)   !net load of main river and olake (outflow-(local and upstream)inflows)
    LOGICAL statuslb,statuslastlb,looplakes(nsub)    !flag for lakebasin in this subbasin, last lakebasin, and for lakebasins in current lakebasinlake
    REAL pein,fnca,fcon   !ilake connectivity variables
    !INTEGER HGDMFLAG !mia flag to activate HGDM
    !Variables for irrigation
    REAL pwneedj,pwneedi               !irrigation water demand for current class, and all irrigated classes (m3)
    REAL gwremi                       !groudwater removed for irrigation (mm)
    REAL ldremi,lrremi,rsremi         !abstraction surface water for irrigation (m3)
    REAL irrevap(nsub)                !accumulation variable for irrigation field and network losses (m3)
    REAL irrappl          !irrappl = applied irrigation (mm), for summation basin output
    REAL irrsinkmass(numsubstances)   !irrsinkmass = mass of substances in the irrigation sink(kg for mg/L concentrations)
    REAL irrigationpar(5)  !hold current parameter-values

    REAL,ALLOCATABLE :: wtransfer(:),cwtransfer(:,:)
    REAL helpload(numsubstances)  !for sa load output

    !Variables for the lake and river ice model
    REAL lakesurftemp(2), riversurftemp(2)
    REAL ctemp_T2
    REAL meanrivertemp,totrivervol  !mean temperature and total river volume over all water course elements
    REAL freezeuparea !freezeuparea for river and lakes
    REAL hypodepth(2)     !lake hypolimnion depth
    INTEGER lakefreezeupday, lakebreakupday, riverfreezeupday, riverbreakupday

    REAL delayedwater(naquifers)    !water in delay to reach aquifer (m3)
    REAL aquiferoutflow(naquifers)
    REAL aqremflow(nsub)            !water removed from soillayers for aquifer
    REAL aqoutside(naquifers)       !water from aquifer to outside model system
    REAL aqirrloss(naquifers)       !water for irrigation from aquifer

    !Variables for error handling
    INTEGER status
    CHARACTER(LEN=80) :: errstring(2)  !error message for location of remove_water call
    PARAMETER (errstring = (/'outflow from local river damping box          ', &  !1
                             'outflow from main river damping box           '/))

    !Variables for comparing forest and open snow cover variables with the FSUS data (Former Soviet Union Snow coarse data)
    REAL snowvari(4,2)    !first index:1=depth,2=cover,3=density,4=swe; second index:1=open,2=forest
    REAL area_forest, area_open

    !Variables to derive subbasin mean precipitation and evaporation on water surfaces (for reservoir inflow regional outvar)
    REAL area_water, prec_water, evap_water

    !Variables for floodplain model
    REAL fpfrac
    REAL qmrflood,qolflood
    REAL wlm3ol,cwlm3ol(numsubstances)
    REAL riverfparea,lakefparea
    REAL interflowpar(5)  !hold current parameter-values
    REAL floodplain_area_fraction
    REAL ffpwl,ffparea
    LOGICAL floodplainclass
    REAL flooddepth,flooddegree
    REAL floodplaindammedflowtoolake(nsub),downstreamdammedflowonolake(nsub),downstreamdammedflowonmainriver(nsub)

    !Variables for fractional river area for evaporation and surface heat exchange
    REAL riverfracarea(nrivertypes)       !fractional river area (-)
    REAL rivereffectivedepth(nrivertypes) !effective river depth (m)

    !Variables for iwet and owet
    LOGICAL wetlandclass
    REAL aowet
    REAL added_water_iwet,added_water_owet

    !Variables for snowfall distribution
    REAL sfdist(nclass)
    INTEGER winddirclass !Direction for wind
    REAL windspeed,winddirdeg !Wind speed

    !Variables for lakebasin lake handling
    INTEGER i2
!    LOGICAL branchwithin
    REAL lakewsti2(nsub),lakeareai2(nsub),qini2(nsub)
    REAL mainflowi2(nsub),branchflowi2(nsub)
    REAL cmainflowi2(numsubstances,nsub),cbranchflowi2(numsubstances,nsub)
    REAL wstlakesimi2(nsub),olakewstoldi2(nsub),wcomaveri2(nsub)
    REAL hypodepthi2(nsub),lakeoutflowi2(nsub),outflowsimi2(nsub)
    REAL Lpathwayi2(numsubstances,18:19,nsub),Lbranchi2(numsubstances,nsub)
    REAL clakeoutflowi2(numsubstances,nsub),qcinflii2(nsub)
    REAL positiveoutflow,cpositiveoutflow(numsubstances)  !help for output
    REAL totaloutflow,ctotaloutflow(numsubstances) !help for output



    !Start of subroutine calculations
    !> \b Algorithm \n
    IF(conductwb) wbflows=0.  !zeroing the timestep for all flows

    !>Initial calculation of regional groundwater
    IF(simulatesubstances) Lgrwclass=0.
    grwout = 0.
    IF(modeloption(p_deepgroundwater)==1 .OR. &
       modeloption(p_deepgroundwater)==2)THEN
      CALL calculate_regional_groundwater_flow(soilstate,grwout,Lgrwclass)    !regional groundwater leaving the soil
      IF(outvarindex(240)>0) outvar(:,outvarindex(240)) = grwout(1,:)   !will be replaced later if aquifermodel
    ENDIF
    IF(conductwb)THEN
      wbflows(w_rgrwof1,:) = grwout(2,:)
      wbflows(w_rgrwof2,:) = grwout(3,:)
      wbflows(w_rgrwof3,:) = grwout(4,:)
    ENDIF
    IF(modeloption(p_deepgroundwater)==2)THEN     !prepare for adding regional groundwater to model
      CALL calculate_current_flow_from_aquifer(naquifers,numsubstances,aquiferstate,aqoutside)
      IF(conductwb) wbflows(w_rgrwtoos,1:naquifers) = aqoutside
    ENDIF

    !>Initial calculation of regional floodplains
    floodplaindammedflowtoolake = 0.
    downstreamdammedflowonolake = 0.
    downstreamdammedflowonmainriver = 0.
    IF(conduct%floodplain)THEN
      IF(modeloption(p_floodplain)==3) CALL calculate_regional_floodplain_flows(nsub,miscstate,floodplaindammedflowtoolake,downstreamdammedflowonolake,downstreamdammedflowonmainriver)
    ENDIF

    !>Get current observations for HYPE specific indata
    IF(conductxoms) CALL get_current_xoms(currentdate,nsub)

    !>Calculate and/or adjust subbasin average atmospheric forcing for all subbasins
    precorg = preci  !original input saved for atmdep recalcution
    IF(outvarindex(o_tobs)>0) outvar(:,outvarindex(o_tobs)) = tempi  !original input data
    IF(outvarindex(o_prec)>0) outvar(:,outvarindex(o_prec)) = preci  !original input data
    CALL calculate_subbasin_temperature(nsub,month,tempi)
    IF(ALLOCATED(tmini)) CALL calculate_subbasin_temperature(nsub,month,tmini)
    IF(ALLOCATED(tmaxi)) CALL calculate_subbasin_temperature(nsub,month,tmaxi)
    CALL calculate_subbasin_precipitation(nsub,tempi,preci,pcorricep)
    IF(calcSWRAD)THEN
      CALL calculate_extraterrestrial_radiation(nsub,dayno,radexti)
      CALL get_current_cloudiness(nsub,month,cloudi)
      IF(outvarindex(o_crex)>0) outvar(:,outvarindex(o_crex)) = radexti
    ENDIF

    !Initiations for main subbasin calculation loop
    accinflow = 0.
    acccinflow = 0.
    irrevap = 0.
    aqirrloss = 0.
    sfdist = 1.
    IF(conductwb) wbirrflows = 0.
    IF(conductwb) wbfpflows = 0.
    IF(conductwb) wbstores = 0.
    regionalirrflows=0.
    horizontalflows2=0.
    IF(conduct%irrigation) CALL initiate_timestep_irrigation(numsubstances,miscstate)
    IF(.NOT.ALLOCATED(cbasinoutflow)) ALLOCATE(cbasinoutflow(numsubstances,2))
    IF(conduct%watertransfer)THEN
      IF(.NOT.ALLOCATED(wtransfer)) ALLOCATE(wtransfer(nsub))
      IF(.NOT.ALLOCATED(cwtransfer)) ALLOCATE(cwtransfer(numsubstances,nsub))
      wtransfer = miscstate%nexttransfer
      miscstate%nexttransfer = 0.
      IF(simulatesubstances)THEN
        cwtransfer = 0.
        DO i = 1,nsub
          IF(wtransfer(i)>0.) cwtransfer(:,i) = miscstate%cnexttransfer(:,i)/wtransfer(i)
        ENDDO
        miscstate%cnexttransfer = 0.
      ENDIF
    ENDIF
    outvarclassfraction = -1 !To capture and set non-class-variables to missing
    CALL initialize_class_outvar_to_zero()
    !For lakebasins
    Lpathwayi2 = 0.
    Lbranchi2 = 0.
    netroutload = 0.
    outflowsimi2 = 0.
    lakeoutflowi2 = 0.
    clakeoutflowi2 = 0.
    qcinflii2 = 0.
    qini2=0.
    mainflowi2 = 0.
    branchflowi2 = 0.
    cmainflowi2 = 0.
    cbranchflowi2 = 0.


    !>Main subbasin-loop, subbasins calculated in flow order

    subbasinloop:  &
    DO i = 1,nsub

      !Initiate variables for calculation of subbasins
      eacti=0.
      snowi=0.
      soilwateri=0.
      runoffi=0.
      runofftsi=0.;
      crunofftsi=0.
      snowdepthi=0.
      qcinfli=0.
      pwneedi=0.
      ldremi=0.;lrremi=0.;gwremi=0.;rsremi=0.
      asum=0.
      soil4runoff=0.
      glacierflows=0.;nutrientloadflows=0.;irrigationflows=0.
      snowvari=0.
      area_forest = 0.; area_open = 0.
      area_water=0.;prec_water = 0.;evap_water=0.
      soillgrossload = 0.; soillnetload = 0.
      IF(conductload)THEN
        Latmdep=0.
        Lcultiv=0.
        Lirrsoil=0.
        Lrurala=0.
        Lgrwsoil=0.
        Lgrwol=0.
        Ltransf=0.
        Lgrwmr=0.
        Lstream=0.
      ENDIF
      !DO isl = 1,3
      !  CALL calculate_class_outvar_initialize(o_pfN(isl),i)
      !  CALL calculate_class_outvar_initialize(o_phN(isl),i)
      !  CALL calculate_class_outvar_initialize(o_pfP(isl),i)
      !  CALL calculate_class_outvar_initialize(o_phP(isl),i)
      !  CALL calculate_class_outvar_initialize(o_ppP(isl),i)
      !  CALL calculate_class_outvar_initialize(o_pfC(isl),i)
      !  CALL calculate_class_outvar_initialize(o_phC(isl),i)
      !  CALL calculate_class_outvar_initialize(o_ppT1(isl),i)
      !  CALL calculate_class_outvar_initialize(o_sltmp(isl),i)
      !  CALL calculate_class_outvar_initialize(o_csoillayerIN(isl),i)
      !  CALL calculate_class_outvar_initialize(o_psoilIN(isl),i)
      !  CALL calculate_class_outvar_initialize(o_psoilSP(isl),i)
      !  CALL calculate_class_outvar_initialize(o_psoilON(isl),i)
      !  CALL calculate_class_outvar_initialize(o_psoilT1(isl),i)
      !ENDDO
      !CALL calculate_class_outvar_initialize(o_csoilIN,i)
      !CALL calculate_class_outvar_initialize(o_csoilOC,i)
      !CALL calculate_class_outvar_initialize(o_csoilT1,i)
      !CALL calculate_class_outvar_initialize(o_csoilT2,i)
      !CALL calculate_class_outvar_initialize(o_snow,i)
      !CALL calculate_class_outvar_initialize(o_snowmelt,i)
      !CALL calculate_class_outvar_initialize(o_snowdepth,i)
      !CALL calculate_class_outvar_initialize(o_snowdens,i)
      !CALL calculate_class_outvar_initialize(o_infi,i)
      !CALL calculate_class_outvar_initialize(o_T1sf,i)
      !CALL calculate_class_outvar_initialize(o_soiltmp,i)
      !CALL calculate_class_outvar_initialize(o_dtmp,i)
      !CALL calculate_class_outvar_initialize(o_grwlevel,i)
      !CALL calculate_class_outvar_initialize(o_epot,i)
      !CALL calculate_class_outvar_initialize(o_evap,i)
      !CALL calculate_class_outvar_initialize(o_evpt,i)
      !CALL calculate_class_outvar_initialize(o_icloss,i)
      !CALL calculate_class_outvar_initialize(o_cevapT1,i)
      !CALL calculate_class_outvar_initialize(o_landevap,i)
      !CALL calculate_class_outvar_initialize(o_crun,i)
      !CALL calculate_class_outvar_initialize(o_crun2,i)
      !CALL calculate_class_outvar_initialize(o_ro1,i)
      !CALL calculate_class_outvar_initialize(o_ro2,i)
      !CALL calculate_class_outvar_initialize(o_ro3,i)
      !CALL calculate_class_outvar_initialize(o_rod,i)
      !CALL calculate_class_outvar_initialize(o_ros,i)
      !CALL calculate_class_outvar_initialize(o_ros1,i)
      !CALL calculate_class_outvar_initialize(o_ros2,i)
      !CALL calculate_class_outvar_initialize(o_ctmp,i)
      !CALL calculate_class_outvar_initialize(o_cprc,i)
      !CALL calculate_class_outvar_initialize(o_psim,i)
      !CALL calculate_class_outvar_initialize(o_rainfall,i)
      !CALL calculate_class_outvar_initialize(o_snowfall,i)
      !CALL calculate_class_outvar_initialize(o_crosT1,i)
      !CALL calculate_class_outvar_initialize(o_crodT1,i)
      !CALL calculate_class_outvar_initialize(o_cro1T1,i)
      !CALL calculate_class_outvar_initialize(o_cro2T1,i)
      !CALL calculate_class_outvar_initialize(o_cro3T1,i)
      !CALL calculate_class_outvar_initialize(o_crunT1,i)
      !CALL calculate_class_outvar_initialize(o_crunT2,i)
      !CALL calculate_class_outvar_initialize(o_crunIN,i)
      !CALL calculate_class_outvar_initialize(o_crunON,i)
      !CALL calculate_class_outvar_initialize(o_crunTN,i)
      !CALL calculate_class_outvar_initialize(o_crunSP,i)
      !CALL calculate_class_outvar_initialize(o_crunPP,i)
      !CALL calculate_class_outvar_initialize(o_crunTP,i)
      !CALL calculate_class_outvar_initialize(o_crunOC,i)
      !CALL calculate_class_outvar_initialize(o_crunSS,i)
      !CALL calculate_class_outvar_initialize(o_smffc,i)
      !CALL calculate_class_outvar_initialize(o_smfdep,i)
      !CALL calculate_class_outvar_initialize(o_smrzfdep,i)
      !CALL calculate_class_outvar_initialize(o_smfpw,i)
      !CALL calculate_class_outvar_initialize(o_smrzfpw,i)
      !CALL calculate_class_outvar_initialize(o_soim,i)
      !CALL calculate_class_outvar_initialize(o_soim12,i)
      !CALL calculate_class_outvar_initialize(o_sml9,i)
      !CALL calculate_class_outvar_initialize(o_sml0,i)
      !CALL calculate_class_outvar_initialize(o_sml1,i)
      !CALL calculate_class_outvar_initialize(o_sml2,i)
      !CALL calculate_class_outvar_initialize(o_sml3,i)
      !CALL calculate_class_outvar_initialize(o_smrz,i)
      !CALL calculate_class_outvar_initialize(o_sm13,i)
      !CALL calculate_class_outvar_initialize(o_soildef,i)
      !CALL calculate_class_outvar_initialize(o_applirr,i)
      !CALL calculate_class_outvar_initialize(o_soildenitr,i)
      !CALL calculate_class_outvar_initialize(o_soildenrz,i)
      !CALL calculate_class_outvar_initialize(o_soilden3,i)
      !CALL calculate_class_outvar_initialize(o_cropNupt,i)
      !CALL calculate_class_outvar_initialize(o_degrfN,i)
      !CALL calculate_class_outvar_initialize(o_soilNatm,i)
      !CALL calculate_class_outvar_initialize(o_soilPatm,i)
      !CALL calculate_class_outvar_initialize(o_evapsnow,i)
      !CALL calculate_class_outvar_initialize(o_soilfrost,i)
      !CALL calculate_class_outvar_initialize(o_snowcover,i)
      !CALL calculate_class_outvar_initialize(o_snowmax,i)
      !CALL calculate_class_outvar_initialize(o_snht,i)
      !CALL calculate_class_outvar_initialize(o_snte,i)
      !CALL calculate_class_outvar_initialize(o_snts,i)
      !CALL calculate_class_outvar_initialize(o_snwc,i)
      !CALL calculate_class_outvar_initialize(o_isps,i)
      !CALL calculate_class_outvar_initialize(o_ispp,i)
      !DO k=285,320
      !  CALL calculate_class_outvar_initialize(k,i)
      !ENDDO

      !Short notation for parameters not dependent on class
      IF(basin(i)%parregion(regiondivision(m_incorr))>0)THEN
        incorr = (1. + regpar(m_incorr,basin(i)%parregion(regiondivision(m_incorr))))     !Correction of inorganic nitrogen
      ELSE
        incorr = 1.
      ENDIF
      IF(basin(i)%parregion(regiondivision(m_oncorr))>0)THEN
        oncorr = (1. + regpar(m_oncorr,basin(i)%parregion(regiondivision(m_incorr))))     !Correction of organic nitrogen
      ELSE
        oncorr = 1.
      ENDIF
      IF(basin(i)%parregion(regiondivision(m_phoscorr))>0)THEN
        phoscorr = 1. + regpar(m_phoscorr,basin(i)%parregion(regiondivision(m_incorr)))   !Correction of phosphorus
      ELSE
        phoscorr = 1.
      ENDIF

      !Calculation of mean air temperature
      IF(conduct%wetland)THEN
        miscstate%temp5(i) = miscstate%temp5(i) + (tempi(i) - miscstate%temp5(i)) / avertemp(1)
        miscstate%temp30(i) = miscstate%temp30(i) + (tempi(i) - miscstate%temp30(i)) / avertemp(4)
      ENDIF
      IF(conduct%simC)THEN
        miscstate%temp10(i) = miscstate%temp10(i) + (tempi(i) - miscstate%temp10(i)) / avertemp(2)
        miscstate%temp20(i) = miscstate%temp20(i) + (tempi(i) - miscstate%temp20(i)) / avertemp(3)
      ENDIF

      !Set subbasin precipitation concentrations
      CALL set_precipitation_concentration(i,numsubstances,cprec)

      !Calculation of snowfall redistribution factors sfdist
      IF(modeloption(p_snowfalldist)>=1)THEN
        sfdist = 1. ! Set to one as default
        IF(ALLOCATED(uwindi) .AND. ALLOCATED(vwindi)) THEN
          CALL calculate_winddirspeed(INT(genpar(m_numdir)),uwindi(i),vwindi(i),winddirclass,winddirdeg, windspeed)
          IF(currently_snowing(i,preci(i),tempi(i),genpar(m_sfdlim))) &
            CALL calculate_snowfall_distribution(i, classdata(:)%luse, windspeed, classbasin(i,:)%wsf(winddirclass),sfdist)
        ENDIF
      ENDIF

      !Calculate pseudo dayno and daylength for temperature dependent growing season
      pseudo_dayno = get_pseudo_dayno(dayno,basin(i)%latitude)
      daylength = 0.
      IF(modeloption(p_growthstart)==1)  &
        CALL calculate_daylength(dayno,basin(i)%latitude,daylength)

      !>Main class-loop for calculation of soil water and substances (land classes)
      DO j=1,nclass
        a=classbasin(i,j)%part
        classarea = a * basin(i)%area * 1.0E-6    !km2
        IF(a>0)THEN
          irrappl = 0.    !for glaciers and no irrigation-simulation
          pwneedj = 0.
          cultivload = 0.
          irrload = 0.
          ruralaload = 0.
          atmdepload1 = 0.
          atmdepload2 = 0.
          rgrwload = 0.
          crunoffd = 0.   !not set for floodplain
          horizontalflows=0.  !not set for floodplain
          old_snow_part = 1.
          snowplain_area_fraction = 1.
          floodplain_area_fraction = 1.
          floodplainclass = .FALSE.
          wetlandclass = .FALSE.
          !> \li Calculate class forcing data
          CALL calculate_class_atmospheric_forcing(i,j,radexti(i),cloudi(i),  &
                  temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
          IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution
          CALL set_class_precipitation_concentration_and_load(numsubstances, &
                   classarea,precorg(i),temp,prec,cprec,cprecj,atmdepload1)

          !> \li Calculate soil processes
          IF(classmodel(j)==0)THEN
            CALL soilmodel_0(i,j,classdata(j)%soil,classdata(j)%luse,basin(i)%subid,pseudo_dayno,classarea,prec,cprecj,temp,  &
                 daylength,tmin,tmax,swrad,radexti(i),netrad,actvap,satvap,wind, &
                 basinrrcscorr(i),phoscorr,basincevpcorr(i),incorr,oncorr,  &
                 frozenstate,soilstate,miscstate,surfaceflow,csrunoff,crunoffd,    &
                 cropuptake,nitrif,denitrif,epot,gwat,frostdepth,smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
                 pwneedj,irrappl,irrload,snowfall,rainfall,cultivload,ruralaload,rgrwload,  &
                 atmdepload2,infiltrationflows,evapflows,runofflows,verticalflows,  &
                 cverticalflows,horizontalflows,horizontalflows2,evapsnow,cruralflow,snowtemp,snowsurftemp)
          ELSEIF(j==slc_lriver)THEN
            CYCLE
          ELSEIF(j==slc_mriver)THEN
            IF(conduct%floodplain)THEN
              IF(floodindex(i)>0)THEN
                IF(flooding(floodindex(i))%fpfmr>0.)THEN
                  CALL soilmodel_4(i,j,classdata(j)%soil,classdata(j)%luse,basin(i)%subid,pseudo_dayno,classarea,prec,cprecj,temp,  &
                   daylength,tmin,tmax,swrad,radexti(i),netrad,actvap,satvap,wind, &
                   basinrrcscorr(i),phoscorr,basincevpcorr(i),incorr,oncorr, &
                   frozenstate,soilstate,miscstate,surfaceflow,csrunoff,crunoffd,    &
                   cropuptake,nitrif,denitrif,epot,gwat,frostdepth,smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
                   snowcov,old_snow_part,pwneedj,irrappl,irrload,snowfall,rainfall,cultivload,ruralaload,atmdepload1,atmdepload2, &
                   infiltrationflows,floodplainflows,evapflows,runofflows,verticalflows,cverticalflows,horizontalflows,horizontalflows2, &
                   evapsnow,soil4runoff,cruralflow,snowtemp,snowsurftemp)
                  floodplain_area_fraction = flooding(floodindex(i))%fpfmr
                  floodplainclass = .TRUE.
                ELSE
                  CYCLE
                ENDIF
              ELSE
                CYCLE
              ENDIF
            ELSE
              CYCLE
            ENDIF
          ELSEIF(classmodel(j)==glacier_model)THEN
            !only one glacier class is allowed
            CALL soilmodel_3(i,j,classdata(j)%soil,classdata(j)%luse,basin(i)%subid,pseudo_dayno,classarea,prec,cprecj,temp, &
                  daylength,tmin,tmax,swrad,radexti(i),netrad,actvap,satvap,wind, &
                  basincevpcorr(i),basinrrcscorr(i),phoscorr, &
                  frozenstate,soilstate,miscstate,surfaceflow,csrunoff,crunoffd,  &
                  cropuptake,nitrif,denitrif,epot,gwat,frostdepth,smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
                  glac_part,old_snow_part,snowfall,rainfall,cultivload,ruralaload, &
                  rgrwload,atmdepload2,infiltrationflows,glacierflows,evapflows,runofflows,  &
                  verticalflows,cverticalflows,horizontalflows,horizontalflows2,evapsnow,cruralflow,snowtemp,snowsurftemp)
            snowplain_area_fraction = 1. - glac_part
            IF(outvarindex(117)>0) outvar(i,outvarindex(117)) = frozenstate%glacvol(i)*1.E-9    !glacier ice volume (km3)
            IF(outvarindex(118)>0) outvar(i,outvarindex(118)) = glac_part * classarea           !glacier area (km2)
            IF(outvarindex(o_gmlt)>0) outvar(i,outvarindex(o_gmlt)) = infiltrationflows(7)*SUM(infiltrationflows(2:6))
            CALL set_outvar_xobs(256,i)
            IF(xobsindex(256,i).GT.0)THEN
              glacperiod = xobsi(xobsindex(256,i)) !Glacier mass balance period
            ELSE
              glacperiod = missing_value
            ENDIF
            IF(glacperiod /= missing_value)THEN   !If there is a Mass balance period to evaluate:
              CALL set_outvar_xobs(253,i) !recorded mass balance
              CALL set_outvar_xobs(255,i) !recorded mass balance area
              CALL calculate_glacier_massbalance(i,glacperiod,computedMassBalance,computedMBArea)   !calculate simulated glacier mass balance and area
              IF(outvarindex(252)>0) outvar(i,outvarindex(252)) = computedMassBalance
              IF(outvarindex(254)>0) outvar(i,outvarindex(254)) = computedMBArea
            ENDIF
          ELSEIF(j==slc_olake)THEN
            IF(conduct%floodplain)THEN
              IF(floodindex(i)>0)THEN
                IF(flooding(floodindex(i))%fpfol>0.)THEN
                  CALL soilmodel_4(i,j,classdata(j)%soil,classdata(j)%luse,basin(i)%subid,pseudo_dayno,classarea,prec,cprecj,temp,  &
                   daylength,tmin,tmax,swrad,radexti(i),netrad,actvap,satvap,wind, &
                   basinrrcscorr(i),phoscorr,basincevpcorr(i),incorr,oncorr,  &
                   frozenstate,soilstate,miscstate,surfaceflow,csrunoff,crunoffd,    &
                   cropuptake,nitrif,denitrif,epot,gwat,frostdepth,smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
                   snowcov,old_snow_part,pwneedj,irrappl,irrload,snowfall,rainfall,cultivload,ruralaload,atmdepload1,atmdepload2, &
                   infiltrationflows,floodplainflows,evapflows,runofflows,verticalflows,cverticalflows,horizontalflows,horizontalflows2, &
                   evapsnow,soil4runoff,cruralflow,snowtemp,snowsurftemp)
                  floodplain_area_fraction = flooding(floodindex(i))%fpfol
                  floodplainclass = .TRUE.
                ELSE
                  CYCLE
                ENDIF
              ELSE
                CYCLE
              ENDIF
            ELSE
              CYCLE
            ENDIF
          ELSEIF(j==slc_ilake)THEN
            CYCLE
          ELSEIF(j==slc_iwet)THEN
            wetlandclass = .TRUE.
            CALL soilmodel_0(i,j,classdata(j)%soil,classdata(j)%luse,basin(i)%subid,pseudo_dayno,classarea,prec,cprecj,temp,  &
                 daylength,tmin,tmax,swrad,radexti(i),netrad,actvap,satvap,wind, &
                 basinrrcscorr(i),phoscorr,basincevpcorr(i),incorr,oncorr,  &
                 frozenstate,soilstate,miscstate,surfaceflow,csrunoff,crunoffd,    &
                 cropuptake,nitrif,denitrif,epot,gwat,frostdepth,smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
                 pwneedj,irrappl,irrload,snowfall,rainfall,cultivload,ruralaload,rgrwload,  &
                 atmdepload2,infiltrationflows,evapflows,runofflows,verticalflows,  &
                 cverticalflows,horizontalflows,horizontalflows2,evapsnow,cruralflow,snowtemp,snowsurftemp)
            added_water_iwet = infiltrationflows(2) + infiltrationflows(4) + verticalflows(5) - verticalflows(1) - evapflows(1)
          ELSEIF(j==slc_owet)THEN
            wetlandclass = .TRUE.
            CALL soilmodel_0(i,j,classdata(j)%soil,classdata(j)%luse,basin(i)%subid,pseudo_dayno,classarea,prec,cprecj,temp,  &
                 daylength,tmin,tmax,swrad,radexti(i),netrad,actvap,satvap,wind, &
                 basinrrcscorr(i),phoscorr,basincevpcorr(i),incorr,oncorr,  &
                 frozenstate,soilstate,miscstate,surfaceflow,csrunoff,crunoffd,    &
                 cropuptake,nitrif,denitrif,epot,gwat,frostdepth,smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
                 pwneedj,irrappl,irrload,snowfall,rainfall,cultivload,ruralaload,rgrwload,  &
                 atmdepload2,infiltrationflows,evapflows,runofflows,verticalflows,  &
                 cverticalflows,horizontalflows,horizontalflows2,evapsnow,cruralflow,snowtemp,snowsurftemp)
            added_water_owet = infiltrationflows(2) + infiltrationflows(4) + verticalflows(5) - verticalflows(1) - evapflows(1)
          ELSEIF(classmodel(j)==5)THEN
            CALL soilmodel_5(i,j,classdata(j)%soil,classdata(j)%luse,basin(i)%subid,pseudo_dayno,classarea,prec,cprecj,temp,  &
                 daylength,tmin,tmax,swrad,radexti(i),netrad,actvap,satvap,wind, &
                 basinrrcscorr(i),basincevpcorr(i),  &
                 frozenstate,soilstate,surfaceflow,csrunoff,crunoffd,    &
                 cropuptake,nitrif,denitrif,epot,gwat,frostdepth,smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
                 pwneedj,irrappl,irrload,snowfall,rainfall,cultivload,ruralaload,rgrwload,  &
                 infiltrationflows,evapflows,runofflows,verticalflows,  &
                 cverticalflows,horizontalflows,horizontalflows2,evapsnow,cruralflow,snowtemp,snowsurftemp)
          ELSE
            CALL soilmodel_0(i,j,classdata(j)%soil,classdata(j)%luse,basin(i)%subid,pseudo_dayno,classarea,prec,cprecj,temp,  &
                 daylength,tmin,tmax,swrad,radexti(i),netrad,actvap,satvap,wind, &
                 basinrrcscorr(i),phoscorr,basincevpcorr(i),incorr,oncorr,  &
                 frozenstate,soilstate,miscstate,surfaceflow,csrunoff,crunoffd,    &
                 cropuptake,nitrif,denitrif,epot,gwat,frostdepth,smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
                 pwneedj,irrappl,irrload,snowfall,rainfall,cultivload,ruralaload,rgrwload,  &
                 atmdepload2,infiltrationflows,evapflows,runofflows,verticalflows,  &
                 cverticalflows,horizontalflows,horizontalflows2,evapsnow,cruralflow,snowtemp,snowsurftemp)
          ENDIF

          !Set load variables for printout
          IF(conductload)THEN
            Latmdep(j,1,:) = atmdepload1      !including flooded floodplain atmdep (=0)
            Latmdep(j,2,:) = atmdepload2      !-"-                                 (>0)
            IF(.NOT.floodplainclass)THEN
              IF(.NOT.wetlandclass) Lcultiv(j,:,:) = cultivload     !not including non-flooded floodplain, exclude wetlandclass cultivload from SourceApp calculations
              Lirrsoil(j,:)  = irrload
              Lrurala(j,:)   = ruralaload
              Lgrwsoil(j,:)  = rgrwload
            ENDIF
          ENDIF

          !Set runoff concentration to T1 leakage parameters
          IF(T1leakage)THEN
            crunoff1(i_t1) = landpar(m_t1leaklu,classdata(j)%luse)*soilpar(m_t1leakst,classdata(j)%soil)
            crunoff2(i_t1) = crunoff1(i_t1)
            crunoff3(i_t1) = crunoff1(i_t1)
            crunoffd(i_t1) = crunoff1(i_t1)
            csrunoff(i_t1) = crunoff1(i_t1)
          ENDIF

          !> \li Accumulate variables for mean over subbasin and soil layers
          aadj = a*floodplain_area_fraction           !landarea fraction of classarea fraction for mriver/olake class
          IF(.NOT.wetlandclass)THEN
            asum(1) = asum(1) + aadj                    !landarea fraction (dry floodplains included)
            IF(soilthick(2,j)>0)THEN
              asum(2) = asum(2) + aadj
              IF(soilthick(3,j)>0)THEN
                asum(3) = asum(3) + aadj
              ENDIF
            ENDIF
          ENDIF
          totalsurfaceflow = surfaceflow(1)+surfaceflow(2)
          IF(.NOT.floodplainclass)THEN  !Floodplain will be added in routing loop
            CALL calculate_class_outvar_add(o_ctmp,i,j,a,temp)
            CALL calculate_class_outvar_add(o_cprc,i,j,a,prec) !prec is not updated with snowdistribution
            IF(outvarstatus(o_psim)) CALL calculate_class_outvar_add(o_psim,i,j,a,prec+pcorricep(i)+icpevap)
            CALL calculate_class_outvar_add(o_rainfall,i,j,a,rainfall)
            CALL calculate_class_outvar_add(o_snowfall,i,j,a,snowfall)
            IF(outvarstatus(o_crgl)) CALL calculate_class_outvar_add(o_crgl,i,j,a,swrad)
            IF(outvarstatus(o_crnt)) CALL calculate_class_outvar_add(o_crnt,i,j,a,netrad)
            IF(outvarstatus(o_crpt)) CALL calculate_class_outvar_add(o_crpt,i,j,a,swpot)
          ENDIF
          IF(.NOT.wetlandclass)THEN
            !Snow output
            IF(outvarstatus(o_snow)) CALL calculate_class_outvar_add(o_snow,i,j,aadj,frozenstate%snow(j,i)*snowplain_area_fraction)  !zero snow on glacier area
            IF(outvarstatus(o_snowdepth)) CALL calculate_class_outvar_add(o_snowdepth,i,j,aadj,frozenstate%snowdepth(j,i)*snowplain_area_fraction)  !zero snow on glacier area
            IF(outvarstatus(o_snowdens))THEN
              IF(frozenstate%snowdepth(j,i)>0.)THEN
                CALL calculate_class_outvar_add_amount(o_snowdens,i,j,aadj,frozenstate%snow(j,i)*snowplain_area_fraction,frozenstate%snow(j,i)/frozenstate%snowdepth(j,i))  !zero snow on glacier area
              ELSE
                CALL calculate_class_outvar_add_amount(o_snowdens,i,j,aadj,0.,0.) !zero snow included in density calculation
              ENDIF
              IF(outvarindex(o_snowdens)>0) snowdepthi = snowdepthi + frozenstate%snowdepth(j,i)*aadj*snowplain_area_fraction    !-"-
            ENDIF
            IF(conductwb) snowi = snowi + frozenstate%snow(j,i)*aadj*snowplain_area_fraction  !zero snow on glacier area
            IF(outvarstatus(o_snowmax)) CALL calculate_class_outvar_add(o_snowmax,i,j,aadj,frozenstate%snowmax(j,i)*snowplain_area_fraction)  !-"-
            IF(outvarstatus(o_snowmelt)) CALL calculate_class_outvar_add(o_snowmelt,i,j,aadj,infiltrationflows(1)*SUM(infiltrationflows(2:6)))
            !Snow heat and temperature output
            IF(modeloption(p_snowheat)>=1)THEN
              IF(outvarindex(o_snht)>0) CALL calculate_class_outvar_add(o_snht,i,j,aadj,frozenstate%snowheat(j,i)*snowplain_area_fraction)
              IF(outvarindex(o_snte)>0) CALL calculate_class_outvar_add(o_snte,i,j,aadj,snowtemp*snowplain_area_fraction)
              IF(outvarindex(o_snts)>0) CALL calculate_class_outvar_add(o_snts,i,j,aadj,snowsurftemp*snowplain_area_fraction)
            ENDIF
            !Snow liquid water content (as mm)
            IF(outvarindex(o_snwc)>0) CALL calculate_class_outvar_add(o_snwc,i,j,aadj,frozenstate%snowliq(j,i)*snowplain_area_fraction)
            !Snow cover FSC (must be rescaled with the fraction of ilake+olake before output)
            IF(.NOT.floodplainclass)THEN
              IF(outvarstatus(o_snowcover)) CALL calculate_class_outvar_add(o_snowcover,i,j,aadj,frozenstate%snowcov(j,i)*snowplain_area_fraction)  !-"-
            ELSE
              IF(outvarstatus(o_snowcover)) CALL calculate_class_outvar_add(o_snowcover,i,j,aadj,snowcov*snowplain_area_fraction)
            ENDIF
            !Forest and Open field snow variables defined by vegtype
            IF(SUM(outvarindex(267:274))>0)THEN
              IF(classdata(j)%vegtype==1.OR.classdata(j)%vegtype==2)THEN      !Open field or forest landuse set
                snowvari(1,classdata(j)%vegtype) = snowvari(1,classdata(j)%vegtype) + frozenstate%snowdepth(j,i)*aadj*snowplain_area_fraction
                IF(frozenstate%snowdepth(j,i)>0.)THEN
                  snowvari(3,classdata(j)%vegtype) = snowvari(3,classdata(j)%vegtype) + 0.1 * frozenstate%snow(j,i)/frozenstate%snowdepth(j,i) * aadj*snowplain_area_fraction
                ENDIF
                IF(.NOT.floodplainclass)THEN
                  snowvari(2,classdata(j)%vegtype) = snowvari(2,classdata(j)%vegtype) + 10. * frozenstate%snowcov(j,i)*aadj*snowplain_area_fraction
                ELSE
                  snowvari(2,classdata(j)%vegtype) = snowvari(2,classdata(j)%vegtype) + 10. * snowcov*aadj*snowplain_area_fraction
                ENDIF
                snowvari(4,classdata(j)%vegtype) = snowvari(4,classdata(j)%vegtype) + frozenstate%snow(j,i)*aadj*snowplain_area_fraction
                IF(classdata(j)%vegtype==1) area_open   = area_open + aadj*snowplain_area_fraction
                IF(classdata(j)%vegtype==2) area_forest = area_forest + aadj*snowplain_area_fraction
              ENDIF
            ENDIF

          ENDIF
          !Evapotranspiration output
          CALL calculate_class_outvar_add(o_evap,i,j,aadj,evap)
          CALL calculate_class_outvar_add(o_epot,i,j,aadj,epot)
          IF(outvarstatus(o_evpt)) CALL calculate_class_outvar_add(o_evpt,i,j,aadj,evap+pcorricep(i)+icpevap)
          IF(outvarstatus(o_icloss)) CALL calculate_class_outvar_add(o_icloss,i,j,aadj,pcorricep(i)+icpevap)
          CALL calculate_class_outvar_add(o_evapsnow,i,j,aadj,evapsnow)
          eacti = eacti+evap*aadj
          IF(i_t1>0.AND.outvarstatus(o_cevapT1)) CALL calculate_class_outvar_add_amount(o_cevapT1,i,j,aadj,cevap(i_t1)*evap,cevap(i_t1))  !accumulation of amount
          IF(.NOT.wetlandclass)THEN
            CALL calculate_class_outvar_add(o_landevap,i,j,aadj,evap)    !no river,lake but include floodplains
            IF(outvarstatus(o_infi)) CALL calculate_class_outvar_add(o_infi,i,j,aadj,infiltrationflows(2)+SUM(infiltrationflows(4:6)))
            CALL calculate_class_outvar_add(o_grwlevel,i,j,aadj,gwat)
            CALL calculate_class_outvar_add(o_soilfrost,i,j,aadj,frostdepth)
            IF(outvarstatus(o_soiltmp)) CALL calculate_class_outvar_add(o_soiltmp,i,j,aadj,(soilstate%temp(1,j,i)*soilthick(1,j)+soilstate%temp(2,j,i)*soilthick(2,j)+soilstate%temp(3,j,i)*soilthick(3,j))/soildepth(3,j))
            IF(outvarstatus(o_dtmp)) CALL calculate_class_outvar_add(o_dtmp,i,j,aadj,soilstate%deeptemp(j,i))
            DO isl=1,3
              IF(outvarstatus(o_sltmp(isl))) CALL calculate_class_outvar_add(o_sltmp(isl),i,j,aadj,soilstate%temp(isl,j,i))
            ENDDO
          ENDIF
          pwneedi = pwneedi + pwneedj   !for calculation of demand, not output
          IF(outvarstatus(o_applirr)) CALL calculate_class_outvar_add_accumulate(o_applirr,i,j,aadj,irrappl)
          !Soil moisture output
          IF(.NOT.wetlandclass)THEN
            soilwateri = soilwateri+soilstate%water(:,j,i)*aadj
            IF(outvarstatus(o_soim)) CALL calculate_class_outvar_add(o_soim,i,j,aadj,SUM(soilstate%water(:,j,i)))
            IF(outvarstatus(o_soim12)) CALL calculate_class_outvar_add(o_soim12,i,j,aadj,soilstate%water(1,j,i)+soilstate%water(2,j,i))
            CALL calculate_class_outvar_add(o_sml9,i,j,aadj,soilstate%water(1,j,i))
            CALL calculate_class_outvar_add(o_sml2,i,j,aadj,soilstate%water(2,j,i))
            CALL calculate_class_outvar_add(o_sml3,i,j,aadj,soilstate%water(3,j,i))
            CALL calculate_class_outvar_add(o_soildef,i,j,aadj,smdef)
            IF(soilstate%water(1,j,i)>pwmm(1,j))THEN
              standingwater = soilstate%water(1,j,i)-pwmm(1,j)
              IF(outvarstatus(o_smffc)) CALL calculate_class_outvar_add(o_smffc,i,j,aadj,(pwmm(1,j)+soilstate%water(2,j,i)-wpmm(1,j)-wpmm(2,j))/(fcmm(1,j)+fcmm(2,j)))
              IF(outvarstatus(o_smfdep)) CALL calculate_class_outvar_add(o_smfdep,i,j,aadj,(pwmm(1,j)+soilstate%water(2,j,i)+soilstate%water(3,j,i))/(1000.*soildepth(3,j)))
              IF(outvarstatus(o_smfpw)) CALL calculate_class_outvar_add(o_smfpw,i,j,aadj,(pwmm(1,j)+soilstate%water(2,j,i)+soilstate%water(3,j,i))/(pwmm(1,j)+pwmm(2,j)+pwmm(3,j)))
              IF(outvarstatus(o_smrzfdep)) CALL calculate_class_outvar_add(o_smrzfdep,i,j,aadj,(pwmm(1,j)+soilstate%water(2,j,i))/(1000.*soildepth(2,j)))
              IF(outvarstatus(o_smrzfpw)) CALL calculate_class_outvar_add(o_smrzfpw,i,j,aadj,(pwmm(1,j)+soilstate%water(2,j,i))/(pwmm(1,j)+pwmm(2,j)))
            ELSE
              standingwater = 0.
              IF(outvarstatus(o_smffc)) CALL calculate_class_outvar_add(o_smffc,i,j,aadj,(soilstate%water(1,j,i)+soilstate%water(2,j,i)-wpmm(1,j)-wpmm(2,j))/(fcmm(1,j)+fcmm(2,j)))
              IF(outvarstatus(o_smfdep)) CALL calculate_class_outvar_add(o_smfdep,i,j,aadj,(soilstate%water(1,j,i)+soilstate%water(2,j,i)+soilstate%water(3,j,i))/(1000.*soildepth(3,j)))
              IF(outvarstatus(o_smfpw)) CALL calculate_class_outvar_add(o_smfpw,i,j,aadj,(soilstate%water(1,j,i)+soilstate%water(2,j,i)+soilstate%water(3,j,i))/(pwmm(1,j)+pwmm(2,j)+pwmm(3,j)))
              IF(outvarstatus(o_smrzfdep)) CALL calculate_class_outvar_add(o_smrzfdep,i,j,aadj,(soilstate%water(1,j,i)+soilstate%water(2,j,i))/(1000.*soildepth(2,j)))
              IF(outvarstatus(o_smrzfpw)) CALL calculate_class_outvar_add(o_smrzfpw,i,j,aadj,(soilstate%water(1,j,i)+soilstate%water(2,j,i))/(pwmm(1,j)+pwmm(2,j)))
            ENDIF
            CALL calculate_class_outvar_add(o_sml0,i,j,aadj,standingwater)
            IF(outvarstatus(o_sml1)) CALL calculate_class_outvar_add(o_sml1,i,j,aadj,soilstate%water(1,j,i)-standingwater)
            IF(outvarstatus(o_smrz)) CALL calculate_class_outvar_add(o_smrz,i,j,aadj,soilstate%water(1,j,i)+soilstate%water(2,j,i)-standingwater)
            IF(outvarstatus(o_sm13)) CALL calculate_class_outvar_add(o_sm13,i,j,aadj,soilstate%water(1,j,i)+soilstate%water(2,j,i)+soilstate%water(3,j,i)-standingwater)
          ENDIF
          !Nutrient processes and pool output
          IF(.NOT.wetlandclass)THEN
            IF(outvarstatus(o_soildenitr)) CALL calculate_class_outvar_add(o_soildenitr,i,j,aadj,SUM(denitrif(:)))
            IF(outvarstatus(o_soildenrz)) CALL calculate_class_outvar_add_accumulate(o_soildenrz,i,j,aadj,denitrif(1)+denitrif(2))
            CALL calculate_class_outvar_add_accumulate(o_soilden3,i,j,aadj,denitrif(3))
            CALL calculate_class_outvar_add(o_cropNupt,i,j,aadj,cropuptake)
            CALL calculate_class_outvar_add(o_degrfN,i,j,aadj,nitrif)
            IF(numsubstances>0)THEN
              IF(outvarstatus(o_csoilIN).OR.outvarstatus(o_csoilT1).OR.outvarstatus(o_csoilOC).OR.outvarstatus(o_csoilT2))THEN
                loadrf = soilstate%conc(:,1,j,i)*soilstate%water(1,j,i) + soilstate%conc(:,2,j,i)*soilstate%water(2,j,i) + soilstate%conc(:,3,j,i)*soilstate%water(3,j,i)
                IF(soilstate%water(1,j,i) + soilstate%water(2,j,i) + soilstate%water(3,j,i)>0.)THEN
                  concrf = loadrf / (soilstate%water(1,j,i) + soilstate%water(2,j,i) + soilstate%water(3,j,i))
                ELSE
                  concrf = 0.
                ENDIF
                IF(conduct%simN.AND.outvarstatus(o_csoilIN)) CALL calculate_class_outvar_add_amount(o_csoilIN,i,j,aadj,loadrf(i_in),concrf(i_in))
                IF(simulate%substance(i_t1).AND.outvarstatus(o_csoilT1)) CALL calculate_class_outvar_add_amount(o_csoilT1,i,j,aadj,loadrf(i_t1),concrf(i_t1))
                IF(conduct%simC.AND.outvarstatus(o_csoilOC)) CALL calculate_class_outvar_add_amount(o_csoilOC,i,j,aadj,loadrf(i_oc),concrf(i_oc))
                IF(i_t2>0.AND.outvarstatus(o_csoilT2)) CALL calculate_class_outvar_add_amount(o_csoilT2,i,j,aadj,loadrf(i_t2),concrf(i_t2))
              ENDIF
            ENDIF
            IF(conduct%simN)THEN
              IF(outvarstatus(o_soilNatm)) CALL calculate_class_outvar_add(o_soilNatm,i,j,a,(atmdepload1(i_in)+atmdepload2(i_in))/classarea)    !Note, already scaled to fpfrac (kg)
              DO isl=1,3
                IF(outvarstatus(o_pfN(isl))) CALL calculate_class_outvar_add(o_pfN(isl),i,j,aadj,soilstate%fastN(isl,j,i))
                IF(outvarstatus(o_phN(isl))) CALL calculate_class_outvar_add(o_phN(isl),i,j,aadj,soilstate%humusN(isl,j,i))
                IF(outvarstatus(o_csoillayerIN(isl))) CALL calculate_class_outvar_add_amount(o_csoillayerIN(isl),i,j,aadj,soilstate%conc(i_in,isl,j,i)*soilstate%water(isl,j,i),soilstate%conc(i_in,isl,j,i))
                IF(outvarstatus(o_psoilIN(isl))) CALL calculate_class_outvar_add(o_psoilIN(isl),i,j,aadj,soilstate%conc(i_in,isl,j,i)*soilstate%water(isl,j,i))
                IF(outvarstatus(o_psoilON(isl))) CALL calculate_class_outvar_add(o_psoilON(isl),i,j,aadj,soilstate%conc(i_in,isl,j,i)*soilstate%water(isl,j,i))
              ENDDO
            ENDIF
            IF(conduct%simP)THEN
              IF(outvarstatus(o_ispp)) CALL calculate_class_outvar_add(o_ispp,i,j,aadj,soilstate%PPrelpool(j,i))
              IF(outvarstatus(o_soilPatm)) CALL calculate_class_outvar_add(o_soilPatm,i,j,a,(atmdepload1(i_sp)+atmdepload2(i_sp))/classarea)
              DO isl=1,3
                IF(outvarstatus(o_pfP(isl))) CALL calculate_class_outvar_add(o_pfP(isl),i,j,aadj,soilstate%fastP(isl,j,i))
                IF(outvarstatus(o_phP(isl))) CALL calculate_class_outvar_add(o_phP(isl),i,j,aadj,soilstate%humusP(isl,j,i))
                IF(outvarstatus(o_ppP(isl))) CALL calculate_class_outvar_add(o_ppP(isl),i,j,aadj,soilstate%partP(isl,j,i))
                IF(outvarstatus(o_psoilSP(isl))) CALL calculate_class_outvar_add(o_psoilSP(isl),i,j,aadj,soilstate%conc(i_sp,isl,j,i)*soilstate%water(isl,j,i))
              ENDDO
            ENDIF
            IF(conduct%simC)THEN
              DO isl=1,3
                IF(outvarstatus(o_pfC(isl))) CALL calculate_class_outvar_add(o_pfC(isl),i,j,aadj,soilstate%fastC(isl,j,i))
                IF(outvarstatus(o_phC(isl))) CALL calculate_class_outvar_add(o_phC(isl),i,j,aadj,soilstate%humusC(isl,j,i))
              ENDDO
            ENDIF
            IF(conduct%simS)THEN
              IF(outvarstatus(o_isps)) CALL calculate_class_outvar_add(o_isps,i,j,aadj,soilstate%Srelpool(j,i))
            ENDIF
            IF(simulate%substance(i_t1))THEN
              DO isl=1,3
                IF(outvarstatus(o_ppT1(isl))) CALL calculate_class_outvar_add(o_ppT1(isl),i,j,aadj,soilstate%partT1(isl,j,i))
                IF(outvarstatus(o_psoilT1(isl))) CALL calculate_class_outvar_add(o_psoilT1(isl),i,j,aadj,soilstate%conc(i_t1,isl,j,i)*soilstate%water(isl,j,i))
              ENDDO
              IF(outvarstatus(o_T1sf)) CALL calculate_class_outvar_add(o_T1sf,i,j,aadj,miscstate%partT1sf(j,i))
            ENDIF
            !Soil load output, mm*mg/L
            IF((conduct%simN.OR.conduct%simP).AND.SUM(outvarindex(285:320))>0)THEN
              soillgrossload(1,:)=cruralflow(:)*horizontalflows(1)+cruralflow(:)*horizontalflows(2)+  &
                (atmdepload1+atmdepload2+cultivload(1,:)+cultivload(2,:))/basin(i)%area*1.E6/a
              soillnetload(1,:)=cverticalflows(2,:)*verticalflows(2)+crunoff1(:)*runofflows(1)+  &
                crunoff2(:)*runofflows(2)+crunoffd(:)*(runofflows(4)+runofflows(5))+   &
                csrunoff(:)*totalsurfaceflow
              soillgrossload(2,:)=cverticalflows(2,:)*verticalflows(2)+cruralflow(:)*horizontalflows(3)
              soillnetload(2,:)=crunoff3(:)*runofflows(3)+crunoffd(:)*runofflows(6)
              soillgrossload(3,:)=cverticalflows(2,:)*verticalflows(2)+cruralflow(:)*horizontalflows(3)+crunoffd(:)*(runofflows(4)+runofflows(5))
              soillnetload(3,:)=crunoff3(:)*runofflows(3)+crunoffd(:)*(runofflows(4)+runofflows(5)+runofflows(6))
              IF(conduct%simP)THEN
                CALL calculate_class_outvar_add_accumulate(285,i,j,a,soillgrossload(1,i_sp))
                CALL calculate_class_outvar_add_accumulate(286,i,j,a,soillnetload(1,i_sp))
                CALL calculate_class_outvar_add_accumulate(287,i,j,a,soillgrossload(1,i_pp))
                CALL calculate_class_outvar_add_accumulate(288,i,j,a,soillnetload(1,i_pp))
                IF(outvarstatus(289)) CALL calculate_class_outvar_add_accumulate(289,i,j,a,soillgrossload(1,i_sp)+soillgrossload(1,i_pp))
                IF(outvarstatus(290)) CALL calculate_class_outvar_add_accumulate(290,i,j,a,soillnetload(1,i_sp)+soillnetload(1,i_pp))
                CALL calculate_class_outvar_add_accumulate(297,i,j,a,soillgrossload(2,i_sp))
                CALL calculate_class_outvar_add_accumulate(298,i,j,a,soillnetload(2,i_sp))
                CALL calculate_class_outvar_add_accumulate(299,i,j,a,soillgrossload(2,i_pp))
                CALL calculate_class_outvar_add_accumulate(300,i,j,a,soillnetload(2,i_pp))
                IF(outvarstatus(301)) CALL calculate_class_outvar_add_accumulate(301,i,j,a,soillgrossload(2,i_sp)+soillgrossload(2,i_pp))
                IF(outvarstatus(302)) CALL calculate_class_outvar_add_accumulate(302,i,j,a,soillnetload(2,i_sp)+soillnetload(2,i_pp))
                CALL calculate_class_outvar_add_accumulate(317,i,j,a,soillgrossload(3,i_sp))
                CALL calculate_class_outvar_add_accumulate(318,i,j,a,soillnetload(3,i_sp))
                CALL calculate_class_outvar_add_accumulate(319,i,j,a,soillgrossload(3,i_pp))
                CALL calculate_class_outvar_add_accumulate(320,i,j,a,soillnetload(3,i_pp))
                IF(outvarstatus(303)) CALL calculate_class_outvar_add_accumulate(303,i,j,a,soillgrossload(3,i_sp)+soillgrossload(3,i_pp))
                IF(outvarstatus(304)) CALL calculate_class_outvar_add_accumulate(304,i,j,a,soillnetload(3,i_sp)+soillnetload(3,i_pp))
              ENDIF
              IF(conduct%simN)THEN
                CALL calculate_class_outvar_add_accumulate(291,i,j,a,soillgrossload(2,i_in))
                CALL calculate_class_outvar_add_accumulate(292,i,j,a,soillnetload(2,i_in))
                CALL calculate_class_outvar_add_accumulate(293,i,j,a,soillgrossload(2,i_on))
                CALL calculate_class_outvar_add_accumulate(294,i,j,a,soillnetload(2,i_on))
                IF(outvarstatus(295)) CALL calculate_class_outvar_add_accumulate(295,i,j,a,soillgrossload(2,i_in)+soillgrossload(2,i_on))
                IF(outvarstatus(296)) CALL calculate_class_outvar_add_accumulate(296,i,j,a,soillnetload(2,i_in)+soillnetload(2,i_on))
                CALL calculate_class_outvar_add_accumulate(305,i,j,a,soillgrossload(1,i_in))
                CALL calculate_class_outvar_add_accumulate(306,i,j,a,soillnetload(1,i_in))
                CALL calculate_class_outvar_add_accumulate(307,i,j,a,soillgrossload(1,i_on))
                CALL calculate_class_outvar_add_accumulate(308,i,j,a,soillnetload(1,i_on))
                IF(outvarstatus(309)) CALL calculate_class_outvar_add_accumulate(309,i,j,a,soillgrossload(1,i_in)+soillgrossload(1,i_on))
                IF(outvarstatus(310)) CALL calculate_class_outvar_add_accumulate(310,i,j,a,soillnetload(1,i_in)+soillnetload(1,i_on))
                CALL calculate_class_outvar_add_accumulate(311,i,j,a,soillgrossload(3,i_in))
                CALL calculate_class_outvar_add_accumulate(312,i,j,a,soillnetload(3,i_in))
                CALL calculate_class_outvar_add_accumulate(313,i,j,a,soillgrossload(3,i_on))
                CALL calculate_class_outvar_add_accumulate(314,i,j,a,soillnetload(3,i_on))
                IF(outvarstatus(315)) CALL calculate_class_outvar_add_accumulate(315,i,j,a,soillgrossload(3,i_in)+soillgrossload(3,i_on))
                IF(outvarstatus(316)) CALL calculate_class_outvar_add_accumulate(316,i,j,a,soillnetload(3,i_in)+soillnetload(3,i_on))
              ENDIF
            ENDIF
            !Runoff output of different kinds
            runoffd = SUM(runofflows(4:6))
            CALL calculate_class_outvar_add(o_ro1,i,j,aadj,runofflows(1))
            CALL calculate_class_outvar_add(o_ro2,i,j,aadj,runofflows(2))
            CALL calculate_class_outvar_add(o_ro3,i,j,aadj,runofflows(3))
            CALL calculate_class_outvar_add(o_rod,i,j,aadj,runoffd)
            CALL calculate_class_outvar_add(o_ros,i,j,aadj,totalsurfaceflow)
            CALL calculate_class_outvar_add(o_ros1,i,j,aadj,surfaceflow(1))
            CALL calculate_class_outvar_add(o_ros2,i,j,aadj,surfaceflow(2))
            IF(simulate%substance(i_t1))THEN
              IF(outvarstatus(o_crosT1)) CALL calculate_class_outvar_add_amount(o_crosT1,i,j,aadj,csrunoff(i_t1)*totalsurfaceflow,csrunoff(i_t1))
              IF(outvarstatus(o_crodT1)) CALL calculate_class_outvar_add_amount(o_crodT1,i,j,aadj,crunoffd(i_t1)*runoffd,crunoffd(i_t1))
              IF(outvarstatus(o_cro1T1)) CALL calculate_class_outvar_add_amount(o_cro1T1,i,j,aadj,crunoff1(i_t1)*runofflows(1),crunoff1(i_t1))
              IF(outvarstatus(o_cro2T1)) CALL calculate_class_outvar_add_amount(o_cro2T1,i,j,aadj,crunoff2(i_t1)*runofflows(2),crunoff2(i_t1))
              IF(outvarstatus(o_cro3T1)) CALL calculate_class_outvar_add_amount(o_cro3T1,i,j,aadj,crunoff3(i_t1)*runofflows(3),crunoff3(i_t1))
            ENDIF
            IF(.NOT.floodplainclass)THEN    !floodplain runoff goes to flooded water not to local stream
              runofftsi = runofftsi + runoffd*a + totalsurfaceflow*a +runofflows(1)*a+runofflows(2)*a+runofflows(3)*a
              crunofftsi(:) = crunofftsi(:) + crunoffd(:)*runoffd*a + csrunoff(:)*totalsurfaceflow*a + crunoff1(:)*runofflows(1)*a + crunoff2(:)*runofflows(2)*a + crunoff3(:)*runofflows(3)*a    !accumulation of amount
            ENDIF
            runf = runofflows(1) + runofflows(2) + runofflows(3) + totalsurfaceflow + runoffd
            runoffi=runoffi+runf*aadj
            IF(outvarstatus(o_crun)) CALL calculate_class_outvar_add(o_crun,i,j,aadj,runf)
            IF(outvarstatus(o_crun2)) CALL calculate_class_outvar_add(o_crun2,i,j,aadj,runf)
            IF(numsubstances>0)THEN
              IF(outvarstatus(o_crunT1).OR.outvarstatus(o_crunT2).OR.outvarstatus(o_crunIN).OR.outvarstatus(o_crunON).OR.outvarstatus(o_crunTN).OR.outvarstatus(o_crunSP).OR.outvarstatus(o_crunPP).OR.outvarstatus(o_crunTP).OR.outvarstatus(o_crunOC).OR.outvarstatus(o_crunSS))THEN
                loadrf(:) = crunoff1(:)*runofflows(1) + crunoff2(:)*runofflows(2) + crunoff3(:)*runofflows(3) + csrunoff(:)*totalsurfaceflow + crunoffd(:)*runoffd    !amount substance of runoff
                IF(runf>0)THEN
                  concrf(:) = loadrf(:)/runf
                ELSE
                  concrf = 0.
                ENDIF
                IF(simulate%substance(i_t1).AND.outvarstatus(o_crunT1)) CALL calculate_class_outvar_add_amount(o_crunT1,i,j,aadj,loadrf(i_t1),concrf(i_t1))
                IF(simulate%substance(i_t2).AND.outvarstatus(o_crunT2)) CALL calculate_class_outvar_add_amount(o_crunT2,i,j,aadj,loadrf(i_t2),concrf(i_t2))
                IF(conduct%simN)THEN
                  IF(outvarstatus(o_crunIN)) CALL calculate_class_outvar_add_amount(o_crunIN,i,j,aadj,loadrf(i_in),concrf(i_in))
                  IF(outvarstatus(o_crunON)) CALL calculate_class_outvar_add_amount(o_crunON,i,j,aadj,loadrf(i_on),concrf(i_on))
                  IF(outvarstatus(o_crunTN)) CALL calculate_class_outvar_add_amount(o_crunTN,i,j,aadj,loadrf(i_in)+loadrf(i_on),concrf(i_in)+concrf(i_on))
                ENDIF
                IF(conduct%simP)THEN
                  IF(outvarstatus(o_crunSP)) CALL calculate_class_outvar_add_amount(o_crunSP,i,j,aadj,loadrf(i_sp),concrf(i_sp))
                  IF(outvarstatus(o_crunPP)) CALL calculate_class_outvar_add_amount(o_crunPP,i,j,aadj,loadrf(i_pp),concrf(i_pp))
                  IF(outvarstatus(o_crunTP)) CALL calculate_class_outvar_add_amount(o_crunTP,i,j,aadj,loadrf(i_sp)+loadrf(i_pp),concrf(i_sp)+concrf(i_pp))
                ENDIF
                IF(simulate%substance(i_oc).AND.outvarstatus(o_crunOC)) CALL calculate_class_outvar_add_amount(o_crunOC,i,j,aadj,loadrf(i_oc),concrf(i_oc))
                IF(simulate%substance(i_ss).AND.outvarstatus(o_crunSS)) CALL calculate_class_outvar_add_amount(o_crunSS,i,j,aadj,loadrf(i_ss),concrf(i_ss))
              ENDIF
            ENDIF
          ENDIF

          IF(conductload.AND..NOT.wetlandclass)THEN
            Lstream(j,:) = (runofflows(1)*crunoff1(:)+runofflows(2)*crunoff2(:)+  &
                            runofflows(3)*crunoff3(:)+totalsurfaceflow*csrunoff(:)+    &
                            crunoffd(:)*runoffd)*classarea*floodplain_area_fraction    !Load in runoff (kg/timestep) (incl. surface runoff and tile runoff)
          ENDIF

          IF(conductwb)THEN
            IF(wetlandclass)THEN
              IF(j==slc_iwet)THEN
                wbflows(w_preciwet,i) = prec * a * basin(i)%area * 1.E-3   !m3/ts
                wbflows(w_evapiwet,i) = evap * a * basin(i)%area * 1.E-3     !m3/ts
              ELSEIF(j==slc_owet)THEN
                wbflows(w_precowet,i) = prec * a * basin(i)%area * 1.E-3   !m3/ts
                wbflows(w_evapowet,i) = evap * a * basin(i)%area * 1.E-3     !m3/ts
              ENDIF
            ELSE  !land class (and floodplain part of these classes)
              wbflows(w_sfallsnow,i)= wbflows(w_sfallsnow,i) + snowfall * aadj * old_snow_part
              wbflows(w_smeltsl1,i) = wbflows(w_smeltsl1,i) + infiltrationflows(1) * infiltrationflows(2) * aadj
              wbflows(w_smeltmp1,i) = wbflows(w_smeltmp1,i) + infiltrationflows(1) * infiltrationflows(4) * aadj
              wbflows(w_smeltmp2,i) = wbflows(w_smeltmp2,i) + infiltrationflows(1) * infiltrationflows(5) * aadj
              wbflows(w_smeltmp3,i) = wbflows(w_smeltmp3,i) + infiltrationflows(1) * infiltrationflows(6) * aadj
              wbflows(w_infrain,i)  = wbflows(w_infrain,i) + (1.-infiltrationflows(1)-infiltrationflows(7)) * infiltrationflows(2) * aadj
              wbflows(w_rainmp1,i)  = wbflows(w_rainmp1,i) + (1.-infiltrationflows(1)-infiltrationflows(7)) * infiltrationflows(4) * aadj
              wbflows(w_rainmp2,i)  = wbflows(w_rainmp2,i) + (1.-infiltrationflows(1)-infiltrationflows(7)) * infiltrationflows(5) * aadj
              wbflows(w_rainmp3,i)  = wbflows(w_rainmp3,i) + (1.-infiltrationflows(1)-infiltrationflows(7)) * infiltrationflows(6) * aadj
              wbflows(w_evap1,i)    = wbflows(w_evap1,i) + evapflows(1) * aadj
              wbflows(w_evap2,i)    = wbflows(w_evap2,i) + evapflows(2) * aadj
              wbflows(w_evap3,i)    = wbflows(w_evap3,i) + evapflows(3) * aadj * old_snow_part
              IF(.NOT. floodplainclass)THEN
                wbflows(w_smeltsr,i)  = wbflows(w_smeltsr,i)  + infiltrationflows(1) * infiltrationflows(3) * aadj
                wbflows(w_rainsr,i)   = wbflows(w_rainsr,i)  + (1.-infiltrationflows(1)-infiltrationflows(7)) * infiltrationflows(3) * aadj
                wbflows(w_gwrunf1,i)  = wbflows(w_gwrunf1,i) + runofflows(1) * a
                wbflows(w_gwrunf2,i)  = wbflows(w_gwrunf2,i) + runofflows(2) * a
                wbflows(w_gwrunf3,i)  = wbflows(w_gwrunf3,i) + runofflows(3) * a
                wbflows(w_tile1,i)    = wbflows(w_tile1,i) + runofflows(4) * a
                wbflows(w_tile2,i)    = wbflows(w_tile2,i) + runofflows(5) * a
                wbflows(w_tile3,i)    = wbflows(w_tile3,i) + runofflows(6) * a
                wbflows(w_surfrf,i)   = wbflows(w_surfrf,i) + runofflows(7) * a
              ELSE
                IF(floodplainclass.AND.j==slc_mriver)THEN
                  wbfpflows(w_grf1mrfp,i)  = runofflows(1) * aadj
                  wbfpflows(w_grf2mrfp,i)  = runofflows(2) * aadj
                  wbfpflows(w_grf3mrfp,i)  = runofflows(3) * aadj
                  wbfpflows(w_trf1mrfp,i)  = runofflows(4) * aadj
                  wbfpflows(w_trf2mrfp,i)  = runofflows(5) * aadj
                  wbfpflows(w_trf3mrfp,i)  = runofflows(6) * aadj
                  wbfpflows(w_smtsrmrfp,i)  = infiltrationflows(1) * infiltrationflows(3) * aadj
                  wbfpflows(w_rtsrmrfp,i)   = (1.-infiltrationflows(1)) * infiltrationflows(3) * aadj
                  wbfpflows(w_srftmrfp,i)   = runofflows(7) * aadj
                ELSEIF(floodplainclass.AND.j==slc_olake)THEN
                  wbfpflows(w_grf1olfp,i)  = runofflows(1) * aadj
                  wbfpflows(w_grf2olfp,i)  = runofflows(2) * aadj
                  wbfpflows(w_grf3olfp,i)  = runofflows(3) * aadj
                  wbfpflows(w_trf1olfp,i)  = runofflows(4) * aadj
                  wbfpflows(w_trf2olfp,i)  = runofflows(5) * aadj
                  wbfpflows(w_trf3olfp,i)  = runofflows(6) * aadj
                  wbfpflows(w_smtsrolfp,i)  = infiltrationflows(1) * infiltrationflows(3) * aadj
                  wbfpflows(w_rtsrolfp,i)   = (1.-infiltrationflows(1)) * infiltrationflows(3) * aadj
                  wbfpflows(w_srftolfp,i)   = runofflows(7) * aadj
                ENDIF
              ENDIF
              wbflows(w_perc1,i)    = wbflows(w_perc1,i) + verticalflows(1) * aadj
              wbflows(w_perc2,i)    = wbflows(w_perc2,i) + verticalflows(2) * aadj
              wbflows(w_upwell1,i)  = wbflows(w_upwell1,i) + (verticalflows(3)+verticalflows(5)) * aadj
              wbflows(w_upwell2,i)  = wbflows(w_upwell2,i) + (verticalflows(4)+verticalflows(6)) * aadj
              wbflows(w_rural1,i)   = wbflows(w_rural1,i) + horizontalflows(1) * aadj
              wbflows(w_rural2,i)   = wbflows(w_rural2,i) + horizontalflows(2) * aadj
              wbflows(w_rural3,i)   = wbflows(w_rural3,i) + horizontalflows(3) * aadj
              wbirrflows(w_apply1,i)  = wbirrflows(w_apply1,i) + irrappl * aadj
              IF(classmodel(j)==glacier_model)THEN
                wbstores(w_glacier,i) = frozenstate%glacvol(i)*genpar(m_glacdens)
                wbflows(w_stoice,i)   = glacierflows(1)
                wbflows(w_precglac,i) = glacierflows(2)
                wbflows(w_gmeltsl1,i) = infiltrationflows(7) * infiltrationflows(2) * a
                wbflows(w_gmeltsr,i)  = infiltrationflows(7) * infiltrationflows(3) * a
                wbflows(w_gmeltmp1,i) = infiltrationflows(7) * infiltrationflows(4) * a
                wbflows(w_gmeltmp2,i) = infiltrationflows(7) * infiltrationflows(5) * a
                wbflows(w_gmeltmp3,i) = infiltrationflows(7) * infiltrationflows(6) * a
                wbflows(w_evap4,i)    = wbflows(w_evap4,i) + evapflows(4) * a * (1. - old_snow_part)
              ENDIF
              IF(floodplainclass.AND.j==slc_mriver)THEN
                wbfpflows(w_emrfp,i)    = evapflows(4)    !these are in m3
                wbfpflows(w_stomrfp,i)  = floodplainflows(1)
                wbfpflows(w_pmrfp,i)    = floodplainflows(2)
                wbfpflows(w_infmrfp,i)  = floodplainflows(3)
              ELSEIF(floodplainclass.AND.j==slc_olake)THEN
                wbfpflows(w_eolfp,i)    = evapflows(4)
                wbfpflows(w_stoolfp,i)  = floodplainflows(1)
                wbfpflows(w_polfp,i)    = floodplainflows(2)
                wbfpflows(w_infolfp,i)  = floodplainflows(3)
              ENDIF
            ENDIF
          ENDIF
        ENDIF !a>0
      ENDDO   !j

      !Calculate average of soil pools for classes with crops in rotation
      IF(dayno==360)THEN
        IF(numrotations>0)THEN
          IF(i_sp>0) CALL croprotation_soilpoolaverage(i,numrotations,soilstate%humusP)
          IF(i_sp>0) CALL croprotation_soilpoolaverage(i,numrotations,soilstate%partP)
          IF(i_in>0) CALL croprotation_soilpoolaverage(i,numrotations,soilstate%humusN)
          IF(i_oc>0) CALL croprotation_soilpoolaverage(i,numrotations,soilstate%humusC)
        ENDIF
      ENDIF

      !Calculate subbasin mean over land area (or second or third soillayer area)
      WHERE(asum>0)
        divasum = 1. / asum
      ELSEWHERE
        divasum = 0.
      ENDWHERE
      CALL calculate_class_outvar_finish(o_snow,i,asum(1))
      CALL calculate_class_outvar_finish(o_snowdepth,i,asum(1))
      CALL calculate_class_outvar_finish(o_evapsnow,i,1.)
      CALL calculate_class_outvar_finish(o_snowcover,i,asum(1))
      CALL calculate_class_outvar_finish(o_snowmax,i,asum(1))
      CALL calculate_class_outvar_finish(o_snowmelt,i,asum(1))
      CALL calculate_class_outvar_finish(o_snht,i,asum(1))
      CALL calculate_class_outvar_finish(o_snte,i,asum(1))
      CALL calculate_class_outvar_finish(o_snts,i,asum(1))
      CALL calculate_class_outvar_finish(o_snowdens,i,snowdepthi)
      CALL calculate_class_outvar_finish(o_snwc,i,asum(1))
      CALL calculate_class_outvar_finish_scale(o_snowdens,i,0.1)  !snow density (g/cm3=cm/cm=1)
      IF(SUM(outvarindex(267:274))>0)THEN      !Forest and open land snow variables
        IF(area_open>0.)THEN
          area_open=1./area_open
          snowvari(:,1) = snowvari(:,1) * area_open
        ELSE
          snowvari(:,1) = missing_value
        ENDIF
        IF(area_forest>0.)THEN
          area_forest=1./area_forest
          snowvari(:,2) = snowvari(:,2) * area_forest
        ELSE
          snowvari(:,2) = missing_value
        ENDIF
      ENDIF
      CALL calculate_class_outvar_finish(o_infi,i,asum(1))
      CALL calculate_class_outvar_finish(o_landevap,i,asum(1))
      CALL calculate_class_outvar_finish(o_soilfrost,i,asum(1))
      CALL calculate_class_outvar_finish(o_grwlevel,i,asum(1))
      CALL calculate_class_outvar_finish(o_soim,i,asum(1))
      CALL calculate_class_outvar_finish(o_soim12,i,asum(1))
      CALL calculate_class_outvar_finish(o_sml9,i,asum(1))
      CALL calculate_class_outvar_finish(o_sml0,i,asum(1))
      CALL calculate_class_outvar_finish(o_sml1,i,asum(1))
      CALL calculate_class_outvar_finish(o_sml2,i,asum(2))
      CALL calculate_class_outvar_finish(o_sml3,i,asum(3))
      CALL calculate_class_outvar_finish(o_smrz,i,asum(1))
      CALL calculate_class_outvar_finish(o_sm13,i,asum(1))
      CALL calculate_class_outvar_finish(o_soildef,i,asum(1))
      CALL calculate_class_outvar_finish(o_smffc,i,asum(1))
      CALL calculate_class_outvar_finish(o_smfdep,i,asum(1))
      CALL calculate_class_outvar_finish(o_smrzfdep,i,asum(1))
      CALL calculate_class_outvar_finish(o_smfpw,i,asum(1))
      CALL calculate_class_outvar_finish(o_smrzfpw,i,asum(1))
      IF(modeloption(p_soilleakage)==0)THEN
        IF(outvarstatus(o_csoilIN)) CALL calculate_class_outvar_finish(o_csoilIN,i,SUM(soilwateri))  !inorgN conc soil (mg/L)
        CALL calculate_class_outvar_finish_scale(o_csoilIN,i,1.E3)  !inorgN conc soil (ug/L)
        IF(outvarstatus(o_csoilOC)) CALL calculate_class_outvar_finish(o_csoilOC,i,SUM(soilwateri))        !orgC conc soil (mg/L)
        IF(outvarstatus(o_csoilT1)) CALL calculate_class_outvar_finish(o_csoilT1,i,SUM(soilwateri))
        IF(outvarstatus(o_csoilT2)) CALL calculate_class_outvar_finish(o_csoilT2,i,SUM(soilwateri))
      ELSE
        CALL set_class_outvar_missing(o_csoilIN,i)
        CALL set_class_outvar_missing(o_csoilOC,i)
        CALL set_class_outvar_missing(o_csoilT1,i)
        CALL set_class_outvar_missing(o_csoilT2,i)
      ENDIF
      DO isl = 1,3
        CALL calculate_class_outvar_finish(o_sltmp(isl),i,asum(isl))
        IF(modeloption(p_soilleakage)==0)THEN
          CALL calculate_class_outvar_finish(o_csoillayerIN(isl),i,soilwateri(isl))  !inorgN conc soillayer (mg/L)
          CALL calculate_class_outvar_finish_scale(o_csoillayerIN(isl),i,1.E3)  !inorgN conc soillayer (ug/L)
          CALL calculate_class_outvar_finish(o_pfN(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_phN(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_pfP(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_phP(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_ppP(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_pfC(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_phC(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_ppT1(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_psoilIN(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_psoilON(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_psoilSP(isl),i,asum(isl))
          CALL calculate_class_outvar_finish(o_psoilT1(isl),i,asum(isl))
        ELSE
          CALL set_class_outvar_missing(o_csoillayerIN(isl),i)
          CALL set_class_outvar_missing(o_pfN(isl),i)
          CALL set_class_outvar_missing(o_phN(isl),i)
          CALL set_class_outvar_missing(o_pfP(isl),i)
          CALL set_class_outvar_missing(o_phP(isl),i)
          CALL set_class_outvar_missing(o_ppP(isl),i)
          CALL set_class_outvar_missing(o_pfC(isl),i)
          CALL set_class_outvar_missing(o_phC(isl),i)
          CALL set_class_outvar_missing(o_ppT1(isl),i)
          CALL set_class_outvar_missing(o_psoilIN(isl),i)
          CALL set_class_outvar_missing(o_psoilON(isl),i)
          CALL set_class_outvar_missing(o_psoilSP(isl),i)
          CALL set_class_outvar_missing(o_psoilT1(isl),i)
        ENDIF
      ENDDO
      CALL calculate_class_outvar_finish(o_soiltmp,i,asum(1))
      CALL calculate_class_outvar_finish(o_dtmp,i,asum(1))
      IF(modeloption(p_soilleakage)==0)THEN
        CALL calculate_class_outvar_finish(o_T1sf,i,asum(1))
        CALL calculate_class_outvar_finish(o_soildenitr,i,asum(1))
        CALL calculate_class_outvar_finish(o_cropNupt,i,asum(1))
        CALL calculate_class_outvar_finish(o_degrfN,i,asum(1))
        CALL calculate_class_outvar_finish(o_soilNatm,i,asum(1))
        CALL calculate_class_outvar_finish(o_soilPatm,i,asum(1))
        CALL calculate_class_outvar_finish(o_isps,i,asum(1))
        CALL calculate_class_outvar_finish(o_ispp,i,asum(1))
        IF(outvarindex(o_crosT1)>0) CALL calculate_class_outvar_finish(o_crosT1,i,outvar(i,outvarindex(o_ros)))  !Need to be called before o_ros is finished!
        IF(outvarindex(o_crodT1)>0) CALL calculate_class_outvar_finish(o_crodT1,i,outvar(i,outvarindex(o_rod)))  !Need to be called before o_rod is finished!
        IF(outvarindex(o_cro1T1)>0) CALL calculate_class_outvar_finish(o_cro1T1,i,outvar(i,outvarindex(o_ro1)))  !Need to be called before o_ro1 is finished!
        IF(outvarindex(o_cro2T1)>0) CALL calculate_class_outvar_finish(o_cro2T1,i,outvar(i,outvarindex(o_ro2)))  !Need to be called before o_ro2 is finished!
        IF(outvarindex(o_cro3T1)>0) CALL calculate_class_outvar_finish(o_cro3T1,i,outvar(i,outvarindex(o_ro3)))  !Need to be called before o_ro3 is finished!
      ELSE
        CALL set_class_outvar_missing(o_T1sf,i)
        CALL set_class_outvar_missing(o_soildenitr,i)
        CALL set_class_outvar_missing(o_cropNupt,i)
        CALL set_class_outvar_missing(o_degrfN,i)
        CALL set_class_outvar_missing(o_soilNatm,i)
        CALL set_class_outvar_missing(o_soilPatm,i)
        CALL set_class_outvar_missing(o_isps,i)
        CALL set_class_outvar_missing(o_ispp,i)
        CALL set_class_outvar_missing(o_crosT1,i)
        CALL set_class_outvar_missing(o_crodT1,i)
        CALL set_class_outvar_missing(o_cro1T1,i)
        CALL set_class_outvar_missing(o_cro2T1,i)
        CALL set_class_outvar_missing(o_cro3T1,i)
      ENDIF
      CALL calculate_class_outvar_finish(o_ros,i,asum(1))
      CALL calculate_class_outvar_finish(o_ros1,i,asum(1))
      CALL calculate_class_outvar_finish(o_ros2,i,asum(1))
      CALL calculate_class_outvar_finish(o_rod,i,asum(1))
      CALL calculate_class_outvar_finish(o_ro1,i,asum(1))
      CALL calculate_class_outvar_finish(o_ro2,i,asum(2))
      CALL calculate_class_outvar_finish(o_ro3,i,asum(3))
      CALL calculate_class_outvar_finish(o_crun,i,asum(1))
      CALL calculate_class_outvar_finish(o_crun2,i,asum(1))
      IF(outvarstatus(o_crun2)) CALL calculate_class_outvar_finish_scale(o_crun2,i,1.E6/seconds_per_timestep)
      CALL calculate_class_outvar_finish(o_crunT1,i,runoffi)  !T1 conc in runoff
      CALL calculate_class_outvar_finish(o_crunT2,i,runoffi)  !T2 in runoff
      CALL calculate_class_outvar_finish(o_crunIN,i,runoffi)  !IN conc in runoff [mg/L]
      CALL calculate_class_outvar_finish_scale(o_crunIN,i,1.E3)  !IN conc in runoff [ug/L]
      CALL calculate_class_outvar_finish(o_crunON,i,runoffi)  !ON conc in runoff [mg/L]
      CALL calculate_class_outvar_finish_scale(o_crunON,i,1.E3)  !ON conc in runoff [ug/L]
      CALL calculate_class_outvar_finish(o_crunTN,i,runoffi)  !TN conc in runoff [mg/L]
      CALL calculate_class_outvar_finish_scale(o_crunTN,i,1.E3)  !TN conc in runoff [ug/L]
      CALL calculate_class_outvar_finish(o_crunSP,i,runoffi)  !SP conc in runoff [mg/L]
      CALL calculate_class_outvar_finish_scale(o_crunSP,i,1.E3)  !SP conc in runoff [ug/L]
      CALL calculate_class_outvar_finish(o_crunPP,i,runoffi)  !PP conc in runoff [mg/L]
      CALL calculate_class_outvar_finish_scale(o_crunPP,i,1.E3)  !PP conc in runoff [ug/L]
      CALL calculate_class_outvar_finish(o_crunTP,i,runoffi)  !TP conc in runoff [mg/L]
      CALL calculate_class_outvar_finish_scale(o_crunTP,i,1.E3)  !TP conc in runoff [ug/L]
      CALL calculate_class_outvar_finish(o_crunOC,i,runoffi)  !OC conc in runoff [mg/L]
      CALL calculate_class_outvar_finish(o_crunSS,i,runoffi)  !SS conc in runoff [mg/L]

      IF(conductwb)THEN
        wbstores(w_snow,i) = snowi * basin(i)%area * 1.E-3  !m3
        wbstores(w_soil1,i) = soilwateri(1) * basin(i)%area * 1.E-3  !m3
        wbstores(w_soil2,i) = soilwateri(2) * basin(i)%area * 1.E-3  !m3
        wbstores(w_soil3,i) = soilwateri(3) * basin(i)%area * 1.E-3  !m3
        wbflows(w_sfallsnow,i)= wbflows(w_sfallsnow,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_smeltsl1,i) = wbflows(w_smeltsl1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_smeltsr,i)  = wbflows(w_smeltsr,i)  * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_smeltmp1,i) = wbflows(w_smeltmp1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_smeltmp2,i) = wbflows(w_smeltmp2,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_smeltmp3,i) = wbflows(w_smeltmp3,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_infrain,i)  = wbflows(w_infrain,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_rainsr,i)   = wbflows(w_rainsr,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_rainmp1,i)  = wbflows(w_rainmp1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_rainmp2,i)  = wbflows(w_rainmp2,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_rainmp3,i)  = wbflows(w_rainmp3,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_evap1,i)    = wbflows(w_evap1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_evap2,i)    = wbflows(w_evap2,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_evap3,i)    = wbflows(w_evap3,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_gwrunf1,i)  = wbflows(w_gwrunf1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_gwrunf2,i)  = wbflows(w_gwrunf2,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_gwrunf3,i)  = wbflows(w_gwrunf3,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_tile1,i)    = wbflows(w_tile1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_tile2,i)    = wbflows(w_tile2,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_tile3,i)    = wbflows(w_tile3,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_surfrf,i)   = wbflows(w_surfrf,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_perc1,i)    = wbflows(w_perc1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_perc2,i)    = wbflows(w_perc2,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_upwell1,i)  = wbflows(w_upwell1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_upwell2,i)  = wbflows(w_upwell2,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_rural1,i)   = wbflows(w_rural1,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_rural2,i)   = wbflows(w_rural2,i) * basin(i)%area * 1.E-3   !m3/ts
        wbflows(w_rural3,i)   = wbflows(w_rural3,i) * basin(i)%area * 1.E-3   !m3/ts
        wbirrflows(w_apply1,i)  = wbirrflows(w_apply1,i) * basin(i)%area * 1.E-3   !m3/ts
        IF(conduct%glacier)THEN
          wbflows(w_gmeltsl1,i) = wbflows(w_gmeltsl1,i) * basin(i)%area * 1.E-3   !m3/ts
          wbflows(w_gmeltsr,i)  = wbflows(w_gmeltsr,i)  * basin(i)%area * 1.E-3   !m3/ts
          wbflows(w_gmeltmp1,i) = wbflows(w_gmeltmp1,i) * basin(i)%area * 1.E-3   !m3/ts
          wbflows(w_gmeltmp2,i) = wbflows(w_gmeltmp2,i) * basin(i)%area * 1.E-3   !m3/ts
          wbflows(w_gmeltmp3,i) = wbflows(w_gmeltmp3,i) * basin(i)%area * 1.E-3   !m3/ts
          wbflows(w_evap4,i)    = wbflows(w_evap4,i) * basin(i)%area * 1.E-3   !m3/ts
        ENDIF
        IF(conduct%floodplain)THEN
          wbfpflows(w_smtsrmrfp,i) = wbfpflows(w_smtsrmrfp,i) * basin(i)%area * 1.E-3  !m3/ts
          wbfpflows(w_rtsrmrfp,i)  = wbfpflows(w_rtsrmrfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_srftmrfp,i)  = wbfpflows(w_srftmrfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_grf1mrfp,i)  = wbfpflows(w_grf1mrfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_grf2mrfp,i)  = wbfpflows(w_grf2mrfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_grf3mrfp,i)  = wbfpflows(w_grf3mrfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_grf1olfp,i)  = wbfpflows(w_grf1olfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_grf2olfp,i)  = wbfpflows(w_grf2olfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_grf3olfp,i)  = wbfpflows(w_grf3olfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_trf1mrfp,i)  = wbfpflows(w_trf1mrfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_trf2mrfp,i)  = wbfpflows(w_trf2mrfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_trf3mrfp,i)  = wbfpflows(w_trf3mrfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_trf1olfp,i)  = wbfpflows(w_trf1olfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_trf2olfp,i)  = wbfpflows(w_trf2olfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_trf3olfp,i)  = wbfpflows(w_trf3olfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_smtsrolfp,i) = wbfpflows(w_smtsrolfp,i) * basin(i)%area * 1.E-3  !m3/ts
          wbfpflows(w_rtsrolfp,i)  = wbfpflows(w_rtsrolfp,i) * basin(i)%area * 1.E-3   !m3/ts
          wbfpflows(w_srftolfp,i)  = wbfpflows(w_srftolfp,i) * basin(i)%area * 1.E-3   !m3/ts
        ENDIF
      ENDIF

      !>Calculations for soil leakage concentrations from input data
      IF(modeloption(p_soilleakage)==1)THEN
        IF(conductload)THEN
          Lcultiv = 0.
          Lirrsoil = 0.
          Lrurala = 0.
          Lgrwsoil = 0.
          Latmdep = 0.  !will be set below for water classes
          Lstream = 0.
        ENDIF
        crunofftsi = get_current_soilleakage_memory(0,i,numsubstances)*runofftsi  !mg/L*mm
        !Set output runoff concentration
        IF(simulate%substance(i_t1))THEN
          CALL calculate_class_outvar_initialize(o_crunT1,i)
          CALL calculate_class_outvar_add(o_crunT1,i,1,1.,crunofftsi(i_t1))
          CALL calculate_class_outvar_finish(o_crunT1,i,runofftsi)  !T1 conc in runoff
        ENDIF
        IF(simulate%substance(i_t2))THEN
          CALL calculate_class_outvar_initialize(o_crunT2,i)
          CALL calculate_class_outvar_add(o_crunT2,i,1,1.,crunofftsi(i_t2))
          CALL calculate_class_outvar_finish(o_crunT2,i,runofftsi)  !T2 in runoff
        ENDIF
        IF(conduct%simN)THEN
          CALL calculate_class_outvar_initialize(o_crunIN,i)
          CALL calculate_class_outvar_initialize(o_crunON,i)
          CALL calculate_class_outvar_initialize(o_crunTN,i)
          CALL calculate_class_outvar_add(o_crunIN,i,1,1.,crunofftsi(i_in))
          CALL calculate_class_outvar_add(o_crunON,i,1,1.,crunofftsi(i_on))
          IF(outvarstatus(o_crunTN)) CALL calculate_class_outvar_add(o_crunTN,i,1,1.,crunofftsi(i_in)+crunofftsi(i_on))
          IF(outvarstatus(o_crunIN)) CALL calculate_class_outvar_finish(o_crunIN,i,runofftsi*1.E-3)  !IN conc in runoff [ug/L]
          IF(outvarstatus(o_crunON)) CALL calculate_class_outvar_finish(o_crunON,i,runofftsi*1.E-3)  !ON conc in runoff [ug/L]
          IF(outvarstatus(o_crunTN)) CALL calculate_class_outvar_finish(o_crunTN,i,runofftsi*1.E-3)  !TN conc in runoff [ug/L]
        ENDIF
        IF(conduct%simP)THEN
          CALL calculate_class_outvar_initialize(o_crunSP,i)
          CALL calculate_class_outvar_initialize(o_crunPP,i)
          CALL calculate_class_outvar_initialize(o_crunTP,i)
          CALL calculate_class_outvar_add(o_crunSP,i,1,1.,crunofftsi(i_sp))
          CALL calculate_class_outvar_add(o_crunPP,i,1,1.,crunofftsi(i_pp))
          IF(outvarstatus(o_crunTP)) CALL calculate_class_outvar_add(o_crunTP,i,1,1.,crunofftsi(i_sp)+crunofftsi(i_pp))
          IF(outvarstatus(o_crunSP)) CALL calculate_class_outvar_finish(o_crunSP,i,runofftsi*1.E-3)  !SP conc in runoff [ug/L]
          IF(outvarstatus(o_crunPP)) CALL calculate_class_outvar_finish(o_crunPP,i,runofftsi*1.E-3)  !PP conc in runoff [ug/L]
          IF(outvarstatus(o_crunTP)) CALL calculate_class_outvar_finish(o_crunTP,i,runofftsi*1.E-3)  !TP conc in runoff [ug/L]
        ENDIF
        IF(simulate%substance(i_oc))THEN
          CALL calculate_class_outvar_initialize(o_crunOC,i)
          CALL calculate_class_outvar_add(o_crunOC,i,1,1.,crunofftsi(i_oc))
          CALL calculate_class_outvar_finish(o_crunOC,i,runofftsi)  !OC conc in runoff [mg/L]
        ENDIF
        IF(simulate%substance(i_ss))THEN
          CALL calculate_class_outvar_initialize(o_crunSS,i)
          CALL calculate_class_outvar_add(o_crunSS,i,1,1.,crunofftsi(i_ss))
          CALL calculate_class_outvar_finish(o_crunSS,i,runofftsi)  !SS conc in runoff [mg/L]
        ENDIF
      ENDIF

      !>Routing in rivers and lakes
      !This means that the primary unit is m3/s below. The waterlevel of the lakes are measured in mm though.

      !Temperature in river and lakes (if T2 is simulated, temperature is calculated later)
      IF(modeloption(p_swtemperature)==0)THEN
        CALL calculate_water_temperature(i,tempi(i),riverstate,lakestate)
      ENDIF

      !Initial stratification of lakes
      CALL calculate_lake_hypolimnion_depth(i,lakestate,hypodepth)

      !Default output
      rivereffectivedepth = -9999.
      riverfracarea = -9999.

      !Calculate flow from soil
      qin   = runofftsi * basin(i)%area / (seconds_per_timestep * 1000.)      !m3/s
      IF(runofftsi>0.)THEN
        cin = crunofftsi / runofftsi              !mg/L (crunofftsi is amount)
      ELSE
        cin = 0.
      ENDIF
      IF(conductload)THEN
        Lpathway(:,1) = cin(:) * qin * seconds_per_timestep / 1000.   !Total load from land, point A (kg/timestep)
        !Default values used when no iwet present
        Lpathway(:,2) = Lpathway(:,1)                                 !Load passing iwet, point B (kg/timestep)
        Lpathway(:,3) = 0.                                            !Load into iwet from land, point C (kg/timestep)
        Lpathway(:,4) = 0.                                            !Load out of iwet, point D (kg/timestep)
      ENDIF

      !>Internal wetland (routing part, soil processes already calculated)
      !-----------------
      CALL calculate_internal_wetland(i,radexti(i),cloudi(i),added_water_iwet,qin,cin,frozenstate,soilstate,miscstate,outvar,wbflows,Lpathway)

      !>Local river (itype=1)
      !----------------------
      itype = 1
      localriverflow = qin
      clocalriverflow = cin
      CALL calculate_local_river(i,riverlength(itype,i),radexti(i),cloudi(i),precorg(i),cprec,sfdist,incorr,pcorricep(i), &
                                 localriverflow,clocalriverflow, &                  !flow in and out of local river
                                 eacti,area_water,prec_water,evap_water, &
                                 frozenstate,miscstate,riverstate,lakestate, &
                                 outvar,wbflows,wbstores,Latmdep,Lruralb,Lpathway)  !output

      !>Local internal lake (itype=1)
      !------------------------------
      !Initiation of lake calculations
      inflowpart = 0.
      lakeoutflow(itype) = 0.
      concout = 0.
      fnca = 0.
      fcon = 1.
      !HGDM prairie algorithm
      !HGDMFLAG = 1 !flag to activate HGDM 1 is active
      a = 0.
      IF(slc_ilake>0)THEN
        j = slc_ilake
        a = classbasin(i,j)%part
      ENDIF

      !Lake calculations
      IF(a>0)THEN      !local lake exist
        !TODO: CALL lakemodel_0()
        lakearea(itype) = a * basin(i)%area       ![m2]
        lakeareakm2 = lakearea(itype)*1.E-6       ![km2]
        qunitfactor = seconds_per_timestep * 1000. / lakearea(itype)     !m3/s->mm/timestep
        atmdepload1 = 0.
        atmdepload1_temp = 0.
        atmdepload2 = 0.

        !Forcing data set precipitation concentration on local lake
        CALL calculate_class_atmospheric_forcing(i,j,radexti(i),cloudi(i),  &
                temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
        IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution
        CALL set_class_precipitation_concentration_and_load(numsubstances,  &
                 lakeareakm2,precorg(i),temp,prec,cprec,cprecj,atmdepload1)
        CALL calculate_rain_snow_from_precipitation(i,classdata(j)%luse,prec,temp,snowfall,rainfall)
        IF(simulate%substance(i_t2)) CALL add_T2_concentration_in_precipitation_on_water(prec,temp, &
                          snowfall,rainfall,lakestate%uppertemp(itype,i),cprecj(i_t2), &
                          frozenstate%lakeicecov(itype,i))

        !Add precipitation and atmospheric deposition to lake water
        IF(prec>0) CALL add_water(numsubstances,lakestate%water(itype,i),lakestate%conc(:,itype,i),prec,cprecj)
        CALL add_deposition_to_lake_as_load(i,itype,classdata(j)%vegtype,month,lakeareakm2,   &
              genpar(m_wetspload),landpar(m_drypp,classdata(j)%luse),   &
              deposition,atmdepload1_temp,atmdepload2,lakestate)
        atmdepload1 = atmdepload1 + atmdepload1_temp  !Sum wet dep with rain and as load

        !Evaporation of lake, taking partial ice cover into account if lakeriverice model is enabled
        CALL calculate_potential_evaporation(i,j,temp,epot,radexti(i),swrad,netrad,actvap,satvap,wind,epotsnow)
        epot = epot * basincevpcorr(i)
        IF(conduct%lakeriverice) epot = epot * (1. - frozenstate%lakeicecov(itype,i))
        IF(epot>0.)THEN
          CALL calculate_actual_lake_evaporation(i,j,itype,numsubstances,temp,epot,evapl,cevapl,lakestate)
        ELSE
          evapl = 0.
        ENDIF

        !For output; accumulate outvar and set other output
        CALL calculate_class_outvar_add(o_ctmp,i,j,a,temp)
        CALL calculate_class_outvar_add(o_cprc,i,j,a,prec)
        IF(outvarstatus(o_psim)) CALL calculate_class_outvar_add(o_psim,i,j,a,prec+pcorricep(i)+icpevap)
        CALL calculate_class_outvar_add(o_rainfall,i,j,a,rainfall)
        CALL calculate_class_outvar_add(o_snowfall,i,j,a,snowfall)
        IF(outvarstatus(o_crgl)) CALL calculate_class_outvar_add(o_crgl,i,j,a,swrad)
        IF(outvarstatus(o_crnt)) CALL calculate_class_outvar_add(o_crnt,i,j,a,netrad)
        IF(outvarstatus(o_crpt)) CALL calculate_class_outvar_add(o_crpt,i,j,a,swpot)
        CALL calculate_class_outvar_add(o_epot,i,j,a,epot)
        CALL calculate_class_outvar_add(o_evap,i,j,a,evapl)
        IF(outvarstatus(o_evpt)) CALL calculate_class_outvar_add(o_evpt,i,j,a,evapl+pcorricep(i)+icpevap)
        IF(outvarstatus(o_icloss)) CALL calculate_class_outvar_add(o_icloss,i,j,a,pcorricep(i)+icpevap)
        eacti = eacti + evapl*a
        IF(simulate%substance(i_t1).AND.outvarstatus(o_cevapT1)) CALL calculate_class_outvar_add_amount(o_cevapT1,i,j,a,cevapl(i_t1)*evapl,cevapl(i_t1))
        IF(conductload)THEN
          Latmdep(j,1,:) = Latmdep(j,1,:) + atmdepload1  !flooded floodplain + lake
          Latmdep(j,2,:) = Latmdep(j,2,:) + atmdepload2  !flooded floodplain + lake
        ENDIF
        !Accumulate outvar for precipitation and evaporation on water (excluding floodplain water)
        area_water = area_water + a
        prec_water = prec_water + (rainfall+snowfall)*a
        evap_water = evap_water + evapl * a

        !Set waterbalance
        IF(conductwb)THEN
          wbflows(w_pilake,i) = prec * lakearea(itype)*1.E-3     !m3
          wbflows(w_eilake,i) = evapl * lakearea(itype)*1.E-3
        ENDIF

        !Calculate and add inflow (including point source) to local lake
        inflowpart = basin(i)%ilakecatch
        qin = localriverflow * inflowpart         ![m3/s]
        cin = clocalriverflow
        IF(conduct%simT1.OR.conduct%simT2) CALL add_tracer_point_source_to_lake(i,itype,qin,cin)
        qinmm = qin * qunitfactor        ![mm/timestep] !moved down, erronous before add_tracer_point_source
        CALL add_water(numsubstances,lakestate%water(itype,i),lakestate%conc(:,itype,i),qinmm,cin)
        IF(conductload)THEN
          Lpathway(:,10) = cin * qin * seconds_per_timestep * 1.E-3  !Load at point J, local flow to ilake (kg/timestep)
          Lpathway(:,9) = Lpathway(:,8) - Lpathway(:,10)             !Load at point I, bypassed local flow
        ENDIF
        IF(conductwb .AND. qin>0)THEN
          wbflows(w_irtoil,i) = qin * seconds_per_timestep
          wbflows(w_irtomr,i) = wbflows(w_irtomr,i) - wbflows(w_irtoil,i)
        ENDIF
        qin = qin + (prec-evapl)/qunitfactor    !All added water (net), to be used for outflow calculation

        !Calculate lake tracer (T1), temperature (T2) and ice processes
        CALL tracer_processes_in_lake(i,itype,lakestate)
        IF(simulate%substance(i_t2)) CALL T2_processes_in_lake(i,itype,temp,swrad,lakesurftemp, &
                              lakearea(itype),hypodepth(itype),frozenstate,lakestate, &
                              lakefreezeupday,freezeuparea)
        IF(conduct%lakeriverice) CALL ice_processes_in_lake(i,itype,classdata(j)%luse,snowfall,temp, &
                                      lakesurftemp,swrad,frozenstate,lakestate, &
                                      lakefreezeupday,lakebreakupday,hypodepth(itype),freezeuparea)
        IF(modeloption(p_swtemperature)==1)  CALL set_water_temperature(itype*2,i,riverstate,lakestate)

        !Calculate substances processes in lakes (in flooded floodplain water to be added here or in soilmodel4)
        CALL np_processes_in_lake(i,itype,lakearea(itype),    &
              (2.-incorr)*lakedatapar(lakedataparindex(i,itype),m_lddenitwl),genpar(m_hsatINwater),  &
              lakedatapar(lakedataparindex(i,itype),m_ldwprodn),   &
              lakedatapar(lakedataparindex(i,itype),m_ldwprodp),genpar(m_hsatTP),   &
              (2.-oncorr)*lakedatapar(lakedataparindex(i,itype),m_ldsedon),    &
              lakedatapar(lakedataparindex(i,itype),m_ldsedpp),genpar(m_sedss), &
              genpar(m_sedae),genpar(m_limsedon),genpar(m_limsedpp),genpar(m_limsedss), &
              lakedatapar(lakedataparindex(i,itype),m_ldmuptn), &
              lakedatapar(lakedataparindex(i,itype),m_ldmuptp),genpar(m_muptdep),lakestate)
        CALL oc_processes_in_lake(i,itype,lakearea(itype),lakedatapar(lakedataparindex(i,itype),m_ldwprodc),    &
              genpar(m_hsatTP),genpar(m_limsedpp),lakedatapar(lakedataparindex(i,itype),m_ldsedoc),lakestate)


        !Calculate and remove outflow from lake
        IF((modeloption(p_connectivity)==1.OR.modeloption(p_connectivity)==3) .AND. basin(i)%lakesection>0)THEN
          !DG20200625 - ilake section connectivity model -> note that qin is separated into runoff and P+E
          pein = (prec-evapl)/qunitfactor
          CALL calculate_ilakesection_outflow(i,basin(i)%subid,numsubstances,qin-pein,pein,    &
                   lakearea(itype),qunitfactor,lakeoutflow(itype),concout,  &
                   Lpathway,wbflows,lakewst(itype),fnca,fcon,lakestate)
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !ELSEIF(modeloption(p_connectivity)==2)THEN   !all ilake is hgdm so far
        ELSEIF((modeloption(p_connectivity)==2.OR.modeloption(p_connectivity)==3) .AND. basin(i)%hgdmdepth>0.)THEN
          !ELSEIF(HGDMFLAG .eq. 1) THEN !HGDM is activated
          !MIA Call HGDM algorithm for the prairies
          pein = (prec-evapl)/qunitfactor
          ! net water inouts (qin) is separated into runoff (qin-pein) and P+E (pein)
          CALL calculate_HGDM_depressions_outflow(i,basin(i)%subid,numsubstances,qin-pein, pein,   &
            lakearea(itype),basin(i)%area,qunitfactor,lakeoutflow(itype),concout,  &
            Lpathway,wbflows,lakewst(itype),fnca,lakestate)

          ! update state variables/outputs
          fcon = 1.0 -fnca
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        ELSE
          ! calculate outflow using regular ilake
          CALL calculate_ilake_outflow(i,basin(i)%subid,numsubstances,qin,    &
                 lakearea(itype),qunitfactor,lakeoutflow(itype),concout,  &
                 Lpathway,wbflows,lakewst(itype),lakestate)
        ENDIF





        !Ilake connectivity outputs
        IF(outvarindex(o_fnca)>0) outvar(i,outvarindex(o_fnca)) = fnca
        IF(outvarindex(o_fcon)>0) outvar(i,outvarindex(o_fcon)) = fcon

      ELSE    !No local lake, outflow = inflow
        IF(conductload)THEN
          Lpathway(:,9)  = Lpathway(:,8)      !Load at point I, bypassed local flow
          Lpathway(:,10) = 0.                 !Load at point J
          Lpathway(:,11) = 0.                 !Load at point K
        ENDIF
        lakewst(itype) = missing_value
        lakesurftemp(itype) = missing_value
      ENDIF   !a>0

      !Finalize local contribution from subbasin
      basinoutflow(itype) = localriverflow*(1.-inflowpart)
      IF(numsubstances>0) bconcout = clocalriverflow
      IF(a>0.) CALL add_water(numsubstances,basinoutflow(itype),bconcout,lakeoutflow(itype),concout)
      IF(conductload) Lpathway(:,12)  = basinoutflow(itype) * bconcout * seconds_per_timestep * 1.E-3  !Load at point L, outflow ilake + bypassed water (kg/timestep)
      IF(doupdate(i_tploccorr))  CALL apply_nutrientcorr(i_tploccorr,i,bconcout(i_sp),bconcout(i_pp))
      IF(doupdate(i_tnloccorr))  CALL apply_nutrientcorr(i_tnloccorr,i,bconcout(i_in),bconcout(i_on))
      IF(numsubstances>0) cbasinoutflow(:,itype) = bconcout


      !>Main river (itype=2)
      !---------------------
      itype = 2
      olakewstold=lakestate%water(2,i)  !Remember olake waterstage for last time step, to be used in war-updating
      olakewstoldi2(i)=lakestate%water(2,i)  !Remember olake waterstage for last time step, to be used in war-updating

      !Initiation of river calculations
      riverarea(itype) = 0.
      fpfrac = 0.             !default is no floodplain
      a = 0.
      j = slc_mriver    !default is 0
      IF(slc_mriver>0) a = classbasin(i,j)%part

      !Calculate inflow to river
      qin = basinoutflow(1) + accinflow(i)         ![m3/s]
      IF(qin>0 .AND. numsubstances>0)THEN
        cin = (cbasinoutflow(:,1)*basinoutflow(1) + acccinflow(:,i)) / qin
      ELSE
        cin = 0.
      ENDIF
      IF(simulate%substance(i_t2)) ctemp_T2 = cin(i_t2)  !Keep T2 concentration in memory, in case water is being extracted/added by irrigation and/or sources
      IF(numsubstances>0) netroutload(:,i) = netroutload(:,i) - qin*cin
      IF(numsubstances>0) helpload(:) = cin(:) * qin * seconds_per_timestep * 1.E-3

      !Irrigation water removal from sources and calculate irrigation to be applied the next day
      IF(conduct%irrigation)THEN
        CALL get_irrigation_parameters(i,irrigationpar)
        CALL calculate_irrigation(i,naquifers,irrigationpar,      &
                qin,lakestate%water(1:2,i),riverstate%water(itype,i),aquiferstate%water,pwneedi,irrevap,gwremi,     & !for floodplain safety
                ldremi,lrremi,rsremi,cin,irrsinkmass,  &
                lakestate%conc(:,1,i),lakestate%conc(:,2,i),riverstate%conc(:,itype,i),aquiferstate%conc, &
                soilstate,miscstate%nextirrigation,miscstate%cnextirrigation,irrigationflows,regionalirrflows,regionalirrevap, &
                aqirrloss)
        IF(conductwb)THEN
          wbirrflows(w_wdfromdg,i) = irrigationflows(1)
          wbirrflows(w_wdfromil,i) = irrigationflows(2)
          wbirrflows(w_wdfromol,i) = irrigationflows(3)
          wbirrflows(w_wdfrommr,i) = irrigationflows(4)
          wbirrflows(w_wdoutside,i) = irrigationflows(6)
          wbirrflows(w_rgrwtoir,i) = irrigationflows(7)
          wbirrflows(w_evapirrc,i) = wbirrflows(w_evapirrc,i) + irrigationflows(5)  !local sources
          wbirrflows(w_evapirrc,:) = wbirrflows(w_evapirrc,:) + regionalirrevap(:)  !regional sources, local network
        ENDIF
      ENDIF

      !Add point sources source, remove abstractions and add aquifer inflow to main river inflow
      IF(conduct%watertransfer)THEN
        CALL add_water_transfer_to_main_river(i,qin,cin,wtransfer,cwtransfer,nutrientloadflows(5))
        IF(conductload) Ltransf = nutrientloadflows(5) * cwtransfer(:,i) * 1.E-3 !kg/ts
      ENDIF
      CALL point_abstraction_from_main_river_inflow(i,itype,qin,riverstate,nutrientloadflows(3))
      CALL add_point_sources_to_main_river(i,qin,cin,Lpoints,nutrientloadflows(2))
      IF(simulate%substance(i_t2)) cin(i_t2) = ctemp_T2  !Restore T2 concentration, in case water was extracted/added by irrigation and/or point sources
      IF(conduct%simT1.OR.conduct%simT2) CALL add_tracer_point_source_to_river(i,itype,qin,cin)  !T1 and T2 point source
      IF(simulate%substance(i_t2)) ctemp_T2 = cin(i_t2)  !Keep T2 concentration in memory, in case water is being added by aquifers
      IF(modeloption(p_deepgroundwater)==2)THEN
        CALL add_aquifer_flow_to_river(i,numsubstances,qin,cin,grwtomr,grwloadmr)
        IF(conductwb) wbflows(w_rgrwtomr,i) = grwtomr
        IF(outvarindex(241)>0) outvar(i,outvarindex(241)) = grwtomr
        IF(conductload) Lgrwmr = grwloadmr
      ENDIF
      IF(numsubstances>0.AND.conductload) helpload=helpload+Ltransf+Lgrwmr+Lpoints(:,1)+Lpoints(:,2)+Lpoints(:,3)
      IF(conductload) Lpathway(:,13) = helpload  !Total load before river abstraction for point source or irrigation (kg/timestep), point MA
      IF(conductload) Lpathway(:,14) = cin(:) * qin * seconds_per_timestep * 1.E-3  !Load after adding uppstream inflow, point sources and aquifer inflow (kg/timestep), point M
      IF(simulate%substance(i_t2)) cin(i_t2) = ctemp_T2  !Restore T2 concentration, in case water was added by aquifers

      !Check for river present (river length>0)
      IF(riverlength(itype,i)>0.)THEN

        !Calculate river wetland
        IF(conduct%riverwetland)  CALL calculate_river_wetland(i,itype,numsubstances,miscstate%temp5(i),miscstate%temp30(i),qin,cin,riverstate%cwetland(:,itype,i))
        IF(conductload)  Lpathway(:,15) = qin * cin * seconds_per_timestep * 1.E-3  !Total load after main river wetland (kg/timestep), point N
        IF(simulate%substance(i_t2)) cin(i_t2) = ctemp_T2   !Restore T2 concentration, in case water was extracted/added by wetlands
        !(later, we should calculate ice and T2 in river wetlands explicitly)

        !Translation (delay) in river (in)flow
        CALL translation_in_river(i,itype,qin,cin,transq,transc,riverstate)

        !Add river inflow to river water volume
        CALL add_water(numsubstances,riverstate%water(itype,i),riverstate%conc(:,itype,i),transq * seconds_per_timestep,transc)

        !Calculate river dimensions, velocity and mean flow for use in substance processes calculation
        CALL calculate_river_characteristics(i,itype,transq,conduct%qbank,riverstate,riverdepth,riverarea(itype),riverQbank)

        !Calculate precipitation, atmospheric deposition and evaporation of river with area
        IF(a>0)THEN   !river has area
          riverarea(itype) = a * basin(i)%area        !default [m2]
          IF(conduct%floodplain)THEN
            IF(floodindex(i)>0)THEN
              IF(flooding(floodindex(i))%fpfmr>0.)THEN
                fpfrac = flooding(floodindex(i))%fpfmr !floodplain fraction of the main river area
                riverarea(itype) = a * basin(i)%area * (1.-fpfrac)   !Adjusted [m2]
                riverfparea = a * basin(i)%area * fpfrac             !Area of floodplain part [m2]
              ENDIF
            ENDIF
          ENDIF
          riverareakm2 = riverarea(itype)*1.E-6      ![km2}
          atmdepload1 = 0.
          atmdepload1_temp = 0.
          atmdepload2 = 0.

          !Forcing data and atmospheric deposition on river
          CALL calculate_class_atmospheric_forcing(i,j,radexti(i),cloudi(i),  &
                  temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
          IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution
          CALL set_class_precipitation_concentration_and_load(numsubstances, &
                   riverareakm2,precorg(i),temp,prec,cprec,cprecj,atmdepload1)
          CALL add_deposition_to_river_as_load(i,itype,classdata(j)%vegtype,month,riverareakm2,   &
              genpar(m_wetspload),landpar(m_drypp,classdata(j)%luse),   &
              deposition,atmdepload1_temp,atmdepload2,riverstate)
          atmdepload1 = atmdepload1 + atmdepload1_temp  !Sum wet dep with rain and as load
          IF(conductload)THEN
            Latmdep(j,1,:) = Latmdep(j,1,:) + atmdepload1  !flooded floodplain + river
            Latmdep(j,2,:) = Latmdep(j,2,:) + atmdepload2  !flooded floodplain + river
          ENDIF
          CALL calculate_rain_snow_from_precipitation(i,classdata(j)%luse,prec,temp,snowfall,rainfall)
          IF(simulate%substance(i_t2))THEN
            CALL get_rivertempvol(i,itype,riverstate,meanrivertemp,totrivervol)  !Get total river water volume and mean T2 temperature
            CALL add_T2_concentration_in_precipitation_on_water(prec,temp,  &  !Set correct T2 temperature concentration in precipitation
                  snowfall,rainfall,meanrivertemp,cprecj(i_t2),frozenstate%rivericecov(itype,i))
          ENDIF
          IF(prec>0) CALL add_precipitation_to_river(i,itype,riverarea(itype),prec,cprecj,riverstate)

          ! Calculate the fractional river area to be used for river evaporation
          CALL calculate_fractional_riverarea(i,itype,riverarea(itype),riverstate,riverfracarea(itype),rivereffectivedepth(itype))

          !Evaporation of river, taking partial ice-cover into account if icemodel is enabled
          CALL calculate_potential_evaporation(i,j,temp,epot,radexti(i),swrad,netrad,actvap,satvap,wind,epotsnow)
          epot = epot * basincevpcorr(i)
          IF(conduct%lakeriverice) epot = epot * (1. - frozenstate%rivericecov(itype,i))
          IF(epot>0.)THEN
            CALL calculate_river_evaporation(i,j,itype,numsubstances,riverarea(itype)*riverfracarea(itype),temp,epot,evapr,cevaprT1,riverstate)
          ELSE
            evapr = 0.
          ENDIF

          !Accumulate output variables for mean over subbasin (accumulation for cevapi)
          CALL calculate_class_outvar_add(o_ctmp,i,j,a,temp)
          CALL calculate_class_outvar_add(o_cprc,i,j,a,prec)
          IF(outvarstatus(o_psim)) CALL calculate_class_outvar_add(o_psim,i,j,a,prec+pcorricep(i)+icpevap)
          CALL calculate_class_outvar_add(o_rainfall,i,j,a,rainfall)
          CALL calculate_class_outvar_add(o_snowfall,i,j,a,snowfall)
          IF(outvarstatus(o_crgl)) CALL calculate_class_outvar_add(o_crgl,i,j,a,swrad)
          IF(outvarstatus(o_crnt)) CALL calculate_class_outvar_add(o_crnt,i,j,a,netrad)
          IF(outvarstatus(o_crpt)) CALL calculate_class_outvar_add(o_crpt,i,j,a,swpot)
          IF(outvarstatus(o_epot)) CALL calculate_class_outvar_add(o_epot,i,j,a*(1.-fpfrac),epot*riverfracarea(itype))
          IF(outvarstatus(o_evap)) CALL calculate_class_outvar_add(o_evap,i,j,a*(1.-fpfrac),evapr*riverfracarea(itype))
          IF(outvarstatus(o_evpt)) CALL calculate_class_outvar_add(o_evpt,i,j,a*(1.-fpfrac),evapr*riverfracarea(itype)+pcorricep(i)+icpevap)
          IF(outvarstatus(o_icloss)) CALL calculate_class_outvar_add(o_icloss,i,j,a*(1.-fpfrac),pcorricep(i)+icpevap)
          eacti = eacti + (evapr * (1.-fpfrac)) * riverfracarea(itype) * a
          IF(simulate%substance(i_t1).AND.outvarstatus(o_cevapT1)) CALL calculate_class_outvar_add_amount(o_cevapT1,i,j,a*(1.-fpfrac),cevaprT1*evapr*riverfracarea(itype),cevaprT1)

          !Accumulate outvar for precipitation and evaporation on water (excluding floodplain water)
          area_water = area_water + a*(1.-fpfrac)
          prec_water = prec_water + (rainfall+snowfall)*a*(1.-fpfrac)
          evap_water = evap_water + evapr * a*(1.-fpfrac)

          !Set water balance output
          IF(conductwb)THEN
            wbflows(w_pmriver,i) = prec * riverarea(itype)*1.E-3
            wbflows(w_emriver,i) = evapr * riverfracarea(itype) * riverarea(itype)*1.E-3
          ENDIF
        ENDIF !(a>0)

        !Calculate river ice and T2 processes of main river
        IF(modeloption(p_lakeriverice)>0)THEN

          !If no river class: Assume same precipitation and temperature conditions as for lake class
          IF(a==0)THEN
            IF(j==0) j=slc_olake
            IF(j==0) j=slc_lriver
            IF(j==0) j=slc_ilake
            IF(j==0) j=1
            CALL calculate_class_atmospheric_forcing(i,j,radexti(i),cloudi(i),  &
                  temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
            IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution
            CALL calculate_rain_snow_from_precipitation(i,classdata(j)%luse,prec,temp,snowfall,rainfall)
          ENDIF

          ! Calculate the fractional river area to be used for river T2 processes
          CALL calculate_fractional_riverarea(i,itype,riverarea(itype),riverstate,riverfracarea(itype),rivereffectivedepth(itype))

          !Calculate main river ice and T2 processes
          CALL T2_processes_in_river(i,itype,temp,swrad,riversurftemp,riverarea(itype) * riverfracarea(itype),frozenstate,riverstate,riverfreezeupday,freezeuparea)
          CALL ice_processes_in_river(i,itype,classdata(j)%luse,snowfall,temp, &
                                        riversurftemp,riverarea(itype) * riverfracarea(itype),swrad,         &
                                        frozenstate,riverstate,riverfreezeupday,riverbreakupday,freezeuparea) ! I am not 100% sure about how the riverfracarea(itype) affects the freezeuparea
          IF(modeloption(p_swtemperature)==1) CALL set_water_temperature(itype*2-1,i,riverstate,lakestate)

          !Reset j to value before lakeriverice calculation
          IF(a==0) j = slc_mriver    !default is 0
        ENDIF

        !Regional groundwater flow from main river
        IF(modeloption(p_deepgroundwater)==2)THEN
          CALL calculate_river_groundwaterflow_removal(i,j,basin(i)%subid,numsubstances,riverstate,grwout2)
          IF(conductwb) wbflows(w_rgrwofmr,i) = grwout2
          IF(fpfrac>0 .AND. a>0)THEN
            IF(genpar(m_optonoff).LE.0)THEN
              interflowpar(3) = flooding(floodindex(i))%fymmr
            ELSE
              interflowpar(3) = genpar(m_opt7)
            ENDIF
            CALL calculate_floodplain_waterlevel(miscstate%floodwater(1,i),riverfparea,interflowpar(3),ffpwl,ffparea)
            CALL calculate_river_floodplain_groundwaterflow_removal(i,j,basin(i)%subid,numsubstances,miscstate,grwout2)
            IF(conductwb) wbfpflows(w_rgrwofmrfp,i) = grwout2
          ENDIF
        ENDIF

        !Alternative location of abstraction of water from main river
        CALL point_abstraction_from_main_river(i,itype,riverstate,nutrientloadflows(3))

        !Calculate substances processes in river
        CALL np_processes_in_river(i,itype,riverarea(itype),riverdepth,transq,riverQbank,       &
                (2.-incorr)*genpar(m_denitwr),   &
                (2.-incorr)*genpar(m_denitwrl),genpar(m_hsatINwater), &
                lakedatapar(lakedataparindex(i,itype),m_ldwprodn),    &
                lakedatapar(lakedataparindex(i,itype),m_ldwprodp),    &
                genpar(m_hsatTP),genpar(m_sedexp),genpar(m_limsedpp), &
                genpar(m_muptnriver),genpar(m_muptpriver),genpar(m_muptdepriver),riverstate)
        CALL oc_processes_in_river(i,itype,riverarea(itype),riverdepth,   &
                lakedatapar(lakedataparindex(i,itype),m_ldwprodc),  &
                genpar(m_hsatTP),genpar(m_limsedpp),riverstate)
        CALL tracer_processes_in_river(i,itype,riverarea(itype),riverdepth,transq,riverQbank,riverstate)

        !Calculate interflow between main river and floodplain
        IF(fpfrac>0 .AND. a>0)THEN
          IF(genpar(m_optonoff).LE.0)THEN   !Get current interflow parameter values
            interflowpar(1) = flooding(floodindex(i))%flmrr    !flmr
            interflowpar(2) = flooding(floodindex(i))%flmrp    !flfp
            interflowpar(3) = flooding(floodindex(i))%fymmr
            interflowpar(4) = flooding(floodindex(i))%rcr2fp
            interflowpar(5) = flooding(floodindex(i))%rcfp2r
          ELSE
            interflowpar(1) = genpar(m_opt3)   !flmr
            interflowpar(2) = genpar(m_opt4)   !flfp
            interflowpar(3) = genpar(m_opt7)
            interflowpar(4) = genpar(m_opt5)
            interflowpar(5) = genpar(m_opt8)
          ENDIF
          CALL calculate_waterbody_floodplain_interflow(i,riverfparea,riverarea(itype),interflowpar,  &
                  miscstate%floodwater(1,i),miscstate%cfloodwater(:,1,i),riverstate%water(itype,i), &
                  riverstate%conc(:,itype,i),flooddepth,flooddegree,qmrflood)
          IF(outvarindex(323)>0) outvar(i,outvarindex(323)) = flooddepth
          IF(outvarindex(325)>0) outvar(i,outvarindex(325)) = flooddegree
          IF(outvarindex(o_rpwl)>0) outvar(i,outvarindex(o_rpwl)) = flooddepth + flooding(floodindex(i))%hrefr
          IF(conductwb)THEN
            IF(qmrflood>0.) wbfpflows(w_mrtofp,i) = qmrflood
            IF(qmrflood<0.) wbfpflows(w_fptomr,i) = -qmrflood
          ENDIF
        ELSE  !no floodplain
          IF(outvarindex(323)>0) outvar(i,outvarindex(323)) = missing_value
          IF(outvarindex(325)>0) outvar(i,outvarindex(325)) = 0.
          IF(outvarindex(o_rpwl)>0) outvar(i,outvarindex(o_rpwl)) = missing_value
        ENDIF

        !Calculate and remove outflow from river water volume
        dampq = (riverrc(itype,i)*(riverstate%water(itype,i) - deadriver(itype,i)))
        IF(dampq<0.) dampq=0.     !safe for irrigation etc
        IF(conduct%floodplain.AND.modeloption(p_floodplain)==3)THEN
          IF(floodplaindammedflowtoolake(i)<0.)THEN
            dampq = dampq + floodplaindammedflowtoolake(i)
          ENDIF
          IF(downstreamdammedflowonmainriver(i)<0.)THEN
            dampq = dampq + downstreamdammedflowonmainriver(i)
          ENDIF
          IF(dampq<0.)THEN
            dampq=0.     !safe for dammed water
          ENDIF
        ENDIF
        dampc = 0.
        IF(numsubstances>0) dampc = riverstate%conc(:,itype,i)
        CALL remove_water(riverstate%water(itype,i),numsubstances,riverstate%conc(:,itype,i),dampq,dampc,status)
        IF(status.NE.0) CALL error_remove_water(errstring(2),basin(i)%subid,i,itype)
        IF(conductload) Lpathway(:,16) = dampc * dampq * 1.E-3  !Load at point O (downstream of main river)
        dampq = dampq / seconds_per_timestep
        IF(outvarindex(o_cmrr)>0) outvar(i,outvarindex(o_cmrr)) = swrad
        IF(outvarindex(o_hged)>0) outvar(i,outvarindex(o_hged)) = genpar(m_hygeomc)*dampq**genpar(m_hygeomf)
        IF(outvarindex(408)>0) outvar(i,outvarindex(408)) = river_water_level(itype,i,dampq,ice_on_river(itype,i,frozenstate),frozenstate)
        IF(outvarindex(409)>0) outvar(i,outvarindex(409)) = genpar(m_hygeomk)*dampq**genpar(m_hygeomm)

      ELSE
        !No river, transport inflow to next calculation step (outlet lake)
        dampq = qin   ![m3/s]
        dampc = 0.
        IF(numsubstances>0) dampc = cin

        !Set output variables
        IF(conductload)THEN
          Lpathway(:,15) = Lpathway(:,14) !Total load  (kg/timestep), point N
          Lpathway(:,16) = Lpathway(:,14)  !Load at point O
        ENDIF
        IF(outvarindex(323)>0) outvar(i,outvarindex(323)) = missing_value !Floodplain output variables
        IF(outvarindex(325)>0) outvar(i,outvarindex(325)) = 0.
        IF(outvarindex(o_rpwl)>0) outvar(i,outvarindex(o_rpwl)) = missing_value
        IF(outvarindex(o_hged)>0) outvar(i,outvarindex(o_hged)) = missing_value
        IF(outvarindex(408)>0) outvar(i,outvarindex(408)) = missing_value   !Unnecessary?
        IF(outvarindex(409)>0) outvar(i,outvarindex(409)) = missing_value   !Unnecessary?

      ENDIF !river present
      IF(conductwb)THEN
        wbflows(w_pstomr,i) = nutrientloadflows(2)
        wbflows(w_abstmr,i) = nutrientloadflows(3)
        wbflows(w_wtrtomr,i) = nutrientloadflows(5)
      ENDIF


      !>Outlet lake (itype=2) or outlet wetland (owet)
      !-----------------------------------------------
      !Initiation of outlet lake/wetland calculations
      lakearea(itype) = 0.
      lakeareai2(i)=0.
      lakeoutflow(itype) = 0.
      concout = 0.
      statuslb = .FALSE.
      statuslastlb = .FALSE.
      a = 0.  !Outlet lake?
      aolake = 0.
      IF(slc_olake>0)THEN
        j = slc_olake
        a = classbasin(i,j)%part
        aolake = classbasin(i,j)%part
      ENDIF
      aowet = 0.  !Outlet wetland?
      IF(slc_owet>0.) aowet = classbasin(i,slc_owet)%part
      IF(conductwb)THEN
        IF(aowet==0.)THEN
          wbflows(w_mrtool,i) = dampq * seconds_per_timestep    !mrtool is used weather olake or not, but not when owet
        ENDIF
      ENDIF

      !Lake exist:
      IF(aolake>0)THEN
        IF(ALLOCATED(lakebasinindex))THEN   !default is no lakebasins
          IF(lakebasinindex(i)>0) statuslb = .TRUE.
        ENDIF
        fpfrac = 0.   !default is no floodplain
        lakearea(itype) = a * basin(i)%area     ![m2]
        lakeareai2(i) = lakearea(itype)
        IF(conductload) Lpathway(:,17) = Lpathway(:,16)  !Load at point P (no owet)
        IF(conduct%floodplain)THEN
          IF(floodindex(i)>0)THEN
            IF(flooding(floodindex(i))%fpfol>0.)THEN
              fpfrac = flooding(floodindex(i))%fpfol          !floodplain fraction of outlet lake area
              lakearea(itype) = a * basin(i)%area * (1.-fpfrac)    !m2
              lakeareai2(i) = lakearea(itype)
              lakefparea = a * basin(i)%area * fpfrac       !Area of floodplain part m2
            ENDIF
          ENDIF
        ENDIF
        lakeareakm2 = lakearea(itype)*1.E-6       ![km2}
        qunitfactor = seconds_per_timestep * 1000. / lakearea(itype)     !m3/s->mm/timestep
        atmdepload1 = 0.
        atmdepload1_temp = 0.
        atmdepload2 = 0.

        !Forcing data set precipitation concentration on lake
        CALL calculate_class_atmospheric_forcing(i,j,radexti(i),cloudi(i),  &
                temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
        IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution
        CALL set_class_precipitation_concentration_and_load(numsubstances, &
                 lakeareakm2,precorg(i),temp,prec,cprec,cprecj,atmdepload1)
        CALL calculate_rain_snow_from_precipitation(i,classdata(j)%luse,prec,temp,snowfall,rainfall)
        IF(simulate%substance(i_t2)) CALL add_T2_concentration_in_precipitation_on_water(prec,temp, &
                          snowfall,rainfall,lakestate%uppertemp(itype,i),cprecj(i_t2), &
                          frozenstate%lakeicecov(itype,i))

        !Add precipitation and atmospheric deposition to lake water
        IF(prec>0) CALL add_water(numsubstances,lakestate%water(itype,i),lakestate%conc(:,itype,i),prec,cprecj)
        CALL add_deposition_to_lake_as_load(i,itype,classdata(j)%vegtype,month,lakeareakm2,   &
              genpar(m_wetspload),landpar(m_drypp,classdata(j)%luse),   &
              deposition,atmdepload1_temp,atmdepload2,lakestate)
        atmdepload1 = atmdepload1 + atmdepload1_temp  !Sum wet dep with rain and as load

        !Evaporation of lake, taking partial ice cover into account if lakeriverice model is enabled
        CALL calculate_potential_evaporation(i,j,temp,epot,radexti(i),swrad,netrad,actvap,satvap,wind,epotsnow)
        epot = epot * basincevpcorr(i)
        IF(conduct%lakeriverice) epot = epot * (1. - frozenstate%lakeicecov(itype,i))
        IF(epot>0.)THEN
          CALL calculate_actual_lake_evaporation(i,j,itype,numsubstances,temp,epot,evapl,cevapl,lakestate)
        ELSE
          evapl = 0.
        ENDIF

        !For output; accumulate outvar and set other output
        CALL calculate_class_outvar_add(o_ctmp,i,j,a,temp)
        CALL calculate_class_outvar_add(o_cprc,i,j,a,prec)
        IF(outvarstatus(o_psim)) CALL calculate_class_outvar_add(o_psim,i,j,a,prec+pcorricep(i)+icpevap)
        CALL calculate_class_outvar_add(o_rainfall,i,j,a,rainfall)
        CALL calculate_class_outvar_add(o_snowfall,i,j,a,snowfall)
        IF(outvarstatus(o_crgl)) CALL calculate_class_outvar_add(o_crgl,i,j,a,swrad)
        IF(outvarstatus(o_crnt)) CALL calculate_class_outvar_add(o_crnt,i,j,a,netrad)
        IF(outvarstatus(o_crpt)) CALL calculate_class_outvar_add(o_crpt,i,j,a,swpot)
        IF(outvarstatus(o_epot)) CALL calculate_class_outvar_add(o_epot,i,j,a*(1.-fpfrac),epot)
        IF(outvarstatus(o_evap)) CALL calculate_class_outvar_add(o_evap,i,j,a*(1.-fpfrac),evapl)
        IF(outvarstatus(o_evpt)) CALL calculate_class_outvar_add(o_evpt,i,j,a*(1.-fpfrac),evapl+pcorricep(i)+icpevap)
        IF(outvarstatus(o_icloss)) CALL calculate_class_outvar_add(o_icloss,i,j,a*(1.-fpfrac),pcorricep(i)+icpevap)
        eacti = eacti + (evapl * (1.-fpfrac))*a
        IF(simulate%substance(i_t1).AND.outvarstatus(o_cevapT1)) CALL calculate_class_outvar_add_amount(o_cevapT1,i,j,a*(1.-fpfrac),cevapl(i_t1)*evapl,cevapl(i_t1))
        qcinfli = qcinfli + (prec-evapl)*lakearea(itype)/seconds_per_timestep/1000.    !m3/s
        IF(conductload)THEN
          Latmdep(j,1,:) = Latmdep(j,1,:) + atmdepload1  !flooded floodplain + lake
          Latmdep(j,2,:) = Latmdep(j,2,:) + atmdepload2  !flooded floodplain + lake
        ENDIF

        !Accumulate outvar for precipitation and evaporation on water (excluding floodplain water)
        area_water = area_water + a*(1.-fpfrac)
        prec_water = prec_water + (rainfall+snowfall)*a*(1.-fpfrac)
        evap_water = evap_water + evapl * a*(1.-fpfrac)

        IF(conductwb)THEN
          wbflows(w_polake,i) = prec * lakearea(itype)*1.E-3
          wbflows(w_eolake,i) = evapl * lakearea(itype)*1.E-3
        ENDIF

        !Calculate inflow to lake (from main river (and lakebasins))
        qin = dampq                     ![m3/s]
        cin = dampc
        IF(conduct%simT1.OR.conduct%simT2) CALL add_tracer_point_source_to_lake(i,itype,qin,cin)
        qinmm = qin * qunitfactor         ![mm/ts]
        IF(conductload) Lpathway(:,18) = cin * qin * seconds_per_timestep * 1.E-3     !Load at point Q inflow to olake (kg/timestep)
        IF(conductload) Lpathwayi2(:,18,i) = Lpathway(:,18)     !Save for lakebasins
        CALL add_water(numsubstances,lakestate%water(itype,i),lakestate%conc(:,itype,i),qinmm,cin)
        qcinfli = qcinfli + qin

        !Add regional groundwater inflow to olake
        IF(modeloption(p_deepgroundwater)==1)THEN
          CALL add_regional_groundwater_flow_to_olake(i,itype,numsubstances,  &
                qunitfactor,lakestate%water(itype,i),lakestate%conc(:,itype,i),  &
                qcinfli,grwloadlake,grwtool)
          IF(conductload)THEN
            Lgrwol(:) = grwloadlake
            Lpathway(:,18) = Lpathway(:,18) + grwloadlake   !Point Q
            Lpathwayi2(:,18,i) = Lpathway(:,18)     !Save for lakebasins
          ENDIF
          IF(conductwb) wbflows(w_rgrwtool,i) = grwtool
        ENDIF

        !Calculate lake tracer (T1), temperature (T2) and ice processes
        IF(simulate%substance(i_t2)) CALL T2_processes_in_lake(i,itype,temp,swrad,lakesurftemp, &
                              lakearea(itype),hypodepth(itype),frozenstate,lakestate, &
                              lakefreezeupday,freezeuparea)
        CALL tracer_processes_in_lake(i,itype,lakestate)
        IF(conduct%lakeriverice) CALL ice_processes_in_lake(i,itype,classdata(j)%luse, &
                              snowfall,temp,lakesurftemp,swrad,frozenstate,lakestate, &
                              lakefreezeupday,lakebreakupday,hypodepth(itype),freezeuparea)
        hypodepthi2(i) = hypodepth(itype)
        IF(modeloption(p_swtemperature)==1) CALL set_water_temperature(itype*2,i,riverstate,lakestate)

        !Abstraction of water from outlet lake
        CALL point_abstraction_from_outlet_lake(i,itype,qunitfactor,lakestate,nutrientloadflows(4))
        IF(conduct%watertransfer) CALL water_transfer_from_outlet_lake(i,itype,qunitfactor,miscstate,lakestate,nutrientloadflows(6))
        qcinfli = qcinfli - (nutrientloadflows(4)+nutrientloadflows(6))/seconds_per_timestep

        !Calculate substances processes in lakes (in flooded floodplain water, to be added here or in soilmodel4)
        CALL np_processes_in_lake(i,itype,lakearea(itype),    &
              (2.-incorr)*lakedatapar(lakedataparindex(i,itype),m_lddenitwl),genpar(m_hsatINwater),  &
              lakedatapar(lakedataparindex(i,itype),m_ldwprodn),   &
              lakedatapar(lakedataparindex(i,itype),m_ldwprodp),genpar(m_hsatTP),   &
              (2.-oncorr)*lakedatapar(lakedataparindex(i,itype),m_ldsedon),    &
              lakedatapar(lakedataparindex(i,itype),m_ldsedpp),genpar(m_sedss), &
              genpar(m_sedae),genpar(m_limsedon),genpar(m_limsedpp),genpar(m_limsedss), &
              lakedatapar(lakedataparindex(i,itype),m_ldmuptn), &
              lakedatapar(lakedataparindex(i,itype),m_ldmuptp),genpar(m_muptdep),lakestate)
        CALL oc_processes_in_lake(i,itype,lakearea(itype),lakedatapar(lakedataparindex(i,itype),m_ldwprodc),    &
              genpar(m_hsatTP),genpar(m_limsedpp),lakedatapar(lakedataparindex(i,itype),m_ldsedoc),lakestate)

        !Calculate interflow between olake and floodplain
        IF(fpfrac>0.)THEN
          wlm3ol = lakestate%water(itype,i)*0.001 * lakearea(itype)   !Calculate water volume and average concentration in lake [m3]
          IF(simulatesubstances)THEN
            cwlm3ol = lakestate%conc(:,itype,i)
          ENDIF
          IF(genpar(m_optonoff).LE.0)THEN   !Get current interflow parameter values
            interflowpar(1) = flooding(floodindex(i))%floll    !flol
            interflowpar(2) = flooding(floodindex(i))%flolp    !flfp
            interflowpar(3) = flooding(floodindex(i))%fymol
            interflowpar(4) = flooding(floodindex(i))%rcl2fp
            interflowpar(5) = flooding(floodindex(i))%rcfp2l
          ELSE
            interflowpar(1) = genpar(m_opt1)
            interflowpar(2) = genpar(m_opt2)
            interflowpar(3) = genpar(m_opt6)
            interflowpar(4) = genpar(m_opt5)
            interflowpar(5) = genpar(m_opt8)
          ENDIF
          CALL calculate_waterbody_floodplain_interflow(i,lakefparea,lakearea(itype),interflowpar,  &
                  miscstate%floodwater(2,i),miscstate%cfloodwater(:,2,i),wlm3ol, &
                  lakestate%conc(:,itype,i),flooddepth,flooddegree,qolflood)
          IF(outvarindex(324)>0) outvar(i,outvarindex(324)) = flooddepth
          IF(outvarindex(326)>0) outvar(i,outvarindex(326)) = flooddegree
          IF(outvarindex(o_lpwl)>0) outvar(i,outvarindex(o_lpwl)) = flooddepth + flooding(floodindex(i))%hrefl
          IF(conductwb)THEN
            IF(qolflood>0.) wbfpflows(w_oltofp,i) = qolflood
            IF(qolflood<0.) wbfpflows(w_fptool,i) = -qolflood
          ENDIF
          lakestate%water(itype,i) = wlm3ol/lakearea(itype)*1000.
          IF(simulatesubstances) lakestate%conc(:,itype,i) = cwlm3ol   !not necessary
          !Update qin for calculation of outflow from average rating curve
          qcinfli = qcinfli - qolflood/seconds_per_timestep
        ELSE  !no floodplain
          IF(outvarindex(324)>0) outvar(i,outvarindex(324)) = missing_value
          IF(outvarindex(326)>0) outvar(i,outvarindex(326)) = 0.
          IF(outvarindex(o_lpwl)>0) outvar(i,outvarindex(o_lpwl)) = missing_value
        ENDIF
        qcinflii2(i) = qcinfli
        qini2(i) = qcinfli

        !Calculate and remove outflows from outlet lake (not lakebasin)
        IF(.NOT.statuslb)THEN

          CALL calculate_flow_from_undivided_lake(i,itype,qcinfli,lakearea(itype),& !IN
                                        downstreamdammedflowonolake(i),hypodepth(itype),olakewstold,&  !IN
                                        outflowsim,lakeoutflow(itype),mainflow,branchflow,& !OUT
                                        concout,& !OUT
                                        wstlakesim,lakewst(itype),wcomaver,&  !OUT
                                        Lpathway,lakestate,miscstate)  !INOUT
          basinoutflow(itype) = lakeoutflow(itype)
          bconcout = concout

        ELSEIF(statuslb)THEN

          !Lakebasin, calculate outflows from all lakebasins, when all lakebasins is passed
          IF(lakebasin(lakebasinindex(i))%last)THEN
            !We have found the designated last lakebasin of this multi-basin lake!
            !Set flag to remember to calculate some things after the olake and (re-)set output
            statuslastlb = .TRUE.

            !First find all lakebasins (their isub) for this lakebasinlake:
            CALL find_lakebasins(i,looplakes)

            CALL calculate_flow_for_lakebasin_lake(i,itype,looplakes,& !IN
                                                qini2,lakeareai2,hypodepthi2,olakewstoldi2,&  !IN
                                                outflowsimi2,lakeoutflowi2,mainflowi2,branchflowi2,& !INOUT
                                                accinflow,acccinflow,clakeoutflowi2,cmainflowi2,cbranchflowi2,qcinflii2,netroutload,& !INOUT
                                                wstlakesimi2,lakewsti2,wcomaveri2,Lpathwayi2,&  !INOUT
                                                Lbranchi2,totaloutflow,ctotaloutflow,&  !OUT
                                                lakestate,miscstate)  !INOUT

          ENDIF
        ENDIF

      ENDIF !olake

      !Calculate flow and processes of outlet wetland if present
      CALL calculate_outlet_wetland(i,radexti(i),cloudi(i),added_water_owet,dampq,dampc,frozenstate,soilstate, &
                                    miscstate,outflowsim,basinoutflow(itype),bconcout, &
                                    mainflow,branchflow,outvar,wbflows,Lpathway)

      !No outlet lake or wetland, outflow = inflow
      IF(aolake==0 .AND. aowet==0)THEN
        basinoutflow(itype) = dampq       !m3/s
        outflowsim = basinoutflow(itype)      !saved for qAR and output
        qcinfli = missing_value   !not needed, qcinfli used if aolake>0
        bconcout = dampc
        IF(conductload)THEN
          Lpathway(:,17) = Lpathway(:,16)  !Load at point P, Q and R is equal to flow in main river (kg/timestep)
          Lpathway(:,18) = Lpathway(:,17)
          Lpathway(:,19) = Lpathway(:,17)
        ENDIF
        lakewst(itype) = missing_value   !not needed, lakewst used if lakearea(2)>0
        wstlakesim     = missing_value   !not needed, wstlakesim used if lakearea(2)>0
        lakesurftemp(itype) = missing_value   !not needed, lakesurftemp used if aolake>0

        !>Update modelled subbasin outflow with observations
        IF(doupdate(i_quseobs)) CALL apply_quseobs(i,basinoutflow(itype))
        IF(doupdate(i_qar))     CALL apply_qarupd(i,outflowsim,basinoutflow(itype),miscstate%updatestationsarcorr(i))
        CALL calculate_branched_flow(i,basinoutflow(2),mainflow,branchflow)
        IF(outvarindex(408)>0) outvar(i,outvarindex(408)) = river_water_level(itype,i,basinoutflow(itype),ice_on_river(itype,i,frozenstate),frozenstate)  !recalculate if updated Q
        IF(outvarindex(o_hged)>0) outvar(i,outvarindex(o_hged)) = genpar(m_hygeomc)*basinoutflow(itype)**genpar(m_hygeomf)
        IF(outvarindex(409)>0) outvar(i,outvarindex(409)) = genpar(m_hygeomk)*basinoutflow(itype)**genpar(m_hygeomm)
      ENDIF   !a==0

      !Output
      IF(conductwb)THEN
        wbflows(w_abstol,i) = nutrientloadflows(4)  !these are ok also for lb
        wbflows(w_wtrofol,i) = nutrientloadflows(6)
      ENDIF

      !Calculate theoretical precipitation and evaporation on water in case area_water = 0 (no ilake, olake, local river, or main river area in this subbasin)
      IF(area_water.LE.0.)THEN
        j=0
        IF(slc_lriver>0) j = slc_lriver
        IF(slc_ilake>0)  j = slc_ilake
        IF(slc_mriver>0) j = slc_mriver
        IF(slc_olake>0)  j = slc_olake
        IF(j>0)THEN
          !Forcing data for hypothetical water surface
          CALL calculate_class_atmospheric_forcing(i,j,radexti(i),cloudi(i),  &
                temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
          IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution
          CALL calculate_rain_snow_from_precipitation(i,classdata(j)%luse,prec,temp,snowfall,rainfall)
          !Evaporation for hypothetical water surface
          CALL calculate_potential_evaporation(i,j,temp,epot,radexti(i),swrad,netrad,actvap,satvap,wind,epotsnow)
          epot = epot * basincevpcorr(i)
          IF(temp>landpar(m_ttmp,classdata(j)%luse) .AND. epot>0) THEN
            evapl = epot
          ELSE
            evapl = 0.
          ENDIF
          !Accumulate outvar for precipitation and evaporation on water (excluding floodplain water)
          area_water = area_water + 1.
          prec_water = rainfall+snowfall
          evap_water = evapl
        ELSE
          prec_water = 0.
          evap_water = 0.
          area_water = area_water + 1.
        ENDIF
      ENDIF

      IF(.NOT.statuslb)THEN
        !Finalize subbasin contribution to down stream subbasin from olake, owet or no lake (but not lakebasin subbasins)
        IF(conductload) Lbranch = branchflow * bconcout * seconds_per_timestep * 1.E-3  !Load in branch (kg/timestep), point S

        !>Update modelled subbasin outflow with nutrient concentrations
        IF(doupdate(i_tpcorr))  CALL apply_nutrientcorr(i_tpcorr,i,bconcout(i_sp),bconcout(i_pp))
        IF(doupdate(i_tncorr))  CALL apply_nutrientcorr(i_tncorr,i,bconcout(i_in),bconcout(i_on))
        IF(doupdate(i_cuseobs)) CALL apply_cuseobs(i,bconcout)

        !>Accumulate flow to downstream subbasins, and calculate volume flow for water balance
        CALL accumulate_flow_to_downstream_subbasin(i,mainflow,branchflow,bconcout,accinflow,acccinflow,wbflows)

        IF(simulatesubstances) cbasinoutflow(:,itype) = bconcout
      ENDIF

      !Subbasin output
      !----------------

      !>Set observed subbasin output variables (outvar) for print out
      IF(simulate%substance(i_t1)) CALL set_outvar_xobs(o_cprecT1,i)
      CALL set_outvar_xobs(410,i)  !rec water level of main river
      CALL set_outvar_xobs(o_cprecIN,i)  !rec concentration of IN in precipitation
      CALL set_outvar_xobs(o_cprecSP,i)  !rec concentration of SP in precipitation
      CALL set_outvar_xobs(o_reswe,i)    !rec snow water equivalent
      CALL set_outvar_xobs(o_reepot,i)  !Observed potential evaporation
      CALL set_outvar_xobs(42,i)  !Observed evaporation (mm)
      CALL set_outvar_xobs(59,i)  !rec snow depth
      CALL set_outvar_xobs(60,i)  !rec soil frost
      CALL set_outvar_xobs(61,i)  !rec grw level
      CALL set_outvar_xobs(190,i)  !Recorded Fractional snow cover area(-)
      CALL set_outvar_xobs(192,i)  !Recorded Fractional snow cover area error (-)
      CALL set_outvar_xobs(193,i)  !Recorded Fractional snow cover multi (-)
      CALL set_outvar_xobs(194,i)  !Recorded Fractional snow cover multi error (-)
      CALL set_outvar_xobs(257,i) !'S105','fsusnow fsc surr. open'
      CALL set_outvar_xobs(258,i) !'S106','fsusnow fsc course open'
      CALL set_outvar_xobs(259,i) !'S108','fsusnow mean depth open'
      CALL set_outvar_xobs(260,i) !'S111','fsusnow mean density open'
      CALL set_outvar_xobs(261,i) !'S114','fsusnow snow water eq. open'
      CALL set_outvar_xobs(262,i) !'S205','fsusnow fsc surr. forest'
      CALL set_outvar_xobs(263,i) !'S206','fsusnow fsc course forest'
      CALL set_outvar_xobs(264,i) !'S208','fsusnow mean depth forest'
      CALL set_outvar_xobs(265,i) !'S211','fsusnow mean density forest'
      CALL set_outvar_xobs(266,i) !'S214','fsusnow snow water eq. forest'
      CALL set_outvar_xobs(o_rrun,i)   !local recorded runoff, mm/ts
      CALL set_outvar_xobs(o_reT1,i)   !T1 in outflow
      CALL set_outvar_xobs(o_reT2,i)   !T2 in outflow
      CALL set_outvar_xobs(o_reIN,i)   !rec IN in outflow, ug/L
      CALL set_outvar_xobs(o_reON,i)   !rec ON in outflow, ug/L
      CALL set_outvar_xobs(o_reSP,i)   !rec SP in outflow, ug/L
      CALL set_outvar_xobs(o_rePP,i)   !rec PP in outflow, ug/L
      CALL set_outvar_xobs(o_reTN,i)   !rec TN in outflow, ug/L
      CALL set_outvar_xobs(o_reTP,i)   !rec TP in outflow, ug/L
      CALL set_outvar_xobs(o_reOC,i)  !DOC in outflow, mg/L
      CALL set_outvar_xobs(o_rewstr,i)  !recorded olake waterstage
      CALL set_outvar_xobs(113,i)    !rec inflow (-"-) (m3/s)
      CALL set_outvar_xobs(o_roum,i)    !Observed flow main channel
      CALL set_outvar_xobs(o_roub,i)
      CALL set_outvar_xobs(o_reSS,i)   !recorded SS in outflow, mg/L
      CALL set_outvar_xobs(o_reAE,i)   !recorded AE in outflow, mg/L
      CALL set_outvar_xobs(o_reTS,i)   !recorded TS in outflow, mg/L
      CALL set_outvar_xobs(62,i)  !rec T1 conc in outflow
      CALL set_outvar_xobs(o_dwtr,i)
      DO k=0,9
        CALL set_outvar_xobsmean(o_xobsm+k,i,k+1)    !recorded meantype variable
        CALL set_outvar_xobsstate(o_xobss+k,i,k+1)   !recorded sumtype variable
      ENDDO
      IF(outvarindex(o_rout)>0.AND.ALLOCATED(qobsi)) outvar(i,outvarindex(o_rout)) = qobsi(i)
      IF(ALLOCATED(qobsi))THEN
        IF(qobsi(i)/=missing_value)THEN
          CALL set_outvar_xobs_scaled(119,i,o_reTN,qobsi(i)*seconds_per_timestep*1.E-6)  !rec TN load (kg/timestep)
          CALL set_outvar_xobs_scaled(120,i,o_reTP,qobsi(i)*seconds_per_timestep*1.E-6)  !rec TP load (kg/timestep)
        ENDIF
      ENDIF
      !Some recorded variables still left below

      !Evaporation output
      IF(eacti>0.0 .AND. simulate%substance(i_t1)) CALL calculate_class_outvar_finish(o_cevapT1,i,eacti)  !T1 conc in evaporation

      !Simulated FSUS snow outputs, forest and open land
      IF(area_open>0)THEN
        IF(outvarindex(267)>0) outvar(i,outvarindex(267)) = snowvari(2,1)     !'C106','comp. fsc course open'
        IF(outvarindex(268)>0) outvar(i,outvarindex(268)) = snowvari(1,1)     !'C108','comp. mean depth open'
        IF(outvarindex(269)>0) outvar(i,outvarindex(269)) = snowvari(3,1)     !'C111','comp. mean density open'
        IF(outvarindex(270)>0) outvar(i,outvarindex(270)) = snowvari(4,1)     !'C114','comp. snow water eq. open'
      ENDIF
      IF(area_forest>0)THEN
        IF(outvarindex(271)>0) outvar(i,outvarindex(271)) = snowvari(2,2)   !'C206','comp. fsc course forest'
        IF(outvarindex(272)>0) outvar(i,outvarindex(272)) = snowvari(1,2)   !'C208','comp. mean depth forest'
        IF(outvarindex(273)>0) outvar(i,outvarindex(273)) = snowvari(3,2)   !'C211','comp. mean density forest'
        IF(outvarindex(274)>0) outvar(i,outvarindex(274)) = snowvari(4,2)   !'C214','comp. snow water eq. forest'
      ENDIF

      !Soil layer output
      IF(modeloption(p_soilleakage)==0)THEN
        IF(i_in>0.AND.i_on>0)THEN
          IF(outvarstatus(o_soilden3)) CALL calculate_class_outvar_finish_scale(o_soilden3,i,basin(i)%area*1.E-6)    !denitrification soil layer 3 (kg)
          IF(outvarstatus(o_soildenrz)) CALL calculate_class_outvar_finish_scale(o_soildenrz,i,basin(i)%area*1.E-6)   !denitrification soil layer 1 and 2 (kg)
        ENDIF
        DO k=285,320
          IF(outvarstatus(k)) CALL calculate_class_outvar_finish_scale(k,i,basin(i)%area*1.E-6)
        ENDDO
      ELSE
        DO k=285,320
          CALL set_class_outvar_missing(k,i)
        ENDDO
        CALL set_class_outvar_missing(o_soilden3,i)
        CALL set_class_outvar_missing(o_soildenrz,i)
      ENDIF

      !Output for Lake and River Ice, Snow, and T2 Water Temperature Model
      IF(conduct%lakeriverice)THEN
        !Lake ice variables
        IF(slc_olake>0)THEN
          IF(outvarindex(148)>0) outvar(i,outvarindex(148)) = frozenstate%lakeice(2,i)        !'coli' 'comp olake ice depth'
          IF(outvarindex(150)>0) outvar(i,outvarindex(150)) = frozenstate%lakebice(2,i)         !'colb','comp olake blackice depth'
          IF(outvarindex(152)>0) outvar(i,outvarindex(152)) = frozenstate%lakesnowdepth(2,i)    !'cols','comp olake snow depth'
          IF(outvarindex(248)>0) outvar(i,outvarindex(248)) = frozenstate%lakeicecov(2,i)       !'coic','comp olake ice cover'
          IF(outvarindex(o_colp)>0) outvar(i,outvarindex(o_colp)) = frozenstate%lakeicepor(2,i) !'colp','comp olake ice porosity'
          CALL set_outvar_xobs(154,i)  !'roli','rec. olake ice depth'
          CALL set_outvar_xobs(156,i)  !'rolb','rec. olake blackice depth'
          CALL set_outvar_xobs(158,i)  !'rols','rec. olake snow depth'
        ENDIF
        IF(slc_ilake>0)THEN
          IF(outvarindex(149)>0) outvar(i,outvarindex(149)) = frozenstate%lakeice(1,i)        !'cili' 'comp ilake ice depth'
          IF(outvarindex(151)>0) outvar(i,outvarindex(151)) = frozenstate%lakebice(1,i)       !'cilb','comp ilake blackice depth'
          IF(outvarindex(153)>0) outvar(i,outvarindex(153)) = frozenstate%lakesnowdepth(1,i)  !'cils','comp ilake snow depth'
          IF(outvarindex(249)>0) outvar(i,outvarindex(249)) = frozenstate%lakeicecov(1,i)     !'ciic','comp ilake ice cover'
          CALL set_outvar_xobs(155,i)  !'rili','rec. ilake ice depth'
          CALL set_outvar_xobs(157,i)  !'rilb','rec. ilake blackice depth'
          CALL set_outvar_xobs(159,i)  !'rils','rec. ilake snow depth'
        ENDIF
        !River ice and snow depth variables
        IF(outvarindex(160)>0) outvar(i,outvarindex(160)) = frozenstate%riverice(2,i)       !'cmri','comp main river ice depth'
!        IF(outvarindex(161)>0) outvar(i,outvarindex(161)) = frozenstate%riverice(1,i)       !'clri','comp local river ice depth'
        IF(outvarindex(162)>0) outvar(i,outvarindex(162)) = frozenstate%riverbice(2,i)      !'cmrb','comp main river blackice depth'
!        IF(outvarindex(163)>0) outvar(i,outvarindex(163)) = frozenstate%riverbice(1,i)      !'clrb','comp local river blackice depth'
        IF(outvarindex(164)>0) outvar(i,outvarindex(164)) = frozenstate%riversnowdepth(2,i) !'cmrs','comp main river snow depth'
!        IF(outvarindex(165)>0) outvar(i,outvarindex(165)) = frozenstate%riversnowdepth(1,i) !'clrs','comp local river snow depth'
        IF(outvarindex(250)>0) outvar(i,outvarindex(250)) = frozenstate%rivericecov(2,i)    !'cmic','comp main river ice cover'
!        IF(outvarindex(251)>0) outvar(i,outvarindex(251)) = frozenstate%rivericecov(1,i)    !'clic','comp local river ice cover'
        IF(outvarindex(o_cmrp)>0) outvar(i,outvarindex(o_cmrp)) = frozenstate%rivericepor(2,i) !'cmrp','comp main river ice porosity'
        CALL set_outvar_xobs(166,i)  !'rmri','rec. main river ice depth'
        CALL set_outvar_xobs(167,i)  !'rlri','rec. local river ice depth'
        CALL set_outvar_xobs(168,i)  !'rmrb','rec. main river blackice depth'
        CALL set_outvar_xobs(169,i)  !'rlrb','rec. local river blackice depth'
        CALL set_outvar_xobs(170,i)  !'rmrs','rec. main river snow depth'
        CALL set_outvar_xobs(171,i)  !'rlrs','rec. local river snow depth'
      ENDIF
      IF(simulate%substance(i_t2))THEN
        !Lake temperature variables (surface, upper layer, lower layer, mean water temp)
        IF(slc_olake>0.AND.aolake>0)THEN
          IF(outvarindex(172)>0) outvar(i,outvarindex(172)) = lakesurftemp(2)           !'olst','comp olake surface temp'
          IF(outvarindex(173)>0) outvar(i,outvarindex(173)) = lakestate%uppertemp(2,i)  !'olut','comp olake upper temp'
          IF(outvarindex(174)>0) outvar(i,outvarindex(174)) = lakestate%lowertemp(2,i)  !'ollt','comp olake lower temp'
          IF(outvarindex(175)>0) outvar(i,outvarindex(175)) = lakestate%conc(i_t2,2,i)  !'olwt','comp olake mean temp'
        ENDIF
        IF(slc_ilake>0)THEN
          IF(outvarindex(176)>0) outvar(i,outvarindex(176)) = lakesurftemp(1)           !'ilst','comp olake surface temp'
          IF(outvarindex(177)>0) outvar(i,outvarindex(177)) = lakestate%conc(i_t2,1,i)  !'ilwt','comp ilake mean temp'
        ENDIF
        !River temperature variables (surface, mean, local and main)
        !IF(outvarindex(178)>0) outvar(i,outvarindex(178)) = riversurftemp(1)      !lrst','comp local river surface temp'
        !CALL get_rivertempvol(i,1,riverstate,meanrivertemp,totrivervol)
        !IF(outvarindex(179)>0) outvar(i,outvarindex(179)) = meanrivertemp         !'lrwt','comp local river mean  temp'
        IF(outvarindex(180)>0) outvar(i,outvarindex(180)) = riversurftemp(2)       !'mrst','comp main  river surface temp'
        CALL get_rivertempvol(i,2,riverstate,meanrivertemp,totrivervol)
        IF(outvarindex(181)>0) outvar(i,outvarindex(181)) = meanrivertemp
      ENDIF
      !Additional variables from old water temp model
      IF(outvarindex(185)>0) outvar(i,outvarindex(185)) = riverstate%temp(2,i) !main river temperature
      !IF(outvarindex(186)>0) outvar(i,outvarindex(186)) = riverstate%temp(1,i) !local river temp
      IF(slc_ilake>0 .AND.outvarindex(187)>0) outvar(i,outvarindex(187)) = lakestate%temp(1,i) !ilake temp
      IF(slc_olake>0 .AND.outvarindex(188)>0) outvar(i,outvarindex(188)) = lakestate%temp(2,i) !olake temp
      IF(outvarstatus(32)) CALL calculate_outvar_watertemperature(32,i,2,-9999.,riverstate,lakestate) !wtmp
      IF(outvarstatus(283)) CALL calculate_outvar_watertemperature(283,i,2,0.,riverstate,lakestate)    !wtm0
      !Recorded Olake water surface temp
      CALL set_outvar_xobs(182,i)  !'rolt','rec. olake surface temp'
      CALL set_outvar_xobs(183,i)  !'rilt','rec. ilake surface temp'
      CALL set_outvar_xobs(184,i)  !'rmrt','rec. main river surface temp'

      !Output variables for ilake and iwet and owet water stage
      IF(outvarindex(o_wstilake)>0.AND.slc_ilake>0)THEN
        IF(classbasin(i,slc_ilake)%part>0)THEN
          outvar(i,outvarindex(o_wstilake)) = lakestate%water(1,i)*1.E-3 - basin(i)%lakedepth(1)
        ENDIF
      ENDIF
!      IF(outvarindex(o_wstiwet)>0.AND.slc_iwet>0)THEN
!        outvar(i,outvarindex(o_wstiwet)) = - get_wetland_threshold(slc_iwet)
!        IF((soilstate%water(1,slc_iwet,i) - pwmm(1,slc_iwet))>=0.) &
!        outvar(i,outvarindex(o_wstiwet)) = outvar(i,outvarindex(o_wstiwet)) + (soilstate%water(1,slc_iwet,i) - pwmm(1,slc_iwet))*1.E-3
!      ENDIF
      !IF(outvarindex(o_wstowet)>0.AND.slc_owet>0)THEN
      !  outvar(i,outvarindex(o_wstowet)) = - get_wetland_threshold(slc_owet)
      !  IF((soilstate%water(1,slc_owet,i) - pwmm(1,slc_owet))>=0.) &
      !  outvar(i,outvarindex(o_wstowet)) = outvar(i,outvarindex(o_wstowet)) + (soilstate%water(1,slc_owet,i) - pwmm(1,slc_owet))*1.E-3
      !ENDIF

      !Output regional groundwater
      IF(conductwb.OR.outvarindex(81)>0) CALL calculate_regional_groundwaterflow_to_outside_system(i,grwout(1,i),outofsystem)
      IF(outvarindex(81)>0) outvar(i,outvarindex(81)) = outofsystem/seconds_per_timestep !loss of water via groundwater flow from the model system, m3/s

      !Output of ilake
      IF(outvarindex(o_cloc)>0) outvar(i,outvarindex(o_cloc)) = basinoutflow(1)
      IF(simulate%substance(i_in).AND.outvarindex(102)>0) outvar(i,outvarindex(102)) = cbasinoutflow(i_in,1)*1000.
      IF(simulate%substance(i_on).AND.outvarindex(103)>0) outvar(i,outvarindex(103)) = cbasinoutflow(i_on,1)*1000.
      IF(simulate%substance(i_sp).AND.outvarindex(104)>0) outvar(i,outvarindex(104)) = cbasinoutflow(i_sp,1)*1000.
      IF(simulate%substance(i_pp).AND.outvarindex(105)>0) outvar(i,outvarindex(105)) = cbasinoutflow(i_pp,1)*1000.
      IF(simulate%substance(i_oc).AND.outvarindex(203)>0) outvar(i,outvarindex(203)) = cbasinoutflow(i_oc,1)    !orgC in flow from local river
      IF(i_in>0.AND.i_on>0.AND.outvarindex(106)>0) outvar(i,outvarindex(106)) = (cbasinoutflow(i_in,1) + cbasinoutflow(i_on,1))*1000.
      IF(i_sp>0.AND.i_pp>0.AND.outvarindex(107)>0) outvar(i,outvarindex(107)) = (cbasinoutflow(i_sp,1) + cbasinoutflow(i_pp,1))*1000.
      IF(simulate%substance(i_t1).AND.outvarindex(342)>0) outvar(i,outvarindex(342)) = cbasinoutflow(i_t1,1)    !T1 in flow from local river
      IF(simulate%substance(i_ss))THEN
        IF(outvarindex(o_clSS)>0) outvar(i,outvarindex(o_clSS)) = cbasinoutflow(i_ss,1)
        IF(outvarindex(o_clTS)>0) outvar(i,outvarindex(o_clTS)) = cbasinoutflow(i_ss,1) + cbasinoutflow(i_ae,1)*dryNratio
      ENDIF
      IF(slc_ilake>0)THEN
        IF(classbasin(i,slc_ilake)%part>0)THEN
          IF(simulate%substance(i_in).AND.outvarindex(o_c5IN)>0) outvar(i,outvarindex(o_c5IN)) = lakestate%conc(i_in,1,i)*1000.
          IF(simulate%substance(i_on).AND.outvarindex(o_c5ON)>0) outvar(i,outvarindex(o_c5ON)) = lakestate%conc(i_on,1,i)*1000.
          IF(simulate%substance(i_sp).AND.outvarindex(o_c5SP)>0) outvar(i,outvarindex(o_c5SP)) = lakestate%conc(i_sp,1,i)*1000.
          IF(simulate%substance(i_pp).AND.outvarindex(o_c5PP)>0) outvar(i,outvarindex(o_c5PP)) = lakestate%conc(i_pp,1,i)*1000.
          IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c5TN)>0) outvar(i,outvarindex(o_c5TN)) = (lakestate%conc(i_in,1,i) + lakestate%conc(i_on,1,i))*1000.
          IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c5TP)>0) outvar(i,outvarindex(o_c5TP)) = (lakestate%conc(i_sp,1,i) + lakestate%conc(i_pp,1,i))*1000.
          IF(simulate%substance(i_oc).AND.outvarindex(o_c5OC)>0) outvar(i,outvarindex(o_c5OC)) = lakestate%conc(i_oc,1,i)
          IF(simulate%substance(i_t1).AND.outvarindex(o_c5T1)>0) outvar(i,outvarindex(o_c5T1)) = lakestate%conc(i_t1,1,i)
          IF(simulate%substance(i_t2).AND.outvarindex(o_c5T2)>0) outvar(i,outvarindex(o_c5T2)) = lakestate%conc(i_t2,1,i)
          IF(simulate%substance(i_ss))THEN
            IF(outvarindex(o_c5SS)>0) outvar(i,outvarindex(o_c5SS)) = lakestate%conc(i_ss,1,i)
            IF(outvarindex(o_c5AE)>0) outvar(i,outvarindex(o_c5AE)) = lakestate%conc(i_ae,1,i)
            IF(outvarindex(o_c5TS)>0) outvar(i,outvarindex(o_c5TS)) = lakestate%conc(i_ss,1,i) + lakestate%conc(i_ae,1,i)*dryNratio
          ENDIF
        ENDIF
      ENDIF
      !Output main rivers
      !IF(conductwb.OR.outvarindex(114)>0.OR.outvarindex(204)>0) rivervolume(1) = riverstate%water(1,i) + (SUM(riverstate%qqueue(1:ttstep(1,i),1,i)) + riverstate%qqueue(ttstep(1,i)+1,1,i) * ttpart(1,i))
      IF(conductwb.OR.outvarindex(115)>0.OR.outvarindex(205)>0) rivervolume(2) = riverstate%water(2,i) + (SUM(riverstate%qqueue(1:ttstep(2,i),2,i)) + riverstate%qqueue(ttstep(2,i)+1,2,i) * ttpart(2,i))
      !IF(outvarindex(114)>0) outvar(i,outvarindex(114)) = rivervolume(1)
      IF(outvarindex(115)>0) outvar(i,outvarindex(115)) = rivervolume(2)
      !IF(outvarindex(204)>0.AND.riverarea(1)>0) outvar(i,outvarindex(204)) = rivervolume(1)/riverarea(1)   !local river depth [m]
      IF(outvarindex(205)>0.AND.riverarea(2)>0) outvar(i,outvarindex(205)) = rivervolume(2)/riverarea(2)   !main river depth [m]
      IF(simulate%substance(i_pp))THEN
        IF(outvarindex(o_psmr)>0) outvar(i,outvarindex(o_psmr)) = riverstate%Psed(2,i)
!        IF(outvarindex(o_pslr)>0) outvar(i,outvarindex(o_pslr)) = riverstate%Psed(1,i)
      ENDIF
      IF(outvarindex(o_mrfa)>0) outvar(i,outvarindex(o_mrfa)) = riverfracarea(2)       !main river fractional area (-)
      !IF(outvarindex(o_lrfa)>0) outvar(i,outvarindex(o_lrfa)) = riverfracarea(1)       !local river fractional area (-)
      IF(outvarindex(o_mred)>0) outvar(i,outvarindex(o_mred)) = rivereffectivedepth(2) !main river effecive depth (m)
      !IF(outvarindex(o_lred)>0) outvar(i,outvarindex(o_lred)) = rivereffectivedepth(1) !local river effecive depth (m)
      IF(simulate%substance(i_t1))THEN
        IF(outvarindex(o_tsmr)>0) outvar(i,outvarindex(o_tsmr)) = riverstate%T1sed(2,i)
!        IF(outvarindex(o_tslr)>0) outvar(i,outvarindex(o_tslr)) = riverstate%T1sed(1,i)
      ENDIF
      IF(simulate%substance(i_ss))THEN
        IF(outvarindex(o_ssmr)>0) outvar(i,outvarindex(o_ssmr)) = riverstate%Ssed(2,i)
!        IF(outvarindex(o_sslr)>0) outvar(i,outvarindex(o_sslr)) = riverstate%Ssed(1,i)
      ENDIF

      !Output irrigation
      IF(outvarindex(124)>0) outvar(i,outvarindex(124)) = ldremi         !Abstraction local dam for irrigation (m3)
      IF(outvarindex(125)>0) outvar(i,outvarindex(125)) = lrremi         !Abstraction local river for irrigation (m3)
      IF(outvarstatus(o_applirr)) CALL calculate_class_outvar_finish_scale(o_applirr,i,basin(i)%area*1.E-3)
      IF(outvarindex(127)>0) outvar(i,outvarindex(127)) = gwremi         !Ground water removed for irrigation (m3)
      IF(outvarindex(128)>0) outvar(i,outvarindex(128)) = rsremi         !Abstraction regional surface water for irrigation (m3)

      !Output concentration in olake (lakebasin or single lake)
      IF(slc_olake>0)THEN
        IF(classbasin(i,slc_olake)%part>0)THEN
          IF(simulate%substance(i_in).AND.outvarindex(o_c6IN)>0) outvar(i,outvarindex(o_c6IN)) = lakestate%conc(i_in,2,i)*1000.
          IF(simulate%substance(i_on).AND.outvarindex(o_c6ON)>0) outvar(i,outvarindex(o_c6ON)) = lakestate%conc(i_on,2,i)*1000.
          IF(simulate%substance(i_sp).AND.outvarindex(o_c6SP)>0) outvar(i,outvarindex(o_c6SP)) = lakestate%conc(i_sp,2,i)*1000.
          IF(simulate%substance(i_pp).AND.outvarindex(o_c6PP)>0) outvar(i,outvarindex(o_c6PP)) = lakestate%conc(i_pp,2,i)*1000.
          IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c6TN)>0) outvar(i,outvarindex(o_c6TN)) = (lakestate%conc(i_in,2,i) + lakestate%conc(i_on,2,i))*1000.
          IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c6TP)>0) outvar(i,outvarindex(o_c6TP)) = (lakestate%conc(i_sp,2,i) + lakestate%conc(i_pp,2,i))*1000.
          IF(simulate%substance(i_oc).AND.outvarindex(o_c6OC)>0) outvar(i,outvarindex(o_c6OC)) = lakestate%conc(i_oc,2,i)
          IF(simulate%substance(i_t1).AND.outvarindex(o_c6T1)>0) outvar(i,outvarindex(o_c6T1)) = lakestate%conc(i_t1,2,i)
          IF(simulate%substance(i_t2).AND.outvarindex(o_c6T2)>0) outvar(i,outvarindex(o_c6T2)) = lakestate%conc(i_t2,2,i)
          IF(simulate%substance(i_ss))THEN
            IF(outvarindex(o_c6SS)>0) outvar(i,outvarindex(o_c6SS)) = lakestate%conc(i_ss,2,i)
            IF(outvarindex(o_c6AE)>0) outvar(i,outvarindex(o_c6AE)) = lakestate%conc(i_ae,2,i)
            IF(outvarindex(o_c6TS)>0) outvar(i,outvarindex(o_c6TS)) = lakestate%conc(i_ss,2,i) + lakestate%conc(i_ae,2,i)*dryNratio
          ENDIF
        ENDIF
      ENDIF

      !>Set subbasin output variables (outvar) for print out dependent on outlet lake/no lake or lakebasins, NEED TO HANDLE LAKEBASINS
      IF(.NOT.statuslb)THEN
      !>Set subbasin output variables (outvar) for print out dependent on outlet lake
!        IF(numsubstances>0) netroutload(:,i) = netroutload(:,i) + basinoutflow(2)*cbasinoutflow(:,2)
        flow1000m3ts=basinoutflow(2)*seconds_per_timestep*1.E-3  !m3/s -> 1000m3/ts
        !Output of recorded load based on computed flow
        CALL set_outvar_xobs_scaled(207,i,o_reIN,flow1000m3ts*1.E-3)  !rec IN load in outflow, kg/ts
        CALL set_outvar_xobs_scaled(208,i,o_reON,flow1000m3ts*1.E-3)  !rec ON load in outflow, kg/ts
        CALL set_outvar_xobs_scaled(209,i,o_reSP,flow1000m3ts*1.E-3)  !rec SP load in outflow, kg/ts
        CALL set_outvar_xobs_scaled(210,i,o_rePP,flow1000m3ts*1.E-3)  !rec PP load in outflow, kg/ts
        CALL set_outvar_xobs_scaled(211,i,o_reTN,flow1000m3ts*1.E-3)  !rec TN load in outflow, kg/ts
        CALL set_outvar_xobs_scaled(212,i,o_reTP,flow1000m3ts*1.E-3)  !rec TP load in outflow, kg/ts
        CALL set_outvar_xobs_scaled(213,i,o_reOC,flow1000m3ts*1.E-3)  !rec DOC load in outflow, kg/ts

        !Output related to flow observations
        IF(ALLOCATED(qobsi)) THEN
          IF(qobsi(i)/=-9999) THEN
            IF(outvarindex(30)>0) outvar(i,outvarindex(30)) = outflowsim-qobsi(i)  !Daily error in Q
          ENDIF
        ENDIF
        IF(outvarindex(31)>0) outvar(i,outvarindex(31)) = outflowsim               !Simulated outflow subbasin (cobc)
        IF(firstoutstep) accdiff(i) = 0.
        IF(ALLOCATED(qobsi))THEN
          IF(qobsi(i)>=0.)THEN
            accdiff(i) = accdiff(i) + (basinoutflow(2)-qobsi(i))/upstreamarea(i)*seconds_per_timestep*1.E3  !accumulated volume error (mm)
            IF(outvarindex(100)>0) outvar(i,outvarindex(100))  = accdiff(i)
          ENDIF
        ENDIF

        !Output variables for olake water stage and lake volume
        IF(outvarindex(33)>0.OR.outvarindex(34)>0.OR.outvarindex(o_wcom)>0.OR. &
           outvarindex(o_cleanwcom)>0.OR.outvarindex(o_cleanwstr)>0.OR. &
           outvarindex(o_wcav)>0.OR.outvarindex(o_cleanwavg)>0)THEN
          IF(slc_olake>0)THEN
            IF(lakearea(2)>0.)THEN
              CALL calculate_olake_waterstage(i,wstlakesim,wstlake,w0ref)
              oldolakewst = wstlake + w0ref     !olake water stage before updating (m)
              IF(outvarindex(34)>0) outvar(i,outvarindex(34)) = oldolakewst
              CALL calculate_olake_waterstage(i,lakewst(2),wstlake,w0ref)
              CALL calculate_regamp_adjusted_waterstage(i,wstlake,wstlakeadj)
              IF(wstlakeadj/=missing_value.AND.outvarindex(o_cleanwcom)>0) outvar(i,outvarindex(o_cleanwcom)) = wstlakeadj    !regamp adjusted olake water stage (not in w-reference-system)
              IF(wstlakeadj/=missing_value.AND.outvarindex(o_wcom)>0) outvar(i,outvarindex(o_wcom)) = wstlakeadj + w0ref    !regamp adjusted olake water stage (in w-reference-system)
              IF(wcomaver/=missing_value)THEN
                IF(outvarindex(o_wcav)>0.OR.outvarindex(o_cleanwavg)>0)THEN
                  CALL calculate_olake_waterstage(i,wcomaver,wstlake,w0ref)
                  CALL calculate_regamp_adjusted_waterstage(i,wstlake,wstlakeadj)
                  IF(outvarindex(o_cleanwavg)>0) outvar(i,outvarindex(o_cleanwavg)) = wstlakeadj   !average olake water stage (not in w-reference-system)
                  IF(outvarindex(o_wcav)>0) outvar(i,outvarindex(o_wcav)) = wstlakeadj + w0ref   !average olake water stage (in w-reference-system)
                ENDIF
              ENDIF
              IF(xobsindex(o_rewstr,i)>0)THEN
                IF(xobsi(xobsindex(o_rewstr,i))/=missing_value)THEN
                  IF(outvarindex(o_cleanwstr)>0) outvar(i,outvarindex(o_cleanwstr)) = xobsi(xobsindex(o_rewstr,i)) - w0ref       !recorded olake waterstage (cleaned from w0ref)
                  IF(outvarindex(33)>0) outvar(i,outvarindex(33)) = oldolakewst - xobsi(xobsindex(o_rewstr,i)) !error in W
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDIF

        IF(outvarindex(o_cout)>0) outvar(i,outvarindex(o_cout)) = basinoutflow(2)   !computed outflow subbasin (m3/s)
        IF(outvarindex(o_coum)>0) outvar(i,outvarindex(o_coum)) = mainflow       !Discharge in main channel (m3/s)
        IF(outvarindex(o_coub)>0) outvar(i,outvarindex(o_coub)) = branchflow     !Discharge in branched river (m3/s)
        !IF(outvarindex(o_cout1)>0) outvar(i,outvarindex(o_cout1)) = missing_value                          !computed lakebasin flow (m3/s)
        !IF(outvarindex(o_cout2)>0) outvar(i,outvarindex(o_cout2)) = 0.                                     !no negative outflow for ordinary subbasins
        IF(simulate%substance(i_in).AND.outvarindex(55)>0) outvar(i,outvarindex(55)) = cbasinoutflow(i_in,2)*1000.   !comp conc IN outflow olake (ug/l)
        IF(simulate%substance(i_on).AND.outvarindex(56)>0) outvar(i,outvarindex(56)) = cbasinoutflow(i_on,2)*1000.   !comp conc ON outflow olake (ug/l)
        IF(simulate%substance(i_sp).AND.outvarindex(57)>0) outvar(i,outvarindex(57)) = cbasinoutflow(i_sp,2)*1000.   !comp conc SRP outflow olake (ug/l)
        IF(simulate%substance(i_pp).AND.outvarindex(58)>0) outvar(i,outvarindex(58)) = cbasinoutflow(i_pp,2)*1000.   !comp conc PP outflow olake (ug/l)
        IF(conduct%simC)THEN
          IF(outvarindex(46)>0) outvar(i,outvarindex(46)) = cbasinoutflow(i_oc,2)  !DOC conc outflow olake, mg/L
        ENDIF
        IF(simulate%substance(i_t1).AND.outvarindex(63)>0) outvar(i,outvarindex(63)) = cbasinoutflow(i_t1,2)                        !comp conc T1 conc in outflow
        IF(simulate%substance(i_t2).AND.outvarindex(64)>0) outvar(i,outvarindex(64)) = cbasinoutflow(i_t2,2)                        !comp conc T2 conc in outflow
        IF(i_in>0.AND.i_on>0.AND.outvarindex(77)>0) outvar(i,outvarindex(77)) = (cbasinoutflow(i_in,2) + cbasinoutflow(i_on,2))*1000.  !TN conc in outflow olake, ug/L
        IF(i_sp>0.AND.i_pp>0.AND.outvarindex(78)>0) outvar(i,outvarindex(78)) = (cbasinoutflow(i_sp,2) + cbasinoutflow(i_pp,2))*1000.  !TP conc in outflow olake, ug/L
        !IF(firstoutstep) accdiff(i) = 0.
        !IF(ALLOCATED(qobsi))THEN
        !  IF(qobsi(i)>=0.)THEN
        !    accdiff(i) = accdiff(i) + (basinoutflow(2)-qobsi(i))/upstreamarea(i)*seconds_per_timestep*1.E3  !accumulated volume error (mm)
        !    IF(outvarindex(100)>0) outvar(i,outvarindex(100))  = accdiff(i)
        !  ENDIF
        !ENDIF
        IF(i_in>0.AND.i_on>0.AND.outvarindex(o_ctnl)>0) outvar(i,outvarindex(o_ctnl)) = (cbasinoutflow(i_in,2) + cbasinoutflow(i_on,2))*flow1000m3ts  !TN load (kg/timestep)
        IF(simulate%substance(i_in).AND.outvarindex(o_cinl)>0) outvar(i,outvarindex(o_cinl)) = cbasinoutflow(i_in,2)*flow1000m3ts  !IN load (kg/timestep)
        IF(simulate%substance(i_on).AND.outvarindex(o_conl)>0) outvar(i,outvarindex(o_conl)) = cbasinoutflow(i_on,2)*flow1000m3ts  !ON load (kg/timestep)
        IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_ctpl)>0) outvar(i,outvarindex(o_ctpl)) = (cbasinoutflow(i_sp,2) + cbasinoutflow(i_pp,2))*flow1000m3ts  !TP load (kg/timestep)
        IF(simulate%substance(i_sp).AND.outvarindex(o_cspl)>0) outvar(i,outvarindex(o_cspl)) = cbasinoutflow(i_sp,2)*flow1000m3ts  !SP load (kg/timestep)
        IF(simulate%substance(i_pp).AND.outvarindex(o_cppl)>0) outvar(i,outvarindex(o_cppl)) = cbasinoutflow(i_pp,2)*flow1000m3ts  !PP load (kg/timestep)
        IF(simulate%substance(i_oc).AND.outvarindex(o_cocl)>0) outvar(i,outvarindex(o_cocl)) = cbasinoutflow(i_oc,2)*flow1000m3ts  !OC load (kg/timestep)
        IF(simulate%substance(i_ss).AND.outvarindex(o_cssl)>0) outvar(i,outvarindex(o_cssl)) = cbasinoutflow(i_ss,2)*flow1000m3ts  !SS load (kg/timestep)
        IF(i_ss>0.AND.i_ae>0.AND.outvarindex(o_ctsl)>0) outvar(i,outvarindex(o_ctsl)) = (cbasinoutflow(i_ss,2) + cbasinoutflow(i_ae,2)*dryNratio)*flow1000m3ts  !TS load (kg/timestep)
        !Sediment routing load output
        IF(numsubstances>0) netroutload(:,i) = netroutload(:,i) + basinoutflow(2)*cbasinoutflow(:,2)
        IF(simulate%substance(i_ss).AND.outvarindex(o_rnlss)>0) outvar(i,outvarindex(o_rnlss)) = netroutload(i_ss,i)*seconds_per_timestep*1.E-3  !SS net load routing (kg/timestep)
        IF(simulate%substance(i_ss).AND.outvarindex(o_rnlts)>0) outvar(i,outvarindex(o_rnlts)) = (netroutload(i_ss,i)+netroutload(i_ae,i)*dryNratio)*seconds_per_timestep*1.E-3  !TS net load routing (kg/timestep)
        IF(outvarindex(112)>0.AND.aolake>0) outvar(i,outvarindex(112)) = qcinfli   !total inflow (minus evaporation) to olake (m3/s)
        !IF(outvarindex(o_coum)>0) outvar(i,outvarindex(o_coum)) = mainflow       !Discharge in main channel (m3/s)
        !IF(outvarindex(o_coub)>0) outvar(i,outvarindex(o_coub)) = branchflow     !Discharge in branched river (m3/s)
        IF(outvarindex(o_specificq)>0) outvar(i,outvarindex(o_specificq)) = 1000.*(basinoutflow(2)*seconds_per_timestep) / upstreamarea(i) !Specific discharge (upstream runoff)
        IF(simulate%substance(i_ss))THEN
          IF(outvarindex(o_ccSS)>0) outvar(i,outvarindex(o_ccSS)) = cbasinoutflow(i_ss,2)
          IF(outvarindex(o_ccAE)>0) outvar(i,outvarindex(o_ccAE)) = cbasinoutflow(i_ae,2)
          IF(outvarindex(o_ccTS)>0) outvar(i,outvarindex(o_ccTS)) = cbasinoutflow(i_ss,2) + cbasinoutflow(i_ae,2)*dryNratio
        ENDIF
        !Output concentration of flow in main and branch
        IF(mainflow>0.)THEN
          IF(simulate%substance(i_in).AND.outvarindex(o_c1IN)>0) outvar(i,outvarindex(o_c1IN)) = cbasinoutflow(i_in,2)*1000.
          IF(simulate%substance(i_on).AND.outvarindex(o_c1ON)>0) outvar(i,outvarindex(o_c1ON)) = cbasinoutflow(i_on,2)*1000.
          IF(simulate%substance(i_sp).AND.outvarindex(o_c1SP)>0) outvar(i,outvarindex(o_c1SP)) = cbasinoutflow(i_sp,2)*1000.
          IF(simulate%substance(i_pp).AND.outvarindex(o_c1PP)>0) outvar(i,outvarindex(o_c1PP)) = cbasinoutflow(i_pp,2)*1000.
          IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c1TN)>0) outvar(i,outvarindex(o_c1TN)) = (cbasinoutflow(i_in,2) + cbasinoutflow(i_on,2))*1000.
          IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c1TP)>0) outvar(i,outvarindex(o_c1TP)) = (cbasinoutflow(i_sp,2) + cbasinoutflow(i_pp,2))*1000.
          IF(simulate%substance(i_oc).AND.outvarindex(o_c1OC)>0) outvar(i,outvarindex(o_c1OC)) = cbasinoutflow(i_oc,2)
          IF(simulate%substance(i_t1).AND.outvarindex(o_c1T1)>0) outvar(i,outvarindex(o_c1T1)) = cbasinoutflow(i_t1,2)
          IF(simulate%substance(i_t2).AND.outvarindex(o_c1T2)>0) outvar(i,outvarindex(o_c1T2)) = cbasinoutflow(i_t2,2)
          IF(simulate%substance(i_ss))THEN
            IF(outvarindex(o_c1SS)>0) outvar(i,outvarindex(o_c1SS)) = cbasinoutflow(i_ss,2)
            IF(outvarindex(o_c1AE)>0) outvar(i,outvarindex(o_c1AE)) = cbasinoutflow(i_ae,2)
            IF(outvarindex(o_c1TS)>0) outvar(i,outvarindex(o_c1TS)) = cbasinoutflow(i_ss,2) + cbasinoutflow(i_ae,2)*dryNratio
            IF(outvarindex(o_c3SS)>0) outvar(i,outvarindex(o_c3SS)) = cbasinoutflow(i_ss,2)
            IF(outvarindex(o_c3AE)>0) outvar(i,outvarindex(o_c3AE)) = cbasinoutflow(i_ae,2)
            IF(outvarindex(o_c3TS)>0) outvar(i,outvarindex(o_c3TS)) = cbasinoutflow(i_ss,2) + cbasinoutflow(i_ae,2)*dryNratio
          ENDIF
          IF(simulate%substance(i_in).AND.outvarindex(o_c3IN)>0) outvar(i,outvarindex(o_c3IN)) = cbasinoutflow(i_in,2)*1000.
          IF(simulate%substance(i_on).AND.outvarindex(o_c3ON)>0) outvar(i,outvarindex(o_c3ON)) = cbasinoutflow(i_on,2)*1000.
          IF(simulate%substance(i_sp).AND.outvarindex(o_c3SP)>0) outvar(i,outvarindex(o_c3SP)) = cbasinoutflow(i_sp,2)*1000.
          IF(simulate%substance(i_pp).AND.outvarindex(o_c3PP)>0) outvar(i,outvarindex(o_c3PP)) = cbasinoutflow(i_pp,2)*1000.
          IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c3TN)>0) outvar(i,outvarindex(o_c3TN)) = (cbasinoutflow(i_in,2) + cbasinoutflow(i_on,2))*1000.
          IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c3TP)>0) outvar(i,outvarindex(o_c3TP)) = (cbasinoutflow(i_sp,2) + cbasinoutflow(i_pp,2))*1000.
          IF(simulate%substance(i_oc).AND.outvarindex(o_c3OC)>0) outvar(i,outvarindex(o_c3OC)) = cbasinoutflow(i_oc,2)
          IF(simulate%substance(i_t1).AND.outvarindex(o_c3T1)>0) outvar(i,outvarindex(o_c3T1)) = cbasinoutflow(i_t1,2)
          IF(simulate%substance(i_t2).AND.outvarindex(o_c3T2)>0) outvar(i,outvarindex(o_c3T2)) = cbasinoutflow(i_t2,2)
        !ELSE
        !  IF(simulate%substance(i_in).AND.outvarindex(o_c1IN)>0) outvar(i,outvarindex(o_c1IN)) = 0.
        !  IF(simulate%substance(i_on).AND.outvarindex(o_c1ON)>0) outvar(i,outvarindex(o_c1ON)) = 0.
        !  IF(simulate%substance(i_sp).AND.outvarindex(o_c1SP)>0) outvar(i,outvarindex(o_c1SP)) = 0.
        !  IF(simulate%substance(i_pp).AND.outvarindex(o_c1PP)>0) outvar(i,outvarindex(o_c1PP)) = 0.
        !  IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c1TN)>0) outvar(i,outvarindex(o_c1TN)) = 0.
        !  IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c1TP)>0) outvar(i,outvarindex(o_c1TP)) = 0.
        !  IF(simulate%substance(i_oc).AND.outvarindex(o_c1OC)>0) outvar(i,outvarindex(o_c1OC)) = 0.
        !  IF(simulate%substance(i_t1).AND.outvarindex(o_c1T1)>0) outvar(i,outvarindex(o_c1T1)) = 0.
        !  IF(simulate%substance(i_t2).AND.outvarindex(o_c1T2)>0) outvar(i,outvarindex(o_c1T2)) = 0.
        !  IF(simulate%substance(i_ss))THEN
        !    IF(outvarindex(o_c1SS)>0) outvar(i,outvarindex(o_c1SS)) = 0.
        !    IF(outvarindex(o_c1AE)>0) outvar(i,outvarindex(o_c1AE)) = 0.
        !    IF(outvarindex(o_c1TS)>0) outvar(i,outvarindex(o_c1TS)) = 0.
        !    IF(outvarindex(o_c3SS)>0) outvar(i,outvarindex(o_c3SS)) = 0.
        !    IF(outvarindex(o_c3AE)>0) outvar(i,outvarindex(o_c3AE)) = 0.
        !    IF(outvarindex(o_c3TS)>0) outvar(i,outvarindex(o_c3TS)) = 0.
        !  ENDIF
        !  IF(simulate%substance(i_in).AND.outvarindex(o_c3IN)>0) outvar(i,outvarindex(o_c3IN)) = 0.
        !  IF(simulate%substance(i_on).AND.outvarindex(o_c3ON)>0) outvar(i,outvarindex(o_c3ON)) = 0.
        !  IF(simulate%substance(i_sp).AND.outvarindex(o_c3SP)>0) outvar(i,outvarindex(o_c3SP)) = 0.
        !  IF(simulate%substance(i_pp).AND.outvarindex(o_c3PP)>0) outvar(i,outvarindex(o_c3PP)) = 0.
        !  IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c3TN)>0) outvar(i,outvarindex(o_c3TN)) = 0.
        !  IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c3TP)>0) outvar(i,outvarindex(o_c3TP)) = 0.
        !  IF(simulate%substance(i_oc).AND.outvarindex(o_c3OC)>0) outvar(i,outvarindex(o_c3OC)) = 0.
        !  IF(simulate%substance(i_t1).AND.outvarindex(o_c3T1)>0) outvar(i,outvarindex(o_c3T1)) = 0.
        !  IF(simulate%substance(i_t2).AND.outvarindex(o_c3T2)>0) outvar(i,outvarindex(o_c3T2)) = 0.
        ENDIF
        IF(branchflow>0.)THEN
          IF(simulate%substance(i_in).AND.outvarindex(o_c2IN)>0) outvar(i,outvarindex(o_c2IN)) = cbasinoutflow(i_in,2)*1000.
          IF(simulate%substance(i_on).AND.outvarindex(o_c2ON)>0) outvar(i,outvarindex(o_c2ON)) = cbasinoutflow(i_on,2)*1000.
          IF(simulate%substance(i_sp).AND.outvarindex(o_c2SP)>0) outvar(i,outvarindex(o_c2SP)) = cbasinoutflow(i_sp,2)*1000.
          IF(simulate%substance(i_pp).AND.outvarindex(o_c2PP)>0) outvar(i,outvarindex(o_c2PP)) = cbasinoutflow(i_pp,2)*1000.
          IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c2TN)>0) outvar(i,outvarindex(o_c2TN)) = (cbasinoutflow(i_in,2) + cbasinoutflow(i_on,2))*1000.
          IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c2TP)>0) outvar(i,outvarindex(o_c2TP)) = (cbasinoutflow(i_sp,2) + cbasinoutflow(i_pp,2))*1000.
          IF(simulate%substance(i_oc).AND.outvarindex(o_c2OC)>0) outvar(i,outvarindex(o_c2OC)) = cbasinoutflow(i_oc,2)
          IF(simulate%substance(i_t1).AND.outvarindex(o_c2T1)>0) outvar(i,outvarindex(o_c2T1)) = cbasinoutflow(i_t1,2)
          IF(simulate%substance(i_t2).AND.outvarindex(o_c2T2)>0) outvar(i,outvarindex(o_c2T2)) = cbasinoutflow(i_t2,2)
          IF(simulate%substance(i_ss))THEN
            IF(outvarindex(o_c2SS)>0) outvar(i,outvarindex(o_c2SS)) = cbasinoutflow(i_ss,2)
            IF(outvarindex(o_c2AE)>0) outvar(i,outvarindex(o_c2AE)) = cbasinoutflow(i_ae,2)
            IF(outvarindex(o_c2TS)>0) outvar(i,outvarindex(o_c2TS)) = cbasinoutflow(i_ss,2) + cbasinoutflow(i_ae,2)*dryNratio
            IF(outvarindex(o_c4SS)>0) outvar(i,outvarindex(o_c4SS)) = cbasinoutflow(i_ss,2)
            IF(outvarindex(o_c4AE)>0) outvar(i,outvarindex(o_c4AE)) = cbasinoutflow(i_ae,2)
            IF(outvarindex(o_c4TS)>0) outvar(i,outvarindex(o_c4TS)) = cbasinoutflow(i_ss,2) + cbasinoutflow(i_ae,2)*dryNratio
          ENDIF
          IF(simulate%substance(i_in).AND.outvarindex(o_c4IN)>0) outvar(i,outvarindex(o_c4IN)) = cbasinoutflow(i_in,2)*1000.
          IF(simulate%substance(i_on).AND.outvarindex(o_c4ON)>0) outvar(i,outvarindex(o_c4ON)) = cbasinoutflow(i_on,2)*1000.
          IF(simulate%substance(i_sp).AND.outvarindex(o_c4SP)>0) outvar(i,outvarindex(o_c4SP)) = cbasinoutflow(i_sp,2)*1000.
          IF(simulate%substance(i_pp).AND.outvarindex(o_c4PP)>0) outvar(i,outvarindex(o_c4PP)) = cbasinoutflow(i_pp,2)*1000.
          IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c4TN)>0) outvar(i,outvarindex(o_c4TN)) = (cbasinoutflow(i_in,2) + cbasinoutflow(i_on,2))*1000.
          IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c4TP)>0) outvar(i,outvarindex(o_c4TP)) = (cbasinoutflow(i_sp,2) + cbasinoutflow(i_pp,2))*1000.
          IF(simulate%substance(i_oc).AND.outvarindex(o_c4OC)>0) outvar(i,outvarindex(o_c4OC)) = cbasinoutflow(i_oc,2)
          IF(simulate%substance(i_t1).AND.outvarindex(o_c4T1)>0) outvar(i,outvarindex(o_c4T1)) = cbasinoutflow(i_t1,2)
          IF(simulate%substance(i_t2).AND.outvarindex(o_c4T2)>0) outvar(i,outvarindex(o_c4T2)) = cbasinoutflow(i_t2,2)
        !ELSE
        !  !CALL set_substance_outvar_zero(i_in,o_c2IN,i)
        !  IF(simulate%substance(i_in).AND.outvarindex(o_c2IN)>0) outvar(i,outvarindex(o_c2IN)) = 0.
        !  IF(simulate%substance(i_on).AND.outvarindex(o_c2ON)>0) outvar(i,outvarindex(o_c2ON)) = 0.
        !  IF(simulate%substance(i_sp).AND.outvarindex(o_c2SP)>0) outvar(i,outvarindex(o_c2SP)) = 0.
        !  IF(simulate%substance(i_pp).AND.outvarindex(o_c2PP)>0) outvar(i,outvarindex(o_c2PP)) = 0.
        !  IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c2TN)>0) outvar(i,outvarindex(o_c2TN)) = 0.
        !  IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c2TP)>0) outvar(i,outvarindex(o_c2TP)) = 0.
        !  IF(simulate%substance(i_oc).AND.outvarindex(o_c2OC)>0) outvar(i,outvarindex(o_c2OC)) = 0.
        !  IF(simulate%substance(i_t1).AND.outvarindex(o_c2T1)>0) outvar(i,outvarindex(o_c2T1)) = 0.
        !  IF(simulate%substance(i_t2).AND.outvarindex(o_c2T2)>0) outvar(i,outvarindex(o_c2T2)) = 0.
        !  IF(simulate%substance(i_ss))THEN
        !    IF(outvarindex(o_c2SS)>0) outvar(i,outvarindex(o_c2SS)) = 0.
        !    IF(outvarindex(o_c2AE)>0) outvar(i,outvarindex(o_c2AE)) = 0.
        !    IF(outvarindex(o_c2TS)>0) outvar(i,outvarindex(o_c2TS)) = 0.
        !    IF(outvarindex(o_c4SS)>0) outvar(i,outvarindex(o_c4SS)) = 0.
        !    IF(outvarindex(o_c4AE)>0) outvar(i,outvarindex(o_c4AE)) = 0.
        !    IF(outvarindex(o_c4TS)>0) outvar(i,outvarindex(o_c4TS)) = 0.
        !  ENDIF
        !  IF(simulate%substance(i_in).AND.outvarindex(o_c4IN)>0) outvar(i,outvarindex(o_c4IN)) = 0.
        !  IF(simulate%substance(i_on).AND.outvarindex(o_c4ON)>0) outvar(i,outvarindex(o_c4ON)) = 0.
        !  IF(simulate%substance(i_sp).AND.outvarindex(o_c4SP)>0) outvar(i,outvarindex(o_c4SP)) = 0.
        !  IF(simulate%substance(i_pp).AND.outvarindex(o_c4PP)>0) outvar(i,outvarindex(o_c4PP)) = 0.
        !  IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c4TN)>0) outvar(i,outvarindex(o_c4TN)) = 0.
        !  IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c4TP)>0) outvar(i,outvarindex(o_c4TP)) = 0.
        !  IF(simulate%substance(i_oc).AND.outvarindex(o_c4OC)>0) outvar(i,outvarindex(o_c4OC)) = 0.
        !  IF(simulate%substance(i_t1).AND.outvarindex(o_c4T1)>0) outvar(i,outvarindex(o_c4T1)) = 0.
        !  IF(simulate%substance(i_t2).AND.outvarindex(o_c4T2)>0) outvar(i,outvarindex(o_c4T2)) = 0.
        ENDIF

        !Output of lake outflow
        IF(slc_olake>0)THEN
          IF(lakearea(2)>0.)THEN
            IF(outvarindex(o_coul)>0) outvar(i,outvarindex(o_coul)) = basinoutflow(2)     !computed outflow olake (m3/s)
            IF(simulate%substance(i_in).AND.outvarindex(o_c7IN)>0) outvar(i,outvarindex(o_c7IN)) = cbasinoutflow(i_in,2)*1000.
            IF(simulate%substance(i_on).AND.outvarindex(o_c7ON)>0) outvar(i,outvarindex(o_c7ON)) = cbasinoutflow(i_on,2)*1000.
            IF(simulate%substance(i_sp).AND.outvarindex(o_c7SP)>0) outvar(i,outvarindex(o_c7SP)) = cbasinoutflow(i_sp,2)*1000.
            IF(simulate%substance(i_pp).AND.outvarindex(o_c7PP)>0) outvar(i,outvarindex(o_c7PP)) = cbasinoutflow(i_pp,2)*1000.
            IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c7TN)>0) outvar(i,outvarindex(o_c7TN)) = (cbasinoutflow(i_in,2) + cbasinoutflow(i_on,2))*1000.
            IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c7TP)>0) outvar(i,outvarindex(o_c7TP)) = (cbasinoutflow(i_sp,2) + cbasinoutflow(i_pp,2))*1000.
            IF(simulate%substance(i_oc).AND.outvarindex(o_c7OC)>0) outvar(i,outvarindex(o_c7OC)) = cbasinoutflow(i_oc,2)
            IF(simulate%substance(i_t1).AND.outvarindex(o_c7T1)>0) outvar(i,outvarindex(o_c7T1)) = cbasinoutflow(i_t1,2)
            IF(simulate%substance(i_t2).AND.outvarindex(o_c7T2)>0) outvar(i,outvarindex(o_c7T2)) = cbasinoutflow(i_t2,2)
            IF(simulate%substance(i_ss))THEN
              IF(outvarindex(o_c7SS)>0) outvar(i,outvarindex(o_c7SS)) = cbasinoutflow(i_ss,2)
              IF(outvarindex(o_c7AE)>0) outvar(i,outvarindex(o_c7AE)) = cbasinoutflow(i_ae,2)
              IF(outvarindex(o_c7TS)>0) outvar(i,outvarindex(o_c7TS)) = cbasinoutflow(i_ss,2) + cbasinoutflow(i_ae,2)*dryNratio
            ENDIF
            IF(simulate%substance(i_in).AND.outvarindex(o_c8IN)>0) outvar(i,outvarindex(o_c8IN)) = cbasinoutflow(i_in,2)*1000.
            IF(simulate%substance(i_on).AND.outvarindex(o_c8ON)>0) outvar(i,outvarindex(o_c8ON)) = cbasinoutflow(i_on,2)*1000.
            IF(simulate%substance(i_sp).AND.outvarindex(o_c8SP)>0) outvar(i,outvarindex(o_c8SP)) = cbasinoutflow(i_sp,2)*1000.
            IF(simulate%substance(i_pp).AND.outvarindex(o_c8PP)>0) outvar(i,outvarindex(o_c8PP)) = cbasinoutflow(i_pp,2)*1000.
            IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c8TN)>0) outvar(i,outvarindex(o_c8TN)) = (cbasinoutflow(i_in,2) + cbasinoutflow(i_on,2))*1000.
            IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c8TP)>0) outvar(i,outvarindex(o_c8TP)) = (cbasinoutflow(i_sp,2) + cbasinoutflow(i_pp,2))*1000.
            IF(simulate%substance(i_oc).AND.outvarindex(o_c8OC)>0) outvar(i,outvarindex(o_c8OC)) = cbasinoutflow(i_oc,2)
            IF(simulate%substance(i_t1).AND.outvarindex(o_c8T1)>0) outvar(i,outvarindex(o_c8T1)) = cbasinoutflow(i_t1,2)
            IF(simulate%substance(i_t2).AND.outvarindex(o_c8T2)>0) outvar(i,outvarindex(o_c8T2)) = cbasinoutflow(i_t2,2)
            IF(simulate%substance(i_ss))THEN
              IF(outvarindex(o_c8SS)>0) outvar(i,outvarindex(o_c8SS)) = cbasinoutflow(i_ss,2)
              IF(outvarindex(o_c8AE)>0) outvar(i,outvarindex(o_c8AE)) = cbasinoutflow(i_ae,2)
              IF(outvarindex(o_c8TS)>0) outvar(i,outvarindex(o_c8TS)) = cbasinoutflow(i_ss,2) + cbasinoutflow(i_ae,2)*dryNratio
            ENDIF
          ENDIF
        ENDIF

      ELSEIF(statuslastlb)THEN
        !>Set subbasin output variables (outvar) for print out dependent on outlet lake for all lakebasins when reach last lakebasin
        DO i2=1,i
          IF(looplakes(i2))THEN
            !IF(numsubstances>0)THEN
            !  IF(clakeoutflowi2(1,i2)/=missing_value) netroutload(:,i2) = netroutload(:,i2) + lakeoutflowi2(i2)*clakeoutflowi2(:,i2)
            !ENDIF
            !Calculate positive outflow and its concentration for use in output
            positiveoutflow = 0.                          !compute outflow lakebasin (m3/s), only positive
            IF(mainflowi2(i2)>0.) positiveoutflow = positiveoutflow + mainflowi2(i2)
            IF(branchflowi2(i2)>0.) positiveoutflow = positiveoutflow + branchflowi2(i2)
            IF(numsubstances>0)THEN
              IF(mainflowi2(i2)>0. .AND. branchflowi2(i2)>0.)THEN
                cpositiveoutflow = (mainflowi2(i2)*cmainflowi2(:,i2)+branchflowi2(i2)*cbranchflowi2(:,i2))/positiveoutflow
              ELSEIF(mainflowi2(i2)>0.)THEN
                cpositiveoutflow = cmainflowi2(:,i2)
              ELSEIF(branchflowi2(i2)>0.)THEN
                cpositiveoutflow = cbranchflowi2(:,i2)
              ELSE
                cpositiveoutflow = missing_value
              ENDIF
            ENDIF
            flow1000m3ts=positiveoutflow*seconds_per_timestep*1.E-3  !m3/s -> 1000m3/ts
!            flow1000m3ts=lakeoutflowi2(i2)*seconds_per_timestep*1.E-3  !m3/s -> 1000m3/ts
            !Output of recorded load based on computed flow
            CALL set_outvar_xobs_scaled(207,i2,o_reIN,flow1000m3ts*1.E-3)  !rec IN load in outflow, kg/ts
            CALL set_outvar_xobs_scaled(208,i2,o_reON,flow1000m3ts*1.E-3)  !rec ON load in outflow, kg/ts
            CALL set_outvar_xobs_scaled(209,i2,o_reSP,flow1000m3ts*1.E-3)  !rec SP load in outflow, kg/ts
            CALL set_outvar_xobs_scaled(210,i2,o_rePP,flow1000m3ts*1.E-3)  !rec PP load in outflow, kg/ts
            CALL set_outvar_xobs_scaled(211,i2,o_reTN,flow1000m3ts*1.E-3)  !rec TN load in outflow, kg/ts
            CALL set_outvar_xobs_scaled(212,i2,o_reTP,flow1000m3ts*1.E-3)  !rec TP load in outflow, kg/ts
            CALL set_outvar_xobs_scaled(213,i2,o_reOC,flow1000m3ts*1.E-3)  !rec DOC load in outflow, kg/ts

            !Output outflow related to observations, outflowsimi2 is netflow, so use only for last lakebasin.
            IF(i2==i)THEN
              IF(firstoutstep) accdiff(i2) = 0.
              IF(ALLOCATED(qobsi))THEN
                IF(qobsi(i2)/=-9999) THEN
                  IF(outvarindex(30)>0) outvar(i2,outvarindex(30)) = outflowsimi2(i2)-qobsi(i2)  !Daily error in Q
                ENDIF
                IF(qobsi(i2)>=0.)THEN
                  accdiff(i2) = accdiff(i2) + (lakeoutflowi2(i2)-qobsi(i2))/upstreamarea(i2)*seconds_per_timestep*1.E3  !accumulated volume error (mm)
                  IF(outvarindex(100)>0) outvar(i2,outvarindex(100))  = accdiff(i2)
                ENDIF
              ENDIF
!              IF(outvarindex(31)>0) outvar(i2,outvarindex(31)) = outflowsimi2(i2)               !Simulated outflow subbasin (cobc)
            !ELSE   !not needed, missing_value is default
            !  IF(outvarindex(30)>0) outvar(i2,outvarindex(30)) = missing_value
            !  IF(outvarindex(31)>0) outvar(i2,outvarindex(31)) = missing_value
            !  IF(outvarindex(100)>0) outvar(i2,outvarindex(100)) = missing_value   !No comparison of flow within multi-basin lake
            ENDIF
            IF(outvarindex(31)>0) outvar(i2,outvarindex(31)) = outflowsimi2(i2)               !Simulated outflow subbasin (cobc), can be negative

            !Output variables for olake water stage and lake volume
            IF(outvarindex(33)>0.OR.outvarindex(34)>0.OR.outvarindex(o_wcom)>0.OR. &
               outvarindex(o_cleanwcom)>0.OR.outvarindex(o_cleanwstr)>0.OR. &
               outvarindex(o_wcav)>0.OR.outvarindex(o_cleanwavg)>0)THEN
              IF(slc_olake>0)THEN
                IF(lakeareai2(i2)>0.)THEN
                  CALL calculate_olake_waterstage(i2,wstlakesimi2(i2),wstlake,w0ref)
                  oldolakewst = wstlake + w0ref     !olake water stage of i2 before updating (m)
                  IF(outvarindex(34)>0) outvar(i2,outvarindex(34)) = oldolakewst
                  CALL calculate_olake_waterstage(i2,lakewsti2(i2),wstlake,w0ref)
                  CALL calculate_regamp_adjusted_waterstage(i2,wstlake,wstlakeadj)
                  IF(wstlakeadj/=missing_value.AND.outvarindex(o_cleanwcom)>0) outvar(i2,outvarindex(o_cleanwcom)) = wstlakeadj    !regamp adjusted olake water stage (not in w-reference-system)
                  IF(wstlakeadj/=missing_value.AND.outvarindex(o_wcom)>0) outvar(i2,outvarindex(o_wcom)) = wstlakeadj + w0ref    !regamp adjusted olake water stage (in w-reference-system)
                  IF(wcomaveri2(i2)/=missing_value)THEN
                    IF(outvarindex(o_wcav)>0.OR.outvarindex(o_cleanwavg)>0)THEN
                      CALL calculate_olake_waterstage(i2,wcomaveri2(i2),wstlake,w0ref)
                      CALL calculate_regamp_adjusted_waterstage(i2,wstlake,wstlakeadj)
                      IF(outvarindex(o_cleanwavg)>0) outvar(i2,outvarindex(o_cleanwavg)) = wstlakeadj   !average olake water stage (not in w-reference-system)
                      IF(outvarindex(o_wcav)>0) outvar(i2,outvarindex(o_wcav)) = wstlakeadj + w0ref   !average olake water stage (in w-reference-system)
                    ENDIF
                  ENDIF
                  IF(xobsindex(o_rewstr,i2)>0)THEN
                    IF(xobsi(xobsindex(o_rewstr,i2))/=missing_value)THEN
                      IF(outvarindex(o_cleanwstr)>0) outvar(i2,outvarindex(o_cleanwstr)) = xobsi(xobsindex(o_rewstr,i2)) - w0ref       !recorded olake waterstage (cleaned from w0ref)
                      IF(outvarindex(33)>0) outvar(i2,outvarindex(33)) = oldolakewst - xobsi(xobsindex(o_rewstr,i2)) !error in W
                    ENDIF
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
            !IF(conduct%simC.AND.outvarindex(46)>0) outvar(i2,outvarindex(46)) = clakeoutflowi2(i_oc,i2)  !DOC conc outflow olake, mg/L
!            IF(outvarindex(o_cout)>0) outvar(i2,outvarindex(o_cout)) = lakeoutflowi2(i2)                          !computed outflow olake (m3/s) net outflow
            IF(outvarindex(o_cout)>0) outvar(i2,outvarindex(o_cout)) = positiveoutflow   !computed outflow subbasin (m3/s), only positive
            IF(outvarindex(o_specificq)>0) outvar(i2,outvarindex(o_specificq)) = 1000.*(positiveoutflow*seconds_per_timestep) / upstreamarea(i2) !Specific discharge (upstream runoff)
            !IF(outvarindex(o_cout)>0)THEN
            !  outvar(i2,outvarindex(o_cout)) = 0.                          !computed outflow olake (m3/s), only positive
            !  IF(mainflowi2(i2)>0.) outvar(i2,outvarindex(o_cout)) = outvar(i2,outvarindex(o_cout)) + mainflowi2(i2)
            !  IF(mainflowi2(i2)>0.) outvar(i2,outvarindex(o_cout)) = outvar(i2,outvarindex(o_cout)) + branchflowi2(i2)
            !ENDIF
            IF(outvarindex(o_coum)>0) outvar(i2,outvarindex(o_coum)) = mainflowi2(i2)       !Discharge in main channel (m3/s)
            IF(outvarindex(o_coub)>0) outvar(i2,outvarindex(o_coub)) = branchflowi2(i2)     !Discharge in branched river (m3/s)
            !IF(outvarindex(o_cout1)>0)THEN
            !  IF(path(i2)%uplakebasin)THEN
            !    outvar(i2,outvarindex(o_cout1)) = mainflowi2(i2)
            !    IF(ALLOCATED(branchindex))THEN
            !      IF(branchindex(i2)>0)THEN
            !        IF(branchdata(branchindex(i2))%uplakebasin) outvar(i2,outvarindex(o_cout1)) = outvar(i2,outvarindex(o_cout1)) + branchflowi2(i2)
            !      ENDIF
            !    ENDIF
            !  ELSE
            !    outvar(i2,outvarindex(o_cout1)) = missing_value
            !  ENDIF
            !ENDIF
            !Set concentrations of subbasin outflow (cout), only positive mainflow and branchflow included.
            IF(mainflowi2(i2)>0. .OR. branchflowi2(i2)>0.)THEN
              IF(conduct%simN)THEN
                IF(outvarindex(55)>0) outvar(i2,outvarindex(55)) = cpositiveoutflow(i_in)*1000.
                IF(outvarindex(56)>0) outvar(i2,outvarindex(56)) = cpositiveoutflow(i_on)*1000.
                IF(outvarindex(77)>0) outvar(i2,outvarindex(77)) = (cpositiveoutflow(i_in) + cpositiveoutflow(i_on))*1000.
              ENDIF
              IF(conduct%simP)THEN
                IF(outvarindex(57)>0) outvar(i2,outvarindex(57)) = cpositiveoutflow(i_sp)*1000.
                IF(outvarindex(58)>0) outvar(i2,outvarindex(58)) = cpositiveoutflow(i_pp)*1000.
                IF(outvarindex(78)>0) outvar(i2,outvarindex(78)) = (cpositiveoutflow(i_sp) + cpositiveoutflow(i_pp))*1000
              ENDIF
              IF(conduct%simC.AND.outvarindex(46)>0) outvar(i2,outvarindex(46)) = cpositiveoutflow(i_oc)
              IF(simulate%substance(i_t1).AND.outvarindex(63)>0) outvar(i2,outvarindex(63)) = cpositiveoutflow(i_t1)
              IF(simulate%substance(i_t2).AND.outvarindex(64)>0) outvar(i2,outvarindex(64)) = cpositiveoutflow(i_t2)
              IF(simulate%substance(i_ss))THEN
                IF(outvarindex(o_ccSS)>0) outvar(i2,outvarindex(o_ccSS)) = cpositiveoutflow(i_ss)
                IF(outvarindex(o_ccAE)>0) outvar(i2,outvarindex(o_ccAE)) = cpositiveoutflow(i_ae)
                IF(outvarindex(o_ccTS)>0) outvar(i2,outvarindex(o_ccTS)) = cpositiveoutflow(i_ss) + cpositiveoutflow(i_ae)*dryNratio
              ENDIF
            !ELSE   !not needed, missing_value is default
            !  IF(conduct%simN)THEN
            !    IF(outvarindex(55)>0) outvar(i2,outvarindex(55)) = missing_value
            !    IF(outvarindex(56)>0) outvar(i2,outvarindex(56)) = missing_value
            !    IF(outvarindex(77)>0) outvar(i2,outvarindex(77)) = missing_value
            !  ENDIF
            !  IF(conduct%simP)THEN
            !    IF(outvarindex(57)>0) outvar(i2,outvarindex(57)) = missing_value
            !    IF(outvarindex(58)>0) outvar(i2,outvarindex(58)) = missing_value
            !    IF(outvarindex(78)>0) outvar(i2,outvarindex(78)) = missing_value
            !  ENDIF
            !  IF(conduct%simC.AND.outvarindex(46)>0) outvar(i2,outvarindex(46)) = missing_value
            !  IF(simulate%substance(i_t1).AND.outvarindex(63)>0) outvar(i2,outvarindex(63)) = missing_value
            !  IF(simulate%substance(i_t2).AND.outvarindex(64)>0) outvar(i2,outvarindex(64)) = missing_value
            !  IF(simulate%substance(i_ss))THEN
            !    IF(outvarindex(o_ccSS)>0) outvar(i2,outvarindex(o_ccSS)) = missing_value
            !    IF(outvarindex(o_ccAE)>0) outvar(i2,outvarindex(o_ccAE)) = missing_value
            !    IF(outvarindex(o_ccTS)>0) outvar(i2,outvarindex(o_ccTS)) = missing_value
            !  ENDIF
            ENDIF
            !IF(mainflowi2(i2)>0. .AND. branchflowi2(i2)>0.)THEN
            !  IF(conduct%simN)THEN
            !    IF(outvarindex(55)>0) outvar(i2,outvarindex(55)) = (mainflowi2(i2)*cmainflowi2(i_in,i2)+branchflowi2(i2)*cbranchflowi2(i_in,i2))/(mainflowi2(i2)+branchflowi2(i2))*1000.
            !    IF(outvarindex(56)>0) outvar(i2,outvarindex(56)) = (mainflowi2(i2)*cmainflowi2(i_on,i2)+branchflowi2(i2)*cbranchflowi2(i_on,i2))/(mainflowi2(i2)+branchflowi2(i2))*1000.
            !    IF(outvarindex(77)>0) outvar(i2,outvarindex(77)) = (mainflowi2(i2)*(cmainflowi2(i_in,i2) + cmainflowi2(i_on,i2))+branchflowi2(i2)*(cbranchflowi2(i_in,i2) + cbranchflowi2(i_on,i2)))/(mainflowi2(i2)+branchflowi2(i2))*1000.
            !  ENDIF
            !  IF(conduct%simP)THEN
            !    IF(outvarindex(57)>0) outvar(i2,outvarindex(57)) = (mainflowi2(i2)*cmainflowi2(i_sp,i2)+branchflowi2(i2)*cbranchflowi2(i_sp,i2))/(mainflowi2(i2)+branchflowi2(i2))*1000.
            !    IF(outvarindex(58)>0) outvar(i2,outvarindex(58)) = (mainflowi2(i2)*cmainflowi2(i_pp,i2)+branchflowi2(i2)*cbranchflowi2(i_pp,i2))/(mainflowi2(i2)+branchflowi2(i2))*1000.
            !    IF(outvarindex(78)>0) outvar(i2,outvarindex(78)) = (mainflowi2(i2)*(cmainflowi2(i_sp,i2) + cmainflowi2(i_pp,i2))+branchflowi2(i2)*(cbranchflowi2(i_sp,i2) + cbranchflowi2(i_pp,i2)))/(mainflowi2(i2)+branchflowi2(i2))*1000.
            !  ENDIF
            !  IF(conduct%simC.AND.outvarindex(46)>0) outvar(i2,outvarindex(46)) = (mainflowi2(i2)*cmainflowi2(i_oc,i2)+branchflowi2(i2)*cbranchflowi2(i_oc,i2))/(mainflowi2(i2)+branchflowi2(i2))
            !  IF(simulate%substance(i_t1).AND.outvarindex(63)>0) outvar(i2,outvarindex(63)) = (mainflowi2(i2)*cmainflowi2(i_t1,i2)+branchflowi2(i2)*cbranchflowi2(i_t1,i2))/(mainflowi2(i2)+branchflowi2(i2))
            !  IF(simulate%substance(i_t2).AND.outvarindex(64)>0) outvar(i2,outvarindex(64)) = (mainflowi2(i2)*cmainflowi2(i_t2,i2)+branchflowi2(i2)*cbranchflowi2(i_t2,i2))/(mainflowi2(i2)+branchflowi2(i2))
            !  IF(simulate%substance(i_ss))THEN
            !    IF(outvarindex(o_ccSS)>0) outvar(i2,outvarindex(o_ccSS)) = (mainflowi2(i2)*cmainflowi2(i_ss,i2)+branchflowi2(i2)*cbranchflowi2(i_ss,i2))/(mainflowi2(i2)+branchflowi2(i2))
            !    IF(outvarindex(o_ccAE)>0) outvar(i2,outvarindex(o_ccAE)) = (mainflowi2(i2)*cmainflowi2(i_ae,i2)+branchflowi2(i2)*cbranchflowi2(i_ae,i2))/(mainflowi2(i2)+branchflowi2(i2))
            !    IF(outvarindex(o_ccTS)>0) outvar(i2,outvarindex(o_ccTS)) = (mainflowi2(i2)*(cmainflowi2(i_ss,i2) + cmainflowi2(i_ae,i2)*dryNratio)+branchflowi2(i2)*(cbranchflowi2(i_ss,i2) + cbranchflowi2(i_ae,i2)*dryNratio))/(mainflowi2(i2)+branchflowi2(i2))
            !  ENDIF
            !ELSEIF(mainflowi2(i2)>0.)THEN
            !  IF(conduct%simN)THEN
            !    IF(outvarindex(55)>0) outvar(i2,outvarindex(55)) = cmainflowi2(i_in,i2)*1000.
            !    IF(outvarindex(56)>0) outvar(i2,outvarindex(56)) = cmainflowi2(i_on,i2)*1000.
            !    IF(outvarindex(77)>0) outvar(i2,outvarindex(77)) = (cmainflowi2(i_in,i2) + cmainflowi2(i_on,i2))*1000.
            !  ENDIF
            !  IF(conduct%simP)THEN
            !    IF(outvarindex(57)>0) outvar(i2,outvarindex(57)) = cmainflowi2(i_sp,i2)*1000.
            !    IF(outvarindex(58)>0) outvar(i2,outvarindex(58)) = cmainflowi2(i_pp,i2)*1000.
            !    IF(outvarindex(78)>0) outvar(i2,outvarindex(78)) = (cmainflowi2(i_sp,i2) + cmainflowi2(i_pp,i2))*1000.
            !  ENDIF
            !  IF(conduct%simC.AND.outvarindex(46)>0) outvar(i2,outvarindex(46)) = cmainflowi2(i_oc,i2)
            !  IF(simulate%substance(i_t1).AND.outvarindex(63)>0) outvar(i2,outvarindex(63)) = cmainflowi2(i_t1,i2)
            !  IF(simulate%substance(i_t2).AND.outvarindex(64)>0) outvar(i2,outvarindex(64)) = cmainflowi2(i_t2,i2)
            !  IF(simulate%substance(i_ss))THEN
            !    IF(outvarindex(o_ccSS)>0) outvar(i2,outvarindex(o_ccSS)) = cmainflowi2(i_ss,i2)
            !    IF(outvarindex(o_ccAE)>0) outvar(i2,outvarindex(o_ccAE)) = cmainflowi2(i_ae,i2)
            !    IF(outvarindex(o_ccTS)>0) outvar(i2,outvarindex(o_ccTS)) = cmainflowi2(i_ss,i2) + cmainflowi2(i_ae,i2)*dryNratio
            !  ENDIF
            !ELSEIF(branchflowi2(i2)>0.)THEN
            !  IF(conduct%simN)THEN
            !    IF(outvarindex(55)>0) outvar(i2,outvarindex(55)) = cbranchflowi2(i_in,i2)*1000.
            !    IF(outvarindex(56)>0) outvar(i2,outvarindex(56)) = cbranchflowi2(i_on,i2)*1000.
            !    IF(outvarindex(77)>0) outvar(i2,outvarindex(77)) = (cbranchflowi2(i_in,i2) + cbranchflowi2(i_on,i2))*1000.
            !  ENDIF
            !  IF(conduct%simP)THEN
            !    IF(outvarindex(57)>0) outvar(i2,outvarindex(57)) = cbranchflowi2(i_sp,i2)*1000.
            !    IF(outvarindex(58)>0) outvar(i2,outvarindex(58)) = cbranchflowi2(i_pp,i2)*1000.
            !    IF(outvarindex(78)>0) outvar(i2,outvarindex(78)) = (cbranchflowi2(i_sp,i2) + cbranchflowi2(i_pp,i2))*1000.
            !  ENDIF
            !  IF(conduct%simC.AND.outvarindex(46)>0) outvar(i2,outvarindex(46)) = cbranchflowi2(i_oc,i2)
            !  IF(simulate%substance(i_t1).AND.outvarindex(63)>0) outvar(i2,outvarindex(63)) = cbranchflowi2(i_t1,i2)
            !  IF(simulate%substance(i_t2).AND.outvarindex(64)>0) outvar(i2,outvarindex(64)) = cbranchflowi2(i_t2,i2)
            !  IF(simulate%substance(i_ss))THEN
            !    IF(outvarindex(o_ccSS)>0) outvar(i2,outvarindex(o_ccSS)) = cbranchflowi2(i_ss,i2)
            !    IF(outvarindex(o_ccAE)>0) outvar(i2,outvarindex(o_ccAE)) = cbranchflowi2(i_ae,i2)
            !    IF(outvarindex(o_ccTS)>0) outvar(i2,outvarindex(o_ccTS)) = cbranchflowi2(i_ss,i2) + cbranchflowi2(i_ae,i2)*dryNratio
            !  ENDIF
            !ELSE
            !  IF(conduct%simN)THEN
            !    IF(outvarindex(55)>0) outvar(i2,outvarindex(55)) = missing_value
            !    IF(outvarindex(56)>0) outvar(i2,outvarindex(56)) = missing_value
            !    IF(outvarindex(77)>0) outvar(i2,outvarindex(77)) = missing_value
            !  ENDIF
            !  IF(conduct%simP)THEN
            !    IF(outvarindex(57)>0) outvar(i2,outvarindex(57)) = missing_value
            !    IF(outvarindex(58)>0) outvar(i2,outvarindex(58)) = missing_value
            !    IF(outvarindex(78)>0) outvar(i2,outvarindex(78)) = missing_value
            !  ENDIF
            !  IF(conduct%simC.AND.outvarindex(46)>0) outvar(i2,outvarindex(46)) = missing_value
            !  IF(simulate%substance(i_t1).AND.outvarindex(63)>0) outvar(i2,outvarindex(63)) = missing_value
            !  IF(simulate%substance(i_t2).AND.outvarindex(64)>0) outvar(i2,outvarindex(64)) = missing_value
            !  IF(simulate%substance(i_ss))THEN
            !    IF(outvarindex(o_ccSS)>0) outvar(i2,outvarindex(o_ccSS)) = missing_value
            !    IF(outvarindex(o_ccAE)>0) outvar(i2,outvarindex(o_ccAE)) = missing_value
            !    IF(outvarindex(o_ccTS)>0) outvar(i2,outvarindex(o_ccTS)) = missing_value
            !  ENDIF
            !ENDIF

            !IF(conduct%simN)THEN
            !  IF(outvarindex(55)>0.AND.clakeoutflowi2(i_in,i2)/=missing_value) outvar(i2,outvarindex(55)) = clakeoutflowi2(i_in,i2)*1000.   !comp conc IN outflow olake (ug/l)
            !  IF(outvarindex(56)>0.AND.clakeoutflowi2(i_on,i2)/=missing_value) outvar(i2,outvarindex(56)) = clakeoutflowi2(i_on,i2)*1000.   !comp conc ON outflow olake (ug/l)
            !  IF(outvarindex(77)>0.AND.clakeoutflowi2(i_in,i2)/=missing_value.AND.clakeoutflowi2(i_on,i2)/=missing_value) outvar(i2,outvarindex(77)) = (clakeoutflowi2(i_in,i2) + clakeoutflowi2(i_on,i2))*1000.  !TN conc in outflow olake, ug/L
            !ENDIF
            !IF(conduct%simP)THEN
            !  IF(outvarindex(57)>0.AND.clakeoutflowi2(i_sp,i2)/=missing_value) outvar(i2,outvarindex(57)) = clakeoutflowi2(i_sp,i2)*1000.   !comp conc SRP outflow olake (ug/l)
            !  IF(outvarindex(58)>0.AND.clakeoutflowi2(i_pp,i2)/=missing_value) outvar(i2,outvarindex(58)) = clakeoutflowi2(i_pp,i2)*1000.   !comp conc PP outflow olake (ug/l)
            !  IF(outvarindex(78)>0.AND.clakeoutflowi2(i_sp,i2)/=missing_value.AND.clakeoutflowi2(i_pp,i2)/=missing_value) outvar(i2,outvarindex(78)) = (clakeoutflowi2(i_sp,i2) + clakeoutflowi2(i_pp,i2))*1000.  !TP conc in outflow olake, ug/L
            !ENDIF
            !IF(conduct%simC.AND.outvarindex(46)>0) outvar(i2,outvarindex(46)) = clakeoutflowi2(i_oc,i2)  !DOC conc outflow olake, mg/L
            !IF(simulate%substance(i_t1).AND.outvarindex(63)>0) outvar(i2,outvarindex(63)) = clakeoutflowi2(i_t1,i2)                        !comp conc T1 conc in outflow
            !IF(simulate%substance(i_t2).AND.outvarindex(64)>0) outvar(i2,outvarindex(64)) = clakeoutflowi2(i_t2,i2)                        !comp conc T2 conc in outflow
            !IF(simulate%substance(i_ss))THEN
            !  IF(outvarindex(o_ccSS)>0.AND.clakeoutflowi2(i_ss,i2)/=missing_value) outvar(i2,outvarindex(o_ccSS)) = clakeoutflowi2(i_ss,i2)
            !  IF(outvarindex(o_ccAE)>0.AND.clakeoutflowi2(i_ae,i2)/=missing_value) outvar(i2,outvarindex(o_ccAE)) = clakeoutflowi2(i_ae,i2)
            !  IF(outvarindex(o_ccTS)>0.AND.clakeoutflowi2(i_ss,i2)/=missing_value.AND.clakeoutflowi2(i_ae,i2)/=missing_value) outvar(i2,outvarindex(o_ccTS)) = clakeoutflowi2(i_ss,i2) + clakeoutflowi2(i_ae,i2)*dryNratio
            !ENDIF
            !IF(i2==i)THEN
            !  IF(firstoutstep) accdiff(i2) = 0.
            !  IF(ALLOCATED(qobsi))THEN
            !    IF(qobsi(i2)>=0.)THEN
            !      accdiff(i2) = accdiff(i2) + (lakeoutflowi2(i2)-qobsi(i2))/upstreamarea(i2)*seconds_per_timestep*1.E3  !accumulated volume error (mm)
            !      IF(outvarindex(100)>0) outvar(i2,outvarindex(100))  = accdiff(i2)
            !    ENDIF
            !  ENDIF
            !ELSE
            !  IF(outvarindex(100)>0) outvar(i2,outvarindex(100)) = missing_value   !No comparison of flow within multi-basin lake
            !ENDIF
            !Output load from subbasin, based on positiveoutflow
            IF(conduct%simN)THEN
              IF(outvarindex(o_ctnl)>0.AND.cpositiveoutflow(i_in)/=missing_value.AND.cpositiveoutflow(i_on)/=missing_value) outvar(i2,outvarindex(o_ctnl)) = (cpositiveoutflow(i_in) + cpositiveoutflow(i_on))*flow1000m3ts  !TN load (kg/timestep)
              IF(outvarindex(o_cinl)>0.AND.cpositiveoutflow(i_in)/=missing_value) outvar(i2,outvarindex(o_cinl)) = cpositiveoutflow(i_in)*flow1000m3ts  !IN load (kg/timestep)
              IF(outvarindex(o_conl)>0.AND.cpositiveoutflow(i_on)/=missing_value) outvar(i2,outvarindex(o_conl)) = cpositiveoutflow(i_on)*flow1000m3ts  !ON load (kg/timestep)
            ENDIF
            IF(conduct%simP)THEN
              IF(outvarindex(o_ctpl)>0.AND.cpositiveoutflow(i_sp)/=missing_value.AND.cpositiveoutflow(i_pp)/=missing_value) outvar(i2,outvarindex(o_ctpl)) = (cpositiveoutflow(i_sp) + cpositiveoutflow(i_pp))*flow1000m3ts  !TP load (kg/timestep)
              IF(outvarindex(o_cspl)>0.AND.cpositiveoutflow(i_sp)/=missing_value) outvar(i2,outvarindex(o_cspl)) = cpositiveoutflow(i_sp)*flow1000m3ts  !SP load (kg/timestep)
              IF(outvarindex(o_cppl)>0.AND.cpositiveoutflow(i_pp)/=missing_value) outvar(i2,outvarindex(o_cppl)) = cpositiveoutflow(i_pp)*flow1000m3ts  !PP load (kg/timestep)
            ENDIF
            IF(simulate%substance(i_oc).AND.outvarindex(o_cocl)>0)THEN
              IF(cpositiveoutflow(i_oc)/=missing_value) outvar(i2,outvarindex(o_cocl)) = cpositiveoutflow(i_oc)*flow1000m3ts  !OC load (kg/timestep)
            ENDIF
            IF(conduct%simS)THEN
              IF(outvarindex(o_cssl)>0.AND.cpositiveoutflow(i_ss)/=missing_value) outvar(i2,outvarindex(o_cssl)) = cpositiveoutflow(i_ss)*flow1000m3ts  !SS load (kg/timestep)
              IF(outvarindex(o_ctsl)>0.AND.cpositiveoutflow(i_ss)/=missing_value.AND.cpositiveoutflow(i_ae)/=missing_value) outvar(i2,outvarindex(o_ctsl)) = (cpositiveoutflow(i_ss) + cpositiveoutflow(i_ae)*dryNratio)*flow1000m3ts  !TS load (kg/timestep)
            ENDIF
            !IF(conduct%simN)THEN
            !  IF(outvarindex(o_ctnl)>0.AND.clakeoutflowi2(i_in,i2)/=missing_value.AND.clakeoutflowi2(i_on,i2)/=missing_value) outvar(i2,outvarindex(o_ctnl)) = (clakeoutflowi2(i_in,i2) + clakeoutflowi2(i_on,i2))*flow1000m3ts  !TN load (kg/timestep)
            !  IF(outvarindex(o_cinl)>0.AND.clakeoutflowi2(i_in,i2)/=missing_value) outvar(i2,outvarindex(o_cinl)) = clakeoutflowi2(i_in,i2)*flow1000m3ts  !IN load (kg/timestep)
            !  IF(outvarindex(o_conl)>0.AND.clakeoutflowi2(i_on,i2)/=missing_value) outvar(i2,outvarindex(o_conl)) = clakeoutflowi2(i_on,i2)*flow1000m3ts  !ON load (kg/timestep)
            !ENDIF
            !IF(conduct%simP)THEN
            !  IF(outvarindex(o_ctpl)>0.AND.clakeoutflowi2(i_sp,i2)/=missing_value.AND.clakeoutflowi2(i_pp,i2)/=missing_value) outvar(i2,outvarindex(o_ctpl)) = (clakeoutflowi2(i_sp,i2) + clakeoutflowi2(i_pp,i2))*flow1000m3ts  !TP load (kg/timestep)
            !  IF(outvarindex(o_cspl)>0.AND.clakeoutflowi2(i_sp,i2)/=missing_value) outvar(i2,outvarindex(o_cspl)) = clakeoutflowi2(i_sp,i2)*flow1000m3ts  !SP load (kg/timestep)
            !  IF(outvarindex(o_cppl)>0.AND.clakeoutflowi2(i_pp,i2)/=missing_value) outvar(i2,outvarindex(o_cppl)) = clakeoutflowi2(i_pp,i2)*flow1000m3ts  !PP load (kg/timestep)
            !ENDIF
            !IF(simulate%substance(i_oc).AND.outvarindex(o_cocl)>0)THEN
            !  IF(clakeoutflowi2(i_oc,i2)/=missing_value) outvar(i2,outvarindex(o_cocl)) = clakeoutflowi2(i_oc,i2)*flow1000m3ts  !OC load (kg/timestep)
            !ENDIF
            !IF(conduct%simS)THEN
            !  IF(outvarindex(o_cssl)>0.AND.clakeoutflowi2(i_ss,i2)/=missing_value) outvar(i2,outvarindex(o_cssl)) = clakeoutflowi2(i_ss,i2)*flow1000m3ts  !SS load (kg/timestep)
            !  IF(outvarindex(o_ctsl)>0.AND.clakeoutflowi2(i_ss,i2)/=missing_value.AND.clakeoutflowi2(i_ae,i2)/=missing_value) outvar(i2,outvarindex(o_ctsl)) = (clakeoutflowi2(i_ss,i2) + clakeoutflowi2(i_ae,i2))*flow1000m3ts  !TS load (kg/timestep)
            !ENDIF
            !Sediment routing load output, baset on netflows
            IF(numsubstances>0)THEN
              IF(clakeoutflowi2(1,i2)/=missing_value) netroutload(:,i2) = netroutload(:,i2) + lakeoutflowi2(i2)*clakeoutflowi2(:,i2)
              IF(conduct%simS)THEN
                IF(outvarindex(o_rnlss)>0) outvar(i2,outvarindex(o_rnlss)) = netroutload(i_ss,i2)*seconds_per_timestep*1.E-3  !SS net load routing (kg/timestep)
                IF(outvarindex(o_rnlts)>0) outvar(i2,outvarindex(o_rnlts)) = (netroutload(i_ss,i2)+netroutload(i_ae,i2)*dryNratio)*seconds_per_timestep*1.E-3  !TS net load routing (kg/timestep)
              ENDIF
            ENDIF
            IF(outvarindex(112)>0) outvar(i2,outvarindex(112)) = qcinflii2(i2)   !total inflow (minus evaporation) to olake (m3/s)
            !IF(outvarindex(o_coum)>0) outvar(i2,outvarindex(o_coum)) = mainflowi2(i2)       !Discharge in main channel (m3/s)
            !IF(outvarindex(o_coub)>0) outvar(i2,outvarindex(o_coub)) = branchflowi2(i2)     !Discharge in branched river (m3/s)
!            IF(outvarindex(o_specificq)>0) outvar(i2,outvarindex(o_specificq)) = 1000.*(positiveoutflow*seconds_per_timestep) / upstreamarea(i2) !Specific discharge (upstream runoff)
!            IF(outvarindex(o_specificq)>0) outvar(i2,outvarindex(o_specificq)) = 1000.*(lakeoutflowi2(i2)*seconds_per_timestep) / upstreamarea(i2) !Specific discharge (upstream runoff)
            !IF(simulate%substance(i_ss))THEN
            !  IF(outvarindex(o_ccSS)>0.AND.clakeoutflowi2(i_ss,i2)/=missing_value) outvar(i2,outvarindex(o_ccSS)) = clakeoutflowi2(i_ss,i2)
            !  IF(outvarindex(o_ccAE)>0.AND.clakeoutflowi2(i_ae,i2)/=missing_value) outvar(i2,outvarindex(o_ccAE)) = clakeoutflowi2(i_ae,i2)
            !  IF(outvarindex(o_ccTS)>0.AND.clakeoutflowi2(i_ss,i2)/=missing_value.AND.clakeoutflowi2(i_ae,i2)/=missing_value) outvar(i2,outvarindex(o_ccTS)) = clakeoutflowi2(i_ss,i2) + clakeoutflowi2(i_ae,i2)*dryNratio
            !ENDIF
            !Output concentration of flow in main and branch
            IF(mainflowi2(i2)/=0.)THEN
              !If o_c1IN etc in prescribed order, subroutine to set all outvar could be written. Difficult when new substances introduced!
              IF(simulate%substance(i_in).AND.outvarindex(o_c1IN)>0) outvar(i2,outvarindex(o_c1IN)) = cmainflowi2(i_in,i2)*1000.
              IF(simulate%substance(i_on).AND.outvarindex(o_c1ON)>0) outvar(i2,outvarindex(o_c1ON)) = cmainflowi2(i_on,i2)*1000.
              IF(simulate%substance(i_sp).AND.outvarindex(o_c1SP)>0) outvar(i2,outvarindex(o_c1SP)) = cmainflowi2(i_sp,i2)*1000.
              IF(simulate%substance(i_pp).AND.outvarindex(o_c1PP)>0) outvar(i2,outvarindex(o_c1PP)) = cmainflowi2(i_pp,i2)*1000.
              IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c1TN)>0) outvar(i2,outvarindex(o_c1TN)) = (cmainflowi2(i_in,i2) + cmainflowi2(i_on,i2))*1000.
              IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c1TP)>0) outvar(i2,outvarindex(o_c1TP)) = (cmainflowi2(i_sp,i2) + cmainflowi2(i_pp,i2))*1000.
              IF(simulate%substance(i_oc).AND.outvarindex(o_c1OC)>0) outvar(i2,outvarindex(o_c1OC)) = cmainflowi2(i_oc,i2)
              IF(simulate%substance(i_t1).AND.outvarindex(o_c1T1)>0) outvar(i2,outvarindex(o_c1T1)) = cmainflowi2(i_t1,i2)
              IF(simulate%substance(i_t2).AND.outvarindex(o_c1T2)>0) outvar(i2,outvarindex(o_c1T2)) = cmainflowi2(i_t2,i2)
              IF(simulate%substance(i_ss))THEN
                IF(outvarindex(o_c1SS)>0) outvar(i2,outvarindex(o_c1SS)) = cmainflowi2(i_ss,i2)
                IF(outvarindex(o_c1AE)>0) outvar(i2,outvarindex(o_c1AE)) = cmainflowi2(i_ae,i2)
                IF(outvarindex(o_c1TS)>0) outvar(i2,outvarindex(o_c1TS)) = cmainflowi2(i_ss,i2) + cmainflowi2(i_ae,i2)*dryNratio
                IF(outvarindex(o_c3SS)>0) outvar(i2,outvarindex(o_c3SS)) = cmainflowi2(i_ss,i2)
                IF(outvarindex(o_c3AE)>0) outvar(i2,outvarindex(o_c3AE)) = cmainflowi2(i_ae,i2)
                IF(outvarindex(o_c3TS)>0) outvar(i2,outvarindex(o_c3TS)) = cmainflowi2(i_ss,i2) + cmainflowi2(i_ae,i2)*dryNratio
              ENDIF
              IF(simulate%substance(i_in).AND.outvarindex(o_c3IN)>0) outvar(i2,outvarindex(o_c3IN)) = cmainflowi2(i_in,i2)*1000.
              IF(simulate%substance(i_on).AND.outvarindex(o_c3ON)>0) outvar(i2,outvarindex(o_c3ON)) = cmainflowi2(i_on,i2)*1000.
              IF(simulate%substance(i_sp).AND.outvarindex(o_c3SP)>0) outvar(i2,outvarindex(o_c3SP)) = cmainflowi2(i_sp,i2)*1000.
              IF(simulate%substance(i_pp).AND.outvarindex(o_c3PP)>0) outvar(i2,outvarindex(o_c3PP)) = cmainflowi2(i_pp,i2)*1000.
              IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c3TN)>0) outvar(i2,outvarindex(o_c3TN)) = (cmainflowi2(i_in,i2) + cmainflowi2(i_on,i2))*1000.
              IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c3TP)>0) outvar(i2,outvarindex(o_c3TP)) = (cmainflowi2(i_sp,i2) + cmainflowi2(i_pp,i2))*1000.
              IF(simulate%substance(i_oc).AND.outvarindex(o_c3OC)>0) outvar(i2,outvarindex(o_c3OC)) = cmainflowi2(i_oc,i2)
              IF(simulate%substance(i_t1).AND.outvarindex(o_c3T1)>0) outvar(i2,outvarindex(o_c3T1)) = cmainflowi2(i_t1,i2)
              IF(simulate%substance(i_t2).AND.outvarindex(o_c3T2)>0) outvar(i2,outvarindex(o_c3T2)) = cmainflowi2(i_t2,i2)
            !ELSE  !mainflow=0, conc will be set to missing later anyway
            !  IF(simulate%substance(i_in).AND.outvarindex(o_c1IN)>0) outvar(i2,outvarindex(o_c1IN)) = 0.
            !  IF(simulate%substance(i_on).AND.outvarindex(o_c1ON)>0) outvar(i2,outvarindex(o_c1ON)) = 0.
            !  IF(simulate%substance(i_sp).AND.outvarindex(o_c1SP)>0) outvar(i2,outvarindex(o_c1SP)) = 0.
            !  IF(simulate%substance(i_pp).AND.outvarindex(o_c1PP)>0) outvar(i2,outvarindex(o_c1PP)) = 0.
            !  IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c1TN)>0) outvar(i2,outvarindex(o_c1TN)) = 0.
            !  IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c1TP)>0) outvar(i2,outvarindex(o_c1TP)) = 0.
            !  IF(simulate%substance(i_oc).AND.outvarindex(o_c1OC)>0) outvar(i2,outvarindex(o_c1OC)) = 0.
            !  IF(simulate%substance(i_t1).AND.outvarindex(o_c1T1)>0) outvar(i2,outvarindex(o_c1T1)) = 0.
            !  IF(simulate%substance(i_t2).AND.outvarindex(o_c1T2)>0) outvar(i2,outvarindex(o_c1T2)) = 0.
            !  IF(simulate%substance(i_ss))THEN
            !    IF(outvarindex(o_c1SS)>0) outvar(i2,outvarindex(o_c1SS)) = 0.
            !    IF(outvarindex(o_c1AE)>0) outvar(i2,outvarindex(o_c1AE)) = 0.
            !    IF(outvarindex(o_c1TS)>0) outvar(i2,outvarindex(o_c1TS)) = 0.
            !    IF(outvarindex(o_c3SS)>0) outvar(i2,outvarindex(o_c3SS)) = 0.
            !    IF(outvarindex(o_c3AE)>0) outvar(i2,outvarindex(o_c3AE)) = 0.
            !    IF(outvarindex(o_c3TS)>0) outvar(i2,outvarindex(o_c3TS)) = 0.
            !  ENDIF
            !  IF(simulate%substance(i_in).AND.outvarindex(o_c3IN)>0) outvar(i2,outvarindex(o_c3IN)) = 0.
            !  IF(simulate%substance(i_on).AND.outvarindex(o_c3ON)>0) outvar(i2,outvarindex(o_c3ON)) = 0.
            !  IF(simulate%substance(i_sp).AND.outvarindex(o_c3SP)>0) outvar(i2,outvarindex(o_c3SP)) = 0.
            !  IF(simulate%substance(i_pp).AND.outvarindex(o_c3PP)>0) outvar(i2,outvarindex(o_c3PP)) = 0.
            !  IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c3TN)>0) outvar(i2,outvarindex(o_c3TN)) = 0.
            !  IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c3TP)>0) outvar(i2,outvarindex(o_c3TP)) = 0.
            !  IF(simulate%substance(i_oc).AND.outvarindex(o_c3OC)>0) outvar(i2,outvarindex(o_c3OC)) = 0.
            !  IF(simulate%substance(i_t1).AND.outvarindex(o_c3T1)>0) outvar(i2,outvarindex(o_c3T1)) = 0.
            !  IF(simulate%substance(i_t2).AND.outvarindex(o_c3T2)>0) outvar(i2,outvarindex(o_c3T2)) = 0.
            ENDIF
            IF(branchflowi2(i2)/=0.)THEN
              IF(simulate%substance(i_in).AND.outvarindex(o_c2IN)>0) outvar(i2,outvarindex(o_c2IN)) = cbranchflowi2(i_in,i2)*1000.
              IF(simulate%substance(i_on).AND.outvarindex(o_c2ON)>0) outvar(i2,outvarindex(o_c2ON)) = cbranchflowi2(i_on,i2)*1000.
              IF(simulate%substance(i_sp).AND.outvarindex(o_c2SP)>0) outvar(i2,outvarindex(o_c2SP)) = cbranchflowi2(i_sp,i2)*1000.
              IF(simulate%substance(i_pp).AND.outvarindex(o_c2PP)>0) outvar(i2,outvarindex(o_c2PP)) = cbranchflowi2(i_pp,i2)*1000.
              IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c2TN)>0) outvar(i2,outvarindex(o_c2TN)) = (cbranchflowi2(i_in,i2) + cbranchflowi2(i_on,i2))*1000.
              IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c2TP)>0) outvar(i2,outvarindex(o_c2TP)) = (cbranchflowi2(i_sp,i2) + cbranchflowi2(i_pp,i2))*1000.
              IF(simulate%substance(i_oc).AND.outvarindex(o_c2OC)>0) outvar(i2,outvarindex(o_c2OC)) = cbranchflowi2(i_oc,i2)
              IF(simulate%substance(i_t1).AND.outvarindex(o_c2T1)>0) outvar(i2,outvarindex(o_c2T1)) = cbranchflowi2(i_t1,i2)
              IF(simulate%substance(i_t2).AND.outvarindex(o_c2T2)>0) outvar(i2,outvarindex(o_c2T2)) = cbranchflowi2(i_t2,i2)
              IF(simulate%substance(i_ss))THEN
                IF(outvarindex(o_c2SS)>0) outvar(i2,outvarindex(o_c2SS)) = cbranchflowi2(i_ss,i2)
                IF(outvarindex(o_c2AE)>0) outvar(i2,outvarindex(o_c2AE)) = cbranchflowi2(i_ae,i2)
                IF(outvarindex(o_c2TS)>0) outvar(i2,outvarindex(o_c2TS)) = cbranchflowi2(i_ss,i2) + cbranchflowi2(i_ae,i2)*dryNratio
                IF(outvarindex(o_c4SS)>0) outvar(i2,outvarindex(o_c4SS)) = cbranchflowi2(i_ss,i2)
                IF(outvarindex(o_c4AE)>0) outvar(i2,outvarindex(o_c4AE)) = cbranchflowi2(i_ae,i2)
                IF(outvarindex(o_c4TS)>0) outvar(i2,outvarindex(o_c4TS)) = cbranchflowi2(i_ss,i2) + cbranchflowi2(i_ae,i2)*dryNratio
              ENDIF
              IF(simulate%substance(i_in).AND.outvarindex(o_c4IN)>0) outvar(i2,outvarindex(o_c4IN)) = cbranchflowi2(i_in,i2)*1000.
              IF(simulate%substance(i_on).AND.outvarindex(o_c4ON)>0) outvar(i2,outvarindex(o_c4ON)) = cbranchflowi2(i_on,i2)*1000.
              IF(simulate%substance(i_sp).AND.outvarindex(o_c4SP)>0) outvar(i2,outvarindex(o_c4SP)) = cbranchflowi2(i_sp,i2)*1000.
              IF(simulate%substance(i_pp).AND.outvarindex(o_c4PP)>0) outvar(i2,outvarindex(o_c4PP)) = cbranchflowi2(i_pp,i2)*1000.
              IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c4TN)>0) outvar(i2,outvarindex(o_c4TN)) = (cbranchflowi2(i_in,i2) + cbranchflowi2(i_on,i2))*1000.
              IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c4TP)>0) outvar(i2,outvarindex(o_c4TP)) = (cbranchflowi2(i_sp,i2) + cbranchflowi2(i_pp,i2))*1000.
              IF(simulate%substance(i_oc).AND.outvarindex(o_c4OC)>0) outvar(i2,outvarindex(o_c4OC)) = cbranchflowi2(i_oc,i2)
              IF(simulate%substance(i_t1).AND.outvarindex(o_c4T1)>0) outvar(i2,outvarindex(o_c4T1)) = cbranchflowi2(i_t1,i2)
              IF(simulate%substance(i_t2).AND.outvarindex(o_c4T2)>0) outvar(i2,outvarindex(o_c4T2)) = cbranchflowi2(i_t2,i2)
            !ELSE  !branchflow=0
            !  IF(simulate%substance(i_in).AND.outvarindex(o_c2IN)>0) outvar(i2,outvarindex(o_c2IN)) = 0.
            !  IF(simulate%substance(i_on).AND.outvarindex(o_c2ON)>0) outvar(i2,outvarindex(o_c2ON)) = 0.
            !  IF(simulate%substance(i_sp).AND.outvarindex(o_c2SP)>0) outvar(i2,outvarindex(o_c2SP)) = 0.
            !  IF(simulate%substance(i_pp).AND.outvarindex(o_c2PP)>0) outvar(i2,outvarindex(o_c2PP)) = 0.
            !  IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c2TN)>0) outvar(i2,outvarindex(o_c2TN)) = 0.
            !  IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c2TP)>0) outvar(i2,outvarindex(o_c2TP)) = 0.
            !  IF(simulate%substance(i_oc).AND.outvarindex(o_c2OC)>0) outvar(i2,outvarindex(o_c2OC)) = 0.
            !  IF(simulate%substance(i_t1).AND.outvarindex(o_c2T1)>0) outvar(i2,outvarindex(o_c2T1)) = 0.
            !  IF(simulate%substance(i_t2).AND.outvarindex(o_c2T2)>0) outvar(i2,outvarindex(o_c2T2)) = 0.
            !  IF(simulate%substance(i_ss))THEN
            !    IF(outvarindex(o_c2SS)>0) outvar(i2,outvarindex(o_c2SS)) = 0.
            !    IF(outvarindex(o_c2AE)>0) outvar(i2,outvarindex(o_c2AE)) = 0.
            !    IF(outvarindex(o_c2TS)>0) outvar(i2,outvarindex(o_c2TS)) = 0.
            !    IF(outvarindex(o_c4SS)>0) outvar(i2,outvarindex(o_c4SS)) = 0.
            !    IF(outvarindex(o_c4AE)>0) outvar(i2,outvarindex(o_c4AE)) = 0.
            !    IF(outvarindex(o_c4TS)>0) outvar(i2,outvarindex(o_c4TS)) = 0.
            !  ENDIF
            !  IF(simulate%substance(i_in).AND.outvarindex(o_c4IN)>0) outvar(i2,outvarindex(o_c4IN)) = 0.
            !  IF(simulate%substance(i_on).AND.outvarindex(o_c4ON)>0) outvar(i2,outvarindex(o_c4ON)) = 0.
            !  IF(simulate%substance(i_sp).AND.outvarindex(o_c4SP)>0) outvar(i2,outvarindex(o_c4SP)) = 0.
            !  IF(simulate%substance(i_pp).AND.outvarindex(o_c4PP)>0) outvar(i2,outvarindex(o_c4PP)) = 0.
            !  IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c4TN)>0) outvar(i2,outvarindex(o_c4TN)) = 0.
            !  IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c4TP)>0) outvar(i2,outvarindex(o_c4TP)) = 0.
            !  IF(simulate%substance(i_oc).AND.outvarindex(o_c4OC)>0) outvar(i2,outvarindex(o_c4OC)) = 0.
            !  IF(simulate%substance(i_t1).AND.outvarindex(o_c4T1)>0) outvar(i2,outvarindex(o_c4T1)) = 0.
            !  IF(simulate%substance(i_t2).AND.outvarindex(o_c4T2)>0) outvar(i2,outvarindex(o_c4T2)) = 0.
            ENDIF

            !Output of total lake outflow
            IF(i2==i)THEN
              IF(outvarindex(o_coul)>0) outvar(i2,outvarindex(o_coul)) = totaloutflow     !computed outflow whole multi-basin lake (m3/s)
              IF(simulate%substance(i_in).AND.outvarindex(o_c7IN)>0) outvar(i,outvarindex(o_c7IN)) = ctotaloutflow(i_in)*1000.
              IF(simulate%substance(i_on).AND.outvarindex(o_c7ON)>0) outvar(i,outvarindex(o_c7ON)) = ctotaloutflow(i_on)*1000.
              IF(simulate%substance(i_sp).AND.outvarindex(o_c7SP)>0) outvar(i,outvarindex(o_c7SP)) = ctotaloutflow(i_sp)*1000.
              IF(simulate%substance(i_pp).AND.outvarindex(o_c7PP)>0) outvar(i,outvarindex(o_c7PP)) = ctotaloutflow(i_pp)*1000.
              IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c7TN)>0) outvar(i,outvarindex(o_c7TN)) = (ctotaloutflow(i_in) + ctotaloutflow(i_on))*1000.
              IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c7TP)>0) outvar(i,outvarindex(o_c7TP)) = (ctotaloutflow(i_sp) + ctotaloutflow(i_pp))*1000.
              IF(simulate%substance(i_oc).AND.outvarindex(o_c7OC)>0) outvar(i,outvarindex(o_c7OC)) = ctotaloutflow(i_oc)
              IF(simulate%substance(i_t1).AND.outvarindex(o_c7T1)>0) outvar(i,outvarindex(o_c7T1)) = ctotaloutflow(i_t1)
              IF(simulate%substance(i_t2).AND.outvarindex(o_c7T2)>0) outvar(i,outvarindex(o_c7T2)) = ctotaloutflow(i_t2)
              IF(simulate%substance(i_ss))THEN
                IF(outvarindex(o_c7SS)>0) outvar(i,outvarindex(o_c7SS)) = ctotaloutflow(i_ss)
                IF(outvarindex(o_c7AE)>0) outvar(i,outvarindex(o_c7AE)) = ctotaloutflow(i_ae)
                IF(outvarindex(o_c7TS)>0) outvar(i,outvarindex(o_c7TS)) = ctotaloutflow(i_ss) + ctotaloutflow(i_ae)*dryNratio
              ENDIF
              IF(simulate%substance(i_in).AND.outvarindex(o_c8IN)>0) outvar(i,outvarindex(o_c8IN)) = ctotaloutflow(i_in)*1000.
              IF(simulate%substance(i_on).AND.outvarindex(o_c8ON)>0) outvar(i,outvarindex(o_c8ON)) = ctotaloutflow(i_on)*1000.
              IF(simulate%substance(i_sp).AND.outvarindex(o_c8SP)>0) outvar(i,outvarindex(o_c8SP)) = ctotaloutflow(i_sp)*1000.
              IF(simulate%substance(i_pp).AND.outvarindex(o_c8PP)>0) outvar(i,outvarindex(o_c8PP)) = ctotaloutflow(i_pp)*1000.
              IF(i_in>0.AND.i_on>0.AND.outvarindex(o_c8TN)>0) outvar(i,outvarindex(o_c8TN)) = (ctotaloutflow(i_in) + ctotaloutflow(i_on))*1000.
              IF(i_sp>0.AND.i_pp>0.AND.outvarindex(o_c8TP)>0) outvar(i,outvarindex(o_c8TP)) = (ctotaloutflow(i_sp) + ctotaloutflow(i_pp))*1000.
              IF(simulate%substance(i_oc).AND.outvarindex(o_c8OC)>0) outvar(i,outvarindex(o_c8OC)) = ctotaloutflow(i_oc)
              IF(simulate%substance(i_t1).AND.outvarindex(o_c8T1)>0) outvar(i,outvarindex(o_c8T1)) = ctotaloutflow(i_t1)
              IF(simulate%substance(i_t2).AND.outvarindex(o_c8T2)>0) outvar(i,outvarindex(o_c8T2)) = ctotaloutflow(i_t2)
              IF(simulate%substance(i_ss))THEN
                IF(outvarindex(o_c8SS)>0) outvar(i,outvarindex(o_c8SS)) = ctotaloutflow(i_ss)
                IF(outvarindex(o_c8AE)>0) outvar(i,outvarindex(o_c8AE)) = ctotaloutflow(i_ae)
                IF(outvarindex(o_c8TS)>0) outvar(i,outvarindex(o_c8TS)) = ctotaloutflow(i_ss) + ctotaloutflow(i_ae)*dryNratio
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF

      !Set subbasin load output variables for possible printout for Source Apportionment program
      IF(conductload) THEN
        outvar_classload(1:nclass,1:2,:,i)  = Latmdep(1:nclass,1:2,:)
        outvar_classload(1:nclass,3:4,:,i)  = Lcultiv(1:nclass,1:2,:)
        outvar_classload(1:nclass,5,:,i)    = Lrurala(1:nclass,:)
        outvar_classload(1:nclass,6,:,i)    = Lgrwsoil(1:nclass,:)
        outvar_classload(1:nclass,7,:,i)    = Lirrsoil(1:nclass,:)
        outvar_classload(1:nclass,8,:,i)    = Lstream(1:nclass,:)
        outvar_basinload(:,1,i)             = Lruralb(:)
        outvar_basinload(:,2:4,i)           = Lpoints(:,1:3)     !Note: Load-files and SourceApp need to change if max_pstype is increased
        outvar_basinload(:,5,i)             = Lgrwmr(:)
        outvar_basinload(:,6,i)             = Ltransf(:)
        outvar_basinload(:,7,i)             = Lgrwol(:)
        outvar_basinload(:,nsourload+1:nsourload+npathway,i)  = Lpathway(:,1:npathway)    !These are not correct for lakebasins, will be replaced below
        outvar_basinload(:,nsourload+npathway+1,i)    = Lbranch(:)                !-"-
        IF(statuslastlb)THEN    !Replace load for lakebasin subbasins
          DO i2=1,i
            IF(looplakes(i2))THEN
              outvar_basinload(:,nsourload-1+npathway,i2)  = Lpathwayi2(:,18,i2)
              outvar_basinload(:,nsourload+npathway,i2)  = Lpathwayi2(:,19,i2)
              outvar_basinload(:,nsourload+1+npathway,i2)  = Lbranchi2(:,i2)
            ENDIF
          ENDDO
        ENDIF
      ENDIF

      !Set water balance flows and stores for print out (current subbasin)
      IF(conductwb)THEN
        IF(modeloption(p_deepgroundwater)==1) wbflows(w_rgrwtoos,i) = outofsystem
        IF(conduct%floodplain)THEN
          IF(floodindex(i)>0)THEN
            IF(flooding(floodindex(i))%fpfol>0.)THEN
              wbstores(w_lakeplain,i) = miscstate%floodwater(2,i)
            ENDIF
            IF(flooding(floodindex(i))%fpfmr>0.)THEN
              wbstores(w_riverplain,i) = miscstate%floodwater(1,i)
            ENDIF
          ENDIF
        ENDIF
        !wbstores(w_iriver,i) = rivervolume(1)
        wbstores(w_mriver,i) = rivervolume(2)
        IF(slc_iwet>0) wbstores(w_iwet,i) = wetland_wbstore(slc_iwet,i,frozenstate,soilstate)
        IF(slc_owet>0) wbstores(w_owet,i) = wetland_wbstore(slc_owet,i,frozenstate,soilstate)
      ENDIF

      !OBSERVATION OPERATORS
      !Additional outvars for data assimilation giving 'model equivalents' for various observations (some are only calculated as regional outvars)

      !CLRF=local runoff to the main river (mm/tstep) = cloc[m3/s] normalized by local basin area excluding olake and main river area
      IF(outvarindex(o_clrf)>0)THEN
        a = 1.                                                             !fraction of basin area used for normalization, initially set to 1.
        IF(slc_olake>0) a = a - classbasin(i,slc_olake)%part               !remove olake fraction
        IF(slc_mriver>0) a = a - classbasin(i,slc_mriver)%part             !remove main river fraction
        IF(a.GT.0)THEN
          outvar(i,outvarindex(o_clrf)) = 1000.*(basinoutflow(1)*seconds_per_timestep)/(a * basin(i)%area) !transform m3/s to mm/tstep
        ELSE
          outvar(i,outvarindex(o_clrf)) = 0. !if there is no land, ilake, or local river in this basin, then there is no local runoff either
        ENDIF
      ENDIF

      !CLRP and CLRE
      IF(outvarindex(o_clrp)>0)outvar(i,outvarindex(o_clrp))=prec_water/area_water
      IF(outvarindex(o_clre)>0)outvar(i,outvarindex(o_clre))=evap_water/area_water

    !>End main subbasin-loop
    ENDDO subbasinloop

    IF(outvarindex(122)>0) outvar(:,outvarindex(122)) = irrevap     !Irrigation losses (m3)

    !Calculate lake volume and set output variables
    basinlakevol = missing_value
    CALL calculate_lake_volume_output(nsub,lakestate,basinlakevol,ilakevol,olakevol,ilakevolmiss,olakevolmiss)
    IF(outvarindex(144)>0) outvar(:,outvarindex(144)) = basinlakevol  !Mm3
    IF(outvarindex(145)>0) outvar(:,outvarindex(145)) = ilakevolmiss  !Mm3
    IF(outvarindex(146)>0) outvar(:,outvarindex(146)) = olakevolmiss  !Mm3
    IF(conductwb)THEN
      wbstores(w_ilake,:)  = ilakevol  !m3
      wbstores(w_olake,:)  = olakevol  !m3
    ENDIF

    !Calculate aquifer delay and outflow
    IF(modeloption(p_deepgroundwater)==2)THEN
      CALL calculate_aquifer(aquiferstate,aquiferoutflow,aqremflow,aqirrloss)
      IF(outvarindex(240)>0) outvar(:,outvarindex(240)) = aqremflow
    ENDIF
    IF(naquifers>0)THEN
      IF(outvarindex(242)>0) outvar(:,outvarindex(242)) = calculate_aquifer_waterlevel(nsub,naquifers,aquiferstate)
      IF(simulate%substance(i_in).AND.outvarindex(501)>0)THEN
        outvar(:,outvarindex(501)) = calculate_aquifer_conc(nsub,naquifers,i_in,aquiferstate)
        WHERE(outvar(:,outvarindex(501))/=missing_value) outvar(:,outvarindex(501)) = outvar(:,outvarindex(501))*1.E3  !ug/L
      ENDIF
      IF(simulate%substance(i_on).AND.outvarindex(502)>0)THEN
        outvar(:,outvarindex(502)) = calculate_aquifer_conc(nsub,naquifers,i_on,aquiferstate)
        WHERE(outvar(:,outvarindex(502))/=missing_value) outvar(:,outvarindex(502)) = outvar(:,outvarindex(502))*1.E3   !ug/L
      ENDIF
      IF(simulate%substance(i_sp).AND.outvarindex(503)>0)THEN
        outvar(:,outvarindex(503)) = calculate_aquifer_conc(nsub,naquifers,i_sp,aquiferstate)
        WHERE(outvar(:,outvarindex(503))/=missing_value) outvar(:,outvarindex(503)) = outvar(:,outvarindex(503))*1.E3  !ug/L
      ENDIF
      IF(simulate%substance(i_pp).AND.outvarindex(504)>0)THEN
        outvar(:,outvarindex(504)) = calculate_aquifer_conc(nsub,naquifers,i_pp,aquiferstate)
        WHERE(outvar(:,outvarindex(504))/=missing_value) outvar(:,outvarindex(504)) = outvar(:,outvarindex(504))*1.E3  !ug/L
      ENDIF
      IF(simulate%substance(i_ss).AND.outvarindex(505)>0) outvar(:,outvarindex(505)) = calculate_aquifer_conc(nsub,naquifers,i_ss,aquiferstate)
      IF(simulate%substance(i_t1).AND.outvarindex(506)>0) outvar(:,outvarindex(506)) = calculate_aquifer_conc(nsub,naquifers,i_t1,aquiferstate)
      IF(simulate%substance(i_oc).AND.outvarindex(507)>0) outvar(:,outvarindex(507)) = calculate_aquifer_conc(nsub,naquifers,i_oc,aquiferstate)
    ENDIF

    !Set water balance flows and stores for print out (all subbasins)
    IF(conductwb)THEN
      wbflows(w_rgrwto1,:)  = horizontalflows2(1,:)     !regional groundwater flow from this subbasin's groundwater reservoir
      wbflows(w_rgrwto2,:)  = horizontalflows2(2,:)
      wbflows(w_rgrwto3,:)  = horizontalflows2(3,:)
      wbirrflows(w_wdregol,:)  = regionalirrflows(1,:)  !regional sources to this subbasin
      wbirrflows(w_wdregmr,:)  = regionalirrflows(2,:)  !regional sources to this subbasin
      wbirrflows(w_evapregol,:) = regionalirrflows(3,:)
      wbirrflows(w_evapregmr,:) = regionalirrflows(4,:)
      IF(conduct%irrigation)THEN
        wbstores(w_irrcanal,:) = 0.
        DO i = 1,nsub
          DO j = 1,nclass
            wbstores(w_irrcanal,i) = wbstores(w_irrcanal,i) + miscstate%nextirrigation(j,i)*classbasin(i,j)%part
          ENDDO
        ENDDO
        wbstores(w_irrcanal,:) = wbstores(w_irrcanal,:) * basin(:)%area *1.E-3  !m3
      ENDIF
      IF(conduct%watertransfer) wbstores(w_wtcanal,:) = miscstate%nexttransfer(:) *seconds_per_timestep !m3
      IF(naquifers>0)THEN
        CALL calculate_delayed_water(aquiferstate,naquifers,delayedwater)
        wbstores(w_aquifer,1:naquifers) = aquiferstate%water + aquiferstate%nextoutflow + delayedwater
      ENDIF
    ENDIF

    !Print out water balance flow
    IF(conductwb) CALL print_waterbalance_timestep(nsub,naquifers,currentdate)

  END SUBROUTINE model

  !>Reads files with model specific input
  !>For HYPE it is files with different observations (xom0..xom9,xos0..xos9)
  !----------------------------------------------------------------------
  SUBROUTINE load_modeldefined_input(dir,dir2,nsmax,ns,indexarray,bdate,edate,loadxoms,loadregs,status)

    USE HYPE_INDATA, ONLY : load_xoms_files,  &
                            load_data_for_regression_parameter_estimate
    USE LibDate, ONLY : DateType

    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: dir   !<File directory (modeldir)
    CHARACTER(LEN=*), INTENT(IN) :: dir2  !<File directory (forcingdir)
    INTEGER, INTENT(IN)  :: nsmax         !<Number of subbasins, basemodel
    INTEGER, INTENT(IN)  :: ns            !<Number of subbasins, submodel
    INTEGER, INTENT(IN) :: indexarray(ns) !<index for basemodel
    TYPE(DateType), INTENT(IN)  :: bdate  !<Begin simulation date
    TYPE(DateType), INTENT(IN)  :: edate  !<End simulation date
    LOGICAL, INTENT(IN) :: loadxoms       !<flag for using xoms-files
    LOGICAL, INTENT(IN) :: loadregs       !<flag for using regression estimation-files
    INTEGER, INTENT(OUT) :: status        !<Status of subroutine

    status = 0
    IF(loadxoms) CALL load_xoms_files(dir2,ns,bdate,edate,status)
    IF(status/=0) RETURN
    IF(loadregs) CALL load_data_for_regression_parameter_estimate(dir,nsmax,ns,indexarray,status)
    IF(status/=0) RETURN

  END SUBROUTINE load_modeldefined_input

  !>Reads files with model specific input
  !>For HYPE it is files with different observations (xom0..xom9,xos0..xos9)
  !----------------------------------------------------------------------
  SUBROUTINE reload_modeldefined_observations(dir,status)

    USE HYPE_INDATA, ONLY : close_xoms_files, &
                            reload_xoms_files

    !Argument declaration
    CHARACTER(LEN=*), INTENT(IN) :: dir   !<File directory (forcingdir)
    INTEGER, INTENT(OUT) :: status        !<Status of subroutine

    status = 0
    IF(conductxoms) CALL close_xoms_files()
    IF(conductxoms) CALL reload_xoms_files(dir,status)
    IF(status/=0) RETURN

  END SUBROUTINE reload_modeldefined_observations

  !>Opens files for printout
  !--------------------------------------------------------------------
  SUBROUTINE open_modeldefined_outputfiles(dir,n,na)

    USE HYPE_WATERBALANCE, ONLY : prepare_waterbalance_files

    !Argument declarations
    CHARACTER(LEN=*), INTENT(IN) :: dir !<Result file directory
    INTEGER, INTENT(IN) :: n            !<Number of subbasins
    INTEGER, INTENT(IN) :: na           !<Number of aquifers

    IF(conductwb) CALL prepare_waterbalance_files(dir,n,na,basin%subid)  !These cannot be printed for several emsembles.

  END SUBROUTINE open_modeldefined_outputfiles

  !>Close files for printout
  !--------------------------------------------------------------------
  SUBROUTINE close_modeldefined_outputfiles(na)

    USE HYPE_WATERBALANCE, ONLY : close_waterbalance_files

    !Argument declarations
    INTEGER, INTENT(IN) :: na           !<Number of aquifers

    IF(conductwb) CALL close_waterbalance_files(na)

  END SUBROUTINE close_modeldefined_outputfiles

  !Private subroutines
  !----------------------------------------------------------------

  !>Calculate output variables for lake volumes
  !--------------------------------------------------------------------
  SUBROUTINE calculate_lake_volume_output(n,lakestate,wlakevol,ilakevol,olakevol,Milakevol,Molakevol)

    USE STATETYPE_MODULE, ONLY : lakestatetype
    USE SURFACEWATER_PROCESSES, ONLY : calculate_lake_volume

    !Argument declarations
    INTEGER, INTENT(IN) :: n            !<number of subbasins
    TYPE(lakestatetype),INTENT(IN) :: lakestate  !<Lake state, submodel
    REAL, INTENT(INOUT) :: wlakevol(n)  !<volume of olake/volume for lake with basins in outlet basin (Mm3)
    REAL, INTENT(OUT)   :: ilakevol(n)  !<volume of ilake (m3)
    REAL, INTENT(OUT)   :: olakevol(n)  !<volume of olake (m3)
    REAL, INTENT(OUT)   :: Milakevol(n) !<volume of ilake (Mm3), missing_value for no lake
    REAL, INTENT(OUT)   :: Molakevol(n) !<volume of olake (Mm3), missing_value for no lake

    !Variable declarations
    INTEGER i,j,itype
    REAL a,fpfrac
    REAL lakearea(nlaketypes)       !lake area (m2) for lakes
    REAL lakebasinvol(nlaketypes)   !volume of ilake & olake (m3)
    REAL lakevol                    !volume for single olake and lakes composed of lakebasins(sum)  (m3)
    REAL lakevolsum(n)    !accumulation of lake basin volumes to outlet during the i-loop (m3) (big enough)

    !Initialisation: wlakevol (outvar) is missing_value when reaching this subroutine already
    ilakevol = 0.
    olakevol = 0.
    Milakevol = missing_value
    Molakevol = missing_value
    lakevolsum = 0.

    !Calculate lake volumes for each subbasin
    DO i=1,n

      !Calculate lake area for ilake and olake (without floodplain area)
      lakearea = 0.
      DO itype = 1,nlaketypes
        a = 0.
        IF(itype==1 .AND. slc_ilake>0 .OR. itype==2 .AND. slc_olake>0)THEN
          IF(itype==1) j=slc_ilake
          IF(itype==2) j=slc_olake
          a = classbasin(i,j)%part
        ENDIF
        IF(a>0)THEN      !lake exist
          lakearea(itype) = a * basin(i)%area                  !m2
          IF(itype==2 .AND. conduct%floodplain)THEN
            IF(floodindex(i)>0)THEN
              fpfrac = flooding(floodindex(i))%fpfol          !floodplain fraction of outlet lake area
              IF(fpfrac>0.) lakearea(itype) = a * basin(i)%area * (1.-fpfrac)    !m2
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      !Calculate lake volume for print out
      lakebasinvol=0.
      lakevol=0.
      IF(slc_ilake>0. .AND. lakearea(1)>0.)THEN
        CALL calculate_lake_volume(1,i,n,lakearea(1),lakestate%water(1,i),lakebasinvol,lakevol,lakevolsum)
        Milakevol(i) = lakebasinvol(1)*1.E-6  !Mm3
      ENDIF
      IF(slc_olake>0 .AND. lakearea(2)>0.)THEN
        CALL calculate_lake_volume(2,i,n,lakearea(2),lakestate%water(2,i),lakebasinvol,lakevol,lakevolsum)
        IF(lakevol.NE.missing_value)THEN
          wlakevol(i) = lakevol/1000000. !whole lake volume for lakebasinlake (10^6 m3)
        ELSE
          wlakevol(i) = lakevol          !upstream lake basin is set to missing
        ENDIF
        Molakevol(i) = lakebasinvol(2)*1.E-6  !Mm3
      ENDIF

      !Additional output variables for lake volume (m3)
      ilakevol(i) = lakebasinvol(1)
      olakevol(i) = lakebasinvol(2)
    ENDDO

  END SUBROUTINE calculate_lake_volume_output


!To make calculated results available for HYSS for output and criteria
!calculations, the results are put into the outvar and outvarclassdata
!variables. These variable is then used by HYSS for further calculations
!and output.
!Output of class dependent variables are handled by accumulating the values
!for each class in the class-loop and then calculating the subbasin average
!after the class-loop. For these actions the following subroutines have been
!created.

  !>Initialize to zero the variables accumulating class values for calculating
  !>output of subbasin mean values and classgroup mean values.
  !>
  !>\b Consequences Module modvar variable outvar, outvarclassdata, and
  !>outvarclassfraction may change.
  !--------------------------------------------------------------------
  SUBROUTINE initialize_class_outvar_to_zero()

    USE MODVAR, ONLY : max_outvar, outvar, & !OUT
                       outvarindex, &
                       outvarclassdata, & !OUT
                       outvarclassfraction, & !OUT
                       outvarclassindex

    !Local variables
    INTEGER :: idindex  !index in outvarid

    DO idindex = 1,max_outvar
      IF(outvarid(idindex)%classtype==i_class)THEN
        IF(outvarindex(idindex)>0) outvar(:,outvarindex(idindex)) = 0.
        IF(outvarclassindex(idindex)>0)THEN
          outvarclassdata(:,:,outvarclassindex(idindex)) = 0.
          outvarclassfraction(:,:,outvarclassindex(idindex)) = 0.
        ENDIF
      ENDIF
    ENDDO

  END SUBROUTINE initialize_class_outvar_to_zero

  !>Initialize to zero the variables accumulating class values for calculating
  !>output of subbasin mean values and classgroup mean values
  !>for a specific variable and subbasin.
  !>
  !>\b Consequences Module modvar variable outvar, outvarclassdata, and
  !>outvarclassfraction may change.
  !--------------------------------------------------------------------
  SUBROUTINE calculate_class_outvar_initialize(idindex,isb)

    USE MODVAR, ONLY : outvar, & !OUT
                       outvarindex, &
                       outvarclassdata, & !OUT
                       outvarclassfraction, & !OUT
                       outvarclassindex

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin

    IF(outvarindex(idindex)>0) outvar(isb,outvarindex(idindex))=0.
    IF(outvarclassindex(idindex)>0)THEN
      outvarclassdata(:,isb,outvarclassindex(idindex)) = 0.
      outvarclassfraction(:,isb,outvarclassindex(idindex)) = 0.
    ENDIF

  END SUBROUTINE calculate_class_outvar_initialize

  !>Add class value to the variables accumulating these for calculating
  !>output of subbasin mean values and classgroup mean values.
  !>Add to outvar-array for subbasin mean and to outvarclassdata/fraction
  !>to later calculate classgroup mean value as classgroup area fraction
  !>average or to output class value.
  !>
  !>\b Consequences Module modvar variable outvar, outvarclassdata, and
  !>outvarclassfraction may change.
  !--------------------------------------------------------------------
  SUBROUTINE calculate_class_outvar_add(idindex,isb,jcl,fraction,value)

    USE MODVAR, ONLY : outvar, & !OUT
                       outvarindex, &
                       outvarclassdata, & !OUT
                       outvarclassfraction, & !OUT
                       outvarclassindex

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    INTEGER, INTENT(IN) :: jcl      !<index of class
    REAL, INTENT(IN)    :: fraction !<area fraction of class
    REAL, INTENT(IN)    :: value    !<class value of output variable

    IF(outvarindex(idindex)>0) outvar(isb,outvarindex(idindex)) = &
               outvar(isb,outvarindex(idindex)) + value*fraction
    IF(outvarclassindex(idindex)>0)THEN
      outvarclassdata(jcl,isb,outvarclassindex(idindex)) = value
      outvarclassfraction(jcl,isb,outvarclassindex(idindex)) = fraction
    ENDIF

  END SUBROUTINE calculate_class_outvar_add

  !>Add class value to the variables accumulating these for calculating
  !>output of subbasin mean values and classgroup mean values.
  !>Add amount for class value to outvar-array for subbasin mean
  !>value of concentration, and concentration to calculate classgroup mean
  !>value of concentrations.
  !>
  !>\b Consequences Module modvar variable outvar, outvarclassdata, and
  !>outvarclassfraction may change.
  !--------------------------------------------------------------------
  SUBROUTINE calculate_class_outvar_add_amount(idindex,isb,jcl,fraction,value,value2)

    USE MODVAR, ONLY : outvar, & !OUT
                       outvarindex, &
                       outvarclassdata, & !OUT
                       outvarclassfraction, & !OUT
                       outvarclassindex

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    INTEGER, INTENT(IN) :: jcl      !<index of class
    REAL, INTENT(IN)    :: fraction !<area fraction of class
    REAL, INTENT(IN)    :: value    !<class value of output variable (amount)
    REAL, INTENT(IN)    :: value2   !<class value of output variable (concentration)

    IF(outvarindex(idindex)>0) outvar(isb,outvarindex(idindex)) = &
               outvar(isb,outvarindex(idindex)) + value*fraction
    IF(outvarclassindex(idindex)>0)THEN
      outvarclassdata(jcl,isb,outvarclassindex(idindex)) = value2
      outvarclassfraction(jcl,isb,outvarclassindex(idindex)) = fraction
    ENDIF

  END SUBROUTINE calculate_class_outvar_add_amount

  !>Add class value to the variables accumulating these for calculating
  !>output of subbasin accumulated values and classgroup accumulated values.
  !>Add class value to calculate subbasin accumulated value and classgroup
  !>accumulated value.
  !>
  !>\b Consequences Module modvar variable outvar, outvarclassdata, and
  !>outvarclassfraction may change.
  !--------------------------------------------------------------------
  SUBROUTINE calculate_class_outvar_add_accumulate(idindex,isb,jcl,fraction,value)

    USE MODVAR, ONLY : outvar, & !OUT
                       outvarindex, &
                       outvarclassdata, & !OUT
                       outvarclassfraction, & !OUT
                       outvarclassindex

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    INTEGER, INTENT(IN) :: jcl      !<index of class
    REAL, INTENT(IN)    :: fraction !<area fraction of class
    REAL, INTENT(IN)    :: value    !<class value of output variable

    IF(outvarindex(idindex)>0) outvar(isb,outvarindex(idindex)) = &
               outvar(isb,outvarindex(idindex)) + value*fraction
    IF(outvarclassindex(idindex)>0)THEN
      outvarclassdata(jcl,isb,outvarclassindex(idindex)) = value*fraction
      outvarclassfraction(jcl,isb,outvarclassindex(idindex)) = 0  !calculate classgroup sum (not average)
    ENDIF

  END SUBROUTINE calculate_class_outvar_add_accumulate

  !>Finalize the calculation of output of subbasin mean values.
  !>The variables accumulating class values will be turned into
  !>subbasin mean values by dividing by the total area fraction
  !>or, for concentration, by the sum of runoff.
  !>
  !>\b Consequences Module modvar variable outvar may change.
  !--------------------------------------------------------------------
  SUBROUTINE calculate_class_outvar_finish(idindex,isb,denominator)

    USE MODVAR, ONLY : outvar, & !OUT
                       outvarindex, &
                       missing_value

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    REAL, INTENT(IN)    :: denominator !<land area fraction (or other fraction) of subbasin area or total runoff

    IF(outvarindex(idindex)>0)THEN
      IF(denominator==0.)THEN
        outvar(isb,outvarindex(idindex)) = missing_value
        RETURN
      ENDIF
      outvar(isb,outvarindex(idindex)) = &
             outvar(isb,outvarindex(idindex)) / denominator
      !IF(fraction>0.)THEN         !This code give NaN for naked ilake??
      !  outvar(isb,outvarindex(idindex)) = &
      !         outvar(isb,outvarindex(idindex)) / fraction
      !ELSE
      !  outvar(isb,outvarindex(idindex)) = 0.
      !ENDIF
    ENDIF

  END SUBROUTINE calculate_class_outvar_finish

  !>Finalize the calculation of subbasin mean value and classgroup mean value
  !>by multiplying by factor.
  !>
  !>\b Consequences Module modvar variables outvar, outvarclassdata may change.
  !--------------------------------------------------------------------
  SUBROUTINE calculate_class_outvar_finish_scale(idindex,isb,factor)

    USE MODVAR, ONLY : outvar, & !OUT
                       outvarindex, &
                       outvarclassdata, & !OUT
                       outvarclassindex, &
                       missing_value

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    REAL, INTENT(IN)    :: factor   !<multipying factor

    IF(outvarindex(idindex)>0)THEN
      IF(outvar(isb,outvarindex(idindex))/=missing_value) &
        outvar(isb,outvarindex(idindex)) = &
               outvar(isb,outvarindex(idindex)) * factor
    ENDIF
    IF(outvarclassindex(idindex)>0) outvarclassdata(:,isb,outvarclassindex(idindex)) = &
               outvarclassdata(:,isb,outvarclassindex(idindex)) * factor

  END SUBROUTINE calculate_class_outvar_finish_scale

  !>Set the output of subbasin mean value from classe values to missing value
  !>in the cases when a variable is not calculated and could be miss used.
  !>
  !>\b Consequences Module modvar variable outvar may change
  !--------------------------------------------------------------------
  SUBROUTINE set_class_outvar_missing(idindex,isb)

    USE MODVAR, ONLY : outvar, & !OUT
                       outvarindex, &
                       missing_value

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin

    IF(outvarindex(idindex)>0) outvar(isb,outvarindex(idindex)) = missing_value

  END SUBROUTINE set_class_outvar_missing

  !>Calculate temperature of outflow from subbasin from river or lake outflow
  !>\b Consequences Module modvar variable outvar may change
  !--------------------------------------------------------------------
  SUBROUTINE calculate_outvar_watertemperature(idindex,isb,itype,lowerlimit,riverstate,lakestate)

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    INTEGER, INTENT(IN) :: itype    !<index of system (1=local, 2=main)
    REAL, INTENT(IN)    :: lowerlimit !<lower limit of temperature
    TYPE(riverstatetype),INTENT(IN) :: riverstate   !<River states
    TYPE(lakestatetype),INTENT(IN)  :: lakestate    !<Lake states

    IF(outvarindex(idindex)>0)THEN
      outvar(isb,outvarindex(idindex)) = riverstate%temp(itype,isb)
      IF(slc_olake>0)THEN
        IF(classbasin(isb,slc_olake)%part>0) outvar(isb,outvarindex(idindex)) = lakestate%temp(itype,isb)
      ENDIF
      IF(outvar(isb,outvarindex(idindex))<lowerlimit) outvar(isb,outvarindex(idindex)) = lowerlimit
    ENDIF

  END SUBROUTINE calculate_outvar_watertemperature

  !>Set output variable from Xobs
  !>\b Consequences Module modvar variable outvar may change
  !--------------------------------------------------------------------
  SUBROUTINE set_outvar_xobs(idindex,isb)

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid
    INTEGER, INTENT(IN) :: isb      !<index of subbasin

    IF(outvarindex(idindex)>0)THEN
      IF(xobsindex(idindex,isb)>0)THEN
        outvar(isb,outvarindex(idindex)) = xobsi(xobsindex(idindex,isb))
      ELSE
        outvar(isb,outvarindex(idindex)) = missing_value
      ENDIF
    ENDIF

  END SUBROUTINE set_outvar_xobs

  !>Set output variable from a scaled Xobs variable
  !>\b Consequences Module modvar variable outvar may change
  !--------------------------------------------------------------------
  SUBROUTINE set_outvar_xobs_scaled(idindex,isb,idxobs,factor)

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid for output variable
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    INTEGER, INTENT(IN) :: idxobs   !<index in outvarid for xobs variable
    REAL, INTENT(IN) :: factor      !<scale factor for output

    IF(outvarindex(idindex)>0)THEN
      outvar(isb,outvarindex(idindex)) = missing_value
      IF(xobsindex(idxobs,isb)>0)THEN
        IF(xobsi(xobsindex(idxobs,isb)).NE.missing_value) outvar(isb,outvarindex(idindex)) = xobsi(xobsindex(idxobs,isb))*factor
      ENDIF
    ENDIF

  END SUBROUTINE set_outvar_xobs_scaled

  !>Set output variable for a meantype Xobs/Xom variable
  !>\b Consequences Module modvar variable outvar may change
  !--------------------------------------------------------------------
  SUBROUTINE set_outvar_xobsmean(idindex,isb,idxobs)

    USE HYPE_INDATA, ONLY : num_xoms, xom

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid for output variable
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    INTEGER, INTENT(IN) :: idxobs   !<index in outvarid for xom variable

    IF(outvarindex(idindex)>0)THEN
      IF(xobsindex(idindex,isb)>0)THEN
        outvar(isb,outvarindex(idindex)) = xobsi(xobsindex(idindex,isb))
      ELSEIF(num_xoms(1)>=idxobs)THEN
        outvar(isb,outvarindex(idindex)) = xom(idxobs,isb)
      ELSE
        outvar(isb,outvarindex(idindex)) = missing_value
      ENDIF
    ENDIF

  END SUBROUTINE set_outvar_xobsmean

  !>Set output variable from a statetype Xobs/Xos variable
  !>\b Consequences Module modvar variable outvar may change
  !--------------------------------------------------------------------
  SUBROUTINE set_outvar_xobsstate(idindex,isb,idxobs)

    USE HYPE_INDATA, ONLY : num_xoms, xos

    INTEGER, INTENT(IN) :: idindex  !<index in outvarid for output variable
    INTEGER, INTENT(IN) :: isb      !<index of subbasin
    INTEGER, INTENT(IN) :: idxobs   !<index in outvarid for xos variable

     IF(outvarindex(idindex)>0)THEN
      IF(xobsindex(idindex,isb)>0)THEN
        outvar(isb,outvarindex(idindex)) = xobsi(xobsindex(idindex,isb))
      ELSEIF(num_xoms(2)>=idxobs)THEN
        outvar(isb,outvarindex(idindex)) = xos(idxobs,isb)
      ELSE
        outvar(isb,outvarindex(idindex)) = missing_value
      ENDIF
    ENDIF

  END SUBROUTINE set_outvar_xobsstate

  !!>Set the output of a subbasin substance value to missing value
  !!>in the cases when a variable is not calculated and could be miss used.
  !!>
  !!>\b Consequences Module modvar variable outvar may change
  !!--------------------------------------------------------------------
  !SUBROUTINE set_substance_outvar_missing(substanceindex,idindex,isb)
  !
  !  USE MODVAR, ONLY : missing_value, &
  !                     outvar, &       !INOUT
  !                     outvarindex, &
  !                     simulate
  !
  !
  !  INTEGER, INTENT(IN) :: substanceindex  !<index od substance
  !  INTEGER, INTENT(IN) :: idindex  !<index in outvarid
  !  INTEGER, INTENT(IN) :: isb      !<index of subbasin
  !
  !  IF(simulate%substance(substanceindex) .AND. outvarindex(idindex)>0)THEN
  !    outvar(isb,outvarindex(idindex)) = missing_value
  !  ENDIF
  !
  !END SUBROUTINE set_substance_outvar_missing
  !
  !!>Set the output of a subbasin substance value to zero
  !!>
  !!>\b Consequences Module modvar variable outvar may change
  !!--------------------------------------------------------------------
  !SUBROUTINE set_substance_outvar_zero(substanceindex,idindex,isb)
  !
  !  USE MODVAR, ONLY : missing_value, &
  !                     outvar, &       !INOUT
  !                     outvarindex, &
  !                     simulate
  !
  !
  !  INTEGER, INTENT(IN) :: substanceindex  !<index od substance
  !  INTEGER, INTENT(IN) :: idindex  !<index in outvarid
  !  INTEGER, INTENT(IN) :: isb      !<index of subbasin
  !
  !  IF(simulate%substance(substanceindex) .AND. outvarindex(idindex)>0)THEN
  !    outvar(isb,outvarindex(idindex)) = 0.
  !  ENDIF
  !
  !END SUBROUTINE set_substance_outvar_zero

  !>Calculate regional groundwater flow to outside model domain (for deepmodel 1)
  !--------------------------------------------------------------------
  SUBROUTINE calculate_regional_groundwaterflow_to_outside_system(isb,totflow,outflow)

    INTEGER, INTENT(IN) :: isb       !<index of subbasin
    REAL, INTENT(IN)    :: totflow   !<total flow of regional groundwater (m3/ts)
    REAL, INTENT(OUT)   :: outflow   !<flow to outside of model domain (m3/ts)

    outflow = 0.
    IF(modeloption(p_deepgroundwater)==1)THEN
      IF(path(isb)%grw1==0) outflow = totflow   !loss of water from the model system via groundwater flow
    ENDIF

  END SUBROUTINE calculate_regional_groundwaterflow_to_outside_system

  !>Set current parameter values for irrigation calculation
  !--------------------------------------------------------------------
  SUBROUTINE get_irrigation_parameters(isb,irrigationpar)

    USE MODVAR, ONLY : genpar,regpar, &
                       basin, &
                       regiondivision
    USE HYPEVARIABLES, ONLY : m_regirr, m_pirrs, m_pirrg, m_cirrsink, m_irrcomp

    !Argument declarations
    INTEGER, INTENT(IN) :: isb  !<index of subbasin
    REAL, INTENT(OUT) :: irrigationpar(5) !<parameter values used by irrigation

    irrigationpar = 0.
    irrigationpar(1) = genpar(m_regirr)
    IF(basin(isb)%parregion(regiondivision(m_pirrs))>0) irrigationpar(2) = regpar(m_pirrs,basin(isb)%parregion(regiondivision(m_pirrs)))
    IF(basin(isb)%parregion(regiondivision(m_pirrg))>0) irrigationpar(3) = regpar(m_pirrg,basin(isb)%parregion(regiondivision(m_pirrg)))
    IF(basin(isb)%parregion(regiondivision(m_cirrsink))>0) irrigationpar(4) = regpar(m_cirrsink,basin(isb)%parregion(regiondivision(m_cirrsink)))
    irrigationpar(5) = genpar(m_irrcomp)

  END SUBROUTINE get_irrigation_parameters

  !>Calculate current amount of water in wetland (iwet,owet) in m3
  !---------------------------------------------------------------
  REAL FUNCTION wetland_wbstore(j,i,frozenstate,soilstate)

    !Argument declarations
    INTEGER, INTENT(IN) :: j    !<soil landuse class
    INTEGER, INTENT(IN) :: i    !<subbasin
    TYPE(snowicestatetype),INTENT(IN) :: frozenstate !<Snow and ice states
    TYPE(soilstatetype),INTENT(IN)    :: soilstate   !<Soil states

    !Local variables
    REAL y

    IF(classbasin(i,j)%part>0.)THEN
      y = (frozenstate%snow(j,i) + &
           soilstate%water(1,j,i) + &
           soilstate%water(2,j,i) + &
           soilstate%water(3,j,i)) * &
          classbasin(i,j)%part * basin(i)%area *1.E-3  !m3
    ELSE
      y = 0.
    ENDIF
    wetland_wbstore = y
    RETURN

  END FUNCTION wetland_wbstore

  !>Calculate flow from lake that is not a lakebasin lake. This include effect of damming floodplains,
  !>updating by flow or water level observations, and setting miscellaneous outputs.
  !>
  !>\b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions, Lakes - Simple outlet lake or dam)
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_flow_from_undivided_lake(i,itype,qin,lakearea,& !IN
                                        flowreduction,hypodepth,olakewstold,&  !IN
                                        outflowsim,lakeoutflow,mainflow,branchflow,& !OUT
                                        concout,& !OUT
                                        wstlakesim,lakewst,wcomaver,&  !OUT
                                        Lpathway,lakestate,miscstate)  !INOUT

    USE MODVAR, ONLY : basin, &
                       conduct, &
                       conductload, &
                       doupdate, &
                       i_war,i_wendupd, &
                       i_qar,i_quseobs, &
                       p_floodplain, &
                       modeloption, &
                       numsubstances, &
                       seconds_per_timestep
    USE UPDATING, ONLY : apply_quseobs ,&
                         apply_warupd,&
                         apply_wendupd,&
                         apply_qarupd
    USE SURFACEWATER_PROCESSES, ONLY : calculate_outflow_from_outlet_lake,&
                                       remove_outflow_from_lake,        &
                                       recalculate_branched_flow

    !Arguments declarations
    INTEGER, INTENT(IN) :: i         !<index of current subbasin
    INTEGER, INTENT(IN) :: itype         !<lake type (internal or outlet) (only outlet allowed)
    REAL, INTENT(IN)    :: qin     !<net inflow to lake (m3/s)
    REAL, INTENT(IN)    :: lakearea !<area of lake (m2)
    REAL, INTENT(IN)    :: flowreduction !<downstream dammed flow on olake (m3/s)
    REAL, INTENT(IN)    :: hypodepth  !<lake hypolimnion depth (m)
    REAL, INTENT(IN)    :: olakewstold !<lake water stage, end of last timestep (mm)
    REAL, INTENT(OUT) :: outflowsim !<flow to downstream subbasins (m3/s)
    REAL, INTENT(OUT) :: lakeoutflow !<flow to downstream subbasins, after updating (m3/s)
    REAL, INTENT(OUT) :: mainflow   !<flow to main downstream subbasin, after updating (m3/s)
    REAL, INTENT(OUT) :: branchflow !<flow to branch downstream subbasin, after updating (m3/s)
    REAL, INTENT(OUT) :: concout(numsubstances) !<concentration of substances in flow to downstream subbasins
    REAL, INTENT(OUT) :: wstlakesim !<lake water stage (mm)
    REAL, INTENT(OUT) :: lakewst !<lake water stage, after updating (mm)
    REAL, INTENT(OUT) :: wcomaver !<average lake water stage over the time step (mm)
    REAL,ALLOCATABLE, INTENT(INOUT) :: Lpathway(:,:) !<output loads for sourceapportionment calculations
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake states
    TYPE(miscstatetype),INTENT(INOUT) :: miscstate  !<Miscstate states

    !Local variables
    REAL wst  !lake water (mm)
    REAL qunitfactor  !m3/s->mm/ts
    REAL outflowm3s, maxProd, minFlow
    REAL outflowmm  !mm/ts
    REAL wthresh    !outflow threshold (mm)

      !>\b Algorithm
      !>Initialisation
      mainflow = 0.
      branchflow = 0.
      wst=lakestate%water(itype,i)
      qunitfactor = seconds_per_timestep * 1000. / lakearea     !m3/s->mm/timestep

      !>Calculate outflow from lake according to rating curve or regulation
      CALL calculate_outflow_from_outlet_lake(i,qin,lakearea, &
                   wst,qunitfactor,outflowm3s,outflowmm, &
                   mainflow,branchflow,maxProd,minFlow)

      !>For floodplain model, the lake outflow can be dammed and thus reduced
      IF(conduct%floodplain.AND.modeloption(p_floodplain)==3)THEN
        IF(flowreduction<0.)THEN
          outflowmm = outflowmm + flowreduction/lakearea*1000.
          wthresh = basin(i)%lakedepth(itype)*1000.
          IF(outflowmm<0.)THEN
            outflowmm = 0.  !Catch negative outflow
          ELSEIF(wst>wthresh)THEN
            IF(outflowmm>wst-wthresh)THEN
              outflowmm = wst - wthresh
            ENDIF
            IF(outflowmm>wst) outflowmm = wst   !Safety
          ELSE
            outflowmm = 0.
          ENDIF
          outflowm3s = outflowmm/qunitfactor
          mainflow = outflowm3s
          !No lake with two outflow allowed here!
          !No basinlake allowed here (lakearea,threshold)
          !No production volume are allowed.
          !Add checks for this when reading
        ENDIF
      ENDIF

      !>Remove water from lake and calculate T2 of outflow
      CALL remove_outflow_from_lake(i,itype,numsubstances,outflowmm,basin(i)%subid,basin(i)%lakedepth(2),hypodepth,wst,concout,lakestate) !remove water (affect T2)
      IF(conductload) Lpathway(:,19) = outflowm3s * concout * seconds_per_timestep * 1.E-3  !Load at point R, outflow of subbasin (kg/timestep)

      !Lake water stage for print out
      lakewst = lakestate%water(itype,i)
      wstlakesim = lakewst          !saved for output
      wcomaver = (wstlakesim + olakewstold)/2.

      !>Update lake outflow and lake water volume with waterstage observations
      outflowsim = outflowm3s      !saved for qAR and output
      lakeoutflow = outflowm3s    !first value
      IF(doupdate(i_war))THEN !move into lake>0
        CALL apply_warupd(i,0,lakearea,olakewstold,lakewst,lakeoutflow,miscstate%updatestationsarcorr(i),wcomaver)
      ENDIF
      IF(doupdate(i_wendupd))THEN
        CALL apply_wendupd(i,lakewst,lakestate)
      ENDIF

      !>Update modelled subbasin outflow with flow observations or nutrient concentrations
      IF(doupdate(i_quseobs)) CALL apply_quseobs(i,lakeoutflow)
      IF(doupdate(i_qar))     CALL apply_qarupd(i,outflowsim,lakeoutflow,miscstate%updatestationsarcorr(i))
      CALL recalculate_branched_flow(i,lakeoutflow,maxProd,minFlow,mainflow,branchflow)

    END SUBROUTINE calculate_flow_from_undivided_lake

  !>Calculate flow withing and out from each lakebasin of a lakebasin lake of
  !>the new type with equal water level. Subroutine called for "last" lakebasin.
  !>
  !>\b Consequences Module hype_waterbalance variable wbflows may be set.
  !>
  !>\b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions, Lakes - Outlet lake as lake basin a part of a multi-basin lake with equal water level)
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_flow_for_lakebasin_lake(ilast,itype,looplakes,& !IN
                                                qini2,lakeareai2,hypodepthi2,olakewstoldi2,&  !IN
                                                outflowsimi2,lakeoutflowi2,mainflowi2,branchflowi2,& !INOUT
                                                accinflow,acccinflow,clakeoutflowi2,cmainflowi2,cbranchflowi2,qcinflii2,netroutload,& !INOUT
                                                wstlakesimi2,lakewsti2,wcomaveri2,Lpathwayi2,&  !INOUT
                                                Lbranchi2,totaloutflow,ctotaloutflow,&  !OUT
                                                lakestate,miscstate)  !INOUT

    USE MODVAR, ONLY : basin,           &
                       branchdata,      &
                       branchindex,     &
                       classbasin,      &
                       conductload, &
                       conductwb, &
                       doupdate, &
                       i_sp,i_pp,i_in,i_on, &
                       i_war,i_wendupd, &
                       i_qar,i_quseobs, &
                       i_tpcorr,i_tncorr, &
                       path,      &
                       nsub,            &
                       numsubstances,   &
                       seconds_per_timestep,      &
                       simulatesubstances, &
                       slc_olake, &
                       check_connected_subbasins
    USE GENERAL_WATER_CONCENTRATION, ONLY : remove_water,       &
                                            error_remove_water, &
                                            add_water
    USE HYPE_WATERBALANCE, ONLY : w_oltobol, &
                                  w_oltob, &
                                  w_oltombol, &
                                  w_oltomb, &
                                  wbflows   !INOUT
    USE UPDATING, ONLY : apply_cuseobs ,&
                         apply_quseobs ,&
                         apply_nutrientcorr,&
                         apply_warupd,&
                         apply_qarupd
    USE SURFACEWATER_PROCESSES, ONLY : calculate_lakebasin_average_waterstage, &
                                       calculate_outflow_from_lakebasin_lake,&
                                       remove_outflow_from_lake,        &
                                       calculate_branched_flow

    !Arguments declarations
    INTEGER, INTENT(IN) :: ilast         !<index of current subbasin (last lakebasin of current lake)
    INTEGER, INTENT(IN) :: itype         !<lake type (internal or outlet) (only outlet allowed)
    LOGICAL, INTENT(IN) :: looplakes(nsub) !<flag for lakebasins belonging to this lake
    REAL, INTENT(IN)    :: qini2(nsub)     !<inflow to lake basin from main river (incl. upstream subbasins but not other lakebasins) (m3/s)
    REAL, INTENT(IN)    :: lakeareai2(nsub) !<area of lakebasins (m2)
    REAL, INTENT(IN)    :: hypodepthi2(nsub)  !<lake hypolimnion depth (m)
    REAL, INTENT(IN)    :: olakewstoldi2(nsub) !<lake water stage, end of last timestep (mm)
    REAL, INTENT(INOUT) :: outflowsimi2(nsub) !<flow to downstream subbasins/lakebasins (m3/s)
    REAL, INTENT(INOUT) :: lakeoutflowi2(nsub) !<flow to downstream subbasins, after updating (m3/s)
    REAL, INTENT(INOUT) :: mainflowi2(nsub)   !<flow to main downstream subbasin, after updating (m3/s)
    REAL, INTENT(INOUT) :: branchflowi2(nsub) !<flow to branch downstream subbasin, after updating (m3/s)
    REAL, INTENT(INOUT) :: accinflow(nsub) !<upstream inflow to subbasin (m3/s)
    REAL, INTENT(INOUT) :: acccinflow(numsubstances,nsub) !<load of substances in upstream inflow to subbasin
    REAL, INTENT(INOUT) :: clakeoutflowi2(numsubstances,nsub) !<concentration of substances in flow to downstream subbasins
    REAL, INTENT(INOUT) :: cmainflowi2(numsubstances,nsub) !<concentration of substances in main flow to downstream subbasins
    REAL, INTENT(INOUT) :: cbranchflowi2(numsubstances,nsub) !<concentration of substances in branch flow to downstream subbasins
    REAL, INTENT(INOUT) :: qcinflii2(nsub) !<net inflow to lake basin (incl. prec-evap etc) (m3/s)
    REAL, INTENT(INOUT) :: netroutload(numsubstances,nsub) !<net load of main river and olake (outflow-(local and upstream)inflows)
    REAL, INTENT(INOUT) :: wstlakesimi2(nsub) !<lake water stage (mm)
    REAL, INTENT(INOUT) :: lakewsti2(nsub) !<lake water stage, after updating (mm)
    REAL, INTENT(INOUT) :: wcomaveri2(nsub) !<average lake water stage over the time step (mm)
    REAL, INTENT(INOUT) :: Lpathwayi2(numsubstances,18:19,nsub) !<output loads for sourceapportionment calculations
    REAL, INTENT(OUT) :: Lbranchi2(numsubstances,nsub) !<output loads for sourceapportionment calculations
    REAL, INTENT(OUT) :: totaloutflow !<total outflow of multi-basin lake (m3/s)
    REAL, INTENT(OUT) :: ctotaloutflow(numsubstances) !<concentration of total outflow of multi-basin lake
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake states
    TYPE(miscstatetype),INTENT(INOUT) :: miscstate  !<Miscstate states

    !Local variables
    LOGICAL isbout(nsub)
    INTEGER i2,isource,iupstream,ibranch
    INTEGER helpcount
    REAL lakeareawhole,oldwholelakewst,newwholelakewst,qinwholelake
    REAL outflowm3s   !all outflow of lake
    REAL w0ref,wstlake,outflowm,outflowmm
    REAL netflowi2(nsub),helpflowi2(nsub)
    REAL clboutflow(nsub,2),clboutflow2(nsub,2) !outflow of lakebasin's main and secondary branch (m3/s result, m3/ts for countdown)
    REAL loadlboutflow(numsubstances,nsub,2),conclboutflow(numsubstances,nsub) !load of inner flows (main/branch), conc of out flows
    REAL clbinflow(nsub),conclbinflow(numsubstances,nsub) !inflow to lake basins (m3/ts for countup)
    REAL flowdir,currentflow,wateravail,conccurrentflow(numsubstances)  !m3/ts
    REAL accinflowlakebasin(nsub)                  !accumulated upstream inflow to lakebasin (m3/s)
    REAL acccinflowlakebasin(numsubstances,nsub)   !concentration of upstream inflow (mg/L)

    !Parameter
    REAL, PARAMETER :: limitlevel = 0.02   !mm, lowest netflow between lakebasin to avoid numerical problem

      !> \b Algoritm \n

      !>Calculate lakebasinlake average waterstage
      CALL calculate_lakebasin_average_waterstage(ilast,lakeareawhole,oldwholelakewst,w0ref,lakestate)  !oldhwolelakewst is m above threshold (standard, not moving with deltaw0)
      !>Calculate inflow to lake (needed for outflow calculation)
      qinwholelake=0.  !I dont know all qin, set to summa landinflow, used for average rating curve
      DO i2 = 1,ilast
        IF(looplakes(i2)) qinwholelake = qinwholelake + qini2(i2)  !m3/s
      ENDDO

      !>Calculate outflow of lakebasin lake (all outlets of lakebasin lake)
      CALL calculate_outflow_from_lakebasin_lake(ilast,qinwholelake,oldwholelakewst, &
              isbout,outflowm3s,clboutflow)

      !Här finns ingen kontroll att det finns vatten i sjön tillräckligt för outflowm3s (= all clboutflow)

      !>Calculate new lake water stage we want to reach, check if water available
      IF(outflowm3s>0.)THEN
        newwholelakewst = oldwholelakewst - outflowm3s*seconds_per_timestep/lakeareawhole  !m !this is what we want to reach
        DO i2 = 1,ilast
          IF(looplakes(i2))THEN
            wstlake = (newwholelakewst + basin(i2)%lakedepth(2))/basin(i2)%lakedepth(2)
            IF(wstlake<0.000000)THEN    !Numerical uncertainty
              WRITE(6,*) 'WARNING: Outflow of multi-basin lake, ',outflowm3s,' causes water level'
              WRITE(6,*) 'WARNING: below bottom in lakebasin of subbasin i ',i2,'.'
              WRITE(6,*) 'WARNING: Setting outflow to zero.'
              outflowm3s = 0.
              clboutflow = 0.
              newwholelakewst = oldwholelakewst   !This does not help if old is below too
            ENDIF
          ENDIF
        ENDDO
      ELSE
        newwholelakewst = oldwholelakewst
      ENDIF

      !>Calculate flow between lakebasins to reach new wst; calculate net flow (first loop)
      !>and then total flow adding inflow from other upstream lakebasins (second loop)
      netflowi2 = 0.
      DO i2 = 1,ilast
        IF(looplakes(i2))THEN
          wstlake = lakestate%water(itype,i2)*1.E-3 - basin(i2)%lakedepth(2)  !m  !all lakebasins have the same threshold
          outflowm = wstlake - newwholelakewst  !m/ts, can be negative
          IF(lakestate%water(itype,i2)<0.)THEN  !Negative volume in lake is not good
            WRITE(6,*) 'WARNING: negative volume of lakebasin in subbasin', basin(i2)%subid, 'water depth (m)',wstlake
          ELSEIF(lakestate%water(itype,i2)<limitlevel)THEN  !little water left in lake, leave for safety (e.g. T2)
            IF(outflowm>0.)THEN
              !write(6,*) 'little VOLUME', i2,outflowm,wstlake
              !IF(outflowm>lakestate%water(itype,i2)*1.E-3)THEN
              !  write(6,*) 'outflow>water',i2
              !ENDIF
              outflowm = 0.
            ELSE  !outflowm<0
              !ok
            ENDIF
          ELSE
            !Real control, could happen for shallow lakebasin, save negative water?? recalc newwholelakewst?
            IF(wstlake-outflowm<- basin(i2)%lakedepth(2))THEN
              WRITE(6,*) 'WARNING: Water level below bottom of lake basin in subbasin', basin(i2)%subid
              !WRITE(6,*) 'Not Enough Water', i2,outflowm,wstlake,basin(i2)%lakedepth(2)
              outflowm = wstlake + basin(i2)%lakedepth(2)
            ENDIF
          ENDIF
          netflowi2(i2) = outflowm/seconds_per_timestep*lakeareai2(i2) !m3/s  !net total flow from this lake basin
        ENDIF
      ENDDO
      DO i2 = 1,ilast-1   !cannot use for ilast
        IF(looplakes(i2))THEN
          outflowsimi2(i2) = outflowsimi2(i2) + netflowi2(i2)  !total flow to downstream lakebasin; accumulated net flow and inflows
          IF(.NOT.ALLOCATED(branchindex))THEN
            clboutflow(i2,1) = outflowsimi2(i2)
            IF(path(i2)%main>0) outflowsimi2(path(i2)%main) = outflowsimi2(path(i2)%main) + clboutflow(i2,1)
          ELSE
            IF(branchindex(i2)==0)THEN
              clboutflow(i2,1) = outflowsimi2(i2)
              IF(path(i2)%main>0) outflowsimi2(path(i2)%main) = outflowsimi2(path(i2)%main) + clboutflow(i2,1)
            ELSE  !branchindex(i2)>0
              IF(.NOT.branchdata(branchindex(i2))%lb2outlet)THEN
                !No outflow out of the lake from this lakebasin
                !Calculate the outflow through main/branch within the lakebasins (negativ if flowing "upstream").
                CALL calculate_branched_flow(i2,outflowsimi2(i2),clboutflow(i2,1),clboutflow(i2,2)) !subroutine handle negative flows
                !The same amount flowing into a lakebasin must leave to give the constant water level
                IF(path(i2)%main>0) outflowsimi2(path(i2)%main) = outflowsimi2(path(i2)%main) + clboutflow(i2,1)
                IF(branchdata(branchindex(i2))%branch>0)THEN
                  outflowsimi2(branchdata(branchindex(i2))%branch) = outflowsimi2(branchdata(branchindex(i2))%branch) + clboutflow(i2,2)
                ENDIF
              ELSEIF(branchdata(branchindex(i2))%lb2outlet)THEN
                !Outflow out of the lake from this lakebasin's branch, only main flow withing lakebasin lake
                clboutflow(i2,1) = outflowsimi2(i2) - clboutflow(i2,2)
                IF(path(i2)%main>0) outflowsimi2(path(i2)%main) = outflowsimi2(path(i2)%main) + clboutflow(i2,1)
              ENDIF
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      !Set some output and initialise some variables needed for moving water within the lakebasin lake
      outflowsimi2(ilast) = outflowsimi2(ilast) + netflowi2(ilast)
      outflowsimi2(ilast) = clboutflow(ilast,1) + clboutflow(ilast,2) !No negative cout! Det är ändå denna jag använder i räkningarna!
      loadlboutflow = 0.  !Amount inner flows
      conclboutflow = 0.  !Conc out flows
      helpflowi2 = 0.     !variable for calculating average concentration of outflows

      !clboutflow2: This is the amout of water that will be moved out of each lake basins.
      !It will be removed stepwise, starting upstream to try and keep the flow path as short as possible.
      !When water is moved the clboutflow2 is reduced, when it became zero, all water has been moved.
      clboutflow2=clboutflow*seconds_per_timestep  !m3/ts, variable used for calculation
      IF(clboutflow2(ilast,1)>0.)THEN
        clboutflow2(ilast,2) = clboutflow2(ilast,1) + clboutflow2(ilast,2)  !Collect in branch to be handles like other outlets
        clboutflow2(ilast,1) = 0.
      ENDIF

      !>Calculate where/when to take water at what concentration and move the water
      helpcount = 0
      DO WHILE (.NOT.(SUM(ABS(clboutflow2))<1.))    !clboutflow2: water remaining to be moved within the lake
        helpcount = helpcount + 1
        IF(helpcount>5)THEN
          DO i2 = 1,ilast
            IF(looplakes(i2))THEN
              IF(clboutflow2(i2,1)+clboutflow2(i2,2)<0.000001/basin(i2)%area/classbasin(i2,slc_olake)%part)THEN  !1um
                clboutflow2(i2,1) = 0.
                clboutflow2(i2,2) = 0.
                !WRITE(6,*) 'Caught by small volume left (<0.001mm) after 5 iterations. Zerod flow.'
              ELSE
                !WRITE(6,*) 'Caught by 5 iterations. No action.'
              ENDIF
            ENDIF
          ENDDO
        ENDIF
        IF(helpcount>100)THEN
          WRITE(6,*) 'Lakebasin infinit loop exited for subid: ',basin(ilast)%subid
          WRITE(6,*) 'Water that will not flow within lake as planned; ',SUM(ABS(clboutflow2)),'m3.'
          !DO i2 = 1,ilast
            !IF(looplakes(i2))THEN
              !WRITE(6,*) i2,netflowi2(i2),clboutflow2(i2,1),lakestate%water(itype,i2),outflowsimi2(i2)
            !ENDIF
          !ENDDO
          !WRITE(6,*) qinwholelake,oldwholelakewst,newwholelakewst,outflowm3s
          EXIT
        ENDIF
        clbinflow = 0.    !Zero the inflows to be ready to calculated for this step
        conclbinflow = 0.
        DO i2=1,ilast
          IF(looplakes(i2))THEN   !Loop over all lakebasins of current lakebasinlake
            DO isource=1,ilast     !Loop over all lakebasins next to the i2 one
              IF(looplakes(isource))THEN

                !Find sender of water to current reciever (i2).
                CALL check_connected_subbasins(i2,isource,iupstream,ibranch)
                IF(iupstream==0) CYCLE  !Not connected
                IF(iupstream == i2) flowdir = -1.
                IF(iupstream == isource) flowdir = +1.
                currentflow = flowdir * clboutflow2(iupstream,ibranch) !m3/ts
                !This lakebasin is next to the i2 basin, is it a source?
                IF(currentflow<=0.) CYCLE   !Not an inflow to i2

                !For found source: 1) calculate inflow to reciever (i2) (currentflow),
                !2) add to other inflows to this reciever (clbinflows),
                !3) remove the flow from the source lake water (lakestate%water)
                !4) and remove it also from from "water remaining to be moved within the lake" (clboutflow2)
                wateravail = lakestate%water(2,isource)*basin(isource)%area*classbasin(isource,slc_olake)%part*1.E-3  !m3
                IF(wateravail>currentflow)THEN    !I think this has to be done in mm for accuracy?
                  outflowmm = currentflow/(basin(isource)%area*classbasin(isource,slc_olake)%part)*1.E3
                  IF(numsubstances>0)THEN
                    CALL remove_outflow_from_lake(isource,2,numsubstances,outflowmm,basin(isource)%subid,basin(isource)%lakedepth(2),hypodepthi2(isource),lakestate%water(itype,isource),conccurrentflow,lakestate) !remove water and set conc of flow
                    CALL add_water(numsubstances,clbinflow(i2),conclbinflow(:,i2),currentflow,conccurrentflow)
                  ELSE
                    lakestate%water(itype,isource) = lakestate%water(itype,isource) - outflowmm
                    clbinflow(i2) = clbinflow(i2) + currentflow
                  ENDIF
                  clboutflow2(iupstream,ibranch) = 0.
                ELSEIF(wateravail>0.)THEN
                  currentflow = wateravail
                  IF(numsubstances>0)THEN
                    conccurrentflow = lakestate%conc(:,2,isource)
                    CALL add_water(numsubstances,clbinflow(i2),conclbinflow(:,i2),currentflow,conccurrentflow)
                  ELSE
                    clbinflow(i2) = clbinflow(i2) + currentflow
                  ENDIF
                  lakestate%water(2,isource) = 0.
                  IF(numsubstances>0) lakestate%conc(:,2,isource) = 0.
                  clboutflow2(iupstream,ibranch) = clboutflow2(iupstream,ibranch) - flowdir*currentflow
                ELSE
                  !No water in lakebasin. Wait on inflow.
                  currentflow = 0.
                  conccurrentflow = 0.
                ENDIF
                IF(numsubstances>0 .AND. currentflow>0.) loadlboutflow(:,iupstream,ibranch) = loadlboutflow(:,iupstream,ibranch) + flowdir*currentflow*conccurrentflow
              ENDIF
            ENDDO
            IF(isbout(i2))THEN
              !Catch both last and other outlets in brances now
              !Remove outflow of last lakebasin (is not inflow to any other lakebasin, so not catched by loops).
              !All outflow of last put in clboutflow2(ilast,2) to catch here
              IF(clboutflow2(i2,2)>0)THEN
                currentflow = clboutflow2(i2,2) !m3/ts
                wateravail = lakestate%water(itype,i2)*basin(i2)%area*classbasin(i2,slc_olake)%part*1.E-3  !m3
                IF(wateravail>currentflow)THEN    !calculate in mm here instead!?
                  outflowmm = currentflow/(basin(i2)%area*classbasin(i2,slc_olake)%part)*1.E3  !mm/ts
                  IF(numsubstances>0)THEN
                    CALL remove_outflow_from_lake(i2,itype,numsubstances,outflowmm,basin(i2)%subid,basin(i2)%lakedepth(2),hypodepthi2(i2),lakestate%water(itype,i2),conccurrentflow,lakestate) !remove water and set conc of flow
                    CALL add_water(numsubstances,helpflowi2(i2),conclboutflow(:,i2),currentflow/seconds_per_timestep,conccurrentflow)
                  ELSE
                    lakestate%water(itype,i2) = lakestate%water(itype,i2) - outflowmm
                  ENDIF
                  clboutflow2(i2,2) = 0.
                ELSEIF(wateravail>0.)THEN
                  currentflow = wateravail
                  outflowmm = currentflow/(basin(i2)%area*classbasin(i2,slc_olake)%part*1.E-3)
                  IF(numsubstances>0)THEN
                    CALL remove_outflow_from_lake(i2,itype,numsubstances,outflowmm,basin(i2)%subid,basin(i2)%lakedepth(2),hypodepthi2(i2),lakestate%water(itype,i2),conccurrentflow,lakestate) !set conc of flow
                    CALL add_water(numsubstances,helpflowi2(i2),conclboutflow(:,i2),currentflow/seconds_per_timestep,conccurrentflow)
                  ENDIF
                  lakestate%water(itype,i2) = 0.
                  IF(numsubstances>0) lakestate%conc(:,itype,i2) = 0.
                  clboutflow2(i2,2) = clboutflow2(i2,2) - currentflow
                ELSE
                  !No water in lakebasin. Wait on inflow.
                ENDIF
              ENDIF
            ENDIF
          ENDIF
        ENDDO
        !>Let calculated flows (clbinflow, m3/ts) move to recieving lakebasin (i2)
        DO i2=1,ilast
          IF(clbinflow(i2)>0.)THEN
            currentflow = clbinflow(i2)/basin(i2)%area/classbasin(i2,slc_olake)%part/1.E-3  !mm/ts
            IF(numsubstances>0)THEN
              CALL add_water(numsubstances,lakestate%water(2,i2),lakestate%conc(:,2,i2),currentflow,conclbinflow(:,i2))
            ELSE
              lakestate%water(2,i2) = lakestate%water(2,i2) + currentflow
            ENDIF
          ENDIF
        ENDDO
      ENDDO

      !>Set variables used for output of upstream lakebasins
      accinflowlakebasin = 0.
      acccinflowlakebasin = 0.
      Lbranchi2 = 0.
      DO i2 = 1,ilast-1
        IF(looplakes(i2))THEN
          lakeoutflowi2(i2) = outflowsimi2(i2) !Kan vara summan av pos branch och neg internt flöde
          lakewsti2(i2) = lakestate%water(itype,i2)
          wstlakesimi2(i2) = lakewsti2(i2)          !saved for output
          wcomaveri2(i2) = (wstlakesimi2(i2) + olakewstoldi2(i2))/2.
          IF(.NOT.isbout(i2))THEN
            CALL calculate_branched_flow(i2,lakeoutflowi2(i2),mainflowi2(i2),branchflowi2(i2))    !unnessecary? same conc
            IF(numsubstances>0)THEN
              IF(lakeoutflowi2(i2)/=0.) clakeoutflowi2(:,i2) = (loadlboutflow(:,i2,1)+loadlboutflow(:,i2,2))/(lakeoutflowi2(i2)*seconds_per_timestep)
              IF(mainflowi2(i2)/=0.)    cmainflowi2(:,i2)    = loadlboutflow(:,i2,1) / (mainflowi2(i2) * seconds_per_timestep)
              IF(branchflowi2(i2)/=0.)  cbranchflowi2(:,i2)  = loadlboutflow(:,i2,2) / (branchflowi2(i2) * seconds_per_timestep)
            ENDIF
            !Accumulate flow to downstream lakebasins
            IF(lakeoutflowi2(i2)/=0.)THEN
              IF(mainflowi2(i2)/=0.)THEN
                IF(path(i2)%uplakebasin)THEN
                  IF(path(i2)%main>0)THEN
                    accinflowlakebasin(path(i2)%main) = accinflowlakebasin(path(i2)%main) + mainflowi2(i2)
                    IF(simulatesubstances) acccinflowlakebasin(:,path(i2)%main) = acccinflowlakebasin(:,path(i2)%main) + loadlboutflow(:,i2,1) / seconds_per_timestep
                  ENDIF
                  IF(conductwb) wbflows(w_oltombol,i2) = mainflowi2(i2) * seconds_per_timestep   !m3/timestep
                ENDIF
              ENDIF
              IF(branchflowi2(i2)/=0.)THEN
                IF(branchdata(branchindex(i2))%branch>0)THEN
                  IF(branchdata(branchindex(i2))%uplakebasin)THEN
                    accinflowlakebasin(branchdata(branchindex(i2))%branch) = accinflowlakebasin(branchdata(branchindex(i2))%branch) + branchflowi2(i2)
                    IF(simulatesubstances)THEN
                      acccinflowlakebasin(:,branchdata(branchindex(i2))%branch) = acccinflowlakebasin(:,branchdata(branchindex(i2))%branch) + loadlboutflow(:,i2,2) / seconds_per_timestep
                      conclboutflow(:,i2) = loadlboutflow(:,i2,2)/(branchflowi2(i2)*seconds_per_timestep)
                    ENDIF
                    IF(conductwb) wbflows(w_oltobol,i2)  = branchflowi2(i2) * seconds_per_timestep
                  ENDIF
                ENDIF
              ENDIF
            ENDIF
          ELSE  !isbout(i2)
            mainflowi2(i2) = clboutflow(i2,1)
            branchflowi2(i2) = clboutflow(i2,2)
            IF(numsubstances>0)THEN
              IF(mainflowi2(i2)/=0.)  cmainflowi2(:,i2)   = loadlboutflow(:,i2,1) / (mainflowi2(i2) * seconds_per_timestep)
              IF(branchflowi2(i2)>0.) cbranchflowi2(:,i2) = conclboutflow(:,i2)
              IF(mainflowi2(i2)>0.)THEN
                clakeoutflowi2(:,i2) = (loadlboutflow(:,i2,1)+conclboutflow(:,i2)*branchflowi2(i2)*seconds_per_timestep)/(lakeoutflowi2(i2)*seconds_per_timestep)
              ELSEIF(mainflowi2(i2)==0.)THEN
                clakeoutflowi2(:,i2) = conclboutflow(:,i2) !conc = branchflow conc
              ELSEIF(mainflowi2(i2)<0.)THEN
                clakeoutflowi2(:,i2) = missing_value !not defined in this case
              ENDIF
            ENDIF
            !Accumulate flow to downstream subbasins/lakebasins
            IF(lakeoutflowi2(i2)/=0.)THEN !can be zero if main and branch match!
              IF(mainflowi2(i2)/=0.)THEN
                IF(path(i2)%uplakebasin)THEN    !Maybe unnecessary, i2/=ilast
                  IF(path(i2)%main>0)THEN
                    accinflowlakebasin(path(i2)%main) = accinflowlakebasin(path(i2)%main) + mainflowi2(i2)
                    IF(simulatesubstances) acccinflowlakebasin(:,path(i2)%main) = acccinflowlakebasin(:,path(i2)%main) + loadlboutflow(:,i2,1) / seconds_per_timestep
                  ENDIF
                  IF(conductwb) wbflows(w_oltombol,i2) = mainflowi2(i2) * seconds_per_timestep   !m3/timestep
                ENDIF
              ENDIF
              IF(branchflowi2(i2)/=0.)THEN  !maybe ok with >0
                IF(branchdata(branchindex(i2))%branch>0)THEN
                  accinflow(branchdata(branchindex(i2))%branch) = accinflow(branchdata(branchindex(i2))%branch) + branchflowi2(i2)
                  IF(simulatesubstances) acccinflow(:,branchdata(branchindex(i2))%branch) = acccinflow(:,branchdata(branchindex(i2))%branch) + conclboutflow(:,i2) * branchflowi2(i2)
                  IF(conductwb) wbflows(w_oltob,ilast)  = branchflowi2(i2) * seconds_per_timestep
                ENDIF
              ENDIF
            ENDIF
          ENDIF
          qcinflii2(i2) = qcinflii2(i2) + accinflowlakebasin(i2)    !For output
          IF(numsubstances>0) netroutload(:,i2) = netroutload(:,i2) - acccinflowlakebasin(:,i2)   !Independent if accinflow is pos or negative!
          IF(conductload) Lpathwayi2(:,18,i2) = Lpathwayi2(:,18,i2) + acccinflowlakebasin(:,i2) * seconds_per_timestep * 1.E-3     !Load at point Q inflow to olake (kg/timestep)
          IF(conductload) Lbranchi2(:,i2) = conclboutflow(:,i2) * branchflowi2(i2)  * seconds_per_timestep * 1.E-3  !Load in branch (kg/timestep), point S, not for ilast!
          IF(conductload) Lpathwayi2(:,19,i2) =  (loadlboutflow(:,i2,1)+conclboutflow(:,i2)*branchflowi2(i2)*seconds_per_timestep)  * 1.E-3  !Load at point R, outflow of subbasin (kg/timestep) not for ilast!
        ENDIF
      ENDDO

      !>Set output of last lakebasin
      lakeoutflowi2(ilast) = outflowsimi2(ilast)
      IF(numsubstances>0) clakeoutflowi2(:,ilast) = conclboutflow(:,ilast)  !all outflow of last put in branch for calculations
      lakewsti2(ilast) = lakestate%water(itype,ilast)
      wstlakesimi2(ilast) = lakewsti2(ilast)          !saved for output
      wcomaveri2(ilast) = (wstlakesimi2(ilast) + olakewstoldi2(ilast))/2.
      qcinflii2(ilast) = qcinflii2(ilast) + accinflowlakebasin(ilast)
      IF(numsubstances>0) netroutload(:,ilast) = netroutload(:,ilast) - acccinflowlakebasin(:,ilast)   !Independent if accinflow is pos or negative!
      IF(conductload) Lpathwayi2(:,18,ilast) = Lpathwayi2(:,18,ilast) + acccinflowlakebasin(:,ilast) * seconds_per_timestep * 1.E-3     !Load at point Q inflow to olake (kg/timestep)
      IF(conductload) Lpathwayi2(:,19,ilast) = lakeoutflowi2(ilast) * conclboutflow(:,ilast) * seconds_per_timestep * 1.E-3  !Load at point R, outflow of subbasin (kg/timestep)

      !>Update by/of water stage
      !Update lake outflow with waterstage observations (only outflow of last lakebasin)
      IF(doupdate(i_war))THEN
        CALL apply_warupd(ilast,1,lakeareai2(ilast),olakewstoldi2(ilast),lakewsti2(ilast),lakeoutflowi2(ilast),miscstate%updatestationsarcorr(ilast),wcomaveri2(ilast))
      ENDIF
      !Update lake water stage (all lakebasins)
      IF(doupdate(i_wendupd))THEN
        CALL apply_wendupd_lakebasin_lake(ilast,looplakes,lakewsti2,lakestate)
      ENDIF

      !>Update modelled subbasin outflow with observations or nutrient concentrations, only for whole lake outflow
      IF(doupdate(i_quseobs)) CALL apply_quseobs(ilast,lakeoutflowi2(ilast))
      IF(doupdate(i_qar))     CALL apply_qarupd(ilast,outflowsimi2(ilast),lakeoutflowi2(ilast),miscstate%updatestationsarcorr(ilast))
      CALL calculate_branched_flow(ilast,lakeoutflowi2(ilast),mainflowi2(ilast),branchflowi2(ilast))    !Enough
      IF(conductload) Lbranchi2(:,ilast) = branchflowi2(ilast) * clakeoutflowi2(:,ilast) * seconds_per_timestep * 1.E-3  !Load in branch (kg/timestep), point S
      !Update modelled subbasin outflow with nutrient concentrations (on outlet of last lakebasin)
      IF(doupdate(i_tpcorr))  CALL apply_nutrientcorr(i_tpcorr,ilast,clakeoutflowi2(i_sp,ilast),clakeoutflowi2(i_pp,ilast))
      IF(doupdate(i_tncorr))  CALL apply_nutrientcorr(i_tncorr,ilast,clakeoutflowi2(i_in,ilast),clakeoutflowi2(i_on,ilast))
      IF(doupdate(i_cuseobs)) CALL apply_cuseobs(ilast,clakeoutflowi2(:,ilast))
      IF(numsubstances>0)THEN
        IF(mainflowi2(ilast)>0.)  cmainflowi2(:,ilast)   = clakeoutflowi2(:,ilast)  !for output
        IF(branchflowi2(ilast)>0.) cbranchflowi2(:,ilast) = clakeoutflowi2(:,ilast)
      ENDIF

      !>Accumulate flow to downstream subbasins, downstream of whole lakebasinlake
      IF(lakeoutflowi2(ilast)>0.)THEN
        IF(mainflowi2(ilast)>0.)THEN
          IF(path(ilast)%main>0)THEN
            accinflow(path(ilast)%main) = accinflow(path(ilast)%main) + mainflowi2(ilast)
            IF(simulatesubstances) acccinflow(:,path(ilast)%main) = acccinflow(:,path(ilast)%main) + clakeoutflowi2(:,ilast) * mainflowi2(ilast)
          ENDIF
          IF(conductwb) wbflows(w_oltomb,ilast) = mainflowi2(ilast) * seconds_per_timestep   !m3/timestep
        ENDIF
        IF(branchflowi2(ilast)>0.)THEN
          IF(branchdata(branchindex(ilast))%branch>0)THEN
            accinflow(branchdata(branchindex(ilast))%branch) = accinflow(branchdata(branchindex(ilast))%branch) + branchflowi2(ilast)
            IF(simulatesubstances) acccinflow(:,branchdata(branchindex(ilast))%branch) = acccinflow(:,branchdata(branchindex(ilast))%branch) + clakeoutflowi2(:,ilast) * branchflowi2(ilast)
            IF(conductwb) wbflows(w_oltob,ilast)  = branchflowi2(ilast) * seconds_per_timestep
          ENDIF
        ENDIF
      ENDIF

      !>Set output of whole multi-basin lake outflow
      totaloutflow = outflowm3s
      ctotaloutflow = 0.
      IF(numsubstances>0)THEN
        ctotaloutflow = ctotaloutflow + lakeoutflowi2(ilast)*conclboutflow(:,ilast)
        DO i2=1,ilast-1
          IF(isbout(i2))THEN
            IF(branchflowi2(i2)>0.) ctotaloutflow = ctotaloutflow + branchflowi2(i2)*cbranchflowi2(:,i2)
          ENDIF
        ENDDO
        ctotaloutflow = ctotaloutflow/totaloutflow
      ENDIF

  END SUBROUTINE calculate_flow_for_lakebasin_lake

  !>Accumulate flow to downstream subbasins, and calculate volume flow for water balance
  !-------------------------------------------------------------------------------------
  SUBROUTINE accumulate_flow_to_downstream_subbasin(i,mainflow,branchflow,concout,accinflow,acccinflow,volumeflow)

    USE MODVAR, ONLY : conductwb, &
                       branchdata, &
                       branchindex, &
                       path, &
                       nsub, &
                       numsubstances
    USE HYPE_WATERBALANCE, ONLY : w_oltomb,w_oltob

    !Argument declarations
    INTEGER, INTENT(IN) :: i  !<index of current subbasin
    REAL, INTENT(IN)    :: mainflow  !<outflow through main branch (m3/s)
    REAL, INTENT(IN)    :: branchflow  !<outflow through second branch (m3/s)
    REAL, INTENT(IN)    :: concout(numsubstances)  !<concentration of outflow
    REAL, INTENT(INOUT) :: accinflow(nsub)  !<inflow from upstream subbasins (m3/s)
    REAL, INTENT(INOUT) :: acccinflow(numsubstances,nsub)  !<concentration of inflow
    REAL,ALLOCATABLE,INTENT(INOUT) :: volumeflow(:,:)    !<volume outflow of subbasin (m3/ts) (and other flows)

    !>\b Algorithm \n
    !>Accumulate flow and load for downstream inflow from main outlet
    IF(mainflow>0)THEN
      IF(path(i)%main>0)THEN
        accinflow(path(i)%main) = accinflow(path(i)%main) + mainflow
        IF(simulatesubstances) acccinflow(:,path(i)%main) = acccinflow(:,path(i)%main) + concout(:) * mainflow
      ENDIF
      !>Save main outlet flow for water balance output
      IF(conductwb) volumeflow(w_oltomb,i) = mainflow * seconds_per_timestep   !m3/timestep
    ENDIF
    !>Accumulate flow and load for downstream inflow from branch
    IF(branchflow>0)THEN
      IF(branchdata(branchindex(i))%branch>0)THEN
        accinflow(branchdata(branchindex(i))%branch) = accinflow(branchdata(branchindex(i))%branch) + branchflow
        IF(simulatesubstances) acccinflow(:,branchdata(branchindex(i))%branch) = acccinflow(:,branchdata(branchindex(i))%branch) + concout(:) * branchflow
        !>Save branched flow for water balance output
        IF(conductwb) volumeflow(w_oltob,i)  = branchflow * seconds_per_timestep
      ENDIF
    ENDIF

  END SUBROUTINE accumulate_flow_to_downstream_subbasin

  !>Calculate inflow, outflow and internal processes of an internal wetland (if present).
  !>Precipitation and soil related processes no included. They are calculated by the soilmodel earlier.
  !>
  !>\b Reference ModelDescription Chapter Water management (Constructed wetlands with
  !>water regulation capability)
  !-------------------------------------------------------------------------------------
  SUBROUTINE calculate_internal_wetland(i,radext,cloud,added_water,q,conc,frozenstate,soilstate, &
                                        miscstate,localoutvar,localwbflows,localLpathway)

    USE MODVAR, ONLY : basin, &
                       classbasin, &
                       classdata, &
                       conduct, &
                       conductload, &
                       conductwb, &
                       i_in,i_on,i_sp,i_pp,i_ss,i_ae,i_oc,i_t2,&
                       landarea, &
                       numsubstances, &
                       outvarindex, &
                       seconds_per_timestep, &
                       slc_iwet
    USE HYPEVARIABLES, ONLY : iwetnoninflow, &
                              pwmm
    USE HYPEVARIABLES, ONLY : o_iwinfl,o_iwoutfl,o_iwincoin,o_iwincoon, &       !lots of indices
                       o_iwutcoin,o_iwutcoon,o_iwincosp,o_iwincopp,o_iwutcosp,o_iwutcopp, &
                       o_iwincoss,o_iwincoae,o_iwutcoss,o_iwutcoae,o_iwincooc,o_iwutcooc, &
                       o_iwincot2,o_iwutcot2,o_iwetvol,o_crun3,o_wstiwet
    USE HYPE_WATERBALANCE, ONLY : w_smeltsrtoiw,w_smeltsr,w_gmeltsrtoiw, &    !lots of indices
                    w_gmeltsr,w_rainsrtoiw,w_rainsr,w_surfrftoiw,w_surfrf, &
                    w_tile1toiw,w_tile1,w_tile2toiw,w_tile2,w_tile3toiw,w_tile3, &
                    w_gwrunf1toiw,w_gwrunf1,w_gwrunf2toiw,w_gwrunf2, &
                    w_gwrunf3toiw,w_gwrunf3,w_iwtoir
    USE ATMOSPHERIC_PROCESSES, ONLY : calculate_class_atmospheric_forcing
    USE SURFACEWATER_PROCESSES, ONLY : get_wetland_threshold, &
                                       wetland_watermodel


      !Argument declarations
      INTEGER, INTENT(IN) :: i  !<current subbasin index
      REAL, INTENT(IN)    :: radext  !<extra terrestrial radiation for subbasin i
      REAL, INTENT(IN)    :: cloud    !<subbasin cloudiness (-)
      REAL, INTENT(IN)    :: added_water !<water already added to wetland (soil layer 1), eg. P-E (mm/ts)
      REAL, INTENT(INOUT) :: q  !<flow to local river
      REAL, INTENT(INOUT) :: conc(numsubstances)  !<concentration of flow to local river
      TYPE(snowicestatetype),INTENT(IN)  :: frozenstate !<Frozen states
      TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
      TYPE(miscstatetype),INTENT(INOUT)  :: miscstate   !<Misc states
      REAL, INTENT(INOUT),ALLOCATABLE :: localoutvar(:,:)  !<outvar; output for print out
      REAL, INTENT(INOUT),ALLOCATABLE :: localwbflows(:,:)  !<wbflows; flow for water balance output (m3/ts)
      REAL, INTENT(INOUT),ALLOCATABLE :: localLpathway(:,:)  !<Lpathway; load for print out

      !Local variables
      REAL classarea   !area of iwet (m2)
      REAL iwetcatch   !area of iwet catchment (m2)
      REAL qout        !flow to local river after internal wetland has influenced it
      REAL added_inflow  !inflow to wetland (soil layer 1) already added, eg. P-E (m3/s)
      REAL wetinflow, wetoutflow   !inflow and outflow to iwet (m3/s)
      REAL cwetoutflow(numsubstances) !concentration of outflow from iwet
      REAL temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot  !class forcing

      !>\b Algorithm \n
      !>Check for internal wetland
      IF(slc_iwet==0) RETURN
      IF(classbasin(i,slc_iwet)%part==0) RETURN

      !Calculate helpful variables for the internal wetland
      classarea = classbasin(i,slc_iwet)%part * basin(i)%area   !m2
      iwetcatch = basin(i)%iwetcatch * basin(i)%area  !m2
      wetinflow = q * (1. - iwetnoninflow(i))   !A part of the flow to local river goes through the wetland
      added_inflow = added_water * classarea *1.E-3 / seconds_per_timestep

      !>If water temperature (T2) is simulated: calculate current temperature and solar radiation for surface heat exchange
      IF(conduct%simT2)Then
        CALL calculate_class_atmospheric_forcing(i,slc_iwet,radext,cloud,  &
              temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
        !IF(sfdist(slc_iwet)/=1.)  prec = prec*sfdist(slc_iwet) !adjust class snowfall wind distribution (prec not used)
      ENDIF

      !>Calculate inflow and outflow to wetland (and processes therein)
      CALL wetland_watermodel(i,slc_iwet,classdata(slc_iwet)%soil,basin(i)%subid,classarea, &
                             temp,swrad,soilstate,miscstate,added_inflow,    &
                             wetinflow,conc,iwetcatch,wetoutflow,cwetoutflow)

      !>Set output variables of internal wetland
      IF(outvarindex(o_iwinfl)>0) localoutvar(i,outvarindex(o_iwinfl)) = wetinflow
      IF(outvarindex(o_iwoutfl)>0) localoutvar(i,outvarindex(o_iwoutfl)) = wetoutflow
      IF(conduct%simN)THEN
        IF(outvarindex(o_iwincoin)>0) localoutvar(i,outvarindex(o_iwincoin)) = conc(i_in)
        IF(outvarindex(o_iwincoon)>0) localoutvar(i,outvarindex(o_iwincoon)) = conc(i_on)
        IF(outvarindex(o_iwutcoin)>0) localoutvar(i,outvarindex(o_iwutcoin)) = cwetoutflow(i_in)
        IF(outvarindex(o_iwutcoon)>0) localoutvar(i,outvarindex(o_iwutcoon)) = cwetoutflow(i_on)
      ENDIF
      IF(conduct%simP)THEN
        IF(outvarindex(o_iwincosp)>0) localoutvar(i,outvarindex(o_iwincosp)) = conc(i_sp)
        IF(outvarindex(o_iwincopp)>0) localoutvar(i,outvarindex(o_iwincopp)) = conc(i_pp)
        IF(outvarindex(o_iwutcosp)>0) localoutvar(i,outvarindex(o_iwutcosp)) = cwetoutflow(i_sp)
        IF(outvarindex(o_iwutcopp)>0) localoutvar(i,outvarindex(o_iwutcopp)) = cwetoutflow(i_pp)
      ENDIF
      IF(conduct%simS)THEN
        IF(outvarindex(o_iwincoss)>0) localoutvar(i,outvarindex(o_iwincoss)) = conc(i_ss)
        IF(outvarindex(o_iwutcoss)>0) localoutvar(i,outvarindex(o_iwutcoss)) = cwetoutflow(i_ss)
        IF(outvarindex(o_iwincoae)>0) localoutvar(i,outvarindex(o_iwincoae)) = conc(i_ae)
        IF(outvarindex(o_iwutcoae)>0) localoutvar(i,outvarindex(o_iwutcoae)) = cwetoutflow(i_ae)
      ENDIF
      IF(conduct%simC)THEN
        IF(outvarindex(o_iwincooc)>0) localoutvar(i,outvarindex(o_iwincooc)) = conc(i_oc)
        IF(outvarindex(o_iwutcooc)>0) localoutvar(i,outvarindex(o_iwutcooc)) = cwetoutflow(i_oc)
      ENDIF
      IF(conduct%simT2)THEN
        IF(outvarindex(o_iwincot2)>0) localoutvar(i,outvarindex(o_iwincot2)) = conc(i_t2)
        IF(outvarindex(o_iwutcot2)>0) localoutvar(i,outvarindex(o_iwutcot2)) = cwetoutflow(i_t2)
      ENDIF
      IF(conductload)THEN
        localLpathway(:,2) = localLpathway(:,1) * iwetnoninflow(i)                       !Load passing iwet, point B (kg/timestep)
        localLpathway(:,3) = wetinflow*conc(:)*seconds_per_timestep*1.E-3           !Load into iwet from land, point C (kg/timestep)
        localLpathway(:,4) = wetoutflow*cwetoutflow(:)*seconds_per_timestep*1.E-3   !Load out of iwet, point D (kg/timestep)
      ENDIF
      IF(outvarindex(o_iwetvol)>0) localoutvar(i,outvarindex(o_iwetvol)) = (frozenstate%snow(slc_iwet,i) + SUM(soilstate%water(:,slc_iwet,i)))*classarea*1.E-3
      IF(outvarindex(o_wstiwet)>0)THEN
        localoutvar(i,outvarindex(o_wstiwet)) = - get_wetland_threshold(slc_iwet)
        IF((soilstate%water(1,slc_iwet,i) - pwmm(1,slc_iwet))>=0.) &
        localoutvar(i,outvarindex(o_wstiwet)) = localoutvar(i,outvarindex(o_wstiwet)) + (soilstate%water(1,slc_iwet,i) - pwmm(1,slc_iwet))*1.E-3
      ENDIF

      !>Set water balance flows for internal wetland (m3/ts)
      IF(conductwb)THEN
        localwbflows(w_smeltsrtoiw,i) = localwbflows(w_smeltsr,i) * (1. - iwetnoninflow(i))
        localwbflows(w_smeltsr,i) = localwbflows(w_smeltsr,i) * iwetnoninflow(i)
        localwbflows(w_gmeltsrtoiw,i) = localwbflows(w_gmeltsr,i) * (1. - iwetnoninflow(i))
        localwbflows(w_gmeltsr,i) = localwbflows(w_gmeltsr,i) * iwetnoninflow(i)
        localwbflows(w_rainsrtoiw,i) = localwbflows(w_rainsr,i) * (1. - iwetnoninflow(i))
        localwbflows(w_rainsr,i) = localwbflows(w_rainsr,i) * iwetnoninflow(i)
        localwbflows(w_surfrftoiw,i) = localwbflows(w_surfrf,i) * (1. - iwetnoninflow(i))
        localwbflows(w_surfrf,i) = localwbflows(w_surfrf,i) * iwetnoninflow(i)
        localwbflows(w_tile1toiw,i) = localwbflows(w_tile1,i) * (1. - iwetnoninflow(i))
        localwbflows(w_tile1,i) = localwbflows(w_tile1,i) * iwetnoninflow(i)
        localwbflows(w_tile2toiw,i) = localwbflows(w_tile2,i) * (1. - iwetnoninflow(i))
        localwbflows(w_tile2,i) = localwbflows(w_tile2,i) * iwetnoninflow(i)
        localwbflows(w_tile3toiw,i) = localwbflows(w_tile3,i) * (1. - iwetnoninflow(i))
        localwbflows(w_tile3,i) = localwbflows(w_tile3,i) * iwetnoninflow(i)
        localwbflows(w_gwrunf1toiw,i) = localwbflows(w_gwrunf1,i) * (1. - iwetnoninflow(i))
        localwbflows(w_gwrunf1,i) = localwbflows(w_gwrunf1,i) * iwetnoninflow(i)
        localwbflows(w_gwrunf2toiw,i) = localwbflows(w_gwrunf2,i) * (1. - iwetnoninflow(i))
        localwbflows(w_gwrunf2,i) = localwbflows(w_gwrunf2,i) * iwetnoninflow(i)
        localwbflows(w_gwrunf3toiw,i) = localwbflows(w_gwrunf3,i) * (1. - iwetnoninflow(i))
        localwbflows(w_gwrunf3,i) = localwbflows(w_gwrunf3,i) * iwetnoninflow(i)
        localwbflows(w_iwtoir,i) = wetoutflow * seconds_per_timestep    !m3/ts
      ENDIF

      !>Calculate resulting inflow to local river after passing internal wetland
      qout = q * iwetnoninflow(i) + wetoutflow
      IF(numsubstances>0 .AND. qout>0.) conc = (conc * q * iwetnoninflow(i) + cwetoutflow * wetoutflow)/qout
      q = qout  !Set OUT
      IF(outvarindex(o_crun3)>0) localoutvar(i,outvarindex(o_crun3)) = q * seconds_per_timestep / (landarea(i)+classarea) * 1.E3 !mm

  END SUBROUTINE calculate_internal_wetland

  !>Calculate inflow, outflow and internal processes of an outlet wetland (if present).
  !>Precipitation and soil related processes no included. They are calculated by the soilmodel earlier.
  !>
  !>\b Reference ModelDescription Chapter Water management (Constructed wetlands with
  !>water regulation capability)
  !-------------------------------------------------------------------------------------
  SUBROUTINE calculate_outlet_wetland(i,radext,cloud,added_water,wetinflow,conc,frozenstate,soilstate,miscstate, &
                                      wetoutflow,updatedoutflow,cwetoutflow,mainflow, &
                                      branchflow,localoutvar,localwbflows,localLpathway)

    USE MODVAR, ONLY : basin, &
                       classbasin, &
                       classdata, &
                       conduct, &
                       conductload, &
                       conductwb, &
                       doupdate, &
                       i_quseobs,i_qar, &
                       numsubstances, &
                       outvarindex, &
                       seconds_per_timestep, &
                       slc_owet, &
                       upstreamarea
    USE HYPEVARIABLES, ONLY : pwmm
    USE HYPEVARIABLES, ONLY : o_owetvol,o_wstowet
    USE HYPE_WATERBALANCE, ONLY : w_mrtoow
    USE ATMOSPHERIC_PROCESSES, ONLY : calculate_class_atmospheric_forcing
    USE UPDATING, ONLY : apply_quseobs,apply_qarupd
    USE SURFACEWATER_PROCESSES, ONLY : calculate_branched_flow, &
                                       get_wetland_threshold, &
                                       wetland_watermodel

      !Argument declarations
      INTEGER, INTENT(IN) :: i  !<current subbasin index
      REAL, INTENT(IN)    :: radext  !<extra terrestrial radiation for subbasin i
      REAL, INTENT(IN)    :: cloud    !<subbasin cloudiness (-)
      REAL, INTENT(IN)    :: added_water !<water already added to wetland (soil layer 1), eg. P-E (mm/ts)
      REAL, INTENT(IN)    :: wetinflow  !<flow from main river (dampq) (m3/s)
      REAL, INTENT(IN)    :: conc(numsubstances)  !<concentration of flow from main river
      TYPE(snowicestatetype),INTENT(IN)  :: frozenstate !<Frozen states
      TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
      TYPE(miscstatetype),INTENT(INOUT)  :: miscstate   !<Misc states
      REAL, INTENT(OUT)   :: wetoutflow  !<flow from outlet wetland (originally simulated) (m3/s)
      REAL, INTENT(OUT)   :: updatedoutflow  !<flow from outlet wetland after updating (m3/s)
      REAL, INTENT(OUT)   :: cwetoutflow(numsubstances)  !<concentration of flow from outlet wetland
      REAL, INTENT(OUT)   :: mainflow    !<flow from outlet wetland to main downstream subbasin (m3/s)
      REAL, INTENT(OUT)   :: branchflow  !<flow from outlet wetland to branched downstream subbasin  (m3/s)
      REAL, INTENT(INOUT),ALLOCATABLE :: localoutvar(:,:)  !<outvar; output for print out
      REAL, INTENT(INOUT),ALLOCATABLE :: localwbflows(:,:)  !<wbflows; flow for water balance output (m3/ts)
      REAL, INTENT(INOUT),ALLOCATABLE :: localLpathway(:,:)  !<Lpathway; load for print out

      !Local variables
      REAL classarea   !area of owet (m2)
      REAL owetcatch   !area of owet catchment (m2)
      REAL added_inflow  !inflow to wetland (soil layer 1) already added, eg. P-E (m3/s)
      REAL temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot  !class forcing

      !>\b Algorithm \n
      !>Check for outlet wetland
      IF(slc_owet==0) RETURN
      IF(classbasin(i,slc_owet)%part==0) RETURN

      !!Calculate helpful variables for the outlet wetland
      classarea = classbasin(i,slc_owet)%part * basin(i)%area   !m2
      owetcatch = upstreamarea(i) - classarea !m2
      added_inflow = added_water * classarea *1.E-3 / seconds_per_timestep

      !>If water temperature (T2) is simulated: calculate current temperature and solar radiation for surface heat exchange
      IF(conduct%simT2)THEN
        CALL calculate_class_atmospheric_forcing(i,slc_owet,radext,cloud,  &
                temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
        !IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution (prec not used)
      ENDIF

      !>Calculate inflow and outflow to wetland
      CALL wetland_watermodel(i,slc_owet,classdata(slc_owet)%soil,basin(i)%subid,classarea, &
                            temp,swrad,soilstate,miscstate,added_inflow,    &
                            wetinflow,conc,owetcatch,wetoutflow,cwetoutflow)

      !>Set output variables of outlet wetland
      IF(conductwb) localwbflows(w_mrtoow,i) = wetinflow * seconds_per_timestep
      IF(conductload)THEN
        localLpathway(:,17) = wetoutflow * cwetoutflow * seconds_per_timestep * 1.E-3  !Load at point P outflow wetland (kg/timestep)
        localLpathway(:,18) = localLpathway(:,17)   !Load into/after outlet lake (no lake), point Q and R
        localLpathway(:,19) = localLpathway(:,17)
      ENDIF
      IF(outvarindex(o_owetvol)>0) localoutvar(i,outvarindex(o_owetvol)) = (frozenstate%snow(slc_owet,i) + SUM(soilstate%water(:,slc_owet,i)))*classarea*1.E-3
      IF(outvarindex(o_wstowet)>0)THEN
        localoutvar(i,outvarindex(o_wstowet)) = - get_wetland_threshold(slc_owet)
        IF((soilstate%water(1,slc_owet,i) - pwmm(1,slc_owet))>=0.) &
        localoutvar(i,outvarindex(o_wstowet)) = localoutvar(i,outvarindex(o_wstowet)) + (soilstate%water(1,slc_owet,i) - pwmm(1,slc_owet))*1.E-3
      ENDIF
      !qcinfli = missing_value  !not needed, used if aolake>0

      !>Update modelled subbasin outflow with flow observations
      updatedoutflow = wetoutflow
      IF(doupdate(i_quseobs)) CALL apply_quseobs(i,updatedoutflow)
      IF(doupdate(i_qar))     CALL apply_qarupd(i,wetoutflow,updatedoutflow,miscstate%updatestationsarcorr(i))
      CALL calculate_branched_flow(i,updatedoutflow,mainflow,branchflow)

  END SUBROUTINE calculate_outlet_wetland

  !>\brief Almost all calculations related to local river.
  !>
  !>Calculate flow into local river and resulting outflow. Calculates processes
  !>related to river wetland, ice, nutrients, and other substances. Calculate
  !>special output, e.g. for loads and waterbalance.
  !>Note that abstraction for irrigation is done outside (before) this subroutine.
  !>
  !>\b Reference ModelDescription Chapter Rivers and lakes (Local river)
  !-------------------------------------------------------------------------------------
  SUBROUTINE calculate_local_river(i,lriverlen,radext,cloud,pobs,cprec,sfdist,incorr,localpcorricep, &  !IN
                                  qriver,criver, &  !INOUT
                                  localeacti,localarea_water,localprec_water,localevap_water, &  !INOUT
                                  frozenstate,miscstate,riverstate,lakestate, &   !INOUT
                                  localoutvar,localwbflows,localwbstores,localLatmdep,localLruralb,localLpathway) !INOUT

    USE MODVAR, ONLY : basin, &
                       classdata, &
                       conduct, &
                       conductload, &
                       conductwb, &
                       deposition, &
                       genpar, &
                       i_pp,i_ss,i_t1,i_t2, &
                       lakedatapar, &
                       lakedataparindex, &
                       landpar, &
                       modeloption, &
                       month, &
                       numsubstances, &
                       nclass, &
                       outvarindex, &
                       outvarstatus, &
                       p_lakeriverice, &
                       p_swtemperature, &
                       seconds_per_timestep, &
                       simulate, &
                       slc_lriver,slc_ilake,slc_mriver,slc_olake
    USE HYPEVARIABLES, ONLY : basincevpcorr, &
                              ttstep,ttpart, &
                              m_wetspload,m_drypp, &
                              m_denitwr,m_denitwrl, &
                              m_hsatINwater, &
                              m_ldwprodn,m_ldwprodp,m_ldwprodc,    &
                              m_hsatTP,m_sedexp,m_limsedpp, &
                              o_ctmp,o_cprc,o_psim,o_rainfall,o_snowfall, &
                              o_epot,o_evap,o_evpt,o_icloss,o_cevapT1, &
                              o_lrfa,o_lred,o_pslr,o_tslr,o_sslr, &
                              o_crgl,o_crnt,o_crpt
    USE HYPE_WATERBALANCE, ONLY : w_rural4,w_piriver,w_eiriver,w_iriver
    USE GENERAL_WATER_CONCENTRATION, ONLY : add_water, &
                                            remove_water, &
                                            error_remove_water
    USE ATMOSPHERIC_PROCESSES, ONLY : calculate_class_atmospheric_forcing, &
                                      calculate_rain_snow_from_precipitation
    USE SOIL_PROCESSES, ONLY : calculate_potential_evaporation
    USE SURFACEWATER_PROCESSES, ONLY : add_precipitation_to_river, &
                                       add_T2_concentration_in_precipitation_on_water,  &
                                       calculate_fractional_riverarea, &
                                       calculate_river_evaporation, &
                                       calculate_river_characteristics, &
                                       get_rivertempvol, &
                                       set_water_temperature, &
                                       T2_processes_in_river, &
                                       ice_processes_in_river, &
                                       translation_in_river
    USE NPC_SOIL_PROCESSES, ONLY : set_class_precipitation_concentration_and_load
    USE NPC_SURFACEWATER_PROCESSES, ONLY : add_deposition_to_river_as_load, &
                                           np_processes_in_river,  &
                                           oc_processes_in_river,  &
                                           add_diffuse_source_to_local_river, &
                                           calculate_river_wetland
    USE TRACER_PROCESSES, ONLY : add_tracer_point_source_to_river,  &
                                 tracer_processes_in_river

    IMPLICIT NONE

    !Argument declarations
      INTEGER, INTENT(IN) :: i  !<current subbasin index
      REAL, INTENT(IN)    :: lriverlen  !<local river length (m)  !Only used to check if river is present
      REAL, INTENT(IN)    :: radext  !<extra terrestrial radiation for subbasin i
      REAL, INTENT(IN)    :: cloud    !<subbasin cloudiness (-)
      REAL, INTENT(IN)    :: pobs !<precipitation original from Pobs.txt (mm) for subbasin i
      REAL, INTENT(IN)    :: cprec(numsubstances)   !concentration of precipitation (pobs), for this subbasin
      REAL, INTENT(IN)    :: sfdist(nclass)  !<snow fall distribution factor for subbasin i
      REAL, INTENT(IN)    :: incorr   !<correction of inorganic nitrogen level by superparameter for subbasin i
      REAL, INTENT(IN)    :: localpcorricep   !<interception evaporation due to negative preccorr-parameter [mm/timestep]
      REAL, INTENT(INOUT) :: qriver  !<flow into/out of local river (m3/s)
      REAL, INTENT(INOUT) :: criver(numsubstances)  !<concentration of flow in local river
      REAL, INTENT(INOUT) :: localeacti   !<accumulation of actual evapotranspiration (mm)
      REAL, INTENT(INOUT) :: localarea_water   !<accumulation of water area fraction (-)
      REAL, INTENT(INOUT) :: localprec_water   !<accumulation of precipitation on water (mm)
      REAL, INTENT(INOUT) :: localevap_water   !<accumulation of actual evapotranspiration on water (mm)
      TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate !<Frozen states
      TYPE(miscstatetype),INTENT(IN)       :: miscstate   !<Misc states
      TYPE(riverstatetype),INTENT(INOUT)   :: riverstate  !<River states
      TYPE(lakestatetype),INTENT(INOUT)    :: lakestate   !<Lake states (is really only IN, but subroutine demande INOUT)
      REAL, INTENT(INOUT),ALLOCATABLE :: localoutvar(:,:)    !<outvar; output for print out
      REAL, INTENT(INOUT),ALLOCATABLE :: localwbflows(:,:)   !<wbflows; flow for water balance output (m3/ts)
      REAL, INTENT(INOUT),ALLOCATABLE :: localwbstores(:,:)   !<wbstores; stores for water balance output (m3)
      REAL, INTENT(INOUT),ALLOCATABLE :: localLatmdep(:,:,:) !<Latmdep; load for print out
      REAL, INTENT(INOUT),ALLOCATABLE :: localLruralb(:)     !<Lruralb; load for print out
      REAL, INTENT(INOUT),ALLOCATABLE :: localLpathway(:,:)  !<Lpathway; load for print out

      !Local variables
      INTEGER status
      INTEGER j   !index of class
      INTEGER riverfreezeupday, riverbreakupday
      REAL a   !area fraction of local river
      REAL ctemp_T2   !T2 temporary memory
      REAL nutrientloadflow  !flow from local diffuse sources (m3/d)
      REAL transq  !flow in local river (m3/s), temporary variable
      REAL transc(numsubstances)  !concentration of flow in local river, temporary variable
      REAL dampq  !flow in local river (m3/ts), temporary variable
      REAL riverQbank,riverdepth,riverareakm2  !used for processes in local river
      REAL lriverarea  !area of local river, estimated or given by indata (m2)
      REAL lrfracarea       !fractional river area (-)
      REAL lreffectivedepth !effective river depth (m)
      REAL atmdepload1(numsubstances),atmdepload1_temp(numsubstances),atmdepload2(numsubstances)  !wet dep (including temp variable used to add dep as load to dep with rain) and dry dep
      REAL rload(numsubstances)   !rural load, needed if localLruralb not allocated, numbsubstances=0
      REAL temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot  !class forcing
      REAL cprecj(numsubstances)   !concentration of precipitation for this class
      REAL snowfall, rainfall  !Precipiation as snow, rain (mm)
      REAL meanrivertemp,totrivervol  !mean temperature and total river volume over all water elements of local river
      REAL epot, epotsnow   !Potential evaporation
      REAL evapr,cevaprT1   !Evaporation
      REAL localriversurftemp(2) !river surface temperature (local and main, only local used here)
      REAL freezeuparea  !freezeuparea for river

      !Local parameters
      INTEGER, PARAMETER :: itype = 1   !change name to ilocal senare
      CHARACTER(LEN=36) :: errstring(1) !error message for location of remove_water call
      PARAMETER (errstring = (/'outflow from local river damping box'/))

      !>\b Algorithm \n
      !>Initiation of river calculations
      lriverarea = 0.
      lrfracarea = missing_value
      lreffectivedepth = missing_value
      localriversurftemp = missing_value
      a = 0.
      j = slc_lriver
      IF(slc_lriver>0) a = classbasin(i,j)%part
      IF(conductload) Lpathway(:,5) = criver(:) * qriver * seconds_per_timestep / 1000.   !Total load in local river, after effect of iwet, point E (kg/timestep)
      IF(simulate%substance(i_t2)) ctemp_T2 = criver(i_t2)  !Keep T2 concentration in memory, in case water is being extracted/added local sources

      !>Add local sources to local river flow
      CALL add_diffuse_source_to_local_river(i,qriver,criver,rload,nutrientloadflow)
      IF(conductload) localLruralb = rload
      IF(conductwb) localwbflows(w_rural4,i) = nutrientloadflow
      IF(conductload) localLpathway(:,6) = criver(:) * qriver * seconds_per_timestep * 1.E-3  !Load after adding rural b (kg/timestep), point F
      IF(simulate%substance(i_t2)) criver(i_t2) = ctemp_T2 !Restore T2 concentration, in case water was added by local sources
      IF(conduct%simT1.OR.conduct%simT2) CALL add_tracer_point_source_to_river(i,itype,qriver,criver)  !T1 and T2 point source
      IF(simulate%substance(i_t2)) ctemp_T2 = criver(i_t2)  !Keep T2 concentration in memory, in case water is being influenced by river wetland

      !>Check for presence of local river. If no river the above flow is directly passed on
      IF(lriverlen>0.)THEN

        !>Calculate river wetland
        IF(conduct%riverwetland)  CALL calculate_river_wetland(i,itype,numsubstances,miscstate%temp5(i),miscstate%temp30(i),qriver,criver,riverstate%cwetland(:,itype,i))
        IF(conductload) localLpathway(:,7) = qriver * criver * seconds_per_timestep * 1.E-3   !Total load after local river wetland (kg/timestep), point G
        IF(simulate%substance(i_t2)) criver(i_t2) = ctemp_T2 !Restore T2 concentration, in case initial T2 of river wetland will influence
        !(later, we should calculate ice and T2 in river wetlands explicitly)

        !>Translation (delay) in river inflow
        CALL translation_in_river(i,itype,qriver,criver,transq,transc,riverstate)

        !Add delayed inflow to river water volume (m3)
        CALL add_water(numsubstances,riverstate%water(itype,i),riverstate%conc(:,itype,i),transq * seconds_per_timestep,transc)

        !Calculate river dimensions, velocity and mean flow for use in substance processes calculation
        CALL calculate_river_characteristics(i,itype,transq,conduct%qbank,riverstate,riverdepth,lriverarea,riverQbank)

        !>Check if local river has been given class area
        !Calculate precipitation, atmospheric deposition and evaporation of local river that has a class area
        IF(a>0)THEN
          lriverarea = a * basin(i)%area       !replace estimated area [m2]
          riverareakm2 = lriverarea*1.E-6      ![km2]
          atmdepload1 = 0.
          atmdepload1_temp = 0.
          atmdepload2 = 0.

          !>\li Forcing data and atmospheric deposition on local river area
          CALL calculate_class_atmospheric_forcing(i,j,radext,cloud,  &
                  temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
          IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution
          CALL set_class_precipitation_concentration_and_load(numsubstances, &
                   riverareakm2,pobs,temp,prec,cprec,cprecj,atmdepload1)
          CALL add_deposition_to_river_as_load(i,itype,classdata(j)%vegtype,month,riverareakm2,   &
              genpar(m_wetspload),landpar(m_drypp,classdata(j)%luse),   &
              deposition,atmdepload1_temp,atmdepload2,riverstate)
          atmdepload1 = atmdepload1 + atmdepload1_temp  !Sum wet dep with rain and as load
          IF(conductload)THEN
            localLatmdep(j,1,:) = localLatmdep(j,1,:) + atmdepload1
            localLatmdep(j,2,:) = localLatmdep(j,2,:) + atmdepload2
          ENDIF
          CALL calculate_rain_snow_from_precipitation(i,classdata(j)%luse,prec,temp,snowfall,rainfall)
          IF(simulate%substance(i_t2))THEN
            CALL get_rivertempvol(i,itype,riverstate,meanrivertemp,totrivervol)  !Get total river water mean T2 temperature
            CALL add_T2_concentration_in_precipitation_on_water(prec,temp,  &    !Set correct T2 temperature concentration in precipitation
                  snowfall,rainfall,meanrivertemp,cprecj(i_t2),frozenstate%rivericecov(itype,i))
          ENDIF

          !>\li Add precipitation with substances to river water
          IF(prec>0) CALL add_precipitation_to_river(i,itype,lriverarea,prec,cprecj,riverstate)

          !Calculate the fractional river area to be used for river evaporation
          CALL calculate_fractional_riverarea(i,itype,lriverarea,riverstate,lrfracarea,lreffectivedepth)

          !>\li Evaporation of river, taking partial ice-cover into account if icemodel is enabled
          CALL calculate_potential_evaporation(i,j,temp,epot,radext,swrad,netrad,actvap,satvap,wind,epotsnow)
          epot = epot * basincevpcorr(i)
          IF(conduct%lakeriverice)  epot = epot * (1. - frozenstate%rivericecov(itype,i))
          IF(epot>0.)THEN
            CALL calculate_river_evaporation(i,j,itype,numsubstances,lriverarea*lrfracarea,temp,epot,evapr,cevaprT1,riverstate)
          ELSE
            evapr = 0.  !current evaporation
          ENDIF

          !>\li Accumulate output variables for mean over subbasin (accumulation for cevapi)
          CALL calculate_class_outvar_add(o_ctmp,i,j,a,temp)
          CALL calculate_class_outvar_add(o_cprc,i,j,a,prec)
          IF(outvarstatus(o_psim)) CALL calculate_class_outvar_add(o_psim,i,j,a,prec+localpcorricep+icpevap)
          CALL calculate_class_outvar_add(o_rainfall,i,j,a,rainfall)
          CALL calculate_class_outvar_add(o_snowfall,i,j,a,snowfall)
          IF(outvarstatus(o_crgl)) CALL calculate_class_outvar_add(o_crgl,i,j,a,swrad)
          IF(outvarstatus(o_crnt)) CALL calculate_class_outvar_add(o_crnt,i,j,a,netrad)
          IF(outvarstatus(o_crpt)) CALL calculate_class_outvar_add(o_crpt,i,j,a,swpot)
          IF(outvarstatus(o_epot)) CALL calculate_class_outvar_add(o_epot,i,j,a,epot*lrfracarea)
          IF(outvarstatus(o_evap)) CALL calculate_class_outvar_add(o_evap,i,j,a,evapr*lrfracarea)
          IF(outvarstatus(o_evpt)) CALL calculate_class_outvar_add(o_evpt,i,j,a,evapr*lrfracarea+localpcorricep+icpevap)
          IF(outvarstatus(o_icloss)) CALL calculate_class_outvar_add(o_icloss,i,j,a,localpcorricep+icpevap)
          localeacti = localeacti + evapr*a*lrfracarea
          IF(simulate%substance(i_t1).AND.outvarstatus(o_cevapT1)) CALL calculate_class_outvar_add_amount(o_cevapT1,i,j,a,cevaprT1*evapr*lrfracarea,cevaprT1)
          !Accumulate outvar for precipitation and evaporation on water (excluding floodplain water)
          localarea_water = localarea_water + a
          localprec_water = localprec_water + (rainfall+snowfall)*a
          localevap_water = localevap_water + evapr * a

          !Set water balance output
          IF(conductwb)THEN
            localwbflows(w_piriver,i) = prec * lriverarea*1.E-3     !m3
            localwbflows(w_eiriver,i) = evapr * lrfracarea * lriverarea*1.E-3
          ENDIF
        ENDIF !(a>0)

        !>Calculate river ice and T2 processes of local river
        IF(modeloption(p_lakeriverice)>0)THEN

          !If no river class area: Assume same precipitation and temperature conditions as for lake class
          IF(a==0)THEN
            IF(j==0) j=slc_ilake
            IF(j==0) j=slc_mriver
            IF(j==0) j=slc_olake
            IF(j==0) j=1
            CALL calculate_class_atmospheric_forcing(i,j,radext,cloud,  &
                  temp,prec,tmin,tmax,swrad,rhmin,actvap,satvap,icpevap,netrad,wind,swpot)
            IF(sfdist(j)/=1.)  prec = prec*sfdist(j)  !adjust class snowfall wind distribution
            CALL calculate_rain_snow_from_precipitation(i,classdata(j)%luse,prec,temp,snowfall,rainfall)
          ENDIF

          !Calculate the fractional river area (updated after evaporation) to be used for river T2 processes
          CALL calculate_fractional_riverarea(i,itype,lriverarea,riverstate,lrfracarea,lreffectivedepth)

          !Calculate river T2 and ice processes
          CALL T2_processes_in_river(i,itype,temp,swrad,localriversurftemp,lriverarea*lrfracarea,frozenstate,riverstate,riverfreezeupday,freezeuparea)
          CALL ice_processes_in_river(i,itype,classdata(j)%luse,snowfall,temp, &
                                        localriversurftemp,lriverarea*lrfracarea,swrad,         &
                                        frozenstate,riverstate,riverfreezeupday,riverbreakupday,freezeuparea)
          IF(modeloption(p_swtemperature)==1) CALL set_water_temperature(itype*2-1,i,riverstate,lakestate)
        ENDIF

        !>Calculate substances processes in river (should they also use fractional river area?)
        CALL np_processes_in_river(i,itype,lriverarea,riverdepth,transq,riverQbank,       &
                (2.-incorr)*genpar(m_denitwr),   &
                (2.-incorr)*genpar(m_denitwrl),genpar(m_hsatINwater), &
                lakedatapar(lakedataparindex(i,itype),m_ldwprodn),    &
                lakedatapar(lakedataparindex(i,itype),m_ldwprodp),    &
                genpar(m_hsatTP),genpar(m_sedexp),genpar(m_limsedpp), &
                genpar(m_muptnriver),genpar(m_muptpriver),genpar(m_muptdepriver),riverstate)
        CALL oc_processes_in_river(i,itype,lriverarea,riverdepth,   &
                lakedatapar(lakedataparindex(i,itype),m_ldwprodc),  &
                genpar(m_hsatTP),genpar(m_limsedpp),riverstate)
        CALL tracer_processes_in_river(i,itype,lriverarea,riverdepth,transq,riverQbank,riverstate)

        !>Calculate outflow of local river and remove from river water volume
        dampq = riverrc(itype,i)*(riverstate%water(itype,i) - deadriver(itype,i))   !m3/ts
        IF(dampq<0.) dampq=0.     !safety
        criver=0.  !safety no substances
        IF(numsubstances>0) criver = riverstate%conc(:,itype,i)
        CALL remove_water(riverstate%water(itype,i),numsubstances,riverstate%conc(:,itype,i),dampq,criver,status)
        IF(status.NE.0) CALL error_remove_water(errstring(1),basin(i)%subid,i,itype)
        IF(conductwb) localwbflows(w_irtomr,i) = dampq  !in case of no ilake
        IF(conductload) localLpathway(:,8) = criver * dampq * 1.E-3   !Load at point C (downstream of local river), point H
        !Set output flow from river
        qriver = dampq / seconds_per_timestep    !m3/s

      ELSE
        !Set some output variables in case of no local river
        IF(conductload)THEN
          localLpathway(:,7) = localLpathway(:,6)  !Total load (kg/timestep), point G
          localLpathway(:,8) = localLpathway(:,6)  !Load at point H
        ENDIF
        IF(conductwb) localwbflows(w_irtomr,i) = qriver * seconds_per_timestep

      ENDIF !river present

      !>Set output variables for local river
      totrivervol = missing_value
      IF(simulate%substance(i_t2))THEN
        IF(outvarindex(178)>0) localoutvar(i,outvarindex(178)) = localriversurftemp(1) !'lrst','comp local river surface temp'
        IF(outvarindex(179)>0)THEN
          CALL get_rivertempvol(i,1,riverstate,meanrivertemp,totrivervol)
          localoutvar(i,outvarindex(179)) = meanrivertemp         !'lrwt','comp local river mean  temp'
        ENDIF
      ENDIF
      IF(outvarindex(186)>0) localoutvar(i,outvarindex(186)) = riverstate%temp(1,i)    !'lrto',local river temp (old water temp model)
      IF(conductwb.OR.outvarindex(114)>0.OR.outvarindex(204)>0)THEN
        IF(totrivervol == missing_value) totrivervol = riverstate%water(1,i) + (SUM(riverstate%qqueue(1:ttstep(1,i),1,i)) + riverstate%qqueue(ttstep(1,i)+1,1,i) * ttpart(1,i))
        IF(outvarindex(114)>0) localoutvar(i,outvarindex(114)) = totrivervol
        IF(outvarindex(204)>0.AND.lriverarea>0) localoutvar(i,outvarindex(204)) = totrivervol/lriverarea   !local river depth [m]
        IF(conductwb)  localwbstores(w_iriver,i) = totrivervol
      ENDIF
      IF(outvarindex(o_lrfa)>0) localoutvar(i,outvarindex(o_lrfa)) = lrfracarea       !local river fractional area (-)
      IF(outvarindex(o_lred)>0) localoutvar(i,outvarindex(o_lred)) = lreffectivedepth !local river effecive depth (m)
      IF(simulate%substance(i_pp))THEN
        IF(outvarindex(o_pslr)>0) localoutvar(i,outvarindex(o_pslr)) = riverstate%Psed(1,i)
      ENDIF
      IF(simulate%substance(i_t1))THEN
        IF(outvarindex(o_tslr)>0) localoutvar(i,outvarindex(o_tslr)) = riverstate%T1sed(1,i)
      ENDIF
      IF(simulate%substance(i_ss))THEN
        IF(outvarindex(o_sslr)>0) localoutvar(i,outvarindex(o_sslr)) = riverstate%Ssed(1,i)
      ENDIF
      IF(modeloption(p_lakeriverice)>0)THEN
        IF(outvarindex(161)>0) localoutvar(i,outvarindex(161)) = frozenstate%riverice(1,i)       !'clri','comp local river ice depth'
        IF(outvarindex(163)>0) localoutvar(i,outvarindex(163)) = frozenstate%riverbice(1,i)      !'clrb','comp local river blackice depth'
        IF(outvarindex(165)>0) localoutvar(i,outvarindex(165)) = frozenstate%riversnowdepth(1,i) !'clrs','comp local river snow depth'
        IF(outvarindex(251)>0) localoutvar(i,outvarindex(251)) = frozenstate%rivericecov(1,i)    !'clic','comp local river ice cover'
      ENDIF

  END SUBROUTINE calculate_local_river

  END MODULE
