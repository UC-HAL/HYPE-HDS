!> \file soilmodel5.f90
!> Contains module soilmodel_traveltime.

!>HYPE substance simplified soil model based on travel time through soil 
!>and half life of pollutants to simulate losses in soil.
MODULE SOILMODEL_TRAVELTIME

  !Copyright 2018-2020 SMHI
  !
  !This file is part of HYPE.

  !HYPE is free software: you can redistribute it and/or modify it under
  !the terms of the Lesser GNU General Public License as published by
  !the Free Software Foundation, either version 3 of the License, or (at
  !your option) any later version. HYPE is distributed in the hope that
  !it will be useful, but WITHOUT ANY WARRANTY; without even the implied
  !warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  !the Lesser GNU General Public License for more details. You should
  !have received a copy of the Lesser GNU General Public License along
  !with HYPE. If not, see <http://www.gnu.org/licenses/>.

  !-----------------------------------------------------------------------------------------
  !Procedures used in this module
  !-----------------------------------------------------------------------------------------

  USE STATETYPE_MODULE
  USE MODVAR,  ONLY: missing_value,pi,   &
                     currentdate, &
                     timesteps_per_day, &
                     genpar,landpar,soilpar,  &
                     load,basin,path, &
                     numsubstances,nsub,maxsoillayers, &
                     classdata,soilthick,soildepth, &
                     i_t1,i_t2,i_in,i_on,i_sp,i_pp,i_oc,i_ae,i_ss,&
                     modeloption,p_deepgroundwater,p_infiltration, &
                     p_snowheat, &
                     get_current_soilleakage_memory, &
                     conduct,simulate, &
                     simulatesubstances
  USE HYPEVARIABLES, ONLY : wpmm,fcmm,epmm,pwmm, &
                            m_srrcs, &
                            m_drypp,m_cfrost,m_sfrost,m_perc1,m_perc2,  &
                            m_sswcorr,m_immdep,m_iwdfrac,m_wdpar, &
                            soilmem,m_deepmem, &
                            epotdist
  USE GENERAL_FUNCTIONS, ONLY : exponential_decay,sedimentation
  USE GENERAL_WATER_CONCENTRATION, ONLY : remove_water, &
                                          error_remove_water, &
                                          add_source_to_water
  USE ATMOSPHERIC_PROCESSES, ONLY :  calculate_rain_snow_from_precipitation
  USE SOIL_PROCESSES, ONLY : calculate_snow, &
                             calculate_potential_evaporation,          &
                             calculate_actual_soil_evapotranspiration, &
                             add_macropore_flow,    &
                             calculate_tile_drainage,  &
                             calculate_soil_runoff,    &
                             calculate_infiltration_flow_diversion, &
                             add_infiltration,   &
                             percolation, &
                             calculate_groundwater_table,  &
                             calculate_soiltemp,   &
                             calculate_frostdepth,  &
                             calculate_unfrozen_soil_water, &
                             calculate_liquid_water_fraction, &
                             calculate_soil_moisture_deficit
  USE NPC_SOIL_PROCESSES, ONLY : add_dry_deposition_to_landclass,  &
                                 local_diffuse_source
  USE TRACER_PROCESSES, ONLY : set_soil_T2_from_soiltemp_model
  USE IRRIGATION_MODULE, ONLY : apply_irrigation, &
                                calculate_irrigation_water_demand
  USE REGIONAL_GROUNDWATER_MODULE, ONLY : add_regional_groundwater_flow_to_soil

  IMPLICIT NONE
  PRIVATE
  !-------------------------------------------------
  ! Private procedures in this module
  !-------------------------------------------------
  !soil_traveltime_processes 
  !decay_of_simplesubstance
  !decay_of_simplesubstance_sorbedphase 
  !left_after_decay
  !release_from_pool
  !distribute_soil_load
  !-------------------------------------------------
  PUBLIC :: soilmodel_5
  
  !Private parameters, global in this module
  CHARACTER(LEN=27) :: errstring(1)  !error message for location of remove_water call
  PARAMETER (errstring = (/'surface runoff, soillayer 1'/))

CONTAINS

  !----------------------------------------------------------------
  !>\brief Default soilmodel for land classes
  !!Calculate snow and soil processes for a land class. 
  !----------------------------------------------------------------
  SUBROUTINE soilmodel_5(i,j,isoil,iluse,subid,dayno,classarea,prec,cprec,temp, & 
       daylength,mintemp,maxtemp,swrad,  &
       radext,netrad,actvap,satvap,wind,rrcscorr,cevpcorr,  &
       frozenstate,soilstate,surfaceflow,csrunoff,crunoffd,   &
       cropuptakein,nitrif,denitrif,epot,gwat,frostdepth,    &
       smdef,evap,cevap,crunoff1,crunoff2,crunoff3,  &
       pwneed,irrappl,irrsources,snowfall,rainfall,cropsources,ruralaload,rgrwload,infiltrationflows,evapflows,  &
       runofflows,verticalflows,cverticalflows,horizontalflows,horizontalflows2,evapsnow,cruralflow,snowtemp,snowsurftemp)  

    INTEGER, INTENT(IN) :: i        !<index for current subbasin
    INTEGER, INTENT(IN) :: j        !<index for current class 
    INTEGER, INTENT(IN) :: isoil    !<index of soil type
    INTEGER, INTENT(IN) :: iluse    !<index of landuse
    INTEGER, INTENT(IN) :: subid    !<subbasin id
    INTEGER, INTENT(IN) :: dayno    !<pseudo dayno for use in soil model subroutines
    REAL, INTENT(IN) :: classarea   !<class area [km2]
    REAL, INTENT(IN) :: prec        !<precipitation (mm/timestep)
    REAL, INTENT(IN) :: cprec(numsubstances)        !<concentration of precipitation
    REAL, INTENT(IN) :: temp        !<temperature
    REAL, INTENT(IN) :: daylength   !<day length (hours)
    REAL, INTENT(IN) :: mintemp     !<current daily min temperature (C)
    REAL, INTENT(IN) :: maxtemp     !<current daily max temperature (C)
    REAL, INTENT(IN) :: swrad       !<downward shortwave radiation [MJ/m2/day]
    REAL, INTENT(IN) :: radext      !<extraterrestrial solar radiation [MJ/m2/day]
    REAL, INTENT(IN) :: netrad      !<net downward radiation [MJ/m2/day]
    REAL, INTENT(IN) :: actvap      !<actual vapor pressure [kPa]
    REAL, INTENT(IN) :: satvap      !<saturated vapour pressure [kPa]
    REAL, INTENT(IN) :: wind        !<wind speed [m/s]
    REAL, INTENT(IN) :: rrcscorr    !<correction of recession coefficients
    REAL, INTENT(IN) :: cevpcorr    !<correction of potential evaporation
    TYPE(snowicestatetype),INTENT(INOUT)  :: frozenstate   !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    REAL, INTENT(OUT) :: surfaceflow(2)  !<saturated overflow and surface excess infilt
    REAL, INTENT(OUT) :: csrunoff(numsubstances)   !<concentration surface flow
    REAL, INTENT(OUT) :: crunoffd (numsubstances)  !<concentration tile runoff
    REAL, INTENT(OUT) :: cropuptakein  !<crop uptake of IN      
    REAL, INTENT(OUT) :: nitrif     !<nitrification
    REAL, INTENT(OUT) :: denitrif(maxsoillayers)   !<denitrification
    REAL, INTENT(OUT) :: epot       !<potential evaporation (mm/timestep)
    REAL, INTENT(OUT) :: gwat       !<groundwater table (m)
    REAL, INTENT(OUT) :: frostdepth   !<soil frost depth 
    REAL, INTENT(OUT) :: smdef        !<soil moisture deficit (mm)
    REAL, INTENT(OUT) :: evap       !<evapotranspiration (mm) weighted sum of evap(snowfree)+evapsnow
    REAL, INTENT(OUT) :: cevap(numsubstances)   !<concentration of evapotranspiration
    REAL, INTENT(OUT) :: crunoff1(numsubstances)   !<concentration of runoff from soil layer 1 (mg/L)
    REAL, INTENT(OUT) :: crunoff2(numsubstances)   !<concentration of runoff from soil layer 2 (mg/L)
    REAL, INTENT(OUT) :: crunoff3(numsubstances)   !<concentration of runoff from soil layer 3 (mg/L)
    REAL, INTENT(OUT) :: pwneed                    !<irrigation water demand for this classe (m3)
    REAL, INTENT(INOUT) :: irrappl   !<applied irrigation (mm), for summation basin output
    REAL, INTENT(INOUT) :: irrsources(numsubstances)     !<Load from irrigation to soil (kg/timestep)
    REAL, INTENT(OUT) :: snowfall     !<Precipitation as rain (mm)
    REAL, INTENT(OUT) :: rainfall     !<Precipitation as snow (mm)
    REAL, INTENT(OUT) :: cropsources(2,numsubstances)  !<Load from (-) and excess load files (kg/timestep)
    REAL, INTENT(OUT) :: ruralaload(numsubstances)   !<Load from rural households (kg/timestep)
    REAL, INTENT(INOUT) :: rgrwload(numsubstances)     !<Load from regional groundwater flow to soil (kg/timestep)
    REAL, INTENT(OUT) :: infiltrationflows(7)  !<several infiltration flows [mm]
    REAL, INTENT(OUT) :: evapflows(4)  !<evaporation from soillayers, snow and (glacier) [mm]
    REAL, INTENT(OUT) :: runofflows(7) !<different runoff flows:1-3=soil runoff sl 1-3,4-6=tile runoff sl 1-3,7=saturated surface runoff
    REAL, INTENT(OUT) :: verticalflows(6) !<vertical flows:1-2=percolation,3-4=upwelling due to rural,5-6=upwelling due to reg. grw flows
    REAL, INTENT(OUT) :: cverticalflows(2,numsubstances) !<concentration of vertical flows:1-2=percolation
    REAL, INTENT(OUT) :: horizontalflows(3)  !<horizontal flows:1-3=recieved rural load flow
    REAL, INTENT(INOUT) :: horizontalflows2(3,nsub) !<horizontal flows:1-3=division of regional groundwater flows to grwdown
    REAL, INTENT(OUT) :: evapsnow !<actual evaporation from snow
    REAL, INTENT(OUT) :: cruralflow(numsubstances) !<concentration of rural flow
    REAL, INTENT(OUT) :: snowtemp !<snowpack temperature
    REAL, INTENT(OUT) :: snowsurftemp !<snow surface temperature
    
    !Local variables
    INTEGER inc      !loopvar, numsubstances
    INTEGER status  !error status of subroutine call
    !REAL tt       !threshold temperature for melting (C)
    !REAL cm       !coefficient for snow melt (mm/Cday)
    REAL sc       !coefficient for runoff recession surface runoff(no unit)
!    REAL helpmm

    !Variables for class values
    REAL ginfilt,infilt   !gross infiltration (rain+melt), actual infiltration (after removed surfaceflow and macroporeflow)
    REAL cginfilt(numsubstances),cinfilt(numsubstances)   !concentration of infiltration
    REAL totalsurfaceflow
    REAL satoverflow    !surface flow from saturated soil
    REAL excessinfilt   !infiltration excess surface runoff 
    REAL macroflow
    REAL cmacroflow(numsubstances), cexcessinfilt(numsubstances)          !concentration in infiltration excess runoff and macropore flow
    REAL melt, cmelt(numsubstances)      !snow melt,concentration in snow melt water
    REAL cevapsnow(numsubstances) !concentration of snow evaporation
    REAL soilrunoff(maxsoillayers)    !soil runoff
    REAL csoilrunoff(numsubstances,maxsoillayers)    !concentration of soil runoff
    REAL cweights(maxsoillayers) ! weigths to calculate T2 conc. in tile drainage 
    REAL effsnowcov       !effective snow cover with respect to evaporation evap = evap(snowfree)*(1-effsnowcov)+evapsnow
    REAL epotsnow         !potential evaporation from snow
    REAL release(numsubstances) !release of substance from surface pool
    REAL runoffd      !tile drainage runoff
    REAL soilwater(maxsoillayers) !soilwater temporary variable
    REAL snowheat     !snow heat content, temporary variable
    REAL liqfrac(maxsoillayers) !liquid fraction of soil water
    REAL frozenvol(maxsoillayers) !<Frozen water volume (mm)
    REAL snowrunoff !<Runoff from snow layer, may be smaller than snowmelt!
    REAL xload(numsubstances)
   
    !Local constants
    LOGICAL, PARAMETER :: no_perc_red = .TRUE.   !turn off concentration reduction during percolation
    LOGICAL, PARAMETER :: no_conc_infilt_transf = .TRUE.   !turn off IN concentration transformation during infiltration

    !Output, default values
    infiltrationflows = 0.
    runofflows = 0.
    evapflows = 0.
    verticalflows=0.
    cverticalflows=0.
    nitrif = 0.; denitrif=0.
    cropuptakein = 0.
    
    !Short notation of parameter values to be used in this subroutine
    !tt=landpar(m_ttmp,iluse)       !Threshold temperature for snow melt and evaporation
    !cm=landpar(m_cmlt,iluse)       !Coefficient for snow melt

    !Locally defined variables for indata to be used in this subroutine
    liqfrac = 1.
    frozenvol = 0.
    release = 0.

    !Get soil leakage load (include atmospheric deposition) and distribute it to the soil pools
    xload = get_current_soilleakage_memory(j,i,numsubstances)      !TODO: units? kg/km2
    CALL distribute_soil_load(i,j,numsubstances,xload,soilstate)
    cropsources(1,:)=0.
    cropsources(2,:)=xload*classarea  !kg/ts

    !Irrigate the soil, OK  
    IF(conduct%irrigation) CALL apply_irrigation(i,j,soilstate,irrappl,irrsources)

    !Potential evapotranspiration (before snow calculations, to calculate snow evaporation), OK
    CALL calculate_potential_evaporation(i,j,temp,epot,radext,swrad,netrad,actvap,satvap,wind,epotsnow)
    epot = epot * cevpcorr          !PET for snow free soil, regionally adjusted
    epotsnow = epotsnow * cevpcorr  !PET for snow covered soil, regionally adjusted
    
    !Update snow pack; snowfall, melting, evaporation (sublimation), OK for now
    CALL calculate_rain_snow_from_precipitation(i,iluse,prec,temp,snowfall,rainfall) !form of precipitation
    snowheat = 0.
    IF(modeloption(p_snowheat)>=1) snowheat = frozenstate%snowheat(j,i)
    CALL calculate_snow(i,j,basin(i)%subid,iluse,snowfall,cprec,frozenstate%snow(j,i),   &
              frozenstate%csnow(:,j,i),temp,melt,cmelt,swrad, &
              frozenstate%snowage(j,i),frozenstate%snowmax(j,i),frozenstate%snowdepth(j,i),  &
              frozenstate%snowcov(j,i),snowheat,epotsnow,evapsnow,cevapsnow,effsnowcov,snowtemp,snowsurftemp,frozenstate%snowliq(j,i),snowrunoff)
    evapflows(3) = evapsnow
    IF(modeloption(p_snowheat)>=1) frozenstate%snowheat(j,i) = snowheat
    
    !Gross infiltration, OK
    ginfilt  = rainfall + snowrunoff 
    IF(simulatesubstances)THEN
      IF(ginfilt>0.)THEN
        cginfilt = (cmelt*snowrunoff + cprec*rainfall) / ginfilt
      ELSE 
        cginfilt = 0.
      ENDIF
    ENDIF

    !Calculate soil temperature and frost depth, OK
    CALL calculate_soiltemp(maxsoillayers,temp,frozenstate%snowdepth(j,i),genpar(m_deepmem),soilmem(:,j),soilstate%deeptemp(j,i),soilstate%temp(:,j,i))
    IF(soilthick(3,j)>0)THEN
      CALL calculate_frostdepth(fcmm(:,j),wpmm(:,j),landpar(m_cfrost,iluse),soilpar(m_sfrost,isoil),   &
            soilstate%water(1,j,i)+soilstate%water(2,j,i)+soilstate%water(3,j,i),frostdepth,soilstate%temp(1:2,j,i),soilthick(:,j))
    ELSEIF(soilthick(2,j)>0)THEN
      CALL calculate_frostdepth(fcmm(:,j),wpmm(:,j),landpar(m_cfrost,iluse),soilpar(m_sfrost,isoil),   &
            soilstate%water(1,j,i)+soilstate%water(2,j,i),frostdepth,soilstate%temp(1:2,j,i),soilthick(:,j))
    ELSE
      CALL calculate_frostdepth(fcmm(:,j),wpmm(:,j),landpar(m_cfrost,iluse),soilpar(m_sfrost,isoil),   &
            soilstate%water(1,j,i),frostdepth,soilstate%temp(1:1,j,i),soilthick(:,j))
    ENDIF

    !Initial calculation of liquid water fraction and frozen water volume
    CALL calculate_unfrozen_soil_water(i,j,isoil,temp,wpmm(:,j),fcmm(:,j),epmm(:,j),soilstate,frozenvol,liqfrac)

    IF(modeloption(p_infiltration)==0 .OR. modeloption(p_infiltration)==1)THEN
      !Calculate and add infiltration to soil, including calculation of surface flow and macropore flow due to limited infiltration capacity, OK
      CALL calculate_infiltration_flow_diversion(i,j,isoil,wpmm(:,j),fcmm(:,j),epmm(:,j),ginfilt,cginfilt,temp,mintemp,maxtemp,  &
         infilt,cinfilt,excessinfilt,cexcessinfilt,macroflow,cmacroflow,frozenstate,soilstate)
      CALL add_infiltration(i,j,iluse,infilt,cinfilt,soilstate,no_conc_infilt_transf)
      IF(ginfilt>0.)THEN
        infiltrationflows(1) = snowrunoff/ginfilt
      ENDIF
      infiltrationflows(2) = infilt
      infiltrationflows(3) = excessinfilt
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    
      !Percolation down through the soil layers, OK
      CALL percolation(i,j,isoil,subid,wpmm(:,j),fcmm(:,j),pwmm(:,j),soilthick(:,j),liqfrac,verticalflows(1:2),cverticalflows,soilstate,no_perc_red)
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF

    !Calculate and remove evapotranspiration from the soil upper two layers, OK for now
    !Will maybe add parameters nnevap for other substances later (all soilmodels)
    soilwater = soilstate%water(:,j,i) !set soilwater variable to be used for evapotranspiration, soil runoff and tile runoff
    CALL calculate_actual_soil_evapotranspiration(i,j,2,soilwater,temp,epot, &
            wpmm(:,j),fcmm(:,j),epotdist(:,j),MIN(1.,MAX(1.-effsnowcov,0.)), &
            liqfrac,soilstate,evap,evapflows(1:2),cevap)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac

    !Total evaporation, and Weighted average concentrations and potential evapotranspiration,OK
    evap = evap + evapsnow
    IF(numsubstances.GT.0 .AND. evap.GT.0.) cevap(:) = (cevap(:)*(evap-evapsnow) + cevapsnow(:)*evapsnow)/evap
    epot = epot * (1.-effsnowcov) + epotsnow * effsnowcov
    
    !Calculate and remove soil runoff, OK
    CALL calculate_soil_runoff(i,j,isoil,subid,soilwater,epmm(:,j), &
              classdata(j)%streamdepth,liqfrac,soilstate,soilrunoff,csoilrunoff)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    runofflows(1:3) = soilrunoff(1:3)
    crunoff1 = csoilrunoff(:,1)
    crunoff2 = csoilrunoff(:,2)
    crunoff3 = csoilrunoff(:,3)

    !Calculate and remove runoff by tile or drainage pipe,OK
    CALL calculate_tile_drainage(i,j,isoil,subid,soilwater,epmm(:,j),   &
              soildepth(:,j),soilthick(:,j),classdata(j)%tiledepth,rrcscorr,&
              liqfrac,soilstate,runoffd, crunoffd,cweights)
    runofflows(4:6) = runoffd*cweights(1:3)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac

    !Regional lateral groundwater flow from other subbasins, OK
    IF(modeloption(p_deepgroundwater)==1) CALL add_regional_groundwater_flow_to_soil(i,j,classarea,pwmm(:,j),soilstate,rgrwload,verticalflows(5:6),horizontalflows2)
    
    !Load from local diffuse NP-source to the lowest soil layer, all added as dissolved nutrients for soilmodel5
    CALL local_diffuse_source(i,j,pwmm(:,j),classarea,soilstate,ruralaload,verticalflows(3:4),horizontalflows(1:3),cruralflow) !,.TRUE.)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    
    IF(modeloption(p_infiltration)==2 .OR. modeloption(p_infiltration)==3)THEN
      !Calculate and add infiltration to soil, including calculation of surface flow and macropore flow due to limited infiltration capacity, OK
      CALL calculate_infiltration_flow_diversion(i,j,isoil,wpmm(:,j),fcmm(:,j),epmm(:,j),ginfilt,cginfilt,temp,mintemp,maxtemp,  &
         infilt,cinfilt,excessinfilt,cexcessinfilt,macroflow,cmacroflow,frozenstate,soilstate)
      CALL add_infiltration(i,j,iluse,infilt,cinfilt,soilstate,no_conc_infilt_transf)
      IF(ginfilt>0.)THEN
        infiltrationflows(1) = snowrunoff/ginfilt
      ENDIF
      infiltrationflows(2) = infilt
      infiltrationflows(3) = excessinfilt
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF
    
    IF(modeloption(p_infiltration)==2 .OR. modeloption(p_infiltration)==3)THEN
      !Percolation down through the soil layers, OK
      CALL percolation(i,j,isoil,subid,wpmm(:,j),fcmm(:,j),pwmm(:,j),soilthick(:,j),liqfrac,verticalflows(1:2),cverticalflows,soilstate,no_perc_red)
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF

    !Surface runoff from saturated overland flow of uppermost soil layer, OK
    sc=landpar(m_srrcs,iluse)*rrcscorr      !Runoff coefficient for surface runoff
    IF(sc>1.) sc = 1.
    satoverflow = MAX(sc * (soilstate%water(1,j,i)-pwmm(1,j)),0.)
    IF(satoverflow > 0.) THEN
      CALL remove_water(soilstate%water(1,j,i),numsubstances,soilstate%conc(:,1,j,i),satoverflow,soilstate%conc(:,1,j,i),status)
      IF(status.NE.0) CALL error_remove_water(errstring(1),subid,i,j)
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF
    runofflows(7) = satoverflow

    !Total surfaceflow (saturated overland flow and excess infiltration), OK
    csrunoff = 0.
    totalsurfaceflow = satoverflow + excessinfilt
    surfaceflow(1) = satoverflow
    surfaceflow(2) = excessinfilt
    IF(totalsurfaceflow > 0. .AND. simulatesubstances) THEN
       csrunoff(:) = (soilstate%conc(:,1,j,i) * satoverflow + cexcessinfilt(:) * excessinfilt) / totalsurfaceflow     !used for satoverflow and excessinfilt
    ENDIF

    !Add macropore water to soil layer with groundwater level, OK
    CALL add_macropore_flow(i,j,macroflow,cmacroflow,epmm(:,j),pwmm(:,j), &
            soildepth(:,j),soilthick(:,j),infiltrationflows(4:6),soilstate)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac

    IF(modeloption(p_infiltration)==0 .OR. modeloption(p_infiltration)==1)THEN
      !Second percolation down through the soil layers (limited to same maxperc), OK
      CALL percolation(i,j,isoil,subid,wpmm(:,j),fcmm(:,j),pwmm(:,j),soilthick(:,j),liqfrac,verticalflows(1:2),cverticalflows,soilstate,no_perc_red)
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF

    !Groundwater level and soil moisture deficit for output variable, OK
    CALL calculate_groundwater_table(j,soilstate%water(:,j,i),epmm(:,j),  &
                                     soildepth(:,j),soilthick(:,j),gwat) 
    CALL calculate_soil_moisture_deficit(soilstate%water(:,j,i),wpmm(:,j),  &
            fcmm(:,j),soilthick(:,j),smdef) 

    !Soil transformation processes for substances
    DO inc = 1,numsubstances
      CALL soil_traveltime_processes(i,j,iluse,inc,soilthick(:,j),soilstate,ginfilt,release(inc))
    ENDDO

    !Add released substance to infiltrating water and surface runoff, OK?
    IF(totalsurfaceflow>0.)THEN
      csrunoff = csrunoff + (release * (totalsurfaceflow/(infilt+totalsurfaceflow))) / totalsurfaceflow
    ENDIF
    IF(infilt+totalsurfaceflow>0.) CALL add_source_to_water(soilstate%water(1,j,i),numsubstances,soilstate%conc(:,1,j,i),release*(infilt/(infilt+totalsurfaceflow)))
    
    !Calculate irrigation water demand (for next timestep), OK
    IF(conduct%irrigation) CALL calculate_irrigation_water_demand(i,j,  &
         classarea,genpar(m_sswcorr),genpar(m_immdep),genpar(m_iwdfrac),  &
         genpar(m_wdpar),soilstate%water(:,j,i),wpmm(:,j),fcmm(:,j),  &
         epmm(:,j),epot,pwneed)

    !Runoff T2 temperature dependent on soiltemp calculation [DG/JS Temp.model, May-2013], OK
    IF(simulate%substance(i_t2)) &
      CALL set_soil_T2_from_soiltemp_model(i,j,satoverflow,excessinfilt,cexcessinfilt,cweights,& 
                                             crunoff1,crunoff2,crunoff3,csrunoff,crunoffd,soilstate)

   END SUBROUTINE soilmodel_5

  !>Calculate substance processes in soil by traveltime soil model
  !
  !>\b Reference ModelDescription Nitrogen and phosphorus in land routines 
  !>(Nutrient soil leakage from outer source)
  !------------------------------------------------------------------------------
  SUBROUTINE soil_traveltime_processes(i,j,iluse,subst,thickness,soilstate,infilt,release)    

    USE MODVAR, ONLY : maxsoillayers
        
    INTEGER, INTENT(IN) :: i                        !<index of subbasin 
    INTEGER, INTENT(IN) :: j                        !<index of class
    INTEGER, INTENT(IN) :: iluse                    !<index of landuse
    INTEGER, INTENT(IN) :: subst                    !<index of substance
    REAL, INTENT(IN)    :: thickness(maxsoillayers) !<thickness of soil layers
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate !<Soil states
    REAL, INTENT(IN)    :: infilt                   !<gross infiltration (snowmelt and precipitation)
    REAL, INTENT(OUT)   :: release                  !<release from surface pool
   
      !T2 is not handled by traveltime processes
      IF(subst==i_t2)RETURN
    
      !Decay of substance dissolved in soil water
      CALL decay_of_simplesubstance(j,iluse,1,subst,soilstate%water(1,j,i),soilstate%conc(:,1,j,i)) 
      IF(thickness(2)>0) CALL decay_of_simplesubstance(j,iluse,2,subst,soilstate%water(2,j,i),soilstate%conc(:,2,j,i))
      IF(thickness(3)>0) CALL decay_of_simplesubstance(j,iluse,3,subst,soilstate%water(3,j,i),soilstate%conc(:,3,j,i))
      
      !Decay of substance in surface pool
      CALL decay_of_simplesubstance_sorbedphase(j,iluse,subst,soilstate%surface(subst,j,i)) 
      
      !Release of surface pool by rain/snowmelt (gross infiltration)
      release = 0.
      IF(infilt>0. .AND. soilstate%surface(subst,j,i)>0.) CALL release_from_pool(subst,soilstate%surface(subst,j,i),infilt,release)
      
  END SUBROUTINE soil_traveltime_processes

  !>Decay processes of simple substance in dissolved phase
  !>
  !>\b Reference ModelDescription Nitrogen and phosphorus in land routines 
  !>(Nutrient soil leakage from outer source)
  !-----------------------------------------------------------------
  SUBROUTINE decay_of_simplesubstance(j,iluse,soillayer,subst,vol,conc)

    USE HYPEVARIABLES, ONLY : m_t1decay,m_indecay,m_ondecay,m_spdecay, &
                              m_ppdecay,m_ocdecay,m_ssdecay,m_aedecay, &
                              m_intot,m_ontot,m_sptot,m_pptot,m_octot, &
                              m_sstot,m_aetot,m_t1tot
    !Argument declarations
    INTEGER, INTENT(IN) :: j                    !<index of class
    INTEGER, INTENT(IN) :: iluse                !<index of landuse
    INTEGER, INTENT(IN) :: soillayer            !<soil layer (1-3)
    INTEGER, INTENT(IN) :: subst                !<index of substance
    REAL, INTENT(IN)    :: vol                  !<volume of water 
    REAL, INTENT(INOUT) :: conc(numsubstances)  !<concentration of water (-)
    
    !Local variables
    INTEGER timesteps  !time for decay [timesteps]
    REAL left

      timesteps = 1
      IF(vol>0. .AND. conc(subst)>0.)THEN
        left = 1. !T2
        IF(subst==i_t1) CALL left_after_decay(timesteps,soillayer,genpar(m_t1decay),landpar(m_t1tot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_in) CALL left_after_decay(timesteps,soillayer,genpar(m_indecay),landpar(m_intot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_on) CALL left_after_decay(timesteps,soillayer,genpar(m_ondecay),landpar(m_ontot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_sp) CALL left_after_decay(timesteps,soillayer,genpar(m_spdecay),landpar(m_sptot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_pp) CALL left_after_decay(timesteps,soillayer,genpar(m_ppdecay),landpar(m_pptot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_oc) CALL left_after_decay(timesteps,soillayer,genpar(m_ocdecay),landpar(m_octot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_ss) CALL left_after_decay(timesteps,soillayer,genpar(m_ssdecay),landpar(m_sstot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_ae) CALL left_after_decay(timesteps,soillayer,genpar(m_aedecay),landpar(m_aetot,iluse),classdata(j)%traveltime,left)
        conc(subst) = conc(subst) * left
       
      ENDIF
    
  END SUBROUTINE decay_of_simplesubstance
  
  !>Decay of simple substance in sorbed or particulate phase
  !>
  !>\b Reference ModelDescription Nitrogen and phosphorus in land routines 
  !>(Nutrient soil leakage from outer source)
  !--------------------------------------------------------------------
  SUBROUTINE decay_of_simplesubstance_sorbedphase(j,iluse,subst,pool)

    USE HYPEVARIABLES, ONLY : m_t1decay,m_indecay,m_ondecay,m_spdecay, &
                              m_ppdecay,m_ocdecay,m_ssdecay,m_aedecay, &
                              m_intot,m_ontot,m_sptot,m_pptot,m_octot, &
                              m_sstot,m_aetot,m_t1tot

    !Argument declarations
    INTEGER, INTENT(IN) :: j                    !<index of class
    INTEGER, INTENT(IN) :: iluse                !<index of landuse
    INTEGER, INTENT(IN) :: subst                !<index of substance
    REAL, INTENT(INOUT) :: pool                 !<amount of substance
    
    !Local variables
    INTEGER timesteps  !time for decay [timesteps]
    INTEGER soillayer
    REAL left

      !Exponential decay with half time parameter during one time step
      IF(pool>0.)THEN
        timesteps = 1
        left = 1. !T2
        soillayer = 0
        IF(subst==i_t1) CALL left_after_decay(timesteps,soillayer,genpar(m_t1decay),landpar(m_t1tot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_in) CALL left_after_decay(timesteps,soillayer,genpar(m_indecay),landpar(m_intot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_on) CALL left_after_decay(timesteps,soillayer,genpar(m_ondecay),landpar(m_ontot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_sp) CALL left_after_decay(timesteps,soillayer,genpar(m_spdecay),landpar(m_sptot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_pp) CALL left_after_decay(timesteps,soillayer,genpar(m_ppdecay),landpar(m_pptot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_oc) CALL left_after_decay(timesteps,soillayer,genpar(m_ocdecay),landpar(m_octot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_ss) CALL left_after_decay(timesteps,soillayer,genpar(m_ssdecay),landpar(m_sstot,iluse),classdata(j)%traveltime,left)
        IF(subst==i_ae) CALL left_after_decay(timesteps,soillayer,genpar(m_aedecay),landpar(m_aetot,iluse),classdata(j)%traveltime,left)
        pool = pool * left
        
      ENDIF
    
  END SUBROUTINE decay_of_simplesubstance_sorbedphase  

  !>Fraction left of substance after decay processes
  !>
  !>\b Reference ModelDescription Nitrogen and phosphorus in land routines 
  !>(Nutrient soil leakage from outer source)
  !-----------------------------------------------------------------
  SUBROUTINE left_after_decay(timesteps,soillayer,decaypar,totluse,totslc,fractionleft)

    USE HYPEVARIABLES, ONLY : m_totref,m_totexp0,m_totexp1,m_totexp2,m_totexp3

    !Argument declarations
    INTEGER, INTENT(IN) :: timesteps            !<time in number of timesteps
    INTEGER, INTENT(IN) :: soillayer            !<soil layer (1-3)
    REAL, INTENT(IN)    :: decaypar             !<decay parameter [half life in days]
    REAL, INTENT(IN)    :: totluse              !<time of travel landuse scaling parameter [days]
    REAL, INTENT(IN)    :: totslc               !<time of travel scaling parameter of class (used if no totpar set) [days]
    REAL, INTENT(OUT)   :: fractionleft         !<fraction remaining after decay
    
    !Local variables
    REAL ts
    REAL exppar !exponent of time of travel dependence of decay
    REAL localtotref

    localtotref = genpar(m_totref)
    IF(localtotref==0) localtotref = 1. !Default
    fractionleft = 1.
    IF(decaypar>0)THEN
      exppar = 0.
      IF(soillayer==0.AND.genpar(m_totexp0)>0.) exppar = genpar(m_totexp0)
      IF(soillayer==1.AND.genpar(m_totexp1)>0.) exppar = genpar(m_totexp1)
      IF(soillayer==2.AND.genpar(m_totexp2)>0.) exppar = genpar(m_totexp2)
      IF(soillayer==3.AND.genpar(m_totexp3)>0.) exppar = genpar(m_totexp3)
      ts = REAL(timesteps) / REAL(timesteps_per_day)  !time in days
      IF(exppar<=0.)THEN
        fractionleft = exponential_decay(ts,decaypar)
      ELSEIF(totluse>0.)THEN
        fractionleft = exponential_decay(ts,decaypar*(localtotref/totluse)**exppar)
      ELSEIF(totslc>0.)THEN
        fractionleft = exponential_decay(ts,decaypar*(localtotref/totslc)**exppar)
      ENDIF
    ENDIF       
    
  END SUBROUTINE left_after_decay
  
  !>Release from surface pool on ground due to rain/snowmelt or surface runoff
  !
  !>\b Reference ModelDescription Nitrogen and phosphorus in land routines 
  !>(Nutrient soil leakage from outer source)
  !---------------------------------------------------------------------------------------
  SUBROUTINE release_from_pool(subst,pool,qmm,release)
      
    USE HYPEVARIABLES, ONLY : m_inrel,m_onrel,m_sprel,m_pprel,m_ocrel, &
                              m_ssrel,m_aerel,m_t1rel

    !Argument declarations
    INTEGER, INTENT(IN) :: subst     !<index of substance
    REAL, INTENT(INOUT) :: pool      !<surface pool of substance
    REAL, INTENT(IN)    :: qmm       !<water in mm/ts in precipitation or surface runoff or infiltration
    REAL, INTENT(OUT)   :: release   !<release of substance from surface pool
    
    !Local variables
    REAL relpar    !release rate parameter
      
    relpar = 0.
    IF(subst==i_in) relpar = genpar(m_inrel)
    IF(subst==i_on) relpar = genpar(m_onrel)
    IF(subst==i_sp) relpar = genpar(m_sprel)
    IF(subst==i_pp) relpar = genpar(m_pprel)
    IF(subst==i_oc) relpar = genpar(m_ocrel)
    IF(subst==i_ss) relpar = genpar(m_ssrel)
    IF(subst==i_ae) relpar = genpar(m_aerel)
    IF(subst==i_t1) relpar = genpar(m_t1rel)
    
    release = pool * (1. - EXP(-relpar * qmm))
    pool = pool - release
  
  END SUBROUTINE release_from_pool
  
  !>Distribute soil load between surface pool and soil layers
  !
  !>\b Reference ModelDescription Nitrogen and phosphorus in land routines 
  !>(Nutrient soil leakage from outer source)
  SUBROUTINE distribute_soil_load(i,j,n,load,soilstate)
  
    USE HYPEVARIABLES, ONLY : m_sloadinf1,m_sloadinf2,m_sloadinf3, &
                              m_sloadonf1,m_sloadonf2,m_sloadonf3, &
                              m_sloadspf1,m_sloadspf2,m_sloadspf3, &
                              m_sloadppf1,m_sloadppf2,m_sloadppf3, &
                              m_sloadocf1,m_sloadocf2,m_sloadocf3, &
                              m_sloadssf1,m_sloadssf2,m_sloadssf3, &
                              m_sloadaef1,m_sloadaef2,m_sloadaef3, &
                              m_sloadt1f1,m_sloadt1f2,m_sloadt1f3

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<index of subbasin 
    INTEGER, INTENT(IN) :: j        !<index of class
    INTEGER, INTENT(IN) :: n        !<number of substances
    REAL, INTENT(INOUT) :: load(n)  !<load to soil
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    
    !Local variables
    INTEGER isubst,isl
    REAL soilloadfraction(0:3),sumslf
    REAL :: slload(n,0:3) !distributed load to soil
    
    !>\b Algoritm
    !>For every substance simulated
    DO isubst = 1,n
      soilloadfraction = 0. !default all on surface, used for T2
      !>\li Get parameter values for current substance
      IF(isubst==i_in)THEN
        soilloadfraction(3) = genpar(m_sloadinf3)
        soilloadfraction(2) = genpar(m_sloadinf2)
        soilloadfraction(1) = genpar(m_sloadinf1)
      ELSEIF(isubst==i_on)THEN
        soilloadfraction(3) = genpar(m_sloadonf3)
        soilloadfraction(2) = genpar(m_sloadonf2)
        soilloadfraction(1) = genpar(m_sloadonf1)
      ELSEIF(isubst==i_sp)THEN
        soilloadfraction(3) = genpar(m_sloadspf3)
        soilloadfraction(2) = genpar(m_sloadspf2)
        soilloadfraction(1) = genpar(m_sloadspf1)
      ELSEIF(isubst==i_pp)THEN
        soilloadfraction(3) = genpar(m_sloadppf3)
        soilloadfraction(2) = genpar(m_sloadppf2)
        soilloadfraction(1) = genpar(m_sloadppf1)
      ELSEIF(isubst==i_oc)THEN
        soilloadfraction(3) = genpar(m_sloadocf3)
        soilloadfraction(2) = genpar(m_sloadocf2)
        soilloadfraction(1) = genpar(m_sloadocf1)
      ELSEIF(isubst==i_ss)THEN
        soilloadfraction(3) = genpar(m_sloadssf3)
        soilloadfraction(2) = genpar(m_sloadssf2)
        soilloadfraction(1) = genpar(m_sloadssf1)
      ELSEIF(isubst==i_ae)THEN
        soilloadfraction(3) = genpar(m_sloadaef3)
        soilloadfraction(2) = genpar(m_sloadaef2)
        soilloadfraction(1) = genpar(m_sloadaef1)
      ELSEIF(isubst==i_t1)THEN
        soilloadfraction(3) = genpar(m_sloadt1f3)
        soilloadfraction(2) = genpar(m_sloadt1f2)
        soilloadfraction(1) = genpar(m_sloadt1f1)
      ENDIF
      !>\li Calculate the loads distribution (fractions)
      sumslf = SUM(soilloadfraction(1:3))
      IF(sumslf>1.)THEN
        soilloadfraction(0) = 0.
        soilloadfraction(3) = soilloadfraction(3)/sumslf
        soilloadfraction(2) = soilloadfraction(2)/sumslf
        soilloadfraction(1) = soilloadfraction(1)/sumslf
      ELSE
        soilloadfraction(0) = 1. - soilloadfraction(1) - soilloadfraction(2) - soilloadfraction(3)
      ENDIF
      !>\li Calculate the distributed load
      slload(isubst,:) = load(isubst)*soilloadfraction(:)
    ENDDO
    !>Add the distributed loads to the pools
    soilstate%surface(:,j,i) = soilstate%surface(:,j,i) + slload(:,0)
    DO isl = 1,3
      CALL add_source_to_water(soilstate%water(isl,j,i),n,soilstate%conc(:,isl,j,i),slload(:,isl))
    ENDDO

  END SUBROUTINE distribute_soil_load

END MODULE SOILMODEL_TRAVELTIME
