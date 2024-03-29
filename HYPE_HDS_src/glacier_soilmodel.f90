!> \file glacier_soilmodel.f90
!> Contains module glacier_soilmodel.

!>HYPE glacier soil model (glacier_model = 3)
MODULE GLACIER_SOILMODEL

  !Copyright 2012-2020 SMHI
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

  USE STATETYPE_MODULE
  USE MODVAR,  ONLY : missing_value,pi,   &
                      currentdate, &
                      genpar,landpar,soilpar,  &
                      load,basin,classbasin,path, &
                      glacier,glacierindex,nglaciers,  &
                      numsubstances,nsub,maxsoillayers, &
                      classdata,soilthick,soildepth, &
                      i_t1,i_t2,i_in,i_on,i_sp,i_pp,i_oc,&
                      modeloption,p_deepgroundwater,p_infiltration, &
                      conduct,simulate, &
                      simulatesubstances, &
                      p_snowmelt,p_snowevap,p_snowheat,p_soilleakage
  USE HYPEVARIABLES, ONLY : wpmm,fcmm,epmm,pwmm, &
                            m_srrcs, &
                            m_wetsp,m_drypp, m_ponatm,m_cfrost,m_sfrost,m_perc1,m_perc2,  &
                            m_sreroexp,m_filtpbuf,m_filtpinner,m_filtpother,m_macfilt, &
                            m_pprelexp,m_fertdays,m_minerfn,m_minerfp,m_degradhn, &
                            m_degradhp,m_denitrlu, &
                            m_dissolfN,m_dissolhN,m_dissolfP,m_dissolhP,m_littdays,  &
                            m_crate1,m_crate2,m_crate3,m_crate9,m_crate10,m_minc,m_ocsoim,m_ocsmslp,  &
                            m_freuc,m_freuexp,m_freurate, &
                            m_sswcorr,m_immdep,m_iwdfrac,m_wdpar,m_ttpi, &
                            m_glacvcoef,m_glacvexp,m_glacdens, &
                            m_glacvcoef1,m_glacvexp1,m_glac2arlim, &
                            soilmem,m_deepmem, &
                            glacier_model, &
                            epotdist, &
                            m_glacannmb
  USE GENERAL_WATER_CONCENTRATION, ONLY : remove_water,           &
                                          error_remove_water
  USE ATMOSPHERIC_PROCESSES, ONLY :  calculate_rain_snow_from_precipitation
  USE SOIL_PROCESSES, ONLY : calculate_snow, &
                             calculate_glacier_melt, &
                             calculate_potential_evaporation,          &
                             calculate_actual_soil_evapotranspiration, &
                             add_macropore_flow,    &
                             calculate_tile_drainage,  &
                             calculate_soil_runoff,    &
                             calculate_infiltration_flow_diversion,   &
                             add_infiltration,   &
                             percolation, &
                             calculate_groundwater_table,  &
                             calculate_soiltemp,   &
                             calculate_frostdepth,  &
                             calculate_unfrozen_soil_water, &
                             calculate_liquid_water_fraction, &
                             calculate_soil_moisture_deficit, &
                             snowalbedo_function
  USE NPC_SOIL_PROCESSES, ONLY : add_dry_deposition_to_landclass,  &
                                 calculate_plant,        &
                                 soil_np_processes,      &
                                 soil_carbon_processes,  &
                                 balance_spsoil,         &
                                 particle_processes_for_runoff, &
                                 local_diffuse_source
  USE TRACER_PROCESSES, ONLY : soil_tracer_processes, &
                               set_soil_T2_from_soiltemp_model
  USE REGIONAL_GROUNDWATER_MODULE, ONLY : add_regional_groundwater_flow_to_soil

  IMPLICIT NONE
  PRIVATE
  !Private subroutines
  !--------------------------------------------------------------------
  !
  !calculate_glacier_area
  !--------------------------------------------------------------------
  PUBLIC :: soilmodel_3, &
            get_glacier_parameters, &
            initiate_glacier, &
            initiate_glacier_state, &
            calculate_glacier_massbalance

  !Parameter declarations (private)
  CHARACTER(LEN=27) :: errstring(1)  !error message for location of remove_water call
  PARAMETER (errstring = (/'surface runoff, soillayer 1'/))

  !Variable declarations
  REAL :: glac_areavol_par2(2)              !<variable holding parameters for glacier area-volume relationship
  REAL,ALLOCATABLE :: glac_areavol_par1(:)  !<variable holding parameters for glacier area-volume relationship
 
!> \brief Type for glacier mass balance calculations
  TYPE glacierTimeseries
    REAL,ALLOCATABLE :: glac_vol(:)  !vector with glacier volume time series for mass balance calculations
    REAL,ALLOCATABLE :: glac_area(:) !vector with glacier area time series for mass balance calculations
  END TYPE glacierTimeseries
  
  TYPE(glacierTimeseries), ALLOCATABLE :: glacierTimeSeriesData(:)
  
  INTEGER, PARAMETER :: glacierTimeSeriesSteps = 731 !<two years (one leap year) maximum mass balance period
  
CONTAINS

  !--------------------------------------------------------------
  !>\brief Soilmodel for glacier land class 
  !!Calculate glacier, snow and soil processes for a glacier land class. 
  !!
  ! TODO: glac_melt goes to soil today? Maybe ignore soil calculation
  ! for glacier part and let glac_melt go directly to runoff instead?
  !>
  !> \b Reference ModelDescription Chapter Land routines (Glaciers)
  !--------------------------------------------------------------
  SUBROUTINE soilmodel_3(i,j,isoil,iluse,subid,pdayno,classarea,prec,cprec,temp, & 
       daylength,mintemp,maxtemp,swrad,  &
       radext,netrad,actvap,satvap,wind,cevpcorr,rrcscorr,phoscorr,  &
       frozenstate,soilstate,miscstate,surfaceflow,csrunoff,crunoffd,    &
       cropuptakein,nitrif,denitrif,epot,gwat,frostdepth,smdef,  &
       evap,cevap,crunoff1,crunoff2,crunoff3,  &
       glac_part,nonglac_part,snowfall,rainfall,cropsources,ruralaload,rgrwload,atmdepload,infiltrationflows,glacierflows, &
       evapflows,runofflows,verticalflows,cverticalflows,horizontalflows,horizontalflows2,evapsnow,cruralflow,snowtemp,snowsurftemp)

    INTEGER, INTENT(IN) :: i        !<index for current subbasin
    INTEGER, INTENT(IN) :: j        !<index for current class 
    INTEGER, INTENT(IN) :: isoil    !<index of soil type
    INTEGER, INTENT(IN) :: iluse    !<index of landuse
    INTEGER, INTENT(IN) :: subid    !<subbasin id
    INTEGER, INTENT(IN) :: pdayno    !<pseudo dayno for use in soil model subroutines
    REAL, INTENT(IN) :: classarea   !<class area (km2)
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
    REAL, INTENT(IN) :: cevpcorr    !<correction of potential evaporation
    REAL, INTENT(IN) :: rrcscorr    !<correction of recession coefficients
    REAL, INTENT(IN) :: phoscorr    !<correction of phosphorus level
    TYPE(snowicestatetype),INTENT(INOUT)  :: frozenstate   !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    TYPE(miscstatetype),INTENT(INOUT)  :: miscstate   !<Misc states
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
    REAL, INTENT(OUT) :: evap       !<evapotranspiration
    REAL, INTENT(OUT) :: cevap(numsubstances)   !<concentration of evapotranspiration
    REAL, INTENT(OUT) :: crunoff1(numsubstances)   !<concentration of runoff from soil layer 1 (mg/L)
    REAL, INTENT(OUT) :: crunoff2(numsubstances)   !<concentration of runoff from soil layer 2 (mg/L)
    REAL, INTENT(OUT) :: crunoff3(numsubstances)   !<concentration of runoff from soil layer 3 (mg/L)
    REAL, INTENT(OUT) :: glac_part       !<fraction of glacier (-)
    REAL, INTENT(OUT) :: nonglac_part    !<old fraction of class is snow field (-)
    REAL, INTENT(OUT) :: snowfall     !<Precipitation as rain (mm)
    REAL, INTENT(OUT) :: rainfall     !<Precipitation as snow (mm)
    REAL, INTENT(INOUT):: cropsources(2,numsubstances)  !<Load from fertiliser and resudues (kg/timestep)
    REAL, INTENT(OUT) :: ruralaload(numsubstances)   !<Load from rural households (kg/timestep)
    REAL, INTENT(INOUT) :: rgrwload(numsubstances)     !<Load from regional groundwater flow to soil (kg/timestep)
    REAL, INTENT(INOUT) :: atmdepload(numsubstances) !<Load of atmospheric dry deposition (kg/timestep)
    REAL, INTENT(OUT) :: infiltrationflows(7)  !<several infiltration flows [mm]
    REAL, INTENT(OUT) :: glacierflows(2)   !<flow between snow plain snow and glacier when glacier grow, precipitation on glacier [m3]
    REAL, INTENT(OUT) :: evapflows(4)  !<evaporation from soillayers, snow and glacier [mm]
    REAL, INTENT(OUT) :: runofflows(7) !<different runoff flows:1-3=soil runoff sl 1-3,4-6=tile runoff sl 1-3,7=saturated surface runoff
    REAL, INTENT(OUT) :: verticalflows(6) !<vertical flows:1-2=percolation,3-4=upwelling due to rural,5-6=upwelling due to reg. grw flows
    REAL, INTENT(OUT) :: cverticalflows(2,numsubstances) !<concentration of vertical flows:1-2=percolation
    REAL, INTENT(OUT) :: horizontalflows(3) !<horizontal flows:1-3=recieved rural load flow
    REAL, INTENT(INOUT) :: horizontalflows2(3,nsub) !<horizontal flows:1-3=division of regional groundwater flows to grwdown
    REAL, INTENT(OUT) :: evapsnow  !<evaporation from snow covered land (from snow and glacier)
    REAL, INTENT(OUT) :: cruralflow(numsubstances) !<conc of rural flow
    REAL, INTENT(OUT) :: snowtemp !<snowpack temperature
    REAL, INTENT(OUT) :: snowsurftemp !<snow surface temperature
    
    !Local variables
    !---------------
    INTEGER status  !error status of subroutine call
    REAL sc         !coefficient for runoff recession surface runoff(no unit)
    !Variables for class values
    REAL ginfilt,infilt   !gross infiltration (rain+melt), actual infiltration (after removed surfaceflow and macroporeflow)
    REAL cginfilt(numsubstances),cinfilt(numsubstances)   !concentration of infiltration
    REAL totalsurfaceflow
    REAL satoverflow
    REAL excessinfilt   !infiltration excess runoff 
    REAL macroflow
    REAL cmacroflow(numsubstances), cexcessinfilt(numsubstances)          !concentration in infiltration excess runoff and macropore flow
    REAL melt,cmelt(numsubstances)      !snow melt,concentration in snow melt water
    REAL snowmelt         !melt of snow (classarea based)
    REAL cevapsnow(numsubstances)  !conc in evaporation from snow covered land
    !More variables
    REAL plantuptake(2,2)          !uptake of plant (what they want), kg NP/km2/timestep
    REAL soilrunoff(maxsoillayers)    !soil runoff
    REAL csoilrunoff(numsubstances,maxsoillayers)    !concentration of soil runoff
    REAL cweights(maxsoillayers) ! weigths to calculate T2 conc. in tile drainage 
    REAL T1release !release of T1 from surface sources
    REAL runoffd      !tile drainage runoff
    
    !Local variables for glacier model
    REAL glac_classarea   !area of slc:glacier (m2)
    REAL glac_area        !glacier area (m2)
    REAL glac_melt        !melt of glacier
    REAL glacmelt         !melt of glacier (classarea based)
    REAL new_glac_area    !area of glacier (m2)  
    REAL new_glac_part    !glacier part of classarea (-)    
    REAL effsnowcov,effsnowcovplain       !effective snow cover with respect to evaporation evap = evap(snowfree)*(1-effsnowcov)+evapsnow
    REAL epotsnow,epotglac         !potential evaporation from snow
    REAL glac_evap,glacevap
    REAL soilwater(maxsoillayers) !soilwater temporary variable
    REAL snowheat     !snow heat content, temporary variable
    REAL liqfrac(maxsoillayers) !liquid fraction of soil water
    REAL frozenvol(maxsoillayers) !<Frozen water volume (mm)
    REAL snowrunoff,snowrunoff2
    
    !Output, default values
    infiltrationflows = 0.
    glacierflows = 0.
    runofflows = 0.
    epotglac = 0.
    evapflows = 0.
    verticalflows=0.
    cverticalflows=0.
    nitrif = 0.; denitrif=0.
    cropuptakein = 0.
    
    !Locally defined variables for indata to be used in this subroutine
    liqfrac = 1.
    frozenvol = 0.
    T1release = 0.

    IF(modeloption(p_soilleakage)==0)THEN
    !Atmospheric deposition, add to soil or precipitation
    CALL add_dry_deposition_to_landclass(i,j,iluse,classdata(j)%vegtype,conduct%simN,conduct%simP, &
         classarea,landpar(m_drypp,iluse),atmdepload,frozenstate,soilstate)
    
    !Calculate plant growth (for uptake of nutrients)
    CALL calculate_plant(i,j,temp,daylength,plantuptake,miscstate)
    ENDIF

    !Current glacier area (simple function of volume)
    glac_classarea = classarea*1.E6      !area of slc:glacier (m2)
    glac_area = calculate_glacier_area(i,glac_classarea,frozenstate%glacvol(i))
    glac_part = glac_area/glac_classarea
    nonglac_part = 1. - glac_part

    !Precipitation, rain/snow division only for snow plain (1-glac_part, DG's note)
    CALL calculate_rain_snow_from_precipitation(i,iluse,prec,temp,snowfall,rainfall)
    
    !Potential evapotranspiration (before snow and glacier melt/evaporation calculations)
    CALL calculate_potential_evaporation(i,j,temp,epot,radext,swrad,netrad,actvap,satvap,wind,epotsnow)
    epot = epot * cevpcorr          !PET for snow free soil, regionally adjusted
    epotsnow = epotsnow * cevpcorr  !PET for snow covered soil, regionally adjusted
    
    !Snow calculations on snow plain (snowfall addition and melting)
    snowheat = 0.
    IF(modeloption(p_snowheat)>=1) snowheat = frozenstate%snowheat(j,i)
    CALL calculate_snow(i,j,basin(i)%subid,iluse,snowfall,cprec,frozenstate%snow(j,i),frozenstate%csnow(:,j,i), &
                        temp,melt,cmelt,swrad,frozenstate%snowage(j,i), &
                        frozenstate%snowmax(j,i),frozenstate%snowdepth(j,i),frozenstate%snowcov(j,i),snowheat, &
                        epotsnow,evapsnow,cevapsnow,effsnowcov,snowtemp,snowsurftemp,frozenstate%snowliq(j,i),snowrunoff)
    evapflows(3) = evapsnow
    snowmelt = melt * nonglac_part  !output variable rescaled to class-area
    snowrunoff2 = snowrunoff * nonglac_part  !output variable rescaled to class-area
    IF(modeloption(p_snowheat)>=1) frozenstate%snowheat(j,i) = snowheat
    
    !Melting of glacier ice (glac_melt) - using glacier parameters, separate from the snow parameters
    CALL calculate_glacier_melt(iluse,glacier(glacierindex(i))%gtype,temp,swrad,epot,glac_area,frozenstate%glacvol(i), &
                                frozenstate%snowage(j,i),frozenstate%snowcov(j,i),glac_melt,epotglac,glac_evap)
    evapflows(4) = glac_evap

    !Gross infiltration (rain+melt) on snow plain + melt of glacier 
    glacmelt = glac_melt*glac_part
    ginfilt = (rainfall + snowrunoff)*nonglac_part+glacmelt
    IF(simulatesubstances)THEN
      IF(ginfilt>0.)THEN
        cginfilt(:) = (cmelt(:)*snowrunoff + cprec(:)*rainfall)*nonglac_part / ginfilt   !correcting the glac_part error, thus cglac_melt(i_T2)==0 /DG
      ELSE 
        cginfilt(:) = 0.
      ENDIF
    ENDIF
    !Evaporation from snow plain and glacier (evapsnow+evapglac)
    glacevap = glac_evap * glac_part
    evapsnow = evapsnow * nonglac_part
    
    !Effective reduction in evapotranspiration from the non-glacier part, taking reduction from snow cover into account.
    effsnowcovplain   = glac_part + effsnowcov * nonglac_part 
        
    !Update glacier volume, change in m3/timestep. 
    !All precipitation on glacier become glacier ice. Atmdep in prec/dry on glacier assumed retained.
    !Increasing glacier incorporate snow on reduced snow field area
    !Receding glacier, snow redistributes on new snow field area.
    IF(glac_area>0.) frozenstate%glacvol(i) = frozenstate%glacvol(i) + (prec-glac_melt-glac_evap)/1000.*glac_area/genpar(m_glacdens)
    IF(frozenstate%glacvol(i)<0 .AND. glacier(glacierindex(i))%gtype/=3) frozenstate%glacvol(i)=0.
    IF(prec>0.) glacierflows(2) = prec*(1.E-3*glac_area)
    IF(frozenstate%glacvol(i)>0 .AND. glac_area<glac_classarea) THEN
      new_glac_area = calculate_glacier_area(i,glac_classarea,frozenstate%glacvol(i))
      IF(new_glac_area < glac_area)THEN
        frozenstate%snow(j,i) = frozenstate%snow(j,i) * ((glac_classarea-glac_area)/(glac_classarea-new_glac_area))
      ELSEIF(new_glac_area > glac_area) THEN
        IF(frozenstate%snow(j,i)>0.)THEN
          glacierflows(1) = (new_glac_area-glac_area)*frozenstate%snow(j,i)*1.E-3 !snow plain snow -> glacier ice (m3 H2O)
          frozenstate%glacvol(i) = frozenstate%glacvol(i) + glacierflows(1)/genpar(m_glacdens)
        ENDIF
      ENDIF
      new_glac_part = new_glac_area/glac_classarea
      glac_area = new_glac_area   !for output
      glac_part = new_glac_part   !for output, evaporation
    ENDIF

    !Calculate soil temperature, snow age, snow depth and frost depth
    CALL calculate_soiltemp(maxsoillayers,temp,frozenstate%snowdepth(j,i), &
            genpar(m_deepmem),soilmem(:,j),soilstate%deeptemp(j,i),soilstate%temp(:,j,i))
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
      !Calculate and add infiltration to soil, including calculation of surface flow and macropore flow due to limited infiltration capacity 
      CALL calculate_infiltration_flow_diversion(i,j,isoil,wpmm(:,j),fcmm(:,j),epmm(:,j),ginfilt,cginfilt,temp,mintemp,maxtemp,  &
         infilt,cinfilt,excessinfilt,cexcessinfilt,macroflow,cmacroflow,frozenstate,soilstate)
      CALL add_infiltration(i,j,iluse,infilt,cinfilt,soilstate)
      IF(ginfilt>0.)THEN
        infiltrationflows(1) = snowrunoff2/ginfilt !fraction of ginfilt from snow melt
        infiltrationflows(7) = glacmelt/ginfilt !fraction of ginfilt from glacier melt
      ENDIF
      infiltrationflows(2) = infilt
      infiltrationflows(3) = excessinfilt
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac

      !Percolation down through the soil layers, including DOC-reduction
      CALL percolation(i,j,isoil,subid,wpmm(:,j),fcmm(:,j),pwmm(:,j),soilthick(:,j),liqfrac,verticalflows(1:2),cverticalflows,soilstate)
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF

    !Calculate and remove evapotranspiration from the soil upper two layers of the snowplain
    soilwater = soilstate%water(:,j,i) !set soilwater variable to be used for evapotranspiration, soil runoff and tile runoff
    CALL calculate_actual_soil_evapotranspiration(i,j,2,soilwater,temp,epot, &
             wpmm(:,j),fcmm(:,j),epotdist(:,j),(1.-effsnowcovplain),liqfrac, &
             soilstate,evap,evapflows(1:2),cevap)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac

    !Combined evaporation from snow-free area, snowplain and glacier, and update concentrations cevap
    evap     = evap + evapsnow + glacevap
    IF(numsubstances.GT.0 .AND. evap.GT.0.) cevap(:) = (cevap(:)*(evap-evapsnow) + cevapsnow(:)*evapsnow)/evap
    epot = epot * (1.-effsnowcovplain) + epotsnow * effsnowcov * nonglac_part + epotglac * glac_part
    evapsnow = evapsnow + glacevap
    
    !Calculate and remove soil runoff
    CALL calculate_soil_runoff(i,j,isoil,subid,soilwater,epmm(:,j), &
             classdata(j)%streamdepth,liqfrac,soilstate,soilrunoff,csoilrunoff)
    runofflows(1:3) = soilrunoff(1:3)
    crunoff1 = csoilrunoff(:,1)
    crunoff2 = csoilrunoff(:,2)
    crunoff3 = csoilrunoff(:,3)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac

    !Calculate and remove runoff by tile or drainage pipe
    CALL calculate_tile_drainage(i,j,isoil,subid,soilwater,epmm(:,j),soildepth(:,j), &
             soilthick(:,j),classdata(j)%tiledepth,rrcscorr,liqfrac,soilstate,runoffd,&
             crunoffd,cweights)    !unnecessary for glacier?
    runofflows(4:6) = runoffd*cweights(1:3)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac

    !Add regional lateral groundwater flow from other subbasins
    IF(modeloption(p_deepgroundwater)==1) CALL add_regional_groundwater_flow_to_soil(i,j,classarea,pwmm(:,j),soilstate,rgrwload,verticalflows(5:6),horizontalflows2)
    
    !Add load from local diffuse sources to the lowest soil layer
    CALL local_diffuse_source(i,j,pwmm(:,j),classarea,soilstate,ruralaload,verticalflows(3:4),horizontalflows(1:3),cruralflow)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    
    IF(modeloption(p_infiltration)==2 .OR. modeloption(p_infiltration)==3)THEN
      !Calculate and add infiltration to soil, including calculation of surface flow and macropore flow due to limited infiltration capacity 
      CALL calculate_infiltration_flow_diversion(i,j,isoil,wpmm(:,j),fcmm(:,j),epmm(:,j),ginfilt,cginfilt,temp,mintemp,maxtemp,  &
         infilt,cinfilt,excessinfilt,cexcessinfilt,macroflow,cmacroflow,frozenstate,soilstate)
      CALL add_infiltration(i,j,iluse,infilt,cinfilt,soilstate)
      IF(ginfilt>0.)THEN
        infiltrationflows(1) = snowrunoff2/ginfilt !fraction of ginfilt from snow melt
        infiltrationflows(7) = glacmelt/ginfilt !fraction of ginfilt from glacier melt
      ENDIF
      infiltrationflows(2) = infilt
      infiltrationflows(3) = excessinfilt
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF

    IF(modeloption(p_infiltration)==2 .OR. modeloption(p_infiltration)==3)THEN
      !Percolation down through the soil layers, including DOC-reduction
      CALL percolation(i,j,isoil,subid,wpmm(:,j),fcmm(:,j),pwmm(:,j),soilthick(:,j),liqfrac,verticalflows(1:2),cverticalflows,soilstate)
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF

    !Surface runoff from saturated overland flow of uppermost soil layer
    sc = landpar(m_srrcs,iluse)*rrcscorr      !Runoff coefficient for surface runoff
    IF(sc>1.) sc = 1.
    satoverflow = MAX(sc * (soilstate%water(1,j,i)-pwmm(1,j)),0.)
    csrunoff = 0.
    IF(satoverflow > 0.) THEN
      CALL remove_water(soilstate%water(1,j,i),numsubstances,soilstate%conc(:,1,j,i),satoverflow,soilstate%conc(:,1,j,i),status)
      IF(status.NE.0) CALL error_remove_water(errstring(1),subid,i,j)
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac
    ENDIF
    runofflows(7) = satoverflow

    !Total surfaceflow (saturated overland flow and excess infiltration)
    totalsurfaceflow = satoverflow + excessinfilt
    surfaceflow(1) = satoverflow
    surfaceflow(2) = excessinfilt
    IF(totalsurfaceflow > 0. .AND. simulatesubstances) THEN
      csrunoff(:) = (soilstate%conc(:,1,j,i) * satoverflow + excessinfilt * cexcessinfilt(:)) / totalsurfaceflow     !used for satoverflow and excessinfilt
    ENDIF

    IF(modeloption(p_soilleakage)==0)THEN
    !Erosion of particles with fastflow (surface flow and macropore flow) including delay in temporary storage.
    IF(conduct%simP.OR.conduct%simS) CALL particle_processes_for_runoff(i,j,isoil,iluse,pdayno,rainfall,totalsurfaceflow,   &
              macroflow,runoffd,runofflows(1)+runofflows(2)+runofflows(3)+runoffd+totalsurfaceflow,phoscorr,  &
              csrunoff,cmacroflow,crunoffd,crunoff1,  &
              crunoff2,crunoff3,frozenstate%snow(j,i),frostdepth,soilstate)
    ENDIF

    !Add macropore water to soil layer with groundwater level (except the PP)
    CALL add_macropore_flow(i,j,macroflow,cmacroflow,epmm(:,j),pwmm(:,j), &
            soildepth(:,j),soilthick(:,j),infiltrationflows(4:6),soilstate)
    CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac) !update liqfrac

    IF(modeloption(p_infiltration)==0 .OR. modeloption(p_infiltration)==1)THEN
      !Second percolation down through the soil layers, including DOC-reduction (limited to same maxperc)
      CALL percolation(i,j,isoil,subid,wpmm(:,j),fcmm(:,j),pwmm(:,j),soilthick(:,j),liqfrac,verticalflows(1:2),cverticalflows,soilstate)
      CALL calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac)
    ENDIF

    !Groundwater level
    CALL calculate_groundwater_table(j,soilstate%water(:,j,i),epmm(:,j),soildepth(:,j),soilthick(:,j),gwat) 
    CALL calculate_soil_moisture_deficit(soilstate%water(:,j,i),wpmm(:,j),fcmm(:,j),soilthick(:,j),smdef) 

    IF(modeloption(p_soilleakage)==0)THEN
      !Soil transformation processes for substances             
      IF(numsubstances>0) CALL soil_np_processes(i,j,iluse,conduct%simN,conduct%simP,conduct%simC,classarea,wpmm(:,j),pwmm(:,j),plantuptake,soilthick(:,j),genpar(m_fertdays),genpar(m_littdays),  &
           nitrif,denitrif,cropuptakein,cropsources,   &
           landpar(m_dissolfN,iluse),landpar(m_dissolfP,iluse),landpar(m_dissolhN,iluse),landpar(m_dissolhP,iluse),      &
           landpar(m_minerfn,iluse),landpar(m_minerfp,iluse),landpar(m_degradhn,iluse),      &
           landpar(m_denitrlu,iluse),landpar(m_degradhp,iluse),soilstate)
      IF(conduct%simC) CALL soil_carbon_processes(i,j,wpmm(:,j),fcmm(:,j),pwmm(:,j),soilthick(:,j),    &
           genpar(m_crate1),genpar(m_crate2),genpar(m_crate3),genpar(m_crate9),genpar(m_crate10),genpar(m_minc),  &
           landpar(m_ocsoim,iluse),landpar(m_ocsmslp,iluse),soilstate)
      IF(conduct%simP) CALL balance_spsoil(i,j,soilthick(:,j), & 
           soilpar(m_freuc,isoil),soilpar(m_freuexp,isoil),soilpar(m_freurate,isoil),soilstate)
      CALL soil_tracer_processes(i,j,currentdate%year,soilthick(:,j),soilstate,miscstate,ginfilt,T1release)
    
      !Add released T1 to surface runoff or top soil
      IF(simulate%substance(i_t1))THEN
        IF(T1release>0.)THEN
          IF(totalsurfaceflow>0.)THEN
            csrunoff(i_t1) = csrunoff(i_t1) + (T1release * (totalsurfaceflow/(infilt+totalsurfaceflow))) / totalsurfaceflow
          ENDIF
          soilstate%partT1(1,j,i) = soilstate%partT1(1,j,i) + T1release * (infilt/(infilt+totalsurfaceflow))
        ENDIF
      ENDIF      

      !Runoff T2 temperature dependent on soiltemp calculation [DG/JS Temp.model, May-2013]
      IF(simulate%substance(i_t2)) &
        CALL set_soil_T2_from_soiltemp_model(i,j,satoverflow,excessinfilt,cexcessinfilt,cweights,& 
                                             crunoff1,crunoff2,crunoff3,csrunoff,crunoffd,soilstate)
    ENDIF
    
    !Update GlacierTimeSeriesData with current glacier state
    IF(ALLOCATED(GlacierTimeSeriesData(i)%glac_vol))THEN
      GlacierTimeSeriesData(i)%glac_vol(1:GlacierTimeSeriesSteps-1)=GlacierTimeSeriesData(i)%glac_vol(2:GlacierTimeSeriesSteps)
      GlacierTimeSeriesData(i)%glac_vol(GlacierTimeSeriesSteps)=frozenstate%glacvol(i)
    ENDIF
    IF(ALLOCATED(GlacierTimeSeriesData(i)%glac_area))THEN
      GlacierTimeSeriesData(i)%glac_area(1:GlacierTimeSeriesSteps-1)=GlacierTimeSeriesData(i)%glac_area(2:GlacierTimeSeriesSteps)
      GlacierTimeSeriesData(i)%glac_area(GlacierTimeSeriesSteps)=glac_area
    ENDIF

  END SUBROUTINE soilmodel_3

  !>Initialize glacier parameters. 
  !>
  !> \b Reference ModelDescription Chapter Land routines (Glaciers)
  !---------------------------------------------------------
  SUBROUTINE get_glacier_parameters(nc,arr)

    !Argument declarations
    INTEGER, INTENT(IN) :: nc         !<number of classes (nclass)
    INTEGER, INTENT(IN) :: arr(nc)    !<classmodels

    !Local variables
    INTEGER i,j
    REAL volcoeff,tot_glac_area
    LOGICAL nogd
    
    !Allocate variables needed
    nogd = .FALSE.
    IF(.NOT.ALLOCATED(glacier))THEN     !No GlacierData.txt!
      nglaciers = nsub
      ALLOCATE(glacier(nglaciers))
      glacier(1:nsub)%gtype = INT(missing_value)
      glacier(1:nsub)%volcorr = 0.
      glacier(1:nsub)%yeardiff = 0.
      glacier(1:nsub)%glacinimb = 0.
      glacier(1:nsub)%inivol = missing_value
      IF(.NOT.ALLOCATED(glacierindex)) ALLOCATE(glacierindex(nsub))
      glacierindex=-9999
      nogd = .TRUE.
    ENDIF
    IF(.NOT.ALLOCATED(glac_areavol_par1)) ALLOCATE(glac_areavol_par1(nglaciers))

    !Default value glacier parameter
    IF(genpar(m_glacdens).LE.0.)   genpar(m_glacdens)   = 0.85   ![m3 water/m3 ice]
    IF(genpar(m_glacvcoef).LE.0.)  genpar(m_glacvcoef)  = 0.205  !Default values for glacier type 0 (Mountain glacier) (Radic&Hock(2010), glacier, trad.fit)
    IF(genpar(m_glacvexp).LE.0.)   genpar(m_glacvexp)   = 1.375  ! -"-
    IF(genpar(m_glacvcoef1).LE.0.) genpar(m_glacvcoef1) = 1.701  !Default values for glacier type 1 (Ice caps) (Radic&Hock(2010), ice cap, trad.fit)
    IF(genpar(m_glacvexp1).LE.0.)  genpar(m_glacvexp1)  = 1.25   ! -"-
    IF(glacier(1)%gtype==INT(missing_value))THEN
      glacier%gtype = 0
      IF(nogd)THEN
        DO i = 1,nsub
          glacierindex(i) = i
        ENDDO
        IF(genpar(m_glac2arlim)>0.)THEN
          DO j = 1,nc
            IF(arr(j)==glacier_model)THEN
              DO i = 1,nsub
                tot_glac_area = classbasin(i,j)%part*basin(i)%area      !area of slc:glacier (m2)
                IF(tot_glac_area>0.)THEN
                  IF(tot_glac_area >= genpar(m_glac2arlim))THEN
                    glacier(i)%gtype = 1
                  ENDIF  
                ENDIF
              ENDDO
            ENDIF
          ENDDO
        ENDIF
      ENDIF
    ENDIF
    
    !Set help variable for glacier area calculation: area = (vol*glac_areavol_par1/volcorr)**glac_areavol_par2
    DO i = 1,nglaciers
      IF(glacier(i)%gtype==0)THEN
        volcoeff = genpar(m_glacvcoef)*EXP(glacier(i)%volcorr)
      ELSE
        volcoeff = genpar(m_glacvcoef1)*EXP(glacier(i)%volcorr)
      ENDIF
      glac_areavol_par1(i) = 1./volcoeff
    ENDDO
    glac_areavol_par2(1) = 1./genpar(m_glacvexp)
    glac_areavol_par2(2) = 1./genpar(m_glacvexp1)
   
  END SUBROUTINE get_glacier_parameters

  !>Initialize glacier parameters and variables. 
  !>
  !> \b Reference ModelDescription Chapter Land routines (Glaciers)
  !---------------------------------------------------------
  SUBROUTINE initiate_glacier(nc,arr)

    !Argument declarations
    INTEGER, INTENT(IN) :: nc         !<number of classes (nclass)
    INTEGER, INTENT(IN) :: arr(nc)    !<classmodels
    
    !Local variables
    INTEGER i,j

    !Default value glacier parameter
    CALL get_glacier_parameters(nc,arr)
      
    !Allocate and initiate the glacier time series tables
    IF(.NOT.ALLOCATED(glacierTimeSeriesData))THEN
      ALLOCATE(glacierTimeSeriesData(nsub))
      DO j=1,nc
        IF(arr(j)==glacier_model)THEN
          DO i = 1,nsub
            IF(classbasin(i,j)%part>0.)THEN
              IF(.NOT.ALLOCATED(glacierTimeSeriesData(i)%glac_vol))ALLOCATE(glacierTimeSeriesData(i)%glac_vol(glacierTimeSeriesSteps))
              IF(.NOT.ALLOCATED(glacierTimeSeriesData(i)%glac_area))ALLOCATE(glacierTimeSeriesData(i)%glac_area(glacierTimeSeriesSteps))
            ENDIF
          ENDDO
        ENDIF
      ENDDO
    ENDIF
    DO j=1,nc
      IF(arr(j)==glacier_model)THEN
        DO i = 1,nsub
          IF(classbasin(i,j)%part>0.)THEN
            glacierTimeSeriesData(i)%glac_vol(:)  = missing_value
            glacierTimeSeriesData(i)%glac_area(:) = missing_value
          ENDIF
        ENDDO
      ENDIF
    ENDDO

  END SUBROUTINE initiate_glacier

  !>Initialize glacier volume. 
  !>
  !> \b Reference ModelDescription Chapter Land routines (Glaciers)
  !---------------------------------------------------------
  SUBROUTINE initiate_glacier_state(nc,arr,frozenstate)

    !Argument declarations
    INTEGER, INTENT(IN) :: nc         !<number of classes (nclass)
    INTEGER, INTENT(IN) :: arr(nc)    !<classmodels
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate !<Snow and ice states
    
    !Local variables
    INTEGER i,j 
    REAL tot_glac_area  !area of slc - glacier

    !Default value glacier parameter
    CALL get_glacier_parameters(nc,arr)

    !Calculate initial glacier volume
    DO j = 1,nc
      IF(arr(j)==glacier_model)THEN
        DO i = 1,nsub
          IF(classbasin(i,j)%part>0.)THEN
            tot_glac_area = classbasin(i,j)%part*basin(i)%area      !area of slc:glacier (m2)
            IF(glacier(glacierindex(i))%inivol>=0)THEN
              frozenstate%glacvol(i) = glacier(glacierindex(i))%inivol !m3
            ELSEIF(glacier(glacierindex(i))%gtype==0)THEN
              frozenstate%glacvol(i) = EXP(glacier(glacierindex(i))%volcorr)*genpar(m_glacvcoef)*(tot_glac_area**genpar(m_glacvexp))  !m3
            ELSE
              frozenstate%glacvol(i) = EXP(glacier(glacierindex(i))%volcorr)*genpar(m_glacvcoef1)*(tot_glac_area**genpar(m_glacvexp1))  !m3
            ENDIF  
            !Set the initial glacier area in relation to simulation starting date
            IF(glacier(glacierindex(i))%yeardiff/=0 .AND. (genpar(m_glacannmb)/=0. .OR. glacier(glacierindex(i))%glacinimb/=0.))THEN
              !update volume by average_annual_massbalance * yearsdiff - priority to glacinimb in GlacierData.txt over par.txt
              IF(glacier(glacierindex(i))%glacinimb/=0.)THEN
                frozenstate%glacvol(i) = frozenstate%glacvol(i) + glacier(glacierindex(i))%yeardiff * glacier(glacierindex(i))%glacinimb/1000.*tot_glac_area/genpar(m_glacdens)
              ELSE
                frozenstate%glacvol(i) = frozenstate%glacvol(i) + glacier(glacierindex(i))%yeardiff * genpar(m_glacannmb)/1000.*tot_glac_area/genpar(m_glacdens)
              ENDIF
            ENDIF
          ENDIF
        ENDDO
      ENDIF
    ENDDO

  END SUBROUTINE initiate_glacier_state

  !>Calculate glacier area based on glacier area-volume relationship or class' area
  !>depending on glacier type.
  !>
  !> \b Reference ModelDescription Chapter Land routines (Glaciers)
  !---------------------------------------------------------
  REAL FUNCTION calculate_glacier_area(i,classarea,vol)

    !Argument declarations
    INTEGER, INTENT(IN) :: i      !<index of current subbasin
    REAL, INTENT(IN) :: classarea !<glacier class' area (m2)
    REAL, INTENT(IN) :: vol       !<glacier volume of current subbasin (m3)
    
    !Local variables
    REAL area

    IF(glacier(glacierindex(i))%gtype==3)THEN
      area = classarea
    ELSEIF(vol==0)THEN
      area = 0
    ELSEIF(glacier(glacierindex(i))%gtype==2)THEN
      area = classarea
    ELSE
      area = (vol*glac_areavol_par1(glacierindex(i)))**glac_areavol_par2(glacier(glacierindex(i))%gtype+1)
      IF(area > classarea) area = classarea
    ENDIF
    calculate_glacier_area = area

  END FUNCTION calculate_glacier_area
  
  !>Glacier mass for a requested obseration period. 
  !>
  !> "observation operator" for comparison with WGMS mass balance data
  !-------------------------------------------------------------------
  SUBROUTINE calculate_glacier_massbalance(i,recMP,comMB,comMA)
  
    INTEGER, INTENT(IN) :: i      !<current subbasin
    REAL, INTENT(IN)    :: recMP  !<mass balance period
    REAL, INTENT(OUT)   :: comMB  !<computed glacier mass balance
    REAL, INTENT(OUT)   :: comMA  !<computed mass balance area
    
    INTEGER glacT0, glacT1
    
    !beginning and ending of evaluation period in the glacier time series
    glacT0 = glacierTimeSeriesSteps-INT(recMP)
    glacT1 = glacierTimeSeriesSteps
    
    !check if the beginning is within the saved time series
    IF(glacT0.GT.0)THEN
      !check if modelled values are available at the beginning date, if so, evaluate mass balance
      IF(glacierTimeSeriesData(i)%glac_vol(glacT0).GE.0.)THEN
        !Mass balance(mm) = dmass/dtime/darea = density * (vol(T1)-vol(T0))*2/(area(T1)+area(T0)
        !  check if glacier area > 0
        comMA = 0.5 * (glacierTimeSeriesData(i)%glac_area(glacT0)+glacierTimeSeriesData(i)%glac_area(glacT1))
        IF(comMA.GT.0.)THEN
          comMB = 1000. * genpar(m_glacdens) * (glacierTimeSeriesData(i)%glac_vol(glacT1) - glacierTimeSeriesData(i)%glac_vol(glacT0))/ comMA
        ELSE
          !if glacier area = 0 in both ends of mass balance period, then the mass balance = 0
          comMB = 0
        ENDIF
        comMA = comMA/1.E6 !transform m2 to km2 for printout
      ELSE
        comMA = -9999.
        comMB = -9999.
      ENDIF
    ELSE
      comMA = -9999.
      comMB = -9999.
    ENDIF

  END SUBROUTINE calculate_glacier_massbalance

END MODULE GLACIER_SOILMODEL
