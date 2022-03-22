!> \file sw_proc.f90
!> Contains module surfacewater_processes.

!>Lake and river water related subroutines in HYPE
MODULE SURFACEWATER_PROCESSES

  !Copyright 2012-2020 SMHI
  !
  !This file is part of HYPE.
  !HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
  !HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
  !You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.
  !------------------------------------------------------------------------
  USE STATETYPE_MODULE
  USE GENERAL_WATER_CONCENTRATION, ONLY : remove_water,       &
                                          error_remove_water, &
                                          add_water, &
                                          calculate_water_fractions
  USE SOIL_PROCESSES, ONLY : calculate_snowmelt,  &
                             calculate_snowdepth,  &
                             snowalbedo_function,   &
                             set_evaporation_concentrations

  !Also uses hypevariables, modvar, general_functions
  IMPLICIT NONE
  PRIVATE
  !--------------------------------------
  !Private procedures
  !--------------------------------------
  ! set_rivertemp
  ! calc_qbank
  ! update_qbank
  ! get_current_lake_outflow_parameters
  ! get_current_production_flow
  ! apply_seasonal_factor_on_production_flow
  ! adjust_threshold_for_seasonal_variation
  ! get_current_rating_parameters
  ! calculate_lake_outlet_outflow
  ! calculate_maxprod_outflow
  ! average_flow_rating_curve
  ! recalculate_branched_flow_two_outlets
  ! calculate_waterice_heatflow
  ! riverice_riverwater_interaction
  ! calculate_snow_on_ice
  ! lake_epilimnion_depth
  ! calculate_lakeice_lakewater_interaction
  ! calculate_icedepth
  ! calculate_T2_transfer
  ! calculate_T2_transfer_upper2lower
  ! calculate_watersurface_heatbalance
  ! calculate_floodplain_volume
  ! calculate_floodplain_equilibriumlevel
  ! calculate_two_floodplain_equilibriumlevel_dp
  ! calculate_equilibrium_floodplain_level_eq1dp
  ! calculate_equilibrium_floodplain_level_eq2dp
  ! calculate_equilibrium_floodplain_level_eq3dp
  ! get_index_of_highest
  ! get_index_of_lowest
  ! get_matching_index
  ! calculate_interflow_between_floodplains2
  ! T2_processes_in_wetland
  !-------------------------------------
  PUBLIC :: calculate_landarea,  &
            calculate_riverlength,  &
            calculate_fractional_riverarea, &
            add_precipitation_to_river, &
            add_precipitation_to_floodplain, &
            calculate_river_evaporation, &
            calculate_floodplain_evaporation, &
            calculate_actual_lake_evaporation, &
            sum_upstream_area, &
            set_general_rating_k,  &
            calculate_water_temperature, &
            set_water_temperature,  &
            calculate_river_characteristics, &
            translation_in_river, &
            point_abstraction_from_main_river_inflow, &
            point_abstraction_from_main_river, &
            point_abstraction_from_outlet_lake, &
            water_transfer_from_outlet_lake, &
            add_water_transfer_to_main_river, &
            calculate_ilake_outflow, &
            calculate_ilakesection_outflow, &
            calculate_outflow_from_lakebasin_lake,&
            calculate_outflow_from_outlet_lake, &
            calculate_flow_from_outlet_lake_waterstage, &
            remove_outflow_from_lake, &
            calculate_olake_waterstage, &
            calculate_lakebasin_average_waterstage, &
            calculate_regamp_adjusted_waterstage, &
            calculate_branched_flow,  &
            recalculate_branched_flow, &
            set_lake_outlets, &
            calculate_lake_volume, &
            calculate_lake_hypolimnion_depth, &
            T2_processes_in_river, &
            T2_processes_in_lake, &
            ice_processes_in_lake, &
            ice_processes_in_river, &
            add_T2_concentration_in_precipitation_on_water, &
            get_rivertempvol, &
            calculate_floodplain_waterlevel, &
            calculate_waterbody_floodplain_interflow, &
            calculate_regional_floodplain_flows, &
            wetland_watermodel, get_wetland_threshold, &
            river_water_level, &
            ice_on_river, &
            initiate_lakeriverice, &
			calculate_HGDM_depressions_outflow

  !Private parameters, global in this module
  CHARACTER(LEN=46) :: errstring(11)  !error message for location of remove_water call
  PARAMETER (errstring = (/'evapotranspiration lake, less than lake volume',    &   !1
                           'evapotranspiration lake, more than lake volume',    &   !2
                           'evapotranspiration lake, slowlake part used   ',    &   !3
                           'lake outflow, no NPC simulation               ',    &   !4
                           'lake outflow, no division in parts (NPC sim)  ',    &   !5
                           'lake outflow, from fastlake part              ',    &   !6
                           'lake outflow, from slowlake part              ',    &   !7
                           'flow between fast- and slowlake parts         ',    &   !8
                           'flow between waterbody and floodplain         ',    &   !9
                           'flow between floodplain and waterbody         ',    &   !10
                           'evapotranspiration river                      ' /))     !11

  CONTAINS

  !>\brief Calculate land area of subbasins.
  !!The landarea include floodplains (dry or flooded).
  !!The land area does not include wetlands (iwet and owet).
  !>
  !\b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions)
  !----------------------------------------------------------------------------
  SUBROUTINE calculate_landarea(nsub,larea)

    USE MODVAR, ONLY : basin,classbasin, &
                       slc_ilake,slc_olake, &
                       slc_lriver,slc_mriver, &
                       slc_iwet,slc_owet, &
                       conduct, &
                       floodindex,flooding

    !Argument declarations
    INTEGER, INTENT(IN) :: nsub        !<Number of subbasins
    REAL, INTENT(OUT)   :: larea(nsub) !<Land area [m2]

    !Local variables
    INTEGER i

    !>\b Algorithm \n
    !>Calculate land area of subbasin
    larea = basin%area
    IF(slc_ilake>0)  larea = larea - basin(:)%area * classbasin(:,slc_ilake)%part
    IF(slc_lriver>0) larea = larea - basin%area * classbasin(:,slc_lriver)%part
    IF(slc_iwet>0) larea = larea - basin%area * classbasin(:,slc_iwet)%part
    IF(slc_owet>0) larea = larea - basin%area * classbasin(:,slc_owet)%part
    IF(.NOT.conduct%floodplain)THEN
      IF(slc_olake>0)  larea = larea - basin%area * classbasin(:,slc_olake)%part
      IF(slc_mriver>0) larea = larea - basin%area * classbasin(:,slc_mriver)%part
    ELSE
      DO i = 1,nsub
        IF(floodindex(i)>0)THEN
          IF(flooding(floodindex(i))%fpfmr>0.)THEN
            larea(i) = larea(i) - basin(i)%area * classbasin(i,slc_mriver)%part * flooding(floodindex(i))%fpfmr
          ELSE
            IF(slc_mriver>0) larea(i) = larea(i) - basin(i)%area * classbasin(i,slc_mriver)%part
          ENDIF
          IF(flooding(floodindex(i))%fpfol>0.)THEN
            larea(i) = larea(i) - basin(i)%area * classbasin(i,slc_olake)%part * flooding(floodindex(i))%fpfol
          ELSE
            IF(slc_olake>0) larea(i) = larea(i) - basin(i)%area * classbasin(i,slc_olake)%part
          ENDIF
        ELSE
          IF(slc_mriver>0) larea(i) = larea(i) - basin(i)%area * classbasin(i,slc_mriver)%part
          IF(slc_olake>0) larea(i) = larea(i) - basin(i)%area * classbasin(i,slc_olake)%part
        ENDIF
      ENDDO
    ENDIF
    !Calculate square root of landarea
    DO i = 1,nsub
      IF(larea(i)<0) larea(i)=0.   !Safe for all lake subbasin (1-1<0)
    ENDDO


  END SUBROUTINE calculate_landarea

  !>\brief Calculate riverlength for local streams and main rivers.
  !>
  !\b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions)
  !----------------------------------------------------------------------------
  SUBROUTINE calculate_riverlength(nsub,landarea,rivlength)

    USE MODVAR, ONLY : basin

    !Argument declarations
    INTEGER, INTENT(IN) :: nsub   !<Number of subbasins
    REAL, INTENT(IN)    :: landarea(nsub)       !<land area [m2]
    REAL, INTENT(OUT)   :: rivlength(2,nsub)    !<river length [m]

    !Local variables
    INTEGER i
    REAL default_rivlen(nsub)

    !>\b Algorithm \n
    !Calculate default river length as square root of landarea
    DO i = 1,nsub
      default_rivlen(i) = SQRT(landarea(i))
    ENDDO
    !>Set river length from GeoData, or if zero use default value, i.e. square root of land area
    rivlength(1,:) = basin(:)%rivlen(1)  !local river length
    WHERE(rivlength(1,:)==0) rivlength(1,:) = default_rivlen(:)
    rivlength(2,:) = basin(:)%rivlen(2)  !main river length
    WHERE(rivlength(2,:)==0) rivlength(2,:) = default_rivlen(:)

  END SUBROUTINE calculate_riverlength

  !>Calculate Fractional River Area
  !----------------------------------------------------------------------------
  SUBROUTINE calculate_fractional_riverarea(i,pooltype,area,riverstate,fracarea,effdepth)

    USE MODVAR, ONLY : genpar,    &
                       realzero
    USE HYPEVARIABLES, ONLY : ttpart,ttstep,m_fraxe,m_fraxm
    USE GENERAL_FUNCTIONS, ONLY: sigmoid_response

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<subbasin index
    INTEGER, INTENT(IN) :: pooltype !<river type (local or main)
    REAL, INTENT(IN)    :: area     !<river area (m2)
    TYPE(riverstatetype),INTENT(IN) :: riverstate  !<River state
    REAL, INTENT(OUT)   :: fracarea !<fractional river area (-)
    REAL, INTENT(OUT)   :: effdepth !<effective river depth (m)

    !Local variables
    REAL xs,xe,xm,ys,ye,x
    REAL totvol     !total river watercourse volume (m3)
    REAL fractionw,fractionpart,notused   !fractions of volume in river compartments
    REAL,ALLOCATABLE :: fractionqueue(:)  !fractions of volume in river compartments

    !>\b Algorithm \n
    !>Set default output values
    fracarea = 1.
    effdepth = 0.

    !Return if:
    IF(area<=realzero) RETURN ! input river area is 0
    IF(genpar(m_fraxe)==0.AND.genpar(m_fraxm)==0) RETURN  !function of fraction river area is not used

    !Set local parameters
    xe = genpar(m_fraxe)       !river depth where the fracarea response function should be at its maximum value (ye=1.)
    xm = genpar(m_fraxm)       !river depth where the fracarea response function should be at its maximum slope
    IF(xm>=xe.OR.xm<0.)THEN
      WRITE(6,*) 'ERROR: Parameter fraxm is not in the interval 0<=fraxm<fraxe'
      STOP 1
    ENDIF

    !Set some non-optional parameter values
    xs = 0.                    !river depth where the fracarea response function should be at its minimum value (ys=0.)
    ys = 0.                    !minimum value of the fracarea response function
    ye = 1.                    !maximum value of the fracarea response function

    !>Get river volume
    ALLOCATE(fractionqueue(ttstep(pooltype,i)))
    CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),0.,riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i),totvol,fractionw,notused,fractionqueue,fractionpart)
    IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)
    IF(totvol<=realzero) RETURN

    !>Calculate mean river water depth (x) assuming full river area
    x = totvol/area

    !>Calculate fractional river area with the sigmoid function
    fracarea = sigmoid_response(x,xs,xe,ys,ye,xm)

    !>Calculate effective river water depth
    IF(fracarea.GT.0.) effdepth = x/fracarea

  END SUBROUTINE calculate_fractional_riverarea

  !>\brief Add precipitation to river, according to volume of watercourse elements.
  !>
  !\b Reference ModelDescription Chapter Rivers and lakes (Rivers - Common river processes)
  !----------------------------------------------------------------------------
  SUBROUTINE add_precipitation_to_river(i,pooltype,area,prec,cprec,riverstate)

    USE MODVAR, ONLY : numsubstances
    USE HYPEVARIABLES, ONLY : ttpart,ttstep

    !Argument declarations
    INTEGER, INTENT(IN) :: i                          !<index of subbasin
    INTEGER, INTENT(IN) :: pooltype                   !<rivertype: 1=lriver, 2=mriver
    REAL, INTENT(IN)    :: area                       !<river area (m2)
    REAL, INTENT(IN)    :: prec                       !<precipitation (mm/timestep)
    REAL, INTENT(IN)    :: cprec(numsubstances)       !<concentration of precipitation
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River state

    !Local variables
    INTEGER l
    REAL precm3
    REAL totvol
    REAL waterfrac
    REAL fractionw,fractionpart,notused   !fractions of volume in river compartments
    REAL,ALLOCATABLE :: fractionqueue(:)   !fractions of volume in river compartments

    !>\b Algorithm \n
    precm3 = prec * 1.E-3 * area

    !>Calculate fractions (of precipitation) to be added to river water compartments
    ALLOCATE(fractionqueue(ttstep(pooltype,i)))
    CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),0.,riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i),totvol,fractionw,notused,fractionqueue,fractionpart)

    !>Add precipitation to river watercourse for each compartment in relation to its volume fraction
    IF(totvol>0.)THEN
      IF(fractionw>0.)THEN
        waterfrac = precm3 * fractionw
        CALL add_water(numsubstances,riverstate%water(pooltype,i),riverstate%conc(:,pooltype,i),waterfrac,cprec)
      ENDIF
      DO l = 1,ttstep(pooltype,i)
        IF(fractionqueue(l)>0.)THEN
          waterfrac = precm3 * fractionqueue(l)
          CALL add_water(numsubstances,riverstate%qqueue(l,pooltype,i),riverstate%cqueue(:,l,pooltype,i),waterfrac,cprec)
        ENDIF
      ENDDO
      IF(ttpart(pooltype,i)>0)THEN
        l = ttstep(pooltype,i) + 1
        IF(fractionpart>0.)THEN
          waterfrac = precm3 *fractionpart    !Note: fraction is based on whole volume so that remaining outflow will be correct
          CALL add_water(numsubstances,riverstate%qqueue(l,pooltype,i),riverstate%cqueue(:,l,pooltype,i),waterfrac,cprec)
        ENDIF
      ENDIF
    ELSE
      !>If no river volume add all precipitation to river water compartment
      riverstate%water(pooltype,i) = precm3
    ENDIF
    IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)

  END SUBROUTINE add_precipitation_to_river

  !>\brief Add precipitation to river floodplain
  !>
  !\b Reference ModelDescription Chapter Rivers and lakes (Floodplains)
  !----------------------------------------------------------------------------
  SUBROUTINE add_precipitation_to_floodplain(i,pooltype,area,prec,cprec,miscstate,load)

    USE MODVAR, ONLY : numsubstances

    !Argument declarations
    INTEGER, INTENT(IN) :: i                          !<index of subbasin
    INTEGER, INTENT(IN) :: pooltype                   !<type: 1=mriver, 2=olake
    REAL, INTENT(IN)    :: area                       !<flooded area (m2)
    REAL, INTENT(IN)    :: prec                       !<precipitation (mm/timestep)
    REAL, INTENT(IN)    :: cprec(numsubstances)       !<concentration of precipitation
    TYPE(miscstatetype),INTENT(INOUT) :: miscstate    !<Floodplain state
    REAL, INTENT(OUT)   :: load(numsubstances)        !<load of precipitation

    !Local variables
    REAL precm3   ![m3]

    precm3 = prec * 1.E-3 * area
    load = 0.

    !Add precipitation to river watercourse
    CALL add_water(numsubstances,miscstate%floodwater(pooltype,i),miscstate%cfloodwater(:,pooltype,i),precm3,cprec)

    !Calculate load
    IF(numsubstances>0) load = cprec*prec*area

  END SUBROUTINE add_precipitation_to_floodplain

  !>\brief Calculate and remove evaporation from river
  !>
  !>\b Reference ModelDescription Chapters Rivers and lakes (Rivers - Common river processes)
  !> and Processes above ground (Evaporation)
  !----------------------------------------------------------------------------------------
  SUBROUTINE calculate_river_evaporation(i,j,pooltype,numsubst,area,temp,epot,evap,cevapT1,riverstate)

    USE MODVAR, ONLY : basin,classdata, &
                       landpar,   &
                       !cwater,    &
                       simulate, &
                       i_t1, &
                       realzero
    USE HYPEVARIABLES, ONLY : m_ttmp,ttpart,ttstep, &
                              m_T1evap

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<subbasin index
    INTEGER, INTENT(IN) :: j        !<class index
    INTEGER, INTENT(IN) :: pooltype !<river type (local or main)
    INTEGER, INTENT(IN) :: numsubst !<number of substances modelled
    REAL, INTENT(IN)    :: area     !<river area (m2)
    REAL, INTENT(IN)    :: temp     !<air temperature
    REAL, INTENT(IN)    :: epot     !<potential evapotranspiration (mm/timestep)
    REAL, INTENT(OUT)   :: evap     !<actual evapotranspiration (mm/timestep)
    REAL, INTENT(OUT)   :: cevapT1  !<concentration of T1 in evapotranspiration
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<Lake state

    !Local variables
    INTEGER l     !loop-variable, substance/queue
    INTEGER status  !error status of subroutine
    REAL tt         !threshold temperature for evaporation (C)
    REAL evapm3     !actual evaporation in m3
    REAL totvol     !total river watercourse volume (m3)
    REAL waterfrac  !fraction of water to be removed
    REAL fractionw,fractionpart,notused   !fractions of volume in river compartments
    REAL cevap(numsubst)  !concentration in evapotranspiration
    REAL,ALLOCATABLE :: fractionqueue(:)  !fractions of volume in river compartments

    !>\b Algorithm \n
    !>Set default values output variables (zero evaporation)
    evap = 0.
    cevap = 0.
    cevapT1 = 0.

    !Set local parameter
    tt = landpar(m_ttmp,classdata(j)%luse)       !Threshold temperature for snow melt and evaporation

    IF(temp > tt) THEN
      !>If temperature is above threshold river evaporation is potential
      evapm3 = epot*area*1.E-3
      !>Set concentration of evaporation from damping box
      IF(numsubst>0) CALL set_evaporation_concentrations(riverstate%conc(:,pooltype,i),cevap)

      !>Calculate fractions of river water compartments
      ALLOCATE(fractionqueue(ttstep(pooltype,i)))
      CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),0.,riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i),totvol,fractionw,notused,fractionqueue,fractionpart)
      IF(totvol<=realzero)THEN
        IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)
        RETURN
      ENDIF

      !>Check if enough water is available for evaporation in each compartment
      IF(evapm3+realzero<totvol)THEN
        !>Remove evaporation from river watercourse compartments
        IF(fractionw>0.)THEN
          waterfrac = fractionw *evapm3
          IF(simulate%substance(i_t1)) cevapT1 = cevapT1 + fractionw * cevap(i_t1) !accumulate evaporating T1
          IF(waterfrac+realzero<riverstate%water(pooltype,i))THEN
            CALL remove_water(riverstate%water(pooltype,i),numsubst,riverstate%conc(:,pooltype,i),waterfrac,cevap,status)
            IF(status.NE.0) CALL error_remove_water(errstring(11),basin(i)%subid,i,j)
          ELSE
            riverstate%water(pooltype,i) = 0.
            IF(numsubst>0.) riverstate%conc(:,pooltype,i) = 0.
          ENDIF
        ENDIF
        DO l = 1,ttstep(pooltype,i)
          IF(fractionqueue(l)>0)THEN
            waterfrac = fractionqueue(l)*evapm3
            !>Set concentration of evaporation from queue
            IF(numsubst>0) CALL set_evaporation_concentrations(riverstate%cqueue(:,l,pooltype,i),cevap)
            IF(simulate%substance(i_t1)) cevapT1 = cevapT1 + fractionqueue(l) * cevap(i_t1) !accumulate evaporating T1
            IF(waterfrac+realzero<riverstate%qqueue(l,pooltype,i))THEN
              CALL remove_water(riverstate%qqueue(l,pooltype,i),numsubst,riverstate%cqueue(:,l,pooltype,i),waterfrac,cevap,status)
              IF(status.NE.0) CALL error_remove_water(errstring(11),basin(i)%subid,i,j)
            ELSE
              riverstate%qqueue(l,pooltype,i) = 0.
              IF(numsubst>0.) riverstate%cqueue(:,l,pooltype,i) = 0.
            ENDIF
          ENDIF
        ENDDO
        IF(ttpart(pooltype,i)>0)THEN
          IF(fractionpart>0)THEN
            l = ttstep(pooltype,i) + 1
            waterfrac = fractionpart*evapm3    !Note whole volume so that pool get correct concentration change
            !>Set concentration of evaporation from queue
            IF(numsubst>0) CALL set_evaporation_concentrations(riverstate%cqueue(:,l,pooltype,i),cevap)
            IF(simulate%substance(i_t1)) cevapT1 = cevapT1 + fractionpart * ttpart(pooltype,i) * cevap(i_t1) !accumulate evaporating T1
            IF(waterfrac+realzero<riverstate%qqueue(l,pooltype,i))THEN
              CALL remove_water(riverstate%qqueue(l,pooltype,i),numsubst,riverstate%cqueue(:,l,pooltype,i),waterfrac,cevap,status)
              IF(status.NE.0) CALL error_remove_water(errstring(11),basin(i)%subid,i,j)
            ELSE
              riverstate%qqueue(l,pooltype,i) = 0.
              IF(numsubst>0.) riverstate%cqueue(:,l,pooltype,i) = 0.
            ENDIF
          ENDIF
        ENDIF
        evap = epot
      ELSE
        !>If less water than wanted, remove last traces of substance with the evaporation
        evapm3 = totvol
        IF(numsubst>0) CALL set_evaporation_concentrations(riverstate%conc(:,pooltype,i),cevap)
        IF(simulate%substance(i_t1)) cevapT1 = fractionw * cevap(i_t1) !accumulate evaporating T1
        riverstate%water(pooltype,i) = 0.
        IF(numsubst>0.) riverstate%conc(:,pooltype,i) = 0.
        DO l = 1,ttstep(pooltype,i)
          IF(numsubst>0) CALL set_evaporation_concentrations(riverstate%cqueue(:,l,pooltype,i),cevap)
          IF(simulate%substance(i_t1)) cevapT1 = cevapT1 + fractionqueue(l) * cevap(i_t1) !accumulate evaporating T1
          riverstate%qqueue(l,pooltype,i) = 0.
          IF(numsubst>0.) riverstate%cqueue(:,l,pooltype,i) = 0.
        ENDDO
        IF(ttpart(pooltype,i)>0)THEN
          l = ttstep(pooltype,i) + 1
          IF(numsubst>0) CALL set_evaporation_concentrations(riverstate%cqueue(:,l,pooltype,i),cevap)
          IF(simulate%substance(i_t1)) cevapT1 = cevapT1 + fractionpart * ttpart(pooltype,i) * cevap(i_t1) !accumulate evaporating T1
          riverstate%qqueue(l,pooltype,i) = 0.
          IF(numsubst>0.) riverstate%cqueue(:,l,pooltype,i) = 0.
        ENDIF
        evap = evapm3/area*1000.
      ENDIF
    ENDIF
    IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)

  END SUBROUTINE calculate_river_evaporation

  !>\brief Calculate and remove evaporation from floodplain
  !>
  !\b Reference ModelDescription Chapter Rivers and Lakes (Rivers - Common river processes)
  !----------------------------------------------------------------------------------------
  SUBROUTINE calculate_floodplain_evaporation(i,j,pooltype,numsubst,area,temp,epot,evap,cevap,miscstate)

    USE MODVAR, ONLY : basin,classdata, &
                       landpar, &
                       realzero
    USE HYPEVARIABLES, ONLY : m_ttmp, &
                              m_T1evap

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<subbasin index
    INTEGER, INTENT(IN) :: j        !<class index
    INTEGER, INTENT(IN) :: pooltype !<type 1= main river, 2=olake
    INTEGER, INTENT(IN) :: numsubst !<number of substances modelled
    REAL, INTENT(IN)    :: area    !<floodplain area (m2)
    REAL, INTENT(IN)    :: temp     !<air temperature
    REAL, INTENT(IN)    :: epot     !<potential evapotranspiration (mm/timestep)
    REAL, INTENT(OUT)   :: evap     !<actual evapotranspiration (mm/timestep)
    REAL, INTENT(OUT)   :: cevap(numsubst) !<concentration in evapotranspiration (eg. mg/L)
    TYPE(miscstatetype),INTENT(INOUT) :: miscstate  !<Floodplain state

    !Local variables
    INTEGER status  !error status of subroutine
    REAL tt         !threshold temperature for evaporation (C)
    REAL evapm3     !actual evaporation in m3
    REAL totvol     !total river watercourse volume (m3)

    !Default values output variables
    evap = 0.
    cevap = 0.

    !Available water
    totvol = miscstate%floodwater(pooltype,i)
    IF(totvol<=realzero) RETURN

    !Set local parameter
    tt = landpar(m_ttmp,classdata(j)%luse)       !Threshold temperature for snow melt and evaporation

    IF(temp > tt) THEN
      !Potential evaporation is default for temperature above threshold
      evapm3 = epot*area*1.E-3
      !Set concentration of evaporation
      IF(numsubst>0) CALL set_evaporation_concentrations(miscstate%cfloodwater(:,pooltype,i),cevap)

      !Check if enough water is available for evaporation
      IF(evapm3+realzero<totvol)THEN
        !Remove evaporation from flooddplain
        CALL remove_water(miscstate%floodwater(pooltype,i),numsubst,miscstate%cfloodwater(:,pooltype,i),evapm3,cevap,status)
        IF(status.NE.0) CALL error_remove_water(errstring(1),basin(i)%subid,i,j)
        evap = epot
      ELSE
        !Remove last traces of substance
        evapm3 = totvol
        miscstate%floodwater(pooltype,i) = 0.
        IF(numsubst>0) miscstate%cfloodwater(:,pooltype,i) = 0.
        evap = evapm3/area*1000.
      ENDIF
    ENDIF

  END SUBROUTINE calculate_floodplain_evaporation

  !>\brief Calculate total volume and mean T2 temperature concentration in river
  !----------------------------------------------------------------------------------------
  SUBROUTINE get_rivertempvol(i,pooltype,riverstate,meanrivertemp,totrivervol)

    USE MODVAR, ONLY : i_t2,realzero
    USE HYPEVARIABLES, ONLY : ttpart,ttstep

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<subbasin index
    INTEGER, INTENT(IN) :: pooltype !<river type (local or main)
    TYPE(riverstatetype),INTENT(IN) :: riverstate  !<River state
    REAL, INTENT(OUT)   :: meanrivertemp  !<temperature of river water
    REAL, INTENT(OUT)   :: totrivervol    !<volume of river water

    INTEGER l
    REAL fractionw1,fractionw2,fractionlast
    REAL,ALLOCATABLE :: fractionqueue(:)  !fractions of volume in river compartments

    !Total volume in all river elements (translation boxes and river volume)
    ALLOCATE(fractionqueue(ttstep(pooltype,i)))
    CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),0.,riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i),totrivervol,fractionw1,fractionw2,fractionqueue,fractionlast)

    IF(totrivervol>realzero)THEN
      !Weighted average T2 concentration
      IF(fractionw1>0.) meanrivertemp = riverstate%conc(i_t2,pooltype,i) * fractionw1
      DO l = 1,ttstep(pooltype,i)
        IF(fractionqueue(l)>0.)THEN
          meanrivertemp = meanrivertemp + riverstate%cqueue(i_t2,l,pooltype,i) * fractionqueue(l)
        ENDIF
      ENDDO
      IF(fractionlast>0.)THEN
        l = ttstep(pooltype,i) + 1
        meanrivertemp = meanrivertemp + riverstate%cqueue(i_t2,l,pooltype,i) * ttpart(pooltype,i) * fractionlast
      ENDIF
    ELSE
      meanrivertemp = 0.
    ENDIF
    IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)

  END SUBROUTINE get_rivertempvol

  !>\brief Set a T2 temperature concentration to all river elements
  !----------------------------------------------------------------------------------------
  SUBROUTINE set_rivertemp(i,pooltype,riverstate,meanrivertemp)

    USE MODVAR, ONLY : i_t2
    USE HYPEVARIABLES, ONLY : ttpart,ttstep

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<subbasin index
    INTEGER, INTENT(IN) :: pooltype !<river type (local or main)
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River state
    REAL, INTENT(IN)   :: meanrivertemp !<temperature of river

    INTEGER l

    !Riverbox
    IF(riverstate%water(pooltype,i).GT.0.)THEN
      riverstate%conc(i_t2,pooltype,i) = meanrivertemp
    ELSE
      riverstate%conc(i_t2,pooltype,i) = 0.
    ENDIF

    !Translation boxes
    DO l = 1,ttstep(pooltype,i)
      IF(riverstate%qqueue(l,pooltype,i)>0)THEN
        riverstate%cqueue(i_t2,l,pooltype,i) = meanrivertemp
      ELSE
        riverstate%cqueue(i_t2,l,pooltype,i) = 0.
      ENDIF
    ENDDO

    IF(ttpart(pooltype,i)>0)THEN
      l = ttstep(pooltype,i) + 1
      IF(riverstate%qqueue(l,pooltype,i)>0)THEN
        riverstate%cqueue(i_t2,l,pooltype,i) = meanrivertemp
      ELSE
        riverstate%cqueue(i_t2,l,pooltype,i) = 0.
      ENDIF
    ENDIF

  END SUBROUTINE set_rivertemp

  !>\brief Calculate and remove evaporation from lake
  !>
  !> \b Reference ModelDescription Chapter Processes above ground (Evaporation)
  !----------------------------------------------------------------------------------------
  SUBROUTINE calculate_actual_lake_evaporation(i,j,itype,numsubst,temp,epot,evap,cevap,lakestate)

    USE MODVAR, ONLY : basin,classdata, &
                       landpar, &
                       realzero
    USE HYPEVARIABLES, ONLY : m_ttmp,m_T1evap

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<subbasin index
    INTEGER, INTENT(IN) :: j        !<class index
    INTEGER, INTENT(IN) :: itype    !<lake type (ilake or olake)
    INTEGER, INTENT(IN) :: numsubst !<number of substances modelled
    REAL, INTENT(IN)    :: temp     !<air temperature
    REAL, INTENT(IN)    :: epot     !<potential evapotranspiration (mm/timestep) (reduced for partly ice covered lake)
    REAL, INTENT(OUT)   :: evap     !<actual evapotranspiration (mm/timestep)
    REAL, INTENT(OUT)   :: cevap(numsubst) !<concentration in evapotranspiration (eg. mg/L)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state

    !Local variables
    INTEGER status  !error status of subroutine
    REAL tt         !threshold temperature for evaporation (C)

    !> \b Algorithm \n
    !>Set default values output variables
    evap = 0.
    cevap = 0.

    !>Set local parameter
    tt = landpar(m_ttmp,classdata(j)%luse)       !Threshold temperature for snow melt and evaporation

    IF(temp>tt .AND. epot>0) THEN

      !>Set actual evaporation to potential evaporation, which is the default for temperature above threshold
      evap = epot

      !>Remove evaporation from lake, check if enough water is available
      IF(evap+realzero<lakestate%water(itype,i))THEN
        CALL set_evaporation_concentrations(lakestate%conc(:,itype,i),cevap)
        CALL remove_water(lakestate%water(itype,i),numsubst,lakestate%conc(:,itype,i),evap,cevap,status)
        IF(status.NE.0) CALL error_remove_water(errstring(1),basin(i)%subid,i,j)
      ELSE
        !not enough water, empty the lake
        evap = lakestate%water(itype,i)
        lakestate%water(itype,i) = 0.
        IF(numsubst>0)THEN
          cevap = lakestate%conc(:,itype,i)    !remove last traces of substances when lake dries out
          lakestate%conc(:,itype,i) = 0.
        ENDIF
      ENDIF
    ENDIF

  END SUBROUTINE calculate_actual_lake_evaporation

  !>Subroutine for summation of the area upstream of the outlet of all
  !>subbasins of the catchment
  !-------------------------------------------------------------------
  SUBROUTINE sum_upstream_area(n,areasum)

    USE MODVAR, ONLY : path,        &
                       basin,       &
                       branchdata,  &
                       branchindex

    !Argument declarations
    INTEGER, INTENT(IN)  :: n             !<number of subbasins
    REAL, INTENT(OUT)    :: areasum(n)    !<upstream area (m2)

    !Local variables
    INTEGER i,j,k,m         !loop variables
    REAL usarea             !summation variable for upstream area
    INTEGER, DIMENSION(n) :: A
    LOGICAL branchexists    !flag for branchdata available

    branchexists = .FALSE.
    IF(ALLOCATED(branchdata)) branchexists = .TRUE.
    A = 0
    areasum = 0.

    DO i = 1,n
      k = 0
      m = 0
      USarea = 0.
      DO j = 1,n
        IF(branchexists)THEN
          IF(branchindex(j)>0)THEN  !branch for this subbasin
            IF(i == path(j)%main)THEN
              m = m + A(j)
              USarea = USarea + areasum(j) * branchdata(branchindex(j))%mainpart
              k = k + 1
            ENDIF
            IF(i == branchdata(branchindex(j))%branch)THEN
              m = m + a(j)
              usarea = usarea + areasum(j)*(1.-branchdata(branchindex(j))%mainpart)
              k = k + 1
            ENDIF
          ELSE  !no branch for this subbasin
            IF(i == path(j)%main)THEN
              m = m + A(j)
              usarea = usarea + areasum(j)
              k = k + 1
            ENDIF
          ENDIF
        ELSE    !no branches at all in the model set-up
           IF(i == path(j)%main)THEN
              m = m + a(j)
              usarea = usarea + areasum(j)
              k = k + 1
           ENDIF
        ENDIF
      ENDDO
      IF(k==0) THEN                 !no inflows
        A(i) = 1
        areasum(i) = basin(i)%area
      ELSEIF(k==m) THEN             !k inflow, m (all) have their upstream area ready
        A(i) = 1
        areasum(i) = USarea + basin(i)%area
      ELSE                          !not all inflow have their upstream area ready (m<k)
        A(i) = 0                    !this indicates an error in coupling
        areasum(i) = 0.

        WRITE(6,*) 'ERROR in coupling of subbasins, some downstream basin before upstream basin'
        WRITE(6,*) 'i= ',i,' subid= ',basin(i)%subid
        WRITE(6,*) '(k= ',k,' m= ',m,')'
        STOP 1
      ENDIF
    ENDDO

  END SUBROUTINE sum_upstream_area

  !>Subroutine for calculation of general rating curve k-value for each lake
  !!
  !\b Reference ModelDescription Chapter Rivers and lakes (Lakes - Common lake processes)
  !-------------------------------------------------------------------
  SUBROUTINE set_general_rating_k(nl,n,locarea,areasum,rating)

    USE HYPEVARIABLES, ONLY : m_grat1,  &
                              m_grat3,   &
                              m_ratcorr, &
                              m_ilrrat1,m_olrrat1
    USE MODVAR, ONLY : genpar,  &
                       regpar,  &
                       basin, &
                       regiondivision

    !Argument declarations
    INTEGER, INTENT(IN)  :: nl            !<number of lake types
    INTEGER, INTENT(IN)  :: n             !<number of subbasins
    REAL, INTENT(IN)     :: locarea(n)    !<landarea of subbasin [m2]   !TODO: here should land+iwt+lriver be used
    REAL, INTENT(IN)     :: areasum(n)    !<upstream area [m2]          !TODO: here olake area should not be
    REAL, INTENT(OUT)    :: rating(nl,n)  !<k-value of rating curve

    !Local variables
    INTEGER i         !loop variables
    REAL ratcorr
    REAL upailakekm2,upaolakekm2  !upstream area of lake in km2

    !Initial value
    rating = 0

    DO i = 1,n
      IF(basin(i)%parregion(regiondivision(m_ratcorr))>0)THEN
        ratcorr = 1. + regpar(m_ratcorr,basin(i)%parregion(regiondivision(m_ratcorr)))   !Correction of general rating curve grat1-parameter
      ELSE
        ratcorr = 1.
      ENDIF
      IF(basin(i)%parregion(regiondivision(m_ilrrat1))>0) rating(1,i) = regpar(m_ilrrat1,basin(i)%parregion(regiondivision(m_ilrrat1))) * ratcorr
      IF(basin(i)%parregion(regiondivision(m_olrrat1))>0) rating(2,i) = regpar(m_olrrat1,basin(i)%parregion(regiondivision(m_olrrat1))) * ratcorr
      IF(rating(1,i)<=0.) rating(1,i) = genpar(m_grat1) * ratcorr
      IF(rating(2,i)<=0.) rating(2,i) = genpar(m_grat1) * ratcorr
    ENDDO

    IF(genpar(m_grat3)>0.)THEN
      DO i = 1,n
        upailakekm2 = locarea(i)*basin(i)%ilakecatch*1.E-6
        IF(upailakekm2>0.) rating(1,i) = rating(1,i)*upailakekm2**genpar(m_grat3)
        upaolakekm2 = areasum(i)*1.E-6
        IF(upaolakekm2>0.) rating(2,i) = rating(2,i)*upaolakekm2**genpar(m_grat3)
      ENDDO
    ENDIF

  END SUBROUTINE set_general_rating_k

  !>\brief Calculates temperature of rivers and lakes and other temperature variables
  !>
  !>These calculations is for modeloption swtemperature 0, the default. River temperature
  !>is calculated as a 20-day moving average of air temperature. Lake temperature is
  !>calculated as a 5-day moving average of air temperature. Also 10- and 20-day
  !>moving average of lake- and river temperature is calculated for determining
  !>rising or decreasing temperature (season).
  !-----------------------------------------------------------------------
  SUBROUTINE calculate_water_temperature(i,airtemp,riverstate,lakestate)

    USE MODVAR, ONLY : genpar,        &
                       basin,         &
                       classbasin,    &
                       slc_ilake,     &
                       slc_olake,     &
                       i_in,i_sp,i_oc,i_ae,  &
                       timesteps_per_day
    USE HYPEVARIABLES, ONLY : m_laketemp

    !Argument declarations
    INTEGER, INTENT(IN) :: i                 !<index of current subbasin
    REAL, INTENT(IN)    :: airtemp           !<air temperature for subbasin
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT)  :: lakestate   !<Lake states

    !Local parameters
    REAL, PARAMETER :: rivertemp_days = 20.     !Number of days for river temperature calculation
    REAL, PARAMETER :: laketemp_days  = 5.      !Number of days for lake temperature calculation
    REAL, PARAMETER :: T10day_parameter = 10.
    REAL, PARAMETER :: T20day_parameter = 20.

    !Local variables
    INTEGER watertype               !Internal or main/outlet
    REAL    mtimesteps,mtimesteps2  !Number of timesteps temperature is averaged over

    !>\b Algorithm \n
    !>Calculate river temperature, same for local and main river
    IF(timesteps_per_day==1)THEN
      riverstate%temp(:,i) = riverstate%temp(:,i) + ((airtemp - riverstate%temp(:,i)) / rivertemp_days)
    ELSE
      mtimesteps = timesteps_per_day*rivertemp_days
      riverstate%temp(:,i) = riverstate%temp(:,i) + ((airtemp - riverstate%temp(:,i)) / mtimesteps)
    ENDIF

    !>Calculate lake temperature, same for internal and outlet lake (if exist)
    IF(genpar(m_laketemp)==0)THEN
      !>\li If parameter not set: as 5-day moving average
      IF(slc_ilake>0)THEN
        IF(classbasin(i,slc_ilake)%part>0)THEN
          IF(timesteps_per_day==1)THEN
            lakestate%temp(1,i) = lakestate%temp(1,i) + ((airtemp - lakestate%temp(1,i)) / laketemp_days)
          ELSE
            mtimesteps = timesteps_per_day*laketemp_days
            lakestate%temp(1,i) = lakestate%temp(1,i) + ((airtemp - lakestate%temp(1,i)) / mtimesteps)
          ENDIF
        ENDIF
      ENDIF
      IF(slc_olake>0)THEN
        IF(classbasin(i,slc_olake)%part>0)THEN
          IF(timesteps_per_day==1)THEN
            lakestate%temp(2,i) = lakestate%temp(2,i) + ((airtemp - lakestate%temp(2,i)) / laketemp_days)
          ELSE
            mtimesteps = timesteps_per_day*laketemp_days
            lakestate%temp(2,i) = lakestate%temp(2,i) + ((airtemp - lakestate%temp(2,i)) / mtimesteps)
          ENDIF
        ENDIF
      ENDIF
    ELSE
      !>\li Elseif parameter set: as a moving average of a period determined by lake depth
      IF(slc_ilake>0)THEN
        IF(classbasin(i,slc_ilake)%part>0)THEN
          mtimesteps = timesteps_per_day*MIN(MAX(basin(i)%lakedepth(1),5.),5.+genpar(m_laketemp))
          lakestate%temp(1,i) = lakestate%temp(1,i) + ((airtemp - lakestate%temp(1,i)) / mtimesteps)
        ENDIF
      ENDIF
      IF(slc_olake>0)THEN
        IF(classbasin(i,slc_olake)%part>0)THEN
          mtimesteps = timesteps_per_day*MIN(MAX(basin(i)%lakedepth(2),5.),5.+genpar(m_laketemp))
          lakestate%temp(2,i) = lakestate%temp(2,i) + ((airtemp - lakestate%temp(2,i)) / mtimesteps)
        ENDIF
      ENDIF
    ENDIF

    !>Calculate 10- and 20-day mean of water temperature for N,P,C or S processes
    IF(i_in>0 .OR. i_sp>0 .OR. i_oc>0 .OR. i_ae>0)THEN
      mtimesteps = timesteps_per_day*t10day_parameter
      mtimesteps2 = timesteps_per_day*t20day_parameter
      DO watertype = 1,2                   !(1=local/internal, 2=main/outlet)
        lakestate%temp10(watertype,i) = lakestate%temp10(watertype,i) + ((lakestate%temp(watertype,i) - lakestate%temp10(watertype,i)) / mtimesteps)
        lakestate%temp20(watertype,i) = lakestate%temp20(watertype,i) + ((lakestate%temp(watertype,i) - lakestate%temp20(watertype,i)) / mtimesteps2)
        riverstate%temp10(watertype,i) = riverstate%temp10(watertype,i) + ((riverstate%temp(watertype,i) - riverstate%temp10(watertype,i)) / mtimesteps)
        riverstate%temp20(watertype,i) = riverstate%temp20(watertype,i) + ((riverstate%temp(watertype,i) - riverstate%temp20(watertype,i)) / mtimesteps2)
      ENDDO
    ENDIF

  END SUBROUTINE calculate_water_temperature

  !>\brief Set temperature of rivers and lakes from T2 and calculate other temperature variables
  !>
  !>These calculations is for modeloption swtemperature 1. River and lake temperature
  !>is set to calculated T2 temperature of each water body. Also 10- and 20-day
  !>moving average of lake- and river temperature is calculated for determining
  !>rising or decreasing temperature (season).
  !-----------------------------------------------------------------------
  SUBROUTINE set_water_temperature(waterbody,i,riverstate,lakestate)

    USE MODVAR, ONLY : classbasin,  &
                       slc_ilake,     &
                       slc_olake,     &
                       i_in,i_sp,i_oc,i_t2,i_ae,  &
                       timesteps_per_day

    !Argument declarations
    INTEGER, INTENT(IN) :: waterbody         !<flag for waterbody, 1=lstream,2=ilake,3=main river,4=olake
    INTEGER, INTENT(IN) :: i                 !<index of current subbasin
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states
    TYPE(lakestatetype),INTENT(INOUT)  :: lakestate   !<Lake states

    !Local parameters
    REAL, PARAMETER :: T10day_parameter = 10.
    REAL, PARAMETER :: T20day_parameter = 20.

    !Local variables
    INTEGER watertype               !Internal or main/outlet
    REAL    mtimesteps,mtimesteps2  !Number of timesteps temperature is averaged over

    !>\b Algorithm \n
    !>Set river temperature to T2 temperature
    IF(waterbody==1) riverstate%temp(1,i) = riverstate%conc(i_t2,1,i)
    IF(waterbody==3) riverstate%temp(2,i) = riverstate%conc(i_t2,2,i)

    !>Set lake temperature (if exist)
    IF(waterbody==2)THEN
      IF(slc_ilake>0)THEN
        IF(classbasin(i,slc_ilake)%part>0)THEN
          lakestate%temp(1,i) = lakestate%conc(i_t2,1,i)
        ENDIF
      ENDIF
    ENDIF
    IF(waterbody==4)THEN
      IF(slc_olake>0)THEN
        IF(classbasin(i,slc_olake)%part>0)THEN
          lakestate%temp(2,i) = lakestate%conc(i_t2,2,i)
        ENDIF
      ENDIF
    ENDIF

    !>Calculate 10- and 20-day mean of water temperature for N,P,C or S processes
    IF(i_in>0 .OR. i_sp>0 .OR. i_oc>0 .OR. i_ae>0)THEN
      mtimesteps = timesteps_per_day*t10day_parameter
      mtimesteps2 = timesteps_per_day*t20day_parameter
      watertype = 1                   !local/internal
      IF(waterbody==2) lakestate%temp10(watertype,i) = lakestate%temp10(watertype,i) + ((lakestate%temp(watertype,i) - lakestate%temp10(watertype,i)) / mtimesteps)
      IF(waterbody==2) lakestate%temp20(watertype,i) = lakestate%temp20(watertype,i) + ((lakestate%temp(watertype,i) - lakestate%temp20(watertype,i)) / mtimesteps2)
      IF(waterbody==1) riverstate%temp10(watertype,i) = riverstate%temp10(watertype,i) + ((riverstate%temp(watertype,i) - riverstate%temp10(watertype,i)) / mtimesteps)
      IF(waterbody==1) riverstate%temp20(watertype,i) = riverstate%temp20(watertype,i) + ((riverstate%temp(watertype,i) - riverstate%temp20(watertype,i)) / mtimesteps2)
      watertype = 2                   !main/outlet)
      IF(waterbody==4) lakestate%temp10(watertype,i) = lakestate%temp10(watertype,i) + ((lakestate%temp(watertype,i) - lakestate%temp10(watertype,i)) / mtimesteps)
      IF(waterbody==4) lakestate%temp20(watertype,i) = lakestate%temp20(watertype,i) + ((lakestate%temp(watertype,i) - lakestate%temp20(watertype,i)) / mtimesteps2)
      IF(waterbody==3) riverstate%temp10(watertype,i) = riverstate%temp10(watertype,i) + ((riverstate%temp(watertype,i) - riverstate%temp10(watertype,i)) / mtimesteps)
      IF(waterbody==3) riverstate%temp20(watertype,i) = riverstate%temp20(watertype,i) + ((riverstate%temp(watertype,i) - riverstate%temp20(watertype,i)) / mtimesteps2)
    ENDIF

  END SUBROUTINE set_water_temperature

  !>\brief Calculate river characteristics
  !>River characteristics include depth, area, bankful flow and 365-day-average-Q
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_river_characteristics(i,itype,flow,calcNPST,riverstate,depth,riverarea,qbank)

    USE MODVAR, ONLY : basin, &
                       regpar, &
                       genpar, &
                       regiondivision, &
                       timesteps_per_day
    USE HYPEVARIABLES, ONLY : riverlength,  &
                              deadwidth,  &
                              m_velpar1,  &
                              m_velpar2,  &
                              m_velpar3,  &
                              m_widpar1,  &
                              m_widpar2,  &
                              m_widpar3,  &
                              m_maxwidth

    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<index of current subbasin
    INTEGER, INTENT(IN) :: itype      !<lake type (ilake or olake)
    REAL, INTENT(IN)    :: flow       !<river flow (m3/s)
    LOGICAL, INTENT(IN) :: calcNPST   !<status of N,P,S,T1 simulation (to calculate bankful flow)
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states
    REAL, INTENT(OUT)   :: depth      !<river depth (m)
    REAL, INTENT(OUT)   :: riverarea  !<river surface area (m2)
    REAL, INTENT(OUT)   :: qbank      !<flow at bank-ful river channel (m3/s)

    !Local variables
    REAL par(6)      !model parameters for velocity and width of river
    REAL rlength     !river length (m)
    REAL velocity    !river velocity (m/s)
    REAL width       !river width (m)

    !>\b Algorithm \n
    !>Set parameter values
    IF(basin(i)%parregion(regiondivision(m_velpar1))>0)THEN
      par = (/regpar(m_velpar1,basin(i)%parregion(regiondivision(m_velpar1))),regpar(m_velpar2,basin(i)%parregion(regiondivision(m_velpar2))),regpar(m_velpar3,basin(i)%parregion(regiondivision(m_velpar3))),regpar(m_widpar1,basin(i)%parregion(regiondivision(m_widpar1))),regpar(m_widpar2,basin(i)%parregion(regiondivision(m_widpar2))),regpar(m_widpar3,basin(i)%parregion(regiondivision(m_widpar3))) /)
    ELSE
      par = (/0,0,0,0,0,0/)   !OK? gives vel=1, width=1 and depth=flow
    ENDIF

    !>Update state variable 365-day mean river discharge (m3/s)
    riverstate%Qmean(itype,i) = riverstate%Qmean(itype,i) + (flow-riverstate%Qmean(itype,i))/(365.*timesteps_per_day)

    !>River length,depth and width, depend on velocity
    rlength = riverlength(itype,i)
    depth = 0.020          !low flow default value
    width = depth * 5.     !low flow default value
    IF(riverstate%Qmean(itype,i)>0.01.AND.flow>0.) THEN
      velocity = (10**par(1)) * (riverstate%Qmean(itype,i)**par(2)) * ((flow/riverstate%Qmean(itype,i))**par(3))
      IF(velocity>0.2) THEN
        width = (10**par(4)) * (flow/velocity)**(par(5)+par(6)*LOG10(flow/velocity))
        depth = (flow / velocity) / width
      ENDIF
    ENDIF

    !>River (surface/bottom) area
    IF(genpar(m_maxwidth)>0)THEN
      riverarea = min(max(width,deadwidth(itype,i)),genpar(m_maxwidth)) * rlength
    ELSE
      riverarea = max(width,deadwidth(itype,i)) * rlength
    ENDIF

    !TODO: separate into two subroutines: Qmean,depth,area resp Qbank. Depth,area only needed if a=0? Qmean more? depth more?
    !>Calculate new bankfull flow, stored in Q2max.
    IF(calcNPST) CALL calc_qbank(flow,i,itype,riverstate%Q365(:,itype,i),riverstate%Qdayacc(:,itype,i),qbank)  !Subroutine also updates Qmax, Qdayacc and riverQ365.

  END SUBROUTINE calculate_river_characteristics

  !>Estimates the bank full flow by the second highest q from the
  !>daily values of last year
  !>
  !>\b Consequences Module hypevariables variables qmax, q2mqx, iqmax, and iq2max
  !> may change.
  !>
  !>\b Reference ModelDescription Chapter Rivers and lakes (Rivers - Common river processes)
  !---------------------------------------------------------------------
  SUBROUTINE calc_qbank(flow,i,itype,riverq365,Qdayacc,Qbank)

    USE HYPEVARIABLES, ONLY : qmax,q2max,   &  !OUT
                              iqmax,iq2max     !OUT
    USE MODVAR, ONLY : dayno,     &
                       timesteps_per_day, &
                       endofday,  &
                       tsofday

    !Argument declarations
    REAL, INTENT(IN)     :: flow    !<flow current time step (m3/s)
    INTEGER, INTENT(IN)  :: i       !<index of current subbasin
    INTEGER, INTENT(IN)  :: itype   !<river type 1=local, 2=main
    REAL,INTENT(INOUT)   :: riverq365(366)  !<river flow last 365 days (m3/s)
    REAL,INTENT(INOUT)   :: Qdayacc(timesteps_per_day)  !<river flow last day (m3/s)
    REAL, INTENT(OUT)    :: qbank   !<bankfull flow

    !local variables
    REAL q        !average flow for day (m3/s)

    !Accumulate flow values for calculation of daily mean
    Qdayacc(tsofday) = flow

    IF(endofday)THEN
      q = SUM(Qdayacc(:))/REAL(timesteps_per_day)
      riverq365(dayno) = q !First year: initial assignment, following years: overwrite oldest value
      !Estimate river bankful flow with second highest flow
      IF(dayno==iqmax(itype,i) .OR. dayno==iq2max(itype,i))THEN !too old values, search whole array for new
        CALL update_qbank(riverq365(:),qmax(itype,i),q2max(itype,i),iqmax(itype,i),iq2max(itype,i))
      ELSEIF(q > qmax(itype,i))THEN
        q2max(itype,i) = qmax(itype,i)     !new highest flow
        iq2max(itype,i) = iqmax(itype,i)
        qmax(itype,i) = q
        iqmax(itype,i) = dayno
      ELSEIF(q > q2max(itype,i))THEN    !new second highest flow
        q2max(itype,i) = q
        iq2max(itype,i) = dayno
      ENDIF
    ENDIF
    qbank = q2max(itype,i)

  END SUBROUTINE calc_qbank

  !>Update highest and second highest flow when one of them reach
  !>retiring age
  !---------------------------------------------------------------------
  SUBROUTINE update_qbank(q_array,qmax,q2,imax,i2)

    !Argument declarations
    REAL, INTENT(IN)     :: q_array(366)  !<flow all days last year
    REAL, INTENT(OUT)    :: qmax          !<highest flow all days last year
    REAL, INTENT(OUT)    :: q2            !<second highest flow all days last year
    INTEGER, INTENT(OUT) :: imax          !<index of highest flow all days last year
    INTEGER, INTENT(OUT) :: i2            !<index of second highest flow all days last year

    !Local variables
    INTEGER i

    qmax = 0.
    q2 = 0.

    DO i = 1, 366
      IF(q_array(i) >= qmax)THEN
        q2 = qmax
        i2 = imax
        qmax = q_array(i)
        imax = i
      ELSEIF(q_array(i) > q2)THEN
        q2 = q_array(i)
        i2 = i
      ENDIF
    ENDDO

  END SUBROUTINE update_qbank

  !>Translation (delay) in river
  !>
  !> \b Reference ModelDescription Chapter Rivers and lakes (Rivers - Common river processes)
  !-------------------------------------------------------------------
  SUBROUTINE translation_in_river(i,itype,qin,cin,qout,cout,riverstate)

    USE MODVAR, ONLY : numsubstances,     &
                       realzero,  &
                       seconds_per_timestep
    USE HYPEVARIABLES, ONLY : transtime,  &
                              ttstep,     &
                              ttpart

    !Argument declaration
    INTEGER, INTENT(IN) :: i                    !<index of current subbasin
    INTEGER, INTENT(IN) :: itype                !<river type (local or main)
    REAL, INTENT(IN)    :: qin                  !<inflow to river train (m3/s)
    REAL, INTENT(IN)    :: cin(numsubstances)   !<concentration of inflow to river train
    REAL, INTENT(OUT)   :: qout                 !<outflow of river train (m3/s)
    REAL, INTENT(OUT)   :: cout(numsubstances)  !<concentration of outflow of river train
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states

    !Local variables
    INTEGER y     !translation, whole time steps
    REAL    x     !translation, additional part of time step

    !>\b Algoritm \n
    !>Add new inflow to translation variable (river train)
    riverstate%qqueue(0,itype,i) = qin * seconds_per_timestep
    IF(numsubstances>0) riverstate%cqueue(:,0,itype,i) = cin

    IF(transtime(itype,i)>0)THEN
      !>Calculate outflow from river train
      y = ttstep(itype,i)
      x = ttpart(itype,i)
      qout = (1.-x)*riverstate%qqueue(y,itype,i) + x*riverstate%qqueue(y+1,itype,i) !Calculate flow (m3) from river after translation
      IF(qout>realzero .AND. numsubstances>0)THEN
        cout = ((1.-x)*riverstate%qqueue(y,itype,i)*riverstate%cqueue(:,y,itype,i) + &
                 x*riverstate%qqueue(y+1,itype,i)*riverstate%cqueue(:,y+1,itype,i))/qout
      ELSE
        cout = 0.
      ENDIF
      qout = qout / seconds_per_timestep  !flow (m3/s)

      !>Translate the flows in the river train
      riverstate%qqueue(1:y+1,itype,i) = riverstate%qqueue(0:y,itype,i)
      IF(numsubstances>0) riverstate%cqueue(:,1:y+1,itype,i) = riverstate%cqueue(:,0:y,itype,i)
    ELSE
      !Elseif no delay, outflow = inflow
      qout = qin
      IF(numsubstances>0) cout = cin
    ENDIF

  END SUBROUTINE translation_in_river


  !>\brief Abstraction of water from main river inflow and river
  !>Abstraction is taken from main river (before adding current inflow) and
  !>local and upstream river inflow
  !>
  !> \b Reference ModelDescription Chapter Water management (Point sources - Negative point source)
  !-------------------------------------------------------------------
  SUBROUTINE point_abstraction_from_main_river_inflow(i,pooltype,q,riverstate,removedflow)

    USE MODVAR, ONLY : basin, load,&
                       nabsused,absinfo,absload, &
                       conductwarning, &
                       seconds_per_timestep, &
                       find_next_pointsource
    USE HYPEVARIABLES, ONLY : ttstep,     &
                              ttpart

    !Argument declaration
    INTEGER, INTENT(IN) :: i                          !<index of current subbasin
    INTEGER, INTENT(IN) :: pooltype                   !<river type (local or main)
    REAL, INTENT(INOUT) :: q                          !<inflow (m3/s)
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<river states
    REAL, INTENT(OUT)   :: removedflow                !<removed flow (m3/timestep)

    !Local variables
    INTEGER l,lastps, ips
    REAL totvol      !volume in river (m3)
    REAL absvol      !abstraction volume to be removed (m3)
    REAL fractionw,fractionlast,fractionqin  !fractions of volume in river compartments
    REAL,ALLOCATABLE :: fractionqueue(:)     !fractions of volume in river compartments

    !>\b Algoritm \n
    removedflow = 0.
    IF(.NOT.ALLOCATED(absload))THEN      !Constant abstraction
      IF(load(i)%abstrind/=3) RETURN  !not main river inflow abstraction
      IF(load(i)%abstrvol==0) RETURN

      !>Calculate volume fractions of river water compartments
      ALLOCATE(fractionqueue(ttstep(pooltype,i)))
      CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),q*seconds_per_timestep,riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i),totvol,fractionw,fractionqin,fractionqueue,fractionlast)

      !>If abstraction of water: Calculate amount
      absvol = load(i)%abstrvol*seconds_per_timestep  !m3
      !>Remove abstraction water proportionally from river and queue
      IF(absvol<totvol)THEN
        IF(fractionqin>0.)THEN
          q = q - fractionqin*absvol/seconds_per_timestep
        ENDIF
        IF(fractionw>0.)THEN
          riverstate%water(pooltype,i) = riverstate%water(pooltype,i) - fractionw*absvol
        ENDIF
        DO l = 1,ttstep(pooltype,i)
          IF(fractionqueue(l)>0.)THEN
            riverstate%qqueue(l,pooltype,i) = riverstate%qqueue(l,pooltype,i) - fractionqueue(l)*absvol
          ENDIF
        ENDDO
        IF(fractionlast>0)THEN
          l = ttstep(pooltype,i) + 1
          riverstate%qqueue(l,pooltype,i) = riverstate%qqueue(l,pooltype,i) - fractionlast*absvol    !Note whole volume so that remaining outflow will be correct
        ENDIF
        removedflow = absvol
      ELSE
        q = 0.
        riverstate%water(pooltype,i) = 0.
        riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i) = 0.
        IF(absvol>totvol .AND. conductwarning)THEN
          WRITE(6,*) 'Warning: Point source abstraction from river could not be fulfilled, not enough water in river.'
          WRITE(6,*) 'Warning: subbasin ',basin(i)%subid, 'abstracted volume: ',totvol, 'of wished volume: ',absvol
        ENDIF
        removedflow = totvol
      ENDIF

    ELSE  !Timeseries abstraction
      lastps = 0
      DO
        CALL find_next_pointsource(i,lastps,nabsused,absinfo,ips)
        IF(ips==0) EXIT
        lastps = ips
        IF(absload(ips)%flow==0.) CYCLE
        IF(absinfo(ips)%sw_code/=3) CYCLE  !main river abstraction not from inflow

        !>Calculate volume fractions of river water compartments
        IF(.NOT.ALLOCATED(fractionqueue)) ALLOCATE(fractionqueue(ttstep(pooltype,i)))
        CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),q*seconds_per_timestep,riverstate%qqueue(1:ttstep(pooltype,i),pooltype,i),totvol,fractionw,fractionqin,fractionqueue,fractionlast)
        !>If abstraction of water: Calculate amount
        absvol = absload(ips)%flow*seconds_per_timestep  !m3
        !>Remove abstraction water proportionally from river and queue
        IF(absvol<totvol)THEN
          IF(fractionqin>0.)THEN
            q = q - fractionqin*absvol/seconds_per_timestep
          ENDIF
          IF(fractionw>0.)THEN
            riverstate%water(pooltype,i) = riverstate%water(pooltype,i) - fractionw*absvol
          ENDIF
          DO l = 1,ttstep(pooltype,i)
            IF(fractionqueue(l)>0.)THEN
              riverstate%qqueue(l,pooltype,i) = riverstate%qqueue(l,pooltype,i) - fractionqueue(l)*absvol
            ENDIF
          ENDDO
          IF(fractionlast>0)THEN
            l = ttstep(pooltype,i) + 1
            riverstate%qqueue(l,pooltype,i) = riverstate%qqueue(l,pooltype,i) - fractionlast*absvol    !Note whole volume so that remaining outflow will be correct
          ENDIF
          removedflow = removedflow + absvol
        ELSE
          q = 0.
          riverstate%water(pooltype,i) = 0.
          riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i) = 0.
          IF(absvol>totvol .AND. conductwarning)THEN
            WRITE(6,*) 'Warning: Point source abstraction from river could not be fulfilled, not enough water in river.'
            WRITE(6,*) 'Warning: subbasin ',basin(i)%subid, 'abstracted volume: ',totvol, 'of wished volume: ',absvol
          ENDIF
          removedflow = removedflow + totvol
        ENDIF
      ENDDO

    ENDIF

    IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)

  END SUBROUTINE point_abstraction_from_main_river_inflow

  !>\brief Abstraction of water from main river.
  !>Abstraction is taken from main river volume and queue after inflow from local and upstream rivers.
  !>
  !> \b Reference ModelDescription Chapter Water management (Point sources - Negative point source)
  !-------------------------------------------------------------------
  SUBROUTINE point_abstraction_from_main_river(i,pooltype,riverstate,removedflow)

    USE MODVAR, ONLY : basin,load, &
                       nabsused,absinfo,absload, &
                       conductwarning, &
                       seconds_per_timestep, &
                       find_next_pointsource
    USE HYPEVARIABLES, ONLY : ttstep, &
                              ttpart

    !Argument declaration
    INTEGER, INTENT(IN) :: i                          !<index of current subbasin
    INTEGER, INTENT(IN) :: pooltype                   !<river type (local or main)
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<river states
    REAL, INTENT(OUT)   :: removedflow                !<removed flow (m3/timestep)

    !Local variables
    INTEGER l,ips,lastps
    REAL    totvol      !volume in river (m3)
    REAL    absvol      !abstraction volume to be removed (m3)
    REAL fractionw,fractionpart,notused   !fractions of volume in river compartments
    REAL,ALLOCATABLE :: fractionqueue(:)  !fractions of volume in river compartments

    !>\b Algoritm \n
    removedflow = 0.
    IF(.NOT.ALLOCATED(absload))THEN
      !>Check if abstraction of water
      IF(load(i)%abstrind/=1) RETURN  !not main river abstraction
      IF(load(i)%abstrvol==0) RETURN

      !>Calculate volume fractions of river water compartments
      ALLOCATE(fractionqueue(ttstep(pooltype,i)))
      CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),0.,riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i),totvol,fractionw,notused,fractionqueue,fractionpart)

      !>Calculate volume of water to be abstracted
      absvol = load(i)%abstrvol*seconds_per_timestep  !m3
      !>Remove abstraction water proportionally from river and queue
      IF(absvol<totvol)THEN
        IF(fractionw>0.)THEN
          riverstate%water(pooltype,i) = riverstate%water(pooltype,i) - fractionw*absvol
        ENDIF
        DO l = 1,ttstep(pooltype,i)
          IF(fractionqueue(l)>0.)THEN
            riverstate%qqueue(l,pooltype,i) = riverstate%qqueue(l,pooltype,i) - fractionqueue(l)*absvol
          ENDIF
        ENDDO
        IF(fractionpart>0)THEN
          l = ttstep(pooltype,i) + 1
          riverstate%qqueue(l,pooltype,i) = riverstate%qqueue(l,pooltype,i) - fractionpart*absvol
        ENDIF
        removedflow = absvol
      ELSE
        riverstate%water(pooltype,i) = 0.
        riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i) = 0.
        IF(absvol>totvol .AND. conductwarning)THEN
          WRITE(6,*) 'Warning: Point source abstraction from river could not be fulfilled, not enough water in river.'
          WRITE(6,*) 'Warning: subbasin ',basin(i)%subid, 'abstracted volume: ',totvol, 'of wished volume: ',absvol
        ENDIF
        removedflow = totvol
      ENDIF

    ELSE  !Timeseries abstraction
      lastps = 0
      DO
        CALL find_next_pointsource(i,lastps,nabsused,absinfo,ips)
        IF(ips==0) EXIT
        lastps = ips
        IF(absload(ips)%flow==0.) CYCLE
        IF(absinfo(ips)%sw_code/=1) CYCLE  !main river abstraction

        !>Calculate volume fractions of river water compartments
        IF(.NOT.ALLOCATED(fractionqueue)) ALLOCATE(fractionqueue(ttstep(pooltype,i)))
        CALL calculate_water_fractions(ttstep(pooltype,i),ttpart(pooltype,i),riverstate%water(pooltype,i),0.,riverstate%qqueue(1:ttstep(pooltype,i),pooltype,i),totvol,fractionw,notused,fractionqueue,fractionpart)
        !>If abstraction of water: Calculate amount
        absvol = absload(ips)%flow*seconds_per_timestep  !m3
        !>Remove abstraction water proportionally from river and queue
        IF(absvol<totvol)THEN
          IF(fractionw>0.)THEN
            riverstate%water(pooltype,i) = riverstate%water(pooltype,i) - fractionw*absvol
          ENDIF
          DO l = 1,ttstep(pooltype,i)
            IF(fractionqueue(l)>0.)THEN
              riverstate%qqueue(l,pooltype,i) = riverstate%qqueue(l,pooltype,i) - fractionqueue(l)*absvol
            ENDIF
          ENDDO
          IF(fractionpart>0)THEN
            l = ttstep(pooltype,i) + 1
            riverstate%qqueue(l,pooltype,i) = riverstate%qqueue(l,pooltype,i) - fractionpart*absvol    !Note whole volume so that remaining outflow will be correct
          ENDIF
          removedflow = removedflow + absvol
        ELSE
          riverstate%water(pooltype,i) = 0.
          riverstate%qqueue(1:ttstep(pooltype,i)+1,pooltype,i) = 0.
          IF(absvol>totvol .AND. conductwarning)THEN
            WRITE(6,*) 'Warning: Point source abstraction from river could not be fulfilled, not enough water in river.'
            WRITE(6,*) 'Warning: subbasin ',basin(i)%subid, 'abstracted volume: ',totvol, 'of wished volume: ',absvol
          ENDIF
          removedflow = removedflow + totvol
        ENDIF
      ENDDO

    ENDIF
    IF(ALLOCATED(fractionqueue)) DEALLOCATE(fractionqueue)

  END SUBROUTINE point_abstraction_from_main_river

  !>Abstraction of water from outlet lake
  !>
  !> \b Reference ModelDescription Chapter Water management (Point sources - Negative point source)
  !-------------------------------------------------------------------
  SUBROUTINE point_abstraction_from_outlet_lake(i,pooltype,qunitfactor,lakestate,removedflow)

    USE MODVAR, ONLY : load,conductwarning,  &
                       nabsused,absinfo,absload, &
                       seconds_per_timestep, &
                       find_next_pointsource

    !Argument declaration
    INTEGER, INTENT(IN) :: i                    !<index of current subbasin
    INTEGER, INTENT(IN) :: pooltype             !<lake type (local or outlet)
    REAL, INTENT(IN)    :: qunitfactor          !<transformation factor m3/s->mm/timestep
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake states
    REAL, INTENT(OUT)   :: removedflow          !<removed flow (m3/timestep)

    !Local variables
    INTEGER lastps, ips
    REAL    lakevol     !volume in lake (mm)
    REAL    absvol      !abstraction volume to be removed (mm)

    !>\b Algoritm \n
    removedflow = 0.
    IF(.NOT.ALLOCATED(absload))THEN
      !Constant abstraction
      IF(load(i)%abstrind/=2) RETURN  !not lake abstraction
      IF(load(i)%abstrvol==0) RETURN

      !>If abstraction of water: Calculate amount
      lakevol = lakestate%water(pooltype,i)       !mm
      absvol = load(i)%abstrvol * qunitfactor  !mm/ts

      !>Remove abstraction water proportionally from fast and slow lake part
      IF(absvol<lakevol)THEN
        lakestate%water(pooltype,i) = lakestate%water(pooltype,i) - absvol
        removedflow = load(i)%abstrvol * seconds_per_timestep
      ELSE
        lakestate%water(pooltype,i) = 0.
        IF(absvol>lakevol .AND. conductwarning) WRITE(6,*) 'Warning: Wanted abstraction from lake could not be fulfilled, not enough water in lake.'
        removedflow = lakevol / qunitfactor * seconds_per_timestep
      ENDIF

    ELSE  !Timeseries abstraction
      lastps = 0
      DO
        CALL find_next_pointsource(i,lastps,nabsused,absinfo,ips)
        IF(ips==0) EXIT
        lastps = ips
        IF(absload(ips)%flow==0.) CYCLE
        IF(absinfo(ips)%sw_code/=2) CYCLE  !not lake abstraction

        !>If abstraction of water: Calculate amount
        lakevol = lakestate%water(pooltype,i)       !mm
        absvol = absload(ips)%flow * qunitfactor  !mm/ts

        !>Remove abstraction water proportionally from fast and slow lake part
        IF(absvol<lakevol)THEN
          lakestate%water(pooltype,i) = lakestate%water(pooltype,i) - absvol
          removedflow = removedflow + absload(ips)%flow * seconds_per_timestep  !m3/ts
        ELSE
          lakestate%water(pooltype,i) = 0.
          IF(absvol>lakevol .AND. conductwarning) WRITE(6,*) 'Warning: Wanted abstraction from lake could not be fulfilled, not enough water in lake.'
          removedflow = removedflow + lakevol / qunitfactor * seconds_per_timestep
        ENDIF
      ENDDO
    ENDIF

  END SUBROUTINE point_abstraction_from_outlet_lake

  !>Abstraction of water from outlet lake for transfer to other subbasin
  !>
  !> \b Reference ModelDescription Chapter Water management (Water transfer)
  !-------------------------------------------------------------------
  SUBROUTINE water_transfer_from_outlet_lake(i,pooltype,qunitfactor,miscstate,lakestate,removedflow)

    USE HYPEVARIABLES, ONLY : o_dwtr
    USE MODVAR, ONLY : basin, &
                       outvarid, &
                       watertransfer, &
                       conductwarning, &
                       xobsi,xobsindex, &
                       seconds_per_timestep, &
                       numsubstances, &
                       find_next_watertransfer

    !Argument declaration
    INTEGER, INTENT(IN) :: i                    !<index of current subbasin
    INTEGER, INTENT(IN) :: pooltype             !<lake type (local or outlet)
    REAL, INTENT(IN)    :: qunitfactor          !<transformation factor m3/s->mm/timestep
    TYPE(miscstatetype),INTENT(INOUT) :: miscstate  !<miscellaneous states
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake states
    REAL, INTENT(OUT)   :: removedflow          !<removed flow (m3/timestep)

    !Local variables
    INTEGER lastwt,wtindex  !index in watertransfer
    REAL    lakevol     !volume in lake (mm)
    REAL    absvol      !abstraction volume to be removed (mm)
    REAL    absconc(numsubstances)     !abstraction volume concentration
    REAL    watertaken !removed flow can come from several transfers (m3/s)

    !>\b Algoritm \n
    removedflow = 0.
    wtindex = 0
    DO
      lastwt = wtindex
      CALL find_next_watertransfer(i,lastwt,wtindex)
      IF(wtindex==0) EXIT
      IF(watertransfer(wtindex)%rcvindex==0) CYCLE

      !>If abstraction of water: Calculate amount
      lakevol = lakestate%water(pooltype,i)       !mm
      IF(watertransfer(wtindex)%flow>=0)THEN
        absvol = watertransfer(wtindex)%flow * qunitfactor  !mm/ts
      ELSEIF(watertransfer(wtindex)%flow<0.)THEN
        IF(xobsindex(o_dwtr,i)<=0)THEN
          !TODO: check this earlier?!
          WRITE(6,*) 'ERROR: Time series of demanded water transfer (',outvarid(o_dwtr)%shortname,') is missing in Xobs for subid: ',basin(i)%subid
          STOP 1
        ENDIF
        absvol = xobsi(xobsindex(o_dwtr,i)) * qunitfactor !mm/ts
      ENDIF
      IF(absvol<=0.)RETURN
      absconc = 0.
      watertaken = 0.

      !>Remove transfer water proportionally from fast and slow lake part
      IF(absvol<lakevol)THEN
        lakestate%water(pooltype,i) = lakestate%water(pooltype,i) - absvol
        IF(numsubstances>0) absconc = lakestate%conc(:,pooltype,i)  !not used
        watertaken = absvol / qunitfactor
      ELSEIF(lakevol>0.)THEN
        IF(numsubstances>0) absconc = lakestate%conc(:,pooltype,i)
        lakestate%water(pooltype,i) = 0.
        IF(absvol>lakevol .AND. conductwarning) WRITE(6,*) 'Warning: Wanted water transfer from lake could not be fulfilled, not enough water in lake.'
        watertaken = lakevol / qunitfactor
      ELSE    !lakevol=0 & absvol>0
        IF(conductwarning) WRITE(6,*) 'Warning: Wanted water transfer from lake could not be fulfilled, no water in lake.'
      ENDIF

      !>Save abstracted water for next timestep
      IF(watertransfer(wtindex)%rcvindex>0)THEN
        miscstate%nexttransfer(watertransfer(wtindex)%rcvindex) = miscstate%nexttransfer(watertransfer(wtindex)%rcvindex) + watertaken
        IF(numsubstances>0) miscstate%cnexttransfer(:,watertransfer(wtindex)%rcvindex) = miscstate%cnexttransfer(:,watertransfer(wtindex)%rcvindex) + absconc*watertaken
        !TODO: Water balance output will not have balance in this case. The water transfer outside model setup is not included.
      ENDIF
      removedflow = removedflow + watertaken * seconds_per_timestep  !m3/ts
    ENDDO

  END SUBROUTINE water_transfer_from_outlet_lake

  !>Add water transfer by management data to main river inflow
  !>
  !> \b Reference ModelDescription Chapter Water management (Water transfer)
  !-----------------------------------------------------------------
  SUBROUTINE add_water_transfer_to_main_river(i,qin,cin,watertransfer,ctransfer,addedflow)

    USE MODVAR, ONLY : numsubstances, &
                       seconds_per_timestep

    !Argument declarations
    INTEGER, INTENT(IN) :: i                        !<index of subbasin
    REAL, INTENT(INOUT) :: qin                      !<flow into main river (m3/s)
    REAL, INTENT(INOUT) :: cin(numsubstances)       !<concentration of flow into main river (mg/L)
    REAL, ALLOCATABLE, INTENT(IN) :: watertransfer(:)     !<water transfer flow into main river (m3/s)
    REAL, ALLOCATABLE, INTENT(IN) :: ctransfer(:,:)       !<concentration of water transfer (mg/L)
    REAL, INTENT(OUT)   :: addedflow                !<added flow (m3/timestep)

    !Local variables
    REAL qadd
    REAL cadd(numsubstances)

    !Check for water transfer to subbasin
    qadd = watertransfer(i)
    IF(qadd==0)RETURN
    cadd = 0.
    IF(numsubstances>0) cadd = ctransfer(:,i)

    !Add source to river
    IF(qin>0)THEN
      cin = (qin * cin + qadd * cadd)/(qin + qadd)
      qin = qin + qadd
    ELSE
      qin = qadd
      cin = cadd
    ENDIF
    addedflow = qadd * seconds_per_timestep

  END SUBROUTINE add_water_transfer_to_main_river

  !>\brief Subroutine for finding current lake outflow parameters.
  !------------------------------------------------------------------------------
  SUBROUTINE get_current_lake_outflow_parameters(i,itype,ioutlet,lakeareain,olakewst,   &
                                         have2outlets,ratck,ratcexp, &
                                         w0Today,wmin,damProd,maxProd,minProd,out2ratck,out2ratcexp,  &
                                         out2w0Today,out2wmin,out2damProd,out2maxProd,out2minProd,qin)

    USE HYPEVARIABLES, ONLY : ratingk,      &
                              m_grat2,      &
                              m_ilrrat2, &
                              m_krelflood,  &
                              m_kthrflood,  &
                              m_klowflood
    USE MODVAR, ONLY : missing_value, &
                       lake, &
                       elake, &
                       dam, &
                       basin, &
                       lakebasin, &
                       genpar, &
                       regpar, &
                       regiondivision, &
                       lakeindex, &
                       lakeout2index, &
                       damindex, &
                       lakebasinindex

    !Argument declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    INTEGER, INTENT(IN) :: itype         !<lake type (local or main)
    INTEGER, INTENT(IN) :: ioutlet       !<index of outlet for elake
    REAL, INTENT(IN)    :: lakeareain    !<lakearea (m2) (from GeoData)
    REAL, INTENT(IN)    :: olakewst      !<outlet lake water stage (m)
    LOGICAL, INTENT(OUT):: have2outlets   !<Lake with two outlets?
    REAL, INTENT(OUT)   :: ratck        !<current rating curve parameter rate
    REAL, INTENT(OUT)   :: ratcexp      !<current rating curve parameter exponent
    REAL, INTENT(OUT)   :: w0Today      !<current water level threshold in w-reference system (m)
    REAL, INTENT(OUT)   :: wmin         !<minimum water level threshold (sänkningsgräns) (m)
    REAL, INTENT(OUT)   :: damProd      !<current dam production flow (m3/s)
    REAL, INTENT(OUT)   :: maxProd      !<(current) maximum production flow (m3/s)
    REAL, INTENT(OUT)   :: minProd      !<(current) minimum flow (m3/s)
    REAL, INTENT(OUT)   :: out2ratck    !<current rating curve parameter rate of outlet 2
    REAL, INTENT(OUT)   :: out2ratcexp  !<current rating curve parameter exponent of outlet 2
    REAL, INTENT(OUT)   :: out2w0Today  !<current water level threshold in w-reference system (m) of outlet 2
    REAL, INTENT(OUT)   :: out2wmin     !<minimum water level threshold (sänkningsgräns) (m) of outlet 2
    REAL, INTENT(OUT)   :: out2damProd  !<current dam production flow (m3/s) of outlet 2
    REAL, INTENT(OUT)   :: out2maxProd  !<(current) maximum production flow (m3/s) of outlet 2
    REAL, INTENT(OUT)   :: out2minProd  !<(current) minimum flow (m3/s) of outlet 2
    REAL,OPTIONAL,INTENT(IN) :: qin     !<current net inflow to lake (m3/s)

    !Local variables
    REAL wlmr                   !water level lake (m)
    REAL qamp,qpha              !parameters for regulation of lake
    REAL qprodToday   !Production flow (m3/s)
    REAL rating2        !general rating curve parameters outlet lake
    REAL out2w0rel  !-"-
    INTEGER dampurpose          ! Purpose of dam, 1=irrigation, 2=water supply, 3=flood control, 4=hydropower
    INTEGER current_lake        ! Index of current lake in lake
    INTEGER current_elake       ! Index of current lake in elake
    REAL qinfmax                ! Max mean monthly inflow
    REAL snowfrac               ! Fraction of precipitaiton falling as snow in catchment upstream of dam
    REAL regvol
    REAL qinftoday              ! Current inflow for 'today'
    REAL qthresh                ! Threshold inflow over which a flood control dam save water
    REAL lthresh                ! Threshold reservoir level over which flood control dam releases extra flow
    REAL qinfmin                ! Min mean monthly inflow
    LOGICAL minflow,out2minflow ! Flag for minimum flow

    !Initial values
    wlmr=0.
    qprodToday = 0.
    qamp = 0.
    qpha = 0.
    regvol=0.
    qinfmax = 0.
    qinfmin=0.
    snowfrac = 0.
    dampurpose = 0
    qinftoday=0.
    lthresh = 0.
    qthresh = 0.
    current_lake = 0
    current_elake = 0
    minflow = .FALSE.
    out2w0rel = 0.
    out2minflow = .FALSE.

    !Default output
    damProd = 0.
    maxProd = 0.
    minProd = 0.
    ratck = 0.
    ratcexp = 0.
    wmin = missing_value
    out2damProd = 0.
    out2maxProd = 0.
    out2minProd = 0.
    out2ratck = 0.
    out2ratcexp = 0.
    out2wmin = missing_value
    have2outlets = .FALSE.

    !Local lake parameter values
    IF(itype==1)THEN
      IF(basin(i)%parregion(regiondivision(m_ilrrat2))>0) rating2=regpar(m_ilrrat2,basin(i)%parregion(regiondivision(m_ilrrat2)))  !TODO: check subroutine for only olake, probably not used
      IF(rating2<=0.) rating2 = genpar(m_grat2)
      w0Today = basin(i)%lakedepth(1)      !ilake depth = threshold (m)
      ratck   = ratingk(itype,i)
      ratcexp = rating2
      RETURN
    ENDIF

    !Outlet lake or dam

    IF(PRESENT(qin))THEN
      qinftoday=qin
    ENDIF

    !Current lake parameter values
      IF(ALLOCATED(lakeindex))THEN
        IF(lakeindex(i)>0)THEN
          current_lake = lakeindex(i)
          maxProd = lake(lakeindex(i))%mqprod         !Maximum production flow
          minflow = lake(lakeindex(i))%minflow        !Minimum flow
          wmin = lake(lakeindex(i))%wmin              !lake threshold/"sänkningsgräns"
        ENDIF
      ENDIF
      IF(ALLOCATED(lakeout2index))THEN  !Second outlet of lake/dam
        IF(lakeout2index(i)>0)THEN
          have2outlets = .TRUE.
          out2maxProd = lake(lakeout2index(i))%mqprod         !Maximum production flow
          out2minflow = lake(lakeout2index(i))%minflow        !Minimum flow
          out2wmin = lake(lakeout2index(i))%wmin              !lake threshold/"sänkningsgräns"
          out2w0rel = lake(lakeout2index(i))%w0ref            !w0 of outlet 2 relative to outlet 1 (w0ref)
        ENDIF
      ENDIF
      IF(ALLOCATED(lakebasinindex))THEN
        IF(lakebasinindex(i)>0)THEN
          current_elake = lakebasin(lakebasinindex(i))%ilk
          wmin = elake(current_elake)%wmin
        ENDIF
      ENDIF
      IF(ALLOCATED(damindex))THEN
        IF(damindex(i)>0)THEN
          regvol = dam(damindex(i))%regvol          !Regvol
          snowfrac = dam(damindex(i))%snowfrac      ! Fraction of prec falling as snow upstream of dam
          qamp = dam(damindex(i))%qamp              !Amplitude of sin-adjustment of qprod
          qpha = dam(damindex(i))%qpha              !Phase of sin-adjustment of qprod
          wmin = dam(damindex(i))%wmin              !lake threshold/"sänkningsgräns"
          qinfmin = dam(damindex(i))%qinfmin
          qinfmax = dam(damindex(i))%qinfmax
          dampurpose = dam(damindex(i))%purpose              !dam purpose
          lthresh = 0.-genpar(m_klowflood)*regvol*1000000./lakeareain       !threshold level for extra flood control releases (typical 1/3 of regvol)
          qthresh = genpar(m_kthrflood)*qinfmax
        ENDIF
      ENDIF

      !Water level for outlet lake
      wlmr = olakewst

      !Dam in DamData
      IF(damindex(i)>0)THEN    !The below code is repeated first for dams, then for lakes

        !Current production flow for dam with regulation volume (calibrated dams)
        IF(wmin.NE.missing_value)THEN       ! i.e. RegVol>0
          CALL get_current_production_flow(0,damindex(i),0,wlmr,qprodToday)

          !Calculate dam outflow depending on dam purpose and current production flow
          IF(dampurpose==4)THEN    !hydroelectric dam
            damProd=qprodToday              ! If not a snow/seasonal redist dam and qamp not given, damProd=constant (Qinf or Qprod1)
            IF(qamp>0)THEN
              damProd = apply_seasonal_factor_on_production_flow(0,damindex(i),0,qprodToday)
            ELSEIF(snowfrac > 0.35)THEN     !CD2014 This determines if dams seasonally change flow by comparing regulation capacity to inflows (i.e. for dams wtih more than 35 % of precip that is snow
              IF(qpha>0) THEN
                qamp = 0.71                 !CD2014 based on regression from data, used if Qamp not given.
                damProd = apply_seasonal_factor_on_production_flow(0,damindex(i),0,qprodToday,qamp)
              ENDIF
            ENDIF
          ELSEIF(dampurpose==1)THEN   !irrigation dam
            damProd=qprodToday
          ELSEIF(dampurpose==2)THEN   !water supply dam
            damProd=qprodToday
          ELSEIF(dampurpose==3)THEN   !flood control dam (aim is to maintain dam as empty as possible)
            IF(qinftoday <= 0.) qinftoday = 0. ! Negative net inflow, no rule set to zero CP200520
            IF(qinftoday < qthresh)THEN        ! IF inflow today < threshold inflow
              IF(wlmr<lthresh)THEN             ! IF water level today < threshold level
                damProd=Qinftoday              ! Release the inflow
              ELSE
                IF(qinftoday < qinfmin)THEN
                  damProd=MIN(Qinftoday*genpar(m_krelflood),qthresh)     ! If water level above threshold, release more than inflows (i.e. try empty the dam)
                ENDIF
              ENDIF
            ELSE                                 ! If inflow today >= threshold inflow
              damProd=qthresh                    ! Release maximum allowable flow
            ENDIF
          ELSE
            damProd=qprodToday
          ENDIF
          damProd=MIN(damProd,regvol*1000000./86400.)          ! Test: Limit damProd to the Regvol for one day
        ENDIF   !Close wmin.ne.missing

        !Ordinary outflow threshold
        w0Today = 0.

        !Set rating curve parameters
        IF(wlmr>w0Today)THEN
          CALL get_current_rating_parameters(i,0,damindex(i),0,ratck,ratcexp)
        ENDIF

      !Lake/Dam in LakeData or GeoData
      ELSEIF(current_lake>0)THEN

        !Current production flow for dam with regulation volume
        IF(wmin.NE.missing_value)THEN
          CALL get_current_production_flow(current_lake,0,0,wlmr,qprodToday)
          damProd = apply_seasonal_factor_on_production_flow(current_lake,0,0,qprodToday)
          IF(minflow) minProd = damProd     !Set minimum flow to current production flow
        ENDIF

        !Outflow threshold
        w0Today = 0.                  !Threshold w0 applies in general
        CALL adjust_threshold_for_seasonal_variation(current_lake,0,w0Today)

        IF(wlmr>w0Today)THEN
          CALL get_current_rating_parameters(i,current_lake,0,0,ratck,ratcexp)
        ENDIF

      !Lake in LakeData composed of lakebasins equal water level
      ELSEIF(current_elake>0)THEN

        !Current production flow for dam with regulation volume
        IF(wmin.NE.missing_value)THEN
          CALL get_current_production_flow(current_elake,0,ioutlet,wlmr,qprodToday)
          damProd = apply_seasonal_factor_on_production_flow(current_elake,0,ioutlet,qprodToday)
        ENDIF

        !Outflow threshold
        w0Today = 0.                  !Threshold w0 applies in general
        CALL adjust_threshold_for_seasonal_variation(current_elake,ioutlet,w0Today)

        IF(wlmr>w0Today)THEN
          CALL get_current_rating_parameters(i,current_elake,0,ioutlet,ratck,ratcexp)
        ENDIF

      !Lake in GeoData
      ELSE

        w0Today = 0.                  !Threshold w0 applies in general
        IF(wlmr>w0Today)THEN
          CALL get_current_rating_parameters(i,0,0,0,ratck,ratcexp)
        ENDIF

      ENDIF   ! ENDIF for lakes (i.e. damindex =/0)

      IF(have2outlets)THEN    !Calculate the second outlet
        !Current production flow for dam with regulation volume
        IF(out2wmin.NE.missing_value)THEN
          CALL get_current_production_flow(lakeout2index(i),0,0,wlmr,qprodToday)
          out2damProd = apply_seasonal_factor_on_production_flow(lakeout2index(i),0,0,qprodToday)
          IF(out2minflow) out2minProd = out2damProd     !Set minimum flow to current production flow
        ENDIF

        !Outflow threshold
        out2w0Today = 0. + out2w0rel        !Ordinary outflow threshold
        CALL adjust_threshold_for_seasonal_variation(lakeout2index(i),0,out2w0Today)

        IF(wlmr>out2w0Today)THEN
          CALL get_current_rating_parameters(i,lakeout2index(i),0,0,out2ratck,out2ratcexp)
        ENDIF

      ENDIF

  END SUBROUTINE get_current_lake_outflow_parameters

  !>\brief Subroutine for finding current production flow
  !------------------------------------------------------------------------------
  SUBROUTINE get_current_production_flow(current_lake,current_dam,current_outlet,wlmr,prodflow)

    USE HYPEVARIABLES, ONLY : m_limprod

    USE MODVAR, ONLY : missing_value, &
                       dayno, &
                       elake, &
                       lake, &
                       dam, &
                       genpar

    !Argument declarations
    INTEGER, INTENT(IN) :: current_lake !<index of lake for current lake
    INTEGER, INTENT(IN) :: current_dam  !<index in dam for current dam
    INTEGER, INTENT(IN) :: current_outlet  !<index of outlet of current elake
    REAL, INTENT(IN)    :: wlmr         !<outlet lake water stage (m)
    REAL, INTENT(OUT)   :: prodflow     !<current production flow (m3/s)

    !Local variables
    REAL qprod0            !Production flow which is equal to mean inflow
    REAL qprod1, qprod2    !Production flow (m3/s) for production periods 1 and 2
    REAL fracLevel         !Actual reservoir situation, in fraction of (w0 - wmin)
    REAL wmin              !minimum water level threshold (sänkningsgräns) (m)
    REAL fillDamThreshold  !Percentage of reservoir capacity bellow which economy regime starts
    INTEGER dayno1, dayno2 !Starting day nr. for production periods 1 and 2

    !Default output
    prodflow = 0.
    IF(current_lake==0 .AND. current_dam==0) RETURN

    !Current lake parameter values
    IF(current_lake>0)THEN
      IF(current_outlet>0)THEN
        qprod1 = elake(current_lake)%outlet(current_outlet)%qprod(1)
        qprod2 = elake(current_lake)%outlet(current_outlet)%qprod(2)
        dayno1 = elake(current_lake)%outlet(current_outlet)%qdate(1)
        dayno2 = elake(current_lake)%outlet(current_outlet)%qdate(2)
        wmin = elake(current_lake)%wmin
        fillDamThreshold = elake(current_lake)%outlet(current_outlet)%limprod
      ELSE
        qprod1 = lake(current_lake)%qprod1
        qprod2 = lake(current_lake)%qprod2
        dayno1 = lake(current_lake)%datum1
        dayno2 = lake(current_lake)%datum2
        wmin = lake(current_lake)%wmin
        fillDamThreshold = lake(current_lake)%limprod
      ENDIF
    ENDIF
    IF(current_dam>0)THEN   !Dam has priority (LakeData can hold NP-parameters)
      qprod0 = dam(current_dam)%qinfmed
      qprod1 = dam(current_dam)%qprod1
      qprod2 = dam(current_dam)%qprod2
      dayno1 = dam(current_dam)%datum1
      dayno2 = dam(current_dam)%datum2
      wmin = dam(current_dam)%wmin
      fillDamThreshold = dam(current_dam)%limprod
      IF(qprod1<=0.)THEN
        qprod1=qprod0
        qprod2=qprod0
      ENDIF
    ENDIF
    IF(fillDamThreshold<0.) fillDamThreshold=genpar(m_limprod) !in case missing value, use general value

    !Calculate current production flow
    prodflow = qprod1                                              !Production rate 1 applies in general
    IF(dayno1*dayno2 > 0) THEN                                       !If both dates for different production regimes are non-zero, ...
      IF(dayno < dayno1 .OR. dayno >= dayno2)  prodflow = qprod2   !... and today is not within the periode of production rate 1, then production rate 2 applies
    ENDIF
    IF(fillDamThreshold>0)THEN
      fracLevel = (wlmr - wmin)/(0. - wmin)
      IF(fracLevel > 0. .AND. fracLevel < fillDamThreshold) THEN
        prodflow = fracLevel/fillDamThreshold * prodflow        !Economy regime, if reservoir fractional filling is lower than threshold
      ENDIF
    ENDIF

  END SUBROUTINE get_current_production_flow

  !>\brief Function for applying seasonal variation on production flow
  !------------------------------------------------------------------------------
  REAL FUNCTION apply_seasonal_factor_on_production_flow(current_lake,current_dam,current_outlet,prodflow,qampin)

    USE MODVAR, ONLY : dayno,pi, &
                       elake,lake,dam, &
                       tsofday,timesteps_per_day

    !Argument declarations
    INTEGER, INTENT(IN) :: current_lake !<index of lake for current lake
    INTEGER, INTENT(IN) :: current_dam  !<index in dam for current dam
    INTEGER, INTENT(IN) :: current_outlet  !<index of outlet in current elake
    REAL, INTENT(IN)    :: prodflow     !<current production flow (m3/s)
    REAL, INTENT(IN), OPTIONAL :: qampin !<amplitude of seasonal variation to be used instead of parameter value

    !Local variables
    REAL qamp    !Amplitude of sin-adjustment of qprod
    REAL qpha    !Phase of sin-adjustment of qprod
    REAL seasonalfactor

    !Initial values
    qamp = 0.
    qpha = 0.
    seasonalfactor = 1.
    apply_seasonal_factor_on_production_flow = 0.

    !Current lake parameter values
    IF(current_lake>0)THEN
      IF(current_outlet==0)THEN
        qamp = lake(current_lake)%qamp
        qpha = lake(current_lake)%qpha
      ELSE
        qamp = elake(current_lake)%outlet(current_outlet)%qamp
        qpha = elake(current_lake)%outlet(current_outlet)%qpha
      ENDIF
    ENDIF
    IF(current_dam>0)THEN   !Dam has priority (LakeData then hold NP-parameters)
      qamp = dam(current_dam)%qamp
      qpha = dam(current_dam)%qpha
    ENDIF
    IF(PRESENT(qampin)) qamp = qampin

    !Calculate seasonal factor
    IF(qamp>0.) seasonalfactor = 1. + qamp * SIN(2.*pi*(dayno-1+REAL(tsofday)/REAL(timesteps_per_day)+qpha)/365.)
    apply_seasonal_factor_on_production_flow = seasonalfactor * prodflow

  END FUNCTION apply_seasonal_factor_on_production_flow

  !>\brief Subroutine for calculation current threshold
  !------------------------------------------------------------------------------
  SUBROUTINE adjust_threshold_for_seasonal_variation(current_lake,current_outlet,w0)

    USE MODVAR, ONLY : dayno, &
                       elake, &
                       lake

    !Argument declarations
    INTEGER, INTENT(IN) :: current_lake   !<index of lake for current lake
    INTEGER, INTENT(IN) :: current_outlet !<index of outlet of current lake
    REAL, INTENT(INOUT) :: w0             !<current threshold (m)

    !Local variables
    INTEGER dayno1, dayno2      !Starting day nr. for production periods 1 and 2
    REAL deltaw0                !difference in water level threshold for period 2 (m)

    !Initial values
    deltaw0 = 0.

    !Current lake parameter values
    IF(current_lake>0)THEN
      IF(current_outlet>0)THEN
        dayno1 = elake(current_lake)%outlet(current_outlet)%qdate(1)
        dayno2 = elake(current_lake)%outlet(current_outlet)%qdate(2)
        deltaw0 = elake(current_lake)%outlet(current_outlet)%deltaw0
      ELSE
        dayno1 = lake(current_lake)%datum1
        dayno2 = lake(current_lake)%datum2
        deltaw0 = lake(current_lake)%deltaw0
      ENDIF
    ENDIF

    !Calculate current threshold
    IF(deltaw0/=0.)THEN
      IF(dayno1*dayno2 > 0) THEN
        IF(dayno<dayno1 .OR. dayno>=dayno2)  w0 = w0 + deltaw0   !threshold for period 2 applies
      ENDIF
    ENDIF

  END SUBROUTINE adjust_threshold_for_seasonal_variation

  !>\brief Subroutine for finding current rating curve parameters for outlet lake
  !------------------------------------------------------------------------------
  SUBROUTINE get_current_rating_parameters(i,current_lake,current_dam,current_outlet,ratck,ratcexp)

    USE HYPEVARIABLES, ONLY : ratingk,      &
                              m_grat2,      &
                              m_olrrat2
    USE MODVAR, ONLY : missing_value, &
                       dam, &
                       elake, &
                       lake, &
                       basin, &
                       regiondivision, &
                       genpar, &
                       regpar

    !Argument declarations
    INTEGER, INTENT(IN) :: i            !<index of current subbasin
    INTEGER, INTENT(IN) :: current_lake !<index of lake for current lake
    INTEGER, INTENT(IN) :: current_dam  !<index in dam for current dam
    INTEGER, INTENT(IN) :: current_outlet !<index of outlet of current lake
    REAL, INTENT(OUT)   :: ratck        !<current rating curve parameter rate
    REAL, INTENT(OUT)   :: ratcexp      !<current rating curve parameter exponent

    !Local variables
    REAL rating2        !general rating curve parameters outlet lake
    REAL regrate,regexp  !current parameters for specific rating curve
    REAL wmin

    INTEGER, PARAMETER :: itype = 2   !olake

    !Default output
    ratck = 0.
    ratcexp = 0.

    !Initial values current parameters
    regrate = 0.
    regexp = 0.
    wmin = missing_value

    !Current lake or dam parameter values
    IF(current_lake>0)THEN
      IF(current_outlet>0)THEN
        regrate = elake(current_lake)%outlet(current_outlet)%rate
        regexp = elake(current_lake)%outlet(current_outlet)%exp
        wmin = elake(current_lake)%wmin
      ELSE
        regrate = lake(current_lake)%rate
        regexp = lake(current_lake)%exp
        wmin = lake(current_lake)%wmin
      ENDIF
    ENDIF
    IF(current_dam>0)THEN   !Dam has priority (LakeData can hold NP-parameters)
      regrate = dam(current_dam)%rate
      regexp = dam(current_dam)%exp
      wmin = dam(current_dam)%wmin
    ENDIF

    !Set output rating curve parameters
    IF(regrate>0.)THEN                 !Specific rating curve for lake or dam spill
      ratck = regrate
      ratcexp = regexp
    ELSEIF(wmin.NE.missing_value)THEN  !Dam without rating curve for spill
    ELSE                               !General rating curve for lake
      !General rating curve
      ratck = ratingk(itype,i)
      rating2 = 0.
      IF(basin(i)%parregion(regiondivision(m_olrrat2))>0) rating2=regpar(m_olrrat2,basin(i)%parregion(regiondivision(m_olrrat2)))
      IF(rating2<=0.) rating2 = genpar(m_grat2)
      ratcexp = rating2
    ENDIF

  END SUBROUTINE get_current_rating_parameters

  !>\brief Subroutine for calculation and removal of outflow from local lake.
  !!General rating curve is used for ilakes.
  !----------------------------------------------------------------------------
  SUBROUTINE calculate_ilake_outflow(i,subid,ns,qin,lakearea,qunitfactor, &
                                     outflowm3s,coutflow,load,volumeflow,wst,lakestate)

    USE HYPEVARIABLES, ONLY : ratingk, &
                              m_grat2, &
                              m_ilrrat2
    USE HYPE_WATERBALANCE, ONLY : w_iltomr
    USE MODVAR, ONLY : basin, &
                       conductload, &
                       conductwb, &
                       genpar, &
                       regpar, &
                       regiondivision, &
                       seconds_per_timestep

    !Argument declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    INTEGER, INTENT(IN) :: subid         !<subid of current subbasin
    INTEGER, INTENT(IN) :: ns            !<number of substances
    REAL, INTENT(IN)    :: qin           !<inflow of lake (m3/s)
    REAL, INTENT(IN)    :: lakearea      !<lakearea (m2)
    REAL, INTENT(IN)    :: qunitfactor   !<factor for transforming flow for lake from m3/s to mm/timestep and back
    REAL, INTENT(OUT)   :: outflowm3s    !<outflow of lake (m3/s)
    REAL, INTENT(OUT)   :: coutflow(ns)  !<concentration of outflow of lake
    REAL,ALLOCATABLE,INTENT(INOUT) :: load(:,:)  !<load of outflow of lake (and other flows)
    REAL,ALLOCATABLE,INTENT(INOUT) :: volumeflow(:,:)    !<volume outflow of lake (m3/ts) (and other flows)
    REAL, INTENT(OUT)   :: wst           !<lake water (mm)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state

    !Local parameters
    INTEGER, PARAMETER :: itype = 1        !lake type (local)
    REAL, PARAMETER :: lakedepth_not_used = 1.
    REAL, PARAMETER :: hypodepth_not_used = 1.

    !Local variables
    REAL lakewstmm              !lake water stage (mm)
    REAL wlmr,wlmr0             !water level lake  (m)
    REAL w0Today                !water level threshold  (m)
    REAL ratingc,ratinge        !current rating curve parameters
    REAL outflowmm              !outflow of lake (mm)

    !>\b Algorithm \n
    !Initial values
    outflowm3s = 0.
    outflowmm = 0.
    lakewstmm=lakestate%water(itype,i)
    wlmr = lakewstmm*0.001              !Water in lake [m]

    !Current parameter values
    ratingc = ratingk(itype,i)
    ratinge = 0.
    IF(basin(i)%parregion(regiondivision(m_ilrrat2))>0) ratinge=regpar(m_ilrrat2,basin(i)%parregion(regiondivision(m_ilrrat2)))  !TODO: check subroutine for only olake, probably not used
    IF(ratinge<=0.) ratinge = genpar(m_grat2)
    w0Today = basin(i)%lakedepth(itype)

    !>Calculate outflow from general rating curve
    wlmr0 = wlmr - w0Today
    IF(wlmr0>0.)THEN
      outflowm3s = average_flow_rating_curve(qin,lakearea,wlmr0,ratingc,ratinge)  ![m3/s]
      outflowmm = outflowm3s * qunitfactor        ![mm/timestep]

      !>Check outflow against lake volume
      IF(outflowmm*0.001>wlmr0)THEN   !Check for enough water in lake (bad rating curve or numerical problems)
        IF(wlmr0>0.)THEN
          outflowmm = wlmr0*1000.
        ELSE
          outflowmm = 0.
        ENDIF
        IF(outflowmm>lakewstmm) outflowmm = lakewstmm   !Safety for rounded wlmr and ldepth = 0
        outflowm3s = outflowmm/qunitfactor
      ENDIF

    ENDIF

    !>Remove outflow from lake
    CALL remove_outflow_from_lake(i,itype,ns,outflowmm,subid,lakedepth_not_used,hypodepth_not_used,lakewstmm,coutflow,lakestate)

    !>Set output variables
    IF(conductload) load(:,11) = outflowm3s * coutflow * seconds_per_timestep * 1.E-3 !Load at point K, outflow ilake (kg/timestep)
    IF(conductwb) volumeflow(w_iltomr,i) = outflowm3s * seconds_per_timestep  !m3/ts
    wst = lakestate%water(itype,i)  !mm

  END SUBROUTINE calculate_ilake_outflow

  !>\brief Subroutine for calculation and removal of outflow from local lake with lake sections (fill-and-spill connectivity model).
  !>
  !>Fraction of volume in lake sections is updated.
  !!Rating curve for ilakes is used for lake section interflow and outflow.
  !--------------------------------------------------------------------------------------------------------------------------------
  SUBROUTINE calculate_ilakesection_outflow(i,subid,ns,qin,pein,lakearea,qunitfactor, &
                                     outflowm3s,coutflow,load,volumeflow,wst,fnca,fcon, &
                                     lakestate)

    USE HYPEVARIABLES, ONLY : ratingk, &
                              m_grat2, &
                              m_ilrrat2
    USE HYPE_WATERBALANCE, ONLY : w_iltomr
    USE MODVAR, ONLY : basin, &
                       conductload, &
                       conductwb, &
                       genpar, &
                       regpar, &
                       regiondivision, &
                       seconds_per_timestep, &
                       lakesectiondata, &
                       maxlakesections

    !Argument declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    INTEGER, INTENT(IN) :: subid         !<subid of current subbasin
    INTEGER, INTENT(IN) :: ns            !<number of substances
    REAL, INTENT(IN)    :: qin           !<lateral inflow of lake (m3/s)  - river discharge and point sources - distributed by icatch
    REAL, INTENT(IN)    :: pein          !<vertical inflow of lake (m3/s) - precipitation and evaporation - distributed by lake area
    REAL, INTENT(IN)    :: lakearea      !<lakearea (m2)
    REAL, INTENT(IN)    :: qunitfactor   !<factor for transforming flow for lake from m3/s to mm/timestep and back
    REAL, INTENT(OUT)   :: outflowm3s    !<outflow of lake (m3/s)
    REAL, INTENT(OUT)   :: coutflow(ns)  !<concentration of outflow of lake
    REAL,ALLOCATABLE,INTENT(INOUT) :: load(:,:)  !<load of outflow of lake (and other flows)
    REAL,ALLOCATABLE,INTENT(INOUT) :: volumeflow(:,:)    !<volume outflow of lake (m3/ts) (and other flows)
    REAL, INTENT(OUT)   :: wst           !<lake water (mm)
    REAL, INTENT(OUT)   :: fnca          !<fraction of non-contributing area (per subbasin area)
    REAL, INTENT(OUT)   :: fcon          !<fraction of ilake connectivity (per ilake area)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state

    !Local parameters
    INTEGER, PARAMETER :: itype = 1        !lake type (local)
    REAL, PARAMETER :: lakedepth_not_used = 1.
    REAL, PARAMETER :: hypodepth_not_used = 1.

    !Local variables
    REAL lakewstmm              !lake water stage (mm)
    REAL wlmr,wlmr0             !water level lake  (m)
    REAL w0Today                !water level threshold  (m)
    REAL ratingc,ratinge        !current rating curve parameters
    REAL outflowmm              !outflow of lake (mm)

    INTEGER j,jj                !loop, lakesection index, j=downstream to upstream, jj=upstream to downstream

    REAL wlmmsec(maxlakesections)   !water level per lakesection (mm)
    REAL qinmmsec(maxlakesections)  !inflow (runoff) per lakesection (mm)
    REAL peinmmsec(maxlakesections) !inflow (prec-evap) per lakesection (mm)
    REAL qintsec(maxlakesections)   !outflow per lakesection (m3/s)
    REAL qinmm,peinmm,negvol,qint,qintmm,qunitfactorsec,posarea,qinloc,negww,negww2
    INTEGER nneg,nitt
    LOGICAL isconnected

    !>\b Algorithm \n
    !Initial values (per the entire lake)
    outflowm3s = 0.
    outflowmm = 0.
    lakewstmm=lakestate%water(itype,i)
    wlmr = lakewstmm*0.001              !Water in lake [m]

    !Initial values (per lake sections)
    wlmmsec = 0.
    qinmmsec = 0.
    peinmmsec = 0.
    negvol = 0.
    nneg = 0
    nitt = 0
    qint = 0.
    qintmm = 0.
    qintsec = 0.

    !>Current parameter values
    ratingc = ratingk(itype,i)
    ratinge = 0.
    IF(basin(i)%parregion(regiondivision(m_ilrrat2))>0) ratinge=regpar(m_ilrrat2,basin(i)%parregion(regiondivision(m_ilrrat2)))  !TODO: check subroutine for only olake, probably not used
    IF(ratinge<=0.) ratinge = genpar(m_grat2)
    w0Today = basin(i)%lakedepth(itype)

    !Inflow (runoff) and net prec-evap in mm
    peinmm = pein * qunitfactor
    qinmm  = qin  * qunitfactor

    !>_1_ Distribute inflows and derive water stage in lake sections after
    !>    inflows, accumulate negative volumes for later handling.
    nneg = 0 !to count number of lakesections with negative water stage
    negvol = 0. !->sum of negative lakesection volumes (m3)
    DO jj=basin(i)%lakesection,1,-1 !jj decrase from n:o lakesection to 1 (upstream to downstream)
      !distribute inflows (qin and pein) on lake sections
      qunitfactorsec = qunitfactor/(lakesectiondata(i,jj)%farea)
      qinmmsec(jj)   = qin * lakesectiondata(i,jj)%ficatch * qunitfactorsec
      peinmmsec(jj)  = peinmm
      !water in lakesection before inflows
      wlmmsec(jj)    = (lakewstmm - peinmm - qinmm) * lakestate%volfrac(jj,i) / (lakesectiondata(i,jj)%farea)
      !water in lakesections after inflows (negative values will be re-distributed in next step)
      wlmmsec(jj) = wlmmsec(jj) + peinmmsec(jj) + qinmmsec(jj)
      IF(wlmmsec(jj).LT.0.)THEN
        nneg = nneg+1
        negww = wlmmsec(jj)
        wlmmsec(jj) = 0.
        peinmmsec(jj) = peinmmsec(jj) - negww
        negvol = negvol + negww * lakesectiondata(i,jj)%farea * lakearea * 0.001 !move neg volume to temporary container[m3]
      ENDIF
    ENDDO
    !>_2_ Distribute negative water levels to lakesections with water
    nitt=0
    DO WHILE(nneg.GT.0 .AND. nitt.LT.1000)
      nitt=nitt+1
      !->sum of lakesection areas onto which the negative water can be distributed (m2)
      posarea=0.
      DO jj=basin(i)%lakesection,1,-1
        !lakesection area with positive water stage onto which we can distribute the missing water
        IF(wlmmsec(jj).GT.0.)THEN
          posarea = posarea + lakesectiondata(i,jj)%farea * lakearea !m2
        ENDIF
      ENDDO
      IF(posarea.GT.0)THEN
      !Transform negative volume from m3 to mm when distributed to the lakesections with water
        negww2 = negvol * 1000. / posarea
        negvol = 0. !ready to accumulate new negative volume
        !Distribute negative volume to lakesections with water
        nneg = 0
        DO jj=basin(i)%lakesection,1,-1
          IF(wlmmsec(jj).GT.0.)THEN
            IF(wlmmsec(jj) + negww .GE.0.)THEN
              wlmmsec(jj) = wlmmsec(jj) + negww2
              peinmmsec(jj) = peinmmsec(jj) + negww2
            ELSE
              negww = wlmmsec(jj) + negww2
              peinmmsec(jj) = peinmmsec(jj) - wlmmsec(jj)
              wlmmsec(jj) = 0.
              nneg = nneg + 1
              negvol = negvol + negww * lakesectiondata(i,jj)%farea * lakearea * 0.001 !move neg volume to temporary container[m3]
            ENDIF
          ENDIF
        ENDDO
      ELSE  !posarea<=0
        !something is wrong - no lakesection with water
        nneg=0
        WRITE(6,*)'ERROR in lakesection distribution of negative area - no lakesection with positive water level'
      ENDIF
      IF(nneg.GT.0 .AND. nitt.EQ.1000)THEN
        nneg=0
        WRITE(6,*)'ERROR in lakesection distribution of negative area - number of iteration>=1000'
      ENDIF
    ENDDO
    !>_3_ Loop over lakesections and calculate lakesection interflow
    qint = 0.
    DO jj=basin(i)%lakesection,1,-1 !jj decrase from n:o lakesection to 1 (upstream to downstream)
      ! lakesection qunitfactor
      qunitfactorsec = qunitfactor/(lakesectiondata(i,jj)%farea)

      ! add interflow to water stage
      wlmmsec(jj) = wlmmsec(jj) + qint * qunitfactorsec

      ! add interflow to local net inflow [m3/s]
      qinloc = qint + (qinmmsec(jj)+peinmmsec(jj))/qunitfactorsec

      ! Calculate and remove outflow from lakesection from general rating curve
      wlmr0 = wlmmsec(jj)*0.001 - (lakesectiondata(i,jj)%depth+basin(i)%lakedepth(itype)) !m
      IF(wlmr0.GT.0.)THEN
        qint  = average_flow_rating_curve(qinloc,lakearea*lakesectiondata(i,jj)%farea,wlmr0,ratingc,ratinge) !m3/s
        qintmm = qint*qunitfactorsec !mm/timestep
        !>Check outflow against lake volume
        IF(qintmm*0.001>wlmr0)THEN   !Check for enough water in lake (bad rating curve or numerical problems)
          IF(wlmr0>0.)THEN
            qintmm = wlmr0*1000.
          ELSE
            qintmm = 0.
          ENDIF
          IF(qintmm>wlmmsec(jj)) qintmm = wlmmsec(jj) !Safety for rounded wlmr and ldepth = 0
          qint = qintmm/qunitfactorsec
        ENDIF
        wlmmsec(jj) = wlmmsec(jj) - qintmm !mm
      ELSE
        qint  = 0.
      ENDIF
      qintsec(jj) = qint
    ENDDO

    !>_4_ Finalize output variables and update lakestate variable volfrac
    outflowm3s = qintsec(1)
    outflowmm  = outflowm3s*qunitfactor
    CALL remove_outflow_from_lake(i,itype,ns,outflowmm,subid,lakedepth_not_used,hypodepth_not_used,lakewstmm,coutflow,lakestate)

    !volfrac, fcon, fnca
    fnca=basin(i)%ilakecatch
    fcon=0.
    isconnected = .TRUE.
    DO j=1,basin(i)%lakesection    ! this time from downstream and up
      !volfrac
      IF(lakewstmm.GT.0.)THEN
        lakestate%volfrac(j,i) = wlmmsec(j) * lakesectiondata(i,j)%farea / lakewstmm
      ELSE
        lakestate%volfrac(j,i) = 0.
      ENDIF
      !fcon and fnca
      IF(isconnected)THEN
        IF(lakesectiondata(i,j)%farea.GT.0.)THEN
          IF(qintsec(j).GT.0.)THEN ! better would be to check the flow from this section
            fnca = AMAX1(0.,fnca - lakesectiondata(i,j)%ficatch * basin(i)%ilakecatch)
            fcon = AMIN1(1.,fcon + lakesectiondata(i,j)%farea)
          ELSE
            isconnected=.FALSE.
          ENDIF
        ENDIF
      ENDIF
    ENDDO

    !>Set other output variables
    IF(conductload) load(:,11) = outflowm3s * coutflow * seconds_per_timestep * 1.E-3 !Load at point K, outflow ilake (kg/timestep)
    IF(conductwb) volumeflow(w_iltomr,i) = outflowm3s * seconds_per_timestep  !m3/ts
    wst = lakestate%water(itype,i)  !mm

  END SUBROUTINE calculate_ilakesection_outflow

  !>\brief Subroutine for calculation outflow from last/only outlet of new lakebasin lake
  !>
  !!For outlet lakes several options exist:
  !!Specific rating curve, general rating curve, regulation with spill by rating curve,
  !!constant production flow depending on date or two separate rating curves for olake.
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_outflow_from_lakebasin_lake(i,qin,oldwholelakewst,   &
                                         outlb,outflowm3s,outflow)

    USE MODVAR, ONLY : missing_value, &
                       nsub,&
                       elake, &
                       lakebasin,  &
                       lakebasinindex,&
                       seconds_per_timestep

    !Argument declarations
    INTEGER, INTENT(IN) :: i               !<index of current subbasin (last lakebasin)
    REAL, INTENT(IN)    :: qin             !<net inflow of whole lake (m3/s)
    REAL, INTENT(IN)    :: oldwholelakewst !<lake wst above theshold (average of lake)
    LOGICAL, INTENT(OUT):: outlb(nsub)     !<status of subbasin with lakebasin with outflow of the lake (m3/s)
    REAL, INTENT(OUT)   :: outflowm3s      !<all outflow of lake (m3/s)
    REAL, INTENT(OUT)   :: outflow(nsub,2) !<outflow of main and second outlet (m3/s) and from which subbasin they comes

    !Local variables
    LOGICAL have2outlets        !Lake with two outlets
    INTEGER ioutlet             !index of outlets
    INTEGER i2                  !subbasin index of other outlets
    REAL wlmr,wlmr0             !water level lake  (m)
    REAL outletoutflow          !total outflow (m3/s) of current outlet
    REAL wmin                   !water levels threshold for production (m)
    REAL w0Today                !water level threshold  (m)
    REAL ratingc,ratinge        !current rating curve parameters outlet lake
    REAL damProd,maxQprod       !Current and maximum dam production flow (m3/s)
    REAL minflow,out2minflow    !Minimum flows (m3/s)
    REAL lakearea               !lakearea of lakebasin lake (m2)
    REAL qunitfactor            !factor for transforming flow for lake from m3/s to mm/timestep and back
    REAL out2ratingc,out2ratinge,out2w0Today,out2wmin,out2damProd,out2maxQprod !parameters for outlet 2 (not used)

    !Local parameters
    INTEGER, PARAMETER :: itype = 2  !lake type (outlet lake)

    !>\b Algorithm \n
    !> Set initial values
    outflowm3s = 0.
    outletoutflow = 0.
    outflow = 0.
    outlb = .FALSE.

    !>Set water level for lake
    lakearea = elake(lakebasin(lakebasinindex(i))%ilk)%area
    wlmr = oldwholelakewst  !above threshold w0 (not affected by delta w0)

    qunitfactor = seconds_per_timestep * 1000. / lakearea     !m3/s->mm/timestep

    !>Calculate outflow main outlet (last lakebasin)
    ioutlet = 1
    outlb(i) = .TRUE.
    !>Get current parameter values
    CALL get_current_lake_outflow_parameters(i,itype,ioutlet,lakearea,wlmr,   &
                 have2outlets,ratingc,ratinge,w0Today,  &
                 wmin,damProd,maxQprod,minflow,out2ratingc,out2ratinge,out2w0Today,out2wmin, &
                 out2damProd,out2maxQprod,out2minflow,qin)

    !>Outflow determination for one outlet lake
    wlmr0 = wlmr - w0Today
    IF(wmin==missing_value)THEN   !Not regulated lake
      IF(wlmr0>0.)THEN            !Rating curve used for water above threshold
        IF(ratingc>0.)THEN
          outletoutflow = MAX(average_flow_rating_curve(qin,lakearea,wlmr0,ratingc,ratinge), damProd) !damProd=0 here
        ELSE
          WRITE(6,*) 'Error: Ended in else that is not acceptable. Outflow of outlet lake'
          WRITE(6,*) 'Check input data for this olake.'
          WRITE(6,*) 'subbasin index i:',i
          WRITE(6,*) 'More info: wlmr',wlmr,'w0Today',w0Today,'wmin',wmin,'ratingc',ratingc
        ENDIF
      ENDIF
    ELSE          !Regulated lake
      IF(wlmr0>0.)THEN
        IF(ratingc>0)THEN     !Specific rating curve for production or dam spill
          outletoutflow = MAX(average_flow_rating_curve(qin,lakearea,wlmr0,ratingc,ratinge), damProd)
        ELSE      !Dam without rating curve for spill; all water above threshold but at least production
          outletoutflow = MAX(wlmr0*1000./qunitfactor, damProd)
        ENDIF
      ELSEIF(wlmr>wmin)THEN             !Production flow to lower threshold
        outletoutflow = MIN((wlmr-wmin) * 1000. / qunitfactor, damProd)
      ENDIF
    ENDIF

    !>Calculate flow in main channel and in branch from BranchData fractions
    CALL calculate_branched_flow(i,outletoutflow,outflow(i,1),outflow(i,2))
    outflowm3s = outletoutflow

    !>Calculate outflow of other outlets
    DO ioutlet = 2,elake(lakebasin(lakebasinindex(i))%ilk)%noutlet
      !>Find subbasin index that outlet originated from
      i2 = elake(lakebasin(lakebasinindex(i))%ilk)%outlet(ioutlet)%isb
      outlb(i2) = .TRUE.
      outletoutflow = 0.
      !>Get current parameter values
      CALL get_current_lake_outflow_parameters(i2,itype,ioutlet,lakearea,wlmr,   &
                   have2outlets,ratingc,ratinge,w0Today,  &
                   wmin,damProd,maxQprod,minflow,out2ratingc,out2ratinge,out2w0Today,out2wmin, &
                   out2damProd,out2maxQprod,out2minflow,qin)

      !>Outflow determination for this outlet (outflow in branch)
      wlmr0 = wlmr - w0Today
      IF(wmin==missing_value)THEN   !Not regulated lake
        IF(wlmr0>0.)THEN            !Rating curve used for water above threshold
          IF(ratingc>0.)THEN
            outletoutflow = MAX(average_flow_rating_curve(qin,lakearea,wlmr0,ratingc,ratinge), damProd) !damProd=0 here
          ELSE
            WRITE(6,*) 'Error: Ended in else that is not acceptable. Additional outflow.'
            WRITE(6,*) 'Check input data for this lakebasin lake.'
            WRITE(6,*) 'subbasin index i:',i2, 'outletindex: ',ioutlet
            WRITE(6,*) 'More info: wlmr',wlmr,'w0Today',w0Today,'wmin',wmin,'ratingc',ratingc
          ENDIF
        ENDIF
      ELSE          !Regulated lake
        IF(wlmr0>0.)THEN
          IF(ratingc>0)THEN     !Specific rating curve for production or dam spill
            outletoutflow = MAX(average_flow_rating_curve(qin,lakearea,wlmr0,ratingc,ratinge), damProd)
          ELSE      !Dam without rating curve for spill; all water above threshold but at least production
            outletoutflow = MAX(wlmr0*1000./qunitfactor-outflowm3s, damProd)
          ENDIF
        ELSEIF(wlmr>wmin)THEN             !Production flow to lower threshold
          outletoutflow = MIN((wlmr-wmin)*1000./qunitfactor-outflowm3s, damProd)
        ENDIF
      ENDIF

      !>Set calculated outlet outflow to branch and add to total outflow
      outflow(i2,1) = 0.
      outflow(i2,2) = outletoutflow
      outflowm3s = outflowm3s + outletoutflow
    ENDDO

  END SUBROUTINE calculate_outflow_from_lakebasin_lake

  !>\brief Subroutine for calculation outflow from outlet lake.
  !>Use for single lakes and dams, not for lakebasinlakes.
  !>
  !!For outlet lakes several options exist:
  !!Specific rating curve, general rating curve, all water above threshold for
  !!upstream lake basin, regulation with spill by rating curve,
  !!constant production flow depending on date or two separate rating curves for olake.
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_outflow_from_outlet_lake(i,qin,lakearea,lakewstmm,   &
                                         qunitfactor,outflowm3s,outflowmm, &
                                         outflow1,outflow2,maxQprodOUT,minFlowOUT)

    USE HYPEVARIABLES, ONLY : lakeoutlet
    USE MODVAR, ONLY : missing_value, &
                       branchindex

    !Argument declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    REAL, INTENT(IN)    :: qin           !<net inflow of lake (m3/s)
    REAL, INTENT(IN)    :: lakearea      !<lakearea (m2)
    REAL, INTENT(IN)    :: lakewstmm     !<lake water stage (mm)
    REAL, INTENT(IN)    :: qunitfactor   !<factor for transforming flow for lake from m3/s to mm/timestep and back
    REAL, INTENT(OUT)   :: outflowm3s    !<outflow of lake (m3/s)
    REAL, INTENT(OUT)   :: outflowmm     !<outflow of lake (mm)
    REAL, INTENT(OUT)   :: outflow1      !<outflow of first outlet (m3/s) or all outflow if not lake with 2 outlets in LD
    REAL, INTENT(OUT)   :: outflow2      !<outflow of second outlet (m3/s)
    REAL, INTENT(OUT)   :: maxQprodOUT   !<temporary maximum production for updated flow (m3/s)
    REAL, INTENT(OUT)   :: minFlowOUT    !<current minimum flow for updated flow (m3/s)

    !Local variables
    LOGICAL have2outlets        !Lake with two outlets
    REAL wlmr,wlmr0             !water level lake  (m)
    REAL wmin                   !water levels threshold for production (m)
    REAL w0ref                  !water stage reference level (not used)
    REAL w0Today                !water level threshold  (m)
    REAL wthresh                !water level threshold for checking volume (m)
    REAL ratingc,ratinge        !current rating curve parameters outlet lake
    REAL damProd,maxQprod       !Current and maximum dam production flow (m3/s)
    REAL minflow,out2minflow    !Minimum flows (m3/s)
    REAL out2ratingc,out2ratinge,out2w0Today,out2wmin,out2damProd,out2maxQprod !parameters for outlet 2
    REAL wcheck1, wcheck2       !water level thresholds for check of flow against volume
    REAL outflowmmnew,outflownew

    !Local parameters
    INTEGER, PARAMETER :: itype = 2  !lake type (outlet lake)

    !>\b Algorithm \n
    !> Set initial values
    outflowm3s = 0.
    outflowmm = 0.
    outflow1 = 0.
    outflow2 = 0.
    maxQprodOUT = 0.
    minFlowOUT = 0.

    !>Calculate water level for outlet lake
    CALL calculate_olake_waterstage(i,lakewstmm,wlmr,w0ref)

    !>Get current parameter values
    CALL get_current_lake_outflow_parameters(i,itype,0,lakearea,wlmr,   &
                 have2outlets,ratingc,ratinge,w0Today,  &
                 wmin,damProd,maxQprod,minflow,out2ratingc,out2ratinge,out2w0Today,out2wmin, &
                 out2damProd,out2maxQprod,out2minflow,qin)

    !>Outflow determination (and check) for two outlet lake
    IF(have2outlets)THEN    !Outlet lake with two outlets
      CALL calculate_lake_outlet_outflow(i,lakeoutlet(branchindex(i))%otype(1), &
                                    qin,lakearea,wlmr,ratingc,  &
                                    ratinge,w0Today,wmin,damProd,wcheck1,outflow1)
      CALL calculate_lake_outlet_outflow(i,lakeoutlet(branchindex(i))%otype(2), &
                                    qin,lakearea,wlmr,out2ratingc,  &
                                    out2ratinge,out2w0Today,out2wmin,out2damProd,wcheck2,outflow2)
      IF(lakeoutlet(branchindex(i))%otype(1)==2 .OR. lakeoutlet(branchindex(i))%otype(1)==7) &  !Check against max production
        CALL calculate_maxprod_outflow(outflow1,outflow2,maxQprod,out2minflow)
      IF(lakeoutlet(branchindex(i))%otype(2)==2 .OR. lakeoutlet(branchindex(i))%otype(2)==7) &
        CALL calculate_maxprod_outflow(outflow2,outflow1,out2maxQprod,minflow)
      IF(lakeoutlet(branchindex(i))%change==4 .OR. &
          lakeoutlet(branchindex(i))%change==6) maxQprodOUT = damProd   !Set temporary maxProd for recalculating branched flow (step 1)
      IF(lakeoutlet(branchindex(i))%change==5 .OR. &
          lakeoutlet(branchindex(i))%change==7) maxQprodOUT = out2damProd
      IF(lakeoutlet(branchindex(i))%change==7 .OR. &
          lakeoutlet(branchindex(i))%change==9) minflowOUT = minflow   !Set current minflow for recalculating branched flow
      IF(lakeoutlet(branchindex(i))%change==6 .OR. &
          lakeoutlet(branchindex(i))%change==8) minflowOUT = out2minflow
      outflowm3s = outflow1 + outflow2
      outflowmm = outflowm3s * qunitfactor        !to mm/ts
      IF((wlmr-MIN(wcheck1,wcheck2)>0. .AND. outflowmm>(wlmr-MIN(wcheck1,wcheck2))*1.E3) .OR. outflowmm>lakewstmm)THEN !Check against available water
        outflowmmnew = MIN((wlmr-MIN(wcheck1,wcheck2))*1.E3,lakewstmm)
        outflownew = outflowmmnew / qunitfactor
        IF(outflownew/=outflowm3s)THEN
          IF(lakeoutlet(branchindex(i))%change==4 .OR. &
              lakeoutlet(branchindex(i))%change==6) maxQprod = maxQprodOUT   !Set temporary maxProd for recalculating branched flow (step 2)
          IF(lakeoutlet(branchindex(i))%change==5 .OR. &
              lakeoutlet(branchindex(i))%change==7) out2maxQprod = maxQprodOUT
          CALL recalculate_branched_flow_two_outlets(lakeoutlet(branchindex(i))%change, &
                      outflownew,maxQprod,out2maxQprod,minflow,out2minflow,outflow1,outflow2)
          outflowm3s = outflownew
          outflowmm = outflowmmnew
        ENDIF
      ENDIF
      RETURN
    ENDIF

    !>Outflow determination for one outlet lake
    wlmr0 = wlmr -w0Today
    IF(wmin==missing_value)THEN   !Not regulated lake
      IF(wlmr0>0.)THEN            !Rating curve used for water above threshold
        IF(ratingc>0.)THEN
          outflowm3s = MAX(average_flow_rating_curve(qin,lakearea,wlmr0,ratingc,ratinge), damProd) !damProd=0 here
        ELSE
          WRITE(6,*) 'Error: Ended in else that is not acceptable. Outflow of outlet lake'
          WRITE(6,*) 'Check input data for this lake.'
          WRITE(6,*) 'i',i,'itype',itype
          WRITE(6,*) 'More info: wlmr',wlmr,'w0Today',w0Today,'wmin',wmin,'ratingc',ratingc
        ENDIF
      ENDIF
      outflow1 = outflowm3s
    ELSE          !Regulated lake
      IF(wlmr0>0.)THEN
        IF(ratingc>0)THEN     !Specific rating curve for production or dam spill
          outflowm3s = MAX(average_flow_rating_curve(qin,lakearea,wlmr0,ratingc,ratinge), damProd)
        ELSE      !Dam without rating curve for spill; all water above threshold but at least production
          outflowm3s = MAX(wlmr0*1000./qunitfactor, damProd)
        ENDIF
      ELSEIF(wlmr>wmin)THEN             !Production flow to lower threshold
        outflowm3s = MIN((wlmr-wmin) * 1000. / qunitfactor, damProd)
      ENDIF
      outflow1 = outflowm3s
    ENDIF
    outflowmm = outflowm3s * qunitfactor        !to mm/ts

    !>Check outflow against lake volume (bad rating curve parameters or numerical problems)

    !>\li Calculate threshold for checking
    IF(wmin==missing_value)THEN
      wthresh = w0Today
    ELSE
      wthresh = wmin
    ENDIF

    !>\li Check against lowest water level allowed and lake volume
    IF(outflowmm*0.001>wlmr-wthresh)THEN
      IF(wlmr>wthresh)THEN
        outflowmm = (wlmr-wthresh)*1000.
      ELSE
        outflowmm = 0.
      ENDIF
      IF(outflowmm>lakewstmm) outflowmm = lakewstmm   !Safety for rounded wlmr used.
      outflowm3s = outflowmm/qunitfactor
    ENDIF

    !>Calculate flow in main channel and in branch from BranchData fractions
    CALL calculate_branched_flow(i,outflowm3s,outflow1,outflow2)

  END SUBROUTINE calculate_outflow_from_outlet_lake

  !>\brief Subroutine for calculating current outflow of one lake outlet
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_lake_outlet_outflow(i,otype,qin,lakearea,wlmr,ratc,ratexp,w0Today,  &
                                      wmin,damProd,wcheck,outflow)

    USE HYPEVARIABLES, ONLY : o_dwtr
    USE MODVAR, ONLY : basin, &
                       outvarid, &
                       xobsi,xobsindex

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<current subbasin
    INTEGER, INTENT(IN) :: otype    !<outlet type as defined in lakeoutlet (1-7)
    REAL, INTENT(IN)    :: qin      !<net inflow of lake (m3/s)
    REAL, INTENT(IN)    :: lakearea !<lakearea (m2)
    REAL, INTENT(IN)    :: wlmr     !<lake water stage (m)
    REAL, INTENT(IN)    :: ratc     !<rating curve coefficient
    REAL, INTENT(IN)    :: ratexp   !<rating curve exponent
    REAL, INTENT(IN)    :: w0Today  !<upper threshold (m)
    REAL, INTENT(IN)    :: wmin     !<lower threshold (m)
    REAL, INTENT(IN)    :: damProd  !<current production flow (m3/s)
    REAL, INTENT(OUT)   :: wcheck   !<current outflow threshold (m)
    REAL, INTENT(OUT)   :: outflow  !<current outflow (m3/s)

    outflow = 0.

    !>Calculate outflow for current outlet type
    IF(otype==1 .OR. otype==2 .OR. otype==8)THEN
      IF(wlmr>wmin)THEN             !Production flow to lower threshold
        outflow = damProd
        wcheck = wmin
      ENDIF
    ELSEIF(otype==3 .OR. otype==4 .OR. otype==6 .OR. otype==7)THEN
      IF(wlmr>w0Today)THEN          !Rating curve for water above threshold
        outflow = average_flow_rating_curve(qin,lakearea,wlmr-w0Today,ratc,ratexp)
        wcheck = w0Today
      ENDIF
    ELSEIF(otype==5 .OR. otype==9)THEN
      IF(wlmr>w0Today)THEN          !Production flow and rating curve above threshold
        outflow = average_flow_rating_curve(qin,lakearea,wlmr-w0Today,ratc,ratexp)
        outflow = MAX(outflow, damProd)
      ELSEIF(wlmr>wmin)THEN
        outflow = damProd
      ENDIF
      wcheck = wmin
    ELSEIF(otype==10)THEN
      IF(wlmr>0)THEN          !Outlet 2 flow provided
        IF(xobsindex(o_dwtr,i)<=0)THEN
          !TODO: check this earlier?!
          WRITE(6,*) 'ERROR: Time series of demanded water transfer (',outvarid(o_dwtr)%shortname,') is missing in Xobs for subid: ',basin(i)%subid
          STOP 1
        ENDIF
        outflow = xobsi(xobsindex(o_dwtr,i))
      ENDIF
      wcheck = 0
    ENDIF

  END SUBROUTINE calculate_lake_outlet_outflow

  !>\brief Subroutine for increasing production flow to maximum before using
  !!overflow branch
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_maxprod_outflow(outflow1,outflow2,maxQprod,minflow2)

    !Argument declarations
    REAL, INTENT(INOUT) :: outflow1   !<outflow of first outlet (m3/s)
    REAL, INTENT(INOUT) :: outflow2   !<outflow of second outlet (m3/s)
    REAL, INTENT(IN)    :: maxQprod   !<maximum production (m3/s), second priority
    REAL, INTENT(IN)    :: minflow2   !<minimum flow (m3/s), first priority

    !Local variables
    REAL outflowm3s   !total flow

    !>Redistribute flow for maximum production
    outflowm3s = outflow1 + outflow2
    IF(outflowm3s<minflow2)THEN
      outflow1 = 0.
      outflow2 = outflowm3s
    ELSEIF(outflowm3s>maxQprod+minflow2)THEN
      outflow1 = maxQprod
      outflow2 = outflowm3s - outflow1
    ELSE
      outflow1 = outflowm3s - minflow2
      outflow2 = minflow2
    ENDIF

  END SUBROUTINE calculate_maxprod_outflow

  !>\brief Momentanous flow by rating curve
  !>
  !>Subroutine for calculation momentanous outflow from lake from current lake
  !>water stage by simple lake rating curve equation. Does not work for upstream lakebasins.
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_flow_from_outlet_lake_waterstage(i,ioutlet,lakeareain,lakewstmm,outflowm3s)

    USE GENERAL_FUNCTIONS, ONLY : simple_rating_curve

    !Argument declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    INTEGER, INTENT(IN) :: ioutlet       !<index of outlet with main outflow which flow will be affected
    REAL, INTENT(IN)    :: lakeareain    !<lakearea (m2)
    REAL, INTENT(IN)    :: lakewstmm     !<lake water stage (mm)
    REAL, INTENT(OUT)   :: outflowm3s    !<outflow of lake (m3/s)

    !Local variables
    LOGICAL have2outlets        !Lake with two outlets
    REAL wlmr                   !water level lake (m)
    REAL wmin                   !water level threshold in w-reference system (m)
    REAL w0Today                !water level threshold in w-reference system (m)
    REAL w0ref                  !water stage reference level (m) (not used)
    REAL ratingc,ratinge        !general rating curve parameters outlet lake
    REAL damProd                !Dam production flow
    REAL out2ratingc,out2ratinge,out2w0Today,out2wmin,out2damProd !parameters for outlet 2
    REAL maxProd,out2maxProd    !parameters not used here
    REAL minflow,out2minflow    !parameters not used here

    !Local constant
    INTEGER, PARAMETER :: itype = 2   !lake type (outlet lake)

    !Initial values
    outflowm3s = 0.

    CALL calculate_olake_waterstage(i,lakewstmm,wlmr,w0ref)

    !Current parameter values
    CALL get_current_lake_outflow_parameters(i,itype,ioutlet,lakeareain,wlmr,   &
                 have2outlets,ratingc,ratinge,w0Today,  &
                 wmin,damProd,maxProd,minflow,out2ratingc,out2ratinge,out2w0Today,out2wmin, &
                 out2damProd,out2maxProd,out2minflow)

    !Outflow determination
    IF(ratingc>0)THEN
      outflowm3s = simple_rating_curve(wlmr,ratingc,ratinge,w0Today)
    ELSE
      outflowm3s = 0. !Error in indata reaching this else?
    ENDIF

  END SUBROUTINE calculate_flow_from_outlet_lake_waterstage

  !>\brief Removal of outflow from lake and setting of
  !>concentration of outflow.
  !>
  !>\b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions)
  !-----------------------------------------------------------------------
  SUBROUTINE remove_outflow_from_lake(i,itype,ns,outflowmm,subid,ldepthm,hypodepth,lakewstmm,coutflow,lakestate)

    USE HYPEVARIABLES, ONLY : m_gt2mix,     &
                              m_ldt2mix
    USE MODVAR, ONLY : lakedatapar,     &
                       lakedataparindex, &
                       realzero, &
                       genpar, &
                       i_t2

    !Argument declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    INTEGER, INTENT(IN) :: itype         !<lake type (local or main)
    INTEGER, INTENT(IN) :: ns            !<number of substances
    REAL, INTENT(IN)    :: outflowmm     !<outflow of lake (mm/timestep)
    INTEGER, INTENT(IN) :: subid         !<subid of current subbasin, for error output
    REAL, INTENT(IN)    :: ldepthm       !<lake depth (m)
    REAL, INTENT(IN)    :: hypodepth     !<lake hypolimnion depth (m)
    REAL, INTENT(IN)    :: lakewstmm     !<lake water stage (mm)
    REAL, INTENT(OUT)   :: coutflow(ns)  !<concentration of outflow of lake
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state

    !Local variables
    INTEGER status
    REAL epidepth     !epilimnion depth (m)
    REAL wstabovethr  !water stage above lake threshold/dam crest (m)
    REAL t2conc       !outflow T2 temperature
    REAL upperpart    !part of outflow that is taken from the upper part of the lake at T2 calculations
    LOGICAL usestrattemp  !set water temp in outflow of lake to depend on lake temperature stratification

    !>\b Algorithm \n
    !>Preparations: default values
    coutflow = 0.
    usestrattemp = .FALSE.

    !>Preparations: T2-simulation with special outflow temperature of outlet lakes
    !Calculate the fraction of the outflow that will be taken from upper lake part
    IF(i_t2 > 0 .AND. itype == 2) THEN
      usestrattemp = .TRUE.
      wstabovethr = lakewstmm * 0.001 - ldepthm   !waterstage above lake threshold
      IF(wstabovethr > 0) THEN
        epidepth = lakewstmm * 0.001 - hypodepth
        upperpart = MAX(0.,MIN(1.,epidepth / wstabovethr))
      ELSE
        upperpart = 1.    !Dams with production flow will get all uppertemp!?
      ENDIF
      !Decide if mean temperature is used as outflow t2 concentration
      IF(genpar(m_gt2mix)==1)THEN
        usestrattemp = .FALSE.
      ELSEIF(lakedatapar(lakedataparindex(i,itype),m_ldt2mix)==1)THEN
        usestrattemp = .FALSE.
      ENDIF
      IF(usestrattemp) t2conc = upperpart * lakestate%uppertemp(itype,i) + (1-upperpart) * lakestate%lowertemp(itype,i)
    ENDIF

    !>Remove outflow and set outflow concentrations:
    IF(outflowmm>0)THEN
      !>\li Outflow is removed from lake
      IF(ns>0) coutflow(:) = lakestate%conc(:,itype,i)
      IF(usestrattemp) coutflow(i_t2) = t2conc
!      IF(outflowmm+realzero<=lakestate%water(itype,i))THEN   !CP200519
      IF(outflowmm+realzero<lakestate%water(itype,i))THEN
        CALL remove_water(lakestate%water(itype,i),ns,lakestate%conc(:,itype,i),outflowmm,coutflow,status)
        IF(status.NE.0) CALL error_remove_water(errstring(5),subid,i,itype)
      ELSE
        lakestate%water(itype,i) = 0.
        IF(ns>0) lakestate%conc(:,itype,i)=0.
      ENDIF
    ENDIF

  END SUBROUTINE remove_outflow_from_lake

  !>\brief Flow from rating curve.
  !>Estimates average lake outflow (m3/s) during one timestep by
  !>linearization of rating equation q = k*(w-w0)**p (further developed
  !>from Lindström, G., 2016. Lake water levels for calibration of the
  !> S-HYPE model. Hydrology Research 47,4, pp. 672-682. doi: 10.2166/nh.2016.019).
  !>
  !> \b Reference ModelDescription Chapter Rivers and lakes (Lakes - Common lake processes)
  !-----------------------------------------------------------------
  REAL FUNCTION average_flow_rating_curve(q_in,l_area,wst,k,p)

    USE MODVAR, ONLY : seconds_per_timestep, &
                       doublezero

    !Argument declarations
    REAL, INTENT(IN) :: q_in   !<inflow (m3/s)
    REAL, INTENT(IN) :: l_area !<lake area (m2)
    REAL, INTENT(IN) :: wst    !<current water level above threshold (m)
    REAL, INTENT(IN) :: k      !<rating curve coefficient
    REAL, INTENT(IN) :: p      !<rating curve exponent

    !Local variables
    DOUBLE PRECISION dh,h,h0,hr,qut,r,t1,t2,z

    qut = 0.D0
    dh = DBLE(q_in)*DBLE(seconds_per_timestep)/DBLE(l_area) !Inflow added in HYPE
    h0 = DBLE(wst)-dh !Initial height (m)
    IF(h0>0.D0) THEN
      t2 = DBLE(seconds_per_timestep)
      hr = h0
    ELSEIF (h0+dh>0.D0) THEN
      t1 = -DBLE(l_area)*h0/DBLE(q_in)
      t2 = DBLE(seconds_per_timestep)-t1
      hr = DBLE(q_in)*t2/DBLE(l_area)/10.D0
    ELSE
      t2 = 0.D0
    ENDIF

    IF(t2>0.D0) THEN
      r = DBLE(p)*DBLE(k)*(hr**(DBLE(p-1.)))/DBLE(l_area) !Linearized recession rate (1/sec)
      IF(r>doublezero)THEN
        z = hr+DBLE(q_in)/r/DBLE(l_area)-hr/DBLE(p)   !Auxiliary variable (m)
        h = (hr-z)*EXP(-r*t2)+z  !New height above threshold (m)
        qut = DBLE(q_in)-DBLE(l_area)*(h-h0)/DBLE(seconds_per_timestep)
        IF(qut<0.D0) qut = 0.D0
      ENDIF
    ENDIF
    average_flow_rating_curve = REAL(qut)

  END FUNCTION average_flow_rating_curve

  !>Calculate outlet lake water stage (m) in local reference system and for w-reference system
  !>
  !> \b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions, Lakes - Outlet lake (olake) as a lake basin)
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_olake_waterstage(i,lakewatermm,lakewst,w0ref)

    USE MODVAR, ONLY : basin,           &
                       dam,             &
                       damindex,        &
                       elake,           &
                       lake,            &
                       lakeindex,       &
                       lakebasin,       &
                       lakebasinindex

    !Arguments declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    REAL, INTENT(IN)    :: lakewatermm   !<outlet lake water content (mm)
    REAL, INTENT(OUT)   :: lakewst       !<outlet lake water stage above threshold (m)
    REAL, INTENT(OUT)   :: w0ref         !<level to be added for w-ref outlet lake water stage (m)

    !> \b Algoritm \n
    !Default output
    w0ref = 0.
    lakewst = lakewatermm * 0.001   !m, total

    !Lake water reference
    IF(ALLOCATED(lakeindex))THEN
      IF(lakeindex(i)>0)THEN
        w0ref = lake(lakeindex(i))%w0ref
      ENDIF
    ENDIF
    IF(ALLOCATED(damindex))THEN
      IF(damindex(i)>0)THEN
        w0ref = dam(damindex(i))%w0ref
      ENDIF
    ENDIF
    IF(ALLOCATED(lakebasinindex))THEN
      IF(lakebasinindex(i)>0)THEN
        w0ref = elake(lakebasin(lakebasinindex(i))%ilk)%w0ref
      ENDIF
    ENDIF

    !>Calculate lake water stage in local reference system
    lakewst = lakewst - basin(i)%lakedepth(2)

  END SUBROUTINE calculate_olake_waterstage

  !>Calculate average lake water stage (m) of a lakebasin lake in local
  !>reference system. Subroutine called for "last" lakebasin.
  !>
  !> \b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions, Lakes - Outlet lake as lake basin a part of a multi-basin lake with equal water level)
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_lakebasin_average_waterstage(ilast,lakearea,lakewst,w0ref,lakestate)

    USE MODVAR, ONLY : basin,           &
                       classbasin,      &
                       elake,           &
                       lakebasin,       &
                       lakebasinindex,  &
                       slc_olake

    !Arguments declarations
    INTEGER, INTENT(IN) :: ilast         !<index of current subbasin (last lakebasin)
    REAL, INTENT(OUT)   :: lakearea      !<outlet lake area (of whole lakebasin lake) (m2)
    REAL, INTENT(OUT)   :: lakewst       !<outlet lake water stage (m) above threshold/local ref-system
    REAL, INTENT(OUT)   :: w0ref         !<level to be added for w-ref outlet lake water stage (m)
    TYPE(lakestatetype),INTENT(IN) :: lakestate  !<Lake state

    !Local variables
    INTEGER isb        !subbasin-loop index
    REAL isb_lakewst   !lake water above threshold of lake basins in lake (m)
    REAL totalwater    !water volume above wmin (to be spread over whole lake area)

    !> \b Algoritm \n
    !>Collect whole lake data; lake water reference at threshold and area
    w0ref = elake(lakebasin(lakebasinindex(ilast))%ilk)%w0ref
    lakearea = elake(lakebasin(lakebasinindex(ilast))%ilk)%area

    !>Calculate volume of lake water in lakebasins above threshold
    totalwater = 0.
    DO isb=1,ilast
      IF(lakebasinindex(isb)>0)THEN
        IF(lakebasin(lakebasinindex(isb))%ilk == lakebasin(lakebasinindex(ilast))%ilk)THEN
          isb_lakewst = lakestate%water(2,isb) * 0.001 - basin(isb)%lakedepth(2)
          totalwater = totalwater + isb_lakewst*classbasin(isb,slc_olake)%part*basin(isb)%area  !m3 above threshold ("w0")
        ENDIF
      ENDIF
    ENDDO

    !>Calculate average water stage for whole lake (negative if below "w0")
    lakewst = totalwater/lakearea  !this method does not consider tresholds between subbasins

  END SUBROUTINE calculate_lakebasin_average_waterstage

  !>Calculate outlet lake water stage (m) in local reference system
  !>adjusted to "real" regulation amplitude
  !------------------------------------------------------------------
    SUBROUTINE calculate_regamp_adjusted_waterstage(i,lakewst,lakewstadj)

    USE MODVAR, ONLY : dam,             &
                       damindex,        &
                       elake,           &
                       lake,            &
                       lakeindex,       &
                       lakeout2index,   &
                       lakebasin,       &
                       lakebasinindex,  &
                       missing_value

    !Arguments declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    REAL, INTENT(IN)    :: lakewst       !<outlet lake water stage (above threshold) (m)
    REAL, INTENT(OUT)   :: lakewstadj    !<outlet lake water stage adjusted for "real" amplitude of regulation volume (m)

    !Local variables
    REAL wfactor       !regulation amplitude scaling factor

    !> \b Algoritm \n
    wfactor = missing_value
    lakewstadj = missing_value

    !>Get regulation amplitude adjustment factor
    !Case of single lake:
    IF(ALLOCATED(lakeindex))THEN
      IF(lakeindex(i)>0)THEN
        wfactor = lake(lakeindex(i))%wampcoeff
      ENDIF
    ENDIF
    IF(ALLOCATED(lakeout2index))THEN
      IF(lakeout2index(i)>0)THEN
        IF(wfactor==missing_value) wfactor = lake(lakeout2index(i))%wampcoeff
      ENDIF
    ENDIF
    IF(ALLOCATED(damindex))THEN
      IF(damindex(i)>0)THEN
        wfactor = dam(damindex(i))%wampcoeff
      ENDIF
    ENDIF

    !Case of lakebasin lake (last basin):
    IF(ALLOCATED(lakebasinindex))THEN
      IF(lakebasinindex(i)>0)THEN
        IF(lakebasin(lakebasinindex(i))%last)THEN
          wfactor = elake(lakebasin(lakebasinindex(i))%ilk)%wampcoeff
        ENDIF
      ENDIF
    ENDIF

    !>Calculate adjusted lake water stage
    lakewstadj = lakewst
    IF(wfactor/=missing_value .AND. lakewst<0.) lakewstadj = lakewst*wfactor

  END SUBROUTINE calculate_regamp_adjusted_waterstage

  !>Calculate division of subbasin outlet flow into main channel and branch
  !>based on information in BranchData.
  !>
  !> \b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions, Rivers - Main river)
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_branched_flow(i,totflow,mainflow,branchflow)

    USE HYPEVARIABLES, ONLY : o_dwtr
    USE MODVAR, ONLY : basin, &
                       outvarid, &
                       branchdata, &
                       branchindex, &
                       xobsi,xobsindex

    !Argument declaration
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    REAL, INTENT(IN)    :: totflow       !<outflow of subbasin
    REAL, INTENT(OUT)   :: mainflow      !<flow in main channel
    REAL, INTENT(OUT)   :: branchflow    !<flow in branch

    !Local variables
    REAL part, minQmain, maxQmain, maxQbranch

    !> \b Algorithm \n
    !>Initialisation, default is all flow in main (single) channel
    mainflow   = totflow
    branchflow = 0.

    !>Check for branch existance and flow>0
    IF(.NOT.ALLOCATED(branchdata)) RETURN
    IF(branchindex(i)==0) RETURN
    IF(totflow == 0) RETURN

    !>Set current parameter values
    part = branchdata(branchindex(i))%mainpart
    maxQmain = branchdata(branchindex(i))%maxQ
    minQmain = branchdata(branchindex(i))%minQ
    maxQbranch = branchdata(branchindex(i))%maxQbranch

    !>Calculate flow in main channel and in branch
    IF(totflow<0.)THEN
      mainflow = part * totflow   !to handle negative lakebasin flows
      branchflow = totflow - mainflow
    ELSEIF(.NOT.branchdata(branchindex(i))%recQbranch)THEN
      mainflow = totflow
      IF(totflow>minQmain)THEN
        mainflow = part * (totflow - minQmain) + minQmain
      ENDIF
      IF(maxQmain>0 .AND. mainflow>maxQmain)THEN
        mainflow = maxQmain
      ELSEIF(maxQbranch>0 .AND. (1.-part)*(totflow-minQmain)>maxQbranch)THEN
        mainflow = totflow - maxQbranch
      ENDIF
      branchflow = totflow - mainflow
    ELSE
      !branch flow provided
      IF(xobsindex(o_dwtr,i)<=0)THEN
        WRITE(6,*) 'ERROR: Time series of demanded water transfer (',outvarid(o_dwtr)%shortname,') is missing in Xobs for subid: ',basin(i)%subid
        STOP 1
      ENDIF
      branchflow = xobsi(xobsindex(o_dwtr,i))
      IF(branchflow>totflow) branchflow = totflow
      mainflow = totflow - branchflow
    ENDIF

  END SUBROUTINE calculate_branched_flow

  !>Recalculate subbasin outlet flow division into main channel and branch
  !>if total flow has changed.
  !>
  !> \b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions, Rivers - Main river)
  !------------------------------------------------------------------------------
  SUBROUTINE recalculate_branched_flow(i,totflow,maxProdin,minflowin,mainflow,branchflow)

    USE HYPEVARIABLES, ONLY : lakeoutlet
    USE MODVAR, ONLY : branchdata,    &
                       branchindex,   &
                       lake,   &
                       lakeindex,   &
                       lakeout2index

    !Argument declaration
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    REAL, INTENT(IN)    :: totflow       !<outflow of subbasin (updated)
    REAL, INTENT(IN)    :: maxProdin     !<temporary maximum production flow for recalculating flow
    REAL, INTENT(IN)    :: minflowin     !<current minimum flow for recalculating flow
    REAL, INTENT(INOUT) :: mainflow      !<flow in main channel
    REAL, INTENT(INOUT) :: branchflow    !<flow in branch

    !Local variables
    REAL simtotflow
    REAL maxQprod,maxQprod2
    REAL minflow,minflow2
    LOGICAL have2outflows

    !> \b Algorithm \n
    !>Check for changed total flow, otherwise keep main- and branchflow
    simtotflow = mainflow + branchflow
    IF(simtotflow == totflow) RETURN

    !>Check for branch existance, otherwise set new mainflow
    IF(.NOT.ALLOCATED(branchdata))THEN
      mainflow = totflow
      RETURN
    ENDIF
    IF(branchindex(i)==0)THEN
      mainflow = totflow
      RETURN
    ENDIF

    !>Set current parameter values for two outlet lake
    have2outflows = .FALSE.
    IF(ALLOCATED(lakeout2index))THEN  !Second outlet of lake/dam
      IF(lakeout2index(i)>0)THEN
        have2outflows = .TRUE.
        maxQprod = lake(lakeindex(i))%mqprod
        maxQprod2 = lake(lakeout2index(i))%mqprod
        IF(lakeoutlet(branchindex(i))%change==4 .OR. &
           lakeoutlet(branchindex(i))%change==6) maxQprod = maxProdin   !Set temporary maxProd for recalculating branched flow
        IF(lakeoutlet(branchindex(i))%change==5 .OR. &
           lakeoutlet(branchindex(i))%change==7) maxQprod2 = maxProdin
        minflow = 0.
        minflow2 = 0.
        IF(lakeoutlet(branchindex(i))%change==7 .OR. &
           lakeoutlet(branchindex(i))%change==9) minflow = minflowin   !Set current minflow for recalculating branched flow
        IF(lakeoutlet(branchindex(i))%change==6 .OR. &
           lakeoutlet(branchindex(i))%change==8) minflow2 = minflowin
      ENDIF
    ENDIF

    !>Recalculate main and branched flow for lake/dam with two outlets
    IF(have2outflows)THEN
      CALL recalculate_branched_flow_two_outlets(lakeoutlet(branchindex(i))%change,totflow,maxQprod,maxQprod2,minflow,minflow2,mainflow,branchflow)
    ELSE
      !>or from BranchData fractions.
      CALL calculate_branched_flow(i,totflow,mainflow,branchflow)
    ENDIF

  END SUBROUTINE recalculate_branched_flow

  !>Recalculate two outlet lake outflow division after total outflow has been changed
  !>
  !> \b Reference ModelDescription Chapter Rivers and lakes (Basic assumptions, Rivers - Main river)
  !------------------------------------------------------------------------------
  SUBROUTINE recalculate_branched_flow_two_outlets(cmethod,totflow,maxQprod,maxQprod2,minflow1,minflow2,simflow1,simflow2)

    !Argument declaration
    INTEGER, INTENT(IN) :: cmethod       !<code for change flow method
    REAL, INTENT(IN)    :: totflow       !<outflow of subbasin (updated)
    REAL, INTENT(IN)    :: maxQprod      !<maximum production flow, outlet 1
    REAL, INTENT(IN)    :: maxQprod2     !<maximum production flow, outlet 2
    REAL, INTENT(IN)    :: minflow1      !<minimum flow, outlet 1
    REAL, INTENT(IN)    :: minflow2      !<minimum flow, outlet 2
    REAL, INTENT(INOUT) :: simflow1      !<outflow of outlet 1
    REAL, INTENT(INOUT) :: simflow2      !<outflow of outlet 2

    !Local variables
    REAL simtotflow
    REAL outflow1,outflow2
    REAL add

    !> \b Algorithm \n
    !>Simple case; new flow is zero
    IF(totflow == 0)THEN
      simflow1 = 0.
      simflow2 = 0.
      RETURN
    ENDIF

    !>Initialisation
    !>Set current parameter values
    outflow1 = 0.
    outflow2 = 0.
    simtotflow = simflow1 + simflow2

    !>Recalculate main and branched flow for lake/dam with two outlets
    IF(cmethod==1 .OR. cmethod==4)THEN
    !>\li Hydropower plant with production flow and spill in different branches: check for maximum/current production flow
      outflow1=MIN(totflow,maxQprod)
      IF(totflow>outflow1) outflow2 = totflow - outflow1
    ELSEIF(cmethod==2 .OR. cmethod==5)THEN
      outflow2=MIN(totflow,maxQprod2)
      IF(totflow>outflow2) outflow1 = totflow - outflow2
    !>\li Minimum flow together with production flow and spill in different branches
    ELSEIF(cmethod==6 .OR. cmethod==8)THEN
      outflow2=MIN(totflow,minflow2)
      outflow1=MIN(totflow-outflow2,maxQprod)
      IF(totflow>outflow1+outflow2)THEN
        add = totflow-outflow1-outflow2
        outflow2=outflow2+add
      ENDIF
    ELSEIF(cmethod==7 .OR. cmethod==9)THEN
      outflow1=MIN(totflow,minflow1)
      outflow2=MIN(totflow-outflow1,maxQprod2)
      IF(totflow>outflow1+outflow2)THEN
        add = totflow-outflow1-outflow2
        outflow1=outflow1+add
      ENDIF
    !>\li Other outflow division: keep relation between flow in the different branches
    ELSEIF(cmethod==3)THEN
      IF(simtotflow>0.)THEN
        outflow1 = simflow1 * (totflow/simtotflow)
        outflow2 = simflow2 * (totflow/simtotflow)
      ELSE
        outflow1 = totflow   !Zero flow calculated: put flow in main channel
      ENDIF
    ELSE
      !? Unknown how to recalculate branched flow
      IF(simtotflow>0.)THEN
        outflow1 = simflow1 * (totflow/simtotflow)
        outflow2 = simflow2 * (totflow/simtotflow)
      ELSE
        outflow1 = totflow
      ENDIF
    ENDIF

    !Set changed output flows
    simflow1 = outflow1
    simflow2 = outflow2


  END SUBROUTINE recalculate_branched_flow_two_outlets

  !>Find information about how to calculate subbasin outlet flows and flow division
  !>into main channel and branch
  !>
  !>\b Consequences Module hypevariable structure lakeoutlet will be allocated and set
  !------------------------------------------------------------------------------
  SUBROUTINE set_lake_outlets()

    USE HYPEVARIABLES, ONLY : lakeoutlet  !OUT
    USE MODVAR, ONLY : basin, &
                       branchdata,   &
                       branchindex,   &
                       missing_value, &
                       nsub, &
                       lake,    &
                       lakeindex, &
                       lakeout2index

    !Argument declaration

    !Local variables
    INTEGER i,nbranch
    REAL deltaw0,wmin             !parameter values outlet 1
    REAL regrate,maxProd          !-"-
    REAL out2maxProd,out2regrate  !parameter values outlet 2
    REAL out2deltaw0,out2wmin,out2w0rel !-"-
    LOGICAL minflow,out2minflow      !flag for minimum flow outlet 1 and 2
    LOGICAL out2obsflow      !flag for wanted flow from dwtr in Xobs for outlet 2


    !> \b Algorithm \n
    !>Initialisation; check and allocate
    IF(.NOT.ALLOCATED(lakeout2index)) RETURN !?
    nbranch = SIZE(branchdata)
    IF(.NOT.ALLOCATED(lakeoutlet)) ALLOCATE(lakeoutlet(nbranch))

    DO i=1,nsub
      IF(lakeout2index(i)>0)THEN

        !Initial values
        regrate = 0.
        maxProd = 0.
        minflow = .FALSE.
        deltaw0 = 0.
        wmin = missing_value
        out2regrate = 0.
        out2maxProd = 0.
        out2minflow = .FALSE.
        out2deltaw0 = 0.
        out2wmin = missing_value
        out2w0rel = 0.
        out2obsflow = .FALSE.

        !Parameter values
        maxProd = lake(lakeindex(i))%mqprod         !Maximum production flow
        minflow = lake(lakeindex(i))%minflow        !Minimum flow
        regrate = lake(lakeindex(i))%rate           !Rating curve parameter
        deltaw0 = lake(lakeindex(i))%deltaw0        !difference in lake threshold/"dämningsgräns" period 2
        wmin = lake(lakeindex(i))%wmin              !lake threshold/"sänkningsgräns"
        out2maxProd = lake(lakeout2index(i))%mqprod         !Maximum production flow
        out2minflow = lake(lakeout2index(i))%minflow        !Minimum flow
        out2regrate = lake(lakeout2index(i))%rate           !Rating curve parameter
        out2deltaw0 = lake(lakeout2index(i))%deltaw0        !difference in lake threshold/"dämningsgräns" period 2
        out2wmin = lake(lakeout2index(i))%wmin              !lake threshold/"sänkningsgräns"
        out2w0rel = lake(lakeout2index(i))%w0ref            !w0 of outlet 2 relative to outlet 1 (w0ref)
        out2obsflow = lake(lakeout2index(i))%obsflow

        !Outlet types:
        !1: Production flow (defined by wmin)
        !2: Production flow with higher maximum production (defined by wmin and mqprod)
        !3: Overflow (rate)
        !4: Additional overflow in branch (rate, w0rel)
        !5: Production flow and overflow (wmin, rate)
        !6: Production flow based on two-time thresholds (defined by rate, deltaw0)
        !7: Production flow based on two-time thresholds with higher maximum production (rate, deltaw0, mqprod)
        !8: Minimum flow (defined by wmin, minflow) ("mintappning")
        !9: Minimum flow and overflow (wmin, rate, minflow) ("mintappning" and "spill")
        !10: Wanted flow from time series
        !>Find lake outlet type, outlet 1
        IF(wmin/=missing_value)THEN
          IF(maxProd>0.)THEN
            lakeoutlet(branchindex(i))%otype(1) = 2
          ELSEIF(minflow)THEN
            IF(regrate>0.)THEN
              lakeoutlet(branchindex(i))%otype(1) = 9
            ELSE
              lakeoutlet(branchindex(i))%otype(1) = 8
            ENDIF
          ELSEIF(regrate>0.)THEN
            lakeoutlet(branchindex(i))%otype(1) = 5
          ELSE
            lakeoutlet(branchindex(i))%otype(1) = 1
          ENDIF
        ELSEIF(regrate>0.)THEN
          IF(deltaw0>0.)THEN
            IF(maxProd>0.)THEN
              lakeoutlet(branchindex(i))%otype(1) = 7
            ELSE
              lakeoutlet(branchindex(i))%otype(1) = 6
            ENDIF
          ELSE
            lakeoutlet(branchindex(i))%otype(1) = 3
          ENDIF
        ELSE
          WRITE(6,*) 'ERROR: Not allowed combination of LakeData parameters' !?
          WRITE(6,*) 'ERROR: Check LakeData. Subbasin: ',basin(i)%subid
          STOP 1
        ENDIF
       !>Find lake outlet type, outlet 2
        IF(out2obsflow)THEN
          lakeoutlet(branchindex(i))%otype(2) = 10
        ELSEIF(out2wmin/=missing_value)THEN
          IF(out2maxProd>0.)THEN
            lakeoutlet(branchindex(i))%otype(2) = 2
          ELSEIF(out2minflow)THEN
            IF(out2regrate>0.)THEN
              lakeoutlet(branchindex(i))%otype(2) = 9
            ELSE
              lakeoutlet(branchindex(i))%otype(2) = 8
            ENDIF
          ELSEIF(out2regrate>0.)THEN
            lakeoutlet(branchindex(i))%otype(2) = 5
          ELSE
            lakeoutlet(branchindex(i))%otype(2) = 1
          ENDIF
        ELSEIF(out2regrate>0.)THEN
          IF(out2deltaw0>0.)THEN
            IF(out2maxProd>0.)THEN
              lakeoutlet(branchindex(i))%otype(2) = 7
            ELSE
              lakeoutlet(branchindex(i))%otype(2) = 6
            ENDIF
          ELSEIF(out2w0rel/=0)THEN
            lakeoutlet(branchindex(i))%otype(2) = 4
          ELSE
            lakeoutlet(branchindex(i))%otype(2) = 3
          ENDIF
        ELSE
          WRITE(6,*) 'ERROR: Not allowed combination of LakeData parameters' !?
          WRITE(6,*) 'ERROR: Check LakeData. Subbasin: ',basin(i)%subid
          STOP 1
        ENDIF

        !>Set method for changing flow (after updating totalflow).
        !Method 1: main has priority and a maximum allowed value
        !Method 2: branch has priority and a maximum allowed value
        !Method 3: main and branch equal, change proportionally to old values
        !Method 4: main has priority and a wanted value
        !Method 5: branch has priority and a wanted value
        !Method 6: minflow in branch has priority, then prod in main, last outflow in branch
        !Method 7: minflow in main has priority, then prod in branch, last outflow in main
        !Method 8: minflow in branch has priority, then maxprod in main, last outflow in branch
        !Method 9: minflow in main has priority, then maxprod in branch, last outflow in main
        IF(lakeoutlet(branchindex(i))%otype(1) == 1)THEN
          IF(lakeoutlet(branchindex(i))%otype(2) == 1)THEN
            lakeoutlet(branchindex(i))%change = 3   !proportionally
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 3 .OR. &
                 lakeoutlet(branchindex(i))%otype(2) == 6)THEN
            lakeoutlet(branchindex(i))%change = 4   !mainprod without max
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 5 .OR. &
                 lakeoutlet(branchindex(i))%otype(2) == 10)THEN
            lakeoutlet(branchindex(i))%change = 3   !proportionally in lack of better method
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 7)THEN
            lakeoutlet(branchindex(i))%change = 2   !branchprodflag, maxProd exist
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 8)THEN
            lakeoutlet(branchindex(i))%change = 5   !minflow in branch, calculated as branchprod without max
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 9)THEN
            lakeoutlet(branchindex(i))%change = 6   !minflow in branch, prod in main
          ELSE
            WRITE(6,*) 'ERROR: Outlet combination of lake not allowed.'  !2 and 4
            WRITE(6,*) 'ERROR: Check LakeData. Subbasin: ',basin(i)%subid
            STOP 1
          ENDIF
        ELSEIF(lakeoutlet(branchindex(i))%otype(1) == 2)THEN
          IF(lakeoutlet(branchindex(i))%otype(2) == 5)THEN
            lakeoutlet(branchindex(i))%change = 3   !proportionally in lack of better method
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 9)THEN
            lakeoutlet(branchindex(i))%change = 8   !minflow in branch, prod in main
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 1 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 2 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 4 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 7 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 8 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 10)THEN
            WRITE(6,*) 'ERROR: Outlet combination of lake not allowed.'  !1,2,4,7,8,10
            WRITE(6,*) 'ERROR: Check LakeData. Subbasin: ',basin(i)%subid
            STOP 1
          ELSE
            lakeoutlet(branchindex(i))%change = 1   !mainprodflag, maxProd exist (3,6)
          ENDIF
        ELSEIF(lakeoutlet(branchindex(i))%otype(1) == 3)THEN
          IF(lakeoutlet(branchindex(i))%otype(2) == 1)THEN
            lakeoutlet(branchindex(i))%change = 5   !branchprod without max
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 8)THEN
            lakeoutlet(branchindex(i))%change = 5   !minflow in branch, calculated as branchprod without max
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 2 .OR. &
                 lakeoutlet(branchindex(i))%otype(2) == 7)THEN
            lakeoutlet(branchindex(i))%change = 2   !branchprodflag, maxProd exist
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 5 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 9)THEN
            WRITE(6,*) 'ERROR: Outlet combination of lake not allowed.'  !5,9
            WRITE(6,*) 'ERROR: Check LakeData.txt subbasin: ',basin(i)%subid
            STOP 1
          ELSE
            lakeoutlet(branchindex(i))%change = 3   !proportionally (3,4,6?,10)
          ENDIF
        ELSEIF(lakeoutlet(branchindex(i))%otype(1) == 5)THEN
          IF(lakeoutlet(branchindex(i))%otype(2) == 2)THEN
            lakeoutlet(branchindex(i))%change = 3   !proportionally in lack of better method
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 1 .OR. &
                 lakeoutlet(branchindex(i))%otype(2) == 4)THEN
            lakeoutlet(branchindex(i))%change = 3   !proportionally in lack of better method
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 7)THEN
            lakeoutlet(branchindex(i))%change = 2   !branchprodflag, maxProd finns
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 8)THEN
            lakeoutlet(branchindex(i))%change = 5   !minflow in branch, calculated as branchprod without max
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 3 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 9)THEN
            WRITE(6,*) 'ERROR: Outlet combination of lake not allowed.'  !3,9
            WRITE(6,*) 'ERROR: Check LakeData.txt subbasin: ',basin(i)%subid
            STOP 1
          ELSE
            lakeoutlet(branchindex(i))%change = 3   !proportionally (5,6?,10)
          ENDIF
        ELSEIF(lakeoutlet(branchindex(i))%otype(1) == 6)THEN
          IF(lakeoutlet(branchindex(i))%otype(2) == 2 .OR. &
             lakeoutlet(branchindex(i))%otype(2) == 7)THEN
            lakeoutlet(branchindex(i))%change = 2   !branchprodflag, maxProd finns
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 1)THEN
            lakeoutlet(branchindex(i))%change = 5   !branchprod without max
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 8)THEN
            lakeoutlet(branchindex(i))%change = 5   !minflow in branch, calculated as branchprod without max
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 9)THEN
            lakeoutlet(branchindex(i))%change = 6   !minflow in branch, prod in main
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 4)THEN
            WRITE(6,*) 'ERROR: Outlet combination of lake not allowed.'  !4
            WRITE(6,*) 'ERROR: Check LakeData.txt subbasin: ',basin(i)%subid
            STOP 1
          ELSE
            lakeoutlet(branchindex(i))%change = 3   !proportionally (3?,5?,6,10)
          ENDIF
        ELSEIF(lakeoutlet(branchindex(i))%otype(1) == 7)THEN
          IF(lakeoutlet(branchindex(i))%otype(2) == 2 .OR. &
             lakeoutlet(branchindex(i))%otype(2) == 4 .OR. &
             lakeoutlet(branchindex(i))%otype(2) == 7 .OR. &
             lakeoutlet(branchindex(i))%otype(2) == 8 .OR. &
             lakeoutlet(branchindex(i))%otype(2) == 10)THEN
            WRITE(6,*) 'ERROR: Outlet combination of lake not allowed.'  !2,4,7,8,10
            WRITE(6,*) 'ERROR: Check LakeData. Subbasin: ',basin(i)%subid
            STOP 1
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 9)THEN
            lakeoutlet(branchindex(i))%change = 8   !minflow in branch, maxprod in main
          ELSE
            lakeoutlet(branchindex(i))%change = 1   !mainprodflag, maxProd finns (1,3,5,6)
          ENDIF
        ELSEIF(lakeoutlet(branchindex(i))%otype(1) == 8)THEN
          IF(lakeoutlet(branchindex(i))%otype(2) == 1 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 3 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 5 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 6)THEN
            lakeoutlet(branchindex(i))%change = 4   !minflow in main, calculated as mainprod without max
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 2 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 2 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 7 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 8 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 9)THEN
            WRITE(6,*) 'ERROR: Outlet combination of lake not allowed.'  !2,7,8,9
            WRITE(6,*) 'ERROR: Check LakeData. Subbasin: ',basin(i)%subid
            STOP 1
          ELSE
            lakeoutlet(branchindex(i))%change = 3   !proportionally (10)
          ENDIF
        ELSEIF(lakeoutlet(branchindex(i))%otype(1) == 9)THEN
          IF(lakeoutlet(branchindex(i))%otype(2) == 1 .OR. &
             lakeoutlet(branchindex(i))%otype(2) == 6)THEN
            lakeoutlet(branchindex(i))%change = 7   !minflow in main, prod in branch
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 2 .OR. &
             lakeoutlet(branchindex(i))%otype(2) == 7)THEN
            lakeoutlet(branchindex(i))%change = 9   !minflow in main, maxprod in branch
          ELSEIF(lakeoutlet(branchindex(i))%otype(2) == 3 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 4 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 5 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 8 .OR. &
            lakeoutlet(branchindex(i))%otype(2) == 9)THEN
            WRITE(6,*) 'ERROR: Outlet combination of lake not allowed.'  !3,4,5,8,9
            WRITE(6,*) 'ERROR: Check LakeData. Subbasin: ',basin(i)%subid
            STOP 1
          ELSE
            lakeoutlet(branchindex(i))%change = 3   !proportionally (10)
          ENDIF
        ENDIF
      ENDIF
    ENDDO

  END SUBROUTINE set_lake_outlets

  !>Calculate different volumes of lakes for print out.
  !>Volume for ilakes, volume for olakes, and volume for whole lakes (basin divided).
  !------------------------------------------------------------------
  SUBROUTINE calculate_lake_volume(itype,i,dim,a,lakewi,lakebasinvol,lakevol,lakevolsum)

    USE MODVAR, ONLY : lakebasin, &
                       lakebasinindex, &
                       missing_value

    !Argument declarations
    INTEGER, INTENT(IN)   :: itype            !<lake type; ilake=1, olake=2
    INTEGER, INTENT(IN)   :: i                !<index of current subbasin
    INTEGER, INTENT(IN)   :: dim              !<size of variable
    REAL, INTENT(IN)      :: a                !<lake area (m2)
    REAL, INTENT(IN)      :: lakewi           !<lake water stage (mm)
    REAL, INTENT(INOUT)   :: lakebasinvol(2)  !<volume of olake and ilake
    REAL, INTENT(INOUT)   :: lakevol          !<volume of olake/volume for lake with basins in outlet basin
    REAL, INTENT(INOUT)   :: lakevolsum(dim)  !<to sum lakebasins to outlet basin (big enough)

    !Local variables
    INTEGER ilake !multibasin lake index

    !> \b Algoritm \n
    !> Calculate lake volume for current lake
    lakebasinvol(itype) = lakewi * 0.001 * a  !volume in lake (m3)

    !> If outlet lake
    IF(itype==2)THEN
      !\li Set volume for olake
      lakevol = lakebasinvol(itype)
      !\li For basin-lakes: calculate volume of outlet, upstream sub-lakebasins volumes set to missing
      IF(ALLOCATED(lakebasinindex))THEN
        IF(lakebasinindex(i) .NE. 0) THEN   !lakebasin
          ilake = lakebasin(lakebasinindex(i))%ilk
          lakevol = missing_value
          lakevolsum(ilake) = lakevolsum(ilake) + lakebasinvol(itype)
          IF(lakebasin(lakebasinindex(i))%last) THEN  !outlet of lakebasin-lake
            lakevol = lakevolsum(ilake)
          ENDIF
        ENDIF
      ENDIF
    ENDIF

  END SUBROUTINE calculate_lake_volume


  !>Calculate temperature(T2) processes in rivers
  !----------------------------------------------------------
   SUBROUTINE T2_processes_in_river(i,itype,temp,swrad,riversurft,riverarea,frozenstate,riverstate,freezeupday,freezeuparea)

    USE MODVAR, ONLY: genpar, &
                      i_t2,realzero, &
                      modeloption, &
                      p_lakeriverice
    USE HYPEVARIABLES, ONLY: m_t2trriver, &
                             m_riceTf, &
                             m_tcfriver, &
                             m_scfriver, &
                             m_ccfriver, &
                             m_lcfriver, &
                             m_stbcorr1, &
                             m_stbcorr2, &
                             m_stbcorr3, &
                             m_limt2exch

    !Argument variables
    INTEGER, INTENT(IN) :: i               !<index of subbasin
    INTEGER, INTENT(IN) :: itype           !<index of river type (local = 1, main = 2)
    REAL, INTENT(IN)    :: temp            !<air temperature
    REAL, INTENT(IN)    :: swrad           !<solar radiation
    REAL, INTENT(INOUT) :: riversurft(2)   !<water surface temperature
    REAL, INTENT(IN)    :: riverarea       !<river area (m2)
    TYPE(snowicestatetype),INTENT(IN)  :: frozenstate   !<Snow and ice states
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states
    INTEGER,INTENT(OUT) :: freezeupday
    REAL, INTENT(INOUT) :: freezeuparea     !<fraction of riverarea with newice formation

    !Local variables
    REAL t2transfcorr
    REAL watertemp, watervol,icefreefraction

    !Initiate heat deficit and freezeup flag and surface temp variables
    freezeuparea = 0.
    freezeupday = 0
    riversurft(itype) = 0.

    !Get total river water volume and mean T2 temperature
    CALL get_rivertempvol(i,itype,riverstate,watertemp,watervol)
    IF(watervol.LE.realzero .OR. riverarea.LE.realzero)RETURN    !Skip calculations if there is no water in the river
    watervol = watervol * 1000. / riverarea    !scale volume [m3] to depth [mm]

    IF(watervol.GT.realzero)THEN    !Skip calculations if there is no water in the river

      icefreefraction = 1. - frozenstate%rivericecov(itype,i)      !Fraction of icefree river surface area
      t2transfcorr = 1.      !Seasonal correction of T2 exchange coefficient

      !River-Atmosphere T2 exchange, only in ice-free conditions and if there is some water in the river
      IF(icefreefraction.GT.0.)THEN
        !River-atmosphere exchange
        ! optional models  (will be reduced to one option after some initial testing for EHYPE3.0 and SHYPE2012)
        SELECT CASE(modeloption(p_lakeriverice))
        CASE(2) ! new model based on Piccolroaz et al 2013, with modifications for fractional ice cover, and calculation of fractional freezup area
          CALL calculate_watersurface_heatbalance(temp,swrad,watertemp,watervol*riverarea*0.001,riverarea*icefreefraction, &
                                                  genpar(m_tcfriver),genpar(m_scfriver),genpar(m_ccfriver),genpar(m_lcfriver), &
                                                  genpar(m_limt2exch),freezeuparea,genpar(m_riceTf),genpar(m_stbcorr1),genpar(m_stbcorr2),genpar(m_stbcorr3))
        CASE(1) ! the simple air-water temperature exchange model (Johan/David), with modifications for fractional ice cover, and calculation of fractional freezup area
          CALL calculate_T2_transfer(temp,watertemp,watervol*riverarea*0.001,riverarea*icefreefraction,genpar(m_t2trriver)*t2transfcorr, &
                                     freezeuparea,genpar(m_riceTf))
        ENDSELECT

        !Check the freezeup conditions
        IF(freezeuparea.GT.0.)THEN
          !freezup area is the fraction of previously unfrozen area (riverarea*icefreefraction), where new ice formation is triggered
          !re-scale to a fraction of the entire river area:
          freezeuparea = freezeuparea * icefreefraction
          freezeupday = 1
        ENDIF

        !Assign update values to the riverstate variables
        CALL set_rivertemp(i,itype,riverstate,watertemp)

        !Assign river surface temperature if (partly) icefree conditions - it's later rescaled after ice calculations
        riversurft(itype) = riverstate%conc(i_t2,itype,i)
      ENDIF
    ELSE
      !Set T2 temperature to 0. if there is no water
      CALL set_rivertemp(i,itype,riverstate,0.)
    ENDIF

  END SUBROUTINE T2_processes_in_river

  !>Subroutine to calculate heat flow from water to ice
  !-----------------------------------------------------
  SUBROUTINE calculate_waterice_heatflow(vel,hw,Tw,Tm,Cwi,qhmin,qhmax,qhw)

!    USE MODVAR, ONLY : seconds_per_timestep    !TODO: make routine work for other timestep than day

    !Argument declarations
    REAL, INTENT(IN)  :: vel   !<(input)     river velocity (m/s)
    REAL, INTENT(IN)  :: hw    !<(input)     water depth (m)
    REAL, INTENT(IN)  :: Tw    !<(input)     water temperature (C)
    REAL, INTENT(IN)  :: Tm    !<(input)     river freezing temperature (C)
    REAL, INTENT(IN)  :: Cwi   !<(input)     heat exchange coefficient (suggested values 1622 W/s^0.8/C^2.6)
    REAL, INTENT(IN)  :: qhmin !<(input)     minimum heat flow (W/m2/s)
    REAL, INTENT(IN)  :: qhmax !<(input)     maximum heat flow (W/m2/s)
    REAL, INTENT(OUT) :: qhw   !<(output)    heat flow (MJ/m2/day)

    !Local variables
    REAL, PARAMETER :: W_to_MJday = 0.0864         !J/s to MJ/timestep, e.g. 86400 s/day * 1e-6 MJ/J = 0.0864

    !Heat flux calculation
    IF(hw.GT.0.)THEN
    !Yoshikawa et al, Journal of JSCE, Vol. 2, 203-213, 2014, limited by qhmin and qhmax
      qhw = AMIN1(qhmax,AMAX1(qhmin,(Tw-Tm)*(Cwi*vel**0.8)/(hw**0.2))) * W_to_MJday
    ELSE
      qhw = qhmin * W_to_MJday
    ENDIF

  END SUBROUTINE calculate_waterice_heatflow

  !>Calculate ice processes in rivers
  !----------------------------------------------------------
  SUBROUTINE ice_processes_in_river(i,itype,iluse,snowfall,temp,riversurftemp,  &
                  riverarea,swrad,frozenstate,riverstate, &
                  freezeupday,breakupday,freezeuparea)

    USE MODVAR, ONLY: genpar
    USE HYPEVARIABLES, ONLY: m_sndens0,&
                             m_ricesndens, &
                             m_ricetf, &
                             m_ricekika, &
                             m_ricekexp, &
                             m_ricetmelt,  &
                             m_ricermelt, &
                             m_ricewme,  &
                             m_ricetf, &
                             m_ricessmft, &
                             m_ricessmfr, &
                             m_ricebupo, &
                             m_riceqhmn, &
                             m_riceqhmx, &
                             m_ricecwi, &
                             m_rivvel

    !Argument declaration
    INTEGER, INTENT(IN) :: i                 !<index of subbasin
    INTEGER, INTENT(IN) :: itype             !<index of lake/river type
    INTEGER, INTENT(IN) :: iluse             !<index of landuse
    REAL,INTENT(IN)     :: snowfall          !<snowfall
    REAL,INTENT(IN)     :: temp              !<air temperature
    REAL,INTENT(INOUT)  :: riversurftemp(2)  !<water surface temperature
    REAL,INTENT(IN)     :: riverarea         !<river area, m2
    REAL,INTENT(IN)     :: swrad             !<shortwave radiation
    TYPE(snowicestatetype),INTENT(INOUT)  :: frozenstate   !<Snow and ice states
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate  !<River states
    INTEGER, INTENT(IN) :: freezeupday       !<status freeze-up day
    INTEGER, INTENT(OUT) :: breakupday       !<status ice break-up day
    REAL, INTENT(IN)     :: freezeuparea     !<fraction of river area with newice formation (calculated by surface heat balance function)

    !Local variables
    REAL driverwidt, dsnowdt
    REAL melt
    REAL newicesurftemp,newice,newbice,newicesnow,newicesnowdepth
    REAL newicepor,newdriverwidt,newdsnowdt
    REAL oldsnow, oldsurftemp
    INTEGER newbreakup
    REAL qh
    REAL waterdepth, watertemp

    !Local parameters
    REAL, PARAMETER :: dice = 0.917    !density of ice, fraction of water
    REAL, PARAMETER :: mm2cm = 0.1
    REAL, PARAMETER :: cm2mm = 10.

    !RiverIceModel: Initialization of some variables
    breakupday = 0
    newicesurftemp = 0.
    newice = 0.
    newbice = 0.
    newicesnow = 0.
    newicesnowdepth = 0.
    newbreakup = 0
    newicepor = 0.
    newdriverwidt=0.
    newdsnowdt=0.
    driverwidt=0.
    dsnowdt=0.

    !Get total river water volume to estimate depth
    CALL get_rivertempvol(i,itype,riverstate,watertemp,waterdepth)

    !Calculate river water depth [m]
    IF(waterdepth.LE.0. .OR. riverarea.LE.0.)THEN
      waterdepth = 0.
    ELSE
      waterdepth = waterdepth / riverarea !scale volume [m3] to depth [m]
    ENDIF

    !Heat flux from water to ice, MJ/m2/timestep by equation adopted from Yoshikawa
    CALL calculate_waterice_heatflow(genpar(m_rivvel),waterdepth,watertemp,genpar(m_ricetf),genpar(m_ricecwi), &
                                     genpar(m_riceqhmn),genpar(m_riceqhmx),qh)

    !New ice formation on "freezeuparea" (calculated by surface heat balance function)
    IF(freezeuparea>0.)THEN
      CALL calculate_icedepth(newicesurftemp, newice, &
                              newbice,newicepor,newicesnow,newicesnowdepth, &
                              temp,newdriverwidt,newdsnowdt,freezeupday,newbreakup, &
                              genpar(m_ricetf),genpar(m_ricekika),genpar(m_ricekexp),genpar(m_ricetmelt), &
                              genpar(m_ricessmft),genpar(m_ricessmfr),genpar(m_ricebupo),swrad,genpar(m_ricermelt),qh)
    ENDIF

    !Calculate development of the old river ice
    IF(frozenstate%riverice(itype,i)>0.)THEN

      !first guess is that the old ice (or snow) is melting at 0 degrees
      oldsurftemp = 0.

      !Snow on riverice calculation
      oldsnow = frozenstate%riversnow(itype,i)
      CALL calculate_snow_on_ice(iluse,i,snowfall,temp,melt,swrad,frozenstate%riversnow(itype,i),  &
                                 frozenstate%riversnowage(itype,i))

      !Update snow age and calculate snow depth for snow on ice
      CALL calculate_snowdepth(iluse,frozenstate%riversnow(itype,i),oldsnow,snowfall,temp,  &
                               genpar(m_ricesndens),frozenstate%riversnowage(itype,i),frozenstate%riversnowdepth(itype,i))

      !Ice depth calculation (incl. update of skin temperature)
      CALL calculate_icedepth(oldsurftemp, frozenstate%riverice(itype,i), &
                              frozenstate%riverbice(itype,i),frozenstate%rivericepor(itype,i),frozenstate%riversnow(itype,i),frozenstate%riversnowdepth(itype,i), &
                              temp,driverwidt,dsnowdt,freezeupday,breakupday, &
                              genpar(m_ricetf),genpar(m_ricekika),genpar(m_ricekexp),genpar(m_ricetmelt), &
                              genpar(m_ricessmft),genpar(m_ricessmfr),genpar(m_ricebupo),swrad,genpar(m_ricermelt),qh)

      !If river temperature is above freezing, use the excess heat to melt some river ice from below (see further in corresponding lake routine)
      CALL riverice_riverwater_interaction(i,itype,riverstate,frozenstate,riverarea,breakupday,driverwidt)
    ENDIF

    !Add new ice to the old ice
    IF(newice>0.)THEN
      IF(frozenstate%riverice(itype,i)>0. .AND. (frozenstate%rivericecov(itype,i)+freezeuparea)>0.)THEN
        frozenstate%riversnow(itype,i) = frozenstate%riversnow(itype,i)*frozenstate%rivericecov(itype,i)/(frozenstate%rivericecov(itype,i)+freezeuparea)
        frozenstate%riversnowdepth(itype,i) = frozenstate%riversnowdepth(itype,i)*frozenstate%rivericecov(itype,i)/(frozenstate%rivericecov(itype,i)+freezeuparea)
        frozenstate%riverice(itype,i) = (frozenstate%riverice(itype,i)*frozenstate%rivericecov(itype,i)+newice*freezeuparea)/(frozenstate%rivericecov(itype,i)+freezeuparea)
        frozenstate%riverbice(itype,i) = (frozenstate%riverbice(itype,i)*frozenstate%rivericecov(itype,i)+newbice*freezeuparea)/(frozenstate%rivericecov(itype,i)+freezeuparea)
        frozenstate%rivericepor(itype,i) = (frozenstate%riverice(itype,i)*frozenstate%rivericepor(itype,i)*frozenstate%rivericecov(itype,i)+newice*newicepor*freezeuparea)/((frozenstate%rivericecov(itype,i)+freezeuparea)*frozenstate%riverice(itype,i))
        riversurftemp(itype) = newicesurftemp*freezeuparea+oldsurftemp*frozenstate%rivericecov(itype,i)+riversurftemp(itype)*(1.-freezeuparea-frozenstate%rivericecov(itype,i))
        driverwidt = (driverwidt*frozenstate%rivericecov(itype,i)+newdriverwidt*freezeuparea)/(frozenstate%rivericecov(itype,i)+freezeuparea)
        dsnowdt = (dsnowdt*frozenstate%rivericecov(itype,i)+newdsnowdt*freezeuparea)/(frozenstate%rivericecov(itype,i)+freezeuparea)
        frozenstate%rivericecov(itype,i) = (frozenstate%rivericecov(itype,i)+freezeuparea)
      ELSE
        frozenstate%riversnow(itype,i) = 0.0
        frozenstate%riversnowage(itype,i) = 0.0
        frozenstate%riversnowdepth(itype,i) = 0.0
        frozenstate%riverice(itype,i) = newice
        frozenstate%riverbice(itype,i) = newbice
        frozenstate%rivericepor(itype,i) = newicepor
        riversurftemp(itype) = newicesurftemp * freezeuparea + riversurftemp(itype)*(1.-freezeuparea)
        driverwidt = newdriverwidt
        dsnowdt = newdsnowdt
        frozenstate%rivericecov(itype,i) = freezeuparea
        !Make sure breakupday is 0 (strange situation with complete meltout of old ice and newice formation at the same time)
        IF(breakupday==1) breakupday=0
      ENDIF
    ELSE
      !Only old ice remaining
      IF(frozenstate%riverice(itype,i).GT.0.)THEN
        !weighted surface temperature (oldice and open water surface temperature)
        riversurftemp(itype) = oldsurftemp * frozenstate%rivericecov(itype,i) + riversurftemp(itype)*(1.-frozenstate%rivericecov(itype,i))
      ELSE
        !no new snow and no old snow
        !check if there was complete meltout today, in that case make sure all variables are reset
        IF(breakupday.EQ.1)THEN
          frozenstate%riverice(itype,i) = 0.
          frozenstate%riverbice(itype,i) = 0.
          frozenstate%riversnow(itype,i) = 0.
          frozenstate%riversnowage(itype,i) = 0.
          frozenstate%riversnowdepth(itype,i) = 0.
          riversurftemp(itype) = genpar(m_riceTf) * frozenstate%rivericecov(itype,i) + riversurftemp(itype)*(1.-frozenstate%rivericecov(itype,i))
          frozenstate%rivericecov(itype,i) = 0.
          frozenstate%rivericepor(itype,i) = 0.
        ENDIF
      ENDIF
    ENDIF

  END SUBROUTINE ice_processes_in_river

  !>Calculate interaction between river water and river ice
  ! - heat from water temperature above freezing is used to melt river ice
  !   by reducing the fractional area, rather than reducing ice depth
  ! - latent heat correspondning to ice meltwater is also added to the water
  !--------------------------------------------------------------------------
  SUBROUTINE riverice_riverwater_interaction(i, itype, riverstate, frozenstate, riverarea, breakupday, driverwidt)

    USE MODVAR, ONLY : genpar, cwater,realzero
    USE HYPEVARIABLES, ONLY : m_riceTf,m_ricewme

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<current subbasin index
    INTEGER, INTENT(IN) :: itype    !<river type
    TYPE(riverstatetype),INTENT(INOUT) :: riverstate      !<River state
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate   !<Snow and ice states
    REAL, INTENT(IN) :: riverarea   !<river area (m2)
    INTEGER, INTENT(INOUT) :: breakupday !<status of river ice break up
    REAL, INTENT(IN) :: driverwidt


    !local variables
    REAL watertemp, watervol, icewater, meltheat, waterheat, meltwater, newwatertemp,oldicecover

    !parameters
    REAL, PARAMETER :: L = 3.35E5     !latent heat of freezing, J/kg
    REAL, PARAMETER :: dice = .917    !density of ice, fraction of water
    REAL, PARAMETER :: mm2cm = 0.1
    REAL, PARAMETER :: cm2mm = 10.

    !Get total river water volume and mean T2 temperature
    CALL get_rivertempvol(i,itype,riverstate,watertemp,watervol)
    IF(watervol.LE.realzero .OR. riverarea.LE.realzero)RETURN

    !scale volume [m3] to depth [mm], volume water per unit area
    watervol = watervol * 1000. / (riverarea)

    oldicecover = frozenstate%rivericecov(itype,i)
    IF(watervol.GT.realzero)THEN

      !available heat for melting (C * KG/M2 * 1000 * KJ/KG/C = J/M2), per unit area
      waterheat = (watertemp-genpar(m_riceTf)) * watervol * 1000. * cwater

      IF(waterheat.GT.0.)THEN
        !Try to melt some ice, but only if the ice did not already melt completely (breakupday==1)
        IF(breakupday.EQ.0)THEN
          ! !melt the ice, from below, in cm ice
          ! bottommelt = min(frozenstate%riverice(itype,i),waterheat/(L*dice)*mm2cm)
          ! meltheat   = bottommelt * (L*dice) * cm2mm
          ! meltwater = bottommelt * dice *cm2mm

          !river ice and snow mass, in mm water, per unit area of ice covered river
          icewater = frozenstate%riverice(itype,i)*dice*cm2mm*(1.-frozenstate%rivericepor(itype,i)) + frozenstate%riversnow(itype,i)

          !ice melt, in mm per unit area of ice-covered river
          ! - it is thus only the water below the ice which is interacting with the ice
          ! - the available heat is scaled with the "meltefficiency" parameter
          meltwater = min(icewater,genpar(m_ricewme)*waterheat/L)
          meltheat = meltwater * L

! 3) update the frozen states with bottom melt

          !frozenstate%riverice(itype,i)=max(0.,frozenstate%riverice(itype,i)-bottommelt)
          !IF(frozenstate%riverice(itype,i).GT.0.)THEN
          IF((icewater-meltwater).GT.0.)THEN
            !some ice remains, reduce ice content by reducing the fractional area
            frozenstate%rivericecov(itype,i) = min(1.,max(0.,frozenstate%rivericecov(itype,i)*(1.- meltwater/icewater)))
!            frozenstate%riverbice(itype,i)=max(0.,frozenstate%riverbice(itype,i)-bottommelt)
          ELSE
            !complete melt of the riverice
            frozenstate%riverice(itype,i) =0.
            frozenstate%riverbice(itype,i)=0.
            frozenstate%rivericepor(itype,i)=0.

            !add heat needed to melt the riversnow to the meltheat
            !meltheat = meltheat + frozenstate%riversnow(itype,i) * L

            !add snow to the meltwater
            !meltwater = meltwater + frozenstate%riversnow(itype,i)

            !reset the snow states
            frozenstate%riversnow(itype,i)=0.
            frozenstate%riversnowage(itype,i)=0.

            !and the ice cover state
            frozenstate%rivericecov(itype,i)=0.

            !set breakup flag to 1
            breakupday = 1
          ENDIF
        ELSE
          !Ice was already melted away by the icedepth function
          meltheat   = 0.
          meltwater  = 0.
        ENDIF
      ELSE
        meltheat = 0.
        meltwater = 0.
      ENDIF
! 4) use any remaining heat and the zero degree melt water to update the river state

      !remove melt heat from heat content of the lake water (this is now per unit area previously ice covered river)
      waterheat = waterheat - meltheat

      !add any previous surface melt water to the meltwater
      IF(driverwidt.GT.0)THEN
        meltwater = meltwater + driverwidt
      ENDIF

      !temperature of water from remaining heat content
      newwatertemp=max(waterheat/(watervol * 1000. * cwater) + genpar(m_riceTf),genpar(m_riceTf))

      !dilute with the meltwater, which is at freezing point
      newwatertemp=max(genpar(m_ricetf),newwatertemp * (watervol - meltwater)/watervol)

      !weighted temperature between (previously) ice covered and ice free water
      watertemp = watertemp * (1.-oldicecover) + newwatertemp * oldicecover
      !finally, assign update values to the riverstate variables
      CALL set_rivertemp(i,itype,riverstate,watertemp)
   ENDIF

  END SUBROUTINE riverice_riverwater_interaction

  !>Calculate lake hypolimnion depth
  !----------------------------------------------------------
  SUBROUTINE calculate_lake_hypolimnion_depth(i,lakestate,hypodepth)

    USE MODVAR, ONLY : basin,classbasin, &
                       slc_ilake,slc_olake, &
                       conduct,simulate,i_t2, &
                       floodindex,flooding
    USE STATETYPE_MODULE, ONLY : lakestatetype

    !Argument declarations
    INTEGER,INTENT(IN) :: i   !<current subbasin
    TYPE(lakestatetype),INTENT(INOUT)    :: lakestate   !<Lake states
    REAL, INTENT(OUT) :: hypodepth(2)   !<lake hypolimnion depth (m)

    !Local variables
    REAL lakearea  !m2
    REAL a,fpfrac

    hypodepth = 0.
    IF(.NOT.simulate%substance(i_t2))RETURN

    IF(slc_ilake>0)THEN
      a = classbasin(i,slc_ilake)%part
      IF(a>0)THEN
        lakearea = a * basin(i)%area       ![m2]
        hypodepth(1) = lakestate%water(1,i)*1.E-3 - lake_epilimnion_depth(lakearea)
      ENDIF
    ENDIF
    IF(slc_olake>0)THEN
      a = classbasin(i,slc_olake)%part
      IF(a>0)THEN
        lakearea = a * basin(i)%area     ![m2]
        IF(conduct%floodplain)THEN
          IF(floodindex(i)>0)THEN
            IF(flooding(floodindex(i))%fpfol>0.)THEN
              fpfrac = flooding(floodindex(i))%fpfol        !floodplain fraction of outlet lake area
              lakearea = a * basin(i)%area * (1.-fpfrac)    !m2
            ENDIF
          ENDIF
        ENDIF
        hypodepth(2) = lakestate%water(2,i)*1.E-3 - lake_epilimnion_depth(lakearea)
      ENDIF
    ENDIF

  END SUBROUTINE calculate_lake_hypolimnion_depth

  !>Calculate lake typical epilimnion depth
  !>
  !>\b Reference Hanna (1990) Evaluation of models predicting mixing depth.
  !>Can. J. Fish. Aquat. Sci. 47:940-947.
  !----------------------------------------------------------
  REAL FUNCTION lake_epilimnion_depth(lakearea)

    !Argument declarations
    REAL,INTENT(IN)   :: lakearea   !<lake area (m2)

    !Typical depth to thermocline, function of lake area (Hanna, 1990)
    lake_epilimnion_depth = 6.95 * (lakearea * 1.0E-6)**0.185


  END FUNCTION lake_epilimnion_depth

  !>Subroutine for calculation of snow on ice changes; snowfall addition and
  !>snow pack melting
  !------------------------------------------------------------------------
  SUBROUTINE calculate_snow_on_ice(iluse,i,snowfall,temp,melt,swrad,snow,snowage)

    USE MODVAR, ONLY : genpar
    USE HYPEVARIABLES, ONLY : m_licewcorr

    INTEGER, INTENT(IN) :: iluse    !<index of landuse
    INTEGER, INTENT(IN) :: i        !<index of subbasin
    REAL, INTENT(IN)    :: snowfall !<precipitation as snow (mm/timestep)
    REAL, INTENT(IN)    :: temp     !<air temperature (C)
    REAL, INTENT(OUT)   :: melt     !<snow melt (mm/timestep)
    REAL, INTENT(IN)    :: swrad    !<shortwave radiation (MJ/m2/day?)
    REAL, INTENT(INOUT) :: snow     !<snow pack (mm)
    REAL, INTENT(INOUT) :: snowage  !<snowage (timesteps)

    !Local variables
    REAL newsnow
    REAL snowcover
    REAL snowliq,refreeze

    !>\b Algorithm \n
    !>Set parameter values
    snowcover = 1.    !just set snowcover = 1., and introduce snowcover calculation on lake ice later...
    snowliq = 0.      !just use snowliq=0 for now -> refreeze=0

    !>Calculate snow melt
    CALL calculate_snowmelt(iluse,i,temp,swrad,snow,snowage,snowcover,melt,snowliq,refreeze)
    melt = MAX(0.,MIN(melt, snow))  !Safeguard

    !>Update the snow pack with snowfall and melting
    newsnow = MAX(0.,snow + genpar(m_licewcorr)*snowfall  - melt)
    snow = newsnow

  END SUBROUTINE calculate_snow_on_ice

  !>Calculate lake ice processes
  !----------------------------------------------------------
  SUBROUTINE ice_processes_in_lake(i,itype,iluse,snowfall,temp,lakesurftemp,  &
                                   swrad,frozenstate,lakestate,freezeupday, &
                                   breakupday,hypodepth,freezeuparea)

    USE MODVAR, ONLY: genpar
    USE HYPEVARIABLES, ONLY: m_sndens0, &
                             m_licesndens, &
                             m_licetf,   &
                             m_licekika, &
                             m_licekexp, &
                             m_licetmelt,  &
                             m_licermelt,  &
                             m_licewme, &
                             m_licessmft, &
                             m_licessmfr, &
                             m_licebupo, &
                             m_liceqhw

    !Argument declarations
    INTEGER, INTENT(IN) :: i                !<index of subbasin
    INTEGER, INTENT(IN) :: itype            !<index of lake/river type
    INTEGER, INTENT(IN) :: iluse            !<index of landuse
    REAL,INTENT(IN)     :: snowfall         !<snowfall
    REAL,INTENT(IN)     :: temp             !<air temp
    REAL,INTENT(INOUT)  :: lakesurftemp(2)  !<water surface temperature
    REAL,INTENT(IN)     :: swrad            !<shortwave radiation
    TYPE(snowicestatetype),INTENT(INOUT)  :: frozenstate   !<Snow and ice states
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state
    INTEGER, INTENT(IN) :: freezeupday   !<status freeze-up day
    INTEGER, INTENT(OUT) :: breakupday   !<status ice break-up day
    REAL, INTENT(IN)    :: hypodepth   !<hypolimnion depth (m)
    REAL, INTENT(IN)    :: freezeuparea    !<fractional water surface area with newice formation, given by temperature routine

    !Local variables
    REAL dlakewidt, dsnowdt
    REAL melt
    REAL oldsnow,oldsurftemp
    REAL newicesurftemp,newice,newbice,newicesnow,newicesnowdepth
    REAL newicepor,newdlakewidt,newdsnowdt
    INTEGER newbreakupday
    REAL qh

    !Initialization
    breakupday  = 0
    newicesurftemp = 0.
    newice = 0.
    newbice = 0.
    newicesnow = 0.
    newicesnowdepth = 0.
    newbreakupday=0
    newicepor=0.
    newdlakewidt=0.
    newdsnowdt=0.
    dlakewidt=0.
    dsnowdt=0.

    !Heat flux from water to limit ice growth (since there is no velocity in the lake, we use the minimum value)
    qh=genpar(m_liceqhw)*0.0864

    !Newice formation on "freezeuparea" (calculated by surface heat balance function)
    IF(freezeuparea.GT.0.)THEN
      CALL calculate_icedepth(newicesurftemp, newice, &
                              newbice,newicepor,newicesnow,newicesnowdepth, &
                              temp,dlakewidt,dsnowdt,freezeupday,newbreakupday, &
                              genpar(m_licetf),genpar(m_licekika),genpar(m_licekexp),genpar(m_licetmelt), &
                              genpar(m_licessmft),genpar(m_licessmfr),genpar(m_licebupo),swrad,genpar(m_licermelt),qh)
    ENDIF

    !Calculate development of the old lake ice
    IF(frozenstate%lakeice(itype,i).GT.0)THEN
       !FROZEN LAKE

       !first guess is that the ice (or snow) is melting at 0 degrees
       oldsurftemp = 0.0

       !snow on lakeice calculation
       oldsnow = frozenstate%lakesnow(itype,i)
       CALL calculate_snow_on_ice(iluse,i,snowfall,temp,melt,swrad,frozenstate%lakesnow(itype,i), &
                                  frozenstate%lakesnowage(itype,i))

       !Update snow age and snow depth for snow on ice
       CALL calculate_snowdepth(iluse,frozenstate%lakesnow(itype,i),oldsnow,snowfall,temp, &
                                genpar(m_licesndens),frozenstate%lakesnowage(itype,i),frozenstate%lakesnowdepth(itype,i))

       !Ice depth calculation (inlc. update of skin temperature)
       CALL calculate_icedepth(oldsurftemp,frozenstate%lakeice(itype,i),frozenstate%lakebice(itype,i), &
                               frozenstate%lakeicepor(itype,i),frozenstate%lakesnow(itype,i),frozenstate%lakesnowdepth(itype,i), &
                               temp,dlakewidt,dsnowdt,freezeupday,breakupday, &
                               genpar(m_licetf),genpar(m_licekika),genpar(m_licekexp),genpar(m_licetmelt), &
                               genpar(m_licessmft),genpar(m_licessmfr),genpar(m_licebupo),swrad,genpar(m_licermelt),qh)

       !Calculate bottom melt due to heat from lake water temperatures above freezing, as well as influence of surface melt on lake water temperature
       CALL calculate_lakeice_lakewater_interaction(itype,i,frozenstate,lakestate,dlakewidt,hypodepth,breakupday)

    ENDIF

    !Add new ice to the old ice
    IF(newice.GT.0.)THEN
      IF(frozenstate%lakeice(itype,i).GT.0. .AND. (frozenstate%lakeicecov(itype,i)+freezeuparea)>0.)THEN
         frozenstate%lakesnow(itype,i) = frozenstate%lakesnow(itype,i)* frozenstate%lakeicecov(itype,i)/(frozenstate%lakeicecov(itype,i)+freezeuparea)
         frozenstate%lakesnowdepth(itype,i) = frozenstate%lakesnowdepth(itype,i) * frozenstate%lakeicecov(itype,i)/(frozenstate%lakeicecov(itype,i)+freezeuparea)
         frozenstate%lakeice(itype,i) = (frozenstate%lakeice(itype,i)*frozenstate%lakeicecov(itype,i) + newice*freezeuparea)/(frozenstate%lakeicecov(itype,i)+freezeuparea)
         frozenstate%lakebice(itype,i) = (frozenstate%lakebice(itype,i)*frozenstate%lakeicecov(itype,i) + newbice*freezeuparea)/(frozenstate%lakeicecov(itype,i)+freezeuparea)
         frozenstate%lakeicepor(itype,i)    = (frozenstate%lakeice(itype,i) * frozenstate%lakeicepor(itype,i) * frozenstate%lakeicecov(itype,i) + newice * newicepor * freezeuparea) / ( (frozenstate%lakeicecov(itype,i)+freezeuparea) * frozenstate%lakeice(itype,i))
         lakesurftemp(itype) = newicesurftemp * freezeuparea + oldsurftemp * frozenstate%lakeicecov(itype,i) + lakesurftemp(itype)*(1. - freezeuparea - frozenstate%lakeicecov(itype,i))
         !not used yet - but could the total new ice growth could be an output variable
         dlakewidt = (dlakewidt*frozenstate%lakeicecov(itype,i) + newdlakewidt * freezeuparea)/(frozenstate%lakeicecov(itype,i)+freezeuparea)
         dsnowdt = (dsnowdt*frozenstate%lakeicecov(itype,i) + newdsnowdt * freezeuparea)/(frozenstate%lakeicecov(itype,i)+freezeuparea)
         frozenstate%lakeicecov(itype,i) = (frozenstate%lakeicecov(itype,i)+freezeuparea)
      ELSE
         frozenstate%lakesnow(itype,i) = 0.0
         frozenstate%lakesnowage(itype,i) = 0.0
         frozenstate%lakesnowdepth(itype,i) = 0.0
         frozenstate%lakeice(itype,i) = newice
         frozenstate%lakebice(itype,i) = newbice
         frozenstate%lakeicepor(itype,i) = newicepor
         lakesurftemp(itype) = newicesurftemp * freezeuparea + lakesurftemp(itype)*(1. - freezeuparea)
         !not used yet - but could the total new ice growth could be an output variable
         dlakewidt = newdlakewidt
         dsnowdt = newdsnowdt
         frozenstate%lakeicecov(itype,i) = freezeuparea
         !Make sure breakupday is 0 (strange situation with complete meltout of old ice and newice formation at the same time)
         IF(breakupday==1) breakupday=0
      ENDIF
    ELSE
      !Or just check breakup conditions of old ice, and/or update the lakesurf temperature
      IF(frozenstate%lakeice(itype,i).GT.0.)THEN
        lakesurftemp(itype) = oldsurftemp * frozenstate%lakeicecov(itype,i) + lakesurftemp(itype)*(1. - frozenstate%lakeicecov(itype,i))
      ELSE
        !no new snow and no old snow
        !check if there was complete meltout today, in that case make sure all variables are reset
        IF(breakupday.EQ.1)THEN
          frozenstate%lakeice(itype,i) = 0.
          frozenstate%lakebice(itype,i) = 0.
          frozenstate%lakeicepor(itype,i) = 0.
          frozenstate%lakesnow(itype,i) = 0.
          frozenstate%lakesnowage(itype,i) = 0.
          frozenstate%lakesnowdepth(itype,i) = 0.0
          lakesurftemp(itype) = genpar(m_liceTf) * frozenstate%lakeicecov(itype,i) + lakesurftemp(itype)*(1.-frozenstate%lakeicecov(itype,i))
          frozenstate%lakeicecov(itype,i) = 0.
        ENDIF
      ENDIF
    ENDIF

  END SUBROUTINE ice_processes_in_lake

  !>Calculate lake ice melt from heat from lake water, as well as influence of ice surface melt on lake water temperature
  ! - depending on lake type (fast and slow split or not, deep or shallow), a mean water temperature and water volume is
  !   calculated for the interaction with the lake ice. The resulting watertemperature is then assigned to the
  !   various lake water components
  ! - heat from water temperature above freezing is used to melt lake ice
  !   by reducing the fractional area, rather than reducing ice depth
  ! - latent heat correspondning to ice meltwater is also added to the water
  !----------------------------------------------------------
  SUBROUTINE calculate_lakeice_lakewater_interaction(itype,i,frozenstate,lakestate,dlakewidt,hypodepth,breakupday)

    USE MODVAR, ONLY: genpar,i_t2,cwater
    USE HYPEVARIABLES, ONLY: m_licetf, m_licewme !, m_lddeeplake, m_ldfastlake

    !Argument declarations
    INTEGER,INTENT(IN) :: i             !<index of subbasin
    INTEGER,INTENT(IN) :: itype         !<index of lake type (ilake = 1, olake = 2)
    TYPE(snowicestatetype),INTENT(INOUT)  :: frozenstate   !<Snow and ice states
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state
    REAL, INTENT(IN)   :: dlakewidt
    REAL, INTENT(IN)   :: hypodepth    !<hypolimnion depth (m)
    INTEGER, INTENT(INOUT) :: breakupday

    !Local variables
    INTEGER laketype
    REAL meantemp,meanwater,waterheat,meltheat,watertemp,watervol,icewater,meltwater,newwatertemp,oldicecov
    REAL epidepth

    !parameters
    real, parameter :: L = 3.35E5     ! latent heat of freezing, J/kg
    real, parameter :: dice = .917    ! density of ice, fraction of water
    real, parameter :: mm2cm = 0.1
    real, parameter :: cm2mm = 10.

!--------------------------------------------------------------------------------------
! lakewater-lakeice interaction:
!
! 1) find out how much water and at what temperature we have for melting ice from below
! 2) melt corresponding ice (from below: black ice, slush ice, snow)
! 3) update the frozen states
! 4) use remaining heat and heat from melt water to update the lake state
!
! the first and last step is complicated by the various lake water storage configurations
!--------------------------------------------------------------------------------------
    oldicecov = frozenstate%lakeicecov(itype,i)

!1)find out how much water and at what temperature we have for melting ice from below

       !Set local variables water stage and average temperature
       meanwater = lakestate%water(itype,i)
       IF(meanwater.GT.0.)THEN
         meantemp  = lakestate%conc(i_t2,itype,i)

         !Check lake depth, if thermal stratification
         IF(itype==2 .AND. hypodepth < meanwater*0.001 .AND. hypodepth>0.)THEN !why is this only possible for olakes?
           !Two-layer olake, waterdepth > thermocline, olake
           epidepth = meanwater * 0.001 - hypodepth
           !->derive lake uppertemp(t) from meantemp(t, preliminary) and lowertemp(t-1)
           lakestate%uppertemp(itype,i) = (meantemp * meanwater * 0.001 - hypodepth * lakestate%lowertemp(itype,i)) / epidepth

           !temperature and water volume interacting with the ice
           watertemp = lakestate%uppertemp(itype,i)
           watervol  = epidepth*1000.
           laketype  = 2
         ELSE
           !one-layer olake or ilakes
           watertemp = meantemp
           watervol  = meanwater
           laketype  = 1
         ENDIF
       ELSE
         !no water in the lake, do nothing
         laketype = 0
       ENDIF

! 2) melt corresponding ice (from below: black ice, slush ice, snow), takin fractional ice cover into account
    IF(laketype.GT.0)THEN
      !available heat for melting (C * KG/M2 * 1000 * KJ/KG/C = J/M2)
      waterheat = (watertemp-genpar(m_liceTf)) * watervol * 1000. * cwater

      IF(waterheat.GT.0.)THEN

        !Try bottom melt only if there was not already complete meltout (breakupday==1)
        IF(breakupday.EQ.0)THEN
          !!melt the ice from below, in cm ice
          !bottommelt = min(frozenstate%lakeice(itype,i),waterheat/(L*dice)*mm2cm)
          !meltheat   = bottommelt * (L*dice) * cm2mm
          !meltwater  = bottommelt * dice *cm2mm

          !lake ice and snow mass, in mm water, per unit area of ice covered lake
          icewater = frozenstate%lakeice(itype,i)*dice*cm2mm*(1.-frozenstate%lakeicepor(itype,i)) + frozenstate%lakesnow(itype,i)

          !ice melt, in mm per unit area of ice-covered river
          ! - it is thus only the water below the ice which is interacting with the ice
          ! - the available heat is scaled with a "Meltefficiency" parameter
          meltwater = MIN(icewater,genpar(m_licewme)*waterheat/L)
          meltheat = meltwater * L

! 3) update the frozen states
          IF((icewater-meltwater).GT.0.)THEN
            !some ice remains, redice icemass by reducing fractional coverage
            frozenstate%lakeicecov(itype,i) = MIN(1.,MAX(0.,frozenstate%lakeicecov(itype,i)*(1-meltwater/icewater)))
          ELSE
            !complete melt of the lakeice
            frozenstate%lakeice(itype,i)=0.
            frozenstate%lakebice(itype,i)=0.
            frozenstate%lakeicepor(itype,i)=0.

            !add heat needed to melt the lakesnow to the meltheat
            !meltheat = meltheat + frozenstate%lakesnow(itype,i) * L

            !add snow to the meltwater
            !meltwater = meltwater + frozenstate%lakesnow(itype,i)

            !reset the snow states
            frozenstate%lakesnow(itype,i)=0.
            frozenstate%lakesnowage(itype,i)=0.

            !and ice cover area
            frozenstate%lakeicecov(itype,i) = 0.

            !set breakupflag to 1
            breakupday = 1
          ENDIF
        ELSE
          meltheat = 0.
          meltwater = 0.
        ENDIF
      ELSE
        meltheat = 0.
        meltwater = 0.
      ENDIF
! 4) use any remaining heat and the zero degree melt water to update the lake state

      !remove melt heat from heat content of the lake water
      waterheat = waterheat - meltheat

      !add any previous surface melt water to the meltwater
      IF(dlakewidt.GT.0)THEN
        meltwater = meltwater + dlakewidt
      ENDIF

      !temperature of water from remaining heat content
      newwatertemp=MAX(waterheat/(watervol * 1000. * cwater) + genpar(m_liceTf),genpar(m_liceTf))

      !dilute with the meltwater, which is at freezing point
      newwatertemp = MAX(genpar(m_liceTf),newwatertemp * (watervol - meltwater)/watervol)

      !weighted temperature, between icefree and icecovered water
      watertemp = oldicecov * newwatertemp + (1.-oldicecov)*watertemp

      !finally, assign update values to the real state variable
      SELECT CASE(laketype)

        CASE(1) !single layer without split
          IF(lakestate%water(itype,i).GT.0.)THEN
            lakestate%conc(i_t2,itype,i) = watertemp
          ELSE
            lakestate%conc(i_t2,itype,i) = 0.
          ENDIF
          lakestate%uppertemp(itype,i) = watertemp
          lakestate%lowertemp(itype,i) = watertemp

        CASE(2) !two-layer without split
          lakestate%uppertemp(itype,i) = watertemp
          meantemp = (lakestate%uppertemp(itype,i) * epidepth + lakestate%lowertemp(itype,i) * hypodepth)/ (meanwater * 0.001)
          IF(lakestate%water(itype,i).GT.0.)THEN
            lakestate%conc(i_t2,itype,i) = meantemp
          ELSE
            lakestate%conc(i_t2,itype,i) = 0.
          ENDIF

      END SELECT

    ENDIF !if laketype = 0, no water in lake -> do nothing

  END SUBROUTINE calculate_lakeice_lakewater_interaction

  !>Calculate lake T2 temperature processes
  !----------------------------------------------------------
  SUBROUTINE T2_processes_in_lake(i,itype,temp,swrad,lakesurft,lakearea,hypodepth,frozenstate,lakestate,freezeup,freezeuparea)

    USE MODVAR, ONLY: genpar, &
                      i_t2,                &
                      modeloption,         &
                      p_lakeriverice
    USE HYPEVARIABLES, ONLY: m_t2trlake,   &
                             m_upper2deep, &
                             m_liceTf,     &
                             m_tcflake,    &
                             m_scflake,    &
                             m_ccflake,    &
                             m_lcflake,    &
                             m_stbcorr1,  &
                             m_stbcorr2,  &
                             m_stbcorr3,  &
                             m_limt2exch

    !Argument declarations
    INTEGER,INTENT(IN) :: i             !<index of subbasin
    INTEGER,INTENT(IN) :: itype         !<index of lake type (ilake = 1, olake = 2)
    REAL,INTENT(IN)    :: temp          !<air temp
    REAL,INTENT(IN)    :: swrad         !<shortwave radiation, MJ/m2/day
    REAL,INTENT(INOUT) :: lakesurft(2)  !<water surface temperature
    REAL,INTENT(IN)    :: lakearea      !<lake area (m2)
    REAL,INTENT(IN)    :: hypodepth     !<hypolimnion depth (m)
    TYPE(snowicestatetype),INTENT(IN)  :: frozenstate   !<Snow and ice states
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state
    INTEGER, INTENT(OUT) :: freezeup    !<is water cooling below freezing piont (1 yes, 0 no)?
    REAL, INTENT(OUT)  :: freezeuparea  !<fraction of lake area with newice formation

    !Local variables
    LOGICAL epilimnion
    REAL meantemp, meanwater,epidepth
    REAL t2transfcorr
    REAL icefreefraction, freezeuparea2

    !0 Some initializations
    freezeup = 0
    epilimnion = .FALSE.
    freezeuparea = 0.
    freezeuparea2 = 0.

    !1 Lake-atmosphere T2 exchange

    !1.1 Seasonal correction of T2 exchange coefficient
    t2transfcorr = 1.  !Modify according to Johans suggestion below?

    !Set local variables for water (volume) and average temperature
    meanwater = lakestate%water(itype,i)
    IF(meanwater.GT.0.)THEN
      meantemp  = lakestate%conc(i_t2,itype,i)
    ELSE
      meantemp  = 0.
    ENDIF

    !1.3 Lake-Atmosphere T2 exchange

      IF(meanwater.GT.0.)THEN

        !Check lake depth, if thermal stratification
        IF(itype==2 .AND. hypodepth < meanwater*0.001 .AND. hypodepth>0.)THEN !why is this only possible for olakes?
          !!Two-layer olake, waterdepth > thermocline, olake
          epilimnion = .TRUE.
          epidepth = meanwater * 0.001 - hypodepth
          !->derive lake uppertemp(t) from meantemp(t, preliminary) and lowertemp(t-1)
          lakestate%uppertemp(itype,i) = (meantemp * meanwater * 0.001 - lakestate%lowertemp(itype,i) * hypodepth) / epidepth

          !Introducing fractional ice cover to get smoother transition over the freezing point
          icefreefraction = 1. - frozenstate%lakeicecov(itype,i)

          !->exchange with atmosphere - if there is some icefree fraction - updating meantemp(t) and uppertemp(t)
          IF(icefreefraction.GT.0.)THEN
            !temperature flow calculated from (temp-uppertemp), updating the mean temperature
            ! optional models  (will be reduced to one option after som initial testing for EHYPE3.0 and SHYPE2012)
            SELECT CASE(modeloption(p_lakeriverice))
            CASE(2) !new model based on Piccolroaz et al 2013, modified for fractional ice cover and newice formation
              CALL calculate_watersurface_heatbalance(temp,swrad,lakestate%uppertemp(itype,i),epidepth*lakearea,lakearea*icefreefraction, &
                                                      genpar(m_tcflake),genpar(m_scflake),genpar(m_ccflake),genpar(m_lcflake), &
                                                      genpar(m_limt2exch),freezeuparea,genpar(m_liceTf),genpar(m_stbcorr1),genpar(m_stbcorr2),genpar(m_stbcorr3))
            CASE(1) !original function of Johan/David, modified for fractional ice cover and newice formation
              CALL calculate_T2_transfer(temp,lakestate%uppertemp(itype,i),epidepth*lakearea,lakearea*icefreefraction, &
                                         genpar(m_t2trlake)*t2transfcorr,freezeuparea,genpar(m_liceTf)) !JS4
            END SELECT

            !->re-calculate meantemp
             meantemp = (lakestate%uppertemp(itype,i) * epidepth + lakestate%lowertemp(itype,i) * hypodepth) / (meanwater * 0.001)

            !Check freezeup conditions, indicated by relative freezeuparea
            IF(freezeuparea.GT.0.)THEN
              !freezup area is the fraction of previously unfrozen area (waterarea*icefreefraction), where new ice formation is triggered
              !re-scale to a fraction of the entire waterarea:
              freezeuparea = freezeuparea * icefreefraction
              freezeup = 1
            ENDIF
          ENDIF
        ELSE
          !Otherwise, single-layer, ilake

          !Introducing fractional ice cover to get smoother transition over the freezing point
          icefreefraction = 1. - frozenstate%lakeicecov(itype,i)
          !!->exchange with atmosphere, if no ice, update meantemp(t)
          IF(icefreefraction.GT.0.)THEN
            ! optional models  (will be reduced to one option after som initial testing for EHYPE3.0 and SHYPE2012)
            SELECT CASE(modeloption(p_lakeriverice))
            CASE(2) ! new model based on Piccolroaz et al 2013
              CALL calculate_watersurface_heatbalance(temp,swrad,meantemp,meanwater*lakearea*0.001,lakearea*icefreefraction,genpar(m_tcflake), &
                                                      genpar(m_scflake),genpar(m_ccflake),genpar(m_lcflake), &
                                                      genpar(m_limt2exch),freezeuparea,genpar(m_liceTf),genpar(m_stbcorr1),genpar(m_stbcorr2),genpar(m_stbcorr3))
            CASE(1)
              CALL calculate_T2_transfer(temp,meantemp,meanwater*lakearea*0.001,lakearea*icefreefraction,genpar(m_t2trlake)*t2transfcorr, &
                                         freezeuparea,genpar(m_liceTf))
            END SELECT

            !Check freezeup conditions, indicated by relative freezeuparea
            IF(freezeuparea.GT.0.)THEN
              !freezup area is the fraction of previously unfrozen area (waterarea*icefreefraction), where new ice formation is triggered
              !re-scale to a fraction of the entire waterarea:
              freezeuparea = freezeuparea * icefreefraction
              freezeup = 1
            ENDIF
          ENDIF

          lakestate%uppertemp(itype,i) = meantemp
          lakestate%lowertemp(itype,i) = meantemp
        ENDIF
      ELSE
        !no water in the lake, set temperature to 0
        meantemp  = 0.
        icefreefraction = 0.
      ENDIF

      !Finally, assign the updated meantemp to the lakestate%conc
      lakestate%conc(i_t2,itype,i)     = meantemp


    !2: Upper-lower lake T2 exchange

    IF(epilimnion)THEN
      !Autumn circulation
      IF(lakestate%uppertemp(itype,i)< lakestate%lowertemp(itype,i) .AND. lakestate%uppertemp(itype,i) > 3.95)THEN  !autumn circulation
        lakestate%lowertemp(itype,i) = meantemp
        lakestate%uppertemp(itype,i) = lakestate%lowertemp(itype,i)
      ELSE
        !Spring circulation
        IF(lakestate%uppertemp(itype,i)> lakestate%lowertemp(itype,i) .AND. lakestate%uppertemp(itype,i) < 3.95)THEN  !spring circulation
          lakestate%lowertemp(itype,i) = meantemp
          lakestate%uppertemp(itype,i) = lakestate%lowertemp(itype,i)
        ELSE
          !Startification; Heat transfer between upper and lower (new function)
          CALL calculate_T2_transfer_upper2lower(lakestate%uppertemp(itype,i),lakestate%lowertemp(itype,i),epidepth*lakearea, &
                                                hypodepth*lakearea,lakearea,genpar(m_upper2deep))
        ENDIF
      ENDIF
    ENDIF

    !Assign lake surface temperature if icefree conditions
    IF((icefreefraction-freezeuparea).GT.0.)lakesurft(itype) = lakestate%uppertemp(itype,i)

  END SUBROUTINE T2_processes_in_lake

  !>Calculate temperature(T2) "concentration" in lake/river precipitation
  !>due to ice presens
  !Changes the default T2 concentration, set for class
  !Modified to use fractional ice cover
  !----------------------------------------------------------
   SUBROUTINE add_T2_concentration_in_precipitation_on_water(prec,temp,snowfall,rainfall,watertemp,cprec,icecover)

    REAL, INTENT(IN)      :: prec       !<precipitation
    REAL, INTENT(IN)      :: temp       !<air temperature
    REAL, INTENT(IN)      :: snowfall   !<snow fall
    REAL, INTENT(IN)      :: rainfall   !<rain fall
    REAL, INTENT(IN)      :: watertemp  !<temperature of water
    REAL, INTENT(INOUT)   :: cprec      !<T2 concentration of precipitation
    REAL, INTENT(IN)      :: icecover   !<ice cover

    !This is now much more straight forward, using the fractional ice cover:
    ! Rainfall has always cprec = airtemp (but not lower than freezing point)
    ! Snowfall on the ice-free fraction has cprec = latentheat of freezing + sensible heat content
    ! Snowfall in the ice-covered fraction has cprec = laketemp
    IF(prec.GT.0)THEN
      !Rainfall temperature = max(0,air temp)
      cprec = rainfall * MAX(0.0,temp)

      !Snowfalltemp on ice   = watertemp (temporary), negative latent heat is added later when snow is melting in the ice routine
      !Snowfalltemp on water = airtemp + negative latent heat, taking into account diff spec.heat of ice and water
      cprec = cprec + snowfall * (watertemp * icecover + (MIN(temp,0.0)*2.1/4.2 - 335./4.2)*(1.-icecover))

      !Weighting by total precipitation
      cprec = cprec/prec
    ELSE
      cprec = 0.
    ENDIF

  END SUBROUTINE add_T2_concentration_in_precipitation_on_water

!>Subroutine to calculate growth of ice on lakes and rivers (only after freezeup has been identified)
! Developers: David Gustafsson(SMHI)
!
! Model is largely based on the review of thermodynamical ice models by Leppäranta (1993)
!
!    Ice growth: "freezing-degree-day"
!    Ice melt:   "positive degree-days" with constant 0.1-0.5 degC day/cm
!
! Snow on ice is considered, however, snowaccumulation and snowmelt is supposed to be calcluated outside of this routine:
!    the routine is calculating freezing of slush ice in case the snowmass is large enough to submerge the ice surface.
!
! Input/output to the model is icedepths, snowmass, snowdepth, and mass rate of lakewater and snowwater transformation from ice
!
! Model was calibrated with Swedish lake ice depth and river ice depth data for the North Hydrology project (Gustafsson et al, 2012)
!---------------------------------------
  SUBROUTINE calculate_icedepth(tsurf,iced,biced,icepor,snowm,snowd,Tair, &
                                dlakewidt,dsnowdt,ifreezeup,ibreakup,tf, &
                                kika,kexp,pm,ssmfT,ssmfR,bupo,sw,rm,qh)

    USE MODVAR, ONLY : conductwarning

    !Argument declarations
    REAL, INTENT(INOUT) :: tsurf       !<lake surface temperature, when the lake is ice and/or snowcovered, Tsurf is back calculated from ice growth, unless its melting, then its set to 0
    REAL, INTENT(INOUT) :: iced        !<ice depth, cm (black ice + snowice)
    REAL, INTENT(INOUT) :: biced       !<black ice, cm
    REAL, INTENT(INOUT) :: icepor      !<ice porosity, fraction
    REAL, INTENT(INOUT) :: snowm       !<snowmass, mm
    REAL, INTENT(INOUT) :: snowd       !<snowdepth, cm
    REAL, INTENT(IN)    :: Tair        !<air temperature, C
    REAL, INTENT(OUT)   :: dlakewidt   !<transformation of lake ice to lake water (positive direction from ice to water)
    REAL, INTENT(OUT)   :: dsnowdt     !<transformation of snow to lake ice       (positive direction from ice to snow)
    INTEGER, INTENT(IN) :: ifreezeup   !<freeze-up day flag (1=yes, 0=no)
    INTEGER, INTENT(OUT):: ibreakup    !<break-up day flag  (1=yes, 0=no)
    REAL, INTENT(IN)    :: tf          !<tf (~0.)  , freezing point temperature of the lake/river water, °C
    REAL, INTENT(IN)    :: kika        !<kika(~10.), ratio between thermal conductivity of ice and heat exchange coef in air
    REAL, INTENT(IN)    :: kexp        !<kiks(~10.), as above but for snow, actually dependent on snow density, but we use a fixed value
    REAL, INTENT(IN)    :: pm          !<pm (~0.5) , degree-day melt factor for ice, cm/°C
    REAL, INTENT(IN)    :: ssmfT       !<ssmfT (~0?), sub-surface melt fraction, fraction 0-1, air temperature driver melt
    REAL, INTENT(IN)    :: ssmfR       !<ssmfR (~0.9?), sub-surface melt fraction, fraction 0-1, radiation driven melt
    REAL, INTENT(IN)    :: bupo        !<bupo (~0.40?), breakup porosity, fraction 0-1
    REAL, INTENT(IN)    :: sw          !<sw,  incoming shortwave radiation, MJ/m2/day
    REAL, INTENT(IN)    :: rm          !<rm (?),  radiation melt factor for ice, [-] = fraction of incoming radiation flux absorbed for ice melt (at air temperatures above 0C)
    REAL, INTENT(IN)    :: qh          !<qh,  heat flow from water, MJ/m2/day ( W/m2 * 86400 s/day * 1e-6)

    !Local variables
    REAL :: slushd    !slush depth,     cm
    REAL :: siced     !snow ice depth, cm
    REAL :: dsnow     !snow density, g/cm3
    REAL :: S         !freezing degree days, Cday
    REAL :: M         !melting degree days, Cday
    REAL :: R         !melting radiation, MJ/m2/day
    REAL :: dHsicedt  !snowice growth, cm/day, potential
    REAL :: dHsicedt2 !snowice growth, cm/day, actual
    REAL :: dHicedt   !blackice growth, cm/day, actual
    REAL :: oldiced   !icedepth at start of calculaton, cm
    !Parameters, calculated in the code from the input parameters
    REAL :: ki       ! ki, thermal conductivity of ice, W/m/C, ki ~= 1.9 (see Leppäranta-93)
    REAL :: ai       ! a, degreeday factor, see Leppäranta(1993)~=3.3 if ki=1.9 W/m/C
    REAL :: ka       ! ka = ki/kika, heat exchange in air
    REAL :: ks       ! ks = ki*(rhosnow/rhoice)^ksexp, thermal conductivity in snow
    REAL :: kiks
    REAL :: dHicedtPorosity
    REAL :: ssmfloc  ! weighted sub-surface melt fraction for combined radiation and temperaure index ice melt
    REAL :: rmloc    ! radiation melt efficiency, cm/MJ/m2/day

    !Physical Constants
    REAL, PARAMETER :: L = 335.      !latent heat of freezing, J/g
    REAL, PARAMETER :: dice = 0.917  !density of ice, g/cm3
    REAL, PARAMETER :: mm2cm = 0.1, cm2mm = 10. !parameters for transformation from mm to cm

    !----------------------------------------------------------
    !Update on the temporary note on ice porosity (2018-10-16)
    ! By introducing ice melt as a function of radiation and air temperature index it becomes natural to assume
    !
    !    radiation driven melt = sub-surface melt
    !    temperature driven melt = surface melt
    !
    ! Thus:
    !  subsurface_melt = global_radiation * radiation_meltcoefficient
    !  surface_melt    = degreedays * temperature_meltcoefficient
    !
    ! The parameter ssmf (sub-surface melt fraction) is thus no longer needed.
    ! The remaining note on ice porosity stays intacts.
    !---------------------------------------------------------
    !Note (temporary) on the introduction of ice porosity
    !
    ! in reality ice melts by 3 main processes: 1) at the surface through surface heat fluxes, 2) internally by absorption of
    ! radiation over some depth below the ice surface, and 3) at the bottom by heat flow from the water.
    !
    ! the internal melt results in the formation of liquid water filled voids, that eventually will weaken the
    ! ice until it falls apart when the porosity exceeds some critical threshold
    !
    ! in the previous version of the model, ice melt through process 1) and 2) is calculated by positive degree days without consideration of radiation - and
    ! all melt is assumed to happen at the surface (so process 2 is actually neglected and process 1 is overestimated).
    ! Melt through process 3 is also calculated, but not by this subroutine.
    !
    ! to obtain a more realistic simulation of the ice breakup phase, we introduce a new parameter to split the ice melt estimated by positive degree days
    ! into surface melt and sub-surface melt (ricessmf and licessmf = river and lake ice sub-surface melt fraction, respectively):
    !
    !  surfacemelt    = degreedays * meltcoefficient * (1-ricessmf)
    !  subsurfacemelt = degreedays * meltcoefficient * ricessmf
    !
    ! We also introduce a new state variable for the ice porosity (rivericepor and lakeicepor) - for simplicity, we assume the ice pore space to be always filled with
    ! liquid water, and we consider this liquid water to be part of the lake (or river) water. Thus, the additional liquid water needed to completely fill the pore space
    ! created by melting a given volume of ice is assumed to flow in from the llake - and vice versa, when refreezing the pore space, the excess water is assumed to be
    ! pushed out into the lake - if the upward heat flow is enough to fill the entire pore space with ice, any additional heat loss is asssumed to create ice melt at the bottom of the
    ! ice- as usual. For simplificy we also assume the ice pore space to be evenly distributed over the ice depth (black ice + snow ice)
    !
    ! The sub-surface melt is first used to increase the pore space in the ice:
    !
    !   icepor_new = voidvol_cm / icedepth_cm = (icepor_old * icedepth_cm + subsurfacemelt_cm) / icedepth_cm
    !
    ! and then, the surface melt is used to reduce the ice depth of the remaining ice:
    !
    !   icedepth_new = icedepth_cm - surfacemelt_cm / (1-icepor_new)
    !
    ! If the updated ice porosity is exceeding some critical threshold, defined by new parameters river and lakeice breakup threshold (ricebupo and licebupo), the ice is assumed
    ! to fall apart, which is considered to breakupday

    !Conversion of some parameters
    ki = 2.2                ! standard value 2.2 for fresh water black ice
    ki = ki * 86400. / 100. ! (W/m/oC) -> (J/d/cm/C)
    ai  = (2*ki/dice/L)**0.5
    ka = ki/kika    ! kika and kexp can be calibration parameters if needed
    ! Radiation melt coefficient
    !    rmloc = rm * 1 / (L * dice * 100*100 * 1e-6)  = 1 / ( [J/g] * [g/cm3] * [cm2 / m2] * [MJ/J] = 1 / [MJ/m2/cm] =>
    !    cm/day = sw [MJ/m2/day] * rmloc [1 / [MJ/m2/cm]] = cm/day
    rmloc = rm * 100. / (L * dice)

    !Initialization of some variables
    dHicedt   = 0.
    dHsicedt  = 0.
    dHsicedt2 = 0.
    dlakewidt = 0.
    dsnowdt   = 0.
    oldiced = iced
    siced = iced - biced

    !If there is old ice or if freeze-up condition has been met, calculate ice growth and ice melt
    IF(iced.gt.0. .OR. ifreezeup.EQ.1)THEN ! ifreezeup eq. to tsurf < 0...

      !Freezing and melting degree days (actually, HYPE is using a threshold temperature for snow melt)
      S = AMAX1(0.,-Tair)   !freezing degree days
      M = AMAX1(0.,Tair)    !melting degree days
      R = AMAX1(0.,sw)      !radiation available for ice melt

      !Accumulation and Melt of snow on ice is treated outside of this function

      !Snow density
      IF(snowm.GT.0.0 .AND. snowd.GT.0.0)THEN
        dsnow = snowm * mm2cm / snowd
        ks = ki * (dsnow/dice)**kexp
      ELSE
        dsnow = 0.0
        ks = ki * (0.1/dice)**kexp
      ENDIF
      kiks = ki/ks

      !Ice growth
      IF(snowm*mm2cm.GT.iced *(1.-dice)*(1.-icepor))THEN
        !Submerged snow on ice, snowmass exceeds floating capacity of the ice
        !slush depth [cm] above ice surface (depends on snow
        !density, snow mass, and ice mass (assuming no capillary rise
        !in snow), limited by snow depth (check density if there is problem):
        slushd = (snowm*mm2cm - iced * (1.-dice)*(1.-icepor))/(dsnow/dice)
        IF(slushd.gt.snowd)THEN
          IF(conductwarning) WRITE(6,*) 'WARNING: slushdepth > snowdepth. slushdepth, snowdepth, dsnow:',slushd,snowd,dsnow
          slushd = snowd
        ENDIF

        ! Snow-ice growth (d(Hsi)/dt), see Leppäranta(1993), eq 21
        IF(Tair.LT.Tf)THEN
          ! height change, of the snow ice, limited by the slush depth
          dHsicedt  = ks * (Tf - Tair)/(snowd+kika)/(dice*L*(1.-dsnow/dice))
          dHsicedt2 = amin1(slushd,dHsicedt)  ! only valid for daily time steps

          ! update surface temperature
          tsurf = dHsicedt*(1.-dsnow/dice)*L*dice / ka + Tair

          !update snow and ice depths, snowmass, and ice porosity:
          snowd  = amax1(0.,snowd - dHsicedt2)          ! snow depth, cm
          snowm  = amax1(0.,snowm - dHsicedt2 * dsnow * cm2mm)  ! snowmass, mm

          siced  = amax1(0.,siced + dHsicedt2)          ! snow ice depth, cm
          slushd = amax1(0.,slushd - dHsicedt2)         ! slush depth, cm

          iced   = amax1(0.,biced + siced)             ! total ice depth, cm
          IF(iced.GT.0.)THEN
            icepor = amax1(0.,amin1(1.,icepor * oldiced / iced)) ! ice porosity, fraction
          ELSE
            icepor = 0.
          ENDIF

          !how much lake water (mm) and snow mass (mm) is transformed to snow-ice?
          ![mm per unit area ice covered lake]
          dlakewidt  = dlakewidt - dHsicedt2 * (1.-dsnow/dice) * cm2mm
          dsnowdt    = dsnowdt   - dHsicedt2 * dsnow * cm2mm

          ! if the potential snow-ice growth was larger than the
          ! slushdepth, it means that we have additional heat loss to
          ! freeze also the black ice, which could be used to calculate black ice
          ! growth at this point:

        ENDIF
      ELSE
        ! ICE SURFACE ABOVE WATER SURFACE, AND WE MAY ESTIMATE BLACK ICE GROWTH
        slushd = 0.
        ! (black) ice growth, including insulation of snow on ice (see Leppäranta(1983), dHdt = 0.5*a^2*S/(H + ki/ka + kiks * h)
        dHicedt = 0.5*ai**2 * S /(iced + kika + snowd*kiks)
        ! Reduction in ice growth by heat flow from water
        dHicedt = amax1(0.,dHicedt - qh *100. / (L*dice))  ! qh->dhicedt = (MJ/m2/day)  *1e6 (J/MJ) /  (J/g * g/cm3 * 10000 * cm2 / m2)  = (J * g  * cm3 * m2) / (m2 * day * J * g * cm2) = cm/day

        ! part of blackice growth used to refreeze porosity from previous melt periods
        dHicedtPorosity = amin1(dHicedt,amax1(0.,icepor*iced))

        ! update ice porosity
        IF((iced+dHicedt-dHicedtPorosity).GT.0.)THEN
          icepor = amax1(0.,icepor*iced - dHicedtPorosity)/(iced+dHicedt-dHicedtPorosity)
        ELSE
          icepor = 0.
        ENDIF

        ! update ice depth
        iced = iced + amax1(0.,dHicedt-dHicedtPorosity)

        ! update surface temperature
        tsurf = dHicedt * L * dice / ka + Tair

        ! we do the calculation for the total ice depth (then separate
        ! snow ice from clack ice)
        biced = iced - siced

        !how much lake water (mm) and snow mass (mm) is transformed to snow-ice?
        dlakewidt  = dlakewidt - dHicedt * dice * cm2mm

      ENDIF

      ! ICE MELT, simple degree day + radiation index
      !(if there is ice, if there is no snow, and if there is positive degree days or radiation available for internal melting)
      !(please note that negative degreedays were already used for ice growth
      IF((M.GT.0. .OR. R.GT.0.) .AND. iced.GT.0. .AND. snowd.LE.0.)THEN
        dHicedt  = - AMIN1(iced,M*pm + R*rmloc)    ! total ice melt by degreeday model and radiation index = degreedays * pm + radiation * rm
        IF(dHicedt.LT.0.)THEN
          ssmfloc = (ssmfR*R*rmloc + ssmfT*M*pm)/(M*pm + R*rmloc) ! fraction of sub-surface ice melt
        ELSE
          ssmfloc = 0.
        ENDIF

        icepor = AMAX1(0.,AMIN1(1.,icepor - amin1(1.,ssmfloc)*dHicedt/iced)) ! ice porosity = old_porosity + subsurfacemelt_cm/icedepth_cm
        iced   = AMAX1(0.,iced + dHicedt*amax1(0.,1.-ssmfloc))   ! total ice depth [cm]
        siced  = AMAX1(0.,siced + dHicedt*amax1(0.,1.-ssmfloc))  ! snow ice [cm], is melted before the black iace
        biced  = AMAX1(0.,iced-siced)                            ! black ice [cm]

        ! how much water is generated?
        dlakewidt  = dlakewidt - dHicedt * dice * cm2mm

        ! set surface temperature to 0
        tsurf = 0.0
      ENDIF

      ! BREAK UP DAY
      IF((iced.LE.0. .OR. icepor.GT.bupo) .AND. oldiced .GT. 0.)THEN
        ibreakup = 1
        iced = 0.
        snowd = 0.
        biced = 0.
        snowm=0.
        slushd=0.
        siced = 0.
        tsurf = 0.0
        icepor = 0.
      ENDIF
    ENDIF

  END SUBROUTINE calculate_icedepth

!>Subroutine to calculate transfer of heat from air to water
!---------------------------------------------------------------
  SUBROUTINE calculate_T2_transfer(airtemp,watertemp,watervol,waterarea,  &
                                   T2transfer,freezeuparea,freezingpoint)

    USE MODVAR, ONLY: cwater,seconds_per_timestep,realzero

    !Argument declaration
    REAL, INTENT(IN)    :: airtemp       !<air temperature (deg Celsius)
    REAL, INTENT(INOUT) :: watertemp     !<water temperature (deg Celsius)
    REAL, INTENT(IN)    :: watervol      !<surface water volume (m3 or mm)
    REAL, INTENT(IN)    :: waterarea     !<surface water area (m2)
    REAL, INTENT(IN)    :: T2transfer    !<heat transfer parmeter from air to water (J/m2/s/deg)
    REAL, INTENT(OUT)   :: freezeuparea  !fractional area were ice formation is trigered (fraction, 0-1)
    REAL, INTENT(IN)    :: freezingpoint !freezingpoint temperature, deg C

    !Local variable declarations
    REAL t2_transf              !T2 transfer
    REAL density
    REAL heatcapacity, thermcond

    density = 1000.
    heatcapacity = cwater * density * 1000.
    thermcond = T2transfer * seconds_per_timestep   !J/m2/deg/timestep
    freezeuparea = 0.

    IF(airtemp > watertemp)THEN
      t2_transf = MIN((airtemp - watertemp) * watervol * heatcapacity,(airtemp - watertemp)* waterarea * thermcond)
    ELSE
      t2_transf = MAX((airtemp - watertemp) * watervol * heatcapacity,(airtemp - watertemp)* waterarea * thermcond)
    ENDIF
    IF(watervol>realzero)THEN
      !evaluate ice formation conditions (new temperature<freezing point)
      IF((watertemp * watervol * heatcapacity + t2_transf) / (watervol * heatcapacity).LT.freezingpoint)THEN
        !estimate a freezup area (reduction in the open water surface area) so that the result of the surface heat balance is equal to the freezing point
        freezeuparea=  max(0.,min(1.,1. - (freezingpoint * (watervol * heatcapacity) - watertemp * watervol * heatcapacity)/t2_transf))
        watertemp = freezingpoint
      ELSE
        !calculate new temperature, water volume must be in m3!
        watertemp = (watertemp * watervol * heatcapacity + t2_transf) / (watervol * heatcapacity)
      ENDIF
    ENDIF

  END SUBROUTINE calculate_T2_transfer

!>Subroutine to calculate transfer of heat(temperature) between upper and lower layer in lakes
!---------------------------------------
  SUBROUTINE calculate_T2_transfer_upper2lower(uppertemp,lowertemp,uppervol,lowervol,waterarea,T2transfer)

    USE MODVAR, ONLY: cwater,seconds_per_timestep,realzero

    !Argument declaration
    REAL, INTENT(INOUT) :: uppertemp     !<upper water temperature (deg Celsius)
    REAL, INTENT(INOUT) :: lowertemp     !<lower water temperature (deg Celsius)
    REAL, INTENT(IN)    :: uppervol      !<upper layer water volume (m3)
    REAL, INTENT(IN)    :: lowervol      !<lower layer water volume (m3)
    REAL, INTENT(IN)    :: waterarea     !<surface water area (m2)
    REAL, INTENT(IN)    :: T2transfer    !<heat transfer parmeter from air to water (J/m2/s/deg)

    !Local variable declarations
    REAL t2_transf              !T2 transfer
    REAL density
    REAL heatcapacity, thermcond
    REAL equiltemp

    density = 1000.
    heatcapacity = cwater * density * 1000.
    thermcond = T2transfer * seconds_per_timestep   !J/m2/deg/timestep

    !Calculate equilibrium temperature, when heat is evenly distributed
    IF((uppervol+lowervol).GT.realzero)THEN
      equiltemp = (uppertemp*uppervol + lowertemp*lowervol)/(uppervol+lowervol)

      !calculate heatflow and update temperatures, depending on initial gradient:
      IF(uppertemp > lowertemp)THEN
        !heat flow from upper to lower
        t2_transf = (uppertemp - lowertemp)* waterarea * thermcond
        !Upper and lower temperatures, limited by equilibrium temperature
        uppertemp = MAX(equiltemp,(uppertemp * uppervol * heatcapacity - t2_transf) / (uppervol * heatcapacity))
        lowertemp = MIN(equiltemp,(lowertemp * lowervol * heatcapacity + t2_transf) / (lowervol * heatcapacity))
      ELSE
        !heat flow from lower to upper
        t2_transf = (lowertemp - uppertemp)* waterarea * thermcond
        !Upper and lower temperatures, limited by equilibrium temperature
        uppertemp = MIN(equiltemp,(uppertemp * uppervol * heatcapacity + t2_transf) / (uppervol * heatcapacity))
        lowertemp = MAX(equiltemp,(lowertemp * lowervol * heatcapacity - t2_transf) / (lowervol * heatcapacity))
      ENDIF
    ELSE
      uppertemp=0.
      lowertemp=0.
    ENDIF

  END SUBROUTINE calculate_T2_transfer_upper2lower

!>\brief Subroutine to calculate transfer of heat from air to water including a solar radiation term and a residual term.
!>
!>The routine is based on the model sugested by Piccolroaz et al (2013), with modifications
!>to use real (or estimated) shortwave radiation.
!>Partly ice covered situations can be taken into account by reducing the input waterarea
!>If the heat balance is negative enough to lower temperature below freezing, a reduction in
!>the surface area is estimated, which shows at how large area the ice is forming.
!
!TODO: make T2 subroutines work for other timestep than day
!---------------------------------------
  SUBROUTINE calculate_watersurface_heatbalance(airtemp,swrad,watertemp,watervol, &
                  waterarea,tempcoef,radcoef,constcoef,lincoef,limt2exch, &
                  freezeuparea,freezingpoint,stabpar1,stabpar2,stabpar3)

  USE MODVAR, ONLY: cwater,seconds_per_timestep,realzero

    !Argument declaration
    REAL, INTENT(IN)    :: airtemp       !<air temperature (deg Celsius)
    REAL, INTENT(IN)    :: swrad         !<shortwave radiation (MJ/m2/day)
    REAL, INTENT(INOUT) :: watertemp     !<water temperature (deg Celsius)
    REAL, INTENT(IN)    :: watervol      !<water volume (m3)
    REAL, INTENT(IN)    :: waterarea     !<water surface area (m2)
    REAL, INTENT(IN)    :: tempcoef      !<heat transfer parameter from air to water (J/m2/s/deg)
    REAL, INTENT(IN)    :: radcoef       !<heat transfer parameter from radiation to water (fraction, 0-1)
    REAL, INTENT(IN)    :: constcoef     !<heat transfer parameter, constant residual term (J/m2/s)
    REAL, INTENT(IN)    :: lincoef       !<heat transfer parameter, linear residualterm (J/m2/s/deg)
    REAL, INTENT(IN)    :: limt2exch     !<heat transfer parameter, limit depth for only temperature exchange (m)
    REAL, INTENT(OUT)   :: freezeuparea  !<fractional area were ice formation is trigered (fraction, 0-1)
    REAL, INTENT(IN)    :: freezingpoint !<freezingpoint temperature, deg C
    REAL, INTENT(IN)    :: stabpar1      !<Stability parameter, affects both heating and cooling. No correction if set to zero
    REAL, INTENT(IN)    :: stabpar2      !<Stability parameter, affects cooling. No correction if set to zero
    REAL, INTENT(IN)    :: stabpar3      !<Stability parameter, affects heating. No correction if set to zero

    !Local variable declarations
    REAL netheat                  !Net heat flux to the water (J/timestep)
    REAL density                  !Water density (kg/m3)
    REAL heatcapacity             !heat capacity of water (J/m3/deg)
    REAL tempdiff                 !Temperature difference
    REAL stabfunction             !Stability correction function

    REAL, PARAMETER :: seconds_per_day = 86400.

    !> \b Algorithm \n
    density      = 1000.                    ! kg/m3, density of water
    heatcapacity = cwater * density * 1000. ! J/m3/deg  [kJ/kg/deg * kg/m3 * 1/k]
    freezeuparea = 0.

    IF(watervol/waterarea>limt2exch)THEN
      IF(watervol>realzero)THEN   !make calculation only if the water has a volume
        netheat = 0.   !initialize the net heat flux, J/timestep

        !>Calculate stability correction for heat exchange between air and water
        tempdiff = airtemp - watertemp
        IF(tempdiff>0.) THEN
          stabfunction = 1./(1. + stabpar1 * tempdiff)**stabpar3
        ELSE
          stabfunction = 1./(1. - stabpar1 * tempdiff)**(-stabpar2)
        ENDIF
        !>Add the air temperature term
        IF(airtemp > watertemp)THEN
          netheat = netheat + MIN((airtemp - watertemp) * watervol * heatcapacity, stabfunction * (airtemp - watertemp)* waterarea * tempcoef * seconds_per_timestep) !J/timestep
        ELSE
          netheat = netheat + MAX((airtemp - watertemp) * watervol * heatcapacity, stabfunction * (airtemp - watertemp)* waterarea * tempcoef * seconds_per_timestep) !J/timestep
        ENDIF
        !>Add the radiation term, MJ/m2/day => J/m2/s and then multiplied with timestep in s.
        netheat = netheat + 1.E6 * swrad /seconds_per_day * waterarea * radcoef * seconds_per_timestep
        !>Add the residual term, same units as temperature equation
        netheat = netheat + (watertemp*lincoef + constcoef) * waterarea * tempcoef * seconds_per_timestep

        !>Evaluate ice formation conditions (new temperature<freezing point) and calculate new water temperature
        IF((watertemp * watervol * heatcapacity + netheat) / (watervol * heatcapacity).LT.freezingpoint)THEN
          !estimate a freezup area (reduction in the open water surface area) so that the result of the surface heat balance is equal to the freezing point
          freezeuparea = MAX(0.,MIN(1.,1. - (freezingpoint * (watervol * heatcapacity) - watertemp * watervol * heatcapacity)/netheat))
          watertemp = freezingpoint
        ELSE
          !calculate new temperature, water volume must be in m3!
          watertemp = (watertemp * watervol * heatcapacity + netheat) / (watervol * heatcapacity)
        ENDIF
      ENDIF
    ELSE
      !Use only temperature heat exchange for shallow waters
      CALL calculate_T2_transfer(airtemp,watertemp,watervol,waterarea,tempcoef,freezeuparea,freezingpoint)
    ENDIF

  END SUBROUTINE calculate_watersurface_heatbalance

!>\brief Calculate water level and flooded area from water volume for a sloping floodplain
!-----------------------------------------------------------------------------------------
SUBROUTINE calculate_floodplain_waterlevel(vol,amax,ymax,y,a)

  !Argument declarations
  REAL, INTENT(IN)  :: vol  !<water volume in floodplain [m3]
  REAL, INTENT(IN)  :: amax !<area at maximum areal extent [m2]
  REAL, INTENT(IN)  :: ymax !<water level at maximum areal extent [m]
  REAL, INTENT(OUT) :: y    !<calculated water level at volume volm3 [m]
  REAL, INTENT(OUT) :: a    !<calculated area [m2]

  !Local variables
  REAL volmax             !water volume when a=amax and y=ymax [m3]

  !volume at maximum extent
  volmax    = amax * ymax * 0.5

  !Calculate water depth and area
  IF(vol <= volmax)THEN
    !case 1: water volume smaller than volume at maximum extent
    y = SQRT(vol * ymax * 2. / amax )
    a = y * amax / ymax
  ELSE
    !case 2: water volume larger than volume at maximum extent
    y = ymax + (vol-volmax)/amax
    a = amax
  ENDIF

END SUBROUTINE calculate_floodplain_waterlevel

!>\brief Calculate water volume and flooded area from water level for a sloping floodplain
!-----------------------------------------------------------------------------------------
SUBROUTINE calculate_floodplain_volume(y,amax,ymax,vol,a)

  !Argument declarations
  REAL, INTENT(IN)  :: y    !<water level [m]
  REAL, INTENT(IN)  :: ymax !<water level at maximum areal extent [m]
  REAL, INTENT(IN)  :: amax !<area at maximum areal extent [m2]
  REAL, INTENT(OUT) :: vol  !<water volume in floodplain [m3]
  REAL, INTENT(OUT) :: a    !<calculated area [m2]

  !Calculate water volume and area
  IF(y <= ymax)THEN
    !case 1: water level below level at maximum extent
    vol = y*y*amax/(ymax*2)
    a = y * amax / ymax
  ELSE
    !case 2: water level above level at maximum extent
    vol = amax * ymax * 0.5 !volume at ymax
    vol = vol + (y-ymax)*amax !adding volume in water above ymax
    a = amax
  ENDIF

END SUBROUTINE calculate_floodplain_volume


!>Calculate equilibrium water level in river(or lake) and flooded area
!>for a sloping floodplain with maximum extent amax and corresponding level ymax.
!>The equilibrium level is given in the reference system for the contributing water storage.
!-----------------------------------------------------------------------------------------------------
SUBROUTINE calculate_floodplain_equilibriumlevel(volp,volr,flr,flp,ar,amax,ymax,yeq,r2p)

  !Argument declarations
  REAL, INTENT(IN)  :: volp   !<water volume in floodplain [m3]
  REAL, INTENT(IN)  :: volr   !<water volume in river (or lake) [m3]
  REAL, INTENT(IN)  :: flr    !<flooding level for the river (or lake) [m]
  REAL, INTENT(IN)  :: flp    !<flooding level for the floodplain [m]
  REAL, INTENT(IN)  :: ar     !<area of the river (or lake) [m2]
  REAL, INTENT(IN)  :: amax   !<area at maximum areal extent [m2]
  REAL, INTENT(IN)  :: ymax   !<water level at maximum areal extent [m]
  REAL, INTENT(OUT) :: yeq    !<equilibrium water level [m]
  INTEGER, INTENT(OUT) :: r2p !<flow direction flag, 1=river2plain, 2=plain2river,0=no flow

  !Local variables
  REAL :: voltot ! [m3] total volume to distribute
  REAL :: yr
  REAL :: yp
  REAL :: ap

  !water level in river (or lake)
  yr = volr/ar

  !water level in floodplain
  CALL calculate_floodplain_waterlevel(volp,amax,ymax,yp,ap)

  !make sure at least one water surface is above its flooding level, otherwise nothing to do
  IF(yr<=flr .AND. yp<= flp)THEN
    !Nothing to do
    yeq = -9999
    r2p = 0
    RETURN
  ELSE !Continue

    !determine which water surface is higher than the other in a common reference system (the floodplain threshold=0)
    IF((yr-flr)>=(yp-flp))THEN
      r2p = 1
    ELSE
      r2p = 2
    ENDIF

    !total water volume to distribute on river (or lake) and floodplain
    voltot = volp + volr

    !Exclude or add river/lake water volume below the lowest floodplain level, in other words:
    ! modify the system into a solvable problem, where the river and floodplain bottom are
    ! at the same level, i.e. the lowest floodplain level.
    voltot = voltot - (flr-flp)*ar

    !If the remaining volume is zero or negative, it means that the equilibriuim level is
    ! below the lowest common level, thus it is set to the flooding level of the water
    ! storage with the highest current surface (river or plain)
    IF(voltot<=0.)THEN
      !Select appropriate flooding level as equilibrium level, and return
      SELECT CASE(r2p)
        CASE(1) !river to plain
          yeq = flr
        CASE(2) !plain to river
          yeq = flp
        CASE DEFAULT
      ENDSELECT
      RETURN
    ELSE !continue

      !solve second degree equation for the equilibrium water level - with regard to the common bottom level (i.e. flp)
      yeq = ( -ar + sqrt(ar*ar + 2. * voltot * (amax / ymax)) ) / (amax / ymax)
      IF(yeq>yr-(flr-flp) .AND. yeq>yp)THEN   !Check for numerical problems; yeq>the highest water level
        WRITE(6,*) 'Numerical problem calculating floodplain equilibrium level. Setting no flow' !,yeq,yr-(flr-flp),yp
        yeq = -9999
        r2p = 0
        RETURN
      ENDIF

      !Check if above equation is valid (level on floodplain sloping bottom part), otherwise use alternative equation
      IF(yeq>ymax) yeq = (voltot + amax * ymax / 2.) / (ar + amax)

      !adjust the equilibrium level to threshold level as reference, limit to above 0
      yeq = MAX(0.,yeq - flp)

      !finally, adjust equilibrium level to appropriate reference system, depending on flow direction:
      SELECT CASE(r2p)
        CASE(1) !river to plain
          yeq = flr+yeq
        CASE(2) !plain to river
          yeq = flp+yeq
        CASE DEFAULT
      ENDSELECT
    ENDIF
  ENDIF
  RETURN

END SUBROUTINE calculate_floodplain_equilibriumlevel

!>\brief Calculate equilibrium water level between two floodplains
!>with sloping floodplain with maximum extent amax and corresponding level ymax.
!------------------------------------------------------------------------------
SUBROUTINE calculate_two_floodplain_equilibriumlevel_dp(volp,wl1,wl2,amax,ymax,href,hwleq,flowdirection)

  USE MODVAR, ONLY : missing_value

  !Argument declarations
  REAL, INTENT(IN)  :: volp(2)  !<water volume in floodplain [m3]
  REAL, INTENT(IN)  :: wl1      !<water level for floodplain 1 [m]
  REAL, INTENT(IN)  :: wl2      !<water level for floodplain 2 [m]
  REAL, INTENT(IN)  :: amax(2)  !<area at maximum areal extent [m2]
  REAL, INTENT(IN)  :: ymax(2)  !<water level at maximum areal extent [m]
  REAL, INTENT(IN)  :: href(2)  !<reference height of floodplain bottom [m]
  REAL, INTENT(OUT) :: hwleq    !<equilibrium water level (in href system) [m]
  INTEGER, INTENT(OUT) :: flowdirection !<flow direction flag, 1=fp1tofp2, 2=fp2tofp1,0=no flow

  !Local variables
  INTEGER iabove  !current floodplain water (1 or 2)
  INTEGER iwater,iwater2  !current floodplain water (1 or 2)
  INTEGER :: nhlow   !index of common lowest level [m]
  REAL :: hrefdelta     !difference in refernce systems [m]
  REAL :: hlow   !common lowest level [m]
  REAL hlocal(2)   !local reference hight (for numerical uncertainty reduction)
  REAL :: hhigh(2) !water level at maximum extent in href system [m]
  REAL :: vol0,vol1,vol2,vol4,vol5 !volumes
  REAL a
  DOUBLE PRECISION :: hwlini(2)   !water level in href system [m]
  DOUBLE PRECISION :: voltot !total volume left to distribute [m3]
  DOUBLE PRECISION :: vol0_dp, vol3_dp !volumes
  DOUBLE PRECISION :: volp_dp(2) !volumes of floodplains [m3]
  DOUBLE PRECISION :: hweq_dp !equlibrium water level [m]
  DOUBLE PRECISION :: amax_dp(2)  !area at maximum areal extent [m2]
  DOUBLE PRECISION :: ymax_dp(2)  !water level at maximum areal extent [m]
  DOUBLE PRECISION :: href_dp(2)  !local reference height of floodplain bottom [m]

    !Make sure at least one water surface is above the common lowest level
    hrefdelta = MINVAL(href)
    hlocal = href - hrefdelta  !local reference heights
    hlow = MAXVAL(hlocal)
    hwlini(1) = DBLE(wl1 + hlocal(1))
    hwlini(2) = DBLE(wl2 + hlocal(2))
    IF(hwlini(1)==hwlini(2))THEN
      flowdirection = 0   !No interflow
      hwleq = missing_value
      !WRITE(6,*) 'New case equal water level'
      RETURN
    ENDIF
    IF(hwlini(1)<=hlow .AND. hwlini(2)<=hlow)THEN
      flowdirection = 0   !No interflow
      hwleq = missing_value
      RETURN
    ENDIF

    !Determine which water surface is higher than the other in a common reference system (needed?)
    flowdirection = get_index_of_highest(wl1 + hlocal(1),wl2 + hlocal(2))
    !Should I remae all variables to high and low and calculate with same equations or have subroutines maybe

    !Calculate total water volume to distribute and water level of distributed water (which is none yet)
    voltot = DBLE(volp(1) + volp(2))

    !Add water volume below the lowest common level
    nhlow = get_matching_index(hlocal(1),hlocal(2),hlow)
    IF(nhlow==1)THEN
      iwater = 2
    ELSEIF(nhlow==2)THEN
      iwater = 1
    ENDIF
    CALL calculate_floodplain_volume(hlow-hlocal(iwater),amax(iwater),ymax(iwater),vol0,a)
    vol0_dp = DBLE(vol0)
    IF(vol0_dp>voltot)THEN
      !WRITE(6,*) 'No equlibrium floodlevel. To little water.'
      hwleq = missing_value
      RETURN
    ELSEIF(vol0_dp==voltot)THEN
      !WRITE(6,*) 'Equlibrium floodlevel is equal to lowest common level.'
      hwleq = hlow + hrefdelta
      RETURN
    ENDIF
    voltot = voltot - vol0_dp

    IF(voltot<0.D0)THEN
      WRITE(6,*) ' ERROR:floodplain code 1'
      STOP 1
    ENDIF

    !I need to calculate the equilibrium level
    volp_dp = DBLE(volp)
    amax_dp = DBLE(amax)
    ymax_dp = DBLE(ymax)
    href_dp = DBLE(hlocal)
    !Check for first threshold where the volume-curve change
    hhigh = hlocal + ymax
    iwater2 = get_index_of_lowest(hhigh(1),hhigh(2))
    IF(hlow>=hhigh(iwater2))THEN
      !Lowest lying water has already been filled to maximum areal extent
      vol1=0.;vol2=0.
      IF(iwater==1) vol1=vol0
      IF(iwater==2) vol2=vol0
    ELSE
      !Calculate volume up to this threshold of both floodplains
      CALL calculate_floodplain_volume(hhigh(iwater2)-hlocal(1),amax(1),ymax(1),vol1,a)
      CALL calculate_floodplain_volume(hhigh(iwater2)-hlocal(2),amax(2),ymax(2),vol2,a)
      vol3_dp = DBLE(vol1 + vol2) - vol0_dp
      IF(voltot<vol3_dp)THEN
        !Calculate equilibrium water level
        CALL calculate_equilibrium_floodplain_level_eq1dp(volp_dp,amax_dp,ymax_dp,href_dp,hweq_dp)
        IF(hweq_dp>hwlini(1).AND.hweq_dp>hwlini(2))THEN
          WRITE(6,*) 'Numerical problem calculating floodplain equilibrium level eq 1. Setting no flow'
          flowdirection = 0   !No interflow
        ENDIF
        hwleq = REAL(hweq_dp) + hrefdelta
        RETURN
      ELSEIF(voltot==vol3_dp)THEN
        hwleq = hhigh(iwater2) + hrefdelta
        RETURN
      ELSE
        voltot = voltot - vol3_dp
      ENDIF
    ENDIF

    IF(voltot<0.D0)THEN
      WRITE(6,*) ' ERROR:floodplain code 2'
      STOP 1
    ENDIF

    !Check for second threshold where the volume-curve change
    iwater = get_index_of_highest(hhigh(1),hhigh(2))

    !Calculate volume up to this threshold of both floodplains
    CALL calculate_floodplain_volume(hhigh(iwater)-hlocal(1),amax(1),ymax(1),vol4,a)
    CALL calculate_floodplain_volume(hhigh(iwater)-hlocal(2),amax(2),ymax(2),vol5,a)
    vol3_dp = DBLE(vol4 + vol5 - vol1 - vol2)
    IF(vol3_dp>voltot)THEN
      !Find first threshold where the volume-curve change, this is passed
      iabove = get_index_of_lowest(hhigh(1),hhigh(2))
      CALL calculate_equilibrium_floodplain_level_eq2dp(flowdirection,iabove,iwater,volp_dp,hwlini,amax_dp,ymax_dp,href_dp,hweq_dp)
      IF(hweq_dp>hwlini(1).AND.hweq_dp>hwlini(2))THEN
        WRITE(6,*) 'Numerical problem calculating floodplain equilibrium level eq 2. Setting no flow'
        flowdirection = 0   !No interflow
      ENDIF
      hwleq = REAL(hweq_dp) + hrefdelta
      RETURN
    ELSEIF(vol3_dp==voltot)THEN
      !Find first threshold where the volume-curve change, this is it
      iabove = get_index_of_lowest(hhigh(1),hhigh(2))
      hwleq = hhigh(iabove) + hrefdelta
      RETURN
    ELSE
      voltot = voltot - vol3_dp
    ENDIF

    IF(voltot<0.D0)THEN
      WRITE(6,*) ' ERROR:floodplain code 3'
      STOP 1
    ENDIF

    !Calculate equilibrium water level for water level above both floodplains' highest extent of area
    CALL calculate_equilibrium_floodplain_level_eq3dp(volp_dp,amax_dp,ymax_dp,href_dp,hweq_dp)
    IF(hweq_dp>hwlini(1).AND.hweq_dp>hwlini(2))THEN
      WRITE(6,*) 'Numerical problem calculating floodplain equilibrium level eq 3. Setting no flow'
      flowdirection = 0   !No interflow
    ENDIF
    hwleq = REAL(hweq_dp) + hrefdelta
    RETURN

  END SUBROUTINE calculate_two_floodplain_equilibriumlevel_dp

  !>Calculate equilibrium water level in floodplains for sloping floodplain with
  !>maximum extent amax and corresponding level ymax. The water level is below both ymax.
  !>The equilibrium level is given in the reference system.
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_equilibrium_floodplain_level_eq1dp(volume,amax,ymax,href,hwleq)

    !Argument declarations
    DOUBLE PRECISION, INTENT(IN)  :: volume(2)  !<water volume in floodplain [m3]
    DOUBLE PRECISION, INTENT(IN)  :: amax(2)  !<area at maximum areal extent [m2]
    DOUBLE PRECISION, INTENT(IN)  :: ymax(2)  !<water level at maximum areal extent [m]
    DOUBLE PRECISION, INTENT(IN)  :: href(2)  !<reference height of floodplain bottom [m]
    DOUBLE PRECISION, INTENT(OUT) :: hwleq    !<equilibrium water level (in href system) [m]

    !Local variables
    DOUBLE PRECISION hwleq1,hwleq2
    DOUBLE PRECISION help1,help2,hdiff2

    !Calculate help variables
    help1 = ymax(2)*amax(1) + ymax(1)*amax(2)
    help2 = ymax(2)*amax(1)*href(1) + ymax(1)*amax(2)*href(2)
    hdiff2 = (href(1)-href(2))**2

    !Calculate equlibrium water level
    hwleq1 = help2/help1 + SQRT(ymax(1)*ymax(2)*(2*(volume(1)+volume(2))*help1-amax(1)*amax(2)*hdiff2))/help1
    hwleq2 = help2/help1 - SQRT(ymax(1)*ymax(2)*(2*(volume(1)+volume(2))*help1-amax(1)*amax(2)*hdiff2))/help1
    hwleq = hwleq1

    !Check equilibrium water level
    IF(0.<hwleq-href(1) .AND. &
       hwleq-href(1)<=ymax(1) .AND. &
       0.<hwleq-href(2) .AND. &
       hwleq-href(2)<=ymax(2))THEN
    ELSE
      !Test the other
      hwleq = hwleq2
      IF(0.<hwleq-href(1) .AND. &
       hwleq-href(1)<=ymax(1) .AND. &
       0.<hwleq-href(2) .AND. &
       hwleq-href(2)<=ymax(2))THEN
      ELSE
        WRITE(6,*) 'ERROR: calculating floodplain equilibrium water level _eq1',hwleq1,hwleq2
      ENDIF
    ENDIF

  END SUBROUTINE calculate_equilibrium_floodplain_level_eq1dp

  !>Calculate equilibrium water level in floodplains for sloping floodplain with
  !>maximum extent amax and corresponding level ymax. The water level is above and
  !>below ymax for the different flood plains.
  !>The equilibrium level is given in the reference system.
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_equilibrium_floodplain_level_eq2dp(ifrom,iabove,ibelow,volume,level,amax,ymax,href,hwleq)

    !Argument declarations
    INTEGER, INTENT(IN) :: ifrom !<index of floodplain which has the highest water level, from which flow should leave, flowdirection
    INTEGER, INTENT(IN) :: iabove !<index of floodplain which reach its maximum extent
    INTEGER, INTENT(IN) :: ibelow !<index of floodplain which not reach its maximum extent
    DOUBLE PRECISION, INTENT(IN)  :: volume(2)  !<water volume in floodplain [m3]
    DOUBLE PRECISION, INTENT(IN)  :: level(2)  !<water level in ref system [m]
    DOUBLE PRECISION, INTENT(IN)  :: amax(2)  !<area at maximum areal extent [m2]
    DOUBLE PRECISION, INTENT(IN)  :: ymax(2)  !<water level at maximum areal extent [m] (not in href system)
    DOUBLE PRECISION, INTENT(IN)  :: href(2)  !<reference height of floodplain bottom [m]
    DOUBLE PRECISION, INTENT(OUT) :: hwleq    !<equilibrium water level (in href system) [m]

    !Local variables
    DOUBLE PRECISION hwleq1
    DOUBLE PRECISION help1,help2
    DOUBLE PRECISION fillup,pourout
    INTEGER ito

    IF(ifrom==iabove)THEN
      help1 = ymax(ibelow)/amax(ibelow)

      !Calculate equlibrium water level
      hwleq1 = -amax(iabove)*help1 + SQRT((help1*amax(iabove))**2+2.*help1*amax(iabove)*(level(iabove)-href(ibelow))+(level(ibelow)-href(ibelow))**2)
      hwleq = hwleq1 + href(ibelow)

      !Check equilibrium water level
      ito = MODULO(ifrom,2)+1
      IF(hwleq>=href(iabove)+ymax(iabove) .AND. &
         hwleq<=href(ibelow)+ymax(ibelow))THEN
        !hwleq in correct range
        IF(hwleq>level(1).AND.hwleq>level(2).AND.level(iabove)>level(ibelow))THEN
          !Try fill up ibelow to iabove, approximative hwleq
          fillup = (level(iabove)**2 - level(ibelow)**2)*amax(ibelow)/ymax(ibelow)/2
          pourout = fillup/amax(iabove)
          IF(pourout<0.001) hwleq = level(iabove)-pourout
        ENDIF
      ELSE
        !The equation is not correct. The equilibrium level is above/below both knees, No checked that on volume before entering.
        !I don't think this can happen.
        WRITE(6,*) 'ERROR: calculating floodplain equilibrium water level _eq2 1',hwleq1
      ENDIF
    ELSEIF(ifrom==ibelow)THEN
      help1 = ymax(ibelow)/amax(ibelow)
      help2 = 1./help1
      hwleq1 = -amax(iabove)*help1 + SQRT(2.*help1*(help1*0.5*amax(iabove)**2 &
               -(href(ibelow)-href(iabove))*amax(iabove)+ymax(iabove)*amax(iabove)*0.5 &
               +volume(ibelow)+volume(iabove)))
      hwleq = hwleq1 + href(ibelow)
      !Check equilibrium water level
      IF(hwleq>=href(iabove)+ymax(iabove) .AND. &
        hwleq<=href(ibelow)+ymax(ibelow))THEN
        !WRITE(6,*) 'OK: calculating floodplain equilibrium water level _eq2 2',hwleq1
      ELSE
        WRITE(6,*) 'ERROR: calculating floodplain equilibrium water level _eq2 2',hwleq1
      ENDIF
      !Here is several cases with different calculation of volumes. The current water level of ibelow can be both above or below the knee.
      !In addition the current water level of iabove can be both above or below the knee. -> four cases
      !IF(level(ibelow)>href(ibelow) + ymax(ibelow))THEN
      !  IF(level(iabove)>href(iabove)+ymax(iabove))THEN
      !    !Case 1. Both current water levels above knees
      !  ELSE
      !    !Case 2. Current water level of iabove is below the knee
      !  ENDIF
      !ELSE
      !  IF(level(iabove)>href(iabove)+ymax(iabove))THEN
      !    !Case 3. Current water level of iabove is above the knee
      !  ELSE
      !    !Case 4. Both current water levels below knees
      !  ENDIF
      !ENDIF
    ENDIF

  END SUBROUTINE calculate_equilibrium_floodplain_level_eq2dp

  !>Calculate equilibrium water level in floodplains for sloping floodplain with
  !>maximum extent amax and corresponding level ymax. The water level is above both ymax.
  !>The equilibrium level is given in the reference system.
  !------------------------------------------------------------------------------
  SUBROUTINE calculate_equilibrium_floodplain_level_eq3dp(volume,amax,ymax,href,hwleq)

    !Argument declarations
    DOUBLE PRECISION, INTENT(IN)  :: volume(2)  !<water volume in floodplain [m3]
    DOUBLE PRECISION, INTENT(IN)  :: amax(2)  !<area at maximum areal extent [m2]
    DOUBLE PRECISION, INTENT(IN)  :: ymax(2)  !<water level at maximum areal extent [m]
    DOUBLE PRECISION, INTENT(IN)  :: href(2)  !<reference height of floodplain bottom [m]
    DOUBLE PRECISION, INTENT(OUT) :: hwleq    !<equilibrium water level (in href system) [m]

    hwleq = (volume(1)+volume(2)+amax(1)*(0.5*ymax(1)+href(1))+amax(2)*(0.5*ymax(2)+href(2)))/(amax(1)+amax(2))
    IF(hwleq<ymax(1)+href(1) .OR. hwleq<ymax(2)+href(2))THEN
      WRITE(6,*) 'ERROR: calculating floodplain equlibrium water level _eq3',hwleq
    ENDIF

  END SUBROUTINE calculate_equilibrium_floodplain_level_eq3dp

  !>\brief Find highest value of two
  !-------------------------------------------------------------
  INTEGER FUNCTION get_index_of_highest(value1,value2)

    !Argument declarations
    REAL,INTENT(IN)    :: value1
    REAL,INTENT(IN)    :: value2

    IF(value1>=value2)THEN
      get_index_of_highest = 1
    ELSE
      get_index_of_highest = 2
    ENDIF

  END FUNCTION get_index_of_highest

  !>\brief Find lowest value of two
  !-------------------------------------------------------------
  INTEGER FUNCTION get_index_of_lowest(value1,value2)

    !Argument declarations
    REAL,INTENT(IN)    :: value1
    REAL,INTENT(IN)    :: value2

    IF(value1<value2)THEN
      get_index_of_lowest = 1
    ELSE
      get_index_of_lowest = 2
    ENDIF

  END FUNCTION get_index_of_lowest

  !>\brief Find matching value of two
  !-------------------------------------------------------------
  INTEGER FUNCTION get_matching_index(value1,value2,currentvalue)

    !Argument declarations
    REAL,INTENT(IN)    :: value1
    REAL,INTENT(IN)    :: value2
    REAL,INTENT(IN)    :: currentvalue

    IF(currentvalue==value1)THEN
      get_matching_index = 1
    ELSEIF(currentvalue==value2)THEN
      get_matching_index = 2
    ELSE
      get_matching_index = 0
    ENDIF

  END FUNCTION get_matching_index

  !>\brief Calculate interflow between water body and floodplain
  !-------------------------------------------------------------
  SUBROUTINE calculate_waterbody_floodplain_interflow(i,fpamax,warea,ifpar,volp,concp,volw,concw,fpdepth,fpdegree,interflow)

    USE MODVAR, ONLY : numsubstances, &
                       basin

    !Argument declarations
    INTEGER,INTENT(IN) :: i         !<index of subbasin
    REAL,INTENT(IN)    :: fpamax    !<maximum area of the flood plain [m2]
    REAL,INTENT(IN)    :: warea     !<(maximum=constant) area of the water body [m2]
    REAL,INTENT(IN)    :: ifpar(5)  !<current interflow parameters (flmrr/floll,flmrp/flolp,fymmr/fymol,rcr2fp/rcl2fp,rcfp2r/rcfp2l
    REAL,INTENT(INOUT) :: volp      !<water volume in floodplain [m3]
    REAL,INTENT(INOUT) :: concp(numsubstances) !<floodplain concentrations
    REAL,INTENT(INOUT) :: volw      !<water volume in water body (river or lake) [m3]
    REAL,INTENT(INOUT) :: concw(numsubstances) !<water body (river or lake) concentration
    REAL,INTENT(OUT)   :: fpdepth   !<flood plain water depth [m]
    REAL,INTENT(OUT)   :: fpdegree  !<flood plain degree of flood [%]
    REAL,INTENT(OUT)   :: interflow !<flow from waterbody to floodplain [m3/timestep] (can be negative)

    !Local variables
    REAL wlmw     !water body water depth [m]
    REAL wlmfp    !floodplain water depth [m]
    REAL fparea   !floodplain water surface area [m2]
    REAL wlequil  !equilibrium level [m]
    REAL qmrflood !interflow [m3 (m)]
    REAL qmrfloodc(numsubstances) !concentration of interflow
    REAL voltemp, atemp
    INTEGER fpflowdir !direction of interflow
    INTEGER status    !error status of subroutine calls

      interflow = 0.

      !water depth in river [m]
      wlmw = volw/warea !water level in main river/outlet lake [m]

      !Calculate current floodplain water depth [m] and water surface area [m2]:
      CALL calculate_floodplain_waterlevel(volp,fpamax,ifpar(3),wlmfp,fparea)

      !Flow occurs  when the river and/or the floodplain are higher than
      !their respective flooding thresholds:
      IF(wlmw>ifpar(1) .OR. wlmfp>ifpar(2))THEN

        !Get flow direction and equilibrium level:
        CALL calculate_floodplain_equilibriumlevel(volp,volw,     &
                                                    ifpar(1),ifpar(2),warea,                        &
                                                    fpamax, ifpar(3), &
                                                    wlequil,fpflowdir)
        !wlequil is now defined in the reference system of the river or the floodplain depending on direction

        IF(fpflowdir == 1)THEN !flow from waterbody (river or lake) to floodplain

          !water level change in river due to flow from river to plain, calculated as
          !a fraction of the potential water level change defined by parameter
          qmrflood = ifpar(4) * (wlmw - wlequil) * warea

          !remove water and substances from waterbody and add it to flood plain
          qmrfloodc(:) = concw
          CALL remove_water(volw,numsubstances,concw,qmrflood,qmrfloodc,status)
          IF(status.NE.0) CALL error_remove_water(errstring(9),basin(i)%subid,i,0)
          CALL add_water(numsubstances,volp,concp,qmrflood,qmrfloodc)
          interflow = qmrflood

        ELSEIF(fpflowdir == 2)THEN !flow from floodplain to river

          !water level change in plain due to flow from plain to river (m):
          qmrflood = ifpar(5) * (wlmfp - wlequil)
          !Floodplain volume at the new water level:
          CALL calculate_floodplain_volume(wlmfp-qmrflood, fpamax, &
                                            ifpar(3),voltemp,atemp)

          !Calculate flow as change in volume [m3]:
          qmrflood = volp - voltemp

          !Remove qmrflood from flood plain and add it to waterbody
          qmrfloodc(:) = concp
          CALL remove_water(volp,numsubstances,concp,qmrflood,qmrfloodc,status)
          IF(status.NE.0) CALL error_remove_water(errstring(10),basin(i)%subid,i,0)
          CALL add_water(numsubstances,volw,concw,qmrflood,qmrfloodc)
          interflow = - qmrflood

        ENDIF
      ELSE
        !No interflow this time:
        !qmrflood = 0.
      ENDIF

      !Calculate output variables floodplain water level and water area
      CALL calculate_floodplain_waterlevel(volp,fpamax,ifpar(3),wlmfp,fparea)
      fpdepth = wlmfp
      fpdegree = fparea/fpamax*100.


  END SUBROUTINE calculate_waterbody_floodplain_interflow

!>\brief Calculate regional floodplain flows
!>
!>Calculate flow between floodplains of rivers and lakes within a subbasin or
!>between subbasins within a regional floodplain for leveling out the water level.
!------------------------------------------------------------------------------
SUBROUTINE calculate_regional_floodplain_flows(n,miscstate,dammedflow,dammedflow2,dammedflow3)

  USE MODVAR, ONLY : flooding,floodindex,path,branchdata,branchindex,classbasin,slc_olake
  USE STATETYPE_MODULE, ONLY : MISCSTATETYPE

  !Argument declarations
  INTEGER, INTENT(IN) :: n  !<number of subbasins
  TYPE(miscstatetype),INTENT(INOUT) :: miscstate  !<Floodplain states
  REAL, INTENT(OUT) :: dammedflow(n) !<potentially dammed flow within subbasin by floodplains (m3/timestep)
  REAL, INTENT(OUT) :: dammedflow2(n) !<potentially dammed flow of olake from downstream subbasin by floodplains (m3/timestep)
  REAL, INTENT(OUT) :: dammedflow3(n) !<potentially dammed flow of main river from downstream subbasin by floodplains (m3/timestep)

  !Local variables
  INTEGER isub,isbmain,isbbranch
  REAL dammedflow4(n),dammedflow5(n)
  REAL tempfloodwater, tempfloodwater2

  !> \b Algorithm \n
  dammedflow = 0. !default output
  dammedflow2 = 0. !default output
  dammedflow3 = 0. !default output

  !>Calculate wanted flow between floodplains in the same subbasin
  DO isub = 1,n
    IF(floodindex(isub)>0)THEN
      IF(flooding(floodindex(isub))%fpfmr>0. .AND. flooding(floodindex(isub))%fpfol>0.)THEN
        CALL calculate_interflow_between_floodplains2(isub,floodindex(isub),1,miscstate%floodwater(1,isub),isub,floodindex(isub),2,miscstate%floodwater(2,isub),dammedflow(isub))
      ENDIF
    ENDIF
  ENDDO

  !>Calculate flow between a floodplain and its downstream floodplain in the maindown subbasin
  DO isub = n-1,1,-1
    IF(floodindex(isub)>0)THEN
      isbmain = path(isub)%main
      IF(isbmain>0)THEN
        IF(floodindex(isbmain)>0)THEN
          IF(flooding(floodindex(isub))%fpfol>0. .AND. flooding(floodindex(isbmain))%fpfmr>0.)THEN
            tempfloodwater = miscstate%floodwater(2,isub)
            IF(dammedflow(isub)<0.) tempfloodwater = tempfloodwater + dammedflow(isub)
            IF(tempfloodwater<0) tempfloodwater=0.
            tempfloodwater2 = miscstate%floodwater(1,isbmain)
            IF(dammedflow(isbmain)<0.) tempfloodwater2 = tempfloodwater2 - dammedflow(isbmain)
            IF(tempfloodwater2<0) tempfloodwater2=0.
            CALL calculate_interflow_between_floodplains2(isub,floodindex(isub),2,tempfloodwater,isbmain,floodindex(isbmain),1,tempfloodwater2,dammedflow2(isub))
          ELSEIF(classbasin(isub,slc_olake)%part==0 .AND. flooding(floodindex(isub))%fpfmr>0. .AND. flooding(floodindex(isbmain))%fpfmr>0.)THEN
            tempfloodwater2 = miscstate%floodwater(1,isbmain)
            IF(dammedflow(isbmain)<0.) tempfloodwater2 = tempfloodwater2 - dammedflow(isbmain)
            IF(tempfloodwater2<0) tempfloodwater2=0.
            CALL calculate_interflow_between_floodplains2(isub,floodindex(isub),1,miscstate%floodwater(1,isub),isbmain,floodindex(isbmain),1,tempfloodwater2,dammedflow3(isub))
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDDO

  !>Calculate flow between a floodplain and its downstream floodplain in the branched subbasin
  DO isub = n-1,1,-1
    IF(floodindex(isub)>0)THEN
      IF(ALLOCATED(branchdata))THEN
        IF(branchindex(isub)>0)THEN
          isbbranch = branchdata(branchindex(isub))%branch
          IF(isbbranch>0)THEN
          IF(floodindex(isbbranch)>0)THEN
            IF(branchdata(branchindex(isub))%mainpart<1.)THEN
              IF(flooding(floodindex(isub))%fpfol>0. .AND. flooding(floodindex(isbbranch))%fpfmr>0.)THEN
                tempfloodwater = miscstate%floodwater(2,isub)
                IF(dammedflow(isub)<0.) tempfloodwater = tempfloodwater + dammedflow(isub)
                IF(dammedflow2(isub)<0.) tempfloodwater = tempfloodwater - dammedflow2(isub)
                IF(tempfloodwater<0) tempfloodwater=0.
                tempfloodwater2 = miscstate%floodwater(1,isbbranch)
                IF(dammedflow(isbbranch)<0.) tempfloodwater2 = tempfloodwater2 - dammedflow(isbbranch)
                IF(tempfloodwater2<0) tempfloodwater2=0.
                CALL calculate_interflow_between_floodplains2(isub,floodindex(isub),2,tempfloodwater,isbbranch,floodindex(isbbranch),1,tempfloodwater2,dammedflow4(isub))
                dammedflow2(isub) = dammedflow2(isub) + dammedflow4(isub)
              ELSEIF(classbasin(isub,slc_olake)%part==0 .AND. flooding(floodindex(isub))%fpfmr>0. .AND. flooding(floodindex(isbbranch))%fpfmr>0.)THEN
                tempfloodwater = miscstate%floodwater(1,isub)
                IF(dammedflow3(isub)<0.) tempfloodwater = tempfloodwater - dammedflow3(isub)
                IF(tempfloodwater<0) tempfloodwater=0.
                tempfloodwater2 = miscstate%floodwater(1,isbbranch)
                IF(dammedflow(isbbranch)<0.) tempfloodwater2 = tempfloodwater2 - dammedflow(isbbranch)
                IF(tempfloodwater2<0) tempfloodwater2=0.
                CALL calculate_interflow_between_floodplains2(isub,floodindex(isub),1,tempfloodwater,isbbranch,floodindex(isbbranch),1,tempfloodwater2,dammedflow5(isub))
                dammedflow3(isub) = dammedflow3(isub) + dammedflow5(isub)
              ENDIF
            ENDIF
          ENDIF
          ENDIF
        ENDIF
      ENDIF
    ENDIF
  ENDDO

END SUBROUTINE calculate_regional_floodplain_flows

!>Calculate equalising flow between floodplains of rivers and lakes.
!------------------------------------------------------------------------------
SUBROUTINE calculate_interflow_between_floodplains2(isub1,iflood1,tflood1,volume1,isub2,iflood2,tflood2,volume2,flow)

  USE MODVAR, ONLY : basin,classbasin,flooding,genpar,slc_mriver,slc_olake,missing_value
  USE HYPEVARIABLES, ONLY : m_optonoff,m_opt7

  !Argument declarations
  INTEGER, INTENT(IN) :: isub1  !<index of subbasin 1
  INTEGER, INTENT(IN) :: iflood1  !<index of flooding for subbasin 1
  INTEGER, INTENT(IN) :: tflood1  !<type of water for subbasin 1 (1=main river, 2=outlet lake)
  REAL, INTENT(IN) :: volume1  !<floodplain 1 water volume (m3)
  INTEGER, INTENT(IN) :: isub2  !<index of subbasin 2
  INTEGER, INTENT(IN) :: iflood2  !<index of flooding for subbasin 2
  INTEGER, INTENT(IN) :: tflood2  !<type of water for subbasin 2 (1=main river, 2=outlet lake)
  REAL, INTENT(IN) :: volume2  !<floodplain 2 water volume (m3)
  REAL, INTENT(OUT) :: flow  !<wanted flow between floodplain 1 and 2 (m3/timestep) (negative for flow from 2 to 1)

  CHARACTER(LEN=80),PARAMETER :: estring = 'error_interflow between floodplains'

  !Local variables
  REAL fpwlmax(2)   !floodplain water level at maximum extent of floodplain (ymax, m)
  REAL fpamax(2) !floodplain area at maximum extent (amax, m2)
  REAL fpwl1,fpwl2 !floodplain water level (m) (wfpi)
  REAL fphref(2) !level of floodplain bottom in reference system (m) (hrefi)
  REAL yeq  !equilibrium water level in reference system (m)
  REAL vol,a
  REAL, PARAMETER :: ratepar(2) = 1. !efficiency of floodplain at damming flow, 1=100%
  REAL volume(2)
  INTEGER flow1to2 !flowdirection; 1=from 1 to 2, 2=from 2 to 1, 0=no flow

  !> \b Algorithm \n
  !>Check if flooding
  IF(volume1<=0. .AND. volume2<=0.) RETURN

  !>Initalize local variables
  IF(tflood1==1)THEN
    fpwlmax(1) = flooding(iflood1)%fymmr
    fpamax(1) = classbasin(isub1,slc_mriver)%part * flooding(iflood1)%fpfmr * basin(isub1)%area
    fphref(1) = flooding(iflood1)%hrefr
  ELSE
    fpwlmax(1) = flooding(iflood1)%fymol
    fpamax(1) = classbasin(isub1,slc_olake)%part * flooding(iflood1)%fpfol * basin(isub1)%area
    fphref(1) = flooding(iflood1)%hrefl
  ENDIF
  IF(tflood2==1)THEN
    fpwlmax(2) = flooding(iflood2)%fymmr
    fpamax(2) = classbasin(isub2,slc_mriver)%part * flooding(iflood2)%fpfmr * basin(isub2)%area
    fphref(2) = flooding(iflood2)%hrefr
  ELSE
    fpwlmax(2) = flooding(iflood2)%fymol
    fpamax(2) = classbasin(isub2,slc_olake)%part * flooding(iflood2)%fpfol * basin(isub2)%area
    fphref(2) = flooding(iflood2)%hrefl
  ENDIF
  IF(genpar(m_optonoff)>0) fpwlmax = genpar(m_opt7)

  !>Calculate floodplain water levels
  CALL calculate_floodplain_waterlevel(volume1,fpamax(1),fpwlmax(1),fpwl1,a)
  CALL calculate_floodplain_waterlevel(volume2,fpamax(2),fpwlmax(2),fpwl2,a)
  !>Calculate equilibrium water level
  volume(1) = volume1;volume(2)=volume2
  CALL calculate_two_floodplain_equilibriumlevel_dp(volume,fpwl1,fpwl2,fpamax,fpwlmax,fphref,yeq,flow1to2)

  !>Calculate wanted interflow
  IF(yeq/=missing_value)THEN
    IF(flow1to2==0)THEN
      flow = 0.
    ELSEIF(flow1to2==1)THEN
      CALL calculate_floodplain_volume(yeq,fpamax(1),fpwlmax(1),vol,a)
      flow = ratepar(1)*(volume1 - vol)
    ELSEIF(flow1to2==2)THEN !flow2to1
      CALL calculate_floodplain_volume(yeq,fpamax(2),fpwlmax(2),vol,a)
      flow = - ratepar(2)*(volume2 - vol)
    ENDIF
  ELSE
    !flow = 0.
    IF(flow1to2==0)THEN
      flow = 0.    !No interflow. Too little water.
    ELSEIF(flow1to2==1)THEN
      flow = ratepar(1)*volume1   !No equlibrium level. All water flow.
    ELSEIF(flow1to2==2)THEN !flow2to1
      flow = - ratepar(2)*volume2   !No equlibrium level. All water flow.
    ENDIF
  ENDIF

END SUBROUTINE calculate_interflow_between_floodplains2


  !>\brief Wetland model for the standing water, including inflow and outflow
  !----------------------------------------------------------------
  SUBROUTINE wetland_watermodel(i,j,isoil,subid,classarea, &
                           temp,swrad,soilstate,miscstate,prev_inflow,   &
                           inflow,cinflow,catcharea,outflow,coutflow)

    USE HYPEVARIABLES, ONLY : wpmm,fcmm,epmm,pwmm, &
                              m_wetrate,m_wetexp,m_iwetw0,m_owetw0,m_grat3
    USE MODVAR, ONLY : conduct,&
                       genpar, &
                       maxsoillayers, &
                       numsubstances, &
                       seconds_per_timestep, &
                       soilthick
    USE SOIL_PROCESSES, ONLY : percolation
    USE NPC_SURFACEWATER_PROCESSES, ONLY : calculate_wetland_processes

    INTEGER, INTENT(IN) :: i        !<index for current subbasin
    INTEGER, INTENT(IN) :: j        !<index for current class
    INTEGER, INTENT(IN) :: isoil    !<index of soil type
    INTEGER, INTENT(IN) :: subid    !<subbasin id
    REAL, INTENT(IN) :: classarea   !<class area [m2]
    REAL, INTENT(IN) :: temp        !<class forcing air temperature
    REAL, INTENT(IN) :: swrad       !<class forcing soalr radiation
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    TYPE(miscstatetype),INTENT(INOUT)  :: miscstate   !<Misc states
    REAL, INTENT(IN) :: prev_inflow  !<inflow to wetland already added (eg. P-E) (m3/s)
    REAL, INTENT(IN) :: inflow       !<inflow to wetland from catchment (m3/s)
    REAL, INTENT(IN) :: cinflow(numsubstances)   !<concentration of inflow (mg/L)
    REAL, INTENT(IN) :: catcharea   !<catchment area of wetland (m2)
    REAL, INTENT(OUT) :: outflow   !<outflow from wetland (m3/s)
    REAL, INTENT(OUT) :: coutflow(numsubstances)   !<concentration of outflow (mg/L)

    !Local variables
    INTEGER nc      !numsubstances
    INTEGER status  !error status of subroutine call

    !Variables for class values
    REAL ginfilt
    REAL cginfilt(numsubstances)   !concentration of infiltration
    REAL overflowmm    !outflow from wetland (mm/ts)
    REAL flowtrans     !factor transfoming m3/s to mm/ts
    REAL w0,wst        !threshold for wetland outflow (m), water level over threshold (m)
    REAL inivolume,stswini  !wetland surface water volume (m3,mm), i.e. standing water
    REAL cwetland(numsubstances)      !wetland surface water concentration
    REAL verticalflows(2) !percolation
    REAL cverticalflows(2,numsubstances) !concentration of percolation
    REAL careaikm2, rating
    REAL liqfrac(maxsoillayers)

    !Local parameters
    CHARACTER(LEN=20) :: errstring(1)  !error message for location of remove_water call
    PARAMETER (errstring = (/'outflow from wetland'/))

    !>\b Algorithm \n
    !>Set output default values
    outflow = 0.
    coutflow = 0.

    !Locally defined variables
    liqfrac = 1.
    flowtrans = seconds_per_timestep/classarea * 1.E+3 !m3/s -> mm/ts
    nc = numsubstances
    w0 = get_wetland_threshold(j)

    !>Add inflow
    ginfilt  = inflow * flowtrans
    cginfilt = cinflow
    IF(ginfilt>0) CALL add_water(numsubstances,soilstate%water(1,j,i),soilstate%conc(:,1,j,i),ginfilt,cginfilt)

    !>Percolation down through the soil layers, including N,P,OC-reduction (safe for more than one soillayer)
    verticalflows = 0.;cverticalflows = 0.
    CALL percolation(i,j,isoil,subid,wpmm(:,j),fcmm(:,j),epmm(:,j),soilthick(:,j),liqfrac,verticalflows(1:2),cverticalflows,soilstate)

    !>NP processes in wetland
    IF(conduct%simN.OR.conduct%simP)THEN
      stswini = (soilstate%water(1,j,i)-pwmm(1,j))  !mm
      IF(stswini>0.)THEN
        inivolume = stswini*1.E-3*classarea  !m3
        cwetland = soilstate%conc(:,1,j,i)
        CALL calculate_wetland_processes(nc,classarea,inivolume,cwetland,miscstate%temp5(i),miscstate%temp30(i),soilstate%fastN(1,j,i),soilstate%fastP(1,j,i),soilstate%humusN(1,j,i),soilstate%humusP(1,j,i),soilstate%partP(1,j,i))
        soilstate%conc(:,1,j,i) = (soilstate%conc(:,1,j,i)*pwmm(1,j) + cwetland*stswini)/(pwmm(1,j)+stswini)
      ENDIF
    ENDIF

    !>T2 processes in wetland; heat exchange (no ice modelled)
    IF(conduct%simT2) CALL T2_processes_in_wetland(i,j,temp,swrad,classarea,soilstate)

    !>Calculate wetland outflow from standing water of uppermost soil layer
    overflowmm = 0.
    IF((soilstate%water(1,j,i)-pwmm(1,j))*1.E-3 - w0>0.)THEN
      careaikm2 = catcharea*1.E-6
      rating = genpar(m_wetrate)
      IF(careaikm2>0.) rating = genpar(m_wetrate)*((catcharea*1.E-6)**genpar(m_grat3))
      wst = (soilstate%water(1,j,i)-pwmm(1,j))*1.E-3 - w0
      outflow = average_flow_rating_curve(inflow+prev_inflow,classarea,wst,rating,genpar(m_wetexp))
      overflowmm = outflow * flowtrans
      IF(overflowmm>(soilstate%water(1,j,i)-pwmm(1,j)) - w0*1.E3)THEN
        overflowmm = (soilstate%water(1,j,i)-pwmm(1,j)) - w0*1.E3
        outflow = overflowmm / flowtrans
      ENDIF
      coutflow = soilstate%conc(:,1,j,i)
      IF(overflowmm > 0.) THEN
        CALL remove_water(soilstate%water(1,j,i),nc,soilstate%conc(:,1,j,i),overflowmm,coutflow,status)
        IF(status.NE.0) CALL error_remove_water(errstring(1),subid,i,j)
      ENDIF
    ENDIF

  END SUBROUTINE wetland_watermodel

  !>Calculate temperature(T2) processes in wetlands
  !----------------------------------------------------------
  SUBROUTINE T2_processes_in_wetland(i,j,temp,swrad,classarea,soilstate)

    USE MODVAR, ONLY: genpar, &
                      i_t2, &
                      modeloption, &
                      p_lakeriverice
    USE HYPEVARIABLES, ONLY: pwmm,&
                             m_t2trriver, &
                             m_riceTf, &
                             m_tcfriver, &
                             m_scfriver, &
                             m_ccfriver, &
                             m_lcfriver, &
                             m_stbcorr1, &
                             m_stbcorr2, &
                             m_stbcorr3, &
                             m_limt2exch

    !Argument variables
    INTEGER, INTENT(IN) :: i               !<index of subbasin
    INTEGER, INTENT(IN) :: j               !<index of class
    REAL, INTENT(IN)    :: temp            !<air temperature
    REAL, INTENT(IN)    :: swrad           !<solar radiation
    REAL, INTENT(IN)    :: classarea       !<wetland area (m2)
    TYPE(soilstatetype),INTENT(INOUT) :: soilstate  !<Soil states

    !Local variables
    REAL stswini
    REAL watertemp, watervol,icefreefraction
    REAL t2transfcorr,freezeuparea

    !Initiate heat deficit and freezeup flag and surface temp variables
    freezeuparea = 0.

    !Get total wetland water volume and mean T2 temperature
    stswini = (soilstate%water(1,j,i)-pwmm(1,j))  !mm
    IF(stswini>0.)THEN
      watervol = stswini*1.E-3*classarea  !m3
      watertemp = soilstate%conc(i_t2,1,j,i)  !Rätt variabel?
    ELSE
      RETURN !?
    ENDIF

    IF(watervol.GT.0.)THEN    !Skip calculations if there is no water in the river

      !Fraction of ice free water surface, calculate more elaborate!
      IF(watertemp>0.)THEN
        icefreefraction = 1
      ELSE
        icefreefraction = 0.
      ENDIF
      t2transfcorr = 1.      !Seasonal correction of T2 exchange coefficient

      !Wetland-Atmosphere T2 exchange, only in ice-free conditions and if there is some water in the river
      IF(icefreefraction.GT.0.)THEN
        ! optional models  (will be reduced to one option after some initial testing for EHYPE3.0 and SHYPE2012)
        SELECT CASE(modeloption(p_lakeriverice))
        CASE(2) ! new model based on Piccolroaz et al 2013, with modifications for fractional ice cover, and calculation of fractional freezup area
          CALL calculate_watersurface_heatbalance(temp,swrad,watertemp,watervol,classarea*icefreefraction, &
                                                  genpar(m_tcfriver),genpar(m_scfriver),genpar(m_ccfriver),genpar(m_lcfriver), &
                                                  genpar(m_limt2exch),freezeuparea,genpar(m_riceTf),genpar(m_stbcorr1),genpar(m_stbcorr2),genpar(m_stbcorr3))
        CASE(1) ! the simple air-water temperature exchange model (Johan/David), with modifications for fractional ice cover, and calculation of fractional freezup area
          CALL calculate_T2_transfer(temp,watertemp,watervol,classarea*icefreefraction,genpar(m_t2trriver)*t2transfcorr, &
                                     freezeuparea,genpar(m_riceTf))
        ENDSELECT

        !Assign update temperature to the wetland
        soilstate%conc(i_t2,1,j,i) = watertemp
      ENDIF
    ENDIF

  END SUBROUTINE T2_processes_in_wetland

  !>Threshold for wetland with regulated flow is set from class or parameters.
  !------------------------------------------------------------------------
  REAL FUNCTION get_wetland_threshold(j)

    USE MODVAR, ONLY: genpar, &
                      classdata, &
                      slc_iwet, &
                      slc_owet
    USE HYPEVARIABLES, ONLY: m_iwetw0,&
                             m_owetw0

    INTEGER, INTENT(IN) :: j  !<class

    !Local variable
    REAL w0

    w0 = MAX(-classdata(j)%streamdepth,0.)
    IF(j==slc_iwet.AND.genpar(m_iwetw0)>0.) w0 = genpar(m_iwetw0)   !For calibration
    IF(j==slc_owet.AND.genpar(m_owetw0)>0.) w0 = genpar(m_owetw0)

    get_wetland_threshold = w0

  END FUNCTION get_wetland_threshold

  !>River water level calculated from inverse function of a rating curve.
  !>q=k*(w-w0)**p -> w=a*q**b+w0, a=(1/k)**b, b=1/p
  !>Parameters given by GeoData.txt
  !------------------------------------------------------------------------
  REAL FUNCTION river_water_level(itype,i,q,ice,frozenstate)

    USE MODVAR, ONLY: basin, genpar
    USE HYPEVARIABLES, ONLY:  m_ricebupo, m_ricethpo, m_ricew0por, m_ricew1por, m_ricew0ice, m_ricew1ice

    INTEGER, INTENT(IN) :: itype   !<type of river; 1=local, 2=main
    INTEGER, INTENT(IN) :: i   !<subbasin
    REAL, INTENT(IN)    :: q   !<river flow (m3/s)
    LOGICAL, INTENT(IN) :: ice !<status of ice on river
    TYPE(snowicestatetype),INTENT(IN) :: frozenstate   !<Snow and ice states

    !Local variable
    REAL level, iceweight,iceinfluence,porosityinfluence
    REAL pice,pnice,kice,knice,pscaled,kscaled

    INTEGER icemodel

    !Currently chosen ice influence on river water level calculation: icemodel 4
    !It is a combination of different models depending on input data given.
    !It scales the rating curve between the given summer and winter parameters
    !based on ice thickness and ice porosity if they are given, otherwise is scales the exponent to 1 for the winter.
    !Old versions kept in code for possible future option inclusion.
    icemodel = 4

    level = 0.
    IF(q>0.)THEN
      IF(ice)THEN
        SELECT CASE(icemodel)
          CASE(1) !based on ice porosity only (version in HYPE 5.11.1 and 5.11.2)
            IF((frozenstate%rivericepor(itype,i).LE.genpar(m_ricethpo) .AND. (genpar(m_ricebupo)-genpar(m_ricethpo)).GT.0.) .OR. &
              (genpar(m_ricebupo)-genpar(m_ricethpo))<=0.)THEN
              level = basin(i)%mrivc(4)*q**basin(i)%mrivc(5) ! 100% ice rating curve
            ELSE ! weighted between ice and non-ice curve, where weight is based on ice porosity
              iceweight = AMAX1(0.,AMIN1(1.,(genpar(m_ricebupo) - frozenstate%rivericepor(itype,i))/(genpar(m_ricebupo)-genpar(m_ricethpo))))
              level = iceweight * basin(i)%mrivc(4)*q**basin(i)%mrivc(5)   + (1. - iceweight) * basin(i)%mrivc(2)*q**basin(i)%mrivc(3)
            ENDIF
          CASE(2) !scale between the given summer and winter parameters based on ice thickness and ice porosity (version used in Arctic-HYPE june2020?)
            !Make sure all parameters area non-zero - otherwise just use summer curve
            IF(basin(i)%mrivc(4).GT.0 .AND. basin(i)%mrivc(5).GT.0 .AND. basin(i)%mrivc(2).GT.0 .AND. basin(i)%mrivc(3).GT.0)THEN
              !ice thickness influence (1 => 100% winter-curve, 0 => 100% summer-curve)
              IF(frozenstate%riverice(itype,i).GE.genpar(m_ricew1ice))THEN
                iceinfluence = 1.
              ELSEIF(frozenstate%riverice(itype,i).LE.genpar(m_ricew0ice))THEN
                iceinfluence = 0.
              ELSE
                iceinfluence = AMIN1(1.,AMAX1(0.,(frozenstate%riverice(itype,i) - genpar(m_ricew0ice))/(genpar(m_ricew1ice)-genpar(m_ricew0ice))))
              ENDIF

              !ice porosity influence (1 => 100% winter curve, 0 => 100% summer curve
              IF(frozenstate%rivericepor(itype,i).GE.genpar(m_ricew0por))THEN
                porosityinfluence = 0.
              ELSEIF(frozenstate%rivericepor(itype,i).LE.genpar(m_ricew1por))THEN
                porosityinfluence = 1.
              ELSE
                porosityinfluence = AMIN1(1.,AMAX1(0.,(genpar(m_ricew0por) - frozenstate%rivericepor(itype,i))/(genpar(m_ricew0por)-genpar(m_ricew1por))))
              ENDIF

              !ice curve weight == MIN(icethickness and porosity influence)
              iceweight = AMIN1(iceinfluence,porosityinfluence)

              !scale both p and k between the given ice and non-ice parameters
              pice    = 1./basin(i)%mrivc(5)
              pnice   = 1./basin(i)%mrivc(3)
              kice    = 1./(basin(i)%mrivc(4)**pice)
              knice   = 1./(basin(i)%mrivc(2)**pnice)
              pscaled = pnice * (1.- iceweight) + iceweight * pice
              kscaled = knice * (1.- iceweight) + iceweight * kice

              !apply the scaled p and k values in the inversed rating curve
              level = ((1./kscaled)**(1./pscaled))*q**(1./pscaled)
            ELSE
              !unscaled summer curve
              level = basin(i)%mrivc(2)*q**basin(i)%mrivc(3)
            ENDIF
          CASE(3) !simple model - just scale summer curve exponent towards 1, but scaled based on ice thickness and ice porosity
            !Make sure all summer parameters area non-zero - otherwise just use summer curve
            IF(basin(i)%mrivc(2).GT.0 .AND. basin(i)%mrivc(3).GT.0)THEN
              !ice thickness influence (1 => 100% winter-curve, 0 => 100% summer-curve)
              IF(frozenstate%riverice(itype,i).GE.genpar(m_ricew1ice))THEN
                iceinfluence = 1.
              ELSEIF(frozenstate%riverice(itype,i).LE.genpar(m_ricew0ice))THEN
                iceinfluence = 0.
              ELSE
                iceinfluence = AMIN1(1.,AMAX1(0.,(genpar(m_ricew1ice) - frozenstate%riverice(itype,i))/(genpar(m_ricew1ice)-genpar(m_ricew0ice))))
              ENDIF

              !ice porosity influence (1 => 100% winter curve, 0 => 100% summer curve
              IF(frozenstate%rivericepor(itype,i).GE.genpar(m_ricew0por))THEN
                porosityinfluence = 0.
              ELSEIF(frozenstate%rivericepor(itype,i).LE.genpar(m_ricew1por))THEN
                porosityinfluence = 1.
              ELSE
                porosityinfluence = AMIN1(1.,AMAX1(0.,(genpar(m_ricew0por) - frozenstate%rivericepor(itype,i))/(genpar(m_ricew0por)-genpar(m_ricew1por))))
              ENDIF

              !ice curve weight == MIN(icethickness and porosity influence)
              iceweight = AMIN1(iceinfluence,porosityinfluence)

              !scale summer curve exponent towards 1 at full ice influence
              pnice   = 1./basin(i)%mrivc(3)
              kscaled = 1./(basin(i)%mrivc(2)**pnice)
              pscaled = pnice * (1.- iceweight) + iceweight

              !apply the scaled p and k values in the inversed rating curve
              level = ((1./kscaled)**(1./pscaled))*q**(1./pscaled)
            ELSE
              !unscaled summer curve
              level = basin(i)%mrivc(2)*q**basin(i)%mrivc(3)
            ENDIF

          CASE(4) !combination of several models; scale between the given summer and winter parameters based on ice thickness and ice porosity if given otherwise to exponent 1
            !If all parameters are non-zero - scale between the given summer and winter parameters based on ice thickness and ice porosity
            IF(basin(i)%mrivc(4).GT.0 .AND. basin(i)%mrivc(5).GT.0 .AND. basin(i)%mrivc(2).GT.0 .AND. basin(i)%mrivc(3).GT.0)THEN
              !ice thickness influence (1 => 100% winter-curve, 0 => 100% summer-curve)
              IF(frozenstate%riverice(itype,i).GE.genpar(m_ricew1ice))THEN
                iceinfluence = 1.
              ELSEIF(frozenstate%riverice(itype,i).LE.genpar(m_ricew0ice))THEN
                iceinfluence = 0.
              ELSE
                iceinfluence = AMIN1(1.,AMAX1(0.,(frozenstate%riverice(itype,i) - genpar(m_ricew0ice))/(genpar(m_ricew1ice)-genpar(m_ricew0ice))))
              ENDIF

              !ice porosity influence (1 => 100% winter curve, 0 => 100% summer curve
              IF(frozenstate%rivericepor(itype,i).GE.genpar(m_ricew0por))THEN
                porosityinfluence = 0.
              ELSEIF(frozenstate%rivericepor(itype,i).LE.genpar(m_ricew1por))THEN
                porosityinfluence = 1.
              ELSE
                porosityinfluence = AMIN1(1.,AMAX1(0.,(genpar(m_ricew0por) - frozenstate%rivericepor(itype,i))/(genpar(m_ricew0por)-genpar(m_ricew1por))))
              ENDIF

              !ice curve weight as minimum of icethickness and porosity influence
              iceweight = AMIN1(iceinfluence,porosityinfluence)

              !scale both p and k between the given ice and non-ice parameters
              pice    = 1./basin(i)%mrivc(5)
              pnice   = 1./basin(i)%mrivc(3)
              kice    = 1./(basin(i)%mrivc(4)**pice)
              knice   = 1./(basin(i)%mrivc(2)**pnice)
              pscaled = pnice * (1.- iceweight) + iceweight * pice
              kscaled = knice * (1.- iceweight) + iceweight * kice

              !apply the scaled p and k values in the inversed rating curve
              level = ((1./kscaled)**(1./pscaled))*q**(1./pscaled)

            !If summer parameters are non-zero - scale the given summer parameters based on ice thickness and ice porosity to exponent one
            ELSEIF(basin(i)%mrivc(2).GT.0 .AND. basin(i)%mrivc(3).GT.0)THEN
              !ice thickness influence (1 => 100% winter-curve, 0 => 100% summer-curve)
              IF(frozenstate%riverice(itype,i).GE.genpar(m_ricew1ice))THEN
                iceinfluence = 1.
              ELSEIF(frozenstate%riverice(itype,i).LE.genpar(m_ricew0ice))THEN
                iceinfluence = 0.
              ELSE
                iceinfluence = AMIN1(1.,AMAX1(0.,(genpar(m_ricew1ice) - frozenstate%riverice(itype,i))/(genpar(m_ricew1ice)-genpar(m_ricew0ice))))
              ENDIF

              !ice porosity influence (1 => 100% winter curve, 0 => 100% summer curve
              IF(frozenstate%rivericepor(itype,i).GE.genpar(m_ricew0por))THEN
                porosityinfluence = 0.
              ELSEIF(frozenstate%rivericepor(itype,i).LE.genpar(m_ricew1por))THEN
                porosityinfluence = 1.
              ELSE
                porosityinfluence = AMIN1(1.,AMAX1(0.,(genpar(m_ricew0por) - frozenstate%rivericepor(itype,i))/(genpar(m_ricew0por)-genpar(m_ricew1por))))
              ENDIF

              !ice curve weight as minimum of icethickness and porosity influence
              iceweight = AMIN1(iceinfluence,porosityinfluence)

              !scale summer curve exponent towards 1 at full ice influence
              pnice   = 1./basin(i)%mrivc(3)
              kscaled = 1./(basin(i)%mrivc(2)**pnice)
              pscaled = pnice * (1.- iceweight) + iceweight

              !apply the scaled p and k values in the inversed rating curve
              level = ((1./kscaled)**(1./pscaled))*q**(1./pscaled)

            !If no parameters - use summer curve (may be zero water level change then)
            ELSE
              !unscaled summer curve
              level = basin(i)%mrivc(2)*q**basin(i)%mrivc(3)
            ENDIF
          CASE DEFAULT !use summer curve
            level = basin(i)%mrivc(2)*q**basin(i)%mrivc(3)
          END SELECT
      ELSE
        level = basin(i)%mrivc(2)*q**basin(i)%mrivc(3)
      ENDIF
    ENDIF

    river_water_level = level + basin(i)%mrivc(1) !reference level

  END FUNCTION river_water_level

  !>Check if there is ice on the river
  !------------------------------------------------------------------------
  LOGICAL FUNCTION ice_on_river(itype,i,frozenstate)

    USE MODVAR, ONLY: conduct

    INTEGER, INTENT(IN) :: itype   !<type of river; 1=local, 2=main
    INTEGER, INTENT(IN) :: i       !<subbasin
    TYPE(snowicestatetype),INTENT(INOUT) :: frozenstate   !<Snow and ice states

    ice_on_river = .FALSE.

    IF(conduct%lakeriverice)THEN
      IF(frozenstate%riverice(itype,i)>0.) ice_on_river = .TRUE.
    ENDIF

  END FUNCTION ice_on_river

  !>Initialise lake and river ice model
  !------------------------------------------------------------------------
  SUBROUTINE initiate_lakeriverice()

    USE HYPEVARIABLES, ONLY : m_licebupo,m_ricebupo,m_ricew1ice,m_ricew0por
    USE MODVAR, ONLY: genpar

    IF(genpar(m_ricebupo)==0.) genpar(m_ricebupo) = 1.
    IF(genpar(m_licebupo)==0.) genpar(m_licebupo) = 1.

    IF(genpar(m_ricew1ice)==0.) genpar(m_ricew1ice) = 0.1   !at least 0.1 cm ice to switch to the winter curve
    IF(genpar(m_ricew0por)==0.) genpar(m_ricew0por) = 0.001 !at least 0.1% ice porosity to switch back to summer curve

  END SUBROUTINE initiate_lakeriverice



  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !!!!!! Implementation of the HGDM algorithm    !!!!!!
  !!!!!! for the prairies						 !!!!!!
  !!!!!! Implemented by M.I.Ahmed UC-HAL 2021    !!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  ! The following subrouting is modified from the calculate_ilake_outflow
  ! to handle lake/pothole daynamics and call HGDM algorithm
  !>\brief Subroutine for calculation and removal of outflow from prairie depressions.
  !! The HGDM algorithm was developed by Kevin Shook
  !! and implemented in HYPE by M.I.Ahmed.
  ! version history
  !*********************
  ! v01 initial implementation
  !*********************
  ! v01.1 modified runoff depths at lake and upland and fixed area_frac = 1 since
  ! changing it made errors in the units conversion (m3/s) since it is not updated in HYPE
  ! there are small differences in current_depth between the different timesteps
  !due to the fact that HGDM and HYPE handles the water depth in lake differently.
  ! HYPE transfers all the water first to the lake (different runoff compared to HGDM) and then subtract
  ! the outflow from the lake
  ! HGDM aggregates the fluxes at the basin scale (handles both lake and upland at the same time)
  ! assiging current depth from vol_frac makes the current_depth consistent between timesteps.
  ! However, the model does not generate reasonable results (water keeps building up to max_depth and fluctuates around it)
  ! For now, we will use the current_depth from the lakestatemm, this makes it more consistent with the lake stage
  !*********************
  !v 01.2 Current_depth is read from the vol_frac variable because HYPE and HGDM treat the
  ! ponded depth differently, which causes problems in the calculations
  ! make the area_frac dynamic
  ! Fixed outflow calc, outflow from HGDM is basin depth (i.e., outflow (cms) = outflow (HGDM)* basinarea), while outflow from ilake
  ! is from the ilake depth (outflow (cms) = outflow (ilake) * lakearea)
  !***************************
  !v 1.3 scale max_depth to be basin average (max_depth * lakearea/basinarea) and take
  ! current_depth from wlmr
  ! so far, the depth is being passed properly from timestep to the other depth(t) = depth(t-1)
  ! fixes area_frac = 1 since it causes errors in the contrib_area
  !***************************
  !v 1.4 -> clean up the code v1.3 to be passed to SMHI
  ! hard-coding area_mult, area_power since the area_frac is kept constant (=1)
  ! skip reading the input parameters from txt and read them from the HYPE inputs max_depth, max_water_area_frac
  !----------------------------------------------------------------------------
  SUBROUTINE calculate_HGDM_depressions_outflow(i,subid,ns,qin,pein,lakearea,basinarea,qunitfactor, &
                                     outflowm3s,coutflow,load,volumeflow,wst,fnca,lakestate)

    USE HYPEVARIABLES, ONLY : ratingk, &
                              m_grat2, &
                              m_ilrrat2
    USE HYPE_WATERBALANCE, ONLY : w_iltomr
    USE MODVAR, ONLY : basin, &
                       conductload, &
                       conductwb, &
                       genpar, &
                       regpar, &
                       regiondivision, &
                       seconds_per_timestep

    !Argument declarations
    INTEGER, INTENT(IN) :: i             !<index of current subbasin
    INTEGER, INTENT(IN) :: subid         !<subid of current subbasin
    !INTEGER, INTENT(IN) :: nsub         !<number of subbasins
    INTEGER, INTENT(IN) :: ns            !<number of substances
    REAL, INTENT(IN)    :: qin           !<lateral inflow (runoff from upland) of lake (m3/s)  - river discharge and point sources - distributed by icatch
    REAL, INTENT(IN)    :: pein          !<vertical inflow of lake (m3/s) - precipitation and evaporation - distributed by lake area
    REAL, INTENT(IN) 	:: lakearea      !<lakearea (m2)
	REAL, INTENT(IN)    :: basinarea      !<basinarea (m2)
    REAL, INTENT(IN)    :: qunitfactor   !<factor for transforming flow for lake from m3/s to mm/timestep and back
    REAL, INTENT(OUT)   :: outflowm3s    !<outflow of lake (m3/s)
    REAL, INTENT(OUT)   :: coutflow(ns)  !<concentration of outflow of lake
    REAL,ALLOCATABLE,INTENT(INOUT) :: load(:,:)  !<load of outflow of lake (and other flows)
    REAL,ALLOCATABLE,INTENT(INOUT) :: volumeflow(:,:)    !<volume outflow of lake (m3/ts) (and other flows)
    REAL, INTENT(OUT)   :: wst           !<lake water (mm)
	REAL, INTENT(INOUT)   :: fnca          !<fraction of non-contributing area (per subbasin area)
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate  !<Lake state

    !Local parameters
    INTEGER, PARAMETER :: itype = 1        !lake type (local)
    REAL, PARAMETER :: lakedepth_not_used = 1.
    REAL, PARAMETER :: hypodepth_not_used = 1.

    !Local variables
    REAL lakewstmm              !lake water stage (mm)
    REAL wlmr,wlmr0             !water level lake  (m)
    REAL w0Today                !water level threshold  (m)
    REAL ratingc,ratinge        !current rating curve parameters
    REAL outflowmm              !outflow of lake (mm)
    REAL qinmm, peinmm          !inflow and vertical fluxes depths (mm) at the lake scale

	!Local variables for HGDM
	REAL delta_depth 			! = pein -> vertical inflow of lake (m3/s) - precipitation and evaporation
	REAL runoff_depth			! = qin -> lateral inflow (runoff from upland) of lake (m3/s)  - river discharge and point sources
    REAL current_depth, current_depth_old			! = wlmr -> water level lake  (m)
	REAL contrib_frac, contrib_frac_old 			! fraction of contributing area
	REAL area_frac 				! fraction of basin area occupied by water
	REAL vol_frac 				! volume fraction (fraction of filling)
	REAL depth 					! water level lake  (m) updated by HGDM
    REAL outflow_depth 			! outflow from depressions calculated by HGDM (m)
    !CHARACTER(LEN=100) trash !read text from HGDM input par file
    !integer ifsubid !subid from HGDM file
    !REAL Mass_Balance !check for mass balance

	!HGDM parameters (to be passed by HYPE)
	REAL max_depth  !m
    REAL max_water_area_frac !fraction
    REAL area_mult  !for SCRB fixed value
    REAL area_power  !for SCRB fixed value
	!>\b Algorithm \n
    !Initial values
    outflowm3s = 0.
    outflowmm = 0.
    lakewstmm=lakestate%water(itype,i) ! that already includes qin and pein
    wlmr = lakewstmm*0.001              !Water in lake [m]



    !! !Current parameter values
    !! ratingc = ratingk(itype,i)
    !! ratinge = 0.
    !! IF(basin(i)%parregion(regiondivision(m_ilrrat2))>0) ratinge=regpar(m_ilrrat2,basin(i)%parregion(regiondivision(m_ilrrat2)))  !TODO: check subroutine for only olake, probably not used
    !! IF(ratinge<=0.) ratinge = genpar(m_grat2)
    w0Today = basin(i)%lakedepth(itype)
	! read HGDM input parameters
	max_water_area_frac = lakearea/basinarea
	max_depth = basin(i)%lakedepth(itype)
	max_depth = max_depth*(lakearea/basinarea) !v1.3 scale max_depth to be basin average
	! hard coded parameter values (area_frac is fixed to 1)
    area_mult = 1.0058485 !for SCRB
    area_power = 1.5198 !for SCRB
	
    !Inflow (runoff) and net prec-evap in mm converted at the lake scale (runoff * upland area / lake area)
    peinmm = pein * qunitfactor
    qinmm  = qin  * qunitfactor

    !water in ilake before inflows
    !scale current depth
    current_depth    = ((lakewstmm - peinmm - qinmm) /1000.0)*(lakearea/basinarea) ! v1.3 mm -> m
    current_depth_old = current_depth

    !>Calculate inflows to depressions using HGDM
    ! runoff needs to be depth from the upland (qin is runoff depth scaled at the lake)

	delta_depth = pein * qunitfactor ! vertical flux (P-E) m3/s -> mm/timestep
	delta_depth = delta_depth / 1000.0 ! mm/timestep -> m/timestep
	!converts the runoff to be depth from upland (divide by upland area (basinarea-lakearea)
	runoff_depth = qin * qunitfactor * (lakearea/(basinarea-lakearea))!runoff from upland m3/s -> mm/timestep
	runoff_depth = runoff_depth / 1000.0 ! mm/timestep -> m/timestep

	!Get states from previous time step
	contrib_frac = lakestate%fcarea(1,i) !state variable to be passed by HYPE
	contrib_frac_old = contrib_frac
	area_frac = 1 !state variable, currently fixed at 1 (max_water_area_frac)
	vol_frac = current_depth / max_depth !lakestate%volfrac(1,i)
	depth = current_depth
    outflow_depth = 0.0 !initialize outflow (m)


	CALL small_depressions_delta_water(current_depth, delta_depth, runoff_depth, contrib_frac, &
                            max_depth, max_water_area_frac, &
                            area_mult, area_power, vol_frac, area_frac, &
                            depth, outflow_depth)
	!update HYPE variables
	! only send the outflow depth to be removed from HYPE's ilake storage
	! outflow from HGDM is for the entire basin
	!scaled to be from lakearea only

	outflowmm = (outflow_depth*basinarea/lakearea) * 1000.0 !m -> mm
	fnca = 1.0 - contrib_frac
	!we may use rating curves at this point, the outflow from HGDM will be the input
	!>Check outflow against lake volume
    IF(outflowmm>lakewstmm) outflowmm = lakewstmm    !Safety for rounded wlmr and ldepth = 0


    outflowm3s = outflowmm/qunitfactor

    !>Remove outflow from lake
    CALL remove_outflow_from_lake(i,itype,ns,outflowmm,subid,lakedepth_not_used,hypodepth_not_used,lakewstmm,coutflow,lakestate)
	!Bookkeeping for the next time step
    lakestate%fcarea(1,i) = contrib_frac 
    lakestate%volfrac(1,i) = depth / max_depth 
    
    !>Set output variables
    IF(conductload) load(:,11) = outflowm3s * coutflow * seconds_per_timestep * 1.E-3 !Load at point K, outflow ilake (kg/timestep)
    IF(conductwb) volumeflow(w_iltomr,i) = outflowm3s * seconds_per_timestep  !m3/ts
    wst = lakestate%water(itype,i)  !mm
	! this should be updated to change lake area
	! lakearea = area_frac * max_water_area_frac * basinarea !m2
    !checking mass balance
    ! lakewstmm  already includes qin and pein (initial sorage + inflow)
    ! wst final storage
    !Mass_Balance = lakewstmm - wst - outflowmm
	!!debug writing the output to file
	!open(unit=123, file='0-output.csv',status='old', access='append')
	!write(123,1110)subid,lakewstmm,wlmr,w0Today,delta_depth,runoff_depth,current_depth,&
    !            contrib_frac,area_frac,outflow_depth,outflowmm,wst,depth,lakearea,current_depth_old,&
    !            lakestate%volfrac(1,i), contrib_frac_old
    !            !lakestate%volfrac(1,i), lakestate%farea(1,i)
    !1110    format(9999(g15.7e2, ','))
    !close(123)

  END SUBROUTINE calculate_HGDM_depressions_outflow


	!*********************************************
	!=============================================
	!+++++++++++++++++++++++++++++++++++++++++++++
	! The Hysteretic and Gatekeeping Depressions Model (HGDM)
	! Developed by Kevin Shook and modified by Mohamed Ahmed to be used in hydrologic models
	! HYPE and MESH
	!Copyright (C) 2021  Kevin Shook & Mohamed Ahmed

	! This is the main subroutine of the algorithm.
	! v01: Currently, the HGDM handles the complxeities of the small depression
	! that are represented using the linear form. No Big Ponds (gatekeeping) function.

	!**************************************************************************

	!    __       __    ______  _______ .__   __.      _______. _______
	!   |  |     |  |  /      ||   ____||  \ |  |     /       ||   ____|
	!   |  |     |  | |  ,----'|  |__   |   \|  |    |   (----`|  |__
	!   |  |     |  | |  |     |   __|  |  . `  |     \   \    |   __|
	!   |  `----.|  | |  `----.|  |____ |  |\   | .----)   |   |  |____
	!   |_______||__|  \______||_______||__| \__| |_______/    |_______|
	!
	!  This program is free software; you can redistribute it and/or modify
	!  it under the terms of the GNU General Public License as published by
	!  the Free Software Foundation; either version 3 of the License, or
	!  (at your option) any later version.
	!
	!  This program is distributed in the hope that it will be useful,
	!  but WITHOUT ANY WARRANTY; without even the implied warranty of
	!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	!  GNU General Public License for more details.
	!
	!  You should have received a copy of the GNU General Public License
	!  along with this program; if not, write to the Free Software
	!  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
	!  MA 02110-1301, USA.
	!**************************************************************************


	!MAIN SUBROUTINE

	! Converted from R -> Fortran by Kevin Shook, June 2020
	!
	! This function applies net precip to water, and calculates runoff depth
	! Because the areas of application (of direct precip., evap, and runoff)
	! depend on the storage

	! The function arguments are mostly dimensionless fractions or
	! areal-average depths, so that that they are more easily shared among
	! basins. However, they need to be dimensionalised within the function.

	! Arguments:
	! current_depth - state variable. Current depth of water (m) ) in
	! depressional storage

	! delta_depth - flux. Change of depth of water to be applied (m)
	! directly to the small pond water area (i.e. direct precip or evap.),
	! either positive or negative

	! runoff_depth - flux. Depth of upland runoff running into
	! small ponds (m)

	! max_depth - state parameter. Maximum depth of water (m) that can be
	! stored in the small ponds

	! max_water_area_frac - state parameter. Max possible areal fraction of
	! the small ponds basin/GRU that can be covered with water.

	! application_iterations - subroutine execution parameter. Number of
	! iterations used for applying fluxes

	! area_mult, area_power - scaling parameters. Multiplier and exponent
	! relating small pond water area to water depth.

	! vol_frac - state variable. Fraction of maximum storage volume filled
	! by water. Updated by subroutine.

	! area_frac - state variable. Fraction of maximum water area covered
	! by water. Updated by subroutine.

	! contrib_frac - state variable. Fracation of the GRU/basin affected
	! by small ponds, which is contributing runoff. Updated by subroutine.

	! depth - state variable. Current depth of water (m) on the GRU/basin.
	! Updated by subroutine.

	! outflow_depth - flux. Depth of outflow (m) from small ponds. Output
	! by subroutine.

  subroutine small_depressions_delta_water(current_depth, delta_depth, runoff_depth, contrib_frac, &
                            max_depth, max_water_area_frac, & !application_iterations,
                            area_mult, area_power, vol_frac, area_frac, &
                            depth, outflow_depth)

	! arguments
	real :: current_depth, delta_depth, runoff_depth, contrib_frac, max_depth
	real ::  max_water_area_frac, area_mult, area_power, vol_frac, area_frac
	real :: depth, outflow_depth
	!integer :: application_iterations

	! internal variables
	real :: current_vol_frac, iteration_delta, iteration_runoff, iteration_contrib_frac
	real :: iteration_area_frac, total_outflow_depth, water_area_frac, upland_area_frac
	real :: delta_depth_applied, excess_depth !, current_depth

	current_vol_frac = current_depth / max_depth  ! current fractional storage volume
	iteration_delta = delta_depth 
	iteration_runoff = runoff_depth 
	iteration_contrib_frac = contrib_frac
	iteration_area_frac = small_depression_water_frac_area(current_vol_frac, area_mult, area_power)
	total_outflow_depth = 0.0

    ! check to see if worth doing
	if ((iteration_delta /= 0.0) .or. (iteration_runoff > 0.0)) then
		! apply fluxes 
		water_area_frac = max_water_area_frac * iteration_area_frac   ! basin/HRU areal fraction of water
		upland_area_frac = 1.0 - water_area_frac
		delta_depth_applied = (iteration_delta * water_area_frac)  + &
        iteration_runoff * upland_area_frac * (1.0 - iteration_contrib_frac)

		current_depth = current_depth + delta_depth_applied

		! check to see if max depth of storage is exceeded
		if (current_depth > max_depth) then
		! max depth is exceeded, so there will be outflows
			excess_depth = max(current_depth - max_depth, 0.0)
			current_depth = max_depth
			iteration_contrib_frac = 1.0
			total_outflow_depth = total_outflow_depth + &
			(excess_depth * water_area_frac) + &
			(iteration_runoff * upland_area_frac * iteration_contrib_frac)

		else
		! max depth is not exceeded, so no outflows from depressions, but there is outflow
		!from upland area
			excess_depth = 0.0
			total_outflow_depth = total_outflow_depth + &
				(iteration_runoff * upland_area_frac * iteration_contrib_frac)
			! update contributing fraction for next iteration/time step
			iteration_contrib_frac = small_depression_contrib_frac(iteration_contrib_frac, current_depth, &
                                                   delta_depth_applied, max_depth)
		endif

		! update state variables
		current_vol_frac = current_depth / max_depth
		iteration_area_frac = small_depression_water_frac_area(current_vol_frac, area_mult, area_power)


		vol_frac = current_depth  / max_depth
		area_frac = iteration_area_frac
		contrib_frac = iteration_contrib_frac
		depth = current_depth
		outflow_depth = total_outflow_depth

	else
		! nothing to do - return initial values
		vol_frac = current_depth / max_depth
		area_frac = iteration_area_frac
		contrib_frac = iteration_contrib_frac
		depth = current_depth
		outflow_depth = 0.0
	endif

  end subroutine


	!==========================================================================

	! F U N C T I O N S

	!------------------------------

	! A function for calculating the water area of parameterized small ponds
	! Converted from R -> Fortran by Kevin Shook, June 2020

	! The function uses a simple exponential relationship to estimate the
	! fractional water area from the fractional water volume.

	! The fractional water area is the fraction of the maximum possible
	! water area occupied by small depressions in a basin/grid/GRU which
	! is occupied by water.

	! Arguments:
	! vol_frac - state variable. The fraction of the depressional
	! storage in a basin/grid/GRU which is occupied by water.

	! mult - calculation parameter. The multiplier in the relationship
	! between the volume and area fractions. Optional.


	! power - calculation parameter. The exponent in the relationship
	! between the volume and area fractions. Optional.

	! If mult or power are not specified, the function will use values
	! estimated from Smith Creek Research Basin. These will NOT be
	! appropriate for your location.


  function  small_depression_water_frac_area (vol_frac, mult, power)
	real :: small_depression_water_frac_area
	! arguments
	real, intent(in) :: vol_frac
	real, optional :: mult, power

	! internal variables
	real :: water_frac

	! check for parameter values - if not specified use SCRB values
	if (.not. present(mult)) then
		mult = 1.0058485
	endif

	if (.not. present(power)) then
		power = 1.5198
	endif

	! use simple exponential equation to calculate water areal fraction
	if (vol_frac <= 0.0) then
		water_frac = 0.0
	else
		water_frac = mult * vol_frac ** power
	endif

	small_depression_water_frac_area = max(min(water_frac, 1.0), 0.0)
	!***********
	!area_frac needs to be fixed because it caused problems in the calculations (since that is not transferred to HYPE)
	! fixing area_frac =1 fixes the NCA calc issue (high contr_area with very small vol_frac)
    small_depression_water_frac_area = 1.0
    !**********
  end function small_depression_water_frac_area

	!------------------------------

	! A function for calculating the contributing fraction of
	! parameterized small depressions

	! Converted from R -> Fortran by Kevin Shook, June 2020

	! This function uses a simple linear form of the
	! hysteretic relationship between the volumetric water fraction
	! and the contributing fraction of a basin/grid/GRU.

	! In this function, the contributing fraction refers to the
	! areal fraction of a basin/grid/GRU which can contribute
	! overland flow to its outlet, whether or not flow is actually
	! occurring.

	! The contributing fraction is estimated inside the space defined
	! by the ranges of the volume fraction (x) and
	! the contributing fraction (y), i.e. between (0,0) and (1,1).

	! If there is evaporation, then the contributing fraction is zero.

	! If there is an addition of water, then the contributing fraction
	! is estimated from the current value, on a trajectory to (1,1).

	! Note that because the contributing fraction of the small depressions
	! varies continuously with the application of water, this function
	! needs to be called repeatedly, as water is added to the depressions.
	! This is shown in the subroutine small_ponds_delta_water.

	! Arguments:
	! current_contribfrac - state variable. Current version of the
	! contributing fraction of small depressions in the basin/grid/GRU

	! current_depth - state variable. Depth of water (m) in the fraction
	! of the basin/grid/GRU occupied by small depressions.

	! delta_depth - flux. Depth of water (m) applied to the fraction of the
	! basin/grid/GRU occupied by small depressions.

	! max-depth - limiting value. Maximum possible depth of water (m) in
	! the fraction of the basin/grid/GRU occupied by small depressions.



  function small_depression_contrib_frac(current_contrib_frac, &
                                  current_depth, delta_depth, max_depth)
	real :: small_depression_contrib_frac

	! arguments
	real, intent(in) :: current_contrib_frac, current_depth, delta_depth, max_depth

	! internal variables
	real :: new_contribfrac, volfrac, volfrac_delta, slope

	if (delta_depth == 0.0 ) then
		new_contribfrac = current_contrib_frac
	else
		if (delta_depth < 0.0) then
		! evaporation
		new_contribfrac = 0.0
		else
		! direct precip and/or runoff
		volfrac = current_depth / max_depth  ! current fractional water vol
		volfrac_delta = delta_depth / max_depth ! change in water vol frac

		! calculate the slope of the line connecting the current point
		! to (1, 1)
		slope = (1.0 - current_contrib_frac) / (1.0 - volfrac)
		slope = max(slope, 0.0)  ! slope can't be < 0 for addition of water

		! using the slope, calculate the new contributing fraction
		new_contribfrac = current_contrib_frac + (volfrac_delta * slope)
		endif
	endif

	small_depression_contrib_frac = min(max(new_contribfrac, 0.0), 1.0)

  end function small_depression_contrib_frac

	!*********************************************
	!=============================================
	!+++++++++++++++++++++++++++++++++++++++++++++


  END MODULE SURFACEWATER_PROCESSES
