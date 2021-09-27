!> \file soil_proc.f90
!> Contains module soil_processes.

!>Water processes in soil in HYPE and some more
MODULE SOIL_PROCESSES

  !Copyright 2012-2020 SMHI
  !
  !This file is part of HYPE.
  !HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
  !HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
  !You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.

  !-----------------------------------------------------------------------------------------

  USE STATETYPE_MODULE, ONLY :soilstatetype,snowicestatetype
  USE GENERAL_WATER_CONCENTRATION, ONLY : remove_water, &
                                          error_remove_water, &
                                          add_water
  USE NPC_SOIL_PROCESSES, ONLY : atmdep_in_loss, &
                                 doc_percolation_reduction, &
                                 onpp_percolation_reduction
  USE ATMOSPHERIC_PROCESSES, ONLY : deltasaturationpressure_function
  !Uses also modvar, hypevariables, hype_indata

  IMPLICIT NONE
  PRIVATE 
!Private procedures
!-------------------
! calculate_porevolumes
! calculate_fractional_snowcover
! calculate_snowheat_processes
! snow_thermal_conductivityfunction
! airpressure_elevationfunction
! psychrometric_constant
! get_current_petmodel
! calculate_weighted_temperature
! calculate_three_soil_temperature
! water_is_above_field_capacity
! water_above_field_capacity
! water_is_above_wilting_point
! water_above_wilting_point
! unfrozen_water_above_field_capacity
! water_relative_porevolume
  PUBLIC initiate_soil_water_state, &
         initiate_soil_water, &
         calculate_snow, &
         calculate_snowmelt, &
         snowalbedo_function, &
         latentheat_tempfunction, &
         set_evaporation_concentrations, &
         calculate_potential_evaporation, &
         calculate_actual_soil_evapotranspiration, &
         calculate_tile_drainage, &
         calculate_soil_runoff, &
         calculate_infiltration_flow_diversion, &
         add_infiltration, &
         flood_infiltration, &
         percolation, &
         add_macropore_flow, &
         calculate_groundwater_table, &
         calculate_snowdepth, &
         calculate_glacier_melt, &
         calculate_soiltemp , &
         calculate_unfrozen_soil_water, &
         calculate_liquid_water_fraction, &
         calculate_frostdepth, &
         calculate_soil_moisture_deficit
         
  !Private parameters, global in this module
  CHARACTER(LEN=80) :: errstring(12)  !error message for location of remove_water call, 1-6 not used
  PARAMETER (errstring = (/'tile runoff, drainage pipe in soillayer 1     ',    & !1
                           'tile runoff, drainage pipe in soillayer 2     ',      &
                           'tile runoff, drainage pipe in soillayer 3     ',      & !3
                           'runoff from soillayer 1                       ',      & !4
                           'runoff from soillayer 2                       ',      &
                           'runoff from soillayer 3                       ',      &
                           'evapotranspiration soil, soillayer 1          ',      & !7
                           'evapotranspiration soil, soillayer 2          ',      & !8
                           'percolation from soillayer 1                  ',      &  
                           'percolation from soillayer 2                  ',      & 
                           'snow melt                                     ',      & !11
                           'snow sublimation                              '/))

CONTAINS

  !>\brief Initiate soil water state variables when no saved state exist.
  !!
  !>\b Reference ModelDescription Chapter Land routines (Basic assumptions)
  !---------------------------------------------------------------------------
  SUBROUTINE initiate_soil_water_state(soilstate)

    USE MODVAR, ONLY : nsub,          &
                       nclass,        &
                       maxsoillayers, &
                       soiliniwet
    
    !Argument declaration
    TYPE(soilstatetype),INTENT(INOUT) :: soilstate   !<Soil states
    
    !Local variables
    INTEGER i
    REAL :: iniwater(maxsoillayers,nclass)
    REAL :: wp(maxsoillayers,nclass),fc(maxsoillayers,nclass),ep(maxsoillayers,nclass)

    !>\b Algoritm \n
    !>Calculate size of water storage in soil (wp,fc and ep) in mm
    CALL calculate_porevolumes(wp,fc,ep)  !in mm
    iniwater = 0.
    iniwater = iniwater + wp + fc
    IF(soiliniwet) iniwater = iniwater + ep

    !>Initiate soil state to saturation or plant available water
    DO i = 1,nsub
      soilstate%water(:,:,i) = iniwater(:,:)
    ENDDO

  END SUBROUTINE initiate_soil_water_state

  !>\brief Initiate soil water help parameters.
  !!
  !>\b Consequences Module hypevariables epotdist, soilrc, basinrrcscorr, basincevpam, 
  !> basincevpph, basinlp, pwmm, wpmm, fcmm and epmm is set.
  !!
  !>\b Reference ModelDescription Chapter Land routines (Basic assumptions, Soil water -
  !!Groundwater runoff) and Processes above ground (Evaporation)
  !---------------------------------------------------------------------------
  SUBROUTINE initiate_soil_water()

    USE HYPEVARIABLES, ONLY : epotdist, &         !OUT
                              soilrc,   &         !OUT
                              wpmm,fcmm,epmm, &   !OUT
                              pwmm,  & !OUT
                              basinrrcscorr,   & !OUT
                              basincevpam,   & !OUT
                              basincevpph,   & !OUT
                              basinlp,   & !OUT
                              m_epotdist,    &
                              m_cevpam,m_cevpph,m_lp,  &
                              m_rrcs1,m_rrcs2,m_rrcs3, &
                              m_rrcscorr,  &
                              n_rrcsc,n_rrcs3,n_cevpa,n_cevpp,n_lp
    USE MODVAR, ONLY : classdata,         &
                       basin,             &
                       nsub, nclass,      &
                       maxsoillayers,     &
                       soildepth,         &
                       soilthick,         &
                       regiondivision, &
                       genpar,soilpar,regpar, &
                       conductregest
    USE HYPE_INDATA, ONLY : set_regest_parameter
         
    !Local variables
    INTEGER i,j,isb           !loop-variables (subbasin,class)
    REAL    coeff
    REAL    sums
    REAL    rc0,rc1,rc2,b   !help variables for recession coefficient calculation

    !>\b Algoritm \n
    !>Calculate distribution of potential evaporation between soil layers
    IF(.NOT.ALLOCATED(epotdist)) ALLOCATE(epotdist(2,nclass))
    coeff=genpar(m_epotdist)
    DO j = 1, nclass
      sums = soilthick(1,j)*EXP(-coeff*soildepth(1,j)/2.) + soilthick(2,j)*EXP(-coeff*(soildepth(1,j)+(soildepth(2,j)-soildepth(1,j))/2.))
      epotdist(1,j) = soilthick(1,j)*EXP(-coeff*soildepth(1,j)/2.) / sums
    ENDDO
    epotdist(2,:) = 1 - epotdist(1,:)

    !>Initiate soil water content parameters
    IF(.NOT.ALLOCATED(wpmm)) ALLOCATE(wpmm(maxsoillayers,nclass))
    IF(.NOT.ALLOCATED(fcmm)) ALLOCATE(fcmm(maxsoillayers,nclass))
    IF(.NOT.ALLOCATED(epmm)) ALLOCATE(epmm(maxsoillayers,nclass))
    IF(.NOT.ALLOCATED(pwmm)) ALLOCATE(pwmm(maxsoillayers,nclass))
    CALL calculate_porevolumes(wpmm,fcmm,epmm)
    pwmm = wpmm + fcmm + epmm

    !Set soil runoff recession correction
    IF(.NOT.ALLOCATED(basinrrcscorr)) ALLOCATE(basinrrcscorr(nsub))
    DO i = 1,nsub
      IF(basin(i)%parregion(regiondivision(m_rrcscorr))>0)THEN
        basinrrcscorr(i) = 1. + regpar(m_rrcscorr,basin(i)%parregion(regiondivision(m_rrcscorr)))   !Correction of recession coefficients
      ELSE
        basinrrcscorr(i)  = 1.
      ENDIF
        
      !Replace parameter values with regional parameter estimates
      IF(conductregest) CALL set_regest_parameter(i,n_rrcsc,basinrrcscorr(i),1.)
    ENDDO

    !Initiate soil runoff recession coeffcients
    IF(.NOT.ALLOCATED(soilrc)) ALLOCATE(soilrc(maxsoillayers,nclass,nsub))
    !>Calculate adjustment factors
    rc2 = genpar(m_rrcs3)                         !Runoff coefficient slope dependence   
    DO i = 1,nsub
      !>Replace parameter values with regional parameter estimates
      IF(conductregest) CALL set_regest_parameter(i,n_rrcs3,rc2)

      !>Calculate soil runoff recession coeffcients for each soil layer, class and subbasin
      DO j=1,nclass
        rc0 = soilpar(m_rrcs1,classdata(j)%soil)*basinrrcscorr(i)       !Runoff coefficient in surface layer 
        IF(rc0>1.) rc0 = 1.
        rc0 = rc0+rc2*basin(i)%slope    !runoff coefficient in upper soil layer (slope dependent)
        IF(rc0>1.) rc0 = 1.
        rc1 = soilpar(m_rrcs2,classdata(j)%soil)*basinrrcscorr(i)       !Runoff coefficient in bottom layer 
        IF(rc1>1.) rc1 = 1.
        IF(rc1==0) rc1 = rc0
        b = LOG(rc0/rc1)/((soildepth(3,j)-soilthick(3,j)/ 2.) - soilthick(1,j)/ 2.)
        soilrc(1,j,i) = rc0
        soilrc(3,j,i) = rc1
        soilrc(2,j,i) = rc0 * EXP(-b* (soildepth(2,j) - soilthick(2,j)/2. - soilthick(1,j)/ 2.))
      ENDDO
    ENDDO
    
    !>Set evaporation seasonal corrections
    IF(.NOT.ALLOCATED(basincevpam))THEN
      ALLOCATE(basincevpam(nsub))
      ALLOCATE(basincevpph(nsub))
    ENDIF
    basincevpam = genpar(m_cevpam)
    basincevpph = genpar(m_cevpph)
    !Replace parameter values with regional parameter estimates
    IF(conductregest)THEN
      DO isb = 1,nsub
        CALL set_regest_parameter(isb,n_cevpa,basincevpam(isb))
        CALL set_regest_parameter(isb,n_cevpp,basincevpph(isb))
      ENDDO
    ENDIF

    !>Set evaporation subbasin parameter
    IF(.NOT.ALLOCATED(basinlp)) ALLOCATE(basinlp(nsub))
    basinlp = genpar(m_lp)
    IF(conductregest)THEN   !Replace parameter value with regional parameter estimates
      DO isb = 1,nsub
        CALL set_regest_parameter(isb,n_lp,basinlp(isb))
      ENDDO
    ENDIF      

  END SUBROUTINE initiate_soil_water

  !>\brief Calculate porevolumes
  !---------------------------------------------------------------------------
  SUBROUTINE calculate_porevolumes(wp,fc,ep)

    USE HYPEVARIABLES, ONLY : m_wcfc,m_wcwp,m_wcep, &
                              m_wcfc1,m_wcfc2,m_wcfc3, &
                              m_wcwp1,m_wcwp2,m_wcwp3, &
                              m_wcep1,m_wcep2,m_wcep3
                              
    USE MODVAR, ONLY : classdata, &
                       nclass, &
                       maxsoillayers, &
                       soilthick, &
                       soilpar
    
    !Argument
    REAL, INTENT(OUT) :: wp(maxsoillayers,nclass) !<water content in mm
    REAL, INTENT(OUT) :: fc(maxsoillayers,nclass) !<water content in mm
    REAL, INTENT(OUT) :: ep(maxsoillayers,nclass) !<water content in mm
    
    !Local variables
    INTEGER j
    REAL    soilthick1000(maxsoillayers,nclass)

    !>\b Algoritm \n

    !>Initiate soil water content parameters
    soilthick1000 = soilthick * 1000.
    DO j = 1,nclass
      !Wilting point (mm)
      wp(:,j) = soilpar(m_wcwp,classdata(j)%soil)
      IF(soilpar(m_wcwp1,classdata(j)%soil) > 0.) wp(1,j) = soilpar(m_wcwp1,classdata(j)%soil)
      IF(soilpar(m_wcwp2,classdata(j)%soil) > 0.) wp(2,j) = soilpar(m_wcwp2,classdata(j)%soil)
      IF(soilpar(m_wcwp3,classdata(j)%soil) > 0.) wp(3,j) = soilpar(m_wcwp3,classdata(j)%soil)
      wp(:,j) = wp(:,j) * soilthick1000(:,j)
      
      !Field capacity (mm)
      fc(:,j) = soilpar(m_wcfc,classdata(j)%soil)
      IF(soilpar(m_wcfc1,classdata(j)%soil) > 0.) fc(1,j) = soilpar(m_wcfc1,classdata(j)%soil)
      IF(soilpar(m_wcfc2,classdata(j)%soil) > 0.) fc(2,j) = soilpar(m_wcfc2,classdata(j)%soil)
      IF(soilpar(m_wcfc3,classdata(j)%soil) > 0.) fc(3,j) = soilpar(m_wcfc3,classdata(j)%soil)
      fc(:,j) = fc(:,j) * soilthick1000(:,j)
      
      !Effectiv porosity (mm)
      ep(:,j) = soilpar(m_wcep,classdata(j)%soil)
      IF(soilpar(m_wcep1,classdata(j)%soil) > 0.) ep(1,j) = soilpar(m_wcep1,classdata(j)%soil)
      IF(soilpar(m_wcep2,classdata(j)%soil) > 0.) ep(2,j) = soilpar(m_wcep2,classdata(j)%soil)
      IF(soilpar(m_wcep3,classdata(j)%soil) > 0.) ep(3,j) = soilpar(m_wcep3,classdata(j)%soil)
      ep(:,j) = ep(:,j) * soilthick1000(:,j)
    ENDDO

  END SUBROUTINE calculate_porevolumes
  
  !>Subroutine for calculation of changes in snow pack; snowfall addition, snow 
  !>pack melting and snow age
  !!
  !>\b Reference ModelDescription Chapter Land routines (Snow routines)
  !------------------------------------------------------------------------
  SUBROUTINE calculate_snow(i,j,subid,iluse,snowfall,csnowfall,snow,csnow,temp,melt, &
                            cmelt,swrad,snowage,snowmax,snowdepth,snowcover,snowheat, &
                            epot,evap,cevap,effcov,snowtemp,snowsurftemp,snowliq,srunoff)
  
    USE MODVAR, ONLY : genpar, &
                       simulate, &
                       i_t1,i_t2,       &
                       numsubstances, &
                       missing_value,   &
                       modeloption,     &
                       p_snowmelt,      &
                       p_snowevap,      &
                       p_snowheat,      &
                       Lfreezing,       &
                       cice
    USE HYPEVARIABLES, ONLY : m_cmrad,m_fsceff,m_cmrefr, &
                              m_dsndens,m_T1evap,m_whcsnow
    
    !Argument declarations
    INTEGER, INTENT(IN) :: i          !<index of current subbasin
    INTEGER, INTENT(IN) :: j          !<index of current class
    INTEGER, INTENT(IN) :: subid      !<subbasin id
    INTEGER, INTENT(IN) :: iluse      !<index of landuse
    REAL, INTENT(IN)    :: snowfall   !<precipitation as snow (mm/timestep)
    REAL, INTENT(IN)    :: csnowfall(numsubstances) !<concentration of precipitation as snow 
    REAL, INTENT(INOUT) :: snow       !<snow pack (mm)
    REAL, INTENT(INOUT) :: csnow(numsubstances) !<concentration of snow 
    REAL, INTENT(IN)    :: temp       !<air temperature (C)
    REAL, INTENT(OUT)   :: melt       !<snow melt (mm/timestep)
    REAL, INTENT(OUT)   :: srunoff    !<snow runoff (mm/timestep)
    REAL, INTENT(OUT)   :: cmelt(numsubstances)     !<substances of snow melt
    REAL, INTENT(IN)    :: swrad      !<shortwave radiation (MJ/m2/day?)
    REAL, INTENT(INOUT) :: snowage    !<age of snow (timesteps)
    REAL, INTENT(INOUT) :: snowmax    !<maximum snow pack during winter (mm)
    REAL, INTENT(INOUT) :: snowdepth  !<current depth of snow (cm)
    REAL, INTENT(INOUT) :: snowcover  !<snowcover fraction
    REAL, INTENT(INOUT) :: snowheat   !<snowpack heat content (J)
    REAL, INTENT(IN)    :: epot       !<potential evapotranspiration of snow (mm/timestep)
    REAL, INTENT(OUT)   :: evap       !<snow sublimation (mm/timestep)
    REAL, INTENT(OUT)   :: effcov     !<effective snowcover used for scaling snow and soil evaporation (0 if snowevap is switched off)
    REAL, INTENT(OUT)   :: cevap(numsubstances)   !<concentrations in snow sublimation
    REAL, INTENT(OUT)   :: snowtemp   !<snowpack temperature
    REAL, INTENT(OUT)   :: snowsurftemp !<snowpack surface temperature
    REAL, INTENT(INOUT) :: snowliq    !<snowpack liquid content (mm)
    
    !Local variables
    INTEGER status
    REAL fsceff,abla,abla0 
    REAL oldsnow    !snow pack at beginning of calculations (cm)
    REAL fracmelt
    REAL snowice
    REAL whc, maxliq, refreeze
  
    !>\b Algorithm \n
    !>Set parameter values and default output
    whc = genpar(m_whcsnow)
    fsceff = genpar(m_fsceff)   !efficiency of fractional snow cover to reduce melt and evap
    effcov = 1.-fsceff*(1.-snowcover) !effective snow cover used to scale melt and evap
    oldsnow = snow  !needed for snow depth calculation
    cmelt = 0.
    cevap = 0.
    snowtemp = 0.
    snowsurftemp = 0.
    fracmelt = 1.
    srunoff = 0.
    refreeze = 0.
    
    !>Calculate snow heat processes; heat content, temperature and surface temperature, refreezing liquid water if needed
    IF(modeloption(p_snowheat).GE.1)THEN
      CALL calculate_snowheat_processes(iluse,temp,snow,snowdepth,snowheat,snowtemp,snowsurftemp,fracmelt,snowliq)
    ENDIF
    
    !>calculate frozen mass of snow (after heat processes)
    snowice = AMAX1(0.,snow - snowliq)
    
    !>Calculate potential snow melt (and re-freeze)
    IF(modeloption(p_snowheat).GE.1)THEN
       CALL calculate_snowmelt(iluse,i,temp,swrad,snow,snowage,effcov,melt,snowliq,refreeze)
      IF(fracmelt>0.)THEN
        melt     = melt*fracmelt
      ELSE
        melt=0.
      ENDIF
    ELSE
      CALL calculate_snowmelt(iluse,i,temp,swrad,snow,snowage,effcov,melt,snowliq,refreeze)
    ENDIF
    !>Limit snow melt to frozen part
    IF(melt>snowice) melt = snowice
    
    !>Limit refreeze to liquid water - and apply it
    IF(refreeze>0.)THEN
      snowice = snowice + AMIN1(refreeze,snowliq)
      snowliq = AMAX1(0.,snowliq-refreeze)
    ENDIF
    
    !>Calculate potential evaporation from snow (sublimation and evaporation of frozen and unfrozen part of snowmass, respectively)
    IF(modeloption(p_snowevap).GE.1)THEN
      IF(snow>0)THEN
        evap = epot * effcov
      ELSE
        evap = 0.
        effcov = 0. !make sure effcov = 0 if there is no snow, otherwise there will be no soil evaporation
      ENDIF
    ELSE
      evap = 0.
      effcov = 0. !make sure effcov = 0 if snowevap is switched off, otherwise there will be no soil evaporation
    ENDIF
    IF(evap>snow) evap = snow

    !>Calculate ablation and check against snow pack
    abla0 = melt + evap      !potential ablation (melt+evap)
    abla = MIN(abla0, snow)  !limit ablation to available snow
    IF(abla0>0.)THEN
      IF(abla<abla0)THEN
        melt = melt * abla/abla0   !melt is already limited to frozen part of snow
        evap = evap * abla/abla0   !so the distribution between melt and evap should still be correct like this
        IF((melt+evap)>snow) evap = snow - melt !extra safe?
      ENDIF
    ELSE
      melt = 0.
      evap = 0.
    ENDIF
    
    !>Update snow pack with snowfall (for now, rain on snow is still bypassing the snowpack - but ideally, if whc>0 the rain on snow covered ground should also be possible to be trapped in the snow)
    IF(snowfall>0.)THEN
      CALL add_water(numsubstances,snow,csnow,snowfall,csnowfall)
      IF(modeloption(p_snowheat).GE.1)THEN
        snowheat = snowheat - Lfreezing*1000.*snowfall + MIN(temp,0.)*cice*1000.*snowfall
      ENDIF
      snowice = snowice + snowfall
    ENDIF
    
    !Calculate maximum water holding capacity based on snowmass after melt and evaporation
    IF((1.-whc)>0.)THEN
      maxliq = whc * AMAX1(0.,snowice-melt-evap) / (1.-whc) ! make check that whc<1 and whc>=0
    ELSE
      maxliq = 0.
    ENDIF
    
    !Update snow and liquid fraction with melt and/or runoff
    IF(melt>0. .OR. snowliq>maxliq)THEN
      snowice = snowice - melt
      snowliq = snowliq + melt
      srunoff = AMAX1(0.,snowliq-maxliq)
      snowliq = AMAX1(0.,snowliq-srunoff)
      
      cmelt = csnow
      
      IF(srunoff<snow .AND. snow>0.)THEN
        CALL remove_water(snow,numsubstances,csnow,srunoff,cmelt,status)
        IF(status/=0) CALL error_remove_water(errstring(11),subid,i,j)
        IF(modeloption(p_snowheat).GE.1)THEN
          snowheat = snowheat + Lfreezing*1000.*melt
        ENDIF
      ELSE
        snow = 0.
        snowliq = 0.
        csnow = 0.  !remove last traces
        IF(modeloption(p_snowheat).GE.1) snowheat = 0.
      ENDIF
      IF(simulate%substance(i_t2)) cmelt(i_t2)=0. !temp.conc. in meltwater = 0
    ENDIF
    
    !Update with evaporation
    IF(evap>0.)THEN
      cevap=0.
      IF(simulate%substance(i_t1)) cevap(i_t1)= genpar(m_T1evap) * csnow(i_t1)
      IF(evap<snow .AND. snow>0.)THEN
        ! remove evaporation from liquid water in relation to the liquid water fraction       
        snowliq = AMAX1(0.,snowliq - evap * snowliq / snow)
        ! remove evaporation from total snowmass
        CALL remove_water(snow,numsubstances,csnow,evap,cevap,status)
        IF(status/=0) CALL error_remove_water(errstring(12),subid,i,j)
        IF(modeloption(p_snowheat).GE.1)THEN
          snowheat = snowheat + Lfreezing*1000.*evap
        ENDIF
      ELSE
        snow = 0.
        snowliq = 0.
        csnow = 0.  !remove last traces
        IF(modeloption(p_snowheat).GE.1) snowheat = 0.
      ENDIF
    ENDIF  

    !>Calculate degree of snowcover and update maximum snow pach during winter
    CALL calculate_fractional_snowcover(iluse,0.,snow,snowmax,snowcover)

    !>Calculate snow age and snow depth
    CALL calculate_snowdepth(iluse,snow,oldsnow,snowfall,temp,genpar(m_dsndens),snowage,snowdepth)
    
    !>final check if snow==0 and snowliq>0 (shouldnt be needed, but seems to be some bug above)
    IF(snow<=0. .AND. snowliq>0.)THEN
      snowliq=0.
    ENDIF

  END SUBROUTINE calculate_snow
  
  !>Subroutine for calculation of snow melt by different methods
  !!
  !> \b Reference ModelDescription Chapter Land routines (Snow routines)
  !------------------------------------------------------------------------
  SUBROUTINE calculate_snowmelt(iluse,i,temp,swrad,snow,snowage,effcov,melt,snowliq,refreeze)
  
    USE MODVAR, ONLY : basin, &
                       genpar, &
                       landpar, &
                       regpar, &
                       regiondivision, &
                       modeloption, &
                       p_snowmelt
    USE HYPEVARIABLES, ONLY : m_ttmp,m_cmlt,  &
                              m_snalbmin,m_snalbmax,m_snalbkexp,  &
                              m_cmrad,m_cmrefr,m_cmltcorr
    
    !Argument declarations
    INTEGER, INTENT(IN) :: iluse      !<index of landuse
    INTEGER, INTENT(IN) :: i          !<index of subbasin
    REAL, INTENT(IN)    :: temp       !<air temperature (C)
    REAL, INTENT(IN)    :: swrad      !<shortwave radiation (MJ/m2/day?)
    REAL, INTENT(IN)    :: snow       !<snow pack (mm)
    REAL, INTENT(IN)    :: snowage    !<age of snow (timesteps)
    REAL, INTENT(IN)    :: effcov     !<effective snowcover used for scaling of snow melt
    REAL, INTENT(OUT)   :: melt       !<snow melt (mm/timestep)
    REAL, INTENT(IN)    :: snowliq    !<snow liquid water (mm)
    REAL, INTENT(OUT)   :: refreeze   !<snow refreezing (mm/timestep)
    
    !Local variables
    REAL tt       !threshold temperature for snow melt (and evaporation) (C)
    REAL cm       !coefficient for snow melt (mm/C/timestep)
    REAL cmc      !correction of cm
    REAL snowalbedo
    REAL snalbmax, snalbmin, snalbkexp
    REAL cmrad     !radiation index snow melt factor (mm/MJ/timestep)
    REAL cmrefr
    

    !>\b Algorithm \n
    !>Set parameter values and default output
    tt = landpar(m_ttmp,iluse)  !threshold temperature for snow melt
    cm = landpar(m_cmlt,iluse)  !Coefficient for snow melt
    IF(basin(i)%parregion(regiondivision(m_cmltcorr))>0)THEN
!      IF(regpar(m_cmltcorr,basin(i)%parregion(regiondivision(m_cmltcorr)))/=0.) &
!        cm = cm * (1.+regpar(m_cmltcorr,basin(i)%parregion(regiondivision(m_cmltcorr))))
      cmc = (1. + regpar(m_cmltcorr,basin(i)%parregion(regiondivision(m_cmltcorr))))
    ELSE
      cmc = 1.
    ENDIF
    cm = cm * cmc
    melt = 0.
    refreeze = 0.
    
    !Melting calculation sequence:
    ! -> To reduce the number of snowmelt options, snow cover melt scaling 
    !    is now included in all snowmelt models. Instead of options for each combination
    !    of melt and sublimation, the fraction of snow cover reduction is controlled
    !    by a new parameter fsceff (fraction of fsc reduction)
    ! -> For backward compitability, the previous snowmelt options values are still used 
    !    (0,1 temp index, 2 temp+rad index) - but note that p_snowmelt = 0 may now also 
    !    imply snowcover scaling and sublimation, depending on parameter fsceff and fepotsnow.
    ! -> Sublimation is calculated separately after the melt subroutine, and is also controlled by fsceff.
    ! -> Ablation = melt + sublimation is introduced
    ! -> Minimization of ablation to current snow is made after calculation of (potential) 
    !    melt and sublimation. The reduction from potential to actual ablation is finally
    !    distributed on melt and sublimation.

    !>Select snow melt model
    SELECT CASE(modeloption(p_snowmelt))
    CASE(0,1) 
      !>\li Case 0 and 1: Original temperature index model, calculated with or without snowcover scaling
      IF(snow>0 .AND. temp >= tt) THEN
        melt = cm   * (temp - tt)  !potential melt
        melt = melt * effcov       !snowcover melt scaling (no reduction of snowcover=1 and/or fsceff=0)
      ENDIF
    CASE(2)
      !>\li Temperature and radiation index model, with/without snowcover scaling and refreezing
      !Set parameter values
      snalbmin  = landpar(m_snalbmin,iluse)
      snalbmax  = landpar(m_snalbmax,iluse)
      snalbkexp = landpar(m_snalbkexp,iluse)
      cmrad     = landpar(m_cmrad,iluse)
      cmrefr    = genpar(m_cmrefr)
    
      !Radiation melt component
      snowalbedo = snowalbedo_function(snowage,snalbmin,snalbmax,snalbkexp)
      melt = cmrad * swrad * (1.-snowalbedo)
      
      !Add Temperature component
      IF(snow>0. .AND. temp >= tt)THEN
        melt = melt + cm * (temp - tt)
      ENDIF
      
      !Refreezing component when temperatures below tt, as a fraction cmrefr of cm 
      IF(snow>0. .AND. temp < tt .AND. melt > 0.)THEN
        melt = melt - cmrefr * cm * (tt - temp)
        IF(melt<0.)THEN
          refreeze = AMIN1(-melt,snowliq)
          melt = 0.
        ENDIF
      ENDIF
      
      !Scale melt with fractional snow cover
      melt     = melt * effcov
      refreeze = refreeze * effcov
    CASE DEFAULT
      !>\li Case default (0): Original temperature index model, calculated with or without snowcover scaling
      IF(snow>0 .AND. temp >= tt) THEN
        melt = cm   * (temp - tt)  !potential melt
        melt = melt * effcov       !snowcover melt scaling
      ENDIF
    END SELECT
    
  END SUBROUTINE calculate_snowmelt
  
!>Function to calculate snow albedo depending on the snow age        
!>
!> \b Reference ModelDescription Chapter Land routines (Snow routines)
!------------------------------------------------------------------------
  FUNCTION snowalbedo_function(snowage,albmin,albmax,kexp) RESULT(albedo)
  
  !Argument declarations
  REAL,INTENT(IN)  :: snowage !<snow age (timesteps)
  REAL,INTENT(IN)  :: albmin  !<minimum albedo (typical value 0.4)
  REAL,INTENT(IN)  :: albmax  !<maximum albedo (typical value 0.9)
  REAL,INTENT(IN)  :: kexp    !<exponential factor (1/time step) (typical value 0.1 for daily timesteps)
  REAL             :: albedo  ! albedo, fractional reflection of shortwave radiation (-)
  !David Gustafsson, 2013-02-05
  
  !Calculate albedo with a simplified exponential function
  albedo = albmin+(albmax-albmin)*EXP(-kexp*snowage)
  
  END FUNCTION snowalbedo_function

  !>\brief Subroutine for calculation of fractional snow cover area
  !>The maximum snow of the winter season is also calculated
  !>
  !Based on Lindström&Gardelin(1999;2000) following the implementation in 
  !the Rossby centre RCA-model (Samuelsson et al, 2006)
  !
  !> \b Reference ModelDescription Chapter Land routines (Snow routines - Snow cover)
  !------------------------------------------------------------------------
  SUBROUTINE calculate_fractional_snowcover(iluse,elevstd,snow,snowmax,fsc)
  
    USE MODVAR, ONLY : landpar, genpar, seconds_per_timestep
    USE HYPEVARIABLES, ONLY :  m_fscmax,m_fscmin,m_fsclim, & 
                               m_fscdistmax, m_fscdist0,m_fscdist1, &
                               m_fsck1, m_fsckexp
    !Argument declarations
    INTEGER, INTENT(IN) :: iluse       !<index of landuse
    REAL, INTENT(IN)    :: elevstd     !<standard deviation of elevation (m)
    REAL, INTENT(IN)    :: snow        !<snow pack (mm)
    REAL, INTENT(INOUT) :: snowmax     !<maximum snow pack during winter (mm)
    REAL, INTENT(OUT)   :: fsc         !<fractional snowcover area (-)

    !Local variables
    REAL timestep_seconds,fscdist
    timestep_seconds = REAL(seconds_per_timestep)
    
    !>\b Algorithm \n
    !>Check snow pack status
    IF(snow.gt.0.)THEN   !Snow present
      !>Check if snowcover model is used
      IF(genpar(m_fscmax)==0)THEN
        fsc = 1.
      ELSE
        !>Check snowpack development phase, and select corresponding FSC function
        IF(snowmax.le.0.)THEN
          !1) Accumulation phase, snowmax = 0
          !1.1) fsc = tangens-hyperbolic function, Eq 28 (Samuelsson 2006)
          fsc = MAX(genpar(m_fscmin),genpar(m_fscmax) * TANH(0.1 * snow))
          !1.2) Set snowmax = snow, if fsc >= fscmax - fsclim
          IF(fsc.ge.(genpar(m_fscmax)-genpar(m_fsclim)))THEN
            snowmax = snow
          ENDIF
        ELSE
          !2) Melting phase, snowmax>0 (onset in previous timesteps)
          !2.1) update snowmax
          IF(snow.GT.snowmax)THEN
            !update snowmax to new maximum snow value
            snowmax = snow
          ELSE
            !decrease snowmax towards end of melt season, eq. 31 (Samuelsson 2006)
            IF(snow.LT.genpar(m_fsck1)*snowmax)THEN
              snowmax = snowmax - (genpar(m_fsck1) * snowmax - snow)*(1.-EXP(-genpar(m_fsckexp) * timestep_seconds)) / genpar(m_fsck1)
            ENDIF 
          ENDIF
          !2.2) calculate snow distribution factor, Eq 30 (Samuelsson 2006)
          fscdist = MIN(landpar(m_fscdistmax,iluse),landpar(m_fscdist0,iluse) + landpar(m_fscdist1,iluse) * elevstd)
          !2.3) fsc=linear function, Eq 29 (Samuelsson 2006)
          fsc = MAX(genpar(m_fscmin),MIN(genpar(m_fscmax),snow / (snowmax * fscdist)))
        ENDIF
      ENDIF 
    ELSE  !No snow
      snowmax = 0.
      fsc = 0.
    ENDIF
    
  END SUBROUTINE calculate_fractional_snowcover
  
  !>Calculation of snowdepth and age of snow. Snow depth depends on age of snow.
  !>
  !>\b Reference ModelDescription Chapter Land routines (Snow routines - Soil temperature and snow depth)
  !--------------------------------------------------------------------
  SUBROUTINE calculate_snowdepth(iluse,snow,oldsnow,snowfall,temp,snowdensdt,snowage,snowdepth)

    USE MODVAR, ONLY : timesteps_per_day, &
                       modeloption,   &
                       p_snowdensity,  &
                       genpar,landpar
    USE HYPEVARIABLES, ONLY : m_ttmp, &
                              m_sndens0,m_sdnsmax, &
                              m_sdnsrate,m_sdnsradd
    
    !Argument declarations
    INTEGER, INTENT(IN) :: iluse      !<index of landuse
    REAL, INTENT(IN)    :: snow       !<snow water equivalent (mm)
    REAL, INTENT(IN)    :: oldsnow    !<snow water equivalent before snowfall/melt this timestep (mm)
    REAL, INTENT(IN)    :: snowfall   !<precipitation as snow (mm/timestep)
    REAL, INTENT(IN)    :: temp       !<air temperature (C)
    REAL, INTENT(IN)    :: snowdensdt !<snow density increase due to ageing (g/cm3.timestep)
    REAL, INTENT(INOUT) :: snowage    !<help variable for snow; age of snow pack (timesteps)
    REAL, INTENT(INOUT) :: snowdepth  !<current depth of snow (cm)

    !Local variables
    REAL snowdens    !snow density (g/cm3)
    REAL ddens       !change in snow density (g/cm3/timestep)
    REAL snowdens0   !snow density at snowfall, model parameter
    REAL tt          !threshold temperature for snow melt (and evaporation) (C)

    !>\b Algorithm \n
    !>Set parameter values

    !> \b Algorithm \n
    !>Set model parameters
    tt = landpar(m_ttmp,iluse)
    snowdens0 = genpar(m_sndens0)
    
    !>Update snow age with time step and new snow
    IF(snow==0)THEN
      snowage = 0.
    ELSE
      snowage = snowage + 1.
      IF(oldsnow < snow) snowage = snowage * oldsnow / snow     !Assume that melt is drawn from snowfall in first hand
    ENDIF
    
    !>Calculate snow depth, depends of choice of snow density model
    IF(snow<=0.)THEN
      snowdepth  = 0.
    ELSE  
      SELECT CASE(modeloption(p_snowdensity))
      !>\li Case of age depending snow density model (0,default):
      CASE(0)
        snowdens = snowdens0 + snowdensdt * snowage / REAL(timesteps_per_day)
        IF(snowdens>0.) snowdepth  = 0.1 * snow / snowdens     !0.1 cm/mm
      !>\li Case of compacting factor snow density model (1):
      CASE(1)
        snowdepth = snowdepth + snowfall/10./snowdens0  !add snowfall to old snow
        snowdens = (oldsnow+snowfall)/10./snowdepth
        IF(temp>tt)THEN
          ddens = (genpar(m_sdnsrate)+genpar(m_sdnsradd)) * (genpar(m_sdnsmax) - snowdens)  !snow compactation, warm days
        ELSE
          ddens = genpar(m_sdnsrate) * (genpar(m_sdnsmax) - snowdens)  !snow compactation, cold days
        ENDIF
        snowdens = snowdens + ddens
        snowdepth = snow/10./snowdens
      ENDSELECT
    ENDIF

  END SUBROUTINE calculate_snowdepth

  !>Subroutine for calculation of snow heat and temperature processes
  !!
  !> \b Reference ModelDescription Chapter Land routines (Snow routines)
  !------------------------------------------------------------------------
  SUBROUTINE calculate_snowheat_processes(iluse,airtemp,snowmass,snowdepth,snowheat,snowtemp,surftemp,fracmelt,snowliq)
  
    USE MODVAR, ONLY : landpar,     &
                       genpar,      &
                       kice,        &
                       Lfreezing,   &
                       cice,        &
                       seconds_per_timestep
    USE HYPEVARIABLES, ONLY : m_snkika, &
                              m_sndens0
    
    !Argument declarations
    INTEGER, INTENT(IN) :: iluse      !<index of landuse
    REAL, INTENT(IN)    :: airtemp    !<air temperature (C)
    REAL, INTENT(IN)    :: snowmass   !<snow pack (mm) = frozen + liquid water
    REAL, INTENT(INOUT) :: snowliq    !<snow liquid water (mm)
    REAL, INTENT(IN)    :: snowdepth  !<snow depth (cm)
    REAL, INTENT(INOUT) :: snowheat   !<snow heat content (J/m2)
    REAL, INTENT(OUT)   :: snowtemp   !<snow temperature (C)
    REAL, INTENT(OUT)   :: surftemp   !<snow surface temperature (C)
    REAL, INTENT(OUT)   :: fracmelt   !<fraction of timestep with melting conditions (snowtemp=0)
    
    !Local variables
    REAL :: kair                 !<surface heat exchange coefficient (J/timestep/C)
    REAL :: ksnow                !<thermal conductivity of snow (J/m/timestep/C)
    REAL :: snowdensity          !<snow density (kg/m3)
    REAL :: a                    !<term in solution of surface energy balance equation
    REAL :: heatflow             !<heat flow (J/m/subtimestep)
    INTEGER :: i                 !<loop index
    REAL :: snowice              !<frozen snow water equivalent
    REAL :: maxIce               !<Maximum mass of water that can be frozen with current snowheat
    
    !Local parameter
    INTEGER,PARAMETER :: nItt=6 !<Number of sub-timesteps
    
    ! calculate the initial mass of frozen water in snowpack
    snowice = AMAX1(0.,snowmass-snowliq)

    !Skip calculations if there is no snow or now frozen water in the snow
    IF(snowmass.LE.0. .OR. snowice .LE.0.)THEN
      snowheat=0.
      snowtemp=0.
      surftemp=0.
      fracmelt=1.
      RETURN
    ENDIF
    
    !Adjust liquid water content according to initial negative heat content 
    maxIce = - snowheat/Lfreezing*1000.
    IF( maxIce > snowice  .AND. snowliq>0.)THEN
      snowliq = AMIN1(snowmass,AMAX1(0.,snowliq - (maxIce-snowice)))
    ELSE
      IF(maxIce<snowice)THEN
        snowice = AMAX1(0.,maxIce)
        snowliq = AMAX1(0.,snowmass-snowice)
      ENDIF
    ENDIF

    !Calculate snow temperature from snowheat and snow water equivalent (heat content of unfrozen water at 0 degrees is defined as 0)
    IF(snowliq>0.)THEN
      snowtemp = 0.
    ELSE
      snowtemp=AMIN1(0.,snowheat + Lfreezing*1000.*snowice)/(cice*1000.*snowice)
    ENDIF    
    
    !Continue to solve surface heat balance only if snowtemp or air temp is below zero - otherwise return that snowpack is ready for snowmelt
    IF(snowtemp.LT.0. .OR. airtemp.LT.0.)THEN
      !Snow density
      IF(snowdepth.GT.0.)THEN
        snowdensity=snowmass/(snowdepth*0.01)
      ELSE
        snowdensity=genpar(m_sndens0)
      ENDIF
      IF(snowdensity.LE.0.) snowdensity=100.
        
      !Thermal conductivity of snow and surface heat exchange coefficient
      ksnow = snow_thermal_conductivityfunction(snowdensity) * seconds_per_timestep !J/m/timestep  
      kair  = kice * seconds_per_timestep / landpar(m_snkika,iluse)
        
      !Term in solution of surface energy balance equation
      a = snowdepth*0.01*0.5*kair/ksnow
        
      !Loop over sub-timesteps solving surface heat balance and updating heat content
      fracmelt=0.
      DO i=1,nItt
        IF(snowtemp.GE.0.)THEN
          fracmelt=fracmelt+1./REAL(nItt)
        ENDIF
        surftemp=AMIN1(0.,(a*airtemp+snowtemp)/(1.+a))
        heatflow=kair*(airtemp-surftemp)/REAL(nItt)
        snowheat=AMIN1(-snowmass*Lfreezing*1000.,snowheat+heatflow)
        snowtemp=AMIN1(0.,(snowheat+Lfreezing*snowmass*1000.))/(cice*1000.*snowmass)
      ENDDO
    ELSE
      surftemp=0.
      fracmelt=1.
    ENDIF
    
 END SUBROUTINE calculate_snowheat_processes

  !>Calculate snow thermal conductivity as a function of snow density
  !------------------------------------------------------------------
  REAL FUNCTION snow_thermal_conductivityfunction(snowdensity)
  
    USE MODVAR, ONLY: icedensity, kice
    
    !Argument decalaration
    REAL, INTENT(IN) :: snowdensity   !<snow density (kg/m3)
    
    !Local parameter
    REAL, PARAMETER :: kexp = 1.885 !<exponent in the CRREL 81-10 function (1.885)
    
    !Snow thermal conductivity (W/m/K) - CRREL report 81-10
    snow_thermal_conductivityfunction = kice * (snowdensity/icedensity)**kexp
    
  END FUNCTION snow_thermal_conductivityfunction

  !>Subroutine for calculation of glacier melt by different methods
  !!
  !> \b Reference ModelDescription Chapter Land routines (Glaciers)
  !------------------------------------------------------------------------
  SUBROUTINE calculate_glacier_melt(iluse,gtype,temp,swrad,epot,area,glacvol,snowage,snowcov,melt,epotglac,evap)
  
    USE MODVAR, ONLY : landpar, &
                       genpar, &
                       modeloption, &
                       p_snowmelt, &
                       p_snowevap, &
                       timesteps_per_day
    USE HYPEVARIABLES, ONLY : m_glacttmp,m_glaccmlt,  &
                              m_snalbmin,m_snalbmax,m_snalbkexp,  &
                              m_glaccmrad,m_glaccmrefr, &
                              m_fepotglac,m_glacdens, &
                              m_glacalb
    
    !Argument declarations
    INTEGER, INTENT(IN) :: iluse      !<index of landuse
    INTEGER, INTENT(IN) :: gtype      !<type of glacier (0=default,1=icecap,2=ice sheet,3=infinit)
    REAL, INTENT(IN)    :: temp       !<air temperature (C)
    REAL, INTENT(IN)    :: swrad      !<shortwave radiation (MJ/m2/day?)
    REAL, INTENT(IN)    :: epot       !<potential evaporation (mm/timestep)
    REAL, INTENT(IN)    :: area       !<glacier area (m2)
    REAL, INTENT(IN)    :: glacvol    !<glacier volume (m3)
    REAL, INTENT(IN)    :: snowage    !<age of snow (timesteps)
    REAL, INTENT(IN)    :: snowcov    !<covarage of snow (fraction)
    REAL, INTENT(OUT)   :: melt       !<glacier melt (mm/timestep)
    REAL, INTENT(OUT)   :: epotglac   !<glacier potential evaporation (mm/timestep)
    REAL, INTENT(OUT)   :: evap       !<glacier evaporation (mm/timestep)
    
    !Local variables
    REAL tt       !threshold temperature for glacier melt (and evaporation) (C)
    REAL cm       !coefficient for glacier melt (mm/C/timestep)
    REAL snalbmax,snalbmin,snalbkexp
    REAL cmrad    !radiation index glacier melt factor (mm/MJ/timestep)
    REAL glacieralbedo
    REAL meltmax  !water of glacier (mm)
    REAL abla,abla0

    !>\b Algorithm \n
    !>Set parameter values and default output
    tt = genpar(m_glacttmp)
    cm = genpar(m_glaccmlt)
    cmrad = genpar(m_glaccmrad)
    melt = 0
    epotglac = 0
    evap = 0
    
    IF(glacvol>0. .OR. gtype==3)THEN
      SELECT CASE(modeloption(p_snowmelt))
      CASE(2) ! Temperature And Radiation Index model
        !Radiation component
        IF(swrad>0.)THEN
          snalbmin  = landpar(m_snalbmin,iluse)
          snalbmax  = landpar(m_snalbmax,iluse)
          snalbkexp = landpar(m_snalbkexp,iluse)
          !Get glacier albedo weighted average between snow-albedo and glacier albedo, 
          !assuming similar snow cover area on glacier as on snow plain           
          glacieralbedo = snowalbedo_function(snowage*timesteps_per_day,snalbmin,snalbmax,snalbkexp) &
                           * snowcov + genpar(m_glacalb) * (1.-snowcov)
          melt = cmrad * swrad * (1.-glacieralbedo)
        ENDIF
        !Add Temperature component
        IF(temp>=tt) THEN  
          melt = melt + cm * (temp - tt)
        ENDIF
        !Remove refreezing component from melt
        IF(temp < tt .AND. melt > 0) THEN  
          melt = MAX(0.,melt - genpar(m_glaccmrefr) * cm * (tt - temp))
        ENDIF 
      
      CASE DEFAULT ! Original Temperature Index Model
        IF(temp>=tt) THEN  
          melt = cm * (temp - tt)
        ENDIF
      END SELECT
    ENDIF
    
    !Glacier evaporation (sublimation)
    IF(modeloption(p_snowevap).GE.1)THEN
      IF(glacvol>0. .OR. gtype==3)THEN
        epotglac  = epot * genpar(m_fepotglac)
        evap = epotglac
      ENDIF
    ENDIF
 
    IF(glacvol>0. .AND. gtype/=3)THEN
      !Check maximum ablation = glacier melt + sublimation
      meltmax = 0.
      IF(area>0.) meltmax = glacvol/area*genpar(m_glacdens)*1000.
      abla0 = melt + evap            !potential ablation (melt+evap)
      abla = MIN(abla0, meltmax)
      IF(abla0.GT.abla)THEN
        melt = melt * abla/abla0    !distribute ablation on melt and evap
        evap = evap * abla/abla0
      ENDIF
    ENDIF
    
  END SUBROUTINE calculate_glacier_melt
  
  !>Calculate air pressure (kPa) as a function of elevation, FAO(7)
  !-------------------------------------------------------------------------------
  REAL FUNCTION airpressure_elevationfunction(elev)
     
    !Argument decalaration
    REAL, INTENT(IN) :: elev   !<elevation
     
    airpressure_elevationfunction = 101.3 * ((293. - 0.0065 * elev)/293. ) ** 5.26
     
  END FUNCTION airpressure_elevationfunction
  
  !>Calculate latent heat of vaporization (MJ kg-1) as a function of temperature
  !-------------------------------------------------------------------------------
  REAL FUNCTION latentheat_tempfunction(temp)

    !Argument decalaration
    REAL, INTENT(IN) :: temp !<temperature (C)
     
    latentheat_tempfunction = 2.501 - 0.002361 * temp  ![MJ/kg]
     
  END FUNCTION latentheat_tempfunction
  
  !> Calculate psychrometric constant (kPa C^-1) as a function of
  !! air pressure and latent heat of vaporization, FAO
  !-------------------------------------------------------------------------------
  REAL FUNCTION psychrometric_constant(airpressure,lambda)
  
    !Argument declarations
    REAL, INTENT(IN) :: airpressure !<air pressure [kPa]
    REAL, INTENT(IN) :: lambda      !<latent heat of vaporization [MJ/kg]
    
    !Parameter declaration
    REAL, PARAMETER :: cp = 0.001013  !specific heat of moist air at constant pressure (MJ kg^-1 C^-1)
 
    psychrometric_constant = cp * airpressure / (0.622 * lambda)
 
  END FUNCTION psychrometric_constant

  !>Calculates potential evaporation or uses a value supplied as input
  !TODO: Why is this in soil_proc. Move to atm_proc?
  !
  !> \b Reference ModelDescription Processes above ground (Evaporation)
  !--------------------------------------------------------------
  SUBROUTINE calculate_potential_evaporation(i,j,temp,epot,radext,swrad,netrad,actvap,satvap,wind,epotsnow)
  
    USE MODVAR, ONLY : basin,classdata, &
                       landpar, &
                       genpar, &
                       dayno, &
                       pi, &
                       xobsi,xobsindex, &
                       classbasin, &
                       tsofday,timesteps_per_day
    USE HYPEVARIABLES, ONLY : o_reepot, &
                              m_ttmp,m_cevp, &
                              basincevpam, &
                              basincevpph, &
                              m_kc, &
                              m_krs,m_jhtadd,m_jhtscale,m_alfapt,m_fepotsnow

    !Argument declarations
    INTEGER, INTENT(IN) :: i      !<index of current subbasin
    INTEGER, INTENT(IN) :: j      !<index of current class 
    REAL, INTENT(IN)    :: temp   !<air temperature
    REAL, INTENT(OUT)   :: epot   !<potential evapotranspiration [mm/timestep]
    REAL, INTENT(IN)    :: radext !<extraterrestrial solar radiation [MJ/m2/day]
    REAL, INTENT(IN)    :: swrad  !<downward shortwave radiation [MJ/m2/day]
    REAL, INTENT(IN)    :: netrad !<net downward radiation [MJ/m2/day]
    REAL, INTENT(IN)    :: actvap !<actual vapor pressure [kPa]
    REAL, INTENT(IN)    :: satvap !<saturated vapour pressure [kPa]
    REAL, INTENT(IN)    :: wind   !<wind speed [m/s]
    REAL, INTENT(OUT)   :: epotsnow !<potential evapotranspiration for snow mm/timestep
    
    !Local variables
    REAL tt       !threshold temperature for melting (C)
    REAL ce       !coefficient for evaporation (mm/C/timestep)
    REAL dsatvap  !Slope of saturation pressure curve [kPa/C]
    REAL gamma    !psychrometric constant
    REAL lambda   !latent heat of evaporation [MJ/kg]
    REAL airpressure  !atmospheric pressure [kPa]
    REAL kc       !crop coefficient used for the new PET functions
    REAL elev     !elevation
    REAL turbidity ! atmospheric turbidity
    REAL fepotsnow ! fraction of potential evaporation used for snow
    INTEGER current_petmodel
    
    !>\b Algorithm \n
    !>Set local parameters and corrections
    tt = landpar(m_ttmp,classdata(j)%luse)       !Threshold temperature for snow melt and evaporation
    ce = landpar(m_cevp,classdata(j)%luse)       !Coefficient for potential evaporation
    fepotsnow = landpar(m_fepotsnow,classdata(j)%luse)       !Coefficient for potential evaporation for snow
    ce = ce * (1 + basincevpam(i)*SIN(2.*pi*(dayno-1+REAL(tsofday)/REAL(timesteps_per_day)-basincevpph(i))/365.))
    
    !>Calculate additional input variables for the alternative PET functions
    current_petmodel = get_current_petmodel(i)
    IF(current_petmodel.GT.1)THEN
      dsatvap = deltasaturationpressure_function(temp)  !Slope of saturated vapour pressure curve, using mean temperature
      lambda = latentheat_tempfunction(temp)  !Latent heat of vaporization
      elev = basin(i)%elev+classbasin(i,j)%deltah
      airpressure = airpressure_elevationfunction(elev)  !Air pressure, assuming normal pressure at sea level
      gamma = psychrometric_constant(airpressure,lambda) !Psychrometric constant
      kc = landpar(m_kc(current_petmodel),classdata(j)%luse)  !PET-model specific Landuse scaling parameter, "crop coefficient"
      IF(kc==0) kc = landpar(m_kc(1),classdata(j)%luse)  !Default Landuse scaling parameter, "crop coefficient"
      IF(radext>0.)THEN
        turbidity = swrad / radext
      ELSE
        turbidity = 1.  !any value, PET(3) will be zero due to radext=0
      ENDIF
    ENDIF
      
    !>Calculate potential evaporation with the selected petmodel
    epot = 0.
    SELECT CASE(current_petmodel)
      CASE(0) !HYPE original model (with Xobs replacement, if available)
        IF(xobsindex(o_reepot,i)>0)THEN       
          epot = xobsi(xobsindex(o_reepot,i))
        ELSEIF(temp>tt)THEN
          epot = ce*(temp-tt)
        ELSE
          epot = 0.  
        ENDIF
      CASE(1) !HYPE original model (without Xobs replacement)
        IF(temp>tt)THEN
          epot = ce*(temp-tt)
        ELSE
          epot = 0.  
        ENDIF
      CASE(2) !Modified Jensen-Haise/McGuinness following Oudin et al (2005)
        !parameters suggested by Oudin et al, jhtadd = 5, jhtscale = 100
        epot = kc * MAX(0.,radext / (lambda) * (temp + genpar(m_jhtadd)) / genpar(m_jhtscale))
      CASE(3) !Hargreaves-Samani (known to overpredict in humid areas)
        ! The function is modified by DG to limit the "turbidity-factor" with the Ångström formula:
        ! 
        !   The original Hargreaves function is:
        !     epot = 0.0023 * radext / (lambda*rho) * (Tmax-Tmin)^0.5 * (temp + 17.8)
        !   and the Hargreaves turbidity for estimating swrad = krs * (Tmax-Tmin)^0.5
        !
        !   Thus, by replacing (Tmax-Tmin)^2 with turbidity/krs, we get a reasonable limitation of the Hargreaves (tmax-Tmin) impact
        !  (furthermore, if Tmax-min was missing, we actually use the clearsky turbidity at this point)
        !
        ! also note that rho = 1 and excluded in equations below...
        epot = max(0.,kc * 0.0023 * radext /(lambda) * turbidity / genpar(m_krs) * (temp+17.8))
      CASE(4) ! Priestly Taylor (known to underpredict in arid and semi-arid areas)
        epot = max(0.,kc * genpar(m_alfapt) * dsatvap * netrad / (lambda * (dsatvap+gamma)))
      CASE(5) ! FAO Penman Monteith reference crop evapotranspiration
        epot = max(0., kc * ((0.408 * dsatvap * netrad + gamma*900./(temp+273.)*wind*(satvap-actvap))/(dsatvap+gamma*(1.+0.34*wind))))
      END SELECT
      !>Calculate potential evaporation for snow evaporation (sublimation)
      epotsnow = fepotsnow * epot

  END SUBROUTINE calculate_potential_evaporation

  !>\brief Get the pet model for the current subbasin
  !--------------------------------------------------------------------
  INTEGER FUNCTION get_current_petmodel(i)

    USE MODVAR, ONLY : conductbasinpetmodel, &
                       petmodel, &
                       modeloption, &
                       p_petmodel
    
    !Argument declarations
    INTEGER, INTENT(IN) :: i  !<current subbasin
    
    IF(conductbasinpetmodel)THEN
      get_current_petmodel = petmodel(i)
    ELSE
      get_current_petmodel = modeloption(p_petmodel)
    ENDIF
    
  END FUNCTION get_current_petmodel
  
  !>\brief Calculate and set concentration of evaporating water
  !
  !> \b Reference ModelDescription Chapter Processes above ground (Evaporation)
  !--------------------------------------------------------------------
  SUBROUTINE set_evaporation_concentrations(conc,cevap)
  
    USE MODVAR, ONLY : numsubstances, &
                       genpar, &
                       simulate, &
                       i_t1,i_t2
    USE HYPEVARIABLES, ONLY : m_T1evap

    !Argument declarations
    REAL, INTENT(IN)  :: conc(numsubstances)     !<concentration in water body (?)
    REAL, INTENT(OUT) :: cevap(numsubstances)    !<concentration in evapotranspiration (?)

    cevap = 0.
    IF(numsubstances == 0) RETURN
    IF(simulate%substance(i_t1)) cevap(i_t1) = genpar(m_T1evap) * conc(i_t1)
    IF(simulate%substance(i_t2)) cevap(i_t2) = conc(i_t2)

  END SUBROUTINE set_evaporation_concentrations
  
  !>\brief Calculate and remove evapotranspiration from the soil upper
  !>two layers
  !
  !> \b Reference ModelDescription Chapter Processes above ground (Evaporation)
  !--------------------------------------------------------------------
  SUBROUTINE calculate_actual_soil_evapotranspiration(i,j,maxevaplayers,water,temp,epot,wp,fc,  &
                                      epotfrac,barefrac,liqfrac,soilstate,evap,evapflows,cevap)

    USE MODVAR, ONLY : basin,classdata, &
                       landpar,  &
                       numsubstances, &
                       maxsoillayers, &
                       soilthick, &
                       realzero
    USE HYPEVARIABLES, ONLY : basinlp, &
                              m_ttmp,  &
                              m_T1evap, &
                              m_ttrig,m_tredA,m_tredB

    !Argument declarations
    INTEGER, INTENT(IN) :: i                        !<index of current subbasin
    INTEGER, INTENT(IN) :: j                        !<index of current class
    INTEGER, INTENT(IN) :: maxevaplayers            !<max number of soil layers with evaporation (2)
    REAL, INTENT(IN)    :: water(maxsoillayers)     !<soil water (mm) (to base evapotranspiration calculation on)
    REAL, INTENT(IN)    :: temp                     !<air temperature
    REAL, INTENT(IN)    :: epot                     !<potential evapotranspiration (mm/timestep)
    REAL, INTENT(IN)    :: wp(maxsoillayers)        !<wilting point (mm)
    REAL, INTENT(IN)    :: fc(maxsoillayers)        !<field capacity (mm)
    REAL, INTENT(IN)    :: epotfrac(maxevaplayers)  !<relative distribution of potential evaporation between upper two soil layers (-)
    REAL, INTENT(IN)    :: barefrac                 !<fraction of soil that has evapotranspiration (-)
    REAL, INTENT(IN)    :: liqfrac(maxsoillayers)   !<fraction of liquid water in soil
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate !<Soil states
    REAL, INTENT(OUT)   :: evap                     !<actual evapotranspiration (mm/timestep)
    REAL, INTENT(OUT)   :: evapflows(maxevaplayers) !<actual evapotranspiration (mm/timestep)
    REAL, INTENT(OUT)   :: cevap(numsubstances)     !<concentration in evapotranspiration (?)

    !Local variables
    INTEGER k   !loop-variable
    INTEGER status  !error status of subroutine
    REAL efwater(maxevaplayers)  !water above wilting point of soillayer (mm)
    REAL evapsl(maxevaplayers)  !evapotranspiration of soillayer (mm/timestep)
    REAL ctemp(numsubstances),cevapsl(numsubstances,maxevaplayers)  !concentration of evapotranspiration
    REAL soiltemp_factor(maxsoillayers),soiltemp

    !>\b Algorithm \n
    !Default values output variables
    evap = 0.
    cevap = 0.
    evapflows = 0.
    IF(epot<=0.)RETURN

    !>Calculate soil temperature reduction
    soiltemp_factor = 1.
    IF(landpar(m_tredA,classdata(j)%luse).GT.0.)THEN
      DO k=1,2
        IF(soilthick(k,j)>0)THEN
          soiltemp = soilstate%temp(k,j,i)
          IF(soiltemp.GT.landpar(m_ttrig,classdata(j)%luse))THEN
            soiltemp_factor(k) = 1.-EXP(-landpar(m_tredA,classdata(j)%luse) * (soiltemp-landpar(m_ttrig,classdata(j)%luse))**landpar(m_tredB,classdata(j)%luse))
          ELSE
            soiltemp_factor(k) = 0.
          ENDIF
        ENDIF
      ENDDO
    ENDIF

    !>If temperature above threshold:
    IF(temp>landpar(m_ttmp,classdata(j)%luse))THEN
      evapsl = 0.; cevapsl = 0.
      
      DO k = 1,maxevaplayers
        IF(soilthick(k,j)>0.)THEN
          !>\li calculate actual evapotranspiration
          efwater(k) = water(k)-wp(k)
          IF(efwater(k) <= 0.0) CYCLE   !No water for evapotranspiration in this soil layer
          evapsl(k) = epot*epotfrac(k)*soiltemp_factor(k)
          IF(efwater(k) < basinlp(i)*fc(k)) evapsl(k) = evapsl(k)*efwater(k)/(basinlp(i) * fc(k))
          
          IF(evapsl(k)+realzero>soilstate%water(k,j,i)-wp(k)) evapsl(k) = soilstate%water(k,j,i)-wp(k)  !Check available water
          evapsl(k) = evapsl(k) * MIN(1.,barefrac)  !Scale evapotranspiration with fraction of bare soil
          evapsl(k) = evapsl(k) * liqfrac(k)        !Scale evapotranspiration with liquid water fraction (as long as we dont separate soil evaporation from transpiration, it's less straight forward to calculate sublimation from frozen soil)
          
          IF(evapsl(k)>0.)THEN
            !>\li Remove evapotranspiration from soil water
            ctemp = 0.
            IF(numsubstances>0) CALL set_evaporation_concentrations(soilstate%conc(:,k,j,i),ctemp)
            IF(evapsl(k)+realzero<soilstate%water(k,j,i))THEN
              CALL remove_water(soilstate%water(k,j,i),numsubstances,soilstate%conc(:,k,j,i),evapsl(k),ctemp,status)
              IF(status.NE.0) CALL error_remove_water(errstring(6+k),basin(i)%subid,i,j)
              IF(numsubstances>0) cevapsl(:,k) = ctemp
            ELSE
              soilstate%water(k,j,i) = 0.
              IF(numsubstances>0) soilstate%conc(:,k,j,i) = 0.    !remove last traces, safe for wp=0
            ENDIF
          ENDIF
        ENDIF
      ENDDO

      !>Set output variables
      evap = SUM(evapsl(:))
      evapflows = evapsl
      IF(evap>0. .AND. numsubstances>0)THEN
        DO k = 1,maxevaplayers
          cevap(:) = cevap(:) + cevapsl(:,k)*evapsl(k)
        ENDDO
        cevap(:) = cevap(:)/evap
      ENDIF
    ENDIF

  END SUBROUTINE calculate_actual_soil_evapotranspiration

  !>\brief Drainage level runoff: tile or drainage pipe
  !!
  !> \b Reference ModelDescription Chapter Land routines (Soil water - Runoff through drainage pipes)
  !------------------------------------------------------------------
  SUBROUTINE calculate_tile_drainage(i,j,isoil,subid,water,ep_org,&
       sdepth,sthick,tdepth,rrcscorr,liqfrac,soilstate,runoffd,crunoffd,cweights)

    USE MODVAR, ONLY : soilpar,  &
                       numsubstances,   &
                       maxsoillayers
    USE HYPEVARIABLES, ONLY : m_trrcs,m_fzsexpand

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<index of current subbasin
    INTEGER, INTENT(IN) :: j        !<index of current class
    INTEGER, INTENT(IN) :: isoil    !<soil type index
    INTEGER, INTENT(IN) :: subid    !<subbasin id
    REAL, INTENT(IN)    :: water(maxsoillayers) !<soil water (mm) (to base runoff calculation on)
    REAL, INTENT(IN)    :: ep_org(maxsoillayers) !<effective porosity volume (mm)
    REAL, INTENT(IN)    :: sdepth(maxsoillayers) !<Lower border of soil layers (m)
    REAL, INTENT(IN)    :: sthick(maxsoillayers) !<Thickness of soil layers (m)
    REAL, INTENT(IN)    :: tdepth                !<Tiledepth (m)
    REAL, INTENT(IN)    :: rrcscorr    !<correction of recession coefficients
    REAL, INTENT(IN)    :: liqfrac(maxsoillayers)   !<fraction of liquid water in soil (-)
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate !<Soil states
    REAL, INTENT(OUT)   :: runoffd                    !<runoff
    REAL, INTENT(OUT)   :: crunoffd(numsubstances)    !<concentration of runoff 
    REAL, INTENT(OUT)   :: cweights(maxsoillayers)    !<weights for calc. drain.conc from layer.conc (zero or one)
    
    !Local parameter
    REAL, PARAMETER :: mindiff = 0.000002    !10^-6 verkar vara en onogrannhetsgräns

    !Local variables 
    INTEGER status    !error status of subroutine call
    INTEGER k,k2   !index of soil layer
    REAL deltah    !groundwater level above tile drainage pipe level (m)
    REAL avail(maxsoillayers)    !water available for runoff (mm)
    REAL epwater(maxsoillayers)  !water in ep to calculate runoff on (mm)
    REAL ep(maxsoillayers)  !unfrozen part of ep pores (mm)
    REAL trc       !coefficient for runoff recession tile flow (fraction per timestep)

    !>\b Algorithm \n
    !>Set default output values
    runoffd = 0.
    crunoffd = 0.
    cweights(:) = 0.

    IF(tdepth<=0.) RETURN   !no tile drainage

    !>Set local parameters
    trc = soilpar(m_trrcs,isoil)*rrcscorr       !Runoff coefficient for tile runoff
    IF(trc<=0.) RETURN
    IF(trc>1.) trc = 1.
    !>Calculate available water for runoff from current state of soil water and 
    !>individual "pressure level" from earlier state.
    !>If part of the soil water is frozen, we try to take expansion of frozen water
    !>into account as an increase in the pressure level
    DO k=1,maxsoillayers
      ep(k) = liqfrac(k)*ep_org(k)
      epwater(k) = liqfrac(k)*water_above_field_capacity(k,j,water(k)*(liqfrac(k)+(1.+soilpar(m_fzsexpand,isoil))*(1.-liqfrac(k)))) !mm
      avail(k) = liqfrac(k)*MIN(water_above_field_capacity(k,j,soilstate%water(k,j,i)*(liqfrac(k)+(1.+soilpar(m_fzsexpand,isoil))*(1.-liqfrac(k)))),water_above_wilting_point(k,j,soilstate%water(k,j,i))) !mm
    ENDDO

    !>Depending on depth of tile drainage pipe calculate:
    DO k=1,maxsoillayers
      IF(tdepth<=sdepth(k))THEN       !Drainage pipe in this soil layer
        deltah = epwater(k)/ep(k) * sthick(k) - (sdepth(k) - tdepth)  !m
        DO k2 = k-1,1,-1
          IF(epwater(k2+1)-ep(k2+1)>=0.-mindiff*ep(k2+1))THEN
            IF(epwater(k2)>0.) deltah = deltah + epwater(k2)/ep(k2) * sthick(k2)
          ELSE
            EXIT
          ENDIF
        ENDDO
        IF(deltah>0.)THEN
          runoffd = trc * deltah / sthick(k) * ep(k)
          IF(runoffd > avail(k)) runoffd = MAX(avail(k),0.)
          IF(numsubstances>0) crunoffd(:)=soilstate%conc(:,k,j,i)
          CALL remove_water(soilstate%water(k,j,i),numsubstances,soilstate%conc(:,k,j,i),runoffd,crunoffd,status) 
          IF(status.NE.0) CALL error_remove_water(errstring(k),subid,i,j)
          cweights(k) = 1.
        ENDIF
        EXIT  !Drainage pipe layer found, skip checking the other layers
      ENDIF
    ENDDO

  END SUBROUTINE calculate_tile_drainage

  !>\brief Runoff from soil layers, down to drainage depth ("streamdepth").
  !>
  !>\b Reference ModelDescription Chapter Land routines (Soil water - Groundwater runoff)
  !--------------------------------------------------------------------------------
  SUBROUTINE calculate_soil_runoff(i,j,isoil,subid,water,ep_org,ddepth,liqfrac,soilstate,soilrunoff,csoilrunoff)

    USE MODVAR, ONLY : numsubstances, &
                       maxsoillayers, &
                       soildepth, &
                       soilthick, &
                       soilpar
    USE HYPEVARIABLES, ONLY : m_fzsexpand

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<index of current subbasin
    INTEGER, INTENT(IN) :: j        !<index of current class
    INTEGER, INTENT(IN) :: isoil    !<index of soil type
    INTEGER, INTENT(IN) :: subid    !<subbasin id
    REAL, INTENT(IN)    :: water(maxsoillayers) !<soil water (mm) (to base runoff calculation on)
    REAL, INTENT(IN)    :: ep_org(maxsoillayers) !<effective porosity volume (mm)
    REAL, INTENT(IN)    :: ddepth         !<drainage depth (m), "streamdepth"
    REAL, INTENT(IN)    :: liqfrac(maxsoillayers)   !<fraction of liquid water in soil (-)
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    REAL, INTENT(OUT)   :: soilrunoff(maxsoillayers)  !<runoff (mm/ts)
    REAL, INTENT(OUT)   :: csoilrunoff(numsubstances,maxsoillayers)    !<concentration of runoff
    
    !Local variables
    INTEGER k,k2    !soil layer (1-3)
    INTEGER status    !error status of subroutine call
    LOGICAL includedd
    REAL delta(maxsoillayers)    !"groundwater level" in soil layer above bottom of same soil layer (m)
    REAL deltah    !groundwater level above soil layer drainage level (bottom of soil layer or stream depth) (m)
    REAL avail(maxsoillayers)    !water available for runoff (mm)
    REAL epwater(maxsoillayers)  !water in ep to calculate runoff on (mm)
    REAL ep(maxsoillayers)  !unfrozen part of ep pores (mm)
    REAL crunoff(numsubstances)
    
    !Local parameter
    REAL, PARAMETER :: mindiff = 0.000002    !10^-6 verkar vara en onogrannhetsgräns

    !>\b Algorithm \n
    !>Initialise soil runoff to zero.
    !>Soil runoff is calculated only if stream is below soil surface
    soilrunoff = 0.
    csoilrunoff = 0.
    IF(ddepth<=0.) RETURN
    
    !>Calculate available water for runoff from current state of soil water and 
    !>individual "pressure level" from earlier state.
    !If part of the soil water is frozen, we try to take expansion of frozen water
    !into account as an increase in the pressure level (groundwater level) of the 
    !remaining unfrozen water above field capacity, and we make unfrozen water
    !between field capacity and wilting point available for runoff.
    delta = 0.  !where ep or soilthick is zero
    DO k=1,maxsoillayers
      ep(k) = ep_org(k)*liqfrac(k)
      epwater(k) = liqfrac(k)*water_above_field_capacity(k,j,water(k)*(liqfrac(k)+(1.+soilpar(m_fzsexpand,isoil))*(1.-liqfrac(k)))) !mm
      avail(k) = liqfrac(k)*MIN(water_above_field_capacity(k,j,soilstate%water(k,j,i)*(liqfrac(k)+(1.+soilpar(m_fzsexpand,isoil))*(1.-liqfrac(k)))),water_above_wilting_point(k,j,soilstate%water(k,j,i))) !mm
      IF(ep(k)>0.) delta(k) = epwater(k)/ep(k)*soilthick(k,j)   !m
    ENDDO

    !>For each soil layer:
    DO k=1,maxsoillayers
      IF(soilthick(k,j)>0.)THEN
        IF(ddepth<soildepth(k,j)-soilthick(k,j)) CYCLE  !No runoff from this soillayer, strange parameters!
        !>\li Calculate initial pressure level; soillayer water and distance to drainage depth
        deltah = delta(k)
        includedd = .FALSE.
        IF(ddepth<=soildepth(k,j)) includedd = .TRUE.
        IF(k<maxsoillayers)THEN
          IF(soilthick(k+1,j)==0.) includedd = .TRUE.
        ELSE
          includedd = .TRUE.
        ENDIF
        IF(includedd) deltah = deltah + (ddepth - soildepth(k,j))
        !>\li Add effect of above layers for saturated soil
        DO k2=k-1,1,-1
          IF(epwater(k2+1)-ep(k2+1)>=0.-mindiff*ep(k2+1))THEN   !if frozen water this will never be fulfilled
            deltah = deltah + delta(k2)
          ELSE
            EXIT
          ENDIF
        ENDDO
        !>\li Calculate runoff from soil layer
        CALL calculate_pressurelevel_soillayer_runoff(i,j,k,deltah,avail(k),ep(k),soilthick(k,j),soilstate,soilrunoff(k),crunoff,status)
        IF(status.NE.0) CALL error_remove_water(errstring(3+k),subid,i,j)
        IF(numsubstances>0) csoilrunoff(:,k) = crunoff
      ENDIF
    ENDDO

  END SUBROUTINE calculate_soil_runoff

  !>\brief Calculate and remove soil layer runoff from a soil layer
  !!
  !>\b Reference ModelDescription Chapter Land routines (Soil water - Groundwater runoff)
  !--------------------------------------------------------------------------------
  SUBROUTINE calculate_pressurelevel_soillayer_runoff(i,j,sl,plevel,water,ep,sthick,soilstate,runoff,crunoff,status)

    USE MODVAR, ONLY : numsubstances,   &
                       maxsoillayers
    USE HYPEVARIABLES, ONLY : soilrc

    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<index of current subbasin
    INTEGER, INTENT(IN) :: j        !<index of current class
    INTEGER, INTENT(IN) :: sl       !<soil layer
    REAL, INTENT(IN)    :: plevel   !<pressure level (m)
    REAL, INTENT(IN)    :: water    !<water available for runoff (mm)
    REAL, INTENT(IN)    :: ep       !<effective porosity volume (mm)
    REAL, INTENT(IN)    :: sthick   !<Thickness of soil layer (m)
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    REAL, INTENT(OUT) :: runoff     !<runoff
    REAL, INTENT(OUT) :: crunoff(numsubstances)    !<concentration of runoff
    INTEGER, INTENT(OUT) :: status  !<error status
    
    status = 0
    runoff = 0.
    crunoff = 0.
    IF(water<=0.) RETURN  !no water available for runoff
    IF(plevel<=0.) RETURN !no runoff
    IF(sthick<=0.) RETURN !div. by zero
    runoff = soilrc(sl,j,i) * plevel / sthick * ep
    IF(runoff>=water) runoff = water
    IF(numsubstances>0) crunoff = soilstate%conc(:,sl,j,i)
    CALL remove_water(soilstate%water(sl,j,i),numsubstances,soilstate%conc(:,sl,j,i),runoff,crunoff,status) 

  END SUBROUTINE calculate_pressurelevel_soillayer_runoff

  !>\brief Calculate infiltration to soil and surface flow and macropore flow 
  !>due to limited infiltration capacity. Several formulations as options; based 
  !>soil water threshold, soil moisture, or soil moisture and rain(+melt).
  !>
  !>\b Reference ModelDescription Chapter Land routines (Soil water - 
  !> Diversion of surface runoff and macropore flow, Infiltration)
  !-----------------------------------------------------------------------------------
  SUBROUTINE calculate_infiltration_flow_diversion(i,j,isoil,wp,fc,ep,ginfilt,cginfilt,temp,tmin,tmax,  &    !TODO: change name of subroutine
       infilt,cinfilt,surfaceflow,csurfaceflow,macroflow,cmacroflow,frozenstate,soilstate)

    USE MODVAR, ONLY : conduct, &
                       maxsoillayers, &
                       missing_value, &
                       modeloption, &
                       numsubstances, &
                       p_infiltration, &
                       p_surfacerunoff, &
                       genpar,soilpar, &
                       timesteps_per_day
    USE HYPEVARIABLES, ONLY : m_macrate,m_mactrinf,m_mactrsm, &
                              m_srrate,m_bfroznsoil, &
                              m_macfrac,m_srbeta,m_sralpha,m_srgamma,m_srnlayer

    !Argument declaration
    INTEGER, INTENT(IN) :: i                  !<index of current subbasin
    INTEGER, INTENT(IN) :: j                  !<index of current class 
    INTEGER, INTENT(IN) :: isoil              !<index of soil type
    REAL, INTENT(IN)    :: wp(maxsoillayers)  !<wilting point volume (mm)
    REAL, INTENT(IN)    :: fc(maxsoillayers)  !<"field capacity" volume (mm) (water available for evaporation but not for runoff)
    REAL, INTENT(IN)    :: ep(maxsoillayers)  !<effective porosity volume (mm) (water available for runoff)
    REAL, INTENT(IN)    :: ginfilt            !<gross infiltration; rain+snowmelt (mm/timestep)
    REAL, INTENT(IN)    :: cginfilt(numsubstances)      !<concentration of gross infiltration
    REAL, INTENT(IN)    :: temp            !<current air temperature (degree Celsius)
    REAL, INTENT(IN)    :: tmin            !<current daily minimum air temperature (degree Celsius)
    REAL, INTENT(IN)    :: tmax            !<current daily maximum air temperature (degree Celsius)
    REAL, INTENT(OUT)   :: infilt             !<infiltration (mm/timestep)
    REAL, INTENT(OUT)   :: cinfilt(numsubstances)       !<concentration of infiltration
    REAL, INTENT(OUT)   :: surfaceflow        !<surface runoff due to limited infiltration capacity (mm/timestep)
    REAL, INTENT(OUT)   :: csurfaceflow(numsubstances)  !<concentration of surface flow
    REAL, INTENT(OUT)   :: macroflow          !<macropore flow (mm/timestep)
    REAL, INTENT(OUT)   :: cmacroflow(numsubstances)    !<concentration of macropore flow
    TYPE(snowicestatetype),INTENT(IN) :: frozenstate !<Snow and ice states
    TYPE(soilstatetype),INTENT(INOUT) :: soilstate   !<Soil states
    
    !Local variables
    REAL help,avail
    REAL macrate,inflowtres,srrate   !infiltration excess runoff and macropore flow parameters
    REAL smtresmm
    REAL macfraction,srfraction   !relative fraction of macroporeflow and surfacerunoff
    REAL beta,srratio,S,Smax      !soil moisture dependent surfacerunoff parameters
    INTEGER srnlayer
    REAL tmin_check, tmax_check   !temperature for checking for ice lens
    REAL potinfilt
    REAL cuminfilt  !cumulative infiltration from snow melt during rest of season according to Zhao and Gray [mm]
    REAL t0         !time for this infiltration, opportunity time [hours]

    !>\b Algorithm \n
    !>Set default output values
    infilt       = ginfilt
    cinfilt      = cginfilt
    surfaceflow  = 0.
    csurfaceflow = 0.
    macroflow    = 0.
    cmacroflow   = 0.

    !>If no incoming water; return
    IF(ginfilt == 0.)  RETURN

    !>Default surface runoff model: Surface  runoff and macropore flow calculated as fractions of water above thresholds
    IF(modeloption(p_surfacerunoff)==0)THEN
      
      !>\li Set parameter values
      macrate = soilpar(m_macrate,isoil)         !macropore flow fraction (-)
      inflowtres = soilpar(m_mactrinf,isoil)     !threshold for macropore (and surface runoff) flow (mm/timestep)
      smtresmm = (wp(1)+ fc(1))*soilpar(m_mactrsm,isoil)       !soil moisture threshold for macropore (and surface runoff) flow (mm)
      srrate = soilpar(m_srrate,isoil)           !surface runoff flow fraction (-)
      IF(macrate+srrate>1.)THEN
        help = macrate + srrate
        macrate = macrate/help
        srrate = srrate/help
      ENDIF

      !>\li Calculate surface flow and macropore flow due to limited infiltration capacity 
      avail = ginfilt - inflowtres 
      IF(avail>0. .AND. soilstate%water(1,j,i) > smtresmm) THEN
        macroflow = macrate * avail
        surfaceflow = srrate * avail
        cmacroflow = cginfilt
        csurfaceflow = cginfilt
      ENDIF

      !>\li Calculate net infiltration
      infilt = ginfilt - macroflow - surfaceflow
      cinfilt = cginfilt
    
      !>\li Calculate relative fraction of macropore flow and surface runoff
      IF(macrate+srrate>0.)THEN
        macfraction = macrate/(macrate+srrate)
        srfraction = 1.- macfraction
      ELSE
        macfraction = 0.5   !Default in case not set, but they should be set if conduct%icelens, TODO: check if test for that exist
        srfraction = 0.5
      ENDIF
      
    !>Surface runoff model 1: Surface  runoff calculated depending on soil moisture (continous equation formulation)
    !>Surface runoff model 2: Surface  runoff calculated depending on soil moisture and rain+melt (continous equation formulation)
    ELSEIF(modeloption(p_surfacerunoff)==1.OR.modeloption(p_surfacerunoff)==2)THEN
      
      !>\li Calculate relative fraction of macropore flow and surface runoff
      macfraction = soilpar(m_macfrac,isoil)         !macropore flow fraction of surface runoff
      srfraction = 1.- macfraction
      
      !>\li Set parameter values; beta is exponent of soil moisture relation
      IF(modeloption(p_surfacerunoff)==1)THEN
        beta = genpar(m_srbeta)
      ELSEIF(modeloption(p_surfacerunoff)==2)THEN
        beta = genpar(m_sralpha)/ginfilt**genpar(m_srgamma)
      ENDIF          
      srnlayer = NINT(genpar(m_srnlayer))

      !>\li Calculate surface runoff ratio
      S = SUM(soilstate%water(1:srnlayer,j,i))
      Smax = SUM(wp(1:srnlayer)+fc(1:srnlayer)+ep(1:srnlayer))
      srratio = (S/Smax)**beta
      srratio = MIN(srratio,1.)

      !>\li Calculate flow of the different paths
      surfaceflow = srratio * ginfilt
      macroflow = macfraction * surfaceflow       !macropore flow
      surfaceflow = surfaceflow - macroflow       !surface runoff
      infilt = ginfilt - macroflow - surfaceflow  !net infiltration upper soil

      !>Set concentration of the flow paths
      IF(numsubstances>0)THEN
        cmacroflow = cginfilt
        csurfaceflow = cginfilt
      ENDIF

    !>Surface runoff model 3: Surface  runoff calculated depending on soil moisture (discrete equation formulation)
    !>Surface runoff model 4: Surface  runoff calculated depending on soil moisture and rain+melt (discrete equation formulation)
    ELSEIF(modeloption(p_surfacerunoff)==3.OR.modeloption(p_surfacerunoff)==4)THEN

      !>\li Calculate relative fraction of macropore flow and surface runoff
      macfraction = soilpar(m_macfrac,isoil)         !macropore flow fraction of surface runoff
      srfraction = 1.- macfraction
      
  
      !>\li Set parameter values; beta is exponent of soil moisture relation
      IF(modeloption(p_surfacerunoff)==3)THEN
        beta = genpar(m_srbeta)
      ELSEIF(modeloption(p_surfacerunoff)==4)THEN
        beta = genpar(m_sralpha)/ginfilt**genpar(m_srgamma)
      ENDIF          
      srnlayer = INT(genpar(m_srnlayer))

      !>\li Calculate net infiltration
      S = SUM(soilstate%water(1:srnlayer,j,i))
      Smax = SUM(wp(1:srnlayer)+fc(1:srnlayer)+ep(1:srnlayer))
      !Doubtful if this equation is valid for model 3 and/or 4. Have to check with Alban again.
      infilt = Smax*(1-(S/Smax)**beta)*TANH(ginfilt/Smax)/(1+(S/Smax)*TANH(ginfilt/Smax))  
      IF(infilt .GT. ginfilt) infilt = ginfilt    !Should not happen
      IF(infilt .LT. 1e-6) infilt = 0.      !Avoid data precision issue (when prec or S/Smax closed to 0)

      !>\li Calculate flow of the different paths
      surfaceflow = ginfilt - infilt
      macroflow = macfraction * surfaceflow  !macropore flow
      surfaceflow = surfaceflow - macroflow  !surface runoff

      !>Set concentration of the flow paths
      IF(numsubstances>0)THEN
        cmacroflow = cginfilt
        csurfaceflow = cginfilt
      ENDIF

    ENDIF
    
    !>Calculate effect of frozen soil on infiltration and surface runoff
    !>Based on Zhao & Gray 1999 Estimate snowmelt infiltration into frozen soils
    !>coded by M.K. MacDonald (27 October 2015)
    IF(conduct%icelens)THEN
      
      !> Presence of icelens depends on daily maximum and minimum air temperature. 
      tmin_check = tmin
      tmax_check = tmax
      IF(tmin_check == missing_value) tmin_check = temp - 5. !based on winter climate normal data for Winnipeg Richardson Int'l A (M.K.MacDonald)
      IF(tmax_check == missing_value) tmax_check = temp + 5.
      IF((tmin_check<-10. .AND. infilt>=5.) .OR. (tmax_check<0. .AND. soilstate%icelens(j,i)==1)) THEN  
        !> Ice lens restricted infiltration, flow redirected to macroflow & surfaceflow, no infiltration
        soilstate%icelens(j,i) = 1
        surfaceflow = surfaceflow + infilt*srfraction
        macroflow = macroflow + infilt*macfraction
        infilt = 0.
      ELSEIF(soilstate%temp(1,j,i)<=0.) THEN
        !> Frozen soil limited infiltration, no ice lens
        soilstate%icelens(j,i) = 0
!        IF(soilstate%water(1,j,i)>=wp(1)+fc(1)+ep(1))THEN
        IF(water_relative_porevolume(1,j,soilstate%water(1,j,i))>=-0.)THEN
          potinfilt = 0.
        ELSE
          t0 = MAX(1., 0.65*frozenstate%snow(j,i)-5) !opportunity time [hours]
          cuminfilt = soilpar(m_bfroznsoil,isoil) * (0.99**2.92) * ((1.-soilstate%water(1,j,i)/(wp(1)+fc(1)+ep(1)))**1.64) * ((0.-soilstate%temp(1,j,i))/273.15)**(-0.45) * t0**0.44 !infiltration [mm]
          potinfilt = cuminfilt/(t0/(24./timesteps_per_day)) ! potential infiltration [mm/timestep]
          potinfilt = MAX(potinfilt, 0.)
        ENDIF
        IF(potinfilt < infilt)THEN ! reduce infilt to potential and redirected water to macroflow & surfaceflow
         surfaceflow = surfaceflow + (infilt - potinfilt)*srfraction
         macroflow = macroflow + (infilt - potinfilt)*macfraction
         infilt = potinfilt
        ENDIF
      ELSE
        !Unfrozen soils
        soilstate%icelens(j,i) = 0
      ENDIF

      !Concentration is the same for all flows, but may not have been set for all flows above
      cmacroflow = cginfilt
      csurfaceflow = cginfilt
    
    ENDIF

  END SUBROUTINE calculate_infiltration_flow_diversion

  !>Add infiltration to the upper soillayer soil, including
  !>transfering of IN in infiltration to solid ON in soil
  !------------------------------------------------------------------       
  SUBROUTINE add_infiltration(i,j,iluse,infilt,cinfilt,soilstate,no_nutrred)
       
    USE MODVAR, ONLY : simulate, &
                       numsubstances, &
                       i_in
       
    !Argument declaration
    INTEGER, INTENT(IN) :: i                  !<index of current subbasin
    INTEGER, INTENT(IN) :: j                  !<index of current class 
    INTEGER, INTENT(IN) :: iluse              !<index of landuse
    REAL, INTENT(IN)    :: infilt             !<infiltration (mm/timestep)
    REAL, INTENT(INOUT)   :: cinfilt(numsubstances)  !<concentration of infiltration
    TYPE(soilstatetype),INTENT(INOUT) :: soilstate   !<soil states
    LOGICAL,OPTIONAL,INTENT(IN) :: no_nutrred         !<flag for turning off nutrient processes

    !Local variables
    LOGICAL nutrproc    !flag for nutrient process action
    
    nutrproc = .TRUE.
    IF(PRESENT(no_nutrred)) nutrproc = .NOT. no_nutrred
    
    !>Add infiltration to the upper soillayer soil
    IF(infilt>0)THEN
      !>Possibly transfer of (atmospheric) IN in infiltration to solid ON (fastN) in soil
      IF(simulate%substance(i_in).AND.nutrproc) CALL atmdep_in_loss(iluse,infilt,soilstate%fastN(:,j,i),cinfilt(i_in))   !(Atmospheric) IN moved to fastN 
      CALL add_water(numsubstances,soilstate%water(1,j,i),soilstate%conc(:,1,j,i),infilt,cinfilt)
    ENDIF

  END SUBROUTINE add_infiltration

  !>Calculate infiltration from flooded floodplain to floodplain soil layer 1
  !------------------------------------------------------------------       
  SUBROUTINE flood_infiltration(i,j,pw,infilt,outfilt,cinfilt,soilstate)
       
    USE MODVAR, ONLY : numsubstances
       
    !Argument declaration
    INTEGER, INTENT(IN) :: i            !<index of current subbasin
    INTEGER, INTENT(IN) :: j            !<index of current class 
    REAL, INTENT(IN)    :: pw           !<pore volume (mm)
    REAL, INTENT(IN)    :: infilt       !<available for infiltration (mm/timestep)
    REAL, INTENT(OUT)   :: outfilt      !<actual infiltration (mm/timestep)
    REAL, INTENT(INOUT) :: cinfilt(numsubstances)       !<concentration of infiltration
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states

    !Local variables
    REAL space
    
    outfilt = infilt
    !>Add infiltration to the upper soillayer soil if there is room
    IF(outfilt>0.)THEN
      space = pw - soilstate%water(1,j,i) 
      IF(space>0.)THEN
        IF(space<outfilt) outfilt = space
        CALL add_water(numsubstances,soilstate%water(1,j,i),soilstate%conc(:,1,j,i),outfilt,cinfilt)
      ELSE
        outfilt = 0.
      ENDIF  
    ENDIF

  END SUBROUTINE flood_infiltration

  !>Calculate percolation down through the soil layers considering previous percolation 
  !>and the maximum percolation of the whole timestep
  !>Includes change in concentration of percolating water. 
  !>
  !>\b Reference ModelDescription Chapter Land routines (Soil water - Percolation)
  !----------------------------------------------------------------------
  SUBROUTINE percolation(i,j,isoil,subid,wp,fc,pw,sthick,liqfrac,percflow,cpercflow,soilstate,nopercred)
  
    USE MODVAR, ONLY : numsubstances,   &
                       maxsoillayers,   &
                       genpar,soilpar,landpar, &            
                       classdata
    USE HYPEVARIABLES, ONLY : m_perc1,m_perc2,    &
                              m_crate5,   &
                              m_onpercred, m_pppercred
  
    !Argument declaration
    INTEGER, INTENT(IN) :: i                  !<index of current subbasin
    INTEGER, INTENT(IN) :: j                  !<index of current class 
    INTEGER, INTENT(IN) :: isoil              !<index of soil type
    INTEGER, INTENT(IN) :: subid              !<subbasin id
    REAL, INTENT(IN)    :: wp(maxsoillayers)  !<wilting point volume (mm)
    REAL, INTENT(IN)    :: fc(maxsoillayers)  !<"field capacity" volume (mm) (water available for evaporation but not for runoff)
    REAL, INTENT(IN)    :: pw(maxsoillayers)  !< pore volume (mm)
    REAL, INTENT(IN)    :: sthick(maxsoillayers) !<thickness of soil layers (m)
    REAL, INTENT(IN)    :: liqfrac(maxsoillayers) !<fraction of liquid water (-)
    REAL, INTENT(INOUT) :: percflow(2)        !<percolation (mm/time step)
    REAL, INTENT(INOUT) :: cpercflow(2,numsubstances) !<concentration of percolation (mm/time step)
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    LOGICAL,OPTIONAL,INTENT(IN) :: nopercred          !<flag for turning off nutrient reduction during percolation
    
    !Local variables
    INTEGER status            !error status of subroutine
    LOGICAL nutrproc          !flag for calling nutrient reduction during percolation
    REAL perc1,perc2          !percolation soillayer 1 and 2 (mm/timestep)
    REAL maxperc1,maxperc2    !maximum percolation from soillayer 1 and maximum percolation to soillayer 2 (mm/timestep)
    REAL cperc(numsubstances) !concentration of percolation
    REAL firstpercflow(2),cfirstpercflow(2,numsubstances)
    REAL water2help,water2help2
  
    !>\b Algorithm \n
    !Include loss of nutrient and organic carbon during percolation?
    nutrproc = .TRUE.
    IF(PRESENT(nopercred)) nutrproc = .NOT.nopercred
    
    IF(sthick(2)>0.)THEN
      !Start calculate percolation
      firstpercflow = percflow
      cfirstpercflow = cpercflow
  
      !>Calculate limitations for second percolation              
      maxperc1 = MIN(unfrozen_water_above_field_capacity(1,j,soilstate%water(1,j,i),liqfrac(1)),soilpar(m_perc1,isoil)-firstpercflow(1))   !Maximum percolation from uppermost soil layer
      maxperc2 = MIN(-water_relative_porevolume(3,j,soilstate%water(3,j,i)),soilpar(m_perc2,isoil)-firstpercflow(2))   !Maximum percolation deep soil layer can accept
  
      !>Calculate percolation amount of water
      water2help = liqfrac(2)*(soilstate%water(2,j,i)-wp(2)-fc(2))         !liquid soil water in relation to field capacity
      water2help2 = water_relative_porevolume(2,j,soilstate%water(2,j,i))  !soil water in relation to porevolume
      perc2 = MIN(MAX(water2help+maxperc1,0.),maxperc2)
      perc1 = MIN(maxperc1,MAX(perc2-water2help2,0.))
  
      !>Move percolation water to underlaying soillayer and reduce the concentrations:
      IF(perc1>0.)THEN
        IF(numsubstances>0) cperc=soilstate%conc(:,1,j,i)
        IF(nutrproc)THEN
          !>\li Reduce OC, ON and PP concentration of water percolating from upper soillayer
          CALL doc_percolation_reduction(numsubstances,cperc,genpar(m_crate5),soilstate%temp(1,j,i),   &
               soilstate%water(1,j,i),wp(1),pw(1),sthick(1))
          CALL onpp_percolation_reduction(numsubstances,cperc,landpar(m_onpercred,classdata(j)%luse),   &
               landpar(m_pppercred,classdata(j)%luse))  
        ENDIF
        !>\li Remove water from upper soillayer and add to second soillayer
        CALL remove_water(soilstate%water(1,j,i),numsubstances,soilstate%conc(:,1,j,i),perc1,cperc,status) 
        IF(status.NE.0) CALL error_remove_water(errstring(9),subid,i,j)
        CALL add_water(numsubstances,soilstate%water(2,j,i),soilstate%conc(:,2,j,i),perc1,cperc)
        percflow(1) = perc1 + firstpercflow(1)
        IF(numsubstances>0.) cpercflow(1,:) = (cperc(:)*perc1+cfirstpercflow(1,:)*firstpercflow(1))/percflow(1)
      ENDIF
      IF(perc2>0.)THEN
        IF(numsubstances>0) cperc=soilstate%conc(:,2,j,i)
        IF(nutrproc)THEN
          !>\li Reduce OC, ON and PP concentration of water percolating from middle soillayer
          CALL doc_percolation_reduction(numsubstances,cperc,genpar(m_crate5),soilstate%temp(2,j,i),   &
               soilstate%water(2,j,i),wp(2),pw(2),sthick(2))
          CALL onpp_percolation_reduction(numsubstances,cperc,landpar(m_onpercred,classdata(j)%luse),   &
               landpar(m_pppercred,classdata(j)%luse))
        ENDIF
        !>\li Remove water from middle soillayer and add to third soillayer
        CALL remove_water(soilstate%water(2,j,i),numsubstances,soilstate%conc(:,2,j,i),perc2,cperc,status) 
        IF(status.NE.0) CALL error_remove_water(errstring(10),subid,i,j)
        CALL add_water(numsubstances,soilstate%water(3,j,i),soilstate%conc(:,3,j,i),perc2,cperc)
        percflow(2) = perc2 + firstpercflow(2)
        IF(numsubstances>0.) cpercflow(2,:) = (cperc(:)*perc2+cfirstpercflow(2,:)*firstpercflow(2))/percflow(2)
      ENDIF
  
    ENDIF
  
  END SUBROUTINE percolation

  !>\brief Add macropore water flow to soil layer with groundwater level. 
  !>
  !!If this soillayer can't take all water the rest is added to the
  !!soillayer(s) above. 
  !>
  !>\b Reference ModelDescription Chapter Land routines (Soil water - Macropore flow)
  !---------------------------------------------------------------------
  SUBROUTINE add_macropore_flow(i,j,macroflow,cmacroflow,ep,pw,sdepth,sthick,slmacroflows,soilstate)

    USE MODVAR, ONLY : numsubstances,   &
                       maxsoillayers

    !Argument declarations
    INTEGER, INTENT(IN) :: i                  !<index of current subbasin
    INTEGER, INTENT(IN) :: j                  !<index of current class 
    REAL, INTENT(IN)    :: macroflow          !<macropore flow to be added (mm/timestep)
    REAL, INTENT(IN)    :: cmacroflow(numsubstances) !<concentration of macropore flow 
    REAL, INTENT(IN)    :: ep(maxsoillayers)  !<"effective porosity" volume (mm) (water avaliable for runoff)
    REAL, INTENT(IN)    :: pw(maxsoillayers)  !<total pore volume (mm)
    REAL, INTENT(IN)    :: sdepth(maxsoillayers) !<lower border of soil layers (m)
    REAL, INTENT(IN)    :: sthick(maxsoillayers) !<thickness of soil layers (m)
    REAL, INTENT(OUT)   :: slmacroflows(maxsoillayers) !<macropore flow to each soil layer (mm/timestep)
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    
    !Local variables
    REAL gwat                   !groundwater table (m) (negative)
    REAL newsoil                
    REAL fill,fill2

    !Start subroutine
    !>\b Algorithm \n
    !>If no macropore flow: return
    slmacroflows = 0
    IF(macroflow<=0) RETURN   

    !>Find soillayer of groundwater table
    CALL calculate_groundwater_table(j,soilstate%water(:,j,i),ep,sdepth(:),sthick(:),gwat)
    !>If groundwater table in soillayer three:
    IF(-gwat>sdepth(2)) THEN
      !>\li Check if soillayer three has room and add the water there is room for
      newsoil = soilstate%water(3,j,i) + macroflow
      IF(newsoil > pw(3)) THEN
        fill = newsoil - pw(3)
        IF(numsubstances>0) soilstate%conc(:,3,j,i) = (soilstate%water(3,j,i)*soilstate%conc(:,3,j,i) + (macroflow - fill)*cmacroflow)/pw(3)
        soilstate%water(3,j,i) = pw(3)
        slmacroflows(3) = macroflow - fill
      ELSE
        fill = 0.
        IF(numsubstances>0) soilstate%conc(:,3,j,i) = (soilstate%water(3,j,i)*soilstate%conc(:,3,j,i) + macroflow*cmacroflow)/newsoil
        soilstate%water(3,j,i) = newsoil
        slmacroflows(3) = macroflow
      ENDIF
      !>\li If too much water, check if soillayer 2 has room and add the water there is room for
      IF(fill > 0.) THEN
        newsoil = soilstate%water(2,j,i) + fill
        IF(newsoil > pw(2)) THEN
          fill2 = newsoil - pw(2)
          IF(numsubstances>0) soilstate%conc(:,2,j,i) = (soilstate%water(2,j,i)*soilstate%conc(:,2,j,i) + (fill-fill2)*cmacroflow)/pw(2)
          soilstate%water(2,j,i) = pw(2)
          slmacroflows(2) = fill - fill2
        ELSE
          fill2 = 0.
          IF(numsubstances>0) soilstate%conc(:,2,j,i) = ((newsoil-fill)*soilstate%conc(:,2,j,i) + fill*cmacroflow)/newsoil
          soilstate%water(2,j,i) = newsoil
          slmacroflows(2) = fill
        ENDIF
        !>\li If still too much water add the rest to soillayer 1
        IF(fill2 > 0.) THEN
          newsoil = soilstate%water(1,j,i) + fill2
          IF(numsubstances>0) soilstate%conc(:,1,j,i) = (soilstate%water(1,j,i)*soilstate%conc(:,1,j,i) + fill2*cmacroflow)/newsoil
          soilstate%water(1,j,i) = newsoil
          slmacroflows(1) = fill2
        ENDIF
      ENDIF

    !>Elseif groundwater table in soillayer two:
    ELSEIF(-gwat>sdepth(1)) THEN
      newsoil = soilstate%water(2,j,i) + macroflow
      !>\li Check if soillayer 2 has room and add the water there is room for
      IF(newsoil > pw(2)) THEN
        fill = newsoil - pw(2)
        IF(numsubstances>0) soilstate%conc(:,2,j,i) = (soilstate%water(2,j,i)*soilstate%conc(:,2,j,i) + (macroflow - fill)*cmacroflow)/pw(2)
        soilstate%water(2,j,i) = pw(2)
        slmacroflows(2) = macroflow - fill
      ELSE
        fill = 0.
        IF(numsubstances>0) soilstate%conc(:,2,j,i) = (soilstate%water(2,j,i)*soilstate%conc(:,2,j,i) + macroflow*cmacroflow)/newsoil
        soilstate%water(2,j,i) = newsoil
        slmacroflows(2) = macroflow
      ENDIF
      !>\li If too much water add the rest to soillayer 1
      IF(fill > 0.) THEN
        CALL add_water(numsubstances,soilstate%water(1,j,i),soilstate%conc(:,1,j,i),fill,cmacroflow)
        slmacroflows(1) = fill
      ENDIF

    !>Elseif groundwater table in soillayer one:
    ELSE
      !>\li Add macropore flow to soillayer 1
      CALL add_water(numsubstances,soilstate%water(1,j,i),soilstate%conc(:,1,j,i),macroflow,cmacroflow)
      slmacroflows(1) = macroflow
    ENDIF

  END SUBROUTINE add_macropore_flow

  !>Subroutine for calculation of ground water table level (metres
  !>above land surface)
  !!
  !> \b Reference ModelDescription Chapter Land routines (Basic assumptions - Diagnostic variables)
  !--------------------------------------------------------------------
  SUBROUTINE calculate_groundwater_table(j,soil,epvol,soildep,thickness,gwat)
  
    USE MODVAR, ONLY : maxsoillayers

    !Argument declarations
    INTEGER,INTENT(IN) :: j                        !<current class
    REAL, INTENT(IN)   :: soil(maxsoillayers)      !<soil moisture (mm)
    REAL, INTENT(IN)   :: epvol(maxsoillayers)     !<effective porosity volume in all layers (mm) 
    REAL, INTENT(IN)   :: soildep(maxsoillayers)   !<depth of soil layers (m)
    REAL, INTENT(IN)   :: thickness(maxsoillayers) !<thickness of soil layers (m)
    REAL, INTENT(OUT)  :: gwat                     !<ground water table (m), measured from land surface and up
    
    !Local parameters
    REAL, PARAMETER :: mindiff = 0.000002
    
    !Local variables
    INTEGER k
    REAL gwatk      !groundwater table of soil layer k

    gwat = 0.

    DO k = maxsoillayers,1,-1
      IF(thickness(k)>0.)THEN
        IF(water_is_above_field_capacity(k,j,soil(k)))THEN
          gwatk = water_relative_porevolume(k,j,soil(k))/epvol(k) * thickness(k)     !negative (m)
          IF(k==1)THEN
            IF(gwatk > 0.) gwatk = water_relative_porevolume(k,j,soil(k)) * 0.001    !100% porositet above land surface
          ELSE
            gwatk = gwatk - soildep(k-1)
          ENDIF
          IF(water_relative_porevolume(k,j,soil(k))>=0.-mindiff*epvol(k))THEN
            CYCLE
          ELSE
            EXIT
          ENDIF
        ELSE
          gwatk = -soildep(k)
          EXIT
        ENDIF
      ENDIF
    ENDDO
    gwat = gwatk

  END SUBROUTINE calculate_groundwater_table

  !>Calculation of soil temperature in soil layers and deep soil
  !>
  !> \b Reference ModelDescription Chapter Land routines (Snow routines - Soil temperature and snow depth)
  !-----------------------------------------------------------------------
  SUBROUTINE calculate_soiltemp(n,airtemp,snowdepth,soilmemdeep,soilmemlayer,deeptemp,soiltemp)

    USE MODVAR, ONLY : timesteps_per_day
    
    !Argument declarations
    INTEGER, INTENT(IN)  :: n               !<number of soil layers
    REAL, INTENT(IN)     :: airtemp         !<air temperature (degree Celcius) 
    REAL, INTENT(IN)     :: snowdepth       !<snow depth (cm)
    REAL, INTENT(IN)     :: soilmemdeep     !<parameter, temperature memory of deep soil (days)
    REAL, INTENT(IN)     :: soilmemlayer(n) !<parameter, temperature memory of soil layer (timesteps)
    REAL, INTENT(INOUT)  :: deeptemp        !<deep soil temperature (degree Celcius)
    REAL, INTENT(INOUT)  :: soiltemp(n)     !<soil temperature (degree Celcius)
    
    !Local parameters
    REAL, PARAMETER :: spfrost = 10.      !coefficient of equation for weight of air temperature in soil temperature calculation (days/cm)
    REAL, PARAMETER :: weightdeep = 0.001 !weight of deep soil temperature for soil temperature calculation (dimensionless)
    
    !Local variables
    INTEGER k        !layer index
    REAL weightair   !weight of air temperature for soil temperature calculation (dimensionless)

    !> \b Algorithm \n
    !>Calculate deep soil temperature
    IF(soilmemdeep + spfrost * snowdepth>1.)THEN
      weightair = 1./ ((soilmemdeep + spfrost * snowdepth)*timesteps_per_day)
    ELSE
      weightair = 1.
    ENDIF
    CALL calculate_weighted_temperature(airtemp,weightair,0.0,0.0,deeptemp)
    !>Calculate soil layer temperature for each soil layer
    DO k = 1,n
      IF(soilmemlayer(k) + spfrost * snowdepth * timesteps_per_day > 1. * timesteps_per_day)THEN
        weightair = 1./ (soilmemlayer(k) + spfrost * snowdepth * timesteps_per_day)
      ELSE
        weightair = 1. - weightdeep
      ENDIF
      CALL calculate_weighted_temperature(airtemp,weightair,deeptemp,weightdeep,soiltemp(k))
    ENDDO

   END SUBROUTINE calculate_soiltemp

  !>Calculation of soil temperature as an average of three temperatures: 
  !>air temperature, deep soil temperature and previous soil temperature.
  !>
  !> \b Reference ModelDescription Chapter Land routines (Snow routines - Soil temperature and snow depth)
  !--------------------------------------------------------------------------------
  SUBROUTINE calculate_weighted_temperature(temp1,weight1,temp2,weight2,soiltemp)

    !Argument declarations
    REAL, INTENT(IN)     :: temp1     !<air temperature (degree Celcius) 
    REAL, INTENT(IN)     :: weight1   !<weight of temp1 (dimensionless)
    REAL, INTENT(IN)     :: temp2     !<temperature of deep soil (ca 1m) (degree Celcius)
    REAL, INTENT(IN)     :: weight2   !<weight of temp2 (dimensionless)
    REAL, INTENT(INOUT)  :: soiltemp  !<soil layer temperature (degree Celcius)

    !> \b Algorithm \n
    !>Calculate weighted temperature
    soiltemp = soiltemp * (1. - weight1 - weight2) + &
               temp1    * weight1  +   &
               temp2    * weight2

  END SUBROUTINE calculate_weighted_temperature

  !>Calculates unfrozen soil water volume and fraction.
  !----------------------------------------------------------------------
  SUBROUTINE calculate_unfrozen_soil_water(i,j,isoil,airtemp,wp,fc,ep,soilstate,frozenvol,liqfrac)
    
    USE MODVAR, ONLY : modeloption, soilpar, maxsoillayers, p_frozensoil, realzero
    USE HYPEVARIABLES, ONLY:  m_logsatmp, m_bcosby
    
    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<index of current subbasin
    INTEGER, INTENT(IN) :: j        !<index of current class
    INTEGER, INTENT(IN) :: isoil    !<index of soil type
    REAL, INTENT(IN)    :: airtemp  !<air temperature (deg)
    REAL, INTENT(IN)    :: wp(maxsoillayers) !<volume below wilting point (mm)
    REAL, INTENT(IN)    :: fc(maxsoillayers) !<"field capacity" volume (mm)
    REAL, INTENT(IN)    :: ep(maxsoillayers) !<effective porosity volume (mm)
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    REAL, INTENT(OUT) :: frozenvol(maxsoillayers) !<Frozen water volume (mm) 
    REAL, INTENT(OUT) :: liqfrac(maxsoillayers) !<Liquid water fraction (-)
    
    !Local variables
    INTEGER k
    REAL liqmax, porosity, numerator, denominator
    REAL threetemp(3),temp, liqfrac1,liqfrac2,liqfrac3,HELPVAR
  
    SELECT CASE(modeloption(p_frozensoil))
    CASE(0)  !Current HYPE (no frozen soil water)
      liqfrac(:)   = 1.
      frozenvol(:) = 0.
    CASE(1)  !Liquid water fraction as a function of soil temperature
      DO k=1,maxsoillayers
        IF(soilstate%temp(k,j,i) >= realzero) THEN
          liqfrac(k) = 1.
        ELSE
          IF(soilstate%water(k,j,i).GT.realzero)THEN
            porosity = ep(k)+wp(k)+fc(k)
            numerator = -334000.*soilstate%temp(k,j,i)
            denominator = 9.81*(soilstate%temp(k,j,i)+273.16)*(10**soilpar(m_logsatmp,isoil)/100.)
            liqmax = porosity*(numerator/denominator)**(-1./soilpar(m_bcosby,isoil))
            liqfrac(k)= MIN( 1., liqmax / soilstate%water(k,j,i) )
          ELSE
            liqfrac(k)=0.
          ENDIF 
        ENDIF
      ENDDO
      frozenvol(:) = (1. - liqfrac(:)) * soilstate%water(:,j,i)
    CASE(2)  !Liquid water fraction as a function of three soil temperature
      DO k=1,maxsoillayers
        IF(soilstate%water(k,j,i).GT.realzero)THEN
          porosity = ep(k)+wp(k)+fc(k)
          helpvar = porosity / soilstate%water(k,j,i)
          CALL calculate_three_soil_temperature(i,j,k,airtemp,soilstate,threetemp)
          temp = threetemp(1)
          IF(temp >= realzero) THEN
            liqfrac1 = 1.
          ELSE
            numerator = -334000.*temp
            denominator = 9.81*(temp+273.16)*(10**soilpar(m_logsatmp,isoil)/100.)
            liqfrac1 = MIN(1. , helpvar*(numerator/denominator)**(-1./soilpar(m_bcosby,isoil)))
          ENDIF
          temp = threetemp(2)
          IF(temp >= realzero) THEN
            liqfrac2 = 1.
          ELSE
            numerator = -334000.*temp
            denominator = 9.81*(temp+273.16)*(10**soilpar(m_logsatmp,isoil)/100.)
            liqfrac2 = MIN(1., helpvar*(numerator/denominator)**(-1./soilpar(m_bcosby,isoil)))
          ENDIF
          temp = threetemp(3)
          IF(temp >= realzero) THEN
            liqfrac3 = 1.
          ELSE
            numerator = -334000.*temp
            denominator = 9.81*(temp+273.16)*(10**soilpar(m_logsatmp,isoil)/100.)
            liqfrac3 = MIN(1., helpvar*(numerator/denominator)**(-1./soilpar(m_bcosby,isoil)))
          ENDIF
          liqfrac(k) = (liqfrac1+liqfrac2+liqfrac3)/3.
        ELSE
          liqfrac(k) = 0.
        ENDIF
      ENDDO
      frozenvol(:) = (1. - liqfrac(:)) * soilstate%water(:,j,i)
    END SELECT
  
  END SUBROUTINE calculate_unfrozen_soil_water

  !>Calculates three soil teperatures representing one soil layer. 
  !>The three temperatures represent 1/6,1/2,5/6 into the soillayer, assuming 
  !>linear interpolation between soilstate%temp representing 1/2 into soillayer.
  !----------------------------------------------------------------------
  SUBROUTINE calculate_three_soil_temperature(i,j,k,airtemp,soilstate,threetemp)
  
    USE MODVAR, ONLY : soildepth,soilthick
    USE GENERAL_FUNCTIONS, ONLY : linear_response
    
    !Argument declaration
    INTEGER, INTENT(IN) :: i     !<subbasin
    INTEGER, INTENT(IN) :: j     !<class
    INTEGER, INTENT(IN) :: k     !<soil layer
    REAL, INTENT(IN)    :: airtemp  !<class airtemp
    TYPE(soilstatetype),INTENT(INOUT)  :: soilstate   !<Soil states
    REAL, INTENT(OUT)   :: threetemp(3) !<three-divided soil temperature of soil layer k
  
    !Local variables
    REAL x(0:4),y(0:4),t(3)
    REAL threedepths(3)
    
    !Set depth,temperature pairs (x,y)
    x(0)=-soilthick(1,j)/2.;y(0)=airtemp
    x(1)=soilthick(1,j)/2.;y(1)=soilstate%temp(1,j,i)
    x(2)=soildepth(1,j)+soilthick(2,j)/2.;y(2)=soilstate%temp(2,j,i)
    x(3)=soildepth(2,j)+soilthick(3,j)/2.;y(3)=soilstate%temp(3,j,i)
    x(4)=soildepth(3,j)+soilthick(3,j)/2.;y(4)=soilstate%deeptemp(j,i)  !This give T3+=(2*T3+Tdeep)/3
    !Set thicknesses
    t=soilthick(:,j)
    
    !Calculate three depth in soil layer, each in the middle of a third of the soil layer 
    threedepths(1) = x(k)-t(k)/3.
    threedepths(2) = x(k)
    threedepths(3) = x(k)+t(k)/3.
    
    !Calculate temperature of the three depths by linear interpolation
    threetemp(1) = linear_response(threedepths(1),x(k-1),x(k),y(k-1),y(k))
    threetemp(2) = y(k)
    threetemp(3) = linear_response(threedepths(3),x(k),x(k+1),y(k),y(k+1))
    
  END SUBROUTINE calculate_three_soil_temperature

  !> \brief Subroutine to calculate the fraction of liquid water in soil as a 
  !>function of frozen water volume and total water volume
  !---------------------------------------------------------------------------
  SUBROUTINE calculate_liquid_water_fraction(i,j,soilstate,frozenvol,liqfrac)
    
    USE MODVAR, ONLY : maxsoillayers, realzero
    
    !Argument declarations
    INTEGER, INTENT(IN) :: i        !<index of current subbasin
    INTEGER, INTENT(IN) :: j        !<index of current class
    TYPE(soilstatetype),INTENT(IN)  :: soilstate  !<Soil states
    REAL, INTENT(IN)  :: frozenvol(maxsoillayers) !<Frozen water volume (mm) 
    REAL, INTENT(OUT) :: liqfrac(maxsoillayers)   !<Liquid water fraction (-)
    
    !Local variables
    INTEGER k
    
    liqfrac(:) = 1.
    DO k=1,maxsoillayers
      IF(soilstate%water(k,j,i).GT.realzero .AND. frozenvol(k).GT.realzero)THEN
        liqfrac(k) = MIN( 1., (soilstate%water(k,j,i) - frozenvol(k)) / soilstate%water(k,j,i) )
        !IF(liqfrac(k)<0.)THEN   !TA bORT?
        !  WRITE(6,*) 'liqfrac<0, i,j,k', i,j,k,liqfrac(k)
        !  STOP 1
        !ENDIF
      ENDIF
    ENDDO

  END SUBROUTINE calculate_liquid_water_fraction
  
  !>Calculation of soil frost depth depending on temperature of soil
  !>
  !> \b Reference ModelDescription Chapter Land routines (Basic assumptions - Diagnostic variables)
  !-------------------------------------------------------------------------------------
  SUBROUTINE calculate_frostdepth(fc,wp,cfrost,sfrost,soil,frostdepth,soiltemp,thickness)
  
    USE MODVAR, ONLY : maxsoillayers, &
                       missing_value

    !Argument declarations
    REAL, INTENT(IN)  :: fc(maxsoillayers)    !<water content at field capacity (mm)
    REAL, INTENT(IN)  :: wp(maxsoillayers)    !<water content at wilting point (mm)
    REAL, INTENT(IN)  :: cfrost       !<soil frost coefficient, land use dependent (cm/degree)
    REAL, INTENT(IN)  :: sfrost       !<soil frost coefficient, soil type dependent (cm/degree)
    REAL, INTENT(IN)  :: soil         !<soil water (mm) 
    REAL, INTENT(OUT) :: frostdepth   !<depth of soil frost (cm)
    REAL, INTENT(IN)  :: soiltemp(:)  !<soil temperature (degree Celcius)
    REAL, INTENT(IN)  :: thickness(maxsoillayers) !<soil layer thickness (m)
    
    !Local variables 
    INTEGER dim
    REAL uppertemp  !average temperature upper soil (ca 50 cm)
    REAL fieldcap

    !> \b Algorithm \n
    !>If soil frost parameters are set:
    IF(cfrost>0 .AND. sfrost>0)THEN
      dim = SIZE(soiltemp)
      !> \li Calculate average temperature of upper two soil layers
      IF(dim==1)THEN
        uppertemp = soiltemp(1)
      ELSE
        uppertemp = (soiltemp(1)*thickness(1)+soiltemp(2)*thickness(2))/(thickness(1)+thickness(2))
      ENDIF
      !> \li If temperature is negative, calculate soil frost depth
      IF(uppertemp<0)THEN
        fieldcap = fc(1)+wp(1)+fc(2)+wp(2)+fc(3)+wp(3)
        frostdepth = cfrost * sfrost * uppertemp * fieldcap / soil
      ELSE
        frostdepth = 0.
      ENDIF
    ELSE
    !Else soil frost is set to missing
      frostdepth = missing_value
    ENDIF

  END SUBROUTINE calculate_frostdepth

  !> \brief Calculation of soil moisture deficit (mm left to field capacity)
  !>in top two layers
  !>
  !> \b Reference ModelDescription Chapter Land routines (Basic assumptions - Diagnostic variables)
  !---------------------------------------------------------------------
  SUBROUTINE calculate_soil_moisture_deficit(soil,wpvol,fcvol,thickness,smdef)
    USE MODVAR, ONLY : maxsoillayers

    !Argument declaration
    REAL, INTENT(IN)     :: soil(maxsoillayers)       !<soil moisture  (mm)
    REAL, INTENT(IN)     :: wpvol(maxsoillayers)      !<wilting point volume in all layers (mm) 
    REAL, INTENT(IN)     :: fcvol(maxsoillayers)      !<"field capacity" volume in all layers (mm)
    REAL, INTENT(IN)     :: thickness(maxsoillayers)  !<thickness of soil layers (m)
    REAL, INTENT(OUT)    :: smdef                     !<soil moisture deficit (mm)
    
    !Local variables
    REAL smdef1,smdef2      !soil moisture deficit of each soil layer

    !> \b Algorithm \n
    !>Initate soil moisture deficit to zero
    smdef = 0.

    !>Calculate soil moisture deficit in top soil layer, add to total
    smdef1 = fcvol(1)+wpvol(1)-soil(1)
    IF(smdef1>0.) smdef = smdef + smdef1

    !>Calculate soil moisture deficit in second soil layer, add to total
    IF(thickness(2)>0)THEN
      smdef2 = fcvol(2)+wpvol(2)-soil(2)
      IF(smdef2>0.) smdef = smdef + smdef2
    ENDIF

  END SUBROUTINE calculate_soil_moisture_deficit

  ! >\brief Checks soil water above field capacity.
  ! --------------------------------------------------------------
  LOGICAL FUNCTION water_is_above_field_capacity(sl,j,water)  
    
    USE HYPEVARIABLES, ONLY : wpmm,fcmm
    
    INTEGER, INTENT(IN) :: sl    !<soil layer
    INTEGER, INTENT(IN) :: j     !<class
    REAL, INTENT(IN) :: water    !<soil water
    
    !Local variables
    LOGICAL fcn
    
    fcn = .FALSE.
    IF(water - wpmm(sl,j) - fcmm(sl,j) > 0.) fcn = .TRUE.
    water_is_above_field_capacity = fcn

  END FUNCTION water_is_above_field_capacity

  ! >\brief Calculates soil water above field capacity in mm (>=0).
  ! --------------------------------------------------------------
  REAL FUNCTION water_above_field_capacity(sl,j,water)  
    
    USE HYPEVARIABLES, ONLY : wpmm,fcmm
    
    INTEGER, INTENT(IN) :: sl    !<soil layer
    INTEGER, INTENT(IN) :: j     !<class
    REAL, INTENT(IN) :: water    !<soil water
    
    !Local variables
    REAL fcn
    
    fcn = 0.
    IF(water_is_above_field_capacity(sl,j,water)) &
      fcn = water - wpmm(sl,j) - fcmm(sl,j)
    water_above_field_capacity = fcn

  END FUNCTION water_above_field_capacity
  
  ! >\brief Checks if soil water is above wilting point
  ! --------------------------------------------------------------
  LOGICAL FUNCTION water_is_above_wilting_point(sl,j,water)  
    
    USE HYPEVARIABLES, ONLY : wpmm
    
    INTEGER, INTENT(IN) :: sl    !<soil layer
    INTEGER, INTENT(IN) :: j     !<class
    REAL, INTENT(IN) :: water    !<soil water
    
    !Local variables
    LOGICAL fcn
    
    fcn = .FALSE.
    IF(water - wpmm(sl,j) > 0.) fcn = .TRUE.
    water_is_above_wilting_point = fcn

  END FUNCTION water_is_above_wilting_point

  ! >\brief Calculates soil water above wilting point in mm (>=0).
  ! --------------------------------------------------------------
  REAL FUNCTION water_above_wilting_point(sl,j,water)  
    
    USE HYPEVARIABLES, ONLY : wpmm
    
    INTEGER, INTENT(IN) :: sl    !<soil layer
    INTEGER, INTENT(IN) :: j     !<class
    REAL, INTENT(IN) :: water    !<soil water
    
    !Local variables
    REAL fcn
    
    fcn = 0.
    IF(water_is_above_wilting_point(sl,j,water)) &
      fcn = water - wpmm(sl,j)
    water_above_wilting_point = fcn

  END FUNCTION water_above_wilting_point
  
  ! >\brief Calculates unfrozen soil water above field capacity in mm (>=0).
  ! --------------------------------------------------------------
  REAL FUNCTION unfrozen_water_above_field_capacity(sl,j,water,liqfrac)  
    
    USE HYPEVARIABLES, ONLY : wpmm,fcmm
    
    INTEGER, INTENT(IN) :: sl    !<soil layer
    INTEGER, INTENT(IN) :: j     !<class
    REAL, INTENT(IN) :: water    !<soil water
    REAL, INTENT(IN) :: liqfrac  !<fraction of water in liquid form
    
    !Local variables
    REAL fcn
    
    fcn = 0.
    IF(water_is_above_field_capacity(sl,j,water)) &
      fcn = liqfrac*(water - wpmm(sl,j) - fcmm(sl,j))
    unfrozen_water_above_field_capacity = fcn

  END FUNCTION unfrozen_water_above_field_capacity
  
  ! >\brief Calculates soil water relative to pore volume in mm 
  !!(positive=over saturated or negative=unsaturated).
  ! --------------------------------------------------------------
  REAL FUNCTION water_relative_porevolume(sl,j,water)  
      
    USE MODVAR, ONLY : soilthick
    USE HYPEVARIABLES, ONLY : pwmm
    
    INTEGER, INTENT(IN) :: sl    !<soil layer
    INTEGER, INTENT(IN) :: j     !<class
    REAL, INTENT(IN) :: water    !<soil water
    
    !Local variables
    REAL fcn
    
    fcn = 0.
    IF(soilthick(sl,j)>0.) fcn = water - pwmm(sl,j)
    water_relative_porevolume = fcn

  END FUNCTION water_relative_porevolume

END MODULE SOIL_PROCESSES
