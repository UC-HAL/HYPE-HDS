!> \file update.f90
!> Contains module updating.

!>Module for updating and correction procedures used with the hydrological model HYPE.
!>
MODULE UPDATING
!Copyright 2020 SMHI
!
!This file is part of HYPE.
!HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
!You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.

!----------------------------------------------------------------------

!Used modules
  USE STATETYPE_MODULE, ONLY : lakestatetype  !model state variable types
!Subroutines also uses HYPEVARIABLES, MODVAR, SURFACEWATER_PROCESSES

  IMPLICIT NONE
  PRIVATE

  ! Private subroutines
  !--------------------------------------------------------------------
  PUBLIC :: apply_quseobs ,&
            apply_qarupd,&
            apply_warupd,&
            apply_wendupd,&
            apply_wendupd_lakebasin_lake,&
            apply_cuseobs,&
            apply_nutrientcorr

  CONTAINS
  
  
  !>Update outflow of subbasin to observed value
  !-------------------------------------------------
  SUBROUTINE apply_quseobs(i,corrFlow)

    USE MODVAR, ONLY : i_quseobs, &
                       missing_value,  &
                       qobsi,   &
                       useinupdate
     
    !Argument declaration
    INTEGER, INTENT(IN) :: i         !<index of current subbasin
    REAL, INTENT(INOUT) :: corrFlow  !<simulated outflow of subbasin !!(lakeoutflow)
     
    IF(ALLOCATED(useinupdate(i_quseobs)%station))THEN 
      IF(useinupdate(i_quseobs)%station(i))THEN
        IF(ALLOCATED(qobsi))THEN
          IF(qobsi(i)/=missing_value)  corrFlow = qobsi(i)          
        ENDIF
      ENDIF
    ENDIF
     
  END SUBROUTINE apply_quseobs
   
  !>Update outflow of subbasin from observed value with AR method
  !----------------------------------------------------------------
  SUBROUTINE apply_qarupd(i,simflow,corrFlow,arcorr)

    USE MODVAR, ONLY : i_qar, &
                       missing_value, &
                       qobsi, &
                       useinupdate
     
    !Argument declaration
    INTEGER, INTENT(IN)            :: i           !<index of current subbasin
    REAL, INTENT(IN)               :: simflow     !<simulated outflow of subbasin
    REAL, INTENT(INOUT)            :: corrFlow    !<updated outflow (lakeoutflow)
    REAL, INTENT(INOUT)            :: arcorr      !<current AR-error (state-variable)
     
    IF(ALLOCATED(useinupdate(i_qar)%station)) THEN 
      IF(useinupdate(i_qar)%station(i)) THEN
        IF(ALLOCATED(qobsi)) THEN
          IF(qobsi(i)/=missing_value) THEN  !calculates the error
            arcorr =  simflow - qobsi(i)
          ELSE  !no observation, using AR
            arcorr = arcorr * useinupdate(i_qar)%factor(i) !Updating AR-correction  
            corrFlow = simflow - arcorr
            IF(corrFlow<0.) corrFlow=0.
          ENDIF
        ELSE  !no observation, using AR
          arcorr = arcorr * useinupdate(i_qar)%factor(i) !Updating AR-correction  
          corrFlow = simflow - arcorr
          IF(corrFlow<0.) corrFlow=0.
        ENDIF
      ENDIF
    ENDIF
  
  END SUBROUTINE apply_qarupd
   
  !>Update outflow of subbasin from observed waterstage value with AR method
  !>This method only applies for lakes with rating curve.
  !-------------------------------------------------------------------------
  SUBROUTINE apply_warupd(i,ioutlet,lakeareain,wstold,corrWst,corrFlow,arcorr,corrWstAve)

    USE MODVAR, ONLY : basin, &
                       i_war, &
                       missing_value, &
                       useinupdate, &
                       wobsvarindex, &
                       xobsi,  &
                       xobsindex
    USE SURFACEWATER_PROCESSES, ONLY : calculate_olake_waterstage, &
                                       calculate_flow_from_outlet_lake_waterstage
  
    !Argument declaration
    INTEGER, INTENT(IN)            :: i           !<index of current subbasin
    INTEGER, INTENT(IN)            :: ioutlet     !<index of outlet with main outflow which flow will be affected
    REAL, INTENT(IN)               :: lakeareain  !<olake area of subbasin (m2)
    REAL, INTENT(IN)               :: wstold      !<simulated lake water end of last time step (mm)
    REAL, INTENT(INOUT)            :: corrWst     !<IN: simulated lake water, OUT: updated lake water (lakewst - for print out only) (mm)
    REAL, INTENT(INOUT)            :: corrFlow    !<updated outflow (lakeoutflow, m3/s)
    REAL, INTENT(INOUT)            :: arcorr      !<current AR-error (state-variable, mm)
    REAL, INTENT(INOUT)            :: corrWstAve  !<updated average lake water (mm)

    !Local variables     
    REAL corrWstold     !updated water stage last time step (mm)
    REAL errorw   !current error in wst in mm
    REAL lakearea !lake area of outlet lake (m2) (whole lake of last lakebasin)
    REAL qoutold  !outflow at old waterstage
    REAL qoutnew  !outflow at new waterstage
    REAL wstobs   !observed lake waterstage wstr from Xobs in w-ref system (m)
    REAL wstm     !lake water stage in local system (m)
    REAL w0ref    !waterstage reference level (m)
    
    !>\b Algorithm \n
    lakearea = lakeareain

    IF(ALLOCATED(useinupdate(i_war)%station)) THEN 
      IF(useinupdate(i_war)%station(i)) THEN
        !>Calculate updated waterstage last timestep
        corrWstold = wstold - arcorr
        !>Calculate error and new arcorr-factor
        IF(ALLOCATED(xobsi))THEN
          IF(xobsindex(wobsvarindex,i)>0)THEN
            wstobs = xobsi(xobsindex(wobsvarindex,i))  !Get current wst observation, m in wref-system
          ELSE
            wstobs = missing_value
          ENDIF
          IF(wstobs/=missing_value)THEN
            CALL calculate_olake_waterstage(i,corrWst,wstm,w0ref)
            !Check observation against lake depth (coarse check to find totally erronous obs)
            IF(wstobs<=w0ref-basin(i)%lakedepth(2)) wstobs = missing_value
          ENDIF
          IF(wstobs/=missing_value)THEN
            !Calculate current error of water stage
            errorw = (wstobs-w0ref-wstm)*1000.         !mm
            arcorr = - errorw
          ELSE  !no observation, use wAR to update wst and q
            arcorr = arcorr * useinupdate(i_war)%factor(i) !Updating AR-correction  
          ENDIF
        ELSE  !no observation, use wAR to update wst and q
          arcorr = arcorr * useinupdate(i_war)%factor(i) !Updating AR-correction  
        ENDIF
        !>Apply AR correction to waterstage and discharge
        corrWst = corrWst - arcorr
        CALL calculate_flow_from_outlet_lake_waterstage(i,ioutlet,lakeareain,corrWstold,qoutold)
        CALL calculate_flow_from_outlet_lake_waterstage(i,ioutlet,lakeareain,corrWst,qoutnew)
        corrFlow = 0.5*(qoutold+qoutnew)
        IF(corrFlow<0.) corrFlow=0.
        corrWstAve = (corrWstold + corrWst)/2.
      ENDIF
    ENDIF
  
  END SUBROUTINE apply_warupd
   
  !>Update outlet lake water stage to observed value
  !-----------------------------------------------------------
  SUBROUTINE apply_wendupd(i,wst,lakestate)
  
    USE MODVAR, ONLY : i_wendupd, &
                       missing_value, &
                       useinupdate, &
                       wobsvarindex, &
                       xobsi, &
                       xobsindex
                       

    USE SURFACEWATER_PROCESSES, ONLY : calculate_olake_waterstage, &
                                       calculate_regamp_adjusted_waterstage
     
  !  !Argument declaration
    INTEGER, INTENT(IN)            :: i            !<index of subbasin
    REAL, INTENT(INOUT)            :: wst          !<water in lake (mm) to be written output
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate !<Lake state
     
    !Local variables
    REAL wstobs   !observed wstr from Xobs in w-ref system
    REAL wstobs2  !observed wstr in local system (above threshold) (adjusted for wamp)
    REAL wstm     !lake water stage above threshold (not adjusted for wamp) (m)
    REAL w0ref    !waterstage reference level (m)
    REAL deltaw   !change in calculated water stage due to updating (mm)
     
    IF(useinupdate(i_wendupd)%station(i)) THEN
      wstobs = xobsi(xobsindex(wobsvarindex,i))   !m
      IF(wstobs/=missing_value)THEN
        !Calculate change of water stage and apply to output variable and state
        CALL calculate_olake_waterstage(i,wst,wstm,w0ref)
        CALL calculate_regamp_adjusted_waterstage(i,wstobs-w0ref,wstobs2)
        deltaw = (wstobs2-wstm)*1000.         !mm
        wst = wst + deltaw      !wst = wstobs (mm)
        lakestate%water(2,i) = wst        !Apply updating on state variable for lake water
      ENDIF
    ENDIF
     
  END SUBROUTINE apply_wendupd
   
  !>Update all lake basin water stage to observed value of outlet
  !--------------------------------------------------------------
  SUBROUTINE apply_wendupd_lakebasin_lake(i,looplakes,wst,lakestate)
  
    USE MODVAR, ONLY : i_wendupd, &
                       missing_value,   &
                       nsub, &
                       useinupdate, &
                       wobsvarindex, &
                       xobsi,   &
                       xobsindex
    USE SURFACEWATER_PROCESSES, ONLY : calculate_olake_waterstage, &
                                       calculate_regamp_adjusted_waterstage
     
    !Argument declaration
    INTEGER, INTENT(IN)            :: i            !<index of last lakebasin
    LOGICAL, INTENT(IN)            :: looplakes(nsub)  !<subbasins that belong to lake
    REAL, INTENT(INOUT)            :: wst(nsub)    !<water in lake (mm) to be written output
    TYPE(lakestatetype),INTENT(INOUT) :: lakestate !<Lake state
     
    !Local variables
    INTEGER isb   !loop index subbasins
    REAL wstobs   !observed wstr from Xobs in w-ref system
    REAL wstobs2  !observed wstr in local system (above threshold) (adjusted for wamp)
    REAL wstm     !lake water stage above threshold (not adjusted for wamp) (m)
    REAL w0ref    !waterstage reference level (m)
    REAL deltaw   !change in calculated water stage due to updating (mm)
     
    IF(useinupdate(i_wendupd)%station(i)) THEN
      wstobs = xobsi(xobsindex(wobsvarindex,i))   !m
      IF(wstobs/=missing_value)THEN
        !Calculate change of water stage in all lake basins, and apply to output variable and state
        DO isb = 1,i
          IF(looplakes(isb))THEN
            CALL calculate_olake_waterstage(isb,wst(isb),wstm,w0ref)
            CALL calculate_regamp_adjusted_waterstage(isb,wstobs-w0ref,wstobs2)
            deltaw = (wstobs2-wstm)*1000.         !mm
            wst(isb) = wst(isb) + deltaw      !wst = wstobs (mm)
            lakestate%water(2,isb) = wst(isb)        !Apply updating on state variables for lake water
          ENDIF
        ENDDO
      ENDIF
    ENDIF
     
  END SUBROUTINE apply_wendupd_lakebasin_lake
   
  !>Update concentrations of subbasin outflow to observed value
  !-------------------------------------------------
  SUBROUTINE apply_cuseobs(i,newconc)

    USE MODVAR, ONLY : i_cuseobs, &
                       i_t1,i_in,i_on,i_sp,i_pp,i_oc,i_ss,i_ae,i_t2, &
                       missing_value, &
                       numsubstances, &
                       simulate, &
                       useinupdate, &
                       xobsi, &
                       xobsindex
    USE HYPEVARIABLES, ONLY : o_reT1,o_reT2, &
                              o_reIN,o_reON,o_reOC, &
                              o_reSP,o_rePP, &
                              o_reSS,o_reAE
     
    !Argument declaration
    INTEGER, INTENT(IN) :: i         !<index of current subbasin
    REAL, INTENT(INOUT) :: newconc(numsubstances)   !<simulated concentration of subbasin !!(clakeoutflow)
     
    IF(ALLOCATED(useinupdate(i_cuseobs)%station))THEN 
      IF(useinupdate(i_cuseobs)%station(i))THEN
        IF(ALLOCATED(xobsindex))THEN
          IF(simulate%substance(i_t1) .AND. xobsindex(o_reT1,i)>0)THEN
            IF(xobsi(xobsindex(o_reT1,i))/=missing_value) newconc(i_t1) = xobsi(xobsindex(o_reT1,i))
          ENDIF
          IF(simulate%substance(i_t2) .AND. xobsindex(o_reT2,i)>0)THEN
            IF(xobsi(xobsindex(o_reT2,i))/=missing_value) newconc(i_t2) = xobsi(xobsindex(o_reT2,i))
          ENDIF
          IF(simulate%substance(i_in) .AND. xobsindex(o_reIN,i)>0)THEN
            IF(xobsi(xobsindex(o_reIN,i))/=missing_value) newconc(i_in) = xobsi(xobsindex(o_reIN,i))*1.E-3  !ug/L->mg/L
          ENDIF
          IF(simulate%substance(i_on) .AND. xobsindex(o_reON,i)>0)THEN
            IF(xobsi(xobsindex(o_reON,i))/=missing_value) newconc(i_on) = xobsi(xobsindex(o_reON,i))*1.E-3  !ug/L->mg/L
          ENDIF
          IF(simulate%substance(i_oc) .AND. xobsindex(o_reOC,i)>0)THEN
            IF(xobsi(xobsindex(o_reOC,i))/=missing_value) newconc(i_oc) = xobsi(xobsindex(o_reOC,i))
          ENDIF
          IF(simulate%substance(i_sp) .AND. xobsindex(o_reSP,i)>0)THEN
            IF(xobsi(xobsindex(o_reSP,i))/=missing_value) newconc(i_sp) = xobsi(xobsindex(o_reSP,i))*1.E-3  !ug/L->mg/L
          ENDIF
          IF(simulate%substance(i_pp) .AND. xobsindex(o_rePP,i)>0)THEN
            IF(xobsi(xobsindex(o_rePP,i))/=missing_value) newconc(i_pp) = xobsi(xobsindex(o_rePP,i))*1.E-3  !ug/L->mg/L
          ENDIF
          IF(simulate%substance(i_ss) .AND. xobsindex(o_reSS,i)>0)THEN
            IF(xobsi(xobsindex(o_reSS,i))/=missing_value) newconc(i_ss) = xobsi(xobsindex(o_reSS,i))
          ENDIF
          IF(simulate%substance(i_ae) .AND. xobsindex(o_reAE,i)>0)THEN
            IF(xobsi(xobsindex(o_reAE,i))/=missing_value) newconc(i_ae) = xobsi(xobsindex(o_reAE,i))
          ENDIF
        ENDIF
      ENDIF
    ENDIF
     
  END SUBROUTINE apply_cuseobs
   
  !>Update nutrient concentration to fraction of modelled value
  !----------------------------------------------------------------------
  SUBROUTINE apply_nutrientcorr(icorr,isub,conc1,conc2)

  USE MODVAR, ONLY : useinupdate
  
    !Argument declaration
    INTEGER, INTENT(IN) :: icorr        !<index for type of updating/correction
    INTEGER, INTENT(IN) :: isub         !<update subbasin
    REAL, INTENT(INOUT) :: conc1        !<simulated concentration (SP,IN) of outflow of subbasin (clakeoutflow)
    REAL, INTENT(INOUT) :: conc2        !<simulated concentration (PP,ON) of outflow of subbasin (clakeoutflow)
     
    REAL  factor    !current correction factor
     
    IF(useinupdate(icorr)%factor(isub)/=0.)THEN
      factor = 1. + useinupdate(icorr)%factor(isub)
      conc1  = conc1*factor
      conc2  = conc2*factor
    ENDIF
     
  END SUBROUTINE apply_nutrientcorr

END MODULE

