!> \file general_func.f90
!> Contains module general_functions.

!>General equations that are used by hydrological models
MODULE GENERAL_FUNCTIONS

!Copyright 2012,2014,2016-2020 SMHI
!
!This file is part of HYPE.
!HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
!You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.

!-----------------------------------------------------------------------------------------

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: tempfactor, &
            halfsatconcfactor, &
            moisturefactor, &
            exponential_moisturefactor, &
            simple_rating_curve,  &
            exponential_decay,  &
            sedimentation, &
            plant_growth, &
            sigmoid_response, &
            linear_response

CONTAINS

  ! >\brief Calculates a temperature factor 
  !! Based on: Q10=2,reference rate is at 20 degrees and with
  !! thresholds at 0 and 5 degrees (from COUP-model).
  ! --------------------------------------------------------------
  REAL FUNCTION tempfactor(temp)  
      
    REAL, INTENT(IN)  :: temp     !<current temperature
    !Local variables
    REAL fcn
    
    fcn = 2**((temp - 20.0) / 10.0)
    IF(temp < 5.0) fcn = fcn * (temp / 5.0)
    IF(temp < 0.)  fcn = 0.
    tempfactor = fcn

  END FUNCTION tempfactor
      
  !>\brief Calculates a concentration dependence factor 
  !! Based on: half saturation function
  !----------------------------------------------------------------
  REAL FUNCTION halfsatconcfactor(conc,par)  
      
    REAL, INTENT(IN)  :: conc     !<current concentration
    REAL, INTENT(IN)  :: par      !<half saturation concentration
    !Local variables
    REAL fcn
    
    fcn = 0.
    IF(conc+par>0.) fcn = conc / (conc + par)
    halfsatconcfactor = fcn
  
  END FUNCTION halfsatconcfactor
      
  !> Calculates a soil moisture dependence factor
  !-----------------------------------------------------------------------------
  REAL FUNCTION moisturefactor(sm,wp,pw,thickm,satsmf,exp,thetalow,thetaupp)
      
    REAL, INTENT(IN)  :: sm     !<soil moisture (mm)
    REAL, INTENT(IN)  :: wp     !<wilting point pore wolume (mm)
    REAL, INTENT(IN)  :: pw     !<total pore wolume (mm)
    REAL, INTENT(IN)  :: thickm !<thickness of soil layer (m)
    REAL, INTENT(IN)  :: satsmf !<saturated moisturefactor (satact)
    REAL, INTENT(IN)  :: exp    !<exponent of moisturefactor (thetapow)
    REAL, INTENT(IN)  :: thetalow !<low(?) moisture coefficient (thetalow)
    REAL, INTENT(IN)  :: thetaupp !<high(?) moisture coefficient (thetaupp)
    
    ! Local variables
    REAL thickness  !thickness of soil layer (mm)
    REAL smfcn      !soil moisture dependence factor
    
    !Initiations    
    smfcn = 0.
    thickness = thickm * 1000.      !mm
    
    !Caclulate soilmoisture function value
    IF(thickness>0)THEN
      IF(sm >= pw) THEN   
        smfcn = satsmf
      ELSEIF(sm <= wp) THEN
        smfcn=0.
      ELSE 
        smfcn = min(1., (1-satsmf)*((pw-sm)/((thetaupp/100.)*thickness))**exp + satsmf, ((sm-wp)/((thetalow/100.)*thickness))**exp)    
      ENDIF
    ENDIF
    
    moisturefactor = smfcn

  END FUNCTION moisturefactor
      
  !>Calculates an exponential soil moisture dependence factor
  !-----------------------------------------------------------------------------
  REAL FUNCTION exponential_moisturefactor(sm,pw,limpar,exp)
      
    REAL, INTENT(IN)  :: sm     !<soil moisture (mm)
    REAL, INTENT(IN)  :: pw     !<total pore wolume (mm)
    REAL, INTENT(IN)  :: limpar !<limitation parameter of moisturefactor (mm)
    REAL, INTENT(IN)  :: exp    !<exponent of moisturefactor
    
    ! Local variables
    REAL smfcn
    
    !Initiations    
    smfcn = 0.
    
    !Calculate soilmoisture function value
    !IF((sm/pw) > limpar)THEN       !Not limited for standing water. Changed summer 2019.
    !  smfcn = (((sm / pw)-limpar)/(1.0-limpar))**exp
    !ENDIF
    IF(sm > pw)THEN
      smfcn = 1.
    ELSEIF((sm/pw) > limpar)THEN
      smfcn = (((sm / pw)-limpar)/(1.0-limpar))**exp
    ENDIF
    
    exponential_moisturefactor = smfcn
    
  END FUNCTION exponential_moisturefactor

  !>Calculates momentanous flow from current water level with simple 
  !>rating curve equation: Q = k*(w-w0)**p
  !-----------------------------------------------------------------------------
  REAL FUNCTION simple_rating_curve(wst,k,p,w0)
      
    !Argument declarations
    REAL, INTENT(IN) :: wst    !<current water level (m)
    REAL, INTENT(IN) :: k      !<rating curve coefficient
    REAL, INTENT(IN) :: p      !<rating curve exponent
    REAL, INTENT(IN) :: w0     !<rating curve threshold (m)

    !Local variables
    REAL flow      !discharge (m3/s)
    
    !Calculate discharge
    IF(wst > w0)THEN
      flow = k * (wst-w0)**p
    ELSE
      flow = 0.
    ENDIF
    
    simple_rating_curve = flow
    
  END FUNCTION simple_rating_curve

  !>Calculates factor of exponential decay from halftime
  !-----------------------------------------------------------------------------
  REAL FUNCTION exponential_decay(timesteps,halftime)
      
    !Argument declarations
    REAL, INTENT(IN) :: timesteps     !<in unit of half time
    REAL, INTENT(IN) :: halftime      !<half time of decay

    !Local variables
    REAL factor      !fraction left
    
    !Calculate discharge
    IF(halftime>0)THEN
      factor = EXP(-LOG(2.)*timesteps/halftime)
    ELSE
      WRITE(6,*) 'ERROR: half time parameter is zero. Check parameter file.'
      STOP 1
    ENDIF
    
    exponential_decay = factor
    
  END FUNCTION exponential_decay

  !>\brief Calculate sedimentation from sinking velocity
  !-----------------------------------------------------------------
  SUBROUTINE sedimentation(depth,conc,vel)

    !Argument declarations
    REAL, INTENT(IN)              :: depth      !<water depth (m)
    REAL, INTENT(INOUT)           :: conc       !<concentration of substance to sedimentate (mg/L or something)
    REAL, INTENT(IN)              :: vel        !<sinking velocity (m/timestep)

    !Local variables
    REAL pool      !pool in water (g/m2)
    REAL sedloss   !change (g/m2/timestep)

    !Calculate pool
    pool = depth * conc

    !Calculate sedimentation
    sedloss = vel * conc

    !Remove sedimentation from the pool and calculate new concentration
    IF(pool>sedloss)THEN
      pool = pool - sedloss
      conc = pool / depth
    ELSE
      conc = 0.
    ENDIF
 
  END SUBROUTINE sedimentation

  !>\brief Calculates a logistic growth for plant based on a 
  !>three-parameter equation from SOILN.
  !>
  !>Reference: Eckersten, H., P.-E. Jansson, and H. Johnsson 1994.
  !>SOILN model – user’s manual 2nd edition, Division of Agricultural 
  !>Hydrotechnics Communications 94:4, Department of Soil Sciences, 
  !>Swedish University of AgriculturalSciences, 58pp, Uppsala).
  ! --------------------------------------------------------------
  REAL FUNCTION plant_growth(up1,up2,up3,day)  
      
    REAL, INTENT(IN) :: up1     !<parameter 1
    REAL, INTENT(IN) :: up2     !<parameter 2
    REAL, INTENT(IN) :: up3     !<parameter 3
    REAL, INTENT(IN) :: day  !<variable

    !Local variables
    REAL help,fcn
    
    help = (up1-up2)*EXP(-up3*day)
    fcn = 0.
    IF(up2+help>0.) fcn = up1*up2*up3*help/(up2+help)/(up2+help)
    plant_growth = fcn

  END FUNCTION plant_growth

  !>\brief Flexible sigmoid function to approximate a response function
  !>(for instance a growth or depletion curve) with well defined parameters.
  !!      
  !!References: Yin et al., Annals of Botany 91: 361-371, 2003, doi:10.1093/aob/mcg029
  !!            Pimentel et al., Hydrol. Earth Syst. Sci., 21, 805–820, 2017
  !!
  !>The original function by Yin et al is defined using xs=0 and ys=0:
  !!
  !>   y = ye * (1 + (xe - x)/(xe - xm))(x/xe)^(xe / (xe-xm)), with 0<= xm < xe
  !!
  !>We can generalize this to have response y between ys and ye for input variable xs to xe:
  !!
  !>   y = ys + (ye-ys)*(1 + (xet-xt)/(xet-xmt))*(xt/xet)^(xet/(xet-xmt)), 
  !>   with xt=x-xs, xet=xe-xs, xmt=xm-xs, and xs <= xm < xe
  !------------------------------------------------------------------
  REAL FUNCTION sigmoid_response(x,xs,xe,ys,ye,xm)
    
    !Argument declarations
    REAL, INTENT(IN) :: x   !<input variable (e. snow depth, water level, temperature, etc)
    REAL, INTENT(IN) :: xs  !<x value defining the start of the response function (for instance 0.)
    REAL, INTENT(IN) :: xe  !<x value defining the end of the response function (for instance 1.)
    REAL, INTENT(IN) :: ys  !<y value at the start of the response function (for instance 0.)
    REAL, INTENT(IN) :: ye  !<y value at the end of the response function (for instance 1.)
    REAL, INTENT(IN) :: xm  !<x value with maximum slope of the response function (any value between xs and xe)
    
    !Local variables
    REAL :: xt, xet, xmt

    !transform x, xe and xm to be in a range between 0 and xe-xs
    xt=x-xs
    xet=xe-xs
    xmt=xm-xs
    
    !relative response in the range 0 to 1
    IF(xt.LE.0.)THEN
      sigmoid_response = 0.
    ELSEIF(xt.GE.xet)THEN
      sigmoid_response = 1.
    ELSE
      sigmoid_response = (1. + (xet-xt)/(xet-xmt)) * ( (xt/xet)**(xet/(xet-xmt)) )
    ENDIF
    
    !response scaled to the range ys to ye
    sigmoid_response = ys + (ye-ys)*sigmoid_response
  
  END FUNCTION sigmoid_response

  !>\brief Linear function between (xs,ys) and (xe,ye)
  !>   y = ys + (ye-ys)/(xe-xs))*x 
  !>   with xs <= x < xe
  !------------------------------------------------------------------
  REAL FUNCTION linear_response(x,xs,xe,ys,ye)
    
    !Argument declarations
    REAL, INTENT(IN) :: x   !<input variable (e. snow depth, water level, temperature, etc)
    REAL, INTENT(IN) :: xs  !<x value defining the start of the response function (for instance 0.)
    REAL, INTENT(IN) :: xe  !<x value defining the end of the response function (for instance 1.)
    REAL, INTENT(IN) :: ys  !<y value at the start of the response function (for instance 0.)
    REAL, INTENT(IN) :: ye  !<y value at the end of the response function (for instance 1.)
    
    !Local variables
    REAL :: xt, xet

    !transform x, xe and xm to be in a range between 0 and xe-xs
    xt=x-xs
    xet=xe-xs
    
    !relative response in the range 0 to 1
    IF(xt.LE.0.)THEN
      linear_response = 0.
    ELSEIF(xt.GE.xet)THEN
      linear_response = 1.
    ELSE
      linear_response = 1./(xe-xs)
    ENDIF
    
    !response scaled to the range ys to ye
    linear_response = ys + (ye-ys)*linear_response
  
  END FUNCTION linear_response

  END MODULE GENERAL_FUNCTIONS
