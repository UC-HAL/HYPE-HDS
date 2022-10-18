!> \file general_wc.f90
!> Contains module general_water_concentration.

!>Routines handling water and concentration in HYPE
MODULE GENERAL_WATER_CONCENTRATION

  !Copyright 2012-2015,2017-2019 SMHI
  !
  !This file is part of HYPE.
  !HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
  !HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
  !You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.

  !-----------------------------------------------------------------------------------------

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: retention_pool, &
            production_pool, &
            new_concentration, &
            remove_water, &
            add_water, &
            error_remove_water, &
            add_source_to_water, &
            inflow_lowest_soillayer, &
            calculate_water_fractions

  REAL, PARAMETER    :: realzero = 1.E-37       !<Value of zero in real

CONTAINS

  !>\brief Removes an amount of substances from a pool. 
  !!If the sink is larger than the pool it removes all of the pool and
  !!changes the sink to what was actually removed. If sink is negativ it is added.
  !-------------------------------------------------------------------
  SUBROUTINE retention_pool(n,pool,sink)

    !Argument declarations
    INTEGER, INTENT(IN) :: n       !<number of soillayers = size of pool-array
    REAL, INTENT(INOUT) :: pool(n) !<soil pool array
    REAL, INTENT(INOUT) :: sink(n) !<amount to be removed
    
    !Local variables
    REAL a(N)

    a = pool - sink
    WHERE(a>=realzero)
      pool = a
    ELSEWHERE
      sink = pool
      pool = 0.
    ENDWHERE

  END SUBROUTINE retention_pool

  !>Add an amount of substances to a pool. If source is negative it is 
  !>removed (without checking pool size!).
  !-------------------------------------------------------------------
  SUBROUTINE production_pool(n,pool,source)

    !Argument declarations
    INTEGER, INTENT(IN) :: n         !<number of soillayers = size of pool-array
    REAL, INTENT(INOUT) :: pool(n)   !<soil pool array
    REAL, INTENT(IN)    :: source(n) !<amount to be added

    pool = pool + source

  END SUBROUTINE production_pool

  !>Calculates concentration based on water volume and amount of one substance
  !---------------------------------------------------------------------
  SUBROUTINE new_concentration(pool,vol,conc)

    !Argument declarations
    REAL, INTENT(IN)    :: pool !<amount          (eg. kg/km2)
    REAL, INTENT(IN)    :: vol  !<volume          (eg. mm)
    REAL, INTENT(INOUT) :: conc !<concentation    (eg. mg/L)

    IF(vol>realzero)THEN
      conc = pool / vol
    ELSE
      conc = 0.
    ENDIF

  END SUBROUTINE new_concentration

  !>Subroutine remove a flow (with substances) from a water body (with
  !>concentration)
  !--------------------------------------------------------------------
  SUBROUTINE remove_water(vol,n,conc,q,cq,err)

    !Argument declarations
    REAL, INTENT(INOUT)  :: vol     !<water body
    INTEGER, INTENT(IN)  :: n       !<numsubstance = size of conc-array
    REAL, INTENT(INOUT)  :: conc(n) !<conc of water body
    REAL, INTENT(IN)     :: q       !<water to be removed 
    REAL, INTENT(IN)     :: cq(n)   !<conc of water to be removed    
    INTEGER, INTENT(OUT) :: err     !<error code

    !Local variables
    INTEGER i   

    err = 0
    IF(vol - q > 0.)THEN
      DO i =1,n
        IF(conc(i)/=cq(i)) conc(i) = (conc(i)*vol - cq(i)*q)/(vol - q)
      ENDDO
      vol   = vol - q
    ELSEIF(vol - q > -realzero)THEN
    !ELSEIF(vol - q == 0.)THEN
      DO I = 1,n
        IF(conc(I)==cq(I) .OR. ISNAN(conc(I)).OR.ISNAN(cq(I)))THEN
          conc(I) = 0.0                 !Takes care of NaN and Infinity concentrations, better way?
        ELSEIF((conc(I)*vol - cq(I)*q) == 0.0)THEN
          conc(I) = 0.0
        ELSE
          WRITE(6,*) 'cq',cq(i),'cvol',conc(i)
          WRITE(6,*) 'q',q,'vol',vol
          WRITE(6,*) 'ERROR - remove water routine cq<>conc'
          err = 1
        ENDIF
      ENDDO
      vol = 0.
    ELSE
      WRITE(6,*) 'ERROR - remove water routine Q>VOL'
      WRITE(6,*) 'ERROR - Q=',q,'VOL=',vol
      err = 1
    ENDIF

  END SUBROUTINE remove_water

  !>Subroutine add a flow/volume (with substances) to a water body
  !>(with concentration)
  !-----------------------------------------------------------------
  SUBROUTINE add_water(n,vol,conc,q,cq)

    !Argument declarations
    INTEGER, INTENT(IN) :: n       !<numsubstance = size of conc-array
    REAL, INTENT(INOUT) :: vol     !<water body
    REAL, INTENT(INOUT) :: conc(n) !<conc of water body
    REAL, INTENT(IN)    :: q       !<water to be added
    REAL, INTENT(IN)    :: cq(n)   !<conc of water to be added    
    
    !Local variables
    REAL newvol

    newvol = vol + q
    IF(newvol > realzero)THEN
      conc = (conc*vol + cq*q)/newvol
      vol = newvol
    ELSE
      conc = 0.
      vol = 0.
    ENDIF

  END SUBROUTINE add_water

  !>Handle error in remove_water subroutine
  !---------------------------------------------------
  SUBROUTINE error_remove_water(rstring,subid,i,j)

    CHARACTER(LEN=*), INTENT(IN)      :: rstring !<error location in code
    INTEGER, INTENT(IN)                :: subid   !<current subid
    INTEGER, INTENT(IN)                :: i       !<current subbasin
    INTEGER, INTENT(IN)                :: j       !<current class or current laketype

    WRITE(6,*) 'ERROR: '//TRIM(rstring)
    WRITE(6,*) 'Subbasin subid:',subid
    WRITE(6,*) 'Subbasin index:',i
    WRITE(6,*) 'SLC or laketype:',j
    STOP 1

  END SUBROUTINE error_remove_water

  !>Subroutine add an amount of substance to a water body and
  !>recalculate the concentration
  !----------------------------------------------------------------
  SUBROUTINE add_source_to_water(vol,n,conc,source)

    !Argument declarations
    REAL, INTENT(IN)    :: vol        !<water body
    INTEGER, INTENT(IN) :: n          !<numsubstance = size of conc-array
    REAL, INTENT(INOUT) :: conc(n)    !<conc of water body
    REAL, INTENT(IN)    :: source(n)  !<amount to be added

    IF(vol > realzero)THEN
       conc(:) = (conc(:)*vol + source(:))/ vol
    ENDIF

  END SUBROUTINE add_source_to_water

  !>Subroutine for adding inflow to lowest soillayer including
  !>substances. May result in flow upward through layers.
  !-------------------------------------------------------------------------
  SUBROUTINE inflow_lowest_soillayer(n,nl,cin,qin,maxwc,layer,soil,csoil,upwardflow,addedflow,lowestlayer)

    !Argument declarations
    INTEGER, INTENT(IN) :: n             !<number of substances
    INTEGER, INTENT(IN) :: nl            !<maximum number of soil layers
    REAL, INTENT(IN)    :: cin(n)        !<concentration of inflow 
    REAL, INTENT(IN)    :: qin           !<inflow (mm)
    REAL, INTENT(IN)    :: maxwc(nl)     !<maximum soil moisture (mm)
    REAL, INTENT(IN)    :: layer(nl)     !<thickness of soillayers (m)
    REAL, INTENT(INOUT) :: soil(nl)      !<soil moisture (mm)
    REAL, INTENT(INOUT) :: csoil(n,nl)   !<concentration of soil 
    REAL, INTENT(OUT)   :: upwardflow(2) !<Upwelling due to overflowing lower soil layers (mm/timestep)
    REAL, INTENT(OUT)   :: addedflow(nl) !<Added flow (mm/timestep)
    INTEGER, INTENT(OUT) :: lowestlayer   !<Soil layer flow was added

    !Local variables
    INTEGER sl,l
    REAL overflow

    upwardflow = 0.
    addedflow = 0.
    overflow = 0.
    !Add inflow to lowest soil layer
    DO sl = nl,1,-1
      IF(layer(sl)>0)THEN
        csoil(:,sl) = (csoil(:,sl) * soil(sl) + cin(:) * qin) / (soil(sl) + qin)
        soil(sl)    = soil(sl) + qin
        addedflow(sl) = qin
        lowestlayer = sl
        EXIT
      ENDIF
    ENDDO
    !Calculate flow upward
    DO l = sl,2,-1
      IF(soil(l)>maxwc(l))THEN
        overflow     = soil(l) - maxwc(l)
        csoil(:,l-1) = (csoil(:,l-1) * soil(l-1) + csoil(:,l) * overflow) / (soil(l-1) + overflow)
        soil(l-1)    = soil(l-1) + overflow
        soil(l)      = soil(l) - overflow
        upwardflow(l-1) = overflow
      ENDIF
    ENDDO

  END SUBROUTINE inflow_lowest_soillayer

  !>\brief Calculate volume fractions of water compartments (used for rivers)
  !----------------------------------------------------------------------------
  SUBROUTINE calculate_water_fractions(m,lastpart,water1,water2,wqueue,totvol,fractionw1,fractionw2,fractionqueue,fractionlast)

    INTEGER, INTENT(IN) :: m      !<dimension (length of river queue)
    REAL, INTENT(IN)  :: lastpart !<fraction of last queued compartment (m+1) that is active
    REAL, INTENT(IN)  :: water1   !<primary water compartment (m3)
    REAL, INTENT(IN)  :: water2   !<secondary water compartment (m3)
    REAL, INTENT(IN)  :: wqueue(m+1) !<queued water compartments (m3)
    REAL, INTENT(OUT) :: totvol      !<total volume in river (m3)
    REAL, INTENT(OUT) :: fractionw1  !<primary water compartment's fraction of total volume
    REAL, INTENT(OUT) :: fractionw2  !<secondary water compartment's fraction of total volume
    REAL, INTENT(OUT) :: fractionqueue(m) !<queued water compartments' fractions of total volume
    REAL, INTENT(OUT) :: fractionlast     !<for the last queued water compartment, the fraction is based on whole volume
    
    !Local parameters and variables
    REAL totvol0
    INTEGER k
    
    !>\b Algorithm \n
    !>Initialization
    fractionw1 = 0.; fractionw2 = 0.; fractionqueue = 0.; fractionlast = 0.
    totvol = 0.

    !>Calculate total volume as a baseline for checking fractions of water in different compartment
    totvol0 = water1 + water2 + SUM(wqueue(1:m)) + wqueue(m+1) * lastpart
    
    IF(totvol0>realzero)THEN

      !>Calculate which water compartments that is positive and thus included in the total volume
      IF(water1/totvol0>realzero)THEN
        totvol = totvol + water1
        fractionw1 = 1.
      ENDIF
      IF(water2/totvol0>realzero)THEN
        totvol = totvol + water2
        fractionw2 = 1.
      ENDIF
      DO k = 1,m
        IF(wqueue(k)/totvol0>realzero)THEN
          totvol = totvol + wqueue(k)
          fractionqueue(k) = 1.
        ENDIF
      ENDDO
      IF(lastpart>0)THEN
        IF(wqueue(m + 1)/totvol0>realzero)THEN
          totvol = totvol + wqueue(m + 1) * lastpart
          fractionlast = 1.
        ENDIF
      ENDIF

      !>Calculate fractions of water compartments
      IF(fractionw1>0.) fractionw1 = water1/totvol
      IF(fractionw2>0.) fractionw2 = water2/totvol
      DO k = 1,m
        IF(fractionqueue(k)>0.) fractionqueue(k) = wqueue(k)/totvol
      ENDDO
      IF(fractionlast>0.) fractionlast = wqueue(m+1)/totvol    !Note: "fraction" is based on whole volume so that scaling remains correct when active fraction is later used to remove water from queue
    ENDIF
    
  END SUBROUTINE calculate_water_fractions


END MODULE GENERAL_WATER_CONCENTRATION
