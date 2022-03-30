!> \file modvar.f90
!> Contains module modvar.

!>\brief Declaration of model data types and variables
!>
!>This is the interface between the model and the simulation system
!!"HYSS". NOTE: everything is public
!>
!>The module modvar contains data types, constants, variables, and procedures 
!>that are HYSS provide for the model. It also holds procedures 
!>for allocating and setting its variables and to locate data in them.
!>
!>The constants define specific numbers, dimensions of arrays, and codes 
!>for accumulation period, model parameter dependence and updating methods.
!>
!>The variables are for holding input data on model set-up, substance modelled,
!>forcing data of current time step, state variables that are common for hydrological 
!>models, variables of (current) time, size of model set-up, settings for 
!>the model run (e.g. output, model parameter values, updating information), 
!>and variables for temporary state transformation.

MODULE MODVAR

!Copyright 2011-2020 SMHI
!
!This file is part of HYPE.
!HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
!You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.

!-----------------------------------------------------------------------------------------
!Procedures in this module (all public)
!-----------------------------------------------------------------------------------------
! allocate_outvar
! reallocate_outvar
! deallocate_modvar
! deallocate_glacier
! allocate_modelparameters
! allocate_basinvariables
! allocate_initialize_modeloptions
! find_next_watertransfer
! find_next_pointsource
! find_next_abstraction
! initiate_xobsinformation
! save_xobsinformation
! set_soildepth
! set_coded_classes
! initiate_cropdata
! find_cropdata
! find_croppart
! get_pseudo_dayno
! in_season_end
! in_season_period
! find_lbtype
! find_lakebasins
! check_connected_subbasins
! is_upstream_lakebasin
!-----------------------------------------------------------------------------------------
  USE LIBDATE, ONLY : DateType
  
  IMPLICIT NONE
  SAVE

!> \name Miscellaneous constant parameters
!> \{
  INTEGER, PARAMETER :: max_classoutvar = 8     !<Number of class load output variables
  INTEGER, PARAMETER :: max_basinoutvar = 27    !<Number of basin load output variables
  INTEGER, PARAMETER :: max_noutvar = 10000     !<Maximum output variables to be calculated
  INTEGER, PARAMETER :: maxsoillayers = 3       !<Maximum number of soil layers
  INTEGER, PARAMETER :: max_pstype = 3          !<Maximum number of point sources types
  INTEGER, PARAMETER :: max_subid = 9999999     !<Maximum value of subid
  INTEGER, PARAMETER :: maxlakesections = 10    !<Maximum number of lake type 1 sections for connectivity (fill-and-spill) model
  REAL, PARAMETER    :: missing_value = -9999.  !<Value used for missing value
  REAL, PARAMETER    :: realzero = 1.E-37       !<Value of zero in real
  DOUBLE PRECISION, PARAMETER    :: doublezero = 1.D-307    !<Value of zero in double precision
  REAL, PARAMETER    :: pi = 3.1415927          !<Mathematical constant pi
  REAL, PARAMETER    :: solar = 0.0820          !<Solar constant, MJ/m2/min
  REAL, PARAMETER    :: cwater = 4.2            !<Heat capacity of water, kJ/kg/C
  REAL, PARAMETER    :: cice   = 2.1            !<Heat capacity of ice, kJ/kg/C
  REAL, PARAMETER    :: Lfreezing = 335.        !<Latent heat of freezing, kJ/kg
  REAL, PARAMETER    :: kice = 2.2              !<Thermal conductivity of ice, W/m/K (freshwater blackice)
  REAL, PARAMETER    :: icedensity = 917.       !<Density of ice, kg/m3
  REAL, PARAMETER    :: waterdensity = 1000.    !<Density of (unfrozen) water, kg/m3
!> \}
!> \name Model parameter dependence type parameters
!> These codes are used for identifying which model parameter-array to find a specific parameter in.
!> \{
  INTEGER, PARAMETER :: m_gpar   = 1        !<Code for general parameter type
  INTEGER, PARAMETER :: m_spar   = 2        !<Code for soil type dependent parameter type
  INTEGER, PARAMETER :: m_lpar   = 3        !<Code for land use dependent parameter type
  INTEGER, PARAMETER :: m_bpar   = 4        !<Code for basin dependent parameter type
  INTEGER, PARAMETER :: m_rpar   = 5        !<Code for parameter region dependent parameter type
  INTEGER, PARAMETER :: m_ldpar  = 6        !<Code for lake specific parameter imported from LakeData.txt
  INTEGER, PARAMETER :: m_mpar   = 7        !<Code for monthly dependent parameters
  INTEGER, PARAMETER :: nregiondivisions = 6  !Temporary here (should be read or calculated for set-up)
!> \}
!> \name Output variable type parameters; code for accumulation of data
!> These codes are used to define how a period value of a variable is calculated.
!> \{
  INTEGER, PARAMETER :: i_sum    = 0    !<Type of output variable, sum over period
  INTEGER, PARAMETER :: i_mean   = 1    !<Type of output variable, mean over period
  INTEGER, PARAMETER :: i_wmean  = 2    !<Type of output variable, weighted mean over period
  INTEGER, PARAMETER :: i_wmean2 = 3    !<Type of output variable, weighted mean over period by observed water variable
!> \}
!> \name Output variable type parameters; code for class type
!> These codes are used to define if class values can be had.
!> \{
  INTEGER, PARAMETER :: i_notdef = 0    !<Type of output variable, set once, not class dependent
  INTEGER, PARAMETER :: i_class  = 1    !<Type of output variable, calculated from class values
!> \}
!> \name Output variable type parameters; code for base area for upstream output calculation
!> These codes are used to define how to calculate upstream value.
!> \{
  !INTEGER, PARAMETER :: i_notdef  = 0    !<Type of output variable, undefined
  INTEGER, PARAMETER :: i_basin   = 1    !<Type of output variable, represent whole subbasin area
  INTEGER, PARAMETER :: i_land    = 2    !<Type of output variable, represent land area
  INTEGER, PARAMETER :: i_scale3  = 3    !<Type of output variable, scaling factor 10^3
  INTEGER, PARAMETER :: i_scale12 = 4    !<Type of output variable, scaling factor 10^12
  INTEGER, PARAMETER :: i_scale9  = 5    !<Type of output variable, scaling factor 10^9
  INTEGER, PARAMETER :: i_scale6  = 6    !<Type of output variable, scaling factor 10^6
!> \}
!> \name Global simulation time variables for the time resolution
!> These variables are used to retrieve the simulation time resolution set by the info file.
!> \{
  INTEGER :: simtimestep = 0            !<Simulation timestep
  CHARACTER(LEN=4) :: simtimeunit = '?' !<Simulation timeunit
!> \}

  !Type declarations
!> \brief Type for holding data about simulation configuration for states
  TYPE STATECONFIGURATIONTYPE
    LOGICAL :: simN           !<status of nitrogen simulation
    LOGICAL :: simP           !<status of phosphorus simulation
    LOGICAL :: simC           !<status of organic carbon simulation
    LOGICAL :: simS           !<status of sediment simulation
    LOGICAL :: simT1          !<status of tracer 1 simulation
    LOGICAL :: simT2          !<status of tracer 2 (water temperature) simulation
    LOGICAL :: arupdating     !<status of AR-updating (war or qar)
    LOGICAL :: floodplain     !<status of floodplain model
    LOGICAL :: glacier        !<status of glacier (soil)model
    LOGICAL :: growthdegreeday !<status of growing season start based on degree days model
    LOGICAL :: icelens        !<status of frozen soil model (icelens simulated)
    LOGICAL :: irrigation     !<Status of irrigation included in model set-up
    LOGICAL :: lakeriverice   !<status of lake and river ice model
    LOGICAL :: wetland        !<status of wetlands (river or water class), used for miscstates
    LOGICAL :: riverwetland   !<status of river wetlands
    LOGICAL :: watertransfer  !<Status of water transfer in model set-up
    LOGICAL :: qbank          !<Status of using bankful flow (N,P,S or T1)
    LOGICAL :: snowheat       !<Status of snow heat in model set-up
    LOGICAL :: traveltime     !<Status of simple soil model using travel time
    LOGICAL :: connectivity   !<Status of connectivity in model set-up
  END TYPE STATECONFIGURATIONTYPE
!> \brief Type for holding dimension data about simulation configuration for states
  TYPE STATEDIMENSIONTYPE
    INTEGER substance     !<Number of substances
    INTEGER slcclass      !<Number of slc-classes
    INTEGER aquifer       !<Number of aquifers in model domain
    INTEGER river         !<Number of rivers per subbasin (=2)
    INTEGER lake          !<Number of lakes per subbasin (=2)
    INTEGER soillayer     !<Number of soillayers (per class)
    INTEGER riverqueue    !<Maximum number of lag timesteps for river queue
    INTEGER timestep      !<Number of timesteps per day
    INTEGER lakesection   !<Maximum number of connectivity ilake model's lake sections or HGDM lakes per subbasin
  END TYPE STATEDIMENSIONTYPE
!> \brief Type for holding data about simulation configuration
  TYPE SIMULATIONCONFIGURATIONTYPE
    LOGICAL, ALLOCATABLE :: substance(:)  !<status of specific substance simulations (0:numsubstances)
    LOGICAL :: soildepthstretch     !<If true, use soilcorr parameter to stretch soil depths
    LOGICAL,ALLOCATABLE :: ilakecatpar(:)  !<If true, use parameter for internal lake location (icatch)
    LOGICAL,ALLOCATABLE :: olakedeppar(:)  !<If true, use parameter for outlet lake depth
  END TYPE SIMULATIONCONFIGURATIONTYPE
!> \brief Type for holding data about subbasin characteristics
  TYPE BASINTYPE
    INTEGER :: subid = 0       !<identification number for subbasin (maximum 7 digits)
    REAL    :: area = 0.       !<area (m2)
    REAL    :: elev = 0.       !<elevation above sea level (m)
    REAL    :: selev = 0.      !<spread of elevation (m)
    REAL    :: slope = 0.      !<slope (%)
    REAL    :: sslope = 0.     !<spread of slope (%)
    REAL    :: xcoord = 0.     !<x-coordinate
    REAL    :: ycoord = 0.     !<y-coordinate
    REAL    :: longitude = 0.  !<longitude, degrees E [-180,180]
    REAL    :: latitude = 0.   !<latitude, degrees N [-90,90]
    INTEGER :: region = HUGE(0)!<vegetation region used by CropData
    INTEGER :: parregion(nregiondivisions) = 1   !<parameter regions
    REAL    :: rivlen(2) = 0.  !<length of local (1) and main (2) river (m)
    REAL    :: ilakecatch = 0. !<part of catchment/runoff that runs to ilake
    REAL    :: iwetcatch = 0.  !<part of catchment/runoff that runs to iwet
    REAL    :: lakedepth(2) = 0.   !<lake water depth at threshold (m) (ilake and olake)
    REAL    :: closewater = 0. !<part of area (possibly agricultural area) close to water courses
    REAL    :: buffer = 0.     !<part of river length with buffer zones (of river in the area above (e.g. agricultural area))
    REAL    :: eroindex = 1.   !<erosion index, used for erosion model 1
    REAL    :: mrivc(5) = 0. !<coefficients for main river water level from flow by river rating curve 
    REAL    :: cloudiness(12) = missing_value !<monthly cloudiness climatology (fraction)
    INTEGER :: lakesection = 0. !<number of lake sections in subbasin
  END TYPE BASINTYPE
!> \brief Type for holding information about dams
  TYPE DAMTYPE
    REAL    :: qprod1 = 0.   !<regulated dam, discharge between wmin and w0 (m3/s) for period 1
    REAL    :: qprod2 = 0.   !                                                      for period 2
    REAL    :: qamp = 0.     !<regulated lake, amplitude of variation of average discharge between wmin and w0
    REAL    :: qpha = 102.   !<regulated lake, phase of variation of average discharge
    REAL    :: w0ref = 0.    !<threshold for dam, "dämningsgräns" (m in w-reference-system)
    REAL    :: wmin = 0      !<threshold for dam, "sänkningsgräns" (m) in local system ("dämningsgräns"=0)
    REAL    :: wampcoeff     !<adjustment factor for waterstage to "real" regulation amplitude
    REAL    :: limprod = missing_value  !<fractional level when production is starting to be limited (-)
    REAL    :: rate = 0.     !<rate parameter of outflow equation (m2/s)
    REAL    :: exp = 0.      !<exponent parameter of outflow equation
    REAL    :: regvol = 0.   !<Volume of dam used for regulation
    REAL    :: qinfmed = 0   !<Observed or Simulated inflow to dam (from natural run), mean of Year
    REAL    :: qinfmin = 0   !<Observed or Simulated inflow to dam (from natural run), minimum monthly mean
    REAL    :: qinfmax = 0   !<Observed or Simulated inflow to dam (from natural run), maximum monthly mean
    REAL    :: snowfrac = 0  !<Observed of Simulated fraction of precip that is snow
    INTEGER :: purpose = 0   !<Purpose of dam, 1=irrigation, 2=water supply, 3=flood control, 4=hydroelectricity, 0 = natural
    INTEGER :: datum1  = 0   !
    INTEGER :: datum2  = 0   !
  END TYPE DAMTYPE
!> \brief Type for holding information about lakes/lakeoutflow
  TYPE LAKETYPE
    REAL    :: area          !<(whole) lake area (m2)
    REAL    :: qprod1 = 0.   !<regulated lake, discharge between wmin and w0 (m3/s) for period 1
    REAL    :: qprod2 = 0.   !                                                      for period 2
    REAL    :: mqprod = 0.   !<maximum production flow (m3/s)
    INTEGER :: datum1 = 0    !<Start production regime period 1
    INTEGER :: datum2 = 0    !<Start production regime period 2
    REAL    :: qamp = 0.     !<regulated lake, amplitude of variation of average discharge between wmin and w0
    REAL    :: qpha = 102.   !<regulated lake, phase of variation of average discharge
    REAL    :: rate = 0.     !<rate parameter of outflow equation (m2/s)
    REAL    :: exp = 0.      !<exponent parameter of outflow equation
    REAL    :: w0ref = 0.    !<threshold for lake or "dämningsgräns" for regulated lake (m in w-reference-system)
    REAL    :: deltaw0 = 0.  !<difference in threshold for lake rating curve production period 2, waterstage parameter of outflow equation (m)
    REAL    :: wmin          !<threshold for lake or "sänkningsgräns" for regulated lake, waterstage parameter of produktion flow (m in local system ("dämningsgräns"=0)
    REAL    :: wampcoeff     !<adjustment factor for waterstage to "real" regulation amplitude
    REAL    :: limprod = missing_value  !<fractional level when production is starting to be limited (-)
    LOGICAL :: minflow = .FALSE. !<flag for minimum flow (-)
    LOGICAL :: obsflow = .FALSE. !<flag for using wanted flow time series for bifurcation (-)
  END TYPE LAKETYPE
!> \brief Type for holding information about lakes/lakeoutflow
  TYPE OUTLETTYPE
    INTEGER :: isb           !index of subbasin where the lakebasin with this outlet is located
    REAL    :: mqprod = 0.   !<maximum production flow (m3/s)
    REAL    :: qprod(2) = 0. !<regulated lake, discharge between wmin and w0 (m3/s) for period 1 and 2
    INTEGER :: qdate(2) = 0  !<Start production regime period 1 and 2
    REAL    :: qamp = 0.     !<regulated lake, amplitude of variation of average discharge between wmin and w0
    REAL    :: qpha = 102.   !<regulated lake, phase of variation of average discharge
    REAL    :: rate = 0.     !<rate parameter of outflow equation (m2/s)
    REAL    :: exp = 0.      !<exponent parameter of outflow equation
    REAL    :: deltaw0 = 0.  !<difference in threshold for lake rating curve production period 2, waterstage parameter of outflow equation (m)
    !REAL    :: deltaw0_2 = 0.  !<difference in threshold compared to outlet 1, general value (m)
    REAL    :: limprod = missing_value  !<fractional level when production is starting to be limited (-)
  END TYPE OUTLETTYPE
!> \brief Type for holding information about lakes/lakeoutflow
  TYPE NEWLAKETYPE
    REAL    :: area          !<whole lake area (m2)
    INTEGER :: noutlet = 0   !<Number of defined outlets
    TYPE(OUTLETTYPE),ALLOCATABLE :: outlet(:)
    REAL    :: w0ref = 0.    !<threshold for lake or "dämningsgräns" for regulated lake (m in w-reference-system)
    REAL    :: wmin          !<threshold for lake or "sänkningsgräns" for regulated lake, waterstage parameter of produktion flow (m in local system ("dämningsgräns"=0)
    REAL    :: wampcoeff     !<adjustment factor for waterstage to "real" regulation amplitude
  END TYPE NEWLAKETYPE
!> \brief Type for holding information about lakebasins
  TYPE LAKEBASINTYPE
    INTEGER :: ilk = 0       !<index for lake in lake-variable in which this lake basin is a part
    LOGICAL :: last = .FALSE.!<flag for lakebasin with outlet of (whole) lake
  END TYPE LAKEBASINTYPE
!> \brief Type for holding information about lake type 1 sections in a subbasin   !TODO: do this need to be type? only reals
  TYPE LAKESECTIONTYPE
    REAL  :: farea = 0.   !<fraction of lake type 1 area in section
    REAL  :: ficatch = 0. !<fraction of lake type 1 upstream area (icatch) draining to section 
    REAL  :: depth = 0.   !<outlet threshold depth of section, relative to lakedepth (m)
  END TYPE LAKESECTIONTYPE
!> \brief Type for holding data about floodplain characteristics
  TYPE FLOODTYPE
    REAL    :: fpfmr           !<floodplain fraction of main river
    REAL    :: fpfol           !<floodplain fraction of olake
    REAL    :: floll           !<flooding level for olake olake
    REAL    :: flolp           !<flooding level for olake floodplain
    REAL    :: flmrr           !<flooding level for main river river
    REAL    :: flmrp           !<flooding level for main river flood plain
    REAL    :: rcl2fp          !<lake2floodplain recession coefficient
    REAL    :: rcr2fp          !<river2floodplain recession coefficient
    REAL    :: rcfp2l          !<floodplain2lake recession coefficient
    REAL    :: rcfp2r          !<floodplain2river recession coefficient
    REAL    :: fymol           !<water level at maximum areal extent, olake floodplain
    REAL    :: fymmr           !<water level at maximum areal extent, main river floodplain
    REAL    :: hrefl           !<reference level for deepest bottom of olake floodplain (m)
    REAL    :: hrefr           !<reference level for deepest bottom of main river floodplain (m)
  END TYPE FLOODTYPE
!> \brief Type for holding information about classes in a subbasin
  TYPE CLASSBASINTYPE
    REAL    :: part         !<part of area for landuse-soil combinations
    REAL    :: part2cr = 0. !<part of class-area that has also a secondary crop
    REAL    :: deltah  = 0. !<deviation of class from mean elevation (m)
    REAL    :: slope   = 0. !<slope for class
    REAL    :: aspect  = 0. !<aspect for class
    REAL    :: wsf(8)  = 0. !<Winstral coefficient
  END TYPE CLASSBASINTYPE
!> \brief Type for holding information about glacier class
  TYPE GLACIERTYPE
    INTEGER :: gtype         !<glacier type (0=default/mountain glacier, 1=icecap, 2=ice sheet, 3=infinit glacier)
    REAL    :: volcorr       !<correction of volume-area relationship (glacvcoef) due to combining of several glacier into one class
    REAL    :: yeardiff      !<year difference between date of slc area information and simulation begin date
    REAL    :: glacinimb     !<annual mass balance for correction of initial volume (mm/year)
    REAL    :: inivol        !<initial volume of glacier (m3) for the date of slc area information / simulation begin date
  END TYPE GLACIERTYPE
!> \brief Type for holding information about aquifers
  TYPE AQUIFERTYPE
    INTEGER :: parregion(nregiondivisions)     !<parameter region, only one in aquifer though
    REAL    :: area          !<aquifer horizontal area (m2)
    REAL    :: porosity      !<(average) porosity of aquifer (-)
    REAL    :: maxvol        !<maximum volume of aquifer (m3) 
    REAL    :: basedepth     !<base depth (returnflow threshold) of aquifer (m)
    REAL    :: passivedep    !<passive depth of aquifer (m)
    REAL    :: reference     !<level of reference to calculate aquifer water level (m)
    REAL    :: inivol        !<initial volume of aquifer (m3)
    REAL    :: passivevol    !<passive volume of aquifer (m3) (part of inivol)
    REAL    :: temperature = 0.  !<temperature of aquifer water (degree Celsius)
    REAL    :: conc_IN = 0.  !<initial IN concentration of aquifer (mg/L)
    REAL    :: conc_SP = 0.  !<initial SP concentration of aquifer (mg/L)
    REAL    :: percdelay     !<delay of deep percolation (days)
    REAL    :: retrate       !<rate parameter of return flow 
  END TYPE AQUIFERTYPE
!> \brief Type for holding information about flow path between subbasins
  TYPE PATHTYPE
    INTEGER :: main = 0      !<receiving subbasin main flow
    INTEGER :: grw1 = 0      !<receiving subbasin regional groundwater flow, TODO this can be combined with aquid (recieveid)
    REAL    :: grwtolake = 0 !<part of groundwater flow that goes to the subbasins olake, TODO THIS can be combined with recievefraction
    LOGICAL :: uplakebasin   !<subbasin and reciever of mainflow are lakebasins
    INTEGER :: aquid            !<id of aquifer this subbasin belongs to, 1..naquifers
    LOGICAL :: rechargebasin    !<delivering subbasin
    REAL    :: recievefraction  !<fraction of aquifer outflow to this subbasin
  END TYPE PATHTYPE
!> \brief Type for holding information about branching flow
  TYPE BRANCHTYPE
    INTEGER :: source        !<subbasin with branch flow
    INTEGER :: branch        !<subbasin receiving branch flow
    REAL    :: mainpart      !<main part of outflow
    REAL    :: maxQ          !<maximum flow in main part
    REAL    :: minQ          !<minimum flow in main part
    REAL    :: maxQbranch    !<maximum flow in branch
    LOGICAL :: recQbranch    !<demanded flow in branch (as timeserie dwtr from Xobs)
    LOGICAL :: lb2outlet     !<flow in branch determined by second lake outlet (LakeData)
    LOGICAL :: uplakebasin   !<source and reciever are lakebasins
  END TYPE BRANCHTYPE
!> \brief Type for holding applied nutrient load data
  TYPE NPCLOADTYPE  
    REAL    :: volloc   = 0.  !<Water discharge from local diffuse source (m3/day)
    REAL    :: locsoil   = 0.  !<Fraction of water discharge from local diffuse source to soil (the rest to river)
    REAL    :: abstrvol = 0.   !<Abstraction volume (m3/s)
    INTEGER :: abstrind = 0    !<Index for abstraction source; 1=main river,2=olake,3=main river inflow
    REAL,ALLOCATABLE :: locconc(:)      !<concentration of local diffuse source (mg/l for NPC)
    REAL,ALLOCATABLE :: psvol(:)     !<Water discharge from point sources (max_pstype) (m3/s)
    REAL,ALLOCATABLE :: psload(:,:)  !<Nutrient load from point sources (max_pstype,numsubstances) (kg/timestep)
  END TYPE NPCLOADTYPE
!> \brief Type for holding deposition data
  TYPE DEPOSITIONTYPE
    REAL,ALLOCATABLE :: inwetconc(:)  !<IN concentration of wet deposition (mg/l) [nsub]
    REAL,ALLOCATABLE :: indryload(:,:) !<IN load of dry deposition on land use group 1-3(vegtype in GeoClass) (kg/km2/timestep) [nsub,3]
    REAL,ALLOCATABLE :: inloadwater(:,:)  !<Load of monthly total IN deposition on water (kg/km2/ts) [nsub,12]
  END TYPE DEPOSITIONTYPE
  !> \brief Type for holding general substance load to surface water
  TYPE T1LOADTYPE  
    INTEGER :: subindex   !<Subbasin index (i, not subid)
    INTEGER :: sw_code    !<Recieving surface water code; 1=local river,2=ilake,3=main river,4=olake
    REAL    :: psvol      !<Water discharge from point source (m3/s)
    REAL    :: psconc     !<T1 load from point source (conc)
    REAL    :: pstemp     !<T2-water temperature of point source (degree C)
  END TYPE T1LOADTYPE
  !> \brief Type for characteristics of point sources and abstractions from PointSourceData.txt (time series)
  TYPE POINTSOURCEDATATYPE  
    INTEGER :: subindex   !<Subbasin index (i, not subid)
    INTEGER :: subid      !<Subbasin id
    INTEGER :: psid       !<Id for point source data
    INTEGER :: pstype     !<Type of point source (industry,..), 1-max_pstype (only used for point sources)
    INTEGER :: sw_code=0  !<Recieving surface water code; 1=local river,2=ilake,3=main river,4=olake OR
                          !Code for abstraction source; 1=main river,2=olake,3=main river inflow
    INTEGER :: flowcol    !<File column with ps_vol
    INTEGER,ALLOCATABLE :: conccol(:)     !<File column with ps_conc for all substances
  END TYPE POINTSOURCEDATATYPE
  !> \brief Type for current point source data
  TYPE POINTSOURCETYPE  
    REAL :: flow    !<Water discharge from point sources or water abstracted (m3/s)
    REAL,ALLOCATABLE :: load(:)  !<Nutrient load from point sources (numsubstances) (m3/s*mg/L=g/s)
  END TYPE POINTSOURCETYPE
!> \brief Type for holding information about irrigation
  TYPE IRRIGATIONTYPE
    INTEGER :: subid                  !<identification number for subbasin
    INTEGER :: regsourceid = 0        !<identification number for regional source for irrigation of subbasin
    INTEGER :: demandtype             !<type of irrigation water demand equation
    LOGICAL :: dam         = .FALSE.  !<status of irrigation dam?
    REAL    :: local_eff   = 1.       !<efficiency of irrigation in the subbasin (-)
    REAL    :: reg_eff     = 1.       !<efficiency of irrigation in the regional irrigation net (-)
    REAL    :: gw_part                !<part of irrigation from (local) groundwater (-)
    REAL    :: sw_part                !<part of irrigation from (local) surface water (-)
  END TYPE IRRIGATIONTYPE
!> \brief Type for holding information about water transfer
  TYPE WATERTRANSFERTYPE
    INTEGER :: srcsubid        !<identification number for source subbasin
    INTEGER :: rcvindex        !<index for receiving subbasin
    REAL    :: flow            !<constant transfer flow (m3/s), or, if negative, flag for using dwtr from Xobs
  END TYPE WATERTRANSFERTYPE
!> \brief Type for holding information about classes
  TYPE CLASSTYPE
    INTEGER :: luse          !<landuse code
    INTEGER :: soil          !<soil type code
    INTEGER :: crop          !<main crop id number
    INTEGER :: crop2         !<secondary crop id number
    INTEGER :: vegtype       !<Vegetation type (groups land uses into 1=open, 2=forest, 3=water)
    INTEGER :: rotation      !<Crop rotation group of classes
    REAL ::    tiledepth     !<Depth of tile drainage system
    REAL ::    streamdepth   !<Depth to ditch or local stream
    REAL ::    traveltime    !<Time of travel to local stream
    REAL ::    soildepth(maxsoillayers) !<Lower border of soil layers (m)
  END TYPE CLASSTYPE
!> \brief Type for holding information about classgroups
  TYPE CLASSGROUPTYPE
    CHARACTER(LEN=6) :: name  !<name of classgroup
    INTEGER :: ncgslcs        !<number of classes in classgroup
    INTEGER,ALLOCATABLE :: slcs(:)  !<classes in group
  END TYPE CLASSGROUPTYPE
!> \brief Type for holding information about vegetation; nutrient uptake, fertilization etc.
  TYPE CROPDATATYPE
    REAL    :: fertnamount1  !<fertiliser amount N (kg/km2)
    REAL    :: fertpamount1  !<fertiliser amount P (kg/km2)
    INTEGER :: fertday1      !<day for fertilizing 1 (dayno)
    REAL    :: fertdown1     !<part of fertilizer amount ploughed down to soillayer 2
    REAL    :: mannamount1   !<manure amount N (kg/km2)
    REAL    :: manpamount1   !<manure amount P (kg/km2)
    INTEGER :: manday1       !<day for manureing 1 (dayno)
    REAL    :: mandown1      !<part of manure amount ploughed down to soillayer 2
    REAL    :: fertnamount2  !<fertiliser amount N (kg/km2)
    REAL    :: fertpamount2  !<fertiliser amount P (kg/km2)
    INTEGER :: fertday2      !<day for fertilizing 2 (dayno)
    REAL    :: fertdown2     !<part of fertilizer amount ploughed down to soillayer 2
    REAL    :: mannamount2   !<manure amount N (kg/km2)
    REAL    :: manpamount2   !<manure amount P (kg/km2)
    INTEGER :: manday2       !<day for manureing 2 (dayno)
    REAL    :: mandown2      !<part of manure amount ploughed down to soillayer 2
    REAL    :: resnamount    !<plant residual amount of N (kg/km2/yr)
    REAL    :: respamount    !<plant residual amount of P (kg/km2/yr)
    REAL    :: rescamount    !<plant resudual (litterfall) of C (kg/km2/yr)
    INTEGER :: resdayno      !<day for residual (dayno)
    REAL    :: resdown       !<part of residual amount ploughed down to soillayer 2
    REAL    :: resfast       !<part of residual amount to fast pool (rest to humus)
    REAL    :: uptake1       !<parameter 1 for plant uptake of nutrient !TODO: these could be an array
    REAL    :: uptake2       !<parameter 2 for plant uptake of nutrient
    REAL    :: uptake3       !<parameter 3 for plant uptake of nutrient
    REAL    :: uptakeupper   !<fraction of plant uptake in upper soil layer
    REAL    :: PNuptakeRatio !<phosphorus plant uptake as a factor of nitrogen uptake
    INTEGER :: baredayno1    !<day for beginning of first period with bare soil (dayno), typically 1
    INTEGER :: baredayno2    !<day for end of first period with bare soil (dayno), typically sawing date + a few days
    INTEGER :: baredayno3    !<day for beginning of second period with bare soil (dayno), typically ploughing date
    INTEGER :: baredayno4    !<day for end of second period with bare soil (dayno), typically 365
    INTEGER :: baredayno5    !<day for end of second period with bare soil (dayno), typically saw date of autumn crop
    REAL    :: ccmax1        !<crop cover during summer
    REAL    :: ccmax2        !<crop cover during winter and all year for year-round-crops (e.g. forest)
    REAL    :: gcmax1        !<ground cover during summer
    REAL    :: gcmax2        !<ground cover during winter and all year for year-round-crops (e.g. forest)
    REAL    :: gddsow        !<accumulated growing degree days needed for sowing
    REAL    :: daylength     !<minimum daylength required for start to accumulate gdd (hours)
    REAL    :: basetemp      !<temperature which is deducted from air temperature when calculating gdd (degrees)
    INTEGER :: firstday      !<first possible day for gdd accumulation (dayno)
    REAL    :: t1amount      !<amount T1 (kg/km2)       
    INTEGER :: t1year        !<year of T1 application      
    INTEGER :: t1day         !<first day of T1 application
    INTEGER :: t1numberofdays !<number of days with T1 application       
    INTEGER :: t1daydown     !<day of T1 incorporation 
    REAL    :: t1down1       !<part of T1 ploughed down to soillayer 1
    REAL    :: t1down2       !<part of T1 ploughed down to soillayer 2
  END TYPE CROPDATATYPE
!> \brief Type for holding information irrigation for vegetation
  TYPE CROPIRRDATATYPE
    INTEGER :: season_end         !<end of irrigation season (may be larger than 365)
    INTEGER :: plantingdayno      !<day of planting (dayno)/irrigation season start
    INTEGER :: lengthini          !<length of initial crop growth state (days)
    INTEGER :: lengthdev          !<length of development crop growth state (days)
    INTEGER :: lengthmid          !<length of middle crop growth state (days)
    INTEGER :: lengthlate         !<length of late crop growth state (days)
    INTEGER :: imm_start = 0      !<day for start of immersion season (dayno)
    INTEGER :: imm_end            !<day for end of immersion season (dayno)
    REAL    :: kcbini             !<parameter for plant transpiration at initial growth state
    REAL    :: kcbmid             !<parameter for plant transpiration at middle growth state
    REAL    :: kcbend             !<parameter for plant transpiration at end of late growth state
    REAL    :: dlref              !<reference depletion level (-)
  END TYPE CROPIRRDATATYPE
!> \brief Type for holding information about river wetlands
  TYPE WETLANDTYPE
    REAL    :: area              !<river wetland surface area (m2)
    REAL    :: depth             !<river wetland depth (m)
    REAL    :: part              !<river wetland part of riverflow as inflow (-)
  END TYPE WETLANDTYPE
!> \brief Type for holding information about soil leakage "forcing" data
  TYPE SOILLEAKAGEDATATYPE
    REAL,ALLOCATABLE :: concentration(:,:,:)    !<matrix for keeping data in memory (numsubstance,month,nsub)
    REAL,ALLOCATABLE :: load(:,:,:,:)    !<matrix for keeping data in memory (numsubstance,nclass,month,nsub)
  END TYPE SOILLEAKAGEDATATYPE
!> \brief Type for holding information about output variables
  TYPE OUTVARIDTYPE
    CHARACTER(LEN=4)  :: shortname      !<short name of output variable
    CHARACTER(LEN=20) :: longname       !<long name of output variable
    INTEGER           :: vartype        !<accumulation type of output variable; sum,mean or weighted mean*2
    INTEGER           :: water          !<corresponding water to concentration (outvar index)
    INTEGER           :: basearea       !<base area for upstream output (0=undefined,1=basinarea,2=landarea,3-5=no area (scaling 1000))
    INTEGER           :: classtype      !<type of class variable (0=non-class variable,1=class variable (could separate land class, all class etc if needed)
    CHARACTER(LEN=6)  :: shortunit      !<short unit of output variable
    CHARACTER(LEN=20) :: longunit       !<unit of output varible
    CHARACTER(LEN=6)  :: upunit         !<short unit of upstream output variable
  END TYPE OUTVARIDTYPE
  !> \brief Type for changing output variables for criteria calculations
  TYPE CHANGEOUTVARTYPE
    INTEGER           :: rec(2)         !<outvarid-index for recorded variable that is changed, and to which
    INTEGER           :: comp(2)        !<outvarid-index for recorded variable that is changed, and to which
  END TYPE CHANGEOUTVARTYPE
  !> \brief Type for holding definition of model parameters
  TYPE MODPARIDTYPE
    CHARACTER(LEN=10) :: shortname       !<Name of parameter
    INTEGER           :: deptype         !<Type of dependence for the parameter
    INTEGER           :: parno           !<Parameter number
  END TYPE MODPARIDTYPE

!> \name Variables for configuration of simulation
!> \brief HYSS make available variables to hold information on the configuration of the simulation.
!> \{
  !Substances available for modelling, index of variables in concentration arrays
  INTEGER :: i_in     !<index of variable, inorganic nitrogen
  INTEGER :: i_on     !<index of variable, organic nitrogen
  INTEGER :: i_sp     !<index of variable, soluble (reactive) phosphorus, i.e. phosphate
  INTEGER :: i_pp     !<index of variable, particulate phosphorus
  INTEGER :: i_t1     !<index of variable, conservative substance 1
  INTEGER :: i_t2     !<index of variable, tracer 2, water temperature
  INTEGER :: i_oc     !<index of variable, (dissolved) organic carbon
  INTEGER :: i_ss     !<index of variable, suspended sediments
  INTEGER :: i_ae     !<index of variable, (suspended) algae (nitrogen)
  INTEGER,PARAMETER :: max_substances = 9     !<maximum number of substances that can be simulated

  !Model simulation status variables
  LOGICAL :: simulatesubstances        !<status of substance simulations (numsubstances>0)
  TYPE(SIMULATIONCONFIGURATIONTYPE) :: simulate  !<configuration of simulation
  TYPE(STATECONFIGURATIONTYPE) :: conduct  !<configuration of states for simulation
  LOGICAL :: irrunlimited         !<If true, simulate unlimited irrigation, else withdraw from actual sources
  LOGICAL :: soiliniwet           !<If true, initiate soil water to porosity, else filed capacity
  LOGICAL :: conductxoms          !<Status if XobsXOMN or XobsXOSN-files should be read
  LOGICAL :: conductwb            !<Status if water balance output should be calculated and written
  LOGICAL :: conductregest        !<Flag for activation of regional parameter estimation
  LOGICAL :: conductbasinpetmodel !<Flag for using different petmodel for each basin
  LOGICAL :: conductbasinlocsoil  !<Flag for using different rural flow to soil (locsoil) for each basin
  LOGICAL :: conductwarning       !<Status for warning output to hyss.log

  !Optional Model Structure Settings (these should be HYPEVAR, together with modeloptionname for info)
  !Processes with model options in HYPE. 0 is always the default.
  INTEGER, PARAMETER :: p_snowfall       = 1  !Snowfall process options, (0) threshold temperatures, (1) snowfall fraction input (SFobs.txt) 
  INTEGER, PARAMETER :: p_snowmelt       = 2  !Snowmelt process options, (0) temperature index, (1) temperature index with snowcover scaling, (2) temperature+radiation index
  INTEGER, PARAMETER :: p_lakeriverice   = 3  !Lake river ice model options, (0) off, (1) version1, (2) version2
  INTEGER, PARAMETER :: p_petmodel       = 4  !PET process options, (0) current HYPE, (1) current HYPE without replacement for observations, (2) (modified) Jensen, (3) (modified) Hargreaves-Samani, (4) Priestly-Taylor, (5) FAO PenmanMonteith
  INTEGER, PARAMETER :: p_deepgroundwater= 5  !Regional groundwater options, (0) none, (1) regional groundwater flow delivered instantly, (2)aquifer modelled explicitly
  INTEGER, PARAMETER :: p_swtemperature  = 6  !Surface water temperature model, (0) old (MA-temp), (1) use T2 for lake and river processes
  INTEGER, PARAMETER :: p_snowevap       = 7  !Snowevaporation model, (0) off, (1) epotsnow = epot * fepotsnow (landuse dependent)
  INTEGER, PARAMETER :: p_growthstart    = 8  !Growth season starting model, (0) use bd2 from CropData, (1) use degree day calculation
  INTEGER, PARAMETER :: p_floodplain     = 9  !Floodplain model, (0) not used, (1=default) simple (P-E for floodplain) (2) soilmodel for non-flooded floodplain
  INTEGER, PARAMETER :: p_snowdensity    = 10 !Snow density model, (0) depending on age of snow, snowdens0 and snowdensdt, (1) depending on compactation factor
  INTEGER, PARAMETER :: p_glacierini     = 11 !Glacier initialization as usual via SLC+parameters or stated save (0), initialization from SLC+parameters overrides state_save (1) 
  INTEGER, PARAMETER :: p_infiltration   = 12 !Infiltration scheme (0) current HYPE, (1) restricted infiltration into frozen soils, (2) old scheme (runoff and evaporation before infiltration), (3) (2)+(1)
  INTEGER, PARAMETER :: p_erosion        = 13 !Soil erosion model (0) MMF-based model, (1) HBV-Sed based model
  INTEGER, PARAMETER :: p_wetland        = 14 !Wetland models (0) no wetland model, (1) river wetlands, (2) landclass wetlands
  INTEGER, PARAMETER :: p_soilleakage    = 15 !Soil leakage concentration model (0) calculate soil concentrations and leakage, (1) read typical monthly soil leakage (concentration) from file, (2) read typical monthly soil leakage (load) from file, (3) read monthly soil leakage (load) from file
  INTEGER, PARAMETER :: p_snowfalldist   = 16 !Snowfall distribution model, (0) off, (1) linear of (2) log-linear dependance on windshelter index
  INTEGER, PARAMETER :: p_frozensoil     = 17 !Options for frozen/unfrozen soil water content (0) no frozen water in soil (current HYPE), (1) unfrozen water as function of temperature, (1) unfrozen water as function of three temperatures
  INTEGER, PARAMETER :: p_snowheat       = 18 !Options for snow heat content, (0) off (1) on (snow melt is delayed until heat content correspond to snow temperature = 0°C) 
  INTEGER, PARAMETER :: p_surfacerunoff  = 19 !Options for surface runoff and infiltration, (0) surface runoff based on water above saturation (1) surface runoff based on soil moisture (continous formulation)  (2) surface runoff based on soil moisture and ginfilt (continous formulation) (3) surface runoff based on soil moisture and ginfilt (discrete formulation)
  INTEGER, PARAMETER :: p_connectivity   = 20 !Hydrological connectivity within subbasins, (0) no connectivity model, (1) ilake fill-and-spill model, (2) HGDM model, (3) fill-and-spill and HGDM models 
  INTEGER, PARAMETER :: num_modelprocess = 20 !Number of processes with options in HYPE
  CHARACTER(LEN=16),ALLOCATABLE :: modeloptionname(:)
  INTEGER,ALLOCATABLE :: modeloption(:)
  
  INTEGER :: maxmodelsinoption(num_modelprocess)
  PARAMETER(maxmodelsinoption = [2,3,3,6,3,2,2,2,3,2,2,4,2,3,4,3,3,2,5,4])

  CHARACTER(LEN=64) :: petmodelnames(0:maxmodelsinoption(p_petmodel)-1)  !<PETmodel names
  PARAMETER(petmodelnames = [CHARACTER(LEN=64) :: 'current HYPE', 'current HYPE without replacement for observations', '(modified) Jensen', '(modified) Hargreaves-Samani', 'Priestly-Taylor', 'FAO PenmanMonteith'])
  CHARACTER(LEN=64) :: snowfallmodelnames(0:maxmodelsinoption(p_snowfall)-1)  !<Snowfallmodel names
  PARAMETER(snowfallmodelnames = [CHARACTER(LEN=64) :: 'threshold temperatures', 'snowfall fraction input (SFobs.txt)'])
  CHARACTER(LEN=64) :: snowfalldistmodelnames(0:maxmodelsinoption(p_snowfalldist)-1)  !<Snowfall distribution model names
  PARAMETER(snowfalldistmodelnames = [CHARACTER(LEN=64) :: 'off', 'linear WSF-function','log-linear WSF function'])
  CHARACTER(LEN=64) :: snowmeltmodelnames(0:maxmodelsinoption(p_snowmelt)-1)  !<Snowmeltmodel names
  PARAMETER(snowmeltmodelnames = [CHARACTER(LEN=64) :: 'temperature index', 'temperature index with snowcover scaling', 'temperature+radiation index'])
  CHARACTER(LEN=64) :: lakerivericemodelnames(0:maxmodelsinoption(p_lakeriverice)-1)  !<Lakerivericemodel names
  PARAMETER(lakerivericemodelnames = [CHARACTER(LEN=64) :: 'off', 'version1', 'version2'])
  CHARACTER(LEN=64) :: deepgroundwatermodelnames(0:maxmodelsinoption(p_deepgroundwater)-1)  !<Deepgroundwater names
  PARAMETER(deepgroundwatermodelnames = [CHARACTER(LEN=64) :: 'none', 'regional groundwater flow delivered instantly', 'aquifer modelled explicitly'])
  CHARACTER(LEN=64) :: swtemperaturemodelnames(0:maxmodelsinoption(p_swtemperature)-1)  !<Swtemperature names
  PARAMETER(swtemperaturemodelnames = [CHARACTER(LEN=64) :: 'old (MA-temp)', 'use T2 for lake and river processes'])
  CHARACTER(LEN=64) :: snowevapmodelnames(0:maxmodelsinoption(p_snowevap)-1)  !<Snowevap names
  PARAMETER(snowevapmodelnames = [CHARACTER(LEN=64) :: 'off', 'epotsnow = epot * fepotsnow (landuse dependent)'])
  CHARACTER(LEN=64) :: growthstartmodelnames(0:maxmodelsinoption(p_growthstart)-1)  !<Growthstart names
  PARAMETER(growthstartmodelnames = [CHARACTER(LEN=64) :: 'use bd2 from CropData', 'use degree day calculation'])
  CHARACTER(LEN=64) :: floodplainmodelnames(0:maxmodelsinoption(p_floodplain)-1)  !<Floodplain names
  PARAMETER(floodplainmodelnames = [CHARACTER(LEN=64) :: 'not used', 'simple (P-E for floodplain)', 'soilmodel for non-flooded floodplain'])
  CHARACTER(LEN=64) :: snowdensitymodelnames(0:maxmodelsinoption(p_snowdensity)-1)  !<Snowdensity names
  PARAMETER(snowdensitymodelnames = [CHARACTER(LEN=64) :: 'depending on age of snow, snowdens0 and snowdensdt', 'depending on compactation factor'])
  CHARACTER(LEN=64) :: glacierinimodelnames(0:maxmodelsinoption(p_glacierini)-1)  !<Glacierini names
  PARAMETER(glacierinimodelnames = [CHARACTER(LEN=64) :: 'Glacier init as usual via SLC+parameters or stated save', 'init from SLC+parameters overrides state_save'])
  CHARACTER(LEN=64) :: infiltrationmodelnames(0:maxmodelsinoption(p_infiltration)-1)  !<Infiltration names
  PARAMETER(infiltrationmodelnames = [CHARACTER(LEN=64) :: 'infiltration before runoff', 'restricted infiltration into frozen soils (infilt before runoff)','old HYPE (infiltration after runoff)', 'restricted infiltration into frozen soils (infilt after runoff)'])
  CHARACTER(LEN=64) :: erosionmodelnames(0:maxmodelsinoption(p_erosion)-1)   !<Erosion names
  PARAMETER(erosionmodelnames = [CHARACTER(LEN=64) :: 'MMF-based model', 'HBV-Sed based model'])
  CHARACTER(LEN=64) :: wetlandmodelnames(0:maxmodelsinoption(p_wetland)-1)   !<Wetland names
  PARAMETER(wetlandmodelnames = [CHARACTER(LEN=64) :: 'no wetland model', 'river wetlands', 'waterclass wetlands'])
  CHARACTER(LEN=64) :: soilleakmodelnames(0:maxmodelsinoption(p_soilleakage)-1)   !<Soil leakage model names
  PARAMETER(soilleakmodelnames = [CHARACTER(LEN=64) :: 'not use soilleak model', 'soil leak concentration', 'typical monthly soil leak load','monthly time-series soil leak load'])
  CHARACTER(LEN=64) :: frozensoilmodelnames(0:maxmodelsinoption(p_frozensoil)-1)   !<frozensoil names
  PARAMETER(frozensoilmodelnames = [CHARACTER(LEN=64) :: 'no frozen water', 'unfrozen water as function of temperature','unfrozen water as function of three temperatures'])
  CHARACTER(LEN=64) :: snowheatmodelnames(0:maxmodelsinoption(p_snowheat)-1)   !<snowheat names
  PARAMETER(snowheatmodelnames = [CHARACTER(LEN=64) :: 'off', 'on (snowheat prevent snowmelt as long as snowtemp is below 0°C)'])
  CHARACTER(LEN=64) :: surfacerunoffmodelnames(0:maxmodelsinoption(p_surfacerunoff)-1)   !<surfacerunoff names
  PARAMETER(surfacerunoffmodelnames = [CHARACTER(LEN=64) :: 'based on soil water treshold', 'based on soil moisture (cont)', 'based on soil moisture and rain (cont)', 'based on soil moisture (disc)', 'based on soil moisture and rain (disc)'])
  CHARACTER(LEN=64) :: connectivitymodelnames(0:maxmodelsinoption(p_connectivity)-1)   !<Connectivity names
  PARAMETER(connectivitymodelnames = [CHARACTER(LEN=64) :: 'no connectivity model', 'fill-and-spill in ilake sections', 'HGDM ilake model', 'HGDM or fill-and-spill models'])
!> \}

!> \name Variables for input data
!> \brief HYSS make available variables to hold input data for simulation, 
!> both static geographical data and time series data for forcing the model 
!> and for evaluation. Variables ending with suffix i are dependent on subbasin.
!> \{
  !Observed variable declarations
  REAL,ALLOCATABLE :: tempi(:)             !<Temperature data for current time step
  REAL,ALLOCATABLE :: preci(:)             !<Precipitaion data for current time step
  REAL,ALLOCATABLE :: qobsi(:)             !<Runoff data for current time step
  REAL,ALLOCATABLE :: xobsi(:)             !<Other observations for current time step
  REAL,ALLOCATABLE :: xoregobsi(:)         !<Other outregion observations for current time step
  INTEGER, ALLOCATABLE :: xobsindex(:,:)   !<Index for finding variables in xobsi
  INTEGER, ALLOCATABLE :: xoregindex(:,:)  !<Index for finding variables in xoregobsi
  REAL,ALLOCATABLE :: snowfraci(:)         !<Precipitation snowfall fraction [-] for current time step
  REAL,ALLOCATABLE :: shortwavei(:)        !<Shortwave (downwards) radiation [MJ/m2/day] for current time step
  REAL,ALLOCATABLE :: windi(:)             !<Winds speed [m/s] for current time step
  REAL,ALLOCATABLE :: uwindi(:)            !<u-component winds speed [m/s] for current time step (west to east)
  REAL,ALLOCATABLE :: vwindi(:)            !<v-component winds speed [m/s] for current time step (south to north)
  REAL,ALLOCATABLE :: humidi(:)            !<Relative humidity [0-1] for current time step
  REAL,ALLOCATABLE :: tmini(:)             !<Daily minimum air temperature for current time step
  REAL,ALLOCATABLE :: tmaxi(:)             !<Daily maximum air temperature for current time step

  !Other (static) indata
  TYPE(BASINTYPE),ALLOCATABLE :: basin(:)         !<Basin characteristics
  REAL, ALLOCATABLE :: upstreamarea(:)            !<area of upstream subbasins
  REAL, ALLOCATABLE :: landarea(:)                !<land area [m2] (including floodplains, excluding wetlands)
  REAL, ALLOCATABLE :: tobselevation(:)           !<Forcing data informations, elevation of Tobs
  TYPE(DAMTYPE),ALLOCATABLE :: dam(:)             !<Dam characteristics 
  TYPE(LAKETYPE),ALLOCATABLE :: lake(:)           !<Lake characteristics
  TYPE(LAKEBASINTYPE),ALLOCATABLE :: lakebasin(:) !<Lakebasin characteristics
  TYPE(NEWLAKETYPE),ALLOCATABLE :: elake(:)       !<Lake characteristics (new lakebasin lakes)
  TYPE(LAKESECTIONTYPE),ALLOCATABLE :: lakesectiondata(:,:) !<Lake section data (laketype 1)
  TYPE(PATHTYPE),ALLOCATABLE  :: path(:)          !<Subbasin coupling with index
  TYPE(PATHTYPE),ALLOCATABLE  :: pathsubid(:)     !<Subbasin coupling with subid (read from file)
  TYPE(BRANCHTYPE),ALLOCATABLE  :: branchdata(:)  !<Subbasin branch coupling with index
  TYPE(BRANCHTYPE),ALLOCATABLE  :: branchsubid(:) !<Subbasin branch coupling with subid (read from file)
  TYPE(GLACIERTYPE),ALLOCATABLE  :: glacier(:)    !<Glacier characteristics
  TYPE(AQUIFERTYPE),ALLOCATABLE  :: aquifer(:)    !<Aquifer characteristics
  TYPE(NPCLOADTYPE),ALLOCATABLE  :: load(:)       !<Loads of nitrogen and phosphorus from point sources and local diffuse load
  TYPE(DEPOSITIONTYPE)  :: deposition !<Loads of nitrogen from atmospheric deposition
  TYPE(T1LOADTYPE),ALLOCATABLE  :: tload(:)       !<Load of general substance T1 from point source
  TYPE(POINTSOURCEDATATYPE),ALLOCATABLE  :: psinfo(:) !<Information of point sources as time series
  TYPE(POINTSOURCETYPE),ALLOCATABLE  :: psload(:) !<Load of point sources as time series
  TYPE(POINTSOURCEDATATYPE),ALLOCATABLE  :: absinfo(:) !<Information of abstraction as time series
  TYPE(POINTSOURCETYPE),ALLOCATABLE  :: absload(:) !<Abstraction as time series
  TYPE(CLASSBASINTYPE),ALLOCATABLE :: classbasin(:,:) !<Basin characteristics that is class dependent
  TYPE(CLASSTYPE),ALLOCATABLE :: classdata(:)     !<Class characteristics
  TYPE(CLASSGROUPTYPE),ALLOCATABLE :: classgroup(:)     !<Class group characteristics (GeoClass)
  TYPE(CLASSGROUPTYPE),ALLOCATABLE :: classgroup2(:)     !<Class group characteristics (info)
  TYPE(CROPDATATYPE),ALLOCATABLE :: cropdata(:)   !<Crop characteristics
  TYPE(CROPIRRDATATYPE),ALLOCATABLE :: cropirrdata(:)  !<Crop characteristics regarding irrigation
  TYPE(IRRIGATIONTYPE),ALLOCATABLE :: irrigationsystem(:) !<Irrigation characteristics
  TYPE(WATERTRANSFERTYPE),ALLOCATABLE :: watertransfer(:) !<Water transfer characteristics
  TYPE(WETLANDTYPE), ALLOCATABLE :: wetland(:,:)  !<River wetland characteristics (local and main river)
  TYPE(FLOODTYPE),ALLOCATABLE :: flooding(:)      !<Flood water bodies characteristics
  TYPE(SOILLEAKAGEDATATYPE) :: soilleak      !<Soil leakage concentrations
  INTEGER, ALLOCATABLE :: petmodel(:)             !<PET model given for subbasin
  INTEGER, ALLOCATABLE :: cropindex(:,:)          !<Index find cropdata for class/region
  INTEGER, ALLOCATABLE :: damindex(:)             !<Index find dam for subbasin
  INTEGER, ALLOCATABLE :: lakeindex(:)            !<Index find lake for subbasin
  INTEGER, ALLOCATABLE :: lakeout2index(:)        !<Index find lake outlet 2 for subbasin
  INTEGER, ALLOCATABLE :: glacierindex(:)         !<Index find glacier for subbasin
  INTEGER, ALLOCATABLE :: lakebasinindex(:)       !<Index find lakebasin for subbasin
  INTEGER, ALLOCATABLE :: floodindex(:)           !<Index find floodplain for subbasin
  INTEGER, ALLOCATABLE :: branchindex(:)          !<Index find branched subbasins
  LOGICAL, ALLOCATABLE :: tloadexist(:)           !<Index find tload for subbasin
!> \}

!> \name Variables for configuration of model set-up
!> \brief HYSS make available variables to hold information on the 
!> configuration of the model set-up.
!> \{
  !Information on model classes
  REAL, ALLOCATABLE ::    soildepth(:,:)          !<Lower border of soil layers (m) (layer,class)
  REAL, ALLOCATABLE ::    soilthick(:,:)          !<Thickness of soil layers (m) (layer,class)
  !Other model variables - to config type? /KN
  TYPE(STATEDIMENSIONTYPE) :: statesize  !<dimensions of states for simulation
  INTEGER nsub                    !<Number of subbasins in current run
  INTEGER nsub_basemodel          !<Number of subbasins in original model set up
  INTEGER numsubstances           !<Number of substances currently modelled
  INTEGER ncrop                   !<Number of crops (rows) in CropData.txt
  INTEGER nluse                   !<Number of land uses in GeoClass. (max number)
  INTEGER nsoil                   !<Number of soil types in GeoClass. (max number)
  INTEGER nclass                  !<Number of slc-classes GeoClass
  INTEGER num_classgroup          !<Number of class groups in GeoClass
  INTEGER numrotations            !<Number of crop rotation class groups
  INTEGER, ALLOCATABLE :: regiondivision(:)  !<parameter region division for each parameter (nregpar)
  INTEGER nregions(nregiondivisions)  !<Number of parameter regions in parregion
  INTEGER naquifers               !<Number of aquifers
  INTEGER nglaciers               !<Number of glaciers
  INTEGER nwtransfer              !<Number of water transfers (in MgmtData)
  INTEGER :: npsused = 0          !<Number of pointsources with time series
  INTEGER :: nabsused = 0         !<Number of abstractions with time series
  INTEGER, PARAMETER :: nrivertypes = 2
  INTEGER, PARAMETER :: nlaketypes = 2
  
  !Coded soil-landuse classes
  INTEGER,ALLOCATABLE :: classmodel(:)   !<Soil model for class
  INTEGER :: slc_olake     !<Class index for outlet lake
  INTEGER :: slc_ilake     !<Class index for local lake
  INTEGER :: slc_lriver    !<Class index for local river
  INTEGER :: slc_mriver    !<Class index for main river
  INTEGER :: slc_iwet      !<Class index for iwet, internal wetland
  INTEGER :: slc_owet      !<Class index for owet, outlet wetland
!>\}

!> \name Time variables
!> \brief HYSS make available variables to hold information on time  
!> during simulation run and for model set-up.
  !TODO: Create a time-structure for all these to send as argument into hype-subroutines?
!> \{
  TYPE(DateType) currentdate      !<Current date in DateType format Year yyyy, Month 01-12, Day 01-31, Hour 00-23, Minute 00-59
  INTEGER month                   !<Current month
  INTEGER dayno                   !<Current day number of year (Julian day)
  INTEGER prevdoy                 !<Number of days the previous year
  INTEGER tsofday                 !<Current time step of day (e.g. second ts of 24 this day)
  INTEGER seconds_per_timestep    !<Number of seconds for one time step
  INTEGER timesteps_per_day       !<Number of time steps per day
  LOGICAL firstoutstep            !<Flag for first timestep with print out
  LOGICAL endofday                !<Flag for timestep that is the last of the day
!> \}

!> \name Output variables
!> \brief HYSS make available variables to hold output values and 
!> information on output variables.
!> \{
  LOGICAL          :: conductload     !<flag for calculation of NP load
  INTEGER          :: max_outvar      !<number of defined output variables, set in define_output_variables
  TYPE(OUTVARIDTYPE),ALLOCATABLE :: outvarid(:)    !<information of output variables for this model
  INTEGER          :: noutvar      !<number of used output variables to be set
  INTEGER          :: noutvarclass      !<number of used output variables for classes to be set
  LOGICAL,ALLOCATABLE :: outvarstatus(:)   !<status of outvar to calculate for print out (max_outvar)
  REAL,ALLOCATABLE :: outvar(:,:)         !<variable to save output for print out in
  INTEGER,ALLOCATABLE :: outvarindex(:)   !<index to find outvar for idindex for subbasin output (max_outvar)
  INTEGER,ALLOCATABLE :: outvarclassindex(:)   !<index to find outvar for idindex for class output (max_outvar)
  REAL,ALLOCATABLE :: outvarclassdata(:,:,:)   !<class values for variable to save output for
  REAL,ALLOCATABLE :: outvarclassfraction(:,:,:)   !<class weights(area fractions) for variable to save output for
  REAL,ALLOCATABLE :: outvar_classload(:,:,:,:)  !<variable to save class and substance dependent load outputs for yearly accumulation
  REAL,ALLOCATABLE :: outvar_basinload(:,:,:)    !<variable to save substance dependent load outputs for yearly accumulation
  TYPE(CHANGEOUTVARTYPE),ALLOCATABLE :: changecritvar(:)   !<variable to tell which variables to use for criteria calculation instead of those given as input
  INTEGER,ALLOCATABLE :: obsfuncoutvar(:)   !<index to find outvar for special variables used for regional obsfunc calculations
  CHARACTER(LEN=6),DIMENSION(max_classoutvar+max_basinoutvar+1) :: loadheadings  !<heading for load-files
  NAMELIST /HEADNML/ loadheadings   !Why is this a namelist?
!> \}

!> \name Model parameter variables
!> \brief HYSS make available variables to hold information on and values for model parameters.
!> \{
  INTEGER          :: max_par           !<maximum number of defined model parameters
  TYPE(MODPARIDTYPE), ALLOCATABLE :: modparid(:)  !<information of model parameters for this model
  REAL,ALLOCATABLE :: genpar(:)             !<parameter values, general for whole model set up
  REAL,ALLOCATABLE :: basinpar(:,:)         !<parameter values for each subbasin
  REAL,ALLOCATABLE,TARGET :: landpar(:,:)   !<parameter values for each land use
  REAL,ALLOCATABLE,TARGET :: soilpar(:,:)   !<parameter values for each soil type
  REAL,ALLOCATABLE,TARGET :: regpar(:,:)    !<parameter values for each parregion
  REAL,ALLOCATABLE :: monthpar(:,:)         !<parameter values for each month
  REAL,ALLOCATABLE :: lakedatapar_ini(:,:)  !<parameter values from LakeData.txt
  REAL,ALLOCATABLE :: lakedatapar(:,:)      !<current parameter values for lakes (from LakeData.txt and par.txt)
  INTEGER,ALLOCATABLE :: lakedataparindex(:,:)  !<table giving for each subbasin ilake and olake the row where the lake-related parameters are to be found in lakedatapar
!> \}

!> \name Parameters for updating; code and dimension.
!> Several updating methods exist. These can be used separately or together. 
!> \{
  INTEGER, PARAMETER :: i_quseobs   = 1          !<Code for updating method, subbasin outflow Q-station updating
  INTEGER, PARAMETER :: i_tpcorr    = 2          !<Code for updating method, updating of total phosphorus out of subbasin
  INTEGER, PARAMETER :: i_wendupd   = 3          !<Code for updating method, updating of outlet lake water stage at end of time step
  INTEGER, PARAMETER :: i_qar       = 4          !<Code for updating method, subbasin outflow Q-station AR-updating
  INTEGER, PARAMETER :: i_war       = 5          !<Code for updating method, subbasin outflow Q updating based on AR on w-error
  INTEGER, PARAMETER :: i_tncorr    = 6          !<Code for updating method, updating of total nitrogen out of subbasin
  INTEGER, PARAMETER :: i_tnloccorr = 7          !<Code for updating method, updating of total nitrogen locally out of subbasin
  INTEGER, PARAMETER :: i_tploccorr = 8          !<Code for updating method, updating of total phosphorus locally out of subbasin
  INTEGER, PARAMETER :: i_cuseobs   = 9          !<Code for updating method, subbasin outflow concentration replacement
  INTEGER, PARAMETER :: dim_update  = 9          !<Number of updating methods (for array dimension)
!> \}
!> \name Variables for updating
!> \brief HYSS make available variables to hold information on updating. Several different updating functions exist.
!> \{
  TYPE UPDATETYPE
    LOGICAL, ALLOCATABLE :: station(:)    !<update status of each subbasin
    REAL, ALLOCATABLE :: factor(:)        !<update correction/AR factor of each subbasin
    CHARACTER(LEN=9) :: name              !<name of update method
  END TYPE UPDATETYPE

  LOGICAL :: doupdate(dim_update)               !<status of update functions
  INTEGER :: wobsvarindex                       !<variable (outvarid-index) to be used for updating wend and war
  TYPE(UPDATETYPE) :: useinupdate(dim_update)  !<update status for each method
!>!> \}

  CONTAINS

  !>Allocate variables for sending output from model to HYSS.
  !>
  !>\b Consequences Module variables outvar, outvarclassdata, outvarclassfraction,
  !>outvar_classload and outvar_basinload may be allocated.
  !----------------------------------------------------------------
  SUBROUTINE allocate_outvar(nout,noutclass)

    !Argument declarations
    INTEGER, INTENT(IN) :: nout   !<number of output variables to be calculated
    INTEGER, INTENT(IN) :: noutclass   !<number of output variables to be calculated for classes

    IF(.NOT.ALLOCATED(outvar)) ALLOCATE(outvar(nsub,nout))
    IF(.NOT.ALLOCATED(outvarclassdata)) ALLOCATE(outvarclassdata(nclass,nsub,noutclass))
    IF(.NOT.ALLOCATED(outvarclassfraction)) ALLOCATE(outvarclassfraction(nclass,nsub,noutclass))
    IF(conductload) THEN
      IF(.NOT.ALLOCATED(outvar_classload)) ALLOCATE(outvar_classload(nclass,max_classoutvar,numsubstances,nsub))
      IF(.NOT.ALLOCATED(outvar_basinload)) ALLOCATE(outvar_basinload(numsubstances,max_basinoutvar,nsub))
    ENDIF

  END SUBROUTINE allocate_outvar

  !>Deallocate modvar arrays
  !>
  !> \b Consequences A lot of memory is freed.
  !-------------------------------------------
  SUBROUTINE deallocate_modvar(n)
  
    !Argument declarations
    INTEGER, INTENT(IN) :: n    !<Number of subbasins
    INTEGER i
  
    IF(ALLOCATED(xobsi)) DEALLOCATE(xobsi)
    IF(ALLOCATED(xobsindex)) DEALLOCATE(xobsindex)
    IF(ALLOCATED(xoregindex)) DEALLOCATE(xoregindex)
    IF(ALLOCATED(basin)) DEALLOCATE(basin)
    IF(ALLOCATED(lake)) DEALLOCATE(lake)  
    IF(ALLOCATED(lakebasin)) DEALLOCATE(lakebasin)
    IF(ALLOCATED(path)) DEALLOCATE(path)
    IF(ALLOCATED(aquifer)) DEALLOCATE(aquifer)
    IF(ALLOCATED(glacier)) DEALLOCATE(glacier)
    IF(ALLOCATED(load))THEN
      DO i = 1,n
        IF(ALLOCATED(load(i)%psvol)) DEALLOCATE(load(i)%psvol)
        IF(ALLOCATED(load(i)%psload)) DEALLOCATE(load(i)%psload)
        IF(ALLOCATED(load(i)%locconc)) DEALLOCATE(load(i)%locconc)
      ENDDO
      DEALLOCATE(load)
    ENDIF
    IF(ALLOCATED(deposition%inloadwater)) DEALLOCATE(deposition%inloadwater)
    IF(ALLOCATED(deposition%indryload)) DEALLOCATE(deposition%indryload)
    IF(ALLOCATED(deposition%inwetconc)) DEALLOCATE(deposition%inwetconc)
    IF(ALLOCATED(outvar)) DEALLOCATE(outvar)
    IF(ALLOCATED(basinpar)) DEALLOCATE(basinpar)
    IF(ALLOCATED(landpar)) DEALLOCATE(landpar)
    IF(ALLOCATED(soilpar)) DEALLOCATE(soilpar)
    IF(ALLOCATED(regpar)) DEALLOCATE(regpar)
    DO i=1,dim_update
      IF(ALLOCATED(useinupdate(i)%station)) DEALLOCATE(useinupdate(i)%station)
      IF(ALLOCATED(useinupdate(i)%factor)) DEALLOCATE(useinupdate(i)%factor)
    ENDDO
    IF(ALLOCATED(lakeindex)) DEALLOCATE(lakeindex)     
    IF(ALLOCATED(lakeout2index)) DEALLOCATE(lakeout2index)     
    IF(ALLOCATED(lakebasinindex)) DEALLOCATE(lakebasinindex)

  END SUBROUTINE deallocate_modvar

  !>Deallocate glacier arrays
  !>
  !> \b Consequences Module variables glacier is deallocated.
  !---------------------------------------
  SUBROUTINE deallocate_glacier()
  
    IF(ALLOCATED(glacier)) DEALLOCATE(glacier)

  END SUBROUTINE deallocate_glacier

  !>Allocate the model parameter variables
  !>
  !> \b Consequences Six module variables for model parameters are allocated.
  !------------------------------------------------------------------
  SUBROUTINE allocate_modelparameters(ns,nregpar)

    INTEGER, INTENT(IN) :: ns  !<number of subbasins
    INTEGER, INTENT(OUT) :: nregpar  !<number of regional parameters
    
    !Local variables
    INTEGER ngenpar,nsoilpar,nlandpar,nbasinpar,nmonthpar
    INTEGER i

    !> \b Algorithm \n
    !> Calculate number of model parameters of each dependence type
    ngenpar = 0; nsoilpar = 0; nlandpar = 0
    nbasinpar = 0; nregpar = 0
    nmonthpar = 0
    DO i = 1,max_par
      IF(modparid(i)%deptype==m_gpar)   ngenpar   = MAX(ngenpar,modparid(i)%parno)
      IF(modparid(i)%deptype==m_spar)   nsoilpar  = MAX(nsoilpar,modparid(i)%parno)
      IF(modparid(i)%deptype==m_lpar)   nlandpar  = MAX(nlandpar,modparid(i)%parno)
      IF(modparid(i)%deptype==m_bpar)   nbasinpar = MAX(nbasinpar,modparid(i)%parno)
      IF(modparid(i)%deptype==m_rpar)   nregpar   = MAX(nregpar,modparid(i)%parno)
      IF(modparid(i)%deptype==m_mpar)   nmonthpar = MAX(nmonthpar,modparid(i)%parno)
    ENDDO
    
    !>Allocate variables for holding model parameter values
    IF(.NOT.ALLOCATED(genpar)) ALLOCATE(genpar(ngenpar))
    IF(.NOT.ALLOCATED(soilpar)) ALLOCATE(soilpar(nsoilpar,nsoil))
    IF(.NOT.ALLOCATED(landpar)) ALLOCATE(landpar(nlandpar,nluse))
    IF(.NOT.ALLOCATED(basinpar)) ALLOCATE(basinpar(nbasinpar,ns))
    IF(.NOT.ALLOCATED(regpar)) ALLOCATE(regpar(nregpar,MAXVAL(nregions)))
    IF(.NOT.ALLOCATED(monthpar)) ALLOCATE(monthpar(nmonthpar,12))

    !>Initialize variables to zero
    IF(ALLOCATED(genpar)) genpar = 0.
    IF(ALLOCATED(soilpar)) soilpar = 0.
    IF(ALLOCATED(landpar)) landpar = 0.
    IF(ALLOCATED(basinpar)) basinpar = 0.
    IF(ALLOCATED(regpar)) regpar = 0.
    IF(ALLOCATED(monthpar)) monthpar = 0.
  
  END SUBROUTINE allocate_modelparameters

  !>Allocate variables for subbasin information
  !>
  !> \b Consequences Module variables for indata are allocated.
  !--------------------------------------------------------------
  SUBROUTINE allocate_basinvariables(ns,nsubst)

    INTEGER, INTENT(IN) :: ns     !<number of subbasins
    INTEGER, INTENT(IN) :: nsubst !<number of substances

    INTEGER i 
    
    IF(.NOT.ALLOCATED(basin)) ALLOCATE(basin(ns))
    IF(.NOT.ALLOCATED(pathsubid)) ALLOCATE(pathsubid(ns))
    ALLOCATE(load(ns))
    DO i = 1,ns
      ALLOCATE(load(i)%locconc(nsubst))
    ENDDO
    IF(.NOT.ALLOCATED(deposition%inloadwater)) ALLOCATE(deposition%inloadwater(ns,12))
    IF(.NOT.ALLOCATED(deposition%indryload)) ALLOCATE(deposition%indryload(ns,3))
    IF(.NOT.ALLOCATED(deposition%inwetconc)) ALLOCATE(deposition%inwetconc(ns))
    IF(.NOT.ALLOCATED(wetland)) ALLOCATE(wetland(ns,2))
    IF(.NOT.ALLOCATED(classbasin)) ALLOCATE(classbasin(ns,nclass))
    IF(.NOT.ALLOCATED(petmodel)) ALLOCATE(petmodel(ns))
    IF(.NOT.ALLOCATED(lakesectiondata)) ALLOCATE(lakesectiondata(ns,maxlakesections))

  END SUBROUTINE allocate_basinvariables

  !>Initialize model simulation configuration variable with default values   
  !--------------------------------------------------------------
  SUBROUTINE initialize_simulation_configuration()

    conduct%simN = .FALSE.
    conduct%simP = .FALSE.
    conduct%simC = .FALSE.
    conduct%simS = .FALSE.
    conduct%simT1 = .FALSE.
    conduct%simT2 = .FALSE.
    conduct%arupdating = .FALSE.
    conduct%floodplain = .FALSE.
    conduct%glacier = .FALSE.
    conduct%growthdegreeday = .FALSE.
    conduct%icelens = .FALSE.
    conduct%irrigation = .FALSE.
    conduct%lakeriverice = .FALSE.
    conduct%riverwetland = .FALSE.
    conduct%watertransfer = .FALSE.
    conduct%qbank = .FALSE.
    conduct%snowheat = .FALSE.
    conduct%traveltime = .FALSE.
    conduct%connectivity = .FALSE.
    
  END SUBROUTINE initialize_simulation_configuration

  !>Allocate and set model optional structure variable    
  !>
  !> \b Consequences Module variables for model options are allocated.
  !--------------------------------------------------------------
  SUBROUTINE allocate_initialize_modeloptions()

    ALLOCATE(modeloptionname(num_modelprocess))
    ALLOCATE(modeloption(num_modelprocess))
    modeloption = 0
    modeloptionname(p_snowfall) = 'snowfallmodel   '
    modeloptionname(p_snowfalldist) = 'snowfalldist    '
    modeloptionname(p_snowmelt) = 'snowmeltmodel   '
    modeloptionname(p_snowevap) = 'snowevaporation '
    modeloptionname(p_snowdensity) = 'snowdensity     '
    modeloptionname(p_lakeriverice) = 'lakeriverice    '
    modeloptionname(p_swtemperature) = 'swtemperature   '
    modeloptionname(p_growthstart) = 'growthstartmodel'
    modeloptionname(p_petmodel) = 'petmodel        '
    modeloptionname(p_deepgroundwater) = 'deepground      '
    modeloptionname(p_floodplain) = 'floodmodel      '
    modeloptionname(p_glacierini) = 'glacierini      '
    modeloptionname(p_infiltration) = 'infiltration    '
    modeloptionname(p_erosion) = 'erosionmodel    '
    modeloptionname(p_wetland) = 'wetlandmodel    '
    modeloptionname(p_soilleakage) = 'soilleakage     '
    modeloptionname(p_frozensoil) = 'frozensoil      '
    modeloptionname(p_snowheat) = 'snowheat        '
    modeloptionname(p_surfacerunoff) = 'surfacerunoff   '
    modeloptionname(p_connectivity) = 'connectivity    '

  END SUBROUTINE allocate_initialize_modeloptions

  !>Allocate and set substance variable    
  !--------------------------------------------------------------
  FUNCTION substance_name(i) RESULT (shortname)

    INTEGER, INTENT(IN) :: i       !<Index of substance
    CHARACTER(LEN=2) :: shortname  !<Name of substance
    
    shortname = ''
    IF(i==i_in) shortname = 'IN'
    IF(i==i_on) shortname = 'ON'
    IF(i==i_sp) shortname = 'SP'
    IF(i==i_pp) shortname = 'PP'
    IF(i==i_oc) shortname = 'OC'
    IF(i==i_t1) shortname = 'T1'
    IF(i==i_t2) shortname = 'T2'
    IF(i==i_ss) shortname = 'SS'
    IF(i==i_ae) shortname = 'AE'
    
  END FUNCTION substance_name

  !>Set update method names
  !>
  !> \b Consequences Module variable useinupdate are set.
  !--------------------------------------------------------------
  SUBROUTINE set_update_method_names()

    useinupdate(i_quseobs)%name   = 'quseobs  '
    useinupdate(i_cuseobs)%name   = 'cuseobs  '
    useinupdate(i_wendupd)%name   = 'wendupd  '
    useinupdate(i_qar)%name       = 'q-AR     '
    useinupdate(i_war)%name       = 'w-AR     '
    useinupdate(i_tpcorr)%name    = 'tpcorr   '
    useinupdate(i_tncorr)%name    = 'tncorr   '
    useinupdate(i_tploccorr)%name = 'tploccorr'
    useinupdate(i_tnloccorr)%name = 'tnloccorr'

  END SUBROUTINE set_update_method_names

  !>Find (new) water transfer (index) for current subbasin
  !---------------------------------------------------------------
  SUBROUTINE find_next_watertransfer(i,lastwt,wtindex)

    INTEGER, INTENT(IN) :: i         !<Index of subbasin
    INTEGER, INTENT(IN) :: lastwt    !<Index of last water transfer found
    INTEGER, INTENT(OUT) :: wtindex  !<Index of next water transfer

    INTEGER itrans
    
    wtindex = 0
    DO itrans = lastwt+1, nwtransfer
      IF(basin(i)%subid==watertransfer(itrans)%srcsubid)THEN
        wtindex = itrans
        RETURN
      ENDIF
    ENDDO
    
  END SUBROUTINE find_next_watertransfer

  !>Find (next) point source (index) for current subbasin
  !---------------------------------------------------------------
  SUBROUTINE find_next_pointsource(isb,lastps,nps,psdata,nextps)

    INTEGER, INTENT(IN) :: isb       !<Index of subbasin
    INTEGER, INTENT(IN) :: lastps    !<Index of last point source found
    INTEGER, INTENT(IN) :: nps       !<Number of point sources
    TYPE(POINTSOURCEDATATYPE),INTENT(IN) :: psdata(nps) !<Point source/abstraction data information
    INTEGER, INTENT(OUT) :: nextps   !<Index of next point source

    INTEGER ips
    
    nextps = 0
    DO ips = lastps+1, nps
      IF(psdata(ips)%subindex==isb)THEN
        nextps = ips
        RETURN
      ENDIF
    ENDDO
    
  END SUBROUTINE find_next_pointsource

  !>Initiate information about other observation series than PTQ
  !>
  !> \b Consequences Module index array (xobsindex) is allocated and initialized.
  !---------------------------------------------------------------
  SUBROUTINE initiate_xobsinformation(array,n)

    INTEGER, INTENT(INOUT), ALLOCATABLE :: array(:,:)     !<Index array
    INTEGER, INTENT(IN) :: n         !<Number of subbasins/outregions

    IF(.NOT.ALLOCATED(array)) ALLOCATE(array(max_outvar,n))
    array = 0

  END SUBROUTINE initiate_xobsinformation

  !>Save information about finding other observation series
  !>
  !> \b Consequences Module index array (xobsindex) is set.
  !-------------------------------------------------------
  SUBROUTINE save_xobsinformation(n,var2,ns)

    INTEGER, INTENT(IN) :: n          !<Number of columns in Xobs
    INTEGER, INTENT(IN) :: var2(n,2)  !<Variables and subid for Xobs columns
    INTEGER, INTENT(IN) :: ns         !<Number of subbasins
    
    !Local variables
    INTEGER ivar,isub,icol

    DO ivar = 1, max_outvar
      DO isub = 1, ns
        DO icol = 1,n
          IF(var2(icol,1)==ivar .AND. var2(icol,2)==basin(isub)%subid) THEN
            xobsindex(ivar,isub) = icol
            EXIT
          ENDIF
        ENDDO
      ENDDO
    ENDDO

  END SUBROUTINE save_xobsinformation

  !>Calculate and set the soil layer depth from GeoClass.
  !>
  !>\b Consequences Module variable classdata%soildepth is set.
  !----------------------------------------------------------------------------------
  SUBROUTINE set_soildepth_classdata(nlayer,depth,geoclassdata)

    INTEGER, INTENT(IN) :: nlayer         !<number of soil layers
    REAL, INTENT(IN)    :: depth(nlayer)  !<soil layer depth (m)
    TYPE(CLASSTYPE) :: geoclassdata       !<variable holding class characteristics

    geoclassdata%soildepth(1:nlayer) = depth
    IF(nlayer<maxsoillayers) geoclassdata%soildepth(nlayer+1:maxsoillayers) = geoclassdata%soildepth(nlayer)

  END SUBROUTINE set_soildepth_classdata

  !>Calculate the soil layer depth and thicknesses (default values).
  !>
  !>\b Consequences Module variable soildepth and soilthick are allocated and set.
  !----------------------------------------------------------------------------------
  INTEGER FUNCTION set_soildepth(nlayer,slc,depth)

    INTEGER, INTENT(IN) :: nlayer         !<number of soil layers
    INTEGER, INTENT(IN) :: slc            !<class index
    REAL, INTENT(IN)    :: depth(nlayer)  !<soil layer depth (m)
    !< \retval set_soildepth error status of function

    !Local variables
    INTEGER layer

    set_soildepth = 0
    IF(.NOT.ALLOCATED(soildepth)) ALLOCATE(soildepth(maxsoillayers,nclass))
    IF(.NOT.ALLOCATED(soilthick)) ALLOCATE(soilthick(maxsoillayers,nclass))

    soildepth(1:nlayer,slc) = depth
    soilthick(:,slc) = 0.
    DO layer = 1,nlayer
      IF(layer==1)THEN
        soilthick(layer,slc) = depth(layer)
      ELSE
        soilthick(layer,slc) = depth(layer)-soildepth(layer-1,slc)
      ENDIF
    ENDDO
    IF(nlayer>0.AND.nlayer<maxsoillayers)THEN
      soildepth(nlayer+1:maxsoillayers,slc) = soildepth(nlayer,slc)
      soilthick(nlayer+1:maxsoillayers,slc) = 0.
    ENDIF

  END FUNCTION set_soildepth

  !>\brief Find special coded classes
  !>
  !>\b Consequences Module variable slc_ilake, slc_olake, slc_lriver
  !> slc_mriver, slc_iwet, and slc_owet are set, as is classmodel.
  !---------------------------------------------------------------
  SUBROUTINE set_coded_classes(dim,array,ilakearg,olakearg,lriverarg,mriverarg,iwetarg,owetarg)

    INTEGER, INTENT(IN)  :: dim                 !<nclass
    REAL   , INTENT(IN)  :: array(dim)          !<special class column from GeoClass
    INTEGER, INTENT(OUT),OPTIONAL :: ilakearg   !<class index for ilake
    INTEGER, INTENT(OUT),OPTIONAL :: olakearg   !<class index for olake
    INTEGER, INTENT(OUT),OPTIONAL :: lriverarg  !<class index for local river
    INTEGER, INTENT(OUT),OPTIONAL :: mriverarg  !<class index for main river
    INTEGER, INTENT(OUT),OPTIONAL :: iwetarg    !<class index for iwet
    INTEGER, INTENT(OUT),OPTIONAL :: owetarg    !<class index for owet

    !Variable declaration
    INTEGER j                       !loop variable, class

    !>\b Algorithm \n
    !>Set special model code for all classes
    IF(.NOT.ALLOCATED(classmodel)) ALLOCATE(classmodel(dim))
    classmodel = NINT(array)

    slc_ilake = 0
    slc_olake = 0
    slc_lriver = 0
    slc_mriver = 0
    slc_iwet = 0
    slc_owet = 0
    DO j = 1,dim
      IF(array(j)==1) slc_olake = j
      IF(array(j)==2) slc_ilake = j
      IF(array(j)==11) slc_lriver = j
      IF(array(j)==12) slc_mriver = j
      IF(array(j)==13) slc_iwet = j
      IF(array(j)==14) slc_owet = j
    ENDDO

    !>Set coded lake classes for SourceApp
    IF(PRESENT(ilakearg)) ilakearg = slc_ilake
    IF(PRESENT(olakearg)) olakearg = slc_olake
    IF(PRESENT(lriverarg)) lriverarg = slc_lriver
    IF(PRESENT(mriverarg)) mriverarg = slc_mriver
    IF(PRESENT(iwetarg)) iwetarg = slc_iwet
    IF(PRESENT(owetarg)) owetarg = slc_owet

  END SUBROUTINE set_coded_classes

  !>Get classgroup information for a class group
  !------------------------------------------------------------------------
  SUBROUTINE get_classgroup_classes(name,nslc,classindex)

    USE CONVERT, ONLY : get_lower_case
    
    CHARACTER(LEN=*), INTENT(IN) :: name      !<name of class group
    INTEGER, INTENT(OUT) :: nslc      !<number of classes
    INTEGER,ALLOCATABLE, INTENT(OUT) :: classindex(:)      !<index of classes

    !Local variables 
    INTEGER icg
    LOGICAL found
    
    !> \b Algorithm \n

    !>Allocate and set classes in classgroup for that class group
    found = .FALSE.
    DO icg = 1,SIZE(classgroup2)
      IF(get_lower_case(6,name)==get_lower_case(6,classgroup2(icg)%name))THEN
        nslc = classgroup2(icg)%ncgslcs
        IF(ALLOCATED(classindex)) DEALLOCATE(classindex)
        ALLOCATE(classindex(nslc))
        classindex = classgroup2(icg)%slcs
        found = .TRUE.
        EXIT
      ENDIF
    ENDDO
    IF(.NOT.found)THEN
      WRITE(6,*) 'ERROR: No matching classgroup name found for output, ',TRIM(name)
      STOP 1
    ENDIF

  END SUBROUTINE get_classgroup_classes

  !>\brief Gets current soil leakage concentration.
  !------------------------------------------------------------------------------
  FUNCTION get_current_soilleakage_memory(icl,isub,nsubst) RESULT(output)

    !Argument declarations
    INTEGER, INTENT(IN)  :: icl          !<current class
    INTEGER, INTENT(IN)  :: isub         !<current subbasin
    INTEGER, INTENT(IN)  :: nsubst       !<number of substances
    REAL :: output(nsubst)               !<current soil leakage output
    
    !>Monthly soilleakage concentrations
    IF(modeloption(p_soilleakage)==1)THEN
      output = soilleak%concentration(:,month,isub)
    !>Or monthly soilleakage load
    ELSEIF(modeloption(p_soilleakage)==2 .OR. modeloption(p_soilleakage)==3)THEN
      output = soilleak%load(:,icl,month,isub)
    ENDIF

  END FUNCTION get_current_soilleakage_memory

  !>Initiate CropData variables to zero
  !>
  !>\b Consequences Module variable cropdata are set.
  !------------------------------------------------------------------------
  SUBROUTINE initiate_cropdata()

    cropdata(:)%fertnamount1  = 0      !fertiliser amount N (kg/km2)
    cropdata(:)%fertpamount1  = 0      !fertiliser amount P (kg/km2)
    cropdata(:)%fertday1      = 0      !day for fertilizing 1 (dayno)
    cropdata(:)%fertdown1     = 0      !part of fertilizer amount ploughed down to soillayer 2
    cropdata(:)%mannamount1   = 0      !manure amount N (kg/km2)
    cropdata(:)%manpamount1   = 0      !manure amount P (kg/km2)
    cropdata(:)%manday1       = 0      !day for manuring 1 (dayno)
    cropdata(:)%mandown1      = 0      !part of manure amount ploughed down to soillayer 2
    cropdata(:)%fertnamount2  = 0      !fertiliser amount N (kg/km2)
    cropdata(:)%fertpamount2  = 0      !fertiliser amount P (kg/km2)
    cropdata(:)%fertday2      = 0      !day for fertilizing 2 (dayno)
    cropdata(:)%fertdown2     = 0      !part of fertilizer amount ploughed down to soillayer 2
    cropdata(:)%mannamount2   = 0      !manure amount N (kg/km2)
    cropdata(:)%manpamount2   = 0      !manure amount P (kg/km2)
    cropdata(:)%manday2       = 0      !day for manuring 2 (dayno)
    cropdata(:)%mandown2      = 0      !part of manure amount ploughed down to soillayer 2
    cropdata(:)%resnamount    = 0      !residual amount of N (kg/km2)
    cropdata(:)%respamount    = 0      !residual amount of P (kg/km2)
    cropdata(:)%rescamount    = 0      !litter fall of C (kg/km2)
    cropdata(:)%resdayno      = 0      !day for residual (dayno), code 0 give residuals every day
    cropdata(:)%resdown       = 0      !part of residual amount ploughed down to soillayer 2
    cropdata(:)%resfast       = 0      !part of residual amount to fastN/P pool (rest to humusN/P)
    cropdata(:)%uptake1       = 0      !parameter for plant uptake of nutrient 1
    cropdata(:)%uptake2       = 0      !parameter for plant uptake of nutrient 2
    cropdata(:)%uptake3       = 0      !parameter for plant uptake of nutrient 3
    cropdata(:)%baredayno1    = 0      !day for beginning of first period with bare soil (dayno), typically 1
    cropdata(:)%baredayno2    = 0      !day for end of first period with bare soil (dayno), typically sawing date + a few days
    cropdata(:)%baredayno3    = 0      !day for beginning of second period with bare soil (dayno), typically ploughing date
    cropdata(:)%baredayno4    = 0      !day for end of second period with bare soil (dayno), typically 365
    cropdata(:)%baredayno5    = 0      !day for end of second period with bare soil (dayno), typically saw date of autumn crop
    cropdata(:)%ccmax1        = 0      !crop cover during summer
    cropdata(:)%ccmax2        = 0      !crop cover during winter and all year for year-round-crops (e.g. forest)
    cropdata(:)%gcmax1        = 0      !ground cover during summer
    cropdata(:)%gcmax2        = 0      !ground cover during winter and all year for year-round-crops (e.g. forest)
    cropdata(:)%uptakeupper   = 1.     !plant uptake from upper soillayer
    cropdata(:)%PNuptakeRatio = 0      !plant uptake P:N ratio
    cropdata(:)%daylength     = 0      !minimum daylength required for start to accumulate gdd
    cropdata(:)%gddsow        = 0      !accumulated growing degree days needed for sowing
    cropdata(:)%basetemp      = 0      !temperature which is deducted from air temperature when calculating gdd
    cropdata(:)%firstday      = 0      !first possible day for gdd accumulation
    cropirrdata(:)%plantingdayno   = 0    !dayno for irrigation season start
    cropdata(:)%t1amount      = 0      !amount T1 (kg/km2)       
    cropdata(:)%t1year        = 0      !year of T1 application      
    cropdata(:)%t1day         = 0      !first day of T1 application
    cropdata(:)%t1numberofdays = 0     !number of days with T1 application       
    cropdata(:)%t1daydown     = 0      !day of T1 incorporation 
    cropdata(:)%t1down1       = 0      !part of T1 ploughed down to soillayer 1
    cropdata(:)%t1down2       = 0      !part of T1 ploughed down to soillayer 2

  END SUBROUTINE initiate_cropdata

  !>Set the index variable for CropData
  !>
  !>\b Consequences Module variable cropindex may be allocated.
  !---------------------------------------------------------------------
  SUBROUTINE set_cropdataindex(n,id,reg,cindex)

    INTEGER, INTENT(IN)  :: n         !<number of crops/rows in CropData
    INTEGER, INTENT(IN)  :: id(n)     !<cropid
    INTEGER, INTENT(IN)  :: reg(n)    !<region
    INTEGER, ALLOCATABLE, INTENT(INOUT)  :: cindex(:,:) !<cropindex
    
    !Local variables
    INTEGER irow
    INTEGER maxreg,maxid

    !Allocate and initialise cropindex      
    maxreg = MAXVAL(reg)
    maxid  = MAXVAL(id)
    IF(.NOT.ALLOCATED(cindex)) ALLOCATE(cindex(maxid,maxreg))
    cindex = 0

    DO irow = 1,n
      cindex(id(irow),reg(irow)) = irow
    ENDDO

  END SUBROUTINE set_cropdataindex

  !>Finds the corresponding row in CropData for the given crop and the
  !>fraction of the classarea its occupying.
  !---------------------------------------------------------------------
  SUBROUTINE find_croppart(i,j,kcrop,row,part)

    INTEGER, INTENT(IN)  :: i       !<index of current subbasin
    INTEGER, INTENT(IN)  :: j       !<index of current class
    INTEGER, INTENT(IN)  :: kcrop   !<main (1) or secondary (2) crop
    INTEGER, INTENT(OUT) :: row     !<row in CropData for this crop
    REAL,    INTENT(OUT) :: part    !<fraction of classarea for this crop
    
    !Local variables
    INTEGER c,r
    row = 0

    !> \b Algorithm \n
    !>For main crop: get the row from cropindex, areafraction is 1
    IF(kcrop==1)THEN
      c = classdata(j)%crop
      r = basin(i)%region
      IF(c>0.AND.r>0) row = cropindex(c,r)
      part = 1.
    !>For secondary crop: get the row from cropindex and areafraction from classbasin
    ELSE
      c = classdata(j)%crop2
      r = basin(i)%region
      IF(c>0.AND.r>0) row = cropindex(c,r)
      part = classbasin(i,j)%part2cr
    ENDIF

  END SUBROUTINE find_croppart

  !>Calculate pseudo daynumber starting July 1 on southern hemisphere
  !------------------------------------------------------------------
  INTEGER FUNCTION get_pseudo_dayno(dn,latitude)
  
  !Argument declarations
  INTEGER, INTENT(IN) :: dn       !<Julian day number (1=1 Jan)
  REAL, INTENT(IN)    :: latitude !<latitude in degrees [-90,90]
  
  IF(latitude>=0.)THEN  !northern hemisphere
    get_pseudo_dayno = dn
  ELSE                  !southern hemisphere
    get_pseudo_dayno = dn - 181   !1=1 July (at least non leap years)
    IF(get_pseudo_dayno<0) get_pseudo_dayno = get_pseudo_dayno + 365
  ENDIF

  END FUNCTION get_pseudo_dayno
  
  !>\brief Function to check day in given period
  !>End is a fixed dayno. It is not assumed to be larger than begin day, 
  !>but can be. A smaller end than begin assume the period is over new year.
  !------------------------------------------------------------------
  LOGICAL FUNCTION in_season_end(begjd,endjd)
    
    !Argument declarations
    INTEGER, INTENT(IN) :: begjd   !<julian day number of beginning of season
    INTEGER, INTENT(IN) :: endjd   !<julian day number of ending of season
    
    !Local variables
    LOGICAL status
    INTEGER endday

    endday = endjd
    IF(endjd < begjd) endday = endjd + 365
    status = .FALSE.
    IF(endday > 365)THEN
      IF(dayno >= begjd) status = .TRUE.
      IF(dayno <= endday - 365) status = .TRUE.
    ELSE
      IF(dayno >= begjd .AND. dayno <= endday) status = .TRUE.
    ENDIF
    in_season_end = status
  
  END FUNCTION in_season_end

  !>\brief Function to check day in given period
  !>End dayno is varying with leapyear. Begin dayno assumed less than 366.
  !------------------------------------------------------------------
  LOGICAL FUNCTION in_season_period(begjd,period)
    
    !Argument declarations
    INTEGER, INTENT(IN) :: begjd   !<julian day number of beginning of season
    INTEGER, INTENT(IN) :: period  !<length of period in days
    
    !Local variables
    INTEGER endjd
    Logical status

    endjd = begjd + period - 1  !including endjd in period, handles period=0
    status = .FALSE.
    IF(endjd > 365)THEN
      IF(dayno >= begjd) status = .TRUE.
      IF(dayno <= endjd - prevdoy) status = .TRUE.
    ELSE
      IF(dayno >= begjd .AND. dayno <= endjd) status = .TRUE.
    ENDIF
    in_season_period = status
  
  END FUNCTION in_season_period

  !>Get lake data type for lakebasin. 0 for no lake/lakebasin
  !------------------------------------------------------------------
  SUBROUTINE find_lbtype(isub,ldtype)

    !Argument declarations
    INTEGER, INTENT(IN) :: isub     !<index of subbasin
    INTEGER, INTENT(OUT) :: ldtype   !<ldtype according to lakedata
    
    !>Set upstream lakebasin flag
    ldtype = 0
    IF(ALLOCATED(lakebasinindex))THEN
      IF(lakebasinindex(isub)>0)THEN
        ldtype=3
        IF(lakebasin(lakebasinindex(isub))%last) ldtype = 4
      ENDIF
    ENDIF
    
  END SUBROUTINE find_lbtype

  !>Find subbasins with lakebasins beloning to this lake
  !------------------------------------------------------------------
  SUBROUTINE find_lakebasins(isub,status)

    !Argument declarations
    INTEGER, INTENT(IN) :: isub     !<index of subbasin of current lake
    LOGICAL, INTENT(OUT) :: status(nsub)   !<status of lake basins
    
    !Local variabled
    INTEGER i2
    
    !>Set lakebasin subasins found
    status = .FALSE.
    status(isub) = .TRUE.
    DO i2 = 1,isub-1
      IF(lakebasinindex(i2)>0)THEN
        IF(lakebasin(lakebasinindex(i2))%ilk==lakebasin(lakebasinindex(isub))%ilk) status(i2) = .TRUE.
      ENDIF
    ENDDO
    
  END SUBROUTINE find_lakebasins

  !>Get direct connection between two subbasins. 0 for no direct connection
  !------------------------------------------------------------------
  SUBROUTINE check_connected_subbasins(i2,i3,isource,ibranch)

    !Argument declarations
    INTEGER, INTENT(IN) :: i2         !<index of subbasin
    INTEGER, INTENT(IN) :: i3         !<index of subbasin
    INTEGER, INTENT(OUT) :: isource   !<index of upstream subbasin
    INTEGER, INTENT(OUT) :: ibranch   !<index of branch (1=main,2=branch)
    
    !>Set upstream lakebasin flag
    isource = 0
    ibranch = 0
    
    IF(path(i2)%main==i3)THEN
      isource = i2
      ibranch = 1
    ELSEIF(path(i3)%main==i2)THEN
      isource = i3
      ibranch = 1
    ELSEIF(ALLOCATED(branchindex))THEN
      IF(branchindex(i2)>0)THEN
        IF(branchdata(branchindex(i2))%branch==i3)THEN
          isource = i2
          ibranch = 2
        ENDIF
      ELSEIF(branchindex(i3)>0)THEN
        IF(branchdata(branchindex(i3))%branch==i2)THEN
          isource = i3
          ibranch = 2
        ENDIF
      ENDIF
    ENDIF
    
  END SUBROUTINE check_connected_subbasins

  !>Check for upstream lakebasin
  !------------------------------------------------------------------
  LOGICAL FUNCTION is_upstream_lakebasin(ldtype) 

    !Argument declarations
    INTEGER, INTENT(IN) :: ldtype     !<ldtype according to lakedata
    
    !>Set upstream lakebasin flag
    is_upstream_lakebasin = .FALSE.
    IF(ldtype==3) is_upstream_lakebasin = .TRUE.
    
  END FUNCTION is_upstream_lakebasin

  !>Get the information about aquifers from AquiferData.txt

END MODULE MODVAR
