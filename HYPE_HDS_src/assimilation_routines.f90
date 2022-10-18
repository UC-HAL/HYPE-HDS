!> \file assimilation_routines.f90
!> Contains module assimilation_routines, with model independent subroutines and functions used for data assimilation.
!  Author: D.Gustafsson (SMHI)
!  Versions: 
!  2011-2015:  Developed by D.Gustafsson (SMHI/KTH) and J.Ahlberg (KTH). The latest version from 2015-09-20 was used with the HOPE model for a "HUVA" snow data assimilation project 2013-2015.
!  2016.10.20: Module name changed to ASSIMILATION_ROUTINES and additional code convention adaptations for new implementation in HYPE_4_12_+.
!  2017.04.27: Replace all code dependent on Intel MKL library with intrinsic functions and other new/old code.
  
!> Generic subroutines and functions for (Ensemble Kalman Filter) Data Assimilation.
!> These functions are supposed to be general, and should not be changed
!> Functions special for a specific model application are in assimilation_interface.f90

MODULE ASSIMILATION_ROUTINES
!Copyright 2016-2017,2020 SMHI
!
!This file is part of HYPE.
!HYPE is free software: you can redistribute it and/or modify it under the terms of the Lesser GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
!HYPE is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the Lesser GNU General Public License for more details.
!You should have received a copy of the Lesser GNU General Public License along with HYPE. If not, see <http://www.gnu.org/licenses/>.
!-----------------------------------------------------------------------------------------

  USE ASSIMILATION_VARIABLES
  USE RANDOM_ROUTINES
  
  IMPLICIT NONE

CONTAINS
!-----------------------------------------------------------------------------------
!VARIOUS ENSEMBLE DATA MANIPULATION ROUTINES
!-----------------------------------------------------------------------------------
  
!<\brief Re-initilize selected state ensembles to mean or median to avoid numerical issues.
  SUBROUTINE meanORmedian_to_ensemble(nx,assimX,meanORmedian)

    !Argument declations
    TYPE(assim_state_ensemble_type) :: assimX(:)   !<ensemble data
    INTEGER, INTENT(IN)       :: nx                !<number of variables
    LOGICAL, INTENT(IN)       :: meanORmedian      !<flag for setting mean (T) or median (F)

    INTEGER i,j,k
    !loop over state ensembles
    DO k=1,nx
      IF(.not.assimX(k)%x%assimilate)THEN
        !loop over ensemble members
        DO j=1,assimX(k)%x%nens
          IF(ALLOCATED(assimX(k)%x%x))THEN
            !if matrix allocated...
            !loop over model units (often subbasins)
            DO i=1,assimX(k)%x%nvar
              IF(meanORmedian)THEN
                assimX(k)%x%x(i,j) = assimX(k)%x%outmean(i)
              ELSE
                ! switch to "outmedian"
                assimX(k)%x%x(i,j) = assimX(k)%x%outquant(2,i)
              ENDIF
            ENDDO
          ELSE
            !else..  write model units to bin-file, mean or median
            IF(meanORmedian)THEN
              WRITE(assimX(k)%x%fileID,REC=assimX(k)%x%rec+j) assimX(k)%x%outmean(:)
            ELSE
                ! switch to "outmedian"
              WRITE(assimX(k)%x%fileID,REC=assimX(k)%x%rec+j) assimX(k)%x%outquant(2,:)
            ENDIF
         ENDIF
        ENDDO
      ENDIF
    ENDDO
  END SUBROUTINE meanORmedian_to_ensemble
    
!>Initializes the information variable for assimilation
!---------------------------------------------------------
  SUBROUTINE initialize_assim_info(assimInfo)
    !INPUT ARGUMENT
    TYPE(assim_info_type), INTENT(INOUT) :: assimInfo !<information on assimilation simulation
  
    !Initialize various info variables
    assimInfo%nE      = 100       !number of ensemble members      (columns in ensemble matrices)
    assimInfo%nX      = 0         !number of state variables       (length of ensemble vector EnkfX)
    assimInfo%nA      = 0         !number of auxiliary variables   (length of ensemble vector EnkfA)
    assimInfo%nF      = 0         !number of forcing variables     (length of ensemble vector EnkfF)
    assimInfo%nObs    = 0         !number of observation variables (rows of ensemble vector EnkfObs(:,:))
    assimInfo%nD      = 0         !number of observations for next Enkf analysis (rows in EnkfD%x%x(:,:))
  !  assimInfo%nDT     = 0         !number of observation timesteps (colss of ensemble vector EnkfObs(:,:))
  !  assimInfo%nP      = 0         !number of parameters            (length of ensemble vector EnkfP)
    assimInfo%nloc    = 0         !number of localization matrices (length of vector EnkfLocCXY)
    assimInfo%ncoord  = 0         !number of spatial domains       (length of vector EnkfCoordinates)
    
    !LOGICAL flags, may be modified...
    assimInfo%FA      = .false.   !include auxil. in kalman filter     (general switch on/off)
    assimInfo%FP      = .false.   !include parameters in kalman filter (general switch on/off) 
    assimInfo%FF      = .false.   !include inputs in kalman filter     (general switch on/off)
    !EnkfInfo%XS      = .false.   !include X states in statistical output (general switch on/off)
    !assimInfo%EC      = .false.   !ECMWF forecasts (general switch on/off)
    assimInfo%meanout = .true.    !ensemble mean(.true.) or median (.false.) in output
    ALLOCATE(assimInfo%assim_flag(assimInfo%nCat+50)) !assimilate state variables (switch for categories and variables) (the size is unecessary large I think)

    !some Ensemble Kalman Filter parameters
    !assimInfo%moradkhani_delta = 0.95 ! coef.[0-1] to retain variance in parameter ensemble (Moradkhani et al, 2004), value typical around 0.95 !CP170615 not used
    !assimInfo%ensgen_minsigma = 1.e-5
    
    assimInfo%missing = -9999.
    
    !switch on/off read/write ensemble data to binary files
    assimInfo%useBinFilesX = 0 !.FALSE.  CP170505 these are integers
    assimInfo%useBinFilesFA = 0 !.FALSE.
    assimInfo%nBinFiles = 0
  
    !Output of statistical simulation results
    assimInfo%nstatout = 0

    !localization parameters
    assimInfo%xy_scalefac = 1000000.
    assimInfo%z_scalefac  = 100000.

    !Options for non-controlled variables
    assimInfo%collapseNonControlled = .false.

    !Options for State Ensemble Initialization
    assimInfo%initializeFromBinFiles = .false.
    
    !Target balance (ratio) between the R and CYY covariance matrices - used to adjust these with random numbers before inversion (to be implemented)
    assimInfo%covratio = -1. !initialize to negative, which means it will not be used.
    
    !Transformation settings/parameter
    assimInfo%transtat = 0
    assimInfo%traneps  = 1.e-6
    
    !Option for failure
    assimInfo%stop_at_failure = .FALSE.
 
  END SUBROUTINE initialize_assim_info

!>Allocate ensemble vectors needed for the data assimilation application
!----------------------------------------------------------------------------------------------  
  SUBROUTINE allocate_assim_ensemble_vectors(assimData,fid_0)
    TYPE(assim_data_type), INTENT(INOUT) :: assimData !<main assimilation variable containing all data
    INTEGER :: fid_0  !<base fileunit for bin files to be used
    !INTEGER :: nx,na,np,nf,nobs,ncoord,nloc,nobsDT
    INTEGER :: i
    !--------------------------------------------------------------------------------------------
    !X state variable ensemble vector
    IF(ALLOCATED(assimData%X))DEALLOCATE(assimData%X)
    IF(assimData%info%nx.GT.0)ALLOCATE(assimData%X(assimData%info%nx))
    
    !A auxiliary variable ensemble vector
    IF(ALLOCATED(assimData%A))DEALLOCATE(assimData%A)
    IF(assimData%info%na.GT.0)ALLOCATE(assimData%A(assimData%info%na))
    
    
    !Obs observation ensemble vector
    IF(ALLOCATED(assimData%Obs))DEALLOCATE(assimData%Obs)
    !  IF(assimData%info%nobs.GT.0)ALLOCATE(assimData%Obs(nobs,nobsDT))
    IF(assimData%info%nobs.GT.0)ALLOCATE(assimData%Obs(assimData%info%nobs))
    
    !F forcing variable ensemble vector
    IF(ALLOCATED(assimData%F))DEALLOCATE(assimData%F)
    IF(assimData%info%nf.GT.0)ALLOCATE(assimData%F(assimData%info%nf))
    
    !If binfiles are used
    assimData%info%nBinFiles = assimData%info%nX + assimData%info%nA + assimData%info%nF*2 + assimData%info%nObs + 3
    IF(ALLOCATED(fid_assim_bin))DEALLOCATE(fid_assim_bin)
    ALLOCATE(fid_assim_bin(assimData%info%nBinFiles))
    
    DO i=1,assimData%info%nBinFiles
      fid_assim_bin(i) = fid_0 + i 
    ENDDO
  END SUBROUTINE allocate_assim_ensemble_vectors

  !--------------------------------------------------------
  !>Allocate and initialize an assim_ensemble_type variable
  !--------------------------------------------------------
  SUBROUTINE allocate_assim_ensemble(assimVar, nens, nvar, varID, fileID, useBinFile, locID, coordID, xini, mini, maxi,  &
                                     allocateOutput,missing,assimilate,iniFromBin,transformation,lambda,epsilon,dxadd)
    !INPUT ARGUMENTS
    TYPE(assim_ensemble_type) :: assimVar !<ensemble data
    INTEGER :: nens      !<number of ensemble members
    INTEGER :: nvar      !<number of variables (model units, i.e. n:o subbasins in HYPE) 
    INTEGER, INTENT(IN) :: varID  !<to set record for binary file
    INTEGER :: fileID         !<file unit ID for direct access binary file I/O
    INTEGER :: locID
    INTEGER :: coordID  !<ID for coordinate system for this variable
    REAL    :: xini(nvar)     !<initial values
    REAL    :: mini, maxi     !max/min thresholds for truncation of unrealistic values
    LOGICAL :: allocateOutput !<flag for output variable allocation
    REAL    :: missing        !<initial output values
    INTEGER, INTENT(IN) :: useBinFile   !<flag for using bin-file
    LOGICAL, INTENT(IN) :: assimilate   !<flag for including variable in assimilation
    LOGICAL, INTENT(IN) :: iniFromBin   !<flag for initializing ensemble from bin-files
    INTEGER, INTENT(IN) :: transformation
    REAL, INTENT(IN) :: lambda
    REAL, INTENT(IN) :: epsilon
    LOGICAL, INTENT(IN) :: dxadd !<flag for allocate and initialize perturbation field (dx)

    !LOCAL
    INTEGER :: j,reclen
    CHARACTER(LEN=10) :: filename
    
    !ASSIGN AND ALLOCATE or open bin-files for saving ensembles (CP added bin-files CP161201)
    assimVar%nvar = nvar
    assimVar%nens = nens
    assimVar%minimum = mini
    assimVar%maximum = maxi
    assimVar%fileID = fileID
    assimVar%locID = locID
    assimVar%coordID = coordID
    IF(useBinFile==1)THEN
      !If one bin-file is used, write initial data to already open file
      assimVar%rec = (varID-1)*nens
      IF(dxadd) assimVar%rec = (varID-1)*nens*2
      IF(.NOT.iniFromBin)THEN
        DO j=1,nens
          WRITE(assimVar%fileID,REC=assimVar%rec+j) xini
        ENDDO
        !CP201028 added dx directly after x in bin-files        
        IF(dxadd)THEN   !for forcing data (dxadd=T) both x and dx is saved
          xini = 0.0
          DO j=1,nens
            WRITE(assimVar%fileID,REC=assimVar%rec+nens+j) xini    !for dx
          ENDDO
        ENDIF
      ENDIF
    ELSEIF(useBinFile==2)THEN
      !If several bin-files is used open file and write initial data
      INQUIRE(IOLENGTH=reclen) xini   !determine suitable record length here, because ifort och gfortran have different file storage unit (i.e. RECL)
      WRITE(filename,'(I6.6,a4)') assimVar%fileID,'.bin'
!      WRITE(filename,'(I5.5,a4)') assimVar%fileID,'.bin'
      OPEN(UNIT=assimVar%fileID,FILE=TRIM(filename),FORM='UNFORMATTED',ACCESS='DIRECT',RECL=reclen,ACTION='readwrite')
      IF(.NOT.iniFromBin)THEN
        DO j=1,nens
          WRITE(assimVar%fileID,REC=j) xini   !for x
        ENDDO
        !CP201028 added dx directly after x in bin-files
        IF(dxadd)THEN   !for forcing data (dxadd=T) both x and dx is saved in the same file
          xini = 0.0
          DO j=1,nens
            WRITE(assimVar%fileID,REC=nens+j) xini    !for dx
          ENDDO
        ENDIF
      ENDIF
      assimVar%rec = 0
    ELSE
      !If no bin-files, then allocate a matrix to hold the ensemble and initialize it.
      assimVar%rec = 0
      IF(ALLOCATED(assimVar%x))DEALLOCATE(assimVar%x)
      ALLOCATE(assimVar%x(nvar,nens))
      DO j=1,nens
        assimVar%x(:,j)=xini
      ENDDO
      !Allocate and initialize perturbation field if requested
      IF(dxadd)THEN
        IF(ALLOCATED(assimVar%dx))DEALLOCATE(assimVar%dx)
        ALLOCATE(assimVar%dx(nvar,nens))
        DO j=1,nens
          assimVar%dx(:,j)=0.0
        ENDDO
      ENDIF
    ENDIF
  
    IF(allocateOutput)THEN
      IF(ALLOCATED(assimVar%outmean))DEALLOCATE(assimVar%outmean)
      ALLOCATE(assimVar%outmean(nvar))
      assimVar%outmean=missing
      IF(ALLOCATED(assimVar%outquant))DEALLOCATE(assimVar%outquant)
      ALLOCATE(assimVar%outquant(3,nvar))
      assimVar%outquant=missing
      IF(ALLOCATED(assimVar%outmin))DEALLOCATE(assimVar%outmin)
      ALLOCATE(assimVar%outmin(nvar))
      assimVar%outmin=missing
      IF(ALLOCATED(assimVar%outmax))DEALLOCATE(assimVar%outmax)
      ALLOCATE(assimVar%outmax(nvar))
      assimVar%outmax=missing
      IF(ALLOCATED(assimVar%outsigma))DEALLOCATE(assimVar%outsigma)
      ALLOCATE(assimVar%outsigma(nvar))
      assimVar%outsigma=missing
    ENDIF
  
    assimVar%assimilate = assimilate
    
    !flag to indicate transformation type (0=none,1=log,2=yeo-johnsson,3=logit)
    assimVar%transform = transformation
    !transformation parameters
    assimVar%lambda = lambda
    assimVar%epsilon = epsilon
    
    !!Allocate and initialize perturbation field if requested  !CP201028 moved up, added dx directly after x in bin-files
    !IF(dxadd)THEN
    !  IF(ALLOCATED(assimVar%dx))DEALLOCATE(assimVar%dx)
    !  ALLOCATE(assimVar%dx(nvar,nens))
    !  DO j=1,nens
    !    assimVar%dx(:,j)=0.0
    !  ENDDO
    !ENDIF
    
  END SUBROUTINE allocate_assim_ensemble

!--------------------------------------------------------
!>Allocate and initialize a assim_interface_type variable
!--------------------------------------------------------
  SUBROUTINE allocate_assim_interface(assimVar,varName,varID,modID,nSubDim,subDimID)
    !INPUT ARGUMENTS
    TYPE(assim_interface_type)  :: assimVar !<interface data
    CHARACTER(LEN=*)     :: varName        !<character string for model variables (used for debugging, and possibly file names and outputs)
    INTEGER              :: varID          !<variable ID (id number used by interface for linking to model variables)  (in HYPE it can be an outvar index, or the order in which the state variables are considered by interface)
    INTEGER              :: modID          !<model ID,  link to the corresponding variables used for H(X)              (in HYPE: outvar index)
    INTEGER              :: nSubDim        !<number of sub-dimensions (if needed, for instance lateral sub-units, vertical layers, substances, etc, in HYPE for instance SLC, substances, landuse, or slc, etc)
    INTEGER              :: subDimID(:)    !<index in the sub-dimensions
    !ALLOCATE AND ASSIGN DATA
    assimVar%varName(1:(MIN(30,LEN_TRIM(varName)))) = TRIM(varName)  
    assimVar%varID   = varID
    assimVar%modID   = modID
    assimVar%nSubDim = nSubDim
    IF(ALLOCATED(assimVar%subDimID))DEALLOCATE(assimVar%subDimID)
    IF(nSubDim.GT.0)THEN
      ALLOCATE(assimVar%subDimID(nSubDim))
      assimVar%subDimID(1:nSubDim)=subDimID(1:nSubDim)
    ENDIF
  END SUBROUTINE allocate_assim_interface

!---------------------------------------------------------
!>Allocate and initialize an enkf_generation_type variable
!---------------------------------------------------------
  SUBROUTINE allocate_assim_generation(assimVar,nvar,ensgen,fixsigma,semimeta,restmeta,minsigma,lscale,gridsize,corrtype,xcoord,ycoord,tau)
    !INPUT ARGUMENTS
    TYPE(assim_generation_type) :: assimVar !<generation_data variable
    INTEGER                     :: nvar     !<number of variables, ie n:o "model units" (for instance, number of sub-basins)
    INTEGER                     :: ensgen   !<type of ensemble generation        (0 none, 1 unrestricted, 2 [min,+inf], 3 [-inf,max], 4 restricted [min,max])   
    REAL                        :: fixsigma !<fixed standard deviation           (ensgen=1)
    REAL                        :: semimeta !<relative sigma for semi-restricted (ensgen=2,3,4, following Turner et al 2008)
    REAL                        :: restmeta !<relative sigma for restricted      (ensgen=2,3,4)
    REAL                        :: minsigma !<minimum sigma                      (ensgen=2,3,4)
    REAL                        :: lscale   !<correlation length for spatially correlated perturbation
    REAL                        :: gridsize !<2D gridsize for spatially correlated perturbation generation
    INTEGER                     :: corrtype !<spatial correlation FUNCTION option
    REAL                        :: xcoord(:)!<x coordinate, for spatially correlated perturbations
    REAL                        :: ycoord(:)!<y coordinate, for spatially correlated perturbations
    REAL                        :: tau      !<perturbation memory coefficient
    
    !ASSIGN
    assimVar%nvar = nvar
    assimVar%ensgen = ensgen
    assimVar%fixsigma = fixsigma
    assimVar%semimeta = semimeta
    assimVar%restmeta =restmeta
    assimVar%minsigma = minsigma
    
    !ALLOCATE
    !sigma, standard deviation (input to ensemble generation)
    IF(ALLOCATED(assimVar%sigma))DEALLOCATE(assimVar%sigma)
    ALLOCATE(assimVar%sigma(nvar))
    assimVar%sigma = 0.0
    IF(ensgen.EQ.1)assimVar%sigma=fixsigma
    
    !mean, mean value  (input to ensemble generation)
    IF(ALLOCATED(assimVar%mean))DEALLOCATE(assimVar%mean)
    ALLOCATE(assimVar%mean(nvar))
    assimVar%mean = 0.0
    
    !data structure for spatially correlated random perturbations
    IF(lscale.GT.0. .AND. gridsize.GT.0. .AND. corrtype.GT.0 .AND. corrtype.LT.4)THEN
      !DO randomxy
      assimVar%dorandxy = .TRUE.
      !initialize randxy data structure
      CALL init_randxy_data(assimVar%myrandxy_data,nvar,xcoord,ycoord,lscale,gridsize,corrtype)
    ELSE
      assimVar%dorandxy = .FALSE.
    ENDIF
    
    !perturbation memory coefficient
    assimVar%tau = tau
    
  END SUBROUTINE allocate_assim_generation

!-----------------------------------------------------------------
!Allocate and initialize STATE, auxiliary, forcing and obseervation ensemble variables
!-----------------------------------------------------------------
!>Allocate and initialize auxiliary ensemble variables; 0-dimensional
  SUBROUTINE allocate_auxiliary_ensemble(assimVar,nens,nvar,varName,xini,varID,recID,locID,coordID,fileID,useBinFile,minimum,maximum,assimilate,missing,transformation,lambda,epsilon)
    !INPUT
    TYPE(assim_state_ensemble_type)  :: assimVar
    INTEGER :: nens,nvar,locID,coordID,fileID,dimID(1),ndim
    INTEGER,INTENT(IN) :: varID   !<outvar-variable index
    INTEGER,INTENT(IN) :: recID   !<record of variable in bin-file
    REAL    :: minimum,maximum
    REAL    :: xini(nvar)
    REAL    :: missing
    CHARACTER(LEN=*) :: varName
    INTEGER, INTENT(IN) :: useBinFile   !<flag for using bin-file
    LOGICAL, INTENT(IN) :: assimilate   !<flag for including variable in assimilation
    INTEGER, INTENT(IN) :: transformation
    REAL, INTENT(IN) :: lambda
    REAL, INTENT(IN) :: epsilon
    
    ndim=0
    dimID(1)=0
    !ALLOCATE and initialize the ensemble matrix data or initialize bin-file
    CALL allocate_assim_ensemble(assimVar%x,nens,nvar,recID,fileID,useBinFile,locID,coordID,xini,minimum,maximum,.true.,missing,assimilate,.false.,transformation,lambda,epsilon,.FALSE.)
    !ALLOCATE the interface data
    CALL allocate_assim_interface(assimVar%info,varName,varID,-1,ndim,dimID)
    !update varID
    !varID = varID+1 !not for aux this is o_rout etc.
  END SUBROUTINE allocate_auxiliary_ensemble
  
!>Allocate and initialize state ensemble variables; 0-dimensional
  SUBROUTINE allocate_0dim_state_ensemble(assimVar,nens,nvar,varName,xini,varID,locID,coordID,fileID,useBinFile,minimum,maximum,assimilate,iniFromBin,missing,transformation,lambda,epsilon)
    !INPUT
    TYPE(assim_state_ensemble_type)  :: assimVar
    INTEGER :: nens,nvar,varID,locID,coordID,fileID,dimID(1),ndim
    REAL    :: minimum,maximum
    REAL    :: xini(nvar)
    REAL    :: missing
    CHARACTER(LEN=*) :: varName
    INTEGER, INTENT(IN) :: useBinFile   !<flag for using bin-file
    LOGICAL, INTENT(IN) :: assimilate   !<flag for including variable in assimilation
    LOGICAL, INTENT(IN) :: iniFromBin   !<flag for initializing ensemble from bin-files
    INTEGER, INTENT(IN) :: transformation
    REAL, INTENT(IN) :: lambda
    REAL, INTENT(IN) :: epsilon
  
    ndim=0
    dimID(1)=0
    !ALLOCATE and initialize the ensemble matrix data or initialize bin-file
    CALL allocate_assim_ensemble(assimVar%x,nens,nvar,varID,fileID,useBinFile,locID,coordID,xini,minimum,maximum,.true.,missing,assimilate,iniFromBin,transformation,lambda,epsilon,.FALSE.)
    !ALLOCATE the interface data
    CALL allocate_assim_interface(assimVar%info,varName,varID,-1,ndim,dimID)
    !update varID
    varID = varID+1
  END SUBROUTINE allocate_0dim_state_ensemble
  
!>Allocate and initialize state ensemble variables; 1-dimensional
  SUBROUTINE allocate_1dim_state_ensemble(assimVar,nens,nvar,varName,xini,varID,locID,coordID,fileID,useBinFile,minimum,maximum,n1,assimilate,iniFromBin,missing,transformation,lambda,epsilon)
    !INPUT
    TYPE(assim_state_ensemble_type)  :: assimVar(:)
    INTEGER :: nens,nvar,varID,locID,coordID,fileID(:),n1,i,dimID(1),ndim
    REAL    :: minimum,maximum
    REAL    :: xini(n1,nvar)
    REAL    :: missing
    CHARACTER(LEN=*) :: varName
    INTEGER, INTENT(IN) :: useBinFile   !<flag for using bin-file
    LOGICAL, INTENT(IN) :: assimilate   !<flag for including variable in assimilation
    LOGICAL, INTENT(IN) :: iniFromBin   !<flag for initializing ensemble from bin-files
    INTEGER, INTENT(IN) :: transformation
    REAL, INTENT(IN) :: lambda
    REAL, INTENT(IN) :: epsilon

    ndim=1
    DO i=1,n1
      dimID(1)=i
      !ALLOCATE and initialize the ensemble matrix data or initialize bin-file
      CALL allocate_assim_ensemble(assimVar(varID)%x,nens,nvar,varID,fileID(varID),useBinFile,locID,coordID,xini(i,:),minimum,maximum,.true.,missing,assimilate,iniFromBin,transformation,lambda,epsilon,.FALSE.)
      !ALLOCATE the interface data
      CALL allocate_assim_interface(assimVar(varID)%info,varName,varID,-1,ndim,dimID)
      !update varID
      varID = varID+1
    ENDDO
  END SUBROUTINE allocate_1dim_state_ensemble
  
!>Allocate and initialize state ensemble variables; 2-dimensional
  SUBROUTINE allocate_2dim_state_ensemble(assimVar,nens,nvar,varName,xini,varID,locID,coordID,fileID,useBinfile,minimum,maximum,n1,n2,assimilate,iniFromBin,missing,transformation,lambda,epsilon)
    !INPUT
    TYPE(assim_state_ensemble_type)  :: assimVar(:)
    INTEGER :: nens,nvar,varID,locID,coordID,fileID(:),n1,n2,i,j,ndim,dimID(2)
    REAL    :: minimum,maximum
    REAL    :: xini(n2,n1,nvar)
    REAL    :: missing
    CHARACTER(LEN=*) :: varName
    INTEGER, INTENT(IN) :: useBinFile   !<flag for using bin-file
    LOGICAL, INTENT(IN) :: assimilate   !<flag for including variable in assimilation
    LOGICAL, INTENT(IN) :: iniFromBin   !<flag for initializing ensemble from bin-files
    INTEGER, INTENT(IN) :: transformation
    REAL, INTENT(IN) :: lambda
    REAL, INTENT(IN) :: epsilon
    
    ndim=1
    DO j=1,n2
      dimID(1)=j
    DO i=1,n1
      dimID(2)=i
      !ALLOCATE and initialize the ensemble matrix data or initialize bin-file
      CALL allocate_assim_ensemble(assimVar(varID)%x,nens,nvar,varID,fileID(varID),useBinFile,locID,coordID,xini(j,i,:),minimum,maximum,.true.,missing,assimilate,iniFromBin,transformation,lambda,epsilon,.FALSE.)
      !ALLOCATE the interface data
      CALL allocate_assim_interface(assimVar(varID)%info,varName,varID,-1,ndim,dimID)
      !update varID
      varID = varID+1
    ENDDO
    ENDDO
  END SUBROUTINE allocate_2dim_state_ensemble
  
!>Allocate and initialize state ensemble variables; 3-dimensional
  SUBROUTINE allocate_3dim_state_ensemble(assimVar,nens,nvar,varName,xini,varID,locID,coordID,fileID,useBinFile,minimum,maximum,n1,n2,n3,assimilate,iniFromBin,missing,transformation,lambda,epsilon)
    !INPUT
    TYPE(assim_state_ensemble_type)  :: assimVar(:)
    INTEGER :: nens,nvar,varID,locID,coordID,fileID(:),n1,n2,n3,i,j,k,ndim,dimID(3)
    REAL    :: minimum,maximum
    REAL    :: xini(n3,n2,n1,nvar)
    REAL    :: missing
    CHARACTER(LEN=*) :: varName
    INTEGER, INTENT(IN) :: useBinFile   !<flag for using bin-files
    LOGICAL, INTENT(IN) :: assimilate   !<flag for including variable in assimilation
    LOGICAL, INTENT(IN) :: iniFromBin   !<flag for initializing ensemble from bin-files
    INTEGER, INTENT(IN) :: transformation
    REAL, INTENT(IN) :: lambda
    REAL, INTENT(IN) :: epsilon
    ndim=3
    DO k=1,n3
      dimID(1)=k
    DO j=1,n2
      dimID(2)=j
    DO i=1,n1
      dimID(3)=i
      !ALLOCATE and initialize the ensemble matrix data or initialize bin-file
      CALL allocate_assim_ensemble(assimVar(varID)%x,nens,nvar,varID,fileID(varID),useBinFile,locID,coordID,xini(k,j,i,:),minimum,maximum,.true.,missing,assimilate,iniFromBin,transformation,lambda,epsilon,.FALSE.)
      !ALLOCATE the interface data
      CALL allocate_assim_interface(assimVar(varID)%info,varName,varID,-1,ndim,dimID)
      !update varID
      varID = varID+1
    ENDDO
    ENDDO
    ENDDO
  END SUBROUTINE allocate_3dim_state_ensemble
  
!>Allocate and initialize forcing ensemble variables
  SUBROUTINE allocate_assim_forcing_ensemble(assimVar,nens,nvar,varName,xini,varID,locID,coordID,fileID,minimum,maximum, &
    ensgen,sigma,semimeta,restmeta,minsigma,lscale,gridsize,corrtype,xcoord,ycoord,useBinFile,missing,transformation,lambda,epsilon,tau)
    
    !INPUT
    TYPE(assim_input_ensemble_type) :: assimVar
    INTEGER :: nens,nvar,locID,coordID,fileID
    INTEGER,INTENT(IN) :: varID   !<counter for variables in ensembles (is not all variables varID for F-variables but hardkoded)
    REAL    :: minimum,maximum
    REAL    :: xini(nvar)
    CHARACTER(LEN=*) :: varName
    REAL    :: lscale,gridsize,xcoord(:),ycoord(:)
    INTEGER :: corrtype,ensgen
    REAL    :: sigma, semimeta, restmeta, minsigma
    REAL    :: missing
    INTEGER, INTENT(IN) :: useBinFile   !<flag for using bin-file
    INTEGER, INTENT(IN) :: transformation
    REAL, INTENT(IN) :: lambda
    REAL, INTENT(IN) :: epsilon
    REAL, INTENT(IN) :: tau

    !LOCAL VARIABLES
    INTEGER :: nDim,dimID(1),modID
  
    nDim=0;dimID(1)=0
    
    !ALLOCATE and initialize the ensemble matrix data (NOT initialize bin-file for now)
    CALL allocate_assim_ensemble(assimVar%x,nens,nvar,varID,fileID,useBinFile,locID,coordID,xini,minimum,maximum,.true.,missing,.true.,.false.,transformation,lambda,epsilon,.TRUE.)

    !ALLOCATE the interface data
    nDim = 0 ; dimID(1) = 0 ; modID = -9999
    CALL allocate_assim_interface(assimVar%info,varName,varID,modID,nDim,dimID)

    !ALLOCATE and initialize the ensemble generation data
    CALL allocate_assim_generation(assimVar%gen,nvar,ensgen,sigma,semimeta,restmeta,minsigma,lscale,gridsize,corrtype,xcoord,ycoord,tau)
  END SUBROUTINE allocate_assim_forcing_ensemble

!>Allocate and initialize observation ensemble variables
  SUBROUTINE allocate_assim_observation_ensemble(assimVar,nens,nvar,varName,xini,obsID,modID,coordID,fileID,minimum,maximum, &
    ensgen,sigma,semimeta,restmeta,minsigma,lscale,gridsize,corrtype,xcoord,ycoord,missing,transformation,lambda,epsilon)
    !INPUT
    TYPE(assim_input_ensemble_type) :: assimVar
    INTEGER :: nens,nvar
    REAL    :: xini(nvar)
    CHARACTER(LEN=*) :: varName
    INTEGER :: obsid, modID
    INTEGER coordID,fileID
    REAL    :: minimum,maximum
    REAL    :: lscale,gridsize
    INTEGER :: corrtype,ensgen
    REAL    :: sigma, semimeta, restmeta, minsigma
    REAL    :: xcoord(:),ycoord(:)
    REAL    :: missing
    INTEGER, INTENT(IN) :: transformation
    REAL, INTENT(IN) :: lambda
    REAL, INTENT(IN) :: epsilon
    !local
    INTEGER :: nDim,dimID(1)

    !ALLOCATE and initialize the ensemble matrix data (NOT initialize bin-file for now)
    CALL allocate_assim_ensemble(assimVar%x,nens,nvar,obsid,fileID,0,0,coordID,xini,minimum,maximum,.true.,missing,.true.,.false.,transformation,lambda,epsilon,.FALSE.)
    !ALLOCATE the interface data
    nDim = 0 ; dimID(1) = 0
    CALL allocate_assim_interface(assimVar%info,varName,obsID,modID,nDim,dimID)
    !ALLOCATE and initialize the ensemble generation data
    CALL allocate_assim_generation(assimVar%gen,nvar,ensgen,sigma,semimeta,restmeta,minsigma,lscale,gridsize,corrtype,xcoord,ycoord,0.)

  END SUBROUTINE allocate_assim_observation_ensemble

!>Truncates an ensemble matrix to minimum and maximum allowed values.
!----------------------------------------------------------------------------------------------
SUBROUTINE assim_checkminmax(nx,ne,ensemble,minval,maxval,missing)
  INTEGER, INTENT(IN) :: nx
  INTEGER, INTENT(IN) :: ne
  REAL, INTENT(INOUT) :: ensemble(nx,ne)
  REAL, INTENT(IN)    :: minval, maxval
  REAL, INTENT(IN)    :: missing
  INTEGER :: i,j
  DO i=1,ne
    DO j=1,nx
      !make sure missing values stay missing
      IF(ensemble(j,i).NE.missing)THEN
        ensemble(j,i)=AMAX1(minval,AMIN1(maxval,ensemble(j,i)))
      ELSE
        ensemble(j,i)=missing
      ENDIF
    ENDDO
  ENDDO
END SUBROUTINE assim_checkminmax
  
!>General routine for Ensemble generation (forcing and observation data).
!>
!> The ensemble generation is made by adding random numbers to the input data.
!>
!> a) The basic assumption is that the random perturbations are gaussian with zero mean and standard deviation sigma.
!> 
!> b) Based on Turner et al (2008), sigma is a constant value for unrestricted variables, 
!>    and a relative value for variables restricted by a minimum or a maximum or both:
!>      pertubation_unrestricted      = G(0,sigma)
!>      pertubation_restricted,min    = G(0,semi.meta * (X-minX))
!>      pertubation_restricted,max    = G(0,semi.meta * (maxX-X))
!>      pertubation_restricted,minmax = G(0,rest.meta * (maxX-X)/(maxX-midX)) if X >  midX  
!>      pertubation_restricted,minmax = G(0,rest.meta * (X-minX)/(midX-minX)) if X <= midX  
!>
!>  (Turner et al(2008) also suggested that in order to get unbiased input ensembles, 
!>    we need to consider two types of perturbations - systematic and random:
!>    Thus, the input x at time k for ensemble member j, x_kj = x_k0+eata_kj+chi_j,
!>    where eata_kj is regenerated every time step and chi_j is generated only once.
!>    Then, eata and chi can be generated with three different types of restrictions on variables.
!>
!>    However, we never implemented this part of Turner - we assume systematic error chi_j = 0.)
!>
!>  c) We now also take into account spatial correlation in the data, by generating 
!>     spatially correlated random data using a FFT-based method (usually only on forcing)
!>
!>  d) Instead of the systematic error suggested by Turner et al (2008); see section in brackets under b)
!>     we introduce a memory in the perturbation (only for forcing):
!>
!>      pert(t) = pert(t-1) * TAU + pert * (1-TAU), where pert is generated with existing methods
!>
!>      for restricted and semi-restricted variables, we probably need to propagate relative
!>      perturbations instead of absolute perturbations.
!>    
!----------------------------------------------------------------------------------------------
SUBROUTINE generate_input_ensemble(n,assimVar,missing,usedx)
  INTEGER n
  TYPE(assim_input_ensemble_type) :: assimVar(n)
  INTEGER i,j
  REAL midvalue
  REAL,ALLOCATABLE :: localx(:,:),localdx(:,:), lastdx(:,:)
  REAL missing
  LOGICAL usedx 
    
  !loop over ensemble vector members
  DO j=1,n

    !get data to a local array for manipulation
    ALLOCATE(localx(assimVar(j)%x%nvar,assimVar(j)%x%nens))
    
    IF(usedx) ALLOCATE(localdx(assimVar(j)%x%nvar,assimVar(j)%x%nens))     
    IF(usedx) ALLOCATE(lastdx(assimVar(j)%x%nvar,assimVar(j)%x%nens))     

    !before ensemble generation, check that number of ensemble members is greater than 1   
    IF(assimVar(j)%x%nens.gt.1)THEN 

      !a) determine standard deviation (sigma) in each model unit (rows in ensemble matrix)
        
      !loop over number of model units (subbasins in HYPE)
      DO i=1,assimVar(j)%x%nvar
        !Check missing values
        IF(assimVar(j)%gen%mean(i).NE.missing)THEN
          SELECT CASE(assimVar(j)%gen%ensgen)
            CASE(4) ! restricted (Turner et al, 2008)
              midvalue = 0.5 * (assimVar(j)%x%maximum+assimVar(j)%x%minimum)
              IF(assimVar(j)%gen%mean(i).gt.midvalue)THEN
                assimVar(j)%gen%sigma(i) = assimVar(j)%gen%restmeta *(assimVar(j)%x%maximum-assimVar(j)%gen%mean(i)) / (assimVar(j)%x%maximum-midvalue);
              ELSE
                assimVar(j)%gen%sigma(i) = assimVar(j)%gen%restmeta*(assimVar(j)%gen%mean(i)-assimVar(j)%x%minimum) / (midvalue - assimVar(j)%x%minimum);
              ENDIF
            CASE(3) ! semirestricted with max (Turner et al, 2008)
              assimVar(j)%gen%sigma(i) = AMAX1(0.0,assimVar(j)%x%maximum-assimVar(j)%gen%mean(i)) * assimVar(j)%gen%semimeta
            CASE(2) ! semirestricted with min (Turner et al, 2008)
              assimVar(j)%gen%sigma(i) = AMAX1(0.0,assimVar(j)%gen%mean(i)-assimVar(j)%x%minimum) * assimVar(j)%gen%semimeta
            CASE DEFAULT ! 0 or 1, ie. unrestricted (Turner et al, 2008)
            assimVar(j)%gen%sigma(i) = assimVar(j)%gen%fixsigma
          END SELECT
          !Make sure sigma is not smaller than the variable specific minimum sigma 
          assimVar(j)%gen%sigma(i) = AMAX1(assimVar(j)%gen%minsigma,assimVar(j)%gen%sigma(i))
          !Also make sure sigma is not smaller than the global (hardcoded) minimum sigma
          assimVar(j)%gen%sigma(i) = AMAX1(assim_minsigma,assimVar(j)%gen%sigma(i))
        ENDIF
      ENDDO
          
      ! b) generate random values with the assigned sigma
      IF(assimVar(j)%gen%dorandxy)THEN
        !Spatially correlated ensemble matrix for a specific variable type
        IF(usedx)THEN
          CALL get_spatially_correlated_random_data2(assimVar(j)%x%nvar,assimVar(j)%x%nens,assimVar(j)%gen,localdx,usedx)
        ELSE
          CALL get_spatially_correlated_random_data2(assimVar(j)%x%nvar,assimVar(j)%x%nens,assimVar(j)%gen,localx,usedx)
        ENDIF  
      ELSE
        ! Non-correlated observation data
        ! loop over records in the observation matrix (irrespective of variable type)
        DO i=1,assimVar(j)%x%nvar
          IF(usedx)THEN
            ! generate ensemble for each record independently, no correlation
            IF(assimVar(j)%gen%mean(i).NE.missing)THEN
              CALL get_random_vector_gaussian(assimVar(j)%x%nens,0.,assimVar(j)%gen%sigma(i),localdx(i,:)) ! DG20170427 Changed to replace use of MKL dependent code.
            ELSE
              localdx(i,:) = missing              
              localx(i,:)  = missing
            ENDIF            
          ELSE
            ! generate ensemble for each record independently, no correlation
            IF(assimVar(j)%gen%mean(i).NE.missing)THEN
              CALL get_random_vector_gaussian(assimVar(j)%x%nens,assimVar(j)%gen%mean(i),assimVar(j)%gen%sigma(i),localx(i,:)) ! DG20170427 Changed to replace use of MKL dependent code.
            ELSE
              localx(i,:)=missing
            ENDIF
          ENDIF
        ENDDO
      ENDIF
      ! propagate perturbation from previous timesteps if requested by usedx
      IF(usedx)THEN
        !CP201028 added subroutine to collect assimVar(j)%x%dx from file (if saved in bin file)
        CALL assim_get_ensemble_data_dx(assimVar(j)%x%nvar,assimVar(j)%x%nens,assimVar(j)%x,lastdx)
        DO i=1,assimVar(j)%x%nvar
          localdx(i,:) = lastdx(i,:) * assimVar(j)%gen%tau + localdx(i,:) * (1. - assimVar(j)%gen%tau)
          localx(i,:)  = assimVar(j)%gen%mean(i) + localdx(i,:)
        ENDDO
      ENDIF
    ELSE
     ! in case someone runs with 1 ensemble member, we just use the mean, and skip all the ensemble generation
      DO i=1,assimVar(j)%x%nvar
        localx(i,1)=assimVar(j)%gen%mean(i)
        IF(usedx)localdx(i,1) = 0.0
      ENDDO    
    ENDIF
    
    ! save manipulated data from local array (to matrix or bin-file), also truncate all values outside the min/max range to the limits
    IF(usedx)THEN
      CALL assim_set_ensemble_data_dx(assimVar(j)%x%nvar,assimVar(j)%x%nens,assimVar(j)%x,localx,localdx,.true.,missing) !checkminmax true here !,.false.
    ELSE
      CALL assim_set_ensemble_data(assimVar(j)%x%nvar,assimVar(j)%x%nens,assimVar(j)%x,localx,.true.,missing) !checkminmax true here !,.false.      
    ENDIF
    IF(ALLOCATED(localx)) DEALLOCATE(localx)
    IF(ALLOCATED(localdx)) DEALLOCATE(localdx)
    IF(ALLOCATED(lastdx)) DEALLOCATE(lastdx)
  ENDDO
  
END SUBROUTINE generate_input_ensemble

!> Function for spatially correlated random variable generation
SUBROUTINE get_spatially_correlated_random_data2(n,nens,assimG,X,usedx)
  
  !Argument declarations
  INTEGER, INTENT(IN) :: n      !<number of element (nsub usually)
  INTEGER, INTENT(IN) :: nens   !<number of ensembles
  TYPE(assim_generation_type),INTENT(INOUT) :: assimG !<information on generation of input data
  REAL,INTENT(OUT) :: X(n,nens) !<generated random matrix
  LOGICAL :: usedx
  
  !Local variables
  INTEGER :: i,iens   !loop-variables
     
  !>\b Algorithm
  !> Loop over ensemble members:
   DO iens=1,nens
     
     !>\li generate random numbers (with mean 0, standard deviation 1 and correlation length L) for all subbasins' location
     CALL resample_randxy_data(assimG%myrandxy_data)

     !>\li scale perturbations with the required sigma (standard deviation) and apply to the mean (if not perturbation is requested by USEDX
     IF(usedx)THEN
       DO i=1,n
         X(i,iens) = assimG%myrandxy_data%pert_xy(i) * assimG%sigma(i)
       ENDDO
     ELSE
       DO i=1,n
         X(i,iens) = assimG%myrandxy_data%pert_xy(i) * assimG%sigma(i)  + assimG%mean(i)
       ENDDO
     ENDIF
   ENDDO
   
END SUBROUTINE get_spatially_correlated_random_data2

!>\brief Get an array of random gaussian values with specified mean and sigma
! USE THE RGAUSS() FROM THE RANDOM_ROUTINES MODULE INSTEAD OF MKL library, DG 2017-04-27
SUBROUTINE get_random_vector_gaussian(n,a,sigma,r)
  REAL, INTENT(OUT) :: r(:)
  REAL, INTENT(IN) :: a,sigma
  INTEGER, INTENT(IN) :: n
  INTEGER i
  !loop over vector and generate random numbers
  DO i=1,n
    ! random number N[0,1], scaled to the requested sigma, and applied to the mean
    r(i)=rgauss()*sigma+a
  ENDDO
END SUBROUTINE get_random_vector_gaussian

! -----------------------------------------------------------------------------------
! MATRIX OPERATIONS
! Method MATMUL (intrinsic function) /David 2017-04-27
! -----------------------------------------------------------------------------------
!>\brief Subroutine for matrix multiplication.
SUBROUTINE matrixmatrixmultiply(mat1,mat2,matout)
  REAL,INTENT(IN) ::  mat1(:,:),mat2(:,:)
  REAL,INTENT(OUT) :: matout(:,:)
  ! make sure matout is 0
  matout(:,:)=0.0
  ! CALL MATMUL
  matout = matmul(mat1,mat2)
END SUBROUTINE matrixmatrixmultiply

! -----------------------------------------------------------------------------------
! CHOLESKY SOLUTION
! Methods from Numerical Recipes in Fortran 90, 2nd edition, Press et al, 1996. 
! Adopted by David 2017-04-27
! -----------------------------------------------------------------------------------

!> Cholesky decomposition (from Numerical Recipes, adopted by David)
SUBROUTINE choldc(a,p,n,failure)
  !USE nrtype; USE nrutil, ONLY : assert_eq,nrerror
  IMPLICIT NONE
  REAL, DIMENSION(:,:), INTENT(INOUT) :: a
  REAL, DIMENSION(:), INTENT(OUT) :: p
  LOGICAL, INTENT(INOUT) :: failure
  !Given an N × N positive-definite symmetric matrix a, this routine constructs its Cholesky
  !decomposition, A = L · LT . On input, only the upper triangle of a need be given; it is
  !not modified. The Cholesky factor L is returned in the lower triangle of a, except for its
  !diagonal elements, which are returned in p, a vector of length N.
  INTEGER :: i,n
  REAL :: summ
  REAL :: helparr(n,1),helparr3(n,n)
  !n=assert_eq(size(a,1),size(a,2),size(p),’choldc’)
  p = 0.
  do i=1,n
    summ=a(i,i)-dot_product(a(i,1:i-1),a(i,1:i-1))
    IF(summ <= 0.0)THEN
      WRITE(6,*)'WARNING: choldc failed.'    !CP201209 handle error part 1 !DG200125 yes I agree, we could: 1) write error message to the log, and 2) continue simulation without ENKF analysis on this time step OR 3) stop simulation (it could be an option in assim_info)
      failure = .TRUE.
      RETURN
    ENDIF      
    p(i)=sqrt(summ)
    IF(i<n)THEN
      helparr(1:i-1,1) = a(i,1:i-1)
      CALL matrixmatrixmultiply(a(i+1:n,1:i-1),helparr(1:i-1,1:1),helparr3(i+1:n,1:1))
      a(i+1:n,i)=(a(i,i+1:n)-helparr3(i+1:n,1))/p(i)
!    a(i+1:n,i)=(a(i,i+1:n)-matmul(a(i+1:n,1:i-1),a(i,1:i-1)))/p(i) !do not work for gfortran -O2 on elin5
    ENDIF
  end do
END SUBROUTINE choldc

!> Cholesky solver (from Numerical Recipes, adopted by David)
SUBROUTINE cholsl(a,p,b,x,n)
  !USE nrtype; USE nrutil, ONLY : assert_eq
  IMPLICIT NONE
  REAL, DIMENSION(:,:), INTENT(IN) :: a
  REAL, DIMENSION(:), INTENT(IN) :: p,b
  REAL, DIMENSION(:), INTENT(INOUT) :: x
  !Solves the set of N linear equations A · x = b, where a is a positive-definite symmetric
  !matrix. a (N × N) and p (of length N) are input as the output of the routine choldc.
  !Only the lower triangle of a is accessed. b is the input right-hand-side vector, of length N.
  !The solution vector, also of length N, is returned in x. a and p are not modified and can be
  !left in place for successive calls with different right-hand sides b. b is not modified unless
  !you identify b and x in the calling sequence, which is allowed.
  INTEGER :: i,n
  !n=assert_eq((/size(a,1),size(a,2),size(p),size(b),size(x)/),’cholsl’)
  do i=1,n !Solve L · y = b, storing y in x.
    x(i)=(b(i)-dot_product(a(i,1:i-1),x(1:i-1)))/p(i)
  enddo
  do i=n,1,-1 !Solve LT · x = y.
    x(i)=(x(i)-dot_product(a(i+1:n,i),x(i+1:n)))/p(i)
  enddo
END SUBROUTINE cholsl

!> Cholesky solution
!!
! new routine called from enkf_analysis_prepare, written by David 2017-04-27
SUBROUTINE choleskysolution(ny,ne,P,Y,M,failure)
  !arguments
  INTEGER, INTENT(IN)  :: ny,ne
  REAL,    INTENT(INOUT)  :: P(ny,ny)
  REAL,    INTENT(IN)     :: Y(ny,ne)
  REAL,    INTENT(OUT) :: M(ny,ne)
  LOGICAL, INTENT(INOUT) :: failure
  !local variables
  REAL                 :: Ldiag(ny)
  integer              :: i 
  !This routine uses Cholesky factorization to solve the set of linear eqations P*M=Y => M=inv(P)*Y
  !The routines for cholesky factorization and solving above are taken from Numerical Recipes for Fortran 90.
  !The result M, is returned to enkf_analysis_prepare.
  !By David, 2017-04-27
  
  ! 1) Cholesky factorization, P = L · LT
  CALL choldc(P,Ldiag,ny,failure)     !Cholesky factors are now in lower triangle of P, except the diagonal elements which are stored in Ldiag
  IF(failure)THEN
    M = 0.
    RETURN
  ENDIF
  
  ! 2) Cholesky solution, solving P · M = Y => M = inv(P) · Y, using P and Ldiag from previous steps
  DO i=1,ne
    ! Since Y and M are multi-column ensemble matrices, we apply the solution for each ensemble member in a loop
    CALL  cholsl(P,Ldiag,Y(:,i),M(:,i),ny)
  ENDDO
  
END SUBROUTINE choleskysolution


! ---------------------------------------------------------------------------------------------
! enkf_analysis_prepare
! enkf_analysis_apply
! enkf_analysis_main
!
! Ensemble Kalman filter equations (Evensen) following (a) Mandel, J. Efficient implementation 
! of the ensemble kalman filter, and (b) DeChant, C. Examining the effectiveness and 
! robustness of sequential data assimilation methods for quantification of uncertainty
! in hydrological forecasting. Localization following Magnusson, Gustafsson et al (2014)
!
! Part I: Innovations (Y=D-HX), and inversion of variances M=1/(var(HX) + R) which is later
!         used in the update equation: X_analysis = X_forecast + cov(X,HX)/(var(HX)+R) * (D-HX)
!
!Author: D.Gustafsson, SMHI/KTH.
!-----------------------------------------------------------------------------------------------
!The implementation is divided into two steps according to this pseudo-code (enkf_analysis_main)
!  CALL enkf_analysis_main()
!      
!   1. CALL enkf_analysis_prepare()
!        a. calculation of innovation, Y = D-HX
!        b. calculation and localization of the covariance of predicted observations Cyy = cov(HX) = locCyy * cov(HX)
!        c. combine cov(HX) with observation error variance R into P = (cov(HX)+R)
!        d. derive intermediate update matrix M, by inversion of variance matrice sum  M = P^(-1)Y
!
!   2. DO i=1,num_model_states
!        CALL enkf_analysis_apply()
!             Here in Part 2, M is multiplied with the covariance Cxy = Cov(X,HX) in the final EnKF update equation:
!      ENDDO
! 
! Terminology, EnKF basics from Mandel and DeChant
! ---------------------------------------------------------------------------------------
! Variable   Mandel        Dechant     Description
! --------   ------------  ----------  --------------------------------------------------
! N          N              Nens        ensemble size, n:o of ensemble members
! NX         n              -           state vector length, n:o state variables
! ND         m              
! X          X              X           model state variables
! A          A              e           ensemble deviation (from ensemble mean, A=X-E(X))
! D          D              y+eps       observations (including observation error eps)
! HX         HX             y'          predicted observations
! -          h(x)           h(x)        observation operator y'=h(x) translating model states to observation space
! HA         HA             -           HX ensemble deviation from ensemble mean (HA=HX-E(HX))
! Y          Y=D-HX         y+eps-y'    innovation, deviation between observations and predicted obserations
! -          C              P           model state error covariance, C = AA^T (Mandel) = ee^T = P (DeChant)
! R          R              R           observation error variance (theoretic or sample, assumed to be uncorrelated)
! -          HCH^T          C_yy        variance of predicted observations
! -          A(HA)^T        C_xy        covariance between model states and predicted observations
! PP         P              C_yy+R      sum of predicted observation variance and observation error variance
! M          M=(P^-1)Y      -           intermediate result of the inversion M = (P^-1)Y, with P and Y from Mandel
! locCyy     -              -           localization matrix for the variance of HX
! locCxy     -              -           localization matrix for the covariance between X and HX 
!-----------------------------------------------------------------------------------
!>\brief Part 1 of ensemble Kalman filter analysis
!!
!> Innovations (Y=D-HX), and inversion of variances M=1/(var(HX) + R) which is later
!> used in part 2; the update equation: 
!>
!> X_analysis = X_forecast + cov(X,HX)/(var(HX)+R) * (D-HX)
!>
SUBROUTINE enkf_analysis_prepare(N,ND,D,HX,R,locCyy,M,Y,HA,status)
  !Arguments
  INTEGER, INTENT(IN)  :: N            !<number of ensemble members
  INTEGER, INTENT(IN)  :: ND           !<number of observations
  REAL,    INTENT(IN)  :: D(ND,N)      !<observation ensemble
  REAL,    INTENT(IN)  :: HX(ND,N)     !<predicted observation ensemble
  REAL,    INTENT(IN)  :: R(ND)        !<observation error variance, uncorrelated
  REAL,    INTENT(IN)  :: locCyy(ND,ND)!<localization matrix for cov(HX)
  REAL,    INTENT(OUT) :: M(ND,N)      !<inverse of (R+CXY) multiplied with innovation Y
  REAL,    INTENT(OUT) :: Y(ND,N)      !<innovation ensemble
  REAL,    INTENT(OUT) :: HA(ND,N)     !<HX deviation from mean(HX)
  LOGICAL, INTENT(INOUT) :: status     !<error status, failure of cholesky 
  !Local variables
  REAL, ALLOCATABLE    :: e_Nx1(:,:)   !unit vector, size Nx1
  REAL, ALLOCATABLE    :: e_1xN(:,:)   !unit vector, size 1xN
  REAL, ALLOCATABLE    :: z_NDx1(:,:)  !intermediate result vector, size NDx1
  REAL, ALLOCATABLE    :: PP(:,:)      !intermediate result vector, size NDxND
  INTEGER              :: I,J          !loop index
  LOGICAL failure
  
  !ALLOCATION and INITIALIZATION
  ALLOCATE(e_Nx1(N,1))
  ALLOCATE(e_1xN(1,N))
  ALLOCATE(z_NDx1(ND,1))
  ALLOCATE(PP(ND,ND))
  
  !1) assign unit vectors
  e_Nx1 = 1.
  e_1xN = 1.
    
  !2) HA = HX-E(HX) = HX-1/N * (HX*e_Nx1)*e_1xN,  in three steps:
  ! a. z_NDx1 = HX*e_Nx1
  CALL matrixmatrixmultiply(HX,e_Nx1,z_NDx1)
  ! b. HA = z_NDx1 * e_1xN
  CALL matrixmatrixmultiply(z_NDx1, e_1xN, HA) !HA = z * e_1xN
  ! c. !HA = HX - 1/N * HA
  HA = HX - HA/N                               
    
  !3) Y = D-HX, in one step
  Y = D - HX
    
  !4) PP = (R+cov(HX)), observation error variance + predicted observation covariance, in three step:              
  ! a. PP = 1/(N-1) * HA * (HA)' = cov(predicted_observations)
  CALL matrixmatrixmultiply(HA, TRANSPOSE(HA), PP)
  PP = PP/(N-1)
    
  ! b. Localization by elementwise multiplication, PP = locCyy[nobs,nobs] .* PP
  DO I=1,ND
    DO J=1,ND
      PP(J,I)=PP(J,I)*locCyy(J,I)
    ENDDO
  ENDDO
    
  ! c. PP = PP + R, adding obs error variance, using the theoretical (assumed uncorrelated) variance R 
  !                 rather than the sample covariance
  DO I = 1,ND
    PP(I,I) = PP(I,I) + R(I)
  ENDDO

  ! 5) Cholesky solution... LL' = P, M=inv(P) * Y    ie. M = "innovations" / (cov(obs) + cov(model))
  CALL choleskysolution(ND, N, PP, Y, M,failure)
  status = failure
  
  !Deallocate temporary results
  DEALLOCATE(PP)
  DEALLOCATE(e_Nx1)
  DEALLOCATE(e_1xN)
  DEALLOCATE(z_NDx1)

END SUBROUTINE enkf_analysis_prepare

! -----------------------------------------------------------------------------------
!>\brief Part 2 of ensemble Kalman filter analysis
!>
!>Apply ENKF filter on a model variable ensemble using the M
!>matrix derived in the enkf_analysis_prepare function.
!>
!> EnKF analysis, following the "basic implementation" in Jan Mandel report modified
!> to include covariance localization: 
!>
!>                (K = Cxy .* loc_Cxy / (Cyy .* loc_Cyy + R))
!>
!> The localization of Cyy is embedded in the M matrix which is 
!> prepared by the enkf_analysis_prepare routine.
!>
!> The localization of Cxy is introduced in the final step of this
!> "apply"-routine, which involves a change in the calculation of the AZ
!> matrix compared to Mandel:
!>
!>                Mandel:    AZ = A * (HA' * M)
!>
!>                Here:      AZ = ((A*HA') .* loc_Cxy) * M
!>
!> Matrix operations are made in an order defined by the parantheses (most inner operations first).
!> This might have consequences on the number of operations compared to Mandel, but I have not checked that.
! -----------------------------------------------------------------------------------
SUBROUTINE enkf_analysis_apply(N,NX,ND,M,HA,locCxy,X) !,Y
  !INPUT VARIABLES
  INTEGER, INTENT(IN)  :: N            !<number of ensemble members
  INTEGER, INTENT(IN)  :: NX           !<number of variables in ensemble (rows)
  INTEGER, INTENT(IN)  :: ND           !<number of observations
  REAL,INTENT(INOUT)   :: X(NX,N)      !<ensemble data
!  REAL,INTENT(IN)       :: M(NX,N)
  REAL,INTENT(IN)      :: M(ND,N)      !<inverse of (R+CXY) multiplied with innovation Y
!  REAL,INTENT(IN)      :: Y(ND,N)      !<innovation ensemble
  REAL,INTENT(IN)      :: HA(ND,N)     !<HX deviation from mean(HX)
  REAL,INTENT(IN)      :: locCxy(NX,ND)!<localization matrix
        
  !LOCAL VARIABLES
  REAL, ALLOCATABLE :: e_Nx1(:,:)
  REAL, ALLOCATABLE :: e_1xN(:,:)
  REAL, ALLOCATABLE :: z_NXx1(:,:)
  REAL, ALLOCATABLE :: Z_NXxND(:,:)
  REAL, ALLOCATABLE :: A(:,:)
  REAL, ALLOCATABLE :: AZ(:,:)
  INTEGER           :: I, J
  
  !ALLOCATION and INITIALIZATION
  ALLOCATE(A(NX,N))
  ALLOCATE(AZ(NX,N))
  ALLOCATE(e_Nx1(N,1))
  ALLOCATE(e_1xN(1,N))
  ALLOCATE(z_NXx1(NX,1))
  ALLOCATE(z_NXxND(NX,ND))
    
  !0) assign unit vectors
  e_Nx1 = 1.
  e_1xN = 1.

  !1) A = X-E(X) = X-1/N*(X*e_Nx1)*e_1xN
  CALL matrixmatrixmultiply(X, e_Nx1, z_NXx1)
  CALL matrixmatrixmultiply(z_NXx1, e_1xN, A)
  A = X - A / N

  !2) EnKF filtering equation
  !
  ! Modification of Mandel's two-step algoritm, to include localization of 
  ! covariances between innovations (Y=HX-D) and all model states (X) 
  !
  ! The Kalman filter analysis is written as:
  !     X = X + locCxy .* (A*HA') * M / (N-1) ;
  !  
  ! Which is formulate the two-step solution from Mandel, into a three-step solution:
  !  7.1: Z_NXxND = A*HA'
  CALL matrixmatrixmultiply(A, TRANSPOSE(HA), Z_NXxND)
  !    
  !  7.2: Z_NXxND = Z_NXxND .* locCX (element-wise)
  DO I=1,ND
    DO J=1,NX
      Z_NXxND(J,I)=Z_NXxND(J,I) * locCxy(J,I)
    ENDDO
  ENDDO
  !    
  !  7.3: Z_NXxN  = Z_NXxND * M      (we can use AZ for this)
  CALL matrixmatrixmultiply(Z_NXxND, M, AZ)
  !
  !! 7.1 Z = (HA)' * M
  !CALL matrixmatrixmultiply(TRANSPOSE(HA), M, Z_NxN)
  ! 
  !! 7.2) X=X+1/(N-1) * A * Z
  !CALL matrixmatrixmultiply(A, Z_NxN, AZ)
  !    
  ! 8) final update, just as before:
  X = X + AZ / (N-1)

  !Deallocation of local arrays necessary for gfortran
  IF(ALLOCATED(A)) DEALLOCATE(A)
  IF(ALLOCATED(AZ)) DEALLOCATE(AZ)
  IF(ALLOCATED(e_Nx1)) DEALLOCATE(e_Nx1)
  IF(ALLOCATED(e_1xN)) DEALLOCATE(e_1xN)
  IF(ALLOCATED(z_NXx1)) DEALLOCATE(z_NXx1)
  IF(ALLOCATED(z_NXxND)) DEALLOCATE(z_NXxND)
  
END SUBROUTINE enkf_analysis_apply

!------------------------------------------------------
!>Calculates summary statistics for an ensemble matrix
!------------------------------------------------------
SUBROUTINE assim_ensemble_statistics(xin, DIM, NN, xmean, xmins, xmaxs, xsigma, xmedian, domedian,missing) !xquantiles,xcovar, xcor)
  USE compout, only: calculate_median

  !Argument declarations
  INTEGER DIM, NN
  REAL xin(DIM,NN)
  REAL xmean(DIM), xmins(DIM), xmaxs(DIM), xsigma(DIM), xmedian(DIM) !xquantiles(3,DIM), xcovar(DIM,DIM), xcor(DIM,DIM)
  LOGICAL domedian
  
! Data needed for simplified statistical calculation
  REAL s(NN), pp(NN)
  REAL var, ep
  INTEGER i
  REAL missing
   
  ! loop over dimension DIM
  DO i=1,DIM
    ! mean value
    xmean(i)=sum(xin(i,:))/NN
    
    ! Min and Max
    xmins(i)=minval(xin(i,:))
    xmaxs(i)=maxval(xin(i,:))
    
    ! standard deviation
    s(:)=xin(i,:)-xmean(i)
    ep=sum(s(:))
    pp(:)=s(:)*s(:)
    var = (sum(pp(:))-ep**2/NN)/(NN-1)
    xsigma(i)=sqrt(var)
    
    ! median
    IF(domedian) CALL calculate_median(NN,xin(i,:),missing,xmedian(i))
      
    ! skip the quantiles
  ENDDO
END SUBROUTINE assim_ensemble_statistics

!---------------------------------------------------------------------
!> Routine that returns the ensemble data in a matrix. 
!> If needed, the data is read from binary file.
!> For speed, records in binary files contain all states in one
!> ensemble member (X transformated).
!---------------------------------------------------------------------
SUBROUTINE assim_get_ensemble_data(NX,NE,ensData,x)
  !input
  INTEGER, INTENT(IN)                    :: NX, NE    !number of states and ensemble members
  REAL, INTENT(OUT)                      :: X(NX,NE)  !ensemble data matrix
  TYPE(assim_ensemble_type),INTENT(IN)   :: ensData   !ensemble data structure
  
  !Local variables
  INTEGER j
  
  !ensemble data ALLOCATED in memory
  IF(ALLOCATED(ensData%x))THEN   !change because needed more options for different kind of bin-files
    X=ensData%x(:,ensData%rec+1:ensData%rec+NE)
  ELSE
  !ensemble data read from direct access binary file
    DO j = 1,NE
      READ(ensdata%fileID,REC=ensData%rec+j) X(:,j)
    ENDDO
  ENDIF
END SUBROUTINE assim_get_ensemble_data

!---------------------------------------------------------------------
!> Routine that writes a matrix into an ensemble data structure. 
!> If needed, the data is written to a binary file.
!---------------------------------------------------------------------
SUBROUTINE assim_set_ensemble_data(NX,NE,ensData,x,checkMinMax,missing) !,doStat
  !input
  INTEGER, INTENT(IN)                     :: NX, NE
  REAL, INTENT(INOUT)                     :: X(NX,NE)
  TYPE(assim_ensemble_type),INTENT(INOUT) :: ensData
  LOGICAL, INTENT(IN)                     :: checkMinMax !check min and max limits before update
!  LOGICAL, INTENT(IN)                     :: doStat      !calculate output statistics
  REAL, INTENT(IN)                        :: missing

  !Local variables
  INTEGER j
  
  !Check min and max limits
  IF(checkMinMax)THEN
    CALL assim_checkminmax(nx,ne,x,ensdata%minimum,ensData%maximum,missing)
  ENDIF

  !Write x to ensemble data...
  IF(ALLOCATED(ensData%x))THEN
    !... ALLOCATED in memory I don't under stand how Rec works here, removed.
    ensData%x(:,ensData%rec+1:ensData%rec+NE)=X
  ELSE
    !... or in direct access binary file.
    DO j = 1,NE
      WRITE(ensdata%fileID,REC=ensData%rec+j) X(:,j)
    ENDDO
  ENDIF
END SUBROUTINE assim_set_ensemble_data

!---------------------------------------------------------------------
!> Routine that returns the perturbation field ensemble data in a matrix. 
!> If needed, the data is read from binary file.
!> For speed, records in binary files contain all states in one
!> ensemble member (X transformated).
!---------------------------------------------------------------------
SUBROUTINE assim_get_ensemble_data_dx(NX,NE,ensData,dx)
  !input
  INTEGER, INTENT(IN)                    :: NX, NE    !number of states and ensemble members
  REAL, INTENT(OUT)                      :: DX(NX,NE) !perturbation ensemble data matrix
  TYPE(assim_ensemble_type),INTENT(IN)   :: ensData   !ensemble data structure
  
  !Local variables
  INTEGER j
  
  !ensemble data ALLOCATED in memory
  IF(ALLOCATED(ensData%dx))THEN
    DX=ensData%dx(:,1:NE)   !kind of unneccessary, the way it is called now but for completeness
  ELSE
    !ensemble data read from direct access binary file
    DO j = 1,NE
      READ(ensdata%fileID,REC=ensData%rec+NE+j) DX(:,j)
    ENDDO
  ENDIF
END SUBROUTINE assim_get_ensemble_data_dx

!---------------------------------------------------------------------
!> Routine that writes a matrix into an ensemble data structure. 
!> If wanted, the data is written to a binary file.
!>
!> dx-version which also writes the dx field (perturbation)
!---------------------------------------------------------------------
SUBROUTINE assim_set_ensemble_data_dx(NX,NE,ensData,x,dx,checkMinMax,missing) !,doStat
  !input
  INTEGER, INTENT(IN)                     :: NX, NE
  REAL, INTENT(INOUT)                     :: X(NX,NE)
  REAL, INTENT(INOUT)                     :: DX(NX,NE)
  TYPE(assim_ensemble_type),INTENT(INOUT) :: ensData
  LOGICAL, INTENT(IN)                     :: checkMinMax !check min and max limits before update
!  LOGICAL, INTENT(IN)                     :: doStat      !calculate output statistics
  REAL, INTENT(IN)                        :: missing

  !Local variables
  INTEGER j
  
  !Check min and max limits (not needed for DX)
  IF(checkMinMax)THEN
    CALL assim_checkminmax(nx,ne,x,ensdata%minimum,ensData%maximum,missing)
  ENDIF

  !Write X and DX to ensemble data...
  IF(ALLOCATED(ensData%x))THEN
    !... ALLOCATED in memory
    ensData%x(:,1:NE)=X
    ensData%dx(:,1:NE)=DX
    !ensData%x(:,ensData%rec+1:ensData%rec+NE)=X    !CP201028: ensData%rec=0, not used, when no binfile
    !ensData%dx(:,ensData%rec+1:ensData%rec+NE)=DX
  ELSE
    !... or in direct access binary file.
    DO j = 1,NE
      WRITE(ensdata%fileID,REC=ensData%rec+j) X(:,j)
      WRITE(ensdata%fileID,REC=ensData%rec+NE+j) DX(:,j)  !CP201028 added dx directly after x in bin-files
    ENDDO
  ENDIF
END SUBROUTINE assim_set_ensemble_data_dx

!>Calculates ensemble statistics for state, forcing and auxiliary ensembles
SUBROUTINE updateEnsembleStatistics(assimData,total_time)
  TYPE(assim_data_type) :: assimData !<main assimilation variable containing all data
  REAL, OPTIONAL, INTENT(INOUT) :: total_time(4) !<optional timing of the subroutine
  REAL start_time, stop_time
  INTEGER i
  REAL,ALLOCATABLE :: X(:,:)  !intermediate matrix of ensemble
  LOGICAL domedian
  REAL missing
  
  missing = assimData%info%missing

  IF(PRESENT(total_time)) CALL cpu_time(start_time)

  ! set domedian if needed for outputs
  IF(assimData%info%meanout)THEN
    domedian=.FALSE. ! median NOT needed for outputs
  ELSE
    domedian=.TRUE.  ! median IS needed for outputs
  ENDIF
  
  !state ensembles
  DO i=1,assimData%info%nX
    !Allocate and collect X from where it is saved before calculating the statistics CP added to handle bin-files.
    ALLOCATE(X(assimData%X(i)%x%nvar,assimData%X(i)%x%nens))

    IF(PRESENT(total_time))THEN
      call cpu_time(stop_time)
      total_time(1)=total_time(1)+stop_time-start_time
      start_time=stop_time
    ENDIF

    CALL assim_get_ensemble_data(assimData%X(i)%x%nvar,assimData%X(i)%x%nens,assimData%X(i)%x,X)   !CP161201 added call to routine to get X-matrix (useful if bin-file)

    IF(PRESENT(total_time))THEN
      call cpu_time(stop_time)
      total_time(2)=total_time(2)+stop_time-start_time
      start_time=stop_time
    ENDIF

    CALL assim_ensemble_statistics(X, assimData%X(i)%x%nvar, assimData%X(i)%x%nens, assimData%X(i)%x%outmean, assimData%X(i)%x%outmin, &
      assimData%X(i)%x%outmax, assimData%X(i)%x%outsigma, assimData%X(i)%x%outquant(2,:),domedian,missing)

    IF(PRESENT(total_time))THEN
      call cpu_time(stop_time)
      total_time(3)=total_time(3)+stop_time-start_time
      start_time=stop_time
    ENDIF

    DEALLOCATE(X)

    IF(PRESENT(total_time))THEN
      call cpu_time(stop_time)
      total_time(4)=total_time(4)+stop_time-start_time
      start_time=stop_time
    ENDIF
  ENDDO

  !forcing ensembles
  DO i=1,assimData%info%nF
    !Allocate and collect X from where it is saved before calculating the statistics, added to handle bin-files.
    ALLOCATE(X(assimData%F(i)%x%nvar,assimData%F(i)%x%nens))
    CALL assim_get_ensemble_data(assimData%F(i)%x%nvar,assimData%F(i)%x%nens,assimData%F(i)%x,X)
    CALL assim_ensemble_statistics(X, assimData%F(i)%x%nvar, assimData%F(i)%x%nens, assimData%F(i)%x%outmean, assimData%F(i)%x%outmin, &
      assimData%F(i)%x%outmax, assimData%F(i)%x%outsigma, assimData%F(i)%x%outquant(2,:),domedian,missing)
    DEALLOCATE(X)
  ENDDO

  !auxiliary ensembles
  DO i=1,assimData%info%nA
    !Allocate and collect X from where it is saved before calculating the statistics, added to handle bin-files.
    ALLOCATE(X(assimData%A(i)%x%nvar,assimData%A(i)%x%nens))
    CALL assim_get_ensemble_data(assimData%A(i)%x%nvar,assimData%A(i)%x%nens,assimData%A(i)%x,X)
    CALL assim_ensemble_statistics(X, assimData%A(i)%x%nvar, assimData%A(i)%x%nens, assimData%A(i)%x%outmean, assimData%A(i)%x%outmin, &
      assimData%A(i)%x%outmax, assimData%A(i)%x%outsigma, assimData%A(i)%x%outquant(2,:),domedian,missing)
    DEALLOCATE(X)
  ENDDO

END SUBROUTINE updateEnsembleStatistics

!----------------------------------------------------------------------
! forward and backward transforms for Gaussian anamorphosis
!----------------------------------------------------------------------
!> assim_forward_transform (Gaussian Anamorphosis)
!>
!> transformation (forward) of ensembles (observations, states, etc)
!> to improve Gaussian nature of ensemble distributions
!----------------------------------------------------------------------
SUBROUTINE assim_forward_transform(nx,ne,x,ensdata,missing)
  INTEGER,INTENT(IN)                       :: nx, ne   ! number of x, number of ensemble members
  REAL, INTENT(INOUT)                      :: x(nx,ne) ! ensemble matrix to be transformed 
  TYPE(assim_ensemble_type), INTENT(INOUT) :: ensdata  ! ensemble metadata
  REAL, INTENT(IN)                         :: missing
  integer i,j
  
  SELECT CASE(ensdata%transform)
  CASE(1) ! LOG transform  -> bounded variables [xmin,+infinity] -> t = log(x-xmin)
    ! loop over data  
    DO j=1,ne
      DO i=1,nx
        ! skip transform if missing value
        IF(x(i,j).NE.missing)THEN
          ! apply transform after scaling data to be within bounds x[xmin,+inf] -> x[epsilon,+inf]
          x(i,j) = log(amax1(x(i,j) - ensdata%minimum,ensdata%epsilon))
        ENDIF
      ENDDO
    ENDDO
  CASE(2) ! Yeo-Johnson transform -> unbounded variables -> t depends on lambda and sign of x
  
  CASE(3) ! LOGIT transform - double bounded variables [xmin,xmax]
    ! loop over data  
    DO j=1,ne
      DO i=1,nx
        ! skip transform if missing value
        IF(x(i,j).NE.missing)THEN
          ! scale data [xmin,xmax] to be within [0,1] with margin >= epsilon
          x(i,j) = x(i,j)-ensdata%minimum
          x(i,j) = x(i,j) / (ensdata%maximum-ensdata%minimum)
          x(i,j) = amax1(x(i,j),ensdata%epsilon)
          x(i,j) = amin1(x(i,j),ensdata%maximum-ensdata%epsilon)
          ! logit transform (forward)
          x(i,j) = log(x(i,j))-log(1.-x(i,j))
        ENDIF
      ENDDO
    ENDDO
  CASE DEFAULT
    ! nothing to do
  END SELECT
  
END SUBROUTINE assim_forward_transform
!----------------------------------------------------------------------
!> assim_forward_transform
!>
!> transformation (forward) of ensembles (observations, states, etc)
!> to improve Gaussian nature of ensemble distributions
!----------------------------------------------------------------------
SUBROUTINE assim_backward_transform(nx,ne,x,ensdata,missing)
  INTEGER,INTENT(IN)                       :: nx, ne   ! number of x, number of ensemble members
  REAL, INTENT(INOUT)                      :: x(nx,ne) ! ensemble matrix to be transformed 
  TYPE(assim_ensemble_type), INTENT(INOUT) :: ensdata  ! ensemble metadata
  REAL,INTENT(IN)                          :: missing
  integer i,j
  
  SELECT CASE(ensdata%transform)
  CASE(1) ! LOG transform inversion, t = log(x-xmin) -> x = exp(t) + xmin -> bounded variables [xmin,+infinity]
    
    DO j=1,ne
      DO i=1,nx
        ! skip transform for missing values
        IF(x(i,j).NE.missing)THEN
          ! inverse transform + scaling to [xmin, +infinity]
          x(i,j) = exp(x(i,j)) + ensdata%minimum
        ENDIF
      ENDDO
    ENDDO
      
  CASE(2) ! Yeo-Johnson transform -> unbounded variables -> t depends on lambda and sign of x
  
  CASE(3) ! LOGIT transform - double bounded variables [xmin,xmax]
    DO j=1,ne
      DO i=1,nx
        ! skip transform for missing values
        IF(x(i,j).NE.missing)THEN
          ! inverse transform of t[-inf,+inf] to x[0,1]
          x(i,j) = exp(x(i,j))/(exp(x(i,j))+1)
          ! scale from x[0,1] to x[xmin,xmax]
          x(i,j) = ensdata%minimum + (ensdata%maximum-ensdata%minimum)*x(i,j)
        ENDIF
      ENDDO
    ENDDO
  CASE DEFAULT
    ! nothing to do
  END SELECT
  
END SUBROUTINE assim_backward_transform

!----------------------------------------------------------------------
!>\brief Main routine in the ensemble Kalman filter analysis.
!!
!> Ensemble Kalman filter equations (Evensen) following (a) Mandel, J. Efficient implementation 
!> of the ensemble kalman filter, and (b) DeChant, C. Examining the effectiveness and 
!> robustness of sequential data assimilation methods for quantification of uncertainty
!> in hydrological forecasting. Localization following Magnusson, Gustafsson et al (2014)
!
! Organizes the calls to enkf_analysis_prepare and enkf_analysis_apply
! following this pseudo-code:
!   1. CALL enkf_analysis_prepare()
!        a. calculation of innovation, Y = D-HX
!        b. calculation and localization of the covariance of predicted observations cov(HX) = locCyy * cov(HX)
!        c. combine cov(HX) with observation error variance R into P = (cov(HX)+R)
!        d. derive intermediate update matrix M, by inversion of variance matrice sum  M = P^(-1)Y
!
!   2. DO i=1,num_model_states
!        CALL enkf_analysis_apply()
!             Here in Part 2, M is multiplied with the covariance Cov(X,HX) in the final EnKF update equation:
!      ENDDO
!----------------------------------------------------------------------------
SUBROUTINE enkf_analysis_main(assimData)
  !ARGUMENT
  TYPE(assim_data_type), INTENT(INOUT) :: assimData !<main assimilation variable containing all data
  !LOCAL VARIABLES
  INTEGER :: N, ND, NVAR, I, NX, NVARold, locID,obsType,DIM
  REAL, ALLOCATABLE :: X(:,:), Y(:,:), HA(:,:), M(:,:), D(:,:), HX(:,:), R(:) !, locCyy(:,:), locCxy(:,:)
  REAL missing
  real xmean(1), xmins(1), xmaxs(1), xsigma(1), xmedian(1)
  logical domedian
  logical skipEnKf  !skip EnKf for this timestep if failure to find cholesky solution
  domedian=.false.
  skipEnKf=.false.
  DIM=1
  missing = assimData%info%missing
  !Continue with ENKF analysis only if there is some data to assimilate
  IF(assimData%info%nD==0) RETURN   !No ENKF analysis if no data
  !Part I:  calculate innovations and covariance matrix inversion
  !--------------------------------------------------------------
    !assign local variables
    ND = assimData%info%nD    !N:o observations to assimlate
    N  = assimData%info%NE    !N:o ensemble members
       
    !ALLOCATE necessary matrices
    ALLOCATE(Y(ND,N))
    ALLOCATE(M(ND,N))
    ALLOCATE(HA(ND,N))
    ALLOCATE(HX(ND,N))
    ALLOCATE(D(ND,N))
    ALLOCATE(R(ND))
      
    !get D, HX, and locCyy data from Enkf data structure (read from binary files if needed)
    !--------------------------------------------------------------------------------------
    D=assimData%D    
    HX=assimData%HX
    R=assimData%R
    
    ! Consider transformation of D and HX to improve Gaussianity
    !---------------------------------------------------------------------------------------
    !loop over rows
    DO I=1,ND
      obsType = assimData%ObsType(I)  
      IF(assimData%Obs(obsType)%X%transform.GT.0)THEN
        !transform D (observation ensemble)
        CALL assim_forward_transform(DIM,N,D(I,:),assimData%Obs(obsType)%x,missing)
        !update R (observation error variance)
        CALL assim_ensemble_statistics(D(I,:), DIM, N, xmean, xmins, xmaxs, xsigma, xmedian, domedian, missing)
        xsigma(1) = AMAX1(xsigma(1),assim_minsigma)
        R(I) = xsigma(1)**2
        !transform HX (model observations)
        CALL assim_forward_transform(DIM,N,HX(I,:),assimData%Obs(obsType)%x,missing)       
      ENDIF
    ENDDO
  
    !CALL preparation routine
    CALL enkf_analysis_prepare(N,ND,D,HX,R,assimData%LocCYY,M,Y,HA,skipEnKf)
  
    !Deallocate some temporary variables
    IF(ALLOCATED(HX)) DEALLOCATE(HX)
    IF(ALLOCATED(D)) DEALLOCATE(D)
    IF(ALLOCATED(R)) DEALLOCATE(R)
      
    IF(.NOT.skipEnKf)THEN
    !Part II: enkf update on model variables one-by-one
    !--------------------------------------------------
    ! Nb. transformations (forward and backward) to improve Gaussianity are made in 
    !     the following sections if requested.
    !-----------------------------------------------------------------------------------
    !X state ensembles
    NX=assimData%Info%nX              !number of variable ensembles (types)
    IF(NX.GT.0)THEN
      NVAR=assimData%X(1)%x%nvar        !number of variables in ensemble (rows)
      NVARold=0
      !Loop over number of variable ensembles, if >0
      DO I=1,NX
        !check if this variable is analysed or re-initialized
        IF(assimData%X(I)%x%assimilate)THEN
          !re-ALLOCATE X and locCxy matrices, if needed
          NVAR=assimData%X(I)%x%nvar        !number of variables in ensemble (rows)
          IF(ALLOCATED(X).AND.NVAR.ne.NVARold)DEALLOCATE(X)
          IF(.not.ALLOCATED(X))ALLOCATE(X(NVAR,N))
          NVARold = NVAR
   
          !Get ensemble data into X matrix
          CALL assim_get_ensemble_data(NVAR,N,assimData%X(I)%x,X)   !CP161202 for no bin-file or several bin-files
 
          !Transform X if requested to improve its' Gaussianity
          IF(assimData%X(I)%x%transform.GT.0)THEN
            CALL assim_forward_transform(NVAR,N,X,assimData%X(I)%x,missing)
          ENDIF
          
          !get localization matrix locCXY
          locID = assimData%X(i)%x%locID !localization ID      
          
          !apply analysis
          CALL enkf_analysis_apply(N,NVAR,ND,M,HA,assimData%locCXY(locID)%x,X) !,Y
          
          !Inverse transform if used above
          IF(assimData%X(I)%x%transform.GT.0)THEN
            CALL assim_backward_transform(NVAR,N,X,assimData%X(I)%x,missing)
          ENDIF
          
          !save updated X matrix back to the ensemble data
          CALL assim_set_ensemble_data(NVAR,N,assimData%X(I)%x,X,.true.,missing) !,.true.
        ENDIF
      ENDDO
    ENDIF
    
    !A auxilary ensembles (outvar in HYPE)
    NX=assimData%Info%nA              !number of variable ensembles (types)
    IF(NX.GT.0)THEN
      NVAR=assimData%A(1)%x%nvar        !number of variables in ensemble (rows)
      NVARold=0
        
      !Loop over number of variable ensembles, if >0
      DO I=1,NX
        !check if this variable is analysed or re-initialized
        IF(assimData%A(I)%x%assimilate)THEN
          !re-ALLOCATE X and locCxy matrices, if needed
          NVAR=assimData%A(I)%x%nvar        !number of variables in ensemble (rows)
          IF(ALLOCATED(X).AND.NVAR.ne.NVARold)DEALLOCATE(X)
          IF(.not.ALLOCATED(X))ALLOCATE(X(NVAR,N))
          NVARold = NVAR
   
          !Get ensemble data into X matrix
          CALL assim_get_ensemble_data(NVAR,N,assimData%A(I)%x,X)   !CP161206 for no bin-file or several bin-files

          !get localization matrix locCXY
          locID = assimData%X(i)%x%locID !localization ID      

          !Apply analysis
          CALL enkf_analysis_apply(N,NVAR,ND,M,HA,assimData%locCXY(locID)%x,X) !,Y
            
          !save updated X matrix back to the ensemble data
          CALL assim_set_ensemble_data(NVAR,N,assimData%A(I)%x,X,.true.,missing) !,.true.

        ENDIF
      ENDDO
    ENDIF

    !F forcing ensembles (P, T, SW etc in HYPE)
    NX=assimData%Info%nF              !number of variable ensembles (types)
    IF(NX.GT.0)THEN
      NVAR=assimData%F(1)%x%nvar        !number of variables in ensemble (rows)
      !locID = assimData%F(i)%x%locID !localization ID
      NVARold=0
        
      !Loop over number of variable ensembles, if >0
      DO I=1,NX
        !check if this variable is analysed or re-initialized
        IF(assimData%F(I)%x%assimilate)THEN
          !re-ALLOCATE X and locCxy matrices, if needed
          NVAR=assimData%F(I)%x%nvar        !number of variables in ensemble (rows)
          IF(ALLOCATED(X).AND.NVAR.ne.NVARold)DEALLOCATE(X)
          IF(.not.ALLOCATED(X))ALLOCATE(X(NVAR,N))
          NVARold = NVAR
   
          !Get ensemble data into X matrix
          CALL assim_get_ensemble_data(NVAR,N,assimData%F(I)%x,X)

          !get localization matrix locCXY
          locID = assimData%X(i)%x%locID !localization ID      
          
          !apply analysis
          CALL enkf_analysis_apply(N,NVAR,ND,M,HA,assimData%locCXY(locID)%x,X) !,Y
            
          !save updated X matrix back to the ensemble data
          CALL assim_set_ensemble_data(NVAR,N,assimData%F(I)%x,X,.true.,missing) !,.true.
        ENDIF
      ENDDO
    ENDIF
    !To come : parameter ensembles
    ELSEIF(assimData%info%stop_at_failure)THEN
      WRITE(6,*) 'Simulation stopped after cholesky factorisation failed.'
      STOP 1
    ELSE
      WRITE(6,*) 'EnKf application skipped for this time step.'
    ENDIF !skipEnKf

    !Deallocate
    IF(ALLOCATED(X))     DEALLOCATE(X)
    IF(ALLOCATED(Y))     DEALLOCATE(Y)
    IF(ALLOCATED(M))     DEALLOCATE(M)
    IF(ALLOCATED(HA))    DEALLOCATE(HA)
END SUBROUTINE enkf_analysis_main


END MODULE ASSIMILATION_ROUTINES
